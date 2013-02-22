-module(stripe).

-export([token_create/10, customer_create/3, customer_get/1, customer_update/3]).
-export([charge_customer/4, charge_card/4]).
-export([subscription_update/5, subscription_update/6, subscription_cancel/2]).
-export([ipn/1]).

-include("stripe.hrl").

-define(VSN_BIN, <<"0.4">>).
-define(VSN_STR, binary_to_list(?VSN_BIN)).

%%%--------------------------------------------------------------------
%%% Charging
%%%--------------------------------------------------------------------
-spec charge_customer(price(), currency(), customer_id(), desc()) -> result.
charge_customer(Amount, Currency, Customer, Desc) ->
  charge(Amount, Currency, {customer, Customer}, Desc).

-spec charge_card(price(), currency(), token_id(), desc()) -> result.
charge_card(Amount, Currency, Card, Desc) ->
  charge(Amount, Currency, {card, Card}, Desc).

-spec charge(price(), currency(),
            {customer, customer_id()} | {card, token_id()}, desc()) -> result.
charge(Amount, Currency, CustomerOrCard, Desc) when
    Amount > 50 andalso is_tuple(CustomerOrCard) ->
  Fields = [{amount, Amount},
            {currency, Currency},
            CustomerOrCard,
            {description, Desc}],
  request_charge(Fields).

%%%--------------------------------------------------------------------
%%% Customer Creation
%%%--------------------------------------------------------------------
-spec customer_create(token_id(), email(), desc()) -> result.
customer_create(Card, Email, Desc) ->
  Fields = [{card, Card},
            {email, Email},
            {description, Desc}],
  OnlyWithValues = [{K, V} || {K, V} <- Fields, V =/= [] andalso V =/= <<>>],
  request_customer_create(OnlyWithValues).

%%%--------------------------------------------------------------------
%%% Customer Fetching
%%%--------------------------------------------------------------------
-spec customer_get(customer_id()) -> result.
customer_get(CustomerId) ->
  request_customer(CustomerId).

%%%--------------------------------------------------------------------
%%% Customer Updating
%%%--------------------------------------------------------------------
-spec customer_update(customer_id(), token_id(), email()) -> result.
customer_update(CustomerId, Token, Email) ->
  Fields = [{"card", Token},
            {"email", Email}],
  OnlyWithValues = [{K, V} || {K, V} <- Fields, V =/= [] andalso V =/= <<>>],
  request_customer_update(CustomerId, OnlyWithValues).

%%%--------------------------------------------------------------------
%%% Token Generation
%%%--------------------------------------------------------------------
token_create(CardNumber, ExpMon, ExpYr, Cvc,
             Name, Addr1, Addr2, Zip, State, Country) ->
  Fields = [{"card[number]", CardNumber},
            {"card[exp_month]", ExpMon},
            {"card[exp_year]", ExpYr},
            {"card[cvc]", Cvc},
            {"card[name]", Name},
            {"card[address_line1]", Addr1},
            {"card[address_line2]", Addr2},
            {"card[address_zip]", Zip},
            {"card[address_state]", State},
            {"card[address_country]", Country}],
  OnlyWithValues = [{K, V} || {K, V} <- Fields, V =/= [] andalso V =/= <<>>],
  request_token_create(OnlyWithValues).

%%%--------------------------------------------------------------------
%%% subscription updating/creation and removal
%%%--------------------------------------------------------------------
subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd) ->
  subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd, "").
subscription_update(Customer, Plan, Coupon, Prorate, TrialEnd, Quantity) ->
  Fields = [{"plan", Plan},
            {"coupon", Coupon},
            {"prorate", Prorate},
            {"trial_end", TrialEnd},
            {"quantity", Quantity}],
  OnlyWithValues = [{K, V} || {K, V} <- Fields, V =/= [] andalso V =/= <<>>],
  request_subscription(subscribe, Customer, OnlyWithValues).

subscription_cancel(Customer, AtPeriodEnd) when is_boolean(AtPeriodEnd) ->
  Fields = [{"at_period_end", AtPeriodEnd}],
  OnlyWithValues = [{K, V} || {K, V} <- Fields, V =/= [] andalso V =/= <<>>],
  request_subscription(unsubscribe, Customer, OnlyWithValues, AtPeriodEnd).

%%%--------------------------------------------------------------------
%%% request generation and sending
%%%--------------------------------------------------------------------
request_charge(Fields) ->
  request(charges, post, Fields).

request_customer_create(Fields) ->
  request(customers, post, Fields).

request_customer(CustomerId) ->
  request_run(gen_customer_url(CustomerId), get, []).

request_customer_update(CustomerId, Fields) ->
  request_run(gen_customer_url(CustomerId), post, Fields).

request_token_create(Fields) ->
  request(tokens, post, Fields).

request(Action, post, Fields) ->
  URL = gen_url(Action),
  request_run(URL, post, Fields).

request_subscription(subscribe, Customer, Fields) ->
  request_run(gen_subscription_url(Customer), post, Fields).

request_subscription(unsubscribe, Customer, Fields, _AtEnd = true) ->
  request_run(gen_subscription_url(Customer) ++ "?at_period_end=true",
    delete, Fields);
request_subscription(unsubscribe, Customer, Fields, _AtEnd = false) ->
  request_run(gen_subscription_url(Customer), delete, Fields).

request_run(URL, Method, Fields) ->
  Headers = [{"X-Stripe-Client-User-Agent", ua_json()},
             {"User-Agent", "Stripe/v1 ErlangBindings/" ++ ?VSN_STR},
             {"Authorization", auth_key()}], 
  Type = "application/x-www-form-urlencoded",
  Body = gen_args(Fields),
  Request = case Method of
              % get and delete are body-less http requests
              get -> {URL, Headers};
              deleted -> {URL, Headers};
              _ -> {URL, Headers, Type, Body}
            end,
  Requested = httpc:request(Method, Request, [], []),
  resolve(Requested).
  
%%%--------------------------------------------------------------------
%%% response parsing
%%%--------------------------------------------------------------------
resolve({ok, {{_HTTPVer, StatusCode, _Reason}, _Headers, Body}}) ->
  resolve_status(StatusCode, Body);
resolve({ok, {StatusCode, Body}}) ->
  resolve_status(StatusCode, Body);
resolve({error, Reason}) ->
  {error, Reason}.

-spec resolve_status(pos_integer(), json()) ->
    #stripe_card{} | #stripe_token{} |
    #stripe_customer{} | #stripe_error{}.
% success range conditions stolen from stripe-python
resolve_status(HTTPStatus, SuccessBody) when
    HTTPStatus >= 200 andalso HTTPStatus < 300 ->
  json_to_record(SuccessBody);
resolve_status(HTTPStatus, ErrorBody) ->
  json_to_error(HTTPStatus, ErrorBody).

%%%--------------------------------------------------------------------
%%% Json to local type object records
%%%--------------------------------------------------------------------
-define(V(X), proplists:get_value(atom_to_binary(X, utf8),
                                  DecodedResult, <<"Not Returned by API">>)).

json_to_record(Body) when is_list(Body) orelse is_binary(Body) ->
  DecodedResult = mochijson2:decode(Body, [{format, proplist}]),
  Type = proplists:get_value(<<"object">>, DecodedResult),
  json_to_record(binary_to_existing_atom(Type, utf8), DecodedResult).

% Yes, these are verbose and dumb because we don't have runtime record/object
% capabilities.  In a way, it's nice being explicit up front.
-spec json_to_record(stripe_object_name(), proplist()) -> record().
json_to_record(charge, DecodedResult) ->
  #stripe_charge{id           = ?V(id),
                 created      = ?V(created),
                 amount       = ?V(amount),
                 fee          = ?V(fee),
                 currency     = binary_to_atom(?V(currency), utf8),
                 description  = ?V(description),
                 livemode     = ?V(livemode),
                 paid         = ?V(paid),
                 refunded     = ?V(refunded),
                 card         = proplist_to_card(?V(card))};

json_to_record(token, DecodedResult) ->
  #stripe_token{id        = ?V(id),
                currency  = binary_to_atom(?V(currency), utf8),
                used      = ?V(used),
                amount    = ?V(amount),
                livemode  = ?V(livemode),
                card = proplist_to_card(?V(card))};

json_to_record(customer, DecodedResult) ->
  #stripe_customer{id              = ?V(id),
                   description     = ?V(description),
                   livemode        = ?V(livemode),
                   created         = ?V(created),
                   active_card     = proplist_to_card(?V(active_card)),
                   email           = ?V(email),
                   delinquent      = ?V(delinquent),
                   subscription    = json_to_record(subscription, ?V(subscription)),
                   discount        = ?V(discount),
                   account_balance = ?V(account_balance)};

json_to_record(subscription, null) -> null;
json_to_record(subscription, DecodedResult) ->
  #stripe_subscription{status               = binary_to_atom(?V(status), utf8),
                       current_period_start = ?V(current_period_start),
                       current_period_end   = ?V(current_period_end),
                       ended_at             = ?V(ended_at),
                       canceled_at          = ?V(canceled_at),
                       customer             = ?V(customer),
                       start                = ?V(start),
                       quantity             = ?V(quantity),
                       plan                 = proplist_to_plan(?V(plan))}.

proplist_to_card(null) -> null;
proplist_to_card(Card) ->
  DecodedResult = Card,
  #stripe_card{last4               = ?V(last4),
               exp_month           = ?V(exp_month),
               exp_year            = ?V(exp_year),
               type                = ?V(type),
               cvc_check           = check_to_atom(?V(cvc_check)),
               address_line1_check = check_to_atom(?V(address_line1_check)),
               address_zip_check   = check_to_atom(?V(address_zip_check)),
               country             = ?V(country)}.

proplist_to_plan(Plan) ->
  DecodedResult = Plan,
  #stripe_plan{id             = ?V(id),
               currency       = binary_to_atom(?V(currency), utf8),
               interval       = ?V(interval),
               interval_count = ?V(interval_count),
               name           = ?V(name),
               amount         = ?V(amount),
               livemode       = ?V(livemode)}.

check_to_atom(null) -> null;
check_to_atom(Check) when is_binary(Check) -> binary_to_atom(Check, utf8).

% error range conditions stolen from stripe-python
json_to_error(ErrCode, Body) ->
  ErrCodeMeaning = case ErrCode of
                     400 -> missing_param;
                     401 -> bad_api_key;
                     402 -> params_ok_but_request_failed;
                     404 -> notfound;
                     E when E >= 500 -> stripe_server_error;
                     E when E =:= 403 orelse E > 404 -> stripe_api_error;
                     _ -> unknown_error
                   end,
  json_to_error(ErrCode, ErrCodeMeaning, Body).

% Let's use a common error object/record instead of breaking out per-type
% errors.  We can match on error types easily.
json_to_error(ErrCode, ErrCodeMeaning, Body) ->
  PreDecoded = mochijson2:decode(Body, [{format, proplist}]),
  DecodedResult = proplists:get_value(<<"error">>, PreDecoded),
  #stripe_error{type    = binary_to_atom(?V(type), utf8),
                code    = binary_to_atom(?V(code), utf8),
                http_error_code = ErrCode,
                http_error_code_meaning = ErrCodeMeaning,
                message = ?V(message),
                param   = ?V(param)}.

%%%--------------------------------------------------------------------
%%% Simple IPN decoding
%%%--------------------------------------------------------------------
ipn(Json) ->
  Decoded = mochijson2:decode(Json, [{format, proplist}]),
  Event = proplists:get_value(<<"event">>, Decoded),
  json_to_ipn_record(binary_to_existing_atom(Event, utf8), Decoded).

-define(C(X), proplists:get_value(<<"customer">>, DecodedResult, nil)).
json_to_ipn_record(recurring_payment_failed, DecodedResult) ->
  {payment_failed, ?C(DecodedResult), DecodedResult};
json_to_ipn_record(invoice_ready, DecodedResult) ->
  {invoice_ready, ?C(DecodedResult), DecodedResult};
json_to_ipn_record(recurring_payment_succeeded, DecodedResult) ->
  {subscribe, ?C(DecodedResult), DecodedResult};
json_to_ipn_record(subscription_trial_ending, DecodedResult) ->
  {trial_ending, ?C(DecodedResult), DecodedResult};
json_to_ipn_record(subscription_final_payment_attempt_failed, DecodedResult) ->
  {subscription_failure, ?C(DecodedResult), DecodedResult};
json_to_ipn_record(ping, _) ->
  ping.

%%%--------------------------------------------------------------------
%%% value helpers
%%%--------------------------------------------------------------------
ua_json() ->
  Props = [{<<"bindings_version">>, ?VSN_BIN},
           {<<"lang">>, <<"erlang">>},
           {<<"publisher">>, <<"mattsta">>}],
  binary_to_list(iolist_to_binary(mochijson2:encode(Props))).

auth_key() ->
  Token = env(auth_token),
  Auth = base64:encode_to_string(Token ++ ":"),
  "Basic " ++ Auth.

env(What) ->
  case env(What, diediedie) of
    diediedie -> throw({<<"You must define this in your app:">>, What});
         Else -> Else
  end.

env(What, Default) ->
  case application:get_env(stripe, What) of
    {ok, Found} -> Found;
      undefined -> Default
  end.

-spec gen_args(proplist()) -> string().
gen_args([]) -> "";
gen_args(Fields) when is_list(Fields) andalso is_tuple(hd(Fields)) ->
  mochiweb_util:urlencode(Fields).

gen_url(Action) when is_atom(Action) ->
  gen_url(atom_to_list(Action));
gen_url(Action) when is_list(Action) ->
  "https://api.stripe.com/v1/" ++ Action.

gen_customer_url(CustomerId) when is_binary(CustomerId) ->
  gen_customer_url(binary_to_list(CustomerId));
gen_customer_url(CustomerId) when is_list(CustomerId) ->
  "https://api.stripe.com/v1/customers/" ++ CustomerId.

gen_subscription_url(Customer) when is_binary(Customer) ->
  gen_subscription_url(binary_to_list(Customer));
gen_subscription_url(Customer) when is_list(Customer) ->
  "https://api.stripe.com/v1/customers/" ++ Customer ++ "/subscription".
