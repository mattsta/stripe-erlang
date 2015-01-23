%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=2 sw=2 et
-module(stripe).

-export([token_create/10, token_create_bank/3]).
-export([customer_create/3, customer_get/1, customer_update/3]).
-export([charge_customer/4, charge_card/4]).
-export([subscription_update/3, subscription_update/5, subscription_update/6, subscription_cancel/2, subscription_cancel/3]).
-export([customer/1, event/1, invoiceitem/1]).
-export([recipient_create/6, recipient_update/6]).
-export([transfer_create/5, transfer_cancel/1]).
-export([invoiceitem_create/4]).
-export([gen_paginated_url/1, gen_paginated_url/2, gen_paginated_url/3, gen_paginated_url/4]).
-export([get_all_customers/0, get_num_customers/1]).

-include("stripe.hrl").

-define(VSN_BIN, <<"0.7.0">>).
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
  request_customer_create(Fields).

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
  request_customer_update(CustomerId, Fields).

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
  request_token_create(Fields).

token_create_bank(Country, RoutingNumber, AccountNumber) ->
  Fields = [{"bank_account[country]", Country},
            {"bank_account[routing_number]", RoutingNumber},
            {"bank_account[account_number]", AccountNumber}],
  request_token_create(Fields).

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
  request_subscription(subscribe, Customer, Fields).

subscription_update(Customer, Subscription, Fields) ->
  request_subscription(update, Customer, Subscription, Fields).

subscription_cancel(Customer, AtPeriodEnd) when is_boolean(AtPeriodEnd) ->
  Fields = [{"at_period_end", AtPeriodEnd}],
  request_subscription(unsubscribe, Customer, Fields, AtPeriodEnd).

subscription_cancel(Customer, Subscription, AtPeriodEnd) when is_boolean(AtPeriodEnd) ->
  Fields = [{"at_period_end", AtPeriodEnd}],
  request_subscription(unsubscribe, Customer, Subscription, Fields, AtPeriodEnd).

%%%--------------------------------------------------------------------
%%% Recipient Management
%%%--------------------------------------------------------------------
recipient_create(Name, Type, TaxId, Bank, Email, Desc) ->
  Fields = [{name, Name},
            {type, Type},
            {tax_id, TaxId},
            {bank_account, Bank},
            {email, Email},
            {description, Desc}],
  request_recipient_create(Fields).

recipient_update(RecipientId, Name, TaxId, Bank, Email, Desc) ->
  Fields = [{name, Name},
            {tax_id, TaxId},
            {bank_account, Bank},
            {email, Email},
            {description, Desc}],
  request_recipient_update(RecipientId, Fields).

%%%--------------------------------------------------------------------
%%% Transfers (Payout) Management
%%%--------------------------------------------------------------------
transfer_create(Amount, Currency, RecipientId, Desc, StatementDesc) ->
  Fields = [{amount, Amount},
            {currency, Currency},
            {recipient, RecipientId},
            {description, Desc},
            {statement_descriptor, StatementDesc}],
  request_transfer_create(Fields).

transfer_cancel(TransferId) ->
  request_transfer_cancel(TransferId).

%%%--------------------------------------------------------------------
%%% event retrieval
%%%--------------------------------------------------------------------
event(EventId) ->
  request_event(EventId).

customer(CustomerId) ->
  request_customer(CustomerId).

%%%--------------------------------------------------------------------
%%% InvoiceItem Support
%%%--------------------------------------------------------------------

invoiceitem(InvoiceItemId) ->
  request_invoiceitem(InvoiceItemId).

invoiceitem_create(Customer, Amount, Currency, Description) ->
    Fields = [{customer, Customer},
              {amount, Amount},
              {currency, Currency},
              {description, Description}],
    request_invoiceitem_create(Fields).

%%%--------------------------------------------------------------------
%%% Pagination Support
%%%--------------------------------------------------------------------

get_all_customers() ->
    %% TODO, needs fixing
    request_paginated_customers().

get_num_customers(Count) ->
    request_paginated_customers(Count).

%%%--------------------------------------------------------------------
%%% request generation and sending
%%%--------------------------------------------------------------------
request_charge(Fields) ->
  request(charges, post, Fields).

request_event(EventId) ->
  request_run(gen_event_url(EventId), get, []).

request_customer(CustomerId) ->
  request_run(gen_customer_url(CustomerId), get, []).

request_invoiceitem(InvoiceItemId) ->
  request_run(gen_invoiceitem_url(InvoiceItemId), get, []).

request_invoiceitem_create(Fields) ->
  request(invoiceitems, post, Fields).

request_customer_create(Fields) ->
  request(customers, post, Fields).

request_customer_update(CustomerId, Fields) ->
  request_run(gen_customer_url(CustomerId), post, Fields).

request_token_create(Fields) ->
  request(tokens, post, Fields).

request_recipient_create(Fields) ->
  request(recipients, post, Fields).

request_recipient_update(RecipientId, Fields) ->
  request_run(gen_recipient_url(RecipientId), post, Fields).

request_transfer_create(Fields) ->
  request(transfers, post, Fields).

request_transfer_cancel(TransferId) ->
  request_run(gen_transfer_cancel_url(TransferId), post, []).

request(Action, post, Fields) ->
  URL = gen_url(Action),
  request_run(URL, post, Fields).

request_subscription(subscribe, Customer, Fields) ->
  request_run(gen_subscription_url(Customer), post, Fields).

request_subscription(update, Customer, Subscription, Fields) ->
  request_run(gen_subscription_url(Customer, Subscription), post, Fields);

request_subscription(unsubscribe, Customer, Fields, _AtEnd = true) ->
  request_run(gen_subscription_url(Customer) ++ "?at_period_end=true",
    delete, Fields);
request_subscription(unsubscribe, Customer, Fields, _AtEnd = false) ->
  request_run(gen_subscription_url(Customer), delete, Fields).

request_subscription(unsubscribe, Customer, Subscription, Fields, _AtEnd = true) ->
  request_run(gen_subscription_url(Customer, Subscription) ++ "?at_period_end=true",
    delete, Fields);
request_subscription(unsubscribe, Customer, Subscription,Fields, _AtEnd = false) ->
  request_run(gen_subscription_url(Customer, Subscription), delete, Fields).

request_paginated_customers() ->
    request_run(gen_paginated_url(customers), get, []).
request_paginated_customers(Count) ->
    request_run(gen_paginated_url(customers, Count), get, []).

request_run(URL, Method, Fields) ->
  Headers = [{"X-Stripe-Client-User-Agent", ua_json()},
             {"User-Agent", "Stripe/v1 ErlangBindings/" ++ ?VSN_STR},
             {"Authorization", auth_key()}],
  Type = "application/x-www-form-urlencoded",
  Body = gen_args(Fields),
  Request = case Method of
              % get and delete are body-less http requests
                 get -> {URL, Headers};
              delete -> {URL, Headers};
                   _ -> {URL, Headers, Type, Body}
            end,
  Requested = httpc:request(Method, Request, [], []),
  resolve(Requested).

%% request_run_paginated(URL) ->
%%     GetRequest = fun(URL) ->
%%                          Headers = [{"X-Stripe-Client-User-Agent", ua_json()},
%%                                     {"User-Agent", "Stripe/v1 ErlangBindings/" ++ ?VSN_STR},
%%                                     {"Authorization", auth_key()}],
%%                          Type = "application/x-www-form-urlencoded",
%%                          Request = {URL, Headers},
%%                          Requested = httpc:request(get, Request, [], [])
%%                  end,
%%     Requested = GetRequest(URL),



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
    #stripe_card{} | #stripe_token{} | #stripe_event{} |
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
-define(NRAPI, <<"Not Returned by API">>).
-define(V(X), proplists:get_value(atom_to_binary(X, utf8),
                                  DecodedResult, ?NRAPI)).

json_to_record(Json) when is_list(Json) andalso is_tuple(hd(Json)) ->
  json_to_record(proplists:get_value(<<"object">>, Json), Json);

json_to_record(Body) when is_list(Body) orelse is_binary(Body) ->
  DecodedResult = mochijson2:decode(Body, [{format, proplist}]),
  json_to_record(DecodedResult).

% Yes, these are verbose and dumb because we don't have runtime record/object
% capabilities.  In a way, it's nice being explicit up front.
-spec json_to_record(stripe_object_name(), proplist()) -> record().
json_to_record(<<"list">>, DecodedResult) ->
    Data = ?V(data),
    #stripe_list{data = [json_to_record(Object) || Object <- Data]};

json_to_record(<<"event">>, DecodedResult) ->
  Data = ?V(data),
  Object = proplists:get_value(<<"object">>, Data),
  ObjectName = proplists:get_value(<<"object">>, Object),
  #stripe_event{id      = ?V(id),
                type    = ?V(type),
                created = ?V(created),
                data    = json_to_record(ObjectName, Object)};

json_to_record(<<"charge">>, DecodedResult) ->
  #stripe_charge{id           = ?V(id),
                 created      = ?V(created),
                 amount       = ?V(amount),
                 balance_transaction = ?V(balance_transaction),
                 currency     = check_to_atom(?V(currency)),
                 description  = ?V(description),
                 livemode     = ?V(livemode),
                 paid         = ?V(paid),
                 refunded     = ?V(refunded),
                 customer     = ?V(customer),
                 failure_code = ?V(failure_code),
                 failure_message = ?V(failure_message),
                 card         = proplist_to_card(?V(card))};

json_to_record(<<"token">>, DecodedResult) ->
  #stripe_token{id        = ?V(id),
                used      = ?V(used),
                livemode  = ?V(livemode),
                card = proplist_to_card(?V(card)),
                bank_account = proplist_to_bank_account(?V(bank_account))};

json_to_record(<<"customer">>, DecodedResult) ->
  #stripe_customer{id              = ?V(id),
                   description     = ?V(description),
                   livemode        = ?V(livemode),
                   created         = ?V(created),
                   email           = ?V(email),
                   delinquent      = ?V(delinquent),
                   discount        = json_to_record(<<"discount">>, ?V(discount)),
                   account_balance = ?V(account_balance)};

% We don't have eunit tests for discount decoding yet.  Use at your own risk.
json_to_record(<<"discount">>, null) -> null;
json_to_record(<<"discount">>, DecodedResult) ->
  #stripe_discount{coupon   = json_to_record(coupon, ?V(coupon)),
                   start    = ?V(start),
                   'end'    = ?V('end'),
                   customer = ?V(customer)
                  };

% We don't have eunit tests for coupon decoding yet.  Use at your own risk.
json_to_record(<<"coupon">>, null) -> null;
json_to_record(<<"coupon">>, DecodedResult) ->
  #stripe_coupon{id                 = ?V(id),
                 percent_off        = ?V(percent_off),
                 amount_off         = ?V(amount_off),
                 currency           = check_to_atom(?V(currency)),
                 duration           = ?V(duration),
                 redeem_by          = ?V(redeem_by),
                 max_redemptions    = ?V(max_redemptions),
                 times_redeemed     = ?V(times_redeemed),
                 duration_in_months = ?V(duration_in_months)
                };

json_to_record(<<"subscription">>, null) -> null;
json_to_record(<<"subscription">>, DecodedResult) when is_list(DecodedResult) ->
  #stripe_subscription{id                   = ?V(id),
                       status               = check_to_atom(?V(status)),
                       current_period_start = ?V(current_period_start),
                       current_period_end   = ?V(current_period_end),
                       trial_start          = ?V(trial_start),
                       trial_end            = ?V(trial_end),
                       ended_at             = ?V(ended_at),
                       canceled_at          = ?V(canceled_at),
                       customer             = ?V(customer),
                       start                = ?V(start),
                       quantity             = ?V(quantity),
                       plan                 = proplist_to_plan(?V(plan))};

json_to_record(<<"invoiceitem">>, DecodedResult) ->
  #stripe_invoiceitem{id           = ?V(id),
                      amount       = ?V(amount),
                      currency     = check_to_atom(?V(currency)),
                      customer     = ?V(customer),
                      date         = ?V(date),
                      description  = ?V(description),
                      proration    = ?V(proration)};

json_to_record(<<"recipient">>, DecodedResult) ->
  #stripe_recipient{id           = ?V(id),
                    created      = ?V(created),
                    type         = check_to_atom(?V(type)),
                    active_account = proplist_to_bank_account(?V(active_account)),
                    verified     = ?V(verified),
                    description  = ?V(description),
                    name         = ?V(name),
                    email        = ?V(email)};

json_to_record(<<"transfer">>, DecodedResult) ->
  #stripe_transfer{id           = ?V(id),
                   amount       = ?V(amount),
                   currency     = check_to_atom(?V(currency)),
                   date         = ?V(date),
                   balance_transaction = ?V(balance_transaction),
                   status       = check_to_atom(?V(status)),
                   account      = proplist_to_bank_account(?V(account)),
                   description  = ?V(description),
                   recipient    = ?V(recipient),
                   statement_descriptor = ?V(statement_descriptor)};

json_to_record(Type, DecodedResult) ->
  error_logger:error_msg({unimplemented, ?MODULE, json_to_record, Type, DecodedResult}),
  {not_implemented_yet, Type, DecodedResult}.

proplist_to_card(null) -> null;
proplist_to_card(A) when is_binary(A) -> A;
proplist_to_card(Card) ->
  DecodedResult = Card,
  #stripe_card{name                = ?V(name),
               last4               = ?V(last4),
               exp_month           = ?V(exp_month),
               exp_year            = ?V(exp_year),
               brand               = ?V(brand),
               cvc_check           = check_to_atom(?V(cvc_check)),
               address_line1_check = check_to_atom(?V(address_line1_check)),
               address_zip_check   = check_to_atom(?V(address_zip_check)),
               country             = ?V(country)}.

proplist_to_plan(Plan) ->
  DecodedResult = Plan,
  #stripe_plan{id             = ?V(id),
               currency       = check_to_atom(?V(currency)),
               interval       = ?V(interval),
               interval_count = ?V(interval_count),
               name           = ?V(name),
               amount         = ?V(amount),
               livemode       = ?V(livemode)}.

proplist_to_bank_account(null) -> null;
proplist_to_bank_account(A) when is_binary(A) -> A;
proplist_to_bank_account(BankAccount) ->
  DecodedResult = BankAccount,
  #stripe_bank_account{fingerprint = ?V(fingerprint),
                       bank_name  = ?V(bank_name),
                       last4      = ?V(last4),
                       country    = ?V(country),
                       validated  = ?V(validated),
                       description = ?V(description),
                       recipient  = ?V(recipient),
                       statement_descriptor = ?V(statement_descriptor)}.

check_to_atom(null) -> null;
check_to_atom(A) when is_atom(A) -> A;
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
  #stripe_error{type    = check_to_atom(?V(type)),
                code    = check_to_atom(?V(code)),
                http_error_code = ErrCode,
                http_error_code_meaning = ErrCodeMeaning,
                message = ?V(message),
                param   = ?V(param)}.

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
  OnlyWithValues = [{K, V} || {K, V} <- Fields, V =/= [] andalso V =/= <<>>],
  mochiweb_util:urlencode(OnlyWithValues).

gen_url(Action) when is_atom(Action) ->
  gen_url(atom_to_list(Action));
gen_url(Action) when is_list(Action) ->
  "https://api.stripe.com/v1/" ++ Action.

gen_customer_url(CustomerId) when is_binary(CustomerId) ->
  gen_customer_url(binary_to_list(CustomerId));
gen_customer_url(CustomerId) when is_list(CustomerId) ->
  "https://api.stripe.com/v1/customers/" ++ CustomerId.

gen_recipient_url(RecipientId) when is_binary(RecipientId) ->
  gen_recipient_url(binary_to_list(RecipientId));
gen_recipient_url(RecipientId) when is_list(RecipientId) ->
  "https://api.stripe.com/v1/recipients/" ++ RecipientId.

gen_transfer_cancel_url(TransferId) when is_binary(TransferId) ->
  gen_transfer_cancel_url(binary_to_list(TransferId));
gen_transfer_cancel_url(TransferId) when is_list(TransferId) ->
  "https://api.stripe.com/v1/transfers/" ++ TransferId ++ "/cancel".

gen_invoiceitem_url(InvoiceItemId) when is_binary(InvoiceItemId) ->
  gen_invoiceitem_url(binary_to_list(InvoiceItemId));
gen_invoiceitem_url(InvoiceItemId) when is_list(InvoiceItemId) ->
  "https://api.stripe.com/v1/invoiceitems/" ++ InvoiceItemId.

gen_subscription_url(Customer) when is_binary(Customer) ->
  gen_subscription_url(binary_to_list(Customer));
gen_subscription_url(Customer) when is_list(Customer) ->
  "https://api.stripe.com/v1/customers/" ++ Customer ++ "/subscription".

gen_subscription_url(Customer, Subscription) when is_binary(Customer) ->
  gen_subscription_url(binary_to_list(Customer), Subscription);
gen_subscription_url(Customer, Subscription) when is_binary(Subscription) ->
  gen_subscription_url(Customer, binary_to_list(Subscription));
gen_subscription_url(Customer, Subscription) when is_list(Customer) ->
  "https://api.stripe.com/v1/customers/" ++ Customer ++ "/subscriptions/" ++ Subscription.

gen_event_url(EventId) when is_binary(EventId) ->
  gen_event_url(binary_to_list(EventId));
gen_event_url(EventId) when is_list(EventId) ->
  "https://api.stripe.com/v1/events/" ++ EventId.


gen_paginated_url(Type) ->
    gen_paginated_url(Type, 10, [], []).
gen_paginated_url(Type, Limit) ->
    gen_paginated_url(Type, Limit, [], []).
gen_paginated_url(Type, Limit, StartingAfter) ->
    gen_paginated_url(Type, Limit, StartingAfter, []).
gen_paginated_url(Type, Limit, StartingAfter, EndingBefore) ->
    Arguments = gen_args([{"limit", Limit},
                          {"starting_after", StartingAfter},
                          {"ending_before", EndingBefore}]),
    gen_paginated_base_url(Type) ++ Arguments.

gen_paginated_base_url(charges) ->
    "https://api.stripe.com/v1/charges?";
gen_paginated_base_url(customers) ->
    "https://api.stripe.com/v1/customers?";
gen_paginated_base_url(invoices) ->
    "https://api.stripe.com/v1/invoices?".
