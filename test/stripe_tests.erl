-module(stripe_tests).

-include_lib("eunit/include/eunit.hrl").
-include("stripe.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
stripe_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Create Token",
       fun create_token/0},
     {"Charge Token",
       fun charge_token/0},
     {"Create Minimum Customer",
       fun create_min_customer/0},
     {"Create Customer",
       fun create_customer/0},
     {"Get Customer",
       fun get_customer/0},
     {"Charge Customer",
       fun charge_customer/0},
     {"Update Customer",
       fun update_customer/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
create_token() ->
  Result = ?debugTime("Creating token",
    stripe:token_create("4242424242424242", 12, 2021, 123,
                        [], [], [], [], [], [])),
  put(current_token, Result#stripe_token.id),
  ?debugFmt("Token ID: ~p~n", [Result#stripe_token.id]),
  ?assertEqual(false, Result#stripe_token.used),
  verify_default_card(Result#stripe_token.card, nocheck).

charge_token() ->
  Token = get(current_token),
  Desc = <<"MAH CHARGE">>,
  Result = ?debugTime("Charging card",
    stripe:charge_card(65540, usd, Token, Desc)),
  ?debugFmt("Token Charge ID: ~p~n", [Result#stripe_charge.id]),
  ?assertEqual(65540, Result#stripe_charge.amount),
  ?assertEqual(usd, Result#stripe_charge.currency),
  ?assertEqual(1931, Result#stripe_charge.fee),
  ?assertEqual(true, Result#stripe_charge.paid),
  ?assertEqual(false, Result#stripe_charge.refunded),
  ?assertEqual(Desc, Result#stripe_charge.description),
  verify_default_card(Result#stripe_charge.card, check).

create_min_customer() ->
  Result = ?debugTime("Creating minimum customer",
    stripe:customer_create("", "", "")),
  ?debugFmt("Customer ID: ~p~n", [Result#stripe_customer.id]).

create_customer() ->
  create_token(),
  Token = get(current_token),
  Email = "hokum@pokum.com",
  Desc = "You really don't wanna know",
  Result = ?debugTime("Creating customer",
    stripe:customer_create(Token, Email, Desc)),
  put(current_customer, Result#stripe_customer.id),
  ?debugFmt("Customer ID: ~p~n", [Result#stripe_customer.id]),
  ?assertEqual(list_to_binary(Desc), Result#stripe_customer.description),
  verify_default_card(Result#stripe_customer.active_card, check).

get_customer() ->
  Customer = get(current_customer),
  Result = ?debugTime("Fetching customer",
    stripe:customer_get(Customer)),
  ?debugFmt("Customer ID: ~p~n", [Result#stripe_customer.id]),
  verify_default_card(Result#stripe_customer.active_card, check).

charge_customer() ->
  Customer = get(current_customer),
  Desc = <<"CHARGE ALL THE THINGS">>,
  Result = ?debugTime("Charging customer",
    stripe:charge_customer(6221, usd, Customer, Desc)),
  ?debugFmt("Customer Charge ID: ~p~n", [Result#stripe_charge.id]),
  ?assertEqual(Desc, Result#stripe_charge.description),
  ?assertEqual(6221, Result#stripe_charge.amount),
  ?assertEqual(usd, Result#stripe_charge.currency),
  ?assertEqual(210, Result#stripe_charge.fee),
  ?assertEqual(true, Result#stripe_charge.paid),
  ?assertEqual(false, Result#stripe_charge.refunded).

update_customer() ->
  create_token(),
  Token = get(current_token),
  Email = "h222okum@pokum.com",
  Result = ?debugTime("Updating customer",
    stripe:customer_update(get(current_customer), Token, Email)),
  ?debugFmt("Customer ID: ~p~n", [Result#stripe_customer.id]),
  verify_default_card(Result#stripe_customer.active_card, check).

%%%----------------------------------------------------------------------
%%% Meta Tests
%%%----------------------------------------------------------------------
verify_default_card(Card, CheckCVC) ->
  case CheckCVC of
    nocheck -> ?assertEqual('Not Returned by API', Card#stripe_card.cvc_check);
      check -> ?assertEqual(pass, Card#stripe_card.cvc_check)
  end,
  ?assertEqual(12, Card#stripe_card.exp_month),
  ?assertEqual(2021, Card#stripe_card.exp_year),
  ?assertEqual(<<"4242">>, Card#stripe_card.last4),
  ?assertEqual(<<"Visa">>, Card#stripe_card.type).

%%%----------------------------------------------------------------------
%%% Setup / Cleanup
%%%----------------------------------------------------------------------
setup() ->
  inets:start(),
  ssl:start(),
  % Prime the inets/ssl code path with a https request to google:
  httpc:request("https://google.com"),
  application:start(stripe),
  application:set_env(stripe, auth_token, "vtUQeOtUnYr7PGCLQ96Ul4zqpDUO4sOE"),
  ok.

teardown(_) ->
  ssl:stop(),
  inets:stop(),
  ok.
