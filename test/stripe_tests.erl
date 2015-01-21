%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=2 sw=2 et
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
     {"Create Credit Card Token",
       fun create_token_card/0},
     {"Create Bank Account Token",
       fun create_token_bank/0},
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
       fun update_customer/0},
     {"Create Recipient",
       fun create_recipient/0},
     {"Update Recipient",
       fun update_recipient/0},
     {"Create Transfer",
       fun create_transfer/0},
     {"Cancel Transfer",
       fun cancel_transfer/0},
     {"Create Invoice Item",
       fun create_invoice_item/0},
     {"Get Invoice Item",
       fun get_invoice_item/0},
     {"Confirm Paginated URL",
       fun verify_paginated_urls/0},
     {"Get all customers",
      fun get_all_customers/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
create_token_card() ->
  Result = ?debugTime("Creating credit card token",
                      stripe:token_create("4242424242424242", 12, 2021, 123,
                                          [], [], [], [], [], [])),
  put(current_card_token, Result#stripe_token.id),
  ?debugFmt("Token ID: ~p~n", [Result#stripe_token.id]),
  ?assertEqual(false, Result#stripe_token.used),
  verify_default_card(Result#stripe_token.card, nocheck).

create_token_bank() ->
  T = ?debugTime("Creating bank account token",
    stripe:token_create_bank("US", "111000025", "000123456789")),
  BankAccountId = T#stripe_token.id,
  ?debugFmt("Token ID: ~p~n", [BankAccountId]),
  put(current_bank_account_token, BankAccountId),
  ?assertEqual(false, T#stripe_token.used).

charge_token() ->
  Token = get(current_card_token),
  Desc = <<"MAH CHARGE">>,
  Result = ?debugTime("Charging card",
    stripe:charge_card(65540, usd, Token, Desc)),
  ?debugFmt("Token Charge ID: ~p~n", [Result#stripe_charge.id]),
  ?assertEqual(65540, Result#stripe_charge.amount),
  ?assertEqual(usd, Result#stripe_charge.currency),
  ?assertEqual(true, is_binary(Result#stripe_charge.balance_transaction)),
  ?assertEqual(true, Result#stripe_charge.paid),
  ?assertEqual(false, Result#stripe_charge.refunded),
  ?assertEqual(Desc, Result#stripe_charge.description),
  verify_default_card(Result#stripe_charge.card, check).

create_min_customer() ->
  Result = ?debugTime("Creating minimum customer",
    stripe:customer_create("", "", "")),
  ?debugFmt("Customer ID: ~p~n", [Result#stripe_customer.id]).

create_customer() ->
  create_token_card(),
  Token = get(current_card_token),
  Email = "hokum@pokum.com",
  Desc = "You really don't wanna know",
  Result = ?debugTime("Creating customer",
    stripe:customer_create(Token, Email, Desc)),
  put(current_customer, Result#stripe_customer.id),
  ?debugFmt("Customer ID: ~p~n", [Result#stripe_customer.id]),
  ?assertEqual(list_to_binary(Desc), Result#stripe_customer.description).

get_customer() ->
  Customer = get(current_customer),
  Result = ?debugTime("Fetching customer",
    stripe:customer_get(Customer)),
  ?debugFmt("Customer ID: ~p~n", [Result#stripe_customer.id]).

charge_customer() ->
  Customer = get(current_customer),
  Desc = <<"CHARGE ALL THE THINGS">>,
  Result = ?debugTime("Charging customer",
    stripe:charge_customer(6221, usd, Customer, Desc)),
  ?debugFmt("Customer Charge ID: ~p~n", [Result#stripe_charge.id]),
  ?assertEqual(Desc, Result#stripe_charge.description),
  ?assertEqual(6221, Result#stripe_charge.amount),
  ?assertEqual(usd, Result#stripe_charge.currency),
  ?assertEqual(true, is_binary(Result#stripe_charge.balance_transaction)),
  ?assertEqual(true, Result#stripe_charge.paid),
  ?assertEqual(false, Result#stripe_charge.refunded).

update_customer() ->
  create_token_card(),
  Token = get(current_card_token),
  Email = "h222okum@pokum.com",
  Result = ?debugTime("Updating customer",
    stripe:customer_update(get(current_customer), Token, Email)),
  ?debugFmt("Customer ID: ~p~n", [Result#stripe_customer.id]).

create_recipient() ->
  % Create a bank account token first so we can create a recipient with a way
  % to receive payments
  BankAccountId = get(current_bank_account_token),
  R = ?debugTime("Creating recipient",
    stripe:recipient_create("Bob Jones", individual,
                            "000000000", BankAccountId, "bob@bob.bob", "A Desc")),
  put(recipient_id, R#stripe_recipient.id),
  ?debugFmt("Recipient ID: ~p~n", [R#stripe_recipient.id]),
  ?assertEqual(individual, R#stripe_recipient.type),
  ?assertEqual(<<"Bob Jones">>, R#stripe_recipient.name),
  ?assertEqual(true, R#stripe_recipient.verified),
  ?assertEqual(<<"bob@bob.bob">>, R#stripe_recipient.email).

update_recipient() ->
  RecipientId = get(recipient_id),
  R = ?debugTime("Updating recipient",
    stripe:recipient_update(RecipientId, "Bob2 Jones2", [], [], "email2@2.com", [])),
  ?debugFmt("Recipient ID: ~p~n", [R#stripe_recipient.id]),
  ?assertEqual(<<"Bob2 Jones2">>, R#stripe_recipient.name),
  ?assertEqual(<<"email2@2.com">>, R#stripe_recipient.email).

create_transfer() ->
  T = ?debugTime("Creating transfer",
    stripe:transfer_create(6500000, usd, get(recipient_id), "Foo", "Prell")),
  case is_record(T, stripe_transfer) of
      true ->
          put(transfer_id, T#stripe_transfer.id),
          ?debugFmt("Transfer ID: ~p~n", [T#stripe_transfer.id]),
          ?assertEqual(pending, T#stripe_transfer.status),
          ?assertEqual(6500000, T#stripe_transfer.amount),
          ?assertEqual(usd, T#stripe_transfer.currency),
          ?assertEqual(true, is_binary(T#stripe_transfer.balance_transaction));
      false ->
          {error, Reason} = T,
          ?debugFmt("Transfer failed: ~p~n", [Reason])
  end.


cancel_transfer() ->
  TransferId = get(transfer_id),
  T = ?debugTime("Canceling transfer",
    stripe:transfer_cancel(TransferId)),
  ?debugFmt("Transfer ID: ~p~n", [TransferId]),
  % This goes through but fails because the previous transfer is
  % considred automatic... or something.
  ?assertMatch(#stripe_error{}, T).

create_invoice_item() ->
  Customer = get(current_customer),
  Desc = <<"INVOICE ALL THE THINGS">>,
  Result = ?debugTime("Adding invoice item to customer",
    stripe:invoiceitem_create(Customer, 12345, usd,  Desc)),
  ?debugFmt("Invoice Item ID: ~p~n", [Result#stripe_invoiceitem.id]),
  ?assertEqual(Desc, Result#stripe_invoiceitem.description),
  ?assertEqual(12345, Result#stripe_invoiceitem.amount),
  ?assertEqual(usd, Result#stripe_invoiceitem.currency),
  ?assertEqual(Customer, Result#stripe_invoiceitem.customer),
  put(invoice_id, Result#stripe_invoiceitem.id).

get_invoice_item() ->
  InvoiceID = get(invoice_id),
  Customer = get(current_customer),
  Desc = <<"INVOICE ALL THE THINGS">>,
  Result = ?debugTime("Fetching previously created invoice item",
                      stripe:invoiceitem(InvoiceID)),
  case is_record(Result, stripe_invoiceitem) of
      true ->
          ?debugFmt("Invoice Item ID: ~p~n", [Result#stripe_invoiceitem.id]),
          ?assertEqual(Desc, Result#stripe_invoiceitem.description),
          ?assertEqual(12345, Result#stripe_invoiceitem.amount),
          ?assertEqual(usd, Result#stripe_invoiceitem.currency),
          ?assertEqual(Customer, Result#stripe_invoiceitem.customer);
      false ->
          {error, Reason} = Result,
          ?debugFmt("Transfer failed: ~p~n", [Reason])
  end.

verify_paginated_urls() ->
    Result = ?debugTime("Trying paginated url/1", stripe:gen_paginated_url(customers)),
    ?debugFmt("Result was: ~p~n", [Result]),
    ?assertEqual(Result, "https://api.stripe.com/v1/customers?limit=10"),
    Result1 = ?debugTime("Trying paginated url/2", stripe:gen_paginated_url(invoices, 50)),
    ?debugFmt("Result was: ~p~n", [Result1]),
    ?assertEqual(Result1, "https://api.stripe.com/v1/invoices?limit=50"),
    Result2 = ?debugTime("Trying paginated url/3", stripe:gen_paginated_url(charges, 100, "cus_123")),
    ?debugFmt("Result was: ~p~n", [Result2]),
    ?assertEqual(Result2, "https://api.stripe.com/v1/charges?limit=100&starting_after=cus_123"),
    Result3 = ?debugTime("Trying paginated url/4", stripe:gen_paginated_url(customers, 50, "cus_123", "cus_456")),
    ?debugFmt("Result was: ~p~n", [Result3]),
    ?assertEqual(Result3, "https://api.stripe.com/v1/customers?limit=50&starting_after=cus_123&ending_before=cus_456").

get_all_customers() ->
    Result = ?debugTime("Trying to get all customers", stripe:get_all_customers()),
    ?debugFmt("Result was: ~p~n", [Result]).



%%%----------------------------------------------------------------------
%%% Meta Tests
%%%----------------------------------------------------------------------
verify_default_card(Card, CheckCVC) ->
  case CheckCVC of
    nocheck -> ?assertEqual(unchecked, Card#stripe_card.cvc_check);
      check -> ?assertEqual(pass, Card#stripe_card.cvc_check)
  end,
  ?assertEqual(12, Card#stripe_card.exp_month),
  ?assertEqual(2021, Card#stripe_card.exp_year),
  ?assertEqual(<<"4242">>, Card#stripe_card.last4),
  ?assertEqual(<<"Visa">>, Card#stripe_card.brand).

%%%----------------------------------------------------------------------
%%% Setup / Cleanup
%%%----------------------------------------------------------------------
setup() ->
  inets:start(),
  ssl:start(),
  % Prime the inets/ssl code path with a https request to google:
  httpc:request("https://google.com"),
  application:start(stripe),
  application:set_env(stripe, auth_token, "sk_test_BQokikJOvBiI2HlWgH4olfQ2"),
  ok.

teardown(_) ->
  ssl:stop(),
  inets:stop(),
  ok.
