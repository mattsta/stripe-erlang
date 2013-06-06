stripe-erlang: stripe client.  erlang flavored.
===============================================

Status
------
stripe-erlang is a minimal stripe client.  Currently we support these
operations: turning a credit card into an opaque token, turning a bank account
into an opaque token, creating a customer with a token,
charging a customer, charging a token, creating a pay-out recipient,
updating details of a recipient, transferring money to a recipient, updating
a subscription, canceling a subscription, and retrieving customers, events,
and invoiceitems.

If you need refunds or transaction management, use the delightful stripe.com
admin interface for now.

Usage
-----
### Quick Start
    Eshell V5.9.3.1  (abort with ^G)
    1> inets:start().
    ok
    2> ssl:start().
    ok
    3> application:start(stripe), application:set_env(stripe, auth_token, "vtUQeOtUnYr7PGCLQ96Ul4zqpDUO4sOE").
    ok
    4> rr(stripe).
    [stripe_card,stripe_charge,stripe_customer,stripe_error,
     stripe_event,stripe_invoiceitem,stripe_plan,
     stripe_subscription,stripe_token]
    5> #stripe_token{id = Token} = stripe:token_create("4242424242424242", 12, 2021, 123, [], [], [], [], [], []).
    #stripe_token{id = <<"tok_1Lok1HEM0RJlE5">>,
                  currency = 'Not Returned by API',used = false,
                  amount = <<"Not Returned by API">>,livemode = false,
                  card = #stripe_card{last4 = <<"4242">>,exp_year = 2021,
                                      exp_month = 12,type = <<"Visa">>,
                                      cvc_check = 'Not Returned by API',
                                      address_line1_check = 'Not Returned by API',
                                      address_zip_check = 'Not Returned by API',
                                      country = <<"US">>}}
    6> stripe:charge_card(5000, usd, Token, "Mah Money").
    #stripe_charge{id = <<"ch_1Lok7sGzzioHBA">>,
                   created = 1361721296,amount = 5000,fee = 175,currency = usd,
                   description = <<"Mah Money">>,livemode = false,paid = true,
                   refunded = false,customer = null,
                   card = #stripe_card{last4 = <<"4242">>,exp_year = 2021,
                                   exp_month = 12,type = <<"Visa">>,cvc_check = pass,
                                   address_line1_check = null,address_zip_check = null,
                                   country = <<"US">>}}


### Configuration
You must start `inets` and `ssl` before using `stripe`.

By default, erlang-stripe uses the global stripe API test token.
You *must* change this to your private API key before you can receive payments:

     inets:start(),
     ssl:start(),
     application:start(stripe),
     application:set_env(stripe, auth_token, "vtUQeOtUnYr7PGCLQ96Ul4zqpDUO4sOE").

Building
--------
        rebar get-deps
        rebar compile

Testing
-------
        rebar eunit skip_deps=true suite=stripe

Next Steps
----------
In no specific order:

* Flesh out remaining stripe API
  * Adding a new API feature requires adding: exported user function, request wrapper, result to record extractor.
  * Add support for the card `fingerprint` element in relevant places.
  * Add support for the `fee_details` structure.
  * Add support for the `disputed` flag.
  * Add support for the `failure_message` note.
* Add tests for error conditions
* Move from env-specified auth token to something more call specific
  * Options:
    * Per-call auth token (stripe:charge_card(AuthToken, ...))
    * Leave env, but add per-auth token options

Contributions
-------------
Want to help?  Patches welcome.

* Add a missing API call
  * Add the record with type info in `include/stripe.hrl`
  * Add the exported function in `src/stripe.erl`
  * Add the request_* wrapper function
  * Add a new `json_to_record` result extractor
  * Add positive and negative tests in `test/stripe_tests.erl`
* Find a bug.  Fix a bug.

Contributors
------------
* Thanks to @stefanrusek for fixing a failing test and expanding functionality
