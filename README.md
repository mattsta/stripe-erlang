stripe-erlang: stripe client.  erlang flavored.
===============================================

[![Build Status](https://secure.travis-ci.org/mattsta/stripe-erlang.png)](http://travis-ci.org/mattsta/stripe-erlang)

Status
------
stripe-erlang is a minimal stripe client.  Currently we support these
operations:

  - turning a credit card into an opaque token
  - turning a bank account into an opaque token
  - creating a customer with a token
  - charging a customer
  - charging in any supported currency
  - charging a token
  - creating a pay-out recipient
  - updating details of a recipient
  - transferring money to a recipient
  - updating a subscription
  - canceling a subscription
  - retrieving customers, events, and invoiceitems

If you need refunds or transaction management, use the delightful stripe.com
admin interface for now.

Usage
-----
### Quick Start
```erlang
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
```

### Configuration
You must start `inets` and `ssl` before using `stripe`.

By default, erlang-stripe uses the global stripe API test token.
You *must* change this to your private API key before you can receive payments:

```erlang
inets:start(),
ssl:start(),
application:start(stripe),
application:set_env(stripe, auth_token, "sk_test_BQokikJOvBiI2HlWgH4olfQ2").
```
Branch Naming Policy
--------------------
All Stripe API endpoints begin with `/v1/` and when I built this application,
Stripe was much younger and didn't have huge API changes yet.  Crazy me thought
by naming their API endpoint `/v1/` they would rev the version number for changes.

Instead, Stripe decided to use date-based
versions.  So, the Stripe API for your account is `/v1/` combined with the most
recent `YYYY-MM-DD` API revision you've used.

If this were a better organized project, we'd have branches or tags against each
individual API change.  But, we're not that well organized.

For now, we aim to always support the most recent Stripe API release defined at
their [API upgrades](https://stripe.com/docs/upgrades) page.  If the most recent
Stripe API has drifted from our implementation here, please file an issue or make
a pull request so we can keep everything working.

Currencies
----------
Stripe added multiple currency support, but how do you figure out your
prices in all the different currencies?

The stripe_currency module supports reading a JSON format of currency
abbreviation to exchange rates.  You can get a currency JSON
by signing up for free at [open exchange rates](https://openexchangerates.org/).

Their free plan allow 1,000 complete exchange rate retrievals every month. You
can easily cache their exchange rate output to stay under their monthly free limit.

For easier ingestion of the currency JSON,
use the currencies script in `priv/` to download the the JSON and
format it correctly.

You can load your currency file by putting the file path in an app environment
variable for `stripe` called `currency_json` or you can load the file directly
with `stripe_currency:load(Filename)`.  Loading the currency JSON will cache
it in the app as well.

You can convert prices using the `stripe_price/4` function:
`stripe_currency:stripe_price(usd, eur, 500)` would convert 5.00 USD into EUR.  Remember,
Stripe prices are in cents — unless — the currency has no cent denomination like
the Yen.  In that case, we can convert *to* a zero cent currency, but not *from* one
for the time being.

**Note:** The `stripe_price` function automatically adds 2% to the final converted
price to cover the additional fee charged by Stripe for all non-native currency
transactions.

Building
--------
        rebar get-deps
        rebar compile

Testing
-------
        rebar eunit skip_deps=true suite=stripe

The tests automatically use valid Stripe public dev test tokens, so you don't
need to enter any information for the tests to complete successfully.

Sometimes Stripe changes their API behavior.  Changes in API behavior can cause
our tests to fail.  If you find failing tests, check their latest API changes at
[API Upgrades](https://stripe.com/docs/upgrades) then fix the code and/or tests
to compensate.

Next Steps
----------
In no specific order:

* Flesh out remaining stripe API
  * Adding a new API feature requires adding: exported user function, request wrapper, result to record extractor.
  * Add support for the card `fingerprint` element in relevant places.
  * Add support for the `fee_details` structure.
  * Add support for the `disputed` flag.
  * Add support for the `failure_message` note.
  * Add support for multi-valued properties (cards, subscriptions, ...).
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
* Thanks to @michaelpellon for fixing an interface due to the API changing out from under us
