%%%--------------------------------------------------------------------
%%% Types
%%%--------------------------------------------------------------------
% General Types
-type proplist()     :: [{term(), term()}].  % predefined in newer releases
-type amount()       :: pos_integer().  % any amount of money
-type price()        :: 50..500000000.  % valid charge prices. $0.50 to $5M.
-type currency()     :: usd.
-type customer_id()  :: binary(). % cu_* | cus_*  (docs show both in use)
-type coupon_id()    :: binary(). % user specidied coupon ID
-type plan_id()      :: binary(). % user specified plan ID
-type charge_id()    :: binary(). % ch_*
-type token_id()     :: binary(). % tok_*
-type invoiceitem_id() :: binary(). % ii_*
-type invoice_id()     :: binary(). % in_*
-type name()         :: binary() | string().
-type desc()         :: binary() | string().
-type email()        :: binary() | string().
-type json()         :: tuple().
-type country()      :: binary().
-type epoch()        :: pos_integer().
-type check_result() :: pass | fail | unchecked.
-type credit_provider() :: binary().
-type stripe_object_name() :: charge | customer | invoice | invoiceitem |
                              plan | token.
-type event_id() :: binary() | string().
-type event_type() :: binary() | string().

% Endpoints
-type action() :: charges | customers | tokens.

% Error Types
-type payment_error() :: invalid_number | incorrect_number |
                         invalid_expiry_month | invalid_expiry_year |
                         invalid_cvc | expired_card | invalid_amount |
                         incorrect_cvc | card_declined | missing |
                         duplicate_transaction | processing_error.
-type error_json() :: json().
-type error_msg()  :: binary().
-type http_error_meaning() :: missing_param | bad_api_key |
                              params_ok_but_request_failed |
                              notfound | stripe_server_error |
                              stripe_api_error | unknown_error.

%%%--------------------------------------------------------------------
%%% Error
%%%--------------------------------------------------------------------
-record(stripe_error, {type    :: card_error | invalid_request_error |
                                  api_error,
                       code    :: payment_error(),
                       message :: error_msg(),
                       param   :: binary(),
                       http_error_code :: 400..499,
                       http_error_code_meaning :: http_error_meaning()
                      }).

%%%--------------------------------------------------------------------
%%% Records / Stripe Objects
%%%--------------------------------------------------------------------
-record(stripe_card, {name       :: name(),
                      last4      :: binary(),
                      exp_year   :: 2011..3000,
                      exp_month  :: 1..12,
                      type       :: credit_provider(),
                      cvc_check  :: check_result(),
                      address_line1_check :: check_result(),
                      address_zip_check   :: check_result(),
                      country    :: country()
                     }).

-record(stripe_charge, {id          :: charge_id(),
                        created     :: epoch(),
                        amount      :: price(),
                        fee         :: amount(),
                        currency    :: currency(),
                        description :: desc(),
                        livemode    :: boolean(),
                        paid        :: boolean(),
                        refunded    :: boolean(),
                        customer    :: customer_id(),
                        card        :: #stripe_card{}
                      }).

-record(stripe_token, {id        :: token_id(),
                       currency  :: currency(),
                       used      :: boolean(),
                       amount    :: amount(),
                       livemode  :: boolean(),
                       card      :: #stripe_card{}
                      }).

-record(stripe_plan, {id             :: plan_id(),
                      currency       :: currency(),
                      interval_count :: pos_integer(),
                      name           :: binary(),
                      amount         :: amount(),
                      interval       :: binary(),
                      livemode       :: boolean()
                     }).

-record(stripe_subscription, {status        :: atom(),
                              current_period_start :: epoch(),
                              current_period_end   :: epoch(),
                              trial_start          :: epoch(),
                              trial_end            :: epoch(),
                              ended_at             :: epoch(),
                              canceled_at          :: epoch(),
                              customer             :: customer_id(),
                              start                :: epoch(),
                              quantity             :: number(),
                              plan                 :: #stripe_plan{}
                             }).

-record(stripe_coupon, {id                 :: coupon_id(),
                        percent_off        :: amount(),
                        amount_off         :: amount(),
                        currentcy          :: currency(),
                        duration           :: amount(),
                        redeem_by          :: epoch(),
                        max_redemptions    :: amount(),
                        times_redeemed     :: amount(),
                        duration_in_months :: amount()
                       }).

-record(stripe_discount, {coupon   :: #stripe_coupon{},
                          start    :: epoch(),
                          'end'    :: epoch(),
                          customer :: customer_id()
                         }).

-record(stripe_customer, {id              :: customer_id(),
                          created         :: epoch(),
                          description     :: desc(),
                          livemode        :: boolean(),
                          active_card     :: #stripe_card{},
                          email           :: email(),
                          delinquent      :: boolean(),
                          subscription    :: #stripe_subscription{},
                          discount        :: #stripe_discount{},
                          account_balance :: amount()
                         }).

-record(stripe_event, {id      :: event_id(),
                       type    :: event_type(),
                       created :: epoch(),
                       data}).

-record(stripe_invoiceitem, {id          :: invoice_id(),
                             amount      :: amount(),
                             currency    :: currency(),
                             customer    :: customer_id(),
                             date        :: epoch(),
                             description :: binary(),
                             proration   :: boolean()}).
