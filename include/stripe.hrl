%%%--------------------------------------------------------------------
%%% Types
%%%--------------------------------------------------------------------
% General Types
-type proplist()     :: [{term(), term()}].  % predefined in newer releases
-type amount()       :: pos_integer().  % any amount of money
-type price()        :: 50..99999999.  % valid charge prices. $0.50 to $1M-0.01
-type currency()     :: aed | % united arab emirates dirham
                        afn | % afghan afghani*
                        all | % albanian lek
                        amd | % armenian dram
                        ang | % netherlands antillean gulden
                        aoa | % angolan kwanza*
                        ars | % argentine peso*
                        aud | % australian dollar*
                        awg | % aruban florin
                        azn | % azerbaijani manat
                        bam | % bosnia & herzegovina convertible mark
                        bbd | % barbadian dollar
                        bdt | % bangladeshi taka
                        bgn | % bulgarian lev
                        bif | % burundian franc
                        bmd | % bermudian dollar
                        bnd | % brunei dollar
                        bob | % bolivian boliviano*
                        brl | % brazilian real*
                        bsd | % bahamian dollar
                        bwp | % botswana pula
                        bzd | % belize dollar
                        cad | % canadian dollar*
                        cdf | % congolese franc
                        chf | % swiss franc
                        clp | % chilean peso*
                        cny | % chinese renminbi yuan
                        cop | % colombian peso*
                        crc | % costa rican colón*
                        cve | % cape verdean escudo*
                        czk | % czech koruna*
                        djf | % djiboutian franc*
                        dkk | % danish krone
                        dop | % dominican peso
                        dzd | % algerian dinar
                        eek | % estonian kroon*
                        egp | % egyptian pound
                        etb | % ethiopian birr
                        eur | % euro
                        fjd | % fijian dollar
                        fkp | % falkland islands pound*
                        gbp | % british pound
                        gel | % georgian lari
                        gip | % gibraltar pound
                        gmd | % gambian dalasi
                        gnf | % guinean franc*
                        gtq | % guatemalan quetzal*
                        gyd | % guyanese dollar
                        hkd | % hong kong dollar
                        hnl | % honduran lempira*
                        hrk | % croatian kuna
                        htg | % haitian gourde
                        huf | % hungarian forint
                        idr | % indonesian rupiah
                        ils | % israeli new sheqel
                        inr | % indian rupee*
                        isk | % icelandic króna
                        jmd | % jamaican dollar
                        jpy | % japanese yen
                        kes | % kenyan shilling
                        kgs | % kyrgyzstani som
                        khr | % cambodian riel
                        kmf | % comorian franc
                        krw | % south korean won
                        kyd | % cayman islands dollar
                        kzt | % kazakhstani tenge
                        lak | % lao kip*
                        lbp | % lebanese pound
                        lkr | % sri lankan rupee
                        lrd | % liberian dollar
                        lsl | % lesotho loti
                        ltl | % lithuanian litas
                        lvl | % latvian lats
                        mad | % moroccan dirham
                        mdl | % moldovan leu
                        mga | % malagasy ariary
                        mkd | % macedonian denar
                        mnt | % mongolian tögrög
                        mop | % macanese pataca
                        mro | % mauritanian ouguiya
                        mur | % mauritian rupee*
                        mvr | % maldivian rufiyaa
                        mwk | % malawian kwacha
                        mxn | % mexican peso*
                        myr | % malaysian ringgit
                        mzn | % mozambican metical
                        nad | % namibian dollar
                        ngn | % nigerian naira
                        nio | % nicaraguan córdoba*
                        nok | % norwegian krone
                        npr | % nepalese rupee
                        nzd | % new zealand dollar
                        pab | % panamanian balboa*
                        pen | % peruvian nuevo sol*
                        pgk | % papua new guinean kina
                        php | % philippine peso
                        pkr | % pakistani rupee
                        pln | % polish złoty
                        pyg | % paraguayan guaraní*
                        qar | % qatari riyal
                        ron | % romanian leu
                        rsd | % serbian dinar
                        rub | % russian ruble
                        rwf | % rwandan franc
                        sar | % saudi riyal
                        sbd | % solomon islands dollar
                        scr | % seychellois rupee
                        sek | % swedish krona
                        sgd | % singapore dollar
                        shp | % saint helenian pound*
                        sll | % sierra leonean leone
                        sos | % somali shilling
                        srd | % surinamese dollar*
                        std | % são tomé and príncipe dobra
                        svc | % salvadoran colón*
                        szl | % swazi lilangeni
                        thb | % thai baht
                        tjs | % tajikistani somoni
                        top | % tongan paʻanga
                        'try' | % turkish lira
                        ttd | % trinidad and tobago dollar
                        twd | % new taiwan dollar
                        tzs | % tanzanian shilling
                        uah | % ukrainian hryvnia
                        ugx | % ugandan shilling
                        usd | % united states dollar
                        uyi | % uruguayan peso*
                        uzs | % uzbekistani som
                        vef | % venezuelan bolívar*
                        vnd | % vietnamese Đồng
                        vuv | % vanuatu vatu
                        wst | % samoan tala
                        xaf | % central african cfa franc
                        xcd | % east caribbean dollar
                        xof | % west african cfa franc*
                        xpf | % cfp franc*
                        yer | % yemeni rial
                        zar | % south african rand
                        zmw.  % zambian kwacha
-type customer_id()  :: binary(). % cu_* | cus_*  (docs show both in use)
-type coupon_id()    :: binary(). % user specidied coupon ID
-type plan_id()      :: binary(). % user specified plan ID
-type charge_id()    :: binary(). % ch_*
-type token_id()     :: binary(). % tok_* (card) or btok_* (bank)
-type invoiceitem_id() :: binary(). % ii_*
-type invoice_id()     :: binary(). % in_*
-type recipient_id() :: binary().   % rp_*
-type transfer_id()  :: binary().   % tr_*
-type balance_txn()  :: binary().   % txn_*
-type bank_name()    :: binary() | string().
-type last4()        :: binary() | string().
-type name()         :: binary() | string().
-type desc()         :: binary() | string().
-type email()        :: binary() | string().
-type fingerprint()  :: binary() | string().
-type json()         :: tuple().
-type country()      :: binary().
-type epoch()        :: pos_integer().
-type check_result() :: pass | fail | unchecked.
-type credit_provider() :: binary().
-type stripe_object_name() :: charge | customer | invoice | invoiceitem |
                              plan | token.
-type event_id() :: binary() | string().
-type event_type() :: binary() | string().

-type token_type() :: card | bank_account.

-type recipient_type() :: individual | corporation.

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

-type transfer_status() :: paid | pending | failed.

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
                      brand      :: credit_provider(),
                      cvc_check  :: check_result(),
                      address_line1_check :: check_result(),
                      address_zip_check   :: check_result(),
                      country    :: country()
                     }).

-record(stripe_bank_account, {fingerprint :: fingerprint(),
                              bank_name   :: bank_name(),
                              last4       :: last4(),
                              country     :: country(),
                              validated   :: boolean(),
                              description :: desc(),
                              recipient   :: recipient_id(),
                              statement_descriptor :: desc()}).

-record(stripe_charge, {id          :: charge_id(),
                        created     :: epoch(),
                        amount      :: price(),
                        balance_transaction :: balance_txn(),
                        currency    :: currency(),
                        description :: desc(),
                        livemode    :: boolean(),
                        paid        :: boolean(),
                        refunded    :: boolean(),
                        customer    :: customer_id(),
                        failure_code :: string(),
                        failure_message :: string(),
                        card        :: #stripe_card{}
                      }).

-record(stripe_token, {id        :: token_id(),
                       used      :: boolean(),
                       livemode  :: boolean(),
                       type      :: token_type(),
                       bank_account :: #stripe_bank_account{},
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

-record(stripe_subscription, {id                   :: binary(),
                              status               :: atom(),
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
                        currency           :: currency(),
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
                          email           :: email(),
                          delinquent      :: boolean(),
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

-record(stripe_recipient, {id            :: recipient_id(),
                           created       :: epoch(),
                           type          :: recipient_type(),
                           active_account :: #stripe_bank_account{},
                           verified      :: boolean(),
                           description   :: desc(),
                           name          :: name(),
                           email         :: email()}).

-record(stripe_transfer, {id             :: transfer_id(),
                          amount         :: amount(),
                          currency       :: currency(),
                          date           :: epoch(),
                          balance_transaction :: balance_txn(),
                          status         :: transfer_status(),
                          account        :: #stripe_bank_account{},
                          description    :: desc(),
                          recipient      :: recipient_id(),
                          statement_descriptor :: desc()}).
