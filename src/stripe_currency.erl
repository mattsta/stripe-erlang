%% -*- mode: erlang;erlang-indent-level: 2;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=2 sw=2 et
-module(stripe_currency).

-include("stripe.hrl").

-export([load/0, load/1]).
-export([exists/1]).
-export([stripe_price/3]).
-export([stripe_price_all/2]).
-export([convert/3]).

-type currency_dict() :: dict:dict(binary(), float()).
%%%--------------------------------------------------------------------
%%% Currency Loading
%%%--------------------------------------------------------------------
-spec load() -> currency_dict().
load() ->
    {ok, Filename} = application:get_env(stripe, currency_json),
    load(Filename).

-spec load(file:filename_all()) -> currency_dict().
load(Filename) ->
    {ok, JSON} = file:read_file(Filename),
    Proplist = mochijson2:decode(JSON, [{format, proplist}]),
    CD = dict:from_list(proplists:get_value(<<"rates">>, Proplist)),
    application:set_env(stripe, currency_dict, CD),
    CD.

exists(Currency) when is_binary(Currency) ->
    case dict:find(Currency, dict()) of
      {ok, _} -> true;
        error -> false
    end.

%%%--------------------------------------------------------------------
%%% Currency Conversion
%%%--------------------------------------------------------------------
-spec stripe_price(binary(), binary(), price()) -> price().
stripe_price(From, To, FromPriceCents) when is_atom(From) andalso
    is_binary(To) andalso is_integer(FromPriceCents) ->
    stripe_price(atom_to_binary(From, utf8), To, FromPriceCents);

stripe_price(From, To, FromPriceCents) when is_atom(From) andalso
    is_atom(To) andalso is_integer(FromPriceCents) ->
    stripe_price(atom_to_binary(From, utf8), atom_to_binary(To, utf8),
                 FromPriceCents);

stripe_price(From, To, FromPriceCents) when is_list(From) andalso
    is_list(To) andalso is_integer(FromPriceCents) ->
    stripe_price(list_to_binary(From), list_to_binary(To), FromPriceCents);

% If we aren't performing a conversion, don't convert or add the extra 2%
stripe_price(From, From, FromPriceCents) -> FromPriceCents;
stripe_price(From, To, FromPriceCents) when is_binary(From) andalso
    is_binary(To) andalso is_integer(FromPriceCents) ->
    Dict = dict(),

    % Get base currency
    Base = dict:fetch(From, Dict),

    % Get target currency
    Target = dict:fetch(To, Dict),

    case is_zerocents_currency(From) of
       true -> throw({unsupported, zerocents_base_currency});
      false -> ok
    end,

    adjust_for_zerocents(To, convert(Base, Target, FromPriceCents)).


convert(BaseRate, TargetRate, BasePrice) when is_number(BaseRate)
    andalso is_number(TargetRate) andalso is_number(BasePrice) ->
    (TargetRate/BaseRate) * BasePrice.

adjust_for_zerocents(To, ConvertedCents) ->
   PotentialZeroCents = case is_zerocents_currency(To) of
                           true -> ConvertedCents / 100;
                          false -> ConvertedCents
                        end,

   % Add a fudge factor of 2.0% rounded up to the next whole unit
   % of currency because Stripe charges an additional 2% for
   % non-USD transactions.
   trunc((PotentialZeroCents * 1.02) + 1).

%%%--------------------------------------------------------------------
%%% Bulk Currency Conversion
%%%--------------------------------------------------------------------
stripe_price_all(From, FromPriceCents) ->
    Dict = dict(),
    BaseRate = dict:fetch(From, Dict),
    dict:map(fun(To, Target) ->
        Converted = convert(BaseRate, Target, FromPriceCents),
        adjust_for_zerocents(To, Converted)
    end, Dict).

%%%--------------------------------------------------------------------
%%% Helpers
%%%--------------------------------------------------------------------
% List taken from https://support.stripe.com/questions/which-zero-decimal-currencies-does-stripe-support
is_zerocents_currency(<<"bif">>) -> true;
is_zerocents_currency(<<"djf">>) -> true;
is_zerocents_currency(<<"jpy">>) -> true;
is_zerocents_currency(<<"krw">>) -> true;
is_zerocents_currency(<<"pyg">>) -> true;
is_zerocents_currency(<<"vuv">>) -> true;
is_zerocents_currency(<<"xof">>) -> true;
is_zerocents_currency(<<"clp">>) -> true;
is_zerocents_currency(<<"gnf">>) -> true;
is_zerocents_currency(<<"kmf">>) -> true;
is_zerocents_currency(<<"mga">>) -> true;
is_zerocents_currency(<<"rwf">>) -> true;
is_zerocents_currency(<<"xaf">>) -> true;
is_zerocents_currency(<<"xpf">>) -> true;
is_zerocents_currency(_Z) when is_binary(_Z) -> false.

dict() ->
  case application:get_env(stripe, currency_dict) of
    undefined -> load();
      {ok, D} -> D
  end.
