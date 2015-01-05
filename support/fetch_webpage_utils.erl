-module(fetch_webpage_utils).
-author("Arthur Clemens <arthurclemens@gmail.com>").

-export([
    make_absolute_urls/2,
    make_absolute_url/2,
    strip_query_params/1,
    strip_query_param/1,
    substitute_spaces/1,
    substitute_space/1
]).

-include_lib("zotonic.hrl").

-define(DEFAULT_PROTOCOL, <<"http:">>).


make_absolute_urls(UrlContext, Urls) ->
    lists:map(fun(Url) ->
        make_absolute_url(UrlContext, Url)
    end, Urls).

    
%% Creates absolute URLs from local URLs
-spec make_absolute_url(UrlContext, Url) -> binary() when
    UrlContext :: {binary(), binary()},
    Url :: binary().
make_absolute_url(_UrlContext, Url = <<"//", _UrlComponent/binary>>) ->
    % url starts with double slash: add protocol
    <<?DEFAULT_PROTOCOL/binary, Url/binary>>;
make_absolute_url({Root, _Context}, Url = <<"/", _UrlComponent/binary>>) ->
    <<Root/binary, Url/binary>>;
make_absolute_url(_UrlContext, Url = <<"https://", _UrlComponent/binary>>) ->
    Url;
make_absolute_url(_UrlContext, Url = <<"http://", _UrlComponent/binary>>) ->
    Url;
make_absolute_url({Root, Context}, Url) ->
    <<Root/binary, Context/binary, "/", Url/binary>>.


-spec strip_query_params(Urls) -> list(binary()) when
    Urls :: list(binary()).
strip_query_params(Urls) ->
    lists:map(fun(Url) ->
        strip_query_param(Url)
    end, Urls).


-spec strip_query_param(Url) -> binary() when
    Url :: binary().
strip_query_param(<<>>) ->
    <<>>;
strip_query_param(Url) ->
    case binary:split(Url, <<"?">>) of
        [FirstPart, _] -> FirstPart;
        [Entire] -> Entire
    end.



-spec substitute_spaces(Urls) -> list(binary()) when
    Urls :: list(binary()).
substitute_spaces(Urls) ->
    lists:map(fun(Url) ->
        substitute_space(Url)
    end, Urls).
    
    
-spec substitute_space(Url) -> binary() when
    Url :: binary().
substitute_space(<<>>) ->
    <<>>;
substitute_space(Url) ->
    binary:replace(Url, [<<" ">>], <<"%20">>, [global]).
