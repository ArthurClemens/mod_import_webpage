-module(mod_import_webpage).
-author("Arthur Clemens <arthurclemens@gmail.com>").

-mod_title("Import webpage").
-mod_description("Facilitates creating a resource from another web page.").
-mod_prio(500).

-export([
    fetch/2,
    fetch/3,
    event/2
]).

-include_lib("zotonic.hrl").

-define(MAX_IMAGES, 25).

%% Returns a list of matches for default rules (title, description, images).
%% Options: remove_duplicates, remove_empty
%% For instance:
%% mod_import_webpage:fetch(<<"http://nyt.com">>, [])
%% returns
%% [{url,<<"http://nyt.com">>},
%%  {size,273759},
%%  {title,[<<"The New York Times - Breaking News, World News & Multimedia">>]},
%%  {description,[<<"The New York Times: Find breaking news, multimedia, reviews & opinion on Washington, busines"...>>]},
%%  {images, [<<"http://static01.nyt.com/images/2014/12/25/us/POLICE/POLICE-largeHorizontal375.jpg">>,
%% etc.

-spec fetch(Url, Options) -> list() when
    Url :: string(),
    Options :: list().
fetch(Url, Options) ->
    fetch_webpage:fetch(Url, Options).


-spec fetch(Url, Options, Rules) -> list() when
    Url :: string(),
    Options :: list({remove_duplicates, true}),
    Rules :: list().
fetch(Url, Rules, Options) ->
    fetch_webpage:fetch(Url, Options, Rules).


event(#submit{message={process_url, [
    {category, Category},
    {ok_template, OkTemplate},
    {ok_target, OkTarget},
    {error_template, ErrorTemplate},
    {error_target, ErrorTarget},
    {collection_id, CollectionId},
    {page_context, PageContext},
    {update_module, UpdateModule},
    {update_fun, UpdateFun}
]}}, Context) ->
    case z_string:trim(z_context:get_q("url", Context)) of
        "" -> 
            feedback_error("IMPORT_FROM_URL_ERROR_EMPTY", ErrorTemplate, ErrorTarget, Context);
        Url ->
            Data = fetch_webpage:fetch(Url, [remove_duplicates, remove_empty]),
            case Data of 
                {error, _Reason} ->
                    feedback_error("IMPORT_FROM_URL_ERROR_INVALID", ErrorTemplate, ErrorTarget, Context);
                _ -> 
                    case proplists:get_value(size, Data) of
                        0 ->
                            feedback_error("IMPORT_FROM_URL_ERROR_NO_DATA", ErrorTemplate, ErrorTarget, Context);
                        _ -> 
                            Images = proplists:get_value(images, Data),
                            UrlContext = proplists:get_value(url_context, Data),
                            Images1 = fetch_webpage_utils:make_absolute_urls(UrlContext, lists:sublist(Images, ?MAX_IMAGES)),
                            Images2 = fetch_webpage_utils:substitute_spaces(Images1),
                            Images3 = lists:map(fun(ImageUrl) ->
                                [<<"">>, ImageUrl] % image_resource, image_url
                            end, Images2),
                            PageUrl = proplists:get_value(url, Data),
                            PageTitle = z_string:trim(proplists:get_value(title, Data)),
                            Summary = z_string:trim(proplists:get_value(summary, Data)),
                            z_render:wire([{replace, [
                                {template, OkTemplate},
                                {target, OkTarget},
                                {category, Category},
                                {url, PageUrl},
                                {page_title, PageTitle},
                                {summary, Summary},
                                {images, Images3},
                                {collection_id, CollectionId},
                                {page_context, PageContext},
                                {update_module, UpdateModule},
                                {update_fun, UpdateFun}
                            ]}], Context)
                    end
            end
    end;

event(#submit{message={create_note, [
    {category, Category},
    {collection_id, CollectionId},
    {page_context, PageContext},
    {update_module, UpdateModule},
    {update_fun, UpdateFun}
]}}, Context) ->
    Title = z_context:get_q("page_title", Context),
    Summary = z_context:get_q("summary", Context),
    ImageCrop = z_convert:to_bool(z_context:get_q("image_crop", Context)),
    File = z_context:get_q("file", Context),
    Options = case File of
        [] -> [];
        F ->
            [{file, F}, {image_crop, ImageCrop}]
    end,
    create_page(Title, Summary, Category, CollectionId, PageContext, UpdateModule, UpdateFun, Options, Context);

event(#submit{message={update_note, [
    {page_id, PageId},
    {collection_id, CollectionId},
    {page_context, PageContext},
    {update_module, UpdateModule},
    {update_fun, UpdateFun}
]}}, Context) ->
    Title = z_context:get_q("page_title", Context),
    Summary = z_context:get_q("summary", Context),
    ImageCrop = z_convert:to_bool(z_context:get_q("image_crop", Context)),
    Image = z_context:get_q("image", Context),
    File = z_context:get_q("file", Context),
    Options = case File of
        [] -> 
            case Image of
                undefined ->
                    [{image, delete}];
                _ -> 
                    % same image passed, do nothing
                    [{image_crop, ImageCrop}]
            end;
        _ -> [{file, File}, {image_crop, ImageCrop}]
    end,
    update_page(PageId, Title, Summary, CollectionId, PageContext, UpdateModule, UpdateFun, Options, Context);
    
event(#submit{message={create_from_url, [
    {category, Category},
    {collection_id, CollectionId},
    {page_context, PageContext},
    {update_module, UpdateModule},
    {update_fun, UpdateFun}
]}}, Context) ->
    Url = z_context:get_q("url", Context),
    Title = z_context:get_q("page_title", Context),
    Summary = z_context:get_q("summary", Context),
    ImageCrop = z_convert:to_bool(z_context:get_q("image_crop", Context)),
    Image = z_context:get_q("image", Context),
    ImageUrl = case Image of
        undefined -> undefined;
        _ -> 
            fetch_webpage_utils:strip_query_param(z_convert:to_binary(Image))
    end,
    create_page(Title, Summary, Category, CollectionId, PageContext, UpdateModule, UpdateFun, [{url, Url}, {image_url, ImageUrl}, {image_crop, ImageCrop}], Context);

event(#submit{message={update_from_url, [
    {page_id, PageId},
    {collection_id, CollectionId},
    {page_context, PageContext},
    {update_module, UpdateModule},
    {update_fun, UpdateFun}
]}}, Context) ->
    Title = z_context:get_q("page_title", Context),
    Summary = z_context:get_q("summary", Context),
    ImageCrop = z_convert:to_bool(z_context:get_q("image_crop", Context)),
    Image = z_context:get_q("image", Context),
    ImageUrl = case is_current_image(Image, PageId, Context) of
        true -> undefined;
        false -> fetch_webpage_utils:strip_query_param(z_convert:to_binary(Image))
    end,
    update_page(PageId, Title, Summary, CollectionId, PageContext, UpdateModule, UpdateFun, [{image_url, ImageUrl}, {image_crop, ImageCrop}], Context);

event(#postback{message={load_images_from_url, [
    {form_id, FormId},
    {url, Url},
    {page_id, PageId}
]}}, Context) ->
    Images = images_for_url(Url),
    case Images of
        [] -> Context;
        _ -> 
            % filter current image
            Images1 = fetch_webpage_utils:strip_query_params(Images),
            Images2 = [Img || Img <- Images1, not is_current_image(Img, PageId, Context)],
            ImageData = lists:map(fun(ImageUrl) ->
                [<<"">>, ImageUrl] % image_resource, image_url
            end, Images2),
            % Update view
            JS = ["mod_import_webpage.showImages('", FormId, "', ", mochijson2:encode(ImageData), ");"],
            z_transport:page(javascript, JS, Context),
            Context
    end.


create_page(Title, Summary, Category, CollectionId, PageContext, UpdateModule, UpdateFun, Options, Context) ->
    RscOpts = [
        {category, z_convert:to_atom(Category)},
        {is_published, true},
        {summary, Summary},
        {title, Title},
        {url, proplists:get_value(url, Options)},
        {image_url, proplists:get_value(image_url, Options)},
        {image_crop, proplists:get_value(image_crop, Options)}
    ],
    RscOpts1 = [{Key, Value} || {Key, Value} <- RscOpts, Value =/= undefined],
    {ok, PageId} = m_rsc:insert(RscOpts1, Context),
    {Context1, MediaId} = case proplists:get_value(file, Options) of
        undefined ->
            case proplists:get_value(image_url, Options) of
                undefined -> {Context, undefined};
                ImageUrl -> upload_url(PageId, ImageUrl, Context)
            end;
        File -> 
            upload_file(PageId, File, Context)
    end,
    % Create edge from new page to collection, if any
    case CollectionId of
        undefined -> ok;
        _ -> m_edge:insert(CollectionId, haspart, PageId, Context1)
    end,
    % Close dialog
    Context2 = z_render:wire({dialog_close, []}, Context1),
    notify_update(UpdateModule, UpdateFun, PageId, CollectionId, MediaId, PageContext, Context2).
    
    
update_page(PageId, Title, Summary, CollectionId, PageContext, UpdateModule, UpdateFun, Options, Context) ->
    case proplists:get_value(image, Options) of
        delete -> delete_media(PageId, Context);
        _ -> ok
    end,            
    {Context1, MediaId} = case proplists:get_value(file, Options) of
        undefined ->
            case proplists:get_value(image_url, Options) of
                undefined -> {Context, get_media(PageId, Context)};
                ImageUrl ->
                    delete_media(PageId, Context),
                    case ImageUrl of
                        <<>> -> {Context, get_media(PageId, Context)};
                        _ -> upload_url(PageId, ImageUrl, Context)
                    end
            end;
        File -> 
            delete_media(PageId, Context),
            upload_file(PageId, File, Context)
    end,
    RscOpts = [
        {summary, Summary},
        {title, Title},
        {image_url, proplists:get_value(image_url, Options, <<"-">>)},
        {image_crop, proplists:get_value(image_crop, Options)}
    ],
    RscOpts1 = [{Key, Value} || {Key, Value} <- RscOpts, Value =/= undefined],
    {ok, _Id} = m_rsc:update(PageId, RscOpts1, Context),
    % Close dialog
    Context2 = z_render:wire({dialog_close, []}, Context1),
    notify_update(UpdateModule, UpdateFun, PageId, CollectionId, MediaId, PageContext, Context2).


get_media(Id, Context) ->
    Depiction = m_media:depiction(Id, Context),
    case Depiction of
        undefined -> undefined;
        _ -> proplists:get_value(id, Depiction)
    end.


delete_media(Id, Context) ->
    lists:foreach(fun(Depiction) ->
        m_rsc:delete(Depiction, Context)
    end, m_edge:objects(Id, depiction, Context)).


upload_file(Id, File, Context) ->
    Upload = #upload{} = File,
    case m_media:insert_file(Upload, [], Context) of
        {ok, MediaId} ->
            m_edge:insert(Id, depiction, MediaId, Context),
            {Context, MediaId};
        {error, _R} ->
            Ctx = z_render:growl_error(?__("ADD_NOTE_ERROR_UPLOAD_MESSAGE", Context), Context),
            {Ctx, undefined}
    end.


upload_url(Id, Url, Context) ->
    {ok, MediaId} = m_media:insert_url(Url, [], Context),
    {ok, _EdgeId} = m_edge:insert(Id, depiction, MediaId, Context),
    {Context, MediaId}.


notify_update(UpdateModule, UpdateFun, PageId, CollectionId, MediaId, PageContext, Context) ->
    case UpdateModule of
        undefined -> Context;
        "" -> Context;
        _ -> 
            Module = z_convert:to_atom(UpdateModule),
            Fun = z_convert:to_atom(UpdateFun),
            rpc:call(node(), Module, Fun, [PageId, CollectionId, MediaId, PageContext, Context])
    end.
    

is_current_image(Image, PageId, Context) ->
    CurrentImage = m_rsc:p(PageId, image_url, Context),
    z_convert:to_list(Image) =:= z_convert:to_list(CurrentImage).


feedback_error(Key, ErrorTemplate, ErrorTarget, Context) ->
    z_render:wire([
        {replace, [{template, ErrorTemplate}, {target, ErrorTarget}, {message, ?__(Key, Context)}]}
    ], Context).


images_for_url(Url) ->
    Data = fetch_webpage:fetch(Url, [remove_duplicates, remove_empty], [fetch_webpage:default_rule(images)]),
    case Data of 
        {error, _Reason} -> [];
        _ ->
            Images = proplists:get_value(images, Data),
            case Images of 
                [] -> [];
                _ -> 
                    UrlContext = proplists:get_value(url_context, Data),
                    Images1 = fetch_webpage_utils:make_absolute_urls(UrlContext, lists:sublist(Images, ?MAX_IMAGES)),
                    Images2 = fetch_webpage_utils:substitute_spaces(Images1),
                    Images2
            end
    end.
    
