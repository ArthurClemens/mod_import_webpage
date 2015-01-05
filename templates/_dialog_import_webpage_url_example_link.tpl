<a id="{{ #select_url }}" href="#">
    <span>
        {_ IMPORT_FROM_URL_ACTION _}
    </span>
</a>
{% wire
    id=#select_url
    action={
        dialog_open
        class="miw-dialog"
        backdrop="static"
        template="_dialog_import_webpage_url.tpl" 
        title=_"IMPORT_FROM_URL_DIALOG_TITLE"
        collection_id=""
        page_context=""
    }
%}
