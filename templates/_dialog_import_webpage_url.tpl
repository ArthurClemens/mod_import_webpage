{% lib
    "css/mod_import_webpage.css"
%}

{# autofocus in IE9 #}
{% javascript %}
$(function() {
    $('[autofocus]:not(:focus)').eq(0).focus();
});
{% endjavascript %}

{% wire
    id=#form_url
    type="submit"
    delegate=`mod_import_webpage`
    postback={
        process_url
        category=category
        ok_template="_dialog_import_webpage.tpl"
        ok_target=#form_url
        error_template="_dialog_import_webpage_error.tpl"
        error_target=#form_url_error
        collection_id=collection_id
        page_context=page_context
        update_module=update_module
        update_fun=update_fun
    }
%}
<form id="{{ #form_url }}" method="post" action="postback" class="form-horizontal">
    <div class="form-group row">
        <label class="control-label col-md-3">{_ IMPORT_FROM_URL_DIALOG_ENTER_URL_LABEL _}</label>
        <div class="col-md-9">
            <div class="input-group">
                <input type="text" id="url" name="url" class="form-control" autofocus="autofocus" placeholder="{_ IMPORT_FROM_URL_DIALOG_ENTER_URL_PLACEHOLDER _}" value="" />
                {% validate id="url" type={presence} only_on_submit %}
                <span class="input-group-btn">
                    <button type="submit" class="btn btn-primary" id="url_check">{_ IMPORT_FROM_URL_DIALOG_ACTION_NEXT _}</button>
                </span>
            </div>
            <div id="{{ #form_url_error }}"></div>
        </div>
    </div>
</form>
