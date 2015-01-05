{% extends "_dialog_import_base.tpl" %}

{#
params:
page_id
category,
collection_id
url
page_title
summary
images
page_context
#}

{% lib
    "css/mod_import_webpage.css"
%}
{% lib
    "js/mod_import_webpage.js"
%}
{% javascript %}
mod_import_webpage.lang.no_image_selected = "{_ IMPORT_FROM_URL_SELECT_IMAGE_MESSAGE _}";
mod_import_webpage.lang.no_images_found = "{_ IMPORT_FROM_URL_NO_IMAGES_MESSAGE _}";
mod_import_webpage.lang.no_useful_images_found = "{_ IMPORT_FROM_URL_NO_USEFUL_IMAGES_MESSAGE _}";
mod_import_webpage.lang.small_image = "{_ IMPORT_FROM_URL_SMALL_IMAGE_WARNING _}";
{% endjavascript %}

{% with 
        url|default:page_id.url,
        page_title|default:page_id.title,
        summary|default:page_id.summary,
        page_id|if:{update_from_url page_id=page_id collection_id=collection_id page_context=page_context update_module=update_module update_fun=update_fun}:{create_from_url category=category collection_id=collection_id page_context=page_context update_module=update_module update_fun=update_fun},
        action_label|default:(page_id|if:
            _"IMPORT_FROM_URL_ACTION_UPDATE":
            _"IMPORT_FROM_URL_ACTION_SAVE"
        ),
        image|default:page_id.depiction.id
    as
        url,
        page_title,
        summary,
        postback,
        action_label,
        image
%}
{% with 
    images|default:(image|if:[[page_id.image_url, "/image/" ++ image.depiction.filename]]:[])
    as
    images
%}
{% wire
    id=#form_props
    type="submit"
    delegate=`mod_import_webpage`
    postback=postback
%}
<form id="{{ #form_props }}" method="post" action="postback" enctype="multipart/form-data" class="form-horizontal">
    <div class="form-group row">
        <label class="control-label col-md-3">{_ IMPORT_FROM_URL_URL_HEADER _}</label>
        <div class="col-md-9">
            <input type="text" readonly=readonly name="url" class="form-control" placeholder="{_ DIALOG_SELECT_URL_ENTER_URL_PLACEHOLDER _}" value="{{ url }}" />
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3" for="page_title">{_ IMPORT_FROM_URL_TITLE_HEADER _}</label>
        <div class="col-md-9">
            <input type="text" class="col-lg-4 col-md-4 form-control" id="page_title" name="page_title" value="{{ page_title|unescape }}" />
            {% validate id="page_title" type={presence} %}
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3" for="summary">{_ IMPORT_FROM_URL_DESCRIPTION_HEADER _}</label>
        <div class="col-md-9">
            <textarea class="col-lg-4 col-md-4 form-control" id="summary" name="summary" rows="2">{{ summary|unescape|replace:["<br />", "\n"] }}</textarea>
        </div>
    </div>
    
    {% with 125 as image_size %} {# small enough for mobile #}
        <div class="form-group row">
            <label class="control-label col-md-3">{_ IMPORT_FROM_URL_IMAGE_HEADER _}</label>
            <div class="col-md-9">
                <div data-id="miw_image_feedback"></div>
                <div class="miw-image">
                    <div data-id="miw_image_row" class="miw-image-row"></div>
                </div>
            </div>
        </div>
        <style>
        .miw-dialog .miw-image {
            max-height: {{ image_size + 20 }}px; {# add room for IE scrollbar #}
        }
        .miw-dialog .miw-image .miw-image-row figure,
        .miw-dialog .miw-image .miw-image-row label {
            width: {{ image_size }}px;
            height: {{ image_size }}px;
        }
        .miw-dialog .miw-image .miw-image-row img {
            width: {{ image_size }}px;
            max-height: {{ image_size }}px;
        }
        </style>
    {% endwith %}
    
    {% wire
        type="load"
        action={
            script
            script="
mod_import_webpage.showImages('" ++ #form_props ++ "', " ++ images|to_json ++ ");"
        }
    %}
    {% if page_id %}
        {% wire
            type="load"
            postback={
                load_images_from_url
                form_id=#form_props
                url=page_id.url
                page_id=page_id
            }
            delegate=`mod_import_webpage`
        %}
    {% endif %}
    <div class="modal-footer">
        <button class="btn btn-primary" type="submit">{{ action_label }}</button>
    </div>
</form>
{% endwith %}
{% endwith %}
