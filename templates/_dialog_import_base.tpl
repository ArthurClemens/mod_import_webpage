{#
params:
page_id
category,
collection_id
page_title
summary
page_context
#}

{% lib
    "css/mod_import_webpage.css"
%}
{% lib
    "js/mod_import_webpage.js"
%}

{% block script %}
    {% javascript %}
    mod_import_webpage.lang.no_image_selected = "{_ IMPORT_FROM_URL_SELECT_IMAGE_MESSAGE _}";
    mod_import_webpage.lang.no_images_found = "{_ IMPORT_FROM_URL_NO_IMAGES_MESSAGE _}";
    mod_import_webpage.lang.no_useful_images_found = "{_ IMPORT_FROM_URL_NO_USEFUL_IMAGES_MESSAGE _}";
    mod_import_webpage.lang.choose_file = "{_ ADD_NOTE_CHOOSE_FILE_ACTION _}";
    mod_import_webpage.lang.remove_image = "{_ IMPORT_FROM_URL_ACTION_REMOVE_IMAGE _}";
    {% endjavascript %}
{% endblock %}

{% block script_init %}
{#
    Example:
    {% javascript %}
    mod_import_webpage.init("{{ #form_props }}", {alternatives: "url"});
    {% endjavascript %}
#}
{% endblock %}

{% with 
        page_title|default:page_id.title,
        summary|default:page_id.summary,
        image|default:page_id.depiction.id
    as
        page_title,
        summary,
        image
%}

{% block wire %}{% endblock %}

<form id="{{ #form_props }}" method="post" action="postback" enctype="multipart/form-data" class="form-horizontal">

    {% block url %}{% endblock %}

    <div class="form-group row">
        <label class="control-label col-md-3">{_ IMPORT_FROM_URL_TITLE_HEADER _}</label>
        <div class="col-md-9">
            <input type="text" class="col-lg-4 col-md-4 form-control do_autofocus" id="page_title" name="page_title" value="{{ page_title|unescape }}"  />
            {% validate id="page_title" type={presence} %}
        </div>
    </div>

    <div class="form-group row">
        <label class="control-label col-md-3">{_ IMPORT_FROM_URL_DESCRIPTION_HEADER _}</label>
        <div class="col-md-9">
            <textarea class="col-lg-4 col-md-4 form-control" id="summary" name="summary" rows="2">{{ summary|unescape|replace:["<br />", "\n"] }}</textarea>
        </div>
    </div>
    
    {% block image %}
        <div class="form-group row">
            <label class="control-label col-md-3">{_ IMPORT_FROM_URL_IMAGE_HEADER _}</label>
            <div class="col-md-9">
                {% block display_images %}{% endblock %}
            </div>
        </div>
        
        {% with
            page_id.image_crop|if_undefined:`true`
            as
            image_crop
        %} 
            <div class="form-group row" data-id="miw_image_crop" data-cropped="{{ image_crop|yesno:"1,0" }}" style="display: none;">
                <label class="control-label col-md-3">{_ IMPORT_FROM_URL_IMAGE_CROP_LABEL _}</label>
                <div class="col-md-9">
                    <div class="miw-image miw-crop-image">
                        <div data-id="miw_crop_image_row" class="miw-image-row">
                        </div>
                    </div>
                </div>
            </div>
        {% endwith %}
        
    {% endblock %}
    
    <div class="modal-footer">
        <button class="btn btn-primary" type="submit">{% block action_label %}{% endblock %}</button>
    </div>
</form>
{% endwith %}