{% extends "_dialog_import_base.tpl" %}

{#
params:
page_id
category,
collection_id
page_title
summary
page_context
#}

{% block script_init %}
    {% javascript %}
    mod_import_webpage.init("{{ #form_props }}", {alternatives: "url"});
    {% endjavascript %}
{% endblock %}

{% block wire %}
{% with 
        page_id|if:{update_from_url page_id=page_id collection_id=collection_id page_context=page_context update_module=update_module update_fun=update_fun}:{create_from_url category=category collection_id=collection_id page_context=page_context update_module=update_module update_fun=update_fun}
    as
        postback
%}
{% wire
    id=#form_props
    type="submit"
    delegate=`mod_import_webpage`
    postback=postback
%}
{% endwith %}
{% endblock %}


{% block url %}
    {% with 
        url|default:page_id.url
        as
        url
    %}
    <div class="form-group row">
        <label class="control-label col-md-3">{_ IMPORT_FROM_URL_URL_HEADER _}</label>
        <div class="col-md-9">
            <input type="text" readonly=readonly name="url" class="form-control" value="{{ url }}" />
        </div>
    </div>
    {% endwith %}
{% endblock %}


{% block display_images %}
    {% include "_import_webpage_images.tpl" %}
    {% with 
        images|default:(image|if:[[page_id.image_url, "/image/" ++ image.depiction.filename]]:[])
        as
        images
    %}
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
    {% endwith %}
{% endblock %}


{% block action_label %}{% with 
    action_label|default:(page_id|if:
        _"IMPORT_FROM_URL_ACTION_UPDATE":
        _"IMPORT_FROM_URL_ACTION_SAVE"
    )
    as
    action_label
%}{{ action_label }}{% endwith %}{% endblock %}
