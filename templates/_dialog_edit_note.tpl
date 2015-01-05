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
    mod_import_webpage.init("{{ #form_props }}", {alternatives: "file"});
    {% endjavascript %}
{% endblock %}

{% block wire %}
{% with 
        page_id|if:{update_note page_id=page_id collection_id=collection_id page_context=page_context update_module=update_module update_fun=update_fun}:{create_note category=category collection_id=collection_id page_context=page_context update_module=update_module update_fun=update_fun}
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



{% block display_images %}
    {% lib "js/bootstrap-filestyle.min.js" %}
    {% include "_import_webpage_images.tpl" %}
    {% with 
        image|if:[[image, "/image/" ++ image.depiction.filename]]:[]
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
    {% endwith %}
{% endblock %}


{% block action_label %}{% with 
    action_label|default:(page_id|if:
        _"EDIT_NOTE_ACTION_UPDATE":
        _"EDIT_NOTE_ACTION_SAVE"
    )
    as
    action_label
%}{{ action_label }}{% endwith %}{% endblock %}
