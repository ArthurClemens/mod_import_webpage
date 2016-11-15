mod_import_webpage = (function ($) {
    "use strict";
    
    var INPUT_SEL = "input[type='radio']",
        IMAGE_TMPL,
        CROP_IMAGE_TMPL,
        FEEDBACK_SELECTION_TMPL,
        FEEDBACK_EMPTY_TMPL,
        CHOOSE_FILE_TMPL,
        MIN_WIDTH = 200,
        MIN_HEIGHT = 200,
        
        // DOM elements
        $container,
        $row,
        $feedback,
        $fileInput,
        $cropping,
        $croppingRow,
        
        // vars
        inited = false,
        initedId,
        toProcessImages = {},
        invalidImages = {},
        validImages = {}, // key:domId; props: selected, rscUrl, sourceUrl, width, height
        mode = {},
        
        // functions
        init,
        updateUI,
        updateSelection,
        updateRowWidth,
        updateCropping,
        showCropOptions,
        updateCroppingWithUrlData,
        imageFeedback,
        imageFeedbackEmptyUrls,
        imageFeedbackEmptyFile,
        readFile,
        selectedImage,
        updateSelectedImage,
        sortImages,
        processImage,
        loadImages;

    IMAGE_TMPL = _.template([
'<figure <%= rscAttr %> data-size="<%= size %>">',
'   <input type="radio" name="image" value="<%= source_url %>" id="<%= id %>" <%= attr %> />',
'   <label for="<%= id %>" style="background-image:url(<%= view_url %>)">',
'       <span class="miw-icon-selected"></span>',
'   </label>',
'</figure>'
    ].join('')
    );
    
    CROP_IMAGE_TMPL = _.template([
'<figure class="miw-crop-<%= type %>">',
'   <input type="radio" name="image_crop" value="<%= is_crop %>" id="<%= id %>" <%= attr %> />',
'   <label for="<%= id %>" style="background-image:url(<%= view_url %>)" class="<%= label_class %>">',
'       <span class="miw-icon-selected"></span>',
'   </label>',
'</figure>'
    ].join('')
    );
       
    FEEDBACK_SELECTION_TMPL = _.template([
'<div>',
'   <a href="#" class="miw-link-remove"><%= remove_link_label %></a',
'</div>'
    ].join('')
    );
    
    FEEDBACK_EMPTY_TMPL = _.template([
'<div class="miw-image-empty">',
'   <p><%= message %></p>',
'</div>'
    ].join('')
    );
    
    CHOOSE_FILE_TMPL = _.template([
'<input type="file" name="file" />'
    ].join('')
    );

    init = function(id, opts) {
        if (inited && initedId == id) {
            return;
        }
        
        toProcessImages = {};
        validImages = {};
        invalidImages = {};
        mode = opts;
        
        $container = $("#" + id);
        $row = $container.find("[data-id='miw_image_row']");
        if (mode.alternatives === "file") {
            $row.hide();
        }
        $feedback = $container.find("[data-id='miw_image_feedback']");
        $fileInput = $container.find("[data-id='miw_file_input']");
        $cropping = $container.find("[data-id='miw_image_crop']");
        $croppingRow = $cropping.find("[data-id='miw_crop_image_row']");
        
        $row.on("change", INPUT_SEL, function(e) {
            updateSelectedImage($(this).attr("id"));
        });
        $container.on("click", ".miw-link-remove", function(e) {
            e.preventDefault();
            $row.find(INPUT_SEL + ":checked").prop('checked', false);
            updateSelectedImage();
            return false;
        });            
        inited = true;
        initedId = id;
    };

    updateUI = function() {
        updateSelection();
        updateCropping();
        imageFeedback(selectedImage());
    };
    
    updateSelection = function() {
        var selected = selectedImage();
        if (selected) {
            $row.hide();
            $fileInput.hide();
            $feedback.show();
        } else {
            if (mode.alternatives !== "file") { 
                updateRowWidth();
                $row.show();
                $feedback.show();
            } else {
                $fileInput.show();
                $feedback.hide();
            }
        }
    };
    
    updateRowWidth = function() {
        var $images = $row.find("figure"),
            imageCount = $images.length,
            $first,
            itemWidth,
            itemMargin;
        if (imageCount > 0) {
            $first = $images.first(),
            itemWidth = $first.width();
            itemMargin = parseInt($first.css("margin-right"), 10);
            $row.css("width", (imageCount * (itemWidth + itemMargin)));
        }
    };
    
    updateCropping = function() {
        var selected = selectedImage();
        if (selected) {
            showCropOptions(selected.sourceUrl, selected.width, selected.height);
        } else {
            $cropping.find(".miw-image label").css("background-image", "");
            $cropping.hide();
        }
    };
    
    showCropOptions = function(imageUrl, width, height) {
        var isCropped,
            tmplData,
            $figure;
        $croppingRow.html("");
        isCropped = parseInt($cropping.attr("data-cropped"), 10);
        
        // crop: yes
        tmplData = {
            is_crop: true,
            type:"yes",
            id: "crop-yes",
            attr: isCropped ? "checked=\"checked\"" : "",
            view_url: imageUrl,
            label_class: ""
        };
        $figure = $(CROP_IMAGE_TMPL(tmplData));
        $croppingRow.append($figure);
        
        // crop: no
        tmplData = {
            is_crop: false,
            type:"no",
            id: "crop-no",
            attr: (!isCropped) ? "checked=\"checked\"" : "",
            view_url: imageUrl,
            label_class: (height > width) ? "miv-image-vertical" : ""
        };
        $figure = $(CROP_IMAGE_TMPL(tmplData));
        $croppingRow.append($figure);

        $cropping.show();
    };

    imageFeedback = function(selected) {
        var tmplData = {};
        if (selected) {
            tmplData.size = selected.width + " x " + selected.height;
            tmplData.url = selected.sourceUrl;
            tmplData.remove_link_label = mod_import_webpage.lang.remove_image;
            $feedback.html($(FEEDBACK_SELECTION_TMPL(tmplData)));
        } else {
            if (mode.alternatives === "url") {
                imageFeedbackEmptyUrls();
            } else if (mode.alternatives === "file") {
                imageFeedbackEmptyFile();
            }
        }
    };
    
    imageFeedbackEmptyUrls = function() {
        var tmplData = {};
        tmplData.message = "";
        if (Object.keys(toProcessImages).length == 0) {
            // no images
            if (invalidImages.length == 0) {
                tmplData.message = mod_import_webpage.lang.no_images_found;
            } else {
                tmplData.message = mod_import_webpage.lang.no_useful_images_found;
            }
        } else {
            tmplData.message = mod_import_webpage.lang.no_image_selected;
        }
        $feedback.html($(FEEDBACK_EMPTY_TMPL(tmplData)));
    };
    
    imageFeedbackEmptyFile = function() {
        var tmplData = {};
        tmplData.label = mod_import_webpage.lang.choose_file;
        $fileInput.html($(CHOOSE_FILE_TMPL(tmplData)));
        $(":file", $fileInput).filestyle({
            icon: false,
            buttonText: mod_import_webpage.lang.choose_file,
            buttonName: "btn-secondary"
        });
        $(":file", $fileInput).change(function() {
            readFile(this);
        });
    };
    
    readFile = function(input) {
        if (input.files && input.files[0]) {
            var reader = new FileReader();
            reader.onload = function (e) {
                var src = e.target.result;
                var image = new Image();
                image.src = src;
                image.onload = function(e) {
                    processImage(image, undefined, src, false);
                };
            }
            reader.readAsDataURL(input.files[0]);
        }
    };
    
    selectedImage = function() {
        var foundKey = Object.keys(validImages).filter(function(domId) {
            return validImages[domId].selected;
        });
        if (foundKey.length) {
            return validImages[foundKey];
        }
    };
    
    updateSelectedImage = function(id) {
        Object.keys(validImages).forEach(function(domId) {
            validImages[domId].selected = false;
        });
        if (id) {
            validImages[id].selected = true;
        }
        updateUI();
    };
    
    /* 
    Sort images by size, largest first, read from the 'data-size' attribute.
    */
    sortImages = function() {
        var $images = $row.find("figure:not([data-rsc-url])");
        if ($images.length == 0) {
            return;
        }
		$images.sort(function(a, b) {
            var aw = parseInt(a.getAttribute("data-size"), 10),
		        bw = parseInt(b.getAttribute("data-size"), 10);
		    // reverse sort
		    if (aw < bw) {
                return 1;
            }
            if (aw > bw) {
                return -1;
            }
            if (aw == bw) {
                var at = $(a).find("input").val(),
                    bt = $(b).find("input").val();
                if (at > bt) {
                    return 1;
                }
                if (at < bt) {
                    return -1;
                }
                return 0;
            }
            return 0;
		});
		$images.detach().appendTo($row);
    };
    
    processImage = function(image, rscUrl, imageUrl, hasError) {
        var domId = "image_" + ($row.find("figure").length + 1),
            isResourceImage,
            match,
            image,
            tmplData,
            $figure,
            $smaller,
            error;
        isResourceImage = (rscUrl !== "");
        error = hasError || (image.width <= MIN_WIDTH && image.height <= MIN_HEIGHT);
        if (error && !isResourceImage) {
            invalidImages[domId] = imageUrl;
            delete(toProcessImages[imageUrl]);
        } else {
            tmplData = {
                size: image.width * image.height,
                id: domId,
                source_url: isResourceImage ? rscUrl : imageUrl,
                view_url: imageUrl,
                attr: isResourceImage ? "checked=\"checked\"" : "",
                rscAttr: isResourceImage ? "data-rsc-url=\"" + rscUrl + "\"" : ""
            };
            $figure = $(IMAGE_TMPL(tmplData));
            $row.append($figure);
            sortImages();
            validImages[domId] = {
                selected: isResourceImage ? true : false,
                rscUrl: rscUrl,
                sourceUrl: imageUrl,
                width: image.width,
                height: image.height
            };
            toProcessImages[imageUrl] = 1;
        }
        updateUI();    };
    
    /*
    imageData: array of image data in sub-arrays [id, url]
    */
    loadImages = function(imageData) {
        imageData.forEach(function(imageDataItem) {
            var image = new Image(),
                src = imageDataItem[1];
            toProcessImages[src] = 0;
            image.src = src;
            image.onload = function(e) {
                processImage(this, imageDataItem[0], imageDataItem[1]);
            };
            image.onerror = function(e) {
                processImage(this, imageDataItem[0], imageDataItem[1], true);
            }
        });
    };

    return {
        /*
        id: form id
        opts: Object with key 'alternatives', either "url" or "file"
        */
        init: function(id, opts) {
            if (id === undefined) {
                return;
            }
            init(id, opts);
        },
        showImages: function(id, imageData) {
            if (imageData !== undefined  && imageData !== "") {
                loadImages(imageData);
            }
            updateUI();
        }
    }

})(jQuery);

// translations to be filled in template
mod_import_webpage.lang = {};
mod_import_webpage.lang.no_images_found = "";
mod_import_webpage.lang.no_useful_images_found = "";
mod_import_webpage.lang.no_image_selected = "";
mod_import_webpage.lang.remove_image = "";

