/*
// jQuery dropPanel
//
// Version 1.0.3 beta
//
// Jolly
// http://www.cnblogs.com/wliang22
// 09 September 2009
//
// Visit http://www.cnblogs.com/wliang22 for more information
//
// Usage: $('#control_id').dropPanel( options, callback )
//
// Options:  contentId          - An indicator for replacing the content in the drop panel
//
// Dependencies:  jQuery 1.2.6 or higher (http://jquery.com/)
//
// Change Log:
//
//		1.0.1	- Updated to work with jQuery 1.2.6+ (no longer requires the dimensions plugin)
//				- Changed $(this).offset() to $(this).position(), per James' and Jono's suggestions
//
//		1.0.2	- Fixed issue where dropdown doesn't scroll up/down with keyboard shortcuts
//				- Changed '$' in setTimeout to use 'jQuery' to support jQuery.noConflict
//				- Renamed from jqueryMultiSelect.* to jquery.multiSelect.* per the standard recommended at
//				  http://docs.jquery.com/Plugins/Authoring (does not affect API methods)
//
//		1.0.3	- Now uses the bgiframe plugin (if it exists) to fix the IE6 layering bug.
//              - Forces IE6 to use a min-height of 200px (this needs to be added to the options)
//
// Licensing & Terms of Use
// 
//	
*/
if (jQuery) (function($) {

    $.extend($.fn, {
        dropPanel: function(o, callback) {
            // Default options
            if (!o) var o = {};
            if (o.contentId == undefined) o.contentId = "dropContent";
            if (o.replaceText == undefined) o.replaceText = "Loading...";
			if (o.onShow == undefined) o.onShow = function(){};

            // Initialize each multiSelect
            $(this).each(function() {
                var select = $(this);
                var init_value = select.val() == null ? "" : select.val();
                var iWidth = select.width() < 40 ? 40 : select.width();
                var html = '<div class="dropPanel"><input type="text" style="cursor:pointer;background: none;border: none;box-shadow: none;" class="dropPanel_txt" id="'+ select.attr("id") +'" name="' + select.attr("name") + '" readonly="readonly" value="'+ init_value +'" /></div>';
                html += '<div class="panelContent" style="position: absolute; z-index: 99999; display: none;">';
								html += "<div id='" + o.contentId + "'>"+ o.replaceText +"</div>"
                html += '</div>';
                $(select).after(html);
                // Events
                $(select).next('.dropPanel').mouseover(function() {
                    $(this).addClass('hover');
                }).mouseout(function() {
                    $(this).removeClass('hover');
                }).click(function() {
                    // Show/hide on click
                    if ($(this).hasClass('active')) {
                        $(this).panelContentHide();
                    } else {
                        $(this).panelContentShow(o.onShow);
                    }
                    return false;
                });
                
                $(select).next('.dropPanel').children('INPUT.dropPanel_txt').width(iWidth).focus(function() {
                    // So it can be styled with CSS
                    $(this).parent().addClass('focus');
                }).blur(function() {
                    // So it can be styled with CSS
                    $(this).parent().removeClass('focus');
                });

                // Keyboard
                $(select).next('.dropPanel').keydown(function(e) {
                    // Is dropdown visible?
                    if ($(this).next('.panelContent').is(':visible')) {
                        // Dropdown is visible
                        // Tab
                        if (e.keyCode == 9) {
                            $(this).addClass('focus').trigger('click'); // esc, left, right - hide
                            $(this).focus().next(':input').focus();
                            return true;
                        }

                        // ESC, Left, Right
                        if (e.keyCode == 27 || e.keyCode == 37 || e.keyCode == 39) {
                            // Hide dropdown
                            $(this).addClass('focus').trigger('click');
                        }
                        return false;
                    } else {
                        // Dropdown is not visible
                        if (e.keyCode == 38 || e.keyCode == 40 || e.keyCode == 13 || e.keyCode == 32) { // down, enter, space - show
                            // Show dropdown
                            $(this).removeClass('focus').trigger('click');
                            $(this).next('.panelContent').find('LABEL:first').addClass('hover');
                            return false;
                        }
                        //  Tab key
                        if (e.keyCode == 9) {
                            // Shift focus to next INPUT element on page
                            $(this).focus().next(':input').focus();
                            return true;
                        }
                    }
                    // Prevent enter key from submitting form
                    if (e.keyCode == 13) return false;

                });
                // Apply bgiframe if available on IE6
                if ($.fn.bgiframe) $(select).next('.dropPanel').next('.panelContent').bgiframe();

                // Eliminate the original form element
                $(select).remove();
            });

        },

        // Hide the dropdown
        panelContentHide: function() {
            $(this).removeClass('active').removeClass('focus').next('.panelContent').hide();
            $("object").show();
        },

        // Show the dropdown
        panelContentShow: function(callback) {
            // Hide any open option boxes
            $('.dropPanel').panelContentHide();            
            var panelContent = $(this).next('.panelContent');
            panelContent.find('LABEL').removeClass('hover');
            $(this).addClass('active').next('.panelContent').show();
            $("object").hide();
            // Position it
            var offset = $(this).position();
            $(this).next('.panelContent').css({ top: offset.top + $(this).outerHeight() + 'px' });
            $(this).next('.panelContent').css({ left: offset.left + 'px' });
            $(this).adjustViewport();
            // Disappear on hover out
            multiSelectCurrent = $(this);
            var timer = '';
            $(this).next('.panelContent').hover(function() {
                clearTimeout(timer);
            }, function() {
                timer = setTimeout('jQuery(multiSelectCurrent).panelContentHide(); $(multiSelectCurrent).unbind("hover");', 250);
            });
            callback($(this));
        },
        // Ensures that the selected item is always in the visible portion of the dropdown (for keyboard controls)
        adjustViewport: function(dropPanel) {
            if(!dropPanel)
                dropPanel = $(this)
            // set width
            var panelContent = $(this).next('.panelContent');
            if(panelContent.width() < $('.dropPanel').width())
            	$(this).next('.panelContent').css({ width: $(this).width() + 23 });
            if(panelContent.height() < $('.dropPanel').height())
                $(this).next('.panelContent').css({ height : "200px" });
        }
    });

})(jQuery);