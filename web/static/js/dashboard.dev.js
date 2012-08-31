var ajaxPopulateWidgets;

jQuery(document).ready( function($) {
	/* Dashboard Welcome Panel */
	var welcomePanel = $('#welcome-panel'),
		welcomePanelHide = $('#wp_welcome_panel-hide'),
	 	updateWelcomePanel = function( visible ) {
			$.post( ajaxurl, {
				action: 'update-welcome-panel',
				visible: visible,
				welcomepanelnonce: $('#welcomepanelnonce').val()
			});
		};

	if ( welcomePanel.hasClass('hidden') && welcomePanelHide.prop('checked') )
		welcomePanel.removeClass('hidden');

	$('.welcome-panel-close, .welcome-panel-dismiss a', welcomePanel).click( function(e) {
		e.preventDefault();
		welcomePanel.addClass('hidden');
		updateWelcomePanel( 0 );
		$('#wp_welcome_panel-hide').prop('checked', false);
	});

	welcomePanelHide.click( function() {
		welcomePanel.toggleClass('hidden', ! this.checked );
		updateWelcomePanel( this.checked ? 1 : 0 );
	});

	ajaxPopulateWidgets = function(el) {
		function show(i, id) {
			var p, e = $('#' + id + ' div.inside:visible').find('.widget-loading');
			if ( e.length ) {
				p = e.parent();
				setTimeout( function(){
					p.load( ajaxurl + '?action=dashboard-widgets&widget=' + id, '', function() {
						p.hide().slideDown('normal', function(){
							$(this).css('display', '');
						});
					});
				}, i * 500 );
			}
		}

	};
	ajaxPopulateWidgets();

	postboxes.add_postbox_toggles(pagenow, { pbshow: ajaxPopulateWidgets } );

} );
