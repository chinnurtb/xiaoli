$(function() {
	var attrs = $(".desc .attrs");
	$(".desc .trigger").click(function() {
		attrs.toggleClass("hide");
		$(this).toggleClass("down");
		return false;
	});
});
