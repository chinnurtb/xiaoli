
$(function(){
  var sid = '#chart';
  var path = 'olt-0';
  
  $('#layout select').change(function(){
    var selected = $(this).find(':selected').val();
    $('.chart').html('').hide();
    
    if (!selected || selected == 'circle'){
      sid = '#chart';
      $(sid).show();
      loadCircleTree(sid, path);
    } else if (selected == 'interactive'){
      sid = '#tichart';
      $(sid).show();
      loadInteractiveTree(sid, path);
    } else {
      console.error('Unexcepted layout');
    }
  });

  $('#layout select').change();
});
