
// Orderable
$(function() {
  var attrs = $(".desc .attrs");
  $(".desc .trigger").click(function() {
    attrs.toggleClass("hide");
    $(this).toggleClass("down");
    return false;
  });
});

// Modal
$(function(){
  $('#multi-delete-btn').click(function(e){
    e.preventDefault();
    ids = [];
    $('table input:checkbox[name=id]:checked').each(function(){
      ids.push($(this).attr('value'));
    });
    $.get($(this).attr('href'), {'id':ids}, function(data){
      console.log(data);
      $('#myModal').html(data).modal('show');
    });
  });
  
  $('.modal-btn').click(function(e){
    e.preventDefault();
    $.get($(this).attr('href'), function(data){
      console.log(data);
      $('#myModal').html(data).modal('show');
    }).error(function(err) {
      console.log(err);
      alert("错误:" + err.status);
    });
  });
});


$(function() { 
    $("[rel=tooltip]").tooltip();
    $("[rel=popover]").popover();
});


