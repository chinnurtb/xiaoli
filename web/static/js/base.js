$(function() {
  var attrs = $(".desc .attrs");
  $(".desc .trigger").click(function() {
    attrs.toggleClass("hide");
    $(this).toggleClass("down");
    return false;
  });
});


$(function(){
  $('#multi-delete-btn').click(function() {
    if ($('table tr th [type=checkbox]:checked').length > 0) {
      if(confirm('删除所有?')){
        $('#table-form').submit();
      }
    } else {
      alert('请选择要删除的项!')
      return false;
    }
  });
})



