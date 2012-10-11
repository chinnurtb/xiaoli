
$(document).ready(function(){
  function getCookie(name) {
    var r = document.cookie.match("\\b" + name + "=([^;]*)\\b");
    return r ? r[1] : undefined;
  }

  $(".hide-column-tog").each(function(){
    var $tar = $(this);
    $tar.change(function(e){
      e.preventDefault();
      var hidden_columns = [];
      $('#table-settings .hide-column-tog').filter('input:checkbox:not(:checked)').each(function(){
        hidden_columns.push($(this).val());
      });
      str_hidden_columns = hidden_columns.join(',')
      $('#hidden-columns').val(str_hidden_columns);
      $.ajax({
        type: 'POST',
        url: '/settings/profile',
        data: {'_xsrf' : getCookie('_xsrf'),
               'hidden_columns': str_hidden_columns,
               'grp': userSettings.grp,
               'key': userSettings.key,
              },
      });
      return false;
    });
  });
});
