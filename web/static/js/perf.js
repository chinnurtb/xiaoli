function updateSelectField(data, fieldId){
  var data = eval("("+data+")");
  opts = [];
  for(var i in data) {
    opts.push('<option value="'+data[i][0]+'">'+data[i][1]+'</option>');
  }
  $(fieldId).html(opts.join(''));
}

$(function(){
  $('#sampletime').change(function(){
    var key = $(this).find(':selected').val();
    $.get('/perf/refresh/intervals', {'key': key},
          function(data){
            updateSelectField(data, '#intervals');
          });
  });
  
  $('#vendors').change(function(){
    var vendors = [];
    $(this).find(':selected').each(function(){
      vendors.push($(this).val());
    });
    $.get('/perf/refresh/models', {'vendors': vendors},
          function(data){
            updateSelectField(data, '#models');
          });
  });
});        



