
$(function(){

  var charts = {
    circle : {
      sid : '#chart',
      updater : loadCircleTree,
    },
    flow : {
      sid : '#tfchart',
      updater : loadFlowTree,
    },
    interactive : {
      sid : '#tichart',
      updater : loadInteractiveTree,
    }
  };

  
  $('#layout select').change(function(){
    var selected = $(this).find(':selected').val();
    selected = !selected ? 'circle' : selected;
    $('.chart').html('').hide();

    chart = charts[selected];// Update global variable
    updateChart();
    if (!chart) {
      console.error("Unexcepted layout");
    }
    console.log('===================Layout Changed=====================');
  });

  path = '';
  loadDirectoryTree('#tree');
  
  path = 'root-0,olt-0';
  $('#layout select').change();
});
