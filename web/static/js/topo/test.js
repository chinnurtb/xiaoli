
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
    selected = !selected ? 'interactive' : selected;
    $('.chart').html('').hide();

    var chart = charts[selected];
    if(chart) {
      chartId = chart.sid;
      $(chartId).show();
      updateChart = chart.updater;
      updateChart(chartId, chartPath);
    } else {
      console.error('Unexcepted layout!');
    }
    console.log('===================Layout Changed=====================');
  });

  $('#layout select').change();
  loadDirectoryTree('#tree');
});
