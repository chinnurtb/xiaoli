
$(function(){
  // document.body.parentNode.style.overflow="hidden";
  // page_height = $(document.body).height() - $(".navbar").height() -98;
  // $(".tree-chart").css({"height":page_height+"px"});
  // $("#tree").css({"height":page_height+18+"px"});

  var charts = {
    circle : {
      sid : '#chart',
      sclass : '.circle',
      updater : loadCircleTree,
    },
    flow : {
      sid : '#tfchart',
      updater : loadFlowTree,
    },
    flowDrag : {
      sid : '#tfdchart',
      sclass : '.flowDrag',
      updater : loadFlowDragTree,
      history : [],
    },
    interactive : {
      sid : '#tichart',
      updater : loadInteractiveTree,
    },
    force : {
      sid : '#fcchart',
      updater : loadForce,
    }
  };


  // Register Event 
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

  /*
  $('#export').click(function(){
    exportChart(chart.sid);
  });
  */

  // Action
  path = '';
  loadDirectoryTree('#tree');
  
  path = 'root-0,olt-1,onu-1-2';
  $('#layout select').change();
});
