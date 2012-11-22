
var vis;
var nodeCount;
var zoomObj;
var zoomTime = 1.0;
var radius = 600/2;

function loadCircleTree(sid, path) {
  d3.json("/topo/test.json?path="+path+"&na=6&nb=10&nc=6", function(json) {
    $(sid).html('');
    $('#lastid').val('');
    $('#keyword').val('');
    
    nodeCount = countNodes(json);
    zoomTime = nodeCount * 3 / 400;
    zoomTime = zoomTime < 1.0 ? 1.0 : zoomTime;
    //zoomTime = 3.0;

    var angle = (json.maxlevel - json.maxpath) == 1 ? 120 : 360;
    var tree = d3.layout.tree()
      .size([angle, radius * zoomTime * 0.8])
      .separation(function(a, b) { return (a.parent == b.parent ? 1 : 2) / a.depth; });
    
    var diagonal = d3.svg.diagonal.radial()
      .projection(function(d) { return [d.y, d.x / 180 * Math.PI]; });

    var width = radius * 2,
    height = radius * 2;

    var x = d3.scale.linear()
      .domain([-width / 2, width / 2])
      .range([0, width]);

    var y = d3.scale.linear()
      .domain([-height / 2, height / 2])
      .range([height, 0]);

    zoomObj = d3.behavior.zoom().scaleExtent([1, zoomTime+2]).on("zoom", zoom);
    vis = d3.select(sid).append("svg")
      .attr("height", radius * 2)
      .append("g")
      .data([{x: radius, y: radius, scale: 1.0/zoomTime}])
      .attr("transform", "translate(" + radius + "," + radius + ")scale("+ 1.0/zoomTime+")")
      .call(zoomObj)
      .append("g");

    vis.append("rect")
      .attr("width", radius * 2 * zoomTime) // 3 = 1 / 0.33
      .attr("height", radius * 2 * zoomTime)
      .attr("transform", "translate(" + -radius*zoomTime + "," + -radius*zoomTime + ")");

    // Dump nodes
    nodes = tree.nodes(json);
    
    var link = vis.selectAll("path.link")
      .data(tree.links(nodes))
      .enter().append("path")
      .attr("class", "link")
      .attr("d", diagonal);

    var node = vis.selectAll("g.node")
      .data(nodes)
      .enter().append("g")
      .attr("id", function(d){ return d.id })
      .attr("class", "node")
      .style("opacity", function(d){ return d.level < 3 ? 1 : 0.4})
      .attr("transform", function(d) { return "rotate(" + (d.x - 90) + ")translate(" + d.y + ")"; });
    
    renderNodes(sid, node);
    
    node.append("text")
      .attr("dy", ".31em")
      .attr("text-anchor", function(d) { return d.x < 180 ? "start" : "end"; })
      .attr("transform", function(d) { return d.x < 180 ? "translate(8)" : "rotate(180)translate(-8)"; })
      .text(function(d) { return d.name; });

    initZoomButtons(sid);
    initTypeahead(sid);
    $('#zoom-reset').click();
    console.log('Load circle tree completed!');
    console.log('-----------------------------------------');
  });
}


function zoom() {
  reColorNodes(d3.event.scale);
  vis.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
}

function getExtraWidth(sid) {
  return zoomTime * ($(sid).width()/2 - radius);
}

function reColorNodes(scale){
  if (scale >= 1.8 || nodeCount < 100){
    vis.selectAll("g.node").style("opacity", 1);
  } else {
    vis.selectAll("g.node").style("opacity", function(d){return d.level < 3 ? 1 : 0.3 });
  }
}

function syncZoom(x, y, scale, action){
  if(action){
    vis.attr('transform', "translate("+ x +","+ y +")scale("+scale+")");
  }
  zoomObj.translate([x, y]);
  zoomObj.scale(scale);
  reColorNodes(scale);
}

function focus(sid){
  var $targetNode = $(sid + ' g.node:contains('+ keyword +')');
  $targetNode.each(function(){
    if($(this).text().trim() == keyword.trim()){
      var id = $(this).attr('id');
      if ($('#lastid').val()){
        d3.select($('#lastid').val()).style("font-weight", "normal");
      }
      d3.select(sid + ' #'+id).style("font-weight", "bold");
      $('#lastid').val('#'+id);

      var scale = zoomTime + 2.0;
      var extra = getExtraWidth(sid);
      var d = d3.select(sid + ' #'+ id).data()[0];
      var px = d.x * Math.PI /180;
      var x = extra - Math.sin(px) * d.y * scale;
      var y = Math.cos(px) * d.y * scale;
      
      var arr = getTransform(sid + ' svg>g>g');
      var oldScale = arr.length > 3 ? parseFloat(arr[3]) : 1.0;
      var p0 = [parseFloat(arr[1]), parseFloat(arr[2]), oldScale],
      p1 = [x, y, scale];
      
      vis.call(transition, p0, p1);
      syncZoom(x, y, scale, false);
    }
  });
}

function renderNodes(sid, node) {
  // 1. xlink, 2. image, 3. circle, 4. menu
  node.append("a")
    .attr("xlink:href", function(d){return d.url;});

  node.append("svg:image")
    .attr("xlink:href", function(d){return "http://ww2.sinaimg.cn/large/412e82dbjw1dsbny7igx2j.jpg";})
    .attr("x", "-10px")
    .attr("y", "-7px")
    .attr("width", "20px")
    .attr("height", "20px");
  
  node.append("circle")
    .style("fill", function(d) { return d.status == 0 ? "red" : "green"})
    .attr("r", 4.5)
    .style("opacity", 0.5);
  
  addMenus(sid);
}

function initTypeahead(sid) {
  $('#toolbar form').submit(function(){
    keyword = $('#keyword').val();
    if(keyword){
      focus(sid);
    }
    return false;
  });

  $('#keyword').typeahead({
    source: function(){
      var nodeNames = [],
      keyword = $('#keyword').val();
      var $targetNode = $(sid + ' g.node:contains('+ keyword +')');
      $targetNode.each(function(){
        nodeNames.push($(this).text());
      });
      return nodeNames;
    },
    updater: function(item){
      return item;
    }
  });
  console.log('Init typeahead completed!');
}


function initZoomButtons(sid){
  
  $('#zoom-in').click(function(){
    var arr = getTransform(sid + ' svg>g>g')
    if (arr.length>3){
      f = parseFloat(arr[3]);
      scale = f < 6.0 ? f * 1.5 : f;
    }
    var x = arr[1];
    var y = arr[2];
    syncZoom(x, y, scale, true);
  });

  $('#zoom-out').click(function(){
    var arr = getTransform(sid + ' svg>g>g')
    if (arr.length>3){
      f = parseFloat(arr[3]);
      scale = f > 1.0 ? f / 1.5 : 1.0;
    }
    var x = arr[1];
    var y = arr[2];
    syncZoom(x, y, scale, true);
  });

  $('#zoom-reset').click(function(){
    var scale = 1.0;
    var x = 0.0, y = 0.0;
    x += getExtraWidth(sid);
    syncZoom(x, y, scale, true);
  });
  
  console.log('Init zoom buttons completed!');
}
