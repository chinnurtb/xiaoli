
var circle_sid;
var vis;
var nodeCount;
var zoomObj;
var zoomTime = 1.0;
var radius = 600/2;

function loadCircleTree(sid) {
  circle_sid = sid;
  $(sid).html('');
  $('#keyword').val('');
  
  nodeCount = countNodes(json);
  zoomTime = nodeCount * 3 / 200;
  zoomTime = zoomTime < 1.0 ? 1.0 : zoomTime;

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

  zoomObj = d3.behavior.zoom().scaleExtent([1, zoomTime*1.2]).on("zoom", zoom);
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
    .enter().append("svg:g")
    .attr("id", function(d){ return d.id })
    .attr("class", "node")
    .style("opacity", function(d){ return d.level < json.maxlevel ? 1 : 0.4})
    .attr("transform", function(d) { return "rotate(" + (d.x - 90) + ")translate(" + d.y + ")"; });
  
  renderNodes(sid, node, false);
  
  node.selectAll('a')
    .append("text")
    .attr("dy", ".31em")
    .attr("text-anchor", function(d) { return d.x < 180 ? "start" : "end"; })
    .attr("transform", function(d) { return d.x < 180 ? "translate(8)" : "rotate(180)translate(-8)"; })
    .text(function(d) { return d.name; });

  initZoomButtons();
  initTypeahead();
  $('#zoom-reset').click();
  console.log('Load circle tree completed!');
  console.log('-----------------------------------------');
}


function zoom() {
  colorNodes(d3.event.scale);
  vis.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
}

function getExtraWidth() {
  return zoomTime * ($(circle_sid).width()/2 - radius);
}

function getTargetPoint(tn, k){
  var m = getCurrentMiddle(tn);
  return {x: tn.x - m.x * k,
          y: tn.y - m.y * k };
}

function getCurrentMiddle(tn){
  var extra = getExtraWidth();
  var maxR = zoomTime * radius * 0.8;
  var dx = (tn.x - extra) ;
  var dy = tn.y;
  var dz = Math.sqrt(dx*dx + dy*dy);
  console.log("maxR, dz:", maxR, dz);
  if (dz > maxR){
    dx = dx * maxR / dz;
    dy = dy * maxR / dz;
  }
  console.log("dx, dy:", dx, dy);
  return {x: dx,
          y: dy};
}

function colorNodes(scale){
  if (scale >= 1.8 || nodeCount < 100){
    vis.selectAll("g.node").style("opacity", 1);
  } else {
    vis.selectAll("g.node").style("opacity", function(d){return d.level < json.maxlevel ? 1 : 0.3 });
  }
}

function syncZoom(x, y, scale, action){
  if(action){
    vis.attr('transform', "translate("+ x +","+ y +")scale("+scale+")");
  }
  zoomObj.translate([x, y]);
  zoomObj.scale(scale);
  colorNodes(scale);
}

function focus(keyword){
  var $targetNode = $(circle_sid + ' g.node:contains('+ keyword +')');
  var hasDone = false;
  $targetNode.each(function(){
    if(!hasDone && $(this).text().trim() == keyword.trim()){
      var id = $(this).attr('id');
      d3.select('g.focus')
        .style("font-weight", "normal")
        .attr("class", "node");
      
      
      d3.select(circle_sid + ' #' + id)
        .style("font-weight", "bold")
        .attr("class", "node focus");

      var scale = zoomTime * 1.2;
      var extra = getExtraWidth();
      var d = d3.select(circle_sid + ' #'+ id).data()[0];
      var px = d.x * Math.PI /180;
      var x = extra - Math.sin(px) * d.y * scale;
      var y = Math.cos(px) * d.y * scale; 
      
      var t = getTransform(circle_sid + ' svg>g>g');
      var p0 = [t.x, t.y, t.scale ? t.scale : 1.0],
      p1 = [x, y, scale];
      
      vis.call(transition, p0, p1);
      syncZoom(x, y, scale, false);
      hasDone = true;
    }
  });
}

function initTypeahead() {
  $('#toolbar form').submit(function(){
    var keyword = $('#keyword').val();
    if(keyword){
      focus(keyword);
    }
    return false;
  });

  $('#keyword').typeahead({
    source: function(){
      var nodeNames = [];
      var keyword = $('#keyword').val();
      var $targetNode = $(circle_sid + ' g.node:contains('+ keyword +')');
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
  console.log('-----------------------------------------------')
}


function initZoomButtons(){
  
  $('#zoom-in').click(function(){
    var t = getTransform(circle_sid + ' svg>g>g')
    var scale = t.scale;
    scale = scale > zoomTime ? zoomTime*1.2 : scale * 1.2;
    syncZoom(t.x, t.y, scale, true);
  });

  $('#zoom-out').click(function(){
    var t = getTransform(circle_sid + ' svg>g>g')
    var scale = t.scale;
    scale = scale / 1.2 < 1.0 ? 1.0 : scale / 1.2;
    var tar = getTargetPoint(t, 1.2);
    syncZoom(tar.x, tar.y, scale, true);
  });

  $('#zoom-reset').click(function(){
    var scale = 1.0;
    var x = 0.0, y = 0.0;
    x += getExtraWidth();
    syncZoom(x, y, scale, true);
  });
  
  console.log('Init zoom buttons completed!');
  console.log('-----------------------------------------------')
}
