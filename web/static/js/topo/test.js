
$(function(){
  var vis, zoomTime, obZoom, keyword,
  innerScale = 1.0,
  radius = 600 / 2,
  chartWidth = $('#chart').width();
  
  function getTransform(selector){
    var transform = $(selector).attr("transform");
    console.log($(selector).attr("transform"));
    if (!transform){
      transform = "translate(0,0)scale(1)";
    }
    var re = /^translate\((\S+),(\S+)\)scale\((\S+)\)/;
    var arr = re.exec(transform);
    return arr;
  }

  function getExtraWidth() {
    return zoomTime * ($('#chart').width()/2 - radius);
  }

  function focus(){
    var $targetNode = $('#chart g.node:contains('+ keyword +')');
    $targetNode.each(function(){
      if($(this).text().trim() == keyword.trim()){
        var id = $(this).attr('id');
        if ($('#lastid').val()){
          d3.select($('#lastid').val()).style("font-weight", "normal");
        }
        d3.select('#'+id).style("font-weight", "bold");
        $('#lastid').val('#'+id);

        var scale = zoomTime + 2.0;
        var extra = getExtraWidth();
        var d = d3.select('#'+ id).data()[0];
        var px = d.x * Math.PI /180;
        var x = extra - Math.sin(px) * d.y * scale;
        var y = Math.cos(px) * d.y * scale;
        
        console.log(extra, Math.sin(px) * d.y, ", x:", x, ", y: ", y, ", s: ", scale);
        
        //vis.attr("transform", "translate("+ x +","+ y +")scale(" + scale + ")");
        var arr = getTransform('svg>g>g');
        var oldScale = arr.length > 3 ? parseFloat(arr[3]) : 1.0;
        var p0 = [parseFloat(arr[1]), parseFloat(arr[2]), oldScale*80],
        p1 = [x, y, scale*80];
        
        console.error("p0:", p0);
        console.error("p1:", p1);
        vis.call(transition, p0, p1);
        syncZoom([x, y], scale);
      }
    });
  }

  function transition(tar, start, end) {
    var center = [getExtraWidth(), 0.0],
    i = d3.interpolateZoom(start, end);
    
    //console.error("center:", center);
    tar.attr("transform", transform(start))
      .transition()
      .delay(250)
      .duration(i.duration)
      .attrTween("transform", function() {return function(t) {return transform(i(t));}});

    /*
    1. k = F(p[2])
    2. p0[2] = F(p0[2])
    3. p1[2] = F(p1[2])
    4. F(p[2]) >= 1

    1. x = G(p[0], k)
    2. p0[0] = G(p0[0], p0[2])
    2. p1[0] = G(p1[0], p0[2])
    */
    
    function transform(p) {
      var k = p[2]/80;
      //console.log(p);
      //console.log("translate(" + (p[0]) + "," + ([1]) + ")scale(" + k + ")");
      return "translate(" + p[0] + "," + p[1] + ")scale(" + k + ")";
    }
  }
  
  function checkNodes(){
    d3.selectAll('g.node').style("opacity", 0.2);
    var $targetNode = $('#chart g.node:contains('+ keyword +')');
    //console.log($targetNode);
    $targetNode.each(function(){
      if($(this).text().trim() == keyword.trim()){
        //console.log($(this));
        var id = $(this).attr('id');
        d3.select('#'+ id).style("opacity", 1);
        var d = d3.select('#'+ id).data()[0];
        console.log(d.x, d.y);
      }
    });
  }

  function reColorNodes(scale){
    if (scale >= 1.8){
      vis.selectAll("g.node").style("opacity", 1);
    } else {
      vis.selectAll("g.node").style("opacity", function(d){return d.level < 3 ? 1 : 0.3 });
    }
  }
  
  function zoom() {
    //console.log(d3.event.translate + ", [" + d3.event.scale + "],", zoomTime);
    var ids = ['#olt-0', '#onu-1', '#onu-2', '#onu-3', '#onu-4', '#onu-5'];
    for(var i in ids){
      var t = d3.select(ids[i]).data()[0];
      console.log(t.x, ' ',t.y);
    }
    console.log('---------------------------');
    
    reColorNodes(d3.event.scale);
    vis.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
  }

  function syncZoom(translate, scale){
    obZoom.translate(translate);
    obZoom.scale(scale);
    reColorNodes(scale);
  }

  function countNodes(cur){
    var count = 0;
    if(cur && cur.children){
      count = cur.children.length;
      if(cur.children){
        for(var i=0; i< cur.children.length; i++) {
          count += countNodes(cur.children[i]);
        }
      }
    }
    return count;
  }

  d3.json("/topo/test.json?na=6&nb=10&nc=6", function(json) {
    
    /**
     * 4级: 4.5/600 --> 3/400
     * zoomTime = nodeCount * 3 / 400;
     */
    
    zoomTime = countNodes(json) * 3 / 400;
    //zoomTime = 3.0;

    var tree = d3.layout.tree()
      .size([360, radius * zoomTime * 0.8])
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

    /*
      var dragGroup = d3.behavior.drag()
      .on('dragstart', function() {
      console.log('Start Dragging Group');
      })
      .on('drag', function(d, i) {
      d.x += d3.event.dx;
      d.y += d3.event.dy;
      d3.select(this).attr("transform", "translate(" + d.x + "," + d.y + ")scale("+ 1.0/zoomTime+")");
      });
    */

    obZoom = d3.behavior.zoom().scaleExtent([1, zoomTime+2]).on("zoom", zoom);
    vis = d3.select("#chart").append("svg")
      .attr("height", radius * 2)
      .append("g")
      .data([{x: radius, y: radius, scale: 1.0/zoomTime}])
      .attr("transform", "translate(" + radius + "," + radius + ")scale("+ 1.0/zoomTime+")")
      .call(obZoom)
      .append("g");

    vis.append("rect")
      .attr("width", radius * 2 * zoomTime) // 3 = 1 / 0.33
      .attr("height", radius * 2 * zoomTime)
      .attr("transform", "translate(" + -radius*zoomTime + "," + -radius*zoomTime + ")");

    console.log('set layout');

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

    
      node.append("svg:image")
      .attr("xlink:href", function(d){return "http://ww2.sinaimg.cn/large/412e82dbjw1dsbny7igx2j.jpg";})
      .attr("x", "-10px")
      .attr("y", "-10px")
      .attr("width", "20px")
      .attr("height", "20px");
    
    
    node.append("circle")
      .style("fill", function(d) { return d.status == 0 ? "red" : "green"})
      .attr("r", 4.5);

    node.append("text")
      .attr("dy", ".31em")
      .attr("text-anchor", function(d) { return d.x < 180 ? "start" : "end"; })
      .attr("transform", function(d) { return d.x < 180 ? "translate(8)" : "rotate(180)translate(-8)"; })
      .text(function(d) { return d.name; });

    console.log('dump nodes');

  });
  
  // Zoom button
  $('#zoom-in').click(function(){
    var arr = getTransform('svg>g>g')
    if (arr.length>3){
      f = parseFloat(arr[3]);
      scale = f < 6.0 ? f * 1.5 : f;
    }
    var x = arr[1];
    var y = arr[2];
    syncZoom([x,y], scale);
    vis.attr('transform', "translate("+ x +","+ y +")scale("+scale+")");
  });

  $('#zoom-out').click(function(){
    var arr = getTransform('svg>g>g')
    if (arr.length>3){
      f = parseFloat(arr[3]);
      scale = f > 1.0 ? f / 1.5 : 1.0;
    }
    var x = arr[1];
    var y = arr[2];
    syncZoom([x,y], scale);
    vis.attr('transform', "translate("+ x +","+ y +")scale("+scale+")");
  });

  $('#zoom-reset').click(function(){
    var scale = 1.0;
    var x = 0.0, y = 0.0;
    x += getExtraWidth();
    syncZoom([x,y], scale);
    vis.attr('transform', "translate("+ x +","+ y +")scale("+scale+")");
  });

  console.log('set zoom');
  
  $('#toolbar form').submit(function(){
    keyword = $('#keyword').val();
    focus();
    return false;
  });

  $('#keyword').typeahead({
    source: function(){
      var nodeNames = [],
      keyword = $('#keyword').val();
      var $targetNode = $('#chart g.node:contains('+ keyword +')');
      $targetNode.each(function(){
        nodeNames.push($(this).text());
      });
      return nodeNames;
    },
    updater: function(item){
      return item;
    }
  });

  
  // Right click menu
  $.contextMenu({
    selector: '.node', 
    callback: function(key, options) {
      var m = "clicked: " + $(this).find('text').text() + "\r\n\r\nAction: "
        + key + "\r\n\r\nTarget: " + $(this).find('a').attr('href');
      window.console && console.log(m) || alert(m); 
    },
    items: {
      "view": {name: "View", callback:function(key, options){
        window.location = $(this).find('a').attr('href');
      }},
      "edit": {name: "Edit", icon: "edit"},
      "cut": {name: "Cut", icon: "cut"},
      "copy": {name: "Copy", icon: "copy"},
      "paste": {name: "Paste", icon: "paste"},
      "delete": {name: "Delete", icon: "delete"},
      "sep1": "---------",
      "quit": {name: "Quit", icon: "quit"}
    }
  });
  console.log('click menu');
});
