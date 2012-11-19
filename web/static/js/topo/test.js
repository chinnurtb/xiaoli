var nodes;
$(function(){
  var vis, obZoom;
  
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

  function focus(){
    
  }

  function reColorNodes(scale){
    if (scale >= 2.0){
      vis.selectAll("g.node").style("opacity", 1);
    } else {
      vis.selectAll("g.node").style("opacity", function(d){ return d.level < 3 ? 1 : 0.3});
    }
  }
  
  function zoom() {
    console.log(d3.event.translate, ":", d3.event.scale);
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
    
    var radius = 600 / 2,
    zoomTime = 1.0,
    innerScale = 1.0;
    
    /**
     * 4级: 4.5/600 --> 3/400
     * zoomTime = nodeCount * 3 / 400;
     */
    zoomTime = countNodes(json) * 3 / 400;

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

    var dragGroup = d3.behavior.drag()
      .on('dragstart', function() {
        console.log('Start Dragging Group');
      })
      .on('drag', function(d, i) {
        d.x += d3.event.dx;
        d.y += d3.event.dy;
        d3.select(this).attr("transform", "translate(" + d.x + "," + d.y + ")scale("+ 1.0/zoomTime+")");
      });

    obZoom = d3.behavior.zoom().scaleExtent([1, 8]).on("zoom", zoom);
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
      .attr("class", "node")
      .style("opacity", function(d){ return d.level < 3 ? 1 : 0.4})
      .attr("transform", function(d) { return "rotate(" + (d.x - 90) + ")translate(" + d.y + ")"; });
    
    /*
      node.append("svg:image")
      .attr("xlink:href", function(d){return "http://ww2.sinaimg.cn/large/412e82dbjw1dsbny7igx2j.jpg";})
      .attr("x", "-32px")
      .attr("y", "-32px")
      .attr("width", "64px")
      .attr("height", "64px");
    */
    
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
      innerScale = f < 6.0 ? f * 1.5 : f;
    }
    var x = arr[1];
    var y = arr[2];
    syncZoom([x,y], innerScale);
    $('svg>g>g').attr('transform', "translate("+ x +","+ y +")scale("+innerScale+")");
  });

  $('#zoom-out').click(function(){
    var arr = getTransform('svg>g>g')
    if (arr.length>3){
      f = parseFloat(arr[3]);
      innerScale = f > 1.0 ? f / 1.5 : 1.0;
    }
    var x = arr[1];
    var y = arr[2];
    syncZoom([x,y], innerScale);
    $('svg>g>g').attr('transform', "translate("+ x +","+ y +")scale("+innerScale+")");
  });

  $('#zoom-reset').click(function(){
    innerScale = 1.0;
    var x = 0.0, y = 0.0;
    syncZoom([x,y], innerScale);
    $('svg>g>g').attr('transform', "translate("+ x +","+ y +")scale("+innerScale+")");
  });

  console.log('set zoom');

  /*
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
  */
  console.log('click menu');
});
