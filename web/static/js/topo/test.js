
$(function(){
  var sid = '#chart';
  var path = 'olt-0';
  
  // loadCircleTree(sid, path);
  // initZoomButtons(sid);
  // initTypeahead(sid);

  sid = '#tichart';
  loadInteractiveTree(sid, path);
  
/*
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
        d3.select('#chart ' + ' #'+id).style("font-weight", "bold");
        $('#lastid').val('#'+id);

        var scale = zoomTime + 2.0;
        var extra = getExtraWidth();
        var d = d3.select('#chart '+ ' #'+ id).data()[0];
        var px = d.x * Math.PI /180;
        var x = extra - Math.sin(px) * d.y * scale;
        var y = Math.cos(px) * d.y * scale;
        
        //vis.attr("transform", "translate("+ x +","+ y +")scale(" + scale + ")");
        var arr = getTransform('#chart svg>g>g');
        var oldScale = arr.length > 3 ? parseFloat(arr[3]) : 1.0;
        var p0 = [parseFloat(arr[1]), parseFloat(arr[2]), oldScale],
            p1 = [x, y, scale];
        
        vis.call(transition, p0, p1);
        syncZoom(x, y, scale, false);
      }
    });
  }

  function transition(tar, start, end) {
    tar.attr("transform", transform(start))
      .transition()
      .delay(250)
      .duration(1000)
      .attrTween("transform", function() {return function(t) {return transform(t);}});
    
    function transform(t) {
      t = typeof(t) == "object" ? 0 : t;
      //console.log("t:", t);
      var x = t * (end[0]-start[0]) + start[0],
      y = t * (end[1]-start[1]) + start[1]
      k = t * (end[2]-start[2]) + start[2];
      //console.log("transform", "translate("+ x +","+ y +")scale(" + k + ")");
      return "transform", "translate("+ x +","+ y +")scale(" + k + ")";
    }
  }

  function reColorNodes(scale){
    if (scale >= 1.8 || nodeCount < 100){
      vis.selectAll("g.node").style("opacity", 1);
    } else {
      vis.selectAll("g.node").style("opacity", function(d){return d.level < 3 ? 1 : 0.3 });
    }
  }
  
  function zoom() {
    //console.log(d3.event.translate + ", [" + d3.event.scale + "],", zoomTime);
    
    reColorNodes(d3.event.scale);
    vis.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
  }

  function syncZoom(x, y, scale, action){
    if(action){
      vis.attr('transform', "translate("+ x +","+ y +")scale("+scale+")");
    }
    obZoom.translate([x, y]);
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

  // Sidebar
  function makeSidebar(){
    var w = 172,
    h = 600,
    i = 0,
    barHeight = 24,
    barWidth = w * .5,
    duration = 400,
    root;

    var tree = d3.layout.tree()
      .size([h, 100]);
      
    var diagonal = function(d) {
        return "M"+ d.source.y+' ' + d.source.x +
                ' L'+ (d.source.y - 5)+' '+ d.source.x +
                ' L'+ (d.source.y -5)+' '+ d.target.x+
                ' L'+ d.target.y+' '+ d.target.x
    };

    var tvis = d3.select("#tree").append("svg:svg")
      .attr("width", w)
      .attr("height", h)
      .append("svg:g")
      .attr("transform", "translate(0, 12)");

    d3.json("/topo/test.json?na=6&nb=10&nc=6", function(json) {
      json.x0 = 0;
      json.y0 = 0;
      root = json;
      function collapse(d) {
        if (d.children) {
          d._children = d.children;
          d._children.forEach(collapse);
          d.children = null;
        }
      }
      
      root.children.forEach(collapse);
      update(root);
    });
    
    function update(source) {
      d3.select('#tree svg').attr("height", h);

      // Compute the flattened node list. TODO use d3.layout.hierarchy.
      var nodes = tree.nodes(root);
      
      // Compute the "layout".
      nodes.forEach(function(n, i) {
        n.x = i * barHeight ;
      });
      
      // Update the nodes…
      var node = tvis.selectAll("g.node")
        .data(nodes, function(d) { return d.id || (d.id = ++i); });
      
      var nodeEnter = node.enter().append("svg:g")
        .attr("class", "node")
        .attr("id", function(d){ return d.id })
        .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
        .style("opacity", 1e-6);

      // Enter any new nodes at the parent's previous position.
      nodeEnter.append("svg:rect")
        .attr("y", -barHeight / 2)
        .attr("height", barHeight -4)
        .attr("width", barWidth-20)
        .style("fill", color)
        .on("click", click);
      
      nodeEnter.append("svg:text")
        .attr("dy", 3.5-2)
        .attr("dx", 5.5)
        .text(function(d) { return d.name; });
      
      // Transition nodes to their new position.
      nodeEnter.transition()
        .duration(duration)
        .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })
        .style("opacity", 1);
      
      node.transition()
        .duration(duration)
        .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })
        .style("opacity", 1)
        .select("rect")
        .style("fill", color);
      
      // Transition exiting nodes to the parent's new position.
      node.exit().transition()
        .duration(duration)
        .attr("transform", function(d) { return "translate(" + source.y + "," + source.x + ")"; })
        .style("opacity", 1e-6)
        .remove();
      
      // Update the links…
      var link = tvis.selectAll("path.link")
        .data(tree.links(nodes), function(d) { return d.target.id; });
      
      // Enter any new links at the parent's previous position.
      link.enter().insert("svg:path", "g")
        .attr("class", "link")
        .attr("d", function(d) {
          var o = {x: source.x0, y: source.y0};
          return diagonal({source: o, target: o});
        })
        .transition()
        .duration(duration)
        .attr("d", diagonal);
      
      // Transition links to their new position.
      link.transition()
        .duration(duration)
        .attr("d", diagonal);
      
      // Transition exiting nodes to the parent's new position.
      link.exit().transition()
        .duration(duration)
        .attr("d", function(d) {
          var o = {x: source.x, y: source.y};
          return diagonal({source: o, target: o});
        })
        .remove();
      
      // Stash the old positions for transition.
      nodes.forEach(function(d) {
        d.x0 = d.x;
        d.y0 = d.y;
      });
    }

    // Toggle children on click.
    function click(d) {
      var isRefresh;
      
      if (d.children) {
        d._children = d.children;
        d.children = null;
        isRefresh = false;
      } else {
        d.children = d._children;
        d._children = null;
        isRefresh = true;
      }
      isRefresh = d.level == 0 ? tree : isRefresh;

      // Compute new tree height
      var children = root.children ? root.children : root._children;
      var level = 1 + children.length;
      function countLevel(d){
        if (d.children) {
          level += d.children.length;
          d.children.forEach(countLevel);
        }
      }
      children.forEach(countLevel);
      h = level * barHeight + 20;
      
      update(d);

      // Make request path
      if (isRefresh && (d.children || d._children)){
        var cur = d;
        var ids = [cur.id];
        while(cur.parent){
          cur = cur.parent;
          ids.push(cur.id);
        }
        console.log(ids.join(','));
        updateChart(ids.join(','));
      }
    }

    function color(d) {
      return d._children ? "#3182bd" : d.children ? "#c6dbef" : "#fd8d3c";
    }
  }

  // Init Sidebar
  makeSidebar();

  function updateChart(path) {
    d3.json("/topo/test.json?path="+path+"&na=6&nb=10&nc=6", function(json) {
      
      $('#chart').html('');
      $('#lastid').val('');
      $('#keyword').val('');
      
      //console.log(json);
      nodeCount = countNodes(json);
      zoomTime = nodeCount * 3 / 400;
      zoomTime = zoomTime < 1.0 ? 1.0 : zoomTime;
      //zoomTime = 3.0;

      //console.log(json.maxpath, json.maxlevel);
      var angle = (json.maxlevel - json.maxpath) == 1 ? 120 : 360;
      angle = 360;
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

      node.append("a")
        .attr("xlink:href", function(d){return d.url;});

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

      $('#zoom-reset').click();

      $.contextMenu({
        selector: '#chart .node', 
        callback: function(key, options) {
          var m = "clicked: " + $(this).find('text').text() + "\r\n\r\nAction: "
            + key + "\r\n\r\nTarget: " + $(this).find('a').attr('href');
          window.console && console.log(m) || alert(m); 
        },
        items: {
          "edit": {name: "Edit", icon: "edit"},
          "cut": {name: "Cut", icon: "cut"},
          "copy": {name: "Copy", icon: "copy"},
          "paste": {name: "Paste", icon: "paste"},
          "delete": {name: "Delete", icon: "delete"},
          "sep1": "---------",
          "quit": {name: "Quit", icon: "quit"}
        }
      });
      
      console.log('dump nodes');
      console.log('------------------------------------');
    });
  }
  updateChart('olt-0');
  
  // Zoom button
  $('#zoom-in').click(function(){
    var arr = getTransform('#chart svg>g>g')
    if (arr.length>3){
      f = parseFloat(arr[3]);
      scale = f < 6.0 ? f * 1.5 : f;
    }
    var x = arr[1];
    var y = arr[2];
    syncZoom(x, y, scale, true);
  });

  $('#zoom-out').click(function(){
    var arr = getTransform('#chart svg>g>g')
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
    x += getExtraWidth();
    syncZoom(x, y, scale, true);
  });

  console.log('set zoom');
  
  $('#toolbar form').submit(function(){
    keyword = $('#keyword').val();
    if(keyword){
      focus();
    }
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
  console.log('click menu');
  */
});
