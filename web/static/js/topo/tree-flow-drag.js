
function loadFlowDragTree(sid){
  $(sid).html('');
  
  var width = $(sid).width()-18;
  height = 600 - 5;
  
  newHeight = countNodes(json) * 22;
  height = newHeight > 600 ? newHeight : height;

  var tree = d3.layout.tree()
    .size([height, width - 160]);

  // Drag
  var drag = d3.behavior.drag()
    .origin(function(d) { return d; })
    .on("dragstart", dragstart)
    .on("drag", dragmove)
    .on("dragend", dragend);
  
  function dragstart() {
    
  }

  function dragmove(d, i) {
    d.x += d3.event.dx;
    d.y += d3.event.dy;
    d3.select(this).attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
    console.log("dragmove > d -- e: ", d.x, d.y, "--");
  }

  function dragend() {
    d3.select(this);
  }

  var diagonal = d3.svg.diagonal()
    .projection(function(d) { return [d.y, d.x]; });

  var vis = d3.select(sid).append("svg")
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", "translate(60, 0)");
  
  var nodes = tree.nodes(json);

  var link = vis.selectAll("path.link")
    .data(tree.links(nodes))
    .enter().append("path")
    .attr("class", "link")
    .attr("d", diagonal);

  // render nodes
  console.log(nodes);
  var node = vis.selectAll("g.node")
    .data(nodes)
    .enter().append("g")
    .attr("class", "node")
    .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })
    .call(drag);

  // renderNodes(sid, node, false);
  d3.selectAll(sid + " path.link").attr("class", function(d) {return d.target.lstatus == 0 ? "broken link" : "link"})
  
  node.append("circle")
    .attr("r", 10)
    .attr("class", statusClass);
  
  /*
  var circle = vis.selectAll("circle")
    .data(nodes)
  .enter().append("circle")
    .attr("r", 10)
    .attr("cx", function(d) { return d.x; })
    .attr("cy", function(d) { return d.y; })
    .style("fill", "blue")
    .call(drag);
    */
  
  node.append("svg:image")
    .attr("xlink:href", nodeImage)
    .attr("x", "-10px")
    .attr("y", "-10px")
    .attr("width", "20px")
    .attr("height", "20px");
  
  node.append("a")
    .attr("xlink:href", function(d){return d.url;});
  
  //addMenus(sid);

  node.selectAll('a')
    .append("text")
    .attr("dx", function(d) { return d.children ? -8 : 8; })
    .attr("dy", 3)
    .attr("text-anchor", function(d) { return d.children ? "end" : "start"; })
    .text(function(d) { return d.name; });

  console.log('Load flow tree completed!');
  console.log('-----------------------------------------------')
}
