
function loadFlowTree(sid){
  $(sid).html('');
  
  var width = $(sid).width()-18;
  height = 600 - 5;
  
  newHeight = countNodes(json) * 22;
  height = newHeight > 600 ? newHeight : height;

  var tree = d3.layout.tree()
    .size([height, width - 160]);

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
  var node = vis.selectAll("g.node")
    .data(nodes)
    .enter().append("g")
    .attr("class", "node")
    .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })

  renderNodes(sid, node, false);

  node.selectAll('a')
    .append("text")
    .attr("dx", function(d) { return d.children ? -8 : 8; })
    .attr("dy", 3)
    .attr("text-anchor", function(d) { return d.children ? "end" : "start"; })
    .text(function(d) { return d.name; });

  console.log('Load flow tree completed!');
  console.log('-----------------------------------------------')
}
