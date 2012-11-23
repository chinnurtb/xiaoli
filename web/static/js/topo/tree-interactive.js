
function loadInteractiveTree(sid){
  
  var margin = {top: 20, right: 80, bottom: 20, left: 80},
  width = $(sid).width() - margin.right - margin.left,
  height = 600 - margin.top - margin.bottom - 5,
  i = 0,
  duration = 250,
  root;

  var vis;
  var tree = d3.layout.tree();
  var diagonal = d3.svg.diagonal()
    .projection(function(d) { return [d.y, d.x]; });

  root = json;
  root.x0 = height / 2;
  root.y0 = 0;

  var opened = false;
  function collapse(d) {
    if (d.children) {
      if (d.children.length == 1 || !opened) {
        if (d.children.length > 1) {
          opened = true;
        }
        d.children.forEach(collapse);
      } else {
        d._children = d.children;
        d.children = null;
        d._children.forEach(collapse);
      }
    }
  }
  collapse(root);
  update(root);
  
  console.log('Load interactive tree completed!');
  console.log('-----------------------------------------------')


  
  function innerUpdate(source) {
    // Compute the new tree layout.
    var nodes = tree.nodes(root).reverse();

    // Normalize for fixed-depth.
    nodes.forEach(function(d) { d.y = d.depth * 180; });

    // Update the nodes…
    var node = vis.selectAll("g.node")
      .data(nodes, function(d) { return d.id || (d.id = ++i); });

    // Enter any new nodes at the parent's previous position.
    var nodeEnter = node.enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
      .on("click", click);

    renderNodes(sid, nodeEnter, true);

    nodeEnter.selectAll('a')
      .append("text")
      .attr("x", function(d) { return d.children || d._children ? -10 : 10; })
      .attr("dy", ".35em")
      .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
      .text(function(d) { return d.name; })
      .style("fill-opacity", 1e-6);

    // Transition nodes to their new position.
    var nodeUpdate = node.transition()
      .duration(duration)
      .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; });

    nodeUpdate.select("circle.collapse")
      .style("fill", function(d) { return d._children ? "lightsteelblue" : "#FFF"; });

    nodeUpdate.select("text")
      .style("fill-opacity", 1);

    // Transition exiting nodes to the parent's new position.
    var nodeExit = node.exit().transition()
      .duration(duration)
      .attr("transform", function(d) { return "translate(" + source.y + "," + source.x + ")"; })
      .remove();

    nodeExit.select("circle")
      .attr("r", 1e-6);

    nodeExit.select("text")
      .style("fill-opacity", 1e-6);

    // Update the links…
    var link = vis.selectAll("path.link")
      .data(tree.links(nodes), function(d) { return d.target.id; });

    // Enter any new links at the parent's previous position.
    link.enter().insert("path", "g")
      .attr("class", function(d) {return d.target.lstatus == 0 ? "broken link" : "link"})
      .attr("d", function(d) {
        var o = {x: source.x0, y: source.y0};
        return diagonal({source: o, target: o});
      });

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
  
  function update(source) {
    $(sid).html('');
    
    tree.size([height, width]);

    vis = d3.select(sid).append("svg")
      .attr("width", width + margin.right + margin.left - 18)
      .attr("height", height + margin.top + margin.bottom)
      .append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
    
    innerUpdate(source);
  }
  
  
  // Toggle children on click.
  function click(d) {
    if (d.children) {
      d._children = d.children;
      d.children = null;
    } else {
      d.children = d._children;
      d._children = null;
    }

    if (d.children || d._children){
      var nodeCount = countNodes(root);
      console.log("nodeCount:", nodeCount);
      
      if (d._children) {
        innerUpdate(d);
      } else {
        height = nodeCount > 32 ? nodeCount * 20 : 600 - margin.top - margin.bottom;
        update(d);
      }
    }
  }
}
