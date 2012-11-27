
function loadForce(sid) {
  $(sid).html('');
  
  var width = $(sid).width() - 18,
  height = 595,
  node,
  link,
  root, vis, force;

    function compute_level(root) {
        var level = 0;
        while(true) {
            if(root.children.length != 1) break;
            level += 1;
            root = root.children[0]
        }
        return level
    }

  d3.json("/topo/nodes.json?path="+path, function(tjson) {
    root = tjson;
    root.fixed = true;
    var nodeCount = countNodes(root);
    var k = 3;
    if (nodeCount > 150) {
      height = k*nodeCount > height ? k*nodeCount : height;
      width = height > width ? height : width;
    }

    var level = compute_level(root)
    if(level == 0) {
        root.x = height / 2;
    }else {
        root.x = 50;
    }
    root.y = height / 2;

    force = d3.layout.force()
      .on("tick", tick)
      .charge(-140 - 300*level)
      .linkDistance(function(d) { return d.target._children ? 200 : 60; })
      .size([width, height]);

    vis = d3.select(sid).append("svg")
      .attr("width", width)
      .attr("height", height);
    
    update();
  });

  function update() {
    var nodes = flatten(root),
    links = d3.layout.tree().links(nodes);

    // Restart the force layout.
    force
      .nodes(nodes)
      .links(links)
      .start();

    // Update the links…
    link = vis.selectAll("line.link")
      .data(links, function(d) { return d.target.id; });

    // Enter any new links.
    link.enter().insert("line", ".node")
      .attr("class", function(d) {return d.target.lstatus == 0 ? "broken link" : "link"})
      .attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

    // Exit any old links.
    link.exit().remove();

    // Update the nodes…
    node = vis.selectAll("g.node")
      .data(nodes, function(d) { return d.id; })

    // node.transition()
    //   .attr("r", function(d) { return d.children ? 4.5 : Math.sqrt(d.size) / 2; });

    // Enter any new nodes.
    node.enter().append("svg:g")
      .attr("id", function(d){ return d.id })
      .attr("class", "node")
      .call(force.drag);
      //.on("click", click)

    node.append("circle")
      .attr("class", "node")
      .attr("r", function(d) { return d.children ? 10 : Math.sqrt(d.size) / 1.3; })
      .style("fill", color);
    
    node.append("text")
      .attr("dy", ".31em")
      .attr("text-anchor", function(d) { return d.x < 180 ? "start" : "end"; })
      .attr("transform", "translate(5,0)")
      .text(function(d) { return d.name; });

    // Exit any old nodes.
    node.exit().remove();
  }

  function tick() {
    link.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

    node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
  }

  // Color leaf nodes orange, and packages white or blue.
  function color(d) {
    return d._children ? "#3182bd" : d.children ? "#c6dbef" : "#fd8d3c";
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
    update();
  }

  // Returns a list of all nodes under the root.
  function flatten(root) {
    var nodes = [], i = 0;

    function recurse(node) {
      if (node.children) node.size = node.children.reduce(function(p, v) { return p + recurse(v); }, 0);
      if (!node.id) node.id = ++i;
      nodes.push(node);
      return node.size;
    }

    root.size = recurse(root);
    return nodes;
  }
  console.log("Update force completed!");
  console.log("========================================");
}
