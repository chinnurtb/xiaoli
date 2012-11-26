function loadDirectoryTree(sid) {
  var w = 202,
  h = 600,
  i = 0,
  barHeight = 24,
  barWidth = w * .58,
  duration = 400,
  depth = 30,
  current_node = "root",
  root;

  var tree = d3.layout.tree();

  var diagonal = function(d) {
    return "M"+ d.source.y+' ' + d.source.x +
      ' L'+ (d.source.y -2)+' '+ d.source.x +
      ' L'+ (d.source.y -2)+' '+ d.target.x+
      ' L'+ d.target.y+' '+ d.target.x
  };

  var vis = d3.select(sid).append("svg:svg")
    .attr("width", w)
    .attr("height", h)
    .append("svg:g")
    .attr("transform", "translate(6, 12)");

  d3.json("/topo/directory.json?path="+path, function (tjson) {
    tjson.x0 = 0;
    tjson.y0 = 0;
    root = tjson;
    function collapse(d) {
      if (d.children) {
        d._children = d.children;
        d._children.forEach(collapse);
        d.children = null;
      }
    }
    if (root.children){
      root.children.forEach(collapse);
    }
    update(root);
    // console.log("root:", root);
  });

  function update(source) {
    // Compute new tree height
    h = (2 + countNodes(root)) * barHeight;
    
    d3.select(sid + ' svg').attr("height", h);

    // Compute the flattened node list. TODO use d3.layout.hierarchy.
    var nodes = tree.nodes(root);

    // Compute the "layout".
    nodes.forEach(function (n, i) {
        n.x = i * barHeight;
        n.y = n.depth * depth;
    });

    // Update the nodes…
    var node = vis.selectAll("g.node")
      .data(nodes, function (d) {
        return d.id || (d.id = ++i);
      });

    var nodeEnter = node.enter().append("svg:g")
      .attr("class", "node")
      .attr("id", function (d) {
        return d.id
      })
      .attr("transform", function (d) {
        return "translate(" + source.y0 + "," + source.x0 + ")";
      })
      .style("opacity", 1e-6);

    // Enter any new nodes at the parent's previous position.
      nodeEnter.append("svg:text")
              .attr("dy", 4.5)
              .attr("dx", 12.5)
              .text(function (d) {
                  return d.name;
              });
      nodeEnter.append("svg:image")
              .attr("xlink:href", collapse_image)
              .attr("x", -11-depth)
              .attr("y", "-9px")
              .attr("width", "16px")
              .attr("height", "16px")
              .on("click",click);
      nodeEnter.append("svg:image")
              .attr("xlink:href", "/static/js/topo/images/olt.png")
              .attr("x", "-12px")
              .attr("y", "-10px")
              .attr("width", "20px")
              .attr("height", "20px")
              .on("click",click);
      nodeEnter.append("svg:rect")
              .attr("y", -barHeight / 2 +2)
              .attr("x", 10)
              .attr("height", barHeight-4)
              .attr("width", function(d) {
                  return d3.select("#"+ d.id+" :first-child").node().clientWidth + 6
              })
              .on("click", click);

    // Transition nodes to their new position.
    nodeEnter.transition()
      .duration(duration)
      .attr("transform", function (d) {
        return "translate(" + d.y + "," + d.x + ")";
      })
      .style("opacity", 1);

    node.transition()
      .duration(duration)
      .attr("transform", function (d) {
        return "translate(" + d.y + "," + d.x + ")";
      })
      .style("opacity", 1)
      .select("rect")
        .style("fill-opacity", function(d) {
            return d.id==current_node ? .3 : 0;
        })
      .style("fill", "grey");

    // Transition exiting nodes to the parent's new position.
    node.exit().transition()
      .duration(duration)
      .attr("transform", function (d) {
        return "translate(" + source.y + "," + source.x + ")";
      })
      .style("opacity", 1e-6)
      .remove();

    // Update the links…
    var link = vis.selectAll("path.link")
      .data(tree.links(nodes), function (d) {
        return d.target.id;
      });

    // Enter any new links at the parent's previous position.
    link.enter().insert("svg:path", "g")
      .attr("class", "link")
      .attr("d", function (d) {
        var o = {x:source.x0, y:source.y0};
        return diagonal({source:o, target:o});
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
      .attr("d", function (d) {
        var o = {x:source.x, y:source.y};
        return diagonal({source:o, target:o});
      })
      .remove();

    // Stash the old positions for transition.
    nodes.forEach(function (d) {
      d.x0 = d.x;
      d.y0 = d.y;
    });

    console.log('Update directory tree completed!');
  }

  // Toggle children on click.
  function click(d) {
    current_node = d.id;
    if (d.children) {
      d._children = d.children;
      d.children = null;
    } else if (d._children) {
      d.children = d._children;
      d._children = null;
    } else {
      return;
    }
    d3.select("#"+ d.id+" image").attr("xlink:href", collapse_image);

    // Make request path
    var cur = d;
    var ids = [cur.id];
    while (cur.parent) {
      cur = cur.parent;
      ids.push(cur.id);
    }
    path = ids.reverse().join(','); // Update global variable
    
    update(d);

    // Open
    if (d.level > 0 && d.children) {
      if (d.children.length > 0) {
        updateChart();
      }else {
        d3.json("/topo/directory.json?path="+path, function (tjson) {
          if (!tjson) {
            console.error("Request Failed!");
          } else if (tjson.length > 0){
            d._children = tjson;
            d.children = null;
            click(d);
          }
        });      
      }
    } 
  }

  function collapse_image(d) {
      return d._children ? "/static/js/topo/images/plus.gif" : d.children ? "/static/js/topo/images/minus.gif" : null;
  }

  console.log('Load directory tree completed!');
  console.log('-----------------------------------------------')
}
