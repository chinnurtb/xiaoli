// Global variables
var path = null;
var chart = null;
var json = null;

// Totally common
function updateChart(){
  $(chart.sid).show();
  $(chart.sid).html('');
  $('.toolbar-item').hide();
  $('.all').show();
  if (chart.sclass) {
    $(chart.sclass).show();
  }

  function update(){
    chart.updater(chart.sid);
    injectStyle();
  }
  
  d3.json("/topo/nodes.json?path="+path, function(tjson) {
    json = tjson;
    if (!(typeof chart.history === 'undefined')) {
      d3.json("/topo/load-drag-history.json?path="+path, function(tjson){
        chart.history = tjson;
        update();
      });
    } else {
      update();
    }
  });
}

var styles = {
  '.chart svg': {
    'background-color': '#FFF',
  },
  '.chart .node': {
    'font' : '10px sans-serif'
  },
  '.chart rect' :{
    'fill': '#FFF',
  },
  '.chart .node circle' : {
    'stroke' : '#D5D5D5',
    'stroke-width': '1px',
  },
  '.chart .node text':{
    'fill': '#333',
  },
  '.chart .link': {
    'fill': 'none',
    'stroke': '#7D7',
    'stroke-width': '1.5px',
  },
  '.chart .broken': {
    'stroke': '#D77',
  },
  // Alarms Status
  '.badge-alarm-clear': {
    'fill': '#00FF00'
  },
  '.badge-alarm-indeterminate': {
    'fill': '#773EF7', 
  },
  '.badge-alarm-warning': {
    'fill': '#43d5fa', 
  },
  '.badge-alarm-minor':{
    'fill': '#E6F940',
  },
  '.badge-alarm-major':{
    'fill': '#F6983E',
  },
  '.badge-alarm-critical':{
    'fill': '#ED4D5A',
  }
};

function injectStyle(){
  for( selector in styles ){
    var style = styles[selector];
    for(name in style) {
      $(selector).css(name, style[name]);
    }
  }
  console.log("Styles injected!");
}


function svgExport(sid){
  var svg = $(sid).html();
  $.ajax({
    type: 'POST',
    url:'/svg-export',
    data: {'svg': svg, 'filename': 'tree', 'type': 'svg'},
    success: function(data){
      console.log('success!');
    },
    
  });
}

function nodeImage(d){
  var images = {
    1 : "/static/js/topo/images/olt.png",
    2 : "/static/js/topo/images/onu.png",
    3 : "/static/js/topo/images/eoc.png",
    4 : "/static/js/topo/images/cpe.png",
  };
  
  return images[d.level];
}

function statusClass(d){
  var statuses = {
    0 : 'badge-alarm-clear',
    1 : 'badge-alarm-indeterminate',
    2 : 'badge-alarm-warning',
    3 : 'badge-alarm-minor',
    4 : 'badge-alarm-major',
    5 : 'badge-alarm-critical',
  };
  return statuses[d.status];
}

function getTransform(selector){
  var transform = $(selector).attr("transform");
  if (!transform){
    transform = "translate(0,0)scale(1)";
  }
  var re = /^translate\((\S+),(\S+)\)scale\((\S+)\)/;
  var arr = re.exec(transform);
  var t = {};
  t.x = parseFloat(arr[1]);
  t.y = parseFloat(arr[2]);
  t.scale = arr.length > 3 ? parseFloat(arr[3]) : 1.0;
  
  return t;
}


function transition(tar, start, end) {
  tar.attr("transform", transform(start))
    .transition()
    .delay(200)
    .duration(800)
    .attrTween("transform", function() {return function(t) {return transform(t);}});
  
  function transform(t) {
    t = typeof(t) == "object" ? 0 : t;
    var x = t * (end[0]-start[0]) + start[0],
    y = t * (end[1]-start[1]) + start[1]
    k = t * (end[2]-start[2]) + start[2];
    return "transform", "translate("+ x +","+ y +")scale(" + k + ")";
  }
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

function renderNodes(sid, node, collapse) {
  // 1. xlink, 2. image, 3. circle, 4. menu
  d3.selectAll(sid + " path.link").attr("class", function(d) {return d.target.lstatus == 0 ? "broken link" : "link"})
  
  node.append("circle")
    .attr("r", 10)
    .attr("class", statusClass);
  
  node.append("svg:image")
    .attr("xlink:href", nodeImage)
    .attr("x", "-10px")
    .attr("y", "-10px")
    .attr("width", "20px")
    .attr("height", "20px");
  
  if (collapse){
    node.append("circle")
      .style("opacity", function(d) { return d.children || d._children ? 1 : 0})
      .attr("r", 4.5)
      .attr("cx", "14px")
      .attr("class", "collapse")
      .style("fill", function(d) { return d._children ? "lightsteelblue" : "#FFF"; });
  }
  
  node.append("a")
    .attr("xlink:href", function(d){return d.url;});
  
  addMenus(sid);
}

function addMenus(sid){
  $.contextMenu({
    selector: sid + ' .node', 
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
}
