#coding: utf-8

import pydot

from flask import Blueprint, request, url_for, \
    render_template, json

from tango.ui import navbar
from nodes.models import Node, Area, AREA_PROVINCE

from .forms import SearchForm

topoview = Blueprint('topo', __name__)

@topoview.context_processor
def inject_navid():
    return dict(navid = 'topo')

@topoview.route('/topo/test')
def test():
    import time
    time.sleep(1)
    return render_template('topo/test.html')

@topoview.route('/topo/test2')
def test2():
    return render_template('topo/test2.html')

@topoview.route('/topo/olts.json', methods=['GET', 'POST'])
def olts_json():
    return 'OK'
    #return json.dumps({'children':[data,data]})

@topoview.route('/topo/directory.json')
def json_load_directory():
    spath = request.args.get('path', '')
    data = None
    lvs_all = ['ROOT', 'OLT', 'ONU', 'EOC', 'CPE']
    
    if spath:
        data = []
        path = spath.split(',')
        if len(path) >= len(lvs_all):
            return json.dumps([])
        lvs = [i.split('-')[0].upper() for i in path]
        lv = lvs_all[len(lvs)]
        for i in range(1, 8):
            node = {
                'name'     : '%s-%s' % (lv ,str(i)),
                'children' : None,
                'level'    : len(lvs)-1,
                'id'       : '%s-%s' % (lv.lower() ,str(i)),
            }
            node['_children'] = [] if  (len(path) < len(lvs_all)-1) else None
            data.append(node)
    else:
        data = {
            'name'     : 'ROOT-0',
            'children' : [],
            'level'    : 0,
            'id'       : 'root-0',
        }
        for i in range(1, 100):
            node = {
                'name'     : 'OLT-' + str(i),
                'children' : [],
                'level'    : 1,
                'id'       : 'olt-' + str(i)
            }
            data['children'].append(node)
    return json.dumps(data)
    
@topoview.route('/topo/nodes.json')
def json_load_nodes():
    # 1. 缩放           DONE
    # 2. 拖拽           DONE
    # 3. 链接           DONE
    # 4. 显示图片       DONE
    # 5. 表达节点的状态 DONE
    # 6. 右键菜单       DONE
    # 7. 搜索跳转       DONE
    from random import Random
    rand = Random()
    
    spath = request.args.get('path', 'olt-0')
    na    = request.args.get('na', 7, type=int)
    nb    = request.args.get('nb', 7, type=int)
    nnc   = request.args.get('nc', 7, type=int)

    ca = cb = cc = 0;
    onu_lv, eoc_lv, cpe_lv = 2, 3, 4
    path = spath.split(',')[1:]
    lvs = [i.split('-')[0].upper() for i in path]
    
    data = {
        'name'     : path[0].upper(),
        'children' : [],
        'level'    : 1,
        'maxlevel' : 1,
        'maxpath'  : len(path) - 1,
        'id'       : path[0]
    }

    def selected(s, i):
        if s in lvs and '%s-%d' % (s.lower(), i) not in path:
            return False
        return True
    print '========================================'
    print spath, path, lvs
    print '--------------------'
    
    for a in range(na):         # ONU
        ca = a+1
        A = {'name': 'ONU-' + str(ca), 'children': [], 'level': onu_lv, 'id': 'onu-'+str(ca)}
        for b in range(nb):     # EOC
            cb = b+1
            B = {'name': 'EOC-' + str(cb), 'children': [], 'level': eoc_lv, 'id': 'eoc-'+str(cb)}
            nc = nnc
            for c in range(nc): # CPE
                cc = c+1
                C = {'name': 'CPE-' + str(cc), 'url': 'http://www.stackoverflow.com', 'level': cpe_lv, 'id': 'cpe-'+str(cc)}
                C['status'] = 1 if c % rand.randint(2, 6) != 0 else 0
                C['lstatus'] =  1 if c % 5 > 1 else 0
                if selected('CPE', cc):
                    if data['maxlevel'] < cpe_lv:
                        data['maxlevel'] = cpe_lv;
                    B['children'].append(C)
            if selected('EOC', cb):
                if data['maxlevel'] < eoc_lv:
                    data['maxlevel'] = eoc_lv;
                A['children'].append(B)
        if selected('ONU', ca):
            if data['maxlevel'] < onu_lv:
                data['maxlevel'] = onu_lv;
            data['children'].append(A)

    def pdict(d):
        print d['id'],
        if 'children' in d:
            print len(d['children'])
            for c in d['children']:
                pdict(c)
    pdict(data)
    return json.dumps(data)


# ==============================================================================
#  The Past: Graphviz
# ==============================================================================
area_style = {
    'shape' : 'polygon',
    # 'sides' : 4,
    'fixedsize' : 'true',
    'style' : 'filled',
    'color' : 'lightblue',
    # 'peripheries': 2,
    'fontsize' : '12.0'
}

node_style = {
    'shape' : "doublecircle",
    'fixedsize' : 'true',
    'width' : '0.40',
    'height' : '0.40',
    'style' : 'filled',
    'fontsize' : '10.0'
}

area_edge_style = {
    'color' : 'skyblue',
    'labelfontcolor' : '#009933',
}

node_edge_style = {
    'labelfontcolor' : '#009933',
    'color' : 'skyblue',
}

images = {
    'area' : 'org_32x32.gif',
    
    'olt': 'olt-network_32x32.png',
    'eoc': 'olt-network_32x32.png',
    'onu': 'olt-network_32x32.png',
    'dslam' : 'olt-network_32x32.png',
    'switch': 'switch_32x32.gif',
}

node_categories = ['olt', 'onu', 'dslam', 'eoc', 'switch']
table_template = '<<TABLE CELLPADDING="0" CELLSPACING="0" BORDER="0" ALIGN="center"><TR><TD><IMG SRC="%(src)s"/></TD></TR> <TR><TD>%(name)s</TD></TR> <TR><TD>%(addr)s</TD></TR> </TABLE>>'

@topoview.route('/topo/')
def index():
    root_id = request.args.get('root_id', 1000, type=int)
    level = request.args.get('level', 2, type=int)
    prog = request.args.get('prog', 'dot')

    from flask import current_app
    base_path = current_app.root_path
    area_url = lambda root_id : url_for('topo.index', root_id=root_id, level=level, prog=prog)
    graph = pydot.Dot(graph_type='digraph')
    root_area = Area.query.get_or_404(root_id)

    def touch_areas(root, lv=level):
        if lv == -1:
            return
        area_children = root.children
        node_children = Node.query.filter(Node.area_id==root.id).all()
        cur_area_attrs = area_style.copy()
        cur_area_attrs['peripheries'] = 2 if area_children else 1
        cur_area_attrs['fillcolor'] = 'lightgreen' if node_children else 'lightblue'
        if area_children:
            cur_area_attrs['URL'] = area_url(root.id)
        else:
            cur_area_attrs['color'] = 'silver'
            cur_area_attrs['fillcolor'] = 'lightgray'
            
        root_node =  pydot.Node('areas_%d' % root.id, label=root.name, **cur_area_attrs)
        graph.add_node(root_node)

        # touch areas
        for area_child in area_children:
            area_child_node = touch_areas(area_child, lv=lv-1)
            if area_child_node:
                graph.add_edge(pydot.Edge(root_node, area_child_node, **area_edge_style))

        if lv == 0:
            return root_node
        return root_node

    root_node = touch_areas(root_area)
    root_name = root_node.get_name()
    for edge in graph.get_edges():
        if root_name in edge.obj_dict['points']:
            graph.del_edge(edge.obj_dict['points'])
    graph.del_node(root_name)
    
    base = root_area
    breadcrumb = [base]
    while base.parent:
        breadcrumb.append(base.parent)
        base = base.parent
    svg = unicode(graph.create(prog=prog, format='svg'), 'utf-8')
    svg = svg[245:].replace(base_path, '')
    return render_template("topo/index.html", svg=svg, breadcrumb=breadcrumb,
                           root_id=root_id, level=level, prog=prog)
    

@topoview.route('/topo/network')
def network():
    from flask import current_app
    base_path = current_app.root_path
    img_path = base_path + '/static/img/topo/'

    node_id = request.args.get('node_id', '', type=int)
    area_id = request.args.get('area_id', 1000, type=int)
    level = request.args.get('level', 2, type=int)
    prog = request.args.get('prog', 'dot')
    
    area_url = lambda a_id : url_for('topo.network', area_id=a_id, level=level, prog=prog)
    node_url = lambda n_id : url_for('topo.network', node_id=n_id, level=level, prog=prog)
    node_label = lambda node : table_template % dict(src=img_path+images[node.category.name],
                                                     name=node.name, addr=node.addr)
    graph = pydot.Dot(graph_type='digraph')
    root_node = Node.query.get(node_id) if node_id else None
    root_area = Area.query.get(area_id) if not node_id else None

    def add_node(node, graph=graph):
        node_id = 'nodes_%d' % node.id
        node_node =  graph.get_node(node_id)
        if not node_node:
            color = 'lightgreen' if node.status == 1 else 'tomato'
            node_attrs = node_style.copy()
            children_cnt = Node.query.filter(Node.controller_id==node.id).count()
            if children_cnt > 0:
                node_attrs['URL'] = node_url(node.id)
            node_node = pydot.Node(node_id, label=node_label(node),
                                   color=color, **node_attrs)
            graph.add_node(node_node)
        else:
            node_node = node_node[0]
        return node_node

    def touch_nodes(node, lv=level, graph=graph):
        if lv < 0:
            return None
        controller_node = add_node(node, graph)
        children = Node.query.filter(Node.controller_id==node.id).all()
        for child in children:
            child_node = touch_nodes(child, lv=lv-1)
            if child_node:
                graph.add_edge(pydot.Edge(controller_node, child_node, **node_edge_style))
        return controller_node
        
    def touch_areas(area, lv=level, graph=graph):
        area_node = pydot.Node('areas_%d' % area.id, label=area.name,
                               URL=area_url(area.id), **area_style)
        graph.add_node(area_node)

        if area.id == 1000:
            children = area.children
            for child in children:
                child_node = pydot.Node('areas_%d' % child.id, label=child.name,
                                       URL=area_url(child.id), **area_style)
                graph.add_node(child_node)
        else:
            nodes = Node.query.filter(Node.area_id==area.id).all()
            for node in nodes:
                touch_nodes(node, lv=lv-1)
                
        return area_node
        
    cur_root_node = touch_nodes(root_node) if root_node else touch_areas(root_area)
    cur_root_name = cur_root_node.get_name()
    for edge in graph.get_edges():
        if cur_root_name in edge.obj_dict['points']:
            graph.del_edge(edge.obj_dict['points'])
    graph.del_node(cur_root_name)
        
    # Making breadcrumb
    def make_area_breadcrumb(t_area):
        breadcrumb = list()
        if t_area:
            base_area = t_area
            breadcrumb.append(base_area)
            while base_area.parent:
                breadcrumb.append(base_area.parent)
                base_area = base_area.parent
        return breadcrumb
        
    def make_node_breadcrumb(t_node):
        breadcrumb = list()
        if t_node:
            base_node = t_node
            while base_node:
                breadcrumb.append(base_node)
                if base_node.controller_id is None:
                    break
                base_node = Node.query.get(base_node.controller_id)

            area_breadcrumb = make_area_breadcrumb(base_node.area)
            breadcrumb.extend(area_breadcrumb)
        return breadcrumb

    breadcrumb = make_node_breadcrumb(root_node) if root_node else make_area_breadcrumb(root_area)

    print unicode(graph.create(prog=prog, format='dot'), 'utf-8')
    svg = unicode(graph.create(prog=prog, format='svg'), 'utf-8')
    svg = svg[245:].replace(base_path, '')
    return render_template("topo/network.html", svg=svg, breadcrumb=breadcrumb,
                           area_id=area_id, node_id=node_id,
                           level=level, prog=prog)
    
@topoview.route('/topo/system')
def system():
    return render_template('topo/system.html')

@topoview.route('/gis')
def gis():
    return render_template('gis/index.html')
    
@topoview.route('/topo/view-all')
def view_all():
    node_url = lambda id : url_for('nodes.node_show', id=id)

    graph = pydot.Dot(graph_type='digraph')
    def mk_areas(root):
        if root is None:
            return

        area_root = pydot.Node('areas_%d' % root.id, label=root.name, **area_style)
        graph.add_node(area_root)

        node_nodes = Node.query.filter_by(area_id=root.id)
        for node in node_nodes:
            n_current = pydot.Node('nodes_%d' % node.id, label=node.id,#label='%s\n%s' % (node.name, node.addr),
                                   URL=node_url(node.id))
            graph.add_node(n_current)
            graph.add_edge(pydot.Edge(area_root, n_current))

            if node.controller_id:
                controller = Node.query.get(node.controller_id)
                node_id = 'nodes_%d' % node.controller_id
                n_controller =  graph.get_node(node_id)
                if not n_controller:
                    n_controller = pydot.Node('nodes_%d' % controller.id, label='%s<br />\n%s' % (node.name, node.addr))
                    graph.add_node(n_controller)
                else:
                    n_controller = n_controller[0]
                graph.add_edge(pydot.Edge(n_controller, n_current))

        for child in root.children:
            node_child = mk_areas(child)
            graph.add_node(node_child)
            graph.add_edge(pydot.Edge(area_root, node_child, **area_edge_style))

        return area_root

    graph.set_node_defaults(**node_style)
    graph.set_edge_defaults(**node_edge_style)

    root_area = Area.query.get(1000)
    mk_areas(root_area)
    svg = unicode(graph.create(prog='dot', format='svg'), 'utf-8')
    svg = svg[245:]
        
    return render_template("topo/index.html", svg=svg, menuid = 'topo')

navbar.add('topo', u'拓扑', 'random', '/topo')

