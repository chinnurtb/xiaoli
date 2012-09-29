#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

import pydot
from tango.ui import menus, Menu
from nodes.models import Node, Area

topoview = Blueprint('topo', __name__)

@topoview.route('/topo/')
def index():
    area_style = {
        'shape' : 'polygon',
        'sides' : 4,
        'style' : 'filled',
        'fillcolor' : 'lightblue',
        'color' : 'lightblue',
        'peripheries': 2,
        'fontsize' : '12.0'
    }

    node_style = {
        'shape' : 'ellipse',
        'style' : 'filled',
        'color' : '#999999',
        'fillcolor' : 'orange',
        'fontsize' : '10.0'
    }

    area_edge_style = {
        'color' : '#222222',
        'labelfontcolor' : '#009933',
    }

    node_edge_style = {
        'color' : '#999999',
        'labelfontcolor' : '#009933',
    }

    node_url = lambda id : url_for('nodes.node_edit', id=id)

    graph = pydot.Dot(graph_type='digraph')
    def mk_areas(root):
        if root is None:
            return

        area_root = pydot.Node('areas_%d' % root.id, label=root.name, **area_style)
        graph.add_node(area_root)

        node_nodes = Node.query.filter_by(area_id=root.id)
        for node in node_nodes:
            n_current = pydot.Node('nodes_%d' % node.id, label='%s\n%s' % (node.name, node.addr),
                                   URL=node_url(node.id))
            graph.add_node(n_current)
            graph.add_edge(pydot.Edge(area_root, n_current))

            if node.controller_id:
                controller = Node.query.get(node.controller_id)
                node_id = 'nodes_%d' % node.controller_id
                n_controller =  graph.get_node(node_id)
                if not n_controller:
                    n_controller = pydot.Node('nodes_%d' % controller.id, label='%s\n%s' % (node.name, node.addr))
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
        
    return render_template("topo/index.html", svg=svg, menuid = 'topo')

menus.append(Menu('topo', u'拓扑', 'topo'))
