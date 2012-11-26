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

@topoview.route('/topo/')
def index():
    return render_template('topo/index.html')

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
                'level'    : len(lvs),
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
        A = {'name': 'ONU-' + str(ca), 'children': [],
             'level': onu_lv, 'id': 'onu-'+str(ca), "size": ca * 30}
        for b in range(nb):     # EOC
            cb = b+1
            B = {'name': 'EOC-' + str(cb), 'children': [],
                 'level': eoc_lv, 'id': 'eoc-'+str(cb), "size": cb * 30}
            nc = nnc
            for c in range(nc): # CPE
                cc = c+1
                C = {'name': 'CPE-' + str(cc), 'url': 'http://www.stackoverflow.com',
                     'level': cpe_lv, 'id': 'cpe-'+str(cc) , "size": cc * 30}
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
    # pdict(data)
    return json.dumps(data)

navbar.add('topo', u'拓扑', 'random', '/topo')

