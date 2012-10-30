# coding: utf-8


from flask import Blueprint, request, url_for, \
    redirect, render_template, flash, json

from tango.ui import navbar
from tango.models import db, Category
from tango.ui.tables import make_table

from .models import NodePerf, PortPerf
from .tables import NodePerfTable

from .forms import PerfFilterForm, pull_intervals, model_choices
from .tables import *

from .forms import PerfFilterForm

perfview = Blueprint('perf', __name__, url_prefix="/perf")

CONFIG = {
    'node_ping': (u'PING时延', NodePerf, PingTable, ['pingrta', 'pingrtmax', 'pingrtmin']),
    'node_cpumem': (u'CPU内存', NodePerf, CpuMemTable, []),
    'lan_portusage': (u'端口占用', NodePerf, PortUsageTable, []),
    'lan_traffic': (u'端口流量', PortPerf, PortPerfTable, []),
    'olt_ping': (u'PING时延', NodePerf, PingTable, ['pingrta', 'pingrtmax', 'pingrtmin'])
}

@perfview.context_processor
def inject_navid():
    return dict(navid = 'perf')


    
@perfview.route('/node/<name>')
def node(name):
    menuid = 'node_' + name
    title, model, tblcls, metrics = CONFIG[menuid]
    table = make_table(model.query, tblcls)
    form = PerfFilterForm(formdata=request.args)
    return render_template('/perf/node/index.html',
        menuid = menuid, title = title,
        name = name, filterForm = form,
        table = table)

@perfview.route('/lan/<name>')
def lan(name):
    menuid = 'lan_' + name
    title, model, tblcls, metrics = CONFIG[menuid]
    table = make_table(model.query, tblcls)
    form = PerfFilterForm(formdata=request.args)
    return render_template('/perf/lan/index.html',
        menuid = menuid, title = title,
        name = name, filterForm = form,
        table = table)

@perfview.route('/olt/<name>')
def olt(name):
    menuid = 'olt_' + name
    title, model, tblcls, metrics = CONFIG[menuid]
    table = make_table(model.query, tblcls)
    form = PerfFilterForm(formdata=request.args)
    return render_template('/perf/olt/index.html',
        menuid = menuid, title = title,
        name = name, filterForm = form,
        table = table)

@perfview.route('/onu/<name>')
def onu(name):
    menuid = 'onu_' + name
    title, model, tblcls, metrics = CONFIG[menuid]
    table = make_table(model.query, tblcls)
    form = PerfFilterForm(formdata=request.args)
    return render_template('/perf/onu/index.html',
        menuid = menuid, title = title,
        name = name, filterForm = form,
        table = table)

@perfview.route('/eoc/<name>')
def eoc(name):
    menuid = 'eoc_' + name
    title, model, tblcls, metrics = CONFIG[menuid]
    table = make_table(model.query, tblcls)
    form = PerfFilterForm(formdata=request.args)
    return render_template('/perf/eoc/index.html',
        menuid = menuid, title = title,
        name = name, filterForm = form,
        table = table)

@perfview.route('/switches')
def switches():
    form = PerfFilterForm(formdata=request.args)
    form.intervals.choices = pull_intervals(request.args.get('sampletime'))
    form.models.query_factory = model_choices(request.args.get('vendors'))
    q = NodePerf.query
    t = make_table(q, NodePerfTable)
    return render_template('/perf/switches/index.html',
        filterForm = form, table=t)

    
@perfview.route('/switches/intervals')
def switches_intervals():
    """ 给搜索表单中的时间选框提供 ajax 服务 """
    key = request.args.get('key', '')
    res = pull_intervals(key)
    return json.dumps(res)

    
@perfview.route('/switches/models')
def switches_models():
    """ 给搜索表单中的 (厂商/型号) 选框提供 ajax 服务 """
    vendors = dict(request.values.lists()).get('vendors[]', [])
    models = apply(model_choices(vendors))
    res = [[str(model.id), model.alias] for model in models]
    return json.dumps(res)
    
    
@perfview.route('/olts/')
def olts():
    return render_template('perf/olts/index.html')

@perfview.route('/olt_boards/')
def olt_boards():
    return render_template('/perf/boards/index.html')

@perfview.route('/olt_pon_ports/')
def olt_pon_ports():
    return render_template('/perf/olt_pon_ports/index.html')

@perfview.route('/onus/')
def onus():
    return render_template('perf/onus/index.html')

@perfview.route('/onu_pon_ports/')
def onu_pon_ports():
    return render_template('/perf/onu_pon_ports/index.html')

@perfview.route('/eocs')
def eocs():
    return render_template('/perf/eocs/index.html')
    

# ==============================================================================
#  Test
# ==============================================================================
@perfview.route('/t-collapse')
def test_collapse():
    return render_template('perf/test-collapse.html')


@perfview.route('/t-fieldset')
def test_fieldset():
    return render_template('perf/test-fieldset.html')


navbar.add('perf', u'性能', '/perf/node/ping')

