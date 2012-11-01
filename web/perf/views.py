# coding: utf-8

from datetime import datetime, timedelta
from flask import Blueprint, request, url_for, \
    redirect, render_template, flash, json

from tango.ui import navbar
from tango.models import db
from tango.ui.tables import make_table

from .models import *
from .tables import NodePerfTable

from .forms import PerfFilterForm, NodePerfFilterForm, pull_intervals, model_choices
from .tables import *

perfview = Blueprint('perf', __name__, url_prefix="/perf")

@perfview.context_processor
def inject_navid():
    return dict(navid = 'perf')

    
@perfview.route('/refresh/intervals')
def ajax_refresh_intervals():
    """ 刷新表单中的时间选框中的选项 """
    key = request.args.get('key', '')
    res = pull_intervals(key)
    return json.dumps(res)

    
@perfview.route('/refresh/models')
def ajax_refresh_models():
    """ 刷新搜索表单中的 (厂商/型号) 选框中的选项 """
    vendors = dict(request.values.lists()).get('vendors[]', [])
    models = apply(model_choices(vendors))
    res = [[str(model.id), model.alias] for model in models]
    return json.dumps(res)
    
        
# CONFIG = {
#     'node_ping'      : (u'PING时延', PingPerf, PingTable, ['pingrta', 'pingrtmax', 'pingrtmin']),
#     'node_cpumem'    : (u'CPU内存', CpuMemPerf, CpuMemTable, []),
#     'node_intfusage' : (u'端口占用', IntfUsagePerf, PortUsageTable, []),
#     'node_traffic'   : (u'端口流量', PortPerf, PortPerfTable, []),
#     'olt_uptraffic'  : (u'上联口流量流速', PortPerf, PortPerfTable, []),
#     'olt_pontraffic' : (u'PON口流量流速', PortPerf, PortPerfTable, []),
#     'olt_ponusage'   : (u'PON口占用率', PortPerf, PortUsageTable, []),
#     'olt_ponpower'   : (u'PON口光功率', PortPerf, PonPowerTable, []),
#     'onu_pontraffic' : (u'PON口流量流速', PortPerf, PortPerfTable, []),
#     'onu_intfusage'  : (u'用户口占用率', PortPerf, PortUsageTable, []),
#     'eoc_uptraffic'  : (u'上联口流量流速', PortPerf, PortPerfTable, []),
#     'eoc_cpetraffic' : (u'CPE口流量流速', PortPerf, PortPerfTable, [])
# }


@perfview.route('/ping/')
def ping():
    pass

@perfview.route('/cpumem/')
def cpumen():
    pass

    
# @perfview.route('/node/<name>')
# def node(name):
#     menuid = 'node_' + name
#     title, model, tblcls, metrics = CONFIG[menuid]

#     form = NodePerfFilterForm(formdata=request.args)
#     # print request.args
#     # print form.data
#     form.refresh_choices(request.args)
#     # print form.data
#     query = form.filter(model)
    
#     table = make_table(query, tblcls)
#     return render_template('/perf/node/index.html',
#         menuid = menuid, title = title,
#         name = name, filterForm = form,
#         table = table)

# @perfview.route('/lan/<name>')
# def lan(name):
#     menuid = 'lan_' + name
#     title, model, tblcls, metrics = CONFIG[menuid]
    
#     form = PerfFilterForm(formdata=request.args)
#     form.refresh_choices(request.args)
#     query = form.filter(model)
    
#     table = make_table(query, tblcls)
#     return render_template('/perf/lan/index.html',
#         menuid = menuid, title = title,
#         name = name, filterForm = form,
#         table = table)

# @perfview.route('/olt/<name>')
# def olt(name):
#     menuid = 'olt_' + name
#     title, model, tblcls, metrics = CONFIG[menuid]

#     form = PerfFilterForm(formdata=request.args)
#     form.refresh_choices(request.args)
#     query = form.filter(model)
    
#     table = make_table(query, tblcls)
#     return render_template('/perf/olt/index.html',
#         menuid = menuid, title = title,
#         name = name, filterForm = form,
#         table = table)

# @perfview.route('/onu/<name>')
# def onu(name):
#     menuid = 'onu_' + name
#     title, model, tblcls, metrics = CONFIG[menuid]

#     form = PerfFilterForm(formdata=request.args)
#     form.refresh_choices(request.args)
#     query = form.filter(model)
    
#     table = make_table(query, tblcls)
#     return render_template('/perf/onu/index.html',
#         menuid = menuid, title = title,
#         name = name, filterForm = form,
#         table = table)

# @perfview.route('/eoc/<name>')
# def eoc(name):
#     menuid = 'eoc_' + name
#     title, model, tblcls, metrics = CONFIG[menuid]
    
#     form = PerfFilterForm(formdata=request.args)
#     form.refresh_choices(request.args)
#     query = form.filter(model)
    
#     table = make_table(query, tblcls)
#     return render_template('/perf/eoc/index.html',
#         menuid = menuid, title = title,
#         name = name, filterForm = form,
#         table = table)

    
@perfview.route('/olts/')
def olts():
    return render_template('perf/olts/index.html')

@perfview.route('/olt_boards/')
def olt_boards():
    return render_template('/perf/boards/index.html')

@perfview.route('/demo-table')
def demo_table():
    return render_template('perf/demo_table.html')


# ==============================================================================
#  Test
# ==============================================================================
@perfview.route('/t-collapse')
def test_collapse():
    return render_template('perf/test-collapse.html')


@perfview.route('/t-fieldset')
def test_fieldset():
    return render_template('perf/test-fieldset.html')



@perfview.route('/do-db')
def add_time():
    from random import Random
    import calendar
    rand = Random()
    num = request.args.get('num', 50, type=int)
        
    for i in range(num):
        year = 2012
        month = rand.randint(9, 10)
        monthrange = calendar.monthrange(year, month)[1]
        day = rand.randint(1, monthrange)
        hour = rand.randint(0, 23)
        minute = rand.randint(0, 59)
        dt = datetime(year, month, day, hour, minute)
        node_perf = PortPerf()
        node_perf.sampletime = dt
        node_perf.sampleyear = year
        node_perf.samplemonth = month
        node_perf.sampleday = day
        node_perf.sampleweekday = dt.weekday()
        node_perf.samplehour = hour
        node_perf.nodeid = 104
        db.session.add(node_perf)
    db.session.commit()
    return 'OK: ' + str(num)


navbar.add('perf', u'性能', '/perf/node/ping')

