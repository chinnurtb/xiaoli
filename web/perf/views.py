# coding: utf-8

from datetime import datetime, timedelta
from flask import Blueprint, request, url_for, \
    redirect, render_template, flash, json

from tango.ui import navbar
from tango.models import db
from tango.ui.tables import make_table

from nodes.models import Node

from .models import *
from .tables import *
from .forms import PerfFilterForm, NodePerfFilterForm, pull_intervals, model_choices


perfview = Blueprint('perf', __name__, url_prefix="/perf")

@perfview.context_processor
def inject_navid():
    return dict(navid = 'perf')

# ==============================================================================
#  Ajax server
# ==============================================================================    
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

    
# ==============================================================================
#  Normal view
# ==============================================================================    
@perfview.route('/node/ping/')
def ping():
    form = NodePerfFilterForm(formdata=request.args)
    form.refresh_choices(request.args)
    query = form.filter(PingPerf)
    if form.keyword.data:
        query = query.filter(PingPerf.node.has(Node.alias=='%'+ form.keyword.data +'%'))
    table = make_table(query, PingTable)
    
    kwargs = {
        'menuid'     : 'ping',
        'name'       : 'ping',
        'title'      : u'PING延时',
        'table'      : table,
        'filterForm' : form
    }
    return render_template('/perf/index.html', **kwargs)


@perfview.route('/node/cpumem')
def cpumem():
    form = NodePerfFilterForm(formdata=request.args)
    form.refresh_choices(request.args)
    query = form.filter(CpuMemPerf)
    table = make_table(query, CpuMemTable)
    
    kwargs = {
        'menuid'     : 'cpumem',
        'name'       : 'cpumem',
        'title'      : u'CPU/内存',
        'table'      : table,
        'filterForm' : form
    }
    return render_template('/perf/index.html', **kwargs)


@perfview.route('/node/intfusage/<name>/')
def intfusage(name):
    CONFIG = {
        'all' : ('intfusage', u'端口流量', NodePerfFilterForm ,IntfUsageTable),
        'onu' : ('intfusage_onu', u'用户口占用率', PerfFilterForm, IntfUsageTable),
    }
    menuid, title, form_cls, table_cls = CONFIG[name]
    
    form = form_cls(formdata=request.args)
    form.refresh_choices(request.args)
    query = form.filter(IntfUsagePerf)
    table = make_table(query, table_cls)
    
    kwargs = {
        'menuid'     : menuid,
        'name'       : name,
        'title'      : title,
        'table'      : table,
        'filterForm' : form
    }
    return render_template('/perf/index.html', **kwargs)
    

@perfview.route('/node/intftraffic/<name>/')
def intftraffic(name):
    CONFIG = {
        'all'    : ('intftraffic', u'端口流量'),
        'oltup'  : ('intftraffic_oltup', u'上联口流量流速'),
        'oltpon' : ('intftraffic_oltpon', u'PON口流量流速'),
        'onupon' : ('intftraffic_onupon', u'PON口流量流速'),
        'eocup'  : ('intftraffic_eocup', u'上联口流量流速'),
        'cpe'    : ('intftraffic_cpe', u'CPE流量流速')
    }
    menuid, title = CONFIG[name]
    form_cls, table_cls = (NodePerfFilterForm, IntfTrafficTable) if name == 'all' \
                          else (PerfFilterForm, IntfTrafficOctetsTable)
    
    form = form_cls(formdata=request.args)
    form.refresh_choices(request.args)
    query = form.filter(IntfTrafficPerf)
    table = make_table(query, table_cls)
    
    kwargs = {
        'menuid'     : menuid,
        'name'       : name,
        'title'      : title,
        'table'      : table,
        'filterForm' : form
    }
    return render_template('/perf/index.html', **kwargs)

    
@perfview.route('/olt/ponusage/')
def ponusage():
    form = PerfFilterForm(formdata=request.args)
    form.refresh_choices(request.args)
    query = form.filter(PonUsagePerf)
    table = make_table(query, PonUsageTable)
    
    kwargs = {
        'menuid'     : 'ponusage',
        'name'       : 'ponusage',
        'title'      : u'PON口占用率',
        'table'      : table,
        'filterForm' : form
    }
    return render_template('/perf/index.html', **kwargs)
    

@perfview.route('/olt/ponpower/')
def ponpower():
    form = PerfFilterForm(formdata=request.args)
    form.refresh_choices(request.args)
    query = form.filter(PonPowerPerf)
    table = make_table(query, PonPowerTable)
    
    kwargs = {
        'menuid'     : 'ponpower',
        'name'       : 'ponpower',
        'title'      : u'PON口光功率',
        'table'      : table,
        'filterForm' : form
    }
    return render_template('/perf/index.html', **kwargs)

    
# ==============================================================================
#  Other
# ==============================================================================    
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
    key = request.args.get('key', '')
    num = request.args.get('num', 50, type=int)
    CLSES = {
        'ping' : PingPerf,
        'cpumem': CpuMemPerf,
        'board': BoardPerf,
        'intftraffic': IntfTrafficPerf,
        'ponpower': PonPowerPerf,
        'ponusage': PonUsagePerf,
        'pontraffic': PonTrafficPerf
    }
    if not key or key not in CLSES.keys():
        return '*key* ERROR(%s)!\r\n %s' % (key, str(CLSES))
    cls = CLSES[key]
    
    for i in range(num):
        year = 2012
        month = rand.randint(9, 10)
        monthrange = calendar.monthrange(year, month)[1]
        day = rand.randint(1, monthrange)
        hour = rand.randint(0, 23)
        minute = rand.randint(0, 59)
        dt = datetime(year, month, day, hour, minute)
        
        obj = cls()
        obj.sampletime = dt
        obj.sampleyear = year
        obj.samplemonth = month
        obj.sampleday = day
        obj.sampleweekday = dt.weekday()
        obj.samplehour = hour
        obj.nodeid = 104
        if key == 'cpumem':
            attrs = ['cupavg', 'cpumax',
                     'memavg', 'memmax',
                     'tempavg', 'tempmax']
            for attr in attrs:
                value = rand.random() * 20
                setattr(obj, attr, value)
        
        db.session.add(obj)
    db.session.commit()
    return 'OK: ' + str(num)


navbar.add('perf', u'性能', 'signal', '/perf/node/ping')

