# coding: utf-8

from datetime import datetime
import calendar
from flask import Blueprint, request, url_for, \
    redirect, render_template, flash, json

from tango.ui import navbar
from tango.models import db, Category
from tango.ui.tables import make_table

from alarms.models import AlarmSeverity
from .models import Threshold, Metric

from .models import NodePerf, PortPerf

from .tables import *

from .forms import ThresholdEditForm, ThresholdNewForm, MetricNewEditForm

from .forms import PerfFilterForm

perfview = Blueprint('perf', __name__, url_prefix="/perf")

CONFIG = {
    'lan_ping': (u'PING时延', NodePerf, PingTable, ['pingrta', 'pingrtmax', 'pingrtmin']),
    'lan_cpumem': (u'CPU内存', NodePerf, CpuMemTable, []),
    'lan_portusage': (u'端口占用', NodePerf, PortUsageTable, []),
    'lan_traffic': (u'端口流量', PortPerf, PortPerfTable, []),
    'olt_ping': (u'PING时延', NodePerf, PingTable, ['pingrta', 'pingrtmax', 'pingrtmin'])
}

@perfview.context_processor
def inject_navid():
    return dict(navid = 'perf')

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
    title, metrics = CONFIG[menuid]
    form = PerfFilterForm(formdata=request.args)
    return render_template('/perf/lan/index.html',
        menuid = menuid, title = title,
        name = name, filterForm = form)
    

@perfview.route('/switches')
def switches():
    form = PerfFilterForm(formdata=request.args)
    q = NodePerf.query
    t = make_table(q, NodePerfTable)
    return render_template('/perf/switches/index.html',
        filterForm = form, table=t)

    
@perfview.route('/switches/intervals')
def switches_intervals():
    """ 给搜索表单中的时间选框提供 ajax 服务 """
    key = request.args.get('key', '')
    now = datetime.now()
    WEEK = [u'星期一', u'星期二', u'星期三', u'星期四', u'星期五', u'星期六', u'星期日']
    res = []
    if key == 'today':
        res = [(str(i), u'%d点'%i) for i in range(now.hour + 1)]
    elif key == 'yesterday':
        res = [(str(i), u'%d点'%i) for i in range(24)]
    elif key == 'thisweek':
        res = [(str(i), WEEK(i)) for i in range(now.weekday() + 1)]
    elif key == 'lastweek':
        res = [(str(i), WEEK(i)) for i in range(7)]
    elif key == 'thismonth':
        res = [(str(i), u'%d号'%i) for i in range(1, now.day+1)]
    elif key == 'lastmonth':
        t = (now.year-1, 12) if now.month == 1 else (now.year, now.month-1)
        day_num = calendar.monthrange(*t)[1]
        res = [(str(i), u'%d号'%i) for i in range(1, day_num+1)]
    res.insert(0, ('all', '全部时段'))
    return json.dumps(dict(res))
    
    
    
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
#  阀值管理
# ==============================================================================
@perfview.route('/')
@perfview.route('/thresholds/')
def thresholds():
    keyword = request.args.get('keyword', '')
    query = Threshold.query
    if keyword:
        ikeyword = '%' + keyword + '%'
        query = query.filter(db.or_(Threshold.name.ilike(ikeyword),
                                    Threshold.alias.ilike(ikeyword),
                                    Threshold.category.has(Category.alias.ilike(ikeyword)),
                                    Threshold.summary.ilike(ikeyword)))
    table = make_table(query, ThresholdTable)
    return render_template("perf/thresholds/index.html", table=table, keyword=keyword)
    
    
@perfview.route('/thresholds/edit/<int:id>', methods=['GET', 'POST'])
def thresholds_edit(id):
    form = ThresholdEditForm()
    threshold = Threshold.query.get_or_404(id)
    
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(threshold)
        threshold.severity1 = threshold.severity1.id
        threshold.severity2 = threshold.severity2.id
        db.session.commit()
        
        flash(u'阀值 %s 修改成功' % threshold.name, 'success')
        return redirect(url_for('perf.thresholds'))
        
    form.process(obj=threshold)
    form.severity1.data = AlarmSeverity.query.get(threshold.severity1)
    form.severity2.data = AlarmSeverity.query.get(threshold.severity2)
    return render_template("perf/thresholds/edit.html", form=form, id=id)

    
@perfview.route('/thresholds/new', methods=['GET', 'POST'])
def thresholds_new():
    form = ThresholdNewForm()
    if form.is_submitted and form.validate_on_submit():
        threshold = Threshold()
        form.populate_obj(threshold)
        threshold.severity1 = threshold.severity1.id
        threshold.severity2 = threshold.severity2.id
        db.session.add(threshold)
        db.session.commit()
        
        flash(u'阀值 %s 添加成功' % threshold.name, 'success')
        return redirect(url_for('perf.thresholds'))
        
    return render_template("perf/thresholds/new.html", form=form, )


# ==============================================================================
#  指标管理
# ==============================================================================    
@perfview.route('/metrics/')
def metrics():
    table = make_table(Metric.query, MetricTable)
    return render_template('perf/metrics/index.html', table=table)

    
@perfview.route('/metrics/new', methods=['GET', 'POST'])
def metrics_new():
    form = MetricNewEditForm()
    if form.is_submitted and form.validate_on_submit():
        metric = Metric()
        form.populate_obj(metric)
        db.session.add(metric)
        db.session.commit()
        flash(u'指标 (%s) 添加成功!' % metric.name, 'success')
        return redirect(url_for('perf.metrics'))
        
    return render_template('perf/metrics/new-edit.html', form=form,
                           action=url_for('perf.metrics_new'), title=u'添加指标')

    
@perfview.route('/metrics/edit/<int:id>', methods=['GET', 'POST'])
def metrics_edit(id):
    form = MetricNewEditForm()
    metric = Metric.query.get_or_404(id)
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(metric)
        db.session.commit()
        flash(u'指标 (%s) 编辑成功' % metric.name, 'success')
        return redirect(url_for('perf.metrics'))
        
    form.process(obj=metric)
    return render_template('perf/metrics/new-edit.html', form=form,
                           action=url_for('perf.metrics_edit', id=id), title=u'编辑指标')

    

# ==============================================================================
#  Test
# ==============================================================================
@perfview.route('/t-collapse')
def test_collapse():
    return render_template('perf/test-collapse.html')


@perfview.route('/t-fieldset')
def test_fieldset():
    return render_template('perf/test-fieldset.html')


navbar.add('perf', u'性能', '/perf')

