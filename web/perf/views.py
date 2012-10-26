# coding: utf-8

from flask import Blueprint, request, url_for, \
    redirect, render_template, flash


from tango.ui import menus, Menu
from tango.models import db, Category
from tango.base import make_table

from alarms.models import AlarmSeverity
from .models import Threshold, Metric
from .tables import ThresholdTable, MetricTable
from .forms import ThresholdEditForm, ThresholdNewForm

perfview = Blueprint('perf', __name__, url_prefix="/perf")


@perfview.route('/t-collapse')
def test_collapse():
    return render_template('perf/test-collapse.html')


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
        
    return render_template("perf/thresholds/new.html", form=form)

    
@perfview.route('/metrics/')
def metrics():
    table = make_table(Metric.query, MetricTable)
    return render_template('perf/metrics/index.html', table=table)

@perfview.route('/metrics/new', methods=['GET', 'POST'])
def metrics_new():
    pass

@perfview.route('/metrics/edit', methods=['GET', 'POST'])
def metrics_edit():
    return render_template('')

menus.append(Menu('perf', u'性能', '/perf'))
