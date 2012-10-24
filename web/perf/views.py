# coding: utf-8

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from sqlalchemy import func

from tango.ui import menus, Menu
from tango.models import db
from tango.base import make_table

from alarms.models import AlarmSeverity
from .models import Miboid, Threshold
from .tables import MiboidTable, ThresholdTable
from .forms import ThresholdForm

perfview = Blueprint('perf', __name__, url_prefix="/perf")


@perfview.route('/t-collapse')
def test_collapse():
    return render_template('perf/test-collapse.html')


@perfview.route('/')
@perfview.route('/thresholds/')
def thresholds():
    table = make_table(Threshold.query, ThresholdTable)
    return render_template("perf/thresholds/index.html", table=table)
    
@perfview.route('/thresholds/edit/<int:id>', methods=['GET', 'POST'])
def thresholds_edit(id):
    form = ThresholdForm()
    threshold = Threshold.query.get_or_404(id)
    
    if form.is_submitted and form.validate_on_submit():
        form.populate_obj(threshold)
        threshold.severity1 = threshold.severity1.id
        threshold.severity2 = threshold.severity2.id
        flash(u'阀值 %s 修改成功' % threshold.name, 'success')
        db.session.commit()
        return redirect(url_for('perf.thresholds'))
        
    form.process(obj=threshold)
    form.severity1.data = AlarmSeverity.query.get(threshold.severity1)
    form.severity2.data = AlarmSeverity.query.get(threshold.severity2)
    return render_template("perf/thresholds/edit.html", form=form, id=id)
    
    
@perfview.route('/metrics/')
def metrics():
    return render_template('perf/metrics/index.html')

    
@perfview.route('/miboids/')
def miboids():
    mib = request.args.get('mib', '')
    query = Miboid.query
    if mib:
        query = query.filter_by(mib=mib)
    table = make_table(query, MiboidTable)
    mibs = db.session.query(func.distinct(Miboid.mib)).all()
    return render_template("perf/miboids/index.html", table=table, mibs=mibs, mib=mib)

menus.append(Menu('perf', u'性能', '/perf'))
