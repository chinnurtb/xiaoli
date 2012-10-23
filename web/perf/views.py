# coding: utf-8

from flask import Blueprint, request, session, url_for, \
    redirect, render_template, g, flash

from tango.ui import menus, Menu
from tango.base import make_table
from .models import Miboid
from .tables import MiboidTable

perfview = Blueprint('perf', __name__, url_prefix="/perf")

@perfview.route('/')
def index():
    return render_template("perf/index.html")

@perfview.route('/t-collapse')
def test_collapse():
    return render_template('perf/test-collapse.html')
    
@perfview.route('/metrics/')
def metrics():
    return render_template('perf/metrics/index.html')

@perfview.route('/thresholds/')
def thresholds():
    return render_template("perf/thresholds/index.html")

@perfview.route('/miboids/')
def miboids():
    table = make_table(Miboid.query, MiboidTable)
    return render_template("perf/miboids/index.html", table=table)

menus.append(Menu('perf', u'性能', '/perf'))
