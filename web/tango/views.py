#!/usr/bin/env python
# -*- coding: utf-8 -*-

from flask import Blueprint, request, make_response, render_template
from users.models import User

tangoview = Blueprint('tango', __name__)

from tango.ui import tables

class SettingTable(tables.Table):
    
    alias   = table.Column(verbose_name='名称', endpoint='settings_edit')


# ==============================================================================
#  Charts
# ==============================================================================    
from .tdata import *
from tango.ui.charts.nvd3charts import *
from tango.ui.charts.highcharts import *

@tangoview.route('/test-nvd3charts/<int:index>')
def test_nvd3charts(index):
    lst = [(PieChart(), pieChart_data),
           (StackedAreaChart(), stackedAreaChart_data),
           (DiscreteBarChart(), discreteBarChart_data),
           (CumulativeLineChart(), cumulativeLineChart_data),
           (MultiBarHorizontalChart(), multiBarHorizontalChart_data)]
    chart = lst[index][0]
    data = lst[index][1]
    chart.data = json.dumps(data)
    return render_template('users/test_nvd3charts.html', chart=chart)


    
@tangoview.route('/test-highcharts/<int:index>')
def test_highcharts(index):
    lst = [AreaStackedChart, PieBasicChart, SplinePlotBandsChart,
           BarBasicChart, BarStacked, BarNegativeStackChart, ColumnRotatedLabels,
           ColumnNegativeChart, LineTimeSeriesChart]

    chart = lst[index]()
    chart.set_html_id('TEST title')
    
    return render_template('users/test_highcharts.html', chart=chart)


@tangoview.route('/highchart-export', methods=['POST'])
def highchart_export():
    from tango.ui.cairosvg import svg2png, svg2pdf, svg2svg
    svg = request.form.get('svg', None)
    filename = request.form.get('filename', None)
    # width = request.form.get('width', None)
    content_type = request.form.get('type', None)
    
    type_dict = {
        'image/png'       : {'ext':'png',
                             'converter': svg2png},
        'application/pdf' : {'ext':'pdf',
                             'converter': svg2pdf},
        'image/svg+xml'   : {'ext':'svg',
                             'converter': svg2svg},
    }
    
    ext = type_dict[content_type]['ext']
    svg = unicode(svg).encode('utf-8')
    content = type_dict[content_type]['converter'](svg)
    resp = make_response(content)
    resp.headers['Content-disposition'] = 'attachment; filename=%s.%s' % (filename, ext)
    resp.headers['Content-Type'] = ';'.join([content_type, 'charset=utf-8'])
    return resp


    
@tangoview.route('/get-chat-json', methods=['POST'])
def get_chat_json():
    resp = '''[
  {
    key: "Cumulative Return",
    values: [
      {
        "label" : "CDS / Options" ,
        "value" : 29.765957771107
      } ,
      {
        "label" : "Cash" ,
        "value" : 0.0000000001
      } ,
      {
        "label" : "Corporate Bonds" ,
        "value" : 32.807804682612
      } ,
      {
        "label" : "Equity" ,
        "value" : 196.45946739256
      } ,
      {
        "label" : "Index Futures" ,
        "value" : 0.19434030906893
      } ,
      {
        "label" : "Options" ,
        "value" : 98.079782601442
      } ,
      {
        "label" : "Preferred" ,
        "value" : 13.925743130903
      } ,
      {
        "label" : "Not Available" ,
        "value" : 5.1387322875705
      }
    ]
  }
]'''
    return resp
    
