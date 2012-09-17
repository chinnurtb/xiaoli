#!/usr/bin/env python
# -*- coding: utf-8 -*-
from configs import (area_stacked, pie_basic, spline_plot_bands, bar_basic, bar_stacked,
                      bar_negative_stack, column_rotated_labels, column_negative,
                     line_time_series)
from tango.base import AutoIncrDict
import time
import demjson
import copy


# class ChartMeta(type):
#     def __new__(cls, name, bases, attrs):

#         config = attrs.get('config', None)
#         if config:
#             for k, v in config.iteritems():
#                 attrs[k] = v
#         else:
#             raise AttributeError('There must be *config* in a Class!')
                
#         #  Here a *return* is required
#         return super(ChartMeta, cls).__new__(cls, name, bases, attrs) 
        

class Chart(AutoIncrDict):
    # __metaclass__ = ChartMeta

    config = {'title': None}

    @staticmethod
    def mktime(dt):
        return time.mktime(dt.timetuple()) * 1000
    
    def dumps(self):
        return demjson.encode(self)
        
        
    def __init__(self, **kwargs):
        # Default options
        for k, v in self.config.iteritems():
            cv = copy.deepcopy(v)
            self[k] = cv

        self.html_id = 'container'
        self.min_width = '400px'
        self.height = '400px'
        # self['series'] = None
        # Custom options
        for k, v in kwargs:
            self[k] = v

    def set_html_id(self, html_id):
        self.html_id = html_id
        self['chart']['renderTo'] = html_id
    
        
class AreaStackedChart(Chart):
    '''
    [http://www.highcharts.com/demo/area-stacked]'''
    
    config = demjson.decode(area_stacked)


    
class PieBasicChart(Chart):
    '''
    [http://www.highcharts.com/demo/pie-basic] '''
    
    config = demjson.decode(pie_basic)
    


class SplinePlotBandsChart(Chart):
    '''
    [http://www.highcharts.com/demo/spline-plot-bands]'''

    config = demjson.decode(spline_plot_bands)
    

    
class BarBasicChart(Chart):
    '''
    [http://www.highcharts.com/demo/bar-basic]'''

    config = demjson.decode(bar_basic)
    

class BarStacked(Chart):
    '''
    [http://www.highcharts.com/demo/bar-stacked] '''

    config = demjson.decode(bar_stacked)
    
    

class BarNegativeStackChart(Chart):
    '''
    [http://www.highcharts.com/demo/bar-negative-stack]'''

    config = demjson.decode(bar_negative_stack)

    

class ColumnRotatedLabels(Chart):
    '''
    [http://www.highcharts.com/demo/column-rotated-labels]
    '''
    config = demjson.decode(column_rotated_labels)
    

class ColumnNegativeChart(Chart):
    '''
    [http://www.highcharts.com/demo/column-negative]'''

    config = demjson.decode(column_negative)


class LineTimeSeriesChart(Chart):
    '''
    [http://www.highcharts.com/demo/line-time-series]'''

    config = demjson.decode(line_time_series)
    

# ==============================================================================
#  Unit Test
# ==============================================================================    
if __name__ == '__main__':
    print dir(AreaStackedChart)
