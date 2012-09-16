#!/usr/bin/env python
# -*- coding: utf-8 -*-

__all__ = ['PieChart', 'StackedAreaChart', 'MultiBarChart',
           'MultiBarHorizontalChart', 'DiscreteBarChart', 'CumulativeLineChart']

class Chart(object):
    '''
    Frequently use charts:
    - stackedAreaChart
    - pieChart
    - multiBarChart
    - multiBarHorizontalChart
    - cumulativeLineChart
    - discreteBarChart
    '''

    x_dict = {
        'd[0]' : "function(d) { return d[0] }",
        'd[1]' : "function(d) { return d[1] }",
        'd.label' : "function(d) { return d.label }",
    }
    y_dict = {
        'd[0]' : "function(d) { return d[0] }",
        'd[1]' : "function(d) { return d[1] }",
        'd[1]/100' : "function(d) { return d[1]/100 }",
        'd.value' : "function(d) { return d.value }",
    }

    x_tickFormat_dict = {
        'date' : "function(d) { return d3.time.format('%x')(new Date(d)) }",
        'd' : "d3.format('f')", # integer
        # 'r' : 'r',              # real
        # '.02f' : '.02f',        # float
    }
    y_tickFormat_dict = {
        'd' : 'f',
        # '' : '.2f',
        # '' : '.02f',
        # '' : '.01f',
        # '' : '.1f',
        # '' : '.1%',
        # '' : '.2%',
    }

    def get_x_tickFormat(self):
        print 'get_x_tickFormat'
        return self.x_tickFormat_dict.get(self.x_tickFormat,
                                          "d3.format('%s')" % self.x_tickFormat)

    def get_y_tickFormat(self):
        print 'get_y_tickFormat'
        return self.y_tickFormat_dict.get(self.y_tickFormat,
                                          "d3.format('%s')" % self.y_tickFormat)

    def get_color_range(self, category):
        return 'd3.scale.%s().range()' % category
        
    id = 'chart'
    title = 'Some Chart'
    height = '400px'
    width = '600px'
    svg_path = '#%s svg' % id
    duration = 500
    data = '[{}]'


# ==============================================================================
#  Frequently use!
# ==============================================================================
class StackedAreaChart(Chart):
    '''
    # NVD3
    Stacked Area Chart
    [http://nvd3.com/ghpages/stackedArea.html]

    #Highcharts
    [http://www.highcharts.com/demo/area-stacked]
    '''

    name = 'stackedAreaChart'

    x = 'd[0]'
    y = 'd[1]'
    x_tickFormat = 'date'
    y_tickFormat = ',.2f'
    clipEdge = 'true'
    showMaxMin = 'false'


class PieChart(Chart):
    '''
    # NVD3
    Pie Chart
    [http://nvd3.com/ghpages/pie.html]

    # Highcharts
    [http://www.highcharts.com/demo/pie-basic]
    '''

    functions = {
        'x': "function(d) { return d.label }",
        'y': "function(d) { return d.value }",
    }
    name = 'pieChart'

    x = 'd.label'
    y = 'd.value'
    showLabels = 'true'
    labelThreshold = 0.05
    donut = 'true'


class MultiBarChart(Chart):
    '''
    # NVD3
    Group / Stacked Bar Chart
    [http://nvd3.com/ghpages/multiBar.html]

    # Highcharts
    [http://www.highcharts.com/demo/bar-basic]
    '''

    name = 'multiBarChart'

    x_tickFormat = 'd'
    y_tickFormat = '.1f'



class MultiBarHorizontalChart(Chart):
    '''
    # NVD3
    Horizontal Grouped / Stacked Bar Chart
    [http://nvd3.com/ghpages/multiBarHorizontal.html]

    # Highcharts
    [http://www.highcharts.com/demo/bar-negative-stack]
    '''

    name = 'multiBarHorizontalChart'

    x = 'd.label'
    y = 'd.value'
    y_tickFormat = ',.2f'
    margin = {'top': 30, 'right': 20, 'bottom': 50, 'left': 175}
    showValues = 'true'
    showControls = 'true'
    tooltips = 'false'



class DiscreteBarChart(Chart):
    '''
    # NVD3
    Discrete Bar Chart
    [http://nvd3.com/ghpages/discreteBar.html]

    # Highcharts
    [http://www.highcharts.com/demo/column-negative]
    '''
    
    name = 'discreteBarChart'

    x = 'd.label'
    y = 'd.value'
    staggerLabels = 'true'
    tooltips = 'false'
    showValues = 'true'



class CumulativeLineChart(Chart):
    '''
    # NVD3
    Cumulative Line Chart
    [http://nvd3.com/ghpages/cumulativeLine.html]

    # Highcharts
    [http://www.highcharts.com/demo/spline-plot-bands]
    '''

    name = 'cumulativeLineChart'

    x = 'd[0]'
    y = 'd[1]/100'
    x_tickFormat = 'date'
    y_tickFormat = '.1%'
    color_category = 'category10'



    
# ==============================================================================
#  Useless
# ==============================================================================
class LineChart(Chart):
    '''
    [http://nvd3.com/ghpages/line.html]
    '''
    name = 'lineChart'
    x_axisLabel = ''
    y_axisLabel = ''
    x_tickFormat = ',r'
    y_tickFormat = '.02f'


class LinePlusBarChart(Chart):
    '''
    [http://nvd3.com/ghpages/linePlusBar.html]
    '''
    name = 'linePlusBarChart'
    margin = {'top': 30, 'right': 60, 'bottom': 50, 'left': 70}
    x = "function(d,i) { return i }"
    x_tickFormat = """function(d) {
         var dx = testdata[0].values[d] && testdata[0].values[d].x || 0;
         return d3.time.format('%x')(new Date(dx)"""
    y1_tickFormat = ',f'
    y2_tickFormat = "function(d) { return '$' + d3.format(',f')(d) }"
    # chart.bars.forceY([0]);


class LineWithFisheyeChart(Chart):
    pass


class LineWithFocusChart(Chart):
    '''
    [http://nvd3.com/ghpages/lineWithFocus.html]
    '''
    name = 'lineWithFocusChart'
    x_tickFormat = ',f'
    y_tickFormat = ',.2f'
    y2_tickFormat = ',.2f'


class MultiChart(Chart):
    pass


class BulletChart(Chart):
    '''
    [http://nvd3.com/ghpages/bullet.html]
    '''
    name = 'bulletChart'
    duration = 1000
    pass

class ScatterChart(Chart):
    '''
    [http://nvd3.com/ghpages/scatter.html]
    '''
    name = 'scatterChart'
    showDistX = 'true'
    showDistY = 'true'
    x_tickFormat = '.02f'
    y_tickFormat = '.02f'




    
