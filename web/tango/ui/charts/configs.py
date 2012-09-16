#!/usr/bin/env python
# -*- coding: utf-8 -*-

from demjson import decode

# ==============================================================================
#  HighCharts default configurations
# ==============================================================================

area_stacked = '''{
   chart: {
       renderTo: 'container',
       type: 'area'
   },
   title: {
       text: 'Historic and Estimated Worldwide Population Growth by Region'
   },
   subtitle: {
       text: 'Source: Wikipedia.org'
   },
   xAxis: {
       categories: ['1750', '1800', '1850', '1900', '1950', '1999', '2050'],
       tickmarkPlacement: 'on',
       title: {
           enabled: false
       }
   },
   yAxis: {
       title: {
           text: 'Billions'
       },
       labels: {
           formatter: "function() {return this.value / 1000;}"
       }
   },
   tooltip: {
       formatter: "function() {return ''+ this.x +': '+ Highcharts.numberFormat(this.y, 0, ',') +' millions';}"
   },
   plotOptions: {
       area: {
           stacking: 'normal',
           lineColor: '#666666',
           lineWidth: 1,
           marker: {
               lineWidth: 1,
               lineColor: '#666666'
           }
       }
   },
   series: [{
       name: 'Asia',
       data: [502, 635, 809, 947, 1402, 3634, 5268]
   }, {
       name: 'Africa',
       data: [106, 107, 111, 133, 221, 767, 1766]
   }, {
       name: 'Europe',
       data: [163, 203, 276, 408, 547, 729, 628]
   }, {
       name: 'America',
       data: [18, 31, 54, 156, 339, 818, 1201]
   }, {
       name: 'Oceania',
       data: [2, 2, 2, 6, 13, 30, 46]
   }]
}
'''

pie_basic = '''{
   chart: {
       renderTo: 'container',
       plotBackgroundColor: null,
       plotBorderWidth: null,
       plotShadow: false
   },
   title: {
       text: 'Browser market shares at a specific website, 2010'
   },
   tooltip: {
       pointFormat: '{series.name}: <b>{point.percentage}%</b>',
       percentageDecimals: 1
   },
   plotOptions: {
       pie: {
           allowPointSelect: true,
           cursor: 'pointer',
           dataLabels: {
               enabled: true,
               color: '#000000',
               connectorColor: '#000000',
               formatter: "function() {return '<b>'+ this.point.name +'</b>: '+ this.percentage +' %';}"
           }
       }
   },
   series: [{
       type: 'pie',
       name: 'Browser share',
       data: [
           ['Firefox',   45.0],
           ['IE',       26.8],
           {
               name: 'Chrome',
               y: 12.8,
               sliced: true,
               selected: true
           },
           ['Safari',    8.5],
           ['Opera',     6.2],
           ['Others',   0.7]
       ]
   }]
}
'''

spline_plot_bands = '''{
    chart: {
        renderTo: 'container',
        type: 'spline'
    },
    title: {
        text: 'Wind speed during two days'
    },
    subtitle: {
        text: 'October 6th and 7th 2009 at two locations in Vik i Sogn, Norway'
    },
    xAxis: {
        type: 'datetime'
    },
    yAxis: {
        title: {
            text: 'Wind speed (m/s)'
        },
        min: 0,
        minorGridLineWidth: 0,
        gridLineWidth: 0,
        alternateGridColor: null,
        plotBands: [{ // Light air
            from: 0.3,
            to: 1.5,
            color: 'rgba(68, 170, 213, 0.1)',
            label: {
                text: 'Light air',
                style: {
                    color: '#606060'
                }
            }
        }, { // Light breeze
            from: 1.5,
            to: 3.3,
            color: 'rgba(0, 0, 0, 0)',
            label: {
                text: 'Light breeze',
                style: {
                    color: '#606060'
                }
            }
        }, { // Gentle breeze
            from: 3.3,
            to: 5.5,
            color: 'rgba(68, 170, 213, 0.1)',
            label: {
                text: 'Gentle breeze',
                style: {
                    color: '#606060'
                }
            }
        }, { // Moderate breeze
            from: 5.5,
            to: 8,
            color: 'rgba(0, 0, 0, 0)',
            label: {
                text: 'Moderate breeze',
                style: {
                    color: '#606060'
                }
            }
        }, { // Fresh breeze
            from: 8,
            to: 11,
            color: 'rgba(68, 170, 213, 0.1)',
            label: {
                text: 'Fresh breeze',
                style: {
                    color: '#606060'
                }
            }
        }, { // Strong breeze
            from: 11,
            to: 14,
            color: 'rgba(0, 0, 0, 0)',
            label: {
                text: 'Strong breeze',
                style: {
                    color: '#606060'
                }
            }
        }, { // High wind
            from: 14,
            to: 15,
            color: 'rgba(68, 170, 213, 0.1)',
            label: {
                text: 'High wind',
                style: {
                    color: '#606060'
                }
            }
        }]
    },
    tooltip: {
        formatter: "function() {return ''+ Highcharts.dateFormat('%e. %b %Y, %H:00', this.x) +': '+ this.y +' m/s';}"
    },
    plotOptions: {
        spline: {
            lineWidth: 4,
            states: {
                hover: {
                    lineWidth: 5
                }
            },
            marker: {
                enabled: false,
                states: {
                    hover: {
                        enabled: true,
                        symbol: 'circle',
                        radius: 5,
                        lineWidth: 1
                    }
                }
            },
            pointInterval: 3600000, // one hour
            pointStart: 1254787200000 //Date.UTC(2009, 9, 6, 0, 0, 0) [Edit by weet]
                                      // Equal to >>  time.mktime(now.timetuple()) * 1000
        }
    },
    series: [{
        name: 'Hestavollane',
        data: [4.3, 5.1, 4.3, 5.2, 5.4, 4.7, 3.5, 4.1, 5.6, 7.4, 6.9, 7.1,
            7.9, 7.9, 7.5, 6.7, 7.7, 7.7, 7.4, 7.0, 7.1, 5.8, 5.9, 7.4,
            8.2, 8.5, 9.4, 8.1, 10.9, 10.4, 10.9, 12.4, 12.1, 9.5, 7.5,
            7.1, 7.5, 8.1, 6.8, 3.4, 2.1, 1.9, 2.8, 2.9, 1.3, 4.4, 4.2,
            3.0, 3.0]

    }, {
        name: 'Voll',
        data: [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.0, 0.3, 0.0,
            0.0, 0.4, 0.0, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.6, 1.2, 1.7, 0.7, 2.9, 4.1, 2.6, 3.7, 3.9, 1.7, 2.3,
            3.0, 3.3, 4.8, 5.0, 4.8, 5.0, 3.2, 2.0, 0.9, 0.4, 0.3, 0.5, 0.4]
    }]
    ,
    navigation: {
        menuItemStyle: {
            fontSize: '10px'
        }
    }
}
'''

bar_basic = '''{
   chart: {
       renderTo: 'container',
       type: 'bar'
   },
   title: {
       text: 'Historic World Population by Region'
   },
   subtitle: {
       text: 'Source: Wikipedia.org'
   },
   xAxis: {
       categories: ['Africa', 'America', 'Asia', 'Europe', 'Oceania'],
       title: {
           text: null
       }
   },
   yAxis: {
       min: 0,
       title: {
           text: 'Population (millions)',
           align: 'high'
       },
       labels: {
           overflow: 'justify'
       }
   },
   tooltip: {
       formatter: "function() {return ''+ this.series.name +': '+ this.y +' millions';}"
   },
   plotOptions: {
       bar: {
           dataLabels: {
               enabled: true
           }
       }
   },
   legend: {
       layout: 'vertical',
       align: 'right',
       verticalAlign: 'top',
       x: -100,
       y: 100,
       floating: true,
       borderWidth: 1,
       backgroundColor: '#FFFFFF',
       shadow: true
   },
   credits: {
       enabled: false
   },
   series: [{
       name: 'Year 1800',
       data: [107, 31, 635, 203, 2]
   }, {
       name: 'Year 1900',
       data: [133, 156, 947, 408, 6]
   }, {
       name: 'Year 2008',
       data: [973, 914, 4054, 732, 34]
   }]
}
'''

bar_negative_stack = '''{
    chart: {
        renderTo: 'container',
        type: 'bar'
    },
    title: {
        text: 'Population pyramid for Germany, midyear 2010'
    },
    subtitle: {
        text: 'Source: www.census.gov'
    },
    xAxis: [{
        categories: ['0-4', '5-9', '10-14', '15-19',
    '20-24', '25-29', '30-34', '35-39', '40-44',
    '45-49', '50-54', '55-59', '60-64', '65-69',
    '70-74', '75-79', '80-84', '85-89', '90-94',
    '95-99', '100 +'],
        reversed: false
    }, { // mirror axis on right side
        opposite: true,
        reversed: false,
        categories: ['0-4', '5-9', '10-14', '15-19',
    '20-24', '25-29', '30-34', '35-39', '40-44',
    '45-49', '50-54', '55-59', '60-64', '65-69',
    '70-74', '75-79', '80-84', '85-89', '90-94',
    '95-99', '100 +'],
        linkedTo: 0
    }],
    yAxis: {
        title: {
            text: null
        },
        labels: {
            formatter: "function() {return (Math.abs(this.value) / 1000000) + 'M';}"
        },
        min: -4000000,
        max: 4000000
    },

    plotOptions: {
        series: {
            stacking: 'normal'
        }
    },

    tooltip: {
        formatter: "function() {return '<b>'+ this.series.name +', age '+ this.point.category +'</b><br/>'+ 'Population: '+ Highcharts.numberFormat(Math.abs(this.point.y), 0);}"
    },

    series: [{
        name: 'Male',
        data: [-1746181, -1884428, -2089758, -2222362, -2537431, -2507081, -2443179,
            -2664537, -3556505, -3680231, -3143062, -2721122, -2229181, -2227768,
            -2176300, -1329968, -836804, -354784, -90569, -28367, -3878]
    }, {
        name: 'Female',
        data: [1656154, 1787564, 1981671, 2108575, 2403438, 2366003, 2301402, 2519874,
            3360596, 3493473, 3050775, 2759560, 2304444, 2426504, 2568938, 1785638,
            1447162, 1005011, 330870, 130632, 21208]
    }]
}
'''

column_rotated_labels = '''{
   chart: {
       renderTo: 'container',
       type: 'column',
       margin: [ 50, 50, 100, 80]
   },
   title: {
       text: "World's largest cities per 2008"
   },
   xAxis: {
       categories: [
           'Tokyo',
           'Jakarta',
           'New York',
           'Seoul',
           'Manila',
           'Mumbai',
           'Sao Paulo',
           'Mexico City',
           'Dehli',
           'Osaka',
           'Cairo',
           'Kolkata',
           'Los Angeles',
           'Shanghai',
           'Moscow',
           'Beijing',
           'Buenos Aires',
           'Guangzhou',
           'Shenzhen',
           'Istanbul'
       ],
       labels: {
           rotation: -45,
           align: 'right',
           style: {
               fontSize: '13px',
               fontFamily: 'Verdana, sans-serif'
           }
       }
   },
   yAxis: {
       min: 0,
       title: {
           text: 'Population (millions)'
       }
   },
   legend: {
       enabled: false
   },
   tooltip: {
       formatter: "function() {return '<b>'+ this.x +'</b><br/>'+ 'Population in 2008: '+ Highcharts.numberFormat(this.y, 1) + ' millions';}"
   },
   series: [{
   name: 'Population',
   data: [34.4, 21.8, 20.1, 20, 19.6, 19.5, 19.1, 18.4, 18,
       17.3, 16.8, 15, 14.7, 14.5, 13.3, 12.8, 12.4, 11.8,
       11.7, 11.2],
   dataLabels: {
       enabled: true,
       rotation: -90,
       color: '#FFFFFF',
       align: 'right',
       x: -3,
       y: 10,
       formatter: "function() {return this.y;}",
       style: {
           fontSize: '13px',
           fontFamily: 'Verdana, sans-serif'
       }
   }}]
}
 '''


column_negative = '''{
   chart: {
       renderTo: 'container',
       type: 'column'
   },
   title: {
       text: 'Column chart with negative values'
   },
   xAxis: {
       categories: ['Apples', 'Oranges', 'Pears', 'Grapes', 'Bananas']
   },
   tooltip: {
       formatter: "function() {return ''+ this.series.name +': '+ this.y +'';}"
   },
   credits: {
       enabled: false
   },
   series: [{
       name: 'John',
       data: [5, 3, 4, 7, 2]
   }, {
       name: 'Jane',
       data: [2, -2, -3, 2, 1]
   }, {
       name: 'Joe',
       data: [3, 4, 4, -2, 5]
   }]
}
'''

area_stacked = decode(area_stacked)
pie_basic = decode(pie_basic)
spline_plot_bands = decode(spline_plot_bands)
bar_basic = decode(bar_basic)
bar_negative_stack = decode(bar_negative_stack)
column_rotated_labels = decode(column_rotated_labels)
column_negative = decode(column_negative)

    
formatters = ["function() {return this.value / 1000;}",
              "function() {return ''+ this.x +': '+ Highcharts.numberFormat(this.y, 0, ',') +' millions';}",
              "function() {return '<b>'+ this.point.name +'</b>: '+ this.percentage +' %';}",
              "function() {return ''+ Highcharts.dateFormat('%e. %b %Y, %H:00', this.x) +': '+ this.y +' m/s';}",
              "function() {return ''+ this.series.name +': '+ this.y +' millions';}",
              "function() {return (Math.abs(this.value) / 1000000) + 'M';}",
              "function() {return '<b>'+ this.series.name +', age '+ this.point.category +'</b><br/>'+ 'Population: '+ Highcharts.numberFormat(Math.abs(this.point.y), 0);}",
              "function() {return ''+ this.series.name +': '+ this.y +'';}"]

if __name__ == '__main__':
    print type(spline_plot_bands)
