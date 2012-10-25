#coding=utf-8


from wtforms.ext.sqlalchemy.fields import QuerySelectField
from flask_wtf import Form, TextField, SelectField

from tango.models import Category
from alarms.models import AlarmSeverity, AlarmClass
from .models import Metric


occur_conds = []
restore_conds = []
for i in range(90, 10, -10):
    occur_conds.append((u'>%d%%' % i, u'> %d%%' % i))
    restore_conds.append((u'<%d%%' % (i-10, ), u'< %d%%' % (i-10, )))
    

class ThresholdForm(Form):
    name        = TextField(u'名称')
    category    = QuerySelectField(u'设备', query_factory=lambda:Category.query, get_label='alias')
    metric      = QuerySelectField(u'指标', query_factory=lambda:Metric.query, get_label='alias')
    alarm_class = QuerySelectField(u'告警类型', query_factory=lambda:AlarmClass.query, get_label='alias')
    
    occur_cond1   = TextField(u'产生条件')
    restore_cond1 = TextField(u'恢复条件')
    severity1     = QuerySelectField(u'产生告警', query_factory=lambda:AlarmSeverity.query, get_label='alias')

    occur_cond2   = TextField(u'产生条件')
    restore_cond2 = TextField(u'恢复条件')
    severity2     = QuerySelectField(u'产生告警', query_factory=lambda:AlarmSeverity.query, get_label='alias')
