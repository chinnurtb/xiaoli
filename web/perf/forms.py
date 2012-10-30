#coding=utf-8


from wtforms.ext.sqlalchemy.fields import QuerySelectField
from flask_wtf import Form, TextField, SelectField, IntegerField, TextAreaField, required

from .constants import DATES

from nodes.models import Vendor, Model

class PerfFilterForm(Form):
    
    sampletime  = SelectField(u'时间', choices=DATES)
    intervals   = SelectField(u'时段', choices=[('all', u'全部时段')]+[(str(i), str(i)+u"点") for i in range(24)])
    vendors     = QuerySelectField(u'厂商', query_factory=lambda: Vendor.query, get_label='alias', allow_blank=True, blank_text=u"全部厂商")
    models      = QuerySelectField(u'型号', query_factory=lambda: Model.query,
get_label='alias', allow_blank=True, blank_text=u"全部型号")


    
