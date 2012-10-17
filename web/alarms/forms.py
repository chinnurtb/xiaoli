# -*- coding: utf-8 -*-

from flask_wtf import (Form, TextField, SubmitField, PasswordField, RadioField,
                          SelectMultipleField, SelectField, HiddenField, DateField, DateTimeField ,
                          IntegerField, TextAreaField, SubmitField, RecaptchaField,
                          ValidationError, validators, required, equal_to, email)

from wtforms import BooleanField

from wtforms.ext.sqlalchemy.fields import QuerySelectField

from tango.form.fields import SelectFieldPro

from .models import AlarmSeverity, AlarmClass

class QueryNewForm(Form):

    tab         = HiddenField()
    name        = TextField(u'名称', validators=[required(message=u'必填')])
    is_public   = BooleanField()

class AlarmAckForm(Form):
    id              = HiddenField(u'Id')
    acked_note        = TextAreaField(u'Note', validators=[required(message=u'必填')])

class AlarmClearForm(Form):
    id              = HiddenField(u'Id')
    cleared_note    = TextAreaField(u'Note', validators=[required(message=u'必填')])
 
class AlarmClassForm(Form):
    id              = HiddenField(u'Id')
    severity        = SelectFieldPro(u'级别', validators=[required(message=u'必填')], choices=lambda: [(unicode(r.id), r.alias) for r in AlarmSeverity.query])
    #x733_type       = tables.Column(verbose_name=u'X733类型')
    probablecause   = TextAreaField(u'可能原因')
    specific_problem = TextAreaField(u'特定原因')
    additionalinfo   = TextAreaField(u'附加信息')
    remark           = TextAreaField(u'备注')

class AlarmKnowledgeForm(Form):
    #id              = HiddenField(u'Id')
    class_id        = SelectFieldPro(u'AlarmClass', validators=[required(message=u'必填')], choices=lambda: [('', u'Choice')] + [(unicode(r.id), r.alias) for r in AlarmClass.query])
    probability     = SelectField(u'probability', validators=[required(message=u'必填')], choices=[(unicode(1), u'极少发生'), (unicode(2), u'偶尔发生'), (unicode(3), u'频繁发生')])
    probable_cause  = TextAreaField(u'可能原因') 
    resolvent       = TextAreaField(u'Jiejuefangan')

class AlarmFilterForm(Form):
    alarm_class = QuerySelectField(query_factory=lambda: AlarmClass.query, get_label='alias', allow_blank=True, blank_text=u'全部告警')
    start_date  = DateTimeField(u'开始时间', format='%Y-%m-%d')
    end_date    = DateTimeField(u'结束时间', format='%Y-%m-%d')
    keyword     = TextField()
    
