# -*- coding: utf-8 -*-

from flask_wtf import (Form, TextField, SubmitField, PasswordField, RadioField,
                          SelectMultipleField, SelectField, HiddenField, DateField,
                          IntegerField, TextAreaField, SubmitField, RecaptchaField,
                          ValidationError, validators, required, equal_to, email)

from wtforms import BooleanField 

from tango.forms import SelectFieldPro

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
    probablecause   = TextAreaField(verbose_name=u'可能原因')
    specific_problem = TextAreaField(verbose_name=u'特定原因')
    additionalinfo   = TextAreaField(verbose_name=u'附加信息')
    remark           = TextAreaField(verbose_name=u'备注')

