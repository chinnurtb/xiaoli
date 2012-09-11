# -*- coding: utf-8 -*-

from flask_wtf import (Form, TextField, SubmitField, PasswordField, RadioField,
                          SelectMultipleField, SelectField, HiddenField, DateField,
                          IntegerField, TextAreaField, SubmitField, RecaptchaField,
                          ValidationError, validators, required, equal_to, email)

from wtforms import BooleanField 

class QueryNewForm(Form):

    name        = TextField(u'名称', validators=[required(message=u'必填')])
    is_public   = BooleanField()

class AlarmAckForm(Form):
    id              = HiddenField(u'Id')
    acked_note        = TextAreaField(u'Note', validators=[required(message=u'必填')])

class AlarmClearForm(Form):
    id              = HiddenField(u'Id')
    cleared_note    = TextAreaField(u'Note', validators=[required(message=u'必填')])
 
