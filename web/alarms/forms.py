# -*- coding: utf-8 -*-

from flask_wtf import (Form, TextField, SubmitField, PasswordField, RadioField,
                          SelectMultipleField, SelectField, HiddenField, DateField,
                          IntegerField, TextAreaField, SubmitField, RecaptchaField,
                          ValidationError, validators, required, equal_to, email)

from wtforms import BooleanField 

class QueryNewForm(Form):

    name        = TextField(u'名称', validators=[required(message=u'必填')])
    is_public   = BooleanField()


