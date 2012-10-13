# coding: utf-8

from flask_wtf import Form, TextField, SubmitField

class SearchForm(Form):

    keyword = TextField()
    
    submit  = SubmitField(u'检索', id = 'search-submit')

class FilterForm(Form):

    pass
    
