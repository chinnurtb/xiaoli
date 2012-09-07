# -*- coding: utf-8 -*-

DEBUG = True

SECRET_KEY = 'public'

SESSION_COOKIE_NAME = 'xiaoli'

SQLALCHEMY_DATABASE_URI = 'postgresql://ipon:public@localhost/ipon'

#SQLALCHEMY_BINDS = {
#    'report': 'sqlite://'
#}

SQLALCHEMY_POOL_SIZE = 5

SQLALCHEMY_POOL_TIMEOUT = 10

try:
    from local_settings import *
except Exception:
    pass

