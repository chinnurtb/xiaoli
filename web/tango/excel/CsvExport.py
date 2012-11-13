#!/usr/bin/env python
# coding: utf-8

import os
import csv
from datetime import datetime
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO

from .export_config import cn

def attrgetter(format, *items):
    if len(items) == 1:
        attr = items[0]
        def g(obj):
            return resolve_attr(obj, attr, format)
    else:
        def g(obj):
            return tuple(resolve_attr(obj, attr, format) for attr in items)
    return g

def resolve_attr(obj, attr, format):
    format_call = format.get(attr)
    for name in attr.split("."):
        obj = getattr(obj, name, '')
    if format_call and callable(format_call):
        obj = format_call(obj)
    return obj.encode('gbk') if isinstance(obj, basestring) else obj

class CsvExport(object):

    # model: 对应cn.py里的keys
    # columns: 只写入哪些列，同时cn.py里中文名称的key
    def __init__(self, model='', columns=[]):
        self.model = model
        self.columns = columns
        model_dict = cn.get(model, {})
        self.title = [model_dict.get(col, col).encode('gbk') for col in columns]  # 根据cn.py中的字典获取Excel title 信息

    # query: SQLAlchemy的query对象
    # is_save:  是否保存文件
    # format: 格式化字典, key 为columns里的元素，value 为格式化函数，函数参数此列的值
    def export(self, query, is_save=False, format={}):
        return_fileobj = None
        if type(query).__name__ != 'BaseQuery' and type(query).__name__ != 'Query':
            raise TypeError('The query argument should be sqlalchemy query object.')
        if not is_save:
            return_fileobj = StringIO()
            writer = csv.writer(return_fileobj)
        else:
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','static','file','download')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file = os.path.join(root_path,self.model+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            f = open(file,'wb')
            writer = csv.writer(f)

        writer.writerow(self.title)
        for record in list(query):
            f = attrgetter(format, *self.columns)
            row_value = f(record)
            writer.writerow(row_value)

        if return_fileobj:
            return_fileobj.seek(0)
            return return_fileobj
        else:
            return file

