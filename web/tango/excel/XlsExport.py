#!/usr/bin/env python
# coding: utf-8

import os
import xlwt
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
    return obj

def get_datetime_style():
    style = xlwt.XFStyle()
    style.num_format_str = 'yyyy-MM-dd hh:mm:ss'
    return style

class XlsExport(object):

    # model: 对应cn.py里的keys
    # columns: 只写入哪些列，同时cn.py里中文名称的key
    def __init__(self, model='', columns=[]):
        self.model = model
        self.columns = columns
        model_dict = cn.get(model, {})
        self.title = [model_dict.get(col, col) for col in columns]  # 根据cn.py中的字典获取Excel title 信息

    # query: SQLAlchemy的query对象
    # is_save:  是否保存文件
    # format: 格式化字典, key 为columns里的元素，value 为格式化函数，函数参数此列的值
    def export(self, query, is_save=False, format={}):
        return_fileobj = None
        if type(query).__name__ != 'BaseQuery' and type(query).__name__ != 'Query':
            raise TypeError('The query argument should be sqlalchemy query object.')

        book = xlwt.Workbook()
        datetime_style = get_datetime_style()
        results = query.all()
        sheet_size = 60000  # 每个sheet页只能写入6万行记录，多于6万行，新建sheet
        sheet_total = len(results)/sheet_size if len(results) % sheet_size == 0 else len(results)/sheet_size + 1
        f = attrgetter(format, *self.columns)
        if not sheet_total:
            sheet_name = self.model
            sheet = book.add_sheet(sheet_name)
            for col,col_name in enumerate(self.title):
                sheet.row(0).write(col, col_name)
        else:
            for i in range(sheet_total):
                sheet_results = results[i*sheet_size:(i+1)*sheet_size]
                sheet_name = self.model if i is 0 else self.model+str(i+1)
                sheet = book.add_sheet(sheet_name)
                for col,col_name in enumerate(self.title):
                    sheet.row(0).write(col, col_name)
                for row,record in enumerate(sheet_results):
                    row_value = f(record)
                    for col,data in enumerate(row_value):
                        if isinstance(data, datetime):
                            sheet.row(row+1).write(col,data,datetime_style)
                        else:
                            sheet.row(row+1).write(col,data)

        if not is_save:
            return_fileobj = StringIO()
            book.save(return_fileobj)
        else:
            root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','static','file','download')
            if not os.path.isdir(root_path): os.mkdir(root_path)
            file = os.path.join(root_path,self.model+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
            book.save(file)

        if return_fileobj:
            return_fileobj.seek(0)
            return return_fileobj
        else:
            return file

