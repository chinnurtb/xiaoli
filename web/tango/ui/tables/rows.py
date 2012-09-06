#coding=utf-8

from __future__ import absolute_import, unicode_literals


from .utils import *

__dict__ = ['BoundRow', 'BoundRows']

## Row
class BoundRow(object):
    def __init__(self, table, record):
        self._table = table
        self._record = record

    @property
    def table(self):
        return self._table

    @property
    def record(self):
        return self._record

    def show_items(self):
        print len(self.table.columns), ' <--> ', self.table.columns
        for column in self.table.columns:
            print 'show item::', column, '\n', self[column.name], '::'

    @property
    def items(self):
        for column in self.table.columns:
            yield (column, self[column.name])


    def __getitem__(self, name):
        bound_column = self.table.columns[name]
        #value = getattr(self.record, name, '')
        value = None
        try:
            value = bound_column.accessor.resolve(self.record)
        except ValueError:
            pass

        kwargs = {
            'value':            lambda: value,
            'record':           lambda: self._record,
            'bound_column':     lambda: bound_column,
            'bound_row':        lambda: self,
            'table':            lambda: self._table,
            }
        kw = {}
        for arg_name in bound_column._render_args:
            kw[arg_name] = kwargs[arg_name]()
        return bound_column.render(**kw)


class BoundRows(object):

    def __init__(self, data):
        self.data = data

    def __iter__(self):
        table = self.data.table
        for record in self.data:
            yield BoundRow(table, record)

    def __len__(self):
        return len(self.data)

    def __getitem__(self, key):
        if isinstance(key, slice):
            return BoundRows(self.data[key])
        else:
            return BoundRow(self.data.table, self.data[key])

