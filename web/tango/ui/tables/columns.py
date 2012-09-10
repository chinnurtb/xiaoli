#coding=utf-8
from __future__ import absolute_import, unicode_literals

from itertools import  islice
import datetime

from jinja2 import Markup
from jinja2.filters import escape
from flask import url_for

from .utils import *

__dict__ = ['Column', 'ManageColumn', 'CheckBoxColumn', 'BaseLinkColumn', 'LinkColumn',
            'EmailColumn', 'DateTimeColumn',
            'BoundColumn', 'BoundColumns']

## Column
class Column(object):
    creation_counter = 0

    def __init__(self, attrs=None, accessor=None,
                 verbose_name=None, orderable=None):
        if not (accessor is None or isinstance(accessor, basestring) or
                callable(accessor)):
            raise TypeError('accessor must be a string or callable, not %s' %
                            type(accessor).__name__)
        if callable(accessor) and default is not None:
            raise TypeError('accessor must be string when default is used, not callable')
        self.accessor = A(accessor) if accessor else None

        self.verbose_name = verbose_name
        self.orderable = orderable

        default_attrs = Attrs(th={'class': 'manage-column', 'scope': 'col'})
        attrs = attrs or Attrs()
        if not isinstance(attrs, Attrs):
            warnings.warn('attrs must be Attrs object, not %s'
                          % type(attrs).__name__, DeprecationWarning)
            attrs = Attrs(attrs)
        default_attrs.update(attrs)
        self.attrs = default_attrs

        self.creation_counter = Column.creation_counter
        Column.creation_counter += 1
        # print 'Column.creation_counter', Column.creation_counter

    
    def cellattrs(self, bound_column, record):
        return {}

    def render(self, value):
        return value


class EnumColumn(Column):

    def __init__(self, name, enums=None, attrs=None, **extra):
        super(EnumColumn, self).__init__(attrs=attrs, **extra)
        self.name = name
        self.enums = enums

    def cellattrs(self, bound_column, record):
        return {'class': '%s-%s' % (self.name, bound_column.accessor.resolve(record))}

    def render(self, value, record, bound_column):
        if value in self.enums:
            return self.enums[value]
        return value

class CheckBoxColumn(Column):

    def __init__(self, attrs=None, **extra):
        attrs = attrs or Attrs()
        if not isinstance(attrs, Attrs):
            warnings.warn('attrs must be Attrs object, not %s'
                          % type(attrs).__name__, DeprecationWarning)
            attrs = Attrs(td__input=attrs)

        kwargs = {b'orderable': False, b'attrs': attrs}
        kwargs.update(extra)
        super(CheckBoxColumn, self).__init__(**kwargs)

        
    @property
    def header(self):
        default = {'type': 'checkbox'}
        general = self.attrs.get('input')
        specific = self.attrs.get('th__input')
        attrs = AttributeDict(default, **(specific or general or {}))
        return Markup(u'<input %s/>' % attrs.as_html())


    def render(self, record, bound_column):  # pylint: disable=W0221
        default = {
            'type': 'checkbox',
            'name': 'id',
            'value': record.id
        }
        general = self.attrs.get('input')
        specific = self.attrs.get('td__input')
        attrs = AttributeDict(default, **(specific or general or {}))
        return Markup(u'<input %s/>' % attrs.as_html())


class BaseLinkColumn(Column):

    def __init__(self, attrs=None, *args, **kwargs):
        attrs = attrs or Attrs()
        if not isinstance(attrs, Attrs):
            warnings.warn('attrs must be Attrs object, not %s'
                          % type(attrs).__name__, DeprecationWarning)
            attrs = Attrs(a=attrs)
        kwargs[b'attrs'] = attrs
        super(BaseLinkColumn, self).__init__(*args, **kwargs)

    def render_link(self, uri, text, attrs=None):
        attrs = AttributeDict(attrs if attrs is not None else
                              self.attrs.get('a', {}))
        html = u'<a href="{uri}"{attrs}>{text}</a>'.format(
            uri=escape(uri),
            attrs=" %s" % attrs.as_html() if attrs else "",
            text=escape(text)
        )
        return Markup(html)


class LinkColumn(BaseLinkColumn):

    def __init__(self, endpoint=None, values=None, _external=None,
                 attrs=None, **extra):
        super(LinkColumn, self).__init__(attrs, **extra)
        self.endpoint = endpoint
        self.values = values
        self._external = _external


    def render(self, value, record, bound_column):
        if not self.endpoint and not bound_column.url_maker:
            raise ValueError('An *endpoint* or *url_maker* is required')

        params = {}
        if self.endpoint:
            params[b'endpoint'] = self.endpoint

        record_id = getattr(record, 'id', None)
        if record_id:
            params['id'] = record_id

        if self.values:
            for key, val in self.values.items():
                params[key] = val

        if self._external:
            params[b'_external'] = self._external

        uri = None
        if bound_column.url_maker:
            uri = bound_column.url_maker(record)
        else:
            uri = url_for(**params)
        return self.render_link(uri, value)

class EmailColumn(BaseLinkColumn):
    def render(self, value):
        return self.render_link("mailto:%s" % value, value)


class ButtonColumn(BaseLinkColumn):
    def __init__(self, endpoint=None, icon_type=None, attrs=None, **extra):
        if endpoint is None:
            raise ValueError('An endpoint must be given')
        self.endpoint = endpoint

        default_attrs = Attrs(th={'style': 'width:2.0em;'})

        attrs = attrs or Attrs()
        if not isinstance(attrs, Attrs):
            warnings.warn('attrs must be Attrs object, not %s'
                          % type(attrs).__name__, DeprecationWarning)
            attrs = Attrs(a=attrs)
        attrs.update(default_attrs)

        danger_btns = ('trash', 'remove')
        self.icon_type = icon_type
        self.btn_class = 'btn btn-mini'
        self.icon_class = 'icon-' + self.icon_type
        if self.icon_type in danger_btns:
            self.btn_class += ' btn-danger'
            self.icon_class += ' icon-white'

        super(ButtonColumn, self).__init__(attrs, **extra)


    @property
    def header(self):
        return u''

    def render(self, value, record, bound_column):
        uri = url_for(self.endpoint, id=getattr(record, 'id', None))
        attrs = AttributeDict({'class': self.btn_class})
        content = Markup(u'<i class="%s"></i>' % self.icon_class)
        return self.render_link(uri, content, attrs)


class EditBtnColumn(ButtonColumn):
    def __init__(self, endpoint=None, attrs=None, **extra):
        super(EditBtnColumn, self).__init__(endpoint, icon_type='pencil', **extra)


class DeleteBtnColumn(ButtonColumn):
    def __init__(self, endpoint=None, attrs=None, **extra):
        super(DeleteBtnColumn, self).__init__(endpoint, icon_type='remove', **extra)
        self.btn_class += ' btn-danger'
        self.icon_class += ' icon-white'


class DateTimeColumn(Column):
    def __init__(self, format=None, **extra):
        super(DateTimeColumn, self).__init__(**extra)
        self.format = format

    def render(self, value):
        if not isinstance(value, datetime.datetime):
            raise TypeError('The value must a *datetime.datetime* type')
        return value.strftime(self.format)


class BoundColumn(object):
    def __init__(self, table, column, name):
        self.table = table
        self.column = column
        self.name = name
        self.is_checkbox = True if isinstance(column, CheckBoxColumn) else False
        self.is_button = True if isinstance(column, ButtonColumn) else False

        url_makers = getattr(table._meta, 'url_makers', None)
        self.url_maker = url_makers.get(name, None) if url_makers else None

        

    @property
    def header(self):
        # 通过 column_header = self.column.header 来获取表头的值
        # 会导致意想不到的错误
        column_header = getattr(self.column, 'header', None)
        if column_header is not None:
            return column_header

        verbose_name = self.verbose_name
        return Markup(title(verbose_name))


    @property
    def accessor(self):
        return self.column.accessor or A(self.name)


    @property
    def verbose_name(self):
        if self.column.verbose_name:
            return self.column.verbose_name

        name = self.name.replace('_', ' ')
        return name

    @property
    def attrs(self):
        # Work on a copy of the Attrs object since we're tweaking stuff
        attrs = dict(self.column.attrs)

        # Find the relevant th attributes (fall back to cell if th isn't
        # explicitly specified).
        attrs["td"] = td = AttributeDict(attrs.get('td', attrs.get('cell', {})))
        attrs["th"] = th = AttributeDict(attrs.get("th", attrs.get("cell", {})))
        # make set of existing classes.
        th_class = set((c for c in th.get("class", "").split(" ") if c))
        td_class = set((c for c in td.get("class", "").split(" ") if c))
        # add classes for ordering
        if getattr(self, 'hidden', None):
            th["style"] = "display: none;"
            td["style"] = "display: none;"
        if self.orderable:
            th_class.add("orderable")
            th_class.add("sortable")

        order_by = getattr(self.table, 'order_by', None)
        if order_by and (order_by == self.name
                         or order_by[1:] == self.name):
            th_class.add('desc' if order_by[0] == '-' else 'asc')
        # Always add the column name as a class
        th_class.add(self.name + '-column')
        td_class.add(self.name + '-column')

        if th_class:
            th['class'] = " ".join(sorted(th_class))
        if td_class:
            td['class'] = " ".join(sorted(td_class))
        return attrs


    @property
    def orderable(self):
        return self.column.orderable


class BoundColumns(object):                   # dict
    def __init__(self, table):
        self.table = table
        self.columns = SortedDict()
        for name, column in self.table.base_columns.iteritems():
            self.columns[name] = BoundColumn(self.table, column, name)

        for name, bound_column in self.iteritems():
            bound_column.render = bound_column.column.render
            bound_column._render_args = getargspec(bound_column.render).args[1:]
            # print 'bound_column._render_args::', bound_column._render_args

    def iteritems(self):
        # print self.table.sequence
        for name in self.table.sequence:
            yield (name, self.columns[name])

    def __len__(self):
        return len(self.columns)


    def iterall(self):
        return (self.columns[name] for name in self.table.sequence)


    def __iter__(self):
        for name in self.table.sequence:
            yield self.columns[name]

    def __getitem__(self, index):
        if isinstance(index, int):
            try:
                return next(islice(self.iterall(), index, index + 1))
            except StopIteration:
                raise IndexError
        elif isinstance(index, basestring):
            for bound_column in self.iterall():
                if bound_column.name == index:
                    return bound_column
            raise KeyError("Column with name '%s' does not exist; "
                           "choices are: %s" % (index, self.names()))
        else:
            raise TypeError(u'row indices must be integers or str, not %s'
                            % type(index).__name__)

