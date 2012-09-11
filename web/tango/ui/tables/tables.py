#coding=utf-8

from __future__ import absolute_import, unicode_literals

from flask import Flask, render_template, url_for, request

from .columns import *
from .rows import *
from .utils import *

from .actions import Action

## Table
class TableData(object):
    def __init__(self, data, table):
        self.queryset = data
        self.table = table

    def ordering(self, order_by):
        model = self.table._meta.model
        order = 'asc'
        
        if order_by[0] == '-':
            order = 'desc'
            order_by = order_by[1:]
        a = A("%s.%s" % (order_by, order)) # i.e. "name.desc"
        self.queryset = self.queryset.order_by(a.resolve(model))
        
    def paginate(self, page, per_page):
        self.page_obj = self.queryset.paginate(page=self.table.page,
                                               per_page=self.table.per_page)
        self.page_obj.total_items = self.queryset.count()
        self.list = self.page_obj.items

    def __iter__(self):
        return iter(self.list)

    def __len__(self):
        return len(self.list)

    def __getitem__(self, key):
        data =  self.list[key]
        if isinstance(key, slice):
            return type(self)(data, self.table, self.page, self.per_page)
        else:
            return data

class TableMeta(type):

    def __new__(cls, name, bases, attrs):

        attrs["_meta"] = TableOptions(attrs.get("Meta", None))
        columns = [(name_, attrs.pop(name_)) for name_, column in attrs.items()
                                             if isinstance(column, Column)]
        columns.sort(lambda x, y: cmp(x[1].creation_counter, y[1].creation_counter))

        parent_columns = []
        for base in bases[::-1]:
            if hasattr(base, "base_columns"):
                parent_columns = base.base_columns.items() + parent_columns

        attrs["base_columns"] = SortedDict(parent_columns)
        attrs["base_columns"].update(SortedDict(columns))

        actions = [(name_, attrs.pop(name_)) for name_, action in attrs.items()
                                             if isinstance(action, Action)]
        actions.sort(lambda x, y: cmp(x[1].creation_counter, y[1].creation_counter))
        attrs["base_actions"] = SortedDict(actions)

        return super(TableMeta, cls).__new__(cls, name, bases, attrs)


class TableOptions(object):

    def __init__(self, options=None):
        super(TableOptions, self).__init__()
        self.order_by = getattr(options, 'order_by', None)
        self.model = getattr(options, 'model', None)
        if self.order_by and self.model is None:
            raise ValueError("if you give *order_by* you should give *model* too!")
        self.per_page_key = getattr(options, 'per_page_key', None)
        self.hidden_columns_key = getattr(options, 'hidden_columns_key', None)
        self.url_makers = getattr(options, 'url_makers', None)
        # print '(TableOptions)self.per_page::', self.per_page


class Table(object):
    __metaclass__ = TableMeta
    TableDataClass = TableData

    def __init__(self, data, sequence=None, template=None,):

        # Sequence
        self.sequence = sequence
        if sequence is not None:
            self._sequence = Sequence(sequence)
            self._sequence.expand(self.base_columns.keys())
        else:
            self._sequence = Sequence(('...',))
            self._sequence.expand(self.base_columns.keys())
        self.columns = BoundColumns(self)

        print self.base_actions

        self.actions = self.base_actions.values()

        # Order By

        self.data = self.TableDataClass(data=data, table=self)
        self.rows = BoundRows(self.data)
        self.template = template

        # print 'self._sequence', self._sequence

    def configure(self, profile, page=1, order_by=None):
        self.hidden_columns = profile.get(self._meta.hidden_columns_key, '').split(',')
        for name in self.hidden_columns:
            if name: self.columns[name].hidden = True

        self.per_page = int(profile.get(self._meta.per_page_key, '20'))
        self.page = page
        if order_by:
            self.data.ordering(order_by)
        self.data.paginate(self.page, self.per_page)
        return self
    
    @property
    def sequence(self):
        return self._sequence

    @sequence.setter
    def sequence(self, value):
        if value:
            value = Sequence(value)
            value.expand(self.base_columns.keys())
        self._sequence = value

    @property
    def page_obj(self):
        return self.data.page_obj

    @property
    def page_url(self):
        req_args = request.args.to_dict()
        uri = request.url_root + request.path[1:]
        def func(page, order_by=None):
            req_args['page'] = page
            old_order_by = req_args.get('order_by', None)
            if order_by:
                if old_order_by == order_by:
                    order_by = '-' + order_by
                req_args['order_by'] = order_by
            # print 'req_args["order_by"]::', req_args['order_by']
            return uri + '?' + '&'.join(['%s=%s'%(k, v)
                                         for k, v in req_args.items()])
        return func

    def as_html():
        pass


if __name__ == '__main__':
    from flask_sqlalchemy import SQLAlchemy
    app = Flask(__name__)
    app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://root:yawen00@localhost/test_table'
    app.config['DEBUG'] = True
    db = SQLAlchemy(app)

    class User(db.Model):
        __tablename__ = 'users3'

        id = db.Column(db.Integer, primary_key=True)
        parent_id = db.Column(db.Integer, db.ForeignKey('users3.id'))
        email = db.Column(db.String(50))
        name = db.Column(db.String(40))
        age = db.Column(db.Integer(40))
        bit = db.Column(db.String(30))

        parent = db.relationship("User", remote_side=[id])

        def __init__(self, parent_id, email, name, age, bit):
            self.parent_id = parent_id
            self.email = email
            self.name = name
            self.age = age
            self.bit = bit

    # db.create_all()

    class UserTable(Table):
        check = CheckBoxColumn()
        parent = LinkColumn('user', accessor="parent.name")
        email = EmailColumn(orderable=True)
        name = Column(orderable=True)
        age = Column(orderable=True)
        bit = Column()

        class Meta():
            per_page = 3
            order_by = '-age'
            model = User                # just for order by
            url_makers = {'parent': lambda record: url_for('user', id=getattr(record, 'id', 1),
                                                           mk='QUERY_STRING')}


    ## views
    @app.route("/user/<int:id>/")
    def user(id):
        user = User.query.get_or_404(id)
        return ', '.join([user.name, str(user.age), 'Parent:', user.parent.name, user.bit])

    @app.route("/table")
    def index():
        # print 'url_for::', url_for('index', a=3, b=2, _external=True)
        page = request.args.get('page', 1, type=int)
        table = UserTable(User.query, page=page, request=request)
        return render_template('test_table.html', table=table)

    app.run(host="0.0.0.0", port=5001)
