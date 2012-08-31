#!/usr/bin/env python  
# -*- coding: utf-8 -*-

from tango.ui import tables

#### Tables
class UserTable(tables.Table):
    check      = tables.CheckBoxColumn()
    edit_btn   = tables.EditBtnColumn(endpoint='user_edit')
    delete_btn = tables.DeleteBtnColumn(endpoint='user_delete')

    username       = tables.Column(verbose_name=u'用户名', orderable=True)
    name           = tables.Column(verbose_name=u'真实姓名')
    role_name      = tables.Column(verbose_name=u'角色名称', accessor='role.name')
    domain         = tables.Column(verbose_name=u'管理域', accessor='domain.name')
    email          = tables.EmailColumn(verbose_name=u'邮箱')
    department     = tables.Column(verbose_name=u'部门')
    telephone      = tables.Column(verbose_name=u'电话')
    # mobile         = tables.Column()
    # memo           = tables.Column()
    # status         = tables.Column()
    # created_at     = tables.DateTimeColumn(verbose_name=u'注册时间',format='%Y-%m-%d %H:%M')
    # remember_token = tables.Column()

    class Meta():
        model = User
        per_page = 3
        order_by = '-username'
