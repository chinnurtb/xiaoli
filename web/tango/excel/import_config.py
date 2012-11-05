# -*- coding: utf-8 -*-
tables = {
    'node_olts': {
        u'名称': ('name','character varying(40)'),
        u'别名': ('alias', 'character varying(200)'),
        u'节点类型': ('category_id', 'integer'),
        u'IP地址': ('addr', 'character varying(200)'),
        u'所属区域': ('area_id', 'integer'),
        u'子网掩码': ('mask', 'character varying(200)'),
        u'读团体名': ('snmp_comm', 'character varying(50)'),
        u'写团体名': ('snmp_wcomm', 'character varying(50)'),
        u'位置': ('location', 'character varying(200)'),
        u'备注': ('remark', 'character varying(200)'),
        },
    }