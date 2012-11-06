# encoding: utf-8
import os
import csv
from datetime import datetime

from .import_config import tables

class CsvImport(object):
    """
        param:session       SQLAlchemy session对象，连接数据库执行语句
        param:table         字符串，导入表的表名
        param:primary_key   表字段名的List,保存数据的key，判断数据是否重复，如导入OLT，primary_key=['ip']，IP不能重复
        param:validate
            验证数据的字典,包括：
                allow_null      True/False 是否允许为空值，默认为True
                format          函数，判断列值是否符合格式，返回True或False
                existed_data    字典，网管中是否存在此数据，value为id，以便直接保存
            example:
                {
                    'addr': {
                        'allow_null': False,
                        'format': lambda value: True,
                    },
                    'vendor_id': {
                        'existed_data': {u'华为':1, u'中兴':2},
                    },
                    'area_id': {
                        'existed_data': {u'':1,u'':2}
                    }
                }
    """
    def __init__(self, session, table, primary_key, validate={}):
        self.session = session
        self.table = table
        self.primary_key = primary_key
        self.validate=validate
        self.header_dict = dict([(key, value[0]) for key, value in tables.get(table,{}).items()]) # 导入excel 列名与对应的字段名 的字段：{u'名称': 'name', u'IP地址': 'ip'}
        self.table_dict = dict([(value,key) for key,value in self.header_dict.items()]) # 字段名与列名的字典：{'name': u'名称', 'ip': u'IP地址'}
        self.column_type = dict([(key, value[1]) for key, value in tables.get(table,{}).items()])
        self.header = []    # 保存csv文件首行信息，list
        self.header_ori = []    # 保存csv文件首行信息，list
        self.key_col = {}   # 保存首行列索引与对应的列名，dict

    # file: 导入的文件
    # is_update: 导入的时候，根据主键判断网管中是否已存在此数据，如果存在则更新数据
    def read(self, file, is_update=True):
        update_dict = {}    # 保存网管中已存在数据
        if is_update:
            result = self.session.execute(
                "select id,%s from %s" % (','.join(self.primary_key), self.table)
            )
            for re in result:
                update_dict[re[1:]] = re[0]

        result = {"insert": {}, 'update': {}, 'error': {}}
        file = os.path.join(os.path.dirname(os.path.abspath(__file__)),file)
        reader = csv.reader(open(file,'rb'))
        for row,row_data in enumerate(reader):
            row_data = [data.decode('gbk','ignore') for data in row_data]
            if row == 0:
                self.header_ori = row_data
                self.header = [title for title in row_data if self.header_dict.has_key(title)]
                for col_index, col_name in enumerate(row_data):
                    self.key_col[col_index] = col_name
            else:
                action, data = self._validate_row(row_data)   # 验证每一行数据，返回操作动作(插入，更新，错误)和数据
                key = tuple([data[self.header.index(self.table_dict.get(col_name))] for col_name in self.primary_key])
                if action == "insert" and update_dict.has_key(key):
                    action = "update"
                    data = [update_dict[key],] + data
                result[action][key] = data

        # 执行批量插入
        self._insert(result.get("insert",{}).values())
        # 执行批量更新
        self._update(result.get("update",{}).values())
        # 错误数据回写
        error_file = self._error(result.get("error",{}).values())
        if error_file:
            error_file = u'<a href="/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入%s表%s条，更新%s条记录。%s" % (self.table, len(result.get("insert",{}).values()), len(result.get("update",{}).values()), error_file)

    def _validate_row(self, row_data):
        error = ""
        data_list = []
        for col_index, data in enumerate(row_data):
            col_name = self.key_col.get(col_index,'')
            info, data = self._validate_data(col_name, data)
            error += info
            data_list.append(data)
        if error == "":
            row_data = [data for col_index, data in enumerate(data_list) if self.header_dict.has_key(self.key_col[col_index])]
            return "insert", row_data
        else:
            return "error", row_data+[error,]

    def _validate_data(self, col_name,data):
        col_en = self.header_dict.get(col_name,'')
        validate = self.validate.get(col_en,{})
        if not validate.get("allow_null", True) and data == "":
            return u'%s不能为空' % col_name, data
        if data == "":
            return "",None
        if validate.get("format") and not validate.get("format")(data):
            return u'%s格式不正确' % col_name, data
        if validate.get("existed_data"):
            if validate.get("existed_data").has_key(data):
                return '', validate.get("existed_data").get(data)
            else:
                return u'网管中不存在%s(%s)' % (col_name, data), data
        return "", data

    def _insert(self, datas):
        if len(datas) == 0: return
        # 拼接插入sql语句：例sql = u'insert into mit_olts(ip, cityid) values(%s,%s);'
        sql = u'INSERT INTO '+ self.table + ' ('
        for col_name in self.header:
            if self.header_dict.get(col_name): sql += self.header_dict[col_name] + ','
        if len(self.header) > 0: sql = sql[:-1]
        sql += ') VALUES (' + '%s,'*len(self.header)
        sql = sql[:-1] + ');'
        self.session.execute(sql, datas)

    def _update(self, datas):
        if len(datas) == 0: return
        # 拼接更新sql语句
        # 1.创建临时表 2.临时表插入更新记录 3.临时表与原表进行关联更新
        # create_temp_table = ''' create temporary table if not exists temp_olts(id int, name varchar(50), index index_id(id) ); '''
        # connection.execute(create_temp_table)
        # insert_data = ''' insert into temp_olts(id, name) values (%s, %s); '''
        # connection.executemany(insert_data, datas)
        # update_data = ''' update mit_olts t1, temp_olts t2 set t1.name = t2.name where t1.id = t2.id; '''
        # connection.execute(update_data)
        self.session.execute("drop table if exists temp_%s;"% self.table)
        create_temp_table = u'create temporary table temp_'+self.table + ' (id int,'
        for col_name in self.header:
            if self.header_dict.get(col_name):create_temp_table += self.header_dict[col_name] + ' '+self.column_type[col_name]+','
        if len(self.header) > 0: create_temp_table = create_temp_table[:-1]
        create_temp_table += ');'
        self.session.execute(create_temp_table)
        self.session.execute("CREATE INDEX index_id ON temp_%s(id)" % self.table)
        insert_data = u'insert into temp_'+self.table+' (id,'
        for col_name in self.header:
            if self.header_dict.get(col_name): insert_data += self.header_dict[col_name] + ','
        if len(self.header) > 0: insert_data = insert_data[:-1]
        insert_data += ') VALUES (' + '%s,'*(len(self.header)+1)
        insert_data = insert_data[:-1] + ');'
        self.session.execute(insert_data, datas)

        update_data = u'update '+self.table+' set '
        for col_name in self.header:
            if self.header_dict.get(col_name): update_data += self.header_dict[col_name]+'=t2.'+self.header_dict[col_name]+ ','
        if len(self.header) > 0: update_data = update_data[:-1]
        update_data += ' from temp_%s as t2 where %s.id=t2.id;' % (self.table,self.table)
        self.session.execute(update_data)

    def _error(self, datas):
        if len(datas) == 0: return ""
        root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','static','file','download')
        if not os.path.isdir(root_path): os.mkdir(root_path)
        file = os.path.join(root_path,self.table+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
        f = open(file,'wb')
        writer = csv.writer(f)
        writer.writerow([title.encode('gbk') for title in self.header_ori+[u'错误提示',]])
        for data_list in datas:
            writer.writerow([data.encode('gbk') for data in data_list])
        return os.path.basename(file)

if __name__ == "__main__":
    from sqlalchemy import create_engine
    from sqlalchemy.orm import sessionmaker
    #import psycopg2
    #conn = psycopg2.connect(database="ipon", user="postgres", password="postgres", host="192.168.100.71")
    #cur = conn.cursor()
    #conn.commit()

    engine = create_engine('postgresql+psycopg2://postgres:postgres@192.168.100.71/ipon')
    reader = CsvImport(session=engine, table='node_olts', primary_key=['addr',])
    reader.read(file='olts.csv', is_update=True)
