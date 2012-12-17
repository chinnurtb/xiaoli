# encoding: utf-8
'''
   支持功能：
       1. 设定某列不能为空
       2. 单元格数据是否符合指定格式
       3. 某些字段不允许更新
       4. 当单元格为空时，指定默认值
       5. 验证数据在网管中是否存在，如 导入数据的厂家，区域是否存在
       6. 是否有权限操作,如 是否有更新此OLT的权限
       7. 导入列可以无序
   暂不支持功能：
       1. 多sheet导入
'''
import os
import csv
from datetime import datetime

class ImportColumn(object):
    def __init__(self, name_cn, name_en, type, allow_null=True, format=None, existed_data={}, allow_update=True, is_key=False, default=None):
        self.name_cn = name_cn
        self.name_en = name_en
        self.type = type
        self.allow_null = allow_null
        self.format = format
        self.existed_data = existed_data
        self.allow_update = allow_update
        self.is_key = is_key
        self.default = default

class CsvImport(object):
    def __init__(self, session, table):
        self.columns = []
        self.session = session
        self.table = table
        self.update_dict = {}
        self.permit_update_dict = {}

    def addColumn(self, column):
        self.columns.append(column)
        return self

    def load_update(self,permit_update_dict, update_dict):
        self.permit_update_dict = permit_update_dict
        self.update_dict = update_dict

    def _read_init(self):
        columns_cn_dict = {}
        columns_en_dict = {}
        key_list = []
        for column in self.columns:
            if column.is_key: key_list.append(column.name_cn)
            columns_cn_dict[column.name_cn] = column
            columns_en_dict[column.name_en] = column
        self.columns_cn_dict = columns_cn_dict
        self.columns_en_dict = columns_en_dict
        self.key_list = key_list
        self.key_col = {}

    def read(self, file):
        self._read_init()   # 读之前进行一些初始化处理
        result = {"insert": {}, 'update': {}, 'error': {}}
        file = os.path.join(os.path.dirname(os.path.abspath(__file__)),file)
        reader = csv.reader(open(file,'rb'))
        for row,row_data in enumerate(reader):
            if row == 0:
                for col_index, col_name in enumerate(row_data):
                    self.key_col[col_index] = col_name.decode('gbk','ignore')
            else:
                row_dict = {}
                for col_index, value in enumerate(row_data):
                    row_dict[self.key_col[col_index]] = value.decode('gbk','ignore')
                action, row_dict_process = self._validate_row(row_dict)      # 验证每一行数据，返回操作动作(插入，更新，错误)和数据
                key = tuple([row_dict.get(key) for key in self.key_list])
                if action == "insert" and self.update_dict.has_key(key):
                    if self.permit_update_dict.has_key(key):    # 如果有权限，则验证更新限制条件
                        action, row_dict = self._validate_update(row_dict_process, row_dict, self.permit_update_dict[key])
                    else:
                        action = "error"
                        row_dict[u'错误提示'] = u'没有操作权限'
                if action == "insert": row_dict = row_dict_process
                result[action][key] = row_dict
        self._insert(result.get("insert",{}).values())  # 执行批量插入
        self._update(result.get("update",{}).values())  # 执行批量更新
        error_file = self._error(result.get("error",{}).values())   # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入%s表%s条，更新%s条记录。%s" % (self.table, len(result.get("insert",{}).values()), len(result.get("update",{}).values()), error_file)

    def _validate_update(self, row_dict_process, row_dict, existed_data_dict):   # 验证更新限制条件
        error = ''
        row_dict_process2 = {}
        for col_name, column in self.columns_cn_dict.items():
            info, data = self._validate_update_data(row_dict_process.get(col_name,''), column, existed_data_dict)
            if info != "": error += info
            row_dict_process2[col_name] = data
        if error == '':
            row_dict_process2['id'] = existed_data_dict['id']
            return "update", row_dict_process2
        else:
            row_dict[u'错误提示'] = error
            return "error", row_dict

    def _validate_update_data(self, data, column, existed_data_dict):
        if  not column.allow_update and data != existed_data_dict.get(column.name_cn):
            return u'%s不允许更新数据; '% column.name_cn, data
        return '', data

    def _validate_row(self, row_dict):
        error = ''
        row_dict_process = {}
        for col_name, column in self.columns_cn_dict.items():
            info, data = self._validate_data(row_dict.get(col_name,''), column)
            if info != "": error += info
            row_dict_process[col_name] = data
        if error == '':
            return "insert", row_dict_process
        else:
            row_dict[u'错误提示'] = error
            return "error", row_dict

    def _validate_data(self, data, column):
        if not column.allow_null and data == "":       # 验证单元格不能为空
            return u'%s不能为空; ' % column.name_cn, data
        if data == "":                                  # 如果单元格为空，返回默认值
            return '', column.default
        if column.format and callable(column.format) and not column.format(data):   # 验证数据格式
            return u'%s格式不正确; ' % column.name_cn, data
        if column.existed_data:
            if column.existed_data.has_key(data):
                return '', column.existed_data[data]
            else:
                return u'网管中不存在%s(%s); ' % (column.name_cn, data), data
        return '', data

    def _insert(self, datas):
        if len(datas) == 0 or len(self.columns) == 0: return
        # 拼接插入sql语句：例sql = u'insert into mit_olts(ip, cityid) values(%s,%s);'
        sql = u'INSERT INTO '+ self.table + ' ('
        for column in self.columns:
            sql += column.name_en + ','
        sql = sql[:-1] + ') VALUES (' + '%s,'*len(self.columns)
        sql = sql[:-1] + ');'
        data_list = [[data_dict.get(column.name_cn) for column in self.columns] for data_dict in datas]
        self.session.execute(sql, data_list)

    def _update(self, datas):
        if len(datas) == 0 or len(self.columns) == 0: return
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
        for column in self.columns:
            create_temp_table += column.name_en + ' '+column.type+','
        create_temp_table = create_temp_table[:-1] + ');'
        self.session.execute(create_temp_table)
        self.session.execute("CREATE INDEX index_id ON temp_%s(id)" % self.table)

        insert_data = u'insert into temp_'+self.table+' (id,'
        for column in self.columns:
            insert_data += column.name_en + ','
        insert_data = insert_data[:-1] + ') VALUES (' + '%s,'*(len(self.columns)+1)
        insert_data = insert_data[:-1] + ');'
        data_list = [[data_dict.get('id')] + [data_dict.get(column.name_cn) for column in self.columns] for data_dict in datas]
        self.session.execute(insert_data, data_list)

        update_data = u'update '+self.table+' set '
        for column in self.columns:
            update_data += column.name_en+'=t2.'+column.name_en+ ','
        update_data = update_data[:-1] + ' from temp_%s as t2 where %s.id=t2.id;' % (self.table,self.table)
        self.session.execute(update_data)

    def _error(self, datas):
        if len(datas) == 0: return ""
        root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','static','file','download')
        if not os.path.isdir(root_path): os.mkdir(root_path)
        file = os.path.join(root_path,self.table+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
        f = open(file,'wb')
        fieldnames = [column.name_cn.encode('gbk') for column in self.columns]+[u'错误提示'.encode('gbk'),]
        writer = csv.writer(f)
        writer.writerow(fieldnames)
        data_list = [[data_dict.get(column.name_cn,'').encode('gbk') for column in self.columns]+[data_dict.get(u'错误提示','').encode('gbk')] for data_dict in datas]
        for data in data_list:
            writer.writerow(data)
        return os.path.basename(file)

def validate_float(value):
    try:
        float(value)
        return True
    except ValueError:
        return False

class CityImport(object):
    def __init__(self, session):
        self.session = session
        self.columns = [
            (u'地市名称', 'name'),
            (u'地市别名', 'alias'),
            (u'经度', 'longitude'),
            (u'纬度', 'latitude'),
            (u'备注', 'remark')
        ]
        self.columns_cn_dict = dict(self.columns)
        self.columns_en_dict = dict([(name_en, name_cn) for name_cn, name_en in self.columns])
        self.error_records = []

    def read(self, file):
        file = os.path.join(os.path.dirname(os.path.abspath(__file__)),file)
        reader = csv.reader(open(file,'rb'))
        key_col = {}
        for col_index, col_name in enumerate(reader[0]):
            name_en = self.columns_cn_dict.get(col_name.decode('gbk','ignore'))
            if name_en:
                key_col[col_index] = name_en
            else:
                return u"导入文件表头错误"
        records = []
        for row_data in reader[1:]:
            row_dict = {}
            for col_index, value in enumerate(row_data):
                row_dict[key_col[col_index]] = value.decode('gbk','ignore')
            records.append(row_dict)

        # 验证数据格式
        right_records = []
        for record_dict in records:
            #验证不能为空
            for name_en in ['name','alias']:
                if not record_dict.get(name_en):
                    record_dict['error'] = self.columns_en_dict.get(name_en)+u'不能为空'
                    self.error_records.append(record_dict)
            #验证数字
            for name_en in ['longitude','latitude']:
                if record_dict.get(name_en) and not validate_float(record_dict[name_en]):
                    record_dict['error'] = self.columns_en_dict.get(name_en)+u'数据格式不正确'
                    self.error_records.append(record_dict)
            right_records.append(record_dict)

        self.session.execute("drop table if exists temp_cities;")
        create_temp_table = '''
        create temporary table temp_cities (id int, name character varying(40), alias character varying(200),
            longitude double precision, latitude double precision, remark character varying(200));
        '''
        self.session.execute(create_temp_table)
        self.session.execute("CREATE INDEX index_id ON temp_cities(id)")
        insert_temp = 'insert into temp_cities (name, alias, longitude, latitude, remark) values (%s,%s,%s,%s,%s);'
        self.session.execute(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.session.execute('update temp_cities set id=t2.id from areas t2 where temp_cities.alias=t2.alias;')
        self.session.execute('update areas set name=t2.name,longitude=t2.longitude,latitude=t2.latitude,remark=t2.remark from temp_cities t2 where areas.id=t2.id')
        insert_data = self.session.execute('select name, alias, longitude, latitude, remark from temp_cities where id is null;')
        insert_sql = 'insert into areas(name,alias,longitude,latitude,remark,parent_id,area_type) values (%s,%s,%s,%s,%s,%s,%s);'
        self.session.execute(insert_sql, [data+(100,1) for data in insert_data])

if __name__ == "__main__":
    from sqlalchemy import create_engine
    import re
    import psycopg2
    from datetime import datetime
    engine = create_engine('postgresql+psycopg2://postgres:postgres@192.168.100.71:5432/ipon')
    #reader = CsvImport(session=engine, table='node_olts')
    #reader.read(file='olts.csv')
    start = datetime.now()
    url_list = re.split('[://|:|@|/]', 'postgresql+psycopg2://postgres:postgres@192.168.100.71:5432/ipon')
    conn = psycopg2.connect(database=url_list[-1],port=url_list[-2],host=url_list[-3], password=url_list[-4], user=url_list[-5])
    cur = conn.cursor()
    io = open('copy_to.txt', 'w')
    cur.copy_to(io, 'node_olts')
    io.close()
    rows = open('copy_to.txt', 'r').readlines()
    print "   File has %d rows:" % len(rows)
    end = datetime.now()
    print end - start

    cur.execute("select * from node_olts")
    result = cur.fetchall()
    end2 = datetime.now()
    print end2 -end

    cur.execute("select * from node_olts")
    while True:
        result = cur.fetchmany(5000)
        if len(result) == 0:
            break
    end3 = datetime.now()
    print end3 -end2
