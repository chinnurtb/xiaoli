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
import psycopg2
from datetime import datetime

def validate_float(value):
    try:
        float(value)
        return True
    except ValueError:
        return False

class CsvImport(object):
    def __init__(self, engine, columns):
        self.columns = columns
        self.columns_cn_dict = dict(self.columns)
        self.columns_en_dict = dict([(name_en, name_cn) for name_cn, name_en in self.columns])
        self.error_records = []
        self.conn = psycopg2.connect(database=engine.url.database, user=engine.url.username,password=engine.url.password, host=engine.url.host, port=engine.url.port)
        self.cursor = self.conn.cursor()

    def _read(self, file):
        file = os.path.join(os.path.dirname(os.path.abspath(__file__)),file)
        reader = [record for record in csv.reader(open(file,'rb'))]
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
        return records

    def _error(self, table):
        if not self.error_records: return ""
        root_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),'..','..','static','file','download')
        if not os.path.isdir(root_path): os.mkdir(root_path)
        file = os.path.join(root_path,table+datetime.now().strftime('(%Y-%m-%d %H-%M-%S %f)')+'.csv')
        f = open(file,'wb')
        fieldnames = [column[0].encode('gbk') for column in self.columns]+[u'错误提示'.encode('gbk'),]
        writer = csv.writer(f)
        writer.writerow(fieldnames)
        for record_dict in self.error_records:
            writer.writerow([record_dict.get(column[1],'').encode('gbk') for column in self.columns] +[record_dict.get('error','').encode('gbk'),])
        return os.path.basename(file)


class CityImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'地市名称', 'name'),
            (u'地市别名', 'alias'),
            (u'经度', 'longitude'),
            (u'纬度', 'latitude'),
            (u'备注', 'remark')
        ]
        super(CityImport, self).__init__(engine, columns)

    def read(self, file):
        # 1.读取文件
        records = self._read(file)

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['name','alias']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            for name_en in ['longitude','latitude']:    #验证数字
                if record_dict.get(name_en) and not validate_float(record_dict[name_en]):
                    error += self.columns_en_dict.get(name_en)+u'数据格式不正确; '
            from tango.pinyin import pinyin
            if record_dict.get('name') and record_dict.get('alias') and  record_dict['name']!= ''.join(pinyin(record_dict['alias'])):
                error += u'拼写错误，名称必须为别名的拼音，如北京：beijing; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_cities;")
        create_temp_table = '''
        create temporary table temp_cities (id int, name character varying(40), alias character varying(200),
            longitude double precision, latitude double precision, remark character varying(200));
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_cities(id)")
        insert_temp = 'insert into temp_cities (name, alias, longitude, latitude, remark) values (%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_cities set id=t2.id from areas t2 where temp_cities.alias=t2.alias;')
        self.cursor.execute('update areas set name=t2.name,longitude=t2.longitude,latitude=t2.latitude,remark=t2.remark from temp_cities t2 where areas.id=t2.id')
        self.cursor.execute('select name, alias, longitude, latitude, remark from temp_cities where id is null;')
        insert_data = self.cursor.fetchall()
        insert_sql = 'insert into areas(name,alias,longitude,latitude,remark,parent_id,area_type) values (%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_sql, [data+(100,1) for data in insert_data])
        self.conn.commit()

        # 4.错误数据回写
        error_file = self._error('cities') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入地市%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

class TownImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'区县名称', 'name'),
            (u'区县别名', 'alias'),
            (u'所属地市', 'city_name'),
            (u'经度', 'longitude'),
            (u'纬度', 'latitude'),
            (u'备注', 'remark')
        ]
        super(TownImport, self).__init__(engine, columns)

    def read(self, file, data_dict):
        # 1.读取文件
        records = self._read(file)

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['name','alias','city_name']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            for name_en in ['longitude','latitude']:    #验证数字
                if record_dict.get(name_en) and not validate_float(record_dict[name_en]):
                    error += self.columns_en_dict.get(name_en)+u'数据格式不正确; '
            for name_en in ['city_name']:
                if record_dict.get(name_en) and record_dict.get(name_en) not in data_dict.get(name_en,{}):
                    error += self.columns_en_dict.get(name_en)+u'不存在或没有权限; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                for name_en in ['city_name']:
                    if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_towns;")
        create_temp_table = '''
        create temporary table temp_towns (id int, name character varying(40), alias character varying(200),parent_id int,
            longitude double precision, latitude double precision, remark character varying(200));
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_towns(id)")
        insert_temp = 'insert into temp_towns (name, alias, parent_id, longitude, latitude, remark) values (%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_towns set id=t2.id from areas t2 where temp_towns.alias=t2.alias;')
        self.cursor.execute('update areas set name=t2.name,parent_id=t2.parent_id,longitude=t2.longitude,latitude=t2.latitude,remark=t2.remark from temp_towns t2 where areas.id=t2.id')
        self.cursor.execute('select name, alias, parent_id, longitude, latitude, remark from temp_towns where id is null;')
        insert_data = self.cursor.fetchall()
        insert_sql = 'insert into areas(name,alias,parent_id,longitude,latitude,remark,area_type) values (%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_sql, [data+(2,) for data in insert_data])
        self.conn.commit()

        # 4.错误数据回写
        error_file = self._error('towns') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入地市%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

class BranchImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'分局名称', 'name'),
            (u'分局别名', 'alias'),
            (u'所属区县', 'town_name'),
            (u'经度', 'longitude'),
            (u'纬度', 'latitude'),
            (u'备注', 'remark')
        ]
        super(BranchImport, self).__init__(engine, columns)

    def read(self, file, data_dict):
        # 1.读取文件
        records = self._read(file)

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['name','alias','town_name']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            for name_en in ['longitude','latitude']:    #验证数字
                if record_dict.get(name_en) and not validate_float(record_dict[name_en]):
                    error += self.columns_en_dict.get(name_en)+u'数据格式不正确; '
            if record_dict.get('town_name') and record_dict.get('town_name') not in data_dict.get('town_name',{}):
                error += self.columns_en_dict.get('town_name')+u'不存在或没有权限; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                for name_en in ['town_name']:
                    if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_branches;")
        create_temp_table = '''
        create temporary table temp_branches (id int, name character varying(40), alias character varying(200),parent_id int,
            longitude double precision, latitude double precision, remark character varying(200));
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_branches(id)")
        insert_temp = 'insert into temp_branches (name, alias, parent_id, longitude, latitude, remark) values (%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_branches set id=t2.id from areas t2 where temp_branches.alias=t2.alias;')
        self.cursor.execute('update areas set name=t2.name,parent_id=t2.parent_id,longitude=t2.longitude,latitude=t2.latitude,remark=t2.remark from temp_branches t2 where areas.id=t2.id')
        self.cursor.execute('select name, alias, parent_id, longitude, latitude, remark from temp_branches where id is null;')
        insert_data = self.cursor.fetchall()
        insert_sql = 'insert into areas(name,alias,parent_id,longitude,latitude,remark,area_type) values (%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_sql, [data+(3,) for data in insert_data])
        self.conn.commit()

        # 4.错误数据回写
        error_file = self._error('branches') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入地市%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

class EntranceImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'接入点名称', 'name'),
            (u'接入点别名', 'alias'),
            (u'所属分局', 'branch_name'),
            (u'经度', 'longitude'),
            (u'纬度', 'latitude'),
            (u'备注', 'remark')
        ]
        super(EntranceImport, self).__init__(engine, columns)

    def read(self, file, data_dict):
        # 1.读取文件
        records = self._read(file)

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['name','alias','branch_name']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            for name_en in ['longitude','latitude']:    #验证数字
                if record_dict.get(name_en) and not validate_float(record_dict[name_en]):
                    error += self.columns_en_dict.get(name_en)+u'数据格式不正确; '
            if record_dict.get('branch_name') and record_dict.get('branch_name') not in data_dict.get('branch_name',{}):
                error += self.columns_en_dict.get('branch_name')+u'不存在或没有权限; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                for name_en in ['branch_name']:
                    if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_entrances;")
        create_temp_table = '''
        create temporary table temp_entrances (id int, name character varying(40), alias character varying(200),parent_id int,
            longitude double precision, latitude double precision, remark character varying(200));
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_entrances(id)")
        insert_temp = 'insert into temp_entrances (name, alias, parent_id, longitude, latitude, remark) values (%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_entrances set id=t2.id from areas t2 where temp_entrances.alias=t2.alias;')
        self.cursor.execute('update areas set name=t2.name,parent_id=t2.parent_id,longitude=t2.longitude,latitude=t2.latitude,remark=t2.remark from temp_entrances t2 where areas.id=t2.id')
        self.cursor.execute('select name, alias, parent_id, longitude, latitude, remark from temp_entrances where id is null;')
        insert_data = self.cursor.fetchall()
        insert_sql = 'insert into areas(name,alias,parent_id,longitude,latitude,remark,area_type) values (%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_sql, [data+(4,) for data in insert_data])
        self.conn.commit()

        # 4.错误数据回写
        error_file = self._error('entrances') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入地市%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)