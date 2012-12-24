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
        if isinstance(records,unicode): return records

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
        if isinstance(records,unicode): return records

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
        return u"成功导入区县%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

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
        if isinstance(records,unicode): return records

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
        return u"成功导入分局%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

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
        if isinstance(records,unicode): return records

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
        return u"成功导入接入点%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

class RouterImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'名称', 'name'),
            (u'别名', 'alias'),
            (u'IP地址', 'addr'),
            (u'所属接入点', 'entrance_name'),
            (u'子网掩码', 'mask'),
            (u'读团体名', 'snmp_comm'),
            (u'写团体名', 'snmp_wcomm'),
            (u'位置', 'location'),
            (u'备注', 'remark'),
        ]
        super(RouterImport, self).__init__(engine, columns)

    def read(self, file, data_dict):
        # 1.读取文件
        records = self._read(file)
        if isinstance(records,unicode): return records

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['name','alias','addr','entrance_name','snmp_comm','snmp_wcomm']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            if record_dict.get('entrance_name') and record_dict.get('entrance_name') not in data_dict.get('entrance_name',{}):
                error += self.columns_en_dict.get('entrance_name')+u'不存在或没有权限; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                for name_en in ['entrance_name']:
                    if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_routers;")
        create_temp_table = '''
        create temporary table temp_routers (id int, name character varying(40), alias character varying(200),addr character varying(20),
            area_id int, mask character varying(60), snmp_comm character varying(40),
            snmp_wcomm character varying(40),location character varying(200),remark character varying(200));
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_routers(id)")
        insert_temp = 'insert into temp_routers (name, alias, addr, area_id, mask, snmp_comm, snmp_wcomm, location, remark) values (%s,%s,%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_routers set id=t2.id from node_routers t2 where temp_routers.addr=t2.addr;')
        self.cursor.execute('update node_routers set name=t2.name,alias=t2.alias,area_id=t2.area_id,snmp_comm=t2.snmp_comm,snmp_wcomm=t2.snmp_wcomm from temp_routers t2 where node_routers.id=t2.id;')
        self.cursor.execute('update node_routers set mask=t2.mask from temp_routers t2 where node_routers.id=t2.id and t2.mask is not null;')
        self.cursor.execute('update node_routers set location=t2.location from temp_routers t2 where node_routers.id=t2.id and t2.location is not null;')
        self.cursor.execute('update node_routers set remark=t2.remark from temp_routers t2 where node_routers.id=t2.id and t2.remark is not null;')
        self.cursor.execute('select name, alias, addr, area_id, mask, snmp_comm, snmp_wcomm, location, remark from temp_routers where id is null;')
        insert_data = self.cursor.fetchall()
        insert_sql = 'insert into node_routers(name, alias, addr, area_id, mask, snmp_comm, snmp_wcomm, location, remark,category_id) values (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_sql, [data+(1,) for data in insert_data])
        self.conn.commit()

        # 4.错误数据回写
        error_file = self._error('routers') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入路由器%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

class SwitchImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'名称', 'name'),
            (u'别名', 'alias'),
            (u'IP地址', 'addr'),
            (u'所属接入点', 'entrance_name'),
            (u'子网掩码', 'mask'),
            (u'读团体名', 'snmp_comm'),
            (u'写团体名', 'snmp_wcomm'),
            (u'位置', 'location'),
            (u'备注', 'remark'),
        ]
        super(SwitchImport, self).__init__(engine, columns)

    def read(self, file, data_dict):
        # 1.读取文件
        records = self._read(file)
        if isinstance(records,unicode): return records

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['name','alias','addr','entrance_name','snmp_comm','snmp_wcomm']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            if record_dict.get('entrance_name') and record_dict.get('entrance_name') not in data_dict.get('entrance_name',{}):
                error += self.columns_en_dict.get('entrance_name')+u'不存在或没有权限; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                for name_en in ['entrance_name']:
                    if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_switches;")
        create_temp_table = '''
        create temporary table temp_switches (id int, name character varying(40), alias character varying(200),addr character varying(20),
            area_id int, mask character varying(60), snmp_comm character varying(40),
            snmp_wcomm character varying(40),location character varying(200),remark character varying(200));
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_switches(id)")
        insert_temp = 'insert into temp_switches (name, alias, addr, area_id, mask, snmp_comm, snmp_wcomm, location, remark) values (%s,%s,%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_switches set id=t2.id from node_switchs t2 where temp_switches.addr=t2.addr;')
        self.cursor.execute('update node_switchs set name=t2.name,alias=t2.alias,area_id=t2.area_id,mask=t2.mask,snmp_comm=t2.snmp_comm,snmp_wcomm=t2.snmp_wcomm,location=t2.location,remark=t2.remark from temp_switches t2 where node_switchs.id=t2.id;')
        self.cursor.execute('select name, alias, addr, area_id, mask, snmp_comm, snmp_wcomm, location, remark from temp_switches where id is null;')
        insert_data = self.cursor.fetchall()
        insert_sql = 'insert into node_switchs(name, alias, addr, area_id, mask, snmp_comm, snmp_wcomm, location, remark,category_id) values (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_sql, [data+(2,) for data in insert_data])
        self.conn.commit()

        # 4.错误数据回写
        error_file = self._error('switches') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入交换机%s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

class OltImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'名称', 'name'),
            (u'别名', 'alias'),
            (u'IP地址', 'addr'),
            (u'所属分局', 'branch_name'),
            (u'厂商', 'vendor_id'),
            (u'子网掩码', 'mask'),
            (u'读团体名', 'snmp_comm'),
            (u'写团体名', 'snmp_wcomm'),
            (u'SNMP版本', 'snmp_ver'),
            (u'备注', 'remark'),
        ]
        super(OltImport, self).__init__(engine, columns)

    def read(self, file, data_dict):
        # 1.读取文件
        records = self._read(file)
        if isinstance(records,unicode): return records

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['name','alias','addr','branch_name','vendor_id','snmp_comm','snmp_wcomm','snmp_ver']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            for name_en in ['vendor_id','snmp_ver']:
                if record_dict.get(name_en) and record_dict.get(name_en) not in data_dict.get(name_en,{}):
                    error += self.columns_en_dict.get(name_en)+u'不存在; '
            if record_dict.get('branch_name') and record_dict.get('branch_name') not in data_dict.get('branch_name',{}):
                error += self.columns_en_dict.get('branch_name')+u'不存在或没有权限; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                for name_en in ['branch_name','vendor_id']:
                    if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_olts;")
        create_temp_table = '''
        create temporary table temp_olts (id int, name character varying(40), alias character varying(200),addr character varying(20),
            area_id int, vendor_id int, mask character varying(60), snmp_comm character varying(40),
            snmp_wcomm character varying(40),snmp_ver character varying(40),remark character varying(200));
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_olts(id)")
        insert_temp = 'insert into temp_olts (name, alias, addr, area_id, vendor_id, mask, snmp_comm, snmp_wcomm, snmp_ver, remark) values (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_olts set id=t2.id from node_olts t2 where temp_olts.addr=t2.addr;')
        self.cursor.execute('update node_olts set name=t2.name,alias=t2.alias,area_id=t2.area_id,vendor_id=t2.vendor_id,mask=t2.mask,snmp_comm=t2.snmp_comm,snmp_wcomm=t2.snmp_wcomm,snmp_ver=t2.snmp_ver,remark=t2.remark from temp_olts t2 where node_olts.id=t2.id;')
        self.cursor.execute('select name, alias, addr, area_id, vendor_id, mask, snmp_comm, snmp_wcomm, snmp_ver, remark from temp_olts where id is null;')
        insert_data = self.cursor.fetchall()
        insert_sql = 'insert into node_olts(name, alias, addr, area_id, vendor_id, mask, snmp_comm, snmp_wcomm, snmp_ver, remark,category_id) values (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_sql, [data+(20,) for data in insert_data])
        self.conn.commit()

        # 4.错误数据回写
        error_file = self._error('olts') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功导入OLT %s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

class EocImport(CsvImport):
        def __init__(self, engine):
            columns = [
                (u'名称', 'name'),
                (u'别名', 'alias'),
                (u'IP地址', 'addr'),
                (u'所属分局', 'branch_name'),
                (u'厂商', 'vendor_id'),
                (u'子网掩码', 'mask'),
                (u'读团体名', 'snmp_comm'),
                (u'写团体名', 'snmp_wcomm'),
                (u'SNMP版本', 'snmp_ver'),
                (u'备注', 'remark'),
            ]
            super(EocImport, self).__init__(engine, columns)

        def read(self, file, data_dict):
            # 1.读取文件
            records = self._read(file)
            if isinstance(records,unicode): return records

            # 2.验证数据
            right_records = []
            for record_dict in records:
                error = ''
                for name_en in ['name','alias','addr','branch_name','vendor_id','snmp_comm','snmp_wcomm','snmp_ver']:    #验证不能为空
                    if not record_dict.get(name_en):
                        error += self.columns_en_dict.get(name_en)+u'不能为空; '
                for name_en in ['vendor_id','snmp_ver']:
                    if record_dict.get(name_en) and record_dict.get(name_en) not in data_dict.get(name_en,{}):
                        error += self.columns_en_dict.get(name_en)+u'不存在; '
                if record_dict.get('branch_name') and record_dict.get('branch_name') not in data_dict.get('branch_name',{}):
                    error += self.columns_en_dict.get('branch_name')+u'不存在或没有权限; '
                if error:
                    record_dict['error'] = error
                    self.error_records.append(record_dict)
                else:
                    for key,value in record_dict.items():
                        if value == '': record_dict[key] = None
                    for name_en in ['branch_name','vendor_id']:
                        if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                    right_records.append(record_dict)

            # 3.插入更新数据
            self.cursor.execute("drop table if exists temp_eocs;")
            create_temp_table = '''
        create temporary table temp_eocs (id int, name character varying(40), alias character varying(200),addr character varying(20),
            area_id int, vendor_id int, mask character varying(60), snmp_comm character varying(40),
            snmp_wcomm character varying(40),snmp_ver character varying(40),remark character varying(200));
        '''
            self.cursor.execute(create_temp_table)
            self.cursor.execute("CREATE INDEX index_id ON temp_eocs(id)")
            insert_temp = 'insert into temp_eocs (name, alias, addr, area_id, vendor_id, mask, snmp_comm, snmp_wcomm, snmp_ver, remark) values (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);'
            self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
            self.cursor.execute('update temp_eocs set id=t2.id from node_eocs t2 where temp_eocs.addr=t2.addr;')
            self.cursor.execute('update node_eocs set name=t2.name,alias=t2.alias,area_id=t2.area_id,vendor_id=t2.vendor_id,mask=t2.mask,snmp_comm=t2.snmp_comm,snmp_wcomm=t2.snmp_wcomm,snmp_ver=t2.snmp_ver,remark=t2.remark from temp_eocs t2 where node_eocs.id=t2.id;')
            self.cursor.execute('select name, alias, addr, area_id, vendor_id, mask, snmp_comm, snmp_wcomm, snmp_ver, remark from temp_eocs where id is null;')
            insert_data = self.cursor.fetchall()
            insert_sql = 'insert into node_eocs(name, alias, addr, area_id, vendor_id, mask, snmp_comm, snmp_wcomm, snmp_ver, remark,category_id) values (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);'
            self.cursor.executemany(insert_sql, [data+(50,) for data in insert_data])
            self.conn.commit()

            # 4.错误数据回写
            error_file = self._error('eocs') # 错误数据回写
            if error_file:
                error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
            return u"成功导入EOC %s条，更新%s条记录。%s" % (len(insert_data), len(right_records)-len(insert_data), error_file)

class OnuImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'名称', 'name'),
            (u'别名', 'alias'),
            (u'IP地址', 'addr'),
            (u'认证标识', 'mac'),
            (u'所属OLT IP', 'olt_ip'),
            (u'所属接入点', 'entrance_name'),
            (u'读团体名', 'snmp_comm'),
            (u'写团体名', 'snmp_wcomm'),
            (u'SNMP版本', 'snmp_ver'),
            (u'备注', 'remark'),
        ]
        super(OnuImport, self).__init__(engine, columns)

    def read(self, file, data_dict):
        # 1.读取文件
        records = self._read(file)
        if isinstance(records,unicode): return records

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['olt_ip']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            for name_en in ['snmp_ver']:
                if record_dict.get(name_en) and record_dict.get(name_en) not in data_dict.get(name_en,{}):
                    error += self.columns_en_dict.get(name_en)+u'不存在; '
            if not record_dict.get('addr') and not record_dict.get('mac'):
                error += u'IP地址和认证标识不能同时为空;'
            if record_dict.get('entrance_name') and record_dict.get('entrance_name') not in data_dict.get('entrance_name',{}):
                error += self.columns_en_dict.get('entrance_name')+u'不存在或没有权限; '
            elif record_dict.get('entrance_name') and record_dict.get('olt_ip') and record_dict['entrance_name'] not in data_dict['olt_entrance'].get(record_dict['olt_ip']):
                error += u'此接入点不在OLT所属分局中; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                for name_en in ['entrance_name']:
                    if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_onus;")
        create_temp_table = '''
        create temporary table temp_onus (
            id int, name character varying(40), alias character varying(200),addr character varying(20),
            mac character varying(20),olt_ip character varying(20), area_id int,snmp_comm character varying(40),
            snmp_wcomm character varying(40),snmp_ver character varying(40),remark character varying(200)
        );
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_onus(id)")
        insert_temp = 'insert into temp_onus (name, alias, addr, mac, olt_ip, area_id, snmp_comm, snmp_wcomm, snmp_ver, remark) values (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_onus set id=t2.id from node_onus t2 left join node_olts t3 on t3.id=t2.ctrl_id left join areas on areas.id=t3.area_id where temp_onus.addr is not null and temp_onus.addr=t2.addr and t3.addr=temp_onus.olt_ip and %s;'% data_dict.get("import_clause_permit"))
        self.cursor.execute('update temp_onus set id=t2.id from node_onus t2 left join node_olts t3 on t3.id=t2.ctrl_id left join areas on areas.id=t3.area_id where temp_onus.mac is not null and temp_onus.mac=t2.mac and t3.addr=temp_onus.olt_ip and %s;'% data_dict.get("import_clause_permit"))

        self.cursor.execute('update node_onus set name=t2.name from temp_onus t2 where node_onus.id=t2.id and t2.name is not null;')
        self.cursor.execute('update node_onus set alias=t2.alias from temp_onus t2 where node_onus.id=t2.id and t2.alias is not null;')
        self.cursor.execute('update node_onus set area_id=t2.area_id from temp_onus t2 where node_onus.id=t2.id and t2.area_id is not null;')
        self.cursor.execute('update node_onus set snmp_comm=t2.snmp_comm from temp_onus t2 where node_onus.id=t2.id and t2.snmp_comm is not null;')
        self.cursor.execute('update node_onus set snmp_wcomm=t2.snmp_wcomm from temp_onus t2 where node_onus.id=t2.id and t2.snmp_wcomm is not null;')
        self.cursor.execute('update node_onus set snmp_ver=t2.snmp_ver from temp_onus t2 where node_onus.id=t2.id and t2.snmp_ver is not null;')
        self.cursor.execute('update node_onus set remark=t2.remark from temp_onus t2 where node_onus.id=t2.id and t2.remark is not null;')
        self.conn.commit()

        self.cursor.execute('select name, alias, addr, mac, olt_ip, area_id, snmp_comm, snmp_wcomm, snmp_ver, remark from temp_onus where id is null;')
        error_data = self.cursor.fetchall()
        entrance_dict = dict([(value,key) for key, value in data_dict.get("entrance_name",{}).items()])
        def replace(data):
            data_dict = dict(zip([column[1] for column in self.columns], [da if da is not None else '' for da in data]))
            data_dict['entrance_name'] = entrance_dict.get(data_dict.get('entrance_name'), '')
            data_dict['error'] = u'ONU不存在或没有权限; '
            return data_dict
        error_data = [replace(data) for data in error_data]
        self.error_records.extend(error_data)

        # 4.错误数据回写
        error_file = self._error('onus') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功更新ONU %s条记录。%s" % (len(right_records)-len(error_data), error_file)

class CpeImport(CsvImport):
    def __init__(self, engine):
        columns = [
            (u'名称', 'name'),
            (u'别名', 'alias'),
            (u'MAC地址', 'mac'),
            (u'所属EOC IP', 'eoc_ip'),
            (u'所属接入点', 'entrance_name'),
            (u'读团体名', 'snmp_comm'),
            (u'写团体名', 'snmp_wcomm'),
            (u'SNMP版本', 'snmp_ver'),
            (u'备注', 'remark'),
        ]
        super(CpeImport, self).__init__(engine, columns)

    def read(self, file, data_dict):
        # 1.读取文件
        records = self._read(file)
        if isinstance(records,unicode): return records

        # 2.验证数据
        right_records = []
        for record_dict in records:
            error = ''
            for name_en in ['mac','eoc_ip']:    #验证不能为空
                if not record_dict.get(name_en):
                    error += self.columns_en_dict.get(name_en)+u'不能为空; '
            for name_en in ['snmp_ver']:
                if record_dict.get(name_en) and record_dict.get(name_en) not in data_dict.get(name_en,{}):
                    error += self.columns_en_dict.get(name_en)+u'不存在; '
            if record_dict.get('entrance_name') and record_dict.get('entrance_name') not in data_dict.get('entrance_name',{}):
                error += self.columns_en_dict.get('entrance_name')+u'不存在或没有权限; '
            elif record_dict.get('entrance_name') and record_dict.get('eoc_ip') and record_dict['entrance_name'] not in data_dict['eoc_entrance'].get(record_dict['eoc_ip']):
                error += u'此接入点不在EOC所属分局中; '
            if error:
                record_dict['error'] = error
                self.error_records.append(record_dict)
            else:
                for key,value in record_dict.items():
                    if value == '': record_dict[key] = None
                for name_en in ['entrance_name']:
                    if record_dict[name_en]: record_dict[name_en] = data_dict[name_en][record_dict[name_en]]
                right_records.append(record_dict)

        # 3.插入更新数据
        self.cursor.execute("drop table if exists temp_cpes;")
        create_temp_table = '''
        create temporary table temp_cpes (
            id int, name character varying(40), alias character varying(200),mac character varying(20),
            eoc_ip character varying(20), area_id int,snmp_comm character varying(40),
            snmp_wcomm character varying(40),snmp_ver character varying(40),remark character varying(200)
        );
        '''
        self.cursor.execute(create_temp_table)
        self.cursor.execute("CREATE INDEX index_id ON temp_cpes(id)")
        insert_temp = 'insert into temp_cpes (name, alias, mac, eoc_ip, area_id, snmp_comm, snmp_wcomm, snmp_ver, remark) values (%s,%s,%s,%s,%s,%s,%s,%s,%s);'
        self.cursor.executemany(insert_temp, [[record_dict.get(column[1]) for column in self.columns] for record_dict in right_records])
        self.cursor.execute('update temp_cpes set id=t2.id from node_cpes t2 left join node_eocs t3 on t3.id=t2.ctrl_id left join areas on areas.id=t3.area_id where temp_cpes.mac is not null and temp_cpes.mac=t2.mac and t3.addr=temp_cpes.eoc_ip and %s;'% data_dict.get("import_clause_permit"))

        self.cursor.execute('update node_cpes set name=t2.name from temp_cpes t2 where node_cpes.id=t2.id and t2.name is not null;')
        self.cursor.execute('update node_cpes set alias=t2.alias from temp_cpes t2 where node_cpes.id=t2.id and t2.alias is not null;')
        self.cursor.execute('update node_cpes set area_id=t2.area_id from temp_cpes t2 where node_cpes.id=t2.id and t2.area_id is not null;')
        self.cursor.execute('update node_cpes set snmp_comm=t2.snmp_comm from temp_cpes t2 where node_cpes.id=t2.id and t2.snmp_comm is not null;')
        self.cursor.execute('update node_cpes set snmp_wcomm=t2.snmp_wcomm from temp_cpes t2 where node_cpes.id=t2.id and t2.snmp_wcomm is not null;')
        self.cursor.execute('update node_cpes set snmp_ver=t2.snmp_ver from temp_cpes t2 where node_cpes.id=t2.id and t2.snmp_ver is not null;')
        self.cursor.execute('update node_cpes set remark=t2.remark from temp_cpes t2 where node_cpes.id=t2.id and t2.remark is not null;')
        self.conn.commit()

        self.cursor.execute('select name, alias, mac, eoc_ip, area_id, snmp_comm, snmp_wcomm, snmp_ver, remark from temp_cpes where id is null;')
        error_data = self.cursor.fetchall()
        entrance_dict = dict([(value,key) for key, value in data_dict.get("entrance_name",{}).items()])
        def replace(data):
            data_dict = dict(zip([column[1] for column in self.columns], [da if da is not None else '' for da in data]))
            data_dict['entrance_name'] = entrance_dict.get(data_dict.get('entrance_name'), '')
            data_dict['error'] = u'CPE不存在或没有权限; '
            return data_dict
        error_data = [replace(data) for data in error_data]
        self.error_records.extend(error_data)

        # 4.错误数据回写
        error_file = self._error('cpes') # 错误数据回写
        if error_file:
            error_file = u'<a href="/download?file=/static/file/download/%s">下载错误数据</a>' % error_file
        return u"成功更新CPE %s条记录。%s" % (len(right_records)-len(error_data), error_file)