# encoding: utf-8
import os
import csv

cn = {
    'olts': {
        u'状态': 'status:int',
        u'名称': 'name:str' ,
        u'别名': 'alias:str' ,
        u'IP地址': 'addr:str',
        u'地市': 'area.city_name',
        u'区县': 'area.town_name',
        u'分局': 'area.branch_name:int',
        u'厂商': 'vendor.alias:int',
        u'型号': 'model.alias:int',
        u'子网掩码': 'mask:str',
        u'读团体名': 'snmp_comm:str',
        u'写团体名': 'snmp_wcomm:str',
        u'位置': 'location:str',
        u'备注': 'remark:str',
        },
    }

class CsvReader(object):
    def __init__(self, model="olts", primary_key=["addr"], validate={}):
        # 根据model获取导入csv的title信息（数据库表的字段名，字段类型）
        self.header_dict = dict([(key, {'field_name': value.split(':')[0], 'field_type': value.split(':')[-1]}) for key, value in cn.get(model, {}).items()])
        self.primary_key = primary_key # 导入数据模型的逻辑主键，如OLT导入为：['addr'], ip唯一，所以更新还是插入操作由ip决定
        self.header = []    # 保存csv文件首行信息，list
        self.key_col = {}   # 保存首行列名与对应的列索引，dict
        self.validate={     # 验证每行数据的字典，是否允许为空？数据格式正确？网管中是否存在此数据？
            'addr': {
                'allow_null': True,
                'format': lambda value: True,
                'existed_data': {1:'huawei'}
            }
        }

    def read(self, file="olts.csv"):
        result = {"insert": {}, 'update': {}, 'delete': {}}
        file = os.path.join(os.path.dirname(os.path.abspath(__file__)),file)
        reader = csv.reader(open(file,'rb'))
        for row,row_data in enumerate(reader):
            if row == 0:
                self.header = row_data
                for col_index, col_name in enumerate(row_data):
                    self.key_col[col_index] = col_name
            else:
                action, data = _validate_row(row_data)   # 验证每一行数据，返回操作动作(插入，更新，错误)和数据
                result[action][(data[self.header.index(col_name)] for col_name in self.primary_key)] = data

        # 执行批量插入
        self._insert(result.get("insert",{}).values())
        # 执行批量更新
        self._update(result.get("update",{}).values())
        # 错误数据回写
        self._error(result.get("error",{}).values())

        return u"成功导入"

    def _validate_row(self, row_data):
        error = ""
        for col_index, data in enumerate(row_data):
            col_name = self.key_col.get(col_index,'')
            error += self._validate_data(col_name, data)
        if error == "":
            return "insert", row_data
        else:
            return "error", row_data+[error,]

    def _validate_data(self, data):
        return ""

    def _insert(self, datas):
        print datas

    def _update(self, datas):
        print datas

    def _error(self, datas):
        print datas

if __name__ == "__main__":
    reader = CsvReader()
    reader.read()