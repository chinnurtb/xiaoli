# -*- coding: utf-8 -*-
import sys
import csv
from pyDes import CBC, PAD_PKCS5, triple_des

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print u'缺少参数，需要输入csv文件名和密钥,如：python generate_license.py license.csv cn=hunan,c=cn'
        sys.exit(1)
    csv_file = sys.argv[1]
    des_key = sys.argv[2]

    suffix = des_key.split(",")[0].split("=")[-1]
    reader = csv.reader(open(csv_file,'rb'))
    data = []
    for row in reader:
        data.append("=".join([value.decode('gbk') for value in row]))
    data = ",".join(data)
    if len(des_key) < 16:
        des_key += '\0'*(16-len(des_key))
    else:
        des_key = des_key[0:16]
    k = triple_des(des_key, CBC, "opengoss",pad=None, padmode=PAD_PKCS5)
    en_data = k.encrypt(data.encode('utf8'))
    with open("LICENSE."+ suffix, 'wb') as file:
        file.write(en_data)

    #de_data = k.decrypt(en_data)
    #for data in de_data.split(","):
    #    print data.split("=")[1]