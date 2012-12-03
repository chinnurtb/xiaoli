# -*- coding: utf-8 -*-
import os
from pyDes import CBC, PAD_PKCS5, triple_des

def decrypt_license(des_key):
    suffix = des_key.split(",")[0].split("=")[-1]
    if len(des_key) < 16:
        des_key += '\0'*(16-len(des_key))
    else:
        des_key = des_key[0:16]
    k = triple_des(des_key, CBC, "opengoss",pad=None, padmode=PAD_PKCS5)
    with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "LICENSE."+ suffix), 'rb') as file:
        en_data = ''.join(file.readlines())
    de_data = k.decrypt(en_data)
    license_dict = {}
    for data in de_data.split(","):
        license_dict[data.split("=")[0]] = data.split("=")[-1].decode('utf8')
    return license_dict

if __name__ == "__main__":
    print decrypt_license("cn=hunan,c=cn")