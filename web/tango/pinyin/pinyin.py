# -*- coding: utf-8 -*-
import os

def load_word():
    word_dict = {}
    with open(os.path.join(os.path.dirname(os.path.abspath(__file__)),'word.data'), 'r') as file:
        for f_line in file.readlines():
            try:
                line = f_line.split('    ')
                word_dict[line[0]] = line[1]
            except:
                line = f_line.split('   ')
                word_dict[line[0]] = line[1]
    return word_dict
            
def pinyin(chinese):
    word_dict = load_word()
    result = []
    if not isinstance(chinese, unicode):
        chinese = chinese.decode("utf-8")
    for char in chinese:
        p = ord(char)
        key = '%X' % p
        value = word_dict.get(key)
        if value:
            result.append(value.split()[0][:-1].lower())
        else:
            result.append(char)
    return result

if __name__ == "__main__":
    str = pinyin(u'-11托尔adg！#！b斯a泰1')
    print str
