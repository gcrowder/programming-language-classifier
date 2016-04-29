import re


def binary(string):
    return re.match(r'^[0,1]+$', string)


def binary_even(string):
    if binary(string):
        return not int(string[-1])
    else:
        return False


def hex(string):
    return re.match(r'^[0-9, A-F]+$', string)


def word(string):
    return re.match(r'^\w*[A-Za-z-]\w*$', string)


def words(string, count=''):
    words = re.findall(r'\w*[A-Za-z-]\w*', string)
    if type(count) == int:
        return words and len(words) == count
    else:
        return words


def phone_number(string):
    return re.search(r'(\d{3})\D*(\d{3})\D*(\d{4})\D*(\d*)$', string)


def money(string):
    return re.search(r'^((^\$\d{1,}\.\d{2})|(^\$\d{1,3},\d{3},\d{3}\.\d{2})|(^\$\d,\d{3}\.\d{2})|(^\$\d{1,3},\d{3},\d{3})|(^\$\d{1,3},\d{3})|(^\$\d{1,4}))+$', string)


def zipcode(string):
    return re.search(r'^(^\d{5}-\d{4})|(^\d{5})+$', string)


def date(string):
    d_m_y = re.search(r'(\b\d{1,2})[-\/:](\d{1,2})[-\/:](\d{4}\b)', string)
    y_m_d = re.search(r'(\d{4})[-\/](\d{2})[-\/](\d{2})', string)
    return d_m_y or y_m_d
