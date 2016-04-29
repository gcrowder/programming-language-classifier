import re


def words(string):
    words = re.findall(r'\w*[A-Za-z-]\w*', string)
    if len(words) < 1:
        return None
    else:
        return words


def phone_number(string):
    phone_num = re.search(r'(\d{3})\D*(\d{3})\D*(\d{4})\D*(\d*)$', string)
    if phone_num:
        area_code, prefix, number, extension = phone_num.groups()
        return {'area_code': area_code, 'number': '{}-{}'.format(prefix,
                number)}
    else:
        return None

# phone number ^(\d{3})\D*(\d{3})\D*(\d{4})\D*(\d*)$


def money(string):
    if len(string) == 1 or string[0] != '$' or string[1] == '$':
        return None
    mo_problems = re.search(r'((^\$\d{1,}\.\d{2})|(^\$\d{1,3},\d{3},\d{3}\.\d{2})|(^\$\d,\d{3}\.\d{2})|(^\$\d{1,3},\d{3},\d{3})|(^\$\d{1,3},\d{3})|(^\$\d{1,4}))', string)
    if len(string) == len(mo_problems.group(1)):
        dollas = mo_problems.group(1)
        return {'currency': '$', 'amount': float(dollas[1:].replace(',', ''))}
    else:
        return None


def zipcode(string):
    length = len(string)
    if length == 5 or length == 10:
        code = re.search(r'(^\d{5}-\d{4})|(^\d{5})', string)
        zip_plus_four, zip_code = code.groups()
        if zip_code:
            return {'zip': zip_code[:5], 'plus4': None}
        else:
            return {'zip': zip_plus_four[:5], 'plus4': zip_plus_four[6:10]}
    else:
        return None


def date(string):
    d_m_y = re.search(r'(\b\d{1,2})[-\/:](\d{1,2})[-\/:](\d{4}\b)', string)
    y_m_d = re.search(r'(\d{4})[-\/](\d{2})[-\/](\d{2})', string)
    if d_m_y:
        month, day, year = d_m_y.groups()
    elif y_m_d:
        year, month, day = y_m_d.groups()
    else:
        return None
    return {'month': int(month),
            'day': int(day),
            'year': int(year)}
