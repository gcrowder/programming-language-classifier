import pandas as pd


def read_in_data():
    """ Read in three 2014 ATUS data files to DataFrames. """
    activity = pd.read_csv('data/atusact_2014.dat')
    respondent = pd.read_csv('data/atusresp_2014.dat')
    roster = pd.read_csv('data/atusrost_2014.dat')
    return activity, respondent, roster


def get_codes(activity):
    """ Transform three Series from the Activity file into activity code. """
    codes_list = []
    for row in activity.itertuples():
        if row.TUTIER1CODE < 10:
            first = '0{}'.format(row.TUTIER1CODE)
        else:
            first = row.TUTIER1CODE
        if row.TUTIER2CODE < 10:
            second = '0{}'.format(row.TUTIER2CODE)
        else:
            second = row.TUTIER2CODE
        if row.TUTIER3CODE < 10:
            third = '0{}'.format(row.TUTIER3CODE)
        else:
            third = row.TUTIER3CODE
        codes_list.append('{}{}{}'.format(first, second, third))
    return codes_list


def get_minutes_subject(grouped, subject):
    """Sum up the minutes of a given activity if it's in a given subject."""
    minutes_dict = {}
    length = len(subject[0])
    for name, group in grouped:
        for row in group.itertuples():
            if row.codes[:length] in subject:
                if name in minutes_dict:
                    minutes_dict[name] = minutes_dict[name] + row.TUACTDUR24
                else:
                    minutes_dict[name] = row.TUACTDUR24
    return minutes_dict


def main():
    print('What are you running a library for???')

if __name__ == '__main__':
    main()
