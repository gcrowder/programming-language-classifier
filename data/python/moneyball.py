import pandas as pd

appearances = pd.read_csv('data/baseballdatabank-master/core/Appearances.csv')

places = appearances[['yearID', 'teamID', 'playerID', 'G_p', 'G_c',
                      'G_1b', 'G_2b', 'G_3b', 'G_ss', 'G_lf',
                      'G_cf', 'G_rf', 'G_of', 'G_dh']].copy()


def key_with_max_value(dictionary):
    """ a) create a list of the dict's keys and values;
        b) return the key with the max value
        From stack overflow: getting-key-with-maximum-value-in-dictionary"""
    values = list(dictionary.values())
    keys = list(dictionary.keys())
    return keys[values.index(max(values))]


def find_position(places):
    played_at = []
    for row in places.itertuples():
        spots = {'pitcher': row.G_p,
                 'catcher': row.G_c,
                 'first_base': row.G_1b,
                 'second_base': row.G_2b,
                 'third_base': row.G_3b,
                 'short_stop': row.G_ss,
                 'left_field': row.G_lf,
                 'center_field': row.G_cf,
                 'right_field': row.G_rf,
                 'out_field': row.G_of,
                 'designated hitter': row.G_dh}
        played_at.append(key_with_max_value(spots))
    return played_at


def on_base_percentage(hits, walks, hit_by_pitch, at_bats, sacrifice_flies):
    numerator = hits + walks + hit_by_pitch
    denominator = at_bats + walks + hit_by_pitch + sacrifice_flies
    return numerator / denominator
