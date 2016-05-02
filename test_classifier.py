from classifier import prepare_pipeline, predict_language

pipe, target_names = prepare_pipeline()


def test_file_one():
    assert predict_language('tests/1', pipe, target_names) == 'clojure'


def test_file_two():
    assert predict_language('tests/2', pipe, target_names) == 'clojure'


def test_file_three():
    assert predict_language('tests/3', pipe, target_names) == 'clojure'


def test_file_four():
    assert predict_language('tests/4', pipe, target_names) == 'clojure'


def test_file_five():
    assert predict_language('tests/5', pipe, target_names) == 'python'


def test_file_six():
    assert predict_language('tests/6', pipe, target_names) == 'python'


def test_file_seven():
    assert predict_language('tests/7', pipe, target_names) == 'python'


def test_file_eight():
    assert predict_language('tests/8', pipe, target_names) == 'python'


def test_file_nine():
    assert predict_language('tests/9', pipe, target_names) == 'javascript'


def test_file_ten():
    assert predict_language('tests/10', pipe, target_names) == 'javascript'


def test_file_eleven():
    assert predict_language('tests/11', pipe, target_names) == 'javascript'


def test_file_twelve():
    assert predict_language('tests/12', pipe, target_names) == 'javascript'


def test_file_thirteen():
    assert predict_language('tests/13', pipe, target_names) == 'ruby'


def test_file_fourteen():
    assert predict_language('tests/14', pipe, target_names) == 'ruby'


def test_file_fifteen():
    assert predict_language('tests/15', pipe, target_names) == 'ruby'


def test_file_sixteen():
    assert predict_language('tests/16', pipe, target_names) == 'haskell'


def test_file_seventeen():
    assert predict_language('tests/17', pipe, target_names) == 'haskell'


def test_file_eighteen():
    assert predict_language('tests/18', pipe, target_names) == 'haskell'


def test_file_nineteen():
    assert predict_language('tests/19', pipe, target_names) == 'scheme'


def test_file_twenty():
    assert predict_language('tests/20', pipe, target_names) == 'scheme'


def test_file_twentyone():
    assert predict_language('tests/21', pipe, target_names) == 'scheme'


def test_file_twentytwo():
    assert predict_language('tests/22', pipe, target_names) == 'java'


def test_file_twentythree():
    assert predict_language('tests/23', pipe, target_names) == 'java'


def test_file_twentyfour():
    assert predict_language('tests/24', pipe, target_names) == 'scala'


def test_file_twentyfive():
    assert predict_language('tests/25', pipe, target_names) == 'scala'


def test_file_twentysix():
    assert not predict_language('tests/26', pipe, target_names) == 'tcl'


def test_file_twentyseven():
    assert not predict_language('tests/27', pipe, target_names) == 'tcl'


def test_file_twentyeight():
    assert predict_language('tests/28', pipe, target_names) == 'php'


def test_file_twentynine():
    assert predict_language('tests/29', pipe, target_names) == 'php'


def test_file_thirty():
    assert predict_language('tests/30', pipe, target_names) == 'php'


def test_file_thirtyone():
    assert predict_language('tests/31', pipe, target_names) == 'ocaml'


def test_file_thirtytwo():
    assert predict_language('tests/32', pipe, target_names) == 'ocaml'
