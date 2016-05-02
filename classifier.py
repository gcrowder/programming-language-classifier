import os
from sklearn.datasets import load_files
from sklearn.cross_validation import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
# from sklearn.naive_bayes import MultinomialNB
# 'classifier', MultinomialNB(alpha=1.25))
from sklearn import linear_model
from sklearn.pipeline import Pipeline
from sklearn.externals import joblib


def read_test_file(filename):
    """Pass in filename string and return contents in a list."""
    with open(filename, 'r') as f:
        program = [f.read()]
    return program


def prepare_dataset():
    """Load data files from directory, split into train and test.
    Return lists of target names, training samples, testing samples,
    training targets and testing targets. """
    # print("Step 1")
    dataset = load_files('data', load_content=True,
                         encoding='UTF-8', decode_error='replace')
    # print("Step 2")
    X_train, X_test, y_train, y_test = train_test_split(
        dataset.data, dataset.target, test_size=0.20, random_state=3114795823)
    # print("Step 3")
    return dataset.target_names, X_train, X_test, y_train, y_test


def prepare_pipeline():
    target_names, docs_train, docs_test, y_train, y_test = prepare_dataset()
    if os.path.isfile('.pipeline.pkl'):
        keystone = joblib.load('.pipeline.pkl')
    else:
        keystone = Pipeline([('vectorizer', CountVectorizer()),
                            ('classifier', linear_model.SGDClassifier())])
        keystone.fit(docs_train, y_train)
        joblib.dump(keystone, '.pipeline.pkl', compress=1)
    return keystone, target_names


def predict_language(filename, keystone, target_names):
    sample = read_test_file(filename)
    # print("Step 8")
    prediction = keystone.predict(sample)
    print(prediction)
    print("Prediction: {}".format(target_names[prediction[0]]))
    return target_names[prediction[0]]


def main():
    pipeline, target_names = prepare_pipeline()
    print("Markdown.pl is written in Perl: ")
    predict_language('Markdown.pl', pipeline, target_names)
    print("sample.py is written in python: ")
    predict_language('sample.py', pipeline, target_names)

if __name__ == '__main__':
    main()
