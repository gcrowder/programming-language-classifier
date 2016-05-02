import os
import argparse
from sklearn.datasets import load_files
from sklearn.cross_validation import train_test_split
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
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
    """ Check to see if pipeline can be loaded from pickle. If not, train
    pipeline. Return pipeline and list of target names."""
    if os.path.isfile('.pipeline.pkl'):
        keystone = joblib.load('.pipeline.pkl')
        target_names = load_files('data').target_names
    else:
        target_names, X_train, X_test, y_train, y_test = prepare_dataset()
        keystone = Pipeline([('vectorizer', CountVectorizer()),
                            ('tfidf', TfidfTransformer()),
                            ('classifier', linear_model.SGDClassifier())])
        keystone.fit(X_train, y_train)
        joblib.dump(keystone, '.pipeline.pkl', compress=1)
    return keystone, target_names


def predict_language(filename, keystone, target_names):
    """ Given a filename, a classifier pipeline and a list of target names:
    make a prediction based on a sample read in from file."""
    sample = read_test_file(filename)
    prediction = keystone.predict(sample)
    print("Prediction: {}".format(target_names[prediction[0]]))
    return target_names[prediction[0]]


def main(filename):
    """Get a pipeline and predict a language."""
    pipeline, target_names = prepare_pipeline()
    predict_language(filename, pipeline, target_names)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Classify programs.')
    parser.add_argument('filename', type=str, nargs='?', default='sample.py',
                        help='A filename for the classifier')
    args = parser.parse_args()
    main(args.filename)
