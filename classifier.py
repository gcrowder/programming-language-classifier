from sklearn.datasets import load_files
from sklearn.cross_validation import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.pipeline import Pipeline


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


def predict_language(filename):
    # print("Step 0")
    target_names, docs_train, docs_test, y_train, y_test = prepare_dataset()
    # print("Step 4")
    keystone = Pipeline([('vectorizer', CountVectorizer()),
                        ('classifier', MultinomialNB(alpha=1.25))])
    # print("Step 5")
    keystone.fit(docs_train, y_train)
    # print("Step 6")
    # train_score = keystone.score(docs_train, y_train)
    # print("MultinomialNB Count Train Score: {}".format(train_score))
    # test_score = keystone.score(docs_test, y_test)
    # print("MultinomialNB Count Test Score: {}".format(test_score))

    sample = read_test_file(filename)
    # print("Step 8")
    prediction = keystone.predict(sample)
    print(prediction)
    print("Prediction: {}".format(target_names[prediction[0]]))
    return target_names[prediction[0]]


def main():
    print("Markdown.pl is written in Perl: ")
    predict_language('Markdown.pl')
    print("sample.py is written in python: ")
    predict_language('sample.py')

if __name__ == '__main__':
    main()
