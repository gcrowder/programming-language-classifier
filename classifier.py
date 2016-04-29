from sklearn.datasets import load_files
from sklearn.cross_validation import train_test_split
from sklearn.feature_extraction.text import HashingVectorizer
from sklearn.naive_bayes import GaussianNB

dataset = load_files('data', load_content=True,
                     encoding='UTF-8', decode_error='replace')

docs_train, docs_test, y_train, y_test = train_test_split(
    dataset.data, dataset.target, test_size=0.4, random_state=30)

hv = HashingVectorizer()

train_code = hv.transform(docs_train)
test_code = hv.transform(docs_test)

# print("Train Row 2: {}.".format(train_code.getrow(2)))
# print("Test Row 2: {}".format(test_code.getrow(2)))

clf = GaussianNB
clf.fit(train_code, hv.transform(y_train))

print("Train Score: {}".format(clf.score(train_code, y_train)))
print("Test Score: {}".format(clf.score(test_code, y_test)))
