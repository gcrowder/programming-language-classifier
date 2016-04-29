import random
import pandas as pd
from sklearn.datasets import load_files
from sklearn.cross_validation import train_test_split
from sklearn.feature_extraction.text import HashingVectorizer
from sklearn.naive_bayes import MultinomialNB

dataset = load_files('data', load_content=True,
                     encoding='UTF-8', decode_error='replace')

seed = random.randint(0, 10000)

docs_train, docs_test, y_train, y_test = train_test_split(
    dataset.data, dataset.target, test_size=0.20, random_state=seed)

hv = HashingVectorizer(non_negative=True)

train_code = hv.transform(docs_train)
test_code = hv.transform(docs_test)

# print("Train Row 2: {}.".format(train_code.getrow(2)))
# print("Test Row 2: {}".format(test_code.getrow(2)))

clf = MultinomialNB()
clf.fit(train_code, y_train)
print("Seed: {}".format(seed))
print("MultinomialNB Train Score: {}".format(clf.score(train_code, y_train)))
print("MultinomialNB Test Score: {}".format(clf.score(test_code, y_test)))
