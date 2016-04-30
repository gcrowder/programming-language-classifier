import random
import numpy as np
from sklearn.datasets import load_files
from sklearn.cross_validation import train_test_split
from sklearn.feature_extraction.text import HashingVectorizer, CountVectorizer
from sklearn.naive_bayes import MultinomialNB


def show_top10(classifier, vectorizer, categories):
    feature_names = np.asarray(vectorizer.get_feature_names())
    for i, category in enumerate(categories):
        top10 = np.argsort(classifier.coef_[i])[-10:]
        print("%s: %s" % (category, " ".join(feature_names[top10])))

dataset = load_files('data', load_content=True,
                     encoding='UTF-8', decode_error='replace')

seed = random.randint(0, 4294967295)

docs_train, docs_test, y_train, y_test = train_test_split(
    dataset.data, dataset.target, test_size=0.20, random_state=seed)

hv = HashingVectorizer(non_negative=True)
cv = CountVectorizer()

hash_train_code = hv.transform(docs_train)
hash_test_code = hv.transform(docs_test)

count_train_code = cv.fit_transform(docs_train)
count_test_code = cv.transform(docs_test)

# print("Train Row 2: {}.".format(train_code.getrow(2)))
# print("Test Row 2: {}".format(test_code.getrow(2)))

clf = MultinomialNB(alpha=1.50)
clf.fit(hash_train_code, y_train)
print("Seed: {}".format(seed))
print("MultinomialNB Hash Train Score: {}".format(clf.score(hash_train_code,
                                                            y_train)))
print("MultinomialNB Hash Test Score: {}".format(clf.score(hash_test_code,
                                                           y_test)))

clf.fit(count_train_code, y_train)
print("Seed: {}".format(seed))
print("MultinomialNB Count Train Score: {}".format(clf.score(count_train_code,
                                                             y_train)))
print("MultinomialNB Count Test Score: {}".format(clf.score(count_test_code,
                                                            y_test)))

show_top10(clf, cv, dataset.target_names)
