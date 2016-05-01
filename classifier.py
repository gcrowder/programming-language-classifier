import random
import numpy as np
from scipy import stats
from sklearn.datasets import load_files
from sklearn.cross_validation import train_test_split
from sklearn.feature_extraction.text import HashingVectorizer, TfidfTransformer, CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.pipeline import Pipeline


def show_top10(classifier, vectorizer, categories):
    feature_names = np.asarray(vectorizer.get_feature_names())
    for i, category in enumerate(categories):
        top10 = np.argsort(classifier.coef_[i])[-10:]
        print("%s: %s" % (category, " ".join(feature_names[top10])))

with open('Markdown.pl', 'r') as f:
    sample = [f.read()]


dataset = load_files('data', load_content=True,
                     encoding='UTF-8', decode_error='replace')
# target_dict = {key: value for key, value in enumerate(dataset.target_names)}
# [target_dict[x] for x in dataset.target]
# print("Target Dict: {}".format(target_dict))
# print(dataset.target)
# print([target_dict[x] for x in dataset.target])
print("Number of Filenames: {}\nNumber of Targets: {}".format(len(dataset.filenames), len(dataset.target)))
seed = random.randint(0, 4294967295)

docs_train, docs_test, y_train, y_test = train_test_split(
    dataset.data, dataset.target, test_size=0.20, random_state=seed)


hv = HashingVectorizer(non_negative=True)
cv = CountVectorizer()
#td = TfidfTransformer()

hash_train_code = hv.transform(docs_train)
hash_test_code = hv.transform(docs_test)

# td.fit(hash_train_code)
# transformed_hash_train_code = td.transform(hash_train_code)
# transformed_hash_test_code = td.transform(hash_test_code)
count_train_code = cv.fit_transform(docs_train)
count_test_code = cv.transform(docs_test)
# print("Number of features: ".format(cv.get_feature_names()))
# print("Train Row 2: {}.".format(train_code.getrow(2)))
# print("Test Row 2: {}".format(test_code.getrow(2)))
#td.fit(count_train_code)
#transformed_count_train_code = td.transform(count_train_code)
#transformed_count_test_code = td.transform(count_test_code)

# hash_clf = MultinomialNB(alpha=.9)
# hash_clf.fit(td.transform(transformed_hash_train_code), y_train)
# print("Seed: {}".format(seed))
# print("MultinomialNB Hash Train Score: {}".format(hash_clf.score(
#                                                   transformed_hash_train_code, y_train)))
# print("MultinomialNB Hash Test Score: {}".format(hash_clf.score(transformed_hash_test_code,
#                                                  y_test)))
clf = MultinomialNB(alpha=1.35)
clf.fit(count_train_code, y_train)
print("Seed: {}".format(seed))
print("MultinomialNB Count Train Score: {}".format(clf.score(count_train_code,
                                                             y_train)))
print("MultinomialNB Count Test Score: {}".format(clf.score(count_test_code,
                                                            y_test)))
show_top10(clf, cv, dataset.target_names)

print("Target: {}".format(dataset.target))
print("Target Names: {}".format(dataset.target_names))

transformed_sample = cv.transform(sample)
print("Transformed Sample: ")
print(transformed_sample.toarray())
prediction = clf.predict(transformed_sample)
print(prediction)
print("Prediction: {}".format(dataset.target_names[prediction]))
# mode, count = stats.mode(prediction)
# print("Target Category: {}".format(mode))
# print("Number of features: ".format(cv.get_feature_names()))
