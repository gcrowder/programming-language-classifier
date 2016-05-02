# programming-language-classifier
This project is a software language classifier built with Python and scikit-learn. Install the dependencies to run the classifier.

## requirements.txt
This program is writen in Python 3.51. You must have installed Python 3.4+ to run it.
To ensure you have the necessary dependencies installed, run: `pip install -r requirements.txt`
## classifier.py
This classifier is familiar with 15 programming languages:
- c
- c#
- clojure
- common_lisp
- haskell
- java
- javascript
- ocaml
- perl
- php
- python
- ruby
- scala
- scheme
- tcl

From your command line, in the programming-language-classifier directory, run `python3 classifier.py filename`, where filename is the path to the file which you want to have classified.

## Classifier-Presentation.ipynb
This is the [presentation notebook](https://github.com/gcrowder/programming-language-classifier/blob/master/Classifier-Presentation.ipynb) that explains the machine learning algorithm behind classifier.py and compares it to another similar method.

## data
This directory contains the source material for classifier.py's machine learning algorithm.
If you wish to add more programs, just add programs to the directory named for the respective programming language. If no applicable directory is present, create one named after the programming language and place the program inside.

If you changed the data directory or its sub-directories in any way, and you have run classifier.py at least once before, *YOU MUST DELETE* .pipeline.pkl in order to force classifier.py to retrain the classifier. You may do this by running `rm .pipeline.pkl` from your command line in the programming-language-classifier directory.

## test_classifier.py
A python script to test the classifier.

## tests
A directory containing the test programs and a csv file containing the programs' target languages.

## sample.py
A sample python script to demonstrate the command line execution of classifier.py
