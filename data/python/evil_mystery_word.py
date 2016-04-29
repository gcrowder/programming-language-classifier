import random
import os
from collections import Counter

def greeting():
    print("Hello! Shall we play an eeeeevil word game?")
    print("Mwuahahahahaha!")
    print("I will pick a word and you will try to guess it one letter at a time.")

def get_difficulty():
    while True:
        difficulty = input("How difficult should this game be? Would you like [E]asy, [N]ormal, or [H]ard? ").lower()
        if difficulty == 'e' or difficulty == 'easy':
            return 'EASY'
        elif difficulty == 'n' or difficulty == 'normal':
            return 'NORMAL'
        elif difficulty == 'h' or difficulty == 'hard':
            return 'HARD'
        else:
            print("What was that? I didn't quite understand you. Let's try again.")


#get words file
def get_words(path='/usr/share/dict/words'):
    list_of_words = []
    with open(path, 'r') as words:
        for line in words:
            word = line.strip()
            list_of_words.append(word)
    return list_of_words

#difficulty level: easy (4-6 character words), normal(6-8 character words), hard (8+ character words)
def make_appropriate_difficulty(word_list, difficulty='EASY'):
    dictionary_of_pain = {'EASY': (3,7), 'NORMAL': (5,9), 'HARD': (7,25)}
    appropriate_difficulty = []
    for word in word_list:
            if len(word) > dictionary_of_pain[difficulty][0] and len(word) < dictionary_of_pain[difficulty][1]:
                appropriate_difficulty.append(word)
    return appropriate_difficulty

#tell/show user the length of the mystery_word
def show_word(mystery_list, guesses):
    word = []
    for letter in mystery_list[0]:
        if letter in guesses:
            word.append(letter)
        else:
            word.append('_')
    print(' '.join(word))
    return word #for unit testing purposes

def is_guessed(mystery_list):
    return len(mystery_list) == 1


#user guesses one letter per round. one letter input only, must be a character.
def get_guess(guesses):
    guess = 'guess'
    while len(guess) > 1 or not guess.isalpha():
        print("Guesses must be a single letter.")
        guess = str(input("Please enter your guess: "))
        if guess in guesses:
            print("You already tried that guess. Try again.")
            guess = 'guess'
            continue
    return guess

def get_evil_dict(word_list, guesses):
    evil_dict = {}
    letter = str(guesses[:-1])
    for word in word_list:
        if letter in word:
            evil_dict[word] = word.index(letter)
        else:
            evil_dict[word] = -1
    return evil_dict

def pick_evil_list(dictionary):
    most_common_letter_index = Counter(dictionary).most_common(1)[0][1]
    smaller_word_list = []
    for key in dictionary:
        if dictionary[key] == most_common_letter_index:
            smaller_word_list.append(key)
    return smaller_word_list


def game_logic(word_list):
    greeting()
    guesses = []
    difficulty = get_difficulty()
    appropriate_difficulty = make_appropriate_difficulty(word_list, difficulty)
    mystery_dict = get_evil_dict(appropriate_difficulty, guesses)
    mystery_list = pick_evil_list(mystery_dict)
    sys = os.system('clear')
    show_word(mystery_list, guesses)
    print()
    missed_guess_list = []
    while len(missed_guess_list) < 8:
        print("Guesses remaining: ", (8 - len(missed_guess_list)))
        guesses.append(get_guess(guesses))
        if is_guessed(mystery_list):
            sys = os.system('clear')
            show_word(mystery_list, guesses)
            print("My word was: ", ''.join(mystery_list))
            return 'WIN'
        else:
            if guesses[-1] not in mystery_list:
                missed_guess_list.append(guesses[-1])
        #print("Mystery Word: ", mystery_word, "guesses: ", guesses, "Wrong letters: ", missed_guess_list) #print debug
        sys = os.system('clear')
        show_word(mystery_list, guesses)
        print("Wrong letters: ", ' '.join(missed_guess_list))
        mystery_dict = get_evil_dict(mystery_list, guesses)
        mystery_list = pick_evil_list(mystery_dict)

    print("My word was: ", mystery_list[0])
    return 'LOSE'

def result(state):
    if state == 'WIN':
        print("Hurray! You win!")
        return "Hurray! You win!" #for unit test purposes
    else:
        print("I'm sorry. You failed to succeed. Perhaps watch more Wheel of Fortune?")

def is_play_again():
    again = input("Play again? Y/n? ").lower()
    if again == 'y' or again == 'yes':
        print("Here we go!")
        return True
    else:
        print("Goodbye.")
        return False



def main():
    all_the_words = get_words()
    again = True
    while again:
        sys = os.system('clear')
        state = game_logic(all_the_words)
        result(state)
        again = is_play_again()



if __name__ == '__main__':
    main()
