import random

secret_num = random.choice(range(1,101)) #this is advanced stuff, we don't need no random.randint
rounds = 0
guessed_list = []
hints = []

def greeting():
    print("Shall we play a game? Guess my secret number!")
    print("You'll have 5 chances to guess.")


def get_guess():
    while True:
        raw_guess = input("Enter an integer from 1-100: ")
        try:
            guess = int(raw_guess)
        except:
            print("That's not even a number! Try again.")
            continue
        else:
            if guess <=0 or guess > 100:
                print("Hey, a number from 1-100. No higher, no lower.")
                print("Try again")
                continue
            else:
                break
    return guess


def is_close(number):
    return abs(number - secret_num) <= 5

def close(number):
    if is_close(number):
        print("But you're really close!")

def waste_guess():
    if len(guessed_list) == 1:
        return
        # if the last two hints are the same
    elif hints[-1] == hints[-2] and abs(guessed_list[-1] - secret_num) > abs(guessed_list[-2] - secret_num):
        print("I already gave you that hint. Way to waste a guess.")

def game_logic(number):
        if number == secret_num:
            print("Congratulations! You win!")
            guessed_list.append(number)
            return 'WIN'
        elif number in guessed_list:
            print("Really? You tried {} again? Lame.".format(number))
            print("That still counts as a guess!\n")
            guessed_list.append(number)
            return 'LOSE'
        else:
            guessed_list.append(number)
            if number < secret_num:
                print("You guessed too low")
                close(number)
                hints.append('LOW')
                waste_guess()
                return 'LOSE'
            else:
                print("You guessed too high")
                close(number)
                hints.append('HIGH')
                waste_guess()
                return 'LOSE'

def end_game(game_state):
    if game_state == 'WIN':
        print("You guessed {} and guess what? I also guessed {}. We must be special.".format(user_guess, secret_num))
        print("It took you {} tries.".format(rounds))
    else:
        print("Ooooh, sorry. You only get the {} attempts. Better luck next time.".format(rounds))
        print("The number was {} by the way.".format(secret_num))

#body of the program
greeting()
while rounds <5:
    user_guess = get_guess()
    state = game_logic(user_guess)
    rounds += 1
    if state == 'WIN':
        break
end_game(state)
