import random

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

secret_num = random.randint(1,100)
rounds = 0
guessed_list = []

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
                return 'LOSE'
            else:
                print("You guessed too high")
                return 'LOSE'


greeting()
while rounds <5:
    user_guess = get_guess()
    state = game_logic(user_guess)
    if state == 'WIN':
        break
    else:
        rounds += 1

if state == 'WIN':
    print("You guessed {} and guess what? I also guessed {}. We must be special.".format(user_guess, secret_num))
    print("It took you {} tries.".format(rounds))
else:
    print("Ooooh, sorry. You only get the {} attempts. Better luck next time.".format(rounds))
    print("The number was {} by the way.".format(secret_num))
