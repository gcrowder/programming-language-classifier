import random

def is_ai():
    while True:
        opponent = input("\nChoose an opponent.\nPlay a person (1)\nPlay the computer (2)\nWhat's your choice?: ")
        if opponent == '2':
            return True
        if opponent == '1':
            return False
        else:
            print("That is not one of the options. Try again.")
            continue

def turn(sticks, player):
    print("{}, it's your turn!".format(player))
    while True:
        try:
            pick_up = int(input("You can pick up 1, 2, or 3 sticks. \nHow many do you want?: "))
            if pick_up > 3:
                print("\nThat's too many, don't be greedy!")
                continue
            elif pick_up > sticks:
                print("\nThere aren't that many sticks left, what are you trying to pull?")
                continue
            else:
                return pick_up
        except ValueError:
            print("\nThat's not even a number. Are you feeling ok?")
            continue

def stick_count(sticks, player):
    sticks = sticks - player
    return sticks

def show_board(sticks):
    print("\nThere are {} sticks on the board\n".format(sticks))

def is_end(sticks, player, player_name):
    if player == sticks:
        print("\n{}, you lose!".format(player_name))
        return True
    else:
        return False

def person_game_loop(sticks):
    while sticks > 0:
        show_board(sticks)
        player1 = turn(sticks, "Player1")
        if is_end(sticks, player1, "Player1"):
            return
        sticks = stick_count(sticks, player1)
        show_board(sticks)
        player2 = turn(sticks, "Player2")
        if is_end(sticks, player2, "Player2"):
            return
        sticks = stick_count(sticks, player2)

def ai_turn(sticks, ai_dict):
    print("It's the computer's turn.")
    while True:
        pick_up = random.choice(ai_dict[sticks])
        if sticks < pick_up:
            continue
        else:
            print("You can pick up 1, 2, or 3 sticks. \nHow many do you want?: {}".format(pick_up))
            return pick_up

def make_ai_memory(ai_dict, ai_choices, state):
    if state == 'WIN':
        for key in ai_choices:
            if key in ai_dict:
                ai_dict[key].append(ai_choices[key])
            else:
                ai_dict[key] = [ai_choices[key]]
        return ai_dict
    else:
        for key in ai_dict:
            if key in ai_choices:
                if ai_dict[key].count(ai_choices[key]) > 1:
                        ai_dict[key].remove(ai_choices[key])
        return ai_dict




def ai_game_loop(sticks, ai_dict):
    print(ai_dict)
    ai_choices = {}
    while sticks > 0:
        show_board(sticks)
        player1 = turn(sticks, "Player1")
        if is_end(sticks, player1, "Player1"):
            return ai_choices, 'WIN'
        sticks = stick_count(sticks, player1)
        show_board(sticks)
        ai = ai_turn(sticks, ai_dict)
        ai_choices[sticks] = ai
        if is_end(sticks, ai, "Computer"):
            return ai_choices, 'LOSE'
        sticks = stick_count(sticks, ai)

def play_again():
    again = input("\nWould you like to play again? Y/n ").lower()
    return again == 'y'

def make_ai_dict():
    ai_dict = {}
    for stick in range(1, 30):
        ai_dict[stick] = [1, 2, 3]
    return ai_dict

def main():
    again = True
    ai_dict = make_ai_dict()
    ai_info = {}
    state = ''
    while again:
        sticks = random.choice(range(5, 30))
        if is_ai():
            ai_dict = make_ai_memory(ai_dict, ai_info, state)
            ai_info, state = ai_game_loop(sticks, ai_dict)
        else:
            person_game_loop(sticks)
        again = play_again()

if __name__ == '__main__':
    main()
