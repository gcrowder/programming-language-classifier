# game of sticks
import random


def turn(sticks, player):
    # tell the player its their turn
    print("\n{}, it's your turn!".format(player))

    # limit to 1-3 sticks
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
    # pass to next player



def stick_count(sticks, player):
    # remove sticks collected by each player
    sticks = sticks - player
    return sticks

def show_board(sticks):
    print("\nThere are {} sticks on the board".format(sticks))

def is_end(sticks, player):
    if player == sticks:
        print("\nYou lose!")
        return True
    else:
        return False

def game_loop(sticks):
    while sticks > 0:
        show_board(sticks)
        player1 = turn(sticks, "Player1")
        if is_end(sticks, player1):
            return
        sticks = stick_count(sticks, player1)
        show_board(sticks)
        player2 = turn(sticks, "Player2")
        if is_end(sticks, player2):
            return
        sticks = stick_count(sticks, player2)


def play_again():
    again = input("\nWould you like to play again? Y/n ").lower()
    return again == 'y'

def show_result(state):
    if state:
        print("I'm sorry, you lost")

def main():
    # get number of sticks for game
    again = True
    while again:
        sticks = random.choice(range(1, 30))
        state = game_loop(sticks)
        again = play_again()


if __name__ == '__main__':
    main()
