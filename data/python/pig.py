import random
import statistics as st
import matplotlib.pyplot as plt


class Player:
    def __init__(self):
        self.total = 0
        self.last_roll = 0

    def is_roll_again(self, turn):
        return False


class EagerPlayer(Player):
    def is_roll_again(self, turn):
        if self.last_roll == 1:
            return False
        elif turn == 0 or self.total < 10:
            return True
        elif self.total / turn < 10:
            return True
        else:
            return False


class ArbitraryPlayer(Player):
    def is_roll_again(self, turn):
        if self.last_roll == 1:
            return False
        else:
            return random.choice([True, False])


class OverlyCautiousPlayer(Player):
    def is_roll_again(self, turn):
        if self.last_roll == 1:
            return False
        elif turn == 0 or self.total < 4:
            return True
        elif self.total / turn > 4:
            return False
        else:
            return True


def roll_die():
    return random.randint(1, 6)


def play(player):
    total = player.total
    roll = roll_die()
    player.last_roll = roll
    if roll == 1:
        return total
    else:
        total += roll
        return total


def game_loop(player):
    result = 0
    for turn in range(7):
        result = play(player)
        player.total = result
        while player.is_roll_again(turn):
            result = play(player)
            player.total = result
    return player.total


def main():
    trials = 1000
    player_class_trials = []
    for _ in range(trials):
        bob = Player()
        player_class_trials.append(game_loop(bob))

    print("Player Mean: ", st.mean(player_class_trials))
    print("Player StDev: ", st.stdev(player_class_trials))
    plt.figure(1)
    plt.boxplot(player_class_trials)
    plt.ylabel('Game Score')
    plt.show()
    eager_player_trials = []
    for _ in range(trials):
        sally = EagerPlayer()
        eager_player_trials.append(game_loop(sally))
    print("Eager Player Mean: ", st.mean(eager_player_trials))
    print("Eager Player StDev: ", st.stdev(eager_player_trials))
    plt.figure(2)
    plt.boxplot(eager_player_trials)
    plt.ylabel('Game Score')
    plt.show()
    arbitrary_player_trials = []
    for _ in range(trials):
        june = ArbitraryPlayer()
        arbitrary_player_trials.append(game_loop(june))
    print("Arbitrary Player Mean: ", st.mean(arbitrary_player_trials))
    print("Arbitrary Player StDev: ", st.stdev(arbitrary_player_trials))
    plt.figure(3)
    plt.boxplot(arbitrary_player_trials)
    plt.ylabel('Game Score')
    plt.show()
    overly_cautious_trials = []
    for _ in range(trials):
        melvin = OverlyCautiousPlayer()
        overly_cautious_trials.append(game_loop(melvin))
    print("Overly Cautious Player Mean: ", st.mean(overly_cautious_trials))
    print("Overly Cautious Player StDev: ", st.stdev(overly_cautious_trials))
    plt.figure(4)
    plt.boxplot(overly_cautious_trials)
    plt.ylabel('Game Score')
    plt.show()

if __name__ == '__main__':
    main()
