import random


def count_5_random_integers():
    return sum([random.randint(1, 101) for x in range(5)])


def main():
    print(count_5_random_integers)


if __name__ == '__main__':
    main()
