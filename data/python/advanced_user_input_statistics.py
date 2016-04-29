def average(amount, items):
    return amount / items

def is_float(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


sum_total = 0.0
count = 0
words = ''


user_input = input("Please enter a number: ")
if is_float(user_input):
    while True:
        try:
            float_input = float(user_input)
        except ValueError:
            if user_input == '': #test for newline, break if true
                break
            else:
                print(user_input, "is invalid input. Try again")
                user_input = input("Please enter a number: ")
                continue
        else:
            sum_total += float_input
            count += 1
        user_input = input("Please enter a number: ")
    mean = average(sum_total, count)
    print('Count: ', count)
    print('Sum: ', sum_total)
    print('Average value of numbers: ', mean)
else:
    while True:
        if user_input == '':
            break
        elif is_float(user_input):
            print(user_input, "is invalid input. Try again")
            user_input = input("Please enter a string: ")
            continue
        else:
            words = words + user_input
        user_input = input("Please enter a string: ")

    print(words)
