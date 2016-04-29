import math ## I didn't feel like writing a square root function.

def average(amount, items):
    return amount / items

def median(list_of_numbers):
    list_of_numbers.sort()
    if len(list_of_numbers) % 2 == 0:
        index1 = (len(list_of_numbers) // 2)
        index0 = index1 - 1
        average = (list_of_numbers[index0] + list_of_numbers[index1]) / 2
        return average
    else:
        index = len(list_of_numbers) // 2
        return list_of_numbers[index]

##if no number in the list occurs more than once, mode(numbers) doesn't work.
def mode(list_of_numbers):
    frequency = 0
    popular_number = 0.0
    for number in list_of_numbers:
        if list_of_numbers.count(number) > frequency:
            popular_number = number
            frequency = list_of_numbers.count(number)
    return popular_number

def std_dev(list_of_numbers):
    sum_of_deviations = 0.0
    avg = sum(list_of_numbers) / len(list_of_numbers)
    for number in list_of_numbers:
        sum_of_deviations = sum_of_deviations + ((number - avg)**2)
    variance = sum_of_deviations / len(list_of_numbers)
    return math.sqrt(variance)

def is_float(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


sum_total = 0.0
count = 0
words_or_nums = []

user_input = input("Please enter a number: ").lower()
if is_float(user_input):
    while True:
        try:
            float_input = float(user_input)
        except ValueError:
            if user_input == '': #test for newline, break if true
                break
            else:
                print(user_input, "is invalid input. Try again")
                user_input = input("Please enter a number: ").lower()
                continue
        else:
            words_or_nums.append(float_input)
            sum_total += float_input
            count += 1
        user_input = input("Please enter a number: ").lower()
    mean = average(sum_total, count)
    print('Count: ', count)
    print('Sum: ', sum_total)
    print('Average value of numbers: ', mean)
    print("Median: ", median(words_or_nums))
    print("Mode: ", mode(words_or_nums))
    print("Standard Deviation: ", std_dev(words_or_nums))

else:
    while True:
        if user_input == '':
            break
        elif is_float(user_input):
            print(user_input, "is invalid input. Try again")
            user_input = input("Please enter a string: ").lower()
            continue
        else:
            words_or_nums.append(user_input)
            count += 1
        user_input = input("Please enter a string: ").lower()

    print(''.join(words_or_nums))
    print('Number of strings: ', count)


    print("Count of letter 'e' in input: ", ''.join(words_or_nums).count('e'))


    #for word in words_or_nums:
        #min length of a string
        #max length of a string
        #average length of strings
