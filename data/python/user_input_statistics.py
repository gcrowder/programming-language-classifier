def average(amount, items):
    return amount / items

sum_total = 0.0
count = 0



while True:
    user_input = input("Please enter a number: ")
    try:
        float_input = float(user_input)
    except ValueError:
        if user_input == '': #test for newline, break if true
            break
        else:
            print(user_input, "is invalid input. Try again")
            continue
    else:
        sum_total += float_input
        count += 1

mean = average(sum_total, count)
print('Count: ', count)
print('Sum: ', sum_total)
print('Average value of numbers: ', mean)
