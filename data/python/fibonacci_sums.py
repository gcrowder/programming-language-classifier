first_number = 1
second_number = 2
total_even = 2
third_number = 0
while third_number < 10e100:
    # print("Before add: ", third_number)
    third_number = first_number + second_number
    # print("After add: ", third_number)
    if third_number > 10e100:
        break
    elif third_number % 2 == 0:
        total_even += third_number
    first_number = second_number
    second_number = third_number


print(total_even)
