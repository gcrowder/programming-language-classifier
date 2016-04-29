def mode(list_of_numbers):
    frequency = 0
    popular_number = 0.0
    for number in list_of_numbers:
        if list_of_numbers.count(number) > frequency:
            popular_number = number
            frequency = list_of_numbers.count(number)
    return popular_number


nums = [1,2,3,4,4,4,4,2,1,0,0,0,0,0,0]

print("Mode: ", mode(nums))
