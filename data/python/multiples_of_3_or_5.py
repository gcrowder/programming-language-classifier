def multiples_of_3_or_5(nums):
    total = 0
    for number in nums:
        if number % 3 == 0 or number % 5 == 0:
            total += number
    return total
