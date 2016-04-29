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

nums = [5,5,1,2,4,3]

print (median(nums))
