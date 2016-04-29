try:
    value = float(input("Enter a number: "))
except ValueError:
    print("That's not a number! Try again, please.")
else:
    print(value)
finally:
    print('program has completed')
