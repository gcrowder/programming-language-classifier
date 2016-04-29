import statistics

all_the_numbers_list = []
all_the_numbers_count = 0
all_the_numbers_sum = 0
all_the_words = ""
all_the_words_count = 0
all_the_word_lengths_list = []
e_counter = 0

while True:
    input_number = input("Enter a number or a word: ")
    if input_number == "":
        if all_the_numbers_count > 0:
            print("Count: {}".format(all_the_numbers_count))
            print("Sum: {}".format(all_the_numbers_sum))
            print("Mean: {}".format(all_the_numbers_sum / all_the_numbers_count))
            print("Median: {}".format(statistics.median(all_the_numbers_list)))
            try:
                statistics.mode(all_the_numbers_list)
                print("Mode: {}".format(statistics.mode(all_the_numbers_list)))
            except:
                print("Mode: N/A all equally common values")
            print("Standard Deviation: {}".format(statistics.stdev(all_the_numbers_list)))
        else:
            print(all_the_words)
            print("You entered {} word(s).".format(all_the_words_count))
            all_the_word_lengths_list.sort()
            print("Minimum word length: {}".format(all_the_word_lengths_list[0]))
            print("Maximum word length: {}".format(all_the_word_lengths_list[-1]))
            print("Average word length: {}".format(statistics.mean(all_the_word_lengths_list)))
            for letter in all_the_words:
                if letter == 'e':
                    e_counter += 1
            print("There are {} e's in your string.".format(e_counter))
        break
    else:
        try:
            float(input_number)
        except:
            if all_the_numbers_count == 0:
                all_the_words += input_number
                all_the_words_count += 1
                all_the_word_lengths_list.append(len(input_number))
            else:
                print("All words or all numbers please! Start over")
                break
            continue
        if all_the_words == "":
            all_the_numbers_sum += float(input_number)
            all_the_numbers_count += 1
            all_the_numbers_list.append(float(input_number))
        else:
            print("All words or all numbers please! Start over")
            break
