birthdays = np.random.randint(1, 366, size=23)

def birthday_sim(simulations, people):
    
    birthday_result = []
    
    for _ in range(simulations):
        birthdays = np.random.randint(1, 366, size=people)
        
        uniques, counts = np.unique(birthdays, return_counts=True)
        for x in counts:
            if x > 1:
                birthday_result.append(True)
                break
        #print(birthday_result)
            
    return len(birthday_result) / simulations