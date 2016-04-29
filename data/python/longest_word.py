def open_file(path='/usr/share/dict/words'):
    with open(path, 'r') as words:
        longest_word = {}
        length = 0
        for line in words:
            word = line.strip()
            if len(word) > length:
                length = len(word)
    return length
