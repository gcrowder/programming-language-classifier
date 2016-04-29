class Stack:

    def __init__(self, capacity):
        self.capacity = capacity
        self.stack = []

    def is_empty(self):
        return len(self.stack) == 0

    def push(self, value):
        self.stack.append(value)

    def peek(self):
        return self.stack[-1]

    def len(self):
        return len(self.stack)
