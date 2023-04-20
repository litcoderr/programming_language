
def some_lambda():
    x = 3
    return lambda y: x+y

# should print out 10
print(some_lambda()(7))

x = 0

# should also print out 10
print(some_lambda()(7))

