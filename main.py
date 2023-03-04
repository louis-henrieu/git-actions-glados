import dis

def add(x, y):
    return x + y

def sub(x, y):
    return x - y

def call(x):
    return add(x, 5) + sub(x, 5)

def z(x, y):
    return x + y

def hello ():
    x = add(23, 3)

dis.dis(hello)