import dis

def add(x, y):
    z = 4
    return x + y

def sub(x, y):
    return x - y

def call(x):
    return add(x, 5) + sub(x, 5)

def z(x, y):
    z = 5.6
    round (z)

if __name__ == "__main__":
    dis.dis(z)