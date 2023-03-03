#!/usr/bin/env python3.8

import importlib as IL
import sys
import dis

byte_list = []

def main():
    if len(sys.argv) == 2:
        d = open(sys.argv[1], "rb")
        header = IL.util.MAGIC_NUMBER
        all = d.read()
        if all.startswith(header):
            print ("GOOD FORMAT")
            print (all)
        else:
            print ("BAD FORMAT: was waiting for")
            print (IL.util.MAGIC_NUMBER)
            print("at the begining of the file")
            print (all)
        print(dis.dis(all))
        return 0
    print("ERROR: NO FILE FOUND")
    return 84

if __name__ == '__main__':
    sys.exit(main())
