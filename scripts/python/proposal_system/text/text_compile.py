# This script compiles all the files in the current directory into a single
# python file 'text.py' with variable names equaling the first part of the 
# file name.

import os

def compile():
    files = os.listdir('.')

    text_file = open('../text.py', 'w')

    files.sort()
    for fname in files:
        if fname[-4:] == ".tpl":
            print fname[:-4]
            text_file.write(fname[:-4]+'=r"""')
            f = open(fname, 'r')
            text_file.write(f.read() + '"""')
            f.close()
            text_file.write('\n\n')

    text_file.close()

if __name__ == "__main__":
    compile()
