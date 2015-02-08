#!/usr/bin/env python

import sys
import macropy.activate
import json
import argparse
import os.path
import subprocess

import parser
from dumper import Dumper
import iemit
import ia
from x86_32 import Backend
import util

if __name__ == '__main__':
    ap = argparse.ArgumentParser(description='Python C Compiler')
    ap.add_argument('-a', '--ast', action='store_true',
            help='Print the abstract syntax tree')
    ap.add_argument('-i', '--intermediate', action='store_true',
            help='Print the intermediate representation')
    ap.add_argument('-x', '--no-assembly', action='store_true',
            help='Do not emit backend assembly')
    ap.add_argument('-s', '--assembly', action='store_true',
            help='Emit assembly language and stop')
    ap.add_argument('files', nargs='*', default=[],
            help='Files to compile')

    args = ap.parse_args()
    for filename in args.files:
        p = subprocess.Popen(['cpp', '-undef', filename], stdout=subprocess.PIPE)
        f = p.stdout
        ast = parser.parse(f.read())
        f.close()


        if args.ast:
            with open(util.rename(filename, 'ast'), 'w') as af:
                print >>af, ast

        intermediate = iemit.emit(ast)
        if args.intermediate:
            with open(util.rename(filename, 'i'), 'w') as ir:
                for r in intermediate:
                    if isinstance(r, ia.Label):
                        print >>ir, r
                    else:
                        print >>ir, '   ', r

        if not args.no_assembly:
            backend = Backend(intermediate)
            asm = backend.generate()
            asmfile = util.rename(filename, 'asm')
            with open(asmfile, 'w') as af:
                for a in asm:
                    print >>af, a

            if not args.assembly:
                Backend.assembler(asmfile)

    Backend.linker(args.files)
    sys.exit(0)
