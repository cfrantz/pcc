#!/usr/bin/env python

import sys
import macropy.activate
import parser
from dumper import Dumper
import iemit
import ia
from x86_32 import Backend
import json
import argparse

if __name__ == '__main__':
    ap = argparse.ArgumentParser(description='Python C Compiler')
    ap.add_argument('-a', '--ast', action='store_true',
            help='Print the abstract syntax tree')
    ap.add_argument('-i', '--intermediate', action='store_true',
            help='Print the intermediate representation')
    ap.add_argument('-x', '--no-assembly', action='store_true',
            help='Do not emit backend assembly')
    ap.add_argument('files', nargs='*', default=[],
            help='Files to compile')

    args = ap.parse_args()
    for filename in args.files:
        with open(filename) as f:
            ast = parser.parse(f.read())

        if args.ast:
            print ast

        intermediate = iemit.emit(ast)
        if args.intermediate:
            for r in intermediate:
                if isinstance(r, ia.Label):
                    print r
                else:
                    print '   ', r

        if not args.no_assembly:
            backend = Backend(intermediate)
            asm = backend.generate()
            for a in asm:
                print a
    sys.exit(0)
