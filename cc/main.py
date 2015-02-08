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

def basename(filename):
    fn, _ = filename.rsplit('.', 1)
    return fn

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
        p = subprocess.Popen(['cpp', filename], stdout=subprocess.PIPE)
        #with open(filename) as f:
        #    ast = parser.parse(f.read())
        ast = parser.parse(p.stdout.read())

        if args.ast:
            with open(basename(filename)+'.ast', 'w') as af:
                print >>af, ast

        intermediate = iemit.emit(ast)
        if args.intermediate:
            with open(basename(filename)+'.i', 'w') as ir:
                for r in intermediate:
                    if isinstance(r, ia.Label):
                        print >>ir, r
                    else:
                        print >>ir, '   ', r

        if not args.no_assembly:
            backend = Backend(intermediate)
            asm = backend.generate()
            with open(basename(filename)+'.asm', 'w') as af:
                for a in asm:
                    print >>af, a
    sys.exit(0)
