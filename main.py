#!/usr/bin/env python3

from interpreter import check
from interpreter import format
from interpreter import inference
from interpreter import lowering
from interpreter import parser
from interpreter import syntax
from interpreter import treewalker

def main(args):
    with open(sys.argv[1]) as inf:
        text = inf.read()

    parse_result = parser.parse(text)

    if '--dump-parsed' in args:
        dump_program(parse_result)
        return

    check_result = check.check(parse_result)

    # Check doesn't change the program, so there's no point printing that

    inference_result = inference.infer_types(check_result)

    if '--dump-inferred' in args:
        dump_program(inference_result)
        return

    lowering_result = lowering.lower(inference_result)

    if '--dump-lowered' in args:
        dump_program(lowering_result, '--show-builtins' in args)
        return

    treewalker.interpret(lowering_result)


def dump_program(program, show_builtins=True):
    print(program.format(show_builtins))


if __name__ == '__main__':
    import sys
    main(sys.argv[1:])
