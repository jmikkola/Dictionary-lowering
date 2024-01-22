from interpreter import lowering
from interpreter import parser
from interpreter import syntax
from interpreter import treewalker

def main(args):
    with open(sys.argv[1]) as inf:
        text = inf.read()

    parse_result = parser.parse(text)

    lowering_input = lowering.LoweringInput(
        declarations=parse_result.functions,
        structs=parse_result.structs,
        classes=parse_result.classes,
        instances=parse_result.instances,
    )

    lowering_output = lowering_input.lower()

    if '--render' in args:
        first = True
        for lisp in lowering_output.to_lisp():
            if not first:
                print()
            first = False
            print(syntax.render_lisp(lisp))

    else:
        intp = treewalker.Interpreter()
        intp.load_declarations(lowering_output.declarations)
        intp.load_structs(lowering_output.dictionaries)
        intp.eval_main()


if __name__ == '__main__':
    import sys
    main(sys.argv[1:])
