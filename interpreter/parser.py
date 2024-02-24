# module parser

import re
from typing import List

from interpreter import syntax
from interpreter import types
from interpreter import program



def parse(text: str) -> program.Program:
    s_expressions = _parse_lists(text)

    functions = []
    structs = []
    classes = []
    instances = []

    for sexpr in s_expressions:
        parsed = _parse_top_level(sexpr)
        if isinstance(parsed, syntax.InstanceDef):
            instances.append(parsed)
        elif isinstance(parsed, syntax.ClassDef):
            classes.append(parsed)
        elif isinstance(parsed, syntax.DFunction):
            functions.append(parsed)
        elif isinstance(parsed, syntax.StructDef):
            structs.append(parsed)

    return program.Program('parser', functions, structs, classes, instances)


def _parse_top_level(sexpr):
    ''' Parses a single top-level definition '''

    if isinstance(sexpr, list) and len(sexpr) > 0:
        first = sexpr[0]
        if first == 'instance':
            return _parse_instance_definition(sexpr)
        elif first == 'class':
            return _parse_class_definition(sexpr)
        elif first == 'fn':
            return _parse_function_declaration(sexpr)
        elif first == 'struct':
            return _parse_struct_definition(sexpr)

    raise RuntimeError(f'Unexpected expression at the top level: {sexpr}')


def _parse_instance_definition(sexpr):
    ''' Parses an instance of a class.

    Example:
      (instance (=> ((Show a)) (Show (List a)))
        (fn show (x)
          (concat "[" (join " " (map show x)) "]")))
    '''

    assert(isinstance(sexpr, list))
    assert(len(sexpr) > 1)

    qualified_predicate = _parse_qualified_predicate(sexpr[1])

    method_impls = [
        _parse_function_declaration(method)
        for method in sexpr[2:]
    ]

    return syntax.InstanceDef(qualified_predicate, method_impls)


def _parse_class_definition(sexpr):
    ''' Parses the definition of a typeclass

    Examples:
      (class (Show a)
        (:: show (Fn a String)))

      (class (Ord a) superclasses (Eq)
        (:: < (Fn a a Bool))
        (:: <= (Fn a a Bool))
        (:: max (Fn a a a))
        (:: min (Fn a a a)))

      (class (Foldable t)
        (:: foldl (Fn (Fn b a b) b (t a) b))
        (:: elem (=> ((Eq a)) (Fn a (t a) Bool))))
    '''

    assert(isinstance(sexpr, list))
    assert(len(sexpr) > 1)

    class_predicate = _parse_predicate(sexpr[1])

    rest = sexpr[2:]

    if len(rest) > 0 and rest[0] == 'superclasses':
        supers = rest[1]
        rest = rest[2:]

        assert(isinstance(supers, list))
        for s in supers:
            assert(isinstance(s, str))
            assert(s[0].isupper())
    else:
        supers = []

    superclasses = [types.TClass(s) for s in supers]

    methods = [
        _parse_method_decl(decl)
        for decl in rest
    ]

    return syntax.ClassDef(
        tclass=class_predicate.tclass,
        supers=superclasses,
        tvar=class_predicate.t.type_variable,
        methods=methods,
    )


def _parse_method_decl(sexpr):
    ''' Parse the declaration of a method on a class.

    Example:
      (:: elem (=> ((Eq a)) (Fn a (t a) Bool)))
    '''
    assert(isinstance(sexpr, list))
    assert(len(sexpr) == 3)
    assert(sexpr[0] == '::')

    name = sexpr[1]
    assert(isinstance(name, str))

    qual_type = _parse_qualified_type(sexpr[2])

    return syntax.MethodDecl(name, qual_type)


def _parse_function_declaration(sexpr):
    ''' Parses a function declaration.

    Examples:
      (fn some_fn (a b)
         (Fn Int Int Int)
         (+ a b))

      (fn untyped_fn (a b)
         (+ a b))
    '''
    assert(isinstance(sexpr, list))
    assert(len(sexpr) in [4, 5])

    name = sexpr[1]
    assert(isinstance(name, str))

    arg_names = sexpr[2]
    assert(isinstance(arg_names, list))
    for a in arg_names:
        assert(isinstance(a, str))

    if len(sexpr) == 5:
        t = _parse_qualified_type(sexpr[3])
        body = _parse_expression(sexpr[4])
    else:
        t = None
        body = _parse_expression(sexpr[3])

    return syntax.DFunction(name, t, arg_names, body)


def _parse_qualified_type(sexpr):
    ''' Parses a qualified type.

    Example:
      (=> ((Show a) (Eq a)) (List a))
    '''
    return _parse_qualified(sexpr, _parse_type)


def _parse_qualified_predicate(sexpr):
    ''' Parses a qualified predicate.

    Example:
      (=> ((Show a)) (Show (List a)))
    '''
    return _parse_qualified(sexpr, _parse_predicate)


def _parse_qualified(sexpr, inner_parser):
    if isinstance(sexpr, list) and sexpr[0] == '=>':
        assert(len(sexpr) == 3)
        predicates = _parse_predicates(sexpr[1])
        t = inner_parser(sexpr[2])
    else:
        predicates = []
        t = inner_parser(sexpr)

    return types.Qualified(predicates, t)


def _parse_predicates(sexpr):
    ''' Parses the predicate list from a qualified type. '''

    assert(isinstance(sexpr, list))
    return [_parse_predicate(e) for e in sexpr]


def _parse_predicate(sexpr):
    ''' Parses a single predicate.

    Example: (Show a)
    '''
    assert(isinstance(sexpr, list))
    assert(len(sexpr) == 2)

    class_name = sexpr[0]
    assert(class_name[0].isupper())
    tclass = types.TClass(class_name)

    # This is _usually_ just a type variable, but not always
    t = _parse_type(sexpr[1])

    return types.Predicate(tclass, t)


def _parse_expression(sexpr):
    ''' Parses expressions from s-expressions.

    Example:
      _parse_expression(['.', 'foo', 'bar'])
      returns syntax.Access(None, syntax.Variable(None, 'foo'), 'bar')
    '''

    if isinstance(sexpr, str):
        if sexpr.isdigit():
            return syntax.ELiteral(syntax.LInt(int(sexpr)))
        if sexpr[0].isdigit() and '.' in sexpr:
            return syntax.ELiteral(syntax.LFloat(float(sexpr)))
        if sexpr[0] == '"' and sexpr[-1] == '"':
            return syntax.ELiteral(syntax.LString(eval(sexpr)))
        if sexpr == 'true':
            return syntax.ELiteral(syntax.LBool(True))
        if sexpr == 'false':
            return syntax.ELiteral(syntax.LBool(False))
        return syntax.EVariable(None, sexpr)
    else:
        assert(isinstance(sexpr, list))
        first = sexpr[0]
        if first == '::':
            return _parse_typed_expression(sexpr)
        elif first == 'let':
            return _parse_let(sexpr)
        elif first == 'if':
            return _parse_if(sexpr)
        elif first == 'new':
            return _parse_new(sexpr)
        elif first == '.':
            return _parse_access(sexpr)
        elif first == '\\':
            return _parse_lambda(sexpr)
        elif first == '*partial*':
            # TODO: hide this from public syntax?
            return _parse_partial(sexpr)
        else:
            # assume it's a function call
            inner = [_parse_expression(e) for e in sexpr]
            return syntax.ECall(None, inner[0], inner[1:])


def _parse_typed_expression(sexpr):
    ''' This is a bit different from other parsers.

    This doesn't generate a syntax node of its own,
    it just adds the type to the inner expression.

    Example: (:: foo String)
    '''

    assert(len(sexpr) == 3)
    inner = _parse_expression(sexpr[1])
    t = _parse_type(sexpr[2])

    assert(inner.get_type() is None)
    inner.t = t
    return inner


def _parse_type(sexpr):
    ''' Parses unqualified types.

    Examples:
      String
      (List (Pair a a))
    '''
    if isinstance(sexpr, str):
        if sexpr[0].islower():
            return types.TVariable.from_varname(sexpr)
        else:
            return types.TConstructor(sexpr)
    else:
        assert(isinstance(sexpr, list))
        t = _parse_type(sexpr[0])
        args = [_parse_type(a) for a in sexpr[1:]]
        return types.TApplication(t, args)


def _parse_if(sexpr):
    ''' Parses an if expression

    Example:
      (if (== x 0) 1 x)
    '''
    assert(len(sexpr) == 4)
    test = _parse_expression(sexpr[1])
    if_case = _parse_expression(sexpr[2])
    else_case = _parse_expression(sexpr[3])
    return syntax.EIf(None, test, if_case, else_case)


def _parse_new(sexpr):
    ''' Parses an expression that constructs a struct.

    Example:
      (new Link value next)
    '''
    assert(len(sexpr) >= 2)
    struct_name = sexpr[1]
    assert(struct_name[0].isupper())
    args = [_parse_expression(a) for a in sexpr[2:]]
    return syntax.EConstruct(None, struct_name, args)


def _parse_access(sexpr):
    ''' Parses accessing a field in a struct.

    Example:
      (. some_struct some_field)
    '''
    assert(len(sexpr) == 3)
    struct_expr = _parse_expression(sexpr[1])
    field_name = sexpr[2]
    assert(isinstance(field_name, str))
    return syntax.EAccess(None, struct_expr, field_name)


def _parse_lambda(sexpr):
    ''' Parses a lambda expression.

    Example:
      (\ (a b) (+ a b))
    '''
    assert(len(sexpr) == 3)
    arg_names = sexpr[1]
    assert(isinstance(arg_names, list))
    for a in arg_names:
        assert(isinstance(a, str))

    body = _parse_expression(sexpr[2])
    return syntax.ELambda(None, arg_names, body)


def _parse_partial(sexpr):
    ''' Parses a partial function application.

    Example:
       (*partial* + 1)
    '''
    assert(len(sexpr) >= 2)
    function_expression = _parse_expression(sexpr[1])

    arg_expressions = [
        _parse_expression(a)
        for a in sexpr[2:]
    ]

    return syntax.EPartial(None, function_expression, arg_expressions)


def _parse_let(sexpr):
    ''' Parses let exprsesions.

    Example: (let ((x 1) (y 2))  (+ x y))
    '''
    assert(len(sexpr) == 3)
    bindings = _parse_let_bindings(sexpr[1])
    inner = _parse_expression(sexpr[2])
    return syntax.ELet(None, bindings, inner)


def _parse_let_bindings(sexprs):
    ''' Parses the list of bindings in a let expression.

    Example: ((x 1) (y 2))
    '''
    assert(isinstance(sexprs, list))
    bindings = []
    for sexpr in sexprs:
        assert(isinstance(sexpr, list))
        assert(len(sexpr) == 2)
        name = sexpr[0]
        value = _parse_expression(sexpr[1])
        binding = syntax.Binding(name, value)
        bindings.append(binding)
    return bindings

def _parse_struct_definition(sexpr):
    ''' Parses struct definitions.

    Examples:
    (struct Name (:: first String) (:: last String))
    (struct (Pair a) (:: x a) (:: y a))
    (struct Void)
    '''
    assert(isinstance(sexpr, list))
    assert(len(sexpr) >= 2)

    name_part = sexpr[1]
    if isinstance(name_part, list):
        name = name_part[0]
        tvs = [types.TypeVariable(x) for x in name_part[1:]]
    else:
        name = name_part
        tvs = []

    fields = [_parse_field(field) for field in sexpr[2:]]

    return syntax.StructDef(name, tvs, fields)


def _parse_field(sexpr):
    ''' Parses a struct field

    Example: (:: year Int)
    '''
    assert(isinstance(sexpr, list))
    assert(len(sexpr) == 3)
    assert(sexpr[0] == '::')

    name = sexpr[1]
    assert(isinstance(name, str))

    t = _parse_type(sexpr[2])

    return (name, t)


def _parse_lists(text: str) -> list:
    ''' Returns a list of the parsed s-expressions.

    Example: _parse_lists("(123)") returns [["123"]]
    '''
    return _parse_tokens(_tokenize(text))


def _parse_one_list(text: str) -> list:
    lists = _parse_lists(text)
    assert(len(lists) == 1)
    return lists[0]


def _parse_tokens(tokens: list) -> list:
    ''' Converts tokens into s-expressions.

    Ignores comments.

    Example:
      _parse_tokens(['(', '123', ')', '456'])
      returns [["123"], "456"]
    '''
    stack = [[]]  # type: List[List[str | List]]
    for t in tokens:
        if t == '(':
            stack.append([])
        elif t == ')':
            assert(len(stack) > 1)
            finished_list = stack.pop()
            stack[-1].append(finished_list)
        elif not t.startswith(';;'):
            stack[-1].append(t)
    assert(len(stack) == 1)
    return stack[0]


TOKEN_RE = re.compile(r'([(]|[)]|;;[^\n]*\n|"(?:[^"\\]|\\.)*"|[^\s()]+|\s+|\n)')


def _tokenize(text: str) -> list:
    ''' Converts text into a list of tokens.

    Example:
      _tokenize('(123) ;; comment\n456')
      returns ['(', '123', ')', ';; comment', '456']
    '''
    tokens = TOKEN_RE.findall(text)
    return [t for t in tokens if not t.isspace()]
