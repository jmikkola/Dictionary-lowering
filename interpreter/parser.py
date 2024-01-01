# module parser

import re

from interpreter import syntax
from interpreter import types


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


def _parse_lists(text: str) -> list:
    ''' Returns a list of the parsed s-expressions.

    Example: _parse_lists("(123)") returns ["123"]
    '''
    return _parse_tokens(_tokenize(text))


def _parse_tokens(tokens: list) -> list:
    ''' Converts tokens into s-expressions.

    Ignores comments.

    Example:
      _parse_tokens(['(', '123', ')', '456'])
      returns [["123"], "456"]
    '''
    stack = ([], None)  # type: ignore
    for t in tokens:
        if t == '(':
            stack = ([], stack)  # type: ignore
        elif t == ')':
            (finished_list, stack) = stack  # type: ignore
            stack[0].append(finished_list)
        elif not t.startswith(';;'):
            stack[0].append(t)
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
