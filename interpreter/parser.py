# module parser

import re

from interpreter import syntax


def _parse_expression(sexpr):
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
        # Special forms (that aren't function invocation) to handle:
        # first arg is another list
        # let, if, new, lambda, access
        # typed


def _parse_lists(text: str) -> list:
    ''' Returns a list of the parsed s-expressions '''
    return _parse_tokens(_tokenize(text))


def _parse_tokens(tokens: list) -> list:
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
    tokens = TOKEN_RE.findall(text)
    return [t for t in tokens if not t.isspace()]
