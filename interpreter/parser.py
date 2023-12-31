# module parser

import re


def _parse_lists(text: str) -> list:
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



