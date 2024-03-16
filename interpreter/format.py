# module format

from interpreter import parser


TARGET_LINE_LENGTH = 80


def reformat(text, target_line_length=TARGET_LINE_LENGTH):
    ''' Reformats lisp text

    This drops comments '''
    lisps = parser._parse_lists(text)
    declarations = [
        format_declaration(lisp, target=target_line_length)
        for lisp in lisps
    ]
    return '\n\n'.join(declarations)


def format_declaration(lisp, target=TARGET_LINE_LENGTH):
    first = lisp[0]
    if first == 'fn':
        return format_fn(lisp, target=target)
    elif first == 'class':
        return format_class(lisp, target=target)
    elif first == 'struct':
        return format_struct(lisp, target=target)
    elif first == 'instance':
        return format_instance(lisp, target=target)
    raise RuntimeError('unknown declaration: ' + str(lisp))


def format_fn(lisp, indent=None, target=TARGET_LINE_LENGTH):
    indent = indent or ''

    args = render_lisp(lisp[2])
    lines = [
        indent + f'(fn {lisp[1]} {args}'
    ]

    inner_indent = indent + '  '
    if len(lisp) > 4:
        lines.append(_format(lisp[3], inner_indent, 0, target=target))

    lines.append(_format(lisp[-1], inner_indent, 1, target=target))

    return '\n'.join(lines)


def format_class(lisp, target=TARGET_LINE_LENGTH):
    line = '(class ' + render_lisp(lisp[1])
    if lisp[2] == 'superclasses':
        line += ' superclasses ' + render_lisp(lisp[3])
        rest = lisp[4:]
    else:
        rest = lisp[2:]

    lines = [line]

    inner_indent = '  '
    for i in range(len(rest)):
        unclosed = 0 if i + 1 < len(rest) else 1
        lines.append(_format(rest[i], inner_indent, unclosed, target=target))

    return '\n'.join(lines)


def format_struct(lisp, target=TARGET_LINE_LENGTH):
    line = '(struct ' + render_lisp(lisp[1])
    lines = [line]

    inner_indent = '  '
    for i in range(2, len(lisp)):
        unclosed = 0 if i + 1 < len(lisp) else 1
        field = _format(lisp[i], inner_indent, unclosed, target=target)
        lines.append(field)

    return '\n'.join(lines)


def format_instance(lisp, target=TARGET_LINE_LENGTH):
    line = '(instance ' + render_lisp(lisp[1])
    lines = [line]

    for i in range(2, len(lisp)):
        method_text = format_fn(lisp[i], '  ', target=target)
        if i + 1 == len(lisp):
            method_text += ')'
        lines.append(method_text)

    return '\n'.join(lines)


def format(lisp, target=TARGET_LINE_LENGTH):
    return _format(lisp, indent='', unclosed=0, target=target)



def _format(lisp, indent, unclosed, target):
    return _format_wrapped(lisp, indent, '', unclosed, target=target)


def _format_wrapped(lisp, indent, prefix, unclosed, target):
    ''' This wraps the current expression, if needed '''
    result = indent + prefix + render_lisp(lisp) + (')' * unclosed)
    if len(result) <= target or isinstance(lisp, str) or lisp == []:
        return result

    first = lisp[0]
    additional_indent = '  '
    keep_2 = False
    indent_after_first = False
    if first == '::':
        additional_indent = '    '
        keep_2 = True
        indent_after_first = True
    elif first in ('if', 'new', '\\', '.', 'let'):
        keep_2 = True
    elif isinstance(first, list):
        additional_indent = ' '
        keep_2 = False

    if keep_2:
        # These are expressions that wrap like
        #
        # (:: foo
        #     Bar)
        second = lisp[1]
        rest = lisp[2:]
        start = prefix + '(' + render_lisp(first) + ' '
        line = _format_wrapped(second, indent, start, 0, target=target)
        if indent_after_first:
            additional_indent = ' ' * len(start)
        else:
            additional_indent += ' ' * len(prefix)
    else:
        # These are expressions that wrap like
        #
        # (foo
        #  bar)
        line = _format_wrapped(first, indent, prefix + '(', 0, target=target)
        rest = lisp[1:]
        additional_indent += ' ' * len(prefix)

    lines = [line]

    inner_indent = indent + additional_indent
    for item in rest:
        lines.append(_format_wrapped(item, inner_indent, '', 0, target=target))

    unclosed += 1
    lines[-1] += ')' * unclosed

    return '\n'.join(lines)


def render_lisp(lisp):
    ''' converts e.g. ['f', '1'] back into "(f 1)" '''
    if isinstance(lisp, str):
        return lisp

    inner = ' '.join(render_lisp(l) for l in lisp)
    return f'({inner})'
