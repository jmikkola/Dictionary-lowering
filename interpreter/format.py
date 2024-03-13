# module format

from interpreter import parser


def format_declaration(lisp):
    first = lisp[0]
    if first == 'fn':
        return format_fn(lisp)
    elif first == 'class':
        return format_class(lisp)
    elif first == 'struct':
        return format_struct(lisp)
    elif first == 'instance':
        return format_instance(lisp)
    raise RuntimeError('unknown declaration: ' + str(lisp))


def format_fn(lisp, indent=None):
    indent = indent or ''

    args = render_lisp(lisp[2])
    lines = [
        indent + f'(fn {lisp[1]} {args}'
    ]

    inner_indent = indent + '  '
    if len(lisp) > 4:
        lines.append(_format(lisp[3], inner_indent, 0))

    lines.append(_format(lisp[-1], inner_indent, 1))

    return '\n'.join(lines)


def format_class(lisp):
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
        lines.append(_format(rest[i], inner_indent, unclosed))

    return '\n'.join(lines)


def format_struct(lisp):
    line = '(struct ' + render_lisp(lisp[1])
    lines = [line]

    inner_indent = '  '
    for i in range(2, len(lisp)):
        unclosed = 0 if i + 1 < len(lisp) else 1
        field = _format(lisp[i], inner_indent, unclosed)
        lines.append(field)

    return '\n'.join(lines)


def format_instance(lisp):
    line = '(instance ' + render_lisp(lisp[1])
    lines = [line]

    for i in range(2, len(lisp)):
        method_text = format_fn(lisp[i], '  ')
        if i + 1 == len(lisp):
            method_text += ')'
        lines.append(method_text)

    return '\n'.join(lines)


def format(lisp):
    return _format(lisp, indent='', unclosed=0)


def _format(lisp, indent, unclosed):
    return indent + render_lisp(lisp) + (')' * unclosed)


def render_lisp(lisp):
    ''' converts e.g. ['f', '1'] back into "(f 1)" '''
    if isinstance(lisp, str):
        return lisp

    inner = ' '.join(render_lisp(l) for l in lisp)
    return f'({inner})'
