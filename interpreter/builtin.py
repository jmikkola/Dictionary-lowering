# module builtins

NAMES = [
    '!=',
    '%',
    '*',
    '+',
    '-',
    '/',
    '<',
    '<=',
    '==',
    '>',
    '>=',
    'and',
    'concat',
    'inc',
    'length',
    'not',
    'or',
    'print',
    'str',
]

NON_TYPECLASS_NAMES = [
    '%',
    'and',
    'concat',
    'inc',
    'length',
    'not',
    'or',
    'print',
    'str',
]


TYPES = [
    'Int',
    'Float',
    'String',
    'Fn',
    'Void',
    'Bool',
    'List',
]

STD_CLASSES = ['Eq', 'Ord', 'Show', 'Read', 'Num', 'Integral']

FUNCTION_TYPES = [
    ('str', '(Fn a String)'),
    ('inc', '(Fn Int Int)'),
    ('and', '(Fn Bool Bool Bool)'),
    ('or', '(Fn Bool Bool Bool)'),
    ('not', '(Fn Bool Bool)'),
    ('print', '(Fn a Void)'),
    ('concat', '(Fn String String String)'),
    ('length', '(Fn String Int)'),
]
