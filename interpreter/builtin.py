# module builtins

from interpreter import parser


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

CLASS_DEFINITIONS = '''
(class (Show a)
  (:: show (Fn a String)))

(class (Read a)
  (:: read (Fn String a)))

(class (Eq a)
  (:: == (Fn a a Bool))
  (:: != (Fn a a Bool)))

(class (Ord a) superclasses (Eq)
  (:: <  (Fn a a Bool))
  (:: <= (Fn a a Bool))
  (:: >  (Fn a a Bool))
  (:: >= (Fn a a Bool)))

(class (Num a) superclasses (Eq Show)
  (:: + (Fn a a a))
  (:: - (Fn a a a))
  (:: * (Fn a a a))
  (:: / (Fn a a a)))

(class (Integral a) superclasses (Num)
  (:: % (Fn a a a)))
'''

INSTANCE_DEFINITIONS = '''
;; Int instances
(instance (Show Int)
  (fn show (x) (show:Int x)))

(instance (Eq Int)
  (fn == (x y) (==:Int x y))
  (fn != (x y) (not (==:Int x y))))

(instance (Num Int)
  (fn + (x y) (+:Int x y))
  (fn - (x y) (-:Int x y))
  (fn * (x y) (*:Int x y))
  (fn / (x y) (/:Int x y)))

(instance (Ord Int)
  (fn <  (x y) (<:Int x y))
  (fn <= (x y) (<=:Int x y))
  (fn >  (x y) (>:Int x y))
  (fn >= (x y) (>=:Int x y)))

(instance (Integral Int)
  (fn % (a b) (%:Int a b)))

;; Float instances
(instance (Show Float)
  (fn show (x) (show:Float x)))

(instance (Eq Float)
  (fn == (x y) (==:Float x y))
  (fn != (x y) (not (==:Float x y))))

(instance (Num Float)
  (fn + (x y) (+:Float x y))
  (fn - (x y) (-:Float x y))
  (fn * (x y) (*:Float x y))
  (fn / (x y) (/:Float x y)))

(instance (Ord Float)
  (fn <  (x y) (<:Float x y))
  (fn <= (x y) (<=:Float x y))
  (fn >  (x y) (>:Float x y))
  (fn >= (x y) (>=:Float x y)))

;; Bool instances
(instance (Show Bool)
  (fn show (b) (if b "true" "false")))

(instance (Eq Bool)
  (fn == (a b) (==:Bool a b))
  (fn != (a b) (not (==:Bool a b))))

(instance (Ord Bool)
  (fn <  (x y) (<:Bool x y))
  (fn <= (x y) (<=:Bool x y))
  (fn >  (x y) (>:Bool x y))
  (fn >= (x y) (>=:Bool x y)))

;; String instances
(instance (Show String)
  (fn show (s) s))

(instance (Eq String)
  (fn == (a b) (==:String a b))
  (fn != (a b) (not (==:String a b))))

(instance (Ord String)
  (fn <  (x y) (<:String x y))
  (fn <= (x y) (<=:String x y))
  (fn >  (x y) (>:String x y))
  (fn >= (x y) (>=:String x y)))

;; List instances
(instance (=> ((Show a)) (Show (List a)))
  (fn show (list) "not implemented"))

(instance (=> ((Eq a)) (Eq (List a)))
  (fn == (a b) false)
  (fn != (a b) false))
'''


_classes = None
_instances = None


def get_classes():
    global _classes

    if _classes is None:
        _classes = parser.parse(CLASS_DEFINITIONS, is_builtin=True).classes
    return _classes


def get_instances():
    global _instances

    if _instances is None:
        _instances = parser.parse(INSTANCE_DEFINITIONS, is_builtin=True).instances
    return _instances


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

_function_types = None


def get_function_types():
    global _function_types

    if _function_types is None:
        _function_types = _compute_function_types()
    return _function_types


def _compute_function_types():
    function_types = {}

    # Add types for basic functions

    standard_functions = [
        ('str', '(Fn a String)'),
        ('inc', '(Fn Int Int)'),
        ('and', '(Fn Bool Bool Bool)'),
        ('or', '(Fn Bool Bool Bool)'),
        ('not', '(Fn Bool Bool)'),
        ('print', '(Fn a Void)'),
        ('concat', '(Fn String String String)'),
        ('length', '(Fn String Int)'),
    ]

    for name, signature in standard_functions:
        function_types[name] = _parse_function_type(signature)

    # Add types for non-generic operations

    function_types['%:Int'] = _parse_function_type('(Fn Int Int Int)')

    num_operators = ['+', '-', '*', '/']
    num_types = ['Int', 'Float']

    eq_operators = ['==', '!=']
    eq_types = ['Int', 'Float', 'String', 'Bool']

    ord_operators = ['<', '<=', '>', '>=']
    ord_types = ['Int', 'Float', 'String', 'Bool']

    show_types = ['Int', 'Float', 'String', 'Bool']

    def add_operators(ops, arg_type, ret_type):
        sig = f'(Fn {arg_type} {arg_type} {ret_type})'
        t = _parse_function_type(sig)

        for op in ops:
            name = op + ':' + arg_type
            function_types[name] = t

    for arg_type in num_types:
        add_operators(num_operators, arg_type, arg_type)

    for arg_type in eq_types:
        add_operators(eq_operators, arg_type, 'Bool')

    for arg_type in ord_types:
        add_operators(ord_operators, arg_type, 'Bool')

    for arg_type in show_types:
        add_operators(['show'], arg_type, 'String')

    return function_types


def _parse_function_type(text):
    sexpr = parser._parse_one_list(text)
    return parser._parse_qualified_type(sexpr)
