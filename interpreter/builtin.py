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

(instance (Show Bool)
  (fn show (b) (if b "true" "false")))

(instance (Show String)
  (fn show (s) s))

(instance (Integral Int)
  (fn % (a b) (%:Int a b)))
'''

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
