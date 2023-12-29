from interpreter import *
from interpreter.types import *
from interpreter.syntax import *
from interpreter.lowering import *

t = TApplication(TConstructor('Fn'), [TConstructor('Int'), TConstructor('Void')])
print(t)
