from interpreter import *
from interpreter.types import *
from interpreter.syntax import *

t = TApplication(TConstructor('Fn'), [TConstructor('Int'), TConstructor('Void')])
print(t)
