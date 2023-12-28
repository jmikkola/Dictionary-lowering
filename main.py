from interpreter import *
from interpreter.types import *

t = TApplication(TConstructor('Fn'), [TConstructor('Int'), TConstructor('Void')])
print(t)
