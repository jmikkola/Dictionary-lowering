# module check

from collections import defaultdict
from typing import Set

from interpreter import builtin
from interpreter import graph
from interpreter import syntax
from interpreter import types
from interpreter.program import Program


def check(program: Program):
    ''' This is a pass that should run right after the parser and
    before any type or lowering passes'''

    checker = Checker(program)
    checker.check()

    # Checker doesn't actually change the code in any way except to note which
    # pass it went through:
    return Program(
        from_stage='check',
        functions=program.functions,
        structs=program.structs,
        classes=program.classes,
        instances=program.instances,
    )


class CheckFailure(RuntimeError):
    pass


class Checker:
    def __init__(self, program: Program):
        self.program = program

        self.global_scope = Scope()
        self.global_scope.define_all(builtin.NAMES)

        # defined_types: all types that are valid to use in a type constructor (e.g. Int, Fn, structs)
        self.defined_types = set(builtin.TYPES)
        # struct_types: types that can be constructed with 'new' (a subset of defined_types)
        self.struct_types = set()  # type: Set[str]
        # class_types: names of typeclasses
        self.class_types = set()  # type: Set[str]

    def check(self):
        (class_names, struct_names) = self._check_declaration_names(
            self.program.classes,
            self.program.structs
        )
        self.defined_types |= set(struct_names)
        self.class_types |= set(class_names)
        self.struct_types |= set(struct_names)

        function_names = self._check_function_names(
            self.program.functions,
            self.program.classes
        )
        self.global_scope.define_all(function_names)

        self._check_structs(self.program.structs)
        self._check_functions(self.program.functions)
        self._check_classes(self.program.classes)
        self._check_instances(self.program.instances)

    def _check_function_names(self, functions, classes):
        function_names = [f.name for f in functions]
        method_names = [name for c in classes for name in c.method_names()]
        names = function_names + method_names
        self._assert_unique(
            names,
            lambda name: CheckFailure(f'Duplicate method name {name}')
        )

        for name in names:
            if name in builtin.NAMES:
                raise CheckFailure(f'Cannot redefine the builtin {name}')

        return names

    def _check_declaration_names(self, classes, structs):
        class_names = [c.class_name() for c in classes]
        struct_names = [s.name for s in structs]
        names = class_names + struct_names
        self._assert_unique(
            names,
            lambda name: CheckFailure(f'Duplicate declaration name {name}')
        )

        for name in names:
            if name in builtin.TYPES:
                raise CheckFailure(f'Cannot redefine the builtin type {name}')

        return (class_names, struct_names)

    def _check_functions(self, functions):
        for f in functions:
            self._check_function(f)

    def _check_function(self, function):
        self._check_valid_name('function', function.name)
        self._check_type(function.t)

        self._assert_unique(
            function.arg_names,
            lambda name: CheckFailure(f'Duplicate argument {name} in function {function.name}')
        )

        scope = self.global_scope.make_inner()
        scope.define_all(function.arg_names)

        self._check_expression(function.body, scope)

    def _check_structs(self, structs):
        for s in structs:
            self._check_struct(s)

    def _check_struct(self, struct):
        self._assert_unique(
            (name for (name, _t) in struct.fields),
            lambda name: CheckFailure(f'struct {struct.name} has multiple fields named {name}')
        )
        for (_name, t) in struct.fields:
            self._check_type(t)

    def _check_classes(self, classes):
        self._assert_unique(
            (c.class_name() for c in classes),
            lambda name: CheckFailure(f'Duplicate classes named {name}')
        )

        for c in classes:
            self._check_class(c)

        class_deps = []
        for c in classes:
            for s in c.supers:
                class_deps.append((c.class_name(), s.name))
        result = graph.topological_order(class_deps)
        if result is False:
            raise CheckFailure(f'Class hierarchy cannot be cyclic')

    def _check_class(self, c):
        self._assert_unique(
            (s.name for s in c.supers),
            lambda name: CheckFailure(f'Duplicate super class {name} for class {c.class_name()}')
        )

        for s in c.supers:
            self._ensure_class_exists(s.name)

        self._assert_unique(
            (m.method_name for m in c.methods),
            lambda name: CheckFailure(f'Duplicate method {name} for class {c.class_name()}')
        )

        for m in c.methods:
            self._check_class_method(c, m)

    def _check_class_method(self, c, m):
        method_type = m.get_type()
        self._check_type(method_type)

        ftvs = method_type.free_type_vars()
        if c.tvar not in ftvs:
            raise CheckFailure(f'Method {m.method_name} on class {c.class_name()} must reference the class type variable')

    def _check_instances(self, instances):
        for inst in instances:
            self._check_instance(inst)

        instances_by_class = defaultdict(list)
        for inst in instances:
            instances_by_class[inst.get_class().name].append(inst)

        for (classname, insts_for_class) in instances_by_class.items():
            self._check_instance_overlap(classname, insts_for_class)

    def _check_instance(self, instance):
        self._ensure_class_exists(instance.get_class().name)

        # Assemble a qualified type to check the type, the predicates, and that the predicates apply to the type
        qual = types.Qualified(instance.get_predicates(), instance.get_type())
        self._check_qualified(qual)

        for method in instance.method_impls:
            self._check_function(method)

        # Find the associated class
        for c in self.program.classes:
            if c.tclass == instance.get_class():
                classdef = c

        # Check the methods
        defined_methods = {
            method.name: method
            for method in instance.method_impls
        }

        class_method_names = {
            method.method_name
            for method in classdef.methods
        }

        iname = show_lisp(instance.qual_pred.t)

        for method in instance.method_impls:
            if method.name not in class_method_names:
                raise CheckFailure(f'The instance {iname} declares a method {method.name} not found on the class')

        for method in classdef.methods:
            if method.method_name not in defined_methods:
                raise CheckFailure(f'The instance {iname} does not define the method {method.method_name}')
            inst_method = defined_methods[method.method_name]
            if len(inst_method.arg_names) != method.num_args():
                raise CheckFailure(f'The {method.method_name} method of instance {iname} has the wrong number of arguments')

    def _check_instance_overlap(self, classname, insts_for_class):
        # Check all pairwise combinations
        for i in range(len(insts_for_class) - 1):
            type_i = insts_for_class[i].get_type()
            for j in range(i + 1, len(insts_for_class)):
                type_j = insts_for_class[j].get_type()
                if types.can_types_unify(type_i, type_j):
                    ti = syntax.render_lisp(type_i.to_lisp())
                    tj = syntax.render_lisp(type_j.to_lisp())
                    raise CheckFailure(f'Instances {ti} and {tj} for {classname} overlap')

    def _check_expression(self, expr, scope):
        self._check_type(expr.get_type())

        if isinstance(expr, syntax.EVariable):
            if not scope.is_defined(expr.name):
                raise CheckFailure(f'Undefined variable {expr.name}')

        elif isinstance(expr, syntax.ELiteral):
            return

        elif isinstance(expr, syntax.ECall):
            self._check_expression(expr.f_expr, scope)
            for a in expr.arg_exprs:
                self._check_expression(a, scope)

        elif isinstance(expr, syntax.EConstruct):
            if expr.struct_name not in self.struct_types:
                raise CheckFailure(f'Cannot find struct named {expr.struct_name}')
            for a in expr.arg_exprs:
                self._check_expression(a, scope)

        elif isinstance(expr, syntax.EPartial):
            self._check_expression(expr.f_expr, scope)
            for a in expr.arg_exprs:
                self._check_expression(a, scope)

        elif isinstance(expr, syntax.EAccess):
            self._check_expression(expr.lhs, scope)

        elif isinstance(expr, syntax.ELet):
            names = [binding.name for binding in expr.bindings]
            self._assert_unique(
                names,
                lambda name: CheckFailure(f'Duplicate name {name} in let binding')
            )
            inner_scope = scope.make_inner()
            inner_scope.define_all(names)
            for binding in expr.bindings:
                self._check_expression(binding.value, inner_scope)
            self._check_expression(expr.inner, inner_scope)

        elif isinstance(expr, syntax.EIf):
            self._check_expression(expr.test, scope)
            self._check_expression(expr.if_case, scope)
            self._check_expression(expr.else_case, scope)

        elif isinstance(expr, syntax.ELambda):
            self._assert_unique(
                expr.arg_names,
                lambda name: CheckFailure(f'Duplicate argument {name} in lambda function')
            )
            inner_scope = scope.make_inner()
            inner_scope.define_all(expr.arg_names)
            self._check_expression(expr.body, inner_scope)

        else:
            raise RuntimeError(f'Unhandled expression: {expr}')

    def _check_type(self, t):
        if t is None:
            return

        if isinstance(t, types.Qualified):
            self._check_qualified(t)
            return

        if isinstance(t, types.Predicate):
            self._check_predicate(t)
            return

        assert(isinstance(t, types.Type))

        if isinstance(t, types.TVariable):
            # Type variables aren't declared before they are used, so any type
            # variable is a valid part of a type
            return
        elif isinstance(t, types.TConstructor):
            if t.type_name not in self.defined_types:
                raise CheckFailure(f'Undefined type {t.type_name}')
        elif isinstance(t, types.TApplication):
            self._check_type(t.t)
            for a in t.args:
                self._check_type(a)
            if t.t == types.TConstructor('Fn'):
                if len(t.args) == 0:
                    raise CheckFailure('Function types must always have a return type')

    def _check_qualified(self, qualified):
        type_vars = qualified.t.free_type_vars()

        for pred in qualified.predicates:
            self._check_predicate(pred)

            # Ensure the predicate is applied to a type variable, not a concrete type
            if not isinstance(pred.t, types.TVariable):
                p = show_lisp(pred)
                raise CheckFailure(f'The predicate {p} applies a predicate to a concrete type')

            # Ensure that the predicate actually applies to some part of the type
            tv = pred.t.type_variable
            if tv not in type_vars:
                p = show_lisp(pred)
                t = show_lisp(qualified.t)
                raise CheckFailure(f'The predicate {p} does not apply to the type {t}')

        self._check_type(qualified.t)

    def _check_predicate(self, predicate):
        self._ensure_class_exists(predicate.tclass.name)
        self._check_type(predicate.t)

    def _ensure_class_exists(self, classname):
        if classname not in self.class_types:
            raise CheckFailure(f'Undefined class {classname}')

    def _check_valid_name(self, name_kind, name):
        first_letter = name[0]
        if not first_letter.isalpha() or not first_letter.islower():
            raise CheckFailure(f'{name_kind} names must start with a lowercase letter, found {name}')

    def _assert_unique(self, names, make_error):
        seen = set()
        for name in names:
            if name in seen:
                raise make_error(name)
            seen.add(name)


class Scope:
    def __init__(self, parent=None):
        self.parent = parent
        self.names = set()

    def define(self, name):
        self.names.add(name)

    def define_all(self, names):
        for name in names:
            self.define(name)

    def is_defined(self, name):
        if name in self.names:
            return True
        if self.parent is not None:
            return self.parent.is_defined(name)
        return False

    def make_inner(self):
        return Scope(parent=self)


def show_lisp(item):
    return syntax.render_lisp(item.to_lisp())
