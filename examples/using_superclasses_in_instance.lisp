(class (Parent p)
  (:: parent (Fn p Int)))

(class (Child c)
  superclasses (Parent)
  (:: child (Fn c Int)))

(instance (Parent String)
  (fn parent (s)
    (:: ((:: length (Fn String Int)) (:: s String)) Int)))

(instance (Child String)
  (fn child (s)
    (::
      ((:: inc (Fn Int Int))
       (:: ((:: parent (Fn String Int)) (:: s String)) Int))
      Int)))

(fn main ()
    (Fn Void)
    (:: ((:: print (Fn String Void)) "Hello world") Void))
