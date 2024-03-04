(class (Parent p)
  (:: parent (Fn p Int)))

(class (Child c)
  superclasses (Parent)
  (:: child (Fn c Int)))

;; TODO: Inference is buggy and doesn't accept this
(instance (Parent String)
  (fn parent (s)
    (length s)))

(instance (Child String)
  (fn child (s)
    (inc (parent s))))

(fn use-child-class (x)
    (child x))

(fn main ()
    (print (use-child-class "xyz")))
