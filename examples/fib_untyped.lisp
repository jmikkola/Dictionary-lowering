(fn main ()
  (Fn Void)
  (print (str (fib 12))))

(fn fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
