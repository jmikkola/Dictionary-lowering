(fn collatz-seq (n)
  (let ((add-num (\ (n rest)
                    (concat (str n) (concat ", " rest))))
        (odd  (\ (n) (ctz (+ (* n 3) 1))))
        (even (\ (n) (ctz (/ n 2))))
        (ctz  (\ (n)
                (if (== n 1)
                  "1"
                  (add-num
                    n
                    (if (== (% n 2) 0) (even n) (odd n)))))))
    (ctz n)))

(fn main ()
    (print (collatz-seq 7)))
