(fn use_partially_applied_function ()
    (Fn String)
    (:: ((:: ((:: get_partially_applied_function
                  (Fn (Fn Int String))))
             (Fn Int String))
         321)
        String))

(fn get_partially_applied_function ()
    (Fn (Fn Int String))
    (:: show__thing (Fn Int String)))

(fn show__thing (thing)
    (=> ((Show_ a)) (Fn a String))
    (:: ((:: show_ (Fn a String)) (:: thing a)) String))

(class (Show_ s)
       (:: show_ (Fn s String)))

(instance (Show_ Int)
          (fn show_ (i)
              (:: ((:: str (Fn Int String)) (:: i Int)) String)))

(fn main ()
    (Fn Void)
    (:: ((:: print (Fn String Void))
         (:: ((:: use_partially_applied_function (Fn String)))
             String))
        Void))
