(fn use_partially_applied_function ()
    (Fn String)
    (:: ((:: ((:: get_partially_applied_function
                  (Fn (Fn Int String))))
             (Fn Int String))
         321)
        String))

(fn get_partially_applied_function ()
    (Fn (Fn Int String))
    (:: show_thing (Fn Int String)))

(fn show_thing (thing)
    (=> ((Show a)) (Fn a String))
    (:: ((:: show (Fn a String)) (:: thing a)) String))

(class (Show s)
       (:: show (Fn s String)))

(instance (Show Int)
          (fn show (i)
              (:: ((:: str (Fn Int String)) (:: i Int)) String)))

(fn main ()
    (Fn Void)
    (:: ((:: print (Fn String Void))
         (:: ((:: use_partially_applied_function (Fn String)))
             String))
        Void))
