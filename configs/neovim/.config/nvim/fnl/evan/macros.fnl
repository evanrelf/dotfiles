(fn nil? [x]
  (= nil x))

(fn begins-with? [prefix str]
  (if (str:match (.. "^" prefix)) true false))

(lambda set! [name ?value]
  (assert-compile (sym? name) "expected symbol for name" name)
  (let [name (tostring name)
        value (if (nil? ?value)
                  (not (begins-with? :no name))
                  ?value)
        name (if (and (nil? ?value) (begins-with? :no name))
                 (name:match "^no(.+)$")
                 name)]
    `(tset vim.opt ,name ,value)))

{: set!}
