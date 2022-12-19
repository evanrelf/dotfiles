(fn nil? [x]
  (= nil x))

(fn string? [x]
  (= :string (type x)))

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

(lambda setlocal! [name ?value]
  (assert-compile (sym? name) "expected symbol for name" name)
  (let [name (tostring name)
        value (if (nil? ?value)
                  (not (begins-with? :no name))
                  ?value)
        name (if (and (nil? ?value) (begins-with? :no name))
                 (name:match "^no(.+)$")
                 name)]
    `(tset vim.opt_local ,name ,value)))

(lambda autocmd! [events patterns ...]
  (assert-compile
    (or (string? events) (sequence? events))
    "expected string or sequence for events"
    events)
  (assert-compile
    (or (string? patterns) (sequence? patterns))
    "expected string or sequence for patterns"
    patterns)
  `(vim.api.nvim_create_autocmd
     ,events
     {:group "Evan"
      :pattern ,patterns
      :callback (lambda [] ,...)}))

{: set!
 : setlocal!
 : autocmd!}
