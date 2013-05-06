; Day 2
(defmacro unless [pred then else]
  `(if-not ~pred ~then ~else))

(macroexpand '(unless false (println "then") (println "else")))

;(defprotocol BerthaResult
;  (status [_])
;  (duration [_]))

;(defrecord BerthaV1 [resp]
;  BerthaResult
;  ())
