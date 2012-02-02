(defstruct foo x)

(defgeneric bar (x)
  (:method ((x foo)) (foo-x x)))
