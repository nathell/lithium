(let [a (+ 1 1)]
  (labels [mul (code [x y] (* x y))]
    (labelcall mul (labelcall mul a a) 3)))

;; Should return 30 in :ax