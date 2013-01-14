(let [x 1 f (fn [y] (+ x y))]
  (f (inc x)))