(labels [f1 (code [x] [y] (+ x y))]
  (let [x 1 f (closure f1 [(inc x)])]
    (fncall f x))))