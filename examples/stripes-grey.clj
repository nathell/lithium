(do (init-graph) 
    (loop [x 0 y 0]
      (put-pixel x y (let [z (mod (+ x y) 32)]
                       (+ 16 (if (< z 16) z (- 31 z)))))
      (if (= y 200)
        nil
        (if (= x 319)
          (recur 0 (inc y))
          (recur (inc x) y)))))