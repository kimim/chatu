(require '[clj-chart.core :as c])
(let [x (range 0 10)
      y (map #(* % %) x)
      chart (c/quick-xy x y)]
  (c/save! "draws_out/chart-clj.svg" chart))
