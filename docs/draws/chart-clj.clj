(require '[clj-chart.core :as c])
(let [x (range 0 10)
      y (map #(* % %) x)
      chart (c/quick-xy x y)]
  (c/save! "images/chart-clj.svg" chart))
