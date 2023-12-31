(ns advent-of-code.2017.2
  (:require [clojure.string :as cs]
            [clojure.math.combinatorics :refer [combinations]]))

(def raw-input
  "3093\t749\t3469\t142\t2049\t3537\t1596\t3035\t2424\t3982\t3290\t125\t249\t131\t118\t3138\n141\t677\t2705\t2404\t2887\t2860\t1123\t2714\t117\t1157\t2607\t1800\t153\t130\t1794\t3272\n182\t93\t2180\t114\t103\t1017\t95\t580\t2179\t2470\t2487\t2806\t1574\t1325\t1898\t1706\n3753\t233\t3961\t3747\t3479\t3597\t1303\t2612\t4043\t1815\t3318\t737\t197\t3943\t239\t254\n113\t147\t961\t157\t3514\t3045\t1270\t3528\t1369\t3377\t492\t156\t1410\t3251\t1839\t1249\n3948\t3651\t888\t3631\t253\t220\t4266\t1284\t3595\t237\t2138\t3799\t2319\t254\t267\t1182\n399\t446\t795\t653\t154\t762\t140\t487\t750\t457\t730\t150\t175\t841\t323\t492\n999\t979\t103\t99\t1544\t1404\t100\t1615\t840\t92\t1552\t1665\t1686\t76\t113\t1700\n4049\t182\t3583\t1712\t200\t3326\t3944\t715\t213\t1855\t2990\t3621\t2560\t842\t249\t2082\n2610\t4749\t2723\t2915\t2189\t3911\t124\t164\t1895\t3095\t3992\t134\t127\t4229\t3453\t4428\n105\t692\t101\t150\t193\t755\t84\t185\t622\t851\t706\t251\t86\t408\t774\t831\n238\t217\t224\t1409\t1850\t2604\t363\t265\t596\t2933\t2641\t2277\t803\t2557\t1399\t237\n304\t247\t192\t4369\t997\t5750\t85\t1248\t4718\t3888\t5228\t5116\t5880\t5348\t6052\t245\n238\t373\t228\t395\t86\t59\t289\t87\t437\t384\t233\t79\t470\t403\t441\t352\n151\t3473\t1435\t87\t1517\t1480\t140\t2353\t1293\t118\t163\t3321\t2537\t3061\t1532\t3402\n127\t375\t330\t257\t220\t295\t145\t335\t304\t165\t151\t141\t289\t256\t195\t272")

(def rows
  (map (fn [line] (map parse-long (cs/split line #"\t")))
       (cs/split-lines raw-input)))

(defn row->diff
  [r]
  (- (apply max r) (apply min r)))                          ;; too lazy to write single-pass impl.

(->> rows                                                   ;; solve part one
     (map row->diff)
     (reduce +))

(defn integer-quotients
  "Returns sequence of integer quotients for all combinations of numbers in coll."
  [coll]
  (->> (combinations (sort #(compare %2 %1) coll) 2)
       (sequence
        (comp
         (map (partial apply /))
         (filter integer?)))))

(->> rows                                                   ;; solve part two
     (map (comp first integer-quotients))
     (reduce +))

(transduce                                                  ;; or solve part two with transduce
 (comp
  (map integer-quotients)
  (map first))
 +
 rows)
