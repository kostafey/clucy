(ns clucy.test.core
  (:use clucy.core
        clojure.test
        [clojure.set :only [intersection]]))

(def people [{:name "Miles" :age 36}
             {:name "Emily" :age 0.3}
             {:name "Joanna" :age 34}
             {:name "Melinda" :age 34}
             {:name "Mary" :age 48}
             {:name "Mary Lou" :age 39}])

(deftest core

  (testing "memory-index fn"
    (let [index (memory-index)]
      (is (not (nil? index)))))

  (testing "disk-index fn"
    (let [index (disk-index "/tmp/test-index")]
      (is (not (nil? index)))))

  (testing "add fn"
    (let [index (memory-index)]
      (doseq [person people] (add index person))
      (is (== 1 (count (search index "name:miles" 10))))))

  (testing "delete fn"
    (let [index (memory-index)]
      (doseq [person people] (add index person))
      (delete index (first people))
      (is (== 0 (count (search index "name:miles" 10))))))

  (testing "search fn"
    (let [index (memory-index)]
      (doseq [person people] (add index person))
      (is (== 1 (count (search index "name:miles" 10))))
      (is (== 1 (count (search index "name:miles age:100" 10))))
      (is (== 0 (count (search index "name:miles AND age:100" 10))))
      (is (== 0 (count (search index "name:miles age:100" 10 :default-operator :and))))))

  (testing "search-and-delete fn"
    (let [index (memory-index)]
      (doseq [person people] (add index person))
      (search-and-delete index "name:mary")
      (is (== 0 (count (search index "name:mary" 10))))))

  (testing "search fn with highlighting"
    (let [index (memory-index)
          config {:field :name}]
      (doseq [person people] (add index person))
      (is (= (map #(-> % meta :_fragments)
                  (search index "name:mary" 10 :highlight config))
             ["<b>Mary</b>" "<b>Mary</b> Lou"]))))

  (testing "search fn returns scores in metadata"
    (let [index (memory-index)
          _ (doseq [person people] (add index person))
          results (search index "name:mary" 10)]
      (is (true? (every? pos? (map (comp :_score meta) results))))
      (is (= 2 (:_total-hits (meta results))))
      (is (pos? (:_max-score (meta results))))
      (is (= (count people) (:_total-hits (meta (search index "*:*" 2)))))))

  (testing "pagination"
    (let [index (memory-index)]
      (doseq [person people] (add index person))
      (is (== 3 (count (search index "m*" 10 :page 0 :results-per-page 3))))
      (is (== 1 (count (search index "m*" 10 :page 1 :results-per-page 3))))
      (is (empty? (intersection
                    (set (search index "m*" 10 :page 0 :results-per-page 3))
                    (set (search index "m*" 10 :page 1 :results-per-page 3)))))))

  (testing "Explanations"
    (let [i (memory-index)]
      (apply add i people)
      (let [exp (-> (search i "Miles" 10 :explain true)
                    (first)
                    (meta)
                    (:explain))]
        (is (map? exp))
        (is (:match? exp))
        (is (string? (:description exp)))
        (is (float? (:value exp)))
        (is (seq? (:children exp))))))

  (testing "Boosting"
    (let [i (memory-index)]
      (add i
           (with-meta {:planet "Earth Mk. II" :designer "Slartibartfast"}
             {:name {:boost 0.0}})
           (with-meta {:planet "Earth" :designer "Slartibartfast"}
             {:name {:boost 1.0}}))
      (is (= "Earth" (:planet (first (search i "Slartibartfast" 2)))))))
  (testing "Array fields"
           (let [i (memory-index)]
             (add i {:name "Paul"
                     :likes ["bread" "onions"]})
             (is (= ["bread" "onions"] (:likes (first 
                                                 (search i "Paul" 10)))))))
  (testing "Nested maps"
           (let [i (memory-index)]
             (add i {:book {:author {:first-name "Dostoyevsky"
                                     :last-name "Fyodor"}
                            :title "Crime and Punishment"}})
             (is (= {:first-name "Dostoyevsky"
                     :last-name "Fyodor"}
                    (:author (:book (first (search i "Crime and Punishment" 10)))))))))
