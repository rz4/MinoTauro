;;; match.hy
;;;

; Imports
(import [hy.models [HyExpression HyList HyKeyword]])
(import [hy.contrib.walk [walk prewalk]])

; linearize
(defn linearize [expr]
  (setv path [])
  (prewalk (fn [x]
               (.append path [(type x)
                              (cond [(= (type x) HyExpression) (len x)]
                                    [(= (type x) HyList) (len x)]
                                    [True x])])
               x)
           expr)
  path)

; kw^type-match?
(defn kw^type-match? [kw type_]
  (setv type_ (get (.split (str type_) "'") 1))
  (setv kw (.join "."(list (rest (.split (str kw) ":")))))
  (if (= -1 (.find type_ kw)) False True))

; pmatch?
(defn pat-match? [expr pat]
  (setv expr_ (linearize expr))
  (setv pat_ (linearize pat))
  (setv flag 0)
  (cond [(> (len pat_) (len expr_)) (return False)]
        [True (for [i (range (len pat_))]
                   (if flag (do (setv expr_ (+ (cut expr_ 0 i)
                                               (cut expr_ (+ i flag) (len expr_))))
                                (setv flag 0)))
                   (setv p (get pat_ i))
                   (setv e (get expr_ i))
                   (cond [(= HyKeyword (get p 0))
                          (if-not (kw^type-match? (get p 1) (get e 0))
                                  (return False)
                                  (setv flag (cond [(= HyExpression (get e 0)) (get e 1)]
                                                   [(= HyList (get e 0) (get e 1))]
                                                   [True 0])))]
                         [(= (type (get p 0)) (type (get e 0)))
                          (if (!= (get p 1) (get e 1)) (return False))]
                         [True (return False)]))])

  True)

; pfind
(defn pat-find [expr pat &optional [n -1]]
      (setv hits '[])
      (prewalk (fn [x]
                   (if (or (= n -1) (< (len hits) n))
                       (if (pat-match? x pat) (.append hits x)))
                   x)
               expr)
      hits)

; prefract
(defmacro pat-refract [expr &rest pat-tuples]
      (setv expr_ (identity expr))
      (for [pt pat-tuples]
        (setv pat (get pt 0))
        (setv ref (get pt 1))
        (setv expr_ (prewalk (fn [x]
                               (if (pat-match? x pat) (setv x ref))
                               x) expr_)))
      expr_)
