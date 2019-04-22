;;; match.hy
;;;

; Imports
(import [hy.models [HyExpression HyList HyKeyword HySymbol]])
(import [hy.contrib.walk [walk prewalk]])
(require [hy.contrib.walk [let]])

;; linearize
(defn linearize [expr]
    (let [seq '[]]
      (setv flag False)
      (setv s (gensym))
      (+= seq (cond [(= HyExpression (type expr)) `[(~s)]]
                    [(= HyList (type expr)) `[[~s]]]
                    [True (do (setv flag True) `[~expr])]))
      (if flag (return seq))
      (walk (fn [x]
                (+= seq (cond [(= HyExpression (type x)) (linearize x)]
                              [(= HyList (type x)) (linearize x)]
                              [True  `[~x]]))
                x)
            identity
            expr)
      (+= seq (cond [(= HyExpression (type expr)) `[(~s)]]
                    [(= HyList (type expr)) `[[~s]]]))
      seq))

; kw^type-match?
(defn kw^type-match? [kw type_]
  (setv type_ (.split (get (.split (str type_) "'") 1) "."))
  (setv kw (list (rest (.split (str kw) ":"))))
  (for [i (range (len type_))]
       (setv flag True)
       (for [j (range (len kw))]
            (if-not (= (get type_ (- i j 1)) (get kw (- 0 j 1)))
                (do (setv flag False) (break))))
       (if flag (return True)))
  False)

; pmatch?
(defn pat-match? [expr pat]
  (setv expr_ (linearize expr))
  (setv pat_ (linearize pat))
  (setv flag False)
  (cond [(> (len pat_) (len expr_)) (return False)]
        [True (for [i (range (len pat_))]
               (if flag
                   (do (setv gap 0)
                       (while (if (!= flag True)
                                  (if (hasattr (get expr_ (+ i gap)) "__len__")
                                      (!= flag (get (get expr_ (+ i gap)) 0))
                                      True)
                                  (and (!= HyList (type (get expr_ (+ i gap))))
                                       (!= HyExpression (type (get expr_ (+ i gap))))))
                              (setv gap (+ 1 gap)))
                       (if (!= flag True) (+= gap 1))
                       (setv expr_ (+ (cut expr_ 0 i) (cut expr_ (+ i gap) (len expr_))))
                       (setv flag False)))
               (setv e (get expr_ i))
               (setv p (get pat_ i))
               (if (and (= HySymbol (type p)) (= "&rest" (str p)))
                   (do (setv flag True) (continue)))
               (if (and (= HySymbol (type (get pat_ (- i 1)))) (= "&rest" (str (get pat_ (- i 1)))))
                  (do (setv expr_ (+ '[()] expr_))  (continue)))
               (cond [(= HyKeyword (type p))
                      (if-not (kw^type-match? p (type e))
                              (return False)
                              (setv flag (cond [(= HyExpression (type e)) (get e 0)]
                                               [(= HyList (type e)) (get e 0)]
                                               [True False])))]

                     [(= (type p) (type e))
                      (cond [(= HyExpression (type p)) (continue)]
                            [(= HyList (type p)) (continue)]
                            [(!= p e) (return False)])]
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
