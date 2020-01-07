;;; mu.hy
;; Updated: 1/4/2020
;; File defines PyTorch Module macros for Minotauro development environment.
;;
;; Design Principles:
;;
;; - Should function as a minimal wrapper for PyTorch's Module Objects.
;;
;; The rise of data-driven differential learning systems shows promise at learning arbitrary functions.
;; As data becomes more easily available, future data scientists will no longer need to explicitly define
;; lambda functions to preform data transformations, but will be able to define specfications of inputs and outputs
;; and fit "learnable lambdas" accordingly.
;;
;; Aside from methods for training learnable parameters, we will also need methods
;; for exploring different model architectures in a semi-supervised fashion. The trend in contemporary models is towards
;; model specificity in order to increase predicitive performance on a given problem domain, those models which exploit domain-
;; specific information along the structure of the model's computational graph out perform arbitrary methods.
;; Intuition leads us to assume that certain classes of computational graphs outperform others on a given predicition task, for
;; example image recognition appears to be best solved by computational graphs employing local 2D convolution.
;; Though packages like Keras and Tensorflow abstract the 2D convolution operation into a psuedo-primitive (Layer), 2D
;; convolutions can be implemented as a computational graph which employs only primitive tensor operators such as reshapes,
;; transposes, and matrix multiplications. If we reduce our options when designing models to a set of primitive operations, we
;; may be able to more easily see the relation between graph structure and predicitive ability. With these insights we may also
;; be able to infer which class of graph structure will best fit the given data and make searching the possible space of
;; differential learning models analagous to a taxtonomy search.
;;
;; Given Lisp's ability to manipulate arbitrary data structures as lists (a more subtle examination of Lisp shows
;; we are actually operating over a set of binary tree graphs),
;; Lisp has been widely adopted as the best langauge to implement searches over
;; graph structures as seen in many implementations of "genetic programs" and formal mathematical systems.
;; Lisp provides a framework which allows computational graphs to be manipulated as data.
;;
;; This project explores the use of Lisp to make the programming of computational graph searches more transparent and
;; manageable with tools adopted for data-oriented functional programming languages like Clojure and Haskell.
;; The system is constructed with the use of macros (programs which process code as data) to reduce the overhead of the system,
;; ensure the same performance typical of native PyTorch applications, and to allow PyTorch's Modules to be expressed as first-
;; class functions.
;;
;; The goal of the project is to create tools to quickly iterate over as many possible computational graphs structures
;; defined with primitive operations, not simply shallow search over a set of custom defined hyper parameters.
;; The result is a coding environment which gives more flexability than any other DL environment. From experience,
;; this framework also makes developing and implementing new models a fast process, and will serve as a development
;; environment for agile workflows or as power multiplier for small development teams.
;;
;; For those bored out of their mind:
;; In short, Minotauro gives you the ability to implement lambda-esk PyTorch Modules with any number of required
;; arguments and allows you to default any of those arguments to trainable parameters. And it all works with native PyTorch!
;;
;; To use macros, import using:
;; (require [minotauro.mu [*]])

; Imports
(import hy [hy.contrib.walk [macroexpand-all]])

;-----MODULES------

(defmacro defmu [module-name required-components &rest forward-procedure]
  """ PyTorch Module Sigmod Definition Macro:
  Macro expands to a new namespaced PyTorch Module Class definition.
  Macro mirrors the definition of namespaced lambda functions and allows PyTorch Modules to
  be expressed as first-class functions with bind-able components.

  The arguments are as follows:
  - module-name: the name of the PyTorch Module Sigmod Class
  - required-components: the list of parameters required by the forward procedure
  - forward-procedure: the procedure definition

  All parameters are treated as required components for the forward procedure as they are
  set as local variables of the PyTorch Module for stable opertation broadcasting to
  sub-components. This allows the PyTorchs auto differentiation to chain mu definition over
  the full computational graph and for movement of the graphs to and from devices.
  Also allows access to any sub-module and parameters of a module using Pythonic dot notation.

  When creating a new instance of a mu, default values for the components can be set.
  These will be used during forward propagation unless overloaded with other values during
  the forward call.

  If there at least two body forms, and the first of them is a string literal, this string
  becomes the docstring of the module.
  """
  ; Argument Asserts
  (assert (instance? HySymbol module-name)
    (.format "Arg 1 must be HySymbol. Found {t}" :t (name (type module-name))))
  (assert (instance? HyList required-components)
    (.format "Arg 2 must be HyList. Found {t}" :t (name (type require-components))))
  (assert (every? (fn [x] (instance? HySymbol x)) required-components)
    "Arg 2 must be HyList of HySymbols.")
  (assert (> (len forward-procedure) 0)
    "Forward procedure must be defined.")

  ; Generate default args expressions
  (setv args (lfor c required-components [c None]))

  ; Generate init-body
  (setv init-body '(setv))
  (for [c required-components] (+= init-body `((. self ~c) ~c)))

  ; Generate component dispatcher
  (setv dispatcher '(setv))
  (for [c required-components] (+= dispatcher `(~c (if (none? ~c) (. self ~c) ~c))))

  ; Macro expand forward-procedure & check for doc string
  (setv forward-procedure (macroexpand-all forward-procedure)
        doc-str "")
  (when (and (> (len forward-procedure) 2) (instance? str (first forward-procedure)))
    (setv forward-procedure (rest forward-procedure)
          doc-str (first forward-procedure)))

  ; Expression
  `(do (import [torch.nn [Module]]
               [hy.contrib.hy-repr [hy-repr]])
       (defclass ~module-name [Module]
         ~doc-str
         (defn __init__ [self &optional ~@args]
           (.__init__ (super ~module-name self))
           ~init-body
           (setv self.expression (cut (hy-repr (quote ~@forward-procedure)) 1)
                 self.arguments (cut (hy-repr (quote [~@required-components])) 1)))
         (defn forward [self &optional ~@args]
           ~dispatcher
           ~@forward-procedure)
         (defn extra_repr [self]
           (setv params (.join "\n" (lfor (, key param) (self._parameters.items)
                                      (.format "{k}: Parameter(size: {s} dtype: {d})"
                                        :k key
                                        :s (cut (get (.split (name (.size param)) "(") -1) 0 -1)
                                        :d param.dtype))))
           (setv repr- (+ "C: " self.arguments "\nλ: " self.expression))
           (if (> (len params) 0) (setv repr- (+ repr- "\n\n" params "\n")) (+= repr- "\n"))
           repr-))))

(defmacro mu [required-components &rest forward-procedure]
  """ PyTorch Anonymous Module Sigmod Definitions Macro:
  Macro expands to a new instance of the defined PyTorch Module Class.
  Macro mirrors the definition of namespaced lambda functions and allows PyTorch Modules to
  be expressed as first-class functions with bind-able components.

  The arguments are as follows:
  - required-components: the list of parameters required by the forward procedure
  - forward-procedure: the procedure definition

  For more information on the mechanics of the mu formalism, refer to the defmu
  documentation.
  """
  ; Argument Asserts
  (assert (instance? HyList required-components)
    (.format "Arg 1 must be HyList. Found {t}" :t (name (type require-components))))
  (assert (every? (fn [x] (instance? HySymbol x)) required-components)
    "Arg 1 must be HyList of HySymbols.")
  (assert (> (len forward-procedure) 0)
    "Forward procedure must be defined.")

  ; Generate default args expressions
  (setv args (lfor c required-components [c None]))

  ; Generate init-body
  (setv init-body '(setv))
  (for [c required-components] (+= init-body `((. self ~c) ~c)))

  ; Generate dispatcher
  (setv dispatcher '(setv))
  (for [c required-components] (+= dispatcher `(~c (if (none? ~c) (. self ~c) ~c))))

  ; Macro expand forward-procedure
  (setv forward-procedure (macroexpand-all forward-procedure))

  `(do (import [torch.nn [Module]])
       ((type "" (, Module)
         { "__init__"
           (fn [self &optional ~@args]
             (.__init__ (super (type self) self))
             ~init-body
             (setv self.expression (cut (hy-repr (quote ~@forward-procedure)) 1)
                   self.arguments (cut (hy-repr (quote [~@required-components])) 1)))
           "forward"
           (fn [self &optional ~@args]
             ~dispatcher
             ~@forward-procedure)
           "extra_repr"
           (fn [self]
             (setv params (.join "\n" (lfor (, key param) (self._parameters.items)
                                        (.format "{k}: Parameter(size: {s} dtype: {d})"
                                          :k key
                                          :s (cut (get (.split (name (.size param)) "(") -1) 0 -1)
                                          :d param.dtype))))
             (setv repr- (+ "C: " self.arguments "\nλ: " self.expression))
             (if (> (len params) 0) (setv repr- (+ repr- "\n\n" params)) (+= repr- "\n"))
             repr-)
           "__repr__"
           (fn [self] (+ "μ" (.__repr__ (super (type self) self))))}))))

(defmacro bind [mu &rest args]
  """ Sigmod Component Binding Macro:
  Macro expands to a default-component setter for Pytorch Module mus.
  Evaluated expression returns the newly initialized mu.

  Macro takes in the following arguments:
  - mu: the mu object
  - args: the list of component name and value pairs.

  Arguments following the mu are pairs of HyKeywords and values.
  Same as defining optional arguments in functions. HyKeywords should refer to
  a named component in the mu.
  """

  ; Re-initialize PyTorch Module with new components
  (setv var (gensym))
  `(do (import [torch.nn [Module]])
       (assert (instance? Module ~mu)
         (.format "Argument 1 must be a torch.nn.Module. Found {t}" :t (name (type ~mu))))
       (setv ~var ~mu)
       (.__init__ ~var ~@args) ~var))

;-----COMMON-UTILITIES------

(defmacro geta [x &rest args]
  """
  Macro for mulitdimensional indexing of Numpy, Pandas, and PyTorch arrays:
  Similar to native python multi-array indexing.

  Example:
  (get x [:] [:] [1])

  equivalent to

  x[:,:,1]
  """
  (setv slices []
        x-pointer x)
  (for [_ args]
    (setv s '() i 0 t 0)
    (for [j _]
      (cond [(and (= ': j) (= 0 t)) (do (.append s i) (setv t 1 i `(len ~x-pointer)))]
            [(and (= ': j) (= 1 t)) (do (.append s i) (setv i 1))]
            [True (setv i j)]))
    (.append s i)
    (setv x-pointer `(get ~x-pointer 0))
    (if (< 1 (len s)) (.append slices `(slice ~@s)) (.append slices (get s 0))))
  `(get ~x (, ~@slices)))
