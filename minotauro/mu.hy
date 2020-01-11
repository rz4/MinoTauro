;; """
;; mu.hy
;; Updated: 1/8/2020
;; File defines PyTorch Module macros for Minotauro development environment.
;;
;; Design Principles:
;;
;; - Should function as a minimal wrapper for PyTorch's Module Objects.
;;
;; The rise of data-driven differential learning (DL) systems shows promise at learning arbitrary
;; data transformations. As information becomes increasingly available, future data scientists will need
;; tools to quickly parse through various data streams and filter out erronious signals from those streams.
;; Luckily, DL programming provides a route to define functional specfications (of inputs and outputs) and
;; fit "learnable lambdas" according to those specifications.
;;
;; Aside from methods for training learnable parameters, we will also need methods
;; for exploring different model architectures in a semi-supervised fashion. The trend
;; in contemporary models is towards model specificity in order to increase predicitive
;; performance on a given problem domain. Those models, which exploit domain-specific
;; information along the structure of the model's computational graph, outperform arbitrary methods.
;;
;; Intuition leads us to assume that certain classes of computational graphs outperform others on
;; a given predicition task, for example the field of image recognition appears to be dominated by
;; scomputational graphs employing local 2D convolution. Though packages like Keras and Tensorflow
;; abstract the 2D convolutionoperation into a psuedo-primitive (Layer), 2D convolutions have varying
;; implementations, all of which are permutations of computational graphs which may employ any combination
;; of lower-level tensor operators such as matrix reshapes, matrix transposes, and matrix products.
;;
;; If we define our search space to be models contructed from a base set of primitive operators, we
;; will see the relation between models and predicitive ability as one that correlates with the class
;; of graph structure employed by the model. These insights will allow us to better infer which class
;; of graph structure will best fit certain data specifications and make searching the possible space
;; of differential learning models analagous to searching through a taxtonomy of learnable functions
;; for a given structure that best fits the training data.
;;
;; Given Lisp's ability to manipulate arbitrary data structures as lists,
;; Lisp has been widely adopted as the best langauge to implement searches over
;; graph structures as seen in many implementations of "genetic programs" and derivation engines
;; for formal mathematical systems. Lisp provides the framework needed to manipulate computational
;; graphs as data. Once the DL system code is formated as data, a formal langauge for DL computation
;; will be accessable to any other program which is able to function off that model data representation.
;;
;; The link between DL systems and traditional symbolic program is a Lisp which encapsulates the
;; interaction between those two components. Meta-learning and Reinforcement Learning will then be
;; able to cast the problem of meta-learning from an esoteric "deep learning" space to the symbolic
;; playground which has been well established to be the best environment to express recursion,
;; formal systems, and probabilistic models.
;;
;; This project explores the use of Lisp to make meta-programming of computational graphs more
;; transparent and manageable with tools adopted for data-oriented functional programming languages
;; like Clojure and Haskell.
;;
;; The system is constructed using macros (programs which process code as data) to reduce the
;; overhead of the system, ensure similar performance typical of native PyTorch applications,
;; and to allow PyTorch's Modules to be expressed as first-class objects.
;;
;; The goal of the project is to create tools which will allow the user to quickly iterate over as
;; many possible computational graphs structures defined from a set of primitive operators, and not
;; simply a shallow search over a set of custom defined hyper parameters.
;;
;; The result is a coding environment which gives more flexability than any other DL environment.
;; From experience, this framework also makes developing and implementing new models a fast process,
;; and will serve as a development environment for agile workflows and as a power multiplier for very
;; small development teams.
;;
;; Minotauro let's you implement lambda-esk "anonymous PyTorch Modules" referred to as "mu expressions".
;; Minotauro let's you bind parameters and modules to a mu expression.
;; Minotauro gives you Pythonic data accessing calls.
;; Minotauro allows you to revert a module back to an S-Expression.
;; You are allowed to do anything with it.
;;
;; And it all works with native PyTorch!
;;
;; To use macros, import using:
;; (require [minotauro.mu [*]])
;; """

; Imports
(import hy
        [hy.contrib.walk [macroexpand-all]])

;-----MU-Expressions------

(defmacro register-mu []
  `(do (import [hy.contrib.hy-repr [hy-repr-register hy-repr]])
       (setv mu-repr (fn [x]
                      (.format "(mu {args} {forms})"
                         :args (cut (hy-repr x.arguments) 1)
                         :forms (cut (hy-repr x.expression) 1))))
       (hy-repr-register Module mu-repr)))

(defmacro defmu [module-name required-components &rest forward-procedure]
  """ PyTorch Module Mu-Expression Definition Macro:
  Macro expands to a new namespaced PyTorch Module Class definition.
  Macro mirrors the definition of namespaced lambda functions and allows PyTorch Modules to
  be expressed as first-class functions with bind-able components.

  The arguments are as follows:
  - module-name: the name of the PyTorch Module Class
  - required-components: the list of parameters required by the forward procedure
  - forward-procedure: the procedure definition

  All parameters are treated as required components for the forward procedure as they are
  set as local variables of the PyTorch Module for stable opertation broadcasting to
  sub-components. This allows the PyTorchs auto differentiation to chain Mu definitions over
  the full computational graph and for movement of the graphs to and from devices.
  Also allows access to any sub-module and parameters of a module using Pythonic dot notation.

  When creating a new instance of a mu, default values for the components can be set through keyword arguments.
  These will be used during forward propagation unless overloaded with other values during
  the forward call.

  The extra_repr has been overloaded to show components, forward-procedure expressions, and sub-modules/parameters.

  A hy-repr is registered which when called on a module, it returns Hy code of the module as a mu expression
  of the full computational graph with substutions of all nested modules into the root module.

  If there at least two body forms, and the first of them is a string literal, this string
  becomes the docstring of the module class.
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
  (for [c required-components]
    (+= init-body `((. self ~c) ~c)))

  ; Generate component dispatcher
  (setv dispatcher '(setv))
  (for [c required-components]
    (+= dispatcher `(~c (if (none? ~c)
                          (. self ~c)
                          ~c))))

  ; Generate Required Argument Asserter
  (setv asserters '[])
  (for [c required-components]
    (+= asserters `((assert (not (none? ~c))
                      (.format "Missing Component: {m}" :m (quote ~c))))))

  ; Macro expand forward-procedure & check for doc string
  (setv forward-procedure (macroexpand-all forward-procedure)
        doc-str "")
  (when (and (> (len forward-procedure) 2) (instance? str (first forward-procedure)))
    (setv forward-procedure (rest forward-procedure)
          doc-str (first forward-procedure)))

  ; Mu Expression Expansion
  `(do (import [torch.nn [Module]]
               [hy.contrib.hy-repr [hy-repr]])
       (register-mu)
       (defclass ~module-name [Module]
         ~doc-str
         (defn __init__ [self &optional ~@args]
           (.__init__ (super ~module-name self))
           ~init-body
           (setv self.identifier (repr (name (gensym)))
                 self.expression (quote ~@forward-procedure)
                 self.arguments (quote [~@required-components])))
         (defn forward [self &optional ~@args]
           ~dispatcher
           ~@asserters
           ~@forward-procedure)
         (defn extra_repr [self]
           (setv params (lfor (, key param) (self._parameters.items)
                          (.format "{k}: Parameter(size: {s} dtype: {d})"
                            :k key
                            :s (-> param .size name (.split "(") last (cut 0 -1))
                            :d param.dtype))
                 params (if (empty? params) "" (+ "\n" (.join "\n" params) "\n")))
           (+ "ID: " (cut self.identifier 1 -1) "\n"
              "C: " (cut (hy-repr self.arguments) 1) "\n"
              "λ: " (cut (hy-repr self.expression) 1) "\n"
              params)))))

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
  (for [c required-components]
    (+= init-body `((. self ~c) ~c)))

  ; Generate component dispatcher
  (setv dispatcher '(setv))
  (for [c required-components]
    (+= dispatcher `(~c (if (none? ~c)
                          (. self ~c)
                          ~c))))

  ; Generate Required Argument Asserter
  (setv asserters '[])
  (for [c required-components]
    (+= asserters `((assert (not (none? ~c))
                      (.format "Missing Component: {m}" :m (quote ~c))))))

  ; Macro expand forward-procedure
  (setv forward-procedure (macroexpand-all forward-procedure))

  `(do (import [torch.nn [Module]]
               [hy.contrib.hy-repr [hy-repr]])
       (register-mu)
       ((type "μ" (, Module)
         { "__init__"
           (fn [self &optional ~@args]
             (.__init__ (super (type self) self))
             ~init-body
             (setv self.identifier (repr (name (gensym)))
                   self.expression (quote ~@forward-procedure)
                   self.arguments (quote [~@required-components])))
           "forward"
           (fn [self &optional ~@args]
             ~dispatcher
             ~@asserters
             ~@forward-procedure)
           "extra_repr"
           (fn [self]
             (setv params (lfor (, key param) (self._parameters.items)
                            (.format "{k}: Parameter(size: {s} dtype: {d})"
                              :k key
                              :s (-> param .size name (.split "(") last (cut 0 -1))
                              :d param.dtype))
                   params (if (empty? params) "" (+ "\n" (.join "\n" params) "\n")))
             (+ "ID: " (cut self.identifier 1 -1) "\n"
                "C: " (cut (hy-repr self.arguments) 1) "\n"
                "λ: " (cut (hy-repr self.expression) 1) "\n"
                params))}))))

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
