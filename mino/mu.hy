"""
mu.hy
Updated: 8/1/2020
Module defines Mu Expression macros for MinoTauro development environment.

;-- DESIGN PRINCIPLES

- Should function as a lightweight wrapper for PyTorch's Module Objects.
- Should function soley as a macro-imported system.

;-- TLDR

MinoTauro lets you implement lambda-esk 'anonymous PyTorch Modules' referred to as 'mu expressions'.
MinoTauro lets you bind parameters and modules to a mu expression.
MinoTauro promotes Pythonic data accessing calls.
MinoTauro allows you to revert a models back to an S-Expressions.
And it all remains compatible with native PyTorch!

;-- FUTURES

- Reverting models to Hy Representations should also simplify expression.

;--

To use macros, import using:
(require [mino.mu [*]])
"""

;- Imports
(import hy [hy.contrib.walk [macroexpand-all]])

;-----Mu-Expressions-----

;--
(defmacro register-mu []
  """ Mu's Hy Representation Register for hy-repr Package:

  Macroexpands to an expression which defines a hy-repr for mu objects. When
  hy-repr is called on a mu object, a string representing the mu object as an
  unevaluated HyExpression is returned.

  If the Mu object contains any nested Mu objects, those child objects are also
  converted to unevaluated Hy expressions and wrap the parent expression through
  `bind`.

  The operation is then registered to the hy.contrib.hy-repr package if not
  already found in registry.

  Returns:
    (HyExpression) : Expanded expression of hy-repr registry entry for mu object.

  Todo:

    * Add function to simplify all components and sub-components into
      a single mu expression.

  """
  `(do (import re
               [torch.nn [Module Parameter]]
               [dill.source [getsource _namespace]]
               [textwrap [dedent]]
               [hy.contrib.hy-repr [hy-repr-register -registry]])

       ;- Add to hy-repr registry if it doesn't exist
       (unless (in Module -registry)

         ;-
         (defn param-repr [param]
           (.format "(torch.nn.Parameter (torch.empty (, {}) :dtype {}))"
             (.join " " (lfor s (.size param) (str s)))
             param.dtype))

         ;-
         (defn mu-import [module]
           (setv forward (getsource (. module forward))
                 forward (re.split "def forward\(self\,(.*)\)\:" forward)
                 code (dedent (re.sub "self\." "" (get forward 2)))
                 code (re.sub "#.*\\n" "\\n" code)
                 comps (lfor arg (.split (get forward 1) ",")
                             (HySymbol (get (.split (.strip arg) "=") 0))))
           (for [v (dir module)]
             (when (instance? (, Module Parameter)
                              (eval `(. module ~(HySymbol v))))
                   (.append comps (HySymbol v))))
           (, `[~@comps] `((pys ~code))))

         ;- Define Object to Hy Expression String Conversion
         (defn mu-repr [mu]
           (setv path (_namespace mu))
           (when (= "torch.nn" (.join "." (cut path 0 2)))
             (setv v (get (.split (.__repr__ mu) "(" 1) 1)
                   v (.replace v "," " ")
                   v (.replace v "(" "(, ")
                   v (re.sub "(\\S+)\=" ":\\1 " v)
                   v (.join " " (.split v)))
             (return (.format "({} {})"
                              (.join "." path)
                              v)))
           (if (in 'components (dir mu))
             (setv args (. mu components)
                   forms (. mu fexpression)
                   comps {})
             (setv (, args forms) (mu-import mu)
                   comps {}))
           (for [a args]
             (setv aa (HySymbol (mangle a)))
             (when (in aa (dir mu))
               (assoc comps a (eval `(. mu ~aa)))))
           (setv mu-expr (.format "(mu {} {})"
                           (cut (hy-repr args) 1)
                           (cut (hy-repr forms) 2 -1))
                 bindables (flatten (lfor c comps
                                      (do (setv a (get comps c))
                                          (cond [(instance? Module a) [(+ ":" c) (mu-repr a)]]
                                                [(instance? Parameter a) [(+ ":" c) (hy-repr a)]]
                                                [True []])))))
           (if (empty? bindables)
               mu-expr
               (.format "(bind {} {})"
                 mu-expr
                 (.join " " bindables))))

         ;- Register functions
         (hy-repr-register Parameter param-repr)
         (hy-repr-register Module mu-repr))))

;--
(defmacro defmu [module-name required-components &rest forward-procedure]
  """ PyTorch Module Mu-Expression Definition Macro:

  Macroexpands to a new namespaced PyTorch Module class definition.
  Macro syntax mirrors the definition of namespaced functions (defn),
  allowing PyTorch Modules to be expressed with concise function syntax.

  All inputs to the forward call are treated as required components
  and raise a ValueError if a component is not passed during the forward call.
  When creating a new instance a mu, default values for components
  can be set through their corresponding keyword arguments. If set, the value is
  bound to that component argument and will be used during forward propagation
  unless another value is supplied during the forward call. Components binding
  can also be done at any time through the 'bind' macro (refer to documentation
  for 'bind').

  The modules' 'extra_repr' has been overloaded to show their components,
  forward-procedure expression, and sub-modules/parameters.

  A hy-repr is registered which when called on a module, returns Hy code of the
  module as as mu expressions of the full computational graph with substutions of
  all nested components into the root module.

  Like function definitions, a docstring can be assigned for the module class
  with the first form of the forward procedure as a string.

  Args:

    module-name (HySymbol): the name of the new PyTorch Module class.
    required-components (HyList[HySymbol]): the list of parameters/submodules
                                            required in the forward procedure.
    forward-procedure (&rest HyExpression): the operations run during forward propagation.

  Returns:

    (HyExpression): Expanded Pytorch Module class definition code.

  Todo:

    *
  """
  ;- Argument Checks
  (assert (instance? HySymbol module-name)
    (.format "Arg 1 must be HySymbol. Found {}"
             (name (type module-name))))
  (assert (instance? HyList required-components)
    (.format "Arg 2 must be HyList. Found {}"
             (name (type require-components))))
  (assert (every? (fn [x] (instance? HySymbol x)) required-components)
    "Arg 2 must be HyList of HySymbols.")
  (assert (> (len forward-procedure) 0)
    "Arg 3 (forward procedure) must be defined.")

  ;- Generate mu's default argus expressions
  (setv args (lfor c required-components [c None])
        forward-procedure (lfor p forward-procedure p))

  ;- Generate mu's __init__ body
  (setv init-body '(setv))
  (for [c required-components]
    (+= init-body `((. self ~c) ~c)))

  ;- Generate mus's component dispatcher
  (setv dispatcher '(setv))
  (for [c required-components]
    (+= dispatcher `(~c (if (none? ~c)
                          (. self ~c)
                          ~c))))

  ;- Generate mu's required argument checks
  (setv asserters '[])
  (for [c required-components]
    (+= asserters
        `((assert (not (none? ~c))
                  (ValueError (.format "Arg {} is not defined."
                                       (quote ~c)))))))

  ;- Macroexpand forward-procedure & check for doc string
  (setv forward-procedure (macroexpand-all forward-procedure)
        doc-str "")
  (while (instance? HyString (first forward-procedure))
    (+= doc-str (first forward-procedure))
    (setv forward-procedure (cut forward-procedure 1)))

  ;- Expand mu representation register
  (setv register (macroexpand '(register-mu)))

  ;- Mu Expression Expansion to PyTorch module
  `(do (import [torch.nn [Module]]
               [hy.contrib.hy-repr [hy-repr]])
       ~register
       (defclass ~module-name [Module]
         ~doc-str
         (defn __init__ [self &optional ~@args]
           (.__init__ (super ~module-name self))
           ~init-body
           (setv self.address (hex (id self))
                 self.components (quote [~@required-components])
                 self.fexpression (quote (~@forward-procedure))))
         (defn forward [self &optional ~@args]
           ~dispatcher
           ~@asserters
           ~@forward-procedure)
         (defn extra_repr [self]
           (setv params
                 (lfor (, key param) (self._parameters.items)
                   (.format "{}: (Parameter :size {} :dtype {})"
                     key
                     (-> param .size name (.split "(") last (cut 0 -1) (.replace "," ""))
                     param.dtype)))
           (.format "At: {}\nC: {}\nλ: {}\n\n{}"
             self.address
             (cut (hy-repr self.components) 1)
             (cut (hy-repr self.fexpression) 2 -1)
             (if (empty? params) "" (.format "{}\n" (.join "\n" params))))))))

;--
(defmacro mu [required-components &rest forward-procedure]
  """ Anonymous PyTorch Module Mu-Expression Macro:

  Macroexpands to an anonymous PyTorch Module class instance.
  Macro syntax mirrors the definition of anonymous functions (fn),
  allowing PyTorch Modules to be expressed with concise function syntax.

  'mu' is used to create a single instance of a PyTorch Module. For more
  information on the mechanics of the mu objects, refer to 'defmu'
  documentation.

  Args:

    required-components (HyList[HySymbol]): the list of parameters/submodules
                                            required in the forward procedure.
    forward-procedure (&rest): the operations run during forward propagation.

  Returns:

    (HyExpression): Expanded PyTorch Module class instance code.
  """
  ;- Argument Asserts
  (assert (instance? HyList required-components)
    (.format "Arg 1 must be HyList. Found {}"
             (name (type require-components))))
  (assert (every? (fn [x] (instance? HySymbol x)) required-components)
    "Arg 1 must be HyList of HySymbols.")
  (assert (> (len forward-procedure) 0)
    "Forward procedure must be defined.")

  ;- Generate mu's default argus expressions
  (setv args (lfor c required-components [c None])
        forward-procedure (lfor p forward-procedure p))

  ;- Generate mu's __init__ body
  (setv init-body '(setv))
  (for [c required-components]
    (+= init-body `((. self ~c) ~c)))

  ;- Generate mus's component dispatcher
  (setv dispatcher '(setv))
  (for [c required-components]
    (+= dispatcher `(~c (if (none? ~c)
                          (. self ~c)
                          ~c))))

  ;- Generate mu's required argument checks
  (setv asserters '[])
  (for [c required-components]
    (+= asserters
        `((assert (not (none? ~c))
                  (ValueError (.format "Arg {} is not defined."
                                       (quote ~c)))))))

  ;- Macroexpand forward-procedure & check for doc string
  (setv forward-procedure (macroexpand-all forward-procedure))

  ;- Expand mu representation register
  (setv register (macroexpand '(register-mu)))

  ;- Mu Expression Expansion to PyTorch module
  `(do (import [torch.nn [Module]]
               [hy.contrib.hy-repr [hy-repr]])
       ~register
       ((type "μ" (, Module)
         { "__init__"
           (fn [self &optional ~@args]
             (.__init__ (super (type self) self))
             ~init-body
             (setv self.address (hex (id self))
                   self.components (quote [~@required-components])
                   self.fexpression (quote (~@forward-procedure))))
           "forward"
           (fn [self &optional ~@args]
             ~dispatcher
             ~@asserters
             ~@forward-procedure)
           "extra_repr"
           (fn [self]
             (setv params
                 (lfor (, key param) (self._parameters.items)
                   (.format "{}: (Parameter :size {} :dtype {})"
                     key
                     (-> param .size name (.split "(") last (cut 0 -1) (.replace "," ""))
                     param.dtype)))
             (.format "At: {}\nC: {}\nλ: {}\n\n{}"
               self.address
               (cut (hy-repr self.components) 1)
               (cut (hy-repr self.fexpression) 2 -1)
               (if (empty? params) "" (.format "{}\n" (.join "\n" params)))))}))))

;--
(defmacro bind [mu &rest args]
  """ Component Binding Macro:
  Macroexpands to a default-component setter for mu objects.
  Evaluated expression returns the newly initialized mu. Values for
  component are set using their corresponding named HyKeyword.

  Args:

    mu (Mu Object): Mu object instance.
    args (&rest): Paired HyKeywords and HySymbol/HyExpression. (ex. :weight value)

  Returns:

    (HyExpression): Expanded default component setter code.
  """
  ;- Argument Asserts
  (assert (even? (len args))
    "Args must be paired.")
  (assert (every? (fn [x] (and (instance? HyKeyword (first x))
                               (not (instance? HyKeyword (last x)))))
                  (partition args 2))
    "Args must be paired HyKeywords with values.")

  ;- Re-initialize Mu Object with new components
  (setv var (gensym))
  `(do (import [torch.nn [Module]])
       (assert (instance? Module ~mu)
         (.format "Argument 1 must be a torch.nn.Module. Found {}"
                  (name (type ~mu))))
       (setv ~var ~mu)
       (.__init__ ~var ~@args) ~var))

;--
(defmacro mino/apply [iterable lambda]
  """ Anonymous Pytorch Dataset Macro:

  Macroexpands to an anonymous instance of a PyTorch Dataset.
  The Dataset definition is built around a user specified iteratable
  object and a lambda expression applied along each element of the
  iterable object.

  This expression can be passed to a PyTorch DataLoader for
  data batching.

  Args:

    iterable (Object): Object capable of iterated indexing.
    lambda (function): Function used to map data to Tensors.

  Returns:

    (HyExpression) : Expanded PyTorch Dataset code.

  """
  `(do (import [torch.utils.data [Dataset]])
       ((type "" (, Dataset)
          {"__init__"
           (fn [self]
             (.__init__ (super (type self) self))
             (setv self.data ~iterable
                   self.LAMBDA ~lambda
                   self.NB (len self.data)))
           "__len__"
           (fn [self] self.NB)
           "__getitem__"
           (fn [self index]
             (setv x (get self.data index))
             (self.LAMBDA x))}))))

;-----COMMON-UTILITIES------

;--
(defmacro geta [x &rest args]
  """ Macro for mulitdimensional indexing of Numpy, Pandas, and PyTorch arrays:

  Macro provides ease-of-life indexing by allowing
  indexing similar to native python multi-array indexing.

  Example:

    (geta x [:] [:] [1])

    is equivalent to

    x[:,:,1]
  """
  (setv slices '[]
        x-pointer x)
  (for [_ args]
    (setv s '() i 0 t 0)
    (for [j _]
      (cond [(and (= ': j) (= 0 t)) (do (+= s `(~i)) (setv t 1 i `(len ~x-pointer)))]
            [(and (= ': j) (= 1 t)) (do (+= s `(~i)) (setv i 1))]
            [True (setv i j)]))
    (+= s `(~i))
    (setv x-pointer `(get ~x-pointer 0))
    (if (< 1 (len s)) (+= slices `((slice ~@s))) (+= slices `(~(get s 0)))))
  `(get ~x (, ~@slices)))
