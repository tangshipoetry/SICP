#lang racket



;网上的,还没看
Switching from a quick-and-dirty variable renaming function to an environment structure
like the rest of Scheme would have to involve an implementation that solves the problem of carrying around frames that are never evaluated.
I would propose what Daniel P. Friedman and David S.
Wise referred to as "Suicidal Suspensions" in their seminal 1976 paper on non-strict evaluation in Lisp,
"Cons Should Not Evaluate Its Arguments,"
where they proved such a method would not involve more calls to eval/apply than McCarthy's strict interpreter.
This would entail terminating all unevaluated environments once a given query has returned,
which means both that the system should use the occasion to batch evaluate this set of environments
(ideally using the linear-time tree-based search from exercise 4.76),
as well as checking for repetition in its history in order to prevent infinite loops (along the lines of exercise 4.67),
as either would be particularly disastrous once the offending environments have been disposed of.




