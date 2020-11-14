### Vision: What are you trying to achieve? 
We are currently trying to create a basic programming language with optional function memoization. We believe that to truly make this project our own, we need to create the language from scratch. We plan to have a special function tag that stors tuples of inputs and outputs of functions in order to speed up execution. We also plan to implement a garbage collector that will discard unused tuples in order to avoid hypothetical memory leaks. We will also implement a function that shows what tuples are stored in our dynamic map.

### Status: What’s the status of your current prototype? Showing some test cases or a screenshot might be helpful.
We have a basic implementation that is based on IMP. Our addition to the IMP language was a __lst__ data type for lists of commands and a __match__ and __switch__ function. we were not able to test our implementation yet. We are currently working on getting the parser and lexer to recognize this data type.

![AST!](../aplha.png "AST")

![Eval!](../aplha_eval.png "Eval")

### Next steps: What do you plan to do next?
Get the parser and lexer working then get closures running.
