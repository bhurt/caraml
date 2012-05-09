CARAML: a toy language

Caraml is a toy language in the ML/Ocaml/Haskell tradition.  It started as an implementation of the exercises in Benjamin Pierce's "Types and Programming Languages" book (http://www.amazon.com/Types-Programming-Languages-Benjamin-Pierce/dp/0262162091), but done "for real"- Lex & Yacc for parsing, and compiling to LLVM.

caramlcc is the main driver.

The general flow of the code is:

        Lexer.mll       Produces tokens

           |
           V

       Parser.mly       Converts tokens into an AST.t

           |
           V

         AST.ml         Definition of AST.t, no real code.

           |
           V

        Annot.ml        Add types/type check

           |
           V

        Alpha.ml        Alpha Rename

           |
           V

      MatchReduce.ml    Simplify match statements.

           |
           V

      LambdaConv.ml     Simplify lambda expressions for conversion

           |
           V

       FreeBind.ml      Bind all free variables

           |
           V

      LambdaLift.ml     Lift all lambdas into top level functions

           |
           V

       Simplify.ml      Replace all arguments with let-bound vars

           |
           V

       CallOpt.ml       Call site optimization/tail call detection

           |
           V

       Assembly.ml      Generate LLVM bytecode


The modules Type.ml, Info.ml, Common.ml, Error.ml, and Config.ml define
data structures common between many if not all passes.  Utils.ml define
generic functions (not directly related to caraml) which the standard
library lacks.  LlvmIntf.ml is a wrapper library around the Llvm
library, and LlvmUtils.ml is various common sequences of Llvm
instructions packaged as a library.

