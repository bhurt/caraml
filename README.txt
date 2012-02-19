
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


The modules Type.ml, Info.ml, Common.ml, and Error.ml define data
structures common between many if not all passes.

The LLVM Monad tree:

In handling the LLVM code, we have a set of concentric rings where
we have more and more context.  So we're using pa_monad and a reader
monad to represent this.  The concentric rings are:

    Reader      (standard reader monad)
    Context     (we have an llcontext)
    Module      (we have an llmodule and llcontext)
    Function    (we have a function, an llmodule, and an llcontext)
    Block       (we have a basic block, a builder, a function, etc.)

