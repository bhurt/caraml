
caramlcc is the main driver.

The general flow of the code is:

        Lexer.mll
    
           |            Produces tokens
           V
    
       Parser.mly
    
           |            Converts into an AST
           V
    
         AST.ml
    
           |            Add types/type check
           V
    
        Annot.ml

           |            Alpha Rename
           V

        Alpha.ml
    
           |            Lambda Lift
           V
    
      LambdaLift.ml

           |            Determine call site optimizations
           V

       CallOpt.ml

           |            Generate LLVM bytecode
           V

         LLVM.ml


The modules Type.ml, Info.ml, Common.ml, and Error.ml define data
structures common between many if not all passes.

