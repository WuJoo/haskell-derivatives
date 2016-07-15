data Function =                            X | -- example f(x) = x
                                Const Double | -- f(x) = 5
                       Power Function Double | -- f(x) = x^5
                                  E Function | -- f(x) = e^x
                 Exponential Double Function | -- f(x) = 5^x - potem to uogólnić na przypadek funkcja do potęgi funkcja
                         Log Double Function | -- f(x) = log_2 x
                                 Ln Function | -- f(x) = ln x
                                Sin Function | -- f(x) = sin x
                                Cos Function | -- f(x) = cos x
                                 Tg Function | -- f(x) = tg x
                                Ctg Function | -- f(x) = ctg x
                       Sum Function Function | -- f(x) = sin x + x^2
                Difference Function Function | -- f(x) = sin x - x^2
                   Product Function Function | -- f(x) = sin x * x^2
                  Quotient Function Function   -- f(x) = (sin x) / (x^2)
                                               deriving Show



derivate :: Function -> Function
derivate X                 = Const 1
derivate (Const _)         = Const 0
derivate (Power f pow)     = Product (Product (Const pow) (Power f (pow-1))) (derivate f)
derivate (E f)             = Product (E f) (derivate f)
derivate (Exponential a f) = Product (Product (Exponential a f) (Ln (Const a))) (derivate f)
derivate (Log a f)         = Quotient (derivate f) (Product f (Ln (Const a)))
derivate (Ln f)            = Quotient (derivate f) f
derivate (Sin f)           = Product (Cos f) (derivate f)
derivate (Cos f)           = Product (Const (-1)) (Product (Sin f) (derivate f))
derivate (Tg f)            = Quotient (derivate f) (Power (Cos f) 2)
derivate (Ctg f)           = Quotient (Product (Const (-1)) (derivate f)) (Power (Sin f) 2)
derivate (Sum f g)         = Sum (derivate f) (derivate g)
derivate (Difference f g)  = Difference (derivate f) (derivate g)
derivate (Product f g)     = Sum (Product (derivate f) g) (Product f (derivate g))
derivate (Quotient f g)    = Quotient (Difference (Product (derivate f) g) (Product f (derivate g))) (Power g 2)
