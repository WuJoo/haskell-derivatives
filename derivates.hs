data Function =                            X | -- example f(x) = x
                                   Const Int | -- f(x) = 5
                          Power Function Int | -- f(x) = x^5
                    Exponential Int Function | -- f(x) = 5^x - potem to uogólnić na przypadek funkcja do potęgi funkcja
                         Log Double Function | -- f(x) = log_2 x
                                Sin Function | -- f(x) = sin x
                                Cos Function | -- f(x) = cos x
                                 Tg Function | -- f(x) = tg x
                                Ctg Function | -- f(x) = ctg x
                       Sum Function Function | -- f(x) = sin x + x^2
                Difference Function Function | -- f(x) = sin x - x^2
                   Product Function Function | -- f(x) = sin x * x^2
                  Quotient Function Function   -- f(x) = (sin x) / (x^2)
