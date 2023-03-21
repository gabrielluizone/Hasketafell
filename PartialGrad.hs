main = do putStrLn "Derivada Parciais de uma Função"

data MultiVarFunc = X Int -- variável x_n
                  | Sin MultiVarFunc -- sen(g(x_1, x_2, ..., x_n))
                  | Cos MultiVarFunc -- cos(g(x_1, x_2, ..., x_n))
                  | Exp MultiVarFunc -- e^g(x_1, x_2, ..., x_n)
                  | Ln MultiVarFunc -- ln(g(x_1, x_2, ..., x_n))
                  | Power MultiVarFunc Double -- g(x_1, x_2, ..., x_n)^n
                  | Const Double -- constante
                  | Sum MultiVarFunc MultiVarFunc -- h(x_1, x_2, ..., x_n) = f(x_1, x_2, ..., x_n) + g(x_1, x_2, ..., x_n)
                  | Mult MultiVarFunc MultiVarFunc -- h(x_1, x_2, ..., x_n) = f(x_1, x_2, ..., x_n) * g(x_1, x_2, ..., x_n)
                  | Div MultiVarFunc MultiVarFunc -- h(x_1, x_2, ..., x_n) = f(x_1, x_2, ..., x_n) / g(x_1, x_2, ..., x_n)
                  deriving (Show)

-- Calcular a derivada parcial de uma expressão em relação a uma variável específica
-- [Uso] partial (função) x, y, z, ...
partial :: MultiVarFunc -> Int -> MultiVarFunc
partial (X n) m
    | n == m = Const 1
    | otherwise = Const 0
partial (Sin f) m = Mult (Cos f) (partial f m)
partial (Cos f) m = Mult (Mult (Const (-1)) (Sin f)) (partial f m)
partial (Exp f) m = Mult (Exp f) (partial f m)
partial (Ln f) m = Div (partial f m) f
partial (Power f n) m
    | n == 0 = Const 0
    | n == 1 = partial f m
    | otherwise = Mult (Mult (Const n) (Power f (n-1))) (partial f m)
partial (Const _) _ = Const 0
partial (Sum f g) m = Sum (partial f m) (partial g m)
partial (Mult f g) m = Sum (Mult (partial f m) g) (Mult f (partial g m))
partial (Div f g) m = Div (Sum (Mult (partial f m) g') (Mult (Mult (Const (-1)) f') (partial g m))) (Mult g g')
    where f' = simplify f
          g' = simplify g

-- [Exe.] partial (Mult (X 1) (X 2))2

-- Clacular o gradiente de uma expressão em relação a todas as suas variáveis:
grad :: MultiVarFunc -> [MultiVarFunc]
grad f = [partial f n | n <- [1..n_vars]]
  where n_vars = countVars f
        countVars (X n) = 1
        countVars (Sin f) = countVars f
        countVars (Cos f) = countVars f
        countVars (Exp f) = countVars f
        countVars (Ln f) = countVars f
        countVars (Power f _) = countVars f
        countVars (Sum f g) = max (countVars f) (countVars g)
        countVars (Mult f g) = max (countVars f) (countVars g)
        countVars (Div f g) = max (countVars f) (countVars g)
        countVars (Const _) = 0

-- Olha só o Render, Simplify e Shw aqui denovo
simplify :: MultiVarFunc -> MultiVarFunc
simplify (Mult (Const 1) f) = simplify f
simplify (Mult f (Const 1)) = simplify f
simplify (Mult (Const a) (Const b)) = Const (a * b)
simplify (Mult f g) = Mult (simplify f) (simplify g)
simplify (Sum f g) = Sum (simplify f) (simplify g)
simplify (Div f g) = Div (simplify f) (simplify g)
simplify (Power f n) = Power (simplify f) n
simplify f = f

-- função que converte uma função em uma representação visual possível para percepção humana
render :: MultiVarFunc -> String
render (X n) = "x" ++ show n
render (Sin f) = "sin(" ++ render f ++ ")"
render (Cos f) = "cos(" ++ render f ++ ")"
render (Exp f) = "e^(" ++ render f ++ ")"
render (Ln f) = "ln(" ++ render f ++ ")"
render (Power f n) = "(" ++ render f ++ ")^" ++ show n
render (Const k) = show k
render (Sum f g) = render f ++ " + " ++ render g
render (Mult f g) = render f ++ " * " ++ render g
render (Div f g) = "(" ++ render f ++ " / " ++ render g ++ ")"

-- função que simplifica e renderiza uma função
shw :: MultiVarFunc -> String
shw f = rendered
  where
    simplified = simplify f
    rendered = render simplified

shwGrad :: [MultiVarFunc] -> [String]
shwGrad fs = map shw (init fs) ++ [shw (last fs)]

---- Vetor Gradiente de f(x, y) = x^2 + y^2
-- grad (Sum (Power (X 1) 2) (Power (X 2) 2))
-- shwGrad (grad (Sum (Power (X 1) 2) (Power (X 2) 2)))

---- Vetor Gradiente de f(x, y) = xy
-- grad (Mult (X 1) (X 2))
-- shwGrad (grad (Mult (X 1) (X 2)))

-- shw (partial (Mult (Power (X 1) 3) (X 2)) 1)
-- grad (Sum (Power (X 1) 2) (Sin (X 2)))
-- shwGrad (grad (Sum (Power (X 1) 2) (Sin (X 2))))
-- retorna [Mult (Const 2.0) (X 1), Cos (X 2)]
