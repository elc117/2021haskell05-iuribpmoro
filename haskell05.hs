--Nome: Iuri Bernardo Picolini Moro

bmi :: Float -> Float -> String
bmi peso altura = 
  let res = peso / (altura^2)
  in if res <= 18.5 then "ABAIXO" else if res >= 30 then "ACIMA" else "NORMAL"


bmi' :: Float -> Float -> String
bmi' peso altura = if res <= 18.5 then "ABAIXO" else if res >= 30 then "ACIMA" else "NORMAL"
  where res = peso / (altura^2)


cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
  let expr = (sum $ zipWith (*) digits mults) `mod` 11
  in if expr < 2 then 0 else 11-expr


cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]


andTable :: [(Bool, Bool, Bool)]
andTable = [(opt1,opt2,(opt1 && opt2)) | opt1 <- options, opt2 <- options]
  where options = [True, False]
