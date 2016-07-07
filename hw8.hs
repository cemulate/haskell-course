import Employee
import Party

main = do
    text <- readFile "res/company.txt"
    let company = read text
    let (GL employees fun) = maxFun company
    putStrLn $ "Total fun: " ++ (show fun)
    sequence $ map (\(Emp name _) -> putStrLn name) employees
