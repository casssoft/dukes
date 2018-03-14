module Items where


data Item = Cookie deriving (Show, Eq)

type Inventory = [Item]

addItem :: Inventory -> Item -> Inventory
--addItem inv item = inv ++ item
addItem inv x = x:inv

countItem :: Inventory -> Item -> Integer
countItem (x:xs) item
    | x == item = 1 + (countItem xs item)
    | otherwise = countItem xs item
countItem [] item = 0

showItems :: Inventory -> [String]
showItems (x:xs) = (show x):(showItems xs)
showItems [] = ["--END--"]
