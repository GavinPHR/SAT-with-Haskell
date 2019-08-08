module China where
import Data.List (tails, sort)
import DPLL

solve :: Valuation (Province, Colour)
solve = sort [n | n <- dpll encode [], isPos n]
         where
           isPos (P _) = True
           isPos (N _) = False

encode :: CNF (Province, Colour)
encode = noSameNeighbour ++ noMoreThanOne ++ atLeastOne

atLeastOne :: CNF (Province, Colour)
atLeastOne = [[P (p, c) | c <- colours] | p <- provinces]

noMoreThanOne :: CNF (Province, Colour)
noMoreThanOne = [[neg x, neg y] | row <- atLeastOne, (x:xs) <- tails row, y <- xs]

noSameNeighbour :: CNF (Province, Colour)
noSameNeighbour = [[N (p,c), N (ngh,c)] | (p,nghs) <- neighbours, ngh <- nghs, c <- colours]

data Colour = Red | Green | Blue | Yellow deriving (Eq, Ord, Show)

colours :: [Colour]
colours = [Red, Green, Blue, Yellow]

data Province = Anhui        | Beijing  | Chongqing | Fujian  | Gansu    |
                Guangdong    | Guangxi  | Guizhou   | Hainan  | Hebei    | 
                Heilongjiang | Henan    | Hongkong  | Hubei   | Hunan    | 
                IMongolia    | Jiangsu  | Jiangxi   | Jilin   | Liaoning |
                Macau        | Ningxia  | Qinghai   | Shaanxi | Shandong | 
                Shanghai     | Shanxi   | Sichuan   | Taiwan  | Tianjin  |
                Tibet        | Xinjiang | Yunnan    | Zhejiang deriving (Eq, Ord, Show)

provinces :: [Province]
provinces = [Anhui, Beijing, Chongqing, Fujian, Gansu,
             Guangdong, Guangxi, Guizhou, Hainan, Hebei, 
             Heilongjiang, Henan, Hongkong, Hubei, Hunan, 
             IMongolia, Jiangsu, Jiangxi, Jilin, Liaoning,
             Macau, Ningxia, Qinghai, Shaanxi, Shandong, 
             Shanghai, Shanxi, Sichuan, Taiwan, Tianjin, 
             Tibet, Xinjiang, Yunnan, Zhejiang]

neighbours :: [(Province,[Province])]
neighbours = [(Anhui,[Shandong,Jiangsu,Zhejiang,Jiangxi,Hubei,Henan]),
              (Beijing,[Hebei,Tianjin]),
              (Chongqing,[Hubei,Hunan,Guizhou,Sichuan,Shaanxi]),
              (Fujian,[Guangdong,Jiangxi,Zhejiang]),
              (Gansu,[IMongolia,Ningxia,Shaanxi,Sichuan,Qinghai,Xinjiang]),
              (Guangdong,[Guangxi,Hunan,Jiangxi,Fujian,Hongkong,Macau]),
              (Guangxi,[Yunnan,Guizhou,Hunan,Guangdong]),
              (Guizhou,[Chongqing,Hunan,Guangxi,Yunnan,Sichuan]),
              (Hainan,[]),
              (Hebei,[IMongolia,Liaoning,Beijing,Tianjin,Shandong,Henan,Shanxi]),
              (Heilongjiang,[IMongolia,Jilin]),
              (Henan,[Hebei,Shandong,Anhui,Hubei,Shaanxi,Shanxi]),
              (Hongkong,[Guangdong]),
              (Hubei,[Henan,Anhui,Jiangxi,Hunan,Chongqing,Shaanxi]),
              (Hunan,[Hubei,Jiangxi,Guangdong,Guangxi,Guizhou,Chongqing]),  
              (IMongolia,[Heilongjiang,Jilin,Liaoning,Hebei,Shanxi,Shaanxi,Ningxia,Gansu]),
              (Jiangsu,[Shandong,Anhui,Shanghai,Zhejiang]),
              (Jiangxi,[Anhui,Zhejiang,Fujian,Guangdong,Hunan,Hubei]),
              (Jilin,[Heilongjiang,IMongolia,Liaoning]),
              (Liaoning,[Jilin,IMongolia,Hebei]),
              (Macau,[Guangdong]),
              (Ningxia,[IMongolia,Shaanxi,Gansu]),
              (Qinghai,[Xinjiang,Gansu,Sichuan,Tibet]),
              (Shaanxi,[Ningxia,IMongolia,Shanxi,Henan,Hubei,Chongqing,Sichuan,Gansu]),
              (Shandong,[Hebei,Henan,Anhui,Jiangsu]), 
              (Shanghai,[Jiangsu,Zhejiang]),
              (Shanxi,[IMongolia,Hebei,Henan,Shaanxi]),
              (Sichuan,[Gansu,Shaanxi,Chongqing,Guizhou,Yunnan,Tibet,Qinghai]),
              (Taiwan,[]),
              (Tianjin,[Beijing,Hebei]),
              (Tibet,[Xinjiang,Qinghai,Sichuan,Yunnan]),
              (Xinjiang,[Tibet,Qinghai,Gansu]),
              (Yunnan,[Tibet,Sichuan,Guizhou,Guangxi]),
              (Zhejiang,[Shanghai,Jiangsu,Anhui,Jiangxi,Fujian])]


