import Data.List 
import Data.Function

type Rank = Int
parseRank :: Char -> Rank
parseRank 'T' = 10
parseRank 'J' = 11
parseRank 'Q' = 12
parseRank 'K' = 13
parseRank 'A' = 14
parseRank n   = read [n]

type Suit = Char

type Card = (Rank, Suit)
parseCard :: String -> Card
parseCard [r, s] = (parseRank r, s)

type Hand = [Card]
sortHand :: Hand -> Hand
-- Same as:
--  sortHand hand = sortBy (compare `on` fst) hand
sortHand = sortBy (compare `on` fst)

isFlush :: Hand -> Bool
-- Same as:
--  isFlush h = ((==1) . length . nub . map snd) h
isFlush = (==1) . length . nub . map snd

isStraight :: Hand -> Bool
isStraight h = values == [v..v+4]
  where values = sort $ map fst h 
        v      = head values

rankGroups :: Hand -> [(Int, Rank)]
-- Same as:
--  rankGroups h = (map (\g -> (length g, head g)) . group . sort . map fst) h
rankGroups = map (\g -> (length g, head g)) . group . sort . map fst

getHandRank :: Hand -> (Int, [Rank])
-- For some reason I wanted to make it fancy 
--  but it only got unreadable, but that's fine.
--  I got this from internet, I'm horrible at Poker.
getHandRank    hand 
  | isFlush    hand       && isStraight hand && elem 14 values = (10, values) -- Royal Flush
  | isFlush    hand       && isStraight hand                   = ( 9, values) -- Straight Flush
  | sort       l == [2,3]                                      = ( 7, values) -- Full House
  | length     l == 1     && head       l == 4                 = ( 8, values) -- Four of a Kind
  | isFlush    hand                                            = ( 6, values) -- Flush
  | isStraight hand                                            = ( 5, values) -- Straight
  | length     l == 1 && head l == 3                           = ( 4, values) -- Three of a Kind
  | length     l == 2 && sort l == [2,2]                       = ( 3, values) -- Two Pairs
  | length     l == 3 && elem 2 l                              = ( 2, values) -- One Pair
  | otherwise                                                  = ( 1, values) -- High Card
  where 
      values = reverse . sort $ map snd $ rankGroups hand
      l      = map fst $ rankGroups hand

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2 = compare (getHandRank h1) (getHandRank h2)

main :: IO ()
main = do
  input <- readFile "poker.txt"
  let linesOfHands = map words (lines input)
  let hands = [(map parseCard (take 5 line), map parseCard (drop 5 line)) | line <- linesOfHands]
  print $ length hands
  print $ length $ filter (== LT) $ map (uncurry compareHands) hands
  putStrLn "Hello, World!"
