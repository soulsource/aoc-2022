module Main (main) where

import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay6

solveDay6 :: String -> String
solveDay6 = formatResults . (=<<) solveDay6Actual . parseDay6Input

newtype RadioSignal = RadioSignal String

-- Rather trivial: Any string of at least 14 characters is valid input (for part 2).
parseDay6Input :: String -> Either String RadioSignal
parseDay6Input (a:b:c:d:e:f:g:h:i:j:k:l:m:n:es) = Right $ RadioSignal (a:b:c:d:e:f:g:h:i:j:k:l:m:n:es)
parseDay6Input _ = Left "Insufficient input. Need at least 14 characters."

-- Without knowing the part 2 prompt, I started out with a hardcoded buffer size. Left in for reference. Part 2 below is generic in buffer size.
data RingBufferFour a = RingBufferFour {
    buffer :: (a,a,a,a),
    position :: Int
}

makeRingBufferFour :: (a,a,a,a) -> RingBufferFour a
makeRingBufferFour b = RingBufferFour { buffer = b, position = 0}

pushRingBufferFour :: RingBufferFour a -> a -> RingBufferFour a
pushRingBufferFour RingBufferFour { buffer = (a,b,c,d), position = 0 } e = RingBufferFour { buffer = (e,b,c,d), position = 1}
pushRingBufferFour RingBufferFour { buffer = (a,b,c,d), position = 1 } e = RingBufferFour { buffer = (a,e,c,d), position = 2}
pushRingBufferFour RingBufferFour { buffer = (a,b,c,d), position = 2 } e = RingBufferFour { buffer = (a,b,e,d), position = 3}
pushRingBufferFour RingBufferFour { buffer = (a,b,c,d), position = 3 } e = RingBufferFour { buffer = (a,b,c,e), position = 0}
pushRingBufferFour _ _ = error "Index out of bounds in RingBufferFour. Should be impossible."

solveDay6Actual :: RadioSignal -> Either String (Int, Int)
solveDay6Actual s = solveDay6Part1 s >>= (\firstResult -> fmap (\x -> (firstResult,x)) (solveDay6Part2 s))

data StartOfPacketSearch = StartOfPacketSearch {
    ringBuffer :: RingBufferFour Char,
    index :: Int,
    remainingData :: String
}

solveDay6Part1 :: RadioSignal -> Either String Int
solveDay6Part1 = findStartOfPacketMarker . initializeStartOfPacketSearch

initializeStartOfPacketSearch :: RadioSignal -> StartOfPacketSearch -- this cannot fail
initializeStartOfPacketSearch (RadioSignal (a:b:c:d:es)) = StartOfPacketSearch { ringBuffer = makeRingBufferFour (a,b,c,d), index = 4, remainingData = es }

findStartOfPacketMarker :: StartOfPacketSearch -> Either String Int
findStartOfPacketMarker s | isFinished s = Right (index s)
    where isFinished StartOfPacketSearch { ringBuffer = RingBufferFour {buffer = (a,b,c,d)} } = a /= b && a /= c && a /= d && b /= c && b /= d && c /= d
findStartOfPacketMarker StartOfPacketSearch { remainingData = [] } = Left "Search for Start-of-Packet Marker did not complete."
findStartOfPacketMarker StartOfPacketSearch { ringBuffer = r, index = i, remainingData = (c:cs)} = 
    findStartOfPacketMarker StartOfPacketSearch { ringBuffer = pushRingBufferFour r c, index = i+1, remainingData = cs }

newtype RingBufferN a = RingBufferN [a]

makeRingBufferN :: [a] -> RingBufferN a
makeRingBufferN = RingBufferN

pushRingBufferN :: RingBufferN a -> a -> RingBufferN a
pushRingBufferN (RingBufferN (a:as)) = RingBufferN . (++) as . return

data StartOfMessageSearch = StartOfMessageSearch {
    mRingBuffer :: RingBufferN Char,
    mIndex :: Int,
    mRemainingData :: String
}

solveDay6Part2 :: RadioSignal -> Either String Int
solveDay6Part2 = findStartOfMessageMarker . initializeStartOfMessageSearch

initializeStartOfMessageSearch :: RadioSignal -> StartOfMessageSearch -- can't fail either. RadioSignal already checked for validity.
initializeStartOfMessageSearch (RadioSignal s) = StartOfMessageSearch { mRingBuffer = makeRingBufferN $ take 14 s, mIndex = 14, mRemainingData = drop 14 s }

findStartOfMessageMarker :: StartOfMessageSearch -> Either String Int
findStartOfMessageMarker s | isFinished s = Right (mIndex s)
    where isFinished StartOfMessageSearch { mRingBuffer = RingBufferN as } = allUnique as
          allUnique (a:as) = (a `notElem` as) && allUnique as
          allUnique [] = True
findStartOfMessageMarker StartOfMessageSearch { mRemainingData = [] } = Left "Search for Start-of-Message Marker did not complete."
findStartOfMessageMarker StartOfMessageSearch { mRingBuffer = r, mIndex = i, mRemainingData = (c:cs)} = 
    findStartOfMessageMarker StartOfMessageSearch { mRingBuffer = pushRingBufferN r c, mIndex = i+1, mRemainingData = cs }

formatResults :: Either String (Int, Int) -> String
formatResults (Left x) = x
formatResults (Right (a,b)) = "Part 1: " ++ show a ++ ", Part 2: " ++ show b 