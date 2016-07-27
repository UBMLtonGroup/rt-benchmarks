--INCOMPLETE

import Data.List
import System.IO
import Data.Bits
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock

data Node = Empty | Node {leftNode :: Node, rightNode :: Node}

emptyNode () = Node {leftNode = Empty, rightNode = Empty}

nonEmptyNode (left,right) = Node {leftNode = left, rightNode = right}

kStretchTreeDepth = 18
kLongLivedTreeDepth = 16
kArraySize = 500000
kMinTreeDepth = 4
kMaxTreeDepth = 16

-- Nodes used by a tree of a given size
treeSize :: Int -> Int
treeSize i = ((1 `shiftL` (i + 1)) - 1)

-- Number of iterations to use for a given tree depth
numIters :: Int -> Int
numIters i = 2 * treeSize (kStretchTreeDepth) `div` treeSize(i)

printDiagnostics () = return ()

populate (iDepth, thisNode) =
	if iDepth <= 0 then return ()
	else case thisNode of
		(Node {leftNode = thisNodeLeft, rightNode = thisNodeRight}) -> 
					do
				let thisNodeLeft = emptyNode()
				let thisNodeRight = emptyNode()
				populate(iDepth-1, thisNodeLeft)
				populate(iDepth-1, thisNodeRight)
		(Empty) -> return ()

makeTree iDepth = 
	if iDepth <= 0 then emptyNode()
		else nonEmptyNode(makeTree(iDepth-1), makeTree(iDepth-1))

timeConstruction depth = do
				let iNumIters = numIters(depth)
				print ("Creating " ++ show iNumIters ++ " trees of depth " ++ show depth)
				tStart <- getCurrentTime
				let topDownLoop i = if i >= iNumIters then return ()
								else do
										populate (depth, emptyNode())
										topDownLoop (i + 1)
				topDownLoop 0				
				tFinish <- getCurrentTime
				print(diffUTCTime tFinish tStart)
				--print("Top down construction took " ++ show (diffUTCTime tFinish tStart) ++ "msecs")
				tStart <- getCurrentTime
				let bottomUpLoop a = if a >= iNumIters then return ()
								else do
									let nm = makeTree(depth)
									bottomUpLoop (a + 1)

				bottomUpLoop 0
				tFinish <- getCurrentTime
				print(diffUTCTime tFinish tStart)
				--print("Bottom up construction took " ++ show (diffUTCTime tFinish tStart) ++ "msecs")

main = do
	print("Garbage Collector Test")
	print("Stretching memory with a binary tree of depth " ++ show kStretchTreeDepth)
	printDiagnostics()
	tStart <- getCurrentTime
				
	-- Stretch the memory space quickly
	let tempTree = makeTree(kStretchTreeDepth)
	let tempTree = 0
	
	-- Create a long lived object
	print(" Creating a long-lived binary tree of depth " ++ show kLongLivedTreeDepth)
	let longLivedTree = emptyNode()
	populate (kLongLivedTreeDepth, longLivedTree)
	
	-- Create long-lived array, filling half of it
	print(" Creating a long-lived array of " ++ show kArraySize ++ " doubles")
	let fillArrauLoop i =
		do
			let listToReturn = []
			if i == kArraySize - 1 
				then return listToReturn
				else if i >= kArraySize/2
					then do
						0.0 : listToReturn
						fillArrauLoop(i+1)
					else do
						1.0/i : listToReturn
						fillArrauLoop(i+1)

	let array = fillArrauLoop 0
	printDiagnostics()

	let timeConstructionLop d = if d > kMaxTreeDepth then return ()
				else do
						timeConstruction(d)
						timeConstructionLop(d+2)
	timeConstructionLop kMinTreeDepth

	tFinish <- getCurrentTime
	printDiagnostics()
	print(diffUTCTime tFinish tStart)
			--print("Completed in " ++ show (diffUTCTime tFinish tStart ) ++ "ms.")

