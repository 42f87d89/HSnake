import UI.NCurses
import System.Random

data Direction = L|D|U|R deriving (Eq)
data Position = Position (Integer, Integer) deriving (Eq)
data Food = Food Position
data Snake = Snake Direction [Position]
data State = State ([Food], Snake) -- add dimensions, make non tuple

--instance Show State where
--    show (State (f:fs,snake)) = "State (" ++ show f ++ ":fs, " ++ show snake

instance Random Position where
    random g =
        let (x,g2) = random g
            (y,g3) = random g2
        in  (Position (y,x),g3)
    randomR (Position (t,l),Position (b,r)) g =
        let (y,g2) = randomR (t,b) g
            (x,g3) = randomR (l,r) g2
        in  (Position (y,x),g3)

instance Random Food where
    random g = (Food (Position (0,0)),g)
    randomR (Food p1,Food p2) g =
        let (p,g') = randomR (p1,p2) g in (Food p, g')
    randomRs (f1,f2) g =
        let (f,g') = randomR (f1,f2) g in f:randomRs (f1,f2) g'

move dir (Position (y,x)) dim = case dir of
    L -> Position ((y  , x-1) % dim)
    D -> Position ((y+1, x  ) % dim)
    U -> Position ((y-1, x  ) % dim)
    R -> Position ((y  , x+1) % dim)
    where (a,b) % (r,c) = ((a+r) `mod` r,(b+c) `mod` c) --mod can return negative

slither (Snake dir (p:ps)) dim = Snake dir ((move dir p dim):(init (p:ps)))

advance (State (foods@((Food foodPos):fs), snake@(Snake dir pos))) (r,c) =
    let nextSnake@(Snake _ (nextSnakeHead:ps)) = slither snake (r,c) in
        if nextSnakeHead == foodPos
        then State (fs, Snake dir (nextSnakeHead:pos))
        else State (foods, nextSnake)
            where unFood (Food x) = x

draw (State (f, Snake dir (Position (y,x):ps))) = do
    moveCursor y x
    drawString "@"
    draw (State (f, Snake dir ps))
draw (State (Food (Position (y,x)):fs, Snake _ [])) = do
    moveCursor y x
    drawString "#"
    moveCursor y x -- Remove this if you find how to hide the cursor

handle w e s@(State (f, Snake dir ps)) dim =
    case e of
        Just (EventCharacter 'h') ->
            advance (State (f, Snake (if dir /= R then L else dir) ps)) dim
        Just (EventCharacter 'j') ->
            advance (State (f, Snake (if dir /= U then D else dir) ps)) dim
        Just (EventCharacter 'k') ->
            advance (State (f, Snake (if dir /= D then U else dir) ps)) dim
        Just (EventCharacter 'l') ->
            advance (State (f, Snake (if dir /= L then R else dir) ps)) dim
        _ -> advance s dim

clearWindow w = do
    dim <- screenSize
    updateWindow w $ do
        moveCursor 0 0
        clearW dim
    where
        clearW (1,c) = drawString$replicate (fromInteger (c-1)) ' '
        clearW (r,c) = do
            drawString$replicate (fromInteger (c-1)) ' '
            moveCursor (r-1) 0
            clearW (r-1,c)

loop w ev state = do
    dim <- screenSize
    let state' = handle w ev state dim
    clearWindow w
    updateWindow w $ draw state
    render
    e <- getEvent w (Just 90)
    case e of
        Just (EventCharacter 'q') -> return ()
        _ -> loop w e state'

makeFoods (top,left) (bottom,right) g =
    randomRs (food (top,left), food (bottom,right)) g
        where food a = Food (Position a)

makeSnake dir length (headY,headX) =
    case dir of
        U -> Snake dir (map (\x->Position (x,headX)) [headY..(headY+length)])
        D -> Snake dir (map (\x->Position (x,headX)) [headY..(headY-length)])
        L -> Snake dir (map (\x->Position (x,headY)) [headX..(headX+length)])
        R -> Snake dir (map (\x->Position (x,headY)) [headX..(headX-length)])

makeStt height width g =
    State (makeFoods (0,0) (height,width) g, makeSnake U 3 (10,10))

main = do
    g <- getStdGen --loop>handle>advance>eat
    runCurses $ do
        setEcho False
        w <- defaultWindow
        (height,width) <- screenSize
        loop w Nothing (makeStt (height-1) (width-1) g)
        closeWindow w

