module Parameters where


tileSize :: Int
tileSize = 48


levelSize :: Int
levelSize = 11


drawableSize :: (Int, Int)
drawableSize = (tileSize*levelSize + 160, tileSize*levelSize)
