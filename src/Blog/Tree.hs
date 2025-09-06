{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Blog.Tree where

import Control.Lens

data Tree a = Tree {_treeVal :: a, _treeKids :: [Tree a]}

data Tooth a = Tooth {_toothVal :: a, _toothKidsLeft :: [Tree a], _toothKidsRight :: [Tree a]}

type Path a = [Tooth a]

data Cursor a = Cursor {_cursorPath :: Path a, _cursorTree :: Tree a}

makeLenses ''Tree
makeLenses ''Tooth
makeLenses ''Cursor

moveUpCursor :: Cursor a -> Cursor a
moveUpCursor c = case c ^. cursorPath of
  [] -> c
  th : path ->
    c
      & (cursorPath .~ path)
      & (cursorTree .~ unTooth th (c ^. cursorTree))

unTooth :: Tooth a -> Tree a -> Tree a
unTooth th t = Tree (th ^. toothVal) (concat [th ^. toothKidsLeft, [t], th ^. toothKidsRight])

unPath :: Path a -> Tree a -> Tree a
unPath = flip (foldl (flip unTooth))

unCursor :: Cursor a -> Tree a
unCursor c = unPath (c ^. cursorPath) (c ^. cursorTree)
