{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Gun(
    Gun(),
    new
    ) where

	import Bullet

	data Gun = Gun { inactiveBullets :: [Bullet], activeBullets :: [Bullet] }

	maxBullets :: Int
	maxBullets = 50

	new :: Gun
	new = Gun { inactiveBullets = replicate maxBullets Bullet.new, activeBullets = [] }