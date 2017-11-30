module Text.ConfigParser.Util where

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b,c)
(&&&) f g a = (f a, g a)
