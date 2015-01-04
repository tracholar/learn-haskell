
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "compared gang size to 9.")


applyLog ::(a, String) -> (a->(b,String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
