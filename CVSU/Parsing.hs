module CVSU.Parsing
( quadForestParse
) where


quadForestParse :: Int -> Double -> Int -> QuadForest -> IO QuadForest
quadForestParse rounds bias minLength forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_parse pforest
        (fromIntegral rounds)
        (realToFrac bias)
        (fromIntegral minLength)
    if r /= c'SUCCESS
      then error $ "quadForestParse failed with " ++ (show r)
      else quadForestRefresh forest
