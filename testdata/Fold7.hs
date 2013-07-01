module Main where

-- | Get the contents of a package index
binaryPackagesOfIndex repo release index =
    liftIO $ getPackages repo release index -- >>= return . either Left (Right . List.map (toBinaryPackage index . packageInfo))

a=1  -- This is a comment too
