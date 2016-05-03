module Debian.Repo.Prelude.Time
    ( formatDebianDate
    , myTimeDiffToString
    ) where

import Control.Exception
import Data.List
import Data.Time
import System.Locale
import System.Time
import Text.Printf

{- This function is so complicated because there seems to be no way
   to get the Data.Time to format seconds without the fractional part,
   which seems not to be allowed in the RFC822 format.
   The mysterious 'take 2' pulls in just the integral part.

   Note the getCurrentTime :: IO UTCTime, so this will always be in UTC time
   with the resulting string ending in "UTC".
 -}


formatDebianDate :: FormatTime t => t -> [Char]
formatDebianDate t =
    prefix ++ seconds ++ suffix
        where prefix = formatTime defaultTimeLocale prefixFormat t
              seconds = take 2 $ formatTime defaultTimeLocale secondsFormat t
              suffix = formatTime defaultTimeLocale suffixFormat t
              prefixFormat = "%a, %d %b %Y %H:%M:"
              secondsFormat = "%S"
              suffixFormat = " %Z"
              format = "%a, %d %b %Y %H:%M:%S %Z"
              _test = assert (format == prefixFormat ++ secondsFormat ++ suffixFormat)

_test :: IO Bool
_test =
    do tz <- getCurrentTimeZone
       let ut = localTimeToUTC tz testtime
       return $ teststring == formatDebianDate ut
    where testtime = LocalTime {localDay=fromGregorian testyear testmonth testday,
                                localTimeOfDay=TimeOfDay{todHour=testhour,todMin=testminute,todSec=testsecond}}
          testyear = 2006
          testmonth = 12
          testday = 19
          testhour = 12
          testminute = 19
          testsecond = 15.29
          teststring = "Tue, 19 Dec 2006 17:19:15 UTC"

myTimeDiffToString :: TimeDiff -> String
myTimeDiffToString diff =
    do
      case () of
        _ | isPrefixOf "00:00:0" s -> drop 7 s ++ printf ".%03d" ms ++ " s."
        _ | isPrefixOf "00:00:" s -> drop 6 s ++ printf ".%03d" ms ++ " s."
        _ | isPrefixOf "00:" s -> drop 3 s
        _ -> s
    where
      s = formatTimeDiff defaultTimeLocale "%T" diff
      ms = ps2ms
      ps2ms = quot (ps + 500000000) 1000000000
      ps = tdPicosec diff
