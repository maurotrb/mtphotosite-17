#!/usr/bin/env stack
{- stack
  script
  --resolver lts-14.20
  --package bytestring
  --package containers
  --package cassava
  --package text
  --package vector
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           GHC.Generics

import qualified Data.ByteString.Lazy as BL
import           Data.Csv as CSV
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

{- Digikam Metadata -}

data ImageDigikamMetadata = IDM
  { image            :: T.Text
  , capture_time     :: T.Text
  , gps_latitude     :: T.Text
  , gps_longitude    :: T.Text
  , title            :: T.Text
  , caption          :: T.Text
  , export_tags      :: T.Text
  , export_galleries :: T.Text
  } deriving (Generic, Show)


{- ImagePixelSize -}

data ImagePixelSize = IPS
  { image_th         :: T.Text
  , image_width      :: T.Text
  , image_height     :: T.Text
  } deriving (Generic, Show)

data ImagePixelSizeS = IPSS
  { image_s          :: T.Text
  , image_width_s    :: T.Text
  , image_height_s   :: T.Text
  } deriving (Show)

data ImagePixelSizeXL = IPSXL
  { image_xl         :: T.Text
  , image_width_xl   :: T.Text
  , image_height_xl  :: T.Text
  } deriving (Show)

data ImagePixelSizeMerged = IPSM
  { image_s_M         :: T.Text
  , image_width_s_M   :: T.Text
  , image_height_s_M  :: T.Text
  , image_xl_M        :: T.Text
  , image_width_xl_M  :: T.Text
  , image_height_xl_M :: T.Text
  } deriving (Show)


{- FrontMatter -}

data FrontMatter = FM
  { title_FM           :: T.Text
  , description_FM     :: T.Text
  , date_FM            :: T.Text
  , image_FM           :: T.Text
  , image_s_FM         :: T.Text
  , image_width_s_FM   :: T.Text
  , image_height_s_FM  :: T.Text
  , image_xl_FM        :: T.Text
  , image_width_xl_FM  :: T.Text
  , image_height_xl_FM :: T.Text
  , gps_latitude_FM    :: T.Text
  , gps_longitude_FM   :: T.Text
  , galleries_FM       :: [T.Text]
  , phototags_FM       :: [T.Text]
  } deriving (Show)

fmToWrite :: FrontMatter -> T.Text
fmToWrite (FM t d dt im is iws ihs ixl iwxl ihxl gla glo gs ts) = T.concat [ "+++\n"
                                                                           , "title = \"", t, "\"\n"
                                                                           , "description = \"", sanitize d, "\"\n"
                                                                           , "date = \"", dt, "\"\n"
                                                                           , "image = \"", im, "\"\n"
                                                                           , "image_s = \"", is, "\"\n"
                                                                           , "image_width_s = \"", iws, "\"\n"
                                                                           , "image_height_s = \"", ihs, "\"\n"
                                                                           , "image_xl = \"", ixl, "\"\n"
                                                                           , "image_width_xl = \"", iwxl, "\"\n"
                                                                           , "image_height_xl = \"", ihxl, "\"\n"
                                                                           , "gps_latitude = \"", gla, "\"\n"
                                                                           , "gps_longitude = \"", glo, "\"\n"
                                                                           , "phototags = [ \"", T.intercalate "\", \"" ts, "\" ]\n"
                                                                           , "galleries = [ \"", T.intercalate "\", \"" gs, "\" ]\n"
                                                                           , "+++\n"
                                                                           ]
  where
    sanitize = T.replace "\"" "'"

{- Merge -}

type IdmMap   = Map.Map T.Text ImageDigikamMetadata
type IpssMap  = Map.Map T.Text ImagePixelSizeS
type IpsxlMap = Map.Map T.Text ImagePixelSizeXL
type IpsmMap  = Map.Map T.Text ImagePixelSizeMerged
type FmMap    = Map.Map T.Text FrontMatter

idmToMap :: IdmMap -> ImageDigikamMetadata -> IdmMap
idmToMap m idm = Map.insert (image idm) idm m

ipsToIpssMap :: IpssMap -> ImagePixelSize -> IpssMap
ipsToIpssMap m (IPS it iw ih) = case ts of "s" -> Map.insert im (IPSS it iw ih) m
                                           _   -> m
  where
    (i, ts) = T.breakOnEnd "-" it
    im      = T.dropEnd 1 i

ipsToIpsxlMap :: IpsxlMap -> ImagePixelSize -> IpsxlMap
ipsToIpsxlMap m (IPS it iw ih) = case ts of "xl" -> Map.insert im (IPSXL it iw ih) m
                                            _    -> m
  where
    (i, ts) = T.breakOnEnd "-" it
    im      = T.dropEnd 1 i

mergeToIpsmMap :: IpssMap -> IpsxlMap -> IpsmMap
mergeToIpsmMap = Map.intersectionWithKey wmatch
  where
    wmatch i (IPSS is iws ihs) (IPSXL ixl iwxl ihxl) = IPSM is iws ihs ixl iwxl ihxl

mergeToFmMap :: IdmMap -> IpsmMap -> FmMap
mergeToFmMap = Map.intersectionWithKey wmatch
  where
    wmatch  i (IDM im ct glat glon t cap tags gals) (IPSM is iws ihs ixl iwxl ihxl) = FM t cap ct im is iws ihs ixl iwxl ihxl glat glon (T.splitOn "|" gals) (T.splitOn "|" tags)

toFM :: V.Vector ImageDigikamMetadata -> V.Vector ImagePixelSize -> V.Vector FrontMatter
toFM idmd ips = Map.foldl' V.snoc V.empty
                $ mergeToFmMap (V.foldl' idmToMap Map.empty idmd)
                $ mergeToIpsmMap (V.foldl' ipsToIpssMap Map.empty ips) (V.foldl' ipsToIpsxlMap Map.empty ips)

{- CSV -}
instance FromNamedRecord ImageDigikamMetadata
instance ToNamedRecord ImageDigikamMetadata

instance FromNamedRecord ImagePixelSize
instance ToNamedRecord ImagePixelSize

type CsvImageDigikamMetadata = (Header, V.Vector ImageDigikamMetadata)
type CsvImagePixelSize       = (Header, V.Vector ImagePixelSize)
type ErrorMsg = String

csvPath                     = "./src/"
csvImageDigikamMetadataFile = "images-digikam-metadata.csv"
csvImagePixelSizeFile       = "images-pixel-size.csv"
photoPagesPath              = "./content/photos/"

parseImageDigikamMetadataCSV :: FilePath -> IO (Either ErrorMsg CsvImageDigikamMetadata)
parseImageDigikamMetadataCSV fname = do
  contents <- BL.readFile fname
  return $ CSV.decodeByName contents

parseImagePixelSizeCSV :: FilePath -> IO (Either ErrorMsg CsvImagePixelSize)
parseImagePixelSizeCSV fname = do
  contents <- BL.readFile fname
  return $ CSV.decodeByName contents

writeFrontMatter:: FrontMatter -> IO ()
writeFrontMatter fm = TIO.writeFile (photoPagesPath ++ T.unpack (image_FM fm) ++ ".md") $ fmToWrite fm

main :: IO()
main = do
  csvIDM <- parseImageDigikamMetadataCSV (csvPath ++ csvImageDigikamMetadataFile)
  csvIPS <- parseImagePixelSizeCSV (csvPath ++ csvImagePixelSizeFile)
  either print (V.mapM_ writeFrontMatter) $ toFM <$> (snd <$> csvIDM) <*> (snd <$> csvIPS)
