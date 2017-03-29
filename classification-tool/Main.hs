{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, MultiParamTypeClasses #-}

import Happstack.Lite (ServerPart, Browsing(EnableBrowsing), Response, serve, path, toResponse, ok, internalServerError, dir, serveDirectory, seeOther, method, Method(POST), lookText)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5.Attributes as A
import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Database.HDBC (fromSql, toSql, prepare, execute, fetchRowMap, SqlValue(SqlString), quickQuery, commit)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Control.Monad (msum)
import Data.Monoid (mempty)
import Data.Maybe (fromJust)
import Data.Convertible (Convertible, safeConvert, ConvertResult)

-- NOTE: Keep in sync with the definition of the BuildingType SQL type in
-- ../Makefile.
data BuildingType
    = Unknown
    | Sunroom
    | BaySunroom
    | Sunporch
    deriving (Read, Show)

instance Convertible BuildingType SqlValue where
    safeConvert buildingType = return $ case buildingType of
        Unknown -> SqlString "UNKNOWN"
        Sunroom -> SqlString "SUNROOM"
        BaySunroom -> SqlString "BAY_SUNROOM"
        Sunporch -> SqlString "SUNPORCH"

main :: IO ()
main = do
    -- Feasible because the list of all pins can easily fit into memory.
    conn <- db
    pins <- map fromSql . concat <$> quickQuery conn (concat
        [ "SELECT pin FROM classification_tool WHERE property_class = 211 "
        , "AND stories > 2 AND exterior_construction != 'Frame' "
        , "AND building_type IS NULL "
        , "ORDER BY RANDOM()"
        ]) []

    serve Nothing $ msum
        [ dir "js" $ javascript
        , dir "property" $ property (makePaginationInfo pins)
        , dir "report" $ method POST >> reportSunroom
        , homePage (head pins)
        ]

db :: IO Connection
db = connectPostgreSQL "dbname = edifice"

javascript :: ServerPart Response
javascript = serveDirectory EnableBrowsing [] "js"

homePage :: Integer -> ServerPart Response
homePage firstProperty = 
    seeOther
        (("/property/" :: String) ++ show firstProperty)
        (toResponse ("Redirecting to the first property matching the query." :: String))

reportSunroom :: ServerPart Response
reportSunroom = do
    buildingType <- read . LazyText.unpack <$> lookText "building_type"
    currentPin <- read . LazyText.unpack <$> lookText "pin"

    conn <- lift db
    statement <- lift $ prepare conn "UPDATE classification_tool SET building_type = ?::BuildingType WHERE pin = ?"
    rowsModified <- lift $ execute statement [toSql (buildingType :: BuildingType), toSql (currentPin :: Integer)] 

    lift $ commit conn

    if rowsModified == 1
    then toResponse <$> ok ("Successfully reported one building" :: String)
    else do
        lift $ putStrLn "Database failure"
        toResponse <$> internalServerError ("Database failure" :: String)

property :: Map Integer (Maybe Integer, Maybe Integer) -> ServerPart Response
property paginationInfo = path $ \(pin :: Integer) -> do
    conn <- lift db

    statement <- lift $ prepare conn (unlines [
        "SELECT *,",
        "  encode(ST_AsPng(ST_AsRaster(footprint, 0.3, 0.3, '8BUI')), 'base64') AS footprint ",
        "FROM classification_tool WHERE pin = ?"])
    status <- lift $ execute statement [toSql pin]

    result <- lift $ fetchRowMap statement
    case result of
        Nothing -> internalServerError $ toResponse $ H.toHtml ("Could not retrieve row from DB" :: String)
        Just row -> ok $ toResponse $
                H.html $ do
                    H.head $ do
                        H.title (H.toHtml ("Chicago Sunroom Apartments Project" :: String))
                        H.script $ do
                            case snd . fromJust $ Map.lookup pin paginationInfo of
                                Nothing -> mempty
                                Just nextId -> H.toHtml $ ("var nextBuilding = " ++ show nextId ++ ";")
                            case fst . fromJust $ Map.lookup pin paginationInfo of
                                Nothing -> mempty
                                Just prevId -> H.toHtml $ ("var prevBuilding = " ++ show prevId ++ ";")
                        H.script mempty ! (A.src . H.textValue . Text.pack $ "/js/main.js")

                    H.body $ do
                        H.table $ do
                            H.tr $ renderField row "building_use"
                            H.tr $ renderField row "property_class"
                            H.tr $ renderField row "property_class_description"
                            H.tr $ renderField row "tax_code"
                            H.tr $ renderField row "exterior_construction"
                            H.tr $ renderField row "stories"

                            H.tr $ do
                                H.td $ H.toHtml ("footprint" :: String)
                                H.td $ do
                                    case ((fromSql <$> Map.lookup "footprint" row) :: Maybe String) of
                                        Nothing -> mempty
                                        Just footprint -> do
                                            H.toHtml $
                                                H.img ! (A.src . H.textValue . Text.pack $
                                                    "data:image/png;base64," ++ footprint)

                            H.tr $ do
                                H.td $ H.toHtml ("photo" :: String)
                                H.td $ do
                                    case ((fromSql <$> Map.lookup "pin" row) :: Maybe String) of
                                        Nothing -> do
                                            H.toHtml ("Error" :: String)
                                        Just pin -> do
                                            H.toHtml $
                                                H.img ! (A.src . H.textValue . Text.pack $
                                                    "http://cookcountyassessor.com/PropertyImage.aspx?pin=" ++ pin)
                                                        
renderField :: Map String SqlValue -> String -> Html
renderField row columnName = do
    H.td $ H.toHtml columnName
    H.td $ case ((fromSql <$> Map.lookup columnName row) :: Maybe String) of
        Nothing -> do
            H.toHtml ("Error" :: String)
        Just val -> do
            H.toHtml val

-- Returns a Map which associates each property PIN with a 'next' and a 'previous' PIN.
makePaginationInfo :: Ord a => [a] -> Map a (Maybe a, Maybe a)
makePaginationInfo ids = 
        Map.fromList $ zip ids (zip (init $ Nothing : justIds) (tail $ justIds ++ [Nothing])) 
    where 
        justIds = map Just ids
