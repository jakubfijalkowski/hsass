module Text.Sass.Values
  (
    SassMapEntry
  , SassValue (..)
  , toNativeValue
  , fromNativeValue
  , deleteNativeValue
  , makeValueForeignPtr
  , Lib.SassSeparator (..)
  ) where

import qualified Binding.Libsass             as Lib
import           Control.Applicative         ((<$>))
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Data.Foldable               (toList)
import           Data.Sequence               (singleton)
import           Foreign
import           Foreign.C

-- | Entry of 'SassMap'.
type SassMapEntry = (SassValue, SassValue)

-- | Represents value used by libsass.
data SassValue = SassBool Bool -- ^ Boolean value.
               | SassNumber Double String -- ^ Number (value and unit)
               | SassColor { -- ^ RGBA color.
                    sassColorR :: Double,
                    sassColorG :: Double,
                    sassColorB :: Double,
                    sassColorA :: Double
                 }
                | SassString String -- ^ String
                | SassList [SassValue] Lib.SassSeparator -- ^ List of 'SassValue's.
                | SassMap [SassMapEntry] -- ^ Map.
                | SassNull -- ^ Null value.
                | SassWarning String -- ^ Warning with message.
                | SassError String -- ^ Error with message.
                deriving (Eq)

-- | Converts a 'SassValue' to native type.
--
--   This is (mostly) internal function.
toNativeValue :: SassValue -> IO (Ptr Lib.SassValue)
toNativeValue (SassBool val) = Lib.sass_make_boolean val
toNativeValue (SassNumber val unit) = withCString unit $
    Lib.sass_make_number (realToFrac val)
toNativeValue (SassColor r g b a) = Lib.sass_make_color r' g' b' a'
    where r' = realToFrac r
          g' = realToFrac g
          b' = realToFrac b
          a' = realToFrac a
toNativeValue (SassString str) = withCString str Lib.sass_make_string
toNativeValue SassNull = Lib.sass_make_null
toNativeValue (SassWarning str) = withCString str Lib.sass_make_warning
toNativeValue (SassError str) = withCString str Lib.sass_make_error
toNativeValue (SassList lst sep') = do
    let len = fromIntegral $ length lst
    let sep = fromIntegral $ fromEnum sep'
    result <- Lib.sass_make_list len sep
    flip runStateT 0 $ forM_ lst $ \e -> do
        idx <- get
        modify' (+1)
        native <- liftIO $ toNativeValue e
        liftIO $ Lib.sass_list_set_value result idx native
    return result
toNativeValue (SassMap lst) = do
    let len = fromIntegral $ length lst
    result <- Lib.sass_make_map len
    flip runStateT 0 $ forM_ lst $ \(key, val) -> do
        idx <- get
        modify' (+1)
        nativeKey <- liftIO $ toNativeValue key
        nativeVal <- liftIO $ toNativeValue val
        liftIO $ Lib.sass_map_set_key result idx nativeKey
        liftIO $ Lib.sass_map_set_value result idx nativeVal
    return result

-- | Converts native value to 'SassValue'.
--
--   This is (mostly) internal function.
fromNativeValue :: Ptr Lib.SassValue -> IO SassValue
fromNativeValue ptr = do
    tag <- Lib.sass_value_get_tag ptr
    fromNativeValue' (toEnum $ fromIntegral tag) ptr

-- | Real code for 'fromNativeValue'.
fromNativeValue' :: Lib.SassTag -> Ptr Lib.SassValue -> IO SassValue
fromNativeValue' Lib.SassBoolean ptr =
    SassBool <$> Lib.sass_boolean_get_value ptr
fromNativeValue' Lib.SassNumber ptr = do
    val <- Lib.sass_number_get_value ptr
    unit <- Lib.sass_number_get_unit ptr >>= peekCString
    return $ SassNumber (realToFrac val) unit
fromNativeValue' Lib.SassColor ptr = do
    r <- realToFrac <$> Lib.sass_color_get_r ptr
    g <- realToFrac <$> Lib.sass_color_get_g ptr
    b <- realToFrac <$> Lib.sass_color_get_b ptr
    a <- realToFrac <$> Lib.sass_color_get_a ptr
    return $ SassColor r g b a
fromNativeValue' Lib.SassString ptr =
    SassString <$> (Lib.sass_string_get_value ptr >>= peekCString)
fromNativeValue' Lib.SassNull _ = return SassNull
fromNativeValue' Lib.SassWarning ptr = do
    SassWarning <$> (Lib.sass_warning_get_message ptr >>= peekCString)
fromNativeValue' Lib.SassError ptr = do
    SassError <$> (Lib.sass_error_get_message ptr >>= peekCString)
fromNativeValue' Lib.SassList ptr = do
    len <- Lib.sass_list_get_length ptr
    sep <- fromIntegral <$> Lib.sass_list_get_separator ptr
    result <- execWriterT $ forM_ [0 .. len - 1] $ \idx -> do
        val <- liftIO $ Lib.sass_list_get_value ptr idx >>= fromNativeValue
        tell $ singleton val
    return $ SassList (toList result) (toEnum sep)
fromNativeValue' Lib.SassMap ptr = do
    len <- Lib.sass_map_get_length ptr
    result <- execWriterT $ forM_ [0 .. len - 1] $ \idx -> do
        key <- liftIO $ Lib.sass_map_get_key ptr idx >>= fromNativeValue
        val <- liftIO $ Lib.sass_map_get_value ptr idx >>= fromNativeValue
        tell $ singleton (key, val)
    return $ SassMap (toList result)

-- | Frees native representation of 'SassValue'.
--
--   This is (mostly) internal function.
deleteNativeValue :: Ptr Lib.SassValue -> IO ()
deleteNativeValue = Lib.sass_delete_value

-- | Makes 'ForeignPtr' from 'Ptr' to native representation of 'SassValue'.
--
--   This is (mostly) internal function.
makeValueForeignPtr :: Ptr Lib.SassValue -> IO (ForeignPtr Lib.SassValue)
makeValueForeignPtr = newForeignPtr Lib.p_sass_delete_value
