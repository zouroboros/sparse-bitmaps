module ImageConv where

import Data.Word
import Codec.Picture.Types

import qualified SparseMatrix as S

-- | converts a color image into a grayscale image
grayScale :: DynamicImage -> Image (PixelBaseComponent Pixel8)
grayScale (ImageRGB8 img) = extractLumaPlane img
grayScale (ImageRGB16 img) = pixelMap (\x -> floor (fromIntegral x / (2.0^16) * 255.0)) (extractLumaPlane img)
grayScale (ImageY8 img) = img
grayScale (ImageY16 img) = pixelMap (\x -> floor (fromIntegral x / (2.0^16) * 255.0)) (extractLumaPlane img)
--grayScale (ImageY32 img) = extractLumaPlane (promoteImage img)


-- | converts a gray scale image into a sparse matrix
sparseMatrix :: Image (PixelBaseComponent Pixel8) -> Word8
  -> S.SparseMatrix Word8
sparseMatrix img@(Image columns rows _) threshold =
  S.generateMatrix (\x y -> pixelAt img (x - 1) (y - 1)) (> threshold) rows columns
