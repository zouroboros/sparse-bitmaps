module ImageConv where

import Data.Word
import Codec.Picture.Types

import qualified SparseMatrix as S

-- | converts a color image into a grayscale image
grayScale :: DynamicImage -> Image (PixelBaseComponent Pixel8)
grayScale (ImageRGB8 img) = extractLumaPlane img

-- | converts a gray scale image into a sparse matrix
sparseMatrix :: Image (PixelBaseComponent Pixel8) -> Word8
  -> S.SparseMatrix Word8
sparseMatrix img@(Image columns rows _) threshold =
  S.generateMatrix (pixelAt img) (>= threshold) rows columns
