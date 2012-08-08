-- -*- mode: Haskell; fill-column; 79; default-justification: full; -*-
{-# LANGUAGE UnicodeSyntax #-}

module Main ( main
            ) where


import           Control.Monad                     (forM_, void, when)
import           Data.Word                         (Word8)
import           Foreign.Ptr                       (Ptr, plusPtr)
import           Foreign.Marshal.Array             (copyArray)
import qualified Graphics.Rendering.Cairo          as Cairo
import qualified Graphics.Rendering.Cairo.Internal as CairoInternal
import           System.Environment                (getArgs)
import           System.Framebuffer

-------------------------------------------------------------------------------

main ∷ IO ()
main = do
  args ← getArgs
  case args of
    [path] → withFramebuffer $ \vinfo finfo fbp → do
      Cairo.withImageSurface Cairo.FormatARGB32                         
                             (fromIntegral $ fixLineLength  finfo)
                             (fromIntegral $ varResolutionY vinfo) $ \surface → do
        Cairo.withImageSurfaceFromPNG path $ \imgSurface → do
          imgW ← Cairo.imageSurfaceGetWidth imgSurface
          imgH ← Cairo.imageSurfaceGetHeight imgSurface
          
          let x = if imgW < (fromIntegral $ varResolutionX vinfo)
                     then ((fromIntegral $ varResolutionX vinfo) - fromIntegral imgW) / 2.0
                     else 0.0
              y = if imgH < (fromIntegral $ varResolutionY vinfo)
                     then ((fromIntegral $ varResolutionY vinfo) - fromIntegral imgH) / 2.0
                     else 0.0
      
          Cairo.renderWith surface $ do
            Cairo.setSourceRGBA 1.0 1.0 1.0 1.0
            Cairo.setSourceSurface imgSurface x y
            Cairo.rectangle x y (fromIntegral $ imgW) (fromIntegral $ imgH)
            Cairo.fill
        
            when (imgW < (fromIntegral $ varResolutionX vinfo) &&
                  imgH < (fromIntegral $ varResolutionY vinfo)) $ do
              Cairo.selectFontFace "sans-serif" Cairo.FontSlantNormal Cairo.FontWeightNormal
              Cairo.setSourceRGBA 0.8 0.8 0.8 1.0
              extents ← Cairo.textExtents path
              Cairo.moveTo (((fromIntegral $ varResolutionX vinfo) - Cairo.textExtentsWidth extents) / 2.0)
                           (y + fromIntegral imgH + Cairo.textExtentsHeight extents)
              Cairo.showText path
         
        stride ← Cairo.imageSurfaceGetStride surface
        sPtr   ← CairoInternal.imageSurfaceGetData surface

        forM_ [0 .. varResolutionY vinfo - 1] $ \row →
          let dst = fbp `plusPtr` fromIntegral ((row + varOffsetY vinfo) * fixLineLength finfo)
              src = sPtr `plusPtr` (stride * fromIntegral row)
          in  copyArray (dst ∷ Ptr Word8) (src ∷ Ptr Word8) (fromIntegral (varResolutionX vinfo * 4))

        void $ getChar

    _ → putStrLn "usage: fb-test <png-file>"
