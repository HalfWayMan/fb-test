{-# LANGUAGE ForeignFunctionInterface, CPP, UnicodeSyntax #-}
module System.Framebuffer ( BitField (..)
                          , FixScreenInfo (..)
                          , VarScreenInfo (..)
                          , withFramebuffer
                          ) where

import Control.Applicative
import Data.Word
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Internals (withFilePath)
import System.Posix.Types

#include <linux/fb.h>

data BitField = BitField { bitFieldOffset   ∷ Word32
                         , bitFieldLength   ∷ Word32
                         , bitFieldMSBRight ∷ Word32
                         }

instance Storable BitField where
  sizeOf _ = (#size struct fb_bitfield)
  alignment _ = alignment (undefined ∷ Word32)
  peek ptr = do
    BitField <$> ((#peek struct fb_bitfield, offset   ) ptr)
             <*> ((#peek struct fb_bitfield, length   ) ptr)
             <*> ((#peek struct fb_bitfield, msb_right) ptr)
  poke ptr (BitField off len msb) = do
    (#poke struct fb_bitfield, offset   ) ptr off
    (#poke struct fb_bitfield, length   ) ptr len
    (#poke struct fb_bitfield, msb_right) ptr msb

data FixScreenInfo = FixScreenInfo {
  fixIdent        ∷ String,
  fixMemStart     ∷ Word32,
  fixMemLength    ∷ Word32,
  fixType         ∷ Word32,
  fixTypeAux      ∷ Word32,
  fixVisual       ∷ Word32,
  fixXPanStep     ∷ Word16,
  fixYPanStep     ∷ Word16,
  fixYWrapStep    ∷ Word16,
  fixLineLength   ∷ Word32,
  fixMmioStart    ∷ Word32,
  fixMmioLength   ∷ Word32,
  fixAccel        ∷ Word32,
  fixCapabilities ∷ Word16,
  fixReserved     ∷ (Word16, Word16)
  }

instance Storable FixScreenInfo where
  sizeOf _ = (#size struct fb_fix_screeninfo)
  alignment _ = alignment (undefined ∷ Word32)
  peek ptr = do
    FixScreenInfo <$> peekString16 ptr
                  <*> ((#peek struct fb_fix_screeninfo, smem_start  ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, smem_len    ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, type        ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, type_aux    ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, visual      ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, xpanstep    ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, ypanstep    ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, ywrapstep   ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, line_length ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, mmio_start  ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, mmio_len    ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, accel       ) ptr)
                  <*> ((#peek struct fb_fix_screeninfo, capabilities) ptr)
                  <*> (pure (0x00, 0x00))
    where
      peekString16 ∷ Ptr FixScreenInfo → IO String
      peekString16 = peekCAString . castPtr
      
  poke ptr fixInfo = do
    pokeString16 ptr (fixIdent fixInfo)
    (#poke struct fb_fix_screeninfo, smem_start  ) ptr (fixMemStart     fixInfo)
    (#poke struct fb_fix_screeninfo, smem_len    ) ptr (fixMemLength    fixInfo)
    (#poke struct fb_fix_screeninfo, type        ) ptr (fixType         fixInfo)
    (#poke struct fb_fix_screeninfo, type_aux    ) ptr (fixTypeAux      fixInfo)
    (#poke struct fb_fix_screeninfo, visual      ) ptr (fixVisual       fixInfo)
    (#poke struct fb_fix_screeninfo, xpanstep    ) ptr (fixXPanStep     fixInfo)
    (#poke struct fb_fix_screeninfo, ypanstep    ) ptr (fixYPanStep     fixInfo)
    (#poke struct fb_fix_screeninfo, ywrapstep   ) ptr (fixYWrapStep    fixInfo)
    (#poke struct fb_fix_screeninfo, line_length ) ptr (fixLineLength   fixInfo)
    (#poke struct fb_fix_screeninfo, mmio_start  ) ptr (fixMmioStart    fixInfo)
    (#poke struct fb_fix_screeninfo, mmio_len    ) ptr (fixMmioLength   fixInfo)
    (#poke struct fb_fix_screeninfo, accel       ) ptr (fixAccel        fixInfo)
    (#poke struct fb_fix_screeninfo, capabilities) ptr (fixCapabilities fixInfo)
    where
      pokeString16 ∷ Ptr FixScreenInfo → String → IO ()
      pokeString16 ptr str = do
        withCStringLen str $ \(strPtr, len) →
          copyArray (castPtr ptr) strPtr (max len 15)
        pokeByteOff ptr 16 (0 ∷ Word8)

data VarScreenInfo = VarScreenInfo {
  varResolutionX            ∷ Word32,
  varResolutionY            ∷ Word32,
  varVirtualResolutionX     ∷ Word32,
  varVirtualResolutionY     ∷ Word32,
  varOffsetX                ∷ Word32,
  varOffsetY                ∷ Word32,
  varBitsPerPixel           ∷ Word32,
  varGrayscale              ∷ Word32,
  varRed                    ∷ BitField,
  varGreen                  ∷ BitField,
  varBlue                   ∷ BitField,
  varTransparent            ∷ BitField,
  varNonStandardPixelFormat ∷ Word32,
  varActivation             ∷ Word32,
  varPictureHeight          ∷ Word32,
  varPictureWidth           ∷ Word32,
  varAccelerationFlags      ∷ Word32,
  varPixClock               ∷ Word32,
  varLeftMargin             ∷ Word32,
  varRightMargin            ∷ Word32,
  varUpperMargin            ∷ Word32,
  varLowerMargin            ∷ Word32,
  varHsyncLength            ∷ Word32,
  varVsyncLength            ∷ Word32,
  varSync                   ∷ Word32,
  varVMode                  ∷ Word32,
  varRotate                 ∷ Word32,
  varColorspace             ∷ Word32,
  varReserved               ∷ (Word32, Word32, Word32, Word32)
  }

instance Storable VarScreenInfo where
  sizeOf _ = (#size struct fb_var_screeninfo)
  alignment _ = alignment (undefined ∷ Word32)
  peek ptr =
    VarScreenInfo <$> ((#peek struct fb_var_screeninfo, xres          ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, yres          ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, xres_virtual  ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, yres_virtual  ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, xoffset       ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, yoffset       ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, bits_per_pixel) ptr)
                  <*> ((#peek struct fb_var_screeninfo, grayscale     ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, red           ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, green         ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, blue          ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, transp        ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, nonstd        ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, activate      ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, height        ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, width         ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, accel_flags   ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, pixclock      ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, left_margin   ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, right_margin  ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, upper_margin  ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, lower_margin  ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, hsync_len     ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, vsync_len     ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, sync          ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, vmode         ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, rotate        ) ptr)
                  <*> ((#peek struct fb_var_screeninfo, colorspace    ) ptr)
                  <*> pure (0x00, 0x00, 0x00, 0x00)
  poke ptr varInfo = do
    (#poke struct fb_var_screeninfo, xres          ) ptr (varResolutionX            varInfo)
    (#poke struct fb_var_screeninfo, yres          ) ptr (varResolutionY            varInfo)
    (#poke struct fb_var_screeninfo, xres_virtual  ) ptr (varVirtualResolutionX     varInfo)
    (#poke struct fb_var_screeninfo, yres_virtual  ) ptr (varVirtualResolutionY     varInfo)
    (#poke struct fb_var_screeninfo, xoffset       ) ptr (varOffsetX                varInfo)
    (#poke struct fb_var_screeninfo, yoffset       ) ptr (varOffsetY                varInfo)
    (#poke struct fb_var_screeninfo, bits_per_pixel) ptr (varBitsPerPixel           varInfo)
    (#poke struct fb_var_screeninfo, grayscale     ) ptr (varGrayscale              varInfo)
    (#poke struct fb_var_screeninfo, red           ) ptr (varRed                    varInfo)
    (#poke struct fb_var_screeninfo, green         ) ptr (varGreen                  varInfo)
    (#poke struct fb_var_screeninfo, blue          ) ptr (varBlue                   varInfo)
    (#poke struct fb_var_screeninfo, transp        ) ptr (varTransparent            varInfo)
    (#poke struct fb_var_screeninfo, nonstd        ) ptr (varNonStandardPixelFormat varInfo)
    (#poke struct fb_var_screeninfo, activate      ) ptr (varActivation             varInfo)
    (#poke struct fb_var_screeninfo, height        ) ptr (varPictureHeight          varInfo)
    (#poke struct fb_var_screeninfo, width         ) ptr (varPictureWidth           varInfo)
    (#poke struct fb_var_screeninfo, accel_flags   ) ptr (varAccelerationFlags      varInfo)
    (#poke struct fb_var_screeninfo, pixclock      ) ptr (varPixClock               varInfo)
    (#poke struct fb_var_screeninfo, left_margin   ) ptr (varLeftMargin             varInfo)
    (#poke struct fb_var_screeninfo, right_margin  ) ptr (varRightMargin            varInfo)
    (#poke struct fb_var_screeninfo, upper_margin  ) ptr (varUpperMargin            varInfo)
    (#poke struct fb_var_screeninfo, lower_margin  ) ptr (varLowerMargin            varInfo)
    (#poke struct fb_var_screeninfo, hsync_len     ) ptr (varHsyncLength            varInfo)
    (#poke struct fb_var_screeninfo, vsync_len     ) ptr (varVsyncLength            varInfo)
    (#poke struct fb_var_screeninfo, sync          ) ptr (varSync                   varInfo)
    (#poke struct fb_var_screeninfo, vmode         ) ptr (varVMode                  varInfo)
    (#poke struct fb_var_screeninfo, rotate        ) ptr (varRotate                 varInfo)
    (#poke struct fb_var_screeninfo, colorspace    ) ptr (varColorspace             varInfo)

-------------------------------------------------------------------------------

foreign import ccall "open" open :: CString -> CInt -> IO CInt

c_open :: FilePath -> CInt -> IO Fd
c_open path mode =
  withFilePath path $ \pathPtr -> do
    fd <- open pathPtr mode
    return (Fd fd)


foreign import ccall "close" close :: CInt -> IO CInt

c_close :: Fd -> IO ()
c_close fd =
  throwErrnoIfMinus1_ "close" $
    close (fromIntegral fd)

foreign import ccall "ioctl" ioctl :: CInt -> CInt -> Ptr () -> IO CInt

c_ioctl :: Fd -> CInt -> Ptr d -> IO ()
c_ioctl f req p =
  throwErrnoIfMinus1_ "ioctl" $
    ioctl (fromIntegral f) req (castPtr p)

foreign import ccall "mmap" mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CInt -> IO (Ptr ())

c_mmap :: Ptr () -> CSize -> CInt -> CInt -> Fd -> CInt -> IO (Ptr Word8)
c_mmap addr len prot flags fd offset =  
  castPtr <$> mmap addr len prot flags (fromIntegral fd) offset


foreign import ccall "munmap" munmap :: Ptr () -> CSize -> IO CInt

c_unmap :: Ptr a -> CSize -> IO ()
c_unmap addr len =
  throwErrnoIfMinus1_ "munmap" $
    munmap (castPtr addr) len

-------------------------------------------------------------------------------

getFixScreenInfo ∷ Fd → IO FixScreenInfo
getFixScreenInfo fbfd =
  alloca $ \finfoP → do
    c_ioctl fbfd 0x4602 finfoP
    peek finfoP

getVarScreenInfo ∷ Fd → IO VarScreenInfo
getVarScreenInfo fbfd =
  alloca $ \vinfoP → do
    c_ioctl fbfd 0x4600 vinfoP
    peek vinfoP
    
-------------------------------------------------------------------------------

withFramebuffer ∷ (VarScreenInfo → FixScreenInfo → Ptr Word8 → IO a) → IO a
withFramebuffer action = do
  fbfd  ← c_open "/dev/fb0" 0x02
  vinfo ← getVarScreenInfo fbfd
  finfo ← getFixScreenInfo fbfd
  fbptr ← c_mmap nullPtr (fromIntegral $ fixMemLength finfo) 0x03 0x01 fbfd 0
  res   ← action vinfo finfo fbptr
  c_unmap fbptr (fromIntegral $ fixMemLength finfo)
  c_close fbfd
  return res
