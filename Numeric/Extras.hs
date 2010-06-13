{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts, TypeFamilies #-}
module Numeric.Extras
    ( RealExtras(..)
    ) where

import Foreign
import Foreign.C.Types

class (Storable (C a), RealFloat (C a), RealFloat a) => RealExtras a where
    type C a :: *
    fmod :: a -> a -> a
    expm1 :: a -> a
    log1p :: a -> a
    hypot :: a -> a -> a
    cbrt  :: a -> a
    erf   :: a -> a

instance RealExtras Double where
    type C Double = CDouble
    fmod = lift2D c_fmod
    expm1 = lift1D c_expm1
    log1p = lift1D c_log1p
    hypot = lift2D c_hypot
    cbrt = lift1D c_cbrt
    erf = lift1D c_erf

lift1D :: (CDouble -> CDouble) -> Double -> Double
lift1D f a = realToFrac (f (realToFrac a))
{-# INLINE lift1D #-}

lift2D :: (CDouble -> CDouble -> CDouble) -> Double -> Double -> Double
lift2D f a b = realToFrac (f (realToFrac a) (realToFrac b))
{-# INLINE lift2D #-}

instance RealExtras Float where
    type C Float = CFloat
    fmod = lift2F c_fmodf
    expm1 = lift1F c_expm1f
    log1p = lift1F c_log1pf
    hypot = lift2F c_hypotf
    cbrt  = lift1F c_cbrtf
    erf   = lift1F c_erff

lift1F :: (CFloat -> CFloat) -> Float -> Float
lift1F f a = realToFrac (f (realToFrac a))
{-# INLINE lift1F #-}

lift2F :: (CFloat -> CFloat -> CFloat) -> Float -> Float -> Float
lift2F f a b = realToFrac (f (realToFrac a) (realToFrac b))
{-# INLINE lift2F #-}

foreign import ccall unsafe "math.h fmod" 
    c_fmod :: CDouble -> CDouble -> CDouble
foreign import ccall unsafe "math.h expm1" 
    c_expm1 :: CDouble -> CDouble
foreign import ccall unsafe "math.h log1p" 
    c_log1p :: CDouble -> CDouble
foreign import ccall unsafe "math.h hypot" 
    c_hypot :: CDouble -> CDouble -> CDouble
foreign import ccall unsafe "math.h cbrt" 
    c_cbrt :: CDouble -> CDouble
foreign import ccall unsafe "math.h erf"
    c_erf :: CDouble -> CDouble

foreign import ccall unsafe "math.h fmodf" 
    c_fmodf :: CFloat -> CFloat -> CFloat
foreign import ccall unsafe "math.h expm1f" 
    c_expm1f :: CFloat -> CFloat
foreign import ccall unsafe "math.h log1pf" 
    c_log1pf :: CFloat -> CFloat
foreign import ccall unsafe "math.h hypotf" 
    c_hypotf :: CFloat -> CFloat -> CFloat
foreign import ccall unsafe "math.h cbrtf" 
    c_cbrtf :: CFloat -> CFloat
foreign import ccall unsafe "math.h erff"
    c_erff :: CFloat -> CFloat
