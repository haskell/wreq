{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Network.Wreq.Lens.TH
    (
      Types.Options
    , manager
    , proxy
    , auth
    , header
    , headers
    , param
    , params
    , redirects
    , cookie
    , cookies
    , checkResponse

    , HTTP.Cookie
    , cookieName
    , cookieValue
    , cookieExpiryTime
    , cookieDomain
    , cookiePath
    , cookieCreationTime
    , cookieLastAccessTime
    , cookiePersistent
    , cookieHostOnly
    , cookieSecureOnly
    , cookieHttpOnly

    , HTTP.Proxy
    , proxyHost
    , proxyPort

    , HTTP.Response
    , responseStatus
    , responseVersion
    , responseHeader
    , responseHeaders
    , responseLink
    , responseBody
    , responseCookie
    , responseCookieJar
    , responseClose'

    , HTTP.HistoriedResponse
    , hrFinalResponse
    , hrFinalRequest
    , hrRedirects

    , HTTP.Status
    , statusCode
    , statusMessage

    , Types.Link
    , linkURL
    , linkParams

    , Form.PartM
    , partName
    , partFilename
    , partContentType
    , partGetBody
    , partHeaders
    ) where

import Control.Lens hiding (makeLenses)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.Wreq.Internal.Lens (assoc, assoc2)
import Network.Wreq.Internal.Link
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.MultipartFormData as Form
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wreq.Types as Types

import qualified Network.HTTP.Client.MultipartFormData
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.Types
import qualified Control.Lens.Type
import qualified Network.Wreq.Internal.Types
import qualified GHC.Maybe
import qualified GHC.Base
import qualified Network.HTTP.Types.Header
import qualified Data.Text.Internal
import qualified GHC.Types
import qualified Data.ByteString.Internal
import qualified Network.HTTP.Types.Status
import qualified Network.HTTP.Types.Version
import qualified Data.ByteString.Lazy.Internal
import qualified Network.Mime

-- these internal modules are not exposed, but we don't need them to be
import qualified Data.Time as Data.Time.Clock.Internal.UTCTime

-- Network/Wreq/Lens/TH.hs:81:1-26: Splicing declarations
auth ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Options (GHC.Maybe.Maybe Network.Wreq.Internal.Types.Auth)
auth
  f_aDCR
  (Network.Wreq.Internal.Types.Options x1_aDCS x2_aDCT x3_aDCU
                                       x4_aDCV x5_aDCW x6_aDCX x7_aDCY x8_aDCZ)
  = (GHC.Base.fmap
       (\ y1_aDD0
          -> (((((((Network.Wreq.Internal.Types.Options x1_aDCS) x2_aDCT)
                    y1_aDD0)
                   x4_aDCV)
                  x5_aDCW)
                 x6_aDCX)
                x7_aDCY)
               x8_aDCZ))
      (f_aDCR x3_aDCU)
{-# INLINE auth #-}
checkResponse ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Options (GHC.Maybe.Maybe Network.Wreq.Internal.Types.ResponseChecker)
checkResponse
  f_aDD1
  (Network.Wreq.Internal.Types.Options x1_aDD2 x2_aDD3 x3_aDD4
                                       x4_aDD5 x5_aDD6 x6_aDD7 x7_aDD8 x8_aDD9)
  = (GHC.Base.fmap
       (\ y1_aDDa
          -> (((((((Network.Wreq.Internal.Types.Options x1_aDD2) x2_aDD3)
                    x3_aDD4)
                   x4_aDD5)
                  x5_aDD6)
                 x6_aDD7)
                x7_aDD8)
               y1_aDDa))
      (f_aDD1 x8_aDD9)
{-# INLINE checkResponse #-}
cookies ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Options (GHC.Maybe.Maybe Network.HTTP.Client.Types.CookieJar)
cookies
  f_aDDb
  (Network.Wreq.Internal.Types.Options x1_aDDc x2_aDDd x3_aDDe
                                       x4_aDDf x5_aDDg x6_aDDh x7_aDDi x8_aDDj)
  = (GHC.Base.fmap
       (\ y1_aDDk
          -> (((((((Network.Wreq.Internal.Types.Options x1_aDDc) x2_aDDd)
                    x3_aDDe)
                   x4_aDDf)
                  x5_aDDg)
                 x6_aDDh)
                y1_aDDk)
               x8_aDDj))
      (f_aDDb x7_aDDi)
{-# INLINE cookies #-}
headers ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Options [Network.HTTP.Types.Header.Header]
headers
  f_aDDl
  (Network.Wreq.Internal.Types.Options x1_aDDm x2_aDDn x3_aDDo
                                       x4_aDDp x5_aDDq x6_aDDr x7_aDDs x8_aDDt)
  = (GHC.Base.fmap
       (\ y1_aDDu
          -> (((((((Network.Wreq.Internal.Types.Options x1_aDDm) x2_aDDn)
                    x3_aDDo)
                   y1_aDDu)
                  x5_aDDq)
                 x6_aDDr)
                x7_aDDs)
               x8_aDDt))
      (f_aDDl x4_aDDp)
{-# INLINE headers #-}
manager ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Options Network.Wreq.Internal.Types.Mgr
manager
  f_aDDv
  (Network.Wreq.Internal.Types.Options x1_aDDw x2_aDDx x3_aDDy
                                       x4_aDDz x5_aDDA x6_aDDB x7_aDDC x8_aDDD)
  = (GHC.Base.fmap
       (\ y1_aDDE
          -> (((((((Network.Wreq.Internal.Types.Options y1_aDDE) x2_aDDx)
                    x3_aDDy)
                   x4_aDDz)
                  x5_aDDA)
                 x6_aDDB)
                x7_aDDC)
               x8_aDDD))
      (f_aDDv x1_aDDw)
{-# INLINE manager #-}
params ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Options [(Data.Text.Internal.Text,
                                                                Data.Text.Internal.Text)]
params
  f_aDDF
  (Network.Wreq.Internal.Types.Options x1_aDDG x2_aDDH x3_aDDI
                                       x4_aDDJ x5_aDDK x6_aDDL x7_aDDM x8_aDDN)
  = (GHC.Base.fmap
       (\ y1_aDDO
          -> (((((((Network.Wreq.Internal.Types.Options x1_aDDG) x2_aDDH)
                    x3_aDDI)
                   x4_aDDJ)
                  y1_aDDO)
                 x6_aDDL)
                x7_aDDM)
               x8_aDDN))
      (f_aDDF x5_aDDK)
{-# INLINE params #-}
proxy ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Options (GHC.Maybe.Maybe Network.HTTP.Client.Types.Proxy)
proxy
  f_aDDP
  (Network.Wreq.Internal.Types.Options x1_aDDQ x2_aDDR x3_aDDS
                                       x4_aDDT x5_aDDU x6_aDDV x7_aDDW x8_aDDX)
  = (GHC.Base.fmap
       (\ y1_aDDY
          -> (((((((Network.Wreq.Internal.Types.Options x1_aDDQ) y1_aDDY)
                    x3_aDDS)
                   x4_aDDT)
                  x5_aDDU)
                 x6_aDDV)
                x7_aDDW)
               x8_aDDX))
      (f_aDDP x2_aDDR)
{-# INLINE proxy #-}
redirects ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Options GHC.Types.Int
redirects
  f_aDDZ
  (Network.Wreq.Internal.Types.Options x1_aDE0 x2_aDE1 x3_aDE2
                                       x4_aDE3 x5_aDE4 x6_aDE5 x7_aDE6 x8_aDE7)
  = (GHC.Base.fmap
       (\ y1_aDE8
          -> (((((((Network.Wreq.Internal.Types.Options x1_aDE0) x2_aDE1)
                    x3_aDE2)
                   x4_aDE3)
                  x5_aDE4)
                 y1_aDE8)
                x7_aDE6)
               x8_aDE7))
      (f_aDDZ x6_aDE5)
{-# INLINE redirects #-}
-- Network/Wreq/Lens/TH.hs:82:1-77: Splicing declarations
cookieCreationTime ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie Data.Time.Clock.Internal.UTCTime.UTCTime
cookieCreationTime
  f_aDJm
  (Network.HTTP.Client.Types.Cookie x1_aDJn x2_aDJo x3_aDJp x4_aDJq
                                    x5_aDJr x6_aDJs x7_aDJt x8_aDJu x9_aDJv x10_aDJw x11_aDJx)
  = (GHC.Base.fmap
       (\ y1_aDJy
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDJn) x2_aDJo)
                       x3_aDJp)
                      x4_aDJq)
                     x5_aDJr)
                    y1_aDJy)
                   x7_aDJt)
                  x8_aDJu)
                 x9_aDJv)
                x10_aDJw)
               x11_aDJx))
      (f_aDJm x6_aDJs)
{-# INLINE cookieCreationTime #-}
cookieDomain ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie Data.ByteString.Internal.ByteString
cookieDomain
  f_aDJz
  (Network.HTTP.Client.Types.Cookie x1_aDJA x2_aDJB x3_aDJC x4_aDJD
                                    x5_aDJE x6_aDJF x7_aDJG x8_aDJH x9_aDJI x10_aDJJ x11_aDJK)
  = (GHC.Base.fmap
       (\ y1_aDJL
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDJA) x2_aDJB)
                       x3_aDJC)
                      y1_aDJL)
                     x5_aDJE)
                    x6_aDJF)
                   x7_aDJG)
                  x8_aDJH)
                 x9_aDJI)
                x10_aDJJ)
               x11_aDJK))
      (f_aDJz x4_aDJD)
{-# INLINE cookieDomain #-}
cookieExpiryTime ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie Data.Time.Clock.Internal.UTCTime.UTCTime
cookieExpiryTime
  f_aDJM
  (Network.HTTP.Client.Types.Cookie x1_aDJN x2_aDJO x3_aDJP x4_aDJQ
                                    x5_aDJR x6_aDJS x7_aDJT x8_aDJU x9_aDJV x10_aDJW x11_aDJX)
  = (GHC.Base.fmap
       (\ y1_aDJY
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDJN) x2_aDJO)
                       y1_aDJY)
                      x4_aDJQ)
                     x5_aDJR)
                    x6_aDJS)
                   x7_aDJT)
                  x8_aDJU)
                 x9_aDJV)
                x10_aDJW)
               x11_aDJX))
      (f_aDJM x3_aDJP)
{-# INLINE cookieExpiryTime #-}
cookieHostOnly ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie GHC.Types.Bool
cookieHostOnly
  f_aDJZ
  (Network.HTTP.Client.Types.Cookie x1_aDK0 x2_aDK1 x3_aDK2 x4_aDK3
                                    x5_aDK4 x6_aDK5 x7_aDK6 x8_aDK7 x9_aDK8 x10_aDK9 x11_aDKa)
  = (GHC.Base.fmap
       (\ y1_aDKb
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDK0) x2_aDK1)
                       x3_aDK2)
                      x4_aDK3)
                     x5_aDK4)
                    x6_aDK5)
                   x7_aDK6)
                  x8_aDK7)
                 y1_aDKb)
                x10_aDK9)
               x11_aDKa))
      (f_aDJZ x9_aDK8)
{-# INLINE cookieHostOnly #-}
cookieHttpOnly ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie GHC.Types.Bool
cookieHttpOnly
  f_aDKc
  (Network.HTTP.Client.Types.Cookie x1_aDKd x2_aDKe x3_aDKf x4_aDKg
                                    x5_aDKh x6_aDKi x7_aDKj x8_aDKk x9_aDKl x10_aDKm x11_aDKn)
  = (GHC.Base.fmap
       (\ y1_aDKo
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDKd) x2_aDKe)
                       x3_aDKf)
                      x4_aDKg)
                     x5_aDKh)
                    x6_aDKi)
                   x7_aDKj)
                  x8_aDKk)
                 x9_aDKl)
                x10_aDKm)
               y1_aDKo))
      (f_aDKc x11_aDKn)
{-# INLINE cookieHttpOnly #-}
cookieLastAccessTime ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie Data.Time.Clock.Internal.UTCTime.UTCTime
cookieLastAccessTime
  f_aDKp
  (Network.HTTP.Client.Types.Cookie x1_aDKq x2_aDKr x3_aDKs x4_aDKt
                                    x5_aDKu x6_aDKv x7_aDKw x8_aDKx x9_aDKy x10_aDKz x11_aDKA)
  = (GHC.Base.fmap
       (\ y1_aDKB
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDKq) x2_aDKr)
                       x3_aDKs)
                      x4_aDKt)
                     x5_aDKu)
                    x6_aDKv)
                   y1_aDKB)
                  x8_aDKx)
                 x9_aDKy)
                x10_aDKz)
               x11_aDKA))
      (f_aDKp x7_aDKw)
{-# INLINE cookieLastAccessTime #-}
cookieName ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie Data.ByteString.Internal.ByteString
cookieName
  f_aDKC
  (Network.HTTP.Client.Types.Cookie x1_aDKD x2_aDKE x3_aDKF x4_aDKG
                                    x5_aDKH x6_aDKI x7_aDKJ x8_aDKK x9_aDKL x10_aDKM x11_aDKN)
  = (GHC.Base.fmap
       (\ y1_aDKO
          -> ((((((((((Network.HTTP.Client.Types.Cookie y1_aDKO) x2_aDKE)
                       x3_aDKF)
                      x4_aDKG)
                     x5_aDKH)
                    x6_aDKI)
                   x7_aDKJ)
                  x8_aDKK)
                 x9_aDKL)
                x10_aDKM)
               x11_aDKN))
      (f_aDKC x1_aDKD)
{-# INLINE cookieName #-}
cookiePath ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie Data.ByteString.Internal.ByteString
cookiePath
  f_aDKP
  (Network.HTTP.Client.Types.Cookie x1_aDKQ x2_aDKR x3_aDKS x4_aDKT
                                    x5_aDKU x6_aDKV x7_aDKW x8_aDKX x9_aDKY x10_aDKZ x11_aDL0)
  = (GHC.Base.fmap
       (\ y1_aDL1
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDKQ) x2_aDKR)
                       x3_aDKS)
                      x4_aDKT)
                     y1_aDL1)
                    x6_aDKV)
                   x7_aDKW)
                  x8_aDKX)
                 x9_aDKY)
                x10_aDKZ)
               x11_aDL0))
      (f_aDKP x5_aDKU)
{-# INLINE cookiePath #-}
cookiePersistent ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie GHC.Types.Bool
cookiePersistent
  f_aDL2
  (Network.HTTP.Client.Types.Cookie x1_aDL3 x2_aDL4 x3_aDL5 x4_aDL6
                                    x5_aDL7 x6_aDL8 x7_aDL9 x8_aDLa x9_aDLb x10_aDLc x11_aDLd)
  = (GHC.Base.fmap
       (\ y1_aDLe
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDL3) x2_aDL4)
                       x3_aDL5)
                      x4_aDL6)
                     x5_aDL7)
                    x6_aDL8)
                   x7_aDL9)
                  y1_aDLe)
                 x9_aDLb)
                x10_aDLc)
               x11_aDLd))
      (f_aDL2 x8_aDLa)
{-# INLINE cookiePersistent #-}
cookieSecureOnly ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie GHC.Types.Bool
cookieSecureOnly
  f_aDLf
  (Network.HTTP.Client.Types.Cookie x1_aDLg x2_aDLh x3_aDLi x4_aDLj
                                    x5_aDLk x6_aDLl x7_aDLm x8_aDLn x9_aDLo x10_aDLp x11_aDLq)
  = (GHC.Base.fmap
       (\ y1_aDLr
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDLg) x2_aDLh)
                       x3_aDLi)
                      x4_aDLj)
                     x5_aDLk)
                    x6_aDLl)
                   x7_aDLm)
                  x8_aDLn)
                 x9_aDLo)
                y1_aDLr)
               x11_aDLq))
      (f_aDLf x10_aDLp)
{-# INLINE cookieSecureOnly #-}
cookieValue ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Cookie Data.ByteString.Internal.ByteString
cookieValue
  f_aDLs
  (Network.HTTP.Client.Types.Cookie x1_aDLt x2_aDLu x3_aDLv x4_aDLw
                                    x5_aDLx x6_aDLy x7_aDLz x8_aDLA x9_aDLB x10_aDLC x11_aDLD)
  = (GHC.Base.fmap
       (\ y1_aDLE
          -> ((((((((((Network.HTTP.Client.Types.Cookie x1_aDLt) y1_aDLE)
                       x3_aDLv)
                      x4_aDLw)
                     x5_aDLx)
                    x6_aDLy)
                   x7_aDLz)
                  x8_aDLA)
                 x9_aDLB)
                x10_aDLC)
               x11_aDLD))
      (f_aDLs x2_aDLu)
{-# INLINE cookieValue #-}
-- Network/Wreq/Lens/TH.hs:83:1-23: Splicing declarations
proxyHost ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Proxy Data.ByteString.Internal.ByteString
proxyHost f_aDRj (Network.HTTP.Client.Types.Proxy x1_aDRk x2_aDRl)
  = (GHC.Base.fmap
       (\ y1_aDRm -> (Network.HTTP.Client.Types.Proxy y1_aDRm) x2_aDRl))
      (f_aDRj x1_aDRk)
{-# INLINE proxyHost #-}
proxyPort ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Proxy GHC.Types.Int
proxyPort f_aDRn (Network.HTTP.Client.Types.Proxy x1_aDRo x2_aDRp)
  = (GHC.Base.fmap
       (\ y1_aDRq -> (Network.HTTP.Client.Types.Proxy x1_aDRo) y1_aDRq))
      (f_aDRn x2_aDRp)
{-# INLINE proxyPort #-}
-- Network/Wreq/Lens/TH.hs:84:1-26: Splicing declarations
responseBody ::
  forall body_a7b9
         body_aDSj. Control.Lens.Type.Lens (Network.HTTP.Client.Types.Response body_a7b9) (Network.HTTP.Client.Types.Response body_aDSj) body_a7b9 body_aDSj
responseBody
  f_aDSk
  (Network.HTTP.Client.Types.Response x1_aDSl x2_aDSm x3_aDSn x4_aDSo
                                      x5_aDSp x6_aDSq x7_aDSr)
  = (GHC.Base.fmap
       (\ y1_aDSs
          -> ((((((Network.HTTP.Client.Types.Response x1_aDSl) x2_aDSm)
                   x3_aDSn)
                  y1_aDSs)
                 x5_aDSp)
                x6_aDSq)
               x7_aDSr))
      (f_aDSk x4_aDSo)
{-# INLINE responseBody #-}
responseClose' ::
  forall body_a7b9. Control.Lens.Type.Lens' (Network.HTTP.Client.Types.Response body_a7b9) Network.HTTP.Client.Types.ResponseClose
responseClose'
  f_aDSt
  (Network.HTTP.Client.Types.Response x1_aDSu x2_aDSv x3_aDSw x4_aDSx
                                      x5_aDSy x6_aDSz x7_aDSA)
  = (GHC.Base.fmap
       (\ y1_aDSB
          -> ((((((Network.HTTP.Client.Types.Response x1_aDSu) x2_aDSv)
                   x3_aDSw)
                  x4_aDSx)
                 x5_aDSy)
                y1_aDSB)
               x7_aDSA))
      (f_aDSt x6_aDSz)
{-# INLINE responseClose' #-}
responseCookieJar ::
  forall body_a7b9. Control.Lens.Type.Lens' (Network.HTTP.Client.Types.Response body_a7b9) Network.HTTP.Client.Types.CookieJar
responseCookieJar
  f_aDSC
  (Network.HTTP.Client.Types.Response x1_aDSD x2_aDSE x3_aDSF x4_aDSG
                                      x5_aDSH x6_aDSI x7_aDSJ)
  = (GHC.Base.fmap
       (\ y1_aDSK
          -> ((((((Network.HTTP.Client.Types.Response x1_aDSD) x2_aDSE)
                   x3_aDSF)
                  x4_aDSG)
                 y1_aDSK)
                x6_aDSI)
               x7_aDSJ))
      (f_aDSC x5_aDSH)
{-# INLINE responseCookieJar #-}
responseHeaders ::
  forall body_a7b9. Control.Lens.Type.Lens' (Network.HTTP.Client.Types.Response body_a7b9) Network.HTTP.Types.Header.ResponseHeaders
responseHeaders
  f_aDSL
  (Network.HTTP.Client.Types.Response x1_aDSM x2_aDSN x3_aDSO x4_aDSP
                                      x5_aDSQ x6_aDSR x7_aDSS)
  = (GHC.Base.fmap
       (\ y1_aDST
          -> ((((((Network.HTTP.Client.Types.Response x1_aDSM) x2_aDSN)
                   y1_aDST)
                  x4_aDSP)
                 x5_aDSQ)
                x6_aDSR)
               x7_aDSS))
      (f_aDSL x3_aDSO)
{-# INLINE responseHeaders #-}
responseOriginalRequest ::
  forall body_a7b9. Control.Lens.Type.Lens' (Network.HTTP.Client.Types.Response body_a7b9) Network.HTTP.Client.Types.Request
responseOriginalRequest
  f_aDSU
  (Network.HTTP.Client.Types.Response x1_aDSV x2_aDSW x3_aDSX x4_aDSY
                                      x5_aDSZ x6_aDT0 x7_aDT1)
  = (GHC.Base.fmap
       (\ y1_aDT2
          -> ((((((Network.HTTP.Client.Types.Response x1_aDSV) x2_aDSW)
                   x3_aDSX)
                  x4_aDSY)
                 x5_aDSZ)
                x6_aDT0)
               y1_aDT2))
      (f_aDSU x7_aDT1)
{-# INLINE responseOriginalRequest #-}
responseStatus ::
  forall body_a7b9. Control.Lens.Type.Lens' (Network.HTTP.Client.Types.Response body_a7b9) Network.HTTP.Types.Status.Status
responseStatus
  f_aDT3
  (Network.HTTP.Client.Types.Response x1_aDT4 x2_aDT5 x3_aDT6 x4_aDT7
                                      x5_aDT8 x6_aDT9 x7_aDTa)
  = (GHC.Base.fmap
       (\ y1_aDTb
          -> ((((((Network.HTTP.Client.Types.Response y1_aDTb) x2_aDT5)
                   x3_aDT6)
                  x4_aDT7)
                 x5_aDT8)
                x6_aDT9)
               x7_aDTa))
      (f_aDT3 x1_aDT4)
{-# INLINE responseStatus #-}
responseVersion ::
  forall body_a7b9. Control.Lens.Type.Lens' (Network.HTTP.Client.Types.Response body_a7b9) Network.HTTP.Types.Version.HttpVersion
responseVersion
  f_aDTc
  (Network.HTTP.Client.Types.Response x1_aDTd x2_aDTe x3_aDTf x4_aDTg
                                      x5_aDTh x6_aDTi x7_aDTj)
  = (GHC.Base.fmap
       (\ y1_aDTk
          -> ((((((Network.HTTP.Client.Types.Response x1_aDTd) y1_aDTk)
                   x3_aDTf)
                  x4_aDTg)
                 x5_aDTh)
                x6_aDTi)
               x7_aDTj))
      (f_aDTc x2_aDTe)
{-# INLINE responseVersion #-}
-- Network/Wreq/Lens/TH.hs:85:1-35: Splicing declarations
hrFinalRequest ::
  forall body_a7gq. Control.Lens.Type.Lens' (Network.HTTP.Client.HistoriedResponse body_a7gq) Network.HTTP.Client.Types.Request
hrFinalRequest
  f_aDXj
  (Network.HTTP.Client.HistoriedResponse x1_aDXk x2_aDXl x3_aDXm)
  = (GHC.Base.fmap
       (\ y1_aDXn
          -> ((Network.HTTP.Client.HistoriedResponse x1_aDXk) y1_aDXn)
               x3_aDXm))
      (f_aDXj x2_aDXl)
{-# INLINE hrFinalRequest #-}
hrFinalResponse ::
  forall body_a7gq
         body_aDXi. Control.Lens.Type.Lens (Network.HTTP.Client.HistoriedResponse body_a7gq) (Network.HTTP.Client.HistoriedResponse body_aDXi) (Network.HTTP.Client.Types.Response body_a7gq) (Network.HTTP.Client.Types.Response body_aDXi)
hrFinalResponse
  f_aDXo
  (Network.HTTP.Client.HistoriedResponse x1_aDXp x2_aDXq x3_aDXr)
  = (GHC.Base.fmap
       (\ y1_aDXs
          -> ((Network.HTTP.Client.HistoriedResponse x1_aDXp) x2_aDXq)
               y1_aDXs))
      (f_aDXo x3_aDXr)
{-# INLINE hrFinalResponse #-}
hrRedirects ::
  forall body_a7gq. Control.Lens.Type.Lens' (Network.HTTP.Client.HistoriedResponse body_a7gq) [(Network.HTTP.Client.Types.Request,
                                                                                                Network.HTTP.Client.Types.Response Data.ByteString.Lazy.Internal.ByteString)]
hrRedirects
  f_aDXt
  (Network.HTTP.Client.HistoriedResponse x1_aDXu x2_aDXv x3_aDXw)
  = (GHC.Base.fmap
       (\ y1_aDXx
          -> ((Network.HTTP.Client.HistoriedResponse y1_aDXx) x2_aDXv)
               x3_aDXw))
      (f_aDXt x1_aDXu)
{-# INLINE hrRedirects #-}
-- Network/Wreq/Lens/TH.hs:86:1-24: Splicing declarations
statusCode ::
  Control.Lens.Type.Lens' Network.HTTP.Types.Status.Status GHC.Types.Int
statusCode
  f_aDZa
  (Network.HTTP.Types.Status.Status x1_aDZb x2_aDZc)
  = (GHC.Base.fmap
       (\ y1_aDZd -> (Network.HTTP.Types.Status.Status y1_aDZd) x2_aDZc))
      (f_aDZa x1_aDZb)
{-# INLINE statusCode #-}
statusMessage ::
  Control.Lens.Type.Lens' Network.HTTP.Types.Status.Status Data.ByteString.Internal.ByteString
statusMessage
  f_aDZe
  (Network.HTTP.Types.Status.Status x1_aDZf x2_aDZg)
  = (GHC.Base.fmap
       (\ y1_aDZh -> (Network.HTTP.Types.Status.Status x1_aDZf) y1_aDZh))
      (f_aDZe x2_aDZg)
{-# INLINE statusMessage #-}
-- Network/Wreq/Lens/TH.hs:87:1-23: Splicing declarations
linkParams ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Link [(Data.ByteString.Internal.ByteString,
                                                             Data.ByteString.Internal.ByteString)]
linkParams
  f_aE09
  (Network.Wreq.Internal.Types.Link x1_aE0a x2_aE0b)
  = (GHC.Base.fmap
       (\ y1_aE0c -> (Network.Wreq.Internal.Types.Link x1_aE0a) y1_aE0c))
      (f_aE09 x2_aE0b)
{-# INLINE linkParams #-}
linkURL ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Link Data.ByteString.Internal.ByteString
linkURL f_aE0d (Network.Wreq.Internal.Types.Link x1_aE0e x2_aE0f)
  = (GHC.Base.fmap
       (\ y1_aE0g -> (Network.Wreq.Internal.Types.Link y1_aE0g) x2_aE0f))
      (f_aE0d x1_aE0e)
{-# INLINE linkURL #-}
-- Network/Wreq/Lens/TH.hs:88:1-23: Splicing declarations
partContentType ::
  forall m_avOQ. Control.Lens.Type.Lens' (Network.HTTP.Client.MultipartFormData.PartM m_avOQ) (GHC.Maybe.Maybe Network.Mime.MimeType)
partContentType
  f_aFtV
  (Network.HTTP.Client.MultipartFormData.Part x1_aFtW x2_aFtX x3_aFtY
                                              x4_aFtZ x5_aFu0)
  = (GHC.Base.fmap
       (\ y1_aFu1
          -> ((((Network.HTTP.Client.MultipartFormData.Part x1_aFtW) x2_aFtX)
                 y1_aFu1)
                x4_aFtZ)
               x5_aFu0))
      (f_aFtV x3_aFtY)
{-# INLINE partContentType #-}
partFilename ::
  forall m_avOQ. Control.Lens.Type.Lens' (Network.HTTP.Client.MultipartFormData.PartM m_avOQ) (GHC.Maybe.Maybe GHC.Base.String)
partFilename
  f_aFu2
  (Network.HTTP.Client.MultipartFormData.Part x1_aFu3 x2_aFu4 x3_aFu5
                                              x4_aFu6 x5_aFu7)
  = (GHC.Base.fmap
       (\ y1_aFu8
          -> ((((Network.HTTP.Client.MultipartFormData.Part x1_aFu3) y1_aFu8)
                 x3_aFu5)
                x4_aFu6)
               x5_aFu7))
      (f_aFu2 x2_aFu4)
{-# INLINE partFilename #-}
partGetBody ::
  forall m_avOQ
         m_aFtU. Control.Lens.Type.Lens (Network.HTTP.Client.MultipartFormData.PartM m_avOQ) (Network.HTTP.Client.MultipartFormData.PartM m_aFtU) (m_avOQ Network.HTTP.Client.Types.RequestBody) (m_aFtU Network.HTTP.Client.Types.RequestBody)
partGetBody
  f_aFu9
  (Network.HTTP.Client.MultipartFormData.Part x1_aFua x2_aFub x3_aFuc
                                              x4_aFud x5_aFue)
  = (GHC.Base.fmap
       (\ y1_aFuf
          -> ((((Network.HTTP.Client.MultipartFormData.Part x1_aFua) x2_aFub)
                 x3_aFuc)
                x4_aFud)
               y1_aFuf))
      (f_aFu9 x5_aFue)
{-# INLINE partGetBody #-}
partHeaders ::
  forall m_avOQ. Control.Lens.Type.Lens' (Network.HTTP.Client.MultipartFormData.PartM m_avOQ) [Network.HTTP.Types.Header.Header]
partHeaders
  f_aFug
  (Network.HTTP.Client.MultipartFormData.Part x1_aFuh x2_aFui x3_aFuj
                                              x4_aFuk x5_aFul)
  = (GHC.Base.fmap
       (\ y1_aFum
          -> ((((Network.HTTP.Client.MultipartFormData.Part x1_aFuh) x2_aFui)
                 x3_aFuj)
                y1_aFum)
               x5_aFul))
      (f_aFug x4_aFuk)
{-# INLINE partHeaders #-}
partName ::
  forall m_avOQ. Control.Lens.Type.Lens' (Network.HTTP.Client.MultipartFormData.PartM m_avOQ) Data.Text.Internal.Text
partName
  f_aFun
  (Network.HTTP.Client.MultipartFormData.Part x1_aFuo x2_aFup x3_aFuq
                                              x4_aFur x5_aFus)
  = (GHC.Base.fmap
       (\ y1_aFut
          -> ((((Network.HTTP.Client.MultipartFormData.Part y1_aFut) x2_aFup)
                 x3_aFuq)
                x4_aFur)
               x5_aFus))
      (f_aFun x1_aFuo)
{-# INLINE partName #-}

responseHeader :: HTTP.HeaderName -> Traversal' (HTTP.Response body) ByteString
responseHeader n = responseHeaders . assoc n

param :: Text -> Lens' Types.Options [Text]
param n = params . assoc2 n

header :: HTTP.HeaderName -> Lens' Types.Options [ByteString]
header n = headers . assoc2 n

_CookieJar :: Iso' HTTP.CookieJar [HTTP.Cookie]
_CookieJar = iso HTTP.destroyCookieJar HTTP.createCookieJar

-- N.B. This is an "illegal" traversal because we can change its cookie_name.
cookie :: ByteString -> Traversal' Types.Options HTTP.Cookie
cookie name = cookies . _Just . _CookieJar . traverse . filtered
              (\c -> HTTP.cookie_name c == name)

responseCookie :: ByteString -> Fold (HTTP.Response body) HTTP.Cookie
responseCookie name =
  responseCookieJar . folding HTTP.destroyCookieJar . filtered
  ((==name) . HTTP.cookie_name)

responseLink :: ByteString -> ByteString -> Fold (HTTP.Response body) Types.Link
responseLink name val =
  responseHeader "Link" . folding links .
  filtered (has (linkParams . folded . filtered (== (name,val))))
