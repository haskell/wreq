{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Network.Wreq.Internal.Lens
    (
      HTTP.Request
    , method
    , secure
    , host
    , port
    , path
    , queryString
    , requestHeaders
    , requestBody
    , requestVersion
    , requestManagerOverride
    , onRequestBodyException
    , proxy
    , hostAddress
    , rawBody
    , decompress
    , redirectCount
    , responseTimeout
    , checkResponse
    , cookieJar
    , seshCookies
    , seshManager
    , seshRun
    , seshRunHistory
    -- * Useful functions
    , assoc
    , assoc2
    , setHeader
    , maybeSetHeader
    , deleteKey
    ) where

import Control.Lens hiding (makeLenses)
import Data.List (partition)
import Network.HTTP.Client (Request)
import Network.HTTP.Types (HeaderName)
import Network.Wreq.Internal.Types (Session)
import qualified Data.ByteString as S
import qualified Network.HTTP.Client as HTTP

import qualified Network.HTTP.Client.Types
import qualified Control.Lens.Type
import qualified Data.ByteString.Internal
import qualified GHC.Base
import qualified GHC.Maybe
import qualified GHC.Types
import qualified Network.HTTP.Types.Header
import qualified Network.Wreq.Internal.Types
import qualified GHC.IORef
import qualified Network.Socket as Network.Socket.Types
import qualified Network.HTTP.Types.Method
import qualified GHC.Exception.Type
import qualified Network.HTTP.Types.Version

-- Network/Wreq/Internal/Lens.hs:47:1-25: Splicing declarations
checkResponse ::
  Control.Lens.Type.Lens'
    Network.HTTP.Client.Types.Request
    ( Network.HTTP.Client.Types.Request ->
      Network.HTTP.Client.Types.Response Network.HTTP.Client.Types.BodyReader ->
      GHC.Types.IO ()
    )
checkResponse
  f_ajCp
  ( Network.HTTP.Client.Types.Request
      x1_ajCq
      x2_ajCr
      x3_ajCs
      x4_ajCt
      x5_ajCu
      x6_ajCv
      x7_ajCw
      x8_ajCx
      x9_ajCy
      x10_ajCz
      x11_ajCA
      x12_ajCB
      x13_ajCC
      x14_ajCD
      x15_ajCE
      x16_ajCF
      x17_ajCG
      x18_ajCH
      x19_ajCI
      x20_ajCJ
      x21_ajCK
    ) =
    ( GHC.Base.fmap
        ( \y1_ajCL ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajCq)
                                                    x2_ajCr
                                                )
                                                  x3_ajCs
                                              )
                                                x4_ajCt
                                            )
                                              x5_ajCu
                                          )
                                            x6_ajCv
                                        )
                                          x7_ajCw
                                      )
                                        x8_ajCx
                                    )
                                      x9_ajCy
                                  )
                                    x10_ajCz
                                )
                                  x11_ajCA
                              )
                                x12_ajCB
                            )
                              x13_ajCC
                          )
                            y1_ajCL
                        )
                          x15_ajCE
                      )
                        x16_ajCF
                    )
                      x17_ajCG
                  )
                    x18_ajCH
                )
                  x19_ajCI
              )
                x20_ajCJ
            )
              x21_ajCK
        )
    )
      (f_ajCp x14_ajCD)
{-# INLINE checkResponse #-}
cookieJar ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request (GHC.Maybe.Maybe Network.HTTP.Client.Types.CookieJar)
cookieJar
  f_ajCM
  ( Network.HTTP.Client.Types.Request
      x1_ajCN
      x2_ajCO
      x3_ajCP
      x4_ajCQ
      x5_ajCR
      x6_ajCS
      x7_ajCT
      x8_ajCU
      x9_ajCV
      x10_ajCW
      x11_ajCX
      x12_ajCY
      x13_ajCZ
      x14_ajD0
      x15_ajD1
      x16_ajD2
      x17_ajD3
      x18_ajD4
      x19_ajD5
      x20_ajD6
      x21_ajD7
    ) =
    ( GHC.Base.fmap
        ( \y1_ajD8 ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajCN)
                                                    x2_ajCO
                                                )
                                                  x3_ajCP
                                              )
                                                x4_ajCQ
                                            )
                                              x5_ajCR
                                          )
                                            x6_ajCS
                                        )
                                          x7_ajCT
                                      )
                                        x8_ajCU
                                    )
                                      x9_ajCV
                                  )
                                    x10_ajCW
                                )
                                  x11_ajCX
                              )
                                x12_ajCY
                            )
                              x13_ajCZ
                          )
                            x14_ajD0
                        )
                          x15_ajD1
                      )
                        y1_ajD8
                    )
                      x17_ajD3
                  )
                    x18_ajD4
                )
                  x19_ajD5
              )
                x20_ajD6
            )
              x21_ajD7
        )
    )
      (f_ajCM x16_ajD2)
{-# INLINE cookieJar #-}
decompress ::
  Control.Lens.Type.Lens'
    Network.HTTP.Client.Types.Request
    ( Data.ByteString.Internal.ByteString ->
      GHC.Types.Bool
    )
decompress
  f_ajD9
  ( Network.HTTP.Client.Types.Request
      x1_ajDa
      x2_ajDb
      x3_ajDc
      x4_ajDd
      x5_ajDe
      x6_ajDf
      x7_ajDg
      x8_ajDh
      x9_ajDi
      x10_ajDj
      x11_ajDk
      x12_ajDl
      x13_ajDm
      x14_ajDn
      x15_ajDo
      x16_ajDp
      x17_ajDq
      x18_ajDr
      x19_ajDs
      x20_ajDt
      x21_ajDu
    ) =
    ( GHC.Base.fmap
        ( \y1_ajDv ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajDa)
                                                    x2_ajDb
                                                )
                                                  x3_ajDc
                                              )
                                                x4_ajDd
                                            )
                                              x5_ajDe
                                          )
                                            x6_ajDf
                                        )
                                          x7_ajDg
                                      )
                                        x8_ajDh
                                    )
                                      x9_ajDi
                                  )
                                    x10_ajDj
                                )
                                  x11_ajDk
                              )
                                y1_ajDv
                            )
                              x13_ajDm
                          )
                            x14_ajDn
                        )
                          x15_ajDo
                      )
                        x16_ajDp
                    )
                      x17_ajDq
                  )
                    x18_ajDr
                )
                  x19_ajDs
              )
                x20_ajDt
            )
              x21_ajDu
        )
    )
      (f_ajD9 x12_ajDl)
{-# INLINE decompress #-}
host ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Data.ByteString.Internal.ByteString
host
  f_ajDw
  ( Network.HTTP.Client.Types.Request
      x1_ajDx
      x2_ajDy
      x3_ajDz
      x4_ajDA
      x5_ajDB
      x6_ajDC
      x7_ajDD
      x8_ajDE
      x9_ajDF
      x10_ajDG
      x11_ajDH
      x12_ajDI
      x13_ajDJ
      x14_ajDK
      x15_ajDL
      x16_ajDM
      x17_ajDN
      x18_ajDO
      x19_ajDP
      x20_ajDQ
      x21_ajDR
    ) =
    ( GHC.Base.fmap
        ( \y1_ajDS ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajDx)
                                                    x2_ajDy
                                                )
                                                  y1_ajDS
                                              )
                                                x4_ajDA
                                            )
                                              x5_ajDB
                                          )
                                            x6_ajDC
                                        )
                                          x7_ajDD
                                      )
                                        x8_ajDE
                                    )
                                      x9_ajDF
                                  )
                                    x10_ajDG
                                )
                                  x11_ajDH
                              )
                                x12_ajDI
                            )
                              x13_ajDJ
                          )
                            x14_ajDK
                        )
                          x15_ajDL
                      )
                        x16_ajDM
                    )
                      x17_ajDN
                  )
                    x18_ajDO
                )
                  x19_ajDP
              )
                x20_ajDQ
            )
              x21_ajDR
        )
    )
      (f_ajDw x3_ajDz)
{-# INLINE host #-}
hostAddress ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request (GHC.Maybe.Maybe Network.Socket.Types.HostAddress)
hostAddress
  f_ajDT
  ( Network.HTTP.Client.Types.Request
      x1_ajDU
      x2_ajDV
      x3_ajDW
      x4_ajDX
      x5_ajDY
      x6_ajDZ
      x7_ajE0
      x8_ajE1
      x9_ajE2
      x10_ajE3
      x11_ajE4
      x12_ajE5
      x13_ajE6
      x14_ajE7
      x15_ajE8
      x16_ajE9
      x17_ajEa
      x18_ajEb
      x19_ajEc
      x20_ajEd
      x21_ajEe
    ) =
    ( GHC.Base.fmap
        ( \y1_ajEf ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajDU)
                                                    x2_ajDV
                                                )
                                                  x3_ajDW
                                              )
                                                x4_ajDX
                                            )
                                              x5_ajDY
                                          )
                                            x6_ajDZ
                                        )
                                          x7_ajE0
                                      )
                                        x8_ajE1
                                    )
                                      x9_ajE2
                                  )
                                    y1_ajEf
                                )
                                  x11_ajE4
                              )
                                x12_ajE5
                            )
                              x13_ajE6
                          )
                            x14_ajE7
                        )
                          x15_ajE8
                      )
                        x16_ajE9
                    )
                      x17_ajEa
                  )
                    x18_ajEb
                )
                  x19_ajEc
              )
                x20_ajEd
            )
              x21_ajEe
        )
    )
      (f_ajDT x10_ajE3)
{-# INLINE hostAddress #-}
method ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Network.HTTP.Types.Method.Method
method
  f_ajEg
  ( Network.HTTP.Client.Types.Request
      x1_ajEh
      x2_ajEi
      x3_ajEj
      x4_ajEk
      x5_ajEl
      x6_ajEm
      x7_ajEn
      x8_ajEo
      x9_ajEp
      x10_ajEq
      x11_ajEr
      x12_ajEs
      x13_ajEt
      x14_ajEu
      x15_ajEv
      x16_ajEw
      x17_ajEx
      x18_ajEy
      x19_ajEz
      x20_ajEA
      x21_ajEB
    ) =
    ( GHC.Base.fmap
        ( \y1_ajEC ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request y1_ajEC)
                                                    x2_ajEi
                                                )
                                                  x3_ajEj
                                              )
                                                x4_ajEk
                                            )
                                              x5_ajEl
                                          )
                                            x6_ajEm
                                        )
                                          x7_ajEn
                                      )
                                        x8_ajEo
                                    )
                                      x9_ajEp
                                  )
                                    x10_ajEq
                                )
                                  x11_ajEr
                              )
                                x12_ajEs
                            )
                              x13_ajEt
                          )
                            x14_ajEu
                        )
                          x15_ajEv
                      )
                        x16_ajEw
                    )
                      x17_ajEx
                  )
                    x18_ajEy
                )
                  x19_ajEz
              )
                x20_ajEA
            )
              x21_ajEB
        )
    )
      (f_ajEg x1_ajEh)
{-# INLINE method #-}
onRequestBodyException ::
  Control.Lens.Type.Lens'
    Network.HTTP.Client.Types.Request
    ( GHC.Exception.Type.SomeException ->
      GHC.Types.IO ()
    )
onRequestBodyException
  f_ajED
  ( Network.HTTP.Client.Types.Request
      x1_ajEE
      x2_ajEF
      x3_ajEG
      x4_ajEH
      x5_ajEI
      x6_ajEJ
      x7_ajEK
      x8_ajEL
      x9_ajEM
      x10_ajEN
      x11_ajEO
      x12_ajEP
      x13_ajEQ
      x14_ajER
      x15_ajES
      x16_ajET
      x17_ajEU
      x18_ajEV
      x19_ajEW
      x20_ajEX
      x21_ajEY
    ) =
    ( GHC.Base.fmap
        ( \y1_ajEZ ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajEE)
                                                    x2_ajEF
                                                )
                                                  x3_ajEG
                                              )
                                                x4_ajEH
                                            )
                                              x5_ajEI
                                          )
                                            x6_ajEJ
                                        )
                                          x7_ajEK
                                      )
                                        x8_ajEL
                                    )
                                      x9_ajEM
                                  )
                                    x10_ajEN
                                )
                                  x11_ajEO
                              )
                                x12_ajEP
                            )
                              x13_ajEQ
                          )
                            x14_ajER
                        )
                          x15_ajES
                      )
                        x16_ajET
                    )
                      x17_ajEU
                  )
                    y1_ajEZ
                )
                  x19_ajEW
              )
                x20_ajEX
            )
              x21_ajEY
        )
    )
      (f_ajED x18_ajEV)
{-# INLINE onRequestBodyException #-}
path ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Data.ByteString.Internal.ByteString
path
  f_ajF0
  ( Network.HTTP.Client.Types.Request
      x1_ajF1
      x2_ajF2
      x3_ajF3
      x4_ajF4
      x5_ajF5
      x6_ajF6
      x7_ajF7
      x8_ajF8
      x9_ajF9
      x10_ajFa
      x11_ajFb
      x12_ajFc
      x13_ajFd
      x14_ajFe
      x15_ajFf
      x16_ajFg
      x17_ajFh
      x18_ajFi
      x19_ajFj
      x20_ajFk
      x21_ajFl
    ) =
    ( GHC.Base.fmap
        ( \y1_ajFm ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajF1)
                                                    x2_ajF2
                                                )
                                                  x3_ajF3
                                              )
                                                x4_ajF4
                                            )
                                              y1_ajFm
                                          )
                                            x6_ajF6
                                        )
                                          x7_ajF7
                                      )
                                        x8_ajF8
                                    )
                                      x9_ajF9
                                  )
                                    x10_ajFa
                                )
                                  x11_ajFb
                              )
                                x12_ajFc
                            )
                              x13_ajFd
                          )
                            x14_ajFe
                        )
                          x15_ajFf
                      )
                        x16_ajFg
                    )
                      x17_ajFh
                  )
                    x18_ajFi
                )
                  x19_ajFj
              )
                x20_ajFk
            )
              x21_ajFl
        )
    )
      (f_ajF0 x5_ajF5)
{-# INLINE path #-}
port ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request GHC.Types.Int
port
  f_ajFn
  ( Network.HTTP.Client.Types.Request
      x1_ajFo
      x2_ajFp
      x3_ajFq
      x4_ajFr
      x5_ajFs
      x6_ajFt
      x7_ajFu
      x8_ajFv
      x9_ajFw
      x10_ajFx
      x11_ajFy
      x12_ajFz
      x13_ajFA
      x14_ajFB
      x15_ajFC
      x16_ajFD
      x17_ajFE
      x18_ajFF
      x19_ajFG
      x20_ajFH
      x21_ajFI
    ) =
    ( GHC.Base.fmap
        ( \y1_ajFJ ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajFo)
                                                    x2_ajFp
                                                )
                                                  x3_ajFq
                                              )
                                                y1_ajFJ
                                            )
                                              x5_ajFs
                                          )
                                            x6_ajFt
                                        )
                                          x7_ajFu
                                      )
                                        x8_ajFv
                                    )
                                      x9_ajFw
                                  )
                                    x10_ajFx
                                )
                                  x11_ajFy
                              )
                                x12_ajFz
                            )
                              x13_ajFA
                          )
                            x14_ajFB
                        )
                          x15_ajFC
                      )
                        x16_ajFD
                    )
                      x17_ajFE
                  )
                    x18_ajFF
                )
                  x19_ajFG
              )
                x20_ajFH
            )
              x21_ajFI
        )
    )
      (f_ajFn x4_ajFr)
{-# INLINE port #-}
proxy ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request (GHC.Maybe.Maybe Network.HTTP.Client.Types.Proxy)
proxy
  f_ajFK
  ( Network.HTTP.Client.Types.Request
      x1_ajFL
      x2_ajFM
      x3_ajFN
      x4_ajFO
      x5_ajFP
      x6_ajFQ
      x7_ajFR
      x8_ajFS
      x9_ajFT
      x10_ajFU
      x11_ajFV
      x12_ajFW
      x13_ajFX
      x14_ajFY
      x15_ajFZ
      x16_ajG0
      x17_ajG1
      x18_ajG2
      x19_ajG3
      x20_ajG4
      x21_ajG5
    ) =
    ( GHC.Base.fmap
        ( \y1_ajG6 ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajFL)
                                                    x2_ajFM
                                                )
                                                  x3_ajFN
                                              )
                                                x4_ajFO
                                            )
                                              x5_ajFP
                                          )
                                            x6_ajFQ
                                        )
                                          x7_ajFR
                                      )
                                        x8_ajFS
                                    )
                                      y1_ajG6
                                  )
                                    x10_ajFU
                                )
                                  x11_ajFV
                              )
                                x12_ajFW
                            )
                              x13_ajFX
                          )
                            x14_ajFY
                        )
                          x15_ajFZ
                      )
                        x16_ajG0
                    )
                      x17_ajG1
                  )
                    x18_ajG2
                )
                  x19_ajG3
              )
                x20_ajG4
            )
              x21_ajG5
        )
    )
      (f_ajFK x9_ajFT)
{-# INLINE proxy #-}
proxySecureMode ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Network.HTTP.Client.Types.ProxySecureMode
proxySecureMode
  f_ajG7
  ( Network.HTTP.Client.Types.Request
      x1_ajG8
      x2_ajG9
      x3_ajGa
      x4_ajGb
      x5_ajGc
      x6_ajGd
      x7_ajGe
      x8_ajGf
      x9_ajGg
      x10_ajGh
      x11_ajGi
      x12_ajGj
      x13_ajGk
      x14_ajGl
      x15_ajGm
      x16_ajGn
      x17_ajGo
      x18_ajGp
      x19_ajGq
      x20_ajGr
      x21_ajGs
    ) =
    ( GHC.Base.fmap
        ( \y1_ajGt ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajG8)
                                                    x2_ajG9
                                                )
                                                  x3_ajGa
                                              )
                                                x4_ajGb
                                            )
                                              x5_ajGc
                                          )
                                            x6_ajGd
                                        )
                                          x7_ajGe
                                      )
                                        x8_ajGf
                                    )
                                      x9_ajGg
                                  )
                                    x10_ajGh
                                )
                                  x11_ajGi
                              )
                                x12_ajGj
                            )
                              x13_ajGk
                          )
                            x14_ajGl
                        )
                          x15_ajGm
                      )
                        x16_ajGn
                    )
                      x17_ajGo
                  )
                    x18_ajGp
                )
                  x19_ajGq
              )
                x20_ajGr
            )
              y1_ajGt
        )
    )
      (f_ajG7 x21_ajGs)
{-# INLINE proxySecureMode #-}
queryString ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Data.ByteString.Internal.ByteString
queryString
  f_ajGu
  ( Network.HTTP.Client.Types.Request
      x1_ajGv
      x2_ajGw
      x3_ajGx
      x4_ajGy
      x5_ajGz
      x6_ajGA
      x7_ajGB
      x8_ajGC
      x9_ajGD
      x10_ajGE
      x11_ajGF
      x12_ajGG
      x13_ajGH
      x14_ajGI
      x15_ajGJ
      x16_ajGK
      x17_ajGL
      x18_ajGM
      x19_ajGN
      x20_ajGO
      x21_ajGP
    ) =
    ( GHC.Base.fmap
        ( \y1_ajGQ ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajGv)
                                                    x2_ajGw
                                                )
                                                  x3_ajGx
                                              )
                                                x4_ajGy
                                            )
                                              x5_ajGz
                                          )
                                            y1_ajGQ
                                        )
                                          x7_ajGB
                                      )
                                        x8_ajGC
                                    )
                                      x9_ajGD
                                  )
                                    x10_ajGE
                                )
                                  x11_ajGF
                              )
                                x12_ajGG
                            )
                              x13_ajGH
                          )
                            x14_ajGI
                        )
                          x15_ajGJ
                      )
                        x16_ajGK
                    )
                      x17_ajGL
                  )
                    x18_ajGM
                )
                  x19_ajGN
              )
                x20_ajGO
            )
              x21_ajGP
        )
    )
      (f_ajGu x6_ajGA)
{-# INLINE queryString #-}
rawBody ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request GHC.Types.Bool
rawBody
  f_ajGR
  ( Network.HTTP.Client.Types.Request
      x1_ajGS
      x2_ajGT
      x3_ajGU
      x4_ajGV
      x5_ajGW
      x6_ajGX
      x7_ajGY
      x8_ajGZ
      x9_ajH0
      x10_ajH1
      x11_ajH2
      x12_ajH3
      x13_ajH4
      x14_ajH5
      x15_ajH6
      x16_ajH7
      x17_ajH8
      x18_ajH9
      x19_ajHa
      x20_ajHb
      x21_ajHc
    ) =
    ( GHC.Base.fmap
        ( \y1_ajHd ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajGS)
                                                    x2_ajGT
                                                )
                                                  x3_ajGU
                                              )
                                                x4_ajGV
                                            )
                                              x5_ajGW
                                          )
                                            x6_ajGX
                                        )
                                          x7_ajGY
                                      )
                                        x8_ajGZ
                                    )
                                      x9_ajH0
                                  )
                                    x10_ajH1
                                )
                                  y1_ajHd
                              )
                                x12_ajH3
                            )
                              x13_ajH4
                          )
                            x14_ajH5
                        )
                          x15_ajH6
                      )
                        x16_ajH7
                    )
                      x17_ajH8
                  )
                    x18_ajH9
                )
                  x19_ajHa
              )
                x20_ajHb
            )
              x21_ajHc
        )
    )
      (f_ajGR x11_ajH2)
{-# INLINE rawBody #-}
redirectCount ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request GHC.Types.Int
redirectCount
  f_ajHe
  ( Network.HTTP.Client.Types.Request
      x1_ajHf
      x2_ajHg
      x3_ajHh
      x4_ajHi
      x5_ajHj
      x6_ajHk
      x7_ajHl
      x8_ajHm
      x9_ajHn
      x10_ajHo
      x11_ajHp
      x12_ajHq
      x13_ajHr
      x14_ajHs
      x15_ajHt
      x16_ajHu
      x17_ajHv
      x18_ajHw
      x19_ajHx
      x20_ajHy
      x21_ajHz
    ) =
    ( GHC.Base.fmap
        ( \y1_ajHA ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajHf)
                                                    x2_ajHg
                                                )
                                                  x3_ajHh
                                              )
                                                x4_ajHi
                                            )
                                              x5_ajHj
                                          )
                                            x6_ajHk
                                        )
                                          x7_ajHl
                                      )
                                        x8_ajHm
                                    )
                                      x9_ajHn
                                  )
                                    x10_ajHo
                                )
                                  x11_ajHp
                              )
                                x12_ajHq
                            )
                              y1_ajHA
                          )
                            x14_ajHs
                        )
                          x15_ajHt
                      )
                        x16_ajHu
                    )
                      x17_ajHv
                  )
                    x18_ajHw
                )
                  x19_ajHx
              )
                x20_ajHy
            )
              x21_ajHz
        )
    )
      (f_ajHe x13_ajHr)
{-# INLINE redirectCount #-}
requestBody ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Network.HTTP.Client.Types.RequestBody
requestBody
  f_ajHB
  ( Network.HTTP.Client.Types.Request
      x1_ajHC
      x2_ajHD
      x3_ajHE
      x4_ajHF
      x5_ajHG
      x6_ajHH
      x7_ajHI
      x8_ajHJ
      x9_ajHK
      x10_ajHL
      x11_ajHM
      x12_ajHN
      x13_ajHO
      x14_ajHP
      x15_ajHQ
      x16_ajHR
      x17_ajHS
      x18_ajHT
      x19_ajHU
      x20_ajHV
      x21_ajHW
    ) =
    ( GHC.Base.fmap
        ( \y1_ajHX ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajHC)
                                                    x2_ajHD
                                                )
                                                  x3_ajHE
                                              )
                                                x4_ajHF
                                            )
                                              x5_ajHG
                                          )
                                            x6_ajHH
                                        )
                                          x7_ajHI
                                      )
                                        y1_ajHX
                                    )
                                      x9_ajHK
                                  )
                                    x10_ajHL
                                )
                                  x11_ajHM
                              )
                                x12_ajHN
                            )
                              x13_ajHO
                          )
                            x14_ajHP
                        )
                          x15_ajHQ
                      )
                        x16_ajHR
                    )
                      x17_ajHS
                  )
                    x18_ajHT
                )
                  x19_ajHU
              )
                x20_ajHV
            )
              x21_ajHW
        )
    )
      (f_ajHB x8_ajHJ)
{-# INLINE requestBody #-}
requestHeaders ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Network.HTTP.Types.Header.RequestHeaders
requestHeaders
  f_ajHY
  ( Network.HTTP.Client.Types.Request
      x1_ajHZ
      x2_ajI0
      x3_ajI1
      x4_ajI2
      x5_ajI3
      x6_ajI4
      x7_ajI5
      x8_ajI6
      x9_ajI7
      x10_ajI8
      x11_ajI9
      x12_ajIa
      x13_ajIb
      x14_ajIc
      x15_ajId
      x16_ajIe
      x17_ajIf
      x18_ajIg
      x19_ajIh
      x20_ajIi
      x21_ajIj
    ) =
    ( GHC.Base.fmap
        ( \y1_ajIk ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajHZ)
                                                    x2_ajI0
                                                )
                                                  x3_ajI1
                                              )
                                                x4_ajI2
                                            )
                                              x5_ajI3
                                          )
                                            x6_ajI4
                                        )
                                          y1_ajIk
                                      )
                                        x8_ajI6
                                    )
                                      x9_ajI7
                                  )
                                    x10_ajI8
                                )
                                  x11_ajI9
                              )
                                x12_ajIa
                            )
                              x13_ajIb
                          )
                            x14_ajIc
                        )
                          x15_ajId
                      )
                        x16_ajIe
                    )
                      x17_ajIf
                  )
                    x18_ajIg
                )
                  x19_ajIh
              )
                x20_ajIi
            )
              x21_ajIj
        )
    )
      (f_ajHY x7_ajI5)
{-# INLINE requestHeaders #-}
requestManagerOverride ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request (GHC.Maybe.Maybe Network.HTTP.Client.Types.Manager)
requestManagerOverride
  f_ajIl
  ( Network.HTTP.Client.Types.Request
      x1_ajIm
      x2_ajIn
      x3_ajIo
      x4_ajIp
      x5_ajIq
      x6_ajIr
      x7_ajIs
      x8_ajIt
      x9_ajIu
      x10_ajIv
      x11_ajIw
      x12_ajIx
      x13_ajIy
      x14_ajIz
      x15_ajIA
      x16_ajIB
      x17_ajIC
      x18_ajID
      x19_ajIE
      x20_ajIF
      x21_ajIG
    ) =
    ( GHC.Base.fmap
        ( \y1_ajIH ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajIm)
                                                    x2_ajIn
                                                )
                                                  x3_ajIo
                                              )
                                                x4_ajIp
                                            )
                                              x5_ajIq
                                          )
                                            x6_ajIr
                                        )
                                          x7_ajIs
                                      )
                                        x8_ajIt
                                    )
                                      x9_ajIu
                                  )
                                    x10_ajIv
                                )
                                  x11_ajIw
                              )
                                x12_ajIx
                            )
                              x13_ajIy
                          )
                            x14_ajIz
                        )
                          x15_ajIA
                      )
                        x16_ajIB
                    )
                      x17_ajIC
                  )
                    x18_ajID
                )
                  y1_ajIH
              )
                x20_ajIF
            )
              x21_ajIG
        )
    )
      (f_ajIl x19_ajIE)
{-# INLINE requestManagerOverride #-}
requestVersion ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Network.HTTP.Types.Version.HttpVersion
requestVersion
  f_ajII
  ( Network.HTTP.Client.Types.Request
      x1_ajIJ
      x2_ajIK
      x3_ajIL
      x4_ajIM
      x5_ajIN
      x6_ajIO
      x7_ajIP
      x8_ajIQ
      x9_ajIR
      x10_ajIS
      x11_ajIT
      x12_ajIU
      x13_ajIV
      x14_ajIW
      x15_ajIX
      x16_ajIY
      x17_ajIZ
      x18_ajJ0
      x19_ajJ1
      x20_ajJ2
      x21_ajJ3
    ) =
    ( GHC.Base.fmap
        ( \y1_ajJ4 ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajIJ)
                                                    x2_ajIK
                                                )
                                                  x3_ajIL
                                              )
                                                x4_ajIM
                                            )
                                              x5_ajIN
                                          )
                                            x6_ajIO
                                        )
                                          x7_ajIP
                                      )
                                        x8_ajIQ
                                    )
                                      x9_ajIR
                                  )
                                    x10_ajIS
                                )
                                  x11_ajIT
                              )
                                x12_ajIU
                            )
                              x13_ajIV
                          )
                            x14_ajIW
                        )
                          x15_ajIX
                      )
                        x16_ajIY
                    )
                      y1_ajJ4
                  )
                    x18_ajJ0
                )
                  x19_ajJ1
              )
                x20_ajJ2
            )
              x21_ajJ3
        )
    )
      (f_ajII x17_ajIZ)
{-# INLINE requestVersion #-}
responseTimeout ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request Network.HTTP.Client.Types.ResponseTimeout
responseTimeout
  f_ajJ5
  ( Network.HTTP.Client.Types.Request
      x1_ajJ6
      x2_ajJ7
      x3_ajJ8
      x4_ajJ9
      x5_ajJa
      x6_ajJb
      x7_ajJc
      x8_ajJd
      x9_ajJe
      x10_ajJf
      x11_ajJg
      x12_ajJh
      x13_ajJi
      x14_ajJj
      x15_ajJk
      x16_ajJl
      x17_ajJm
      x18_ajJn
      x19_ajJo
      x20_ajJp
      x21_ajJq
    ) =
    ( GHC.Base.fmap
        ( \y1_ajJr ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajJ6)
                                                    x2_ajJ7
                                                )
                                                  x3_ajJ8
                                              )
                                                x4_ajJ9
                                            )
                                              x5_ajJa
                                          )
                                            x6_ajJb
                                        )
                                          x7_ajJc
                                      )
                                        x8_ajJd
                                    )
                                      x9_ajJe
                                  )
                                    x10_ajJf
                                )
                                  x11_ajJg
                              )
                                x12_ajJh
                            )
                              x13_ajJi
                          )
                            x14_ajJj
                        )
                          y1_ajJr
                      )
                        x16_ajJl
                    )
                      x17_ajJm
                  )
                    x18_ajJn
                )
                  x19_ajJo
              )
                x20_ajJp
            )
              x21_ajJq
        )
    )
      (f_ajJ5 x15_ajJk)
{-# INLINE responseTimeout #-}
secure ::
  Control.Lens.Type.Lens' Network.HTTP.Client.Types.Request GHC.Types.Bool
secure
  f_ajJs
  ( Network.HTTP.Client.Types.Request
      x1_ajJt
      x2_ajJu
      x3_ajJv
      x4_ajJw
      x5_ajJx
      x6_ajJy
      x7_ajJz
      x8_ajJA
      x9_ajJB
      x10_ajJC
      x11_ajJD
      x12_ajJE
      x13_ajJF
      x14_ajJG
      x15_ajJH
      x16_ajJI
      x17_ajJJ
      x18_ajJK
      x19_ajJL
      x20_ajJM
      x21_ajJN
    ) =
    ( GHC.Base.fmap
        ( \y1_ajJO ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajJt)
                                                    y1_ajJO
                                                )
                                                  x3_ajJv
                                              )
                                                x4_ajJw
                                            )
                                              x5_ajJx
                                          )
                                            x6_ajJy
                                        )
                                          x7_ajJz
                                      )
                                        x8_ajJA
                                    )
                                      x9_ajJB
                                  )
                                    x10_ajJC
                                )
                                  x11_ajJD
                              )
                                x12_ajJE
                            )
                              x13_ajJF
                          )
                            x14_ajJG
                        )
                          x15_ajJH
                      )
                        x16_ajJI
                    )
                      x17_ajJJ
                  )
                    x18_ajJK
                )
                  x19_ajJL
              )
                x20_ajJM
            )
              x21_ajJN
        )
    )
      (f_ajJs x2_ajJu)
{-# INLINE secure #-}
shouldStripHeaderOnRedirect ::
  Control.Lens.Type.Lens'
    Network.HTTP.Client.Types.Request
    ( Network.HTTP.Types.Header.HeaderName ->
      GHC.Types.Bool
    )
shouldStripHeaderOnRedirect
  f_ajJP
  ( Network.HTTP.Client.Types.Request
      x1_ajJQ
      x2_ajJR
      x3_ajJS
      x4_ajJT
      x5_ajJU
      x6_ajJV
      x7_ajJW
      x8_ajJX
      x9_ajJY
      x10_ajJZ
      x11_ajK0
      x12_ajK1
      x13_ajK2
      x14_ajK3
      x15_ajK4
      x16_ajK5
      x17_ajK6
      x18_ajK7
      x19_ajK8
      x20_ajK9
      x21_ajKa
    ) =
    ( GHC.Base.fmap
        ( \y1_ajKb ->
            ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( ( (Network.HTTP.Client.Types.Request x1_ajJQ)
                                                    x2_ajJR
                                                )
                                                  x3_ajJS
                                              )
                                                x4_ajJT
                                            )
                                              x5_ajJU
                                          )
                                            x6_ajJV
                                        )
                                          x7_ajJW
                                      )
                                        x8_ajJX
                                    )
                                      x9_ajJY
                                  )
                                    x10_ajJZ
                                )
                                  x11_ajK0
                              )
                                x12_ajK1
                            )
                              x13_ajK2
                          )
                            x14_ajK3
                        )
                          x15_ajK4
                      )
                        x16_ajK5
                    )
                      x17_ajK6
                  )
                    x18_ajK7
                )
                  x19_ajK8
              )
                y1_ajKb
            )
              x21_ajKa
        )
    )
      (f_ajJP x20_ajK9)
{-# INLINE shouldStripHeaderOnRedirect #-}

-- Network/Wreq/Internal/Lens.hs:48:1-20: Splicing declarations
seshCookies ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Session (GHC.Maybe.Maybe (GHC.IORef.IORef Network.HTTP.Client.Types.CookieJar))
seshCookies
  f_ak82
  ( Network.Wreq.Internal.Types.Session
      x1_ak83
      x2_ak84
      x3_ak85
      x4_ak86
    ) =
    ( GHC.Base.fmap
        ( \y1_ak87 ->
            ( ((Network.Wreq.Internal.Types.Session y1_ak87) x2_ak84)
                x3_ak85
            )
              x4_ak86
        )
    )
      (f_ak82 x1_ak83)
{-# INLINE seshCookies #-}
seshManager ::
  Control.Lens.Type.Lens' Network.Wreq.Internal.Types.Session Network.HTTP.Client.Types.Manager
seshManager
  f_ak88
  ( Network.Wreq.Internal.Types.Session
      x1_ak89
      x2_ak8a
      x3_ak8b
      x4_ak8c
    ) =
    ( GHC.Base.fmap
        ( \y1_ak8d ->
            ( ((Network.Wreq.Internal.Types.Session x1_ak89) y1_ak8d)
                x3_ak8b
            )
              x4_ak8c
        )
    )
      (f_ak88 x2_ak8a)
{-# INLINE seshManager #-}
seshRun ::
  Control.Lens.Type.Lens'
    Network.Wreq.Internal.Types.Session
    ( Network.Wreq.Internal.Types.Session ->
      Network.Wreq.Internal.Types.Run Network.Wreq.Internal.Types.Body ->
      Network.Wreq.Internal.Types.Run Network.Wreq.Internal.Types.Body
    )
seshRun
  f_ak8e
  ( Network.Wreq.Internal.Types.Session
      x1_ak8f
      x2_ak8g
      x3_ak8h
      x4_ak8i
    ) =
    ( GHC.Base.fmap
        ( \y1_ak8j ->
            ( ((Network.Wreq.Internal.Types.Session x1_ak8f) x2_ak8g)
                y1_ak8j
            )
              x4_ak8i
        )
    )
      (f_ak8e x3_ak8h)
{-# INLINE seshRun #-}
seshRunHistory ::
  Control.Lens.Type.Lens'
    Network.Wreq.Internal.Types.Session
    ( Network.Wreq.Internal.Types.Session ->
      Network.Wreq.Internal.Types.RunHistory Network.Wreq.Internal.Types.Body ->
      Network.Wreq.Internal.Types.RunHistory Network.Wreq.Internal.Types.Body
    )
seshRunHistory
  f_ak8k
  ( Network.Wreq.Internal.Types.Session
      x1_ak8l
      x2_ak8m
      x3_ak8n
      x4_ak8o
    ) =
    ( GHC.Base.fmap
        ( \y1_ak8p ->
            ( ((Network.Wreq.Internal.Types.Session x1_ak8l) x2_ak8m)
                x3_ak8n
            )
              y1_ak8p
        )
    )
      (f_ak8k x4_ak8o)
{-# INLINE seshRunHistory #-}

assoc :: (Eq k) => k -> IndexedTraversal' k [(k, a)] a
assoc i = traverse . itraversed . index i

assoc2 :: Eq k => k -> Lens' [(k,a)] [a]
-- This is only a lens up to the ordering of the list (which changes
-- when we modify the list).
-- assoc2 :: (Eq b, Functor f) => b -> ([a] -> f [a]) -> [(b, a)] -> f [(b, a)]
assoc2 k f = fmap (uncurry ((++) . fmap ((,) k))) .
             _1 (f . fmap snd) . partition ((==k) . fst)

-- | Set a header to the given value, replacing any prior value.
setHeader :: HeaderName -> S.ByteString -> Request -> Request
setHeader name value = requestHeaders %~ ((name,value) :) . deleteKey name

-- | Set a header to the given value, but only if the header was not
-- already set.
maybeSetHeader :: HeaderName -> S.ByteString -> Request -> Request
maybeSetHeader name value = requestHeaders %~
  \hdrs -> case lookup name hdrs of
             Just _  -> hdrs
             Nothing -> (name,value) : hdrs

deleteKey :: (Eq a) => a -> [(a,b)] -> [(a,b)]
deleteKey key = filter ((/= key) . fst)
