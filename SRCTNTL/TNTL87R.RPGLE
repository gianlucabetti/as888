000100050930     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200050930
000300120531     Ftntl87d   CF   E             WORKSTN
000400050930     F                                     SFILE(S01:S01RRN)
000500120531     Ffiadt03l  IF   E           K DISK
000501120531     Ffiapd01l  IF   E           K DISK
001300050930
001301120531     D param           DS
001302120531     D  parnrc                        7s 0
001303120531     D  pardtsca                      8s 0
001304120604     D  paresito                      1
001700050930
001800050930     D S01RRN          S              5I 0
001900050930     D S01RRNFirst     S              5I 0
002000050930     D S01RRNLast      S              5I 0
002001120531     d dataiso         s               d   datfmt(*iso)
002002120531     d dataeur         s               d   datfmt(*eur)
002100060302      *--------------------------------------------------------------------
002200060302     c                   exsr      elabora
002300060302     c                   seton                                        lr
002400050930
002500060302      *___________________________________________________                  ****
002600060302     C     elabora       BEGSR
002700060302      *___________________________________________________                  ****
002800060302      * reperisco progressivi precedenti ed escludo il progressivo 0
002801120605     c                   z-add     parnrc        vnrc
002900120531     c     parnrc        setll     fiadt03l
003000060302     c                   do        *hival
003100120531     c     parnrc        reade     fiadt03l
003200120531     c                   if        %eof(fiadt03l)
003300060302     c                   leave
003400060302     c                   endif
003401060323      * solo fase 20
003402120531     c                   if        adtdst < pardtsca
003403060323     c                   iter
003404060323     c                   endif
003800060302      * caricamento sfl
003900060302     c                   exsr      carica
004000060302     c                   enddo
004100060302      * emissione sfl
004101060302     c     s01rrn        comp      0                                  0101
004200060302     c                   if        s01rrn > 0
004300060302     c                   z-add     1             s01rrn
004400060302     c                   exsr      emissione
004500060302     c                   endif
004600060302     c                   endsr
004700060302      *___________________________________________________                  ****
004800060302     C     carica        BEGSR
004900060302      *___________________________________________________                  ****
005000120531     c                   movel     adtprg        vprg
005100120531     c                   movel     adtpdr        vpdr
005101120531     c                   clear                   vpdrd
005102120531     c                   clear                   vddt
005103120531     c                   clear                   vdst
005104120531     c     kapd          chain     fiapd01l
005105120531     c                   if        %found(fiapd01l)
005106120531     c                   movel     apdrsc        vpdrd
005107120531     c                   endif
005200120531     c                   if        adtddt > 0
005201120531     c                   move      adtddt        dataiso
005202120531     c                   move      dataiso       dataeur
005203120531     c                   move      dataeur       vddt
005206120531     c                   endif
005207120531     c                   if        adtdst > 0
005208120605     c                   move      adtdst        dataiso
005209120531     c                   move      dataiso       dataeur
005210120531     c                   move      dataeur       vdst
005211120531     c                   endif
007616060323
007700060323     c                   if        s01rrn < 100
007800060302     c                   add       1             s01rrn
007900060302     c                   write     s01
008000060302     c                   endif
008100060302     c                   endsr
008200060302      *___________________________________________________                  ****
008300060302     C     emissione     BEGSR
008400060302      *___________________________________________________                  ****
008500051005
008501060323     c                   do        *hival
008701060302     C                   WRITE     f01
008702060302     C                   exfmt     c01
008900060302     c   kc              leave
009000060302     c                   enddo
009200050930
009201060323     C                   endsr
009300060302      *___________________________________________________                  ****
009400060302     C     *INZSR        BEGSR
009500060302      *___________________________________________________                  ****
009600060302     C     *ENTRY        PLIST
009700120531     C                   PARM                    param
010200060302
010300120531     C     kapd          KLIST
010400120531     C                   KFLD                    tipo              1
010500120531     C                   KFLD                    adtpdr
010501120531     C                   movel     'D'           tipo
010700060302
011500060302      *inizializza sfl
011600060302     C                   SETOn                                        02
011700060302     C                   WRITE     C01
011800060302     C                   SETOff                                       02
011900060302     C                   CLEAR                   S01RRN
012000060302     C                   CLEAR                   S01RRNLast
012100060302
012200060302     C                   ENDSR
012300060302
