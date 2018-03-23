000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040913      * FITS20R  *----------------------------------------------------*
000400070207      *  autotrasportatori: FIAPD*
000500021010      *---------------------------------------------------------------* c
000600040913     fFIAPDNEW  If A e           k disk    rename(FIAPD000:APDNEW)
000700040913     fFIAPD00F  If   e             disk
000800070207     fFIAPD2NEW If A e           k disk    rename(FIAPD200:APD2NEW)
000900070207     fFIAPD20F  If   e             disk
001000070207     fFIAPD3NEW If A e           k disk    rename(FIAPD300:APD3NEW)
001100070207     fFIAPD30F  If   e             disk
001200140507     fFIAPD4NEW If A e           k disk    rename(FIAPD400:APD4NEW)
001300140507     fFIAPD40F  If   e             disk
001400070725     fFIAPD5NEW If A e           k disk    rename(FIAPD500:APD5NEW)
001500070725     fFIAPD50F  If   e             disk
001600040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001700021008      *
001800021010     d KPJBA         E DS
001900040805     D* Schiera p.o. da trasferire
002000040805     D  SKPO           s              3    dim(500)
002100040805      *
002200021008      *****************************************************************
002300021010     c     *ENTRY        PLIST
002400021010     c                   PARM                    KPJBA
002500040805     C                   parm                    PARAM          1500
002600040913     C     KAPD          klist
002700040913     C                   kfld                    APDTIP
002800040913     C                   kfld                    APDPDR
002900070207     C     KAPD2         klist
003000070207     C                   kfld                    APD2TIPA
003100070207     C                   kfld                    APD2PDR
003200070207     C                   kfld                    APD2FPP
003300070207     C                   kfld                    APD2DDE
003400070207     C                   kfld                    APD2DSCAD
003500070207     C     KAPD3         klist
003600070207     C                   kfld                    APD3TIPA
003700070207     C                   kfld                    APD3PDR
003800070207     C                   kfld                    APD3FPP
003900070207     C                   kfld                    APD3DDE
004000070207     C                   kfld                    APD3DSCAD
004100070207     C                   kfld                    APD3PRGG
004200070207     C                   kfld                    APD3SQL
004300140507     C     KAPD4         klist
004400140507     C                   kfld                    APD4TIPA
004500140507     C                   kfld                    APD4PDR
004600140507     C                   kfld                    APD4TRD
004700070725     C     KAPD5         klist
004800070725     C                   kfld                    APD5TIPA
004900070725     C                   kfld                    APD5PDR
005000070725     C                   kfld                    APD5FPP
005100070725     C                   kfld                    APD5DDE
005200070725     C                   kfld                    APD5DSCAD
005300070725     C                   kfld                    APD5PRGG
005400070725     C                   kfld                    APD5SQL
005500040805     c
005600040805     c     ' ':'0'       xlate     param         param
005700040805     c                   movea     param         skpo
005800021010      *---------------------------------------------------------------*
005900021008      ****
006000040910      ** LETTURA
006100021008      ****
006200040913     c                   READ      Fiapd00f                               22
006300040910    1c     *IN22         DOWEQ     *OFF
006400040805     c
006500040913     c                   movel     apdpdr        wlinea            3
006600040805     c     wlinea        lookup    skpo                                   35
006700040913     C
006800040910    2c                   if        *in35
006900040913     c     kapd          setll     fiaPDnew                               31
007000040910    3c                   if        not *in31
007100040913     c                   write     apdNEW                               61
007200040913     c                   add       1             Totapd            7 0
007300040910    3c                   endif
007400040910    2c                   endif
007500021008      *
007600040913     c                   READ      Fiapd00f                               22
007700040910    1c                   ENDDO
007800070207     c* fiapd20f
007900070207     c                   READ      Fiapd20f                               22
008000070207    1c     *IN22         DOWEQ     *OFF
008100070207     c
008200070207     c                   movel     apd2pdr       wlinea            3
008300070207     c     wlinea        lookup    skpo                                   35
008400070207     C
008500070207    2c                   if        *in35
008600070207     c     kapd2         setll     fiaPD2new                              31
008700070207    3c                   if        not *in31
008800070207     c                   write     apd2NEW                              61
008900070207     c                   add       1             Totapd2           7 0
009000070207    3c                   endif
009100070207    2c                   endif
009200070207      *
009300070207     c                   READ      Fiapd20f                               22
009400070207    1c                   ENDDO
009500070207     c* fiapd30f
009600070207     c                   READ      Fiapd30f                               22
009700070207    1c     *IN22         DOWEQ     *OFF
009800070207     c
009900070207     c                   movel     apd3pdr       wlinea            3
010000070207     c     wlinea        lookup    skpo                                   35
010100070207     C
010200070207    2c                   if        *in35
010300070207     c     kapd3         setll     fiaPD3new                              31
010400070207    3c                   if        not *in31
010500070207     c                   write     apd3NEW                              61
010600070207     c                   add       1             Totapd3           7 0
010700070207    3c                   endif
010800070207    2c                   endif
010900070207      *
011000070207     c                   READ      Fiapd30f                               22
011100070207    1c                   ENDDO
011200140507     c* fiapd40f
011300140507     c                   READ      Fiapd40f                               22
011400140507    1c     *IN22         DOWEQ     *OFF
011500140507     c
011600140507     c                   movel     apd4pdr       wlinea            3
011700140507     c     wlinea        lookup    skpo                                   35
011800140507     C
011900140507    2c                   if        *in35
012000140507     c     kapd4         setll     fiaPD4new                              31
012100140507    3c                   if        not *in31
012200140507     c                   write     apd4NEW                              61
012300140507     c                   add       1             Totapd4           7 0
012400140507    3c                   endif
012500140507    2c                   endif
012600140507      *
012700140507     c                   READ      Fiapd30f                               22
012800140507    1c                   ENDDO
012900070725     c* fiapd50f
013000070725     c                   READ      Fiapd50f                               22
013100070725    1c     *IN22         DOWEQ     *OFF
013200070725     c
013300070725     c                   movel     apd5pdr       wlinea            3
013400070725     c     wlinea        lookup    skpo                                   35
013500070725     C
013600070725    2c                   if        *in35
013700070725     c     kapd5         setll     fiaPD5new                              31
013800070725    3c                   if        not *in31
013900070725     c                   write     apd5NEW                              61
014000070725     c                   add       1             Totapd5           7 0
014100070725    3c                   endif
014200070725    2c                   endif
014300070725      *
014400070725     c                   READ      Fiapd50f                               22
014500070725    1c                   ENDDO
014600021010      *
014700040805     c                   EXCEPT    RINUM1
014800140507      * aggiunta autisti test PDA per as888
014900140507     c                   clear                   apdnew
015000140507     c                   eval      apdtip = 'A'
015100140507     c                   eval      apdpdr = 0479500
015200140508     c                   eval      apdrsc = '9500 TEST PDA'
015300140508     c                   eval      apdrsF = '9500 TEST PDA'
015400140507     c                   eval      APDETM = 'N'
015500140507     c                   eval      APDETP = 'N'
015600140507     c                   eval      APDPKM = 'N'
015700140507     c                   eval      APDPKP = 'N'
015800140507     c                   eval      APDCAM = 'S'
015900140507     c                   eval      APDCAP = 'S'
016000140507     c                   eval      APDSCM = 'S'
016100140507     c                   eval      APDSCP = 'S'
016200140507     c                   eval      APDFLR = '00 000 E0000000'
016300140507     c                   write     apdNEW
016400140507     c                   eval      apdpdr = 0479501
016500140508     c                   eval      apdrsc = '9501 TEST PDA'
016600140508     c                   eval      apdrsF = '9501 TEST PDA'
016700140507     c                   write     apdNEW
016800140507     c                   eval      apdpdr = 0479502
016900140508     c                   eval      apdrsc = '9502 TEST PDA'
017000140508     c                   eval      apdrsF = '9502 TEST PDA'
017100140507     c                   write     apdNEW
017200021011     c                   seton                                        LR
017300021010      *
017400021010      *---------------------------------------------------------------*
017500040805     oQSYSPRT   E            RINUM1         2 02
017600140507     o                                         +  1 'AUTOTRASPORTATORI'
017700140507     o                                         +  1 'Fits20R'
017800021010     o                       UDATE             +  2 '  /  /  '
017900021010     o          E            RINUM1      1
018000140507     o                                         +  1 'TOT.FIAPD00F:'
018100140507     o                       TOTAPD        2   +  1
018200140507     o                                         +  1 'TOT.FIAPD20F:'
018300140507     o                       TOTAPD2       2   +  1
018400140507     o                                         +  1 'TOT.FIAPD30F:'
018500140507     o                       TOTAPD3       2   +  1
018600140507     o                                         +  1 'TOT.FIAPD40F:'
018700140507     o                       TOTAPD4       2   +  1
018800140507     o                                         +  1 'TOT.FIAPD50F:'
018900140507     o                       TOTAPD5       2   +  1
