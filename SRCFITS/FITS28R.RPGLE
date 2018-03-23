000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040916      * FITS28R  *----------------------------------------------------*
000400040916      *  anomalie : fnanm00f
000500021010      *---------------------------------------------------------------* c
000600040916     ffnanmNEW  o  A e           k disk    rename(fnanm000:anmNEW)
000700040916     ffnanm00F  If   e             disk
000800040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900021008      *
001000021010     d KPJBA         E DS
001100040805     D* Schiera p.o. da trasferire
001200040805     D  SKPO           s              3    dim(500)
001300040916     D  SKTFA          s              3  0 dim(1000)
001400040916     D  SKTFP          s              3  0 dim(1000)
001500040916     d FNLV55ds      e ds                  inz
001600040916     d KLIN            s                   like(ANMLNP)
001700021008      *****************************************************************
001800021010     c     *ENTRY        PLIST
001900021010     c                   PARM                    KPJBA
002000040805     C                   parm                    PARAM          1500
002100040805     c
002200040805     c     ' ':'0'       xlate     param         param
002300040805     c                   movea     param         skpo
002400040916     C* CARICO DI TUTTI I P.O. IL TERMINAL DI ARRIVO
002500040916     c                   z-add     1             y                 4 0
002600040916     c                   z-add     1             x                 4 0
002700040916     c
002800040916    1c                   dow       skpo(x)>*zeros
002900040916     c                   movel     skpo(X)       klin
003000040916     c     klin          lookup    skTFA                                  35
003100040916     c                   if        not *in35
003200040916     c                   eval      skTFA(y)=klin
003300040916     c                   add       1             y
003400040916     c                   endif
003500040916     c
003600040916     c                   clear                   FNLV55DS
003700040916     c                   movel     klin          D55lin
003800040916     C                   MOVEL     'A'           D55TPT
003900040916     c                   z-add     *date         d55drf
004000040916     c                   call      'FNLV55R'
004100040916     c                   parm                    fnlv55ds
004200040916     c
004300040916     c     d55tfa        lookup    sktfa                                  35
004400040916     c                   if        not *in35
004500040916     c                   movel     d55tfa        sktfa(y)
004600040916     c                   add       1             y
004700040916     c                   endif
004800040916     c
004900040916     c                   add       1             x
005000040916     c                   enddo
005100040916     c
005200040916     C* CARICO DI TUTTI I terminal di partenza
005300040916     c                   z-add     1             y                 4 0
005400040916     c                   z-add     1             x
005500040916     c
005600040916    1c                   dow       skpo(x)>*zeros
005700040916     c                   movel     skpo(X)       klin
005800040916     c
005900040916     c                   clear                   FNLV55DS
006000040916     c                   movel     klin          D55lin
006100040916     C                   MOVEL     'P'           D55TPT
006200040916     c                   z-add     *date         d55drf
006300040916     c                   call      'FNLV55R'
006400040916     c                   parm                    fnlv55ds
006500040916     c
006600040916     c     d55tfp        lookup    sktfp                                  35
006700040916     c                   if        not *in35
006800040916     c                   movel     d55tfp        sktfp(y)
006900040916     c                   add       1             y
007000040916     c                   endif
007100040916     c
007200040916     c                   add       1             x
007300040916     c                   enddo
007400021010      *---------------------------------------------------------------*
007500040916     c                   READ      Fnanm00f                               22
007600040910    1c     *IN22         DOWEQ     *OFF
007700040916     c* anomalia 95: le prendo tutte
007800040916     c                   select
007900040916     c                   when      anmcaa=95
008000040916     c                   write     anmnew
008100040916     c                   add       1             totanm            7 0
008200040916     c* anomalia 90 senza foglio: scarico le mie o del mio terminal di
008300040916     c* arrivo in anmlna
008400040916     c                   when      anmcaa=90 and anmnfv=0
008500040916     c     anmlna        lookup    sktfa                                  35
008600040916     c                   if        *in35
008700040916     c                   write     anmnew
008800040916     c                   add       1             totanm
008900040916     c                   endif
009000040916     c
009100040916     c* anomalia 90 con   foglio: scarico le mie o del mio terminal di
009200040916     c* pertenza in anmcdu
009300040916     c                   when      anmcaa=90 and anmnfv>0
009400040916     c     anmcdu        lookup    sktfp                                  35
009500040916     c                   if        *in35
009600040916     c                   write     anmnew
009700040916     c                   add       1             totanm
009800040916     c                   endif
009900040916   x1c                   other
010000040805     c
010100040916     c                   setoff                                       35
010200040916    2c                   if        anmlnp>0
010300040916     c                   movel     anmlnp        wlinea            3
010400040916   x2c                   else
010500040916     c                   movel     anmfls        wlinea            3
010600040916    2c                   endif
010700040916     C                   IF        WLINEA>'000'
010800040805     c     wlinea        lookup    skpo                                   35
010900040916     C                   ENDIF
011000040916    2c                   if        not *in35
011100040916     c                   movel     anmlna        wlinea            3
011200040916     C                   IF        WLINEA>'000'
011300040916     c     wlinea        lookup    skpo                                   35
011400040916     c                   endif
011500040916    3c                   if        not *in35
011600040916     c                   movel     anmcdu        wlinea            3
011700040916     C                   IF        WLINEA>'000'
011800040916     c     wlinea        lookup    skpo                                   35
011900040916     c                   endif
012000040916    4c                   if        not *in35
012100040916     c                   movel     anmfle        wlinea            3
012200040916     C                   IF        WLINEA>'000'
012300040916     c     wlinea        lookup    skpo                                   35
012400040916     c                   endif
012500040916     c
012600040916    4c                   endif
012700040916    3c                   endif
012800040916    2c                   endif
012900040916     c
013000040910    2c                   if        *in35
013100040916     c                   write     anmNEW                               99
013200040916     c  n99              add       1             Totanm            7 0
013300040910    2c                   endif
013400040916     c                   endsl
013500021008      *
013600040916     c                   READ      Fnanm00f                               22
013700040910    1c                   ENDDO
013800021010      *
013900040805     c                   EXCEPT    RINUM1
014000021011     c                   seton                                        LR
014100021010      *
014200021010      *---------------------------------------------------------------*
014300040805     oQSYSPRT   E            RINUM1         2 02
014400040916     o                                         +  5 'ANOMLAIE:'
014500040916     o                                         +  5 'Fits28R'
014600021010     o                       UDATE             +  2 '  /  /  '
014700021010     o          E            RINUM1      1
014800040916     o                                         +  3 'FNANM00F:'
014900040916     o                       TOTANM        2   +  2
