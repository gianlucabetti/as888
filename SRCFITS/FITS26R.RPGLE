000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040915      * FITS26R  *----------------------------------------------------*
000400040915      *  Saldi filiale
000500021010      *---------------------------------------------------------------* c
000600040915     fsisdfNEW  If A e           k disk    rename(sisdf000:sdfNEW)
000700040915     fsisdRNEW  O  A e           k disk    rename(sisdR000:sdRNEW)
000800040915     fsisdtNEW  O  A e           k disk    rename(sisdt000:sdtNEW)
000900040915     F
001000040915     fSISDF00F  If   e             disk
001100040915     fSISDR01L  If   e           K disk
001200040915     fSISDR02L  If   e           K disk    rename(sisdR000:sISDR002)
001300040915     fSISDt03L  If   e           K disk
001400040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001500021008      *
001600021010     d KPJBA         E DS
001700040805     D* Schiera p.o. da trasferire
001800040805     D  SKPO           s              3    dim(500)
001900040915     D  SKtot          s              3  0 dim(1500)
002000040915     d DataMin         s              8  0
002100040915     d Dataoggi        s               d   datfmt(*iso)
002200040915     d KLIN            s                   like(sdrflp)
002300040915     d Kanno           s                   like(sdrann)
002400040915     d Kmesmin         s                   like(sdrmes)
002500040915     d Kmesmax         s                   like(sdrmes)
002600040915     d FNLV55ds      e ds                  inz
002700040915     D                 DS
002800040915     D  sdtann
002900040915     D  sdtmes
003000040915     D  sdtannomes             1      6  0
003100040805      *
003200021008      *****************************************************************
003300021010     c     *ENTRY        PLIST
003400021010     c                   PARM                    KPJBA
003500040805     C                   parm                    PARAM          1500
003600040805     c
003700040805     c     ' ':'0'       xlate     param         param
003800040805     c                   movea     param         skpo
003900040915     C
004000040915     C* PER IL FILE SISDR DEVO ELABORARE GLI ULTIMI 12 MESI
004100040915     c                   Move      *Date         Dataoggi
004200040915     c                   move      Dataoggi      DataMin
004300040915     c                   movel     DataMin       maxannomes        6 0
004400040915     c                   movel     DataMin       maxanno           4 0
004500040915     c
004600040915     c                   Subdur    1:*y          Dataoggi
004700040915     c                   move      Dataoggi      DataMin
004800040915     c                   movel     DataMin       minannomes        6 0
004900040915     c                   movel     DataMin       minanno           4 0
005000021010      *---------------------------------------------------------------*
005100040915     C     kSDf          KLIST
005200040915     C                   KFLD                    sdfann
005300040915     C                   KFLD                    sdfmes
005400040915     C                   KFLD                    sdffil
005500040915     C     kSDr          KLIST
005600040915     C                   KFLD                    kanno
005700040915     C                   KFLD                    klin
005800040915     C     kSDt          KLIST
005900040915     C                   KFLD                    kanno
006000040915     C                   KFLD                    kmesmin
006100040915      *---------------------------------------------------------------*
006200040915      **** SISDF
006300040915     c                   READ      sisdf00f                               22
006400040910    1c     *IN22         DOWEQ     *OFF
006500040913     C
006600040915     c                   movel     sdffil        wlinea            3
006700040915     c     wlinea        lookup    skpo                                   35
006800040915     C
006900040915    2c                   if        *in35
007000040915     c     ksdf          setll     sisdfnew                               31
007100040910    3c                   if        not *in31
007200040915     c                   write     sdfNEW                               61
007300040915     c                   add       1             Totsdf            7 0
007400040910    3c                   endif
007500040910    2c                   endif
007600021008      *
007700040915     c                   READ      sisdf00f                               22
007800040910    1c                   ENDDO
007900040915      **** SISDR
008000040915     c                   z-add     1             x                 4 0
008100040915     c
008200040915    1c                   dow       skpo(x)>*zeros
008300040915     c                   movel     skpo(X)       klin
008400040915     c                   eval      kanno=minanno
008500040915     c                   move      minannomes    kmesmin
008600040915     c                   move      12            kmesmax
008700040915     c                   clear                   wfine             1
008800040915     c
008900040915    2c                   dow       wfine=' '
009000040915     c
009100040915     c     ksdr          setll     sisdr01l
009200040915     c     ksdr          READe     sisdr01l
009300040915    3c                   dow       not %eof(sisdr01l)
009400040915    4c                   if        sdrmes<=kmesmax and sdrmes>=kmesmin
009500040915     c                   write     sdrnew                               99
009600040915     c  n99              add       1             totsdr            7 0
009700040915    4c                   endif
009800040915     c
009900040915     c     ksdr          READe     sisdr01l
010000040915    3c                   enddo
010100040915     c**
010200040915     c     ksdr          setll     sisdr02l
010300040915     c     ksdr          READe     sisdr02l
010400040915    3c                   dow       not %eof(sisdr02l)
010500040915    4c                   if        sdrmes<=kmesmax and sdrmes>=kmesmin
010600040915     c                   write     sdrnew                               99
010700040915     c  n99              add       1             totsdr            7 0
010800040915    4c                   endif
010900040915     c
011000040915     c     ksdr          READe     sisdr02l
011100040915    3c                   enddo
011200040915     c
011300040915    3c                   if        kanno<>maxanno
011400040915     c                   eval      kanno=maxanno
011500040915     c                   move      maxannomes    kmesmax
011600040915     c                   move      1             kmesmin
011700040915     c                   else
011800040915     c                   eval      wfine='S'
011900040915    3c                   endif
012000040915     c
012100040915    2c                   enddo
012200040915     c
012300040915     c                   add       1             x
012400040915    1c                   enddo
012500040915     c
012600040915      **** SISDT
012700040915     c* Per ogni p.o. cerco :
012800040915     c*  - i suoi dati
012900040915     c*  - i dati del suo term.partenza se <> (come tfp o transito)
013000040915     c*  - i dati del suo terminal di arrivo se <> (comne tfa)
013100040915     c                   z-add     1             y                 4 0
013200040916     c                   z-add     1             x
013300040915     c
013400040915    1c                   dow       skpo(x)>*zeros
013500040915     c                   movel     skpo(X)       klin
013600040915     c     klin          lookup    sktot                                  35
013700040915     c                   if        not *in35
013800040915     c                   eval      sktot(y)=klin
013900040915     c                   add       1             y
014000040915     c                   endif
014100040915     c
014200040915     c                   clear                   FNLV55DS
014300040915     c                   eval      D55lin = klin
014400040915     c                   z-add     *date         d55drf
014500040915     c                   call      'FNLV55R'
014600040915     c                   parm                    fnlv55ds
014700040915     c
014800040915     c     d55tfp        lookup    sktot                                  35
014900040915     c                   if        not *in35
015000040915     c                   eval      sktot(y)=d55tfp
015100040915     c                   add       1             y
015200040915     c                   endif
015300040915     c     d55tfa        lookup    sktot                                  35
015400040915     c                   if        not *in35
015500040915     c                   eval      sktot(y)=d55tfa
015600040915     c                   add       1             y
015700040915     c                   endif
015800040915     c
015900040915     c                   add       1             x
016000040915     c                   enddo
016100040915     c
016200040915     c                   eval      kanno=minanno
016300040915     c                   move      minannomes    kmesmin
016400040915     c
016500040915     c     ksdt          setll     sisdt03l
016600040915     c                   READ      sisdt03l
016700040915    1c                   dow       not %eof(sisdt03l) and
016800040915     c                             sdtannomes<=maxannomes
016900040915     c* verifico i p.o.
017000040915     c     sdttfp        lookup    sktot                                  35
017100040915     c                   if        not *in35
017200040915     c     sdttfa        lookup    sktot                                  35
017300040915     c                   if        not *in35
017400040915
017500040915     c                   if        sdttt1>0
017600040915     c     sdttt1        lookup    sktot                                  35
017700040915     c                   endif
017800040915     c                   if        not *in35
017900040915     c                   if        sdttt2>0
018000040915     c     sdttt2        lookup    sktot                                  35
018100040915     c                   endif
018200040915     c                   endif
018300040915     c                   if        not *in35
018400040915     c                   if        sdttt3>0
018500040915     c     sdttt3        lookup    sktot                                  35
018600040915     c                   endif
018700040915     c                   endif
018800040915     c                   if        not *in35
018900040915     c                   if        sdttt4>0
019000040915     c     sdttt4        lookup    sktot                                  35
019100040915     c                   endif
019200040915     c                   endif
019300040915     c                   if        not *in35
019400040915     c                   if        sdttt5>0
019500040915     c     sdttt5        lookup    sktot                                  35
019600040915     c                   endif
019700040915     c                   endif
019800040915     c
019900040915     c                   endif
020000040915     c                   endif
020100040915     c
020200040915     c                   if        *in35
020300040915     c                   write     sdtnew                               99
020400040915     c  n99              add       1             totsdt            7 0
020500040915     c                   endif
020600040915     c
020700040915     c                   READ      sisdt03l
020800040915    1c                   enddo
020900040915     c
021000021010      *
021100040805     c                   EXCEPT    RINUM1
021200040915     c
021300040915     c                   clear                   FNLV55DS
021400040915     c                   eval      D55tla = 'C'
021500040915     c                   call      'FNLV55R'
021600040915     c                   parm                    fnlv55ds
021700021011     c                   seton                                        LR
021800021010      *
021900021010      *---------------------------------------------------------------*
022000040805     oQSYSPRT   E            RINUM1         2 02
022100040915     o                                         +  5 'SALDI FILIALE'
022200040915     o                                         +  5 'Fits26R'
022300021010     o                       UDATE             +  2 '  /  /  '
022400021010     o          E            RINUM1      1
022500040915     o                                         +  3 'SISDF00F:'
022600040915     o                       TOTSDF        2   +  2
022700040915     o                                         +  3 'SISDT00F:'
022800040915     o                       TOTSDT        2   +  2
022900040915     o                                         +  3 'SISDR00F:'
023000040915     o                       TOTSDR        2   +  2
