000100130530     /*PRM  dbgview(*source)
000200130530     /*CMD  ovrdbf file(FLTR7new) tofile(FILTRAPRD/FLTR702L) +
000300130530     /*CMD         ovrscope(*calllvl)
000400161214     /*CMD  ovrdbf file(FNMSAnew) tofile(*libl/FNMSA03L) +
000500161214     /*CMD         ovrscope(*calllvl)
000600170210     /*CMD  ovrdbf file(FIMSPnew) tofile(*libl/FIMSP01L) +
000700170210     /*CMD         ovrscope(*calllvl)
000800130530     /*END  dltovr file(*all) lvl(*)
000900130530     /*END
001000040805     h decedit('0,') datedit(*ymd.)
001100021010      *
001200040916      * FITS27R  *----------------------------------------------------*
001300040916      *  Spunte da vdl fltr700f
001400021010      *---------------------------------------------------------------* c
001500040916     ffltr7new  O  A e           k disk    rename(FLTR7000:TR7NEW)
001600161214     fFNMSAnew  O  A e           k disk    rename(FNMSA000:msaNEW)
001700170210     fFIMSPnew  O  A e           k disk    rename(FIMSP000:MSPNEW)
001800070508      *
001900130530     ffltr702l  If   e           K disk
002000170303     fFNMSA03L  If   e           K disk
002100170210     fFIMSP01L  if   e           k disk
002200130530      *
002300040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002400021008      *
002500021010     d KPJBA         E DS
002600040805     D* Schiera p.o. da trasferire
002700040805     D  SKPO           s              3    dim(500)
002800040916     D  SKtot          s              3    dim(1500)
002900040916     d KLIN            s                   like(tr7fil)
003000040915     d FNLV55ds      e ds                  inz
003100040915     D                 DS
003200040805      *
003300021008      *****************************************************************
003400021010     c     *ENTRY        PLIST
003500021010     c                   PARM                    KPJBA
003600040805     C                   parm                    PARAM          1500
003700040805     c
003800040805     c     ' ':'0'       xlate     param         param
003900040805     c                   movea     param         skpo
004000040915      *---------------------------------------------------------------*
004100040915     c* Per ogni p.o. cerco :
004200040915     c*  - i suoi dati
004300040916     c*  - i dati del suo term.partenza se <>
004400040916     c*  - i dati del suo terminal di arrivo se <>
004500040915     c                   z-add     1             y                 4 0
004600040916     c                   z-add     1             x
004700040915     c
004800040915    1c                   dow       skpo(x)>*zeros
004900040915     c                   movel     skpo(X)       klin
005000040915     c     klin          lookup    sktot                                  35
005100040915     c                   if        not *in35
005200040915     c                   eval      sktot(y)=klin
005300040915     c                   add       1             y
005400040915     c                   endif
005500040915     c
005600040915     c                   clear                   FNLV55DS
005700040916     c                   movel     klin          D55lin
005800040915     c                   z-add     *date         d55drf
005900040915     c                   call      'FNLV55R'
006000040915     c                   parm                    fnlv55ds
006100040915     c
006200040916     c                   movel     d55tfp        klin
006300040916     c     klin          lookup    sktot                                  35
006400040915     c                   if        not *in35
006500040916     c                   movel     d55tfp        sktot(y)
006600040915     c                   add       1             y
006700040915     c                   endif
006800040916     c                   movel     d55tfa        klin
006900040916     c     klin          lookup    sktot                                  35
007000040915     c                   if        not *in35
007100040916     c                   movel     d55tfa        sktot(y)
007200040915     c                   add       1             y
007300040915     c                   endif
007400040915     c
007500040915     c                   add       1             x
007600040915     c                   enddo
007700040916
007800040916     c*** FLTR7
007900040916     c                   z-add     1             x                 4 0
008000040916     c
008100040916    1c                   dow       sktot(x)>*zeros
008200040916     c                   movel     sktot(X)      klin
008300130530     c     klin          setll     fltr7000
008400130530     c     klin          READe     fltr7000
008500130530    3c                   dow       not %eof(fltr702l)
008600040916     c                   write     tr7new                               99
008700040916     c  n99              add       1             tottr7            7 0
008800130530     c     klin          READe     fltr7000
008900040916     c                   enddo
009000040916     c
009100040916     c                   add       1             x
009200040916     c                   enddo
009300161214      *
009400161214      *** FNMSA00F
009500161214     c     *loval        setll     FNMSA000
009600161214     c                   read      FNMSA000
009700161214    3c                   DOW       not %eof(FNMSA03L)
009800170303     c                   movel     MSAfil        klin
009900161214     c     klin          lookup    sktot                                  35
010000161214     c                   if        *in35
010100161214     c                   write     msaNEW                               99
010200161214     c  n99              add       1             totmsa            7 0
010300161214     c                   endif
010400161214      *
010500161214     c                   read      FNMSA000
010600161214     c                   ENDDO
010700170210      *
010800170210      *** FIMSP00F
010900170210     c     *loval        setll     FIMSP000
011000170210     c                   read      FIMSP000
011100170210    3c                   DOW       not %eof(FIMSP01L)
011200170210     c                   movel     MSPfil        klin
011300170210     c     klin          lookup    sktot                                  35
011400170210     c                   if        *in35
011500170210     c                   write     MSPNEW                               99
011600170210     c  n99              add       1             totMSP            7 0
011700170210     c                   endif
011800170210      *
011900170210     c                   read      FIMSP000
012000170210     c                   ENDDO
012100070508      *
012200021010      *
012300040805     c                   EXCEPT    RINUM1
012400040915     c
012500040915     c                   clear                   FNLV55DS
012600040915     c                   eval      D55tla = 'C'
012700040915     c                   call      'FNLV55R'
012800040915     c                   parm                    fnlv55ds
012900021011     c                   seton                                        LR
013000021010      *
013100021010      *---------------------------------------------------------------*
013200040805     oQSYSPRT   E            RINUM1         2 02
013300040916     o                                         +  5 'spunte V.D.L.'
013400040916     o                                         +  5 'Fits27R'
013500021010     o                       UDATE             +  2 '  /  /  '
013600021010     o          E            RINUM1      1
013700040916     o                                         +  3 'FLTR700F:'
013800040916     o                       TOTTR7        2   +  2
013900161214     o                                         +  3 'FNMSA00F:'
014000161214     o                       TOTMSA        2   +  2
014100170210     o                                         +  3 'FIMSP00F:'
014200170210     o                       TOTMSP        2   +  2
