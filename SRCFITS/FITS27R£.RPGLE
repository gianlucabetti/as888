000100130530     /*PRM  dbgview(*source)
000200130530     /*CMD  ovrdbf file(FLTR7new) tofile(FILTRAPRD/FLTR702L) +
000300130530     /*CMD         ovrscope(*calllvl)
000400130530     /*CMD  ovrdbf file(FNMSA1nw) tofile(FILTRAGRPF/FNMSA13L) +
000500130530     /*CMD         ovrscope(*calllvl)
000600161214     /*CMD  ovrdbf file(FNMSAnew) tofile(*libl/FNMSA03L) +
000700161214     /*CMD         ovrscope(*calllvl)
000800170210     /*CMD  ovrdbf file(FIMSPnew) tofile(*libl/FIMSP01L) +
000900170210     /*CMD         ovrscope(*calllvl)
001000130530     /*END  dltovr file(*all) lvl(*)
001100130530     /*END
001200040805     h decedit('0,') datedit(*ymd.)
001300021010      *
001400040916      * FITS27R  *----------------------------------------------------*
001500040916      *  Spunte da vdl fltr700f
001600021010      *---------------------------------------------------------------* c
001700040916     ffltr7new  O  A e           k disk    rename(FLTR7000:TR7NEW)
001800070508     fFNMSA1nw  O  A e           k disk    rename(FNMSA000:msa1NW)
001900161214     fFNMSAnew  O  A e           k disk    rename(FNMSA000:msaNEW)
002000161214     f                                     prefix(MSA0 : 3)
002100170210     fFIMSPnew  O  A e           k disk    rename(FIMSP000:MSPNEW)
002200070508      *
002300130530     ffltr702l  If   e           K disk
002400070726     ffnmsa13l  If   e           K disk    rename(FNMSA000:FNMSA100)
002500161214     ffnmsa03l  If   e           K disk
002600161214     f                                     prefix(MSA0 : 3)
002700170210     fFIMSP01L  if   e           k disk
002800130530      *
002900040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003000021008      *
003100021010     d KPJBA         E DS
003200040805     D* Schiera p.o. da trasferire
003300040805     D  SKPO           s              3    dim(500)
003400040916     D  SKtot          s              3    dim(1500)
003500040916     d KLIN            s                   like(tr7fil)
003600040915     d FNLV55ds      e ds                  inz
003700040915     D                 DS
003800040805      *
003900021008      *****************************************************************
004000021010     c     *ENTRY        PLIST
004100021010     c                   PARM                    KPJBA
004200040805     C                   parm                    PARAM          1500
004300040805     c
004400040805     c     ' ':'0'       xlate     param         param
004500040805     c                   movea     param         skpo
004600040915      *---------------------------------------------------------------*
004700040915     c* Per ogni p.o. cerco :
004800040915     c*  - i suoi dati
004900040916     c*  - i dati del suo term.partenza se <>
005000040916     c*  - i dati del suo terminal di arrivo se <>
005100040915     c                   z-add     1             y                 4 0
005200040916     c                   z-add     1             x
005300040915     c
005400040915    1c                   dow       skpo(x)>*zeros
005500040915     c                   movel     skpo(X)       klin
005600040915     c     klin          lookup    sktot                                  35
005700040915     c                   if        not *in35
005800040915     c                   eval      sktot(y)=klin
005900040915     c                   add       1             y
006000040915     c                   endif
006100040915     c
006200040915     c                   clear                   FNLV55DS
006300040916     c                   movel     klin          D55lin
006400040915     c                   z-add     *date         d55drf
006500040915     c                   call      'FNLV55R'
006600040915     c                   parm                    fnlv55ds
006700040915     c
006800040916     c                   movel     d55tfp        klin
006900040916     c     klin          lookup    sktot                                  35
007000040915     c                   if        not *in35
007100040916     c                   movel     d55tfp        sktot(y)
007200040915     c                   add       1             y
007300040915     c                   endif
007400040916     c                   movel     d55tfa        klin
007500040916     c     klin          lookup    sktot                                  35
007600040915     c                   if        not *in35
007700040916     c                   movel     d55tfa        sktot(y)
007800040915     c                   add       1             y
007900040915     c                   endif
008000040915     c
008100040915     c                   add       1             x
008200040915     c                   enddo
008300040916
008400040916     c*** FLTR7
008500040916     c                   z-add     1             x                 4 0
008600040916     c
008700040916    1c                   dow       sktot(x)>*zeros
008800040916     c                   movel     sktot(X)      klin
008900130530     c     klin          setll     fltr7000
009000130530     c     klin          READe     fltr7000
009100130530    3c                   dow       not %eof(fltr702l)
009200040916     c                   write     tr7new                               99
009300040916     c  n99              add       1             tottr7            7 0
009400130530     c     klin          READe     fltr7000
009500040916     c                   enddo
009600040916     c
009700040916     c                   add       1             x
009800040916     c                   enddo
009900070508      *
010000070508      *** FNMSA10F
010100070508     c     *loval        setll     FNMSA100
010200070508     c                   read      FNMSA100
010300070726    3c                   DOW       not %eof(FNMSA13L)
010400070508     c                   movel     msafil        klin
010500070508     c     klin          lookup    sktot                                  35
010600070508     c                   if        *in35
010700070508     c                   write     msa1NW                               99
010800070508     c  n99              add       1             totmsa1           7 0
010900070508     c                   endif
011000070508      *
011100070508     c                   read      FNMSA100
011200070508     c                   ENDDO
011300161214      *
011400161214      *** FNMSA00F
011500161214     c     *loval        setll     FNMSA000
011600161214     c                   read      FNMSA000
011700161214    3c                   DOW       not %eof(FNMSA03L)
011800161214     c                   movel     msa0fil       klin
011900161214     c     klin          lookup    sktot                                  35
012000161214     c                   if        *in35
012100161214     c                   write     msaNEW                               99
012200161214     c  n99              add       1             totmsa            7 0
012300161214     c                   endif
012400161214      *
012500161214     c                   read      FNMSA000
012600161214     c                   ENDDO
012700170210      *
012800170210      *** FIMSP00F
012900170210     c     *loval        setll     FIMSP000
013000170210     c                   read      FIMSP000
013100170210    3c                   DOW       not %eof(FIMSP01L)
013200170210     c                   movel     MSPfil        klin
013300170210     c     klin          lookup    sktot                                  35
013400170210     c                   if        *in35
013500170210     c                   write     MSPNEW                               99
013600170210     c  n99              add       1             totMSP            7 0
013700170210     c                   endif
013800170210      *
013900170210     c                   read      FIMSP000
014000170210     c                   ENDDO
014100070508      *
014200021010      *
014300040805     c                   EXCEPT    RINUM1
014400040915     c
014500040915     c                   clear                   FNLV55DS
014600040915     c                   eval      D55tla = 'C'
014700040915     c                   call      'FNLV55R'
014800040915     c                   parm                    fnlv55ds
014900021011     c                   seton                                        LR
015000021010      *
015100021010      *---------------------------------------------------------------*
015200040805     oQSYSPRT   E            RINUM1         2 02
015300040916     o                                         +  5 'spunte V.D.L.'
015400040916     o                                         +  5 'Fits27R'
015500021010     o                       UDATE             +  2 '  /  /  '
015600021010     o          E            RINUM1      1
015700040916     o                                         +  3 'FLTR700F:'
015800040916     o                       TOTTR7        2   +  2
015900070508     o                                         +  3 'FNMSA10F:'
016000070508     o                       TOTMSA1       2   +  2
016100161214     o                                         +  3 'FNMSA00F:'
016200161214     o                       TOTMSA        2   +  2
016300170210     o                                         +  3 'FIMSP00F:'
016400170210     o                       TOTMSP        2   +  2
