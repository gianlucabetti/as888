000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040909      * FITS14R  *----------------------------------------------------*
000400090916      *  STATISTICHE PARTENZA: fISPA/FISAT/FNSCE/FISBO/FNSCO
000500021010      *---------------------------------------------------------------* c
000600090916     fFISATNEW  If A e           k disk    rename(FISAT000:SATNEW)
000700090916     fFISAT04L  If   e           k disk
000800021007      *
000900090911     fFiSBONEW  If A e           k disk    rename(FiSBO000:SBONEW)
001000090911     fFiSBO01l  If   e           k disk
001100040909      *
001200090915     ffispaNEW  If A e           k disk    rename(fispa000:spaNEW)
001300090915     ffispa01l  If   e           k disk
001400040909      *
001500040909     fFnsceNEW  If A e           k disk    rename(FNSce000:sceNEW)
001600040909     fFnsce01l  If   e           k disk
001700040909      *
001800040909     fFnscoNEW  If A e           k disk    rename(FNSco000:scoNEW)
001900040909     fFnsco01l  If   e           k disk
002000040909      *
002100090929     fFisarNEW  If A e           k disk    rename(FiSar000:sarNEW)
002200090930     fFisar01l  If   e           k disk
002300040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002400021008      *
002500021010     d KPJBA         E DS
002600040805     D* Schiera p.o. da trasferire
002700040805     D  SKPO           s              3    dim(500)
002800040909     D  SKtfp          s              3  0 dim(500)
002900040909     d FNLV55ds      e ds                  inz
003000040909     d  D55tla       e                     inz(' ')
003100040909     d  D55tpt       e                     inz('P')
003200040909     d KLIN            s                   like(d55lin)
003300040909     d Ktfp            s                   like(d55tfp)
003400040909     d Ksos            s                   like(sbosos) inz(' ')
003500040805      *
003600021008      *****************************************************************
003700040909     C     kSBO          KLIST
003800040909     C                   KFLD                    ksos
003900040909     C                   KFLD                    ktfp
004000040909     C     ksce          KLIST
004100040909     C                   KFLD                    d55tfp
004200040909     C                   KFLD                    klin
004300040909      *****************************************************************
004400021010     c     *ENTRY        PLIST
004500021010     c                   PARM                    KPJBA
004600040805     C                   parm                    PARAM          1500
004700040805     c
004800040805     c     ' ':'0'       xlate     param         param
004900040805     c                   movea     param         skpo
005000021010      *---------------------------------------------------------------*
005100040909     C* CARICO TUTTI I TERMINAL DI PARTENZA DELLA SCHIERA DEI P.O.
005200040909     C*  ED elaboro tutti i dati dei terminal dei p.o.
005300040909     c                   z-add     1             x                 4 0
005400040909     c                   z-add     1             y                 4 0
005500040909    1c                   dow       skpo(x)>*zeros
005600040909     c                   movel     skpo(X)       klin
005700040909     c                   reset                   FNLV55DS
005800040909     c                   eval      D55lin = klin
005900040909     c                   z-add     *date         d55drf
006000040909     c                   call      'FNLV55R'
006100040909     c                   parm                    fnlv55ds
006200040909     c     d55tfp        lookup    sktfp                                  31
006300040909    2c                   if        not *in31
006400040909     c                   eval      sktfp(y)=d55tfp
006500040909     c                   add       1             y
006600040909    2c                   endif
006700040909     c
006800040909     c                   add       1             x
006900040909    1c                   enddo
007000040909      *
007100040909     c                   z-add     1             x                 4 0
007200040909    1c                   dow       sktfp(x)>*zeros
007300040909     c                   movel     sktfp(x)      ktfp
007400040909     c
007500040909     c* Se trovo almeno un record nel NEW non elaboro
007600090916     c     ktfp          setll     FISATnew                               90
007700040909     c                   if        not *in90
007800040909     C
007900090916      * STATISTICA A TERRA: FISAT
008000090916     c     ktfp          setll     FISAT04l
008100090916     c     ktfp          READe     FISAT04l                               22
008200040909    2c     *IN22         DOWEQ     *OFF
008300040909     c                   write     satnew                               99
008400040909     c                   add       1             totsat            7 0
008500040909     c
008600090916     c     ktfp          READe     FISAT04l                               22
008700040909    2c                   enddo
008800040909     c                   endif
008900040909     C
009000090911      * STATISTICA BOLLETTATO: FiSBO
009100040909     c                   do        2
009200040909     c* Se trovo almeno un record nel NEW non elaboro
009300090911     c     ksbo          setll     Fisbonew                               90
009400040909     c                   if        not *in90
009500040909     c
009600090911     c     ksbo          setll     Fisbo01l
009700090911     c     ksbo          READe     Fisbo01l                               22
009800040909    2c     *IN22         DOWEQ     *OFF
009900040909     c                   write     sbonew                               99
010000040909     c                   add       1             totsbo            7 0
010100040909     c
010200090911     c     ksbo          READe     Fisbo01l                               22
010300040909    2c                   enddo
010400040909     c                   endif
010500040909     c
010600040909     c                   eval      ksos='S'
010700040909    2c                   enddo
010800040909     C
010900090915      * STATISTICA partito: fispa
011000040909     c* Se trovo almeno un record nel NEW non elaboro
011100090915     c     ktfp          setll     fispanew                               90
011200040909     c                   if        not *in90
011300040909     c
011400090915     c     ktfp          setll     fispa01l
011500090915     c     ktfp          READe     fispa01l                               22
011600040909    2c     *IN22         DOWEQ     *OFF
011700040909     c                   write     spanew                               99
011800040909     c                   add       1             totspa            7 0
011900040909     c
012000090915     c     ktfp          READe     fispa01l                               22
012100040909    2c                   enddo
012200040909     c                   endif
012300040805     c
012400040909     c                   add       1             x
012500040909    1c                   enddo
012600021010      *
012700040909     c                   z-add     1             x                 4 0
012800040909    1c                   dow       skpo(x)>*zeros
012900040909     c                   movel     skpo(X)       klin
013000040909     c                   reset                   FNLV55DS
013100040909     c                   eval      D55lin = klin
013200040909     c                   z-add     *date         d55drf
013300040909     c                   call      'FNLV55R'
013400040909     c                   parm                    fnlv55ds
013500040909     c*
013600040909     c* STATISTICA CONTROLLO ENTRATA: elaboro solo la linea
013700040909     c     ksce          setll     fnsce01l
013800040909     c     ksce          reade     fnsce01l                               22
013900040909    2c                   dow       not *in22
014000040909     c                   write     scenew                               99
014100040909     c  n99              add       1             TOTSCE            7 0
014200040909     c
014300040909     c     ksce          READe     Fnsce01l                               22
014400040909    2c                   enddo
014500040909     c*
014600040909     c* STATISTICA CONSEGNE: elaboro solo la linea
014700040909     c     ksce          setll     fnsconew                               90
014800040909     c                   if        not *in90
014900040909     c
015000040909     c     ksce          setll     fnsco01l
015100040909     c     ksce          reade     fnsco01l                               22
015200040909    2c                   dow       not *in22
015300040909     c                   write     sconew                               99
015400040909     c  n99              add       1             TOTSCo            7 0
015500040909     c
015600040909     c     ksce          READe     Fnsco01l                               22
015700040909    2c                   enddo
015800040909     c                   endif
015900040909     c*
016000040909     c* STATISTICA ARRIVI: elaboro solo la linea
016100090930     c     klin          setll     fisar01l
016200090930     c     klin          reade     fisar01l                               22
016300040909    2c                   dow       not *in22
016400040909     c                   write     sarnew                               99
016500040909     c  n99              add       1             TOTSar            7 0
016600040909     c
016700090930     c     klin          READe     Fisar01l                               22
016800040909    2c                   enddo
016900040909     c
017000040909     c                   add       1             x
017100040909    1c                   enddo
017200040909     c* stampa totali
017300040805     c                   EXCEPT    RINUM1
017400040909     c
017500040909     c                   clear                   FNLV55DS
017600040909     c                   eval      D55tla = 'C'
017700040909     c                   call      'FNLV55R'
017800040909     c                   parm                    fnlv55ds
017900021011     c                   seton                                        LR
018000021009      *
018100021010      *---------------------------------------------------------------*
018200040805     oQSYSPRT   E            RINUM1         2 02
018300040909     o                                         +  5 'STATISTICHE PARTENZA'
018400040909     o                                         +  5 'Fits14R'
018500021010     o                       UDATE             +  2 '  /  /  '
018600021010     o          E            RINUM1      1
018700090916     o                                         +  3 'FISAT00F:'
018800040909     o                       TOTSAT        2   +  2
018900090915     o                                         +  3 'FISPA00F:'
019000040909     o                       TOTSPA        2   +  2
019100090911     o                                         +  3 'FISBO00F:'
019200040909     o                       TOTSBO        2   +  2
019300040909     o                                         +  3 'FNSCE00F:'
019400040909     o                       TOTSCE        2   +  2
019500040909     o                                         +  3 'FNSCO00F:'
019600040909     o                       TOTSCo        2   +  2
019700040909     o          E            RINUM1      2  2
019800090929     o                                         +  3 'FISAR00F:'
019900040909     o                       TOTSar        2   +  2
