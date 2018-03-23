000100941109     H*------------------------------------------------------------------------*
000200070522     H* Azione schedulata scambio dati con TELLUS                              *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600070522     fazorg01l  if   e           k disk
000700060828     d
000800070523     D liv2            s              3  0 dim(500)
000900070523     d
001000070522     D KPJBA         E DS
001100070522     D og148         E DS
001200070522     D FNLV55DS      E DS
001300070917     d
001400070917      * DATA E ORA SCHEDULAZIONE PER KPJBA
001500070917     D                 DS
001600070917     d* Data in ggmmaa
001700070917     D  D1DATS                 1      6  0
001800070917     D  D1ORAS                 7     10  0
001900070917     D  SCHEDS                 1     10  0
002000070522     d
002100070124     d* DS ricevuta: p.o. in gestione da elaborare
002200060831     d FNLRL1DS        ds
002300060831     d  ILRL1lna                      3s 0
002400070406     d  ILRL1dat                      8s 0
002500070618     d  ILRL1dis                      1a   inz
002600070618     d  ILRL1tst                      1a   inz
002700070522     d  ILRL1senGiro                  1a
002800070618     d  LRL1contrBL                   1a   inz
002900070618     d  LRL1botlls                    2s 0 inz
003000060831     d
003100070522     d Dataiso         s               d   datfmt(*iso)
003200070917     d Datadmy         s               d   datfmt(*dmy)
003300070917     d Oraiso          s               t   timfmt(*iso)
003400070917     d worasked        s               z
003500941109     C*------------------------------------------------------------------------*
003600070522     C     *ENTRY        PLIST
003700070522     C                   PARM                    KPJBA
003800070522     c* S=elabora solo bolle senza giro
003900070522     C                   PARM                    lrl5SenGiro       1
004000070522     C                   PARM                    lrl5ggela         1 0
004100071023     C                   PARM                    Tipofil           1
004200070522     c
004300070522     c                   clear                   fnlrl1ds
004400071023     c                   if        %parms=4
004500071023     c                   eval      wtipofil=tipofil
004600071023     c                   else
004700071023     c                   clear                   wtipofil          1
004800071023     c                   endif
004900070522     c*
005000070523     C* PARAMETRO --> DATA DI ELABORAZIONE GIRI DI CONSEGNA
005100070522     c                   time                    w0140            14 0
005200070522     c
005300070522     C                   MOVE      W0140         WDTGIO            8 0
005400070917     c*
005500070522     c     *eur          movel     wdtgio        dataiso
005600070522    1c                   if        lrl5ggela=0
005700070522     c                   movel     dataiso       ilrl1dat
005800070522    1c                   endif
005900070522     c
006000070522    1c                   if        lrl5ggela>0
006100070522     c* Elaboro sempre alla data + gg passati
006200070522     c     dataiso       adddur    lrl5ggela:*d  daTAISO
006300070522     c                   movel     dataiso       ilrl1dat
006400070522    1c                   endif
006500070523     C
006600070523     c* PARAMETRO --> BOLLE SENZA GIRO
006700070523     c                   eval      ilrl1SenGiro=lrl5SenGiro
006800070522     c
006900070523     C* PARAMETRO --> LINEA DA ELABORARE
007000070523     c                   z-add     0             x                 3 0
007100070523     c* Elaboro prima i terminal di arrivo e  poi i secondi livello
007200071023     c*  Se richiesti tutti, o solo il richiesto
007300070522     c     *loval        setll     azorg01l
007400070522     c                   read      azorg01l
007500070522    1c                   dow       not %eof(azorg01l)
007600070522     c                   movel     orgde8        og148
007700070529    2c                   if        §ogtlls='S'
007800070522     c* controllo che non sia in £6
007900070522     c                   clear                   fnlv55ds
008000070522     c                   eval      d55tpt='6'
008100070522     c                   eval      d55lin=orgfil
008200070522     c                   eval      d55drf=ilrl1dat
008300070522     c                   call      'FNLV55R'
008400070522     c                   parm                    fnlv55ds
008500070522     c
008600070522    3c                   if        d55tfa=d55lin
008700070523     c
008800071023     c*
008900070523     c                   clear                   fnlv55ds
009000070523     c                   eval      d55tpt='A'
009100070523     c                   eval      d55lin=orgfil
009200070523     c                   eval      d55drf=ilrl1dat
009300070523     c                   call      'FNLV55R'
009400070523     c                   parm                    fnlv55ds
009500071023     c
009600071023     c* Verifico il tipo filiale primo o secondo livello se richiesto
009700071023    4c                   if        wtipofil=' ' or
009800071023     c                             (wtipofil='1' and d55tfa=d55lin) or
009900071023     c                             (wtipofil='2' and d55tfa<>d55lin)
010000071023     c
010100070523     c
010200071023    5c                   if        wtipofil=' ' and d55tfa<>d55lin
010300071023     c* elaboro i secondi livello dopo solo se lancio complessivo
010400070523     c                   add       1             x
010500070523     c                   eval      liv2(x)=orgfil
010600071023     c
010700071023   x5c                   else
010800070522     c
010900070523     c* Sottometto azione di elaborazione TELLUS
011000070522     c                   eval      ilrl1lna=orgfil
011100070522     c                   movel     fnlrl1ds      kpjbu
011200070621     c                   eval      %subst(kbuff:1:10)='          '
011300070522     c                   movel     'LRL1'        kcoaz
011400070522     c                   call      'BCH10'
011500070522     c                   parm                    kpjba
011600070522     c
011700071023    5c                   endif
011800071023    4c                   endif
011900071023    3c                   endif
012000071023    2c                   endif
012100070523     c
012200070522     c                   read      azorg01l
012300070522    1c                   enddo
012400070523     c
012500070523     c* finita la sottomissione elaborazione dei terminal di arrivo
012600071023     c*  sottometto i secondi livello solo se richieste tutte le filiali
012700070523     c                   z-add     1             x
012800070523     c                   dow       x<=500 and liv2(x)>0
012900070523     c
013000070523     c* Sottometto azione di elaborazione TELLUS
013100070523     c                   eval      ilrl1lna=liv2(x)
013200070917     c* schedulata 10 minuti più avanti
013300070620     c                   eval      %subst(kbuff:1:10)='          '
013400070917     c                   time                    worasked
013500070917     c
013600070917     c                   adddur    10:*mn        worasked
013700070917     c                   movel     worasked      dataiso
013800070917     c                   movel     dataiso       datadmy
013900070917     c                   movel     datadmy       d1dats
014000070917     c
014100070917     c                   move      worasked      oraiso
014200070917     c                   movel     oraiso        w0060             6 0
014300070917     c                   movel     w0060         d1oras
014400070917     c                   movel     scheds        kbuff
014500070917     c
014600070523     c                   movel     fnlrl1ds      kpjbu
014700070523     c                   movel     'LRL1'        kcoaz
014800070523     c                   call      'BCH10'
014900070523     c                   parm                    kpjba
015000070523     c
015100070523     c                   add       1             x
015200070523    3c                   enddo
015300070522     c
015400070522     c                   SETON                                        LR
