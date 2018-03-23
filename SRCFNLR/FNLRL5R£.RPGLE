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
004400101116     c                   clear                   wnumfil           1
004500071023     c                   if        %parms=4
004600101116     c* Se tipo fil='P' --> sottometto 2 livello numeri pari
004700101116     c* Se tipo fil='D' --> sottometto 2 livello numeri Dispari
004800101116     c                   select
004900101116     c                   when      tipofil='D'
005000101116     c                   eval      wtipofil='2'
005100101116     c                   eval      wnumfil='D'
005200101116     c
005300101116     c                   when      tipofil='P'
005400101116     c                   eval      wtipofil='2'
005500101116     c                   eval      wnumfil='P'
005600101116     c
005700101116     c                   other
005800071023     c                   eval      wtipofil=tipofil
005900101116     c                   endsl
006000101116     c
006100071023     c                   else
006200071023     c                   clear                   wtipofil          1
006300071023     c                   endif
006400101116     c
006500070522     c*
006600070523     C* PARAMETRO --> DATA DI ELABORAZIONE GIRI DI CONSEGNA
006700070522     c                   time                    w0140            14 0
006800070522     c
006900070522     C                   MOVE      W0140         WDTGIO            8 0
007000070917     c*
007100070522     c     *eur          movel     wdtgio        dataiso
007200070522    1c                   if        lrl5ggela=0
007300070522     c                   movel     dataiso       ilrl1dat
007400070522    1c                   endif
007500070522     c
007600070522    1c                   if        lrl5ggela>0
007700070522     c* Elaboro sempre alla data + gg passati
007800070522     c     dataiso       adddur    lrl5ggela:*d  daTAISO
007900070522     c                   movel     dataiso       ilrl1dat
008000070522    1c                   endif
008100070523     C
008200070523     c* PARAMETRO --> BOLLE SENZA GIRO
008300070523     c                   eval      ilrl1SenGiro=lrl5SenGiro
008400070522     c
008500070523     C* PARAMETRO --> LINEA DA ELABORARE
008600070523     c                   z-add     0             x                 3 0
008700070523     c* Elaboro prima i terminal di arrivo e  poi i secondi livello
008800071023     c*  Se richiesti tutti, o solo il richiesto
008900070522     c     *loval        setll     azorg01l
009000070522     c                   read      azorg01l
009100070522    1c                   dow       not %eof(azorg01l)
009200070522     c                   movel     orgde8        og148
009300070529    2c                   if        §ogtlls='S'
009400070522     c* controllo che non sia in £6
009500070522     c                   clear                   fnlv55ds
009600070522     c                   eval      d55tpt='6'
009700070522     c                   eval      d55lin=orgfil
009800070522     c                   eval      d55drf=ilrl1dat
009900070522     c                   call      'FNLV55R'
010000070522     c                   parm                    fnlv55ds
010100070522     c
010200070522    3c                   if        d55tfa=d55lin
010300070523     c
010400071023     c*
010500070523     c                   clear                   fnlv55ds
010600070523     c                   eval      d55tpt='A'
010700070523     c                   eval      d55lin=orgfil
010800070523     c                   eval      d55drf=ilrl1dat
010900070523     c                   call      'FNLV55R'
011000070523     c                   parm                    fnlv55ds
011100071023     c
011200071023     c* Verifico il tipo filiale primo o secondo livello se richiesto
011300071023    4c                   if        wtipofil=' ' or
011400071023     c                             (wtipofil='1' and d55tfa=d55lin) or
011500071023     c                             (wtipofil='2' and d55tfa<>d55lin)
011600071023     c
011700070523     c
011800071023    5c                   if        wtipofil=' ' and d55tfa<>d55lin
011900071023     c* elaboro i secondi livello dopo solo se lancio complessivo
012000070523     c                   add       1             x
012100070523     c                   eval      liv2(x)=orgfil
012200071023     c
012300071023   x5c                   else
012400101116     c* Se Wnunfil è pieno sottometto solo se giusto
012500101116     c                   clear                   wresto
012600101116     c                   clear                   wdiffe
012700101116    6c                   if        wnumfil<>' '
012800101116     c     orgfil        div       2             wdiffe            3 0
012900101116     c                   mvr                     wresto            2 0
013000101116    6c                   endif
013100070522     c
013200101116    6c                   if        wnumfil=' ' or (wnumfil='P' and wresto=0) or
013300101116     c                             (wnumfil='D' and wresto>0)
013400070523     c* Sottometto azione di elaborazione TELLUS
013500070522     c                   eval      ilrl1lna=orgfil
013600070522     c                   movel     fnlrl1ds      kpjbu
013700070621     c                   eval      %subst(kbuff:1:10)='          '
013800070522     c                   movel     'LRL1'        kcoaz
013900070522     c                   call      'BCH10'
014000070522     c                   parm                    kpjba
014100101116    6c                   endif
014200070522     c
014300071023    5c                   endif
014400071023    4c                   endif
014500071023    3c                   endif
014600071023    2c                   endif
014700070523     c
014800070522     c                   read      azorg01l
014900070522    1c                   enddo
015000070523     c
015100070523     c* finita la sottomissione elaborazione dei terminal di arrivo
015200071023     c*  sottometto i secondi livello solo se richieste tutte le filiali
015300070523     c                   z-add     1             x
015400070523     c                   dow       x<=500 and liv2(x)>0
015500070523     c
015600070523     c* Sottometto azione di elaborazione TELLUS
015700070523     c                   eval      ilrl1lna=liv2(x)
015800070917     c* schedulata 10 minuti più avanti
015900070620     c                   eval      %subst(kbuff:1:10)='          '
016000070917     c                   time                    worasked
016100070917     c
016200070917     c                   adddur    10:*mn        worasked
016300070917     c                   movel     worasked      dataiso
016400070917     c                   movel     dataiso       datadmy
016500070917     c                   movel     datadmy       d1dats
016600070917     c
016700070917     c                   move      worasked      oraiso
016800070917     c                   movel     oraiso        w0060             6 0
016900070917     c                   movel     w0060         d1oras
017000070917     c                   movel     scheds        kbuff
017100070917     c
017200070523     c                   movel     fnlrl1ds      kpjbu
017300070523     c                   movel     'LRL1'        kcoaz
017400070523     c                   call      'BCH10'
017500070523     c                   parm                    kpjba
017600070523     c
017700070523     c                   add       1             x
017800070523    3c                   enddo
017900070522     c
018000070522     c                   SETON                                        LR
