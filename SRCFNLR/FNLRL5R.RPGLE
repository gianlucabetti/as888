000100941109     H*------------------------------------------------------------------------*
000200070522     H* Azione schedulata scambio dati con TELLUS                              *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600070522     fazorg01l  if   e           k disk
000700111124     ffilte03l  if   e           k disk
000800060828     d
000900070523     D liv2            s              3  0 dim(500)
001000111124     D CMdly           S             20    DIM(1) CTDATA PERRCD(1)
001100070523     d
001200070522     D KPJBA         E DS
001300070522     D og148         E DS
001400070522     D FNLV55DS      E DS
001500111124     D tibs02ds      E DS
001600111124     D DVPOtellus    E DS
001700070917     d
001800070917      * DATA E ORA SCHEDULAZIONE PER KPJBA
001900070917     D                 DS
002000070917     d* Data in ggmmaa
002100070917     D  D1DATS                 1      6  0
002200070917     D  D1ORAS                 7     10  0
002300070917     D  SCHEDS                 1     10  0
002400070522     d
002500070124     d* DS ricevuta: p.o. in gestione da elaborare
002600060831     d FNLRL1DS        ds
002700060831     d  ILRL1lna                      3s 0
002800070406     d  ILRL1dat                      8s 0
002900070618     d  ILRL1dis                      1a   inz
003000070618     d  ILRL1tst                      1a   inz
003100070522     d  ILRL1senGiro                  1a
003200070618     d  LRL1contrBL                   1a   inz
003300070618     d  LRL1botlls                    2s 0 inz
003400060831     d
003500070522     d Dataiso         s               d   datfmt(*iso)
003600070917     d Datadmy         s               d   datfmt(*dmy)
003700070917     d Oraiso          s               t   timfmt(*iso)
003800070917     d worasked        s               z
003900941109     C*------------------------------------------------------------------------*
004000070522     C     *ENTRY        PLIST
004100070522     C                   PARM                    KPJBA
004200070522     c* S=elabora solo bolle senza giro
004300070522     C                   PARM                    lrl5SenGiro       1
004400070522     C                   PARM                    lrl5ggela         1 0
004500071023     C                   PARM                    Tipofil           1
004600070522     c
004700070522     c                   clear                   fnlrl1ds
004800101116     c                   clear                   wnumfil           1
004900111124     c                   clear                   Primasott         1
005000071023     c                   if        %parms=4
005100101116     c* Se tipo fil='P' --> sottometto 2 livello numeri pari
005200101116     c* Se tipo fil='D' --> sottometto 2 livello numeri Dispari
005300101116     c                   select
005400101116     c                   when      tipofil='D'
005500101116     c                   eval      wtipofil='2'
005600101116     c                   eval      wnumfil='D'
005700101116     c
005800101116     c                   when      tipofil='P'
005900101116     c                   eval      wtipofil='2'
006000101116     c                   eval      wnumfil='P'
006100101116     c
006200101116     c                   other
006300071023     c                   eval      wtipofil=tipofil
006400101116     c                   endsl
006500101116     c
006600071023     c                   else
006700071023     c                   clear                   wtipofil          1
006800071023     c                   endif
006900101116     c
007000070522     c*
007100070523     C* PARAMETRO --> DATA DI ELABORAZIONE GIRI DI CONSEGNA
007200070522     c                   time                    w0140            14 0
007300070522     c
007400070522     C                   MOVE      W0140         WDTGIO            8 0
007500070917     c*
007600070522     c     *eur          movel     wdtgio        dataiso
007700070522    1c                   if        lrl5ggela=0
007800070522     c                   movel     dataiso       ilrl1dat
007900070522    1c                   endif
008000070522     c
008100070522    1c                   if        lrl5ggela>0
008200070522     c* Elaboro sempre alla data + gg passati
008300070522     c     dataiso       adddur    lrl5ggela:*d  daTAISO
008400070522     c                   movel     dataiso       ilrl1dat
008500070522    1c                   endif
008600070523     C
008700070523     c* PARAMETRO --> BOLLE SENZA GIRO
008800070523     c                   eval      ilrl1SenGiro=lrl5SenGiro
008900070522     c
009000070523     C* PARAMETRO --> LINEA DA ELABORARE
009100070523     c                   z-add     0             x                 3 0
009200070523     c* Elaboro prima i terminal di arrivo e  poi i secondi livello
009300071023     c*  Se richiesti tutti, o solo il richiesto
009400070522     c     *loval        setll     azorg01l
009500070522     c                   read      azorg01l
009600070522    1c                   dow       not %eof(azorg01l)
009700070522     c                   movel     orgde8        og148
009800070529    2c                   if        §ogtlls='S'
009900070522     c* controllo che non sia in £6
010000070522     c                   clear                   fnlv55ds
010100070522     c                   eval      d55tpt='6'
010200070522     c                   eval      d55lin=orgfil
010300070522     c                   eval      d55drf=ilrl1dat
010400070522     c                   call      'FNLV55R'
010500070522     c                   parm                    fnlv55ds
010600070522     c
010700070522    3c                   if        d55tfa=d55lin
010800070523     c
010900071023     c*
011000070523     c                   clear                   fnlv55ds
011100070523     c                   eval      d55tpt='A'
011200070523     c                   eval      d55lin=orgfil
011300070523     c                   eval      d55drf=ilrl1dat
011400070523     c                   call      'FNLV55R'
011500070523     c                   parm                    fnlv55ds
011600071023     c
011700071023     c* Verifico il tipo filiale primo o secondo livello se richiesto
011800071023    4c                   if        wtipofil=' ' or
011900071023     c                             (wtipofil='1' and d55tfa=d55lin) or
012000071023     c                             (wtipofil='2' and d55tfa<>d55lin)
012100071023     c
012200070523     c
012300071023    5c                   if        wtipofil=' ' and d55tfa<>d55lin
012400071023     c* elaboro i secondi livello dopo solo se lancio complessivo
012500070523     c                   add       1             x
012600070523     c                   eval      liv2(x)=orgfil
012700071023     c
012800071023   x5c                   else
012900101116     c* Se Wnunfil è pieno sottometto solo se giusto
013000101116     c                   clear                   wresto
013100101116     c                   clear                   wdiffe
013200101116    6c                   if        wnumfil<>' '
013300101116     c     orgfil        div       2             wdiffe            3 0
013400101116     c                   mvr                     wresto            2 0
013500101116    6c                   endif
013600070522     c
013700101116    6c                   if        wnumfil=' ' or (wnumfil='P' and wresto=0) or
013800101116     c                             (wnumfil='D' and wresto>0)
013900111124     c* Verifico quanti lavori ci sono in coda --> se più di xx ( da tabella)
014000111124     c*  attendo
014100111124     c                   exsr      CtrFILTE
014200111124     c
014300070523     c* Sottometto azione di elaborazione TELLUS
014400070522     c                   eval      ilrl1lna=orgfil
014500070522     c                   movel     fnlrl1ds      kpjbu
014600070621     c                   eval      %subst(kbuff:1:10)='          '
014700111124     c                   clear                   kqebi
014800070522     c                   movel     'LRL1'        kcoaz
014900070522     c                   call      'BCH10'
015000070522     c                   parm                    kpjba
015100111124     c
015200111124     c* eseguita la prima sottomissione
015300111124     c                   eval      Primasott='S'
015400101116    6c                   endif
015500070522     c
015600071023    5c                   endif
015700071023    4c                   endif
015800071023    3c                   endif
015900071023    2c                   endif
016000070523     c
016100070522     c                   read      azorg01l
016200070522    1c                   enddo
016300070523     c
016400070523     c* finita la sottomissione elaborazione dei terminal di arrivo
016500071023     c*  sottometto i secondi livello solo se richieste tutte le filiali
016600070523     c                   z-add     1             x
016700070523     c                   dow       x<=500 and liv2(x)>0
016800070523     c
016900070523     c* Sottometto azione di elaborazione TELLUS
017000070523     c                   eval      ilrl1lna=liv2(x)
017100070917     c* schedulata 10 minuti più avanti
017200070620     c                   eval      %subst(kbuff:1:10)='          '
017300070917     c                   time                    worasked
017400070917     c
017500070917     c                   adddur    10:*mn        worasked
017600070917     c                   movel     worasked      dataiso
017700070917     c                   movel     dataiso       datadmy
017800070917     c                   movel     datadmy       d1dats
017900070917     c
018000070917     c                   move      worasked      oraiso
018100070917     c                   movel     oraiso        w0060             6 0
018200070917     c                   movel     w0060         d1oras
018300070917     c                   movel     scheds        kbuff
018400070917     c
018500070523     c                   movel     fnlrl1ds      kpjbu
018600111124     c                   eval      kqebi='KSDIFFE   '
018700070523     c                   movel     'LRL1'        kcoaz
018800070523     c                   call      'BCH10'
018900070523     c                   parm                    kpjba
019000070523     c
019100070523     c                   add       1             x
019200070523    3c                   enddo
019300070522     c
019400070522     c                   SETON                                        LR
019500111124     c* --------------------------------------------------------------------
019600111124     c     Ctrfilte      BEGSR
019700111124     c                   z-add     999           contae
019800111124     c
019900111124     c* Carico la DVPOTELLUS
020000111124     C                   CLEAR                   tibs02ds
020100111124     c                   clear                   dvpotellus
020200111124     C                   MOVEL     'C'           T02MOD
020300111124     C                   MOVEL     KNSIF         T02SIF
020400111124     C                   MOVEL     'VPO'         T02COD
020500111124     c                   movel(P)  'TELLUS'      t02ke1
020600111124     C                   CALL      'TIBS02R'
020700111124     C                   PARM                    KPJBA
020800111124     C                   PARM                    tibs02ds
020900111124     c
021000111124    2C                   IF        T02ERR = *BLANKS
021100111124     C                   MOVEL     T02uni        dvpotellus
021200111124     c                   else
021300111124     c                   eval      §vpoatjob=19
021400111124     c                   eval      §vpoatbe1=015
021500111124     c                   eval      §vpoatbe2=003
021600111124     c                   eval      §vpoataf1=180
021700111124     c                   eval      §vpoataf2=090
021800111124    2c                   endif
021900111124     c
022000111124     c* Attendo  dei secondi  e poi eseguo in controllo
022100111124     c                   if        PrimaSott='S'
022200111124     c                   clear                   comman           80
022300111124     C                   Z-ADD     20            LUNG             15 5
022400111124     c
022500111124     C                   MOVEA     CMdly(1)      COMMAN
022600111124     c                   if        tipofil='1'
022700111124     c                   eval      %subst(comman:12:3)=%editc(§vpoatbe1:'X')
022800111124     c                   else
022900111124     c                   eval      %subst(comman:12:3)=%editc(§vpoatbe2:'X')
023000111124     c                   endif
023100111124     c
023200111124     C                   CALL      'QCMDEXC'
023300111124     C                   PARM                    COMMAN
023400111124     C                   PARM                    LUNG
023500111124     c                   endif
023600111124     c
023700111124    1c                   dow       contaE>§vpoatjob and §vpoatjob<999
023800111124     c                   clear                   contaE            7 0
023900111124     c
024000111124     c* Se = 999 --> esco dal ciclo ed elaboro
024100111124    2c                   if        §vpoatjob < 999
024200111124     c
024300111124     c     *loval        setll     filte03l
024400111124     c                   read      filte03l
024500111124    3c                   dow       not %eof(filte03l)
024600111124     c                   eval      contaE=contaE+1
024700111124     c                   read      filte03l
024800111124    3c                   enddo
024900111124     c
025000111124    3c                   if        contaE>§vpoatjob
025100111124     c                   clear                   comman           80
025200111124     C                   Z-ADD     20            LUNG             15 5
025300111124     C                   MOVEA     CMdly(1)      COMMAN
025400111124     c                   if        tipofil='1'
025500111124     c                   eval      %subst(comman:12:3)=%editc(§vpoataf1:'X')
025600111124     c                   else
025700111124     c                   eval      %subst(comman:12:3)=%editc(§vpoataf2:'X')
025800111124     c                   endif
025900111124     c
026000111124     C                   CALL      'QCMDEXC'
026100111124     C                   PARM                    COMMAN
026200111124     C                   PARM                    LUNG
026300111124     c
026400111124     c* Carico la DVPOTELLUS
026500111124     C                   CLEAR                   tibs02ds
026600111124     c                   clear                   dvpotellus
026700111124     C                   MOVEL     'C'           T02MOD
026800111124     C                   MOVEL     KNSIF         T02SIF
026900111124     C                   MOVEL     'VPO'         T02COD
027000111124     c                   movel(P)  'TELLUS'      t02ke1
027100111124     C                   CALL      'TIBS02R'
027200111124     C                   PARM                    KPJBA
027300111124     C                   PARM                    tibs02ds
027400111124     c
027500111124    2C                   IF        T02ERR = *BLANKS
027600111124     C                   MOVEL     T02uni        dvpotellus
027700111124     c                   else
027800111124     c                   eval      §vpoatjob=19
027900111124     c                   eval      §vpoatbe1=015
028000111124     c                   eval      §vpoatbe2=003
028100111124     c                   eval      §vpoataf1=180
028200111124     c                   eval      §vpoataf2=090
028300111124    2c                   endif
028400111124    3c                   endif
028500111124     c
028600111124    2c                   endif
028700111124    1c                   enddo
028800111124     c
028900111124     c                   ENDSR
029000111124**
029100111124DLYJOB DLY(005)
