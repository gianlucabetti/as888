000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200990527     H*PARMS CVTOPT(*NONE)
000300950711     H DECEDIT('0,') DATEDIT(*DMY.)
000400950711      *
000500990527      *                   CARICAMENTO ABI/CAB
000600990527      *                  ---------------------
000700950711      *
000800950711      *               MAPPA INDICATORI
000900950711      *
001000950711      *  09           NON EMETTE LA DATA SUL SFL SE = 0001-01-01
001100950711      *
001200950711      *  21           GENERICO OPERAZIONI I/O
001300950713      *  22           GENERICO OPERAZIONI I/O
001400950711      *
001500950711      *  30           SFLDSP
001600950711      * N31           SFLCLR
001700950711      *  31           SFLDSPCTL
001800950711      *  32           SFLNXTCHG
001900950711      *  33           SFLEND
002000950711      *  39           OF PRTF
002100950711      *
002200950711      *  Specificare l'uso dei singoli indicatori
002300950711      *  40 <---> 49  DSPATR ERRORI SU SFL
002400950711      *
002500950711      *  Specificare l'uso dei singoli indicatori
002600950711      *  50 <---> 98  ERRORI SU VIDEO
002700950711      *
002800950711      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002900950711      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003000950711      *  99           INDIC. GENERALE DI ERRORE
003100950711     F*----------------------------------------------------*
003200950711     FANAB800F  IF   E           K DISK
003300041012>>>>>FANAB900F  IF   E             DISK
003400980706     FANACT01L  UF A E           K DISK
003500980706     FANACR01L  UF A E           K DISK
003600950711    >FANA191P   O    E             PRINTER OFLIND(*IN39)
003700950711     D*----------------------------------------------------*
003800950711     D* Passaggio Parametri
003900950711     D KPJBA         E DS
004000950711     D D1180C                247    247
004100950711     D D1190C                248    248
004200950711     D*-------------
004300990527     D* Ds per suddivisione tracciato
004400950711    >Dab911DS        E DS                  EXTNAME(ANAB911DS)
004500950711    >Dab921DS        E DS                  EXTNAME(ANAB921DS)
004600950711    >Dab931DS        E DS                  EXTNAME(ANAB931DS)
004700950711     D*-------------
004800950711    >Dab811DS        E DS                  EXTNAME(ANAB811DS)
004900950711    >Dab821DS        E DS                  EXTNAME(ANAB821DS)
005000950711     D*-------------
005100980706     DDsAbi          E DS                  EXTNAME(ANACT00F)
005200980706     DDsCab          E DS                  EXTNAME(ANACR00F)
005300950711     D*-------------
005400950712     D$chiusa          C                   const('Banca/Agenzia chiusa    ')
005500950712     D$assorb          C                   const('Banca/Agenzia assorbita ')
005600950712     D$aggiun          C                   const('Banca/Agenzia aggiunta  ')
005700950712     D$aggior          C                   const('Banca/Agenzia aggiornata')
005800990527     D*-------------
005900980706     D* DS di appoggio per conversione
006000980706     D WDes            DS
006100980706     D WIst                    1     44A
006200980706     D WAggIst                45     80A
006300980706     D*
006400980706     D WInd            DS
006500980706     D WIndiriz                1     44A
006600980706     D WIndAgg                45     80A
006700990527     D*-------------
006800990527     D SavStato        S                   Like(ActStato)
006900990527     D*-------------
007000950711     C*----------------------------------------------------*
007100950711     C*                MAIN LINE PROGRAM
007200950711     C*----------------------------------------------------*
007300950711     C*
007400950711     C* inizializzazione variabili
007500950711     C                   EXSR      INZVAR
007600950711     C*
007700950711     C     $FINE         DOWNE     *ON
007800950711     C     $GEST         CASEQ     'P1'          GESABI
007900950711     C                   ENDCS
008000950711     C                   ENDDO
008100950711     C* fine programma
008200950711     C                   EXSR      ENDPGM
008300950711     C*
008400950711     C/EJECT
008500950711     C************************************************************
008600950711     C* FINE PROGRAMMA
008700950711     C************************************************************
008800950711     C     ENDPGM        BEGSR
008900950711     C*
009000950711     C                   SETON                                        LR
009100950711     C*
009200950711     C                   ENDSR
009300950711     C/EJECT
009400950711     C************************************************************
009500950713     C* GESTIONE ANAB800F/ANAB900F
009600950711     C************************************************************
009700980720     C     GesAbi        BegSr
009800950711     C*
009900980706     C                   Write     a191ts
010000980706     C                   Write     a191t1
010100980706     C                   Eval       *in21=*off
010200950713     C* ANAB800F
010300950711   1 C                   if         d1180c='S'
010400950711   2 C                   dow        *in21=*off
010500950713     C                   read      anab800f                               21
010600950711     C*
010700950711     C                   eval      abi=ab8abi
010800950711     C                   eval      cab=ab8cab
010900950712     C                   eval      p1abi=ab8abi
011000950712     C                   eval      p1cab=ab8cab
011100950713     C* tipo record 11
011200950711   3 C                   if         ab8tr='11'
011300950711     C                   eval      ab811ds=ab8f01
011400020328B0431C*
011500020328  "  C                   eval      ActAbi=abi
011600020328  "  C                   eval      AcrAbi=abi
011700020328  "  C                   eval      AcrCab=cab
011800020328  "  C                   move      ab8denist     Wdes
011900020328  "  C                   movel     Wist          ActDesIst
012000020328  "  C                   movel     WaggIst       ActDesAgis
012100020328  "  C                   movel     ActDesIst     ActDesBrIs
012200020328B0431C*
012300950711   4 C                   if         ab8tipo='1' or ab8tipo='2'
012400950713     C* stampa segnalazione e torna a leggere
012500950712     C                   eval       p1denist=ab8denist
012600950713   5 C                   if         ab8tipo='1'
012700950713     C                   eval       p1msg=$assorb
012800950713     C                   else
012900950713     C                   eval       p1msg=$chiusa
013000950713   5-C                   endif
013100950713     C                   exsr      stampa
013200950711     C                   iter
013300950711   4-C                   endif
013400020328     C*
013500020328B0431C****               eval      ActAbi=abi
013600020328  "  C****               eval      AcrAbi=abi
013700020328  "  C****               eval      AcrCab=cab
013800020328  "  C****               move      ab8denist     Wdes
013900020328  "  C****               movel     Wist          ActDesIst
014000020328  "  C****               movel     WaggIst       ActDesAgis
014100020328B0431C****               movel     ActDesIst     ActDesBrIs
014200950711   3-C                   endif
014300950713     C* tipo record 21
014400950711   3 C                   if         ab8tr='21'
014500950711     C                   eval      ab821ds=ab8f01
014600980707     C                   eval      ActAbi=abi
014700980707     C                   eval      AcrAbi=abi
014800980707     C                   eval      AcrCab=cab
014900980707     C                   eval      AcrDesAge=ab8denom
015000980707     C                   Move      ab8indir      Wind
015100980707     C                   Movel     Windiriz      AcrIndiriz
015200980707     C                   Movel     WindAgg       AcrIndAgg
015300980707     C                   eval      AcrLocalit = ab8comune
015400980707     C                   eval      AcrProv = ab8provinc
015500980707     C                   eval      AcrCap = ab8cap
015600950713     C* scrittura/aggiornamento d.b.
015700950711     C                   exsr      scrivi
015800950711     C*
015900950711   3-C                   endif
016000950711     C*
016100950711   2-C                   enddo
016200950711   1-C                   endif
016300950711     C*
016400950713     C* ANAB900F
016500950711     C                   eval       *in21=*off
016600980707   1 C                   If         d1190c='S'
016700980707   2 C                   DoW        *in21=*off
016800950711     C                   read      anab900f                               21
016900950711     C*
017000950711     C                   eval      abi=ab9abi
017100950711     C                   eval      cab=ab9cab
017200000125B0160C*****              eval      p1abi=ab8abi
017300000125  "  C*****              eval      p1cab=ab8cab
017400000125  "  C                   eval      p1abi=ab9abi
017500000125B0160C                   eval      p1cab=ab9cab
017600950713     C* tipo record 11
017700980707   3 C                   If         ab9tr='11'
017800950711     C                   eval      ab911ds=ab9f01
017900020328B0431C*
018000020328  "  C                   eval      ActAbi = abi
018100020328  "  C                   eval      AcrAbi = abi
018200020328  "  C                   eval      AcrCab = cab
018300020328  "  C                   move      ab9denist     Wdes
018400020328  "  C                   movel     Wist          ActDesIst
018500020328  "  C                   movel     WaggIst       ActDesAgis
018600020328  "  C                   eval      ActDesBrIs = ActDesIst
018700020328B0431C*
018800980707   4 C                   If         ab9tipo='1' or ab9tipo='2'
018900950713     C* stampa segnalazione e torna a leggere
019000950713     C                   eval       p1denist=ab9denist
019100980707   5 C                   If         ab9tipo='1'
019200950713     C                   eval       p1msg=$assorb
019300950713     C                   else
019400950713     C                   eval       p1msg=$chiusa
019500980707   5-C                   Endif
019600950713     C                   exsr      stampa
019700950711   4-C                   iter
019800980707     C                   Endif
019900020328     C*
020000020328B0431C****               eval      ActAbi = abi
020100020328  "  C****               eval      AcrAbi = abi
020200020328  "  C****               eval      AcrCab = cab
020300020328  "  C****               move      ab9denist     Wdes
020400020328  "  C****               movel     Wist          ActDesIst
020500020328  "  C****               movel     WaggIst       ActDesAgis
020600020328B0431C****               eval      ActDesBrIs = ActDesIst
020700980707   3-C                   Endif
020800950713     C* tipo record 21
020900980707   3 C                   If         ab9tr='21'
021000950711     C                   eval      ab921ds=ab9f01
021100980707     C                   eval      ActAbi = abi
021200980707     C                   eval      AcrAbi = abi
021300980707     C                   eval      AcrCab = cab
021400980707     C                   Move      ab9indir      Wind
021500980707     C                   Movel     Windiriz      AcrIndiriz
021600980707     C                   Movel     WindAgg       AcrIndAgg
021700980707     C                   If        ab9localit <> *blank
021800980707     C                   movel     ab9localit    AcrLocalit
021900980707     C                   Else
022000980707     C                   movel     ab9comune     AcrLocalit
022100980707     C                   EndIf
022200980707     C                   eval      AcrProv = ab9provinc
022300980707     C                   eval      AcrCap = ab9cap
022400980707   3-C                   Endif
022500950713     C* tipo record 31
022600950711   3 C                   if         ab9tr='31'
022700950711     C                   eval      ab931ds=ab9f01
022800980707     C                   Movel     ab9denom      AcrDesAge
022900980707     C                   Movel     ab9denom      AcrDesBrAg
023000950713     C* scrittura/aggiornamento d.b.
023100950711     C                   exsr      scrivi
023200950711     C*
023300980707   3-C                   EndIf
023400950711     C*
023500980707   2-C                   EndDo
023600980707   1-C                   EndIf
023700950711     C*
023800950711     C                   MOVE      *ON           $FINE
023900950711     C*
024000950711     C                   ENDSR
024100950711     C/EJECT
024200950711     C************************************************************
024300990527     C* Scrittura/Aggiornamento ANACT/ANACR
024400950711     C************************************************************
024500950713     C     SCRIVI        BEGSR
024600980707     C* salva i nuovi dati
024700980707     C                   Eval      NewAbi = dsAbi
024800980707     C                   Eval      NewCab = dsCab
024900980707     C* riga di AnaCt00f
025000980707     C     Abi           Chain     ANACT01L                           22
025100950711     C*
025200980707   1 C                   If        *in22=*on
025300980707     C                   clear                   ActStato
025400980707     C                   clear                   ActStatus
025500980707     C                   clear                   ActAssorDa
025600980707     C                   clear                   ActCartell
025700980707     C                   clear                   ActPreSuMa
025800980707     C                   clear                   ActGesReIn
025900980707     C                   clear                   ActCodSia
026000980707     C                   clear                   ActCoEst
026100980707     C                   Write     ANACT000
026200950711     C*
026300980707   1xC                   Else
026400950711     C*
026500980707     C                   Eval      OldAbi = DsAbi
026600980707   2 C                   If        OldAbi <> NewAbi
026700990527B0031C                   Eval      SavStato = ActStato
026800980707     C                   eval      DsAbi = NewAbi
026900990527B0031C*****              clear                   ActStato
027000990527B0031C                   move      SavStato      ActStato
027100980707     C                   clear                   ActStatus
027200980707     C                   clear                   ActAssorDa
027300980707     C                   clear                   ActCartell
027400980707     C                   clear                   ActPreSuMa
027500980707     C                   clear                   ActGesReIn
027600980707     C                   clear                   ActCodSia
027700980707     C                   clear                   ActCoEst
027800980707     C                   UPDATE    ANACT000
027900980707   2-C                   EndIf
028000980707   1-C                   EndIf
028100980707     C*
028200980707     C* riga di AnaCr00f
028300980707     C     K2Abi1        Chain     ANACR01L                           22
028400980707     C*
028500980707   1 C                   If        *in22=*on
028600980707     C                   clear                   AcrStato
028700980707     C                   clear                   ActCoEst
028800980707     C                   Write     ANACR000
028900980707     C*
029000980707   1xC                   Else
029100980707     C*
029200980707     C                   Eval      OldCab = DsCab
029300980707   2 C                   If        OldCab <> NewCab
029400990527B0031C                   Eval      SavStato = AcrStato
029500980707     C                   eval      DsCab = NewCab
029600990527B0031C*****              clear                   AcrStato
029700990527B0031C                   move      SavStato      AcrStato
029800980707     C                   clear                   ActCoEst
029900980707     C                   UPDATE    ANACR000
030000980707   2-C                   EndIf
030100980707   1-C                   EndIf
030200950711     C*
030300950713     C* stampa
030400980707     C                   eval      p1denist=ActDesIst
030500980707   1 C                   if         *in22=*on
030600980707     C                   eval       p1msg=$aggiun
030700980707     C                   exsr      stampa
030800950713     C                   else
030900950713   2 C                   if        oldabi<>newabi
031000950713     C                   eval       p1msg=$aggior
031100980720     C                   eval       p1abiv=%subst(oldCab:5:5)
031200980720     C                   eval       p1cabv=%subst(oldCab:10:5)
031300950713     C                   exsr      stampa
031400950713   2-C                   endif
031500950713   1-C                   endif
031600950713     C*
031700950713     C                   ENDSR
031800950713     C/EJECT
031900950713     C************************************************************
032000950713     C* stampa
032100950713     C************************************************************
032200950713     C     STAMPA        BEGSR
032300950713     C*
032400950713   1 C                   if        *in39=*on
032500950713     C                   write     a191ts
032600950713     C                   write     a191t1
032700950713     C                   eval      *in39=*off
032800950713   1-C                   endif
032900950713     C                   write     a191p1
033000950711     C*
033100950711     C                   ENDSR
033200950711     C/EJECT
033300950711     C************************************************************
033400950711     C* OPERAZIONI INIZIALI
033500950711     C************************************************************
033600950711     C     *INZSR        BEGSR
033700950711     C*
033800950711     C                   MOVE      'P1'          $GEST
033900950711     C*
034000950711     C                   ENDSR
034100950711     C/EJECT
034200950711     C************************************************************
034300950711     C* INIZIALIZZAZIONE VARIABILI
034400950711     C************************************************************
034500950711     C     INZVAR        BEGSR
034600950711     C*
034700950711     C* Reperimento parametri
034800950711     C*
034900950711     C     *ENTRY        PLIST
035000950711    >C                   PARM                    KPJBA
035100950711    >C*
035200950711    >C*                  PARM                    ___011           10
035300950711    >C*
035400950711    >C                   EXSR      INZP1
035500950711     C*
035600950711     C* imposto costanti
035700950711     C*
035800950711     C                   ENDSR
035900950711     C/EJECT
036000950711     C************************************************************
036100950711     C* INIZIALIZZAZIONE
036200950711     C************************************************************
036300950711     C     INZP1         BEGSR
036400950711     C*
036500950711     C                   CLEAR                   A191P1
036600950711     C*
036700950711     C* Immettere sempre un campo di descrizione
036800950711    >C*                  MOVEL     DES011        D1DES
036900950711     C*
037000950711     C                   ENDSR
037100950711     C/EJECT
037200950711     C************************************************************
037300950711     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
037400950711     C************************************************************
037500950711     C     DEFCAM        BEGSR
037600950711     C*
037700950711     C* Variabili per gestione videate
037800950711     C*
037900950711     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
038000950711     C                   MOVE      *BLANK        $FINE             1            fine pgm
038100950711     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
038200950711     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
038300950711     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
038400950711     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
038500950711     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
038600950711     C                   MOVE      *BLANK        $EFILE            1            memo fine file
038700950711     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
038800950711     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
038900950711     C                   MOVE      *BLANK        $AGGOK            1            confermatore OK
039000950711     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
039100950711     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
039200950711     C*
039300950711     C* Variabili appoggio sempre presenti in tutte le anagrafiche
039400950711     C*
039500950711     C     *LIKE         DEFINE    $INZS1        WINZS1
039600950711     C*    *LIKE         DEFINE    *INKR         WINKR
039700950711     C                   Z-ADD     0             CURR              2 0
039800950711     C                   Z-ADD     0             CURC              2 0
039900950711     C                   MOVE      *BLANK        WIN              99
040000950711     C*
040100950711     C* Variabili appoggio specifiche della singola anagrafica
040200980706     C     *Like         Define    AcrAbi        abi
040300980706     C     *Like         Define    AcrCab        cab
040400980706     C     k2abi1        KList
040500980706     C                   KFld                    abi
040600980706     C                   KFld                    cab
040700980706     C                   Clear                   OldAbi          163
040800980706     C                   Clear                   NewAbi          163
040900950711     C*
041000980706     C                   Clear                   OldCab          231
041100980706     C                   Clear                   NewCab          231
041200950711     C* Indici
041300950711     C*
041400950711     C                   Z-ADD     0             X                 3 0
041500950711     C                   Z-ADD     0             Y                 3 0
041600950711     C*
041700950711     C                   ENDSR
041800950711     C/EJECT
