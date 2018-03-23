000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA DELTA - CREAZIONE WFILE DI STAMPA                           *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000900000824     FTABEL00F  IF   E           K DISK
001000020404     FTNTBE01L  IF   E           K DISK
001100990609     FSISDC01L  IF   E           K DISK
001200990609     FSISDO01L  IF   E           K DISK
001300061129     FWFksc01L  UF A E           K DISK    rename(WFWFD000:WFKSC000)
001301061129     FWFreg01L  UF A E           K DISK    rename(WFWFT000:WFREG000)
004300000000     D*------------------
004400000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
004500000000     D*------------------
004600000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
004700000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
004800000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
004900970821     D*------------------
005000970821     D* FASCE DI PESO
005100970821     D*------------------
005101061129     d* fasce di peso fino a 10 kg
005200061129     D FPE1            S              2    DIM(11)
005201061129     d* fasce di peso > 10 kg e fino a 100 kg
005202061129     D FPE2            S              2    DIM(11)
005203061129     d* fasce di peso > 100 kg
005204061129     D FPE3            S              2    DIM(11)
005300970821     D*------------------
005400970821     D* PERIODO DA ELABORARE
005500970821     D*------------------
005600990609     D ANN             S              4  0 DIM(12)                              *ANNO
005700000823     D MES             S              2  0 DIM(12)                              *MESE
006400020314     D*------------------
006500020314     D* RAGGRUPPAMENTI REGIONI/NETWORK
006600020314     D*------------------
006700020315     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIO
006800020315     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RA
006900020404     D*------------------
007000020404     D* NETWORK AZIENDALI
007100020404     D*------------------
007200020404     D NTWCOD          S              3    DIM(100)                             *CODICI NETWORK
007300020404     D NTWORD          S              2  0 DIM(100)                             *PRIORITA' CONSIDERA
008200000823     D*------------------
008300000823     D* DS DI PROCEDURA
008400000823     D*------------------
008500000823     D BS10DS        E DS                  EXTNAME(TIBS10DS)
008600000000     D*------------------
008700000000     D* PARAMETRI DI LANCIO
008800000000     D*------------------
008900000000     D PARAM           DS
009000990609     D  PARDAT                 1      6  0
009100990609     D  PARAAI                 1      4  0
009200990609     D  PARMMI                 5      6  0
009300990609     D  PARDA2                 7     12  0
009400990609     D  PARAAF                 7     10  0
009500990609     D  PARMMF                11     12  0
013400000000     D*------------------
013500990609     D* DATA DI SISDC01L
013600000000     D*------------------
013700000000     D                 DS
013800990609     D  SDCANN                 1      4  0
013900990609     D  SDCMES                 5      6  0
014000990609     D  SDCDAT                 1      6  0
014100970821     D*------------------
014200970821     D* DATA DI WFDEL00F
014300970821     D*------------------
014400970821     D                 DS
014500970821     D  WFDANN                 1      4  0
014600970821     D  WFDMES                 5      6  0
014700990609     D  WFDDAT                 1      6  0
015200000000     D*------------------
015300000000     D* DS FASCE DI PESO
015400000000     D*------------------
015500000000     D DS2L          E DS
015600000000     D*------------------
015700000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
015800000000     D*------------------
015900000000     D DS2W          E DS
016000980403     D*------------------
016100980403     D* DS REPERIMENTO DATI UTENTE
016200980403     D*-------------------
016300980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
016400980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
016500980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
016600980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
016700980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
016800000000     D*------------------
016900000000     D* ARCHITETTURA
017000000000     D*------------------
017100000000     D KPJBA         E DS
017200020314     D*------------------
017300020314     D* DS DI LETTURA TABELLA "2M"
017400020314     D*------------------
017500020314     D DS2M          E DS
017600020404     D*-------------------
017700020404     D* DS DEFINIZIONE TABELLA "NETWORK"
017800020404     D*-------------------
017900020404     D DNTW          E DS
018000000000     C*------------------------------------------------------------------------*
018100000000     C* MAIN LINE
018200000000     C*------------------------------------------------------------------------*
018400000000     C* LEGGO IL PRIMO CLIENTE VALIDO
018500000000     C                   Z-ADD     *ZEROS        KSOKSC
018600990609     C     KEYSDO        SETGT     SISDO01L
018700990609     C                   READ      SISDO01L                               99
018800000000     C*
018900000000     C* PER TUTTI I CLIENTI PRESENTI NEI SALDI
019000000000DO  1C     *IN99         DOWEQ     *OFF
019100980119     C                   Z-ADD     SDOKSC        DEPKSC                         *DEPOSITO CLIENTE
019500000000     C*
019600000000     C* OPERAZIONI PER NUOVO CLIENTE
019800000000     C                   EXSR      NEWKSC
019900000000     C*
020000000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
020100000000     C                   EXSR      SETFIL
020101061129     C                   EXSR      LETFIL
020200000000     C*
020300000000     C* CICLO FINO A ROTTURA CODICE CLIENTE
020400980119DO  3C     $FINE         DOWEQ     '0'
020804061129     C*
021300000000     C* CREA I SALDI NEL WORK FILE
021400000824     C                   EXSR      MEMSAL
021500000000     C*
021600000000     C* LEGGE IL RECORD SUCCESSIVO
021700000000     C                   EXSR      LETFIL
021901061129E   3C                   ENDDO                                                  *ROTTURA CLIENTE
022100000000     C*
022200000000     C* LEGGO IL SUCESSIVO CLIENTE VALIDO
022300000000     C                   Z-ADD     DEPKSC        KSOKSC
022400990609     C     KEYSDO        SETGT     SISDO01L
022500990609     C                   READ      SISDO01L                               99
022600000824E   1C                   ENDDO
023000980403     C*
023100061129     C* SCRIVE FILE TOTALI
023200061129     C                   EXSR      TOTALI
023201061129     C*
023202061129     C* OPERAZIONI FINALI
023203061129     C                   EXSR      FINSR
023300000000     C*
023400000000     C                   SETON                                        LR
023500000000     C*------------------------------------------------------------------------*
023600000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
023700000000     C*------------------------------------------------------------------------*
023800000000     C     NEWKSC        BEGSR
023900000824     C*
024000000824     C* IMPOSTA IL CLIENTE DA SCRIVERE SUL WFILE -CLIENTE O UNIFICANTE-
024200000823     C                   CLEAR                   BS10DS
024400010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
024500000823     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
024600000823     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
024700000823     C                   MOVE      DEPKSC        D10COD
024800000823     C                   CALL      'TIBS10R'
024900000823     C                   PARM                    BS10DS
025001061129     C*
025002061129     C                   Z-ADD     DEPKSC        DEPCLI
025100000823IF  2C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
025200000823     C     D10COP        ANDNE     *ZEROS
025300000823     C                   Z-ADD     D10COP        DEPCLI                         *UNIFICANTE
025400000823E   2C                   ENDIF
025401061129     C*
027200000824     C* REPERISCE IL COMMERCIALE, SE NON ESISTE IMPOSTA IL CODICE AGENTE = FIL + 0000
027300980403     C                   CLEAR                   BS69DS
027400980403     C                   CLEAR                   CLPDS
027500980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
027600000823     C                   MOVEL     DEPCLI        I69KCP                         *CLIENTE X CNCLP
027601061129     C                   MOVEL     DEPCLI        I69KAC                         *CLIENTE X CNACO
027700980403     C                   CALL      'TIBS69R'
027800980403     C                   PARM                    BS69DS
027900980403     C                   PARM                    ACODS
028000980403     C                   PARM                    INDDS
028100980403     C                   PARM                    CLPDS
028200980403     C                   PARM                    CLSDS
028201061129      **
028202061129     c                   clear                   deprag           35
028204061129IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
028205061129     C                   MOVEL     ACORAG        deprag                         *RAGIONE SOCIALE
028206061129     c                   else
028207061129     c                   move      *all'?'       deprag           35
028208061129     c                   end
028209061129     C*
029200000000     C* REPERISCE IL CODICE QUALITA'
029300000000     C                   MOVEL     'C'           DEPCLV
029400000000IF  1C     CLPCLV        IFNE      *BLANKS
029500000000     C                   MOVEL     CLPCLV        DEPCLV
029600000000E   1C                   ENDIF
033500000000     C*
033600000000     C                   ENDSR
036400000000     C*------------------------------------------------------------------------*
036500000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
036600000000     C*------------------------------------------------------------------------*
036700000000     C     SETFIL        BEGSR
036800000000     C*
036900000000     C* REIMPOSTO FLAG DI FINE LETTURA SALDI
037000000000     C                   MOVEL     '0'           $FINE
037100000000     C*
037200000000     C                   Z-ADD     DEPKSC        KSCKSC                         *CLIENTE
037300061129     C                   Z-ADD     PARAAI        KSCANN                         *ANNO PRECEDENTE INI
037400061129     C                   Z-ADD     PARMMI        KSCMES                         *MESE PRECEDENTE INI
037500000000     C*
037600000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO SALDO CLIENTE VALIDO
037700990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
038400000000     C*
038500000000     C                   ENDSR
038600000000     C*------------------------------------------------------------------------*
038700000000     C* LETFIL - LETTURA PROSSIMO RECORD SALDI CLIENTE
038800000000     C*------------------------------------------------------------------------*
038900000000     C     LETFIL        BEGSR
039000000000     C*
039100000000     C* LEGGO FINO A:
039200000000     C*     - FINE FILE
039300000000     C*     - TROVATO RECORD VALIDO
039400980601     C                   MOVEL     'N'           WRECOK
039500000000DO  1C     $FINE         DOWEQ     '0'
039600000000     C     WRECOK        ANDEQ     'N'
039700000000     C*
039800990609     C     KSCKSC        READE     SISDC01L                               99
039900000000     C*
040000000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
040100000000     C                   MOVEL     '1'           $FINE
040200000000X   2C                   ELSE
040300000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
040400000000E   2C                   ENDIF
040500000000     C*
040600000000E   1C                   ENDDO
040700000000     C*
040800000000     C                   ENDSR
040900000000     C*------------------------------------------------------------------------*
041000000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
041100000000     C*------------------------------------------------------------------------*
041200000000     C     CHKREC        BEGSR
041300000000     C*
041400980601     C                   MOVEL     'S'           WRECOK
041500020516     C*
041600020516     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
041700020516     C                   Z-ADD     1             G
041800020516     C                   SETOFF                                           28
041900020516     C     SDCRBL        LOOKUP    FLAB(G)                                28
042000020516IF  1C     *IN28         IFEQ      *OFF                                         *TIPO NON TROVATO --
042100020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042200020516E   1C                   ENDIF
042300020516     C*
042400020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
042500020516IF  1C     WRECOK        IFEQ      'S'
042600020516IF  2C     SDCNSP        IFEQ      *ZEROS
042700020516     C     SDCPKG        ANDEQ     *ZEROS
042800020516     C     SDCPCC        ANDEQ     *ZEROS
042900020516     C     SDCPLS        ANDEQ     *ZEROS
043000020516     C     SDCNCO        ANDEQ     *ZEROS
043100020516     C     SDCVOL        ANDEQ     *ZEROS
043200020516     C     SDCVCC        ANDEQ     *ZEROS
043300020516     C     SDCVLS        ANDEQ     *ZEROS
043400020516     C     SDCIRB        ANDEQ     *ZEROS
043500020516     C     SDCIRR        ANDEQ     *ZEROS
043600020516     C     SDCINC        ANDEQ     *ZEROS
043700020516     C     SDCIRP        ANDEQ     *ZEROS
043800020516     C     SDCIRC        ANDEQ     *ZEROS
043900020516     C     SDCIRD        ANDEQ     *ZEROS
044000020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
044100020516E   2C                   ENDIF
044200020516E   1C                   ENDIF
045200000000     C*--------------------
045300000000     C* SELEZIONI
045400000000     C*--------------------
045500000000     C* SE I SALDI SONO DI DATA SUPERIORE A QUELLA RICHIESTA, ESCO DAL CICLO
045600000000IF  1C     SDCDAT        IFGT      PARDA2
045700000000     C                   MOVEL     'N'           WRECOK
045800000000     C                   MOVEL     '1'           $FINE
045900000000E   1C                   ENDIF
046600000000     C*
046700000000     C                   ENDSR
046800000000     C*------------------------------------------------------------------------*
046900061129     C* srioe  - italia estero
047000000000     C*------------------------------------------------------------------------*
047100061129     C     srioe         BEGSR
048300020314     C*
048400061129     C* DETERMINA IL NETWORK  sia x Partenza che Arrivo
048500061129     C                   MOVEL     *BLANKS       DEPNTW            3
048501061129     C                   MOVEL     *BLANKS       DEPNTWP           3
048600020404     C                   Z-ADD     *ZEROS        DEPORDP           3 0
048700020314     C                   Z-ADD     1             R
048701061129     C*
048702061129     C* regione Partenza
048800020314     C     SDCREP        LOOKUP    RGREG(R)                               55
048900020314     C                   IF        *IN55 = *ON
049000020404     C                   EVAL      DEPNTWP = RGNTW(R)
049100061129     C* REPERISCO L'ORDINE DI CONSIDERAZIONE DEL NETWORK
049200020404     C                   Z-ADD     1             X
049300020404     C     DEPNTWP       LOOKUP    NTWCOD(X)                              55
049400020404     C                   IF        *IN55 = *ON
049500020404     C                   EVAL      DEPORDP = NTWORD(X)
049600020404     C                   ENDIF
049700020314     C                   ENDIF
049800020314     C*
049900020404     C                   MOVEL     *BLANKS       DEPNTWA           3
050000020404     C                   Z-ADD     *ZEROS        DEPORDA           3 0
050100020314     C                   Z-ADD     1             R
050101061129     C* regione Arrivo
050200020314     C     SDCREA        LOOKUP    RGREG(R)                               55
050300020314     C                   IF        *IN55 = *ON
050400020404     C                   EVAL      DEPNTWA = RGNTW(R)
050500061129     C* REPERISCO L'ORDINE DI CONSIDERAZIONE DEL NETWORK
050600020404     C                   Z-ADD     1             X
050700020404     C     DEPNTWA       LOOKUP    NTWCOD(X)                              55
050800020404     C                   IF        *IN55 = *ON
050900020404     C                   EVAL      DEPORDA = NTWORD(X)
051000020404     C                   ENDIF
051100020314     C                   ENDIF
051200020404     C*
051300020404     C* IN BASE ALL'ORDINE DI CONSIDERAZIONE ATTRIBUISCO IL NETWORK DELLA SPEDIZIONE (ATTUALMENTE SO
051400020404     C*
051500020404     C* 1) Italia su Italia
051600020404     C                   IF        DEPNTWP = *BLANKS AND
051700020404     C                             DEPNTWA = *BLANKS
051800020404     C                   EVAL      DEPNTW = *BLANKS
051900020404     C                   ELSE
052000020404     C* 2) Estero su Estero
052100020404     C                   IF        DEPNTWP <> *BLANKS AND
052200020404     C                             DEPNTWA <> *BLANKS
052300020404     C                   IF        DEPORDP < DEPORDA
052400020404     C                   EVAL      DEPNTW = DEPNTWP
052500020404     C                   ELSE
052600020404     C                   EVAL      DEPNTW = DEPNTWA
052700020404     C                   ENDIF
052800020404     C                   ELSE
052900020404     C* 3) Estero su Italia / Italia su Estero
053000020404     C                   IF        DEPNTWP <> *BLANKS
053100020404     C                   EVAL      DEPNTW = DEPNTWP
053200020404     C                   ELSE
053300020404     C                   EVAL      DEPNTW = DEPNTWA
053400020404     C                   ENDIF
053500020404     C                   ENDIF
053600020404     C                   ENDIF
053800020315     C* IMPOSTA SE FLAG ITALIA O ESTERO
053801061129     C* Logica x determinare se Italia o Estero:
053802061129     C*   Se presente il Network in uno dei 2 casi si tratta di Estero
053900020404     C     DEPNTWP       IFNE      *BLANKS
054000020404     C     DEPNTWA       ORNE      *BLANKS
054100061129     C                   MOVEL     'E'           DEPIOE            1            *ESTERO
054200020315X   1C                   ELSE
054300020315     C                   MOVEL     'I'           DEPIOE                         *ITALIA
054400020315E   1C                   ENDIF
054401061129     c                   endsr
054402061129     C*------------------------------------------------------------------------*
054403061129     C* MEMSAL - MEMORIZZA I DATI
054404061129     C*------------------------------------------------------------------------*
054405061129     C     MEMSAL        BEGSR
054406061129     C*
054407061129      * ? italia o estero        al momento da saltare
054408061129     c                   goto      salta_x_ora
054409061129     c*                  exsr      srioe
054410061129     c     salta_x_ora   tag
054411061129     C*
054412061129     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
054413061129     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
054414061129IF  1C     FSP(G)        IFNE      'S'
054415061129     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
054416061129E   1C                   ENDIF
054417061129     C*
054418061129     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
054419061129     c                   clear                   deprrn           15 3
054420061129     c                   eval      deprrn = sdcird + sdcirr + sdcinc
054502061129     C*
054503061129     c* se trovo la fascia di peso nella schiera FPE2 vuol dire che il peso
054504061129     c* è tra 11 e 100 kg
054505061129     C                   SETOFF                                       2829
054506061129     C                   Z-ADD     1             P2
054507061129     C     SDCFPE        LOOKUP    FPE2(P2)                               28
054508061129      ***
054509061129     c                   if        not *in28
054510061129     c* se trovo la fascia di peso nella schiera FPE3 vuol dire che il peso
054511061129     c* è oltre 100 kg
054512061129     C                   Z-ADD     1             P3
054513061129     C     SDCFPE        LOOKUP    FPE3(P3)                               29
054514061129     c                   end
054515061129      ***
054516061129     c* altrimenti è nella fascia di peso FPE1
054517061129     c*
054600000000     C* CONTROLLA ESISTENZA RECORD DI LAVORO
055100061129     C                   MOVEL     DEPCLV        KWDCLV                         *CODICE QUALITA'
055200000823     C                   Z-ADD     DEPCLI        KWDCLI                         *CLIENTE
055300000824     C                   Z-ADD     SDCANN        KWDANN                         *ANNO
055400000824     C                   Z-ADD     SDCMES        KWDMES                         *MESE
055500061129     C                   MOVEL     SDCrep        KWDrep                         *FASCIA DI PESO
055501061129     C                   MOVEL     SDCrea        KWDrea                         *FASCIA DI PESO
055800061129     C     KEYDEL        CHAIN     WFksc01L                           99
055900000000     C***
056000000000     C* I M M I S S I O N E
056100000000     C***
056200000000IF  1C     *IN99         IFEQ      '1'
056300061129     c                   CLEAR                   WFksc000
056800000000     C                   MOVEL     DEPCLV        WFDCLV                         *CODICE QUALITA'
056900061129     C                   Z-ADD     DEPCLI        WFDksc                         *CLIENTE
056901061129     C                   movel     DEPrag        WFDksd                         *RAG.SOC.
057000000824     C                   Z-ADD     SDCANN        WFDANN                         *ANNO
057100000824     C                   Z-ADD     SDCMES        WFDMES                         *MESE
057200061129     C                   MOVEL     SDCrep        WFDrep                         *Regione Partenza
057201061129     C                   MOVEL     SDCrea        WFDrea                         *Regione Arrivo
057202061129      **
057704061129     c* *in28 vuol dire che il peso è compreso fra 11 e 100 kg
057705061129     c* *in29 vuol dire che il peso è oltre 100 kg
057706061129     c                   select
057707061129     c                   when      *in28
057714061129     C                   Z-ADD     SDCNSP        WFDNS2                         *NUMERO SPEDIZIONI
057715061129     C                   Z-ADD     DEPRRN        WFDIR2                         RIC REALE NETTO DELT
057716061129     C                   Z-ADD     SDCIRP        WFDIC2                         RICAVO PRESUNTO DELT
057717061129     c                   when      *in29
057718061129     C                   Z-ADD     SDCNSP        WFDNS3                         *NUMERO SPEDIZIONI
057800061129     C                   Z-ADD     DEPRRN        WFDIR3                         RIC REALE NETTO DELT
057900061129     C                   Z-ADD     SDCIRP        WFDIC3                         RICAVO PRESUNTO DELT
057901061129     c                   other
057902061129     C                   Z-ADD     SDCNSP        WFDNS1                         *NUMERO SPEDIZIONI
057903061129     C                   Z-ADD     DEPRRN        WFDIR1                         RIC REALE NETTO DELT
057904061129     C                   Z-ADD     SDCIRP        WFDIC1                         RICAVO PRESUNTO DELT
057905061129     c                   endsl
058300061129     C                   WRITE     WFksc000
058400000000X   1C                   ELSE
058500000000     C***
058600000000     C* V A R I A Z I O N E
058700000000     C***
058701061129     c* *in28 vuol dire che il peso è compreso fra 11 e 100 kg
058702061129     c* *in29 vuol dire che il peso è oltre 100 kg
058703061129     c                   select
058705061129     c                   when      *in28
058706061129     C                   add       SDCNSP        WFDNS2                         *NUMERO SPEDIZIONI
058707061129     C                   add       DEPRRN        WFDIR2                         RIC REALE NETTO DELT
058708061129     C                   add       SDCIRP        WFDIC2                         RICAVO PRESUNTO DELT
058709061129     c                   when      *in29
058710061129     C                   add       SDCNSP        WFDNS3                         *NUMERO SPEDIZIONI
058711061129     C                   add       DEPRRN        WFDIR3                         RIC REALE NETTO DELT
058712061129     C                   add       SDCIRP        WFDIC3                         RICAVO PRESUNTO DELT
058713061129     c                   other
058714061129     C                   add       SDCNSP        WFDNS1                         *NUMERO SPEDIZIONI
058715061129     C                   add       DEPRRN        WFDIR1                         RIC REALE NETTO DELT
058716061129     C                   add       SDCIRP        WFDIC1                         RICAVO PRESUNTO DELT
058717061129     c                   endsl
059200061129     C                   UPDATE    WFksc000
059300000000E   1C                   ENDIF
059400000000     C*
059500000000     C                   ENDSR
093000000000     C*------------------------------------------------------------------------*
093100061129     C* TOTALI - SCRIVE FILE WFREG00F
093200000000     C*------------------------------------------------------------------------*
093300061129     C     TOTALI        BEGSR
093301061129     C*
093302061129     c     *loval        setll     WFksc01l
093303061129     c                   Read      WFksc01l
093304061129     C*
093305061129     c                   Dow       not %EoF(WFksc01l)
093306061129     c                   z-add     WFDANN        savANN
093307061129     c                   z-add     WFDMES        savMES
093308061129     c                   move      WFDREP        savREP
093309061129     c                   move      WFDREA        savREA
093310061129     c                   move      WFDCLV        savCLV
093311061129      **
093312061129      **   pulizia campi di totale
093313061129     c                   clear                   totNS1
093314061129     c                   clear                   totIR1
093315061129     c                   clear                   totIC1
093316061129     c                   clear                   totNS2
093317061129     c                   clear                   totIR2
093318061129     c                   clear                   totIC2
093319061129     c                   clear                   totNS3
093320061129     c                   clear                   totIR3
093321061129     c                   clear                   totIC3
093322061129     c
093323061129     c* x rottura di chiave di totale
093324061129     c                   Dow       not %EoF(WFksc01l) and
093325061129     c                               WFDANN = savANN  and
093326061129     c                               WFDMES = savMES  and
093327061129     c                               WFDREP = savREP  and
093328061129     c                               WFDREA = savREA  and
093329061129     c                               WFDCLV = savCLV
093330061129      **   somma i campi di valore
093331061129     c                   add       WFdNS1        totNS1
093332061129     c                   add       WFdIR1        totIR1
093333061129     c                   add       WFdIC1        totIC1
093334061129     c                   add       WFdNS2        totNS2
093335061129     c                   add       WFdIR2        totIR2
093336061129     c                   add       WFdIC2        totIC2
093337061129     c                   add       WFdNS3        totNS3
093338061129     c                   add       WFdIR3        totIR3
093339061129     c                   add       WFdIC3        totIC3
093340061129      **
093341061129     c                   Read      WFksc01l
093342061129     c                   enddo
093343061129      **
093344061129     c* a rottura di chiave di totale scrive record WFREG
093345061129     c                   clear                   WFreg000
093346061129     c                   eval      WFtANN = savANN
093347061129     c                   eval      WFtMES = savMES
093348061129     c                   eval      WFtREP = savREP
093349061129     c                   eval      WFtREA = savREA
093350061129     c                   eval      WFtCLV = savCLV
093351061129     c
093352061129     c                   z-add     totNS1        WFtNS1
093353061129     c                   z-add     totIR1        WFtIR1
093354061129     c                   z-add     totIC1        WFtIC1
093355061129     c                   z-add     totNS2        WFtNS2
093356061129     c                   z-add     totIR2        WFtIR2
093357061129     c                   z-add     totIC2        WFtIC2
093358061129     c                   z-add     totNS3        WFtNS3
093359061129     c                   z-add     totIR3        WFtIR3
093360061129     c                   z-add     totIC3        WFtIC3
093361061129     c
093362061129     c                   write     WFreg000
093363061129      **
093364061129     c                   enddo
093365061129     C*
093366061129     C                   ENDSR
093367061129     C*------------------------------------------------------------------------*
093368061129     C* CARTAB - CARICA I DATI IN TABELLE
093369061129     C*------------------------------------------------------------------------*
093370061129     C     CARTAB        BEGSR
098700000000     C*-------------------------
098800000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
098900000000     C*-------------------------
099000000000     C                   Z-ADD     *ZEROS        G
099100020906     C                   Z-ADD     1             KTBKUT
099200000000     C                   MOVEL     '2W'          KTBCOD
099300000000     C     KEYTA2        SETLL     TABEL00F
099400000824     C     KEYTA2        READE     TABEL00F                               99
099500000000DO  1C     *IN99         DOWEQ     *OFF
099600000000IF  2C     TBLFLG        IFEQ      *BLANKS
099700000000     C                   ADD       1             G
099800000000     C                   MOVEL     TBLKEY        FLAB(G)
099900000000     C                   MOVEL     TBLUNI        DS2W
100000000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
100100000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
100200000000E   2C                   ENDIF
100300000824     C     KEYTA2        READE     TABEL00F                               99
100400000000E   1C                   ENDDO
100500990217     C*--------------------
100600000000     C* FASCE DI PESO
100700990217     C*--------------------
100800990609IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
100900990217     C                   MOVEL     'SDC'         WSX               3
101000990217X   1C                   ELSE                                                   *FASCE DAL '99
101100020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO
101200020325     C                   MOVEL     'S9C'         WSX
101300020325X   2C                   ELSE                                                   *FASCE DAL 2002
101400020325     C                   MOVEL     'S2C'         WSX
101500020325E   2C                   ENDIF
101600020325E   1C                   ENDIF
101601061129      *
101700061129     C                   Z-ADD     *ZEROS        P1
101701061129     C                   Z-ADD     *zeros        P2
101702061129     C                   Z-ADD     *zeros        P3
101800020906     C                   Z-ADD     1             KTBKUT
101900000000     C                   MOVEL     '2L'          KTBCOD
102000000000     C     KEYTA2        SETLL     TABEL00F
102100000824     C     KEYTA2        READE     TABEL00F                               99
102101061129      *
102200000000DO  1C     *IN99         DOWEQ     *OFF
102201061129      *
102300000000IF  2C     TBLFLG        IFEQ      *BLANKS
102400990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
102500000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
102501061129      *
102600990217IF  3C     FILEST        IFEQ      WSX
102700000000     C                   MOVEL     TBLUNI        DS2L
102701061129      *
102800000309IF  4C     §2LKGI        IFGE      5000
102900000309     C     PARDAT        ANDLT     200001
103000000309     C                   ELSE
103001061129      *
103002061129      * ? CONTROLLARE  come carica le fasce dei pesi .
103003061129      *
103004061129     c                   if        §2LKGF <= 10
103100061129     C                   ADD       1             P1
103200061129     C                   MOVE      STACOD        FPE1(P1)                        *...+XX
103201061129     c                   end
103202061129      *
103203061129     c                   if        §2LKGI >= 10 and §2LKGF <= 100
103204061129     C                   ADD       1             P2
103205061129     c                   move      stacod        fpe2(p2)
103206061129     c                   end
103207061129      *
103208061129     c                   if        §2LKGI >= 100
103209061129     C                   ADD       1             P3
103210061129     c                   move      stacod        fpe3(p3)
103211061129     c                   end
103212061129      *
103300000309E   4C                   ENDIF
103400000000E   3C                   ENDIF
103500000000E   2C                   ENDIF
103501061129      *
103600000824     C     KEYTA2        READE     TABEL00F                               99
103700000000E   1C                   ENDDO
103701061129      *
103800020314     C*-------------------------
103900020314     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
104000020314     C*-------------------------
104100020314     C                   Z-ADD     *ZEROS        R
104200020906     C                   Z-ADD     1             KTBKUT
104300020314     C                   MOVEL     '2M'          KTBCOD
104400020314     C     KEYTA2        SETLL     TABEL00F
104500020314     C     KEYTA2        READE     TABEL00F                               99
104600020314DO  1C     *IN99         DOWEQ     *OFF
104700020314IF  2C     TBLFLG        IFEQ      *BLANKS
104800020314     C                   ADD       1             R
104900020314     C                   MOVEL     TBLKEY        RGREG(R)
105000020314     C                   MOVEL     TBLUNI        DS2M
105100020314     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
105200020314E   2C                   ENDIF
105300020314     C     KEYTA2        READE     TABEL00F                               99
105400020314E   1C                   ENDDO
105500020404     C*----------
105600020404     C* NETWORK AZIENDALI
105700020404     C*----------
105800020404     C                   Z-ADD     *ZEROS        X
105900020404     C                   EVAL      TBECOD = 'NTW'
106000020404     C     KEYTBE        SETLL     TNTBE01L
106100020404     C     KEYTBE        READE     TNTBE01L
106200020404     C                   DOW       NOT %EOF(TNTBE01L)
106300020404     C                   ADD       1             X
106400020404     C                   MOVEL     TBEUNI        DNTW
106500020404     C                   EVAL      NTWCOD(X) = TBEKE1
106600020404     C                   EVAL      NTWORD(X) = §NTWORD
106700020404     C     KEYTBE        READE     TNTBE01L
106800020404     C                   ENDDO
106900000000     C*
107000000000     C                   ENDSR
107100970821     C*------------------------------------------------------------------------*
107200970821     C* AMELAB - RIEMPE SCHIERE ANNI E MESI DA ELABORARE
107300970821     C*------------------------------------------------------------------------*
107400970821     C     AMELAB        BEGSR
107500970821     C*---
107600970821     C* ANNO INIZIALE UGUALE AD ANNO FINALE
107700970821     C*---
107800970821IF  1C     WAA2          IFEQ      WAA
107900970821     C*
108000970821     C* CALCOLO I MESI DI DIFFERENZA
108100970821DO  2C     WMM           DOWLE     WMM2
108200970821     C                   ADD       1             JI
108300970821     C                   Z-ADD     WAA           ANN(JI)
108400970821     C                   Z-ADD     WMM           MES(JI)
108500970821     C                   ADD       1             WMM
108600970821E   2C                   ENDDO
108700970821     C*
108800970821     C* IL RESTO DELLA SCHIERA A ZERO
108900970821DO  2C     JI            DOWLT     12
109000970821     C                   ADD       1             JI
109100970821     C                   Z-ADD     *ZEROS        ANN(JI)
109200970821     C                   Z-ADD     *ZEROS        MES(JI)
109300970821E   2C                   ENDDO
109400970821     C*---
109500970821     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
109600970821     C*---
109700970821X   1C                   ELSE
109800970821     C*
109900970821     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
110000970821DO  2C     WMM           DOWLE     12
110100970821     C                   ADD       1             JI
110200970821     C                   Z-ADD     WAA           ANN(JI)
110300970821     C                   Z-ADD     WMM           MES(JI)
110400970821     C                   ADD       1             WMM
110500970821E   2C                   ENDDO
110600970821     C*
110700970821     C* MEMORIZZO I MESI DELL'ANNO FINALE
110800970821     C                   Z-ADD     01            WMM
110900970821DO  2C     WMM           DOWLE     WMM2
111000970821     C                   ADD       1             JI
111100970821     C                   Z-ADD     WAA2          ANN(JI)
111200970821     C                   Z-ADD     WMM           MES(JI)
111300970821     C                   ADD       1             WMM
111400970821E   2C                   ENDDO
111500970821     C*
111600970821     C* IL RESTO DELLA SCHIERA A ZERO
111700970821DO  2C     JI            DOWLT     12
111800970821     C                   ADD       1             JI
111900970821     C                   Z-ADD     *ZEROS        ANN(JI)
112000970821     C                   Z-ADD     *ZEROS        MES(JI)
112100970821E   2C                   ENDDO
112200970821E   1C                   ENDIF
112300970821     C*
112400970821     C                   ENDSR
112500980403     C*------------------------------------------------------------------------*
112600980403     C* FINSR - OPERAZIONI FINALI
112700980403     C*------------------------------------------------------------------------*
112800980403     C     FINSR         BEGSR
112900980403     C*
113000980403     C* CHIUDE I FILE DEI PGM CHIAMATI
113100980403     C                   CLEAR                   BS69DS
113200980403     C                   CLEAR                   ACODS
113300980403     C                   CLEAR                   INDDS
113400980403     C                   CLEAR                   CLPDS
113500980403     C                   CLEAR                   CLSDS
113600980403     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
113700980403     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
113800980403     C                   CALL      'TIBS69R'
113900980403     C                   PARM                    BS69DS
114000980403     C                   PARM                    ACODS
114100980403     C                   PARM                    INDDS
114200980403     C                   PARM                    CLPDS
114300980403     C                   PARM                    CLSDS
114400980403     C*
114500980403     C                   ENDSR
114600000000     C*------------------------------------------------------------------------*
114700000000     C* *INZSR - IMPOSTAZIONI INIZIALI
114800000000     C*------------------------------------------------------------------------*
114900000000     C     *INZSR        BEGSR
115000000000     C*
115100000000     C* PARAMETRI IN INPUT AL PGM
115200000000     C     *ENTRY        PLIST
115300000000     C                   PARM                    KPJBA
115400000000     C                   MOVEL     KPJBU         PARAM
115500000000     C*
115600000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
115800000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
115900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
116000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
116001061129     C*
116100990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC01L
116200000000     C     *LIKE         DEFINE    SDCANN        KSCANN
116300000000     C     *LIKE         DEFINE    SDCMES        KSCMES
116301061129
116400990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO01L
116601061129     C*
117401061129     C     *LIKE         DEFINE    SDCANN        KWDANN
117402061129     C     *LIKE         DEFINE    SDCMES        KWDMES
117500061129     C     *LIKE         DEFINE    SDCrep        KWDrep
117600061129     C     *LIKE         DEFINE    SDCrea        KWDrea
117601061129     C     *LIKE         DEFINE    clpCLV        KWDCLV
117602061129     C     *LIKE         DEFINE    clpksc        KWDCLI
117800000000     C*
117900000000     C* DEFINIZIONI DEPOSITI E VARIABILI
118000000000     C     *LIKE         DEFINE    CLPKSC        DEPKSC
118100000823     C     *LIKE         DEFINE    CLPKSC        DEPCLI
118900000000     C     *LIKE         DEFINE    CLPCLV        DEPCLV
119101061129     C*
119102061129     c     *like         define    WFDANN        savANN
119103061129     c     *like         define    WFDMES        savMES
119104061129     c     *like         define    WFDREP        savREP
119105061129     c     *like         define    WFDREA        savREA
119106061129     c     *like         define    WFDCLV        savCLV
119400000825     C*
119401061129     c     *like         define    WFTNS1        totNS1
119402061129     c     *like         define    WFTIR1        totIR1
119403061129     c     *like         define    WFTIC1        totIC1
119404061129     c     *like         define    WFTNS2        totNS2
119405061129     c     *like         define    WFTIR2        totIR2
119406061129     c     *like         define    WFTIC2        totIC2
119407061129     c     *like         define    WFTNS3        totNS3
119408061129     c     *like         define    WFTIR3        totIR3
119409061129     c     *like         define    WFTIC3        totIC3
119410061129     C*
119500000825     C* DEFINIZIONI VARIABILI DI LAVORO
119700000000     C                   MOVEL     '0'           $FINE             1            *FINE PROGRAMMA
119800980601     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
119801061129     C                   Z-ADD     *ZEROS        P1                4 0          *PER FASCE DI PESO
119802061129     C                   Z-ADD     *ZEROS        P2                4 0          *PER FASCE DI PESO
119803061129     C                   Z-ADD     *ZEROS        P3                4 0          *PER FASCE DI PESO
120100000000     C                   Z-ADD     *ZEROS        C                 4 0          *PER COMMERCIALI
120200000000     C                   Z-ADD     *ZEROS        G                 4 0          *PER FLAG TIPI BOLLA
120300000000     C                   Z-ADD     *ZEROS        J                 4 0          *PER MESI ELABORABIL
120400000000     C                   Z-ADD     *ZEROS        F                 4 0          *PER ORGANIGRAMMA
120600970821     C                   Z-ADD     *ZEROS        A                 4 0          *PER ANNI
120700970821     C                   Z-ADD     *ZEROS        M                 4 0          *PER MESI
120800020404     C                   Z-ADD     *ZEROS        X                 3 0          *PER RAGGRUPPAMENTI
120900020404     C                   Z-ADD     *ZEROS        R                 3 0          *PER NETWORK
121000000000     C                   SETON                                        OF        *OVERFLOW
121500000000     C*
121600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
121700000000     C     KEYTAB        KLIST
121800000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
121900000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
122000000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
122100000000     C*
122200000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
122300000000     C     KEYTA2        KLIST
122400000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
122500000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
122600020404     C*
122700020404     C* CHIAVE LETTURA TNTBE00F - PARZIALE
122800020404     C     KEYTBE        KLIST
122900020404     C                   KFLD                    TBECOD
123000000000     C*
123100990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
123200000000     C     KEYSDC        KLIST
123300000000     C                   KFLD                    KSCKSC                         *CODICE CLIENTE
123400000000     C                   KFLD                    KSCANN                         *ANNO
123500000000     C                   KFLD                    KSCMES                         *MESE
123600000000     C*
123700990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
123800000000     C     KEYSDO        KLIST
123900000000     C                   KFLD                    KSOKSC                         *CODICE CLIENTE
124600000000     C*
124700000000     C* CHIAVE LETTURA WFDEL01L - COMPLETA
124800000000     C     KEYDEL        KLIST
124801061129     C                   KFLD                    KWDANN                         *ANNO
124802061129     C                   KFLD                    KWDMES                         *MESE
124803061129     C                   KFLD                    KWDREP                         *Reg.Partenza
124804061129     C                   KFLD                    KWDREA                         *Reg.Arrivo
125300000000     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
125301061129     C                   KFLD                    KWDCLI                         *CLIENTE
126900000000     C*
127000000825     C* CARICA TABELLE OCCORRENTI
127100000000     C                   EXSR      CARTAB
127200970821     C*
127300970821     C* CALCOLA I MESI DA ELABORARE
127400990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
127500970821     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
127600990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
127700970821     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
127800970821     C                   Z-ADD     *ZEROS        JI                2 0          *N° MESI DA ELABORAR
127900970821     C                   EXSR      AMELAB
128500001122     C*
128600001122     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
128700010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
128800010725     C                   MOVE      28            WDFIN
128900000000     C*
129000000000     C                   ENDSR
