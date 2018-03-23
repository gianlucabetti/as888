000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICHE FASCE DI RICAVO/COLLI E BANCALI - STAMPA                   *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTABEL00F  IF   E           K DISK
000900020117     FTNTBE01L  IF   E           K DISK
001000080717     FWfsba00f  o    e             disk    usropn
001100000000     F*------------
001200000000     F* PRINTER FILE
001300000000     F*------------
001400010510     FTISE05P   O    E             PRINTER OFLIND(*IN01)
001500000000     F                                     USROPN
001600010510     FTISE06P   O    E             PRINTER OFLIND(*IN02)
001700000000     F                                     USROPN
001800091016     FTISE07P   O    E             PRINTER OFLIND(*IN03)
001900091016     F                                     USROPN
002000000000     F*------------
002100990806     F* BOLLE
002200000000     F*------------
002300990805     FTITAS31C  IF   E           K DISK
002400990805     F                                     IGNORE(TITAS000)
002500990806     F*------------
002600990824     F* ESTENSIONE BOLLE
002700990806     F*------------
002800990805     FTITA730C  IF   E           K DISK
002900990806     F*------------
003000990806     F* ANAGRAFICHE BOLLE
003100990806     F*------------
003200990806     FTITAA30C  IF   E           K DISK
003300990806     F                                     IGNORE(TITAAP00)
003400000000     F*------------
003500000000     F* COMPETENZE FISSE E VARIABILI
003600000000     F*------------
003700000000     FECCET30C  IF   E           K DISK
003800000000     FECCED30C  IF   E           K DISK
003900971204     F*------------
004000971204     F* ORGANIGRAMMA
004100971204     F*------------
004200971204     FAZORG01L  IF   E           K DISK
004300000000     D*------------------------------------------------------------------------*
004400000000     D* SCHIERE
004500000000     D*------------------------------------------------------------------------*
004600000000     D*------------------
004700000000     D* DECODIFICA RIGHE DI STAMPA
004800000000     D*------------------
004900000000     D DRI             S             15    DIM(51) CTDATA PERRCD(1)
005000000000     D TCO             S             15    DIM(5) CTDATA PERRCD(1)
005100000000     D DCO             S             15    DIM(7) CTDATA PERRCD(1)
005200000000     D*------------------
005300000000     D* TIPI BOLLA
005400000000     D*------------------
005500000000     D TBL             S              2    DIM(20)
005600000000     D RBL             S              1    DIM(20)
005700000000     D*------------------
005800000000     D* VARIE
005900000000     D*------------------
006000990806     D SVN             S              1    DIM(50)
006100010510     D VAN             S             17  5 DIM(50)
006200971204     D*------------------
006300000614     D* ORGANIGRAMMA BARTOLINI
006400971204     D*------------------
006500971204     D FIL             S              3  0 DIM(999)
006600971204     D FL1             S              1    DIM(999)
006700000614     D*------------------
006800000614     D* ORGANIGRAMMA POSTE
006900000614     D*------------------
007000000619     D PFIL            S              3  0 DIM(999)
007100000614     D PFL1            S              1    DIM(999)
007200000000     D*------------------
007300000000     D* SCHIERE DI MEMORIZZAZIONE -FASCE DI RICAVO-
007400000000     D*------------------
007500000000     D* CON ACCESSORI
007600000000     D CNSP            S             13  0 DIM(51)
007700000000     D CNCL            S             13  0 DIM(51)
007800990806     D CPKF            S             13  1 DIM(51)
007900000000     D CVLF            S             13  3 DIM(51)
008000010510     D CRIC            S             17  5 DIM(51)
008100010510     D CCOM            S             17  5 DIM(51)
008200000000     D* SENZA ACCESSORI
008300000000     D SNSP            S             13  0 DIM(51)
008400000000     D SNCL            S             13  0 DIM(51)
008500990806     D SPKF            S             13  1 DIM(51)
008600000000     D SVLF            S             13  3 DIM(51)
008700010510     D SRIC            S             17  5 DIM(51)
008800010510     D SCOM            S             17  5 DIM(51)
008900000000     D*------------------
009000000000     D* SCHIERE DI MEMORIZZAZIONE -FASCE DI COLLI-
009100000000     D*------------------
009200000000     D***
009300000000     D* CLIENTE
009400000000     D***
009500000000     D PNSP            S             13  0 DIM(7)
009600000000     D PNCL            S             13  0 DIM(7)
009700990806     D PPKF            S             13  1 DIM(7)
009800991126     D PVLF            S             13  3 DIM(7)
009900010510     D PPOR            S             17  5 DIM(7)
010000010510     D PACC            S             17  5 DIM(7)
010100010510     D PCOM            S             17  5 DIM(7)
010200000000     D***
010300000000     D*  TOTALI                                          - 4 PERCENTUALI -
010400000000     D***
010500000000     D* PRIMO GRUPPO
010600000000     D* RIMANENZA
010700010510     D RAC             S             15  3 DIM(7)
010800000000     D*------------------------------------------------------------------------*
010900000000     D* INPUT
011000000000     D*------------------------------------------------------------------------*
011100041227     d dataiso         s               d   datfmt(*iso)
011200041227     d depmes          s              2  0
011300080717
011400080717     d comman          s            110
011500080717     d lenght          s             15  5 inz(110)
011600080717     d wlib            s              9
011700000000     D*------------------
011800991126     D* DS MULTIPLA TOTALI STATISTICA FASCE DI COLLI      - 4 PERCENTUALI-
011900000000     D*------------------
012000000000     D* (13:2)+1 = 7,5 ; (7 X 7) = 49
012100010510     D GPER            DS                  OCCURS(4) INZ
012200010510     D  CLI                           9  0 DIM(7)
012300010510     D  PNS                           9  0 DIM(7)
012400010510     D  PNC                           9  0 DIM(7)
012500010510     D  PPK                           9  1 DIM(7)
012600010510     D  PVL                           9  3 DIM(7)
012700010510     D  PPO                          15  3 DIM(7)
012800010510     D  PAC                           9  3 DIM(7)
012900010510     D  PCO                          15  3 DIM(7)
013000010510     D  RNS                           9  0 DIM(7)
013100010510     D  RNC                           9  0 DIM(7)
013200010510     D  RPK                           9  1 DIM(7)
013300010510     D  RVL                           9  3 DIM(7)
013400010510     D  RPO                          15  3 DIM(7)
013500010510     D  RCO                          15  3 DIM(7)
013600991126     D*-------------------
013700991126     D* CAMBIO DIVISA
013800991126     D*-------------------
013900991126     D  YEUR         E DS                  extname(YEURCODS)
014000020117     D*------------------
014100020117     D* DS LETTURA TABELLA "GED" TIPO RECORD "ST"
014200020117     D*------------------
014300020117     D DGEDST        E DS
014400000000     D*------------------
014500000000     D* ARCHITETTURA
014600000000     D*------------------
014700000000     D KPJBA         E DS
014800000000     D  PROFIL                 1     10
014900000000     D*------------------
015000000000     D* DS COMPOSIZIONE DATA SPEDIZIONE
015100000000     D*------------------
015200000000     D                 DS
015300000000     D  TASAAS                 1      4  0
015400000000     D  TASMGS                 5      8  0
015500000000     D  TASMES                 5      6  0
015600000000     D  TASDAT                 1      6  0
015700000000     D*------------------
015800000000     D* DS COMPOSIZIONE DATA FATTURA
015900000000     D*------------------
016000000000     D                 DS
016100000000     D  TA2MES                 5      6  0
016200000000     D  TASDFT                 1      8  0
016300000000     D*------------------
016400000000     D* DS PARAMETRI LANCIO
016500000000     D*------------------
016600000000     D PARAM           DS
016700000000     D  PARAAI                 1      4  0
016800000000     D  PARMMI                 5      6  0
016900000000     D  PARDAI                 1      6  0
017000000000     D  PARAAF                 7     10  0
017100000000     D  PARMMF                11     12  0
017200000000     D  PARDAF                 7     12  0
017300000000     D  PARRIC                13     13
017400000000     D  PARCOL                14     14
017500000000     D  PARBAN                15     15
017600000614     D  PARPT                 16     16
017700080717     D  PARFIL                17     17
017800000000     D*------------------
017900000000     D* DS TIPI BOLLA
018000000000     D*------------------
018100000000     D DSTB          E DS
018200000000     D*------------------
018300000000     D* DS CAMBI VALUTA
018400000000     D*------------------
018500000000     D DSCV          E DS
018600980116     D*------------------
018700980116     D* DS REPERIMENTO DATI UTENTE
018800980116     D*-------------------
018900980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
019000980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
019100980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
019200980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
019300980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
019400000000     D*------------------
019500000000     D* DEFINIZIONE COSTANTI
019600000000     D*------------------
019700000000     D DESCON          C                   CONST('CON   ACCESSORI')
019800000000     D DESSEN          C                   CONST('SENZA ACCESSORI')
019900000614     D*------------------
020000000614     D* DS PER LETTURA 143 DESCRIZIONE ORGANIGRAMMA (X POSTE)
020100000614     D*------------------
020200000614     D OG143         E DS
020300020702     D*------------------
020400020702     D* DS PER LETTURA TABELLA "NTW"
020500020702     D*------------------
020600020702     D DNTW          E DS
020700000614     D*------------------
020800000614     D* DS X SCOMPOSIZIONE CODICE FILIALE/CLIENTE
020900000614     D*------------------
021000000614     D                 DS
021100000614     D  KSCFIL                 1      3  0
021200000614     D  KSCKSC                 4      7  0
021300000614     D  KSCCOD                 1      7  0
021400020117     D*-------------------
021500020117     D* DS "XSRDA8" - CONTROLLO DATA (8)
021600020117     D*-------------------
021700020117     D WLBDA8          DS                  INZ
021800020117     D  G08DAT                 1      8  0
021900020117     D  G08INV                 9     16  0
022000020117     D  G08ERR                17     17
022100020117     D  G08TGI                18     22  0
022200041227      *-------------------
022300041227      * DS "TISE70DS" - REPERIMENTO SALDI CLIENTI
022400041227      *-------------------
022500041227     d Tise70ds      e ds
022600041227      *-------------------
022700041227      * DS 'wdataf' x aaaamm da passare al tise70ds
022800041227      *-------------------
022900041227     d                 ds
023000041227     d  wdataf                 1      8  0
023100041227     d  waaf                   1      4  0
023200041227     d  wmmf                   5      6  0
023300041227     d  waammf                 1      6  0
023400041227     d  wggf                   7      8  0
023500041227      *-------------------
023600041227      * DS 'waammi' x aaaamm da passare al tise70ds
023700041227      *-------------------
023800041227     d                 ds
023900041227     d  waammi                 1      6  0
024000041227     d  waai                   1      4  0
024100041227     d  wmmi                   5      6  0
024200000000     C*------------------------------------------------------------------------*
024300020117     C* MAIN LINE
024400020117     C*------------------------------------------------------------------------*
024500000000     C*
024600000000     C* APRE I FILE PRINTER DESIDERATI
024700000000     C                   EXSR      OPNPRT
024800000000     C*
024900000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
025000000000     C                   EXSR      SETFIL
025100000000     C*
025200000000     C* CICLO FINO A FINE FILE
025300000000DO  1C     $FINE         DOWEQ     '0'
025400000000     C*
025500000000     C* OPERAZIONI PER NUOVO CLIENTE
025600000000     C                   EXSR      NEWKSC
025700000000     C*
025800000000     C* CICLO FINO A ROTTURA CLIENTE
025900000000DO  2C     $FINE         DOWEQ     '0'
026000000000     C     TASKSC        ANDEQ     DEPKSC
026100000000     C*
026200000000     C* OPERAZIONI PER NUOVO MESE
026300000000     C                   EXSR      NEWMES
026400000000     C*
026500000000     C* CICLO FINO A ROTTURA MESE
026600000000DO  3C     $FINE         DOWEQ     '0'
026700000000     C     TASKSC        ANDEQ     DEPKSC
026800000000     C     TA2MES        ANDEQ     DEPMES
026900000000     C*
027000000000     C* MEMORIZZA I DATI DELLE BOLLE
027100000000     C                   EXSR      MEMSAL
027200000000     C*
027300000000     C* STAMPA LA STATISTICA BANCALI (SE RICHIESTA)
027400000000IF  4C     PARBAN        IFEQ      '1'
027500000000     C                   EXSR      STABAN
027600000000E   4C                   ENDIF
027700000000     C*
027800000000     C* LEGGE IL RECORD SUCCESSIVO
027900000000     C                   EXSR      LETFIL
028000000000E   3C                   ENDDO
028100000000E   2C                   ENDDO
028200000000     C*
028300000000     C* MEMORIZZA I DATI TOTALI (ST.FASCE DI COLLI)
028400000000     C                   EXSR      MEMCOL
028500000000E   1C                   ENDDO
028600000000     C*
028700000000     C* STAMPA LA STATISTICA FASCE DI RICAVO (SE RICHIESTA)
028800000000IF  1C     PARRIC        IFEQ      '1'
028900000000     C                   EXSR      STARIC
029000000000E   1C                   ENDIF
029100000000     C*
029200000000     C* STAMPA LA STATISTICA FASCE DI COLLI (SE RICHIESTA)
029300000000IF  1C     PARCOL        IFEQ      '1'
029400000000     C                   EXSR      STACOL
029500000000E   1C                   ENDIF
029600000000     C*
029700980116     C* OPERAZIONI FINALI
029800980116     C                   EXSR      FINSR
029900000000     C*
030000000000     C                   SETON                                        LR
030100000000     C*------------------------------------------------------------------------*
030200000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
030300000000     C*------------------------------------------------------------------------*
030400000000     C     SETFIL        BEGSR
030500991130     C*
030600991130     C     *LOVAL        SETLL     TITAS31C                           99
030700000000     C*
030800000000IF  1C     *IN99         IFEQ      *ON
030900000000     C                   MOVEL     '1'           $FINE
031000000000X   1C                   ELSE
031100000000     C                   EXSR      LETFIL
031200000000E   1C                   ENDIF
031300000000     C*
031400000000     C                   ENDSR
031500000000     C*------------------------------------------------------------------------*
031600000000     C* LETFIL - LETTURA PROSSIMO RECORD
031700000000     C*------------------------------------------------------------------------*
031800000000     C     LETFIL        BEGSR
031900000000     C*
032000000000     C* LEGGE FINO A:
032100000000     C*     - FINE FILE
032200000000     C*     - TROVATO RECORD VALIDO
032300000000     C                   MOVEL     'N'           WRECOK
032400000000DO  1C     $FINE         DOWEQ     '0'
032500000000     C     WRECOK        ANDEQ     'N'
032600000000     C*
032700990805     C                   READ      TITAS31C                               99
032800000000     C*
032900000000IF  2C     *IN99         IFEQ      *ON
033000000000     C                   MOVEL     '1'           $FINE
033100000000X   2C                   ELSE
033200991126     C*
033300991126     C                   EXSR      CHKREC
033400000000E   2C                   ENDIF
033500000000E   1C                   ENDDO
033600000000     C*
033700000000     C                   ENDSR
033800000000     C*------------------------------------------------------------------------*
033900000621     C* CHKREC - CONTROLLA VALIDITA' RECORD (CONTROLLI SU TITAS)
034000000000     C*------------------------------------------------------------------------*
034100000000     C     CHKREC        BEGSR
034200000000     C*
034300000000     C                   MOVEL     'S'           WRECOK
034400000619     C*
034500000621     C* VERIFICO LA FILIALE CORRENTE RISPETTO AI PARAMETRI DI LANCIO:
034600000621     C* SE SOLO POSTE O SOLO BARTOLINI
034700000621     C*
034800000621     C* 1° CONTROLLO ==> LINEE ARRIVO/PARTENZA
034900000621     C     WRECOK        IFEQ      'S'
035000000621     C     PARPT         IFEQ      'S'
035100000621     C     TASLNA        LOOKUP    PFIL                                   45
035200000621     C     TASLNP        LOOKUP    PFIL                                   55
035300000621     C                   IF        *IN45=*ON OR *IN55=*ON
035400000621     C                   ELSE
035500000621     C                   MOVEL     'N'           WRECOK
035600000621     C                   ENDIF
035700000621     C                   ELSE
035800000621     C     TASLNA        LOOKUP    FIL                                    45
035900000621     C     TASLNP        LOOKUP    FIL                                    55
036000000621     C                   IF        *IN45=*ON AND *IN55=*ON
036100000621     C                   ELSE
036200000621     C                   MOVEL     'N'           WRECOK
036300000621     C                   ENDIF
036400000621     C                   ENDIF
036500000621     C                   ENDIF
036600000621     C*
036700000621     C* 2° CONTROLLO ==> CODICE CLIENTE
036800000621     C     WRECOK        IFEQ      'S'
036900000621     C                   Z-ADD     TASKSC        KSCCOD
037000000621     C     PARPT         IFEQ      'S'
037100000621     C     KSCFIL        LOOKUP    PFIL                                   45
037200000621     C                   IF        *IN45=*ON
037300000621     C                   ELSE
037400000621     C                   MOVEL     'N'           WRECOK
037500000621     C                   ENDIF
037600000621     C                   ELSE
037700000621     C     KSCFIL        LOOKUP    FIL                                    45
037800000621     C                   IF        *IN45=*ON
037900000621     C                   ELSE
038000000621     C                   MOVEL     'N'           WRECOK
038100000621     C                   ENDIF
038200000621     C                   ENDIF
038300000621     C                   ENDIF
038400000000     C*
038500000000     C* PERIODO NON RICHIESTO, ESCLUDE
038600000614IF  0C     WRECOK        IFEQ      'S'
038700000000IF  1C     TASDAT        IFGE      PARDAI
038800000000     C     TASDAT        ANDLE     PARDAF
038900000000X   1C                   ELSE
039000000000     C                   MOVEL     'N'           WRECOK
039100000000E   1C                   ENDIF
039200000614E   0C                   ENDIF
039300000000     C*
039400000000     C* TIPO BOLLA DIVERSO DA "NORMALE", ESCLUDE
039500000000IF  1C     WRECOK        IFEQ      'S'
039600000000     C                   Z-ADD     1             I
039700000000     C                   SETOFF                                           28
039800000000     C     TASTBL        LOOKUP    TBL(I)                                 28
039900000000IF  2C     *IN28         IFEQ      *ON
040000000000     C     RBL(I)        ANDEQ     'N'
040100000000X   2C                   ELSE
040200000000     C                   MOVEL     'N'           WRECOK
040300000000E   2C                   ENDIF
040400000000E   1C                   ENDIF
040500000000     C*
040600000000     C                   ENDSR
040700000000     C*------------------------------------------------------------------------*
040800000000     C* MEMSAL - MEMORIZZA I DATI
040900000000     C*------------------------------------------------------------------------*
041000000000     C     MEMSAL        BEGSR
041100000000     C*
041200000000     C* PULISCE LE VARIABILI DI LAVORO
041300000000     C                   CLEAR                   SVN
041400000000     C                   CLEAR                   VAN
041500000000     C*
041600000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
041700000000     C                   MOVEL     '1'           PASGEN
041800000000     C****
041900000000     C* REPERISCE I RICAVI E LE COMPETENZE
042000000000     C****
042100000000     C* CARICA IN SCHIERA LE VARIE DEL RECORD BOLLA
042200000000     C                   Z-ADD     *ZEROS        J                 4 0
042300000000IF  1C     TASSV1        IFNE      *BLANKS
042400000000     C                   ADD       1             J
042500010510     C                   MOVEL     TASSV1        SVN(J)
042600000111     C                   Z-ADD     TASVA1        VAN(J)
042700000000E   1C                   ENDIF
042800000000IF  1C     TASSV2        IFNE      *BLANKS
042900000000     C                   ADD       1             J
043000000000     C                   MOVEL     TASSV2        SVN(J)
043100000111     C                   Z-ADD     TASVA2        VAN(J)
043200000000E   1C                   ENDIF
043300000000IF  1C     TASSV3        IFNE      *BLANK
043400000000     C                   ADD       1             J
043500000000     C                   MOVEL     TASSV3        SVN(J)
043600000111     C                   Z-ADD     TASVA3        VAN(J)
043700000000E   1C                   ENDIF
043800000000     C*
043900000000     C* CARICA IN SCHIERA LE VARIE DEI RECORD ESTENSIONE BOLLA
044000000000IF  1C     TASSV3        IFNE      *BLANKS
044100000000     C                   Z-ADD     TASAAS        KT7AAS
044200000000     C                   Z-ADD     TASLNP        KT7LNP
044300000000     C                   Z-ADD     TASNRS        KT7NRS
044400000000     C                   Z-ADD     TASNSP        KT7NSP
044500000000     C                   MOVEL     TASTBL        KT7TBL
044600990805     C     KEYTA7        SETLL     TITA730C                           98
044700990805     C     KEYTA7        READE     TITA730C                               98
044800000000DO  2C     *IN98         DOWEQ     *OFF
044900000000     C                   ADD       1             J
045000000000     C                   MOVEL     TA7SVN        SVN(J)
045100000111     C                   Z-ADD     TA7VAN        VAN(J)
045200990805     C     KEYTA7        READE     TITA730C                               98
045300000000E   2C                   ENDDO
045400000000E   1C                   ENDIF
045500000111     c*
045600000111     c* Se la divisa è italia le bolle franco import o assegnato export hanno i
045700000111     c* ricavi espressi in Ecu x 100, alrimenti sono in lire;
045800000111     c* Se la divisa è estera applica il relativo cambio
045900000111IF  0C     TASDIV        IFEQ      *BLANKS
046000000111     C     TASDIV        OREQ      'ITL'
046100000000     C                   MOVEL     TASTBL        WTPO              1
046200000000     C                   MOVEL     'N'           WFLCMB            1
046300971204     C                   MOVEL     *BLANKS       DLPFL1
046400971204     C                   MOVEL     *BLANKS       DLAFL1
046500971204     C                   Z-ADD     1             J
046600971204     C                   SETOFF                                           28
046700971204     C     TASLNP        LOOKUP    FIL(J)                                 28
046800971204IF  1C     *IN28         IFEQ      *ON
046900971204     C                   MOVEL     FL1(J)        DLPFL1
047000971204E   1C                   ENDIF
047100971204     C                   Z-ADD     1             J
047200971204     C                   SETOFF                                           28
047300971204     C     TASLNA        LOOKUP    FIL(J)                                 28
047400971204IF  1C     *IN28         IFEQ      *ON
047500971204     C                   MOVEL     FL1(J)        DLAFL1
047600971204E   1C                   ENDIF
047700971204IF  1C     DLPFL1        IFNE      *BLANKS
047800971204     C     WTPO          ANDEQ     'F'
047900000000     C                   MOVEL     'S'           WFLCMB
048000000000E   1C                   ENDIF
048100971204IF  1C     DLAFL1        IFNE      *BLANKS
048200971204     C     WTPO          ANDEQ     'A'
048300000000     C                   MOVEL     'S'           WFLCMB
048400000000E   1C                   ENDIF
048500000111     c* Applica il cambio
048600000000IF  1C     WFLCMB        IFEQ      'S'
048700010510     C                   Z-ADD     TASPOR        N9               17 5
048800020117     C     N9            DIV       100           COMIMP
048900020117     C                   MOVEL     'EUR'         COMDIV
049000020117     C                   EXSR      CNVEUR
049100020117     C                   Z-ADD     COMIMP        CNVPOR           15 3
049200990806DO  2C     1             DO        50            J
049300000000IF  3C     SVN(J)        IFNE      *BLANKS
049400010510     C                   Z-ADD     VAN(J)        N9
049500020117     C     N9            DIV       100           COMIMP
049600020117     C                   MOVEL     'EUR'         COMDIV
049700020117     C                   EXSR      CNVEUR
049800020117     C                   Z-ADD     COMIMP        VAN(J)
049900000000E   3C                   ENDIF
050000000000E   2C                   ENDDO
050100041103     c* lo faccio anche per il totale imponibile
050200041103     C                   Z-ADD     TASimv        N9               17 5
050300041103     C     N9            DIV       100           COMIMP
050400041103     C                   MOVEL     'EUR'         COMDIV
050500041103     C                   EXSR      CNVEUR
050600041103     C                   Z-ADD     COMIMP        CNVimv           15 3
050700000217     c* Non applica il cambio
050800000217X   1C                   ELSE
050900010510     C                   Z-ADD     TASPOR        CNVPOR           15 3
051000041103     C                   Z-ADD     TASimv        CNVimv           15 3
051100000000E   1C                   ENDIF
051200000111     C*
051300000111     C* DIVISA ESTERA: APPLICA IL RELATIVO CAMBIO
051400000111X   0C                   ELSE
051500000111     C                   Z-ADD     TASPOR        COMIMP
051600000111     C                   MOVEL     TASDIV        COMDIV
051700000111     C                   EXSR      CNVEUR
051800010510     C                   Z-ADD     COMIMP        CNVPOR
051900000111DO  1C     1             DO        50            J
052000000111IF  2C     SVN(J)        IFNE      *BLANKS
052100000111     C                   Z-ADD     VAN(J)        COMIMP
052200000111     C                   MOVEL     TASDIV        COMDIV
052300000111     C                   EXSR      CNVEUR
052400000111     C                   Z-ADD     COMIMP        VAN(J)
052500000111E   2C                   ENDIF
052600000111E   1C                   ENDDO
052700041103     c
052800041103     C                   Z-ADD     TASimv        COMIMP
052900041103     C                   MOVEL     TASDIV        COMDIV
053000041103     C                   EXSR      CNVEUR
053100041103     C                   Z-ADD     COMIMP        CNVimv
053200000111E   0C                   ENDIF
053300990806     C*
053400000000     C* CALCOLA LE COMPETENZE
053500010510     C                   Z-ADD     *ZEROS        WCOM             17 5
053600000000     C                   Z-ADD     TASAAS        KCTAAS
053700000000     C                   Z-ADD     TASLNP        KCTLNP
053800000000     C                   Z-ADD     TASNRS        KCTNRS
053900000000     C                   Z-ADD     TASNSP        KCTNSP
054000000000     C                   MOVEL     TASTBL        KCTTBL
054100000000     C     KEYCET        CHAIN     ECCET30C                           98
054200000000IF  1C     *IN98         IFEQ      *OFF
054300000713     C                   ADD       CETPAR        WCOM
054400000713     C                   ADD       CETPAM        WCOM
054500000713     C                   ADD       CETPAU        WCOM
054600000713     C                   ADD       CETPAV        WCOM
054700000713     C                   ADD       CETARD        WCOM
054800000713     C                   ADD       CETARM        WCOM
054900000713     C                   ADD       CETARU        WCOM
055000000713     C                   ADD       CETARV        WCOM
055100000713     C                   ADD       CETTRL        WCOM
055200000713     C                   ADD       CETPAG        WCOM
055300000713     C                   ADD       CETSEA        WCOM
055400000713     C                   ADD       CETSEC        WCOM
055500000713     C                   ADD       CETRCV        WCOM
055600000713     C                   ADD       CETMDE        WCOM
055700000713     C                   ADD       CETTDE        WCOM
055800000713     C                   ADD       CETMAF        WCOM
055900000713     C                   ADD       CETTAF        WCOM
056000010829     C                   ADD       CETSES        WCOM
056100000000IF  2C     CETDET        IFEQ      'S'
056200000000     C     KEYCET        SETLL     ECCED30C                           98
056300000000     C     KEYCET        READE     ECCED30C                               98
056400000000DO  3C     *IN98         DOWEQ     *OFF
056500000713     C                   ADD       CEDIMP        WCOM
056600000000     C     KEYCET        READE     ECCED30C                               98
056700000000E   3C                   ENDDO
056800000000E   2C                   ENDIF
056900000000E   1C                   ENDIF
057000000000     C***
057100000000     C* CALCOLA IL RICAVI DA TRASPORTO
057200000000     C***
057300010510     C                   Z-ADD     *ZEROS        WPOR             17 3
057400991126     C                   ADD       CNVPOR        WPOR
057500990806DO  1C     1             DO        50            J
057600000000IF  2C     SVN(J)        IFNE      *BLANKS
057700000000IF  3C     SVN(J)        IFNE      'E'
057800000000     C     SVN(J)        ANDNE     'R'
057900000000     C     SVN(J)        ANDNE     'G'
058000000000     C     SVN(J)        ANDNE     'I'
058100990806     C     SVN(J)        ANDNE     '6'
058200990806     C     SVN(J)        ANDNE     '8'
058300000000     C                   ADD       VAN(J)        WPOR
058400000000E   3C                   ENDIF
058500000000E   2C                   ENDIF
058600000000E   1C                   ENDDO
058700000000     C***
058800000000     C* CALCOLA IL RICAVO ACCESSORIO
058900000000     C***
059000010510     C                   Z-ADD     *ZEROS        WACC             17 5
059100990806DO  1C     1             DO        50            J
059200000000IF  2C     SVN(J)        IFNE      *BLANKS
059300000000IF  3C     SVN(J)        IFEQ      'E'
059400000000     C     SVN(J)        OREQ      'R'
059500000000     C     SVN(J)        OREQ      'G'
059600000000     C     SVN(J)        OREQ      'I'
059700000000     C                   ADD       VAN(J)        WACC
059800000000E   3C                   ENDIF
059900000000E   2C                   ENDIF
060000000000E   1C                   ENDDO
060100000000     C***
060200000000     C* MEMORIZZA I DATI PER LA STATISTICA FASCE DI RICAVO
060300000000     C***
060400000000     C                   EXSR      MEMRIC
060500000000     C***
060600000000     C* MEMORIZZA I DATI PER LA STATISTICA FASCE DI COLLI
060700000000     C***
060800000000     C                   EXSR      MEMKSC
060900000000     C*
061000000000     C                   ENDSR
061100000000     C*------------------------------------------------------------------------*
061200000000     C* MEMRIC - MEMORIZZA I DATI PER LA STATISTICA FASCE DI RICAVO
061300000000     C*------------------------------------------------------------------------*
061400000000     C     MEMRIC        BEGSR
061500000000     C*
061600000000     C* CONTROLLA IN QUALI FASCE DI RICAVO SI POSIZIONA LA SPEDIZIONE
061700000000     C***
061800000000     C* RICAVO = TRASPORTO
061900000000     C***
062000010510     C                   Z-ADD     WPOR          RICAVO           17 5
062100010510     C                   Z-ADD     WPOR          RICTOT           17 5
062200000000IF  1C     RICAVO        IFGT      *ZEROS
062300000000     C     TASNCL        ANDGT     *ZEROS
062400000000     C     RICAVO        DIV(H)    TASNCL        RICAVO
062500000000     C                   Z-ADD     *ZEROS        F                 2 0
062600000000     C                   EXSR      CALFSR
062700000000     C                   ADD       1             SNSP(F)
062800000000     C                   ADD       TASNCL        SNCL(F)
062900990806     C                   ADD       TASPKF        SPKF(F)
063000000000     C                   ADD       TASVLF        SVLF(F)
063100000000     C                   ADD       RICTOT        SRIC(F)
063200000000     C                   ADD       WCOM          SCOM(F)
063300000000E   1C                   ENDIF
063400000000     C***
063500000000     C* RICAVO = TRASPORTO + ACCESSORI
063600000000     C***
063700000000     C     WPOR          ADD       WACC          RICAVO
063800000000     C     WPOR          ADD       WACC          RICTOT
063900000000IF  1C     RICAVO        IFGT      *ZEROS
064000000000     C     TASNCL        ANDGT     *ZEROS
064100000000     C     RICAVO        DIV(H)    TASNCL        RICAVO
064200000000     C                   Z-ADD     *ZEROS        F
064300000000     C                   EXSR      CALFSR
064400000000     C                   ADD       1             CNSP(F)
064500000000     C                   ADD       TASNCL        CNCL(F)
064600990806     C                   ADD       TASPKF        CPKF(F)
064700000000     C                   ADD       TASVLF        CVLF(F)
064800000000     C                   ADD       RICTOT        CRIC(F)
064900000000     C                   ADD       WCOM          CCOM(F)
065000000000E   1C                   ENDIF
065100000000     C*
065200000000     C                   ENDSR
065300000000     C*------------------------------------------------------------------------*
065400000000     C* MEMKSC - MEMORIZZA I DATI PER LA STATISTICA FASCE DI COLLI X CLIENTE
065500000000     C*------------------------------------------------------------------------*
065600000000     C     MEMKSC        BEGSR
065700000000     C*
065800000000     C* SPEDIZIONI TOTALI PER CLIENTE
065900000000     C                   ADD       1             TOKNSP
066000000000     C*
066100000000     C* CONTROLLA IN QUALI FASCE DI COLLI SI POSIZIONA LA SPEDIZIOE
066200000000     C                   Z-ADD     TASNCL        COLLI            13 0
066300000000IF  1C     COLLI         IFGT      *ZEROS
066400000000     C                   Z-ADD     *ZEROS        F
066500000000     C                   EXSR      CALFSC
066600000000DO  2C     F             DOWLE     7
066700000000     C                   ADD       1             PNSP(F)
066800000000     C                   ADD       TASNCL        PNCL(F)
066900990806     C                   ADD       TASPKF        PPKF(F)
067000000000     C                   ADD       TASVLF        PVLF(F)
067100000000     C                   ADD       WPOR          PPOR(F)
067200000000     C                   ADD       WACC          PACC(F)
067300000000     C                   ADD       WCOM          PCOM(F)
067400000000     C                   ADD       1             F
067500000000E   2C                   ENDDO
067600000000E   1C                   ENDIF
067700000000     C*
067800000000     C                   ENDSR
067900000000     C*------------------------------------------------------------------------*
068000000000     C* MEMCOL - MEMORIZZA I DATI PER LA STATISTICA FASCE DI COLLI
068100000000     C*------------------------------------------------------------------------*
068200000000     C     MEMCOL        BEGSR
068300000000     C*
068400000000     C* CALCOLA LA PERCENTUALE DI INCIDENZA PER CIASCUNA FASCIA MEMORIZZATA
068500000000IF  1C     TOKNSP        IFGT      *ZEROS
068600000000DO  2C     1             DO        7             I
068700000000     C     PNSP(I)       MULT      100           WN150            15 0
068800000000     C     WN150         DIV       TOKNSP        WN051             5 1
068900000000     C*** 60% ***
069000000000     C     1             OCCUR     GPER
069100000000IF  3C     WN051         IFGE      60
069200000000     C                   ADD       1             CLI(I)
069300000000     C                   ADD       PNSP(I)       PNS(I)
069400000000     C                   ADD       PNCL(I)       PNC(I)
069500990806     C                   ADD       PPKF(I)       PPK(I)
069600000000     C                   ADD       PVLF(I)       PVL(I)
069700000000     C                   ADD       PPOR(I)       PPO(I)
069800000000     C                   ADD       PACC(I)       PAC(I)
069900000000     C                   ADD       PCOM(I)       PCO(I)
070000000000X   3C                   ELSE
070100000000     C                   ADD       PNSP(I)       RNS(I)
070200000000     C                   ADD       PNCL(I)       RNC(I)
070300990806     C                   ADD       PPKF(I)       RPK(I)
070400000000     C                   ADD       PVLF(I)       RVL(I)
070500000000     C                   ADD       PPOR(I)       RPO(I)
070600000000     C                   ADD       PACC(I)       RAC(I)
070700000000     C                   ADD       PCOM(I)       RCO(I)
070800000000E   3C                   ENDIF
070900000000     C*** 70% ***
071000000000     C     2             OCCUR     GPER
071100000000IF  3C     WN051         IFGE      70
071200000000     C                   ADD       1             CLI(I)
071300000000     C                   ADD       PNSP(I)       PNS(I)
071400000000     C                   ADD       PNCL(I)       PNC(I)
071500990806     C                   ADD       PPKF(I)       PPK(I)
071600000000     C                   ADD       PVLF(I)       PVL(I)
071700000000     C                   ADD       PPOR(I)       PPO(I)
071800000000     C                   ADD       PACC(I)       PAC(I)
071900000000     C                   ADD       PCOM(I)       PCO(I)
072000000000X   3C                   ELSE
072100000000     C                   ADD       PNSP(I)       RNS(I)
072200000000     C                   ADD       PNCL(I)       RNC(I)
072300990806     C                   ADD       PPKF(I)       RPK(I)
072400000000     C                   ADD       PVLF(I)       RVL(I)
072500000000     C                   ADD       PPOR(I)       RPO(I)
072600000000     C                   ADD       PACC(I)       RAC(I)
072700000000     C                   ADD       PCOM(I)       RCO(I)
072800000000E   3C                   ENDIF
072900000000     C*** 80% ***
073000000000     C     3             OCCUR     GPER
073100000000IF  3C     WN051         IFGE      80
073200000000     C                   ADD       1             CLI(I)
073300000000     C                   ADD       PNSP(I)       PNS(I)
073400000000     C                   ADD       PNCL(I)       PNC(I)
073500990806     C                   ADD       PPKF(I)       PPK(I)
073600000000     C                   ADD       PVLF(I)       PVL(I)
073700000000     C                   ADD       PPOR(I)       PPO(I)
073800000000     C                   ADD       PACC(I)       PAC(I)
073900000000     C                   ADD       PCOM(I)       PCO(I)
074000000000X   3C                   ELSE
074100000000     C                   ADD       PNSP(I)       RNS(I)
074200000000     C                   ADD       PNCL(I)       RNC(I)
074300990806     C                   ADD       PPKF(I)       RPK(I)
074400000000     C                   ADD       PVLF(I)       RVL(I)
074500000000     C                   ADD       PPOR(I)       RPO(I)
074600000000     C                   ADD       PACC(I)       RAC(I)
074700000000     C                   ADD       PCOM(I)       RCO(I)
074800000000E   3C                   ENDIF
074900000000     C*** 90% ***
075000000000     C     4             OCCUR     GPER
075100000000IF  3C     WN051         IFGE      90
075200000000     C                   ADD       1             CLI(I)
075300000000     C                   ADD       PNSP(I)       PNS(I)
075400000000     C                   ADD       PNCL(I)       PNC(I)
075500990806     C                   ADD       PPKF(I)       PPK(I)
075600000000     C                   ADD       PVLF(I)       PVL(I)
075700000000     C                   ADD       PPOR(I)       PPO(I)
075800000000     C                   ADD       PACC(I)       PAC(I)
075900000000     C                   ADD       PCOM(I)       PCO(I)
076000000000X   3C                   ELSE
076100000000     C                   ADD       PNSP(I)       RNS(I)
076200000000     C                   ADD       PNCL(I)       RNC(I)
076300990806     C                   ADD       PPKF(I)       RPK(I)
076400000000     C                   ADD       PVLF(I)       RVL(I)
076500000000     C                   ADD       PPOR(I)       RPO(I)
076600000000     C                   ADD       PACC(I)       RAC(I)
076700000000     C                   ADD       PCOM(I)       RCO(I)
076800000000E   3C                   ENDIF
076900000000     C*
077000000000E   2C                   ENDDO
077100000000E   1C                   ENDIF
077200000000     C*
077300000000     C                   ENDSR
077400000000     C*------------------------------------------------------------------------*
077500000000     C* STABAN - STAMPA LA STATISTICA BANCALI
077600000000     C*------------------------------------------------------------------------*
077700000000     C     STABAN        BEGSR
077800000000     C*
077900000000     C* STAMPA PARAMETRI DI LANCIO (SOLO LA PRIMA VOLTA)
078000091016IF  1C     $VOLTB        IFEQ      '0'
078100091016     C**!!!              CLEAR                   SE07PI
078200091016     C**!!!              CLEAR                   SE07P2
078300091016     C                   EXSR      STAPAB
078400091016     C                   MOVEL     '1'           $VOLTB
078500091016E   1C                   ENDIF
078600000000     C*
078700000000     C* STAMPA TESTATA SE FINE PAGINA
078800091015IF  1C**!!!*IN03         IFEQ      *ON
078900091015     C**!!!              WRITE     SE07PT
079000091015     C**!!!              WRITE     SE07PI
079100091015     C**!!!              SETOFF                                       03
079200091015E   1C**!!!              ENDIF
079300000000     C*
079400000000     C* CONTROLLA LA VALIDITA' DEL RECORD PER LA STATISTICA BANCALI
079500000000     C                   Z-ADD     *ZEROS        BOLMED
079600000000     C                   Z-ADD     *ZEROS        WMED
079700000000     C                   EXSR      CHKBAN
079800000000IF  1C     WRECOK        IFEQ      'S'
079900091015
080000091015      * se richiesta scrittura del file scrivo un record su wfsba00f
080100091015     c                   if        parfil = 'S'
080200091015     c                   clear                   wfsba000
080300091015
080400091015     c                   eval      bandip = pardai
080500091015     c                   eval      bandfp = pardaf
080600091015
080700091015     C                   Z-ADD     DEPKSC        banksc
080800091015     C                   MOVEL     DESKSC        banrag
080900091015     C                   Z-ADD     DEPMED        banmed
081000091015     C                   Z-ADD     TASAAS        banaas
081100091015     C                   Z-ADD     TASLNP        banlnp
081200091015     C                   Z-ADD     TASNRS        bannrs
081300091015     C                   Z-ADD     TASNSP        bannsp
081400091015     C                   MOVEL     TASTBL        bantbl
081500091015     C                   Z-ADD     TASMGS        banmgs
081600091015     C                   Z-ADD     TASNCL        banncl
081700091015     C                   Z-ADD     TASPKF        banpkb
081800091015     C                   Z-ADD     TASVLB        banvlb
081900991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
082000991130     C     VALCNT        IFEQ      'ITL'
082100041103     C***                Z-ADD(H)  CNVPOR        ITLPOR           15 0
082200041103     C***                Z-ADD     ITLPOR        CNVPOR
082300041103     C                   Z-ADD(H)  CNVimv        ITLimv           15 0
082400041103     C                   Z-ADD     ITLimv        CNVimv
082500060117     c                   Z-add(h)  wcom          itlcom           12 0
082600060117     c                   Z-add     itlcom        wcom
082700991130     C                   ENDIF
082800041103     C***                Z-ADD     CNVPOR        P2CPOR
082900091015     C                   Z-ADD     CNVimv        banpor
083000091015     c                   Z-add(h)  wcom          bancom
083100091015     C                   Z-ADD     BOLMED        banmeb
083200091015     C**!!!              WRITE     SE07P2
083300080717
083400080717     c                   write     wfsba000
083500080717
083600080717     c                   endif
083700080717
083800000000E   1C                   ENDIF
083900000000     C*
084000000000     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C* CHKBAN - CONTROLLA LA VALIDITA' DEL RECORD PER LA STATISTICA BANCALI
084300000000     C*------------------------------------------------------------------------*
084400000000     C     CHKBAN        BEGSR
084500000000     C*
084600000000     C                   MOVEL     'S'           WRECOK
084700000000     C*
084800000000     C* BOLLA CON TARIFFA NON A COLLO, ESCLUDE
084900000000     C                   MOVEL     TASCTR        TIPTAR            1
085000000000IF  1C     TIPTAR        IFNE      '1'
085100000000     C                   MOVEL     'N'           WRECOK
085200000000E   1C                   ENDIF
085300041227
085400041227      * BOLLA CON VARIA 'O' BANCALI ESCLUDE
085500090327     c                   If        %Subst(TasGva:1:1) ='O' and wrecok <> 'N'
085600041227     c                   Eval      wrecok = 'N'
085700041228     c                   Leavesr
085800041227E   1c                   EndIf
085900000000     C*
086000000000     C* COLLI A ZERO, ESCLUDE (DA ERRORE NELLA DIVISIONE)
086100000000IF  1C     TASNCL        IFEQ      *ZEROS
086200000000     C                   MOVEL     'N'           WRECOK
086300000000X   1C                   ELSE
086400000000     C*
086500090305     C* SE LA MEDIA PESO/COLLO DELLA BOLLA E' SUPERIORE DI PIU' DI 10 VOLTE LA
086600000000     C* MEDIA STANDARD DEL CLIENTE LA BOLLA E' DA SEGNALARE, ALTRIMENTI ESCLUDE
086700090305      *                                                                     UDE
086800090305      * modificato a 5 VOLTE  5/03/09                                       LA
086900990806     C     TASPKF        DIV       TASNCL        BOLMED           11 3
087000090305      **   BOLMED        DIV       10            WMED             11 3
087100090305     C     BOLMED        DIV       5             WMED             11 3
087200000000IF  2C     WMED          IFLE      DEPMED
087300000000     C                   MOVEL     'N'           WRECOK
087400000000E   2C                   ENDIF
087500000000E   1C                   ENDIF
087600000000     C*
087700000000     C                   ENDSR
087800000000     C*------------------------------------------------------------------------*
087900000000     C* STARIC - STAMPA LA STATISTICA FASCE DI RICAVO
088000000000     C*------------------------------------------------------------------------*
088100000000     C     STARIC        BEGSR
088200000000     C*
088300010510     C                   CLEAR                   SE05PI
088400010510     C                   CLEAR                   SE05P1
088500000000     C*
088600000000     C* STAMPA PARAMETRI DI LANCIO
088700000000     C                   EXSR      STAPAR
088800000000IF  1C     PASGEN        IFEQ      '1'
088900000000     C***
089000000000     C* CON ACCESSORI
089100000000     C***
089200000000     C* TESTATA
089300010510     C                   WRITE     SE05PT
089400000000     C* DESCRIZIONE
089500000000     C                   MOVEL     DESCON        PDDACC
089600010510     C                   WRITE     SE05PD
089700000000     C* INTESTAZIONE
089800010510     C                   WRITE     SE05PI
089900000000     C* DETTAGLIO
090000000000DO  2C     1             DO        51            F
090100000000IF  3C     CNSP(F)       IFGT      *ZEROS
090200000000     C     CNCL(F)       ORGT      *ZEROS
090300990806     C     CPKF(F)       ORGT      *ZEROS
090400000000     C     CVLF(F)       ORGT      *ZEROS
090500000000     C     CRIC(F)       ORGT      *ZEROS
090600000000     C     CCOM(F)       ORGT      *ZEROS
090700000000     C                   MOVEL     DRI(F)        P1DDES
090800000000     C                   Z-ADD     CNSP(F)       P1CNSP
090900000000     C                   Z-ADD     CNCL(F)       P1CNCL
091000990806     C                   Z-ADD     CPKF(F)       P1CPKB
091100991130     C                   Z-ADD     CVLF(F)       P1CVLF
091200991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
091300991130     C     VALCNT        IFEQ      'ITL'
091400041103     C                   Z-ADD(H)  CRIC(F)       ITLPOR           15 0
091500991130     C                   Z-ADD     ITLPOR        CRIC(F)
091600010510     C                   Z-ADD(H)  CCOM(F)       ITLPOR
091700991130     C                   Z-ADD     ITLPOR        CCOM(F)
091800991130     C                   ENDIF
091900991130     C                   Z-ADD     CRIC(F)       P1CRIC
092000090925     C                   Z-ADD(h)  CCOM(F)       P1CCOM
092100010510     C                   WRITE     SE05P1
092200000000E   3C                   ENDIF
092300000000E   2C                   ENDDO
092400000000     C* RIGA
092500010510     C                   WRITE     SE05RI
092600000000     C***
092700000000     C* SENZA ACCESSORI
092800000000     C***
092900000000     C* TESTATA
093000010510     C                   WRITE     SE05PT
093100000000     C* DESCRIZIONE
093200000000     C                   MOVEL     DESSEN        PDDACC
093300010510     C                   WRITE     SE05PD
093400000000     C* INTESTAZIONE
093500010510     C                   WRITE     SE05PI
093600000000     C* DETTAGLIO
093700000000DO  2C     1             DO        51            F
093800000000IF  3C     SNSP(F)       IFGT      *ZEROS
093900000000     C     SNCL(F)       ORGT      *ZEROS
094000990806     C     SPKF(F)       ORGT      *ZEROS
094100000000     C     SVLF(F)       ORGT      *ZEROS
094200000000     C     SRIC(F)       ORGT      *ZEROS
094300000000     C     SCOM(F)       ORGT      *ZEROS
094400000000     C                   MOVEL     DRI(F)        P1DDES
094500000000     C                   Z-ADD     SNSP(F)       P1CNSP
094600000000     C                   Z-ADD     SNCL(F)       P1CNCL
094700990806     C                   Z-ADD     SPKF(F)       P1CPKB
094800991130     C                   Z-ADD     SVLF(F)       P1CVLF
094900991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
095000991130     C     VALCNT        IFEQ      'ITL'
095100010510     C                   Z-ADD(H)  SRIC(F)       ITLPOR
095200991130     C                   Z-ADD     ITLPOR        SRIC(F)
095300010510     C                   Z-ADD(H)  SCOM(F)       ITLPOR
095400991130     C                   Z-ADD     ITLPOR        SCOM(F)
095500991130     C                   ENDIF
095600991130     C                   Z-ADD     SRIC(F)       P1CRIC
095700090925     C                   Z-ADD(h)  SCOM(F)       P1CCOM
095800010510     C                   WRITE     SE05P1
095900000000E   3C                   ENDIF
096000000000E   2C                   ENDDO
096100000000     C* RIGA
096200010510     C                   WRITE     SE05RI
096300000000E   1C                   ENDIF
096400000000     C*
096500000000     C* STAMPA RIGA DI FINE PROSPETTO
096600010510     C                   WRITE     SE05FI
096700000000     C*
096800000000     C                   ENDSR
096900000000     C*------------------------------------------------------------------------*
097000000000     C* STACOL - STAMPA LA STATISTICA FASCE DI COLLI
097100000000     C*------------------------------------------------------------------------*
097200000000     C     STACOL        BEGSR
097300000000     C*
097400000000     C* PULISCE I FORMATI RECORD DEL PRINTER FILE
097500010510     C                   CLEAR                   SE06PA
097600010510     C                   CLEAR                   SE06PI
097700010510     C                   CLEAR                   SE06P1
097800000000     C*
097900000000     C* STAMPA PARAMETRI DI LANCIO
098000000000     C                   EXSR      STAPAC
098100000000IF  1C     PASGEN        IFEQ      '1'
098200000000     C*
098300000000     C* PER CIASCUNA PERCENTUALE MEMORIZZATA
098400000000DO  2C     1             DO        4             I
098500000000     C     I             OCCUR     GPER
098600000000     C***
098700000000     C* CON ACCESSORI
098800000000     C***
098900000000     C* TESTATA
099000010510     C                   WRITE     SE06PT
099100000000     C* DESCRIZIONE
099200000000     C                   MOVEL     DESCON        PDDACC
099300010510     C                   WRITE     SE06PD
099400000000     C* INTESTAZIONE
099500000000     C                   MOVEL     TCO(I)        PICPER
099600010510     C                   WRITE     SE06PI
099700000000     C* DETTAGLIO
099800000000DO  3C     1             DO        7             F
099900000000IF  4C     CLI(F)        IFGT      *ZEROS
100000000000     C     PNS(F)        ORGT      *ZEROS
100100000000     C     PNC(F)        ORGT      *ZEROS
100200000000     C     PPK(F)        ORGT      *ZEROS
100300000000     C     PVL(F)        ORGT      *ZEROS
100400000000     C     PPO(F)        ORGT      *ZEROS
100500000000     C     PAC(F)        ORGT      *ZEROS
100600000000     C     PCO(F)        ORGT      *ZEROS
100700000000     C     RNS(F)        ORGT      *ZEROS
100800000000     C     RNC(F)        ORGT      *ZEROS
100900000000     C     RPK(F)        ORGT      *ZEROS
101000000000     C     RVL(F)        ORGT      *ZEROS
101100000000     C     RPO(F)        ORGT      *ZEROS
101200000000     C     RAC(F)        ORGT      *ZEROS
101300000000     C     RCO(F)        ORGT      *ZEROS
101400000000     C                   MOVEL     DCO(F)        P1DDES
101500000000     C                   Z-ADD     CLI(F)        P1CCLI
101600000000     C                   Z-ADD     PNS(F)        P1CPNS
101700000000     C                   Z-ADD     PNC(F)        P1CPNC
101800000000     C                   Z-ADD     PPK(F)        P1CPPK
101900991130     C                   Z-ADD     PVL(F)        P1CPVL
102000000000     C     PPO(F)        ADD       PAC(F)        P1CPRI
102100000000     C                   Z-ADD     PCO(F)        P1CPCO
102200000000     C                   Z-ADD     RNS(F)        P1CRNS
102300000000     C                   Z-ADD     RNC(F)        P1CRNC
102400000000     C                   Z-ADD     RPK(F)        P1CRPK
102500991130     C                   Z-ADD     RVL(F)        P1CRVL
102600991130     C     RPO(F)        ADD       RAC(F)        P1CRRI
102700991130     C                   Z-ADD     RCO(F)        P1CRCO
102800991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
102900991130     C     VALCNT        IFEQ      'ITL'
103000010510     C                   Z-ADD(H)  P1CRRI        ITLPOR
103100991130     C                   Z-ADD     ITLPOR        P1CRRI
103200010510     C                   Z-ADD(H)  P1CRCO        ITLPOR
103300991130     C                   Z-ADD     ITLPOR        P1CRCO
103400991130     C                   ENDIF
103500010510     C                   WRITE     SE06P1
103600000000E   4C                   ENDIF
103700000000E   3C                   ENDDO
103800000000     C* RIGA
103900010510     C                   WRITE     SE06RI
104000000000     C***
104100000000     C* SENZA ACCESSORI
104200000000     C***
104300000000     C* DESCRIZIONE
104400000000     C                   MOVEL     DESSEN        PDDACC
104500010510     C                   WRITE     SE06PD
104600000000     C* INTESTAZIONE
104700010510     C                   WRITE     SE06PI
104800000000     C* DETTAGLIO
104900000000DO  3C     1             DO        7             F
105000000000IF  4C     CLI(F)        IFGT      *ZEROS
105100000000     C     PNS(F)        ORGT      *ZEROS
105200000000     C     PNC(F)        ORGT      *ZEROS
105300000000     C     PPK(F)        ORGT      *ZEROS
105400000000     C     PVL(F)        ORGT      *ZEROS
105500000000     C     PPO(F)        ORGT      *ZEROS
105600000000     C     PAC(F)        ORGT      *ZEROS
105700000000     C     PCO(F)        ORGT      *ZEROS
105800000000     C     RNS(F)        ORGT      *ZEROS
105900000000     C     RNC(F)        ORGT      *ZEROS
106000000000     C     RPK(F)        ORGT      *ZEROS
106100000000     C     RVL(F)        ORGT      *ZEROS
106200000000     C     RPO(F)        ORGT      *ZEROS
106300000000     C     RAC(F)        ORGT      *ZEROS
106400000000     C     RCO(F)        ORGT      *ZEROS
106500000000     C                   MOVEL     DCO(F)        P1DDES
106600000000     C                   Z-ADD     CLI(F)        P1CCLI
106700000000     C                   Z-ADD     PNS(F)        P1CPNS
106800000000     C                   Z-ADD     PNC(F)        P1CPNC
106900000000     C                   Z-ADD     PPK(F)        P1CPPK
107000991130     C                   Z-ADD     PVL(F)        P1CPVL
107100000000     C                   Z-ADD     PPO(F)        P1CPRI
107200000000     C                   Z-ADD     PCO(F)        P1CPCO
107300000000     C                   Z-ADD     RNS(F)        P1CRNS
107400000000     C                   Z-ADD     RNC(F)        P1CRNC
107500000000     C                   Z-ADD     RPK(F)        P1CRPK
107600991130     C                   Z-ADD     RVL(F)        P1CRVL
107700991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
107800991130     C     VALCNT        IFEQ      'ITL'
107900010510     C                   Z-ADD(H)  RPO(F)        ITLPOR
108000991130     C                   Z-ADD     ITLPOR        RPO(F)
108100010510     C                   Z-ADD(H)  RCO(F)        ITLPOR
108200991130     C                   Z-ADD     ITLPOR        RCO(F)
108300991130     C                   ENDIF
108400991130     C                   Z-ADD     RPO(F)        P1CRRI
108500991130     C                   Z-ADD     RCO(F)        P1CRCO
108600010510     C                   WRITE     SE06P1
108700000000E   4C                   ENDIF
108800000000E   3C                   ENDDO
108900000000     C* RIGA
109000010510     C                   WRITE     SE06RI
109100000000     C*
109200000000E   2C                   ENDDO
109300000000E   1C                   ENDIF
109400000000     C*
109500000000     C* STAMPA RIGA DI FINE PROSPETTO
109600010510     C                   WRITE     SE06FI
109700000000     C*
109800000000     C                   ENDSR
109900000000     C*------------------------------------------------------------------------*
110000000000     C* STAPAR - STAMPA PAGINA PARAMETRI STATISTICA FASCE DI RICAVO
110100000000     C*------------------------------------------------------------------------*
110200000000     C     STAPAR        BEGSR
110300000000     C*
110400000000     C                   Z-ADD     PARDAI        PACDAI
110500000000     C                   Z-ADD     PARDAF        PACDAF
110600000000     C*
110700010510     C                   WRITE     SE05PT
110800010510     C                   WRITE     SE05PA
110900000000     C*
111000000000     C                   ENDSR
111100000000     C*------------------------------------------------------------------------*
111200000000     C* STAPAC - STAMPA PAGINA PARAMETRI STATISTICA FASCE DI COLLI
111300000000     C*------------------------------------------------------------------------*
111400000000     C     STAPAC        BEGSR
111500000000     C*
111600000000     C                   Z-ADD     PARDAI        PACDAI
111700000000     C                   Z-ADD     PARDAF        PACDAF
111800000000     C*
111900010510     C                   WRITE     SE06PT
112000010510     C                   WRITE     SE06PA
112100000000     C*
112200000000     C                   ENDSR
112300000000     C*------------------------------------------------------------------------*
112400000000     C* STAPAB - STAMPA PAGINA PARAMETRI STATISTICA BANCALI
112500000000     C*------------------------------------------------------------------------*
112600091016     C     STAPAB        BEGSR
112700000000     C*
112800091016     C                   Z-ADD     PARDAI        PACDAI
112900091016     C                   Z-ADD     PARDAF        PACDAF
113000000000     C*
113100091016     C                   WRITE     SE07PT
113200091016     C                   WRITE     SE07PA
113300000000     C*
113400091016     C                   ENDSR
113500000000     C*------------------------------------------------------------------------*
113600000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
113700000000     C*------------------------------------------------------------------------*
113800000000     C     NEWKSC        BEGSR
113900000000     C*
114000000000     C* DEPOSITA IL NUOVO CODICE
114100000000     C                   Z-ADD     TASKSC        DEPKSC
114200990806     C*
114300990806     C* RECUPERA LA RAGIONE SOCIALE
114400990806     C                   MOVEL     *BLANKS       DESKSC           30
114500990806     C                   MOVE      DEPKSC        N4                4 0
114600990806     C                   MOVEL     TASTBL        A1                1
114700990806IF  1C     N4            IFNE      8888
114800980116     C                   CLEAR                   BS69DS
114900980116     C                   CLEAR                   ACODS
115000980116     C                   MOVEL     KNSIF         I69SIF
115100980116     C                   MOVEL     DEPKSC        I69KAC
115200990806     C                   CALL      'TIBS69R'
115300980116     C                   PARM                    BS69DS
115400980116     C                   PARM                    ACODS
115500980116     C                   PARM                    INDDS
115600980116     C                   PARM                    CLPDS
115700980116     C                   PARM                    CLSDS
115800990806IF  2C     O69ERR        IFNE      '1'
115900980116     C                   MOVEL     ACORAG        DESKSC
116000990806E   2C                   ENDIF
116100990806X   1C                   ELSE
116200990806IF  2C     A1            IFEQ      'F'
116300990806     C                   Z-ADD     TASAAS        KAAAAS
116400990806     C                   Z-ADD     TASLNP        KAALNP
116500990806     C                   Z-ADD     TASNRS        KAANRS
116600990806     C                   Z-ADD     TASNSP        KAANSP
116700990806     C                   MOVEL     'M'           KAATRC
116800990806     C     KEYTAA        CHAIN     TITAA30C                           98
116900990806IF  3C     *IN98         IFEQ      *OFF
117000990806     C                   MOVEL     TAARSC        DESKSC
117100990806E   3C                   ENDIF
117200990806X   2C                   ELSE
117300990806     C                   MOVEL     TASRSD        DESKSC
117400990806E   2C                   ENDIF
117500990806E   1C                   ENDIF
117600000000     C*----
117700000000     C* STATISTICA COLLI
117800000000     C*----
117900000000     C* REIMPOSTA I CAMPI DI MEMORIZZAZIONE
118000000000     C                   Z-ADD     *ZEROS        TOKNSP           15 0
118100000000     C                   CLEAR                   PNSP
118200000000     C                   CLEAR                   PNCL
118300990806     C                   CLEAR                   PPKF
118400000000     C                   CLEAR                   PVLF
118500000000     C                   CLEAR                   PPOR
118600000000     C                   CLEAR                   PACC
118700000000     C                   CLEAR                   PCOM
118800000000     C*
118900000000     C                   ENDSR
119000000000     C*------------------------------------------------------------------------*
119100000000     C* NEWMES - OPERAZIONI PER NUOVO MESE
119200000000     C*------------------------------------------------------------------------*
119300000000     C     NEWMES        BEGSR
119400000000     C*
119500000000     C* DEPOSITA IL NUOVO MESE
119600000000     C                   Z-ADD     TA2MES        DEPMES
119700000000     C*
119800000000     C* CALCOLA LA MEDIA PESO/COLLO DEL CLIENTE
119900000000     C                   EXSR      CALMED
120000000000     C*
120100000000     C                   ENDSR
120200000000     C*------------------------------------------------------------------------*
120300000000     C* CALMED - CALCOLA LA MEDIA PESO/COLLO
120400000000     C*------------------------------------------------------------------------*
120500000000     C     CALMED        BEGSR
120600000000     C*
120700000000     C                   Z-ADD     *ZEROS        DEPMED
120800041227
120900041228     c                   move      tasaas        waaf
121000041228     c                   movel     tasmgs        wmmf
121100041227     c                   move      01            wggf
121200041227
121300041227     c     *iso          move      wdataf        dataiso
121400041228     c                   subdur    2:*m          dataiso
121500041227     c                   extrct    dataiso:*m    wmmi
121600041227     c                   extrct    dataiso:*y    waai
121700041227
121800041227     C                   clear                   TISE70DS
121900041227     C                   eval      I70TLA = 'S'
122000041227     C                   eval      I70TLE = 'ST'
122100041227     C                   eval      I70KSC = depksc
122200041227     C                   eval      I70DAF = waammf
122300041227     C                   eval      I70DAI = waammi
122400041227
122500041227     C                   call      'TISE70R'
122600041227     c                   parm                    TISE70DS
122700041227
122800041227     c                   If        o70nco > *Zeros
122900041227     c     o70pkg        div       o70nco        depmed
123000041227     c                   EndIf
123100000000     C*
123200000000     C                   ENDSR
123300000000     C*------------------------------------------------------------------------*
123400020702     C* CALFSR - CALCOLA LA FASCIA DI RICAVO
123500000000     C*------------------------------------------------------------------------*
123600000000     C     CALFSR        BEGSR
123700000000     C*
123800000000IF  1C     RICAVO        IFLE      1000
123900000000     C                   Z-ADD     1             F
124000000000E   1C                   ENDIF
124100000000IF  1C     RICAVO        IFGT      1000
124200000000     C     RICAVO        ANDLE     2000
124300000000     C                   Z-ADD     2             F
124400000000E   1C                   ENDIF
124500000000IF  1C     RICAVO        IFGT      2000
124600000000     C     RICAVO        ANDLE     3000
124700000000     C                   Z-ADD     3             F
124800000000E   1C                   ENDIF
124900000000IF  1C     RICAVO        IFGT      3000
125000000000     C     RICAVO        ANDLE     4000
125100000000     C                   Z-ADD     4             F
125200000000E   1C                   ENDIF
125300000000IF  1C     RICAVO        IFGT      4000
125400000000     C     RICAVO        ANDLE     5000
125500000000     C                   Z-ADD     5             F
125600000000E   1C                   ENDIF
125700000000IF  1C     RICAVO        IFGT      5000
125800000000     C     RICAVO        ANDLE     6000
125900000000     C                   Z-ADD     6             F
126000000000E   1C                   ENDIF
126100000000IF  1C     RICAVO        IFGT      6000
126200000000     C     RICAVO        ANDLE     7000
126300000000     C                   Z-ADD     7             F
126400000000E   1C                   ENDIF
126500000000IF  1C     RICAVO        IFGT      7000
126600000000     C     RICAVO        ANDLE     8000
126700000000     C                   Z-ADD     8             F
126800000000E   1C                   ENDIF
126900000000IF  1C     RICAVO        IFGT      8000
127000000000     C     RICAVO        ANDLE     9000
127100000000     C                   Z-ADD     9             F
127200000000E   1C                   ENDIF
127300000000IF  1C     RICAVO        IFGT      9000
127400000000     C     RICAVO        ANDLE     10000
127500000000     C                   Z-ADD     10            F
127600000000E   1C                   ENDIF
127700000000IF  1C     RICAVO        IFGT      10000
127800000000     C     RICAVO        ANDLE     11000
127900000000     C                   Z-ADD     11            F
128000000000E   1C                   ENDIF
128100000000IF  1C     RICAVO        IFGT      11000
128200000000     C     RICAVO        ANDLE     12000
128300000000     C                   Z-ADD     12            F
128400000000E   1C                   ENDIF
128500000000IF  1C     RICAVO        IFGT      12000
128600000000     C     RICAVO        ANDLE     13000
128700000000     C                   Z-ADD     13            F
128800000000E   1C                   ENDIF
128900000000IF  1C     RICAVO        IFGT      13000
129000000000     C     RICAVO        ANDLE     14000
129100000000     C                   Z-ADD     14            F
129200000000E   1C                   ENDIF
129300000000IF  1C     RICAVO        IFGT      14000
129400000000     C     RICAVO        ANDLE     15000
129500000000     C                   Z-ADD     15            F
129600000000E   1C                   ENDIF
129700000000IF  1C     RICAVO        IFGT      15000
129800000000     C     RICAVO        ANDLE     16000
129900000000     C                   Z-ADD     16            F
130000000000E   1C                   ENDIF
130100000000IF  1C     RICAVO        IFGT      16000
130200000000     C     RICAVO        ANDLE     17000
130300000000     C                   Z-ADD     17            F
130400000000E   1C                   ENDIF
130500000000IF  1C     RICAVO        IFGT      17000
130600000000     C     RICAVO        ANDLE     18000
130700000000     C                   Z-ADD     18            F
130800000000E   1C                   ENDIF
130900000000IF  1C     RICAVO        IFGT      18000
131000000000     C     RICAVO        ANDLE     19000
131100000000     C                   Z-ADD     19            F
131200000000E   1C                   ENDIF
131300000000IF  1C     RICAVO        IFGT      19000
131400000000     C     RICAVO        ANDLE     20000
131500000000     C                   Z-ADD     20            F
131600000000E   1C                   ENDIF
131700000000IF  1C     RICAVO        IFGT      20000
131800000000     C     RICAVO        ANDLE     21000
131900000000     C                   Z-ADD     21            F
132000000000E   1C                   ENDIF
132100000000IF  1C     RICAVO        IFGT      21000
132200000000     C     RICAVO        ANDLE     22000
132300000000     C                   Z-ADD     22            F
132400000000E   1C                   ENDIF
132500000000IF  1C     RICAVO        IFGT      22000
132600000000     C     RICAVO        ANDLE     23000
132700000000     C                   Z-ADD     23            F
132800000000E   1C                   ENDIF
132900000000IF  1C     RICAVO        IFGT      23000
133000000000     C     RICAVO        ANDLE     24000
133100000000     C                   Z-ADD     24            F
133200000000E   1C                   ENDIF
133300000000IF  1C     RICAVO        IFGT      24000
133400000000     C     RICAVO        ANDLE     25000
133500000000     C                   Z-ADD     25            F
133600000000E   1C                   ENDIF
133700000000IF  1C     RICAVO        IFGT      25000
133800000000     C     RICAVO        ANDLE     26000
133900000000     C                   Z-ADD     26            F
134000000000E   1C                   ENDIF
134100000000IF  1C     RICAVO        IFGT      26000
134200000000     C     RICAVO        ANDLE     27000
134300000000     C                   Z-ADD     27            F
134400000000E   1C                   ENDIF
134500000000IF  1C     RICAVO        IFGT      27000
134600000000     C     RICAVO        ANDLE     28000
134700000000     C                   Z-ADD     28            F
134800000000E   1C                   ENDIF
134900000000IF  1C     RICAVO        IFGT      28000
135000000000     C     RICAVO        ANDLE     29000
135100000000     C                   Z-ADD     29            F
135200000000E   1C                   ENDIF
135300000000IF  1C     RICAVO        IFGT      29000
135400000000     C     RICAVO        ANDLE     30000
135500000000     C                   Z-ADD     30            F
135600000000E   1C                   ENDIF
135700000000IF  1C     RICAVO        IFGT      30000
135800000000     C     RICAVO        ANDLE     31000
135900000000     C                   Z-ADD     31            F
136000000000E   1C                   ENDIF
136100000000IF  1C     RICAVO        IFGT      31000
136200000000     C     RICAVO        ANDLE     32000
136300000000     C                   Z-ADD     32            F
136400000000E   1C                   ENDIF
136500000000IF  1C     RICAVO        IFGT      32000
136600000000     C     RICAVO        ANDLE     33000
136700000000     C                   Z-ADD     33            F
136800000000E   1C                   ENDIF
136900000000IF  1C     RICAVO        IFGT      33000
137000000000     C     RICAVO        ANDLE     34000
137100000000     C                   Z-ADD     34            F
137200000000E   1C                   ENDIF
137300000000IF  1C     RICAVO        IFGT      34000
137400000000     C     RICAVO        ANDLE     35000
137500000000     C                   Z-ADD     35            F
137600000000E   1C                   ENDIF
137700000000IF  1C     RICAVO        IFGT      35000
137800000000     C     RICAVO        ANDLE     36000
137900000000     C                   Z-ADD     36            F
138000000000E   1C                   ENDIF
138100000000IF  1C     RICAVO        IFGT      36000
138200000000     C     RICAVO        ANDLE     37000
138300000000     C                   Z-ADD     37            F
138400000000E   1C                   ENDIF
138500000000IF  1C     RICAVO        IFGT      37000
138600000000     C     RICAVO        ANDLE     38000
138700000000     C                   Z-ADD     38            F
138800000000E   1C                   ENDIF
138900000000IF  1C     RICAVO        IFGT      38000
139000000000     C     RICAVO        ANDLE     39000
139100000000     C                   Z-ADD     39            F
139200000000E   1C                   ENDIF
139300000000IF  1C     RICAVO        IFGT      39000
139400000000     C     RICAVO        ANDLE     40000
139500000000     C                   Z-ADD     40            F
139600000000E   1C                   ENDIF
139700000000IF  1C     RICAVO        IFGT      40000
139800000000     C     RICAVO        ANDLE     41000
139900000000     C                   Z-ADD     41            F
140000000000E   1C                   ENDIF
140100000000IF  1C     RICAVO        IFGT      41000
140200000000     C     RICAVO        ANDLE     42000
140300000000     C                   Z-ADD     42            F
140400000000E   1C                   ENDIF
140500000000IF  1C     RICAVO        IFGT      42000
140600000000     C     RICAVO        ANDLE     43000
140700000000     C                   Z-ADD     43            F
140800000000E   1C                   ENDIF
140900000000IF  1C     RICAVO        IFGT      43000
141000000000     C     RICAVO        ANDLE     44000
141100000000     C                   Z-ADD     44            F
141200000000E   1C                   ENDIF
141300000000IF  1C     RICAVO        IFGT      44000
141400000000     C     RICAVO        ANDLE     45000
141500000000     C                   Z-ADD     45            F
141600000000E   1C                   ENDIF
141700000000IF  1C     RICAVO        IFGT      45000
141800000000     C     RICAVO        ANDLE     46000
141900000000     C                   Z-ADD     46            F
142000000000E   1C                   ENDIF
142100000000IF  1C     RICAVO        IFGT      46000
142200000000     C     RICAVO        ANDLE     47000
142300000000     C                   Z-ADD     47            F
142400000000E   1C                   ENDIF
142500000000IF  1C     RICAVO        IFGT      47000
142600000000     C     RICAVO        ANDLE     48000
142700000000     C                   Z-ADD     48            F
142800000000E   1C                   ENDIF
142900000000IF  1C     RICAVO        IFGT      48000
143000000000     C     RICAVO        ANDLE     49000
143100000000     C                   Z-ADD     49            F
143200000000E   1C                   ENDIF
143300000000IF  1C     RICAVO        IFGT      49000
143400000000     C     RICAVO        ANDLE     50000
143500000000     C                   Z-ADD     50            F
143600000000E   1C                   ENDIF
143700000000IF  1C     RICAVO        IFGT      50000
143800000000     C                   Z-ADD     51            F
143900000000E   1C                   ENDIF
144000000000     C*
144100000000     C                   ENDSR
144200000000     C*------------------------------------------------------------------------*
144300000000     C* CALFSC - CALCOLA LA FASCIA DI COLLI
144400000000     C*------------------------------------------------------------------------*
144500000000     C     CALFSC        BEGSR
144600000000     C*
144700000000IF  1C     COLLI         IFLE      1
144800000000     C                   Z-ADD     1             F
144900000000E   1C                   ENDIF
145000000000IF  1C     COLLI         IFGT      1
145100000000     C     COLLI         ANDLE     2
145200000000     C                   Z-ADD     2             F
145300000000E   1C                   ENDIF
145400000000IF  1C     COLLI         IFGT      2
145500000000     C     COLLI         ANDLE     3
145600000000     C                   Z-ADD     3             F
145700000000E   1C                   ENDIF
145800000000IF  1C     COLLI         IFGT      3
145900000000     C     COLLI         ANDLE     4
146000000000     C                   Z-ADD     4             F
146100000000E   1C                   ENDIF
146200000000IF  1C     COLLI         IFGT      4
146300000000     C     COLLI         ANDLE     5
146400000000     C                   Z-ADD     5             F
146500000000E   1C                   ENDIF
146600000000IF  1C     COLLI         IFGT      5
146700000000     C     COLLI         ANDLE     6
146800000000     C                   Z-ADD     6             F
146900000000E   1C                   ENDIF
147000000000IF  1C     COLLI         IFGT      6
147100000000     C                   Z-ADD     7             F
147200000000E   1C                   ENDIF
147300000000     C*
147400000000     C                   ENDSR
147500000000     C*------------------------------------------------------------------------*
147600000000     C* CARTAB - CARICA LE TABELLE
147700000000     C*------------------------------------------------------------------------*
147800000000     C     CARTAB        BEGSR
147900000000     C*--------------------
148000000000     C* TIPI BOLLA
148100000000     C*--------------------
148200000000     C                   Z-ADD     *ZEROS        I                 4 0
148300020906     C                   Z-ADD     1             KTBKUT
148400000000     C                   MOVEL     'TB'          KTBCOD
148500000000     C     KE2TAB        SETLL     TABEL00F
148600000000     C     KE2TAB        READE     TABEL00F                               98
148700000000DO  1C     *IN98         DOWEQ     *OFF
148800000000IF  2C     TBLFLG        IFEQ      *BLANKS
148900000000     C                   ADD       1             I
149000000000     C                   MOVEL     TBLKEY        TBL(I)
149100000000     C                   MOVEL     TBLUNI        DSTB
149200000000     C                   MOVEL     §TBRBL        RBL(I)
149300000000E   2C                   ENDIF
149400000000     C     KE2TAB        READE     TABEL00F                               98
149500000000E   1C                   ENDDO
149600000000     C*--------------------
149700000000     C* CAMBIO ECU
149800000000     C*--------------------
149900020906     C                   Z-ADD     1             KTBKUT
150000000000     C                   MOVEL     'CV'          KTBCOD
150100000000     C                   MOVEL(P)  'XEU'         KTBKEY
150200000000     C     KEYTAB        CHAIN     TABEL00F                           98
150300000000IF  1C     *IN98         IFEQ      *OFF
150400000000     C     TBLFLG        ANDEQ     *BLANKS
150500000000     C                   MOVEL     TBLUNI        DSCV
150600000000     C                   Z-ADD     §CVCMB        ECUCMB            9 3
150700000000X   1C                   ELSE
150800000000     C                   Z-ADD     100           ECUCMB            9 3
150900000000E   1C                   ENDIF
151000971204     C*--------------------
151100971204     C* ORGANIGRAMMA
151200971204     C*--------------------
151300971204     C                   Z-ADD     *ZEROS        I                 4 0
151400971204     C     *LOVAL        SETLL     AZORG01L
151500971204     C                   READ      AZORG01L                               98
151600971204DO  1C     *IN98         DOWEQ     *OFF
151700971204IF  2C     ORGFVA        IFEQ      *BLANKS
151800971204     C     ORGFAG        ANDNE     'V'
151900000614     C                   MOVEL     ORGDE3        OG143
152000971215     C                   ADD       1             I
152100020627 MOM C*                  IF        §OGNTW = 'PPT'
152200020627     C*                  Z-ADD     ORGFIL        PFIL(I)
152300020627     C*                  MOVEL     ORGFL1        PFL1(I)
152400020627     C*                  ELSE
152500000614     C                   Z-ADD     ORGFIL        FIL(I)
152600020702     C*
152700020627     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
152800020702     C                   CLEAR                   DNTW
152900020702     C*
153000020702     C                   MOVEL(P)  'NTW'         KTECOD
153100020702     C                   MOVEL(P)  §OGNTW        KTEKE1
153200020702     C     KEYTBE        CHAIN     TNTBE01L
153300020702     C                   IF        %found(TNTBE01L)
153400020702     C                   MOVEL     TBEUNI        DNTW
153500020702     C                   ENDIF
153600020702     C*
153700020702     C                   MOVEL     §NTWFIE       FL1(I)
153800020702     C                   IF        §OGNTW = 'DPD'
153900020702     C                   MOVEL     'E'           FL1(I)
154000020702     C                   ENDIF
154100020627     C*
154200000619     C*                  ENDIF
154300971204E   2C                   ENDIF
154400971204     C                   READ      AZORG01L                               98
154500971204E   1C                   ENDDO
154600971204     C*
154700000000     C                   ENDSR
154800000000     C*------------------------------------------------------------------------*
154900000000     C* OPNPRT - APRE I FILE PRINTER DESIDERATI
155000000000     C*------------------------------------------------------------------------*
155100000000     C     OPNPRT        BEGSR
155200000000     C*
155300000000IF  1C     PARRIC        IFEQ      '1'
155400010510     C                   OPEN      TISE05P
155500000000E   1C                   ENDIF
155600000000IF  1C     PARCOL        IFEQ      '1'
155700010510     C                   OPEN      TISE06P
155800000000E   1C                   ENDIF
155900091016IF  1C     PARBAN        IFEQ      '1'
156000091016     C                   OPEN      TISE07P
156100091016E   1C                   ENDIF
156200000000     C*
156300000000     C                   ENDSR
156400000000     C*------------------------------------------------------------------------*
156500980116     C* FINSR - OPERAZIONI FINALI
156600000000     C*------------------------------------------------------------------------*
156700980116     C     FINSR         BEGSR
156800000000     C*
156900980116     C* CHIUDE I FILE PRINTER APERTI
157000000000IF  1C     PARRIC        IFEQ      '1'
157100010510     C                   CLOSE     TISE05P
157200000000E   1C                   ENDIF
157300000000IF  1C     PARCOL        IFEQ      '1'
157400010510     C                   CLOSE     TISE06P
157500000000E   1C                   ENDIF
157600091016IF  1C     PARBAN        IFEQ      '1'
157700091016     C                   WRITE     SE07FI
157800091016     C                   CLOSE     TISE07P
157900091016E   1C                   ENDIF
158000980116     C*
158100980116     C* CHIUDE I FILE DEI PGM CHIAMATI
158200980116     C                   CLEAR                   BS69DS
158300980116     C                   CLEAR                   ACODS
158400980116     C                   CLEAR                   INDDS
158500980116     C                   CLEAR                   CLPDS
158600980116     C                   CLEAR                   CLSDS
158700980116     C                   MOVEL     'C'           I69TLA
158800980116     C                   MOVEL     KNSIF         I69SIF
158900980116     C                   CALL      'TIBS69R'
159000980116     C                   PARM                    BS69DS
159100980116     C                   PARM                    ACODS
159200980116     C                   PARM                    INDDS
159300980116     C                   PARM                    CLPDS
159400980116     C                   PARM                    CLSDS
159500000000     C*
159600000000     C                   ENDSR
159700991126     C*-------------------------------------------------------------------*
159800991126     C* CONVERSIONE IN MONETA ED EVENTUALE ARROTONDAMENTO
159900991126     C*-------------------------------------------------------------------*
160000991126     C     CNVEUR        BEGSR
160100991126     C*
160200991126    4C     COMIMP        IFNE      *ZEROS
160300991130     C     COMDIV        ANDNE     VALCNT
160400991126     C                   CLEAR                   YEUR
160500991126     C                   MOVEL     COMDIV        YECDVI
160600991130     C                   MOVEL     VALCNT        YECDVO
160700991126     C                   Z-ADD     COMIMP        YECIMI
160800991126     C                   MOVE      '3'           YECDCO
160900991126     C*
161000991126     C                   CALL      'YEURCO'
161100991126     C                   PARM                    YEUR
161200991126     C*
161300991126    5C     YECESI        IFEQ      ' '
161400991126     C                   Z-ADD     YECIMO        COMIMP           30 9
161500991126     C                   ELSE
161600991126     C                   Z-ADD     *ZEROS        COMIMP
161700991126    5C                   ENDIF
161800991126     C*
161900991126    5C                   ENDIF
162000991126     C*
162100991126     C* MANTENGO SEMPRE INIZIALIZZATA LA DIVISA DI INPUT
162200991126     C                   MOVEL     *BLANKS       COMDIV            3
162300991126     C*
162400991126     C                   ENDSR
162500020117     C*------------------------------------------------------------------------*
162600020117     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "ST" E CONSIDERAZIONI SU DIVISE
162700020117     C*------------------------------------------------------------------------*
162800020117     C     CARGEDCE      BEGSR
162900020117     C*
163000020117     C                   CLEAR                   DGEDST
163100020117     C                   MOVEL     *blanks       WDIVCE            3
163200020117     C*
163300020117     C                   MOVEL(P)  'GED'         KTECOD
163400020117     C                   MOVEL(P)  'ST'          KTEKE1
163500020117     C     KEYTBE        CHAIN     TNTBE01L
163600020117     C                   IF        %found(TNTBE01L)
163700020117     C                   MOVEL     TBEUNI        DGEDST
163800020117     C                   ENDIF
163900020117     C*
164000020117     C                   IF        DATCOR < §GEDSTDATR
164100020117     C                   EVAL      WDIVCE = §GEDSTDIPR
164200020117     C                   ELSE
164300020117     C                   EVAL      WDIVCE = §GEDSTDIDR
164400020117     C                   ENDIF
164500020117     C*
164600020117     C                   ENDSR
164700000000     C*------------------------------------------------------------------------*
164800000000     C* *INZSR - OPERAZIONI INIZIALI
164900000000     C*------------------------------------------------------------------------*
165000000000     C     *INZSR        BEGSR
165100000000     C*--------------------
165200000000     C* RICEVIMENTO PARAMETRI
165300000000     C*--------------------
165400000000     C     *ENTRY        PLIST
165500000000     C                   PARM                    KPJBA
165600000000     C                   MOVEL     KPJBU         PARAM
165700000000     C*--------------------
165800000000     C* VARIABILI RIFERITE AL DATA BASE
165900000000     C*--------------------
166000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
166100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
166200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
166300990805     C     *LIKE         DEFINE    TA7AAS        KT7AAS
166400000000     C     *LIKE         DEFINE    TA7LNP        KT7LNP
166500000000     C     *LIKE         DEFINE    TA7NRS        KT7NRS
166600000000     C     *LIKE         DEFINE    TA7NSP        KT7NSP
166700000000     C     *LIKE         DEFINE    TA7TBL        KT7TBL
166800990806     C     *LIKE         DEFINE    TAAAAS        KAAAAS
166900990806     C     *LIKE         DEFINE    TAALNP        KAALNP
167000990806     C     *LIKE         DEFINE    TAANRS        KAANRS
167100990806     C     *LIKE         DEFINE    TAANSP        KAANSP
167200990806     C     *LIKE         DEFINE    TAATRC        KAATRC
167300000000     C     *LIKE         DEFINE    CETAAS        KCTAAS
167400000000     C     *LIKE         DEFINE    CETLNP        KCTLNP
167500000000     C     *LIKE         DEFINE    CETNRS        KCTNRS
167600000000     C     *LIKE         DEFINE    CETNSP        KCTNSP
167700000000     C     *LIKE         DEFINE    CETTBL        KCTTBL
167800000000     C     *LIKE         DEFINE    TASKSC        DEPKSC
167900971204     C     *LIKE         DEFINE    ORGFL1        DLPFL1
168000971204     C     *LIKE         DEFINE    ORGFL1        DLAFL1
168100020117     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
168200020117     C     *LIKE         DEFINE    TBEKE1        KTEKE1
168300000000     C                   Z-ADD     *ZEROS        DEPMED           11 3
168400000000     C*--------------------
168500000000     C* CHIAVI DI LETTURA
168600000000     C*--------------------
168700000000     C* LETTURA TABEL00F - PARZIALE
168800000000     C     KE2TAB        KLIST
168900000000     C                   KFLD                    KTBKUT
169000000000     C                   KFLD                    KTBCOD
169100000000     C* LETTURA TABEL00F - COMPLETA
169200000000     C     KEYTAB        KLIST
169300000000     C                   KFLD                    KTBKUT
169400000000     C                   KFLD                    KTBCOD
169500000000     C                   KFLD                    KTBKEY
169600990805     C* LETTURA TITA730C - PARZIALE
169700000000     C     KEYTA7        KLIST
169800000000     C                   KFLD                    KT7AAS
169900000000     C                   KFLD                    KT7LNP
170000000000     C                   KFLD                    KT7NRS
170100000000     C                   KFLD                    KT7NSP
170200000000     C                   KFLD                    KT7TBL
170300990806     C* LETTURA TITAA00L - COMPLETA
170400990806     C     KEYTAA        KLIST
170500990806     C                   KFLD                    KAAAAS
170600990806     C                   KFLD                    KAALNP
170700990806     C                   KFLD                    KAANRS
170800990806     C                   KFLD                    KAANSP
170900990806     C                   KFLD                    KAATRC
171000000000     C* LETTURA ECCET30C - COMPLETA
171100000000     C     KEYCET        KLIST
171200000000     C                   KFLD                    KCTAAS
171300000000     C                   KFLD                    KCTLNP
171400000000     C                   KFLD                    KCTNRS
171500000000     C                   KFLD                    KCTNSP
171600000000     C                   KFLD                    KCTTBL
171700020117     C* LETTURA TNTBE01L - PARZIALE
171800020117     C     KEYTBE        KLIST
171900020117     C                   KFLD                    KTECOD                         *UTENTE
172000020117     C                   KFLD                    KTEKE1                         *CODICE
172100090326
172200000000     C*--------------------
172300000000     C* VARIABILI DI CONTROLLO
172400000000     C*--------------------
172500000000     C                   MOVEL     '0'           $FINE             1
172600000000     C                   MOVEL     'N'           WRECOK            1
172700000000     C                   MOVEL     '0'           PASGEN            1
172800000000     C                   MOVEL     '0'           $VOLTB            1
172900000000     C                   MOVEL     '1'           *IN01
173000000000     C                   MOVEL     '1'           *IN02
173100000000     C                   MOVEL     '1'           *IN03
173200020117     C*
173300020117     C* REPERISCE LA DATA CORRENTE
173400020117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
173500020117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
173600020117     C                   Z-ADD     WN8           G08DAT
173700020117     C                   Z-ADD     *ZEROS        G08INV
173800020117     C                   MOVEL     '0'           G08ERR
173900020117     C                   CALL      'XSRDA8'
174000020117     C                   PARM                    WLBDA8
174100020117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
174200020117     C*
174300020117     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
174400020117     C                   EXSR      CARGEDCE
174500020117     C                   MOVEL     WDIVCE        VALCNT            3
174600000000     C*
174700000000     C* CARICA LE TABELLE
174800000000     C                   EXSR      CARTAB
174900080717
175000080717      * se richiesta la scrittura del file lo pulisco prima di scriverci dei dati
175100080717
175200080717     c                   if        parfil = 'S'
175300080717
175400080717      * se s.i. di prova imposto la libreria di prova
175500080717     c                   if        %subst(knsif:7:1) = 'P'
175600080717     c                   eval      wlib = 'GAITRAAZP '
175700080717     c                   else
175800080717     c                   eval      wlib = 'GAITRAAZM '
175900080717     c                   endif
176000080717      * pulisco il file
176100080717     c                   clear                   comman
176200080717     c                   EVAL      comman = 'CLRPFM FILE('
176300080717     c                             + %TRIMR(WLIB)
176400080717     c                             + '/WFSBA00F)'
176500080717
176600080717     c                   call      'QCMDEXC'
176700080717     c                   parm                    comman
176800080717     c                   parm                    lenght
176900080717
177000080717     c                   open      wfsba00f
177100080717
177200080717     c                   endif
177300080717
177400000000     C*
177500000000     C                   ENDSR
177600000000     O*------------------------------------------------------------------------*
177700000000**   DRI - DECODIFICHE RIGHE DI DETTAGLIO STATISTICA FASCE DI RICAVO
1778000000000/  1.000      1
1779000000001/  2.000      2
1780000000002/  3.000      3
1781000000003/  4.000      4
1782000000004/  5.000      5
1783000000005/  6.000      6
1784000000006/  7.000      7
1785000000007/  8.000      8
1786000000008/  9.000      9
1787000000009/ 10.000      10
17880000000010/11.000      11
17890000000011/12.000      12
17900000000012/13.000      13
17910000000013/14.000      14
17920000000014/15.000      15
17930000000015/16.000      16
17940000000016/17.000      17
17950000000017/18.000      18
17960000000018/19.000      19
17970000000019/20.000      20
17980000000020/21.000      21
17990000000021/22.000      22
18000000000022/23.000      23
18010000000023/24.000      24
18020000000024/25.000      25
18030000000025/26.000      26
18040000000026/27.000      27
18050000000027/28.000      28
18060000000028/29.000      29
18070000000029/30.000      30
18080000000030/31.000      31
18090000000031/32.000      32
18100000000032/33.000      33
18110000000033/34.000      34
18120000000034/35.000      35
18130000000035/36.000      36
18140000000036/37.000      37
18150000000037/38.000      38
18160000000038/39.000      39
18170000000039/40.000      40
18180000000040/41.000      41
18190000000041/42.000      42
18200000000042/43.000      43
18210000000043/44.000      44
18220000000044/45.000      45
18230000000045/46.000      46
18240000000046/47.000      47
18250000000047/48.000      48
18260000000048/49.000      49
18270000000049/50.000      50
182800000000OLTRE 50.000   51
182900000000**   TCO - DECODIFICHE RIGHE DI TESTATA STATISTICA FASCE DI COLLI
18300000000060             1
18310000000070             2
18320000000080             3
18330000000090             4
183400000000               5
183500000000**   DCO - DECODIFICHE RIGHE DI DETTAGLIO STATISTICA FASCE DI COLLI
1836000000000/ 1           1
1837000000000/ 2           2
1838000000000/ 3           3
1839000000000/ 4           4
1840000000000/ 5           5
1841000000000/ 6           6
1842000000000/OLTRE 6      7
