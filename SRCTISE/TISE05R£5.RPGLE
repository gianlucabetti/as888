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
085900130913      * BOLLA CON peso bolla minore o uguale a 1 kg esclude
086000130913     c                   If        taspkf <= 1
086100130913     c                   Eval      wrecok = 'N'
086200130913     c                   Leavesr
086300130913E   1c                   EndIf
086400000000     C*
086500000000     C* COLLI A ZERO, ESCLUDE (DA ERRORE NELLA DIVISIONE)
086600000000IF  1C     TASNCL        IFEQ      *ZEROS
086700000000     C                   MOVEL     'N'           WRECOK
086800000000X   1C                   ELSE
086900000000     C*
087000090305     C* SE LA MEDIA PESO/COLLO DELLA BOLLA E' SUPERIORE DI PIU' DI 10 VOLTE LA
087100000000     C* MEDIA STANDARD DEL CLIENTE LA BOLLA E' DA SEGNALARE, ALTRIMENTI ESCLUDE
087200090305      *                                                                     UDE
087300090305      * modificato a 5 VOLTE  5/03/09                                       LA
087400990806     C     TASPKF        DIV       TASNCL        BOLMED           11 3
087500090305      **   BOLMED        DIV       10            WMED             11 3
087600090305     C     BOLMED        DIV       5             WMED             11 3
087700000000IF  2C     WMED          IFLE      DEPMED
087800000000     C                   MOVEL     'N'           WRECOK
087900000000E   2C                   ENDIF
088000000000E   1C                   ENDIF
088100000000     C*
088200000000     C                   ENDSR
088300000000     C*------------------------------------------------------------------------*
088400000000     C* STARIC - STAMPA LA STATISTICA FASCE DI RICAVO
088500000000     C*------------------------------------------------------------------------*
088600000000     C     STARIC        BEGSR
088700000000     C*
088800010510     C                   CLEAR                   SE05PI
088900010510     C                   CLEAR                   SE05P1
089000000000     C*
089100000000     C* STAMPA PARAMETRI DI LANCIO
089200000000     C                   EXSR      STAPAR
089300000000IF  1C     PASGEN        IFEQ      '1'
089400000000     C***
089500000000     C* CON ACCESSORI
089600000000     C***
089700000000     C* TESTATA
089800010510     C                   WRITE     SE05PT
089900000000     C* DESCRIZIONE
090000000000     C                   MOVEL     DESCON        PDDACC
090100010510     C                   WRITE     SE05PD
090200000000     C* INTESTAZIONE
090300010510     C                   WRITE     SE05PI
090400000000     C* DETTAGLIO
090500000000DO  2C     1             DO        51            F
090600000000IF  3C     CNSP(F)       IFGT      *ZEROS
090700000000     C     CNCL(F)       ORGT      *ZEROS
090800990806     C     CPKF(F)       ORGT      *ZEROS
090900000000     C     CVLF(F)       ORGT      *ZEROS
091000000000     C     CRIC(F)       ORGT      *ZEROS
091100000000     C     CCOM(F)       ORGT      *ZEROS
091200000000     C                   MOVEL     DRI(F)        P1DDES
091300000000     C                   Z-ADD     CNSP(F)       P1CNSP
091400000000     C                   Z-ADD     CNCL(F)       P1CNCL
091500990806     C                   Z-ADD     CPKF(F)       P1CPKB
091600991130     C                   Z-ADD     CVLF(F)       P1CVLF
091700991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
091800991130     C     VALCNT        IFEQ      'ITL'
091900041103     C                   Z-ADD(H)  CRIC(F)       ITLPOR           15 0
092000991130     C                   Z-ADD     ITLPOR        CRIC(F)
092100010510     C                   Z-ADD(H)  CCOM(F)       ITLPOR
092200991130     C                   Z-ADD     ITLPOR        CCOM(F)
092300991130     C                   ENDIF
092400991130     C                   Z-ADD     CRIC(F)       P1CRIC
092500090925     C                   Z-ADD(h)  CCOM(F)       P1CCOM
092600010510     C                   WRITE     SE05P1
092700000000E   3C                   ENDIF
092800000000E   2C                   ENDDO
092900000000     C* RIGA
093000010510     C                   WRITE     SE05RI
093100000000     C***
093200000000     C* SENZA ACCESSORI
093300000000     C***
093400000000     C* TESTATA
093500010510     C                   WRITE     SE05PT
093600000000     C* DESCRIZIONE
093700000000     C                   MOVEL     DESSEN        PDDACC
093800010510     C                   WRITE     SE05PD
093900000000     C* INTESTAZIONE
094000010510     C                   WRITE     SE05PI
094100000000     C* DETTAGLIO
094200000000DO  2C     1             DO        51            F
094300000000IF  3C     SNSP(F)       IFGT      *ZEROS
094400000000     C     SNCL(F)       ORGT      *ZEROS
094500990806     C     SPKF(F)       ORGT      *ZEROS
094600000000     C     SVLF(F)       ORGT      *ZEROS
094700000000     C     SRIC(F)       ORGT      *ZEROS
094800000000     C     SCOM(F)       ORGT      *ZEROS
094900000000     C                   MOVEL     DRI(F)        P1DDES
095000000000     C                   Z-ADD     SNSP(F)       P1CNSP
095100000000     C                   Z-ADD     SNCL(F)       P1CNCL
095200990806     C                   Z-ADD     SPKF(F)       P1CPKB
095300991130     C                   Z-ADD     SVLF(F)       P1CVLF
095400991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
095500991130     C     VALCNT        IFEQ      'ITL'
095600010510     C                   Z-ADD(H)  SRIC(F)       ITLPOR
095700991130     C                   Z-ADD     ITLPOR        SRIC(F)
095800010510     C                   Z-ADD(H)  SCOM(F)       ITLPOR
095900991130     C                   Z-ADD     ITLPOR        SCOM(F)
096000991130     C                   ENDIF
096100991130     C                   Z-ADD     SRIC(F)       P1CRIC
096200090925     C                   Z-ADD(h)  SCOM(F)       P1CCOM
096300010510     C                   WRITE     SE05P1
096400000000E   3C                   ENDIF
096500000000E   2C                   ENDDO
096600000000     C* RIGA
096700010510     C                   WRITE     SE05RI
096800000000E   1C                   ENDIF
096900000000     C*
097000000000     C* STAMPA RIGA DI FINE PROSPETTO
097100010510     C                   WRITE     SE05FI
097200000000     C*
097300000000     C                   ENDSR
097400000000     C*------------------------------------------------------------------------*
097500000000     C* STACOL - STAMPA LA STATISTICA FASCE DI COLLI
097600000000     C*------------------------------------------------------------------------*
097700000000     C     STACOL        BEGSR
097800000000     C*
097900000000     C* PULISCE I FORMATI RECORD DEL PRINTER FILE
098000010510     C                   CLEAR                   SE06PA
098100010510     C                   CLEAR                   SE06PI
098200010510     C                   CLEAR                   SE06P1
098300000000     C*
098400000000     C* STAMPA PARAMETRI DI LANCIO
098500000000     C                   EXSR      STAPAC
098600000000IF  1C     PASGEN        IFEQ      '1'
098700000000     C*
098800000000     C* PER CIASCUNA PERCENTUALE MEMORIZZATA
098900000000DO  2C     1             DO        4             I
099000000000     C     I             OCCUR     GPER
099100000000     C***
099200000000     C* CON ACCESSORI
099300000000     C***
099400000000     C* TESTATA
099500010510     C                   WRITE     SE06PT
099600000000     C* DESCRIZIONE
099700000000     C                   MOVEL     DESCON        PDDACC
099800010510     C                   WRITE     SE06PD
099900000000     C* INTESTAZIONE
100000000000     C                   MOVEL     TCO(I)        PICPER
100100010510     C                   WRITE     SE06PI
100200000000     C* DETTAGLIO
100300000000DO  3C     1             DO        7             F
100400000000IF  4C     CLI(F)        IFGT      *ZEROS
100500000000     C     PNS(F)        ORGT      *ZEROS
100600000000     C     PNC(F)        ORGT      *ZEROS
100700000000     C     PPK(F)        ORGT      *ZEROS
100800000000     C     PVL(F)        ORGT      *ZEROS
100900000000     C     PPO(F)        ORGT      *ZEROS
101000000000     C     PAC(F)        ORGT      *ZEROS
101100000000     C     PCO(F)        ORGT      *ZEROS
101200000000     C     RNS(F)        ORGT      *ZEROS
101300000000     C     RNC(F)        ORGT      *ZEROS
101400000000     C     RPK(F)        ORGT      *ZEROS
101500000000     C     RVL(F)        ORGT      *ZEROS
101600000000     C     RPO(F)        ORGT      *ZEROS
101700000000     C     RAC(F)        ORGT      *ZEROS
101800000000     C     RCO(F)        ORGT      *ZEROS
101900000000     C                   MOVEL     DCO(F)        P1DDES
102000000000     C                   Z-ADD     CLI(F)        P1CCLI
102100000000     C                   Z-ADD     PNS(F)        P1CPNS
102200000000     C                   Z-ADD     PNC(F)        P1CPNC
102300000000     C                   Z-ADD     PPK(F)        P1CPPK
102400991130     C                   Z-ADD     PVL(F)        P1CPVL
102500000000     C     PPO(F)        ADD       PAC(F)        P1CPRI
102600000000     C                   Z-ADD     PCO(F)        P1CPCO
102700000000     C                   Z-ADD     RNS(F)        P1CRNS
102800000000     C                   Z-ADD     RNC(F)        P1CRNC
102900000000     C                   Z-ADD     RPK(F)        P1CRPK
103000991130     C                   Z-ADD     RVL(F)        P1CRVL
103100991130     C     RPO(F)        ADD       RAC(F)        P1CRRI
103200991130     C                   Z-ADD     RCO(F)        P1CRCO
103300991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
103400991130     C     VALCNT        IFEQ      'ITL'
103500010510     C                   Z-ADD(H)  P1CRRI        ITLPOR
103600991130     C                   Z-ADD     ITLPOR        P1CRRI
103700010510     C                   Z-ADD(H)  P1CRCO        ITLPOR
103800991130     C                   Z-ADD     ITLPOR        P1CRCO
103900991130     C                   ENDIF
104000010510     C                   WRITE     SE06P1
104100000000E   4C                   ENDIF
104200000000E   3C                   ENDDO
104300000000     C* RIGA
104400010510     C                   WRITE     SE06RI
104500000000     C***
104600000000     C* SENZA ACCESSORI
104700000000     C***
104800000000     C* DESCRIZIONE
104900000000     C                   MOVEL     DESSEN        PDDACC
105000010510     C                   WRITE     SE06PD
105100000000     C* INTESTAZIONE
105200010510     C                   WRITE     SE06PI
105300000000     C* DETTAGLIO
105400000000DO  3C     1             DO        7             F
105500000000IF  4C     CLI(F)        IFGT      *ZEROS
105600000000     C     PNS(F)        ORGT      *ZEROS
105700000000     C     PNC(F)        ORGT      *ZEROS
105800000000     C     PPK(F)        ORGT      *ZEROS
105900000000     C     PVL(F)        ORGT      *ZEROS
106000000000     C     PPO(F)        ORGT      *ZEROS
106100000000     C     PAC(F)        ORGT      *ZEROS
106200000000     C     PCO(F)        ORGT      *ZEROS
106300000000     C     RNS(F)        ORGT      *ZEROS
106400000000     C     RNC(F)        ORGT      *ZEROS
106500000000     C     RPK(F)        ORGT      *ZEROS
106600000000     C     RVL(F)        ORGT      *ZEROS
106700000000     C     RPO(F)        ORGT      *ZEROS
106800000000     C     RAC(F)        ORGT      *ZEROS
106900000000     C     RCO(F)        ORGT      *ZEROS
107000000000     C                   MOVEL     DCO(F)        P1DDES
107100000000     C                   Z-ADD     CLI(F)        P1CCLI
107200000000     C                   Z-ADD     PNS(F)        P1CPNS
107300000000     C                   Z-ADD     PNC(F)        P1CPNC
107400000000     C                   Z-ADD     PPK(F)        P1CPPK
107500991130     C                   Z-ADD     PVL(F)        P1CPVL
107600000000     C                   Z-ADD     PPO(F)        P1CPRI
107700000000     C                   Z-ADD     PCO(F)        P1CPCO
107800000000     C                   Z-ADD     RNS(F)        P1CRNS
107900000000     C                   Z-ADD     RNC(F)        P1CRNC
108000000000     C                   Z-ADD     RPK(F)        P1CRPK
108100991130     C                   Z-ADD     RVL(F)        P1CRVL
108200991130     C* SE VALUTA DI CONTO INTERNA E' 'ITL' (LIRE) FORZO I ZERO DECIMALI
108300991130     C     VALCNT        IFEQ      'ITL'
108400010510     C                   Z-ADD(H)  RPO(F)        ITLPOR
108500991130     C                   Z-ADD     ITLPOR        RPO(F)
108600010510     C                   Z-ADD(H)  RCO(F)        ITLPOR
108700991130     C                   Z-ADD     ITLPOR        RCO(F)
108800991130     C                   ENDIF
108900991130     C                   Z-ADD     RPO(F)        P1CRRI
109000991130     C                   Z-ADD     RCO(F)        P1CRCO
109100010510     C                   WRITE     SE06P1
109200000000E   4C                   ENDIF
109300000000E   3C                   ENDDO
109400000000     C* RIGA
109500010510     C                   WRITE     SE06RI
109600000000     C*
109700000000E   2C                   ENDDO
109800000000E   1C                   ENDIF
109900000000     C*
110000000000     C* STAMPA RIGA DI FINE PROSPETTO
110100010510     C                   WRITE     SE06FI
110200000000     C*
110300000000     C                   ENDSR
110400000000     C*------------------------------------------------------------------------*
110500000000     C* STAPAR - STAMPA PAGINA PARAMETRI STATISTICA FASCE DI RICAVO
110600000000     C*------------------------------------------------------------------------*
110700000000     C     STAPAR        BEGSR
110800000000     C*
110900000000     C                   Z-ADD     PARDAI        PACDAI
111000000000     C                   Z-ADD     PARDAF        PACDAF
111100000000     C*
111200010510     C                   WRITE     SE05PT
111300010510     C                   WRITE     SE05PA
111400000000     C*
111500000000     C                   ENDSR
111600000000     C*------------------------------------------------------------------------*
111700000000     C* STAPAC - STAMPA PAGINA PARAMETRI STATISTICA FASCE DI COLLI
111800000000     C*------------------------------------------------------------------------*
111900000000     C     STAPAC        BEGSR
112000000000     C*
112100000000     C                   Z-ADD     PARDAI        PACDAI
112200000000     C                   Z-ADD     PARDAF        PACDAF
112300000000     C*
112400010510     C                   WRITE     SE06PT
112500010510     C                   WRITE     SE06PA
112600000000     C*
112700000000     C                   ENDSR
112800000000     C*------------------------------------------------------------------------*
112900000000     C* STAPAB - STAMPA PAGINA PARAMETRI STATISTICA BANCALI
113000000000     C*------------------------------------------------------------------------*
113100091016     C     STAPAB        BEGSR
113200000000     C*
113300091016     C                   Z-ADD     PARDAI        PACDAI
113400091016     C                   Z-ADD     PARDAF        PACDAF
113500000000     C*
113600091016     C                   WRITE     SE07PT
113700091016     C                   WRITE     SE07PA
113800000000     C*
113900091016     C                   ENDSR
114000000000     C*------------------------------------------------------------------------*
114100000000     C* NEWKSC - OPERAZIONI PER NUOVO CLIENTE
114200000000     C*------------------------------------------------------------------------*
114300000000     C     NEWKSC        BEGSR
114400000000     C*
114500000000     C* DEPOSITA IL NUOVO CODICE
114600000000     C                   Z-ADD     TASKSC        DEPKSC
114700990806     C*
114800990806     C* RECUPERA LA RAGIONE SOCIALE
114900990806     C                   MOVEL     *BLANKS       DESKSC           30
115000990806     C                   MOVE      DEPKSC        N4                4 0
115100990806     C                   MOVEL     TASTBL        A1                1
115200990806IF  1C     N4            IFNE      8888
115300980116     C                   CLEAR                   BS69DS
115400980116     C                   CLEAR                   ACODS
115500980116     C                   MOVEL     KNSIF         I69SIF
115600980116     C                   MOVEL     DEPKSC        I69KAC
115700990806     C                   CALL      'TIBS69R'
115800980116     C                   PARM                    BS69DS
115900980116     C                   PARM                    ACODS
116000980116     C                   PARM                    INDDS
116100980116     C                   PARM                    CLPDS
116200980116     C                   PARM                    CLSDS
116300990806IF  2C     O69ERR        IFNE      '1'
116400980116     C                   MOVEL     ACORAG        DESKSC
116500990806E   2C                   ENDIF
116600990806X   1C                   ELSE
116700990806IF  2C     A1            IFEQ      'F'
116800990806     C                   Z-ADD     TASAAS        KAAAAS
116900990806     C                   Z-ADD     TASLNP        KAALNP
117000990806     C                   Z-ADD     TASNRS        KAANRS
117100990806     C                   Z-ADD     TASNSP        KAANSP
117200990806     C                   MOVEL     'M'           KAATRC
117300990806     C     KEYTAA        CHAIN     TITAA30C                           98
117400990806IF  3C     *IN98         IFEQ      *OFF
117500990806     C                   MOVEL     TAARSC        DESKSC
117600990806E   3C                   ENDIF
117700990806X   2C                   ELSE
117800990806     C                   MOVEL     TASRSD        DESKSC
117900990806E   2C                   ENDIF
118000990806E   1C                   ENDIF
118100000000     C*----
118200000000     C* STATISTICA COLLI
118300000000     C*----
118400000000     C* REIMPOSTA I CAMPI DI MEMORIZZAZIONE
118500000000     C                   Z-ADD     *ZEROS        TOKNSP           15 0
118600000000     C                   CLEAR                   PNSP
118700000000     C                   CLEAR                   PNCL
118800990806     C                   CLEAR                   PPKF
118900000000     C                   CLEAR                   PVLF
119000000000     C                   CLEAR                   PPOR
119100000000     C                   CLEAR                   PACC
119200000000     C                   CLEAR                   PCOM
119300000000     C*
119400000000     C                   ENDSR
119500000000     C*------------------------------------------------------------------------*
119600000000     C* NEWMES - OPERAZIONI PER NUOVO MESE
119700000000     C*------------------------------------------------------------------------*
119800000000     C     NEWMES        BEGSR
119900000000     C*
120000000000     C* DEPOSITA IL NUOVO MESE
120100000000     C                   Z-ADD     TA2MES        DEPMES
120200000000     C*
120300000000     C* CALCOLA LA MEDIA PESO/COLLO DEL CLIENTE
120400000000     C                   EXSR      CALMED
120500000000     C*
120600000000     C                   ENDSR
120700000000     C*------------------------------------------------------------------------*
120800000000     C* CALMED - CALCOLA LA MEDIA PESO/COLLO
120900000000     C*------------------------------------------------------------------------*
121000000000     C     CALMED        BEGSR
121100000000     C*
121200000000     C                   Z-ADD     *ZEROS        DEPMED
121300041227
121400041228     c                   move      tasaas        waaf
121500041228     c                   movel     tasmgs        wmmf
121600041227     c                   move      01            wggf
121700041227
121800041227     c     *iso          move      wdataf        dataiso
121900041228     c                   subdur    2:*m          dataiso
122000041227     c                   extrct    dataiso:*m    wmmi
122100041227     c                   extrct    dataiso:*y    waai
122200041227
122300041227     C                   clear                   TISE70DS
122400041227     C                   eval      I70TLA = 'S'
122500041227     C                   eval      I70TLE = 'ST'
122600041227     C                   eval      I70KSC = depksc
122700041227     C                   eval      I70DAF = waammf
122800041227     C                   eval      I70DAI = waammi
122900041227
123000041227     C                   call      'TISE70R'
123100041227     c                   parm                    TISE70DS
123200041227
123300041227     c                   If        o70nco > *Zeros
123400041227     c     o70pkg        div       o70nco        depmed
123500041227     c                   EndIf
123600000000     C*
123700000000     C                   ENDSR
123800000000     C*------------------------------------------------------------------------*
123900020702     C* CALFSR - CALCOLA LA FASCIA DI RICAVO
124000000000     C*------------------------------------------------------------------------*
124100000000     C     CALFSR        BEGSR
124200000000     C*
124300000000IF  1C     RICAVO        IFLE      1000
124400000000     C                   Z-ADD     1             F
124500000000E   1C                   ENDIF
124600000000IF  1C     RICAVO        IFGT      1000
124700000000     C     RICAVO        ANDLE     2000
124800000000     C                   Z-ADD     2             F
124900000000E   1C                   ENDIF
125000000000IF  1C     RICAVO        IFGT      2000
125100000000     C     RICAVO        ANDLE     3000
125200000000     C                   Z-ADD     3             F
125300000000E   1C                   ENDIF
125400000000IF  1C     RICAVO        IFGT      3000
125500000000     C     RICAVO        ANDLE     4000
125600000000     C                   Z-ADD     4             F
125700000000E   1C                   ENDIF
125800000000IF  1C     RICAVO        IFGT      4000
125900000000     C     RICAVO        ANDLE     5000
126000000000     C                   Z-ADD     5             F
126100000000E   1C                   ENDIF
126200000000IF  1C     RICAVO        IFGT      5000
126300000000     C     RICAVO        ANDLE     6000
126400000000     C                   Z-ADD     6             F
126500000000E   1C                   ENDIF
126600000000IF  1C     RICAVO        IFGT      6000
126700000000     C     RICAVO        ANDLE     7000
126800000000     C                   Z-ADD     7             F
126900000000E   1C                   ENDIF
127000000000IF  1C     RICAVO        IFGT      7000
127100000000     C     RICAVO        ANDLE     8000
127200000000     C                   Z-ADD     8             F
127300000000E   1C                   ENDIF
127400000000IF  1C     RICAVO        IFGT      8000
127500000000     C     RICAVO        ANDLE     9000
127600000000     C                   Z-ADD     9             F
127700000000E   1C                   ENDIF
127800000000IF  1C     RICAVO        IFGT      9000
127900000000     C     RICAVO        ANDLE     10000
128000000000     C                   Z-ADD     10            F
128100000000E   1C                   ENDIF
128200000000IF  1C     RICAVO        IFGT      10000
128300000000     C     RICAVO        ANDLE     11000
128400000000     C                   Z-ADD     11            F
128500000000E   1C                   ENDIF
128600000000IF  1C     RICAVO        IFGT      11000
128700000000     C     RICAVO        ANDLE     12000
128800000000     C                   Z-ADD     12            F
128900000000E   1C                   ENDIF
129000000000IF  1C     RICAVO        IFGT      12000
129100000000     C     RICAVO        ANDLE     13000
129200000000     C                   Z-ADD     13            F
129300000000E   1C                   ENDIF
129400000000IF  1C     RICAVO        IFGT      13000
129500000000     C     RICAVO        ANDLE     14000
129600000000     C                   Z-ADD     14            F
129700000000E   1C                   ENDIF
129800000000IF  1C     RICAVO        IFGT      14000
129900000000     C     RICAVO        ANDLE     15000
130000000000     C                   Z-ADD     15            F
130100000000E   1C                   ENDIF
130200000000IF  1C     RICAVO        IFGT      15000
130300000000     C     RICAVO        ANDLE     16000
130400000000     C                   Z-ADD     16            F
130500000000E   1C                   ENDIF
130600000000IF  1C     RICAVO        IFGT      16000
130700000000     C     RICAVO        ANDLE     17000
130800000000     C                   Z-ADD     17            F
130900000000E   1C                   ENDIF
131000000000IF  1C     RICAVO        IFGT      17000
131100000000     C     RICAVO        ANDLE     18000
131200000000     C                   Z-ADD     18            F
131300000000E   1C                   ENDIF
131400000000IF  1C     RICAVO        IFGT      18000
131500000000     C     RICAVO        ANDLE     19000
131600000000     C                   Z-ADD     19            F
131700000000E   1C                   ENDIF
131800000000IF  1C     RICAVO        IFGT      19000
131900000000     C     RICAVO        ANDLE     20000
132000000000     C                   Z-ADD     20            F
132100000000E   1C                   ENDIF
132200000000IF  1C     RICAVO        IFGT      20000
132300000000     C     RICAVO        ANDLE     21000
132400000000     C                   Z-ADD     21            F
132500000000E   1C                   ENDIF
132600000000IF  1C     RICAVO        IFGT      21000
132700000000     C     RICAVO        ANDLE     22000
132800000000     C                   Z-ADD     22            F
132900000000E   1C                   ENDIF
133000000000IF  1C     RICAVO        IFGT      22000
133100000000     C     RICAVO        ANDLE     23000
133200000000     C                   Z-ADD     23            F
133300000000E   1C                   ENDIF
133400000000IF  1C     RICAVO        IFGT      23000
133500000000     C     RICAVO        ANDLE     24000
133600000000     C                   Z-ADD     24            F
133700000000E   1C                   ENDIF
133800000000IF  1C     RICAVO        IFGT      24000
133900000000     C     RICAVO        ANDLE     25000
134000000000     C                   Z-ADD     25            F
134100000000E   1C                   ENDIF
134200000000IF  1C     RICAVO        IFGT      25000
134300000000     C     RICAVO        ANDLE     26000
134400000000     C                   Z-ADD     26            F
134500000000E   1C                   ENDIF
134600000000IF  1C     RICAVO        IFGT      26000
134700000000     C     RICAVO        ANDLE     27000
134800000000     C                   Z-ADD     27            F
134900000000E   1C                   ENDIF
135000000000IF  1C     RICAVO        IFGT      27000
135100000000     C     RICAVO        ANDLE     28000
135200000000     C                   Z-ADD     28            F
135300000000E   1C                   ENDIF
135400000000IF  1C     RICAVO        IFGT      28000
135500000000     C     RICAVO        ANDLE     29000
135600000000     C                   Z-ADD     29            F
135700000000E   1C                   ENDIF
135800000000IF  1C     RICAVO        IFGT      29000
135900000000     C     RICAVO        ANDLE     30000
136000000000     C                   Z-ADD     30            F
136100000000E   1C                   ENDIF
136200000000IF  1C     RICAVO        IFGT      30000
136300000000     C     RICAVO        ANDLE     31000
136400000000     C                   Z-ADD     31            F
136500000000E   1C                   ENDIF
136600000000IF  1C     RICAVO        IFGT      31000
136700000000     C     RICAVO        ANDLE     32000
136800000000     C                   Z-ADD     32            F
136900000000E   1C                   ENDIF
137000000000IF  1C     RICAVO        IFGT      32000
137100000000     C     RICAVO        ANDLE     33000
137200000000     C                   Z-ADD     33            F
137300000000E   1C                   ENDIF
137400000000IF  1C     RICAVO        IFGT      33000
137500000000     C     RICAVO        ANDLE     34000
137600000000     C                   Z-ADD     34            F
137700000000E   1C                   ENDIF
137800000000IF  1C     RICAVO        IFGT      34000
137900000000     C     RICAVO        ANDLE     35000
138000000000     C                   Z-ADD     35            F
138100000000E   1C                   ENDIF
138200000000IF  1C     RICAVO        IFGT      35000
138300000000     C     RICAVO        ANDLE     36000
138400000000     C                   Z-ADD     36            F
138500000000E   1C                   ENDIF
138600000000IF  1C     RICAVO        IFGT      36000
138700000000     C     RICAVO        ANDLE     37000
138800000000     C                   Z-ADD     37            F
138900000000E   1C                   ENDIF
139000000000IF  1C     RICAVO        IFGT      37000
139100000000     C     RICAVO        ANDLE     38000
139200000000     C                   Z-ADD     38            F
139300000000E   1C                   ENDIF
139400000000IF  1C     RICAVO        IFGT      38000
139500000000     C     RICAVO        ANDLE     39000
139600000000     C                   Z-ADD     39            F
139700000000E   1C                   ENDIF
139800000000IF  1C     RICAVO        IFGT      39000
139900000000     C     RICAVO        ANDLE     40000
140000000000     C                   Z-ADD     40            F
140100000000E   1C                   ENDIF
140200000000IF  1C     RICAVO        IFGT      40000
140300000000     C     RICAVO        ANDLE     41000
140400000000     C                   Z-ADD     41            F
140500000000E   1C                   ENDIF
140600000000IF  1C     RICAVO        IFGT      41000
140700000000     C     RICAVO        ANDLE     42000
140800000000     C                   Z-ADD     42            F
140900000000E   1C                   ENDIF
141000000000IF  1C     RICAVO        IFGT      42000
141100000000     C     RICAVO        ANDLE     43000
141200000000     C                   Z-ADD     43            F
141300000000E   1C                   ENDIF
141400000000IF  1C     RICAVO        IFGT      43000
141500000000     C     RICAVO        ANDLE     44000
141600000000     C                   Z-ADD     44            F
141700000000E   1C                   ENDIF
141800000000IF  1C     RICAVO        IFGT      44000
141900000000     C     RICAVO        ANDLE     45000
142000000000     C                   Z-ADD     45            F
142100000000E   1C                   ENDIF
142200000000IF  1C     RICAVO        IFGT      45000
142300000000     C     RICAVO        ANDLE     46000
142400000000     C                   Z-ADD     46            F
142500000000E   1C                   ENDIF
142600000000IF  1C     RICAVO        IFGT      46000
142700000000     C     RICAVO        ANDLE     47000
142800000000     C                   Z-ADD     47            F
142900000000E   1C                   ENDIF
143000000000IF  1C     RICAVO        IFGT      47000
143100000000     C     RICAVO        ANDLE     48000
143200000000     C                   Z-ADD     48            F
143300000000E   1C                   ENDIF
143400000000IF  1C     RICAVO        IFGT      48000
143500000000     C     RICAVO        ANDLE     49000
143600000000     C                   Z-ADD     49            F
143700000000E   1C                   ENDIF
143800000000IF  1C     RICAVO        IFGT      49000
143900000000     C     RICAVO        ANDLE     50000
144000000000     C                   Z-ADD     50            F
144100000000E   1C                   ENDIF
144200000000IF  1C     RICAVO        IFGT      50000
144300000000     C                   Z-ADD     51            F
144400000000E   1C                   ENDIF
144500000000     C*
144600000000     C                   ENDSR
144700000000     C*------------------------------------------------------------------------*
144800000000     C* CALFSC - CALCOLA LA FASCIA DI COLLI
144900000000     C*------------------------------------------------------------------------*
145000000000     C     CALFSC        BEGSR
145100000000     C*
145200000000IF  1C     COLLI         IFLE      1
145300000000     C                   Z-ADD     1             F
145400000000E   1C                   ENDIF
145500000000IF  1C     COLLI         IFGT      1
145600000000     C     COLLI         ANDLE     2
145700000000     C                   Z-ADD     2             F
145800000000E   1C                   ENDIF
145900000000IF  1C     COLLI         IFGT      2
146000000000     C     COLLI         ANDLE     3
146100000000     C                   Z-ADD     3             F
146200000000E   1C                   ENDIF
146300000000IF  1C     COLLI         IFGT      3
146400000000     C     COLLI         ANDLE     4
146500000000     C                   Z-ADD     4             F
146600000000E   1C                   ENDIF
146700000000IF  1C     COLLI         IFGT      4
146800000000     C     COLLI         ANDLE     5
146900000000     C                   Z-ADD     5             F
147000000000E   1C                   ENDIF
147100000000IF  1C     COLLI         IFGT      5
147200000000     C     COLLI         ANDLE     6
147300000000     C                   Z-ADD     6             F
147400000000E   1C                   ENDIF
147500000000IF  1C     COLLI         IFGT      6
147600000000     C                   Z-ADD     7             F
147700000000E   1C                   ENDIF
147800000000     C*
147900000000     C                   ENDSR
148000000000     C*------------------------------------------------------------------------*
148100000000     C* CARTAB - CARICA LE TABELLE
148200000000     C*------------------------------------------------------------------------*
148300000000     C     CARTAB        BEGSR
148400000000     C*--------------------
148500000000     C* TIPI BOLLA
148600000000     C*--------------------
148700000000     C                   Z-ADD     *ZEROS        I                 4 0
148800020906     C                   Z-ADD     1             KTBKUT
148900000000     C                   MOVEL     'TB'          KTBCOD
149000000000     C     KE2TAB        SETLL     TABEL00F
149100000000     C     KE2TAB        READE     TABEL00F                               98
149200000000DO  1C     *IN98         DOWEQ     *OFF
149300000000IF  2C     TBLFLG        IFEQ      *BLANKS
149400000000     C                   ADD       1             I
149500000000     C                   MOVEL     TBLKEY        TBL(I)
149600000000     C                   MOVEL     TBLUNI        DSTB
149700000000     C                   MOVEL     §TBRBL        RBL(I)
149800000000E   2C                   ENDIF
149900000000     C     KE2TAB        READE     TABEL00F                               98
150000000000E   1C                   ENDDO
150100000000     C*--------------------
150200000000     C* CAMBIO ECU
150300000000     C*--------------------
150400020906     C                   Z-ADD     1             KTBKUT
150500000000     C                   MOVEL     'CV'          KTBCOD
150600000000     C                   MOVEL(P)  'XEU'         KTBKEY
150700000000     C     KEYTAB        CHAIN     TABEL00F                           98
150800000000IF  1C     *IN98         IFEQ      *OFF
150900000000     C     TBLFLG        ANDEQ     *BLANKS
151000000000     C                   MOVEL     TBLUNI        DSCV
151100000000     C                   Z-ADD     §CVCMB        ECUCMB            9 3
151200000000X   1C                   ELSE
151300000000     C                   Z-ADD     100           ECUCMB            9 3
151400000000E   1C                   ENDIF
151500971204     C*--------------------
151600971204     C* ORGANIGRAMMA
151700971204     C*--------------------
151800971204     C                   Z-ADD     *ZEROS        I                 4 0
151900971204     C     *LOVAL        SETLL     AZORG01L
152000971204     C                   READ      AZORG01L                               98
152100971204DO  1C     *IN98         DOWEQ     *OFF
152200971204IF  2C     ORGFVA        IFEQ      *BLANKS
152300971204     C     ORGFAG        ANDNE     'V'
152400000614     C                   MOVEL     ORGDE3        OG143
152500971215     C                   ADD       1             I
152600020627 MOM C*                  IF        §OGNTW = 'PPT'
152700020627     C*                  Z-ADD     ORGFIL        PFIL(I)
152800020627     C*                  MOVEL     ORGFL1        PFL1(I)
152900020627     C*                  ELSE
153000000614     C                   Z-ADD     ORGFIL        FIL(I)
153100020702     C*
153200020627     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
153300020702     C                   CLEAR                   DNTW
153400020702     C*
153500020702     C                   MOVEL(P)  'NTW'         KTECOD
153600020702     C                   MOVEL(P)  §OGNTW        KTEKE1
153700020702     C     KEYTBE        CHAIN     TNTBE01L
153800020702     C                   IF        %found(TNTBE01L)
153900020702     C                   MOVEL     TBEUNI        DNTW
154000020702     C                   ENDIF
154100020702     C*
154200020702     C                   MOVEL     §NTWFIE       FL1(I)
154300020702     C                   IF        §OGNTW = 'DPD'
154400020702     C                   MOVEL     'E'           FL1(I)
154500020702     C                   ENDIF
154600020627     C*
154700000619     C*                  ENDIF
154800971204E   2C                   ENDIF
154900971204     C                   READ      AZORG01L                               98
155000971204E   1C                   ENDDO
155100971204     C*
155200000000     C                   ENDSR
155300000000     C*------------------------------------------------------------------------*
155400000000     C* OPNPRT - APRE I FILE PRINTER DESIDERATI
155500000000     C*------------------------------------------------------------------------*
155600000000     C     OPNPRT        BEGSR
155700000000     C*
155800000000IF  1C     PARRIC        IFEQ      '1'
155900010510     C                   OPEN      TISE05P
156000000000E   1C                   ENDIF
156100000000IF  1C     PARCOL        IFEQ      '1'
156200010510     C                   OPEN      TISE06P
156300000000E   1C                   ENDIF
156400091016IF  1C     PARBAN        IFEQ      '1'
156500091016     C                   OPEN      TISE07P
156600091016E   1C                   ENDIF
156700000000     C*
156800000000     C                   ENDSR
156900000000     C*------------------------------------------------------------------------*
157000980116     C* FINSR - OPERAZIONI FINALI
157100000000     C*------------------------------------------------------------------------*
157200980116     C     FINSR         BEGSR
157300000000     C*
157400980116     C* CHIUDE I FILE PRINTER APERTI
157500000000IF  1C     PARRIC        IFEQ      '1'
157600010510     C                   CLOSE     TISE05P
157700000000E   1C                   ENDIF
157800000000IF  1C     PARCOL        IFEQ      '1'
157900010510     C                   CLOSE     TISE06P
158000000000E   1C                   ENDIF
158100091016IF  1C     PARBAN        IFEQ      '1'
158200091016     C                   WRITE     SE07FI
158300091016     C                   CLOSE     TISE07P
158400091016E   1C                   ENDIF
158500980116     C*
158600980116     C* CHIUDE I FILE DEI PGM CHIAMATI
158700980116     C                   CLEAR                   BS69DS
158800980116     C                   CLEAR                   ACODS
158900980116     C                   CLEAR                   INDDS
159000980116     C                   CLEAR                   CLPDS
159100980116     C                   CLEAR                   CLSDS
159200980116     C                   MOVEL     'C'           I69TLA
159300980116     C                   MOVEL     KNSIF         I69SIF
159400980116     C                   CALL      'TIBS69R'
159500980116     C                   PARM                    BS69DS
159600980116     C                   PARM                    ACODS
159700980116     C                   PARM                    INDDS
159800980116     C                   PARM                    CLPDS
159900980116     C                   PARM                    CLSDS
160000000000     C*
160100000000     C                   ENDSR
160200991126     C*-------------------------------------------------------------------*
160300991126     C* CONVERSIONE IN MONETA ED EVENTUALE ARROTONDAMENTO
160400991126     C*-------------------------------------------------------------------*
160500991126     C     CNVEUR        BEGSR
160600991126     C*
160700991126    4C     COMIMP        IFNE      *ZEROS
160800991130     C     COMDIV        ANDNE     VALCNT
160900991126     C                   CLEAR                   YEUR
161000991126     C                   MOVEL     COMDIV        YECDVI
161100991130     C                   MOVEL     VALCNT        YECDVO
161200991126     C                   Z-ADD     COMIMP        YECIMI
161300991126     C                   MOVE      '3'           YECDCO
161400991126     C*
161500991126     C                   CALL      'YEURCO'
161600991126     C                   PARM                    YEUR
161700991126     C*
161800991126    5C     YECESI        IFEQ      ' '
161900991126     C                   Z-ADD     YECIMO        COMIMP           30 9
162000991126     C                   ELSE
162100991126     C                   Z-ADD     *ZEROS        COMIMP
162200991126    5C                   ENDIF
162300991126     C*
162400991126    5C                   ENDIF
162500991126     C*
162600991126     C* MANTENGO SEMPRE INIZIALIZZATA LA DIVISA DI INPUT
162700991126     C                   MOVEL     *BLANKS       COMDIV            3
162800991126     C*
162900991126     C                   ENDSR
163000020117     C*------------------------------------------------------------------------*
163100020117     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "ST" E CONSIDERAZIONI SU DIVISE
163200020117     C*------------------------------------------------------------------------*
163300020117     C     CARGEDCE      BEGSR
163400020117     C*
163500020117     C                   CLEAR                   DGEDST
163600020117     C                   MOVEL     *blanks       WDIVCE            3
163700020117     C*
163800020117     C                   MOVEL(P)  'GED'         KTECOD
163900020117     C                   MOVEL(P)  'ST'          KTEKE1
164000020117     C     KEYTBE        CHAIN     TNTBE01L
164100020117     C                   IF        %found(TNTBE01L)
164200020117     C                   MOVEL     TBEUNI        DGEDST
164300020117     C                   ENDIF
164400020117     C*
164500020117     C                   IF        DATCOR < §GEDSTDATR
164600020117     C                   EVAL      WDIVCE = §GEDSTDIPR
164700020117     C                   ELSE
164800020117     C                   EVAL      WDIVCE = §GEDSTDIDR
164900020117     C                   ENDIF
165000020117     C*
165100020117     C                   ENDSR
165200000000     C*------------------------------------------------------------------------*
165300000000     C* *INZSR - OPERAZIONI INIZIALI
165400000000     C*------------------------------------------------------------------------*
165500000000     C     *INZSR        BEGSR
165600000000     C*--------------------
165700000000     C* RICEVIMENTO PARAMETRI
165800000000     C*--------------------
165900000000     C     *ENTRY        PLIST
166000000000     C                   PARM                    KPJBA
166100000000     C                   MOVEL     KPJBU         PARAM
166200000000     C*--------------------
166300000000     C* VARIABILI RIFERITE AL DATA BASE
166400000000     C*--------------------
166500000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
166600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
166700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
166800990805     C     *LIKE         DEFINE    TA7AAS        KT7AAS
166900000000     C     *LIKE         DEFINE    TA7LNP        KT7LNP
167000000000     C     *LIKE         DEFINE    TA7NRS        KT7NRS
167100000000     C     *LIKE         DEFINE    TA7NSP        KT7NSP
167200000000     C     *LIKE         DEFINE    TA7TBL        KT7TBL
167300990806     C     *LIKE         DEFINE    TAAAAS        KAAAAS
167400990806     C     *LIKE         DEFINE    TAALNP        KAALNP
167500990806     C     *LIKE         DEFINE    TAANRS        KAANRS
167600990806     C     *LIKE         DEFINE    TAANSP        KAANSP
167700990806     C     *LIKE         DEFINE    TAATRC        KAATRC
167800000000     C     *LIKE         DEFINE    CETAAS        KCTAAS
167900000000     C     *LIKE         DEFINE    CETLNP        KCTLNP
168000000000     C     *LIKE         DEFINE    CETNRS        KCTNRS
168100000000     C     *LIKE         DEFINE    CETNSP        KCTNSP
168200000000     C     *LIKE         DEFINE    CETTBL        KCTTBL
168300000000     C     *LIKE         DEFINE    TASKSC        DEPKSC
168400971204     C     *LIKE         DEFINE    ORGFL1        DLPFL1
168500971204     C     *LIKE         DEFINE    ORGFL1        DLAFL1
168600020117     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
168700020117     C     *LIKE         DEFINE    TBEKE1        KTEKE1
168800000000     C                   Z-ADD     *ZEROS        DEPMED           11 3
168900000000     C*--------------------
169000000000     C* CHIAVI DI LETTURA
169100000000     C*--------------------
169200000000     C* LETTURA TABEL00F - PARZIALE
169300000000     C     KE2TAB        KLIST
169400000000     C                   KFLD                    KTBKUT
169500000000     C                   KFLD                    KTBCOD
169600000000     C* LETTURA TABEL00F - COMPLETA
169700000000     C     KEYTAB        KLIST
169800000000     C                   KFLD                    KTBKUT
169900000000     C                   KFLD                    KTBCOD
170000000000     C                   KFLD                    KTBKEY
170100990805     C* LETTURA TITA730C - PARZIALE
170200000000     C     KEYTA7        KLIST
170300000000     C                   KFLD                    KT7AAS
170400000000     C                   KFLD                    KT7LNP
170500000000     C                   KFLD                    KT7NRS
170600000000     C                   KFLD                    KT7NSP
170700000000     C                   KFLD                    KT7TBL
170800990806     C* LETTURA TITAA00L - COMPLETA
170900990806     C     KEYTAA        KLIST
171000990806     C                   KFLD                    KAAAAS
171100990806     C                   KFLD                    KAALNP
171200990806     C                   KFLD                    KAANRS
171300990806     C                   KFLD                    KAANSP
171400990806     C                   KFLD                    KAATRC
171500000000     C* LETTURA ECCET30C - COMPLETA
171600000000     C     KEYCET        KLIST
171700000000     C                   KFLD                    KCTAAS
171800000000     C                   KFLD                    KCTLNP
171900000000     C                   KFLD                    KCTNRS
172000000000     C                   KFLD                    KCTNSP
172100000000     C                   KFLD                    KCTTBL
172200020117     C* LETTURA TNTBE01L - PARZIALE
172300020117     C     KEYTBE        KLIST
172400020117     C                   KFLD                    KTECOD                         *UTENTE
172500020117     C                   KFLD                    KTEKE1                         *CODICE
172600090326
172700000000     C*--------------------
172800000000     C* VARIABILI DI CONTROLLO
172900000000     C*--------------------
173000000000     C                   MOVEL     '0'           $FINE             1
173100000000     C                   MOVEL     'N'           WRECOK            1
173200000000     C                   MOVEL     '0'           PASGEN            1
173300000000     C                   MOVEL     '0'           $VOLTB            1
173400000000     C                   MOVEL     '1'           *IN01
173500000000     C                   MOVEL     '1'           *IN02
173600000000     C                   MOVEL     '1'           *IN03
173700020117     C*
173800020117     C* REPERISCE LA DATA CORRENTE
173900020117     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
174000020117     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
174100020117     C                   Z-ADD     WN8           G08DAT
174200020117     C                   Z-ADD     *ZEROS        G08INV
174300020117     C                   MOVEL     '0'           G08ERR
174400020117     C                   CALL      'XSRDA8'
174500020117     C                   PARM                    WLBDA8
174600020117     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
174700020117     C*
174800020117     C* RECUPERA DATI DA TEBELLA "GED" TIPO RECORD "CE"
174900020117     C                   EXSR      CARGEDCE
175000020117     C                   MOVEL     WDIVCE        VALCNT            3
175100000000     C*
175200000000     C* CARICA LE TABELLE
175300000000     C                   EXSR      CARTAB
175400080717
175500080717      * se richiesta la scrittura del file lo pulisco prima di scriverci dei dati
175600080717
175700080717     c                   if        parfil = 'S'
175800080717
175900080717      * se s.i. di prova imposto la libreria di prova
176000080717     c                   if        %subst(knsif:7:1) = 'P'
176100080717     c                   eval      wlib = 'GAITRAAZP '
176200080717     c                   else
176300080717     c                   eval      wlib = 'GAITRAAZM '
176400080717     c                   endif
176500080717      * pulisco il file
176600080717     c                   clear                   comman
176700080717     c                   EVAL      comman = 'CLRPFM FILE('
176800080717     c                             + %TRIMR(WLIB)
176900080717     c                             + '/WFSBA00F)'
177000080717
177100080717     c                   call      'QCMDEXC'
177200080717     c                   parm                    comman
177300080717     c                   parm                    lenght
177400080717
177500080717     c                   open      wfsba00f
177600080717
177700080717     c                   endif
177800080717
177900000000     C*
178000000000     C                   ENDSR
178100000000     O*------------------------------------------------------------------------*
178200000000**   DRI - DECODIFICHE RIGHE DI DETTAGLIO STATISTICA FASCE DI RICAVO
1783000000000/  1.000      1
1784000000001/  2.000      2
1785000000002/  3.000      3
1786000000003/  4.000      4
1787000000004/  5.000      5
1788000000005/  6.000      6
1789000000006/  7.000      7
1790000000007/  8.000      8
1791000000008/  9.000      9
1792000000009/ 10.000      10
17930000000010/11.000      11
17940000000011/12.000      12
17950000000012/13.000      13
17960000000013/14.000      14
17970000000014/15.000      15
17980000000015/16.000      16
17990000000016/17.000      17
18000000000017/18.000      18
18010000000018/19.000      19
18020000000019/20.000      20
18030000000020/21.000      21
18040000000021/22.000      22
18050000000022/23.000      23
18060000000023/24.000      24
18070000000024/25.000      25
18080000000025/26.000      26
18090000000026/27.000      27
18100000000027/28.000      28
18110000000028/29.000      29
18120000000029/30.000      30
18130000000030/31.000      31
18140000000031/32.000      32
18150000000032/33.000      33
18160000000033/34.000      34
18170000000034/35.000      35
18180000000035/36.000      36
18190000000036/37.000      37
18200000000037/38.000      38
18210000000038/39.000      39
18220000000039/40.000      40
18230000000040/41.000      41
18240000000041/42.000      42
18250000000042/43.000      43
18260000000043/44.000      44
18270000000044/45.000      45
18280000000045/46.000      46
18290000000046/47.000      47
18300000000047/48.000      48
18310000000048/49.000      49
18320000000049/50.000      50
183300000000OLTRE 50.000   51
183400000000**   TCO - DECODIFICHE RIGHE DI TESTATA STATISTICA FASCE DI COLLI
18350000000060             1
18360000000070             2
18370000000080             3
18380000000090             4
183900000000               5
184000000000**   DCO - DECODIFICHE RIGHE DI DETTAGLIO STATISTICA FASCE DI COLLI
1841000000000/ 1           1
1842000000000/ 2           2
1843000000000/ 3           3
1844000000000/ 4           4
1845000000000/ 5           5
1846000000000/ 6           6
1847000000000/OLTRE 6      7
