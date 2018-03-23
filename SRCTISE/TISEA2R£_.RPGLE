000100130130     /*END
000200000000     H*------------------------------------------------------------------------*
000300000000     H*                      - STATISTICA DELTA FILIALE -                      *
000400000000     H*------------------------------------------------------------------------*
000500000000     H* INPUT  : -------- - ----------------------------------------------------
000600020117     H*          PARAM    - PARAMETRI PASSATI DALLA GUIDA 'TISE91C'
000700000000     H*                     PARDAT  -  DATA INIZIALE
000800000000     H*                     PARDA2  -  DATA FINALE
000900950208     H*                     PARIOE  -  DELTA ITALIA/ESTERO/TUTTO
001000000614     H*                     PARPT   -  STATISTICA X POSTE O MENO
001100000000     H* OUTPUT : -------- - ----------------------------------------------------
001200010507     H*          TISEA2P  - STATISTICA DELTA PER FASCE DI PESO
001300000000     F*------------------------------------------------------------------------*
001400000000     F* DATA BASE
001500000000     F*------------------------------------------------------------------------*
001600000000     H DECEDIT('0,') DATEDIT(*DMY.)
001700990609     FSISDC01L  IF   E           K DISK
001800950216     FAZORG02L  IF   E           K DISK
001900000000     FTABEL00F  IF   E           K DISK
002000010507     FTISEA2P   O    E             PRINTER OFLIND(*IN01)
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE
002300000000     D*------------------------------------------------------------------------*
002400000000     D*------------------
002500000000     D* DECODIFICA MESI DELL'ANNO
002600000000     D*------------------
002700000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
002800950214     D*-------------------
002900950214     D* FASCE DI PESO
003000950214     D*-------------------
003100020522     D FPE             S              2    DIM(11)                              *CODICI FASCE ESISTENTI
003200020522     D DFPE            S             14    DIM(11)                              *DECODIFICA FASCE
003300950214     D*------------------
003400950214     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
003500950214     D*------------------
003600950214     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
003700950214     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/N)
003800950214     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
003900000000     D*------------------
004000000000     D* ORGANIGRAMMA
004100000000     D*------------------
004200971124     D FIL             S              3  0 DIM(500)                             *CODICI
004300971124     D ARE             S              3  0 DIM(500)
004400971124     D DIV             S              1    DIM(500)
004500971124     D DFIL            S             20    DIM(500)                             *DECODIFICHE
004600971124     D DARE            S             20    DIM(500)
004700971124     D DDIV            S             20    DIM(500)
004800000614     D PIL             S              3  0 DIM(500)                             *CODICI POSTE
004900000614     D DPIL            S             20    DIM(500)                             *DECODIFICHE POSTE
005000020328     D*------------------
005100020328     D* RAGGRUPPAMENTI REGIONI/NETWORK
005200020328     D*------------------
005300020328     D RGREG           S              3  0 DIM(100)                             *CODICI RAGGR. REGIONE
005400020328     D RGNTW           S              3    DIM(100)                             *CODICI NETWORK x RAGGR. REGIONE
005500000000     D*------------------
005600000000     D* MESI DA ELABORARE
005700000000     D*------------------
005800000000     D MES             S              2  0 DIM(12)
005900000000     D*------------------
006000000000     D* ANNI DEI MESI DA ELABORARE
006100000000     D*------------------
006200990524     D ANN             S              4  0 DIM(12)
006300000000    ?D*-------------------
006400950214    ?D* S C H I E R E   M U L T I P L E
006500000000    ?D*-------------------
006600950214     D*-------------------
006700950214     D* DELTA FILIALE
006800950214     D*-------------------
006900950214     D*-------------------
007000950214     D* DELTA AREA
007100950214     D*-------------------
007200950214     D*-------------------
007300950214     D* DELTA DIVISIONE
007400950214     D*-------------------
007500950214     D*-------------------
007600950214     D* DELTA GENERALE
007700950214     D*-------------------
007800950214     D*-------------------
007900950214     D* DELTA IN STAMPA
008000950214     D*-------------------
008100950214    ?D*-------------------
008200000000    ?D* S C H I E R E   N O N   M U L T I P L E
008300000000    ?D*-------------------
008400010515     D TRS             S             19  5 DIM(12)                              STAMPA TOT.RIC.REALE
008500010515     D TPS             S             19  5 DIM(12)                              STAMPA TOT.RIC.PRES.
008600010515     D TSS             S             19  0 DIM(12)                              STAMPA TOT.SPED.
008700010515     D SVA             S             19  5 DIM(12)                              STAMPA VALORI
008800950216     D SDE             S              4  1 DIM(12)                              STAMPA PERC. DELTA
008900000000     D*------------------------------------------------------------------------*
009000000000     D* INPUT
009100000000     D*------------------------------------------------------------------------*
009200950214    ?D*-------------------
009300950214    ?D* S C H I E R E   M U L T I P L E      - 11 FASCE X 12 MESI -
009400950214    ?D*-------------------
009500950214     D*------------------
009600950214     D* NUMERO DI BYTE IN PACKED
009700950214     D*------------------
009800010507     D* (17:2) + 1 = 9 ; (9 X 12) = 108
009900950214     D*------------------
010000950214     D* DELTA FILIALE
010100950214     D*------------------
010200020522     D FDEL            DS                  OCCURS(11) INZ
010300010515     D  DRF                          19  5 DIM(12)                              R REAL
010400010515     D  DPF                          19  5 DIM(12)                              R PRES
010500010515     D  DSF                          17  0 DIM(12)                              SPEDIZ
010600950214     D*------------------
010700950214     D* DELTA AREA
010800950214     D*------------------
010900020522     D ADEL            DS                  OCCURS(11) INZ
011000010515     D  DRA                          19  5 DIM(12)                              R REAL
011100010515     D  DPA                          19  5 DIM(12)                              R PRES
011200010515     D  DSA                          17  0 DIM(12)                              SPEDIZ
011300950214     D*------------------
011400950214     D* DELTA DIVISIONE
011500950214     D*------------------
011600020522     D DDEL            DS                  OCCURS(11) INZ
011700010515     D  DRD                          19  5 DIM(12)                              R REAL
011800010515     D  DPD                          19  5 DIM(12)                              R PRES
011900010515     D  DSD                          17  0 DIM(12)                              SPEDIZ
012000950214     D*------------------
012100950214     D* DELTA GENERALE
012200950214     D*------------------
012300020522     D GDEL            DS                  OCCURS(11) INZ
012400010515     D  DRG                          19  5 DIM(12)                              R REAL
012500010515     D  DPG                          19  5 DIM(12)                              R PRES
012600010515     D  DSG                          17  0 DIM(12)                              SPEDIZ
012700950214     D*------------------
012800950214     D* DELTA IN STAMPA
012900950214     D*------------------
013000020522     D SDEL            DS                  OCCURS(11) INZ
013100010515     D  DRS                          19  5 DIM(12)                              R REAL
013200010515     D  DPS                          19  5 DIM(12)                              R PRES
013300010515     D  DSS                          17  0 DIM(12)                              SPEDIZ
013400950214     D*------------------
013500950214     D* ARCHITETTURA
013600950214     D*------------------
013700000000     D KPJBA         E DS
013800950214     D  PROFIL                 1     10                                         UTENTE
013900950214     D*------------------
014000990609     D* DS UNIONE DATA SISDC01L
014100950214     D*------------------
014200950214     D                 DS
014300990524     D  SDCANN                 1      4  0
014400990524     D  SDCMES                 5      6  0
014500990524     D  SDCDAT                 1      6  0
014600950214     D*------------------
014700950214     D* DS FASCE DI PESO
014800950214     D*------------------
014900950214     D DS2L          E DS
015000950214     D*------------------
015100950214     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
015200950214     D*------------------
015300950214     D DS2W          E DS
015400000000     D*------------------
015500000000     D* PARAMETRI FILTRO
015600000000     D*------------------
015700000000     D PARAM           DS
015800990524     D  PARDAT                 1      6  0
015900990524     D  PARAAI                 1      4  0
016000990524     D  PARMMI                 5      6  0
016100990524     D  PARDA2                 7     12  0
016200990524     D  PARAAF                 7     10  0
016300990524     D  PARMMF                11     12  0
016400990524     D  PARIOE                13     13
016500000614     D  PARPT                 14     14
016600130130     d  ParTOT                15     15                                         *Solo Totali
016700000000     D*------------------
016800020916     D* DS REPERIMENTO DATI UTENTE
016900020916     D*-------------------
017000020916     D TIBS34DS      E DS                                                       *Profili utente
017100020916     D DDATIUTE      E DS                                                       *Dati utente
017200020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
017300000614     D*------------------
017400000614     D* DS LETTURA 143° DESCRIZIO ORGANIGRAMMA (X POSTE)
017500000614     D*------------------
017600000614     D OG143         E DS
017700000614     D*------------------
017800000614     D* DS DI SCOMPOSIZIONE CLIENTE
017900000614     D*------------------
018000000614     D                 DS
018100000614     D  KSCFIL                 1      3  0
018200000614     D  KSCCLI                 4      7  0
018300000614     D  KSCCOD                 1      7  0
018400020328     D*------------------
018500020328     D* DS DI LETTURA TABELLA "2M"
018600020328     D*------------------
018700020328     D DS2M          E DS
018800020523     D*------------------
018900020523     D* VARIABILE INDICANTE IL NUMERO DI FASCE DI PESO
019000020523     D*------------------
019100020523     D FPENUM          S              2  0 INZ(10)
019200000000     C*------------------------------------------------------------------------*
019300950214     C* MAIN LINE
019400000000     C*------------------------------------------------------------------------*
019500000000     C*
019600000000     C* STAMPA PARAMETRI FILTRO
019700000000     C                   EXSR      STAPAR
019800950214     C*
019900950216     C* LETTURA PRIMA FILIALE
020000950216     C                   Z-ADD     1             F                 6 0          *PRIMA FILIALE
020100000000     C*
020200950214     C* CICLO FINO A FINE FILE E FINE FILIALI
020300950216DO  2C     F             DOWLE     ULTFIL
020400000000     C*
020500000000     C* OPERAZIONI PER NUOVA DIVISIONE
020600000000     C                   EXSR      NEWDIV
020700000000     C*
020800000000     C* CICLO FINO A ROTTURA DI DIVISIONE
020900950216DO  3C     F             DOWLE     ULTFIL
021000950214     C     DIV(F)        ANDEQ     DEPDIV
021100000000     C*
021200000000     C* OPERAZIONI PER NUOVA AREA
021300000000     C                   EXSR      NEWARE
021400000000     C*
021500000000     C* CICLO FINO A ROTTURA DI AREA
021600950216DO  4C     F             DOWLE     ULTFIL
021700950214     C     DIV(F)        ANDEQ     DEPDIV
021800950214     C     ARE(F)        ANDEQ     DEPARE
021900000000     C*
022000000000     C* OPERAZIONI PER NUOVA FILIALE
022100000000     C                   EXSR      NEWFIL
022200950214     C*
022300950216     C* LETTURA DEI SALDI DELLA FILIALE
022400950216     C                   EXSR      SETSDC
022500000000     C*
022600950216     C* CICLO FINO A FINE SALDI PER QUESTA FILIALE
022700950216DO  5C     $FINE         DOWEQ     '0'
022800000000     C*
022900000000     C* MEMORIZZA I SALDI
023000000000     C                   EXSR      MEMSAL
023100000000     C*
023200950216     C* LETTURA RECORD SALDI SUCCESSIVO
023300950216     C                   EXSR      LETSDC
023400950214E   5C                   ENDDO
023500950214     C*
023600950214     C* STAMPA IL TOTALE FILIALE
023700130130     c                   if        ParTOT <> 'S'
023800000614     C  N45              EXSR      STAFIL
023900130130     c                   endif
024000950216     C*
024100950216     C* SI POSIZIONA SULLA NUOVA FILIALE
024200950216     C                   ADD       1             F
024300950214E   4C                   ENDDO
024400000000     C*
024500000000     C* STAMPA IL TOTALE AREA
024600000614     C     CHKFIL        IFEQ      '1'
024700130130     c     ParTOT        andNE     'S'
024800000614     C                   EXSR      STAARE
024900000614     C                   ENDIF
025000950214E   3C                   ENDDO
025100000000     C*
025200000000     C* STAMPA IL TOTALE DIVISIONE
025300000614     C     CHKARE        IFEQ      '1'
025400130130     c     ParTOT        andNE     'S'
025500000614     C                   EXSR      STADIV
025600000614     C                   ENDIF
025700950214E   2C                   ENDDO
025800000000     C*
025900000000     C* STAMPA IL TOTALE GENERALE
026000000000     C                   EXSR      STAGEN
026100000000     C*
026200000000     C* STAMPA RIGA FINE STAMPA
026300010507     C                   WRITE     SEA2FI
026400000000     C*
026500000000     C                   SETON                                        LR
026600000000     C*------------------------------------------------------------------------*
026700950216     C* SETSDC - POSIZIONAMENTO E PRIMA LETTURA
026800000000     C*------------------------------------------------------------------------*
026900950216     C     SETSDC        BEGSR
027000950214     C*
027100950216     C                   MOVEL     '0'           $FINE             1
027200950214     C*
027300950216     C                   Z-ADD     9999999       KSCFIN            7 0          *CLIENTE
027400000614     C     PARPT         IFEQ      'S'
027500000614     C                   MOVEL     PIL(F)        KSCFIN                         *CLIENTE FINALE
027600000614     C                   Z-ADD     *ZEROS        KSCKSC                         *CLIENTE
027700000614     C                   MOVEL     PIL(F)        KSCKSC                         *FIL+0000
027800000614     C                   ELSE
027900000614     C                   MOVEL     FIL(F)        KSCFIN                         *CLIENTE FINALE
028000000614     C                   Z-ADD     *ZEROS        KSCKSC                         *CLIENTE
028100000614     C                   MOVEL     FIL(F)        KSCKSC                         *FIL+0000
028200000614     C                   ENDIF
028300950216     C*
028400000614     C     KSCKSC        IFEQ      *ZEROS
028500000614     C                   MOVEL     '1'           $FINE
028600000614     C                   SETON                                        45
028700000614     C                   ELSE
028800000614     C                   SETOFF                                       45
028900000614     C     KEYSDC        SETLL     SISDC01L                           99        *FINE FILIALE
029000000000     C*
029100950214IF  1C     *IN99         IFEQ      *ON
029200000000     C                   MOVEL     '1'           $FINE
029300000614     C                   SETON                                        45
029400000000X   1C                   ELSE
029500950216     C                   EXSR      LETSDC                                       *LETTURA RECORD
029600000000E   1C                   ENDIF
029700000614     C                   ENDIF
029800000000     C*
029900000000     C                   ENDSR
030000000000     C*------------------------------------------------------------------------*
030100950216     C* LETSDC - LETTURA PROSSIMO RECORD
030200000000     C*------------------------------------------------------------------------*
030300950216     C     LETSDC        BEGSR
030400000000     C*
030500000000     C* LEGGO FINO A:
030600000000     C*     - FINE FILE
030700000000     C*     - TROVATO RECORD VALIDO
030800000000     C                   MOVEL     'N'           WRECOK
030900000000DO  1C     $FINE         DOWEQ     '0'
031000000000     C     WRECOK        ANDEQ     'N'
031100000000     C*
031200000000     C* LETTURE SUCCESSIVE ALLA PRIMA
031300990609     C                   READ      SISDC01L                               99
031400000000     C*
031500950214IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
031600950216     C     SDCKSC        ORGT      KSCFIN                                       *FINE FILIALE
031700000000     C                   MOVEL     '1'           $FINE
031800000000X   2C                   ELSE
031900000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
032000000000E   2C                   ENDIF
032100000000     C*
032200000000E   1C                   ENDDO
032300000000     C*
032400000000     C                   ENDSR
032500000000     C*------------------------------------------------------------------------*
032600950214     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
032700000000     C*------------------------------------------------------------------------*
032800000000     C     CHKREC        BEGSR
032900000000     C*
033000000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
033100020516     C*
033200020516     C* CONTROLLO SE IL RECORD E' VALORIZZATO
033300020516IF  1C     WRECOK        IFEQ      'S'
033400020516IF  2C     SDCNSP        IFEQ      *ZEROS
033500020516     C     SDCPKG        ANDEQ     *ZEROS
033600020516     C     SDCPCC        ANDEQ     *ZEROS
033700020516     C     SDCPLS        ANDEQ     *ZEROS
033800020516     C     SDCNCO        ANDEQ     *ZEROS
033900020516     C     SDCVOL        ANDEQ     *ZEROS
034000020516     C     SDCVCC        ANDEQ     *ZEROS
034100020516     C     SDCVLS        ANDEQ     *ZEROS
034200020516     C     SDCIRB        ANDEQ     *ZEROS
034300020516     C     SDCIRR        ANDEQ     *ZEROS
034400020516     C     SDCINC        ANDEQ     *ZEROS
034500020516     C     SDCIRP        ANDEQ     *ZEROS
034600020516     C     SDCIRC        ANDEQ     *ZEROS
034700020516     C     SDCIRD        ANDEQ     *ZEROS
034800020516     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
034900020516E   2C                   ENDIF
035000020516E   1C                   ENDIF
035100000614     C*
035200000614     C* VERIFICO SE CONSIDERARE A SECONDA DEL LANCIO PER POSTE OPPURE NO
035300000614     C                   Z-ADD     SDCKSC        KSCCOD
035400000614     C     PARPT         IFEQ      'S'
035500000614     C     KSCFIL        LOOKUP    PIL                                    99
035600000614IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
035700000614     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
035800000614     C                   ENDIF
035900000614     C                   ELSE
036000000614     C     KSCFIL        LOOKUP    FIL                                    99
036100000614IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
036200000614     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
036300000614     C                   ENDIF
036400000614E   1C                   ENDIF
036500950214     C*
036600950214     C* ESCLUSIONI PER DATA NON RICHIESTA
036700950214IF  1C     SDCDAT        IFLT      PARDAT
036800950214     C     SDCDAT        ORGT      PARDA2
036900950214     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
037000950214E   1C                   ENDIF
037100020328     C*
037200020328     C* DETERMINA IL NETWORK
037300020328     C                   MOVEL     *BLANKS       DEPNTWP           3
037400020328     C                   Z-ADD     1             R
037500020328     C     SDCREP        LOOKUP    RGREG(R)                               55
037600020328     C                   IF        *IN55 = *ON
037700020328     C                   EVAL      DEPNTWP = RGNTW(R)
037800020328     C                   ENDIF
037900020328     C*
038000020328     C                   MOVEL     *BLANKS       DEPNTWA           3
038100020328     C                   Z-ADD     1             R
038200020328     C     SDCREA        LOOKUP    RGREG(R)                               55
038300020328     C                   IF        *IN55 = *ON
038400020328     C                   EVAL      DEPNTWA = RGNTW(R)
038500020328     C                   ENDIF
038600950214     C*
038700950208     C* ESCLUSIONI PER DELTA ITALIA O ESTERO
038800950216     C     PARIOE        IFNE      *BLANKS
038900020328     C     DEPNTWP       IFNE      *BLANKS
039000020328     C     DEPNTWA       ORNE      *BLANKS
039100950216     C                   MOVEL     'E'           WIOE              1            *ESTERO
039200950216     C                   ELSE
039300950216     C                   MOVEL     'I'           WIOE                           *ITALIA
039400950216     C                   ENDIF
039500950216     C     PARIOE        IFNE      WIOE
039600950208     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
039700950216     C                   ENDIF
039800950216     C                   ENDIF
039900950216     C*
040000950216     C* REPERISCO I FLAGS RICAVO/SPEDIZIONE DEL TIPO BOLLA
040100950216     C                   Z-ADD     1             I
040200950216     C                   SETOFF                                           99
040300950216     C     SDCRBL        LOOKUP    FLAB(I)                                99
040400950216IF  1C     *IN99         IFEQ      *OFF                                         *TIPO NON TROVATO -->
040500950216     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
040600950216     C                   ELSE
040700971204     C*
040800950216     C* CONTROLLO SE LA BOLLA E' DA COSIDERARE COME N°SPEDIZIONE: PRENDO SOLO
040900950216     C* LE BOLLE CON FLAG FSP='S' PER IL CONTEGGIO IN STAMPA
041000950216IF  1C     FSP(I)        IFNE      'S'
041100950216     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
041200950216E   1C                   ENDIF
041300950216E   1C                   ENDIF
041400950216     C*
041500990217     C* ESCLUDO LE FASCE DI PESO OLTRE 5000 KG
041600000309IF  1C     SDCFPE        IFEQ      '07'
041700000309     C     PARDAT        ANDLT     200001
041800000309     C                   MOVEL     'N'           WRECOK
041900000309E   1C                   ENDIF
042000950208     C*
042100000000     C                   ENDSR
042200000000     C*------------------------------------------------------------------------*
042300950214     C* NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
042400000000     C*------------------------------------------------------------------------*
042500000000     C     NEWDIV        BEGSR
042600000000     C*
042700000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
042800950216     C                   MOVEL     DIV(F)        DEPDIV            1
042900950216     C                   MOVEL     DDIV(F)       DESDIV           20
043000000000     C*
043100000000     C* AZZERA I TOTALI DELLA DIVISIONE
043200020523     C                   DO        FPENUM        FP                             * X OGNI FASCIA DI PESO
043300950216     C     FP            OCCUR     DDEL
043400950215     C                   CLEAR                   DRD
043500950215     C                   CLEAR                   DPD
043600950215     C                   CLEAR                   DSD
043700950216     C                   ENDDO
043800000000     C*
043900000000     C                   ENDSR
044000000000     C*------------------------------------------------------------------------*
044100000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
044200000000     C*------------------------------------------------------------------------*
044300000000     C     NEWARE        BEGSR
044400000614     C*
044500000614     C* INIZIALIZZO FLAG X CONTROLLO STAMPA
044600000614     C                   MOVEL     '0'           CHKARE            1
044700000000     C*
044800000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
044900950216     C                   Z-ADD     ARE(F)        DEPARE            3 0
045000950216     C                   MOVEL     DARE(F)       DESARE           20
045100000000     C*
045200000000     C* AZZERA I TOTALI DELL'AREA
045300020523     C                   DO        FPENUM        FP                             * X OGNI FASCIA DI PESO
045400950216     C     FP            OCCUR     ADEL
045500950215     C                   CLEAR                   DRA
045600950215     C                   CLEAR                   DPA
045700950215     C                   CLEAR                   DSA
045800950216     C                   ENDDO
045900000000     C*
046000000000     C                   ENDSR
046100000000     C*------------------------------------------------------------------------*
046200000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
046300000000     C*------------------------------------------------------------------------*
046400000000     C     NEWFIL        BEGSR
046500000614     C*
046600000614     C* INIZIALIZZO FLAG X CONTROLLO STAMPA
046700000614     C                   MOVEL     '0'           CHKFIL            1
046800000000     C*
046900000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
047000000614     C     PARPT         IFEQ      'S'
047100000614     C                   Z-ADD     PIL(F)        DEPFIL            3 0
047200000614     C                   MOVEL     DPIL(F)       DESFIL           20
047300000614     C                   ELSE
047400000614     C                   Z-ADD     FIL(F)        DEPFIL            3 0
047500000614     C                   MOVEL     DFIL(F)       DESFIL           20
047600000614     C                   ENDIF
047700000000     C*
047800000000     C* AZZERA I TOTALI DELLA FILIALE
047900020523     C                   DO        FPENUM        FP                             * X OGNI FASCIA DI PESO
048000950216     C     FP            OCCUR     FDEL
048100950215     C                   CLEAR                   DRF
048200950215     C                   CLEAR                   DPF
048300950215     C                   CLEAR                   DSF
048400950216     C                   ENDDO
048500000000     C*
048600000000     C                   ENDSR
048700000000     C*------------------------------------------------------------------------*
048800000000     C*?MEMSAL - MEMORIZZA I SALDI DEL WORK FILE IN SCHIERE
048900000000     C*------------------------------------------------------------------------*
049000000000     C     MEMSAL        BEGSR
049100950216     C*
049200950216     C* FASCIA DI PESO
049300950216     C                   Z-ADD     1             FP
049400950216     C                   SETOFF                                           99
049500950216     C     SDCFPE        LOOKUP    FPE(FP)                                99
049600000000     C*
049700000000     C* M E S E
049800950216     C                   Z-ADD     1             MM
049900950216     C                   SETOFF                                           99
050000950216     C     SDCMES        LOOKUP    MES(MM)                                99
050100950216     C*
050200950216     C* COSTRUISCO RICAVO REALE NETTO (RICAVO REALE DELTA + NOTE + RETTIFICHE)
050300010508     C                   Z-ADD     *ZEROS        TOTRIC           21 5
050400950216     C     SDCIRD        ADD       SDCIRR        TOTRIC
050500950216     C     SDCINC        ADD       TOTRIC        TOTRIC
050600130130      *
050700130130     c                   If        ParTOT <> 'S'                                NON SOLO TOT. GEN.
050800130130      *
050900000000     C*--------------------
051000000000     C* F I L I A L E
051100000000     C*--------------------
051200950216     C     FP            OCCUR     FDEL
051300950215     C                   ADD       SDCNSP        DSF(MM)                        SPEDIZIONI
051400950216     C                   ADD       TOTRIC        DRF(MM)                        RICAVI REALI DELTA
051500950215     C                   ADD       SDCIRP        DPF(MM)                        RICAVI PRESUNTI DELTA
051600000000     C*--------------------
051700000000     C* A R E A
051800000000     C*--------------------
051900950216     C     FP            OCCUR     ADEL
052000950215     C                   ADD       SDCNSP        DSA(MM)                        SPEDIZIONI
052100950216     C                   ADD       TOTRIC        DRA(MM)                        RICAVI REALI DELTA
052200950215     C                   ADD       SDCIRP        DPA(MM)                        RICAVI PRESUNTI DELTA
052300000000     C*--------------------
052400000000     C* D I V I S I O N E
052500000000     C*--------------------
052600950216     C     FP            OCCUR     DDEL
052700950215     C                   ADD       SDCNSP        DSD(MM)                        SPEDIZIONI
052800950216     C                   ADD       TOTRIC        DRD(MM)                        RICAVI REALI DELTA
052900950215     C                   ADD       SDCIRP        DPD(MM)                        RICAVI PRESUNTI DELTA
053000130130      *
053100130130     c                   EndIf
053200130130      *
053300000000     C*--------------------
053400000000     C* G E N E R A L E
053500000000     C*--------------------
053600950216     C     FP            OCCUR     GDEL
053700950215     C                   ADD       SDCNSP        DSG(MM)                        SPEDIZIONI
053800950216     C                   ADD       TOTRIC        DRG(MM)                        RICAVI REALI DELTA
053900950215     C                   ADD       SDCIRP        DPG(MM)                        RICAVI PRESUNTI DELTA
054000000000     C*
054100000000     C                   ENDSR
054200000000     C*------------------------------------------------------------------------*
054300000000     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
054400000000     C*------------------------------------------------------------------------*
054500000000     C     STAFIL        BEGSR
054600000614     C*
054700000614     C* VALIZZO IL FLAG DI "STAMPATO ALMENO UNA FILIALE"
054800000614     C                   MOVEL     '1'           CHKFIL
054900950216     C*
055000000000     C* DESCRIZIONI INTESTAZIONE
055100010507     C                   CLEAR                   SEA2IN
055200000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
055300000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
055400000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
055500000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
055600000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
055700000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISIONE
055800000000     C*
055900000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
056000020523     C                   DO        FPENUM        FP                6 0
056100950216     C     FP            OCCUR     FDEL
056200950216     C     FP            OCCUR     SDEL
056300950216     C                   Z-ADD     DRF           DRS
056400950216     C                   Z-ADD     DPF           DPS
056500950216     C                   Z-ADD     DSF           DSS
056600950216     C                   ENDDO
056700000000     C*
056800000000     C* ROUTINE DI STAMPA TOTALI
056900000000     C                   EXSR      STAMPA
057000000000     C*
057100000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
057200000000     C                   SETON                                        01
057300000000     C*
057400000000     C                   ENDSR
057500000000     C*------------------------------------------------------------------------*
057600000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
057700000000     C*------------------------------------------------------------------------*
057800000000     C     STAARE        BEGSR
057900000614     C*
058000000614     C* VALIZZO IL FLAG DI "STAMPATO ALMENO UNA AREA"
058100000614     C                   MOVEL     '1'           CHKARE
058200950216     C*
058300000000     C* DESCRIZIONI INTESTAZIONE
058400010507     C                   CLEAR                   SEA2IN
058500000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
058600950216     C                   MOVEL     'TUTTE'       INDFIL                         *DESCRIZIONE FILIALE
058700000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
058800000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
058900000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
059000000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISIONE
059100000000     C*
059200000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
059300020523     C                   DO        FPENUM        FP
059400950216     C     FP            OCCUR     ADEL
059500950216     C     FP            OCCUR     SDEL
059600950216     C                   Z-ADD     DRA           DRS
059700950216     C                   Z-ADD     DPA           DPS
059800950216     C                   Z-ADD     DSA           DSS
059900950216     C                   ENDDO
060000000000     C*
060100000000     C* ROUTINE DI STAMPA TOTALI
060200000000     C                   EXSR      STAMPA
060300000000     C*
060400000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
060500000000     C                   SETON                                        01
060600000000     C*
060700000000     C                   ENDSR
060800000000     C*------------------------------------------------------------------------*
060900000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
061000000000     C*------------------------------------------------------------------------*
061100000000     C     STADIV        BEGSR
061200950216     C*
061300000000     C* DESCRIZIONI INTESTAZIONE
061400010507     C                   CLEAR                   SEA2IN
061500000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
061600950216     C                   MOVEL     'TUTTE'       INDFIL                         *DESCRIZIONE FILIALE
061700000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
061800950216     C                   MOVEL     'TUTTE'       INDARE                         *DESCRIZIONE AREA
061900000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
062000000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISIONE
062100000000     C*
062200000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
062300020523     C                   DO        FPENUM        FP
062400950216     C     FP            OCCUR     DDEL
062500950216     C     FP            OCCUR     SDEL
062600950216     C                   Z-ADD     DRD           DRS
062700950216     C                   Z-ADD     DPD           DPS
062800950216     C                   Z-ADD     DSD           DSS
062900950216     C                   ENDDO
063000000000     C*
063100000000     C* ROUTINE DI STAMPA TOTALI
063200000000     C                   EXSR      STAMPA
063300000000     C*
063400000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
063500000000     C                   SETON                                        01
063600000000     C*
063700000000     C                   ENDSR
063800000000     C*------------------------------------------------------------------------*
063900000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
064000000000     C*------------------------------------------------------------------------*
064100000000     C     STAGEN        BEGSR
064200950216     C*
064300000000     C* DESCRIZIONI INTESTAZIONE
064400010507     C                   CLEAR                   SEA2IN
064500000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
064600950216     C                   MOVEL     'TUTTE'       INDFIL                         *DESCRIZIONE FILIALE
064700000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
064800950216     C                   MOVEL     'TUTTE'       INDARE                         *DESCRIZIONE AREA
064900000000     C                   MOVEL     *BLANKS       INCDIV                         *DIVISIONE
065000950216     C                   MOVEL     'TUTTE'       INDDIV                         DESCRIZIONE DIVISIONE
065100000000     C*
065200000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
065300020523     C                   DO        FPENUM        FP
065400950216     C     FP            OCCUR     GDEL
065500950216     C     FP            OCCUR     SDEL
065600950216     C                   Z-ADD     DRG           DRS
065700950216     C                   Z-ADD     DPG           DPS
065800950216     C                   Z-ADD     DSG           DSS
065900950216     C                   ENDDO
066000000000     C*
066100000000     C* ROUTINE DI STAMPA TOTALI
066200000000     C                   EXSR      STAMPA
066300000000     C*
066400000000     C                   ENDSR
066500000000     C*------------------------------------------------------------------------*
066600000000     C*?STAINT - STAMPA INTESTAZIONE
066700000000     C*------------------------------------------------------------------------*
066800000000     C     STAINT        BEGSR
066900000000     C*
067000010507     C                   WRITE     SEA2PT                                       *TESTATA
067100150331     C                   WRITE     SEA2PRM                                      *pro-memoria parametri
067200010507     C                   WRITE     SEA2IN                                       *RIGA INTESTAZIONE
067300010507     C                   WRITE     SEA2R1                                       *RIGA SEPARAZIONE
067400010507     C                   WRITE     SEA2PI                                       *RIGA MESI
067500010507     C                   WRITE     SEA2R2                                       *RIGA SEPARAZIONE
067600000000     C                   SETOFF                                       01
067700000000     C*
067800000000     C                   ENDSR
067900000000     C*------------------------------------------------------------------------*
068000950216     C*?STAMPA - STAMPA I TOTALI NELLA FORMA: SPE / RIC / DEL
068100950216     C*          INPUT  --> DRS    : SCHIERA RICAVO REALE       MULT.
068200950216     C*                     DPS    : SCHIERA RICAVO PRESUNTO    MULT.
068300950216     C*                     DSS    : SCHIERA SPEDIZIONI         MULT.
068400000000     C*------------------------------------------------------------------------*
068500950216     C     STAMPA        BEGSR
068600000000     C*--------------------
068700000000     C* I N T E S T A Z I O N E
068800000000     C*--------------------
068900000000IF  1C     *IN01         IFEQ      *ON
069000000000     C                   EXSR      STAINT
069100000000E   1C                   ENDIF
069200950216     C*--------------------
069300950216     C* CICLO PER TUTTE LE FASCIE DI PESO
069400950216     C*--------------------
069500950216     C                   CLEAR                   TRS
069600950216     C                   CLEAR                   TPS
069700950216     C                   CLEAR                   TSS
069800020523     C                   DO        FPENUM        FP
069900950216     C     FP            OCCUR     SDEL
070000950216     C*
070100000000     C* S P E D I Z I O N I
070200950216     C                   MOVEL     'SPE'         DES03             3
070300000000     C                   MOVEL     *BLANKS       P1DDES
070400950216     C                   MOVEL     DFPE(FP)      P1DDES
070500000000     C                   MOVE      DES03         P1DDES
070600000000     C*
070700950216     C                   Z-ADD     DSS           SVA                            *SCH.VALORI 12 MESI
070800010508     C                   XFOOT     DSS           SVA13C           21 5          *TOTALE ANNO
070900000000     C*
071000950216     C                   EXSR      SCRVAL                                       *STAMPA RIGA VALORI
071100950216     C*
071200000000     C* R I C A V I
071300950216     C                   MOVEL     'RIC'         DES03             3
071400000000     C                   MOVEL     *BLANKS       P1DDES
071500000000     C                   MOVE      DES03         P1DDES
071600000000     C*
071700010507     C***  DRS           DIV       1000          SVA                            *SCH.VALORI 12 MESI
071800010507     C                   Z-ADD     DRS           SVA                            *SCH.VALORI 12 MESI
071900010507     C                   XFOOT     SVA           SVA13C                         *TOTALE ANNO
072000000000     C*
072100950216     C                   EXSR      SCRVAL                                       *SCRIVE RIGA VALORI
072200950216     C*
072300000000     C* D E L T A
072400950216     C                   MOVEL     'DEL'         DES03             3
072500000000     C                   MOVEL     *BLANKS       P2DDES
072600000000     C                   MOVE      DES03         P2DDES
072700950216     C* DELTA 12 MESI
072800950216DO  1C     1             DO        SPARM         J                 6 0
072900010508     C                   Z-ADD     DRS(J)        WRRW             21 5          *RICAVO REALE
073000010508     C                   Z-ADD     DPS(J)        WRPW             21 5          *RICAVO PRESUNTO
073100950216     C                   EXSR      CALDEL
073200950216     C                   Z-ADD     WDPW          SDE(J)                         *SCH.DELTA IN STAMPA
073300950216E   1C                   ENDDO
073400950216     C* DELTA 13^ COLONNA
073500010507     C                   XFOOT     DRS           WRRW                           *RICAVO REALE
073600010507     C                   XFOOT     DPS           WRPW                           *RICAVO PRESUNTO
073700950216     C                   EXSR      CALDEL
073800950216     C                   Z-ADD     WDPW          SDE13C            4 1          *13^ COLONNA DELTA
073900000000     C*
074000950216     C                   EXSR      SCRDEL                                       *SCRIVE PERCENT.DELTA
074100950216     C*
074200010507     C                   WRITE     SEA2R2                                       *STAMPA RIGA SEPARAZI
074300950216     C*
074400950216     C* MEMORIZZO PER TOTALE DI TUTTE LE FASCIE DI PESO
074500950216     C                   DO        12            I                 6 0
074600950216     C                   ADD       DRS(I)        TRS(I)
074700950216     C                   ADD       DPS(I)        TPS(I)
074800950216     C                   ADD       DSS(I)        TSS(I)
074900950216     C                   ENDDO
075000950216     C*
075100950216     C* FINE CICLO FASCIE DI PESO
075200950216     C                   ENDDO
075300950216     C*
075400950216     C*--------------------
075500950216     C* TOTALE DI TUTTE LE FASCIE DI PESO
075600950216     C*--------------------
075700950216     C* S P E D I Z I O N I
075800950216     C                   MOVEL     'SPE'         DES03             3
075900950216     C                   MOVEL     *BLANKS       P1DDES
076000950216     C                   MOVEL     'TOTALE'      P1DDES
076100950216     C                   MOVE      DES03         P1DDES
076200950216     C*
076300950216     C                   Z-ADD     TSS           SVA                            *SCH.VALORI 12 MESI
076400010508     C                   XFOOT     TSS           SVA13C           21 5          *TOTALE ANNO
076500950216     C*
076600950216     C                   EXSR      SCRVAL                                       *STAMPA RIGA VALORI
076700950216     C*
076800950216     C* R I C A V I
076900950216     C                   MOVEL     'RIC'         DES03             3
077000950216     C                   MOVEL     *BLANKS       P1DDES
077100950216     C                   MOVE      DES03         P1DDES
077200950216     C*
077300010507     C***  TRS           DIV       1000          SVA                            *SCH.VALORI 12 MESI
077400010507     C                   Z-ADD     TRS           SVA                            *SCH.VALORI 12 MESI
077500010507     C                   XFOOT     SVA           SVA13C                         *TOTALE ANNO
077600950216     C*
077700950216     C                   EXSR      SCRVAL                                       *SCRIVE RIGA VALORI
077800950216     C*
077900950216     C* D E L T A
078000950216     C                   MOVEL     'DEL'         DES03             3
078100950216     C                   MOVEL     *BLANKS       P2DDES
078200950216     C                   MOVE      DES03         P2DDES
078300950216     C* DELTA 12 MESI
078400950216DO  1C     1             DO        SPARM         J
078500010507     C                   Z-ADD     TRS(J)        WRRW                           *RICAVO REALE
078600010507     C                   Z-ADD     TPS(J)        WRPW                           *RICAVO PRESUNTO
078700950216     C                   EXSR      CALDEL
078800950216     C                   Z-ADD     WDPW          SDE(J)                         *SCH.DELTA IN STAMPA
078900950216E   1C                   ENDDO
079000950216     C* DELTA 13^ COLONNA
079100010507     C                   XFOOT     TRS           WRRW                           *RICAVO REALE
079200010507     C                   XFOOT     TPS           WRPW                           *RICAVO PRESUNTO
079300950216     C                   EXSR      CALDEL
079400950216     C                   Z-ADD     WDPW          SDE13C                         *13^ COLONNA DELTA
079500950216     C*
079600950216     C                   EXSR      SCRDEL                                       *SCRIVE PERCENT.DELTA
079700950216     C*
079800010507     C                   WRITE     SEA2R2                                       *STAMPA RIGA SEPARAZI
079900000000     C*
080000000000     C                   ENDSR
080100000000     C*------------------------------------------------------------------------*
080200000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
080300000000     C*          INPUT  --> WRRW: RICAVO REALE
080400000000     C*                     WRPW: RICAVO PRESUNTO
080500000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
080600000000     C*------------------------------------------------------------------------*
080700000000     C     CALDEL        BEGSR
080800000000     C*--------------------
080900000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
081000000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
081100000000     C*--------------------
081200000000     C*
081300950216     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
081400000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
081500010508     C     WRRW          SUB       WRPW          GAP              21 5          (REALE - PRESUNTO)
081600000000     C     GAP           MULT      100           GAP                            (GAP x 100)
081700000000     C*
081800000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
081900000000IF  1C     WRRW          IFEQ      *ZEROS
082000000000     C                   Z-ADD     1             WRRW
082100000000E   1C                   ENDIF
082200000000     C*
082300000000     C     GAP           DIV       WRRW          GAP
082400000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
082500000000     C*
082600000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
082700000000     C     WDPW          COMP      0                                    28
082800000000     C* POSITIVO
082900000000IF  1C     *IN28         IFEQ      *OFF
083000000000IF  2C     GAP           IFGE      1000
083100000000     C                   Z-ADD     +999,9        WDPW
083200000000E   2C                   ENDIF
083300000000     C* NEGATIVO
083400000000X   1C                   ELSE
083500000000IF  2C     GAP           IFLE      -1000
083600000000     C                   Z-ADD     -999,9        WDPW
083700000000E   2C                   ENDIF
083800000000E   1C                   ENDIF
083900000000     C*
084000000000     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C*?SCRVAL - SCRIVE I VALORI IN STAMPA
084300000000     C*------------------------------------------------------------------------*
084400000000     C     SCRVAL        BEGSR
084500010515     C*
084600010515     C                   Z-ADD(H)  SVA(1)        P1CVA1                         *MESE UNO
084700010515     C                   Z-ADD(H)  SVA(2)        P1CVA2                         *MESE DUE
084800010515     C                   Z-ADD(H)  SVA(3)        P1CVA3                         *MESE TRE
084900010515     C                   Z-ADD(H)  SVA(4)        P1CVA4                         *MESE QUATTRO
085000010515     C                   Z-ADD(H)  SVA(5)        P1CVA5                         *MESE CINQUE
085100010515     C                   Z-ADD(H)  SVA(6)        P1CVA6                         *MESE SEI
085200010515     C                   Z-ADD(H)  SVA(7)        P1CVA7                         *MESE SETTE
085300010515     C                   Z-ADD(H)  SVA(8)        P1CVA8                         *MESE OTTO
085400010515     C                   Z-ADD(H)  SVA(9)        P1CVA9                         *MESE NOVE
085500010515     C                   Z-ADD(H)  SVA(10)       P1CVA0                         *MESE DIECI
085600010515     C                   Z-ADD(H)  SVA(11)       P1CVAA                         *MESE UNDICI
085700010515     C                   Z-ADD(H)  SVA(12)       P1CVAB                         *MESE DODICI
085800010515     C                   Z-ADD(H)  SVA13C        P1CVAT                         *TOTALE MESI
085900000000     C*
086000010507     C                   WRITE     SEA2P1                                       *STAMPA RIGA VALORE
086100000000     C*
086200000000     C                   ENDSR
086300000000     C*------------------------------------------------------------------------*
086400000000     C*?SCRDEL - SCRIVE I VALORI DELTA IN STAMPA
086500000000     C*------------------------------------------------------------------------*
086600000000     C     SCRDEL        BEGSR
086700000000     C*
086800950216     C                   Z-ADD     SDE(1)        P2CDE1                         *MESE UNO
086900950216     C                   Z-ADD     SDE(2)        P2CDE2                         *MESE DUE
087000950216     C                   Z-ADD     SDE(3)        P2CDE3                         *MESE TRE
087100950216     C                   Z-ADD     SDE(4)        P2CDE4                         *MESE QUATTRO
087200950216     C                   Z-ADD     SDE(5)        P2CDE5                         *MESE CINQUE
087300950216     C                   Z-ADD     SDE(6)        P2CDE6                         *MESE SEI
087400950216     C                   Z-ADD     SDE(7)        P2CDE7                         *MESE SETTE
087500950216     C                   Z-ADD     SDE(8)        P2CDE8                         *MESE OTTO
087600950216     C                   Z-ADD     SDE(9)        P2CDE9                         *MESE NOVE
087700950216     C                   Z-ADD     SDE(10)       P2CDE0                         *MESE DIECI
087800950216     C                   Z-ADD     SDE(11)       P2CDEA                         *MESE UNDICI
087900950216     C                   Z-ADD     SDE(12)       P2CDEB                         *MESE DODICI
088000950216     C                   Z-ADD     SDE13C        P2CDET                         *TOTALE MESI
088100000000     C*
088200010507     C                   WRITE     SEA2P2                                       *STAMPA RIGA VALORE
088300000000     C*
088400000000     C                   ENDSR
088500000000     C*------------------------------------------------------------------------*
088600000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
088700000000     C*------------------------------------------------------------------------*
088800000000     C     CARMES        BEGSR
088900000000     C*
089000000000     C                   CLEAR                   ANN                            *ANNI
089100000000     C                   CLEAR                   MES                            *MESI
089200000000     C                   Z-ADD     *ZEROS        J                              *N° MESI DA ELABORARE
089300000000     C*
089400990524     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
089500000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
089600990524     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVORO
089700000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVORO
089800000000     C*
089900000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
090000000000IF  1C     WAA2          IFEQ      WAA
090100000000     C*
090200000000     C* CALCOLO I MESI DI DIFFERENZA
090300000000DO  2C     WMM           DOWLE     WMM2
090400000000     C                   ADD       1             J
090500000000     C                   Z-ADD     WAA           ANN(J)
090600000000     C                   Z-ADD     WMM           MES(J)
090700000000     C                   ADD       1             WMM
090800000000E   2C                   ENDDO
090900000000     C***
091000000000     C* SPARM = N° MESI SELEZIONATI
091100000000     C***
091200000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
091300000000     C*
091400000000     C* IL RESTO DELLA SCHIERA A ZERO
091500000000DO  2C     J             DOWLT     12
091600000000     C                   ADD       1             J
091700000000     C                   Z-ADD     *ZEROS        ANN(J)
091800000000     C                   Z-ADD     *ZEROS        MES(J)
091900010515E   2C                   ENDDO
092000000000     C*
092100000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
092200000000X   1C                   ELSE
092300000000     C*
092400000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
092500000000DO  2C     WMM           DOWLE     12
092600000000     C                   ADD       1             J
092700000000     C                   Z-ADD     WAA           ANN(J)
092800000000     C                   Z-ADD     WMM           MES(J)
092900000000     C                   ADD       1             WMM
093000000000E   2C                   ENDDO
093100000000     C*
093200000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
093300000000     C                   Z-ADD     01            WMM
093400000000DO  2C     WMM           DOWLE     WMM2
093500000000     C                   ADD       1             J
093600000000     C                   Z-ADD     WAA2          ANN(J)
093700000000     C                   Z-ADD     WMM           MES(J)
093800000000     C                   ADD       1             WMM
093900000000E   2C                   ENDDO
094000000000     C***
094100000000     C* SPARM = N° MESI SELEZIONATI
094200000000     C***
094300000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
094400000000     C*
094500000000     C* IL RESTO DELLA SCHIERA A ZERO
094600000000DO  2C     J             DOWLT     12
094700000000     C                   ADD       1             J
094800000000     C                   Z-ADD     *ZEROS        ANN(J)
094900000000     C                   Z-ADD     *ZEROS        MES(J)
095000000000E   2C                   ENDDO
095100000000E   1C                   ENDIF
095200000000     C*
095300000000     C                   ENDSR
095400000000     C*------------------------------------------------------------------------*
095500000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
095600000000     C*------------------------------------------------------------------------*
095700000000     C     DECTES        BEGSR
095800000000     C*--------------------
095900000000     C* DATA E ORA
096000000000     C*--------------------
096100000000     C                   Z-ADD     UDATE         DATA              6 0
096200000000     C                   TIME                    ORA               6 0
096300000000     C*--------------------
096400000000     C* DECODIFICA MESI
096500000000     C*--------------------
096600000000IF  1C     MES(1)        IFGT      *ZEROS
096700000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ.
096800000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
096900990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
097000000000E   1C                   ENDIF
097100000000     C*
097200000000IF  1C     MES(2)        IFGT      *ZEROS
097300000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ.
097400000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
097500990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
097600000000E   1C                   ENDIF
097700000000     C*
097800000000IF  1C     MES(3)        IFGT      *ZEROS
097900000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ.
098000000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
098100990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
098200000000E   1C                   ENDIF
098300000000     C*
098400000000IF  1C     MES(4)        IFGT      *ZEROS
098500000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ.
098600000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
098700990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
098800000000E   1C                   ENDIF
098900000000     C*
099000000000IF  1C     MES(5)        IFGT      *ZEROS
099100000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ.
099200000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
099300990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
099400000000E   1C                   ENDIF
099500000000     C*
099600000000IF  1C     MES(6)        IFGT      *ZEROS
099700000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ.
099800000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
099900990608     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
100000000000E   1C                   ENDIF
100100000000     C*
100200000000IF  1C     MES(7)        IFGT      *ZEROS
100300000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ.
100400000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
100500990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
100600000000E   1C                   ENDIF
100700000000     C*
100800000000IF  1C     MES(8)        IFGT      *ZEROS
100900000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ.
101000000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
101100990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
101200000000E   1C                   ENDIF
101300000000     C*
101400000000IF  1C     MES(9)        IFGT      *ZEROS
101500000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ.
101600000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
101700990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
101800000000E   1C                   ENDIF
101900000000     C*
102000000000IF  1C     MES(10)       IFGT      *ZEROS
102100000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ.
102200000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
102300990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
102400000000E   1C                   ENDIF
102500000000     C*
102600000000IF  1C     MES(11)       IFGT      *ZEROS
102700000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ.
102800000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
102900990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
103000000000E   1C                   ENDIF
103100000000     C*
103200000000IF  1C     MES(12)       IFGT      *ZEROS
103300000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ.
103400000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
103500990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
103600000000E   1C                   ENDIF
103700000000     C*
103800000000     C                   ENDSR
103900000000     C*------------------------------------------------------------------------*
104000000000     C*?CARTAB - CARICA I DATI IN TABELLE
104100000000     C*------------------------------------------------------------------------*
104200000000     C     CARTAB        BEGSR
104300000000     C*--------------------
104400971204     C* ORGANIGRAMMA
104500000000     C*--------------------
104600950216     C                   MOVEL     *BLANKS       KORDIV
104700950216     C     KEYORG        SETLL     AZORG02L
104800950216     C                   READ      AZORG02L                               99
104900000000     C                   Z-ADD     *ZEROS        F
105000000000DO  1C     *IN99         DOWEQ     '0'
105100000000IF  2C     ORGFVA        IFEQ      *BLANKS
105200950216IF  3C     ORGFAG        IFEQ      'F'
105300950216     C     ORGFAG        OREQ      'A'
105400000000     C                   ADD       1             F
105500000614     C                   MOVEL     ORGDE3        OG143
105600020627     C     §OGNTW        IFEQ      'PPT'
105700000614     C* F I L I A L I  POSTE
105800000614     C                   Z-ADD     ORGFIL        PIL(F)
105900000614     C                   MOVEL     ORGDES        DPIL(F)
106000000614     C                   ELSE
106100000000     C* F I L I A L I
106200000000     C                   Z-ADD     ORGFIL        FIL(F)
106300000000     C                   MOVEL     ORGDES        DFIL(F)
106400000614     C                   ENDIF
106500000000     C* A R E E
106600020916     C                   Z-ADD     1             KTBKUT
106700000000     C                   MOVEL     '05'          KTBCOD
106800000000     C                   MOVEL     *BLANKS       KTBKEY
106900000000     C                   MOVEL     ORGCAR        KTBKEY
107000000000     C     KEYTAB        CHAIN     TABEL00F                           99
107100950216IF  4C     *IN99         IFEQ      *OFF
107200000000     C                   Z-ADD     ORGCAR        ARE(F)
107300000000     C                   MOVEL     TBLUNI        DARE(F)
107400950216E   4C                   ENDIF
107500000000     C* D I V I S I O N I
107600020916     C                   Z-ADD     1             KTBKUT
107700000000     C                   MOVEL     '17'          KTBCOD
107800000000     C                   MOVEL     *BLANKS       KTBKEY
107900000000     C                   MOVEL     ORGFL3        KTBKEY
108000000000     C     KEYTAB        CHAIN     TABEL00F                           99
108100950216IF  4C     *IN99         IFEQ      *OFF
108200000000     C                   MOVEL     ORGFL3        DIV(F)
108300000000     C                   MOVEL     TBLUNI        DDIV(F)
108400950216E   4C                   ENDIF
108500950216E   3C                   ENDIF
108600000000E   2C                   ENDIF
108700950216     C                   READ      AZORG02L                               99
108800000000E   1C                   ENDDO
108900950216     C                   Z-ADD     F             ULTFIL            6 0
109000990217     C*--------------------
109100950216     C* FASCE DI PESO
109200990217     C*--------------------
109300020325IF  1C     PARDA2        IFLE      199812                                       *FASCE FINO AL '98
109400990217     C                   MOVEL     'SDC'         WSX               3
109500020325X   1C                   ELSE
109600020325IF  2C     PARDA2        IFLE      200112                                       *FASCE DAL '99 FINO AL 2001
109700020325     C                   MOVEL     'S9C'         WSX
109800020325X   2C                   ELSE                                                   *FASCE DAL 2002
109900020325     C                   MOVEL     'S2C'         WSX
110000020325E   2C                   ENDIF
110100020325E   1C                   ENDIF
110200950216     C                   Z-ADD     CODUT         KTBKUT                         *CODICE UTENTE
110300950216     C                   MOVEL     '2L'          KTBCOD
110400950216     C     KEYTA2        SETLL     TABEL00F
110500950216     C     KEYTA2        READE     TABEL00F                               99
110600950216     C                   Z-ADD     *ZEROS        J                              *INDICE
110700950216DO  1C     *IN99         DOWEQ     *OFF
110800950216IF  2C     TBLFLG        IFEQ      *BLANKS
110900990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
111000950216     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
111100990217IF  3C     FILEST        IFEQ      WSX
111200950216     C                   ADD       1             J
111300950216     C                   MOVE      STACOD        FPE(J)                         *CODICE STATISTICA
111400950216     C                   MOVEL     TBLUNI        DS2L
111500950216     C                   MOVEL     §2LDFP        DFPE(J)                        *DECODIFICA FASCIA
111600950216E   3C                   ENDIF
111700950216E   2C                   ENDIF
111800950216     C     KEYTA2        READE     TABEL00F                               99
111900950216E   1C                   ENDDO
112000990217     C*--------------------
112100950216     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
112200990217     C*--------------------
112300020916     C                   Z-ADD     1             KTBKUT
112400950216     C                   MOVEL     '2W'          KTBCOD
112500950216     C     KEYTA2        SETLL     TABEL00F
112600950216     C     KEYTA2        READE     TABEL00F                               99
112700950216     C                   Z-ADD     *ZEROS        I
112800950216DO  1C     *IN99         DOWEQ     *OFF
112900950216IF  2C     TBLFLG        IFEQ      *BLANKS
113000950216     C                   ADD       1             I
113100950216     C                   MOVEL     TBLKEY        FLAB(I)
113200950216     C                   MOVEL     TBLUNI        DS2W
113300950216     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONI
113400950216     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVI
113500950216E   2C                   ENDIF
113600950216     C     KEYTA2        READE     TABEL00F                               99
113700950216E   1C                   ENDDO
113800020328     C*-------------------------
113900020328     C* CODICI RAGGRUPPAMENTO REGIONI (X NETWORK)
114000020328     C*-------------------------
114100020328     C                   Z-ADD     *ZEROS        R
114200020916     C                   Z-ADD     1             KTBKUT
114300020328     C                   MOVEL     '2M'          KTBCOD
114400020328     C     KEYTA2        SETLL     TABEL00F
114500020328     C     KEYTA2        READE     TABEL00F                               99
114600020328DO  1C     *IN99         DOWEQ     *OFF
114700020328IF  2C     TBLFLG        IFEQ      *BLANKS
114800020328     C                   ADD       1             R
114900020328     C                   MOVEL     TBLKEY        RGREG(R)
115000020328     C                   MOVEL     TBLUNI        DS2M
115100020328     C                   MOVEL     §2MNTW        RGNTW(R)                       *FLAG SPEDIZIONI
115200020328E   2C                   ENDIF
115300020328     C     KEYTA2        READE     TABEL00F                               99
115400020328E   1C                   ENDDO
115500000000     C*
115600000000     C                   ENDSR
115700000000     C*------------------------------------------------------------------------*
115800000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
115900000000     C*------------------------------------------------------------------------*
116000000000     C     STAPAR        BEGSR
116100000000     C*--------------------
116200000000     C* DATE LANCIO
116300000000     C*--------------------
116400000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
116500000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
116600000000     C*--------------------
116700950214     C* PARZIALIZZAZIONI
116800000000     C*--------------------
116900130208     C     PARPT         IFEQ      'S'
117000130208     C                   MOVEL     'POSTE   '    STPPT
117100130208     C                   ELSE
117200130208     C                   MOVEL     'BRT     '    STPPT
117300130208     C                   ENDIF
117400130208     c                   if        ParTOT = 'S'
117500130208     c                   eval      StpTOT = 'SI'
117600130208     c                   else
117700130208     c                   eval      StpTOT = 'NO'
117800130208     c                   endif
117900950214     C*
118000950214     C                   MOVEL     PARIOE        PACIOE
118100950214IF  1C     PARIOE        IFEQ      'I'
118200950216     C                   MOVEL     'ITALIA'      PADIOE
118300950214E   1C                   ENDIF
118400950214IF  1C     PARIOE        IFEQ      'E'
118500950216     C                   MOVEL     'ESTERO'      PADIOE
118600950214E   1C                   ENDIF
118700950214IF  1C     PARIOE        IFEQ      *BLANKS
118800950216     C                   MOVEL     'ITA+EST'     PADIOE
118900950214E   1C                   ENDIF
119000950214     C***
119100000000     C* S T A M P A
119200950214     C***
119300010507     C                   WRITE     SEA2PT                                       *INTESTAZIONE
119400010507     C                   WRITE     SEA2PA                                       *PARAMETRI
119500000000     C*
119600000000     C* ACCENDO INDICATORE DI OF PER STAMPARE LA TESTATA ALLA 1^ ROTTURA CLIENTE
119700000000     C                   SETON                                        01
119800000000     C*
119900000000     C                   ENDSR
120000020916     C*--------------------------------------------------------------------------------------------*
120100020916     C* REPERISCE I DATI UTENTE
120200020916     C*--------------------------------------------------------------------------------------------*
120300020916     C     REPDATIUTE    BEGSR
120400020916     C*
120500020916     C* INIZIALIZZA VARIABILI DI WRK
120600020916     C                   CLEAR                   TIBS34DS
120700020916     C                   CLEAR                   AZUTEDS
120800020916     C                   CLEAR                   DDATIUTE
120900020916     C*
121000020916     C     *DTAARA       DEFINE    §azute        azuteds
121100020916     C     *DTAARA       DEFINE    §datiute      ddatiute
121200020916     C                   IN(E)     *DTAARA
121300020916if  1C                   IF        %Error
121400020916     c                   EVAL      I34Tla = 'L'
121500020916     C                   CALL      'TIBS34R'
121600020916     C                   PARM                    Tibs34Ds
121700020916     C                   IN        *DTAARA
121800020916e   1C                   ENDIF
121900020916     C*
122000020916     C                   ENDSR
122100000000     C*------------------------------------------------------------------------*
122200000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
122300000000     C*------------------------------------------------------------------------*
122400000000     C     *INZSR        BEGSR
122500000000     C*
122600000000     C* PARAMETRI IN INPUT AL PGM
122700000000     C     *ENTRY        PLIST
122800000000     C                   PARM                    KPJBA
122900000000     C                   MOVEL     KPJBU         PARAM
123000000000     C*
123100000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
123200000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
123300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
123400000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
123500950216     C     *LIKE         DEFINE    ORGFL3        KORDIV                         *AZORG01L
123600950216     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *CLIENTE
123700000000     C*
123800000000     C* DEFINISCE INDICI DI SCHIERA
123900000000     C                   Z-ADD     *ZEROS        X                 2 0          *PER I MESI
124000000000     C                   Z-ADD     *ZEROS        MM                2 0          *PER I MESI
124100000000     C                   Z-ADD     *ZEROS        IN                2 0          *PER I MESI
124200000000     C                   Z-ADD     *ZEROS        SPARM             4 0          *N° MESI SELEZIONATI
124300020328     C                   Z-ADD     *ZEROS        R                 3 0          *PER RAGGRUPPAMENTI REGIONI/NETWORK
124400000000     C*
124500000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
124600000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
124700000000     C                   MOVEL     ' '           TIPANN            1            *TIPO ANNO (P/C)
124800000000     C*
124900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
125000000000     C     KEYTAB        KLIST
125100000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
125200000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
125300000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
125400000000     C*
125500000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
125600000000     C     KEYTA2        KLIST
125700000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
125800000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
125900000000     C*
126000950216     C* CHIAVE LETTURA AZORG02L - PARZIALE
126100000000     C     KEYORG        KLIST
126200950216     C                   KFLD                    KORDIV                         *DIVISIONE
126300000000     C*
126400990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
126500950216     C     KEYSDC        KLIST
126600950216     C                   KFLD                    KSCKSC                         *DIVISIONE
126700000000     C*
126800000000     C* LANCIO ROUTINE PER VERIFICARE SE SONO IN SEDE, IN FILIALE DI 1°LIVELLO O
126900000000     C* IN FILIALE DI 2° LIVELLO E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
127000020916     C                   EXSR      REPDATIUTE
127100020916     C                   Z-ADD     1             CODUT             1 0
127200000000     C*
127300000000     C* MEMORIZZA I DATI IN TABELLE
127400000000     C                   EXSR      CARTAB
127500000000     C*
127600000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
127700000000     C                   EXSR      CARMES
127800000000     C*
127900000000     C* DECODIFICA INTESTAZIONE
128000000000     C                   EXSR      DECTES
128100000000     C*
128200000000     C                   ENDSR
128300000000     O*------------------------------------------------------------------------*
128400000000**   DMM - DECODIFICHE MESI DELL'ANNO
128500000000GEN  1
128600000000FEB  2
128700000000MAR  3
128800000000APR  4
128900000000MAG  5
129000000000GIU  6
129100000000LUG  7
129200000000AGO  8
129300000000SET  9
129400000000OTT  10
129500000000NOV  11
129600000000DIC  12
