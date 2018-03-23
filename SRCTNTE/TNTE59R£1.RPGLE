000100000000     H*------------------------------------------------------------------------*
000200060809     H* STATISTICA DELTA BOLLE  - GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500060904     Ftitas31c  IF   E           K DISK    ignore(titasp00)
000600970415     FTNTAM01L  IF   E           K DISK
000700990820     FTITAD04L  IF   E           K DISK
000800970418     FTABEL00F  IF   E           K DISK
000900020301     FAZORG01L  IF   E           K DISK
001000000329     FWFDBF01L  O    E           K DISK
001100000000     D*------------------------------------------------------------------------*
001200000000     D* SCHIERE
001300000000     D*------------------------------------------------------------------------*
001400970418     D*-------------------
001500970418     D* MESSAGGI
001600970418     D*-------------------
001700970418     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001800970418     D*-------------------
001900970418     D* DATI TARIFFA
002000970418     D*-------------------
002100060222     D SSGL            S             13  3 DIM(200)                             *SCAGLIONI
002200050103     D CTS             S              2    DIM(600)                             *CODICI TASSAZIONE
002300020301     D*-------------------
002400020301     D* SCHIERA FILIALI FEDEX
002500020301     D*-------------------
002600020301     D FILFED          S              3  0 DIM(999)
002700000000     D*------------------------------------------------------------------------*
002800000000     D* INPUT
002900000000     D*------------------------------------------------------------------------*
003000970418     D*-------------------
003100970418     D* DS CODICI TASSAZIONE
003200970418     D*-------------------
003300970418     D DSCT          E DS
003400021202     D*-------------------
003500021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
003600021202     D*-------------------
003700021202     D TRUL27DS      E DS
003800970415     D*-------------------
003900970418     D* DS PASSAGGIO PARAMETRI -TNTE50R-
004000970415     D*-------------------
004100970415     D DS50          E DS                  EXTNAME(TNTE50DS)
004200000000     D*-------------------
004300970415     D* DS SCOMPOSIZIONE DATA
004400000000     D*-------------------
004500000000     D                 DS                  INZ
004600970418     D  DSAAS                  1      4  0
004700970418     D  DSMES                  5      6  0
004800970418     D  DSGIO                  7      8  0
004900970418     D  DSMGS                  5      8  0
005000970418     D  DSDAT                  1      8  0
005100970513     D*-------------------
005200970513     D* INFDS - INFORMAZIONI PROGRAMMA
005300970513     D*-------------------
005400970513     D                SDS
005500981001     D  JOB                  244    253
005600970513     D  USER                 254    263
005700000000     D*-------------------
005800970418     D* DS PARAMETRI DI LANCIO
005900000000     D*-------------------
006000950703     D PARAM           DS
006100970414     D  PARKS1                 1      7  0
006200970414     D  PARTA1                 8     10  0
006300970414     D  PARKS2                11     17  0
006400970414     D  PARTA2                18     20  0
006500970414     D  PARKS3                21     27  0
006600970414     D  PARTA3                28     30  0
006700970414     D  PARKS4                31     37  0
006800970414     D  PARTA4                38     40  0
006900970414     D  PARKS5                41     47  0
007000970414     D  PARTA5                48     50  0
007100970414     D  PARDS1                51     58  0
007200970414     D  PARDS2                59     66  0
007300970414     D  PARKTT                67     73  0
007400970414     D  PARTTT                74     76  0
007500970415     D  PARDVT                77     84  0
007600970415     D  PARDET                85     85
007700970415     D  PARCT1                86     87
007800970415     D  PARCT2                88     89
007900970415     D  PARCT3                90     91
008000970415     D  PARCT4                92     93
008100970415     D  PARCT5                94     95
008200970415     D  PARCT6                96     97
008300970415     D  PARCT7                98     99
008400970415     D  PARCT8               100    101
008500970415     D  PARCT9               102    103
008600970415     D  PARCT0               104    105
008700970415     D  PARKGD               106    112  1
008800970415     D  PARKGA               113    119  1
008900970415     D  PARCOD               120    125  0
009000970415     D  PARCOA               126    131  0
009100970416     D  PARLNP               132    134  0
009101060809     D  PARLN1               135    137  0
009102060809     D  PARLN2               138    140  0
009103060809     D  PARLN3               141    143  0
009104060809     D  PARLN4               144    146  0
009105060809     D  PARLN5               147    149  0
009106060809     d  lnp                  135    149  0 dim(5)
009200970418     D*-------------------
009300970418     D* ARCHIETTURA
009400970418     D*-------------------
009500970418     D KPJBA         E DS
009600020301     D*-------------------
009700020301     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
009800020301     D*-------------------
009900020301     D OG143         E DS
010000020911     D*-------------
010100020911     D* DS DI WRK
010200020911     D*-------------
010204060809     D TIBS02DS      E DS                                                       *Ricerca tabella
010205060809     D*------------------
010206060809     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
010207060809     D*-------------------
010208060809     D DUTE01        E DS                  INZ                                  *Abilitazioni utenti
010209060809     D*------------------
010210060809     D* DS FUNZIONI AZIENDALI
010211060809     D*-------------------
010212060809     D DLAT          E DS                  INZ                                  *Funzioni aziendali
010213060809     D TRUL31DS      E DS                  INZ
010214060809     D  S31                   10    759  0 dim(250)
010300020911     D TIBS34DS      E DS                                                       *Profili utente
010400020911     D DDATIUTE      E DS                                                       *Dati utente
010500020911     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
011100020911     D*-------------
011200020911     D* FILIALI IN GESTIONE
011300020911     D*-------------
011401060904     D ksc             S              7  0 DIM(5)                               *FILIALI IN GESTIONE
011402060904     D l1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
011403070530     D wpes            S                   like(taspkf)                         *FILIALI IN GESTIONE
011404070530     D wvol            S                   like(tasvlf)                         *FILIALI IN GESTIONE
011405060904     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS31C
011406060809     ITITAS000      01
011407060809     ITITAS010      02
011500000000     C*------------------------------------------------------------------------*
011600000000     C* MAIN LINE
011700000000     C*------------------------------------------------------------------------*
011800000000     C*
011900000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
012000000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
012100020911     C*
012200020911     C* CICLO X TUTTE LE FILIALI DELL'ELABORATORE
012300060904     C     1             DO        5             III               3 0
012400060904     C                   IF        ksc(III) > *ZEROS
012500020911     C                   MOVEL     '0'           FINL00
012700000000     C*
012800000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
012900000000     C                   EXSR      SETFIL
013000000000     C*
013100000000     C* CICLO FINO A FINE FILE
013200000000     C     FINL00        DOWEQ     '0'                                          --- 2 -->
013300970415     C*
013400970415     C* LANCIA PGM CALCOLO RICAVI/COMPETENZE
013500970415     C                   EXSR      LANCIA
013600000000     C*
013700000000     C* CREA IL RECORD SUI FILE DI LAVORO
013800000000     C                   EXSR      CREARE
013900000000     C*
014000000000     C* LEGGE IL RECORD SUCCESSIVO
014100000000     C                   EXSR      LETFIL
014200020911     C                   ENDDO                                                  <-- 2 ---
014300020911     C*
014400020911     C                   ELSE
014500020911     C                   LEAVE
014600020911     C                   ENDIF
014700020911     C                   ENDDO
014800020911     C                   ENDIF                                                  <-- 1 ---
014900000000     C*
015000970418     C                   MOVEL     'C'           D50TLA
015100970418     C                   CALL      'TNTE50R'
015200970418     C                   PARM                    DS50
015300970418     C*
015400000000     C                   SETON                                        LR
015500000000     C*------------------------------------------------------------------------*
015600000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
015700000000     C*------------------------------------------------------------------------*
015800000000     C     SETFIL        BEGSR
015900000000     C*
016000000000     C* POSIZIONAMENTO E PRIMA LETTURA
016300060904     C     ksc(III)      SETLL     titas31c                           99        *FINE FILE
016400000000     C*
016500000000     C     *IN99         IFEQ      '1'
016600000000     C                   MOVEL     '1'           FINL00
016700000000     C                   ELSE
016800000000     C                   EXSR      LETFIL
016900000000     C                   END
017000000000     C*
017100000000     C                   ENDSR
017200000000     C*------------------------------------------------------------------------*
017300000000     C* LETFIL - LETTURA PROSSIMO RECORD
017400000000     C*------------------------------------------------------------------------*
017500000000     C     LETFIL        BEGSR
017600000000     C*
017700000000     C* LEGGO FINO A:
017800000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
017900000000     C*     - TROVATO RECORD VALIDO
018000000000     C                   MOVEL     'N'           WRECOK
018100000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
018200000000     C     WRECOK        ANDEQ     'N'
018300000000     C*
018301060809     c                   setoff                                       0102
018400000000     C* LETTURE SUCCESSIVE ALLA PRIMA
018500060904     C     ksc(III)      READe     titas31c                               99
018600000000     C*
018700060809     C                   Z-ADD     tasAAS        DSAAS                          *DATA SPED BOLLA
018800060809     C                   Z-ADD     tasMGS        DSMGS
018801070530     c* imposto il peso con regola CML
018802070530IF  2C     tasPKC        IFGT      *ZEROS
018803070530     C     tasNCL        ANDEQ     tasNCP
018804070530     C                   Z-ADD     tasPKC        Wpes
018805070530X   2C                   ELSE
018806070530     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
018807070530IF  3C     tasPKF        IFGT      tasPKC
018808070530     C                   Z-ADD     tasPKF        Wpes
018809070530X   3C                   ELSE
018810070530     C                   Z-ADD     tasPKC        Wpes
018811070530E   3C                   ENDIF
018812070530E   2C                   ENDIF
018813070530     c* imposto il volume con regola CML
018814070530IF  1C     tasFVF        IFEQ      'T'
018815070530     C                   Z-ADD     tasVLF        Wvol
018816070530X   1C                   ELSE
018817070530     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
018818070530IF  2C     tasVLC        IFGT      *ZEROS
018819070530     C     tasNCL        ANDEQ     tasNCR
018820070530     C                   Z-ADD     tasVLC        Wvol
018821070530X   2C                   ELSE
018822070530     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
018823070530IF  3C     tasVLF        IFGT      tasVLC
018824070530     C                   Z-ADD     tasVLF        Wvol
018825070530X   3C                   ELSE
018826070530     C                   Z-ADD     tasVLC        Wvol
018827070530E   3C                   ENDIF
018828070530E   2C                   ENDIF
018829070530E   1C                   ENDIF
018900000000     C*
019000000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
019200000000     C                   MOVEL     '1'           FINL00
019300000000     C                   ELSE
019400000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
019500000000     C                   END
019600000000     C*
019700000000     C                   END                                                    <-- 1 ---
019800000000     C*
019900000000     C                   ENDSR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
020200000000     C*------------------------------------------------------------------------*
020300000000     C     CHKREC        BEGSR
020400000000     C*
020500000329     C                   MOVEL     'S'           WRECOK            1
020600020911     C*
020700060904     C* VERIFICO CHE LA LINEA DI PARTENZA CORRENTE SIA = lnp richiesta
020701060904     c     parln1        ifne      0
020702060904     c     tasksc        andeq     parks1
020800060904     C     taslnp        andne     parln1
021000020911     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021200020911E   1C                   ENDIF
021201060904     c     parln2        ifne      0
021202060904     c     tasksc        andeq     parks2
021203060904     C     taslnp        andne     parln2
021204060904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021205060904E   1C                   ENDIF
021206060904     c     parln3        ifne      0
021207060904     c     tasksc        andeq     parks3
021208060904     C     taslnp        andne     parln3
021209060904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021210060904E   1C                   ENDIF
021211060904     c     parln4        ifne      0
021212060904     c     tasksc        andeq     parks4
021213060904     C     taslnp        andne     parln4
021214060904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021215060904E   1C                   ENDIF
021216060904     c     parln5        ifne      0
021217060904     c     tasksc        andeq     parks5
021218060904     C     taslnp        andne     parln5
021219060904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
021220060904E   1C                   ENDIF
021221060904     C* CONTROLLO DATA
021222060904     C                   Z-ADD     tasAAS        DSAAS
021223060904     C                   Z-ADD     tasMGS        DSMGS
021224060904     C     DSDAT         IFGE      PARDS1
021225060904     C     DSDAT         ANDLE     PARDS2
021226060904     C                   ELSE
021227060904     C                   MOVEL     'N'           WRECOK
021228060904     C                   ENDIF
021300000329     C*---
021400000329     C* CONTROLLO CLIENTE/TARIFFE
021500000329     C*---
021600000329     C* CLIENTE 1
021700000329     C                   MOVEL     'N'           $CTOK             1            *CLIENTE/TARIFFA OK
022000060904IF  3C     tasksc        IFEQ      PARks1                                       *TARIFFA = TARIFFA
022001060904IF  3C     tasCTR        IFEQ      PARTA1                                       *TARIFFA = TARIFFA
022100000329     C     PARTA1        OREQ      999                                          *TARIFFA = TUTTE
022101060809IF  3C     taslnp        IFEQ      PARln1                                       *TARIFFA = TARIFFA
022102060904     C     PARln1        OREQ      0                                            *TARIFFA = TUTTE
022200000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 1 O
022300000329E   3C                   ENDIF
022301060904E   3C                   ENDIF
022302060809E   3C                   ENDIF
022600000329     C* CLIENTE 2
022601060904IF  3C     tasksc        IFEQ      PARks2                                       *TARIFFA = TARIFFA
022900060809IF  3C     tasCTR        IFEQ      PARTA2                                       *TARIFFA = TARIFFA
023000000329     C     PARTA2        OREQ      999                                          *TARIFFA = TUTTE
023001060809IF  3C     taslnp        IFEQ      PARln2                                       *TARIFFA = TARIFFA
023002060904     C     PARln2        OREQ      0                                            *TARIFFA = TUTTE
023100000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 2 O
023400000329E   1C                   ENDIF
023401060904E   1C                   ENDIF
023402060809E   1C                   ENDIF
023500000329     C* CLIENTE 3
023501060904IF  3C     tasksc        IFEQ      PARks3                                       *TARIFFA = TARIFFA
023800060809IF  3C     tasCTR        IFEQ      PARTA3                                       *TARIFFA = TARIFFA
023900000329     C     PARTA3        OREQ      999                                          *TARIFFA = TUTTE
023901060809IF  3C     taslnp        IFEQ      PARln3                                       *TARIFFA = TARIFFA
023902060904     C     PARln3        OREQ      0                                            *TARIFFA = TUTTE
024000000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 3 O
024300000329E   1C                   ENDIF
024301060809E   1C                   ENDIF
024302060904E   1C                   ENDIF
024400000329     C* CLIENTE 4
024401060904IF  3C     tasksc        IFEQ      PARks4                                       *TARIFFA = TARIFFA
024700060809IF  3C     tasCTR        IFEQ      PARTA4                                       *TARIFFA = TARIFFA
024800000329     C     PARTA4        OREQ      999                                          *TARIFFA = TUTTE
024801060809IF  3C     taslnp        IFEQ      PARln4                                       *TARIFFA = TARIFFA
024802060904     C     PARln4        OREQ      0                                            *TARIFFA = TUTTE
024900000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 4 O
025200000329E   1C                   ENDIF
025201060904E   1C                   ENDIF
025202060809E   1C                   ENDIF
025300000329     C* CLIENTE 5
025500060809IF  2C     tasKSC        IFEQ      PARKS5                                       *CLIENTE = CLIENTE 5
025600060809IF  3C     tasCTR        IFEQ      PARTA5                                       *TARIFFA = TARIFFA
025700000329     C     PARTA5        OREQ      999                                          *TARIFFA = TUTTE
025701060809IF  3C     taslnp        IFEQ      PARln5                                       *TARIFFA = TARIFFA
025702060904     C     PARln5        OREQ      0                                            *TARIFFA = TUTTE
025800000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 5 O
026000000329E   2C                   ENDIF
026100000329E   1C                   ENDIF
026101060809E   1C                   ENDIF
026200000329     C*
026300000329IF  1C     $CTOK         IFEQ      'N'                                          *NO CLI/TAR OK
026400000329     C                   MOVEL     'N'           WRECOK
026500000329E   1C                   ENDIF
027500970418     C*
027600970418     C* CONTROLLO SELEZIONE PER CODICE TASSAZIONE
027700970415IF  1C     WRECOK        IFEQ      'S'
027800970415IF  2C     PARCT1        IFEQ      *BLANKS
027900970415     C     PARCT2        ANDEQ     *BLANKS
028000970415     C     PARCT3        ANDEQ     *BLANKS
028100970415     C     PARCT4        ANDEQ     *BLANKS
028200970415     C     PARCT5        ANDEQ     *BLANKS
028300970415     C     PARCT6        ANDEQ     *BLANKS
028400970415     C     PARCT7        ANDEQ     *BLANKS
028500970415     C     PARCT8        ANDEQ     *BLANKS
028600970415     C     PARCT9        ANDEQ     *BLANKS
028700970415     C     PARCT0        ANDEQ     *BLANKS
028800970415X   2C                   ELSE
028900060809IF  3C     tasCTS        IFEQ      PARCT1
029000060809     C     tasCTS        OREQ      PARCT2
029100060809     C     tasCTS        OREQ      PARCT3
029200060809     C     tasCTS        OREQ      PARCT4
029300060809     C     tasCTS        OREQ      PARCT5
029400060809     C     tasCTS        OREQ      PARCT6
029500060809     C     tasCTS        OREQ      PARCT7
029600060809     C     tasCTS        OREQ      PARCT8
029700060809     C     tasCTS        OREQ      PARCT9
029800060809     C     tasCTS        OREQ      PARCT0
029900970415X   3C                   ELSE
030000970415     C                   MOVEL     'N'           WRECOK
030100970415E   3C                   ENDIF
030200970415E   2C                   ENDIF
030300970415E   1C                   ENDIF
030400970418     C*
030500970418     C* CONTROLLO SELEZIONE PER PESO
030600970415IF  1C     WRECOK        IFEQ      'S'
030700970415IF  2C     PARKGD        IFEQ      *ZEROS
030800970415     C     PARKGA        ANDEQ     *ZEROS
030900970415X   2C                   ELSE
031000070530IF  3C     wpes          IFGE      PARKGD
031100070530     C     wpes          ANDLE     PARKGA
031200970415X   3C                   ELSE
031300970415     C                   MOVEL     'N'           WRECOK
031400970415E   3C                   ENDIF
031500970415E   2C                   ENDIF
031600970415E   1C                   ENDIF
031700970418     C*
031800970418     C* CONTROLLO SELEZIONE PER COLLO
031900970415IF  1C     WRECOK        IFEQ      'S'
032000970415IF  2C     PARCOD        IFEQ      *ZEROS
032100970415     C     PARCOA        ANDEQ     *ZEROS
032200970415X   2C                   ELSE
032300060809IF  3C     tasNCL        IFGE      PARCOD
032400060809     C     tasNCL        ANDLE     PARCOA
032500970415X   3C                   ELSE
032600970415     C                   MOVEL     'N'           WRECOK
032700970415E   3C                   ENDIF
032800970415E   2C                   ENDIF
032900970415E   1C                   ENDIF
033000020301     C*
033100020301     C* VERIFICO SE LNP/LNA E' FILIALE FEDEX
033200060809     C                   IF        tasLNP > *zeros
033300060809     C     tasLNP        LOOKUP    FILFED                                 55
033400020301     C                   IF        *IN55 = *ON
033500020301     C                   MOVEL     'N'           WRECOK
033600020301     C                   ENDIF
033700020301     C                   ENDIF
033800060809     C                   IF        tasLNA > *zeros
033900060809     C     tasLNA        LOOKUP    FILFED                                 55
034000020301     C                   IF        *IN55 = *ON
034100020301     C                   MOVEL     'N'           WRECOK
034200020301     C                   ENDIF
034300020301     C                   ENDIF
034400020301     C*
034500000000     C                   ENDSR
034600000000     C*------------------------------------------------------------------------*
034700970415     C* LANCIA - LANCIA PGM CHE RESTITUISCE RICAVI E COMPETENZE
034800000000     C*------------------------------------------------------------------------*
034900970415     C     LANCIA        BEGSR
035000000000     C*
035100970416     C     $ERR          IFEQ      '0'
035200970415     C                   MOVEL     *BLANKS       D50TLA
035300970415     C                   MOVEL     'P'           D50TLA
035400060809     C                   Z-ADD     tasAAS        D50AAS
035500060809     C                   Z-ADD     tasLNP        D50LNP
035600060809     C                   Z-ADD     tasNRS        D50NRS
035700060809     C                   Z-ADD     tasNSP        D50NSP
035800970415     C                   MOVEL     *BLANKS       D50CTO
036000970415     C                   Z-ADD     PARKTT        D50KSC
036100970415     C                   Z-ADD     PARTTT        D50CTR
036600970415     C                   Z-ADD     *ZEROS        D50PRG
036700970415     C     PARDVT        IFGT      *ZEROS
036800970415     C                   Z-ADD     PARDVT        D50DTR
036900970415     C                   ELSE
037000970415     C                   Z-ADD     *ZEROS        D50DTR
037100970415     C                   ENDIF
037200970415     C                   Z-ADD     *ZEROS        D50RIC
037300970415     C                   Z-ADD     *ZEROS        D50COM
037400970415     C                   Z-ADD     *ZEROS        D50DEL
037500970415     C                   Z-ADD     *ZEROS        D50PVL
037600970416     C                   MOVEL     *BLANKS       D50ERR
037700970416     C                   MOVEL     *BLANKS       D50MSG
037800970415     C                   CALL      'TNTE50R'
037900970415     C                   PARM                    DS50
038000970415     C                   ENDIF
038100000000     C*
038200000000     C                   ENDSR
038300970415     C*------------------------------------------------------------------------*
038400970415     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
038500970415     C*------------------------------------------------------------------------*
038600970415     C     CREARE        BEGSR
038700970415     C*
038800981001     C                   MOVEL     JOB           WFDUSR
038900060809     C                   Z-ADD     tasAAS        WFDAAS
039000060809     C                   Z-ADD     tasMGS        WFDMGS
039100060809     C                   Z-ADD     tasLNP        WFDLNP
039200060809     C                   Z-ADD     tasNRS        WFDNRS
039300060809     C                   Z-ADD     tasNSP        WFDNSP
039400060809     C                   Z-ADD     tasKSC        WFDKSC
039500970418     C* CODICE TASSAZIONE
039600060809     C                   MOVEL     tasCTS        WCTSI
039700970418     C                   EXSR      CERCTS
039800970418     C                   MOVEL     WCTSO         WFDCTS
039900970418     C* SCAGLIONE
040000000721     C                   MOVEL     PARTTT        WTIP              1            *TIPO TARIFFA
040100000721     C     WTIP          IFEQ      '0'                                          *A QUINTALI
040200000721     C     WTIP          OREQ      '3'                                          *A KILOGRAMMI
040201070530     C                   Z-ADD     wpes          WSGL             13 3
040800000721     C                   ELSE
040900060809     C                   Z-ADD     tasNCL        WSGL
041000000721     C                   ENDIF
041100970418     C                   DO        ULTSGL        I
041200000721     C     WSGL          IFLE      SSGL(I)
041300970416     C                   Z-ADD     SSGL(I)       WFDSGL
041400970415     C                   LEAVE
041500970415     C                   ENDIF
041600970415     C                   ENDDO
041700970707     C     D50COM        IFEQ      *ZEROS
041800970707     C                   Z-ADD     *ZEROS        WFDPKG
041900970707     C                   Z-ADD     *ZEROS        WFDNCL
042000970707     C                   Z-ADD     *ZEROS        WFDVLB
042100970707     C                   Z-ADD     *ZEROS        WFDQFT
042200970707     C                   ELSE
042300070530     C                   Z-ADD     wpes          WFDPKG
042500070530     C                   Z-ADD     wvol          WFDVLB
042501070528     C                   Z-ADD     tasNCL        WFDNCL
042600060809     C                   Z-ADD     tasQFT        WFDQFT
042700970707     C                   ENDIF
042800970415     C                   Z-ADD     D50RIC        WFDRIC
042900970415     C                   Z-ADD     D50COM        WFDCOM
043000970415     C                   Z-ADD     D50DEL        WFDDEL
043100970415     C                   MOVEL     D50MSG        WFDERR
043200970513     C                   WRITE     WFDBF000
043300970415     C*
043400970415     C                   ENDSR
043500970418     C*------------------------------------------------------------------------*
043600970418     C* CERCTS - CERCA IL CTS DELLA TARIFFA
043700970418     C*------------------------------------------------------------------------*
043800970418     C     CERCTS        BEGSR
043900970418     C*
044000970418     C* CONTROLLA CON CODICE TASSAZIONE ELEMENTARE
044100970418     C                   MOVEL     *BLANKS       WCTSO
044200970418     C                   DO        ULTCTS        I
044300970418     C     WCTSI         IFEQ      CTS(I)
044400970418     C                   MOVEL     WCTSI         WCTSO
044500970418     C                   ENDIF
044600970418     C                   ENDDO
044700970418     C*
044800970418     C* SE NON TROVATO, CONTROLLA CON CODICE TASSAZIONE REGIONALE
044900970418IF  1C     WCTSO         IFEQ      *BLANKS
045000970418     C                   MOVEL     CODUT         KTBKUT
045100970418     C                   MOVE      'CT'          KTBCOD
045200970418     C                   MOVE      *BLANKS       KTBKEY
045300970418     C                   MOVEL     WCTSI         KTBKEY
045400970418     C     KEYTAB        CHAIN     TABEL00F                           99
045500970418IF  2C     *IN99         IFEQ      '1'
045600970418     C     TBLFLG        OREQ      '*'
045700970418     C                   MOVEL     'TE56'        D50ERR
045800011102     C                   MOVEL     MSG(1)        D50MSG                         *CTS NON TROV.TAB.CT
045900970418     C                   MOVEL     '1'           $ERR
046000970418X   2C                   ELSE
046100970418     C                   MOVEL     TBLUNI        DSCT
046200970418     C                   DO        ULTCTS        I
046300970418     C     §CTRAP        IFEQ      CTS(I)
046400970418     C                   MOVEL     §CTRAP        WCTSO
046500970418     C                   ENDIF
046600970418     C                   ENDDO
046700970418E   2C                   ENDIF
046800970418E   1C                   ENDIF
046900970418     C*
047000970418     C* SE NON TROVATO, CONTROLLA CON CODICE TASSAZIONE NAZIONALE
047100970418IF  1C     WCTSO         IFEQ      *BLANKS
047200970418     C     $ERR          OREQ      '1'
047300970418IF  2C     §CTNAR        IFEQ      *BLANKS
047400970418     C                   MOVEL     'IT'          WNAZ              2
047500970418X   2C                   ELSE
047600970418     C                   MOVEL     'EE'          WNAZ
047700970418E   2C                   ENDIF
047800970418     C                   DO        ULTCTS        I
047900970418     C     WNAZ          IFEQ      CTS(I)
048000970418     C                   MOVEL     WNAZ          WCTSO
048100970418     C                   ENDIF
048200970418     C                   ENDDO
048300970418E   1C                   ENDIF
048400970418     C*
048500970418     C                   ENDSR
048600970415     C*------------------------------------------------------------------------*
048700970415     C* CARSGL - CARICA GLI SCAGLIONI
048800970415     C*------------------------------------------------------------------------*
048900970415     C     CARSGL        BEGSR
049000011102     C*
049100011102     C* AZZERA VARIABILI DI LAVORO
049200011102     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO NO TROVAT
049300011102     C                   Z-ADD     *ZEROS        I                 4 0
049400011102     C                   Z-ADD     *ZEROS        C                 4 0
049500011102     C*---
049600970415     C* CERCA IL PROGRESSIVO VALIDO SULLA TESTATA
049700011102     C*---
049800011102     C                   Z-ADD     *ZEROS        TAMPRG
049900970415     C                   Z-ADD     PARKTT        KTMKSC
050000970415     C                   Z-ADD     PARTTT        KTMCTR
050100970415     C     KEYTAM        SETLL     TNTAM01L
050200970415     C     KEYTAM        READE     TNTAM01L                               99
050300011102DO  1C     *IN99         DOWEQ     *OFF
050400011102IF  2C     TAMATB        IFNE      'A'                                          *NO ANNULLATE
050500970415     C     TAMBAP        ANDNE     'B'                                          *NO BLOCCATE
050600011102IF  3C     PARDVT        IFGE      TAMDDT                                       *IN DECORRENZA
050700970415     C     PARDVT        ANDLE     TAMDST
050800970415     C                   LEAVE
050900011102E   3c                   ENDIF
051000011102E   2C                   ENDIF
051100970506     C     KEYTAM        READE     TNTAM01L                               99
051200011102E   1C                   ENDDO
051300011102     C*---
051400011102     C* CERCA IL DETTAGLIO TARIFFARIO -DEL PROGRESSIVO VALIDO TROVATO IN TESTATA-
052000970415     C                   Z-ADD     PARKTT        KTDKSC
052100970415     C                   Z-ADD     PARTTT        KTDCTR
052200970415     C                   Z-ADD     TAMPRG        KTDPRG
052300011102     C     KEYTAD        SETLL     TITAD04L
052400990820     C     KEYTAD        READE     TITAD04L                               99
052500011102DO  1C     *IN99         DOWEQ     *OFF
052600011102IF  2C     TADATB        IFEQ      *BLANKS
052700011102IF  3C     PARLNP        IFGT      *ZEROS
052800011102     C     TADLNP        ANDEQ     PARLNP
052901060809     c     PARLNP        OREQ      999
053000011102     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
053100011102     C                   EXSR      MEMTAD                                       *MEMORIZZA DATI
053200011102E   3C                   ENDIF
053300011102E   2C                   ENDIF
053400011102     C     KEYTAD        READE     TITAD04L                               99
053500011102E   1C                   ENDDO                                                  *FINE DETTAGLIO
053600011102     C*
053700011102     C* DETTAGLIO CLIENTE NON TROVATO, PROVA CON LA CARTELLO
053800011102IF  1C     $TAD          IFEQ      '0'                                          *DETT.NON TROVATO
053900011102     C                   Z-ADD     CARKSC        KTDKSC
054000011102     C                   Z-ADD     CARCTR        KTDCTR
054100011102     C                   Z-ADD     CARPRG        KTDPRG
054200011102     C     KEYTAD        SETLL     TITAD04L
054300011102     C     KEYTAD        READE     TITAD04L                               99
054400011102DO  1C     *IN99         DOWEQ     *OFF
054500011102IF  2C     TADATB        IFEQ      *BLANKS
054600011102IF  3C     PARLNP        IFGT      *ZEROS
054700011102     C     TADLNP        ANDEQ     PARLNP
054801060809     c     PARLNP        OREQ      999
054900011102     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
055000011102     C                   EXSR      MEMTAD                                       *MEMORIZZA DATI
055100011102E   3C                   ENDIF
055200011102E   2C                   ENDIF
055300011102     C     KEYTAD        READE     TITAD04L                               99
055400011102E   1C                   ENDDO                                                  *FINE DETTAGLIO
055500990210E   1C                   ENDIF
055600011102     C*
055700011102     C* ELEMENTI MEMORIZZATI
055800011102     C                   Z-ADD     I             ULTSGL            6 0
055900011102     C                   Z-ADD     C             ULTCTS            6 0
056000011102     C*
056100011102     C* DETTAGLIO NON TROVATO, ESCE DAL PGM
056200011102IF  1C     $TAD          IFEQ      '0'                                          *DETT.NON TROVATO
056300011102     C                   MOVEL     '1'           FINL00                         *FINE PGM
056400011102E   1C                   ENDIF
056500011102     C*
056600970415     C                   ENDSR
056700011102     C*------------------------------------------------------------------------*
056800011102     C* MEMORIZZA DETTAGLIO TARIFFARIO
056900011102     C*------------------------------------------------------------------------*
057000011102     C     MEMTAD        BEGSR
057100011102     C*
057200011102     C* SCAGLIONI
057300011102     C                   SETOFF                                           98
057400011102     C     TADSGL        LOOKUP    SSGL                                   98
057500011102IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
057600011102     C                   ADD       1             I
057700011102     C                   Z-ADD     TADSGL        SSGL(I)
057800011102E   1C                   ENDIF
057900011102     C* CODICI TASSAZIONE
058000011102     C                   SETOFF                                           98
058100011102     C     TADCTS        LOOKUP    CTS                                    98
058200011102IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
058300011102     C                   ADD       1             C
058400011102     C                   MOVEL     TADCTS        CTS(C)
058500011102E   1C                   ENDIF
058600011102     C*
058700011102     C                   ENDSR
058800011102     C*------------------------------------------------------------------------*
058900011102     C* CARICA LA TARIFFA DI CARTELLO
059000011102     C*------------------------------------------------------------------------*
059100011102     C     CARCAR        BEGSR
059200011102     C*
059300021202     C                   Z-ADD     *ZEROS        CARKSC
059400021202     C                   Z-ADD     *ZEROS        CARCTR
059500021202     C                   MOVE(P)   *ZEROS        CARPRG
059600021202     C                   CLEAR                   TRUL27DS
059700021202     C                   EVAL      I27TLA = 'L'
059800021202     C                   EVAL      I27DTA = PARDVT
059900021202     C                   CALL      'TRUL27R'
060000021202     C                   PARM                    TRUL27DS
060100021202     C*
060200021202     C                   IF        O27ERR <> 'E'
060300021202     C                   Z-ADD     O27KSC        CARKSC
060400021202     C                   Z-ADD     O27CTR        CARCTR
060500021202     C                   MOVE(P)   O27PRG        CARPRG
060600021202     C                   ENDIF
060700011102     C*
060800011102     C                   ENDSR
060900011102     C*------------------------------------------------------------------------*
061000011102     C* CARICA LE TABELLE OCCORRENTI
061100011102     C*------------------------------------------------------------------------*
061200011102     C     CARTAB        BEGSR
061300020911     C*
061400020911     C* REPERISCE I DATI UTENTE
061500020911     C                   EXSR      REPDATIUTE
061600020301     C*
061700020301     C* CARICA FILIALI FEDEX
061800020301     C                   Z-ADD     *ZEROS        K                 3 0
061900020301     C     *LOVAL        SETLL     AZORG01L
062000020301     C                   READ      AZORG01L                               98
062100020301DO  1C     *IN98         DOWEQ     *OFF
062200020301IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
062300020301     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
062400020301     C* VERIFICO SE FILIALE FEDEX
062500020301     C                   MOVEL     ORGDE3        OG143
062600020301     C                   IF        §OGNTW = 'FED'
062700020301     C                   ADD       1             K
062800020301     C                   Z-ADD     ORGFIL        FILFED(K)
062900020301     C                   ENDIF
063000020301E   2C                   ENDIF
063100020301     C                   READ      AZORG01L                               98
063200020301E   1C                   ENDDO
063300011102     C*
063400011102     C                   ENDSR
063500020911     C*--------------------------------------------------------------------------------------------*
063600020911     C* REPERISCE I DATI UTENTE
063700020911     C*--------------------------------------------------------------------------------------------*
063800020911     C     REPDATIUTE    BEGSR
063900020911     C*
064000020911     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
064100020911     C                   CLEAR                   L1
064700020911     C                   CLEAR                   TIBS34DS
064800020911     C                   CLEAR                   AZUTEDS
064900020911     C                   CLEAR                   DDATIUTE
064901060809     C                   CLEAR                   WABI              2
064902060809     C                   CLEAR                   DLAT
064903060809     C                   CLEAR                   werr              1
064904060809     c                   movea     lnp           l1
065000020911     C*
065100020911     C     *DTAARA       DEFINE    §azute        azuteds
065200020911     C     *DTAARA       DEFINE    §datiute      ddatiute
065300020911     C                   IN(E)     *DTAARA
065400020911if  1C                   IF        %Error
065500020911     c                   EVAL      I34Tla = 'L'
065600020911     C                   CALL      'TIBS34R'
065700020911     C                   PARM                    Tibs34Ds
065800020911     C                   IN        *DTAARA
065900020911e   1C                   ENDIF
065901060809     C* carico la schiera dei po da elaborare solo se non richiesto nel
065902060809     c* lancio
065903060809     c                   if        (lnp(1) = 0 and parks1 <> 0) or
065904060809     c                             (lnp(2) = 0 and parks2 <> 0) or
065905060809     c                             (lnp(3) = 0 and parks3 <> 0) or
065906060809     c                             (lnp(4) = 0 and parks4 <> 0) or
065907060809     c                             (lnp(5) = 0 and parks5 <> 0)
066000020911     C*
066001060809     C                   SELECT
066002060809     C                   WHEN      DUTERR = 'E'
066003060809     C                   EVAL      WERR  = '1'
066005060809     C*
066006060809     C* NON siamo in sede (filiale remota o filiale locale)
066007060809     C* --> se 1° livello, abilitazioni al terminal
066008060809     C*     se 2° livello, abilitazioni al punto operativo
066009060809     C                   WHEN      UTEAUT = *blanks
066010060809     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
066011060809     C                   EVAL      WABI   = 'TP'
066012060809     C                   ENDIF
066013060809     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
066014060809     C                   EVAL      WABI   = 'PO'
066015060809     C                   ENDIF
066016060809     C*
066017060809     C* siamo in sede
066018060809     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
066019060809     C*     carica le abilitazioni proprie del profilo
066020060809     C                   OTHER
066021060809     C                   MOVEL     UTEFAF        DUTE01
066022060809     C                   IF        §UTEIST <> *blanks                           *esiste specifica
066023060809     C                   EVAL      WABI = §UTEIST
066024060809     C                   ELSE                                                   *NON esiste specific
066025060809     C                   EVAL      WABI = UTEAUT
066026060809     C                   ENDIF
066027060809     C                   ENDSL
066028060809     C*
066029060809     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
066030060809     C                   CLEAR                   TIBS02DS
066031060809     C                   MOVEL     'C'           T02MOD
066032060809     C                   MOVEL     KNSIF         T02SIF
066033060809     C                   MOVEL     'LAT'         T02COD
066034060809     C                   MOVEL(P)  WABI          T02KE1
066035060809     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
066036060809     C                   CALL      'TIBS02R'
066037060809     C                   PARM                    KPJBA
066038060809     C                   PARM                    TIBS02DS
066039060809     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
066040060809     C                   IF        T02ERR = *blanks
066041060809     C                   MOVEL     T02UNI        DLAT
066042060809     C                   ENDIF
066043060809     C                   IF        T02ERR <> *blanks OR
066044060809     C                             §LATABI = 'S'
066045060809     C                   EVAL      WERR  = '1'
066047060809     C                   ENDIF
066048060809     C*
066049060809     C* Reperisce i P.O. gestibili dall'utente
066050060809     C                   IF        WERR <> '1'                                  *NO errori
066051060809     C                   CLEAR                   TRUL31DS
066053060809     C                   EVAL      I31ABI   = WABI
066054060809     C                   EVAL      I31CDI   = DUTDIS
066055060809     C                   EVAL      I31CAR   = DUTARE
066056060809     C                   EVAL      I31CPO   = DUTPOU
066057060809     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
066058060809     C                   CALL      'TRUL31R'
066059060809     C                   PARM                    KPJBA
066060060809     C                   PARM                    TRUL31DS
066062060809     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
066063060809if  2C                   IF        O31POG > *zeros
066064060809     C                   MOVEA     s31           L1
066065060809x   2C                   ELSE
066066060809     C                   EVAL      WERR  = '1'
066068060809e   2C                   ENDIF
066069060809e   1C                   ENDIF
066070060809e   1C                   ENDIF
068000020911     C*
068100020911     C                   ENDSR
068200000000     C*------------------------------------------------------------------------*
068300000000     C* *INZSR - ROUTINE INIZIALE
068400000000     C*------------------------------------------------------------------------*
068500000000     C     *INZSR        BEGSR
068600000000     C*--------------------
068700000000     C* RICEVIMENTO PARAMETRI
068800000000     C*--------------------
068900000000     C     *ENTRY        PLIST
069000000000     C                   PARM                    KPJBA
069100950703     C                   MOVEL     KPJBU         PARAM
069101060904     c                   move      parks1        ksc(1)
069102060904     c                   move      parks2        ksc(2)
069103060904     c                   move      parks3        ksc(3)
069104060904     c                   move      parks4        ksc(4)
069105060904     c                   move      parks5        ksc(5)
069200000000     C*--------------------
069300970418     C* VARIABILI RIFERITE AL DATA BASE
069400000000     C*--------------------
069500970418     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
069600970418     C     *LIKE         DEFINE    TBLCOD        KTBCOD
069700970418     C     *LIKE         DEFINE    TBLKEY        KTBKEY
069800060809     C     *LIKE         DEFINE    tasLNP        KBPLNP
069900060809     C     *LIKE         DEFINE    tasAAS        KBLAAS
070000060809     C     *LIKE         DEFINE    tasMGS        KBLMGS
070100060809     C     *LIKE         DEFINE    TAMKSC        KTMKSC
070200970415     C     *LIKE         DEFINE    TAMCTR        KTMCTR
070300060809     C     *LIKE         DEFINE    TADKSC        KTDKSC
070400970415     C     *LIKE         DEFINE    TADCTR        KTDCTR
070500970415     C     *LIKE         DEFINE    TADPRG        KTDPRG
070600060809     C     *LIKE         DEFINE    TADCTS        WCTSI
070700970418     C     *LIKE         DEFINE    TADCTS        WCTSO
070800011102     C     *LIKE         DEFINE    TAMKSC        CARKSC
070900011102     C     *LIKE         DEFINE    TAMCTR        CARCTR
071000011102     C     *LIKE         DEFINE    TAMPRG        CARPRG
071100060809     C     *LIKE         DEFINE    KPJBU         DEPKPJBU
071200000000     C*--------------------
071300000000     C* CHIAVI DI LETTURA
071400000000     C*--------------------
071500060809     C* CHIAVE LETTURA titas39c - PARZIALE
071600060809     C     KEYtas        KLIST
071700020911     C                   KFLD                    KBPLNP                         *LINEA DI PARTENZA
071800970414     C                   KFLD                    KBLAAS
071900970414     C                   KFLD                    KBLMGS
072000970415     C* LETTURA TNTAM01L - PARZIALE
072100970415     C     KEYTAM        KLIST
072200970415     C                   KFLD                    KTMKSC                         *CLIENTE
072300970415     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
072400011102     C* LETTURA TITAD04L - PARZIAL
072500970415     C     KEYTAD        KLIST
072600970415     C                   KFLD                    KTDKSC                         *CLIENTE
072700970415     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
072800970415     C                   KFLD                    KTDPRG                         *PROGRESSIVO
072900970418     C* LETTURA TABEL00F - COMPLETA
073000970418     C     KEYTAB        KLIST
073100970418     C                   KFLD                    KTBKUT                         *UTENTE
073200970418     C                   KFLD                    KTBCOD                         *TABELLA
073300970418     C                   KFLD                    KTBKEY                         *CODICE
073400000000     C*--------------------
073500970418     C* VARIABILI DI CONTROLLO
073600000000     C*--------------------
073700000000     C                   Z-ADD     1             CODUT             1 0
073800970416     C                   MOVEL     '0'           FINL00            1
073900970416     C                   MOVEL     '0'           $ERR              1
074000970414     C*
074100970418     C* IMPOSTA LE DATE DI LANCIO
074200970415     C                   Z-ADD     PARDS1        DSDAT
074300970415     C                   Z-ADD     DSMGS         WMGSI             4 0
074400970415     C                   Z-ADD     DSAAS         WAASI             4 0
074500970415     C                   Z-ADD     PARDS2        DSDAT
074600970415     C                   Z-ADD     DSMGS         WMGSF             4 0
074700970415     C                   Z-ADD     DSAAS         WAASF             4 0
074800011102     C*
074900011102     C* CARICA TABELLE OCCORRENTI
075000011102     C                   EXSR      CARTAB
075100011102     C*
075200011102     C* CARICA TARIFFA DI CARTELLO
075300011102     C                   EXSR      CARCAR
075400970415     C*
075500970415     C* CARICA GLI SCAGLIONI DELLA TARIFFA
075600970415     C                   EXSR      CARSGL
075700000000     C*
075800970415     C                   ENDSR
075900970415     O*------------------------------------------------------------------------*
076000011102** MSG - MESSAGGI
076100011102TNTE56R- Cpdice tassazione non trovato in tabella "CT".                        1
076200011102TNTE56R- 2                                                                     2
076300970415TNTE56R- 3                                                                     3
076400970415TNTE56R- 4                                                                     4
076500970415TNTE56R- 5                                                                     5
076600970415TNTE56R- 6                                                                     6
076700970415TNTE56R- 7                                                                     7
076800970415TNTE56R- 8                                                                     8
076900970415TNTE56R- 9                                                                     9
077000970415TNTE56R- 10                                                                   10
077100970415TNTE56R- 11                                                                   11
077200970415TNTE56R- 12                                                                   12
077300970415TNTE56R- 13                                                                   13
077400970415TNTE56R- 14                                                                   14
077500970415TNTE56R- 15                                                                   15
077600970415TNTE56R- 16                                                                   16
077700970415TNTE56R- 17                                                                   17
077800970415TNTE56R- 18                                                                   18
077900970415TNTE56R- 19                                                                   19
078000970415TNTE56R- 20                                                                   20
