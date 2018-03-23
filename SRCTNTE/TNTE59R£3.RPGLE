000100000000     H*------------------------------------------------------------------------*
000200060809     H* STATISTICA DELTA BOLLE  - GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500060904     Ftitas31c  IF   E           K DISK    ignore(titasp00)
000600130226     FTNTAM00L  IF   E           K DISK
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
003900130507     D* DS PASSAGGIO PARAMETRI -TNTE50R-
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
006100130226     d  parks1                 1      7  0
006200130226     d  parks2                 8     14  0
006300130226     d  parks3                15     21  0
006400130226     d  parks4                22     28  0
006500130226     d  parks5                29     35  0
006600130226     d  ksc                    1     35  0 dim(5)
006700130226     d  parta1                36     38  0
006800130226     d  parta2                39     41  0
006900130226     d  parta3                42     44  0
007000130226     d  parta4                45     47  0
007100130226     d  parta5                48     50  0
007200130228     d  parln1                51     53  0
007300130228     d  parln2                54     56  0
007400130228     d  parln3                57     59  0
007500130228     d  parln4                60     62  0
007600130228     d  parln5                63     65  0
007700130228     d  lnp                   51     65  0 dim(5)
007800130228     d  pards1                66     73  0
007900130228     d  pards2                74     81  0
008000130228     d  parct1                82     83
008100130228     d  parct2                84     85
008200130228     d  parct3                86     87
008300130228     d  parct4                88     89
008400130228     d  parct5                90     91
008500130228     d  parct6                92     93
008600130228     d  parct7                94     95
008700130228     d  parct8                96     97
008800130228     d  parct9                98     99
008900130228     d  parct10              100    101
009000130228     d  parkgd               102    108  1
009100130228     d  parkga               109    115  1
009200130228     d  parcod               116    121  0
009300130228     d  parcoa               122    127  0
009400130228     d  partaof              128    128
009500130228     d  parksc               129    135  0
009600130228     d  parctr               136    138  0
009700130228     d  parprg               139    141  0
009800130228     d  parlnp               142    144  0
009900130228     d  pardtcal             145    145
010000130228     d  pardet               146    146
010100130228     d  parnrv               147    153  0
010200130228     d  parddt               154    161  0
010300130226
010400970418     D*-------------------
010500970418     D* ARCHIETTURA
010600970418     D*-------------------
010700970418     D KPJBA         E DS
010800020301     D*-------------------
010900020301     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
011000020301     D*-------------------
011100020301     D OG143         E DS
011200020911     D*-------------
011300020911     D* DS DI WRK
011400020911     D*-------------
011500060809     D TIBS02DS      E DS                                                       *Ricerca tabella
011600060809     D*------------------
011700060809     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
011800060809     D*-------------------
011900060809     D DUTE01        E DS                  INZ                                  *Abilitazioni utenti
012000060809     D*------------------
012100060809     D* DS FUNZIONI AZIENDALI
012200060809     D*-------------------
012300060809     D DLAT          E DS                  INZ                                  *Funzioni aziendali
012400060809     D TRUL31DS      E DS                  INZ
012500060809     D  S31                   10    759  0 dim(250)
012600020911     D TIBS34DS      E DS                                                       *Profili utente
012700020911     D DDATIUTE      E DS                                                       *Dati utente
012800020911     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
012900020911     D*-------------
013000020911     D* FILIALI IN GESTIONE
013100020911     D*-------------
013200060904     D l1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
013300070530     D wpes            S                   like(taspkf)                         *FILIALI IN GESTIONE
013400070530     D wvol            S                   like(tasvlf)                         *FILIALI IN GESTIONE
013500130226
013600130226       // -?Campi Data
013700130226     d wOggi           s              8s 0 inz
013800130226
013900060904     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS31C
014000060809     ITITAS000      01
014100060809     ITITAS010      02
014200000000     C*------------------------------------------------------------------------*
014300000000     C* MAIN LINE
014400000000     C*------------------------------------------------------------------------*
014500000000     C*
014600000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
014700000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
014800020911     C*
014900130226     C* CICLO X TUTTi i clienti richiesti
015000060904     C     1             DO        5             III               3 0
015100060904     C                   IF        ksc(III) > *ZEROS
015200020911     C                   MOVEL     '0'           FINL00
015300000000     C*
015400000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
015500000000     C                   EXSR      SETFIL
015600000000     C*
015700000000     C* CICLO FINO A FINE FILE
015800000000     C     FINL00        DOWEQ     '0'                                          --- 2 -->
015900970415     C*
016000970415     C* LANCIA PGM CALCOLO RICAVI/COMPETENZE
016100970415     C                   EXSR      LANCIA
016200000000     C*
016300000000     C* CREA IL RECORD SUI FILE DI LAVORO
016400000000     C                   EXSR      CREARE
016500000000     C*
016600000000     C* LEGGE IL RECORD SUCCESSIVO
016700000000     C                   EXSR      LETFIL
016800020911     C                   ENDDO                                                  <-- 2 ---
016900020911     C*
017000020911     C                   ELSE
017100020911     C                   LEAVE
017200020911     C                   ENDIF
017300020911     C                   ENDDO
017400020911     C                   ENDIF                                                  <-- 1 ---
017500000000     C*
017600970418     C                   MOVEL     'C'           D50TLA
017700130507     C                   CALL      'TNTE50R'
017800970418     C                   PARM                    DS50
017900970418     C*
018000000000     C                   SETON                                        LR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
018300000000     C*------------------------------------------------------------------------*
018400000000     C     SETFIL        BEGSR
018500000000     C*
018600000000     C* POSIZIONAMENTO E PRIMA LETTURA
018700060904     C     ksc(III)      SETLL     titas31c                           99        *FINE FILE
018800000000     C*
018900000000     C     *IN99         IFEQ      '1'
019000000000     C                   MOVEL     '1'           FINL00
019100000000     C                   ELSE
019200000000     C                   EXSR      LETFIL
019300000000     C                   END
019400000000     C*
019500000000     C                   ENDSR
019600000000     C*------------------------------------------------------------------------*
019700000000     C* LETFIL - LETTURA PROSSIMO RECORD
019800000000     C*------------------------------------------------------------------------*
019900000000     C     LETFIL        BEGSR
020000000000     C*
020100000000     C* LEGGO FINO A:
020200000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
020300000000     C*     - TROVATO RECORD VALIDO
020400000000     C                   MOVEL     'N'           WRECOK
020500000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
020600000000     C     WRECOK        ANDEQ     'N'
020700000000     C*
020800060809     c                   setoff                                       0102
020900000000     C* LETTURE SUCCESSIVE ALLA PRIMA
021000060904     C     ksc(III)      READe     titas31c                               99
021100000000     C*
021200060809     C                   Z-ADD     tasAAS        DSAAS                          *DATA SPED BOLLA
021300060809     C                   Z-ADD     tasMGS        DSMGS
021400070530     c* imposto il peso con regola CML
021500070530IF  2C     tasPKC        IFGT      *ZEROS
021600070530     C     tasNCL        ANDEQ     tasNCP
021700070530     C                   Z-ADD     tasPKC        Wpes
021800070530X   2C                   ELSE
021900070530     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA PESO FATTURATO E CML
022000070530IF  3C     tasPKF        IFGT      tasPKC
022100070530     C                   Z-ADD     tasPKF        Wpes
022200070530X   3C                   ELSE
022300070530     C                   Z-ADD     tasPKC        Wpes
022400070530E   3C                   ENDIF
022500070530E   2C                   ENDIF
022600070530     c* imposto il volume con regola CML
022700070530IF  1C     tasFVF        IFEQ      'T'
022800070530     C                   Z-ADD     tasVLF        Wvol
022900070530X   1C                   ELSE
023000070530     C* ALTRIMENTI SE I COLLI=COLLI CML PRENDO VOLUME CML
023100070530IF  2C     tasVLC        IFGT      *ZEROS
023200070530     C     tasNCL        ANDEQ     tasNCR
023300070530     C                   Z-ADD     tasVLC        Wvol
023400070530X   2C                   ELSE
023500070530     C* ALTRIMENTI PRENDO IL PIU' ALTO TRA VOUME FATTURATO E CML
023600070530IF  3C     tasVLF        IFGT      tasVLC
023700070530     C                   Z-ADD     tasVLF        Wvol
023800070530X   3C                   ELSE
023900070530     C                   Z-ADD     tasVLC        Wvol
024000070530E   3C                   ENDIF
024100070530E   2C                   ENDIF
024200070530E   1C                   ENDIF
024300000000     C*
024400000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
024500000000     C                   MOVEL     '1'           FINL00
024600000000     C                   ELSE
024700000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
024800000000     C                   END
024900000000     C*
025000000000     C                   END                                                    <-- 1 ---
025100000000     C*
025200000000     C                   ENDSR
025300000000     C*------------------------------------------------------------------------*
025400000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
025500000000     C*------------------------------------------------------------------------*
025600000000     C     CHKREC        BEGSR
025700000000     C*
025800000329     C                   MOVEL     'S'           WRECOK            1
025900020911     C*
026000060904     C* VERIFICO CHE LA LINEA DI PARTENZA CORRENTE SIA = lnp richiesta
026100060904     c     parln1        ifne      0
026200060904     c     tasksc        andeq     parks1
026300060904     C     taslnp        andne     parln1
026400020911     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026500020911E   1C                   ENDIF
026600060904     c     parln2        ifne      0
026700060904     c     tasksc        andeq     parks2
026800060904     C     taslnp        andne     parln2
026900060904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027000060904E   1C                   ENDIF
027100060904     c     parln3        ifne      0
027200060904     c     tasksc        andeq     parks3
027300060904     C     taslnp        andne     parln3
027400060904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027500060904E   1C                   ENDIF
027600060904     c     parln4        ifne      0
027700060904     c     tasksc        andeq     parks4
027800060904     C     taslnp        andne     parln4
027900060904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028000060904E   1C                   ENDIF
028100060904     c     parln5        ifne      0
028200060904     c     tasksc        andeq     parks5
028300060904     C     taslnp        andne     parln5
028400060904     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
028500060904E   1C                   ENDIF
028600060904     C* CONTROLLO DATA
028700060904     C                   Z-ADD     tasAAS        DSAAS
028800060904     C                   Z-ADD     tasMGS        DSMGS
028900060904     C     DSDAT         IFGE      PARDS1
029000060904     C     DSDAT         ANDLE     PARDS2
029100060904     C                   ELSE
029200060904     C                   MOVEL     'N'           WRECOK
029300060904     C                   ENDIF
029400000329     C*---
029500000329     C* CONTROLLO CLIENTE/TARIFFE
029600000329     C*---
029700000329     C* CLIENTE 1
029800000329     C                   MOVEL     'N'           $CTOK             1            *CLIENTE/TARIFFA OK
029900060904IF  3C     tasksc        IFEQ      PARks1                                       *TARIFFA = TARIFFA
030000060904IF  3C     tasCTR        IFEQ      PARTA1                                       *TARIFFA = TARIFFA
030100000329     C     PARTA1        OREQ      999                                          *TARIFFA = TUTTE
030200060809IF  3C     taslnp        IFEQ      PARln1                                       *TARIFFA = TARIFFA
030300060904     C     PARln1        OREQ      0                                            *TARIFFA = TUTTE
030400000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 1 O
030500000329E   3C                   ENDIF
030600060904E   3C                   ENDIF
030700060809E   3C                   ENDIF
030800000329     C* CLIENTE 2
030900060904IF  3C     tasksc        IFEQ      PARks2                                       *TARIFFA = TARIFFA
031000060809IF  3C     tasCTR        IFEQ      PARTA2                                       *TARIFFA = TARIFFA
031100000329     C     PARTA2        OREQ      999                                          *TARIFFA = TUTTE
031200060809IF  3C     taslnp        IFEQ      PARln2                                       *TARIFFA = TARIFFA
031300060904     C     PARln2        OREQ      0                                            *TARIFFA = TUTTE
031400000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 2 O
031500000329E   1C                   ENDIF
031600060904E   1C                   ENDIF
031700060809E   1C                   ENDIF
031800000329     C* CLIENTE 3
031900060904IF  3C     tasksc        IFEQ      PARks3                                       *TARIFFA = TARIFFA
032000060809IF  3C     tasCTR        IFEQ      PARTA3                                       *TARIFFA = TARIFFA
032100000329     C     PARTA3        OREQ      999                                          *TARIFFA = TUTTE
032200060809IF  3C     taslnp        IFEQ      PARln3                                       *TARIFFA = TARIFFA
032300060904     C     PARln3        OREQ      0                                            *TARIFFA = TUTTE
032400000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 3 O
032500000329E   1C                   ENDIF
032600060809E   1C                   ENDIF
032700060904E   1C                   ENDIF
032800000329     C* CLIENTE 4
032900060904IF  3C     tasksc        IFEQ      PARks4                                       *TARIFFA = TARIFFA
033000060809IF  3C     tasCTR        IFEQ      PARTA4                                       *TARIFFA = TARIFFA
033100000329     C     PARTA4        OREQ      999                                          *TARIFFA = TUTTE
033200060809IF  3C     taslnp        IFEQ      PARln4                                       *TARIFFA = TARIFFA
033300060904     C     PARln4        OREQ      0                                            *TARIFFA = TUTTE
033400000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 4 O
033500000329E   1C                   ENDIF
033600060904E   1C                   ENDIF
033700060809E   1C                   ENDIF
033800000329     C* CLIENTE 5
033900060809IF  2C     tasKSC        IFEQ      PARKS5                                       *CLIENTE = CLIENTE 5
034000060809IF  3C     tasCTR        IFEQ      PARTA5                                       *TARIFFA = TARIFFA
034100000329     C     PARTA5        OREQ      999                                          *TARIFFA = TUTTE
034200060809IF  3C     taslnp        IFEQ      PARln5                                       *TARIFFA = TARIFFA
034300060904     C     PARln5        OREQ      0                                            *TARIFFA = TUTTE
034400000329     C                   MOVEL     'S'           $CTOK                          *CLIENTE/TARIFFA 5 O
034500000329E   2C                   ENDIF
034600000329E   1C                   ENDIF
034700060809E   1C                   ENDIF
034800000329     C*
034900000329IF  1C     $CTOK         IFEQ      'N'                                          *NO CLI/TAR OK
035000000329     C                   MOVEL     'N'           WRECOK
035100000329E   1C                   ENDIF
035200970418     C*
035300970418     C* CONTROLLO SELEZIONE PER CODICE TASSAZIONE
035400970415IF  1C     WRECOK        IFEQ      'S'
035500970415IF  2C     PARCT1        IFEQ      *BLANKS
035600970415     C     PARCT2        ANDEQ     *BLANKS
035700970415     C     PARCT3        ANDEQ     *BLANKS
035800970415     C     PARCT4        ANDEQ     *BLANKS
035900970415     C     PARCT5        ANDEQ     *BLANKS
036000970415     C     PARCT6        ANDEQ     *BLANKS
036100970415     C     PARCT7        ANDEQ     *BLANKS
036200970415     C     PARCT8        ANDEQ     *BLANKS
036300970415     C     PARCT9        ANDEQ     *BLANKS
036400130226     C     PARCT10       ANDEQ     *BLANKS
036500970415X   2C                   ELSE
036600060809IF  3C     tasCTS        IFEQ      PARCT1
036700060809     C     tasCTS        OREQ      PARCT2
036800060809     C     tasCTS        OREQ      PARCT3
036900060809     C     tasCTS        OREQ      PARCT4
037000060809     C     tasCTS        OREQ      PARCT5
037100060809     C     tasCTS        OREQ      PARCT6
037200060809     C     tasCTS        OREQ      PARCT7
037300060809     C     tasCTS        OREQ      PARCT8
037400060809     C     tasCTS        OREQ      PARCT9
037500130226     C     tasCTS        OREQ      PARCT10
037600970415X   3C                   ELSE
037700970415     C                   MOVEL     'N'           WRECOK
037800970415E   3C                   ENDIF
037900970415E   2C                   ENDIF
038000970415E   1C                   ENDIF
038100970418     C*
038200970418     C* CONTROLLO SELEZIONE PER PESO
038300970415IF  1C     WRECOK        IFEQ      'S'
038400970415IF  2C     PARKGD        IFEQ      *ZEROS
038500970415     C     PARKGA        ANDEQ     *ZEROS
038600970415X   2C                   ELSE
038700070530IF  3C     wpes          IFGE      PARKGD
038800070530     C     wpes          ANDLE     PARKGA
038900970415X   3C                   ELSE
039000970415     C                   MOVEL     'N'           WRECOK
039100970415E   3C                   ENDIF
039200970415E   2C                   ENDIF
039300970415E   1C                   ENDIF
039400970418     C*
039500970418     C* CONTROLLO SELEZIONE PER COLLO
039600970415IF  1C     WRECOK        IFEQ      'S'
039700970415IF  2C     PARCOD        IFEQ      *ZEROS
039800970415     C     PARCOA        ANDEQ     *ZEROS
039900970415X   2C                   ELSE
040000060809IF  3C     tasNCL        IFGE      PARCOD
040100060809     C     tasNCL        ANDLE     PARCOA
040200970415X   3C                   ELSE
040300970415     C                   MOVEL     'N'           WRECOK
040400970415E   3C                   ENDIF
040500970415E   2C                   ENDIF
040600970415E   1C                   ENDIF
040700020301     C*
040800020301     C* VERIFICO SE LNP/LNA E' FILIALE FEDEX
040900060809     C                   IF        tasLNP > *zeros
041000060809     C     tasLNP        LOOKUP    FILFED                                 55
041100020301     C                   IF        *IN55 = *ON
041200020301     C                   MOVEL     'N'           WRECOK
041300020301     C                   ENDIF
041400020301     C                   ENDIF
041500060809     C                   IF        tasLNA > *zeros
041600060809     C     tasLNA        LOOKUP    FILFED                                 55
041700020301     C                   IF        *IN55 = *ON
041800020301     C                   MOVEL     'N'           WRECOK
041900020301     C                   ENDIF
042000020301     C                   ENDIF
042100020301     C*
042200000000     C                   ENDSR
042300000000     C*------------------------------------------------------------------------*
042400970415     C* LANCIA - LANCIA PGM CHE RESTITUISCE RICAVI E COMPETENZE
042500000000     C*------------------------------------------------------------------------*
042600970415     C     LANCIA        BEGSR
042700000000     C*
042800970416     C     $ERR          IFEQ      '0'
042900970415     C                   MOVEL     *BLANKS       D50TLA
043000970415     C                   MOVEL     'P'           D50TLA
043100060809     C                   Z-ADD     tasAAS        D50AAS
043200060809     C                   Z-ADD     tasLNP        D50LNP
043300060809     C                   Z-ADD     tasNRS        D50NRS
043400060809     C                   Z-ADD     tasNSP        D50NSP
043500970415     C                   MOVEL     *BLANKS       D50CTO
043600130226     C                   Z-ADD     PARKsc        D50KSC
043700130226     C                   Z-ADD     PARctr        D50CTR
043800130226     C                   Z-ADD     parprg        D50PRG
043900130226     c                   clear                   D50dtr
044000130226     c                   IF        pardtcal = 'O'
044100130226     c                   eval      D50dtr = wOggi
044200130226     c                   ENDIF
044300970415     C                   Z-ADD     *ZEROS        D50RIC
044400970415     C                   Z-ADD     *ZEROS        D50COM
044500970415     C                   Z-ADD     *ZEROS        D50DEL
044600970415     C                   Z-ADD     *ZEROS        D50PVL
044700970416     C                   MOVEL     *BLANKS       D50ERR
044800970416     C                   MOVEL     *BLANKS       D50MSG
044900130507     C                   CALL      'TNTE50R'
045000970415     C                   PARM                    DS50
045100130228     c                   parm                    parddt
045200970415     C                   ENDIF
045300000000     C*
045400000000     C                   ENDSR
045500970415     C*------------------------------------------------------------------------*
045600970415     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
045700970415     C*------------------------------------------------------------------------*
045800970415     C     CREARE        BEGSR
045900970415     C*
046000981001     C                   MOVEL     JOB           WFDUSR
046100060809     C                   Z-ADD     tasAAS        WFDAAS
046200060809     C                   Z-ADD     tasMGS        WFDMGS
046300060809     C                   Z-ADD     tasLNP        WFDLNP
046400060809     C                   Z-ADD     tasNRS        WFDNRS
046500060809     C                   Z-ADD     tasNSP        WFDNSP
046600060809     C                   Z-ADD     tasKSC        WFDKSC
046700970418     C* CODICE TASSAZIONE
046800060809     C                   MOVEL     tasCTS        WCTSI
046900970418     C                   EXSR      CERCTS
047000970418     C                   MOVEL     WCTSO         WFDCTS
047100970418     C* SCAGLIONE
047200130226     C                   MOVEL     PARctr        WTIP              1            *TIPO TARIFFA
047300000721     C     WTIP          IFEQ      '0'                                          *A QUINTALI
047400000721     C     WTIP          OREQ      '3'                                          *A KILOGRAMMI
047500070530     C                   Z-ADD     wpes          WSGL             13 3
047600000721     C                   ELSE
047700060809     C                   Z-ADD     tasNCL        WSGL
047800000721     C                   ENDIF
047900970418     C                   DO        ULTSGL        I
048000000721     C     WSGL          IFLE      SSGL(I)
048100970416     C                   Z-ADD     SSGL(I)       WFDSGL
048200970415     C                   LEAVE
048300970415     C                   ENDIF
048400970415     C                   ENDDO
048500970707     C     D50COM        IFEQ      *ZEROS
048600970707     C                   Z-ADD     *ZEROS        WFDPKG
048700970707     C                   Z-ADD     *ZEROS        WFDNCL
048800970707     C                   Z-ADD     *ZEROS        WFDVLB
048900970707     C                   Z-ADD     *ZEROS        WFDQFT
049000970707     C                   ELSE
049100070530     C                   Z-ADD     wpes          WFDPKG
049200070530     C                   Z-ADD     wvol          WFDVLB
049300070528     C                   Z-ADD     tasNCL        WFDNCL
049400060809     C                   Z-ADD     tasQFT        WFDQFT
049500970707     C                   ENDIF
049600970415     C                   Z-ADD     D50RIC        WFDRIC
049700970415     C                   Z-ADD     D50COM        WFDCOM
049800970415     C                   Z-ADD     D50DEL        WFDDEL
049900970415     C                   MOVEL     D50MSG        WFDERR
050000970513     C                   WRITE     WFDBF000
050100970415     C*
050200970415     C                   ENDSR
050300970418     C*------------------------------------------------------------------------*
050400970418     C* CERCTS - CERCA IL CTS DELLA TARIFFA
050500970418     C*------------------------------------------------------------------------*
050600970418     C     CERCTS        BEGSR
050700970418     C*
050800970418     C* CONTROLLA CON CODICE TASSAZIONE ELEMENTARE
050900970418     C                   MOVEL     *BLANKS       WCTSO
051000970418     C                   DO        ULTCTS        I
051100970418     C     WCTSI         IFEQ      CTS(I)
051200970418     C                   MOVEL     WCTSI         WCTSO
051300970418     C                   ENDIF
051400970418     C                   ENDDO
051500970418     C*
051600970418     C* SE NON TROVATO, CONTROLLA CON CODICE TASSAZIONE REGIONALE
051700970418IF  1C     WCTSO         IFEQ      *BLANKS
051800970418     C                   MOVEL     CODUT         KTBKUT
051900970418     C                   MOVE      'CT'          KTBCOD
052000970418     C                   MOVE      *BLANKS       KTBKEY
052100970418     C                   MOVEL     WCTSI         KTBKEY
052200970418     C     KEYTAB        CHAIN     TABEL00F                           99
052300970418IF  2C     *IN99         IFEQ      '1'
052400970418     C     TBLFLG        OREQ      '*'
052500970418     C                   MOVEL     'TE56'        D50ERR
052600011102     C                   MOVEL     MSG(1)        D50MSG                         *CTS NON TROV.TAB.CT
052700970418     C                   MOVEL     '1'           $ERR
052800970418X   2C                   ELSE
052900970418     C                   MOVEL     TBLUNI        DSCT
053000970418     C                   DO        ULTCTS        I
053100970418     C     §CTRAP        IFEQ      CTS(I)
053200970418     C                   MOVEL     §CTRAP        WCTSO
053300970418     C                   ENDIF
053400970418     C                   ENDDO
053500970418E   2C                   ENDIF
053600970418E   1C                   ENDIF
053700970418     C*
053800970418     C* SE NON TROVATO, CONTROLLA CON CODICE TASSAZIONE NAZIONALE
053900970418IF  1C     WCTSO         IFEQ      *BLANKS
054000970418     C     $ERR          OREQ      '1'
054100970418IF  2C     §CTNAR        IFEQ      *BLANKS
054200970418     C                   MOVEL     'IT'          WNAZ              2
054300970418X   2C                   ELSE
054400970418     C                   MOVEL     'EE'          WNAZ
054500970418E   2C                   ENDIF
054600970418     C                   DO        ULTCTS        I
054700970418     C     WNAZ          IFEQ      CTS(I)
054800970418     C                   MOVEL     WNAZ          WCTSO
054900970418     C                   ENDIF
055000970418     C                   ENDDO
055100970418E   1C                   ENDIF
055200970418     C*
055300970418     C                   ENDSR
055400970415     C*------------------------------------------------------------------------*
055500970415     C* CARSGL - CARICA GLI SCAGLIONI
055600970415     C*------------------------------------------------------------------------*
055700970415     C     CARSGL        BEGSR
055800011102     C*
055900011102     C* AZZERA VARIABILI DI LAVORO
056000011102     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO NO TROVAT
056100011102     C                   Z-ADD     *ZEROS        I                 4 0
056200011102     C                   Z-ADD     *ZEROS        C                 4 0
056300011102     C*---
056400130226     C* CERCA IL DETTAGLIO TARIFFARIO
056500130226     C                   Z-ADD     PARKsc        KTDKSC
056600130226     C                   Z-ADD     PARctr        KTDCTR
056700130205     C                   Z-ADD     parPRG        KTDPRG
056800130226      /free
056900130226        IF  parlnp = 0;
057000130226          setll (KTDksc:KTDctr:KTDprg) TITAD04L;
057100130226          reade (KTDksc:KTDctr:KTDprg) TITAD04L;
057200130226        ELSE;
057300130226          setll (KTDksc:KTDctr:KTDprg:parlnp) TITAD04L;
057400130226          reade (KTDksc:KTDctr:KTDprg:parlnp) TITAD04L;
057500130226        ENDIF;
057600130226        DOW  not %eof(TITAD04L);
057700130226      /end-free
057800011102     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
057900011102     C                   EXSR      MEMTAD                                       *MEMORIZZA DATI
058000130226      /free
058100130226        IF  parlnp = 0;
058200130226          reade (KTDksc:KTDctr:KTDprg) TITAD04L;
058300130226        ELSE;
058400130226          reade (KTDksc:KTDctr:KTDprg:parlnp) TITAD04L;
058500130226        ENDIF;
058600130226      /end-free
058700011102E   1C                   ENDDO                                                  *FINE DETTAGLIO
058800011102     C*
058900011102     C* ELEMENTI MEMORIZZATI
059000011102     C                   Z-ADD     I             ULTSGL            6 0
059100011102     C                   Z-ADD     C             ULTCTS            6 0
059200011102     C*
059300011102     C* DETTAGLIO NON TROVATO, ESCE DAL PGM
059400011102IF  1C     $TAD          IFEQ      '0'                                          *DETT.NON TROVATO
059500011102     C                   MOVEL     '1'           FINL00                         *FINE PGM
059600011102E   1C                   ENDIF
059700011102     C*
059800970415     C                   ENDSR
059900011102     C*------------------------------------------------------------------------*
060000011102     C* MEMORIZZA DETTAGLIO TARIFFARIO
060100011102     C*------------------------------------------------------------------------*
060200011102     C     MEMTAD        BEGSR
060300011102     C*
060400011102     C* SCAGLIONI
060500011102     C                   SETOFF                                           98
060600011102     C     TADSGL        LOOKUP    SSGL                                   98
060700011102IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
060800011102     C                   ADD       1             I
060900011102     C                   Z-ADD     TADSGL        SSGL(I)
061000011102E   1C                   ENDIF
061100011102     C* CODICI TASSAZIONE
061200011102     C                   SETOFF                                           98
061300011102     C     TADCTS        LOOKUP    CTS                                    98
061400011102IF  1C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
061500011102     C                   ADD       1             C
061600011102     C                   MOVEL     TADCTS        CTS(C)
061700011102E   1C                   ENDIF
061800011102     C*
061900011102     C                   ENDSR
062000011102     C*------------------------------------------------------------------------*
062100011102     C* CARICA LE TABELLE OCCORRENTI
062200011102     C*------------------------------------------------------------------------*
062300011102     C     CARTAB        BEGSR
062400020911     C*
062500020911     C* REPERISCE I DATI UTENTE
062600020911     C                   EXSR      REPDATIUTE
062700020301     C*
062800020301     C* CARICA FILIALI FEDEX
062900020301     C                   Z-ADD     *ZEROS        K                 3 0
063000020301     C     *LOVAL        SETLL     AZORG01L
063100020301     C                   READ      AZORG01L                               98
063200020301DO  1C     *IN98         DOWEQ     *OFF
063300020301IF  2C     ORGFVA        IFEQ      *BLANKS                                      *VALIDA
063400020301     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
063500020301     C* VERIFICO SE FILIALE FEDEX
063600020301     C                   MOVEL     ORGDE3        OG143
063700020301     C                   IF        §OGNTW = 'FED'
063800020301     C                   ADD       1             K
063900020301     C                   Z-ADD     ORGFIL        FILFED(K)
064000020301     C                   ENDIF
064100020301E   2C                   ENDIF
064200020301     C                   READ      AZORG01L                               98
064300020301E   1C                   ENDDO
064400011102     C*
064500011102     C                   ENDSR
064600020911     C*--------------------------------------------------------------------------------------------*
064700020911     C* REPERISCE I DATI UTENTE
064800020911     C*--------------------------------------------------------------------------------------------*
064900020911     C     REPDATIUTE    BEGSR
065000020911     C*
065100020911     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
065200020911     C                   CLEAR                   L1
065300020911     C                   CLEAR                   TIBS34DS
065400020911     C                   CLEAR                   AZUTEDS
065500020911     C                   CLEAR                   DDATIUTE
065600060809     C                   CLEAR                   WABI              2
065700060809     C                   CLEAR                   DLAT
065800060809     C                   CLEAR                   werr              1
065900060809     c                   movea     lnp           l1
066000020911     C*
066100020911     C     *DTAARA       DEFINE    §azute        azuteds
066200020911     C     *DTAARA       DEFINE    §datiute      ddatiute
066300020911     C                   IN(E)     *DTAARA
066400020911if  1C                   IF        %Error
066500020911     c                   EVAL      I34Tla = 'L'
066600020911     C                   CALL      'TIBS34R'
066700020911     C                   PARM                    Tibs34Ds
066800020911     C                   IN        *DTAARA
066900020911e   1C                   ENDIF
067000060809     C* carico la schiera dei po da elaborare solo se non richiesto nel
067100060809     c* lancio
067200060809     c                   if        (lnp(1) = 0 and parks1 <> 0) or
067300060809     c                             (lnp(2) = 0 and parks2 <> 0) or
067400060809     c                             (lnp(3) = 0 and parks3 <> 0) or
067500060809     c                             (lnp(4) = 0 and parks4 <> 0) or
067600060809     c                             (lnp(5) = 0 and parks5 <> 0)
067700020911     C*
067800060809     C                   SELECT
067900060809     C                   WHEN      DUTERR = 'E'
068000060809     C                   EVAL      WERR  = '1'
068100060809     C*
068200060809     C* NON siamo in sede (filiale remota o filiale locale)
068300060809     C* --> se 1° livello, abilitazioni al terminal
068400060809     C*     se 2° livello, abilitazioni al punto operativo
068500060809     C                   WHEN      UTEAUT = *blanks
068600060809     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
068700060809     C                   EVAL      WABI   = 'TP'
068800060809     C                   ENDIF
068900060809     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
069000060809     C                   EVAL      WABI   = 'PO'
069100060809     C                   ENDIF
069200060809     C*
069300060809     C* siamo in sede
069400060809     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
069500060809     C*     carica le abilitazioni proprie del profilo
069600060809     C                   OTHER
069700060809     C                   MOVEL     UTEFAF        DUTE01
069800060809     C                   IF        §UTEIST <> *blanks                           *esiste specifica
069900060809     C                   EVAL      WABI = §UTEIST
070000060809     C                   ELSE                                                   *NON esiste specific
070100060809     C                   EVAL      WABI = UTEAUT
070200060809     C                   ENDIF
070300060809     C                   ENDSL
070400060809     C*
070500060809     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
070600060809     C                   CLEAR                   TIBS02DS
070700060809     C                   MOVEL     'C'           T02MOD
070800060809     C                   MOVEL     KNSIF         T02SIF
070900060809     C                   MOVEL     'LAT'         T02COD
071000060809     C                   MOVEL(P)  WABI          T02KE1
071100060809     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
071200060809     C                   CALL      'TIBS02R'
071300060809     C                   PARM                    KPJBA
071400060809     C                   PARM                    TIBS02DS
071500060809     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
071600060809     C                   IF        T02ERR = *blanks
071700060809     C                   MOVEL     T02UNI        DLAT
071800060809     C                   ENDIF
071900060809     C                   IF        T02ERR <> *blanks OR
072000060809     C                             §LATABI = 'S'
072100060809     C                   EVAL      WERR  = '1'
072200060809     C                   ENDIF
072300060809     C*
072400060809     C* Reperisce i P.O. gestibili dall'utente
072500060809     C                   IF        WERR <> '1'                                  *NO errori
072600060809     C                   CLEAR                   TRUL31DS
072700060809     C                   EVAL      I31ABI   = WABI
072800060809     C                   EVAL      I31CDI   = DUTDIS
072900060809     C                   EVAL      I31CAR   = DUTARE
073000060809     C                   EVAL      I31CPO   = DUTPOU
073100060809     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
073200060809     C                   CALL      'TRUL31R'
073300060809     C                   PARM                    KPJBA
073400060809     C                   PARM                    TRUL31DS
073500060809     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
073600060809if  2C                   IF        O31POG > *zeros
073700060809     C                   MOVEA     s31           L1
073800060809x   2C                   ELSE
073900060809     C                   EVAL      WERR  = '1'
074000060809e   2C                   ENDIF
074100060809e   1C                   ENDIF
074200060809e   1C                   ENDIF
074300020911     C*
074400020911     C                   ENDSR
074500000000     C*------------------------------------------------------------------------*
074600000000     C* *INZSR - ROUTINE INIZIALE
074700000000     C*------------------------------------------------------------------------*
074800000000     C     *INZSR        BEGSR
074900000000     C*--------------------
075000000000     C* RICEVIMENTO PARAMETRI
075100000000     C*--------------------
075200000000     C     *ENTRY        PLIST
075300000000     C                   PARM                    KPJBA
075400950703     C                   MOVEL     KPJBU         PARAM
075500000000     C*--------------------
075600970418     C* VARIABILI RIFERITE AL DATA BASE
075700000000     C*--------------------
075800970418     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
075900970418     C     *LIKE         DEFINE    TBLCOD        KTBCOD
076000970418     C     *LIKE         DEFINE    TBLKEY        KTBKEY
076100060809     C     *LIKE         DEFINE    tasLNP        KBPLNP
076200060809     C     *LIKE         DEFINE    tasAAS        KBLAAS
076300060809     C     *LIKE         DEFINE    tasMGS        KBLMGS
076400060809     C     *LIKE         DEFINE    TAMKSC        KTMKSC
076500970415     C     *LIKE         DEFINE    TAMCTR        KTMCTR
076600130205     C     *LIKE         DEFINE    TAMPRG        KTMprg
076700060809     C     *LIKE         DEFINE    TADKSC        KTDKSC
076800970415     C     *LIKE         DEFINE    TADCTR        KTDCTR
076900970415     C     *LIKE         DEFINE    TADPRG        KTDPRG
077000060809     C     *LIKE         DEFINE    TADCTS        WCTSI
077100970418     C     *LIKE         DEFINE    TADCTS        WCTSO
077200011102     C     *LIKE         DEFINE    TAMKSC        CARKSC
077300011102     C     *LIKE         DEFINE    TAMCTR        CARCTR
077400011102     C     *LIKE         DEFINE    TAMPRG        CARPRG
077500060809     C     *LIKE         DEFINE    KPJBU         DEPKPJBU
077600000000     C*--------------------
077700000000     C* CHIAVI DI LETTURA
077800000000     C*--------------------
077900060809     C* CHIAVE LETTURA titas39c - PARZIALE
078000060809     C     KEYtas        KLIST
078100020911     C                   KFLD                    KBPLNP                         *LINEA DI PARTENZA
078200970414     C                   KFLD                    KBLAAS
078300970414     C                   KFLD                    KBLMGS
078400970415     C* LETTURA TNTAM01L - PARZIALE
078500970415     C     KEYTAM        KLIST
078600970415     C                   KFLD                    KTMKSC                         *CLIENTE
078700970415     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
078800130205     C                   KFLD                    KTMprg                         *PROGR. TARIFFA
078900011102     C* LETTURA TITAD04L - PARZIAL
079000970415     C     KEYTAD        KLIST
079100970415     C                   KFLD                    KTDKSC                         *CLIENTE
079200970415     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
079300970415     C                   KFLD                    KTDPRG                         *PROGRESSIVO
079400970418     C* LETTURA TABEL00F - COMPLETA
079500970418     C     KEYTAB        KLIST
079600970418     C                   KFLD                    KTBKUT                         *UTENTE
079700970418     C                   KFLD                    KTBCOD                         *TABELLA
079800970418     C                   KFLD                    KTBKEY                         *CODICE
079900000000     C*--------------------
080000970418     C* VARIABILI DI CONTROLLO
080100000000     C*--------------------
080200000000     C                   Z-ADD     1             CODUT             1 0
080300970416     C                   MOVEL     '0'           FINL00            1
080400970416     C                   MOVEL     '0'           $ERR              1
080500970414     C*
080600970418     C* IMPOSTA LE DATE DI LANCIO
080700970415     C                   Z-ADD     PARDS1        DSDAT
080800970415     C                   Z-ADD     DSMGS         WMGSI             4 0
080900970415     C                   Z-ADD     DSAAS         WAASI             4 0
081000970415     C                   Z-ADD     PARDS2        DSDAT
081100970415     C                   Z-ADD     DSMGS         WMGSF             4 0
081200970415     C                   Z-ADD     DSAAS         WAASF             4 0
081300130226
081400130226      /free
081500130226       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
081600130226         wOggi = %dec(%date());
081700130226      /end-free
081800130226
081900011102     C*
082000011102     C* CARICA TABELLE OCCORRENTI
082100011102     C                   EXSR      CARTAB
082200970415     C*
082300970415     C* CARICA GLI SCAGLIONI DELLA TARIFFA
082400970415     C                   EXSR      CARSGL
082500000000     C*
082600970415     C                   ENDSR
082700970415     O*------------------------------------------------------------------------*
082800011102** MSG - MESSAGGI
082900011102TNTE56R- Cpdice tassazione non trovato in tabella "CT".                        1
083000011102TNTE56R- 2                                                                     2
083100970415TNTE56R- 3                                                                     3
083200970415TNTE56R- 4                                                                     4
083300970415TNTE56R- 5                                                                     5
083400970415TNTE56R- 6                                                                     6
083500970415TNTE56R- 7                                                                     7
083600970415TNTE56R- 8                                                                     8
083700970415TNTE56R- 9                                                                     9
083800970415TNTE56R- 10                                                                   10
083900970415TNTE56R- 11                                                                   11
084000970415TNTE56R- 12                                                                   12
084100970415TNTE56R- 13                                                                   13
084200970415TNTE56R- 14                                                                   14
084300970415TNTE56R- 15                                                                   15
084400970415TNTE56R- 16                                                                   16
084500970415TNTE56R- 17                                                                   17
084600970415TNTE56R- 18                                                                   18
084700970415TNTE56R- 19                                                                   19
084800970415TNTE56R- 20                                                                   20
