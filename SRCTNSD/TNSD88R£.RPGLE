000100961206     H*------------------------------------------------------------------------*
000200961206     H* STATISTICA CONSEGNE RICHIESTE - ESEGUE                                 *
000300011109     H* RICHIAMATO A VIDEO E DAL PGM DI INIZIO LAVORI IN FILIALE -TNSY00C-     *
000400961206     H*------------------------------------------------------------------------*
000500961206     H DECEDIT('0,') DATEDIT(*DMY.)
000600961206     F*------------------------------------------------------------------------*
000700961206     F* DATA BASE
000800961206     F*------------------------------------------------------------------------*
000900961206     F*-------------------
001000961206     F* BOLLE ARRIVO --> LNA+AAS+MGS
001100961206     F*-------------------
001200961206     FFNARB26L  IF   E           K DISK
001300961206     F*-------------------
001400961206     F* ESTENSIONE C/A BOLLE ARRIVO --> AAS+LNP+NRS+NSP
001500961206     F*-------------------
001600051115     FFiAR901L  IF   E           K DISK
001700961206     F*-------------------
001800961206     F* VARIAZIONI BOLLE ARRIVO --> AAS+LNP+NRS+NSP+DTV+ORV
001900961206     F*-------------------
002000131014     FFIARBF2C  IF   E           K DISK
002100961206     F                                     IGNORE(FNARBK00)
002200991124     F                                     IGNORE(FIARBT00)
002300961206     F                                     IGNORE(FNARBM00)
002400961206     F                                     IGNORE(FNARBV00)
002500131014     F                                     IGNORE(FNARBN00)
002600961206     F*-------------------
002700961206     F* EVENTI BOLLE --> AAS(2)+LNP+NRS+NSP+CEV+DEV+HEV
002800961206     F*-------------------
002900990810     FFNEVB01L  IF   E           K DISK
003000961206     F*-------------------
003100961206     F* GIACENZE ARRIVO --> AAS+LNP+NRS+NSP
003200961206     F*-------------------
003300050310     FTIGCP01L  IF   E           K DISK
003400961206     F*-------------------
003500961206     F* FOGLI VARI --> NPG+NFV+FGS
003600961206     F*-------------------
003700961206     FFNFVV01L  IF   E           K DISK
003800961206     FTABEL00F  IF   E           K DISK
003900961206     FAZORG01L  IF   E           K DISK
004000010124     FTNSD88P   O    E             PRINTER OFLIND(*IN01)
004100010124     F                                     USROPN
004200020910     FTNTBE01L  IF   E           K DISK
004300961206     D*------------------------------------------------------------------------*
004400961206     D* SCHIERE
004500961206     D*------------------------------------------------------------------------*
004600961206     D*-------------------
004700961206     D* DECODIFICHE IN STAMPA
004800961206     D*-------------------
004900991124     D DEC             S             20    DIM(5) CTDATA PERRCD(1)
005000961206     D*-------------------
005100961206     D* FILIALI DA ELABORARE
005200961206     D*-------------------
005300961206     D LNA             S              3  0 DIM(30)                              CODICI
005400961206     D DLNA            S             20    DIM(30)                              DESCRIZIONI
005500971204     D LFL1            S              1    DIM(30)                              FLAG ITALIA/ESTERO
005600961206     D*-------------------
005700961206     D* CAUSALE EVENTI
005800961206     D*-------------------
005900080903     D CEV             S              3    DIM(200)                             CODICI
006000080903     D TEV             S              1    DIM(200)                             TIPI EVENTO (GIAC,LA
006100971002     D*-------------------
006200971002     D* PARTICOLARITA' GIACENZE
006300971002     D*-------------------
006400971002     D GGA             S              2    DIM(100)                             *CODICI
006500971002     D G2MF            S              1    DIM(100)                             *FASE INVIO MESSAGGI
006600971002     D G2MQ            S              1    DIM(100)                             *QUANDO INVIO
006700971002     D G5DI            S              1    DIM(100)                             *CODICE DISPOSIZIONE
006800971002     D G4D1            S              1    DIM(100)                             *CODICE DISPOSIZIONE
006900971002     D G4D2            S              1    DIM(100)                             *CODICE DISPOSIZIONE
007000971002     D G4D3            S              1    DIM(100)                             *CODICE DISPOSIZIONE
007100971002     D G4D4            S              1    DIM(100)                             *CODICE DISPOSIZIONE
007200971002     D G4D5            S              1    DIM(100)                             *CODICE DISPOSIZIONE
007300961206     D*-------------------
007400961206     D* TEMPI FISSI ARRIVO TRAZIONE
007500961206     D*-------------------
007600080513     d c_dim_1X        c                   const(500)
007700080513     D TRA             S              3  0 DIM(c_dim_1X)                        *TRAZIONI
007800080513     D OSA             S              4  0 DIM(c_dim_1X)                        *1^ SOGLIA ARRIVO TR
007900080513     D OSA1            S              4  0 DIM(c_dim_1X)                        *2^ SOGLIA ARRIVO TR
008000970902     D*-------------------
008100970902     D* CAUSALI STAMPA DETTAGLIO BOLLE
008200970902     D*-------------------
008300970902     D CSTM            S              1    DIM(20)
008400970916     D*-------------------
008500970916     D* DI LAVORO
008600970916     D*-------------------
008700970916     D SA1             S              1    DIM(20)                              *SCHIERA ALFANUMERIC
008800010124     D*-------------------
008900010124     D* COMANDI
009000010124     D*-------------------
009100010124     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
009200961206     D*------------------
009300961206     D* ARCHITETTURA
009400961206     D*------------------
009500961206     D KPJBA         E DS
009600961206     D*------------------
009700961206     D* DS "TRUL06R" - CARICAMENTO TABELLE "£X"
009800961206     D*------------------
009900961206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
010000961206     D  LIN                    1     90  0
010100961206     D                                     DIM(30)                              DI COMODO
010200961206     D*------------------
010300971009     D* DS "TNSD99R" - CAOCLO TEMPI DI TRAZIONI INTERNI
010400961206     D*------------------
010500971009     D DS99          E DS                  EXTNAME(TNSD99DS)
010600961206     D*------------------
010700961206     D* DS CAUSALI EVENTI
010800961206     D*------------------
010900961206     D DS2A          E DS
011000971002     D*------------------
011100971002     D* DS PARTICOLARITA' GIACENZE
011200971002     D*------------------
011300971002     D DS7Q          E DS
011400961206     D*------------------
011500961206     D* DS TEMPI FISSI DI DELIVERY
011600961206     D*------------------
011700961206     D DS1X          E DS
011800961206     D*------------------
011900961206     D* DS TIPO INCASSO C/ASSEGNO
012000961206     D*------------------
012100961206     D DS1A          E DS
012200961206     D*------------------
012300961206     D* DS DIVISE E CAMBI
012400961206     D*------------------
012500961206     D DSCV          E DS
012600130828     D*------------------
012700130828     D* DS Codici bolla
012800130828     D*------------------
012900130828     d ds3a          e ds
013000020717
013100020717     d DdatiUte      e ds
013200020717     d AzuteDs       e ds                  ExtName(Azute00f)
013300020717     d Tibs34Ds      e ds                  Inz
013400020717
013401131128     d devb01        E ds
013402131128
013500961206     D*------------------
013600961206     D* DS SOGLIA DELIVERY
013700961206     D*------------------
013800961206     D                 DS
013900961206     D  TABFIL                 1      3                                         FILIAL
014000961206     D  TABOSA                 4      6                                         "OSA"
014100961206     D  TABPOS                 7      7                                         ' '/1
014200961206     D  TABKEY                 1      7
014300961206     D*------------------
014400961206     D* DS CONTROLLA DATA (8)
014500961206     D*------------------
014600961206     D WLBDA8          DS                  INZ
014700961206     D  G08DAT                 1      8  0
014800961206     D  G08INV                 9     16  0
014900961206     D  G08ERR                17     17
015000961206     D  G08TGI                18     22  0
015100010124     D*-------------------
015200010124     D* RICALCOLA DATA (8)
015300010124     D*-------------------
015400010124     D WGIDA8          DS
015500010124     D  GI8DAT                 1      8  0
015600010124     D  GI8INV                 9     16  0
015700010124     D  GI8TGI                17     21  0
015800961206     D*------------------
015900961206     D* DS DATA SPEDIZIONE FNARB00F
016000961206     D*------------------
016100961206     D                 DS
016200961206     D  ARBAAS                 1      4  0
016300961206     D  ARBMGS                 5      8  0
016400961206     D  ARBDSP                 1      8  0
016500961206     D*------------------
016600961206     D* PARAMETRI IN INPUT
016700961206     D*------------------
016800010125     D PARAM           DS                  INZ
016900970916     D  PARLNA                 1      3  0
017000970916     D  PARDTI                 4     11  0
017100970916     D  PARAAI                 4      7  0
017200970916     D  PARMGI                 8     11  0
017300970916     D  PARDTF                12     19  0
017400970916     D  PARAAF                12     15  0
017500970916     D  PARMGF                16     19  0
017600970916     D  PARDET                20     20
017700970916     D  PARCD1                21     21
017800970916     D  PARCD2                22     22
017900970916     D  PARCD3                23     23
018000970916     D  PARCD4                24     24
018100970916     D  PARCD5                25     25
018200970916     D  PARCD6                26     26
018300970916     D  PARCD7                27     27
018400970916     D  PARCD8                28     28
018500970916     D  PARCD9                29     29
018600970916     D  PARCD0                30     30
018700970916     D  PARCDA                31     31
018800970916     D  PARCDB                32     32
018900970916     D  PARCDC                33     33
019000970916     D  PARCDD                34     34
019100970916     D  PARCDE                35     35
019200970916     D  PARCDF                36     36
019300970916     D  PARCDG                37     37
019400970916     D  PARCDH                38     38
019500970916     D  PARCDI                39     39
019600970916     D  PARCDL                40     40
019700971003     D  PARCAU                21     40
019800010822     D*------------------
019900010822     D* DS CAMBIO DIVISA
020000010822     D*------------------
020100011211     D*YEURDS        E DS                  EXTNAME(YEURCODS)
020200010822     D*------------------
020300010822     D* DS REPERIMENTO GESTIONE IMPORTI VARI
020400011211     D*------------------
020500011211     D* DGEI          E DS
020600011211     D*------------------
020700011211     D* DS REPERIMENTO IMPORTO MASSIMO CONTRASSEGNO
020800011211     D*------------------
020900011211     D TRUL21DS      E DS
021000020627     D*------------------
021100020627     D* DESCRIZIONE ORGANIGRAMMA
021200020627     D*------------------
021300020627     D OG143         E DS
021400020702     D*------------------
021500020702     D* DS PER LETTURA TABELLA "NTW"
021600020702     D*------------------
021700020702     D DNTW          E DS
021800961206     I*------------------------------------------------------------------------*
021900961206     I* INPUT
022000961206     I*------------------------------------------------------------------------*
022100961206     I*------------------
022200961206     I* RIDENOMINA I CAMPI DELL'ARCHIVIO VARIAZIONI
022300961206     I*------------------
022400961206     IFNARBD00      10
022500961206     I              ARBATB                      ARDATB
022600961206     I              ARBAAS                      ARDAAS
022700961206     I              ARBLNP                      ARDLNP
022800961206     I              ARBNRS                      ARDNRS
022900961206     I              ARBNSP                      ARDNSP
023000961206     I              ARBCBO                      ARDCBO
023100961206     I              ARBDTV                      ARDDTV
023200961206     I              ARBORV                      ARDORV
023300961206     I              ARBCVB                      ARDCVB
023400961206     I              ARBPRU                      ARDPRU
023500961206     I              ARBRSD                      ARDRSD
023600961206     I              ARBRD2                      ARDRD2
023700961206     I              ARBIND                      ARDIND
023800961206     I              ARBCAD                      ARDCAD
023900961206     I              ARBLOD                      ARDLOD
024000961206     I              ARBPRD                      ARDPRD
024100961206     I              ARBNZD                      ARDNZD
024200961206     I              ARBFIN                      ARDFIN
024300961206     I              ARBCPI                      ARDCPI
024400961206     I              ARBPKB                      ARDPKB
024500961206     I              ARBPKF                      ARDPKF
024600961206     I              ARBVLB                      ARDVLB
024700961206     I              ARBFVB                      ARDFVB
024800961206     I              ARBVLF                      ARDVLF
024900961206     I              ARBFVF                      ARDFVF
025000961206     I              ARBFFD                      ARDFFD
025100961206     I              ARBFST                      ARDFST
025200961206     IFNARBG00      20
025300961206     I              ARBATB                      ARGATB
025400961206     I              ARBAAS                      ARGAAS
025500961206     I              ARBLNP                      ARGLNP
025600961206     I              ARBNRS                      ARGNRS
025700961206     I              ARBNSP                      ARGNSP
025800961206     I              ARBCBO                      ARGCBO
025900961206     I              ARBDTV                      ARGDTV
026000961206     I              ARBORV                      ARGORV
026100961206     I              ARBCVB                      ARGCVB
026200961206     I              ARBPRU                      ARGPRU
026300961206     I              ARBTCR                      ARGTCR
026400961206     I              ARBDCR                      ARGDCR
026500961206     I              ARBHCR                      ARGHCR
026600961206     I              ARBGC1                      ARGGC1
026700961206     I              ARBGC2                      ARGGC2
026800961206     I              ARBTC1                      ARGTC1
026900961206     I              ARBTC2                      ARGTC2
027000961206     I              ARBFST                      ARGFST
027100961206     C*------------------------------------------------------------------------*
027200961206     C* MAIN LINE
027300961206     C*------------------------------------------------------------------------*
027400010124     C*
027500010124     C* APRO IL FILE DI STAMPA
027600010124     C                   OPEN      TNSD88P
027700961206     C*
027800961206     C* STAMPA PARAMETRI
027900961206     C                   EXSR      STAPAR
028000961206     C*
028100961206     C* PER CIASCUNA FILIALE CARICATA DA ELABORARE
028200970916     C                   Z-ADD     1             F                 4 0
028300970916DO  1C     F             DOWLE     30
028400970916     C     LNA(F)        ANDGT     *ZEROS
028500961206     C*
028600961206     C* OPERAZIONI PER NUOVA LINEA DI ARRIVO
028700961206     C                   EXSR      NEWLNA
028800961206     C*
028900961206     C* POSIZIONAMENTO E LETTURA BOLLE ARRIVO
029000961206     C                   EXSR      SETFIL
029100961206     C*
029200961206     C* CICLO FINO A ROTTURA LINEA DI ARRIVO
029300961206DO  2C     $FINE         DOWEQ     '0'
029400961206     C*
029500961206     C* MEMORIZZA I TOTALI PER FILIALE
029600961206     C                   EXSR      MEMARB
029700961206     C*
029800961206     C* SE RICHIESTO DETTAGLIO, STAMPA I DATI DELLA BOLLA
029900961206IF  3C     PARDET        IFEQ      'S'
030000961206     C                   EXSR      STADET
030100961206E   3C                   ENDIF
030200961206     C*
030300961206     C* LETTURA SUCESSIVA BOLLE ARRIVO
030400961206     C                   EXSR      LETFIL
030500961206E   2C                   ENDDO
030600961206     C*
030700961206     C* SE MEMORIZZATO QC, STAMPA I TOTALI PER FILIALE
030800961206IF  2C     $MEMF         IFEQ      '1'
030900961206     C                   EXSR      STATOF
031000961206E   2C                   ENDIF
031100961206     C*
031200961206     C* LETTURA SUCESSIVA FILIALI CARICATE
031300970916     C                   ADD       1             F
031400961206E   1C                   ENDDO
031500961206     C*
031600961206     C* SE MEMORIZZATO QC, STAMPA I TOTALI GENERALI
031700961206IF  1C     $MEMG         IFEQ      '1'
031800961206     C                   EXSR      STATOG
031900961206E   1C                   ENDIF
032000961206     C*
032100961206     C                   WRITE     SD88FI                                       *FINE PROSPETTO
032200961206     C*
032300961206     C* CHIUDE I FILES DEI LANCI PGM
032400961206     C                   EXSR      CHIUDE
032500010124     C*
032600010124     C* CHIUDO IL FILE DI STAMPA
032700010124     C                   CLOSE     TNSD88P
032800010124     C*
032900010124     C* ELIMINO TUTTE LE SOVRAPPOSIZIONI EFFETUATE IN PRECEDENZA
033000010126     C                   IF        TIPLAN = '1'
033100010124     C                   Z-ADD     80            LENGH            15 5
033200010125     C                   CALL(e)   'QCMDEXC'
033300010124     C                   PARM                    CMD(2)
033400010124     C                   PARM                    LENGH
033500010126     C                   ENDIF
033600011211     C*
033700961206     C                   SETON                                            LR
033800961206     C*------------------------------------------------------------------------*
033900961206     C* CHIUDE - CHIUDE I FILES DEI LANCI PGM
034000961206     C*------------------------------------------------------------------------*
034100961206     C     CHIUDE        BEGSR
034200961206     C* TRUL06R
034300961206     C                   MOVEL     'C'           D06TLA
034400961206     C                   MOVEL     DSUL06        KPJBU
034500961206     C                   CALL      'TRUL06R'
034600961206     C                   PARM                    KPJBA
034700971009     C* TNSD99R
034800971009IF  1C     WSD99         IFEQ      '1'
034900961206     C                   MOVEL     'C'           D98TLA
035000971009     C                   CALL      'TNSD99R'
035100971009     C                   PARM                    DS99
035200961206E   1C                   ENDIF
035300011211     C*
035400011211     C                   CLEAR                   Trul21DS
035500011211     C                   EVAL      I21Tla = 'C'
035600011211     C                   CALL      'TRUL21R'
035700011211     C                   PARM                    Trul21DS
035800961206     C*
035900961206     C                   ENDSR
036000961206     C*------------------------------------------------------------------------*
036100961206     C* NEWLNA - OPERAZIONI PER NUOVA LINEA DI ARRIVO
036200961206     C*------------------------------------------------------------------------*
036300961206     C     NEWLNA        BEGSR
036400961206     C*
036500961206     C     *LIKE         DEFINE    ARBLNA        DEPLNA
036600971204     C     *LIKE         DEFINE    ORGFL1        DEPFL1
036700961206     C     *LIKE         DEFINE    ARBHTI        DEPOSA
036800961206     C     *LIKE         DEFINE    ARBHTI        DEPOS1
036900961206     C     *LIKE         DEFINE    ORGDES        DECLNA
037000961206     C*
037100961206     C* REIMPOSTA I FLAGS
037200961206     C                   MOVEL     '0'           $FINE                          *FINE CICLO
037300961206     C                   MOVEL     '0'           $MEMF                          *MEMORIZZATO QC
037400961206     C                   MOVEL     '1'           *IN01                          *OVERFLOW
037500961206     C*
037600961206     C* MEMORIZZA LA FILIALE IN UN DEPOSITO
037700970916     C                   Z-ADD     LNA(F)        DEPLNA
037800971204     C*
037900971204     C* DATI FILIALE
038000971204     C                   MOVEL     LFL1(F)       DEPFL1
038100971204     C                   MOVEL     DLNA(F)       DECLNA
038200961206     C*
038300961206     C* REPERISCE LE SOGLIE DI ARRIVO TRAZIONE
038400961206     C                   Z-ADD     1             J
038500961206     C     DEPLNA        LOOKUP    TRA(J)                                 28     *SOGLIE ARRIVO TRAZ
038600961206     C                   Z-ADD     OSA(J)        DEPOSA                          *1^ SOGLIA
038700961206     C                   Z-ADD     OSA1(J)       DEPOS1                          *2^ SOGLIA
038800961206     C*
038900961206     C* AZZERA I TOTALI DI FILIALE
039000961206     C                   Z-ADD     *ZEROS        FILTBL            7 0          *TOT. BOLLE
039100961206     C                   Z-ADD     *ZEROS        FILTCR            7 0          *TOT. CONS. RICHIEST
039200961206     C                   Z-ADD     *ZEROS        FILLAV            7 0            -LASCIATO AVVISO
039300961206     C                   Z-ADD     *ZEROS        FILLEG            7 0            -L.AVVISO+GIACENZA
039400961206     C                   Z-ADD     *ZEROS        FILGCA            7 0            -GIACENZA
039500961206     C                   Z-ADD     *ZEROS        FILFFD            7 0            -FERMO DEPOSITO
039600961206     C                   Z-ADD     *ZEROS        FILSMK            7 0            -SUPERMARKET
039700961206     C                   Z-ADD     *ZEROS        FILAPP            7 0            -APPUNTAMENTO
039800961206     C                   Z-ADD     *ZEROS        FILRIT            7 0            -ARRIVO RITARDO 1^
039900961206     C                   Z-ADD     *ZEROS        FILRI2            7 0            -ARRIVO RITARDO 2^
040000961206     C                   Z-ADD     *ZEROS        FILCAS            7 0            -CONTRASSEGNO ABIL
040100961206     C                   Z-ADD     *ZEROS        FILTCT            7 0          *TOT. CONS.RICH.TOT.
040200961206     C                   Z-ADD     *ZEROS        FILTFD            7 0          *TOT. FERMI DEPOSITI
040300961206     C                   Z-ADD     *ZEROS        FILTIT            7 0          *TOT. ARRIVI RIT. 1^
040400961206     C                   Z-ADD     *ZEROS        FILTI2            7 0          *TOT. ARRIVI RIT. 2^
040500961206     C                   Z-ADD     *ZEROS        FILTTC            7 0          *TOT. TURNI CHIUSURA
040600961206     C                   Z-ADD     *ZEROS        FILTPT            7 0          *TOT. CONS. PRIMA 13
040700961206     C                   Z-ADD     *ZEROS        FILTPM            7 0          *TOT. CONS. X POMERI
040800961206     C                   Z-ADD     *ZEROS        FILTDT            7 0          *TOT. CONS. DOPO 13
040900961206     C                   Z-ADD     *ZEROS        FILTEG            7 0          *TOT. EVEN.SENZA GIA
041000970902     C                   Z-ADD     *ZEROS        FILTPG            7 0          *TOT. + EVENTI GIAC
041100961206     C*
041200961206     C                   ENDSR
041300961206     C*------------------------------------------------------------------------*
041400961206     C* SETFIL - POSIZIONAMENTO E PRIMA LETTRURA RECORD
041500961206     C*------------------------------------------------------------------------*
041600961206     C     SETFIL        BEGSR
041700961206     C*
041800961206     C                   Z-ADD     DEPLNA        KARLNA                         *LINEA DI ARRIVO
041900961206     C                   Z-ADD     PARAAI        KARAAS                         *ANNO INIZIALE
042000961206     C                   Z-ADD     PARMGI        KARMGS                         *MESE INIZIALE
042100961206     C*
042200961206     C     KEYARB        SETLL     FNARB26L                           99        *FINE ARCHIVIO
042300961206     C*
042400961206IF  1C     *IN99         IFEQ      *ON
042500961206     C                   MOVEL     '1'           $FINE
042600961206X   1C                   ELSE
042700961206     C                   EXSR      LETFIL                                       *LETTURA RECORD
042800961206E   1C                   ENDIF
042900961206     C*
043000961206     C                   ENDSR
043100961206     C*------------------------------------------------------------------------*
043200961206     C* LETFIL - LETTURA PROSSIMO RECORD
043300961206     C*------------------------------------------------------------------------*
043400961206     C     LETFIL        BEGSR
043500961206     C*
043600961206     C* LEGGO FINO A:
043700961206     C*     - FINE FILE
043800961206     C*     - TROVATO RECORD VALIDO
043900961206     C                   MOVEL     'N'           WRECOK
044000961206DO  1C     $FINE         DOWEQ     '0'
044100961206     C     WRECOK        ANDEQ     'N'
044200961206     C*
044300961206     C                   READ      FNARB26L                               99
044400961206     C*
044500961206IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
044600961206     C                   MOVEL     '1'           $FINE
044700961206X   2C                   ELSE
044800961206     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
044900961206E   2C                   ENDIF
045000961206     C*
045100961206E   1C                   ENDDO
045200961206     C*
045300961206     C                   ENDSR
045400961206     C*------------------------------------------------------------------------*
045500961206     C* CHKREC - CONTROLLA VALIDITA' RECORD
045600961206     C*------------------------------------------------------------------------*
045700961206     C     CHKREC        BEGSR
045800961206     C*
045900961206     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
046000961206     C*
046100961206     C* SE LINEA ARRIVO <>  O P P U R E  DATA SPEDIZIONE > RICHIESTA, ESCE
046200961206IF  1C     ARBLNA        IFNE      DEPLNA
046300961206     C     ARBDSP        ORGT      PARDTF
046400961206     C                   MOVEL     '1'           $FINE                          *FINE LETTURA BOLLE
046500961206E   1C                   ENDIF
046600961206     C*
046700961206     C* BOLLA NON ANCORA CONSEGNATA, ESCLUDE
046800961206IF  1C     ARBDCM        IFEQ      *ZEROS
046900961206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047000961206E   1C                   ENDIF
047100961206     C*
047200961206     C                   ENDSR
047300961206     C*------------------------------------------------------------------------*
047400961206     C* MEMARB - MEMORIZZA I DATI DELLA BOLLA
047500961206     C*------------------------------------------------------------------------*
047600961206     C     MEMARB        BEGSR
047700961206     C*
047800961206     C* IMPOSTA I FLAG DI MEMORIZZAZIONE BOLLE
047900961206     C                   MOVEL     '1'           $MEMF                          *MEMOR. QC FILIALE
048000961206     C                   MOVEL     '1'           $MEMG                          *MEMOR. QC GENERALE
048100961206     C                   MOVEL     '0'           $CR                            *FIND CONS.RICHIESTE
048200961206     C                   MOVEL     '0'           $LG                            *FIND L.AVVISO/GIACE
048300961206     C                   MOVEL     '0'           $FD                            *FIND FERMO DEPOSITO
048400961206     C                   MOVEL     '0'           $TC                            *FIND TURNO CHIUSURA
048500961206     C                   MOVEL     '0'           $GC                            *FIND GIACENZA
048600961206     C                   MOVEL     '0'           $EG                            *FIND EVENTO GIACENZ
048700970115     C                   MOVEL     '0'           $PG                            *FIND + EVENTI GIAC.
048800970902     C*
048900970902     C* IMPOSTA I FLAG DI STAMPA BOLLE
049000970902     C                   MOVEL     '0'           §TCR                           *TOT. CONS. RICHIEST
049100970901     C                   MOVEL     '0'           §LAV                             -LASCIATO AVVISO
049200970901     C                   MOVEL     '0'           §LEG                             -L.AVVISO+GIACENZA
049300970901     C                   MOVEL     '0'           §GCA                             -GIACENZA
049400970901     C                   MOVEL     '0'           §FFD                             -FERMO DEPOSITO
049500970901     C                   MOVEL     '0'           §SMK                             -SUPERMARKET
049600970901     C                   MOVEL     '0'           §APP                             -APPUNTAMENTO
049700970901     C                   MOVEL     '0'           §RIT                             -ARRIVO RITARDO 1^
049800970901     C                   MOVEL     '0'           §RI2                             -ARRIVO RITARDO 2^
049900970901     C                   MOVEL     '0'           §CAS                             -CONTRASSEGNO ABIL
050000970901     C                   MOVEL     '0'           §TCT                           *TOT. CONS.RICH.TOT.
050100970901     C                   MOVEL     '0'           §TFD                           *TOT. FERMI DEPOSITI
050200970901     C                   MOVEL     '0'           §TIT                           *TOT. ARRIVI RIT. 1^
050300970901     C                   MOVEL     '0'           §TI2                           *TOT. ARRIVI RIT. 2^
050400970901     C                   MOVEL     '0'           §TTC                           *TOT. TURNI CHIUSURA
050500970901     C                   MOVEL     '0'           §TPT                           *TOT. CONS. PRIMA 13
050600970901     C                   MOVEL     '0'           §TPM                           *TOT. CONS. X POMERI
050700970901     C                   MOVEL     '0'           §TDT                           *TOT. CONS. DOPO 13
050800970901     C                   MOVEL     '0'           §TEG                           *TOT. EVEN.SENZA GIA
050900970902     C                   MOVEL     '0'           §TPG                           *TOT. + EVENTI GIAC
051000961206     C*
051100961206     C* PULISCO CAMPI ESTENSIONE BOLLE PER LA STAMPA
051200961206     C                   MOVEL     *BLANKS       AR9TIC                         *TIPO INCASSO C/A
051300961206     C                   MOVEL     *BLANKS       AR9VCA                         *DIVISA C/A
051400961206     C                   Z-ADD     *ZEROS        AR9CAS                         *C/ASSEGNO
051500961206     C*
051600961206     C* VALORIZZA LE CHIAVI DI LETTURA
051700051115     C                   Z-ADD     ARBAAS        KA9AAS                         *FiAR900F
051800961206     C                   Z-ADD     ARBLNP        KA9LNP
051900961206     C                   Z-ADD     ARBNRS        KA9NRS
052000961206     C                   Z-ADD     ARBNSP        KA9NSP
052100961206     C*
052200961206     C                   Z-ADD     ARBAAS        KAGAAS                         *FNARBG0F
052300961206     C                   Z-ADD     ARBLNP        KAGLNP
052400961206     C                   Z-ADD     ARBNRS        KAGNRS
052500961206     C                   Z-ADD     ARBNSP        KAGNSP
052600961206     C*
052700961206     C                   Z-ADD     ARBAAS        KADAAS                         *FNARBD0F
052800961206     C                   Z-ADD     ARBLNP        KADLNP
052900961206     C                   Z-ADD     ARBNRS        KADNRS
053000961206     C                   Z-ADD     ARBNSP        KADNSP
053100961206     C*
053200990810     C                   Z-ADD     ARBAAS        KEVAAS                         *FNEVB01L
053300961206     C                   Z-ADD     ARBLNP        KEVLNP
053400961206     C                   Z-ADD     ARBNRS        KEVNRS
053500961206     C                   Z-ADD     ARBNSP        KEVNSP
053600961206     C*
053700050310     C                   Z-ADD     ARBAAS        KGCAAS                         *TIGCP01L
053800961206     C                   Z-ADD     ARBLNP        KGCLNP
053900961206     C                   Z-ADD     ARBNRS        KGCNRS
054000961206     C                   Z-ADD     ARBNSP        KGCNSP
054100961206     C*
054200961206     C                   Z-ADD     4             KFVNPG                         *FNFVV01L
054300961206     C                   Z-ADD     ARBNDC        KFVNFV
054400020912     c                   z-add     ArbIfp        Kfvfgs
054500961206     C***
054600961206     C* TOTALE BOLLE
054700961206     C***
054800961206     C                   ADD       1             FILTBL                         *TOT. BOLLE
054900971204IF  1C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
055000961206     C                   ADD       1             GENTBL
055100961206E   1C                   ENDIF
055200961206     C***
055300961206     C* TOTALE CONSEGNE RICHIESTE
055400961206     C***
055500961206     C                   MOVEL     '0'           $CR
055600961206IF  1C     ARBDCR        IFNE      *ZEROS                                       *RICHIESTA CONSEGNA
055700961206     C* LEGGE LE VARIAZIONI
055800961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
055900131014     C     KEYARG        SETLL     FIARBF2C
056000131014     C     KEYARG        READE     FIARBF2C                               98
056100961206DO  2C     *IN98         DOWEQ     *OFF                                         *FINO A FINE FILE
056200030108IF  3***C     *IN20         IFEQ      *ON
056300030108     ***C     ARGCVB        ANDEQ     'CR'
056400030108IF  3C                   IF        *in20   and
056500040225     C                             (ARGcvb = 'CR'  or ARGcvb = 'CS' or
056600040225     C                              ARGcvb = 'CA'  or ARGcvb = 'CD')
056700961206     C*
056800961206     C* SE IN PARTENZA NON C'ERA LA CONSEGNA RICHIESTA O ERA DIVERSA :
056900961206     C*    - CONTEGGIA LA BOLLA COME CONSEGNA RICHIESTA ALL'ARRIVO
057000961206     C*    - CONTINUA A LEGGERE LE VARIZIONI SUCESSIVE PER CONTARNE IL N° TOTALE
057100961206IF  4C     ARGTCR        IFNE      ARBTCR
057200961206     C     ARGDCR        ORNE      ARBDCR
057300961206     C     ARGHCR        ORNE      ARBHCR
057400961206     C***
057500961206     C* VARIAZIONI SUCESSIVE
057600961206     C***
057700961206IF  5C     $CR           IFEQ      '1'                                          *LA 1^NON CONTEGGIAT
057800961206     C                   ADD       1             FILTCT                         *TOT. CONS.RICH.TOT.
057900970902     C                   MOVEL     '1'           §TCT                           *DA STAMPARE
058000971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
058100961206     C                   ADD       1             GENTCT
058200961206E   6C                   ENDIF
058300961206E   5C                   ENDIF
058400961206     C***
058500961206     C* PRIMA VARIAZIONE
058600961206     C***
058700961206IF  5C     $CR           IFEQ      '0'
058800961206     C                   ADD       1             FILTCR                         *TOT. CONS. RICHIEST
058900970902     C                   MOVEL     '1'           §TCR                           *DA STAMPARE
059000971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
059100961206     C                   ADD       1             GENTCR
059200961206E   6C                   ENDIF
059300961206     C                   MOVEL     '1'           $CR
059400961206E   5C                   ENDIF
059500961206E   4C                   ENDIF
059600961206E   3C                   ENDIF
059700961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
059800131014     C     KEYARG        READE     FIARBF2C                               98
059900961206E   2C                   ENDDO
060000961206E   1C                   ENDIF
060100961206     C*--------------------
060200961206     C* E V E N T I  DI TIPO "GIACENZA"
060300961206     C*--------------------
060400961206     C                   MOVEL     '0'           $LG                            *EVENTO LAVV/GIAC
060500961206     C                   MOVEL     '0'           $GC                            *APERTURA GIACENZA
060600970115     C                   MOVEL     '0'           $EG                            *EVENTO GIACENZA
060700970115     C                   MOVEL     '0'           $PG                            *PIU' EVENTI GIACENZ
060800990810     C     KEYEVB        SETLL     FNEVB01L
060900990810     C     KEYEVB        READE     FNEVB01L                               98
061000961206DO  1C     *IN98         DOWEQ     *OFF                                         *FINO A FINE FILE
061001131128     c                   eval      devb01   = evbnot
061002131128     c* elaboro solo gli ultimi eventi
061003131128   1ac                   if        §notprg=*blanks or §notprg='U'
061100961206     C                   Z-ADD     1             J
061200961206     C                   SETOFF                                           28
061300961206     C     EVBCEV        LOOKUP    CEV(J)                                 28
061400961206IF  2C     *IN28         IFEQ      *ON                                          *CONSIDERARE EVENTO
061500961206IF  3C     TEV(J)        IFNE      'A'
061600961206     C                   MOVEL     '1'           $EG                            *EVENTO GIACENZA
061700970115IF  4C     $PG           IFEQ      '0'
061800970115     C                   MOVEL     '1'           $PG                            *UN SOLO EVENTO GIAC
061900970115X   4C                   ELSE
062000970115     C                   MOVEL     '2'           $PG                            *PIU' EVENTI GIACENZ
062100970115E   4C                   ENDIF
062200970115E   3C                   ENDIF
062300961206     C* LEGGE LE GIACENZE
062400961206     C                   SETOFF                                           97
062500050310     C     KEYGCP        SETLL     TIGCP01L                               97
062600961206IF  3C     *IN97         IFEQ      *ON                                          *C'E GIACENZA
062700970115     C                   MOVEL     '1'           $GC                            *APERTURA GIACENZA
062800961206     C***
062900961206     C* LASCIATO AVVISO + GIACENZA
063000961206     C***
063100961206IF  4C     TEV(J)        IFEQ      'A'                                          *"LASCIATO AVVISO"
063200961206IF  5C     $CR           IFEQ      '1'                                          *SOLO CONS.RICH.MAN.
063300961206     C     $LG           ANDEQ     '0'                                          *SOLO UN EVENTO
063400961206     C                   ADD       1             FILLEG
063500970902     C                   MOVEL     '1'           §LEG                           *DA STAMPARE
063600971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
063700961206     C                   ADD       1             GENLEG
063800961206E   6C                   ENDIF
063900961206E   5C                   ENDIF
064000961206     C***
064100961206     C* GIACENZA
064200961206     C***
064300961206X   4C                   ELSE                                                   *"GIACENZA"
064400961206IF  5C     $CR           IFEQ      '1'                                          *SOLO CONS.RICH.MAN.
064500961206     C     $LG           ANDEQ     '0'                                          *SOLO UN EVENTO
064600961206     C                   ADD       1             FILGCA
064700970902     C                   MOVEL     '1'           §GCA                           *DA STAMPARE
064800971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
064900961206     C                   ADD       1             GENGCA
065000961206E   6C                   ENDIF
065100961206E   5C                   ENDIF
065200961206E   4C                   ENDIF
065300961206     C* NON C'E' GIACENZA
065400961206X   3C                   ELSE
065500961206IF  4C     TEV(J)        IFEQ      'A'                                          *"LASCIATO AVVISO"
065600961206     C***
065700961206     C* LASCIATO AVVISO
065800961206     C***
065900961206IF  5C     $CR           IFEQ      '1'                                          *SOLO CONS.RICH.MAN.
066000961206     C     $LG           ANDEQ     '0'                                          *SOLO UN EVENTO
066100961206     C                   ADD       1             FILLAV
066200970902     C                   MOVEL     '1'           §LAV                           *DA STAMPARE
066300971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
066400961206     C                   ADD       1             GENLAV
066500961206E   6C                   ENDIF
066600961206E   5C                   ENDIF
066700961206E   4C                   ENDIF
066800961206E   3C                   ENDIF
066900970115     C*
067000961206IF  3C     $CR           IFEQ      '1'                                          *SOLO CONS.RICH.MAN.
067100961206     C                   MOVEL     '1'           $LG                            *L.AVVISO / GIACENZA
067200961206E   3C                   ENDIF
067300961206E   2C                   ENDIF
067301131128   1ac                   endif
067400961206     C*
067500990810     C     KEYEVB        READE     FNEVB01L                               98
067600961206E   1C                   ENDDO                                                  *FINE EVENTI
067700961206     C***
067800961206     C* EVENTO DI GIACENZA SENZA APERTURA GIACENZA
067900961206     C***
068000971002     C* IGNORA LE BOLLE CON PARTICOLARITA' GIACENZA:
068100971002     C* - NON APRIRE GIACENZA MA RENDERE
068200971002     C* - ATTENDERE PRIMA DI APRIRE GIACENZA
068300971003     C                   MOVEL     'N'           WGIACV                         *GIACENZA NON VALIDA
068400971003IF  1C     ARBGGA        IFEQ      *BLANKS                                      *NO PARTICOLARITA'
068500971003     C                   MOVEL     'S'           WGIACV                         *GIACENZA VALIDA
068600971003X   1C                   ELSE
068700971002     C                   SETOFF                                           28
068800971002     C                   Z-ADD     1             J
068900971003     C     ARBGGA        LOOKUP    GGA(J)                                 28
069000971003IF  2C     *IN28         IFEQ      *ON                                          *TROVATA
069100971003IF  3C     G2MF(J)       IFEQ      'A'
069200971002     C     G2MQ(J)       ANDEQ     'P'
069300971002     C     G5DI(J)       OREQ      'R'
069400971002     C     G4D1(J)       OREQ      'R'
069500971002     C     G4D2(J)       OREQ      'R'
069600971002     C     G4D3(J)       OREQ      'R'
069700971002     C     G4D4(J)       OREQ      'R'
069800971002     C     G4D5(J)       OREQ      'R'
069900971003X   3C                   ELSE
070000971003     C                   MOVEL     'S'           WGIACV                         *GIACENZA VALIDA
070100971003E   3C                   ENDIF
070200971003E   2C                   ENDIF
070300971003E   1C                   ENDIF
070400971003IF  1C     WGIACV        IFEQ      'S'                                          *GIACENZA VALIDA
070500970115IF  2C     $EG           IFEQ      '1'                                          *EVENTO GIACENZA
070600961206     C     $GC           ANDEQ     '0'                                          *NO APERTURA GIACENZ
070700961206     C                   ADD       1             FILTEG
070800970902     C                   MOVEL     '1'           §TEG                           *DA STAMPARE
070900971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
071000961206     C                   ADD       1             GENTEG
071100970115E   3C                   ENDIF
071200970115     C***
071300970115     C* PIU' EVENTI DI GIACENZA SENZA APERTURA GIACENZA
071400970115     C***
071500970115IF  3C     $PG           IFGT      '1'
071600970115     C                   ADD       1             FILTPG
071700970902     C                   MOVEL     '1'           §TPG                           *DA STAMPARE
071800971204IF  4C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
071900970115     C                   ADD       1             GENTPG
072000970115E   4C                   ENDIF
072100970115E   3C                   ENDIF
072200970115E   2C                   ENDIF
072300961206E   1C                   ENDIF
072400961206     C***
072500961206     C* FERMO DEPOSITO
072600961206     C***
072700961206     C                   MOVEL     '0'           $FD
072800961206IF  1C     ARBFFD        IFNE      *BLANKS                                      *C'E' FERMO DEPOSITO
072900961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
073000131014     C     KEYARD        SETLL     FIARBF2C
073100131014     C     KEYARD        READE     FIARBF2C                               98
073200961206DO  2C     *IN98         DOWEQ     *OFF                                         *FINO A FINE FILE
073300961206     C     $FD           ANDEQ     '0'                                          *FERMO DEP. TROVATO
073400961206IF  3C     *IN10         IFEQ      *ON
073500961206     C     ARDCVB        ANDEQ     'I0'
073600961206     C*
073700961206     C* SE IN PARTENZA NON C'ERA IL FERMO DEPOSITO :
073800961206     C*  - CONTEGGIA LA BOLLA COME FERMO DEPOSITO ALL'ARRIVO (NEI TOTALI F.D.)
073900961206     C*  - ESCE DALLA LETTURA VARIAZIONI
074000961206IF  4C     ARDFFD        IFEQ      *BLANKS
074100961206     C                   MOVEL     '1'           $FD
074200961206     C                   ADD       1             FILTFD                         *TOT. FERMI DEPOSITI
074300970902     C                   MOVEL     '1'           §TFD                           *DA STAMPARE
074400971204IF  5C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
074500961206     C                   ADD       1             GENTFD
074600961206E   5C                   ENDIF
074700961206E   4C                   ENDIF
074800961206E   3C                   ENDIF
074900961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
075000131014     C     KEYARD        READE     FIARBF2C                               98
075100961206E   2C                   ENDDO
075200961206     C*  - CONTEGGIA LA BOLLA COME FERMO DEPOSITO DELLE CONSEGNE RICHIESTE
075300961206IF  2C     $CR           IFEQ      '1'
075400961206     C                   MOVEL     '1'           $FD
075500961206     C                   ADD       1             FILFFD                         *FERMO DEPOSITO
075600970902     C                   MOVEL     '1'           §FFD                           *DA STAMPARE
075700971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
075800961206     C                   ADD       1             GENFFD
075900961206E   3C                   ENDIF
076000961206E   2C                   ENDIF
076100961206E   1C                   ENDIF
076200961206     C***
076300961206     C* SUPERMERCATO
076400961206     C***
076500961206     C* CALCOLA SOLO SE' E' UNA BOLLA CON CONSEGNA RICHIESTA
076600961206IF  1C     $CR           IFEQ      '1'
076700961206IF  2C     ARBTC1        IFEQ      'S'                                          *CONSEGNA SUPERMARKE
076800961206     C     ARBTC2        OREQ      'S'
076900961206     C                   ADD       1             FILSMK                         *SUPERMARKET
077000970902     C                   MOVEL     '1'           §SMK                           *DA STAMPARE
077100971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
077200961206     C                   ADD       1             GENSMK
077300961206E   3C                   ENDIF
077400961206E   2C                   ENDIF
077500961206E   1C                   ENDIF
077600961206     C***
077700961206     C* APPUNTAMENTO
077800961206     C***
077900961206     C* CALCOLA SOLO SE' E' UNA BOLLA CON CONSEGNA RICHIESTA
078000961206IF  1C     $CR           IFEQ      '1'
078100961206IF  2C     ARBTC1        IFEQ      'A'                                          *CONSEGNA SU APPUNTA
078200961206     C     ARBTC2        OREQ      'A'
078300961206     C                   ADD       1             FILAPP                         *APPUNTAMENTO
078400970902     C                   MOVEL     '1'           §APP                           *DA STAMPARE
078500971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
078600961206     C                   ADD       1             GENAPP
078700961206E   3C                   ENDIF
078800961206E   2C                   ENDIF
078900961206E   1C                   ENDIF
079000961206     C***
079100961206     C* DATA ARRIVO REALE =  DATA PARTENZA ULTIMO COLLO ->
079200961206     C* . NON ESEGUE IL CONFRONTO DELLE ORE CON LE SOGLIE
079300961206     C* DATA ARRIVO REALE <> DATA PARTENZA ULTIMO COLLO ->
079400961206     C* . REPERISCE LA DATA ARRIVO TEORICA
079500961206     C* DATA ARRIVO REALE <  DATA ARRIVO TEORICA ->
079600961206     C* . NON ESEGUE IL CONFRONTO DELLE ORE CON LE SOGLIE
079700961206     C* DATA ARRIVO REALE >=  DATA ARRIVO TEORICA ->
079800961206     C* . CONFRONTA DATA+ORA REALE CON DATA+ORE TEORICHE
079900961206     C***
080000961206     C                   Z-ADD     *ZEROS        D98DTI
080100961206     C                   Z-ADD     *ZEROS        WDUC              8 0
080200961206IF  1C     ARBDUC        IFGT      *ZEROS
080300961206     C                   Z-ADD     ARBDUC        WDUC
080400961206X   1C                   ELSE
080500961206     C                   Z-ADD     ARBDPC        WDUC
080600961206E   1C                   ENDIF
080700961206     C                   SETOFF                                           28
080800961206     C                   Z-ADD     1             J
080900961206     C     ARBLNA        LOOKUP    TRA(J)                                 28     *REPERISCE LE SOGLI
081000961206     C*
081100961206IF  1C     ARBDTI        IFEQ      WDUC                                         *DATA ARR = PAR UL C
081200961206X   1C                   ELSE                                                   *DATA ARR <>PAR UL C
081300961206     C*
081400961206     C* REPERISCE DATA ARRIVO TEORICA
081500961206     C                   EXSR      REPDTI
081600961206IF  2C     ARBDTI        IFLT      D98DTI                                       *DATA REAL < DATA TE
081700961206X   2C                   ELSE                                                   *DATA REAL>= DATA TE
081800961206     C* DATA+ORA REALE
081900961206     C                   Z-ADD     *ZEROS        WDATAR           12 0          *DATA+ORA REALE
082000961206     C                   MOVEL     ARBDTI        WDATAR                         *DATA + ...
082100961206     C                   MOVE      ARBHTI        WDATAR                         * ... + ORA
082200961206     C* 1^DATA+ORA TEORICA
082300961206     C                   Z-ADD     *ZEROS        WDATAT           12 0          *1^DATA+ORA TEORICA
082400961206     C                   MOVEL     D98DTI        WDATAT                         *DATA + ...
082500961206     C                   MOVE      OSA(J)        WDATAT                         * ... + ORA
082600961206IF  3C     WDATAR        IFGT      WDATAT                                       *RITARDO SU 1^SOGLIA
082700961206     C                   ADD       1             FILTIT                         *TOT. ARRIVI RIT. 1^
082800970902     C                   MOVEL     '1'           §TIT                           *DA STAMPARE
082900971204IF  4C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
083000961206     C                   ADD       1             GENTIT
083100961206E   4C                   ENDIF
083200961206IF  4C     $CR           IFEQ      '1'                                          *CONSEGNE RICHIESTE
083300961206     C                   ADD       1             FILRIT                         *ARRIVO RITARDO 1^S
083400970902     C                   MOVEL     '1'           §RIT                           *DA STAMPARE
083500971204IF  5C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
083600961206     C                   ADD       1             GENRIT
083700961206E   5C                   ENDIF
083800961206E   4C                   ENDIF
083900961206E   3C                   ENDIF
084000961206     C* 2^DATA+ORA TEORICA
084100961206     C                   Z-ADD     *ZEROS        WDATAT           12 0          *2^DATA+ORA TEORICA
084200961206     C                   MOVEL     D98DTI        WDATAT                         *DATA + ...
084300961206     C                   MOVE      OSA1(J)       WDATAT                         * ... + ORA
084400961206IF  3C     WDATAR        IFGT      WDATAT                                       *RITARDO SU 2^SOGLIA
084500961206     C                   ADD       1             FILTI2                         *TOT. ARRIVI RIT. 2^
084600970902     C                   MOVEL     '1'           §TI2                           *DA STAMPARE
084700971204IF  4C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
084800961206     C                   ADD       1             GENTI2
084900961206E   4C                   ENDIF
085000961206IF  4C     $CR           IFEQ      '1'                                          *CONSEGNE RICHIESTE
085100961206     C                   ADD       1             FILRI2                         *ARRIVO RITARDO 2^S
085200970902     C                   MOVEL     '1'           §RI2                           *DA STAMPARE
085300971204IF  5C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
085400961206     C                   ADD       1             GENRI2
085500961206E   5C                   ENDIF
085600961206E   4C                   ENDIF
085700961206E   3C                   ENDIF
085800961206E   2C                   ENDIF
085900961206E   1C                   ENDIF
086000961206     C***
086100961206     C* CONTRASSEGNI CHE SONO STATI ABILITATI ALLA CONSEGNA
086200961206     C***
086300961206IF  1C     $CR           IFEQ      '1'                                          *SOLO PER CONS.RICH.
086400051115     C     KEYAR9        CHAIN     FiAR901L                           98
086500011211     C*                  MOVEL     'N'           WABATM                         *ABILITAZ.AUTOM.C/A
086600961206     C*
086700961206     C* SOLO SE ESISTE CONTRASSEGNO
086800961206IF  2C     *IN98         IFEQ      *OFF                                         *ESISTE C/ASSEGNO
086900961206     C     AR9CAS        ANDGT     *ZEROS
087000961206     C*
087100010124     C* CONTROLLA SE IL C/A E' STATO ABILITATO AUTOMATICAMENTE
087200020906     C*                  Z-ADD     1             KTBKUT                         *CODICE UTENTE
087300011211     C*                  MOVEL     '1A'          KTBCOD                         *CODICE TABELLA
087400011211     C*                  MOVEL(P)  AR9TIC        KTBKEY                         *CHAIVE TABELLA
087500011211     C*    KEYTAB        CHAIN     TABEL00F                           98
087600011211IF  3C*    *IN98         IFEQ      *OFF
087700011211     C*                  MOVEL     TBLUNI        DS1A
087800011211     C*                  Z-ADD     *ZEROS        W0133            13 3
087900011211     C*                  EXSR      CTRCAS
088000011211     C*                  EXSR      REPGEI
088100011211     C                   EXSR      REPMAXCSB
088200011211IF  4C*    W0133         IFLE      §GELCA                                       *SOTTO LIM C/ASSEGNO
088300011211IF  5C*    §1AFCA        IFNE      'C'                                          *NO ASSEGNO CIRCOLAR
088400011211     C*                  MOVEL     'S'           WABATM                         *ABILITAZ.AUTOM. C/A
088500011211E   5C*                  ENDIF
088600011211X   4C*                  ELSE                                                   *SOPRA LIM C/ASSEGNO
088700011211IF  5C*    §1AFCA        IFNE      'C'                                          *NO ASSEGNO CIRCOLAR
088800011211     C*    §1AFCA        ANDNE     'S'                                          *NO CONTANTI
088900011211     C*                  MOVEL     'S'           WABATM                          *ABILITAZ.AUTOM. C/
089000011211E   5C*                  ENDIF
089100011211E   4C*                  ENDIF
089200011211E   3C*                  ENDIF
089300961206     C*
089400961206     C* ABILITAZIONE NON AUTOMATICA, CONTEGGIO
089500011211IF  3C*    WABATM        IFEQ      'N'
089600011211     C     O21FCA        IFNE      *BLANKS
089700961206     C                   ADD       1             FILCAS                         *C/A ABILITATI
089800970902     C                   MOVEL     '1'           §CAS                           *DA STAMPARE
089900971204IF  4C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
090000961206     C                   ADD       1             GENCAS
090100961206E   4C                   ENDIF
090200961206E   3C                   ENDIF
090300961206E   2C                   ENDIF
090400961206E   1C                   ENDIF
090500961206     C***
090600961206     C* TURNI DI CHIUSURA
090700961206     C***
090800961206     C                   MOVEL     '0'           $TC
090900961206IF  1C     ARBGC1        IFNE      *BLANKS                                      *1°GG TURNO CHIUSURA
091000961206     C     ARBGC2        ORNE      *BLANKS                                      *2°GG TURNO CHIUSURA
091100961206     C* LEGGE LE VARIAZIONI
091200961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
091300961206     C                   MOVEL     '0'           $TC
091400131014     C     KEYARG        SETLL     FIARBF2C
091500131014     C     KEYARG        READE     FIARBF2C                               98
091600961206DO  2C     *IN98         DOWEQ     *OFF                                         *FINO A FINE FILE
091700961206     C     $TC           ANDEQ     '0'                                          *TURNO CHIUS. TROVAT
091800030108IF  3***C     *IN20         IFEQ      *ON
091900030108     ***C     ARGCVB        ANDEQ     'CR'
092000030108IF  3C                   IF        *in20   and
092100040225     C                             (ARGcvb = 'CR'  or ARGcvb = 'CS' or
092200040225     C                              ARGcvb = 'CD')
092300961206     C*
092400961206     C* IN ARRIVO C'E' IL 1° GG TURNO DI CHIUSURA (ARBGC1<>' ')
092500961206IF  4C     ARBGC1        IFNE      *BLANKS
092600070314      * conteggio solo se la variazione è stata fatta in arrivo o in partenza ma su bolla locale
092700070314      * (lnp = lna)
092800070314   4ac                   if        %subst(argpru:10:1) = 'A' or
092900070314     c                             %subst(argpru:10:1) = 'P' and arblnp = arblna
093000961206     C*
093100961206     C* SE IN PARTENZA NON C'ERA IL 1° GG TURNO DI CHIUSURA (ARGGC1=' ')   E
093200961206     C* QUELLO IN ARRIVO E' DIVERSO DAL 2° GG DELLA PARTENZA (ARBGC1<>ARGGC2)
093300961206     C*   - CONTEGGIA LA BOLLA CON TURNO DI CHIUSURA ALL'ARRIVO
093400961206     C*   - ESCE DALLA LETTURA VARIAZIONI
093500961206IF  5C     ARGGC1        IFEQ      *BLANKS                                      *NO IN PARTENZA
093600961206     C     ARBGC1        ANDNE     ARGGC2                                       *1°ARRIVO<>2°PARTENZ
093700961206     C                   MOVEL     '1'           $TC
093800961206     C                   ADD       1             FILTTC                         *TOT. TURNI CHIUSURA
093900970902     C                   MOVEL     '1'           §TTC                           *DA STAMPARE
094000971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
094100961206     C                   ADD       1             GENTTC
094200961206E   6C                   ENDIF
094300961206E   5C                   ENDIF
094400070314   4ac                   endif
094500961206E   4C                   ENDIF
094600961206     C*
094700961206     C* IN ARRIVO C'E' IL 2° GG TURNO DI CHIUSURA (ARBGC2<>' ')
094800961206IF  4C     $TC           IFEQ      '0'                                          *NON ANCORA CONTEGG.
094900961206IF  5C     ARBGC2        IFNE      *BLANKS
095000070314      * conteggio solo se la variazione è stata fatta in arrivo o in partenza ma su bolla locale
095100070314      * (lnp = lna)
095200070314   5ac                   if        %subst(argpru:10:1) = 'A' or
095300070314     c                             %subst(argpru:10:1) = 'P' and arblnp = arblna
095400961206     C*
095500961206     C* SE IN PARTENZA NON C'ERA IL 2° GG TURNO DI CHIUSURA (ARGGC2=' ')   E
095600961206     C* QUELLO IN ARRIVO E' DIVERSO DAL 1° GG DELLA PARTENZA (ARBGC2<>ARGGC1)
095700961206     C*   - CONTEGGIA LA BOLLA CON TURNO DI CHIUSURA ALL'ARRIVO
095800961206     C*   - ESCE DALLA LETTURA VARIAZIONI
095900961206IF  6C     ARGGC2        IFEQ      *BLANKS                                      *NO IN PARTENZA
096000961206     C     ARBGC2        ANDNE     ARGGC1                                       *2°ARRIVO<>1°PARTENZ
096100961206     C                   MOVEL     '1'           $TC
096200961206     C                   ADD       1             FILTTC                         *TOT. TURNI CHIUSURA
096300970902     C                   MOVEL     '1'           §TTC                           *DA STAMPARE
096400971204IF  7C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
096500961206     C                   ADD       1             GENTTC                         *TOT. TURNI CHIUSURA
096600961206E   7C                   ENDIF
096700961206E   6C                   ENDIF
096800070314   5ac                   endif
096900961206E   5C                   ENDIF
097000961206E   4C                   ENDIF
097100961206E   3C                   ENDIF
097200961206     C*
097300961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
097400131014     C     KEYARG        READE     FIARBF2C                               98
097500961206E   2C                   ENDDO
097600961206E   1C                   ENDIF
097700961206     C***
097800961206     C* CONSEGNA PRIMA / DOPO 13:00
097900961206     C***
098000961206IF  1C     ARBHMC        IFLE      1300                                         *M A T T I N O
098100961206     C                   ADD       1             FILTPT                         *TOT. CONS. PRIMA 13
098200970902     C                   MOVEL     '1'           §TPT                           *DA STAMPARE
098300971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
098400961206     C                   ADD       1             GENTPT
098500961206E   3C                   ENDIF
098600961206     C*
098700961206     C* LEGGE LA DISTINTA CONSEGNA PER VEDERE SE E' DEL POMERIGGIO
098800961206     C     KEYFVV        CHAIN     FNFVV01L                           98
098900961206IF  2C     *IN98         IFEQ      *OFF
099000961206     C     FVVATB        ANDEQ     *BLANKS
099100961206     C     FVVFPP        ANDEQ     'P'
099200961206     C                   ADD       1             FILTPM                         *TOT. CONS. X POMERI
099300970902     C                   MOVEL     '1'           §TPM                           *DA STAMPARE
099400971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
099500961206     C                   ADD       1             GENTPM
099600961206E   3C                   ENDIF
099700961206E   2C                   ENDIF
099800961206X   1C                   ELSE                                                   *P O M E R I G G I O
099900961206     C                   ADD       1             FILTDT                         *TOT. CONS. DOPO 13
100000970902     C                   MOVEL     '1'           §TDT                           *DA STAMPARE
100100971204IF  2C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
100200961206     C                   ADD       1             GENTDT
100300961206E   2C                   ENDIF
100400961206E   1C                   ENDIF
100500961206     C*
100600961206     C                   ENDSR
100700961206     C*------------------------------------------------------------------------*
100800961206     C* REPDTI - REPERISCE LA DATA DI ARRIVO TEORICA DELLA TRAZIONE
100900961206     C*------------------------------------------------------------------------*
101000961206     C     REPDTI        BEGSR
101100961206     C*
101200971009     C                   MOVEL     '1'           WSD99                          *LANCIATO TNSD99R
101300961206     C*
101400971009     C                   CLEAR                   DS99
101500961206     C                   MOVEL     'T'           D98TLA                         *TIPO LANCIO
101600961206     C                   MOVEL     ARBLNP        D98LNP                         *LINEA DI PARTENZA
101700961206     C                   MOVEL     ARBTFP        D98TFP                         *TERMINAL DI PARTENZ
101800961206     C                   MOVEL     ARBLNA        D98LNA                         *LINEA DI ARRIVO
101900961206     C                   MOVEL     ARBTFA        D98TFA                         *TERMINAL DI ARRIVO
102000961206     C                   MOVEL     WDUC          D98DSP                         *DATA PART ULT COLLO
102100961206     C                   MOVEL     'E'           D98TSP                         *TIPO SERVIZIO ESPRE
102200971009     C                   MOVEL     ARBCAD        D98CAD                         *CAP DESTINO
102300971009     C                   MOVEL     ARBLOD        D98LOD                         *LOCALITA' DESTINO
102400971009     C                   CALL      'TNSD99R'
102500971009     C                   PARM                    DS99
102600961206     C*
102700961206     C                   ENDSR
102800961206     C*------------------------------------------------------------------------*
102900961206     C* CTRCAS - CONTROLLA SE IL CONTRASSEGNO E' IN DIVISA ESTERA
103000961206     C*------------------------------------------------------------------------*
103100011211     C*    CTRCAS        BEGSR
103200010822     C*
103300011211     C*                  IF        AR9VCA = *blanks
103400011211     C*                  EVAL      AR9VCA = 'ITL'
103500011211     C*                  ENDIF
103600010822     C*
103700011211     C*                  IF        AR9VCA <> 'ITL' and
103800011211     C*                            AR9VCA <> 'EUR'
103900010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
104000011211     C*                  CLEAR                   YEURDS
104100011211     C*                  Z-ADD     AR9CAS        YECIMI
104200011211     C*                  MOVEL     AR9VCA        YECDVI
104300011211     C*                  EXSR      EXECNV
104400011211     C*                  Z-ADD     YECIMO        W0133
104500011211     C*                  MOVEL     YECDVO        WVCA              3
104600011211     C*                  ELSE
104700011211     C*                  Z-ADD     AR9CAS        W0133
104800011211     C*                  MOVEL     AR9VCA        WVCA
104900011211     C*                  ENDIF
105000011211     C*
105100011211     C*                  ENDSR
105200010822     C*------------------------------------------------------------------------*
105300011211     C*   REPGEI - REPERISCE I LIMITI CONSENTITI X IL CONTRASSEGNO DALLA TABELLA "GEI"
105400011211     C*------------------------------------------------------------------------*
105500011211     C*    REPGEI        BEGSR
105600010822     C*
105700011211     C* REPERISCO I LIMITI X IL CONTRASEGNO
105800011211     C*                  MOVEL     'GEI'         KTECOD
105900011211     C*                  MOVEL     *BLANKS       KTEKE1
106000011211     C*                  MOVEL     *BLANKS       KTEKE2
106100011211     C*                  MOVEL     WVCA          KTEKE1
106200011211     C*    KTBE          CHAIN     TNTBE01L                           21
106300011211     C* N21              MOVEL     TBEUNI        DGEI
106400011211     C*
106500011211     C*                  ENDSR
106600011211
106700011211     C*------------------------------------------------------------------------*
106800011211     C* Controllo importo massimo contrassegno.
106900011211     C*------------------------------------------------------------------------*
107000011211     C     REPMAXCSB     BEGSR
107100011211     C*
107200011211     C                   CLEAR                   Trul21DS
107300011211     C*
107400011211     C                   EVAL      I21TSp = arbTSP
107500011211     C                   EVAL      I21NzD = arbNZD
107600011211     C                   EVAL      I21CBo = arbCBO
107700011211     C                   EVAL      I21LnP = arbLNP
107800011211     C                   EVAL      I21LnA = arbLNA
107900130828     C******             EVAL      I21Ksc = arbKSC
108000011211     C                   EVAL      I21NzM = arbNZM
108100011211     C                   MOVE(p)   arbCTR        I21Ctr
108200011211     C                   EVAL      I21Tic = ar9TIC
108300011211     C                   EVAL      I21Imp = ar9CAS
108400011211     C                   EVAL      I21Div = ar9VCA
108500011211     C                   EVAL      I21Ute = KNMUS
108600130828     c*
108700130828     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
108800130828     c                   z-add     1             ktbkut
108900130828     c                   move      '3A'          ktbcod
109000130828     c                   movel(p)  arbcbo        ktbkey
109100130828     c                   clear                   ds3a
109200130828     c     keytab        chain     tabel00f
109300130828     c                   if        %found(tabel00f)
109400130828     c                   movel     tbluni        ds3a
109500130828     c                   endif
109600130828     c                   if        %subst(§3atb1:1:1)='F'
109700130828     c                   z-add     arbksc        i21ksc
109800130828     c                   else
109900130828     c                   z-add     arbccm        i21ksc
110000130828     c                   if        i21ksc=0
110100130828     c                   z-add     8888          i21ksc
110200130828     c                   movel     arblnp        i21ksc
110300130828     c                   endif
110400130828     c                   endif
110500011211     C*
110600011211     C                   CALL      'TRUL21R'
110700011211     C                   PARM                    Trul21DS
110800011211     C*
110900011211     C                   ENDSR
111000011211     C*------------------------------------------------------------------------*
111100011211
111200961206     C*------------------------------------------------------------------------*
111300961206     C* STADET - STAMPA RIGA DI DETTAGLIO
111400961206     C*------------------------------------------------------------------------*
111500961206     C     STADET        BEGSR
111600961206     C*
111700961206     C* STAMPA SOLO LE BOLLE DI PARTICOLARE INTERESSE
111800970902IF  1C     §TCR          IFEQ      '1'
111900970902     C     §LAV          OREQ      '1'
112000970902     C     §LEG          OREQ      '1'
112100970902     C     §GCA          OREQ      '1'
112200970902     C     §FFD          OREQ      '1'
112300970902     C     §SMK          OREQ      '1'
112400970902     C     §APP          OREQ      '1'
112500970902     C     §RIT          OREQ      '1'
112600970902     C     §RI2          OREQ      '1'
112700970902     C     §CAS          OREQ      '1'
112800970902     C     §TCT          OREQ      '1'
112900970902     C     §TFD          OREQ      '1'
113000970902     C     §TIT          OREQ      '1'
113100970902     C     §TI2          OREQ      '1'
113200970902     C     §TTC          OREQ      '1'
113300970902     C     §TPM          OREQ      '1'
113400970902     C     §TDT          OREQ      '1'
113500970902     C     §TEG          OREQ      '1'
113600970902     C     §TPG          OREQ      '1'
113700961206     C*--------------------
113800961206     C* D E C O D I F I C H E
113900961206     C*--------------------
114000970902     C                   CLEAR                   CSTM                           *CAUSALI
114100970902     C                   Z-ADD     *ZEROS        I                              *INDICE
114200971002IF  2C     §TCR          IFEQ      '1'
114300970902     C                   ADD       1             I
114400970902     C                   MOVEL     '1'           CSTM(I)                        *TOT. CONS. RICHIEST
114500971002E   2C                   ENDIF
114600971002IF  2C     §LAV          IFEQ      '1'
114700970902     C                   ADD       1             I
114800970902     C                   MOVEL     '2'           CSTM(I)                          -LASCIATO AVVISO
114900971002E   2C                   ENDIF
115000971002IF  2C     §LEG          IFEQ      '1'
115100970902     C                   ADD       1             I
115200970902     C                   MOVEL     '3'           CSTM(I)                          -L.AVVISO+GIACENZA
115300971002E   2C                   ENDIF
115400971002IF  2C     §GCA          IFEQ      '1'
115500970902     C                   ADD       1             I
115600970902     C                   MOVEL     '4'           CSTM(I)                          -GIACENZA
115700971002E   2C                   ENDIF
115800971002IF  2C     §FFD          IFEQ      '1'
115900970902     C                   ADD       1             I
116000970902     C                   MOVEL     '5'           CSTM(I)                          -FERMO DEPOSITO
116100971002E   2C                   ENDIF
116200971002IF  2C     §SMK          IFEQ      '1'
116300970902     C                   ADD       1             I
116400970902     C                   MOVEL     '6'           CSTM(I)                          -SUPERMARKET
116500971002E   2C                   ENDIF
116600971002IF  2C     §APP          IFEQ      '1'
116700970902     C                   ADD       1             I
116800970902     C                   MOVEL     '7'           CSTM(I)                          -APPUNTAMENTO
116900971002E   2C                   ENDIF
117000971002IF  2C     §RIT          IFEQ      '1'
117100970902     C                   ADD       1             I
117200970902     C                   MOVEL     '8'           CSTM(I)                          -ARRIVO RITARDO 1^
117300971002E   2C                   ENDIF
117400971002IF  2C     §RI2          IFEQ      '1'
117500970902     C                   ADD       1             I
117600970902     C                   MOVEL     '9'           CSTM(I)                          -ARRIVO RITARDO 2^
117700971002E   2C                   ENDIF
117800971002IF  2C     §CAS          IFEQ      '1'
117900970902     C                   ADD       1             I
118000970902     C                   MOVEL     'A'           CSTM(I)                          -CONTRASSEGNO ABIL
118100971002E   2C                   ENDIF
118200971002IF  2C     §TCT          IFEQ      '1'
118300970902     C                   ADD       1             I
118400970902     C                   MOVEL     'B'           CSTM(I)                        *TOT. CONS.RICH.TOT.
118500971002E   2C                   ENDIF
118600971002IF  2C     §TFD          IFEQ      '1'
118700970902     C                   ADD       1             I
118800970902     C                   MOVEL     'C'           CSTM(I)                        *TOT. FERMI DEPOSITI
118900971002E   2C                   ENDIF
119000971002IF  2C     §TIT          IFEQ      '1'
119100970902     C                   ADD       1             I
119200970902     C                   MOVEL     'D'           CSTM(I)                        *TOT. ARRIVI RIT. 1^
119300971002E   2C                   ENDIF
119400971002IF  2C     §TI2          IFEQ      '1'
119500970902     C                   ADD       1             I
119600970902     C                   MOVEL     'E'           CSTM(I)                        *TOT. ARRIVI RIT. 2^
119700971002E   2C                   ENDIF
119800971002IF  2C     §TTC          IFEQ      '1'
119900970902     C                   ADD       1             I
120000970902     C                   MOVEL     'F'           CSTM(I)                        *TOT. TURNI CHIUSURA
120100971002E   2C                   ENDIF
120200971002IF  2C     §TPM          IFEQ      '1'
120300970902     C                   ADD       1             I
120400970902     C                   MOVEL     'H'           CSTM(I)                        *TOT. CONS. X POMERI
120500971002E   2C                   ENDIF
120600971002IF  2C     §TDT          IFEQ      '1'
120700970902     C                   ADD       1             I
120800970902     C                   MOVEL     'I'           CSTM(I)                        *TOT. CONS. DOPO 13
120900971002E   2C                   ENDIF
121000971002IF  2C     §TEG          IFEQ      '1'
121100970902     C                   ADD       1             I
121200970902     C                   MOVEL     'L'           CSTM(I)                        *TOT. EVEN.SENZA GIA
121300971002E   2C                   ENDIF
121400971002IF  2C     §TPG          IFEQ      '1'
121500970902     C                   ADD       1             I
121600970902     C                   MOVEL     'M'           CSTM(I)                        *TOT.+ EVENTI GIAC.
121700971002E   2C                   ENDIF
121800970902     C                   MOVEA     CSTM          DTCCAU                         *CAUSALI STAMPA DETT
121900970916     C*
122000970916     C* STAMPA SOLO LE BOLLE CON CAUSALI DETTAGLIO RICHIESTE
122100970916     C                   MOVEL     'N'           WSTDET                         *NO STAMPA DETTAGLIO
122200971002IF  2C     PARCAU        IFEQ      *BLANKS                                      *NESSUNA SELEZIONE
122300971002     C                   MOVEL     'S'           WSTDET                         *STAMPA DETTAGLIO
122400971002X   2C                   ELSE                                                   *SELEZIONI
122500970916     C                   Z-ADD     1             I
122600970916     C                   Z-ADD     1             J
122700970916     C                   MOVEA     PARCAU        SA1
122800971002DO  3C     1             DO        20            I                              *CAUSALI DA STAMPARE
122900971002IF  4C     SA1(I)        IFNE      *BLANKS
123000971002DO  5C     1             DO        20            J                              *CAUSALI BOLLE
123100971002IF  6C     CSTM(J)       IFNE      *BLANKS
123200971002IF  7C     SA1(I)        IFEQ      CSTM(J)
123300970916     C                   MOVEL     'S'           WSTDET                         *STAMPA DETTAGLIO
123400971002E   7C                   ENDIF
123500971002E   5C                   ENDIF
123600971002E   4C                   ENDDO
123700971002E   4C                   ENDIF
123800971002E   3C                   ENDDO
123900971002E   2C                   ENDIF
124000970902     C*
124100970916IF  2C     WSTDET        IFEQ      'S'                                          *DETTAGLIO DA STAMAR
124200961206     C                   Z-ADD     ARBDCR        G08INV                         *CONSEGNA RICHIESTA
124300961206     C                   Z-ADD     *ZEROS        G08DAT
124400961206     C                   MOVEL     '3'           G08ERR
124500961206     C                   CALL      'XSRDA8'
124600961206     C                   PARM                    WLBDA8
124700961206     C                   Z-ADD     G08DAT        DTCDCR
124800961206     C*
124900961206     C                   Z-ADD     ARBDTI        G08INV                         *ARRIVO TRAZIONE
125000961206     C                   Z-ADD     *ZEROS        G08DAT
125100961206     C                   MOVEL     '3'           G08ERR
125200961206     C                   CALL      'XSRDA8'
125300961206     C                   PARM                    WLBDA8
125400961206     C                   Z-ADD     G08DAT        DTCDTI
125500961206     C*
125600961206     C                   Z-ADD     D98DTI        G08INV                         *ARRIVO TRAZ. TEORIC
125700961206     C                   Z-ADD     *ZEROS        G08DAT
125800961206     C                   MOVEL     '3'           G08ERR
125900961206     C                   CALL      'XSRDA8'
126000961206     C                   PARM                    WLBDA8
126100961206     C                   Z-ADD     G08DAT        DTID98
126200961206     C*
126300961206     C                   Z-ADD     WDUC          G08INV                         PARTENZA ULTIMO COLL
126400961206     C                   Z-ADD     *ZEROS        G08DAT
126500961206     C                   MOVEL     '3'           G08ERR
126600961206     C                   CALL      'XSRDA8'
126700961206     C                   PARM                    WLBDA8
126800961206     C                   Z-ADD     G08DAT        DUCARB
126900961206     C*
127000961206     C                   Z-ADD     ARBDCE        G08INV                         *CONSEGNA PREVISTA
127100961206     C                   Z-ADD     *ZEROS        G08DAT
127200961206     C                   MOVEL     '3'           G08ERR
127300961206     C                   CALL      'XSRDA8'
127400961206     C                   PARM                    WLBDA8
127500961206     C                   Z-ADD     G08DAT        DTCDCE
127600961206     C*
127700961206     C                   Z-ADD     ARBDCM        G08INV                         *CONSEGNA MERCE
127800961206     C                   Z-ADD     *ZEROS        G08DAT
127900961206     C                   MOVEL     '3'           G08ERR
128000961206     C                   CALL      'XSRDA8'
128100961206     C                   PARM                    WLBDA8
128200961206     C                   Z-ADD     G08DAT        DTCDCM
128300961206     C*
128400970916     C* FINE PAGINA
128500970916IF  3C     *IN01         IFEQ      *ON
128600970916     C                   WRITE     SD88TE                                       *TESTATA
128700970916     C                   MOVEL     DEC(1)        INCTOT
128800970916     C                   Z-ADD     DEPLNA        INCDET
128900970916     C                   MOVEL     DECLNA        INDDET
129000970916     C                   Z-ADD     DEPOSA        INCOSA
129100970916     C                   Z-ADD     DEPOS1        INCOS1
129200970916     C                   WRITE     SD88IG                                       *INTEST.NE GENERALE
129300970916     C                   WRITE     SD88ID                                       *INTEST.NE DETTAGLIO
129400970916     C                   SETOFF                                       01
129500970916E   3C                   ENDIF
129600961206     C                   WRITE     SD88DT                                       *DETTAGLIO
129700970916E   2C                   ENDIF
129800961206E   1C                   ENDIF
129900961206     C*
130000961206     C                   ENDSR
130100961206     C*------------------------------------------------------------------------*
130200961206     C* STATOF - STAMPA TOTALI FILIALE
130300961206     C*------------------------------------------------------------------------*
130400961206     C     STATOF        BEGSR
130500961206     C*
130600961206     C                   MOVEL     DEC(1)        INCTOT                         *PER INTESTAZIONE
130700961206     C                   Z-ADD     DEPLNA        INCDET
130800961206     C                   MOVEL     DECLNA        INDDET
130900961206     C                   Z-ADD     DEPOSA        INCOSA
131000961206     C                   Z-ADD     DEPOS1        INCOS1
131100961206     C* PASSA I TOTALI
131200961206     C                   Z-ADD     FILTBL        WTBL                           *TOT. BOLLE
131300961206     C                   Z-ADD     FILTCR        WTCR                           *TOT. CONS. RICHIEST
131400961206     C                   Z-ADD     FILLAV        WLAV                             -LASCIATO AVVISO
131500961206     C                   Z-ADD     FILLEG        WLEG                             -L.AVVISO+GIACENZA
131600961206     C                   Z-ADD     FILGCA        WGCA                             -GIACENZA
131700961206     C                   Z-ADD     FILFFD        WFFD                             -FERMO DEPOSITO
131800961206     C                   Z-ADD     FILSMK        WSMK                             -SUPERMARKET
131900961206     C                   Z-ADD     FILAPP        WAPP                             -APPUNTAMENTO
132000961206     C                   Z-ADD     FILRIT        WRIT                             -ARRIVO RITARDO 1^
132100961206     C                   Z-ADD     FILRI2        WRI2                             -ARRIVO RITARDO 2^
132200961206     C                   Z-ADD     FILCAS        WCAS                             -CONTRASSEGNO
132300961206     C                   Z-ADD     FILTCT        WTCT                           *TOT. CONS.RICH.TOT.
132400961206     C                   Z-ADD     FILTIT        WTIT                           *TOT. ARRIVI RIT. 1^
132500961206     C                   Z-ADD     FILTI2        WTI2                           *TOT. ARRIVI RIT. 2^
132600961206     C                   Z-ADD     FILTTC        WTTC                           *TOT. TURNI CHIUSURA
132700961206     C                   Z-ADD     FILTFD        WTFD                           *TOT. FERMI DEPOSITI
132800961206     C                   Z-ADD     FILTPT        WTPT                           *TOT. CONS. PRIMA 13
132900961206     C                   Z-ADD     FILTPM        WTPM                           *TOT. CONS. X POMERI
133000961206     C                   Z-ADD     FILTDT        WTDT                           *TOT. CONS. DOPO 13
133100961206     C                   Z-ADD     FILTEG        WTEG                           *TOT. EVEN.SENZA GIA
133200970115     C                   Z-ADD     FILTPG        WTPG                           *TOT.+EVEN.SENZA GIA
133300961206     C* STAMPA I TOTALI
133400961206     C                   EXSR      STATOT
133500961206     C*
133600961206     C                   ENDSR
133700961206     C*------------------------------------------------------------------------*
133800961206     C* STATOG - STAMPA TOTALI GENERALI
133900961206     C*------------------------------------------------------------------------*
134000961206     C     STATOG        BEGSR
134100961206     C*
134200961206     C                   MOVEL     DEC(2)        INCTOT                         *PER INTESTAZIONE
134300961206     C                   Z-ADD     *ZEROS        INCDET
134400961206     C                   MOVEL     *BLANKS       INDDET
134500961206     C                   Z-ADD     *ZEROS        INCOSA
134600961206     C                   Z-ADD     *ZEROS        INCOS1
134700961206     C* PASSA I TOTALI
134800961206     C                   Z-ADD     GENTBL        WTBL                           *TOT. BOLLE
134900961206     C                   Z-ADD     GENTCR        WTCR                           *TOT. CONS. RICHIEST
135000961206     C                   Z-ADD     GENLAV        WLAV                             -LASCIATO AVVISO
135100961206     C                   Z-ADD     GENLEG        WLEG                             -L.AVVISO+GIACENZA
135200961206     C                   Z-ADD     GENGCA        WGCA                             -GIACENZA
135300961206     C                   Z-ADD     GENFFD        WFFD                             -FERMO DEPOSITO
135400961206     C                   Z-ADD     GENSMK        WSMK                             -SUPERMARKET
135500961206     C                   Z-ADD     GENAPP        WAPP                             -APPUNTAMENTO
135600961206     C                   Z-ADD     GENRIT        WRIT                             -ARRIVO RITARDO 1^
135700961206     C                   Z-ADD     GENRI2        WRI2                             -ARRIVO RITARDO 2^
135800961206     C                   Z-ADD     GENCAS        WCAS                             -CONTRASSEGNO
135900961206     C                   Z-ADD     GENTCT        WTCT                           *TOT. CONS.RICH.TOT.
136000961206     C                   Z-ADD     GENTIT        WTIT                           *TOT. ARRIVI RIT. 1^
136100961206     C                   Z-ADD     GENTI2        WTI2                           *TOT. ARRIVI RIT. 2^
136200961206     C                   Z-ADD     GENTTC        WTTC                           *TOT. TURNI CHIUSURA
136300961206     C                   Z-ADD     GENTFD        WTFD                           *TOT. FERMI DEPOSITI
136400961206     C                   Z-ADD     GENTPT        WTPT                           *TOT. CONS. PRIMA 13
136500961206     C                   Z-ADD     GENTPM        WTPM                           *TOT. CONS. X POMERI
136600961206     C                   Z-ADD     GENTDT        WTDT                           *TOT. CONS. DOPO 13
136700961206     C                   Z-ADD     GENTEG        WTEG                           *TOT. EVEN.SENZA GIA
136800970115     C                   Z-ADD     GENTPG        WTPG                           *TOT.+EVEN.SENZA GIA
136900961206     C* STAMPA I TOTALI
137000961206     C                   EXSR      STATOT
137100961206     C*
137200961206     C                   ENDSR
137300961206     C*------------------------------------------------------------------------*
137400961206     C* STATOT - STAMPA TOTALI
137500961206     C*------------------------------------------------------------------------*
137600961206     C     STATOT        BEGSR
137700961206     C*
137800961206     C* STAMPA IL TOTALE A PAGINA NUOVA SE NE HA GIA' STAMPATI DUE SU UNA STESSA
137900961206     C* E SE STO STAMPANDO I SOLI TOTALI  O  SE STO STAMPANDO ANCHE IL DETTAGLIO
138000961206     C                   ADD       1             $TOTST                         *N° TOTALI STAMPATI
138100961206     C     $TOTST        DIV       2             COMODO            7 0
138200961206     C                   MVR                     RESTO             3 0
138300961206IF  1C     PARDET        IFEQ      'N'                                          *SOLO TOTALI
138400961206     C     RESTO         ANDGT     *ZEROS                                       *GIA' STAMPATI DUE
138500970902     C     PARDET        OREQ      'S'                                          *ANCHE DETTAGLIO
138600961206     C                   WRITE     SD88TE                                       *TESTATA
138700961206E   1C                   ENDIF
138800961206     C*
138900961206     C                   WRITE     SD88IG                                       *INTEST.NE GENERALE
139000961206     C*
139100961206     C                   Z-ADD     *ZEROS        $NUM              9 0          *CAMPO DI COMODO
139200961206     C                   CLEAR                   SD88TO                         *PULISCE RIGA STAMPA
139300961206     C*--------------------
139400961206     C* N U M E R I   E   P E R C E N T U A L I
139500961206     C*--------------------
139600961206     C                   Z-ADD     WTBL          TOCTBL
139700961206     C*
139800961206     C                   Z-ADD     WTCR          TOCTCR
139900961206IF  1C     WTCR          IFGT      *ZEROS
140000961206     C     100           MULT      WTCR          $NUM
140100961206     C     $NUM          DIV       WTBL          TOPTCR
140200961206E   1C                   ENDIF
140300961206     C*
140400961206     C                   Z-ADD     WLAV          TOCLAV
140500961206IF  1C     WLAV          IFGT      *ZEROS
140600961206     C     100           MULT      WLAV          $NUM
140700961206     C     $NUM          DIV       WTBL          TOPLAV
140800961206E   1C                   ENDIF
140900961206     C*
141000961206     C                   Z-ADD     WLEG          TOCLEG
141100961206IF  1C     WLEG          IFGT      *ZEROS
141200961206     C     100           MULT      WLEG          $NUM
141300961206     C     $NUM          DIV       WTBL          TOPLEG
141400961206E   1C                   ENDIF
141500961206     C*
141600961206     C                   Z-ADD     WGCA          TOCGCA
141700961206IF  1C     WGCA          IFGT      *ZEROS
141800961206     C     100           MULT      WGCA          $NUM
141900961206     C     $NUM          DIV       WTBL          TOPGCA
142000961206E   1C                   ENDIF
142100961206     C*
142200961206     C                   Z-ADD     WFFD          TOCFFD
142300961206IF  1C     WFFD          IFGT      *ZEROS
142400961206     C     100           MULT      WFFD          $NUM
142500961206     C     $NUM          DIV       WTBL          TOPFFD
142600961206E   1C                   ENDIF
142700961206     C*
142800961206     C                   Z-ADD     WSMK          TOCSMK
142900961206IF  1C     WSMK          IFGT      *ZEROS
143000961206     C     100           MULT      WSMK          $NUM
143100961206     C     $NUM          DIV       WTBL          TOPSMK
143200961206E   1C                   ENDIF
143300961206     C*
143400961206     C                   Z-ADD     WAPP          TOCAPP
143500961206IF  1C     WAPP          IFGT      *ZEROS
143600961206     C     100           MULT      WAPP          $NUM
143700961206     C     $NUM          DIV       WTBL          TOPAPP
143800961206E   1C                   ENDIF
143900961206     C*
144000961206     C                   Z-ADD     WRIT          TOCRIT
144100961206IF  1C     WRIT          IFGT      *ZEROS
144200961206     C     100           MULT      WRIT          $NUM
144300961206     C     $NUM          DIV       WTBL          TOPRIT
144400961206E   1C                   ENDIF
144500961206     C*
144600961206     C                   Z-ADD     WRI2          TOCRI2
144700961206IF  1C     WRI2          IFGT      *ZEROS
144800961206     C     100           MULT      WRI2          $NUM
144900961206     C     $NUM          DIV       WTBL          TOPRI2
145000961206E   1C                   ENDIF
145100961206     C*
145200961206     C                   Z-ADD     WCAS          TOCCAS
145300961206IF  1C     WCAS          IFGT      *ZEROS
145400961206     C     100           MULT      WCAS          $NUM
145500961206     C     $NUM          DIV       WTBL          TOPCAS
145600961206E   1C                   ENDIF
145700961206     C*
145800961206     C                   Z-ADD     WTCT          TOCTCT
145900961206IF  1C     WTCT          IFGT      *ZEROS
146000961206     C     100           MULT      WTCT          $NUM
146100961206     C     $NUM          DIV       WTBL          TOPTCT
146200961206E   1C                   ENDIF
146300961206     C*
146400961206     C                   Z-ADD     WTFD          TOCTFD
146500961206IF  1C     WTFD          IFGT      *ZEROS
146600961206     C     100           MULT      WTFD          $NUM
146700961206     C     $NUM          DIV       WTBL          TOPTFD
146800961206E   1C                   ENDIF
146900961206     C*
147000961206     C                   Z-ADD     WTIT          TOCTIT
147100961206IF  1C     WTIT          IFGT      *ZEROS
147200961206     C     100           MULT      WTIT          $NUM
147300961206     C     $NUM          DIV       WTBL          TOPTIT
147400961206E   1C                   ENDIF
147500961206     C*
147600961206     C                   Z-ADD     WTI2          TOCTI2
147700961206IF  1C     WTI2          IFGT      *ZEROS
147800961206     C     100           MULT      WTI2          $NUM
147900961206     C     $NUM          DIV       WTBL          TOPTI2
148000961206E   1C                   ENDIF
148100961206     C*
148200961206     C                   Z-ADD     WTTC          TOCTTC
148300961206IF  1C     WTTC          IFGT      *ZEROS
148400961206     C     100           MULT      WTTC          $NUM
148500961206     C     $NUM          DIV       WTBL          TOPTTC
148600961206E   1C                   ENDIF
148700961206     C*
148800961206     C                   Z-ADD     WTPT          TOCTPT
148900961206IF  1C     WTPT          IFGT      *ZEROS
149000961206     C     100           MULT      WTPT          $NUM
149100961206     C     $NUM          DIV       WTBL          TOPTPT
149200961206E   1C                   ENDIF
149300961206     C*
149400961206     C                   Z-ADD     WTPM          TOCTPM
149500961206IF  1C     WTPM          IFGT      *ZEROS
149600961206     C     100           MULT      WTPM          $NUM
149700961206     C     $NUM          DIV       WTBL          TOPTPM
149800961206E   1C                   ENDIF
149900961206     C*
150000961206     C                   Z-ADD     WTDT          TOCTDT
150100961206IF  1C     WTDT          IFGT      *ZEROS
150200961206     C     100           MULT      WTDT          $NUM
150300961206     C     $NUM          DIV       WTBL          TOPTDT
150400961206E   1C                   ENDIF
150500961206     C*
150600961206     C                   Z-ADD     WTEG          TOCTEG
150700961206IF  1C     WTEG          IFGT      *ZEROS
150800961206     C     100           MULT      WTEG          $NUM
150900961206     C     $NUM          DIV       WTBL          TOPTEG
151000961206E   1C                   ENDIF
151100970115     C*
151200970115     C                   Z-ADD     WTPG          TOCTPG
151300970115IF  1C     WTPG          IFGT      *ZEROS
151400970115     C     100           MULT      WTPG          $NUM
151500970115     C     $NUM          DIV       WTBL          TOPTPG
151600970115E   1C                   ENDIF
151700961206     C*
151800961206     C                   WRITE     SD88TO                                       *TOTALI
151900961206     C*
152000961206     C                   ENDSR
152100961206     C*------------------------------------------------------------------------*
152200961206     C* STAPAR - STAMPA I PARAMETRI DI LANCIO
152300961206     C*------------------------------------------------------------------------*
152400961206     C     STAPAR        BEGSR
152500961206     C***
152600961206     C* LINEA DI ARRIVO
152700961206     C***
152800961206IF  1C     PARLNA        IFGT      *ZEROS
152900961206     C                   Z-ADD     PARLNA        PACLNA
153000961206     C                   SETOFF                                           28
153100961206     C                   Z-ADD     1             J
153200961206     C     PARLNA        LOOKUP    LNA(J)                                 28
153300961206IF  2C     *IN28         IFEQ      *ON
153400961206     C                   MOVEL     DLNA(J)       PADLNA
153500961206X   2C                   ELSE
153600961206     C                   MOVEL     *ALL'*'       PADLNA
153700961206E   2C                   ENDIF
153800961206X   1C                   ELSE
153900961206     C                   MOVEL     'TUTTE'       PADLNA
154000961206E   1C                   ENDIF
154100961206     C***
154200961206     C* DATA SPEDIZIONE DAL - AL
154300961206     C***
154400961206     C                   Z-ADD     PARDTI        G08INV
154500961206     C                   Z-ADD     *ZEROS        G08DAT
154600961206     C                   MOVEL     '3'           G08ERR
154700961206     C                   CALL      'XSRDA8'
154800961206     C                   PARM                    WLBDA8
154900961206     C                   Z-ADD     G08DAT        PACDTI
155000961206     C*
155100961206     C                   Z-ADD     PARDTF        G08INV
155200961206     C                   Z-ADD     *ZEROS        G08DAT
155300961206     C                   MOVEL     '3'           G08ERR
155400961206     C                   CALL      'XSRDA8'
155500961206     C                   PARM                    WLBDA8
155600961206     C                   Z-ADD     G08DAT        PACDTF
155700961206     C***
155800961206     C* DETTAGLIO BOLLE
155900961206     C***
156000961206IF  1C     PARDET        IFEQ      'S'
156100961206     C                   MOVEL     'SI'          PADDET
156200961206X   1C                   ELSE
156300961206     C                   MOVEL     'NO'          PADDET
156400961206E   1C                   ENDIF
156500970916     C***
156600970916     C* CAUSALI IN DETTAGLIO
156700970916     C***
156800970916     C                   MOVEL     PARCD1        PACCD1
156900970916     C                   MOVEL     PARCD2        PACCD2
157000970916     C                   MOVEL     PARCD3        PACCD3
157100970916     C                   MOVEL     PARCD4        PACCD4
157200970916     C                   MOVEL     PARCD5        PACCD5
157300970916     C                   MOVEL     PARCD6        PACCD6
157400970916     C                   MOVEL     PARCD7        PACCD7
157500970916     C                   MOVEL     PARCD8        PACCD8
157600970916     C                   MOVEL     PARCD9        PACCD9
157700970916     C                   MOVEL     PARCD0        PACCD0
157800970916     C                   MOVEL     PARCDA        PACCDA
157900970916     C                   MOVEL     PARCDB        PACCDB
158000970916     C                   MOVEL     PARCDC        PACCDC
158100970916     C                   MOVEL     PARCDD        PACCDD
158200970916     C                   MOVEL     PARCDE        PACCDE
158300970916     C                   MOVEL     PARCDF        PACCDF
158400970916     C                   MOVEL     PARCDG        PACCDG
158500970916     C                   MOVEL     PARCDH        PACCDH
158600970916     C                   MOVEL     PARCDI        PACCDI
158700970916     C                   MOVEL     PARCDL        PACCDL
158800961206     C*
158900961206     C                   WRITE     SD88TE                                       *TESTATA
159000961206     C                   WRITE     SD88PA                                       *PARAMETRI
159100961206     C* REIMPOSTA OVERFLOW
159200961206     C                   MOVEL     '1'           *IN01
159300961206     C*
159400961206     C                   ENDSR
159500961206     C*------------------------------------------------------------------------*
159600961206     C* CARTAB - CARICA LE TABELLE OCCORRENTI
159700961206     C*------------------------------------------------------------------------*
159800961206     C     CARTAB        BEGSR
159900961206     C***
160000961206     C* CAUSALE EVENTI
160100961206     C***
160200020717     C                   Z-ADD     1             KTBKUT                         *CODICE UTENTE
160300961206     C                   MOVEL     '2A'          KTBCOD                         *CODICE TABELLA
160400970916     C                   Z-ADD     *ZEROS        I                 4 0
160500961206     C     KEYTA2        SETLL     TABEL00F
160600961206     C     KEYTA2        READE     TABEL00F                               99
160700961206DO  1C     *IN99         DOWEQ     *OFF
160800961206IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATO
160900961206     C                   MOVEL     TBLUNI        DS2A
161000961206     C*
161100961206     C* CARICA SOLO GLI EVENTI DI TIPO GIACENZA (GIACENZA E LASCIATO AVVISO)
161200961206IF  3C     §2ATIP        IFEQ      'G'
161300961206     C                   ADD       1             I
161400961206     C                   MOVEL     TBLKEY        CEV(I)                         *CODICE EVENTO
161500961206     C                   MOVEL     §2AFTC        TEV(I)                         *TIPO EVENTO (G/A)
161600961206E   3C                   ENDIF
161700961206E   2C                   ENDIF
161800961206     C     KEYTA2        READE     TABEL00F                               99
161900961206E   1C                   ENDDO
162000971002     C***
162100971002     C* PARTICOLARITA' GIACENZE
162200971002     C***
162300971002     C                   Z-ADD     *ZEROS        I                 4 0
162400020717     C                   Z-ADD     1             KTBKUT                         *CODICE UTENTE
162500971002     C                   MOVEL     '7Q'          KTBCOD                         *CODICE TABELLA
162600971002     C     KEYTA2        SETLL     TABEL00F
162700971002     C     KEYTA2        READE     TABEL00F                               99
162800971002DO  1C     *IN99         DOWEQ     *OFF
162900971002IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATO
163000971002     C                   MOVEL     TBLUNI        DS7Q
163100971002     C                   ADD       1             I
163200971002     C                   MOVEL     TBLKEY        GGA(I)                         *CODICE
163300971002     C                   MOVEL     §7Q2MF        G2MF(I)                        *FASE INVIO MESSAGGI
163400971002     C                   MOVEL     §7Q2MQ        G2MQ(I)                        *QUANDO INVIO
163500971002     C                   MOVEL     §7Q5DI        G5DI(I)                        *CODICE DISPOSIZIONE
163600971002     C                   MOVEL     §7Q4D1        G4D1(I)                        *COD DISPOSIZIONE 1
163700971002     C                   MOVEL     §7Q4D2        G4D2(I)                        *COD DISPOSIZIONE 1
163800971002     C                   MOVEL     §7Q4D3        G4D3(I)                        *COD DISPOSIZIONE 1
163900971002     C                   MOVEL     §7Q4D4        G4D4(I)                        *COD DISPOSIZIONE 1
164000971002     C                   MOVEL     §7Q4D5        G4D5(I)                        *COD DISPOSIZIONE 1
164100971002E   2C                   ENDIF
164200971002     C     KEYTA2        READE     TABEL00F                               99
164300971002E   1C                   ENDDO
164400961206     C***
164500961206     C* SOGLIE ARRIVO TRAZIONE
164600961206     C***
164700961206     C* NB: SE C'E LA 1^ SOGLIA (OSA), C'E' ANCHE LA 2^ SOGLIA (OSA1)
164800961206     C                   Z-ADD     *ZEROS        I
164900020717     C                   Z-ADD     1             KTBKUT                         *CODICE UTENTE
165000961206     C                   MOVEL     '1X'          KTBCOD                         *CODICE TABELLA
165100961206     C     KEYTA2        SETLL     TABEL00F
165200961206     C     KEYTA2        READE     TABEL00F                               99
165300961206DO  1C     *IN99         DOWEQ     *OFF
165400961206     C                   MOVEL     TBLKEY        TABKEY
165500961206IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATO
165600961206     C     TABOSA        ANDEQ     'OSA'                                        *SOGLIA TRAZIONE
165700961206IF  3C     TABFIL        IFEQ      *BLANKS                                      *OSA/1 STANDARD
165800961206     C                   MOVEL     *ZEROS        TABFIL                         *FILIALE = 000
165900961206E   3C                   ENDIF
166000961206     C                   MOVEL     TBLUNI        DS1X
166100961206IF  3C     TABPOS        IFEQ      *BLANKS                                      *1 ^SOGLIA
166200961206     C                   ADD       1             I
166300961206     C                   MOVEL     TABFIL        TRA(I)                         *FILIALE
166400961206     C                   Z-ADD     §1XHHO        OSA(I)                         *ORA SOGLIA
166500961206X   3C                   ELSE                                                   *2^ SOGLIA
166600961206     C                   Z-ADD     §1XHHO        OSA1(I)                        *ORA SOGLIA
166700961206E   3C                   ENDIF
166800961206E   2C                   ENDIF
166900961206     C     KEYTA2        READE     TABEL00F                               99
167000961206E   1C                   ENDDO
167100961206     C*
167200961206     C                   ENDSR
167300961206     C*------------------------------------------------------------------------*
167400961206     C* IMPSTA - IMPOSTAZIONI STAMPA
167500961206     C*------------------------------------------------------------------------*
167600961206     C     IMPSTA        BEGSR
167700961206     C***
167800961206     C* RIEMPE LA SCHIERA DELLA/E FILIALE/I DA ELABORARE :
167900961206     C*   - SE E' STATA SELEZIONATA UNA FILIALE -->
168000961206     C*        . ELABORA SOLAMENTE LA FILALE SELEZIONATA
168100961206     C*   - SE NON E' STATA SELEZIONATA ALCUNA FILIALE -->
168200020415     C*        . SE 1° LIVELLO --> ELABORA LE FILIALI PRESENTI IN "£1"
168300961206     C*        . SE 2° LIVELLO --> ELABORA LE FILIALI PRESENTI IN "£6"
168400961206     C***
168500961206IF  1C     PARLNA        IFNE      *ZEROS                                       *SELEZ. UNA FILIALE
168600961206     C                   Z-ADD     PARLNA        LNA(1)
168700961206X   1C                   ELSE                                                   *NESSUNA SELEZIONE
168800961206     C* 1° LIVELLO
168900020717IF  2C     Dutlpo        IFEQ      '1'
169000961206     C                   CLEAR                   DSUL06
169100020415     C                   MOVEL     '£1'          D06COD                         *CODICE TABELLA
169200020717     C                   MOVEL     dutpou        D06KEY                         *FILIALE ELABORATORE
169300961206     C                   MOVEL     DSUL06        KPJBU
169400961206     C                   CALL      'TRUL06R'
169500961206     C                   PARM                    KPJBA
169600961206     C                   MOVEL     KPJBU         DSUL06
169700961206     C                   MOVEA     LIN           LNA
169800961206     C* 2° LIVELLO
169900961206X   2C                   ELSE
170000961206     C                   CLEAR                   DSUL06
170100961206     C                   MOVEL     '£6'          D06COD                         *CODICE TABELLA
170200020717     C                   MOVEL     dutpou        D06KEY                         *FILIALE ELABORATORE
170300961206     C                   MOVEL     DSUL06        KPJBU
170400961206     C                   CALL      'TRUL06R'
170500961206     C                   PARM                    KPJBA
170600961206     C                   MOVEL     KPJBU         DSUL06
170700961206     C                   MOVEA     LIN           LNA
170800961206E   2C                   ENDIF
170900961206E   1C                   ENDIF
171000961206     C* DECODIFICA FILIALI
171100961206DO  1C     1             DO        30            J                 4 0
171200961206IF  2C     LNA(J)        IFGT      *ZEROS
171300961206     C                   Z-ADD     LNA(J)        KORFIL
171400961206     C     KEYORG        CHAIN     AZORG01L                           99
171500961206IF  3C     *IN99         IFEQ      *OFF
171600020627     C                   MOVEL     ORGDE3        OG143
171700961206     C                   MOVEL     ORGDES        DLNA(J)
171800020702     C*
171900020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
172000020702     C                   CLEAR                   DNTW
172100020702     C*
172200020702     C                   MOVEL(P)  'NTW'         KTECOD
172300020702     C                   MOVEL(P)  §OGNTW        KTEKE1
172400020702     C     KTBE          CHAIN     TNTBE01L
172500020702     C                   IF        %found(TNTBE01L)
172600020702     C                   MOVEL     TBEUNI        DNTW
172700020702     C                   ENDIF
172800020702     C*
172900020702     C                   MOVEL     §NTWFIE       LFL1(J)
173300020627     C*
173400961206X   3C                   ELSE
173500961206     C                   MOVEL     *ALL'*'       DLNA(J)
173600971204     C                   MOVEL     *BLANKS       LFL1(J)
173700961206E   3C                   ENDIF
173800961206E   2C                   ENDIF
173900961206E   1C                   ENDDO
174000010411     C*
174100961206     C                   ENDSR
174200010411     C*------------------------------------------------------------------------*
174300010411     C* CHKPAR - CONSIDERAZIONI SU PARAMETRI RICEVUTI IN INPUT
174400010411     C*------------------------------------------------------------------------*
174500010411     C     CHKPAR        BEGSR
174600010411     C*
174700010411     C* VERIFICO IL PASSAGGIO PARAMETRI PER STABILIRE SE E' UN LANCIO DA VIDEO
174800010411     C     KPJBU         IFNE      *BLANKS
174900010411     C                   MOVEL     KPJBU         PARAM
175000010411     C                   ELSE
175100010411     C* IMPOSTO IL FLAG CHE IDENTIFICA IL TIPO DI LANCIO EFFETTUATO
175200010411     C                   MOVEL     '1'           TIPLAN
175300010411     C* ESEGUE IL COMANDO DI SOVRAPPOSIZIONE DEL PRINTER FILE PER INDIRIZZAMENTO SU CODA DI SEDE
175400010411     C                   Z-ADD     80            LENGH            15 5
175500030923      * per s.i. Filtra201 la coda di stampa deve essere diversa
175600030923     c                   If        knsif = 'FILTRA201 '
175700030923     C                   CALL(e)   'QCMDEXC'
175800030923     C                   PARM                    CMD(3)
175900030923     C                   PARM                    LENGH
176000030923     c                   Else
176100010411     C                   CALL(e)   'QCMDEXC'
176200010411     C                   PARM                    CMD(1)
176300010411     C                   PARM                    LENGH
176400030923     c                   EndIf
176500010411     C* FORZO LA VALORIZZAZIONE DEI PARAMETRI X IL LANCIO AUTOMATICO
176600010411     C                   MOVEL     'N'           PARDET
176700010411     C* REPERISCE LA DATA CORRENTE
176800010411     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
176900010411     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
177000010411     C                   Z-ADD     N8            G08DAT
177100010411     C                   Z-ADD     *ZEROS        G08INV
177200010411     C                   MOVEL     '0'           G08ERR
177300010411     C                   Z-ADD     *ZEROS        G08TGI
177400010411     C                   CALL      'XSRDA8'
177500010411     C                   PARM                    WLBDA8
177600010411     C                   Z-ADD     G08INV        DATCOR            8 0
177700010411     C                   Z-ADD     G08INV        PARDTF
177800010411     C* TRASFORMA LA DATA IN GIORNI
177900010411     C                   Z-ADD     DATCOR        G08INV
178000010411     C                   Z-ADD     *ZEROS        G08DAT
178100010411     C                   MOVEL     '3'           G08ERR
178200010411     C                   Z-ADD     *ZEROS        G08TGI
178300010411     C                   CALL      'XSRDA8'
178400010411     C                   PARM                    WLBDA8
178500020910     C* TOGLIE 7 GIORNI ALLA DATA CORRENTE E LA RITRASFORMA IN DATA (AAAA/MM/GG)
178600010411     C     G08TGI        SUB       7             G08TGI
178700010411     C                   Z-ADD     *ZEROS        GI8DAT
178800010411     C                   Z-ADD     *ZEROS        GI8INV
178900010411     C                   Z-ADD     G08TGI        GI8TGI
179000010411     C                   CALL      'XSRGI8'
179100010411     C                   PARM                    WGIDA8
179200020910     C                   Z-ADD     GI8DAT        G08DAT
179300020910     C                   Z-ADD     *ZEROS        G08INV
179400020910     C                   MOVEL     '0'           G08ERR
179500020910     C                   Z-ADD     *ZEROS        G08TGI
179600020910     C                   CALL      'XSRDA8'
179700020910     C                   PARM                    WLBDA8
179800020910     C                   Z-ADD     G08INV        PARDTI            8 0
179900010411     C                   ENDIF
180000010411     C*
180100010411     C                   ENDSR
180200961206     C*------------------------------------------------------------------------*
180300961206     C* *INZSR - OPERAZIONI INIZIALI
180400961206     C*------------------------------------------------------------------------*
180500961206     C     *INZSR        BEGSR
180600961206     C*--------------------
180700961206     C* RICEVIMENTO PARAMETRI
180800961206     C*--------------------
180900961206     C     *ENTRY        PLIST
181000961206     C                   PARM                    KPJBA
181100961206     C*--------------------
181200961206     C* CAMPI RIFERITI AL DATA BASE
181300961206     C*--------------------
181400961206     C     *LIKE         DEFINE    ARBLNA        KARLNA                         *FNARB00F
181500961206     C     *LIKE         DEFINE    ARBAAS        KARAAS
181600961206     C     *LIKE         DEFINE    ARBMGS        KARMGS
181700051115     C     *LIKE         DEFINE    AR9AAS        KA9AAS                         *FiAR900F
181800961206     C     *LIKE         DEFINE    AR9LNP        KA9LNP
181900961206     C     *LIKE         DEFINE    AR9NRS        KA9NRS
182000961206     C     *LIKE         DEFINE    AR9NSP        KA9NSP
182100961206     C     *LIKE         DEFINE    ARDAAS        KADAAS                         *FNARBD0F
182200961206     C     *LIKE         DEFINE    ARDLNP        KADLNP
182300961206     C     *LIKE         DEFINE    ARDNRS        KADNRS
182400961206     C     *LIKE         DEFINE    ARDNSP        KADNSP
182500961206     C     *LIKE         DEFINE    ARGAAS        KAGAAS                         *FNARBG0F
182600961206     C     *LIKE         DEFINE    ARGLNP        KAGLNP
182700961206     C     *LIKE         DEFINE    ARGNRS        KAGNRS
182800961206     C     *LIKE         DEFINE    ARGNSP        KAGNSP
182900990810     C     *LIKE         DEFINE    EVBAAS        KEVAAS                         *FNEVB00F
183000961206     C     *LIKE         DEFINE    EVBLNP        KEVLNP
183100961206     C     *LIKE         DEFINE    EVBNRS        KEVNRS
183200961206     C     *LIKE         DEFINE    EVBNSP        KEVNSP
183300050310     C     *LIKE         DEFINE    GCPAAS        KGCAAS                         *TIGCP01L
183400961206     C     *LIKE         DEFINE    GCPLNP        KGCLNP
183500961206     C     *LIKE         DEFINE    GCPNRS        KGCNRS
183600961206     C     *LIKE         DEFINE    GCPNSP        KGCNSP
183700961206     C     *LIKE         DEFINE    FVVNPG        KFVNPG                         *FNFVV00F
183800961206     C     *LIKE         DEFINE    FVVNFV        KFVNFV
183900020912     c     *Like         Define    FvvFgs        KfvFgs
184000961206     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
184100961206     C     *LIKE         DEFINE    TBLCOD        KTBCOD
184200961206     C     *LIKE         DEFINE    TBLKEY        KTBKEY
184300961206     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
184400010124     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
184500010124     C     *LIKE         DEFINE    TBEKE1        KTEKE1
184600010124     C     *LIKE         DEFINE    TBEKE1        KTEKE2
184700010124     C     *LIKE         DEFINE    TBELIN        KTELIN
184800010124     C     *LIKE         DEFINE    TBESIF        KTESIF
184900961206     C*--------------------
185000961206     C* CAMPI RIFERITI A CAMPI DEL PGM
185100961206     C*--------------------
185200961206     C     *LIKE         DEFINE    FILTBL        GENTBL                         *TOTALI GENERALI
185300961206     C     *LIKE         DEFINE    FILTCR        GENTCR
185400961206     C     *LIKE         DEFINE    FILLAV        GENLAV
185500961206     C     *LIKE         DEFINE    FILLEG        GENLEG
185600961206     C     *LIKE         DEFINE    FILGCA        GENGCA
185700961206     C     *LIKE         DEFINE    FILFFD        GENFFD
185800961206     C     *LIKE         DEFINE    FILSMK        GENSMK
185900961206     C     *LIKE         DEFINE    FILAPP        GENAPP
186000961206     C     *LIKE         DEFINE    FILRIT        GENRIT
186100961206     C     *LIKE         DEFINE    FILRI2        GENRI2
186200961206     C     *LIKE         DEFINE    FILCAS        GENCAS
186300961206     C     *LIKE         DEFINE    FILTCT        GENTCT
186400961206     C     *LIKE         DEFINE    FILTIT        GENTIT
186500961206     C     *LIKE         DEFINE    FILTI2        GENTI2
186600961206     C     *LIKE         DEFINE    FILTTC        GENTTC
186700961206     C     *LIKE         DEFINE    FILTFD        GENTFD
186800961206     C     *LIKE         DEFINE    FILTPT        GENTPT
186900961206     C     *LIKE         DEFINE    FILTPM        GENTPM
187000961206     C     *LIKE         DEFINE    FILTDT        GENTDT
187100961206     C     *LIKE         DEFINE    FILTEG        GENTEG
187200970115     C     *LIKE         DEFINE    FILTPG        GENTPG
187300961206     C     *LIKE         DEFINE    FILTBL        WTBL                           *DI LAVORO
187400961206     C     *LIKE         DEFINE    FILTCR        WTCR
187500961206     C     *LIKE         DEFINE    FILLAV        WLAV
187600961206     C     *LIKE         DEFINE    FILLEG        WLEG
187700961206     C     *LIKE         DEFINE    FILGCA        WGCA
187800961206     C     *LIKE         DEFINE    FILFFD        WFFD
187900961206     C     *LIKE         DEFINE    FILSMK        WSMK
188000961206     C     *LIKE         DEFINE    FILAPP        WAPP
188100961206     C     *LIKE         DEFINE    FILRIT        WRIT
188200961206     C     *LIKE         DEFINE    FILRI2        WRI2
188300961206     C     *LIKE         DEFINE    FILCAS        WCAS
188400961206     C     *LIKE         DEFINE    FILTCT        WTCT
188500961206     C     *LIKE         DEFINE    FILTIT        WTIT
188600961206     C     *LIKE         DEFINE    FILTI2        WTI2
188700961206     C     *LIKE         DEFINE    FILTTC        WTTC
188800961206     C     *LIKE         DEFINE    FILTFD        WTFD
188900961206     C     *LIKE         DEFINE    FILTPT        WTPT
189000961206     C     *LIKE         DEFINE    FILTPM        WTPM
189100961206     C     *LIKE         DEFINE    FILTDT        WTDT
189200961206     C     *LIKE         DEFINE    FILTEG        WTEG
189300970115     C     *LIKE         DEFINE    FILTPG        WTPG
189400961206     C*--------------------
189500961206     C* CHIAVI DI LETTURA
189600961206     C*--------------------
189700961206     C* FNARB26L - PARZIALE
189800961206     C     KEYARB        KLIST
189900961206     C                   KFLD                    KARLNA                         *LINEA DI ARRIVO
190000961206     C                   KFLD                    KARAAS                         *ANNO SPEDIZIONE
190100961206     C                   KFLD                    KARMGS                         *MESE/GIORNO SPEDIZ.
190200051115     C* FiAR901L - COMPLETA
190300961206     C     KEYAR9        KLIST
190400961206     C                   KFLD                    KA9AAS                         *ANNO SPEDIZIONE
190500961206     C                   KFLD                    KA9LNP                         *LINEA DI PARTENZA
190600961206     C                   KFLD                    KA9NRS                         *SERIE
190700961206     C                   KFLD                    KA9NSP                         *SPEDIZIONE
190800131014     C* FIARBF2C / FNARBD0F - PARZIALE
190900961206     C     KEYARD        KLIST
191000961206     C                   KFLD                    KADAAS                         *ANNO SPEDIZIONE
191100961206     C                   KFLD                    KADLNP                         *LINEA DI PARTENZA
191200961206     C                   KFLD                    KADNRS                         *SERIE
191300961206     C                   KFLD                    KADNSP                         *SPEDIZIONE
191400131014     C* FIARBF2C / FNARBG0F - PARZIALE
191500961206     C     KEYARG        KLIST
191600961206     C                   KFLD                    KAGAAS                         *ANNO SPEDIZIONE
191700961206     C                   KFLD                    KAGLNP                         *LINEA DI PARTENZA
191800961206     C                   KFLD                    KAGNRS                         *SERIE
191900961206     C                   KFLD                    KAGNSP                         *SPEDIZIONE
192000990810     C* FNEVB01L - PARZIALE
192100961206     C     KEYEVB        KLIST
192200990810     C                   KFLD                    KEVAAS                         *ANNO SPEDIZIONE
192300961206     C                   KFLD                    KEVLNP                         *LINEA DI PARTENZA
192400961206     C                   KFLD                    KEVNRS                         *SERIE
192500961206     C                   KFLD                    KEVNSP                         *SPEDIZIONE
192600050310     C* TIGCP01L - PARZIALE
192700050310     C     KEYGCP        KLIST
192800961206     C                   KFLD                    KGCAAS                         *ANNO SPEDIZIONE
192900961206     C                   KFLD                    KGCLNP                         *LINEA DI PARTENZA
193000961206     C                   KFLD                    KGCNRS                         *SERIE
193100961206     C                   KFLD                    KGCNSP                         *SPEDIZIONE
193200961206     C* FNFVV01L - PARZIALE
193300961206     C     KEYFVV        KLIST
193400961206     C                   KFLD                    KFVNPG                         *CATEGORIA FOGLIO
193500961206     C                   KFLD                    KFVNFV                         *NUMERO FOGLIO
193600020912     c                   Kfld                    kfvfgs                          p.o. gestione
193700961206     C* TABEL00F - COMPLETA
193800961206     C     KEYTAB        KLIST
193900961206     C                   KFLD                    KTBKUT                         *UTENTE
194000961206     C                   KFLD                    KTBCOD                         *CODICE TABELLA
194100961206     C                   KFLD                    KTBKEY                         *RECORD TABELLA
194200961206     C* TABEL00F - PARZIALE
194300961206     C     KEYTA2        KLIST
194400961206     C                   KFLD                    KTBKUT                         *UTENTE
194500961206     C                   KFLD                    KTBCOD                         *CODICE TABELLA
194600010124     C* TNTBE01L - COMPLETA
194700010124     C     KEYTBE        KLIST
194800010124     C                   KFLD                    KTECOD                         *CODICE TABELLA
194900010124     C                   KFLD                    KTEKE1                         *CHIAVE TABELLA 1
195000010124     C                   KFLD                    KTEKE2                         *CHIAVE TABELLA 2
195100010124     C                   KFLD                    KTELIN                         *LINGUA TABELLA
195200010124     C                   KFLD                    KTESIF                         *SISTEMA INFORMATIVO
195300010822     C* TNTBE01L - PARZIALE
195400010822     C     KTBE          KLIST
195500010822     C                   KFLD                    KTECOD                         *CODICE TABELLA
195600010822     C                   KFLD                    KTEKE1                         *CHIAVE TABELLA 1
195700010822     C                   KFLD                    KTEKE2                         *CHIAVE TABELLA 2
195800961206     C* AZORG01L - COMPLETA
195900961206     C     KEYORG        KLIST
196000961206     C                   KFLD                    KORFIL                         *FILIALE
196100961206     C*--------------------
196200961206     C* CAMPI DI CONTROLLO
196300961206     C*--------------------
196400961206     C                   MOVEL     '0'           $FINE             1            *FINE CICLO
196500961206     C                   MOVEL     'N'           WRECOK            1            *RECORD NON/VALIDO
196600961206     C                   MOVEL     '0'           $MEMF             1            *MEMORIZZATO QC X FI
196700961206     C                   MOVEL     '0'           $MEMG             1            *MEMORIZZATO QC GENE
196800970902     C                   MOVEL     '0'           $CR               1            *MEMORIZZA PARTICOLA
196900970902     C                   MOVEL     '0'           $LG               1
197000970902     C                   MOVEL     '0'           $FD               1
197100970902     C                   MOVEL     '0'           $TC               1
197200970902     C                   MOVEL     '0'           $GC               1
197300970902     C                   MOVEL     '0'           $EG               1
197400970902     C                   MOVEL     '0'           $PG               1
197500970902     C                   MOVEL     '0'           §TCR              1
197600970901     C                   MOVEL     '0'           §LAV              1
197700970901     C                   MOVEL     '0'           §LEG              1
197800970901     C                   MOVEL     '0'           §GCA              1
197900970901     C                   MOVEL     '0'           §FFD              1
198000970901     C                   MOVEL     '0'           §SMK              1
198100970901     C                   MOVEL     '0'           §APP              1
198200970901     C                   MOVEL     '0'           §RIT              1
198300970901     C                   MOVEL     '0'           §RI2              1
198400970901     C                   MOVEL     '0'           §CAS              1
198500970901     C                   MOVEL     '0'           §TCT              1
198600970901     C                   MOVEL     '0'           §TFD              1
198700970901     C                   MOVEL     '0'           §TIT              1
198800970901     C                   MOVEL     '0'           §TI2              1
198900970901     C                   MOVEL     '0'           §TTC              1
199000970901     C                   MOVEL     '0'           §TPT              1
199100970901     C                   MOVEL     '0'           §TPM              1
199200970901     C                   MOVEL     '0'           §TDT              1
199300970901     C                   MOVEL     '0'           §TEG              1
199400970901     C                   MOVEL     '0'           §TPG              1
199500970916     C                   MOVEL     'N'           WSTDET            1            *STAMPA DETTAGLIO
199600011211     C*                  MOVEL     'N'           WABATM            1            *ABILITAZ.AUTOM. C/A
199700971003     C                   MOVEL     'N'           WGIACV            1            *GIACENZA
199800971003     C                   Z-ADD     *ZEROS        $TOTST            4 0          *N° TOTALI STAMPATI
199900961206     C                   MOVEL     '1'           *IN01                          *OVERFLOW
200000971009     C                   MOVEL     '0'           WSD99             1            *LANCIO TNSD99R
200100010126     C                   MOVEL     '0'           TIPLAN            1            *TIPO LANCIO
200200961206     C*
200300961206     C* RECUPERA I DATI DELLA FILIALE
200400020717
200500020717     c     *dtaara       define    §azute        azuteds
200600020717     c     *dtaara       define    §datiute      ddatiute
200700020717
200800020717     c                   in(E)     *dtaara
200900020717     c                   if        %Error or rsut = *blanks
201000020717     c                   clear                   tibs34ds
201100020717     c                   call      'TIBS34R'
201200020717     c                   parm                    Tibs34Ds
201300020717     c                   in        *dtaara
201400020717     c                   endif
201500010411     C*
201600010411     C* CONSIDERAZIONI SU PARAMETRI RICEVUTI IN INPUT
201700010411     C                   EXSR      CHKPAR
201800961206     C*
201900961206     C* CARICA LE TABELLE OCCORRENTI
202000961206     C                   EXSR      CARTAB
202100961206     C*
202200961206     C* IMPOSTAZIONI STAMPA
202300961206     C                   EXSR      IMPSTA
202400961206     C*
202500961206     C                   ENDSR
202600961206     O*------------------------------------------------------------------------*
202700961206     O* DECODIFICHE IN STAMPA
202800961206     O*------------------------------------------------------------------------*
202900010124**  -DEC-
203000970902P.OPERATIVO         1
203100961206GENERALE            2
2032009712043
2033009612064
2034009612065
203500010124** CMD - COMANDI CLP
203600010125OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TNSD88P) OUTQ(SETRAS)
203700010124DLTOVR FILE(TNSD88P) LVL(*)
203800030923OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TNSD88P) OUTQ(P046179S)
203900010124
204000010124
