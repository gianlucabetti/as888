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
002000991124     FFIARBF1C  IF   E           K DISK
002100961206     F                                     IGNORE(FNARBK00)
002200991124     F                                     IGNORE(FIARBT00)
002300961206     F                                     IGNORE(FNARBM00)
002400961206     F                                     IGNORE(FNARBV00)
002500961206     F*-------------------
002600961206     F* EVENTI BOLLE --> AAS(2)+LNP+NRS+NSP+CEV+DEV+HEV
002700961206     F*-------------------
002800990810     FFNEVB01L  IF   E           K DISK
002900961206     F*-------------------
003000961206     F* GIACENZE ARRIVO --> AAS+LNP+NRS+NSP
003100961206     F*-------------------
003200050310     FTIGCP01L  IF   E           K DISK
003300961206     F*-------------------
003400961206     F* FOGLI VARI --> NPG+NFV+FGS
003500961206     F*-------------------
003600961206     FFNFVV01L  IF   E           K DISK
003700961206     FTABEL00F  IF   E           K DISK
003800961206     FAZORG01L  IF   E           K DISK
003900010124     FTNSD88P   O    E             PRINTER OFLIND(*IN01)
004000010124     F                                     USROPN
004100020910     FTNTBE01L  IF   E           K DISK
004200961206     D*------------------------------------------------------------------------*
004300961206     D* SCHIERE
004400961206     D*------------------------------------------------------------------------*
004500961206     D*-------------------
004600961206     D* DECODIFICHE IN STAMPA
004700961206     D*-------------------
004800991124     D DEC             S             20    DIM(5) CTDATA PERRCD(1)
004900961206     D*-------------------
005000961206     D* FILIALI DA ELABORARE
005100961206     D*-------------------
005200961206     D LNA             S              3  0 DIM(30)                              CODICI
005300961206     D DLNA            S             20    DIM(30)                              DESCRIZIONI
005400971204     D LFL1            S              1    DIM(30)                              FLAG ITALIA/ESTERO
005500961206     D*-------------------
005600961206     D* CAUSALE EVENTI
005700961206     D*-------------------
005800080903     D CEV             S              3    DIM(200)                             CODICI
005900080903     D TEV             S              1    DIM(200)                             TIPI EVENTO (GIAC,LA
006000971002     D*-------------------
006100971002     D* PARTICOLARITA' GIACENZE
006200971002     D*-------------------
006300971002     D GGA             S              2    DIM(100)                             *CODICI
006400971002     D G2MF            S              1    DIM(100)                             *FASE INVIO MESSAGGI
006500971002     D G2MQ            S              1    DIM(100)                             *QUANDO INVIO
006600971002     D G5DI            S              1    DIM(100)                             *CODICE DISPOSIZIONE
006700971002     D G4D1            S              1    DIM(100)                             *CODICE DISPOSIZIONE
006800971002     D G4D2            S              1    DIM(100)                             *CODICE DISPOSIZIONE
006900971002     D G4D3            S              1    DIM(100)                             *CODICE DISPOSIZIONE
007000971002     D G4D4            S              1    DIM(100)                             *CODICE DISPOSIZIONE
007100971002     D G4D5            S              1    DIM(100)                             *CODICE DISPOSIZIONE
007200961206     D*-------------------
007300961206     D* TEMPI FISSI ARRIVO TRAZIONE
007400961206     D*-------------------
007500080513     d c_dim_1X        c                   const(500)
007600080513     D TRA             S              3  0 DIM(c_dim_1X)                        *TRAZIONI
007700080513     D OSA             S              4  0 DIM(c_dim_1X)                        *1^ SOGLIA ARRIVO TR
007800080513     D OSA1            S              4  0 DIM(c_dim_1X)                        *2^ SOGLIA ARRIVO TR
007900970902     D*-------------------
008000970902     D* CAUSALI STAMPA DETTAGLIO BOLLE
008100970902     D*-------------------
008200970902     D CSTM            S              1    DIM(20)
008300970916     D*-------------------
008400970916     D* DI LAVORO
008500970916     D*-------------------
008600970916     D SA1             S              1    DIM(20)                              *SCHIERA ALFANUMERIC
008700010124     D*-------------------
008800010124     D* COMANDI
008900010124     D*-------------------
009000010124     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
009100961206     D*------------------
009200961206     D* ARCHITETTURA
009300961206     D*------------------
009400961206     D KPJBA         E DS
009500961206     D*------------------
009600961206     D* DS "TRUL06R" - CARICAMENTO TABELLE "£X"
009700961206     D*------------------
009800961206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009900961206     D  LIN                    1     90  0
010000961206     D                                     DIM(30)                              DI COMODO
010100961206     D*------------------
010200971009     D* DS "TNSD99R" - CAOCLO TEMPI DI TRAZIONI INTERNI
010300961206     D*------------------
010400971009     D DS99          E DS                  EXTNAME(TNSD99DS)
010500961206     D*------------------
010600961206     D* DS CAUSALI EVENTI
010700961206     D*------------------
010800961206     D DS2A          E DS
010900971002     D*------------------
011000971002     D* DS PARTICOLARITA' GIACENZE
011100971002     D*------------------
011200971002     D DS7Q          E DS
011300961206     D*------------------
011400961206     D* DS TEMPI FISSI DI DELIVERY
011500961206     D*------------------
011600961206     D DS1X          E DS
011700961206     D*------------------
011800961206     D* DS TIPO INCASSO C/ASSEGNO
011900961206     D*------------------
012000961206     D DS1A          E DS
012100961206     D*------------------
012200961206     D* DS DIVISE E CAMBI
012300961206     D*------------------
012400961206     D DSCV          E DS
012401130828     D*------------------
012402130828     D* DS Codici bolla
012403130828     D*------------------
012404130828     d ds3a          e ds
012500020717
012600020717     d DdatiUte      e ds
012700020717     d AzuteDs       e ds                  ExtName(Azute00f)
012800020717     d Tibs34Ds      e ds                  Inz
012900020717
013000961206     D*------------------
013100961206     D* DS SOGLIA DELIVERY
013200961206     D*------------------
013300961206     D                 DS
013400961206     D  TABFIL                 1      3                                         FILIAL
013500961206     D  TABOSA                 4      6                                         "OSA"
013600961206     D  TABPOS                 7      7                                         ' '/1
013700961206     D  TABKEY                 1      7
013800961206     D*------------------
013900961206     D* DS CONTROLLA DATA (8)
014000961206     D*------------------
014100961206     D WLBDA8          DS                  INZ
014200961206     D  G08DAT                 1      8  0
014300961206     D  G08INV                 9     16  0
014400961206     D  G08ERR                17     17
014500961206     D  G08TGI                18     22  0
014600010124     D*-------------------
014700010124     D* RICALCOLA DATA (8)
014800010124     D*-------------------
014900010124     D WGIDA8          DS
015000010124     D  GI8DAT                 1      8  0
015100010124     D  GI8INV                 9     16  0
015200010124     D  GI8TGI                17     21  0
015300961206     D*------------------
015400961206     D* DS DATA SPEDIZIONE FNARB00F
015500961206     D*------------------
015600961206     D                 DS
015700961206     D  ARBAAS                 1      4  0
015800961206     D  ARBMGS                 5      8  0
015900961206     D  ARBDSP                 1      8  0
016000961206     D*------------------
016100961206     D* PARAMETRI IN INPUT
016200961206     D*------------------
016300010125     D PARAM           DS                  INZ
016400970916     D  PARLNA                 1      3  0
016500970916     D  PARDTI                 4     11  0
016600970916     D  PARAAI                 4      7  0
016700970916     D  PARMGI                 8     11  0
016800970916     D  PARDTF                12     19  0
016900970916     D  PARAAF                12     15  0
017000970916     D  PARMGF                16     19  0
017100970916     D  PARDET                20     20
017200970916     D  PARCD1                21     21
017300970916     D  PARCD2                22     22
017400970916     D  PARCD3                23     23
017500970916     D  PARCD4                24     24
017600970916     D  PARCD5                25     25
017700970916     D  PARCD6                26     26
017800970916     D  PARCD7                27     27
017900970916     D  PARCD8                28     28
018000970916     D  PARCD9                29     29
018100970916     D  PARCD0                30     30
018200970916     D  PARCDA                31     31
018300970916     D  PARCDB                32     32
018400970916     D  PARCDC                33     33
018500970916     D  PARCDD                34     34
018600970916     D  PARCDE                35     35
018700970916     D  PARCDF                36     36
018800970916     D  PARCDG                37     37
018900970916     D  PARCDH                38     38
019000970916     D  PARCDI                39     39
019100970916     D  PARCDL                40     40
019200971003     D  PARCAU                21     40
019300010822     D*------------------
019400010822     D* DS CAMBIO DIVISA
019500010822     D*------------------
019600011211     D*YEURDS        E DS                  EXTNAME(YEURCODS)
019700010822     D*------------------
019800010822     D* DS REPERIMENTO GESTIONE IMPORTI VARI
019900011211     D*------------------
020000011211     D* DGEI          E DS
020100011211     D*------------------
020200011211     D* DS REPERIMENTO IMPORTO MASSIMO CONTRASSEGNO
020300011211     D*------------------
020400011211     D TRUL21DS      E DS
020500020627     D*------------------
020600020627     D* DESCRIZIONE ORGANIGRAMMA
020700020627     D*------------------
020800020627     D OG143         E DS
020900020702     D*------------------
021000020702     D* DS PER LETTURA TABELLA "NTW"
021100020702     D*------------------
021200020702     D DNTW          E DS
021300961206     I*------------------------------------------------------------------------*
021400961206     I* INPUT
021500961206     I*------------------------------------------------------------------------*
021600961206     I*------------------
021700961206     I* RIDENOMINA I CAMPI DELL'ARCHIVIO VARIAZIONI
021800961206     I*------------------
021900961206     IFNARBD00      10
022000961206     I              ARBATB                      ARDATB
022100961206     I              ARBAAS                      ARDAAS
022200961206     I              ARBLNP                      ARDLNP
022300961206     I              ARBNRS                      ARDNRS
022400961206     I              ARBNSP                      ARDNSP
022500961206     I              ARBCBO                      ARDCBO
022600961206     I              ARBDTV                      ARDDTV
022700961206     I              ARBORV                      ARDORV
022800961206     I              ARBCVB                      ARDCVB
022900961206     I              ARBPRU                      ARDPRU
023000961206     I              ARBRSD                      ARDRSD
023100961206     I              ARBRD2                      ARDRD2
023200961206     I              ARBIND                      ARDIND
023300961206     I              ARBCAD                      ARDCAD
023400961206     I              ARBLOD                      ARDLOD
023500961206     I              ARBPRD                      ARDPRD
023600961206     I              ARBNZD                      ARDNZD
023700961206     I              ARBFIN                      ARDFIN
023800961206     I              ARBCPI                      ARDCPI
023900961206     I              ARBPKB                      ARDPKB
024000961206     I              ARBPKF                      ARDPKF
024100961206     I              ARBVLB                      ARDVLB
024200961206     I              ARBFVB                      ARDFVB
024300961206     I              ARBVLF                      ARDVLF
024400961206     I              ARBFVF                      ARDFVF
024500961206     I              ARBFFD                      ARDFFD
024600961206     I              ARBFST                      ARDFST
024700961206     IFNARBG00      20
024800961206     I              ARBATB                      ARGATB
024900961206     I              ARBAAS                      ARGAAS
025000961206     I              ARBLNP                      ARGLNP
025100961206     I              ARBNRS                      ARGNRS
025200961206     I              ARBNSP                      ARGNSP
025300961206     I              ARBCBO                      ARGCBO
025400961206     I              ARBDTV                      ARGDTV
025500961206     I              ARBORV                      ARGORV
025600961206     I              ARBCVB                      ARGCVB
025700961206     I              ARBPRU                      ARGPRU
025800961206     I              ARBTCR                      ARGTCR
025900961206     I              ARBDCR                      ARGDCR
026000961206     I              ARBHCR                      ARGHCR
026100961206     I              ARBGC1                      ARGGC1
026200961206     I              ARBGC2                      ARGGC2
026300961206     I              ARBTC1                      ARGTC1
026400961206     I              ARBTC2                      ARGTC2
026500961206     I              ARBFST                      ARGFST
026600961206     C*------------------------------------------------------------------------*
026700961206     C* MAIN LINE
026800961206     C*------------------------------------------------------------------------*
026900010124     C*
027000010124     C* APRO IL FILE DI STAMPA
027100010124     C                   OPEN      TNSD88P
027200961206     C*
027300961206     C* STAMPA PARAMETRI
027400961206     C                   EXSR      STAPAR
027500961206     C*
027600961206     C* PER CIASCUNA FILIALE CARICATA DA ELABORARE
027700970916     C                   Z-ADD     1             F                 4 0
027800970916DO  1C     F             DOWLE     30
027900970916     C     LNA(F)        ANDGT     *ZEROS
028000961206     C*
028100961206     C* OPERAZIONI PER NUOVA LINEA DI ARRIVO
028200961206     C                   EXSR      NEWLNA
028300961206     C*
028400961206     C* POSIZIONAMENTO E LETTURA BOLLE ARRIVO
028500961206     C                   EXSR      SETFIL
028600961206     C*
028700961206     C* CICLO FINO A ROTTURA LINEA DI ARRIVO
028800961206DO  2C     $FINE         DOWEQ     '0'
028900961206     C*
029000961206     C* MEMORIZZA I TOTALI PER FILIALE
029100961206     C                   EXSR      MEMARB
029200961206     C*
029300961206     C* SE RICHIESTO DETTAGLIO, STAMPA I DATI DELLA BOLLA
029400961206IF  3C     PARDET        IFEQ      'S'
029500961206     C                   EXSR      STADET
029600961206E   3C                   ENDIF
029700961206     C*
029800961206     C* LETTURA SUCESSIVA BOLLE ARRIVO
029900961206     C                   EXSR      LETFIL
030000961206E   2C                   ENDDO
030100961206     C*
030200961206     C* SE MEMORIZZATO QC, STAMPA I TOTALI PER FILIALE
030300961206IF  2C     $MEMF         IFEQ      '1'
030400961206     C                   EXSR      STATOF
030500961206E   2C                   ENDIF
030600961206     C*
030700961206     C* LETTURA SUCESSIVA FILIALI CARICATE
030800970916     C                   ADD       1             F
030900961206E   1C                   ENDDO
031000961206     C*
031100961206     C* SE MEMORIZZATO QC, STAMPA I TOTALI GENERALI
031200961206IF  1C     $MEMG         IFEQ      '1'
031300961206     C                   EXSR      STATOG
031400961206E   1C                   ENDIF
031500961206     C*
031600961206     C                   WRITE     SD88FI                                       *FINE PROSPETTO
031700961206     C*
031800961206     C* CHIUDE I FILES DEI LANCI PGM
031900961206     C                   EXSR      CHIUDE
032000010124     C*
032100010124     C* CHIUDO IL FILE DI STAMPA
032200010124     C                   CLOSE     TNSD88P
032300010124     C*
032400010124     C* ELIMINO TUTTE LE SOVRAPPOSIZIONI EFFETUATE IN PRECEDENZA
032500010126     C                   IF        TIPLAN = '1'
032600010124     C                   Z-ADD     80            LENGH            15 5
032700010125     C                   CALL(e)   'QCMDEXC'
032800010124     C                   PARM                    CMD(2)
032900010124     C                   PARM                    LENGH
033000010126     C                   ENDIF
033100011211     C*
033200961206     C                   SETON                                            LR
033300961206     C*------------------------------------------------------------------------*
033400961206     C* CHIUDE - CHIUDE I FILES DEI LANCI PGM
033500961206     C*------------------------------------------------------------------------*
033600961206     C     CHIUDE        BEGSR
033700961206     C* TRUL06R
033800961206     C                   MOVEL     'C'           D06TLA
033900961206     C                   MOVEL     DSUL06        KPJBU
034000961206     C                   CALL      'TRUL06R'
034100961206     C                   PARM                    KPJBA
034200971009     C* TNSD99R
034300971009IF  1C     WSD99         IFEQ      '1'
034400961206     C                   MOVEL     'C'           D98TLA
034500971009     C                   CALL      'TNSD99R'
034600971009     C                   PARM                    DS99
034700961206E   1C                   ENDIF
034800011211     C*
034900011211     C                   CLEAR                   Trul21DS
035000011211     C                   EVAL      I21Tla = 'C'
035100011211     C                   CALL      'TRUL21R'
035200011211     C                   PARM                    Trul21DS
035300961206     C*
035400961206     C                   ENDSR
035500961206     C*------------------------------------------------------------------------*
035600961206     C* NEWLNA - OPERAZIONI PER NUOVA LINEA DI ARRIVO
035700961206     C*------------------------------------------------------------------------*
035800961206     C     NEWLNA        BEGSR
035900961206     C*
036000961206     C     *LIKE         DEFINE    ARBLNA        DEPLNA
036100971204     C     *LIKE         DEFINE    ORGFL1        DEPFL1
036200961206     C     *LIKE         DEFINE    ARBHTI        DEPOSA
036300961206     C     *LIKE         DEFINE    ARBHTI        DEPOS1
036400961206     C     *LIKE         DEFINE    ORGDES        DECLNA
036500961206     C*
036600961206     C* REIMPOSTA I FLAGS
036700961206     C                   MOVEL     '0'           $FINE                          *FINE CICLO
036800961206     C                   MOVEL     '0'           $MEMF                          *MEMORIZZATO QC
036900961206     C                   MOVEL     '1'           *IN01                          *OVERFLOW
037000961206     C*
037100961206     C* MEMORIZZA LA FILIALE IN UN DEPOSITO
037200970916     C                   Z-ADD     LNA(F)        DEPLNA
037300971204     C*
037400971204     C* DATI FILIALE
037500971204     C                   MOVEL     LFL1(F)       DEPFL1
037600971204     C                   MOVEL     DLNA(F)       DECLNA
037700961206     C*
037800961206     C* REPERISCE LE SOGLIE DI ARRIVO TRAZIONE
037900961206     C                   Z-ADD     1             J
038000961206     C     DEPLNA        LOOKUP    TRA(J)                                 28     *SOGLIE ARRIVO TRAZ
038100961206     C                   Z-ADD     OSA(J)        DEPOSA                          *1^ SOGLIA
038200961206     C                   Z-ADD     OSA1(J)       DEPOS1                          *2^ SOGLIA
038300961206     C*
038400961206     C* AZZERA I TOTALI DI FILIALE
038500961206     C                   Z-ADD     *ZEROS        FILTBL            7 0          *TOT. BOLLE
038600961206     C                   Z-ADD     *ZEROS        FILTCR            7 0          *TOT. CONS. RICHIEST
038700961206     C                   Z-ADD     *ZEROS        FILLAV            7 0            -LASCIATO AVVISO
038800961206     C                   Z-ADD     *ZEROS        FILLEG            7 0            -L.AVVISO+GIACENZA
038900961206     C                   Z-ADD     *ZEROS        FILGCA            7 0            -GIACENZA
039000961206     C                   Z-ADD     *ZEROS        FILFFD            7 0            -FERMO DEPOSITO
039100961206     C                   Z-ADD     *ZEROS        FILSMK            7 0            -SUPERMARKET
039200961206     C                   Z-ADD     *ZEROS        FILAPP            7 0            -APPUNTAMENTO
039300961206     C                   Z-ADD     *ZEROS        FILRIT            7 0            -ARRIVO RITARDO 1^
039400961206     C                   Z-ADD     *ZEROS        FILRI2            7 0            -ARRIVO RITARDO 2^
039500961206     C                   Z-ADD     *ZEROS        FILCAS            7 0            -CONTRASSEGNO ABIL
039600961206     C                   Z-ADD     *ZEROS        FILTCT            7 0          *TOT. CONS.RICH.TOT.
039700961206     C                   Z-ADD     *ZEROS        FILTFD            7 0          *TOT. FERMI DEPOSITI
039800961206     C                   Z-ADD     *ZEROS        FILTIT            7 0          *TOT. ARRIVI RIT. 1^
039900961206     C                   Z-ADD     *ZEROS        FILTI2            7 0          *TOT. ARRIVI RIT. 2^
040000961206     C                   Z-ADD     *ZEROS        FILTTC            7 0          *TOT. TURNI CHIUSURA
040100961206     C                   Z-ADD     *ZEROS        FILTPT            7 0          *TOT. CONS. PRIMA 13
040200961206     C                   Z-ADD     *ZEROS        FILTPM            7 0          *TOT. CONS. X POMERI
040300961206     C                   Z-ADD     *ZEROS        FILTDT            7 0          *TOT. CONS. DOPO 13
040400961206     C                   Z-ADD     *ZEROS        FILTEG            7 0          *TOT. EVEN.SENZA GIA
040500970902     C                   Z-ADD     *ZEROS        FILTPG            7 0          *TOT. + EVENTI GIAC
040600961206     C*
040700961206     C                   ENDSR
040800961206     C*------------------------------------------------------------------------*
040900961206     C* SETFIL - POSIZIONAMENTO E PRIMA LETTRURA RECORD
041000961206     C*------------------------------------------------------------------------*
041100961206     C     SETFIL        BEGSR
041200961206     C*
041300961206     C                   Z-ADD     DEPLNA        KARLNA                         *LINEA DI ARRIVO
041400961206     C                   Z-ADD     PARAAI        KARAAS                         *ANNO INIZIALE
041500961206     C                   Z-ADD     PARMGI        KARMGS                         *MESE INIZIALE
041600961206     C*
041700961206     C     KEYARB        SETLL     FNARB26L                           99        *FINE ARCHIVIO
041800961206     C*
041900961206IF  1C     *IN99         IFEQ      *ON
042000961206     C                   MOVEL     '1'           $FINE
042100961206X   1C                   ELSE
042200961206     C                   EXSR      LETFIL                                       *LETTURA RECORD
042300961206E   1C                   ENDIF
042400961206     C*
042500961206     C                   ENDSR
042600961206     C*------------------------------------------------------------------------*
042700961206     C* LETFIL - LETTURA PROSSIMO RECORD
042800961206     C*------------------------------------------------------------------------*
042900961206     C     LETFIL        BEGSR
043000961206     C*
043100961206     C* LEGGO FINO A:
043200961206     C*     - FINE FILE
043300961206     C*     - TROVATO RECORD VALIDO
043400961206     C                   MOVEL     'N'           WRECOK
043500961206DO  1C     $FINE         DOWEQ     '0'
043600961206     C     WRECOK        ANDEQ     'N'
043700961206     C*
043800961206     C                   READ      FNARB26L                               99
043900961206     C*
044000961206IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
044100961206     C                   MOVEL     '1'           $FINE
044200961206X   2C                   ELSE
044300961206     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
044400961206E   2C                   ENDIF
044500961206     C*
044600961206E   1C                   ENDDO
044700961206     C*
044800961206     C                   ENDSR
044900961206     C*------------------------------------------------------------------------*
045000961206     C* CHKREC - CONTROLLA VALIDITA' RECORD
045100961206     C*------------------------------------------------------------------------*
045200961206     C     CHKREC        BEGSR
045300961206     C*
045400961206     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
045500961206     C*
045600961206     C* SE LINEA ARRIVO <>  O P P U R E  DATA SPEDIZIONE > RICHIESTA, ESCE
045700961206IF  1C     ARBLNA        IFNE      DEPLNA
045800961206     C     ARBDSP        ORGT      PARDTF
045900961206     C                   MOVEL     '1'           $FINE                          *FINE LETTURA BOLLE
046000961206E   1C                   ENDIF
046100961206     C*
046200961206     C* BOLLA NON ANCORA CONSEGNATA, ESCLUDE
046300961206IF  1C     ARBDCM        IFEQ      *ZEROS
046400961206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046500961206E   1C                   ENDIF
046600961206     C*
046700961206     C                   ENDSR
046800961206     C*------------------------------------------------------------------------*
046900961206     C* MEMARB - MEMORIZZA I DATI DELLA BOLLA
047000961206     C*------------------------------------------------------------------------*
047100961206     C     MEMARB        BEGSR
047200961206     C*
047300961206     C* IMPOSTA I FLAG DI MEMORIZZAZIONE BOLLE
047400961206     C                   MOVEL     '1'           $MEMF                          *MEMOR. QC FILIALE
047500961206     C                   MOVEL     '1'           $MEMG                          *MEMOR. QC GENERALE
047600961206     C                   MOVEL     '0'           $CR                            *FIND CONS.RICHIESTE
047700961206     C                   MOVEL     '0'           $LG                            *FIND L.AVVISO/GIACE
047800961206     C                   MOVEL     '0'           $FD                            *FIND FERMO DEPOSITO
047900961206     C                   MOVEL     '0'           $TC                            *FIND TURNO CHIUSURA
048000961206     C                   MOVEL     '0'           $GC                            *FIND GIACENZA
048100961206     C                   MOVEL     '0'           $EG                            *FIND EVENTO GIACENZ
048200970115     C                   MOVEL     '0'           $PG                            *FIND + EVENTI GIAC.
048300970902     C*
048400970902     C* IMPOSTA I FLAG DI STAMPA BOLLE
048500970902     C                   MOVEL     '0'           §TCR                           *TOT. CONS. RICHIEST
048600970901     C                   MOVEL     '0'           §LAV                             -LASCIATO AVVISO
048700970901     C                   MOVEL     '0'           §LEG                             -L.AVVISO+GIACENZA
048800970901     C                   MOVEL     '0'           §GCA                             -GIACENZA
048900970901     C                   MOVEL     '0'           §FFD                             -FERMO DEPOSITO
049000970901     C                   MOVEL     '0'           §SMK                             -SUPERMARKET
049100970901     C                   MOVEL     '0'           §APP                             -APPUNTAMENTO
049200970901     C                   MOVEL     '0'           §RIT                             -ARRIVO RITARDO 1^
049300970901     C                   MOVEL     '0'           §RI2                             -ARRIVO RITARDO 2^
049400970901     C                   MOVEL     '0'           §CAS                             -CONTRASSEGNO ABIL
049500970901     C                   MOVEL     '0'           §TCT                           *TOT. CONS.RICH.TOT.
049600970901     C                   MOVEL     '0'           §TFD                           *TOT. FERMI DEPOSITI
049700970901     C                   MOVEL     '0'           §TIT                           *TOT. ARRIVI RIT. 1^
049800970901     C                   MOVEL     '0'           §TI2                           *TOT. ARRIVI RIT. 2^
049900970901     C                   MOVEL     '0'           §TTC                           *TOT. TURNI CHIUSURA
050000970901     C                   MOVEL     '0'           §TPT                           *TOT. CONS. PRIMA 13
050100970901     C                   MOVEL     '0'           §TPM                           *TOT. CONS. X POMERI
050200970901     C                   MOVEL     '0'           §TDT                           *TOT. CONS. DOPO 13
050300970901     C                   MOVEL     '0'           §TEG                           *TOT. EVEN.SENZA GIA
050400970902     C                   MOVEL     '0'           §TPG                           *TOT. + EVENTI GIAC
050500961206     C*
050600961206     C* PULISCO CAMPI ESTENSIONE BOLLE PER LA STAMPA
050700961206     C                   MOVEL     *BLANKS       AR9TIC                         *TIPO INCASSO C/A
050800961206     C                   MOVEL     *BLANKS       AR9VCA                         *DIVISA C/A
050900961206     C                   Z-ADD     *ZEROS        AR9CAS                         *C/ASSEGNO
051000961206     C*
051100961206     C* VALORIZZA LE CHIAVI DI LETTURA
051200051115     C                   Z-ADD     ARBAAS        KA9AAS                         *FiAR900F
051300961206     C                   Z-ADD     ARBLNP        KA9LNP
051400961206     C                   Z-ADD     ARBNRS        KA9NRS
051500961206     C                   Z-ADD     ARBNSP        KA9NSP
051600961206     C*
051700961206     C                   Z-ADD     ARBAAS        KAGAAS                         *FNARBG0F
051800961206     C                   Z-ADD     ARBLNP        KAGLNP
051900961206     C                   Z-ADD     ARBNRS        KAGNRS
052000961206     C                   Z-ADD     ARBNSP        KAGNSP
052100961206     C*
052200961206     C                   Z-ADD     ARBAAS        KADAAS                         *FNARBD0F
052300961206     C                   Z-ADD     ARBLNP        KADLNP
052400961206     C                   Z-ADD     ARBNRS        KADNRS
052500961206     C                   Z-ADD     ARBNSP        KADNSP
052600961206     C*
052700990810     C                   Z-ADD     ARBAAS        KEVAAS                         *FNEVB01L
052800961206     C                   Z-ADD     ARBLNP        KEVLNP
052900961206     C                   Z-ADD     ARBNRS        KEVNRS
053000961206     C                   Z-ADD     ARBNSP        KEVNSP
053100961206     C*
053200050310     C                   Z-ADD     ARBAAS        KGCAAS                         *TIGCP01L
053300961206     C                   Z-ADD     ARBLNP        KGCLNP
053400961206     C                   Z-ADD     ARBNRS        KGCNRS
053500961206     C                   Z-ADD     ARBNSP        KGCNSP
053600961206     C*
053700961206     C                   Z-ADD     4             KFVNPG                         *FNFVV01L
053800961206     C                   Z-ADD     ARBNDC        KFVNFV
053900020912     c                   z-add     ArbIfp        Kfvfgs
054000961206     C***
054100961206     C* TOTALE BOLLE
054200961206     C***
054300961206     C                   ADD       1             FILTBL                         *TOT. BOLLE
054400971204IF  1C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
054500961206     C                   ADD       1             GENTBL
054600961206E   1C                   ENDIF
054700961206     C***
054800961206     C* TOTALE CONSEGNE RICHIESTE
054900961206     C***
055000961206     C                   MOVEL     '0'           $CR
055100961206IF  1C     ARBDCR        IFNE      *ZEROS                                       *RICHIESTA CONSEGNA
055200961206     C* LEGGE LE VARIAZIONI
055300961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
055400991124     C     KEYARG        SETLL     FIARBF1C
055500991124     C     KEYARG        READE     FIARBF1C                               98
055600961206DO  2C     *IN98         DOWEQ     *OFF                                         *FINO A FINE FILE
055700030108IF  3***C     *IN20         IFEQ      *ON
055800030108     ***C     ARGCVB        ANDEQ     'CR'
055900030108IF  3C                   IF        *in20   and
056000040225     C                             (ARGcvb = 'CR'  or ARGcvb = 'CS' or
056100040225     C                              ARGcvb = 'CA'  or ARGcvb = 'CD')
056200961206     C*
056300961206     C* SE IN PARTENZA NON C'ERA LA CONSEGNA RICHIESTA O ERA DIVERSA :
056400961206     C*    - CONTEGGIA LA BOLLA COME CONSEGNA RICHIESTA ALL'ARRIVO
056500961206     C*    - CONTINUA A LEGGERE LE VARIZIONI SUCESSIVE PER CONTARNE IL N° TOTALE
056600961206IF  4C     ARGTCR        IFNE      ARBTCR
056700961206     C     ARGDCR        ORNE      ARBDCR
056800961206     C     ARGHCR        ORNE      ARBHCR
056900961206     C***
057000961206     C* VARIAZIONI SUCESSIVE
057100961206     C***
057200961206IF  5C     $CR           IFEQ      '1'                                          *LA 1^NON CONTEGGIAT
057300961206     C                   ADD       1             FILTCT                         *TOT. CONS.RICH.TOT.
057400970902     C                   MOVEL     '1'           §TCT                           *DA STAMPARE
057500971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
057600961206     C                   ADD       1             GENTCT
057700961206E   6C                   ENDIF
057800961206E   5C                   ENDIF
057900961206     C***
058000961206     C* PRIMA VARIAZIONE
058100961206     C***
058200961206IF  5C     $CR           IFEQ      '0'
058300961206     C                   ADD       1             FILTCR                         *TOT. CONS. RICHIEST
058400970902     C                   MOVEL     '1'           §TCR                           *DA STAMPARE
058500971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
058600961206     C                   ADD       1             GENTCR
058700961206E   6C                   ENDIF
058800961206     C                   MOVEL     '1'           $CR
058900961206E   5C                   ENDIF
059000961206E   4C                   ENDIF
059100961206E   3C                   ENDIF
059200961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
059300991124     C     KEYARG        READE     FIARBF1C                               98
059400961206E   2C                   ENDDO
059500961206E   1C                   ENDIF
059600961206     C*--------------------
059700961206     C* E V E N T I  DI TIPO "GIACENZA"
059800961206     C*--------------------
059900961206     C                   MOVEL     '0'           $LG                            *EVENTO LAVV/GIAC
060000961206     C                   MOVEL     '0'           $GC                            *APERTURA GIACENZA
060100970115     C                   MOVEL     '0'           $EG                            *EVENTO GIACENZA
060200970115     C                   MOVEL     '0'           $PG                            *PIU' EVENTI GIACENZ
060300990810     C     KEYEVB        SETLL     FNEVB01L
060400990810     C     KEYEVB        READE     FNEVB01L                               98
060500961206DO  1C     *IN98         DOWEQ     *OFF                                         *FINO A FINE FILE
060600961206     C                   Z-ADD     1             J
060700961206     C                   SETOFF                                           28
060800961206     C     EVBCEV        LOOKUP    CEV(J)                                 28
060900961206IF  2C     *IN28         IFEQ      *ON                                          *CONSIDERARE EVENTO
061000961206IF  3C     TEV(J)        IFNE      'A'
061100961206     C                   MOVEL     '1'           $EG                            *EVENTO GIACENZA
061200970115IF  4C     $PG           IFEQ      '0'
061300970115     C                   MOVEL     '1'           $PG                            *UN SOLO EVENTO GIAC
061400970115X   4C                   ELSE
061500970115     C                   MOVEL     '2'           $PG                            *PIU' EVENTI GIACENZ
061600970115E   4C                   ENDIF
061700970115E   3C                   ENDIF
061800961206     C* LEGGE LE GIACENZE
061900961206     C                   SETOFF                                           97
062000050310     C     KEYGCP        SETLL     TIGCP01L                               97
062100961206IF  3C     *IN97         IFEQ      *ON                                          *C'E GIACENZA
062200970115     C                   MOVEL     '1'           $GC                            *APERTURA GIACENZA
062300961206     C***
062400961206     C* LASCIATO AVVISO + GIACENZA
062500961206     C***
062600961206IF  4C     TEV(J)        IFEQ      'A'                                          *"LASCIATO AVVISO"
062700961206IF  5C     $CR           IFEQ      '1'                                          *SOLO CONS.RICH.MAN.
062800961206     C     $LG           ANDEQ     '0'                                          *SOLO UN EVENTO
062900961206     C                   ADD       1             FILLEG
063000970902     C                   MOVEL     '1'           §LEG                           *DA STAMPARE
063100971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
063200961206     C                   ADD       1             GENLEG
063300961206E   6C                   ENDIF
063400961206E   5C                   ENDIF
063500961206     C***
063600961206     C* GIACENZA
063700961206     C***
063800961206X   4C                   ELSE                                                   *"GIACENZA"
063900961206IF  5C     $CR           IFEQ      '1'                                          *SOLO CONS.RICH.MAN.
064000961206     C     $LG           ANDEQ     '0'                                          *SOLO UN EVENTO
064100961206     C                   ADD       1             FILGCA
064200970902     C                   MOVEL     '1'           §GCA                           *DA STAMPARE
064300971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
064400961206     C                   ADD       1             GENGCA
064500961206E   6C                   ENDIF
064600961206E   5C                   ENDIF
064700961206E   4C                   ENDIF
064800961206     C* NON C'E' GIACENZA
064900961206X   3C                   ELSE
065000961206IF  4C     TEV(J)        IFEQ      'A'                                          *"LASCIATO AVVISO"
065100961206     C***
065200961206     C* LASCIATO AVVISO
065300961206     C***
065400961206IF  5C     $CR           IFEQ      '1'                                          *SOLO CONS.RICH.MAN.
065500961206     C     $LG           ANDEQ     '0'                                          *SOLO UN EVENTO
065600961206     C                   ADD       1             FILLAV
065700970902     C                   MOVEL     '1'           §LAV                           *DA STAMPARE
065800971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
065900961206     C                   ADD       1             GENLAV
066000961206E   6C                   ENDIF
066100961206E   5C                   ENDIF
066200961206E   4C                   ENDIF
066300961206E   3C                   ENDIF
066400970115     C*
066500961206IF  3C     $CR           IFEQ      '1'                                          *SOLO CONS.RICH.MAN.
066600961206     C                   MOVEL     '1'           $LG                            *L.AVVISO / GIACENZA
066700961206E   3C                   ENDIF
066800961206E   2C                   ENDIF
066900961206     C*
067000990810     C     KEYEVB        READE     FNEVB01L                               98
067100961206E   1C                   ENDDO                                                  *FINE EVENTI
067200961206     C***
067300961206     C* EVENTO DI GIACENZA SENZA APERTURA GIACENZA
067400961206     C***
067500971002     C* IGNORA LE BOLLE CON PARTICOLARITA' GIACENZA:
067600971002     C* - NON APRIRE GIACENZA MA RENDERE
067700971002     C* - ATTENDERE PRIMA DI APRIRE GIACENZA
067800971003     C                   MOVEL     'N'           WGIACV                         *GIACENZA NON VALIDA
067900971003IF  1C     ARBGGA        IFEQ      *BLANKS                                      *NO PARTICOLARITA'
068000971003     C                   MOVEL     'S'           WGIACV                         *GIACENZA VALIDA
068100971003X   1C                   ELSE
068200971002     C                   SETOFF                                           28
068300971002     C                   Z-ADD     1             J
068400971003     C     ARBGGA        LOOKUP    GGA(J)                                 28
068500971003IF  2C     *IN28         IFEQ      *ON                                          *TROVATA
068600971003IF  3C     G2MF(J)       IFEQ      'A'
068700971002     C     G2MQ(J)       ANDEQ     'P'
068800971002     C     G5DI(J)       OREQ      'R'
068900971002     C     G4D1(J)       OREQ      'R'
069000971002     C     G4D2(J)       OREQ      'R'
069100971002     C     G4D3(J)       OREQ      'R'
069200971002     C     G4D4(J)       OREQ      'R'
069300971002     C     G4D5(J)       OREQ      'R'
069400971003X   3C                   ELSE
069500971003     C                   MOVEL     'S'           WGIACV                         *GIACENZA VALIDA
069600971003E   3C                   ENDIF
069700971003E   2C                   ENDIF
069800971003E   1C                   ENDIF
069900971003IF  1C     WGIACV        IFEQ      'S'                                          *GIACENZA VALIDA
070000970115IF  2C     $EG           IFEQ      '1'                                          *EVENTO GIACENZA
070100961206     C     $GC           ANDEQ     '0'                                          *NO APERTURA GIACENZ
070200961206     C                   ADD       1             FILTEG
070300970902     C                   MOVEL     '1'           §TEG                           *DA STAMPARE
070400971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
070500961206     C                   ADD       1             GENTEG
070600970115E   3C                   ENDIF
070700970115     C***
070800970115     C* PIU' EVENTI DI GIACENZA SENZA APERTURA GIACENZA
070900970115     C***
071000970115IF  3C     $PG           IFGT      '1'
071100970115     C                   ADD       1             FILTPG
071200970902     C                   MOVEL     '1'           §TPG                           *DA STAMPARE
071300971204IF  4C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
071400970115     C                   ADD       1             GENTPG
071500970115E   4C                   ENDIF
071600970115E   3C                   ENDIF
071700970115E   2C                   ENDIF
071800961206E   1C                   ENDIF
071900961206     C***
072000961206     C* FERMO DEPOSITO
072100961206     C***
072200961206     C                   MOVEL     '0'           $FD
072300961206IF  1C     ARBFFD        IFNE      *BLANKS                                      *C'E' FERMO DEPOSITO
072400961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
072500991124     C     KEYARD        SETLL     FIARBF1C
072600991124     C     KEYARD        READE     FIARBF1C                               98
072700961206DO  2C     *IN98         DOWEQ     *OFF                                         *FINO A FINE FILE
072800961206     C     $FD           ANDEQ     '0'                                          *FERMO DEP. TROVATO
072900961206IF  3C     *IN10         IFEQ      *ON
073000961206     C     ARDCVB        ANDEQ     'I0'
073100961206     C*
073200961206     C* SE IN PARTENZA NON C'ERA IL FERMO DEPOSITO :
073300961206     C*  - CONTEGGIA LA BOLLA COME FERMO DEPOSITO ALL'ARRIVO (NEI TOTALI F.D.)
073400961206     C*  - ESCE DALLA LETTURA VARIAZIONI
073500961206IF  4C     ARDFFD        IFEQ      *BLANKS
073600961206     C                   MOVEL     '1'           $FD
073700961206     C                   ADD       1             FILTFD                         *TOT. FERMI DEPOSITI
073800970902     C                   MOVEL     '1'           §TFD                           *DA STAMPARE
073900971204IF  5C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
074000961206     C                   ADD       1             GENTFD
074100961206E   5C                   ENDIF
074200961206E   4C                   ENDIF
074300961206E   3C                   ENDIF
074400961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
074500991124     C     KEYARD        READE     FIARBF1C                               98
074600961206E   2C                   ENDDO
074700961206     C*  - CONTEGGIA LA BOLLA COME FERMO DEPOSITO DELLE CONSEGNE RICHIESTE
074800961206IF  2C     $CR           IFEQ      '1'
074900961206     C                   MOVEL     '1'           $FD
075000961206     C                   ADD       1             FILFFD                         *FERMO DEPOSITO
075100970902     C                   MOVEL     '1'           §FFD                           *DA STAMPARE
075200971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
075300961206     C                   ADD       1             GENFFD
075400961206E   3C                   ENDIF
075500961206E   2C                   ENDIF
075600961206E   1C                   ENDIF
075700961206     C***
075800961206     C* SUPERMERCATO
075900961206     C***
076000961206     C* CALCOLA SOLO SE' E' UNA BOLLA CON CONSEGNA RICHIESTA
076100961206IF  1C     $CR           IFEQ      '1'
076200961206IF  2C     ARBTC1        IFEQ      'S'                                          *CONSEGNA SUPERMARKE
076300961206     C     ARBTC2        OREQ      'S'
076400961206     C                   ADD       1             FILSMK                         *SUPERMARKET
076500970902     C                   MOVEL     '1'           §SMK                           *DA STAMPARE
076600971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
076700961206     C                   ADD       1             GENSMK
076800961206E   3C                   ENDIF
076900961206E   2C                   ENDIF
077000961206E   1C                   ENDIF
077100961206     C***
077200961206     C* APPUNTAMENTO
077300961206     C***
077400961206     C* CALCOLA SOLO SE' E' UNA BOLLA CON CONSEGNA RICHIESTA
077500961206IF  1C     $CR           IFEQ      '1'
077600961206IF  2C     ARBTC1        IFEQ      'A'                                          *CONSEGNA SU APPUNTA
077700961206     C     ARBTC2        OREQ      'A'
077800961206     C                   ADD       1             FILAPP                         *APPUNTAMENTO
077900970902     C                   MOVEL     '1'           §APP                           *DA STAMPARE
078000971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
078100961206     C                   ADD       1             GENAPP
078200961206E   3C                   ENDIF
078300961206E   2C                   ENDIF
078400961206E   1C                   ENDIF
078500961206     C***
078600961206     C* DATA ARRIVO REALE =  DATA PARTENZA ULTIMO COLLO ->
078700961206     C* . NON ESEGUE IL CONFRONTO DELLE ORE CON LE SOGLIE
078800961206     C* DATA ARRIVO REALE <> DATA PARTENZA ULTIMO COLLO ->
078900961206     C* . REPERISCE LA DATA ARRIVO TEORICA
079000961206     C* DATA ARRIVO REALE <  DATA ARRIVO TEORICA ->
079100961206     C* . NON ESEGUE IL CONFRONTO DELLE ORE CON LE SOGLIE
079200961206     C* DATA ARRIVO REALE >=  DATA ARRIVO TEORICA ->
079300961206     C* . CONFRONTA DATA+ORA REALE CON DATA+ORE TEORICHE
079400961206     C***
079500961206     C                   Z-ADD     *ZEROS        D98DTI
079600961206     C                   Z-ADD     *ZEROS        WDUC              8 0
079700961206IF  1C     ARBDUC        IFGT      *ZEROS
079800961206     C                   Z-ADD     ARBDUC        WDUC
079900961206X   1C                   ELSE
080000961206     C                   Z-ADD     ARBDPC        WDUC
080100961206E   1C                   ENDIF
080200961206     C                   SETOFF                                           28
080300961206     C                   Z-ADD     1             J
080400961206     C     ARBLNA        LOOKUP    TRA(J)                                 28     *REPERISCE LE SOGLI
080500961206     C*
080600961206IF  1C     ARBDTI        IFEQ      WDUC                                         *DATA ARR = PAR UL C
080700961206X   1C                   ELSE                                                   *DATA ARR <>PAR UL C
080800961206     C*
080900961206     C* REPERISCE DATA ARRIVO TEORICA
081000961206     C                   EXSR      REPDTI
081100961206IF  2C     ARBDTI        IFLT      D98DTI                                       *DATA REAL < DATA TE
081200961206X   2C                   ELSE                                                   *DATA REAL>= DATA TE
081300961206     C* DATA+ORA REALE
081400961206     C                   Z-ADD     *ZEROS        WDATAR           12 0          *DATA+ORA REALE
081500961206     C                   MOVEL     ARBDTI        WDATAR                         *DATA + ...
081600961206     C                   MOVE      ARBHTI        WDATAR                         * ... + ORA
081700961206     C* 1^DATA+ORA TEORICA
081800961206     C                   Z-ADD     *ZEROS        WDATAT           12 0          *1^DATA+ORA TEORICA
081900961206     C                   MOVEL     D98DTI        WDATAT                         *DATA + ...
082000961206     C                   MOVE      OSA(J)        WDATAT                         * ... + ORA
082100961206IF  3C     WDATAR        IFGT      WDATAT                                       *RITARDO SU 1^SOGLIA
082200961206     C                   ADD       1             FILTIT                         *TOT. ARRIVI RIT. 1^
082300970902     C                   MOVEL     '1'           §TIT                           *DA STAMPARE
082400971204IF  4C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
082500961206     C                   ADD       1             GENTIT
082600961206E   4C                   ENDIF
082700961206IF  4C     $CR           IFEQ      '1'                                          *CONSEGNE RICHIESTE
082800961206     C                   ADD       1             FILRIT                         *ARRIVO RITARDO 1^S
082900970902     C                   MOVEL     '1'           §RIT                           *DA STAMPARE
083000971204IF  5C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
083100961206     C                   ADD       1             GENRIT
083200961206E   5C                   ENDIF
083300961206E   4C                   ENDIF
083400961206E   3C                   ENDIF
083500961206     C* 2^DATA+ORA TEORICA
083600961206     C                   Z-ADD     *ZEROS        WDATAT           12 0          *2^DATA+ORA TEORICA
083700961206     C                   MOVEL     D98DTI        WDATAT                         *DATA + ...
083800961206     C                   MOVE      OSA1(J)       WDATAT                         * ... + ORA
083900961206IF  3C     WDATAR        IFGT      WDATAT                                       *RITARDO SU 2^SOGLIA
084000961206     C                   ADD       1             FILTI2                         *TOT. ARRIVI RIT. 2^
084100970902     C                   MOVEL     '1'           §TI2                           *DA STAMPARE
084200971204IF  4C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
084300961206     C                   ADD       1             GENTI2
084400961206E   4C                   ENDIF
084500961206IF  4C     $CR           IFEQ      '1'                                          *CONSEGNE RICHIESTE
084600961206     C                   ADD       1             FILRI2                         *ARRIVO RITARDO 2^S
084700970902     C                   MOVEL     '1'           §RI2                           *DA STAMPARE
084800971204IF  5C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
084900961206     C                   ADD       1             GENRI2
085000961206E   5C                   ENDIF
085100961206E   4C                   ENDIF
085200961206E   3C                   ENDIF
085300961206E   2C                   ENDIF
085400961206E   1C                   ENDIF
085500961206     C***
085600961206     C* CONTRASSEGNI CHE SONO STATI ABILITATI ALLA CONSEGNA
085700961206     C***
085800961206IF  1C     $CR           IFEQ      '1'                                          *SOLO PER CONS.RICH.
085900051115     C     KEYAR9        CHAIN     FiAR901L                           98
086000011211     C*                  MOVEL     'N'           WABATM                         *ABILITAZ.AUTOM.C/A
086100961206     C*
086200961206     C* SOLO SE ESISTE CONTRASSEGNO
086300961206IF  2C     *IN98         IFEQ      *OFF                                         *ESISTE C/ASSEGNO
086400961206     C     AR9CAS        ANDGT     *ZEROS
086500961206     C*
086600010124     C* CONTROLLA SE IL C/A E' STATO ABILITATO AUTOMATICAMENTE
086700020906     C*                  Z-ADD     1             KTBKUT                         *CODICE UTENTE
086800011211     C*                  MOVEL     '1A'          KTBCOD                         *CODICE TABELLA
086900011211     C*                  MOVEL(P)  AR9TIC        KTBKEY                         *CHAIVE TABELLA
087000011211     C*    KEYTAB        CHAIN     TABEL00F                           98
087100011211IF  3C*    *IN98         IFEQ      *OFF
087200011211     C*                  MOVEL     TBLUNI        DS1A
087300011211     C*                  Z-ADD     *ZEROS        W0133            13 3
087400011211     C*                  EXSR      CTRCAS
087500011211     C*                  EXSR      REPGEI
087600011211     C                   EXSR      REPMAXCSB
087700011211IF  4C*    W0133         IFLE      §GELCA                                       *SOTTO LIM C/ASSEGNO
087800011211IF  5C*    §1AFCA        IFNE      'C'                                          *NO ASSEGNO CIRCOLAR
087900011211     C*                  MOVEL     'S'           WABATM                         *ABILITAZ.AUTOM. C/A
088000011211E   5C*                  ENDIF
088100011211X   4C*                  ELSE                                                   *SOPRA LIM C/ASSEGNO
088200011211IF  5C*    §1AFCA        IFNE      'C'                                          *NO ASSEGNO CIRCOLAR
088300011211     C*    §1AFCA        ANDNE     'S'                                          *NO CONTANTI
088400011211     C*                  MOVEL     'S'           WABATM                          *ABILITAZ.AUTOM. C/
088500011211E   5C*                  ENDIF
088600011211E   4C*                  ENDIF
088700011211E   3C*                  ENDIF
088800961206     C*
088900961206     C* ABILITAZIONE NON AUTOMATICA, CONTEGGIO
089000011211IF  3C*    WABATM        IFEQ      'N'
089100011211     C     O21FCA        IFNE      *BLANKS
089200961206     C                   ADD       1             FILCAS                         *C/A ABILITATI
089300970902     C                   MOVEL     '1'           §CAS                           *DA STAMPARE
089400971204IF  4C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
089500961206     C                   ADD       1             GENCAS
089600961206E   4C                   ENDIF
089700961206E   3C                   ENDIF
089800961206E   2C                   ENDIF
089900961206E   1C                   ENDIF
090000961206     C***
090100961206     C* TURNI DI CHIUSURA
090200961206     C***
090300961206     C                   MOVEL     '0'           $TC
090400961206IF  1C     ARBGC1        IFNE      *BLANKS                                      *1°GG TURNO CHIUSURA
090500961206     C     ARBGC2        ORNE      *BLANKS                                      *2°GG TURNO CHIUSURA
090600961206     C* LEGGE LE VARIAZIONI
090700961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
090800961206     C                   MOVEL     '0'           $TC
090900991124     C     KEYARG        SETLL     FIARBF1C
091000991124     C     KEYARG        READE     FIARBF1C                               98
091100961206DO  2C     *IN98         DOWEQ     *OFF                                         *FINO A FINE FILE
091200961206     C     $TC           ANDEQ     '0'                                          *TURNO CHIUS. TROVAT
091300030108IF  3***C     *IN20         IFEQ      *ON
091400030108     ***C     ARGCVB        ANDEQ     'CR'
091500030108IF  3C                   IF        *in20   and
091600040225     C                             (ARGcvb = 'CR'  or ARGcvb = 'CS' or
091700040225     C                              ARGcvb = 'CD')
091800961206     C*
091900961206     C* IN ARRIVO C'E' IL 1° GG TURNO DI CHIUSURA (ARBGC1<>' ')
092000961206IF  4C     ARBGC1        IFNE      *BLANKS
092100070314      * conteggio solo se la variazione è stata fatta in arrivo o in partenza ma su bolla locale
092200070314      * (lnp = lna)
092300070314   4ac                   if        %subst(argpru:10:1) = 'A' or
092400070314     c                             %subst(argpru:10:1) = 'P' and arblnp = arblna
092500961206     C*
092600961206     C* SE IN PARTENZA NON C'ERA IL 1° GG TURNO DI CHIUSURA (ARGGC1=' ')   E
092700961206     C* QUELLO IN ARRIVO E' DIVERSO DAL 2° GG DELLA PARTENZA (ARBGC1<>ARGGC2)
092800961206     C*   - CONTEGGIA LA BOLLA CON TURNO DI CHIUSURA ALL'ARRIVO
092900961206     C*   - ESCE DALLA LETTURA VARIAZIONI
093000961206IF  5C     ARGGC1        IFEQ      *BLANKS                                      *NO IN PARTENZA
093100961206     C     ARBGC1        ANDNE     ARGGC2                                       *1°ARRIVO<>2°PARTENZ
093200961206     C                   MOVEL     '1'           $TC
093300961206     C                   ADD       1             FILTTC                         *TOT. TURNI CHIUSURA
093400970902     C                   MOVEL     '1'           §TTC                           *DA STAMPARE
093500971204IF  6C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
093600961206     C                   ADD       1             GENTTC
093700961206E   6C                   ENDIF
093800961206E   5C                   ENDIF
093900070314   4ac                   endif
094000961206E   4C                   ENDIF
094100961206     C*
094200961206     C* IN ARRIVO C'E' IL 2° GG TURNO DI CHIUSURA (ARBGC2<>' ')
094300961206IF  4C     $TC           IFEQ      '0'                                          *NON ANCORA CONTEGG.
094400961206IF  5C     ARBGC2        IFNE      *BLANKS
094500070314      * conteggio solo se la variazione è stata fatta in arrivo o in partenza ma su bolla locale
094600070314      * (lnp = lna)
094700070314   5ac                   if        %subst(argpru:10:1) = 'A' or
094800070314     c                             %subst(argpru:10:1) = 'P' and arblnp = arblna
094900961206     C*
095000961206     C* SE IN PARTENZA NON C'ERA IL 2° GG TURNO DI CHIUSURA (ARGGC2=' ')   E
095100961206     C* QUELLO IN ARRIVO E' DIVERSO DAL 1° GG DELLA PARTENZA (ARBGC2<>ARGGC1)
095200961206     C*   - CONTEGGIA LA BOLLA CON TURNO DI CHIUSURA ALL'ARRIVO
095300961206     C*   - ESCE DALLA LETTURA VARIAZIONI
095400961206IF  6C     ARGGC2        IFEQ      *BLANKS                                      *NO IN PARTENZA
095500961206     C     ARBGC2        ANDNE     ARGGC1                                       *2°ARRIVO<>1°PARTENZ
095600961206     C                   MOVEL     '1'           $TC
095700961206     C                   ADD       1             FILTTC                         *TOT. TURNI CHIUSURA
095800970902     C                   MOVEL     '1'           §TTC                           *DA STAMPARE
095900971204IF  7C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
096000961206     C                   ADD       1             GENTTC                         *TOT. TURNI CHIUSURA
096100961206E   7C                   ENDIF
096200961206E   6C                   ENDIF
096300070314   5ac                   endif
096400961206E   5C                   ENDIF
096500961206E   4C                   ENDIF
096600961206E   3C                   ENDIF
096700961206     C*
096800961206     C                   SETOFF                                       1020      *INDICATORI LETTURA
096900991124     C     KEYARG        READE     FIARBF1C                               98
097000961206E   2C                   ENDDO
097100961206E   1C                   ENDIF
097200961206     C***
097300961206     C* CONSEGNA PRIMA / DOPO 13:00
097400961206     C***
097500961206IF  1C     ARBHMC        IFLE      1300                                         *M A T T I N O
097600961206     C                   ADD       1             FILTPT                         *TOT. CONS. PRIMA 13
097700970902     C                   MOVEL     '1'           §TPT                           *DA STAMPARE
097800971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
097900961206     C                   ADD       1             GENTPT
098000961206E   3C                   ENDIF
098100961206     C*
098200961206     C* LEGGE LA DISTINTA CONSEGNA PER VEDERE SE E' DEL POMERIGGIO
098300961206     C     KEYFVV        CHAIN     FNFVV01L                           98
098400961206IF  2C     *IN98         IFEQ      *OFF
098500961206     C     FVVATB        ANDEQ     *BLANKS
098600961206     C     FVVFPP        ANDEQ     'P'
098700961206     C                   ADD       1             FILTPM                         *TOT. CONS. X POMERI
098800970902     C                   MOVEL     '1'           §TPM                           *DA STAMPARE
098900971204IF  3C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
099000961206     C                   ADD       1             GENTPM
099100961206E   3C                   ENDIF
099200961206E   2C                   ENDIF
099300961206X   1C                   ELSE                                                   *P O M E R I G G I O
099400961206     C                   ADD       1             FILTDT                         *TOT. CONS. DOPO 13
099500970902     C                   MOVEL     '1'           §TDT                           *DA STAMPARE
099600971204IF  2C     DEPFL1        IFEQ      *BLANKS                                      *MISUD NO TOT ESTERO
099700961206     C                   ADD       1             GENTDT
099800961206E   2C                   ENDIF
099900961206E   1C                   ENDIF
100000961206     C*
100100961206     C                   ENDSR
100200961206     C*------------------------------------------------------------------------*
100300961206     C* REPDTI - REPERISCE LA DATA DI ARRIVO TEORICA DELLA TRAZIONE
100400961206     C*------------------------------------------------------------------------*
100500961206     C     REPDTI        BEGSR
100600961206     C*
100700971009     C                   MOVEL     '1'           WSD99                          *LANCIATO TNSD99R
100800961206     C*
100900971009     C                   CLEAR                   DS99
101000961206     C                   MOVEL     'T'           D98TLA                         *TIPO LANCIO
101100961206     C                   MOVEL     ARBLNP        D98LNP                         *LINEA DI PARTENZA
101200961206     C                   MOVEL     ARBTFP        D98TFP                         *TERMINAL DI PARTENZ
101300961206     C                   MOVEL     ARBLNA        D98LNA                         *LINEA DI ARRIVO
101400961206     C                   MOVEL     ARBTFA        D98TFA                         *TERMINAL DI ARRIVO
101500961206     C                   MOVEL     WDUC          D98DSP                         *DATA PART ULT COLLO
101600961206     C                   MOVEL     'E'           D98TSP                         *TIPO SERVIZIO ESPRE
101700971009     C                   MOVEL     ARBCAD        D98CAD                         *CAP DESTINO
101800971009     C                   MOVEL     ARBLOD        D98LOD                         *LOCALITA' DESTINO
101900971009     C                   CALL      'TNSD99R'
102000971009     C                   PARM                    DS99
102100961206     C*
102200961206     C                   ENDSR
102300961206     C*------------------------------------------------------------------------*
102400961206     C* CTRCAS - CONTROLLA SE IL CONTRASSEGNO E' IN DIVISA ESTERA
102500961206     C*------------------------------------------------------------------------*
102600011211     C*    CTRCAS        BEGSR
102700010822     C*
102800011211     C*                  IF        AR9VCA = *blanks
102900011211     C*                  EVAL      AR9VCA = 'ITL'
103000011211     C*                  ENDIF
103100010822     C*
103200011211     C*                  IF        AR9VCA <> 'ITL' and
103300011211     C*                            AR9VCA <> 'EUR'
103400010822     C* ESEGUO CONVERSIONE TRAMITE YEURCO
103500011211     C*                  CLEAR                   YEURDS
103600011211     C*                  Z-ADD     AR9CAS        YECIMI
103700011211     C*                  MOVEL     AR9VCA        YECDVI
103800011211     C*                  EXSR      EXECNV
103900011211     C*                  Z-ADD     YECIMO        W0133
104000011211     C*                  MOVEL     YECDVO        WVCA              3
104100011211     C*                  ELSE
104200011211     C*                  Z-ADD     AR9CAS        W0133
104300011211     C*                  MOVEL     AR9VCA        WVCA
104400011211     C*                  ENDIF
104500011211     C*
104600011211     C*                  ENDSR
104700010822     C*------------------------------------------------------------------------*
104800011211     C*   REPGEI - REPERISCE I LIMITI CONSENTITI X IL CONTRASSEGNO DALLA TABELLA "GEI"
104900011211     C*------------------------------------------------------------------------*
105000011211     C*    REPGEI        BEGSR
105100010822     C*
105200011211     C* REPERISCO I LIMITI X IL CONTRASEGNO
105300011211     C*                  MOVEL     'GEI'         KTECOD
105400011211     C*                  MOVEL     *BLANKS       KTEKE1
105500011211     C*                  MOVEL     *BLANKS       KTEKE2
105600011211     C*                  MOVEL     WVCA          KTEKE1
105700011211     C*    KTBE          CHAIN     TNTBE01L                           21
105800011211     C* N21              MOVEL     TBEUNI        DGEI
105900011211     C*
106000011211     C*                  ENDSR
106100011211
106200011211     C*------------------------------------------------------------------------*
106300011211     C* Controllo importo massimo contrassegno.
106400011211     C*------------------------------------------------------------------------*
106500011211     C     REPMAXCSB     BEGSR
106600011211     C*
106700011211     C                   CLEAR                   Trul21DS
106800011211     C*
106900011211     C                   EVAL      I21TSp = arbTSP
107000011211     C                   EVAL      I21NzD = arbNZD
107100011211     C                   EVAL      I21CBo = arbCBO
107200011211     C                   EVAL      I21LnP = arbLNP
107300011211     C                   EVAL      I21LnA = arbLNA
107400130828     C******             EVAL      I21Ksc = arbKSC
107500011211     C                   EVAL      I21NzM = arbNZM
107600011211     C                   MOVE(p)   arbCTR        I21Ctr
107700011211     C                   EVAL      I21Tic = ar9TIC
107800011211     C                   EVAL      I21Imp = ar9CAS
107900011211     C                   EVAL      I21Div = ar9VCA
108000011211     C                   EVAL      I21Ute = KNMUS
108001130828     c*
108002130828     c* in i21ksc passo il MITTENTE: CCM se assegnato, KSC se franco
108004130828     c                   z-add     1             ktbkut
108005130828     c                   move      '3A'          ktbcod
108006130828     c                   movel(p)  arbcbo        ktbkey
108009130828     c                   clear                   ds3a
108010130828     c     keytab        chain     tabel00f
108011130828     c                   if        %found(tabel00f)
108012130828     c                   movel     tbluni        ds3a
108013130828     c                   endif
108017130828     c                   if        %subst(§3atb1:1:1)='F'
108018130828     c                   z-add     arbksc        i21ksc
108019130828     c                   else
108020130828     c                   z-add     arbccm        i21ksc
108021130828     c                   if        i21ksc=0
108022130828     c                   z-add     8888          i21ksc
108023130828     c                   movel     arblnp        i21ksc
108024130828     c                   endif
108025130828     c                   endif
108100011211     C*
108200011211     C                   CALL      'TRUL21R'
108300011211     C                   PARM                    Trul21DS
108400011211     C*
108500011211     C                   ENDSR
108600011211     C*------------------------------------------------------------------------*
108700011211
108800961206     C*------------------------------------------------------------------------*
108900961206     C* STADET - STAMPA RIGA DI DETTAGLIO
109000961206     C*------------------------------------------------------------------------*
109100961206     C     STADET        BEGSR
109200961206     C*
109300961206     C* STAMPA SOLO LE BOLLE DI PARTICOLARE INTERESSE
109400970902IF  1C     §TCR          IFEQ      '1'
109500970902     C     §LAV          OREQ      '1'
109600970902     C     §LEG          OREQ      '1'
109700970902     C     §GCA          OREQ      '1'
109800970902     C     §FFD          OREQ      '1'
109900970902     C     §SMK          OREQ      '1'
110000970902     C     §APP          OREQ      '1'
110100970902     C     §RIT          OREQ      '1'
110200970902     C     §RI2          OREQ      '1'
110300970902     C     §CAS          OREQ      '1'
110400970902     C     §TCT          OREQ      '1'
110500970902     C     §TFD          OREQ      '1'
110600970902     C     §TIT          OREQ      '1'
110700970902     C     §TI2          OREQ      '1'
110800970902     C     §TTC          OREQ      '1'
110900970902     C     §TPM          OREQ      '1'
111000970902     C     §TDT          OREQ      '1'
111100970902     C     §TEG          OREQ      '1'
111200970902     C     §TPG          OREQ      '1'
111300961206     C*--------------------
111400961206     C* D E C O D I F I C H E
111500961206     C*--------------------
111600970902     C                   CLEAR                   CSTM                           *CAUSALI
111700970902     C                   Z-ADD     *ZEROS        I                              *INDICE
111800971002IF  2C     §TCR          IFEQ      '1'
111900970902     C                   ADD       1             I
112000970902     C                   MOVEL     '1'           CSTM(I)                        *TOT. CONS. RICHIEST
112100971002E   2C                   ENDIF
112200971002IF  2C     §LAV          IFEQ      '1'
112300970902     C                   ADD       1             I
112400970902     C                   MOVEL     '2'           CSTM(I)                          -LASCIATO AVVISO
112500971002E   2C                   ENDIF
112600971002IF  2C     §LEG          IFEQ      '1'
112700970902     C                   ADD       1             I
112800970902     C                   MOVEL     '3'           CSTM(I)                          -L.AVVISO+GIACENZA
112900971002E   2C                   ENDIF
113000971002IF  2C     §GCA          IFEQ      '1'
113100970902     C                   ADD       1             I
113200970902     C                   MOVEL     '4'           CSTM(I)                          -GIACENZA
113300971002E   2C                   ENDIF
113400971002IF  2C     §FFD          IFEQ      '1'
113500970902     C                   ADD       1             I
113600970902     C                   MOVEL     '5'           CSTM(I)                          -FERMO DEPOSITO
113700971002E   2C                   ENDIF
113800971002IF  2C     §SMK          IFEQ      '1'
113900970902     C                   ADD       1             I
114000970902     C                   MOVEL     '6'           CSTM(I)                          -SUPERMARKET
114100971002E   2C                   ENDIF
114200971002IF  2C     §APP          IFEQ      '1'
114300970902     C                   ADD       1             I
114400970902     C                   MOVEL     '7'           CSTM(I)                          -APPUNTAMENTO
114500971002E   2C                   ENDIF
114600971002IF  2C     §RIT          IFEQ      '1'
114700970902     C                   ADD       1             I
114800970902     C                   MOVEL     '8'           CSTM(I)                          -ARRIVO RITARDO 1^
114900971002E   2C                   ENDIF
115000971002IF  2C     §RI2          IFEQ      '1'
115100970902     C                   ADD       1             I
115200970902     C                   MOVEL     '9'           CSTM(I)                          -ARRIVO RITARDO 2^
115300971002E   2C                   ENDIF
115400971002IF  2C     §CAS          IFEQ      '1'
115500970902     C                   ADD       1             I
115600970902     C                   MOVEL     'A'           CSTM(I)                          -CONTRASSEGNO ABIL
115700971002E   2C                   ENDIF
115800971002IF  2C     §TCT          IFEQ      '1'
115900970902     C                   ADD       1             I
116000970902     C                   MOVEL     'B'           CSTM(I)                        *TOT. CONS.RICH.TOT.
116100971002E   2C                   ENDIF
116200971002IF  2C     §TFD          IFEQ      '1'
116300970902     C                   ADD       1             I
116400970902     C                   MOVEL     'C'           CSTM(I)                        *TOT. FERMI DEPOSITI
116500971002E   2C                   ENDIF
116600971002IF  2C     §TIT          IFEQ      '1'
116700970902     C                   ADD       1             I
116800970902     C                   MOVEL     'D'           CSTM(I)                        *TOT. ARRIVI RIT. 1^
116900971002E   2C                   ENDIF
117000971002IF  2C     §TI2          IFEQ      '1'
117100970902     C                   ADD       1             I
117200970902     C                   MOVEL     'E'           CSTM(I)                        *TOT. ARRIVI RIT. 2^
117300971002E   2C                   ENDIF
117400971002IF  2C     §TTC          IFEQ      '1'
117500970902     C                   ADD       1             I
117600970902     C                   MOVEL     'F'           CSTM(I)                        *TOT. TURNI CHIUSURA
117700971002E   2C                   ENDIF
117800971002IF  2C     §TPM          IFEQ      '1'
117900970902     C                   ADD       1             I
118000970902     C                   MOVEL     'H'           CSTM(I)                        *TOT. CONS. X POMERI
118100971002E   2C                   ENDIF
118200971002IF  2C     §TDT          IFEQ      '1'
118300970902     C                   ADD       1             I
118400970902     C                   MOVEL     'I'           CSTM(I)                        *TOT. CONS. DOPO 13
118500971002E   2C                   ENDIF
118600971002IF  2C     §TEG          IFEQ      '1'
118700970902     C                   ADD       1             I
118800970902     C                   MOVEL     'L'           CSTM(I)                        *TOT. EVEN.SENZA GIA
118900971002E   2C                   ENDIF
119000971002IF  2C     §TPG          IFEQ      '1'
119100970902     C                   ADD       1             I
119200970902     C                   MOVEL     'M'           CSTM(I)                        *TOT.+ EVENTI GIAC.
119300971002E   2C                   ENDIF
119400970902     C                   MOVEA     CSTM          DTCCAU                         *CAUSALI STAMPA DETT
119500970916     C*
119600970916     C* STAMPA SOLO LE BOLLE CON CAUSALI DETTAGLIO RICHIESTE
119700970916     C                   MOVEL     'N'           WSTDET                         *NO STAMPA DETTAGLIO
119800971002IF  2C     PARCAU        IFEQ      *BLANKS                                      *NESSUNA SELEZIONE
119900971002     C                   MOVEL     'S'           WSTDET                         *STAMPA DETTAGLIO
120000971002X   2C                   ELSE                                                   *SELEZIONI
120100970916     C                   Z-ADD     1             I
120200970916     C                   Z-ADD     1             J
120300970916     C                   MOVEA     PARCAU        SA1
120400971002DO  3C     1             DO        20            I                              *CAUSALI DA STAMPARE
120500971002IF  4C     SA1(I)        IFNE      *BLANKS
120600971002DO  5C     1             DO        20            J                              *CAUSALI BOLLE
120700971002IF  6C     CSTM(J)       IFNE      *BLANKS
120800971002IF  7C     SA1(I)        IFEQ      CSTM(J)
120900970916     C                   MOVEL     'S'           WSTDET                         *STAMPA DETTAGLIO
121000971002E   7C                   ENDIF
121100971002E   5C                   ENDIF
121200971002E   4C                   ENDDO
121300971002E   4C                   ENDIF
121400971002E   3C                   ENDDO
121500971002E   2C                   ENDIF
121600970902     C*
121700970916IF  2C     WSTDET        IFEQ      'S'                                          *DETTAGLIO DA STAMAR
121800961206     C                   Z-ADD     ARBDCR        G08INV                         *CONSEGNA RICHIESTA
121900961206     C                   Z-ADD     *ZEROS        G08DAT
122000961206     C                   MOVEL     '3'           G08ERR
122100961206     C                   CALL      'XSRDA8'
122200961206     C                   PARM                    WLBDA8
122300961206     C                   Z-ADD     G08DAT        DTCDCR
122400961206     C*
122500961206     C                   Z-ADD     ARBDTI        G08INV                         *ARRIVO TRAZIONE
122600961206     C                   Z-ADD     *ZEROS        G08DAT
122700961206     C                   MOVEL     '3'           G08ERR
122800961206     C                   CALL      'XSRDA8'
122900961206     C                   PARM                    WLBDA8
123000961206     C                   Z-ADD     G08DAT        DTCDTI
123100961206     C*
123200961206     C                   Z-ADD     D98DTI        G08INV                         *ARRIVO TRAZ. TEORIC
123300961206     C                   Z-ADD     *ZEROS        G08DAT
123400961206     C                   MOVEL     '3'           G08ERR
123500961206     C                   CALL      'XSRDA8'
123600961206     C                   PARM                    WLBDA8
123700961206     C                   Z-ADD     G08DAT        DTID98
123800961206     C*
123900961206     C                   Z-ADD     WDUC          G08INV                         PARTENZA ULTIMO COLL
124000961206     C                   Z-ADD     *ZEROS        G08DAT
124100961206     C                   MOVEL     '3'           G08ERR
124200961206     C                   CALL      'XSRDA8'
124300961206     C                   PARM                    WLBDA8
124400961206     C                   Z-ADD     G08DAT        DUCARB
124500961206     C*
124600961206     C                   Z-ADD     ARBDCE        G08INV                         *CONSEGNA PREVISTA
124700961206     C                   Z-ADD     *ZEROS        G08DAT
124800961206     C                   MOVEL     '3'           G08ERR
124900961206     C                   CALL      'XSRDA8'
125000961206     C                   PARM                    WLBDA8
125100961206     C                   Z-ADD     G08DAT        DTCDCE
125200961206     C*
125300961206     C                   Z-ADD     ARBDCM        G08INV                         *CONSEGNA MERCE
125400961206     C                   Z-ADD     *ZEROS        G08DAT
125500961206     C                   MOVEL     '3'           G08ERR
125600961206     C                   CALL      'XSRDA8'
125700961206     C                   PARM                    WLBDA8
125800961206     C                   Z-ADD     G08DAT        DTCDCM
125900961206     C*
126000970916     C* FINE PAGINA
126100970916IF  3C     *IN01         IFEQ      *ON
126200970916     C                   WRITE     SD88TE                                       *TESTATA
126300970916     C                   MOVEL     DEC(1)        INCTOT
126400970916     C                   Z-ADD     DEPLNA        INCDET
126500970916     C                   MOVEL     DECLNA        INDDET
126600970916     C                   Z-ADD     DEPOSA        INCOSA
126700970916     C                   Z-ADD     DEPOS1        INCOS1
126800970916     C                   WRITE     SD88IG                                       *INTEST.NE GENERALE
126900970916     C                   WRITE     SD88ID                                       *INTEST.NE DETTAGLIO
127000970916     C                   SETOFF                                       01
127100970916E   3C                   ENDIF
127200961206     C                   WRITE     SD88DT                                       *DETTAGLIO
127300970916E   2C                   ENDIF
127400961206E   1C                   ENDIF
127500961206     C*
127600961206     C                   ENDSR
127700961206     C*------------------------------------------------------------------------*
127800961206     C* STATOF - STAMPA TOTALI FILIALE
127900961206     C*------------------------------------------------------------------------*
128000961206     C     STATOF        BEGSR
128100961206     C*
128200961206     C                   MOVEL     DEC(1)        INCTOT                         *PER INTESTAZIONE
128300961206     C                   Z-ADD     DEPLNA        INCDET
128400961206     C                   MOVEL     DECLNA        INDDET
128500961206     C                   Z-ADD     DEPOSA        INCOSA
128600961206     C                   Z-ADD     DEPOS1        INCOS1
128700961206     C* PASSA I TOTALI
128800961206     C                   Z-ADD     FILTBL        WTBL                           *TOT. BOLLE
128900961206     C                   Z-ADD     FILTCR        WTCR                           *TOT. CONS. RICHIEST
129000961206     C                   Z-ADD     FILLAV        WLAV                             -LASCIATO AVVISO
129100961206     C                   Z-ADD     FILLEG        WLEG                             -L.AVVISO+GIACENZA
129200961206     C                   Z-ADD     FILGCA        WGCA                             -GIACENZA
129300961206     C                   Z-ADD     FILFFD        WFFD                             -FERMO DEPOSITO
129400961206     C                   Z-ADD     FILSMK        WSMK                             -SUPERMARKET
129500961206     C                   Z-ADD     FILAPP        WAPP                             -APPUNTAMENTO
129600961206     C                   Z-ADD     FILRIT        WRIT                             -ARRIVO RITARDO 1^
129700961206     C                   Z-ADD     FILRI2        WRI2                             -ARRIVO RITARDO 2^
129800961206     C                   Z-ADD     FILCAS        WCAS                             -CONTRASSEGNO
129900961206     C                   Z-ADD     FILTCT        WTCT                           *TOT. CONS.RICH.TOT.
130000961206     C                   Z-ADD     FILTIT        WTIT                           *TOT. ARRIVI RIT. 1^
130100961206     C                   Z-ADD     FILTI2        WTI2                           *TOT. ARRIVI RIT. 2^
130200961206     C                   Z-ADD     FILTTC        WTTC                           *TOT. TURNI CHIUSURA
130300961206     C                   Z-ADD     FILTFD        WTFD                           *TOT. FERMI DEPOSITI
130400961206     C                   Z-ADD     FILTPT        WTPT                           *TOT. CONS. PRIMA 13
130500961206     C                   Z-ADD     FILTPM        WTPM                           *TOT. CONS. X POMERI
130600961206     C                   Z-ADD     FILTDT        WTDT                           *TOT. CONS. DOPO 13
130700961206     C                   Z-ADD     FILTEG        WTEG                           *TOT. EVEN.SENZA GIA
130800970115     C                   Z-ADD     FILTPG        WTPG                           *TOT.+EVEN.SENZA GIA
130900961206     C* STAMPA I TOTALI
131000961206     C                   EXSR      STATOT
131100961206     C*
131200961206     C                   ENDSR
131300961206     C*------------------------------------------------------------------------*
131400961206     C* STATOG - STAMPA TOTALI GENERALI
131500961206     C*------------------------------------------------------------------------*
131600961206     C     STATOG        BEGSR
131700961206     C*
131800961206     C                   MOVEL     DEC(2)        INCTOT                         *PER INTESTAZIONE
131900961206     C                   Z-ADD     *ZEROS        INCDET
132000961206     C                   MOVEL     *BLANKS       INDDET
132100961206     C                   Z-ADD     *ZEROS        INCOSA
132200961206     C                   Z-ADD     *ZEROS        INCOS1
132300961206     C* PASSA I TOTALI
132400961206     C                   Z-ADD     GENTBL        WTBL                           *TOT. BOLLE
132500961206     C                   Z-ADD     GENTCR        WTCR                           *TOT. CONS. RICHIEST
132600961206     C                   Z-ADD     GENLAV        WLAV                             -LASCIATO AVVISO
132700961206     C                   Z-ADD     GENLEG        WLEG                             -L.AVVISO+GIACENZA
132800961206     C                   Z-ADD     GENGCA        WGCA                             -GIACENZA
132900961206     C                   Z-ADD     GENFFD        WFFD                             -FERMO DEPOSITO
133000961206     C                   Z-ADD     GENSMK        WSMK                             -SUPERMARKET
133100961206     C                   Z-ADD     GENAPP        WAPP                             -APPUNTAMENTO
133200961206     C                   Z-ADD     GENRIT        WRIT                             -ARRIVO RITARDO 1^
133300961206     C                   Z-ADD     GENRI2        WRI2                             -ARRIVO RITARDO 2^
133400961206     C                   Z-ADD     GENCAS        WCAS                             -CONTRASSEGNO
133500961206     C                   Z-ADD     GENTCT        WTCT                           *TOT. CONS.RICH.TOT.
133600961206     C                   Z-ADD     GENTIT        WTIT                           *TOT. ARRIVI RIT. 1^
133700961206     C                   Z-ADD     GENTI2        WTI2                           *TOT. ARRIVI RIT. 2^
133800961206     C                   Z-ADD     GENTTC        WTTC                           *TOT. TURNI CHIUSURA
133900961206     C                   Z-ADD     GENTFD        WTFD                           *TOT. FERMI DEPOSITI
134000961206     C                   Z-ADD     GENTPT        WTPT                           *TOT. CONS. PRIMA 13
134100961206     C                   Z-ADD     GENTPM        WTPM                           *TOT. CONS. X POMERI
134200961206     C                   Z-ADD     GENTDT        WTDT                           *TOT. CONS. DOPO 13
134300961206     C                   Z-ADD     GENTEG        WTEG                           *TOT. EVEN.SENZA GIA
134400970115     C                   Z-ADD     GENTPG        WTPG                           *TOT.+EVEN.SENZA GIA
134500961206     C* STAMPA I TOTALI
134600961206     C                   EXSR      STATOT
134700961206     C*
134800961206     C                   ENDSR
134900961206     C*------------------------------------------------------------------------*
135000961206     C* STATOT - STAMPA TOTALI
135100961206     C*------------------------------------------------------------------------*
135200961206     C     STATOT        BEGSR
135300961206     C*
135400961206     C* STAMPA IL TOTALE A PAGINA NUOVA SE NE HA GIA' STAMPATI DUE SU UNA STESSA
135500961206     C* E SE STO STAMPANDO I SOLI TOTALI  O  SE STO STAMPANDO ANCHE IL DETTAGLIO
135600961206     C                   ADD       1             $TOTST                         *N° TOTALI STAMPATI
135700961206     C     $TOTST        DIV       2             COMODO            7 0
135800961206     C                   MVR                     RESTO             3 0
135900961206IF  1C     PARDET        IFEQ      'N'                                          *SOLO TOTALI
136000961206     C     RESTO         ANDGT     *ZEROS                                       *GIA' STAMPATI DUE
136100970902     C     PARDET        OREQ      'S'                                          *ANCHE DETTAGLIO
136200961206     C                   WRITE     SD88TE                                       *TESTATA
136300961206E   1C                   ENDIF
136400961206     C*
136500961206     C                   WRITE     SD88IG                                       *INTEST.NE GENERALE
136600961206     C*
136700961206     C                   Z-ADD     *ZEROS        $NUM              9 0          *CAMPO DI COMODO
136800961206     C                   CLEAR                   SD88TO                         *PULISCE RIGA STAMPA
136900961206     C*--------------------
137000961206     C* N U M E R I   E   P E R C E N T U A L I
137100961206     C*--------------------
137200961206     C                   Z-ADD     WTBL          TOCTBL
137300961206     C*
137400961206     C                   Z-ADD     WTCR          TOCTCR
137500961206IF  1C     WTCR          IFGT      *ZEROS
137600961206     C     100           MULT      WTCR          $NUM
137700961206     C     $NUM          DIV       WTBL          TOPTCR
137800961206E   1C                   ENDIF
137900961206     C*
138000961206     C                   Z-ADD     WLAV          TOCLAV
138100961206IF  1C     WLAV          IFGT      *ZEROS
138200961206     C     100           MULT      WLAV          $NUM
138300961206     C     $NUM          DIV       WTBL          TOPLAV
138400961206E   1C                   ENDIF
138500961206     C*
138600961206     C                   Z-ADD     WLEG          TOCLEG
138700961206IF  1C     WLEG          IFGT      *ZEROS
138800961206     C     100           MULT      WLEG          $NUM
138900961206     C     $NUM          DIV       WTBL          TOPLEG
139000961206E   1C                   ENDIF
139100961206     C*
139200961206     C                   Z-ADD     WGCA          TOCGCA
139300961206IF  1C     WGCA          IFGT      *ZEROS
139400961206     C     100           MULT      WGCA          $NUM
139500961206     C     $NUM          DIV       WTBL          TOPGCA
139600961206E   1C                   ENDIF
139700961206     C*
139800961206     C                   Z-ADD     WFFD          TOCFFD
139900961206IF  1C     WFFD          IFGT      *ZEROS
140000961206     C     100           MULT      WFFD          $NUM
140100961206     C     $NUM          DIV       WTBL          TOPFFD
140200961206E   1C                   ENDIF
140300961206     C*
140400961206     C                   Z-ADD     WSMK          TOCSMK
140500961206IF  1C     WSMK          IFGT      *ZEROS
140600961206     C     100           MULT      WSMK          $NUM
140700961206     C     $NUM          DIV       WTBL          TOPSMK
140800961206E   1C                   ENDIF
140900961206     C*
141000961206     C                   Z-ADD     WAPP          TOCAPP
141100961206IF  1C     WAPP          IFGT      *ZEROS
141200961206     C     100           MULT      WAPP          $NUM
141300961206     C     $NUM          DIV       WTBL          TOPAPP
141400961206E   1C                   ENDIF
141500961206     C*
141600961206     C                   Z-ADD     WRIT          TOCRIT
141700961206IF  1C     WRIT          IFGT      *ZEROS
141800961206     C     100           MULT      WRIT          $NUM
141900961206     C     $NUM          DIV       WTBL          TOPRIT
142000961206E   1C                   ENDIF
142100961206     C*
142200961206     C                   Z-ADD     WRI2          TOCRI2
142300961206IF  1C     WRI2          IFGT      *ZEROS
142400961206     C     100           MULT      WRI2          $NUM
142500961206     C     $NUM          DIV       WTBL          TOPRI2
142600961206E   1C                   ENDIF
142700961206     C*
142800961206     C                   Z-ADD     WCAS          TOCCAS
142900961206IF  1C     WCAS          IFGT      *ZEROS
143000961206     C     100           MULT      WCAS          $NUM
143100961206     C     $NUM          DIV       WTBL          TOPCAS
143200961206E   1C                   ENDIF
143300961206     C*
143400961206     C                   Z-ADD     WTCT          TOCTCT
143500961206IF  1C     WTCT          IFGT      *ZEROS
143600961206     C     100           MULT      WTCT          $NUM
143700961206     C     $NUM          DIV       WTBL          TOPTCT
143800961206E   1C                   ENDIF
143900961206     C*
144000961206     C                   Z-ADD     WTFD          TOCTFD
144100961206IF  1C     WTFD          IFGT      *ZEROS
144200961206     C     100           MULT      WTFD          $NUM
144300961206     C     $NUM          DIV       WTBL          TOPTFD
144400961206E   1C                   ENDIF
144500961206     C*
144600961206     C                   Z-ADD     WTIT          TOCTIT
144700961206IF  1C     WTIT          IFGT      *ZEROS
144800961206     C     100           MULT      WTIT          $NUM
144900961206     C     $NUM          DIV       WTBL          TOPTIT
145000961206E   1C                   ENDIF
145100961206     C*
145200961206     C                   Z-ADD     WTI2          TOCTI2
145300961206IF  1C     WTI2          IFGT      *ZEROS
145400961206     C     100           MULT      WTI2          $NUM
145500961206     C     $NUM          DIV       WTBL          TOPTI2
145600961206E   1C                   ENDIF
145700961206     C*
145800961206     C                   Z-ADD     WTTC          TOCTTC
145900961206IF  1C     WTTC          IFGT      *ZEROS
146000961206     C     100           MULT      WTTC          $NUM
146100961206     C     $NUM          DIV       WTBL          TOPTTC
146200961206E   1C                   ENDIF
146300961206     C*
146400961206     C                   Z-ADD     WTPT          TOCTPT
146500961206IF  1C     WTPT          IFGT      *ZEROS
146600961206     C     100           MULT      WTPT          $NUM
146700961206     C     $NUM          DIV       WTBL          TOPTPT
146800961206E   1C                   ENDIF
146900961206     C*
147000961206     C                   Z-ADD     WTPM          TOCTPM
147100961206IF  1C     WTPM          IFGT      *ZEROS
147200961206     C     100           MULT      WTPM          $NUM
147300961206     C     $NUM          DIV       WTBL          TOPTPM
147400961206E   1C                   ENDIF
147500961206     C*
147600961206     C                   Z-ADD     WTDT          TOCTDT
147700961206IF  1C     WTDT          IFGT      *ZEROS
147800961206     C     100           MULT      WTDT          $NUM
147900961206     C     $NUM          DIV       WTBL          TOPTDT
148000961206E   1C                   ENDIF
148100961206     C*
148200961206     C                   Z-ADD     WTEG          TOCTEG
148300961206IF  1C     WTEG          IFGT      *ZEROS
148400961206     C     100           MULT      WTEG          $NUM
148500961206     C     $NUM          DIV       WTBL          TOPTEG
148600961206E   1C                   ENDIF
148700970115     C*
148800970115     C                   Z-ADD     WTPG          TOCTPG
148900970115IF  1C     WTPG          IFGT      *ZEROS
149000970115     C     100           MULT      WTPG          $NUM
149100970115     C     $NUM          DIV       WTBL          TOPTPG
149200970115E   1C                   ENDIF
149300961206     C*
149400961206     C                   WRITE     SD88TO                                       *TOTALI
149500961206     C*
149600961206     C                   ENDSR
149700961206     C*------------------------------------------------------------------------*
149800961206     C* STAPAR - STAMPA I PARAMETRI DI LANCIO
149900961206     C*------------------------------------------------------------------------*
150000961206     C     STAPAR        BEGSR
150100961206     C***
150200961206     C* LINEA DI ARRIVO
150300961206     C***
150400961206IF  1C     PARLNA        IFGT      *ZEROS
150500961206     C                   Z-ADD     PARLNA        PACLNA
150600961206     C                   SETOFF                                           28
150700961206     C                   Z-ADD     1             J
150800961206     C     PARLNA        LOOKUP    LNA(J)                                 28
150900961206IF  2C     *IN28         IFEQ      *ON
151000961206     C                   MOVEL     DLNA(J)       PADLNA
151100961206X   2C                   ELSE
151200961206     C                   MOVEL     *ALL'*'       PADLNA
151300961206E   2C                   ENDIF
151400961206X   1C                   ELSE
151500961206     C                   MOVEL     'TUTTE'       PADLNA
151600961206E   1C                   ENDIF
151700961206     C***
151800961206     C* DATA SPEDIZIONE DAL - AL
151900961206     C***
152000961206     C                   Z-ADD     PARDTI        G08INV
152100961206     C                   Z-ADD     *ZEROS        G08DAT
152200961206     C                   MOVEL     '3'           G08ERR
152300961206     C                   CALL      'XSRDA8'
152400961206     C                   PARM                    WLBDA8
152500961206     C                   Z-ADD     G08DAT        PACDTI
152600961206     C*
152700961206     C                   Z-ADD     PARDTF        G08INV
152800961206     C                   Z-ADD     *ZEROS        G08DAT
152900961206     C                   MOVEL     '3'           G08ERR
153000961206     C                   CALL      'XSRDA8'
153100961206     C                   PARM                    WLBDA8
153200961206     C                   Z-ADD     G08DAT        PACDTF
153300961206     C***
153400961206     C* DETTAGLIO BOLLE
153500961206     C***
153600961206IF  1C     PARDET        IFEQ      'S'
153700961206     C                   MOVEL     'SI'          PADDET
153800961206X   1C                   ELSE
153900961206     C                   MOVEL     'NO'          PADDET
154000961206E   1C                   ENDIF
154100970916     C***
154200970916     C* CAUSALI IN DETTAGLIO
154300970916     C***
154400970916     C                   MOVEL     PARCD1        PACCD1
154500970916     C                   MOVEL     PARCD2        PACCD2
154600970916     C                   MOVEL     PARCD3        PACCD3
154700970916     C                   MOVEL     PARCD4        PACCD4
154800970916     C                   MOVEL     PARCD5        PACCD5
154900970916     C                   MOVEL     PARCD6        PACCD6
155000970916     C                   MOVEL     PARCD7        PACCD7
155100970916     C                   MOVEL     PARCD8        PACCD8
155200970916     C                   MOVEL     PARCD9        PACCD9
155300970916     C                   MOVEL     PARCD0        PACCD0
155400970916     C                   MOVEL     PARCDA        PACCDA
155500970916     C                   MOVEL     PARCDB        PACCDB
155600970916     C                   MOVEL     PARCDC        PACCDC
155700970916     C                   MOVEL     PARCDD        PACCDD
155800970916     C                   MOVEL     PARCDE        PACCDE
155900970916     C                   MOVEL     PARCDF        PACCDF
156000970916     C                   MOVEL     PARCDG        PACCDG
156100970916     C                   MOVEL     PARCDH        PACCDH
156200970916     C                   MOVEL     PARCDI        PACCDI
156300970916     C                   MOVEL     PARCDL        PACCDL
156400961206     C*
156500961206     C                   WRITE     SD88TE                                       *TESTATA
156600961206     C                   WRITE     SD88PA                                       *PARAMETRI
156700961206     C* REIMPOSTA OVERFLOW
156800961206     C                   MOVEL     '1'           *IN01
156900961206     C*
157000961206     C                   ENDSR
157100961206     C*------------------------------------------------------------------------*
157200961206     C* CARTAB - CARICA LE TABELLE OCCORRENTI
157300961206     C*------------------------------------------------------------------------*
157400961206     C     CARTAB        BEGSR
157500961206     C***
157600961206     C* CAUSALE EVENTI
157700961206     C***
157800020717     C                   Z-ADD     1             KTBKUT                         *CODICE UTENTE
157900961206     C                   MOVEL     '2A'          KTBCOD                         *CODICE TABELLA
158000970916     C                   Z-ADD     *ZEROS        I                 4 0
158100961206     C     KEYTA2        SETLL     TABEL00F
158200961206     C     KEYTA2        READE     TABEL00F                               99
158300961206DO  1C     *IN99         DOWEQ     *OFF
158400961206IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATO
158500961206     C                   MOVEL     TBLUNI        DS2A
158600961206     C*
158700961206     C* CARICA SOLO GLI EVENTI DI TIPO GIACENZA (GIACENZA E LASCIATO AVVISO)
158800961206IF  3C     §2ATIP        IFEQ      'G'
158900961206     C                   ADD       1             I
159000961206     C                   MOVEL     TBLKEY        CEV(I)                         *CODICE EVENTO
159100961206     C                   MOVEL     §2AFTC        TEV(I)                         *TIPO EVENTO (G/A)
159200961206E   3C                   ENDIF
159300961206E   2C                   ENDIF
159400961206     C     KEYTA2        READE     TABEL00F                               99
159500961206E   1C                   ENDDO
159600971002     C***
159700971002     C* PARTICOLARITA' GIACENZE
159800971002     C***
159900971002     C                   Z-ADD     *ZEROS        I                 4 0
160000020717     C                   Z-ADD     1             KTBKUT                         *CODICE UTENTE
160100971002     C                   MOVEL     '7Q'          KTBCOD                         *CODICE TABELLA
160200971002     C     KEYTA2        SETLL     TABEL00F
160300971002     C     KEYTA2        READE     TABEL00F                               99
160400971002DO  1C     *IN99         DOWEQ     *OFF
160500971002IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATO
160600971002     C                   MOVEL     TBLUNI        DS7Q
160700971002     C                   ADD       1             I
160800971002     C                   MOVEL     TBLKEY        GGA(I)                         *CODICE
160900971002     C                   MOVEL     §7Q2MF        G2MF(I)                        *FASE INVIO MESSAGGI
161000971002     C                   MOVEL     §7Q2MQ        G2MQ(I)                        *QUANDO INVIO
161100971002     C                   MOVEL     §7Q5DI        G5DI(I)                        *CODICE DISPOSIZIONE
161200971002     C                   MOVEL     §7Q4D1        G4D1(I)                        *COD DISPOSIZIONE 1
161300971002     C                   MOVEL     §7Q4D2        G4D2(I)                        *COD DISPOSIZIONE 1
161400971002     C                   MOVEL     §7Q4D3        G4D3(I)                        *COD DISPOSIZIONE 1
161500971002     C                   MOVEL     §7Q4D4        G4D4(I)                        *COD DISPOSIZIONE 1
161600971002     C                   MOVEL     §7Q4D5        G4D5(I)                        *COD DISPOSIZIONE 1
161700971002E   2C                   ENDIF
161800971002     C     KEYTA2        READE     TABEL00F                               99
161900971002E   1C                   ENDDO
162000961206     C***
162100961206     C* SOGLIE ARRIVO TRAZIONE
162200961206     C***
162300961206     C* NB: SE C'E LA 1^ SOGLIA (OSA), C'E' ANCHE LA 2^ SOGLIA (OSA1)
162400961206     C                   Z-ADD     *ZEROS        I
162500020717     C                   Z-ADD     1             KTBKUT                         *CODICE UTENTE
162600961206     C                   MOVEL     '1X'          KTBCOD                         *CODICE TABELLA
162700961206     C     KEYTA2        SETLL     TABEL00F
162800961206     C     KEYTA2        READE     TABEL00F                               99
162900961206DO  1C     *IN99         DOWEQ     *OFF
163000961206     C                   MOVEL     TBLKEY        TABKEY
163100961206IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATO
163200961206     C     TABOSA        ANDEQ     'OSA'                                        *SOGLIA TRAZIONE
163300961206IF  3C     TABFIL        IFEQ      *BLANKS                                      *OSA/1 STANDARD
163400961206     C                   MOVEL     *ZEROS        TABFIL                         *FILIALE = 000
163500961206E   3C                   ENDIF
163600961206     C                   MOVEL     TBLUNI        DS1X
163700961206IF  3C     TABPOS        IFEQ      *BLANKS                                      *1 ^SOGLIA
163800961206     C                   ADD       1             I
163900961206     C                   MOVEL     TABFIL        TRA(I)                         *FILIALE
164000961206     C                   Z-ADD     §1XHHO        OSA(I)                         *ORA SOGLIA
164100961206X   3C                   ELSE                                                   *2^ SOGLIA
164200961206     C                   Z-ADD     §1XHHO        OSA1(I)                        *ORA SOGLIA
164300961206E   3C                   ENDIF
164400961206E   2C                   ENDIF
164500961206     C     KEYTA2        READE     TABEL00F                               99
164600961206E   1C                   ENDDO
164700961206     C*
164800961206     C                   ENDSR
164900961206     C*------------------------------------------------------------------------*
165000961206     C* IMPSTA - IMPOSTAZIONI STAMPA
165100961206     C*------------------------------------------------------------------------*
165200961206     C     IMPSTA        BEGSR
165300961206     C***
165400961206     C* RIEMPE LA SCHIERA DELLA/E FILIALE/I DA ELABORARE :
165500961206     C*   - SE E' STATA SELEZIONATA UNA FILIALE -->
165600961206     C*        . ELABORA SOLAMENTE LA FILALE SELEZIONATA
165700961206     C*   - SE NON E' STATA SELEZIONATA ALCUNA FILIALE -->
165800020415     C*        . SE 1° LIVELLO --> ELABORA LE FILIALI PRESENTI IN "£1"
165900961206     C*        . SE 2° LIVELLO --> ELABORA LE FILIALI PRESENTI IN "£6"
166000961206     C***
166100961206IF  1C     PARLNA        IFNE      *ZEROS                                       *SELEZ. UNA FILIALE
166200961206     C                   Z-ADD     PARLNA        LNA(1)
166300961206X   1C                   ELSE                                                   *NESSUNA SELEZIONE
166400961206     C* 1° LIVELLO
166500020717IF  2C     Dutlpo        IFEQ      '1'
166600961206     C                   CLEAR                   DSUL06
166700020415     C                   MOVEL     '£1'          D06COD                         *CODICE TABELLA
166800020717     C                   MOVEL     dutpou        D06KEY                         *FILIALE ELABORATORE
166900961206     C                   MOVEL     DSUL06        KPJBU
167000961206     C                   CALL      'TRUL06R'
167100961206     C                   PARM                    KPJBA
167200961206     C                   MOVEL     KPJBU         DSUL06
167300961206     C                   MOVEA     LIN           LNA
167400961206     C* 2° LIVELLO
167500961206X   2C                   ELSE
167600961206     C                   CLEAR                   DSUL06
167700961206     C                   MOVEL     '£6'          D06COD                         *CODICE TABELLA
167800020717     C                   MOVEL     dutpou        D06KEY                         *FILIALE ELABORATORE
167900961206     C                   MOVEL     DSUL06        KPJBU
168000961206     C                   CALL      'TRUL06R'
168100961206     C                   PARM                    KPJBA
168200961206     C                   MOVEL     KPJBU         DSUL06
168300961206     C                   MOVEA     LIN           LNA
168400961206E   2C                   ENDIF
168500961206E   1C                   ENDIF
168600961206     C* DECODIFICA FILIALI
168700961206DO  1C     1             DO        30            J                 4 0
168800961206IF  2C     LNA(J)        IFGT      *ZEROS
168900961206     C                   Z-ADD     LNA(J)        KORFIL
169000961206     C     KEYORG        CHAIN     AZORG01L                           99
169100961206IF  3C     *IN99         IFEQ      *OFF
169200020627     C                   MOVEL     ORGDE3        OG143
169300961206     C                   MOVEL     ORGDES        DLNA(J)
169400020702     C*
169500020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
169600020702     C                   CLEAR                   DNTW
169700020702     C*
169800020702     C                   MOVEL(P)  'NTW'         KTECOD
169900020702     C                   MOVEL(P)  §OGNTW        KTEKE1
170000020702     C     KTBE          CHAIN     TNTBE01L
170100020702     C                   IF        %found(TNTBE01L)
170200020702     C                   MOVEL     TBEUNI        DNTW
170300020702     C                   ENDIF
170400020702     C*
170500020702     C                   MOVEL     §NTWFIE       LFL1(J)
170600020702     C                   IF        §OGNTW = 'DPD'
170700020702     C                   MOVEL     'E'           LFL1(J)
170800020702     C                   ENDIF
170900020627     C*
171000961206X   3C                   ELSE
171100961206     C                   MOVEL     *ALL'*'       DLNA(J)
171200971204     C                   MOVEL     *BLANKS       LFL1(J)
171300961206E   3C                   ENDIF
171400961206E   2C                   ENDIF
171500961206E   1C                   ENDDO
171600010411     C*
171700961206     C                   ENDSR
171800010411     C*------------------------------------------------------------------------*
171900010411     C* CHKPAR - CONSIDERAZIONI SU PARAMETRI RICEVUTI IN INPUT
172000010411     C*------------------------------------------------------------------------*
172100010411     C     CHKPAR        BEGSR
172200010411     C*
172300010411     C* VERIFICO IL PASSAGGIO PARAMETRI PER STABILIRE SE E' UN LANCIO DA VIDEO
172400010411     C     KPJBU         IFNE      *BLANKS
172500010411     C                   MOVEL     KPJBU         PARAM
172600010411     C                   ELSE
172700010411     C* IMPOSTO IL FLAG CHE IDENTIFICA IL TIPO DI LANCIO EFFETTUATO
172800010411     C                   MOVEL     '1'           TIPLAN
172900010411     C* ESEGUE IL COMANDO DI SOVRAPPOSIZIONE DEL PRINTER FILE PER INDIRIZZAMENTO SU CODA DI SEDE
173000010411     C                   Z-ADD     80            LENGH            15 5
173100030923      * per s.i. Filtra201 la coda di stampa deve essere diversa
173200030923     c                   If        knsif = 'FILTRA201 '
173300030923     C                   CALL(e)   'QCMDEXC'
173400030923     C                   PARM                    CMD(3)
173500030923     C                   PARM                    LENGH
173600030923     c                   Else
173700010411     C                   CALL(e)   'QCMDEXC'
173800010411     C                   PARM                    CMD(1)
173900010411     C                   PARM                    LENGH
174000030923     c                   EndIf
174100010411     C* FORZO LA VALORIZZAZIONE DEI PARAMETRI X IL LANCIO AUTOMATICO
174200010411     C                   MOVEL     'N'           PARDET
174300010411     C* REPERISCE LA DATA CORRENTE
174400010411     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
174500010411     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
174600010411     C                   Z-ADD     N8            G08DAT
174700010411     C                   Z-ADD     *ZEROS        G08INV
174800010411     C                   MOVEL     '0'           G08ERR
174900010411     C                   Z-ADD     *ZEROS        G08TGI
175000010411     C                   CALL      'XSRDA8'
175100010411     C                   PARM                    WLBDA8
175200010411     C                   Z-ADD     G08INV        DATCOR            8 0
175300010411     C                   Z-ADD     G08INV        PARDTF
175400010411     C* TRASFORMA LA DATA IN GIORNI
175500010411     C                   Z-ADD     DATCOR        G08INV
175600010411     C                   Z-ADD     *ZEROS        G08DAT
175700010411     C                   MOVEL     '3'           G08ERR
175800010411     C                   Z-ADD     *ZEROS        G08TGI
175900010411     C                   CALL      'XSRDA8'
176000010411     C                   PARM                    WLBDA8
176100020910     C* TOGLIE 7 GIORNI ALLA DATA CORRENTE E LA RITRASFORMA IN DATA (AAAA/MM/GG)
176200010411     C     G08TGI        SUB       7             G08TGI
176300010411     C                   Z-ADD     *ZEROS        GI8DAT
176400010411     C                   Z-ADD     *ZEROS        GI8INV
176500010411     C                   Z-ADD     G08TGI        GI8TGI
176600010411     C                   CALL      'XSRGI8'
176700010411     C                   PARM                    WGIDA8
176800020910     C                   Z-ADD     GI8DAT        G08DAT
176900020910     C                   Z-ADD     *ZEROS        G08INV
177000020910     C                   MOVEL     '0'           G08ERR
177100020910     C                   Z-ADD     *ZEROS        G08TGI
177200020910     C                   CALL      'XSRDA8'
177300020910     C                   PARM                    WLBDA8
177400020910     C                   Z-ADD     G08INV        PARDTI            8 0
177500010411     C                   ENDIF
177600010411     C*
177700010411     C                   ENDSR
177800961206     C*------------------------------------------------------------------------*
177900961206     C* *INZSR - OPERAZIONI INIZIALI
178000961206     C*------------------------------------------------------------------------*
178100961206     C     *INZSR        BEGSR
178200961206     C*--------------------
178300961206     C* RICEVIMENTO PARAMETRI
178400961206     C*--------------------
178500961206     C     *ENTRY        PLIST
178600961206     C                   PARM                    KPJBA
178700961206     C*--------------------
178800961206     C* CAMPI RIFERITI AL DATA BASE
178900961206     C*--------------------
179000961206     C     *LIKE         DEFINE    ARBLNA        KARLNA                         *FNARB00F
179100961206     C     *LIKE         DEFINE    ARBAAS        KARAAS
179200961206     C     *LIKE         DEFINE    ARBMGS        KARMGS
179300051115     C     *LIKE         DEFINE    AR9AAS        KA9AAS                         *FiAR900F
179400961206     C     *LIKE         DEFINE    AR9LNP        KA9LNP
179500961206     C     *LIKE         DEFINE    AR9NRS        KA9NRS
179600961206     C     *LIKE         DEFINE    AR9NSP        KA9NSP
179700961206     C     *LIKE         DEFINE    ARDAAS        KADAAS                         *FNARBD0F
179800961206     C     *LIKE         DEFINE    ARDLNP        KADLNP
179900961206     C     *LIKE         DEFINE    ARDNRS        KADNRS
180000961206     C     *LIKE         DEFINE    ARDNSP        KADNSP
180100961206     C     *LIKE         DEFINE    ARGAAS        KAGAAS                         *FNARBG0F
180200961206     C     *LIKE         DEFINE    ARGLNP        KAGLNP
180300961206     C     *LIKE         DEFINE    ARGNRS        KAGNRS
180400961206     C     *LIKE         DEFINE    ARGNSP        KAGNSP
180500990810     C     *LIKE         DEFINE    EVBAAS        KEVAAS                         *FNEVB00F
180600961206     C     *LIKE         DEFINE    EVBLNP        KEVLNP
180700961206     C     *LIKE         DEFINE    EVBNRS        KEVNRS
180800961206     C     *LIKE         DEFINE    EVBNSP        KEVNSP
180900050310     C     *LIKE         DEFINE    GCPAAS        KGCAAS                         *TIGCP01L
181000961206     C     *LIKE         DEFINE    GCPLNP        KGCLNP
181100961206     C     *LIKE         DEFINE    GCPNRS        KGCNRS
181200961206     C     *LIKE         DEFINE    GCPNSP        KGCNSP
181300961206     C     *LIKE         DEFINE    FVVNPG        KFVNPG                         *FNFVV00F
181400961206     C     *LIKE         DEFINE    FVVNFV        KFVNFV
181500020912     c     *Like         Define    FvvFgs        KfvFgs
181600961206     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
181700961206     C     *LIKE         DEFINE    TBLCOD        KTBCOD
181800961206     C     *LIKE         DEFINE    TBLKEY        KTBKEY
181900961206     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
182000010124     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
182100010124     C     *LIKE         DEFINE    TBEKE1        KTEKE1
182200010124     C     *LIKE         DEFINE    TBEKE1        KTEKE2
182300010124     C     *LIKE         DEFINE    TBELIN        KTELIN
182400010124     C     *LIKE         DEFINE    TBESIF        KTESIF
182500961206     C*--------------------
182600961206     C* CAMPI RIFERITI A CAMPI DEL PGM
182700961206     C*--------------------
182800961206     C     *LIKE         DEFINE    FILTBL        GENTBL                         *TOTALI GENERALI
182900961206     C     *LIKE         DEFINE    FILTCR        GENTCR
183000961206     C     *LIKE         DEFINE    FILLAV        GENLAV
183100961206     C     *LIKE         DEFINE    FILLEG        GENLEG
183200961206     C     *LIKE         DEFINE    FILGCA        GENGCA
183300961206     C     *LIKE         DEFINE    FILFFD        GENFFD
183400961206     C     *LIKE         DEFINE    FILSMK        GENSMK
183500961206     C     *LIKE         DEFINE    FILAPP        GENAPP
183600961206     C     *LIKE         DEFINE    FILRIT        GENRIT
183700961206     C     *LIKE         DEFINE    FILRI2        GENRI2
183800961206     C     *LIKE         DEFINE    FILCAS        GENCAS
183900961206     C     *LIKE         DEFINE    FILTCT        GENTCT
184000961206     C     *LIKE         DEFINE    FILTIT        GENTIT
184100961206     C     *LIKE         DEFINE    FILTI2        GENTI2
184200961206     C     *LIKE         DEFINE    FILTTC        GENTTC
184300961206     C     *LIKE         DEFINE    FILTFD        GENTFD
184400961206     C     *LIKE         DEFINE    FILTPT        GENTPT
184500961206     C     *LIKE         DEFINE    FILTPM        GENTPM
184600961206     C     *LIKE         DEFINE    FILTDT        GENTDT
184700961206     C     *LIKE         DEFINE    FILTEG        GENTEG
184800970115     C     *LIKE         DEFINE    FILTPG        GENTPG
184900961206     C     *LIKE         DEFINE    FILTBL        WTBL                           *DI LAVORO
185000961206     C     *LIKE         DEFINE    FILTCR        WTCR
185100961206     C     *LIKE         DEFINE    FILLAV        WLAV
185200961206     C     *LIKE         DEFINE    FILLEG        WLEG
185300961206     C     *LIKE         DEFINE    FILGCA        WGCA
185400961206     C     *LIKE         DEFINE    FILFFD        WFFD
185500961206     C     *LIKE         DEFINE    FILSMK        WSMK
185600961206     C     *LIKE         DEFINE    FILAPP        WAPP
185700961206     C     *LIKE         DEFINE    FILRIT        WRIT
185800961206     C     *LIKE         DEFINE    FILRI2        WRI2
185900961206     C     *LIKE         DEFINE    FILCAS        WCAS
186000961206     C     *LIKE         DEFINE    FILTCT        WTCT
186100961206     C     *LIKE         DEFINE    FILTIT        WTIT
186200961206     C     *LIKE         DEFINE    FILTI2        WTI2
186300961206     C     *LIKE         DEFINE    FILTTC        WTTC
186400961206     C     *LIKE         DEFINE    FILTFD        WTFD
186500961206     C     *LIKE         DEFINE    FILTPT        WTPT
186600961206     C     *LIKE         DEFINE    FILTPM        WTPM
186700961206     C     *LIKE         DEFINE    FILTDT        WTDT
186800961206     C     *LIKE         DEFINE    FILTEG        WTEG
186900970115     C     *LIKE         DEFINE    FILTPG        WTPG
187000961206     C*--------------------
187100961206     C* CHIAVI DI LETTURA
187200961206     C*--------------------
187300961206     C* FNARB26L - PARZIALE
187400961206     C     KEYARB        KLIST
187500961206     C                   KFLD                    KARLNA                         *LINEA DI ARRIVO
187600961206     C                   KFLD                    KARAAS                         *ANNO SPEDIZIONE
187700961206     C                   KFLD                    KARMGS                         *MESE/GIORNO SPEDIZ.
187800051115     C* FiAR901L - COMPLETA
187900961206     C     KEYAR9        KLIST
188000961206     C                   KFLD                    KA9AAS                         *ANNO SPEDIZIONE
188100961206     C                   KFLD                    KA9LNP                         *LINEA DI PARTENZA
188200961206     C                   KFLD                    KA9NRS                         *SERIE
188300961206     C                   KFLD                    KA9NSP                         *SPEDIZIONE
188400991124     C* FIARBF1C / FNARBD0F - PARZIALE
188500961206     C     KEYARD        KLIST
188600961206     C                   KFLD                    KADAAS                         *ANNO SPEDIZIONE
188700961206     C                   KFLD                    KADLNP                         *LINEA DI PARTENZA
188800961206     C                   KFLD                    KADNRS                         *SERIE
188900961206     C                   KFLD                    KADNSP                         *SPEDIZIONE
189000991124     C* FIARBF1C / FNARBG0F - PARZIALE
189100961206     C     KEYARG        KLIST
189200961206     C                   KFLD                    KAGAAS                         *ANNO SPEDIZIONE
189300961206     C                   KFLD                    KAGLNP                         *LINEA DI PARTENZA
189400961206     C                   KFLD                    KAGNRS                         *SERIE
189500961206     C                   KFLD                    KAGNSP                         *SPEDIZIONE
189600990810     C* FNEVB01L - PARZIALE
189700961206     C     KEYEVB        KLIST
189800990810     C                   KFLD                    KEVAAS                         *ANNO SPEDIZIONE
189900961206     C                   KFLD                    KEVLNP                         *LINEA DI PARTENZA
190000961206     C                   KFLD                    KEVNRS                         *SERIE
190100961206     C                   KFLD                    KEVNSP                         *SPEDIZIONE
190200050310     C* TIGCP01L - PARZIALE
190300050310     C     KEYGCP        KLIST
190400961206     C                   KFLD                    KGCAAS                         *ANNO SPEDIZIONE
190500961206     C                   KFLD                    KGCLNP                         *LINEA DI PARTENZA
190600961206     C                   KFLD                    KGCNRS                         *SERIE
190700961206     C                   KFLD                    KGCNSP                         *SPEDIZIONE
190800961206     C* FNFVV01L - PARZIALE
190900961206     C     KEYFVV        KLIST
191000961206     C                   KFLD                    KFVNPG                         *CATEGORIA FOGLIO
191100961206     C                   KFLD                    KFVNFV                         *NUMERO FOGLIO
191200020912     c                   Kfld                    kfvfgs                          p.o. gestione
191300961206     C* TABEL00F - COMPLETA
191400961206     C     KEYTAB        KLIST
191500961206     C                   KFLD                    KTBKUT                         *UTENTE
191600961206     C                   KFLD                    KTBCOD                         *CODICE TABELLA
191700961206     C                   KFLD                    KTBKEY                         *RECORD TABELLA
191800961206     C* TABEL00F - PARZIALE
191900961206     C     KEYTA2        KLIST
192000961206     C                   KFLD                    KTBKUT                         *UTENTE
192100961206     C                   KFLD                    KTBCOD                         *CODICE TABELLA
192200010124     C* TNTBE01L - COMPLETA
192300010124     C     KEYTBE        KLIST
192400010124     C                   KFLD                    KTECOD                         *CODICE TABELLA
192500010124     C                   KFLD                    KTEKE1                         *CHIAVE TABELLA 1
192600010124     C                   KFLD                    KTEKE2                         *CHIAVE TABELLA 2
192700010124     C                   KFLD                    KTELIN                         *LINGUA TABELLA
192800010124     C                   KFLD                    KTESIF                         *SISTEMA INFORMATIVO
192900010822     C* TNTBE01L - PARZIALE
193000010822     C     KTBE          KLIST
193100010822     C                   KFLD                    KTECOD                         *CODICE TABELLA
193200010822     C                   KFLD                    KTEKE1                         *CHIAVE TABELLA 1
193300010822     C                   KFLD                    KTEKE2                         *CHIAVE TABELLA 2
193400961206     C* AZORG01L - COMPLETA
193500961206     C     KEYORG        KLIST
193600961206     C                   KFLD                    KORFIL                         *FILIALE
193700961206     C*--------------------
193800961206     C* CAMPI DI CONTROLLO
193900961206     C*--------------------
194000961206     C                   MOVEL     '0'           $FINE             1            *FINE CICLO
194100961206     C                   MOVEL     'N'           WRECOK            1            *RECORD NON/VALIDO
194200961206     C                   MOVEL     '0'           $MEMF             1            *MEMORIZZATO QC X FI
194300961206     C                   MOVEL     '0'           $MEMG             1            *MEMORIZZATO QC GENE
194400970902     C                   MOVEL     '0'           $CR               1            *MEMORIZZA PARTICOLA
194500970902     C                   MOVEL     '0'           $LG               1
194600970902     C                   MOVEL     '0'           $FD               1
194700970902     C                   MOVEL     '0'           $TC               1
194800970902     C                   MOVEL     '0'           $GC               1
194900970902     C                   MOVEL     '0'           $EG               1
195000970902     C                   MOVEL     '0'           $PG               1
195100970902     C                   MOVEL     '0'           §TCR              1
195200970901     C                   MOVEL     '0'           §LAV              1
195300970901     C                   MOVEL     '0'           §LEG              1
195400970901     C                   MOVEL     '0'           §GCA              1
195500970901     C                   MOVEL     '0'           §FFD              1
195600970901     C                   MOVEL     '0'           §SMK              1
195700970901     C                   MOVEL     '0'           §APP              1
195800970901     C                   MOVEL     '0'           §RIT              1
195900970901     C                   MOVEL     '0'           §RI2              1
196000970901     C                   MOVEL     '0'           §CAS              1
196100970901     C                   MOVEL     '0'           §TCT              1
196200970901     C                   MOVEL     '0'           §TFD              1
196300970901     C                   MOVEL     '0'           §TIT              1
196400970901     C                   MOVEL     '0'           §TI2              1
196500970901     C                   MOVEL     '0'           §TTC              1
196600970901     C                   MOVEL     '0'           §TPT              1
196700970901     C                   MOVEL     '0'           §TPM              1
196800970901     C                   MOVEL     '0'           §TDT              1
196900970901     C                   MOVEL     '0'           §TEG              1
197000970901     C                   MOVEL     '0'           §TPG              1
197100970916     C                   MOVEL     'N'           WSTDET            1            *STAMPA DETTAGLIO
197200011211     C*                  MOVEL     'N'           WABATM            1            *ABILITAZ.AUTOM. C/A
197300971003     C                   MOVEL     'N'           WGIACV            1            *GIACENZA
197400971003     C                   Z-ADD     *ZEROS        $TOTST            4 0          *N° TOTALI STAMPATI
197500961206     C                   MOVEL     '1'           *IN01                          *OVERFLOW
197600971009     C                   MOVEL     '0'           WSD99             1            *LANCIO TNSD99R
197700010126     C                   MOVEL     '0'           TIPLAN            1            *TIPO LANCIO
197800961206     C*
197900961206     C* RECUPERA I DATI DELLA FILIALE
198000020717
198100020717     c     *dtaara       define    §azute        azuteds
198200020717     c     *dtaara       define    §datiute      ddatiute
198300020717
198400020717     c                   in(E)     *dtaara
198500020717     c                   if        %Error or rsut = *blanks
198600020717     c                   clear                   tibs34ds
198700020717     c                   call      'TIBS34R'
198800020717     c                   parm                    Tibs34Ds
198900020717     c                   in        *dtaara
199000020717     c                   endif
199100010411     C*
199200010411     C* CONSIDERAZIONI SU PARAMETRI RICEVUTI IN INPUT
199300010411     C                   EXSR      CHKPAR
199400961206     C*
199500961206     C* CARICA LE TABELLE OCCORRENTI
199600961206     C                   EXSR      CARTAB
199700961206     C*
199800961206     C* IMPOSTAZIONI STAMPA
199900961206     C                   EXSR      IMPSTA
200000961206     C*
200100961206     C                   ENDSR
200200961206     O*------------------------------------------------------------------------*
200300961206     O* DECODIFICHE IN STAMPA
200400961206     O*------------------------------------------------------------------------*
200500010124**  -DEC-
200600970902P.OPERATIVO         1
200700961206GENERALE            2
2008009712043
2009009612064
2010009612065
201100010124** CMD - COMANDI CLP
201200010125OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TNSD88P) OUTQ(SETRAS)
201300010124DLTOVR FILE(TNSD88P) LVL(*)
201400030923OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TNSD88P) OUTQ(P046179S)
201500010124
201600010124
