000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - TRAINI - ROUTINE DI GESTIONE
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL02D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL02DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FAZCAE01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300000000     F*------------
001400000000     F* TRAINI
001500000000     F*------------
001600000000     FTNTLT01L  UF A E           K DISK
001700000000     F                                     COMMIT
001800000000     F*------------
001900000000     F* FERMATE
002000000000     F*------------
002100000000     FTNTLR01L  UF A E           K DISK
002200000000     F                                     COMMIT
002300000000     F*------------
002400000000     F* ORARI
002500000000     F*------------
002600000000     FTNTLO01L  UF A E           K DISK
002700000000     F                                     COMMIT
002800000000     F*------------
002900000000     F* MC ASSEGNATI
003000000000     F*------------
003100000000     FTNTLM01L  UF A E           K DISK
003200000000     F                                     COMMIT
003300000000     F*------------
003400000000     F* LISTINI
003500000000     F*------------
003600010723     FTITLL01L  UF A E           K DISK
003700000000     F                                     COMMIT
003800000000     F*------------
003900000000     F* NOTE
004000000000     F*------------
004100000000     FTNTLN01L  UF A E           K DISK
004200000000     F                                     COMMIT
004300110506     F*------------
004400110506     F* Punti di scambio
004500110506     F*------------
004600110506     FTNTLS01L  UF A E           K DISK
004700110506     F                                     COMMIT
004800000000     D*------------------------------------------------------------------------*
004900000000     D* SCHIERE
005000000000     D*------------------------------------------------------------------------*
005100000000     D*-------------------
005200000000     D* MESSAGGI E FUNZIONI
005300000000     D*-------------------
005400000000     D MSG             S             78    DIM(40) CTDATA PERRCD(1)             *MESSAGGI
005500000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
005600000000     D*-------------------
005700000000     D* DECODIFICA OPZIONE RICHIAMATA
005800000000     D*-------------------
005900000000     D DOPZ            S             20    DIM(5) CTDATA PERRCD(1)              *DECODIFICA OPZIONE
006000000000     D*-------------------
006100000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
006200000000     D*-------------------
006300000000     D FFU             S              6    DIM(31)                              *FUNZIONE
006400000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
006500000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
006600000000     D*-------------------
006700000000     D* TABELLA DESCRIZIONI A VIDEO
006800000000     D*-------------------
006900000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
007000110506     D fih             S              1    DIM(8)                               *FUNZIONI A VIDEO
007100000000     D*-------------------
007200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
007300000000     D*-------------------
007400000000     D KEYS            S              1    DIM(40)
007500000000     D*-------------------
007600000000     D* FILIALI
007700000000     D*-------------------
007800971125     D ORG             S              3  0 DIM(500)                             *CODICE
007900971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
008000000518     D PORG            S              1    DIM(500)                             *S=P.O. POSTE
008100000000     D*-------------------
008200000000     D* TIPI TRAINI
008300000000     D*-------------------
008400000000     D TTR             S              1    DIM(20)                              *TIPI TRAINI
008500000000     D DTTR            S             30    DIM(20)                              *DESCRIZIONE
008600000000     D DEF             S              1    DIM(20)                              *DEFLUENZA
008700000000     D*-------------------
008800000000     D* TIPI AUTOMEZZI
008900000000     D*-------------------
009000000000     D TMZ             S              2    DIM(20)                              *CODICE
009100000000     D DTMZ            S             30    DIM(20)                              *DESCRIZIONE
009200000000     D*------------------------------------------------------------------------*
009300000000     D* INPUT
009400000000     D*------------------------------------------------------------------------*
009500000000     D*-------------------
009600000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
009700000000     D*-------------------
009800000000     D DSFUN         E DS
009900000000     D  FUNRIG                 1     40
010000000000     D*-------------------
010100000000     D* DS PER GESTIONE TASTI FUNZIONALI
010200000000     D*-------------------
010300000000     D DSKEYS        E DS
010400000000     D  KEYRIG                 1     40
010500000000     D*-------------------
010600000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
010700000000     D*-------------------
010800000000     D                SDS
010900000000     D  USER                 254    263
011000000000     D*-------------------
011100000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
011200000000     D*-------------------
011300000000     D DEVDS1          DS
011400000000     D  KEY                  369    369
011500000000     D  CURSOR               370    371
011600000000     D  FRTSFL               378    379B 0
011700000000     D*-------------------
011800000000     D* ARCHITETTURA
011900000000     D*-------------------
012000000000     D KPJBA         E DS
012100000000     D*---------------------
012200000000     D* DS ESTERNA DI PROCEDURA
012300000000     D*---------------------
012400000000     D DS00          E DS                  EXTNAME(TNTL00DS)
012500000000     D*-------------------
012600000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
012700000000     D*-------------------
012800000000     D DSMSG         E DS
012900000000     D*---------------------
013000000000     D* DS TIPI TRAINI
013100000000     D*---------------------
013200000000     D DSTV          E DS
013300000000     D*---------------------
013400000000     D* DS TIPI AUTOMEZZI
013500000000     D*---------------------
013600000000     D DSTU          E DS
013700000000     D*---------------------
013800000000     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
013900000000     D*---------------------
014000000000     D OG148         E DS
014100000518     D*---
014200000518     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
014300000518     D*---
014400000518     D OG143         E DS
014500971020     D*------------------
014600971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
014700971020     D*------------------
014800971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
014900020715
015000020715     d DdatiUte      e ds
015100020715     d AzuteDs       e ds                  ExtName(Azute00f)
015200020715     d Tibs34Ds      e ds                  Inz
015300020715
015400000000     D*------------------
015500000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
015600000000     D*------------------
015700000000     D WLBDA8          DS
015800000000     D  G08DAT                 1      8  0
015900000000     D  G08INV                 9     16  0
016000000000     D  G08ERR                17     17
016100000000     D  G08TGI                18     22  0
016200000000     D*-------------------
016300000000     D* DS OPERAZIONI DATA (8)
016400000000     D*-------------------
016500000000     D WGIDA8          DS
016600000000     D  GI8DAT                 1      8  0
016700000000     D  GI8INV                 9     16  0
016800000000     D  GI8TGI                17     21  0
016900000000     D*------------------
017000000000     D* DS UNIONE CAMPI VIDEO - GIORNI DELLA SETTIMANA
017100000000     D*------------------
017200000000     D                 DS
017300000000     D  V1CGS1                 1      1
017400000000     D  V1CGS2                 2      2
017500000000     D  V1CGS3                 3      3
017600000000     D  V1CGS4                 4      4
017700000000     D  V1CGS5                 5      5
017800000000     D  V1CGS6                 6      6
017900000000     D  V1CGS7                 7      7
018000000000     D  V1CGSE                 1      7
018100000000     D*------------------
018200000000     D* DS SCOMPOSIZIONE CAMPI VIDEO - MESSAGGIO VIDEO N° 16
018300000000     D*------------------
018400000000     D                 DS
018500000000     D  ERRNUM                31     31
018600000000     D  ERRGIO                68     77
018700000000     D  ERRM16                 1     78
018800000000     D*------------------
018900000000     D* DS SCOMPOSIZIONE CAMPI VIDEO - MESSAGGIO VIDEO N° 23
019000000000     D*------------------
019100000000     D                 DS
019200000000     D  ERRTFP                73     75
019300000000     D  ERRM23                 1     78
019400000000     D*------------------
019500000000     D* COSTANTI - GIORNI DELLA SETTIMANA
019600000000     D*------------------
019700000000     D CLUN            C                   CONST('LUNEDI    ')
019800000000     D CMAR            C                   CONST('MARTEDI   ')
019900000000     D CMER            C                   CONST('MERCOLEDI ')
020000000000     D CGIO            C                   CONST('GIOVEDI   ')
020100000000     D CVEN            C                   CONST('VENERDI   ')
020200000000     D CSAB            C                   CONST('SABATO    ')
020300000000     D CDOM            C                   CONST('DOMENICA  ')
020400000000     C*------------------------------------------------------------------------*
020500000000     C* MAIN LINE
020600000000     C*------------------------------------------------------------------------*
020700000000     C*
020800000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCE DAL PGM
020900000000IF  1C     $FINE         IFEQ      '1'
021000000000     C                   MOVEL     '1'           D00ERR
021100000000X   1C                   ELSE
021200000000     C*
021300000000DO  2C     $FINE         DOWEQ     '0'
021400000000     C                   MOVEL     '0'           $ERR
021500000000     C*
021600000000IF  3C     $GES          IFEQ      'V1'
021700000000     C                   EXSR      GESV1
021800000000E   3C                   ENDIF
021900000000     C*
022000000000E   2C                   ENDDO
022100000000E   1C                   ENDIF
022200000000     C*
022300000000IF  1C* SE E' STATO PREMUTO F03 IMPOSTA FLAG DELLA DS DI PROCEDURA
022400000000IF  1C     $F03          IFEQ      '1'
022500000000     C                   MOVEL     '1'           D00F03
022600000000E   1C                   ENDIF
022700000000IF  1C     $F12          IFEQ      '1'
022800000000     C                   MOVEL     '1'           D00F12
022900000000E   1C                   ENDIF
023000000000     C*
023100000000     C* ESEGUE IL ROLBK PER TORNARE INDIETRO CON LE MODIFICHE (SE CE NE SONO)
023200000000     C                   ROLBK
023300971020     C*
023400971020     C* OPERAZIONI FINALI
023500971020     C                   EXSR      FINSR
023600000000     C*
023700000000     C                   MOVEL     DS00          KPJBU
023800000000     C                   SETON                                        LR
023900000000     C*------------------------------------------------------------------------*
024000000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
024100000000     C*------------------------------------------------------------------------*
024200000000     C     CARV1         BEGSR
024300000000     C*
024400000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
024500000000     C*
024600000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
024700000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
024800000000     C*
024900000000     C                   WRITE     TL02DVT                                      *TESTATA
025000000000     C                   WRITE     TL02DVF                                      *FUNZIONI
025100000000     C*
025200000000     C                   ENDSR
025300000000     C*------------------------------------------------------------------------*
025400000000     C* GESV1  - GESTIONE PRIMO VIDEO
025500000000     C*------------------------------------------------------------------------*
025600000000     C     GESV1         BEGSR
025700000000     C*
025800000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
025900000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
026000000000E   1C                   ENDIF
026100000000     C*
026200000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
026300000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
026400000000E   1C                   ENDIF
026500000000     C***
026600000000IF  1C     $INS          IFEQ      '1'
026700000000     C                   SETON                                            90
026800000000E   1C                   ENDIF
026900000000     C***
027000000000     C                   EXFMT     TL02DV1
027100000000     C*
027200000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
027300000000     C*
027400000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
027500000000SL  2C                   SELECT
027600000000     C     KEY           WHENEQ    ENTER
027700000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
027800000000IF  3C     DEPOP1        IFEQ      'O05'
027900000000     C                   MOVEL     '1'           $FINE
028000000000E   3C                   ENDIF
028100110519     C     KEY           WHENEQ    F2
028200110519     C                   EXSR      F02GES                                       *punti scambio
028300000000     C     KEY           WHENEQ    F6
028400000000     C                   EXSR      F06GES                                       *GEST F6 CONFERMA
028500000000     C     KEY           WHENEQ    F8
028600000000     C                   EXSR      F08GES                                       *GEST. FERMATE
028700000000     C     KEY           WHENEQ    F9
028800000000     C                   EXSR      F09GES                                       *GEST. MC ASSEGNATI
028900000000     C     KEY           WHENEQ    F10
029000000000     C                   EXSR      F10GES                                       *GEST. LISTINI
029100000000     C     KEY           WHENEQ    F11
029200000000     C                   EXSR      F11GES                                       *GEST. NOTE
029300000000     C                   OTHER
029400000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
029500000000E   2C                   ENDSL
029600000000E   1C                   ENDIF
029700000000     C*
029800000000     C                   ENDSR
029900000000     C*------------------------------------------------------------------------*
030000000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
030100000000     C*------------------------------------------------------------------------*
030200000000     C     ENTV1         BEGSR
030300000000     C*
030400000000     C* CONTROLLA I CAMPI IMMESSI
030500000000IF  1C     DEPOP1        IFNE      'O04'                                        *NO X ANNULLAMENTO
030600000000     C     DEPOP1        ANDNE     'O07'                                        *NO X RIPRISTINO
030700000000     C     DEPOP1        ANDNE     'O05'                                        *NO X VISUALIZZAZION
030800000000     C                   EXSR      DECODI
030900000000E   1C                   ENDIF
031000000000     C*
031100000000     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
031200000000IF  1C     $ERR          IFEQ      '0'
031300000000SL  2C                   SELECT
031400000000     C     DEPOP1        WHENEQ    'F10'
031500000000     C                   EXSR      INSERI                                       *INSERIMENTO
031600000000     C     DEPOP1        WHENEQ    'O02'
031700000000     C                   EXSR      MODIFI                                       *MODIFICA
031800000000     C     DEPOP1        WHENEQ    'O04'
031900000000     C                   EXSR      ANNULL                                       *ANNULLAMENTO
032000000000     C     DEPOP1        WHENEQ    'O07'
032100000000     C                   EXSR      RIPRIS                                       *RIPRISTINO
032200000000     C                   OTHER
032300000000E   2C                   ENDSL
032400000000E   1C                   ENDIF
032500000000     C*
032600000000     C                   ENDSR
032700000000     C*------------------------------------------------------------------------*
032800000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA RICHIESTA
032900000000     C*------------------------------------------------------------------------*
033000000000     C     F06GES        BEGSR
033100000000     C*
033200000000     C* FA' QUELLO CHE FA IL TASTO ENTER PIU' LA GESTIONE DEL F06 --->
033300000000     C* CONTROLLA CAMPI E AGGIORNA I FILES SE MODIFICATI
033400000000     C                   EXSR      ENTV1
033500000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
033600000000     C*
033700961017     C* LANCIA IL PGM CHE CONTROLLA LA CONGRUITA' DEI DATI, SE ANNULLMENTO O
033800961017     C* VISUALIZZAZIONE NON LO FA PERCHE' NON C'E' NULLA DI VARIATO
033900000000     C                   MOVEL     '0'           PARERR
034000000000IF  2C     DEPOP1        IFNE      'O04'
034100000000     C     DEPOP1        ANDNE     'O05'
034200000000     C                   EXSR      CONGRU
034300000000E   2C                   ENDIF
034400000000     C***
034500000000     C* SE NON CI SONO ERRORI AGGIORNA I FILES SOTTO IL CONTROLLO DI SINCRONIA
034600000000     C***
034700000000IF  2C     PARERR        IFEQ      '0'
034800000000     C                   COMMIT
034900000000     C                   MOVEL     '1'           $FINE
035000000000X   2C                   ELSE
035100000000     C                   EXSR      CARMSG
035200000000E   2C                   ENDIF
035300000000E   1C                   ENDIF
035400000000     C*
035500000000     C                   ENDSR
035600000000     C*------------------------------------------------------------------------*
035700000000     C* F08GES - GESTIONE TASTO FUNZIONALE F08 ==> GESTIONE FERMATE E ORARI
035800000000     C*------------------------------------------------------------------------*
035900000000     C     F08GES        BEGSR
036000000000     C*
036100000000     C* FA' QUELLO CHE FA IL TASTO ENTER PIU' LA GESTIONE DEL F06 --->
036200000000     C* CONTROLLA CAMPI E AGGIORNA I FILES SE MODIFICATI
036300000000     C                   EXSR      ENTV1
036400000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
036500000000     C*
036600000000     C* LANCIA LA GESTIONE FERMATE E ORARI
036700000000     C                   Z-ADD     V1CDDE        WDA8                           *GIRA LA DATA
036800000000     C                   Z-ADD     *ZEROS        DA8W
036900000000     C                   EXSR      CHKDAT
037000000000     C                   Z-ADD     DA8W          DDEV1C
037100000000     C*
037200000000     C                   Z-ADD     V1CTRN        WTRN                           *NUMERO TRAINO
037300000000     C                   Z-ADD     DDEV1C        WDDE                           *DATA DECORRENZA
037400000000     C*
037500000000     C                   MOVEL     DEPOP0        WOP0                           *LIVELLO PROCEDURA
037600000000     C                   MOVEL     'F08'         WOP1                           *LIVELLO PROGRAMMA
037700000000     C                   MOVEL     'TNTL03R'     WPGM              8            *PGM DI GESTIONE
037800000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
037900000000E   1C                   ENDIF
038000000000     C*
038100000000     C                   ENDSR
038200000000     C*------------------------------------------------------------------------*
038300000000     C* F09GES - GESTIONE TASTO FUNZIONALE F09 ==> GESTIONE METRI CUBI ASSEGNATI
038400000000     C*------------------------------------------------------------------------*
038500000000     C     F09GES        BEGSR
038600000000     C*
038700000000     C* FA' QUELLO CHE FA IL TASTO ENTER PIU' LA GESTIONE DEL F06 --->
038800000000     C* CONTROLLA CAMPI E AGGIORNA I FILES SE MODIFICATI
038900000000     C                   EXSR      ENTV1
039000000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
039100000000     C*
039200000000     C* LANCIA LA GESTIONE METRICUBI ASSEGNATI
039300000000     C                   Z-ADD     V1CDDE        WDA8                           *GIRA LA DATA
039400000000     C                   Z-ADD     *ZEROS        DA8W
039500000000     C                   EXSR      CHKDAT
039600000000     C                   Z-ADD     DA8W          DDEV1C
039700000000     C*
039800000000     C                   Z-ADD     V1CTRN        WTRN                           *NUMERO TRAINO
039900000000     C                   Z-ADD     DDEV1C        WDDE                           *DATA DECORRENZA
040000000000     C*
040100000000     C                   MOVEL     DEPOP0        WOP0                           *LIVELLO PROCEDURA
040200000000     C                   MOVEL     'F09'         WOP1                           *LIVELLO PROGRAMMA
040300000000     C                   MOVEL     'TNTL08R'     WPGM                           *PGM DI GESTIONE
040400000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
040500000000E   1C                   ENDIF
040600000000     C*
040700000000     C                   ENDSR
040800000000     C*------------------------------------------------------------------------*
040900000000     C* F10GES - GESTIONE TASTO FUNZIONALE F10 ==> GESTIONE LISTINI
041000000000     C*------------------------------------------------------------------------*
041100000000     C     F10GES        BEGSR
041200000000     C*
041300000000     C* FA' QUELLO CHE FA IL TASTO ENTER PIU' LA GESTIONE DEL F06 --->
041400000000     C* CONTROLLA CAMPI E AGGIORNA I FILES SE MODIFICATI
041500000000     C                   EXSR      ENTV1
041600000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
041700000000     C*
041800000000     C* LANCIA LA GESTIONE LISTINI
041900000000     C                   Z-ADD     V1CDDE        WDA8                           *GIRA LA DATA
042000000000     C                   Z-ADD     *ZEROS        DA8W
042100000000     C                   EXSR      CHKDAT
042200000000     C                   Z-ADD     DA8W          DDEV1C
042300000000     C*
042400000000     C                   Z-ADD     V1CTRN        WTRN                           *NUMERO TRAINO
042500000000     C                   Z-ADD     DDEV1C        WDDE                           *DATA DECORRENZA
042600000000     C*
042700000000     C                   MOVEL     DEPOP0        WOP0                           *LIVELLO PROCEDURA
042800000000     C                   MOVEL     'F10'         WOP1                           *LIVELLO PROGRAMMA
042900010723     C                   MOVEL     'TNTL38R'     WPGM                           *PGM DI GESTIONE
043000000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
043100000000E   1C                   ENDIF
043200000000     C*
043300000000     C                   ENDSR
043400000000     C*------------------------------------------------------------------------*
043500000000     C* F11GES - GESTIONE TASTO FUNZIONALE F11 ==> GESTIONE NOTE
043600000000     C*------------------------------------------------------------------------*
043700000000     C     F11GES        BEGSR
043800000000     C*
043900000000     C* FA' QUELLO CHE FA IL TASTO ENTER PIU' LA GESTIONE DEL F06 --->
044000000000     C* CONTROLLA CAMPI E AGGIORNA I FILES SE MODIFICATI
044100000000     C                   EXSR      ENTV1
044200000000IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
044300000000     C*
044400000000     C* LANCIA LA GESTIONE NOTE
044500000000     C                   Z-ADD     V1CDDE        WDA8                           *GIRA LA DATA
044600000000     C                   Z-ADD     *ZEROS        DA8W
044700000000     C                   EXSR      CHKDAT
044800000000     C                   Z-ADD     DA8W          DDEV1C
044900000000     C*
045000000000     C                   Z-ADD     V1CTRN        WTRN                           *NUMERO TRAINO
045100000000     C                   Z-ADD     DDEV1C        WDDE                           *DATA DECORRENZA
045200000000     C*
045300000000     C                   MOVEL     DEPOP0        WOP0                           *LIVELLO PROCEDURA
045400000000     C                   MOVEL     'F11'         WOP1                           *LIVELLO PROGRAMMA
045500000000     C                   MOVEL     'TNTL12R'     WPGM                           *PGM DI GESTIONE
045600000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
045700000000E   1C                   ENDIF
045800000000     C*
045900000000     C                   ENDSR
046000110506     C*------------------------------------------------------------------------*
046100110519     C* F02GES - Punti di scambio
046200110506     C*------------------------------------------------------------------------*
046300110519     C     F02GES        BEGSR
046400110506     C*
046500110506     C* FA' QUELLO CHE FA IL TASTO ENTER PIU' LA GESTIONE DEL F06 --->
046600110506     C* CONTROLLA CAMPI E AGGIORNA I FILES SE MODIFICATI
046700110506     C                   EXSR      ENTV1
046800110506IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
046900110506     C*
047000110519     C* LANCIA LA GESTIONE
047100110506     C                   Z-ADD     V1CDDE        WDA8                           *GIRA LA DATA
047200110506     C                   Z-ADD     *ZEROS        DA8W
047300110506     C                   EXSR      CHKDAT
047400110506     C                   Z-ADD     DA8W          DDEV1C
047500110506     C*
047600110506     C                   Z-ADD     V1CTRN        WTRN                           *NUMERO TRAINO
047700110506     C                   Z-ADD     DDEV1C        WDDE                           *DATA DECORRENZA
047800110506     C*
047900110506     C                   MOVEL     DEPOP0        WOP0                           *LIVELLO PROCEDURA
048000110519     C                   MOVEL     'F02'         WOP1                           *LIVELLO PROGRAMMA
048100110506     C                   MOVEL     'TNTLZ3R'     WPGM                           *PGM DI GESTIONE
048200110506     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
048300110506E   1C                   ENDIF
048400110506     C*
048500110506     C                   ENDSR
048600000000     C*------------------------------------------------------------------------*
048700000000     C* INSERI - FUNZIONE DI INSERIMENTO
048800000000     C*------------------------------------------------------------------------*
048900000000     C     INSERI        BEGSR
049000000000     C*
049100000000     C* ESEGUE L'INSERIMENTO SOLO LA PRIMA VOLTA
049200000000IF  1C     $INS          IFEQ      '0'
049300000000     C                   MOVEL     '1'           $INS
049400000000     C                   Z-ADD     V1CTRN        TLTTRN                         *TRAINO
049500000000     C                   Z-ADD     DDEV1C        TLTDDE                         *DECORRENZA
049600000000     C                   Z-ADD     DSCV1C        TLTDSC                         *SCADENZA
049700000000     C                   MOVEL     V1CTFP        TLTTFP                         *TERMINAL PARTENZA
049800000000     C                   MOVEL     V1CTFA        TLTTFA                         *TERMINAL ARRIVO
049900000000     C                   MOVEL     V1CTTR        TLTTTR                         *TIPO TRAINO
050000000000     C                   MOVEL     V1CGSE        TLTGSE                         *GIORNI SETTIMANA
050100000000     C                   MOVEL     V1CTMZ        TLTTMZ                         *TIPO AUTOMEZZO
050200000000     C                   MOVEL     V1CBIS        TLTBIS                         *NORMALE/BIS
050300000000     C                   MOVEL     V1CFES        TLTFES                         *FILIALE IMPORTO
050400000000     C                   Z-ADD     DATCOR        TLTDIR                         *DATA INSERIMENTO
050500000000     C                   Z-ADD     *ZEROS        TLTDUV                         *DATA VARIAZIONE
050600000000     C                   Z-ADD     *ZEROS        TLTDTR                         *DATA TRASMISSIONE
050700000000     C                   MOVEL     *BLANKS       TLTFTR                         *FLAG TRASMISSIONE
050800000000     C                   MOVEL     *BLANKS       TLTATB                         *FLAG ANNULLAMENTO
050900000000     C*
051000000000     C                   WRITE     TNTLT000
051100000000     C* PROTEGGE LA CHIAVE
051200000000     C                   SETON                                        90
051300000000     C*
051400000000     C* LA SECONDA VOLTA FA DELLE MODIFICHE
051500000000X   1C                   ELSE
051600000000     C                   EXSR      MODIFI
051700000000E   1C                   ENDIF
051800000000     C*
051900000000     C                   ENDSR
052000000000     C*------------------------------------------------------------------------*
052100000000     C* MODIFI - FUNZIONE DI MODIFICA
052200000000     C*------------------------------------------------------------------------*
052300000000     C     MODIFI        BEGSR
052400000000     C*
052500000000     C                   Z-ADD     V1CTRN        KLTTRN                         *TRAINO
052600000000     C                   Z-ADD     DDEV1C        KLTDDE                         *DATA DECORRENZA
052700000000     C     KEYTLT        CHAIN     TNTLT01L                           99
052800000000IF  1C     *IN99         IFEQ      *OFF
052900000000     C                   Z-ADD     DSCV1C        TLTDSC                         *SCADENZA
053000000000     C                   MOVEL     V1CTFP        TLTTFP                         *TERMINAL PARTENZA
053100000000     C                   MOVEL     V1CTFA        TLTTFA                         *TERMINAL ARRIVO
053200000000     C                   MOVEL     V1CTTR        TLTTTR                         *TIPO TRAINO
053300000000     C                   MOVEL     V1CGSE        TLTGSE                         *GIORNI SETTIMANA
053400000000     C                   MOVEL     V1CTMZ        TLTTMZ                         *TIPO AUTOMEZZO
053500000000     C                   MOVEL     V1CBIS        TLTBIS                         *NORMALE/BIS
053600000000     C                   MOVEL     V1CFES        TLTFES                         *FILIALE IMPORTO
053700000000     C                   Z-ADD     DATCOR        TLTDUV                         *DATA VARIAZIONE
053800000000     C                   MOVEL     *BLANKS       TLTFTR                         *FLAG TRASMISSIONE
053900000000     C                   MOVEL     *BLANKS       TLTATB                         *FLAG ANNULLAMENTO
054000000000     C*
054100000000     C                   UPDATE    TNTLT000
054200000000E   1C                   ENDIF
054300000000     C*
054400000000     C                   ENDSR
054500000000     C*------------------------------------------------------------------------*
054600000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
054700000000     C*------------------------------------------------------------------------*
054800000000     C     ANNULL        BEGSR
054900000000     C*
055000000000     C* ESEGUE L'ANNULLAMENTO SOLO LA PRIMA VOLTA
055100000000IF  1C     $ANN          IFEQ      '0'
055200000000     C                   MOVEL     '1'           $ANN
055300000000     C*---
055400960930     C* RECORD TRASMESSO     --> ANNULLAMENTO LOGICO
055500960930     C* RECORD MAI TRASMESSO --> ANNULLAMENTO FISICO
055600000000     C*---
055700000000     C                   Z-ADD     V1CTRN        KLTTRN                         *TRAINO
055800000000     C                   Z-ADD     DDEV1C        KLTDDE                         *DATA DECORRENZA
055900000000     C     KEYTLT        CHAIN     TNTLT01L                           99
056000000000IF  2C     *IN99         IFEQ      *OFF
056100000000     C***
056200000000     C* FERMATE
056300000000     C***
056400000000     C* ANNULLA LE FERMATE DEL TRAINO CON STESSA DECORRENZA
056500000000     C                   Z-ADD     V1CTRN        KLRTRN
056600000000     C                   Z-ADD     DDEV1C        KLRDDE
056700000000     C     KEYTLR        SETLL     TNTLR01L
056800000000     C     KEYTLR        READE     TNTLR01L                               99
056900000000DO  3C     *IN99         DOWEQ     *OFF
057000000000IF  4C     TLRATB        IFEQ      *BLANKS
057100000000IF  5C     TLRDTR        IFGT      *ZEROS
057200000000     C                   MOVEL     'A'           TLRATB                         *FLAG ANNULLAMENTO
057300000000     C                   Z-ADD     DATCOR        TLRDUV                         *DATA VARIAZIONE
057400000000     C                   MOVEL     *BLANKS       TLRFTR                         *FLAG TRASMISSIONE
057500000000     C                   UPDATE    TNTLR000
057600000000X   5C                   ELSE
057700000000     C                   DELETE    TNTLR000
057800000000E   5C                   ENDIF
057900000000E   4C                   ENDIF
058000000000     C     KEYTLR        READE     TNTLR01L                               99
058100000000E   3C                   ENDDO
058200000000     C***
058300000000     C* ORARI
058400000000     C***
058500000000     C* ANNULLA GLI ORARI CON STESSA DECORRENZA
058600000000     C                   Z-ADD     V1CTRN        KLOTRN
058700000000     C                   Z-ADD     DDEV1C        KLODDE
058800000000     C     KEYTLO        SETLL     TNTLO01L
058900000000     C     KEYTLO        READE     TNTLO01L                               99
059000000000DO  3C     *IN99         DOWEQ     *OFF
059100000000IF  4C     TLOATB        IFEQ      *BLANKS
059200000000IF  5C     TLODTR        IFGT      *ZEROS
059300000000     C                   MOVEL     'A'           TLOATB                         *FLAG ANNULLAMENTO
059400000000     C                   Z-ADD     DATCOR        TLODUV                         *DATA VARIAZIONE
059500000000     C                   MOVEL     *BLANKS       TLOFTR                         *FLAG TRASMISSIONE
059600000000     C                   UPDATE    TNTLO000
059700000000X   5C                   ELSE
059800000000     C                   DELETE    TNTLO000
059900000000E   5C                   ENDIF
060000000000E   4C                   ENDIF
060100000000     C     KEYTLO        READE     TNTLO01L                               99
060200000000E   3C                   ENDDO
060300000000     C***
060400000000     C* MCUBI ASSEGNATI
060500000000     C***
060600000000     C* ANNULLA I METRICUBI ASSEGNATI CON STESSA DECORRENZA
060700000000     C                   Z-ADD     V1CTRN        KLMTRN
060800000000     C                   Z-ADD     DDEV1C        KLMDDE
060900000000     C     KEYTLM        SETLL     TNTLM01L
061000000000     C     KEYTLM        READE     TNTLM01L                               99
061100000000DO  3C     *IN99         DOWEQ     *OFF
061200000000IF  4C     TLMATB        IFEQ      *BLANKS
061300000000IF  5C     TLMDTR        IFGT      *ZEROS
061400000000     C                   MOVEL     'A'           TLMATB                         *FLAG ANNULLAMENTO
061500000000     C                   Z-ADD     DATCOR        TLMDUV                         *DATA VARIAZIONE
061600000000     C                   MOVEL     *BLANKS       TLMFTR                         *FLAG TRASMISSIONE
061700000000     C                   UPDATE    TNTLM000
061800000000X   5C                   ELSE
061900000000     C                   DELETE    TNTLM000
062000000000E   5C                   ENDIF
062100000000E   4C                   ENDIF
062200000000     C     KEYTLM        READE     TNTLM01L                               99
062300000000E   3C                   ENDDO
062400000000     C***
062500000000     C* LISTINI
062600000000     C***
062700000000     C* ANNULLA I LISTINI CON STESSA DECORRENZA
062800000000     C                   Z-ADD     V1CTRN        KLLTRN
062900000000     C                   Z-ADD     DDEV1C        KLLDDE
063000010723     C     KEYTLL        SETLL     TITLL01L
063100010723     C     KEYTLL        READE     TITLL01L                               99
063200000000DO  3C     *IN99         DOWEQ     *OFF
063300000000IF  4C     TLLATB        IFEQ      *BLANKS
063400000000IF  5C     TLLDTR        IFGT      *ZEROS
063500000000     C                   MOVEL     'A'           TLLATB                         *FLAG ANNULLAMENTO
063600000000     C                   Z-ADD     DATCOR        TLLDUV                         *DATA VARIAZIONE
063700000000     C                   MOVEL     *BLANKS       TLLFTR                         *FLAG TRASMISSIONE
063800010723     C                   UPDATE    TITLL000
063900000000X   5C                   ELSE
064000010723     C                   DELETE    TITLL000
064100000000E   5C                   ENDIF
064200000000E   4C                   ENDIF
064300010723     C     KEYTLL        READE     TITLL01L                               99
064400000000E   3C                   ENDDO
064500000000     C***
064600000000     C* NOTE
064700000000     C***
064800000000     C* ANNULLA LE NOTE CON STESSA DECORRENZA
064900000000     C                   Z-ADD     V1CTRN        KLNTRN
065000000000     C                   Z-ADD     DDEV1C        KLNDDE
065100000000     C     KEYTLN        SETLL     TNTLN01L
065200000000     C     KEYTLN        READE     TNTLN01L                               99
065300000000DO  3C     *IN99         DOWEQ     *OFF
065400000000IF  4C     TLNATB        IFEQ      *BLANKS
065500000000IF  5C     TLNDTR        IFGT      *ZEROS
065600000000     C                   MOVEL     'A'           TLNATB                         *FLAG ANNULLAMENTO
065700000000     C                   Z-ADD     DATCOR        TLNDUV                         *DATA VARIAZIONE
065800000000     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
065900000000     C                   UPDATE    TNTLN000
066000000000X   5C                   ELSE
066100000000     C                   DELETE    TNTLN000
066200000000E   5C                   ENDIF
066300000000E   4C                   ENDIF
066400000000     C     KEYTLN        READE     TNTLN01L                               99
066500000000E   3C                   ENDDO
066600110506     C***
066700110506     C*Punti scambio
066800110506     C***
066900110506     C* ANNULLA         CON STESSA DECORRENZA
067000110506     C                   Z-ADD     V1CTRN        KLNTRN
067100110506     C                   Z-ADD     DDEV1C        KLNDDE
067200110506     C     KEYTLN        SETLL     TNTLS01L
067300110506     C     KEYTLN        READE     TNTLS01L                               99
067400110506DO  3C     *IN99         DOWEQ     *OFF
067500110506IF  4C     TLSATB        IFEQ      *BLANKS
067600110506IF  5C     TLSDTR        IFGT      *ZEROS
067700110506     C                   MOVEL     'A'           TLSATB                         *FLAG ANNULLAMENTO
067800110506     C                   Z-ADD     DATCOR        TLSDUV                         *DATA VARIAZIONE
067900110506     C                   MOVEL     *BLANKS       TLSFTR                         *FLAG TRASMISSIONE
068000110506     C                   UPDATE    TNTLS000
068100110506X   5C                   ELSE
068200110506     C                   DELETE    TNTLS000
068300110506E   5C                   ENDIF
068400110506E   4C                   ENDIF
068500110506     C     KEYTLN        READE     TNTLS01L                               99
068600110506E   3C                   ENDDO
068700000000     C***
068800000000     C* TRAINO
068900000000     C***
069000000000     C* ANNULLA IL TRAINO
069100000000IF  3C     TLTDTR        IFGT      *ZEROS
069200000000     C                   MOVEL     'A'           TLTATB                         *FLAG ANNULLAMENTO
069300000000     C                   Z-ADD     DATCOR        TLTDUV                         *DATA VARIAZIONE
069400000000     C                   MOVEL     *BLANKS       TLTFTR                         *FLAG TRASMISSIONE
069500000000     C                   UPDATE    TNTLT000
069600000000X   3C                   ELSE
069700000000     C                   DELETE    TNTLT000
069800000000E   3C                   ENDIF
069900000000E   2C                   ENDIF
070000000000E   1C                   ENDIF
070100000000     C*
070200000000     C                   ENDSR
070300000000     C*------------------------------------------------------------------------*
070400000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
070500000000     C*------------------------------------------------------------------------*
070600000000     C     RIPRIS        BEGSR
070700000000     C*
070800000000     C* ESEGUE IL RIPRISTINO SOLO LA PRIMA VOLTA
070900000000IF  1C     $RIP          IFEQ      '0'
071000000000     C                   MOVEL     '1'           $RIP
071100000000     C                   Z-ADD     V1CTRN        KLTTRN                         *TRAINO
071200000000     C                   Z-ADD     DDEV1C        KLTDDE                         *DATA DECORRENZA
071300000000     C     KEYTLT        CHAIN     TNTLT01L                           99
071400000000IF  2C     *IN99         IFEQ      *OFF
071500000000     C***
071600000000     C* FERMATE
071700000000     C***
071800000000     C* RIPRISTINA LE FERMATE DEL TRAINO CON STESSA DECORRENZA
071900000000     C                   Z-ADD     V1CTRN        KLRTRN
072000000000     C                   Z-ADD     DDEV1C        KLRDDE
072100000000     C     KEYTLR        SETLL     TNTLR01L
072200000000     C     KEYTLR        READE     TNTLR01L                               99
072300000000DO  3C     *IN99         DOWEQ     *OFF
072400000000IF  4C     TLRDDE        IFEQ      DDEV1C
072500000000IF  5C     TLRATB        IFEQ      'A'
072600000000     C                   MOVEL     *BLANKS       TLRATB                         *FLAG ANNULLAMENTO
072700000000     C                   Z-ADD     DATCOR        TLRDUV                         *DATA VARIAZIONE
072800000000     C                   MOVEL     *BLANKS       TLRFTR                         *FLAG TRASMISSIONE
072900000000     C                   UPDATE    TNTLR000
073000000000E   5C                   ENDIF
073100000000E   4C                   ENDIF
073200000000     C     KEYTLR        READE     TNTLR01L                               99
073300000000E   3C                   ENDDO
073400000000     C***
073500000000     C* ORARI
073600000000     C***
073700000000     C* RIPRISTINA GLI ORARI CON STESSA DECORRENZA
073800000000     C                   Z-ADD     V1CTRN        KLOTRN
073900000000     C                   Z-ADD     DDEV1C        KLODDE
074000000000     C     KEYTLO        SETLL     TNTLO01L
074100000000     C     KEYTLO        READE     TNTLO01L                               99
074200000000DO  3C     *IN99         DOWEQ     *OFF
074300000000IF  4C     TLODDE        IFEQ      DDEV1C
074400000000IF  5C     TLOATB        IFEQ      'A'
074500000000     C                   MOVEL     *BLANKS       TLOATB                         *FLAG ANNULLAMENTO
074600000000     C                   Z-ADD     DATCOR        TLODUV                         *DATA VARIAZIONE
074700000000     C                   MOVEL     *BLANKS       TLOFTR                         *FLAG TRASMISSIONE
074800000000     C                   UPDATE    TNTLO000
074900000000E   5C                   ENDIF
075000000000E   4C                   ENDIF
075100000000     C     KEYTLO        READE     TNTLO01L                               99
075200000000E   3C                   ENDDO
075300000000     C***
075400000000     C* MCUBI ASSEGNATI
075500000000     C***
075600000000     C* RIPRISTINA I METRICUBI ASSEGNATI CON STESSA DECORRENZA
075700000000     C                   Z-ADD     V1CTRN        KLMTRN
075800000000     C                   Z-ADD     DDEV1C        KLMDDE
075900000000     C     KEYTLM        SETLL     TNTLM01L
076000000000     C     KEYTLM        READE     TNTLM01L                               99
076100000000DO  3C     *IN99         DOWEQ     *OFF
076200000000IF  4C     TLMDDE        IFEQ      DDEV1C
076300000000IF  5C     TLMATB        IFEQ      'A'
076400000000     C                   MOVEL     *BLANKS       TLMATB                         *FLAG ANNULLAMENTO
076500000000     C                   Z-ADD     DATCOR        TLMDUV                         *DATA VARIAZIONE
076600000000     C                   MOVEL     *BLANKS       TLMFTR                         *FLAG TRASMISSIONE
076700000000     C                   UPDATE    TNTLM000
076800000000E   5C                   ENDIF
076900000000E   4C                   ENDIF
077000000000     C     KEYTLM        READE     TNTLM01L                               99
077100000000E   3C                   ENDDO
077200000000     C***
077300000000     C* LISTINI
077400000000     C***
077500000000     C* RIPRISTINA I LISTINI CON STESSA DECORRENZA
077600000000     C                   Z-ADD     V1CTRN        KLLTRN
077700000000     C                   Z-ADD     DDEV1C        KLLDDE
077800010723     C     KEYTLL        SETLL     TITLL01L
077900010723     C     KEYTLL        READE     TITLL01L                               99
078000000000DO  3C     *IN99         DOWEQ     *OFF
078100000000IF  4C     TLLDDE        IFEQ      DDEV1C
078200000000IF  5C     TLLATB        IFEQ      'A'
078300000000     C                   MOVEL     *BLANKS       TLLATB                         *FLAG ANNULLAMENTO
078400000000     C                   Z-ADD     DATCOR        TLLDUV                         *DATA VARIAZIONE
078500000000     C                   MOVEL     *BLANKS       TLLFTR                         *FLAG TRASMISSIONE
078600010723     C                   UPDATE    TITLL000
078700000000E   5C                   ENDIF
078800000000E   4C                   ENDIF
078900010723     C     KEYTLL        READE     TITLL01L                               99
079000000000E   3C                   ENDDO
079100000000     C***
079200000000     C* NOTE
079300000000     C***
079400000000     C* RIPRISTINA LE NOTE CON STESSA DECORRENZA
079500000000     C                   Z-ADD     V1CTRN        KLNTRN
079600000000     C                   Z-ADD     DDEV1C        KLNDDE
079700000000     C     KEYTLN        SETLL     TNTLN01L
079800000000     C     KEYTLN        READE     TNTLN01L                               99
079900000000DO  3C     *IN99         DOWEQ     *OFF
080000000000IF  4C     TLNDDE        IFEQ      DDEV1C
080100000000IF  5C     TLNATB        IFEQ      'A'
080200000000     C                   MOVEL     *BLANKS       TLNATB                         *FLAG ANNULLAMENTO
080300000000     C                   Z-ADD     DATCOR        TLNDUV                         *DATA VARIAZIONE
080400000000     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
080500000000     C                   UPDATE    TNTLN000
080600000000E   5C                   ENDIF
080700000000E   4C                   ENDIF
080800000000     C     KEYTLN        READE     TNTLN01L                               99
080900000000E   3C                   ENDDO
081000110506     C***
081100110506     C* Punti di scambio
081200110506     C***
081300110506     C* RIPRISTINA         CON STESSA DECORRENZA
081400110506     C                   Z-ADD     V1CTRN        KLNTRN
081500110506     C                   Z-ADD     DDEV1C        KLNDDE
081600110506     C     KEYTLN        SETLL     TNTLS01L
081700110506     C     KEYTLN        READE     TNTLS01L                               99
081800110506DO  3C     *IN99         DOWEQ     *OFF
081900110506IF  4C     TLSDDE        IFEQ      DDEV1C
082000110506IF  5C     TLSATB        IFEQ      'A'
082100110506     C                   MOVEL     *BLANKS       TLSATB                         *FLAG ANNULLAMENTO
082200110506     C                   Z-ADD     DATCOR        TLSDUV                         *DATA VARIAZIONE
082300110506     C                   MOVEL     *BLANKS       TLSFTR                         *FLAG TRASMISSIONE
082400110506     C                   UPDATE    TNTLS000
082500110506E   5C                   ENDIF
082600110506E   4C                   ENDIF
082700110506     C     KEYTLN        READE     TNTLS01L                               99
082800110506E   3C                   ENDDO
082900000000     C***
083000000000     C* TRAINO
083100000000     C***
083200000000     C* RIPRISTINA IL TRAINO
083300000000     C                   MOVEL     *BLANKS       TLTATB                         *FLAG ANNULLAMENTO
083400000000     C                   Z-ADD     DATCOR        TLTDUV                         *DATA VARIAZIONE
083500000000     C                   MOVEL     *BLANKS       TLTFTR                         *FLAG TRASMISSIONE
083600000000     C*
083700000000     C                   UPDATE    TNTLT000
083800000000E   2C                   ENDIF
083900000000E   1C                   ENDIF
084000000000     C*
084100000000     C                   ENDSR
084200000000     C*------------------------------------------------------------------------*
084300000000     C* CONGRU - CONTROLLA LA CONGRUITA' DEI DATI DI TUTTO IL DATA BASE TRAINI
084400000000     C*------------------------------------------------------------------------*
084500000000     C     CONGRU        BEGSR
084600000000     C*
084700000000     C     *LIKE         DEFINE    D00TRN        PARTRN
084800000000     C     *LIKE         DEFINE    D00DDE        PARDDE
084900000000     C     *LIKE         DEFINE    D00INL        PARINL
085000000000     C     *LIKE         DEFINE    D00ERR        PARERR
085100000000     C     *LIKE         DEFINE    D00MSG        PARMSG
085200000000     C*
085300000000     C* LANCIA IL PROGRAMMA DI CONGRUITA'
085400000000     C                   Z-ADD     V1CTRN        PARTRN                         *NUMERO TRAINO
085500000000     C                   Z-ADD     DDEV1C        PARDDE                         *DATA DECORRENZA
085600000000     C                   MOVEL     WINL          PARINL                         *TIPO TRAINO INOLTRO
085700000000     C                   MOVEL     '0'           PARERR                         *FLAG ERRORE (0/1)
085800000000     C                   MOVEL     *BLANKS       PARMSG                         *MESSAGGIO ERRORE
085900000000     C                   MOVEL     'T'           PARGES            1            *GESTIONE TRAINI
086000000000     C                   CALL      'TNTL50R'
086100000000     C                   PARM                    PARTRN
086200000000     C                   PARM                    PARDDE
086300000000     C                   PARM                    PARINL
086400000000     C                   PARM                    PARERR
086500000000     C                   PARM                    PARMSG
086600000000     C                   PARM                    PARGES
086700000214     C                   PARM                    KPJBA
086800000000     C*
086900000000IF  1C     PARERR        IFEQ      '1'
087000961017IF  2C     DEPOP1        IFEQ      'O07'                                        *IN RIPRISTINO
087100961017     C                   MOVEL     MSG(40)       $MSG                           *MESSAGGIO DIVERSO
087200961017X   2C                   ELSE
087300000000     C                   MOVEL     PARMSG        $MSG
087400961017E   2C                   ENDIF
087500961017E   1C                   ENDIF
087600000000     C*
087700000000     C                   ENDSR
087800000000     C*------------------------------------------------------------------------*
087900000000     C* LANPGM - LANCIA UN PROGRAMMA
088000000000     C*------------------------------------------------------------------------*
088100000000     C     LANPGM        BEGSR
088200000000     C***
088300000000     C* PARAMETRI INFORMATIVI
088400000000     C***
088500000000     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
088600000000     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PROGRAMMA
088700000000     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
088800000000     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
088900000000     C                   MOVEL     '0'           D00ERR                         *ERRORE
089000000000     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
089100000000     C***
089200000000     C* PARAMETRI
089300000000     C***
089400000000     C                   Z-ADD     WTRN          D00TRN                         *TRAINO
089500000000     C                   Z-ADD     WDDE          D00DDE                         *DECORRENZA (AA/M/G)
089600000000     C                   MOVEL     WINL          D00INL                         *TIPO TRAINO INOLTRO
089700000000     C*--------------------
089800000000     C* L A N C I O
089900000000     C*--------------------
090000000000     C                   MOVEL     DS00          KPJBU
090100000000     C                   CALL      WPGM                                 99
090200000000     C                   PARM                    KPJBA
090300000000     C                   MOVEL     KPJBU         DS00
090400000000     C*
090500000000     C* SE RITORNA PREMUTO F3 CHIUDE IL PROGRAMMA, ALTRIMENTI MANDA IL MESSAGGIO
090600000000     C* RICEVUTO DAL PROGRAMMA CHIAMATO
090700000000IF  1C     D00F03        IFEQ      '1'
090800000000     C                   MOVEL     '1'           $F03
090900000000     C                   MOVEL     '1'           $FINE
091000000000E   1C                   ENDIF
091100000000     C*
091200000000     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
091300000000     C                   MOVEL     D00MSG        $MSG
091400000000     C                   EXSR      CARMSG
091500000000     C*
091600000000     C                   ENDSR
091700000000     C*------------------------------------------------------------------------*
091800000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
091900000000     C*------------------------------------------------------------------------*
092000000000     C     DECODI        BEGSR
092100000000     C*?ALCUNI DEI CONTROLLO DI QUESTA ROUTINE SONO RIPRESI NEL "TNTL50R" CHE
092200000000     C*?CONTROLLA LA CONGRUITA' DEL DATABASE TRAINI.
092300000000     C*?LE MODIFICHE QUINIDI DOVRANNO ESSERE APPORTATE ANCHE IN CONSIDERAZIONE
092400000000     C*?DI QUESTO PROGRAMMA.
092500000000     C*
092600000000     C* REIMPOSTA INDICATORI DI ERRORE
092700000000     C                   SETOFF                                       606162
092800000000     C                   SETOFF                                       636465
092900000000     C                   SETOFF                                       666768
093000000000     C                   SETOFF                                       697071
093100000000     C                   SETOFF                                       727374
093200000000     C                   SETOFF                                       75
093300000000     C*
093400000000     C* REIMPOSTA IL FLAG PER EMETTERE IL MESSAGGIO DI AVVERTIMENTO DELL' F12
093500000000     C                   MOVEL     '0'           MSGF12                         *EMETTE MESSAGGIO
093600000000     C*
093700000000DO  1C                   DO
093800000000     C*--------------------
093900000000     C* TRAINO
094000000000     C*--------------------
094100000000IF  2C     V1CTRN        IFLE      *ZEROS
094200000000     C                   MOVEL     '1'           $ERR
094300000000     C                   MOVEL     MSG(8)        $MSG
094400000000     C                   EXSR      CARMSG
094500000000     C                   SETON                                        60
094600000000     C                   LEAVE
094700000000E   2C                   ENDIF
094800000000     C*
094900000000     C* SE SONO IN INSERIMENTO NON POSSO INSERIRE UN TRAINO > 9.000.000 PERCHE'
095000000000     C* QUESTI NUMERI SONO DI COMPETENZA DI UN ALTRO PGM E PRESI DA UNA DATAAREA
095100000000IF  2C     DEPOP1        IFEQ      'F10'
095200000000IF  3C     V1CTRN        IFGT      9000000
095300000000     C                   MOVEL     '1'           $ERR
095400000000     C                   MOVEL     MSG(7)        $MSG
095500000000     C                   EXSR      CARMSG
095600000000     C                   SETON                                        60
095700000000     C                   LEAVE
095800000000E   3C                   ENDIF
095900000000E   2C                   ENDIF
096000000000     C*--------------------
096100000000     C* DATA DECORRENZA
096200000000     C*--------------------
096300000000     C                   Z-ADD     V1CDDE        WDA8
096400000000     C                   Z-ADD     *ZEROS        DA8W
096500000000     C                   EXSR      CHKDAT
096600000000     C                   Z-ADD     WDA8          V1CDDE
096700000000     C                   Z-ADD     DA8W          DDEV1C
096800000000IF  2C     $ERR          IFEQ      '1'
096900000000     C                   MOVEL     MSG(9)        $MSG
097000000000     C                   EXSR      CARMSG
097100000000     C                   SETON                                        61
097200000000     C                   LEAVE
097300000000E   2C                   ENDIF
097400000000     C*--------------------
097500000000     C* DATA SCADENZA
097600000000     C*--------------------
097700000000     C                   Z-ADD     V1CDSC        WDA8
097800000000     C                   Z-ADD     *ZEROS        DA8W
097900000000     C                   EXSR      CHKDAT
098000000000     C                   Z-ADD     WDA8          V1CDSC
098100000000     C                   Z-ADD     DA8W          DSCV1C
098200000000IF  2C     $ERR          IFEQ      '1'
098300000000     C                   MOVEL     MSG(9)        $MSG
098400000000     C                   EXSR      CARMSG
098500000000     C                   SETON                                        62
098600000000     C                   LEAVE
098700000000E   2C                   ENDIF
098800000000     C* SCADENZA > DECORRENZA
098900000000IF  2C     DSCV1C        IFLT      DDEV1C
099000000000     C                   MOVEL     '1'           $ERR
099100000000     C                   MOVEL     MSG(11)       $MSG
099200000000     C                   EXSR      CARMSG
099300000000     C                   SETON                                        6162
099400000000     C                   LEAVE
099500000000E   2C                   ENDIF
099600000000     C* SCADENZA >= DATA CORRENTE
099700000000     C* NO PER I TRAINI > 9.000.000 CHE POSSONO SCADERE PRIMA DI OGGI
099800970526IF  2C     DEPOP1        IFEQ      'O02'
099900970526     C     DEPOP1        OREQ      'O07'
100000970526     C     DEPOP1        OREQ      'F10'
100100970526IF  3C     DSCV1C        IFLT      DATCOR
100200970526     C     V1CTRN        ANDLE     9000000
100300000000     C                   MOVEL     '1'           $ERR
100400000000     C                   MOVEL     MSG(27)       $MSG
100500000000     C                   EXSR      CARMSG
100600000000     C                   SETON                                        62
100700000000     C                   LEAVE
100800970526E   3C                   ENDIF
100900970526E   2C                   ENDIF
101000000000     C*--------------------
101100000000     C* FILIALE PARTENZA
101200000000     C*--------------------
101300000000     C     *LIKE         DEFINE    WTFP          WPTFP
101400000000     C     *LIKE         DEFINE    WTFA          WPTFA
101500000000     C     *LIKE         DEFINE    WFEL          WPFEL
101600000518     C                   MOVEL     'P'           WTTF
101700000000     C                   MOVEL     V1CTFP        WTFX
101800000000     C                   MOVEL     *BLANKS       WDTFX
101900000000     C                   EXSR      CHKTFX
102000000000     C                   MOVEL     WTFX          V1CTFP
102100000000     C                   MOVEL     WDTFX         V1DTFP
102200000000     C                   Z-ADD     WTFP          WPTFP
102300000000     C                   Z-ADD     WTFA          WPTFA
102400000000     C                   Z-ADD     WFEL          WPFEL
102500000000IF  2C     $ERR          IFEQ      '1'
102600000000     C                   MOVEL     MSG(13)       $MSG
102700000000     C                   EXSR      CARMSG
102800000000     C                   SETON                                        63
102900000000     C                   LEAVE
103000000000E   2C                   ENDIF
103100000000     C*
103200000000     C* LA FILIALE PARTENZA NON PUO'ESSERE MODIFICATA SE IL TRAINO E'GIA'STATO
103300980610     C* TRASMESSO IN FILIALE E LA DATA DECORRENZA DEL E' MINORE/UGUALE AD OGGI
103400980610     C* -CIOE' SE NON CI SONO DEI FOGLI VIAGGIO IN GIRO CON QUEL TERMINAL-
103500000000     C                   MOVEL     V1CTFP        NUM3              3 0
103600000000IF  2C     V1HDTR        IFNE      *ZEROS
103700000000     C     NUM3          ANDNE     V1HTFP
103800980610     C     DDEV1C        ANDLE     DATCOR
103900000000     C                   MOVEL     MSG(23)       ERRM23
104000000000     C                   MOVEL     V1HTFP        ERRTFP
104100000000     C                   MOVEL     ERRM23        $MSG
104200000000     C                   EXSR      CARMSG
104300000000     C                   MOVEL     '1'           $ERR
104400000000     C                   SETON                                        63
104500000000     C                   LEAVE
104600000000E   2C                   ENDIF
104700000000     C*
104800000000     C* PROPONE COME FILIALE CHE ESPONE IMPORTO LA FILIALE PARTENZA (SE VUOTA)
104900000000IF  1C     V1CFES        IFEQ      *BLANKS
105000000000     C     V1CFES        OREQ      *ZEROS
105100000000     C                   MOVEL     V1CTFP        V1CFES
105200000000E   2C                   ENDIF
105300000000     C*--------------------
105400000000     C* FILIALE ARRIVO
105500000000     C*--------------------
105600000000     C     *LIKE         DEFINE    WTFP          WATFP
105700000000     C     *LIKE         DEFINE    WTFA          WATFA
105800000000     C     *LIKE         DEFINE    WFEL          WAFEL
105900000518     C                   MOVEL     'A'           WTTF
106000000000     C                   MOVEL     V1CTFA        WTFX
106100000000     C                   MOVEL     *BLANKS       WDTFX
106200000000     C                   EXSR      CHKTFX
106300000000     C                   MOVEL     WTFX          V1CTFA
106400000000     C                   MOVEL     WDTFX         V1DTFA
106500000000     C                   Z-ADD     WTFP          WATFP
106600000000     C                   Z-ADD     WTFA          WATFA
106700000000     C                   Z-ADD     WFEL          WAFEL
106800000000IF  2C     $ERR          IFEQ      '1'
106900000000     C                   MOVEL     MSG(13)       $MSG
107000000000     C                   EXSR      CARMSG
107100000000     C                   SETON                                        64
107200000000     C                   LEAVE
107300000000E   2C                   ENDIF
107400000000     C*--------------------
107500000000     C* TIPO TRAINO
107600000000     C*--------------------
107700000000     C                   MOVEL     V1CTTR        WTTR
107800000000     C                   MOVEL     *BLANKS       WDTTR
107900000000     C                   MOVEL     *BLANKS       WDEF
108000000000     C                   EXSR      CHKTTR
108100000000     C                   MOVEL     WTTR          V1CTTR
108200000000     C                   MOVEL     WDTTR         V1DTTR
108300000000IF  2C     $ERR          IFEQ      '1'
108400000000     C                   MOVEL     MSG(14)       $MSG
108500000000     C                   EXSR      CARMSG
108600000000     C                   SETON                                        65
108700000000     C                   LEAVE
108800000000E   2C                   ENDIF
108900000000     C*
109000000000     C* PGM RICHIAMATO CON CXX -> NON INSERIRE UN TRAINO DI TIPO "CHIUSO"
109100000000IF  2C     WCALL         IFEQ      'C'                                          *PGM RICHIAMATO
109200000000     C     V1CTTR        ANDEQ     'C'                                          *TIPO TRAINO "CHIUSO
109300000000     C                   MOVEL     '1'           $ERR
109400000000     C                   MOVEL     MSG(33)       $MSG
109500000000     C                   EXSR      CARMSG
109600000000     C                   SETON                                        65
109700000000     C                   LEAVE
109800000000E   2C                   ENDIF
109900000000     C*--------------------
110000000000     C* FILIALE PARTENZA + TIPO TRAINO
110100000000     C*--------------------
110200000000     C                   MOVEL     V1CTFP        NUM3              3 0
110300000000IF  2C     V1CTTR        IFEQ      'A'                                          *TRAINO AFFLUENZA
110400971128IF  3C     NUM3          IFNE      WPFEL                                        *NO FIL ELABORATORE
110500000000     C     NUM3          ANDNE     WPTFA                                        *NO TERMINAL ARRIVO
110600000000X   3C                   ELSE
110700000000     C                   MOVEL     '1'           $ERR
110800000000     C                   MOVEL     MSG(19)       $MSG
110900000000     C                   EXSR      CARMSG
111000000000     C                   SETON                                        63
111100000000     C                   LEAVE
111200000000E   3C                   ENDIF
111300000000X   2C                   ELSE                                                   *TRAINO NON AFFLUENZ
111400000000IF  3C     V1CTTR        IFNE      'I'                                          *TRAINO NON INOLTRO
111500971128IF  4C     NUM3          IFEQ      WPFEL                                        *SI FIL ELABORATORE
111600000000X   4C                   ELSE
111700000000     C                   MOVEL     '1'           $ERR
111800000000     C                   MOVEL     MSG(20)       $MSG
111900000000     C                   EXSR      CARMSG
112000000000     C                   SETON                                        63
112100000000     C                   LEAVE
112200000000E   4C                   ENDIF
112300000000E   3C                   ENDIF
112400000000E   2C                   ENDIF
112500000000     C*--------------------
112600000000     C* FILIALE ARRIVO + TIPO TRAINO
112700000000     C*--------------------
112800000000     C                   MOVEL     V1CTFP        NUM3              3 0
112900000000     C                   MOVEL     V1CTFA        NUM3B             3 0
113000000000IF  2C     WDEF          IFEQ      'S'                                          *TRAINO DEFLUENZA
113100000000     C     V1CTTR        OREQ      'L'                                          *TRAINO LOCALE
113200000000     C* LEGGE LE ECCEZIONI
113300000000     C                   MOVEL     '0'           WECC              1            *SI/NO ECCEZIONE
113400000000     C                   EXSR      CTRECC
113500000000IF  3C     NUM3          IFNE      WATFA                                        *FIL PART=TERM ARR
113600000000     C     WECC          ANDNE     '1'                                          *FIL PART=ECC TERM A
113700000000     C                   MOVEL     '1'           $ERR
113800000000E   3C                   ENDIF
113900000000IF  3C     $ERR          IFEQ      '1'
114000000000     C                   MOVEL     MSG(21)       $MSG
114100000000     C                   EXSR      CARMSG
114200000000     C                   SETON                                        64
114300000000     C                   LEAVE
114400000000E   3C                   ENDIF
114500000000X   2C                   ELSE
114600000000IF  3C     V1CTTR        IFEQ      'A'                                          *TRAINO AFFLUENZA
114700000000IF  4C     NUM3B         IFEQ      WPTFP                                        *FIL AR=TERM PAR LNP
114800000000X   4C                   ELSE
114900000000     C                   MOVEL     '1'           $ERR
115000000000     C                   MOVEL     MSG(22)       $MSG
115100000000     C                   EXSR      CARMSG
115200000000     C                   SETON                                        64
115300000000     C                   LEAVE
115400000000E   4C                   ENDIF
115500000000X   3C                   ELSE                                                   *TRAINO ANDATA/CHIUS
115600000000     C* LEGGE L'ECCEZIONE-> TFP TFA TFA
115700000000     C* (ES: TRAINO: TFP=66 TFA=53 --> ECC: TFP=66 TFA=53 TFE=53)
115800960715IF  4C*---       V1CTTR    IFNE 'I'                        *SOLO ANDATA O CHIUSO
115900960715     C* LO FA SOLO PER I TRAINI CHIUSI IN QUANTO PER I SOLO ANDATA E' POSSIBILE
116000960715     C* INSERIRE ANCHE FILIALI NON TERMINAL PURCHE'SIANO FERMATE DI SOLO SCARICO
116100960715     C* E NEI METRICUBI ABBIANO MERCE IN SCARICO SOLO PER SE STESSE (ES.DIRETTI)
116200960715IF  4C     V1CTTR        IFEQ      'C'                                          *CHIUSO
116300000000     C                   MOVEL     '0'           WECC                           *SI/NO ECCEZIONE
116400000000     C                   EXSR      CTREC1
116500000000IF  5C     NUM3B         IFNE      WATFA                                        *FIL ARR E'TERM ARR
116600000000     C     WECC          ANDNE     '1'                                          *FIL ARR E'TER AR EC
116700000000     C                   MOVEL     '1'           $ERR
116800000000     C                   MOVEL     MSG(26)       $MSG
116900000000     C                   EXSR      CARMSG
117000000000     C                   SETON                                        64
117100000000     C                   LEAVE
117200000000E   5C                   ENDIF
117300960715E   4C                   ENDIF
117400960715E   4C*---                 ENDIF
117500000000E   3C                   ENDIF
117600000000E   2C                   ENDIF
117700000000     C*--------------------
117800000000     C* FILIALE PARTENZA + FILIALE ARRIVO + TIPO TRAINO
117900000000     C*--------------------
118000000000IF  2C     V1CTTR        IFEQ      'L'                                          *TRAINO LOCALE
118100000000IF  3C     V1CTFP        IFNE      V1CTFA                                       *PARTENZA <> ARRIVO
118200000000     C                   MOVEL     '1'           $ERR
118300000000     C                   MOVEL     MSG(28)       $MSG
118400000000     C                   EXSR      CARMSG
118500000000     C                   SETON                                        6364
118600000000     C                   LEAVE
118700000000E   3C                   ENDIF
118800000000X   2C                   ELSE
118900000000IF  3C     V1CTTR        IFNE      'I'                                          *TRAINO NO INOLTRO
119000000000IF  4C     V1CTFP        IFEQ      V1CTFA                                       *PARTENZA = ARRIVO
119100000000     C                   MOVEL     '1'           $ERR
119200000000     C                   MOVEL     MSG(29)       $MSG
119300000000     C                   EXSR      CARMSG
119400000000     C                   SETON                                        6364
119500000000     C                   LEAVE
119600000000E   4C                   ENDIF
119700000000E   3C                   ENDIF
119800000000E   2C                   ENDIF
119900000000     C*--------------------
120000000000     C* TIPO INOLTRO
120100000000     C*--------------------
120200000000     C* SE TIPO TRAINO INOLTRO CONTROLLA A QUALE TIPO TRAINO SI PUO' ASSOCIARE
120300000000     C* I POSSIBLI SONO S-SOLO ANDATA, D-DEFLUENZA E L-LOCALE, ALTRIMENTI ERRORE
120400000000     C                   MOVEL     *BLANKS       WINL
120500000000IF  2C     V1CTTR        IFEQ      'I'                                          *INOLTRO
120600000000     C                   MOVEL     V1CTFP        WFIP
120700000000     C                   MOVEL     V1CTFA        WFIA
120800000000     C                   EXSR      CHKINL
120900000000IF  3C     $ERR          IFEQ      '1'
121000000000     C                   MOVEL     MSG(37)       $MSG
121100000000     C                   EXSR      CARMSG
121200000000     C                   SETON                                        636465
121300000000     C                   LEAVE
121400000000E   3C                   ENDIF
121500000000X   2C                   ELSE                                                   *NON INOLTRO
121600000000     C                   MOVEL     V1CTTR        WINL
121700000000E   2C                   ENDIF
121800000000     C*--------------------
121900000000     C* GIORNI DI PARTENZA
122000000000     C*--------------------
122100000000IF  2C     V1CGSE        IFEQ      *BLANKS
122200000000     C                   MOVEL     '1'           $ERR
122300000000     C                   MOVEL     MSG(15)       $MSG
122400000000     C                   EXSR      CARMSG
122500000000     C                   SETON                                        666768
122600000000     C                   SETON                                        697071
122700000000     C                   SETON                                        72
122800000000     C                   LEAVE
122900000000E   2C                   ENDIF
123000000000     C***
123100000000     C* LUNEDI'
123200000000     C***
123300000000IF  2C     V1CGS1        IFNE      *BLANKS
123400000000     C     V1CGS1        ANDNE     '1'
123500000000     C                   MOVEL     MSG(16)       ERRM16
123600000000     C                   MOVEL     '1'           ERRNUM
123700000000     C                   MOVEL     CLUN          ERRGIO
123800000000     C                   MOVEL     ERRM16        $MSG
123900000000     C                   EXSR      CARMSG
124000000000     C                   MOVEL     '1'           $ERR
124100000000     C                   SETON                                        66
124200000000     C                   LEAVE
124300000000E   2C                   ENDIF
124400000000     C***
124500000000     C* MARTEDI'
124600000000     C***
124700000000IF  2C     V1CGS2        IFNE      *BLANKS
124800000000     C     V1CGS2        ANDNE     '2'
124900000000     C                   MOVEL     MSG(16)       ERRM16
125000000000     C                   MOVEL     '2'           ERRNUM
125100000000     C                   MOVEL     CMAR          ERRGIO
125200000000     C                   MOVEL     ERRM16        $MSG
125300000000     C                   EXSR      CARMSG
125400000000     C                   MOVEL     '1'           $ERR
125500000000     C                   SETON                                        67
125600000000     C                   LEAVE
125700000000E   2C                   ENDIF
125800000000     C***
125900000000     C* MECOLEDI'
126000000000     C***
126100000000IF  2C     V1CGS3        IFNE      *BLANKS
126200000000     C     V1CGS3        ANDNE     '3'
126300000000     C                   MOVEL     MSG(16)       ERRM16
126400000000     C                   MOVEL     '3'           ERRNUM
126500000000     C                   MOVEL     CMER          ERRGIO
126600000000     C                   MOVEL     ERRM16        $MSG
126700000000     C                   EXSR      CARMSG
126800000000     C                   MOVEL     '1'           $ERR
126900000000     C                   SETON                                        68
127000000000     C                   LEAVE
127100000000E   2C                   ENDIF
127200000000     C***
127300000000     C* GIOVEDI'
127400000000     C***
127500000000IF  2C     V1CGS4        IFNE      *BLANKS
127600000000     C     V1CGS4        ANDNE     '4'
127700000000     C                   MOVEL     MSG(16)       ERRM16
127800000000     C                   MOVEL     '4'           ERRNUM
127900000000     C                   MOVEL     CGIO          ERRGIO
128000000000     C                   MOVEL     ERRM16        $MSG
128100000000     C                   EXSR      CARMSG
128200000000     C                   MOVEL     '1'           $ERR
128300000000     C                   SETON                                        69
128400000000     C                   LEAVE
128500000000E   2C                   ENDIF
128600000000     C***
128700000000     C* VENERDI'
128800000000     C***
128900000000IF  2C     V1CGS5        IFNE      *BLANKS
129000000000     C     V1CGS5        ANDNE     '5'
129100000000     C                   MOVEL     MSG(16)       ERRM16
129200000000     C                   MOVEL     '5'           ERRNUM
129300000000     C                   MOVEL     CVEN          ERRGIO
129400000000     C                   MOVEL     ERRM16        $MSG
129500000000     C                   EXSR      CARMSG
129600000000     C                   MOVEL     '1'           $ERR
129700000000     C                   SETON                                        70
129800000000     C                   LEAVE
129900000000E   2C                   ENDIF
130000000000     C***
130100000000     C* SABATO
130200000000     C***
130300000000IF  2C     V1CGS6        IFNE      *BLANKS
130400000000     C     V1CGS6        ANDNE     '6'
130500000000     C                   MOVEL     MSG(16)       ERRM16
130600000000     C                   MOVEL     '6'           ERRNUM
130700000000     C                   MOVEL     CSAB          ERRGIO
130800000000     C                   MOVEL     ERRM16        $MSG
130900000000     C                   EXSR      CARMSG
131000000000     C                   MOVEL     '1'           $ERR
131100000000     C                   SETON                                        71
131200000000     C                   LEAVE
131300000000E   2C                   ENDIF
131400000000     C***
131500000000     C* DOMENICA
131600000000     C***
131700000000IF  2C     V1CGS7        IFNE      *BLANKS
131800000000     C     V1CGS7        ANDNE     '7'
131900000000     C                   MOVEL     MSG(16)       ERRM16
132000000000     C                   MOVEL     '7'           ERRNUM
132100000000     C                   MOVEL     CDOM          ERRGIO
132200000000     C                   MOVEL     ERRM16        $MSG
132300000000     C                   EXSR      CARMSG
132400000000     C                   MOVEL     '1'           $ERR
132500000000     C                   SETON                                        72
132600000000     C                   LEAVE
132700000000E   2C                   ENDIF
132800000000     C*--------------------
132900000000     C* TIPO AUTOMEZZO
133000000000     C*--------------------
133100000000     C                   MOVEL     V1CTMZ        WTMZ
133200000000     C                   MOVEL     *BLANKS       WDTMZ
133300000000     C                   EXSR      CHKTMZ
133400000000     C                   MOVEL     WTMZ          V1CTMZ
133500000000     C                   MOVEL     WDTMZ         V1DTMZ
133600000000IF  2C     $ERR          IFEQ      '1'
133700000000     C                   MOVEL     MSG(17)       $MSG
133800000000     C                   EXSR      CARMSG
133900000000     C                   SETON                                        73
134000000000     C                   LEAVE
134100000000E   2C                   ENDIF
134200000000     C*--------------------
134300000000     C* NORMALE / BIS
134400000000     C*--------------------
134500000000IF  2C     V1CBIS        IFEQ      *BLANKS
134600000000     C     V1CBIS        OREQ      'B'
134700000000X   2C                   ELSE
134800000000     C                   MOVEL     '1'           $ERR
134900000000     C                   MOVEL     MSG(18)       $MSG
135000000000     C                   EXSR      CARMSG
135100000000     C                   SETON                                        74
135200000000     C                   LEAVE
135300000000E   2C                   ENDIF
135400000000     C* SE NUMERO TRAINO > 9.000.000 LA DATA DECORRENZA = DATA SCADENZA
135500000000IF  2C     V1CTRN        IFGT      9000000
135600000000IF  3C     DDEV1C        IFNE      DSCV1C
135700000000     C                   MOVEL     '1'           $ERR
135800000000     C                   MOVEL     MSG(24)       $MSG
135900000000     C                   EXSR      CARMSG
136000000000     C                   SETON                                        6162
136100000000     C                   LEAVE
136200000000E   3C                   ENDIF
136300000000E   2C                   ENDIF
136400000000     C*--------------------
136500000000     C* FILIALE CHE ESPONE IMPORTO
136600000000     C*--------------------
136700000000     C                   MOVEL     V1CFES        WORG
136800000000     C                   EXSR      CHKFIL
136900000000     C                   MOVEL     WORG          V1CFES
137000000000     C                   MOVEL     WDORG         V1DFES
137100000000IF  2C     $ERR          IFEQ      '1'
137200000000     C                   MOVEL     MSG(13)       $MSG
137300000000     C                   EXSR      CARMSG
137400000000     C                   SETON                                        75
137500000000     C                   LEAVE
137600000000E   2C                   ENDIF
137700000000     C* MESSAGGIO DI WARNING SE LA FILIALE CHE ESPONE E' DIVERSA DALLA PARTENZA
137800000000IF  2C     V1CFES        IFNE      V1CTFP
137900000000     C                   MOVEL     MSG(31)       $MSG
138000000000     C                   EXSR      CARMSG
138100000000E   2C                   ENDIF
138200000000     C*--------------------
138300000000     C* VALIDITA' TRAINO
138400000000     C*--------------------
138500000000     C***
138600000000     C* INSERIMENTO -> TRAINO INESISTENTE
138700000000     C***
138800000000IF  2C     $ERR          IFEQ      '0'
138900000000IF  3C     DEPOP1        IFEQ      'F10'                                        *INSERIMENTO
139000000000     C     $INS          ANDEQ     '0'                                          *SOLO LA PRIMA VOLTA
139100000000     C                   Z-ADD     V1CTRN        KLTTRN
139200000000     C                   Z-ADD     DDEV1C        KLTDDE
139300000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
139400000000IF  4C     *IN99         IFEQ      *OFF
139500000000     C                   MOVEL     '1'           $ERR
139600000000     C                   MOVEL     MSG(34)       $MSG
139700000000     C                   EXSR      CARMSG
139800000000     C                   SETON                                        6061
139900000000E   4C                   ENDIF
140000000000E   3C                   ENDIF
140100000000E   2C                   ENDIF
140200000000     C***
140300000000     C* NO PERIODI ACCAVALLATI
140400000000     C***
140500000000     C                   Z-ADD     V1CTRN        KLTTRN
140600000000     C     KLTTRN        SETLL     TNTLT01L                           99
140700000000     C     KLTTRN        READE(N)  TNTLT01L                               99
140800000000DO  2C     *IN99         DOWEQ     *OFF
140900000000     C*
141000000000     C* SE SONO IN MODIFI/ANNULL/RIPRIS ESCLUDO DAL CONTROLLO IL TRAINO IN LINEA
141100000000     C* OPPURE SE SONO ENTRATA IN INS, MA ESSENDO GIA' INSERITO E' COME UNA MOD
141200000000IF  3C     DDEV1C        IFNE      TLTDDE                                       *GIA' CONTROLLATO
141300000000     C*
141400960801IF  4C     TLTATB        IFEQ      *BLANKS
141500960801IF  5C     DDEV1C        IFGE      TLTDDE                                       *DECORRENZA
141600000000     C     DDEV1C        ANDLE     TLTDSC
141700000000     C     DSCV1C        ORGE      TLTDDE                                       *SCADENZA
141800000000     C     DSCV1C        ANDLE     TLTDSC
141900000000     C     TLTDDE        ORGE      DDEV1C                                       *DECORRENZA
142000000000     C     TLTDSC        ANDLE     DSCV1C
142100000000     C                   MOVEL     '1'           $ERR
142200000000     C                   MOVEL     MSG(12)       $MSG
142300000000     C                   EXSR      CARMSG
142400000000     C                   SETON                                        606162
142500000000     C                   LEAVE
142600960801E   5C                   ENDIF
142700960801E   4C                   ENDIF
142800000000E   3C                   ENDIF
142900000000     C     KLTTRN        READE(N)  TNTLT01L                               99
143000000000E   2C                   ENDDO
143100000000IF  2C     $ERR          IFEQ      '1'
143200000000     C                   LEAVE                                                  *PER USCIRE
143300000000E   2C                   ENDIF
143400000000     C***
143500000000     C* STESSO TIPO TRAINO
143600000000     C***
143700000000     C*---
143800000000     C* SE UN TRAINO HA PIU' DECORRENZE IL TIPO TRAINO PUO' ESSERE MODIFICATO
143900000000     C* SOLO SE UNO DI ESSI NON E' UN TIPO CHIUSO;  NON SI PUO' PASSARE CIOE'
144000000000     C* DA "C"-> "<>C"   E   DA "<>C"->"C"
144100000000     C                   Z-ADD     V1CTRN        KLTTRN
144200000000     C     KLTTRN        SETLL     TNTLT01L                           99
144300000000     C     KLTTRN        READE(N)  TNTLT01L                               99
144400000000DO  2C     *IN99         DOWEQ     *OFF
144500000000IF  3C     TLTATB        IFEQ      *BLANKS
144600000000IF  4C     V1CTTR        IFEQ      'C'
144700000000     C     TLTTTR        ANDNE     'C'
144800000000     C     V1CTTR        ORNE      'C'
144900000000     C     TLTTTR        ANDEQ     'C'
145000000000     C                   MOVEL     '1'           $ERR
145100000000     C                   MOVEL     MSG(25)       $MSG
145200000000     C                   EXSR      CARMSG
145300000000     C                   SETON                                        65
145400000000     C                   LEAVE
145500000000E   4C                   ENDIF
145600000000E   3C                   ENDIF
145700000000     C     KLTTRN        READE(N)  TNTLT01L                               99
145800000000E   2C                   ENDDO
145900000000     C*
146000000000E   1C                   ENDDO
146100000000     C*
146200000000     C* SE ERRORE NON EMETTE SFL PER POSIZIONARE IL CURSORE SUL CAMPO DEL SFLCTL
146300000000IF  1C     $ERR          IFEQ      '1'
146400000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
146500000000X   1C                   ELSE
146600000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
146700000000E   1C                   ENDIF
146800000000     C*
146900000000     C                   ENDSR
147000000000     C*------------------------------------------------------------------------*
147100000000     C* CHKDAT - CONTROLLA LA DATA
147200000000     C*          INPUT  -> WDA8 - DATA (GG/MM/AAAA)
147300000000     C*          OUTPUT -> DA8W - DATA (AAAA/MM/GG)
147400000000     C*------------------------------------------------------------------------*
147500000000     C     CHKDAT        BEGSR
147600000000     C*
147700000000     C     *LIKE         DEFINE    V1CDDE        WDA8
147800000000     C     *LIKE         DEFINE    V1CDDE        DA8W
147900000000     C*
148000000000     C                   Z-ADD     *ZEROS        DA8W
148100000000     C*
148200000000     C                   Z-ADD     WDA8          G08DAT
148300000000     C                   Z-ADD     *ZEROS        G08INV
148400000000     C                   MOVEL     '0'           G08ERR
148500000000     C                   CALL      'XSRDA8'
148600000000     C                   PARM                    WLBDA8
148700000000     C                   Z-ADD     G08DAT        WDA8
148800000000IF  1C     G08ERR        IFEQ      '1'
148900000000     C                   MOVEL     '1'           $ERR
149000000000     C                   ELSE
149100000000     C                   Z-ADD     G08INV        DA8W
149200000000E   1C                   ENDIF
149300000000     C*
149400000000     C                   ENDSR
149500000000     C*------------------------------------------------------------------------*
149600000000     C* CHKTFX - CONTROLLA LA FILIALE
149700000000     C*          INPUT  -> WTFX  - FILIALE
149800000518     C*                    WTTF  - TIPO TRAINO (P/A)
149900000000     C*          OUTPUT -> WDTFX - DESCRIZIONE FILIALE
150000000000     C*                    WFEL  - FILIALE ELABORATORE
150100000000     C*                    WTFA  - TERMINAL ARRIVO
150200000000     C*                    WTFP  - TERMINAL PARTENZA
150300000000     C*------------------------------------------------------------------------*
150400000000     C     CHKTFX        BEGSR
150500000000     C*
150600000000     C     *LIKE         DEFINE    V1CTFP        WTFX
150700000518     C     *LIKE         DEFINE    CAEEPA        WTTF
150800000000     C     *LIKE         DEFINE    V1DTFP        WDTFX
150900971128     C     *LIKE         DEFINE    ORGFIL        WFEL
151000971020     C     *LIKE         DEFINE    ORGFIL        WTFA
151100971020     C     *LIKE         DEFINE    ORGFIL        WTFP
151200000000     C*
151300000000     C                   MOVEL     *BLANKS       WDTFX
151400000000     C                   Z-ADD     *ZEROS        WFEL
151500000000     C                   Z-ADD     *ZEROS        WTFA
151600000000     C                   Z-ADD     *ZEROS        WTFP
151700000000     C* RICERCA FILIALI
151800000000     C     '?'           SCAN      WTFX                                   28
151900000000IF  1C     *IN28         IFEQ      *ON
152000000000     C                   MOVEL     *BLANKS       §OFIL
152100000000     C                   MOVEL     *BLANKS       §OTIP
152200000000     C                   MOVEL     *BLANKS       §ODES
152300971203     C                   MOVEL     *BLANKS       §OPRA
152400971203     C                   CALL      'TNSD24R'
152500000000     C                   PARM                    §OFIL             3
152600000000     C                   PARM                    §OTIP             1
152700000000     C                   PARM                    §ODES            25
152800971203     C                   PARM                    §OPRA             3
152900000000     C                   MOVEL     §OFIL         WTFX                           *CODICE
153000000000     C                   MOVEL     §ODES         WDTFX                          *DESCRIZIONE
153100000000     C                   MOVE      '1'           $TESVD                         *RIEMETTERE TESTATA
153200000000E   1C                   ENDIF
153300000000     C* NUMERICO
153400000000IF  1C     WTFX          IFLT      *ZEROS
153500000000     C                   MOVEL     '1'           $ERR
153600000000X   1C                   ELSE
153700000000     C* ESISTENTE
153800000000     C                   MOVEL     WTFX          KORFIL
153900000000     C     KEYORG        CHAIN     AZORG01L                           99
154000000000IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATA
154100000000     C     ORGFVA        ORNE      *BLANKS                                      *NO ANNULLATI
154200000000     C                   MOVEL     '1'           $ERR
154300000000X   2C                   ELSE
154400000518     C                   MOVEL     ORGDE3        OG143
154500000000     C                   MOVEL     ORGDE8        OG148
154600000000IF  3C     §OGLFV        IFEQ      'N'                                          *NON GESTITA NEI FV
154700000000     C                   MOVEL     '1'           $ERR
154800000000X   3C                   ELSE
154900000000     C                   MOVEL     ORGDES        WDTFX                          *DESCRIZIONE
155000971020     C* RECUPERA TERMINAL
155100971020     C                   CLEAR                   DSLV55
155200971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
155300971020     C                   MOVEL     *BLANKS       D55TPT                         *PARTENZA/ARRIVO
155400971020     C                   Z-ADD     ORGFIL        D55LIN                         *LINEA
155500980330IF  4C     V1CTRN        IFGT      9000000
155600980330     C                   Z-ADD     DSCV1C        D55DRF                         *DATA RIF.=DTA. SCAD
155700980330X   4C                   ELSE
155800980603     C                   Z-ADD     DDEV1C        D55DRF                         *DATA RIFERIMENTO
155900980330E   4C                   END
156000971020     C                   CALL      'FNLV55R'
156100971020     C                   PARM                    DSLV55
156200971128IF  4C     D55ERR        IFNE      *BLANKS
156300971128     C                   Z-ADD     ORGFIL        D55TFP
156400971128     C                   Z-ADD     ORGFIL        D55TFA
156500971128E   4C                   ENDIF
156600971020     C                   MOVEL     D55TFP        WTFP                           *TERMINAL PARTENZA
156700971020     C                   MOVEL     D55TFA        WTFA                           *TERMINAL ARRIVO
156800971128     C                   MOVEL     D55TFP        WFEL                           *FILIALE ELABORATORE
156900000518IF  4C     WTTF          IFEQ      'P'                                          *SOLO PARTENZA
157000000518     C     §OGPT         ANDEQ     'S'                                          *P.O. PARTENZA PT
157100000518     C                   Z-ADD     ORGFIL        WTFP                           *TERMINAL PARTENZA P
157200000518     C                   Z-ADD     ORGFIL        WFEL                           *FILIALE ELABORATORE
157300000518E   4C                   ENDIF
157400000518E   3C                   ENDIF
157500000000E   2C                   ENDIF
157600000000E   1C                   ENDIF
157700000000     C*
157800000000     C                   ENDSR
157900000000     C*------------------------------------------------------------------------*
158000000000     C* CHKTTR - CONTROLLA IL TIPO TRAINO
158100000000     C*          INPUT  -> WTTR  - TIPO TRAINO
158200000000     C*          OUTPUT -> WDTTR - DESCRIZIONE TIPO TRAINO
158300000000     C*                    WDEF  - TRAINO DEFLUENZA (S/ )
158400000000     C*------------------------------------------------------------------------*
158500000000     C     CHKTTR        BEGSR
158600000000     C*
158700000000     C     *LIKE         DEFINE    V1CTTR        WTTR
158800000000     C     *LIKE         DEFINE    V1DTTR        WDTTR
158900000000     C     *LIKE         DEFINE    §TVDEF        WDEF
159000000000     C*
159100000000     C                   MOVEL     *BLANKS       WDTTR
159200000000     C                   MOVEL     *BLANKS       WDEF
159300000000     C* RICERCA TABELLA
159400000000     C     '?'           SCAN      WTTR                                   28
159500000000IF  1C     *IN28         IFEQ      *ON
159600020715     c                   z-add     1             §tkut
159700000000     C                   MOVEL     'TV'          §TCOD
159800000000     C                   MOVEL     *BLANKS       §TKEY
159900000000     C                   MOVEL     *BLANKS       §TDES
160000000000     C                   CALL      'X§TABER'
160100000000     C                   PARM                    §TKUT             1 0
160200000000     C                   PARM                    §TCOD             2
160300000000     C                   PARM                    §TKEY             8
160400000000     C                   PARM                    §TDES            30
160500000000     C                   MOVEL     §TKEY         WTTR                           *CODICE
160600000000     C                   MOVEL     §TDES         WDTTR                          *DESCRIZIONE
160700000000     C                   MOVE      '1'           $TESVD                         *RIEMETTERE TESTATA
160800000000E   1C                   ENDIF
160900000000     C* ESISTENTE
161000000000IF  1C     WTTR          IFEQ      *BLANKS
161100000000     C                   MOVEL     '1'           $ERR
161200000000X   1C                   ELSE
161300000000     C                   SETOFF                                           28
161400000000     C                   Z-ADD     1             J
161500000000     C     WTTR          LOOKUP    TTR(J)                                 28
161600000000IF  2C     *IN28         IFEQ      *ON
161700000000     C                   MOVEL     DTTR(J)       WDTTR                          *DESCRIZ. TIPO TRAIN
161800000000     C                   MOVEL     DEF(J)        WDEF                           *TRAINO DEFLUENZA
161900000000X   2C                   ELSE
162000000000     C                   MOVEL     '1'           $ERR
162100000000E   2C                   ENDIF
162200000000E   1C                   ENDIF
162300000000     C*
162400000000     C                   ENDSR
162500000000     C*------------------------------------------------------------------------*
162600000000     C* CHKTMZ - CONTROLLA IL TIPO AUTOMEZZO
162700000000     C*          INPUT  -> WTMZ  - TIPO AUTOMEZZO
162800000000     C*          OUTPUT -> WDTMZ - DESCRIZIONE TIPO AUTOMEZZO
162900000000     C*------------------------------------------------------------------------*
163000000000     C     CHKTMZ        BEGSR
163100000000     C*
163200000000     C     *LIKE         DEFINE    V1CTMZ        WTMZ
163300000000     C     *LIKE         DEFINE    V1DTMZ        WDTMZ
163400000000     C*
163500000000     C                   MOVEL     *BLANKS       WDTMZ
163600000000     C* RICERCA TABELLA
163700000000     C     '?'           SCAN      WTMZ                                   28
163800000000IF  1C     *IN28         IFEQ      *ON
163900020715     c                   z-add     1             §tkut
164000000000     C                   MOVEL     'TU'          §TCOD
164100000000     C                   MOVEL     *BLANKS       §TKEY
164200000000     C                   MOVEL     *BLANKS       §TDES
164300000000     C                   CALL      'X§TABER'
164400000000     C                   PARM                    §TKUT             1 0
164500000000     C                   PARM                    §TCOD             2
164600000000     C                   PARM                    §TKEY             8
164700000000     C                   PARM                    §TDES            30
164800000000     C                   MOVEL     §TKEY         WTMZ                           *CODICE
164900000000     C                   MOVEL     §TDES         WDTMZ                          *DESCRIZIONE
165000000000     C                   MOVE      '1'           $TESVD                         *RIEMETTERE TESTATA
165100000000E   1C                   ENDIF
165200000000     C* ESISTENTE
165300000000IF  1C     WTMZ          IFEQ      *BLANKS
165400000000     C                   MOVEL     '1'           $ERR
165500000000X   1C                   ELSE
165600000000     C                   SETOFF                                           28
165700000000     C                   Z-ADD     1             J
165800000000     C     WTMZ          LOOKUP    TMZ(J)                                 28
165900000000IF  2C     *IN28         IFEQ      *ON
166000000000     C                   MOVEL     DTMZ(J)       WDTMZ                          *DESCRIZ. AUTOMEZZO
166100000000X   2C                   ELSE
166200000000     C                   MOVEL     '1'           $ERR
166300000000E   2C                   ENDIF
166400000000E   1C                   ENDIF
166500000000     C*
166600000000     C                   ENDSR
166700000000     C*------------------------------------------------------------------------*
166800000000     C* CHKFIL - CONTROLLA LA FILIALE CON RICERCA PER FERMATE
166900000000     C*------------------------------------------------------------------------*
167000000000     C     CHKFIL        BEGSR
167100000000     C*
167200000000     C     *LIKE         DEFINE    V1CFES        WORG
167300000000     C     *LIKE         DEFINE    V1DFES        WDORG
167400000000     C*
167500000000     C                   MOVEL     *BLANKS       WDORG                          *DESCRIZIONE
167600000000     C* RICHIESTO ELENCO
167700000000     C                   SETOFF                                           28
167800000000     C     '?'           SCAN      WORG                                   28
167900000000IF  1C     *IN28         IFEQ      *ON
168000000000     C                   Z-ADD     *ZEROS        D00FIL
168100000000     C                   MOVEL     DS00          KPJBU
168200000000     C                   CALL      'TNTL16R'
168300000000     C                   PARM                    KPJBA
168400000000     C                   MOVEL     KPJBU         DS00
168500000000     C                   MOVEL     D00FIL        WORG
168600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
168700000000X   1C                   ELSE
168800000000     C* SOLO NUMERICO
168900000000     C                   SETOFF                                       28
169000000000     C                   TESTN                   WORG                 28
169100000000     C                   MOVE      WORG          WALF1             1
169200000000IF  2C     *IN28         IFEQ      *OFF
169300000000     C     WALF1         ORLT      *ZEROS
169400000000     C                   MOVEL     '1'           $ERR
169500000000X   2C                   ELSE
169600000000     C* ESISTENZA
169700000000     C                   SETOFF                                           28
169800000000     C                   Z-ADD     1             J
169900000000     C                   MOVEL     WORG          WNUM3             3 0
170000000000     C     WNUM3         LOOKUP    ORG(J)                                 28
170100000000IF  3C     *IN28         IFEQ      *ON
170200000000     C     ORG(J)        ANDGT     *ZEROS
170300000000     C                   MOVEL     DORG(J)       WDORG                          *DESCRIZIONE
170400000000X   3C                   ELSE
170500000000     C                   MOVEL     '1'           $ERR
170600000000E   3C                   ENDIF
170700000000E   2C                   ENDIF
170800000000E   1C                   ENDIF
170900000000     C*
171000000000     C                   ENDSR
171100000000     C*------------------------------------------------------------------------*
171200000000     C* CTRECC - CONTROLLA LE ECCEZIONI
171300000000     C*------------------------------------------------------------------------*
171400000000     C     CTRECC        BEGSR
171500000000     C*
171600000000     C     *LOVAL        SETLL     AZCAE01L
171700000000     C                   READ      AZCAE01L                               99
171800000000DO  1C     *IN99         DOWEQ     *OFF
171900000000     C     WECC          ANDEQ     '0'
172000970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
172100970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
172200970717     C     CAETFP        ANDGT     *ZEROS
172300970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
172400970717     C     CAETFA        ANDGT     *ZEROS
172500970717IF  4C     CAEEPA        IFEQ      'A'                                          *ECCEZIONE ARRIVO
172600000000     C     CAETFA        ANDEQ     NUM3B                                        *FILIALE ARRIVO
172700000000     C     CAETFE        ANDEQ     NUM3                                         *LNP = TERM ARR ECC
172800000000     C     CAEDDE        ANDLE     DDEV1C                                       *IN DECORRENZA
172900000000     C     CAEDSC        ANDGE     DSCV1C
173000000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
173100970717E   4C                   ENDIF
173200970717E   3C                   ENDIF
173300960821E   2C                   ENDIF
173400000000     C                   READ      AZCAE01L                               99
173500000000E   1C                   ENDDO
173600000000     C*
173700000000     C                   ENDSR
173800000000     C*------------------------------------------------------------------------*
173900000000     C* CTREC1 - CONTROLLA LE ECCEZIONI (1)
174000000000     C*------------------------------------------------------------------------*
174100000000     C     CTREC1        BEGSR
174200000000     C*
174300000000     C                   MOVEL     'A'           KAEEPA                         *ARRIVO
174400000000     C                   MOVEL     V1CTFP        KAETFP                         *PARTENZA
174500000000     C                   MOVEL     V1CTFA        KAETFA                         *ARRIVO
174600000000     C     KEYCAE        SETLL     AZCAE01L
174700000000     C     KEYCAE        READE     AZCAE01L                               99
174800000000DO  1C     *IN99         DOWEQ     *OFF
174900000000     C     WECC          ANDEQ     '0'
175000970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
175100970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
175200970717     C     CAETFP        ANDGT     *ZEROS
175300970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
175400970717     C     CAETFA        ANDGT     *ZEROS
175500970717IF  4C     CAETFE        IFEQ      NUM3B                                        *LNA = TERM ARR ECC
175600000000     C     CAEDDE        ANDLE     DDEV1C                                       *IN DECORRENZA
175700000000     C     CAEDSC        ANDGE     DSCV1C
175800000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
175900970717E   4C                   ENDIF
176000970717E   3C                   ENDIF
176100960821E   2C                   ENDIF
176200000000     C     KEYCAE        READE     AZCAE01L                               99
176300000000E   1C                   ENDDO
176400000000     C*
176500000000     C                   ENDSR
176600000000     C*------------------------------------------------------------------------*
176700000000     C* CHKINL - CONTROLLA A QUALE TIPO TRAINO SI PUO' RICONDURRE L'INOLTRO
176800000000     C*          INPUT  -> WFIP  - FILIALE PARTENZA
176900000000     C*                    WFIA  - FILIALE ARRIVO
177000000000     C*          OUTPUT -> WINL  - TIPO TRAINO DELL'INOLTRO
177100000000     C*------------------------------------------------------------------------*
177200000000     C     CHKINL        BEGSR
177300000000     C*
177400000000     C     *LIKE         DEFINE    ORGFIL        WFIP
177500000000     C     *LIKE         DEFINE    ORGFIL        WFIA
177600000000     C     *LIKE         DEFINE    TLTTTR        WINL
177700000000     C*--------------------
177800000000     C* DEFLUENZA
177900000000     C*--------------------
178000000000     C                   MOVEL     '0'           WECC              1
178100000000     C                   EXSR      CTRECC
178200971128IF  1C     WFIP          IFEQ      WPFEL                                        *FIL PAR=FIL ELABORA
178300000000IF  2C     WFIP          IFEQ      WATFA                                        *FIL PAR=TERM ARR
178400000000     C     WECC          OREQ      '1'                                          *FIL PAR=TERM ARR EC
178500000000IF  3C     WFIP          IFNE      WFIA                                         *FIL PAR<>FIL ARR
178600000000     C                   MOVEL     'D'           WINL
178700000000E   3C                   ENDIF
178800000000E   2C                   ENDIF
178900000000E   1C                   ENDIF
179000000000     C*--------------------
179100000000     C* LOCALE
179200000000     C*--------------------
179300000000     C                   MOVEL     '0'           WECC              1
179400000000     C                   EXSR      CTRECC
179500971128IF  1C     WFIP          IFEQ      WPFEL                                        *FIL PAR=FIL ELABORA
179600000000IF  2C     WFIP          IFEQ      WATFA                                        *FIL PAR=TERM ARR
179700000000     C     WECC          OREQ      '1'                                          *FIL PAR=TERM ARR EC
179800000000IF  3C     WFIP          IFEQ      WFIA                                         *FIL PAR=FIL ARR
179900000000     C                   MOVEL     'L'           WINL
180000000000E   3C                   ENDIF
180100000000E   2C                   ENDIF
180200000000E   1C                   ENDIF
180300000000     C*--------------------
180400000000     C* SOLO ANDATA
180500000000     C*--------------------
180600000000     C                   MOVEL     '0'           WECC              1
180700000000     C                   EXSR      CTREC1
180800971128IF  1C     WFIP          IFEQ      WPFEL                                        *FIL PAR=FIL ELABORA
180900000000IF  2C     WFIA          IFEQ      WATFA                                        *FIL ARR=TERM ARR
181000000000     C     WECC          OREQ      '1'                                          *FIL ARR=TERM ARR EC
181100000000IF  3C     WFIP          IFNE      WFIA                                         *FIL PAR<>FIL ARR
181200000000     C                   MOVEL     'S'           WINL
181300000000E   3C                   ENDIF
181400000000E   2C                   ENDIF
181500000000E   1C                   ENDIF
181600000000     C*
181700000000     C* SE TIPO TRAINO INOLTRO NON E' RICONDUCIBILE A NESSUN ALTRO TIPO, ERRORE
181800000000IF  1C     WINL          IFEQ      *BLANKS
181900000000     C                   MOVEL     '1'           $ERR
182000000000E   1C                   ENDIF
182100000000     C*
182200000000     C                   ENDSR
182300000000     C*------------------------------------------------------------------------*
182400000000     C* RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
182500000000     C*------------------------------------------------------------------------*
182600000000     C     RECVID        BEGSR
182700000000     C*
182800000000     C                   Z-ADD     TLTTFP        V1HTFP                         *FIL PARTENZA HIDDEN
182900000000     C                   Z-ADD     TLTDTR        V1HDTR                         *DATA TRASM. HIDDEN
183000000000     C*
183100000000     C                   Z-ADD     D00TRN        V1CTRN                         *TRAINO
183200000000     C                   Z-ADD     D00DDE        G08INV                         *DATA DECORRENZA
183300000000     C                   Z-ADD     *ZEROS        G08DAT
183400000000     C                   MOVEL     '3'           G08ERR
183500000000     C                   CALL      'XSRDA8'
183600000000     C                   PARM                    WLBDA8
183700000000     C                   Z-ADD     G08DAT        V1CDDE
183800000000     C                   Z-ADD     TLTDSC        G08INV                         *DATA SCADENZA
183900000000     C                   Z-ADD     *ZEROS        G08DAT
184000000000     C                   MOVEL     '3'           G08ERR
184100000000     C                   CALL      'XSRDA8'
184200000000     C                   PARM                    WLBDA8
184300000000     C                   Z-ADD     G08DAT        V1CDSC
184400000000     C                   MOVEL     TLTTFP        V1CTFP                         *FILIALE PARTENZA
184500000000     C                   MOVEL     TLTTFA        V1CTFA                         *FILIALE ARRIVO
184600000000     C                   MOVEL     TLTTTR        V1CTTR                         *TIPO TRAINO
184700000000     C                   MOVEL     TLTGSE        V1CGSE                         *GIORNI SETTIMANA
184800000000     C                   MOVEL     TLTTMZ        V1CTMZ                         *TIPO AUTOMEZZO
184900000000     C                   MOVEL     TLTBIS        V1CBIS                         *NORMALE/BIS
185000000000     C                   MOVEL     TLTFES        V1CFES                         *FILIALE IMPORTO
185100000000     C                   Z-ADD     TLTDIR        G08INV                         *DATA INSERIMENTO
185200000000     C                   Z-ADD     *ZEROS        G08DAT
185300000000     C                   MOVEL     '3'           G08ERR
185400000000     C                   CALL      'XSRDA8'
185500000000     C                   PARM                    WLBDA8
185600000000     C                   Z-ADD     G08DAT        V1CDIR
185700000000E   1C     TLTDUV        IFGT      *ZEROS
185800000000     C                   Z-ADD     TLTDUV        G08INV                         *DATA VARIAZIONE
185900000000     C                   Z-ADD     *ZEROS        G08DAT
186000000000     C                   MOVEL     '3'           G08ERR
186100000000     C                   CALL      'XSRDA8'
186200000000     C                   PARM                    WLBDA8
186300000000     C                   Z-ADD     G08DAT        V1CDUV
186400000000E   1C                   ENDIF
186500000000     C* DECODIFICHE
186600000000     C                   SETOFF                                           28
186700000000     C                   Z-ADD     1             J
186800000000     C                   MOVEL     V1CTFP        WNUM3             3 0
186900000000     C     WNUM3         LOOKUP    ORG(J)                                 28
187000000000IF  1C     *IN28         IFEQ      *ON
187100000000     C                   MOVEL     DORG(J)       V1DTFP                         *DESCRIZIONE PARTENZ
187200000000E   1C                   ENDIF
187300000000     C                   SETOFF                                           28
187400000000     C                   Z-ADD     1             J
187500000000     C                   MOVEL     V1CTFA        WNUM3
187600000000     C     WNUM3         LOOKUP    ORG(J)                                 28
187700000000IF  1C     *IN28         IFEQ      *ON
187800000000     C                   MOVEL     DORG(J)       V1DTFA                         *DESCRIZIONE ARRIVO
187900000000E   1C                   ENDIF
188000000000     C                   SETOFF                                           28
188100000000     C                   Z-ADD     1             J
188200000000     C     V1CTTR        LOOKUP    TTR(J)                                 28
188300000000IF  1C     *IN28         IFEQ      *ON
188400000000     C                   MOVEL     DTTR(J)       V1DTTR                         *DESCRIZ. TIPO TRAIN
188500000000E   1C                   ENDIF
188600000000     C                   SETOFF                                           28
188700000000     C                   Z-ADD     1             J
188800000000     C     V1CTMZ        LOOKUP    TMZ(J)                                 28
188900000000IF  1C     *IN28         IFEQ      *ON
189000000000     C                   MOVEL     DTMZ(J)       V1DTMZ                         *DESCRIZ. AUTOMEZZO
189100000000E   1C                   ENDIF
189200000000     C                   SETOFF                                           28
189300000000     C                   Z-ADD     1             J
189400000000     C                   MOVEL     V1CFES        WNUM3
189500000000     C     WNUM3         LOOKUP    ORG(J)                                 28
189600000000IF  1C     *IN28         IFEQ      *ON
189700000000     C                   MOVEL     DORG(J)       V1DFES                         *DESCRIZ.FIL.IMPORTO
189800000000E   1C                   ENDIF
189900000000     C*
190000000000     C* NON VISUALIZZA LA DATA SCADENZA (SE UGUALE A ZERO)
190100000000IF  1C     TLTDSC        IFEQ      *ZEROS
190200000000     C                   SETON                                        01
190300000000E   1C                   ENDIF
190400000000     C*
190500000000     C                   ENDSR
190600000000     C*------------------------------------------------------------------------*
190700000000     C* AZZVID - PULISCE TUTTI I CAMPI DEL VIDEO
190800000000     C*------------------------------------------------------------------------*
190900000000     C     AZZVID        BEGSR
191000000000     C*
191100000000     C                   Z-ADD     *ZEROS        V1HTFP                         *FIL PARTENZA HIDDEN
191200000000     C                   Z-ADD     *ZEROS        V1HDTR                         *DATA TRASM. HIDDEN
191300000000     C*
191400000000     C                   Z-ADD     *ZEROS        V1CTRN                         *TRAINO
191500000000     C                   Z-ADD     *ZEROS        V1CDDE                         *DATA DECORRENZA
191600000000     C                   Z-ADD     *ZEROS        V1CDSC                         *DATA SCADENZA
191700000000     C                   MOVEL     *BLANKS       V1CTFP                         *FILIALE PARTENZA
191800000000     C                   MOVEL     *BLANKS       V1CTFA                         *FILIALE ARRIVO
191900000000     C                   MOVEL     *BLANKS       V1CTTR                         *TIPO TRAINO
192000000000     C                   MOVEL     *BLANKS       V1CGSE                         *GIORNI SETTIMANA
192100000000     C                   MOVEL     *BLANKS       V1CTMZ                         *TIPO AUTOMEZZO
192200000000     C                   MOVEL     *BLANKS       V1CBIS                         *NORMALE/BIS
192300000000     C                   MOVEL     *BLANKS       V1CFES                         *FILIALE IMPORTO
192400000000     C                   Z-ADD     *ZEROS        V1CDIR                         *DATA INSERIMENTO
192500000000     C                   Z-ADD     *ZEROS        V1CDUV                         *DATA VARIAZIONE
192600000000     C* DECODIFICHE
192700000000     C                   MOVEL     *BLANKS       V1DTFP                         *DESCRIZIONE PARTENZ
192800000000     C                   MOVEL     *BLANKS       V1DTFA                         *DESCRIZIONE ARRIVO
192900000000     C                   MOVEL     *BLANKS       V1DTTR                         *DESCRIZ. TIPO TRAIN
193000000000     C                   MOVEL     *BLANKS       V1DTMZ                         *DESCRIZ. AUTOMEZZO
193100000000     C                   MOVEL     *BLANKS       V1DFES                         *DESCRIZIONE FIL.IMP
193200000000     C*
193300000000     C* NON VISUALIZZA LA DATA SCADENZA (IN INSERIMENTO E' UGUALE A ZERO)
193400000000     C                   SETON                                        01
193500000000     C*
193600000000     C                   ENDSR
193700000000     C*------------------------------------------------------------------------*
193800000000     C* CARTAB - CARICA LE TABELLE
193900000000     C*------------------------------------------------------------------------*
194000000000     C     CARTAB        BEGSR
194100000000     C*--------------------
194200000000     C* FILIALI
194300000000     C*--------------------
194400000518     C                   CLEAR                   OG148
194500000518     C                   CLEAR                   OG143
194600000000     C                   Z-ADD     *ZEROS        J                 4 0
194700000000     C     *LOVAL        SETLL     AZORG01L
194800000000     C                   READ      AZORG01L                               99
194900000000DO  1C     *IN99         DOWEQ     *OFF
195000000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
195100000000     C                   MOVEL     ORGDE8        OG148
195200000000IF  3C     §OGLFV        IFNE      'N'                                          *NON GESTIONE IN FV
195300000000     C                   ADD       1             J
195400000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
195500000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
195600000518     C                   MOVEL     ORGDE3        OG143
195700000518     C                   MOVEL     §OGPT         PORG(J)                        *P.O. POSTE
195800000000E   3C                   ENDIF
195900000000E   2C                   ENDIF
196000000000     C                   READ      AZORG01L                               99
196100000000E   1C                   ENDDO
196200000000     C*--------------------
196300000000     C* TIPI TRAINI
196400000000     C*--------------------
196500000000     C                   Z-ADD     *ZEROS        J
196600020715     c                   z-add     1             ktbkut
196700000000     C                   MOVEL     'TV'          KTBCOD
196800000000     C     KE2TAB        SETLL     TABEL00F
196900000000     C     KE2TAB        READE     TABEL00F                               99
197000000000DO  1C     *IN99         DOWEQ     *OFF
197100000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
197200000000     C                   ADD       1             J
197300000000     C                   MOVEL     TBLKEY        TTR(J)                         *CODICE
197400000000     C                   MOVEL     TBLUNI        DSTV
197500000000     C                   MOVEL     §TVDES        DTTR(J)                        *DESCRIZIONE
197600000000     C                   MOVEL     §TVDEF        DEF(J)                         *DEFLUENZA
197700000000E   2C                   ENDIF
197800000000     C     KE2TAB        READE     TABEL00F                               99
197900000000E   1C                   ENDDO
198000000000     C*--------------------
198100000000     C* TIPI AUTOMEZZI
198200000000     C*--------------------
198300000000     C                   Z-ADD     *ZEROS        J
198400020715     c                   z-add     1             ktbkut
198500000000     C                   MOVEL     'TU'          KTBCOD
198600000000     C     KE2TAB        SETLL     TABEL00F
198700000000     C     KE2TAB        READE     TABEL00F                               99
198800000000DO  1C     *IN99         DOWEQ     *OFF
198900000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
199000000000     C                   ADD       1             J
199100000000     C                   MOVEL     TBLKEY        TMZ(J)                         *CODICE
199200000000     C                   MOVEL     TBLUNI        DSTU
199300000000     C                   MOVEL     §TUDES        DTMZ(J)                        *DESCRIZIONE
199400000000E   2C                   ENDIF
199500000000     C     KE2TAB        READE     TABEL00F                               99
199600000000E   1C                   ENDDO
199700000000     C*
199800000000     C                   ENDSR
199900000000     C*------------------------------------------------------------------------*
200000000000     C* IMPVID - IMPOSTAIZONI VIDEO
200100000000     C*------------------------------------------------------------------------*
200200000000     C     IMPVID        BEGSR
200300000000     C*
200400000000     C* 1°) DECODIFICA L'OPZIONE RICHIAMATA
200500000000     C* 2°) IMPOSTA LA PROTEZIONE DEI CAMPI -> 90=CAMPI CHIAVE / 91=ALTRI CAMPI
200600000000SL  1C                   SELECT
200700000000     C     DEPOP1        WHENEQ    'F10'
200800000000     C                   MOVEL     DOPZ(1)       VTDFUN                         *INSERIMENTO
200900000000     C                   SETOFF                                       9091      *NESSUNA PROTEZIONE
201000000000     C     DEPOP1        WHENEQ    'O02'
201100000000     C                   MOVEL     DOPZ(2)       VTDFUN                         *VARIAZIONE
201200000000     C                   SETON                                        90        *CHIAVE PROTETTA
201300000000     C     DEPOP1        WHENEQ    'O04'
201400000000     C                   MOVEL     DOPZ(3)       VTDFUN                         *CANCELLAZIONE
201500000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
201600000000     C     DEPOP1        WHENEQ    'O07'
201700000000     C                   MOVEL     DOPZ(4)       VTDFUN                         *RIPRISTINO
201800000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
201900000000     C     DEPOP1        WHENEQ    'O05'
202000000000     C                   MOVEL     DOPZ(5)       VTDFUN                         *VISUALIZZAZIONE
202100000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
202200000000     C                   MOVEL     FUN(6)        FUNRIG                         *TOGLIE FUN F6=AGGIO
202300000000     C                   MOVEL     'N'           FUNASG
202400000000     C                   MOVEL     'N'           FUNASI
202500000000     C                   MOVEL     'N'           FUNAFG
202600000000     C                   MOVEL     'N'           FUNAFG
202700000000     C                   MOVEL     FUNRIG        FUN(6)
202800000000     C                   OTHER
202900000000E   1C                   ENDSL
203000000000     C*
203100000000     C* CONTROLLA ESISTENZA E DISPONIBILITA' RECORD (NO SE INSERIMENTO)
203200000000SL  1C                   SELECT
203300000000     C     DEPOP1        WHENEQ    'F10'
203400000000     C                   OTHER
203500000000     C                   Z-ADD     D00TRN        KLTTRN                         *TRAINO
203600000000     C                   Z-ADD     D00DDE        KLTDDE                         *DATA DECORRENZA
203700000000     C                   Z-ADD     D00DDE        DDEV1C                         DATA DECORRENZA VIDE
203800000000     C* INTERROGAZIONE
203900000000IF  2C     DEPOP1        IFEQ      'O05'
204000000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
204100000000X   2C                   ELSE
204200000000     C* MODIFICA
204300000000     C     KEYTLT        CHAIN     TNTLT01L                           9998
204400000000IF  3C     *IN98         IFEQ      *ON                                          *ALLOCATO
204500000000     C                   MOVEL     '1'           $ERR
204600000000     C                   MOVEL     '1'           $FINE
204700000000     C                   MOVEL     MSG(36)       D00MSG
204800000000E   3C                   ENDIF
204900000000E   2C                   ENDIF
205000000000IF  2C     *IN99         IFEQ      *ON                                          *INSESISTENTE
205100000000     C                   MOVEL     '1'           $ERR
205200000000     C                   MOVEL     '1'           $FINE
205300000000     C                   MOVEL     MSG(35)       D00MSG
205400000000E   2C                   ENDIF
205500000000E   1C                   ENDSL
205600000000     C* RIEMPE IL VIDEO
205700000000IF  1C     $ERR          IFEQ      '0'
205800000000SL  2C                   SELECT
205900000000     C     DEPOP1        WHENEQ    'F10'
206000000000     C                   EXSR      AZZVID                                       *AZZERA CAMPI VIDEO
206100000000     C                   OTHER
206200000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
206300000000E   2C                   ENDSL
206400000000E   1C                   ENDIF
206500000000     C*
206600000000     C                   ENDSR
206700971020     C*------------------------------------------------------------------------*
206800971020     C* FINSR - OPERAZIONI FINALI
206900971020     C*------------------------------------------------------------------------*
207000971020     C     FINSR         BEGSR
207100971020     C* FNLV55R
207200971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
207300971020     C                   MOVEL     'C'           D55TLA
207400971020     C                   CALL      'FNLV55R'
207500971020     C                   PARM                    DSLV55
207600971020     C*
207700971020     C                   ENDSR
207800000000     C*------------------------------------------------------------------------*
207900000000     C* *INZSR - IMPOSTAZIONI INIZIALI
208000000000     C*------------------------------------------------------------------------*
208100000000     C     *INZSR        BEGSR
208200000000     C*--------------------
208300000000     C* RICEVIMENTO PARAMETRI
208400000000     C*--------------------
208500000000     C     *ENTRY        PLIST
208600000000     C                   PARM                    KPJBA
208700000000     C                   MOVEL     KPJBU         DS00
208800000000     C*--------------------
208900000000     C* VARIABILI RIFERITE AL DATA BASE
209000000000     C*--------------------
209100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
209200000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
209300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
209400000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
209500000000     C     *LIKE         DEFINE    CAEEPA        KAEEPA                         *AZCAE01L
209600000000     C     *LIKE         DEFINE    CAETFP        KAETFP
209700000000     C     *LIKE         DEFINE    CAETFA        KAETFA
209800000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
209900000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
210000000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
210100000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
210200000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
210300000000     C     *LIKE         DEFINE    TLODDE        KLODDE
210400000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
210500000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
210600010723     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
210700000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
210800010723     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
210900000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
211000000000     C*--------------------
211100000000     C* VARIABILI NON RIFERITE AL DATA BASE
211200000000     C*--------------------
211300000000     C     *LIKE         DEFINE    D00OP0        WOP0                           *DS00
211400000000     C     *LIKE         DEFINE    D00OP1        WOP1
211500000000     C     *LIKE         DEFINE    D00F03        WF03
211600000000     C     *LIKE         DEFINE    D00F12        WF12
211700000000     C     *LIKE         DEFINE    D00ERR        WERR
211800000000     C     *LIKE         DEFINE    D00MSG        WMSG
211900000000     C     *LIKE         DEFINE    D00TRN        WTRN
212000000000     C     *LIKE         DEFINE    D00DDE        WDDE
212100000000     C     *LIKE         DEFINE    V1CDDE        DDEV1C                         *VIDEO
212200000000     C     *LIKE         DEFINE    V1CDSC        DSCV1C
212300000000     C*--------------------
212400000000     C* CHIAVI DI LETTURA
212500000000     C*--------------------
212600000000     C* LETTURA AZORG01L - COMPLETA
212700000000     C     KEYORG        KLIST
212800000000     C                   KFLD                    KORFIL                         *FILIALE
212900000000     C* LETTURA TABEL00F - COMPLETA
213000000000     C     KEYTAB        KLIST
213100000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
213200000000     C                   KFLD                    KTBCOD                         *TABELLA
213300000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
213400000000     C* LETTURA TABEL00F - PARZIALE
213500000000     C     KE2TAB        KLIST
213600000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
213700000000     C                   KFLD                    KTBCOD                         *TABELLA
213800000000     C* LETTURA AZCAE01L - PARZIALE
213900000000     C     KEYCAE        KLIST
214000000000     C                   KFLD                    KAEEPA                         *PARTENZA/ARRIVO
214100000000     C                   KFLD                    KAETFP                         *PARTENZA
214200000000     C                   KFLD                    KAETFA                         *ARRIVO
214300000000     C* LETTURA TNTLT01L - COMPLETA
214400000000     C     KEYTLT        KLIST
214500000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
214600000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
214700000000     C* LETTURA TNTLR01L - PARZIALE
214800000000     C     KEYTLR        KLIST
214900000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
215000000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
215100000000     C* LETTURA TNTLO01L - PARZIALE
215200000000     C     KEYTLO        KLIST
215300000000     C                   KFLD                    KLOTRN                         *NUMERO TRAINO
215400000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
215500000000     C* LETTURA TNTLM01L - PARZIALE
215600000000     C     KEYTLM        KLIST
215700000000     C                   KFLD                    KLMTRN                         *NUMERO TRAINO
215800000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
215900010723     C* LETTURA TITLL01L - PARZIALE
216000000000     C     KEYTLL        KLIST
216100000000     C                   KFLD                    KLLTRN                         *NUMERO TRAINO
216200000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
216300000000     C* LETTURA TNTLN01L - PARZIALE
216400000000     C     KEYTLN        KLIST
216500000000     C                   KFLD                    KLNTRN                         *NUMERO TRAINO
216600000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
216700000000     C*--------------------
216800000000     C* VARIABILI DI CONTROLLO
216900000000     C*--------------------
217000000000     C                   MOVEL     '1'           MSGF12            1            *MESSAGGIO PER F12
217100000000     C                   MOVEL     '0'           $INS              1            *RECORD INSERITO
217200000000     C                   MOVEL     '0'           $ANN              1            *RECORD ANNULLATO
217300000000     C                   MOVEL     '0'           $RIP              1            *RECORD RIPRISTINATO
217400000000     C*
217500000000     C* DEPOSITA I LIVELLI DI ENTRATA
217600000000     C     *LIKE         DEFINE    D00OP0        DEPOP0
217700000000     C     *LIKE         DEFINE    D00OP1        DEPOP1
217800000000     C                   MOVEL     D00OP0        DEPOP0
217900000000     C                   MOVEL     D00OP1        DEPOP1
218000971020     C*
218100971020     C* CALCOLA LA DATA CORRENTE
218200971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
218300971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
218400971020     C                   Z-ADD     N8            G08DAT
218500971020     C                   Z-ADD     *ZEROS        G08INV
218600971020     C                   MOVEL     '0'           G08ERR
218700971020     C                   CALL      'XSRDA8'
218800971020     C                   PARM                    WLBDA8
218900971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
219000000000     C*
219100000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
219200000000     C                   MOVE      DEPOP0        INTGES            2
219300000000IF  1C     INTGES        IFEQ      '02'
219400000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
219500000000X   1C                   ELSE
219600000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
219700000000E   1C                   ENDIF
219800000000     C*
219900000000     C* CXX -> SE IL PGM E' RICHIAMATO CON CXX NON PERMETTE DI INSERIRE IL TIPO
220000000000     C*        TRAINO "CHIUSO" PERCHE' QUESTI NON LO GESTISCONO
220100000000     C                   MOVEL     DEPOP0        WCALL             1
220200000000     C*
220300000000     C* TUTTI I FILES SONO STATI APERTI IN I/O PERCHE' QUESTO PROGRAMMA ESEGUE
220400000000     C* LE VARIAZIONI (COMIT); SONO NECESSARIE QUINDI DELLE VARIAZIONI FITTIZIE
220500000000     C* PER QUEI FILES CHE QUESTO PGM NON AGGIORNA
220600000000IF  1C     *IN99         IFEQ      *ON
220700000000     C     *IN99         ANDEQ     *OFF
220800000000     C* FERMATE
220900000000     C                   WRITE     TNTLR000
221000000000     C* ORARI
221100000000     C                   WRITE     TNTLO000
221200000000     C* MCUBI ASSEGNATI
221300000000     C                   WRITE     TNTLM000
221400000000     C* LISTINI
221500010723     C                   WRITE     TITLL000
221600000000     C* NOTE
221700000000     C                   WRITE     TNTLN000
221800110506     C* NOTE
221900110506     C                   WRITE     TNTLS000
222000000000E   1C                   ENDIF
222100000000     C*
222200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
222300000000     C                   EXSR      INZSTD
222400000000     C*
222500000000     C* CARICA LE TABELLE
222600000000     C                   EXSR      CARTAB
222700000000     C*
222800000000     C* IMPOSTAZIONI VIDEO
222900000000     C                   EXSR      IMPVID
223000000000     C* EMETTE VIDEATA
223100000000     C                   EXSR      CARV1
223200000000     C*
223300000000     C                   ENDSR
223400000000     C*------------------------------------------------------------------------*
223500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
223600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
223700000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
223800000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
223900000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
224000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
224100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
224200000000     C*------------------------------------------------------------------------*
224300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
224400000000     C*------------------------------------------------------------------------*
224500000000     C     INZSTD        BEGSR
224600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
224700000000     C                   CALL      'XKEYS'
224800000000     C                   PARM                    DSKEYS
224900000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
225000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
225100000000     C     XMSGPA        PLIST
225200000000     C                   PARM                    DSMSMS
225300000000     C                   PARM                    DSMSPG
225400000000     C                   PARM                    DSMSKE
225500000000     C                   PARM                    DSMSER
225600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
225700000000     C                   MOVEL     'TNTL02R'     DSMSPG                         *NOME PGM CHIAMANTE
225800000000     C                   MOVEL     'TNTL02R'     VMCQUE                         *CODA MESSAGGI DESTI
225900000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
226000000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
226100000000     C* INIZIALIZZO FLAG CONTROLLO PGM
226200000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
226300000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
226400000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
226500000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
226600000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
226700000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
226800000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
226900000000     C* LANCIO ROUTINE PER VERIFICARE SE
227000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
227100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
227200020715
227300020715     c     *dtaara       define    §azute        azuteds
227400020715     c     *dtaara       define    §datiute      ddatiute
227500020715
227600020715     c                   in(E)     *dtaara
227700020715     c                   if        %Error or rsut = *blanks
227800020715     c                   clear                   tibs34ds
227900020715     c                   call      'TIBS34R'
228000020715     c                   parm                    Tibs34Ds
228100020715     c                   in        *dtaara
228200020715     c                   endif
228300000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
228400000000     C                   BITOFF    '01234567'    POSMI1            1
228500000000     C                   BITOFF    '01234567'    POSMI2            1
228600000000     C                   BITOFF    '01234567'    POSMA1            1
228700000000     C                   BITOFF    '01234567'    POSMA2            1
228800000000     C                   BITON     '34'          POSMI1            1            *18
228900000000     C                   BITON     '7'           POSMI2            1            *01
229000000000     C                   BITON     '34'          POSMA1            1            *18
229100000000     C                   BITON     '0'           POSMA2            1            *80
229200000000     C                   MOVE      POSMI2        POSMIN            2
229300000000     C                   MOVEL     POSMI1        POSMIN
229400000000     C                   MOVE      POSMA2        POSMAX            2
229500000000     C                   MOVEL     POSMA1        POSMAX
229600000000     C                   ENDSR
229700000000     C*------------------------------------------------------------------------*
229800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
229900000000     C*------------------------------------------------------------------------*
230000000000     C     FUNSTD        BEGSR
230100000000     C                   SELECT
230200000000     C     KEY           WHENEQ    F3
230300000000     C                   EXSR      F03STD
230400000000     C     KEY           WHENEQ    F12
230500000000     C                   EXSR      F12STD
230600000000     C     KEY           WHENEQ    F24
230700000000     C                   EXSR      F24STD
230800000000     C                   OTHER
230900000000     C                   MOVEL     MSG(2)        $MSG
231000000000     C                   EXSR      CARMSG
231100000000     C                   ENDSL
231200000000     C                   ENDSR
231300000000     C*------------------------------------------------------------------------*
231400000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
231500000000     C*------------------------------------------------------------------------*
231600000000     C     F03STD        BEGSR
231700000000     C*
231800000000     C* SOLO LA PRIMA VOLTA MANDA UN MESSAGGIO DI CONTROLLO PER ESSERE SICURI DI
231900000000     C* VOLER TORNARE INDIETRO E NON REGISTRARE LE VARIAZIONI ESEGUITE FIN LI'
232000000000IF  1C     $TGES         IFEQ      'G'                                          *GESTIONE
232100000000     C     MSGF12        ANDEQ     '0'                                          *EMETTERE MESSAGGIO
232200000000     C                   MOVEL     MSG(30)       $MSG
232300000000     C                   EXSR      CARMSG
232400000000     C                   MOVEL     '1'           MSGF12                         *MESSAGGIO EMESSO
232500000000X   1C                   ELSE
232600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
232700000000     C                   MOVEL     '1'           $F03
232800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
232900000000     C                   MOVEL     '1'           $FINE
233000000000     C* PULISCE IL MESSAGGIO
233100000000     C                   MOVEL     *BLANKS       D00MSG                         PULISCE MESSAGGIO DS
233200000000E   1C                   ENDIF
233300000000     C*
233400000000     C                   ENDSR
233500000000     C*------------------------------------------------------------------------*
233600000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
233700000000     C*------------------------------------------------------------------------*
233800000000     C     F12STD        BEGSR
233900000000     C*
234000000000     C* SOLO LA PRIMA VOLTA MANDA UN MESSAGGIO DI CONTROLLO PER ESSERE SICURI DI
234100000000     C* VOLER TORNARE INDIETRO E NON REGISTRARE LE VARIAZIONI ESEGUITE FIN LI'
234200000000IF  1C     $TGES         IFEQ      'G'                                          *GESTIONE
234300000000     C     MSGF12        ANDEQ     '0'                                          *EMETTERE MESSAGGIO
234400000000     C                   MOVEL     MSG(30)       $MSG
234500000000     C                   EXSR      CARMSG
234600000000     C                   MOVEL     '1'           MSGF12                         *MESSAGGIO EMESSO
234700000000X   1C                   ELSE
234800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
234900000000     C                   MOVEL     '1'           $F12
235000000000     C* IMPOSTO FLAG DI FIN PGM
235100000000     C                   MOVEL     '1'           $FINE
235200000000     C* PULISCE IL MESSAGGIO
235300000000     C                   MOVEL     *BLANKS       D00MSG                         PULISCE MESSAGGIO DS
235400000000E   1C                   ENDIF
235500000000     C*
235600000000     C                   ENDSR
235700000000     C*------------------------------------------------------------------------*
235800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
235900000000     C*------------------------------------------------------------------------*
236000000000     C     F24STD        BEGSR
236100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
236200000000     C                   EXSR      OUTFUN
236300000000     C                   ENDSR
236400000000     C*------------------------------------------------------------------------*
236500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
236600000000     C*          CODA MESSAGGI DEL PGM
236700000000     C*------------------------------------------------------------------------*
236800000000     C     CARMSG        BEGSR
236900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
237000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
237100000000     C     MSGULT        IFEQ      0
237200000000     C                   MOVEL     '1'           *IN40
237300000000     C                   WRITE     TL02DVK
237400000000     C                   MOVEL     '0'           *IN40
237500000000     C                   ENDIF
237600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
237700000000     C                   MOVEL     $MSG          DSMSMS
237800000000     C                   CALL      'XMSG'        XMSGPA
237900000000     C     DSMSER        IFEQ      0
238000000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
238100000000     C                   ENDIF
238200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
238300000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
238400000000     C                   WRITE     TL02DVM                                      *SFL MESS
238500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
238600000000     C                   MOVEL     *BLANKS       $MSG
238700000000     C                   ENDSR
238800000000     C*------------------------------------------------------------------------*
238900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
239000000000     C*------------------------------------------------------------------------*
239100000000     C     OUTMSG        BEGSR
239200000000     C* VISUALIZZAZIONE MESSAGGIO
239300000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
239400000000     C                   WRITE     TL02DVK                                      *CONTR MESS
239500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
239600000000     C                   Z-ADD     *ZEROS        MSGULT
239700000000     C                   ENDSR
239800000000     C*------------------------------------------------------------------------*
239900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
240000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
240100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
240200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
240300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
240400000000     C*------------------------------------------------------------------------*
240500000000     C     CARFUN        BEGSR
240600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
240700000000     C     1             DO        31            I                 5 0          --- 1 -->
240800000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
240900000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
241000000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
241100020715     c                   If        DutLpo = 'S'
241200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
241300000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
241400000000     C                   ELSE                                                   *INTERROG.
241500000000     C                   MOVEL     FUNASI        FAT(I)
241600000000     C                   ENDIF
241700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
241800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
241900000000     C                   MOVEL     FUNAFG        FAT(I)
242000000000     C                   ELSE                                                   *INTERROG.
242100000000     C                   MOVEL     FUNAFI        FAT(I)
242200000000     C                   ENDIF
242300000000     C                   ENDIF                                                  <-- 2 ---
242400000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
242500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
242600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
242700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
242800000000     C                   ELSE
242900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
243000000000     C                   ENDIF
243100000000     C                   ENDIF
243200000000     C                   ENDDO                                                  <-- 1 ---
243300000000     C                   ENDSR
243400000000     C*------------------------------------------------------------------------*
243500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
243600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
243700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
243800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
243900000000     C*------------------------------------------------------------------------*
244000000000     C     OUTFUN        BEGSR
244100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
244200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
244300110506     C                   MOVEL     *BLANKS       FiH(I)                         *VIDEO
244400000000     C                   ENDDO
244500110506     c
244600110506     c                   setoff                                       818283
244700110506     c                   setoff                                       858586
244800110506     c                   setoff                                       8788
244900110506     c
245000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
245100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
245200000000     C     1             DO        31            I
245300000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
245400000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
245500000000     C                   ADD       1             FUNNAT
245600000000     C                   ENDIF
245700000000     C                   ENDDO
245800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
245900000000     C     FUNNAT        IFGT      8
246000000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
246100000000     C                   ENDIF
246200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
246300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
246400000000     C                   Z-ADD     8             FUNVIS            5 0
246500000000     C     FAT(23)       IFEQ      'S'
246600000000     C                   SUB       1             FUNVIS
246700000000     C                   ENDIF
246800000000     C     FAT(24)       IFEQ      'S'
246900000000     C                   SUB       1             FUNVIS
247000000000     C                   ENDIF
247100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
247200000000     C                   Z-ADD     *ZEROS        W                 5 0
247300000000     C                   ADD       1             FUNULT
247400000000     C     FUNULT        IFGT      31
247500000000     C                   Z-ADD     1             FUNULT
247600000000     C                   ENDIF
247700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
247800000000     C     FUNULT        DO        31            I                              --- 1 -->
247900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
248000000000     C     W             IFLT      FUNVIS                                       --- 2 -->
248100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
248200000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
248300000000     C     FDE(I)        ANDNE     *BLANKS
248400000000     C     I             IFEQ      23                                           *TOLGO F23
248500000000     C     I             OREQ      24                                           *E F24
248600000000     C                   ELSE
248700000000     C                   ADD       1             W
248800000000     C                   MOVEL     FDE(I)        FUV(W)
248900110506     c* In base alle richeiste visualizzo in HI il tasto se presennti i dati nel
249000110506     c*  relativo file
249100110506     c                   exsr      SkiFIH
249200110506     c
249300000000     C                   ENDIF
249400000000     C                   ENDIF                                                  <-- 3 ---
249500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
249600000000     C                   Z-ADD     I             FUNULT
249700000000     C                   ENDIF                                                  <-- 2 ---
249800000000     C                   ENDDO                                                  <-- 1 ---
249900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
250000000000     C                   MOVEL     FUV(1)        VFD001
250100110506     c                   if        fih(1)='S'
250200110506     c                   seton                                        81
250300110506     c                   endif
250400000000     C                   MOVEL     FUV(2)        VFD002
250500110506     c                   if        fih(2)='S'
250600110506     c                   seton                                        82
250700110506     c                   endif
250800000000     C                   MOVEL     FUV(3)        VFD003
250900110506     c                   if        fih(3)='S'
251000110506     c                   seton                                        83
251100110506     c                   endif
251200000000     C                   MOVEL     FUV(4)        VFD004
251300110506     c                   if        fih(4)='S'
251400110506     c                   seton                                        84
251500110506     c                   endif
251600000000     C                   MOVEL     FUV(5)        VFD005
251700110506     c                   if        fih(5)='S'
251800110506     c                   seton                                        85
251900110506     c                   endif
252000000000     C                   MOVEL     FUV(6)        VFD006
252100110506     c                   if        fih(6)='S'
252200110506     c                   seton                                        86
252300110506     c                   endif
252400000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
252500000000     C     FAT(24)       ANDEQ     'N'
252600000000     C                   MOVEL     FUV(7)        VFD007
252700110506     c                   if        fih(7)='S'
252800110506     c                   seton                                        87
252900110506     c                   endif
253000000000     C                   MOVEL     FUV(8)        VFD008
253100110506     c                   if        fih(8)='S'
253200110506     c                   seton                                        88
253300110506     c                   endif
253400000000     C                   ELSE                                                   <-- 1 -->
253500000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
253600000000     C     FAT(24)       ANDEQ     'S'
253700000000     C                   MOVEL     FDE(23)       VFD007
253800000000     C                   MOVEL     FDE(24)       VFD008
253900000000     C                   ELSE                                                   <-- 2 -->
254000000000     C                   MOVEL     FUV(7)        VFD007
254100110506     c                   if        fih(7)='S'
254200110506     c                   seton                                        87
254300110506     c                   endif
254400000000     C     FAT(23)       IFEQ      'S'
254500000000     C                   MOVEL     FDE(23)       VFD008
254600000000     C                   ELSE
254700000000     C                   MOVEL     FDE(24)       VFD008
254800000000     C                   ENDIF
254900000000     C                   ENDIF                                                  <-- 2 ---
255000000000     C                   ENDIF                                                  <-- 1 ---
255100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
255200000000     C                   ENDSR
255300000000     C*------------------------------------------------------------------------*
255400000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
255500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
255600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
255700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
255800000000     C*------------------------------------------------------------------------*
255900000000     C     CONFUN        BEGSR
256000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
256100000000     C     1             DO        31            I                              --- 1 -->
256200000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
256300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
256400000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
256500000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
256600000000     C                   EXSR      CARMSG
256700000000     C                   MOVEL     '1'           $ERR
256800000000     C                   ENDIF                                                  <-- 3 ---
256900000000     C                   ENDIF                                                  <-- 2 ---
257000000000     C                   ENDDO                                                  <-- 1 ---
257100000000     C                   ENDSR
257200110506     C*------------------------------------------------------------------------*
257300110506     C*  SkiFIH - imposto la schier per l'indicatore di HI
257400110506     C*------------------------------------------------------------------------*
257500110506     c     SkiFIH        BEGSR
257600110506     c                   eval      FIH(W)='N'
257700110506     c
257800110506     C                   Z-ADD     V1CTRN        KLrTRN                         *TRAINO
257900110506     C                   Z-ADD     DDEV1C        KLrDDE                         *DATA DECORRENZA
258000110506     c
258100110506     c                   select
258200110506     c                   when      ffu(i)='F8   '
258300110506     c     keytlr        setll     tntlr01l
258400110506     c                   if        %equal(tntlr01l)
258500110506     c                   eval      FIH(W)='S'
258600110506     c                   endif
258700110506
258800110506     c                   when      ffu(i)='F9   '
258900110506     c     keytlr        setll     tntlm01l
259000110506     c                   if        %equal(tntlm01l)
259100110506     c                   eval      FIH(W)='S'
259200110506     c                   endif
259300110506
259400110506     c                   when      ffu(i)='F10  '
259500110506     c     keytlr        setll     titll01l
259600110506     c                   if        %equal(titll01l)
259700110506     c                   eval      FIH(W)='S'
259800110506     c                   endif
259900110506
260000110506     c                   when      ffu(i)='F11  '
260100110506     c     keytlr        setll     tntln01l
260200110506     c                   if        %equal(tntln01l)
260300110506     c                   eval      FIH(W)='S'
260400110506     c                   endif
260500110506
260600110519     c                   when      ffu(i)='F2   '
260700110526     c     keytlr        chain     tntls01l
260800110526     c                   if        %found(tntls01l)  and tlsatb=' '
260900110506     c                   eval      FIH(W)='S'
261000110506     c                   endif
261100110506     c                   endsl
261200110506     c                   ENDSR
261300000000** - MSG MESSAGGI
261400000000TNTL02R- Tasto funzionale non valido.                                          1
261500000000TNTL02R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
261600000000TNTL02R- Opzione non valida.                                                   3
261700000000TNTL02R- Opzione abilitata ma non gestita. Avvertire CED.                      4
261800000000TNTL02R- Inizio elenco.                                                        5
261900000000TNTL02R- Fine elenco.                                                          6
262000000000TNTL02R- Impossibile inserire traini con numero > 9.000.000                    7
262100000000TNTL02R- Numero traino errato                                                  8
262200000000TNTL02R- Data errata                                                           9
262300000000TNTL02R- Richiesta funzione per traino già esistente                          10
262400000000TNTL02R- La data scadenza deve essere maggiore della decorrenza               11
262500000000TNTL02R- Il traino immesso si sovrappone ad uno già in decorrenza             12
262600000000TNTL02R- Filiale errata o non gestita nei fogli viaggio                       13
262700000000TNTL02R- Tipo traino errato                                                   14
262800000000TNTL02R- Immettere un numero da 1 a 7 corrispondente al giorno di partenza    15
262900000000TNTL02R- Immettere il numero 'x' per indicare che la partenza è di xxxxxxxxxx 16
263000000000TNTL02R- Tipo automezzo errato                                                17
263100000000TNTL02R- Immettere ' ' oppure 'B' per indicare un traino Normale o Bis        18
263200000000TNTL02R- La filiale partenza deve essere un 2° livello non terminal di arrivo 19
263300000000TNTL02R- la filiale partenza deve essere un 1° livello                        20
263400000000TNTL02R- La fil arrivo deve avere come terminal arrivo la filiale part traino 21
263500000000TNTL02R- La fil arrivo deve essere terminal partenza della fil part traino    22
263600000000TNTL02R- Traino già trasmesso: impossibile cambiare la fil.partenza (Ex=999)  23
263700000000TNTL02R- La data decorrenza deve essere uguale alla data scadenza             24
263800000000TNTL02R- Impossibile modificare il tipo traino da chiuso a non, e viceversa   25
263900000000TNTL02R- La filiale arrivo deve essere terminal arrivo                        26
264000000000TNTL02R- Il traino non può scadere in un giorno già trascorso                 27
264100000000TNTL02R- Nei traini locali filiale partenza e arrivo devono essere uguali     28
264200000000TNTL02R- La filiale partenza deve essere diversa dalla filiale arrivo         29
264300000000Ritorno senza aggiornamento: premere di nuovo F12/F3 per confermare l'uscita  70
264400000000TNTL02R- Attenzione! La filiale che espone è diversa dalla filiale partenza   31
264500000000TNTL02R- 32
264600000000TNTL02R- Impossibile inserire un traino di tipo "chiuso"                      33
264700000000TNTL02R- Traino già esistente                                                 34
264800000000TNTL02R- Record inesistente                                                   35
264900000000TNTL02R- Record momentaneamente allocato: riprovare più tardi                 36
265000000000TNTL02R- Il traino inoltro non si riconduce a nessun tipo traino previsto     37
265100000000TNTL02R- 38
265200000000TNTL02R- 39
265300961017TNTL02R- Errori di congruità nel database. Traino non più ripristinabile      40
265400000000** - FUN  FUNZIONI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
265500000000F1    -                   -    NN     NN
265600110609F2    -F2=Punti Scambio   -    SS     SS
265700000000F3    -F3=Fine            -    SS     SS
265800000000F4    -                   -    NN     NN
265900000000F5    -                   -    NN     NN
266000000000F6    -F6=CONFERMA        -    SN     SN
266100110519F7    -                   -    NN     NN
266200000000F8    -F8=Fermate         -    SS     SS
266300000000F9    -F9=MCubi assegnati -    SS     SS
266400000000F10   -F10=Listini        -    SS     SS
266500000000F11   -F11=Note           -    SS     SS
266600000000F12   -F12=Ritorno        -    SS     SS
266700000000F13   -                   -    NN     NN
266800110519F14   -                   -    NN     NN
266900000000F15   -                   -    NN     NN
267000000000F16   -                   -    NN     NN
267100000000F17   -                   -    NN     NN
267200000000F18   -                   -    NN     NN
267300000000F19   -                   -    NN     NN
267400000000F20   -                   -    NN     NN
267500000000F21   -                   -    NN     NN
267600000000F22   -                   -    NN     NN
267700000000F23   -                   -    NN     NN
267800000000F24   -F24=Altri tasti    -    NN     NN
267900000000ENTER -                   -    SS     SS
268000000000ROLLUP-                   -    NN     NN
268100000000ROLLDN-                   -    NN     NN
268200000000HELP  -                   -    NN     NN
268300000000PRINT -                   -    NN     NN
268400000000BSPACE-                   -    NN     NN
268500000000CLEAR -                   -    NN     NN
268600000000** - DOPZ  OPZIONE RICHIAMATA
268700000000  INSERIMENTO       1
268800000000  VARIAZIONE        2
268900000000 ANNULLAMENTO       3
269000000000  RIPRISTINO        4
269100000000VISUALIZZAZIONE     5
