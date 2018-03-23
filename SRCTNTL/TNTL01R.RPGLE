000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - TRAINI - SCELTA SUBFILE
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL01D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL01DVS:SFLULT)
001000000000     F                                     SFILE(TL01DVM:MSGULT)
001100960208     FAZORG01L  IF   E           K DISK
001200960208     FTABEL00F  IF   E           K DISK
001300960930     F*------------
001400960930     F* ANAGRAFICHE TRAINI
001500960930     F*------------
001600000000     FTNTLT01L  IF   E           K DISK
001700960208     FTNTLT03L  IF   E           K DISK
001800960208     F                                     RENAME(TNTLT000:TNTLT3)
001900960930     F*------------
002000960930     F* STORICO TRAINI
002100960930     F*------------
002200971216     FTNTST01L  IF   E           K DISK    USROPN
002300000000     D*------------------------------------------------------------------------*
002400000000     D* SCHIERE
002500000000     D*------------------------------------------------------------------------*
002600000000     D*-------------------
002700000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002800000000     D*-------------------
002900000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
003000000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
003100000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
003200000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
003300000000     D*-------------------
003400000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003500000000     D*-------------------
003600000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003700000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003800000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003900000000     D*-------------------
004000000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
004100000000     D*-------------------
004200000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
004300000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
004400000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
004500000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
004600000000     D*-------------------
004700000000     D* TABELLA DESCRIZIONI A VIDEO
004800000000     D*-------------------
004900000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
005000000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
005100000000     D*-------------------
005200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
005300000000     D*-------------------
005400000000     D KEYS            S              1    DIM(40)
005500000000     D*-------------------
005600000000     D* FILIALI
005700000000     D*-------------------
005800971125     D ORG             S              3  0 DIM(500)                             *CODICE
005900971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
006000000000     D*-------------------
006100000000     D* TIPI TRAINI
006200000000     D*-------------------
006300000000     D TTR             S              1    DIM(20)                              *TIPI TRAINI
006400000000     D DTTR            S             30    DIM(20)                              *DESCRIZIONE
006500000000     D*-------------------
006600000000     D* TIPI AUTOMEZZI
006700000000     D*-------------------
006800000000     D TMZ             S              2    DIM(20)                              *CODICE
006900000000     D DTMZ            S             30    DIM(20)                              *DESCRIZIONE
007000000000     D*------------------------------------------------------------------------*
007100000000     D* INPUT
007200000000     D*------------------------------------------------------------------------*
007300000000     D*---------------------
007400000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
007500000000     D*---------------------
007600000000     D DSOPZ         E DS
007700000000     D  OPZRIG                 1     40
007800000000     D*---------------------
007900000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
008000000000     D*---------------------
008100000000     D DSFUN         E DS
008200000000     D  FUNRIG                 1     40
008300000000     D*---------------------
008400000000     D* DS PER GESTIONE TASTI FUNZIONALI
008500000000     D*---------------------
008600000000     D DSKEYS        E DS
008700000000     D  KEYRIG                 1     40
008800000000     D*---------------------
008900000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
009000000000     D*---------------------
009100000000     D                SDS
009200000000     D  USER                 254    263
009300000000     D*---------------------
009400000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
009500000000     D*---------------------
009600000000     D DEVDS1          DS
009700000000     D  KEY                  369    369
009800000000     D  CURSOR               370    371
009900000000     D  FRTSFL               378    379B 0
010000000000     D*---------------------
010100000000     D* ARCHITETTURA
010200000000     D*---------------------
010300000000     D KPJBA         E DS
010400000000     D*---------------------
010500000000     D* DS ESTERNA DI PROCEDURA
010600000000     D*---------------------
010700000000     D DS00          E DS                  EXTNAME(TNTL00DS)
010800000000     D*---------------------
010900000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
011000000000     D*---------------------
011100000000     D DSMSG         E DS
011200000000     D*---------------------
011300000000     D* DS TIPI TRAINI
011400000000     D*---------------------
011500000000     D DSTV          E DS
011600000000     D*---------------------
011700000000     D* DS TIPI AUTOMEZZI
011800000000     D*---------------------
011900000000     D DSTU          E DS
012000960208     D*---------------------
012100960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
012200960208     D*---------------------
012300960208     D OG148         E DS
012400020715
012500020715     d DdatiUte      e ds
012600020715     d AzuteDs       e ds                  ExtName(Azute00f)
012700020715     d Tibs34Ds      e ds                  Inz
012800020715
012900000000     D*------------------
013000000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
013100000000     D*------------------
013200000000     D WLBDA8          DS
013300000000     D  G08DAT                 1      8  0
013400000000     D  G08INV                 9     16  0
013500000000     D  G08ERR                17     17
013600000000     D  G08TGI                18     22  0
013700991229     D*------------------
013800991229     D* DS S/COMPOSIZIONE DATA (8)
013900991229     D*------------------
014000991229     D                 DS
014100991229     D  DSGIO8                 1      2  0
014200991229     D  DSMES8                 3      4  0
014300991229     D  DSANN8                 5      8  0
014400991229     D  DSDAT8                 1      8  0
014500991229     D*------------------
014600991229     D* DS S/COMPOSIZIONE DATA (6)
014700991229     D*------------------
014800991229     D                 DS
014900991229     D  DSGIO6                 1      2  0
015000991229     D  DSMES6                 3      4  0
015100991229     D  DSANN6                 5      6  0
015200991229     D  DSDAT6                 1      6  0
015300960325     D*------------------
015400960325     D* DS "TNTL23R" - LANCIO CREAZIONE CRIGLIA
015500960325     D*------------------
015600960325     D PARD23          DS                  INZ
015700960325     D  D23TRN                 1      7
015800960325     D  D23CRT                 8      8
015900000000     D*------------------
016000000000     D* DEFINIZIONE COSTANTI
016100000000     D*------------------
016200000000     D DESGES          C                   CONST('** GESTIONE **')
016300000000     D DESINT          C                   CONST('INTERROGAZIONE')
016400000000     C*------------------------------------------------------------------------*
016500000000     C* MAIN LINE
016600000000     C*------------------------------------------------------------------------*
016700000000     C*
016800000000     C* CICLO FINO A CHE:
016900000000DO  1C     $FINE         DOUEQ     '1'
017000000000     C*
017100000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
017200000000     C*
017300000000     C* CONTROLLA GESTIONE VIDEO
017400000000IF  2C     $GES          IFEQ      'VS'
017500000000     C                   EXSR      GESVS                                        *SUBFILE
017600000000E   2C                   ENDIF
017700000000     C*
017800000000E   1C                   ENDDO
017900000000     C*
018000000000     C                   MOVEL     DS00          KPJBU
018100000000     C                   SETON                                        LR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
018400000000     C*------------------------------------------------------------------------*
018500000000     C     CARVS         BEGSR
018600000000     C*
018700000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
018800000000     C*
018900000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
019000000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
019100000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
019200000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
019300000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
019400000000     C*
019500000000     C                   WRITE     TL01DVT                                      *TESTATA
019600000000     C                   WRITE     TL01DVO                                      *OPZIONI
019700000000     C                   WRITE     TL01DVF                                      *FUNZIONI
019800000000     C*
019900000000     C                   EXSR      POSSFL                                       *CARICA SFL
020000000000     C*
020100000000     C                   ENDSR
020200000000     C*------------------------------------------------------------------------*
020300000000     C* GESVS  - GESTIONE SUBFILE
020400000000     C*------------------------------------------------------------------------*
020500000000     C     GESVS         BEGSR
020600000000     C*
020700000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
020800000000     C                   WRITE     TL01DVT                                      *TESTATA
020900000000     C                   WRITE     TL01DVO                                      *OPZIONI
021000000000     C                   WRITE     TL01DVF                                      *FUNZIONI
021100000000E   1C                   ENDIF
021200000000     C*
021300000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
021400000000     C*
021500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
021600000000     C*
021700000000IF  1C     $ERR          IFEQ      '0'
021800000000SL  2C                   SELECT
021900000000     C     KEY           WHENEQ    F10
022000000000     C                   EXSR      F10GES                                       *INSERIMENTO
022100000000     C     KEY           WHENEQ    F21
022200000000     C                   EXSR      F21GES                                       *STAMPA
022300000000     C                   OTHER
022400000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
022500000000E   2C                   ENDSL
022600000000E   1C                   ENDIF
022700000000     C*
022800000000     C                   ENDSR
022900000000     C*------------------------------------------------------------------------*
023000000000     C* F10GES - GESTIONE TASTO FUNZIONALE F10 ==> INSERIMENTO
023100000000     C*------------------------------------------------------------------------*
023200000000     C     F10GES        BEGSR
023300000000     C*
023400000000     C* LANCIA L'INSERIMENTO
023500000000     C                   Z-ADD     *ZEROS        WTRN                           *NUMERO TRAINO
023600000000     C                   Z-ADD     *ZEROS        WDDE                           *DATA DECORRENZA
023700000000     C*
023800000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
023900000000     C                   MOVEL     'F10'         WOP1                           *LIVELLO PROGRAMMA
024000000000     C                   MOVEL     'TNTL01C'     WPGM              8            *PGM DI GESTIONE
024100000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
024200000000     C*
024300000000     C                   EXSR      POSSFL                                       *RICARICA SUBFILE
024400000000     C*
024500000000     C                   ENDSR
024600000000     C*------------------------------------------------------------------------*
024700000000     C* F21GES - GESTIONE TASTO FUNZIONALE F21 ==> STAMPA TRAINI
024800000000     C*------------------------------------------------------------------------*
024900000000     C     F21GES        BEGSR
025000000000     C*
025100000000     C* LANCIA LA RICHIESTA PARAMETRI PER LA STAMPA
025200000000     C                   MOVEL     'F21'         D00OP1                         *LIVELLO PROGRAMMA
025300000000     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
025400000000     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
025500000000     C                   MOVEL     '0'           D00ERR                         *ERRORE
025600000000     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
025700000000     C*
025800000000     C                   MOVEL     DS00          KPJBU
025900000000     C                   CALL      'TNTL18R'                            99
026000000000     C                   PARM                    KPJBA
026100000000     C                   MOVEL     KPJBU         DS00
026200000000     C*
026300000000     C* MANDA IL MESSAGGIO DI RITORNO
026400000000IF  1C     D00F12        IFNE      '1'                                          *NO F12-RITORNO
026500000000     C                   MOVEL     D00MSG        $MSG
026600000000     C                   EXSR      CARMSG
026700000000E   1C                   ENDIF
026800000000     C*
026900000000     C                   ENDSR
027000000000     C*------------------------------------------------------------------------*
027100000000     C*  GESOPZ - GESTIONE OPZIONI
027200000000     C*------------------------------------------------------------------------*
027300000000     C     GESOPZ        BEGSR
027400000000     C*
027500000000SL  1C                   SELECT
027600000000     C     VSCOPZ        WHENEQ    ' 2'
027700000000     C                   EXSR      O02GES                                       *MODIFICA
027800000000     C     VSCOPZ        WHENEQ    ' 3'
027900000000     C                   EXSR      O03GES                                       *DUPLICA
028000000000     C     VSCOPZ        WHENEQ    ' 4'
028100000000     C                   EXSR      O04GES                                       *ANNULLAMENTO
028200000000     C     VSCOPZ        WHENEQ    ' 5'
028300000000     C                   EXSR      O05GES                                       *VISUALIZZAZIONE
028400000000     C     VSCOPZ        WHENEQ    ' 7'
028500000000     C                   EXSR      O07GES                                       *RIPRISTINO
028600000000     C     VSCOPZ        WHENEQ    '12'
028700000000     C                   EXSR      O12GES                                       *LANCIA CREA GRIGLIA
028800000000     C                   OTHER
028900000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
029000000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
029100000000E   1C                   ENDSL
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C* O02GES - GESTIONE OPZIONE 2 ==> MODIFICA
029600000000     C*------------------------------------------------------------------------*
029700000000     C     O02GES        BEGSR
029800000000     C*
029900000000IF  1C     VSCATB        IFEQ      'A'                                          *ANNULLATO
030000000000     C                   MOVEL     '1'           $ERR
030100000000     C                   MOVEL     MSG(19)       $MSG
030200000000     C                   EXSR      CARMSG
030300000000E   1C                   ENDIF
030400000000     C*
030500000000     C* LANCIA LA MODIFICA
030600000000IF  1C     $ERR          IFEQ      '0'
030700000000     C                   Z-ADD     VSCTRN        WTRN                           *NUMERO TRAINO
030800000000     C                   Z-ADD     VSHDDE        WDDE                           *DATA DECORRENZA
030900000000     C*
031000000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
031100000000     C                   MOVEL     'O02'         WOP1                           *LIVELLO PROGRAMMA
031200000000     C                   MOVEL     'TNTL01C'     WPGM                           *PGM DI GESTIONE
031300000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
031400000000     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
031500000000E   1C                   ENDIF
031600000000     C*
031700000000     C                   ENDSR
031800000000     C*------------------------------------------------------------------------*
031900000000     C* O03GES - GESTIONE OPZIONE 3 ==> DUPLICA
032000000000     C*------------------------------------------------------------------------*
032100000000     C     O03GES        BEGSR
032200961017     C*
032300961017     C* ESCLUDE GLI ANNULLATI, DEVE PASSARE DAI CONTROLLI DEL RIPRISTINO
032400961017IF  1C     VSCATB        IFEQ      'A'
032500961017     C                   MOVEL     '1'           $ERR
032600961017     C                   MOVEL     MSG(19)       $MSG
032700961017     C                   EXSR      CARMSG
032800961017E   1C                   ENDIF
032900000000     C*
033000000000     C* LANCIA LA DUPLICA
033100000000IF  1C     $ERR          IFEQ      '0'
033200000000     C                   Z-ADD     VSCTRN        WTRN                           *NUMERO TRAINO
033300000000     C                   Z-ADD     VSHDDE        WDDE                           *DATA DECORRENZA
033400000000     C*
033500000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
033600000000     C                   MOVEL     'O03'         WOP1                           *LIVELLO PROGRAMMA
033700000000     C                   MOVEL     'TNTL21R'     WPGM                           *PGM DI GESTIONE
033800000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
033900000000     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
034000000000E   1C                   ENDIF
034100000000     C*
034200000000     C                   ENDSR
034300000000     C*------------------------------------------------------------------------*
034400000000     C* O04GES - GESTIONE OPZIONE 4 ==> ANNULLAMENTO
034500000000     C*------------------------------------------------------------------------*
034600000000     C     O04GES        BEGSR
034700000000     C*
034800000000IF  1C     VSCATB        IFEQ      'A'                                          *ANNULLATO
034900000000     C                   MOVEL     '1'           $ERR
035000000000     C                   MOVEL     MSG(19)       $MSG
035100000000     C                   EXSR      CARMSG
035200000000E   1C                   ENDIF
035300960930     C*
035400960930     C* CONTROLLA SE IL TRAINO PUO' ESSERE ANNULLATO
035500960930IF  1C     $ERR          IFEQ      '0'
035600960930     C                   EXSR      CHKANN
035700960930E   1C                   ENDIF
035800000000     C*
035900000000     C* LANCIA L'ANNULLAMENTO
036000000000IF  1C     $ERR          IFEQ      '0'
036100000000     C                   Z-ADD     VSCTRN        WTRN                           *NUMERO TRAINO
036200000000     C                   Z-ADD     VSHDDE        WDDE                           *DATA DECORRENZA
036300000000     C*
036400000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
036500000000     C                   MOVEL     'O04'         WOP1                           *LIVELLO PROGRAMMA
036600000000     C                   MOVEL     'TNTL01C'     WPGM                           *PGM DI GESTIONE
036700000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
036800000000     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
036900000000E   1C                   ENDIF
037000000000     C*
037100000000     C                   ENDSR
037200000000     C*------------------------------------------------------------------------*
037300000000     C* O05GES - GESTIONE OPZIONE 5 ==> VISUALIZZAZIONE
037400000000     C*------------------------------------------------------------------------*
037500000000     C     O05GES        BEGSR
037600000000     C*
037700000000     C* LANCIA LA VISUALIZZAZIONE
037800000000     C                   Z-ADD     VSCTRN        WTRN                           *NUMERO TRAINO
037900000000     C                   Z-ADD     VSHDDE        WDDE                           *DATA DECORRENZA
038000000000     C*
038100000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
038200000000     C                   MOVEL     'O05'         WOP1                           *LIVELLO PROGRAMMA
038300000000     C                   MOVEL     'TNTL01C'     WPGM                           *PGM DI GESTIONE
038400000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
038500000000     C*
038600000000     C                   ENDSR
038700000000     C*------------------------------------------------------------------------*
038800000000     C* O07GES - GESTIONE OPZIONE 7 ==> RIPRISTINO
038900000000     C*------------------------------------------------------------------------*
039000000000     C     O07GES        BEGSR
039100000000     C*
039200000000IF  1C     VSCATB        IFEQ      *BLANKS                                      *NON ANCORA ANNULLAT
039300000000     C                   MOVEL     '1'           $ERR
039400000000     C                   MOVEL     MSG(20)       $MSG
039500000000     C                   EXSR      CARMSG
039600000000E   1C                   ENDIF
039700000000     C*
039800000000     C* LANCIA IL RIPRISTINO
039900000000IF  1C     $ERR          IFEQ      '0'
040000000000     C                   Z-ADD     VSCTRN        WTRN                           *NUMERO TRAINO
040100000000     C                   Z-ADD     VSHDDE        WDDE                           *DATA DECORRENZA
040200000000     C*
040300000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
040400000000     C                   MOVEL     'O07'         WOP1                           *LIVELLO PROGRAMMA
040500000000     C                   MOVEL     'TNTL01C'     WPGM                           *PGM DI GESTIONE
040600000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
040700000000     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
040800000000E   1C                   ENDIF
040900000000     C*
041000000000     C                   ENDSR
041100000000     C*------------------------------------------------------------------------*
041200000000     C* O12GES - GESTIONE OPZIONE 12 ==> CREA GRIGLIA PER I TRAINO
041300000000     C*------------------------------------------------------------------------*
041400000000     C     O12GES        BEGSR
041500000000     C*
041600000000IF  1C     $TGES         IFEQ      'G'
041700000000     C     VSCATB        ANDEQ     'A'
041800000000     C                   MOVEL     '1'           $ERR
041900000000     C                   MOVEL     MSG(19)       $MSG
042000000000     C                   EXSR      CARMSG
042100000000E   1C                   ENDIF
042200960325     C*
042300960325     C* LANCIA LA CREAZIONE DELLA GRIGLIA PER IL TRAINO
042400960325IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
042500960325     C                   MOVEL     KPJBU         DKPJBU                         *KPJBA --> DEPOSITO
042600960325     C                   MOVEL     VSCTRN        D23TRN                         *NUMERO TRAINO
042700960325     C                   MOVEL     'S'           D23CRT                         *RICRAZIONE GRIGLIA
042800960325     C                   MOVEL     PARD23        KPJBU
042900960325     C                   CALL      'TNTL23R'
043000960325     C                   PARM                    KPJBA
043100960325     C                   MOVEL     DKPJBU        KPJBU                          *DEPOSITO --> KPJBA
043200960325     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
043300960325E   1C                   ENDIF
043400000000     C*
043500000000     C                   ENDSR
043600960930     C*------------------------------------------------------------------------*
043700960930     C* CHKANN - CONTROLLA SE IL TRAINO PUO' ESSERE ANNULLATO
043800960930     C*------------------------------------------------------------------------*
043900960930     C     CHKANN        BEGSR
044000960930     C*---
044100960930     C* SE IN GRIGLIA ESISTONO DEI RECORD VALIDI E DA CONTABILIZZARE CHE SI
044200960930     C* RIFERISCONO A QUESTO TRAINO E SONO COMPRESI NELLE SUE DATE, NON ANNULLA
044300960930     C*---
044400020715     c                   If        DutLpo = 'S'
044500960930     C                   Z-ADD     VSCTRN        KSTTRN                         *NUMERO TRAINO
044600960930     C     KEYTST        SETLL     TNTST01L
044700960930     C     KEYTST        READE     TNTST01L                               98
044800971216DO  2C     *IN98         DOWEQ     *OFF
044900971216IF  3C     TSTATB        IFEQ      *BLANKS                                      *VALIDO
045000960930     C     TSTDFT        ANDEQ     *ZEROS                                       *DA CONTABILIZZARE
045100960930     C     TSTDTN        ANDGE     VSHDDE                                       *TRAINO COMPRESO IN
045200960930     C     TSTDTN        ANDLE     VSHDSC                                       *DECOR-SCAD TRAINO
045300960930     C                   MOVEL     '1'           $ERR
045400960930     C                   MOVEL     MSG(10)       $MSG
045500960930     C                   EXSR      CARMSG
045600971216E   3C                   ENDIF
045700960930     C     KEYTST        READE     TNTST01L                               98
045800971216E   2C                   ENDDO
045900971216E   1C                   ENDIF
046000960930     C*
046100960930     C                   ENDSR
046200000000     C*------------------------------------------------------------------------*
046300000000     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
046400000000     C*------------------------------------------------------------------------*
046500000000     C     NEWPOS        BEGSR
046600000000     C*
046700000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
046800000000     C*        1 = RICHIESTO POSIZIONAMENTO
046900000000     C                   Z-ADD     0             SWPOSI            7 0
047000000000     C*
047100000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
047200000000     C*            NON OCCORRE RICARICARE IL SUBFILE
047300000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
047400000000     C                   Z-ADD     0             SWMODI            1 0
047500000000     C*
047600000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
047700000000IF  1C     VCCTRN        IFNE      DEPTRN
047800960208     C     VCCTFP        ORNE      DEPTFP
047900000000     C     VCCDDE        ORNE      DEPDDE
048000960301     C     VCCBIS        ORNE      DEPBIS
048100000000     C*
048200000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
048300000000     C                   EXSR      CHKPAR
048400000000     C*
048500000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
048600000000IF  2C     $ERR          IFEQ      '0'
048700000000     C                   Z-ADD     1             SWPOSI                         *RICHIESTO POSIZ.TO
048800000000     C                   Z-ADD     VCCTRN        DEPTRN                         *SALVATO NEW POSIZ.T
048900960208     C                   Z-ADD     VCCTFP        DEPTFP                         *SALVATO NEW POSIZ.T
049000000000     C                   Z-ADD     VCCDDE        DEPDDE                         *SALVATO NEW POSIZ.T
049100960301     C                   MOVEL     VCCBIS        DEPBIS                         *SALVATO NEW POSIZ.T
049200000000E   2C                   ENDIF
049300000000E   1C                   ENDIF
049400000000     C*
049500000000     C                   ENDSR
049600000000     C*------------------------------------------------------------------------*
049700000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
049800000000     C*------------------------------------------------------------------------*
049900000000     C     CHKPAR        BEGSR
050000000000     C*
050100000000     C* REIMPOSTA INDICATORI DI ERRORE
050200960208     C                   SETOFF                                       808182
050300960301     C                   SETOFF                                       83
050400960208     C*
050500960208     C* UN SOLO TIPO DI INIZIO ELENCO
050600960208IF  1C     VCCTRN        IFNE      *ZEROS
050700960208     C     VCCTFP        ANDNE     *ZEROS
050800960208     C                   MOVEL     '1'           $ERR
050900960208     C                   MOVEL     MSG(7)        $MSG
051000960208     C                   EXSR      CARMSG
051100960208     C                   SETON                                        8082
051200960208X   1C                   ELSE
051300960208IF  2C     VCCTRN        IFNE      *ZEROS
051400960208     C                   MOVEL     'TRN'         WTIPRI                         *TIPO POSIZIONAMENTO
051500960208E   2C                   ENDIF
051600960208IF  2C     VCCTFP        IFNE      *ZEROS
051700960208     C                   MOVEL     'TFP'         WTIPRI                         *TIPO POSIZIONAMENTO
051800960208E   2C                   ENDIF
051900960208E   1C                   ENDIF
052000000000     C*--------------------
052100000000     C* DATA DECORRENZA
052200000000     C*--------------------
052300960506     C     *LIKE         DEFINE    VCCDDE        DDEVCC                         *DECORRENZA IN A/M/G
052400960506     C                   Z-ADD     *ZEROS        DDEVCC
052500960208IF  1C     $ERR          IFEQ      '0'
052600960208IF  2C     VCCDDE        IFGT      *ZEROS
052700000000     C                   Z-ADD     VCCDDE        G08DAT
052800000000     C                   Z-ADD     *ZEROS        G08INV
052900000000     C                   MOVEL     '0'           G08ERR
053000000000     C                   CALL      'XSRDA8'
053100000000     C                   PARM                    WLBDA8
053200000000     C                   Z-ADD     G08DAT        VCCDDE
053300960506     C                   Z-ADD     G08INV        DDEVCC
053400960208IF  3C     G08ERR        IFEQ      '1'
053500000000     C                   MOVEL     '1'           $ERR
053600000000     C                   MOVEL     MSG(8)        $MSG
053700000000     C                   EXSR      CARMSG
053800000000     C                   SETON                                        81
053900960208E   3C                   ENDIF
054000960208E   2C                   ENDIF
054100960208E   1C                   ENDIF
054200960301     C*--------------------
054300960423     C* SOLO TRAINI DI FILIALE (S=SOLO, N=NO, ' '=TUTTI)
054400960301     C*--------------------
054500960301IF  1C     $ERR          IFEQ      '0'
054600960301IF  2C     VCCBIS        IFEQ      ' '
054700960301     C     VCCBIS        OREQ      'S'
054800960301     C     VCCBIS        OREQ      'N'
054900960301X   2C                   ELSE
055000960301     C                   MOVEL     '1'           $ERR
055100960301     C                   MOVEL     MSG(9)        $MSG
055200960301     C                   EXSR      CARMSG
055300960301     C                   SETON                                        83
055400960301E   2C                   ENDIF
055500960301E   1C                   ENDIF
055600000000     C*
055700000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
055800000000     C* SUL CAMPO DEL CONTROL
055900000000IF  1C     $ERR          IFEQ      '1'
056000000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
056100000000X   1C                   ELSE
056200000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
056300000000E   1C                   ENDIF
056400000000     C*
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
056800000000     C*------------------------------------------------------------------------*
056900000000     C     SETFIL        BEGSR
057000000000     C*
057100000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFILE
057200000000     C*
057300960208IF  1C     WTIPRI        IFEQ      'TRN'
057400000000     C                   Z-ADD     VCCTRN        KLTTRN
057500000000     C     KEYTLT        SETLL     TNTLT01L                           99        *FINE FILE
057600960208X   1C                   ELSE
057700960208     C                   Z-ADD     VCCTFP        KLTTFP
057800960208     C     KE2TLT        SETLL     TNTLT03L                           99        *FINE FILE
057900960208E   1C                   ENDIF
058000000000     C*
058100000000IF  1C     *IN99         IFEQ      *OFF
058200000000     C                   EXSR      LETFIL
058300000000E   1C                   ENDIF
058400000000     C*
058500000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
058600000000IF  1C     *IN99         IFEQ      *ON
058700000000     C                   Z-ADD     1             SFLEOF
058800000000E   1C                   ENDIF
058900000000     C*
059000000000     C                   ENDSR
059100000000     C*------------------------------------------------------------------------*
059200000000     C* LETFIL - LETTURA SUCESSIVA RECORD
059300000000     C*------------------------------------------------------------------------*
059400000000     C     LETFIL        BEGSR
059500000000     C*
059600000000     C* LEGGO FINO A:
059700000000     C*     - FINE FILE
059800000000     C*     - TROVATO RECORD VALIDO
059900000000     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
060000000000DO  1C     *IN99         DOWEQ     *OFF
060100000000     C     WRECOK        ANDEQ     'N'
060200000000     C*
060300960208IF  2C     WTIPRI        IFEQ      'TRN'
060400000000     C                   READ(N)   TNTLT01L                               99
060500960208X   2C                   ELSE
060600960208     C                   READ(N)   TNTLT03L                               99
060700960208E   2C                   ENDIF
060800000000     C*
060900000000IF  2C     *IN99         IFEQ      *ON
061000000000     C                   Z-ADD     1             SFLEOF
061100000000X   2C                   ELSE
061200000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
061300000000E   2C                   ENDIF
061400000000     C*
061500000000E   1C                   ENDDO
061600000000     C*
061700000000     C                   ENDSR
061800000000     C*------------------------------------------------------------------------*
061900000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
062000000000     C*------------------------------------------------------------------------*
062100000000     C     CHKREC        BEGSR
062200000000     C*
062300000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
062400000000     C*
062500960506     C* TRAINO CON DECORRENZA MINORE DAL GIORNO RICHIESTO, ESCLUDE
062600000000IF  1C     VCCDDE        IFGT      *ZEROS
062700960506IF  2C     DDEVCC        IFGE      TLTDDE
062800960506     C     DDEVCC        ANDLE     TLTDSC
062900960506X   2C                   ELSE
063000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
063100000000E   2C                   ENDIF
063200000000E   1C                   ENDIF
063300960423     C*
063400960423     C* TIPO TRAINO NON RICHIESTO, ESCLUDE
063500960423IF  1C*---       VCCBIS    IFNE *BLANKS
063600960423IF  2C*---       VCCBIS    IFEQ 'S'
063700960423     C*---       TLTBIS    ANDEQ*BLANKS
063800960423     C*---       VCCBIS    OREQ 'N'
063900960423     C*---       TLTBIS    ANDEQ'E'
064000960423     C*---                 MOVEL'N'       WRECOK           *RECORD NON VALIDO
064100960423E   2C*---                 ENDIF
064200960423E   1C*---                 ENDIF
064300960423     C*
064400960423     C* TIPO TRAINO NON RICHIESTO, ESCLUDE
064500960423IF  1C     VCCBIS        IFNE      *BLANKS
064600960423IF  2C     VCCBIS        IFEQ      'S'                                          *SOLO TRAINI FILIALE
064700960423     C     TLTTRN        ANDLT     9000000
064800960423     C     VCCBIS        OREQ      'N'                                          *NO TRAINI FILIALE
064900960423     C     TLTTRN        ANDGE     9000000
065000960423     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
065100960423E   2C                   ENDIF
065200960423E   1C                   ENDIF
065300000000     C*
065400000000     C                   ENDSR
065500000000     C*------------------------------------------------------------------------*
065600000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
065700000000     C*------------------------------------------------------------------------*
065800000000     C     IRESFL        BEGSR
065900000000     C***
066000000000     C* CAMPI NON VISUALIZZATI
066100000000     C***
066200000000     C                   Z-ADD     TLTDDE        VSHDDE                         DATA DECORRENZA (8,0
066300960930     C                   Z-ADD     TLTDSC        VSHDSC                         DATA SCADENZA   (8,0
066400000000IF  1C     TLTATB        IFNE      *BLANKS
066500000000     C                   MOVEL     'A'           VSCATB                         *ANNULLAMENTO
066600000000X   1C                   ELSE
066700000000     C                   MOVEL     *BLANKS       VSCATB
066800000000E   1C                   ENDIF
066900000000     C                   Z-ADD     TLTTRN        VSCTRN                         *TRAINO
067000991229     C*
067100991229     C                   Z-ADD     TLTDDE        G08INV                         *DATA DECORRENZA
067200991229     C                   Z-ADD     *ZEROS        G08DAT
067300991229     C                   MOVEL     '3'           G08ERR
067400991229     C                   CALL      'XSRDA8'
067500991229     C                   PARM                    WLBDA8
067600991229     C                   MOVEL     G08DAT        DSDAT8
067700991229     C                   Z-ADD     DSGIO8        DSGIO6
067800991229     C                   Z-ADD     DSMES8        DSMES6
067900991229     C                   Z-ADD     DSANN8        DSANN6
068000991229     C                   Z-ADD     DSDAT6        VSCDDE
068100991229     C*
068200991229     C                   Z-ADD     TLTDSC        G08INV                         *DATA SCADENZA
068300991229     C                   Z-ADD     *ZEROS        G08DAT
068400991229     C                   MOVEL     '3'           G08ERR
068500991229     C                   CALL      'XSRDA8'
068600991229     C                   PARM                    WLBDA8
068700991229     C                   MOVEL     G08DAT        DSDAT8
068800991229     C                   Z-ADD     DSGIO8        DSGIO6
068900991229     C                   Z-ADD     DSMES8        DSMES6
069000991229     C                   Z-ADD     DSANN8        DSANN6
069100991229     C                   Z-ADD     DSDAT6        VSCDSC
069200991229     C*
069300000000     C                   Z-ADD     TLTTFP        VSCTFP                         *FILIAE PARTENZA
069400000000     C                   Z-ADD     TLTTFA        VSCTFA                         *FILIAE ARRIVO
069500000000     C                   MOVEL     TLTTTR        VSCTTR                         *TIPO TRAINO
069600000000     C                   MOVEL     TLTTMZ        VSCTMZ                         *TIPO AUTOMEZZO
069700000000     C* DECODIFICHE
069800000000     C                   SETOFF                                           28
069900000000     C                   Z-ADD     1             J
070000000000     C     VSCTFP        LOOKUP    ORG(J)                                 28
070100000000IF  1C     *IN28         IFEQ      *ON
070200000000     C                   MOVEL     DORG(J)       VSDTFP                         *DESCRIZIONE PARTENZ
070300000000E   1C                   ENDIF
070400000000     C                   SETOFF                                           28
070500000000     C                   Z-ADD     1             J
070600000000     C     VSCTFA        LOOKUP    ORG(J)                                 28
070700000000IF  1C     *IN28         IFEQ      *ON
070800000000     C                   MOVEL     DORG(J)       VSDTFA                         *DESCRIZIONE ARRIVO
070900000000E   1C                   ENDIF
071000000000     C                   SETOFF                                           28
071100000000     C                   Z-ADD     1             J
071200000000     C     VSCTTR        LOOKUP    TTR(J)                                 28
071300000000IF  1C     *IN28         IFEQ      *ON
071400000000     C                   MOVEL     DTTR(J)       VSDTTR                         *DESCRIZ. TIPO TRAIN
071500000000E   1C                   ENDIF
071600000000     C*
071700000000     C                   ENDSR
071800000000     C*------------------------------------------------------------------------*
071900000000     C* LANPGM - LANCIA UN PROGRAMMA
072000000000     C*------------------------------------------------------------------------*
072100000000     C     LANPGM        BEGSR
072200000000     C***
072300000000     C* PARAMETRI INFORMATIVI
072400000000     C***
072500000000     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
072600000000     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PROGRAMMA
072700000000     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
072800000000     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
072900000000     C                   MOVEL     '0'           D00ERR                         *ERRORE
073000000000     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
073100000000     C***
073200000000     C* PARAMETRI
073300000000     C***
073400000000     C                   Z-ADD     WTRN          D00TRN                         *TRAINO
073500000000     C                   Z-ADD     WDDE          D00DDE                         *DECORRENZA (AA/M/G)
073600000000     C                   Z-ADD     WDTN          D00DTN                         *DATA TRAINO
073700000000     C*--------------------
073800000000     C* L A N C I O
073900000000     C*--------------------
074000000000     C                   MOVEL     DS00          KPJBU
074100000000     C                   CALL      WPGM                                 99
074200000000     C                   PARM                    KPJBA
074300000000     C                   MOVEL     KPJBU         DS00
074400000000     C*
074500000000     C* SE RITORNA PREMUTO F3 CHIUDE IL PROGRAMMA, ALTRIMENTI MANDA IL MESSAGGIO
074600000000     C* RICEVUTO DAL PROGRAMMA CHIAMATO
074700000000IF  1C     D00F03        IFEQ      '1'
074800000000     C                   MOVEL     '1'           $F03
074900000000     C                   MOVEL     '1'           $FINE
075000000000X   1C                   ELSE
075100000000     C                   MOVEL     D00MSG        $MSG
075200000000     C                   EXSR      CARMSG
075300000000E   1C                   ENDIF
075400000000     C*
075500000000     C                   ENDSR
075600000000     C*------------------------------------------------------------------------*
075700000000     C* CARTAB - CARICA LE TABELLE
075800000000     C*------------------------------------------------------------------------*
075900000000     C     CARTAB        BEGSR
076000000000     C*--------------------
076100000000     C* FILIALI
076200000000     C*--------------------
076300000000     C                   Z-ADD     *ZEROS        J                 4 0
076400000000     C     *LOVAL        SETLL     AZORG01L
076500000000     C                   READ      AZORG01L                               99
076600000000DO  1C     *IN99         DOWEQ     *OFF
076700000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
076800960208     C                   MOVEL     ORGDE8        OG148
076900960208IF  3C     §OGLFV        IFNE      'N'                                          *NON GESTIONE IN FV
077000000000     C                   ADD       1             J
077100000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
077200000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
077300960208E   3C                   ENDIF
077400960208E   2C                   ENDIF
077500000000     C                   READ      AZORG01L                               99
077600000000E   1C                   ENDDO
077700000000     C*--------------------
077800000000     C* TIPI TRAINI
077900000000     C*--------------------
078000000000     C                   Z-ADD     *ZEROS        J
078100020715     c                   z-add     1             ktbkut
078200000000     C                   MOVEL     'TV'          KTBCOD
078300000000     C     KEYTAB        SETLL     TABEL00F
078400000000     C     KEYTAB        READE     TABEL00F                               99
078500000000DO  1C     *IN99         DOWEQ     *OFF
078600000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
078700000000     C                   ADD       1             J
078800000000     C                   MOVEL     TBLKEY        TTR(J)                         *CODICE
078900000000     C                   MOVEL     TBLUNI        DSTV
079000000000     C                   MOVEL     §TVDES        DTTR(J)                        *DESCRIZIONE
079100000000E   2C                   ENDIF
079200000000     C     KEYTAB        READE     TABEL00F                               99
079300000000E   1C                   ENDDO
079400000000     C*--------------------
079500000000     C* TIPI AUTOMEZZI
079600000000     C*--------------------
079700000000     C                   Z-ADD     *ZEROS        J
079800020715     c                   z-add     1             ktbkut
079900000000     C                   MOVEL     'TU'          KTBCOD
080000000000     C     KEYTAB        SETLL     TABEL00F
080100000000     C     KEYTAB        READE     TABEL00F                               99
080200000000DO  1C     *IN99         DOWEQ     *OFF
080300000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
080400000000     C                   ADD       1             J
080500000000     C                   MOVEL     TBLKEY        TMZ(J)                         *CODICE
080600000000     C                   MOVEL     TBLUNI        DSTU
080700000000     C                   MOVEL     §TUDES        DTMZ(J)                        *DESCRIZIONE
080800000000E   2C                   ENDIF
080900000000     C     KEYTAB        READE     TABEL00F                               99
081000000000E   1C                   ENDDO
081100000000     C*
081200000000     C                   ENDSR
081300971216     C*------------------------------------------------------------------------*
081400971216     C* FINSR - OPERAZIONI FINALI
081500971216     C*------------------------------------------------------------------------*
081600971216     C     FINSR         BEGSR
081700971216     C*
081800020715     c                   If        DutLpo = 'S'
081900971216     C                   CLOSE     TNTST01L
082000971216E   1C                   ENDIF
082100971216     C*
082200971216     C                   ENDSR
082300000000     C*------------------------------------------------------------------------*
082400000000     C* *INZSR - OPERAZIONI INIZIALI
082500000000     C*------------------------------------------------------------------------*
082600000000     C     *INZSR        BEGSR
082700000000     C*--------------------
082800000000     C* RICEVIMENTO PARAMETRI
082900000000     C*--------------------
083000000000     C     *ENTRY        PLIST
083100000000     C                   PARM                    KPJBA
083200000000     C                   MOVEL     KPJBU         DS00
083300000000     C*--------------------
083400000000     C* VARIABILI RIFERITE AL DATA BASE
083500000000     C*--------------------
083600000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
083700960208     C     *LIKE         DEFINE    TLTTFP        KLTTFP
083800960930     C     *LIKE         DEFINE    TSTTRN        KSTTRN                         *TNTST00F
083900000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
084000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
084100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
084200000000     C     *LIKE         DEFINE    D00OP0        WOP0                           *DS00
084300000000     C     *LIKE         DEFINE    D00OP1        WOP1
084400000000     C     *LIKE         DEFINE    D00F03        WF03
084500000000     C     *LIKE         DEFINE    D00F12        WF12
084600000000     C     *LIKE         DEFINE    D00ERR        WERR
084700000000     C     *LIKE         DEFINE    D00MSG        WMSG
084800000000     C     *LIKE         DEFINE    D00TRN        WTRN
084900000000     C     *LIKE         DEFINE    D00DDE        WDDE
085000000000     C     *LIKE         DEFINE    D00DTN        WDTN
085100000000     C*--------------------
085200000000     C* VARIABILI NON RIFERITE AL DATA BASE
085300000000     C*--------------------
085400000000     C     *LIKE         DEFINE    VCCTRN        DEPTRN                         *DEPOSITI
085500960208     C     *LIKE         DEFINE    VCCTFP        DEPTFP
085600000000     C     *LIKE         DEFINE    VCCDDE        DEPDDE
085700960301     C     *LIKE         DEFINE    VCCBIS        DEPBIS
085800960325     C     *LIKE         DEFINE    KPJBU         DKPJBU
085900000000     C*--------------------
086000000000     C* CHIAVI DI LETTURA
086100000000     C*--------------------
086200000000     C* LETTURA TNTLT01L - PARZIALE
086300000000     C     KEYTLT        KLIST
086400000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
086500960208     C* LETTURA TNTLT03L - COMPLETA
086600960208     C     KE2TLT        KLIST
086700960208     C                   KFLD                    KLTTFP                         *FILIALE PARTENZA
086800960930     C* LETTURA TNTST01L - PARZIALE
086900960930     C     KEYTST        KLIST
087000960930     C                   KFLD                    KSTTRN                         *NUMERO TRAINO
087100000000     C* LETTURA TABEL00F - PARZIALE
087200000000     C     KEYTAB        KLIST
087300000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
087400000000     C                   KFLD                    KTBCOD                         *TABELLA
087500000000     C*--------------------
087600000000     C* VARIABILI DI CONTROLLO
087700000000     C*--------------------
087800000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
087900000000     C***
088000000000     C* IMPOSTAZIONI VIDEO
088100000000     C***
088200000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
088300000000     C                   MOVE      D00OP0        INTGES            2
088400000000IF  1C     INTGES        IFEQ      '02'
088500000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
088600000000     C                   MOVEL     DESGES        VTDOPE                         *DESCR OPERAZIONE
088700000000X   1C                   ELSE
088800000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
088900000000     C                   MOVEL     DESINT        VTDOPE                         *DESCR OPERAZIONE
089000000000E   1C                   ENDIF
089100960213     C                   MOVEL     'TRN'         WTIPRI            3            *TIPO POSIZIONAMENTO
089200960213     C                   SETON                                        50
089300960301     C***
089400960301     C* IMPOSTAZIONI CONTROL
089500960301     C***
089600960423     C                   MOVEL     'N'           VCCBIS                         *TRAINI DI FILIALE
089700960620     C                   MOVEL     VCCBIS        DEPBIS                         *TRAINI DI FILIALE
089800960321     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
089900960321     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
090000960321     C                   Z-ADD     WNUM8         VCCDDE                         *DATA DECORRENZA
090100960620     C                   Z-ADD     VCCDDE        DEPDDE                         *DATA DECORRENZA
090200960506     C                   Z-ADD     VCCDDE        G08DAT
090300960506     C                   Z-ADD     *ZEROS        G08INV
090400960506     C                   MOVEL     '0'           G08ERR
090500960506     C                   CALL      'XSRDA8'
090600960506     C                   PARM                    WLBDA8
090700960506     C                   Z-ADD     G08INV        DDEVCC
090800000000     C*
090900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
091000000000     C                   EXSR      INZSTD
091100000000     C*
091200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
091300000000     C                   EXSR      INZSFL
091400000000     C*
091500000000     C* CARICA LE TABELLE
091600000000     C                   EXSR      CARTAB
091700971216     C*
091800971216     C* APRE IL FILE DELLO STORICO TRAINI PER CONTROLLI
091900020715     c                   If        DutLpo = 'S'
092000971216     C                   OPEN      TNTST01L
092100971216E   1C                   ENDIF
092200000000     C*
092300000000     C* EMETTO VIDEATA SUBFILE
092400960506     C                   EXSR      CARVS
092500960506     C                   EXSR      CHKPAR
092600000000     C*
092700000000     C                   ENDSR
092800000000     C*------------------------------------------------------------------------*
092900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093100000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
093200000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
093300000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
093400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093600000000     C*------------------------------------------------------------------------*
093700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
093800000000     C*------------------------------------------------------------------------*
093900000000     C     INZSTD        BEGSR
094000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
094100000000     C                   CALL      'XKEYS'
094200000000     C                   PARM                    DSKEYS
094300000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
094400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
094500000000     C     XMSGPA        PLIST
094600000000     C                   PARM                    DSMSMS
094700000000     C                   PARM                    DSMSPG
094800000000     C                   PARM                    DSMSKE
094900000000     C                   PARM                    DSMSER
095000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
095100000000     C                   MOVEL     'TNTL01R'     DSMSPG                         *NOME PGM CHIAMANTE
095200000000     C                   MOVEL     'TNTL01R'     VMCQUE                         *CODA MESSAGGI DESTI
095300000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
095400000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
095500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
095600000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
095700000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
095800000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
095900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
096000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
096100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
096200000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
096300000000     C* LANCIO ROUTINE PER VERIFICARE SE
096400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
096500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
096600020715
096700020715     c     *dtaara       define    §azute        azuteds
096800020715     c     *dtaara       define    §datiute      ddatiute
096900020715
097000020715     c                   in(E)     *dtaara
097100020715     c                   if        %Error or rsut = *blanks
097200020715     c                   clear                   tibs34ds
097300020715     c                   call      'TIBS34R'
097400020715     c                   parm                    Tibs34Ds
097500020715     c                   in        *dtaara
097600020715     c                   endif
097700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
097800000000     C                   BITOFF    '01234567'    POSMI1            1
097900000000     C                   BITOFF    '01234567'    POSMI2            1
098000000000     C                   BITOFF    '01234567'    POSMA1            1
098100000000     C                   BITOFF    '01234567'    POSMA2            1
098200000000     C                   BITON     '34'          POSMI1            1            *18
098300000000     C                   BITON     '7'           POSMI2            1            *01
098400000000     C                   BITON     '34'          POSMA1            1            *18
098500000000     C                   BITON     '0'           POSMA2            1            *80
098600000000     C                   MOVE      POSMI2        POSMIN            2
098700000000     C                   MOVEL     POSMI1        POSMIN
098800000000     C                   MOVE      POSMA2        POSMAX            2
098900000000     C                   MOVEL     POSMA1        POSMAX
099000000000     C                   ENDSR
099100000000     C*------------------------------------------------------------------------*
099200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
099300000000     C*------------------------------------------------------------------------*
099400000000     C     FUNSTD        BEGSR
099500000000     C                   SELECT
099600000000     C     KEY           WHENEQ    F3
099700000000     C                   EXSR      F03STD
099800000000     C     KEY           WHENEQ    F12
099900000000     C                   EXSR      F12STD
100000000000     C     KEY           WHENEQ    F24
100100000000     C                   EXSR      F24STD
100200000000     C                   OTHER
100300000000     C                   MOVEL     MSG(2)        $MSG
100400000000     C                   EXSR      CARMSG
100500000000     C                   ENDSL
100600000000     C                   ENDSR
100700000000     C*------------------------------------------------------------------------*
100800000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
100900000000     C*------------------------------------------------------------------------*
101000000000     C     F03STD        BEGSR
101100000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
101200000000     C                   MOVEL     '1'           $F03
101300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
101400000000     C                   MOVEL     '1'           $FINE
101500951212     C* PULISCE IL MESSAGGLIO
101600951212     C                   MOVEL     *BLANKS       D00MSG
101700000000     C                   ENDSR
101800000000     C*------------------------------------------------------------------------*
101900000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
102000000000     C*------------------------------------------------------------------------*
102100000000     C     F12STD        BEGSR
102200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
102300000000     C                   MOVEL     '1'           $F12
102400000000     C* IMPOSTO FLAG DI FIN PGM
102500000000     C                   MOVEL     '1'           $FINE
102600951212     C* PULISCE IL MESSAGGLIO
102700951212     C                   MOVEL     *BLANKS       D00MSG
102800000000     C                   ENDSR
102900000000     C*------------------------------------------------------------------------*
103000000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
103100000000     C*------------------------------------------------------------------------*
103200000000     C     F24STD        BEGSR
103300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
103400000000     C                   EXSR      OUTFUN
103500000000     C                   ENDSR
103600000000     C*------------------------------------------------------------------------*
103700000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
103800000000     C*------------------------------------------------------------------------*
103900000000     C     INZSFL        BEGSR
104000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
104100000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
104200000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
104300000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
104400000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
104500000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
104600000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
104700000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
104800000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
104900000000     C                   Z-ADD     12            SFLPAG
105000000000     C                   ENDSR
105100000000     C*------------------------------------------------------------------------*
105200000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
105300000000     C*------------------------------------------------------------------------*
105400000000     C     FUNSFL        BEGSR
105500000000     C                   SELECT
105600000000     C     KEY           WHENEQ    ENTER
105700000000     C                   EXSR      ENTSFL
105800000000     C     KEY           WHENEQ    F3
105900000000     C                   EXSR      F03STD
106000000000     C     KEY           WHENEQ    F5
106100000000     C                   EXSR      F05STD
106200000000     C     KEY           WHENEQ    F12
106300000000     C                   EXSR      F12STD
106400000000     C     KEY           WHENEQ    F17
106500000000     C                   EXSR      F17STD
106600000000     C     KEY           WHENEQ    F18
106700000000     C                   EXSR      F18STD
106800000000     C     KEY           WHENEQ    F23
106900000000     C                   EXSR      F23STD
107000000000     C     KEY           WHENEQ    F24
107100000000     C                   EXSR      F24STD
107200000000     C     KEY           WHENEQ    ROLLUP
107300000000     C                   EXSR      RUPSTD
107400000000     C     KEY           WHENEQ    ROLLDN
107500000000     C                   EXSR      RDNSTD
107600000000     C                   OTHER
107700000000     C                   MOVEL     MSG(2)        $MSG
107800000000     C                   EXSR      CARMSG
107900000000     C                   ENDSL
108000000000     C                   ENDSR
108100000000     C*------------------------------------------------------------------------*
108200000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
108300000000     C*------------------------------------------------------------------------*
108400000000     C     F05STD        BEGSR
108500000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
108600000000     C                   EXSR      POSSFL
108700000000     C                   ENDSR
108800000000     C*------------------------------------------------------------------------*
108900000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
109000000000     C*------------------------------------------------------------------------*
109100000000     C     F17STD        BEGSR
109200000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
109300000000     C                   Z-ADD     1             VCCNBR
109400000000     C                   ENDSR
109500000000     C*------------------------------------------------------------------------*
109600000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
109700000000     C*------------------------------------------------------------------------*
109800000000     C     F18STD        BEGSR
109900000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
110000000000     C                   Z-ADD     SFLULT        VCCNBR
110100000000     C                   ENDSR
110200000000     C*------------------------------------------------------------------------*
110300000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
110400000000     C*------------------------------------------------------------------------*
110500000000     C     F23STD        BEGSR
110600000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
110700000000     C                   EXSR      OUTOPZ
110800000000     C                   ENDSR
110900000000     C*------------------------------------------------------------------------*
111000000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
111100000000     C*------------------------------------------------------------------------*
111200000000     C     RUPSTD        BEGSR
111300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
111400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
111500000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
111600000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
111700000000     C                   ELSE
111800000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
111900000000     C                   ENDIF
112000000000     C                   ENDSR
112100000000     C*------------------------------------------------------------------------*
112200000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
112300000000     C*------------------------------------------------------------------------*
112400000000     C     RDNSTD        BEGSR
112500000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
112600000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
112700000000     C     CURSOR        IFGE      POSMIN
112800000000     C     CURSOR        ANDLE     POSMAX
112900000000     C                   ELSE
113000000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
113100000000     C                   MOVEL     '1'           $ERR
113200000000     C                   MOVEL     MSG(5)        $MSG
113300000000     C                   EXSR      CARMSG
113400000000     C                   ENDIF
113500000000     C                   ENDSR
113600000000     C*------------------------------------------------------------------------*
113700000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
113800000000     C*------------------------------------------------------------------------*
113900000000     C     ENTSFL        BEGSR
114000000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
114100000000     C                   EXSR      NEWPOS
114200000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
114300000000     C     $ERR          IFEQ      '0'                                          --- 1 -->
114400000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
114500000000     C     SWPOSI        IFNE      0                                            --- 2 -->
114600000000     C                   EXSR      POSSFL
114700000000     C                   ELSE                                                   <-- 2 -->
114800000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
114900000000     C     SFLULT        IFNE      0                                            --- 3 -->
115000000000     C                   EXSR      OPZSFL
115100000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
115200000000     C     SWMODI        IFNE      0                                            --- 4 -->
115300000000     C                   EXSR      POSSFL
115400000000     C                   ENDIF                                                  <-- 4 ---
115500000000     C                   ENDIF                                                  <-- 3 ---
115600000000     C                   ENDIF                                                  <-- 2 ---
115700000000     C                   ENDIF                                                  <-- 1 ---
115800000000     C                   ENDSR
115900000000     C*------------------------------------------------------------------------*
116000000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
116100000000     C*------------------------------------------------------------------------*
116200000000     C     POSSFL        BEGSR
116300000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
116400000000     C                   EXSR      CLRSFL
116500000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
116600000000     C                   EXSR      SETFIL
116700000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
116800000000     C     SFLEOF        IFEQ      0
116900000000     C                   EXSR      CARSFL
117000000000     C                   ENDIF
117100000000     C                   ENDSR
117200000000     C*------------------------------------------------------------------------*
117300000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
117400000000     C*------------------------------------------------------------------------*
117500000000     C     OPZSFL        BEGSR
117600000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
117700000000     C                   MOVEL     'S'           OPZOK             1
117800000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
117900000000     C                   Z-ADD     0             SFLVAR            5 0
118000000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
118100000000     C                   Z-ADD     0             SFLERR
118200000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
118300000000     C                   Z-ADD     SFLULT        WFLULT
118400000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
118500000000     C                   READC     TL01DVS                                49
118600000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
118700000000     C     $ERR          ANDEQ     '0'
118800000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
118900000000     C                   ADD       1             SFLVAR
119000000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
119100000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
119200000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
119300000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
119400000000     C                   EXSR      CONOPZ
119500000000     C     OPZOK         IFEQ      'S'
119600000000     C     $ERR          ANDEQ     '0'
119700000000     C                   EXSR      GESOPZ
119800000000     C                   ENDIF
119900000000     C                   ENDIF                                                  <-- 3 ---
120000000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
120100000000     C*            2. PULIZIA SCELTA IN SFL
120200000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
120300000000     C                   MOVEL     '0'           *IN41
120400000000     C                   MOVEL     *BLANKS       VSCOPZ
120500000000     C                   ELSE                                                   <-- 3 -->
120600000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
120700000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
120800000000     C                   MOVEL     '1'           *IN41
120900000000     C                   Z-ADD     SFLULT        SFLERR
121000000000     C                   ENDIF                                                  <-- 3 ---
121100000000     C                   ELSE                                                   <-- 2 -->
121200000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
121300000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
121400000000     C                   MOVEL     '1'           *IN41
121500000000     C                   ENDIF                                                  <-- 2 ---
121600000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
121700000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
121800000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
121900000000     C     OPZOK         IFNE      'S'
122000000000     C                   Z-ADD     SFLERR        VCCNBR
122100000000     C                   ELSE
122200000000     C                   Z-ADD     SFLULT        VCCNBR
122300000000     C                   ENDIF
122400000000     C* AGGIORNO RECORD SFL
122500000000     C                   MOVEL     '1'           *IN46                          *POSIZIONAMENTO
122600000000     C                   UPDATE    TL01DVS
122700000000     C     $ERR          IFEQ      '0'
122800000000     C                   READC     TL01DVS                                49
122900000000     C                   ENDIF
123000000000     C                   ENDDO                                                  <-- 1 ---
123100000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
123200000000     C                   Z-ADD     WFLULT        SFLULT
123300000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
123400000000     C     SFLVAR        IFEQ      0
123500000000     C                   MOVEL     '1'           $FINE
123600000000     C                   ENDIF
123700000000     C                   ENDSR
123800000000     C*------------------------------------------------------------------------*
123900000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
124000000000     C*------------------------------------------------------------------------*
124100000000     C     CLRSFL        BEGSR
124200000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
124300000000     C* (0=NON FINE FILE  1=FINE FILE)
124400000000     C                   Z-ADD     0             SFLEOF
124500000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
124600000000     C                   Z-ADD     0             SFLULT
124700000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
124800000000     C                   MOVEL     '1'           *IN40
124900000000     C                   WRITE     TL01DVC
125000000000     C                   MOVEL     '0'           *IN40
125100000000     C                   ENDSR
125200000000     C*------------------------------------------------------------------------*
125300000000     C* CARSFL - CARICAMENTO SFL
125400000000     C*------------------------------------------------------------------------*
125500000000     C     CARSFL        BEGSR
125600000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
125700000000     C     SFLULT        IFEQ      0
125800000000     C                   EXSR      SETFIL
125900000000     C                   ENDIF
126000000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
126100000000     C                   Z-ADD     SFLULT        WFLULT
126200000000     C* CICLO:
126300000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
126400000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
126500000000     C                   Z-ADD     1             SFLRIG
126600000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
126700000000     C     SFLEOF        ANDEQ     0
126800000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
126900000000     C                   MOVEL     *BLANKS       VSCOPZ
127000000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
127100000000     C                   EXSR      IRESFL
127200000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
127300000000     C                   ADD       1             SFLULT
127400000000     C* SCRITTURA RECORD SFL
127500000000     C                   WRITE     TL01DVS
127600000000     C                   ADD       1             SFLRIG
127700000000     C* LETTURA FILE DA CARICARE
127800000000     C                   EXSR      LETFIL
127900000000     C                   ENDDO                                                  <-- 1 ---
128000000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
128100000000     C     WFLULT        IFNE      SFLULT
128200000000     C                   Z-ADD     WFLULT        VCCNBR
128300000000     C                   ADD       1             VCCNBR
128400000000     C                   ENDIF
128500000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
128600000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
128700000000     C     SFLULT        ANDGT     *ZEROS
128800000000     C                   MOVEL     '1'           $ERR
128900000000     C                   MOVEL     MSG(6)        $MSG
129000000000     C                   EXSR      CARMSG
129100000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
129200000000     C     SFLULT        CHAIN     TL01DVS                            99
129300000000     C     *IN99         IFEQ      '0'
129400000000     C                   SETON                                        45
129500000000     C                   UPDATE    TL01DVS
129600000000     C                   SETOFF                                       45
129700000000     C                   ENDIF
129800000000     C                   ENDIF                                                  <-- 1 ---
129900000000     C                   ENDSR
130000000000     C*------------------------------------------------------------------------*
130100000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
130200000000     C*------------------------------------------------------------------------*
130300000000     C     OUTSFL        BEGSR
130400000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
130500000000     C     MSGULT        IFNE      *ZEROS
130600000000     C                   EXSR      OUTMSG
130700000000     C                   ENDIF
130800000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
130900000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
131000000000     C     SFLEOF        IFEQ      0
131100000000     C                   MOVEL     '0'           *IN42
131200000000     C                   ELSE
131300000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
131400000000     C                   MOVEL     '1'           *IN42
131500000000     C                   ENDIF
131600000000     C* EMISSIONE SUBFILE
131700000000     C     SFLULT        IFLE      0
131800000000     C*  --> CASO DI SFL VUOTO
131900000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
132000000000     C                   WRITE     TL01DVV                                      *'NON VI SONO DATI'
132100000000     C                   WRITE     TL01DVC                                      *SFL CONTROL
132200000000     C                   READ      TL01DVC                                99    *SFL CONTROL
132300000000     C                   ELSE
132400000000     C*  --> CASO DI SFL CON RECORDS
132500000000     C                   WRITE     TL01DVC                                      *SFL CONTROL
132600000000     C                   READ      TL01DVC                                99    *SFL CONTROL
132700000000     C                   ENDIF
132800000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
132900000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
133000000000     C                   Z-ADD     FRTSFL        VCCNBR
133100000000     C                   ENDSR
133200000000     C*------------------------------------------------------------------------*
133300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
133400000000     C*          CODA MESSAGGI DEL PGM
133500000000     C*------------------------------------------------------------------------*
133600000000     C     CARMSG        BEGSR
133700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
133800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
133900000000     C     MSGULT        IFEQ      0
134000000000     C                   MOVEL     '1'           *IN40
134100000000     C                   WRITE     TL01DVK
134200000000     C                   MOVEL     '0'           *IN40
134300000000     C                   ENDIF
134400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
134500000000     C                   MOVEL     $MSG          DSMSMS
134600000000     C                   CALL      'XMSG'        XMSGPA
134700000000     C     DSMSER        IFEQ      0
134800000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
134900000000     C                   ENDIF
135000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
135100000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
135200000000     C                   WRITE     TL01DVM                                      *SFL MESS
135300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
135400000000     C                   MOVEL     *BLANKS       $MSG
135500000000     C                   ENDSR
135600000000     C*------------------------------------------------------------------------*
135700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
135800000000     C*------------------------------------------------------------------------*
135900000000     C     OUTMSG        BEGSR
136000000000     C* VISUALIZZAZIONE MESSAGGIO
136100000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
136200000000     C                   WRITE     TL01DVK                                      *CONTR MESS
136300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
136400000000     C                   Z-ADD     *ZEROS        MSGULT
136500000000     C                   ENDSR
136600000000     C*------------------------------------------------------------------------*
136700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
136800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
136900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
137000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
137100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
137200000000     C*------------------------------------------------------------------------*
137300000000     C     CARFUN        BEGSR
137400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
137500000000     C     1             DO        31            I                 5 0          --- 1 -->
137600000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
137700000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
137800000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
137900020715     c                   If        DutLpo = 'S'
138000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
138100000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
138200000000     C                   ELSE                                                   *INTERROG.
138300000000     C                   MOVEL     FUNASI        FAT(I)
138400000000     C                   ENDIF
138500000000     C                   ELSE                                                   <-- 2 --> *FILIALE
138600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
138700000000     C                   MOVEL     FUNAFG        FAT(I)
138800000000     C                   ELSE                                                   *INTERROG.
138900000000     C                   MOVEL     FUNAFI        FAT(I)
139000000000     C                   ENDIF
139100000000     C                   ENDIF                                                  <-- 2 ---
139200000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
139300000000     C                   MOVEL     USER          WEDP              3            *PROFILO
139400000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
139500000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
139600000000     C                   ELSE
139700000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
139800000000     C                   ENDIF
139900000000     C                   ENDIF
140000000000     C                   ENDDO                                                  <-- 1 ---
140100000000     C                   ENDSR
140200000000     C*------------------------------------------------------------------------*
140300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
140400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
140500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
140600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
140700000000     C*------------------------------------------------------------------------*
140800000000     C     OUTFUN        BEGSR
140900000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
141000000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
141100000000     C                   ENDDO
141200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
141300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
141400000000     C     1             DO        31            I
141500000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
141600000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
141700000000     C                   ADD       1             FUNNAT
141800000000     C                   ENDIF
141900000000     C                   ENDDO
142000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
142100000000     C     FUNNAT        IFGT      8
142200000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
142300000000     C                   ENDIF
142400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
142500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
142600000000     C                   Z-ADD     8             FUNVIS            5 0
142700000000     C     FAT(23)       IFEQ      'S'
142800000000     C                   SUB       1             FUNVIS
142900000000     C                   ENDIF
143000000000     C     FAT(24)       IFEQ      'S'
143100000000     C                   SUB       1             FUNVIS
143200000000     C                   ENDIF
143300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
143400000000     C                   Z-ADD     *ZEROS        W                 5 0
143500000000     C                   ADD       1             FUNULT
143600000000     C     FUNULT        IFGT      31
143700000000     C                   Z-ADD     1             FUNULT
143800000000     C                   ENDIF
143900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
144000000000     C     FUNULT        DO        31            I                              --- 1 -->
144100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
144200000000     C     W             IFLT      FUNVIS                                       --- 2 -->
144300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
144400000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
144500000000     C     FDE(I)        ANDNE     *BLANKS
144600000000     C     I             IFEQ      23                                           *TOLGO F23
144700000000     C     I             OREQ      24                                           *E F24
144800000000     C                   ELSE
144900000000     C                   ADD       1             W
145000000000     C                   MOVEL     FDE(I)        FUV(W)
145100000000     C                   ENDIF
145200000000     C                   ENDIF                                                  <-- 3 ---
145300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
145400000000     C                   Z-ADD     I             FUNULT
145500000000     C                   ENDIF                                                  <-- 2 ---
145600000000     C                   ENDDO                                                  <-- 1 ---
145700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
145800000000     C                   MOVEL     FUV(1)        VFD001
145900000000     C                   MOVEL     FUV(2)        VFD002
146000000000     C                   MOVEL     FUV(3)        VFD003
146100000000     C                   MOVEL     FUV(4)        VFD004
146200000000     C                   MOVEL     FUV(5)        VFD005
146300000000     C                   MOVEL     FUV(6)        VFD006
146400000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
146500000000     C     FAT(24)       ANDEQ     'N'
146600000000     C                   MOVEL     FUV(7)        VFD007
146700000000     C                   MOVEL     FUV(8)        VFD008
146800000000     C                   ELSE                                                   <-- 1 -->
146900000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
147000000000     C     FAT(24)       ANDEQ     'S'
147100000000     C                   MOVEL     FDE(23)       VFD007
147200000000     C                   MOVEL     FDE(24)       VFD008
147300000000     C                   ELSE                                                   <-- 2 -->
147400000000     C                   MOVEL     FUV(7)        VFD007
147500000000     C     FAT(23)       IFEQ      'S'
147600000000     C                   MOVEL     FDE(23)       VFD008
147700000000     C                   ELSE
147800000000     C                   MOVEL     FDE(24)       VFD008
147900000000     C                   ENDIF
148000000000     C                   ENDIF                                                  <-- 2 ---
148100000000     C                   ENDIF                                                  <-- 1 ---
148200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
148300000000     C                   ENDSR
148400000000     C*------------------------------------------------------------------------*
148500000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
148600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
148700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
148800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
148900000000     C*------------------------------------------------------------------------*
149000000000     C     CONFUN        BEGSR
149100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
149200000000     C     1             DO        31            I                              --- 1 -->
149300000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
149400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
149500000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
149600000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
149700000000     C                   EXSR      CARMSG
149800000000     C                   MOVEL     '1'           $ERR
149900000000     C                   ENDIF                                                  <-- 3 ---
150000000000     C                   ENDIF                                                  <-- 2 ---
150100000000     C                   ENDDO                                                  <-- 1 ---
150200000000     C                   ENDSR
150300000000     C*------------------------------------------------------------------------*
150400000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
150500000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
150600000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
150700000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
150800000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
150900000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
151000000000     C*------------------------------------------------------------------------*
151100000000     C     CAROPZ        BEGSR
151200000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
151300000000     C     1             DO        24            I                              --- 1 -->
151400000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
151500000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
151600000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
151700000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
151800020715     c                   If        DutLpo = 'S'
151900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
152000000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
152100000000     C                   ELSE                                                   *INTERROG.
152200000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
152300000000     C                   ENDIF
152400000000     C                   ELSE                                                   <-- 2 --> *FILIALE
152500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
152600000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
152700000000     C                   ELSE                                                   *INTERROG.
152800000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
152900000000     C                   ENDIF
153000000000     C                   ENDIF                                                  <-- 2 ---
153100000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
153200000000     C                   MOVEL     USER          WEDP              3            *PROFILO
153300000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
153400000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
153500000000     C                   ELSE
153600000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
153700000000     C                   ENDIF
153800000000     C                   ENDIF
153900000000     C                   ENDDO                                                  <-- 1 ---
154000000000     C                   ENDSR
154100000000     C*------------------------------------------------------------------------*
154200000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
154300000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
154400000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
154500000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
154600000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
154700000000     C*------------------------------------------------------------------------*
154800000000     C     OUTOPZ        BEGSR
154900000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
155000000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
155100000000     C                   ENDDO
155200000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
155300000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
155400000000     C                   ADD       1             OPZULT
155500000000     C     OPZULT        IFGT      24
155600000000     C                   Z-ADD     1             OPZULT
155700000000     C                   END
155800000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
155900000000     C     1             DO        24            I
156000000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
156100000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
156200000000     C                   ADD       1             OPZNAT
156300000000     C                   ENDIF
156400000000     C                   ENDDO
156500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
156600000000     C                   Z-ADD     0             W
156700000000     C     OPZULT        DO        24            I                              --- 1 -->
156800000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
156900000000     C     W             IFLT      8                                            --- 2 -->
157000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
157100000000     C     OAT(I)        IFEQ      'S'
157200000000     C     ODE(I)        ANDNE     *BLANKS
157300000000     C                   ADD       1             W
157400000000     C                   MOVEL     ODE(I)        OPV(W)
157500000000     C                   ENDIF
157600000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
157700000000     C                   Z-ADD     I             OPZULT
157800000000     C                   ENDIF                                                  <-- 2 ---
157900000000     C                   ENDDO                                                  <-- 1 ---
158000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
158100000000     C                   MOVEL     OPV(1)        VOD001
158200000000     C                   MOVEL     OPV(2)        VOD002
158300000000     C                   MOVEL     OPV(3)        VOD003
158400000000     C                   MOVEL     OPV(4)        VOD004
158500000000     C                   MOVEL     OPV(5)        VOD005
158600000000     C                   MOVEL     OPV(6)        VOD006
158700000000     C                   MOVEL     OPV(7)        VOD007
158800000000     C                   MOVEL     OPV(8)        VOD008
158900000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
159000000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
159100000000     C     OPZNAT        IFGT      8
159200000000     C                   MOVEL     'S'           FAT(23)
159300000000     C                   ENDIF
159400000000     C                   ENDSR
159500000000     C*------------------------------------------------------------------------*
159600000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
159700000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
159800000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
159900000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
160000000000     C*------------------------------------------------------------------------*
160100000000     C     CONOPZ        BEGSR
160200000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
160300000000     C                   MOVEL     *BLANKS       OPZOK             1
160400000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
160500000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
160600000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
160700000000     C     OAT(I)        IFEQ      'S'
160800000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
160900000000     C                   MOVEL     'S'           OPZOK
161000000000     C                   ELSE
161100000000     C                   MOVEL     'N'           OPZOK             1
161200000000     C                   ENDIF
161300000000     C                   ENDIF                                                  <-- 2 ---
161400000000     C                   ENDDO                                                  <-- 1 ---
161500000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
161600000000     C     OPZOK         IFNE      'S'
161700000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
161800000000     C                   EXSR      CARMSG
161900000000     C                   MOVEL     '1'           $ERR
162000000000     C                   MOVEL     'N'           OPZOK             1
162100000000     C                   ENDIF
162200000000     C                   ENDSR
162300000000** - MSG MESSAGGI
162400000000TNTL01R- Tasto funzionale non valido.                                          1
162500000000TNTL01R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
162600000000TNTL01R- Opzione non valida.                                                   3
162700000000TNTL01R- Opzione abilitata ma non gestita. Avvertire CED.                      4
162800000000TNTL01R- Inizio elenco.                                                        5
162900000000TNTL01R- Fine elenco.                                                          6
163000960208TNTL01R- Richiedere un solo tipo di inizio elenco                              7
163100000000TNTL01R- Data errata                                                           8
163200960423TNTL01R- Immettere S=Solo i traini filiale, N=No i traini filiale o ' '=Tutti  9
163300960930TNTL01R- Per il traino esistono riferimenti in griglia. Impossibile annullare 10
163400000000TNTL01R- 11
163500000000TNTL01R- 12
163600000000TNTL01R- 13
163700000000TNTL01R- 14
163800000000TNTL01R- 15
163900000000TNTL01R- 16
164000000000TNTL01R- 17
164100000000TNTL01R- 18
164200000000TNTL01R- Record annullato, consentito solo il ripristino                      19
164300000000TNTL01R- Record non annullato, impossibile ripristinare                       20
164400000000** - OPZ OPZIONI          SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
164500000000- 1-1 -                   -    NN     NN
164600000000- 2-2 -2=Modifica         -    SN     SN
164700000000- 3-3 -3=Duplica          -    SN     SN
164800000000- 4-4 -4=Annullamento     -    SN     SN
164900000000- 5-5 -5=Visualizzazione  -    NS     NS
165000000000- 6-6 -                   -    NN     NN
165100130529- 7-7 -7=Ripristino       -    NN     NN
165200000000- 8-8 -                   -    NN     NN
165300000000- 9-9 -                   -    NN     NN
165400000000-10-10-                   -    NN     NN
165500000000-11-11-                   -    NN     NN
165600000000-12-12-12=Crea griglia    -    SN     SN
165700000000-13-  -                   -    NN     NN
165800000000-14-  -                   -    NN     NN
165900000000-15-  -                   -    NN     NN
166000000000-16-  -                   -    NN     NN
166100000000-17-  -                   -    NN     NN
166200000000-18-  -                   -    NN     NN
166300000000-19-  -                   -    NN     NN
166400000000-20-  -                   -    NN     NN
166500000000-21-  -                   -    NN     NN
166600000000-22-  -                   -    NN     NN
166700000000-23-  -                   -    NN     NN
166800000000-24-  -                   -    NN     NN
166900000000** - FUN2 FUNZIONI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
167000000000F1    -                   -    NN     NN
167100000000F2    -                   -    NN     NN
167200000000F3    -F3=Fine            -    SS     SS
167300000000F4    -                   -    NN     NN
167400000000F5    -F5=Rivisualizza    -    SS     SS
167500000000F6    -                   -    NN     NN
167600000000F7    -                   -    NN     NN
167700000000F8    -                   -    NN     NN
167800000000F9    -                   -    NN     NN
167900000000F10   -F10=Inserimento    -    SN     SN
168000000000F11   -                   -    NN     NN
168100000000F12   -F12=Ritorno        -    SS     SS
168200000000F13   -                   -    NN     NN
168300000000F14   -                   -    NN     NN
168400000000F15   -                   -    NN     NN
168500000000F16   -                   -    NN     NN
168600000000F17   -                   -    NN     NN
168700000000F18   -                   -    NN     NN
168800000000F19   -                   -    NN     NN
168900000000F20   -                   -    NN     NN
169000000000F21   -F21=Stampa traini  -    SS     SS
169100000000F22   -                   -    NN     NN
169200000000F23   -F23=Altre opzioni  -    NN     NN
169300000000F24   -F24=Altri tasti    -    NN     NN
169400000000ENTER -                   -    SS     SS
169500000000ROLLUP-                   -    SS     SS
169600000000ROLLDN-                   -    SS     SS
169700000000HELP  -                   -    NN     NN
169800000000PRINT -                   -    NN     NN
169900000000BSPACE-                   -    NN     NN
170000000000CLEAR -                   -    NN     NN
