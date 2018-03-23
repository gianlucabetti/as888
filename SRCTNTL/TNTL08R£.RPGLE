000100000000     H*------------------------------------------------------------------------*
000200000000     H* GESTIONE TRAINI - METRICUBI ASSEGNATI - SUBFILE DI SCELTA
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL08D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL08DVS:SFLULT)
001000000000     F                                     SFILE(TL08DVM:MSGULT)
001100000000     FAZORG01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300000000     FTNTLT01L  UF A E           K DISK
001400000000     F                                     COMMIT
001500000000     FTNTLM01L  UF A E           K DISK
001600000000     F                                     COMMIT
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D*-------------------
002100000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002200000000     D*-------------------
002300000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
002400000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
002500000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
002600000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
002700000000     D*-------------------
002800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002900000000     D*-------------------
003000000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003100000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003200000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003300000000     D*-------------------
003400000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003500000000     D*-------------------
003600000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
003700000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
003800000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
003900000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
004000000000     D*-------------------
004100000000     D* TABELLA DESCRIZIONI A VIDEO
004200000000     D*-------------------
004300000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004400000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
004500000000     D*-------------------
004600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004700000000     D*-------------------
004800000000     D KEYS            S              1    DIM(40)
004900000000     D*-------------------
005000000000     D* FILIALI
005100000000     D*-------------------
005200971125     D ORG             S              3  0 DIM(500)                             *CODICE
005300971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
005400960423     D*-------------------
005500960423     D* SCHIERE ALFA/NUMERICHE DI LAVORO
005600960423     D*-------------------
005700960424     D SA0             S              1    DIM(10)                              *ALFANUMERICA 10
005800000000     D*------------------------------------------------------------------------*
005900000000     D* INPUT
006000000000     D*------------------------------------------------------------------------*
006100000000     D*---------------------
006200000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
006300000000     D*---------------------
006400000000     D DSOPZ         E DS
006500000000     D  OPZRIG                 1     40
006600000000     D*---------------------
006700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006800000000     D*---------------------
006900000000     D DSFUN         E DS
007000000000     D  FUNRIG                 1     40
007100000000     D*---------------------
007200000000     D* DS PER GESTIONE TASTI FUNZIONALI
007300000000     D*---------------------
007400000000     D DSKEYS        E DS
007500000000     D  KEYRIG                 1     40
007600000000     D*---------------------
007700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007800000000     D*---------------------
007900000000     D                SDS
008000000000     D  USER                 254    263
008100000000     D*---------------------
008200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008300000000     D*---------------------
008400000000     D DEVDS1          DS
008500000000     D  KEY                  369    369
008600000000     D  CURSOR               370    371
008700000000     D  FRTSFL               378    379B 0
008800000000     D*---------------------
008900000000     D* ARCHITETTURA
009000000000     D*---------------------
009100000000     D KPJBA         E DS
009200000000     D*---------------------
009300000000     D* DS ESTERNA DI PROCEDURA
009400000000     D*---------------------
009500000000     D DS00          E DS                  EXTNAME(TNTL00DS)
009600000000     D*---------------------
009700000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
009800000000     D*---------------------
009900000000     D DSMSG         E DS
010000000000     D*---------------------
010100000000     D* DS TIPI TRAINI
010200000000     D*---------------------
010300000000     D DSTV          E DS
010400960208     D*---------------------
010500960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
010600960208     D*---------------------
010700960208     D OG148         E DS
010800020715
010900020715     d DdatiUte      e ds
011000020715     d AzuteDs       e ds                  ExtName(Azute00f)
011100020715     d Tibs34Ds      e ds                  Inz
011200020715
011300000000     D*------------------
011400000000     D* DS CONTROLLO DATA (8)
011500000000     D*------------------
011600000000     D WLBDA8          DS                  INZ
011700000000     D  G08DAT                 1      8  0
011800000000     D  G08INV                 9     16  0
011900000000     D  G08ERR                17     17
012000000000     D  G08TGI                18     22  0
012100000000     D*------------------
012200000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
012300000000     D*------------------
012400000000     D* MENU
012500000000     D DESGES          C                   CONST('** GESTIONE **')
012600000000     D DESINT          C                   CONST('INTERROGAZIONE')
012700000000     D* COMMENTI
012800000000     D DESATB          C                   CONST('  ANNULLATO ')
012900000000     C*------------------------------------------------------------------------*
013000000000     C* MAIN LINE
013100000000     C*------------------------------------------------------------------------*
013200000000     C*
013300000000     C* CICLO FINO A CHE:
013400000000DO  1C     $FINE         DOUEQ     '1'
013500000000     C*
013600000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
013700000000     C*
013800000000     C* CONTROLLA GESTIONE VIDEO
013900000000IF  2C     $GES          IFEQ      'VS'
014000000000     C                   EXSR      GESVS                                        *SUBFILE
014100000000E   2C                   ENDIF
014200000000     C*
014300000000E   1C                   ENDDO
014400000000     C*
014500000000IF  1C     $F03          IFEQ      '1'
014600000000     C                   MOVEL     '1'           D00F03
014700000000E   1C                   ENDIF
014800000000IF  1C     $F12          IFEQ      '1'
014900000000     C                   MOVEL     '1'           D00F12
015000000000E   1C                   ENDIF
015100000000     C* FINE PGM
015200000000     C                   MOVEL     *BLANKS       D00MSG
015300000000     C                   MOVEL     DS00          KPJBU
015400000000     C                   SETON                                        LR
015500000000     C*------------------------------------------------------------------------*
015600000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
015700000000     C*------------------------------------------------------------------------*
015800000000     C     CARVS         BEGSR
015900000000     C*
016000000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
016100000000     C*
016200000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
016300000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
016400000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
016500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016700000000     C*
016800000000     C                   WRITE     TL08DVT                                      *TESTATA
016900000000     C                   WRITE     TL08DVO                                      *OPZIONI
017000000000     C                   WRITE     TL08DVF                                      *FUNZIONI
017100000000     C*
017200000000     C                   EXSR      POSSFL                                       *CARICA SFL
017300000000     C*
017400000000     C                   ENDSR
017500000000     C*------------------------------------------------------------------------*
017600000000     C* GESVS  - GESTIONE SUBFILE
017700000000     C*------------------------------------------------------------------------*
017800000000     C     GESVS         BEGSR
017900000000     C*
018000000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
018100000000     C                   WRITE     TL08DVT                                      *TESTATA
018200000000     C                   WRITE     TL08DVO                                      *OPZIONI
018300000000     C                   WRITE     TL08DVF                                      *FUNZIONI
018400000000E   1C                   ENDIF
018500000000     C*
018600000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
018700000000     C*
018800000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
018900000000     C*
019000000000IF  1C     $ERR          IFEQ      '0'
019100000000SL  2C                   SELECT
019200000000     C     KEY           WHENEQ    F10
019300000000     C                   EXSR      F10GES                                       *INSERIMENTO
019400000000     C                   OTHER
019500000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
019600000000E   2C                   ENDSL
019700000000E   1C                   ENDIF
019800000000     C*
019900000000     C                   ENDSR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* F10GES - GESTIONE TASTO FUNZIONALE F10 ==> INSERIMENTO
020200000000     C*------------------------------------------------------------------------*
020300000000     C     F10GES        BEGSR
020400000000     C*
020500000000     C* LANCIA L'INSERIMENTO
020600000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
020700000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
020800000000     C*
020900000000     C                   Z-ADD     *ZEROS        WTFP                           *PARTENZA
021000000000     C                   Z-ADD     *ZEROS        WTFA                           *SCARICO
021100000000     C                   Z-ADD     *ZEROS        WTFF                           *ARRIVO
021200000000     C*
021300000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
021400000000     C                   MOVEL     'F10'         WOP1                           *LIVELLO PROGRAMMA
021500000000     C                   MOVEL     'TNTL09R'     WPGM              8            *PGM DI GESTIONE
021600000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
021700000000     C                   MOVEL     *BLANKS       VSCOPZ
021800000000     C*
021900000000     C                   EXSR      POSSFL
022000000000     C*
022100000000     C                   ENDSR
022200000000     C*------------------------------------------------------------------------*
022300000000     C* GESOPZ - GESTIONE OPZIONI
022400000000     C*------------------------------------------------------------------------*
022500000000     C     GESOPZ        BEGSR
022600000000     C*
022700000000SL  1C                   SELECT
022800000000     C     VSCOPZ        WHENEQ    ' 2'
022900000000     C                   EXSR      O02GES                                       *MODIFICA
023000000000     C     VSCOPZ        WHENEQ    ' 4'
023100000000     C                   EXSR      O04GES                                       *ANNULLAMENTO
023200000000     C     VSCOPZ        WHENEQ    ' 7'
023300000000     C                   EXSR      O07GES                                       *RIPRISTINA
023400000000     C                   OTHER
023500000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
023600000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
023700000000E   1C                   ENDSL
023800951130     C*
023900000000     C                   ENDSR
024000000000     C*------------------------------------------------------------------------*
024100000000     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
024200000000     C*------------------------------------------------------------------------*
024300000000     C     O02GES        BEGSR
024400000000     C*
024500000000IF  1C     VSCATB        IFNE      *BLANKS
024600000000     C                   MOVEL     '1'           $ERR
024700000000     C                   MOVEL     MSG(29)       $MSG                           *ANNULLATO
024800000000     C                   EXSR      CARMSG
024900000000E   1C                   ENDIF
025000000000     C*
025100000000     C* LANCIA LA MODIFICA
025200000000IF  1C     $ERR          IFEQ      '0'
025300000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
025400000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
025500000000     C*
025600000000     C                   Z-ADD     VSCTFP        WTFP                           *PARTENZA
025700000000     C                   Z-ADD     VSCTFA        WTFA                           *SCARICO
025800000000     C                   Z-ADD     VSCTFF        WTFF                           *ARRIVO
025900000000     C*
026000000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
026100000000     C                   MOVEL     'O02'         WOP1                           *LIVELLO PROGRAMMA
026200000000     C                   MOVEL     'TNTL09R'     WPGM                           *ROUTINE MODIFICA
026300000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
026400000000     C*
026500000000E   1C                   ENDIF
026600951130     C                   MOVEL     '1'           $ERR
026700000000     C*
026800000000     C                   ENDSR
026900000000     C*------------------------------------------------------------------------*
027000000000     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
027100000000     C*------------------------------------------------------------------------*
027200000000     C     O04GES        BEGSR
027300000000     C*
027400000000IF  1C     VSCATB        IFNE      *BLANKS
027500000000     C                   MOVEL     '1'           $ERR
027600000000     C                   MOVEL     MSG(29)       $MSG                           *ANNULLATO
027700000000     C                   EXSR      CARMSG
027800000000E   1C                   ENDIF
027900000000     C*
028000000000     C* LANCIA L'ANNULLAMENTO
028100000000IF  1C     $ERR          IFEQ      '0'
028200000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
028300000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
028400000000     C*
028500000000     C                   Z-ADD     VSCTFP        WTFP                           *PARTENZA
028600000000     C                   Z-ADD     VSCTFA        WTFA                           *SCARICO
028700000000     C                   Z-ADD     VSCTFF        WTFF                           *ARRIVO
028800000000     C*
028900000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
029000000000     C                   MOVEL     'O04'         WOP1                           *LIVELLO PROGRAMMA
029100000000     C                   MOVEL     'TNTL09R'     WPGM                           *ROUTINE CANCELLAZ.N
029200000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
029300000000     C*
029400000000E   1C                   ENDIF
029500951130     C                   MOVEL     '1'           $ERR
029600000000     C*
029700000000     C                   ENDSR
029800000000     C*------------------------------------------------------------------------*
029900000000     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
030000000000     C*------------------------------------------------------------------------*
030100000000     C     O07GES        BEGSR
030200000000     C*
030300000000IF  1C     VSCATB        IFEQ      *BLANKS
030400000000     C                   MOVEL     '1'           $ERR
030500000000     C                   MOVEL     MSG(30)       $MSG                           *NON ANNULLATO
030600000000     C                   EXSR      CARMSG
030700000000E   1C                   ENDIF
030800000000     C*
030900000000     C* LANCIA IL RIPRISTINO
031000000000IF  1C     $ERR          IFEQ      '0'
031100000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
031200000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
031300000000     C*
031400000000     C                   Z-ADD     VSCTFP        WTFP                           *PARTENZA
031500000000     C                   Z-ADD     VSCTFA        WTFA                           *SCARICO
031600000000     C                   Z-ADD     VSCTFF        WTFF                           *ARRIVO
031700000000     C*
031800000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
031900000000     C                   MOVEL     'O07'         WOP1                           *LIVELLO PROGRAMMA
032000000000     C                   MOVEL     'TNTL09R'     WPGM                           *ROUTINE CANCELLAZ.N
032100000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
032200000000     C*
032300000000E   1C                   ENDIF
032400951130     C                   MOVEL     '1'           $ERR
032500000000     C*
032600000000     C                   ENDSR
032700000000     C*------------------------------------------------------------------------*
032800000000     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
032900000000     C*------------------------------------------------------------------------*
033000000000     C     NEWPOS        BEGSR
033100000000     C*
033200000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
033300000000     C*        1 = RICHIESTO POSIZIONAMENTO
033400000000     C                   Z-ADD     0             SWPOSI            7 0
033500000000     C*
033600000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
033700000000     C*            NON OCCORRE RICARICARE IL SUBFILE
033800000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
033900000000     C                   Z-ADD     0             SWMODI            1 0
034000000000     C*
034100000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLA I PARAMETRI
034200000000     C                   EXSR      CHKPAR
034300000000     C*
034400000000     C                   ENDSR
034500000000     C*------------------------------------------------------------------------*
034600000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
034700000000     C*------------------------------------------------------------------------*
034800000000     C     CHKPAR        BEGSR
034900000000     C*
035000000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
035100000000     C* CURSORE SUL CAMPO DEL CONTROL
035200000000IF  1C     $ERR          IFEQ      '1'
035300000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
035400000000X   1C                   ELSE
035500000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
035600000000E   1C                   ENDIF
035700000000     C*
035800000000     C                   Z-ADD     1             SWMODI                         *SI MODIFICA
035900000000     C*
036000000000     C                   ENDSR
036100000000     C*------------------------------------------------------------------------*
036200000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
036300000000     C*------------------------------------------------------------------------*
036400000000     C     SETFIL        BEGSR
036500000000     C*
036600000000     C                   MOVEL     '1'           *IN43                          *EMETTE SFILE
036700000000     C*
036800000000     C                   Z-ADD     D00TRN        KLMTRN
036900000000     C                   Z-ADD     D00DDE        KLMDDE
037000000000     C     KEYTLM        SETLL     TNTLM01L                           99        *FINE FILE
037100000000     C*
037200000000     C  N99              EXSR      LETFIL
037300000000     C*
037400000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
037500000000IF  1C     *IN99         IFEQ      *ON
037600000000     C                   Z-ADD     1             SFLEOF
037700000000E   1C                   ENDIF
037800000000     C*
037900000000     C                   ENDSR
038000000000     C*------------------------------------------------------------------------*
038100000000     C* LETFIL - LETTURA RECORD SUCESSIVO
038200000000     C*------------------------------------------------------------------------*
038300000000     C     LETFIL        BEGSR
038400000000     C*
038500000000     C* LEGGO FINO A:
038600000000     C*     - FINE FILE
038700000000     C*     - TROVATO RECORD VALIDO
038800000000     C                   MOVEL     'N'           WRECOK            1            *RECORD NON VALIDO
038900000000DO  1C     *IN99         DOWEQ     *OFF
039000000000     C     WRECOK        ANDEQ     'N'
039100000000     C     KEYTLM        READE(N)  TNTLM01L                               99
039200000000     C*
039300000000IF  2C     *IN99         IFEQ      *ON
039400000000     C                   Z-ADD     1             SFLEOF
039500000000X   2C                   ELSE
039600000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
039700000000E   2C                   ENDIF
039800000000     C*
039900000000E   1C                   ENDDO
040000000000     C*
040100000000     C                   ENDSR
040200000000     C*------------------------------------------------------------------------*
040300000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
040400000000     C*------------------------------------------------------------------------*
040500000000     C     CHKREC        BEGSR
040600000000     C*
040700000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
040800000000     C*
040900000000     C                   ENDSR
041000000000     C*------------------------------------------------------------------------*
041100000000     C* IRESFL - IMPOSTA I CAMPI DEL SUBFILE DAL FILE FISICO
041200000000     C*------------------------------------------------------------------------*
041300000000     C     IRESFL        BEGSR
041400000000     C*
041500000000     C                   MOVEL     *BLANKS       VSCOPZ                         *OPZIONE
041600000000     C                   MOVEL     TLMATB        VSCATB                         *ANNULLAMENTO
041700000000     C                   Z-ADD     TLMTFP        VSCTFP                         *FILIALE PARTENZA
041800000000     C                   Z-ADD     TLMTFA        VSCTFA                         *FILIALE SCARICO
041900000000     C                   Z-ADD     TLMTFF        VSCTFF                         *FILIALE ARR FINALE
042000960423     C                   Z-ADD     TLMMCA        VSCMCA                         *MCUBI NORMALI
042100960423     C                   Z-ADD     TLMMCB        VSCMCB                         *MCUBI BIS
042200960105     C                   Z-ADD     TLMMCT        VSCMCT                         *MCUBI TRAINI SEDE
042300000000     C* DECODIFICHE
042400000000     C                   MOVEL     *BLANKS       VSDTFP
042500000000     C                   SETOFF                                           28
042600000000     C                   Z-ADD     1             J
042700000000     C     VSCTFP        LOOKUP    ORG(J)                                 28
042800000000IF  1C     *IN28         IFEQ      *ON
042900000000     C                   MOVEL     DORG(J)       VSDTFP                         *FILIALE PARTENZA
043000000000E   1C                   ENDIF
043100000000     C                   MOVEL     *BLANKS       VSDTFA
043200000000     C                   SETOFF                                           28
043300000000     C                   Z-ADD     1             J
043400000000     C     VSCTFA        LOOKUP    ORG(J)                                 28
043500000000IF  1C     *IN28         IFEQ      *ON
043600000000     C                   MOVEL     DORG(J)       VSDTFA                         *FILIALE SCARICO
043700000000E   1C                   ENDIF
043800000000     C                   MOVEL     *BLANKS       VSDTFF
043900000000     C                   SETOFF                                           28
044000000000     C                   Z-ADD     1             J
044100000000     C     VSCTFF        LOOKUP    ORG(J)                                 28
044200000000IF  1C     *IN28         IFEQ      *ON
044300000000     C                   MOVEL     DORG(J)       VSDTFF                         *FILIALE ARR FINALE
044400000000E   1C                   ENDIF
044500000000     C*
044600000000     C                   ENDSR
044700000000     C*------------------------------------------------------------------------*
044800000000     C* LANPGM - LANCIA UN PROGRAMMA
044900000000     C*------------------------------------------------------------------------*
045000000000     C     LANPGM        BEGSR
045100000000     C***
045200000000     C* PARAMETRI INFORMATIVI
045300000000     C***
045400000000     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
045500000000     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PROGRAMMA
045600000000     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
045700000000     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
045800000000     C                   MOVEL     '0'           D00ERR                         *ERRORE
045900000000     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
046000000000     C***
046100000000     C* PARAMETRI
046200000000     C***
046300000000     C                   Z-ADD     WTRN          D00TRN                         *TRAINO
046400000000     C                   Z-ADD     WDDE          D00DDE                         *DECORRENZA (AA/M/G)
046500000000     C                   Z-ADD     WTFP          D00TFP                         FILIALE PARTENZA
046600000000     C                   Z-ADD     WTFA          D00TFA                         FILIALE SCARICO
046700000000     C                   Z-ADD     WTFF          D00TFF                         FILIALE ARRIVO
046800000000     C*--------------------
046900000000     C* L A N C I O
047000000000     C*--------------------
047100000000     C                   MOVEL     DS00          KPJBU
047200000000     C                   CALL      WPGM                                 99
047300000000     C                   PARM                    KPJBA
047400000000     C                   MOVEL     KPJBU         DS00
047500000000     C*
047600000000IF  1C     D00F03        IFEQ      '1'
047700000000     C                   MOVEL     '1'           $F03
047800000000     C                   MOVEL     '1'           $FINE
047900000000E   1C                   ENDIF
048000000000IF  1C     D00F12        IFEQ      '1'
048100000000     C                   MOVEL     '1'           $F12
048200000000E   1C                   ENDIF
048300000000     C*
048400000000     C                   MOVEL     D00MSG        $MSG
048500000000     C                   EXSR      CARMSG
048600000000     C*
048700000000     C                   ENDSR
048800000000     C*------------------------------------------------------------------------*
048900000000     C* IMPVID - IMPOSTAZIONI VIDEO
049000000000     C*------------------------------------------------------------------------*
049100000000     C     IMPVID        BEGSR
049200000000     C*
049300000000     C* CALCOLA LA DATA CORRENTE
049400000000     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
049500000000     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
049600000000     C                   Z-ADD     WNUM8         G08DAT
049700000000     C                   Z-ADD     *ZEROS        G08INV
049800000000     C                   MOVEL     '0'           G08ERR
049900000000     C                   CALL      'XSRDA8'
050000000000     C                   PARM                    WLBDA8
050100000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
050200000000     C* DATI TRAINO
050300000000     C                   Z-ADD     D00TRN        VCCTRN                         *TRAINO
050400000000     C                   Z-ADD     D00DDE        G08INV
050500000000     C                   Z-ADD     *ZEROS        G08DAT
050600000000     C                   MOVEL     '3'           G08ERR
050700000000     C                   CALL      'XSRDA8'
050800000000     C                   PARM                    WLBDA8
050900000000     C                   Z-ADD     G08DAT        VCCDDE                         *DATA DECORRENZA
051000000000     C* LEGGE IL TRAINO
051100000000     C                   Z-ADD     D00TRN        KLTTRN
051200000000     C                   Z-ADD     D00DDE        KLTDDE
051300000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
051400020715     c                   z-add     1             ktbkut
051500000000     C                   MOVEL     'TV'          KTBCOD
051600000000     C                   MOVEL(P)  TLTTTR        KTBKEY
051700000000     C     KEYTAB        CHAIN     TABEL00F                           99
051800000000     C                   MOVEL     TBLUNI        DSTV
051900000000     C                   MOVEL     §TVDES        VCCTTR                         *TIPO TRAINO
052000000000     C                   MOVEL     §TVDEF        DEPDEF                         *TRAINO DEFLUENZA
052100000000     C                   Z-ADD     TLTDSC        G08INV
052200000000     C                   Z-ADD     *ZEROS        G08DAT
052300000000     C                   MOVEL     '3'           G08ERR
052400000000     C                   CALL      'XSRDA8'
052500000000     C                   PARM                    WLBDA8
052600000000     C                   Z-ADD     G08DAT        VCCDSC                         *DATA SCADENEZA
052700000000     C                   Z-ADD     TLTTFP        VCCTFP                         *TERMINAL PARTENZA
052800000000     C                   Z-ADD     TLTTFA        VCCTFA                         *TERMINAL PARTENZA
052900960423     C*
053000960423     C* SE TIPO TRAINO INOLTRO METTE NELLA DESCRIZIONE IL TIPO TRAINO ASSOCIATO
053100960423IF  1C     TLTTTR        IFEQ      'I'
053200960423     C                   CLEAR                   SA0
053300960423     C                   MOVEA     'INOLTRO'     SA0
053400960423     C                   MOVEL     '('           SA0(8)
053500960424     C                   MOVEL     D00INL        SA0(9)
053600960423     C                   MOVEL     ')'           SA0(10)
053700960423     C                   MOVEA     SA0           VCCTTR
053800960423E   1C                   ENDIF
053900000000     C*
054000000000     C* EMETTE UN MESSAGGIO SE I METRICUBI SONO ANNULLATI (UNICO CASO QUANDO
054100000000     C* ARRIVA DALL'INTERROGAZIONE DI UN TRAINO ANNULLATO)
054200000000     C                   MOVEL     *BLANKS       VCCATB                         *RECORD NON ANNULLAT
054300000000     C                   MOVEL     '0'           *IN48                          *NON VISUALIZZATO
054400000000IF  1C     TLTATB        IFNE      *BLANKS
054500000000     C                   MOVEL     DESATB        VCCATB                         *RECORD ANNULLATO
054600000000     C                   MOVEL     '1'           *IN48                          *VISUALIZZATO
054700000000E   1C                   ENDIF
054800000000     C*
054900000000     C                   ENDSR
055000000000     C*------------------------------------------------------------------------*
055100000000     C* CARTAB - CARICA LE TABELLE
055200000000     C*------------------------------------------------------------------------*
055300000000     C     CARTAB        BEGSR
055400000000     C*--------------------
055500000000     C* FILIALI
055600000000     C*--------------------
055700000000     C                   Z-ADD     *ZEROS        J                 4 0
055800000000     C     *LOVAL        SETLL     AZORG01L
055900000000     C                   READ      AZORG01L                               99
056000000000DO  1C     *IN99         DOWEQ     *OFF
056100000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
056200960208     C                   MOVEL     ORGDE8        OG148
056300960208IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
056400000000     C                   ADD       1             J
056500000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
056600000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
056700960208E   3C                   ENDIF
056800960208E   2C                   ENDIF
056900000000     C                   READ      AZORG01L                               99
057000000000E   1C                   ENDDO
057100000000     C*
057200000000     C                   ENDSR
057300000000     C*------------------------------------------------------------------------*
057400000000     C* *INZSR - ROUTINE INIZIALE
057500000000     C*------------------------------------------------------------------------*
057600000000     C     *INZSR        BEGSR
057700000000     C*--------------------
057800000000     C* RICEVIMENTO PARAMETRI
057900000000     C*--------------------
058000000000     C     *ENTRY        PLIST
058100000000     C                   PARM                    KPJBA
058200000000     C                   MOVEL     KPJBU         DS00
058300000000     C*--------------------
058400000000     C* VARIABILI RIFERITE AL DATA BASE
058500000000     C*--------------------
058600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
058700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
058800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
058900000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
059000000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
059100000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
059200000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
059300000000     C     *LIKE         DEFINE    D00OP0        WOP0                           *DS00
059400000000     C     *LIKE         DEFINE    D00OP1        WOP1
059500000000     C     *LIKE         DEFINE    D00F03        WF03
059600000000     C     *LIKE         DEFINE    D00F12        WF12
059700000000     C     *LIKE         DEFINE    D00ERR        WERR
059800000000     C     *LIKE         DEFINE    D00MSG        WMSG
059900000000     C     *LIKE         DEFINE    D00TRN        WTRN
060000000000     C     *LIKE         DEFINE    D00DDE        WDDE
060100000000     C     *LIKE         DEFINE    D00TFP        WTFP
060200000000     C     *LIKE         DEFINE    D00TFA        WTFA
060300000000     C     *LIKE         DEFINE    D00TFF        WTFF
060400000000     C     *LIKE         DEFINE    §TVDEF        DEPDEF                         *DEPOSITO
060500000000     C*--------------------
060600000000     C* CHIAVI DI LETTURA
060700000000     C*--------------------
060800000000     C* LETTURA TABEL00F - COMPLETA
060900000000     C     KEYTAB        KLIST
061000000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
061100000000     C                   KFLD                    KTBCOD                         *TABELLA
061200000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
061300000000     C* LETTURA TNTLT01L - COMPLETA
061400000000     C     KEYTLT        KLIST
061500000000     C                   KFLD                    KLTTRN                         *TRAINO
061600000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
061700000000     C* LETTURA TNTLM01L - PARZIALE
061800000000     C     KEYTLM        KLIST
061900000000     C                   KFLD                    KLMTRN                         *TRAINO
062000000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
062100000000     C*--------------------
062200000000     C* VARIABILI DI CONTROLLO
062300000000     C*--------------------
062400000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
062500000000     C***
062600000000     C* IMPOSTAZIONI VIDEO
062700000000     C***
062800000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
062900000000     C                   MOVE      D00OP0        INTGES            2
063000000000IF  1C     INTGES        IFEQ      '02'
063100000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
063200000000     C                   MOVEL     DESGES        VTDOPE                         *DESCR OPERAZIONE
063300000000X   1C                   ELSE
063400000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
063500000000     C                   MOVEL     DESINT        VTDOPE                         *DESCR OPERAZIONE
063600000000E   1C                   ENDIF
063700000000     C*
063800000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
063900000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
064000000000IF  1C     *IN99         IFEQ      *ON
064100000000     C     *IN99         ANDEQ     *OFF
064200000000     C* TRAINI
064300000000     C                   UPDATE    TNTLT000
064400000000     C                   WRITE     TNTLT000
064500000000     C* METRICUBI
064600000000     C                   UPDATE    TNTLM000
064700000000     C                   WRITE     TNTLM000
064800000000E   1C                   ENDIF
064900000000     C*
065000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
065100000000     C                   EXSR      INZSTD
065200000000     C*
065300000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
065400000000     C                   EXSR      INZSFL
065500000000     C*
065600000000     C* IMPOSTAZIONE INIZIALE VIDEO
065700000000     C                   EXSR      IMPVID
065800000000     C*
065900000000     C* CARICA LE TABELLE
066000000000     C                   EXSR      CARTAB
066100000000     C*
066200000000     C* EMETTE VIDEATA SUBFILE
066300000000     C                   EXSR      CHKPAR
066400000000     C                   EXSR      CARVS
066500000000     C*
066600000000     C                   ENDSR
066700000000     C*------------------------------------------------------------------------*
066800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067000000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
067100000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
067200000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
067300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067500000000     C*------------------------------------------------------------------------*
067600000000     C* INZSTD - ROUTINE INIZIALE STANDARD
067700000000     C*------------------------------------------------------------------------*
067800000000     C     INZSTD        BEGSR
067900000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
068000000000     C                   CALL      'XKEYS'
068100000000     C                   PARM                    DSKEYS
068200000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
068300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
068400000000     C     XMSGPA        PLIST
068500000000     C                   PARM                    DSMSMS
068600000000     C                   PARM                    DSMSPG
068700000000     C                   PARM                    DSMSKE
068800000000     C                   PARM                    DSMSER
068900000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
069000000000     C                   MOVEL     'TNTL08R'     DSMSPG                         *NOME PGM CHIAMANTE
069100000000     C                   MOVEL     'TNTL08R'     VMCQUE                         *CODA MESSAGGI DESTI
069200000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
069300000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
069400000000     C* INIZIALIZZO FLAG CONTROLLO PGM
069500000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
069600000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
069700000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F03
069800000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
069900000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
070000000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
070100000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
070200000000     C* LANCIO ROUTINE PER VERIFICARE SE
070300000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
070400000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
070500020715
070600020715     c     *dtaara       define    §azute        azuteds
070700020715     c     *dtaara       define    §datiute      ddatiute
070800020715
070900020715     c                   in(E)     *dtaara
071000020715     c                   if        %Error or rsut = *blanks
071100020715     c                   clear                   tibs34ds
071200020715     c                   call      'TIBS34R'
071300020715     c                   parm                    Tibs34Ds
071400020715     c                   in        *dtaara
071500020715     c                   endif
071600000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
071700000000     C                   BITOFF    '01234567'    POSMI1            1
071800000000     C                   BITOFF    '01234567'    POSMI2            1
071900000000     C                   BITOFF    '01234567'    POSMA1            1
072000000000     C                   BITOFF    '01234567'    POSMA2            1
072100000000     C                   BITON     '34'          POSMI1            1            *18
072200000000     C                   BITON     '7'           POSMI2            1            *01
072300000000     C                   BITON     '34'          POSMA1            1            *18
072400000000     C                   BITON     '0'           POSMA2            1            *80
072500000000     C                   MOVE      POSMI2        POSMIN            2
072600000000     C                   MOVEL     POSMI1        POSMIN
072700000000     C                   MOVE      POSMA2        POSMAX            2
072800000000     C                   MOVEL     POSMA1        POSMAX
072900000000     C                   ENDSR
073000000000     C*------------------------------------------------------------------------*
073100000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
073200000000     C*------------------------------------------------------------------------*
073300000000     C     FUNSTD        BEGSR
073400000000     C                   SELECT
073500000000     C     KEY           WHENEQ    F3
073600000000     C                   EXSR      F03STD
073700000000     C     KEY           WHENEQ    F12
073800000000     C                   EXSR      F12STD
073900000000     C     KEY           WHENEQ    F24
074000000000     C                   EXSR      F24STD
074100000000     C                   OTHER
074200000000     C                   MOVEL     MSG(2)        $MSG
074300000000     C                   EXSR      CARMSG
074400000000     C                   ENDSL
074500000000     C                   ENDSR
074600000000     C*------------------------------------------------------------------------*
074700000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
074800000000     C*------------------------------------------------------------------------*
074900000000     C     F03STD        BEGSR
075000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
075100000000     C                   MOVEL     '1'           $F03
075200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
075300000000     C                   MOVEL     '1'           $FINE
075400951212     C* PULISCE IL MESSAGGIO
075500951212     C                   MOVEL     *BLANKS       D00MSG
075600000000     C                   ENDSR
075700000000     C*------------------------------------------------------------------------*
075800000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
075900000000     C*------------------------------------------------------------------------*
076000000000     C     F12STD        BEGSR
076100000000     C*
076200000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
076300000000     C                   MOVEL     '1'           $F12
076400000000     C* IMPOSTO FLAG DI FIN PGM
076500000000     C                   MOVEL     '1'           $FINE
076600951212     C* PULISCE IL MESSAGGIO
076700951212     C                   MOVEL     *BLANKS       D00MSG
076800000000     C                   ENDSR
076900000000     C*------------------------------------------------------------------------*
077000000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
077100000000     C*------------------------------------------------------------------------*
077200000000     C     F24STD        BEGSR
077300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
077400000000     C                   EXSR      OUTFUN
077500000000     C                   ENDSR
077600000000     C*------------------------------------------------------------------------*
077700000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
077800000000     C*------------------------------------------------------------------------*
077900000000     C     INZSFL        BEGSR
078000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
078100000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
078200000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
078300000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
078400000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
078500000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
078600000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
078700000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
078800000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
078900000000     C                   Z-ADD     09            SFLPAG
079000000000     C                   ENDSR
079100000000     C*------------------------------------------------------------------------*
079200000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
079300000000     C*------------------------------------------------------------------------*
079400000000     C     FUNSFL        BEGSR
079500000000     C                   SELECT
079600000000     C     KEY           WHENEQ    ENTER
079700000000     C                   EXSR      ENTSFL
079800000000     C     KEY           WHENEQ    F3
079900000000     C                   EXSR      F03STD
080000000000     C     KEY           WHENEQ    F5
080100000000     C                   EXSR      F05STD
080200000000     C     KEY           WHENEQ    F12
080300000000     C                   EXSR      F12STD
080400000000     C     KEY           WHENEQ    F17
080500000000     C                   EXSR      F17STD
080600000000     C     KEY           WHENEQ    F18
080700000000     C                   EXSR      F18STD
080800000000     C     KEY           WHENEQ    F23
080900000000     C                   EXSR      F23STD
081000000000     C     KEY           WHENEQ    F24
081100000000     C                   EXSR      F24STD
081200000000     C     KEY           WHENEQ    ROLLUP
081300000000     C                   EXSR      RUPSTD
081400000000     C     KEY           WHENEQ    ROLLDN
081500000000     C                   EXSR      RDNSTD
081600000000     C                   OTHER
081700000000     C                   MOVEL     MSG(2)        $MSG
081800000000     C                   EXSR      CARMSG
081900000000     C                   ENDSL
082000000000     C                   ENDSR
082100000000     C*------------------------------------------------------------------------*
082200000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
082300000000     C*------------------------------------------------------------------------*
082400000000     C     F05STD        BEGSR
082500000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
082600000000     C                   EXSR      POSSFL
082700000000     C                   ENDSR
082800000000     C*------------------------------------------------------------------------*
082900000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
083000000000     C*------------------------------------------------------------------------*
083100000000     C     F17STD        BEGSR
083200000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
083300000000     C                   Z-ADD     1             VCCNBR
083400000000     C                   ENDSR
083500000000     C*------------------------------------------------------------------------*
083600000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
083700000000     C*------------------------------------------------------------------------*
083800000000     C     F18STD        BEGSR
083900000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
084000000000     C                   Z-ADD     SFLULT        VCCNBR
084100000000     C                   ENDSR
084200000000     C*------------------------------------------------------------------------*
084300000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
084400000000     C*------------------------------------------------------------------------*
084500000000     C     F23STD        BEGSR
084600000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
084700000000     C                   EXSR      OUTOPZ
084800000000     C                   ENDSR
084900000000     C*------------------------------------------------------------------------*
085000000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
085100000000     C*------------------------------------------------------------------------*
085200000000     C     RUPSTD        BEGSR
085300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
085400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
085500000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
085600000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
085700000000     C                   ELSE
085800000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
085900000000     C                   ENDIF
086000000000     C                   ENDSR
086100000000     C*------------------------------------------------------------------------*
086200000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
086300000000     C*------------------------------------------------------------------------*
086400000000     C     RDNSTD        BEGSR
086500000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
086600000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
086700000000     C     CURSOR        IFGE      POSMIN
086800000000     C     CURSOR        ANDLE     POSMAX
086900000000     C                   ELSE
087000000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
087100000000     C                   MOVEL     '1'           $ERR
087200000000     C                   MOVEL     MSG(5)        $MSG
087300000000     C                   EXSR      CARMSG
087400000000     C                   ENDIF
087500000000     C                   ENDSR
087600000000     C*------------------------------------------------------------------------*
087700000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
087800000000     C*------------------------------------------------------------------------*
087900000000     C     ENTSFL        BEGSR
088000000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
088100000000     C                   EXSR      NEWPOS
088200000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
088300000000     C     $ERR          IFEQ      '0'                                          --- 1 -->
088400000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
088500000000     C     SWPOSI        IFNE      0                                            --- 2 -->
088600000000     C                   EXSR      POSSFL
088700000000     C                   ELSE                                                   <-- 2 -->
088800000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
088900000000     C     SFLULT        IFNE      0                                            --- 3 -->
089000000000     C                   EXSR      OPZSFL
089100000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
089200000000     C     SWMODI        IFNE      0                                            --- 4 -->
089300000000     C                   EXSR      POSSFL
089400000000     C                   ENDIF                                                  <-- 4 ---
089500000000     C                   ENDIF                                                  <-- 3 ---
089600000000     C                   ENDIF                                                  <-- 2 ---
089700000000     C                   ENDIF                                                  <-- 1 ---
089800000000     C                   ENDSR
089900000000     C*------------------------------------------------------------------------*
090000000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
090100000000     C*------------------------------------------------------------------------*
090200000000     C     POSSFL        BEGSR
090300000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
090400000000     C                   EXSR      CLRSFL
090500000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
090600000000     C                   EXSR      SETFIL
090700000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
090800000000     C     SFLEOF        IFEQ      0
090900000000     C                   EXSR      CARSFL
091000000000     C                   ENDIF
091100000000     C                   ENDSR
091200000000     C*------------------------------------------------------------------------*
091300000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
091400000000     C*------------------------------------------------------------------------*
091500000000     C     OPZSFL        BEGSR
091600000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
091700000000     C                   MOVEL     'S'           OPZOK             1
091800000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
091900000000     C                   Z-ADD     0             SFLVAR            5 0
092000000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
092100000000     C                   Z-ADD     0             SFLERR
092200000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
092300000000     C                   Z-ADD     SFLULT        WFLULT
092400000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
092500000000     C                   READC     TL08DVS                                49
092600000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
092700000000     C     $ERR          ANDEQ     '0'
092800000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
092900000000     C                   ADD       1             SFLVAR
093000000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
093100000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
093200000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
093300000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
093400000000     C                   EXSR      CONOPZ
093500000000     C     OPZOK         IFEQ      'S'
093600000000     C     $ERR          ANDEQ     '0'
093700000000     C                   EXSR      GESOPZ
093800000000     C                   ENDIF
093900000000     C                   ENDIF                                                  <-- 3 ---
094000000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
094100000000     C*            2. PULIZIA SCELTA IN SFL
094200000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
094300000000     C                   MOVEL     '0'           *IN41
094400000000     C                   MOVEL     *BLANKS       VSCOPZ
094500000000     C                   ELSE                                                   <-- 3 -->
094600000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
094700000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
094800000000     C                   MOVEL     '1'           *IN41
094900000000     C                   Z-ADD     SFLULT        SFLERR
095000000000     C                   ENDIF                                                  <-- 3 ---
095100000000     C                   ELSE                                                   <-- 2 -->
095200000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
095300000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
095400000000     C                   MOVEL     '1'           *IN41
095500000000     C                   ENDIF                                                  <-- 2 ---
095600000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
095700000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
095800000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
095900000000     C     OPZOK         IFNE      'S'
096000000000     C                   Z-ADD     SFLERR        VCCNBR
096100000000     C                   ELSE
096200000000     C                   Z-ADD     SFLULT        VCCNBR
096300000000     C                   ENDIF
096400000000     C* AGGIORNO RECORD SFL
096500000000     C                   UPDATE    TL08DVS
096600000000     C     $ERR          IFEQ      '0'
096700000000     C                   READC     TL08DVS                                49
096800000000     C                   ENDIF
096900000000     C                   ENDDO                                                  <-- 1 ---
097000000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
097100000000     C                   Z-ADD     WFLULT        SFLULT
097200000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
097300000000     C     SFLVAR        IFEQ      0
097400000000     C                   MOVEL     '1'           $FINE
097500000000     C                   ENDIF
097600000000     C                   ENDSR
097700000000     C*------------------------------------------------------------------------*
097800000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
097900000000     C*------------------------------------------------------------------------*
098000000000     C     CLRSFL        BEGSR
098100000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
098200000000     C* (0=NON FINE FILE  1=FINE FILE)
098300000000     C                   Z-ADD     0             SFLEOF
098400000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
098500000000     C                   Z-ADD     0             SFLULT
098600000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
098700000000     C                   MOVEL     '1'           *IN40
098800000000     C                   WRITE     TL08DVC
098900000000     C                   MOVEL     '0'           *IN40
099000000000     C                   ENDSR
099100000000     C*------------------------------------------------------------------------*
099200000000     C* CARSFL - CARICAMENTO SFL
099300000000     C*------------------------------------------------------------------------*
099400000000     C     CARSFL        BEGSR
099500000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
099600000000     C     SFLULT        IFEQ      0
099700000000     C                   EXSR      SETFIL
099800000000     C                   ENDIF
099900000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
100000000000     C                   Z-ADD     SFLULT        WFLULT
100100000000     C* CICLO:
100200000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
100300000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
100400000000     C                   Z-ADD     1             SFLRIG
100500000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
100600000000     C     SFLEOF        ANDEQ     0
100700000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
100800000000     C                   MOVEL     *BLANKS       VSCOPZ
100900000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
101000000000     C                   EXSR      IRESFL
101100000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
101200000000     C                   ADD       1             SFLULT
101300000000     C* SCRITTURA RECORD SFL
101400000000     C                   WRITE     TL08DVS
101500000000     C                   ADD       1             SFLRIG
101600000000     C* LETTURA FILE DA CARICARE
101700000000     C                   EXSR      LETFIL
101800000000     C                   ENDDO                                                  <-- 1 ---
101900000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
102000000000     C     WFLULT        IFNE      SFLULT
102100000000     C                   Z-ADD     WFLULT        VCCNBR
102200000000     C                   ADD       1             VCCNBR
102300000000     C                   ENDIF
102400000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
102500000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
102600000000     C     SFLULT        ANDGT     *ZEROS
102700000000     C                   MOVEL     '1'           $ERR
102800000000     C                   MOVEL     MSG(6)        $MSG
102900000000     C                   EXSR      CARMSG
103000000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
103100000000     C     SFLULT        CHAIN     TL08DVS                            99
103200000000     C     *IN99         IFEQ      '0'
103300000000     C                   SETON                                        45
103400000000     C                   UPDATE    TL08DVS
103500000000     C                   SETOFF                                       45
103600000000     C                   ENDIF
103700000000     C                   ENDIF                                                  <-- 1 ---
103800000000     C                   ENDSR
103900000000     C*------------------------------------------------------------------------*
104000000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
104100000000     C*------------------------------------------------------------------------*
104200000000     C     OUTSFL        BEGSR
104300000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
104400000000     C     MSGULT        IFNE      *ZEROS
104500000000     C                   EXSR      OUTMSG
104600000000     C                   ENDIF
104700000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
104800000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
104900000000     C     SFLEOF        IFEQ      0
105000000000     C                   MOVEL     '0'           *IN42
105100000000     C                   ELSE
105200000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
105300000000     C                   MOVEL     '1'           *IN42
105400000000     C                   ENDIF
105500000000     C* EMISSIONE SUBFILE
105600000000     C     SFLULT        IFLE      0
105700000000     C*  --> CASO DI SFL VUOTO
105800000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
105900000000     C                   WRITE     TL08DVV                                      *'NON VI SONO DATI'
106000000000     C                   WRITE     TL08DVC                                      *SFL CONTROL
106100000000     C                   READ      TL08DVC                                99    *SFL CONTROL
106200000000     C                   ELSE
106300000000     C*  --> CASO DI SFL CON RECORDS
106400000000     C                   WRITE     TL08DVC                                      *SFL CONTROL
106500000000     C                   READ      TL08DVC                                99    *SFL CONTROL
106600000000     C                   ENDIF
106700000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
106800000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
106900000000     C                   Z-ADD     FRTSFL        VCCNBR
107000000000     C                   ENDSR
107100000000     C*------------------------------------------------------------------------*
107200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
107300000000     C*          CODA MESSAGGI DEL PGM
107400000000     C*------------------------------------------------------------------------*
107500000000     C     CARMSG        BEGSR
107600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
107700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
107800000000     C     MSGULT        IFEQ      0
107900000000     C                   MOVEL     '1'           *IN40
108000000000     C                   WRITE     TL08DVK
108100000000     C                   MOVEL     '0'           *IN40
108200000000     C                   ENDIF
108300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
108400000000     C                   MOVEL     $MSG          DSMSMS
108500000000     C                   CALL      'XMSG'        XMSGPA
108600000000     C     DSMSER        IFEQ      0
108700000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
108800000000     C                   ENDIF
108900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
109000000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
109100000000     C                   WRITE     TL08DVM                                      *SFL MESS
109200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
109300000000     C                   MOVEL     *BLANKS       $MSG
109400000000     C                   ENDSR
109500000000     C*------------------------------------------------------------------------*
109600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
109700000000     C*------------------------------------------------------------------------*
109800000000     C     OUTMSG        BEGSR
109900000000     C* VISUALIZZAZIONE MESSAGGIO
110000000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
110100000000     C                   WRITE     TL08DVK                                      *CONTR MESS
110200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
110300000000     C                   Z-ADD     *ZEROS        MSGULT
110400000000     C                   ENDSR
110500000000     C*------------------------------------------------------------------------*
110600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
110700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
110800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
110900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
111000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
111100000000     C*------------------------------------------------------------------------*
111200000000     C     CARFUN        BEGSR
111300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
111400000000     C     1             DO        31            I                 5 0          --- 1 -->
111500000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
111600000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
111700000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
111800020715     c                   If        DutLpo = 'S'
111900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
112000000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
112100000000     C                   ELSE                                                   *INTERROG.
112200000000     C                   MOVEL     FUNASI        FAT(I)
112300000000     C                   ENDIF
112400000000     C                   ELSE                                                   <-- 2 --> *FILIALE
112500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
112600000000     C                   MOVEL     FUNAFG        FAT(I)
112700000000     C                   ELSE                                                   *INTERROG.
112800000000     C                   MOVEL     FUNAFI        FAT(I)
112900000000     C                   ENDIF
113000000000     C                   ENDIF                                                  <-- 2 ---
113100000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
113200000000     C                   MOVEL     USER          WEDP              3            *PROFILO
113300000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
113400000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
113500000000     C                   ELSE
113600000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
113700000000     C                   ENDIF
113800000000     C                   ENDIF
113900000000     C                   ENDDO                                                  <-- 1 ---
114000000000     C                   ENDSR
114100000000     C*------------------------------------------------------------------------*
114200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
114300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
114400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
114500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
114600000000     C*------------------------------------------------------------------------*
114700000000     C     OUTFUN        BEGSR
114800000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
114900000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
115000000000     C                   ENDDO
115100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
115200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
115300000000     C     1             DO        31            I
115400000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
115500000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
115600000000     C                   ADD       1             FUNNAT
115700000000     C                   ENDIF
115800000000     C                   ENDDO
115900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
116000000000     C     FUNNAT        IFGT      8
116100000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
116200000000     C                   ENDIF
116300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
116400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
116500000000     C                   Z-ADD     8             FUNVIS            5 0
116600000000     C     FAT(23)       IFEQ      'S'
116700000000     C                   SUB       1             FUNVIS
116800000000     C                   ENDIF
116900000000     C     FAT(24)       IFEQ      'S'
117000000000     C                   SUB       1             FUNVIS
117100000000     C                   ENDIF
117200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
117300000000     C                   Z-ADD     *ZEROS        W                 5 0
117400000000     C                   ADD       1             FUNULT
117500000000     C     FUNULT        IFGT      31
117600000000     C                   Z-ADD     1             FUNULT
117700000000     C                   ENDIF
117800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
117900000000     C     FUNULT        DO        31            I                              --- 1 -->
118000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
118100000000     C     W             IFLT      FUNVIS                                       --- 2 -->
118200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
118300000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
118400000000     C     FDE(I)        ANDNE     *BLANKS
118500000000     C     I             IFEQ      23                                           *TOLGO F23
118600000000     C     I             OREQ      24                                           *E F24
118700000000     C                   ELSE
118800000000     C                   ADD       1             W
118900000000     C                   MOVEL     FDE(I)        FUV(W)
119000000000     C                   ENDIF
119100000000     C                   ENDIF                                                  <-- 3 ---
119200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
119300000000     C                   Z-ADD     I             FUNULT
119400000000     C                   ENDIF                                                  <-- 2 ---
119500000000     C                   ENDDO                                                  <-- 1 ---
119600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
119700000000     C                   MOVEL     FUV(1)        VFD001
119800000000     C                   MOVEL     FUV(2)        VFD002
119900000000     C                   MOVEL     FUV(3)        VFD003
120000000000     C                   MOVEL     FUV(4)        VFD004
120100000000     C                   MOVEL     FUV(5)        VFD005
120200000000     C                   MOVEL     FUV(6)        VFD006
120300000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
120400000000     C     FAT(24)       ANDEQ     'N'
120500000000     C                   MOVEL     FUV(7)        VFD007
120600000000     C                   MOVEL     FUV(8)        VFD008
120700000000     C                   ELSE                                                   <-- 1 -->
120800000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
120900000000     C     FAT(24)       ANDEQ     'S'
121000000000     C                   MOVEL     FDE(23)       VFD007
121100000000     C                   MOVEL     FDE(24)       VFD008
121200000000     C                   ELSE                                                   <-- 2 -->
121300000000     C                   MOVEL     FUV(7)        VFD007
121400000000     C     FAT(23)       IFEQ      'S'
121500000000     C                   MOVEL     FDE(23)       VFD008
121600000000     C                   ELSE
121700000000     C                   MOVEL     FDE(24)       VFD008
121800000000     C                   ENDIF
121900000000     C                   ENDIF                                                  <-- 2 ---
122000000000     C                   ENDIF                                                  <-- 1 ---
122100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
122200000000     C                   ENDSR
122300000000     C*------------------------------------------------------------------------*
122400000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
122500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
122600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
122700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
122800000000     C*------------------------------------------------------------------------*
122900000000     C     CONFUN        BEGSR
123000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
123100000000     C     1             DO        31            I                              --- 1 -->
123200000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
123300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
123400000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
123500000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
123600000000     C                   EXSR      CARMSG
123700000000     C                   MOVEL     '1'           $ERR
123800000000     C                   ENDIF                                                  <-- 3 ---
123900000000     C                   ENDIF                                                  <-- 2 ---
124000000000     C                   ENDDO                                                  <-- 1 ---
124100000000     C                   ENDSR
124200000000     C*------------------------------------------------------------------------*
124300000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
124400000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
124500000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
124600000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
124700000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
124800000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
124900000000     C*------------------------------------------------------------------------*
125000000000     C     CAROPZ        BEGSR
125100000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
125200000000     C     1             DO        24            I                              --- 1 -->
125300000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
125400000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
125500000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
125600000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
125700020715     c                   If        DutLpo = 'S'
125800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
125900000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
126000000000     C                   ELSE                                                   *INTERROG.
126100000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
126200000000     C                   ENDIF
126300000000     C                   ELSE                                                   <-- 2 --> *FILIALE
126400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
126500000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
126600000000     C                   ELSE                                                   *INTERROG.
126700000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
126800000000     C                   ENDIF
126900000000     C                   ENDIF                                                  <-- 2 ---
127000000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
127100000000     C                   MOVEL     USER          WEDP              3            *PROFILO
127200000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
127300000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
127400000000     C                   ELSE
127500000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
127600000000     C                   ENDIF
127700000000     C                   ENDIF
127800000000     C                   ENDDO                                                  <-- 1 ---
127900000000     C                   ENDSR
128000000000     C*------------------------------------------------------------------------*
128100000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
128200000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
128300000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
128400000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
128500000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
128600000000     C*------------------------------------------------------------------------*
128700000000     C     OUTOPZ        BEGSR
128800000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
128900000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
129000000000     C                   ENDDO
129100000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
129200000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
129300000000     C                   ADD       1             OPZULT
129400000000     C     OPZULT        IFGT      24
129500000000     C                   Z-ADD     1             OPZULT
129600000000     C                   END
129700000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
129800000000     C     1             DO        24            I
129900000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
130000000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
130100000000     C                   ADD       1             OPZNAT
130200000000     C                   ENDIF
130300000000     C                   ENDDO
130400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
130500000000     C                   Z-ADD     0             W
130600000000     C     OPZULT        DO        24            I                              --- 1 -->
130700000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
130800000000     C     W             IFLT      8                                            --- 2 -->
130900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
131000000000     C     OAT(I)        IFEQ      'S'
131100000000     C     ODE(I)        ANDNE     *BLANKS
131200000000     C                   ADD       1             W
131300000000     C                   MOVEL     ODE(I)        OPV(W)
131400000000     C                   ENDIF
131500000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
131600000000     C                   Z-ADD     I             OPZULT
131700000000     C                   ENDIF                                                  <-- 2 ---
131800000000     C                   ENDDO                                                  <-- 1 ---
131900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
132000000000     C                   MOVEL     OPV(1)        VOD001
132100000000     C                   MOVEL     OPV(2)        VOD002
132200000000     C                   MOVEL     OPV(3)        VOD003
132300000000     C                   MOVEL     OPV(4)        VOD004
132400000000     C                   MOVEL     OPV(5)        VOD005
132500000000     C                   MOVEL     OPV(6)        VOD006
132600000000     C                   MOVEL     OPV(7)        VOD007
132700000000     C                   MOVEL     OPV(8)        VOD008
132800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
132900000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
133000000000     C     OPZNAT        IFGT      8
133100000000     C                   MOVEL     'S'           FAT(23)
133200000000     C                   ENDIF
133300000000     C                   ENDSR
133400000000     C*------------------------------------------------------------------------*
133500000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
133600000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
133700000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
133800000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
133900000000     C*------------------------------------------------------------------------*
134000000000     C     CONOPZ        BEGSR
134100000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
134200000000     C                   MOVEL     *BLANKS       OPZOK             1
134300000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
134400000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
134500000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
134600000000     C     OAT(I)        IFEQ      'S'
134700000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
134800000000     C                   MOVEL     'S'           OPZOK
134900000000     C                   ELSE
135000000000     C                   MOVEL     'N'           OPZOK             1
135100000000     C                   ENDIF
135200000000     C                   ENDIF                                                  <-- 2 ---
135300000000     C                   ENDDO                                                  <-- 1 ---
135400000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
135500000000     C     OPZOK         IFNE      'S'
135600000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
135700000000     C                   EXSR      CARMSG
135800000000     C                   MOVEL     '1'           $ERR
135900000000     C                   MOVEL     'N'           OPZOK             1
136000000000     C                   ENDIF
136100000000     C                   ENDSR
136200000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
136300000000TNTL08R- Tasto funzionale non valido.                                          1
136400000000TNTL08R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
136500000000TNTL08R- Opzione non valida.                                                   3
136600000000TNTL08R- Opzione abilitata ma non gestita. Avvertire CED.                      4
136700000000TNTL08R- Inizio elenco.                                                        5
136800000000TNTL08R- Fine elenco.                                                          6
136900000000TNTL08R- 7
137000000000TNTL08R- 8
137100000000TNTL08R- 9
137200000000TNTL08R- 10
137300000000TNTL08R- 11
137400000000TNTL08R- 12
137500000000TNTL08R- 13
137600000000TNTL08R- 14
137700000000TNTL08R- 15
137800000000TNTL08R- 16
137900000000TNTL08R- 17
138000000000TNTL08R- 18
138100000000TNTL08R- 19
138200000000TNTL08R- 20
138300000000TNTL08R- 21
138400000000TNTL08R- 22
138500000000TNTL08R- 23
138600000000TNTL08R- 24
138700000000TNTL08R- 25
138800000000TNTL08R- 26
138900000000TNTL08R- 27
139000000000TNTL08R- 28
139100000000TNTL08R- Record annullato, consentito solo il ripristino                      29
139200000000TNTL08R- Record non annullato, impossibile ripristinare                       30
139300000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
139400000000- 1-1 -                   -    NN     NN
139500000000- 2-2 -2=Modifica         -    SN     SN
139600000000- 3-3 -                   -    NN     NN
139700000000- 4-4 -4=Annullamento     -    SN     SN
139800000000- 5-5 -                   -    NN     NN
139900000000- 6-6 -                   -    NN     NN
140000000000- 7-7 -7=Ripristino       -    SN     SN
140100000000- 8-8 -                   -    NN     NN
140200000000- 9-9 -                   -    NN     NN
140300000000-10-  -                   -    NN     NN
140400000000-11-  -                   -    NN     NN
140500000000-12-  -                   -    NN     NN
140600000000-13-  -                   -    NN     NN
140700000000-14-  -                   -    NN     NN
140800000000-15-  -                   -    NN     NN
140900000000-16-  -                   -    NN     NN
141000000000-17-  -                   -    NN     NN
141100000000-18-  -                   -    NN     NN
141200000000-19-  -                   -    NN     NN
141300000000-20-  -                   -    NN     NN
141400000000-21-  -                   -    NN     NN
141500000000-22-  -                   -    NN     NN
141600000000-23-  -                   -    NN     NN
141700000000-24-  -                   -    NN     NN
141800000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
141900000000F1    -                   -    NN     NN
142000000000F2    -                   -    NN     NN
142100000000F3    -F3=Fine            -    NN     NN
142200000000F4    -                   -    NN     NN
142300000000F5    -                   -    NN     NN
142400000000F6    -                   -    NN     NN
142500000000F7    -                   -    NN     NN
142600000000F8    -                   -    NN     NN
142700000000F9    -                   -    NN     NN
142800000000F10   -F10=Inserimento    -    SN     SN
142900000000F11   -                   -    NN     NN
143000000000F12   -F12=Ritorno        -    SS     SS
143100000000F13   -                   -    NN     NN
143200000000F14   -                   -    NN     NN
143300000000F15   -                   -    NN     NN
143400000000F16   -                   -    NN     NN
143500000000F17   -                   -    NN     NN
143600000000F18   -                   -    NN     NN
143700000000F19   -                   -    NN     NN
143800000000F20   -                   -    NN     NN
143900000000F21   -                   -    NN     NN
144000000000F22   -                   -    NN     NN
144100000000F23   -F23=Altre opzioni  -    NN     NN
144200000000F24   -F24=Altri tasti    -    NN     NN
144300000000ENTER -                   -    SS     SS
144400000000ROLLUP-                   -    SS     SS
144500000000ROLLDN-                   -    SS     SS
144600000000HELP  -                   -    NN     NN
144700000000PRINT -                   -    NN     NN
144800000000BSPACE-                   -    NN     NN
144900000000CLEAR -                   -    NN     NN
