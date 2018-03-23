000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* ELENCO TRAINI - SUBFILE DI SCELTA
000400000000     H*------------------------------------------------------------------------*
000500000000     H* INPUT -->  TRAINO VUOTO
000600000000     H*            DATA IN AAAA/MM/GG   -OPZIONALE-
000700000000     H*            FILIALE FERMATA      -OPZIONALE-
000800000000     H*            CARICO/SCARICO       -OPZIONALE-  (C/S/' ')
000900000000     H* OUTPUT --> TRAINO
001000000000     H*            DECORRENZA IN AAAA/MM/GG
001100000000     H*------------------------------------------------------------------------*
001200000000     H* IL PROGRAMMA ELENCA TUTTI I TRAINI IN DECORRENZA ALLA DATA (DDE) CHE
001300000000     H* CARICANO O SCARICANO (SCA) ALLA FILIALE RICHIESTA (FIL).
001400000000     H* SE UNO O PIU' PARAMETRI SONO VUOTI IL PGM NON FA NESSUNA ESCLUSIONE.
001500000000     H*------------------------------------------------------------------------*
001600000000     F*------------------------------------------------------------------------*
001700000000     F* DATA BASE
001800000000     F*------------------------------------------------------------------------*
001900000000     FTNTL17D   CF   E             WORKSTN INFDS(DEVDS1)
002000000000     F                                     SFILE(TL17DVS:SFLULT)
002100000000     F                                     SFILE(TL17DVM:MSGULT)
002200000000     FAZORG01L  IF   E           K DISK
002300000000     FTABEL00F  IF   E           K DISK
002400000000     FTNTLT01L  IF   E           K DISK
002500000000     FTNTLR01L  IF   E           K DISK
002600960226     FTNTLR03L  IF   E           K DISK
002700960226     F                                     RENAME(TNTLR000:TNTLR3)
002800960226     FTNTLO01L  IF   E           K DISK
002900000000     D*------------------------------------------------------------------------*
003000000000     D* SCHIERE
003100000000     D*------------------------------------------------------------------------*
003200000000     D*-------------------
003300000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
003400000000     D*-------------------
003500000000     D MSG             S             78    DIM(10) CTDATA PERRCD(1)             *MESSAGGI
003600000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
003700000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
003800000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
003900000000     D*-------------------
004000000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
004100000000     D*-------------------
004200000000     D FFU             S              6    DIM(31)                              *FUNZIONE
004300000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
004400000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
004500000000     D*-------------------
004600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
004700000000     D*-------------------
004800000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
004900000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
005000000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
005100000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
005200000000     D*-------------------
005300000000     D* TABELLA DESCRIZIONI A VIDEO
005400000000     D*-------------------
005500000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
005600000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
005700000000     D*-------------------
005800000000     D* TABELLA TASTI FUNZIONALI PREMUTI
005900000000     D*-------------------
006000000000     D KEYS            S              1    DIM(40)
006100000000     D*-------------------
006200000000     D* FILIALI
006300000000     D*-------------------
006400971125     D ORG             S              3  0 DIM(500)                             *CODICE
006500971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
006600000000     D*-------------------
006700000000     D* TIPI TRAINI
006800000000     D*-------------------
006900000000     D TTR             S              1    DIM(20)                              *TIPI TRAINI
007000000000     D DTTR            S             30    DIM(20)                              *DESCRIZIONE
007100960226     D*-------------------
007200960226     D* GIORNI DI PARTENZA
007300960226     D*-------------------
007400960226     D GIO             S              1    DIM(7)
007500960226     D*-------------------
007600960226     D* DECODIFICA GIORNI DELLA SETTIMANA
007700960226     D*-------------------
007800960226     D GSE             S             10    DIM(8) CTDATA PERRCD(1)
007900000000     D*------------------------------------------------------------------------*
008000000000     D* INPUT
008100000000     D*------------------------------------------------------------------------*
008200000000     D*---------------------
008300000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
008400000000     D*---------------------
008500000000     D DSOPZ         E DS
008600000000     D  OPZRIG                 1     40
008700000000     D*---------------------
008800000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
008900000000     D*---------------------
009000000000     D DSFUN         E DS
009100000000     D  FUNRIG                 1     40
009200000000     D*---------------------
009300000000     D* DS PER GESTIONE TASTI FUNZIONALI
009400000000     D*---------------------
009500000000     D DSKEYS        E DS
009600000000     D  KEYRIG                 1     40
009700000000     D*---------------------
009800000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
009900000000     D*---------------------
010000000000     D                SDS
010100000000     D  USER                 254    263
010200000000     D*---------------------
010300000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
010400000000     D*---------------------
010500000000     D DEVDS1          DS
010600000000     D  KEY                  369    369
010700000000     D  CURSOR               370    371
010800000000     D  FRTSFL               378    379B 0
010900000000     D*---------------------
011000000000     D* ARCHITETTURA
011100000000     D*---------------------
011200000000     D KPJBA         E DS
011300000000     D*---------------------
011400000000     D* DS ESTERNA DI PROCEDURA
011500000000     D*---------------------
011600000000     D DS00          E DS                  EXTNAME(TNTL00DS)
011700000000     D*---------------------
011800000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
011900000000     D*---------------------
012000000000     D DSMSG         E DS
012100000000     D*---------------------
012200000000     D* DS TIPI TRAINO
012300000000     D*---------------------
012400000000     D DSTV          E DS
012500020715
012600020715     d DdatiUte      e ds
012700020715     d AzuteDs       e ds                  ExtName(Azute00f)
012800020715     d Tibs34Ds      e ds                  Inz
012900020715
013000000000     D*------------------
013100000000     D* DS CONTROLLO DATA (8)
013200000000     D*------------------
013300000000     D WLBDA8          DS                  INZ
013400000000     D  G08DAT                 1      8  0
013500000000     D  G08INV                 9     16  0
013600000000     D  G08ERR                17     17
013700000000     D  G08TGI                18     22  0
013800000000     D*------------------
013900000000     D* DEFINIZIONE COSTANTI
014000000000     D*------------------
014100000000     D DESCAR          C                   CONST('CARICO    ')
014200000000     D DESSCA          C                   CONST('SCARICO   ')
014300000000     D DESTUT          C                   CONST('TUTTI     ')
014400000000     C*------------------------------------------------------------------------*
014500000000     C* MAIN LINE
014600000000     C*------------------------------------------------------------------------*
014700000000     C*
014800000000     C* CICLO FINO A CHE:
014900000000DO  1C     $FINE         DOUEQ     '1'
015000000000     C*
015100000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
015200000000     C*
015300000000     C* CONTROLLA GESTIONE VIDEO
015400000000IF  2C     $GES          IFEQ      'VS'
015500000000     C                   EXSR      GESVS                                        *SUBFILE
015600000000E   2C                   ENDIF
015700000000     C*
015800000000E   1C                   ENDDO
015900000000     C*
016000000000IF  1C     $F03          IFEQ      '1'
016100000000     C                   MOVEL     '1'           D00F03
016200000000E   1C                   ENDIF
016300000000IF  1C     $F12          IFEQ      '1'
016400000000     C                   MOVEL     '1'           D00F12
016500000000E   1C                   ENDIF
016600000000     C* FINE PGM
016700000000     C                   MOVEL     DS00          KPJBU
016800000000     C                   SETON                                        LR
016900000000     C*------------------------------------------------------------------------*
017000000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
017100000000     C*------------------------------------------------------------------------*
017200000000     C     CARVS         BEGSR
017300000000     C*
017400000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
017500000000     C*
017600000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
017700000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
017800000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
017900000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
018000000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
018100000000     C*
018200000000     C                   WRITE     TL17DVT                                      *TESTATA
018300000000     C                   WRITE     TL17DVO                                      *OPZIONI
018400000000     C                   WRITE     TL17DVF                                      *FUNZIONI
018500000000     C*
018600000000     C                   EXSR      POSSFL                                       *CARICA SFL
018700000000     C*
018800000000     C                   ENDSR
018900000000     C*------------------------------------------------------------------------*
019000000000     C* GESVS  - GESTIONE SUBFILE
019100000000     C*------------------------------------------------------------------------*
019200000000     C     GESVS         BEGSR
019300000000     C*
019400000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
019500000000     C                   WRITE     TL17DVT                                      *TESTATA
019600000000     C                   WRITE     TL17DVO                                      *OPZIONI
019700000000     C                   WRITE     TL17DVF                                      *FUNZIONI
019800000000E   1C                   ENDIF
019900000000     C*
020000000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
020100000000     C*
020200000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
020300000000     C*
020400000000IF  1C     $ERR          IFEQ      '0'
020500000000SL  2C                   SELECT
020600000000     C                   OTHER
020700000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
020800000000E   2C                   ENDSL
020900000000E   1C                   ENDIF
021000000000     C*
021100000000     C                   ENDSR
021200000000     C*------------------------------------------------------------------------*
021300000000     C* GESOPZ - GESTIONE OPZIONI
021400000000     C*------------------------------------------------------------------------*
021500000000     C     GESOPZ        BEGSR
021600000000     C*
021700000000SL  1C                   SELECT
021800000000     C     VSCOPZ        WHENEQ    ' 1'
021900000000     C                   EXSR      O01GES                                       *SCELTA
022000000000     C     VSCOPZ        WHENEQ    ' 5'
022100000000     C                   EXSR      O05GES                                       *VISUALIZZAZIONE
022200000000     C                   OTHER
022300000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
022400000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
022500000000E   1C                   ENDSL
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*------------------------------------------------------------------------*
022900000000     C* O01GES - GESTIONE OPZIONE 1=SCELTA
023000000000     C*------------------------------------------------------------------------*
023100000000     C     O01GES        BEGSR
023200000000     C*
023300000000     C* CHAIMA IL PROGRAMMA DI GUIDA DELLA GESTIONE TRAINI
023400000000IF  1C     $ERR          IFEQ      '0'
023500000000     C                   Z-ADD     VSCTRN        D00TRN
023600000000     C                   Z-ADD     VSHDDE        D00DDE
023700000000     C                   MOVEL     '1'           $FINE
023800000000E   1C                   ENDIF
023900000000     C*
024000000000     C                   ENDSR
024100000000     C*------------------------------------------------------------------------*
024200000000     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
024300000000     C*------------------------------------------------------------------------*
024400000000     C     O05GES        BEGSR
024500000000     C*
024600000000     C* RITORNA IL CODICE SELEZIONATO
024700000000IF  1C     $ERR          IFEQ      '0'
024800000000     C                   MOVEL     'T05'         D00OP0                         *PARAMETRI FISSI
024900000000     C                   MOVEL     'O05'         D00OP1
025000000000     C                   MOVEL     '0'           D00F03
025100000000     C                   MOVEL     '0'           D00F12
025200000000     C                   MOVEL     '0'           D00ERR
025300000000     C                   MOVEL     *BLANKS       D00MSG
025400000000     C                   Z-ADD     VSCTRN        D00TRN                         *PARAMETRI SELEZ.
025500000000     C                   Z-ADD     VSHDDE        D00DDE
025600000000     C                   MOVEL     DS00          KPJBU
025700000000     C                   CALL      'TNTL01C'
025800000000     C                   PARM                    KPJBA
025900000000E   1C                   ENDIF
026000000000     C*
026100000000     C                   ENDSR
026200000000     C*------------------------------------------------------------------------*
026300000000     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
026400000000     C*------------------------------------------------------------------------*
026500000000     C     NEWPOS        BEGSR
026600000000     C*
026700000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
026800000000     C*        1 = RICHIESTO POSIZIONAMENTO
026900000000     C                   Z-ADD     0             SWPOSI            7 0
027000000000     C*
027100000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
027200000000     C*            NON OCCORRE RICARICARE IL SUBFILE
027300000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
027400000000     C                   Z-ADD     0             SWMODI            1 0
027500000000     C*
027600000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLA I PARAMETRI
027700000000     C                   EXSR      CHKPAR
027800000000     C*
027900000000     C                   ENDSR
028000000000     C*------------------------------------------------------------------------*
028100000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
028200000000     C*------------------------------------------------------------------------*
028300000000     C     CHKPAR        BEGSR
028400000000     C*
028500000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
028600000000     C* CURSORE SUL CAMPO DEL CONTROL
028700000000IF  1C     $ERR          IFEQ      '1'
028800000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
028900000000X   1C                   ELSE
029000000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
029100000000E   1C                   ENDIF
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
029600000000     C*------------------------------------------------------------------------*
029700000000     C     SETFIL        BEGSR
029800000000     C*
029900000000     C                   MOVEL     '1'           *IN43                          *EMETTE SFILE
030000000000     C*
030100000000     C                   Z-ADD     D00TRN        KLTTRN
030200000000     C                   Z-ADD     D00DDE        KLTDDE
030300000000     C     KEYTLT        SETLL     TNTLT01L                           99        *FINE FILE
030400000000     C*
030500000000     C  N99              EXSR      LETFIL
030600000000     C*
030700000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
030800000000IF  1C     *IN99         IFEQ      *ON
030900000000     C                   Z-ADD     1             SFLEOF
031000000000E   1C                   ENDIF
031100000000     C*
031200000000     C                   ENDSR
031300000000     C*------------------------------------------------------------------------*
031400000000     C* LETFIL - LETTURA RECORD SUCESSIVO
031500000000     C*------------------------------------------------------------------------*
031600000000     C     LETFIL        BEGSR
031700000000     C*
031800000000     C* LEGGO FINO A:
031900000000     C*     - FINE FILE
032000000000     C*     - TROVATO RECORD VALIDO
032100000000     C                   MOVEL     'N'           WRECOK            1            *RECORD NON VALIDO
032200000000DO  1C     *IN99         DOWEQ     *OFF
032300000000     C     WRECOK        ANDEQ     'N'
032400960226     C                   READ      TNTLT01L                               99
032500000000     C*
032600000000IF  2C     *IN99         IFEQ      *ON
032700000000     C                   Z-ADD     1             SFLEOF
032800000000X   2C                   ELSE
032900000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
033000000000E   2C                   ENDIF
033100000000     C*
033200000000E   1C                   ENDDO
033300000000     C*
033400000000     C                   ENDSR
033500000000     C*------------------------------------------------------------------------*
033600000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
033700000000     C*------------------------------------------------------------------------*
033800000000     C     CHKREC        BEGSR
033900000000     C***
034000000000     C* ESCLUSIONI DA TRAINO
034100000000     C***
034200000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
034300000000     C* TRAINO ANNULLATO, ESCLUDE
034400000000IF  1C     TLTATB        IFNE      *BLANKS
034500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
034600000000E   1C                   ENDIF
034700000000     C* TRAINO SCADUTO, ESCLUDE
034800000000IF  1C     D00DDE        IFGT      *ZEROS
034900000000IF  2C     D00DDE        IFLT      TLTDDE
035000000000     C     D00DDE        ORGT      TLTDSC
035100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
035200000000E   2C                   ENDIF
035300000000E   1C                   ENDIF
035400000000     C***
035500000000     C* ESCLUSIONI DA FERMATE
035600000000     C***
035700000000IF  1C     WRECOK        IFEQ      'S'
035800000000     C                   Z-ADD     TLTTRN        KLRTRN
035900000000     C                   Z-ADD     TLTDDE        KLRDDE
036000000000     C     KEYTLR        SETLL     TNTLR01L
036100000000     C     KEYTLR        READE     TNTLR01L                               98
036200000000DO  2C     *IN98         DOWEQ     *OFF
036300000000     C                   MOVEL     'N'           $OKF                           *RECORD NON OK
036400000000     C                   MOVEL     'N'           $OKS                           *RECORD NON OK
036500000000     C* FERMATA
036600000000IF  3C     D00FIL        IFEQ      *ZEROS                                       *NESSUNA SELEZIONE
036700000000     C                   MOVEL     'S'           $OKF                           *RECORD OK
036800000000X   3C                   ELSE
036900000000IF  4C     D00FIL        IFEQ      TLRFIL                                       *SELEZIONE
037000000000     C                   MOVEL     'S'           $OKF                           *RECORD OK
037100000000E   4C                   ENDIF
037200000000E   3C                   ENDIF
037300000000     C* CARICO
037400000000IF  3C     D00SCA        IFEQ      *BLANKS                                      *NESSUNA SELEZIONE
037500000000     C                   MOVEL     'S'           $OKS                           *RECORD OK
037600000000X   3C                   ELSE
037700000000IF  4C     D00SCA        IFEQ      'C'                                          *SELEZIONE
037800000000     C     TLRCAR        ANDEQ     'S'
037900000000     C                   MOVEL     'S'           $OKS                           *RECORD OK
038000000000E   4C                   ENDIF
038100000000     C* SCARICO
038200000000IF  4C     D00SCA        IFEQ      'S'                                          *SELEZIONE
038300000000     C     TLRSCA        ANDEQ     'S'
038400000000     C                   MOVEL     'S'           $OKS                           *RECORD OK
038500000000E   4C                   ENDIF
038600000000E   3C                   ENDIF
038700000000     C*
038800000000IF  3C     $OKF          IFEQ      'S'                                          *OK PER FILIALE
038900000000     C     $OKS          ANDEQ     'S'                                          *OK PER CARICO
039000000000     C                   LEAVE
039100000000E   3C                   ENDIF
039200000000     C     KEYTLR        READE     TNTLR01L                               98
039300000000E   2C                   ENDDO
039400000000IF  2C     $OKF          IFNE      'S'                                          *NON OK PER FILIALE
039500000000     C     $OKS          ORNE      'S'                                          *NON OK PER CAR/SCA
039600000000     C                   MOVEL     'N'           WRECOK
039700000000E   2C                   ENDIF
039800000000     C*
039900000000E   1C                   ENDIF
040000000000     C*
040100000000     C                   ENDSR
040200000000     C*------------------------------------------------------------------------*
040300000000     C* IRESFL - IMPOSTA I CAMPI DEL SUBFILE DAL FILE FISICO
040400000000     C*------------------------------------------------------------------------*
040500960226     C     IRESFL        BEGSR
040600000000     C* HIDDEN
040700000000     C                   Z-ADD     TLTDDE        VSHDDE                         *DECORRENZA
040800960226     C* CAMPI
040900000000     C                   Z-ADD     TLTTRN        VSCTRN                         *TRAINO
041000000000     C                   Z-ADD     TLTTFP        VSCTFP                         *PARTENZA
041100000000     C                   Z-ADD     TLTTFA        VSCTFA                         *ARRIVO
041200960226     C                   MOVEA     TLTGSE        GIO                            *GIORNI DI PARTENZA
041300960226IF  1C     GIO(1)        IFNE      *BLANKS
041400960226     C                   MOVEL     GSE(1)        VSCLUN
041500960226X   1C                   ELSE
041600960226     C                   MOVEL     '-'           VSCLUN
041700960226E   1C                   ENDIF
041800960226IF  1C     GIO(2)        IFNE      *BLANKS
041900960226     C                   MOVEL     GSE(2)        VSCMAR
042000960226X   1C                   ELSE
042100960226     C                   MOVEL     '-'           VSCMAR
042200960226E   1C                   ENDIF
042300960226IF  1C     GIO(3)        IFNE      *BLANKS
042400960226     C                   MOVEL     GSE(3)        VSCMER
042500960226X   1C                   ELSE
042600960226     C                   MOVEL     '-'           VSCMER
042700960226E   1C                   ENDIF
042800960226IF  1C     GIO(4)        IFNE      *BLANKS
042900960226     C                   MOVEL     GSE(4)        VSCGIO
043000960226X   1C                   ELSE
043100960226     C                   MOVEL     '-'           VSCGIO
043200960226E   1C                   ENDIF
043300960226IF  1C     GIO(5)        IFNE      *BLANKS
043400960226     C                   MOVEL     GSE(5)        VSCVEN
043500960226X   1C                   ELSE
043600960226     C                   MOVEL     '-'           VSCVEN
043700960226E   1C                   ENDIF
043800960226IF  1C     GIO(6)        IFNE      *BLANKS
043900960226     C                   MOVEL     GSE(6)        VSCSAB
044000960226X   1C                   ELSE
044100960226     C                   MOVEL     '-'           VSCSAB
044200960226E   1C                   ENDIF
044300960226IF  1C     GIO(7)        IFNE      *BLANKS
044400960226     C                   MOVEL     GSE(7)        VSCDOM
044500960226X   1C                   ELSE
044600960226     C                   MOVEL     '-'           VSCDOM
044700960226E   1C                   ENDIF
044800000000     C* DECODIFICHE
044900000000     C                   SETOFF                                           28
045000000000     C                   Z-ADD     1             J
045100000000     C                   MOVEL     *BLANKS       VSDTFP
045200000000     C     VSCTFP        LOOKUP    ORG(J)                                 28
045300000000IF  1C     *IN28         IFEQ      *ON
045400000000     C                   MOVEL     DORG(J)       VSDTFP                         *PARTENZA
045500000000E   1C                   ENDIF
045600000000     C*
045700000000     C                   SETOFF                                           28
045800000000     C                   Z-ADD     1             J
045900000000     C                   MOVEL     *BLANKS       VSDTFA
046000000000     C     VSCTFA        LOOKUP    ORG(J)                                 28
046100000000IF  1C     *IN28         IFEQ      *ON
046200000000     C                   MOVEL     DORG(J)       VSDTFA                         *ARRIVO
046300000000E   1C                   ENDIF
046400000000     C*
046500000000     C                   SETOFF                                           28
046600000000     C                   Z-ADD     1             J
046700000000     C                   MOVEL     *BLANKS       VSDTTR
046800960226     C     TLTTTR        LOOKUP    TTR(J)                                 28
046900000000IF  1C     *IN28         IFEQ      *ON
047000000000     C                   MOVEL     DTTR(J)       VSDTTR                         *TIPO TRAINO
047100000000E   1C                   ENDIF
047200960226     C*
047300960226     C* REPERISCE GIORNO E ORA DELLA FILIALE FERMATA:
047400960226     C* SE FILIALE DI CARICO  -> ORARIO DI PARTENZA
047500960226     C* SE FILIALE DI SCARICO -> ORARIO DI ARRIVO
047600960227     C* NB.CONTROLLA SE E' STATO INSERITO L'ORARIO PERCHE' NEI TRAINI LOCALI LA
047700960227     C*    FILIALE E' RIPETUTA DUE VOLTE (CARICO E SCARICO).
047800960226     C                   Z-ADD     TLTTRN        KLRTRN                         *NUMERO TRAINO
047900960226     C                   Z-ADD     TLTDDE        KLRDDE                         *DATA DECORRENZA
048000960226     C                   MOVEL     'A'           KLRTFM                         *TIPO FERMATA
048100960226     C                   Z-ADD     D00FIL        KLRFIL                         *FILIALE FERMATA
048200960227     C     KE2TLR        SETLL     TNTLR03L
048300960227     C     KE2TLR        READE     TNTLR03L                               98
048400960227     C                   Z-ADD     *ZEROS        WNFM
048500960227     C* REPERISCE LA FERMATA
048600960227DO  1C     *IN98         DOWEQ     *OFF
048700960227     C     WNFM          ANDEQ     *ZEROS
048800960227     C                   Z-ADD     TLTTRN        KLOTRN                         *NUMERO TRAINO
048900960227     C                   Z-ADD     TLTDDE        KLODDE                         *DATA DECORRENZA
049000960227     C                   Z-ADD     TLRNFM        KLONFM                         *NUMERO FERMATA
049100960227     C     KE2TLO        CHAIN     TNTLO01L                           97
049200960227IF  2C     *IN97         IFEQ      *OFF
049300960227IF  3C     VCCSCA        IFEQ      'S'                                          *SCARICO
049400960227     C     TLOGGA        ANDNE     *BLANKS                                      *ORARIO ARRIVO INS.
049500960227     C     VCCSCA        OREQ      'C'                                          *CARICO
049600960227     C     TLOGGP        ANDNE     *BLANKS                                      *ORARIO PARTENZA INS
049700960227     C     VCCSCA        OREQ      *BLANKS                                      *NO CARICO NO SCARIC
049800960227     C                   Z-ADD     TLRNFM        WNFM                           *NUMERO FERMATA
049900960227E   3C                   ENDIF
050000960227E   2C                   ENDIF
050100960227     C     KE2TLR        READE     TNTLR03L                               98
050200960227E   1C                   ENDDO
050300960227IF  1C     WNFM          IFEQ      *ZEROS                                       *INESISTENTE
050400960227     C                   Z-ADD     1             WNFM                           *PRIMA FERMATA
050500960227E   1C                   ENDIF
050600960227     C* REPERISCE L'ORARIO
050700960226     C                   Z-ADD     TLTTRN        KLOTRN                         *NUMERO TRAINO
050800960226     C                   Z-ADD     TLTDDE        KLODDE                         *DATA DECORRENZA
050900960226     C                   Z-ADD     WNFM          KLONFM                         *NUMERO FERMATA
051000960226     C                   MOVEL     GIOCOR        KLOGSE                         *GIORNO CORRENTE
051100960226     C     KEYTLO        SETLL     TNTLO01L
051200960226     C     KEYTLO        READE     TNTLO01L                               98
051300960226IF  1C     *IN98         IFEQ      *ON
051400960226     C                   MOVEL     *BLANKS       KLOGSE                         *GIORNO "TUTTI" O 1°
051500960226     C     KEYTLO        SETLL     TNTLO01L
051600960226     C     KE2TLO        READE     TNTLO01L                               98
051700960226E   1C                   ENDIF
051800960226IF  1C     VCCSCA        IFEQ      'S'                                          *SCARICO
051900960226     C                   MOVEL     TLOGGA        VSCGGX                         *GIORNO ARRIVO
052000960226     C                   Z-ADD     TLOHHA        VSCHHX                         *ORA ARRIVO
052100960227X   1C                   ELSE
052200960227IF  2C     VCCSCA        IFEQ      'C'                                          *CARICO
052300960226     C                   MOVEL     TLOGGP        VSCGGX                         *GIORNO PARTENZA
052400960226     C                   Z-ADD     TLOHHP        VSCHHX                         *ORA PARTENZA
052500960227X   2C                   ELSE
052600960227IF  3C     TLOGGP        IFNE      *BLANKS                                      *NO CARICO NO SCARIC
052700960227     C                   MOVEL     TLOGGP        VSCGGX                         *GIORNO PARTENZA
052800960227     C                   Z-ADD     TLOHHP        VSCHHX                         *ORA PARTENZA
052900960227X   3C                   ELSE
053000960227     C                   MOVEL     TLOGGA        VSCGGX                         *GIORNO ARRIVO
053100960227     C                   Z-ADD     TLOHHA        VSCHHX                         *ORA ARRIVO
053200960227E   3C                   ENDIF
053300960227E   2C                   ENDIF
053400960227E   1C                   ENDIF
053500960226IF  1C     KLOGSE        IFEQ      *BLANKS
053600960226     C                   MOVEL     GSE(8)        VSCGSE
053700960226     C                   ELSE
053800960226     C                   MOVEL     KLOGSE        G                 1 0
053900960226     C                   MOVEL     GSE(G)        VSCGSE
054000960226E   1C                   ENDIF
054100000000     C*
054200000000     C                   ENDSR
054300000000     C*------------------------------------------------------------------------*
054400000000     C* IMPVID - IMPOSTAZIONI VIDEO
054500000000     C*------------------------------------------------------------------------*
054600000000     C     IMPVID        BEGSR
054700000000     C* DATA
054800000000     C                   Z-ADD     D00DDE        G08INV
054900000000     C                   Z-ADD     *ZEROS        G08DAT
055000000000     C                   MOVEL     '3'           G08ERR
055100000000     C                   CALL      'XSRDA8'
055200000000     C                   PARM                    WLBDA8
055300000000     C                   Z-ADD     G08DAT        VCCDDE
055400000000     C* FILIALE
055500000000     C                   Z-ADD     D00FIL        VCCFIL
055600000000     C                   SETOFF                                           28
055700000000     C                   Z-ADD     1             J
055800000000     C     VCCFIL        LOOKUP    ORG(J)                                 28
055900000000IF  1C     *IN28         IFEQ      *ON
056000000000     C                   MOVEL     DORG(J)       VCDFIL
056100000000E   1C                   ENDIF
056200000000     C* CARICO / SCARICO
056300000000     C                   MOVEL     D00SCA        VCCSCA
056400000000IF  1C     VCCSCA        IFEQ      'C'
056500000000     C                   MOVEL     DESCAR        VCDSCA
056600000000E   1C                   ENDIF
056700000000IF  1C     VCCSCA        IFEQ      'S'
056800000000     C                   MOVEL     DESSCA        VCDSCA
056900000000E   1C                   ENDIF
057000000000IF  1C     VCCSCA        IFEQ      *BLANKS
057100000000     C                   MOVEL     DESTUT        VCDSCA
057200000000E   1C                   ENDIF
057300960226     C*
057400960226     C* PARTENZA / ARRIVO
057500960226IF  1C     VCCSCA        IFEQ      'C'
057600960226     C                   MOVEL     'PAR'         VCCMPA
057700960227E   1C                   ENDIF
057800960227IF  1C     VCCSCA        IFEQ      'S'
057900960226     C                   MOVEL     'ARR'         VCCMPA
058000960226E   1C                   ENDIF
058100960227IF  1C     VCCSCA        IFEQ      *BLANKS
058200960227     C                   MOVEL     'P/A'         VCCMPA
058300960227E   1C                   ENDIF
058400000000     C*
058500000000     C                   ENDSR
058600960226     C*------------------------------------------------------------------------*
058700960226     C* REPDAY - REPERISCE IL GIORNO ATTUALE
058800960226     C*------------------------------------------------------------------------*
058900960226     C     REPDAY        BEGSR
059000960226     C*
059100960226     C                   MOVEL     UDATE         PARDAT                         *DATA CORRENTE G/M/A
059200960226     C                   MOVEL     *BLANKS       PARGIO                         *GIORNO DI SETTIMANA
059300960226     C                   CALL      'XGIOSE1'
059400960226     C                   PARM                    PARDAT            6
059500960226     C                   PARM                    PARGIO            1
059600960226     C*
059700960226     C                   MOVEL     PARGIO        GIOCOR            1            *GIORNO CORRENTE
059800960226     C*
059900960226     C                   ENDSR
060000000000     C*------------------------------------------------------------------------*
060100000000     C* CARTAB - CARICA LE TABELLE
060200000000     C*------------------------------------------------------------------------*
060300000000     C     CARTAB        BEGSR
060400000000     C*--------------------
060500000000     C* FILIALI
060600000000     C*--------------------
060700000000     C                   Z-ADD     *ZEROS        J                 4 0
060800000000     C     *LOVAL        SETLL     AZORG01L
060900000000     C                   READ      AZORG01L                               99
061000000000DO  1C     *IN99         DOWEQ     *OFF
061100000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
061200000000     C                   ADD       1             J
061300000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
061400000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
061500000000E   2C                   ENDIF
061600000000     C                   READ      AZORG01L                               99
061700000000E   1C                   ENDDO
061800000000     C*--------------------
061900000000     C* TIPI TRAINI
062000000000     C*--------------------
062100000000     C                   Z-ADD     *ZEROS        J
062200020715     c                   z-add     1             ktbkut
062300000000     C                   MOVEL     'TV'          KTBCOD
062400000000     C     KE2TAB        SETLL     TABEL00F
062500000000     C     KE2TAB        READE     TABEL00F                               99
062600000000DO  1C     *IN99         DOWEQ     *OFF
062700000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
062800000000     C                   ADD       1             J
062900000000     C                   MOVEL     TBLKEY        TTR(J)                         *CODICE
063000000000     C                   MOVEL     TBLUNI        DSTV
063100000000     C                   MOVEL     §TVDES        DTTR(J)                        *DESCRIZIONE
063200000000E   2C                   ENDIF
063300000000     C     KE2TAB        READE     TABEL00F                               99
063400000000E   1C                   ENDDO
063500000000     C*
063600000000     C                   ENDSR
063700000000     C*------------------------------------------------------------------------*
063800000000     C* *INZSR - ROUTINE INIZIALE
063900000000     C*------------------------------------------------------------------------*
064000000000     C     *INZSR        BEGSR
064100000000     C*--------------------
064200000000     C* RICEVIMENTO PARAMETRI
064300000000     C*--------------------
064400000000     C     *ENTRY        PLIST
064500000000     C                   PARM                    KPJBA
064600000000     C                   MOVEL     KPJBU         DS00
064700000000     C*--------------------
064800000000     C* VARIABILI RIFERITE AL DATA BASE
064900000000     C*--------------------
065000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
065100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
065200000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
065300000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
065400960226     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
065500960226     C     *LIKE         DEFINE    TLRDDE        KLRDDE
065600960226     C     *LIKE         DEFINE    TLRTFM        KLRTFM
065700960226     C     *LIKE         DEFINE    TLRFIL        KLRFIL
065800960226     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
065900960226     C     *LIKE         DEFINE    TLODDE        KLODDE
066000960226     C     *LIKE         DEFINE    TLONFM        KLONFM
066100960226     C     *LIKE         DEFINE    TLOGSE        KLOGSE
066200960226     C     *LIKE         DEFINE    TLRNFM        WNFM                           *DI LAVORO
066300000000     C*--------------------
066400000000     C* CHIAVI DI LETTURA
066500000000     C*--------------------
066600000000     C* LETTURA TNTLT01L - PARZIALE
066700000000     C     KE2TAB        KLIST
066800000000     C                   KFLD                    KTBKUT                         *UTENTE
066900000000     C                   KFLD                    KTBCOD                         *TABELLA
067000000000     C* LETTURA TNTLT01L - COMPLETA
067100000000     C     KEYTLT        KLIST
067200000000     C                   KFLD                    KLTTRN                         *TRAINO
067300960226     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
067400000000     C* LETTURA TNTLR01L - PARZIALE
067500000000     C     KEYTLR        KLIST
067600000000     C                   KFLD                    KLRTRN                         *TRAINO
067700000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
067800960226     C* LETTURA TNTLR03L - COMPLETA
067900960226     C     KE2TLR        KLIST
068000960226     C                   KFLD                    KLRTRN                         *TRAINO
068100960226     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
068200960226     C                   KFLD                    KLRTFM                         *TIPO FERMATA
068300960226     C                   KFLD                    KLRFIL                         *FILIALE FERMATA
068400960226     C* LETTURA TNTLO01L - COMPLETA
068500960226     C     KEYTLO        KLIST
068600960226     C                   KFLD                    KLOTRN                         *TRAINO
068700960226     C                   KFLD                    KLODDE                         *DATA DECORRENZA
068800960226     C                   KFLD                    KLONFM                         *NUMERO FERMATA
068900960226     C                   KFLD                    KLOGSE                         *GIORNO SETTIMANA
069000960226     C* LETTURA TNTLO01L - PARZIALE
069100960226     C     KE2TLO        KLIST
069200960226     C                   KFLD                    KLOTRN                         *TRAINO
069300960226     C                   KFLD                    KLODDE                         *DATA DECORRENZA
069400960226     C                   KFLD                    KLONFM                         *NUMERO FERMATA
069500000000     C*--------------------
069600000000     C* VARIABILI DI CONTROLLO
069700000000     C*--------------------
069800000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
069900000000     C                   MOVEL     'N'           $OKF              1            *VALIDITA' RECORD
070000000000     C                   MOVEL     'N'           $OKS              1            *VALIDITA' RECORD
070100000000     C***
070200000000     C* IMPOSTAZIONI VIDEO
070300000000     C***
070400000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
070500000000     C                   MOVE      D00OP0        INTGES            2
070600000000IF  1C     INTGES        IFEQ      '02'
070700000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
070800000000X   1C                   ELSE
070900000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
071000000000E   1C                   ENDIF
071100000000     C*
071200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
071300000000     C                   EXSR      INZSTD
071400000000     C*
071500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
071600000000     C                   EXSR      INZSFL
071700000000     C*
071800000000     C* CARICA LE TABELLE
071900000000     C                   EXSR      CARTAB
072000960226     C*
072100960226     C* REPERISCE IL GIORNO ATTUALE
072200960226     C                   EXSR      REPDAY
072300000000     C*
072400000000     C* IMPOSTAZIONI VIDEO
072500000000     C                   EXSR      IMPVID
072600000000     C*
072700000000     C* EMETTE VIDEATA SUBFILE
072800000000     C                   EXSR      CHKPAR
072900000000     C                   EXSR      CARVS
073000000000     C*
073100000000     C                   ENDSR
073200000000     C*------------------------------------------------------------------------*
073300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073500000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
073600000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
073700000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
073800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074000000000     C*------------------------------------------------------------------------*
074100000000     C* INZSTD - ROUTINE INIZIALE STANDARD
074200000000     C*------------------------------------------------------------------------*
074300000000     C     INZSTD        BEGSR
074400000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
074500000000     C                   CALL      'XKEYS'
074600000000     C                   PARM                    DSKEYS
074700000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
074800000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
074900000000     C     XMSGPA        PLIST
075000000000     C                   PARM                    DSMSMS
075100000000     C                   PARM                    DSMSPG
075200000000     C                   PARM                    DSMSKE
075300000000     C                   PARM                    DSMSER
075400000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
075500000000     C                   MOVEL     'TNTL17R'     DSMSPG                         *NOME PGM CHIAMANTE
075600000000     C                   MOVEL     'TNTL17R'     VMCQUE                         *CODA MESSAGGI DESTI
075700000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
075800000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
075900000000     C* INIZIALIZZO FLAG CONTROLLO PGM
076000000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
076100000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
076200000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F03
076300000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
076400000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
076500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
076600000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
076700000000     C* LANCIO ROUTINE PER VERIFICARE SE
076800000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
076900000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
077000020715
077100020715     c     *dtaara       define    §azute        azuteds
077200020715     c     *dtaara       define    §datiute      ddatiute
077300020715
077400020715     c                   in(E)     *dtaara
077500020715     c                   if        %Error or rsut = *blanks
077600020715     c                   clear                   tibs34ds
077700020715     c                   call      'TIBS34R'
077800020715     c                   parm                    Tibs34Ds
077900020715     c                   in        *dtaara
078000020715     c                   endif
078100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
078200000000     C                   BITOFF    '01234567'    POSMI1            1
078300000000     C                   BITOFF    '01234567'    POSMI2            1
078400000000     C                   BITOFF    '01234567'    POSMA1            1
078500000000     C                   BITOFF    '01234567'    POSMA2            1
078600000000     C                   BITON     '34'          POSMI1            1            *18
078700000000     C                   BITON     '7'           POSMI2            1            *01
078800000000     C                   BITON     '34'          POSMA1            1            *18
078900000000     C                   BITON     '0'           POSMA2            1            *80
079000000000     C                   MOVE      POSMI2        POSMIN            2
079100000000     C                   MOVEL     POSMI1        POSMIN
079200000000     C                   MOVE      POSMA2        POSMAX            2
079300000000     C                   MOVEL     POSMA1        POSMAX
079400000000     C                   ENDSR
079500000000     C*------------------------------------------------------------------------*
079600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
079700000000     C*------------------------------------------------------------------------*
079800000000     C     FUNSTD        BEGSR
079900000000     C                   SELECT
080000000000     C     KEY           WHENEQ    F3
080100000000     C                   EXSR      F03STD
080200000000     C     KEY           WHENEQ    F12
080300000000     C                   EXSR      F12STD
080400000000     C     KEY           WHENEQ    F24
080500000000     C                   EXSR      F24STD
080600000000     C                   OTHER
080700000000     C                   MOVEL     MSG(2)        $MSG
080800000000     C                   EXSR      CARMSG
080900000000     C                   ENDSL
081000000000     C                   ENDSR
081100000000     C*------------------------------------------------------------------------*
081200000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
081300000000     C*------------------------------------------------------------------------*
081400000000     C     F03STD        BEGSR
081500000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
081600000000     C                   MOVEL     '1'           $F03
081700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
081800000000     C                   MOVEL     '1'           $FINE
081900000000     C* PULISCE IL MESSAGGIO
082000000000     C                   MOVEL     *BLANKS       D00MSG
082100000000     C                   ENDSR
082200000000     C*------------------------------------------------------------------------*
082300000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
082400000000     C*------------------------------------------------------------------------*
082500000000     C     F12STD        BEGSR
082600000000     C*
082700000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
082800000000     C                   MOVEL     '1'           $F12
082900000000     C* IMPOSTO FLAG DI FIN PGM
083000000000     C                   MOVEL     '1'           $FINE
083100000000     C* PULISCE IL MESSAGGIO
083200000000     C                   MOVEL     *BLANKS       D00MSG
083300000000     C                   ENDSR
083400000000     C*------------------------------------------------------------------------*
083500000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
083600000000     C*------------------------------------------------------------------------*
083700000000     C     F24STD        BEGSR
083800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
083900000000     C                   EXSR      OUTFUN
084000000000     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
084300000000     C*------------------------------------------------------------------------*
084400000000     C     INZSFL        BEGSR
084500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
084600000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
084700000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
084800000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
084900000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
085000000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
085100000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
085200000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
085300000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
085400000000     C                   Z-ADD     12            SFLPAG
085500000000     C                   ENDSR
085600000000     C*------------------------------------------------------------------------*
085700000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
085800000000     C*------------------------------------------------------------------------*
085900000000     C     FUNSFL        BEGSR
086000000000     C                   SELECT
086100000000     C     KEY           WHENEQ    ENTER
086200000000     C                   EXSR      ENTSFL
086300000000     C     KEY           WHENEQ    F3
086400000000     C                   EXSR      F03STD
086500000000     C     KEY           WHENEQ    F5
086600000000     C                   EXSR      F05STD
086700000000     C     KEY           WHENEQ    F12
086800000000     C                   EXSR      F12STD
086900000000     C     KEY           WHENEQ    F17
087000000000     C                   EXSR      F17STD
087100000000     C     KEY           WHENEQ    F18
087200000000     C                   EXSR      F18STD
087300000000     C     KEY           WHENEQ    F23
087400000000     C                   EXSR      F23STD
087500000000     C     KEY           WHENEQ    F24
087600000000     C                   EXSR      F24STD
087700000000     C     KEY           WHENEQ    ROLLUP
087800000000     C                   EXSR      RUPSTD
087900000000     C     KEY           WHENEQ    ROLLDN
088000000000     C                   EXSR      RDNSTD
088100000000     C                   OTHER
088200000000     C                   MOVEL     MSG(2)        $MSG
088300000000     C                   EXSR      CARMSG
088400000000     C                   ENDSL
088500000000     C                   ENDSR
088600000000     C*------------------------------------------------------------------------*
088700000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
088800000000     C*------------------------------------------------------------------------*
088900000000     C     F05STD        BEGSR
089000000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
089100000000     C                   EXSR      POSSFL
089200000000     C                   ENDSR
089300000000     C*------------------------------------------------------------------------*
089400000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
089500000000     C*------------------------------------------------------------------------*
089600000000     C     F17STD        BEGSR
089700000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
089800000000     C                   Z-ADD     1             VCCNBR
089900000000     C                   ENDSR
090000000000     C*------------------------------------------------------------------------*
090100000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
090200000000     C*------------------------------------------------------------------------*
090300000000     C     F18STD        BEGSR
090400000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
090500000000     C                   Z-ADD     SFLULT        VCCNBR
090600000000     C                   ENDSR
090700000000     C*------------------------------------------------------------------------*
090800000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
090900000000     C*------------------------------------------------------------------------*
091000000000     C     F23STD        BEGSR
091100000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
091200000000     C                   EXSR      OUTOPZ
091300000000     C                   ENDSR
091400000000     C*------------------------------------------------------------------------*
091500000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
091600000000     C*------------------------------------------------------------------------*
091700000000     C     RUPSTD        BEGSR
091800000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
091900000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
092000000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
092100000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
092200000000     C                   ELSE
092300000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
092400000000     C                   ENDIF
092500000000     C                   ENDSR
092600000000     C*------------------------------------------------------------------------*
092700000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
092800000000     C*------------------------------------------------------------------------*
092900000000     C     RDNSTD        BEGSR
093000000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
093100000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
093200000000     C     CURSOR        IFGE      POSMIN
093300000000     C     CURSOR        ANDLE     POSMAX
093400000000     C                   ELSE
093500000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
093600000000     C                   MOVEL     '1'           $ERR
093700000000     C                   MOVEL     MSG(5)        $MSG
093800000000     C                   EXSR      CARMSG
093900000000     C                   ENDIF
094000000000     C                   ENDSR
094100000000     C*------------------------------------------------------------------------*
094200000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
094300000000     C*------------------------------------------------------------------------*
094400000000     C     ENTSFL        BEGSR
094500000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
094600000000     C                   EXSR      NEWPOS
094700000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
094800000000     C     $ERR          IFEQ      '0'                                          --- 1 -->
094900000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
095000000000     C     SWPOSI        IFNE      0                                            --- 2 -->
095100000000     C                   EXSR      POSSFL
095200000000     C                   ELSE                                                   <-- 2 -->
095300000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
095400000000     C     SFLULT        IFNE      0                                            --- 3 -->
095500000000     C                   EXSR      OPZSFL
095600000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
095700000000     C     SWMODI        IFNE      0                                            --- 4 -->
095800000000     C                   EXSR      POSSFL
095900000000     C                   ENDIF                                                  <-- 4 ---
096000000000     C                   ENDIF                                                  <-- 3 ---
096100000000     C                   ENDIF                                                  <-- 2 ---
096200000000     C                   ENDIF                                                  <-- 1 ---
096300000000     C                   ENDSR
096400000000     C*------------------------------------------------------------------------*
096500000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
096600000000     C*------------------------------------------------------------------------*
096700000000     C     POSSFL        BEGSR
096800000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
096900000000     C                   EXSR      CLRSFL
097000000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
097100000000     C                   EXSR      SETFIL
097200000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
097300000000     C     SFLEOF        IFEQ      0
097400000000     C                   EXSR      CARSFL
097500000000     C                   ENDIF
097600000000     C                   ENDSR
097700000000     C*------------------------------------------------------------------------*
097800000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
097900000000     C*------------------------------------------------------------------------*
098000000000     C     OPZSFL        BEGSR
098100000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
098200000000     C                   MOVEL     'S'           OPZOK             1
098300000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
098400000000     C                   Z-ADD     0             SFLVAR            5 0
098500000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
098600000000     C                   Z-ADD     0             SFLERR
098700000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
098800000000     C                   Z-ADD     SFLULT        WFLULT
098900000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
099000000000     C                   READC     TL17DVS                                49
099100000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
099200000000     C     $ERR          ANDEQ     '0'
099300000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
099400000000     C                   ADD       1             SFLVAR
099500000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
099600000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
099700000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
099800000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
099900000000     C                   EXSR      CONOPZ
100000000000     C     OPZOK         IFEQ      'S'
100100000000     C     $ERR          ANDEQ     '0'
100200000000     C                   EXSR      GESOPZ
100300000000     C                   ENDIF
100400000000     C                   ENDIF                                                  <-- 3 ---
100500000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
100600000000     C*            2. PULIZIA SCELTA IN SFL
100700000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
100800000000     C                   MOVEL     '0'           *IN41
100900000000     C                   MOVEL     *BLANKS       VSCOPZ
101000000000     C                   ELSE                                                   <-- 3 -->
101100000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
101200000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
101300000000     C                   MOVEL     '1'           *IN41
101400000000     C                   Z-ADD     SFLULT        SFLERR
101500000000     C                   ENDIF                                                  <-- 3 ---
101600000000     C                   ELSE                                                   <-- 2 -->
101700000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
101800000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
101900000000     C                   MOVEL     '1'           *IN41
102000000000     C                   ENDIF                                                  <-- 2 ---
102100000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
102200000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
102300000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
102400000000     C     OPZOK         IFNE      'S'
102500000000     C                   Z-ADD     SFLERR        VCCNBR
102600000000     C                   ELSE
102700000000     C                   Z-ADD     SFLULT        VCCNBR
102800000000     C                   ENDIF
102900000000     C* AGGIORNO RECORD SFL
103000000000     C                   UPDATE    TL17DVS
103100000000     C     $ERR          IFEQ      '0'
103200000000     C                   READC     TL17DVS                                49
103300000000     C                   ENDIF
103400000000     C                   ENDDO                                                  <-- 1 ---
103500000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
103600000000     C                   Z-ADD     WFLULT        SFLULT
103700000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
103800000000     C     SFLVAR        IFEQ      0
103900000000     C                   MOVEL     '1'           $FINE
104000000000     C                   ENDIF
104100000000     C                   ENDSR
104200000000     C*------------------------------------------------------------------------*
104300000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
104400000000     C*------------------------------------------------------------------------*
104500000000     C     CLRSFL        BEGSR
104600000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
104700000000     C* (0=NON FINE FILE  1=FINE FILE)
104800000000     C                   Z-ADD     0             SFLEOF
104900000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
105000000000     C                   Z-ADD     0             SFLULT
105100000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
105200000000     C                   MOVEL     '1'           *IN40
105300000000     C                   WRITE     TL17DVC
105400000000     C                   MOVEL     '0'           *IN40
105500000000     C                   ENDSR
105600000000     C*------------------------------------------------------------------------*
105700000000     C* CARSFL - CARICAMENTO SFL
105800000000     C*------------------------------------------------------------------------*
105900000000     C     CARSFL        BEGSR
106000000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
106100000000     C     SFLULT        IFEQ      0
106200000000     C                   EXSR      SETFIL
106300000000     C                   ENDIF
106400000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
106500000000     C                   Z-ADD     SFLULT        WFLULT
106600000000     C* CICLO:
106700000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
106800000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
106900000000     C                   Z-ADD     1             SFLRIG
107000000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
107100000000     C     SFLEOF        ANDEQ     0
107200000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
107300000000     C                   MOVEL     *BLANKS       VSCOPZ
107400000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
107500000000     C                   EXSR      IRESFL
107600000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
107700000000     C                   ADD       1             SFLULT
107800000000     C* SCRITTURA RECORD SFL
107900000000     C                   WRITE     TL17DVS
108000000000     C                   ADD       1             SFLRIG
108100000000     C* LETTURA FILE DA CARICARE
108200000000     C                   EXSR      LETFIL
108300000000     C                   ENDDO                                                  <-- 1 ---
108400000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
108500000000     C     WFLULT        IFNE      SFLULT
108600000000     C                   Z-ADD     WFLULT        VCCNBR
108700000000     C                   ADD       1             VCCNBR
108800000000     C                   ENDIF
108900000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
109000000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
109100000000     C     SFLULT        ANDGT     *ZEROS
109200960226     C*---                 MOVEL'1'       $ERR
109300960226     C*---                 MOVELMSG,6     $MSG
109400960226     C*---                 EXSR CARMSG
109500000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
109600000000     C     SFLULT        CHAIN     TL17DVS                            99
109700000000     C     *IN99         IFEQ      '0'
109800000000     C                   SETON                                        45
109900000000     C                   UPDATE    TL17DVS
110000000000     C                   SETOFF                                       45
110100000000     C                   ENDIF
110200000000     C                   ENDIF                                                  <-- 1 ---
110300000000     C                   ENDSR
110400000000     C*------------------------------------------------------------------------*
110500000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
110600000000     C*------------------------------------------------------------------------*
110700000000     C     OUTSFL        BEGSR
110800000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
110900000000     C     MSGULT        IFNE      *ZEROS
111000000000     C                   EXSR      OUTMSG
111100000000     C                   ENDIF
111200000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
111300000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
111400000000     C     SFLEOF        IFEQ      0
111500000000     C                   MOVEL     '0'           *IN42
111600000000     C                   ELSE
111700000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
111800000000     C                   MOVEL     '1'           *IN42
111900000000     C                   ENDIF
112000000000     C* EMISSIONE SUBFILE
112100000000     C     SFLULT        IFLE      0
112200000000     C*  --> CASO DI SFL VUOTO
112300000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
112400000000     C                   WRITE     TL17DVV                                      *'NON VI SONO DATI'
112500000000     C                   WRITE     TL17DVC                                      *SFL CONTROL
112600000000     C                   READ      TL17DVC                                99    *SFL CONTROL
112700000000     C                   ELSE
112800000000     C*  --> CASO DI SFL CON RECORDS
112900000000     C                   WRITE     TL17DVC                                      *SFL CONTROL
113000000000     C                   READ      TL17DVC                                99    *SFL CONTROL
113100000000     C                   ENDIF
113200000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
113300000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
113400000000     C                   Z-ADD     FRTSFL        VCCNBR
113500000000     C                   ENDSR
113600000000     C*------------------------------------------------------------------------*
113700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
113800000000     C*          CODA MESSAGGI DEL PGM
113900000000     C*------------------------------------------------------------------------*
114000000000     C     CARMSG        BEGSR
114100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
114200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
114300000000     C     MSGULT        IFEQ      0
114400000000     C                   MOVEL     '1'           *IN40
114500000000     C                   WRITE     TL17DVK
114600000000     C                   MOVEL     '0'           *IN40
114700000000     C                   ENDIF
114800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
114900000000     C                   MOVEL     $MSG          DSMSMS
115000000000     C                   CALL      'XMSG'        XMSGPA
115100000000     C     DSMSER        IFEQ      0
115200000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
115300000000     C                   ENDIF
115400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
115500000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
115600000000     C                   WRITE     TL17DVM                                      *SFL MESS
115700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
115800000000     C                   MOVEL     *BLANKS       $MSG
115900000000     C                   ENDSR
116000000000     C*------------------------------------------------------------------------*
116100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
116200000000     C*------------------------------------------------------------------------*
116300000000     C     OUTMSG        BEGSR
116400000000     C* VISUALIZZAZIONE MESSAGGIO
116500000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
116600000000     C                   WRITE     TL17DVK                                      *CONTR MESS
116700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
116800000000     C                   Z-ADD     *ZEROS        MSGULT
116900000000     C                   ENDSR
117000000000     C*------------------------------------------------------------------------*
117100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
117200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
117300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
117400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
117500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
117600000000     C*------------------------------------------------------------------------*
117700000000     C     CARFUN        BEGSR
117800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
117900000000     C     1             DO        31            I                 5 0          --- 1 -->
118000000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
118100000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
118200000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
118300020715     c                   If        DutLpo = 'S'
118400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
118500000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
118600000000     C                   ELSE                                                   *INTERROG.
118700000000     C                   MOVEL     FUNASI        FAT(I)
118800000000     C                   ENDIF
118900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
119000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
119100000000     C                   MOVEL     FUNAFG        FAT(I)
119200000000     C                   ELSE                                                   *INTERROG.
119300000000     C                   MOVEL     FUNAFI        FAT(I)
119400000000     C                   ENDIF
119500000000     C                   ENDIF                                                  <-- 2 ---
119600000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
119700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
119800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
119900000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
120000000000     C                   ELSE
120100000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
120200000000     C                   ENDIF
120300000000     C                   ENDIF
120400000000     C                   ENDDO                                                  <-- 1 ---
120500000000     C                   ENDSR
120600000000     C*------------------------------------------------------------------------*
120700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
120800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
120900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
121000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
121100000000     C*------------------------------------------------------------------------*
121200000000     C     OUTFUN        BEGSR
121300000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
121400000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
121500000000     C                   ENDDO
121600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
121700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
121800000000     C     1             DO        31            I
121900000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
122000000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
122100000000     C                   ADD       1             FUNNAT
122200000000     C                   ENDIF
122300000000     C                   ENDDO
122400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
122500000000     C     FUNNAT        IFGT      8
122600000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
122700000000     C                   ENDIF
122800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
122900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
123000000000     C                   Z-ADD     8             FUNVIS            5 0
123100000000     C     FAT(23)       IFEQ      'S'
123200000000     C                   SUB       1             FUNVIS
123300000000     C                   ENDIF
123400000000     C     FAT(24)       IFEQ      'S'
123500000000     C                   SUB       1             FUNVIS
123600000000     C                   ENDIF
123700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
123800000000     C                   Z-ADD     *ZEROS        W                 5 0
123900000000     C                   ADD       1             FUNULT
124000000000     C     FUNULT        IFGT      31
124100000000     C                   Z-ADD     1             FUNULT
124200000000     C                   ENDIF
124300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
124400000000     C     FUNULT        DO        31            I                              --- 1 -->
124500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
124600000000     C     W             IFLT      FUNVIS                                       --- 2 -->
124700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
124800000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
124900000000     C     FDE(I)        ANDNE     *BLANKS
125000000000     C     I             IFEQ      23                                           *TOLGO F23
125100000000     C     I             OREQ      24                                           *E F24
125200000000     C                   ELSE
125300000000     C                   ADD       1             W
125400000000     C                   MOVEL     FDE(I)        FUV(W)
125500000000     C                   ENDIF
125600000000     C                   ENDIF                                                  <-- 3 ---
125700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
125800000000     C                   Z-ADD     I             FUNULT
125900000000     C                   ENDIF                                                  <-- 2 ---
126000000000     C                   ENDDO                                                  <-- 1 ---
126100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
126200000000     C                   MOVEL     FUV(1)        VFD001
126300000000     C                   MOVEL     FUV(2)        VFD002
126400000000     C                   MOVEL     FUV(3)        VFD003
126500000000     C                   MOVEL     FUV(4)        VFD004
126600000000     C                   MOVEL     FUV(5)        VFD005
126700000000     C                   MOVEL     FUV(6)        VFD006
126800000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
126900000000     C     FAT(24)       ANDEQ     'N'
127000000000     C                   MOVEL     FUV(7)        VFD007
127100000000     C                   MOVEL     FUV(8)        VFD008
127200000000     C                   ELSE                                                   <-- 1 -->
127300000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
127400000000     C     FAT(24)       ANDEQ     'S'
127500000000     C                   MOVEL     FDE(23)       VFD007
127600000000     C                   MOVEL     FDE(24)       VFD008
127700000000     C                   ELSE                                                   <-- 2 -->
127800000000     C                   MOVEL     FUV(7)        VFD007
127900000000     C     FAT(23)       IFEQ      'S'
128000000000     C                   MOVEL     FDE(23)       VFD008
128100000000     C                   ELSE
128200000000     C                   MOVEL     FDE(24)       VFD008
128300000000     C                   ENDIF
128400000000     C                   ENDIF                                                  <-- 2 ---
128500000000     C                   ENDIF                                                  <-- 1 ---
128600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
128700000000     C                   ENDSR
128800000000     C*------------------------------------------------------------------------*
128900000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
129000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
129100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
129200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
129300000000     C*------------------------------------------------------------------------*
129400000000     C     CONFUN        BEGSR
129500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
129600000000     C     1             DO        31            I                              --- 1 -->
129700000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
129800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
129900000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
130000000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
130100000000     C                   EXSR      CARMSG
130200000000     C                   MOVEL     '1'           $ERR
130300000000     C                   ENDIF                                                  <-- 3 ---
130400000000     C                   ENDIF                                                  <-- 2 ---
130500000000     C                   ENDDO                                                  <-- 1 ---
130600000000     C                   ENDSR
130700000000     C*------------------------------------------------------------------------*
130800000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
130900000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
131000000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
131100000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
131200000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
131300000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
131400000000     C*------------------------------------------------------------------------*
131500000000     C     CAROPZ        BEGSR
131600000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
131700000000     C     1             DO        24            I                              --- 1 -->
131800000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
131900000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
132000000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
132100000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
132200020715     c                   If        DutLpo = 'S'
132300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
132400000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
132500000000     C                   ELSE                                                   *INTERROG.
132600000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
132700000000     C                   ENDIF
132800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
132900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
133000000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
133100000000     C                   ELSE                                                   *INTERROG.
133200000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
133300000000     C                   ENDIF
133400000000     C                   ENDIF                                                  <-- 2 ---
133500000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
133600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
133700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
133800000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
133900000000     C                   ELSE
134000000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
134100000000     C                   ENDIF
134200000000     C                   ENDIF
134300000000     C                   ENDDO                                                  <-- 1 ---
134400000000     C                   ENDSR
134500000000     C*------------------------------------------------------------------------*
134600000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
134700000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
134800000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
134900000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
135000000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
135100000000     C*------------------------------------------------------------------------*
135200000000     C     OUTOPZ        BEGSR
135300000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
135400000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
135500000000     C                   ENDDO
135600000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
135700000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
135800000000     C                   ADD       1             OPZULT
135900000000     C     OPZULT        IFGT      24
136000000000     C                   Z-ADD     1             OPZULT
136100000000     C                   END
136200000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
136300000000     C     1             DO        24            I
136400000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
136500000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
136600000000     C                   ADD       1             OPZNAT
136700000000     C                   ENDIF
136800000000     C                   ENDDO
136900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
137000000000     C                   Z-ADD     0             W
137100000000     C     OPZULT        DO        24            I                              --- 1 -->
137200000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
137300000000     C     W             IFLT      8                                            --- 2 -->
137400000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
137500000000     C     OAT(I)        IFEQ      'S'
137600000000     C     ODE(I)        ANDNE     *BLANKS
137700000000     C                   ADD       1             W
137800000000     C                   MOVEL     ODE(I)        OPV(W)
137900000000     C                   ENDIF
138000000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
138100000000     C                   Z-ADD     I             OPZULT
138200000000     C                   ENDIF                                                  <-- 2 ---
138300000000     C                   ENDDO                                                  <-- 1 ---
138400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
138500000000     C                   MOVEL     OPV(1)        VOD001
138600000000     C                   MOVEL     OPV(2)        VOD002
138700000000     C                   MOVEL     OPV(3)        VOD003
138800000000     C                   MOVEL     OPV(4)        VOD004
138900000000     C                   MOVEL     OPV(5)        VOD005
139000000000     C                   MOVEL     OPV(6)        VOD006
139100000000     C                   MOVEL     OPV(7)        VOD007
139200000000     C                   MOVEL     OPV(8)        VOD008
139300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
139400000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
139500000000     C     OPZNAT        IFGT      8
139600000000     C                   MOVEL     'S'           FAT(23)
139700000000     C                   ENDIF
139800000000     C                   ENDSR
139900000000     C*------------------------------------------------------------------------*
140000000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
140100000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
140200000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
140300000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
140400000000     C*------------------------------------------------------------------------*
140500000000     C     CONOPZ        BEGSR
140600000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
140700000000     C                   MOVEL     *BLANKS       OPZOK             1
140800000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
140900000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
141000000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
141100000000     C     OAT(I)        IFEQ      'S'
141200000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
141300000000     C                   MOVEL     'S'           OPZOK
141400000000     C                   ELSE
141500000000     C                   MOVEL     'N'           OPZOK             1
141600000000     C                   ENDIF
141700000000     C                   ENDIF                                                  <-- 2 ---
141800000000     C                   ENDDO                                                  <-- 1 ---
141900000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
142000000000     C     OPZOK         IFNE      'S'
142100000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
142200000000     C                   EXSR      CARMSG
142300000000     C                   MOVEL     '1'           $ERR
142400000000     C                   MOVEL     'N'           OPZOK             1
142500000000     C                   ENDIF
142600000000     C                   ENDSR
142700960226** MSG  (MESSAGGI)
142800000000TNTL17R- Tasto funzionale non valido.                                          1
142900000000TNTL17R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
143000000000TNTL17R- Opzione non valida.                                                   3
143100000000TNTL17R- Opzione abilitata ma non gestita. Avvertire CED.                      4
143200000000TNTL17R- Inizio elenco.                                                        5
143300000000TNTL17R- Fine elenco.                                                          6
143400000000TNTL17R- 7
143500000000TNTL17R- 8
143600000000TNTL17R- 9
143700000000TNTL17R- 10
143800000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
143900000000- 1-1 -1=Scelta           -    SS     SS
144000000000- 2-2 -                   -    NN     NN
144100000000- 3-3 -                   -    NN     NN
144200000000- 4-4 -                   -    NN     NN
144300000000- 5-5 -5=Visualizzazione  -    SS     SS
144400000000- 6-6 -                   -    NN     NN
144500000000- 7-7 -                   -    NN     NN
144600000000- 8-8 -                   -    NN     NN
144700000000- 9-9 -                   -    NN     NN
144800000000-10-  -                   -    NN     NN
144900000000-11-  -                   -    NN     NN
145000000000-12-  -                   -    NN     NN
145100000000-13-  -                   -    NN     NN
145200000000-14-  -                   -    NN     NN
145300000000-15-  -                   -    NN     NN
145400000000-16-  -                   -    NN     NN
145500000000-17-  -                   -    NN     NN
145600000000-18-  -                   -    NN     NN
145700000000-19-  -                   -    NN     NN
145800000000-20-  -                   -    NN     NN
145900000000-21-  -                   -    NN     NN
146000000000-22-  -                   -    NN     NN
146100000000-23-  -                   -    NN     NN
146200000000-24-  -                   -    NN     NN
146300000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
146400000000F1    -                   -    NN     NN
146500000000F2    -                   -    NN     NN
146600000000F3    -                   -    NN     NN
146700000000F4    -                   -    NN     NN
146800000000F5    -                   -    NN     NN
146900000000F6    -                   -    NN     NN
147000000000F7    -                   -    NN     NN
147100000000F8    -                   -    NN     NN
147200000000F9    -                   -    NN     NN
147300000000F10   -                   -    NN     NN
147400000000F11   -                   -    NN     NN
147500000000F12   -F12=Ritorno        -    SS     SS
147600000000F13   -                   -    NN     NN
147700000000F14   -                   -    NN     NN
147800000000F15   -                   -    NN     NN
147900000000F16   -                   -    NN     NN
148000000000F17   -                   -    NN     NN
148100000000F18   -                   -    NN     NN
148200000000F19   -                   -    NN     NN
148300000000F20   -                   -    NN     NN
148400000000F21   -                   -    NN     NN
148500000000F22   -                   -    NN     NN
148600000000F23   -F23=Altre opzioni  -    NN     NN
148700000000F24   -F24=Altri tasti    -    NN     NN
148800000000ENTER -                   -    SS     SS
148900000000ROLLUP-                   -    SS     SS
149000000000ROLLDN-                   -    SS     SS
149100000000HELP  -                   -    NN     NN
149200000000PRINT -                   -    NN     NN
149300000000BSPACE-                   -    NN     NN
149400000000CLEAR -                   -    NN     NN
149500960226** GSE  (GIORNI DELLA SETTIMANA)
149600960226LUNEDI'       1
149700960226MARTEDI'      2
149800960226MERCOLEDI'    3
149900960226GIOVEDI'      4
150000960226VENERDI'      5
150100960226SABATO        6
150200960226DOMENICA      7
150300960226TUTTI         8
