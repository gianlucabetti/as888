000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - NOTE - SUBFILE DI SCELTA
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL12D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL12DVS:SFLULT)
001000000000     F                                     SFILE(TL12DVM:MSGULT)
001100000000     FTNTLT01L  UF A E           K DISK
001200000000     F                                     COMMIT
001300000000     FTNTLN01L  UF A E           K DISK
001400000000     F                                     COMMIT
001500951130     FTNTLR01L  UF A E           K DISK
001600951130     F                                     COMMIT
001700951130     FAZORG01L  IF   E           K DISK
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100000000     D*-------------------
002200000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002300000000     D*-------------------
002400000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
002500000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
002600000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
002700000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
002800000000     D*-------------------
002900000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003000000000     D*-------------------
003100000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003200000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003300000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003400000000     D*-------------------
003500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003600000000     D*-------------------
003700000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
003800000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
003900000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
004000000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
004100000000     D*-------------------
004200000000     D* TABELLA DESCRIZIONI A VIDEO
004300000000     D*-------------------
004400000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004500000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
004600000000     D*-------------------
004700000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004800000000     D*-------------------
004900000000     D KEYS            S              1    DIM(40)
005000951130     D*-------------------
005100951130     D* FILIALI
005200951130     D*-------------------
005300971125     D ORG             S              3  0 DIM(500)                             *CODICE
005400971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
005500960424     D*-------------------
005600960424     D* SCHIERE ALFA/NUMERICHE DI LAVORO
005700960424     D*-------------------
005800960424     D SA0             S              1    DIM(10)                              *ALFANUMERICA 10
005900000000     D*------------------------------------------------------------------------*
006000000000     D* INPUT
006100000000     D*------------------------------------------------------------------------*
006200000000     D*---------------------
006300000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
006400000000     D*---------------------
006500000000     D DSOPZ         E DS
006600000000     D  OPZRIG                 1     40
006700000000     D*---------------------
006800000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006900000000     D*---------------------
007000000000     D DSFUN         E DS
007100000000     D  FUNRIG                 1     40
007200000000     D*---------------------
007300000000     D* DS PER GESTIONE TASTI FUNZIONALI
007400000000     D*---------------------
007500000000     D DSKEYS        E DS
007600000000     D  KEYRIG                 1     40
007700000000     D*---------------------
007800000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007900000000     D*---------------------
008000000000     D                SDS
008100000000     D  USER                 254    263
008200000000     D*---------------------
008300000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008400000000     D*---------------------
008500000000     D DEVDS1          DS
008600000000     D  KEY                  369    369
008700000000     D  CURSOR               370    371
008800000000     D  FRTSFL               378    379B 0
008900000000     D*---------------------
009000000000     D* ARCHITETTURA
009100000000     D*---------------------
009200000000     D KPJBA         E DS
009300000000     D*---------------------
009400000000     D* DS ESTERNA DI PROCEDURA
009500000000     D*---------------------
009600000000     D DS00          E DS                  EXTNAME(TNTL00DS)
009700000000     D*---------------------
009800000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
009900000000     D*---------------------
010000000000     D DSMSG         E DS
010100960208     D*---------------------
010200960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
010300960208     D*---------------------
010400960208     D OG148         E DS
010500020715
010600020715     d DdatiUte      e ds
010700020715     d AzuteDs       e ds                  ExtName(Azute00f)
010800020715     d Tibs34Ds      e ds                  Inz
010900020715
011000000000     D*------------------
011100000000     D* DS CONTROLLO DATA (8)
011200000000     D*------------------
011300000000     D WLBDA8          DS                  INZ
011400000000     D  G08DAT                 1      8  0
011500000000     D  G08INV                 9     16  0
011600000000     D  G08ERR                17     17
011700000000     D  G08TGI                18     22  0
011800000000     D*------------------
011900000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
012000000000     D*------------------
012100000000     D* MENU
012200000000     D DESGES          C                   CONST('** GESTIONE **')
012300000000     D DESINT          C                   CONST('INTERROGAZIONE')
012400000000     D* COMMENTI
012500000000     D DESATB          C                   CONST('  ANNULLATO ')
012600951130     D DESTFA          C                   CONST('ANDATA ')
012700951130     D DESTFR          C                   CONST('RITORNO')
012800000000     C*------------------------------------------------------------------------*
012900000000     C* MAIN LINE
013000000000     C*------------------------------------------------------------------------*
013100000000     C*
013200000000     C* CICLO FINO A CHE:
013300000000DO  1C     $FINE         DOUEQ     '1'
013400000000     C*
013500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
013600000000     C*
013700000000     C* CONTROLLA GESTIONE VIDEO
013800000000IF  2C     $GES          IFEQ      'VS'
013900000000     C                   EXSR      GESVS                                        *SUBFILE
014000000000E   2C                   ENDIF
014100000000     C*
014200000000E   1C                   ENDDO
014300000000     C*
014400000000IF  1C     $F03          IFEQ      '1'
014500000000     C                   MOVEL     '1'           D00F03
014600000000E   1C                   ENDIF
014700000000IF  1C     $F12          IFEQ      '1'
014800000000     C                   MOVEL     '1'           D00F12
014900000000E   1C                   ENDIF
015000000000     C* FINE PGM
015100000000     C                   MOVEL     *BLANKS       D00MSG
015200000000     C                   MOVEL     DS00          KPJBU
015300000000     C                   SETON                                        LR
015400000000     C*------------------------------------------------------------------------*
015500000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
015600000000     C*------------------------------------------------------------------------*
015700000000     C     CARVS         BEGSR
015800000000     C*
015900000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
016000000000     C*
016100000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
016200000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
016300000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
016400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016600000000     C*
016700000000     C                   WRITE     TL12DVT                                      *TESTATA
016800000000     C                   WRITE     TL12DVO                                      *OPZIONI
016900000000     C                   WRITE     TL12DVF                                      *FUNZIONI
017000000000     C*
017100000000     C                   EXSR      POSSFL                                       *CARICA SFL
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500000000     C* GESVS  - GESTIONE SUBFILE
017600000000     C*------------------------------------------------------------------------*
017700000000     C     GESVS         BEGSR
017800000000     C*
017900000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
018000000000     C                   WRITE     TL12DVT                                      *TESTATA
018100000000     C                   WRITE     TL12DVO                                      *OPZIONI
018200000000     C                   WRITE     TL12DVF                                      *FUNZIONI
018300000000E   1C                   ENDIF
018400000000     C*
018500000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
018600000000     C*
018700000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
018800000000     C*
018900000000IF  1C     $ERR          IFEQ      '0'
019000000000SL  2C                   SELECT
019100000000     C     KEY           WHENEQ    F10
019200000000     C                   EXSR      F10GES                                       *INSERIMENTO
019300000000     C                   OTHER
019400000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
019500000000E   2C                   ENDSL
019600000000E   1C                   ENDIF
019700000000     C*
019800000000     C                   ENDSR
019900000000     C*------------------------------------------------------------------------*
020000000000     C* F10GES - GESTIONE TASTO FUNZIONALE F10 ==> INSERIMENTO
020100000000     C*------------------------------------------------------------------------*
020200000000     C     F10GES        BEGSR
020300000000     C*
020400000000     C* LANCIA L'INSERIMENTO
020500000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
020600000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
020700000000     C*
020800951130     C                   Z-ADD     *ZEROS        WNFM                           *NUMERO FERMATA
020900000000     C                   Z-ADD     *ZEROS        WPNO                           *PROGRESSIVO NOTE
021000000000     C*
021100000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
021200000000     C                   MOVEL     'F10'         WOP1                           *LIVELLO PROGRAMMA
021300000000     C                   MOVEL     'TNTL13R'     WPGM              8            *PGM DI GESTIONE
021400000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
021500000000     C                   MOVEL     *BLANKS       VSCOPZ
021600000000     C*
021700000000     C                   EXSR      POSSFL
021800000000     C*
021900000000     C                   ENDSR
022000000000     C*------------------------------------------------------------------------*
022100000000     C* GESOPZ - GESTIONE OPZIONI
022200000000     C*------------------------------------------------------------------------*
022300000000     C     GESOPZ        BEGSR
022400000000     C*
022500000000SL  1C                   SELECT
022600000000     C     VSCOPZ        WHENEQ    ' 2'
022700000000     C                   EXSR      O02GES                                       *MODIFICA
022800000000     C     VSCOPZ        WHENEQ    ' 4'
022900000000     C                   EXSR      O04GES                                       *ANNULLAMENTO
023000000000     C     VSCOPZ        WHENEQ    ' 7'
023100000000     C                   EXSR      O07GES                                       *RIPRISTINA
023200000000     C                   OTHER
023300000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
023400000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
023500000000E   1C                   ENDSL
023600000000     C*
023700000000     C                   ENDSR
023800000000     C*------------------------------------------------------------------------*
023900000000     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
024000000000     C*------------------------------------------------------------------------*
024100000000     C     O02GES        BEGSR
024200000000     C*
024300000000IF  1C     VSCATB        IFNE      *BLANKS
024400000000     C                   MOVEL     '1'           $ERR
024500000000     C                   MOVEL     MSG(29)       $MSG                           *ANNULLATO
024600000000     C                   EXSR      CARMSG
024700000000E   1C                   ENDIF
024800000000     C*
024900000000     C* LANCIA LA MODIFICA
025000000000IF  1C     $ERR          IFEQ      '0'
025100000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
025200000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
025300000000     C*
025400951130     C                   Z-ADD     VSCNFM        WNFM                           *NUMERO FERMATA
025500951130     C                   Z-ADD     VSCPNO        WPNO                           *PROGRESSIVO FERMATA
025600000000     C*
025700000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
025800000000     C                   MOVEL     'O02'         WOP1                           *LIVELLO PROGRAMMA
025900000000     C                   MOVEL     'TNTL13R'     WPGM                           *ROUTINE MODIFICA
026000000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
026100000000     C*
026200000000E   1C                   ENDIF
026300951130     C                   MOVEL     '1'           $ERR
026400000000     C*
026500000000     C                   ENDSR
026600000000     C*------------------------------------------------------------------------*
026700000000     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
026800000000     C*------------------------------------------------------------------------*
026900000000     C     O04GES        BEGSR
027000000000     C*
027100000000IF  1C     VSCATB        IFNE      *BLANKS
027200000000     C                   MOVEL     '1'           $ERR
027300000000     C                   MOVEL     MSG(29)       $MSG                           *ANNULLATO
027400000000     C                   EXSR      CARMSG
027500000000E   1C                   ENDIF
027600000000     C*
027700000000     C* LANCIA L'ANNULLAMENTO
027800000000IF  1C     $ERR          IFEQ      '0'
027900000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
028000000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
028100000000     C*
028200951130     C                   Z-ADD     VSCNFM        WNFM                           *NUMERO FERMATA
028300951130     C                   Z-ADD     VSCPNO        WPNO                           *PROGRESSIVO FERMATA
028400000000     C*
028500000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
028600000000     C                   MOVEL     'O04'         WOP1                           *LIVELLO PROGRAMMA
028700000000     C                   MOVEL     'TNTL13R'     WPGM                           *ROUTINE CANCELLAZ.N
028800000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
028900000000     C*
029000000000E   1C                   ENDIF
029100951130     C                   MOVEL     '1'           $ERR
029200000000     C*
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
029600000000     C*------------------------------------------------------------------------*
029700000000     C     O07GES        BEGSR
029800000000     C*
029900000000IF  1C     VSCATB        IFEQ      *BLANKS
030000000000     C                   MOVEL     '1'           $ERR
030100000000     C                   MOVEL     MSG(30)       $MSG                           *NON ANNULLATO
030200000000     C                   EXSR      CARMSG
030300000000E   1C                   ENDIF
030400000000     C*
030500000000     C* LANCIA IL RIPRISTINO
030600000000IF  1C     $ERR          IFEQ      '0'
030700000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
030800000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
030900000000     C*
031000951130     C                   Z-ADD     VSCNFM        WNFM                           *NUMERO FERMATA
031100951130     C                   Z-ADD     VSCPNO        WPNO                           *PROGRESSIVO FERMATA
031200000000     C*
031300000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
031400000000     C                   MOVEL     'O07'         WOP1                           *LIVELLO PROGRAMMA
031500000000     C                   MOVEL     'TNTL13R'     WPGM                           *ROUTINE CANCELLAZ.N
031600000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
031700000000     C*
031800000000E   1C                   ENDIF
031900951130     C                   MOVEL     '1'           $ERR
032000000000     C*
032100000000     C                   ENDSR
032200000000     C*------------------------------------------------------------------------*
032300000000     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
032400000000     C*------------------------------------------------------------------------*
032500000000     C     NEWPOS        BEGSR
032600000000     C*
032700000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
032800000000     C*        1 = RICHIESTO POSIZIONAMENTO
032900000000     C                   Z-ADD     0             SWPOSI            7 0
033000000000     C*
033100000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
033200000000     C*            NON OCCORRE RICARICARE IL SUBFILE
033300000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
033400000000     C                   Z-ADD     0             SWMODI            1 0
033500000000     C*
033600000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLA I PARAMETRI
033700000000     C                   EXSR      CHKPAR
033800000000     C*
033900000000     C                   ENDSR
034000000000     C*------------------------------------------------------------------------*
034100000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
034200000000     C*------------------------------------------------------------------------*
034300000000     C     CHKPAR        BEGSR
034400000000     C*
034500000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
034600000000     C* CURSORE SUL CAMPO DEL CONTROL
034700000000IF  1C     $ERR          IFEQ      '1'
034800000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
034900000000X   1C                   ELSE
035000000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
035100000000E   1C                   ENDIF
035200000000     C*
035300000000     C                   Z-ADD     1             SWMODI                         *SI MODIFICA
035400000000     C*
035500000000     C                   ENDSR
035600000000     C*------------------------------------------------------------------------*
035700000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
035800000000     C*------------------------------------------------------------------------*
035900000000     C     SETFIL        BEGSR
036000000000     C*
036100000000     C                   MOVEL     '1'           *IN43                          *EMETTE SFILE
036200000000     C*
036300000000     C                   Z-ADD     D00TRN        KLNTRN
036400000000     C                   Z-ADD     D00DDE        KLNDDE
036500000000     C     KEYTLN        SETLL     TNTLN01L                           99        *FINE FILE
036600000000     C*
036700000000     C  N99              EXSR      LETFIL
036800000000     C*
036900000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
037000000000IF  1C     *IN99         IFEQ      *ON
037100000000     C                   Z-ADD     1             SFLEOF
037200000000E   1C                   ENDIF
037300000000     C*
037400000000     C                   ENDSR
037500000000     C*------------------------------------------------------------------------*
037600000000     C* LETFIL - LETTURA RECORD SUCESSIVO
037700000000     C*------------------------------------------------------------------------*
037800000000     C     LETFIL        BEGSR
037900000000     C*
038000000000     C* LEGGO FINO A:
038100000000     C*     - FINE FILE
038200000000     C*     - TROVATO RECORD VALIDO
038300000000     C                   MOVEL     'N'           WRECOK            1            *RECORD NON VALIDO
038400000000DO  1C     *IN99         DOWEQ     *OFF
038500000000     C     WRECOK        ANDEQ     'N'
038600000000     C     KEYTLN        READE(N)  TNTLN01L                               99
038700000000     C*
038800000000IF  2C     *IN99         IFEQ      *ON
038900000000     C                   Z-ADD     1             SFLEOF
039000000000X   2C                   ELSE
039100000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
039200000000E   2C                   ENDIF
039300000000     C*
039400000000E   1C                   ENDDO
039500000000     C*
039600000000     C                   ENDSR
039700000000     C*------------------------------------------------------------------------*
039800000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
039900000000     C*------------------------------------------------------------------------*
040000000000     C     CHKREC        BEGSR
040100000000     C*
040200000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
040300951130     C*
040400951130     C* RECORD ANNULLATI DA PROGRAMMA, ESCLUDE
040500951130IF  1C     TLNATB        IFEQ      '*'                                          *ANNULLATO DA PGM
040600951130     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
040700951130E   1C                   ENDIF
040800951130     C*
040900000000     C                   ENDSR
041000000000     C*------------------------------------------------------------------------*
041100000000     C* IRESFL - IMPOSTA I CAMPI DEL SUBFILE DAL FILE FISICO
041200000000     C*------------------------------------------------------------------------*
041300000000     C     IRESFL        BEGSR
041400951130     C*
041500951130     C                   Z-ADD     TLNNFM        VSCNFM                         *NUMERO FERMATA
041600000000     C                   MOVEL     *BLANKS       VSCOPZ                         *OPZIONE
041700000000     C                   MOVEL     TLNATB        VSCATB                         *ANNULLAMENTO
041800000000     C                   Z-ADD     TLNPNO        VSCPNO                         *PROGRESSIVO NOTE
041900000000     C                   MOVEL     TLNTNO        VSCTNO                         *TESTO NOTE
042000951130     C*
042100951130     C* LEGGE LA FERMATA ALLA QUALE LA NOTA SI RIFERISCE
042200951130IF  1C     TLNNFM        IFGT      *ZEROS                                       *NOTA DI FERMATA
042300960206     C*---                 MOVEL*BLANKS   VSDTFM           *TIPO FERMATA
042400951130     C                   Z-ADD     *ZEROS        VSCFIL                         *FILIALE FERMATA
042500951130     C                   Z-ADD     D00TRN        KLRTRN
042600951130     C                   Z-ADD     D00DDE        KLRDDE
042700951130     C                   Z-ADD     TLNNFM        KLRNFM
042800951130     C     KEYTLR        CHAIN(N)  TNTLR01L                           98
042900951130IF  2C     *IN98         IFEQ      *OFF
043000960206     C*---                 MOVELTLRTFM    VSDTFM           *TIPO FERMATA
043100951130     C                   Z-ADD     TLRFIL        VSCFIL                         *FILIALE FERMATA
043200951130E   2C                   ENDIF
043300960206     C*--- DECODIFICHE
043400960206     C*---                 Z-ADD1         J       40
043500960206     C*---                 SETOF                         28
043600960206     C*---       VSCFIL    LOKUPORG,J                    28
043700960206IF  2C*---       VSCFIL    IFGT *ZEROS
043800960206     C*---       *IN28     ANDEQ*ON
043900960206     C*---                 MOVELDORG,J    VSDFIL           *FILIALE FERMATA
044000960206E   2C*---                 ENDIF
044100960206     C*---
044200960206IF  2C*---       VSDTFM    IFNE *BLANKS
044300960206IF  3C*---       TLRTFM    IFEQ 'A'
044400960206     C*---                 MOVELDESTFA    VSDTFM           *FERMATA ANDATA
044500960206X   3C*---                 ELSE
044600960206     C*---                 MOVELDESTFR    VSDTFM           *FERMATA RITORNO
044700960206E   3C*---                 ENDIF
044800960206E   2C*---                 ENDIF
044900960206E   1C                   ENDIF
045000951130     C*
045100000000     C                   ENDSR
045200000000     C*------------------------------------------------------------------------*
045300000000     C* LANPGM - LANCIA UN PROGRAMMA
045400000000     C*------------------------------------------------------------------------*
045500000000     C     LANPGM        BEGSR
045600000000     C***
045700000000     C* PARAMETRI INFORMATIVI
045800000000     C***
045900000000     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
046000000000     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PROGRAMMA
046100000000     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
046200000000     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
046300000000     C                   MOVEL     '0'           D00ERR                         *ERRORE
046400000000     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
046500000000     C***
046600000000     C* PARAMETRI
046700000000     C***
046800000000     C                   Z-ADD     WTRN          D00TRN                         *TRAINO
046900000000     C                   Z-ADD     WDDE          D00DDE                         *DECORRENZA (AA/M/G)
047000000000     C                   Z-ADD     WNFM          D00NFM                         *NUMERO FERMATA
047100000000     C                   Z-ADD     WPNO          D00PNO                         *PROGRESSIVO NOTE
047200000000     C*--------------------
047300000000     C* L A N C I O
047400000000     C*--------------------
047500000000     C                   MOVEL     DS00          KPJBU
047600000000     C                   CALL      WPGM                                 99
047700000000     C                   PARM                    KPJBA
047800000000     C                   MOVEL     KPJBU         DS00
047900000000     C*
048000000000IF  1C     D00F03        IFEQ      '1'
048100000000     C                   MOVEL     '1'           $F03
048200000000     C                   MOVEL     '1'           $FINE
048300000000E   1C                   ENDIF
048400000000IF  1C     D00F12        IFEQ      '1'
048500000000     C                   MOVEL     '1'           $F12
048600000000E   1C                   ENDIF
048700000000     C*
048800000000     C                   MOVEL     D00MSG        $MSG
048900000000     C                   EXSR      CARMSG
049000000000     C*
049100000000     C                   ENDSR
049200000000     C*------------------------------------------------------------------------*
049300000000     C* IMPVID - IMPOSTAZIONI VIDEO
049400000000     C*------------------------------------------------------------------------*
049500000000     C     IMPVID        BEGSR
049600000000     C*
049700000000     C* CALCOLA LA DATA CORRENTE
049800000000     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
049900000000     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
050000000000     C                   Z-ADD     WNUM8         G08DAT
050100000000     C                   Z-ADD     *ZEROS        G08INV
050200000000     C                   MOVEL     '0'           G08ERR
050300000000     C                   CALL      'XSRDA8'
050400000000     C                   PARM                    WLBDA8
050500000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
050600000000     C* DATI TRAINO
050700000000     C                   Z-ADD     D00TRN        VCCTRN                         *TRAINO
050800000000     C                   Z-ADD     D00DDE        G08INV
050900000000     C                   Z-ADD     *ZEROS        G08DAT
051000000000     C                   MOVEL     '3'           G08ERR
051100000000     C                   CALL      'XSRDA8'
051200000000     C                   PARM                    WLBDA8
051300000000     C                   Z-ADD     G08DAT        VCCDDE                         *DATA DECORRENZA
051400000000     C* LEGGE IL TRAINO
051500000000     C                   Z-ADD     D00TRN        KLTTRN
051600000000     C                   Z-ADD     D00DDE        KLTDDE
051700000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
051800000000     C                   Z-ADD     TLTDSC        G08INV
051900000000     C                   Z-ADD     *ZEROS        G08DAT
052000000000     C                   MOVEL     '3'           G08ERR
052100000000     C                   CALL      'XSRDA8'
052200000000     C                   PARM                    WLBDA8
052300000000     C                   Z-ADD     G08DAT        VCCDSC                         *DATA SCADENEZA
052400000000     C                   Z-ADD     TLTTFP        VCCTFP                         *TERMINAL PARTENZA
052500000000     C                   Z-ADD     TLTTFA        VCCTFA                         *TERMINAL PARTENZA
052600960424     C*
052700960424     C* SE TIPO TRAINO INOLTRO METTE NELLA DESCRIZIONE IL TIPO TRAINO ASSOCIATO
052800960424IF  1C     TLTTTR        IFEQ      'I'
052900960424     C                   CLEAR                   SA0
053000960424     C                   MOVEA     'INOLTRO'     SA0
053100960424     C                   MOVEL     '('           SA0(8)
053200960424     C                   MOVEL     D00INL        SA0(9)
053300960424     C                   MOVEL     ')'           SA0(10)
053400960424     C                   MOVEA     SA0           VCCTTR
053500960424E   1C                   ENDIF
053600000000     C*
053700000000     C* EMETTE UN MESSAGGIO SE I LISTINI SONO ANNULLATI (UNICO CASO QUANDO
053800000000     C* ARRIVA DALL'INTERROGAZIONE DI UN TRAINO ANNULLATO)
053900000000     C                   MOVEL     *BLANKS       VCCATB                         *RECORD NON ANNULLAT
054000000000     C                   MOVEL     '0'           *IN48                          *NON VISUALIZZATO
054100000000IF  1C     TLTATB        IFNE      *BLANKS
054200000000     C                   MOVEL     DESATB        VCCATB                         *RECORD ANNULLATO
054300000000     C                   MOVEL     '1'           *IN48                          *VISUALIZZATO
054400000000E   1C                   ENDIF
054500000000     C*
054600000000     C                   ENDSR
054700000000     C*------------------------------------------------------------------------*
054800000000     C* CARTAB - CARICA LE TABELLE
054900000000     C*------------------------------------------------------------------------*
055000000000     C     CARTAB        BEGSR
055100951130     C*--------------------
055200951130     C* FILIALI
055300951130     C*--------------------
055400951130     C                   Z-ADD     *ZEROS        J                 4 0
055500951130     C     *LOVAL        SETLL     AZORG01L
055600951130     C                   READ      AZORG01L                               99
055700951130DO  1C     *IN99         DOWEQ     *OFF
055800951130IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
055900960208     C                   MOVEL     ORGDE8        OG148
056000960208IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
056100951130     C                   ADD       1             J
056200951130     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
056300951130     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
056400960208E   3C                   ENDIF
056500960208E   2C                   ENDIF
056600951130     C                   READ      AZORG01L                               99
056700951130E   1C                   ENDDO
056800951130     C*
056900000000     C                   ENDSR
057000000000     C*------------------------------------------------------------------------*
057100000000     C* *INZSR - ROUTINE INIZIALE
057200000000     C*------------------------------------------------------------------------*
057300000000     C     *INZSR        BEGSR
057400000000     C*--------------------
057500000000     C* RICEVIMENTO PARAMETRI
057600000000     C*--------------------
057700000000     C     *ENTRY        PLIST
057800000000     C                   PARM                    KPJBA
057900000000     C                   MOVEL     KPJBU         DS00
058000000000     C*--------------------
058100000000     C* VARIABILI RIFERITE AL DATA BASE
058200000000     C*--------------------
058300000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
058400000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
058500951130     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
058600951130     C     *LIKE         DEFINE    TLRDDE        KLRDDE
058700951130     C     *LIKE         DEFINE    TLRNFM        KLRNFM
058800000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
058900000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
059000000000     C     *LIKE         DEFINE    D00OP0        WOP0                           *DS00
059100000000     C     *LIKE         DEFINE    D00OP1        WOP1
059200000000     C     *LIKE         DEFINE    D00F03        WF03
059300000000     C     *LIKE         DEFINE    D00F12        WF12
059400000000     C     *LIKE         DEFINE    D00ERR        WERR
059500000000     C     *LIKE         DEFINE    D00MSG        WMSG
059600000000     C     *LIKE         DEFINE    D00TRN        WTRN
059700000000     C     *LIKE         DEFINE    D00DDE        WDDE
059800951130     C     *LIKE         DEFINE    D00NFM        WNFM
059900000000     C     *LIKE         DEFINE    D00PNO        WPNO
060000000000     C*--------------------
060100000000     C* CHIAVI DI LETTURA
060200000000     C*--------------------
060300000000     C* LETTURA TNTLT01L - COMPLETA
060400000000     C     KEYTLT        KLIST
060500000000     C                   KFLD                    KLTTRN                         *TRAINO
060600000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
060700951130     C* LETTURA TNTLT04L - COMPLETA
060800951130     C     KEYTLR        KLIST
060900951130     C                   KFLD                    KLRTRN                         *TRAINO
061000951130     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
061100951130     C                   KFLD                    KLRNFM                         *NUMERO FERMATA
061200000000     C* LETTURA TNTLN01L - PARZIALE
061300000000     C     KEYTLN        KLIST
061400000000     C                   KFLD                    KLNTRN                         *TRAINO
061500000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
061600000000     C*--------------------
061700000000     C* VARIABILI DI CONTROLLO
061800000000     C*--------------------
061900000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
062000000000     C***
062100000000     C* IMPOSTAZIONI VIDEO
062200000000     C***
062300000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
062400000000     C                   MOVE      D00OP0        INTGES            2
062500000000IF  1C     INTGES        IFEQ      '02'
062600000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
062700000000     C                   MOVEL     DESGES        VTDOPE                         *DESCR OPERAZIONE
062800000000X   1C                   ELSE
062900000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
063000000000     C                   MOVEL     DESINT        VTDOPE                         *DESCR OPERAZIONE
063100000000E   1C                   ENDIF
063200000000     C*
063300000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
063400000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
063500000000IF  1C     *IN99         IFEQ      *ON
063600000000     C     *IN99         ANDEQ     *OFF
063700000000     C* TRAINI
063800000000     C                   UPDATE    TNTLT000
063900000000     C                   WRITE     TNTLT000
064000951130     C* FERMATE
064100951130     C                   UPDATE    TNTLR000
064200951130     C                   WRITE     TNTLR000
064300000000     C* NOTE
064400000000     C                   UPDATE    TNTLN000
064500000000     C                   WRITE     TNTLN000
064600000000E   1C                   ENDIF
064700000000     C*
064800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
064900000000     C                   EXSR      INZSTD
065000000000     C*
065100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
065200000000     C                   EXSR      INZSFL
065300000000     C*
065400000000     C* IMPOSTAZIONE INIZIALE VIDEO
065500000000     C                   EXSR      IMPVID
065600000000     C*
065700000000     C* CARICA LE TABELLE
065800000000     C                   EXSR      CARTAB
065900000000     C*
066000000000     C* EMETTE VIDEATA SUBFILE
066100000000     C                   EXSR      CHKPAR
066200000000     C                   EXSR      CARVS
066300000000     C*
066400000000     C                   ENDSR
066500000000     C*------------------------------------------------------------------------*
066600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
066900000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
067000000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
067100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067300000000     C*------------------------------------------------------------------------*
067400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
067500000000     C*------------------------------------------------------------------------*
067600000000     C     INZSTD        BEGSR
067700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
067800000000     C                   CALL      'XKEYS'
067900000000     C                   PARM                    DSKEYS
068000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
068100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
068200000000     C     XMSGPA        PLIST
068300000000     C                   PARM                    DSMSMS
068400000000     C                   PARM                    DSMSPG
068500000000     C                   PARM                    DSMSKE
068600000000     C                   PARM                    DSMSER
068700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
068800000000     C                   MOVEL     'TNTL12R'     DSMSPG                         *NOME PGM CHIAMANTE
068900000000     C                   MOVEL     'TNTL12R'     VMCQUE                         *CODA MESSAGGI DESTI
069000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
069100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
069200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
069300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
069400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
069500000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F03
069600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
069700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
069800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
069900000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
070000000000     C* LANCIO ROUTINE PER VERIFICARE SE
070100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
070200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
070300020715
070400020715     c     *dtaara       define    §azute        azuteds
070500020715     c     *dtaara       define    §datiute      ddatiute
070600020715
070700020715     c                   in(E)     *dtaara
070800020715     c                   if        %Error or rsut = *blanks
070900020715     c                   clear                   tibs34ds
071000020715     c                   call      'TIBS34R'
071100020715     c                   parm                    Tibs34Ds
071200020715     c                   in        *dtaara
071300020715     c                   endif
071400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
071500000000     C                   BITOFF    '01234567'    POSMI1            1
071600000000     C                   BITOFF    '01234567'    POSMI2            1
071700000000     C                   BITOFF    '01234567'    POSMA1            1
071800000000     C                   BITOFF    '01234567'    POSMA2            1
071900000000     C                   BITON     '34'          POSMI1            1            *18
072000000000     C                   BITON     '7'           POSMI2            1            *01
072100000000     C                   BITON     '34'          POSMA1            1            *18
072200000000     C                   BITON     '0'           POSMA2            1            *80
072300000000     C                   MOVE      POSMI2        POSMIN            2
072400000000     C                   MOVEL     POSMI1        POSMIN
072500000000     C                   MOVE      POSMA2        POSMAX            2
072600000000     C                   MOVEL     POSMA1        POSMAX
072700000000     C                   ENDSR
072800000000     C*------------------------------------------------------------------------*
072900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
073000000000     C*------------------------------------------------------------------------*
073100000000     C     FUNSTD        BEGSR
073200000000     C                   SELECT
073300000000     C     KEY           WHENEQ    F3
073400000000     C                   EXSR      F03STD
073500000000     C     KEY           WHENEQ    F12
073600000000     C                   EXSR      F12STD
073700000000     C     KEY           WHENEQ    F24
073800000000     C                   EXSR      F24STD
073900000000     C                   OTHER
074000000000     C                   MOVEL     MSG(2)        $MSG
074100000000     C                   EXSR      CARMSG
074200000000     C                   ENDSL
074300000000     C                   ENDSR
074400000000     C*------------------------------------------------------------------------*
074500000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
074600000000     C*------------------------------------------------------------------------*
074700000000     C     F03STD        BEGSR
074800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
074900000000     C                   MOVEL     '1'           $F03
075000000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
075100000000     C                   MOVEL     '1'           $FINE
075200951212     C* PULISCE IL MESSAGGIO
075300951212     C                   MOVEL     *BLANKS       D00MSG
075400000000     C                   ENDSR
075500000000     C*------------------------------------------------------------------------*
075600000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
075700000000     C*------------------------------------------------------------------------*
075800000000     C     F12STD        BEGSR
075900000000     C*
076000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
076100000000     C                   MOVEL     '1'           $F12
076200000000     C* IMPOSTO FLAG DI FIN PGM
076300000000     C                   MOVEL     '1'           $FINE
076400951212     C* PULISCE IL MESSAGGIO
076500951212     C                   MOVEL     *BLANKS       D00MSG
076600000000     C                   ENDSR
076700000000     C*------------------------------------------------------------------------*
076800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
076900000000     C*------------------------------------------------------------------------*
077000000000     C     F24STD        BEGSR
077100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
077200000000     C                   EXSR      OUTFUN
077300000000     C                   ENDSR
077400000000     C*------------------------------------------------------------------------*
077500000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
077600000000     C*------------------------------------------------------------------------*
077700000000     C     INZSFL        BEGSR
077800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
077900000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
078000000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
078100000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
078200000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
078300000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
078400000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
078500000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
078600000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
078700951130     C                   Z-ADD     08            SFLPAG
078800000000     C                   ENDSR
078900000000     C*------------------------------------------------------------------------*
079000000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
079100000000     C*------------------------------------------------------------------------*
079200000000     C     FUNSFL        BEGSR
079300000000     C                   SELECT
079400000000     C     KEY           WHENEQ    ENTER
079500000000     C                   EXSR      ENTSFL
079600000000     C     KEY           WHENEQ    F3
079700000000     C                   EXSR      F03STD
079800000000     C     KEY           WHENEQ    F5
079900000000     C                   EXSR      F05STD
080000000000     C     KEY           WHENEQ    F12
080100000000     C                   EXSR      F12STD
080200000000     C     KEY           WHENEQ    F17
080300000000     C                   EXSR      F17STD
080400000000     C     KEY           WHENEQ    F18
080500000000     C                   EXSR      F18STD
080600000000     C     KEY           WHENEQ    F23
080700000000     C                   EXSR      F23STD
080800000000     C     KEY           WHENEQ    F24
080900000000     C                   EXSR      F24STD
081000000000     C     KEY           WHENEQ    ROLLUP
081100000000     C                   EXSR      RUPSTD
081200000000     C     KEY           WHENEQ    ROLLDN
081300000000     C                   EXSR      RDNSTD
081400000000     C                   OTHER
081500000000     C                   MOVEL     MSG(2)        $MSG
081600000000     C                   EXSR      CARMSG
081700000000     C                   ENDSL
081800000000     C                   ENDSR
081900000000     C*------------------------------------------------------------------------*
082000000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
082100000000     C*------------------------------------------------------------------------*
082200000000     C     F05STD        BEGSR
082300000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
082400000000     C                   EXSR      POSSFL
082500000000     C                   ENDSR
082600000000     C*------------------------------------------------------------------------*
082700000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
082800000000     C*------------------------------------------------------------------------*
082900000000     C     F17STD        BEGSR
083000000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
083100000000     C                   Z-ADD     1             VCCNBR
083200000000     C                   ENDSR
083300000000     C*------------------------------------------------------------------------*
083400000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
083500000000     C*------------------------------------------------------------------------*
083600000000     C     F18STD        BEGSR
083700000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
083800000000     C                   Z-ADD     SFLULT        VCCNBR
083900000000     C                   ENDSR
084000000000     C*------------------------------------------------------------------------*
084100000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
084200000000     C*------------------------------------------------------------------------*
084300000000     C     F23STD        BEGSR
084400000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
084500000000     C                   EXSR      OUTOPZ
084600000000     C                   ENDSR
084700000000     C*------------------------------------------------------------------------*
084800000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
084900000000     C*------------------------------------------------------------------------*
085000000000     C     RUPSTD        BEGSR
085100000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
085200000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
085300000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
085400000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
085500000000     C                   ELSE
085600000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
085700000000     C                   ENDIF
085800000000     C                   ENDSR
085900000000     C*------------------------------------------------------------------------*
086000000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
086100000000     C*------------------------------------------------------------------------*
086200000000     C     RDNSTD        BEGSR
086300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
086400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
086500000000     C     CURSOR        IFGE      POSMIN
086600000000     C     CURSOR        ANDLE     POSMAX
086700000000     C                   ELSE
086800000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
086900000000     C                   MOVEL     '1'           $ERR
087000000000     C                   MOVEL     MSG(5)        $MSG
087100000000     C                   EXSR      CARMSG
087200000000     C                   ENDIF
087300000000     C                   ENDSR
087400000000     C*------------------------------------------------------------------------*
087500000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
087600000000     C*------------------------------------------------------------------------*
087700000000     C     ENTSFL        BEGSR
087800000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
087900000000     C                   EXSR      NEWPOS
088000000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
088100000000     C     $ERR          IFEQ      '0'                                          --- 1 -->
088200000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
088300000000     C     SWPOSI        IFNE      0                                            --- 2 -->
088400000000     C                   EXSR      POSSFL
088500000000     C                   ELSE                                                   <-- 2 -->
088600000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
088700000000     C     SFLULT        IFNE      0                                            --- 3 -->
088800000000     C                   EXSR      OPZSFL
088900000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
089000000000     C     SWMODI        IFNE      0                                            --- 4 -->
089100000000     C                   EXSR      POSSFL
089200000000     C                   ENDIF                                                  <-- 4 ---
089300000000     C                   ENDIF                                                  <-- 3 ---
089400000000     C                   ENDIF                                                  <-- 2 ---
089500000000     C                   ENDIF                                                  <-- 1 ---
089600000000     C                   ENDSR
089700000000     C*------------------------------------------------------------------------*
089800000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
089900000000     C*------------------------------------------------------------------------*
090000000000     C     POSSFL        BEGSR
090100000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
090200000000     C                   EXSR      CLRSFL
090300000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
090400000000     C                   EXSR      SETFIL
090500000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
090600000000     C     SFLEOF        IFEQ      0
090700000000     C                   EXSR      CARSFL
090800000000     C                   ENDIF
090900000000     C                   ENDSR
091000000000     C*------------------------------------------------------------------------*
091100000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
091200000000     C*------------------------------------------------------------------------*
091300000000     C     OPZSFL        BEGSR
091400000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
091500000000     C                   MOVEL     'S'           OPZOK             1
091600000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
091700000000     C                   Z-ADD     0             SFLVAR            5 0
091800000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
091900000000     C                   Z-ADD     0             SFLERR
092000000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
092100000000     C                   Z-ADD     SFLULT        WFLULT
092200000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
092300000000     C                   READC     TL12DVS                                49
092400000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
092500000000     C     $ERR          ANDEQ     '0'
092600000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
092700000000     C                   ADD       1             SFLVAR
092800000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
092900000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
093000000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
093100000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
093200000000     C                   EXSR      CONOPZ
093300000000     C     OPZOK         IFEQ      'S'
093400000000     C     $ERR          ANDEQ     '0'
093500000000     C                   EXSR      GESOPZ
093600000000     C                   ENDIF
093700000000     C                   ENDIF                                                  <-- 3 ---
093800000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
093900000000     C*            2. PULIZIA SCELTA IN SFL
094000000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
094100000000     C                   MOVEL     '0'           *IN41
094200000000     C                   MOVEL     *BLANKS       VSCOPZ
094300000000     C                   ELSE                                                   <-- 3 -->
094400000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
094500000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
094600000000     C                   MOVEL     '1'           *IN41
094700000000     C                   Z-ADD     SFLULT        SFLERR
094800000000     C                   ENDIF                                                  <-- 3 ---
094900000000     C                   ELSE                                                   <-- 2 -->
095000000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
095100000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
095200000000     C                   MOVEL     '1'           *IN41
095300000000     C                   ENDIF                                                  <-- 2 ---
095400000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
095500000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
095600000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
095700000000     C     OPZOK         IFNE      'S'
095800000000     C                   Z-ADD     SFLERR        VCCNBR
095900000000     C                   ELSE
096000000000     C                   Z-ADD     SFLULT        VCCNBR
096100000000     C                   ENDIF
096200000000     C* AGGIORNO RECORD SFL
096300000000     C                   UPDATE    TL12DVS
096400000000     C     $ERR          IFEQ      '0'
096500000000     C                   READC     TL12DVS                                49
096600000000     C                   ENDIF
096700000000     C                   ENDDO                                                  <-- 1 ---
096800000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
096900000000     C                   Z-ADD     WFLULT        SFLULT
097000000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
097100000000     C     SFLVAR        IFEQ      0
097200000000     C                   MOVEL     '1'           $FINE
097300000000     C                   ENDIF
097400000000     C                   ENDSR
097500000000     C*------------------------------------------------------------------------*
097600000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
097700000000     C*------------------------------------------------------------------------*
097800000000     C     CLRSFL        BEGSR
097900000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
098000000000     C* (0=NON FINE FILE  1=FINE FILE)
098100000000     C                   Z-ADD     0             SFLEOF
098200000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
098300000000     C                   Z-ADD     0             SFLULT
098400000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
098500000000     C                   MOVEL     '1'           *IN40
098600000000     C                   WRITE     TL12DVC
098700000000     C                   MOVEL     '0'           *IN40
098800000000     C                   ENDSR
098900000000     C*------------------------------------------------------------------------*
099000000000     C* CARSFL - CARICAMENTO SFL
099100000000     C*------------------------------------------------------------------------*
099200000000     C     CARSFL        BEGSR
099300000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
099400000000     C     SFLULT        IFEQ      0
099500000000     C                   EXSR      SETFIL
099600000000     C                   ENDIF
099700000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
099800000000     C                   Z-ADD     SFLULT        WFLULT
099900000000     C* CICLO:
100000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
100100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
100200000000     C                   Z-ADD     1             SFLRIG
100300000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
100400000000     C     SFLEOF        ANDEQ     0
100500000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
100600000000     C                   MOVEL     *BLANKS       VSCOPZ
100700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
100800000000     C                   EXSR      IRESFL
100900000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
101000000000     C                   ADD       1             SFLULT
101100000000     C* SCRITTURA RECORD SFL
101200000000     C                   WRITE     TL12DVS
101300000000     C                   ADD       1             SFLRIG
101400000000     C* LETTURA FILE DA CARICARE
101500000000     C                   EXSR      LETFIL
101600000000     C                   ENDDO                                                  <-- 1 ---
101700000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
101800000000     C     WFLULT        IFNE      SFLULT
101900000000     C                   Z-ADD     WFLULT        VCCNBR
102000000000     C                   ADD       1             VCCNBR
102100000000     C                   ENDIF
102200000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
102300000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
102400000000     C     SFLULT        ANDGT     *ZEROS
102500000000     C                   MOVEL     '1'           $ERR
102600000000     C                   MOVEL     MSG(6)        $MSG
102700000000     C                   EXSR      CARMSG
102800000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
102900000000     C     SFLULT        CHAIN     TL12DVS                            99
103000000000     C     *IN99         IFEQ      '0'
103100000000     C                   SETON                                        45
103200000000     C                   UPDATE    TL12DVS
103300000000     C                   SETOFF                                       45
103400000000     C                   ENDIF
103500000000     C                   ENDIF                                                  <-- 1 ---
103600000000     C                   ENDSR
103700000000     C*------------------------------------------------------------------------*
103800000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
103900000000     C*------------------------------------------------------------------------*
104000000000     C     OUTSFL        BEGSR
104100000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
104200000000     C     MSGULT        IFNE      *ZEROS
104300000000     C                   EXSR      OUTMSG
104400000000     C                   ENDIF
104500000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
104600000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
104700000000     C     SFLEOF        IFEQ      0
104800000000     C                   MOVEL     '0'           *IN42
104900000000     C                   ELSE
105000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
105100000000     C                   MOVEL     '1'           *IN42
105200000000     C                   ENDIF
105300000000     C* EMISSIONE SUBFILE
105400000000     C     SFLULT        IFLE      0
105500000000     C*  --> CASO DI SFL VUOTO
105600000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
105700000000     C                   WRITE     TL12DVV                                      *'NON VI SONO DATI'
105800000000     C                   WRITE     TL12DVC                                      *SFL CONTROL
105900000000     C                   READ      TL12DVC                                99    *SFL CONTROL
106000000000     C                   ELSE
106100000000     C*  --> CASO DI SFL CON RECORDS
106200000000     C                   WRITE     TL12DVC                                      *SFL CONTROL
106300000000     C                   READ      TL12DVC                                99    *SFL CONTROL
106400000000     C                   ENDIF
106500000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
106600000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
106700000000     C                   Z-ADD     FRTSFL        VCCNBR
106800000000     C                   ENDSR
106900000000     C*------------------------------------------------------------------------*
107000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
107100000000     C*          CODA MESSAGGI DEL PGM
107200000000     C*------------------------------------------------------------------------*
107300000000     C     CARMSG        BEGSR
107400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
107500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
107600000000     C     MSGULT        IFEQ      0
107700000000     C                   MOVEL     '1'           *IN40
107800000000     C                   WRITE     TL12DVK
107900000000     C                   MOVEL     '0'           *IN40
108000000000     C                   ENDIF
108100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
108200000000     C                   MOVEL     $MSG          DSMSMS
108300000000     C                   CALL      'XMSG'        XMSGPA
108400000000     C     DSMSER        IFEQ      0
108500000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
108600000000     C                   ENDIF
108700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
108800000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
108900000000     C                   WRITE     TL12DVM                                      *SFL MESS
109000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
109100000000     C                   MOVEL     *BLANKS       $MSG
109200000000     C                   ENDSR
109300000000     C*------------------------------------------------------------------------*
109400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
109500000000     C*------------------------------------------------------------------------*
109600000000     C     OUTMSG        BEGSR
109700000000     C* VISUALIZZAZIONE MESSAGGIO
109800000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
109900000000     C                   WRITE     TL12DVK                                      *CONTR MESS
110000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
110100000000     C                   Z-ADD     *ZEROS        MSGULT
110200000000     C                   ENDSR
110300000000     C*------------------------------------------------------------------------*
110400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
110500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
110600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
110700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
110800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
110900000000     C*------------------------------------------------------------------------*
111000000000     C     CARFUN        BEGSR
111100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
111200000000     C     1             DO        31            I                 5 0          --- 1 -->
111300000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
111400000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
111500000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
111600020715     c                   If        DutLpo = 'S'
111700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
111800000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
111900000000     C                   ELSE                                                   *INTERROG.
112000000000     C                   MOVEL     FUNASI        FAT(I)
112100000000     C                   ENDIF
112200000000     C                   ELSE                                                   <-- 2 --> *FILIALE
112300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
112400000000     C                   MOVEL     FUNAFG        FAT(I)
112500000000     C                   ELSE                                                   *INTERROG.
112600000000     C                   MOVEL     FUNAFI        FAT(I)
112700000000     C                   ENDIF
112800000000     C                   ENDIF                                                  <-- 2 ---
112900000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
113000000000     C                   MOVEL     USER          WEDP              3            *PROFILO
113100000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
113200000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
113300000000     C                   ELSE
113400000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
113500000000     C                   ENDIF
113600000000     C                   ENDIF
113700000000     C                   ENDDO                                                  <-- 1 ---
113800000000     C                   ENDSR
113900000000     C*------------------------------------------------------------------------*
114000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
114100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
114200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
114300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
114400000000     C*------------------------------------------------------------------------*
114500000000     C     OUTFUN        BEGSR
114600000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
114700000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
114800000000     C                   ENDDO
114900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
115000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
115100000000     C     1             DO        31            I
115200000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
115300000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
115400000000     C                   ADD       1             FUNNAT
115500000000     C                   ENDIF
115600000000     C                   ENDDO
115700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
115800000000     C     FUNNAT        IFGT      8
115900000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
116000000000     C                   ENDIF
116100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
116200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
116300000000     C                   Z-ADD     8             FUNVIS            5 0
116400000000     C     FAT(23)       IFEQ      'S'
116500000000     C                   SUB       1             FUNVIS
116600000000     C                   ENDIF
116700000000     C     FAT(24)       IFEQ      'S'
116800000000     C                   SUB       1             FUNVIS
116900000000     C                   ENDIF
117000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
117100000000     C                   Z-ADD     *ZEROS        W                 5 0
117200000000     C                   ADD       1             FUNULT
117300000000     C     FUNULT        IFGT      31
117400000000     C                   Z-ADD     1             FUNULT
117500000000     C                   ENDIF
117600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
117700000000     C     FUNULT        DO        31            I                              --- 1 -->
117800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
117900000000     C     W             IFLT      FUNVIS                                       --- 2 -->
118000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
118100000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
118200000000     C     FDE(I)        ANDNE     *BLANKS
118300000000     C     I             IFEQ      23                                           *TOLGO F23
118400000000     C     I             OREQ      24                                           *E F24
118500000000     C                   ELSE
118600000000     C                   ADD       1             W
118700000000     C                   MOVEL     FDE(I)        FUV(W)
118800000000     C                   ENDIF
118900000000     C                   ENDIF                                                  <-- 3 ---
119000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
119100000000     C                   Z-ADD     I             FUNULT
119200000000     C                   ENDIF                                                  <-- 2 ---
119300000000     C                   ENDDO                                                  <-- 1 ---
119400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
119500000000     C                   MOVEL     FUV(1)        VFD001
119600000000     C                   MOVEL     FUV(2)        VFD002
119700000000     C                   MOVEL     FUV(3)        VFD003
119800000000     C                   MOVEL     FUV(4)        VFD004
119900000000     C                   MOVEL     FUV(5)        VFD005
120000000000     C                   MOVEL     FUV(6)        VFD006
120100000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
120200000000     C     FAT(24)       ANDEQ     'N'
120300000000     C                   MOVEL     FUV(7)        VFD007
120400000000     C                   MOVEL     FUV(8)        VFD008
120500000000     C                   ELSE                                                   <-- 1 -->
120600000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
120700000000     C     FAT(24)       ANDEQ     'S'
120800000000     C                   MOVEL     FDE(23)       VFD007
120900000000     C                   MOVEL     FDE(24)       VFD008
121000000000     C                   ELSE                                                   <-- 2 -->
121100000000     C                   MOVEL     FUV(7)        VFD007
121200000000     C     FAT(23)       IFEQ      'S'
121300000000     C                   MOVEL     FDE(23)       VFD008
121400000000     C                   ELSE
121500000000     C                   MOVEL     FDE(24)       VFD008
121600000000     C                   ENDIF
121700000000     C                   ENDIF                                                  <-- 2 ---
121800000000     C                   ENDIF                                                  <-- 1 ---
121900000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
122000000000     C                   ENDSR
122100000000     C*------------------------------------------------------------------------*
122200000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
122300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
122400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
122500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
122600000000     C*------------------------------------------------------------------------*
122700000000     C     CONFUN        BEGSR
122800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
122900000000     C     1             DO        31            I                              --- 1 -->
123000000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
123100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
123200000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
123300000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
123400000000     C                   EXSR      CARMSG
123500000000     C                   MOVEL     '1'           $ERR
123600000000     C                   ENDIF                                                  <-- 3 ---
123700000000     C                   ENDIF                                                  <-- 2 ---
123800000000     C                   ENDDO                                                  <-- 1 ---
123900000000     C                   ENDSR
124000000000     C*------------------------------------------------------------------------*
124100000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
124200000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
124300000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
124400000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
124500000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
124600000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
124700000000     C*------------------------------------------------------------------------*
124800000000     C     CAROPZ        BEGSR
124900000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
125000000000     C     1             DO        24            I                              --- 1 -->
125100000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
125200000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
125300000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
125400000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
125500020715     c                   If        DutLpo = 'S'
125600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
125700000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
125800000000     C                   ELSE                                                   *INTERROG.
125900000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
126000000000     C                   ENDIF
126100000000     C                   ELSE                                                   <-- 2 --> *FILIALE
126200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
126300000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
126400000000     C                   ELSE                                                   *INTERROG.
126500000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
126600000000     C                   ENDIF
126700000000     C                   ENDIF                                                  <-- 2 ---
126800000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
126900000000     C                   MOVEL     USER          WEDP              3            *PROFILO
127000000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
127100000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
127200000000     C                   ELSE
127300000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
127400000000     C                   ENDIF
127500000000     C                   ENDIF
127600000000     C                   ENDDO                                                  <-- 1 ---
127700000000     C                   ENDSR
127800000000     C*------------------------------------------------------------------------*
127900000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
128000000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
128100000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
128200000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
128300000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
128400000000     C*------------------------------------------------------------------------*
128500000000     C     OUTOPZ        BEGSR
128600000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
128700000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
128800000000     C                   ENDDO
128900000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
129000000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
129100000000     C                   ADD       1             OPZULT
129200000000     C     OPZULT        IFGT      24
129300000000     C                   Z-ADD     1             OPZULT
129400000000     C                   END
129500000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
129600000000     C     1             DO        24            I
129700000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
129800000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
129900000000     C                   ADD       1             OPZNAT
130000000000     C                   ENDIF
130100000000     C                   ENDDO
130200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
130300000000     C                   Z-ADD     0             W
130400000000     C     OPZULT        DO        24            I                              --- 1 -->
130500000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
130600000000     C     W             IFLT      8                                            --- 2 -->
130700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
130800000000     C     OAT(I)        IFEQ      'S'
130900000000     C     ODE(I)        ANDNE     *BLANKS
131000000000     C                   ADD       1             W
131100000000     C                   MOVEL     ODE(I)        OPV(W)
131200000000     C                   ENDIF
131300000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
131400000000     C                   Z-ADD     I             OPZULT
131500000000     C                   ENDIF                                                  <-- 2 ---
131600000000     C                   ENDDO                                                  <-- 1 ---
131700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
131800000000     C                   MOVEL     OPV(1)        VOD001
131900000000     C                   MOVEL     OPV(2)        VOD002
132000000000     C                   MOVEL     OPV(3)        VOD003
132100000000     C                   MOVEL     OPV(4)        VOD004
132200000000     C                   MOVEL     OPV(5)        VOD005
132300000000     C                   MOVEL     OPV(6)        VOD006
132400000000     C                   MOVEL     OPV(7)        VOD007
132500000000     C                   MOVEL     OPV(8)        VOD008
132600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
132700000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
132800000000     C     OPZNAT        IFGT      8
132900000000     C                   MOVEL     'S'           FAT(23)
133000000000     C                   ENDIF
133100000000     C                   ENDSR
133200000000     C*------------------------------------------------------------------------*
133300000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
133400000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
133500000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
133600000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
133700000000     C*------------------------------------------------------------------------*
133800000000     C     CONOPZ        BEGSR
133900000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
134000000000     C                   MOVEL     *BLANKS       OPZOK             1
134100000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
134200000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
134300000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
134400000000     C     OAT(I)        IFEQ      'S'
134500000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
134600000000     C                   MOVEL     'S'           OPZOK
134700000000     C                   ELSE
134800000000     C                   MOVEL     'N'           OPZOK             1
134900000000     C                   ENDIF
135000000000     C                   ENDIF                                                  <-- 2 ---
135100000000     C                   ENDDO                                                  <-- 1 ---
135200000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
135300000000     C     OPZOK         IFNE      'S'
135400000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
135500000000     C                   EXSR      CARMSG
135600000000     C                   MOVEL     '1'           $ERR
135700000000     C                   MOVEL     'N'           OPZOK             1
135800000000     C                   ENDIF
135900000000     C                   ENDSR
136000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
136100000000TNTL12R- Tasto funzionale non valido.                                          1
136200000000TNTL12R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
136300000000TNTL12R- Opzione non valida.                                                   3
136400000000TNTL12R- Opzione abilitata ma non gestita. Avvertire CED.                      4
136500000000TNTL12R- Inizio elenco.                                                        5
136600000000TNTL12R- Fine elenco.                                                          6
136700000000TNTL12R- 7
136800000000TNTL12R- 8
136900000000TNTL12R- 9
137000000000TNTL12R- 10
137100000000TNTL12R- 11
137200000000TNTL12R- 12
137300000000TNTL12R- 13
137400000000TNTL12R- 14
137500000000TNTL12R- 15
137600000000TNTL12R- 16
137700000000TNTL12R- 17
137800000000TNTL12R- 18
137900000000TNTL12R- 19
138000000000TNTL12R- 20
138100000000TNTL12R- 21
138200000000TNTL12R- 22
138300000000TNTL12R- 23
138400000000TNTL12R- 24
138500000000TNTL12R- 25
138600000000TNTL12R- 26
138700000000TNTL12R- 27
138800000000TNTL12R- 28
138900000000TNTL12R- Record annullato, consentito solo il ripristino                      29
139000000000TNTL12<- Record non annullato, impossibile ripristinare                       30
139100000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
139200000000- 1-1 -                   -    NN     NN
139300000000- 2-2 -2=Modifica         -    SN     SN
139400000000- 3-3 -                   -    NN     NN
139500000000- 4-4 -4=Annullamento     -    SN     SN
139600000000- 5-5 -                   -    NN     NN
139700000000- 6-6 -                   -    NN     NN
139800000000- 7-7 -7=Ripristino       -    SN     SN
139900000000- 8-8 -                   -    NN     NN
140000000000- 9-9 -                   -    NN     NN
140100000000-10-  -                   -    NN     NN
140200000000-11-  -                   -    NN     NN
140300000000-12-  -                   -    NN     NN
140400000000-13-  -                   -    NN     NN
140500000000-14-  -                   -    NN     NN
140600000000-15-  -                   -    NN     NN
140700000000-16-  -                   -    NN     NN
140800000000-17-  -                   -    NN     NN
140900000000-18-  -                   -    NN     NN
141000000000-19-  -                   -    NN     NN
141100000000-20-  -                   -    NN     NN
141200000000-21-  -                   -    NN     NN
141300000000-22-  -                   -    NN     NN
141400000000-23-  -                   -    NN     NN
141500000000-24-  -                   -    NN     NN
141600000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
141700000000F1    -                   -    NN     NN
141800000000F2    -                   -    NN     NN
141900000000F3    -F3=Fine            -    NN     NN
142000000000F4    -                   -    NN     NN
142100000000F5    -                   -    NN     NN
142200000000F6    -                   -    NN     NN
142300000000F7    -                   -    NN     NN
142400000000F8    -                   -    NN     NN
142500000000F9    -                   -    NN     NN
142600000000F10   -F10=Inserimento    -    SN     SN
142700000000F11   -                   -    NN     NN
142800000000F12   -F12=Ritorno        -    SS     SS
142900000000F13   -                   -    NN     NN
143000000000F14   -                   -    NN     NN
143100000000F15   -                   -    NN     NN
143200000000F16   -                   -    NN     NN
143300000000F17   -                   -    NN     NN
143400000000F18   -                   -    NN     NN
143500000000F19   -                   -    NN     NN
143600000000F20   -                   -    NN     NN
143700000000F21   -                   -    NN     NN
143800000000F22   -                   -    NN     NN
143900000000F23   -F23=Altre opzioni  -    NN     NN
144000000000F24   -F24=Altri tasti    -    NN     NN
144100000000ENTER -                   -    SS     SS
144200000000ROLLUP-                   -    SS     SS
144300000000ROLLDN-                   -    SS     SS
144400000000HELP  -                   -    NN     NN
144500000000PRINT -                   -    NN     NN
144600000000BSPACE-                   -    NN     NN
144700000000CLEAR -                   -    NN     NN
