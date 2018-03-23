000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - NOTE - ROUTINE DI GESTIONE
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800960206     FTNTL13D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL13DVM:MSGULT)
001000000000     FTNTLR01L  UF A E           K DISK
001100000000     F                                     COMMIT
001200000000     FTNTLN01L  UF A E           K DISK
001300000000     F                                     COMMIT
001400951130     FAZORG01L  IF   E           K DISK
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* MESSAGGI E FUNZIONI
002000000000     D*-------------------
002100000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
002200000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002300000000     D*-------------------
002400951130     D* DECODIFICA OPZIONE RICHIAMATA
002500951130     D*-------------------
002600000000     D DOPZ            S             20    DIM(5) CTDATA PERRCD(1)              *DECODIFICA OPZIONE
002700000000     D*-------------------
002800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002900000000     D*-------------------
003000000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003100000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003200000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003300000000     D*-------------------
003400000000     D* TABELLA DESCRIZIONI A VIDEO
003500000000     D*-------------------
003600000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003700000000     D*-------------------
003800000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003900000000     D*-------------------
004000000000     D KEYS            S              1    DIM(40)
004100951130     D*-------------------
004200951130     D* FILIALI
004300951130     D*-------------------
004400971125     D ORG             S              3  0 DIM(500)                             *CODICE
004500971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
004600000000     D*------------------------------------------------------------------------*
004700000000     D* INPUT
004800000000     D*------------------------------------------------------------------------*
004900000000     D*-------------------
005000000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005100000000     D*-------------------
005200000000     D DSFUN         E DS
005300000000     D  FUNRIG                 1     40
005400000000     D*-------------------
005500000000     D* DS PER GESTIONE TASTI FUNZIONALI
005600000000     D*-------------------
005700000000     D DSKEYS        E DS
005800000000     D  KEYRIG                 1     40
005900000000     D*-------------------
006000000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006100000000     D*-------------------
006200000000     D                SDS
006300000000     D  USER                 254    263
006400000000     D*-------------------
006500000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006600000000     D*-------------------
006700000000     D DEVDS1          DS
006800000000     D  KEY                  369    369
006900000000     D  CURSOR               370    371
007000000000     D  FRTSFL               378    379B 0
007100000000     D*-------------------
007200000000     D* ARCHITETTURA
007300000000     D*-------------------
007400000000     D KPJBA         E DS
007500000000     D*---------------------
007600000000     D* DS ESTERNA DI PROCEDURA
007700000000     D*---------------------
007800000000     D DS00          E DS                  EXTNAME(TNTL00DS)
007900000000     D*-------------------
008000000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
008100000000     D*-------------------
008200000000     D DSMSG         E DS
008300960208     D*---------------------
008400960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
008500960208     D*---------------------
008600960208     D OG148         E DS
008700020715
008800020715     d DdatiUte      e ds
008900020715     d AzuteDs       e ds                  ExtName(Azute00f)
009000020715     d Tibs34Ds      e ds                  Inz
009100020715
009200000000     D*------------------
009300000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
009400000000     D*------------------
009500000000     D WLBDA8          DS
009600000000     D  G08DAT                 1      8  0
009700000000     D  G08INV                 9     16  0
009800000000     D  G08ERR                17     17
009900000000     D  G08TGI                18     22  0
010000951130     D*------------------
010100951130     D* DS UNIONE TESTO NOTA VIDEO
010200951130     D*------------------
010300960206     D*---        DS
010400960206     D*---                                    1  30 V1CTN1
010500960206     D*---                                   31  60 V1CTN2
010600960206     D*---                                    1  60 V1CTNO
010700960206     D                 DS
010800960206     D  V1CTN1                 1     59
010900960206     D  V1CTNO                 1     60
011000951130     D*------------------
011100951130     D* COSTANTI
011200951130     D*------------------
011300951130     D DESTFA          C                   CONST('ANDATA ')
011400951130     D DESTFR          C                   CONST('RITORNO')
011500000000     C*------------------------------------------------------------------------*
011600000000     C* MAIN LINE
011700000000     C*------------------------------------------------------------------------*
011800000000     C*
011900000000DO  1C     $FINE         DOWEQ     '0'
012000000000     C                   MOVEL     '0'           $ERR
012100000000     C*
012200000000IF  2C     $GES          IFEQ      'V1'
012300000000     C                   EXSR      GESV1
012400000000E   2C                   ENDIF
012500000000     C*
012600000000E   1C                   ENDDO
012700000000     C*
012800000000IF  1C* SE E' STATO PREMUTO F03 O F12 IMPOSTA FLAG DELLA DS DI PROCEDURA
012900000000     C     $F03          IFEQ      '1'
013000000000     C                   MOVEL     '1'           D00F03
013100000000E   1C                   ENDIF
013200000000     C     $F12          IFEQ      '1'
013300000000     C                   MOVEL     '1'           D00F12
013400000000E   1C                   ENDIF
013500000000     C*
013600000000     C                   MOVEL     DS00          KPJBU
013700000000     C                   SETON                                        LR
013800000000     C*------------------------------------------------------------------------*
013900000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
014000000000     C*------------------------------------------------------------------------*
014100000000     C     CARV1         BEGSR
014200000000     C*
014300000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
014400000000     C*
014500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014700000000     C*
014800000000     C                   WRITE     TL13DVT                                      *TESTATA
014900000000     C*---                 WRITETL13DVF                    *FUNZIONI
015000000000     C*
015100000000     C                   ENDSR
015200000000     C*------------------------------------------------------------------------*
015300000000     C* GESV1  - GESTIONE PRIMO VIDEO
015400000000     C*------------------------------------------------------------------------*
015500000000     C     GESV1         BEGSR
015600000000     C*
015700000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
015800000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
015900000000E   1C                   ENDIF
016000000000     C*
016100000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
016200000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
016300000000E   1C                   ENDIF
016400000000     C*
016500000000     C                   EXFMT     TL13DV1                                      *TEMPO DELIVERY
016600000000     C*
016700000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
016800000000     C*
016900000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
017000000000SL  2C                   SELECT
017100000000     C     KEY           WHENEQ    ENTER
017200000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
017300000000     C                   OTHER
017400000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
017500000000E   2C                   ENDSL
017600000000E   1C                   ENDIF
017700000000     C*
017800000000     C                   ENDSR
017900000000     C*------------------------------------------------------------------------*
018000000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
018100000000     C*------------------------------------------------------------------------*
018200000000     C     ENTV1         BEGSR
018300000000     C*
018400000000     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
018500000000IF  1C     D00OP1        IFEQ      'F10'
018600000000     C                   EXSR      INSERI                                       *INSERIMENTO
018700000000E   1C                   ENDIF
018800000000IF  1C     D00OP1        IFEQ      'O02'
018900000000     C                   EXSR      MODIFI                                       *MODIFICA
019000000000E   1C                   ENDIF
019100000000IF  1C     D00OP1        IFEQ      'O04'
019200000000     C                   EXSR      ANNULL                                       *ANNULLAMENTO
019300000000E   1C                   ENDIF
019400000000IF  1C     D00OP1        IFEQ      'O07'
019500000000     C                   EXSR      RIPRIS                                       *RIPRISTINO
019600000000E   1C                   ENDIF
019700000000     C*
019800000000     C                   ENDSR
019900000000     C*------------------------------------------------------------------------*
020000000000     C* INSERI - FUNZIONE DI INSERIMENTO
020100000000     C*------------------------------------------------------------------------*
020200000000     C     INSERI        BEGSR
020300000000     C*
020400000000     C* CONTROLLA I CAMPI IMMESSI
020500000000     C                   EXSR      DECODI
020600000000     C* NO ERRORI
020700000000IF  1C     $ERR          IFEQ      '0'
020800000000     C                   Z-ADD     D00TRN        TLNTRN                         *TRAINI
020900000000     C                   Z-ADD     D00DDE        TLNDDE                         *DATA DECORRENZA
021000060321     C***                MOVEL     V1CNFM        TLNNFM                         *NUMERO FERMATA
021100060321     C                   z-add     V1CNFM        TLNNFM                         *NUMERO FERMATA
021200000000     C                   Z-ADD     V1CPNO        TLNPNO                         *PROGRESSIVO NOTE
021300000000     C                   MOVEL     V1CTNO        TLNTNO                         *TESTO NOTE
021400000000     C                   Z-ADD     DATCOR        TLNDIR                         *DATA INSERIMENTO
021500000000     C                   Z-ADD     *ZEROS        TLNDUV                         *DATA VARIAZIONE
021600000000     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
021700000000     C                   Z-ADD     *ZEROS        TLNDTR                         *DATA TRASMISSIONE
021800000000     C                   MOVEL     *BLANKS       TLNATB                         *ANNULLAMENTO
021900000000     C*
022000000000     C                   WRITE     TNTLN000                                     *SCRIVE
022100000000     C*
022200000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
022300000000     C                   MOVEL     MSG(27)       D00MSG                         *CON MESSAGGIO
022400000000E   1C                   ENDIF
022500000000     C*
022600000000     C                   ENDSR
022700000000     C*------------------------------------------------------------------------*
022800000000     C* MODIFI - FUNZIONE DI MODIFICA
022900000000     C*------------------------------------------------------------------------*
023000000000     C     MODIFI        BEGSR
023100000000     C*
023200000000     C* CONTROLLA I CAMPI IMMESSI
023300000000     C                   EXSR      DECODI
023400000000     C* NO ERRORI
023500000000IF  1C     $ERR          IFEQ      '0'
023600000000     C                   MOVEL     V1CTNO        TLNTNO                         *TESTO NOTE
023700000000     C                   Z-ADD     DATCOR        TLNDUV                         *DATA VARIAZIONE
023800000000     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
023900000000     C                   MOVEL     *BLANKS       TLNATB                         *ANNULLAMENTO
024000000000     C*
024100000000     C                   UPDATE    TNTLN000                                     *AGGIORNA
024200000000     C*
024300000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
024400000000     C                   MOVEL     MSG(28)       D00MSG                         *CON MESSAGGIO
024500000000E   1C                   ENDIF
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
025000000000     C*------------------------------------------------------------------------*
025100000000     C     ANNULL        BEGSR
025200000000     C*---
025300000000     C* RECORD GIA'TRASMESSO --> ANNULLAMENTO LOGICO
025400000000     C* RECORD NON TRASMESSO --> ANNULLAMENTO FISICO
025500000000     C*---
025600000000IF  1C     TLNDTR        IFGT      *ZEROS
025700000000     C                   Z-ADD     DATCOR        TLNDUV                         *DATA VARIAZIONE
025800000000     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
025900000000     C                   MOVEL     'A'           TLNATB                         *ANNULLAMENTO
026000000000     C                   UPDATE    TNTLN000                                     *AGGIORNA
026100000000X   1C                   ELSE
026200000000     C                   DELETE    TNTLN000
026300000000E   1C                   ENDIF
026400000000     C*
026500000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
026600000000     C                   MOVEL     MSG(29)       D00MSG                         *CON MESSAGGIO
026700000000     C*
026800000000     C                   ENDSR
026900000000     C*------------------------------------------------------------------------*
027000000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
027100000000     C*------------------------------------------------------------------------*
027200000000     C     RIPRIS        BEGSR
027300951130     C*
027400951130     C* CONTROLLA I CAMPI IMMESSI
027500951130     C                   EXSR      DECODI
027600951130     C* NO ERRORI
027700951130IF  1C     $ERR          IFEQ      '0'
027800000000     C                   Z-ADD     DATCOR        TLNDUV                         *DATA VARIAZIONE
027900000000     C                   MOVEL     *BLANKS       TLNFTR                         *FLAG TRASMISSIONE
028000000000     C                   MOVEL     *BLANKS       TLNATB                         *ANNULLAMENTO
028100000000     C*
028200000000     C                   UPDATE    TNTLN000                                     *AGGIORNA
028300000000     C*
028400000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
028500000000     C                   MOVEL     MSG(30)       D00MSG                         *CON MESSAGGIO
028600951130E   1C                   ENDIF
028700000000     C*
028800000000     C                   ENDSR
028900000000     C*------------------------------------------------------------------------*
029000000000     C* DECODI - CONTROLLA LE SCHIERE DEL GIORNO
029100000000     C*------------------------------------------------------------------------*
029200000000     C     DECODI        BEGSR
029300000000     C*?I CONTROLLI DI QUESTA ROUTINE SONO DUPLICATI NEL PROGRAMMA "TNTL50R" CHE
029400000000     C*?CONTROLLA LA CONGRUITA' DEL DATABASE TRAINI.
029500000000     C*?OGNI MODIFICA APPORTATA A QUESTA ROUTINE QUINDI, DEVE ESSERE RICOPIATA
029600000000     C*?DENTRO IL PROGRAMMA ONDE EVITARE DISALLINEAMENTI.
029700000000     C*
029800000000     C* REIMPOSTA INDICATORI DI ERRORE
029900000000     C                   SETOFF                                       606162
030000000000     C                   MOVEL     '0'           $ERR
030100000000     C*-------------------
030200000000     C* NUMERO FERMATA
030300000000     C*-------------------
030400060321IF  1C***  $ERR          IFEQ      '0'
030500060321IF  2C***  V1CNFM        IFNE      *ZEROS
030600060321     C***                MOVEL     V1CNFM        WNFM
030700060321     C***                EXSR      CHKNFM
030800060321     C***                MOVEL     WNFM          V1CNFM
030900060321IF  3C***  $ERR          IFEQ      '1'
031000060321     C***                MOVEL     MSG(10)       $MSG
031100060321     C***                EXSR      CARMSG
031200060321     C***                SETON                                        60
031300060321E   3C***                ENDIF
031400060321E   2C***                ENDIF
031500060321E   1C***                ENDIF
031600000000     C*-------------------
031700000000     C* PROGRESSIVO NOTA
031800000000     C*-------------------
031900000000IF  1C     $ERR          IFEQ      '0'
032000000000IF  2C     V1CPNO        IFEQ      *ZEROS
032100000000     C                   MOVEL     '1'           $ERR
032200000000     C                   MOVEL     MSG(11)       $MSG
032300000000     C                   EXSR      CARMSG
032400000000     C                   SETON                                        61
032500000000E   2C                   ENDIF
032600000000E   1C                   ENDIF
032700000000     C*-------------------
032800000000     C* INSERIMENTO --> RECORD INESISTENTE
032900000000     C*-------------------
033000000000IF  1C     $ERR          IFEQ      '0'
033100000000IF  2C     D00OP1        IFEQ      'F10'
033200000000     C                   Z-ADD     D00TRN        KLNTRN                         *TRAINO
033300000000     C                   Z-ADD     D00DDE        KLNDDE                         *DATA DECORRENZA
033400060321     C***                MOVEL     V1CNFM        KLNNFM                         *NUMERO FERMATA
033500060321     C                   z-add     V1CNFM        KLNNFM                         *NUMERO FERMATA
033600000000     C                   Z-ADD     V1CPNO        KLNPNO                         *PROGRESSIVO NOTA
033700000000     C     KEYTLN        CHAIN(N)  TNTLN01L                           99
033800000000IF  3C     *IN99         IFEQ      *OFF                                         *INSESISTENTE
033900000000     C                   MOVEL     '1'           $ERR
034000000000     C                   MOVEL     MSG(24)       $MSG
034100000000     C                   EXSR      CARMSG
034200000000     C                   SETON                                        6061
034300000000E   3C                   ENDIF
034400000000E   2C                   ENDIF
034500000000E   1C                   ENDIF
034600000000     C*-------------------
034700000000     C* TESTO NOTA
034800000000     C*-------------------
034900000000IF  1C     $ERR          IFEQ      '0'
035000951130IF  2C     V1CTN1        IFEQ      *BLANKS
035100000000     C                   MOVEL     '1'           $ERR
035200000000     C                   MOVEL     MSG(12)       $MSG
035300000000     C                   EXSR      CARMSG
035400000000     C                   SETON                                        62
035500000000E   2C                   ENDIF
035600000000E   1C                   ENDIF
035700000000     C*
035800000000     C                   ENDSR
035900000000     C*------------------------------------------------------------------------*
036000000000     C* CHKNFM - CONTROLLA IL NUMERO FERMATA
036100000000     C*------------------------------------------------------------------------*
036200060321     C***  CHKNFM        BEGSR
036300000000     C*
036400060321     C***  *LIKE         DEFINE    V1CNFM        WNFM
036500000000     C*
036600000000     C* RICHIESTO ELENCO
036700060321     C***                SETOFF                                           28
036800060321     C***  '?'           SCAN      WNFM                                   28
036900060321IF  1C***  *IN28         IFEQ      *ON
037000060321     C***                Z-ADD     *ZEROS        D00SEQ
037100060321     C***                MOVEL     DS00          KPJBU
037200060321     C***                CALL      'TNTL16R'
037300060321     C***                PARM                    KPJBA
037400060321     C***                MOVEL     KPJBU         DS00
037500060321     C***                MOVEL     D00SEQ        WNFM
037600060321     C***                MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
037700060321E   1C***                ENDIF
037800000000     C* SOLO NUMERICO
037900060321     C***                SETOFF                                       28
038000060321     C***                TESTN                   WNFM                 28
038100060321     C***                MOVE      WNFM          WALF1             1
038200060321IF  1C***  *IN28         IFEQ      *OFF
038300060321     C***  WALF1         ORLT      *ZEROS
038400060321     C***                MOVEL     '1'           $ERR
038500060321X   1C***                ELSE
038600000000     C* ESISTENZA
038700060321IF  2C***  $ERR          IFEQ      '0'
038800060321     C***                Z-ADD     D00TRN        KLRTRN
038900060321     C***                Z-ADD     D00DDE        KLRDDE
039000060321     C***                MOVEL     WNFM          KLRNFM
039100060321     C***  KEYTLR        CHAIN(N)  TNTLR01L                           28
039200060321IF  3C***  *IN28         IFEQ      *ON
039300060321     C***  TLRATB        ORNE      *BLANKS
039400060321     C***                MOVEL     '1'           $ERR
039500060321X   3C***                ELSE
039600951130     C* DECODIFICHE
039700960206     C*---                 MOVEL*BLANKS   V1DTFM
039800960206     C*---                 MOVEL*BLANKS   V1DFIL
039900960206     C*---                 Z-ADD1         J
040000960206     C*---                 SETOF                         28
040100960206     C*---       TLRFIL    LOKUPORG,J                    28
040200960206IF  4C*---       *IN28     IFEQ *ON
040300960206     C*---                 MOVELDORG,J    V1DFIL           *FILIALE FERMATA
040400960206E   4C*---                 ENDIF
040500960206IF  4C*---       TLRTFM    IFEQ 'A'
040600960206     C*---                 MOVELDESTFA    V1DTFM           *FERMATA ANDATA
040700960206E   4C*---                 ENDIF
040800960206IF  4C*---       TLRTFM    IFEQ 'R'
040900960206     C*---                 MOVELDESTFR    V1DTFM           *FERMATA RITORNO
041000960206E   4C*---                 ENDIF
041100060321E   3C***                ENDIF
041200060321E   2C***                ENDIF
041300060321E   1C***                ENDIF
041400000000     C*
041500060321     C***                ENDSR
041600000000     C*------------------------------------------------------------------------*
041700000000     C* IMPVID - IMPOSTAZIONI VIDEO
041800000000     C*------------------------------------------------------------------------*
041900000000     C     IMPVID        BEGSR
042000000000     C*
042100000000     C* CALCOLA LA DATA CORRENTE
042200000000     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
042300000000     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
042400000000     C                   Z-ADD     WNUM8         G08DAT
042500000000     C                   Z-ADD     *ZEROS        G08INV
042600000000     C                   MOVEL     '0'           G08ERR
042700000000     C                   CALL      'XSRDA8'
042800000000     C                   PARM                    WLBDA8
042900000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
043000000000     C*
043100000000     C* 1°) DECODIFICA L'OPZIONE RICHIAMATA
043200000000     C* 2°) IMPOSTA LA PROTEZIONE DEI CAMPI -> 90=CAMPI CHIAVE / 91=ALTRI CAMPI
043300000000SL  1C                   SELECT
043400000000     C     D00OP1        WHENEQ    'F10'
043500000000     C                   MOVEL     DOPZ(1)       VTDFUN                         *INSERIMENTO
043600000000     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
043700000000     C     D00OP1        WHENEQ    'O02'
043800000000     C                   MOVEL     DOPZ(2)       VTDFUN                         *VARIAZIONE
043900000000     C                   SETON                                        90        *CHIAVE PROTETTA
044000000000     C     D00OP1        WHENEQ    'O04'
044100000000     C                   MOVEL     DOPZ(3)       VTDFUN                         *CANCELLAZIONE
044200000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
044300000000     C     D00OP1        WHENEQ    'O07'
044400000000     C                   MOVEL     DOPZ(4)       VTDFUN                         *RIPRISTINO
044500000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
044600000000     C                   OTHER
044700000000E   1C                   ENDSL
044800000000     C*
044900000000     C* CONTROLLA ESISTENZA E DISPONIBILITA' RECORD (NO SE INSERIMENTO)
045000000000     C* N.B. LEGGE IL RECORD VINCOLANDOLO SOLO SE NON E' IN INTERROGAZIONE
045100000000SL  1C                   SELECT
045200000000     C     D00OP1        WHENEQ    'F10'
045300000000     C                   OTHER
045400000000     C                   Z-ADD     D00TRN        KLNTRN                         *TRAINO
045500000000     C                   Z-ADD     D00DDE        KLNDDE                         *DATA DECORRENZA
045600000000     C                   Z-ADD     D00NFM        KLNNFM                         *NUMERO FERMATA
045700000000     C                   Z-ADD     D00PNO        KLNPNO                         *PROGRESSIVO NOTA
045800000000     C* INTERROGAZIONE
045900000000IF  2C     $TGES         IFEQ      'I'
046000000000     C     KEYTLN        CHAIN(N)  TNTLN01L                           99
046100000000X   2C                   ELSE
046200000000     C* MODIFICA
046300000000     C     KEYTLN        CHAIN     TNTLN01L                           9998
046400000000IF  3C     *IN98         IFEQ      *ON                                          *ALLOCATO
046500000000     C                   MOVEL     '1'           $ERR
046600000000     C                   MOVEL     '1'           $FINE
046700000000     C                   MOVEL     MSG(26)       D00MSG
046800000000E   3C                   ENDIF
046900000000E   2C                   ENDIF
047000000000IF  2C     *IN99         IFEQ      *ON                                          *INSESISTENTE
047100000000     C                   MOVEL     '1'           $ERR
047200000000     C                   MOVEL     '1'           $FINE
047300000000     C                   MOVEL     MSG(25)       D00MSG
047400000000E   2C                   ENDIF
047500000000E   1C                   ENDSL
047600000000     C* RIEMPE IL VIDEO
047700000000IF  1C     $ERR          IFEQ      '0'
047800000000SL  2C                   SELECT
047900000000     C     D00OP1        WHENEQ    'F10'
048000000000     C                   EXSR      AZZVID                                       *AZZERA CAMPI VIDEO
048100000000     C                   OTHER
048200951130     C                   Z-ADD     D00TRN        KLRTRN                         *LEGGE LA FERMATA
048300951130     C                   Z-ADD     D00DDE        KLRDDE
048400951130     C                   MOVEL     D00NFM        KLRNFM
048500951130     C     KEYTLR        CHAIN(N)  TNTLR01L                           28
048600000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
048700000000E   2C                   ENDSL
048800000000E   1C                   ENDIF
048900000000     C*
049000000000     C                   ENDSR
049100000000     C*------------------------------------------------------------------------*
049200000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
049300000000     C*------------------------------------------------------------------------*
049400000000     C     RECVID        BEGSR
049500000000     C*
049600000000     C                   MOVEL     TLNATB        V1CATB                         *ANNULLAMENTO
049700060321     C***                MOVEL     TLNNFM        V1CNFM                         *NUMERO FERMATA
049800060321     C                   z-add     TLNNFM        V1CNFM                         *NUMERO FERMATA
049900000000     C                   Z-ADD     TLNPNO        V1CPNO                         *PROGRESSIVO NOTA
050000000000     C                   MOVEL     TLNTNO        V1CTNO                         *TESTO NOTA
050100960206     C*---                 MOVELTLRTFM    V1DTFM           *TIPO FERMATA
050200960206     C*---                 Z-ADDTLRFIL    V1CFIL           *FILIALE
050300960206     C*--- DECODIFICHE
050400960206IF  1C*---       TLRFIL    IFGT *ZEROS
050500960206     C*---                 Z-ADD1         J
050600960206     C*---                 SETOF                         28
050700960206     C*---       TLRFIL    LOKUPORG,J                    28
050800960206IF  2C*---       *IN28     IFEQ *ON
050900960206     C*---                 MOVELDORG,J    V1DFIL
051000960206E   2C*---                 ENDIF
051100960206IF  2C*---       TLRTFM    IFEQ 'A'
051200960206     C*---                 MOVELDESTFA    V1DTFM
051300960206E   2C*---                 ENDIF
051400960206IF  2C*---       TLRTFM    IFEQ 'R'
051500960206     C*---                 MOVELDESTFR    V1DTFM
051600960206E   2C*---                 ENDIF
051700960206E   1C*---                 ENDIF
051800000000     C*
051900000000     C                   ENDSR
052000000000     C*------------------------------------------------------------------------*
052100000000     C*  AZZVID - PULISCE TUTTI I CAMPI DEL VIDEO
052200000000     C*------------------------------------------------------------------------*
052300000000     C     AZZVID        BEGSR
052400000000     C*
052500000000     C                   MOVEL     *BLANKS       V1CATB                         *ANNULLAMENTO
052600951130     C                   MOVEL     *ZEROS        V1CNFM                         *NUMERO FERMATA
052700000000     C                   Z-ADD     *ZEROS        V1CPNO                         *PROGRESSIVO NOTA
052800000000     C                   MOVEL     *BLANKS       V1CTNO                         *TESTO NOTA
052900960206     C*---                 MOVEL*BLANKS   V1DTFM           *TIPO FERMATA
053000951130     C                   Z-ADD     *ZEROS        V1CFIL                         *FILIALE
053100960206     C*---                 MOVEL*BLANKS   V1DFIL           *DESCRIZIONE FIL
053200000000     C*
053300000000     C                   ENDSR
053400000000     C*------------------------------------------------------------------------*
053500000000     C* CARTAB - CARICA LE TABELLE
053600000000     C*------------------------------------------------------------------------*
053700000000     C     CARTAB        BEGSR
053800951130     C*--------------------
053900951130     C* FILIALI
054000951130     C*--------------------
054100951130     C                   Z-ADD     *ZEROS        J                 4 0
054200951130     C     *LOVAL        SETLL     AZORG01L
054300951130     C                   READ      AZORG01L                               99
054400951130DO  1C     *IN99         DOWEQ     *OFF
054500951130IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
054600960208     C                   MOVEL     ORGDE8        OG148
054700960208IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
054800951130     C                   ADD       1             J
054900951130     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
055000951130     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
055100960208E   3C                   ENDIF
055200960208E   2C                   ENDIF
055300951130     C                   READ      AZORG01L                               99
055400951130E   1C                   ENDDO
055500951130     C*
055600000000     C                   ENDSR
055700000000     C*------------------------------------------------------------------------*
055800000000     C* *INZSR - IMPOSTAZIONI INIZIALI
055900000000     C*------------------------------------------------------------------------*
056000000000     C     *INZSR        BEGSR
056100000000     C*--------------------
056200000000     C* RICEVIMENTO PARAMETRI
056300000000     C*--------------------
056400000000     C     *ENTRY        PLIST
056500000000     C                   PARM                    KPJBA
056600000000     C                   MOVEL     KPJBU         DS00
056700000000     C*--------------------
056800000000     C* VARIABILI RIFERITE AL DATA BASE
056900000000     C*--------------------
057000000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
057100000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
057200000000     C     *LIKE         DEFINE    TLNNFM        KLNNFM
057300000000     C     *LIKE         DEFINE    TLNPNO        KLNPNO
057400000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
057500000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
057600000000     C     *LIKE         DEFINE    TLRNFM        KLRNFM
057700000000     C*--------------------
057800000000     C* CHIAVI DI LETTURA
057900000000     C*--------------------
058000000000     C* LETTURA TNTLN01L - COMPLETA
058100000000     C     KEYTLN        KLIST
058200000000     C                   KFLD                    KLNTRN                         *NUMERO TRAINO
058300000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
058400000000     C                   KFLD                    KLNNFM                         *NUMERO FERMATA
058500000000     C                   KFLD                    KLNPNO                         *PROGRESSIVO NOTE
058600000000     C* LETTURA TNTLR01L - COMPLETA
058700000000     C     KEYTLR        KLIST
058800000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
058900000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
059000000000     C                   KFLD                    KLRNFM                         *NUMERO FERMATA
059100000000     C*--------------------
059200000000     C* VARIABILI DI CONTROLLO
059300000000     C*--------------------
059400000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
059500000000     C                   MOVE      D00OP0        INTGES            2
059600000000IF  1C     INTGES        IFEQ      '02'
059700000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
059800000000X   1C                   ELSE
059900000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
060000000000E   1C                   ENDIF
060100000000     C*
060200000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
060300000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
060400000000IF  1C     *IN99         IFEQ      *ON
060500000000     C     *IN99         ANDEQ     *OFF
060600000000     C* FERMATE
060700000000     C                   UPDATE    TNTLR000
060800000000     C                   WRITE     TNTLR000
060900000000E   1C                   ENDIF
061000000000     C*
061100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
061200000000     C                   EXSR      INZSTD
061300000000     C*
061400000000     C* CARICA LE TABELLE
061500000000     C                   EXSR      CARTAB
061600000000     C*
061700000000     C* IMPOSTAZIONI VIDEO
061800000000     C                   EXSR      IMPVID
061900000000     C*
062000000000     C* EMETTE VIDEATA
062100000000IF  1C     $ERR          IFEQ      '0'                                          ERRORE IN ALLOCAZION
062200000000     C                   EXSR      CARV1
062300000000E   1C                   ENDIF
062400000000     C*
062500000000     C                   ENDSR
062600000000     C*------------------------------------------------------------------------*
062700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
062800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
062900000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
063000000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
063100000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
063200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063400000000     C*------------------------------------------------------------------------*
063500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
063600000000     C*------------------------------------------------------------------------*
063700000000     C     INZSTD        BEGSR
063800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
063900000000     C                   CALL      'XKEYS'
064000000000     C                   PARM                    DSKEYS
064100000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
064200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
064300000000     C     XMSGPA        PLIST
064400000000     C                   PARM                    DSMSMS
064500000000     C                   PARM                    DSMSPG
064600000000     C                   PARM                    DSMSKE
064700000000     C                   PARM                    DSMSER
064800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
064900000000     C                   MOVEL     'TNTL13R'     DSMSPG                         *NOME PGM CHIAMANTE
065000000000     C                   MOVEL     'TNTL13R'     VMCQUE                         *CODA MESSAGGI DESTI
065100000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
065200000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
065300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
065400000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
065500000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
065600000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
065700000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
065800000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
065900000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
066000000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
066100000000     C* LANCIO ROUTINE PER VERIFICARE SE
066200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
066300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
066400020715
066500020715     c     *dtaara       define    §azute        azuteds
066600020715     c     *dtaara       define    §datiute      ddatiute
066700020715
066800020715     c                   in(E)     *dtaara
066900020715     c                   if        %Error or rsut = *blanks
067000020715     c                   clear                   tibs34ds
067100020715     c                   call      'TIBS34R'
067200020715     c                   parm                    Tibs34Ds
067300020715     c                   in        *dtaara
067400020715     c                   endif
067500000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
067600000000     C                   BITOFF    '01234567'    POSMI1            1
067700000000     C                   BITOFF    '01234567'    POSMI2            1
067800000000     C                   BITOFF    '01234567'    POSMA1            1
067900000000     C                   BITOFF    '01234567'    POSMA2            1
068000000000     C                   BITON     '34'          POSMI1            1            *18
068100000000     C                   BITON     '7'           POSMI2            1            *01
068200000000     C                   BITON     '34'          POSMA1            1            *18
068300000000     C                   BITON     '0'           POSMA2            1            *80
068400000000     C                   MOVE      POSMI2        POSMIN            2
068500000000     C                   MOVEL     POSMI1        POSMIN
068600000000     C                   MOVE      POSMA2        POSMAX            2
068700000000     C                   MOVEL     POSMA1        POSMAX
068800000000     C                   ENDSR
068900000000     C*------------------------------------------------------------------------*
069000000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
069100000000     C*------------------------------------------------------------------------*
069200000000     C     FUNSTD        BEGSR
069300000000     C                   SELECT
069400000000     C     KEY           WHENEQ    F3
069500000000     C                   EXSR      F03STD
069600000000     C     KEY           WHENEQ    F12
069700000000     C                   EXSR      F12STD
069800000000     C     KEY           WHENEQ    F24
069900000000     C                   EXSR      F24STD
070000000000     C                   OTHER
070100000000     C                   MOVEL     MSG(2)        $MSG
070200000000     C                   EXSR      CARMSG
070300000000     C                   ENDSL
070400000000     C                   ENDSR
070500000000     C*------------------------------------------------------------------------*
070600000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
070700000000     C*------------------------------------------------------------------------*
070800000000     C     F03STD        BEGSR
070900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
071000000000     C                   MOVEL     '1'           $F03
071100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
071200000000     C                   MOVEL     '1'           $FINE
071300951212     C* PULISCE IL MESSAGGIO
071400951212     C                   MOVEL     *BLANKS       D00MSG
071500000000     C                   ENDSR
071600000000     C*------------------------------------------------------------------------*
071700000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
071800000000     C*------------------------------------------------------------------------*
071900000000     C     F12STD        BEGSR
072000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
072100000000     C                   MOVEL     '1'           $F12
072200000000     C* IMPOSTO FLAG DI FIN PGM
072300000000     C                   MOVEL     '1'           $FINE
072400951212     C* PULISCE IL MESSAGGIO
072500951212     C                   MOVEL     *BLANKS       D00MSG
072600000000     C                   ENDSR
072700000000     C*------------------------------------------------------------------------*
072800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
072900000000     C*------------------------------------------------------------------------*
073000000000     C     F24STD        BEGSR
073100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
073200000000     C                   EXSR      OUTFUN
073300000000     C                   ENDSR
073400000000     C*------------------------------------------------------------------------*
073500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
073600000000     C*          CODA MESSAGGI DEL PGM
073700000000     C*------------------------------------------------------------------------*
073800000000     C     CARMSG        BEGSR
073900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
074000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
074100000000     C     MSGULT        IFEQ      0
074200000000     C                   MOVEL     '1'           *IN40
074300000000     C                   WRITE     TL13DVK
074400000000     C                   MOVEL     '0'           *IN40
074500000000     C                   ENDIF
074600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
074700000000     C                   MOVEL     $MSG          DSMSMS
074800000000     C                   CALL      'XMSG'        XMSGPA
074900000000     C     DSMSER        IFEQ      0
075000000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
075100000000     C                   ENDIF
075200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
075300000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
075400000000     C                   WRITE     TL13DVM                                      *SFL MESS
075500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
075600000000     C                   MOVEL     *BLANKS       $MSG
075700000000     C                   ENDSR
075800000000     C*------------------------------------------------------------------------*
075900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
076000000000     C*------------------------------------------------------------------------*
076100000000     C     OUTMSG        BEGSR
076200000000     C* VISUALIZZAZIONE MESSAGGIO
076300000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
076400000000     C                   WRITE     TL13DVK                                      *CONTR MESS
076500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
076600000000     C                   Z-ADD     *ZEROS        MSGULT
076700000000     C                   ENDSR
076800000000     C*------------------------------------------------------------------------*
076900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
077000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
077100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
077200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
077300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
077400000000     C*------------------------------------------------------------------------*
077500000000     C     CARFUN        BEGSR
077600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
077700000000     C     1             DO        31            I                 5 0          --- 1 -->
077800000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
077900000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
078000000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
078100020715     c                   If        DutLpo = 'S'
078200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
078300000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
078400000000     C                   ELSE                                                   *INTERROG.
078500000000     C                   MOVEL     FUNASI        FAT(I)
078600000000     C                   ENDIF
078700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
078800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
078900000000     C                   MOVEL     FUNAFG        FAT(I)
079000000000     C                   ELSE                                                   *INTERROG.
079100000000     C                   MOVEL     FUNAFI        FAT(I)
079200000000     C                   ENDIF
079300000000     C                   ENDIF                                                  <-- 2 ---
079400000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
079500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
079600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
079700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
079800000000     C                   ELSE
079900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
080000000000     C                   ENDIF
080100000000     C                   ENDIF
080200000000     C                   ENDDO                                                  <-- 1 ---
080300000000     C                   ENDSR
080400000000     C*------------------------------------------------------------------------*
080500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
080600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
080700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
080800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
080900000000     C*------------------------------------------------------------------------*
081000000000     C     OUTFUN        BEGSR
081100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
081200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
081300000000     C                   ENDDO
081400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
081500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
081600000000     C     1             DO        31            I
081700000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
081800000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
081900000000     C                   ADD       1             FUNNAT
082000000000     C                   ENDIF
082100000000     C                   ENDDO
082200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
082300000000     C     FUNNAT        IFGT      8
082400000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
082500000000     C                   ENDIF
082600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
082700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
082800000000     C                   Z-ADD     8             FUNVIS            5 0
082900000000     C     FAT(23)       IFEQ      'S'
083000000000     C                   SUB       1             FUNVIS
083100000000     C                   ENDIF
083200000000     C     FAT(24)       IFEQ      'S'
083300000000     C                   SUB       1             FUNVIS
083400000000     C                   ENDIF
083500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
083600000000     C                   Z-ADD     *ZEROS        W                 5 0
083700000000     C                   ADD       1             FUNULT
083800000000     C     FUNULT        IFGT      31
083900000000     C                   Z-ADD     1             FUNULT
084000000000     C                   ENDIF
084100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
084200000000     C     FUNULT        DO        31            I                              --- 1 -->
084300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
084400000000     C     W             IFLT      FUNVIS                                       --- 2 -->
084500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
084600000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
084700000000     C     FDE(I)        ANDNE     *BLANKS
084800000000     C     I             IFEQ      23                                           *TOLGO F23
084900000000     C     I             OREQ      24                                           *E F24
085000000000     C                   ELSE
085100000000     C                   ADD       1             W
085200000000     C                   MOVEL     FDE(I)        FUV(W)
085300000000     C                   ENDIF
085400000000     C                   ENDIF                                                  <-- 3 ---
085500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
085600000000     C                   Z-ADD     I             FUNULT
085700000000     C                   ENDIF                                                  <-- 2 ---
085800000000     C                   ENDDO                                                  <-- 1 ---
085900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
086000000000     C                   MOVEL     FUV(1)        VFD001
086100000000     C                   MOVEL     FUV(2)        VFD002
086200000000     C                   MOVEL     FUV(3)        VFD003
086300000000     C                   MOVEL     FUV(4)        VFD004
086400000000     C                   MOVEL     FUV(5)        VFD005
086500000000     C                   MOVEL     FUV(6)        VFD006
086600000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
086700000000     C     FAT(24)       ANDEQ     'N'
086800000000     C                   MOVEL     FUV(7)        VFD007
086900000000     C                   MOVEL     FUV(8)        VFD008
087000000000     C                   ELSE                                                   <-- 1 -->
087100000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
087200000000     C     FAT(24)       ANDEQ     'S'
087300000000     C                   MOVEL     FDE(23)       VFD007
087400000000     C                   MOVEL     FDE(24)       VFD008
087500000000     C                   ELSE                                                   <-- 2 -->
087600000000     C                   MOVEL     FUV(7)        VFD007
087700000000     C     FAT(23)       IFEQ      'S'
087800000000     C                   MOVEL     FDE(23)       VFD008
087900000000     C                   ELSE
088000000000     C                   MOVEL     FDE(24)       VFD008
088100000000     C                   ENDIF
088200000000     C                   ENDIF                                                  <-- 2 ---
088300000000     C                   ENDIF                                                  <-- 1 ---
088400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
088500000000     C                   ENDSR
088600000000     C*------------------------------------------------------------------------*
088700000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
088800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
088900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
089000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
089100000000     C*------------------------------------------------------------------------*
089200000000     C     CONFUN        BEGSR
089300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
089400000000     C     1             DO        31            I                              --- 1 -->
089500000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
089600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
089700000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
089800000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
089900000000     C                   EXSR      CARMSG
090000000000     C                   MOVEL     '1'           $ERR
090100000000     C                   ENDIF                                                  <-- 3 ---
090200000000     C                   ENDIF                                                  <-- 2 ---
090300000000     C                   ENDDO                                                  <-- 1 ---
090400000000     C                   ENDSR
090500000000     C*------------------------------------------------------------------------*
090600000000** - MSG MESSAGGI
090700000000TNTL13R- Tasto funzionale non valido.                                          1
090800000000TNTL13R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
090900000000TNTL13R- Opzione non valida.                                                   3
091000000000TNTL13R- Opzione abilitata ma non gestita. Avvertire CED.                      4
091100000000TNTL13R- Inizio elenco.                                                        5
091200000000TNTL13R- Fine elenco.                                                          6
091300000000TNTL13R- 7
091400000000TNTL13R- 8
091500000000TNTL13R- 9
091600000000TNTL13R- Numero fermata inesistente                                           10
091700000000TNTL13R- Inserire il progressivo                                              11
091800000000TNTL13R- Inserire il testo della nota                                         12
091900000000TNTL13R- 13
092000000000TNTL13R- 14
092100000000TNTL13R- 15
092200000000TNTL13R- 16
092300000000TNTL13R- 17
092400000000TNTL13R- 18
092500000000TNTL13R- 19
092600000000TNTL13R- 20
092700000000TNTL13R- 21
092800000000TNTL13R- 22
092900000000TNTL13R- 23
093000000000TNTL13R- Richiesta funzione per record già esistente                          24
093100000000TNTL13R- Record inesistente                                                   25
093200000000TNTL13R- Record momentaneamente allocato: riprovare più tardi                 26
093300000000TNTL13R- Inserimento effettuato                                               27
093400000000TNTL13R- Variazione effettuata                                                28
093500000000TNTL13R- Annullamento effettuato                                              29
093600000000TNTL13R- Ripristino effettuato                                                30
093700000000** - FUN  FUNZIONI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
093800000000F1    -                   -    NN     NN
093900000000F2    -                   -    NN     NN
094000000000F3    -F3=Fine            -    NN     NN
094100000000F4    -                   -    NN     NN
094200000000F5    -                   -    NN     NN
094300000000F6    -                   -    NN     NN
094400000000F7    -                   -    NN     NN
094500000000F8    -                   -    NN     NN
094600000000F9    -                   -    NN     NN
094700000000F10   -                   -    NN     NN
094800000000F11   -                   -    NN     NN
094900000000F12   -F12=Ritorno        -    SS     SS
095000000000F13   -                   -    NN     NN
095100000000F14   -                   -    NN     NN
095200000000F15   -                   -    NN     NN
095300000000F16   -                   -    NN     NN
095400000000F17   -                   -    NN     NN
095500000000F18   -                   -    NN     NN
095600000000F19   -                   -    NN     NN
095700000000F20   -                   -    NN     NN
095800000000F21   -                   -    NN     NN
095900000000F22   -                   -    NN     NN
096000000000F23   -                   -    NN     NN
096100000000F24   -F24=Altri tasti    -    NN     NN
096200000000ENTER -                   -    SS     SS
096300000000ROLLUP-                   -    NN     NN
096400000000ROLLDN-                   -    NN     NN
096500000000HELP  -                   -    NN     NN
096600000000PRINT -                   -    NN     NN
096700000000BSPACE-                   -    NN     NN
096800000000CLEAR -                   -    NN     NN
096900000000** DOPZ  (OPZIONE RICHIAMATA)
097000000000* INSERIMENTO *     1
097100000000*  VARIAZIONE *     2
097200000000*CANCELLAZIONE*     3
097300000000*  RIPRISTINO *     4
0974000000005
