000100000210     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200000218     H*PARMS ACTGRP(*CALLER)
000300000000     H*------------------------------------------------------------------------*
000400000000     H* GESTIONE TRAINI - LISTINI - ROUTINE DI GESTIONE
000500010822
000600010822      * ATTENZIONE: questo programma era il vecchio TNTL11R
000700010822
000800000000     H*------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300010725     FTNTL39D   CF   E             WORKSTN INFDS(DEVDS1)
001400010725     F                                     SFILE(TL39DVM:MSGULT)
001500000821     FANATB01L  IF   E           K DISK     usropn
001600000210     FTABEL00F  IF   E           K DISK
001700000000     FTNTLZ01L  IF   E           K DISK
001800010723     FTITLL01L  UF A E           K DISK    COMMIT
001900010822
002000010822     d w_decimali      s              2  0
002100010928     d sav_v1cdiv      s                   like(v1cdiv)
002200010822
002300000000     D*------------------------------------------------------------------------*
002400000000     D* SCHIERE
002500000000     D*------------------------------------------------------------------------*
002600000000     D*-------------------
002700000000     D* MESSAGGI E FUNZIONI
002800000000     D*-------------------
002900000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
003000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
003100000000     D*-------------------
003200000000     D* DECODIFICA OPZIONE RICHIAMATA
003300000000     D*-------------------
003400000000     D DOPZ            S             20    DIM(5) CTDATA PERRCD(1)              *DECODIFICA OPZIONE
003500000000     D*-------------------
003600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003700000000     D*-------------------
003800000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003900000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
004000000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
004100000000     D*-------------------
004200000000     D* TABELLA DESCRIZIONI A VIDEO
004300000000     D*-------------------
004400000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004500000000     D*-------------------
004600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004700000000     D*-------------------
004800000000     D KEYS            S              1    DIM(40)
004900000000     D*-------------------
005000000000     D* TIPI AUTOMEZZO
005100000000     D*-------------------
005200000000     D TMZ             S              2    DIM(20)                              *CODICE
005300000000     D DTMZ            S             30    DIM(20)                              *DESCRIZIONE
005400000000     D*-------------------
005500000000     D* CAUSALI CONTABILIZZAZIONE
005600000000     D*-------------------
005700000000     D CAU             S              1    DIM(20)                              *CODICE
005800000000     D DCAU            S             30    DIM(20)                              *DESCRIZIONE
005900010724     D*-------------------
006000010724     D* DIVISA
006100010724     D*-------------------
006200010724     D DIV             S              3    DIM(20)                              *CODICE
006300010822     D cvtra           S              1    DIM(20)
006400010822     D cvfdc           S              1    DIM(20)
006500000000     D*------------------------------------------------------------------------*
006600000000     D* INPUT
006700000000     D*------------------------------------------------------------------------*
006800000000     D*-------------------
006900000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
007000000000     D*-------------------
007100000000     D DSFUN         E DS
007200000000     D  FUNRIG                 1     40
007300000000     D*-------------------
007400000000     D* DS PER GESTIONE TASTI FUNZIONALI
007500000000     D*-------------------
007600000000     D DSKEYS        E DS
007700000000     D  KEYRIG                 1     40
007800000000     D*-------------------
007900000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
008000000000     D*-------------------
008100000000     D                SDS
008200000000     D  USER                 254    263
008300000000     D*-------------------
008400000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008500000000     D*-------------------
008600000000     D DEVDS1          DS
008700000000     D  KEY                  369    369
008800000000     D  CURSOR               370    371
008900000000     D  FRTSFL               378    379B 0
009000000000     D*-------------------
009100000000     D* ARCHITETTURA
009200000000     D*-------------------
009300000000     D KPJBA         E DS
009400000000     D*---------------------
009500000000     D* DS ESTERNA DI PROCEDURA
009600000000     D*---------------------
009700000000     D DS00          E DS                  EXTNAME(TNTL00DS)
009800000000     D*-------------------
009900000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
010000000000     D*-------------------
010100000000     D DSMSG         E DS
010200000000     D*---------------------
010300000000     D* DS TIPI AUTOMEZZI
010400000000     D*---------------------
010500000000     D DSTU          E DS
010600010724     D*-------------------
010700010821     D* DS per la DIVISA
010800010724     D*-------------------
010900010821     D dscv          e ds
011000010821     D dged          e ds
011100010821     D tibs02ds      e ds
011200000000     D*---------------------
011300000000     D* DS CAUSALI CONTABILIZZAZIONE
011400000000     D*---------------------
011500000210     D ANGY4ZDS      E DS
011600000210     D XTABDS        E DS                  INZ
011700020716
011800020716     d DdatiUte      e ds
011900020716     d AzuteDs       e ds                  ExtName(Azute00f)
012000020716     d Tibs34Ds      e ds                  Inz
012100020716
012200000000     D*------------------
012300000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
012400000000     D*------------------
012500000000     D WLBDA8          DS
012600000000     D  G08DAT                 1      8  0
012700000000     D  G08INV                 9     16  0
012800000000     D  G08ERR                17     17
012900000000     D  G08TGI                18     22  0
013000000000     D*------------------
013100000000     D* DS "TRUL16R" - RICERCA ALFABETICA TRAZIONISTI
013200000000     D*------------------
013300000000     D PARAM           DS                  INZ
013400000821     D  PARTIP                 1      2
013500000821     D  PARRSC                 3     37
013600000821     D  PARCSF                38     40
013700000821     D  PARPDR                41     47  0
013800000821     D  PARFLG                48     48
013900000000     D*------------------
014000000000     D* DESCRIZIONE OPERAZIONE IN TESTATA
014100000000     D*------------------
014200000000     D* COMMENTI
014300000000     D DES999          C                   CONST('DI CARTELLO')
014400000000     C*------------------------------------------------------------------------*
014500000000     C* MAIN LINE
014600000000     C*------------------------------------------------------------------------*
014700000000     C*
014800000000DO  1C     $FINE         DOWEQ     '0'
014900000000     C                   MOVEL     '0'           $ERR
015000000000     C*
015100000000IF  2C     $GES          IFEQ      'V1'
015200000000     C                   EXSR      GESV1
015300000000E   2C                   ENDIF
015400000000     C*
015500000000E   1C                   ENDDO
015600000000     C*
015700000000IF  1C* SE E' STATO PREMUTO F03 O F12 IMPOSTA FLAG DELLA DS DI PROCEDURA
015800000000     C     $F03          IFEQ      '1'
015900000000     C                   MOVEL     '1'           D00F03
016000000000E   1C                   ENDIF
016100000000     C     $F12          IFEQ      '1'
016200000000     C                   MOVEL     '1'           D00F12
016300000000E   1C                   ENDIF
016400000000     C*
016500000000     C                   MOVEL     DS00          KPJBU
016600000000     C                   SETON                                        LR
016700000000     C*------------------------------------------------------------------------*
016800000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
016900000000     C*------------------------------------------------------------------------*
017000000000     C     CARV1         BEGSR
017100000000     C*
017200000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
017300000000     C*
017400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
017500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
017600000000     C*
017700010725     C                   WRITE     TL39DVT                                      *TESTATA
017800010723     C*---                 WRITETL38DVF                    *FUNZIONI
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* GESV1  - GESTIONE PRIMO VIDEO
018300000000     C*------------------------------------------------------------------------*
018400000000     C     GESV1         BEGSR
018500000000     C*
018600000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
018700000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
018800000000E   1C                   ENDIF
018900000000     C*
019000000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
019100000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
019200000000E   1C                   ENDIF
019300000000     C*
019400010725     C                   EXFMT     TL39DV1                                      *TEMPO DELIVERY
019500000000     C*
019600000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
019700000000     C*
019800000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
019900000000SL  2C                   SELECT
020000000000     C     KEY           WHENEQ    ENTER
020100000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
020200000000     C                   OTHER
020300000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
020400000000E   2C                   ENDSL
020500000000E   1C                   ENDIF
020600000000     C*
020700000000     C                   ENDSR
020800000000     C*------------------------------------------------------------------------*
020900000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
021000000000     C*------------------------------------------------------------------------*
021100000000     C     ENTV1         BEGSR
021200000000     C*
021300000000     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
021400000000IF  1C     D00OP1        IFEQ      'F10'
021500000000     C                   EXSR      INSERI                                       *INSERIMENTO
021600000000E   1C                   ENDIF
021700000000IF  1C     D00OP1        IFEQ      'O02'
021800000000     C                   EXSR      MODIFI                                       *MODIFICA
021900000000E   1C                   ENDIF
022000000000IF  1C     D00OP1        IFEQ      'O04'
022100000000     C                   EXSR      ANNULL                                       *ANNULLAMENTO
022200000000E   1C                   ENDIF
022300000000IF  1C     D00OP1        IFEQ      'O07'
022400000000     C                   EXSR      RIPRIS                                       *RIPRISTINO
022500000000E   1C                   ENDIF
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*------------------------------------------------------------------------*
022900000000     C* INSERI - FUNZIONE DI INSERIMENTO
023000000000     C*------------------------------------------------------------------------*
023100000000     C     INSERI        BEGSR
023200000000     C*
023300000000     C* CONTROLLA I CAMPI IMMESSI
023400000000     C                   EXSR      DECODI
023500000000     C* NO ERRORI
023600000000IF  1C     $ERR          IFEQ      '0'
023700000000     C                   Z-ADD     D00TRN        TLLTRN                         *TRAINI
023800000000     C                   Z-ADD     D00DDE        TLLDDE                         *DATA DECORRENZA
023900000000     C                   MOVEL     V1CTMZ        TLLTMZ                         *TIPO AUTOMEZZO
024000000000     C                   MOVEL     V1CPDR        TLLPDR                         *TRAZIONISTA
024100000000     C                   Z-ADD     V1CILI        TLLILI                         *IMPORTO LISTINO
024200000000     C                   Z-ADD     V1CIES        TLLIES                         *IMPORTO SU F.V.
024300010723     C                   MOVEL     V1CDIV        TLLDIV                         *DIVISA
024400000000     C                   MOVEL     V1CCAU        TLLCAU                         *CAUSALE CONTAB.NE
024500000000     C                   Z-ADD     DATCOR        TLLDIR                         *DATA INSERIMENTO
024600000000     C                   Z-ADD     *ZEROS        TLLDUV                         *DATA VARIAZIONE
024700000000     C                   MOVEL     *BLANKS       TLLFTR                         *FLAG TRASMISSIONE
024800000000     C                   Z-ADD     *ZEROS        TLLDTR                         *DATA TRASMISSIONE
024900000000     C                   MOVEL     *BLANKS       TLLATB                         *ANNULLAMENTO
025000000000     C*
025100010723     C                   WRITE     TITLL000                                     *SCRIVE
025200000000     C*
025300000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
025400000000     C                   MOVEL     MSG(27)       D00MSG                         *CON MESSAGGIO
025500000000E   1C                   ENDIF
025600000000     C*
025700000000     C                   ENDSR
025800000000     C*------------------------------------------------------------------------*
025900000000     C* MODIFI - FUNZIONE DI MODIFICA
026000000000     C*------------------------------------------------------------------------*
026100000000     C     MODIFI        BEGSR
026200000000     C*
026300000000     C* CONTROLLA I CAMPI IMMESSI
026400000000     C                   EXSR      DECODI
026500000000     C* NO ERRORI
026600000000IF  1C     $ERR          IFEQ      '0'
026700000000     C                   Z-ADD     V1CILI        TLLILI                         *IMPORTO LISTINO
026800000000     C                   Z-ADD     V1CIES        TLLIES                         *IMPORTO SU F.V.
026900010723     C                   MOVEL     V1CDIV        TLLDIV                         *DIVISA
027000000000     C                   MOVEL     V1CCAU        TLLCAU                         *CAUSALE CONTAB.NE
027100000000     C                   Z-ADD     DATCOR        TLLDUV                         *DATA VARIAZIONE
027200000000     C                   MOVEL     *BLANKS       TLLFTR                         *FLAG TRASMISSIONE
027300000000     C                   MOVEL     *BLANKS       TLLATB                         *ANNULLAMENTO
027400000000     C*
027500010723     C                   UPDATE    TITLL000                                     *AGGIORNA
027600000000     C*
027700000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
027800000000     C                   MOVEL     MSG(28)       D00MSG                         *CON MESSAGGIO
027900000000E   1C                   ENDIF
028000000000     C*
028100000000     C                   ENDSR
028200000000     C*------------------------------------------------------------------------*
028300000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
028400000000     C*------------------------------------------------------------------------*
028500000000     C     ANNULL        BEGSR
028600000000     C*---
028700000000     C* RECORD GIA'TRASMESSO --> ANNULLAMENTO LOGICO
028800000000     C* RECORD NON TRASMESSO --> ANNULLAMENTO FISICO
028900000000     C*---
029000130529IF  1C**   TLLDTR        IFGT      *ZEROS
029100130529     C**                 Z-ADD     DATCOR        TLLDUV                         *DATA VARIAZIONE
029200130529     C**                 MOVEL     *BLANKS       TLLFTR                         *FLAG TRASMISSIONE
029300130529     C**                 MOVEL     'A'           TLLATB                         *ANNULLAMENTO
029400130529     C**                 UPDATE    TITLL000                                     *AGGIORNA
029500130529X   1C**                 ELSE
029600010723     C                   DELETE    TITLL000
029700130529E   1C**                 ENDIF
029800000000     C*
029900000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
030000000000     C                   MOVEL     MSG(29)       D00MSG                         *CON MESSAGGIO
030100000000     C*
030200000000     C                   ENDSR
030300000000     C*------------------------------------------------------------------------*
030400000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
030500000000     C*------------------------------------------------------------------------*
030600000000     C     RIPRIS        BEGSR
030700000000     C*
030800000000     C                   Z-ADD     DATCOR        TLLDUV                         *DATA VARIAZIONE
030900000000     C                   MOVEL     *BLANKS       TLLFTR                         *FLAG TRASMISSIONE
031000000000     C                   MOVEL     *BLANKS       TLLATB                         *ANNULLAMENTO
031100000000     C*
031200010723     C                   UPDATE    TITLL000                                     *AGGIORNA
031300000000     C*
031400000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
031500000000     C                   MOVEL     MSG(30)       D00MSG                         *CON MESSAGGIO
031600000000     C*
031700000000     C                   ENDSR
031800000000     C*------------------------------------------------------------------------*
031900000000     C* DECODI - CONTROLLA LE SCHIERE DEL GIORNO
032000000000     C*------------------------------------------------------------------------*
032100000000     C     DECODI        BEGSR
032200000000     C*?I CONTROLLI DI QUESTA ROUTINE SONO DUPLICATI NEL PROGRAMMA "TNTL50R" CHE
032300000000     C*?CONTROLLA LA CONGRUITA' DEL DATABASE TRAINI.
032400000000     C*?OGNI MODIFICA APPORTATA A QUESTA ROUTINE QUINDI, DEVE ESSERE RICOPIATA
032500000000     C*?DENTRO IL PROGRAMMA ONDE EVITARE DISALLINEAMENTI.
032600000000     C*
032700000000     C* REIMPOSTA INDICATORI DI ERRORE
032800000000     C                   SETOFF                                       606162
032900010724     C                   SETOFF                                       636465
033000000000     C                   MOVEL     '0'           $ERR
033100000000     C*-------------------
033200000000     C* TIPO AUTOMEZZO
033300000000     C*-------------------
033400000000IF  1C     $ERR          IFEQ      '0'
033500000000     C                   MOVEL     V1CTMZ        WTMZ
033600000000     C                   MOVEL     *BLANKS       WDTMZ
033700000000     C                   EXSR      CHKTMZ
033800000000     C                   MOVEL     WTMZ          V1CTMZ
033900000000     C                   MOVEL     WDTMZ         V1DTMZ
034000000000IF  2C     $ERR          IFEQ      '1'
034100000000     C                   MOVEL     MSG(10)       $MSG
034200000000     C                   EXSR      CARMSG
034300000000     C                   SETON                                        60
034400000000E   2C                   ENDIF
034500000000E   1C                   ENDIF
034600000000     C*-------------------
034700000000     C* TRAZIONISTA
034800000000     C*-------------------
034900000000IF  1C     $ERR          IFEQ      '0'
035000000000     C                   MOVEL     V1CPDR        WPDR
035100000000     C                   MOVEL     V1DPDR        WDPDR
035200000000     C                   EXSR      CHKPDR
035300000000     C                   MOVEL     WPDR          V1CPDR
035400000000     C                   MOVEL     WDPDR         V1DPDR
035500000000IF  2C     $ERR          IFEQ      '1'
035600000000     C                   EXSR      CARMSG
035700000000     C                   SETON                                        61
035800000000E   2C                   ENDIF
035900000000E   1C                   ENDIF
036000011001
036100000000     C*-------------------
036200000000     C* IMPORTO DI LISTINO
036300000000     C*-------------------
036400000000IF  1C     $ERR          IFEQ      '0'
036500180102IF  2C     V1CILI        andle     *ZEROS
036600000000     C                   MOVEL     '1'           $ERR
036700000000     C                   MOVEL     MSG(13)       $MSG
036800000000     C                   EXSR      CARMSG
036900000000     C                   SETON                                        62
037000000000E   1C                   ENDIF
037100011012     C*-------------------
037200011012     C* DIVISA
037300011012     C*-------------------
037400011012IF  1C     $ERR          IFEQ      '0'
037500011012      * obbligatoria
037600011012     C                   if        v1cdiv = *blanks
037700011012     C                   eval      $ERR = '1'
037800011012     C                   movel     msg(19)       $msg
037900011012     C                   exsr      carmsg
038000011012     C                   eval      *in65 = *on
038100011012     C                   endif
038200011012E   2C                   ENDIF
038300011012      * controllo divisa
038400011012IF  1C     $ERR          IFEQ      '0'
038500011012     C                   MOVEL     V1Cdiv        Wdiv
038600011012     C                   EXSR      CHKdiv
038700011012     C                   MOVEL     Wdiv          V1Cdiv
038800011012      * non codificata
038900011012IF  2C     $ERR          IFEQ      '1'
039000011012     C                   MOVEL     MSG(16)       $MSG
039100011012     C                   EXSR      CARMSG
039200011012     C                   SETON                                        65
039300011012E   2C                   ENDIF
039400011012      * non ammessa nei traini
039500011012     C                   if        $err = '0' and cvtra(j) <> 'S'
039600011012     C                   movel     msg(20)       $msg
039700011012     C                   exsr      carmsg
039800011012     C                   eval      *in65 = *on
039900011012     C                   eval      $ERR = '1'
040000011012     C                   endif
040100011012      * se modificata la divisa e non l'importo devo dare msg di errore
040200011012IF  2C     $ERR          IFEQ      '0'
040300011012     c                   if        v1cdiv <> sav_v1cdiv
040400011012     c                             and *in66 = *off
040500011012     c                             and (v1cili <> *zeros
040600011012     c                                  or v1cdiv <> *blanks)
040700011012     C                   MOVEL     MSG(18)       $MSG
040800011012     C                   EXSR      CARMSG
040900011012     C                   SETON                                        62
041000011012     C                   MOVEL     '1'           $ERR
041100011012     c                   endif
041200011012E   2C                   ENDIF
041300011012IF  2C     $ERR          IFEQ      '0'
041400011012     c                   movel     v1cdiv        sav_v1cdiv
041500011012E   2C                   ENDIF
041600011012E   1C                   ENDIF
041700010822      * controllo se ok i decimali
041800010822IF  1C     $ERR          IFEQ      '0'
041900010822     c                   move      v1cili        w_decimali
042000010822     c                   if        w_decimali > 0
042100010822     C                   SETOFF                                           28
042200010822     C                   Z-ADD     1             J
042300010822     C     Wdiv          LOOKUP    div(J)                                 28
042400010822IF  2C     *IN28         IFEQ      *ON
042500010822     c                   eval      $err = (cvfdc(J) <> 'S')
042600010822E   2C                   ENDIF
042700010822     c                   endif
042800010822IF  2C     $ERR          IFEQ      '1'
042900010822     C                   MOVEL     MSG(17)       $MSG
043000010822     C                   EXSR      CARMSG
043100010822     C                   SETON                                        62
043200010822E   2C                   ENDIF
043300010822E   1C                   ENDIF
043400000000     C*-------------------
043500000000     C* IMPORTO DA ESPORRE SUL F.V.
043600000000     C*-------------------
043700000000IF  1C     $ERR          IFEQ      '0'
043800000000IF  2C     V1CIES        IFEQ      *ZEROS
043900000000     C     V1CIES        OREQ      V1CILI
044000000000X   2C                   ELSE
044100000000     C                   MOVEL     '1'           $ERR
044200000000     C                   MOVEL     MSG(14)       $MSG
044300000000     C                   EXSR      CARMSG
044400000000     C                   SETON                                        63
044500000000E   2C                   ENDIF
044600000000E   1C                   ENDIF
044700010822      * controllo se ok i decimali
044800010822IF  1C     $ERR          IFEQ      '0'
044900010822     c                   move      v1cies        w_decimali
045000010822     c                   if        w_decimali > 0
045100010822     C                   SETOFF                                           28
045200010822     C                   Z-ADD     1             J
045300010822     C     Wdiv          LOOKUP    div(J)                                 28
045400010822IF  2C     *IN28         IFEQ      *ON
045500010822     c                   eval      $err = (cvfdc(J) <> 'S')
045600010822E   2C                   ENDIF
045700010822     c                   endif
045800010822IF  2C     $ERR          IFEQ      '1'
045900010822     C                   MOVEL     MSG(17)       $MSG
046000010822     C                   EXSR      CARMSG
046100010822     C                   SETON                                        63
046200010822E   2C                   ENDIF
046300010822E   2C                   ENDIF
046400000000     C*-------------------
046500000000     C* CAUSALE DI CONTABILIZZAZIONE
046600000000     C*-------------------
046700020716     c                   If        DutLpo = 'S'
046800000821IF  2C     $ERR          IFEQ      '0'
046900000000     C                   MOVEL     V1CCAU        WCAU
047000000000     C                   EXSR      CHKCAU
047100000000     C                   MOVEL     WCAU          V1CCAU
047200000000     C                   MOVEL     WDCAU         V1DCAU
047300000821IF  3C     $ERR          IFEQ      '1'
047400000000     C                   MOVEL     MSG(15)       $MSG
047500000000     C                   EXSR      CARMSG
047600000000     C                   SETON                                        64
047700000821E   3C                   ENDIF
047800000821E   2C                   ENDIF
047900000821E   1C                   ENDIF
048000000000     C*-------------------
048100000000     C* INSERIMENTO --> RECORD INESISTENTE
048200000000     C*-------------------
048300000000IF  1C     $ERR          IFEQ      '0'
048400000000IF  2C     D00OP1        IFEQ      'F10'
048500000000     C                   Z-ADD     D00TRN        KLLTRN                         *TRAINO
048600000000     C                   Z-ADD     D00DDE        KLLDDE                         *DATA DECORRENZA
048700000000     C                   MOVEL     V1CTMZ        KLLTMZ                         *TIPO AUTOMEZZO
048800000000     C                   MOVEL     V1CPDR        KLLPDR                         *PADRONCINO
048900010723     C     KEYTLL        CHAIN(N)  TITLL01L                           99
049000000000IF  3C     *IN99         IFEQ      *OFF                                         *INSESISTENTE
049100000000     C                   MOVEL     '1'           $ERR
049200000000     C                   MOVEL     MSG(24)       $MSG
049300000000     C                   EXSR      CARMSG
049400000000     C                   SETON                                        6061
049500000000E   3C                   ENDIF
049600000000E   2C                   ENDIF
049700000000E   1C                   ENDIF
049800000000     C*
049900011001     C                   ENDSR
050000000000     C*------------------------------------------------------------------------*
050100000000     C* CHKTMZ - CONTROLLA IL TIPO AUTOMEZZO
050200000000     C*          INPUT  -> WTMZ  - TIPO AUTOMEZZO
050300000000     C*          OUTPUT -> WDTMZ - DESCRIZIONE TIPO AUTOMEZZO
050400000000     C*------------------------------------------------------------------------*
050500000000     C     CHKTMZ        BEGSR
050600000000     C*
050700000000     C     *LIKE         DEFINE    V1CTMZ        WTMZ
050800000000     C     *LIKE         DEFINE    V1DTMZ        WDTMZ
050900000000     C*
051000000000     C                   MOVEL     *BLANKS       WDTMZ
051100000000     C* RICERCA TABELLA
051200000218     C     '?'           SCAN      WTMZ                                   28
051300000218IF  1C     *IN28         IFEQ      *ON
051400020716     c                   Z-Add     1             §tKut
051500000218     C                   MOVEL     'TU'          §TCOD
051600000218     C                   MOVEL     *BLANKS       §TKEY
051700000218     C                   MOVEL     *BLANKS       §TDES
051800000000     C                   CALL      'X§TABER'
051900000000     C                   PARM                    §TKUT             1 0
052000000000     C                   PARM                    §TCOD             2
052100000000     C                   PARM                    §TKEY             8
052200000000     C                   PARM                    §TDES            30
052300000000     C                   MOVEL     §TKEY         WTMZ                           *CODICE
052400000000     C                   MOVEL     §TDES         WDTMZ                          *DESCRIZIONE
052500000000     C                   MOVE      '1'           $TESVD                         *RIEMETTERE TESTATA
052600000000E   1C                   ENDIF
052700000000     C* ESISTENTE
052800000000IF  1C     WTMZ          IFEQ      *BLANKS
052900000000     C                   MOVEL     '1'           $ERR
053000000000X   1C                   ELSE
053100000000     C                   SETOFF                                           28
053200000000     C                   Z-ADD     1             J
053300000000     C     WTMZ          LOOKUP    TMZ(J)                                 28      *1° CONTROLLO
053400000000     C                   MOVEL     WTMZ          WALF1                            *2° CONTROLLO
053500000000IF  2C     *IN28         IFEQ      *ON
053600000000     C     WALF1         ANDNE     '9'
053700000000     C                   MOVEL     DTMZ(J)       WDTMZ                          *DESCRIZ. AUTOMEZZO
053800000000X   2C                   ELSE
053900000000IF  3C     WTMZ          IFEQ      '99'                                         *TARIFFA DI CARTELLO
054000000000     C                   MOVEL     DES999        WDTMZ                          *DESCRIZ. AUTOMEZZO
054100000000X   3C                   ELSE
054200000000     C                   MOVEL     '1'           $ERR
054300000000E   3C                   ENDIF
054400000000E   2C                   ENDIF
054500000000E   1C                   ENDIF
054600000000     C*
054700000000     C                   ENDSR
054800000000     C*------------------------------------------------------------------------*
054900000000     C* CHKPDR - CONTROLLA IL TRAZIONISTA
055000000000     C*          INPUT  -> WPDR  - CODICE TRAZIONISTA
055100000000     C*                    WDPDR - CHIAVE RICERCA ALFABETICA
055200000000     C*          OUTPUT -> WDPDR - DESCRIZIONE TRAZIONISTA
055300000000     C*------------------------------------------------------------------------*
055400000000     C     CHKPDR        BEGSR
055500000000     C*
055600000000     C     *LIKE         DEFINE    V1CPDR        WPDR
055700000000     C     *LIKE         DEFINE    V1DPDR        WDPDR
055800000000     C*
055900000000     C* RICERCA ALFABETICA
056000000000     C     '?'           SCAN      WPDR                                   28
056100000000IF  1C     *IN28         IFEQ      *ON
056200000000     C* DEPOSITA LA KPJBU
056300000000     C                   MOVEL     KPJBU         DKPJBU
056400000821     C                   EVAL      PARTIP = 'T '                                PAR-1
056500000821     C                   MOVEL(P)  WDPDR         PARRSC                         PAR-2
056600000821     C                   CLEAR                   PARCSF                         PAR-3
056700000821     C                   Z-ADD     *ZEROS        PARPDR                         PAR-4
056800000821     C                   MOVEL     *BLANKS       PARFLG                         PAR-5
056900000000     C                   MOVEL     PARAM         KPJBU
057000000000     C                   CALL      'TRUL16R'
057100000000     C                   PARM                    KPJBA
057200000000     C                   MOVEL     KPJBU         PARAM
057300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
057400000000IF  2C     PARFLG        IFEQ      '3'
057500000000     C                   MOVEL     *BLANKS       WPDR
057600000000     C                   MOVEL     '1'           $ERR
057700000000     C                   MOVEL     MSG(11)       $MSG
057800000000X   2C                   ELSE
057900000000     C                   MOVEL     PARPDR        WPDR
058000000000     C                   MOVEL     PARRSC        WDPDR
058100000000E   2C                   ENDIF
058200000000     C* REIMPOSTA LA KPJBU
058300000000     C                   MOVEL     DKPJBU        KPJBU
058400000000E   1C                   ENDIF
058500000000     C* SOLO NUMERICO
058600000000IF  1C     $ERR          IFEQ      '0'
058700000000     C                   SETOFF                                       28
058800000000     C                   TESTN                   WPDR                 28
058900000000     C                   MOVE      WPDR          WALF1             1
059000000000IF  2C     *IN28         IFEQ      *OFF
059100000000     C     WALF1         ORLT      *ZEROS
059200000000     C                   MOVEL     '1'           $ERR
059300000000     C                   MOVEL     MSG(12)       $MSG
059400000000E   2C                   ENDIF
059500000000E   1C                   ENDIF
059600000000     C* ESISTENTE
059700000000IF  1C     $ERR          IFEQ      '0'
059800000821     C                   EVAL      KLZTIP = 'T '
059900000000     C                   MOVEL     WPDR          KLZPDR
060000000000     C     KEYTLZ        CHAIN     TNTLZ01L                           28
060100000000IF  2C     *IN28         IFEQ      *OFF
060200000000     C                   MOVEL     TLZRSC        WDPDR                          *DESCR.NE TRAZIONIST
060300000000X   2C                   ELSE
060400000000IF  3C     WPDR          IFEQ      '9999999'                                    *TARIFFA DI CARTELLO
060500000000     C                   MOVEL     DES999        WDPDR                          *DESCRIZ. AUTOMEZZO
060600000000X   3C                   ELSE
060700000000     C                   MOVEL     '1'           $ERR
060800000000     C                   MOVEL     MSG(12)       $MSG
060900000000E   3C                   ENDIF
061000000000E   2C                   ENDIF
061100000000E   1C                   ENDIF
061200000000     C*
061300000000     C                   ENDSR
061400000000     C*------------------------------------------------------------------------*
061500000000     C* CHKCAU - CONTROLLA LA CAUSALE DI CONTABILIZZAZIONE
061600000000     C*          INPUT  -> WCAU  - CAUSALE
061700000000     C*          OUTPUT -> WDCAU - DESCRIZIONE CAUSALE
061800000000     C*------------------------------------------------------------------------*
061900000000     C     CHKCAU        BEGSR
062000000000     C*
062100000000     C     *LIKE         DEFINE    V1CCAU        WCAU
062200000000     C     *LIKE         DEFINE    V1DCAU        WDCAU
062300000000     C*
062400000000     C                   MOVEL     *BLANKS       WDCAU
062500000000     C* ESISTENTE
062600000000IF  1C     WCAU          IFEQ      *BLANKS
062700000000     C                   MOVEL     '1'           $ERR
062800000000X   1C                   ELSE
062900000000     C                   SETOFF                                           28
063000000000     C                   Z-ADD     1             J
063100000000     C     WCAU          LOOKUP    CAU(J)                                 28
063200000000IF  2C     *IN28         IFEQ      *ON
063300000000     C                   MOVEL     DCAU(J)       WDCAU                          *DESCRIZ. AUTOMEZZO
063400000000X   2C                   ELSE
063500000000     C                   MOVEL     '1'           $ERR
063600000000E   2C                   ENDIF
063700000000E   1C                   ENDIF
063800000000     C*
063900000000     C                   ENDSR
064000010724     C*------------------------------------------------------------------------*
064100010724     C* CHKDIV - CONTROLLA LA DIVISA
064200010724     C*          INPUT  -> Wdiv  - divisa
064300010724     C*          OUTPUT ->
064400010724     C*------------------------------------------------------------------------*
064500010724     C     CHKdiv        BEGSR
064600010724     C*
064700010724     C     *LIKE         DEFINE    V1Cdiv        Wdiv
064800010724     C*
064900010724     C* ESISTENTE
065000010724     C                   SETOFF                                           28
065100010724     C                   Z-ADD     1             J
065200010724     C     Wdiv          LOOKUP    div(J)                                 28
065300011012IF  2C     *IN28         IFEQ      *OFF
065400010724     C                   MOVEL     '1'           $ERR
065500010724E   2C                   ENDIF
065600010724     C*
065700010724     C                   ENDSR
065800000000     C*------------------------------------------------------------------------*
065900000000     C* IMPVID - IMPOSTAZIONI VIDEO
066000000000     C*------------------------------------------------------------------------*
066100000000     C     IMPVID        BEGSR
066200000000     C*
066300000000     C* CALCOLA LA DATA CORRENTE
066400000000     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
066500000000     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
066600000000     C                   Z-ADD     WNUM8         G08DAT
066700000000     C                   Z-ADD     *ZEROS        G08INV
066800000000     C                   MOVEL     '0'           G08ERR
066900000000     C                   CALL      'XSRDA8'
067000000000     C                   PARM                    WLBDA8
067100000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
067200000000     C*
067300000000     C* 1°) DECODIFICA L'OPZIONE RICHIAMATA
067400000000     C* 2°) IMPOSTA LA PROTEZIONE DEI CAMPI -> 90=CAMPI CHIAVE / 91=ALTRI CAMPI
067500000000SL  1C                   SELECT
067600000000     C     D00OP1        WHENEQ    'F10'
067700000000     C                   MOVEL     DOPZ(1)       VTDFUN                         *INSERIMENTO
067800000000     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
067900000000     C     D00OP1        WHENEQ    'O02'
068000000000     C                   MOVEL     DOPZ(2)       VTDFUN                         *VARIAZIONE
068100000000     C                   SETON                                        90        *CHIAVE PROTETTA
068200000000     C     D00OP1        WHENEQ    'O04'
068300000000     C                   MOVEL     DOPZ(3)       VTDFUN                         *CANCELLAZIONE
068400000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
068500000000     C     D00OP1        WHENEQ    'O07'
068600000000     C                   MOVEL     DOPZ(4)       VTDFUN                         *RIPRISTINO
068700000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
068800000000     C                   OTHER
068900000000E   1C                   ENDSL
069000000000     C*
069100000000     C* CONTROLLA ESISTENZA E DISPONIBILITA' RECORD (NO SE INSERIMENTO)
069200000000     C* N.B. LEGGE IL RECORD VINCOLANDOLO SOLO SE NON E' IN INTERROGAZIONE
069300000000SL  1C                   SELECT
069400000000     C     D00OP1        WHENEQ    'F10'
069500000000     C                   OTHER
069600000000     C                   Z-ADD     D00TRN        KLLTRN                         *TRAINO
069700000000     C                   Z-ADD     D00DDE        KLLDDE                         *DATA DECORRENZA
069800000000     C                   MOVEL     D00TMZ        KLLTMZ                         *TIPO AUOTMEZZO
069900000000     C                   Z-ADD     D00PDR        KLLPDR                         *PADRONCINO
070000000000     C* INTERROGAZIONE
070100000000IF  2C     $TGES         IFEQ      'I'
070200010723     C     KEYTLL        CHAIN(N)  TITLL01L                           99
070300000000X   2C                   ELSE
070400010723     C     KEYTLL        CHAIN     TITLL01L                           9998
070500000000IF  3C     *IN98         IFEQ      *ON                                          *ALLOCATO
070600000000     C                   MOVEL     '1'           $ERR
070700000000     C                   MOVEL     '1'           $FINE
070800000000     C                   MOVEL     MSG(26)       D00MSG
070900000000E   3C                   ENDIF
071000000000E   2C                   ENDIF
071100000000IF  2C     *IN99         IFEQ      *ON                                          *INSESISTENTE
071200000000     C                   MOVEL     '1'           $ERR
071300000000     C                   MOVEL     '1'           $FINE
071400000000     C                   MOVEL     MSG(25)       D00MSG
071500000000E   2C                   ENDIF
071600000000E   1C                   ENDSL
071700000000     C* RIEMPE IL VIDEO
071800000000IF  1C     $ERR          IFEQ      '0'
071900000000SL  2C                   SELECT
072000000000     C     D00OP1        WHENEQ    'F10'
072100000000     C                   EXSR      AZZVID                                       *AZZERA CAMPI VIDEO
072200000000     C                   OTHER
072300000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
072400000000E   2C                   ENDSL
072500000000E   1C                   ENDIF
072600000000     C*
072700000000     C                   ENDSR
072800000000     C*------------------------------------------------------------------------*
072900000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
073000000000     C*------------------------------------------------------------------------*
073100000000     C     RECVID        BEGSR
073200000000     C*
073300000000     C                   MOVEL     TLLATB        V1CATB                         *ANNULLAMENTO
073400000000     C                   MOVEL     TLLTMZ        V1CTMZ                         *TIPO AUTOMEZZO
073500000000     C                   MOVEL     TLLPDR        V1CPDR                         *PADRONCINO
073600000000     C                   Z-ADD     TLLILI        V1CILI                         *IMPORTO LISTINO
073700000000     C                   Z-ADD     TLLIES        V1CIES                         *IMPORTO SU F.V.
073800010723     C                   MOVEL     TLLDIV        V1CDIV                         *DIVISA
073900010928     c                   movel     tlldiv        sav_v1cdiv
074000000000     C                   MOVEL     TLLCAU        V1CCAU                         *CAUSALE CONTAB.NE
074100000000     C* DECODIFICHE
074200000000     C                   SETOFF                                           28
074300000000     C                   Z-ADD     1             J
074400000000     C                   MOVEL     *BLANKS       V1DTMZ
074500000000     C     TLLTMZ        LOOKUP    TMZ(J)                                 28
074600000000IF  1C     *IN28         IFEQ      *ON
074700000000     C                   MOVEL     DTMZ(J)       V1DTMZ                         *TIPO AUTOMEZZO
074800000000E   1C                   ENDIF
074900000000     C*
075000000000     C                   MOVEL     *BLANKS       V1DPDR
075100000821     C                   EVAL      KLZTIP = 'T '
075200000000     C                   MOVEL     TLLPDR        KLZPDR
075300000000     C     KEYTLZ        CHAIN     TNTLZ01L                           28
075400000000IF  1C     *IN28         IFEQ      *OFF
075500000000     C                   MOVEL     TLZRSC        V1DPDR                         *TRAZIONISTA
075600000000E   1C                   ENDIF
075700000000     C*
075800000000     C                   SETOFF                                           28
075900000000     C                   Z-ADD     1             J
076000000000     C                   MOVEL     *BLANKS       V1DCAU
076100000000     C     TLLCAU        LOOKUP    CAU(J)                                 28
076200000000IF  1C     *IN28         IFEQ      *ON
076300000000     C                   MOVEL     DCAU(J)       V1DCAU                         *CAUSALE CONTAB.NE
076400000000E   1C                   ENDIF
076500000000     C*
076600000000     C                   ENDSR
076700000000     C*------------------------------------------------------------------------*
076800000000     C*  AZZVID - PULISCE TUTTI I CAMPI DEL VIDEO
076900000000     C*------------------------------------------------------------------------*
077000000000     C     AZZVID        BEGSR
077100000000     C*
077200010821      * recupero la divisa dalla tabella GED
077300010821     c                   clear                   dged
077400010821     c                   clear                   tibs02ds
077500010821     c                   movel     'L'           t02tla
077600010821     c                   movel     'C'           t02mod
077700010821     c                   movel     knsif         t02sif
077800010821     c                   movel     'GED'         t02cod
077900010821     c                   movel     '1'           t02ke1
078000010821     c                   call      'TIBS02R'
078100010821     c                   parm                    kpjba
078200010821     c                   parm                    tibs02ds
078300010821    5c                   movel     t02uni        dged
078400010821     c                   movel     §gedcn        v1cdiv
078500010928     c                   movel     §gedcn        sav_v1cdiv
078600010821
078700000000     C                   MOVEL     *BLANKS       V1CATB                         *ANNULLAMENTO
078800000000     C                   MOVEL     *BLANKS       V1CTMZ                         *TIPO AUTOMEZZO
078900000000     C                   MOVEL     *BLANKS       V1CPDR                         *TRAZIONISTA
079000000000     C                   Z-ADD     *ZEROS        V1CILI                         *IMPORTO LISTINO
079100000000     C                   Z-ADD     *ZEROS        V1CIES                         *IMPORTO SU F.V.
079200000000     C                   MOVEL     *BLANKS       V1CCAU                         *CAUSALE CONTAB.NE
079300000000     C* DECODIFICHE
079400000000     C                   MOVEL     *BLANKS       V1DTMZ                         *TIPO AUTOMEZZO
079500000000     C                   MOVEL     *BLANKS       V1DPDR                         *TRAZIONISTA
079600000000     C                   MOVEL     *BLANKS       V1DCAU                         *CAUSALE CONTAB.NE
079700000000     C*
079800000000     C                   ENDSR
079900000000     C*------------------------------------------------------------------------*
080000000000     C* CARTAB - CARICA LE TABELLE
080100000000     C*------------------------------------------------------------------------*
080200000000     C     CARTAB        BEGSR
080300000000     C*--------------------
080400000000     C* TIPI AUTOMEZZI
080500000000     C*--------------------
080600000000     C                   Z-ADD     *ZEROS        J                 4 0
080700020716     c                   Z-Add     1             KtbKut
080800000000     C                   MOVEL     'TU'          KTBCOD
080900000000     C     KE2TAB        SETLL     TABEL00F
081000000000     C     KE2TAB        READE     TABEL00F                               99
081100000000DO  1C     *IN99         DOWEQ     *OFF
081200000000IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
081300000000     C                   ADD       1             J
081400000000     C                   MOVEL     TBLKEY        TMZ(J)                         *CODICE
081500000000     C                   MOVEL     TBLUNI        DSTU
081600000000     C                   MOVEL     §TUDES        DTMZ(J)                        *DESCRIZIONE
081700000000E   2C                   ENDIF
081800000000     C     KE2TAB        READE     TABEL00F                               99
081900000000E   1C                   ENDDO
082000000000     C*--------------------
082100000000     C* CAUSALI CONTABILIZZAZIONE
082200000000     C*--------------------
082300020716     c                   If        DutLpo = 'S'
082400000821     c                   OPEN      ANATB01L
082500000000     C                   Z-ADD     *ZEROS        J
082600000218     C                   MOVE      KNSIF         WCOMO4            4
082700000218     C                   MOVEL     WCOMO4        WSOC              3
082800000218     C                   movel     WSOC          KAtazi
082900000210     C                   MOVEL     'Y4Z'         KatCOD
083000000210     C     KEATAB        CHAIN     ANATB01L                           99
083100000821IF  2C     *IN99         IFEQ      *ON
083200000218     C                   movel     '888'         KAtazi
083300000210     C     KEATAB        CHAIN     ANATB01L                           99
083400000821E   2C                   ENDIF
083500000821DO  2C     *IN99         DOWEQ     *OFF
083600000210     C                   ADD       1             J
083700000210     C                   MOVEL     ATBKEY        CAU(J)
083800000211     C                   MOVEL     ATBUNI        ANGY4ZDS
083900000211     C                   MOVEL     §4ZDES        DCAU(J)
084000000210     C     KEATAB        READE     ANATB01L                               99
084100000821E   2C                   ENDDO
084200000821     c                   CLOSE     ANATB01L
084300000821E   1C                   ENDIF
084400010724     C*--------------------
084500010724     C* DIVISA
084600010724     C*--------------------
084700010724     C                   Z-ADD     *ZEROS        J
084800020716     c                   Z-Add     1             KtbKut
084900010724     C                   MOVEL     'CV'          KTBCOD
085000010724     C     KE2TAB        SETLL     TABEL00F
085100010724     C     KE2TAB        READE     TABEL00F                               99
085200010724DO  1C     *IN99         DOWEQ     *OFF
085300010724IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
085400010724     C                   ADD       1             J
085500010724     C                   MOVEL     TBLKEY        DIV(J)                         *CODICE
085600010724     C                   MOVEL     TBLUNI        DSCV
085700010822     C                   MOVEL     §CVTRA        cvtra(j)
085800010822     C                   MOVEL     §CVFDC        cvfdc(j)
085900010724E   2C                   ENDIF
086000010724     C     KE2TAB        READE     TABEL00F                               99
086100010724E   1C                   ENDDO
086200000000     C*
086300000000     C                   ENDSR
086400000000     C*------------------------------------------------------------------------*
086500000000     C* *INZSR - IMPOSTAZIONI INIZIALI
086600000000     C*------------------------------------------------------------------------*
086700000000     C     *INZSR        BEGSR
086800000000     C*--------------------
086900000000     C* RICEVIMENTO PARAMETRI
087000000000     C*--------------------
087100000000     C     *ENTRY        PLIST
087200000000     C                   PARM                    KPJBA
087300000000     C                   MOVEL     KPJBU         DS00
087400000000     C*--------------------
087500000000     C* VARIABILI RIFERITE AL DATA BASE
087600000000     C*--------------------
087700000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
087800000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
087900000210     C     *LIKE         DEFINE    ATBCOD        KATCOD
088000000210     C     *LIKE         DEFINE    ATBAZI        KATAZI
088100000000     C     *LIKE         DEFINE    TLZPDR        KLZPDR                         *TNTLZ00F
088200000821     C     *LIKE         DEFINE    TLZTIP        KLZTIP
088300010723     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
088400000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
088500000000     C     *LIKE         DEFINE    TLLTMZ        KLLTMZ
088600000000     C     *LIKE         DEFINE    TLLPDR        KLLPDR
088700000000     C     *LIKE         DEFINE    KPJBU         DKPJBU                         *DEPOSITI
088800000000     C*--------------------
088900000000     C* CHIAVI DI LETTURA
089000000000     C*--------------------
089100000000     C* LETTURA TABEL00F - COMPLETA
089200000000     C     KE2TAB        KLIST
089300000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
089400000000     C                   KFLD                    KTBCOD                         *TABELLA
089500000210     C* LETTURA ANATB01L - PARZIALE
089600000210     C     KEATAB        KLIST
089700000210     C                   KFLD                    KATCOD
089800000210     C                   KFLD                    KATAZI
089900000000     C* LETTURA TNTLZ01L - COMPLETA
090000000000     C     KEYTLZ        KLIST
090100000821     C                   KFLD                    KLZTIP                         *TIPO
090200000821     C                   KFLD                    KLZPDR                         *CODICE
090300010723     C* LETTURA TITLL01L - COMPLETA
090400000000     C     KEYTLL        KLIST
090500000000     C                   KFLD                    KLLTRN                         *NUMERO TRAINO
090600000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
090700000000     C                   KFLD                    KLLTMZ                         *TIPO AUTOMEZZO
090800000000     C                   KFLD                    KLLPDR                         *TRAZIONISTA
090900000000     C*--------------------
091000000000     C* VARIABILI DI CONTROLLO
091100000000     C*--------------------
091200000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
091300000000     C                   MOVE      D00OP0        INTGES            2
091400000000IF  1C     INTGES        IFEQ      '02'
091500000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
091600000000X   1C                   ELSE
091700000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
091800000000E   1C                   ENDIF
091900000000     C*
092000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
092100000000     C                   EXSR      INZSTD
092200000000     C*
092300000000     C* CARICA LE TABELLE
092400000000     C                   EXSR      CARTAB
092500000000     C*
092600000000     C* IMPOSTAZIONI VIDEO
092700000000     C                   EXSR      IMPVID
092800000000     C*
092900000000     C* EMETTE VIDEATA
093000000000IF  1C     $ERR          IFEQ      '0'                                          ERRORE IN ALLOCAZION
093100000000     C                   EXSR      CARV1
093200000000E   1C                   ENDIF
093300000000     C*
093400000000     C                   ENDSR
093500000000     C*------------------------------------------------------------------------*
093600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
093800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
093900000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
094000000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
094100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
094300000000     C*------------------------------------------------------------------------*
094400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
094500000000     C*------------------------------------------------------------------------*
094600000000     C     INZSTD        BEGSR
094700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
094800000000     C                   CALL      'XKEYS'
094900000000     C                   PARM                    DSKEYS
095000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
095100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
095200000000     C     XMSGPA        PLIST
095300000000     C                   PARM                    DSMSMS
095400000000     C                   PARM                    DSMSPG
095500000000     C                   PARM                    DSMSKE
095600000000     C                   PARM                    DSMSER
095700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
095800010725     C                   MOVEL     'TNTL39R'     DSMSPG                         *NOME PGM CHIAMANTE
095900010725     C                   MOVEL     'TNTL39R'     VMCQUE                         *CODA MESSAGGI DESTI
096000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
096100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
096200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
096300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
096400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
096500000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
096600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
096700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
096800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
096900000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
097000000000     C* LANCIO ROUTINE PER VERIFICARE SE
097100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
097200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
097300020716
097400020716     c     *dtaara       define    §azute        azuteds
097500020716     c     *dtaara       define    §datiute      ddatiute
097600020716
097700020716     c                   in(E)     *dtaara
097800020716     c                   if        %Error or rsut = *blanks
097900020716     c                   clear                   tibs34ds
098000020716     c                   call      'TIBS34R'
098100020716     c                   parm                    Tibs34Ds
098200020716     c                   in        *dtaara
098300020716     c                   endif
098400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
098500000000     C                   BITOFF    '01234567'    POSMI1            1
098600000000     C                   BITOFF    '01234567'    POSMI2            1
098700000000     C                   BITOFF    '01234567'    POSMA1            1
098800000000     C                   BITOFF    '01234567'    POSMA2            1
098900000000     C                   BITON     '34'          POSMI1            1            *18
099000000000     C                   BITON     '7'           POSMI2            1            *01
099100000000     C                   BITON     '34'          POSMA1            1            *18
099200000000     C                   BITON     '0'           POSMA2            1            *80
099300000000     C                   MOVE      POSMI2        POSMIN            2
099400000000     C                   MOVEL     POSMI1        POSMIN
099500000000     C                   MOVE      POSMA2        POSMAX            2
099600000000     C                   MOVEL     POSMA1        POSMAX
099700000000     C                   ENDSR
099800000000     C*------------------------------------------------------------------------*
099900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
100000000000     C*------------------------------------------------------------------------*
100100000000     C     FUNSTD        BEGSR
100200000000     C                   SELECT
100300000000     C     KEY           WHENEQ    F3
100400000000     C                   EXSR      F03STD
100500000000     C     KEY           WHENEQ    F12
100600000000     C                   EXSR      F12STD
100700000000     C     KEY           WHENEQ    F24
100800000000     C                   EXSR      F24STD
100900000000     C                   OTHER
101000000000     C                   MOVEL     MSG(2)        $MSG
101100000000     C                   EXSR      CARMSG
101200000000     C                   ENDSL
101300000000     C                   ENDSR
101400000000     C*------------------------------------------------------------------------*
101500000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
101600000000     C*------------------------------------------------------------------------*
101700000000     C     F03STD        BEGSR
101800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
101900000000     C                   MOVEL     '1'           $F03
102000000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
102100000000     C                   MOVEL     '1'           $FINE
102200951212     C* PULISCE IL MESSAGGIO
102300951212     C                   MOVEL     *BLANKS       D00MSG
102400000000     C                   ENDSR
102500000000     C*------------------------------------------------------------------------*
102600000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
102700000000     C*------------------------------------------------------------------------*
102800000000     C     F12STD        BEGSR
102900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
103000000000     C                   MOVEL     '1'           $F12
103100000000     C* IMPOSTO FLAG DI FIN PGM
103200000000     C                   MOVEL     '1'           $FINE
103300951212     C* PULISCE IL MESSAGGIO
103400951212     C                   MOVEL     *BLANKS       D00MSG
103500000000     C                   ENDSR
103600000000     C*------------------------------------------------------------------------*
103700000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
103800000000     C*------------------------------------------------------------------------*
103900000000     C     F24STD        BEGSR
104000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
104100000000     C                   EXSR      OUTFUN
104200000000     C                   ENDSR
104300000000     C*------------------------------------------------------------------------*
104400000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
104500000000     C*          CODA MESSAGGI DEL PGM
104600000000     C*------------------------------------------------------------------------*
104700000000     C     CARMSG        BEGSR
104800000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
104900000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
105000000000     C     MSGULT        IFEQ      0
105100000000     C                   MOVEL     '1'           *IN40
105200010725     C                   WRITE     TL39DVK
105300000000     C                   MOVEL     '0'           *IN40
105400000000     C                   ENDIF
105500000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
105600000000     C                   MOVEL     $MSG          DSMSMS
105700000000     C                   CALL      'XMSG'        XMSGPA
105800000000     C     DSMSER        IFEQ      0
105900000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
106000000000     C                   ENDIF
106100000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
106200000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
106300010725     C                   WRITE     TL39DVM                                      *SFL MESS
106400000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
106500000000     C                   MOVEL     *BLANKS       $MSG
106600000000     C                   ENDSR
106700000000     C*------------------------------------------------------------------------*
106800000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
106900000000     C*------------------------------------------------------------------------*
107000000000     C     OUTMSG        BEGSR
107100000000     C* VISUALIZZAZIONE MESSAGGIO
107200000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
107300010725     C                   WRITE     TL39DVK                                      *CONTR MESS
107400000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
107500000000     C                   Z-ADD     *ZEROS        MSGULT
107600000000     C                   ENDSR
107700000000     C*------------------------------------------------------------------------*
107800000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
107900000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
108000000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
108100000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
108200000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
108300000000     C*------------------------------------------------------------------------*
108400000000     C     CARFUN        BEGSR
108500000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
108600000000     C     1             DO        31            I                 5 0          --- 1 -->
108700000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
108800000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
108900000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
109000020716     c                   If        DutLpo = 'S'
109100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
109200000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
109300000000     C                   ELSE                                                   *INTERROG.
109400000000     C                   MOVEL     FUNASI        FAT(I)
109500000000     C                   ENDIF
109600000000     C                   ELSE                                                   <-- 2 --> *FILIALE
109700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
109800000000     C                   MOVEL     FUNAFG        FAT(I)
109900000000     C                   ELSE                                                   *INTERROG.
110000000000     C                   MOVEL     FUNAFI        FAT(I)
110100000000     C                   ENDIF
110200000000     C                   ENDIF                                                  <-- 2 ---
110300000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
110400000000     C                   MOVEL     USER          WEDP              3            *PROFILO
110500000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
110600000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
110700000000     C                   ELSE
110800000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
110900000000     C                   ENDIF
111000000000     C                   ENDIF
111100000000     C                   ENDDO                                                  <-- 1 ---
111200000000     C                   ENDSR
111300000000     C*------------------------------------------------------------------------*
111400000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
111500000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
111600000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
111700000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
111800000000     C*------------------------------------------------------------------------*
111900000000     C     OUTFUN        BEGSR
112000000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
112100000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
112200000000     C                   ENDDO
112300000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
112400000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
112500000000     C     1             DO        31            I
112600000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
112700000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
112800000000     C                   ADD       1             FUNNAT
112900000000     C                   ENDIF
113000000000     C                   ENDDO
113100000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
113200000000     C     FUNNAT        IFGT      8
113300000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
113400000000     C                   ENDIF
113500000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
113600000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
113700000000     C                   Z-ADD     8             FUNVIS            5 0
113800000000     C     FAT(23)       IFEQ      'S'
113900000000     C                   SUB       1             FUNVIS
114000000000     C                   ENDIF
114100000000     C     FAT(24)       IFEQ      'S'
114200000000     C                   SUB       1             FUNVIS
114300000000     C                   ENDIF
114400000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
114500000000     C                   Z-ADD     *ZEROS        W                 5 0
114600000000     C                   ADD       1             FUNULT
114700000000     C     FUNULT        IFGT      31
114800000000     C                   Z-ADD     1             FUNULT
114900000000     C                   ENDIF
115000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
115100000000     C     FUNULT        DO        31            I                              --- 1 -->
115200000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
115300000000     C     W             IFLT      FUNVIS                                       --- 2 -->
115400000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
115500000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
115600000000     C     FDE(I)        ANDNE     *BLANKS
115700000000     C     I             IFEQ      23                                           *TOLGO F23
115800000000     C     I             OREQ      24                                           *E F24
115900000000     C                   ELSE
116000000000     C                   ADD       1             W
116100000000     C                   MOVEL     FDE(I)        FUV(W)
116200000000     C                   ENDIF
116300000000     C                   ENDIF                                                  <-- 3 ---
116400000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
116500000000     C                   Z-ADD     I             FUNULT
116600000000     C                   ENDIF                                                  <-- 2 ---
116700000000     C                   ENDDO                                                  <-- 1 ---
116800000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
116900000000     C                   MOVEL     FUV(1)        VFD001
117000000000     C                   MOVEL     FUV(2)        VFD002
117100000000     C                   MOVEL     FUV(3)        VFD003
117200000000     C                   MOVEL     FUV(4)        VFD004
117300000000     C                   MOVEL     FUV(5)        VFD005
117400000000     C                   MOVEL     FUV(6)        VFD006
117500000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
117600000000     C     FAT(24)       ANDEQ     'N'
117700000000     C                   MOVEL     FUV(7)        VFD007
117800000000     C                   MOVEL     FUV(8)        VFD008
117900000000     C                   ELSE                                                   <-- 1 -->
118000000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
118100000000     C     FAT(24)       ANDEQ     'S'
118200000000     C                   MOVEL     FDE(23)       VFD007
118300000000     C                   MOVEL     FDE(24)       VFD008
118400000000     C                   ELSE                                                   <-- 2 -->
118500000000     C                   MOVEL     FUV(7)        VFD007
118600000000     C     FAT(23)       IFEQ      'S'
118700000000     C                   MOVEL     FDE(23)       VFD008
118800000000     C                   ELSE
118900000000     C                   MOVEL     FDE(24)       VFD008
119000000000     C                   ENDIF
119100000000     C                   ENDIF                                                  <-- 2 ---
119200000000     C                   ENDIF                                                  <-- 1 ---
119300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
119400000000     C                   ENDSR
119500000000     C*------------------------------------------------------------------------*
119600000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
119700000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
119800000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
119900000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
120000000000     C*------------------------------------------------------------------------*
120100000000     C     CONFUN        BEGSR
120200000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
120300000000     C     1             DO        31            I                              --- 1 -->
120400000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
120500000000     C* CONTROLLO SE IL TASTO E' ATTIVO
120600000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
120700000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
120800000000     C                   EXSR      CARMSG
120900000000     C                   MOVEL     '1'           $ERR
121000000000     C                   ENDIF                                                  <-- 3 ---
121100000000     C                   ENDIF                                                  <-- 2 ---
121200000000     C                   ENDDO                                                  <-- 1 ---
121300000000     C                   ENDSR
121400000000     C*------------------------------------------------------------------------*
121500000000** - MSG MESSAGGI
121600010725TNTL39R- Tasto funzionale non valido.                                          1
121700010725TNTL39R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
121800010725TNTL39R- Opzione non valida.                                                   3
121900010725TNTL39R- Opzione abilitata ma non gestita. Avvertire CED.                      4
122000010725TNTL39R- Inizio elenco.                                                        5
122100010725TNTL39R- Fine elenco.                                                          6
122200010725TNTL39R- 7
122300010725TNTL39R- 8
122400010725TNTL39R- 9
122500010725TNTL39R- Tipo automezzo errato                                                10
122600010725TNTL39R- Non esistono trazionisti con chiave alfabetica rischiesta            11
122700010725TNTL39R- Codice trazionista errato                                            12
122800180102TNTL39R- Inserire l'importo di listino (con valore positivo)                  13
122900010725TNTL39R- L'importo da esporre su FV deve essere = zero o = importo di listino 14
123000010725TNTL39R- Causale di contabilizzazione errata                                  15
123100010725TNTL39R- Divisa errata                                                        16
123200010822TNTL39R- Non ammessi importi con decimali per la divisa indicata              17
123300010928TNTL39R- Se modificata la divisa ridigitare il relativo importo               18
123400011012TNTL39R- Immettere la divisa                                                  19
123500011012TNTL39R- Divisa non ammessa per i traini                                      20
123600010725TNTL39R- 21
123700010725TNTL39R- 22
123800010725TNTL39R- 23
123900010725TNTL39R- Richiesta funzione per record già esistente                          24
124000010725TNTL39R- Record inesistente                                                   25
124100010725TNTL39R- Record momentaneamente allocato: riprovare più tardi                 26
124200010725TNTL39R- Inserimento effettuato                                               27
124300010725TNTL39R- Variazione effettuata                                                28
124400010725TNTL39R- Annullamento effettuato                                              29
124500010725TNTL39R- Ripristino effettuato                                                30
124600000000** - FUN  FUNZIONI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
124700000000F1    -                   -    NN     NN
124800000000F2    -                   -    NN     NN
124900000000F3    -F3=Fine            -    NN     NN
125000000000F4    -                   -    NN     NN
125100000000F5    -                   -    NN     NN
125200000000F6    -                   -    NN     NN
125300000000F7    -                   -    NN     NN
125400000000F8    -                   -    NN     NN
125500000000F9    -                   -    NN     NN
125600000000F10   -                   -    NN     NN
125700000000F11   -                   -    NN     NN
125800000000F12   -F12=Ritorno        -    SS     SS
125900000000F13   -                   -    NN     NN
126000000000F14   -                   -    NN     NN
126100000000F15   -                   -    NN     NN
126200000000F16   -                   -    NN     NN
126300000000F17   -                   -    NN     NN
126400000000F18   -                   -    NN     NN
126500000000F19   -                   -    NN     NN
126600000000F20   -                   -    NN     NN
126700000000F21   -                   -    NN     NN
126800000000F22   -                   -    NN     NN
126900000000F23   -                   -    NN     NN
127000000000F24   -F24=Altri tasti    -    NN     NN
127100000000ENTER -                   -    SS     SS
127200000000ROLLUP-                   -    NN     NN
127300000000ROLLDN-                   -    NN     NN
127400000000HELP  -                   -    NN     NN
127500000000PRINT -                   -    NN     NN
127600000000BSPACE-                   -    NN     NN
127700000000CLEAR -                   -    NN     NN
127800000000** DOPZ  (OPZIONE RICHIAMATA)
127900000000* INSERIMENTO *     1
128000000000*  VARIAZIONE *     2
128100000000*CANCELLAZIONE*     3
128200000000*  RIPRISTINO *     4
1283000000005
