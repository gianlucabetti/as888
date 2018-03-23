000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - METRICUBI ASSEGNATI - ROUTINE DI GESTIONE
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL09D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL09DVM:MSGULT)
001000000000     FTABEL00F  IF   E           K DISK
001100000000     FAZORG01L  IF   E           K DISK
001200000000     FAZCAE01L  IF   E           K DISK
001300000000     FTNTLT01L  UF A E           K DISK
001400000000     F                                     COMMIT
001500000000     FTNTLR01L  UF A E           K DISK
001600000000     F                                     COMMIT
001700000000     FTNTLM01L  UF A E           K DISK
001800000000     F                                     COMMIT
001900000000     D*------------------------------------------------------------------------*
002000000000     D* SCHIERE
002100000000     D*------------------------------------------------------------------------*
002200000000     D*-------------------
002300000000     D* MESSAGGI E FUNZIONI
002400000000     D*-------------------
002500000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
002600000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002700000000     D*-------------------
002800000000     D* DECODIFICA OPZIONE RICHIAMATA
002900000000     D*-------------------
003000000000     D DOPZ            S             20    DIM(5) CTDATA PERRCD(1)              *DECODIFICA OPZIONE
003100000000     D*-------------------
003200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003300000000     D*-------------------
003400000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003500000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003600000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003700000000     D*-------------------
003800000000     D* TABELLA DESCRIZIONI A VIDEO
003900000000     D*-------------------
004000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004100000000     D*-------------------
004200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004300000000     D*-------------------
004400000000     D KEYS            S              1    DIM(40)
004500000000     D*-------------------
004600000000     D* FILIALI
004700000000     D*-------------------
004800971125     D ORG             S              3  0 DIM(500)                             *CODICE
004900971125     D TFP             S              3  0 DIM(500)                             *TERMINAL PARTENZA
005000971125     D TFA             S              3  0 DIM(500)                             *TERMINAL ARRIVO
005100971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
005200000000     D*-------------------
005300000000     D* SCHIERE DI MEMORIZZAZIONI DATI TRAINO
005400000000     D*-------------------
005500000000     D***
005600000000     D* FERMATE
005700000000     D***
005800000000     D FTFM            S              1    DIM(50)                              *TIPO
005900000000     D FFIL            S              3  0 DIM(50)                              *FILIALE
006000000000     D FSCA            S              1    DIM(50)                              *SCARICO
006100000000     D FCAR            S              1    DIM(50)                              *CARICO
006200000000     D FKMT            S              5  0 DIM(50)                              *KILOMETRI
006300000000     D FTEM            S              4  0 DIM(50)                              *TEMPO
006400000000     D FGFV            S              1    DIM(50)                              *GIORNO APERTURA F.V
006500000000     D*-------------------
006600000000     D* SCHIERE DI LAVORO (MASSIMO 2 X ANDATA E RITORNO)
006700000000     D*-------------------
006800000000     D JSEA            S              3  0 DIM(2)                               *SEQUENZA ARRIVO
006900000000     D JTFA            S              1    DIM(2)                               *TIPO FERMATA ARRIVO
007000000000     D*---
007100000000     D JSEP            S              3  0 DIM(2)                               *SEQUENZA PARTENZA
007200000000     D JTFP            S              1    DIM(2)                               *TIPO FERMATA PARTEN
007300000000     D*------------------------------------------------------------------------*
007400000000     D* INPUT
007500000000     D*------------------------------------------------------------------------*
007600000000     D*-------------------
007700000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
007800000000     D*-------------------
007900000000     D DSFUN         E DS
008000000000     D  FUNRIG                 1     40
008100000000     D*-------------------
008200000000     D* DS PER GESTIONE TASTI FUNZIONALI
008300000000     D*-------------------
008400000000     D DSKEYS        E DS
008500000000     D  KEYRIG                 1     40
008600000000     D*-------------------
008700000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
008800000000     D*-------------------
008900000000     D                SDS
009000000000     D  USER                 254    263
009100000000     D*-------------------
009200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
009300000000     D*-------------------
009400000000     D DEVDS1          DS
009500000000     D  KEY                  369    369
009600000000     D  CURSOR               370    371
009700000000     D  FRTSFL               378    379B 0
009800000000     D*-------------------
009900000000     D* ARCHITETTURA
010000000000     D*-------------------
010100000000     D KPJBA         E DS
010200000000     D*---------------------
010300000000     D* DS ESTERNA DI PROCEDURA
010400000000     D*---------------------
010500000000     D DS00          E DS                  EXTNAME(TNTL00DS)
010600000000     D*-------------------
010700000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
010800000000     D*-------------------
010900000000     D DSMSG         E DS
011000960208     D*---------------------
011100960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
011200960208     D*---------------------
011300960208     D OG148         E DS
011400020715
011500020715     d DdatiUte      e ds
011600020715     d AzuteDs       e ds                  ExtName(Azute00f)
011700020715     d Tibs34Ds      e ds                  Inz
011800020715
011900971020     D*------------------
012000971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
012100971020     D*------------------
012200971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
012300000000     D*------------------
012400000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
012500000000     D*------------------
012600000000     D WLBDA8          DS
012700000000     D  G08DAT                 1      8  0
012800000000     D  G08INV                 9     16  0
012900000000     D  G08ERR                17     17
013000000000     D  G08TGI                18     22  0
013100000000     D*---------------------
013200000000     D* DS TIPI TRAINI
013300000000     D*---------------------
013400000000     D DSTV          E DS
013500000000     C*------------------------------------------------------------------------*
013600000000     C* MAIN LINE
013700000000     C*------------------------------------------------------------------------*
013800000000     C*
013900000000DO  1C     $FINE         DOWEQ     '0'
014000000000     C                   MOVEL     '0'           $ERR
014100000000     C*
014200000000IF  2C     $GES          IFEQ      'V1'
014300000000     C                   EXSR      GESV1
014400000000E   2C                   ENDIF
014500000000     C*
014600000000E   1C                   ENDDO
014700000000     C*
014800000000IF  1C* SE E' STATO PREMUTO F03 O F12 IMPOSTA FLAG DELLA DS DI PROCEDURA
014900000000     C     $F03          IFEQ      '1'
015000000000     C                   MOVEL     '1'           D00F03
015100000000E   1C                   ENDIF
015200000000     C     $F12          IFEQ      '1'
015300000000     C                   MOVEL     '1'           D00F12
015400000000E   1C                   ENDIF
015500971020     C* OPERAZIONI FINALI
015600971020     C                   EXSR      FINSR
015700000000     C*
015800000000     C                   MOVEL     DS00          KPJBU
015900000000     C                   SETON                                        LR
016000000000     C*------------------------------------------------------------------------*
016100000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
016200000000     C*------------------------------------------------------------------------*
016300000000     C     CARV1         BEGSR
016400000000     C*
016500000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016600000000     C*
016700000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016800000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016900000000     C*
017000000000     C                   WRITE     TL09DVT                                      *TESTATA
017100000000     C*---                 WRITETL09DVF                    *FUNZIONI
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500000000     C* GESV1  - GESTIONE PRIMO VIDEO
017600000000     C*------------------------------------------------------------------------*
017700000000     C     GESV1         BEGSR
017800000000     C*
017900000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
018000000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
018100000000E   1C                   ENDIF
018200000000     C*
018300000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
018400000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
018500000000E   1C                   ENDIF
018600000000     C*
018700000000     C                   EXFMT     TL09DV1                                      *TEMPO DELIVERY
018800000000     C*
018900000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
019000000000     C*
019100000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
019200000000SL  2C                   SELECT
019300000000     C     KEY           WHENEQ    ENTER
019400000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
019500000000     C                   OTHER
019600000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
019700000000E   2C                   ENDSL
019800000000E   1C                   ENDIF
019900000000     C*
020000000000     C                   ENDSR
020100000000     C*------------------------------------------------------------------------*
020200000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
020300000000     C*------------------------------------------------------------------------*
020400000000     C     ENTV1         BEGSR
020500000000     C*
020600000000     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
020700000000IF  1C     D00OP1        IFEQ      'F10'
020800000000     C                   EXSR      INSERI                                       *INSERIMENTO
020900000000E   1C                   ENDIF
021000000000IF  1C     D00OP1        IFEQ      'O02'
021100000000     C                   EXSR      MODIFI                                       *MODIFICA
021200000000E   1C                   ENDIF
021300000000IF  1C     D00OP1        IFEQ      'O04'
021400000000     C                   EXSR      ANNULL                                       *ANNULLAMENTO
021500000000E   1C                   ENDIF
021600000000IF  1C     D00OP1        IFEQ      'O07'
021700000000     C                   EXSR      RIPRIS                                       *RIPRISTINO
021800000000E   1C                   ENDIF
021900000000     C*
022000000000     C                   ENDSR
022100000000     C*------------------------------------------------------------------------*
022200000000     C* INSERI - FUNZIONE DI INSERIMENTO
022300000000     C*------------------------------------------------------------------------*
022400000000     C     INSERI        BEGSR
022500000000     C*
022600000000     C* CONTROLLA I CAMPI IMMESSI
022700000000     C                   EXSR      DECODI
022800000000     C* NO ERRORI
022900000000IF  1C     $ERR          IFEQ      '0'
023000000000     C                   Z-ADD     D00TRN        TLMTRN                         *TRAINI
023100000000     C                   Z-ADD     D00DDE        TLMDDE                         *DATA DECORRENZA
023200000000     C                   MOVEL     V1CTFP        TLMTFP                         *PARTENZA
023300000000     C                   MOVEL     V1CTFA        TLMTFA                         *SCARICO
023400000000     C                   MOVEL     V1CTFF        TLMTFF                         *ARRIVO
023500960423     C                   Z-ADD     V1CMCA        TLMMCA                         *MCUBI NORMALI
023600960423     C                   Z-ADD     V1CMCB        TLMMCB                         *MCUBI BIS
023700960105     C                   Z-ADD     V1CMCT        TLMMCT                         *MCUBI TRAINI SEDE
023800000000     C                   Z-ADD     DATCOR        TLMDIR                         *DATA INSERIMENTO
023900000000     C                   Z-ADD     *ZEROS        TLMDUV                         *DATA VARIAZIONE
024000000000     C                   MOVEL     *BLANKS       TLMFTR                         *FLAG TRASMISSIONE
024100000000     C                   Z-ADD     *ZEROS        TLMDTR                         *DATA TRASMISSIONE
024200000000     C                   MOVEL     *BLANKS       TLMATB                         *ANNULLAMENTO
024300000000     C*
024400000000     C                   WRITE     TNTLM000                                     *SCRIVE
024500000000     C*
024600000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
024700000000     C                   MOVEL     MSG(27)       D00MSG                         *CON MESSAGGIO
024800000000E   1C                   ENDIF
024900000000     C*
025000000000     C                   ENDSR
025100000000     C*------------------------------------------------------------------------*
025200000000     C* MODIFI - FUNZIONE DI MODIFICA
025300000000     C*------------------------------------------------------------------------*
025400000000     C     MODIFI        BEGSR
025500000000     C*
025600000000     C* CONTROLLA I CAMPI IMMESSI
025700000000     C                   EXSR      DECODI
025800000000     C* NO ERRORI
025900000000IF  1C     $ERR          IFEQ      '0'
026000960423     C                   Z-ADD     V1CMCA        TLMMCA                         *MCUBI NORMALI
026100960423     C                   Z-ADD     V1CMCB        TLMMCB                         *MCUBI BIS
026200960105     C                   Z-ADD     V1CMCT        TLMMCT                         *MCUBI TRAINI SEDE
026300000000     C                   Z-ADD     DATCOR        TLMDUV                         *DATA VARIAZIONE
026400000000     C                   MOVEL     *BLANKS       TLMFTR                         *FLAG TRASMISSIONE
026500000000     C                   MOVEL     *BLANKS       TLMATB                         *ANNULLAMENTO
026600000000     C*
026700000000     C                   UPDATE    TNTLM000                                     *AGGIORNA
026800000000     C*
026900000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
027000000000     C                   MOVEL     MSG(28)       D00MSG                         *CON MESSAGGIO
027100000000E   1C                   ENDIF
027200000000     C*
027300000000     C                   ENDSR
027400000000     C*------------------------------------------------------------------------*
027500000000     C* ANNULL - FUNZIONE DI ANNULLAMENTO
027600000000     C*------------------------------------------------------------------------*
027700000000     C     ANNULL        BEGSR
027800000000     C*---
027900000000     C* RECORD GIA'TRASMESSO --> ANNULLAMENTO LOGICO
028000000000     C* RECORD NON TRASMESSO --> ANNULLAMENTO FISICO
028100000000     C*---
028200130529IF  1C**   TLMDTR        IFGT      *ZEROS
028300130529     C**                 Z-ADD     DATCOR        TLMDUV                         *DATA VARIAZIONE
028400130529     C**                 MOVEL     *BLANKS       TLMFTR                         *FLAG TRASMISSIONE
028500130529     C**                 MOVEL     'A'           TLMATB                         *ANNULLAMENTO
028600130529     C**                 UPDATE    TNTLM000                                     *AGGIORNA
028700130529X   1C**                 ELSE
028800000000     C                   DELETE    TNTLM000
028900130529E   1C**                 ENDIF
029000000000     C*
029100000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
029200000000     C                   MOVEL     MSG(29)       D00MSG                         *CON MESSAGGIO
029300000000     C*
029400000000     C                   ENDSR
029500000000     C*------------------------------------------------------------------------*
029600000000     C* RIPRIS - FUNZIONE DI RIPRISTINO
029700000000     C*------------------------------------------------------------------------*
029800000000     C     RIPRIS        BEGSR
029900000000     C*
030000000000     C                   Z-ADD     DATCOR        TLMDUV                         *DATA VARIAZIONE
030100000000     C                   MOVEL     *BLANKS       TLMFTR                         *FLAG TRASMISSIONE
030200000000     C                   MOVEL     *BLANKS       TLMATB                         *ANNULLAMENTO
030300000000     C*
030400000000     C                   UPDATE    TNTLM000                                     *AGGIORNA
030500000000     C*
030600000000     C                   MOVEL     '1'           $FINE                          *ESCE DAL PGM
030700000000     C                   MOVEL     MSG(30)       D00MSG                         *CON MESSAGGIO
030800000000     C*
030900000000     C                   ENDSR
031000000000     C*------------------------------------------------------------------------*
031100000000     C* DECODI - CONTROLLA LE SCHIERE DEL GIORNO
031200000000     C*------------------------------------------------------------------------*
031300000000     C     DECODI        BEGSR
031400000000     C*?I CONTROLLI DI QUESTA ROUTINE SONO DUPLICATI NEL PROGRAMMA "TNTL50R" CHE
031500000000     C*?CONTROLLA LA CONGRUITA' DEL DATABASE TRAINI.
031600000000     C*?OGNI MODIFICA APPORTATA A QUESTA ROUTINE QUINDI, DEVE ESSERE RICOPIATA
031700000000     C*?DENTRO IL PROGRAMMA ONDE EVITARE DISALLINEAMENTI.
031800000000     C*
031900000000     C* REIMPOSTA INDICATORI DI ERRORE
032000000000     C                   SETOFF                                       606162
032100960423     C                   SETOFF                                       636465
032200000000     C                   MOVEL     '0'           $ERR
032300000000     C*-------------------
032400000000     C* PARTENZA
032500000000     C*-------------------
032600000000     C***
032700000000     C* ESISTENZA
032800000000     C***
032900000000IF  1C     $ERR          IFEQ      '0'
033000000000     C                   MOVEL     V1CTFP        WORG
033100000000     C                   EXSR      CHKFIL
033200000000     C                   MOVEL     WORG          V1CTFP
033300000000     C                   MOVEL     WDORG         V1DTFP
033400000000IF  2C     $ERR          IFEQ      '1'
033500000000     C                   MOVEL     MSG(10)       $MSG
033600000000     C                   EXSR      CARMSG
033700000000     C                   SETON                                        60
033800000000E   2C                   ENDIF
033900000000E   1C                   ENDIF
034000000000     C***
034100000000     C* FERMATA NON DI CARICO, ESCLUDE
034200000000     C***
034300000000IF  1C     $ERR          IFEQ      '0'
034400000000     C                   MOVEL     V1CTFP        WNUM3
034500000000     C                   MOVEL     '0'           $OK                            *NON VALIDO
034600000000DO  2C     1             DO        TOTTLR        J
034700000000IF  3C     FFIL(J)       IFEQ      WNUM3
034800000000     C     FCAR(J)       ANDEQ     'S'
034900000000     C                   MOVEL     '1'           $OK                            *VALIDO
035000000000     C                   LEAVE
035100000000E   3C                   ENDIF
035200000000E   2C                   ENDDO
035300000000IF  2C     $OK           IFEQ      '0'
035400000000     C                   MOVEL     '1'           $ERR
035500000000     C                   MOVEL     MSG(11)       $MSG
035600000000     C                   EXSR      CARMSG
035700000000     C                   SETON                                        60
035800000000E   2C                   ENDIF
035900000000E   1C                   ENDIF
036000000000     C*-------------------
036100000000     C* ARRIVO
036200000000     C*-------------------
036300000000     C***
036400000000     C* ESISTENZA
036500000000     C***
036600000000IF  1C     $ERR          IFEQ      '0'
036700000000     C                   MOVEL     V1CTFF        WORG
036800000000     C                   EXSR      CHKFIL
036900000000     C                   MOVEL     WORG          V1CTFF
037000000000     C                   MOVEL     WDORG         V1DTFF
037100000000IF  2C     $ERR          IFEQ      '1'
037200000000     C                   MOVEL     MSG(10)       $MSG
037300000000     C                   EXSR      CARMSG
037400000000     C                   SETON                                        62
037500000000E   2C                   ENDIF
037600000000E   1C                   ENDIF
037700000000     C***
037800000000     C* NO AFFLUENZA / NO DEFLUENZA
037900000000     C***
038000000000     C* TFF=TRMINAL DI ARRIVO DA ORGANIGRAMMA   OPPURE
038100000000     C* TFP+TFA+TFF= ESISTENTE COME ECCEZIONE
038200000000     C* NB: QUESTI CONTROLLI LI FA DOPO AVER CONTROLLATO TUTTE LE 3 FILIALI
038300000000     C***
038400000000     C* DEFLUENZA
038500000000     C***
038600000000     C* TFF = TFA
038700000000IF  1C     $ERR          IFEQ      '0'
038800000000IF  2C     DEPDEF        IFEQ      'S'                                          *DEFLUENZA
038900960424     C     D00INL        OREQ      'D'                                          *INOLTRO DEFLUENZA
039000000000IF  3C     V1CTFF        IFNE      V1CTFA                                       *TFF=TFA
039100000000     C                   MOVEL     '1'           $ERR
039200000000     C                   MOVEL     MSG(14)       $MSG
039300000000     C                   EXSR      CARMSG
039400000000     C                   SETON                                        62
039500000000E   3C                   ENDIF
039600000000E   2C                   ENDIF
039700000000E   1C                   ENDIF
039800000000     C***
039900000000     C* AFFLUENZA
040000000000     C***
040100000000     C* TFF = TFP
040200000000IF  1C     $ERR          IFEQ      '0'
040300000000IF  2C     TLTTTR        IFEQ      'A'                                          *AFFLUENZA
040400000000IF  3C     V1CTFF        IFNE      V1CTFP                                       *TFF=TFP
040500000000     C                   MOVEL     '1'           $ERR
040600000000     C                   MOVEL     MSG(15)       $MSG
040700000000     C                   EXSR      CARMSG
040800000000     C                   SETON                                        62
040900000000E   3C                   ENDIF
041000000000E   2C                   ENDIF
041100000000E   1C                   ENDIF
041200000000     C***
041300000000     C* MEMORIZZA SE E' UNA FERMATA DI SCARICO PER CONTROLLI SUCCESSIVI
041400000000     C***
041500000000     C                   MOVEL     '0'           $SCA
041600000000IF  1C     $ERR          IFEQ      '0'
041700000000     C                   MOVEL     V1CTFF        WNUM3
041800000000DO  2C     1             DO        TOTTLR        J
041900000000IF  3C     FFIL(J)       IFEQ      WNUM3
042000000000     C     FSCA(J)       ANDEQ     'S'
042100000000     C                   MOVEL     '1'           $SCA
042200000000     C                   LEAVE
042300000000E   3C                   ENDIF
042400000000E   2C                   ENDDO
042500000000E   1C                   ENDIF
042600000000     C*-------------------
042700000000     C* SCARICO
042800000000     C*-------------------
042900000000     C***
043000000000     C* ESISTENZA
043100000000     C***
043200000000IF  1C     $ERR          IFEQ      '0'
043300000000     C                   MOVEL     V1CTFA        WORG
043400000000     C                   EXSR      CHKFIL
043500000000     C                   MOVEL     WORG          V1CTFA
043600000000     C                   MOVEL     WDORG         V1DTFA
043700960719     C                   MOVEL     WTFA          DEPTFA
043800000000IF  2C     $ERR          IFEQ      '1'
043900000000     C                   MOVEL     MSG(10)       $MSG
044000000000     C                   EXSR      CARMSG
044100000000     C                   SETON                                        61
044200000000E   2C                   ENDIF
044300000000E   1C                   ENDIF
044400000000     C***
044500000000     C* FERMATA NON DI SCARICO, ESCLUDE
044600000000     C***
044700000000IF  1C     $ERR          IFEQ      '0'
044800000000     C                   MOVEL     V1CTFA        WNUM3
044900000000     C                   MOVEL     '0'           $OK                            *NON VALIDO
045000000000DO  2C     1             DO        TOTTLR        J
045100000000IF  3C     FFIL(J)       IFEQ      WNUM3
045200000000     C     FSCA(J)       ANDEQ     'S'
045300000000     C                   MOVEL     '1'           $OK                            *VALIDO
045400000000     C                   LEAVE
045500000000E   3C                   ENDIF
045600000000E   2C                   ENDDO
045700000000IF  2C     $OK           IFEQ      '0'
045800000000     C                   MOVEL     '1'           $ERR
045900000000     C                   MOVEL     MSG(12)       $MSG
046000000000     C                   EXSR      CARMSG
046100000000     C                   SETON                                        61
046200000000E   2C                   ENDIF
046300000000E   1C                   ENDIF
046400000000     C***
046500000000     C* SEQ. FERMATA DI SCARICO > SEQ. FERMATA DI PARTENZA
046600000000     C***
046700000000IF  1C     $ERR          IFEQ      '0'
046800960424IF  2C     TLTTTR        IFNE      'L'                                          *NO LOCALE
046900960424     C     D00INL        ANDNE     'L'                                          *NO INOLTRO LOCALE
047000000000     C                   MOVEL     V1CTFP        WNUM3P            3 0          *PARTENZA
047100000000     C                   MOVEL     V1CTFA        WNUM3A            3 0          *SCARICO
047200000000     C                   CLEAR                   JSEP                           *SEQUENZE PART
047300000000     C                   CLEAR                   JTFP                           *TIPO FERMATA PART
047400000000     C                   CLEAR                   JSEA                           *SEQUENZE ARR
047500000000     C                   CLEAR                   JTFA                           *TIPO FERMATA ARR
047600951214     C                   MOVEL     '0'           $OK                            *NON VALIDO
047700951213DO  3C     1             DO        TOTTLR        J
047800951213IF  4C     WNUM3A        IFEQ      FFIL(J)
047900951214IF  5C     FTFM(J)       IFEQ      'A'                                          *ANDATA
048000951214     C                   Z-ADD     J             JSEA(1)
048100951214     C                   MOVEL     FTFM(J)       JTFA(1)
048200951214X   5C                   ELSE                                                   *RITORNO
048300951214     C                   Z-ADD     J             JSEA(2)
048400951214     C                   MOVEL     FTFM(J)       JTFA(2)
048500951214E   5C                   ENDIF
048600951214E   4C                   ENDIF
048700951213IF  4C     WNUM3P        IFEQ      FFIL(J)
048800951214IF  5C     FTFM(J)       IFEQ      'A'                                          *ANDATA
048900951214     C                   Z-ADD     J             JSEP(1)
049000951214     C                   MOVEL     FTFM(J)       JTFP(1)
049100951214X   5C                   ELSE                                                   *RITORNO
049200951214     C                   Z-ADD     J             JSEP(2)
049300951214     C                   MOVEL     FTFM(J)       JTFP(2)
049400951214E   5C                   ENDIF
049500951214E   4C                   ENDIF
049600951213E   3C                   ENDDO
049700951214     C* ANDATA
049800951213IF  3C     JTFA(1)       IFEQ      JTFP(1)                                      *STESSO TIPO FERMATA
049900951214     C     JSEA(1)       ANDGT     JSEP(1)                                      *SEQ ARR > SEQ PART
050000951214     C                   MOVEL     '1'           $OK                            *VALIDO
050100951214E   3C                   ENDIF
050200951214     C* RITORNO
050300951214IF  3C     JTFA(2)       IFEQ      JTFP(2)                                      *STESSO TIPO FERMATA
050400951214     C     JSEA(2)       ANDGT     JSEP(2)                                      *SEQ ARR > SEQ PART
050500951214     C                   MOVEL     '1'           $OK                            *VALIDO
050600951214E   3C                   ENDIF
050700951214IF  3C     $OK           IFEQ      '0'
050800951213     C                   MOVEL     '1'           $ERR
050900951213     C                   MOVEL     MSG(16)       $MSG
051000951213     C                   SETON                                        61
051100951213     C                   EXSR      CARMSG
051200951213E   3C                   ENDIF
051300951213E   2C                   ENDIF
051400951213E   1C                   ENDIF
051500000000     C***
051600000000     C* SE LA FILIALE ARRIVO E' UNA FERMATA DI SCARICO, LA FILIALE DI
051700000000     C* SCARICO DEVE ESSERE UGUALE ALLA FILIALE ARRIVO
051800000000     C***
051900000000IF  1C     $ERR          IFEQ      '0'
052000000000IF  2C     $SCA          IFEQ      'S'
052100000000     C     V1CTFF        ANDNE     V1CTFA
052200000000     C                   MOVEL     '1'           $ERR
052300000000     C                   MOVEL     MSG(18)       $MSG
052400000000     C                   EXSR      CARMSG
052500000000     C                   SETON                                        61
052600000000E   2C                   ENDIF
052700000000E   1C                   ENDIF
052800960904     C***
052900960904     C* IL CASO PARTICOALRE ADESSO SI RICONOSCE PIU' GUARDANDO L'ORGANIGRAMMA O
053000960904     C* LE ECCEZIONI (PERCHE' FA A PACCHE CON UN ALTRO CONTROLLO), MA GUARDANDO
053100960904     C* CHE SIA UN TRAINI DI SOLA ANDATA E CHE ARRIVO E SCARICO SIANO UGUALI.
053200960904     C***
053300960904     C***
053400960904     C*--- SE LA FILIALE DI SCARICO NON E' TERMINAL ARRIVO (ORG O ECC)
053500960904     C*--- SCARICO E ARRIVO DEVONO ESSERE UGUALI (CASO PARTICOLARE)
053600960904     C***
053700960904     C*---
053800960904IF  1C*---       $ERR      IFEQ '0'
053900960719     C* ECCEZIONI
054000960904     C*---                 MOVEL'0'       WECC
054100960904     C*---                 EXSR CTREC2
054200960904     C*---                 MOVELWECC      DEPEC2  1
054300960719     C* CASO PARTICOLARE
054400960904IF  2C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
054500960904     C*---       V1CTFA    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
054600960904     C*---       WECC      ANDNE'1'                        *FIL=TERM ARR ECC
054700960904IF  3C*---       V1CTFF    IFNE V1CTFA
054800960904     C*---                 MOVEL'1'       $ERR
054900960904     C*---                 MOVELMSG,18    $MSG
055000960904     C*---                 EXSR CARMSG
055100960904     C*---                 SETON                     61
055200960904E   3C*---                 ENDIF
055300960904E   2C*---                 ENDIF
055400960904E   1C*---                 ENDIF
055500960719     C***---***---***---***
055600000000     C* ARRIVO
055700960719     C***---***---***---***
055800960719     C* SE CASO PARTICOLARE -> (VEDI SOPRA) NON FA QUESTI CONTROLLI
055900960202     C* TFP+TFA+TFF= ESISTENTE COME ECCEZIONE
056000960208     C* SE NON C'E VERIFICO SE E' UN TRANSITO
056100960202     C* SE NON C'E' L'ECCEZIONE -> TFF=TRMINAL DI ARRIVO DA ORGANIGRAMMA
056200000000IF  1C     $ERR          IFEQ      '0'
056300960719     C* CASO PARTICOLARE
056400960904IF  2C     TLTTTR        IFEQ      'S'                                          *SOLO ANDATA
056500960904     C     V1CTFA        ANDEQ     V1CTFF                                       *ARRIVO=SCARICO
056600960904X   2C                   ELSE                                                   *CASO NORMALE
056700960904IF  2C*---       TLTTTR    IFEQ 'S'                        *SOLO ANDATA
056800960904     C*---       V1CTFA    ANDNEDEPTFA                     *FIL=TERM ARR OPPURE
056900960904     C*---       DEPEC2    ANDNE'1'                        *FIL=TERM ARR ECC
057000960904X   2C*---                 ELSE                            *CASO NORMALE
057100960719     C* COMINCIA I CONTROLLI
057200960719IF  3C     TLTTTR        IFNE      'A'                                          *NO AFFLUENZA
057300000000     C     DEPDEF        ANDNE     'S'                                          *NO DEFLUENZA
057400960719     C     D00INL        ANDNE     'D'                                          *NO INOLTRO DEFLUENZ
057500000000     C                   MOVEL     '0'           WECC
057600000000     C                   EXSR      CTREC3
057700960202     C*
057800960202     C* ERRORE EVIDENZIATO SULLA FILIALE DI SCARICO
057900960719IF  4C     $ERR          IFEQ      '1'
058000960202     C                   EXSR      CARMSG
058100960202     C                   SETON                                        61
058200960719X   4C                   ELSE
058300960209     C*
058400960202     C* SE NON TROVA L'ECCEZIONE
058500960719IF  5C     WECC          IFNE      '1'
058600960202     C                   MOVEL     V1CTFF        WORG
058700960202     C                   EXSR      CHKFIL
058800960202     C                   MOVEL     WORG          V1CTFF
058900960202     C                   MOVEL     V1CTFF        WNUM3
059000960209     C*
059100960209     C* CONTROLLA ORGANIGRAMMA
059200960719IF  6C     WNUM3         IFNE      WTFA                                         *TERM ARRIVO STANDAR
059300000000     C                   MOVEL     '1'           $ERR
059400960202     C                   MOVEL     MSG(13)       $MSG
059500000000     C                   EXSR      CARMSG
059600000000     C                   SETON                                        62
059700960719E   6C                   ENDIF
059800960209     C*
059900960202     C* SE OK IL TERMINAL ARRIVO, CONTROLLO SE LA FILIALE DI SCARICO E' CORRETTA
060000960719IF  6C     $ERR          IFEQ      '0'
060100960209     C*
060200960209     C* CONTRALLA ECCEZIONE COME SCARICO
060300960209     C                   MOVEL     '0'           WECC
060400960209     C                   EXSR      CTREC4
060500960209     C*
060600960209     C* ERRORE EVIDENZIATO SULLA FILIALE DI SCARICO
060700960719IF  7C     $ERR          IFEQ      '1'
060800960209     C                   EXSR      CARMSG
060900960209     C                   SETON                                        61
061000960719X   7C                   ELSE
061100960719IF  8C     WECC          IFNE      '1'
061200960209     C*
061300960209     C* CONTROLLA ORGANIGRAMMA
061400960202     C                   MOVEL     V1CTFA        WORG
061500960202     C                   EXSR      CHKFIL
061600960202     C                   MOVEL     WORG          V1CTFA
061700960202     C                   MOVEL     V1CTFA        WNUM3
061800960202     C*
061900960719IF  9C     WNUM3         IFNE      WTFA                                         *TERM ARRIVO STANDAR
062000960202     C                   MOVEL     '1'           $ERR
062100960202     C                   MOVEL     MSG(7)        $MSG
062200960202     C                   EXSR      CARMSG
062300960202     C                   SETON                                        61
062400960719E   9C                   ENDIF
062500960719E   8C                   ENDIF
062600960719E   7C                   ENDIF
062700960719E   6C                   ENDIF
062800960719E   5C                   ENDIF
062900960719E   4C                   ENDIF
063000960719E   3C                   ENDIF
063100960719E   2C                   ENDIF
063200000000E   1C                   ENDIF
063300960719     C***---***---***---***
063400000000     C* PARTENZA E SCARICO
063500960719     C***---***---***---***
063600000000     C* LA PARTENZA DEVE ESSERE <> DALLO SCARICO TRANNE CHE NEI LOCALI
063700000000IF  1C     $ERR          IFEQ      '0'
063800000000IF  2C     TLTTTR        IFNE      'L'                                          *NO LOCALE
063900960424     C     D00INL        ANDNE     'L'                                          *NO INOLTRO LOCALE
064000000000IF  3C     V1CTFP        IFEQ      V1CTFA
064100000000     C                   MOVEL     '1'           $ERR
064200000000     C                   MOVEL     MSG(19)       $MSG
064300000000     C                   EXSR      CARMSG
064400000000     C                   SETON                                        6061
064500000000E   3C                   ENDIF
064600000000E   2C                   ENDIF
064700000000E   1C                   ENDIF
064800000000     C***
064900000000     C* PARTENZA E ARRIVO
065000000000     C***
065100000000     C* LA PARTENZA DEVE ESSERE <> DALL'ARRIVO TRANNE CHE NEI LOCALI
065200000000IF  1C     $ERR          IFEQ      '0'
065300000000IF  2C     TLTTTR        IFNE      'L'                                          *NO LOCALE
065400960424     C     D00INL        ANDNE     'L'                                          *NO INOLTRO LOCALE
065500000000IF  3C     V1CTFP        IFEQ      V1CTFF
065600000000     C                   MOVEL     '1'           $ERR
065700000000     C                   MOVEL     MSG(20)       $MSG
065800000000     C                   EXSR      CARMSG
065900000000     C                   SETON                                        6062
066000000000E   3C                   ENDIF
066100000000E   2C                   ENDIF
066200000000E   1C                   ENDIF
066300960719     C***---***---***---***
066400951213     C* PARTENZA E ARRIVO E SCARICO
066500960719     C***---***---***---***
066600951213     C* NEL LOCALI LA PARTENZA DEVE ESSERE UGUALE ALL'ARRIVO E ALLO SCARICO
066700951213IF  1C     $ERR          IFEQ      '0'
066800951213IF  2C     TLTTTR        IFEQ      'L'                                          *LOCALE
066900960424     C     D00INL        OREQ      'L'                                          *INOLTRO LOCALE
067000951213IF  3C     V1CTFP        IFEQ      V1CTFF
067100951213     C     V1CTFA        ANDEQ     V1CTFF
067200951213X   3C                   ELSE
067300951213     C                   MOVEL     '1'           $ERR
067400951213     C                   MOVEL     MSG(17)       $MSG
067500951213     C                   EXSR      CARMSG
067600951213     C                   SETON                                        606162
067700951213E   3C                   ENDIF
067800951213E   2C                   ENDIF
067900951213E   1C                   ENDIF
068000960719     C***---***---***---***
068100960206     C* ARRIVO E SCARICO
068200960719     C***---***---***---***
068300960206     C* NEGLI INOLTRI ARRIVO E SCARICO DEVONO ESSERE UGUALI (NO TRANSITI)
068400960424     C* SE IL TIPO TRAINO A CUI SI RICONDUCE E' UN SOLO ANDATA
068500960424IF  1C     $ERR          IFEQ      '0'
068600960424IF  2C     TLTTTR        IFEQ      'I'                                          *INOLTRO
068700960424     C     D00INL        ANDEQ     'S'                                          *TIPO SOLO ANDATA
068800960424IF  3C     V1CTFA        IFNE      V1CTFF
068900960424     C                   MOVEL     '1'           $ERR
069000960424     C                   MOVEL     MSG(21)       $MSG
069100960424     C                   EXSR      CARMSG
069200960424     C                   SETON                                          6162
069300960424E   3C                   ENDIF
069400960424E   2C                   ENDIF
069500960424E   1C                   ENDIF
069600000000     C*-------------------
069700000000     C* INSERIMENTO --> RECORD INESISTENTE
069800000000     C*-------------------
069900000000IF  1C     $ERR          IFEQ      '0'
070000000000IF  2C     D00OP1        IFEQ      'F10'
070100000000     C                   Z-ADD     D00TRN        KLMTRN                         *TRAINO
070200000000     C                   Z-ADD     D00DDE        KLMDDE                         *DATA DECORRENZA
070300000000     C                   MOVEL     V1CTFP        KLMTFP                         *PARTENZA
070400000000     C                   MOVEL     V1CTFA        KLMTFA                         *SCARICO
070500000000     C                   MOVEL     V1CTFF        KLMTFF                         *ARRIVO
070600000000     C     KEYTLM        CHAIN(N)  TNTLM01L                           99
070700000000IF  3C     *IN99         IFEQ      *OFF                                         *INSESISTENTE
070800000000     C                   MOVEL     '1'           $ERR
070900000000     C                   MOVEL     MSG(24)       $MSG
071000000000     C                   EXSR      CARMSG
071100000000     C                   SETON                                        606162
071200000000E   3C                   ENDIF
071300000000E   2C                   ENDIF
071400000000E   1C                   ENDIF
071500960424     C*-------------------
071600960424     C* MC NORMALI / BIS
071700960424     C*-------------------
071800960424IF  1C     $ERR          IFEQ      '0'
071900960424IF  2C     TLTBIS        IFEQ      *BLANKS                                      *TRAINO NORMALE
072000960424IF  3C     V1CMCA        IFEQ      *ZEROS
072100960424     C                   MOVEL     '1'           $ERR
072200960424     C                   MOVEL     MSG(22)       $MSG
072300960424     C                   EXSR      CARMSG
072400960424     C                   SETON                                        63
072500960424E   3C                   ENDIF
072600960424X   2C                   ELSE                                                   *TRAINO BIS
072700960424IF  3C     V1CMCB        IFEQ      *ZEROS
072800960424     C                   MOVEL     '1'           $ERR
072900960424     C                   MOVEL     MSG(23)       $MSG
073000960424     C                   EXSR      CARMSG
073100960424     C                   SETON                                        65
073200960424E   3C                   ENDIF
073300960424E   2C                   ENDIF
073400960424E   1C                   ENDIF
073500960301     C*-------------------
073600960301     C* MC ASSEGNATI
073700960301     C*-------------------
073800960301     C* OBBLIGATORIO SE IL TRAINO E' DI TIPO ECCEZIONALE
073900960423IF  1C*---       $ERR      IFEQ '0'
074000960423IF  2C*---       TLTBIS    IFEQ 'E'
074100960423     C*---       V1CMCA    ANDEQ*ZEROS
074200960423     C*---                 MOVEL'1'       $ERR
074300960423     C*---                 MOVELMSG,22    $MSG
074400960423     C*---                 EXSR CARMSG
074500960423     C*---                 SETON                     63
074600960423E   2C*---                 ENDIF
074700960423E   1C*---                 ENDIF
074800000000     C*
074900000000     C                   ENDSR
075000000000     C*------------------------------------------------------------------------*
075100000000     C* CHKFIL - CONTROLLA LA FILIALE
075200000000     C*------------------------------------------------------------------------*
075300000000     C     CHKFIL        BEGSR
075400000000     C*
075500000000     C     *LIKE         DEFINE    V1CTFP        WORG
075600000000     C     *LIKE         DEFINE    V1DTFP        WDORG
075700000000     C     *LIKE         DEFINE    ORGFIL        WTFP
075800000000     C     *LIKE         DEFINE    ORGFIL        WTFA
075900000000     C*
076000000000     C                   MOVEL     *BLANKS       WDORG                          *DESCRIZIONE
076100000000     C                   Z-ADD     *ZEROS        WTFP                           *TERMINAL PARTENZA
076200000000     C                   Z-ADD     *ZEROS        WTFA                           *TERMINAL ARRIVO
076300000000     C* RICHIESTO ELENCO
076400000000     C                   SETOFF                                           28
076500000000     C     '?'           SCAN      WORG                                   28
076600000000IF  1C     *IN28         IFEQ      *ON
076700000000     C                   Z-ADD     *ZEROS        D00FIL
076800000000     C                   MOVEL     DS00          KPJBU
076900960108     C                   CALL      'TNTL16R'
077000000000     C                   PARM                    KPJBA
077100000000     C                   MOVEL     KPJBU         DS00
077200000000     C                   MOVEL     D00FIL        WORG
077300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
077400000000X   1C                   ELSE
077500000000     C* SOLO NUMERICO
077600000000     C                   SETOFF                                       28
077700000000     C                   TESTN                   WORG                 28
077800000000     C                   MOVE      WORG          WALF1             1
077900000000IF  2C     *IN28         IFEQ      *OFF
078000000000     C     WALF1         ORLT      *ZEROS
078100000000     C                   MOVEL     '1'           $ERR
078200000000X   2C                   ELSE
078300000000     C* ESISTENZA
078400000000     C                   SETOFF                                           28
078500000000     C                   Z-ADD     1             J
078600000000     C                   MOVEL     WORG          WNUM3             3 0
078700000000     C     WNUM3         LOOKUP    ORG(J)                                 28
078800000000IF  3C     *IN28         IFEQ      *ON
078900000000     C     ORG(J)        ANDGT     *ZEROS
079000000000     C                   MOVEL     DORG(J)       WDORG                          *DESCRIZIONE
079100000000     C                   Z-ADD     TFP(J)        WTFP                           *FILIALE PARTENZA
079200000000     C                   Z-ADD     TFA(J)        WTFA                           *FILIALE ARRIVO
079300000000X   3C                   ELSE
079400000000     C                   MOVEL     '1'           $ERR
079500000000E   3C                   ENDIF
079600000000E   2C                   ENDIF
079700000000E   1C                   ENDIF
079800000000     C*
079900000000     C                   ENDSR
080000960719     C*------------------------------------------------------------------------*
080100960719     C* CTREC2 - CONTROLLA LE ECCEZIONI (2)
080200960719     C*------------------------------------------------------------------------*
080300960719     C     CTREC2        BEGSR
080400960719     C*
080500960719     C                   MOVEL     V1CTFA        WNUM3B            3 0          *SCARICO
080600960719     C*
080700960719     C     *LOVAL        SETLL     AZCAE01L
080800960719     C                   READ      AZCAE01L                               99
080900960719DO  1C     *IN99         DOWEQ     *OFF
081000960719     C     WECC          ANDEQ     '0'
081100970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
081200970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
081300970717     C     CAETFP        ANDGT     *ZEROS
081400970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
081500970717     C     CAETFA        ANDGT     *ZEROS
081600970717IF  4C     CAEEPA        IFEQ      'A'                                          *ECCEZIONE ARRIVO
081700960719     C     CAETFE        ANDEQ     WNUM3B                                       *FILIALE SCARICO
081800960719     C     CAEDDE        ANDLE     TLTDDE                                       *IN DECORRENZA
081900960719     C     CAEDSC        ANDGE     TLTDSC
082000960719     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
082100970717E   4C                   ENDIF
082200970717E   3C                   ENDIF
082300960821E   2C                   ENDIF
082400960719     C                   READ      AZCAE01L                               99
082500960719E   1C                   ENDDO
082600960719     C*
082700960719     C                   ENDSR
082800000000     C*------------------------------------------------------------------------*
082900000000     C* CTREC3 - CONTROLLA LE ECCEZIONI (3)
083000000000     C*------------------------------------------------------------------------*
083100000000     C     CTREC3        BEGSR
083200000000     C*
083300960209     C                   MOVEL     V1CTFA        WNUM3B            3 0          *SCARICO
083400000000     C*
083500000000     C                   MOVEL     'A'           KAEEPA                         *ARRIVO
083600000000     C                   MOVEL     V1CTFP        KAETFP                         *PARTENZA
083700000000     C                   MOVEL     V1CTFF        KAETFA                         *ARRIVO
083800000000     C     KEYCAE        SETLL     AZCAE01L
083900000000     C     KEYCAE        READE     AZCAE01L                               99
084000000000DO  1C     *IN99         DOWEQ     *OFF
084100000000     C     WECC          ANDEQ     '0'
084200970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
084300970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
084400970717     C     CAETFP        ANDGT     *ZEROS
084500970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
084600970717     C     CAETFA        ANDGT     *ZEROS
084700970717IF  4C     CAEDDE        IFLE      TLTDDE                                       IN DECORRENZA
084800000000     C     CAEDSC        ANDGE     TLTDSC
084900970717IF  5C     CAETFE        IFEQ      WNUM3B                                       *ECCEZ=ARRIVO/SCAR
085000000000     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
085100970717X   5C                   ELSE
085200970717IF  6C     CAETFE        IFEQ      KAETFA                                       *ECCEZ=ARRIVO FINALE
085300960208     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
085400970717X   6C                   ELSE
085500960209     C                   MOVEL     '1'           $ERR                           *ERRORE ECCEZIONE
085600960202     C                   MOVEL     MSG(8)        $MSG
085700970717E   6C                   ENDIF
085800970717E   5C                   ENDIF
085900960202     C*
086000960202     C* NON SI POSSONO ACCETTARE ECCEZIONI A CAVALLO DI UNA VALIDITA' TRAINO
086100970717X   4C                   ELSE
086200970717IF  5C     CAEDDE        IFGT      TLTDSC
086300960202     C     CAEDSC        ORLT      TLTDDE
086400970717X   5C                   ELSE
086500960209     C                   MOVEL     '1'           $ERR                           *ERRORE ECCEZIONE
086600960202     C                   MOVEL     MSG(9)        $MSG
086700970717E   5C                   ENDIF
086800970717E   4C                   ENDIF
086900970717E   3C                   ENDIF
087000970717E   2C                   ENDIF
087100000000     C     KEYCAE        READE     AZCAE01L                               99
087200000000E   1C                   ENDDO
087300000000     C*
087400000000     C                   ENDSR
087500960209     C*------------------------------------------------------------------------*
087600960209     C* CTREC4 - CONTROLLA LE ECCEZIONI (4)
087700960209     C*------------------------------------------------------------------------*
087800960209     C     CTREC4        BEGSR
087900960209     C*
088000960209     C                   MOVEL     V1CTFA        WNUM3B            3 0          *SCARICO
088100960209     C*
088200960209     C                   MOVEL     'A'           KAEEPA                         *ARRIVO
088300960209     C                   MOVEL     V1CTFP        KAETFP                         *PARTENZA
088400960209     C                   MOVEL     V1CTFA        KAETFA                         *SCARICO
088500960209     C     KEYCAE        SETLL     AZCAE01L
088600960209     C     KEYCAE        READE     AZCAE01L                               99
088700960209DO  1C     *IN99         DOWEQ     *OFF
088800960209     C     WECC          ANDEQ     '0'
088900970717IF  2C     CAEATB        IFEQ      *BLANKS                                      *VALIDI
089000970717IF  3C     CAEEPA        IFEQ      'A'                                          *NO STANDARD ARRIVO
089100970717     C     CAETFP        ANDGT     *ZEROS
089200970717     C     CAEEPA        OREQ      'P'                                          *NO STANDARD PARTENZ
089300970717     C     CAETFA        ANDGT     *ZEROS
089400970717IF  4C     CAEDDE        IFLE      TLTDDE                                       IN DECORRENZA
089500960209     C     CAEDSC        ANDGE     TLTDSC
089600970717IF  5C     CAETFE        IFEQ      WNUM3B                                       *ECCEZ=SCARICO
089700960209     C                   MOVEL     '1'           WECC                           *SI COME ECCEZIONE
089800970717X   5C                   ELSE
089900960209     C                   MOVEL     '1'           $ERR                           *ERRORE ECCEZIONE
090000960209     C                   MOVEL     MSG(7)        $MSG
090100970717E   5C                   ENDIF
090200960209     C*
090300960209     C* NON SI POSSONO ACCETTARE ECCEZIONI A CAVALLO DI UNA VALIDITA' TRAINO
090400970717X   4C                   ELSE
090500970717IF  5C     CAEDDE        IFGT      TLTDSC
090600960209     C     CAEDSC        ORLT      TLTDDE
090700970717X   5C                   ELSE
090800960209     C                   MOVEL     '1'           $ERR                           *ERRORE ECCEZIONE
090900960209     C                   MOVEL     MSG(9)        $MSG
091000970717E   5C                   ENDIF
091100970717E   4C                   ENDIF
091200970717E   3C                   ENDIF
091300970717E   2C                   ENDIF
091400960209     C     KEYCAE        READE     AZCAE01L                               99
091500960209E   1C                   ENDDO
091600960209     C*
091700960209     C                   ENDSR
091800000000     C*------------------------------------------------------------------------*
091900000000     C* IMPVID - IMPOSTAZIONI VIDEO
092000000000     C*------------------------------------------------------------------------*
092100000000     C     IMPVID        BEGSR
092200000000     C*
092300000000     C* 1°) DECODIFICA L'OPZIONE RICHIAMATA
092400000000     C* 2°) IMPOSTA LA PROTEZIONE DEI CAMPI -> 90=CAMPI CHIAVE / 91=ALTRI CAMPI
092500000000SL  1C                   SELECT
092600000000     C     D00OP1        WHENEQ    'F10'
092700000000     C                   MOVEL     DOPZ(1)       VTDFUN                         *INSERIMENTO
092800000000     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
092900000000     C     D00OP1        WHENEQ    'O02'
093000000000     C                   MOVEL     DOPZ(2)       VTDFUN                         *VARIAZIONE
093100000000     C                   SETON                                        90        *CHIAVE PROTETTA
093200000000     C     D00OP1        WHENEQ    'O04'
093300000000     C                   MOVEL     DOPZ(3)       VTDFUN                         *CANCELLAZIONE
093400000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
093500000000     C     D00OP1        WHENEQ    'O07'
093600000000     C                   MOVEL     DOPZ(4)       VTDFUN                         *RIPRISTINO
093700000000     C                   SETON                                        9091      *PROTEZIONE COMPLETA
093800000000     C                   OTHER
093900000000E   1C                   ENDSL
094000000000     C*
094100000000     C* CONTROLLA ESISTENZA E DISPONIBILITA' RECORD (NO SE INSERIMENTO)
094200000000     C* N.B. LEGGE IL RECORD VINCOLANDOLO SOLO SE NON E' IN INTERROGAZIONE
094300000000SL  1C                   SELECT
094400000000     C     D00OP1        WHENEQ    'F10'
094500000000     C                   OTHER
094600000000     C                   Z-ADD     D00TRN        KLMTRN                         *TRAINO
094700000000     C                   Z-ADD     D00DDE        KLMDDE                         *DATA DECORRENZA
094800000000     C                   Z-ADD     D00TFP        KLMTFP                         *PARTENZA
094900000000     C                   Z-ADD     D00TFA        KLMTFA                         *SCARICO
095000000000     C                   Z-ADD     D00TFF        KLMTFF                         *ARRIVO
095100000000     C* INTERROGAZIONE
095200000000IF  2C     $TGES         IFEQ      'I'
095300000000     C     KEYTLM        CHAIN(N)  TNTLM01L                           99
095400000000X   2C                   ELSE
095500000000     C* MODIFICA
095600000000     C     KEYTLM        CHAIN     TNTLM01L                           9998
095700000000IF  3C     *IN98         IFEQ      *ON                                          *ALLOCATO
095800000000     C                   MOVEL     '1'           $ERR
095900000000     C                   MOVEL     '1'           $FINE
096000000000     C                   MOVEL     MSG(26)       D00MSG
096100000000E   3C                   ENDIF
096200000000E   2C                   ENDIF
096300000000IF  2C     *IN99         IFEQ      *ON                                          *INSESISTENTE
096400000000     C                   MOVEL     '1'           $ERR
096500000000     C                   MOVEL     '1'           $FINE
096600000000     C                   MOVEL     MSG(25)       D00MSG
096700000000E   2C                   ENDIF
096800000000E   1C                   ENDSL
096900000000     C* RIEMPE IL VIDEO
097000000000IF  1C     $ERR          IFEQ      '0'
097100000000SL  2C                   SELECT
097200000000     C     D00OP1        WHENEQ    'F10'
097300000000     C                   EXSR      AZZVID                                       *AZZERA CAMPI VIDEO
097400000000     C                   OTHER
097500000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
097600000000E   2C                   ENDSL
097700000000E   1C                   ENDIF
097800000000     C*
097900000000     C                   ENDSR
098000000000     C*------------------------------------------------------------------------*
098100000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
098200000000     C*------------------------------------------------------------------------*
098300000000     C     RECVID        BEGSR
098400000000     C*
098500000000     C                   MOVEL     TLMATB        V1CATB                         *ANNULLAMENTO
098600000000     C                   MOVEL     TLMTFP        V1CTFP                         *PARTENZA
098700000000     C                   MOVEL     TLMTFA        V1CTFA                         *SCARICO
098800000000     C                   MOVEL     TLMTFF        V1CTFF                         *ARRIVO
098900960423     C                   Z-ADD     TLMMCA        V1CMCA                         *MCUBI NORMALI
099000960423     C                   Z-ADD     TLMMCB        V1CMCB                         *MCUBI BIS
099100960105     C                   Z-ADD     TLMMCT        V1CMCT                         *MCUBI TRAINI SEDE
099200000000     C* DECODIFICHE
099300000000     C                   SETOFF                                           28
099400000000     C                   Z-ADD     1             J
099500000000     C                   MOVEL     *BLANKS       V1DTFP
099600000000     C     TLMTFP        LOOKUP    ORG(J)                                 28
099700000000IF  1C     *IN28         IFEQ      *ON
099800000000     C                   MOVEL     DORG(J)       V1DTFP                         *PARTENZA
099900000000E   1C                   ENDIF
100000000000     C                   SETOFF                                           28
100100000000     C                   Z-ADD     1             J
100200000000     C                   MOVEL     *BLANKS       V1DTFA
100300000000     C     TLMTFA        LOOKUP    ORG(J)                                 28
100400000000IF  1C     *IN28         IFEQ      *ON
100500000000     C                   MOVEL     DORG(J)       V1DTFA                         *SCARICO
100600000000E   1C                   ENDIF
100700000000     C                   SETOFF                                           28
100800000000     C                   Z-ADD     1             J
100900000000     C                   MOVEL     *BLANKS       V1DTFF
101000000000     C     TLMTFF        LOOKUP    ORG(J)                                 28
101100000000IF  1C     *IN28         IFEQ      *ON
101200000000     C                   MOVEL     DORG(J)       V1DTFF                         *ARRIVO
101300000000E   1C                   ENDIF
101400000000     C*
101500000000     C                   ENDSR
101600000000     C*------------------------------------------------------------------------*
101700000000     C*  AZZVID - PULISCE TUTTI I CAMPI DEL VIDEO
101800000000     C*------------------------------------------------------------------------*
101900000000     C     AZZVID        BEGSR
102000000000     C*
102100000000     C                   MOVEL     *BLANKS       V1CATB                         *ANNULLAMENTO
102200000000     C                   MOVEL     '  ?'         V1CTFP                         *PARTENZA
102300000000     C                   MOVEL     *BLANKS       V1CTFA                         *SCARICO
102400000000     C                   MOVEL     *BLANKS       V1CTFF                         *ARRIVO
102500960423     C                   Z-ADD     *ZEROS        V1CMCA                         *MCUBI NORMALI
102600960423     C                   Z-ADD     *ZEROS        V1CMCB                         *MCUBI BIS
102700960105     C                   Z-ADD     *ZEROS        V1CMCT                         *MCUBI TRAINI SEDE
102800000000     C* DECODIFICHE
102900000000     C                   MOVEL     *BLANKS       V1DTFP                         *PARTENZA
103000000000     C                   MOVEL     *BLANKS       V1DTFA                         *SCARICO
103100000000     C                   MOVEL     *BLANKS       V1DTFF                         *ARRIVO
103200000000     C*
103300000000     C                   ENDSR
103400000000     C*------------------------------------------------------------------------*
103500000000     C* MEMTRN - MEMORIZZA TUTTI I DATI DEL TRAINO
103600000000     C*------------------------------------------------------------------------*
103700000000     C     MEMTRN        BEGSR
103800000000     C*
103900000000     C                   Z-ADD     *ZEROS        TOTTLR            5 0
104000000000     C* TRAINO
104100000000     C                   Z-ADD     D00TRN        KLTTRN                         *NUMERO TRAINO
104200000000     C                   Z-ADD     D00DDE        KLTDDE                         *DATA DECORRENZA
104300000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
104400020715     c                   z-add     1             ktbkut
104500000000     C                   MOVEL     'TV'          KTBCOD
104600000000     C                   MOVEL(P)  TLTTTR        KTBKEY
104700000000     C     KEYTAB        CHAIN     TABEL00F                           99
104800000000     C                   MOVEL     TBLUNI        DSTV
104900000000     C                   MOVEL     §TVDEF        DEPDEF                         *TRAINO DEFLUENZA
105000000000     C* FERMATE
105100000000     C                   Z-ADD     D00TRN        KLRTRN                         *NUMERO TRAINO
105200000000     C                   Z-ADD     D00DDE        KLRDDE                         *DATA DECORRENZA
105300000000     C     KEYTLR        SETLL     TNTLR01L
105400000000     C     KEYTLR        READE(N)  TNTLR01L                               99
105500000000     C                   Z-ADD     *ZEROS        J
105600000000DO  1C     *IN99         DOWEQ     *OFF
105700000000IF  2C     TLRATB        IFEQ      *BLANKS
105800000000     C                   ADD       1             J
105900000000     C                   MOVEL     TLRTFM        FTFM(J)
106000000000     C                   Z-ADD     TLRFIL        FFIL(J)
106100000000     C                   MOVEL     TLRSCA        FSCA(J)
106200000000     C                   MOVEL     TLRCAR        FCAR(J)
106300000000     C                   Z-ADD     TLRKMT        FKMT(J)
106400000000     C                   Z-ADD     TLRTEM        FTEM(J)
106500000000     C                   MOVEL     TLRGFV        FGFV(J)
106600951215E   2C                   ENDIF
106700000000     C     KEYTLR        READE(N)  TNTLR01L                               99
106800000000E   1C                   ENDDO
106900000000     C                   Z-ADD     J             TOTTLR                         *N° TOTALE FERMATE
107000000000     C*
107100000000     C                   ENDSR
107200000000     C*------------------------------------------------------------------------*
107300000000     C* CARTAB - CARICA LE TABELLE
107400000000     C*------------------------------------------------------------------------*
107500000000     C     CARTAB        BEGSR
107600000000     C*--------------------
107700000000     C* FILIALI
107800000000     C*--------------------
107900000000     C                   Z-ADD     *ZEROS        J                 4 0
108000000000     C     *LOVAL        SETLL     AZORG01L
108100000000     C                   READ      AZORG01L                               99
108200000000DO  1C     *IN99         DOWEQ     *OFF
108300000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
108400960208     C                   MOVEL     ORGDE8        OG148
108500960208IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
108600000000     C                   ADD       1             J
108700000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
108800000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
108900971020     C                   CLEAR                   DSLV55
109000971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
109100971020     C                   MOVEL     *BLANKS       D55TPT                         *PARTENZA/ARRIVO
109200971020     C                   Z-ADD     ORGFIL        D55LIN                         *LINEA
109300980603     C                   Z-ADD     D00DDE        D55DRF                         *DATA RIFERIMENTO
109400971020     C                   CALL      'FNLV55R'
109500971020     C                   PARM                    DSLV55
109600971128IF  4C     D55ERR        IFNE      *BLANKS
109700971128     C                   Z-ADD     ORGFIL        D55TFP
109800971128     C                   Z-ADD     ORGFIL        D55TFA
109900971128E   4C                   ENDIF
110000971020     C                   Z-ADD     D55TFP        TFP(J)                         *TERMINAL PARTENZA
110100971020     C                   Z-ADD     D55TFA        TFA(J)                         *TERMINAL PARTENZA
110200960208E   3C                   ENDIF
110300960208E   2C                   ENDIF
110400000000     C                   READ      AZORG01L                               99
110500000000E   1C                   ENDDO
110600000000     C*
110700000000     C                   ENDSR
110800971020     C*------------------------------------------------------------------------*
110900971020     C* FINSR - OPERAZIONI FINALI
111000971020     C*------------------------------------------------------------------------*
111100971020     C     FINSR         BEGSR
111200971020     C* FNLV55R
111300971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
111400971020     C                   MOVEL     'C'           D55TLA
111500971020     C                   CALL      'FNLV55R'
111600971020     C                   PARM                    DSLV55
111700971020     C*
111800971020     C                   ENDSR
111900000000     C*------------------------------------------------------------------------*
112000000000     C* *INZSR - IMPOSTAZIONI INIZIALI
112100000000     C*------------------------------------------------------------------------*
112200000000     C     *INZSR        BEGSR
112300000000     C*--------------------
112400000000     C* RICEVIMENTO PARAMETRI
112500000000     C*--------------------
112600000000     C     *ENTRY        PLIST
112700000000     C                   PARM                    KPJBA
112800000000     C                   MOVEL     KPJBU         DS00
112900000000     C*--------------------
113000000000     C* VARIABILI RIFERITE AL DATA BASE
113100000000     C*--------------------
113200000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
113300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
113400000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
113500000000     C     *LIKE         DEFINE    CAEEPA        KAEEPA                         *AZCAE01L
113600000000     C     *LIKE         DEFINE    CAETFP        KAETFP
113700000000     C     *LIKE         DEFINE    CAETFA        KAETFA
113800000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
113900000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
114000000000     C     *LIKE         DEFINE    TLMTFP        KLMTFP
114100000000     C     *LIKE         DEFINE    TLMTFA        KLMTFA
114200000000     C     *LIKE         DEFINE    TLMTFF        KLMTFF
114300000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
114400000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
114500000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
114600000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
114700000000     C     *LIKE         DEFINE    §TVDEF        DEPDEF                         *DEPOSITI
114800960719     C*--------------------
114900960719     C* VARIABILI NON RIFERITE AL DATA BASE
115000960719     C*--------------------
115100960719     C     *LIKE         DEFINE    V1CTFP        DEPTFP                         *DEPOSITI
115200960719     C     *LIKE         DEFINE    V1CTFP        DEPTFA
115300000000     C*--------------------
115400000000     C* CHIAVI DI LETTURA
115500000000     C*--------------------
115600000000     C* LETTURA TABEL00F - COMPLETA
115700000000     C     KEYTAB        KLIST
115800000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
115900000000     C                   KFLD                    KTBCOD                         *TABELLA
116000000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
116100000000     C* LETTURA AZCAE01L - PARZIALE
116200000000     C     KEYCAE        KLIST
116300000000     C                   KFLD                    KAEEPA                         *PARTENZA/ARRIVO
116400000000     C                   KFLD                    KAETFP                         *PARTENZA
116500000000     C                   KFLD                    KAETFA                         *ARRIVO
116600000000     C* LETTURA TNTLM01L - COMPLETA
116700000000     C     KEYTLM        KLIST
116800000000     C                   KFLD                    KLMTRN                         *NUMERO TRAINO
116900000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
117000000000     C                   KFLD                    KLMTFP                         *PARTENZA
117100000000     C                   KFLD                    KLMTFA                         *SCARICO
117200000000     C                   KFLD                    KLMTFF                         *ARRIVO
117300000000     C* LETTURA TNTLT01L - COMPETA
117400000000     C     KEYTLT        KLIST
117500000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
117600000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
117700000000     C* LETTURA TNTLR01L - PARZIALE
117800000000     C     KEYTLR        KLIST
117900000000     C                   KFLD                    KLRTRN                         *NUMERO TRAINO
118000000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
118100000000     C*--------------------
118200000000     C* VARIABILI DI CONTROLLO
118300000000     C*--------------------
118400000000     C                   MOVEL     '0'           $OK               1            *OK / NON OK
118500000000     C                   MOVEL     '0'           $SCA              1            *FERMATA DI SCARICO
118600000000     C                   MOVEL     '0'           WECC              1            *ECCEZIONE TERMINAL
118700971020     C*
118800971020     C* CALCOLA LA DATA CORRENTE
118900971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
119000971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
119100971020     C                   Z-ADD     N8            G08DAT
119200971020     C                   Z-ADD     *ZEROS        G08INV
119300971020     C                   MOVEL     '0'           G08ERR
119400971020     C                   CALL      'XSRDA8'
119500971020     C                   PARM                    WLBDA8
119600971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
119700000000     C*
119800000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
119900000000     C                   MOVE      D00OP0        INTGES            2
120000000000IF  1C     INTGES        IFEQ      '02'
120100000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
120200000000X   1C                   ELSE
120300000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
120400000000E   1C                   ENDIF
120500000000     C*
120600000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
120700000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
120800000000IF  1C     *IN99         IFEQ      *ON
120900000000     C     *IN99         ANDEQ     *OFF
121000000000     C* TRAINI
121100000000     C                   UPDATE    TNTLT000
121200000000     C                   WRITE     TNTLT000
121300000000     C* FERMATE
121400000000     C                   UPDATE    TNTLR000
121500000000     C                   WRITE     TNTLR000
121600000000E   1C                   ENDIF
121700000000     C*
121800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
121900000000     C                   EXSR      INZSTD
122000000000     C*
122100000000     C* CARICA LE TABELLE
122200000000     C                   EXSR      CARTAB
122300000000     C*
122400000000     C* IMPOSTAZIONI VIDEO
122500000000     C                   EXSR      IMPVID
122600000000     C*
122700000000     C* MEMORIZZA I DATI DEL TRAINI
122800000000     C                   EXSR      MEMTRN
122900000000     C*
123000000000     C* EMETTE VIDEATA
123100000000IF  1C     $ERR          IFEQ      '0'                                          ERRORE IN ALLOCAZION
123200000000     C                   EXSR      CARV1
123300000000E   1C                   ENDIF
123400000000     C*
123500000000     C                   ENDSR
123600000000     C*------------------------------------------------------------------------*
123700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
123800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
123900000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
124000000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
124100000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
124200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
124400000000     C*------------------------------------------------------------------------*
124500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
124600000000     C*------------------------------------------------------------------------*
124700000000     C     INZSTD        BEGSR
124800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
124900000000     C                   CALL      'XKEYS'
125000000000     C                   PARM                    DSKEYS
125100000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
125200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
125300000000     C     XMSGPA        PLIST
125400000000     C                   PARM                    DSMSMS
125500000000     C                   PARM                    DSMSPG
125600000000     C                   PARM                    DSMSKE
125700000000     C                   PARM                    DSMSER
125800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
125900000000     C                   MOVEL     'TNTL09R'     DSMSPG                         *NOME PGM CHIAMANTE
126000000000     C                   MOVEL     'TNTL09R'     VMCQUE                         *CODA MESSAGGI DESTI
126100000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
126200000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
126300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
126400000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
126500000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
126600000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
126700000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
126800000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
126900000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
127000000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
127100000000     C* LANCIO ROUTINE PER VERIFICARE SE
127200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
127300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
127400020715
127500020715     c     *dtaara       define    §azute        azuteds
127600020715     c     *dtaara       define    §datiute      ddatiute
127700020715
127800020715     c                   in(E)     *dtaara
127900020715     c                   if        %Error or rsut = *blanks
128000020715     c                   clear                   tibs34ds
128100020715     c                   call      'TIBS34R'
128200020715     c                   parm                    Tibs34Ds
128300020715     c                   in        *dtaara
128400020715     c                   endif
128500000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
128600000000     C                   BITOFF    '01234567'    POSMI1            1
128700000000     C                   BITOFF    '01234567'    POSMI2            1
128800000000     C                   BITOFF    '01234567'    POSMA1            1
128900000000     C                   BITOFF    '01234567'    POSMA2            1
129000000000     C                   BITON     '34'          POSMI1            1            *18
129100000000     C                   BITON     '7'           POSMI2            1            *01
129200000000     C                   BITON     '34'          POSMA1            1            *18
129300000000     C                   BITON     '0'           POSMA2            1            *80
129400000000     C                   MOVE      POSMI2        POSMIN            2
129500000000     C                   MOVEL     POSMI1        POSMIN
129600000000     C                   MOVE      POSMA2        POSMAX            2
129700000000     C                   MOVEL     POSMA1        POSMAX
129800000000     C                   ENDSR
129900000000     C*------------------------------------------------------------------------*
130000000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
130100000000     C*------------------------------------------------------------------------*
130200000000     C     FUNSTD        BEGSR
130300000000     C                   SELECT
130400000000     C     KEY           WHENEQ    F3
130500000000     C                   EXSR      F03STD
130600000000     C     KEY           WHENEQ    F12
130700000000     C                   EXSR      F12STD
130800000000     C     KEY           WHENEQ    F24
130900000000     C                   EXSR      F24STD
131000000000     C                   OTHER
131100000000     C                   MOVEL     MSG(2)        $MSG
131200000000     C                   EXSR      CARMSG
131300000000     C                   ENDSL
131400000000     C                   ENDSR
131500000000     C*------------------------------------------------------------------------*
131600000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
131700000000     C*------------------------------------------------------------------------*
131800000000     C     F03STD        BEGSR
131900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
132000000000     C                   MOVEL     '1'           $F03
132100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
132200000000     C                   MOVEL     '1'           $FINE
132300951212     C* PULISCE IL MESSAGGIO
132400951212     C                   MOVEL     *BLANKS       D00MSG
132500000000     C                   ENDSR
132600000000     C*------------------------------------------------------------------------*
132700000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
132800000000     C*------------------------------------------------------------------------*
132900000000     C     F12STD        BEGSR
133000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
133100000000     C                   MOVEL     '1'           $F12
133200000000     C* IMPOSTO FLAG DI FIN PGM
133300000000     C                   MOVEL     '1'           $FINE
133400951212     C* PULISCE IL MESSAGGIO
133500951212     C                   MOVEL     *BLANKS       D00MSG
133600000000     C                   ENDSR
133700000000     C*------------------------------------------------------------------------*
133800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
133900000000     C*------------------------------------------------------------------------*
134000000000     C     F24STD        BEGSR
134100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
134200000000     C                   EXSR      OUTFUN
134300000000     C                   ENDSR
134400000000     C*------------------------------------------------------------------------*
134500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
134600000000     C*          CODA MESSAGGI DEL PGM
134700000000     C*------------------------------------------------------------------------*
134800000000     C     CARMSG        BEGSR
134900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
135000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
135100000000     C     MSGULT        IFEQ      0
135200000000     C                   MOVEL     '1'           *IN40
135300000000     C                   WRITE     TL09DVK
135400000000     C                   MOVEL     '0'           *IN40
135500000000     C                   ENDIF
135600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
135700000000     C                   MOVEL     $MSG          DSMSMS
135800000000     C                   CALL      'XMSG'        XMSGPA
135900000000     C     DSMSER        IFEQ      0
136000000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
136100000000     C                   ENDIF
136200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
136300000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
136400000000     C                   WRITE     TL09DVM                                      *SFL MESS
136500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
136600000000     C                   MOVEL     *BLANKS       $MSG
136700000000     C                   ENDSR
136800000000     C*------------------------------------------------------------------------*
136900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
137000000000     C*------------------------------------------------------------------------*
137100000000     C     OUTMSG        BEGSR
137200000000     C* VISUALIZZAZIONE MESSAGGIO
137300000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
137400000000     C                   WRITE     TL09DVK                                      *CONTR MESS
137500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
137600000000     C                   Z-ADD     *ZEROS        MSGULT
137700000000     C                   ENDSR
137800000000     C*------------------------------------------------------------------------*
137900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
138000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
138100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
138200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
138300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
138400000000     C*------------------------------------------------------------------------*
138500000000     C     CARFUN        BEGSR
138600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
138700000000     C     1             DO        31            I                 5 0          --- 1 -->
138800000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
138900000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
139000000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
139100020715     c                   If        DutLpo = 'S'
139200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
139300000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
139400000000     C                   ELSE                                                   *INTERROG.
139500000000     C                   MOVEL     FUNASI        FAT(I)
139600000000     C                   ENDIF
139700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
139800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
139900000000     C                   MOVEL     FUNAFG        FAT(I)
140000000000     C                   ELSE                                                   *INTERROG.
140100000000     C                   MOVEL     FUNAFI        FAT(I)
140200000000     C                   ENDIF
140300000000     C                   ENDIF                                                  <-- 2 ---
140400000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
140500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
140600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
140700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
140800000000     C                   ELSE
140900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
141000000000     C                   ENDIF
141100000000     C                   ENDIF
141200000000     C                   ENDDO                                                  <-- 1 ---
141300000000     C                   ENDSR
141400000000     C*------------------------------------------------------------------------*
141500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
141600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
141700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
141800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
141900000000     C*------------------------------------------------------------------------*
142000000000     C     OUTFUN        BEGSR
142100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
142200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
142300000000     C                   ENDDO
142400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
142500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
142600000000     C     1             DO        31            I
142700000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
142800000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
142900000000     C                   ADD       1             FUNNAT
143000000000     C                   ENDIF
143100000000     C                   ENDDO
143200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
143300000000     C     FUNNAT        IFGT      8
143400000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
143500000000     C                   ENDIF
143600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
143700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
143800000000     C                   Z-ADD     8             FUNVIS            5 0
143900000000     C     FAT(23)       IFEQ      'S'
144000000000     C                   SUB       1             FUNVIS
144100000000     C                   ENDIF
144200000000     C     FAT(24)       IFEQ      'S'
144300000000     C                   SUB       1             FUNVIS
144400000000     C                   ENDIF
144500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
144600000000     C                   Z-ADD     *ZEROS        W                 5 0
144700000000     C                   ADD       1             FUNULT
144800000000     C     FUNULT        IFGT      31
144900000000     C                   Z-ADD     1             FUNULT
145000000000     C                   ENDIF
145100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
145200000000     C     FUNULT        DO        31            I                              --- 1 -->
145300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
145400000000     C     W             IFLT      FUNVIS                                       --- 2 -->
145500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
145600000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
145700000000     C     FDE(I)        ANDNE     *BLANKS
145800000000     C     I             IFEQ      23                                           *TOLGO F23
145900000000     C     I             OREQ      24                                           *E F24
146000000000     C                   ELSE
146100000000     C                   ADD       1             W
146200000000     C                   MOVEL     FDE(I)        FUV(W)
146300000000     C                   ENDIF
146400000000     C                   ENDIF                                                  <-- 3 ---
146500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
146600000000     C                   Z-ADD     I             FUNULT
146700000000     C                   ENDIF                                                  <-- 2 ---
146800000000     C                   ENDDO                                                  <-- 1 ---
146900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
147000000000     C                   MOVEL     FUV(1)        VFD001
147100000000     C                   MOVEL     FUV(2)        VFD002
147200000000     C                   MOVEL     FUV(3)        VFD003
147300000000     C                   MOVEL     FUV(4)        VFD004
147400000000     C                   MOVEL     FUV(5)        VFD005
147500000000     C                   MOVEL     FUV(6)        VFD006
147600000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
147700000000     C     FAT(24)       ANDEQ     'N'
147800000000     C                   MOVEL     FUV(7)        VFD007
147900000000     C                   MOVEL     FUV(8)        VFD008
148000000000     C                   ELSE                                                   <-- 1 -->
148100000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
148200000000     C     FAT(24)       ANDEQ     'S'
148300000000     C                   MOVEL     FDE(23)       VFD007
148400000000     C                   MOVEL     FDE(24)       VFD008
148500000000     C                   ELSE                                                   <-- 2 -->
148600000000     C                   MOVEL     FUV(7)        VFD007
148700000000     C     FAT(23)       IFEQ      'S'
148800000000     C                   MOVEL     FDE(23)       VFD008
148900000000     C                   ELSE
149000000000     C                   MOVEL     FDE(24)       VFD008
149100000000     C                   ENDIF
149200000000     C                   ENDIF                                                  <-- 2 ---
149300000000     C                   ENDIF                                                  <-- 1 ---
149400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
149500000000     C                   ENDSR
149600000000     C*------------------------------------------------------------------------*
149700000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
149800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
149900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
150000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
150100000000     C*------------------------------------------------------------------------*
150200000000     C     CONFUN        BEGSR
150300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
150400000000     C     1             DO        31            I                              --- 1 -->
150500000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
150600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
150700000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
150800000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
150900000000     C                   EXSR      CARMSG
151000000000     C                   MOVEL     '1'           $ERR
151100000000     C                   ENDIF                                                  <-- 3 ---
151200000000     C                   ENDIF                                                  <-- 2 ---
151300000000     C                   ENDDO                                                  <-- 1 ---
151400000000     C                   ENDSR
151500000000     C*------------------------------------------------------------------------*
151600000000** - MSG MESSAGGI
151700000000TNTL09R- Tasto funzionale non valido.                                          1
151800000000TNTL09R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
151900000000TNTL09R- Opzione non valida.                                                   3
152000000000TNTL09R- Opzione abilitata ma non gestita. Avvertire CED.                      4
152100000000TNTL09R- Inizio elenco.                                                        5
152200000000TNTL09R- Fine elenco.                                                          6
152300960202TNTL09R- La filiale di scarico non e' un terminal di arrivo                    7
152400960202TNTL09R- Filiale di scarico errata per L.part e term arr. inseriti da eccez.   8
152500960202TNTL09R- Esiste eccezione terminal con decor/scad incongruente con il traino   9
152600000000TNTL09R- Filiale errata                                                       10
152700000000TNTL09R- La filiale non è presente come fermata di carico.                    11
152800000000TNTL09R- La filiale non è presente come fermata di scarico.                   12
152900000000TNTL09R- La filiale arrivo deve essere terminal arrivo per la fil di partenza 13
153000000000TNTL09R- La filiale arrivo deve essere uguale alla filiale di scarico         14
153100000000TNTL09R- La filiale arrivo deve essere uguale alla filiale di partenza        15
153200000000TNTL09R- La filiale partenza non può scaricare ad una fermata precedente      16
153300960424TNTL09R- Nei traino locali partenza, arrivo e scarico devono essere uguali    17
153400960904TNTL09R- Inserire la filiale di scarico uguale alla filiale arrivo            18
153500000000TNTL09R- La filiale partenza deve essere diversa dalla filiale di scarico     19
153600000000TNTL09R- La filiale partenza deve essere diversa dalla filiale di arrivo      20
153700960424TNTL09R- Nei traini inoltro arrivo e scarico devono essere uguali             21
153800960424TNTL09R- Inserire i metri cubi "normali"                                      22
153900960424TNTL09R- Inserire i metri cubi "bis"                                          23
154000000000TNTL09R- Richiesta funzione per record già esistente                          24
154100000000TNTL09R- Record inesistente                                                   25
154200000000TNTL09R- Record momentaneamente allocato: riprovare più tardi                 26
154300000000TNTL09R- Inserimento effettuato                                               27
154400000000TNTL09R- Variazione effettuata                                                28
154500000000TNTL09R- Annullamento effettuato                                              29
154600000000TNTL09R- Ripristino effettuato                                                30
154700000000** - FUN  FUNZIONI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
154800000000F1    -                   -    NN     NN
154900000000F2    -                   -    NN     NN
155000000000F3    -F3=Fine            -    NN     NN
155100000000F4    -                   -    NN     NN
155200000000F5    -                   -    NN     NN
155300000000F6    -                   -    NN     NN
155400000000F7    -                   -    NN     NN
155500000000F8    -                   -    NN     NN
155600000000F9    -                   -    NN     NN
155700000000F10   -                   -    NN     NN
155800000000F11   -                   -    NN     NN
155900000000F12   -F12=Ritorno        -    SS     SS
156000000000F13   -                   -    NN     NN
156100000000F14   -                   -    NN     NN
156200000000F15   -                   -    NN     NN
156300000000F16   -                   -    NN     NN
156400000000F17   -                   -    NN     NN
156500000000F18   -                   -    NN     NN
156600000000F19   -                   -    NN     NN
156700000000F20   -                   -    NN     NN
156800000000F21   -                   -    NN     NN
156900000000F22   -                   -    NN     NN
157000000000F23   -                   -    NN     NN
157100000000F24   -F24=Altri tasti    -    NN     NN
157200000000ENTER -                   -    SS     SS
157300000000ROLLUP-                   -    NN     NN
157400000000ROLLDN-                   -    NN     NN
157500000000HELP  -                   -    NN     NN
157600000000PRINT -                   -    NN     NN
157700000000BSPACE-                   -    NN     NN
157800000000CLEAR -                   -    NN     NN
157900000000** DOPZ  (OPZIONE RICHIAMATA)
158000000000* INSERIMENTO *     1
158100000000*  VARIAZIONE *     2
158200000000*CANCELLAZIONE*     3
158300000000*  RIPRISTINO *     4
1584000000005
