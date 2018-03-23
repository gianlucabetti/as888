000100930604     H DECEDIT('0,') DATEDIT(*DMY/)
000200931129     H* TRTB48R *--------------------------------------------------------------*
000300931129     H*-----------------*                                                      *
000400931129     H*                  - GESTIONE TABELLA FASCE DI PESO -                    *
000500931129     H*------------------------------------------------------------------------*
000600931129     H* FILE   : -------- - ----------------------------------------------------
000700931129     H*          TABEL00F - TABELLE GENERICHE
000800931129     H*          TRTB48D  - FILE VIDEO
000900931129     H*                     TB48DVT / TESTATA VIDEO
001000931129     H*                     TB48DV1 / RICHIESTA CODICE FASCIA
001100931129     H*                     TB48DV2 / RICHIESTA DATI FASCIA
001200931129     H*                     TB48MSG / MESSAGGI DI ERRORE
001300931129     H* PGM    : -------- - ----------------------------------------------------
001400931129     H*          X§TABER  - RICERCA GENERICA IN TABELLE
001500931129     H* SBR    : -------- - ----------------------------------------------------
001600931129     H*          *INZSR   - IMPOSTAZIONI INIZIALI
001700931130     H*          GESTD1   - GESTIONE PRIMO FORMATO VIDEO
001800931130     H*          GESTD2   - GESTIONE SECONDO FORMATO VIDEO
001900931129     H*          CONTR1   - CONTROLLO PRIMO FROMATO VIDEO
002000931129     H*          CONTR2   - CONTROLLO SECPONDO FORMATO VIDEO
002100931130     H*          RIEMP2   - RIEMPIMENTO CAMPI SECONDO FORMATO VIDEO
002200931129     H*------------------------------------------------------------------------*
002300931129     H* INDICATORI USATI
002400931129     H*------------------------------------------------------------------------*
002500931129     H* 01     - INSERIMENTO/VARIAZIONE
002600931130     H* 02     - ANNULLAMENTO/RIPRISTINO
002700931129     H* 10     - PER CODICE FASCIA
002800931130     H* 11     - PER DESCRIZIONE FASCIA
002900931130     H* 12     - PER PESO INIZIALE FASCIA
003000931130     H* 13     - PER PESO FINALE   FASCIA
003100931129     H* 99     - DI COMODO
003200931129     F*------------------------------------------------------------------------*
003300931129     F* DATA BASE
003400931129     F*------------------------------------------------------------------------*
003500931130     FTABEL00F  UF A E           K DISK
003600931129     FTRTB48D   CF   E             WORKSTN
003700931129     D*------------------------------------------------------------------------*
003800931129     D* SCHIERE                                                                *
003900931129     D*------------------------------------------------------------------------*
004000931129     D*------------------
004100931129     D* DESCRIZIONE OPERAZIONE
004200931129     D*------------------
004300931130     D OPE             S             11    DIM(3) CTDATA PERRCD(1)
004400931129     D*------------------
004500931129     D* MESSAGGI DI ERRORE
004600931129     D*------------------
004700931130     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
004800931129     D*------------------------------------------------------------------------*
004900931129     D* INPUT
005000931129     D*------------------------------------------------------------------------*
005100931129     D*------------------
005200931129     D* ARCHITETTURA
005300931129     D*------------------
005400931129     D KPJBA         E DS
005500931129     D*------------------
005600931129     D* DS FASCE DI PESO
005700931129     D*------------------
005800931129     D DS2L          E DS
005900931129     D*------------------
006000020923     D* DS REPERIMENTO DATI UTENTE
006100020923     D*-------------------
006200020923     D TIBS34DS      E DS
006300020923     D DDATIUTE      E DS
006400020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006500980130     D*------------------
006600980130     D* DS CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
006700980130     D*------------------
006800980130     D UL12DS        E DS                  EXTNAME(TRUL12DS)
006900931129     C*------------------------------------------------------------------------*
007000931129     C*?MAIN   - PROGRAMMA GESTIONE TABELLA FASCE DI PESO
007100931129     C*------------------------------------------------------------------------*
007200931129     C*
007300931129     C* FINO A FINE LAVORO
007400931129     C     $END          DOUEQ     'S'
007500931129     C*
007600931130     C     $GES          CASEQ     '1'           GESTD1
007700931130     C     $GES          CASEQ     '2'           GESTD2
007800931129     C                   ENDCS
007900931129     C*
008000931129     C                   ENDDO
008100931129     C*
008200931129     C                   SETON                                        LR
008300931129     C*------------------------------------------------------------------------*
008400931130     C*?GESTD1 - GESTIONE PRIMO FORMATO VIDEO
008500931129     C*------------------------------------------------------------------------*
008600931130     C     GESTD1        BEGSR
008700931129     C*
008800931129     C                   WRITE     TB48DVT
008900931129     C                   EXFMT     TB48DV1
009000931129     C*
009100931129     C* F3-FINE LAVORO
009200931129IF  1C     *INKC         IFEQ      *ON
009300931129     C                   MOVEL     'S'           $END
009400931129X   1C                   ELSE
009500931129     C*
009600931129     C* CONTROLLO DATI PRIMO FORMATO VIDEO
009700931129     C                   EXSR      CONTR1
009800931129     C*
009900931130     C* SE NO ERRORI
010000931129IF  2C     $ERR          IFEQ      '0'
010100931130     C                   EXSR      RIEMP2
010200931130     C                   MOVEL     '2'           $GES
010300931129E   2C                   ENDIF
010400931129     C*
010500931129E   1C                   ENDIF
010600931129     C*
010700931129     C                   ENDSR
010800931129     C*------------------------------------------------------------------------*
010900931129     C*?CONTR1 - CONTROLLO DATI PRIMO FORMATO VIDEO
011000931129     C*------------------------------------------------------------------------*
011100931129     C     CONTR1        BEGSR
011200931130     C*
011300931130     C* REIMPOSTO VARIABILI E INDICATORI
011400931130     C                   MOVEL     '0'           $ERR
011500931130     C                   MOVEL     *BLANKS       V1DERR
011600931130     C                   SETOFF                                       100102
011700931129     C*
011800931129     C* CODICE FASCIA OBBLIGATORIO
011900931129IF  1C     V1CKEY        IFEQ      *BLANKS
012000931129     C     V1CKEY        OREQ      *ZEROS
012100931129     C                   MOVEL     '1'           $ERR
012200931130     C                   MOVEL     MSG(1)        V1DERR
012300931129     C                   SETON                                        10
012400931129     C*
012500931129     C* CONTROLLO CODICE
012600931129X   1C                   ELSE
012700931129     C     '?'           SCAN      V1CKEY                                 99
012800931129     C*
012900931129     C* RICHIESTO PUNTO INTERROGATIVO (RICERCA)
013000931129IF  2C     *IN99         IFEQ      *ON
013100931129     C*
013200931129     C                   MOVEL     CODUT         §KUT
013300931129     C                   MOVEL     '2L'          §COD
013400931129     C                   MOVEL     *BLANKS       §KEY
013500931129     C                   MOVEL     *BLANKS       §DES             30
013600931129     C                   CALL      'X§TABER'
013700931129     C                   PARM                    §KUT
013800931129     C                   PARM                    §COD
013900931129     C                   PARM                    §KEY
014000931129     C                   PARM                    §DES
014100931129     C*
014200931129     C                   MOVEL     §KEY          V1CKEY
014300931129     C                   MOVEL     '1'           $ERR
014400931129X   2C                   ELSE
014500931129     C*
014600931129     C* RICERCA DEL CODICE IN TABELLA
014700931129     C                   Z-ADD     CODUT         KTBKUT
014800931129     C                   MOVEL     '2L'          KTBCOD
014900931129     C                   MOVEL     V1CKEY        KTBKEY
015000931129     C     KEYTAB        CHAIN     TABEL00F                           99
015100931129     C*
015200931129     C* CODICE INESISTENTE
015300931129IF  3C     *IN99         IFEQ      *ON
015400931130     C                   SETON                                        01
015500931130X   3C                   ELSE
015600931130     C* CODICE ESISTENTE
015700931130IF  4C     TBLFLG        IFNE      *BLANKS
015800931130     C                   SETON                                        02
015900931130E   4C                   ENDIF
016000931130E   3C                   ENDIF
016100931129     C*
016200931130E   2C                   ENDIF
016300931130E   1C                   ENDIF
016400931130     C*
016500931129     C                   ENDSR
016600931130     C*------------------------------------------------------------------------*
016700931130     C*?RIEMP2 - RIEMPIMENTO CAMPI SECONDO FORAMTO VIDEO
016800931130     C*------------------------------------------------------------------------*
016900931130     C     RIEMP2        BEGSR
017000931130     C*
017100931130     C                   MOVEL     V1CKEY        V2CKEY
017200931130     C*
017300931130     C* FASCIA INESISTENTE
017400931130IF  1C     *IN01         IFEQ      *ON
017500931130     C                   MOVEL     *BLANKS       V2DDFP
017600931130     C                   Z-ADD     *ZEROS        V2CKGI
017700931130     C                   Z-ADD     *ZEROS        V2CKGF
017800931130     C                   MOVEL     OPE(1)        V2DOPE
017900931130     C* FASCIA ESISTENTE
018000931130X   1C                   ELSE
018100931130     C*
018200931130     C                   MOVEL     TBLUNI        DS2L
018300931130     C                   MOVEL     §2LDFP        V2DDFP
018400931130     C                   Z-ADD     §2LKGI        V2CKGI
018500931130     C                   Z-ADD     §2LKGF        V2CKGF
018600931130     C*
018700931130IF  2C     *IN02         IFEQ      *ON
018800931130     C                   MOVEL     OPE(3)        V2DOPE
018900931130X   2C                   ELSE
019000931130     C                   MOVEL     OPE(2)        V2DOPE
019100931130E   2C                   ENDIF
019200931130E   1C                   ENDIF
019300931130     C*
019400931130     C                   ENDSR
019500931129     C*------------------------------------------------------------------------*
019600931130     C*?GESTD2 - GESTIONE SECONDO FORMATO VIDEO
019700931129     C*------------------------------------------------------------------------*
019800931130     C     GESTD2        BEGSR
019900931130     C*
020000931130     C                   EXFMT     TB48DV2
020100931130     C*
020200931130     C* F3-FINE LAVORO
020300931130IF  1C     *INKC         IFEQ      *ON
020400931130     C                   MOVEL     'S'           $END
020500931130X   1C                   ELSE
020600931130     C*
020700931130     C* F12-RITORNO VIDEATA PRECEDENTE
020800931130IF  2C     *INKL         IFEQ      *ON
020900931130     C                   CLEAR                   TB48DV2
021000931130     C                   MOVEL     '1'           $GES
021100931130X   2C                   ELSE
021200931130     C*
021300931130     C* CONTROLLO DATI SECONDO FORMATO VIDEO
021400931130     C                   EXSR      CONTR2
021500931130     C*
021600931130     C* SE NO ERRORI
021700931130IF  3C     $ERR          IFEQ      '0'
021800931130     C*
021900931130     C* F06-AGGIORNAMENTO
022000931130IF  4C     *INKF         IFEQ      *ON
022100931130     C                   EXSR      AGGIO
022200931130     C                   MOVEL     '1'           $GES
022300931130E   4C                   ENDIF
022400931130     C*
022500931130     C* F05-RIPRISTINO
022600931130IF  4C     *INKE         IFEQ      *ON
022700931130     C                   EXSR      RIPRI
022800931130     C                   MOVEL     '1'           $GES
022900931130E   4C                   ENDIF
023000931130     C*
023100931130     C* F16-ANNULLAMENTO
023200931130IF  4C     *INKQ         IFEQ      *ON
023300931130     C                   EXSR      ANNUL
023400931130     C                   MOVEL     '1'           $GES
023500931130E   4C                   ENDIF
023600931130E   3C                   ENDIF
023700931130     C*
023800931130E   2C                   ENDIF
023900931130E   1C                   ENDIF
024000931130     C*
024100931130     C                   ENDSR
024200931130     C*------------------------------------------------------------------------*
024300931130     C*?CONTR2 - CONTROLLO DATI SECONDO FORMATO VIDEO
024400931130     C*------------------------------------------------------------------------*
024500931130     C     CONTR2        BEGSR
024600931130     C*
024700931130     C* REIMPOSTAO VARIABILI E INDICATORI
024800931130     C                   MOVEL     '0'           $ERR
024900931130     C                   MOVEL     *BLANKS       V2DERR
025000931130     C                   SETOFF                                       111213
025100931130     C*
025200931130     C* DESCRIZIONE OBBLIGATORIA
025300931130IF  1C     V2DDFP        IFEQ      *BLANKS
025400931130     C                   MOVEL     '1'           $ERR
025500931130     C                   MOVEL     MSG(2)        V2DERR
025600931130     C                   SETON                                        11
025700931130E   1C                   ENDIF
025800931130     C*
025900931130     C* PESO FINALE OBBLIGATORIO
026000931130IF  1C     $ERR          IFEQ      '0'
026100931130IF  2C     V2CKGF        IFEQ      *ZEROS
026200931130     C                   MOVEL     '1'           $ERR
026300931130     C                   MOVEL     MSG(3)        V2DERR
026400931130     C                   SETON                                        13
026500931130E   2C                   ENDIF
026600931130E   1C                   ENDIF
026700931130     C*
026800931130     C* PESO FINALE INFERIORE AL PESO INIZIALE
026900931130IF  1C     $ERR          IFEQ      '0'
027000931130IF  2C     V2CKGF        IFLT      V2CKGI
027100931130     C                   MOVEL     '1'           $ERR
027200931130     C                   MOVEL     MSG(4)        V2DERR
027300931130     C                   SETON                                        13
027400931130E   2C                   ENDIF
027500931130E   1C                   ENDIF
027600931130     C*
027700931130     C                   ENDSR
027800931130     C*------------------------------------------------------------------------*
027900931130     C*?AGGIO  - AGGIORNAMENTO RECORD TABELLA
028000931130     C*------------------------------------------------------------------------*
028100931130     C     AGGIO         BEGSR
028200931130     C*
028300931130     C                   MOVEL     *BLANKS       TBLFLG
028400940517     C                   MOVEL     '1'           TBLFTT
028500931130     C                   Z-ADD     *ZEROS        TBLFLT
028600931130     C                   MOVEL     *BLANKS       TBLFTR
028700940517     C                   Z-ADD     *ZEROS        TBLDTR
028800931130     C*
028900931130     C                   MOVEL     V2DDFP        §2LDFP
029000931130     C                   Z-ADD     V2CKGI        §2LKGI
029100931130     C                   Z-ADD     V2CKGF        §2LKGF
029200931130     C                   MOVEL     DS2L          TBLUNI
029300931130     C*
029400931130IF  1C* INSERIMENTO
029500931130     C     *IN01         IFEQ      *ON
029600931130     C                   MOVEL     CODUT         TBLKUT
029700931130     C                   MOVEL     '2L'          TBLCOD
029800931130     C                   MOVEL     V2CKEY        TBLKEY
029900931130     C*
030000931130     C                   WRITE     TABEL
030100931130X   1C                   ELSE
030200931130     C                   UPDATE    TABEL
030300931130E   1C                   ENDIF
030400931130     C*
030500931130     C                   ENDSR
030600931130     C*------------------------------------------------------------------------*
030700931130     C*?RIPRI  - RIPRISTINO RECORD TABELLA
030800931130     C*------------------------------------------------------------------------*
030900931130     C     RIPRI         BEGSR
031000931130     C*
031100931130     C                   MOVEL     *BLANKS       TBLFLG
031200940517     C                   MOVEL     '1'           TBLFTT
031300931130     C                   Z-ADD     *ZEROS        TBLFLT
031400931130     C                   MOVEL     *BLANKS       TBLFTR
031500940517     C                   Z-ADD     *ZEROS        TBLDTR
031600931130     C*
031700931130     C                   UPDATE    TABEL
031800931130     C*
031900931130     C                   ENDSR
032000931130     C*------------------------------------------------------------------------*
032100931130     C*?ANNUL  - ANNULLAMENTO RECORD TABELLA
032200931130     C*------------------------------------------------------------------------*
032300931130     C     ANNUL         BEGSR
032400931130     C*
032500931130     C                   MOVEL     '*'           TBLFLG
032600940517     C                   MOVEL     '1'           TBLFTT
032700931130     C                   Z-ADD     *ZEROS        TBLFLT
032800931130     C                   MOVEL     *BLANKS       TBLFTR
032900940517     C                   Z-ADD     *ZEROS        TBLDTR
033000931130     C*
033100931130     C                   UPDATE    TABEL
033200931130     C*
033300931130     C                   ENDSR
033400020923     C*-------------------------------------------------------------------------
033500020923     C* REPERISCE I DATI UTENTE
033600020923     C*-------------------------------------------------------------------------
033700020923     C     REPDATIUTE    BEGSR
033800020923     C*
033900020923     C* INIZIALIZZA VARIABILI DI WRK
034000020923     C                   CLEAR                   TIBS34DS
034100020923     C                   CLEAR                   AZUTEDS
034200020923     C                   CLEAR                   DDATIUTE
034300020923     C*
034400020923     C     *DTAARA       DEFINE    §azute        azuteds
034500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
034600020923     C                   IN(E)     *DTAARA
034700020923if  1C                   IF        %Error
034800020923     c                   EVAL      I34Tla = 'L'
034900020923     C                   CALL      'TIBS34R'
035000020923     C                   PARM                    Tibs34Ds
035100020923     C                   IN        *DTAARA
035200020923e   1C                   ENDIF
035300020923     C*
035400020923     C                   ENDSR
035500931129     C*------------------------------------------------------------------------*
035600931129     C*?*INZSR - IMPOSTAZIONI INIZIALI
035700931129     C*------------------------------------------------------------------------*
035800931129     C     *INZSR        BEGSR
035900931129     C*
036000931129     C* RICEVIMENTO PARAMETRI
036100931129     C     *ENTRY        PLIST
036200931129     C                   PARM                    KPJBA
036300931129     C*
036400931129     C* DEFINIZIONI VARIABILI
036500931129     C     *LIKE         DEFINE    TBLKUT        KTBKUT
036600931129     C     *LIKE         DEFINE    TBLCOD        KTBCOD
036700931129     C     *LIKE         DEFINE    TBLKEY        KTBKEY
036800931129     C     *LIKE         DEFINE    TBLKUT        §KUT
036900931129     C     *LIKE         DEFINE    TBLCOD        §COD
037000931129     C     *LIKE         DEFINE    TBLKEY        §KEY
037100931129     C*
037200931129     C* CHIAVE LETTURA TABEL00F - COMPLETA
037300931129     C     KEYTAB        KLIST
037400931129     C                   KFLD                    KTBKUT
037500931129     C                   KFLD                    KTBCOD
037600931129     C                   KFLD                    KTBKEY
037700931129     C*
037800931129     C* RICERCA FILIALE IN CUI SONO
037900020923     C                   EXSR      REPDATIUTE
038000020923     C                   Z-ADD     1             CODUT             1 0
038100931129     C*
038200931129     C* IMPOSTAZIONI INIZIALI VARIABILI DI LAVORO
038300931129     C                   MOVEL     'N'           $END              1
038400931129     C                   MOVEL     '1'           $GES              1
038500931129     C                   MOVEL     '0'           $ERR              1
038600980130     C*
038700980130     C* CONTROLLA INDICE E MODIFICABILITA' DI UNA TABELLA
038800980130     C                   CLEAR                   UL12DS
038900980130     C                   MOVEL     'L'           I12TLA
039000980130     C                   MOVEL     '2L'          I12COD
039100980130     C                   MOVEL     KNSIF         I12NSI
039200980130     C                   CALL      'TRUL12R'
039300980130     C                   PARM                    UL12DS
039400980130     C     O12ERR        IFEQ      '1'
039500980130     C                   MOVEL     'S'           $END
039600980130     C                   ENDIF
039700931129     C*
039800931129     C* IMPOSTAZIONI INIZIALI 1° FORMATO VIDEO
039900931129     C                   MOVEL     '?'           V1CKEY
040000931129     C*
040100931129     C                   ENDSR
040200931129     O*------------------------------------------------------------------------*
040300931130**   OPE - OPERAZIONI EFFETTUABILI
040400931129INSERIMENTO
040500931129VARIAZIONE
040600931130ANNULLATO
040700931129**   MSG - MESSAGGI DI ERRORE FORMATI VIDEO
040800931129TRTB48R - Codice fascia obbilgatorio                                          1
040900931130TRTB48R - Descrizione fascia obbligatoria                                     2
041000931130TRTB48R - Peso finale obbligatorio                                            3
041100931130TRTB48R - Peso finale inferiore al peso iniziale                              4
041200931130TRTB48R -                                                                     5
