000100970110     H*------------------------------------------------------------------------*
000200970110     H* TARIFFE DA CONTO ECONOMICO - STAMPA DATI TIPO - FILTRO                 *
000300970110     H*------------------------------------------------------------------------*
000400970110     H DATEDIT(*YMD)
000500970110     F*------------------------------------------------------------------------*
000600970110     F* DATA BASE
000700970110     F*------------------------------------------------------------------------*
000800970110     FTNTE42D   CF   E             WORKSTN
000900970110     FAZORG01L  IF   E           K DISK
001000970110     FTNTAM01L  IF   E           K DISK
001100060808     FTNVIS05L  IF   E           K DISK
001200060808     FTNOFM01L  IF   E           K DISK
001300970110     F                                     RENAME(TNTAM000:TNOFM000)
001400970110     D*------------------------------------------------------------------------*
001500970110     D* SCHIERE
001600970110     D*------------------------------------------------------------------------*
001700970110     D*-------------------
001800970110     D* MESSAGGI VIDEO
001900970110     D*-------------------
002000970110     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
002100970110     D*------------------------------------------------------------------------*
002200970110     D* INPUT
002300970110     D*------------------------------------------------------------------------*
002400020925     D*------------
002500020925     D* DS REPERIMENTO DATI UTENTE
002600020925     D*------------
002700020925     D TIBS34DS      E DS                                                       *Profili utente
002800020925     D DDATIUTE      E DS                                                       *Dati utente
002900020925     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003000020925     D*------------
003100020925     D* DS CONTROLLO/SCELTA TABELLE
003200020925     D*------------
003300020925     D TIBS02DS      E DS                                                       *Ricerca tabella
003400020925     D*------------------
003500020925     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
003600020925     D*-------------------
003700020925     D DUTE01        E DS                  INZ                                  *Abilitazioni utenti
003800020925     D*------------------
003900020925     D* DS FUNZIONI AZIENDALI
004000020925     D*-------------------
004100020925     D DLAT          E DS                  INZ                                  *Funzioni aziendali
004200020925     D*------------------
004300020925     D* DS CARICAMENTO P.O. GESTITI
004400020925     D*-------------------
004500020925     D TRUL31DS      E DS                  INZ
004600020925     D*------------------
004700020925     D* SCHIERA X LINEEE CONSENTITE
004800020925     D*------------------
004900020925     D L1              S              3    DIM(250)
005000970110     D*-------------------
005100970110     D* DS DI PROCEDURA
005200970110     D*-------------------
005300970110     D DSTE00        E DS                  EXTNAME(TNTE00DS)
005400011016     D*-------------------
005500011016     D* DS DI LETTURA TNTAM00F
005600011016     D*-------------------
005700011016     D DSTA01        E DS
005800970110     D*-------------------
005900970110     D* ARCHITETTURA
006000970110     D*-------------------
006100970110     D KPJBA         E DS
006200970110     C*------------------------------------------------------------------------*
006300970110     C* MAIN LINE
006400970110     C*------------------------------------------------------------------------*
006500970110     C*
006600970110     C* IMPOSTAZIONI PRIMA VIDEATA
006700970110     C                   EXSR      IMPVD1
006800970110     C*
006900970110     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
007000970110DO  1C     $FINE         DOUEQ     'S'
007100970110     C     TIPVID        CASEQ     '1'           GESVD1                         *1° VIDEO
007200970110     C                   ENDCS
007300970110E   1C                   ENDDO
007400970110     C*
007500970110     C                   SETON                                        LR
007600970110     C*------------------------------------------------------------------------*
007700970110     C* IMPVD1 - IMPOSTAZIONI PRIMA VIDEATA
007800970110     C*------------------------------------------------------------------------*
007900970110     C     IMPVD1        BEGSR
008000970110     C*
008100970110     C* REIMPOSTA FORMATO VIDEO E INDICATORI
008200970110     C                   EXSR      SETVD1
008300970110     C                   MOVEL     '1'           TIPVID                         *GESTIONE VIDEO
008400970110     C*
008500970110     C                   ENDSR
008600970110     C*------------------------------------------------------------------------*
008700970110     C* SETVD1 - IMPOSTAZIONI INDICATORI PRIMA VIDEATA
008800970110     C*------------------------------------------------------------------------*
008900970110     C     SETVD1        BEGSR
009000970110     C*
009100970110     C                   SETOFF                                       202122     ERRORI
009200970417     C                   SETOFF                                       232425
009300980603     C                   SETOFF                                       2627
009400970110     C                   SETOFF                                       90         GENERICO DI ERRORE
009500970110     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
009600970110     C*
009700970110     C                   ENDSR
009800970110     C*------------------------------------------------------------------------*
009900970110     C* GESVD1 - GESTIONE PRIMA VIDEATA
010000970110     C*------------------------------------------------------------------------*
010100970110     C     GESVD1        BEGSR
010200970110     C*
010300970110     C* EMISSIONE VIDEATA
010400970110IF  1C     $TESVD        IFEQ      '1'
010500970110     C                   WRITE     TE42D00                                      *TESTATA
010600970110     C                   MOVEL     '0'           $TESVD
010700970110E   1C                   ENDIF
010800970110     C                   EXFMT     TE42D01
010900970110     C                   EXSR      SETVD1
011000970110     C* F03-FINE
011100970110IF  1C     *INKC         IFEQ      *ON
011200970110     C                   EXSR      F03GES
011300970110     C                   GOTO      FINVD1
011400970110E   1C                   ENDIF
011500970110     C* F12-RITORNO
011600970110IF  1C     *INKL         IFEQ      *ON
011700970110     C                   EXSR      F12GES
011800970110     C                   GOTO      FINVD1
011900970110E   1C                   ENDIF
012000970110     C* ENTER-CONTROLLA VIDEO
012100970110     C                   EXSR      CTRVD1
012200970110IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
012300970110     C                   GOTO      FINVD1
012400970110E   1C                   ENDIF
012500970110     C* F06-CONFERMA
012600970110IF  1C     *INKF         IFEQ      *ON
012700970110     C                   EXSR      F06GES
012800970110     C                   GOTO      FINVD1
012900970110E   1C                   ENDIF
013000970110     C*
013100970110     C     FINVD1        ENDSR
013200970110     C*------------------------------------------------------------------------*
013300970110     C* CTRVD1 - CONTROLLA PRIMA VIDEATA
013400970110     C*------------------------------------------------------------------------*
013500970110     C     CTRVD1        BEGSR
013600970110     C*--------------------
013700970110     C* TARIFFA O OFFERTA
013800970110     C*--------------------
013900970110IF  1C     V1CCTO        IFEQ      'O'
014000970110     C     V1CCTO        OREQ      'T'
014100970110X   1C                   ELSE
014200970110     C                   SETON                                        2490
014300970110     C                   MOVEL     MSG(1)        $MSG
014400970110     C                   GOTO      FINCT1
014500970110E   1C                   ENDIF
014600970110     C*--------------------
014700970110     C* CLIENTE O VISITA
014800970110     C*--------------------
014900970110IF  1C     V1CCTO        IFEQ      'T'                                          T A R I F F A
015000970110     C                   Z-ADD     V1CKSC        KTMKSC
015100970110     C                   Z-ADD     V1CCTR        KTMCTR
015200970110     C                   Z-ADD     V1CPRG        KTMPRG
015300970110     C     KEYTAM        CHAIN     TNTAM01L                           99
015400970110IF  2C     *IN99         IFEQ      *ON                                          *INESISTENTE
015500970110     C                   SETON                                        202122
015600970110     C                   SETON                                        90
015700970110     C                   MOVEL     MSG(2)        $MSG
015800970110     C                   GOTO      FINCT1
015900011016     C                   ELSE
016000011016     C                   MOVEL     TAMFLO        DSTA01
016100011016     C                   MOVEL     §TADIV        V1CDIV
016200970110E   2C                   ENDIF
016300020925     C*
016400020314     C                   MOVEL     V1CKSC        DEPCART           5 0
016500020314     C                   IF        DEPCART = 88888
016600020314     C                   ELSE
016700020925     C                   MOVEL     V1CKSC        DEPFIL            3
016800020314     C     DEPFIL        LOOKUP    L1                                     55
016900020314     C                   IF        *IN55 = *OFF
017000020314     C                   SETON                                        202122
017100020314     C                   SETON                                        90
017200020314     C                   MOVEL     MSG(7)        $MSG
017300020314     C                   GOTO      FINCT1
017400020314     C                   ENDIF
017500020314     C                   ENDIF
017600020314     C*
017700970110X   1C                   ELSE                                                   O F F E R T A
017800970110     C                   Z-ADD     V1CKSC        KFMKSC
017900970110     C                   Z-ADD     V1CCTR        KFMCTR
018000970110     C                   Z-ADD     V1CPRG        KFMPRG
018100970110     C     KEYOFM        CHAIN     TNOFM01L                           99
018200970110IF  2C     *IN99         IFEQ      *ON                                          *INESISTENTE
018300970110     C                   SETON                                        202122
018400970110     C                   SETON                                        90
018500970110     C                   MOVEL     MSG(2)        $MSG
018600970110     C                   GOTO      FINCT1
018700011026     C                   ELSE
018800011026     C                   MOVEL     TAMFLO        DSTA01
018900011026     C                   MOVEL     §TADIV        V1CDIV
019000970110E   2C                   ENDIF
019100020925     C*
019200020314     C     V1CKSC        CHAIN     TNVIS05L
019300020314     C                   IF        %found(TNVIS05L)
019400020314     C                   IF        VISKSC > *zeros
019500020314     C                   MOVEL     VISKSC        DEPFIL
019600020314     C                   ELSE
019700020314     C                   MOVEL     VISCMM        DEPFIL
019800020314     C                   ENDIF
019900020314     C     DEPFIL        LOOKUP    L1                                     55
020000020314     C                   IF        *IN55 = *OFF
020100020314     C                   SETON                                        202122
020200020314     C                   SETON                                        90
020300020314     C                   MOVEL     MSG(7)        $MSG
020400020314     C                   GOTO      FINCT1
020500020314     C                   ENDIF
020600020314     C                   ELSE
020700020314     C                   SETON                                        202122
020800020314     C                   SETON                                        90
020900020314     C                   MOVEL     MSG(7)        $MSG
021000020314     C                   GOTO      FINCT1
021100020314     C                   ENDIF
021200020925     C*
021300020925E   1C                   ENDIF
021400970110     C*--------------------
021500970110     C* LINEA DI PARTENZA
021600970110     C*--------------------
021700981014     C                   MOVEL     *BLANKS       V1DLNP
021800981014IF  1C     V1CLNP        IFEQ      999
021900981014     C     V1CLNP        OREQ      888
022000981014     C     V1CLNP        OREQ      889
022100981014X   1C                   ELSE
022200970110     C                   MOVEL     V1CLNP        WFIL
022300970110     C                   MOVEL     *BLANKS       WDFIL
022400970110     C                   MOVEL     '0'           WERR
022500970110     C                   EXSR      CHKFIL
022600970110     C                   MOVEL     WFIL          V1CLNP
022700970110     C                   MOVEL     WDFIL         V1DLNP
022800970110IF  2C     WERR          IFEQ      '1'
022900970110     C                   SETON                                        2390
023000970110     C                   MOVEL     MSG(4)        $MSG
023100970110     C                   GOTO      FINCT1
023200970110E   2C                   ENDIF
023300981014E   1C                   ENDIF
023400970418     C*--------------------
023500970418     C* TARIFFA ITALIA / ESTERO
023600970418     C*--------------------
023700970418IF  1C     V1CSIE        IFEQ      'I'
023800970418     C     V1CSIE        OREQ      'E'
023900970418X   1C                   ELSE
024000970418     C                   SETON                                        2690
024100980603     C                   MOVEL     MSG(5)        $MSG
024200970418     C                   GOTO      FINCT1
024300970418E   1C                   ENDIF
024400980603     C*--------------------
024500980603     C* DATI SEDE O FILIALE
024600980603     C*--------------------
024700980603IF  1C     V1CDSF        IFEQ      'S'
024800980603     C     V1CDSF        OREQ      'F'
024900980603X   1C                   ELSE
025000980603     C                   SETON                                        2790
025100980603     C                   MOVEL     MSG(5)        $MSG
025200980603     C                   GOTO      FINCT1
025300980603E   1C                   ENDIF
025400970417     C*--------------------
025500970417     C* STAMPA CODICI TASSAZIONE
025600970417     C*--------------------
025700970417IF  1C     V1CSCT        IFEQ      ' '
025800970417     C     V1CSCT        OREQ      'T'
025900970417     C     V1CSCT        OREQ      'R'
026000970417X   1C                   ELSE
026100970417     C                   SETON                                        2590
026200970417     C                   MOVEL     MSG(5)        $MSG
026300970417     C                   GOTO      FINCT1
026400970417E   1C                   ENDIF
026500970110     C*
026600981014IF  1C     V1CLNP        IFEQ      888
026700981014     C     V1CLNP        OREQ      889
026800981014IF  2C     V1CSCT        IFEQ      ' '
026900981014     C                   SETON                                        232590
027000981014     C                   MOVEL     MSG(6)        $MSG
027100981014     C                   GOTO      FINCT1
027200981014E   2C                   ENDIF
027300981014E   1C                   ENDIF
027400981014     C*
027500970110     C     FINCT1        ENDSR
027600970110     C*------------------------------------------------------------------------*
027700970110     C* CHKFIL - CONTROLLA LA FILIALE
027800970110     C*------------------------------------------------------------------------*
027900970110     C     CHKFIL        BEGSR
028000970110     C*
028100970110     C     *LIKE         DEFINE    V1CLNP        WFIL
028200970110     C     *LIKE         DEFINE    V1DLNP        WDFIL
028300970110     C*
028400970110     C* CONTROLLA ESISTENZA CODICE IN ARCHIVIO
028500970110     C                   MOVEL     WFIL          KORFIL
028600970110     C     KEYORG        CHAIN     AZORG01L                           98
028700970110IF  1C     *IN98         IFEQ      *ON                                          *NON TROVATA
028800970110     C     ORGFVA        ORNE      *BLANKS                                      *NO ANNULLATI
028900970110     C                   MOVEL     '1'           WERR
029000970110X   1C                   ELSE
029100970110     C                   MOVEL     ORGDES        WDFIL                          *DESCRIZIONE
029200970110E   1C                   ENDIF
029300970110     C*
029400970110     C                   ENDSR
029500970110     C*------------------------------------------------------------------------*
029600970110     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
029700970110     C*------------------------------------------------------------------------*
029800970110     C     F03GES        BEGSR
029900970110     C*
030000970110     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
030100970110     C                   MOVEL     '1'           D00F03                         *F03 IN PROCEDURA
030200970110     C*
030300970110     C                   ENDSR
030400970110     C*------------------------------------------------------------------------*
030500970110     C* F12GES - GESTIONE TASTO PRIMO SFILE F12 ==> RITORNO
030600970110     C*------------------------------------------------------------------------*
030700970110     C     F12GES        BEGSR
030800970110     C*
030900970110     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
031000970110     C                   MOVEL     '1'           D00F12                         *F12 IN PROCEDURA
031100970110     C*
031200970110     C                   ENDSR
031300970110     C*------------------------------------------------------------------------*
031400970110     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
031500970110     C*------------------------------------------------------------------------*
031600970110     C     F06GES        BEGSR
031700970110     C*
031800970110     C                   MOVEL     'F06'         D00OP1                         *LIVELLO PROGRAMMA
031900970110     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
032000970110     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
032100970110     C                   MOVEL     '0'           D00ERR                         *ERRORE
032200970110     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO ERRORE
032300970110     C*
032400970110     C                   MOVEL     V1CCTO        D00CTO                         *TARIFFA O OFFERTA
032500970110     C                   Z-ADD     V1CKSC        D00KSC                         *CLIENTE O VISITA
032600970110     C                   Z-ADD     V1CCTR        D00CTR                         *TARIFFA
032700970110     C                   Z-ADD     V1CPRG        D00PRG                         *PROGRESSIVO
032800970110     C                   Z-ADD     V1CLNP        D00LNP                         *LINEA DI PARTENZA
032900970110     C                   MOVEL     TAMTSP        D00TSP                         *TIPO SERVIZIO
033000970110     C                   MOVEL     TAMFIE        D00FIE                         *FLAG ITALIA/ESTERO
033100970418     C                   MOVEL     V1CSIE        D00SIE                         *TARIFFA ITA/EST
033200980603     C                   MOVEL     V1CDSF        D00SSF                         *DATI SEDE O FILIALE
033300970418     C                   MOVEL     V1CSCT        D00SCT                         *REPPRESENTAZIONE CT
033400011003     C                   MOVEL     V1CDIV        D00DIV                         *DIVISA TARIFFE/OFFE
033500970110     C*
033600060808     C                   MOVEL     'TE3N'        KCOAZ
033700970110     C                   MOVEL     DSTE00        KPJBU
033701060830     C                   MOVE      'TNTE43R   '  KPJBU
033800970110     C                   CALL      'BCH10'
033900970110     C                   PARM                    KPJBA
034000970421     C*
034100970110     C                   ENDSR
034200970110     C*------------------------------------------------------------------------*
034300970110     C* CARTAB - CARICA LE TABELLE OCCORRENTI
034400970110     C*------------------------------------------------------------------------*
034500970110     C     CARTAB        BEGSR
034600970110     C*
034700020925     C*
034800970110     C                   ENDSR
034900970110     C*------------------------------------------------------------------------*
035000970110     C* IMPVD0 - IMPOSTAZIONE TESTATA
035100970110     C*------------------------------------------------------------------------*
035200970110     C     IMPVD0        BEGSR
035300970110     C*
035400970110     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
035500970110     C*
035600970110     C                   ENDSR
035700020925     C*--------------------------------------------------------------------------------------------*
035800020925     C* REPERISCE DATI UTENTE
035900020925     C*--------------------------------------------------------------------------------------------*
036000020925     C     REPDATIUTE    BEGSR
036100020925     C*
036200020925     C* REimposta le variabili di lavoro
036300020925     C                   CLEAR                   TIBS34DS
036400020925     C                   CLEAR                   AZUTEDS
036500020925     C                   CLEAR                   DDATIUTE
036600020925     C                   CLEAR                   WABI              2
036700020925     C                   CLEAR                   DLAT
036800020925     C*
036900020925     C* Reperisce i dati del profilo in ingresso
037000020925     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
037100020925     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
037200020925     C                   IN(E)     *DTAARA
037300020925     C                   IF        %Error                                       *NO errori
037400020925     C                   EVAL      I34TLA = 'L'
037500020925     C                   CALL      'TIBS34R'
037600020925     C                   PARM                    TIBS34DS
037700020925     C                   IN        *DTAARA
037800020925     C                   ENDIF
037900020925     C*
038000020925     C* Se ci sono degli errori sull'abilitazione utente, esce
038100020925     C                   SELECT
038200020925     C                   WHEN      DUTERR = 'E'
038300020925     C                   EVAL      WERR  = '1'
038400020925     C                   EVAL      $FINE = '1'
038500020925     C*
038600020925     C* NON siamo in sede (filiale remota o filiale locale)
038700020925     C* --> se 1° livello, abilitazioni al terminal
038800020925     C*     se 2° livello, abilitazioni al punto operativo
038900020925     C                   WHEN      UTEAUT = *blanks
039000020925     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
039100020925     C                   EVAL      WABI   = 'TP'
039200020925     C                   ENDIF
039300020925     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
039400020925     C                   EVAL      WABI   = 'PO'
039500020925     C                   ENDIF
039600020925     C*
039700020925     C* siamo in sede
039800020925     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
039900020925     C*     carica le abilitazioni proprie del profilo
040000020925     C                   OTHER
040100020925     C                   MOVEL     UTEFAF        DUTE01
040200060913     C                   IF        §UTEgtc <> *blanks                           *esiste specifica
040300060913     C                   EVAL      WABI = §UTEgtc
040400020925     C                   ELSE                                                   *NON esiste specific
040500020925     C                   EVAL      WABI = UTEAUT
040600020925     C                   ENDIF
040700020925     C                   ENDSL
040800020925     C*
040900020925     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
041000020925     C                   CLEAR                   TIBS02DS
041100020925     C                   MOVEL     'C'           T02MOD
041200020925     C                   MOVEL     KNSIF         T02SIF
041300020925     C                   MOVEL     'LAT'         T02COD
041400020925     C                   MOVEL(P)  WABI          T02KE1
041500020925     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
041600020925     C                   CALL      'TIBS02R'
041700020925     C                   PARM                    KPJBA
041800020925     C                   PARM                    TIBS02DS
041900020925     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
042000020925     C                   IF        T02ERR = *blanks
042100020925     C                   MOVEL     T02UNI        DLAT
042200020925     C                   ENDIF
042300020925     C                   IF        T02ERR <> *blanks OR
042400020925     C                             §LATABI = 'S'
042500020925     C                   EVAL      WERR  = '1'
042600020925     C                   EVAL      $FINE = '1'
042700020925     C                   ENDIF
042800020925     C*
042900020925     C* Reperisce i P.O. gestibili dall'utente
043000020925     C                   IF        WERR <> '1'                                  *NO errori
043100020925     C                   CLEAR                   TRUL31DS
043200020925     C                   EVAL      I31ABI   = WABI
043300020925     C                   EVAL      I31CDI   = DUTDIS
043400020925     C                   EVAL      I31CAR   = DUTARE
043500020925     C                   EVAL      I31CPO   = DUTPOU
043600020925     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
043700020925     C                   CALL      'TRUL31R'
043800020925     C                   PARM                    KPJBA
043900020925     C                   PARM                    TRUL31DS
044000020925     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
044100020925if  2C                   IF        O31POG > *zeros
044200020925     C                   MOVEA     O31POG        L1
044300020925x   2C                   ELSE
044400020925     C                   EVAL      WERR  = '1'
044500020925     C                   EVAL      $FINE = '1'
044600020925e   2C                   ENDIF
044700020925e   1C                   ENDIF
044800020925     C*
044900020925     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
045000020925     C                   MOVEL(P)  RSUT          DSFIRS           20
045100020925     C*
045200020925     C                   ENDSR
045300970110     C*------------------------------------------------------------------------*
045400970110     C* *INZSR - OPERAZIONI INIZIALI
045500970110     C*------------------------------------------------------------------------*
045600970110     C     *INZSR        BEGSR
045700970110     C*--------------------
045800970110     C* RICEVIMENTO PARAMETRI
045900970110     C*--------------------
046000970110     C     *ENTRY        PLIST
046100970110     C                   PARM                    KPJBA
046101060808     c                   movel     'S'           v1cdsf            1
046400970110     C*--------------------
046500970110     C* VARIABILI RIFERITE AL DATABASE
046600970110     C*--------------------
046700970110     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
046800970110     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
046900970110     C     *LIKE         DEFINE    TAMCTR        KTMCTR
047000970110     C     *LIKE         DEFINE    TAMPRG        KTMPRG
047100970110     C     *LIKE         DEFINE    TAMKSC        KFMKSC                         *TNOFM00F
047200970110     C     *LIKE         DEFINE    TAMCTR        KFMCTR
047300970110     C     *LIKE         DEFINE    TAMPRG        KFMPRG
047400020924     C     *LIKE         DEFINE    KPJBU         DEPKPJBU                       *depositi
047500970110     C*--------------------
047600970110     C* CHIAVI DI LETTURA
047700970110     C*--------------------
047800970110     C* LETTURA AZORG01L - COMPLETA
047900970110     C     KEYORG        KLIST
048000970110     C                   KFLD                    KORFIL                         *FILIALE
048100970110     C* LETTURA TNTAM01L - COMPLETA
048200970110     C     KEYTAM        KLIST
048300970110     C                   KFLD                    KTMKSC                         *CLIENTE
048400970110     C                   KFLD                    KTMCTR                         *TARIFFA
048500970110     C                   KFLD                    KTMPRG                         *PROGRESSIVO
048600970110     C* LETTURA TNOFM01L - COMPLETA
048700970110     C     KEYOFM        KLIST
048800970110     C                   KFLD                    KFMKSC                         *CLIENTE
048900970110     C                   KFLD                    KFMCTR                         *TARIFFA
049000970110     C                   KFLD                    KFMPRG                         *PROGRESSIVO
049100970110     C*--------------------
049200970110     C* VARIABILI NON RIFERITE AL DATABASE
049300970110     C*--------------------
049400970110     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
049500970110     C                   MOVEL     '0'           WERR              1            *ERRORE
049600970110     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
049700970110     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
049800970110     C*
049900970110     C* REPERISCE I DATI DELL'UTENTE
050000020923     C                   EXSR      REPDATIUTE
050100970110     C*
050200970110     C* CARICA LE TABELLE OCCORRENTI
050300970110     C                   EXSR      CARTAB
050400970110     C*
050500970110     C* IMPOSTAZIONI TESTATA
050600970110     C                   EXSR      IMPVD0
050700970110     C*
050800970110     C                   ENDSR
050900970110     O*------------------------------------------------------------------------*
051000970110** -MSG-
051100970110TNTE42R- "T" per stampare i dati tariffa, "O" per stampare quelli offerta     1
051200970110TNTE42R- Codice tariffa inesistente                                           2
051300970110TNTE42R- Codice offerta inesistente                                           3
051400970110TNTE42R- Linea di partenza errata o inesistente                               4
051500980603TNTE42R- Immettere uno dei valori possibili                                   5
051600981014TNTE42R- Per linea partenza 888 o 889 la rappresentazione è solo R o T        6
051700020314TNTE42R- Impossibile indicare filiale non in gestione                         7
051800970110TNTE42R- 8
051900970110TNTE42R- 9
052000970110TNTE42R- 10
