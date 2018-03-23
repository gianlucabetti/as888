000100000000     H*------------------------------------------------------------------------*
000200000000     H*                    - STATISTICA CLIENTI -           RICHIESTA CLIENTE  *
000300000000     H*------------------------------------------------------------------------*
000400000000     H* VALORIZZA NELLA DS DI PROCEDURA (DS60):
000500000000     H*         - LIVELLO PROCEDURA = 'VIS'
000600951005     H*         - LIVELLO PROGRAMMA = 'F06' --> SE RICHIESTA LA VISUALIZZAZIONE
000700000000     H*                               'F21' --> SE RICHIESTA LA STAMPA
000800000000     H*------------------------------------------------------------------------*
000900000000     H DECEDIT('0,') DATEDIT(*DMY.)
001000000000     F*------------------------------------------------------------------------*
001100000000     F* DATA BASE
001200000000     F*------------------------------------------------------------------------*
001300010920     FTISE60D   CF   E             WORKSTN INFDS(DEVDS1)
001400010920     F                                     SFILE(SE60DVM:MSGULT)
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800950718     D*------------------
001900950718     D* MESSAGGI, COMANDI, FUNZIONI
002000950718     D*------------------
002100000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002200000906     D DEC             S             35    DIM(5) CTDATA PERRCD(1)              *DECODIFICHE
002300000906     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002400000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002500950718     D*------------------
002600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002700950718     D*------------------
002800000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002900000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003000000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003100950718     D*------------------
003200000000     D* TABELLA DESCRIZIONI A VIDEO
003300950718     D*------------------
003400000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003500950718     D*------------------
003600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003700950718     D*------------------
003800000000     D KEYS            S              1    DIM(40)
003900950718     D*-------------------
004000950718     D* CAPOCONTO / SOTTOCONTO DI RITORNO "XCLIR"
004100950718     D*-------------------
004200050117      ** i codici clienti da passare nella DS sono diventati 28 da 30 che erano
004300050117      ** x permettere passaggio di altri parametri poichè la DS era già di 256
004400050117     D*KSC*****        S              7  0 DIM(30)
004500050117     D*KSA*****        S              4    DIM(30)                              *ALFANUMERICO
004600050117     D KSC             S              7  0 DIM(28)
004700050117     D KSA             S              4    DIM(28)                              *ALFANUMERICO
004800020913     D*------------
004900020913     D* FILIALI IN GESTIONE
005000020913     D*------------
005100020913     D $POG            s              3    DIM(250) INZ(*zeros)
005200000000     D*------------------------------------------------------------------------*
005300000000     D* INPUT
005400000000     D*------------------------------------------------------------------------*
005500000000     D*------------------
005600000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005700000000     D*------------------
005800000000     D DSFUN         E DS
005900000000     D  FUNRIG                 1     40
006000000000     D*------------------
006100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006200000000     D*------------------
006300000000     D DSMSG         E DS
006400000000     D*------------------
006500000000     D* DS PER GESTIONE TASTI FUNZIONALI
006600000000     D*------------------
006700000000     D DSKEYS        E DS
006800000000     D  KEYRIG                 1     40
006900000000     D*------------------
007000000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007100000000     D*------------------
007200000000     D                SDS
007300000000     D  USER                 254    263
007400000000     D*------------------
007500000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007600000000     D*------------------
007700000000     D DEVDS1          DS
007800000000     D  KEY                  369    369
007900000000     D  CURSOR               370    371
008000000807     D*------------------
008100000807     D* DS CONTROLLO DATA (8)
008200000807     D*------------------
008300000807     D WLBDA8          DS                  INZ
008400000807     D  G08DAT                 1      8  0
008500000807     D  G08INV                 9     16  0
008600000807     D  G08ERR                17     17
008700000807     D  G08TGI                18     22  0
008800000000     D*------------------
008900000000     D* ARCHITETTURA
009000000000     D*------------------
009100000000     D KPJBA         E DS
009200100512     D*------------------
009300100512     D* DS X CONTROLLO ABILITAZIONI
009400100512     D*------------------
009500100512     D TAA1DS        E DS                  EXTNAME(TNTAA1DS)
009600950718     D*------------------
009700950718     D* DS DI PROCEDURA
009800950718     D*------------------
009900010920     D DS60          E DS                  EXTNAME(TISE60DS)
010000950718     D*------------------
010100020117     D* DS "TISE66R"
010200950718     D*------------------
010300020117     D DSCL          E DS                  EXTNAME(TISE66DS) INZ
010400950719     D*------------------
010500950719     D* DS "XCLIR"
010600950719     D*------------------
010700950718     D DSXC          E DS                  EXTNAME(XCLIRDS)
010800000807     D*------------------
010900000807     D* DS "TIBS10R"
011000000807     D*------------------
011100000807     D BS10DS        E DS                  EXTNAME(TIBS10DS)
011200950724     D*------------------
011300950724     D* DS PER PASSAGGIO CAMPI PACKED
011400950724     D*------------------
011500950724     D                 DS
011600950724     D  DSKSN                  1      4P 0
011700950724     D  DSKSA                  1      4
011800000810     D*------------------
011900000810     D* DS REPERIMENTO DATI UTENTE
012000000810     D*-------------------
012100000810     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
012200000810     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
012300000810     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
012400000810     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
012500000810     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
012600020905     D*------------
012700020905     D* DS REPERIMENTO DATI UTENTE
012800020905     D*------------
012900020913     D TIBS02DS      E DS                                                       *Ricerca tabella
013000020905     D TIBS34DS      E DS                                                       *Profili utente
013100020905     D DDATIUTE      E DS                                                       *Dati utente
013200020905     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
013300020913     D*------------------
013400020913     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
013500020913     D*-------------------
013600020913     D DUTE01        e ds                  INZ                                  *Abilitazioni utenti
013700020913     D*------------------
013800020913     D* DS FUNZIONI AZIENDALI
013900020913     D*-------------------
014000020913     D DLAT          e ds                  INZ                                  *Funzioni aziendali
014100020913     D*------------------
014200020913     D* DS CARICAMENTO P.O. GESTITI
014300020913     D*-------------------
014400020913     D TRUL31DS      e ds                  INZ
014500000000     C*------------------------------------------------------------------------*
014600950719     C* MAIN LINE
014700000000     C*------------------------------------------------------------------------*
014800000000     C*
014900000000     C* CICLO FINO A CHE:
015000000000DO  1C     $FINE         DOUEQ     '1'
015100000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
015200000000     C*
015300000000     C* CONTROLLA GESTIONE VIDEO
015400000000IF  2C     $GES          IFEQ      'V1'
015500000000     C                   EXSR      GESV1                                        *RICHIESTA CODICE
015600000000E   2C                   ENDIF
015700000000     C*
015800000000E   1C                   ENDDO
015900000000     C*
016000000000     C* SE PREMUTO F03 LO IMPOSTO NELLA DS DELLA PROCEDURA
016100000000     C     $F03          IFEQ      '1'
016200000000     C                   MOVEL     '1'           D60F03
016300000000     C                   ENDIF
016400000810     C*
016500000810     C* OPERAZIONI FINALI
016600000810     C                   EXSR      FINSR
016700000000     C* FINE PGM
016800950724     C                   MOVEL     DS60          KPJBU
016900000000     C                   SETON                                        LR
017000000000     C*------------------------------------------------------------------------*
017100950719     C* GESV1 - GESTIONE VIDEO RICHIESTA CODICE CLIENTE
017200000000     C*------------------------------------------------------------------------*
017300000000     C     GESV1         BEGSR
017400000000     C*
017500000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
017600000000     C                   EXSR      CARV1                                        *CARICA TESTA + FUNZ
017700000000E   1C                   ENDIF
017800000000     C*
017900000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
018000000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
018100000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
018200000000E   1C                   ENDIF
018300000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
018400000000     C*
018500010920     C                   EXFMT     SE60DV1                                      *RICHIESTA CODICE CL
018600000000     C*
018700000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
018800000000     C*
018900000000IF  1C     $ERR          IFEQ      '0'
019000000000SL  2C                   SELECT
019100000000     C     KEY           WHENEQ    ENTER
019200000000     C                   EXSR      ENTV1                                        *CONTROLLO VIDEO
019300951005     C     KEY           WHENEQ    F6
019400951005     C                   EXSR      F06GES                                       *GEST F06-VISUALIZZA
019500000000     C     KEY           WHENEQ    F21
019600000000     C                   EXSR      F21GES                                       *GEST F21-STAMPA
019700000000     C     KEY           WHENEQ    F22
019800000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
019900000000     C                   OTHER
020000000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
020100000000E   2C                   ENDSL
020200000000E   1C                   ENDIF
020300000000     C*
020400000000     C                   ENDSR
020500000000     C*------------------------------------------------------------------------*
020600950719     C* CARV1 - CARICA VIDEO RICHIESTA CODICE CLIENTE
020700000000     C*------------------------------------------------------------------------*
020800000000     C     CARV1         BEGSR
020900000000     C*
021000000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
021100000000     C*
021200000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
021300000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
021400000000     C*
021500010920     C                   WRITE     SE60DVT                                      *TESTATA
021600010920     C                   WRITE     SE60DVF                                      *FUNZIONI
021700000000     C*
021800000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
021900000000     C*
022000000000     C                   ENDSR
022100000000     C*------------------------------------------------------------------------*
022200950719     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1
022300000000     C*------------------------------------------------------------------------*
022400000000     C     ENTV1         BEGSR
022500000000     C*
022600000000     C* CONTROLLA I CAMPI IMMESSI
022700000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
022800000000     C*
022900000000     C                   ENDSR
023000000000     C*------------------------------------------------------------------------*
023100951005     C* F06GES - GEST. TASTO FUNS. F06 ==> VISUALIZZAZIONE STATISTICHE CLIENTE
023200000000     C*------------------------------------------------------------------------*
023300951005     C     F06GES        BEGSR
023400000000     C*
023500000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
023600000000IF  1C     $ERR          IFEQ      '0'                                          *SE NO ERRORI
023700000000     C*
023800000000     C* LANCIO PROGRAMMA  RICHIESTA PARAMETRI
023900000000     C                   MOVEL     'VIS'         WOP0                           *LIVELLO PROCEDURA
024000951005     C                   MOVEL     'F06'         WOP1                           *LIVELLO PGM
024100010920     C                   MOVEL     'TISE61R'     WPGM                           *PGM RICH PARAMETRI
024200950724     C*
024300950724     C* COMPATTA IL CODICE CLIENTE SELEZIONATO PER PASSARLO NELLA DS
024400950724     C                   MOVEL     V1CKSC        KSCNUM            7 0
024500950724     C                   MOVEL     KSCNUM        DSKSN                          *SCH NUM --> DS  NUM
024600950724     C                   MOVE      DSKSA         KSA(1)                         *DS  ALF --> SCH ALF
024700950724     C                   MOVEA     KSA           WKSC                           SCH NUM PACK->CAMPON
024800000000     C*
024900000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
025000000000     C*
025100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
025200000000E   1C                   ENDIF
025300000000     C*
025400000000     C                   ENDSR
025500000000     C*------------------------------------------------------------------------*
025600950719     C* F21GES - GEST. TASTO FUNZ. F21 ==> STAMPA STATISTICHE CLIENTE
025700000000     C*------------------------------------------------------------------------*
025800000000     C     F21GES        BEGSR
025900000000     C*
026000000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
026100000000IF  1C     $ERR          IFEQ      '0'                                          *SE NO ERRORI
026200000000     C*
026300000000     C* LANCIO PROGRAMMA  RICHIESTA PARAMETRI
026400000000     C                   MOVEL     'VIS'         WOP0                           *LIVELLO PROCEDURA
026500000000     C                   MOVEL     'F21'         WOP1                           *LIVELLO PGM
026600010920     C                   MOVEL     'TISE61R'     WPGM              8            *PGM RICH PARAMETRI
026700950725     C                   MOVEL     V1CKSC        KSCNUM            7 0
026800950725     C                   MOVEL     KSCNUM        DSKSN                          *SCH NUM --> DS  NUM
026900950725     C                   MOVE      DSKSA         KSA(1)                         *DS  ALF --> SCH ALF
027000950725     C                   MOVEA     KSA           WKSC                           SCH NUM PACK->CAMPON
027100000000     C*
027200000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
027300000000     C*
027400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
027500000000E   1C                   ENDIF
027600000000     C*
027700000000     C                   ENDSR
027800000000     C*------------------------------------------------------------------------*
027900950719     C* F22GES -GEST.TASTO FUNZ. F22 ==> GESTIONE STAMPE PER EDP
028000000000     C*------------------------------------------------------------------------*
028100000000     C     F22GES        BEGSR
028200000000     C*
028300000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
028400000000     C                   Z-ADD     80            LENGH            15 5
028500000000     C                   CALL      'QCAEXEC'
028600000000     C                   PARM                    CMD(1)
028700000000     C                   PARM                    LENGH
028800000000     C*
028900000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
029000000000     C*
029100000000     C                   ENDSR
029200000000     C*------------------------------------------------------------------------*
029300950719     C* LANPGM - LANCIA PROGRAMMA RICHIESTA PARAMETRI
029400000000     C*------------------------------------------------------------------------*
029500000000     C     LANPGM        BEGSR
029600950725     C*
029700000811     C*---               CLEAR                   DS60                           *PULISCE DS ESTERNA
029800950725     C*
029900000000     C*--------------------
030000000000     C* IMPOSTO DS ESTERNA PROCEDURA
030100000000     C*--------------------
030200000000     C*
030300000000     C* PARAMETRI INFORMATIVI
030400000000     C                   MOVEL     WOP0          D60OP0                         *LIVELLO PROCEDURA
030500000000     C                   MOVEL     WOP1          D60OP1                         *LIVELLO PGM
030600000000     C                   MOVEL     '0'           D60F03                         PREMUTO F03
030700000000     C                   MOVEL     '0'           D60ERR                         '1'=ERRORE
030800000000     C                   MOVEL     *BLANKS       D60MSG                         MESSAGGIO
030900000000     C                   MOVEL     WKSC          D60KSC                         *CODICE CLIENTE
031000000000     C*
031100000000     C* LANCIO PROGRAMMA
031200950724     C                   MOVEL     DS60          KPJBU
031300000000     C                   CALL      WPGM
031400000000     C                   PARM                    KPJBA
031500000000     C*
031600950725     C                   MOVEL     KPJBU         DS60
031700950724     C*
031800000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
031900000000IF  1C     D60F03        IFEQ      '1'
032000000000     C                   MOVEL     '1'           $F03
032100000000     C                   MOVEL     '1'           $FINE
032200000000X   1C                   ELSE
032300000000     C*
032400000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
032500950724     C                   MOVEL(P)  D60MSG        $MSG
032600000000     C                   EXSR      CARMSG
032700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
032800000000E   1C                   ENDIF
032900000000     C*
033000000000     C                   ENDSR
033100000000     C*------------------------------------------------------------------------*
033200950719     C* DECODI - CONTROLLA CAMPI FORMATO VIDEO
033300000000     C*------------------------------------------------------------------------*
033400000000     C     DECODI        BEGSR
033500000000     C*
033600000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
033700000000     C                   SETOFF                                       50
033800000000     C*
033900000000     C* PULISCE IL SECONDO FORMATO VIDEO
034000000000     C                   EXSR      PULVID
034100000000     C*
034200000000     C*--------------------
034300000000     C* CODICE CLIENTE
034400000000     C*--------------------
034500000000     C*
034600000000     C* CODICE CLIENTE OBBLIGATORIO
034700000000IF  1C     V1CKSC        IFEQ      *BLANKS
034800000000     C     V1CKSC        OREQ      *ZEROS
034900000000     C                   MOVEL     '1'           $ERR
035000000000     C                   MOVEL     MSG(7)        $MSG
035100000000     C                   EXSR      CARMSG
035200000000     C                   SETON                                        50
035300000804E   1C                   ENDIF
035400000000     C*
035500000000     C* RICHIESTA RICERCA CON IL ?
035600000804IF  1C     $ERR          IFEQ      '0'                                          *ANCORA VALIDO
035700000000     C     '?'           SCAN      V1CKSC                                 01
035800000000IF  2C     *IN01         IFEQ      *ON
035900950718     C                   CLEAR                   DSXC
036000020916     C                   Z-ADD     DUTKCI        DXCCAP
036100021113     C                   MOVEL     WAbi          DXCAUT
036200000000     C                   CALL      'XCLIR'
036300950718     C                   PARM                    DSXC
036400000804     C                   MOVEA     DXCKSC        KSA                            *SCOMATTA CODICI
036500050117DO  3C**** 1             DO        30            J                 4 0
036600050117DO  3C     1             DO        28            J                 4 0
036700950724     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
036800950724     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
036900950724E   3C                   ENDDO
037000950718     C                   MOVEL     KSC(1)        V1CKSC
037100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
037200000906     C*---               MOVEL     '1'           $ERR
037300000804E   2C                   ENDIF
037400000804E   1C                   ENDIF
037500000804     C*
037600000804     C* CONTROLLA SE CLIENTE NUMERICO
037700000804IF  1C     $ERR          IFEQ      '0'                                          *ANCORA VALIDO
037800970220     C                   SETOFF                                       28
037900970220     C                   TESTN                   V1CKSC               28
038000000804IF  2C     *IN28         IFEQ      *ON                                          DA TESTN E'NUMERO
038100970220     C                   MOVE      V1CKSC        W001              1
038200000804IF  3C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
038300970220     C                   MOVEL     '1'           $ERR
038400970220     C                   MOVEL     MSG(8)        $MSG
038500970220     C                   EXSR      CARMSG
038600970220     C                   SETON                                        50
038700000804E   3C                   ENDIF
038800000804X   2C                   ELSE                                                   DA TESTN NON E'NUMER
038900970220     C                   MOVEL     '1'           $ERR
039000970220     C                   MOVEL     MSG(8)        $MSG
039100970220     C                   EXSR      CARMSG
039200970220     C                   SETON                                        50
039300000804E   2C                   ENDIF
039400000804E   1C                   ENDIF
039500970220     C*
039600000804     C* CONTROLLA SE CLIENTE VALIDO
039700000810IF  1C     $ERR          IFEQ      '0'
039800000810     C                   CLEAR                   BS69DS
039900000810     C                   CLEAR                   ACODS
040000000810     C                   MOVEL     KNSIF         I69SIF
040100000810     C                   MOVEL     V1CKSC        I69KAC
040200000810     C                   CALL      'TIBS69R'
040300000810     C                   PARM                    BS69DS
040400000810     C                   PARM                    ACODS
040500000810     C                   PARM                    INDDS
040600000810     C                   PARM                    CLPDS
040700000810     C                   PARM                    CLSDS
040800000810IF  2C     O69ERR        IFNE      '1'
040900000810X   2C                   ELSE
041000000810     C                   MOVEL     '1'           $ERR
041100000810     C                   MOVEL     MSG(8)        $MSG
041200000810     C                   EXSR      CARMSG
041300000810     C                   SETON                                        50
041400000810E   2C                   ENDIF
041500000810E   1C                   ENDIF
041600000804     C*
041700000804     C* CONTROLLA SE CLIENTE GESTIBILE:
041800020910IF  1C     $ERR          IFEQ      '0'
041900100512      * CONTROLLA SE PUÒ ESSERE VISTO OPPURE NO CON ABILITAZIONI ANCHE X FUORI AREA
042000100512      * MA PER AGENTE UNIFICANTE:
042100100512     C                   clear                   TAA1DS
042200101011     C                   EVAL        ITAA1CAUT = '§UTEIST'
042300100512     C                   move      V1CKSC        ITAA1KSC
042400100512     C                   EVAL      KPJBU = TAA1DS
042500100512     C                   CALL      'TNTAA1C'
042600100512     C                   PARM                    KPJBA
042700100512     C                   EVAL              TAA1DS = KPJBU
042800100512      *
042900100512      *  SE NON ABILITATO PROCEDO CON IL VECCHIO CONTROLLO SULLA FILIALE GESTITA IN LISTA
043000100512     C                   IF        OTAA1FABI <> 'S'
043100100512     C                   MOVEL     '1'           $ERR
043200100512     C                   MOVEL     MSG(9)        $MSG
043300100512     C                   EXSR      CARMSG
043400100512     C                   SETON                                        50
043500100512     c                   end
043600100512      *
043700100512     C*********          SETOFF                                           28
043800100512     c*********          MOVEL     V1CKSC        WAFIL             3
043900100512     C**** WAFIL         LOOKUP    $POG                                   28
044000100512IF  2C**** *IN28         IFEQ      *OFF                                         *CLIENTE NON DELL'AR
044100100512     C*********          MOVEL     '1'           $ERR
044200100512     C*********          MOVEL     MSG(9)        $MSG
044300100512     C*********          EXSR      CARMSG
044400100512     C*********          SETON                                        50
044500100512E   2C*********          ENDIF
044600020910E   1C                   ENDIF
044700000807     C*
044800100512     C*
044900000906     C* SE SCELTA CLIENTE UNIFICANTE, CONTROLLA E NEL CASO REPERISCE IL CLIENTE PADRE
045000000807IF  1C     $ERR          IFEQ      '0'
045100000807IF  2C     D60SCE        IFEQ      '3'                                          *SCELTA UNIFICANTE
045200000807     C                   CLEAR                   BS10DS
045300000807     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
045400000807     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
045500000816     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
045600000817     C                   MOVE      V1CKSC        D10COD
045700000807     C                   CALL      'TIBS10R'
045800000807     C                   PARM                    BS10DS
045900000906IF  3C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
046000000906     C     D10COD        ANDNE     D10COP                                       *MA NON SELEZIONATO
046100000906     c                   MOVE      V1CKSC        V1CKSE                         *C.SELEZIONATO
046200000906     C                   MOVEL     DEC(1)        V1DKSE                         *MESSAGGIO
046300000906     c                   MOVE      D10COP        V1CKSC                         *C.DEDOTTO
046400000906     C                   MOVEL     DEC(2)        V1DKSC                         *MESSAGGIO
046500000906E   3C                   ENDIF
046600000807E   2C                   ENDIF
046700000807E   1C                   ENDIF
046800000807     C*
046900000804     C* RIEMPE I CAMPI DAT  DEL CLIENTE
047000000804IF  1C     $ERR          IFEQ      '0'                                          *ANCORA VALIDO
047100000000     C                   EXSR      RIEMPI                                       *CARICA I DATI
047200000000E   1C                   ENDIF
047300000000     C*
047400000000     C                   ENDSR
047500000000     C*------------------------------------------------------------------------*
047600950719     C* PULVID - PULISCE LA PARTE DI VIDEO CON I DATI DEL CLIENTE
047700000000     C*------------------------------------------------------------------------*
047800000000     C     PULVID        BEGSR
047900000000     C*
048000000000     C* CAMPI DATI
048100000000     C                   MOVEL     *BLANKS       V1CRAG
048200000000     C                   MOVEL     *BLANKS       V1CVIA
048300000000     C                   MOVEL     *BLANKS       V1CCIT
048400000000     C                   MOVEL     *BLANKS       V1CCAP
048500000000     C                   MOVEL     *BLANKS       V1CPRV
048600000000     C                   MOVEL     *BLANKS       V1CSTA
048700000000     C                   MOVEL     *BLANKS       V1CTEL
048800000000     C                   MOVEL     *BLANKS       V1CCLV
048900951006     C                   Z-ADD     *ZEROS        V1CITC
049000000000     C                   Z-ADD     *ZEROS        V1CDPS
049100000000     C                   Z-ADD     *ZEROS        V1CAGE
049200000000     C                   MOVEL     *BLANKS       V1CCDP
049300000000     C                   MOVEL     *BLANKS       V1CSOL
049400000000     C*
049500000000     C* CAMPI DECODIFICA
049600000000     C                   MOVEL     *BLANKS       V1DPRV
049700951006     C                   MOVEL     *BLANKS       V1DITC
049800000000     C                   MOVEL     *BLANKS       V1DAGE
049900000000     C                   MOVEL     *BLANKS       V1DCDP
050000000906     C                   Z-ADD     *ZEROS        V1CKSE
050100000906     C                   MOVEL     *BLANKS       V1DKSE
050200000906     C                   MOVEL     *BLANKS       V1DKSC
050300000000     C*
050400000000     C                   ENDSR
050500000000     C*------------------------------------------------------------------------*
050600950719     C* RIEMPI - RIEMPIMENTO VIDEO CON I DATI DEL CLIENTE
050700000000     C*------------------------------------------------------------------------*
050800000000     C     RIEMPI        BEGSR
050900000000     C*
051000020117     C* CHIAMA IL PGM 'TISE66R' CHE REPERISCE I DATI DEL CLIENTE
051100000000     C                   CLEAR                   DSCL                           *PULISCE DS PASSAGGI
051200020902     C                   Z-ADD     1             DCLKUT
051300000000     C                   MOVEL     V1CKSC        DCLKSC
051400020117     C                   CALL      'TISE66R'
051500000000     C                   PARM                    DSCL
051600000000     C*--------------------
051700000000     C* CAMPI
051800000000     C*--------------------
051900000000     C                   MOVEL     DCLRAG        V1CRAG
052000000000     C                   MOVEL     DCLVIA        V1CVIA
052100000000     C                   MOVEL     DCLCIT        V1CCIT
052200000000     C                   MOVEL     DCLCAP        V1CCAP
052300000000     C                   MOVEL     DCLPRV        V1CPRV
052400000000     C                   MOVEL     DCLSTA        V1CSTA
052500000000     C                   MOVEL     DCLTEL        V1CTEL
052600000000     C                   MOVEL     DCLCLV        V1CCLV
052700951006     C                   Z-ADD     DCLITC        V1CITC
052800000000     C                   Z-ADD     DCLDPS        V1CDPS
052900000000     C                   Z-ADD     DCLAGE        V1CAGE
053000000000     C                   MOVE      DCLCDP        V1CCDP
053100000000     C                   MOVEL     DCLSOL        V1CSOL
053200000000     C*--------------------
053300000000     C* DECODIFICHE
053400000000     C*--------------------
053500000000     C                   MOVEL     DCLDPR        V1DPRV
053600951006     C                   MOVEL     DCLDIT        V1DITC
053700000000     C                   MOVEL     DCLDAG        V1DAGE
053800000000     C                   MOVEL     DCLDCD        V1DCDP
053900000000     C*
054000000000     C                   ENDSR
054100000810     C*------------------------------------------------------------------------*
054200000810     C* FINSR - OPERAZIONI FINALI
054300000810     C*------------------------------------------------------------------------*
054400000810     C     FINSR         BEGSR
054500000810     C*
054600000810     C* CHIUDE I FILE DEI PGM CHIAMATI
054700000810     C                   CLEAR                   BS69DS
054800000810     C                   CLEAR                   ACODS
054900000810     C                   CLEAR                   INDDS
055000000810     C                   CLEAR                   CLPDS
055100000810     C                   CLEAR                   CLSDS
055200000810     C                   MOVEL     'C'           I69TLA
055300000810     C                   MOVEL     KNSIF         I69SIF
055400000810     C                   CALL      'TIBS69R'
055500000810     C                   PARM                    BS69DS
055600000810     C                   PARM                    ACODS
055700000810     C                   PARM                    INDDS
055800000810     C                   PARM                    CLPDS
055900000810     C                   PARM                    CLSDS
056000000810     C*
056100000810     C                   ENDSR
056200000000     C*------------------------------------------------------------------------*
056300950719     C* *INZSR - ROUTINE INIZIALE
056400000000     C*------------------------------------------------------------------------*
056500000000     C     *INZSR        BEGSR
056600000000     C*
056700000000     C* RICEVIMENTO PARAMETRI
056800000000     C     *ENTRY        PLIST
056900000000     C                   PARM                    KPJBA
057000950726     C                   MOVEL     KPJBU         DS60
057100000000     C*
057200000000     C* DEFINIZIONI VARIABILI
057300000000     C     *LIKE         DEFINE    ACOKUT        KACKUT
057400000000     C     *LIKE         DEFINE    ACOKCC        KACKCC
057500000000     C     *LIKE         DEFINE    ACOKSC        KACKSC
057600020905     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
057700000000     C*
057800000000     C     *LIKE         DEFINE    D60OP0        WOP0
057900000000     C     *LIKE         DEFINE    D60OP1        WOP1
058000000000     C     *LIKE         DEFINE    D60KSC        WKSC
058100000807     C*
058200000807     C* IMNPOSTA DATA / ORA CORRENTE
058300000807     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
058400000807     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
058500000807     C                   Z-ADD     *ZEROS        G08INV
058600000807     C                   MOVEL     '0'           G08ERR
058700000807     C                   CALL      'XSRDA8'
058800000807     C                   PARM                    WLBDA8
058900000807     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
059000000000     C*
059100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
059200000000     C                   EXSR      INZSTD
059300020913     C*
059400020913     C* REPERISCE I DATI UTENTE
059500020913     C                   EXSR      REPDATIUTE
059600020913     C*
059700020913     C* ASSEGNA LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO VIDEO
059800020916     C                   MOVEL(P)  RSUT          DSFIRS
059900000000     C*
060000000000     C* EMETTO FORMATO VIDEO
060100000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
060200000000     C*
060300000000     C                   ENDSR
060400020913     C*--------------------------------------------------------------------------------------------*
060500020913     C* REPERISCE DATI UTENTE
060600020913     C*--------------------------------------------------------------------------------------------*
060700020913     C     REPDATIUTE    BEGSR
060800020913     C*
060900020913     C* REimposta le variabili di lavoro
061000020913     C                   CLEAR                   TIBS34DS
061100020913     C                   CLEAR                   AZUTEDS
061200020913     C                   CLEAR                   DDATIUTE
061300020913     C                   CLEAR                   WAbi              2
061400020913     C                   CLEAR                   DLAT
061500020913     C*
061600020913     C* Reperisce i dati del profilo in ingresso
061700020913     C     *DTAARA       DEFINE    §azute        azuteds
061800020913     C     *DTAARA       DEFINE    §datiute      ddatiute
061900020913     C                   IN(E)     *DTAARA
062000020913if  1C                   IF        %Error                                       *NO errori
062100020913     c                   EVAL      I34Tla = 'L'
062200020913     C                   CALL      'TIBS34R'
062300020913     C                   PARM                    Tibs34Ds
062400020913     C                   IN        *DTAARA
062500020913e   1C                   ENDIF
062600101011     ***C*
062700101011     ***C* Se ci sono degli errori sull'abilitazione utente, esce
062800101011sl  1***C                   SELECT
062900101011w   2***C                   WHEN      DUTERR = 'E'
063000101011     ***C                   EVAL      $ERR   = '1'
063100101011     ***C                   EVAL      $FINE  = '1'
063200101011     ***C*
063300101011     ***C* NON siamo in sede (filiale "là" o filiale "qua")
063400101011     ***C* --> se 1° livello, abilitazioni al terminal
063500101011     ***C*     se 2° livello, abilitazioni al punto operativo
063600101011w   2***C                   WHEN      UTEAUT = *blank
063700101011if  3***C                   IF        DUTLPO = '1'                                 *1° LIVELLO
063800101011     ***C                   EVAL      WAbi = 'TP'
063900101011e   3***C                   ENDIF
064000101011if  3***C                   IF        DUTLPO = '2'                                 *2° LIVELLO
064100101011     ***C                   EVAL      WAbi = 'PO'
064200101011e   3***C                   ENDIF
064300101011     ***C*
064400101011     ***C* siamo in sede
064500101011     ***C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
064600101011     ***C*     carica le abilitazioni proprie del profilo
064700101011slx 1***C                   OTHER
064800101011     ***C                   MOVEL     UTEFAF        DUTE01
064900101011if  2***C                   IF        §UTEIst <> *blanks                           *esiste specifica
065000101011     ***C                   EVAL      WAbi = §UTEIst
065100101011x   2***C                   ELSE                                                   *NON esiste specifica
065200101011     ***C                   EVAL      wabi = UTEAUT
065300101011e   2***C                   ENDIF
065400101011e   1***C                   ENDSL
065500101011     ***C*
065600101011     ***C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
065700101011     ***C                   CLEAR                   tibs02ds
065800101011     ***C                   MOVEL     'C'           t02mod
065900101011     ***C                   MOVEL     knsif         t02sif
066000101011     ***C                   MOVEL     'LAT'         t02cod
066100101011     ***C                   MOVEL(P)  Wabi          t02ke1
066200101011     ***C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
066300101011     ***C                   CALL      'TIBS02R'
066400101011     ***C                   PARM                    kpjba
066500101011     ***C                   PARM                    TIBS02DS
066600101011     ***C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
066700101011if  1***C                   IF        t02err = *blanks
066800101011     ***C                   MOVEL     t02uni        DLat
066900101011e   1***C                   ENDIF
067000101011if  1***C                   IF        T02err <> *blanks OR
067100101011     ***C                             §LATabi = 'S'
067200101011     ***C                   EVAL      $ERR   = '1'
067300101011     ***C                   EVAL      $FINE  = '1'
067400101011e   1***C                   ENDIF
067500101011     ***C*
067600101011     ***C* Reperisce i P.O. gestibili dall'utente
067700101011if  1***C                   IF        $ERR <> '1'                                  *NO errori
067800101011     ***C                   CLEAR                   TRUL31DS
067900101011     ***C                   EVAL      I31abi = WAbi
068000101011     ***C                   EVAL      I31cpo = DUTpou
068100101011     ***C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
068200101011     ***C                   CALL      'TRUL31R'
068300101011     ***C                   PARM                    KPJBA
068400101011     ***C                   PARM                    TRUL31DS
068500101011     ***C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
068600101011if  2***C                   IF        O31pog > *zeros
068700101011     ***C                   MOVEA     O31pog        $POG
068800101011x   2***C                   ELSE
068900101011     ***C                   EVAL      $ERR  = '1'
069000101011     ***C                   EVAL      $FINE = '1'
069100101011e   2***C                   ENDIF
069200101011e   1***C                   ENDIF
069300101011      ******
069400101011      ****** La routine TNTAA2R  sostituisce le specifiche precedenti asteriscate
069500101011      ******  con asterischi davanti al tipo "C" di specifica di calcolo.
069600101011     C                   clear                   TAA1DS
069700101011     C                   EVAL        ITAA1CAUT = '§UTEIST'
069800101011     C                   EVAL      KPJBU = TAA1DS
069900101011     C                   CALL      'TNTAA1C'
070000101011     C                   PARM                    KPJBA
070100101011     C                   EVAL              TAA1DS = KPJBU
070200101011      *
070300101011x   2C                   if         OTAA1ERR ='S'
070400101011     C                   EVAL      $ERR  = '1'
070500101011     C                   EVAL      $FINE = '1'
070600101014e   2C                   Else
070700101014x   2C                   eval      wABI =  OTAA1CABI
070800101014e   2C                   ENDIF
070900020913     C*
071000020913     C                   ENDSR
071100020913     C*------------------------------------------------------------------------*
071200020913     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071300000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071400000000     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
071500000000     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
071600000000     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
071700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071800000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071900000000     C*------------------------------------------------------------------------*
072000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
072100000000     C*------------------------------------------------------------------------*
072200000000     C     INZSTD        BEGSR
072300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
072400000000     C                   CALL      'XKEYS'
072500000000     C                   PARM                    DSKEYS
072600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
072700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
072800000000     C     XMSGPA        PLIST
072900000000     C                   PARM                    DSMSMS
073000000000     C                   PARM                    DSMSPG
073100000000     C                   PARM                    DSMSKE
073200000000     C                   PARM                    DSMSER
073300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
073400010920     C                   MOVEL     'TISE60R'     DSMSPG                         *NOME PGM CHIAMANTE
073500010920     C                   MOVEL     'TISE60R'     VMCQUE                         *CODA MESSAGGI DESTI
073600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
073700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
073800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
073900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
074000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
074100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
074200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
074300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
074400000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
074500000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROG,G=GESTIO
074600020905     C*
074700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
074800000000     C                   BITOFF    '01234567'    POSMI1            1
074900000000     C                   BITOFF    '01234567'    POSMI2            1
075000000000     C                   BITOFF    '01234567'    POSMA1            1
075100000000     C                   BITOFF    '01234567'    POSMA2            1
075200000000     C                   BITON     '34'          POSMI1            1            *18
075300000000     C                   BITON     '7'           POSMI2            1            *01
075400000000     C                   BITON     '34'          POSMA1            1            *18
075500000000     C                   BITON     '0'           POSMA2            1            *80
075600000000     C                   MOVE      POSMI2        POSMIN            2
075700000000     C                   MOVEL     POSMI1        POSMIN
075800000000     C                   MOVE      POSMA2        POSMAX            2
075900000000     C                   MOVEL     POSMA1        POSMAX
076000000000     C                   ENDSR
076100000000     C*------------------------------------------------------------------------*
076200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
076300000000     C*------------------------------------------------------------------------*
076400000000     C     FUNSTD        BEGSR
076500000000     C                   SELECT
076600000000     C     KEY           WHENEQ    F3
076700000000     C                   EXSR      F03STD
076800000000     C     KEY           WHENEQ    F12
076900000000     C                   EXSR      F12STD
077000000000     C     KEY           WHENEQ    F24
077100000000     C                   EXSR      F24STD
077200000000     C                   OTHER
077300000000     C                   MOVEL     MSG(2)        $MSG
077400000000     C                   EXSR      CARMSG
077500000000     C                   ENDSL
077600000000     C                   ENDSR
077700000000     C*------------------------------------------------------------------------*
077800000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
077900000000     C*------------------------------------------------------------------------*
078000000000     C     F03STD        BEGSR
078100950721     C*
078200950721     C* IMPOSTO FLAG F03 USCITA PGM
078300950721     C                   MOVEL     '1'           $F03
078400000000     C*
078500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
078600000000     C                   MOVEL     '1'           $FINE
078700000000     C*
078800000000     C                   ENDSR
078900000000     C*------------------------------------------------------------------------*
079000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
079100000000     C*------------------------------------------------------------------------*
079200000000     C     F12STD        BEGSR
079300000000     C*
079400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
079500000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
079600000000     C*
079700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
079800000000     C*--------------------MOVEL'V1'      $GES             *EMISSIONE 1° VIDEO
079900000000     C*
080000000000     C                   ENDSR
080100000000     C*------------------------------------------------------------------------*
080200000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
080300000000     C*------------------------------------------------------------------------*
080400000000     C     F24STD        BEGSR
080500000000     C*
080600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
080700000000     C                   EXSR      OUTFUN
080800000000     C*
080900000000     C                   ENDSR
081000000000     C*------------------------------------------------------------------------*
081100000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
081200000000     C*          CODA MESSAGGI DEL PGM
081300000000     C*------------------------------------------------------------------------*
081400000000     C     CARMSG        BEGSR
081500000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
081600000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
081700000000     C     MSGULT        IFEQ      0
081800000000     C                   MOVEL     '1'           *IN40
081900010920     C                   WRITE     SE60DVK
082000000000     C                   MOVEL     '0'           *IN40
082100000000     C                   ENDIF
082200000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
082300000000     C                   MOVEL     $MSG          DSMSMS
082400000000     C                   CALL      'XMSG'        XMSGPA
082500000000     C     DSMSER        IFEQ      0
082600000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
082700000000     C                   ENDIF
082800000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
082900000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
083000010920     C                   WRITE     SE60DVM                                      *SFL MESS
083100000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
083200000000     C                   MOVEL     *BLANKS       $MSG
083300000000     C                   ENDSR
083400000000     C*------------------------------------------------------------------------*
083500000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
083600000000     C*------------------------------------------------------------------------*
083700000000     C     OUTMSG        BEGSR
083800000000     C* VISUALIZZAZIONE MESSAGGIO
083900000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
084000010920     C                   WRITE     SE60DVK                                      *CONTR MESS
084100000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
084200000000     C                   Z-ADD     *ZEROS        MSGULT
084300000000     C                   ENDSR
084400000000     C*------------------------------------------------------------------------*
084500000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
084600000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
084700000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
084800000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
084900000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
085000000000     C*------------------------------------------------------------------------*
085100000000     C     CARFUN        BEGSR
085200000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
085300000000     C     1             DO        31            I                 5 0          --- 1 -->
085400000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
085500000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
085600000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
085700020905     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
085800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
085900000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
086000000000     C                   ELSE                                                   *INTERROG.
086100000000     C                   MOVEL     FUNASI        FAT(I)
086200000000     C                   ENDIF
086300000000     C                   ELSE                                                   <-- 2 --> *FILIALE
086400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
086500000000     C                   MOVEL     FUNAFG        FAT(I)
086600000000     C                   ELSE                                                   *INTERROG.
086700000000     C                   MOVEL     FUNAFI        FAT(I)
086800000000     C                   ENDIF
086900000000     C                   ENDIF                                                  <-- 2 ---
087000000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
087100000000     C                   MOVEL     USER          WEDP              3            *PROFILO
087200000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
087300000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
087400000000     C                   ELSE
087500000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
087600000000     C                   ENDIF
087700000000     C                   ENDIF
087800000000     C                   ENDDO                                                  <-- 1 ---
087900000000     C                   ENDSR
088000000000     C*------------------------------------------------------------------------*
088100000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
088200000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
088300000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
088400000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
088500000000     C*------------------------------------------------------------------------*
088600000000     C     OUTFUN        BEGSR
088700000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
088800000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
088900000000     C                   ENDDO
089000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
089100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
089200000000     C     1             DO        31            I
089300000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
089400000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
089500000000     C                   ADD       1             FUNNAT
089600000000     C                   ENDIF
089700000000     C                   ENDDO
089800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
089900000000     C     FUNNAT        IFGT      8
090000000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
090100000000     C                   ENDIF
090200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
090300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
090400000000     C                   Z-ADD     8             FUNVIS            5 0
090500000000     C     FAT(23)       IFEQ      'S'
090600000000     C                   SUB       1             FUNVIS
090700000000     C                   ENDIF
090800000000     C     FAT(24)       IFEQ      'S'
090900000000     C                   SUB       1             FUNVIS
091000000000     C                   ENDIF
091100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
091200000000     C                   Z-ADD     *ZEROS        W                 5 0
091300000000     C                   ADD       1             FUNULT
091400000000     C     FUNULT        IFGT      31
091500000000     C                   Z-ADD     1             FUNULT
091600000000     C                   ENDIF
091700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
091800000000     C     FUNULT        DO        31            I                              --- 1 -->
091900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
092000000000     C     W             IFLT      FUNVIS                                       --- 2 -->
092100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
092200000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
092300000000     C     FDE(I)        ANDNE     *BLANKS
092400000000     C     I             IFEQ      23                                           *TOLGO F23
092500000000     C     I             OREQ      24                                           *E F24
092600000000     C                   ELSE
092700000000     C                   ADD       1             W
092800000000     C                   MOVEL     FDE(I)        FUV(W)
092900000000     C                   ENDIF
093000000000     C                   ENDIF                                                  <-- 3 ---
093100000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
093200000000     C                   Z-ADD     I             FUNULT
093300000000     C                   ENDIF                                                  <-- 2 ---
093400000000     C                   ENDDO                                                  <-- 1 ---
093500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
093600000000     C                   MOVEL     FUV(1)        VFD001
093700000000     C                   MOVEL     FUV(2)        VFD002
093800000000     C                   MOVEL     FUV(3)        VFD003
093900000000     C                   MOVEL     FUV(4)        VFD004
094000000000     C                   MOVEL     FUV(5)        VFD005
094100000000     C                   MOVEL     FUV(6)        VFD006
094200000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
094300000000     C     FAT(24)       ANDEQ     'N'
094400000000     C                   MOVEL     FUV(7)        VFD007
094500000000     C                   MOVEL     FUV(8)        VFD008
094600000000     C                   ELSE                                                   <-- 1 -->
094700000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
094800000000     C     FAT(24)       ANDEQ     'S'
094900000000     C                   MOVEL     FDE(23)       VFD007
095000000000     C                   MOVEL     FDE(24)       VFD008
095100000000     C                   ELSE                                                   <-- 2 -->
095200000000     C                   MOVEL     FUV(7)        VFD007
095300000000     C     FAT(23)       IFEQ      'S'
095400000000     C                   MOVEL     FDE(23)       VFD008
095500000000     C                   ELSE
095600000000     C                   MOVEL     FDE(24)       VFD008
095700000000     C                   ENDIF
095800000000     C                   ENDIF                                                  <-- 2 ---
095900000000     C                   ENDIF                                                  <-- 1 ---
096000000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
096100000000     C                   ENDSR
096200000000     C*------------------------------------------------------------------------*
096300000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
096400000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
096500000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
096600000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
096700000000     C*------------------------------------------------------------------------*
096800000000     C     CONFUN        BEGSR
096900000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
097000000000     C     1             DO        31            I                              --- 1 -->
097100000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
097200000000     C* CONTROLLO SE IL TASTO E' ATTIVO
097300000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
097400000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
097500000000     C                   EXSR      CARMSG
097600000000     C                   MOVEL     '1'           $ERR
097700000000     C                   ENDIF                                                  <-- 3 ---
097800000000     C                   ENDIF                                                  <-- 2 ---
097900000000     C                   ENDDO                                                  <-- 1 ---
098000000000     C                   ENDSR
098100000906** MSG  (MESSAGGI)
098200010920TISE60R- Tasto funzionale non valido.                                          1
098300010920TISE60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
098400010920TISE60R- Opzione non valida.                                                   3
098500010920TISE60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
098600010920TISE60R- Inizio elenco.                                                        5
098700010920TISE60R- Fine elenco.                                                          6
098800010920TISE60R- Inserire il codice cliente                                            7
098900010920TISE60R- Cliente errato                                                        8
099000020916TISE60R- Cliente non gestibile                                                 9
099100010920TISE60R- Il cliente richiesto non è unificante                                 10
099200010920TISE60R- 11
099300010920TISE60R- 12
099400010920TISE60R- 13
099500010920TISE60R- 14
099600010920TISE60R- 15
099700010920TISE60R- 16
099800010920TISE60R- 17
099900010920TISE60R- 18
100000010920TISE60R- 19
100100010920TISE60R- 20
100200000906** DEC  (DECODIFICHE)
100300000906Cliente selezionato                1
100400000906Cliente unificante da utilizzare   2
1005000009063
1006000009064
1007000009065
100800000906** CMD  (COMANDI)
100900000000WRKSPLF
101000000000
101100000000
101200000000
101300000000
101400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
101500000000F1    -                   -    NN     NN
101600000000F2    -                   -    NN     NN
101700000000F3    -F3=Fine            -    SS     SS
101800000000F4    -                   -    NN     NN
101900951005F5    -                   -    NN     NN
102000951005F6    -F6=Visualizza      -    SS     SS
102100000000F7    -                   -    NN     NN
102200000000F8    -                   -    NN     NN
102300000000F9    -                   -    NN     NN
102400000000F10   -                   -    NN     NN
102500000000F11   -                   -    NN     NN
102600000000F12   -F12=Ritorno        -    SS     SS
102700000000F13   -                   -    NN     NN
102800000000F14   -                   -    NN     NN
102900000000F15   -                   -    NN     NN
103000000000F16   -                   -    NN     NN
103100000000F17   -                   -    NN     NN
103200000000F18   -                   -    NN     NN
103300000000F19   -                   -    NN     NN
103400000000F20   -                   -    NN     NN
103500951005F21   -F21=Stampa         -    SS     SS
103600951005F22   -F22=Gestione stampe-    EE     EE
103700000000F23   -                   -    NN     NN
103800000000F24   -F24=Altri tasti    -    NN     NN
103900000000ENTER -                   -    SS     SS
104000000000ROLLUP-                   -    NN     NN
104100000000ROLLDN-                   -    NN     NN
104200000000HELP  -                   -    NN     NN
104300000000PRINT -                   -    NN     NN
104400000000BSPACE-                   -    NN     NN
104500000000CLEAR -                   -    NN     NN
