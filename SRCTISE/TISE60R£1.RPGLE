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
042400101011     C                   move      'N'           ITAA1TIPO
042500100512     C                   EVAL      KPJBU = TAA1DS
042600100512     C                   CALL      'TNTAA1C'
042700100512     C                   PARM                    KPJBA
042800100512     C                   EVAL              TAA1DS = KPJBU
042900100512      *
043000100512      *  SE NON ABILITATO PROCEDO CON IL VECCHIO CONTROLLO SULLA FILIALE GESTITA IN LISTA
043100100512     C                   IF        OTAA1FABI <> 'S'
043200100512     C                   MOVEL     '1'           $ERR
043300100512     C                   MOVEL     MSG(9)        $MSG
043400100512     C                   EXSR      CARMSG
043500100512     C                   SETON                                        50
043600100512     c                   end
043700100512      *
043800100512     C*********          SETOFF                                           28
043900100512     c*********          MOVEL     V1CKSC        WAFIL             3
044000100512     C**** WAFIL         LOOKUP    $POG                                   28
044100100512IF  2C**** *IN28         IFEQ      *OFF                                         *CLIENTE NON DELL'AR
044200100512     C*********          MOVEL     '1'           $ERR
044300100512     C*********          MOVEL     MSG(9)        $MSG
044400100512     C*********          EXSR      CARMSG
044500100512     C*********          SETON                                        50
044600100512E   2C*********          ENDIF
044700020910E   1C                   ENDIF
044800000807     C*
044900100512     C*
045000000906     C* SE SCELTA CLIENTE UNIFICANTE, CONTROLLA E NEL CASO REPERISCE IL CLIENTE PADRE
045100000807IF  1C     $ERR          IFEQ      '0'
045200000807IF  2C     D60SCE        IFEQ      '3'                                          *SCELTA UNIFICANTE
045300000807     C                   CLEAR                   BS10DS
045400000807     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
045500000807     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
045600000816     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
045700000817     C                   MOVE      V1CKSC        D10COD
045800000807     C                   CALL      'TIBS10R'
045900000807     C                   PARM                    BS10DS
046000000906IF  3C     D10ERR        IFEQ      *BLANKS                                      *HA UN PADRE
046100000906     C     D10COD        ANDNE     D10COP                                       *MA NON SELEZIONATO
046200000906     c                   MOVE      V1CKSC        V1CKSE                         *C.SELEZIONATO
046300000906     C                   MOVEL     DEC(1)        V1DKSE                         *MESSAGGIO
046400000906     c                   MOVE      D10COP        V1CKSC                         *C.DEDOTTO
046500000906     C                   MOVEL     DEC(2)        V1DKSC                         *MESSAGGIO
046600000906E   3C                   ENDIF
046700000807E   2C                   ENDIF
046800000807E   1C                   ENDIF
046900000807     C*
047000000804     C* RIEMPE I CAMPI DAT  DEL CLIENTE
047100000804IF  1C     $ERR          IFEQ      '0'                                          *ANCORA VALIDO
047200000000     C                   EXSR      RIEMPI                                       *CARICA I DATI
047300000000E   1C                   ENDIF
047400000000     C*
047500000000     C                   ENDSR
047600000000     C*------------------------------------------------------------------------*
047700950719     C* PULVID - PULISCE LA PARTE DI VIDEO CON I DATI DEL CLIENTE
047800000000     C*------------------------------------------------------------------------*
047900000000     C     PULVID        BEGSR
048000000000     C*
048100000000     C* CAMPI DATI
048200000000     C                   MOVEL     *BLANKS       V1CRAG
048300000000     C                   MOVEL     *BLANKS       V1CVIA
048400000000     C                   MOVEL     *BLANKS       V1CCIT
048500000000     C                   MOVEL     *BLANKS       V1CCAP
048600000000     C                   MOVEL     *BLANKS       V1CPRV
048700000000     C                   MOVEL     *BLANKS       V1CSTA
048800000000     C                   MOVEL     *BLANKS       V1CTEL
048900000000     C                   MOVEL     *BLANKS       V1CCLV
049000951006     C                   Z-ADD     *ZEROS        V1CITC
049100000000     C                   Z-ADD     *ZEROS        V1CDPS
049200000000     C                   Z-ADD     *ZEROS        V1CAGE
049300000000     C                   MOVEL     *BLANKS       V1CCDP
049400000000     C                   MOVEL     *BLANKS       V1CSOL
049500000000     C*
049600000000     C* CAMPI DECODIFICA
049700000000     C                   MOVEL     *BLANKS       V1DPRV
049800951006     C                   MOVEL     *BLANKS       V1DITC
049900000000     C                   MOVEL     *BLANKS       V1DAGE
050000000000     C                   MOVEL     *BLANKS       V1DCDP
050100000906     C                   Z-ADD     *ZEROS        V1CKSE
050200000906     C                   MOVEL     *BLANKS       V1DKSE
050300000906     C                   MOVEL     *BLANKS       V1DKSC
050400000000     C*
050500000000     C                   ENDSR
050600000000     C*------------------------------------------------------------------------*
050700950719     C* RIEMPI - RIEMPIMENTO VIDEO CON I DATI DEL CLIENTE
050800000000     C*------------------------------------------------------------------------*
050900000000     C     RIEMPI        BEGSR
051000000000     C*
051100020117     C* CHIAMA IL PGM 'TISE66R' CHE REPERISCE I DATI DEL CLIENTE
051200000000     C                   CLEAR                   DSCL                           *PULISCE DS PASSAGGI
051300020902     C                   Z-ADD     1             DCLKUT
051400000000     C                   MOVEL     V1CKSC        DCLKSC
051500020117     C                   CALL      'TISE66R'
051600000000     C                   PARM                    DSCL
051700000000     C*--------------------
051800000000     C* CAMPI
051900000000     C*--------------------
052000000000     C                   MOVEL     DCLRAG        V1CRAG
052100000000     C                   MOVEL     DCLVIA        V1CVIA
052200000000     C                   MOVEL     DCLCIT        V1CCIT
052300000000     C                   MOVEL     DCLCAP        V1CCAP
052400000000     C                   MOVEL     DCLPRV        V1CPRV
052500000000     C                   MOVEL     DCLSTA        V1CSTA
052600000000     C                   MOVEL     DCLTEL        V1CTEL
052700000000     C                   MOVEL     DCLCLV        V1CCLV
052800951006     C                   Z-ADD     DCLITC        V1CITC
052900000000     C                   Z-ADD     DCLDPS        V1CDPS
053000000000     C                   Z-ADD     DCLAGE        V1CAGE
053100000000     C                   MOVE      DCLCDP        V1CCDP
053200000000     C                   MOVEL     DCLSOL        V1CSOL
053300000000     C*--------------------
053400000000     C* DECODIFICHE
053500000000     C*--------------------
053600000000     C                   MOVEL     DCLDPR        V1DPRV
053700951006     C                   MOVEL     DCLDIT        V1DITC
053800000000     C                   MOVEL     DCLDAG        V1DAGE
053900000000     C                   MOVEL     DCLDCD        V1DCDP
054000000000     C*
054100000000     C                   ENDSR
054200000810     C*------------------------------------------------------------------------*
054300000810     C* FINSR - OPERAZIONI FINALI
054400000810     C*------------------------------------------------------------------------*
054500000810     C     FINSR         BEGSR
054600000810     C*
054700000810     C* CHIUDE I FILE DEI PGM CHIAMATI
054800000810     C                   CLEAR                   BS69DS
054900000810     C                   CLEAR                   ACODS
055000000810     C                   CLEAR                   INDDS
055100000810     C                   CLEAR                   CLPDS
055200000810     C                   CLEAR                   CLSDS
055300000810     C                   MOVEL     'C'           I69TLA
055400000810     C                   MOVEL     KNSIF         I69SIF
055500000810     C                   CALL      'TIBS69R'
055600000810     C                   PARM                    BS69DS
055700000810     C                   PARM                    ACODS
055800000810     C                   PARM                    INDDS
055900000810     C                   PARM                    CLPDS
056000000810     C                   PARM                    CLSDS
056100000810     C*
056200000810     C                   ENDSR
056300000000     C*------------------------------------------------------------------------*
056400950719     C* *INZSR - ROUTINE INIZIALE
056500000000     C*------------------------------------------------------------------------*
056600000000     C     *INZSR        BEGSR
056700000000     C*
056800000000     C* RICEVIMENTO PARAMETRI
056900000000     C     *ENTRY        PLIST
057000000000     C                   PARM                    KPJBA
057100950726     C                   MOVEL     KPJBU         DS60
057200000000     C*
057300000000     C* DEFINIZIONI VARIABILI
057400000000     C     *LIKE         DEFINE    ACOKUT        KACKUT
057500000000     C     *LIKE         DEFINE    ACOKCC        KACKCC
057600000000     C     *LIKE         DEFINE    ACOKSC        KACKSC
057700020905     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
057800000000     C*
057900000000     C     *LIKE         DEFINE    D60OP0        WOP0
058000000000     C     *LIKE         DEFINE    D60OP1        WOP1
058100000000     C     *LIKE         DEFINE    D60KSC        WKSC
058200000807     C*
058300000807     C* IMNPOSTA DATA / ORA CORRENTE
058400000807     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
058500000807     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
058600000807     C                   Z-ADD     *ZEROS        G08INV
058700000807     C                   MOVEL     '0'           G08ERR
058800000807     C                   CALL      'XSRDA8'
058900000807     C                   PARM                    WLBDA8
059000000807     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
059100000000     C*
059200000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
059300000000     C                   EXSR      INZSTD
059400020913     C*
059500020913     C* REPERISCE I DATI UTENTE
059600020913     C                   EXSR      REPDATIUTE
059700020913     C*
059800020913     C* ASSEGNA LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO VIDEO
059900020916     C                   MOVEL(P)  RSUT          DSFIRS
060000000000     C*
060100000000     C* EMETTO FORMATO VIDEO
060200000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
060300000000     C*
060400000000     C                   ENDSR
060500020913     C*--------------------------------------------------------------------------------------------*
060600020913     C* REPERISCE DATI UTENTE
060700020913     C*--------------------------------------------------------------------------------------------*
060800020913     C     REPDATIUTE    BEGSR
060900020913     C*
061000020913     C* REimposta le variabili di lavoro
061100020913     C                   CLEAR                   TIBS34DS
061200020913     C                   CLEAR                   AZUTEDS
061300020913     C                   CLEAR                   DDATIUTE
061400020913     C                   CLEAR                   WAbi              2
061500020913     C                   CLEAR                   DLAT
061600020913     C*
061700020913     C* Reperisce i dati del profilo in ingresso
061800020913     C     *DTAARA       DEFINE    §azute        azuteds
061900020913     C     *DTAARA       DEFINE    §datiute      ddatiute
062000020913     C                   IN(E)     *DTAARA
062100020913if  1C                   IF        %Error                                       *NO errori
062200020913     c                   EVAL      I34Tla = 'L'
062300020913     C                   CALL      'TIBS34R'
062400020913     C                   PARM                    Tibs34Ds
062500020913     C                   IN        *DTAARA
062600020913e   1C                   ENDIF
062700101011     ***C*
062800101011     ***C* Se ci sono degli errori sull'abilitazione utente, esce
062900101011sl  1***C                   SELECT
063000101011w   2***C                   WHEN      DUTERR = 'E'
063100101011     ***C                   EVAL      $ERR   = '1'
063200101011     ***C                   EVAL      $FINE  = '1'
063300101011     ***C*
063400101011     ***C* NON siamo in sede (filiale "là" o filiale "qua")
063500101011     ***C* --> se 1° livello, abilitazioni al terminal
063600101011     ***C*     se 2° livello, abilitazioni al punto operativo
063700101011w   2***C                   WHEN      UTEAUT = *blank
063800101011if  3***C                   IF        DUTLPO = '1'                                 *1° LIVELLO
063900101011     ***C                   EVAL      WAbi = 'TP'
064000101011e   3***C                   ENDIF
064100101011if  3***C                   IF        DUTLPO = '2'                                 *2° LIVELLO
064200101011     ***C                   EVAL      WAbi = 'PO'
064300101011e   3***C                   ENDIF
064400101011     ***C*
064500101011     ***C* siamo in sede
064600101011     ***C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
064700101011     ***C*     carica le abilitazioni proprie del profilo
064800101011slx 1***C                   OTHER
064900101011     ***C                   MOVEL     UTEFAF        DUTE01
065000101011if  2***C                   IF        §UTEIst <> *blanks                           *esiste specifica
065100101011     ***C                   EVAL      WAbi = §UTEIst
065200101011x   2***C                   ELSE                                                   *NON esiste specifica
065300101011     ***C                   EVAL      wabi = UTEAUT
065400101011e   2***C                   ENDIF
065500101011e   1***C                   ENDSL
065600101011     ***C*
065700101011     ***C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
065800101011     ***C                   CLEAR                   tibs02ds
065900101011     ***C                   MOVEL     'C'           t02mod
066000101011     ***C                   MOVEL     knsif         t02sif
066100101011     ***C                   MOVEL     'LAT'         t02cod
066200101011     ***C                   MOVEL(P)  Wabi          t02ke1
066300101011     ***C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
066400101011     ***C                   CALL      'TIBS02R'
066500101011     ***C                   PARM                    kpjba
066600101011     ***C                   PARM                    TIBS02DS
066700101011     ***C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
066800101011if  1***C                   IF        t02err = *blanks
066900101011     ***C                   MOVEL     t02uni        DLat
067000101011e   1***C                   ENDIF
067100101011if  1***C                   IF        T02err <> *blanks OR
067200101011     ***C                             §LATabi = 'S'
067300101011     ***C                   EVAL      $ERR   = '1'
067400101011     ***C                   EVAL      $FINE  = '1'
067500101011e   1***C                   ENDIF
067600101011     ***C*
067700101011     ***C* Reperisce i P.O. gestibili dall'utente
067800101011if  1***C                   IF        $ERR <> '1'                                  *NO errori
067900101011     ***C                   CLEAR                   TRUL31DS
068000101011     ***C                   EVAL      I31abi = WAbi
068100101011     ***C                   EVAL      I31cpo = DUTpou
068200101011     ***C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
068300101011     ***C                   CALL      'TRUL31R'
068400101011     ***C                   PARM                    KPJBA
068500101011     ***C                   PARM                    TRUL31DS
068600101011     ***C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
068700101011if  2***C                   IF        O31pog > *zeros
068800101011     ***C                   MOVEA     O31pog        $POG
068900101011x   2***C                   ELSE
069000101011     ***C                   EVAL      $ERR  = '1'
069100101011     ***C                   EVAL      $FINE = '1'
069200101011e   2***C                   ENDIF
069300101011e   1***C                   ENDIF
069400101011      ******
069500101011      ****** La routine TNTAA2R  sostituisce le specifiche precedenti asteriscate
069600101011      ******  con asterischi davanti al tipo "C" di specifica di calcolo.
069700101011     C                   clear                   TAA1DS
069800101011     C                   EVAL        ITAA1CAUT = '§UTEIST'
069900101011     C                   move      'N'           ITAA1TIPO
070000101011     C                   EVAL      KPJBU = TAA1DS
070100101011     C                   CALL      'TNTAA1C'
070200101011     C                   PARM                    KPJBA
070300101011     C                   EVAL              TAA1DS = KPJBU
070400101011      *
070500101011x   2C                   if         OTAA1ERR ='S'
070600101011     C                   EVAL      $ERR  = '1'
070700101011     C                   EVAL      $FINE = '1'
070800101014e   2C                   Else
070900101014x   2C                   eval      wABI =  OTAA1CABI
071000101014e   2C                   ENDIF
071100020913     C*
071200020913     C                   ENDSR
071300020913     C*------------------------------------------------------------------------*
071400020913     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071600000000     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
071700000000     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
071800000000     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
071900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072100000000     C*------------------------------------------------------------------------*
072200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
072300000000     C*------------------------------------------------------------------------*
072400000000     C     INZSTD        BEGSR
072500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
072600000000     C                   CALL      'XKEYS'
072700000000     C                   PARM                    DSKEYS
072800000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
072900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
073000000000     C     XMSGPA        PLIST
073100000000     C                   PARM                    DSMSMS
073200000000     C                   PARM                    DSMSPG
073300000000     C                   PARM                    DSMSKE
073400000000     C                   PARM                    DSMSER
073500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
073600010920     C                   MOVEL     'TISE60R'     DSMSPG                         *NOME PGM CHIAMANTE
073700010920     C                   MOVEL     'TISE60R'     VMCQUE                         *CODA MESSAGGI DESTI
073800000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
073900000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
074000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
074100000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
074200000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
074300000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
074400000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
074500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
074600000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
074700000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROG,G=GESTIO
074800020905     C*
074900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
075000000000     C                   BITOFF    '01234567'    POSMI1            1
075100000000     C                   BITOFF    '01234567'    POSMI2            1
075200000000     C                   BITOFF    '01234567'    POSMA1            1
075300000000     C                   BITOFF    '01234567'    POSMA2            1
075400000000     C                   BITON     '34'          POSMI1            1            *18
075500000000     C                   BITON     '7'           POSMI2            1            *01
075600000000     C                   BITON     '34'          POSMA1            1            *18
075700000000     C                   BITON     '0'           POSMA2            1            *80
075800000000     C                   MOVE      POSMI2        POSMIN            2
075900000000     C                   MOVEL     POSMI1        POSMIN
076000000000     C                   MOVE      POSMA2        POSMAX            2
076100000000     C                   MOVEL     POSMA1        POSMAX
076200000000     C                   ENDSR
076300000000     C*------------------------------------------------------------------------*
076400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
076500000000     C*------------------------------------------------------------------------*
076600000000     C     FUNSTD        BEGSR
076700000000     C                   SELECT
076800000000     C     KEY           WHENEQ    F3
076900000000     C                   EXSR      F03STD
077000000000     C     KEY           WHENEQ    F12
077100000000     C                   EXSR      F12STD
077200000000     C     KEY           WHENEQ    F24
077300000000     C                   EXSR      F24STD
077400000000     C                   OTHER
077500000000     C                   MOVEL     MSG(2)        $MSG
077600000000     C                   EXSR      CARMSG
077700000000     C                   ENDSL
077800000000     C                   ENDSR
077900000000     C*------------------------------------------------------------------------*
078000000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
078100000000     C*------------------------------------------------------------------------*
078200000000     C     F03STD        BEGSR
078300950721     C*
078400950721     C* IMPOSTO FLAG F03 USCITA PGM
078500950721     C                   MOVEL     '1'           $F03
078600000000     C*
078700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
078800000000     C                   MOVEL     '1'           $FINE
078900000000     C*
079000000000     C                   ENDSR
079100000000     C*------------------------------------------------------------------------*
079200000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
079300000000     C*------------------------------------------------------------------------*
079400000000     C     F12STD        BEGSR
079500000000     C*
079600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
079700000000     C                   MOVEL     '1'           $FINE                          *FINE PGM
079800000000     C*
079900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
080000000000     C*--------------------MOVEL'V1'      $GES             *EMISSIONE 1° VIDEO
080100000000     C*
080200000000     C                   ENDSR
080300000000     C*------------------------------------------------------------------------*
080400000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
080500000000     C*------------------------------------------------------------------------*
080600000000     C     F24STD        BEGSR
080700000000     C*
080800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
080900000000     C                   EXSR      OUTFUN
081000000000     C*
081100000000     C                   ENDSR
081200000000     C*------------------------------------------------------------------------*
081300000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
081400000000     C*          CODA MESSAGGI DEL PGM
081500000000     C*------------------------------------------------------------------------*
081600000000     C     CARMSG        BEGSR
081700000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
081800000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
081900000000     C     MSGULT        IFEQ      0
082000000000     C                   MOVEL     '1'           *IN40
082100010920     C                   WRITE     SE60DVK
082200000000     C                   MOVEL     '0'           *IN40
082300000000     C                   ENDIF
082400000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
082500000000     C                   MOVEL     $MSG          DSMSMS
082600000000     C                   CALL      'XMSG'        XMSGPA
082700000000     C     DSMSER        IFEQ      0
082800000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
082900000000     C                   ENDIF
083000000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
083100000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
083200010920     C                   WRITE     SE60DVM                                      *SFL MESS
083300000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
083400000000     C                   MOVEL     *BLANKS       $MSG
083500000000     C                   ENDSR
083600000000     C*------------------------------------------------------------------------*
083700000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
083800000000     C*------------------------------------------------------------------------*
083900000000     C     OUTMSG        BEGSR
084000000000     C* VISUALIZZAZIONE MESSAGGIO
084100000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
084200010920     C                   WRITE     SE60DVK                                      *CONTR MESS
084300000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
084400000000     C                   Z-ADD     *ZEROS        MSGULT
084500000000     C                   ENDSR
084600000000     C*------------------------------------------------------------------------*
084700000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
084800000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
084900000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
085000000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
085100000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
085200000000     C*------------------------------------------------------------------------*
085300000000     C     CARFUN        BEGSR
085400000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
085500000000     C     1             DO        31            I                 5 0          --- 1 -->
085600000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
085700000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
085800000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
085900020905     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
086000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
086100000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
086200000000     C                   ELSE                                                   *INTERROG.
086300000000     C                   MOVEL     FUNASI        FAT(I)
086400000000     C                   ENDIF
086500000000     C                   ELSE                                                   <-- 2 --> *FILIALE
086600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
086700000000     C                   MOVEL     FUNAFG        FAT(I)
086800000000     C                   ELSE                                                   *INTERROG.
086900000000     C                   MOVEL     FUNAFI        FAT(I)
087000000000     C                   ENDIF
087100000000     C                   ENDIF                                                  <-- 2 ---
087200000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
087300000000     C                   MOVEL     USER          WEDP              3            *PROFILO
087400000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
087500000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
087600000000     C                   ELSE
087700000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
087800000000     C                   ENDIF
087900000000     C                   ENDIF
088000000000     C                   ENDDO                                                  <-- 1 ---
088100000000     C                   ENDSR
088200000000     C*------------------------------------------------------------------------*
088300000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
088400000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
088500000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
088600000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
088700000000     C*------------------------------------------------------------------------*
088800000000     C     OUTFUN        BEGSR
088900000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
089000000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
089100000000     C                   ENDDO
089200000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
089300000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
089400000000     C     1             DO        31            I
089500000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
089600000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
089700000000     C                   ADD       1             FUNNAT
089800000000     C                   ENDIF
089900000000     C                   ENDDO
090000000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
090100000000     C     FUNNAT        IFGT      8
090200000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
090300000000     C                   ENDIF
090400000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
090500000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
090600000000     C                   Z-ADD     8             FUNVIS            5 0
090700000000     C     FAT(23)       IFEQ      'S'
090800000000     C                   SUB       1             FUNVIS
090900000000     C                   ENDIF
091000000000     C     FAT(24)       IFEQ      'S'
091100000000     C                   SUB       1             FUNVIS
091200000000     C                   ENDIF
091300000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
091400000000     C                   Z-ADD     *ZEROS        W                 5 0
091500000000     C                   ADD       1             FUNULT
091600000000     C     FUNULT        IFGT      31
091700000000     C                   Z-ADD     1             FUNULT
091800000000     C                   ENDIF
091900000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
092000000000     C     FUNULT        DO        31            I                              --- 1 -->
092100000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
092200000000     C     W             IFLT      FUNVIS                                       --- 2 -->
092300000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
092400000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
092500000000     C     FDE(I)        ANDNE     *BLANKS
092600000000     C     I             IFEQ      23                                           *TOLGO F23
092700000000     C     I             OREQ      24                                           *E F24
092800000000     C                   ELSE
092900000000     C                   ADD       1             W
093000000000     C                   MOVEL     FDE(I)        FUV(W)
093100000000     C                   ENDIF
093200000000     C                   ENDIF                                                  <-- 3 ---
093300000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
093400000000     C                   Z-ADD     I             FUNULT
093500000000     C                   ENDIF                                                  <-- 2 ---
093600000000     C                   ENDDO                                                  <-- 1 ---
093700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
093800000000     C                   MOVEL     FUV(1)        VFD001
093900000000     C                   MOVEL     FUV(2)        VFD002
094000000000     C                   MOVEL     FUV(3)        VFD003
094100000000     C                   MOVEL     FUV(4)        VFD004
094200000000     C                   MOVEL     FUV(5)        VFD005
094300000000     C                   MOVEL     FUV(6)        VFD006
094400000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
094500000000     C     FAT(24)       ANDEQ     'N'
094600000000     C                   MOVEL     FUV(7)        VFD007
094700000000     C                   MOVEL     FUV(8)        VFD008
094800000000     C                   ELSE                                                   <-- 1 -->
094900000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
095000000000     C     FAT(24)       ANDEQ     'S'
095100000000     C                   MOVEL     FDE(23)       VFD007
095200000000     C                   MOVEL     FDE(24)       VFD008
095300000000     C                   ELSE                                                   <-- 2 -->
095400000000     C                   MOVEL     FUV(7)        VFD007
095500000000     C     FAT(23)       IFEQ      'S'
095600000000     C                   MOVEL     FDE(23)       VFD008
095700000000     C                   ELSE
095800000000     C                   MOVEL     FDE(24)       VFD008
095900000000     C                   ENDIF
096000000000     C                   ENDIF                                                  <-- 2 ---
096100000000     C                   ENDIF                                                  <-- 1 ---
096200000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
096300000000     C                   ENDSR
096400000000     C*------------------------------------------------------------------------*
096500000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
096600000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
096700000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
096800000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
096900000000     C*------------------------------------------------------------------------*
097000000000     C     CONFUN        BEGSR
097100000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
097200000000     C     1             DO        31            I                              --- 1 -->
097300000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
097400000000     C* CONTROLLO SE IL TASTO E' ATTIVO
097500000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
097600000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
097700000000     C                   EXSR      CARMSG
097800000000     C                   MOVEL     '1'           $ERR
097900000000     C                   ENDIF                                                  <-- 3 ---
098000000000     C                   ENDIF                                                  <-- 2 ---
098100000000     C                   ENDDO                                                  <-- 1 ---
098200000000     C                   ENDSR
098300000906** MSG  (MESSAGGI)
098400010920TISE60R- Tasto funzionale non valido.                                          1
098500010920TISE60R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
098600010920TISE60R- Opzione non valida.                                                   3
098700010920TISE60R- Opzione abilitata ma non gestita. Avvertire CED.                      4
098800010920TISE60R- Inizio elenco.                                                        5
098900010920TISE60R- Fine elenco.                                                          6
099000010920TISE60R- Inserire il codice cliente                                            7
099100010920TISE60R- Cliente errato                                                        8
099200020916TISE60R- Cliente non gestibile                                                 9
099300010920TISE60R- Il cliente richiesto non è unificante                                 10
099400010920TISE60R- 11
099500010920TISE60R- 12
099600010920TISE60R- 13
099700010920TISE60R- 14
099800010920TISE60R- 15
099900010920TISE60R- 16
100000010920TISE60R- 17
100100010920TISE60R- 18
100200010920TISE60R- 19
100300010920TISE60R- 20
100400000906** DEC  (DECODIFICHE)
100500000906Cliente selezionato                1
100600000906Cliente unificante da utilizzare   2
1007000009063
1008000009064
1009000009065
101000000906** CMD  (COMANDI)
101100000000WRKSPLF
101200000000
101300000000
101400000000
101500000000
101600000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
101700000000F1    -                   -    NN     NN
101800000000F2    -                   -    NN     NN
101900000000F3    -F3=Fine            -    SS     SS
102000000000F4    -                   -    NN     NN
102100951005F5    -                   -    NN     NN
102200951005F6    -F6=Visualizza      -    SS     SS
102300000000F7    -                   -    NN     NN
102400000000F8    -                   -    NN     NN
102500000000F9    -                   -    NN     NN
102600000000F10   -                   -    NN     NN
102700000000F11   -                   -    NN     NN
102800000000F12   -F12=Ritorno        -    SS     SS
102900000000F13   -                   -    NN     NN
103000000000F14   -                   -    NN     NN
103100000000F15   -                   -    NN     NN
103200000000F16   -                   -    NN     NN
103300000000F17   -                   -    NN     NN
103400000000F18   -                   -    NN     NN
103500000000F19   -                   -    NN     NN
103600000000F20   -                   -    NN     NN
103700951005F21   -F21=Stampa         -    SS     SS
103800951005F22   -F22=Gestione stampe-    EE     EE
103900000000F23   -                   -    NN     NN
104000000000F24   -F24=Altri tasti    -    NN     NN
104100000000ENTER -                   -    SS     SS
104200000000ROLLUP-                   -    NN     NN
104300000000ROLLDN-                   -    NN     NN
104400000000HELP  -                   -    NN     NN
104500000000PRINT -                   -    NN     NN
104600000000BSPACE-                   -    NN     NN
104700000000CLEAR -                   -    NN     NN
