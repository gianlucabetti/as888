000100950721     H*------------------------------------------------------------------------*
000200950721     H*                    - STATISTICA CLIENTI -      RICHIESTA PIU' CLIENTI  *
000300950721     H*------------------------------------------------------------------------*
000400950721     H* VALORIZZA NELLA DS DI PROCEDURA (DS60):
000500000816     H*         - LIVELLO PROCEDURA = 'VIS'
000600951005     H*         - LIVELLO PROGRAMMA = 'F06' --> SE RICHIESTA LA VISUALIZZAZIONE
000700950721     H*                               'F21' --> SE RICHIESTA LA STAMPA
000800950721     H*------------------------------------------------------------------------*
000900950721     H DECEDIT('0,') DATEDIT(*DMY.)
001000950721     F*------------------------------------------------------------------------*
001100950721     F* DATA BASE
001200950721     F*------------------------------------------------------------------------*
001300020117     FTISE59D   CF   E             WORKSTN INFDS(DEVDS1)
001400020117     F                                     SFILE(SE59DVM:MSGULT)
001500950721     D*------------------------------------------------------------------------*
001600950721     D* SCHIERE
001700950721     D*------------------------------------------------------------------------*
001800950721     D*------------------
001900950721     D* MESSAGGI, COMANDI, FUNZIONI
002000950721     D*------------------
002100950721     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002200950721     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
002300950721     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002400950721     D*------------------
002500950721     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600950721     D*------------------
002700950721     D FFU             S              6    DIM(31)                              *FUNZIONE
002800950721     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002900950721     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003000950721     D*------------------
003100950721     D* TABELLA DESCRIZIONI A VIDEO
003200950721     D*------------------
003300950721     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003400950721     D*------------------
003500950721     D* TABELLA TASTI FUNZIONALI PREMUTI
003600950721     D*------------------
003700950721     D KEYS            S              1    DIM(40)
003800950721     D*-------------------
003900950721     D* CAPOCONTO DI RITORNO "XCLIR"
004000950721     D*-------------------
004100050117      ** i codici clienti da passare nella DS sono diventati 28 da 30 che erano
004200050117      ** x permettere passaggio di altri parametri poichè la DS era già di 256
004300050117     D*KSC****         S              7  0 DIM(30)                              *NUMERICO
004400050117     D*KSA****         S              4    DIM(30)                              *ALFANUMERICO
004500050117     D KSC             S              7  0 DIM(28)                              *NUMERICO
004600050117     D KSA             S              4    DIM(28)                              *ALFANUMERICO
004700020916     D*------------
004800020916     D* FILIALI IN GESTIONE
004900020916     D*------------
005000020916     D $POG            s              3    DIM(250) INZ(*zeros)
005100950721     D*-------------------
005200950721     D* SOTTOCONTO/DESCRIZIONE DI RITORNO "XCLIR" PER IL VIDEO
005300950721     D*-------------------
005400050117     D*VKSC****        S              7    DIM(30)                              *CODICE
005500050117     D*VKSD****        S             20    DIM(30)                              *RAGIONE SOCIALE
005600050117     D VKSC            S              7    DIM(28)                              *CODICE
005700050117     D VKSD            S             20    DIM(28)                              *RAGIONE SOCIALE
005800950721     D*------------------------------------------------------------------------*
005900950721     D* INPUT
006000950721     D*------------------------------------------------------------------------*
006100950721     D*------------------
006200950721     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006300950721     D*------------------
006400950721     D DSFUN         E DS
006500950721     D  FUNRIG                 1     40
006600950721     D*------------------
006700950721     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006800950721     D*------------------
006900950721     D DSMSG         E DS
007000950721     D*------------------
007100950721     D* DS PER GESTIONE TASTI FUNZIONALI
007200950721     D*------------------
007300950721     D DSKEYS        E DS
007400950721     D  KEYRIG                 1     40
007500950721     D*------------------
007600950721     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
007700950721     D*------------------
007800950721     D                SDS
007900950721     D  USER                 254    263
008000950721     D*------------------
008100950721     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008200950721     D*------------------
008300950721     D DEVDS1          DS
008400950721     D  KEY                  369    369
008500950721     D  CURSOR               370    371
008600950721     D*------------------
008700950721     D* ARCHITETTURA
008800950721     D*------------------
008900950721     D KPJBA         E DS
009000101011     D*------------------
009100101011     D* DS X CONTROLLO ABILITAZIONI
009200101011     D*------------------
009300101011     D TAA1DS        E DS                  EXTNAME(TNTAA1DS)
009400950721     D*------------------
009500950721     D* DS DI PROCEDURA
009600950721     D*------------------
009700020117     D DS60          E DS                  EXTNAME(TISE60DS)
009800950721     D*------------------
009900950721     D* DS "XCLIR"
010000950721     D*------------------
010100950721     D DSXC          E DS                  EXTNAME(XCLIRDS)
010200000810     D*------------------
010300000810     D* DS REPERIMENTO DATI UTENTE
010400000810     D*-------------------
010500000810     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
010600000810     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
010700000810     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
010800000810     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
010900000810     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
011000950721     D*------------------
011100950721     D* DS UNIONE CODICI CLIENTE A VIDEO
011200950721     D*------------------
011300950721     D                 DS
011400950721     D  V1CKS1                 1      7
011500950721     D  V1CKS2                 8     14
011600950721     D  V1CKS3                15     21
011700950721     D  V1CKS4                22     28
011800950721     D  V1CKS5                29     35
011900950721     D  V1CKS6                36     42
012000950721     D  V1CKS7                43     49
012100950721     D  V1CKS8                50     56
012200950721     D  V1CKS9                57     63
012300950721     D  V1CK10                64     70
012400950721     D  V1CK11                71     77
012500950721     D  V1CK12                78     84
012600950721     D  V1CK13                85     91
012700950721     D  V1CK14                92     98
012800950721     D  V1CK15                99    105
012900950721     D  V1CK16               106    112
013000950721     D  V1CK17               113    119
013100950721     D  V1CK18               120    126
013200950721     D  V1CK19               127    133
013300950721     D  V1CK20               134    140
013400950721     D  V1CK21               141    147
013500950721     D  V1CK22               148    154
013600950721     D  V1CK23               155    161
013700950721     D  V1CK24               162    168
013800950721     D  V1CK25               169    175
013900950721     D  V1CK26               176    182
014000950721     D  V1CK27               183    189
014100950721     D  V1CK28               190    196
014200050117      ** i codici clienti da passare nella DS sono diventati 28 da 30 che erano
014300050117      ** x permettere passaggio di altri parametri poichè la DS era già di 256
014400050117     D**V1CK29*****          197    203
014500050117     D**V1CK30*****          204    210
014600050117     D**V1CKSC*****            1    210
014700050117     D  V1CKSC                 1    196
014800950724     D*------------------
014900950724     D* DS PER PASSAGGIO CAMPI PACKED
015000950724     D*------------------
015100950724     D                 DS
015200950724     D  DSKSN                  1      4P 0
015300950724     D  DSKSA                  1      4
015400950724     D*------------------
015500950724     D* COSTANTI
015600950724     D*------------------
015700950724     D DECSTA          C                   CONST('PIU" CLIENTI INSIEME')
015800020905     D*-------------
015900020905     D* DS DI WRK
016000020905     D*-------------
016100020916     D TIBS02DS      E DS                                                       *Ricerca tabella
016200020905     D TIBS34DS      E DS                                                       *Profili utente
016300020905     D DDATIUTE      E DS                                                       *Dati utente
016400020905     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
016500020916     D*------------------
016600020916     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
016700020916     D*-------------------
016800020916     D DUTE01        e ds                  INZ                                  *Abilitazioni utenti
016900020916     D*------------------
017000020916     D* DS FUNZIONI AZIENDALI
017100020916     D*-------------------
017200020916     D DLAT          e ds                  INZ                                  *Funzioni aziendali
017300020916     D*------------------
017400020916     D* DS CARICAMENTO P.O. GESTITI
017500020916     D*-------------------
017600020916     D TRUL31DS      e ds                  INZ
017700950721     C*------------------------------------------------------------------------*
017800950721     C* MAIN LINE
017900950721     C*------------------------------------------------------------------------*
018000950721     C*
018100950721     C* CICLO FINO A CHE:
018200950721DO  1C     $FINE         DOUEQ     '1'
018300950721     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
018400950721     C*
018500950721     C* CONTROLLA GESTIONE VIDEO
018600950721IF  2C     $GES          IFEQ      'V1'
018700950721     C                   EXSR      GESV1                                        *RICHIESTA CODICE
018800950721E   2C                   ENDIF
018900950721     C*
019000950721E   1C                   ENDDO
019100950721     C*
019200950721     C* SE PREMUTO F03 LO IMPOSTO NELLA DS DELLA PROCEDURA
019300950721     C     $F03          IFEQ      '1'
019400950721     C                   MOVEL     '1'           D60F03
019500950721     C                   ENDIF
019600000810     C*
019700000810     C* OPERAZIONI FINALI
019800000810     C                   EXSR      FINSR
019900950721     C* FINE PGM
020000950724     C                   MOVEL     DS60          KPJBU
020100950721     C                   SETON                                        LR
020200950721     C*------------------------------------------------------------------------*
020300950721     C* GESV1 - GESTIONE VIDEO RICHIESTA CODICE CLIENTE
020400950721     C*------------------------------------------------------------------------*
020500950721     C     GESV1         BEGSR
020600950721     C*
020700950721IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
020800950721     C                   EXSR      CARV1                                        *CARICA TESTA + FUNZ
020900950721E   1C                   ENDIF
021000950721     C*
021100950721IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
021200950721     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
021300950721     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
021400950721E   1C                   ENDIF
021500950721     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
021600950721     C*
021700020117     C                   EXFMT     SE59DV1                                      *RICHIESTA CODICE CL
021800950721     C*
021900950721     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
022000950721     C*
022100950721IF  1C     $ERR          IFEQ      '0'
022200950721SL  2C                   SELECT
022300950721     C     KEY           WHENEQ    ENTER
022400950721     C                   EXSR      ENTV1                                        *CONTROLLO VIDEO
022500951005     C     KEY           WHENEQ    F6
022600951005     C                   EXSR      F06GES                                       *GEST F06-VISUALIZZA
022700950721     C     KEY           WHENEQ    F21
022800950721     C                   EXSR      F21GES                                       *GEST F21-STAMPA
022900950721     C     KEY           WHENEQ    F22
023000950721     C                   EXSR      F22GES                                       *GESTIONE STAMPE
023100950721     C                   OTHER
023200950721     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
023300950721E   2C                   ENDSL
023400950721E   1C                   ENDIF
023500950721     C*
023600950721     C                   ENDSR
023700950721     C*------------------------------------------------------------------------*
023800950721     C* CARV1 - CARICA VIDEO RICHIESTA CODICE CLIENTE
023900950721     C*------------------------------------------------------------------------*
024000950721     C     CARV1         BEGSR
024100950721     C*
024200950721     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
024300950721     C*
024400950721     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
024500950721     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
024600950721     C*
024700020117     C                   WRITE     SE59DVT                                      *TESTATA
024800020117     C                   WRITE     SE59DVF                                      *FUNZIONI
024900950721     C*
025000950721     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
025100950721     C*
025200950721     C                   ENDSR
025300950721     C*------------------------------------------------------------------------*
025400950721     C* ENTV1 - GESTIONE TASTO ENTER VIDEO 1
025500950721     C*------------------------------------------------------------------------*
025600950721     C     ENTV1         BEGSR
025700950721     C*
025800950721     C* CONTROLLA I CAMPI IMMESSI
025900950721     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
026000950721     C*
026100950721     C                   ENDSR
026200950721     C*------------------------------------------------------------------------*
026300951005     C* F06GES - GEST. TASTO FUNS. F06 ==> VISUALIZZAZIONE STATISTICHE CLIENTE
026400950721     C*------------------------------------------------------------------------*
026500951005     C     F06GES        BEGSR
026600950721     C*
026700950721     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
026800950721IF  1C     $ERR          IFEQ      '0'                                          *SE NO ERRORI
026900950721     C*
027000950721     C* LANCIO PROGRAMMA  RICHIESTA PARAMETRI
027100000816     C                   MOVEL     'VIS'         WOP0                           *LIVELLO PROCEDURA
027200951005     C                   MOVEL     'F06'         WOP1                           *LIVELLO PGM
027300020117     C                   MOVEL     'TISE61R'     WPGM                           *PGM RICH PARAMETRI
027400950724     C*
027500950724     C* COMPATTA I CODICI CLIENTE SELEZIONATI PER PASSARLI NELLA DS
027600050117DO  1C**** 1             DO        30            C
027700050117DO  1C     1             DO        28            C
027800950724     C                   Z-ADD     *ZEROS        KSCNUM            7 0
027900950724IF  2C     VKSC(C)       IFGT      *ZEROS
028000950724     C                   MOVEL     VKSC(C)       KSCNUM
028100950724E   2C                   ENDIF
028200950724     C                   MOVEL     KSCNUM        DSKSN                          *SCH NUM --> DS  NUM
028300950724     C                   MOVE      DSKSA         KSA(C)                         *DS  ALF --> SCH ALF
028400950724E   1C                   ENDDO
028500950724     C                   MOVEA     KSA           WKSC                           SCH NUM PACK->CAMPON
028600950721     C*
028700950721     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
028800950721     C*
028900950721     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
029000950721E   1C                   ENDIF
029100950721     C*
029200950721     C                   ENDSR
029300950721     C*------------------------------------------------------------------------*
029400950721     C* F21GES - GEST. TASTO FUNZ. F21 ==> STAMPA STATISTICHE CLIENTE
029500950721     C*------------------------------------------------------------------------*
029600950721     C     F21GES        BEGSR
029700950721     C*
029800950721     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
029900950721IF  1C     $ERR          IFEQ      '0'                                          *SE NO ERRORI
030000950721     C*
030100950721     C* LANCIO PROGRAMMA  RICHIESTA PARAMETRI
030200000816     C                   MOVEL     'VIS'         WOP0                           *LIVELLO PROCEDURA
030300950721     C                   MOVEL     'F21'         WOP1                           *LIVELLO PGM
030400020117     C                   MOVEL     'TISE61R'     WPGM              8            *PGM RICH PARAMETRI
030500050117DO  1C**** 1             DO        30            C
030600050117DO  1C     1             DO        28            C
030700950724     C                   MOVEL     VKSC(C)       DSKSN                          *SCH NUM --> DS  NUM
030800950724     C                   MOVE      DSKSA         KSA(C)                         *DS  ALF --> SCH ALF
030900950724E   1C                   ENDDO
031000950724     C                   MOVEA     KSA           WKSC                           SCH NUM PACK->CAMPON
031100950721     C*
031200950721     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
031300950721     C*
031400950721     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA
031500950721E   1C                   ENDIF
031600950721     C*
031700950721     C                   ENDSR
031800950721     C*------------------------------------------------------------------------*
031900950721     C* F22GES -GEST.TASTO FUNZ. F22 ==> GESTIONE STAMPE PER EDP
032000950721     C*------------------------------------------------------------------------*
032100950721     C     F22GES        BEGSR
032200950721     C*
032300950721     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
032400950721     C                   Z-ADD     80            LENGH            15 5
032500950721     C                   CALL      'QCAEXEC'
032600950721     C                   PARM                    CMD(1)
032700950721     C                   PARM                    LENGH
032800950721     C*
032900950721     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
033000950721     C*
033100950721     C                   ENDSR
033200950721     C*------------------------------------------------------------------------*
033300950721     C* LANPGM - LANCIA PROGRAMMA RICHIESTA PARAMETRI
033400950721     C*------------------------------------------------------------------------*
033500950721     C     LANPGM        BEGSR
033600950721     C*
033700950721     C*--------------------
033800950721     C* IMPOSTO DS ESTERNA PROCEDURA
033900950721     C*--------------------
034000950721     C*
034100950721     C* PARAMETRI INFORMATIVI
034200950721     C                   MOVEL     WOP0          D60OP0                         *LIVELLO PROCEDURA
034300950721     C                   MOVEL     WOP1          D60OP1                         *LIVELLO PGM
034400950721     C                   MOVEL     '0'           D60F03                         PREMUTO F03
034500950721     C                   MOVEL     '0'           D60ERR                         '1'=ERRORE
034600950724     C                   MOVEL(P)  *BLANKS       D60MSG                         MESSAGGIO
034700950724     C                   CLEAR                   D60KSC
034800950724     C                   MOVEL     WKSC          D60KSC
034900950721     C*
035000950721     C* LANCIO PROGRAMMA
035100950724     C                   MOVEL     DS60          KPJBU
035200950721     C                   CALL      WPGM
035300950721     C                   PARM                    KPJBA
035400950724     C*
035500950724     C                   MOVEL     KPJBU         DS60
035600950721     C*
035700950721     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
035800950721IF  1C     D60F03        IFEQ      '1'
035900950721     C                   MOVEL     '1'           $F03
036000950721     C                   MOVEL     '1'           $FINE
036100950721X   1C                   ELSE
036200950721     C*
036300950721     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
036400950724     C                   MOVEL(P)  D60MSG        $MSG
036500950721     C                   EXSR      CARMSG
036600950721     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
036700950721E   1C                   ENDIF
036800950721     C*
036900950721     C                   ENDSR
037000950721     C*------------------------------------------------------------------------*
037100950721     C* DECODI - CONTROLLA CAMPI FORMATO VIDEO
037200950721     C*------------------------------------------------------------------------*
037300950721     C     DECODI        BEGSR
037400950721     C*
037500950721     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
037600950721     C                   SETOFF                                       505152
037700950721     C                   SETOFF                                       535455
037800950721     C                   SETOFF                                       565758
037900950721     C                   SETOFF                                       596061
038000950721     C                   SETOFF                                       626364
038100950721     C                   SETOFF                                       656667
038200950721     C                   SETOFF                                       686970
038300950721     C                   SETOFF                                       717273
038400950721     C                   SETOFF                                       747576
038500950721     C                   SETOFF                                       777879
038600950721     C                   CLEAR                   VKSC
038700950724     C                   CLEAR                   VKSD
038800950721     C*--------------------
038900950721     C* CODICE CLIENTE
039000950721     C*--------------------
039100000807     C*
039200000807     C* CODICE CLIENTE OBBLIGATORIO
039300000807IF  1C     $ERR          IFEQ      '0'                                          *ANCORA VALIDO
039400950721IF  2C     V1CKSC        IFEQ      *BLANKS
039500950721     C     V1CKSC        OREQ      *ZEROS
039600950721     C                   MOVEL     '1'           $ERR
039700000807     C                   MOVEL     MSG(8)        $MSG
039800950721     C                   EXSR      CARMSG
039900950721     C                   SETON                                        50
040000950721E   2C                   ENDIF
040100000807E   1C                   ENDIF
040200000807     C*
040300000807     C* RICHIESTA RICERCA CON IL ?
040400000807     C                   MOVEA     V1CKSC        VKSC
040500000807IF  1C     $ERR          IFEQ      '0'                                          *ANCORA VALIDO
040600950721     C                   SETOFF                                           28
040700950721     C     '?'           SCAN      V1CKSC                                 28
040800950721IF  2C     *IN28         IFEQ      *ON
040900950721     C                   CLEAR                   DSXC
041000020916     C                   Z-ADD     DUTKCI        DXCCAP
041100101014     C                   MOVEL     WAbi          DXCAUT
041200950721     C                   CALL      'XCLIR'
041300950721     C                   PARM                    DSXC
041400950721     C                   MOVEL     '1'           $TESVD
041500950724     C                   MOVEA     DXCKSC        KSA                            *DS INPUT -> SCH ALF
041600050117DO  3C**** 1             DO        30            J
041700050117DO  3C     1             DO        28            J
041800950724     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
041900950724     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
042000950724E   3C                   ENDDO
042100950721     C                   Z-ADD     1             J                 4 0
042200050117DO  3C**** 1             DO        30            J
042300050117DO  3C     1             DO        28            J
042400950724IF  4C     KSC(J)        IFNE      *ZEROS
042500000816     C                   MOVEL     '0'           $INC
042600950721     C                   Z-ADD     *ZEROS        C                 4 0
042700050117DO  5C**** C             DOWLT     30
042800050117DO  5C     C             DOWLT     28
042900950721     C                   ADD       1             C
043000000816IF  6C     VKSC(C)       IFNE      *BLANKS                                      *C'E' GIA' QC
043100000816     C     VKSC(C)       ANDNE     *ZEROS
043200000816     C                   SETOFF                                           28
043300000816     C     '?'           SCAN      VKSC(C)                                28
043400000816IF  7C     *IN28         IFEQ      *ON                                          *C'E' UN "?"
043500000816     C                   MOVEL     *BLANKS       VKSC(C)                        *AZZERA
043600000816E   7C                   ENDIF
043700000816X   6C                   ELSE                                                   *BUCO LIBERO
043800000816IF  7C     $INC          IFEQ      '0'                                          *MAI INSERITO
043900950721     C                   MOVEL     KSC(J)        VKSC(C)
044000000816     C                   MOVEL     '1'           $INC                           *GIA'INSERITO
044100000816E   7C                   ENDIF
044200000816E   6C                   ENDIF
044300950721E   5C                   ENDDO
044400950721E   4C                   ENDIF
044500950721E   3C                   ENDDO
044600950721E   2C                   ENDIF
044700000807E   1C                   ENDIF
044800000816     C*---               MOVEA     VKSC          V1CKSC
044900000807     C*
045000000807     C* LEGGE I CLIENTI RICHIESTI
045100000807IF  1C     $ERR          IFEQ      '0'                                          *ANCORA VALIDO
045200050117DO  2C**** 1             DO        30            C
045300050117DO  2C     1             DO        28            C
045400950721     C     C             ADD       49            IN                4 0
045500000807     C*
045600000807     C* CONTROLLA SE CLIENTE NUMERICO
045700000807IF  3C     VKSC(C)       IFGT      *BLANKS                                      *IMMESSO QC
045800000807     C                   MOVE      VKSC(C)       W001              1
045900000807     C                   SETOFF                                       28
046000000807     C                   TESTN                   VKSC(C)              28
046100000807IF  4C     *IN28         IFEQ      *OFF                                         *DA TESTN NON E'NUME
046200000807     C     W001          ORLT      '0'                                          *TROVATA UN LETTERA
046300000807     C                   MOVEL     '1'           $ERR
046400000807     C                   MOVEL     MSG(8)        $MSG
046500000807     C                   EXSR      CARMSG
046600000807     C                   MOVEL     '1'           *IN(IN)
046700000807     C                   LEAVE
046800000807E   4C                   ENDIF
046900000807E   3C                   ENDIF
047000000807     C*
047100000807     C* CONTROLLA SE CLIENTE VALIDO
047200000810     C                   MOVEL     *BLANKS       VKSD(C)                        *AZZERA DESCRIZIONE
047300000810IF  3C     VKSC(C)       IFGT      *ZEROS                                       *IMMESSO QC
047400000810     C                   CLEAR                   BS69DS
047500000810     C                   CLEAR                   ACODS
047600000810     C                   MOVEL     KNSIF         I69SIF
047700000810     C                   MOVEL     VKSC(C)       I69KAC
047800000810     C                   CALL      'TIBS69R'
047900000810     C                   PARM                    BS69DS
048000000810     C                   PARM                    ACODS
048100000810     C                   PARM                    INDDS
048200000810     C                   PARM                    CLPDS
048300000810     C                   PARM                    CLSDS
048400000810IF  4C     O69ERR        IFNE      '1'
048500000811     C                   MOVEL     ACORAG        VKSD(C)                        *IMPOSTA DESCRIZIONE
048600000810X   4C                   ELSE
048700000810     C                   MOVEL     '1'           $ERR
048800000810     C                   MOVEL     MSG(8)        $MSG
048900000810     C                   EXSR      CARMSG
049000000810     C                   MOVEL     '1'           *IN(IN)
049100000810     C                   LEAVE
049200000810E   4C                   ENDIF
049300000807     C*
049400101011      * CONTROLLA SE PUÒ ESSERE VISTO OPPURE NO CON ABILITAZIONI ANCHE X FUORI AREA
049500101011      * MA PER AGENTE UNIFICANTE:
049600101011     C                   clear                   TAA1DS
049700101011     C                   EVAL        ITAA1CAUT = '§UTEIST'
049800101011     C                   move      VKSC(C)       ITAA1KSC
049900101011     C                   move      'N'           ITAA1TIPO
050000101011     C                   EVAL      KPJBU = TAA1DS
050100101011     C                   CALL      'TNTAA1C'
050200101011     C                   PARM                    KPJBA
050300101011     C                   EVAL              TAA1DS = KPJBU
050400101011      *
050500101011      *  SE NON ABILITATO PROCEDO CON IL VECCHIO CONTROLLO SULLA FILIALE GESTITA IN LISTA
050600101011     C                   IF        OTAA1FABI <> 'S'
050700101011     C                   MOVEL     '1'           $ERR
050800101011     C                   MOVEL     MSG(9)        $MSG
050900101011     C                   EXSR      CARMSG
051000101011     C                   MOVEL     '1'           *IN(IN)
051100101011     C                   LEAVE
051200101014e   2C                   Else
051300101014x   2C                   eval      wABI =  OTAA1CABI
051400101011     c                   end
051500101011      *
051600000807     C* CONTROLLA SE CLIENTE GESTIBILE:
051700101011     C*********          MOVEL     VKSC(C)       WAFIL             3
051800101011     C*********          SETOFF                                           28
051900101011     C**** WAFIL         LOOKUP    $POG                                   28
052000101011IF  4C**** *IN28         IFEQ      *OFF                                         *CLIENTE NON DELL'AR
052100101011     C*********          MOVEL     '1'           $ERR
052200101011     C*********          MOVEL     MSG(9)        $MSG
052300101011     C*********          EXSR      CARMSG
052400101011     C*********          MOVEL     '1'           *IN(IN)
052500101011     C*********          LEAVE
052600101011E   4C*********          ENDIF
052700000807E   3C                   ENDIF
052800020916     C*
052900000807E   2C                   ENDDO                                                  *FINE CLIENTI
053000000807E   1C                   ENDIF
053100950721     C                   MOVEA     VKSC          V1CKSC
053200950721     C*
053300950721     C* DECODIFICHE
053400950724IF  1C     $ERR          IFEQ      '0'
053500950724     C                   MOVEL     VKSD(1)       V1DKS1
053600950724     C                   MOVEL     VKSD(2)       V1DKS2
053700950724     C                   MOVEL     VKSD(3)       V1DKS3
053800950724     C                   MOVEL     VKSD(4)       V1DKS4
053900950724     C                   MOVEL     VKSD(5)       V1DKS5
054000950724     C                   MOVEL     VKSD(6)       V1DKS6
054100950724     C                   MOVEL     VKSD(7)       V1DKS7
054200950724     C                   MOVEL     VKSD(8)       V1DKS8
054300950724     C                   MOVEL     VKSD(9)       V1DKS9
054400950724     C                   MOVEL     VKSD(10)      V1DK10
054500950724     C                   MOVEL     VKSD(11)      V1DK11
054600950724     C                   MOVEL     VKSD(12)      V1DK12
054700950724     C                   MOVEL     VKSD(13)      V1DK13
054800950724     C                   MOVEL     VKSD(14)      V1DK14
054900950724     C                   MOVEL     VKSD(15)      V1DK15
055000950724     C                   MOVEL     VKSD(16)      V1DK16
055100950724     C                   MOVEL     VKSD(17)      V1DK17
055200950724     C                   MOVEL     VKSD(18)      V1DK18
055300950724     C                   MOVEL     VKSD(19)      V1DK19
055400950724     C                   MOVEL     VKSD(20)      V1DK20
055500950724     C                   MOVEL     VKSD(21)      V1DK21
055600950724     C                   MOVEL     VKSD(22)      V1DK22
055700950724     C                   MOVEL     VKSD(23)      V1DK23
055800950724     C                   MOVEL     VKSD(24)      V1DK24
055900950724     C                   MOVEL     VKSD(25)      V1DK25
056000950724     C                   MOVEL     VKSD(26)      V1DK26
056100950724     C                   MOVEL     VKSD(27)      V1DK27
056200950724     C                   MOVEL     VKSD(28)      V1DK28
056300050117     C****               MOVEL     VKSD(29)      V1DK29
056400050117     C****               MOVEL     VKSD(30)      V1DK30
056500950724E   1C                   ENDIF
056600950721     C*
056700950721     C                   ENDSR
056800000810     C*------------------------------------------------------------------------*
056900000810     C* FINSR - OPERAZIONI FINALI
057000000810     C*------------------------------------------------------------------------*
057100000810     C     FINSR         BEGSR
057200000810     C*
057300000810     C* CHIUDE I FILE DEI PGM CHIAMATI
057400000810     C                   CLEAR                   BS69DS
057500000810     C                   CLEAR                   ACODS
057600000810     C                   CLEAR                   INDDS
057700000810     C                   CLEAR                   CLPDS
057800000810     C                   CLEAR                   CLSDS
057900000810     C                   MOVEL     'C'           I69TLA
058000000810     C                   MOVEL     KNSIF         I69SIF
058100000810     C                   CALL      'TIBS69R'
058200000810     C                   PARM                    BS69DS
058300000810     C                   PARM                    ACODS
058400000810     C                   PARM                    INDDS
058500000810     C                   PARM                    CLPDS
058600000810     C                   PARM                    CLSDS
058700000810     C*
058800000810     C                   ENDSR
058900950721     C*------------------------------------------------------------------------*
059000950721     C* *INZSR - ROUTINE INIZIALE
059100950721     C*------------------------------------------------------------------------*
059200950721     C     *INZSR        BEGSR
059300950721     C*--------------------
059400950721     C* RICEVIMENTO PARAMETRI
059500950721     C*--------------------
059600950721     C     *ENTRY        PLIST
059700950721     C                   PARM                    KPJBA
059800950726     C                   MOVEL     KPJBU         DS60
059900950721     C*--------------------
060000950721     C* VARIABILI RIFERITE AL DATA BASE
060100950721     C*--------------------
060200950721     C     *LIKE         DEFINE    D60OP0        WOP0                           *DS60
060300950721     C     *LIKE         DEFINE    D60OP1        WOP1
060400950721     C     *LIKE         DEFINE    D60KSC        WKSC
060500020905     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
060600000816     C*--------------------
060700000816     C* VARIABILI DI CONTROLLO
060800000816     C*--------------------
060900000816     C                   MOVEL     '0'           $INC              1
061000950721     C*
061100950721     C* ESEGUE OPERAZIONI INIZIALI STANDARD
061200950721     C                   EXSR      INZSTD
061300020916     C*
061400020916     C* REPERISCE I DATI UTENTE
061500020916     C                   EXSR      REPDATIUTE
061600020916     C*
061700020916     C* ASSEGNA LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO VIDEO
061800020916     C                   MOVEL(P)  RSUT          DSFIRS
061900950721     C*
062000950721     C* EMETTO FORMATO VIDEO
062100950721     C                   EXSR      CARV1
062200950721     C*
062300950721     C                   ENDSR
062400020916     C*--------------------------------------------------------------------------------------------*
062500020916     C* REPERISCE DATI UTENTE
062600020916     C*--------------------------------------------------------------------------------------------*
062700020916     C     REPDATIUTE    BEGSR
062800020916     C*
062900020916     C* REimposta le variabili di lavoro
063000020916     C                   CLEAR                   TIBS34DS
063100020916     C                   CLEAR                   AZUTEDS
063200020916     C                   CLEAR                   DDATIUTE
063300020916     C                   CLEAR                   WAbi              2
063400020916     C                   CLEAR                   DLAT
063500020916     C*
063600020916     C* Reperisce i dati del profilo in ingresso
063700020916     C     *DTAARA       DEFINE    §azute        azuteds
063800020916     C     *DTAARA       DEFINE    §datiute      ddatiute
063900020916     C                   IN(E)     *DTAARA
064000020916if  1C                   IF        %Error                                       *NO errori
064100020916     c                   EVAL      I34Tla = 'L'
064200020916     C                   CALL      'TIBS34R'
064300020916     C                   PARM                    Tibs34Ds
064400020916     C                   IN        *DTAARA
064500020916e   1C                   ENDIF
064600020916     C*
064700020916     C* Se ci sono degli errori sull'abilitazione utente, esce
064800101011sl  1***C                   SELECT
064900101011w   2***C                   WHEN      DUTERR = 'E'
065000101011     ***C                   EVAL      $ERR   = '1'
065100101011     ***C                   EVAL      $FINE  = '1'
065200101011     ***C*
065300101011     ***C* NON siamo in sede (filiale "là" o filiale "qua")
065400101011     ***C* --> se 1° livello, abilitazioni al terminal
065500101011     ***C*     se 2° livello, abilitazioni al punto operativo
065600101011w   2***C                   WHEN      UTEAUT = *blank
065700101011if  3***C                   IF        DUTLPO = '1'                                 *1° LIVELLO
065800101011     ***c                   EVAL      WAbi = 'TP'
065900101011e   3***C                   ENDIF
066000101011if  3***C                   IF        DUTLPO = '2'                                 *2° LIVELLO
066100101011     ***C                   EVAL      WAbi = 'PO'
066200101011e   3***C                   ENDIF
066300101011     ***C*
066400101011     ***C* siamo in sede
066500101011     ***C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
066600101011     ***C*     carica le abilitazioni proprie del profilo
066700101011slx 1***C                   OTHER
066800101011     ***C                   MOVEL     UTEFAF        DUTE01
066900101011if  2***C                   IF        §UTEIst <> *blanks                           *esiste specifica
067000101011     ***C                   EVAL      WAbi = §UTEIst
067100101011x   2***C                   ELSE                                                   *NON esiste specifica
067200101011     ***C                   EVAL      wabi = UTEAUT
067300101011e   2***C                   ENDIF
067400101011e   1***C                   ENDSL
067500101011     ***C*
067600101011     ***C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
067700101011     ***C                   CLEAR                   tibs02ds
067800101011     ***C                   MOVEL     'C'           t02mod
067900101011     ***C                   MOVEL     knsif         t02sif
068000101011     ***C                   MOVEL     'LAT'         t02cod
068100101011     ***C                   MOVEL(P)  Wabi          t02ke1
068200101011     ***C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
068300101011     ***C                   CALL      'TIBS02R'
068400101011     ***C                   PARM                    kpjba
068500101011     ***C                   PARM                    TIBS02DS
068600101011     ***C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
068700101011if  1***C                   IF        t02err = *blanks
068800101011     ***C                   MOVEL     t02uni        DLat
068900101011e   1***C                   ENDIF
069000101011if  1***C                   IF        T02err <> *blanks OR
069100101011     ***C                             §LATabi = 'S'
069200101011     ***C                   EVAL      $ERR   = '1'
069300101011     ***C                   EVAL      $FINE  = '1'
069400101011e   1***C                   ENDIF
069500101011     ***C*
069600101011     ***C* Reperisce i P.O. gestibili dall'utente
069700101011if  1***C                   IF        $ERR <> '1'                                  *NO errori
069800101011     ***C                   CLEAR                   TRUL31DS
069900101011     ***C                   EVAL      I31abi = WAbi
070000101011     ***C                   EVAL      I31cdi = DUTdis
070100101011     ***C                   EVAL      I31car = DUTare
070200101011     ***C                   EVAL      I31cpo = DUTpou
070300101011     ***C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
070400101011     ***C                   CALL      'TRUL31R'
070500101011     ***C                   PARM                    KPJBA
070600101011     ***C                   PARM                    TRUL31DS
070700101011     ***C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
070800101011if  2***C                   IF        O31pog > *zeros
070900101011     ***C                   MOVEA     O31pog        $POG
071000101011x   2***C                   ELSE
071100101011     ***C                   EVAL      $ERR  = '1'
071200101011     ***C                   EVAL      $FINE = '1'
071300101011e   2***C                   ENDIF
071400101011e   1***C                   ENDIF
071500101011     ***
071600101011      ******
071700101011      ****** La routine TNTAA2R  sostituisce le specifiche precedenti asteriscate
071800101011      ******  con asterischi davanti al tipo "C" di specifica di calcolo.
071900101011     C                   clear                   TAA1DS
072000101011     C                   EVAL        ITAA1CAUT = '§UTEIST'
072100101011     C                   move      'N'           ITAA1TIPO
072200101011     C                   EVAL      KPJBU = TAA1DS
072300101011     C                   CALL      'TNTAA1C'
072400101011     C                   PARM                    KPJBA
072500101011     C                   EVAL              TAA1DS = KPJBU
072600101011      *
072700101011x   2C                   if         OTAA1ERR ='S'
072800101011     C                   EVAL      $ERR  = '1'
072900101011     C                   EVAL      $FINE = '1'
073000101011e   2C                   ENDIF
073100101011     C*
073200020916     C                   ENDSR
073300950721     C*------------------------------------------------------------------------*
073400950721     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073500950721     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073600950721     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
073700950721     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
073800950721     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
073900950721     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074000950721     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074100950721     C*------------------------------------------------------------------------*
074200950721     C* INZSTD - ROUTINE INIZIALE STANDARD
074300950721     C*------------------------------------------------------------------------*
074400950721     C     INZSTD        BEGSR
074500950721     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
074600950721     C                   CALL      'XKEYS'
074700950721     C                   PARM                    DSKEYS
074800950721     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
074900950721     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
075000950721     C     XMSGPA        PLIST
075100950721     C                   PARM                    DSMSMS
075200950721     C                   PARM                    DSMSPG
075300950721     C                   PARM                    DSMSKE
075400950721     C                   PARM                    DSMSER
075500950721     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
075600020117     C                   MOVEL     'TISE59R'     DSMSPG                         *NOME PGM CHIAMANTE
075700020117     C                   MOVEL     'TISE59R'     VMCQUE                         *CODA MESSAGGI DESTI
075800950721     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
075900950721     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
076000950721     C* INIZIALIZZO FLAG CONTROLLO PGM
076100950721     C                   MOVEL     '0'           $ERR              1            *ERRORE
076200950721     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
076300950721     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
076400950721     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
076500950721     C                   MOVEL     '0'           $FINE             1            *FINE PGM
076600950721     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
076700950721     C                   MOVEL     'I'           $TGES             1            *I=INTERROG,G=GESTIO
076800020905     C*
076900950721     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
077000950721     C                   BITOFF    '01234567'    POSMI1            1
077100950721     C                   BITOFF    '01234567'    POSMI2            1
077200950721     C                   BITOFF    '01234567'    POSMA1            1
077300950721     C                   BITOFF    '01234567'    POSMA2            1
077400950721     C                   BITON     '34'          POSMI1            1            *18
077500950721     C                   BITON     '7'           POSMI2            1            *01
077600950721     C                   BITON     '34'          POSMA1            1            *18
077700950721     C                   BITON     '0'           POSMA2            1            *80
077800950721     C                   MOVE      POSMI2        POSMIN            2
077900950721     C                   MOVEL     POSMI1        POSMIN
078000950721     C                   MOVE      POSMA2        POSMAX            2
078100950721     C                   MOVEL     POSMA1        POSMAX
078200950721     C                   ENDSR
078300950721     C*------------------------------------------------------------------------*
078400950721     C* FUNSTD - LANCIA SUBROUTINE STANDARD
078500950721     C*------------------------------------------------------------------------*
078600950721     C     FUNSTD        BEGSR
078700950721     C                   SELECT
078800950721     C     KEY           WHENEQ    F3
078900950721     C                   EXSR      F03STD
079000950721     C     KEY           WHENEQ    F12
079100950721     C                   EXSR      F12STD
079200950721     C     KEY           WHENEQ    F24
079300950721     C                   EXSR      F24STD
079400950721     C                   OTHER
079500950721     C                   MOVEL     MSG(2)        $MSG
079600950721     C                   EXSR      CARMSG
079700950721     C                   ENDSL
079800950721     C                   ENDSR
079900950721     C*------------------------------------------------------------------------*
080000950721     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
080100950721     C*------------------------------------------------------------------------*
080200950721     C     F03STD        BEGSR
080300950721     C*
080400950721     C* IMPOSTO FLAG DI F03 USCITA PGM
080500950721     C                   MOVEL     '1'           $F03
080600950721     C*
080700950721     C* IMPOSTO FLAG DI FINE PROGRAMMA
080800950721     C                   MOVEL     '1'           $FINE
080900950721     C*
081000950721     C                   ENDSR
081100950721     C*------------------------------------------------------------------------*
081200950721     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
081300950721     C*------------------------------------------------------------------------*
081400950721     C     F12STD        BEGSR
081500950721     C*
081600950721     C* IMPOSTO FLAG DI FINE PROGRAMMA
081700950721     C                   MOVEL     '1'           $FINE                          *FINE PGM
081800950721     C*
081900950721     C* IMPOSTO FLAG DI FINE PROGRAMMA
082000950721     C*--------------------MOVEL'V1'      $GES             *EMISSIONE 1° VIDEO
082100950721     C*
082200950721     C                   ENDSR
082300950721     C*------------------------------------------------------------------------*
082400950721     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
082500950721     C*------------------------------------------------------------------------*
082600950721     C     F24STD        BEGSR
082700950721     C*
082800950721     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
082900950721     C                   EXSR      OUTFUN
083000950721     C*
083100950721     C                   ENDSR
083200950721     C*------------------------------------------------------------------------*
083300950721     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
083400950721     C*          CODA MESSAGGI DEL PGM
083500950721     C*------------------------------------------------------------------------*
083600950721     C     CARMSG        BEGSR
083700950721     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
083800950721     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
083900950721     C     MSGULT        IFEQ      0
084000950721     C                   MOVEL     '1'           *IN40
084100020117     C                   WRITE     SE59DVK
084200950721     C                   MOVEL     '0'           *IN40
084300950721     C                   ENDIF
084400950721     C* INVIA MESSAGGIO ALLA CODA DEL PGM
084500950721     C                   MOVEL     $MSG          DSMSMS
084600950721     C                   CALL      'XMSG'        XMSGPA
084700950721     C     DSMSER        IFEQ      0
084800950721     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
084900950721     C                   ENDIF
085000950721     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
085100950721     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
085200020117     C                   WRITE     SE59DVM                                      *SFL MESS
085300950721     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
085400950721     C                   MOVEL     *BLANKS       $MSG
085500950721     C                   ENDSR
085600950721     C*------------------------------------------------------------------------*
085700950721     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
085800950721     C*------------------------------------------------------------------------*
085900950721     C     OUTMSG        BEGSR
086000950721     C* VISUALIZZAZIONE MESSAGGIO
086100950721     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
086200020117     C                   WRITE     SE59DVK                                      *CONTR MESS
086300950721     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
086400950721     C                   Z-ADD     *ZEROS        MSGULT
086500950721     C                   ENDSR
086600950721     C*------------------------------------------------------------------------*
086700950721     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
086800950721     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
086900950721     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
087000950721     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
087100950721     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
087200950721     C*------------------------------------------------------------------------*
087300950721     C     CARFUN        BEGSR
087400950721     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
087500950721     C     1             DO        31            I                 5 0          --- 1 -->
087600950721     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
087700950721     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
087800950721     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
087900020905     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
088000950721     C     $TGES         IFEQ      'G'                                          *GESTIONE
088100950721     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
088200950721     C                   ELSE                                                   *INTERROG.
088300950721     C                   MOVEL     FUNASI        FAT(I)
088400950721     C                   ENDIF
088500950721     C                   ELSE                                                   <-- 2 --> *FILIALE
088600950721     C     $TGES         IFEQ      'G'                                          *GESTIONE
088700950721     C                   MOVEL     FUNAFG        FAT(I)
088800950721     C                   ELSE                                                   *INTERROG.
088900950721     C                   MOVEL     FUNAFI        FAT(I)
089000950721     C                   ENDIF
089100950721     C                   ENDIF                                                  <-- 2 ---
089200950721     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
089300950721     C                   MOVEL     USER          WEDP              3            *PROFILO
089400950721     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
089500950721     C                   MOVEL     'S'           FAT(I)                         *ABILITO
089600950721     C                   ELSE
089700950721     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
089800950721     C                   ENDIF
089900950721     C                   ENDIF
090000950721     C                   ENDDO                                                  <-- 1 ---
090100950721     C                   ENDSR
090200950721     C*------------------------------------------------------------------------*
090300950721     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
090400950721     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
090500950721     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
090600950721     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
090700950721     C*------------------------------------------------------------------------*
090800950721     C     OUTFUN        BEGSR
090900950721     C     1             DO        8             I                              *PULIZIA FUNZIONI A
091000950721     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
091100950721     C                   ENDDO
091200950721     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
091300950721     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
091400950721     C     1             DO        31            I
091500950721     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
091600950721     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
091700950721     C                   ADD       1             FUNNAT
091800950721     C                   ENDIF
091900950721     C                   ENDDO
092000950721     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
092100950721     C     FUNNAT        IFGT      8
092200950721     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
092300950721     C                   ENDIF
092400950721     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
092500950721     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
092600950721     C                   Z-ADD     8             FUNVIS            5 0
092700950721     C     FAT(23)       IFEQ      'S'
092800950721     C                   SUB       1             FUNVIS
092900950721     C                   ENDIF
093000950721     C     FAT(24)       IFEQ      'S'
093100950721     C                   SUB       1             FUNVIS
093200950721     C                   ENDIF
093300950721     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
093400950721     C                   Z-ADD     *ZEROS        W                 5 0
093500950721     C                   ADD       1             FUNULT
093600950721     C     FUNULT        IFGT      31
093700950721     C                   Z-ADD     1             FUNULT
093800950721     C                   ENDIF
093900950721     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
094000950721     C     FUNULT        DO        31            I                              --- 1 -->
094100950721     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
094200950721     C     W             IFLT      FUNVIS                                       --- 2 -->
094300950721     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
094400950721     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
094500950721     C     FDE(I)        ANDNE     *BLANKS
094600950721     C     I             IFEQ      23                                           *TOLGO F23
094700950721     C     I             OREQ      24                                           *E F24
094800950721     C                   ELSE
094900950721     C                   ADD       1             W
095000950721     C                   MOVEL     FDE(I)        FUV(W)
095100950721     C                   ENDIF
095200950721     C                   ENDIF                                                  <-- 3 ---
095300950721     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
095400950721     C                   Z-ADD     I             FUNULT
095500950721     C                   ENDIF                                                  <-- 2 ---
095600950721     C                   ENDDO                                                  <-- 1 ---
095700950721     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
095800950721     C                   MOVEL     FUV(1)        VFD001
095900950721     C                   MOVEL     FUV(2)        VFD002
096000950721     C                   MOVEL     FUV(3)        VFD003
096100950721     C                   MOVEL     FUV(4)        VFD004
096200950721     C                   MOVEL     FUV(5)        VFD005
096300950721     C                   MOVEL     FUV(6)        VFD006
096400950721     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
096500950721     C     FAT(24)       ANDEQ     'N'
096600950721     C                   MOVEL     FUV(7)        VFD007
096700950721     C                   MOVEL     FUV(8)        VFD008
096800950721     C                   ELSE                                                   <-- 1 -->
096900950721     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
097000950721     C     FAT(24)       ANDEQ     'S'
097100950721     C                   MOVEL     FDE(23)       VFD007
097200950721     C                   MOVEL     FDE(24)       VFD008
097300950721     C                   ELSE                                                   <-- 2 -->
097400950721     C                   MOVEL     FUV(7)        VFD007
097500950721     C     FAT(23)       IFEQ      'S'
097600950721     C                   MOVEL     FDE(23)       VFD008
097700950721     C                   ELSE
097800950721     C                   MOVEL     FDE(24)       VFD008
097900950721     C                   ENDIF
098000950721     C                   ENDIF                                                  <-- 2 ---
098100950721     C                   ENDIF                                                  <-- 1 ---
098200950721     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
098300950721     C                   ENDSR
098400950721     C*------------------------------------------------------------------------*
098500950721     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
098600950721     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
098700950721     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
098800950721     C*                   - ERRORE SE FUNZIONE NON ATTIVA
098900950721     C*------------------------------------------------------------------------*
099000950721     C     CONFUN        BEGSR
099100950721     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
099200950721     C     1             DO        31            I                              --- 1 -->
099300950721     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
099400950721     C* CONTROLLO SE IL TASTO E' ATTIVO
099500950721     C     FAT(I)        IFNE      'S'                                          --- 3 -->
099600950721     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
099700950721     C                   EXSR      CARMSG
099800950721     C                   MOVEL     '1'           $ERR
099900950721     C                   ENDIF                                                  <-- 3 ---
100000950721     C                   ENDIF                                                  <-- 2 ---
100100950721     C                   ENDDO                                                  <-- 1 ---
100200950721     C                   ENDSR
100300950721**   SCHIERA: MSG  (MESSAGGI)
100400020117TISE59R- Tasto funzionale non valido.                                          1
100500020117TISE59R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
100600020117TISE59R- Opzione non valida.                                                   3
100700020117TISE59R- Opzione abilitata ma non gestita. Avvertire CED.                      4
100800020117TISE59R- Inizio elenco.                                                        5
100900020117TISE59R- Fine elenco.                                                          6
101000020117TISE59R- 7
101100020117TISE59R- Cliente errato                                                        8
101200020916TISE59R- Cliente non gestibile                                                 9
101300020117TISE59R- 10
101400020117TISE59R- 11
101500020117TISE59R- 12
101600020117TISE59R- 13
101700020117TISE59R- 14
101800020117TISE59R- 15
101900020117TISE59R- 16
102000020117TISE59R- 17
102100020117TISE59R- 18
102200020117TISE59R- 19
102300020117TISE59R- 20
102400950721**   SCHIERA: CMD  (COMANDI)
102500950721WRKSPLF
102600950721
102700950721
102800950721
102900950721
103000950721** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
103100950721F1    -                   -    NN     NN
103200950721F2    -                   -    NN     NN
103300950721F3    -F3=Fine            -    SS     SS
103400950721F4    -                   -    NN     NN
103500951005F5    -                   -    NN     NN
103600951005F6    -F6=Visualizza      -    SS     SS
103700950721F7    -                   -    NN     NN
103800950721F8    -                   -    NN     NN
103900950721F9    -                   -    NN     NN
104000950721F10   -                   -    NN     NN
104100950721F11   -                   -    NN     NN
104200950721F12   -F12=Ritorno        -    SS     SS
104300950721F13   -                   -    NN     NN
104400950721F14   -                   -    NN     NN
104500950721F15   -                   -    NN     NN
104600950721F16   -                   -    NN     NN
104700950721F17   -                   -    NN     NN
104800950721F18   -                   -    NN     NN
104900950721F19   -                   -    NN     NN
105000950721F20   -                   -    NN     NN
105100951005F21   -F21=Stampa         -    SS     SS
105200951005F22   -F22=Gestione stampe-    EE     EE
105300950721F23   -                   -    NN     NN
105400950721F24   -F24=Altri tasti    -    NN     NN
105500950721ENTER -                   -    SS     SS
105600950721ROLLUP-                   -    NN     NN
105700950721ROLLDN-                   -    NN     NN
105800950721HELP  -                   -    NN     NN
105900950721PRINT -                   -    NN     NN
106000950721BSPACE-                   -    NN     NN
106100950721CLEAR -                   -    NN     NN
