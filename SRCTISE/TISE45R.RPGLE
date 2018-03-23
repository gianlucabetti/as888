000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA MOVIMENTO - RICHIESTA PARAMETRI                             *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800020115     FTISE45D   CF   E             WORKSTN INFDS(DEVDS1)
000900020115     F                                     SFILE(SE45DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FTABEL00F  IF   E           K DISK
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* SCHIERE GESTIONE VIDEO
001700000000     D*-------------------
001800000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
001900000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002000000000     D*-------------------
002100000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200000000     D*-------------------
002300000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002400000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZIONE
002500000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002600000000     D*-------------------
002700000000     D* TABELLA DESCRIZIONI A VIDEO
002800000000     D*-------------------
002900000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003000000000     D*-------------------
003100000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003200000000     D*-------------------
003300000000     D KEYS            S              1    DIM(40)
003400000000     D*------------------------------------------------------------------------*
003500000000     D* INPUT
003600000000     D*------------------------------------------------------------------------*
003700000000     D*-------------------
003800000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003900000000     D*-------------------
004000000000     D DSFUN         E DS
004100000000     D  FUNRIG                 1     40
004200000000     D*-------------------
004300000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004400000000     D*-------------------
004500000000     D DSMSG         E DS
004600000000     D*-------------------
004700000000     D* DS PER GESTIONE TASTI FUNZIONALI
004800000000     D*-------------------
004900000000     D DSKEYS        E DS
005000000000     D  KEYRIG                 1     40
005100000000     D*-------------------
005200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005300000000     D*-------------------
005400000000     D                SDS
005500000000     D  USER                 254    263
005600000000     D*-------------------
005700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005800000000     D*-------------------
005900000000     D DEVDS1          DS
006000000000     D  KEY                  369    369
006100000000     D  CURSOR               370    371
006200000000     D*-------------------
006300000000     D* ARCHITETTURA
006400000000     D*-------------------
006500000000     D KPJBA         E DS
006600000000     D*------------------
006700000000     D* PARAMETRI DI LANCIO
006800000000     D*------------------
006900000000     D PARAM           DS                  INZ
007000160422     d  param2                 1     27
007100990527     D  PARDAT                 1      6  0
007200990527     D  PARDA2                 7     12  0
007300990527     D  PARMPA                13     13
007400990527     D  PARFIL                14     16  0
007500990527     D  PARARE                17     19  0
007600990527     D  PARDIV                20     20
007700990527     D  PARSER                21     21
007800990527     D  PARPOR                22     22
007900990527     D  PARSPE                23     23
008000990527     D  PARPRO                24     24
008100990527     D  PARDET                25     25
008200990527     D  PARFPC                26     26
008300990527     D  PARIOE                27     27
008400000703     D  PARPOS                28     28
008500000000     D*-------------------
008600020916     D* DS REPERIMENTO DATI UTENTE
008700000000     D*-------------------
008800020916     D TIBS34DS      E DS                                                       *Profili utente
008900020916     D DDATIUTE      E DS                                                       *Dati utente
009000020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009100000000     D*-------------------
009200000000     D* DS RAGGRUPPAMENTI BOLLA
009300000000     D*-------------------
009400000000     D DS2W          E DS
009500990527     D*------------------
009600990527     D* DS CONTROLLO DATA (8)
009700990527     D*------------------
009800990527     D WLBDA8          DS                  INZ
009900990527     D  G08DAT                 1      8  0
010000990527     D  G08INV                 9     16  0
010100990527     D  G08ERR                17     17
010200990527     D  G08TGI                18     22  0
010300990527     D*------------------
010400990527     D* DS DATA GENERICA(8) IN GG/MM/AAAA
010500990527     D*------------------
010600990527     D                 DS
010700990527     D  DGIOR8                 1      2  0
010800990527     D  DMESE8                 3      4  0
010900990527     D  DANNO8                 5      8  0
011000990527     D  DDATA8                 1      8  0
011100000000     C*------------------------------------------------------------------------*
011200000000     C* MAIN LINE
011300000000     C*------------------------------------------------------------------------*
011400000000     C*
011500000000     C* CICLO FINO A CHE:
011600000000DO  1C     $FINE         DOUEQ     '1'
011700000000     C*
011800000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
011900000000     C*
012000000000     C* CONTROLLA GESTIONE VIDEO
012100000000IF  2C     $GES          IFEQ      'V1'
012200000000     C                   EXSR      GESV1                                        *PARAMETRI
012300000000E   2C                   ENDIF
012400000000     C*
012500000000E   1C                   ENDDO
012600000000     C*
012700000000     C                   SETON                                        LR
012800000000     C*------------------------------------------------------------------------*
012900000000     C* GESV1 - GESTIONE PRIMO VIDEO
013000000000     C*------------------------------------------------------------------------*
013100000000     C     GESV1         BEGSR
013200000000     C*
013300000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
013400000000     C                   EXSR      CARV1                                        *CARICA VIDEO
013500000000E   1C                   ENDIF
013600000000     C*
013700000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
013800000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
013900000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
014000000000E   1C                   ENDIF
014100000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
014200000000     C*
014300020115     C                   EXFMT     SE45DV1                                      *PARAMETRI
014400000000     C*
014500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
014600000000     C*
014700000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
014800000000SL  2C                   SELECT
014900000000     C     KEY           WHENEQ    ENTER
015000000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015100000000     C     KEY           WHENEQ    F6
015200000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
015300000000     C                   OTHER
015400000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015500000000E   2C                   ENDSL
015600000000E   1C                   ENDIF
015700000000     C*
015800000000     C                   ENDSR
015900000000     C*------------------------------------------------------------------------*
016000000000     C* CARV1 - CARICA PRIMO VIDEO
016100000000     C*------------------------------------------------------------------------*
016200000000     C     CARV1         BEGSR
016300000000     C*
016400000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016500000000     C*
016600000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016700000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016800000000     C*
016900020115     C                   WRITE     SE45DVT                                      *TESTATA
017000020115     C                   WRITE     SE45DVF                                      *FUNZIONI
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*------------------------------------------------------------------------*
017400000000     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
017500000000     C*------------------------------------------------------------------------*
017600000000     C     ENTV1         BEGSR
017700000000     C*
017800000000     C* CONTROLLA I CAMPI IMMESSI
017900000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018000000000     C*
018100000000     C                   ENDSR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
018400000000     C*------------------------------------------------------------------------*
018500000000     C     F06GES        BEGSR
018600000000     C*
018700000000     C* CONTROLLA I CAMPI IMMESSI
018800000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018900000000     C*
019000000000     C* NON CI SONO ERRORI
019100000000IF  1C     $ERR          IFEQ      '0'
019200000000     C*
019300000000     C* LANCIO PROGRAMMA DI STAMPA
019400000000     C                   Z-ADD     V1CDAT        PARDAT
019500000000     C                   Z-ADD     V1CDA2        PARDA2
019600000000     C                   MOVEL     V1CMPA        PARMPA
019700000000     C                   MOVEL     V1CSER        PARSER
019800000000     C                   MOVEL     V1CPOR        PARPOR
019900000000     C                   MOVEL     V1CSPE        PARSPE
020000000000     C                   MOVEL     V1CPRO        PARPRO
020100000000     C                   MOVEL     V1CDET        PARDET
020200000000     C                   MOVEL     V1CFPC        PARFPC
020300950526     C                   MOVEL     V1CIOE        PARIOE
020400160422     C                   MOVEL     'N'           PARPOS
020500000000     C*
020600020115     C                   MOVEL     'SE46'        KCOAZ
020700000000     C                   MOVEL     PARAM         KPJBU
020800020408 MOM C                   CALL      'BCH10'
020900020408     C*                  CALL      'TISE46C'
021000000000     C                   PARM                    KPJBA
021100000000E   1C                   ENDIF
021200160914     C*m                 if        v1csta = 'S'
021300160914     C*m                 MOVEL     'SE4C'        KCOAZ
021400160914     C*m                 MOVEL     PARAM2        KPJBU
021500160914 MOM C*m                 CALL      'BCH10'
021600160914     C*m                 PARM                    KPJBA
021700160914E   1C*m                 ENDIF
021800000000     C*
021900000000     C                   ENDSR
022000000000     C*------------------------------------------------------------------------*
022100000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
022200000000     C*------------------------------------------------------------------------*
022300000000     C     DECODI        BEGSR
022400000000     C*
022500000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
022600000000     C                   SETOFF                                       606162
022700000000     C                   SETOFF                                       646566
022800000000     C                   SETOFF                                       676869
022900000000     C                   SETOFF                                       707172
023000000703     C                   SETOFF                                       7374
023100000000     C*
023200000000     C                   Z-ADD     *ZEROS        SELEZ             1 0
023300000000     C                   Z-ADD     *ZEROS        RISUL             4 0
023400000000     C*--------------------
023500000000     C* ANNO/MESE INIZIALE
023600000000     C*--------------------
023700000000     C* OBBLIGATORIO
023800000000IF  1C     V1CDAT        IFEQ      *ZEROS
023900000000     C                   MOVEL     '1'           $ERR
024000000000     C                   MOVEL     MSG(7)        $MSG
024100000000     C                   EXSR      CARMSG
024200000000     C                   SETON                                        60
024300000000E   1C                   ENDIF
024400000000     C* CORRETTO
024500000000IF  1C     $ERR          IFEQ      '0'
024600990527     C                   MOVEL     V1CDAT        G08INV
024700990527     C                   MOVE      01            G08INV
024800990527     C                   Z-ADD     *ZEROS        G08DAT
024900990527     C                   MOVEL     '3'           G08ERR
025000990527     C                   CALL      'XSRDA8'
025100990527     C                   PARM                    WLBDA8
025200000000     C*
025300990527IF  2C     G08ERR        IFEQ      '1'
025400000000     C                   MOVEL     '1'           $ERR
025500000000     C                   MOVEL     MSG(8)        $MSG
025600000000     C                   EXSR      CARMSG
025700000000     C                   SETON                                        60
025800000000E   2C                   ENDIF
025900000000E   1C                   ENDIF
026000000000     C*--------------------
026100000000     C* ANNO/MESE FINALE
026200000000     C*--------------------
026300000000     C* OBBLIGATORIO
026400000000IF  1C     V1CDA2        IFEQ      *ZEROS
026500000000     C                   MOVEL     '1'           $ERR
026600000000     C                   MOVEL     MSG(7)        $MSG
026700000000     C                   EXSR      CARMSG
026800000000     C                   SETON                                        62
026900000000E   1C                   ENDIF
027000000000     C* CORRETTO
027100000000IF  1C     $ERR          IFEQ      '0'
027200990527     C                   MOVEL     V1CDA2        G08INV
027300990527     C                   MOVE      01            G08INV
027400990527     C                   Z-ADD     *ZEROS        G08DAT
027500990527     C                   MOVEL     '3'           G08ERR
027600990527     C                   CALL      'XSRDA8'
027700990527     C                   PARM                    WLBDA8
027800000000     C*
027900990527IF  2C     G08ERR        IFEQ      '1'
028000000000     C                   MOVEL     '1'           $ERR
028100000000     C                   MOVEL     MSG(8)        $MSG
028200000000     C                   EXSR      CARMSG
028300000000     C                   SETON                                        62
028400000000E   2C                   ENDIF
028500000000E   1C                   ENDIF
028600000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
028700000000IF  1C     $ERR          IFEQ      '0'
028800000000IF  2C     V1CDA2        IFLT      V1CDAT
028900000000     C                   MOVEL     '1'           $ERR
029000000000     C                   MOVEL     MSG(11)       $MSG
029100000000     C                   EXSR      CARMSG
029200000000     C                   SETON                                        62
029300000000E   2C                   ENDIF
029400000000E   1C                   ENDIF
029500000000     C*
029600000000     C* RICHIEDERE NON PIU' DI DODICI MESI
029700000000IF  1C     $ERR          IFEQ      '0'
029800000000     C     V1CDA2        SUB       V1CDAT        RISUL
029900000000IF  2C     RISUL         IFGE      100
030000000000     C                   MOVEL     '1'           $ERR
030100000000     C                   MOVEL     MSG(21)       $MSG
030200000000     C                   EXSR      CARMSG
030300000000     C                   SETON                                        62
030400000000E   2C                   ENDIF
030500000000E   1C                   ENDIF
030600020325     C*
030700020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'ANNO)
030800020325IF  1C     $ERR          IFEQ      '0'
030900020325     C                   MOVEL     V1CDAT        ANNODA            4 0
031000020325     C                   MOVEL     V1CDA2        ANNOA             4 0
031100020325IF  2C     ANNODA        IFNE      ANNOA
031200020325     C                   MOVEL     '1'           $ERR
031300020325     C                   MOVEL     MSG(25)       $MSG
031400020325     C                   EXSR      CARMSG
031500020325     C                   SETON                                        6062
031600020325E   2C                   ENDIF
031700020325E   1C                   ENDIF
031800000000     C*--------------------
031900950526     C* SELEZIONI
032000000000     C*--------------------
032100000000     C* CONTROLLO FLAG DETTAGLIO FIL/FIL
032200000000IF  1C     $ERR          IFEQ      '0'
032300000000IF  2C     V1CDET        IFEQ      'S'
032400000000     C     V1CDET        OREQ      'N'
032500000000X   2C                   ELSE
032600000000     C                   MOVEL     '1'           $ERR
032700000000     C                   MOVEL     MSG(22)       $MSG
032800000000     C                   EXSR      CARMSG
032900000000     C                   SETON                                        72
033000000000E   2C                   ENDIF
033100000000E   1C                   ENDIF
033200000000     C*
033300000000     C* CONTROLLO FLAG CREAZIONE FILE SU PC
033400000000IF  1C     $ERR          IFEQ      '0'
033500000000IF  2C     V1CFPC        IFEQ      'S'
033600000000     C     V1CFPC        OREQ      'N'
033700000000X   2C                   ELSE
033800000000     C                   MOVEL     '1'           $ERR
033900000000     C                   MOVEL     MSG(23)       $MSG
034000000000     C                   EXSR      CARMSG
034100000000     C                   SETON                                        64
034200000000E   2C                   ENDIF
034300000000E   1C                   ENDIF
034400950526     C*
034500950526     C* CONTROLLO FLAG PARTENZE/ARRIVI
034600950526IF  1C     $ERR          IFEQ      '0'
034700950526IF  2C     V1CMPA        IFEQ      'A'
034800950526     C     V1CMPA        OREQ      'P'
034900950526X   2C                   ELSE
035000950526     C                   MOVEL     '1'           $ERR
035100950526     C                   MOVEL     MSG(10)       $MSG
035200950526     C                   EXSR      CARMSG
035300950526     C                   SETON                                        61
035400950526E   2C                   ENDIF
035500950526E   1C                   ENDIF
035600000000     C*
035700000000     C* CONTROLLO LA FILIALE
035800000000IF  1C     $ERR          IFEQ      '0'
035900000000     C                   MOVEL     *BLANKS       V1DFIL
036000000000IF  2C     V1CFIL        IFNE      *ZEROS
036100000000     C     V1CFIL        ANDNE     *BLANKS
036200000000     C                   MOVEL     V1CFIL        W1CFIL            3
036300000000     C                   EXSR      CHKFIL                                       *CONTROLLA FILIALE
036400000000IF  3C     $ERR          IFEQ      '1'
036500000000     C                   MOVEL     MSG(12)       $MSG
036600000000     C                   EXSR      CARMSG
036700000000     C                   SETON                                        65
036800000000X   3C                   ELSE
036900000000     C                   MOVEL     W1CFIL        V1CFIL
037000000000     C                   MOVEL     W1CFIL        PARFIL                         *PARAMETRO
037100000000     C                   MOVEL     W1DFIL        V1DFIL                         *DESCRIZIONE FILIALE
037200000000E   3C                   ENDIF
037300000000     C*
037400000000     C* FILIALE NON SELEZIONATA: PASSO PARAMETRO A ZERO
037500000000X   2C                   ELSE
037600000000     C                   Z-ADD     *ZEROS        PARFIL
037700000000E   2C                   ENDIF
037800000000E   1C                   ENDIF
037900000000     C*
038000000000     C* CONTROLLO L'AREA
038100000000IF  1C     $ERR          IFEQ      '0'
038200000000     C                   MOVEL     *BLANKS       V1DARE
038300000000IF  2C     V1CARE        IFNE      *ZEROS
038400000000     C     V1CARE        ANDNE     *BLANKS
038500000000     C                   MOVEL     V1CARE        W1CARE            3
038600000000     C                   EXSR      CHKARE                                       *CONTROLLA AREA
038700000000IF  3C     $ERR          IFEQ      '1'
038800000000     C                   MOVEL     MSG(13)       $MSG
038900000000     C                   EXSR      CARMSG
039000000000     C                   SETON                                        66
039100000000X   3C                   ELSE
039200000000     C                   MOVEL     W1CARE        V1CARE
039300000000     C                   MOVEL     W1CARE        PARARE                         *PARAMETRO
039400000000     C                   MOVEL     W1DARE        V1DARE                         *DESCRIZIONE FILIALE
039500000000E   3C                   ENDIF
039600000000     C*
039700000000     C* AREA NON SELEZIONATA: PASSO PARAMETRO A ZERO
039800000000X   2C                   ELSE
039900000000     C                   Z-ADD     *ZEROS        PARARE
040000000000E   2C                   ENDIF
040100000000E   1C                   ENDIF
040200000000     C*
040300000000     C* CONTROLLO LA DIVISIONE
040400000000IF  1C     $ERR          IFEQ      '0'
040500000000     C                   MOVEL     *BLANKS       V1DDIV
040600000000IF  2C     V1CDIV        IFNE      *ZEROS
040700000000     C     V1CDIV        ANDNE     *BLANKS
040800000000     C                   MOVEL     V1CDIV        W1CDIV            1
040900000000     C                   EXSR      CHKDIV                                       *CONTROLLA DIVISIONE
041000000000IF  3C     $ERR          IFEQ      '1'
041100000000     C                   MOVEL     MSG(14)       $MSG
041200000000     C                   EXSR      CARMSG
041300000000     C                   SETON                                        67
041400000000X   3C                   ELSE
041500000000     C                   MOVEL     W1CDIV        V1CDIV
041600000000     C                   MOVEL     W1CDIV        PARDIV                         *PARAMETRO
041700000000     C                   MOVEL     W1DDIV        V1DDIV                         *DESCRIZIONE FILIALE
041800000000E   3C                   ENDIF
041900000000E   2C                   ENDIF
042000000000E   1C                   ENDIF
042100000000     C*
042200000000     C* CONTROLLO CHE LA SELEZIONE FRA FILIALE, AREA E DIVISIONE SIA UNA SOLA
042300000000IF  1C     $ERR          IFEQ      '0'
042400000000IF  2C     V1CFIL        IFGT      *ZEROS
042500000000     C                   ADD       1             SELEZ
042600000000E   2C                   ENDIF
042700000000IF  2C     V1CARE        IFGT      *ZEROS
042800000000     C                   ADD       1             SELEZ
042900000000E   2C                   ENDIF
043000000000IF  2C     V1CDIV        IFGT      *ZEROS
043100000000     C                   ADD       1             SELEZ
043200000000E   2C                   ENDIF
043300000000IF  2C     SELEZ         IFGT      1
043400000000     C                   MOVEL     '1'           $ERR
043500980403     C                   MOVEL     MSG(19)       $MSG
043600000000     C                   EXSR      CARMSG
043700000000     C                   SETON                                        65
043800000000E   2C                   ENDIF
043900000000E   1C                   ENDIF
044000000000     C*
044100000000     C* CONTROLLO IL TIPO PORTO
044200000000IF  1C     $ERR          IFEQ      '0'
044300000000IF  2C     V1CPOR        IFEQ      'F'
044400000000     C     V1CPOR        OREQ      'A'
044500000000     C     V1CPOR        OREQ      ' '
044600000000X   2C                   ELSE
044700000000     C                   MOVEL     '1'           $ERR
044800000000     C                   MOVEL     MSG(16)       $MSG
044900000000     C                   EXSR      CARMSG
045000000000     C                   SETON                                        69
045100000000E   2C                   ENDIF
045200000000E   1C                   ENDIF
045300000000     C*
045400000000     C* CONTROLLO IL TIPO SERVIZIO
045500000000     C                   MOVEL     *BLANKS       V1DSER
045600000000IF  1C     $ERR          IFEQ      '0'
045700000000IF  2C     V1CSER        IFNE      *BLANKS
045800000000     C                   MOVEL     V1CSER        WSER
045900000000     C                   EXSR      CHKSER
046000000000     C                   MOVEL     WSER          V1CSER
046100000000     C                   MOVEL     WDSER         V1DSER
046200000000IF  3C     $ERR          IFEQ      '1'
046300000000     C                   MOVEL     MSG(15)       $MSG
046400000000     C                   EXSR      CARMSG
046500000000     C                   SETON                                        68
046600000000E   3C                   ENDIF
046700000000E   2C                   ENDIF
046800000000E   1C                   ENDIF
046900950526     C*
047000950526     C* CONTROLLO IL TIPO DI DATI IN STATISTICA
047100950526IF  1C     $ERR          IFEQ      '0'
047200950526IF  2C     V1CIOE        IFEQ      'I'
047300950526     C     V1CIOE        OREQ      'M'
047400950526     C     V1CIOE        OREQ      'X'
047500950526     C     V1CIOE        OREQ      *BLANKS
047600950526X   2C                   ELSE
047700950526     C                   MOVEL     '1'           $ERR
047800950526     C                   MOVEL     MSG(29)       $MSG
047900950526     C                   EXSR      CARMSG
048000950526     C                   SETON                                        73
048100950526E   2C                   ENDIF
048200950526E   1C                   ENDIF
048300000000     C*--------------------
048400000000     C* ANNO/MESE FINALE
048500000000     C*--------------------
048600000000     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO CORRENTE - 1
048700000000     C* DAL 18/04/95 MESSAGGIO SONO INFORMATIVO
048800000000IF  1C     $ERR          IFEQ      '0'
048900000000IF  2C     V1CDA2        IFGT      AMCOR
049000000000     C                   MOVEL     MSG(9)        $MSG
049100000000     C                   EXSR      CARMSG
049200000000     C*---                 MOVEL'1'       $ERR
049300000000     C*---                 SETON                     62
049400000000E   2C                   ENDIF
049500000000E   1C                   ENDIF
049600000614     C*
049700000703     C* DATI POSTE
049800000614IF  1C     $ERR          IFEQ      '0'
049900160422IF  2C     V1Csta        IFNE      'S'
050000160422     C     V1Csta        ANDNE     'N'
050100000614     C                   MOVEL     '1'           $ERR
050200000614     C                   MOVEL     MSG(24)       $MSG
050300000614     C                   EXSR      CARMSG
050400000614     C                   SETON                                        74
050500000614E   2C                   ENDIF
050600000614E   1C                   ENDIF
050700000000     C*
050800000000     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000000000     C* CHKFIL - CONTROLLA LA FILIALE
051100000000     C*------------------------------------------------------------------------*
051200000000     C     CHKFIL        BEGSR
051300000000     C*
051400000000     C* CONTROLLO SE E' STATO INSERITO '?'
051500000000     C     '?'           SCAN      V1CFIL                                 99
051600000000     C*
051700000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
051800000000IF  1C     *IN99         IFEQ      *ON
051900000000     C                   MOVEL     *BLANKS       §COD1
052000000000     C                   MOVE      *BLANKS       §DES1
052100971203     C                   MOVE      *BLANKS       §PRA
052200971203     C                   CALL      'TNSD24R'
052300000000     C                   PARM                    §COD1             3
052400000000     C                   PARM                    §TIP              1
052500000000     C                   PARM                    §DES1            25
052600971203     C                   PARM                    §PRA              3
052700000000     C* SELEZIONATO
052800000000IF  2C     §DES1         IFNE      *BLANKS
052900000000     C                   MOVEL     §COD1         W1CFIL
053000000000     C                   MOVEL     §DES1         W1DFIL           25
053100000000X   2C                   ELSE
053200000000     C* NON SELEZIONATO
053300000000     C                   MOVEL     *ZEROS        W1CFIL
053400000000E   2C                   ENDIF
053500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
053600000000E   1C                   ENDIF
053700000000     C*
053800000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
053900000000     C                   MOVEL     W1CFIL        ORGFIL
054000000000     C     KEYORG        CHAIN     AZORG                              99
054100000000IF  1C     *IN99         IFEQ      *OFF
054200000000     C     ORGFVA        ANDEQ     *BLANKS
054300000000     C     ORGFAG        ANDNE     'V'
054400000000     C                   MOVEL     ORGDES        W1DFIL
054500000000X   1C                   ELSE
054600000000     C                   MOVEL     '1'           $ERR
054700000000E   1C                   ENDIF
054800000000     C*
054900000000     C                   ENDSR
055000000000     C*------------------------------------------------------------------------*
055100000000     C* CHKARE - CONTROLLA L'AREA
055200000000     C*------------------------------------------------------------------------*
055300000000     C     CHKARE        BEGSR
055400000000     C*
055500000000     C*
055600000000     C* CONTROLLO SE E' STATO INSERITO '?'
055700000000     C     '?'           SCAN      V1CARE                                 99
055800000000     C*
055900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
056000000000IF  1C     *IN99         IFEQ      *ON
056100020916     C                   MOVEL     1             §KUT
056200000000     C                   MOVEL     '05'          §COD
056300000000     C                   MOVEL     *BLANKS       §KEY
056400000000     C                   MOVEL     *BLANKS       §DES
056500000000     C                   CALL      'X§TABER'
056600000000     C                   PARM                    §KUT              1 0
056700000000     C                   PARM                    §COD              2
056800000000     C                   PARM                    §KEY              8
056900000000     C                   PARM                    §DES             30
057000000000     C                   MOVEL     §KEY          W1CARE
057100000000     C                   MOVEL     §DES          W1DARE           25
057200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
057300000000E   1C                   ENDIF
057400000000     C*
057500000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
057600020916     C                   MOVEL     1             TBLKUT
057700000000     C                   MOVEL     '05'          TBLCOD
057800000000     C                   MOVEL     *BLANKS       TBLKEY
057900000000     C                   MOVEL     W1CARE        TBLKEY
058000000000     C     KEYTAB        CHAIN     TABEL                              99
058100000000IF  1C     *IN99         IFEQ      *ON
058200000000     C     TBLFLG        OREQ      '*'
058300000000     C                   MOVEL     '1'           $ERR
058400000000X   1C                   ELSE
058500000000     C                   MOVEL     TBLUNI        W1DARE
058600000000E   1C                   ENDIF
058700000000     C*
058800000000     C                   ENDSR
058900000000     C*------------------------------------------------------------------------*
059000000000     C* CHKDIV - CONTROLLA LA DIVISIONE
059100000000     C*------------------------------------------------------------------------*
059200000000     C     CHKDIV        BEGSR
059300000000     C*
059400000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
059500000000IF  1C     V1CDIV        IFEQ      '?'
059600020916     C                   MOVEL     1             §KUT
059700000000     C                   MOVEL     '17'          §COD
059800000000     C                   MOVEL     *BLANKS       §KEY
059900000000     C                   MOVEL     *BLANKS       §DES
060000000000     C                   CALL      'X§TABER'
060100000000     C                   PARM                    §KUT
060200000000     C                   PARM                    §COD
060300000000     C                   PARM                    §KEY
060400000000     C                   PARM                    §DES
060500000000     C                   MOVEL     §KEY          W1CDIV
060600000000     C                   MOVEL     §DES          W1DDIV           25
060700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
060800000000E   1C                   ENDIF
060900000000     C*
061000000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
061100020916     C                   MOVEL     1             TBLKUT
061200000000     C                   MOVEL     '17'          TBLCOD
061300000000     C                   MOVEL     *BLANKS       TBLKEY
061400000000     C                   MOVEL     W1CDIV        TBLKEY
061500000000     C     KEYTAB        CHAIN     TABEL                              99
061600000000IF  1C     *IN99         IFEQ      *ON
061700000000     C     TBLFLG        OREQ      '*'
061800000000     C                   MOVEL     '1'           $ERR
061900000000X   1C                   ELSE
062000000000     C                   MOVEL     TBLUNI        W1DDIV
062100000000E   1C                   ENDIF
062200000000     C*
062300000000     C                   ENDSR
062400000000     C*------------------------------------------------------------------------*
062500000000     C* CHKSER - CONTROLLA IL TIPO SERVIZIO
062600000000     C*------------------------------------------------------------------------*
062700000000     C     CHKSER        BEGSR
062800000000     C*
062900000000     C     *LIKE         DEFINE    V1CSER        WSER
063000000000     C     *LIKE         DEFINE    V1DSER        WDSER
063100000000     C*
063200000000     C                   MOVEL     *BLANKS       WDSER
063300000000     C*
063400000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
063500000000IF  1C     WSER          IFEQ      '?'
063600020916     C                   MOVEL     1             §KUT
063700000000     C                   MOVEL     '5E'          §COD
063800000000     C                   MOVEL     *BLANKS       §KEY
063900000000     C                   MOVEL     *BLANKS       §DES
064000000000     C                   CALL      'X§TABER'
064100000000     C                   PARM                    §KUT
064200000000     C                   PARM                    §COD
064300000000     C                   PARM                    §KEY
064400000000     C                   PARM                    §DES
064500000000     C                   MOVEL     §KEY          WSER
064600000000     C                   MOVEL     §DES          WDSER
064700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
064800000000E   1C                   ENDIF
064900000000     C*
065000000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
065100020916     C                   MOVEL     1             TBLKUT
065200000000     C                   MOVEL     '5E'          TBLCOD
065300000000     C                   MOVEL     *BLANKS       TBLKEY
065400000000     C                   MOVEL     WSER          TBLKEY
065500000000     C     KEYTAB        CHAIN     TABEL                              99
065600000000IF  1C     *IN99         IFEQ      *ON
065700000000     C     TBLFLG        OREQ      '*'
065800000000     C                   MOVEL     '1'           $ERR
065900000000X   1C                   ELSE
066000000000     C                   MOVEL     TBLUNI        WDSER
066100000000E   1C                   ENDIF
066200000000     C*
066300000000     C                   ENDSR
066400000000     C*------------------------------------------------------------------------*
066500000000     C* IMPVID  - IMPOSTAZIONI INIZIALI VIDEO
066600000000     C*------------------------------------------------------------------------*
066700000000     C     IMPVID        BEGSR
066800000614     C*
066900000000     C*--------------------
067000000000     C* D A T A  F I N E --> ANNO/MESE CORRENTE - 1 MESE
067100000000     C*--------------------
067200990527     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
067300990527     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
067400990527     C                   Z-ADD     N8            DDATA8                         *SCOMPOSIZIONE DATA
067500990527     C*
067600990527     C                   Z-ADD     DANNO8        ANNO              4 0
067700990527     C                   Z-ADD     DMESE8        MESE              2 0
067800000000     C                   SUB       1             MESE
067900000000IF  1C     MESE          IFEQ      *ZEROS                                       *SE GENNAIO
068000000000     C                   Z-ADD     12            MESE                           *IMPOSTO DICEMBRE
068100000000     C                   SUB       1             ANNO                           *DELL'ANNO PRECEDENTE
068200000000E   1C                   ENDIF
068300000000     C                   MOVEL     ANNO          V1CDA2
068400000000     C                   MOVE      MESE          V1CDA2
068500990527     C*
068600990527     C                   Z-ADD     V1CDA2        AMCOR             6 0          *ANN/MESE CORRENTE
068700000000     C*--------------------
068800000000     C* D A T A  I N I Z I O --> INIIZO DELL'ANNO
068900000000     C*--------------------
069000000000     C                   Z-ADD     V1CDA2        V1CDAT
069100000000     C                   MOVE      01            V1CDAT
069200000000     C*--------------------
069300000000     C* RAGIONE SOCIALE
069400000000     C*--------------------
069500020916     C                   MOVEL     RSUT          VTDRSU
069600000000     C*--------------------
069700000000     C* RICHIESTE
069800000000     C*--------------------
069900950526     C                   MOVEL     'P'           V1CMPA                         *TIPO MOVIMENTO
070000000000     C                   MOVEL     'N'           V1CDET                         *DETTAGLIO FIL/FIL
070100950531     C                   MOVEL     'S'           V1CFPC                         *CREAZIONE FILE SU PC
070200950526     C                   MOVEL     'I'           V1CIOE                         *DATI IN STATISTICA
070300160422     C                   MOVEL     'N'           v1csta
070400000000     C*
070500000000     C* IMPOSTAZIONE TEMPORANEA
070600950713     C*---                 MOVEL'N'       V1CSPE           *SPEDIZIONI NORMALI
070700000000     C*
070800000000     C                   ENDSR
070900020916     C*--------------------------------------------------------------------------------------------*
071000020916     C* REPERISCE I DATI UTENTE
071100020916     C*--------------------------------------------------------------------------------------------*
071200020916     C     REPDATIUTE    BEGSR
071300020916     C*
071400020916     C* INIZIALIZZA VARIABILI DI WRK
071500020916     C                   CLEAR                   TIBS34DS
071600020916     C                   CLEAR                   AZUTEDS
071700020916     C                   CLEAR                   DDATIUTE
071800020916     C*
071900020916     C     *DTAARA       DEFINE    §azute        azuteds
072000020916     C     *DTAARA       DEFINE    §datiute      ddatiute
072100020916     C                   IN(E)     *DTAARA
072200020916if  1C                   IF        %Error
072300020916     c                   EVAL      I34Tla = 'L'
072400020916     C                   CALL      'TIBS34R'
072500020916     C                   PARM                    Tibs34Ds
072600020916     C                   IN        *DTAARA
072700020916e   1C                   ENDIF
072800020916     C*
072900020916     C                   ENDSR
073000000000     C*------------------------------------------------------------------------*
073100000000     C* *INZSR  - OPERAZIONI INIZIALI
073200000000     C*------------------------------------------------------------------------*
073300000000     C     *INZSR        BEGSR
073400950526     C*--------------------
073500000000     C* RICEVIMENTO PARAMETRI
073600950526     C*--------------------
073700000000     C     *ENTRY        PLIST
073800000000     C                   PARM                    KPJBA
073900950526     C*--------------------
074000950526     C* CHIAVI DI LETTURA
074100950526     C*--------------------
074200000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
074300000000     C     KEYORG        KLIST
074400000000     C                   KFLD                    ORGFIL
074500000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
074600000000     C     KEYTAB        KLIST
074700000000     C                   KFLD                    TBLKUT
074800000000     C                   KFLD                    TBLCOD
074900000000     C                   KFLD                    TBLKEY
075000000000     C*
075100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
075200000000     C                   EXSR      INZSTD
075300000000     C*
075400000000     C* IMPOSTAZIONI INIZIALI VIDEO
075500000000     C                   EXSR      IMPVID
075600000000     C*
075700950526     C* EMETTE FORMATO VIDEO
075800950526     C                   EXSR      CARV1
075900000000     C*
076000000000     C                   ENDSR
076100000000     C*------------------------------------------------------------------------*
076200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076400000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
076500000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
076600000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
076700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076900000000     C*------------------------------------------------------------------------*
077000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
077100000000     C*------------------------------------------------------------------------*
077200000000     C     INZSTD        BEGSR
077300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
077400000000     C                   CALL      'XKEYS'
077500000000     C                   PARM                    DSKEYS
077600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
077700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
077800000000     C     XMSGPA        PLIST
077900000000     C                   PARM                    DSMSMS
078000000000     C                   PARM                    DSMSPG
078100000000     C                   PARM                    DSMSKE
078200000000     C                   PARM                    DSMSER
078300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
078400020115     C                   MOVEL     'TISE45R'     DSMSPG                         *NOME PGM CHIAMANTE
078500020115     C                   MOVEL     'TISE45R'     VMCQUE                         *CODA MESSAGGI DESTIN
078600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
078700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
078800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
078900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
079000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
079100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
079200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
079300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
079400000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
079500000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
079600000000     C* LANCIO ROUTINE PER VERIFICARE SE
079700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
079800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
079900020916     C                   EXSR      REPDATIUTE
080000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
080100000000     C                   BITOFF    '01234567'    POSMI1            1
080200000000     C                   BITOFF    '01234567'    POSMI2            1
080300000000     C                   BITOFF    '01234567'    POSMA1            1
080400000000     C                   BITOFF    '01234567'    POSMA2            1
080500000000     C                   BITON     '34'          POSMI1            1            *18
080600000000     C                   BITON     '7'           POSMI2            1            *01
080700000000     C                   BITON     '34'          POSMA1            1            *18
080800000000     C                   BITON     '0'           POSMA2            1            *80
080900000000     C                   MOVE      POSMI2        POSMIN            2
081000000000     C                   MOVEL     POSMI1        POSMIN
081100000000     C                   MOVE      POSMA2        POSMAX            2
081200000000     C                   MOVEL     POSMA1        POSMAX
081300000000     C                   ENDSR
081400000000     C*------------------------------------------------------------------------*
081500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
081600000000     C*------------------------------------------------------------------------*
081700000000     C     FUNSTD        BEGSR
081800000000     C                   SELECT
081900000000     C     KEY           WHENEQ    F3
082000000000     C                   EXSR      F03STD
082100000000     C     KEY           WHENEQ    F12
082200000000     C                   EXSR      F12STD
082300000000     C     KEY           WHENEQ    F24
082400000000     C                   EXSR      F24STD
082500000000     C                   OTHER
082600000000     C                   MOVEL     MSG(2)        $MSG
082700000000     C                   EXSR      CARMSG
082800000000     C                   ENDSL
082900000000     C                   ENDSR
083000000000     C*------------------------------------------------------------------------*
083100000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
083200000000     C*------------------------------------------------------------------------*
083300000000     C     F03STD        BEGSR
083400000000     C*
083500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
083600000000     C                   MOVEL     '1'           $FINE
083700000000     C*
083800000000     C                   ENDSR
083900000000     C*------------------------------------------------------------------------*
084000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
084100000000     C*------------------------------------------------------------------------*
084200000000     C     F12STD        BEGSR
084300000000     C*
084400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
084500000000     C                   MOVEL     '1'           $FINE
084600000000     C*
084700000000     C                   ENDSR
084800000000     C*------------------------------------------------------------------------*
084900000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
085000000000     C*------------------------------------------------------------------------*
085100000000     C     F24STD        BEGSR
085200000000     C*
085300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
085400000000     C                   EXSR      OUTFUN
085500000000     C*
085600000000     C                   ENDSR
085700000000     C*------------------------------------------------------------------------*
085800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
085900000000     C*          CODA MESSAGGI DEL PGM
086000000000     C*------------------------------------------------------------------------*
086100000000     C     CARMSG        BEGSR
086200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
086300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
086400000000     C     MSGULT        IFEQ      0
086500000000     C                   MOVEL     '1'           *IN40
086600020115     C                   WRITE     SE45DVK
086700000000     C                   MOVEL     '0'           *IN40
086800000000     C                   ENDIF
086900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
087000000000     C                   MOVEL     $MSG          DSMSMS
087100000000     C                   CALL      'XMSG'        XMSGPA
087200000000     C     DSMSER        IFEQ      0
087300000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
087400000000     C                   ENDIF
087500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
087600000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
087700020115     C                   WRITE     SE45DVM                                      *SFL MESS
087800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
087900000000     C                   MOVEL     *BLANKS       $MSG
088000000000     C                   ENDSR
088100000000     C*------------------------------------------------------------------------*
088200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
088300000000     C*------------------------------------------------------------------------*
088400000000     C     OUTMSG        BEGSR
088500000000     C* VISUALIZZAZIONE MESSAGGIO
088600000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
088700020115     C                   WRITE     SE45DVK                                      *CONTR MESS
088800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
088900000000     C                   Z-ADD     *ZEROS        MSGULT
089000000000     C                   ENDSR
089100000000     C*------------------------------------------------------------------------*
089200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
089300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
089400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
089500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
089600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
089700000000     C*------------------------------------------------------------------------*
089800000000     C     CARFUN        BEGSR
089900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
090000000000     C     1             DO        31            I                 5 0          --- 1 -->
090100000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
090200000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
090300000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
090400020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
090500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
090600000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
090700000000     C                   ELSE                                                   *INTERROG.
090800000000     C                   MOVEL     FUNASI        FAT(I)
090900000000     C                   ENDIF
091000000000     C                   ELSE                                                   <-- 2 --> *FILIALE
091100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
091200000000     C                   MOVEL     FUNAFG        FAT(I)
091300000000     C                   ELSE                                                   *INTERROG.
091400000000     C                   MOVEL     FUNAFI        FAT(I)
091500000000     C                   ENDIF
091600000000     C                   ENDIF                                                  <-- 2 ---
091700000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
091800000000     C                   MOVEL     USER          WEDP              3            *PROFILO
091900000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
092000000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
092100000000     C                   ELSE
092200000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
092300000000     C                   ENDIF
092400000000     C                   ENDIF
092500000000     C                   ENDDO                                                  <-- 1 ---
092600000000     C                   ENDSR
092700000000     C*------------------------------------------------------------------------*
092800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
092900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
093000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
093100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
093200000000     C*------------------------------------------------------------------------*
093300000000     C     OUTFUN        BEGSR
093400000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
093500000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
093600000000     C                   ENDDO
093700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
093800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
093900000000     C     1             DO        31            I
094000000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
094100000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
094200000000     C                   ADD       1             FUNNAT
094300000000     C                   ENDIF
094400000000     C                   ENDDO
094500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
094600000000     C     FUNNAT        IFGT      8
094700000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
094800000000     C                   ENDIF
094900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
095000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
095100000000     C                   Z-ADD     8             FUNVIS            5 0
095200000000     C     FAT(23)       IFEQ      'S'
095300000000     C                   SUB       1             FUNVIS
095400000000     C                   ENDIF
095500000000     C     FAT(24)       IFEQ      'S'
095600000000     C                   SUB       1             FUNVIS
095700000000     C                   ENDIF
095800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
095900000000     C                   Z-ADD     *ZEROS        W                 5 0
096000000000     C                   ADD       1             FUNULT
096100000000     C     FUNULT        IFGT      31
096200000000     C                   Z-ADD     1             FUNULT
096300000000     C                   ENDIF
096400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
096500000000     C     FUNULT        DO        31            I                              --- 1 -->
096600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
096700000000     C     W             IFLT      FUNVIS                                       --- 2 -->
096800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
096900000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
097000000000     C     FDE(I)        ANDNE     *BLANKS
097100000000     C     I             IFEQ      23                                           *TOLGO F23
097200000000     C     I             OREQ      24                                           *E F24
097300000000     C                   ELSE
097400000000     C                   ADD       1             W
097500000000     C                   MOVEL     FDE(I)        FUV(W)
097600000000     C                   ENDIF
097700000000     C                   ENDIF                                                  <-- 3 ---
097800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
097900000000     C                   Z-ADD     I             FUNULT
098000000000     C                   ENDIF                                                  <-- 2 ---
098100000000     C                   ENDDO                                                  <-- 1 ---
098200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
098300000000     C                   MOVEL     FUV(1)        VFD001
098400000000     C                   MOVEL     FUV(2)        VFD002
098500000000     C                   MOVEL     FUV(3)        VFD003
098600000000     C                   MOVEL     FUV(4)        VFD004
098700000000     C                   MOVEL     FUV(5)        VFD005
098800000000     C                   MOVEL     FUV(6)        VFD006
098900000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
099000000000     C     FAT(24)       ANDEQ     'N'
099100000000     C                   MOVEL     FUV(7)        VFD007
099200000000     C                   MOVEL     FUV(8)        VFD008
099300000000     C                   ELSE                                                   <-- 1 -->
099400000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
099500000000     C     FAT(24)       ANDEQ     'S'
099600000000     C                   MOVEL     FDE(23)       VFD007
099700000000     C                   MOVEL     FDE(24)       VFD008
099800000000     C                   ELSE                                                   <-- 2 -->
099900000000     C                   MOVEL     FUV(7)        VFD007
100000000000     C     FAT(23)       IFEQ      'S'
100100000000     C                   MOVEL     FDE(23)       VFD008
100200000000     C                   ELSE
100300000000     C                   MOVEL     FDE(24)       VFD008
100400000000     C                   ENDIF
100500000000     C                   ENDIF                                                  <-- 2 ---
100600000000     C                   ENDIF                                                  <-- 1 ---
100700000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
100800000000     C                   ENDSR
100900000000     C*------------------------------------------------------------------------*
101000000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
101100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
101200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
101300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
101400000000     C*------------------------------------------------------------------------*
101500000000     C     CONFUN        BEGSR
101600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
101700000000     C     1             DO        31            I                              --- 1 -->
101800000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
101900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
102000000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
102100000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
102200000000     C                   EXSR      CARMSG
102300000000     C                   MOVEL     '1'           $ERR
102400000000     C                   ENDIF                                                  <-- 3 ---
102500000000     C                   ENDIF                                                  <-- 2 ---
102600000000     C                   ENDDO                                                  <-- 1 ---
102700000000     C                   ENDSR
102800000000     O*------------------------------------------------------------------------*
102900000000**   MSG - MESSAGGI
103000020115TISE45R- Tasto funzionale non valido.                                          1
103100020115TISE45R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
103200020115TISE45R- Opzione non valida.                                                   3
103300020115TISE45R- Opzione abilitata ma non gestita. Avvertire CED.                      4
103400020115TISE45R- Inizio elenco.                                                        5
103500020115TISE45R- Fine elenco.                                                          6
103600020115TISE45R- Anno/Mese obbligatorio                                                7
103700020115TISE45R- Anno/Mese errato                                                      8
103800020115TISE45R- Data superiore alla data creazione saldi                              9
103900020115TISE45R- 'P' per elaborare i movimenti in partenza,'A' per quelli in arrivo   10
104000020115TISE45R- Anno/Mese finale inferiore all'Anno/Mese iniziale                    11
104100020115TISE45R- Punto operativo errato                                               12
104200020115TISE45R- Area errata                                                          13
104300020115TISE45R- Divisione errata                                                     14
104400020115TISE45R- Tipo servizio non accettabile                                        15
104500020115TISE45R- Tipo porto non accettabile                                           16
104600020115TISE45R- Tipo bolla non accettabile                                           17
104700020115TISE45R- Tipo prodotto non accettabile                                        18
104800020115TISE45R- Immettere solo una selezione per punto operativo, area o divisione   19
104900020115TISE45R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
105000020115TISE45R- Impossibile stampare più di 12 mesi                                  21
105100020115TISE45R- 'S' per stampare il dettaglio P.O./P.O. o 'N' per non stamparlo      22
105200020115TISE45R- 'S' per creare file su PC, 'N' per non crearlo                       23
105300020115TISE45R- Immettere uno dei valori possibili                                   24
105400020325TISE45R- Impossibile lanciare la statistica su anni differenti                25
105500020115TISE45R- 26
105600020115TISE45R- 27
105700020115TISE45R- 28
105800020115TISE45R- Immettere il tipo di dati che si vuole in statistica                 29
105900020115TISE45R- 30
106000000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
106100000000F1    -                   -    NN     NN
106200000000F2    -                   -    NN     NN
106300000000F3    -F3=Fine            -    SS     SS
106400000000F4    -                   -    NN     NN
106500000000F5    -                   -    NN     NN
106600000000F6    -F6=Lancio          -    SS     SS
106700000000F7    -                   -    NN     NN
106800000000F8    -                   -    NN     NN
106900000000F9    -                   -    NN     NN
107000000000F10   -                   -    NN     NN
107100000000F11   -                   -    NN     NN
107200000000F12   -F12-Ritorno        -    SS     SS
107300000000F13   -                   -    NN     NN
107400000000F14   -                   -    NN     NN
107500000000F15   -                   -    NN     NN
107600000000F16   -                   -    NN     NN
107700000000F17   -                   -    NN     NN
107800000000F18   -                   -    NN     NN
107900000000F19   -                   -    NN     NN
108000000000F20   -                   -    NN     NN
108100000000F21   -                   -    NN     NN
108200000000F22   -                   -    NN     NN
108300000000F23   -                   -    NN     NN
108400000000F24   -F24=Altri tasti    -    NN     NN
108500000000ENTER -                   -    SS     SS
108600000000ROLLUP-                   -    NN     NN
108700000000ROLLDN-                   -    NN     NN
108800000000HELP  -                   -    NN     NN
108900000000PRINT -                   -    NN     NN
109000000000BSPACE-                   -    NN     NN
109100000000CLEAR -                   -    NN     NN
