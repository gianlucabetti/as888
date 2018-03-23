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
007000990527     D  PARDAT                 1      6  0
007100990527     D  PARDA2                 7     12  0
007200990527     D  PARMPA                13     13
007300990527     D  PARFIL                14     16  0
007400990527     D  PARARE                17     19  0
007500990527     D  PARDIV                20     20
007600990527     D  PARSER                21     21
007700990527     D  PARPOR                22     22
007800990527     D  PARSPE                23     23
007900990527     D  PARPRO                24     24
008000990527     D  PARDET                25     25
008100990527     D  PARFPC                26     26
008200990527     D  PARIOE                27     27
008300000703     D  PARPOS                28     28
008400000000     D*-------------------
008500020916     D* DS REPERIMENTO DATI UTENTE
008600000000     D*-------------------
008700020916     D TIBS34DS      E DS                                                       *Profili utente
008800020916     D DDATIUTE      E DS                                                       *Dati utente
008900020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009000000000     D*-------------------
009100000000     D* DS RAGGRUPPAMENTI BOLLA
009200000000     D*-------------------
009300000000     D DS2W          E DS
009400990527     D*------------------
009500990527     D* DS CONTROLLO DATA (8)
009600990527     D*------------------
009700990527     D WLBDA8          DS                  INZ
009800990527     D  G08DAT                 1      8  0
009900990527     D  G08INV                 9     16  0
010000990527     D  G08ERR                17     17
010100990527     D  G08TGI                18     22  0
010200990527     D*------------------
010300990527     D* DS DATA GENERICA(8) IN GG/MM/AAAA
010400990527     D*------------------
010500990527     D                 DS
010600990527     D  DGIOR8                 1      2  0
010700990527     D  DMESE8                 3      4  0
010800990527     D  DANNO8                 5      8  0
010900990527     D  DDATA8                 1      8  0
011000000000     C*------------------------------------------------------------------------*
011100000000     C* MAIN LINE
011200000000     C*------------------------------------------------------------------------*
011300000000     C*
011400000000     C* CICLO FINO A CHE:
011500000000DO  1C     $FINE         DOUEQ     '1'
011600000000     C*
011700000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
011800000000     C*
011900000000     C* CONTROLLA GESTIONE VIDEO
012000000000IF  2C     $GES          IFEQ      'V1'
012100000000     C                   EXSR      GESV1                                        *PARAMETRI
012200000000E   2C                   ENDIF
012300000000     C*
012400000000E   1C                   ENDDO
012500000000     C*
012600000000     C                   SETON                                        LR
012700000000     C*------------------------------------------------------------------------*
012800000000     C* GESV1 - GESTIONE PRIMO VIDEO
012900000000     C*------------------------------------------------------------------------*
013000000000     C     GESV1         BEGSR
013100000000     C*
013200000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
013300000000     C                   EXSR      CARV1                                        *CARICA VIDEO
013400000000E   1C                   ENDIF
013500000000     C*
013600000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
013700000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
013800000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
013900000000E   1C                   ENDIF
014000000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
014100000000     C*
014200020115     C                   EXFMT     SE45DV1                                      *PARAMETRI
014300000000     C*
014400000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
014500000000     C*
014600000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
014700000000SL  2C                   SELECT
014800000000     C     KEY           WHENEQ    ENTER
014900000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015000000000     C     KEY           WHENEQ    F6
015100000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
015200000000     C                   OTHER
015300000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
015400000000E   2C                   ENDSL
015500000000E   1C                   ENDIF
015600000000     C*
015700000000     C                   ENDSR
015800000000     C*------------------------------------------------------------------------*
015900000000     C* CARV1 - CARICA PRIMO VIDEO
016000000000     C*------------------------------------------------------------------------*
016100000000     C     CARV1         BEGSR
016200000000     C*
016300000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
016400000000     C*
016500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016700000000     C*
016800020115     C                   WRITE     SE45DVT                                      *TESTATA
016900020115     C                   WRITE     SE45DVF                                      *FUNZIONI
017000000000     C*
017100000000     C                   ENDSR
017200000000     C*------------------------------------------------------------------------*
017300000000     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
017400000000     C*------------------------------------------------------------------------*
017500000000     C     ENTV1         BEGSR
017600000000     C*
017700000000     C* CONTROLLA I CAMPI IMMESSI
017800000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
018300000000     C*------------------------------------------------------------------------*
018400000000     C     F06GES        BEGSR
018500000000     C*
018600000000     C* CONTROLLA I CAMPI IMMESSI
018700000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018800000000     C*
018900000000     C* NON CI SONO ERRORI
019000000000IF  1C     $ERR          IFEQ      '0'
019100000000     C*
019200000000     C* LANCIO PROGRAMMA DI STAMPA
019300000000     C                   Z-ADD     V1CDAT        PARDAT
019400000000     C                   Z-ADD     V1CDA2        PARDA2
019500000000     C                   MOVEL     V1CMPA        PARMPA
019600000000     C                   MOVEL     V1CSER        PARSER
019700000000     C                   MOVEL     V1CPOR        PARPOR
019800000000     C                   MOVEL     V1CSPE        PARSPE
019900000000     C                   MOVEL     V1CPRO        PARPRO
020000000000     C                   MOVEL     V1CDET        PARDET
020100000000     C                   MOVEL     V1CFPC        PARFPC
020200950526     C                   MOVEL     V1CIOE        PARIOE
020300000703     C                   MOVEL     V1CPOS        PARPOS
020400000000     C*
020500020115     C                   MOVEL     'SE46'        KCOAZ
020600000000     C                   MOVEL     PARAM         KPJBU
020700020408 MOM C                   CALL      'BCH10'
020800020408     C*                  CALL      'TISE46C'
020900000000     C                   PARM                    KPJBA
021000000000E   1C                   ENDIF
021100000000     C*
021200000000     C                   ENDSR
021300000000     C*------------------------------------------------------------------------*
021400000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
021500000000     C*------------------------------------------------------------------------*
021600000000     C     DECODI        BEGSR
021700000000     C*
021800000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
021900000000     C                   SETOFF                                       606162
022000000000     C                   SETOFF                                       646566
022100000000     C                   SETOFF                                       676869
022200000000     C                   SETOFF                                       707172
022300000703     C                   SETOFF                                       7374
022400000000     C*
022500000000     C                   Z-ADD     *ZEROS        SELEZ             1 0
022600000000     C                   Z-ADD     *ZEROS        RISUL             4 0
022700000000     C*--------------------
022800000000     C* ANNO/MESE INIZIALE
022900000000     C*--------------------
023000000000     C* OBBLIGATORIO
023100000000IF  1C     V1CDAT        IFEQ      *ZEROS
023200000000     C                   MOVEL     '1'           $ERR
023300000000     C                   MOVEL     MSG(7)        $MSG
023400000000     C                   EXSR      CARMSG
023500000000     C                   SETON                                        60
023600000000E   1C                   ENDIF
023700000000     C* CORRETTO
023800000000IF  1C     $ERR          IFEQ      '0'
023900990527     C                   MOVEL     V1CDAT        G08INV
024000990527     C                   MOVE      01            G08INV
024100990527     C                   Z-ADD     *ZEROS        G08DAT
024200990527     C                   MOVEL     '3'           G08ERR
024300990527     C                   CALL      'XSRDA8'
024400990527     C                   PARM                    WLBDA8
024500000000     C*
024600990527IF  2C     G08ERR        IFEQ      '1'
024700000000     C                   MOVEL     '1'           $ERR
024800000000     C                   MOVEL     MSG(8)        $MSG
024900000000     C                   EXSR      CARMSG
025000000000     C                   SETON                                        60
025100000000E   2C                   ENDIF
025200000000E   1C                   ENDIF
025300000000     C*--------------------
025400000000     C* ANNO/MESE FINALE
025500000000     C*--------------------
025600000000     C* OBBLIGATORIO
025700000000IF  1C     V1CDA2        IFEQ      *ZEROS
025800000000     C                   MOVEL     '1'           $ERR
025900000000     C                   MOVEL     MSG(7)        $MSG
026000000000     C                   EXSR      CARMSG
026100000000     C                   SETON                                        62
026200000000E   1C                   ENDIF
026300000000     C* CORRETTO
026400000000IF  1C     $ERR          IFEQ      '0'
026500990527     C                   MOVEL     V1CDA2        G08INV
026600990527     C                   MOVE      01            G08INV
026700990527     C                   Z-ADD     *ZEROS        G08DAT
026800990527     C                   MOVEL     '3'           G08ERR
026900990527     C                   CALL      'XSRDA8'
027000990527     C                   PARM                    WLBDA8
027100000000     C*
027200990527IF  2C     G08ERR        IFEQ      '1'
027300000000     C                   MOVEL     '1'           $ERR
027400000000     C                   MOVEL     MSG(8)        $MSG
027500000000     C                   EXSR      CARMSG
027600000000     C                   SETON                                        62
027700000000E   2C                   ENDIF
027800000000E   1C                   ENDIF
027900000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
028000000000IF  1C     $ERR          IFEQ      '0'
028100000000IF  2C     V1CDA2        IFLT      V1CDAT
028200000000     C                   MOVEL     '1'           $ERR
028300000000     C                   MOVEL     MSG(11)       $MSG
028400000000     C                   EXSR      CARMSG
028500000000     C                   SETON                                        62
028600000000E   2C                   ENDIF
028700000000E   1C                   ENDIF
028800000000     C*
028900000000     C* RICHIEDERE NON PIU' DI DODICI MESI
029000000000IF  1C     $ERR          IFEQ      '0'
029100000000     C     V1CDA2        SUB       V1CDAT        RISUL
029200000000IF  2C     RISUL         IFGE      100
029300000000     C                   MOVEL     '1'           $ERR
029400000000     C                   MOVEL     MSG(21)       $MSG
029500000000     C                   EXSR      CARMSG
029600000000     C                   SETON                                        62
029700000000E   2C                   ENDIF
029800000000E   1C                   ENDIF
029900020325     C*
030000020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'ANNO)
030100020325IF  1C     $ERR          IFEQ      '0'
030200020325     C                   MOVEL     V1CDAT        ANNODA            4 0
030300020325     C                   MOVEL     V1CDA2        ANNOA             4 0
030400020325IF  2C     ANNODA        IFNE      ANNOA
030500020325     C                   MOVEL     '1'           $ERR
030600020325     C                   MOVEL     MSG(25)       $MSG
030700020325     C                   EXSR      CARMSG
030800020325     C                   SETON                                        6062
030900020325E   2C                   ENDIF
031000020325E   1C                   ENDIF
031100000000     C*--------------------
031200950526     C* SELEZIONI
031300000000     C*--------------------
031400000000     C* CONTROLLO FLAG DETTAGLIO FIL/FIL
031500000000IF  1C     $ERR          IFEQ      '0'
031600000000IF  2C     V1CDET        IFEQ      'S'
031700000000     C     V1CDET        OREQ      'N'
031800000000X   2C                   ELSE
031900000000     C                   MOVEL     '1'           $ERR
032000000000     C                   MOVEL     MSG(22)       $MSG
032100000000     C                   EXSR      CARMSG
032200000000     C                   SETON                                        72
032300000000E   2C                   ENDIF
032400000000E   1C                   ENDIF
032500000000     C*
032600000000     C* CONTROLLO FLAG CREAZIONE FILE SU PC
032700000000IF  1C     $ERR          IFEQ      '0'
032800000000IF  2C     V1CFPC        IFEQ      'S'
032900000000     C     V1CFPC        OREQ      'N'
033000000000X   2C                   ELSE
033100000000     C                   MOVEL     '1'           $ERR
033200000000     C                   MOVEL     MSG(23)       $MSG
033300000000     C                   EXSR      CARMSG
033400000000     C                   SETON                                        64
033500000000E   2C                   ENDIF
033600000000E   1C                   ENDIF
033700950526     C*
033800950526     C* CONTROLLO FLAG PARTENZE/ARRIVI
033900950526IF  1C     $ERR          IFEQ      '0'
034000950526IF  2C     V1CMPA        IFEQ      'A'
034100950526     C     V1CMPA        OREQ      'P'
034200950526X   2C                   ELSE
034300950526     C                   MOVEL     '1'           $ERR
034400950526     C                   MOVEL     MSG(10)       $MSG
034500950526     C                   EXSR      CARMSG
034600950526     C                   SETON                                        61
034700950526E   2C                   ENDIF
034800950526E   1C                   ENDIF
034900000000     C*
035000000000     C* CONTROLLO LA FILIALE
035100000000IF  1C     $ERR          IFEQ      '0'
035200000000     C                   MOVEL     *BLANKS       V1DFIL
035300000000IF  2C     V1CFIL        IFNE      *ZEROS
035400000000     C     V1CFIL        ANDNE     *BLANKS
035500000000     C                   MOVEL     V1CFIL        W1CFIL            3
035600000000     C                   EXSR      CHKFIL                                       *CONTROLLA FILIALE
035700000000IF  3C     $ERR          IFEQ      '1'
035800000000     C                   MOVEL     MSG(12)       $MSG
035900000000     C                   EXSR      CARMSG
036000000000     C                   SETON                                        65
036100000000X   3C                   ELSE
036200000000     C                   MOVEL     W1CFIL        V1CFIL
036300000000     C                   MOVEL     W1CFIL        PARFIL                         *PARAMETRO
036400000000     C                   MOVEL     W1DFIL        V1DFIL                         *DESCRIZIONE FILIALE
036500000000E   3C                   ENDIF
036600000000     C*
036700000000     C* FILIALE NON SELEZIONATA: PASSO PARAMETRO A ZERO
036800000000X   2C                   ELSE
036900000000     C                   Z-ADD     *ZEROS        PARFIL
037000000000E   2C                   ENDIF
037100000000E   1C                   ENDIF
037200000000     C*
037300000000     C* CONTROLLO L'AREA
037400000000IF  1C     $ERR          IFEQ      '0'
037500000000     C                   MOVEL     *BLANKS       V1DARE
037600000000IF  2C     V1CARE        IFNE      *ZEROS
037700000000     C     V1CARE        ANDNE     *BLANKS
037800000000     C                   MOVEL     V1CARE        W1CARE            3
037900000000     C                   EXSR      CHKARE                                       *CONTROLLA AREA
038000000000IF  3C     $ERR          IFEQ      '1'
038100000000     C                   MOVEL     MSG(13)       $MSG
038200000000     C                   EXSR      CARMSG
038300000000     C                   SETON                                        66
038400000000X   3C                   ELSE
038500000000     C                   MOVEL     W1CARE        V1CARE
038600000000     C                   MOVEL     W1CARE        PARARE                         *PARAMETRO
038700000000     C                   MOVEL     W1DARE        V1DARE                         *DESCRIZIONE FILIALE
038800000000E   3C                   ENDIF
038900000000     C*
039000000000     C* AREA NON SELEZIONATA: PASSO PARAMETRO A ZERO
039100000000X   2C                   ELSE
039200000000     C                   Z-ADD     *ZEROS        PARARE
039300000000E   2C                   ENDIF
039400000000E   1C                   ENDIF
039500000000     C*
039600000000     C* CONTROLLO LA DIVISIONE
039700000000IF  1C     $ERR          IFEQ      '0'
039800000000     C                   MOVEL     *BLANKS       V1DDIV
039900000000IF  2C     V1CDIV        IFNE      *ZEROS
040000000000     C     V1CDIV        ANDNE     *BLANKS
040100000000     C                   MOVEL     V1CDIV        W1CDIV            1
040200000000     C                   EXSR      CHKDIV                                       *CONTROLLA DIVISIONE
040300000000IF  3C     $ERR          IFEQ      '1'
040400000000     C                   MOVEL     MSG(14)       $MSG
040500000000     C                   EXSR      CARMSG
040600000000     C                   SETON                                        67
040700000000X   3C                   ELSE
040800000000     C                   MOVEL     W1CDIV        V1CDIV
040900000000     C                   MOVEL     W1CDIV        PARDIV                         *PARAMETRO
041000000000     C                   MOVEL     W1DDIV        V1DDIV                         *DESCRIZIONE FILIALE
041100000000E   3C                   ENDIF
041200000000E   2C                   ENDIF
041300000000E   1C                   ENDIF
041400000000     C*
041500000000     C* CONTROLLO CHE LA SELEZIONE FRA FILIALE, AREA E DIVISIONE SIA UNA SOLA
041600000000IF  1C     $ERR          IFEQ      '0'
041700000000IF  2C     V1CFIL        IFGT      *ZEROS
041800000000     C                   ADD       1             SELEZ
041900000000E   2C                   ENDIF
042000000000IF  2C     V1CARE        IFGT      *ZEROS
042100000000     C                   ADD       1             SELEZ
042200000000E   2C                   ENDIF
042300000000IF  2C     V1CDIV        IFGT      *ZEROS
042400000000     C                   ADD       1             SELEZ
042500000000E   2C                   ENDIF
042600000000IF  2C     SELEZ         IFGT      1
042700000000     C                   MOVEL     '1'           $ERR
042800980403     C                   MOVEL     MSG(19)       $MSG
042900000000     C                   EXSR      CARMSG
043000000000     C                   SETON                                        65
043100000000E   2C                   ENDIF
043200000000E   1C                   ENDIF
043300000000     C*
043400000000     C* CONTROLLO IL TIPO PORTO
043500000000IF  1C     $ERR          IFEQ      '0'
043600000000IF  2C     V1CPOR        IFEQ      'F'
043700000000     C     V1CPOR        OREQ      'A'
043800000000     C     V1CPOR        OREQ      ' '
043900000000X   2C                   ELSE
044000000000     C                   MOVEL     '1'           $ERR
044100000000     C                   MOVEL     MSG(16)       $MSG
044200000000     C                   EXSR      CARMSG
044300000000     C                   SETON                                        69
044400000000E   2C                   ENDIF
044500000000E   1C                   ENDIF
044600000000     C*
044700000000     C* CONTROLLO IL TIPO SERVIZIO
044800000000     C                   MOVEL     *BLANKS       V1DSER
044900000000IF  1C     $ERR          IFEQ      '0'
045000000000IF  2C     V1CSER        IFNE      *BLANKS
045100000000     C                   MOVEL     V1CSER        WSER
045200000000     C                   EXSR      CHKSER
045300000000     C                   MOVEL     WSER          V1CSER
045400000000     C                   MOVEL     WDSER         V1DSER
045500000000IF  3C     $ERR          IFEQ      '1'
045600000000     C                   MOVEL     MSG(15)       $MSG
045700000000     C                   EXSR      CARMSG
045800000000     C                   SETON                                        68
045900000000E   3C                   ENDIF
046000000000E   2C                   ENDIF
046100000000E   1C                   ENDIF
046200950526     C*
046300950526     C* CONTROLLO IL TIPO DI DATI IN STATISTICA
046400950526IF  1C     $ERR          IFEQ      '0'
046500950526IF  2C     V1CIOE        IFEQ      'I'
046600950526     C     V1CIOE        OREQ      'M'
046700950526     C     V1CIOE        OREQ      'X'
046800950526     C     V1CIOE        OREQ      *BLANKS
046900950526X   2C                   ELSE
047000950526     C                   MOVEL     '1'           $ERR
047100950526     C                   MOVEL     MSG(29)       $MSG
047200950526     C                   EXSR      CARMSG
047300950526     C                   SETON                                        73
047400950526E   2C                   ENDIF
047500950526E   1C                   ENDIF
047600000000     C*--------------------
047700000000     C* ANNO/MESE FINALE
047800000000     C*--------------------
047900000000     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO CORRENTE - 1
048000000000     C* DAL 18/04/95 MESSAGGIO SONO INFORMATIVO
048100000000IF  1C     $ERR          IFEQ      '0'
048200000000IF  2C     V1CDA2        IFGT      AMCOR
048300000000     C                   MOVEL     MSG(9)        $MSG
048400000000     C                   EXSR      CARMSG
048500000000     C*---                 MOVEL'1'       $ERR
048600000000     C*---                 SETON                     62
048700000000E   2C                   ENDIF
048800000000E   1C                   ENDIF
048900000614     C*
049000000703     C* DATI POSTE
049100000614IF  1C     $ERR          IFEQ      '0'
049200000703IF  2C     V1CPOS        IFNE      'S'
049300000703     C     V1CPOS        ANDNE     'N'
049400000614     C                   MOVEL     '1'           $ERR
049500000614     C                   MOVEL     MSG(24)       $MSG
049600000614     C                   EXSR      CARMSG
049700000614     C                   SETON                                        74
049800000614E   2C                   ENDIF
049900000614E   1C                   ENDIF
050000000000     C*
050100000000     C                   ENDSR
050200000000     C*------------------------------------------------------------------------*
050300000000     C* CHKFIL - CONTROLLA LA FILIALE
050400000000     C*------------------------------------------------------------------------*
050500000000     C     CHKFIL        BEGSR
050600000000     C*
050700000000     C* CONTROLLO SE E' STATO INSERITO '?'
050800000000     C     '?'           SCAN      V1CFIL                                 99
050900000000     C*
051000000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
051100000000IF  1C     *IN99         IFEQ      *ON
051200000000     C                   MOVEL     *BLANKS       §COD1
051300000000     C                   MOVE      *BLANKS       §DES1
051400971203     C                   MOVE      *BLANKS       §PRA
051500971203     C                   CALL      'TNSD24R'
051600000000     C                   PARM                    §COD1             3
051700000000     C                   PARM                    §TIP              1
051800000000     C                   PARM                    §DES1            25
051900971203     C                   PARM                    §PRA              3
052000000000     C* SELEZIONATO
052100000000IF  2C     §DES1         IFNE      *BLANKS
052200000000     C                   MOVEL     §COD1         W1CFIL
052300000000     C                   MOVEL     §DES1         W1DFIL           25
052400000000X   2C                   ELSE
052500000000     C* NON SELEZIONATO
052600000000     C                   MOVEL     *ZEROS        W1CFIL
052700000000E   2C                   ENDIF
052800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
052900000000E   1C                   ENDIF
053000000000     C*
053100000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
053200000000     C                   MOVEL     W1CFIL        ORGFIL
053300000000     C     KEYORG        CHAIN     AZORG                              99
053400000000IF  1C     *IN99         IFEQ      *OFF
053500000000     C     ORGFVA        ANDEQ     *BLANKS
053600000000     C     ORGFAG        ANDNE     'V'
053700000000     C                   MOVEL     ORGDES        W1DFIL
053800000000X   1C                   ELSE
053900000000     C                   MOVEL     '1'           $ERR
054000000000E   1C                   ENDIF
054100000000     C*
054200000000     C                   ENDSR
054300000000     C*------------------------------------------------------------------------*
054400000000     C* CHKARE - CONTROLLA L'AREA
054500000000     C*------------------------------------------------------------------------*
054600000000     C     CHKARE        BEGSR
054700000000     C*
054800000000     C*
054900000000     C* CONTROLLO SE E' STATO INSERITO '?'
055000000000     C     '?'           SCAN      V1CARE                                 99
055100000000     C*
055200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
055300000000IF  1C     *IN99         IFEQ      *ON
055400020916     C                   MOVEL     1             §KUT
055500000000     C                   MOVEL     '05'          §COD
055600000000     C                   MOVEL     *BLANKS       §KEY
055700000000     C                   MOVEL     *BLANKS       §DES
055800000000     C                   CALL      'X§TABER'
055900000000     C                   PARM                    §KUT              1 0
056000000000     C                   PARM                    §COD              2
056100000000     C                   PARM                    §KEY              8
056200000000     C                   PARM                    §DES             30
056300000000     C                   MOVEL     §KEY          W1CARE
056400000000     C                   MOVEL     §DES          W1DARE           25
056500000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
056600000000E   1C                   ENDIF
056700000000     C*
056800000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
056900020916     C                   MOVEL     1             TBLKUT
057000000000     C                   MOVEL     '05'          TBLCOD
057100000000     C                   MOVEL     *BLANKS       TBLKEY
057200000000     C                   MOVEL     W1CARE        TBLKEY
057300000000     C     KEYTAB        CHAIN     TABEL                              99
057400000000IF  1C     *IN99         IFEQ      *ON
057500000000     C     TBLFLG        OREQ      '*'
057600000000     C                   MOVEL     '1'           $ERR
057700000000X   1C                   ELSE
057800000000     C                   MOVEL     TBLUNI        W1DARE
057900000000E   1C                   ENDIF
058000000000     C*
058100000000     C                   ENDSR
058200000000     C*------------------------------------------------------------------------*
058300000000     C* CHKDIV - CONTROLLA LA DIVISIONE
058400000000     C*------------------------------------------------------------------------*
058500000000     C     CHKDIV        BEGSR
058600000000     C*
058700000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
058800000000IF  1C     V1CDIV        IFEQ      '?'
058900020916     C                   MOVEL     1             §KUT
059000000000     C                   MOVEL     '17'          §COD
059100000000     C                   MOVEL     *BLANKS       §KEY
059200000000     C                   MOVEL     *BLANKS       §DES
059300000000     C                   CALL      'X§TABER'
059400000000     C                   PARM                    §KUT
059500000000     C                   PARM                    §COD
059600000000     C                   PARM                    §KEY
059700000000     C                   PARM                    §DES
059800000000     C                   MOVEL     §KEY          W1CDIV
059900000000     C                   MOVEL     §DES          W1DDIV           25
060000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
060100000000E   1C                   ENDIF
060200000000     C*
060300000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
060400020916     C                   MOVEL     1             TBLKUT
060500000000     C                   MOVEL     '17'          TBLCOD
060600000000     C                   MOVEL     *BLANKS       TBLKEY
060700000000     C                   MOVEL     W1CDIV        TBLKEY
060800000000     C     KEYTAB        CHAIN     TABEL                              99
060900000000IF  1C     *IN99         IFEQ      *ON
061000000000     C     TBLFLG        OREQ      '*'
061100000000     C                   MOVEL     '1'           $ERR
061200000000X   1C                   ELSE
061300000000     C                   MOVEL     TBLUNI        W1DDIV
061400000000E   1C                   ENDIF
061500000000     C*
061600000000     C                   ENDSR
061700000000     C*------------------------------------------------------------------------*
061800000000     C* CHKSER - CONTROLLA IL TIPO SERVIZIO
061900000000     C*------------------------------------------------------------------------*
062000000000     C     CHKSER        BEGSR
062100000000     C*
062200000000     C     *LIKE         DEFINE    V1CSER        WSER
062300000000     C     *LIKE         DEFINE    V1DSER        WDSER
062400000000     C*
062500000000     C                   MOVEL     *BLANKS       WDSER
062600000000     C*
062700000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
062800000000IF  1C     WSER          IFEQ      '?'
062900020916     C                   MOVEL     1             §KUT
063000000000     C                   MOVEL     '5E'          §COD
063100000000     C                   MOVEL     *BLANKS       §KEY
063200000000     C                   MOVEL     *BLANKS       §DES
063300000000     C                   CALL      'X§TABER'
063400000000     C                   PARM                    §KUT
063500000000     C                   PARM                    §COD
063600000000     C                   PARM                    §KEY
063700000000     C                   PARM                    §DES
063800000000     C                   MOVEL     §KEY          WSER
063900000000     C                   MOVEL     §DES          WDSER
064000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
064100000000E   1C                   ENDIF
064200000000     C*
064300000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
064400020916     C                   MOVEL     1             TBLKUT
064500000000     C                   MOVEL     '5E'          TBLCOD
064600000000     C                   MOVEL     *BLANKS       TBLKEY
064700000000     C                   MOVEL     WSER          TBLKEY
064800000000     C     KEYTAB        CHAIN     TABEL                              99
064900000000IF  1C     *IN99         IFEQ      *ON
065000000000     C     TBLFLG        OREQ      '*'
065100000000     C                   MOVEL     '1'           $ERR
065200000000X   1C                   ELSE
065300000000     C                   MOVEL     TBLUNI        WDSER
065400000000E   1C                   ENDIF
065500000000     C*
065600000000     C                   ENDSR
065700000000     C*------------------------------------------------------------------------*
065800000000     C* IMPVID  - IMPOSTAZIONI INIZIALI VIDEO
065900000000     C*------------------------------------------------------------------------*
066000000000     C     IMPVID        BEGSR
066100000614     C*
066200000000     C*--------------------
066300000000     C* D A T A  F I N E --> ANNO/MESE CORRENTE - 1 MESE
066400000000     C*--------------------
066500990527     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
066600990527     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
066700990527     C                   Z-ADD     N8            DDATA8                         *SCOMPOSIZIONE DATA
066800990527     C*
066900990527     C                   Z-ADD     DANNO8        ANNO              4 0
067000990527     C                   Z-ADD     DMESE8        MESE              2 0
067100000000     C                   SUB       1             MESE
067200000000IF  1C     MESE          IFEQ      *ZEROS                                       *SE GENNAIO
067300000000     C                   Z-ADD     12            MESE                           *IMPOSTO DICEMBRE
067400000000     C                   SUB       1             ANNO                           *DELL'ANNO PRECEDENTE
067500000000E   1C                   ENDIF
067600000000     C                   MOVEL     ANNO          V1CDA2
067700000000     C                   MOVE      MESE          V1CDA2
067800990527     C*
067900990527     C                   Z-ADD     V1CDA2        AMCOR             6 0          *ANN/MESE CORRENTE
068000000000     C*--------------------
068100000000     C* D A T A  I N I Z I O --> INIIZO DELL'ANNO
068200000000     C*--------------------
068300000000     C                   Z-ADD     V1CDA2        V1CDAT
068400000000     C                   MOVE      01            V1CDAT
068500000000     C*--------------------
068600000000     C* RAGIONE SOCIALE
068700000000     C*--------------------
068800020916     C                   MOVEL     RSUT          VTDRSU
068900000000     C*--------------------
069000000000     C* RICHIESTE
069100000000     C*--------------------
069200950526     C                   MOVEL     'P'           V1CMPA                         *TIPO MOVIMENTO
069300000000     C                   MOVEL     'N'           V1CDET                         *DETTAGLIO FIL/FIL
069400950531     C                   MOVEL     'S'           V1CFPC                         *CREAZIONE FILE SU PC
069500950526     C                   MOVEL     'I'           V1CIOE                         *DATI IN STATISTICA
069600000703     C                   MOVEL     'N'           V1CPOS
069700000000     C*
069800000000     C* IMPOSTAZIONE TEMPORANEA
069900950713     C*---                 MOVEL'N'       V1CSPE           *SPEDIZIONI NORMALI
070000000000     C*
070100000000     C                   ENDSR
070200020916     C*--------------------------------------------------------------------------------------------*
070300020916     C* REPERISCE I DATI UTENTE
070400020916     C*--------------------------------------------------------------------------------------------*
070500020916     C     REPDATIUTE    BEGSR
070600020916     C*
070700020916     C* INIZIALIZZA VARIABILI DI WRK
070800020916     C                   CLEAR                   TIBS34DS
070900020916     C                   CLEAR                   AZUTEDS
071000020916     C                   CLEAR                   DDATIUTE
071100020916     C*
071200020916     C     *DTAARA       DEFINE    §azute        azuteds
071300020916     C     *DTAARA       DEFINE    §datiute      ddatiute
071400020916     C                   IN(E)     *DTAARA
071500020916if  1C                   IF        %Error
071600020916     c                   EVAL      I34Tla = 'L'
071700020916     C                   CALL      'TIBS34R'
071800020916     C                   PARM                    Tibs34Ds
071900020916     C                   IN        *DTAARA
072000020916e   1C                   ENDIF
072100020916     C*
072200020916     C                   ENDSR
072300000000     C*------------------------------------------------------------------------*
072400000000     C* *INZSR  - OPERAZIONI INIZIALI
072500000000     C*------------------------------------------------------------------------*
072600000000     C     *INZSR        BEGSR
072700950526     C*--------------------
072800000000     C* RICEVIMENTO PARAMETRI
072900950526     C*--------------------
073000000000     C     *ENTRY        PLIST
073100000000     C                   PARM                    KPJBA
073200950526     C*--------------------
073300950526     C* CHIAVI DI LETTURA
073400950526     C*--------------------
073500000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
073600000000     C     KEYORG        KLIST
073700000000     C                   KFLD                    ORGFIL
073800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
073900000000     C     KEYTAB        KLIST
074000000000     C                   KFLD                    TBLKUT
074100000000     C                   KFLD                    TBLCOD
074200000000     C                   KFLD                    TBLKEY
074300000000     C*
074400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
074500000000     C                   EXSR      INZSTD
074600000000     C*
074700000000     C* IMPOSTAZIONI INIZIALI VIDEO
074800000000     C                   EXSR      IMPVID
074900000000     C*
075000950526     C* EMETTE FORMATO VIDEO
075100950526     C                   EXSR      CARV1
075200000000     C*
075300000000     C                   ENDSR
075400000000     C*------------------------------------------------------------------------*
075500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075700000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
075800000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
075900000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
076000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076200000000     C*------------------------------------------------------------------------*
076300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
076400000000     C*------------------------------------------------------------------------*
076500000000     C     INZSTD        BEGSR
076600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
076700000000     C                   CALL      'XKEYS'
076800000000     C                   PARM                    DSKEYS
076900000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
077000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
077100000000     C     XMSGPA        PLIST
077200000000     C                   PARM                    DSMSMS
077300000000     C                   PARM                    DSMSPG
077400000000     C                   PARM                    DSMSKE
077500000000     C                   PARM                    DSMSER
077600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
077700020115     C                   MOVEL     'TISE45R'     DSMSPG                         *NOME PGM CHIAMANTE
077800020115     C                   MOVEL     'TISE45R'     VMCQUE                         *CODA MESSAGGI DESTIN
077900000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
078000000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
078100000000     C* INIZIALIZZO FLAG CONTROLLO PGM
078200000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
078300000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
078400000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
078500000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
078600000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
078700000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
078800000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
078900000000     C* LANCIO ROUTINE PER VERIFICARE SE
079000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
079100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
079200020916     C                   EXSR      REPDATIUTE
079300000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
079400000000     C                   BITOFF    '01234567'    POSMI1            1
079500000000     C                   BITOFF    '01234567'    POSMI2            1
079600000000     C                   BITOFF    '01234567'    POSMA1            1
079700000000     C                   BITOFF    '01234567'    POSMA2            1
079800000000     C                   BITON     '34'          POSMI1            1            *18
079900000000     C                   BITON     '7'           POSMI2            1            *01
080000000000     C                   BITON     '34'          POSMA1            1            *18
080100000000     C                   BITON     '0'           POSMA2            1            *80
080200000000     C                   MOVE      POSMI2        POSMIN            2
080300000000     C                   MOVEL     POSMI1        POSMIN
080400000000     C                   MOVE      POSMA2        POSMAX            2
080500000000     C                   MOVEL     POSMA1        POSMAX
080600000000     C                   ENDSR
080700000000     C*------------------------------------------------------------------------*
080800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
080900000000     C*------------------------------------------------------------------------*
081000000000     C     FUNSTD        BEGSR
081100000000     C                   SELECT
081200000000     C     KEY           WHENEQ    F3
081300000000     C                   EXSR      F03STD
081400000000     C     KEY           WHENEQ    F12
081500000000     C                   EXSR      F12STD
081600000000     C     KEY           WHENEQ    F24
081700000000     C                   EXSR      F24STD
081800000000     C                   OTHER
081900000000     C                   MOVEL     MSG(2)        $MSG
082000000000     C                   EXSR      CARMSG
082100000000     C                   ENDSL
082200000000     C                   ENDSR
082300000000     C*------------------------------------------------------------------------*
082400000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
082500000000     C*------------------------------------------------------------------------*
082600000000     C     F03STD        BEGSR
082700000000     C*
082800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
082900000000     C                   MOVEL     '1'           $FINE
083000000000     C*
083100000000     C                   ENDSR
083200000000     C*------------------------------------------------------------------------*
083300000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
083400000000     C*------------------------------------------------------------------------*
083500000000     C     F12STD        BEGSR
083600000000     C*
083700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
083800000000     C                   MOVEL     '1'           $FINE
083900000000     C*
084000000000     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
084300000000     C*------------------------------------------------------------------------*
084400000000     C     F24STD        BEGSR
084500000000     C*
084600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
084700000000     C                   EXSR      OUTFUN
084800000000     C*
084900000000     C                   ENDSR
085000000000     C*------------------------------------------------------------------------*
085100000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
085200000000     C*          CODA MESSAGGI DEL PGM
085300000000     C*------------------------------------------------------------------------*
085400000000     C     CARMSG        BEGSR
085500000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
085600000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
085700000000     C     MSGULT        IFEQ      0
085800000000     C                   MOVEL     '1'           *IN40
085900020115     C                   WRITE     SE45DVK
086000000000     C                   MOVEL     '0'           *IN40
086100000000     C                   ENDIF
086200000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
086300000000     C                   MOVEL     $MSG          DSMSMS
086400000000     C                   CALL      'XMSG'        XMSGPA
086500000000     C     DSMSER        IFEQ      0
086600000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
086700000000     C                   ENDIF
086800000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
086900000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
087000020115     C                   WRITE     SE45DVM                                      *SFL MESS
087100000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
087200000000     C                   MOVEL     *BLANKS       $MSG
087300000000     C                   ENDSR
087400000000     C*------------------------------------------------------------------------*
087500000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
087600000000     C*------------------------------------------------------------------------*
087700000000     C     OUTMSG        BEGSR
087800000000     C* VISUALIZZAZIONE MESSAGGIO
087900000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
088000020115     C                   WRITE     SE45DVK                                      *CONTR MESS
088100000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
088200000000     C                   Z-ADD     *ZEROS        MSGULT
088300000000     C                   ENDSR
088400000000     C*------------------------------------------------------------------------*
088500000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
088600000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
088700000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
088800000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
088900000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
089000000000     C*------------------------------------------------------------------------*
089100000000     C     CARFUN        BEGSR
089200000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
089300000000     C     1             DO        31            I                 5 0          --- 1 -->
089400000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
089500000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
089600000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
089700020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
089800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
089900000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
090000000000     C                   ELSE                                                   *INTERROG.
090100000000     C                   MOVEL     FUNASI        FAT(I)
090200000000     C                   ENDIF
090300000000     C                   ELSE                                                   <-- 2 --> *FILIALE
090400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
090500000000     C                   MOVEL     FUNAFG        FAT(I)
090600000000     C                   ELSE                                                   *INTERROG.
090700000000     C                   MOVEL     FUNAFI        FAT(I)
090800000000     C                   ENDIF
090900000000     C                   ENDIF                                                  <-- 2 ---
091000000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
091100000000     C                   MOVEL     USER          WEDP              3            *PROFILO
091200000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
091300000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
091400000000     C                   ELSE
091500000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
091600000000     C                   ENDIF
091700000000     C                   ENDIF
091800000000     C                   ENDDO                                                  <-- 1 ---
091900000000     C                   ENDSR
092000000000     C*------------------------------------------------------------------------*
092100000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
092200000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
092300000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
092400000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
092500000000     C*------------------------------------------------------------------------*
092600000000     C     OUTFUN        BEGSR
092700000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
092800000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
092900000000     C                   ENDDO
093000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
093100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
093200000000     C     1             DO        31            I
093300000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
093400000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
093500000000     C                   ADD       1             FUNNAT
093600000000     C                   ENDIF
093700000000     C                   ENDDO
093800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
093900000000     C     FUNNAT        IFGT      8
094000000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
094100000000     C                   ENDIF
094200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
094300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
094400000000     C                   Z-ADD     8             FUNVIS            5 0
094500000000     C     FAT(23)       IFEQ      'S'
094600000000     C                   SUB       1             FUNVIS
094700000000     C                   ENDIF
094800000000     C     FAT(24)       IFEQ      'S'
094900000000     C                   SUB       1             FUNVIS
095000000000     C                   ENDIF
095100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
095200000000     C                   Z-ADD     *ZEROS        W                 5 0
095300000000     C                   ADD       1             FUNULT
095400000000     C     FUNULT        IFGT      31
095500000000     C                   Z-ADD     1             FUNULT
095600000000     C                   ENDIF
095700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
095800000000     C     FUNULT        DO        31            I                              --- 1 -->
095900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
096000000000     C     W             IFLT      FUNVIS                                       --- 2 -->
096100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
096200000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
096300000000     C     FDE(I)        ANDNE     *BLANKS
096400000000     C     I             IFEQ      23                                           *TOLGO F23
096500000000     C     I             OREQ      24                                           *E F24
096600000000     C                   ELSE
096700000000     C                   ADD       1             W
096800000000     C                   MOVEL     FDE(I)        FUV(W)
096900000000     C                   ENDIF
097000000000     C                   ENDIF                                                  <-- 3 ---
097100000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
097200000000     C                   Z-ADD     I             FUNULT
097300000000     C                   ENDIF                                                  <-- 2 ---
097400000000     C                   ENDDO                                                  <-- 1 ---
097500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
097600000000     C                   MOVEL     FUV(1)        VFD001
097700000000     C                   MOVEL     FUV(2)        VFD002
097800000000     C                   MOVEL     FUV(3)        VFD003
097900000000     C                   MOVEL     FUV(4)        VFD004
098000000000     C                   MOVEL     FUV(5)        VFD005
098100000000     C                   MOVEL     FUV(6)        VFD006
098200000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
098300000000     C     FAT(24)       ANDEQ     'N'
098400000000     C                   MOVEL     FUV(7)        VFD007
098500000000     C                   MOVEL     FUV(8)        VFD008
098600000000     C                   ELSE                                                   <-- 1 -->
098700000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
098800000000     C     FAT(24)       ANDEQ     'S'
098900000000     C                   MOVEL     FDE(23)       VFD007
099000000000     C                   MOVEL     FDE(24)       VFD008
099100000000     C                   ELSE                                                   <-- 2 -->
099200000000     C                   MOVEL     FUV(7)        VFD007
099300000000     C     FAT(23)       IFEQ      'S'
099400000000     C                   MOVEL     FDE(23)       VFD008
099500000000     C                   ELSE
099600000000     C                   MOVEL     FDE(24)       VFD008
099700000000     C                   ENDIF
099800000000     C                   ENDIF                                                  <-- 2 ---
099900000000     C                   ENDIF                                                  <-- 1 ---
100000000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
100100000000     C                   ENDSR
100200000000     C*------------------------------------------------------------------------*
100300000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
100400000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
100500000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
100600000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
100700000000     C*------------------------------------------------------------------------*
100800000000     C     CONFUN        BEGSR
100900000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
101000000000     C     1             DO        31            I                              --- 1 -->
101100000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
101200000000     C* CONTROLLO SE IL TASTO E' ATTIVO
101300000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
101400000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
101500000000     C                   EXSR      CARMSG
101600000000     C                   MOVEL     '1'           $ERR
101700000000     C                   ENDIF                                                  <-- 3 ---
101800000000     C                   ENDIF                                                  <-- 2 ---
101900000000     C                   ENDDO                                                  <-- 1 ---
102000000000     C                   ENDSR
102100000000     O*------------------------------------------------------------------------*
102200000000**   MSG - MESSAGGI
102300020115TISE45R- Tasto funzionale non valido.                                          1
102400020115TISE45R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
102500020115TISE45R- Opzione non valida.                                                   3
102600020115TISE45R- Opzione abilitata ma non gestita. Avvertire CED.                      4
102700020115TISE45R- Inizio elenco.                                                        5
102800020115TISE45R- Fine elenco.                                                          6
102900020115TISE45R- Anno/Mese obbligatorio                                                7
103000020115TISE45R- Anno/Mese errato                                                      8
103100020115TISE45R- Data superiore alla data creazione saldi                              9
103200020115TISE45R- 'P' per elaborare i movimenti in partenza,'A' per quelli in arrivo   10
103300020115TISE45R- Anno/Mese finale inferiore all'Anno/Mese iniziale                    11
103400020115TISE45R- Punto operativo errato                                               12
103500020115TISE45R- Area errata                                                          13
103600020115TISE45R- Divisione errata                                                     14
103700020115TISE45R- Tipo servizio non accettabile                                        15
103800020115TISE45R- Tipo porto non accettabile                                           16
103900020115TISE45R- Tipo bolla non accettabile                                           17
104000020115TISE45R- Tipo prodotto non accettabile                                        18
104100020115TISE45R- Immettere solo una selezione per punto operativo, area o divisione   19
104200020115TISE45R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
104300020115TISE45R- Impossibile stampare più di 12 mesi                                  21
104400020115TISE45R- 'S' per stampare il dettaglio P.O./P.O. o 'N' per non stamparlo      22
104500020115TISE45R- 'S' per creare file su PC, 'N' per non crearlo                       23
104600020115TISE45R- Immettere uno dei valori possibili                                   24
104700020325TISE45R- Impossibile lanciare la statistica su anni differenti                25
104800020115TISE45R- 26
104900020115TISE45R- 27
105000020115TISE45R- 28
105100020115TISE45R- Immettere il tipo di dati che si vuole in statistica                 29
105200020115TISE45R- 30
105300000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
105400000000F1    -                   -    NN     NN
105500000000F2    -                   -    NN     NN
105600000000F3    -F3=Fine            -    SS     SS
105700000000F4    -                   -    NN     NN
105800000000F5    -                   -    NN     NN
105900000000F6    -F6=Lancio          -    SS     SS
106000000000F7    -                   -    NN     NN
106100000000F8    -                   -    NN     NN
106200000000F9    -                   -    NN     NN
106300000000F10   -                   -    NN     NN
106400000000F11   -                   -    NN     NN
106500000000F12   -F12-Ritorno        -    SS     SS
106600000000F13   -                   -    NN     NN
106700000000F14   -                   -    NN     NN
106800000000F15   -                   -    NN     NN
106900000000F16   -                   -    NN     NN
107000000000F17   -                   -    NN     NN
107100000000F18   -                   -    NN     NN
107200000000F19   -                   -    NN     NN
107300000000F20   -                   -    NN     NN
107400000000F21   -                   -    NN     NN
107500000000F22   -                   -    NN     NN
107600000000F23   -                   -    NN     NN
107700000000F24   -F24=Altri tasti    -    NN     NN
107800000000ENTER -                   -    SS     SS
107900000000ROLLUP-                   -    NN     NN
108000000000ROLLDN-                   -    NN     NN
108100000000HELP  -                   -    NN     NN
108200000000PRINT -                   -    NN     NN
108300000000BSPACE-                   -    NN     NN
108400000000CLEAR -                   -    NN     NN
