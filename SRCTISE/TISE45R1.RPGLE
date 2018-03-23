000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA MOVIMENTO - RICHIESTA PARAMETRI                             *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800160302     FTISE45D1  CF   E             WORKSTN INFDS(DEVDS1)
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
006600160302     D TIBS02DS      E DS                                                       *Profili utente
006700000000     D*------------------
006800000000     D* PARAMETRI DI LANCIO
006900000000     D*------------------
007000000000     D PARAM           DS                  INZ
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
008400000000     D*-------------------
008500020916     D* DS REPERIMENTO DATI UTENTE
008600000000     D*-------------------
008700020916     D TIBS34DS      E DS                                                       *Profili utente
008800020916     D DDATIUTE      E DS                                                       *Dati utente
008900020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009000160302     D dute01        E DS                                                       *Utenti
009100160302     d dLat          e ds
009200160302     D TRUL31DS      E DS
009300160302     D TRUL31DS2     E DS
009400160302     d wabi            s                   like(UteAut)
009500160302     d* p.o. abilitati
009600160302     D $pog            s              3    dim(250)
009700160302     D $DIG            s              1    dim(20)
009800160302     D $ARG            s              3    dim(50)
009900000000     D*-------------------
010000000000     D* DS RAGGRUPPAMENTI BOLLA
010100000000     D*-------------------
010200000000     D DS2W          E DS
010300990527     D*------------------
010400990527     D* DS CONTROLLO DATA (8)
010500990527     D*------------------
010600990527     D WLBDA8          DS                  INZ
010700990527     D  G08DAT                 1      8  0
010800990527     D  G08INV                 9     16  0
010900990527     D  G08ERR                17     17
011000990527     D  G08TGI                18     22  0
011100990527     D*------------------
011200990527     D* DS DATA GENERICA(8) IN GG/MM/AAAA
011300990527     D*------------------
011400990527     D                 DS
011500990527     D  DGIOR8                 1      2  0
011600990527     D  DMESE8                 3      4  0
011700990527     D  DANNO8                 5      8  0
011800990527     D  DDATA8                 1      8  0
011900000000     C*------------------------------------------------------------------------*
012000000000     C* MAIN LINE
012100000000     C*------------------------------------------------------------------------*
012200000000     C*
012300000000     C* CICLO FINO A CHE:
012400000000DO  1C     $FINE         DOUEQ     '1'
012500000000     C*
012600000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
012700000000     C*
012800000000     C* CONTROLLA GESTIONE VIDEO
012900000000IF  2C     $GES          IFEQ      'V1'
013000000000     C                   EXSR      GESV1                                        *PARAMETRI
013100000000E   2C                   ENDIF
013200000000     C*
013300000000E   1C                   ENDDO
013400000000     C*
013500000000     C                   SETON                                        LR
013600000000     C*------------------------------------------------------------------------*
013700000000     C* GESV1 - GESTIONE PRIMO VIDEO
013800000000     C*------------------------------------------------------------------------*
013900000000     C     GESV1         BEGSR
014000000000     C*
014100000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
014200000000     C                   EXSR      CARV1                                        *CARICA VIDEO
014300000000E   1C                   ENDIF
014400000000     C*
014500000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
014600000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
014700000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
014800000000E   1C                   ENDIF
014900000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
015000000000     C*
015100020115     C                   EXFMT     SE45DV1                                      *PARAMETRI
015200000000     C*
015300000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
015400000000     C*
015500000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
015600000000SL  2C                   SELECT
015700000000     C     KEY           WHENEQ    ENTER
015800000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015900000000     C     KEY           WHENEQ    F6
016000000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
016100000000     C                   OTHER
016200000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
016300000000E   2C                   ENDSL
016400000000E   1C                   ENDIF
016500000000     C*
016600000000     C                   ENDSR
016700000000     C*------------------------------------------------------------------------*
016800000000     C* CARV1 - CARICA PRIMO VIDEO
016900000000     C*------------------------------------------------------------------------*
017000000000     C     CARV1         BEGSR
017100000000     C*
017200000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
017300000000     C*
017400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
017500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
017600000000     C*
017700020115     C                   WRITE     SE45DVT                                      *TESTATA
017800020115     C                   WRITE     SE45DVF                                      *FUNZIONI
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*------------------------------------------------------------------------*
018200000000     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
018300000000     C*------------------------------------------------------------------------*
018400000000     C     ENTV1         BEGSR
018500000000     C*
018600000000     C* CONTROLLA I CAMPI IMMESSI
018700000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018800000000     C*
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
019200000000     C*------------------------------------------------------------------------*
019300000000     C     F06GES        BEGSR
019400000000     C*
019500000000     C* CONTROLLA I CAMPI IMMESSI
019600000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
019700000000     C*
019800000000     C* NON CI SONO ERRORI
019900000000IF  1C     $ERR          IFEQ      '0'
020000000000     C*
020100000000     C* LANCIO PROGRAMMA DI STAMPA
020200000000     C                   Z-ADD     V1CDAT        PARDAT
020300000000     C                   Z-ADD     V1CDA2        PARDA2
020400000000     C                   MOVEL     V1CMPA        PARMPA
020500000000     C                   MOVEL     V1CSER        PARSER
020600000000     C                   MOVEL     V1CPOR        PARPOR
020700000000     C                   MOVEL     V1CSPE        PARSPE
020800000000     C                   MOVEL     V1CPRO        PARPRO
020900000000     C                   MOVEL     V1CDET        PARDET
021000950526     C                   MOVEL     V1CIOE        PARIOE
021100000000     C*
021200160503     C                   MOVEL     'SE4C'        KCOAZ
021300000000     C                   MOVEL     PARAM         KPJBU
021400160504 MOM C                   CALL      'BCH10'
021500160504     C*m                 CALL      'TISE46R1'
021600000000     C                   PARM                    KPJBA
021700000000E   1C                   ENDIF
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
033200950526     C*
033300950526     C* CONTROLLO FLAG PARTENZE/ARRIVI
033400950526IF  1C     $ERR          IFEQ      '0'
033500950526IF  2C     V1CMPA        IFEQ      'A'
033600950526     C     V1CMPA        OREQ      'P'
033700950526X   2C                   ELSE
033800950526     C                   MOVEL     '1'           $ERR
033900950526     C                   MOVEL     MSG(10)       $MSG
034000950526     C                   EXSR      CARMSG
034100950526     C                   SETON                                        61
034200950526E   2C                   ENDIF
034300950526E   1C                   ENDIF
034400000000     C*
034500000000     C* CONTROLLO LA FILIALE
034600000000IF  1C     $ERR          IFEQ      '0'
034700000000     C                   MOVEL     *BLANKS       V1DFIL
034800000000IF  2C     V1CFIL        IFNE      *ZEROS
034900000000     C     V1CFIL        ANDNE     *BLANKS
035000160426     C                   MOVEL     V1CFIL        W1CFIL            3
035100000000     C                   EXSR      CHKFIL                                       *CONTROLLA FILIALE
035200160426     C     w1cfil        lookup    $pog                                   79
035300160302     c  n79              move      '1'           $err
035400000000IF  3C     $ERR          IFEQ      '1'
035500000000     C                   MOVEL     MSG(12)       $MSG
035600000000     C                   EXSR      CARMSG
035700000000     C                   SETON                                        65
035800000000X   3C                   ELSE
035900000000     C                   MOVEL     W1CFIL        V1CFIL
036000000000     C                   MOVEL     W1CFIL        PARFIL                         *PARAMETRO
036100000000     C                   MOVEL     W1DFIL        V1DFIL                         *DESCRIZIONE FILIALE
036200000000E   3C                   ENDIF
036300000000     C*
036400000000     C* FILIALE NON SELEZIONATA: PASSO PARAMETRO A ZERO
036500000000X   2C                   ELSE
036600000000     C                   Z-ADD     *ZEROS        PARFIL
036700000000E   2C                   ENDIF
036800000000E   1C                   ENDIF
036900000000     C*
037000000000     C* CONTROLLO L'AREA
037100000000IF  1C     $ERR          IFEQ      '0'
037200000000     C                   MOVEL     *BLANKS       V1DARE
037300000000IF  2C     V1CARE        IFNE      *ZEROS
037400000000     C     V1CARE        ANDNE     *BLANKS
037500000000     C                   MOVEL     V1CARE        W1CARE            3
037600000000     C                   EXSR      CHKARE                                       *CONTROLLA AREA
037700160302     C     w1care        lookup    $arg                                   79
037800160302     c  n79              move      '1'           $err
037900000000IF  3C     $ERR          IFEQ      '1'
038000000000     C                   MOVEL     MSG(13)       $MSG
038100000000     C                   EXSR      CARMSG
038200000000     C                   SETON                                        66
038300000000X   3C                   ELSE
038400000000     C                   MOVEL     W1CARE        V1CARE
038500000000     C                   MOVEL     W1CARE        PARARE                         *PARAMETRO
038600000000     C                   MOVEL     W1DARE        V1DARE                         *DESCRIZIONE FILIALE
038700000000E   3C                   ENDIF
038800000000     C*
038900000000     C* AREA NON SELEZIONATA: PASSO PARAMETRO A ZERO
039000000000X   2C                   ELSE
039100000000     C                   Z-ADD     *ZEROS        PARARE
039200000000E   2C                   ENDIF
039300000000E   1C                   ENDIF
039400000000     C*
039500000000     C* CONTROLLO LA DIVISIONE
039600000000IF  1C     $ERR          IFEQ      '0'
039700000000     C                   MOVEL     *BLANKS       V1DDIV
039800000000IF  2C     V1CDIV        IFNE      *ZEROS
039900000000     C     V1CDIV        ANDNE     *BLANKS
040000000000     C                   MOVEL     V1CDIV        W1CDIV            1
040100000000     C                   EXSR      CHKDIV                                       *CONTROLLA DIVISIONE
040200160302     C     w1cdiv        lookup    $dig                                   79
040300160302     c  n79              move      '1'           $err
040400000000IF  3C     $ERR          IFEQ      '1'
040500000000     C                   MOVEL     MSG(14)       $MSG
040600000000     C                   EXSR      CARMSG
040700000000     C                   SETON                                        67
040800000000X   3C                   ELSE
040900000000     C                   MOVEL     W1CDIV        V1CDIV
041000000000     C                   MOVEL     W1CDIV        PARDIV                         *PARAMETRO
041100000000     C                   MOVEL     W1DDIV        V1DDIV                         *DESCRIZIONE FILIALE
041200000000E   3C                   ENDIF
041300000000E   2C                   ENDIF
041400000000E   1C                   ENDIF
041500000000     C*
041600000000     C* CONTROLLO CHE LA SELEZIONE FRA FILIALE, AREA E DIVISIONE SIA UNA SOLA
041700000000IF  1C     $ERR          IFEQ      '0'
041800000000IF  2C     V1CFIL        IFGT      *ZEROS
041900000000     C                   ADD       1             SELEZ
042000000000E   2C                   ENDIF
042100000000IF  2C     V1CARE        IFGT      *ZEROS
042200000000     C                   ADD       1             SELEZ
042300000000E   2C                   ENDIF
042400000000IF  2C     V1CDIV        IFGT      *ZEROS
042500000000     C                   ADD       1             SELEZ
042600000000E   2C                   ENDIF
042700000000IF  2C     SELEZ         IFGT      1
042800000000     C                   MOVEL     '1'           $ERR
042900980403     C                   MOVEL     MSG(19)       $MSG
043000000000     C                   EXSR      CARMSG
043100000000     C                   SETON                                        65
043200000000E   2C                   ENDIF
043300000000E   1C                   ENDIF
043400000000     C*
043500160421     C* CONTROLLO CHE LA SELEZIONE FRA FILIALE, AREA E DIVISIONE SIA UNA SOLA
043600160421IF  1C     dutpou        IFne      046
043700160421IF  1C     $ERR          IFEQ      '0'
043800160422IF  2C     V1CFIL        IFle      *ZEROS
043900160422IF  2C     V1CARE        andle     *ZEROS
044000160712IF  2C     V1CDIV        andeq     *blank
044100160421     C                   MOVEL     '1'           $ERR
044200160421     C                   MOVEL     MSG(26)       $MSG
044300160421     C                   EXSR      CARMSG
044400160421     C                   SETON                                        656667
044500160421E   2C                   ENDIF
044600160421E   1C                   ENDIF
044700160421E   1C                   endif
044800160421     C*
044900000000     C* CONTROLLO IL TIPO PORTO
045000000000IF  1C     $ERR          IFEQ      '0'
045100000000IF  2C     V1CPOR        IFEQ      'F'
045200000000     C     V1CPOR        OREQ      'A'
045300000000     C     V1CPOR        OREQ      ' '
045400000000X   2C                   ELSE
045500000000     C                   MOVEL     '1'           $ERR
045600000000     C                   MOVEL     MSG(16)       $MSG
045700000000     C                   EXSR      CARMSG
045800000000     C                   SETON                                        69
045900000000E   2C                   ENDIF
046000000000E   1C                   ENDIF
046100000000     C*
046200000000     C* CONTROLLO IL TIPO SERVIZIO
046300000000     C                   MOVEL     *BLANKS       V1DSER
046400000000IF  1C     $ERR          IFEQ      '0'
046500000000IF  2C     V1CSER        IFNE      *BLANKS
046600000000     C                   MOVEL     V1CSER        WSER
046700000000     C                   EXSR      CHKSER
046800000000     C                   MOVEL     WSER          V1CSER
046900000000     C                   MOVEL     WDSER         V1DSER
047000000000IF  3C     $ERR          IFEQ      '1'
047100000000     C                   MOVEL     MSG(15)       $MSG
047200000000     C                   EXSR      CARMSG
047300000000     C                   SETON                                        68
047400000000E   3C                   ENDIF
047500000000E   2C                   ENDIF
047600000000E   1C                   ENDIF
047700950526     C*
047800950526     C* CONTROLLO IL TIPO DI DATI IN STATISTICA
047900950526IF  1C     $ERR          IFEQ      '0'
048000950526IF  2C     V1CIOE        IFEQ      'I'
048100950526     C     V1CIOE        OREQ      'M'
048200950526     C     V1CIOE        OREQ      'X'
048300950526     C     V1CIOE        OREQ      *BLANKS
048400950526X   2C                   ELSE
048500950526     C                   MOVEL     '1'           $ERR
048600950526     C                   MOVEL     MSG(29)       $MSG
048700950526     C                   EXSR      CARMSG
048800950526     C                   SETON                                        73
048900950526E   2C                   ENDIF
049000950526E   1C                   ENDIF
049100000000     C*--------------------
049200000000     C* ANNO/MESE FINALE
049300000000     C*--------------------
049400000000     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO CORRENTE - 1
049500000000     C* DAL 18/04/95 MESSAGGIO SONO INFORMATIVO
049600000000IF  1C     $ERR          IFEQ      '0'
049700000000IF  2C     V1CDA2        IFGT      AMCOR
049800000000     C                   MOVEL     MSG(9)        $MSG
049900000000     C                   EXSR      CARMSG
050000000000E   2C                   ENDIF
050100000000E   1C                   ENDIF
050200000000     C*
050300000000     C                   ENDSR
050400000000     C*------------------------------------------------------------------------*
050500000000     C* CHKFIL - CONTROLLA LA FILIALE
050600000000     C*------------------------------------------------------------------------*
050700000000     C     CHKFIL        BEGSR
050800000000     C*
050900000000     C* CONTROLLO SE E' STATO INSERITO '?'
051000000000     C     '?'           SCAN      V1CFIL                                 99
051100000000     C*
051200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
051300000000IF  1C     *IN99         IFEQ      *ON
051400000000     C                   MOVEL     *BLANKS       §COD1
051500000000     C                   MOVE      *BLANKS       §DES1
051600971203     C                   MOVE      *BLANKS       §PRA
051700971203     C                   CALL      'TNSD24R'
051800000000     C                   PARM                    §COD1             3
051900000000     C                   PARM                    §TIP              1
052000000000     C                   PARM                    §DES1            25
052100971203     C                   PARM                    §PRA              3
052200000000     C* SELEZIONATO
052300000000IF  2C     §DES1         IFNE      *BLANKS
052400000000     C                   MOVEL     §COD1         W1CFIL
052500000000     C                   MOVEL     §DES1         W1DFIL           25
052600000000X   2C                   ELSE
052700000000     C* NON SELEZIONATO
052800000000     C                   MOVEL     *ZEROS        W1CFIL
052900000000E   2C                   ENDIF
053000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
053100000000E   1C                   ENDIF
053200000000     C*
053300000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
053400000000     C                   MOVEL     W1CFIL        ORGFIL
053500000000     C     KEYORG        CHAIN     AZORG                              99
053600000000IF  1C     *IN99         IFEQ      *OFF
053700000000     C     ORGFVA        ANDEQ     *BLANKS
053800000000     C     ORGFAG        ANDNE     'V'
053900000000     C                   MOVEL     ORGDES        W1DFIL
054000000000X   1C                   ELSE
054100000000     C                   MOVEL     '1'           $ERR
054200000000E   1C                   ENDIF
054300000000     C*
054400000000     C                   ENDSR
054500000000     C*------------------------------------------------------------------------*
054600000000     C* CHKARE - CONTROLLA L'AREA
054700000000     C*------------------------------------------------------------------------*
054800000000     C     CHKARE        BEGSR
054900000000     C*
055000000000     C*
055100000000     C* CONTROLLO SE E' STATO INSERITO '?'
055200000000     C     '?'           SCAN      V1CARE                                 99
055300000000     C*
055400000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
055500000000IF  1C     *IN99         IFEQ      *ON
055600020916     C                   MOVEL     1             §KUT
055700000000     C                   MOVEL     '05'          §COD
055800000000     C                   MOVEL     *BLANKS       §KEY
055900000000     C                   MOVEL     *BLANKS       §DES
056000000000     C                   CALL      'X§TABER'
056100000000     C                   PARM                    §KUT              1 0
056200000000     C                   PARM                    §COD              2
056300000000     C                   PARM                    §KEY              8
056400000000     C                   PARM                    §DES             30
056500000000     C                   MOVEL     §KEY          W1CARE
056600000000     C                   MOVEL     §DES          W1DARE           25
056700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
056800000000E   1C                   ENDIF
056900000000     C*
057000000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
057100020916     C                   MOVEL     1             TBLKUT
057200000000     C                   MOVEL     '05'          TBLCOD
057300000000     C                   MOVEL     *BLANKS       TBLKEY
057400000000     C                   MOVEL     W1CARE        TBLKEY
057500000000     C     KEYTAB        CHAIN     TABEL                              99
057600000000IF  1C     *IN99         IFEQ      *ON
057700000000     C     TBLFLG        OREQ      '*'
057800000000     C                   MOVEL     '1'           $ERR
057900000000X   1C                   ELSE
058000000000     C                   MOVEL     TBLUNI        W1DARE
058100000000E   1C                   ENDIF
058200000000     C*
058300000000     C                   ENDSR
058400000000     C*------------------------------------------------------------------------*
058500000000     C* CHKDIV - CONTROLLA LA DIVISIONE
058600000000     C*------------------------------------------------------------------------*
058700000000     C     CHKDIV        BEGSR
058800000000     C*
058900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
059000000000IF  1C     V1CDIV        IFEQ      '?'
059100020916     C                   MOVEL     1             §KUT
059200000000     C                   MOVEL     '17'          §COD
059300000000     C                   MOVEL     *BLANKS       §KEY
059400000000     C                   MOVEL     *BLANKS       §DES
059500000000     C                   CALL      'X§TABER'
059600000000     C                   PARM                    §KUT
059700000000     C                   PARM                    §COD
059800000000     C                   PARM                    §KEY
059900000000     C                   PARM                    §DES
060000000000     C                   MOVEL     §KEY          W1CDIV
060100000000     C                   MOVEL     §DES          W1DDIV           25
060200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
060300000000E   1C                   ENDIF
060400000000     C*
060500000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
060600020916     C                   MOVEL     1             TBLKUT
060700000000     C                   MOVEL     '17'          TBLCOD
060800000000     C                   MOVEL     *BLANKS       TBLKEY
060900000000     C                   MOVEL     W1CDIV        TBLKEY
061000000000     C     KEYTAB        CHAIN     TABEL                              99
061100000000IF  1C     *IN99         IFEQ      *ON
061200000000     C     TBLFLG        OREQ      '*'
061300000000     C                   MOVEL     '1'           $ERR
061400000000X   1C                   ELSE
061500000000     C                   MOVEL     TBLUNI        W1DDIV
061600000000E   1C                   ENDIF
061700000000     C*
061800000000     C                   ENDSR
061900000000     C*------------------------------------------------------------------------*
062000000000     C* CHKSER - CONTROLLA IL TIPO SERVIZIO
062100000000     C*------------------------------------------------------------------------*
062200000000     C     CHKSER        BEGSR
062300000000     C*
062400000000     C     *LIKE         DEFINE    V1CSER        WSER
062500000000     C     *LIKE         DEFINE    V1DSER        WDSER
062600000000     C*
062700000000     C                   MOVEL     *BLANKS       WDSER
062800000000     C*
062900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
063000000000IF  1C     WSER          IFEQ      '?'
063100020916     C                   MOVEL     1             §KUT
063200000000     C                   MOVEL     '5E'          §COD
063300000000     C                   MOVEL     *BLANKS       §KEY
063400000000     C                   MOVEL     *BLANKS       §DES
063500000000     C                   CALL      'X§TABER'
063600000000     C                   PARM                    §KUT
063700000000     C                   PARM                    §COD
063800000000     C                   PARM                    §KEY
063900000000     C                   PARM                    §DES
064000000000     C                   MOVEL     §KEY          WSER
064100000000     C                   MOVEL     §DES          WDSER
064200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
064300000000E   1C                   ENDIF
064400000000     C*
064500000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
064600020916     C                   MOVEL     1             TBLKUT
064700000000     C                   MOVEL     '5E'          TBLCOD
064800000000     C                   MOVEL     *BLANKS       TBLKEY
064900000000     C                   MOVEL     WSER          TBLKEY
065000000000     C     KEYTAB        CHAIN     TABEL                              99
065100000000IF  1C     *IN99         IFEQ      *ON
065200000000     C     TBLFLG        OREQ      '*'
065300000000     C                   MOVEL     '1'           $ERR
065400000000X   1C                   ELSE
065500000000     C                   MOVEL     TBLUNI        WDSER
065600000000E   1C                   ENDIF
065700000000     C*
065800000000     C                   ENDSR
065900000000     C*------------------------------------------------------------------------*
066000000000     C* IMPVID  - IMPOSTAZIONI INIZIALI VIDEO
066100000000     C*------------------------------------------------------------------------*
066200000000     C     IMPVID        BEGSR
066300000614     C*
066400000000     C*--------------------
066500000000     C* D A T A  F I N E --> ANNO/MESE CORRENTE - 1 MESE
066600000000     C*--------------------
066700990527     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
066800990527     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
066900990527     C                   Z-ADD     N8            DDATA8                         *SCOMPOSIZIONE DATA
067000990527     C*
067100990527     C                   Z-ADD     DANNO8        ANNO              4 0
067200990527     C                   Z-ADD     DMESE8        MESE              2 0
067300000000     C                   SUB       1             MESE
067400000000IF  1C     MESE          IFEQ      *ZEROS                                       *SE GENNAIO
067500000000     C                   Z-ADD     12            MESE                           *IMPOSTO DICEMBRE
067600000000     C                   SUB       1             ANNO                           *DELL'ANNO PRECEDENTE
067700000000E   1C                   ENDIF
067800000000     C                   MOVEL     ANNO          V1CDA2
067900000000     C                   MOVE      MESE          V1CDA2
068000990527     C*
068100990527     C                   Z-ADD     V1CDA2        AMCOR             6 0          *ANN/MESE CORRENTE
068200000000     C*--------------------
068300000000     C* D A T A  I N I Z I O --> INIIZO DELL'ANNO
068400000000     C*--------------------
068500000000     C                   Z-ADD     V1CDA2        V1CDAT
068600000000     C                   MOVE      01            V1CDAT
068700000000     C*--------------------
068800000000     C* RAGIONE SOCIALE
068900000000     C*--------------------
069000020916     C                   MOVEL     RSUT          VTDRSU
069100000000     C*--------------------
069200000000     C* RICHIESTE
069300000000     C*--------------------
069400950526     C                   MOVEL     'P'           V1CMPA                         *TIPO MOVIMENTO
069500000000     C                   MOVEL     'N'           V1CDET                         *DETTAGLIO FIL/FIL
069600950526     C                   MOVEL     'I'           V1CIOE                         *DATI IN STATISTICA
069700000000     C*
069800000000     C                   ENDSR
069900020916     C*--------------------------------------------------------------------------------------------*
070000020916     C* REPERISCE I DATI UTENTE
070100020916     C*--------------------------------------------------------------------------------------------*
070200020916     C     REPDATIUTE    BEGSR
070300020916     C*
070400020916     C* INIZIALIZZA VARIABILI DI WRK
070500020916     C                   CLEAR                   TIBS34DS
070600020916     C                   CLEAR                   AZUTEDS
070700020916     C                   CLEAR                   DDATIUTE
070800020916     C*
070900020916     C     *DTAARA       DEFINE    §azute        azuteds
071000020916     C     *DTAARA       DEFINE    §datiute      ddatiute
071100020916     C                   IN(E)     *DTAARA
071200020916if  1C                   IF        %Error
071300020916     c                   EVAL      I34Tla = 'L'
071400020916     C                   CALL      'TIBS34R'
071500020916     C                   PARM                    Tibs34Ds
071600020916     C                   IN        *DTAARA
071700020916e   1C                   ENDIF
071800020916     C*
071900160302     c                   Clear                   wabi
072000160302     c                   Clear                   dLat
072100160302
072200160302      * Verifica errori e autorità profilo
072300160302s   1c                   Select
072400160302      * se ho errori nei dati utente esco dal pgm
072500160302w   1c                   When      DutErr = 'E'
072600160302     c                   seton                                        lr
072700160302     c                   return
072800160302      * se non c'è l'abilitazione
072900160302      * --> se 1° livello, abilitazioni al terminal
073000160302      *     se 2° livello, abilitazioni al punto operativo
073100160302w   1c                   When      UteAut = *Blanks
073200160302if  2c                   If        DutLpo = '1'
073300160302     c                   Eval      wabi   = 'TP'
073400160302e   2c                   EndIf
073500160302if  2c                   If        DutLpo = '2'
073600160302     c                   Eval      wabi   = 'PO'
073700160302e   2c                   EndIf
073800160302if  2c                   If        DutLpo = 'S'
073900160302     c                   Eval      wabi   = 'AZ'
074000160302e   2c                   EndIf
074100160302      * carica le abilitazioni del profilo
074200160302x   1c                   Other
074300160302     c                   Movel     UteFaf        Dute01
074400160421     c                   If        §Uteistmo <> *Blanks
074500160421     c                   Eval      wabi = §Uteistmo
074600160302     c                   Else
074700160422     c*M                 Eval      wabi = UteAut
074800160302     c                   EndIf
074900160302e   1c                   EndSl
075000160302      * controllo se ok l'abilitazione dell'utente
075100160302     c                   Clear                   Tibs02ds
075200160302     c                   Eval      T02Mod = 'C'
075300160302     c                   Eval      T02Sif = knsif
075400160302     c                   Eval      T02Cod = 'LAT'
075500160302     c                   Movel(p)  wabi          T02Ke1
075600160302     c                   Call      'TIBS02R'
075700160302     c                   Parm                    kpjba
075800160302     c                   Parm                    Tibs02ds
075900160302if  1c                   If        T02Err = *Blanks
076000160302     c                   Eval      dLat = T02Uni
076100160302e   1c                   EndIf
076200160302      * errore o non abilitato
076300160302if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
076400160422if  1c                             or §uteistMO = 'NO'
076500160302     c                   seton                                        98
076600160302     c                   call      'XUTENOAB'
076700160302     c                   parm                    kpjba
076800160302     c                   seton                                        lr
076900160302     c                   return
077000160302   x1c                   endif
077100160302      * Reperimento dei P.O. gestibili dall'utente
077200160302     c                   clear                   TRUL31DS
077300160302     c                   clear                   TRUL31DS2
077400160302     c                   eval      I31abi = wabi
077500160302     c                   eval      I31cdi = DUTdis
077600160302     c                   eval      I31car = DUTare
077700160302     c                   eval      I31cpo = DUTpou
077800160302     c                   call      'TRUL31R'
077900160302     c                   parm                    KPJBA
078000160302     c                   parm                    TRUL31DS
078100160302     c                   parm                    TRUL31DS2
078200160302if  2c                   if        O31pog > *zeros
078300160302     c                   movea     O31pog        $pog
078400160302     c                   movea     O31arg        $arg
078500160302     c                   movea     O31dig        $dig
078600160302x   2c                   else
078700160302     c* impossibile perchè controllato nel filtrp
078800160302     c                   seton                                        lr
078900160302     c                   return
079000160302    1c                   endif
079100020916     C                   ENDSR
079200000000     C*------------------------------------------------------------------------*
079300000000     C* *INZSR  - OPERAZIONI INIZIALI
079400000000     C*------------------------------------------------------------------------*
079500000000     C     *INZSR        BEGSR
079600950526     C*--------------------
079700000000     C* RICEVIMENTO PARAMETRI
079800950526     C*--------------------
079900000000     C     *ENTRY        PLIST
080000000000     C                   PARM                    KPJBA
080100950526     C*--------------------
080200950526     C* CHIAVI DI LETTURA
080300950526     C*--------------------
080400000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
080500000000     C     KEYORG        KLIST
080600000000     C                   KFLD                    ORGFIL
080700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
080800000000     C     KEYTAB        KLIST
080900000000     C                   KFLD                    TBLKUT
081000000000     C                   KFLD                    TBLCOD
081100000000     C                   KFLD                    TBLKEY
081200000000     C*
081300000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
081400000000     C                   EXSR      INZSTD
081500000000     C*
081600000000     C* IMPOSTAZIONI INIZIALI VIDEO
081700000000     C                   EXSR      IMPVID
081800000000     C*
081900950526     C* EMETTE FORMATO VIDEO
082000950526     C                   EXSR      CARV1
082100000000     C*
082200000000     C                   ENDSR
082300000000     C*------------------------------------------------------------------------*
082400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082600000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
082700000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
082800000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
082900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083100000000     C*------------------------------------------------------------------------*
083200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
083300000000     C*------------------------------------------------------------------------*
083400000000     C     INZSTD        BEGSR
083500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
083600000000     C                   CALL      'XKEYS'
083700000000     C                   PARM                    DSKEYS
083800000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
083900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
084000000000     C     XMSGPA        PLIST
084100000000     C                   PARM                    DSMSMS
084200000000     C                   PARM                    DSMSPG
084300000000     C                   PARM                    DSMSKE
084400000000     C                   PARM                    DSMSER
084500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
084600020115     C                   MOVEL     'TISE45R'     DSMSPG                         *NOME PGM CHIAMANTE
084700020115     C                   MOVEL     'TISE45R'     VMCQUE                         *CODA MESSAGGI DESTIN
084800000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
084900000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
085000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
085100000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
085200000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
085300000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
085400000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
085500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
085600000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
085700000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
085800000000     C* LANCIO ROUTINE PER VERIFICARE SE
085900000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
086000000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
086100020916     C                   EXSR      REPDATIUTE
086200000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
086300000000     C                   BITOFF    '01234567'    POSMI1            1
086400000000     C                   BITOFF    '01234567'    POSMI2            1
086500000000     C                   BITOFF    '01234567'    POSMA1            1
086600000000     C                   BITOFF    '01234567'    POSMA2            1
086700000000     C                   BITON     '34'          POSMI1            1            *18
086800000000     C                   BITON     '7'           POSMI2            1            *01
086900000000     C                   BITON     '34'          POSMA1            1            *18
087000000000     C                   BITON     '0'           POSMA2            1            *80
087100000000     C                   MOVE      POSMI2        POSMIN            2
087200000000     C                   MOVEL     POSMI1        POSMIN
087300000000     C                   MOVE      POSMA2        POSMAX            2
087400000000     C                   MOVEL     POSMA1        POSMAX
087500000000     C                   ENDSR
087600000000     C*------------------------------------------------------------------------*
087700000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
087800000000     C*------------------------------------------------------------------------*
087900000000     C     FUNSTD        BEGSR
088000000000     C                   SELECT
088100000000     C     KEY           WHENEQ    F3
088200000000     C                   EXSR      F03STD
088300000000     C     KEY           WHENEQ    F12
088400000000     C                   EXSR      F12STD
088500000000     C     KEY           WHENEQ    F24
088600000000     C                   EXSR      F24STD
088700000000     C                   OTHER
088800000000     C                   MOVEL     MSG(2)        $MSG
088900000000     C                   EXSR      CARMSG
089000000000     C                   ENDSL
089100000000     C                   ENDSR
089200000000     C*------------------------------------------------------------------------*
089300000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
089400000000     C*------------------------------------------------------------------------*
089500000000     C     F03STD        BEGSR
089600000000     C*
089700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
089800000000     C                   MOVEL     '1'           $FINE
089900000000     C*
090000000000     C                   ENDSR
090100000000     C*------------------------------------------------------------------------*
090200000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
090300000000     C*------------------------------------------------------------------------*
090400000000     C     F12STD        BEGSR
090500000000     C*
090600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
090700000000     C                   MOVEL     '1'           $FINE
090800000000     C*
090900000000     C                   ENDSR
091000000000     C*------------------------------------------------------------------------*
091100000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
091200000000     C*------------------------------------------------------------------------*
091300000000     C     F24STD        BEGSR
091400000000     C*
091500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
091600000000     C                   EXSR      OUTFUN
091700000000     C*
091800000000     C                   ENDSR
091900000000     C*------------------------------------------------------------------------*
092000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
092100000000     C*          CODA MESSAGGI DEL PGM
092200000000     C*------------------------------------------------------------------------*
092300000000     C     CARMSG        BEGSR
092400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
092500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
092600000000     C     MSGULT        IFEQ      0
092700000000     C                   MOVEL     '1'           *IN40
092800020115     C                   WRITE     SE45DVK
092900000000     C                   MOVEL     '0'           *IN40
093000000000     C                   ENDIF
093100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
093200000000     C                   MOVEL     $MSG          DSMSMS
093300000000     C                   CALL      'XMSG'        XMSGPA
093400000000     C     DSMSER        IFEQ      0
093500000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
093600000000     C                   ENDIF
093700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
093800000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
093900020115     C                   WRITE     SE45DVM                                      *SFL MESS
094000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
094100000000     C                   MOVEL     *BLANKS       $MSG
094200000000     C                   ENDSR
094300000000     C*------------------------------------------------------------------------*
094400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
094500000000     C*------------------------------------------------------------------------*
094600000000     C     OUTMSG        BEGSR
094700000000     C* VISUALIZZAZIONE MESSAGGIO
094800000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
094900020115     C                   WRITE     SE45DVK                                      *CONTR MESS
095000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
095100000000     C                   Z-ADD     *ZEROS        MSGULT
095200000000     C                   ENDSR
095300000000     C*------------------------------------------------------------------------*
095400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
095500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
095600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
095700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
095800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
095900000000     C*------------------------------------------------------------------------*
096000000000     C     CARFUN        BEGSR
096100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
096200000000     C     1             DO        31            I                 5 0          --- 1 -->
096300000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
096400000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
096500000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
096600020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
096700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
096800000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
096900000000     C                   ELSE                                                   *INTERROG.
097000000000     C                   MOVEL     FUNASI        FAT(I)
097100000000     C                   ENDIF
097200000000     C                   ELSE                                                   <-- 2 --> *FILIALE
097300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
097400000000     C                   MOVEL     FUNAFG        FAT(I)
097500000000     C                   ELSE                                                   *INTERROG.
097600000000     C                   MOVEL     FUNAFI        FAT(I)
097700000000     C                   ENDIF
097800000000     C                   ENDIF                                                  <-- 2 ---
097900000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
098000000000     C                   MOVEL     USER          WEDP              3            *PROFILO
098100000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
098200000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
098300000000     C                   ELSE
098400000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
098500000000     C                   ENDIF
098600000000     C                   ENDIF
098700000000     C                   ENDDO                                                  <-- 1 ---
098800000000     C                   ENDSR
098900000000     C*------------------------------------------------------------------------*
099000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
099100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
099200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
099300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
099400000000     C*------------------------------------------------------------------------*
099500000000     C     OUTFUN        BEGSR
099600000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
099700000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
099800000000     C                   ENDDO
099900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
100000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
100100000000     C     1             DO        31            I
100200000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
100300000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
100400000000     C                   ADD       1             FUNNAT
100500000000     C                   ENDIF
100600000000     C                   ENDDO
100700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
100800000000     C     FUNNAT        IFGT      8
100900000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
101000000000     C                   ENDIF
101100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
101200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
101300000000     C                   Z-ADD     8             FUNVIS            5 0
101400000000     C     FAT(23)       IFEQ      'S'
101500000000     C                   SUB       1             FUNVIS
101600000000     C                   ENDIF
101700000000     C     FAT(24)       IFEQ      'S'
101800000000     C                   SUB       1             FUNVIS
101900000000     C                   ENDIF
102000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
102100000000     C                   Z-ADD     *ZEROS        W                 5 0
102200000000     C                   ADD       1             FUNULT
102300000000     C     FUNULT        IFGT      31
102400000000     C                   Z-ADD     1             FUNULT
102500000000     C                   ENDIF
102600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
102700000000     C     FUNULT        DO        31            I                              --- 1 -->
102800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
102900000000     C     W             IFLT      FUNVIS                                       --- 2 -->
103000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
103100000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
103200000000     C     FDE(I)        ANDNE     *BLANKS
103300000000     C     I             IFEQ      23                                           *TOLGO F23
103400000000     C     I             OREQ      24                                           *E F24
103500000000     C                   ELSE
103600000000     C                   ADD       1             W
103700000000     C                   MOVEL     FDE(I)        FUV(W)
103800000000     C                   ENDIF
103900000000     C                   ENDIF                                                  <-- 3 ---
104000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
104100000000     C                   Z-ADD     I             FUNULT
104200000000     C                   ENDIF                                                  <-- 2 ---
104300000000     C                   ENDDO                                                  <-- 1 ---
104400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
104500000000     C                   MOVEL     FUV(1)        VFD001
104600000000     C                   MOVEL     FUV(2)        VFD002
104700000000     C                   MOVEL     FUV(3)        VFD003
104800000000     C                   MOVEL     FUV(4)        VFD004
104900000000     C                   MOVEL     FUV(5)        VFD005
105000000000     C                   MOVEL     FUV(6)        VFD006
105100000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
105200000000     C     FAT(24)       ANDEQ     'N'
105300000000     C                   MOVEL     FUV(7)        VFD007
105400000000     C                   MOVEL     FUV(8)        VFD008
105500000000     C                   ELSE                                                   <-- 1 -->
105600000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
105700000000     C     FAT(24)       ANDEQ     'S'
105800000000     C                   MOVEL     FDE(23)       VFD007
105900000000     C                   MOVEL     FDE(24)       VFD008
106000000000     C                   ELSE                                                   <-- 2 -->
106100000000     C                   MOVEL     FUV(7)        VFD007
106200000000     C     FAT(23)       IFEQ      'S'
106300000000     C                   MOVEL     FDE(23)       VFD008
106400000000     C                   ELSE
106500000000     C                   MOVEL     FDE(24)       VFD008
106600000000     C                   ENDIF
106700000000     C                   ENDIF                                                  <-- 2 ---
106800000000     C                   ENDIF                                                  <-- 1 ---
106900000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
107000000000     C                   ENDSR
107100000000     C*------------------------------------------------------------------------*
107200000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
107300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
107400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
107500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
107600000000     C*------------------------------------------------------------------------*
107700000000     C     CONFUN        BEGSR
107800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
107900000000     C     1             DO        31            I                              --- 1 -->
108000000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
108100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
108200000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
108300000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
108400000000     C                   EXSR      CARMSG
108500000000     C                   MOVEL     '1'           $ERR
108600000000     C                   ENDIF                                                  <-- 3 ---
108700000000     C                   ENDIF                                                  <-- 2 ---
108800000000     C                   ENDDO                                                  <-- 1 ---
108900000000     C                   ENDSR
109000000000     O*------------------------------------------------------------------------*
109100000000**   MSG - MESSAGGI
109200020115TISE45R- Tasto funzionale non valido.                                          1
109300020115TISE45R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
109400020115TISE45R- Opzione non valida.                                                   3
109500020115TISE45R- Opzione abilitata ma non gestita. Avvertire CED.                      4
109600020115TISE45R- Inizio elenco.                                                        5
109700020115TISE45R- Fine elenco.                                                          6
109800020115TISE45R- Anno/Mese obbligatorio                                                7
109900020115TISE45R- Anno/Mese errato                                                      8
110000020115TISE45R- Data superiore alla data creazione saldi                              9
110100020115TISE45R- 'P' per elaborare i movimenti in partenza,'A' per quelli in arrivo   10
110200020115TISE45R- Anno/Mese finale inferiore all'Anno/Mese iniziale                    11
110300160426TISE45R- Filiale errata o non in gestione                                     12
110400160422TISE45R- Area errata o non in gestione                                        13
110500160422TISE45R- Divisione errata o non in gestione                                   14
110600020115TISE45R- Tipo servizio non accettabile                                        15
110700020115TISE45R- Tipo porto non accettabile                                           16
110800020115TISE45R- Tipo bolla non accettabile                                           17
110900020115TISE45R- Tipo prodotto non accettabile                                        18
111000160426TISE45R- Immettere solo una selezione per filiale, area o divisione           19
111100020115TISE45R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
111200020115TISE45R- Impossibile stampare più di 12 mesi                                  21
111300160427TISE45R- 'S' per stampare il dettaglio Filiale  o 'N' per non stamparlo       22
111400020115TISE45R- 'S' per creare file su PC, 'N' per non crearlo                       23
111500020115TISE45R- Immettere uno dei valori possibili                                   24
111600020325TISE45R- Impossibile lanciare la statistica su anni differenti                25
111700160426TISE45R- Selezionare almeno una fra Filiale Area o Distretto se abilitati     26
111800020115TISE45R- 27
111900020115TISE45R- 28
112000020115TISE45R- Immettere il tipo di dati che si vuole in statistica                 29
112100020115TISE45R- 30
112200000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
112300000000F1    -                   -    NN     NN
112400000000F2    -                   -    NN     NN
112500000000F3    -F3=Fine            -    SS     SS
112600000000F4    -                   -    NN     NN
112700000000F5    -                   -    NN     NN
112800000000F6    -F6=Lancio          -    SS     SS
112900000000F7    -                   -    NN     NN
113000000000F8    -                   -    NN     NN
113100000000F9    -                   -    NN     NN
113200000000F10   -                   -    NN     NN
113300000000F11   -                   -    NN     NN
113400000000F12   -F12-Ritorno        -    SS     SS
113500000000F13   -                   -    NN     NN
113600000000F14   -                   -    NN     NN
113700000000F15   -                   -    NN     NN
113800000000F16   -                   -    NN     NN
113900000000F17   -                   -    NN     NN
114000000000F18   -                   -    NN     NN
114100000000F19   -                   -    NN     NN
114200000000F20   -                   -    NN     NN
114300000000F21   -                   -    NN     NN
114400000000F22   -                   -    NN     NN
114500000000F23   -                   -    NN     NN
114600000000F24   -F24=Altri tasti    -    NN     NN
114700000000ENTER -                   -    SS     SS
114800000000ROLLUP-                   -    NN     NN
114900000000ROLLDN-                   -    NN     NN
115000000000HELP  -                   -    NN     NN
115100000000PRINT -                   -    NN     NN
115200000000BSPACE-                   -    NN     NN
115300000000CLEAR -                   -    NN     NN
