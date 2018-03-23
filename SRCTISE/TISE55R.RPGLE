000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA RICAVI - RICHIESTA PARAMETRI                                *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800010511     FTISE55D   CF   E             WORKSTN INFDS(DEVDS1)
000900010511     F                                     SFILE(SE55DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FTABEL00F  IF   E           K DISK
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* SCHIERE GESTIONE VIDEO
001700000000     D*-------------------
001800950525     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
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
006600990531     D*-------------------
006700990531     D* PARAMETRI DI LANCIO
006800990531     D*-------------------
006900990531     D PARAM           DS                  INZ
007000990531     D  PARDAT                 1      6  0
007100990531     D  PARDA2                 7     12  0
007200990531     D  PARDET                13     13
007300990531     D  PARFIL                14     16  0
007400990531     D  PARARE                17     19  0
007500990531     D  PARDIV                20     20
007600990531     D  PARSER                21     21
007700990531     D  PARPRO                22     22
007800990531     D  PARRNC                23     23
007900990531     D  PARFPC                24     24
008000990531     D  PARIOE                25     25
008100000704     D  PARPOS                26     26
008200000000     D*-------------------
008300020916     D* REPERIMNETO DATI UTENTE
008400000000     D*-------------------
008500020916     D TIBS34DS      E DS                                                       *Profili utente
008600020916     D DDATIUTE      E DS                                                       *Dati utente
008700020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008800000000     D*-------------------
008900010410     D* DS DATI VARI PER STATISTICHE
009000000000     D*-------------------
009100010410     D DS5S          E DS
009200990531     D*------------------
009300990531     D* DS CONTROLLO DATA (8)
009400990531     D*------------------
009500990531     D WLBDA8          DS                  INZ
009600990531     D  G08DAT                 1      8  0
009700990531     D  G08INV                 9     16  0
009800990531     D  G08ERR                17     17
009900990531     D  G08TGI                18     22  0
010000981117     D*------------------
010100981117     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
010200981117     D*------------------
010300981117     D                 DS
010400990531     D  AGGAA                  1      4  0
010500981117     D  AGGMM                  5      6  0
010600990531     D  AGGAM                  1      6  0
010700981117     D  AGGGG                  7      8  0
010800981117     D  WDAE                   1      8  0
010900990531     D*------------------
011000990531     D* DS DATA GENERICA(8) IN GG/MM/AAAA
011100990531     D*------------------
011200990531     D                 DS
011300990531     D  DGIOR8                 1      2  0
011400990531     D  DMESE8                 3      4  0
011500990531     D  DANNO8                 5      8  0
011600990531     D  DDATA8                 1      8  0
011700000000     C*------------------------------------------------------------------------*
011800000000     C* MAIN LINE
011900000000     C*------------------------------------------------------------------------*
012000000000     C*
012100000000     C* CICLO FINO A CHE:
012200000000DO  1C     $FINE         DOUEQ     '1'
012300000000     C*
012400000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
012500000000     C*
012600000000     C* CONTROLLA GESTIONE VIDEO
012700000000IF  2C     $GES          IFEQ      'V1'
012800000000     C                   EXSR      GESV1                                        *PARAMETRI
012900000000E   2C                   ENDIF
013000000000     C*
013100000000E   1C                   ENDDO
013200000000     C*
013300000000     C                   SETON                                        LR
013400000000     C*------------------------------------------------------------------------*
013500000000     C* GESV1 - GESTIONE PRIMO VIDEO
013600000000     C*------------------------------------------------------------------------*
013700000000     C     GESV1         BEGSR
013800000000     C*
013900000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTER
014000000000     C                   EXSR      CARV1                                        *CARICA VIDEO
014100000000E   1C                   ENDIF
014200000000     C*
014300000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGGI
014400000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
014500000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
014600000000E   1C                   ENDIF
014700000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
014800000000     C*
014900010511     C                   EXFMT     SE55DV1                                      *PARAMETRI
015000000000     C*
015100000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUNZ
015200000000     C*
015300000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
015400000000SL  2C                   SELECT
015500000000     C     KEY           WHENEQ    ENTER
015600000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
015700000000     C     KEY           WHENEQ    F6
015800000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
015900000000     C                   OTHER
016000000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
016100000000E   2C                   ENDSL
016200000000E   1C                   ENDIF
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*------------------------------------------------------------------------*
016600000000     C* CARV1 - CARICA PRIMO VIDEO
016700000000     C*------------------------------------------------------------------------*
016800000000     C     CARV1         BEGSR
016900000000     C*
017000000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
017100000000     C*
017200000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
017300000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
017400000000     C*
017500010511     C                   WRITE     SE55DVT                                      *TESTATA
017600010511     C                   WRITE     SE55DVF                                      *FUNZIONI
017700000000     C*
017800000000     C                   ENDSR
017900000000     C*------------------------------------------------------------------------*
018000000000     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
018100000000     C*------------------------------------------------------------------------*
018200000000     C     ENTV1         BEGSR
018300000000     C*
018400000000     C* CONTROLLA I CAMPI IMMESSI
018500000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
018600000000     C*
018700000000     C                   ENDSR
018800000000     C*------------------------------------------------------------------------*
018900000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
019000000000     C*------------------------------------------------------------------------*
019100000000     C     F06GES        BEGSR
019200000000     C*
019300000000     C* CONTROLLA I CAMPI IMMESSI
019400000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
019500000000     C*
019600000000     C* NON CI SONO ERRORI
019700000000IF  1C     $ERR          IFEQ      '0'
019800000000     C*
019900000000     C* LANCIO PROGRAMMA GUIDA
020000000000     C                   Z-ADD     V1CDAT        PARDAT
020100000000     C                   Z-ADD     V1CDA2        PARDA2
020200000000     C                   MOVEL     V1CSER        PARSER
020300000000     C                   MOVEL     V1CPRO        PARPRO
020400000000     C                   MOVEL     V1CDET        PARDET
020500000000     C                   MOVEL     V1CRNC        PARRNC
020600000000     C                   MOVEL     V1CFPC        PARFPC
020700950525     C                   MOVEL     V1CIOE        PARIOE
020800000704     C                   MOVEL     V1CPOS        PARPOS
020900000000     C*
021000010511     C                   MOVEL     'SE56'        KCOAZ
021100000000     C                   MOVEL     PARAM         KPJBU
021200020118     C                   CALL      'BCH10'
021300020325 MOM C*                  CALL      'TISE56C'
021400000000     C                   PARM                    KPJBA
021500000000     C*
021600000000     C*---                 MOVEL'1'       $FINE            *USCITA DAL PGM
021700000000E   1C                   ENDIF
021800000000     C*
021900000000     C                   ENDSR
022000000000     C*------------------------------------------------------------------------*
022100000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
022200000000     C*------------------------------------------------------------------------*
022300000000     C     DECODI        BEGSR
022400000000     C*
022500000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
022600000000     C                   SETOFF                                       606263
022700000000     C                   SETOFF                                       646566
022800000000     C                   SETOFF                                       676871
022900000704     C                   SETOFF                                       727374
023000000000     C*
023100000000     C* REIMPOSTO VARIABILI DI LAVORO
023200000000     C                   Z-ADD     *ZEROS        SELEZ             1 0
023300000000     C                   Z-ADD     *ZEROS        RISUL             4 0
023400000000     C*--------------------
023500000000     C* ANNO/MESE INIZIALE
023600000000     C*--------------------
023700000000IF  1C     V1CDAT        IFEQ      *ZEROS
023800000000     C                   MOVEL     '1'           $ERR
023900000000     C                   MOVEL     MSG(7)        $MSG
024000000000     C                   EXSR      CARMSG
024100000000     C                   SETON                                        60
024200000000E   1C                   ENDIF
024300000000IF  1C     $ERR          IFEQ      '0'
024400990531     C                   MOVEL     V1CDAT        G08INV
024500990531     C                   MOVE      01            G08INV
024600990531     C                   Z-ADD     *ZEROS        G08DAT
024700990531     C                   MOVEL     '3'           G08ERR
024800990531     C                   CALL      'XSRDA8'
024900990531     C                   PARM                    WLBDA8
025000990531IF  2C     G08ERR        IFEQ      '1'
025100000000     C                   MOVEL     '1'           $ERR
025200000000     C                   MOVEL     MSG(8)        $MSG
025300000000     C                   EXSR      CARMSG
025400000000     C                   SETON                                        60
025500000000E   2C                   ENDIF
025600000000E   1C                   ENDIF
025700000000     C*--------------------
025800000000     C* ANNO/MESE FINALE
025900000000     C*--------------------
026000000000IF  1C     V1CDA2        IFEQ      *ZEROS
026100000000     C                   MOVEL     '1'           $ERR
026200000000     C                   MOVEL     MSG(7)        $MSG
026300000000     C                   EXSR      CARMSG
026400000000     C                   SETON                                        62
026500000000E   1C                   ENDIF
026600000000IF  1C     $ERR          IFEQ      '0'
026700990531     C                   MOVEL     V1CDA2        G08INV
026800990531     C                   MOVE      01            G08INV
026900990531     C                   Z-ADD     *ZEROS        G08DAT
027000990531     C                   MOVEL     '3'           G08ERR
027100990531     C                   CALL      'XSRDA8'
027200990531     C                   PARM                    WLBDA8
027300990531IF  2C     G08ERR        IFEQ      '1'
027400000000     C                   MOVEL     '1'           $ERR
027500000000     C                   MOVEL     MSG(8)        $MSG
027600000000     C                   EXSR      CARMSG
027700000000     C                   SETON                                        62
027800000000E   2C                   ENDIF
027900000000E   1C                   ENDIF
028000990531     C*
028100000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
028200000000IF  1C     $ERR          IFEQ      '0'
028300000000IF  2C     V1CDA2        IFLT      V1CDAT
028400000000     C                   MOVEL     '1'           $ERR
028500000000     C                   MOVEL     MSG(11)       $MSG
028600000000     C                   EXSR      CARMSG
028700000000     C                   SETON                                        62
028800000000E   2C                   ENDIF
028900000000E   1C                   ENDIF
029000000000     C*
029100000000     C* RICHIEDERE NON PIU' DI DODICI MESI
029200000000IF  1C     $ERR          IFEQ      '0'
029300000000     C     V1CDA2        SUB       V1CDAT        RISUL
029400000000IF  2C     RISUL         IFGE      100
029500000000     C                   MOVEL     '1'           $ERR
029600000000     C                   MOVEL     MSG(19)       $MSG
029700000000     C                   EXSR      CARMSG
029800000000     C                   SETON                                        62
029900000000E   2C                   ENDIF
030000000000E   1C                   ENDIF
030100020325     C*
030200020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'ANNO)
030300020325IF  1C     $ERR          IFEQ      '0'
030400020325     C                   MOVEL     V1CDAT        ANNODA            4 0
030500020325     C                   MOVEL     V1CDA2        ANNOA             4 0
030600020325IF  2C     ANNODA        IFNE      ANNOA
030700020325     C                   MOVEL     '1'           $ERR
030800020325     C                   MOVEL     MSG(25)       $MSG
030900020325     C                   EXSR      CARMSG
031000020325     C                   SETON                                        6062
031100020325E   2C                   ENDIF
031200020325E   1C                   ENDIF
031300000000     C*--------------------
031400000000     C* FLAG RICHIESTE
031500000000     C*--------------------
031600000000     C* CONTROLLO FLAG DETTAGLIO FIL/FIL
031700000000IF  1C     $ERR          IFEQ      '0'
031800000000IF  2C     V1CDET        IFEQ      'S'
031900000000     C     V1CDET        OREQ      'N'
032000000000X   2C                   ELSE
032100000000     C                   MOVEL     '1'           $ERR
032200000000     C                   MOVEL     MSG(20)       $MSG
032300000000     C                   EXSR      CARMSG
032400000000     C                   SETON                                        72
032500000000E   2C                   ENDIF
032600000000E   1C                   ENDIF
032700000000     C*--------------------
032800000000     C* AGGIORNAMENTO RETTIFICHE/NOTE DI CREDITO
032900000000     C*--------------------
033000000000IF  1C     $ERR          IFEQ      '0'
033100000000IF  2C     V1CRNC        IFEQ      'S'
033200000000     C     V1CRNC        OREQ      'N'
033300000000X   2C                   ELSE
033400000000     C                   MOVEL     '1'           $ERR
033500000000     C                   MOVEL     MSG(10)       $MSG
033600000000     C                   EXSR      CARMSG
033700000000     C                   SETON                                        63
033800000000E   2C                   ENDIF
033900000000E   1C                   ENDIF
034000000000     C*
034100000000IF  1C     $ERR          IFEQ      '0'
034200950531IF  2C     V1CRNC        IFEQ      'N'                                          *ATT. SALDI DA SPALM
034300981117     C     WFSE          ANDEQ     'N'
034400950531     C*---                 MOVEL'1'       $ERR
034500950531     C*---                 SETON                     63
034600000000     C                   MOVEL     MSG(22)       $MSG
034700000000     C                   EXSR      CARMSG
034800000000E   2C                   ENDIF
034900000000E   1C                   ENDIF
035000000000     C*--------------------
035100000000     C* CONTROLLO FLAG CREAZIONE FILE SU PC
035200000000     C*--------------------
035300000000IF  1C     $ERR          IFEQ      '0'
035400000000IF  2C     V1CFPC        IFEQ      'S'
035500000000     C     V1CFPC        OREQ      'N'
035600000000X   2C                   ELSE
035700000000     C                   MOVEL     '1'           $ERR
035800000000     C                   MOVEL     *BLANKS       $MSG
035900000000     C                   EXSR      CARMSG
036000000000     C                   MOVEL     MSG(21)       $MSG
036100000000     C                   EXSR      CARMSG
036200000000     C                   SETON                                        64
036300000000E   2C                   ENDIF
036400000000E   1C                   ENDIF
036500000000     C*--------------------
036600000000     C* CONTROLLO LA FILIALE
036700000000     C*--------------------
036800000000IF  1C     $ERR          IFEQ      '0'
036900000000     C                   MOVEL     *BLANKS       V1DFIL
037000000000IF  2C     V1CFIL        IFNE      *ZEROS
037100000000     C     V1CFIL        ANDNE     *BLANKS
037200000000     C                   MOVEL     V1CFIL        W1CFIL            3
037300000000     C                   EXSR      CHKFIL                                       *CONTROLLA FILIALE
037400000000IF  3C     $ERR          IFEQ      '1'
037500000000     C                   MOVEL     MSG(12)       $MSG
037600000000     C                   EXSR      CARMSG
037700000000     C                   SETON                                        65
037800000000X   3C                   ELSE
037900000000     C                   MOVEL     W1CFIL        V1CFIL
038000000000     C                   MOVEL     W1CFIL        PARFIL                         *PARAMETRO
038100000000     C                   MOVEL     W1DFIL        V1DFIL                         *DESCRIZIONE FILIALE
038200000000E   3C                   ENDIF
038300000000     C*
038400000000     C* FILIALE NON SELEZIONATA: PASSO PARAMETRO A ZERO
038500000000X   2C                   ELSE
038600000000     C                   Z-ADD     *ZEROS        PARFIL
038700000000E   2C                   ENDIF
038800000000E   1C                   ENDIF
038900000000     C*--------------------
039000000000     C* CONTROLLO L'AREA
039100000000     C*--------------------
039200000000IF  1C     $ERR          IFEQ      '0'
039300000000     C                   MOVEL     *BLANKS       V1DARE
039400000000IF  2C     V1CARE        IFNE      *ZEROS
039500000000     C     V1CARE        ANDNE     *BLANKS
039600000000     C                   MOVEL     V1CARE        W1CARE            3
039700000000     C                   EXSR      CHKARE                                       *CONTROLLA AREA
039800000000IF  3C     $ERR          IFEQ      '1'
039900000000     C                   MOVEL     MSG(13)       $MSG
040000000000     C                   EXSR      CARMSG
040100000000     C                   SETON                                        66
040200000000X   3C                   ELSE
040300000000     C                   MOVEL     W1CARE        V1CARE
040400000000     C                   MOVEL     W1CARE        PARARE                         *PARAMETRO
040500000000     C                   MOVEL     W1DARE        V1DARE                         *DESCRIZIONE FILIALE
040600000000E   3C                   ENDIF
040700000000     C*
040800000000     C* AREA NON SELEZIONATA: PASSO PARAMETRO A ZERO
040900000000X   2C                   ELSE
041000000000     C                   Z-ADD     *ZEROS        PARARE
041100000000E   2C                   ENDIF
041200000000E   1C                   ENDIF
041300000000     C*--------------------
041400000000     C* CONTROLLO LA DIVISIONE
041500000000     C*--------------------
041600000000IF  1C     $ERR          IFEQ      '0'
041700000000     C                   MOVEL     *BLANKS       V1DDIV
041800000000IF  2C     V1CDIV        IFNE      *ZEROS
041900000000     C     V1CDIV        ANDNE     *BLANKS
042000000000     C                   MOVEL     V1CDIV        W1CDIV            1
042100000000     C                   EXSR      CHKDIV                                       *CONTROLLA DIVISIONE
042200000000IF  3C     $ERR          IFEQ      '1'
042300000000     C                   MOVEL     MSG(14)       $MSG
042400000000     C                   EXSR      CARMSG
042500000000     C                   SETON                                        67
042600000000X   3C                   ELSE
042700000000     C                   MOVEL     W1CDIV        V1CDIV
042800000000     C                   MOVEL     W1CDIV        PARDIV                         *PARAMETRO
042900000000     C                   MOVEL     W1DDIV        V1DDIV                         *DESCRIZIONE FILIALE
043000000000E   3C                   ENDIF
043100000000E   2C                   ENDIF
043200000000E   1C                   ENDIF
043300000000     C*--------------------
043400000000     C* CONTROLLO CHE LA SELEZIONE FRA FILIALE, AREA E DIVISIONE SIA UNA SOLA
043500000000     C*--------------------
043600000000IF  1C     $ERR          IFEQ      '0'
043700000000IF  2C     V1CFIL        IFGT      *ZEROS
043800000000     C                   ADD       1             SELEZ
043900000000E   2C                   ENDIF
044000000000IF  2C     V1CARE        IFGT      *ZEROS
044100000000     C                   ADD       1             SELEZ
044200000000E   2C                   ENDIF
044300000000IF  2C     V1CDIV        IFGT      *ZEROS
044400000000     C                   ADD       1             SELEZ
044500000000E   2C                   ENDIF
044600000000IF  2C     SELEZ         IFGT      1
044700000000     C                   MOVEL     '1'           $ERR
044800000000     C                   MOVEL     MSG(17)       $MSG
044900000000     C                   EXSR      CARMSG
045000000000     C                   SETON                                        65
045100000000E   2C                   ENDIF
045200000000E   1C                   ENDIF
045300000000     C*--------------------
045400000000     C* CONTROLLO IL TIPO SERVIZIO
045500000000     C*--------------------
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
046900950525     C*--------------------
047000960105     C* STATISTICA ITALIA / IMPORT/ EXPORT
047100950525     C*--------------------
047200950525IF  1C     $ERR          IFEQ      '0'
047300960105IF  2C     V1CIOE        IFEQ      'I'                                          *ITALIA
047400960105     C     V1CIOE        OREQ      'M'                                          *IMPORT
047500960105     C     V1CIOE        OREQ      'X'                                          *EXPORT
047600950525     C     V1CIOE        OREQ      *BLANKS
047700950525X   2C                   ELSE
047800950525     C                   MOVEL     '1'           $ERR
047900950525     C                   MOVEL     MSG(29)       $MSG
048000950525     C                   EXSR      CARMSG
048100950525     C                   SETON                                        73
048200950525E   2C                   ENDIF
048300950525E   1C                   ENDIF
048400000704     C*--------------------
048500000704     C* DATI POSTE
048600000704     C*--------------------
048700000704IF  1C     $ERR          IFEQ      '0'
048800000704IF  2C     V1CPOS        IFNE      'S'
048900000704     C     V1CPOS        ANDNE     'N'
049000000704     C                   MOVEL     '1'           $ERR
049100000704     C                   MOVEL     MSG(24)       $MSG
049200000704     C                   EXSR      CARMSG
049300000704     C                   SETON                                        74
049400000704E   2C                   ENDIF
049500000704E   1C                   ENDIF
049600000000     C*--------------------
049700000000     C* AGGIORNAMENTO RETTIFICHE/NOTE DI CREDITO
049800000000     C*--------------------
049900000000IF  1C     $ERR          IFEQ      '0'
050000000000IF  2C     V1CRNC        IFEQ      'S'                                          *ATT. SALDI SPALMATI
050100981117     C     WFSE          ANDEQ     'S'
050200000000     C                   MOVEL     MSG(23)       $MSG
050300000000     C                   EXSR      CARMSG
050400000000E   2C                   ENDIF
050500000000E   1C                   ENDIF
050600000000     C*--------------------
050700000000     C* ANNO/MESE FINALE
050800000000     C*--------------------
050900000000     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO DI ULTIMO AGGIORNAMENTO - 1
051000000000IF  1C     $ERR          IFEQ      '0'
051100990531IF  2C     V1CDA2        IFGT      AGGAM
051200000000     C                   MOVEL     MSG(9)        $MSG
051300000000     C                   EXSR      CARMSG
051400000000     C*---                 MOVEL'1'       $ERR
051500000000     C*---                 SETON                     62
051600000000E   2C                   ENDIF
051700000000E   1C                   ENDIF
051800000000     C*
051900000000     C                   ENDSR
052000000000     C*------------------------------------------------------------------------*
052100000000     C* CHKFIL - CONTROLLA LA FILIALE
052200000000     C*------------------------------------------------------------------------*
052300000000     C     CHKFIL        BEGSR
052400000000     C*
052500000000     C* CONTROLLO SE E' STATO INSERITO '?'
052600000000     C     '?'           SCAN      V1CFIL                                 99
052700000000     C*
052800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
052900000000IF  1C     *IN99         IFEQ      *ON
053000000000     C                   MOVEL     *BLANKS       §COD1
053100000000     C                   MOVE      *BLANKS       §DES1
053200971203     C                   MOVE      *BLANKS       §PRA
053300980409     C                   CALL      'TNSD24R'
053400000000     C                   PARM                    §COD1             3
053500000000     C                   PARM                    §TIP              1
053600000000     C                   PARM                    §DES1            25
053700971203     C                   PARM                    §PRA              3
053800000000     C* SELEZIONATO
053900000000IF  2C     §DES1         IFNE      *BLANKS
054000000000     C                   MOVEL     §COD1         W1CFIL
054100000000     C                   MOVEL     §DES1         W1DFIL           25
054200000000X   2C                   ELSE
054300000000     C* NON SELEZIONATO
054400000000     C                   MOVEL     *ZEROS        W1CFIL
054500000000E   2C                   ENDIF
054600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
054700000000E   1C                   ENDIF
054800000000     C*
054900000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
055000000000     C                   MOVEL     W1CFIL        ORGFIL
055100000000     C     KEYORG        CHAIN     AZORG                              99
055200000000IF  1C     *IN99         IFEQ      *OFF
055300000000     C     ORGFVA        ANDEQ     *BLANKS
055400000000     C     ORGFAG        ANDNE     'V'
055500000000     C                   MOVEL     ORGDES        W1DFIL
055600000000X   1C                   ELSE
055700000000     C                   MOVEL     '1'           $ERR
055800000000E   1C                   ENDIF
055900000000     C*
056000000000     C                   ENDSR
056100000000     C*------------------------------------------------------------------------*
056200000000     C* CHKARE - CONTROLLA L'AREA
056300000000     C*------------------------------------------------------------------------*
056400000000     C     CHKARE        BEGSR
056500000000     C*
056600000000     C*
056700000000     C* CONTROLLO SE E' STATO INSERITO '?'
056800000000     C     '?'           SCAN      V1CARE                                 99
056900000000     C*
057000000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
057100000000IF  1C     *IN99         IFEQ      *ON
057200020916     C                   MOVEL     1             §KUT
057300000000     C                   MOVEL     '05'          §COD
057400000000     C                   MOVEL     *BLANKS       §KEY
057500000000     C                   MOVEL     *BLANKS       §DES
057600000000     C                   CALL      'X§TABER'
057700000000     C                   PARM                    §KUT              1 0
057800000000     C                   PARM                    §COD              2
057900000000     C                   PARM                    §KEY              8
058000000000     C                   PARM                    §DES             30
058100000000     C                   MOVEL     §KEY          W1CARE
058200000000     C                   MOVEL     §DES          W1DARE           25
058300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
058400000000E   1C                   ENDIF
058500000000     C*
058600000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
058700020916     C                   MOVEL     1             TBLKUT
058800000000     C                   MOVEL     '05'          TBLCOD
058900000000     C                   MOVEL     *BLANKS       TBLKEY
059000000000     C                   MOVEL     W1CARE        TBLKEY
059100000000     C     KEYTAB        CHAIN     TABEL                              99
059200000000IF  1C     *IN99         IFEQ      *ON
059300000000     C     TBLFLG        OREQ      '*'
059400000000     C                   MOVEL     '1'           $ERR
059500000000X   1C                   ELSE
059600000000     C                   MOVEL     TBLUNI        W1DARE
059700000000E   1C                   ENDIF
059800000000     C*
059900000000     C                   ENDSR
060000000000     C*------------------------------------------------------------------------*
060100000000     C* CHKDIV - CONTROLLA LA DIVISIONE
060200000000     C*------------------------------------------------------------------------*
060300000000     C     CHKDIV        BEGSR
060400000000     C*
060500000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
060600000000IF  1C     V1CDIV        IFEQ      '?'
060700020916     C                   MOVEL     1             §KUT
060800000000     C                   MOVEL     '17'          §COD
060900000000     C                   MOVEL     *BLANKS       §KEY
061000000000     C                   MOVEL     *BLANKS       §DES
061100000000     C                   CALL      'X§TABER'
061200000000     C                   PARM                    §KUT
061300000000     C                   PARM                    §COD
061400000000     C                   PARM                    §KEY
061500000000     C                   PARM                    §DES
061600000000     C                   MOVEL     §KEY          W1CDIV
061700000000     C                   MOVEL     §DES          W1DDIV           25
061800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
061900000000E   1C                   ENDIF
062000000000     C*
062100000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
062200020916     C                   MOVEL     1             TBLKUT
062300000000     C                   MOVEL     '17'          TBLCOD
062400000000     C                   MOVEL     *BLANKS       TBLKEY
062500000000     C                   MOVEL     W1CDIV        TBLKEY
062600000000     C     KEYTAB        CHAIN     TABEL                              99
062700000000IF  1C     *IN99         IFEQ      *ON
062800000000     C     TBLFLG        OREQ      '*'
062900000000     C                   MOVEL     '1'           $ERR
063000000000X   1C                   ELSE
063100000000     C                   MOVEL     TBLUNI        W1DDIV
063200000000E   1C                   ENDIF
063300000000     C*
063400000000     C                   ENDSR
063500000000     C*------------------------------------------------------------------------*
063600000000     C* CHKSER - CONTROLLA IL TIPO SERVIZIO
063700000000     C*------------------------------------------------------------------------*
063800000000     C     CHKSER        BEGSR
063900000000     C*
064000000000     C     *LIKE         DEFINE    V1CSER        WSER
064100000000     C     *LIKE         DEFINE    V1DSER        WDSER
064200000000     C*
064300000000     C                   MOVEL     *BLANKS       WDSER
064400000000     C*
064500000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
064600000000IF  1C     WSER          IFEQ      '?'
064700020916     C                   MOVEL     1             §KUT
064800000000     C                   MOVEL     '5E'          §COD
064900000000     C                   MOVEL     *BLANKS       §KEY
065000000000     C                   MOVEL     *BLANKS       §DES
065100000000     C                   CALL      'X§TABER'
065200000000     C                   PARM                    §KUT
065300000000     C                   PARM                    §COD
065400000000     C                   PARM                    §KEY
065500000000     C                   PARM                    §DES
065600000000     C                   MOVEL     §KEY          WSER
065700000000     C                   MOVEL     §DES          WDSER
065800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VID
065900000000E   1C                   ENDIF
066000000000     C*
066100000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
066200020916     C                   MOVEL     1             TBLKUT
066300000000     C                   MOVEL     '5E'          TBLCOD
066400000000     C                   MOVEL     *BLANKS       TBLKEY
066500000000     C                   MOVEL     WSER          TBLKEY
066600000000     C     KEYTAB        CHAIN     TABEL                              99
066700000000IF  1C     *IN99         IFEQ      *ON
066800000000     C     TBLFLG        OREQ      '*'
066900000000     C                   MOVEL     '1'           $ERR
067000000000X   1C                   ELSE
067100000000     C                   MOVEL     TBLUNI        WDSER
067200000000E   1C                   ENDIF
067300000000     C*
067400000000     C                   ENDSR
067500000000     C*------------------------------------------------------------------------*
067600000000     C* IMPVID  - IMPOSTAZIONI INIZIALI VIDEO
067700000000     C*------------------------------------------------------------------------*
067800000000     C     IMPVID        BEGSR
067900981117     C*
068000010410     C     *LIKE         DEFINE    §5SFSE        WFSE
068100000000     C*--------------------
068200000000     C*                      \  DATA ULTIMO AGGIORNAMENTO
068300000000     C* REPERISCO DA TABELLA:
068400000000     C*                      /  FLAG DI AGGIORNAMENTO
068500000000     C*--------------------
068600020916     C                   MOVEL     1             TBLKUT
068700010410     C                   MOVEL     '5S'          TBLCOD
068800000000     C                   MOVEL     *BLANKS       TBLKEY
068900000000     C     KEYTA2        CHAIN     TABEL00F                           99
069000000000IF  1C     *IN99         IFEQ      *ON
069100981117     C                   MOVEL     'N'           WFSE
069200000000X   1C                   ELSE
069300010410     C                   MOVEL     TBLUNI        DS5S
069400010410     C                   Z-ADD     §5SDAE        WDAE                           *ULTIMO AGGIORNAMENTO
069500010410     C                   MOVEL     §5SFSE        WFSE
069600000000E   1C                   ENDIF
069700000000     C*
069800000000     C* DATA ULTIMO AGGIORNAMENTO - 1 MESE
069900000000     C                   SUB       1             AGGMM
070000000000IF  1C     AGGMM         IFEQ      *ZEROS                                       *SE GENNAIO
070100000000     C                   Z-ADD     12            AGGMM                          *IMPOSTO DICEMBRE
070200000000     C                   SUB       1             AGGAA                          *DELL'ANNO PRECEDENTE
070300000000E   1C                   ENDIF
070400000000     C*--------------------
070500000000     C* DATE DI ELABORAZIONE
070600000000     C*--------------------
070700000000     C***
070800000000     C* AA/MM FINALE   --> ULTIMO AGGIORNAMENTO - 1 MESE
070900000000     C* AA/MM INIZIALE --> INIZIO ANNO
071000000000     C***
071100000000     C*--------------------
071200000000     C* D A T A   F I N E
071300000000     C*--------------------
071400990531     C                   Z-ADD     AGGAM         V1CDA2
071500000000     C*--------------------
071600000000     C* D A T A   I N I Z I O
071700000000     C*--------------------
071800990531     C                   Z-ADD     AGGAM         V1CDAT
071900000000     C                   MOVE      01            V1CDAT
072000000000     C*--------------------
072100000000     C* RAGIONE SOCIALE
072200000000     C*--------------------
072300020916     C                   MOVEL     RSUT          VTDRSU                         *RAG.SOC.UTENTE
072400000000     C*--------------------
072500000000     C* RICHIESTE
072600000000     C*--------------------
072700000000     C                   MOVEL     'N'           V1CDET                         *DETTAGLIO FIL/FIL
072800950531     C                   MOVEL     'S'           V1CFPC                         *CREAZIONE FILE SU PC
072900950525     C                   MOVEL     'I'           V1CIOE                         *DATI IN STATISTICA
073000000704     C                   MOVEL     'N'           V1CPOS                         *DATI POSTE
073100000000     C*--------------------
073200000000     C* AGGIORNAMENTO RETTIFICHE E NOTE DI CREDITO
073300000000     C*--------------------
073400000000     C* SE IL FLAG IN TABELLA E' "S" NON BISOGNA SPALMARE (IMPOSTO A VIDEO "N")
073500000000     C* SE IL FLAG IN TABELLA E' "N" BISOGNA SPALMARE     (IMPOSTO A VIDEO "S")
073600981117IF  1C     WFSE          IFEQ      'S'                                          *GIA' SPALMATI
073700000000     C                   MOVEL     'N'           V1CRNC                         *N O  SPALMAMENTO
073800000000X   1C                   ELSE                                                   *NON SPALMATI
073900000000     C                   MOVEL     'S'           V1CRNC                         *S I  SPALMAMENTO
074000000000E   1C                   ENDIF
074100000000     C*
074200000000     C                   ENDSR
074300020916     C*--------------------------------------------------------------------------------------------*
074400020916     C* REPERISCE I DATI UTENTE
074500020916     C*--------------------------------------------------------------------------------------------*
074600020916     C     REPDATIUTE    BEGSR
074700020916     C*
074800020916     C* INIZIALIZZA VARIABILI DI WRK
074900020916     C                   CLEAR                   TIBS34DS
075000020916     C                   CLEAR                   AZUTEDS
075100020916     C                   CLEAR                   DDATIUTE
075200020916     C*
075300020916     C     *DTAARA       DEFINE    §azute        azuteds
075400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
075500020916     C                   IN(E)     *DTAARA
075600020916if  1C                   IF        %Error
075700020916     c                   EVAL      I34Tla = 'L'
075800020916     C                   CALL      'TIBS34R'
075900020916     C                   PARM                    Tibs34Ds
076000020916     C                   IN        *DTAARA
076100020916e   1C                   ENDIF
076200020916     C*
076300020916     C                   ENDSR
076400000000     C*------------------------------------------------------------------------*
076500000000     C* *INZSR  - IMPOSTAZIONI INIZIALI
076600000000     C*------------------------------------------------------------------------*
076700000000     C     *INZSR        BEGSR
076800000000     C*--------------------
076900000000     C* RICEVIMENTO PARAMETRI
077000000000     C*--------------------
077100000000     C     *ENTRY        PLIST
077200000000     C                   PARM                    KPJBA
077300000000     C*--------------------
077400000000     C* CHIAVI DI LETTURA
077500000000     C*--------------------
077600000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
077700000000     C     KEYORG        KLIST
077800000000     C                   KFLD                    ORGFIL
077900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
078000000000     C     KEYTAB        KLIST
078100000000     C                   KFLD                    TBLKUT
078200000000     C                   KFLD                    TBLCOD
078300000000     C                   KFLD                    TBLKEY
078400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
078500000000     C     KEYTA2        KLIST
078600000000     C                   KFLD                    TBLKUT
078700000000     C                   KFLD                    TBLCOD
078800000000     C*
078900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
079000000000     C                   EXSR      INZSTD
079100000000     C*
079200000000     C* IMPOSTAZIONI INIZIALI VIDEO
079300000000     C                   EXSR      IMPVID
079400000000     C*
079500000000     C* EMETTE FORMATO VIDEO
079600000000     C                   EXSR      CARV1
079700000000     C*
079800000000     C                   ENDSR
079900000000     C*------------------------------------------------------------------------*
080000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080200000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
080300000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
080400000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
080500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080700000000     C*------------------------------------------------------------------------*
080800000000     C* INZSTD - ROUTINE INIZIALE STANDARD
080900000000     C*------------------------------------------------------------------------*
081000000000     C     INZSTD        BEGSR
081100000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
081200000000     C                   CALL      'XKEYS'
081300000000     C                   PARM                    DSKEYS
081400000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ.
081500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
081600000000     C     XMSGPA        PLIST
081700000000     C                   PARM                    DSMSMS
081800000000     C                   PARM                    DSMSPG
081900000000     C                   PARM                    DSMSKE
082000000000     C                   PARM                    DSMSER
082100000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
082200010511     C                   MOVEL     'TISE55R'     DSMSPG                         *NOME PGM CHIAMANTE
082300010511     C                   MOVEL     'TISE55R'     VMCQUE                         *CODA MESSAGGI DESTIN
082400000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
082500000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
082600000000     C* INIZIALIZZO FLAG CONTROLLO PGM
082700000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
082800000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
082900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
083000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
083100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
083200000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDEO
083300000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
083400000000     C* LANCIO ROUTINE PER VERIFICARE SE
083500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
083600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
083700020916     C                   EXSR      REPDATIUTE
083800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
083900000000     C                   BITOFF    '01234567'    POSMI1            1
084000000000     C                   BITOFF    '01234567'    POSMI2            1
084100000000     C                   BITOFF    '01234567'    POSMA1            1
084200000000     C                   BITOFF    '01234567'    POSMA2            1
084300000000     C                   BITON     '34'          POSMI1            1            *18
084400000000     C                   BITON     '7'           POSMI2            1            *01
084500000000     C                   BITON     '34'          POSMA1            1            *18
084600000000     C                   BITON     '0'           POSMA2            1            *80
084700000000     C                   MOVE      POSMI2        POSMIN            2
084800000000     C                   MOVEL     POSMI1        POSMIN
084900000000     C                   MOVE      POSMA2        POSMAX            2
085000000000     C                   MOVEL     POSMA1        POSMAX
085100000000     C                   ENDSR
085200000000     C*------------------------------------------------------------------------*
085300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
085400000000     C*------------------------------------------------------------------------*
085500000000     C     FUNSTD        BEGSR
085600000000     C                   SELECT
085700000000     C     KEY           WHENEQ    F3
085800000000     C                   EXSR      F03STD
085900000000     C     KEY           WHENEQ    F12
086000000000     C                   EXSR      F12STD
086100000000     C     KEY           WHENEQ    F24
086200000000     C                   EXSR      F24STD
086300000000     C                   OTHER
086400000000     C                   MOVEL     MSG(2)        $MSG
086500000000     C                   EXSR      CARMSG
086600000000     C                   ENDSL
086700000000     C                   ENDSR
086800000000     C*------------------------------------------------------------------------*
086900000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
087000000000     C*------------------------------------------------------------------------*
087100000000     C     F03STD        BEGSR
087200000000     C*
087300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
087400000000     C                   MOVEL     '1'           $FINE
087500000000     C*
087600000000     C                   ENDSR
087700000000     C*------------------------------------------------------------------------*
087800000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
087900000000     C*------------------------------------------------------------------------*
088000000000     C     F12STD        BEGSR
088100000000     C*
088200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
088300000000     C                   MOVEL     '1'           $FINE
088400000000     C*
088500000000     C                   ENDSR
088600000000     C*------------------------------------------------------------------------*
088700000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
088800000000     C*------------------------------------------------------------------------*
088900000000     C     F24STD        BEGSR
089000000000     C*
089100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
089200000000     C                   EXSR      OUTFUN
089300000000     C*
089400000000     C                   ENDSR
089500000000     C*------------------------------------------------------------------------*
089600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
089700000000     C*          CODA MESSAGGI DEL PGM
089800000000     C*------------------------------------------------------------------------*
089900000000     C     CARMSG        BEGSR
090000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
090100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
090200000000     C     MSGULT        IFEQ      0
090300000000     C                   MOVEL     '1'           *IN40
090400010511     C                   WRITE     SE55DVK
090500000000     C                   MOVEL     '0'           *IN40
090600000000     C                   ENDIF
090700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
090800000000     C                   MOVEL     $MSG          DSMSMS
090900000000     C                   CALL      'XMSG'        XMSGPA
091000000000     C     DSMSER        IFEQ      0
091100000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGIO
091200000000     C                   ENDIF
091300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
091400000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
091500010511     C                   WRITE     SE55DVM                                      *SFL MESS
091600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
091700000000     C                   MOVEL     *BLANKS       $MSG
091800000000     C                   ENDSR
091900000000     C*------------------------------------------------------------------------*
092000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
092100000000     C*------------------------------------------------------------------------*
092200000000     C     OUTMSG        BEGSR
092300000000     C* VISUALIZZAZIONE MESSAGGIO
092400000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
092500010511     C                   WRITE     SE55DVK                                      *CONTR MESS
092600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
092700000000     C                   Z-ADD     *ZEROS        MSGULT
092800000000     C                   ENDSR
092900000000     C*------------------------------------------------------------------------*
093000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
093100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
093200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
093300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
093400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
093500000000     C*------------------------------------------------------------------------*
093600000000     C     CARFUN        BEGSR
093700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL.
093800000000     C     1             DO        31            I                 5 0          --- 1 -->
093900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
094000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
094100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
094200020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
094300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
094400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
094500000000     C                   ELSE                                                   *INTERROG.
094600000000     C                   MOVEL     FUNASI        FAT(I)
094700000000     C                   ENDIF
094800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
094900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
095000000000     C                   MOVEL     FUNAFG        FAT(I)
095100000000     C                   ELSE                                                   *INTERROG.
095200000000     C                   MOVEL     FUNAFI        FAT(I)
095300000000     C                   ENDIF
095400000000     C                   ENDIF                                                  <-- 2 ---
095500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
095600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
095700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
095800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
095900000000     C                   ELSE
096000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
096100000000     C                   ENDIF
096200000000     C                   ENDIF
096300000000     C                   ENDDO                                                  <-- 1 ---
096400000000     C                   ENDSR
096500000000     C*------------------------------------------------------------------------*
096600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
096700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
096800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
096900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
097000000000     C*------------------------------------------------------------------------*
097100000000     C     OUTFUN        BEGSR
097200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
097300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
097400000000     C                   ENDDO
097500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
097600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
097700000000     C     1             DO        31            I
097800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
097900000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZIONE
098000000000     C                   ADD       1             FUNNAT
098100000000     C                   ENDIF
098200000000     C                   ENDDO
098300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
098400000000     C     FUNNAT        IFGT      8
098500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
098600000000     C                   ENDIF
098700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
098800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
098900000000     C                   Z-ADD     8             FUNVIS            5 0
099000000000     C     FAT(23)       IFEQ      'S'
099100000000     C                   SUB       1             FUNVIS
099200000000     C                   ENDIF
099300000000     C     FAT(24)       IFEQ      'S'
099400000000     C                   SUB       1             FUNVIS
099500000000     C                   ENDIF
099600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
099700000000     C                   Z-ADD     *ZEROS        W                 5 0
099800000000     C                   ADD       1             FUNULT
099900000000     C     FUNULT        IFGT      31
100000000000     C                   Z-ADD     1             FUNULT
100100000000     C                   ENDIF
100200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
100300000000     C     FUNULT        DO        31            I                              --- 1 -->
100400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
100500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
100600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
100700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
100800000000     C     FDE(I)        ANDNE     *BLANKS
100900000000     C     I             IFEQ      23                                           *TOLGO F23
101000000000     C     I             OREQ      24                                           *E F24
101100000000     C                   ELSE
101200000000     C                   ADD       1             W
101300000000     C                   MOVEL     FDE(I)        FUV(W)
101400000000     C                   ENDIF
101500000000     C                   ENDIF                                                  <-- 3 ---
101600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
101700000000     C                   Z-ADD     I             FUNULT
101800000000     C                   ENDIF                                                  <-- 2 ---
101900000000     C                   ENDDO                                                  <-- 1 ---
102000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
102100000000     C                   MOVEL     FUV(1)        VFD001
102200000000     C                   MOVEL     FUV(2)        VFD002
102300000000     C                   MOVEL     FUV(3)        VFD003
102400000000     C                   MOVEL     FUV(4)        VFD004
102500000000     C                   MOVEL     FUV(5)        VFD005
102600000000     C                   MOVEL     FUV(6)        VFD006
102700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
102800000000     C     FAT(24)       ANDEQ     'N'
102900000000     C                   MOVEL     FUV(7)        VFD007
103000000000     C                   MOVEL     FUV(8)        VFD008
103100000000     C                   ELSE                                                   <-- 1 -->
103200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
103300000000     C     FAT(24)       ANDEQ     'S'
103400000000     C                   MOVEL     FDE(23)       VFD007
103500000000     C                   MOVEL     FDE(24)       VFD008
103600000000     C                   ELSE                                                   <-- 2 -->
103700000000     C                   MOVEL     FUV(7)        VFD007
103800000000     C     FAT(23)       IFEQ      'S'
103900000000     C                   MOVEL     FDE(23)       VFD008
104000000000     C                   ELSE
104100000000     C                   MOVEL     FDE(24)       VFD008
104200000000     C                   ENDIF
104300000000     C                   ENDIF                                                  <-- 2 ---
104400000000     C                   ENDIF                                                  <-- 1 ---
104500000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TESTA
104600000000     C                   ENDSR
104700000000     C*------------------------------------------------------------------------*
104800000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
104900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
105000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
105100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
105200000000     C*------------------------------------------------------------------------*
105300000000     C     CONFUN        BEGSR
105400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
105500000000     C     1             DO        31            I                              --- 1 -->
105600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
105700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
105800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
105900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABILE
106000000000     C                   EXSR      CARMSG
106100000000     C                   MOVEL     '1'           $ERR
106200000000     C                   ENDIF                                                  <-- 3 ---
106300000000     C                   ENDIF                                                  <-- 2 ---
106400000000     C                   ENDDO                                                  <-- 1 ---
106500000000     C                   ENDSR
106600000000     O*------------------------------------------------------------------------*
106700000000**   SCHIERA: MSG  (MESSAGGI)
106800010511TISE55R- Tasto funzionale non valido.                                          1
106900010511TISE55R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
107000010511TISE55R- Opzione non valida.                                                   3
107100010511TISE55R- Opzione abilitata ma non gestita. Avvertire CED.                      4
107200010511TISE55R- Inizio elenco.                                                        5
107300010511TISE55R- Fine elenco.                                                          6
107400010511TISE55R- Anno/Mese obbligatorio                                                7
107500010511TISE55R- Anno/Mese errato                                                      8
107600010511TISE55R- Data superiore alla data di ultimo aggiornamento saldi                9
107700010511TISE55R- 'S' per spalmare rettifiche e note di credito, 'N' per non spalmarle 10
107800010511TISE55R- Anno/Mese finale inferiore all'Anno/Mese iniziale                    11
107900010511TISE55R- Punto operativo errato                                               12
108000010511TISE55R- Area errata                                                          13
108100010511TISE55R- Divisione errata                                                     14
108200010511TISE55R- Tipo servizio non accettabile                                        15
108300010511TISE55R- Tipo prodotto non accettabile                                        16
108400010511TISE55R- Immettere solo una selezione per punto operativo, area o divisione   17
108500010511TISE55R- Richiedere stampa partenze o arrivi                                  18
108600010511TISE55R- Impossibile stampare più di 12 mesi                                  19
108700010511TISE55R- 'S' per stampare il dettaglio P.O/P.O o 'N' per non stamparlo        20
108800010511TISE55R- 'S' per creare file su PC, 'N' per non crearlo                       21
108900010511TISE55R- ATTENZIONE !! Saldi non spalmati con rettifiche e note               22
109000010511TISE55R- ATTENZIONE !! Saldi già spalmati con rettifiche e note               23
109100010511TISE55R- Non si possono lanciare le statistiche di gruppo in SDI !!!          24
109200020325TISE55R- Impossibile lanciare la statistica su anni differenti                25
109300020325TISE55R-
109400020325TISE55R-
10950095052528
109600010511TISE55R- Immettere il tipo di dati che si vuole in statistica                 29
10970095052530
109800000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
109900000000F1    -                   -    NN     NN
110000000000F2    -                   -    NN     NN
110100000000F3    -F3=Fine            -    SS     SS
110200000000F4    -                   -    NN     NN
110300000000F5    -                   -    NN     NN
110400000000F6    -F6=Lancio          -    SS     SS
110500000000F7    -                   -    NN     NN
110600000000F8    -                   -    NN     NN
110700000000F9    -                   -    NN     NN
110800000000F10   -                   -    NN     NN
110900000000F11   -                   -    NN     NN
111000000000F12   -F12=Ritorno        -    SS     SS
111100000000F13   -                   -    NN     NN
111200000000F14   -                   -    NN     NN
111300000000F15   -                   -    NN     NN
111400000000F16   -                   -    NN     NN
111500000000F17   -                   -    NN     NN
111600000000F18   -                   -    NN     NN
111700000000F19   -                   -    NN     NN
111800000000F20   -                   -    NN     NN
111900000000F21   -                   -    NN     NN
112000000000F22   -                   -    NN     NN
112100000000F23   -                   -    NN     NN
112200000000F24   -F24=Altri tasti    -    NN     NN
112300000000ENTER -                   -    SS     SS
112400000000ROLLUP-                   -    NN     NN
112500000000ROLLDN-                   -    NN     NN
112600000000HELP  -                   -    NN     NN
112700000000PRINT -                   -    NN     NN
112800000000BSPACE-                   -    NN     NN
112900000000CLEAR -                   -    NN     NN
