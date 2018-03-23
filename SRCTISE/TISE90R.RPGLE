000100000000     H*------------------------------------------------------------------------*
000200020404     H*                  STATISTICHE DELTA       - RICHIESTA PARAMETRI -       *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600950418     F* DATA BASE
000700950418     F*------------------------------------------------------------------------*
000800020116     FTISE90D   CF   E             WORKSTN INFDS(DEVDS1)
000900020116     F                                     SFILE(SE90DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100140612     FAZCMM01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300950418     D*------------------------------------------------------------------------*
001400950418     D* SCHIERE
001500950418     D*------------------------------------------------------------------------*
001600950418     D*-------------------
001700950418     D* SCHIERE A TEMPO DI COMPILAZIONE
001800950418     D*-------------------
001900000707     D MSG             S             78    DIM(40) CTDATA PERRCD(1)             *MESSAGGI
002000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002100950418     D*-------------------
002200000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002300950418     D*-------------------
002400000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002500000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002600000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002700950418     D*-------------------
002800000000     D* TABELLA DESCRIZIONI A VIDEO
002900950418     D*-------------------
003000000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003100950418     D*-------------------
003200000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003300950418     D*-------------------
003400000000     D KEYS            S              1    DIM(40)
003500000000     D*------------------------------------------------------------------------*
003600950418     D* INPUT
003700950418     D*------------------------------------------------------------------------*
003800950418     D*-------------------
003900000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004000950418     D*-------------------
004100000000     D DSFUN         E DS
004200000000     D  FUNRIG                 1     40
004300950418     D*-------------------
004400000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004500950418     D*-------------------
004600000000     D DSMSG         E DS
004700950418     D*-------------------
004800000000     D* DS PER GESTIONE TASTI FUNZIONALI
004900950418     D*-------------------
005000000000     D DSKEYS        E DS
005100000000     D  KEYRIG                 1     40
005200950418     D*-------------------
005300000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005400950418     D*-------------------
005500000000     D                SDS
005600000000     D  USER                 254    263                                         UTENTE
005700950418     D*-------------------
005800000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005900950418     D*-------------------
006000000000     D DEVDS1          DS
006100000000     D  KEY                  369    369
006200000000     D  CURSOR               370    371
006300950418     D*-------------------
006400000000     D* ARCHITETTURA
006500950418     D*-------------------
006600000000     D KPJBA         E DS
006700950418     D*-------------------
006800020916     D* DS REPERIMENTO DATI UTENTE
006900020916     D*-------------------
007000020916     D TIBS34DS      E DS                                                       *Profili utente
007100140612     d TRMK43ds      e ds                  inz
007200020916     D DDATIUTE      E DS                                                       *Dati utente
007300020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007400980403     D*-------------------
007500980403     D* DS DATI VARI STATISTICHE DI GRUPPO
007600980403     D*-------------------
007700980403     D DS5S          E DS
007800090610     D*-------------------
007900090610     D* DS descrizione tipi servizio
008000090610     D*-------------------
008100090610     D DS5e          E DS
008200950418     D*-------------------
008300000000     D* DS COMMERCIALI
008400950418     D*-------------------
008500140612     D***** DS01          E DS
008600990609     D*------------------
008700990609     D* DS CONTROLLO DATA (8)
008800990609     D*------------------
008900990609     D WLBDA8          DS                  INZ
009000990609     D  G08DAT                 1      8  0
009100990609     D  G08INV                 9     16  0
009200990609     D  G08ERR                17     17
009300990609     D  G08TGI                18     22  0
009400000000     D*------------------
009500000000     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
009600000000     D*------------------
009700000000     D                 DS
009800990609     D  AGGAA                  1      4  0
009900981117     D  AGGMM                  5      6  0
010000990609     D  AGGAM                  1      6  0
010100981117     D  AGGGG                  7      8  0
010200981117     D  WDAC                   1      8  0
010300000000     D*------------------
010400000000     D* DS DATA ULTIMA CANCELLAZIONE SALDI
010500000000     D*------------------
010600980202     D                 DS
010700990609     D  CANAA                  1      4  0
010800981117     D  CANMM                  5      6  0
010900990609     D  CANAM                  1      6  0
011000981117     D  CANGG                  7      8  0
011100981117     D  WDCC                   1      8  0
011200951220     D*------------------
011300951220     D* DATA INIZIALE PRECEDENTE
011400951220     D*------------------
011500951220     D                 DS
011600990609     D  ANPAAI                 1      4  0
011700990609     D  ANPMMI                 5      6  0
011800990609     D  ANPDAT                 1      6  0
011900951220     D*------------------
012000951220     D* DATA FINALE PRECEDENTE
012100951220     D*------------------
012200951220     D                 DS
012300990609     D  ANPAAF                 1      4  0
012400990609     D  ANPMMF                 5      6  0
012500990609     D  ANPDA2                 1      6  0
012600000000     D*------------------
012700000000     D* PARAMETRI LANCIO
012800000000     D*------------------
012900000000     D PARAM           DS                  INZ
013000990609     D  PARDAT                 1      6  0
013100990609     D  PARAAI                 1      4  0
013200990609     D  PARMMI                 5      6  0
013300990609     D  PARDA2                 7     12  0
013400990609     D  PARAAF                 7     10  0
013500990609     D  PARMMF                11     12  0
013600990609     D  PARDCK                13     13
013700990609     D  PARDCC                14     14
013800990609     D  PARDCF                15     15
013900990609     D  PARFIC                16     18  0
014000990609     D  PARARC                19     21  0
014100990609     D  PARDIC                22     22
014200990609     D  PARDFF                23     23
014300990609     D  PARCOF                24     30  0
014400990609     D  PARDIF                31     31
014500990609     D  PARSTA                32     32
014600000823     D  PARSCE                33     33
014700990609     D  PARTSP                34     34
014800990609     D  PARIOE                35     35
014900990609     D  PARCLV                36     36
015000990609     D  PARDPT                37     42  0
015100990609     D  PARDP2                43     48  0
015200990609     D  PARSPD                49     52  1
015300990609     D  PARPDD                53     56  1
015400990609     D  PARPDA                57     60  1
015500000707     D  PARPOS                61     61
015600000825     D  PARCRE                62     62
015700010618     D  PARCL1                63     63
015800010618     D  PARCL2                64     64
015900010618     D  PARSIN                65     65
016000020220     D  PARCL3                66     66
016100020220     D  PARCL4                67     67
016200020404     D  PARNTW                68     68
016300020701     D  PARDFD                69     72  1
016400020701     D  PARDFA                73     76  1
016500020701     D  PARDFDP               77     80  1
016600020701     D  PARDFAP               81     84  1
016700030311     D  PARFPC                85     85
016800030912     D  PARWPC                86     86
016900030923     D  PARWCP                87     87
017000090824     D  PARWfdpv              88     88
017100090824      * Stat. Clienti Persi (Trimestrale x il momento)                          lancio trimestre(T)
017200090824     D  PARCLPtmp             89     89                                         M/B/T/Q/S
017300000000     C*------------------------------------------------------------------------*
017400000000     C*?MAIN LINE
017500000000     C*------------------------------------------------------------------------*
017600000000     C*
017700000000     C* CICLO FINO A CHE:
017800000000DO  1C     $FINE         DOUEQ     '1'
017900000000     C*
018000000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
018100000000     C*
018200000000     C* CONTROLLA GESTIONE VIDEO
018300000000IF  2C     $GES          IFEQ      'V1'
018400000000     C                   EXSR      GESV1                                        *PARAMETRI
018500000000E   2C                   ENDIF
018600000000     C*
018700000000E   1C                   ENDDO
018800000000     C*
018900000000     C                   SETON                                        LR
019000000000     C*------------------------------------------------------------------------*
019100000000     C*?GESV1 - GESIONE PRIMO VIDEO
019200000000     C*------------------------------------------------------------------------*
019300000000     C     GESV1         BEGSR
019400000000     C*
019500000000IF  1C     $TESVD        IFEQ      '1'                                          TESTATA DA RIEMETTER
019600000000     C                   EXSR      CARV1                                        *CARICA VIDEO
019700000000E   1C                   ENDIF
019800000000     C*
019900000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
020000000000     C                   MOVEL     *BLANKS       $MSG
020100000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
020200000000E   1C                   ENDIF
020300000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
020400000000     C*
020500020116     C                   EXFMT     SE90DV1                                      *PARAMETRI
020600000000     C*
020700000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
020800000000     C*
020900000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
021000000000SL  2C                   SELECT
021100000000     C     KEY           WHENEQ    ENTER
021200000000     C                   EXSR      ENTV1                                        *ENTER--> DECODIFICA
021300000000     C     KEY           WHENEQ    F6
021400000000     C                   EXSR      F06GES                                       *F06 --> LANCIO
021500000000     C                   OTHER
021600000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
021700000000E   2C                   ENDSL
021800000000E   1C                   ENDIF
021900000000     C*
022000000000     C                   ENDSR
022100000000     C*------------------------------------------------------------------------*
022200000000     C*?CARV1 - CARICAMENTO PRIMO VIDEO
022300000000     C*------------------------------------------------------------------------*
022400000000     C     CARV1         BEGSR
022500000000     C*
022600100510     C* messaggio di avviso se nella libreria Budget mancano i files necessari
022700100510     c                   if            manca_file = 'S'  and lib_budget = 'S'
022800100510     c                   exfmt     win_alert
022900100510     c                   end
023000100510     C*
023100000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
023200000000     C*
023300000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
023400000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
023500000000     C*
023600020116     C                   WRITE     SE90DVT                                      *TESTATA
023700020116     C                   WRITE     SE90DVF                                      *FUNZIONI
023800000000     C*
023900000000     C                   ENDSR
024000000000     C*------------------------------------------------------------------------*
024100000000     C*?ENTV1 - GESTIONE TASTO "ENTER" PRIMO VIDEO
024200000000     C*------------------------------------------------------------------------*
024300000000     C     ENTV1         BEGSR
024400000000     C*
024500000000     C* CONTROLLA I CAMPI IMMESSI
024600000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
024700000000     C*
024800000000     C                   ENDSR
024900000000     C*------------------------------------------------------------------------*
025000000000     C*?F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
025100000000     C*------------------------------------------------------------------------*
025200000000     C     F06GES        BEGSR
025300000000     C*
025400000000     C* CONTROLLA I CAMPI IMMESSI
025500000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
025600000000     C*
025700000000     C* NON CI SONO ERRORI
025800000000IF  1C     $ERR          IFEQ      '0'
025900000000     C*
026000000000     C* LANCIO PROGRAMMA DI GUIDA
026100000000     C                   Z-ADD     V1CDAT        PARDAT                         *AA/MM INIZIALE
026200000000     C                   Z-ADD     V1CDA2        PARDA2                         *AA/MM FINALE
026300000000     C                   MOVEL     V1CDCK        PARDCK                         *1^ STATISTICA
026400000000     C                   MOVEL     V1CDCC        PARDCC                         *2^ STATISTICA
026500000000     C                   MOVEL     V1CDCF        PARDCF                         *3^ STATISTICA
026600000000     C                   MOVEL     W1CFIL        PARFIC                         *FILIALE
026700000000     C                   MOVEL     W1CARE        PARARC                         *AREA
026800000000     C                   MOVEL     V1CDIC        PARDIC                         *DIVISIONE
026900000000     C                   MOVEL     V1CDFF        PARDFF                         *4^ STATISTICA
027000000000     C                   MOVEL     W1CCOM        PARCOF                         *COMMERCIALE
027100000000     C                   MOVEL     V1CDIF        PARDIF                         *DIVISIONE
027200000000     C                   MOVEL     V1CTSP        PARTSP                         *TIPO SERVIZIO
027300950207     C                   MOVEL     V1CIOE        PARIOE                         *ITALIA/ESTERO/TUTTO
027400951018     C                   MOVEL     V1CCLV        PARCLV                         *QUALITA' CLIENTE
027500010618     C                   MOVEL     V1CCL1        PARCL1                         *QUALITA' CLIENTE
027600010618     C                   MOVEL     V1CCL2        PARCL2                         *QUALITA' CLIENTE
027700020220     C                   MOVEL     V1CCL3        PARCL3                         *QUALITA' CLIENTE
027800020220     C                   MOVEL     V1CCL4        PARCL4                         *QUALITA' CLIENTE
027900010618     C                   MOVEL     V1CSIN        PARSIN                         *SINTETICA
028000020404     C                   MOVEL     V1CNTW        PARNTW                         *ROTTURA NETWORK ESTERI
028100970819     C                   Z-ADD     V1CSPD        PARSPD                         *% SCOSTAMENTO DELTA
028200980527     C                   Z-ADD     V1CPDD        PARPDD                         *% DELTA TOTALE DA
028300980527     C                   Z-ADD     V1CPDA        PARPDA                         *% DELTA TOTALE A
028400000707     C                   MOVEL     V1CPOS        PARPOS                         *DATI POSTE
028500000821     C                   MOVEL     V1CSCE        PARSCE                         *SCELTA CLIENTE
028600030313     C                   MOVEL     'S'           PARCRE                         *CREA SEMPRE WFILE
028700020701     C                   Z-ADD     V1CDFD        PARDFD                         *% SCOSTAMENTO FATTURATO DA MESE
028800020701     C                   Z-ADD     V1CDFA        PARDFA                         *% SCOSTAMENTO FATTURATO  A MESE
028900020701     C                   Z-ADD     V1CDFDP       PARDFDP                        *% SCOSTAMENTO FATTURATO DA PROG
029000020701     C                   Z-ADD     V1CDFAP       PARDFAP                        *% SCOSTAMENTO FATTURATO  A PROG
029100030311     C                   MOVEL     V1CFPC        PARFPC                         *CREAZIONE FILE PC
029200030912     C                   MOVEL     V1CWPC        PARWPC                         *CREAZIONE FILE PC del TISE94R
029300030923     C                   MOVEL     V1CWCP        PARWCP                         *CREAZIONE FILE PC del TISE93R
029400951220     C*
029500951220     C* CALCOLA 12 MESI PRECEDENTI ALLA DATA INIZIALE RICHIESTA
029600951220     C                   EXSR      X12PRE
029700951220     C                   Z-ADD     ANPDAT        PARDPT                         AA/MM INIZ.PRECEDENT
029800951220     C                   Z-ADD     ANPDA2        PARDP2                         AA/MM FIN. PRECEDENT
029900000000     C*
030000000000     C* QUESTO PARAMETRO VIENE VALORIZZATO DAL CL DI GUIDA PER SAPERE QUALE
030100000000     C* DELLE TRE STATISTICHE DELTA COMMERCIALE, IL PGM DI STAMPA DEVE ELABORARE
030200000000     C                   MOVEL     *BLANKS       PARSTA                         *LASCIARE VUOTO
030300090824      *
030400090824      * Stat. Clienti Persi (Mensilmente x il momento quindi sempre)
030500090824     c                   eval      PARCLPtmp = 'M'
030600100510     C*
030700100510     C*  Se richiesta un'elaborazione completa di ITALIA + ESTERO
030800100510     C*   occorre sottomettere 2 volte:
030900100510     C*    la 1°volta pulisce il file guida estraendo tutto il necessario
031000100510     C*    la 2°volta NON deve più pulire i dati generati ma solo leggerli
031100100510     C*         e accodarli ai precedenti.
031200100510      *
031300100510     C* la prima volta esegue elaborazioni per l'Italia
031400100510     c                   if        v1cIOE = 'T'
031500100510     c                   move      'I'           parIOE
031600100510     C                   MOVEL     'S'           parNTW                         ROTTURA NETWORK ESTERI
031700100510     c                   eval      PARCLPtmp = 'N'                              NON FARE (CLP)
031800160713     c                   clear                   kpjbu
031900100510     c                   end
032000160713      *
032100160713     C* Se INVECE stiamo eseguendo in AMBIENTE BUDGET x SIMULARE
032200160713      *     imposta la DATA di riferimento UNIFICANTE e totalizza ITALIA +ESTERO
032300160713      *     su un unico FILE.
032400160713     C                   if        *in10  and  h1dtUNI > 0
032500160713     C                   move      h1dtUNI       kpjbu
032600160713     c                   end
032700160712      *
032800020116     C                   MOVEL     'SE91'        KCOAZ
032900000000     C                   MOVEL     PARAM         KPJBU
033000030313     C                   CALL      'BCH10'
033100030313 MOM C***                CALL      'TISE91C'
033200000000     C                   PARM                    KPJBA
033300000000     C*
033400100510     C* la seconda volta esegue elaborazioni in accodamento per l'Estero
033500100510     c                   if        v1cIOE = 'T'
033600100510     c                   move      'E'           parIOE
033700100510     C                   MOVEL     'N'           parCRE                         *NON deve CREARE il WorkFILE
033800100510     C                   MOVEL     'S'           parNTW                         ROTTURA NETWORK ESTERI
033900100510     c                   eval      PARCLPtmp = 'T'                              TRIMESTRALE
034000100510     C                   MOVEL     'SE91'        KCOAZ
034100100510     C                   MOVEL     PARAM         KPJBU
034200100510     C                   CALL      'BCH10'
034300100510 MOM C***                CALL      'TISE91C'
034400100510     C                   PARM                    KPJBA
034500100510     c                   end
034600100510     C*
034700950525     C*---                 MOVEL'1'       $FINE            *USCITA DAL PGM
034800000000E   1C                   ENDIF
034900000000     C*
035000000000     C                   ENDSR
035100000000     C*------------------------------------------------------------------------*
035200000000     C*?DECODI - CONTROLLA CAMPI E DECODIFICA
035300000000     C*------------------------------------------------------------------------*
035400000000     C     DECODI        BEGSR
035500000000     C*
035600950525     C* IMPOSTA INDICATORI DI ERRORE SUI CAMPI
035700160712     C                   SETOFF                                           59
035800000000     C                   SETOFF                                       606162
035900000000     C                   SETOFF                                       636465
036000000000     C                   SETOFF                                       666768
036100000000     C                   SETOFF                                       697071
036200970819     C                   SETOFF                                       727374
036300000707     C                   SETOFF                                       757677
036400050228     C                   SETOFF                                       787980
036500000000     C*
036600950525     C* REIMPOSTA VARIABILI DI LAVORO
036700000000     C                   Z-ADD     *ZEROS        SELEZC            1 0
036800000000     C                   Z-ADD     *ZEROS        SELEZF            1 0
036900000000     C                   Z-ADD     *ZEROS        RISUL             4 0
037000000823     C***
037100000823     C* LANCIO
037200000823     C***
037300000000     C*--------------------
037400000000     C* ANNO/MESE INIZIALE
037500000000     C*--------------------
037600000000     C* OBBLIGATORIO
037700000000IF  1C     $ERR          IFEQ      '0'
037800000000IF  2C     V1CDAT        IFEQ      *ZEROS
037900000000     C                   MOVEL     '1'           $ERR
038000000000     C                   MOVEL     MSG(7)        $MSG
038100000000     C                   EXSR      CARMSG
038200000000     C                   SETON                                        60
038300000000E   2C                   ENDIF
038400000000E   1C                   ENDIF
038500000000     C* CORRETTO
038600000000IF  1C     $ERR          IFEQ      '0'
038700990609     C                   MOVEL     V1CDAT        G08INV
038800990609     C                   MOVE      01            G08INV
038900990609     C                   Z-ADD     *ZEROS        G08DAT
039000990609     C                   MOVEL     '3'           G08ERR
039100990609     C                   CALL      'XSRDA8'
039200990609     C                   PARM                    WLBDA8
039300990609IF  2C     G08ERR        IFEQ      '1'
039400000000     C                   MOVEL     '1'           $ERR
039500000000     C                   MOVEL     MSG(8)        $MSG
039600000000     C                   EXSR      CARMSG
039700000000     C                   SETON                                        60
039800000000E   2C                   ENDIF
039900000000E   1C                   ENDIF
040000000000     C*--------------------
040100000000     C* ANNO/MESE FINALE
040200000000     C*--------------------
040300000000     C* OBBLIGATORIO
040400000000IF  1C     V1CDA2        IFEQ      *ZEROS
040500000000     C                   MOVEL     '1'           $ERR
040600000000     C                   MOVEL     MSG(7)        $MSG
040700000000     C                   EXSR      CARMSG
040800000000     C                   SETON                                        61
040900000000E   1C                   ENDIF
041000000000     C* CORRETTO
041100000000IF  1C     $ERR          IFEQ      '0'
041200990609     C                   MOVEL     V1CDA2        G08INV
041300990609     C                   MOVE      01            G08INV
041400990609     C                   Z-ADD     *ZEROS        G08DAT
041500990609     C                   MOVEL     '3'           G08ERR
041600990609     C                   CALL      'XSRDA8'
041700990609     C                   PARM                    WLBDA8
041800990609IF  2C     G08ERR        IFEQ      '1'
041900000000     C                   MOVEL     '1'           $ERR
042000000000     C                   MOVEL     MSG(8)        $MSG
042100000000     C                   EXSR      CARMSG
042200000000     C                   SETON                                        61
042300000000E   2C                   ENDIF
042400000000E   1C                   ENDIF
042500000823     C*--------------------
042600000823     C* ANNO/MESE INIZIALE + FINALE
042700000823     C*--------------------
042800000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
042900000000IF  1C     $ERR          IFEQ      '0'
043000000000IF  2C     V1CDA2        IFLT      V1CDAT
043100000000     C                   MOVEL     '1'           $ERR
043200000000     C                   MOVEL     MSG(11)       $MSG
043300000000     C                   EXSR      CARMSG
043400000000     C                   SETON                                        61
043500000000E   2C                   ENDIF
043600000000E   1C                   ENDIF
043700000000     C*
043800000000     C* ANNO/MESE INIZIALE SUPERIORE/UGUALE A QUELLO CANCELLAZIONE + 1
043900000000IF  1C     $ERR          IFEQ      '0'
044000990609IF  2C     V1CDAT        IFLT      CANAM
044100000000     C                   MOVEL     '1'           $ERR
044200000000     C                   MOVEL     MSG(21)       $MSG
044300000000     C                   EXSR      CARMSG
044400000000     C                   SETON                                        60
044500000000E   2C                   ENDIF
044600000000E   1C                   ENDIF
044700000000     C*
044800000000     C* RICHIEDERE NON PIU' DI DODICI MESI
044900000000IF  1C     $ERR          IFEQ      '0'
045000000000     C     V1CDA2        SUB       V1CDAT        RISUL
045100000000IF  2C     RISUL         IFGE      100
045200000000     C                   MOVEL     '1'           $ERR
045300000000     C                   MOVEL     MSG(19)       $MSG
045400000000     C                   EXSR      CARMSG
045500000000     C                   SETON                                        6160
045600000000E   2C                   ENDIF
045700000000E   1C                   ENDIF
045800020325     C*
045900020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'ANNO)
046000020325IF  1C     $ERR          IFEQ      '0'
046100020325     C                   MOVEL     V1CDAT        ANNODA            4 0
046200020325     C                   MOVEL     V1CDA2        ANNOA             4 0
046300020325IF  2C     ANNODA        IFNE      ANNOA
046400020325     C                   MOVEL     '1'           $ERR
046500020325     C                   MOVEL     MSG(27)       $MSG
046600020325     C                   EXSR      CARMSG
046700020325     C                   SETON                                        6061
046800020325E   2C                   ENDIF
046900020325E   1C                   ENDIF
047000160712     C***
047100160712     C** Solo x BUDGET    Data UNificante di riferimento
047200160712     C***
047300160712     C                   MOVEL     *ZEROS        H1dtUNI           8 0
047400160712IF  1C     $ERR          IFEQ      '0'
047500160712     C     *in10         andeq     '1'
047600160712IF  2C     V1dtUNI       IFeq      *ZEROS
047700160712     C                   MOVEL     '1'           $ERR
047800160712     C                   MOVEL     MSG(39)       $MSG
047900160712     C                   EXSR      CARMSG
048000160712     C                   SETON                                        59
048100160712 2x  c                   else
048200160712     C                   MOVEL     V1dtUNI       G08DAT
048300160712     C                   MOVEL     *blank        G08ERR
048400160712     C                   CALL      'XSRDA8'
048500160712     C                   PARM                    WLBDA8
048600160712IF  3C     G08ERR        IFEQ      '1'
048700160712     C                   MOVEL     '1'           $ERR
048800160712     C                   MOVEL     MSG(39)       $MSG
048900160712     C                   EXSR      CARMSG
049000160712     C                   SETON                                        59
049100160712E   3C                   Else
049200160712     C                   MOVEL     G08DAT        V1dtUNI
049300160712     C                   MOVEL     G08INV        H1dtUNI
049400160712E   3C                   ENDIF
049500160712E   2C                   ENDIF
049600160712E   1C                   ENDIF
049700000823     C***
049800000823     C* SELEZIONI COMUNI
049900000823     C***
050000000823     C*--------------------
050100000823     C* % DELTA TOTALE PER STAMPA
050200000823     C*--------------------
050300000823IF  1C     $ERR          IFEQ      '0'
050400000823IF  2C     V1CPDD        IFNE      *ZEROS
050500000823     C     V1CPDA        ORNE      *ZEROS
050600000823IF  3C     V1CPDA        IFLT      V1CPDD
050700000823     C                   MOVEL     '1'           $ERR
050800000823     C                   MOVEL     MSG(34)       $MSG
050900000823     C                   EXSR      CARMSG
051000000823     C                   SETON                                        76
051100000823E   3C                   ENDIF
051200000823E   2C                   ENDIF
051300000823E   1C                   ENDIF
051400000823     C*--------------------
051500000823     C* % DELTA FASCE PER SEGNALAZIONE
051600000823     C*--------------------
051700000823IF  1C     $ERR          IFEQ      '0'
051800000823IF  2C     V1CSPD        IFNE      *ZEROS
051900000823IF  3C     V1CSPD        IFGT      100
052000000823     C                   MOVEL     '1'           $ERR
052100000823     C                   MOVEL     MSG(32)       $MSG
052200000823     C                   EXSR      CARMSG
052300000823     C                   SETON                                        74
052400000823E   3C                   ENDIF
052500000823E   2C                   ENDIF
052600000823E   1C                   ENDIF
052700020701     C*--------------------
052800020701     C* % SCOSTAMENTO FATTURATO ANNO PRECEDENTE
052900020701     C*--------------------
053000020701IF  1C     $ERR          IFEQ      '0'
053100020701IF  2C     V1CDFD        IFNE      *ZEROS
053200020701     C     V1CDFA        ORNE      *ZEROS
053300020701IF  3C     V1CDFD        IFGT      V1CDFA
053400020701     C                   MOVEL     '1'           $ERR
053500020701     C                   MOVEL     MSG(34)       $MSG
053600020701     C                   EXSR      CARMSG
053700020701     C                   SETON                                        47
053800020701E   3C                   ENDIF
053900020701E   2C                   ENDIF
054000020701E   1C                   ENDIF
054100020701     C*
054200020701IF  1C     $ERR          IFEQ      '0'
054300020701IF  2C     V1CDFDP       IFNE      *ZEROS
054400020701     C     V1CDFAP       ORNE      *ZEROS
054500020701IF  3C     V1CDFDP       IFGT      V1CDFAP
054600020701     C                   MOVEL     '1'           $ERR
054700020701     C                   MOVEL     MSG(34)       $MSG
054800020701     C                   EXSR      CARMSG
054900020701     C                   SETON                                        49
055000020701E   3C                   ENDIF
055100020701E   2C                   ENDIF
055200020701E   1C                   ENDIF
055300000823     C*--------------------
055400000823     C* TIPO SERVIZIO
055500000823     C*--------------------
055600000823     C                   MOVEL     *BLANKS       WTSP              1
055700000823     C                   MOVEL     *BLANKS       WDTSP            15
055800000823     C                   MOVEL     *BLANKS       V1DTSP
055900000823IF  1C     $ERR          IFEQ      '0'
056000000823IF  2C     V1CTSP        IFNE      *BLANKS
056100000823     C                   MOVEL     V1CTSP        WTSP
056200000823     C                   EXSR      CHKTSP
056300000823     C                   MOVEL     WTSP          V1CTSP
056400000823     C                   MOVEL     WDTSP         V1DTSP
056500000823IF  3C     $ERR          IFEQ      '1'
056600000823     C                   MOVEL     '1'           $ERR
056700000823     C                   MOVEL     MSG(28)       $MSG
056800000823     C                   EXSR      CARMSG
056900000823     C                   SETON                                        71
057000000823E   3C                   ENDIF
057100000823E   2C                   ENDIF
057200000823E   1C                   ENDIF
057300000823     C*--------------------
057400000823     C* DATI ITALIA / ESTERO
057500000823     C*--------------------
057600000823IF  1C     $ERR          IFEQ      '0'
057700000823IF  2C     V1CIOE        IFEQ      'I'
057800000823     C     V1CIOE        OREQ      'E'
057900100510     C     V1CIOE        OREQ      'T'
058000000823X   2C                   ELSE
058100160721IF  2C                   IF        V1CIOE = *blank and *in10
058200160721      **  solo per BUDGET  no errore (10=ON)
058300160721     c                   else
058400000823     C                   MOVEL     '1'           $ERR
058500000823     C                   MOVEL     MSG(29)       $MSG
058600000823     C                   EXSR      CARMSG
058700000823     C                   SETON                                        72
058800160721     c                   end
058900000823E   2C                   ENDIF
059000000823E   1C                   ENDIF
059100050228     C*--------------------
059200050228     C* Crea File PC : Non può crearlo se richiesto Italia+Estero
059300050228     C*--------------------
059400051107E   1C************   devono essere creati i dati nel WFDELPCF se richiesto Italia+Estero
059500051107IF  1C**** $ERR          IFEQ      '0'
059600051107IF  2C**** V1CIOE        IFEQ      *BLANKS
059700051107     C**** V1CFPC        andeq     'S'
059800051107     C****               MOVEL     '1'           $ERR
059900051107     C****               MOVEL     MSG(30)       $MSG
060000051107     C****               EXSR      CARMSG
060100051107     C****               SETON                                        73
060200051107E   2C****               ENDIF
060300051107E   1C****               ENDIF
060400051107E   1C************
060500000823     C*--------------------
060600000823     C* QUALITA' CLIENTE
060700000823     C*--------------------
060800010618     C* 1° categoria
060900000823IF  1C     $ERR          IFEQ      '0'
061000000823IF  2C     V1CCLV        IFNE      *BLANKS
061100000823     C                   MOVEL     V1CCLV        WCLV              1
061200000823     C                   EXSR      CHKCLV
061300000823     C                   MOVEL     WCLV          V1CCLV
061400000823IF  3C     $ERR          IFEQ      '1'
061500000823     C                   MOVEL     MSG(31)       $MSG
061600000823     C                   EXSR      CARMSG
061700010618     C                   SETON                                        50
061800000823E   3C                   ENDIF
061900000823E   2C                   ENDIF
062000000823E   1C                   ENDIF
062100010618     C* 2° categoria
062200010618IF  1C     $ERR          IFEQ      '0'
062300010618IF  2C     V1CCL1        IFNE      *BLANKS
062400010618     C                   MOVEL     V1CCL1        WCLV              1
062500010618     C                   EXSR      CHKCLV
062600010618     C                   MOVEL     WCLV          V1CCL1
062700010618IF  3C     $ERR          IFEQ      '1'
062800010618     C                   MOVEL     MSG(31)       $MSG
062900010618     C                   EXSR      CARMSG
063000010618     C                   SETON                                        51
063100010618E   3C                   ENDIF
063200010618E   2C                   ENDIF
063300010618E   1C                   ENDIF
063400010618     C* 3° categoria
063500010618IF  1C     $ERR          IFEQ      '0'
063600010618IF  2C     V1CCL2        IFNE      *BLANKS
063700010618     C                   MOVEL     V1CCL2        WCLV              1
063800010618     C                   EXSR      CHKCLV
063900010618     C                   MOVEL     WCLV          V1CCL2
064000010618IF  3C     $ERR          IFEQ      '1'
064100010618     C                   MOVEL     MSG(31)       $MSG
064200010618     C                   EXSR      CARMSG
064300010618     C                   SETON                                        52
064400010618E   3C                   ENDIF
064500010618E   2C                   ENDIF
064600010618E   1C                   ENDIF
064700020220     C* 4° categoria
064800020220IF  1C     $ERR          IFEQ      '0'
064900020220IF  2C     V1CCL3        IFNE      *BLANKS
065000020220     C                   MOVEL     V1CCL3        WCLV              1
065100020220     C                   EXSR      CHKCLV
065200020220     C                   MOVEL     WCLV          V1CCL3
065300020220IF  3C     $ERR          IFEQ      '1'
065400020220     C                   MOVEL     MSG(31)       $MSG
065500020220     C                   EXSR      CARMSG
065600020220     C                   SETON                                        53
065700020220E   3C                   ENDIF
065800020220E   2C                   ENDIF
065900020220E   1C                   ENDIF
066000020220     C* 5° categoria
066100020220IF  1C     $ERR          IFEQ      '0'
066200020220IF  2C     V1CCL4        IFNE      *BLANKS
066300020220     C                   MOVEL     V1CCL4        WCLV              1
066400020220     C                   EXSR      CHKCLV
066500020220     C                   MOVEL     WCLV          V1CCL4
066600020220IF  3C     $ERR          IFEQ      '1'
066700020220     C                   MOVEL     MSG(31)       $MSG
066800020220     C                   EXSR      CARMSG
066900020220     C                   SETON                                        54
067000020220E   3C                   ENDIF
067100020220E   2C                   ENDIF
067200020220E   1C                   ENDIF
067300000823     C*--------------------
067400000823     C* DATI POSTE
067500000823     C*--------------------
067600000823IF  1C     $ERR          IFEQ      '0'
067700000823IF  2C     V1CPOS        IFEQ      'S'
067800000823     C     V1CPOS        OREQ      'N'
067900000823X   2C                   ELSE
068000000823     C                   MOVEL     '1'           $ERR
068100000823     C                   MOVEL     MSG(35)       $MSG
068200000823     C                   EXSR      CARMSG
068300000823     C                   SETON                                        77
068400000823E   2C                   ENDIF
068500000823E   1C                   ENDIF
068600000823     C*--------------------
068700000823     C* DATI CLIENTE / UNIFICANTE
068800000823     C*--------------------
068900000823IF  1C     $ERR          IFEQ      '0'
069000000823IF  2C     V1CSCE        IFEQ      'C'
069100000823     C     V1CSCE        OREQ      'U'
069200000823X   2C                   ELSE
069300000823     C                   MOVEL     '1'           $ERR
069400000823     C                   MOVEL     MSG(35)       $MSG
069500000823     C                   EXSR      CARMSG
069600000823     C                   SETON                                        78
069700000823E   2C                   ENDIF
069800000823E   1C                   ENDIF
069900000823     C***
070000000823     C* STATISTICHE GRUPPO (A)
070100000823     C***
070200000823     C*--------------------
070300000823     C* DELTA DI FILIALE
070400000823     C*--------------------
070500000000IF  1C     $ERR          IFEQ      '0'
070600000000IF  2C     V1CDCK        IFEQ      'S'
070700000000     C     V1CDCK        OREQ      'N'
070800000000X   2C                   ELSE
070900000000     C                   MOVEL     '1'           $ERR
071000000000     C                   MOVEL     MSG(10)       $MSG
071100000000     C                   EXSR      CARMSG
071200000000     C                   SETON                                        62
071300000000E   2C                   ENDIF
071400000000E   1C                   ENDIF
071500050228     C*--------------------
071600050228     C* Crea File PC : Non può crearlo se NON richiesto Delta di Filiale
071700050228     C*--------------------
071800050228IF  1C     $ERR          IFEQ      '0'
071900050228IF  2C     V1CDCK        IFEQ      'N'
072000050228     C     V1CFPC        andeq     'S'
072100050228     C                   MOVEL     '1'           $ERR
072200050228     C                   MOVEL     MSG(38)       $MSG
072300050228     C                   EXSR      CARMSG
072400050228     C                   SETON                                        73
072500050228E   2C                   ENDIF
072600050228E   1C                   ENDIF
072700000823     C*--------------------
072800000823     C* DELTA DI AREA
072900000823     C*--------------------
073000000000IF  1C     $ERR          IFEQ      '0'
073100000000IF  2C     V1CDCC        IFEQ      'S'
073200000000     C     V1CDCC        OREQ      'N'
073300000000X   2C                   ELSE
073400000000     C                   MOVEL     '1'           $ERR
073500000000     C                   MOVEL     MSG(15)       $MSG
073600000000     C                   EXSR      CARMSG
073700000000     C                   SETON                                        63
073800000000E   2C                   ENDIF
073900000000E   1C                   ENDIF
074000000823     C*--------------------
074100000823     C* DELTA DI DIVISIONE
074200000823     C*--------------------
074300000000IF  1C     $ERR          IFEQ      '0'
074400000000IF  2C     V1CDCF        IFEQ      'S'
074500000000     C     V1CDCF        OREQ      'N'
074600000000X   2C                   ELSE
074700000000     C                   MOVEL     '1'           $ERR
074800000000     C                   MOVEL     MSG(16)       $MSG
074900000000     C                   EXSR      CARMSG
075000000000     C                   SETON                                        64
075100000000E   2C                   ENDIF
075200000000E   1C                   ENDIF
075300000823     C***
075400000823     C* SELEZIONI STATISTICHE GRUPPO (A)
075500000823     C***
075600000823     C*--------------------
075700000000     C* FILIALE
075800000823     C*--------------------
075900000000IF  1C     $ERR          IFEQ      '0'
076000000000IF  2C     V1CFIC        IFNE      *BLANKS                                      *SELEZIONATA
076100000000     C                   MOVEL     V1CFIC        W1CFIL            3
076200000000     C                   EXSR      CHKFIL                                       *CONTROLLO
076300000000     C                   MOVEL     W1CFIL        V1CFIC
076400000000IF  3C     $ERR          IFEQ      '1'
076500000000     C                   MOVEL     MSG(12)       $MSG
076600000000     C                   EXSR      CARMSG
076700000000     C                   SETON                                        65
076800000000E   3C                   ENDIF
076900000000X   2C                   ELSE
077000000000     C                   MOVEL     *ZEROS        W1CFIL                         *NON SELEZIONATA
077100000000E   2C                   ENDIF
077200000000E   1C                   ENDIF
077300000823     C*--------------------
077400000823     C* AREA
077500000823     C*--------------------
077600000000IF  1C     $ERR          IFEQ      '0'
077700000000IF  2C     V1CARC        IFNE      *BLANKS                                      *SELEZIONATA
077800000000     C                   MOVEL     V1CARC        W1CARE            3
077900000000     C                   EXSR      CHKARE                                       *CONTROLLO
078000000000     C                   MOVEL     W1CARE        V1CARC
078100000000IF  3C     $ERR          IFEQ      '1'
078200000000     C                   MOVEL     MSG(13)       $MSG
078300000000     C                   EXSR      CARMSG
078400000000     C                   SETON                                        66
078500000000E   3C                   ENDIF
078600000000X   2C                   ELSE                                                   *NON SELEZIONATA
078700000000     C                   MOVEL     *ZEROS        W1CARE
078800000000E   2C                   ENDIF
078900000000E   1C                   ENDIF
079000000823     C*--------------------
079100000000     C* DIVISIONE
079200000823     C*--------------------
079300000000IF  1C     $ERR          IFEQ      '0'
079400000000IF  2C     V1CDIC        IFNE      *BLANKS                                      *SELEZIONATA
079500000000     C                   MOVEL     V1CDIC        W1CDIV            1
079600000000     C                   EXSR      CHKDIV                                       *CONTROLLO
079700000000     C                   MOVEL     W1CDIV        V1CDIC
079800000000IF  3C     $ERR          IFEQ      '1'
079900000000     C                   MOVEL     MSG(14)       $MSG
080000000000     C                   EXSR      CARMSG
080100000000     C                   SETON                                        67
080200000000E   3C                   ENDIF
080300000000E   2C                   ENDIF
080400000000E   1C                   ENDIF
080500000824     C*--------------------
080600000824     C* SELEZIONI SOLO PER LE STATISTICHE SCELTE GRUPPO (A)
080700000824     C*--------------------
080800000824IF  1C     $ERR          IFEQ      '0'
080900000824IF  2C     V1CDCK        IFEQ      'N'
081000000824     C     V1CDCC        ANDEQ     'N'
081100000824     C     V1CDCF        ANDEQ     'N'
081200000824IF  3C     V1CFIC        IFGT      *BLANKS
081300000824     C     V1CARC        ORGT      *BLANKS
081400000824     C     V1CDIC        ORGT      *ZEROS
081500000824     C                   MOVEL     MSG(25)       $MSG
081600000824     C                   EXSR      CARMSG
081700000824     C                   SETON                                        626364
081800000824E   3C                   ENDIF
081900000824E   2C                   ENDIF
082000000824E   1C                   ENDIF
082100000824     C*--------------------
082200000824     C* SOLO UNA SELEZIONE STATISTICHE GRUPPO (A)
082300000824     C*--------------------
082400000824IF  1C     $ERR          IFEQ      '0'
082500000824IF  2C     V1CFIC        IFGT      *ZEROS
082600000824     C                   ADD       1             SELEZC
082700000824E   2C                   ENDIF
082800000824IF  2C     V1CARC        IFGT      *ZEROS
082900000824     C                   ADD       1             SELEZC
083000000824E   2C                   ENDIF
083100000824IF  2C     V1CDIC        IFNE      *BLANKS
083200000824     C                   ADD       1             SELEZC
083300000824E   2C                   ENDIF
083400000824IF  2C     SELEZC        IFGT      1
083500000824     C                   MOVEL     '1'           $ERR
083600000824     C                   MOVEL     MSG(17)       $MSG
083700000824     C                   EXSR      CARMSG
083800000824     C                   SETON                                        656667
083900000824E   2C                   ENDIF
084000000824E   1C                   ENDIF
084100050228     C*--------------------
084200050228     C* Crea File PC : WFTISE93F Non può crearlo se NON richiesto Delta di Divisione
084300050228     C*--------------------
084400050228IF  1C     $ERR          IFEQ      '0'
084500050228IF  2C     V1CDCF        IFEQ      'N'
084600050228     C     V1CWCP        andeq     'S'
084700050228     C                   MOVEL     '1'           $ERR
084800050228     C                   MOVEL     MSG(36)       $MSG
084900050228     C                   EXSR      CARMSG
085000050228     C                   SETON                                        79
085100050228E   2C                   ENDIF
085200050228E   1C                   ENDIF
085300000823     C***
085400000823     C* STATISTICHE GRUPPO (B)
085500000823     C***
085600000823     C*--------------------
085700000823     C* DELTA COMMERCIALE
085800000823     C*--------------------
085900000000IF  1C     $ERR          IFEQ      '0'
086000000000IF  2C     V1CDFF        IFEQ      'S'
086100000000     C     V1CDFF        OREQ      'N'
086200000000X   2C                   ELSE
086300000000     C                   MOVEL     '1'           $ERR
086400000000     C                   MOVEL     MSG(18)       $MSG
086500000000     C                   EXSR      CARMSG
086600000000     C                   SETON                                        68
086700000000E   2C                   ENDIF
086800000000E   1C                   ENDIF
086900000823     C***
087000000823     C* SELEZIONI STATISTICHE GRUPPO (B)
087100000823     C***
087200000823     C*--------------------
087300000000     C* RAGGRUPPAMENTO COMMERCIALE
087400000823     C*--------------------
087500000000IF  1C     $ERR          IFEQ      '0'
087600000000IF  2C     V1CCOF        IFNE      *BLANKS                                      *SELEZIONATO
087700000000     C                   MOVEL     V1CCOF        W1CCOM            7
087800000000     C                   EXSR      CHKCOM                                       *CONTROLLO
087900000000     C                   MOVEL     W1CCOM        V1CCOF
088000000000IF  3C     $ERR          IFEQ      '1'
088100000000     C                   EXSR      CARMSG
088200000000     C                   SETON                                        69
088300000000E   3C                   ENDIF
088400000000X   2C                   ELSE                                                   *NON SELEZIONATO
088500000000     C                   MOVEL     *ZEROS        W1CCOM
088600000000E   2C                   ENDIF
088700000000E   1C                   ENDIF
088800000823     C*--------------------
088900000000     C* DIVISIONE
089000000823     C*--------------------
089100000000IF  1C     $ERR          IFEQ      '0'
089200000000IF  2C     V1CDIF        IFNE      *BLANKS                                      *SELEZIONATA
089300000000     C                   MOVEL     V1CDIF        W1CDIV            1
089400000000     C                   EXSR      CHKDIV                                       *CONTROLLO
089500000000     C                   MOVEL     W1CDIV        V1CDIF
089600000000IF  3C     $ERR          IFEQ      '1'
089700000000     C                   MOVEL     MSG(14)       $MSG
089800000000     C                   EXSR      CARMSG
089900000000     C                   SETON                                        70
090000000000E   3C                   ENDIF
090100000000E   2C                   ENDIF
090200000000E   1C                   ENDIF
090300000824     C*--------------------
090400000824     C* SELEZIONI SOLO PER LE STATISTICHE SCELTE GRUPPO (B)
090500000824     C*--------------------
090600000824IF  1C     $ERR          IFEQ      '0'
090700000824IF  2C     V1CCOF        IFNE      *BLANKS
090800000824     C                   ADD       1             SELEZF
090900000824E   2C                   ENDIF
091000000824IF  2C     V1CDIF        IFNE      *BLANKS
091100000824     C                   ADD       1             SELEZF
091200000824E   2C                   ENDIF
091300000824IF  2C     SELEZF        IFGT      1
091400000824     C                   MOVEL     '1'           $ERR
091500000824     C                   MOVEL     MSG(23)       $MSG
091600000824     C                   EXSR      CARMSG
091700000824     C                   SETON                                        6970
091800000824E   2C                   ENDIF
091900000824E   1C                   ENDIF
092000000824     C*--------------------
092100000824     C* SOLO UNA SELEZIONE STATISTICHE GRUPPO (B)
092200000824     C*--------------------
092300000824IF  1C     $ERR          IFEQ      '0'
092400000824IF  2C     V1CDFF        IFEQ      'N'
092500000824IF  3C     V1CCOF        IFGT      *BLANKS
092600000824     C     V1CDIF        ORGT      *BLANKS
092700000824     C                   MOVEL     MSG(25)       $MSG
092800000824     C                   EXSR      CARMSG
092900000824     C                   SETON                                        68
093000000824E   3C                   ENDIF
093100000824E   2C                   ENDIF
093200000824E   1C                   ENDIF
093300050228     C*--------------------
093400050228     C* Crea File PC : WFTISE94F Non può crearlo se NON richiesto Delta Commerciale
093500050228     C*--------------------
093600050228IF  1C     $ERR          IFEQ      '0'
093700050228IF  2C     V1CDFF        IFEQ      'N'
093800050228     C     V1CWPC        andeq     'S'
093900050228     C                   MOVEL     '1'           $ERR
094000050228     C                   MOVEL     MSG(37)       $MSG
094100050228     C                   EXSR      CARMSG
094200050228     C                   SETON                                        80
094300050228E   2C                   ENDIF
094400050228E   1C                   ENDIF
094500000823     C***
094600000823     C* STATISTICHE GRUPPO (A) + GRUPPO (B)
094700000823     C***
094800000000     C*--------------------
094900000823     C* ALMENO UNA STATISTICA
095000000000     C*--------------------
095100000000IF  1C     $ERR          IFEQ      '0'
095200000000IF  2C     V1CDCK        IFEQ      'N'
095300000000     C     V1CDCC        ANDEQ     'N'
095400000000     C     V1CDCF        ANDEQ     'N'
095500000000     C     V1CDFF        ANDEQ     'N'
095600000000     C                   MOVEL     '1'           $ERR
095700000000     C                   MOVEL     MSG(24)       $MSG
095800000000     C                   EXSR      CARMSG
095900000000     C                   SETON                                        626364
096000000000     C                   SETON                                        68
096100000000E   2C                   ENDIF
096200000000E   1C                   ENDIF
096300000823     C*--------------------
096400000823     C* MESSAGGI SOLO DI ATTENZIONE
096500000823     C*--------------------
096600000823     C* I SALDI NON SONO AGGIORNATI CON LE RETTIFICHE E LE NOTE DI CREDITO
096700000823IF  1C     $ERR          IFEQ      '0'
096800000823IF  2C     WFSC          IFEQ      'N'                                          *SALDI NON AGGIORNAT
096900000823     C                   MOVEL     MSG(22)       $MSG
097000000823     C                   EXSR      CARMSG
097100000823E   2C                   ENDIF
097200000823E   1C                   ENDIF
097300000823     C*
097400000823     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO AGGIORNAMENTO - 1
097500000823IF  1C     $ERR          IFEQ      '0'
097600000823IF  2C     V1CDA2        IFGT      AGGAM
097700100510IF  2C     V1budget      andeq     'N'
097800000823     C                   MOVEL     MSG(9)        $MSG
097900000823     C                   EXSR      CARMSG
098000000823E   2C                   ENDIF
098100000823E   1C                   ENDIF
098200950418     C*
098300000000     C                   ENDSR
098400000000     C*------------------------------------------------------------------------*
098500000000     C*?CHKTSP - CONTROLLA IL TIPO SERVIZIO
098600000000     C*------------------------------------------------------------------------*
098700000000     C     CHKTSP        BEGSR
098800000000     C*
098900000000     C* CONTROLLO SE E' STATO INSERITO '?'
099000000000     C     '?'           SCAN      WTSP                                   99
099100000000     C*
099200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
099300000000IF  1C     *IN99         IFEQ      '1'
099400020916     C                   Z-ADD     1             §KUT
099500000000     C                   MOVEL     '5E'          §COD
099600000000     C                   MOVEL     *BLANKS       §KEY
099700000000     C                   MOVEL     *BLANKS       §DES
099800980403     C                   CALL      'X§TABER'
099900000000     C                   PARM                    §KUT              1 0
100000000000     C                   PARM                    §COD              2
100100000000     C                   PARM                    §KEY              8
100200000000     C                   PARM                    §DES             30
100300000000     C*
100400000000     C                   MOVEL     §KEY          WTSP
100500000000     C                   MOVEL     §DES          WDTSP
100600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
100700000000X   1C                   ELSE
100800000000     C*
100900000000     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
101000020916     C                   Z-ADD     1             KTBKUT
101100000000     C                   MOVEL     '5E'          KTBCOD
101200000000     C                   MOVEL     *BLANKS       KTBKEY
101300000000     C                   MOVEL     WTSP          KTBKEY
101400000000     C     KEYTAB        CHAIN     TABEL00F                           99
101500000000IF  2C     *IN99         IFEQ      '1'
101600000000     C     TBLFLG        OREQ      '*'
101700000000     C                   MOVEL     '1'           $ERR
101800000000     C                   ELSE
101900090610     C                   MOVEL     TBLUNI        ds5e
102000090610     C                   MOVEL     §5ED08        WDTSP
102100000000E   2C                   ENDIF
102200000000E   1C                   ENDIF
102300000000     C*
102400000000     C                   ENDSR
102500980521     C*------------------------------------------------------------------------*
102600980521     C*?CHKCLV - CONTROLLA LA QUALITA' CLIENTE
102700980521     C*------------------------------------------------------------------------*
102800980521     C     CHKCLV        BEGSR
102900980521     C*
103000980521     C* CONTROLLO SE E' STATO INSERITO '?'
103100980521     C     '?'           SCAN      WCLV                                   99
103200980521     C*
103300980521     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
103400980521IF  1C     *IN99         IFEQ      '1'
103500020916     C                   Z-ADD     1             §KUT
103600980521     C                   MOVEL     'IC'          §COD
103700980521     C                   MOVEL     *BLANKS       §KEY
103800980521     C                   MOVEL     *BLANKS       §DES
103900980521     C                   CALL      'X§TABER'
104000980521     C                   PARM                    §KUT              1 0
104100980521     C                   PARM                    §COD              2
104200980521     C                   PARM                    §KEY              8
104300980521     C                   PARM                    §DES             30
104400980521     C                   MOVEL     §KEY          WCLV
104500980521     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
104600980521X   1C                   ELSE
104700980521     C*
104800980521     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
104900020916     C                   Z-ADD     1             KTBKUT
105000980521     C                   MOVEL     'IC'          KTBCOD
105100980521     C                   MOVEL     *BLANKS       KTBKEY
105200980521     C                   MOVEL     WCLV          KTBKEY
105300980521     C     KEYTAB        CHAIN     TABEL00F                           99
105400980521IF  2C     *IN99         IFEQ      '1'
105500980521     C     TBLFLG        OREQ      '*'
105600980521     C                   MOVEL     '1'           $ERR
105700980521E   2C                   ENDIF
105800980521E   1C                   ENDIF
105900980521     C*
106000980521     C                   ENDSR
106100000000     C*------------------------------------------------------------------------*
106200000000     C*?CHKFIL - CONTROLLA LA FILIALE
106300000000     C*------------------------------------------------------------------------*
106400000000     C     CHKFIL        BEGSR
106500000000     C*
106600000000     C* CONTROLLO SE E' STATO INSERITO '?'
106700000000     C     '?'           SCAN      W1CFIL        A                 1 0    99
106800000000     C*
106900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE FILIALI
107000000000IF  1C     *IN99         IFEQ      '1'
107100000000     C                   MOVEL     *BLANKS       §COD1
107200000000     C                   MOVE      *BLANKS       §DES1
107300971203     C                   MOVE      *BLANKS       §PRA
107400971203     C                   CALL      'TNSD24R'
107500000000     C                   PARM                    §COD1             3
107600000000     C                   PARM                    §TIP              1
107700000000     C                   PARM                    §DES1            25
107800971203     C                   PARM                    §PRA              3
107900000000     C*
108000000000     C                   MOVEL     §COD1         W1CFIL            3
108100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
108200000000X   1C                   ELSE
108300000000     C*
108400000000     C* CARATTERI ALFABETICI NON VALIDI
108500000000IF  2C     W1CFIL        IFLT      *ZEROS
108600000000     C                   MOVEL     '1'           $ERR
108700000000X   2C                   ELSE
108800000000     C*
108900000000     C* CONTROLLO CHE LA FILIALE ESISTA IN ARCHIVIO
109000000000     C                   MOVEL     W1CFIL        KORFIL
109100000000     C     KEYORG        CHAIN     AZORG01L                           99
109200000000IF  3C     *IN99         IFEQ      '1'
109300000000     C     ORGFAG        OREQ      'V'
109400000000     C                   MOVEL     '1'           $ERR
109500000000E   3C                   ENDIF
109600000000E   2C                   ENDIF
109700000000E   1C                   ENDIF
109800000000     C*
109900000000     C                   ENDSR
110000000000     C*------------------------------------------------------------------------*
110100000000     C*?CHKARE - CONTROLLA L' AREA
110200000000     C*------------------------------------------------------------------------*
110300000000     C     CHKARE        BEGSR
110400000000     C*
110500000000     C* CONTROLLO SE E' STATO INSERITO '?'
110600000000     C     '?'           SCAN      W1CARE        A                 1 0    99
110700000000     C*
110800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE AREE
110900000000IF  1C     *IN99         IFEQ      '1'
111000020916     C                   Z-ADD     1             §KUT
111100000000     C                   MOVEL     '05'          §COD
111200000000     C                   MOVEL     *BLANKS       §KEY
111300000000     C                   MOVEL     *BLANKS       §DES
111400000000     C                   CALL      'X§TABER'
111500000000     C                   PARM                    §KUT              1 0
111600000000     C                   PARM                    §COD              2
111700000000     C                   PARM                    §KEY              8
111800000000     C                   PARM                    §DES             30
111900000000     C*
112000000000     C                   MOVEL     §KEY          W1CARE
112100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
112200000000X   1C                   ELSE
112300000000     C*
112400000000     C* CARATTERI ALFABETICI NON VALIDI
112500000000IF  2C     W1CARE        IFLT      *ZEROS
112600000000     C                   MOVEL     '1'           $ERR
112700000000X   2C                   ELSE
112800000000     C*
112900000000     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
113000020916     C                   Z-ADD     1             KTBKUT
113100000000     C                   MOVEL     '05'          KTBCOD
113200000000     C                   MOVEL     *BLANKS       KTBKEY
113300000000     C                   MOVEL     W1CARE        KTBKEY
113400000000     C     KEYTAB        CHAIN     TABEL00F                           99
113500000000IF  3C     *IN99         IFEQ      '1'
113600000000     C     TBLFLG        OREQ      '*'
113700000000     C                   MOVEL     '1'           $ERR
113800000000E   3C                   ENDIF
113900000000E   2C                   ENDIF
114000000000E   1C                   ENDIF
114100000000     C*
114200000000     C                   ENDSR
114300000000     C*------------------------------------------------------------------------*
114400000000     C*?CHKDIV - CONTROLLA LA DIVISIONE
114500000000     C*------------------------------------------------------------------------*
114600000000     C     CHKDIV        BEGSR
114700000000     C*
114800000000     C* CONTROLLO SE E' STATO INSERITO '?'
114900000000     C     '?'           SCAN      W1CDIV                                 99
115000000000     C*
115100000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE DIVISIONI
115200000000IF  1C     *IN99         IFEQ      *ON
115300020916     C                   Z-ADD     1             §KUT
115400000000     C                   MOVEL     '17'          §COD
115500000000     C                   MOVEL     *BLANKS       §KEY
115600000000     C                   MOVEL     *BLANKS       §DES
115700980403     C                   CALL      'X§TABER'
115800000000     C                   PARM                    §KUT
115900000000     C                   PARM                    §COD
116000000000     C                   PARM                    §KEY
116100000000     C                   PARM                    §DES
116200000000     C*
116300000000     C                   MOVEL     §KEY          W1CDIV
116400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
116500000000X   1C                   ELSE
116600000000     C*
116700000000     C* CONTROLLO CHE LA DIVISIONE ESISTA IN TABELLA
116800020916     C                   Z-ADD     1             KTBKUT
116900000000     C                   MOVEL     '17'          KTBCOD
117000000000     C                   MOVEL     *BLANKS       KTBKEY
117100000000     C                   MOVEL     W1CDIV        KTBKEY
117200000000     C     KEYTAB        CHAIN     TABEL00F                           99
117300000000IF  2C     *IN99         IFEQ      *ON
117400000000     C     TBLFLG        OREQ      '*'
117500000000     C                   MOVEL     '1'           $ERR
117600000000E   2C                   ENDIF
117700000000E   1C                   ENDIF
117800000000     C*
117900000000     C                   ENDSR
118000000000     C*------------------------------------------------------------------------*
118100000000     C*?CHKCOM - CONTROLLO IL CODICE COMMERCIALE E SUO RAGGRUPPAMENTO
118200000000     C*------------------------------------------------------------------------*
118300000000     C     CHKCOM        BEGSR
118400000000     C*
118500000000     C* CONTROLLO SE E' STATO INSERITO '?'
118600000000     C     '?'           SCAN      W1CCOM                                 99
118700000000     C*
118800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE COMMERCIALI
118900000000IF  1C     *IN99         IFEQ      *ON
119000140612     C                   CLEAR                   TRMK43ds
119100140612     C                   CALL      'TRMK43R'
119200140612     C                   PARM                    KPJBA
119300140612     C                   PARM                    TRMK43ds
119400140612     c                   if        oMK43err = *off  and
119500140612     c                             oMK43fxx = *blank
119600140612     C                   MOVEL     oMK43cmm      W1CCOM
119700140612     c                   endif
119800140612     C*
119900140612     C*********          Z-ADD     1             §KUT
120000140612     C*********          MOVEL     '01'          §COD
120100140612     C*********          MOVEL     *BLANKS       §KEY
120200140612     C*********          MOVEL     *BLANKS       §DES
120300140612     C*********          CALL      'X§TABER'
120400140612     C*********          PARM                    §KUT
120500140612     C*********          PARM                    §COD
120600140612     C*********          PARM                    §KEY
120700140612     C*********          PARM                    §DES
120800140612     C*********          MOVEL     §KEY          W1CCOM
120900140612     C*********
121000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
121100140612     C*********
121200000000X   1C                   ELSE
121300000000     C*
121400000000     C* CARATTERI ALFABETICI NON VALIDI
121500000000IF  2C     W1CCOM        IFLT      *ZEROS
121600000000     C                   MOVEL     '1'           $ERR
121700000000     C                   MOVEL     MSG(20)       $MSG
121800000000X   2C                   ELSE
121900000000     C*
122000000000     C* CONTROLLO CHE IL COMMERCIALE ESISTA IN TABELLA
122100140612     C*********          Z-ADD     1             KTBKUT
122200140612     C*********          MOVEL     '01'          KTBCOD
122300140612     C*********          MOVEL     *BLANKS       KTBKEY
122400140612     C*********          MOVEL     W1CCOM        KTBKEY
122500140612     C***  KEYTAB        CHAIN     TABEL00F                           99
122600140612IF  3C***  *IN99         IFEQ      *ON
122700140612     C**** TBLFLG        OREQ      '*'
122800140612     C*********          MOVEL     '1'           $ERR
122900140612     C*********          MOVEL     MSG(20)       $MSG
123000140612X   3C*********          ELSE
123100140612     C*********
123200000000     C* CONTROLLO CHE IL CODICE SIA ANCHE DI RAGGRUPPAMENTO
123300140612     C*********          MOVEL     TBLUNI        DS01
123400140612     C*********          MOVEL     §01RGF        ALF7              7
123500140612IF  4C**** ALF7          IFNE      TBLKEY
123600140612     C*********          MOVEL     '1'           $ERR
123700140612     C*********          MOVEL     MSG(26)       $MSG
123800140612E   4C*********          ENDIF
123900140612E   3C*********          ENDIF
124000140612     C*********
124100140612     C*   ora è sul file e non più sulle tabelle
124200140612     C*********
124300140612     C                   MOVE      W1CCOM        code7n            7 0
124400140612     C     code7n        chain     azCMM01l                           99
124500140612IF  3C                   If        not %Found(azCMM01l) or
124600140612     C                                cmmATB <> *blank
124700140612     C                   MOVEL     '1'           $ERR
124800140612     C                   MOVEL     MSG(20)       $MSG
124900140612X   3C                   ELSE
125000140612     C*
125100140612     C* CONTROLLO CHE IL CODICE SIA ANCHE DI RAGGRUPPAMENTO
125200140612IF  4C     cmmCOD        IFNE      cmmUNI
125300140612     C                   MOVEL     '1'           $ERR
125400140612     C                   MOVEL     MSG(26)       $MSG
125500140612E   4C                   ENDIF
125600140612E   3C                   ENDIF
125700140612     C*
125800000000E   2C                   ENDIF
125900000000E   1C                   ENDIF
126000000000     C*
126100000000     C                   ENDSR
126200000000     C*------------------------------------------------------------------------*
126300000000     C*?IMPVID - IMPOSTAZIONI INIZIALI VIDEO
126400000000     C*------------------------------------------------------------------------*
126500000000     C     IMPVID        BEGSR
126600970825     C*
126700010410     C     *LIKE         DEFINE    §5SFSC        WFSC
126800980403     C     *LIKE         DEFINE    §5SDPS        WSPD
126900020701     C     *LIKE         DEFINE    §5SDFD        WDFD
127000020701     C     *LIKE         DEFINE    §5SDFA        WDFA
127100020701     C     *LIKE         DEFINE    §5SDFDP       WDFDP
127200020701     C     *LIKE         DEFINE    §5SDFAP       WDFAP
127300980129     C*
127400160721     C*   Sistema INFO  BUDGET
127500160721     c                   setoff                                       10
127600160721     c                   if        %subst(KNSIF:1:9) = 'GAITRABDG'
127700160721     c                   seton                                        10
127800160721     c                   end
127900160721     C* oppure
128000100510     C*  controlla che il programma sia lanciato per gestire il budget
128100100510     C*   nella lista di libreria si deve trovare una libreria BUDGET*
128200100510     C                   MOVEL     'N'           v1budget          1            *GESTIONE ATTUALE
128300100511     c                   call      'TISE91C0'
128400100510     c                   parm                    lib_budget        1
128500100510     c                   parm                    manca_file        1
128600100510     C*  se trovata la libreria Budget ritorna con "S".
128700100510     c                   if        lib_budget = 'S'
128800100510     C                   MOVEL     'S'           v1budget          1            *GESTIONE ATTUALE
128900160712     c                   seton                                        10
129000100510     c                   end
129100020528     C*---
129200020916     C*---               Z-ADD     1             KTBKUT
129300020528     C*---               MOVEL     '2U'          KTBCOD
129400020528     C*--- KEYTA2        CHAIN     TABEL00F                           99
129500020528IF  1C*--- *IN99         IFEQ      *ON                                          *TABELLA NON TROVATA
129600020528     C*---               Z-ADD     99999999      WDCC
129700020528X   1C*---               ELSE
129800020528     C*---               MOVEL     TBLUNI        DS2U
129900020528     C*---               Z-ADD     §2UDCC        WDCC                           *ULTIMA CANCELLAZION
130000020528E   1C*---               ENDIF
130100980403     C*
130200020916     C                   Z-ADD     1             KTBKUT
130300980403     C                   MOVEL     '5S'          KTBCOD
130400980403     C     KEYTA2        CHAIN     TABEL00F                           99
130500980403IF  1C     *IN99         IFEQ      *ON                                          *TABELLA NON TROVATA
130600010410     C                   MOVEL     'N'           WFSC                           *NON PERMETTO LANCIO
130700980403     C                   Z-ADD     *ZEROS        WSPD                           *% SCOSTAMENTO DELTA
130800020701     C                   Z-ADD     *ZEROS        WDFD                           *% SCOSTAMENTO FATTURATO DA MESE
130900020701     C                   Z-ADD     *ZEROS        WDFA                           *% SCOSTAMENTO FATTURATO  A MESE
131000020701     C                   Z-ADD     *ZEROS        WDFDP                          *% SCOSTAMENTO FATTURATO DA PROG
131100020701     C                   Z-ADD     *ZEROS        WDFAP                          *% SCOSTAMENTO FATTURATO  A PROG
131200020528     C                   Z-ADD     99999999      WDCC
131300980403X   1C                   ELSE
131400980403     C                   MOVEL     TBLUNI        DS5S
131500980403     C                   Z-ADD     §5SDPS        WSPD                           *% SCOSTAMENTO DELTA
131600010410     C                   Z-ADD     §5SDAC        WDAC                           *ULTIMO AGGIORNAMENT
131700010410     C                   MOVEL     §5SFSC        WFSC                           *FLAG AGGIORNAMENTO
131800020528     C                   Z-ADD     §5SDCC        WDCC                           *ULTIMA CANCELLAZION
131900020701     C                   Z-ADD     §5SDFD        WDFD                           *% SCOSTAMENTO FATTURATO DA
132000020701     C                   Z-ADD     §5SDFA        WDFA                           *% SCOSTAMENTO FATTURATO  A
132100020701     C                   Z-ADD     §5SDFDP       WDFDP                          *% SCOSTAMENTO FATTURATO DA PROG
132200020701     C                   Z-ADD     §5SDFAP       WDFAP                          *% SCOSTAMENTO FATTURATO  A PROG
132300980403E   1C                   ENDIF
132400990609     C*---
132500950210     C* DATA ULTIMO AGGIORNAMENTO - 1 MESE
132600990609     C*---
132700950210     C                   SUB       1             AGGMM
132800950210IF  1C     AGGMM         IFEQ      *ZEROS                                       *SE GENNAIO
132900950210     C                   Z-ADD     12            AGGMM                          *IMPOSTO DICEMBRE
133000950210     C                   SUB       1             AGGAA                          *DELL'ANNO PRECEDENT
133100950210E   1C                   ENDIF
133200990609     C*---
133300950210     C* DATA ULTIMA CANCELLAZIONE + 1 MESE
133400990609     C*---
133500950210     C                   ADD       1             CANMM
133600950210IF  1C     CANMM         IFEQ      13                                           *SE DICEMBRE
133700950210     C                   Z-ADD     01            CANMM                          *IMPOSTO GENNAIO
133800950210     C                   ADD       1             CANAA                          *DELL'ANNO SUCESSIVO
133900950210E   1C                   ENDIF
134000000000     C*--------------------
134100000000     C* DATE DI ELABORAZIONE
134200000000     C*--------------------
134300950210     C***
134400950210     C* AA/MM FINALE   --> ULTIMO AGGIORNAMENTO - 1 MESE
134500950210     C* AA/MM INIZIALE --> INIZIO ANNO oppure DATA ULTIMA CANCELLAZIONE + 1 MESE
134600950210     C***
134700000824     C*--------------------
134800000824     C* LANCIO
134900000824     C*--------------------
135000000824     C                   Z-ADD     AGGAM         V1CDA2                         *DATA FINE
135100990609     C                   Z-ADD     AGGAM         AAMM1             6 0          *INIZIO ANNO
135200990609     C                   MOVE      01            AAMM1
135300990609     C                   Z-ADD     CANAM         AAMM2             6 0          *ULTIMA CANCELLAZION
135400990609IF  1C     AAMM2         IFGT      AAMM1
135500990609     C                   Z-ADD     CANAM         V1CDAT
135600990609     C                   ELSE
135700000824     C                   Z-ADD     AAMM1         V1CDAT                         *DATA INIZIO
135800990609E   1C                   ENDIF
135900100510      *
136000100510      * Se elabora il Budget imposta l'anno successivo
136100100510     c                   if        lib_budget = 'S'
136200100510     C                   add       100           V1CDAT
136300100510     C                   add       100           V1CDA2
136400100510     c                   end
136500100510      *
136600030311     C                   MOVEL     'S'           V1CFPC                         *CREAZIONE FILE PC
136700030912     C                   MOVEL     'S'           V1CWPC                         *CREAZIONE FILE PC del TISE94R
136800030923     C                   MOVEL     'S'           V1CWCP                         *CREAZIONE FILE PC del TISE93R
136900980527     C*--------------------
137000000824     C* SELEZIONI COMUNI
137100980527     C*--------------------
137200980527     C                   Z-ADD     -999,9        V1CPDD
137300980527     C                   Z-ADD     +999,9        V1CPDA
137400020701     C                   IF        §5SDSS = '-'
137500020701     C                   EVAL      V1CSPD = WSPD * -1
137600020701     C                   ENDIF
137700020701     C                   IF        §5SDDS = '-'
137800020701     C                   EVAL      V1CDFD = WDFD * -1
137900020701     C                   ENDIF
138000020701     C                   IF        §5SDAS = '-'
138100020701     C                   EVAL      V1CDFA = WDFA * -1
138200020701     C                   ENDIF
138300020701     C                   IF        §5SDDSP = '-'
138400020701     C                   EVAL      V1CDFDP = WDFDP * -1
138500020701     C                   ENDIF
138600020701     C                   IF        §5SDASP = '-'
138700020701     C                   EVAL      V1CDFAP = WDFAP * -1
138800020701     C                   ENDIF
138900100510     C                   MOVEL     'T'           V1CIOE                         DELTA ITALIA/ESTERO
139000160721      * x Budget si vuole il lancio accorpato I+E
139100160721     C   10              MOVEL     *blank        V1CIOE                         DELTA ITALIA/ESTERO
139200011204     C                   MOVEL     'U'           V1CSCE                         DETTAGLIO X CLIENTE
139300000824     C                   MOVEL     'N'           V1CPOS
139400000000     C*--------------------
139500000824     C* SELEZIONI STATISTICHE
139600000000     C*--------------------
139700000000     C                   MOVEL     'S'           V1CDCK                         DELTA COMM.LE CLIENT
139800000000     C                   MOVEL     'S'           V1CDCC                         DELTA COMM.LE COMM.L
139900000000     C                   MOVEL     'S'           V1CDCF                         DELTA COMM.LE FILIAL
140000000000     C                   MOVEL     'S'           V1CDFF                         DELTA FILIALE
140100010618     C                   MOVEL     'N'           V1CSIN                         SINTETICA
140200020404     C                   MOVEL     'N'           V1CNTW                         ROTTURA NETWORK ESTERI
140300100510     C*
140400000000     C                   ENDSR
140500951220     C*------------------------------------------------------------------------*
140600951220     C*?X12PRE - CALCOLA 12 MESI PRECEDENTI ALLA DATA
140700951220     C*------------------------------------------------------------------------*
140800951220     C     X12PRE        BEGSR
140900951220     C*
141000951220     C*--------------------
141100951220     C* DATA FINALE PRECEDENTE = DATA INIZIALE RICHIESTA - 1 MESE
141200951220     C*--------------------
141300951220     C                   Z-ADD     PARAAI        ANPAAF                         *ANNO FINALE
141400951220     C     PARMMI        SUB       1             ANPMMF                         *MESE FINALE
141500951220IF  1C     ANPMMF        IFEQ      *ZEROS                                       *SE ERA GENNAIO
141600951220     C                   Z-ADD     12            ANPMMF                         *IMPOSTO DICEMBRE
141700951220     C     PARAAI        SUB       1             ANPAAF                         *DELL'ANNO PRECEDENT
141800951220E   1C                   ENDIF
141900951220     C*--------------------
142000951220     C* DATA INIZIALE PRECEDENTE = DATA FINALE PRECEDENTE - 12 MESI
142100951220     C*--------------------
142200990609     C                   Z-ADD     ANPAAF        WAAF              4 0
142300951220     C                   Z-ADD     ANPMMF        WMMF              2 0
142400951220     C                   Z-ADD     *ZEROS        J                 4 0
142500951220     C                   Z-ADD     WAAF          ANPAAI                         *ANNO INIZIALE
142600951220DO  1C     WMMF          DOUEQ     *ZEROS
142700951220     C                   ADD       1             J
142800951220     C                   Z-ADD     WMMF          ANPMMI                         *MESE INIZIALE
142900951220     C                   SUB       1             WMMF
143000951220E   1C                   ENDDO
143100951220IF  1C     J             IFLT      12
143200951220     C                   Z-ADD     12            WMMF
143300951220     C                   SUB       1             WAAF
143400951220     C                   Z-ADD     WAAF          ANPAAI                         *ANNO INIZIALE
143500951220DO  2C     J             DOUEQ     12
143600951220     C                   ADD       1             J
143700951220     C                   Z-ADD     WMMF          ANPMMI                         *MESE INIZIALE
143800951220     C                   SUB       1             WMMF
143900951220E   2C                   ENDDO
144000951220E   1C                   ENDIF
144100951220     C*
144200951220     C                   ENDSR
144300020916     C*--------------------------------------------------------------------------------------------*
144400020916     C* REPERISCE I DATI UTENTE
144500020916     C*--------------------------------------------------------------------------------------------*
144600020916     C     REPDATIUTE    BEGSR
144700020916     C*
144800020916     C* INIZIALIZZA VARIABILI DI WRK
144900020916     C                   CLEAR                   TIBS34DS
145000020916     C                   CLEAR                   AZUTEDS
145100020916     C                   CLEAR                   DDATIUTE
145200020916     C*
145300020916     C     *DTAARA       DEFINE    §azute        azuteds
145400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
145500020916     C                   IN(E)     *DTAARA
145600020916if  1C                   IF        %Error
145700020916     c                   EVAL      I34Tla = 'L'
145800020916     C                   CALL      'TIBS34R'
145900020916     C                   PARM                    Tibs34Ds
146000020916     C                   IN        *DTAARA
146100020916e   1C                   ENDIF
146200020916     C*
146300020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
146400020916     C                   MOVEL(P)  RSUT          DSFIRS           20
146500020916     C*
146600020916     C                   ENDSR
146700000000     C*------------------------------------------------------------------------*
146800000000     C*?*INZSR - ROUTINE INIZIALE
146900000000     C*------------------------------------------------------------------------*
147000000000     C     *INZSR        BEGSR
147100000000     C*
147200000000     C* RICEVIMENTO PARAMETRI
147300000000     C     *ENTRY        PLIST
147400000000     C                   PARM                    KPJBA
147500000000     C*
147600000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
147700000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
147800000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
147900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
148000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
148100000000     C*
148200000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
148300000000     C     KEYORG        KLIST
148400000000     C                   KFLD                    KORFIL
148500000000     C*
148600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
148700000000     C     KEYTAB        KLIST
148800000000     C                   KFLD                    KTBKUT
148900000000     C                   KFLD                    KTBCOD
149000000000     C                   KFLD                    KTBKEY
149100000000     C*
149200000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
149300000000     C     KEYTA2        KLIST
149400000000     C                   KFLD                    KTBKUT
149500000000     C                   KFLD                    KTBCOD
149600000000     C*
149700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
149800000000     C                   EXSR      INZSTD
149900000000     C*
150000000000     C* IMPOSTAZIONI INIZIALI VIDEO
150100000000     C                   EXSR      IMPVID
150200000000     C*
150300000000     C* EMETTO PRIMO FORMATO VIDEO
150400000000     C                   EXSR      CARV1
150500000000     C*
150600000000     C                   ENDSR
150700950418     C*------------------------------------------------------------------------*
150800950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
150900950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151000950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151100000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
151200000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
151300000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
151400950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151500950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151600950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
151700000000     C*------------------------------------------------------------------------*
151800000000     C* INZSTD - ROUTINE INIZIALE STANDARD
151900000000     C*------------------------------------------------------------------------*
152000000000     C     INZSTD        BEGSR
152100000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
152200000000     C                   CALL      'XKEYS'
152300000000     C                   PARM                    DSKEYS
152400000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
152500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
152600000000     C     XMSGPA        PLIST
152700000000     C                   PARM                    DSMSMS
152800000000     C                   PARM                    DSMSPG
152900000000     C                   PARM                    DSMSKE
153000000000     C                   PARM                    DSMSER
153100000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
153200020116     C                   MOVEL     'TISE90R'     DSMSPG                         *NOME PGM CHIAMANTE
153300020116     C                   MOVEL     'TISE90R'     VMCQUE                         *CODA MESSAGGI DESTI
153400000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
153500000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
153600000000     C* INIZIALIZZO FLAG CONTROLLO PGM
153700000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
153800000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
153900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
154000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
154100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
154200000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
154300000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
154400000000     C* LANCIO ROUTINE PER VERIFICARE SE
154500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
154600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
154700020916     C                   EXSR      REPDATIUTE
154800000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
154900000000     C                   BITOFF    '01234567'    POSMI1            1
155000000000     C                   BITOFF    '01234567'    POSMI2            1
155100000000     C                   BITOFF    '01234567'    POSMA1            1
155200000000     C                   BITOFF    '01234567'    POSMA2            1
155300000000     C                   BITON     '34'          POSMI1            1            *18
155400000000     C                   BITON     '7'           POSMI2            1            *01
155500000000     C                   BITON     '34'          POSMA1            1            *18
155600000000     C                   BITON     '0'           POSMA2            1            *80
155700000000     C                   MOVE      POSMI2        POSMIN            2
155800000000     C                   MOVEL     POSMI1        POSMIN
155900000000     C                   MOVE      POSMA2        POSMAX            2
156000000000     C                   MOVEL     POSMA1        POSMAX
156100000000     C                   ENDSR
156200000000     C*------------------------------------------------------------------------*
156300000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
156400000000     C*------------------------------------------------------------------------*
156500000000     C     FUNSTD        BEGSR
156600000000     C                   SELECT
156700000000     C     KEY           WHENEQ    F3
156800000000     C                   EXSR      F03STD
156900000000     C     KEY           WHENEQ    F12
157000000000     C                   EXSR      F12STD
157100000000     C     KEY           WHENEQ    F24
157200000000     C                   EXSR      F24STD
157300000000     C                   OTHER
157400000000     C                   MOVEL     MSG(2)        $MSG
157500000000     C                   EXSR      CARMSG
157600000000     C                   ENDSL
157700000000     C                   ENDSR
157800000000     C*------------------------------------------------------------------------*
157900000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
158000000000     C*------------------------------------------------------------------------*
158100000000     C     F03STD        BEGSR
158200000000     C*
158300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
158400000000     C                   MOVEL     '1'           $FINE
158500000000     C*
158600000000     C                   ENDSR
158700000000     C*------------------------------------------------------------------------*
158800000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
158900000000     C*------------------------------------------------------------------------*
159000000000     C     F12STD        BEGSR
159100000000     C*
159200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
159300000000     C                   MOVEL     '1'           $FINE
159400000000     C*
159500000000     C                   ENDSR
159600000000     C*------------------------------------------------------------------------*
159700000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
159800000000     C*------------------------------------------------------------------------*
159900000000     C     F24STD        BEGSR
160000000000     C*
160100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
160200000000     C                   EXSR      OUTFUN
160300000000     C*
160400000000     C                   ENDSR
160500000000     C*------------------------------------------------------------------------*
160600000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
160700000000     C*          CODA MESSAGGI DEL PGM
160800000000     C*------------------------------------------------------------------------*
160900000000     C     CARMSG        BEGSR
161000000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
161100000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
161200000000     C     MSGULT        IFEQ      0
161300000000     C                   MOVEL     '1'           *IN40
161400020116     C                   WRITE     SE90DVK
161500000000     C                   MOVEL     '0'           *IN40
161600000000     C                   ENDIF
161700000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
161800000000     C                   MOVEL     $MSG          DSMSMS
161900000000     C                   CALL      'XMSG'        XMSGPA
162000000000     C     DSMSER        IFEQ      0
162100000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
162200000000     C                   ENDIF
162300000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
162400000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
162500020116     C                   WRITE     SE90DVM                                      *SFL MESS
162600000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
162700000000     C                   MOVEL     *BLANKS       $MSG
162800000000     C                   ENDSR
162900000000     C*------------------------------------------------------------------------*
163000000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
163100000000     C*------------------------------------------------------------------------*
163200000000     C     OUTMSG        BEGSR
163300000000     C* VISUALIZZAZIONE MESSAGGIO
163400000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
163500020116     C                   WRITE     SE90DVK                                      *CONTR MESS
163600000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
163700000000     C                   Z-ADD     *ZEROS        MSGULT
163800000000     C                   ENDSR
163900000000     C*------------------------------------------------------------------------*
164000000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
164100000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
164200000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
164300000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
164400000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
164500000000     C*------------------------------------------------------------------------*
164600000000     C     CARFUN        BEGSR
164700000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
164800000000     C     1             DO        31            I                 5 0          --- 1 -->
164900000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
165000000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
165100000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
165200020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
165300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
165400000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
165500000000     C                   ELSE                                                   *INTERROG.
165600000000     C                   MOVEL     FUNASI        FAT(I)
165700000000     C                   ENDIF
165800000000     C                   ELSE                                                   <-- 2 --> *FILIALE
165900000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
166000000000     C                   MOVEL     FUNAFG        FAT(I)
166100000000     C                   ELSE                                                   *INTERROG.
166200000000     C                   MOVEL     FUNAFI        FAT(I)
166300000000     C                   ENDIF
166400000000     C                   ENDIF                                                  <-- 2 ---
166500000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
166600000000     C                   MOVEL     USER          WEDP              3            *PROFILO
166700000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
166800000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
166900000000     C                   ELSE
167000000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
167100000000     C                   ENDIF
167200000000     C                   ENDIF
167300000000     C                   ENDDO                                                  <-- 1 ---
167400000000     C                   ENDSR
167500000000     C*------------------------------------------------------------------------*
167600000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
167700000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
167800000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
167900000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
168000000000     C*------------------------------------------------------------------------*
168100000000     C     OUTFUN        BEGSR
168200000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
168300000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
168400000000     C                   ENDDO
168500000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
168600000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
168700000000     C     1             DO        31            I
168800000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
168900000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
169000000000     C                   ADD       1             FUNNAT
169100000000     C                   ENDIF
169200000000     C                   ENDDO
169300000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
169400000000     C     FUNNAT        IFGT      8
169500000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
169600000000     C                   ENDIF
169700000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
169800000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
169900000000     C                   Z-ADD     8             FUNVIS            5 0
170000000000     C     FAT(23)       IFEQ      'S'
170100000000     C                   SUB       1             FUNVIS
170200000000     C                   ENDIF
170300000000     C     FAT(24)       IFEQ      'S'
170400000000     C                   SUB       1             FUNVIS
170500000000     C                   ENDIF
170600000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
170700000000     C                   Z-ADD     *ZEROS        W                 5 0
170800000000     C                   ADD       1             FUNULT
170900000000     C     FUNULT        IFGT      31
171000000000     C                   Z-ADD     1             FUNULT
171100000000     C                   ENDIF
171200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
171300000000     C     FUNULT        DO        31            I                              --- 1 -->
171400000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
171500000000     C     W             IFLT      FUNVIS                                       --- 2 -->
171600000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
171700000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
171800000000     C     FDE(I)        ANDNE     *BLANKS
171900000000     C     I             IFEQ      23                                           *TOLGO F23
172000000000     C     I             OREQ      24                                           *E F24
172100000000     C                   ELSE
172200000000     C                   ADD       1             W
172300000000     C                   MOVEL     FDE(I)        FUV(W)
172400000000     C                   ENDIF
172500000000     C                   ENDIF                                                  <-- 3 ---
172600000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
172700000000     C                   Z-ADD     I             FUNULT
172800000000     C                   ENDIF                                                  <-- 2 ---
172900000000     C                   ENDDO                                                  <-- 1 ---
173000000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
173100000000     C                   MOVEL     FUV(1)        VFD001
173200000000     C                   MOVEL     FUV(2)        VFD002
173300000000     C                   MOVEL     FUV(3)        VFD003
173400000000     C                   MOVEL     FUV(4)        VFD004
173500000000     C                   MOVEL     FUV(5)        VFD005
173600000000     C                   MOVEL     FUV(6)        VFD006
173700000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
173800000000     C     FAT(24)       ANDEQ     'N'
173900000000     C                   MOVEL     FUV(7)        VFD007
174000000000     C                   MOVEL     FUV(8)        VFD008
174100000000     C                   ELSE                                                   <-- 1 -->
174200000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
174300000000     C     FAT(24)       ANDEQ     'S'
174400000000     C                   MOVEL     FDE(23)       VFD007
174500000000     C                   MOVEL     FDE(24)       VFD008
174600000000     C                   ELSE                                                   <-- 2 -->
174700000000     C                   MOVEL     FUV(7)        VFD007
174800000000     C     FAT(23)       IFEQ      'S'
174900000000     C                   MOVEL     FDE(23)       VFD008
175000000000     C                   ELSE
175100000000     C                   MOVEL     FDE(24)       VFD008
175200000000     C                   ENDIF
175300000000     C                   ENDIF                                                  <-- 2 ---
175400000000     C                   ENDIF                                                  <-- 1 ---
175500000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
175600000000     C                   ENDSR
175700000000     C*------------------------------------------------------------------------*
175800000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
175900000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
176000000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
176100000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
176200000000     C*------------------------------------------------------------------------*
176300000000     C     CONFUN        BEGSR
176400000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
176500000000     C     1             DO        31            I                              --- 1 -->
176600000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
176700000000     C* CONTROLLO SE IL TASTO E' ATTIVO
176800000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
176900000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
177000000000     C                   EXSR      CARMSG
177100000000     C                   MOVEL     '1'           $ERR
177200000000     C                   ENDIF                                                  <-- 3 ---
177300000000     C                   ENDIF                                                  <-- 2 ---
177400000000     C                   ENDDO                                                  <-- 1 ---
177500000000     C                   ENDSR
177600000000**   SCHIERA: MSG  (MESSAGGI)
177700020116TISE90R- Tasto funzionale non valido.                                          1
177800020116TISE90R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
177900020116TISE90R- Opzione non valida.                                                   3
178000020116TISE90R- Opzione abilitata ma non gestita. Avvertire CED.                      4
178100020116TISE90R- Inizio elenco.                                                        5
178200020116TISE90R- Fine elenco.                                                          6
178300020116TISE90R- Anno/Mese obbligatorio                                                7
178400020116TISE90R- Anno/Mese errato                                                      8
178500020116TISE90R- ATTENZIONE !! Data superiore all' ultimo agg. saldi da contabilità    9
178600020116TISE90R- 'S' per richiedere il Delta di Filiale, 'N' per non richiederlo      10
178700020116TISE90R- Anno/Mese finale inferiore all'Anno/Mese iniziale                    11
178800020116TISE90R- Codice filiale errato o inesistente                                  12
178900020116TISE90R- Codice area errato o inesistente                                     13
179000020116TISE90R- Codice divisione errato o inesistente                                14
179100020116TISE90R- 'S' per richiedere il Delta di Area, 'N' per non richiederlo         15
179200020116TISE90R- 'S' per richiedere il Delta di Divisione, 'N' per non richiederlo    16
179300020116TISE90R- Immettere solo una selezione per filiale, area o divisione           17
179400020116TISE90R- 'S' per richiedere il Delta Commerciale, 'N' per non richiederlo     18
179500020116TISE90R- Impossibile richiedere più di 12 mesi                                19
179600020116TISE90R- Codice commerciale errato o inesistente                              20
179700020116TISE90R- Saldi non presenti: data inferiore all'ultima cancellazione          21
179800020116TISE90R- ATTENZIONE !! Saldi cliente non ancora aggiornati da contabilità     22
179900020116TISE90R- Immettere solo una selezione per commerciale o divisione             23
180000020116TISE90R- Richiedere almeno un statistica                                      24
180100020116TISE90R- Impossibile parzializzare statistiche non selezionate                25
180200020116TISE90R- Il codice commerciale non è di raggruppamento                        26
180300020325TISE90R- Impossibile lanciare la statistica su anni differenti                27
180400020116TISE90R- Tipo servizio errato                                                 28
180500020116TISE90R- Immettere il tipo di dati che si vuole in statistica                 29
180600050224TISE90R- Se Italia+Estero non è possibile generare File PC                    30
180700020116TISE90R- Codice qualità cliente errato.                                       31
180800020116TISE90R- La percentuale non può essere maggiore di 100                        32
180900020116TISE90R- Non si possono lanciare le statistiche di gruppo in SDI !!!          33
181000020116TISE90R- La percentuale A deve essere maggione/uguale della percentuale DA    34
181100020116TISE90R- Immettere uno dei valori possibili                                   35
181200050228TISE90R- Per creare file PC Selezionare Stat.Grp.(A) Delta x Divisione        36
181300050228TISE90R- Per creare file PC Selezionare Stat.Grp.(B) Delta Commerciale        37
181400050228TISE90R- Per creare file PC Selezionare Stat.Grp.(A) Delta x Filiale          38
181500160712TISE90R- Data Unificante Errata - impostare una data corretta.                39
181600020116TISE90R- 40
181700000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
181800000000F1    -                   -    NN     NN
181900000000F2    -                   -    NN     NN
182000000000F3    -F3=Fine            -    SS     SS
182100000000F4    -                   -    NN     NN
182200000000F5    -                   -    NN     NN
182300000000F6    -F6=Lancio          -    SS     SS
182400000000F7    -                   -    NN     NN
182500000000F8    -                   -    NN     NN
182600000000F9    -                   -    NN     NN
182700000000F10   -                   -    NN     NN
182800000000F11   -                   -    NN     NN
182900000000F12   -F12=Ritorno        -    SS     SS
183000000000F13   -                   -    NN     NN
183100000000F14   -                   -    NN     NN
183200000000F15   -                   -    NN     NN
183300000000F16   -                   -    NN     NN
183400000000F17   -                   -    NN     NN
183500000000F18   -                   -    NN     NN
183600000000F19   -                   -    NN     NN
183700000000F20   -                   -    NN     NN
183800000000F21   -                   -    NN     NN
183900000000F22   -                   -    NN     NN
184000000000F23   -                   -    NN     NN
184100000000F24   -F24=Altri tasti    -    NN     NN
184200000000ENTER -                   -    SS     SS
184300000000ROLLUP-                   -    NN     NN
184400000000ROLLDN-                   -    NN     NN
184500000000HELP  -                   -    NN     NN
184600000000PRINT -                   -    NN     NN
184700000000BSPACE-                   -    NN     NN
184800000000CLEAR -                   -    NN     NN
