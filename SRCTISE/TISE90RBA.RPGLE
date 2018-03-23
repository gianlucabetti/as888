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
031400160318     c                   if        v1cIOE = 'T' or V1CIOE = ' '
031500100510     c                   move      'I'           parIOE
031600100510     C                   MOVEL     'S'           parNTW                         ROTTURA NETWORK ESTERI
031700100510     c                   eval      PARCLPtmp = 'N'                              NON FARE (CLP)
031800100510     c                   end
031801160318     c                   if        v1cIOE = ' '
031802160318     c                   movel     *blanks       parioe
031803160318     c                   end
031900000000     C*
032000020116     C                   MOVEL     'SE91'        KCOAZ
032100000000     C                   MOVEL     PARAM         KPJBU
032200030313     C                   CALL      'BCH10'
032300030313 MOM C***                CALL      'TISE91C'
032400000000     C                   PARM                    KPJBA
032500000000     C*
032600100510     C* la seconda volta esegue elaborazioni in accodamento per l'Estero
032700100510     c                   if        v1cIOE = 'T'
032800100510     c                   move      'E'           parIOE
032900100510     C                   MOVEL     'N'           parCRE                         *NON deve CREARE il WorkFILE
033000100510     C                   MOVEL     'S'           parNTW                         ROTTURA NETWORK ESTERI
033100100510     c                   eval      PARCLPtmp = 'T'                              TRIMESTRALE
033200100510     C                   MOVEL     'SE91'        KCOAZ
033300100510     C                   MOVEL     PARAM         KPJBU
033400100510     C                   CALL      'BCH10'
033500100510 MOM C***                CALL      'TISE91C'
033600100510     C                   PARM                    KPJBA
033700100510     c                   end
033800100510     C*
033900950525     C*---                 MOVEL'1'       $FINE            *USCITA DAL PGM
034000000000E   1C                   ENDIF
034100000000     C*
034200000000     C                   ENDSR
034300000000     C*------------------------------------------------------------------------*
034400000000     C*?DECODI - CONTROLLA CAMPI E DECODIFICA
034500000000     C*------------------------------------------------------------------------*
034600000000     C     DECODI        BEGSR
034700000000     C*
034800950525     C* IMPOSTA INDICATORI DI ERRORE SUI CAMPI
034900000000     C                   SETOFF                                       606162
035000000000     C                   SETOFF                                       636465
035100000000     C                   SETOFF                                       666768
035200000000     C                   SETOFF                                       697071
035300970819     C                   SETOFF                                       727374
035400000707     C                   SETOFF                                       757677
035500050228     C                   SETOFF                                       787980
035600000000     C*
035700950525     C* REIMPOSTA VARIABILI DI LAVORO
035800000000     C                   Z-ADD     *ZEROS        SELEZC            1 0
035900000000     C                   Z-ADD     *ZEROS        SELEZF            1 0
036000000000     C                   Z-ADD     *ZEROS        RISUL             4 0
036100000823     C***
036200000823     C* LANCIO
036300000823     C***
036400000000     C*--------------------
036500000000     C* ANNO/MESE INIZIALE
036600000000     C*--------------------
036700000000     C* OBBLIGATORIO
036800000000IF  1C     $ERR          IFEQ      '0'
036900000000IF  2C     V1CDAT        IFEQ      *ZEROS
037000000000     C                   MOVEL     '1'           $ERR
037100000000     C                   MOVEL     MSG(7)        $MSG
037200000000     C                   EXSR      CARMSG
037300000000     C                   SETON                                        60
037400000000E   2C                   ENDIF
037500000000E   1C                   ENDIF
037600000000     C* CORRETTO
037700000000IF  1C     $ERR          IFEQ      '0'
037800990609     C                   MOVEL     V1CDAT        G08INV
037900990609     C                   MOVE      01            G08INV
038000990609     C                   Z-ADD     *ZEROS        G08DAT
038100990609     C                   MOVEL     '3'           G08ERR
038200990609     C                   CALL      'XSRDA8'
038300990609     C                   PARM                    WLBDA8
038400990609IF  2C     G08ERR        IFEQ      '1'
038500000000     C                   MOVEL     '1'           $ERR
038600000000     C                   MOVEL     MSG(8)        $MSG
038700000000     C                   EXSR      CARMSG
038800000000     C                   SETON                                        60
038900000000E   2C                   ENDIF
039000000000E   1C                   ENDIF
039100000000     C*--------------------
039200000000     C* ANNO/MESE FINALE
039300000000     C*--------------------
039400000000     C* OBBLIGATORIO
039500000000IF  1C     V1CDA2        IFEQ      *ZEROS
039600000000     C                   MOVEL     '1'           $ERR
039700000000     C                   MOVEL     MSG(7)        $MSG
039800000000     C                   EXSR      CARMSG
039900000000     C                   SETON                                        61
040000000000E   1C                   ENDIF
040100000000     C* CORRETTO
040200000000IF  1C     $ERR          IFEQ      '0'
040300990609     C                   MOVEL     V1CDA2        G08INV
040400990609     C                   MOVE      01            G08INV
040500990609     C                   Z-ADD     *ZEROS        G08DAT
040600990609     C                   MOVEL     '3'           G08ERR
040700990609     C                   CALL      'XSRDA8'
040800990609     C                   PARM                    WLBDA8
040900990609IF  2C     G08ERR        IFEQ      '1'
041000000000     C                   MOVEL     '1'           $ERR
041100000000     C                   MOVEL     MSG(8)        $MSG
041200000000     C                   EXSR      CARMSG
041300000000     C                   SETON                                        61
041400000000E   2C                   ENDIF
041500000000E   1C                   ENDIF
041600000823     C*--------------------
041700000823     C* ANNO/MESE INIZIALE + FINALE
041800000823     C*--------------------
041900000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
042000000000IF  1C     $ERR          IFEQ      '0'
042100000000IF  2C     V1CDA2        IFLT      V1CDAT
042200000000     C                   MOVEL     '1'           $ERR
042300000000     C                   MOVEL     MSG(11)       $MSG
042400000000     C                   EXSR      CARMSG
042500000000     C                   SETON                                        61
042600000000E   2C                   ENDIF
042700000000E   1C                   ENDIF
042800000000     C*
042900000000     C* ANNO/MESE INIZIALE SUPERIORE/UGUALE A QUELLO CANCELLAZIONE + 1
043000000000IF  1C     $ERR          IFEQ      '0'
043100990609IF  2C     V1CDAT        IFLT      CANAM
043200000000     C                   MOVEL     '1'           $ERR
043300000000     C                   MOVEL     MSG(21)       $MSG
043400000000     C                   EXSR      CARMSG
043500000000     C                   SETON                                        60
043600000000E   2C                   ENDIF
043700000000E   1C                   ENDIF
043800000000     C*
043900000000     C* RICHIEDERE NON PIU' DI DODICI MESI
044000000000IF  1C     $ERR          IFEQ      '0'
044100000000     C     V1CDA2        SUB       V1CDAT        RISUL
044200000000IF  2C     RISUL         IFGE      100
044300000000     C                   MOVEL     '1'           $ERR
044400000000     C                   MOVEL     MSG(19)       $MSG
044500000000     C                   EXSR      CARMSG
044600000000     C                   SETON                                        6160
044700000000E   2C                   ENDIF
044800000000E   1C                   ENDIF
044900020325     C*
045000020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'ANNO)
045100020325IF  1C     $ERR          IFEQ      '0'
045200020325     C                   MOVEL     V1CDAT        ANNODA            4 0
045300020325     C                   MOVEL     V1CDA2        ANNOA             4 0
045400020325IF  2C     ANNODA        IFNE      ANNOA
045500020325     C                   MOVEL     '1'           $ERR
045600020325     C                   MOVEL     MSG(27)       $MSG
045700020325     C                   EXSR      CARMSG
045800020325     C                   SETON                                        6061
045900020325E   2C                   ENDIF
046000020325E   1C                   ENDIF
046100000823     C***
046200000823     C* SELEZIONI COMUNI
046300000823     C***
046400000823     C*--------------------
046500000823     C* % DELTA TOTALE PER STAMPA
046600000823     C*--------------------
046700000823IF  1C     $ERR          IFEQ      '0'
046800000823IF  2C     V1CPDD        IFNE      *ZEROS
046900000823     C     V1CPDA        ORNE      *ZEROS
047000000823IF  3C     V1CPDA        IFLT      V1CPDD
047100000823     C                   MOVEL     '1'           $ERR
047200000823     C                   MOVEL     MSG(34)       $MSG
047300000823     C                   EXSR      CARMSG
047400000823     C                   SETON                                        76
047500000823E   3C                   ENDIF
047600000823E   2C                   ENDIF
047700000823E   1C                   ENDIF
047800000823     C*--------------------
047900000823     C* % DELTA FASCE PER SEGNALAZIONE
048000000823     C*--------------------
048100000823IF  1C     $ERR          IFEQ      '0'
048200000823IF  2C     V1CSPD        IFNE      *ZEROS
048300000823IF  3C     V1CSPD        IFGT      100
048400000823     C                   MOVEL     '1'           $ERR
048500000823     C                   MOVEL     MSG(32)       $MSG
048600000823     C                   EXSR      CARMSG
048700000823     C                   SETON                                        74
048800000823E   3C                   ENDIF
048900000823E   2C                   ENDIF
049000000823E   1C                   ENDIF
049100020701     C*--------------------
049200020701     C* % SCOSTAMENTO FATTURATO ANNO PRECEDENTE
049300020701     C*--------------------
049400020701IF  1C     $ERR          IFEQ      '0'
049500020701IF  2C     V1CDFD        IFNE      *ZEROS
049600020701     C     V1CDFA        ORNE      *ZEROS
049700020701IF  3C     V1CDFD        IFGT      V1CDFA
049800020701     C                   MOVEL     '1'           $ERR
049900020701     C                   MOVEL     MSG(34)       $MSG
050000020701     C                   EXSR      CARMSG
050100020701     C                   SETON                                        47
050200020701E   3C                   ENDIF
050300020701E   2C                   ENDIF
050400020701E   1C                   ENDIF
050500020701     C*
050600020701IF  1C     $ERR          IFEQ      '0'
050700020701IF  2C     V1CDFDP       IFNE      *ZEROS
050800020701     C     V1CDFAP       ORNE      *ZEROS
050900020701IF  3C     V1CDFDP       IFGT      V1CDFAP
051000020701     C                   MOVEL     '1'           $ERR
051100020701     C                   MOVEL     MSG(34)       $MSG
051200020701     C                   EXSR      CARMSG
051300020701     C                   SETON                                        49
051400020701E   3C                   ENDIF
051500020701E   2C                   ENDIF
051600020701E   1C                   ENDIF
051700000823     C*--------------------
051800000823     C* TIPO SERVIZIO
051900000823     C*--------------------
052000000823     C                   MOVEL     *BLANKS       WTSP              1
052100000823     C                   MOVEL     *BLANKS       WDTSP            15
052200000823     C                   MOVEL     *BLANKS       V1DTSP
052300000823IF  1C     $ERR          IFEQ      '0'
052400000823IF  2C     V1CTSP        IFNE      *BLANKS
052500000823     C                   MOVEL     V1CTSP        WTSP
052600000823     C                   EXSR      CHKTSP
052700000823     C                   MOVEL     WTSP          V1CTSP
052800000823     C                   MOVEL     WDTSP         V1DTSP
052900000823IF  3C     $ERR          IFEQ      '1'
053000000823     C                   MOVEL     '1'           $ERR
053100000823     C                   MOVEL     MSG(28)       $MSG
053200000823     C                   EXSR      CARMSG
053300000823     C                   SETON                                        71
053400000823E   3C                   ENDIF
053500000823E   2C                   ENDIF
053600000823E   1C                   ENDIF
053700000823     C*--------------------
053800000823     C* DATI ITALIA / ESTERO
053900000823     C*--------------------
054000000823IF  1C     $ERR          IFEQ      '0'
054100000823IF  2C     V1CIOE        IFEQ      'I'
054200000823     C     V1CIOE        OREQ      'E'
054300100510     C     V1CIOE        OREQ      'T'
054301160318     C     V1CIOE        OREQ      ' '
054400000823X   2C                   ELSE
054500000823     C                   MOVEL     '1'           $ERR
054600000823     C                   MOVEL     MSG(29)       $MSG
054700000823     C                   EXSR      CARMSG
054800000823     C                   SETON                                        72
054900000823E   2C                   ENDIF
055000000823E   1C                   ENDIF
055100050228     C*--------------------
055200050228     C* Crea File PC : Non può crearlo se richiesto Italia+Estero
055300050228     C*--------------------
055400051107E   1C************   devono essere creati i dati nel WFDELPCF se richiesto Italia+Estero
055500051107IF  1C**** $ERR          IFEQ      '0'
055600051107IF  2C**** V1CIOE        IFEQ      *BLANKS
055700051107     C**** V1CFPC        andeq     'S'
055800051107     C****               MOVEL     '1'           $ERR
055900051107     C****               MOVEL     MSG(30)       $MSG
056000051107     C****               EXSR      CARMSG
056100051107     C****               SETON                                        73
056200051107E   2C****               ENDIF
056300051107E   1C****               ENDIF
056400051107E   1C************
056500000823     C*--------------------
056600000823     C* QUALITA' CLIENTE
056700000823     C*--------------------
056800010618     C* 1° categoria
056900000823IF  1C     $ERR          IFEQ      '0'
057000000823IF  2C     V1CCLV        IFNE      *BLANKS
057100000823     C                   MOVEL     V1CCLV        WCLV              1
057200000823     C                   EXSR      CHKCLV
057300000823     C                   MOVEL     WCLV          V1CCLV
057400000823IF  3C     $ERR          IFEQ      '1'
057500000823     C                   MOVEL     MSG(31)       $MSG
057600000823     C                   EXSR      CARMSG
057700010618     C                   SETON                                        50
057800000823E   3C                   ENDIF
057900000823E   2C                   ENDIF
058000000823E   1C                   ENDIF
058100010618     C* 2° categoria
058200010618IF  1C     $ERR          IFEQ      '0'
058300010618IF  2C     V1CCL1        IFNE      *BLANKS
058400010618     C                   MOVEL     V1CCL1        WCLV              1
058500010618     C                   EXSR      CHKCLV
058600010618     C                   MOVEL     WCLV          V1CCL1
058700010618IF  3C     $ERR          IFEQ      '1'
058800010618     C                   MOVEL     MSG(31)       $MSG
058900010618     C                   EXSR      CARMSG
059000010618     C                   SETON                                        51
059100010618E   3C                   ENDIF
059200010618E   2C                   ENDIF
059300010618E   1C                   ENDIF
059400010618     C* 3° categoria
059500010618IF  1C     $ERR          IFEQ      '0'
059600010618IF  2C     V1CCL2        IFNE      *BLANKS
059700010618     C                   MOVEL     V1CCL2        WCLV              1
059800010618     C                   EXSR      CHKCLV
059900010618     C                   MOVEL     WCLV          V1CCL2
060000010618IF  3C     $ERR          IFEQ      '1'
060100010618     C                   MOVEL     MSG(31)       $MSG
060200010618     C                   EXSR      CARMSG
060300010618     C                   SETON                                        52
060400010618E   3C                   ENDIF
060500010618E   2C                   ENDIF
060600010618E   1C                   ENDIF
060700020220     C* 4° categoria
060800020220IF  1C     $ERR          IFEQ      '0'
060900020220IF  2C     V1CCL3        IFNE      *BLANKS
061000020220     C                   MOVEL     V1CCL3        WCLV              1
061100020220     C                   EXSR      CHKCLV
061200020220     C                   MOVEL     WCLV          V1CCL3
061300020220IF  3C     $ERR          IFEQ      '1'
061400020220     C                   MOVEL     MSG(31)       $MSG
061500020220     C                   EXSR      CARMSG
061600020220     C                   SETON                                        53
061700020220E   3C                   ENDIF
061800020220E   2C                   ENDIF
061900020220E   1C                   ENDIF
062000020220     C* 5° categoria
062100020220IF  1C     $ERR          IFEQ      '0'
062200020220IF  2C     V1CCL4        IFNE      *BLANKS
062300020220     C                   MOVEL     V1CCL4        WCLV              1
062400020220     C                   EXSR      CHKCLV
062500020220     C                   MOVEL     WCLV          V1CCL4
062600020220IF  3C     $ERR          IFEQ      '1'
062700020220     C                   MOVEL     MSG(31)       $MSG
062800020220     C                   EXSR      CARMSG
062900020220     C                   SETON                                        54
063000020220E   3C                   ENDIF
063100020220E   2C                   ENDIF
063200020220E   1C                   ENDIF
063300000823     C*--------------------
063400000823     C* DATI POSTE
063500000823     C*--------------------
063600000823IF  1C     $ERR          IFEQ      '0'
063700000823IF  2C     V1CPOS        IFEQ      'S'
063800000823     C     V1CPOS        OREQ      'N'
063900000823X   2C                   ELSE
064000000823     C                   MOVEL     '1'           $ERR
064100000823     C                   MOVEL     MSG(35)       $MSG
064200000823     C                   EXSR      CARMSG
064300000823     C                   SETON                                        77
064400000823E   2C                   ENDIF
064500000823E   1C                   ENDIF
064600000823     C*--------------------
064700000823     C* DATI CLIENTE / UNIFICANTE
064800000823     C*--------------------
064900000823IF  1C     $ERR          IFEQ      '0'
065000000823IF  2C     V1CSCE        IFEQ      'C'
065100000823     C     V1CSCE        OREQ      'U'
065200000823X   2C                   ELSE
065300000823     C                   MOVEL     '1'           $ERR
065400000823     C                   MOVEL     MSG(35)       $MSG
065500000823     C                   EXSR      CARMSG
065600000823     C                   SETON                                        78
065700000823E   2C                   ENDIF
065800000823E   1C                   ENDIF
065900000823     C***
066000000823     C* STATISTICHE GRUPPO (A)
066100000823     C***
066200000823     C*--------------------
066300000823     C* DELTA DI FILIALE
066400000823     C*--------------------
066500000000IF  1C     $ERR          IFEQ      '0'
066600000000IF  2C     V1CDCK        IFEQ      'S'
066700000000     C     V1CDCK        OREQ      'N'
066800000000X   2C                   ELSE
066900000000     C                   MOVEL     '1'           $ERR
067000000000     C                   MOVEL     MSG(10)       $MSG
067100000000     C                   EXSR      CARMSG
067200000000     C                   SETON                                        62
067300000000E   2C                   ENDIF
067400000000E   1C                   ENDIF
067500050228     C*--------------------
067600050228     C* Crea File PC : Non può crearlo se NON richiesto Delta di Filiale
067700050228     C*--------------------
067800050228IF  1C     $ERR          IFEQ      '0'
067900050228IF  2C     V1CDCK        IFEQ      'N'
068000050228     C     V1CFPC        andeq     'S'
068100050228     C                   MOVEL     '1'           $ERR
068200050228     C                   MOVEL     MSG(38)       $MSG
068300050228     C                   EXSR      CARMSG
068400050228     C                   SETON                                        73
068500050228E   2C                   ENDIF
068600050228E   1C                   ENDIF
068700000823     C*--------------------
068800000823     C* DELTA DI AREA
068900000823     C*--------------------
069000000000IF  1C     $ERR          IFEQ      '0'
069100000000IF  2C     V1CDCC        IFEQ      'S'
069200000000     C     V1CDCC        OREQ      'N'
069300000000X   2C                   ELSE
069400000000     C                   MOVEL     '1'           $ERR
069500000000     C                   MOVEL     MSG(15)       $MSG
069600000000     C                   EXSR      CARMSG
069700000000     C                   SETON                                        63
069800000000E   2C                   ENDIF
069900000000E   1C                   ENDIF
070000000823     C*--------------------
070100000823     C* DELTA DI DIVISIONE
070200000823     C*--------------------
070300000000IF  1C     $ERR          IFEQ      '0'
070400000000IF  2C     V1CDCF        IFEQ      'S'
070500000000     C     V1CDCF        OREQ      'N'
070600000000X   2C                   ELSE
070700000000     C                   MOVEL     '1'           $ERR
070800000000     C                   MOVEL     MSG(16)       $MSG
070900000000     C                   EXSR      CARMSG
071000000000     C                   SETON                                        64
071100000000E   2C                   ENDIF
071200000000E   1C                   ENDIF
071300000823     C***
071400000823     C* SELEZIONI STATISTICHE GRUPPO (A)
071500000823     C***
071600000823     C*--------------------
071700000000     C* FILIALE
071800000823     C*--------------------
071900000000IF  1C     $ERR          IFEQ      '0'
072000000000IF  2C     V1CFIC        IFNE      *BLANKS                                      *SELEZIONATA
072100000000     C                   MOVEL     V1CFIC        W1CFIL            3
072200000000     C                   EXSR      CHKFIL                                       *CONTROLLO
072300000000     C                   MOVEL     W1CFIL        V1CFIC
072400000000IF  3C     $ERR          IFEQ      '1'
072500000000     C                   MOVEL     MSG(12)       $MSG
072600000000     C                   EXSR      CARMSG
072700000000     C                   SETON                                        65
072800000000E   3C                   ENDIF
072900000000X   2C                   ELSE
073000000000     C                   MOVEL     *ZEROS        W1CFIL                         *NON SELEZIONATA
073100000000E   2C                   ENDIF
073200000000E   1C                   ENDIF
073300000823     C*--------------------
073400000823     C* AREA
073500000823     C*--------------------
073600000000IF  1C     $ERR          IFEQ      '0'
073700000000IF  2C     V1CARC        IFNE      *BLANKS                                      *SELEZIONATA
073800000000     C                   MOVEL     V1CARC        W1CARE            3
073900000000     C                   EXSR      CHKARE                                       *CONTROLLO
074000000000     C                   MOVEL     W1CARE        V1CARC
074100000000IF  3C     $ERR          IFEQ      '1'
074200000000     C                   MOVEL     MSG(13)       $MSG
074300000000     C                   EXSR      CARMSG
074400000000     C                   SETON                                        66
074500000000E   3C                   ENDIF
074600000000X   2C                   ELSE                                                   *NON SELEZIONATA
074700000000     C                   MOVEL     *ZEROS        W1CARE
074800000000E   2C                   ENDIF
074900000000E   1C                   ENDIF
075000000823     C*--------------------
075100000000     C* DIVISIONE
075200000823     C*--------------------
075300000000IF  1C     $ERR          IFEQ      '0'
075400000000IF  2C     V1CDIC        IFNE      *BLANKS                                      *SELEZIONATA
075500000000     C                   MOVEL     V1CDIC        W1CDIV            1
075600000000     C                   EXSR      CHKDIV                                       *CONTROLLO
075700000000     C                   MOVEL     W1CDIV        V1CDIC
075800000000IF  3C     $ERR          IFEQ      '1'
075900000000     C                   MOVEL     MSG(14)       $MSG
076000000000     C                   EXSR      CARMSG
076100000000     C                   SETON                                        67
076200000000E   3C                   ENDIF
076300000000E   2C                   ENDIF
076400000000E   1C                   ENDIF
076500000824     C*--------------------
076600000824     C* SELEZIONI SOLO PER LE STATISTICHE SCELTE GRUPPO (A)
076700000824     C*--------------------
076800000824IF  1C     $ERR          IFEQ      '0'
076900000824IF  2C     V1CDCK        IFEQ      'N'
077000000824     C     V1CDCC        ANDEQ     'N'
077100000824     C     V1CDCF        ANDEQ     'N'
077200000824IF  3C     V1CFIC        IFGT      *BLANKS
077300000824     C     V1CARC        ORGT      *BLANKS
077400000824     C     V1CDIC        ORGT      *ZEROS
077500000824     C                   MOVEL     MSG(25)       $MSG
077600000824     C                   EXSR      CARMSG
077700000824     C                   SETON                                        626364
077800000824E   3C                   ENDIF
077900000824E   2C                   ENDIF
078000000824E   1C                   ENDIF
078100000824     C*--------------------
078200000824     C* SOLO UNA SELEZIONE STATISTICHE GRUPPO (A)
078300000824     C*--------------------
078400000824IF  1C     $ERR          IFEQ      '0'
078500000824IF  2C     V1CFIC        IFGT      *ZEROS
078600000824     C                   ADD       1             SELEZC
078700000824E   2C                   ENDIF
078800000824IF  2C     V1CARC        IFGT      *ZEROS
078900000824     C                   ADD       1             SELEZC
079000000824E   2C                   ENDIF
079100000824IF  2C     V1CDIC        IFNE      *BLANKS
079200000824     C                   ADD       1             SELEZC
079300000824E   2C                   ENDIF
079400000824IF  2C     SELEZC        IFGT      1
079500000824     C                   MOVEL     '1'           $ERR
079600000824     C                   MOVEL     MSG(17)       $MSG
079700000824     C                   EXSR      CARMSG
079800000824     C                   SETON                                        656667
079900000824E   2C                   ENDIF
080000000824E   1C                   ENDIF
080100050228     C*--------------------
080200050228     C* Crea File PC : WFTISE93F Non può crearlo se NON richiesto Delta di Divisione
080300050228     C*--------------------
080400050228IF  1C     $ERR          IFEQ      '0'
080500050228IF  2C     V1CDCF        IFEQ      'N'
080600050228     C     V1CWCP        andeq     'S'
080700050228     C                   MOVEL     '1'           $ERR
080800050228     C                   MOVEL     MSG(36)       $MSG
080900050228     C                   EXSR      CARMSG
081000050228     C                   SETON                                        79
081100050228E   2C                   ENDIF
081200050228E   1C                   ENDIF
081300000823     C***
081400000823     C* STATISTICHE GRUPPO (B)
081500000823     C***
081600000823     C*--------------------
081700000823     C* DELTA COMMERCIALE
081800000823     C*--------------------
081900000000IF  1C     $ERR          IFEQ      '0'
082000000000IF  2C     V1CDFF        IFEQ      'S'
082100000000     C     V1CDFF        OREQ      'N'
082200000000X   2C                   ELSE
082300000000     C                   MOVEL     '1'           $ERR
082400000000     C                   MOVEL     MSG(18)       $MSG
082500000000     C                   EXSR      CARMSG
082600000000     C                   SETON                                        68
082700000000E   2C                   ENDIF
082800000000E   1C                   ENDIF
082900000823     C***
083000000823     C* SELEZIONI STATISTICHE GRUPPO (B)
083100000823     C***
083200000823     C*--------------------
083300000000     C* RAGGRUPPAMENTO COMMERCIALE
083400000823     C*--------------------
083500000000IF  1C     $ERR          IFEQ      '0'
083600000000IF  2C     V1CCOF        IFNE      *BLANKS                                      *SELEZIONATO
083700000000     C                   MOVEL     V1CCOF        W1CCOM            7
083800000000     C                   EXSR      CHKCOM                                       *CONTROLLO
083900000000     C                   MOVEL     W1CCOM        V1CCOF
084000000000IF  3C     $ERR          IFEQ      '1'
084100000000     C                   EXSR      CARMSG
084200000000     C                   SETON                                        69
084300000000E   3C                   ENDIF
084400000000X   2C                   ELSE                                                   *NON SELEZIONATO
084500000000     C                   MOVEL     *ZEROS        W1CCOM
084600000000E   2C                   ENDIF
084700000000E   1C                   ENDIF
084800000823     C*--------------------
084900000000     C* DIVISIONE
085000000823     C*--------------------
085100000000IF  1C     $ERR          IFEQ      '0'
085200000000IF  2C     V1CDIF        IFNE      *BLANKS                                      *SELEZIONATA
085300000000     C                   MOVEL     V1CDIF        W1CDIV            1
085400000000     C                   EXSR      CHKDIV                                       *CONTROLLO
085500000000     C                   MOVEL     W1CDIV        V1CDIF
085600000000IF  3C     $ERR          IFEQ      '1'
085700000000     C                   MOVEL     MSG(14)       $MSG
085800000000     C                   EXSR      CARMSG
085900000000     C                   SETON                                        70
086000000000E   3C                   ENDIF
086100000000E   2C                   ENDIF
086200000000E   1C                   ENDIF
086300000824     C*--------------------
086400000824     C* SELEZIONI SOLO PER LE STATISTICHE SCELTE GRUPPO (B)
086500000824     C*--------------------
086600000824IF  1C     $ERR          IFEQ      '0'
086700000824IF  2C     V1CCOF        IFNE      *BLANKS
086800000824     C                   ADD       1             SELEZF
086900000824E   2C                   ENDIF
087000000824IF  2C     V1CDIF        IFNE      *BLANKS
087100000824     C                   ADD       1             SELEZF
087200000824E   2C                   ENDIF
087300000824IF  2C     SELEZF        IFGT      1
087400000824     C                   MOVEL     '1'           $ERR
087500000824     C                   MOVEL     MSG(23)       $MSG
087600000824     C                   EXSR      CARMSG
087700000824     C                   SETON                                        6970
087800000824E   2C                   ENDIF
087900000824E   1C                   ENDIF
088000000824     C*--------------------
088100000824     C* SOLO UNA SELEZIONE STATISTICHE GRUPPO (B)
088200000824     C*--------------------
088300000824IF  1C     $ERR          IFEQ      '0'
088400000824IF  2C     V1CDFF        IFEQ      'N'
088500000824IF  3C     V1CCOF        IFGT      *BLANKS
088600000824     C     V1CDIF        ORGT      *BLANKS
088700000824     C                   MOVEL     MSG(25)       $MSG
088800000824     C                   EXSR      CARMSG
088900000824     C                   SETON                                        68
089000000824E   3C                   ENDIF
089100000824E   2C                   ENDIF
089200000824E   1C                   ENDIF
089300050228     C*--------------------
089400050228     C* Crea File PC : WFTISE94F Non può crearlo se NON richiesto Delta Commerciale
089500050228     C*--------------------
089600050228IF  1C     $ERR          IFEQ      '0'
089700050228IF  2C     V1CDFF        IFEQ      'N'
089800050228     C     V1CWPC        andeq     'S'
089900050228     C                   MOVEL     '1'           $ERR
090000050228     C                   MOVEL     MSG(37)       $MSG
090100050228     C                   EXSR      CARMSG
090200050228     C                   SETON                                        80
090300050228E   2C                   ENDIF
090400050228E   1C                   ENDIF
090500000823     C***
090600000823     C* STATISTICHE GRUPPO (A) + GRUPPO (B)
090700000823     C***
090800000000     C*--------------------
090900000823     C* ALMENO UNA STATISTICA
091000000000     C*--------------------
091100000000IF  1C     $ERR          IFEQ      '0'
091200000000IF  2C     V1CDCK        IFEQ      'N'
091300000000     C     V1CDCC        ANDEQ     'N'
091400000000     C     V1CDCF        ANDEQ     'N'
091500000000     C     V1CDFF        ANDEQ     'N'
091600000000     C                   MOVEL     '1'           $ERR
091700000000     C                   MOVEL     MSG(24)       $MSG
091800000000     C                   EXSR      CARMSG
091900000000     C                   SETON                                        626364
092000000000     C                   SETON                                        68
092100000000E   2C                   ENDIF
092200000000E   1C                   ENDIF
092300000823     C*--------------------
092400000823     C* MESSAGGI SOLO DI ATTENZIONE
092500000823     C*--------------------
092600000823     C* I SALDI NON SONO AGGIORNATI CON LE RETTIFICHE E LE NOTE DI CREDITO
092700000823IF  1C     $ERR          IFEQ      '0'
092800000823IF  2C     WFSC          IFEQ      'N'                                          *SALDI NON AGGIORNAT
092900000823     C                   MOVEL     MSG(22)       $MSG
093000000823     C                   EXSR      CARMSG
093100000823E   2C                   ENDIF
093200000823E   1C                   ENDIF
093300000823     C*
093400000823     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO AGGIORNAMENTO - 1
093500000823IF  1C     $ERR          IFEQ      '0'
093600000823IF  2C     V1CDA2        IFGT      AGGAM
093700100510IF  2C     V1budget      andeq     'N'
093800000823     C                   MOVEL     MSG(9)        $MSG
093900000823     C                   EXSR      CARMSG
094000000823E   2C                   ENDIF
094100000823E   1C                   ENDIF
094200950418     C*
094300000000     C                   ENDSR
094400000000     C*------------------------------------------------------------------------*
094500000000     C*?CHKTSP - CONTROLLA IL TIPO SERVIZIO
094600000000     C*------------------------------------------------------------------------*
094700000000     C     CHKTSP        BEGSR
094800000000     C*
094900000000     C* CONTROLLO SE E' STATO INSERITO '?'
095000000000     C     '?'           SCAN      WTSP                                   99
095100000000     C*
095200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
095300000000IF  1C     *IN99         IFEQ      '1'
095400020916     C                   Z-ADD     1             §KUT
095500000000     C                   MOVEL     '5E'          §COD
095600000000     C                   MOVEL     *BLANKS       §KEY
095700000000     C                   MOVEL     *BLANKS       §DES
095800980403     C                   CALL      'X§TABER'
095900000000     C                   PARM                    §KUT              1 0
096000000000     C                   PARM                    §COD              2
096100000000     C                   PARM                    §KEY              8
096200000000     C                   PARM                    §DES             30
096300000000     C*
096400000000     C                   MOVEL     §KEY          WTSP
096500000000     C                   MOVEL     §DES          WDTSP
096600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
096700000000X   1C                   ELSE
096800000000     C*
096900000000     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
097000020916     C                   Z-ADD     1             KTBKUT
097100000000     C                   MOVEL     '5E'          KTBCOD
097200000000     C                   MOVEL     *BLANKS       KTBKEY
097300000000     C                   MOVEL     WTSP          KTBKEY
097400000000     C     KEYTAB        CHAIN     TABEL00F                           99
097500000000IF  2C     *IN99         IFEQ      '1'
097600000000     C     TBLFLG        OREQ      '*'
097700000000     C                   MOVEL     '1'           $ERR
097800000000     C                   ELSE
097900090610     C                   MOVEL     TBLUNI        ds5e
098000090610     C                   MOVEL     §5ED08        WDTSP
098100000000E   2C                   ENDIF
098200000000E   1C                   ENDIF
098300000000     C*
098400000000     C                   ENDSR
098500980521     C*------------------------------------------------------------------------*
098600980521     C*?CHKCLV - CONTROLLA LA QUALITA' CLIENTE
098700980521     C*------------------------------------------------------------------------*
098800980521     C     CHKCLV        BEGSR
098900980521     C*
099000980521     C* CONTROLLO SE E' STATO INSERITO '?'
099100980521     C     '?'           SCAN      WCLV                                   99
099200980521     C*
099300980521     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
099400980521IF  1C     *IN99         IFEQ      '1'
099500020916     C                   Z-ADD     1             §KUT
099600980521     C                   MOVEL     'IC'          §COD
099700980521     C                   MOVEL     *BLANKS       §KEY
099800980521     C                   MOVEL     *BLANKS       §DES
099900980521     C                   CALL      'X§TABER'
100000980521     C                   PARM                    §KUT              1 0
100100980521     C                   PARM                    §COD              2
100200980521     C                   PARM                    §KEY              8
100300980521     C                   PARM                    §DES             30
100400980521     C                   MOVEL     §KEY          WCLV
100500980521     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
100600980521X   1C                   ELSE
100700980521     C*
100800980521     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
100900020916     C                   Z-ADD     1             KTBKUT
101000980521     C                   MOVEL     'IC'          KTBCOD
101100980521     C                   MOVEL     *BLANKS       KTBKEY
101200980521     C                   MOVEL     WCLV          KTBKEY
101300980521     C     KEYTAB        CHAIN     TABEL00F                           99
101400980521IF  2C     *IN99         IFEQ      '1'
101500980521     C     TBLFLG        OREQ      '*'
101600980521     C                   MOVEL     '1'           $ERR
101700980521E   2C                   ENDIF
101800980521E   1C                   ENDIF
101900980521     C*
102000980521     C                   ENDSR
102100000000     C*------------------------------------------------------------------------*
102200000000     C*?CHKFIL - CONTROLLA LA FILIALE
102300000000     C*------------------------------------------------------------------------*
102400000000     C     CHKFIL        BEGSR
102500000000     C*
102600000000     C* CONTROLLO SE E' STATO INSERITO '?'
102700000000     C     '?'           SCAN      W1CFIL        A                 1 0    99
102800000000     C*
102900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE FILIALI
103000000000IF  1C     *IN99         IFEQ      '1'
103100000000     C                   MOVEL     *BLANKS       §COD1
103200000000     C                   MOVE      *BLANKS       §DES1
103300971203     C                   MOVE      *BLANKS       §PRA
103400971203     C                   CALL      'TNSD24R'
103500000000     C                   PARM                    §COD1             3
103600000000     C                   PARM                    §TIP              1
103700000000     C                   PARM                    §DES1            25
103800971203     C                   PARM                    §PRA              3
103900000000     C*
104000000000     C                   MOVEL     §COD1         W1CFIL            3
104100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
104200000000X   1C                   ELSE
104300000000     C*
104400000000     C* CARATTERI ALFABETICI NON VALIDI
104500000000IF  2C     W1CFIL        IFLT      *ZEROS
104600000000     C                   MOVEL     '1'           $ERR
104700000000X   2C                   ELSE
104800000000     C*
104900000000     C* CONTROLLO CHE LA FILIALE ESISTA IN ARCHIVIO
105000000000     C                   MOVEL     W1CFIL        KORFIL
105100000000     C     KEYORG        CHAIN     AZORG01L                           99
105200000000IF  3C     *IN99         IFEQ      '1'
105300000000     C     ORGFAG        OREQ      'V'
105400000000     C                   MOVEL     '1'           $ERR
105500000000E   3C                   ENDIF
105600000000E   2C                   ENDIF
105700000000E   1C                   ENDIF
105800000000     C*
105900000000     C                   ENDSR
106000000000     C*------------------------------------------------------------------------*
106100000000     C*?CHKARE - CONTROLLA L' AREA
106200000000     C*------------------------------------------------------------------------*
106300000000     C     CHKARE        BEGSR
106400000000     C*
106500000000     C* CONTROLLO SE E' STATO INSERITO '?'
106600000000     C     '?'           SCAN      W1CARE        A                 1 0    99
106700000000     C*
106800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE AREE
106900000000IF  1C     *IN99         IFEQ      '1'
107000020916     C                   Z-ADD     1             §KUT
107100000000     C                   MOVEL     '05'          §COD
107200000000     C                   MOVEL     *BLANKS       §KEY
107300000000     C                   MOVEL     *BLANKS       §DES
107400000000     C                   CALL      'X§TABER'
107500000000     C                   PARM                    §KUT              1 0
107600000000     C                   PARM                    §COD              2
107700000000     C                   PARM                    §KEY              8
107800000000     C                   PARM                    §DES             30
107900000000     C*
108000000000     C                   MOVEL     §KEY          W1CARE
108100000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
108200000000X   1C                   ELSE
108300000000     C*
108400000000     C* CARATTERI ALFABETICI NON VALIDI
108500000000IF  2C     W1CARE        IFLT      *ZEROS
108600000000     C                   MOVEL     '1'           $ERR
108700000000X   2C                   ELSE
108800000000     C*
108900000000     C* CONTROLLO CHE L'AREA ESISTA IN TABELLA
109000020916     C                   Z-ADD     1             KTBKUT
109100000000     C                   MOVEL     '05'          KTBCOD
109200000000     C                   MOVEL     *BLANKS       KTBKEY
109300000000     C                   MOVEL     W1CARE        KTBKEY
109400000000     C     KEYTAB        CHAIN     TABEL00F                           99
109500000000IF  3C     *IN99         IFEQ      '1'
109600000000     C     TBLFLG        OREQ      '*'
109700000000     C                   MOVEL     '1'           $ERR
109800000000E   3C                   ENDIF
109900000000E   2C                   ENDIF
110000000000E   1C                   ENDIF
110100000000     C*
110200000000     C                   ENDSR
110300000000     C*------------------------------------------------------------------------*
110400000000     C*?CHKDIV - CONTROLLA LA DIVISIONE
110500000000     C*------------------------------------------------------------------------*
110600000000     C     CHKDIV        BEGSR
110700000000     C*
110800000000     C* CONTROLLO SE E' STATO INSERITO '?'
110900000000     C     '?'           SCAN      W1CDIV                                 99
111000000000     C*
111100000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE DIVISIONI
111200000000IF  1C     *IN99         IFEQ      *ON
111300020916     C                   Z-ADD     1             §KUT
111400000000     C                   MOVEL     '17'          §COD
111500000000     C                   MOVEL     *BLANKS       §KEY
111600000000     C                   MOVEL     *BLANKS       §DES
111700980403     C                   CALL      'X§TABER'
111800000000     C                   PARM                    §KUT
111900000000     C                   PARM                    §COD
112000000000     C                   PARM                    §KEY
112100000000     C                   PARM                    §DES
112200000000     C*
112300000000     C                   MOVEL     §KEY          W1CDIV
112400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
112500000000X   1C                   ELSE
112600000000     C*
112700000000     C* CONTROLLO CHE LA DIVISIONE ESISTA IN TABELLA
112800020916     C                   Z-ADD     1             KTBKUT
112900000000     C                   MOVEL     '17'          KTBCOD
113000000000     C                   MOVEL     *BLANKS       KTBKEY
113100000000     C                   MOVEL     W1CDIV        KTBKEY
113200000000     C     KEYTAB        CHAIN     TABEL00F                           99
113300000000IF  2C     *IN99         IFEQ      *ON
113400000000     C     TBLFLG        OREQ      '*'
113500000000     C                   MOVEL     '1'           $ERR
113600000000E   2C                   ENDIF
113700000000E   1C                   ENDIF
113800000000     C*
113900000000     C                   ENDSR
114000000000     C*------------------------------------------------------------------------*
114100000000     C*?CHKCOM - CONTROLLO IL CODICE COMMERCIALE E SUO RAGGRUPPAMENTO
114200000000     C*------------------------------------------------------------------------*
114300000000     C     CHKCOM        BEGSR
114400000000     C*
114500000000     C* CONTROLLO SE E' STATO INSERITO '?'
114600000000     C     '?'           SCAN      W1CCOM                                 99
114700000000     C*
114800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE COMMERCIALI
114900000000IF  1C     *IN99         IFEQ      *ON
115000140612     C                   CLEAR                   TRMK43ds
115100140612     C                   CALL      'TRMK43R'
115200140612     C                   PARM                    KPJBA
115300140612     C                   PARM                    TRMK43ds
115400140612     c                   if        oMK43err = *off  and
115500140612     c                             oMK43fxx = *blank
115600140612     C                   MOVEL     oMK43cmm      W1CCOM
115700140612     c                   endif
115800140612     C*
115900140612     C*********          Z-ADD     1             §KUT
116000140612     C*********          MOVEL     '01'          §COD
116100140612     C*********          MOVEL     *BLANKS       §KEY
116200140612     C*********          MOVEL     *BLANKS       §DES
116300140612     C*********          CALL      'X§TABER'
116400140612     C*********          PARM                    §KUT
116500140612     C*********          PARM                    §COD
116600140612     C*********          PARM                    §KEY
116700140612     C*********          PARM                    §DES
116800140612     C*********          MOVEL     §KEY          W1CCOM
116900140612     C*********
117000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
117100140612     C*********
117200000000X   1C                   ELSE
117300000000     C*
117400000000     C* CARATTERI ALFABETICI NON VALIDI
117500000000IF  2C     W1CCOM        IFLT      *ZEROS
117600000000     C                   MOVEL     '1'           $ERR
117700000000     C                   MOVEL     MSG(20)       $MSG
117800000000X   2C                   ELSE
117900000000     C*
118000000000     C* CONTROLLO CHE IL COMMERCIALE ESISTA IN TABELLA
118100140612     C*********          Z-ADD     1             KTBKUT
118200140612     C*********          MOVEL     '01'          KTBCOD
118300140612     C*********          MOVEL     *BLANKS       KTBKEY
118400140612     C*********          MOVEL     W1CCOM        KTBKEY
118500140612     C***  KEYTAB        CHAIN     TABEL00F                           99
118600140612IF  3C***  *IN99         IFEQ      *ON
118700140612     C**** TBLFLG        OREQ      '*'
118800140612     C*********          MOVEL     '1'           $ERR
118900140612     C*********          MOVEL     MSG(20)       $MSG
119000140612X   3C*********          ELSE
119100140612     C*********
119200000000     C* CONTROLLO CHE IL CODICE SIA ANCHE DI RAGGRUPPAMENTO
119300140612     C*********          MOVEL     TBLUNI        DS01
119400140612     C*********          MOVEL     §01RGF        ALF7              7
119500140612IF  4C**** ALF7          IFNE      TBLKEY
119600140612     C*********          MOVEL     '1'           $ERR
119700140612     C*********          MOVEL     MSG(26)       $MSG
119800140612E   4C*********          ENDIF
119900140612E   3C*********          ENDIF
120000140612     C*********
120100140612     C*   ora è sul file e non più sulle tabelle
120200140612     C*********
120300140612     C                   MOVE      W1CCOM        code7n            7 0
120400140612     C     code7n        chain     azCMM01l                           99
120500140612IF  3C                   If        not %Found(azCMM01l) or
120600140612     C                                cmmATB <> *blank
120700140612     C                   MOVEL     '1'           $ERR
120800140612     C                   MOVEL     MSG(20)       $MSG
120900140612X   3C                   ELSE
121000140612     C*
121100140612     C* CONTROLLO CHE IL CODICE SIA ANCHE DI RAGGRUPPAMENTO
121200140612IF  4C     cmmCOD        IFNE      cmmUNI
121300140612     C                   MOVEL     '1'           $ERR
121400140612     C                   MOVEL     MSG(26)       $MSG
121500140612E   4C                   ENDIF
121600140612E   3C                   ENDIF
121700140612     C*
121800000000E   2C                   ENDIF
121900000000E   1C                   ENDIF
122000000000     C*
122100000000     C                   ENDSR
122200000000     C*------------------------------------------------------------------------*
122300000000     C*?IMPVID - IMPOSTAZIONI INIZIALI VIDEO
122400000000     C*------------------------------------------------------------------------*
122500000000     C     IMPVID        BEGSR
122600970825     C*
122700010410     C     *LIKE         DEFINE    §5SFSC        WFSC
122800980403     C     *LIKE         DEFINE    §5SDPS        WSPD
122900020701     C     *LIKE         DEFINE    §5SDFD        WDFD
123000020701     C     *LIKE         DEFINE    §5SDFA        WDFA
123100020701     C     *LIKE         DEFINE    §5SDFDP       WDFDP
123200020701     C     *LIKE         DEFINE    §5SDFAP       WDFAP
123300980129     C*
123400100510     C*
123500100510     C*  controlla che il programma sia lanciato per gestire il budget
123600100510     C*   nella lista di libreria si deve trovare una libreria BUDGET*
123700100510     C                   MOVEL     'N'           v1budget          1            *GESTIONE ATTUALE
123800100511     c                   call      'TISE91C0'
123900100510     c                   parm                    lib_budget        1
124000100510     c                   parm                    manca_file        1
124100100510     C*  se trovata la libreria Budget ritorna con "S".
124200100510     c                   if        lib_budget = 'S'
124300100510     C                   MOVEL     'S'           v1budget          1            *GESTIONE ATTUALE
124400100510     c                   end
124500020528     C*---
124600020916     C*---               Z-ADD     1             KTBKUT
124700020528     C*---               MOVEL     '2U'          KTBCOD
124800020528     C*--- KEYTA2        CHAIN     TABEL00F                           99
124900020528IF  1C*--- *IN99         IFEQ      *ON                                          *TABELLA NON TROVATA
125000020528     C*---               Z-ADD     99999999      WDCC
125100020528X   1C*---               ELSE
125200020528     C*---               MOVEL     TBLUNI        DS2U
125300020528     C*---               Z-ADD     §2UDCC        WDCC                           *ULTIMA CANCELLAZION
125400020528E   1C*---               ENDIF
125500980403     C*
125600020916     C                   Z-ADD     1             KTBKUT
125700980403     C                   MOVEL     '5S'          KTBCOD
125800980403     C     KEYTA2        CHAIN     TABEL00F                           99
125900980403IF  1C     *IN99         IFEQ      *ON                                          *TABELLA NON TROVATA
126000010410     C                   MOVEL     'N'           WFSC                           *NON PERMETTO LANCIO
126100980403     C                   Z-ADD     *ZEROS        WSPD                           *% SCOSTAMENTO DELTA
126200020701     C                   Z-ADD     *ZEROS        WDFD                           *% SCOSTAMENTO FATTURATO DA MESE
126300020701     C                   Z-ADD     *ZEROS        WDFA                           *% SCOSTAMENTO FATTURATO  A MESE
126400020701     C                   Z-ADD     *ZEROS        WDFDP                          *% SCOSTAMENTO FATTURATO DA PROG
126500020701     C                   Z-ADD     *ZEROS        WDFAP                          *% SCOSTAMENTO FATTURATO  A PROG
126600020528     C                   Z-ADD     99999999      WDCC
126700980403X   1C                   ELSE
126800980403     C                   MOVEL     TBLUNI        DS5S
126900980403     C                   Z-ADD     §5SDPS        WSPD                           *% SCOSTAMENTO DELTA
127000010410     C                   Z-ADD     §5SDAC        WDAC                           *ULTIMO AGGIORNAMENT
127100010410     C                   MOVEL     §5SFSC        WFSC                           *FLAG AGGIORNAMENTO
127200020528     C                   Z-ADD     §5SDCC        WDCC                           *ULTIMA CANCELLAZION
127300020701     C                   Z-ADD     §5SDFD        WDFD                           *% SCOSTAMENTO FATTURATO DA
127400020701     C                   Z-ADD     §5SDFA        WDFA                           *% SCOSTAMENTO FATTURATO  A
127500020701     C                   Z-ADD     §5SDFDP       WDFDP                          *% SCOSTAMENTO FATTURATO DA PROG
127600020701     C                   Z-ADD     §5SDFAP       WDFAP                          *% SCOSTAMENTO FATTURATO  A PROG
127700980403E   1C                   ENDIF
127800990609     C*---
127900950210     C* DATA ULTIMO AGGIORNAMENTO - 1 MESE
128000990609     C*---
128100950210     C                   SUB       1             AGGMM
128200950210IF  1C     AGGMM         IFEQ      *ZEROS                                       *SE GENNAIO
128300950210     C                   Z-ADD     12            AGGMM                          *IMPOSTO DICEMBRE
128400950210     C                   SUB       1             AGGAA                          *DELL'ANNO PRECEDENT
128500950210E   1C                   ENDIF
128600990609     C*---
128700950210     C* DATA ULTIMA CANCELLAZIONE + 1 MESE
128800990609     C*---
128900950210     C                   ADD       1             CANMM
129000950210IF  1C     CANMM         IFEQ      13                                           *SE DICEMBRE
129100950210     C                   Z-ADD     01            CANMM                          *IMPOSTO GENNAIO
129200950210     C                   ADD       1             CANAA                          *DELL'ANNO SUCESSIVO
129300950210E   1C                   ENDIF
129400000000     C*--------------------
129500000000     C* DATE DI ELABORAZIONE
129600000000     C*--------------------
129700950210     C***
129800950210     C* AA/MM FINALE   --> ULTIMO AGGIORNAMENTO - 1 MESE
129900950210     C* AA/MM INIZIALE --> INIZIO ANNO oppure DATA ULTIMA CANCELLAZIONE + 1 MESE
130000950210     C***
130100000824     C*--------------------
130200000824     C* LANCIO
130300000824     C*--------------------
130400000824     C                   Z-ADD     AGGAM         V1CDA2                         *DATA FINE
130500990609     C                   Z-ADD     AGGAM         AAMM1             6 0          *INIZIO ANNO
130600990609     C                   MOVE      01            AAMM1
130700990609     C                   Z-ADD     CANAM         AAMM2             6 0          *ULTIMA CANCELLAZION
130800990609IF  1C     AAMM2         IFGT      AAMM1
130900990609     C                   Z-ADD     CANAM         V1CDAT
131000990609     C                   ELSE
131100000824     C                   Z-ADD     AAMM1         V1CDAT                         *DATA INIZIO
131200990609E   1C                   ENDIF
131300100510      *
131400100510      * Se elabora il Budget imposta l'anno successivo
131500100510     c                   if        lib_budget = 'S'
131600100510     C                   add       100           V1CDAT
131700100510     C                   add       100           V1CDA2
131800100510     c                   end
131900100510      *
132000030311     C                   MOVEL     'S'           V1CFPC                         *CREAZIONE FILE PC
132100030912     C                   MOVEL     'S'           V1CWPC                         *CREAZIONE FILE PC del TISE94R
132200030923     C                   MOVEL     'S'           V1CWCP                         *CREAZIONE FILE PC del TISE93R
132300980527     C*--------------------
132400000824     C* SELEZIONI COMUNI
132500980527     C*--------------------
132600980527     C                   Z-ADD     -999,9        V1CPDD
132700980527     C                   Z-ADD     +999,9        V1CPDA
132800020701     C                   IF        §5SDSS = '-'
132900020701     C                   EVAL      V1CSPD = WSPD * -1
133000020701     C                   ENDIF
133100020701     C                   IF        §5SDDS = '-'
133200020701     C                   EVAL      V1CDFD = WDFD * -1
133300020701     C                   ENDIF
133400020701     C                   IF        §5SDAS = '-'
133500020701     C                   EVAL      V1CDFA = WDFA * -1
133600020701     C                   ENDIF
133700020701     C                   IF        §5SDDSP = '-'
133800020701     C                   EVAL      V1CDFDP = WDFDP * -1
133900020701     C                   ENDIF
134000020701     C                   IF        §5SDASP = '-'
134100020701     C                   EVAL      V1CDFAP = WDFAP * -1
134200020701     C                   ENDIF
134300100510     C                   MOVEL     'T'           V1CIOE                         DELTA ITALIA/ESTERO
134400011204     C                   MOVEL     'U'           V1CSCE                         DETTAGLIO X CLIENTE
134500000824     C                   MOVEL     'N'           V1CPOS
134600000000     C*--------------------
134700000824     C* SELEZIONI STATISTICHE
134800000000     C*--------------------
134900000000     C                   MOVEL     'S'           V1CDCK                         DELTA COMM.LE CLIENT
135000000000     C                   MOVEL     'S'           V1CDCC                         DELTA COMM.LE COMM.L
135100000000     C                   MOVEL     'S'           V1CDCF                         DELTA COMM.LE FILIAL
135200000000     C                   MOVEL     'S'           V1CDFF                         DELTA FILIALE
135300010618     C                   MOVEL     'N'           V1CSIN                         SINTETICA
135400020404     C                   MOVEL     'N'           V1CNTW                         ROTTURA NETWORK ESTERI
135500100510     C*
135600000000     C                   ENDSR
135700951220     C*------------------------------------------------------------------------*
135800951220     C*?X12PRE - CALCOLA 12 MESI PRECEDENTI ALLA DATA
135900951220     C*------------------------------------------------------------------------*
136000951220     C     X12PRE        BEGSR
136100951220     C*
136200951220     C*--------------------
136300951220     C* DATA FINALE PRECEDENTE = DATA INIZIALE RICHIESTA - 1 MESE
136400951220     C*--------------------
136500951220     C                   Z-ADD     PARAAI        ANPAAF                         *ANNO FINALE
136600951220     C     PARMMI        SUB       1             ANPMMF                         *MESE FINALE
136700951220IF  1C     ANPMMF        IFEQ      *ZEROS                                       *SE ERA GENNAIO
136800951220     C                   Z-ADD     12            ANPMMF                         *IMPOSTO DICEMBRE
136900951220     C     PARAAI        SUB       1             ANPAAF                         *DELL'ANNO PRECEDENT
137000951220E   1C                   ENDIF
137100951220     C*--------------------
137200951220     C* DATA INIZIALE PRECEDENTE = DATA FINALE PRECEDENTE - 12 MESI
137300951220     C*--------------------
137400990609     C                   Z-ADD     ANPAAF        WAAF              4 0
137500951220     C                   Z-ADD     ANPMMF        WMMF              2 0
137600951220     C                   Z-ADD     *ZEROS        J                 4 0
137700951220     C                   Z-ADD     WAAF          ANPAAI                         *ANNO INIZIALE
137800951220DO  1C     WMMF          DOUEQ     *ZEROS
137900951220     C                   ADD       1             J
138000951220     C                   Z-ADD     WMMF          ANPMMI                         *MESE INIZIALE
138100951220     C                   SUB       1             WMMF
138200951220E   1C                   ENDDO
138300951220IF  1C     J             IFLT      12
138400951220     C                   Z-ADD     12            WMMF
138500951220     C                   SUB       1             WAAF
138600951220     C                   Z-ADD     WAAF          ANPAAI                         *ANNO INIZIALE
138700951220DO  2C     J             DOUEQ     12
138800951220     C                   ADD       1             J
138900951220     C                   Z-ADD     WMMF          ANPMMI                         *MESE INIZIALE
139000951220     C                   SUB       1             WMMF
139100951220E   2C                   ENDDO
139200951220E   1C                   ENDIF
139300951220     C*
139400951220     C                   ENDSR
139500020916     C*--------------------------------------------------------------------------------------------*
139600020916     C* REPERISCE I DATI UTENTE
139700020916     C*--------------------------------------------------------------------------------------------*
139800020916     C     REPDATIUTE    BEGSR
139900020916     C*
140000020916     C* INIZIALIZZA VARIABILI DI WRK
140100020916     C                   CLEAR                   TIBS34DS
140200020916     C                   CLEAR                   AZUTEDS
140300020916     C                   CLEAR                   DDATIUTE
140400020916     C*
140500020916     C     *DTAARA       DEFINE    §azute        azuteds
140600020916     C     *DTAARA       DEFINE    §datiute      ddatiute
140700020916     C                   IN(E)     *DTAARA
140800020916if  1C                   IF        %Error
140900020916     c                   EVAL      I34Tla = 'L'
141000020916     C                   CALL      'TIBS34R'
141100020916     C                   PARM                    Tibs34Ds
141200020916     C                   IN        *DTAARA
141300020916e   1C                   ENDIF
141400020916     C*
141500020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
141600020916     C                   MOVEL(P)  RSUT          DSFIRS           20
141700020916     C*
141800020916     C                   ENDSR
141900000000     C*------------------------------------------------------------------------*
142000000000     C*?*INZSR - ROUTINE INIZIALE
142100000000     C*------------------------------------------------------------------------*
142200000000     C     *INZSR        BEGSR
142300000000     C*
142400000000     C* RICEVIMENTO PARAMETRI
142500000000     C     *ENTRY        PLIST
142600000000     C                   PARM                    KPJBA
142700000000     C*
142800000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
142900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
143000000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
143100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
143200000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
143300000000     C*
143400000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
143500000000     C     KEYORG        KLIST
143600000000     C                   KFLD                    KORFIL
143700000000     C*
143800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
143900000000     C     KEYTAB        KLIST
144000000000     C                   KFLD                    KTBKUT
144100000000     C                   KFLD                    KTBCOD
144200000000     C                   KFLD                    KTBKEY
144300000000     C*
144400000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
144500000000     C     KEYTA2        KLIST
144600000000     C                   KFLD                    KTBKUT
144700000000     C                   KFLD                    KTBCOD
144800000000     C*
144900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
145000000000     C                   EXSR      INZSTD
145100000000     C*
145200000000     C* IMPOSTAZIONI INIZIALI VIDEO
145300000000     C                   EXSR      IMPVID
145400000000     C*
145500000000     C* EMETTO PRIMO FORMATO VIDEO
145600000000     C                   EXSR      CARV1
145700000000     C*
145800000000     C                   ENDSR
145900950418     C*------------------------------------------------------------------------*
146000950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
146100950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
146200950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
146300000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
146400000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
146500000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
146600950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
146700950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
146800950418     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
146900000000     C*------------------------------------------------------------------------*
147000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
147100000000     C*------------------------------------------------------------------------*
147200000000     C     INZSTD        BEGSR
147300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
147400000000     C                   CALL      'XKEYS'
147500000000     C                   PARM                    DSKEYS
147600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
147700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
147800000000     C     XMSGPA        PLIST
147900000000     C                   PARM                    DSMSMS
148000000000     C                   PARM                    DSMSPG
148100000000     C                   PARM                    DSMSKE
148200000000     C                   PARM                    DSMSER
148300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
148400020116     C                   MOVEL     'TISE90R'     DSMSPG                         *NOME PGM CHIAMANTE
148500020116     C                   MOVEL     'TISE90R'     VMCQUE                         *CODA MESSAGGI DESTI
148600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
148700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
148800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
148900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
149000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
149100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
149200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
149300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
149400000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
149500000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
149600000000     C* LANCIO ROUTINE PER VERIFICARE SE
149700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
149800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
149900020916     C                   EXSR      REPDATIUTE
150000000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
150100000000     C                   BITOFF    '01234567'    POSMI1            1
150200000000     C                   BITOFF    '01234567'    POSMI2            1
150300000000     C                   BITOFF    '01234567'    POSMA1            1
150400000000     C                   BITOFF    '01234567'    POSMA2            1
150500000000     C                   BITON     '34'          POSMI1            1            *18
150600000000     C                   BITON     '7'           POSMI2            1            *01
150700000000     C                   BITON     '34'          POSMA1            1            *18
150800000000     C                   BITON     '0'           POSMA2            1            *80
150900000000     C                   MOVE      POSMI2        POSMIN            2
151000000000     C                   MOVEL     POSMI1        POSMIN
151100000000     C                   MOVE      POSMA2        POSMAX            2
151200000000     C                   MOVEL     POSMA1        POSMAX
151300000000     C                   ENDSR
151400000000     C*------------------------------------------------------------------------*
151500000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
151600000000     C*------------------------------------------------------------------------*
151700000000     C     FUNSTD        BEGSR
151800000000     C                   SELECT
151900000000     C     KEY           WHENEQ    F3
152000000000     C                   EXSR      F03STD
152100000000     C     KEY           WHENEQ    F12
152200000000     C                   EXSR      F12STD
152300000000     C     KEY           WHENEQ    F24
152400000000     C                   EXSR      F24STD
152500000000     C                   OTHER
152600000000     C                   MOVEL     MSG(2)        $MSG
152700000000     C                   EXSR      CARMSG
152800000000     C                   ENDSL
152900000000     C                   ENDSR
153000000000     C*------------------------------------------------------------------------*
153100000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
153200000000     C*------------------------------------------------------------------------*
153300000000     C     F03STD        BEGSR
153400000000     C*
153500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
153600000000     C                   MOVEL     '1'           $FINE
153700000000     C*
153800000000     C                   ENDSR
153900000000     C*------------------------------------------------------------------------*
154000000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
154100000000     C*------------------------------------------------------------------------*
154200000000     C     F12STD        BEGSR
154300000000     C*
154400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
154500000000     C                   MOVEL     '1'           $FINE
154600000000     C*
154700000000     C                   ENDSR
154800000000     C*------------------------------------------------------------------------*
154900000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
155000000000     C*------------------------------------------------------------------------*
155100000000     C     F24STD        BEGSR
155200000000     C*
155300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
155400000000     C                   EXSR      OUTFUN
155500000000     C*
155600000000     C                   ENDSR
155700000000     C*------------------------------------------------------------------------*
155800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
155900000000     C*          CODA MESSAGGI DEL PGM
156000000000     C*------------------------------------------------------------------------*
156100000000     C     CARMSG        BEGSR
156200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
156300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
156400000000     C     MSGULT        IFEQ      0
156500000000     C                   MOVEL     '1'           *IN40
156600020116     C                   WRITE     SE90DVK
156700000000     C                   MOVEL     '0'           *IN40
156800000000     C                   ENDIF
156900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
157000000000     C                   MOVEL     $MSG          DSMSMS
157100000000     C                   CALL      'XMSG'        XMSGPA
157200000000     C     DSMSER        IFEQ      0
157300000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
157400000000     C                   ENDIF
157500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
157600000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
157700020116     C                   WRITE     SE90DVM                                      *SFL MESS
157800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
157900000000     C                   MOVEL     *BLANKS       $MSG
158000000000     C                   ENDSR
158100000000     C*------------------------------------------------------------------------*
158200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
158300000000     C*------------------------------------------------------------------------*
158400000000     C     OUTMSG        BEGSR
158500000000     C* VISUALIZZAZIONE MESSAGGIO
158600000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
158700020116     C                   WRITE     SE90DVK                                      *CONTR MESS
158800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
158900000000     C                   Z-ADD     *ZEROS        MSGULT
159000000000     C                   ENDSR
159100000000     C*------------------------------------------------------------------------*
159200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
159300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
159400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
159500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
159600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
159700000000     C*------------------------------------------------------------------------*
159800000000     C     CARFUN        BEGSR
159900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
160000000000     C     1             DO        31            I                 5 0          --- 1 -->
160100000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
160200000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
160300000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
160400020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
160500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
160600000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
160700000000     C                   ELSE                                                   *INTERROG.
160800000000     C                   MOVEL     FUNASI        FAT(I)
160900000000     C                   ENDIF
161000000000     C                   ELSE                                                   <-- 2 --> *FILIALE
161100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
161200000000     C                   MOVEL     FUNAFG        FAT(I)
161300000000     C                   ELSE                                                   *INTERROG.
161400000000     C                   MOVEL     FUNAFI        FAT(I)
161500000000     C                   ENDIF
161600000000     C                   ENDIF                                                  <-- 2 ---
161700000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
161800000000     C                   MOVEL     USER          WEDP              3            *PROFILO
161900000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
162000000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
162100000000     C                   ELSE
162200000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
162300000000     C                   ENDIF
162400000000     C                   ENDIF
162500000000     C                   ENDDO                                                  <-- 1 ---
162600000000     C                   ENDSR
162700000000     C*------------------------------------------------------------------------*
162800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
162900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
163000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
163100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
163200000000     C*------------------------------------------------------------------------*
163300000000     C     OUTFUN        BEGSR
163400000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
163500000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
163600000000     C                   ENDDO
163700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
163800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
163900000000     C     1             DO        31            I
164000000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
164100000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
164200000000     C                   ADD       1             FUNNAT
164300000000     C                   ENDIF
164400000000     C                   ENDDO
164500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
164600000000     C     FUNNAT        IFGT      8
164700000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
164800000000     C                   ENDIF
164900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
165000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
165100000000     C                   Z-ADD     8             FUNVIS            5 0
165200000000     C     FAT(23)       IFEQ      'S'
165300000000     C                   SUB       1             FUNVIS
165400000000     C                   ENDIF
165500000000     C     FAT(24)       IFEQ      'S'
165600000000     C                   SUB       1             FUNVIS
165700000000     C                   ENDIF
165800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
165900000000     C                   Z-ADD     *ZEROS        W                 5 0
166000000000     C                   ADD       1             FUNULT
166100000000     C     FUNULT        IFGT      31
166200000000     C                   Z-ADD     1             FUNULT
166300000000     C                   ENDIF
166400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
166500000000     C     FUNULT        DO        31            I                              --- 1 -->
166600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
166700000000     C     W             IFLT      FUNVIS                                       --- 2 -->
166800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
166900000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
167000000000     C     FDE(I)        ANDNE     *BLANKS
167100000000     C     I             IFEQ      23                                           *TOLGO F23
167200000000     C     I             OREQ      24                                           *E F24
167300000000     C                   ELSE
167400000000     C                   ADD       1             W
167500000000     C                   MOVEL     FDE(I)        FUV(W)
167600000000     C                   ENDIF
167700000000     C                   ENDIF                                                  <-- 3 ---
167800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
167900000000     C                   Z-ADD     I             FUNULT
168000000000     C                   ENDIF                                                  <-- 2 ---
168100000000     C                   ENDDO                                                  <-- 1 ---
168200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
168300000000     C                   MOVEL     FUV(1)        VFD001
168400000000     C                   MOVEL     FUV(2)        VFD002
168500000000     C                   MOVEL     FUV(3)        VFD003
168600000000     C                   MOVEL     FUV(4)        VFD004
168700000000     C                   MOVEL     FUV(5)        VFD005
168800000000     C                   MOVEL     FUV(6)        VFD006
168900000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
169000000000     C     FAT(24)       ANDEQ     'N'
169100000000     C                   MOVEL     FUV(7)        VFD007
169200000000     C                   MOVEL     FUV(8)        VFD008
169300000000     C                   ELSE                                                   <-- 1 -->
169400000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
169500000000     C     FAT(24)       ANDEQ     'S'
169600000000     C                   MOVEL     FDE(23)       VFD007
169700000000     C                   MOVEL     FDE(24)       VFD008
169800000000     C                   ELSE                                                   <-- 2 -->
169900000000     C                   MOVEL     FUV(7)        VFD007
170000000000     C     FAT(23)       IFEQ      'S'
170100000000     C                   MOVEL     FDE(23)       VFD008
170200000000     C                   ELSE
170300000000     C                   MOVEL     FDE(24)       VFD008
170400000000     C                   ENDIF
170500000000     C                   ENDIF                                                  <-- 2 ---
170600000000     C                   ENDIF                                                  <-- 1 ---
170700000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
170800000000     C                   ENDSR
170900000000     C*------------------------------------------------------------------------*
171000000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
171100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
171200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
171300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
171400000000     C*------------------------------------------------------------------------*
171500000000     C     CONFUN        BEGSR
171600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
171700000000     C     1             DO        31            I                              --- 1 -->
171800000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
171900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
172000000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
172100000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
172200000000     C                   EXSR      CARMSG
172300000000     C                   MOVEL     '1'           $ERR
172400000000     C                   ENDIF                                                  <-- 3 ---
172500000000     C                   ENDIF                                                  <-- 2 ---
172600000000     C                   ENDDO                                                  <-- 1 ---
172700000000     C                   ENDSR
172800000000**   SCHIERA: MSG  (MESSAGGI)
172900020116TISE90R- Tasto funzionale non valido.                                          1
173000020116TISE90R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
173100020116TISE90R- Opzione non valida.                                                   3
173200020116TISE90R- Opzione abilitata ma non gestita. Avvertire CED.                      4
173300020116TISE90R- Inizio elenco.                                                        5
173400020116TISE90R- Fine elenco.                                                          6
173500020116TISE90R- Anno/Mese obbligatorio                                                7
173600020116TISE90R- Anno/Mese errato                                                      8
173700020116TISE90R- ATTENZIONE !! Data superiore all' ultimo agg. saldi da contabilità    9
173800020116TISE90R- 'S' per richiedere il Delta di Filiale, 'N' per non richiederlo      10
173900020116TISE90R- Anno/Mese finale inferiore all'Anno/Mese iniziale                    11
174000020116TISE90R- Codice filiale errato o inesistente                                  12
174100020116TISE90R- Codice area errato o inesistente                                     13
174200020116TISE90R- Codice divisione errato o inesistente                                14
174300020116TISE90R- 'S' per richiedere il Delta di Area, 'N' per non richiederlo         15
174400020116TISE90R- 'S' per richiedere il Delta di Divisione, 'N' per non richiederlo    16
174500020116TISE90R- Immettere solo una selezione per filiale, area o divisione           17
174600020116TISE90R- 'S' per richiedere il Delta Commerciale, 'N' per non richiederlo     18
174700020116TISE90R- Impossibile richiedere più di 12 mesi                                19
174800020116TISE90R- Codice commerciale errato o inesistente                              20
174900020116TISE90R- Saldi non presenti: data inferiore all'ultima cancellazione          21
175000020116TISE90R- ATTENZIONE !! Saldi cliente non ancora aggiornati da contabilità     22
175100020116TISE90R- Immettere solo una selezione per commerciale o divisione             23
175200020116TISE90R- Richiedere almeno un statistica                                      24
175300020116TISE90R- Impossibile parzializzare statistiche non selezionate                25
175400020116TISE90R- Il codice commerciale non è di raggruppamento                        26
175500020325TISE90R- Impossibile lanciare la statistica su anni differenti                27
175600020116TISE90R- Tipo servizio errato                                                 28
175700020116TISE90R- Immettere il tipo di dati che si vuole in statistica                 29
175800050224TISE90R- Se Italia+Estero non è possibile generare File PC                    30
175900020116TISE90R- Codice qualità cliente errato.                                       31
176000020116TISE90R- La percentuale non può essere maggiore di 100                        32
176100020116TISE90R- Non si possono lanciare le statistiche di gruppo in SDI !!!          33
176200020116TISE90R- La percentuale A deve essere maggione/uguale della percentuale DA    34
176300020116TISE90R- Immettere uno dei valori possibili                                   35
176400050228TISE90R- Per creare file PC Selezionare Stat.Grp.(A) Delta x Divisione        36
176500050228TISE90R- Per creare file PC Selezionare Stat.Grp.(B) Delta Commerciale        37
176600050228TISE90R- Per creare file PC Selezionare Stat.Grp.(A) Delta x Filiale          38
176700020116TISE90R- 39
176800020116TISE90R- 40
176900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
177000000000F1    -                   -    NN     NN
177100000000F2    -                   -    NN     NN
177200000000F3    -F3=Fine            -    SS     SS
177300000000F4    -                   -    NN     NN
177400000000F5    -                   -    NN     NN
177500000000F6    -F6=Lancio          -    SS     SS
177600000000F7    -                   -    NN     NN
177700000000F8    -                   -    NN     NN
177800000000F9    -                   -    NN     NN
177900000000F10   -                   -    NN     NN
178000000000F11   -                   -    NN     NN
178100000000F12   -F12=Ritorno        -    SS     SS
178200000000F13   -                   -    NN     NN
178300000000F14   -                   -    NN     NN
178400000000F15   -                   -    NN     NN
178500000000F16   -                   -    NN     NN
178600000000F17   -                   -    NN     NN
178700000000F18   -                   -    NN     NN
178800000000F19   -                   -    NN     NN
178900000000F20   -                   -    NN     NN
179000000000F21   -                   -    NN     NN
179100000000F22   -                   -    NN     NN
179200000000F23   -                   -    NN     NN
179300000000F24   -F24=Altri tasti    -    NN     NN
179400000000ENTER -                   -    SS     SS
179500000000ROLLUP-                   -    NN     NN
179600000000ROLLDN-                   -    NN     NN
179700000000HELP  -                   -    NN     NN
179800000000PRINT -                   -    NN     NN
179900000000BSPACE-                   -    NN     NN
180000000000CLEAR -                   -    NN     NN
