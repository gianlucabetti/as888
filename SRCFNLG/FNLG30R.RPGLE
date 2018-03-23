000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200941207     H* FNLG30R *----------------------------------------------------*
000300950316     H*         INTERROGAZIONE GIACENZE PARTENZE/ARRIVI              *
000400950316     H*--------------------------------------------------------------*
000500900509     H*             - RICERCHE X INTERROGAZIONI                      *
000600900418     H*  03 FINE LAVORO                                              *
000700900418     H*  19 IND. ERRORE GENERICO VIDEO                               *
000800900418     H*  21 IND. COMODO                                              *
000900900509     H*  40 RICERCA X NR. GIACENZA                                   *
001000900509     H*  41 RICERCA X NR. SPEDIZIONE                                 *
001100950316     H*  42 RICERCA X CODICE MITTENTE (SOLO PARTENZE)                *
001200950316     H*  43 RICERCA X FILIALE (SOLO ARRIVI)                          *
001300941207     H*  60 ON-INTERROG. GIAC.    OFF-INTERR. STORICO                *
001400941207     H*  80 ON-GIAC. IN ARRIVO    OFF.GIAC.IN PARTENZA               *
001500900418     H*--------------------------------------------------------------*
001600941207     FFNLG30D   CF   E             WORKSTN
001700941207     F                                     SFILE(LG30S02:NRR)
001800941207     FFNARB01L  IF   E           K DISK    USROPN
001900941207     FFNBLP01L  IF   E           K DISK    USROPN
002000990716     FTITAS30C  IF   E           K DISK    USROPN
002100050307     Ftigcp07L  IF   E           K DISK    USROPN
002200050308     F                                     RENAME(tigcp000:tigcp7)
002300050418     Ftigcp26L  IF   E           K DISK    USROPN
002400950316     D*----------------------------------------------------------------
002500900509     D KSC§            S              1    DIM(7)
002600161123     D ERR             S             70    DIM(14) CTDATA PERRCD(1)
002700950315     D L1              S              3  0 DIM(30)
002800950315     D*----------------------------------------------------------------
002900990830     D TNSB50        E DS                  EXTNAME(TNSB50DS)
003000950316     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE (FNLS05R)
003100161118     D*PARA04          DS                  INZ
003200161118     D* P04AAS                 1      4  0
003300161118     D* P04LNP                 5      7  0
003400161118     D* P04NRS                 8      9  0
003500161118     D* P04NSP                10     16  0
003600161118     D* P04FLG                31     31
003700970205     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
003800161118     D* P04GIA                48     48
003900950316      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
004000161118     D* P04CLI               143    143
004100161118     D*PARA05          DS                  INZ
004200161118     D* P05AAS                 1      4  0
004300161118     D* P05LNP                 5      7  0
004400161118     D* P05NRS                 8      9  0
004500161118     D* P05NSP                10     16  0
004600161118     D* P05F03                17     17
004700161118     D* P05FLG                18     18
004800161118     D* P05SIM                44     46  0
004900941213     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
005000161118     D* P05GIA                48     48
005100941213     D* DATI X RICHIAMO PGM INTERR. BOLLE ARRIVI
005200161118     D*PARAM1          DS                  INZ
005300161118     D* PA1AAS                14     17  0
005400161118     D* PA1LNP                18     20  0
005500161118     D* PA1NRS                21     22  0
005600161118     D* PA1NSP                23     29  0
005700161118     D* PA1F03                30     30
005800161118     D* PA1FLG                31     31
005900941213     D* SE =1 NON RICH. FNLG30R IN MODO RICORSIVO DA INTERR.BOLLE
006000161118     D* PA1GIA               144    144
006001161118     d fnlru6ds      e ds
006002161118      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
006003161118     D PARAMu6ds       DS
006004161118     d* CAMPI DI OUTPUT:
006005161118     d* . Key spedizione
006006161118     D  pa1AAS                14     17  0
006007161118     D  pa1LNP                18     20  0
006008161118     D  pa1NRS                21     22  0
006009161118     D  pa1NSP                23     29  0
006010161118     d* . Flag '1'=Premuto F3=Fine
006011161118     D  PA1F03                30     30
006012161118     d* CAMPI DI INPUT:
006013161118     d* . Modalità di richiamo
006014161118     D  PA1FLG                31     31
006015161118     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
006016161118     d*                                in interrogazione della bolla ed evitare
006017161118     d*                                errore di chiamata ricorsiva
006018161118     D  PA1GIA               144    144
006019161118     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
006020161118     d  pa1rma               149    163
006021161118     d  pa1xco               164    164
006022161118     d* non utilizzato
006023161118     d  pa1flo               165    256
006024161118      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
006025161118     D PARAMU6ds1      DS
006026161118     d** INPUT/OUTPUT (se richiesta la selezione bolla)
006027161118     D  pa1AAS_               14     17  0
006028161118     D  pa1LNP_               18     20  0
006029161118     D  pa1NRS_               21     22  0
006030161118     D  pa1NSP_               23     29  0
006031161118     d** OUTPUT
006032161118     D  PA1F03_               30     30
006033161118     d** INPUT
006034161118     D  PA1FLG_               31     31
006035161118     D*    SE RICH DA PGM GIAC. = 1
006036161118     D  PA1GIA_              144    144
006037161118     d** OUTPUT
006038161118     d  pa1rma_              149    163
006039161118     d  pa1xco_              164    164
006040161118     d** INPUT
006041161118     d*   Tipo lancio: "C" -           CHIUSO CON LR
006042161118     d*                "L" - ELABORO E CHIUDO CON LR
006043161118     d*                " " - ELABORO E CHIUDO IN RETRN
006044161118     d  pa1tla_              165    165
006045161118     d  pa1flo_              166    256
006100900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
006200900418     D  TCU                  398    697
006300900509     D                                     DIM(50)
006400900418     D  KCU                  698    847P 0
006500900509     D                                     DIM(50)
006600900509     D                                     PACKEVEN
006700990716     D TCUDS           DS
006800900509     D  F1                     1      1
006900900509     D  F3                     3      3
007000900509     D  F2                     2      2
007100900509     D  F4                     4      4
007200900509     D  F56                    5      6
007300900418     D CNCR80        E DS
007400900418     D KPJBA         E DS
007500941207     D FNLG30        E DS                  EXTNAME(FNLG30DS)
007600960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
007700941207     D WLBDA8          DS
007800941207     D  G08DAT                 1      8  0
007900941207     D  G08INV                 9     16  0
008000941207     D  G08ERR                17     17
008100941207     D  G08TGI                18     22  0
008200941207     D                SDS
008300941207     D  §PGM                   1     10
008400950315     D* DS PER TRUL06R - CARICAMENTO £X
008500950315     D DSUL06        E DS                  EXTNAME(TRUL06DS)
008600950315     D  T06                    1     90  0
008700950315     D                                     DIM(30)
008800950315     C*----------------------------------------------------------------
008900941213     C* SE HO GIA' IMPOSTATI IL NR.SPEDIZIONE è
009000941213     C* RICHIAMATO DA INTERROGAZ. BOLLE
009100941213     C     D30AAS        IFNE      *ZEROS
009200941213     C     D30LNP        ANDNE     *ZEROS
009300941213     C     D30NSP        ANDNE     *ZEROS
009400941213     C                   MOVE      *ZEROS        VAGC
009500941213     C                   MOVE      *ZEROS        VFGC
009600941213     C                   MOVE      *ZEROS        VNGC
009700941213     C                   MOVE      D30AAS        VAAS
009800941213     C                   MOVE      D30LNP        VLNP
009900941213     C                   MOVE      D30NRS        VNRS
010000941213     C                   MOVE      D30NSP        VNSP
010100941213     C                   EXSR      RICINT
010200941213     C*
010300941213     C                   ELSE
010400941213     C* EMETTO VIDEATA X RICHIEDERE IL NR.SPEDIZIONE
010500900418     C     INIZIO        TAG
010600900418     C                   EXSR      PULVID
010700900418     C     EMFG01        TAG
010800941209     C                   SETOFF                                       29
010900941207     C                   EXFMT     LG30D01
011000900418     C*  CMD 03  FINE LAVORO
011100941207     C     *INKC         CABEQ     '1'           FINE
011200941212     C*  CMD 07  INTERROGAZ. BOLLE
011300941212     C     *INKG         IFEQ      *ON
011400941212     C                   EXSR      RICHBL
011500941212     C                   GOTO      EMFG01
011600941212     C                   ENDIF
011700900509     C                   EXSR      CTRVID
011800941209     C   28
011900941209     COR 29              GOTO      EMFG01
012000900509     C*
012100941213     C     *IN40         IFEQ      '1'
012200941207     C                   EXSR      RICINT
012300950112     C   28              GOTO      EMFG01
012400950112     C                   GOTO      INIZIO
012500941213     C                   END
012600941209      *
012700941213     C     *IN41         IFEQ      '1'
012800941207     C                   EXSR      RICINT
012900950112     C   28              GOTO      EMFG01
013000950112     C                   GOTO      INIZIO
013100941213     C                   END
013200941209      *
013300941213     C     *IN42         IFEQ      '1'
013400950315     C     *IN43         OREQ      '1'
013500900509     C                   EXSR      GESSFL
013600941209     C* SE NON TROVO  RECORD RITORNO A EMFG01
013700941209     C     NRR           IFEQ      *ZEROS
013800941209     C                   SETON                                        28
013900941209     C                   MOVEL     ERR(10)       $MSG
014000941209     C                   GOTO      EMFG01
014100941209     C                   ENDIF
014200900509     C*
014300950316     C* EMETTO IL SFL
014400900509     C     EMFGSF        TAG
014500941207     C                   WRITE     LG30Z02
014600941207     C                   EXFMT     LG30C02
014700950316     C                   SETOFF                                       28
014800900418     C     *IN12         CABEQ     '1'           INIZIO
014900950316     C*
015000950316     C* PER ROLLUP CARICO UN'ALTRA PAGINA DI SFL
015100950316     C   26              EXSR      CARIC
015200950316     C*
015300900509     C* LETTURA SFL
015400900509     C                   EXSR      LETSFL
015500911120     C                   GOTO      EMFGSF
015600941213     C                   ENDIF
015700941213     C*
015800941213     C                   ENDIF
015900941213     C*
016000900418     C     FINE          TAG
016001161118
016100900418     C                   SETON                                        LR
016200950316     C**************************************************************************
016300941212     C*  Routine per gestione richiamo interrogazione bolle
016400950316     C**************************************************************************
016500941212     C     RICHBL        BEGSR
016600941212     C*
016700941212     C                   MOVEL     KPJBU         §KPJBU
016800990716     C* SEDE
016900990716     C     SIMFEL        IFEQ      0
017000990716     C* RICHIAMO INTERROGAZIONE BOLLE
017100990716     C                   EXSR      INTBOL
017200990716     C                   ELSE
017201161118     C     Vnsp          IFGT      0
017202161118     C                   CLEAR                   paramu6ds1
017203161118     C                   Z-ADD     Vaas          pa1AAS_
017204161118     C                   Z-ADD     Vlnp          Pa1LNP_
017205161118     C                   Z-ADD     Vnrs          Pa1NRS_
017206161118     C                   Z-ADD     Vnsp          Pa1NSP_
017207161118     C                   eval      PA1GIA_='1'
017208161118     C                   eval      PA1tla_='L'
017209161118     C                   MOVEL     paramu6ds1    KPJBU
017210161118     C                   CALL      'FNLRU6R1'
017211161118     C                   PARM                    KPJBA
017217161118     C*
017218161118     C                   ELSE
017219161118     C*
017220161118     C                   CLEAR                   PARAmu6ds
017221161118     c                   clear                   fnlru6ds
017222161118     C                   MOVEL     '2'           Pa1FLG
017223161118     C                   eval      PA1GIA='1'
017224161118     C                   MOVEL     PARAmu6ds     KPJBU
017225161118     c                   if        D30POA='P'
017226161118     c                   eval      ilru6lp1=simpou
017227161118     c                   else
017228161118     c                   eval      ilru6lna=simpou
017229161118     c                   endif
017230161118     C                   CALL      'FNLRU6R'
017231161118     C                   PARM                    KPJBA
017232161118     c                   parm                    fnlru6ds
017233161118     C                   MOVEL     KPJBU         PARAMu6ds
017234161118     C     Pa1F03        IFEQ      *BLANKS
017235161118     C                   Z-ADD     Pa1AAS        VAAS
017236161118     C                   Z-ADD     Pa1LNP        VLNP
017237161118     C                   Z-ADD     Pa1NRS        VNRS
017238161118     C                   Z-ADD     Pa1NSP        VNSP
017239161118     C                   ENDIF
017240161118     C                   ENDIF
017300950316     C*
017400161118     C*    D30POA        IFEQ      'P'
017500990830     C*
017600161118     C*    VNSP          IFGT      0
017700161118     C*                  CLEAR                   PARA05
017800161118     C*                  Z-ADD     VAAS          P05AAS
017900161118     C*                  Z-ADD     VLNP          P05LNP
018000161118     C*                  Z-ADD     VNRS          P05NRS
018100161118     C*                  Z-ADD     VNSP          P05NSP
018200161118     C*                  MOVEL     '1'           P05GIA
018300161118     C*                  MOVEL     PARA05        KPJBU
018400161118     C*                  CALL      'FNLS05R'
018500161118     C*                  PARM                    KPJBA
018600161118     C*                  MOVEL     KPJBU         PARA05
018700950316     C*
018800161118     C*                  ELSE
018900950316     C*
019000161118     C*                  CLEAR                   PARA04
019100161118     C*                  MOVEL     '1'           P04FLG
019200161118     C*                  MOVEL     '1'           P04GIA
019300161118     C*                  MOVEL     PARA04        KPJBU
019400161118     C*                  CALL      'FNLS04R'
019500161118     C*                  PARM                    KPJBA
019600161118     C*                  MOVEL     KPJBU         PARA04
019700161118     C*    P04NSP        IFGT      *ZERO
019800161118     C*                  Z-ADD     P04AAS        VAAS
019900161118     C*                  Z-ADD     P04LNP        VLNP
020000161118     C*                  Z-ADD     P04NRS        VNRS
020100161118     C*                  Z-ADD     P04NSP        VNSP
020200161118     C*                  ENDIF
020300950316     C*
020400161118     C*                  END
020500950316     C*
020600161118     C*                  ELSE
020700950316     C*
020800161118     C*                  Z-ADD     VAAS          PA1AAS
020900161118     C*                  Z-ADD     VLNP          PA1LNP
021000161118     C*                  Z-ADD     VNRS          PA1NRS
021100161118     C*                  Z-ADD     VNSP          PA1NSP
021200161118     C*                  MOVEL     ' '           PA1F03
021300161118     C*                  MOVEL     '2'           PA1FLG
021400161118     C*                  MOVEL     '1'           PA1GIA
021500161118     C*    PA1NSP        IFGT      0
021600161118     C*                  MOVEL     '1'           PA1FLG
021700161118     C*                  END
021800161118     C*                  MOVEL     PARAM1        KPJBU
021900161118     C*                  CALL      'FNLR36R'
022000161118     C*                  PARM                    KPJBA
022100161118     C*                  MOVEL     KPJBU         PARAM1
022200161118     C*    PA1F03        IFNE      '1'
022300161118     C*                  Z-ADD     PA1AAS        VAAS
022400161118     C*                  Z-ADD     PA1LNP        VLNP
022500161118     C*                  Z-ADD     PA1NRS        VNRS
022600161118     C*                  Z-ADD     PA1NSP        VNSP
022700161118     C*                  END
022800161118     C*                  END
022900941212     C*
023000990830     C                   END
023100990830     C*
023200941212     C                   MOVEL     §KPJBU        KPJBU
023300950316     C*
023400941212     C                   ENDSR
023500990716      *---------------------------------------------------------------*
023600990716      *  INTBOL: INTERROGAZIONE BOLLE                                 *
023700990716      *---------------------------------------------------------------*
023800990716     C     INTBOL        BEGSR
023900990716     C*----------------------------------------------------*
024000990716     C                   SETON                                        30
024100990830     C                   MOVEL     KPJBU         KPJBUS          256
024200990830     C                   MOVEL     *BLANK        KPJBU
024300990830     C                   CLEAR                   TNSB50
024400990830     C* SE IMPOSTATO IL NUMERO  DELLA SPEDIZIONE RICHIAMO L'INTERROGAZ.
024500990830     C* IMPOSTANDOLO, ALTRIMENTI RICHIAMO IL PGM IN INTERR./SELEZIONE
024600990830     C     VNSP          IFEQ      0
024700990830     C                   MOVEL     'G01'         I50OP0
024800990830     C                   MOVEL     TNSB50        KPJBU
024900990830     C                   CALL      'TNSB50R'
025000990830     C                   PARM                    KPJBA
025100990830     C                   MOVEL     KPJBU         TNSB50
025200990830     C                   ELSE
025300990830     C                   MOVEL     'G05'         I50OP0
025400990830     C                   MOVEL     '0'           O50F03
025500990830     C                   MOVEL     '0'           O50F12
025600990830     C                   MOVEL     '0'           O50ERR
025700990830     C                   MOVEL     *BLANKS       O50MSG
025800990830     C                   Z-ADD     VAAS          D50AAS
025900990830     C                   Z-ADD     VLNP          D50LNP
026000990830     C                   Z-ADD     VNRS          D50NRS
026100990830     C                   Z-ADD     VNSP          D50NSP
026200990830     C                   MOVEL     *BLANKS       D50TBL
026300990830     C                   MOVEL     TNSB50        KPJBU
026400990830     C                   CALL      'TNSB50R'
026500990830     C                   PARM                    KPJBA
026600990830     C                   MOVEL     KPJBU         TNSB50
026700990830     C     O50ERR        IFEQ      '1'
026800990830     C                   MOVEL     O50MSG        $MSG
026900990830     C                   SETON                                        6328
027000990830     C                   END
027100990830     C                   END
027200990830     C*
027300990830     C* Controllo se ho reimpostato il numero della spedizione
027400990830     C     D50NSP        IFNE      0
027500990830     C                   MOVE      D50AAS        VAAS
027600990830     C                   MOVE      D50LNP        VLNP
027700990830     C                   MOVE      D50NRS        VNRS
027800990830     C                   MOVE      D50NSP        VNSP
027900990830     C                   END
028000990830     C                   MOVEL     KPJBU         KPJBUS          256
028100990716     C*
028200990716     C                   ENDSR
028300950316     C**************************************************************************
028400950316     C*   P U L V I D
028500950316     C**************************************************************************
028600900418     C     PULVID        BEGSR
028700950316     C*
028800941207     C                   MOVE      ANNO          VAGC
028900941207     C                   MOVE      *ZEROS        VFGC
029000941207     C                   MOVE      *ZEROS        VNGC
029100941207     C                   MOVE      ANNO          VAAS
029200941207     C                   MOVE      *ZEROS        VLNP
029300941207     C                   MOVE      *ZEROS        VNRS
029400941207     C                   MOVE      *ZEROS        VNSP
029500941207     C                   MOVE      *BLANKS       VSCM
029600950315     C                   Z-ADD     *ZERO         VFIL
029700900510     C*
029800900510     C                   MOVE      'T'           VPARZ
029900950316     C*
030000900418     C                   ENDSR
030100950316     C**************************************************************************
030200950316     C*   CONTROLLO CAMPI IMMESSI A VIDEO
030300950316     C**************************************************************************
030400900418     C     CTRVID        BEGSR
030500950316     C*
030600950315     C                   SETOFF                                       28  54
030700941209     C                   SETOFF                                       515253
030800941209     C                   SETOFF                                       616263
030900950316     C                   SETOFF                                       404142
031000950316     C                   SETOFF                                       43
031100941207     C                   MOVEL     *BLANKS       $MSG
031200950315     C  N80VNGC          IFEQ      *ZEROS
031300941209     C     VNSP          ANDEQ     *ZEROS
031400941209     C     VSCM          ANDEQ     *BLANK
031401161123     C     Vparz         ANDne     'A'
031500161123     C                   SETON                                        28
031600161123     C                   MOVEL     ERR(14)       $MSG
031700941209     C                   GOTO      ENDCTR
031800941209     C                   ENDIF
031900941207     C* CONTROLLO CONGRUENZA DELLE SCELTE
032000941209      *
032100941207     C     VNGC          IFGT      *ZEROS
032200941207     C     VNSP          ANDGT     *ZEROS
032300941207     C                   SETON                                        28
032400941207     C                   MOVEL     ERR(1)        $MSG
032500941207     C                   GOTO      ENDCTR
032600941207     C                   ENDIF
032700941207     C     VNGC          IFGT      *ZEROS
032800941207     C     VSCM          ANDGT     *BLANK
032900941207     C                   SETON                                        28
033000941207     C                   MOVEL     ERR(2)        $MSG
033100941207     C                   GOTO      ENDCTR
033200941207     C                   ENDIF
033300941207     C     VNSP          IFGT      *ZEROS
033400941207     C     VSCM          ANDGT     *BLANK
033500941207     C                   SETON                                        28
033600941207     C                   MOVEL     ERR(3)        $MSG
033700941207     C                   GOTO      ENDCTR
033800941207     C                   ENDIF
033900941207      *
034000941207      * SCELTA INTERR. X NR. GIACENZA
034100941207     C     VNGC          IFGT      *ZEROS
034200941207      *
034300941207     C     VAGC          IFEQ      *ZEROS
034400941207     C                   SETON                                        5128
034500941207     C                   MOVEL     ERR(4)        $MSG
034600941207     C                   GOTO      ENDCTR
034700941207     C                   ELSE
034800941207     C                   MOVEL     VAGC          W20               2 0
034900941207     C     W20           IFEQ      *ZEROS
035000941207     C     VAGC          IFLT      60
035100941207     C                   MOVEL     20            VAGC
035200941207     C                   ELSE
035300941207     C                   MOVEL     19            VAGC
035400941207     C                   ENDIF
035500941207     C                   ENDIF
035600941207     C                   ENDIF
035700941207      *
035800941207     C     VFGC          IFEQ      *ZEROS
035900941207     C                   SETON                                        5228
036000941207     C                   MOVEL     ERR(5)        $MSG
036100941207     C                   GOTO      ENDCTR
036200900509     C                   END
036300900509     C*
036400941207     C                   MOVE      VAGC          §AGC
036500941207     C                   MOVE      VFGC          §FGC
036600941207     C                   MOVE      VNGC          §NGC
036700900509     C                   SETON                                        40
036800941209     C                   GOTO      ENDCTR
036900941207     C                   ELSE
037000941207     C     VFGC          IFGT      0
037100941207     C                   SETON                                        5328
037200941207     C                   MOVEL     ERR(6)        $MSG
037300941207     C                   GOTO      ENDCTR
037400941207     C                   ENDIF
037500941207     C                   ENDIF
037600941207      *
037700900509     C* SCELTA PER NR. SPEDIZIONE
037800941207     C     VNSP          IFGT      *ZEROS
037900941207      *
038000941207     C     VAAS          IFEQ      *ZEROS
038100941207     C                   SETON                                        6128
038200941207     C                   MOVEL     ERR(7)        $MSG
038300941207     C                   GOTO      ENDCTR
038400941207     C                   ELSE
038500941213     C                   MOVEL     VAAS          W20               2 0
038600941207     C     W20           IFEQ      *ZEROS
038700941213     C     VAAS          IFLT      60
038800941213     C                   MOVEL     20            VAAS
038900941207     C                   ELSE
039000941213     C                   MOVEL     19            VAAS
039100941207     C                   ENDIF
039200941207     C                   ENDIF
039300941207     C                   ENDIF
039400941207     C*
039500941207     C     VLNP          IFEQ      *ZEROS
039600941207     C                   MOVEL     ERR(8)        $MSG
039700941207     C                   SETON                                        6228
039800941207     C                   GOTO      ENDCTR
039900900509     C                   END
040000941207     C                   MOVE      VAAS          §AAS
040100941207     C                   MOVE      VLNP          §LNP
040200941207     C                   MOVE      VNRS          §NRS
040300941207     C                   MOVE      VNSP          §NSP
040400941207     C                   SETON                                        41
040500941209     C                   GOTO      ENDCTR
040600941207     C                   ELSE
040700941209     C     VLNP          IFGT      *ZEROS
040800941207     C                   MOVEL     ERR(9)        $MSG
040900941207     C                   SETON                                        6328
041000941207     C                   GOTO      ENDCTR
041100900509     C                   END
041200941207     C                   END
041300941207     C*
041400900509     C* SCELTA PER CODICE MITTENTE
041500941207     C     VSCM          IFNE      *BLANK
041600900509     C* RICERCA CLIENTE
041700941207     C                   MOVEA     VSCM          KSC§
041800900509     C     '?'           LOOKUP    KSC§                                   21
041900900509     C   21              DO
042000901011     C                   MOVEL     RAGUT         PARDUT
042100901011     C                   Z-ADD     CODUT         CODUT§
042200901011     C                   Z-ADD     KCI           KCI§
042300900509     C                   MOVEL     *BLANKS       RICRAG
042400900509     C                   MOVEL     RICALF        RICRAG
042500900509     C                   Z-ADD     0             RICKSC
042600900509     C                   Z-ADD     0             PARSTA            1 0
042700900509     C                   CALL      'XALFA1'
042800900509     C                   PARM                    PARDUT
042900901011     C                   PARM                    CODUT§
043000900509     C                   PARM                    RICRAG
043100901011     C                   PARM                    KCI§
043200900509     C                   PARM                    RICKSC
043300900509     C                   PARM                    PARSTA
043400961016     C                   PARM                    PARFLR           90
043500900509     C     PARSTA        IFNE      -1
043600941207     C                   MOVE      RICKSC        VSCM
043700900509     C                   ELSE
043800941207     C                   MOVE      *ZEROS        VSCM
043900900509     C                   END
044000941209     C                   SETON                                        29
044100941207     C                   GOTO      ENDCTR
044200900509     C                   END
044300900509     C*
044400950301     C                   TESTN                   VSCM                 31
044500950301     C   31              MOVE      VSCM          §SCM
044600900509     C                   SETON                                        42
044700941207     C                   ENDIF
044800950315     C*
044900950315     C* SE IN ARRIVO, SE 41 E 42 SONO SPENTI PARTO
045000950315     C* CON L'INTERROGAZIONE A=APERTI, C=CHIUSI, T=TUTTI
045100950315     C   80
045200950315     CANN40
045300950315     CANN41              SETON                                        43
045301161123     C*
045302161123     C* SE IN Partenza, SE 41 E 42 SONO SPENTI PARTO
045303161123     C* CON L'INTERROGAZIONE A=APERTI
045304161123     C  N80
045305161123     CANN40
045306161123     CANN41              SETON                                        42
045400950315     C*
045500950315     C* SE INSERITA LA FILIALE, LA VERIFICO
045600950315     C     VFIL          IFNE      *ZERO
045700950315     C     VFIL          LOOKUP    L1                                     31
045800950316     C  N31              MOVEL     ERR(12)       $MSG
045900950315     C  N31              SETON                                        54  28
046000950316     C  N31              GOTO      ENDCTR
046100950315     C                   ENDIF
046200950315     C*
046300900418     C     ENDCTR        ENDSR
046400950316     C**************************************************************************
046500950316     C*  RICHIAMA PGM INTERROGAZIONE GIACENZA
046600950316     C**************************************************************************
046700941207     C     RICINT        BEGSR
046800950316     C*
046900941212     C                   MOVEL     *BLANK        D30F03
047000941212     C                   MOVEL     *BLANK        D30F12
047100941213     C                   MOVEL     *BLANK        D30ERR
047200941212     C     D30F03        DOWEQ     ' '
047300941212     C     D30F12        ANDEQ     ' '
047400020822     c     d30err        andeq     ' '
047500941207     C                   MOVEL     KPJBU         §KPJBU          256
047600941212     C                   MOVEL     *BLANK        D30F03
047700941212     C                   MOVEL     *BLANK        D30F12
047800941213     C                   MOVEL     *BLANK        D30ERR
047900941207     C                   MOVE      VAGC          D30AGC
048000941207     C                   MOVE      VFGC          D30FGC
048100941207     C                   MOVE      VNGC          D30NGC
048200941207     C                   MOVE      VAAS          D30AAS
048300941207     C                   MOVE      VLNP          D30LNP
048400941207     C                   MOVE      VNRS          D30NRS
048500941207     C                   MOVE      VNSP          D30NSP
048600941213     C                   MOVEL     *ZEROS        D30FRG
048700941213     C                   MOVEL     *BLANKS       D30RIC
048800941207     C                   MOVEL     FNLG30        KPJBU
048900941207     C                   CALL      'FNLG31R'
049000941207     C                   PARM                    KPJBA
049100941207     C*
049200941207     C                   MOVEL     KPJBU         FNLG30
049300941207     C     D30RIC        IFEQ      'S'
049400941207     C                   CALL      'FNLG32R'
049500941207     C                   PARM                    KPJBA
049600941213     C                   MOVEL     KPJBU         FNLG30
049700941207     C                   END
049800941212     C                   ENDDO
049900941213     C                   MOVEL     §KPJBU        KPJBU
050000941213     C     D30ERR        IFEQ      '1'
050100941213     C                   SETON                                        28
050200941213     C                   MOVEL     ERR(11)       $MSG
050300941213     C                   ENDIF
050400950316     C*
050500941207     C                   ENDSR
050600950316     C**************************************************************************
050700950316     C*   PULIZIA E CARICAMENTO SUBFILE
050800950316     C**************************************************************************
050900900509     C     GESSFL        BEGSR
051000950316     C*
051100900509     C                   EXSR      PULIZ
051200900509     C* CARICAMENTO SUBFILE
051300900509     C                   EXSR      CARIC
051400950316     C                   Z-ADD     1             REC
051500950316     C*
051600900509     C                   ENDSR
051700950316     C**************************************************************************
051800950316     C*  PULIZIA DEL SUBFILE
051900950316     C**************************************************************************
052000900509     C     PULIZ         BEGSR
052100950316     C*
052200900509     C                   SETON                                        70
052300941207     C                   WRITE     LG30C02
052400941209     C                   SETOFF                                       7071
052500950316     C*
052600950316     C* PREDISPONGO IL FILE PER LE LETTURE
052700950316     C                   Z-ADD     0             NRR               4 0
052800950316     C                   Z-ADD     0             TOTREC            4 0
052900050307     C   42kgcp4         SETLL     tigcp07L
053000050418     C   43VFIL          SETLL     tigcp26L
053100950316     C*
053200900509     C                   ENDSR
053300950316     C**************************************************************************
053400950316     C*  CARICAMENTO SUBFILE UNA PAGINA ALLA VOLTA
053500950316     C**************************************************************************
053600900509     C     CARIC         BEGSR
053700950316     C*
053800900510     C                   SETOFF                                       2122
053900950316     C                   Z-ADD     *ZERO         WPAGIN            2 0
054000950316     C                   Z-ADD     TOTREC        NRR
054100950316     C*
054200941207     C* X COD. MITTENTE
054300900509     C     *IN21         DOUEQ     '1'
054400950316     C     WPAGIN        ORGT      16
054500990716     C* PARTENZE
054501161123     c                   if        *in42
054502161123     c                   if        §scm=0
054600161123     C     simpou        READE     tigcp07L                               21
054601161123     c                   else
054602161123     C     kgcp4         READE     tigcp07L                               21
054603161123     c                   end
054604161123     c                   end
054700950316     C* ARRIVI
054800950315     C   43VFIL          IFNE      *ZERO
054900050418     C     VFIL          READE     tigcp26L                               21
055000950315     C                   ELSE
055100050418     C                   READ      tigcp26L                               21
055200020822     c     gcpfgc        lookup    l1                                     75
055300020822     c  n75              iter
055400950315     C                   ENDIF
055500950316     C*
055600941207     C  N21GCPFRG        COMP      0                                  2222
055700950316     C*
055800941207     C* PARZIALIZZAZIONI
055900950316     C* TESTO LA FASE PER SAPERE SE LA GIACENZA E' CHIUSA O APERTA
056000950316     C*    PARTENZE :   APERTE CON FASE < 050
056100950316     C*    ARRIVI   :      "    "    "  < 038
056200900626     C  N21
056300900626     CANN22VPARZ         IFEQ      'C'
056400950316     C   42GCPFAS        COMP      050                                  22
056500950316     C   43GCPFAS        COMP      038                                  22
056600900510     C                   END
056700900626     C  N21
056800900626     CANN22VPARZ         IFEQ      'A'
056900950316     C   42GCPFAS        COMP      050                                22  22
057000950316     C   43GCPFAS        COMP      038                                22  22
057100900510     C                   END
057200900510     C*
057300950316     C* CARICO NEL SFL SOLO SE INDICATORI SPENTI
057400900510     C     *IN22         IFEQ      '0'
057500900510     C     *IN21         ANDEQ     '0'
057600900509     C**------------------------
057700900509     C** CARICO RECORDS DA FILE
057800900509     C**------------------------
057900901005     C                   Z-ADD     GCPAAS        §AAS
058000901005     C                   Z-ADD     GCPLNP        §LNP
058100901005     C                   Z-ADD     GCPNRS        §NRS
058200901005     C                   Z-ADD     GCPNSP        §NSP
058300990716     C     *IN93         IFEQ      '1'
058400990830     C     KSPE          CHAIN     TITAS30C                           31
058500990830     C  N31              MOVEL     TASRSD        SDES
058600990830     C                   ELSE
058700950117     C     *IN80         IFEQ      '1'
058800050307     C                   Z-ADD     GCPAASa       §AAS
058900050307     C                   Z-ADD     GCPLNPa       §LNP
059000050307     C                   Z-ADD     GCPNRSa       §NRS
059100050307     C                   Z-ADD     GCPNSPa       §NSP
059200950117     C     KSPE          CHAIN     FNARB01L                           31
059300950117     C  N31              MOVEL     ARBRSD        SDES
059400950117     C                   ELSE
059500960123     C                   Z-ADD     GCPAAS        DN0AAS
059600960123     C                   Z-ADD     GCPLNP        DN0LNP
059700960123     C                   Z-ADD     GCPNRS        DN0NRS
059800960123     C                   Z-ADD     GCPNSP        DN0NSP
059900960123     C                   MOVEL     'R'           DN0TRC
060000960125     C                   Z-ADD     GCPDUR        DN0DAG
060100960123     C                   MOVEL     FNLSN0        KPJBU
060200960123     C                   CALL      'FNLSN0R'
060300960123     C                   PARM                    KPJBA
060400960123     C                   MOVEL     KPJBU         FNLSN0
060500960123     C     DN0TRV        IFEQ      'S'
060600960123     C                   MOVEL     DN0RSD        SDES
060700960123     C                   SETOFF                                       31
060800960123     C                   ELSE
060900950117     C     KSPE          CHAIN     FNBLP01L                           31
061000010706     C     *IN31         IFEQ      '0'
061100010706     C                   MOVEL     BLPRSD        SDES
061200010706     C                   END
061300010706      *
061400960123     C                   END
061500950117     C                   END
061600990830     C                   END
061700950117     C*
061800901004     C   31              MOVEL     *BLANKS       SDES
061900900509     C                   MOVEL     *BLANKS       SCELTA
062000900509     C                   Z-ADD     GCPAGC        SAGC
062100900509     C                   Z-ADD     GCPFGC        SFGC
062200900509     C                   Z-ADD     GCPNGC        SNGC
062300050307     c                   if        *in80
062400050307     C                   Z-ADD     GCPAASa       SAAS
062500050307     C                   Z-ADD     GCPLNPa       SLNP
062600050307     C                   Z-ADD     GCPNRSa       SNRS
062700050307     C                   Z-ADD     GCPNSPa       SNSP
062800050307     c                   else
062900900509     C                   Z-ADD     GCPAAS        SAAS
063000900509     C                   Z-ADD     GCPLNP        SLNP
063100900509     C                   Z-ADD     GCPNRS        SNRS
063200900509     C                   Z-ADD     GCPNSP        SNSP
063300050307     c                   end
063400900509     C                   Z-ADD     GCPSCM        SSCM
063500900509     C                   MOVEL     GCPCFG        SCFG
063600900509     C*INVERSIONE DATA CHIUSURA
063700941207     C                   MOVEL     *ZEROS        G08DAT
063800941207     C                   MOVEL     *ZEROS        G08TGI
063900941207     C                   MOVE      GCPDCG        G08INV
064000941207     C                   MOVEL     '3'           G08ERR
064100941207     C                   CALL      'XSRDA8'
064200941207     C                   PARM                    WLBDA8
064300941207     C                   Z-ADD     G08DAT        SDCG
064400900509     C*
064500900509     C                   ADD       1             NRR
064600941207     C                   WRITE     LG30S02
064700950316     C                   ADD       1             WPAGIN
064800950316     C                   ADD       1             TOTREC
064900900509     C                   END
065000900509     C                   END
065100950316     C                   Z-ADD     NRR           REC
065200950316     C   21              Z-ADD     TOTREC        REC
065300950316     C   21
065400950316     CAN 26              MOVEL     ERR(13)       $MSG
065500950316     C   21
065600950316     CAN 26              SETON                                        28
065700950316     C*
065800900509     C     FINCAR        ENDSR
065900950316     C**************************************************************************
066000950316     C*  LETTURA SFL
066100950316     C**************************************************************************
066200900509     C     LETSFL        BEGSR
066300950316     C*
066400900509     C     SCEL          TAG
066500941207     C                   READC     LG30S02                                05
066600900509     C   05              GOTO      FINLET
066700941207     C     SCELTA        IFEQ      '1'
066800900509     C* RICH. PGM INTERROGAZIONE SINGOLA GIACENZA SCELTA
066900900509     C*
067000900509     C                   MOVEL     KPJBU         §KPJBU          256
067100941207     C                   MOVE      SAGC          D30AGC
067200941207     C                   MOVE      SFGC          D30FGC
067300941207     C                   MOVE      SNGC          D30NGC
067400941207     C                   MOVE      SAAS          D30AAS
067500941207     C                   MOVE      SLNP          D30LNP
067600941207     C                   MOVE      SNRS          D30NRS
067700941207     C                   MOVE      SNSP          D30NSP
067800941207     C                   MOVEL     *BLANKS       D30RIC
067900941207     C                   MOVEL     *BLANKS       D30F03
068000941213     C                   MOVEL     *BLANKS       D30F12
068100941213     C                   MOVEL     *BLANKS       D30ERR
068200941207     C                   MOVEL     FNLG30        KPJBU
068300941207     C                   CALL      'FNLG31R'
068400900510     C                   PARM                    KPJBA
068500900509     C*
068600941207     C                   MOVEL     KPJBU         FNLG30
068700900509     C                   MOVE      *BLANKS       SCELTA
068800950316     C                   UPDATE    LG30S02
068900950316     C                   Z-ADD     NRR           REC
069000950316     C*
069100941207     C     D30RIC        IFEQ      'S'
069200941207     C                   CALL      'FNLG32R'
069300911119     C                   PARM                    KPJBA
069400911119     C                   END
069500941207     C                   MOVEL     §KPJBU        KPJBU
069600900509     C                   GOTO      SCEL
069700900509     C                   END
069800950316     C*
069900900509     C     FINLET        ENDSR
070000950316     C**************************************************************************
070100950316     C*
070200950316     C**************************************************************************
070300941207     C     *INZSR        BEGSR
070400950316     C*
070500941207     C     *ENTRY        PLIST
070600941207     C                   PARM                    KPJBA
070700941207     C                   MOVEL     KPJBU         FNLG30
070800990716     C* RICHIAMO PARUT
070900990716     C                   Z-ADD     1             CODUT
071000990716     C                   CALL      'X§PARUT'
071100990716     C                   PARM                    UTEDSE
071200990716     C     CODUT         CABEQ     -1            FINE
071300990716     C                   MOVEL     REC80         CNCR80
071400990716      * SEDE
071500990716     C     SIMFEL        IFEQ      0
071600990716     C                   SETON                                        93
071700050307     C                   OPEN      tigcp07L
071800990716     C                   OPEN      TITAS30C
071900990716     C                   ELSE
072000990716      * FILIALE
072100990716      * GIAC. IN ARRIVO
072200941207     C     D30POA        IFEQ      'A'
072300990903     C                   SETOFF                                       93
072400941207     C                   SETON                                        80
072500050418     C                   OPEN      tigcp26L
072600941207     C                   OPEN      FNARB01L
072700941207     C                   ELSE
072800990716      * FILIALE
072900990716      * GIAC. IN PARTENZA
073000990903     C                   SETOFF                                       93
073100050307     C                   OPEN      tigcp07L
073200941207     C                   OPEN      FNBLP01L
073300941207     C                   ENDIF
073400990716      * FILIALE
073500990716     C* CARICO LA £1
073600990716     C                   CLEAR                   DSUL06
073700990716     C                   MOVE      '£1'          D06COD
073800990716     C                   MOVEL     SIMFEL        D06KEY
073900990716     C                   MOVEL     DSUL06        KPJBU
074000990716     C                   CALL      'TRUL06R'
074100990716     C                   PARM                    KPJBA
074200990716     C                   MOVEL     KPJBU         DSUL06
074300990716     C                   MOVEA     T06           L1
074400990716     C                   ENDIF
074500941207      *
074600941207     C                   MOVEL     RAGUT         RSUT             20
074700941207     C                   MOVEL     RAGUT         RSUT2            25
074800941207     C                   MOVEL     RAGUT         PARDUT           30
074900941207     C*§
075000941207     C                   TIME                    WHHUDT           14 0
075100941207     C                   MOVEL     WHHUDT        ORA               6 0
075200941207     C                   MOVE      WHHUDT        G08DAT
075300941207     C                   MOVE      G08DAT        ANNO              4 0
075400941207     C*--- RICERCA CAPOCONTI------------------------------------------*
075500941207     C                   DO        50            X                 2 0
075600941207     C                   MOVE      TCU(X)        TCUDS
075700941207     C     F56           CABNE     'CG'          END1
075800941207     C     F4            COMP      '1'                                    21
075900941207     C** 1 CLIENTI   21
076000941207     C     F3            COMP      '0'                                242425
076100941207     C** 0 ITALIA   25
076200941207     C** 1 ESTERO   24
076300941207     C   21
076400941207     CAN 24              Z-ADD     KCU(X)        KCE               4 0
076500941207     C   21
076600941207     CAN 25              Z-ADD     KCU(X)        KCI               4 0
076700941207     C     END1          TAG
076800941207     C                   END
076900941207     C*---------------------------------------------------------------*
077000941207     C                   SETON                                        90
077100941207     C                   SETON                                        60
077200950315     C*
077300941207     C*  KLIST
077400941207     C     KSPE          KLIST
077500941207     C                   KFLD                    §AAS
077600941207     C                   KFLD                    §LNP
077700941207     C                   KFLD                    §NRS
077800941207     C                   KFLD                    §NSP
077900941207     C     KGIA          KLIST
078000941207     C                   KFLD                    §AGC
078100941207     C                   KFLD                    §FGC
078200941207     C                   KFLD                    §NGC
078300020822     c     kgcp4         klist
078400020822     c                   kfld                    simpou
078500020822     c                   kfld                    §scm
078600941207     C*
078700941207     C     *LIKE         DEFINE    KCI           KCI§
078800941207     C     *LIKE         DEFINE    CODUT         CODUT§
078900941207     C     *LIKE         DEFINE    GCPAAS        §AAS
079000941207     C     *LIKE         DEFINE    GCPLNP        §LNP
079100941207     C     *LIKE         DEFINE    GCPNRS        §NRS
079200941207     C     *LIKE         DEFINE    GCPNSP        §NSP
079300941207     C     *LIKE         DEFINE    GCPAGC        §AGC
079400941207     C     *LIKE         DEFINE    GCPFGC        §FGC
079500941207     C     *LIKE         DEFINE    GCPNGC        §NGC
079600941207     C     *LIKE         DEFINE    GCPSCM        RICKSC
079700941209     C     *LIKE         DEFINE    GCPSCM        §SCM
079800941209     C                   MOVEL     *BLANKS       RICRAG           48
079900950316     C*
080000941207     C                   ENDSR
080100950316     C**************************************************************************
080200950316** ERRORI
080300941209Immettere o il Nr.Spedizione o il Nr. Giacenza                                 1
080400941209Immettere o il Nr.di Giacenza o il Codice Mittente                             2
080500941209Immettere o il Nr.di Spedizione  o il Codice Mittente                          3
080600941209Immettere l'Anno di Apertura Giacenza                                          4
080700961113Immettere il punto operativo che ha aperto la Giacenza                         5
080800941209Immettere il Numero della Giacenza                                             6
080900941209Immettere l'Anno della Spedizione                                              7
081000941209Immettere la linea di Partenza                                                 8
081100941209Immettere il Numero della Spedizione                                           9
081200941209Non trovate Giacenze per il Cliente immesso                                   10
081300941213Non trovata Giacenza !!!                                                      11
081400961114Immettere punto operativo esistente e appartenente all'area                   12
081500950316Fine scorrimento                                                              13
081600161123Immettere parzializzazioni oppure SOLO le Aperte                              14
