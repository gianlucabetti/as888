000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011115     H* FICND5R *----------------------------------------------------*
000300000808     H*         - STAMPA RIEPILOGO CONTEGGI PADRONCINI
000400000000     H*--------------------------------------------------------------*
000500011115     FFICND5D   CF   E             WORKSTN
000600000808     FTNTLZ01L  IF   E           K DISK
000700021203     FFIAPD01L  IF   E           K DISK
000800991228     FTABEL00F  IF   E           K DISK
000900930707     F*
001000950125     D* SCHIERE PER CARICAMENTO L1 E L6
001100950125     D L1              S              3  0 DIM(30)                              FIL GESTITE
001200950125     D L6              S              3  0 DIM(30)                              FIL GESTITE
001300920810     D KPJBA         E DS
001400020701     D* REM                    1      3
001500020701     D* REMFIL                 4      6
001600950125     D*
001700011115     D* PASSAGGIO DATI A PGM DI STAMPA RIEPILOGO         - FICND6R -
001800931126     D PARAM           DS
001900931126     D  PARP1                 28     34  0
002000931126     D  PARP2                 35     41  0
002100931126     D  VI1KSC                42     48  0
002200931209     D  VI1DKC                49     78
002300931202     D  VI1CSF                79     81
002400931210     D  VI1SML                82     82
002500940325     D  VI1TSP                83     83
002600950125     D  VI1DD1                84     91  0
002700950125     D  W01DD1                92     99  0
002800950125     D  VI1DD2               100    107  0
002900950125     D  W01DD2               108    115  0
003000000808     D* PASSAGGIO DATI ALLA RICERCA TRAZ./PADR.  - TRUL16R -
003100000808     D PARAM3          DS
003200000808     D* TIPOLOGIA P_=PADRONCINO  (T_=TRAZIONISTA)
003300000808     D  PA3TIP                 1      2
003400000808     D* RAGIONE SOCIALE
003500000808     D  PA3RSC                 3     37
003600000808     D* SISTEMA INFORMATIVO
003700000808     D  PA3CSF                38     40
003800000808     D* CODICE
003900000808     D  PA3PDR                41     47  0
004000000808     D* PA3FLG = "3" --> RESTITUISCE AL PGM CHIAMANTE IL MESSAGGIO CHE
004100000808     D*   NON ESISTONO RECORDS PER LA CHIAVE ALFABETICA RICHIESTA
004200000808     D  PA3FLG                48     48
004300950125     D*
004400950125     D* DS PER TRUL06R - CARICAMENTO £1
004500950125     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004600950125     D  LIN                    1     90  0
004700950125     D                                     DIM(30)                              SKI COMODO
004800910423     D WLBDAT          DS
004900950125     D  G02DAT                 1      8  0
005000950125     D  G02INV                 9     16  0
005100950125     D  G02ERR                17     17
005200950125     D  G02TGI                18     22  0
005300931015     D                 DS
005400931015     D  VI1PF1                 1      3  0
005500931116     D  VI1PD1                 4      7
005600931115     D  VI1P1                  1      7  0
005700931126     D                 DS
005800931126     D  VI1PF2                 1      3  0
005900931126     D  VI1PD2                 4      7
006000931126     D  VI1P2                  1      7  0
006100991228     D DS5A          E DS
006200021203     D FNLV24DS      E DS
006300991228     D CNCR80        E DS
006400900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006500000000     I/SPACE 3
006600900521     C****************************************************************
006700900521     C*  RIEPILOGO INDICATORI
006800900521     C***************************************************************
006900931116     C* 30    - DI COMODO
007000010906     C* 40/56 - ERRORI
007100950125     C* 89    - SONO UN "REMXXX"
007200900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
007300900521     C*****************************************************************
007400000000     C     *ENTRY        PLIST
007500000000     C                   PARM                    KPJBA
007600000000     C                   Z-ADD     1             CODUT
007700900615     C                   CALL      'X§PARUT'
007800900615     C                   PARM                    UT§DSE
007900000000     C                   MOVEL     RAGUT         RSUT             20
008000900615     C                   MOVEL     REC80         CNCR80
008100031023     C                   MOVEL     knmus         Primi_tre         3
008200900511     C*---------------------------------------------------------------*
008300991228     C     KTAB          KLIST
008400991228     C                   KFLD                    CODUT
008500991228     C                   KFLD                    COD
008600991228     C                   KFLD                    KEY
008700000808     C*
008800000808     C     KTNTLZ        KLIST
008900000808     C                   KFLD                    KTIP              2
009000000808     C                   KFLD                    VI1KSC
009100000808     C                   KFLD                    VI1CSF
009200000808     C*
009300000808     C                   MOVEL     'P '          KTIP
009400021203     C*
009500021203     C     KAPDV         KLIST
009600021203     C                   KFLD                    APDTIP
009700021203     C                   KFLD                    VI1P1
009800021203     C*
009900021203     C                   MOVEL     'A'           APDTIP
010000000808     C*
010100991228     C     *LIKE         DEFINE    TBLCOD        COD
010200991228     C     *LIKE         DEFINE    TBLKEY        KEY
010300991228     C     *LIKE         DEFINE    G02TGI        DD1GIO
010400991228     C     *LIKE         DEFINE    G02TGI        DD2GIO
010500991228     C     *LIKE         DEFINE    G02TGI        WRESTO
010600931116     C*---------------------------------------------------------------*
010700910419     C*
010800950411     C* OPERAZIONI INIZIALI
010900931111     C                   EXSR      PUL01
011000950411     C*
011100950411     C     INIZIO        TAG
011200031023      *
011300950411     C                   MOVEL     '0000'        VI1PD1
011400950411     C                   MOVEL     '9999'        VI1PD2
011500950411     C                   CLEAR                   VI1DD1
011600950411     C                   CLEAR                   VI1DD2
011700950411     C                   CLEAR                   VI1KSC
011800950411     C                   CLEAR                   VI1RKC
011900950411     C                   CLEAR                   VI1DKC
012000950411     C                   CLEAR                   VI1CSF
012100020517     C                   MOVEL     '1'           VI1TSP
012200930426     C*
012300931015     C     FOR01         TAG
012400950125     C                   EXFMT     LRD5D01
012500931116     C*
012600920323     C** CMD3 - FINE LAVORO
012700931112     C   KC              GOTO      FINE
012800931015     C* CONTROLLI FORMATO1
012900900528     C                   EXSR      CONTR1
013000950125     C  NKF
013100931116     COR 90              GOTO      FOR01
013200931116     C*
013300931202     C     VI1PD1        IFEQ      *BLANKS
013400931207     C                   MOVEL     *ZEROS        VI1PD1
013500931202     C                   ENDIF
013600931202     C     VI1PD2        IFEQ      *BLANKS
013700931207     C                   MOVEL     *ZEROS        VI1PD2
013800931202     C                   ENDIF
013900931126     C                   MOVE      VI1P1         PARP1
014000931126     C                   MOVEL     VI1P2         PARP2
014100931126     C                   MOVEL     PARAM         KPJBU
014200931202     C*
014300011115     c     knmus         ifeq      *all'1'
014400011115     C     vi1sml        ifeq      ' '
014500011115     C                   call      'FICND6R'
014600011115     C                   parm                    Kpjba
014700011115     C                   else
014800011115     C* RIEPILOGO CONTEGGI DI SIMULAZIONE
014900011115     C                   call      'FICNE0C'
015000011115     C                   parm                    Kpjba
015100011115     C                   end
015200011115     c                   else
015300931202     C* RIEPILOGO CONTEGGI EFFETTIVI
015400931202     C     VI1SML        IFEQ      ' '
015500011116     C                   MOVEL     'FND6'        KCOAZ
015600931202     C                   ELSE
015700931202     C* RIEPILOGO CONTEGGI DI SIMULAZIONE
015800011116     C                   MOVEL     'FNE0'        KCOAZ
015900931202     C                   ENDIF
016000931116     C                   CALL      'BCH10'
016100931116     C                   PARM                    KPJBA
016200011115     c                   end
016300900611     C*
016400950411     C                   GOTO      INIZIO
016500000000     C     FINE          TAG
016600931027     C                   SETON                                        LR
016700931111     C*
016800931111     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
016900931111     C     PUL01         BEGSR
017000931126     C* CARICO L1
017100950125     C                   CLEAR                   DSUL06
017200950125     C                   MOVE      '£1'          D06COD
017300950125     C                   MOVEL     SIMFEL        D06KEY
017400950125     C                   MOVEL     DSUL06        KPJBU
017500950125     C*
017600950125     C                   CALL      'TRUL06R'
017700950125     C                   PARM                    KPJBA
017800950125     C                   MOVEL     KPJBU         DSUL06
017900950125     C                   MOVEA     LIN           L1
018000931126     C*
018100950125     C***
018200950125     C* VEDO SE SONO SIMFEL O REMOTO
018300950125     C***
018400020701     C                   EVAL      Vi1PF1 = SimPOU
018500020701     C                   EVAL      Vi1PF2 = SimPOU
018600020701    1C*    REM           IFEQ      'REM'
018700020701     C*    REMFIL        ANDGT     *ZEROS
018800020701     C                   IF        SimTpP = '2' OR SimTpP = *BLANK
018900020701     C*                  MOVEL     REMFIL        VI1PF1
019000020701     C*                  MOVEL     REMFIL        VI1PF2
019100950125     C                   SETON                                        89
019200950125     C*
019300950125     C* CARICO TABELLA FILIALI GESTITE £6
019400950125     C                   CLEAR                   DSUL06
019500950125     C                   MOVE      '£6'          D06COD
019600020701     C*                  MOVEL     REMFIL        D06KEY
019700020701     C                   MOVEL     SimPOU        D06KEY
019800950125     C                   MOVEL     DSUL06        KPJBU
019900950125     C*
020000950125     C                   CALL      'TRUL06R'
020100950125     C                   PARM                    KPJBA
020200950125     C                   MOVEL     KPJBU         DSUL06
020300950125     C                   MOVEA     LIN           L6
020400950125     C*
020500950125     C                   ELSE
020600950125     C* SONO SIMFEL
020700950125     C                   MOVEL     SIMFEL        VI1PF1
020800991228     C                   MOVEL     SIMFEL        VI1PF2
020900950125     C                   ENDIF
021000991228     C                   MOVEL     '5A'          COD
021100991228     C                   MOVEL(P)  '1'           KEY
021200991228     C     KTAB          CHAIN     TABEL                              30
021300991228     C  N30              MOVEL     TBLUNI        DS5A
021400991228     C   30              CLEAR                   DS5A
021500000203     C                   TIME                    W0140            14 0
021600000203     C* UDATE IN GGMMAAAA
021700000203     C                   MOVE      W0140         WDTGIO            8 0
021800000203     C                   MOVE      WDTGIO        ANNOCO            4 0
021900000203     C     ANNOCO        ADD       1             ANNOPI            4 0
022000000203     C     ANNOCO        SUB       1             ANNOME            4 0
022100950125     C*
022200931111     C                   ENDSR
022300900528     C*
022400900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
022500900528     C     CONTR1        BEGSR
022600931111     C                   SETOFF                                       90
022700931126     C*
022800931126     C* D A T A   D I S T I N T A: DAL
022900931126     C                   MOVE      VI1DD1        G02DAT
023000931126     C                   MOVEL     *BLANK        G02ERR
023100950125     C                   CALL      'XSRDA8'
023200931126     C                   PARM                    WLBDAT
023300931126    1C     G02ERR        IFEQ      '1'
023400931126     C                   SETON                                        43  90
023500931126     C                   GOTO      ENDCT1
023600931126    1C                   END
023700931126     C                   Z-ADD     G02INV        W01DD1
023800950125     C                   Z-ADD     G02DAT        VI1DD1
023900991228     C                   Z-ADD     G02TGI        DD1GIO
024000011115     C                   z-add     vi1dd1        an1               4 0
024100000203     C* CONTROLLO L'ANNO
024200000203     C                   Z-ADD     G02DAT        W0040             4 0
024300000203     C     W0040         IFGT      ANNOPI
024400000203     C     W0040         ORLT      ANNOME
024500000203     C                   SETON                                        43  90
024600000203     C                   GOTO      ENDCT1
024700000203    1C                   END
024800931126     C*
024900021106     C* comunque imposta il 2 campo data
025000021106     C                   Z-ADD     w01dd1        w01dd2
025100931126    1C     VI1DD2        IFGT      0
025200931126     C                   MOVE      VI1DD2        G02DAT
025300931126     C                   MOVEL     *BLANK        G02ERR
025400950125     C                   CALL      'XSRDA8'
025500931126     C                   PARM                    WLBDAT
025600931126    2C     G02ERR        IFEQ      '1'
025700931126     C                   SETON                                        44  90
025800931126     C                   GOTO      ENDCT1
025900931126    2C                   END
026000931126     C                   Z-ADD     G02INV        W01DD2
026100950125     C                   Z-ADD     G02DAT        VI1DD2
026200991228     C                   Z-ADD     G02TGI        DD2GIO
026300011115     C                   z-add     vi1dd2        an2               4 0
026400000203     C* CONTROLLO L'ANNO
026500000203     C                   Z-ADD     G02DAT        W0040             4 0
026600011115     c     an1           ifne      an2
026700011115     C                   SETON                                        56  90
026800011115     C                   GOTO      ENDCT1
026900011115     c                   end
027000000203     C     W0040         IFGT      ANNOPI
027100000203     C     W0040         ORLT      ANNOME
027200000203     C                   SETON                                        44  90
027300000203     C                   GOTO      ENDCT1
027400000203    1C                   END
027500931126     C* DATA AL NO < DATA DAL
027600931126    2C     W01DD2        IFLT      W01DD1
027700931126     C                   SETON                                        45  90
027800931126     C                   GOTO      ENDCT1
027900931126    2C                   ENDIF
028000991228     C* I GIORNI TRA UNA DATA E L'ALTRA NON DEVONO SUPERARE
028100991228     Q* QUELLI DI PULIZIA + 10
028200991228    2C     VI1SML        IFEQ      ' '
028300991228     C     §5AFTT        ADD       10            WGIORN            4 0
028400991228     C                   ELSE
028500991228     C     §5AFST        ADD       10            WGIORN            4 0
028600991228    2C                   ENDIF
028700991228     C**
028800991228     C     DD2GIO        SUB       DD1GIO        WRESTO
028900991228     C**
029000991228    2C     WRESTO        IFGT      WGIORN
029100991228     C                   SETON                                        55  90
029200991228     C                   GOTO      ENDCT1
029300991228    2C                   ENDIF
029400991228     C**
029500991228    1C                   ENDIF
029600931123     C*
029700031023     C* Se non è un utente EDPxxx controlla la filiale inserita
029800091113     c                   if        simTPP <> 'S' or
029801130118     c                             (primi_tre <> 'EDP' and primi_tre <> 'RIC'
029802151112     c                              and primi_tre <> 'GEN' and
029803151112     c                              primi_tre <> 'DLO' )
029900031023     C*
030000950125     C* F I L I A L E    P A D R O N C I N O    D A L
030100950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
030200950125    1C     *IN89         IFEQ      *OFF
030300950125     C     VI1PF1        IFNE      SIMFEL
030400080109     C* non può essere (0) la filiale
030500080109     c                   if        VI1PF1 = 0
030600080109     c                   z-add     simfel        VI1PF1
030700080109     c                   end
030800950125     C     VI1PF1        LOOKUP    L1                                     35
030900950125     C  N35              SETON                                        5390
031000950125     C  N35              GOTO      ENDCT1
031100950125     C                   ENDIF
031200950125     C*
031300950125   X1C                   ELSE
031400950125     C* 89 ON  - SONO UN "REMXXX" QUINDI CONTROLLO LA FILIALE NELLA L6
031500020701     C                   MOVEL     VI1PF1        WFIL              3 0
031600020701     C*    WFIL          IFNE      REMFIL
031700020701     C     WFIL          IFNE      SimPOU
031800950125     C     VI1PF1        LOOKUP    L6                                     35
031900950125     C  N35              SETON                                        5390
032000950125     C  N35              GOTO      ENDCT1
032100950125     C                   ENDIF
032200950125    1C                   ENDIF
032300021015      *
032400031023     c                   end
032500031023      *
032600021015     c                   z-add     vi1pf1        vi1pf2
032700031023     C*
032800031023     C* Se non è un utente EDPxxx controlla la filiale inserita
032900091113     c*mm                if        primi_tre <> 'EDP' or simTPP <> 'S'
032901091113     c                   if        simTPP <> 'S' or
032902130118     c                             (primi_tre <> 'EDP' and primi_tre <> 'RIC'
032903151112     c                              and primi_tre <> 'GEN'
032904151112     c                              and primi_tre <> 'DLO')
033000950125     C*
033100950125     C* F I L I A L E    P A D R O N C I N O    A L
033200950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
033300950125    1C     *IN89         IFEQ      *OFF
033400950125     C     VI1PF2        IFNE      SIMFEL
033500950125     C     VI1PF2        LOOKUP    L1                                     35
033600950125     C  N35              SETON                                        5490
033700950125     C  N35              GOTO      ENDCT1
033800950125     C                   ENDIF
033900950125     C*
034000950125   X1C                   ELSE
034100950125     C* 89 ON  - SONO UN "REMXXX" QUINDI CONTROLLO LA FILIALE NELLA L6
034200020701     C                   MOVEL     VI1PF2        WFIL
034300020701     C*    WFIL          IFNE      REMFIL
034400020701     C     WFIL          IFNE      SimPOU
034500950125     C     VI1PF2        LOOKUP    L6                                     35
034600950125     C  N35              SETON                                        5490
034700950125     C  N35              GOTO      ENDCT1
034800950125     C                   ENDIF
034900950125    1C                   ENDIF
035000031023      *
035100031023     c                   end
035200950125     C*
035300950131     C* CONTROLLO CONGRUENZA FILIALI DAL-AL
035400950131    1C     VI1PF1        IFNE      VI1PF2
035500950131     C                   SETON                                        4890
035600950131     C                   GOTO      ENDCT1
035700950131    1C                   ENDIF
035800950131     C*
035900931116     C* P A D R O N C I N O
036000931116     C* RICERCA
036100931116     C     '?'           SCAN      VI1PD1                                 90
036200931116     C*
036300931126    1C     *IN90         IFEQ      *ON
036400021203     C                   Z-ADD     VI1PF1        D24FIL
036500931126     C                   EXSR      RICPAD
036600931126     C*
036700021203     C     D24PDR        IFNE      0
036800021203     C                   MOVEL     D24PDR        VI1PF1
036900931116     C                   MOVEL     *ZEROS        VI1PD1
037000021203     C                   MOVE      D24PDR        VI1PD1
037100021203     C                   MOVEL     D24RSC        VI1DP1
037200950125     C                   ENDIF
037300931116     C                   GOTO      ENDCT1
037400931126    1C                   ENDIF
037500931126     C* PADRONCINO AL
037600931126     C* RICERCA
037700931126     C     '?'           SCAN      VI1PD2                                 90
037800931126     C*
037900931126    1C     *IN90         IFEQ      *ON
038000021203     C                   Z-ADD     VI1PF2        D24FIL
038100931126     C                   EXSR      RICPAD
038200931126     C*
038300021203     C     D24PDR        IFNE      0
038400021203     C                   MOVEL     D24PDR        VI1PF2
038500931126     C                   MOVEL     *ZEROS        VI1PD2
038600021203     C                   MOVE      D24PDR        VI1PD2
038700950125     C                   ENDIF
038800931126     C                   GOTO      ENDCT1
038900931126    1C                   ENDIF
039000931126     C*
039100931126     C*
039200931126    1C     VI1PD2        IFNE      *BLANKS
039300931126     C     VI1PD2        ANDNE     *ZEROS
039400931126     C                   TESTN                   VI1PD2               30
039500931126     C  N30              SETON                                        4790
039600931126     C                   ENDIF
039700931116     C*
039800931126    1C     VI1PD1        IFNE      *BLANKS
039900931126     C     VI1PD1        ANDNE     *ZEROS
040000931126     C                   TESTN                   VI1PD1               30
040100931126     C  N30              SETON                                        4190
040200931116     C*
040300931123     C* SE NE HO IMMESSO SOLTANTO UNO CONTROLLO L'ESISTENZ
040400931126    2C     VI1PD2        IFEQ      *BLANKS
040500931126     C     VI1PD2        OREQ      *ZEROS
040600931126     C*
040700021203     C     KAPDV         CHAIN     FIAPD000                           30
040800931015     C*
040900931126    3C     *IN30         IFEQ      *ON
041000020520     C* Tolto il controllo sul padroncino annullato
041100020520     C                   SETON                                        4190
041200931028     C                   GOTO      ENDCT1
041300931126    3C                   ENDIF
041400931129     C*
041500100223     C                   MOVEL     APDRSf        VI1DP1
041600931123     C*
041700931123   X2C                   ELSE
041800931126     C*
041900931123     C* PADRONCINO AL  <  MINORE PADRONCINO DAL
042000950125    3C     VI1P2         IFLT      VI1P1
042100931123     C                   SETON                                        4290
042200931123     C                   GOTO      ENDCT1
042300931126    3C                   ENDIF
042400931126    2C                   ENDIF
042500931123    1C                   ENDIF
042600010906     C*
042700010906     C* PADRONCINO AL E PADRONCINO DAL A ZERO
042800010906     C     VI1PD1        IFEQ      *ZEROS
042900010906     C     VI1PD2        ANDEQ     *ZEROS
043000010906     C                   SETON                                        4090
043100010906     C                   GOTO      ENDCT1
043200010906     C                   ENDIF
043300931126     C*
043400931126     C* C O D I C E   F O R N I T O R E
043500931126     C* RICERCA
043600931126    1C     VI1RKC        IFEQ      '?'
043700931126     C     VI1KSC        ANDEQ     0
043800931126     C                   SETON                                        90
043900931126     C                   CLEAR                   VI1RKC
044000000808     C                   MOVEL     'P '          PA3TIP                         PAR-1
044100000808     C                   MOVEL(P)  VI1DKC        PA3RSC                         PAR-2
044200000808     C                   MOVEL     VI1CSF        PA3CSF                         PAR-3
044300000808     C                   CLEAR                   PA3PDR                         PAR-4
044400000808     C                   CLEAR                   PA3FLG                         PAR-5
044500000808     C                   MOVEL     PARAM3        KPJBU
044600000808     C                   CALL      'TRUL16R'
044700000808     C                   PARM                    KPJBA
044800000808     C                   MOVEL     KPJBU         PARAM3
044900000808     C                   MOVEL     PA3PDR        VI1KSC                         CODICE
045000000808     C                   MOVEL     PA3RSC        VI1DKC                         RAG.SOCIALE
045100000808     C                   MOVEL     PA3CSF        VI1CSF                         S.I.
045200000808     C     PA3FLG        IFEQ      '3'
045300000808     C                   SETON                                        5290
045400000808     C                   ENDIF
045500000808     C                   GOTO      ENDCT1
045600931126    1C                   ENDIF
045700931130     C*
045800931130     C* FORNITORE E SISTEMA INFORMATIVO: DEVONO ESSERCI ENTRAMBI
045900931130     C     VI1KSC        IFGT      0
046000931130     C     VI1CSF        ANDEQ     *BLANKS
046100931130     C                   SETON                                        5190
046200931130     C                   GOTO      ENDCT1
046300931130     C                   ENDIF
046400931130     C     VI1KSC        IFEQ      0
046500931130     C     VI1CSF        ANDNE     *BLANKS
046600931130     C                   SETON                                        5090
046700931130     C                   GOTO      ENDCT1
046800931130     C                   ENDIF
046900000808     C* CONTROLLO SU TNTLZ
047000000808    1C     VI1KSC        IFGT      0
047100000808     C     KTNTLZ        CHAIN     TNTLZ01L                           90
047200000808     C  N90TLZATB        IFNE      ' '
047300000808     C                   SETON                                        90
047400000808     C                   ENDIF
047500000808     C   90              SETON                                        46
047600000808     C   90              GOTO      ENDCT1
047700000808     C                   MOVEL     TLZRSC        VI1DKC
047800931126     C* SE IMMESSO UN SOLO PADRONCINO IL FORNITORE DEVE CORRISPONDERE
047900931126    2C     VI1PD1        IFGT      *ZEROS
048000931126     C     VI1PD2        ANDLE     *ZEROS
048100931126     C*
048200950125    3C     APDKSC        IFNE      VI1KSC
048300000808     C     APDCSF        ORNE      VI1CSF
048400000808     C                   SETON                                        4990
048500931126    3C                   ENDIF
048600931126    2C                   ENDIF
048700931126    1C                   ENDIF
048800020603     c* si può scelgliere il sintetico solo in simulazione
048900020604     c*                  if        vi1sml = *blanks and vi1tsp = '2'
049000020604     C*                  seton                                        5890
049100020604     c*                  end
049200910320     C*
049300900614     C     ENDCT1        ENDSR
049400931126     C*
049500931126     C* RICERCA CODICE PADRONCINO ------------------------------------*
049600931126     C     RICPAD        BEGSR
049700021203     C                   MOVEL     *BLANKS       D24RSC
049800021203     C                   Z-ADD     0             D24PDR
049900021203     C                   MOVEL     'R'           D24FLG
050000021203     C                   MOVEL     'A'           D24TIP
050100021203     C                   MOVEL(P)  FNLV24DS      KPJBU
050200950125     C                   CALL      'FNLV24R'
050300931126     C                   PARM                    KPJBA
050400021203     C                   MOVEL     KPJBU         FNLV24DS
050500931126     C                   ENDSR
