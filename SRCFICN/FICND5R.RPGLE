000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011115     H* FICND5R *----------------------------------------------------*
000300000808     H*         - STAMPA RIEPILOGO CONTEGGI PADRONCINI
000400000000     H*--------------------------------------------------------------*
000500011115     FFICND5D   CF   E             WORKSTN
000600000808     FTNTLZ01L  IF   E           K DISK
000700021203     FFIAPD01L  IF   E           K DISK
000800991228     FTABEL00F  IF   E           K DISK
000900160512     Ffiftt04l  IF   E           K DISK
001000930707     F*
001100950125     D* SCHIERE PER CARICAMENTO L1 E L6
001200950125     D L1              S              3  0 DIM(30)                              FIL GESTITE
001300950125     D L6              S              3  0 DIM(30)                              FIL GESTITE
001400920810     D KPJBA         E DS
001500020701     D* REM                    1      3
001600020701     D* REMFIL                 4      6
001700950125     D*
001800011115     D* PASSAGGIO DATI A PGM DI STAMPA RIEPILOGO         - FICND6R -
001900931126     D PARAM           DS
002000931126     D  PARP1                 28     34  0
002100931126     D  PARP2                 35     41  0
002200931126     D  VI1KSC                42     48  0
002300931209     D  VI1DKC                49     78
002400931202     D  VI1CSF                79     81
002500931210     D  VI1SML                82     82
002600940325     D  VI1TSP                83     83
002700950125     D  VI1DD1                84     91  0
002800950125     D  W01DD1                92     99  0
002900950125     D  VI1DD2               100    107  0
003000950125     D  W01DD2               108    115  0
003100000808     D* PASSAGGIO DATI ALLA RICERCA TRAZ./PADR.  - TRUL16R -
003200000808     D PARAM3          DS
003300000808     D* TIPOLOGIA P_=PADRONCINO  (T_=TRAZIONISTA)
003400000808     D  PA3TIP                 1      2
003500000808     D* RAGIONE SOCIALE
003600000808     D  PA3RSC                 3     37
003700000808     D* SISTEMA INFORMATIVO
003800000808     D  PA3CSF                38     40
003900000808     D* CODICE
004000000808     D  PA3PDR                41     47  0
004100000808     D* PA3FLG = "3" --> RESTITUISCE AL PGM CHIAMANTE IL MESSAGGIO CHE
004200000808     D*   NON ESISTONO RECORDS PER LA CHIAVE ALFABETICA RICHIESTA
004300000808     D  PA3FLG                48     48
004400950125     D*
004500950125     D* DS PER TRUL06R - CARICAMENTO �1
004600950125     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004700950125     D  LIN                    1     90  0
004800950125     D                                     DIM(30)                              SKI COMODO
004900910423     D WLBDAT          DS
005000950125     D  G02DAT                 1      8  0
005100950125     D  G02INV                 9     16  0
005200950125     D  G02ERR                17     17
005300950125     D  G02TGI                18     22  0
005400931015     D                 DS
005500931015     D  VI1PF1                 1      3  0
005600931116     D  VI1PD1                 4      7
005700931115     D  VI1P1                  1      7  0
005800931126     D                 DS
005900931126     D  VI1PF2                 1      3  0
006000931126     D  VI1PD2                 4      7
006100931126     D  VI1P2                  1      7  0
006200991228     D DS5A          E DS
006300021203     D FNLV24DS      E DS
006400991228     D CNCR80        E DS
006500900615     D UT�DSE        E DS                  EXTNAME(UT�DSE0F)
006600000000     I/SPACE 3
006700900521     C****************************************************************
006800900521     C*  RIEPILOGO INDICATORI
006900900521     C***************************************************************
007000931116     C* 30    - DI COMODO
007100010906     C* 40/56 - ERRORI
007200950125     C* 89    - SONO UN "REMXXX"
007300900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
007400900521     C*****************************************************************
007500000000     C     *ENTRY        PLIST
007600000000     C                   PARM                    KPJBA
007700000000     C                   Z-ADD     1             CODUT
007800900615     C                   CALL      'X�PARUT'
007900900615     C                   PARM                    UT�DSE
008000000000     C                   MOVEL     RAGUT         RSUT             20
008100900615     C                   MOVEL     REC80         CNCR80
008200031023     C                   MOVEL     knmus         Primi_tre         3
008300900511     C*---------------------------------------------------------------*
008400991228     C     KTAB          KLIST
008500991228     C                   KFLD                    CODUT
008600991228     C                   KFLD                    COD
008700991228     C                   KFLD                    KEY
008800000808     C*
008900160512     C     Kftt          KLIST
009000160512     C                   KFLD                    flgv              1
009100160512     C                   KFLD                    w01dd1
009200160512     C                   MOVEL     'C'           flgv
009300000808     C     KTNTLZ        KLIST
009400000808     C                   KFLD                    KTIP              2
009500000808     C                   KFLD                    VI1KSC
009600000808     C                   KFLD                    VI1CSF
009700000808     C*
009800000808     C                   MOVEL     'P '          KTIP
009900021203     C*
010000021203     C     KAPDV         KLIST
010100021203     C                   KFLD                    APDTIP
010200021203     C                   KFLD                    VI1P1
010300021203     C*
010400021203     C                   MOVEL     'A'           APDTIP
010500000808     C*
010600991228     C     *LIKE         DEFINE    TBLCOD        COD
010700991228     C     *LIKE         DEFINE    TBLKEY        KEY
010800991228     C     *LIKE         DEFINE    G02TGI        DD1GIO
010900991228     C     *LIKE         DEFINE    G02TGI        DD2GIO
011000991228     C     *LIKE         DEFINE    G02TGI        WRESTO
011100931116     C*---------------------------------------------------------------*
011200910419     C*
011300950411     C* OPERAZIONI INIZIALI
011400931111     C                   EXSR      PUL01
011500950411     C*
011600950411     C     INIZIO        TAG
011700031023      *
011800950411     C                   MOVEL     '0000'        VI1PD1
011900950411     C                   MOVEL     '9999'        VI1PD2
012000950411     C                   CLEAR                   VI1DD1
012100950411     C                   CLEAR                   VI1DD2
012200950411     C                   CLEAR                   VI1KSC
012300950411     C                   CLEAR                   VI1RKC
012400950411     C                   CLEAR                   VI1DKC
012500950411     C                   CLEAR                   VI1CSF
012600020517     C                   MOVEL     '1'           VI1TSP
012700930426     C*
012800931015     C     FOR01         TAG
012900950125     C                   EXFMT     LRD5D01
013000931116     C*
013100920323     C** CMD3 - FINE LAVORO
013200931112     C   KC              GOTO      FINE
013300931015     C* CONTROLLI FORMATO1
013400900528     C                   EXSR      CONTR1
013500950125     C  NKF
013600931116     COR 90              GOTO      FOR01
013700931116     C*
013800931202     C     VI1PD1        IFEQ      *BLANKS
013900931207     C                   MOVEL     *ZEROS        VI1PD1
014000931202     C                   ENDIF
014100931202     C     VI1PD2        IFEQ      *BLANKS
014200931207     C                   MOVEL     *ZEROS        VI1PD2
014300931202     C                   ENDIF
014400931126     C                   MOVE      VI1P1         PARP1
014500931126     C                   MOVEL     VI1P2         PARP2
014600931126     C                   MOVEL     PARAM         KPJBU
014700931202     C*
014800011115     c     knmus         ifeq      *all'1'
014900011115     C     vi1sml        ifeq      ' '
015000011115     C                   call      'FICND6R'
015100011115     C                   parm                    Kpjba
015200011115     C                   else
015300011115     C* RIEPILOGO CONTEGGI DI SIMULAZIONE
015400011115     C                   call      'FICNE0C'
015500011115     C                   parm                    Kpjba
015600011115     C                   end
015700011115     c                   else
015800931202     C* RIEPILOGO CONTEGGI EFFETTIVI
015900931202     C     VI1SML        IFEQ      ' '
016000011116     C                   MOVEL     'FND6'        KCOAZ
016100931202     C                   ELSE
016200931202     C* RIEPILOGO CONTEGGI DI SIMULAZIONE
016300011116     C                   MOVEL     'FNE0'        KCOAZ
016400931202     C                   ENDIF
016500931116     C                   CALL      'BCH10'
016600931116     C                   PARM                    KPJBA
016700011115     c                   end
016800900611     C*
016900950411     C                   GOTO      INIZIO
017000000000     C     FINE          TAG
017100931027     C                   SETON                                        LR
017200931111     C*
017300931111     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
017400931111     C     PUL01         BEGSR
017500931126     C* CARICO L1
017600950125     C                   CLEAR                   DSUL06
017700950125     C                   MOVE      '�1'          D06COD
017800950125     C                   MOVEL     SIMFEL        D06KEY
017900950125     C                   MOVEL     DSUL06        KPJBU
018000950125     C*
018100950125     C                   CALL      'TRUL06R'
018200950125     C                   PARM                    KPJBA
018300950125     C                   MOVEL     KPJBU         DSUL06
018400950125     C                   MOVEA     LIN           L1
018500931126     C*
018600950125     C***
018700950125     C* VEDO SE SONO SIMFEL O REMOTO
018800950125     C***
018900020701     C                   EVAL      Vi1PF1 = SimPOU
019000020701     C                   EVAL      Vi1PF2 = SimPOU
019100020701    1C*    REM           IFEQ      'REM'
019200020701     C*    REMFIL        ANDGT     *ZEROS
019300020701     C                   IF        SimTpP = '2' OR SimTpP = *BLANK
019400020701     C*                  MOVEL     REMFIL        VI1PF1
019500020701     C*                  MOVEL     REMFIL        VI1PF2
019600950125     C                   SETON                                        89
019700950125     C*
019800950125     C* CARICO TABELLA FILIALI GESTITE �6
019900950125     C                   CLEAR                   DSUL06
020000950125     C                   MOVE      '�6'          D06COD
020100020701     C*                  MOVEL     REMFIL        D06KEY
020200020701     C                   MOVEL     SimPOU        D06KEY
020300950125     C                   MOVEL     DSUL06        KPJBU
020400950125     C*
020500950125     C                   CALL      'TRUL06R'
020600950125     C                   PARM                    KPJBA
020700950125     C                   MOVEL     KPJBU         DSUL06
020800950125     C                   MOVEA     LIN           L6
020900950125     C*
021000950125     C                   ELSE
021100950125     C* SONO SIMFEL
021200950125     C                   MOVEL     SIMFEL        VI1PF1
021300991228     C                   MOVEL     SIMFEL        VI1PF2
021400950125     C                   ENDIF
021500991228     C                   MOVEL     '5A'          COD
021600991228     C                   MOVEL(P)  '1'           KEY
021700991228     C     KTAB          CHAIN     TABEL                              30
021800991228     C  N30              MOVEL     TBLUNI        DS5A
021900991228     C   30              CLEAR                   DS5A
022000000203     C                   TIME                    W0140            14 0
022100000203     C* UDATE IN GGMMAAAA
022200000203     C                   MOVE      W0140         WDTGIO            8 0
022300000203     C                   MOVE      WDTGIO        ANNOCO            4 0
022400000203     C     ANNOCO        ADD       1             ANNOPI            4 0
022500000203     C     ANNOCO        SUB       1             ANNOME            4 0
022600950125     C*
022700931111     C                   ENDSR
022800900528     C*
022900900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
023000900528     C     CONTR1        BEGSR
023100931111     C                   SETOFF                                       90
023200931126     C*
023300931126     C* D A T A   D I S T I N T A: DAL
023400931126     C                   MOVE      VI1DD1        G02DAT
023500931126     C                   MOVEL     *BLANK        G02ERR
023600950125     C                   CALL      'XSRDA8'
023700931126     C                   PARM                    WLBDAT
023800931126    1C     G02ERR        IFEQ      '1'
023900931126     C                   SETON                                        43  90
024000931126     C                   GOTO      ENDCT1
024100931126    1C                   END
024200931126     C                   Z-ADD     G02INV        W01DD1
024300950125     C                   Z-ADD     G02DAT        VI1DD1
024400991228     C                   Z-ADD     G02TGI        DD1GIO
024500011115     C                   z-add     vi1dd1        an1               4 0
024600000203     C* CONTROLLO L'ANNO
024700000203     C                   Z-ADD     G02DAT        W0040             4 0
024800000203     C     W0040         IFGT      ANNOPI
024900000203     C     W0040         ORLT      ANNOME
025000000203     C                   SETON                                        43  90
025100000203     C                   GOTO      ENDCT1
025200000203    1C                   END
025300931126     C*
025400021106     C* comunque imposta il 2 campo data
025500021106     C                   Z-ADD     w01dd1        w01dd2
025600931126    1C     VI1DD2        IFGT      0
025700931126     C                   MOVE      VI1DD2        G02DAT
025800931126     C                   MOVEL     *BLANK        G02ERR
025900950125     C                   CALL      'XSRDA8'
026000931126     C                   PARM                    WLBDAT
026100931126    2C     G02ERR        IFEQ      '1'
026200931126     C                   SETON                                        44  90
026300931126     C                   GOTO      ENDCT1
026400931126    2C                   END
026500931126     C                   Z-ADD     G02INV        W01DD2
026600950125     C                   Z-ADD     G02DAT        VI1DD2
026700991228     C                   Z-ADD     G02TGI        DD2GIO
026800011115     C                   z-add     vi1dd2        an2               4 0
026900000203     C* CONTROLLO L'ANNO
027000000203     C                   Z-ADD     G02DAT        W0040             4 0
027100011115     c     an1           ifne      an2
027200011115     C                   SETON                                        56  90
027300011115     C                   GOTO      ENDCT1
027400011115     c                   end
027500000203     C     W0040         IFGT      ANNOPI
027600000203     C     W0040         ORLT      ANNOME
027700000203     C                   SETON                                        44  90
027800000203     C                   GOTO      ENDCT1
027900000203    1C                   END
028000931126     C* DATA AL NO < DATA DAL
028100931126    2C     W01DD2        IFLT      W01DD1
028200931126     C                   SETON                                        45  90
028300931126     C                   GOTO      ENDCT1
028400931126    2C                   ENDIF
028500160512    1C                   ENDIF
028600160512     C* verifico se per la data inizio � presente almeno un conteggio valido
028700160512     Q*
028800160512     c                   clear                   trovato           1
028900160512     c     kftt          setll     fiftt04l
029000160512     c                   do        *hival
029100170223     c     flgv          reade     fiftt04l
029200170223      **
029300170223     c                   if        %eof(fiftt04l) or
029400170223     c                             not %eof(fiftt04l) and fttddc > W01DD2
029500160512     c                   leave
029600160512     c                   endif
029700170223      **
029800160512     c                   if        ftttsr = *blank
029900160512     c                   move      'X'           trovato
030000160512     c                   leave
030100160512     c                   endif
030200170223      **
030300160512     c                   enddo
030400170223      **
030500160512     c     trovato       comp      ' '                                    55
030600160512     c   55              seton                                        90
030700170223     C   90              GOTO      ENDCT1
030800991228     C**
030900031023     C* Se non � un utente EDPxxx controlla la filiale inserita
031000091113     c                   if        simTPP <> 'S' or
031100130118     c                             (primi_tre <> 'EDP' and primi_tre <> 'RIC'
031200151112     c                              and primi_tre <> 'GEN' and
031300151112     c                              primi_tre <> 'DLO' )
031400031023     C*
031500950125     C* F I L I A L E    P A D R O N C I N O    D A L
031600950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
031700950125    1C     *IN89         IFEQ      *OFF
031800950125     C     VI1PF1        IFNE      SIMFEL
031900080109     C* non pu� essere (0) la filiale
032000080109     c                   if        VI1PF1 = 0
032100080109     c                   z-add     simfel        VI1PF1
032200080109     c                   end
032300950125     C     VI1PF1        LOOKUP    L1                                     35
032400950125     C  N35              SETON                                        5390
032500950125     C  N35              GOTO      ENDCT1
032600950125     C                   ENDIF
032700950125     C*
032800950125   X1C                   ELSE
032900950125     C* 89 ON  - SONO UN "REMXXX" QUINDI CONTROLLO LA FILIALE NELLA L6
033000020701     C                   MOVEL     VI1PF1        WFIL              3 0
033100020701     C*    WFIL          IFNE      REMFIL
033200020701     C     WFIL          IFNE      SimPOU
033300950125     C     VI1PF1        LOOKUP    L6                                     35
033400950125     C  N35              SETON                                        5390
033500950125     C  N35              GOTO      ENDCT1
033600950125     C                   ENDIF
033700950125    1C                   ENDIF
033800021015      *
033900031023     c                   end
034000031023      *
034100021015     c                   z-add     vi1pf1        vi1pf2
034200031023     C*
034300031023     C* Se non � un utente EDPxxx controlla la filiale inserita
034400091113     c*mm                if        primi_tre <> 'EDP' or simTPP <> 'S'
034500091113     c                   if        simTPP <> 'S' or
034600130118     c                             (primi_tre <> 'EDP' and primi_tre <> 'RIC'
034700151112     c                              and primi_tre <> 'GEN'
034800151112     c                              and primi_tre <> 'DLO')
034900950125     C*
035000950125     C* F I L I A L E    P A D R O N C I N O    A L
035100950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
035200950125    1C     *IN89         IFEQ      *OFF
035300950125     C     VI1PF2        IFNE      SIMFEL
035400950125     C     VI1PF2        LOOKUP    L1                                     35
035500950125     C  N35              SETON                                        5490
035600950125     C  N35              GOTO      ENDCT1
035700950125     C                   ENDIF
035800950125     C*
035900950125   X1C                   ELSE
036000950125     C* 89 ON  - SONO UN "REMXXX" QUINDI CONTROLLO LA FILIALE NELLA L6
036100020701     C                   MOVEL     VI1PF2        WFIL
036200020701     C*    WFIL          IFNE      REMFIL
036300020701     C     WFIL          IFNE      SimPOU
036400950125     C     VI1PF2        LOOKUP    L6                                     35
036500950125     C  N35              SETON                                        5490
036600950125     C  N35              GOTO      ENDCT1
036700950125     C                   ENDIF
036800950125    1C                   ENDIF
036900031023      *
037000031023     c                   end
037100950125     C*
037200950131     C* CONTROLLO CONGRUENZA FILIALI DAL-AL
037300950131    1C     VI1PF1        IFNE      VI1PF2
037400950131     C                   SETON                                        4890
037500950131     C                   GOTO      ENDCT1
037600950131    1C                   ENDIF
037700950131     C*
037800931116     C* P A D R O N C I N O
037900931116     C* RICERCA
038000931116     C     '?'           SCAN      VI1PD1                                 90
038100931116     C*
038200931126    1C     *IN90         IFEQ      *ON
038300021203     C                   Z-ADD     VI1PF1        D24FIL
038400931126     C                   EXSR      RICPAD
038500931126     C*
038600021203     C     D24PDR        IFNE      0
038700021203     C                   MOVEL     D24PDR        VI1PF1
038800931116     C                   MOVEL     *ZEROS        VI1PD1
038900021203     C                   MOVE      D24PDR        VI1PD1
039000021203     C                   MOVEL     D24RSC        VI1DP1
039100950125     C                   ENDIF
039200931116     C                   GOTO      ENDCT1
039300931126    1C                   ENDIF
039400931126     C* PADRONCINO AL
039500931126     C* RICERCA
039600931126     C     '?'           SCAN      VI1PD2                                 90
039700931126     C*
039800931126    1C     *IN90         IFEQ      *ON
039900021203     C                   Z-ADD     VI1PF2        D24FIL
040000931126     C                   EXSR      RICPAD
040100931126     C*
040200021203     C     D24PDR        IFNE      0
040300021203     C                   MOVEL     D24PDR        VI1PF2
040400931126     C                   MOVEL     *ZEROS        VI1PD2
040500021203     C                   MOVE      D24PDR        VI1PD2
040600950125     C                   ENDIF
040700931126     C                   GOTO      ENDCT1
040800931126    1C                   ENDIF
040900931126     C*
041000931126     C*
041100931126    1C     VI1PD2        IFNE      *BLANKS
041200931126     C     VI1PD2        ANDNE     *ZEROS
041300931126     C                   TESTN                   VI1PD2               30
041400931126     C  N30              SETON                                        4790
041500931126     C                   ENDIF
041600931116     C*
041700931126    1C     VI1PD1        IFNE      *BLANKS
041800931126     C     VI1PD1        ANDNE     *ZEROS
041900931126     C                   TESTN                   VI1PD1               30
042000931126     C  N30              SETON                                        4190
042100931116     C*
042200931123     C* SE NE HO IMMESSO SOLTANTO UNO CONTROLLO L'ESISTENZ
042300931126    2C     VI1PD2        IFEQ      *BLANKS
042400931126     C     VI1PD2        OREQ      *ZEROS
042500931126     C*
042600021203     C     KAPDV         CHAIN     FIAPD000                           30
042700931015     C*
042800931126    3C     *IN30         IFEQ      *ON
042900020520     C* Tolto il controllo sul padroncino annullato
043000020520     C                   SETON                                        4190
043100931028     C                   GOTO      ENDCT1
043200931126    3C                   ENDIF
043300931129     C*
043400100223     C                   MOVEL     APDRSf        VI1DP1
043500931123     C*
043600931123   X2C                   ELSE
043700931126     C*
043800931123     C* PADRONCINO AL  <  MINORE PADRONCINO DAL
043900950125    3C     VI1P2         IFLT      VI1P1
044000931123     C                   SETON                                        4290
044100931123     C                   GOTO      ENDCT1
044200931126    3C                   ENDIF
044300931126    2C                   ENDIF
044400931123    1C                   ENDIF
044500010906     C*
044600010906     C* PADRONCINO AL E PADRONCINO DAL A ZERO
044700010906     C     VI1PD1        IFEQ      *ZEROS
044800010906     C     VI1PD2        ANDEQ     *ZEROS
044900010906     C                   SETON                                        4090
045000010906     C                   GOTO      ENDCT1
045100010906     C                   ENDIF
045200931126     C*
045300931126     C* C O D I C E   F O R N I T O R E
045400931126     C* RICERCA
045500931126    1C     VI1RKC        IFEQ      '?'
045600931126     C     VI1KSC        ANDEQ     0
045700931126     C                   SETON                                        90
045800931126     C                   CLEAR                   VI1RKC
045900000808     C                   MOVEL     'P '          PA3TIP                         PAR-1
046000000808     C                   MOVEL(P)  VI1DKC        PA3RSC                         PAR-2
046100000808     C                   MOVEL     VI1CSF        PA3CSF                         PAR-3
046200000808     C                   CLEAR                   PA3PDR                         PAR-4
046300000808     C                   CLEAR                   PA3FLG                         PAR-5
046400000808     C                   MOVEL     PARAM3        KPJBU
046500000808     C                   CALL      'TRUL16R'
046600000808     C                   PARM                    KPJBA
046700000808     C                   MOVEL     KPJBU         PARAM3
046800000808     C                   MOVEL     PA3PDR        VI1KSC                         CODICE
046900000808     C                   MOVEL     PA3RSC        VI1DKC                         RAG.SOCIALE
047000000808     C                   MOVEL     PA3CSF        VI1CSF                         S.I.
047100000808     C     PA3FLG        IFEQ      '3'
047200000808     C                   SETON                                        5290
047300000808     C                   ENDIF
047400000808     C                   GOTO      ENDCT1
047500931126    1C                   ENDIF
047600931130     C*
047700931130     C* FORNITORE E SISTEMA INFORMATIVO: DEVONO ESSERCI ENTRAMBI
047800931130     C     VI1KSC        IFGT      0
047900931130     C     VI1CSF        ANDEQ     *BLANKS
048000931130     C                   SETON                                        5190
048100931130     C                   GOTO      ENDCT1
048200931130     C                   ENDIF
048300931130     C     VI1KSC        IFEQ      0
048400931130     C     VI1CSF        ANDNE     *BLANKS
048500931130     C                   SETON                                        5090
048600931130     C                   GOTO      ENDCT1
048700931130     C                   ENDIF
048800000808     C* CONTROLLO SU TNTLZ
048900000808    1C     VI1KSC        IFGT      0
049000000808     C     KTNTLZ        CHAIN     TNTLZ01L                           90
049100000808     C  N90TLZATB        IFNE      ' '
049200000808     C                   SETON                                        90
049300000808     C                   ENDIF
049400000808     C   90              SETON                                        46
049500000808     C   90              GOTO      ENDCT1
049600000808     C                   MOVEL     TLZRSC        VI1DKC
049700931126     C* SE IMMESSO UN SOLO PADRONCINO IL FORNITORE DEVE CORRISPONDERE
049800931126    2C     VI1PD1        IFGT      *ZEROS
049900931126     C     VI1PD2        ANDLE     *ZEROS
050000931126     C*
050100950125    3C     APDKSC        IFNE      VI1KSC
050200000808     C     APDCSF        ORNE      VI1CSF
050300000808     C                   SETON                                        4990
050400931126    3C                   ENDIF
050500931126    2C                   ENDIF
050600931126    1C                   ENDIF
050700020603     c* si pu� scelgliere il sintetico solo in simulazione
050800020604     c*                  if        vi1sml = *blanks and vi1tsp = '2'
050900020604     C*                  seton                                        5890
051000020604     c*                  end
051100910320     C*
051200900614     C     ENDCT1        ENDSR
051300931126     C*
051400931126     C* RICERCA CODICE PADRONCINO ------------------------------------*
051500931126     C     RICPAD        BEGSR
051600021203     C                   MOVEL     *BLANKS       D24RSC
051700021203     C                   Z-ADD     0             D24PDR
051800021203     C                   MOVEL     'R'           D24FLG
051900021203     C                   MOVEL     'A'           D24TIP
052000021203     C                   MOVEL(P)  FNLV24DS      KPJBU
052100950125     C                   CALL      'FNLV24R'
052200931126     C                   PARM                    KPJBA
052300021203     C                   MOVEL     KPJBU         FNLV24DS
052400931126     C                   ENDSR
