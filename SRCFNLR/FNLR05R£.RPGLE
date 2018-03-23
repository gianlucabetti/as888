000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941012     H* FNLR05R *----------------------------------------------------*
000300910320     H*         - STAMPA SPUNTA FILTRO
000400000000     H*--------------------------------------------------------------*
000500941012     FFNLR05D   CF   E             WORKSTN
000600070115     FFNBRV01L  IF   E           K DISK
000700941017     FFNFGV01L  IF   E           K DISK
000800020927     fFNFGW01L  if   e           k disk    rename(FNFGW000:FNFGW001)
000900020927     fFNFGW02L  if   e           k disk    rename(FNFGW000:FNFGW002)
001000941027     FFNFVA01L  IF   E           K DISK
001100020927     fFNFWA01L  if   e           k disk    rename(FNFWA000:FNFWA001)
001200020927     fFNFWA02L  if   e           k disk    rename(FNFWA000:FNFWA002)
001300941013     FFNFVV01L  IF   E           K DISK
001400920521     FAZORG01L  IF   E           K DISK
001500920807     FTABEL00F  IF   E           K DISK
001600020808     D*
001700020808     D KPJBA         E DS
001800020808     D***                                     1   3 REM
001900020808     D***                                     4   6 REMFIL
002000020808     D UT§DSE0F      E DS
002100020808     D CNCR80        E DS
002200020808     D DS7N          E DS
002300020808     D  §7NDE2                 1     10
002400020808     D* DS STANDARD FOGLI VARI
002500020808     D DSLR01        E DS
002600020927     d* Parametri per pgm. FNLV55R
002700020927     d FNLV55DS      e ds                  inz
002800020927     d  D55tpt       e                     inz('P')
002900020927     d*
003000020927     d OG143         e ds
003100020808     D*
003200020808     D* DS PER TRUL06R - CARICAMENTO £X
003300020808     D TRUL06DS      E DS
003400020808     D  LIN                    1     90  0
003500020808     D                                     DIM(30)                              FIL. COMODO
003600941013     D*
003700920807     D DEC             S             12    DIM(6)                               DESCR CATEGOR
003800941013     D L1              S              3  0 DIM(30)                              FIL GESTITE
003900941014     D FVA             S              9  0 DIM(70)                              FVA DA INTERR
004000941013     D*
004100941027     D* CHAIAMATA AL PGM DI RICERCA FOGLI PARTENZA ALL'ARRIVO -FNLR26R-
004200941014     D PARAM2          DS
004300941014     D  PA2PGM                 1      7
004400941014     D  PA2LAI                 8     10  0
004500941014     D  PA2FEL                11     13  0
004600941014     D  PA2ABB                14     14
004700941014     D  PA2DSD                15     22  0
004800941014     D  PA2DSA                23     30  0
004900941014     D  PA2FLG                31     31
005000020808     ***D  VIDFIL                32     34  0
005100020808     D  PA2fil                32     34  0
005200971002     D  PA2LAN                35     35    INZ('4')
005300980204     D  PA2MSG                36    113    INZ
005400951109     D* CHIAMATA AL PGM DI INTERROGAZ.FV PARTENZA             -FNLS53R-
005500941013     D PARAM5          DS
005600951109     D  PA5FLG                 1      1
005700951109     D  PA5FL2                 2      2
005800951109     D  PA5NFV                 3      5P 0
005900951109     D  PA5DIN                 6     10P 0
006000951109     D  PA5DFI                11     15P 0
006100951109     D  PA5TFV                16     16
006200000614     D  PA5FGS               242    244  0
006300951109     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
006400951109     D*   SELEZIONATI ---> SE NON UTILIZZATO NON LO DEFINISCO A PGM
006500020808     D**FVA                   17    241P 0
006600920807     D TBLKEY          DS
006700920807     D  ALF1                   1      1
006800920707     D WLBDAT          DS
006900941012     D  G02DAT                 1      8  0
007000941012     D  G02INV                 9     16  0
007100941012     D  G02ERR                17     17
007200941012     D  G02TGI                18     22  0
007300920707     D WGIDAT          DS
007400941012     D  GIODAT                 1      8  0
007500941012     D  GIOINV                 9     16  0
007600941012     D  GIOTGI                17     21  0
007700020808     D*
007800020808     D COD             S                   LIKE(TBLCOD)                         CODICE TABELLA
007900020808     D KEY             S                   LIKE(TBLKEY)                         ELEMENTO
008000020808     d COMfil          s                   like(FVVfgs) inz
008100020808     D COMNPG          S                   LIKE(FVVNPG)                         CATEGORIA FOGLIO
008200020808     D COMFVD          S                   LIKE(FVVNFV)
008300020808     D WFVD            S                   LIKE(BRVNFV)
008400900702     C****************************************************************
008500900702     C*  RIEPILOGO INDICATORI
008600900702     C***************************************************************
008700020927     C* 30/32 - DI COMODO
008800941020     C* 40/48 - ERRORE
008900941013     C* 50    - ERRORE
009000941013     C* 90    - GENERICO DI ERRORE
009100900702     C*****************************************************************
009200000000     C     *ENTRY        PLIST
009300000000     C                   PARM                    KPJBA
009400000000     C                   Z-ADD     1             CODUT
009500920807     C                   CALL      'X§PARUT'
009600020808     C                   PARM                    UT§DSE0F
009700941027     C                   MOVEL     RAGUT         VIDDSA
009800920807     C                   MOVEL     REC80         CNCR80
009900900511     C*---------------------------------------------------------------*
010000941012     C                   TIME                    WTIME            14 0          ORA E DATA
010100941012     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
010200920707     C*
010300950616     C* DATA INIZIO INTERROGAZIONE FOGLI - 5
010400941012     C                   MOVE      WDATE         G02DAT
010500920707     C                   MOVEL     *BLANK        G02ERR
010600941012     C                   CALL      'XSRDA8'
010700920707     C                   PARM                    WLBDAT
010800020927     ***C                   Z-ADD     G02DAT        WDATEU            8 0          AAAA/MM/GG
010900020927     C                   Z-ADD     G02INV        WDATEU            8 0          AAAA/MM/GG
011000950616     C* SOTRAGGO 5 AI GIORNI
011100950616     C     G02TGI        SUB       5             GIOTGI
011200941012     C                   CALL      'XSRGI8'
011300920707     C                   PARM                    WGIDAT
011400941013     C                   Z-ADD     GIODAT        SAVDIN            8 0          GG/MM/AAAA
011500941013     C                   Z-ADD     GIOINV        SAVDI1            8 0          AAAA/MM/GG
011600920807     C*
011700920807     C                   EXSR      CARCAT
011800920707     C*
011900920521     C                   Z-ADD     0             VIDLNA
012000941012     C                   MOVEL     '0=TUTTE'     DESLNA
012100920707     C                   MOVEL     *BLANKS       VIDFVD
012200920707     C                   Z-ADD     0             COMFVD
012300920707     C                   Z-ADD     0             VIDFVA
012400920807     C                   MOVEL     ' '           VIDNPG
012500910522     C                   MOVEL     'S'           VIDSN
012600941020     C                   Z-ADD     *ZERO         VIDPID                         PISTOLA INIZIALE
012700941020     C                   Z-ADD     *HIVAL        VIDPIA                         PISTOLA FINALE
012800941020     C                   Z-ADD     *ZERO         VIDSRD                         SERIE INIZIALE
012900941020     C                   Z-ADD     *HIVAL        VIDSRA                         SERIE FINALE
013000920707     C*
013100941012     C* EMETTO FORMATO VIDEO PER SCELTA PARAMETRI
013200900509     C     FOR01         TAG
013300941012     C                   EXFMT     LR05D01
013400941020     C                   SETOFF                                       90
013500941012     C*
013600910320     C** CMD3 - FINE LAVORO
013700900509     C   KC              GOTO      FINE
013800941027     C*
013900941027     C** CMD7 - INTERROGAZIONE FOGLI VARI
014000941027     C     *INKG         IFEQ      *ON
014100941027     C                   CLEAR                   DSLR01
014200950615     C                   MOVEL     '§'           DLRRIC
014300950615     C                   MOVEL     'FNLR05R'     DLRPGM                         PROGRAMMA
014400950615     C                   Z-ADD     VIDFIL        DLRFGS                         FIL.GESTIONE
014500950615     C* DATA FINALE: DATA DEL GIORNO
014600950615     C                   MOVE      WDATE         DLRGAL                         GG/MM/AAAA
014700950615     C                   MOVE      WDATEU        DLRAAL                         AAAA/MM/GG
014800950615     C                   CLEAR                   KPJBU
014900941027     C                   MOVEL     DSLR01        KPJBU
015000941027     C                   CALL      'FNLR02R'
015100941027     C                   PARM                    KPJBA
015200950615     C                   MOVEL     KPJBU         DSLR01
015300950615     C*
015400950615     C     DLRERR        IFEQ      *BLANKS                                      NO ERRORE
015500950615     C                   MOVEL     DLRNPG        VIDNPG
015600000121     C                   MOVEL     DLRNFV        VIDFVD
015700950615     C                   ENDIF
015800950615     C*
015900941027     C                   GOTO      FOR01
016000941027     C                   ENDIF
016100950615     C*
016200950615     C** CMD14- INTERROGAZIONE FOGLI VIAGGIO PARTENZA
016300950615     C     *INKN         IFEQ      *ON
016400950615     C                   CLEAR                   PARAM5
016500951109     C                   MOVEL     'P'           PA5FLG
016600950615     C* DATA FINALE: DATA DEL GIORNO
016700951109     C                   Z-ADD     WDATE         PA5DFI
016800020808     ***C                   Z-ADD     0             PA5FGS
016900020808     C                   z-add     VIDfil        PA5fgs
017000950615     C                   MOVEL     PARAM5        KPJBU
017100951109     C                   CALL      'FNLS53R'
017200950615     C                   PARM                    KPJBA
017300950615     C                   MOVEL     KPJBU         PARAM5
017400020808     C                   z-add     PA5fgs        VIDfil
017500950615     C                   MOVEL     PA5NFV        VIDFVD
017600950616     C     PA5NFV        IFGT      0
017700950616     C                   MOVEL     '1'           VIDNPG
017800950616     C                   ELSE
017900950616     C                   CLEAR                   VIDNPG
018000950616     C                   ENDIF
018100950616     C*
018200950615     C                   GOTO      FOR01
018300950615     C                   ENDIF
018400920707     C*
018500910801     C*  CONTROLLI
018600941020     C  N90              EXSR      CONTR                                        CONTROLLI VIDEO
018700910320     C*
018800941012     C  NKF
018900910801     COR 90              GOTO      FOR01
019000941012     C*
019100941012     C* CMD6 - ELABORAZIONE
019200910801     C     VIDFVA        IFEQ      0
019300920707     C                   MOVEL     COMFVD        VIDFVA
019400910801     C                   END
019500941027     C*
019600941027     C* PREPARO LA DSLR01 DA PASSARE AL PGM DI STAMPA SPUNTA
019700941027     C                   CLEAR                   DSLR01
019800941027     C                   MOVEL     VIDNPG        DLRNPG
019900941027     C                   MOVEL     VIDLNA        DLRLNA
020000941027     C                   MOVEL     VIDPID        DLRNPD
020100941027     C                   MOVEL     VIDPIA        DLRNPA
020200941027     C                   MOVEL     VIDSRD        DLRSRD
020300941027     C                   MOVEL     VIDSRA        DLRSRA
020400941027     C                   MOVEL     VIDFIL        DLRFGS
020500941020     C*
020600941027     C* FIN TANTO CHE CI SONO FOGLI LANCIO L'AZIONE
020700941027    1C     COMFVD        DOWLE     VIDFVA
020800941027     C*
020900920807     C* CONTROLLO SE DI FILIALE GESTIBILE
021000000121     C                   Z-ADD     COMFVD        WFVD
021100020927     c                   if        §OGntw = 'PPT'
021200020927     c                             and VIDnpg = '1'
021300020927     c                   z-add     D55tfp        COMfil
021400020927     c                   else
021500020927     c                   z-add     VIDfil        COMfil
021600020927     c                   endif
021700070115     C     KBRV          SETLL     FNBRV01L                               31
021800941027    2C     *IN31         IFEQ      *ON
021900910801     C*
022000941027     C                   MOVEL     COMFVD        DLRNFV
022100941027     C                   MOVEL     'S'           DLRFS1
022200941027     C                   MOVEL     DSLR01        KPJBU
022300941012     C                   MOVEL     'LR06'        KCOAZ
022400020927     C                   CALL      'BCH10'
022500900702     C                   PARM                    KPJBA
022600941020     C*
022700910522     C* LANCIO LISTA SEGNACOLLI CON ANOMALIA SE RICHIESTA STAMPA
022800941027    3C     VIDSN         IFEQ      'S'
022900941027     C                   MOVEL     'A'           DLRFS1
023000941027     C                   MOVEL     DSLR01        KPJBU
023100941027     C                   MOVEL     'LR06'        KCOAZ
023200020927     C                   CALL      'BCH10'
023300910522     C                   PARM                    KPJBA
023400941027    3C                   END
023500910522     C*
023600941027    2C                   ENDIF
023700941027     C*
023800920707     C                   ADD       1             COMFVD
023900941027    1C                   ENDDO
024000910320     C*
024100000000     C     FINE          TAG
024200020927     c                   clear                   FNLV55DS
024300020927     c                   eval      D55tla = 'C'
024400020927     c                   call      'FNLV55R'
024500020927     c                   parm                    FNLV55DS
024600000000     C                   SETON                                        LR
024700941012     C**************************************************************************
024800941027     C* ? DI RICERCA NEI FOGLI
024900941012     C**************************************************************************
025000941013     C     INTER         BEGSR
025100941013     C*
025200941013     C* VEDO SE IMMESSO ? DI RICERCA
025300941013     C                   SETOFF                                       32
025400941027     C     '?'           SCAN      VIDFVD                                 32     ON=TROVATO
025500941012     C*
025600941027    1C     *IN32         IFEQ      *ON
025700941013     C*
025800941013     C                   SETON                                        90
025900941027     C                   CLEAR                   VIDFVD
026000941013     C*
026100941013     C*?CATEGORIA 1:  F.V. PARTENZE (CERCARE IN FNFGV00F)                      ?
026200941027    2C     VIDNPG        IFEQ      '1'
026300941027     C*
026400941027     C* VERIFICO SE LNP E' PRESENTE IN £1
026500941027     C                   SETOFF                                       31
026600941027     C     VIDFIL        LOOKUP    L1                                     31     ON=TROVATO
026700941027     C*
026800941027    3C     *IN31         IFEQ      *ON
026900941027     C*
027000941013     C                   CLEAR                   PARAM5
027100951109     C                   MOVEL     'R'           PA5FLG
027200941013     C* DATA FINALE: DATA DEL GIORNO
027300951109     C                   Z-ADD     WDATE         PA5DFI
027400950616     C                   Z-ADD     SAVDIN        PA5DIN
027500020808     ***C                   Z-ADD     0             PA5FGS
027600020808     C                   z-add     VIDfil        PA5fgs
027700941013     C                   MOVEL     *BLANKS       KPJBU
027800941013     C                   MOVEL     PARAM5        KPJBU
027900951109     C                   CALL      'FNLS53R'
028000941013     C                   PARM                    KPJBA
028100941027     C                   MOVEL     KPJBU         PARAM5
028200941027     C                   MOVEL     PA5NFV        VIDFVD
028300020808     C***                z-add     PA5fgs        VIDfil
028400941027     C*
028500941027   X3C                   ELSE
028600941027     C*
028700941027     C*?CATEGORIA =1 CON LNP NON PRESENTE IN £1:  CERCARE IN FNFVA00F          ?
028800941027     C                   MOVEL     'FNLR05R '    PA2PGM
028900941027     C                   Z-ADD     *ZERO         PA2LAI
029000941027     C                   MOVEL     SIMFEL        PA2FEL
029100941027     C                   MOVEL     *BLANKS       PA2ABB
029200950616     C                   Z-ADD     SAVDI1        PA2DSD
029300941027     C                   Z-ADD     99991231      PA2DSA
029400941027     C                   MOVEL     'I'           PA2FLG                         'I'=INTERROGAZIONE
029500020927     c                   if        §OGntw = 'PPT'
029600020927     c                   z-add     D55tfp        PA2fil
029700020927     c                   else
029800020927     c                   z-add     VIDfil        PA2fil
029900020927     c                   endif
030000941027     C                   CLEAR                   KPJBU
030100941027     C                   MOVEL     PARAM2        KPJBU
030200941027     C                   MOVEA     *ZEROS        FVA
030300941027     C*
030400941027     C                   CALL      'FNLR26R'
030500941027     C                   PARM                    KPJBA
030600941027     C                   PARM                    FVA
030700941027     C                   MOVEL     KPJBU         PARAM2
030800941027     C                   MOVE      FVA(1)        VIDFVD
030900941027    3C                   ENDIF
031000941027     C*
031100941027   X2C                   ELSE
031200941013     C*
031300941027     C*?CATEGORIA >1         : CERCARE IN FNFVV00F                             ?
031400941013     C                   CLEAR                   DSLR01
031500941013     C                   MOVEL     'S'           DLRRIC                         'S'=RICHIAMATO
031600950615     C                   MOVEL     'FNLR05R'     DLRPGM                         PROGRAMMA
031700941027     C                   Z-ADD     SAVDIN        DLRGDA                         DAL GG/MM/AAAA
031800941013     C                   MOVE      31129999      DLRGAL                         AL GG/MM/AAAA
031900941027     C                   Z-ADD     SAVDI1        DLRADA                         DAL AAAA/MM/GG
032000941013     C                   Z-ADD     99991231      DLRAAL                         AL AAAA/MM/GG
032100941013     C                   Z-ADD     VIDFIL        DLRFGS                         FIL.GESTIONE
032200941027     C                   MOVEL     VIDNPG        DLRNPG                         CATEGORIA
032300941013     C                   CLEAR                   KPJBU
032400941013     C                   MOVEL     DSLR01        KPJBU
032500941013     C*
032600941013     C                   CALL      'FNLR02R'
032700941013     C                   PARM                    KPJBA
032800941013     C                   MOVEL     KPJBU         DSLR01
032900941027     C*
033000941027    3C     DLRERR        IFEQ      *BLANKS                                      NO ERRORE
033100941013     C                   MOVEL     DLRNFV        VIDFVD
033200941027    3C                   ENDIF
033300941013     C*
033400941027    2C                   ENDIF
033500941013     C*
033600941027    1C                   ENDIF
033700941013     C*
033800941013     C                   ENDSR
033900941013     C**************************************************************************
034000941013     C* CONTROLLO CAMPI
034100941013     C**************************************************************************
034200941013     C     CONTR         BEGSR
034300941012     C*
034400941012     C* FILIALE IN GESTIONE
034500941012     C     VIDFIL        CHAIN     AZORG01L                           50
034600941012     C   50              SETON                                        90
034700941012     C   50              GOTO      ENDCTR
034800941012     C                   MOVEL     ORGDES        DESFIL                         DECODIFICA
034900020927     c                   movel     ORGde3        OG143
035000020927     c                   move      VIDfil        COMfil
035100020927     c                   reset                   FNLV55ds
035200020927     c                   if        §OGntw = 'PPT'
035300020927     c                   eval      D55lin = VIDfil
035400020927     c                   eval      D55drf = WDATEU
035500020927     c                   call      'FNLV55R'
035600020927     c                   parm                    FNLV55DS
035700020927     c***                if        D55err <> *blanks
035800020927     c***                eval      D55tfp =  D55lin
035900020927     c***                endif
036000020927     c                   endif
036100941012     C*
036200910320     C* CONTROLLO
036300920807     C* CONTROLLO CATEGORIA FOGLI
036400920807     C     VIDNPG        IFEQ      ' '
036500920807     C                   SETON                                        4090
036600920807     C                   GOTO      ENDCTR
036700920807     C                   END
036800920807     C* RICERCA IN TABELLA
036900920807     C     VIDNPG        IFEQ      '?'
037000920807     C                   MOVEL     CODUT         §KUT
037100920807     C                   MOVEL     '7N'          §COD
037200920807     C                   MOVE      ' '           VIDNPG
037300920807     C                   MOVEL     *BLANKS       §KEY
037400920807     C                   CALL      'X§TABER'
037500920807     C                   PARM                    §KUT              1 0
037600920807     C                   PARM                    §COD              2
037700920807     C                   PARM                    §KEY              8
037800920807     C                   PARM                    §DES             30
037900920807     C                   MOVEL     §KEY          VIDNPG
038000920807     C                   SETON                                        90
038100920807     C                   GOTO      ENDCTR
038200920807     C                   END
038300920807     C* CONTROLLO
038400920807     C                   MOVEL     '7N'          COD
038500920807     C                   MOVEL     *BLANKS       KEY
038600920807     C                   MOVEL     VIDNPG        KEY
038700920807     C* INESISTENTE
038800941013     C     KTAB2         CHAIN     TABEL00F                           40
038900920807     C* ANNULLATA
039000920807     C  N40TBLFLG        IFNE      ' '
039100920807     C                   SETON                                        40
039200920807     C                   ELSE
039300920807     C                   MOVEL     TBLUNI        DS7N
039400920807     C                   END
039500920807     C*
039600920807     C   40              SETON                                        90
039700920807     C   40              GOTO      ENDCTR
039800920807     C                   MOVEL     VIDNPG        COMNPG
039900941027     C* VEDO SE C'E' INTERROGAZIONE SUI FOGLI
040000941027     C                   EXSR      INTER                                        INTERROGAZIONI
040100941027     C   90              GOTO      ENDCTR
040200930129     C*
040300941027     C* FOGLIO DAL OBBLIGATORIO
040400920707     C                   TESTN                   VIDFVD               30
040500920707     C   30VIDFVD        IFLE      *ZEROS
040600920707     C                   SETOFF                                       30
040700920707     C                   END
040800920707     C  N30              SETON                                        4190
040900920707     C  N30              GOTO      ENDCTR
041000920707     C*
041100920707     C                   MOVEL     VIDFVD        COMFVD
041200941027     C*
041300941027     C* SE RICHIESTO UN UNICO FOGLIO CONTROLLO FOGLIO
041400941027    1C     VIDFVA        IFEQ      *ZERO
041500941027     C     VIDFVA        OREQ      COMFVD
041600920807     C*
041700941027     C*?  .. SU FNFGV  <---  SE CATEGORIA = 1 & LNP=SIMFEL     ?
041800941027     C*?  .. SU FNFVA  <---  SE CATEGORIA = 1 & LNP NON IN £1  ?
041900941027     C*?  .. SU FNFVV  <---  SE CATEGORIA > 1                  ?
042000941017     C*
042100941027    2C     VIDNPG        IFEQ      '1'                                          ? '1' = PARTENZE ?
042200020927
042300941027     C* VERIFICO SE LNP E' PRESENTE IN £1
042400941027     C                   SETOFF                                       31
042500941027     C     VIDFIL        LOOKUP    L1                                     31     ON=TROVATO
042600941027    3C     *IN31         IFEQ      *ON                                          C'E' IN £1
042700941017     C*
042800020927     ***C     COMFVD        CHAIN     FNFGV01L                           30
042900020927    4c                   if        §OGntw = 'PPT'
043000020927      * - se p.o. Poste controllo su FNFGW02L
043100020927     c     KFVA1         chain     FNFGW02L                           30
043200020927     c  N30FGWatb        comp      *blanks                            3030      NO ANNULLATO
043300020927   x4c                   else
043400020927      * - altrimenti controllo su FNFGV01L
043500020808     C     K02FGV01      CHAIN     FNFGV01L                           30
043600941017     C  N30FGVATB        COMP      *BLANKS                            3030      NO ANNULLATO
043700020927    5c                   if        not *in30
043800020927      * - se p.o. non poste controllo che il p.o. gestione non sia poste
043900020927     c     K02FGV01      chain     FNFGW01L
044000020927    6c                   if        %found(FNFGW01L)
044100020927     c                             and FGWatb =  *blanks
044200020927     c                             and FGWfgs <> *zeros
044300020927     c                             and FGWfgs <> FGVlnp
044400020927     c                   eval      *in30 = *on
044500020927    6c                   endif
044600020927    5c                   endif
044700020927    4c                   endif
044800941017     C   30              SETON                                        4890
044900941017     C   30              GOTO      ENDCTR
045000941027     C*
045100941027   X3C                   ELSE                                                   NON C'E' IN £1
045200941027     C*
045300020927     c                   if        §OGntw = 'PPT'
045400020927      * - se p.o. Poste controllo su FNFGW02L
045500020927     c     KFVA1         chain     FNFWA02L                           30
045600020927     c  N30FWAatb        comp      *blanks                            3030      NO ANNULLATO
045700020927     c                   else
045800020927      * - altrimenti controllo su FNFGV01L
045900941027     C     KFVA1         CHAIN     FNFVA01L                           30
046000941027     C  N30FVAATB        COMP      *BLANKS                            3030      NO ANNULLATO
046100020927    5c                   if        not *in30
046200020927      * - se p.o. non poste controllo che il p.o. gestione non sia poste
046300020927     c     KFVA1         chain     FNFWA01L                           31
046400020927    6c                   if        %found(FNFWA01L)
046500020927     c                             and FWAatb =  *blanks
046600020927     c                             and FWAfgs <> *zeros
046700020927     c                             and FWAfgs <> FVAlnp
046800020927     c                   eval      *in30 = *on
046900020927    6c                   endif
047000020927    5c                   endif
047100020927     c                   endif
047200941027     C   30              SETON                                        4890
047300941027     C   30              GOTO      ENDCTR
047400941027     C*
047500941027    3C                   ENDIF
047600941017     C*
047700941027   X2C                   ELSE                                                   ? > '1' = ARRIVI ?
047800941017     C*
047900941013     C     KFVV          CHAIN     FNFVV01L                           30
048000941027    3C  N30FVVATB        IFNE      ' '
048100920807     C                   SETON                                        30
048200941027    3C                   END
048300920807     C   30              SETON                                        4890
048400920807     C   30              GOTO      ENDCTR
048500941027     C*
048600941027    2C                   ENDIF
048700941027     C*
048800941027     C* VEDO SE ESISTONO SPUNTE SUL FOGLIO
048900941027     C                   SETOFF                                       30
049000000121     C                   Z-ADD     COMFVD        WFVD
049100020927     c                   if        §OGntw = 'PPT'
049200020927     c                             and VIDnpg = '1'
049300020927     c                   z-add     D55tfp        COMfil
049400020927     c                   else
049500020927     c                   z-add     VIDfil        COMfil
049600020927     c                   endif
049700070115     C     KBRV          SETLL     FNBRV01L                               30     ON=TROVATO
049800941027     C  N30              SETON                                        49  90
049900941028     C  N30              GOTO      ENDCTR
050000941027     C*
050100941027    1C                   END
050200920807     C*
050300920707     C* FOGLIO AL SE IMMESSO NO MINORE  DEL DAL
050400941027    1C     VIDFVA        IFGT      0
050500941027    2C     VIDFVA        IFLT      COMFVD
050600910801     C                   SETON                                        4390
050700910801     C                   GOTO      ENDCTR
050800941027    2C                   END
050900941027     C*
051000920707     C* FOGLIO AL NON PUO' ESSERE > DI 500 DAL DAL
051100941027     C     VIDFVA        SUB       COMFVD        RESTO             5 0
051200941027    2C     RESTO         IFGT      500
051300910801     C                   SETON                                        4490
051400910801     C                   GOTO      ENDCTR
051500941027    2C                   END
051600941027    1C                   END
051700941020     C*
051800941020     C* PISTOLA INIZIALE <= PISTOLA FINALE
051900941020     C     VIDPID        IFGT      VIDPIA
052000941020     C                   SETON                                        4290
052100941020     C                   GOTO      ENDCTR
052200941020     C                   ENDIF
052300941020     C*
052400941020     C* SERIE INIZIALE <= SERIE FINALE
052500941027     C     VIDSRD        IFGT      VIDSRA
052600941020     C                   SETON                                        4690
052700941020     C                   GOTO      ENDCTR
052800941020     C                   ENDIF
052900910320     C*
053000941027     C* CONTROLLO VALIDITA' FILIALE IMMESSA
053100941027     C                   MOVEL     *BLANKS       DESLNA
053200941027     C                   MOVEL     '0=TUTTE'     DESLNA
053300941027     C     VIDLNA        IFGT      0
053400941027     C     VIDLNA        CHAIN     AZORG                              31
053500941027     C   31              SETON                                        4590
053600941027     C   31              GOTO      ENDCTR
053700941027     C                   MOVEL     ORGDES        DESLNA                         DECODIFICA
053800941027     C                   END
053900941027     C*
054000910320     C     ENDCTR        ENDSR
054100941012     C**************************************************************************
054200941012     C*    CARICO CATEGORIE
054300941012     C**************************************************************************
054400920807     C     CARCAT        BEGSR
054500941012     C*
054600920807     C                   MOVEL     '7N'          COD
054700920807     C                   Z-ADD     0             I                 2 0
054800941013     C     KTAB          SETLL     TABEL00F
054900941013     C     KTAB          READE     TABEL00F                               31
055000920807     C     *IN31         DOWEQ     '0'
055100920807     C                   MOVEL     TBLUNI        DS7N
055200920807     C* SOLO SE ABILITATA PER FOGLI VARI
055300920807     C                   ADD       1             I
055400920807     C                   MOVEL     ' ='          DEC(I)
055500920807     C                   MOVEL     ALF1          DEC(I)
055600920807     C                   MOVE      §7NDE2        DEC(I)
055700920807     C     I             IFEQ      6
055800920807     C                   SETON                                        31
055900920807     C                   END
056000920807     C*
056100941013     C  N31KTAB          READE     TABEL00F                               31
056200920807     C                   END
056300920807     C*
056400920807     C     DEC(1)        IFNE      *BLANKS
056500920807     C                   MOVEL     DEC(1)        DESCA1
056600920807     C     DEC(2)        IFNE      *BLANKS
056700920807     C                   MOVEL     DEC(2)        DESCA2
056800920807     C     DEC(3)        IFNE      *BLANKS
056900920807     C                   MOVEL     DEC(3)        DESCA3
057000920807     C     DEC(4)        IFNE      *BLANKS
057100920807     C                   MOVEL     DEC(4)        DESCA4
057200920807     C     DEC(5)        IFNE      *BLANKS
057300920807     C                   MOVEL     DEC(5)        DESCA5
057400920807     C     DEC(6)        IFNE      *BLANKS
057500920807     C                   MOVEL     DEC(6)        DESCA6
057600920807     C                   END
057700920807     C                   END
057800920807     C                   END
057900920807     C                   END
058000920807     C                   END
058100920807     C                   END
058200920807     C*
058300020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
058400920807     C*
058500020424     C***                  MOVELSIMFEL    VIDFIL
058600920807     C*
058700020424     C***        REM       IFEQ 'REM'
058800020424     C***        REMFIL    ANDGT*ZEROS
058900020424     C***                  MOVELREMFIL    VIDFIL
059000020424     C***                  END
059100020424     C                   Z-ADD     SIMPOU        VIDFIL
059200941013     C*
059300941013     C* CARICO TABELLA FILIALI GESTITE £1
059400020808     C                   CLEAR                   TRUL06DS
059500941013     C                   MOVE      '£1'          D06COD
059600941013     C                   MOVEL     SIMFEL        D06KEY
059700020808     C                   MOVEL     TRUL06DS      KPJBU
059800941013     C*
059900941013     C                   CALL      'TRUL06R'
060000941013     C                   PARM                    KPJBA
060100020808     C                   MOVEL     KPJBU         TRUL06DS
060200941013     C                   MOVEA     LIN           L1
060300941012     C*
060400920807     C                   ENDSR
060500941012     C**************************************************************************
060600941012     C*    R O U T I N E     I N I Z I A L E
060700941012     C**************************************************************************
060800941012     C     *INZSR        BEGSR
060900941012     C*
061000000121     C* N.B.: QUANDO VERRA' AUMENTATA LA LUNGHEZZA NUMERO FOGLIO ANCHE
061100000121     C* SUL FILE FNFVV/FNFVA/FNFGV LA KLIST KBRV SI POTRA' CANCELLARE
061200070115     C* ED UTLIZZARE KFVV ANCHE PER LE LETTURE DI FNBRV.
061300000121     C* (QUINDI ANCHE IL CAMPO WFVD SI POTRÀ CANCELLARE)
061400941013     C     KFVV          KLIST                                                  FNFVV01L/FNBRV01L
061500941013     C                   KFLD                    COMNPG                         ..CATEGORIA
061600941013     C                   KFLD                    COMFVD                         ..NR.FOGLIO
061700020808     C                   KFLD                    VIDFIL                         ..FIL GESTIONE
061800000121     C     KBRV          KLIST                                                  FIBRV01L
061900000121     C                   KFLD                    COMNPG                         ..CATEGORIA
062000000121     C                   KFLD                    WFVD                           ..NR.FOGLIO
062100020927     C                   KFLD                    COMfil                         ..FIL GESTIONE
062200941027     C     KFVA1         KLIST                                                  FNFVA01L
062300941027     C                   KFLD                    VIDFIL                         ..LINEA PARTENZA
062400941027     C                   KFLD                    COMFVD                         ..NR.FOGLIO
062500020808     C     K02FGV01      klist                                                  FNFGV01L
062600020808     C                   kfld                    COMfvd                         ..NR.FOGLIO
062700020808     C                   kfld                    VIDfil                         ..P.O.GESTIONE
062800941013     C     KTAB          KLIST                                                  TABEL00F
062900941012     C                   KFLD                    CODUT
063000941013     C                   KFLD                    COD
063100941013     C     KTAB2         KLIST                                                  TABEL00F
063200941012     C                   KFLD                    CODUT
063300941013     C                   KFLD                    COD
063400941013     C                   KFLD                    KEY
063500020808     ***C*
063600020808     ***C     *LIKE         DEFINE    FVVNPG        COMNPG                         CATEGORIA FOGLIO
063700020808     ***C     *LIKE         DEFINE    TBLCOD        COD                            CODICE TABELLA
063800020808     ***C     *LIKE         DEFINE    TBLKEY        KEY                            ELEMENTO
063900020808     ***C     *LIKE         DEFINE    FVVNFV        COMFVD                         ELEMENTO
064000020808     ***C     *LIKE         DEFINE    BRVNFV        WFVD
064100941014     C*
064200941012     C                   ENDSR
