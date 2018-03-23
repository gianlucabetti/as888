000100930609     H DECEDIT('0,') DATEDIT(*DMY.)
000200950131     H* FNLRA3R *-----------------------------------------------------*
000300930609     H*                 - QUADRATURA DEFLUENZA -                      *
000400930609     H*---------------------------------------------------------------*
000500930609     H* INDICATORI USATI                                              *
000600930609     H*---------------------------------------------------------------*
000700960712     H*  01    - SUPERATI GLI 82 ELEMENTI MASSIMI
000800930609     H*  05    - ON SONO REM (II LIVELLO) - OFF (I LIVELLO)
000900930609     H*  09    - RICERCA FOGLIO VIAGGIO
001000930609     H*  10    - ROLL UP
001100930609     H*  30/32 - DI COMODO
001200930609     H*  36/37 - DEL SUBFILE
001300930609     H*  40/46 - ERRORE
001400930609     H*  57/58 - ERRORE
001500930609     H*  90    - ERRORE GENERICO
001600930609     F*---------------------------------------------------------------*
001700930609     F* DATA BASE                                                     *
001800930609     F*---------------------------------------------------------------*
001900950131     FFNLRA3D   CF   E             WORKSTN
002000950131     F                                     SFILE(LRA3DSF:NRR)
002100950131     FFNFVV01L  IF   E           K DISK
002200950131     FFNFGV01L  IF   E           K DISK
002300920907     FTABEL00F  IF   E           K DISK
002400920911     FAZORG01L  IF   E           K DISK
002500930609     D*---------------------------------------------------------------*
002600930609     D* SCHIERE                                                       *
002700930609     D*---------------------------------------------------------------*
002800911008     D SKI             S              1    DIM(5)                               ? SU F.V.
002900920907     D*
003000920907     D L1              S              3  0 DIM(30)                              FIL GESTITE
003100920907     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
003200920907     D L6S             S              3  0 DIM(30)                              FIL GEST SIMFE
003300930609     D*---------------------------------------------------------------*
003400930609     D* DS                                                            *
003500930609     D*---------------------------------------------------------------*
003600920907     D PARAM           DS
003700960712     D  NFV                    1    246P 0
003800960712     D                                     DIM(82)                              FVA SELEZ
003900960712     D  PARSTA               249    249
004000960712     D  PARFGS               250    251P 0
004100950201     D  PARNFV               252    254P 0
004200920916     D  VIDSN                255    255
004300930609     D  PARNP2               256    256  0
004400951108     D*
004500130506     D* PASSAGGIO A INTERROGAZIONE FV PARTENZE      -FNLST3R-
004600130506     D fnlst3ds        DS
004700951108     D  PA5FLG                 1      1    INZ('R')
004800951108     D  PA5FL2                 2      2    INZ('F')
004900951108     D  PA5NFV                 3      5P 0 INZ
005000951108     D  PA5DIN                 6     10P 0
005100951108     D  PA5DFI                11     15P 0
005200951108     D  PA5TFV                16     16    INZ
005300951108     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
005400951108     D*   SELEZIONATI
005500951108     D  FVA                   17    241P 0 INZ
005600951108     D                                     DIM(45)                              FVA SELEZ '?'
005700130506     D  PA5FGS               242    244  0
005800960228     D*
005900930609     D DS7N          E DS
006000020618     D DSTV          E DS
006100070611     D og143         E DS
006200920901     D KPJBA         E DS
006300101202      *
006400101202      * - Parametri per richiamo al pgm di controllo profilo utenti
006500101202     d TIBS34ds      e ds
006600101202      * - Ds di riferimento al file esterno AzUte00F
006700101202     d AZUTEds       e ds                  ExtName(AzUte00F)
006800101202      * - Ds per dati organigramma
006900101202     d dDatiUte      e ds
007000101202      *
007100950131     D DSLR01        E DS
007200950131     D WLBDAT          DS
007300950131     D  G02DAT                 1      8  0
007400950131     D  G02INV                 9     16  0
007500950131     D  G02ERR                17     17
007600950131     D  G02TGI                18     22  0
007700950131     D* DS PER TRUL06R - CARICAMENTO £X
007800950131     D DSUL06        E DS                  EXTNAME(TRUL06DS)
007900950131     D  LIN                    1     90  0
008000950131     D                                     DIM(30)                              FIL. COMODO
008100911010     C*---------------------------------------------------------------*
008200930609     C*                    MAIN                                       *
008300930609     C* PROGRAMMA PRINCIPALE QUADRATURA DEFLUENZA                     *
008400930609     C*---------------------------------------------------------------*
008500930609     C*
008600930609     C* DEFINIZIONE VARIABILI, KLIST E PLIST
008700930609     C                   EXSR      DEFVAR
008800900517     C*
008900930609     C* OPERAZIONI INIZIALI E CARICAMENTO TABELLE
009000920907     C                   EXSR      OPERAZ
009100920907     C*
009200911008     C                   EXSR      PULIZ
009300920907     C                   Z-ADD     1             REC
009400911010     C     RICNFV        TAG
009500950131     C                   WRITE     LRA3D01
009600911008     C*
009700920907     C     FORCT         TAG
009800950131     C                   EXFMT     LRA3DCT
009900930609     C*
010000930609     C* CMD3 - FINE LAVORO
010100911008     C   KC              GOTO      FINE
010200930609     C*
010300930609     C* CM18- CAMBIO FILIALE
010400920907     C   KS              SETON                                        04
010500950201     C   KS              GOTO      RICNFV
010600920907     C*
010700920907     C   04              DO
010800920907     C     VIDFIL        LOOKUP    L1                                     30
010900920907     C  N30              SETON                                        50
011000920907     C  N30              GOTO      FORCT
011100920907     C*
011200920907     C* NON POSSO CAMBIARE SE E' IN MIA L6
011300920907     C     VIDFIL        IFNE      SIMFEL
011400920907     C     VIDFIL        LOOKUP    L6S                                    49
011500920911     C   49              GOTO      FORCT
011600920907     C                   END
011700920907     C*
011800920907     C                   EXSR      CARL6
011900920907     C                   END
012000920907     C*
012100930609     C* 10 ON  - ROLL UP SOLO SE L'ULTIMO E' PIENO
012200920907     C   10              DO
012300920907     C   01              SETON                                        43
012400920907     C*
012500920907     C  N01              Z-ADD     SAVNRR        NRR
012600950131     C  N01NRR           CHAIN     LRA3DSF                            30
012700930406     C  N01
012800930406     CANN30VIDNFV        IFLE      *ZEROS
012900920907     C                   SETON                                        43
013000920907     C                   END
013100920907     C*
013200930406     C   43              Z-ADD     SAVNRR        REC
013300920907     C   43              GOTO      FORCT
013400920907     C*
013500930331     C                   Z-ADD     0             B
013600920907     C                   EXSR      PULSFL
013700920907     C                   GOTO      FORCT
013800920907     C                   END
013900911008     C*
014000911008     C* CONTROLLO FOGLIO VIAGGIO ARRIVI
014100911008     C                   EXSR      CONTR1
014200920907     C   09              GOTO      RICNFV
014300950131     C  NKF
014400920911     COR 90              GOTO      FORCT
014500911008     C*
014600950131     C* CMD6 - ELABORAZIONE
014700950201     C                   Z-ADD     VIDFIL        PARFGS                         FILIALE GESTIONE
014800950201     C                   MOVEL     VIDNFA        PARNFV                         NR.FOGLIO
014900960712     C                   MOVEL     VIDSTA        PARSTA                         NR.FOGLIO
015000920907     C                   MOVEL     PARAM         KPJBU
015100950131     C                   MOVEL     'LRA4'        KCOAZ
015200920907     C                   CALL      'BCH10'
015300920907     C                   PARM                    KPJBA
015400950131     C*
015500000000     C     FINE          TAG
015600000000     C                   SETON                                        LR
015700930609     C*---------------------------------------------------------------*
015800930609     C*                    OPERAZ                                     *
015900930609     C* ROUTINE DI OPERAZIONI INIZIALI E CARICAMENTO TABELLE          *
016000930609     C*---------------------------------------------------------------*
016100920907     C     OPERAZ        BEGSR
016200950131     C*
016300950131     C                   TIME                    WTIME            14 0          ORA & DATA
016400950131     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
016500101202      *
016600101202      * impostazione campi fissi
016700101202     C                   Z-ADD     1             CODUT             1 0
016800971009     C***
016900971009     C* CARICO TUTTI I P.O. GESTITI DA ALTRI (DA £6)
017000971009     C***
017100971009     C                   CLEAR                   L6S
017200971009     C                   CLEAR                   DSUL06
017300971009     C                   MOVE      '£6'          D06COD
017400020509     c                   eval      d06esc = 'G'
017500020509     c                   movel     simfel        d06key
017600971009     C                   MOVEL     DSUL06        KPJBU
017700971009     C                   CALL      'TRUL06R'
017800971009     C                   PARM                    KPJBA
017900971009     C                   MOVEL     KPJBU         DSUL06
018000971009     C                   MOVEA     LIN           L6S
018100920907     C*
018200920907     C* VEDO SE SONO SIMFEL O REMOTO
018300020424     C*
018400101202     C                   Z-ADD     dutPOU        VIDFIL
018500101202     C     dutLPO        IFEQ      '2'
018600101202     C     dutLPO        OREQ      *BLANKS
018700020424     C                   SETON                                        05
018800020424     C                   ENDIF
018900971009     C*
019000971009     C                   EXSR      CARL6
019100920907     C*
019200920907     C* CARICO TABELLA FILIALI GESTITE £1
019300950131     C                   CLEAR                   DSUL06
019400950131     C                   MOVE      '£1'          D06COD
019500950131     C                   MOVEL     SIMFEL        D06KEY
019600950131     C                   MOVEL     DSUL06        KPJBU
019700950131     C*
019800950131     C                   CALL      'TRUL06R'
019900950131     C                   PARM                    KPJBA
020000950131     C                   MOVEL     KPJBU         DSUL06
020100950131     C                   MOVEA     LIN           L1
020200930609     C*
020300920907     C                   ENDSR
020400930609     C*
020500930609     C*---------------------------------------------------------------*
020600930609     C*                    PULIZ                                      *
020700930609     C* ROUTINE DI PULIZIA CAMPI                                      *
020800930609     C*---------------------------------------------------------------*
020900911008     C     PULIZ         BEGSR
021000930609     C*
021100920709     C* PRIMA VIDEATA
021200920907     C                   SETOFF                                       01
021300920907     C                   MOVEL     *BLANKS       VIDNFA
021400930609     C*
021500040227     C                   MOVEL     2             VIDNPG
021600930609     C*
021700911009     C* CAMPI DI PASSAGGIO
021800920916     C                   MOVEL     'N'           VIDSN
021900960712     C                   MOVEL     'N'           VIDSTA
022000920907     C*
022100911009     C* SUBFILE
022200920907     C                   SETON                                        37
022300950131     C                   WRITE     LRA3DCT
022400920907     C                   SETOFF                                       37
022500920907     C*
022600930331     C                   Z-ADD     0             B
022700930331     C                   Z-ADD     0             NRR
022800920911     C                   EXSR      PULSFL
022900911008     C*
023000911008     C                   ENDSR
023100930609     C*---------------------------------------------------------------*
023200930609     C*                    PULSFL                                     *
023300930609     C* ROUTINE DI PULIZIA CAMPI DEL SUBFILE                          *
023400930609     C*---------------------------------------------------------------*
023500911008     C     PULSFL        BEGSR
023600930609     C*
023700930331     C     B             DOWLT     11
023800960712     C     NRR           ANDLT     82
023900920911     C                   MOVEL     *BLANKS       VIDNFV
024000920907     C                   MOVEL     *ZEROS        VIDDFV
024100920911     C                   Z-ADD     0             VIDLNA
024200920907     C                   MOVEL     *BLANKS       DESLNA
024300920907     C                   MOVEL     *BLANKS       VIDTRM
024400920907     C                   MOVEL     *BLANKS       VIDTRR
024500920911     C                   ADD       1             NRR               5 0
024600920316     C                   ADD       1             B                 3 0
024700930609     C*
024800950131     C                   WRITE     LRA3DSF
024900911008     C                   END
025000930609     C*
025100911008     C* SALVO L'ULTIMO
025200920316     C                   Z-ADD     NRR           SAVNRR            5 0
025300930401     C                   Z-ADD     NRR           REC
025400930609     C*
025500960712     C* NON POSSO PASSARE PIU' DI 82 ELEMENTI SE PASSO ERRORE
025600960712     C     NRR           IFEQ      82
025700930406     C                   SETON                                        01
025800930406     C                   END
025900930609     C*
026000911008     C                   ENDSR
026100930609     C*---------------------------------------------------------------*
026200930609     C*                    CONTR1                                     *
026300930609     C* ROUTINE DI CONTROLLO DATI FORMATO VIDEO                       *
026400930609     C*---------------------------------------------------------------*
026500911008     C     CONTR1        BEGSR
026600930609     C*
026700101202     C                   SETOFF                                       30  90
026800950201     C                   SETOFF                                       09
026900930609     C*
027000930609     C                   Z-ADD     1             X                 2 0
027100930609     C                   MOVEA     *ZEROS        NFV
027200930609     C*
027300930609     C* CATEGORIA FOGLI
027400930609IF  1C     VIDNPG        IFEQ      ' '
027500930609     C                   SETON                                        5790
027600930609     C                   GOTO      ENDCT2
027700930609E   1C                   ENDIF
027800930609     C*
027900930609     C* RICERCA CATEGORIE FOGLI
028000930609IF  1C     VIDNPG        IFEQ      '?'
028100930609     C                   MOVEL     CODUT         §KUT
028200930609     C                   MOVEL     '7N'          §COD
028300930609     C                   MOVE      ' '           VIDNPG
028400930609     C                   MOVEL     *BLANKS       §KEY
028500930609     C                   CALL      'X§TABER'
028600930609     C                   PARM                    §KUT              1 0
028700930609     C                   PARM                    §COD              2
028800930609     C                   PARM                    §KEY              8
028900930609     C                   PARM                    §DES             30
029000930609     C                   MOVEL     §KEY          VIDNPG
029100930609     C                   SETON                                        90
029200930609     C                   GOTO      ENDCT2
029300930609E   1C                   ENDIF
029400930609     C*
029500930609     C* CONTROLLO ESISTENZA
029600930609     C                   MOVEL     '7N'          COD
029700930609     C                   MOVEL     *BLANKS       KEY
029800930609     C                   MOVEL     VIDNPG        KEY
029900930609     C*
030000930609     C* INESISTENTE
030100930609     C     KTAB2         CHAIN     TABEL                              57
030200930609     C*
030300930609     C* ANNULLATA
030400930609IF  1C  N57TBLFLG        IFNE      ' '
030500930609     C                   SETON                                        57
030600930609X   1C                   ELSE
030700930609     C*
030800101202     C* SOLO CATEGORIA '2'
030900930609IF  2C     VIDNPG        IFNE      '2'
031000930609     C                   SETON                                        57
031100930609E   2C                   ENDIF
031200930609E   1C                   ENDIF
031300930609     C* ERRATA
031400930609     C   57              SETON                                        90
031500930609     C   57              GOTO      ENDCT2
031600930609     C*
031700930609     C                   MOVEL     TBLUNI        DS7N
031800930609     C*
031900930609     C* NON UTILIZZABILE DALLA FILIALE IN GESTIONE
032000930609IF  1C     §7NUTI        IFEQ      '9'
032100930609     C                   SETON                                        5890
032200930609     C                   GOTO      ENDCT2
032300930609X   1C                   ELSE
032400930609     C*
032500930609     C* UTILIZZABILE SOLO DALLA PRIMO LIVELLO
032600930609IF  2C     §7NUTI        IFEQ      '1'
032700930609     C     VIDFIL        ANDNE     SIMFEL
032800930609     C                   SETON                                        5890
032900930609     C                   GOTO      ENDCT2
033000930609E   2C                   ENDIF
033100930609     C*
033200930609     C* UTILIZZABILE SOLO DALLA SECONDO LIVELLO
033300930609IF  2C     §7NUTI        IFEQ      '2'
033400930609     C     VIDFIL        ANDEQ     SIMFEL
033500930609     C                   SETON                                        5890
033600930609     C                   GOTO      ENDCT2
033700930609E   2C                   ENDIF
033800930609E   1C                   ENDIF
033900930609     C*
034000950131     C                   MOVEL     VIDNPG        COMNPG
034100930609     C                   MOVEL     VIDNPG        PARNP2
034200920907     C*
034300920907     C* NUMERO FOGLIO VIAGGIO
034400920907     C     VIDNFA        IFEQ      *BLANKS
034500920915     C     VIDNFA        OREQ      *ZEROS
034600920907     C                   SETON                                        4090
034700920911     C                   GOTO      ENDCT2
034800920907     C*
034900920907     C                   ELSE
035000920907     C                   MOVEA     VIDNFA        SKI
035100920907     C* RICERCA
035200920907     C     '?'           LOOKUP    SKI                                    09
035300920907     C   09              DO
035400950131     C                   CLEAR                   DSLR01
035500950131     C                   MOVEL     'S'           DLRRIC                         PGM.RICHIAMATO
035600950131     C                   MOVEL     VIDNPG        DLRNPG                         CATEGORIA
035700950131     C                   MOVEL     '2'           DLRTFV
035800950131     C                   Z-ADD     VIDFIL        DLRFGS                         FIL.GESTIONE
035900950131     C                   MOVEL     'FNLRA3R'     DLRPGM                         PROGRAMMA
036000950131     C                   MOVEL     DSLR01        KPJBU
036100950131     C                   CALL      'FNLR02R'
036200920907     C                   PARM                    KPJBA
036300950131     C                   MOVEL     KPJBU         DSLR01
036400950131     C                   MOVEL     DLRNFV        VIDNFA
036500920911     C                   GOTO      ENDCT2
036600920907     C                   END
036700920907     C* CONTROLLI
036800920907     C                   TESTN                   VIDNFA               31
036900920907     C  N31              SETON                                        30
037000920907     C*
037100920907     C  N30              MOVEL     VIDNFA        COMNFV
037200950131     C  N30KFVV          CHAIN     FNFVV000                           30
037300930609     C*
037400920907     C* ANNULLATO
037500920907     C  N30FVVATB        IFNE      *BLANKS
037600920907     C                   SETON                                        30
037700920907     C                   END
037800920907     C*
037900920907     C   30              SETON                                        4090
038000920911     C   30              GOTO      ENDCT2
038100920907     C*
038200920907     C* SE E' IN GESTIONE ALLA FILIALE
038300950131     C     FVVFGS        LOOKUP    L6                                     30
038400920907     C  N30              SETON                                        4190
038500920911     C  N30              GOTO      ENDCT2
038600920907     C*
038700920907     C                   END
038800920907     C*
038900920907     C* CONTROLLO TUTTO IL SUBFILE
039000920907     C                   Z-ADD     1             NRR
039100920907     C     NRR           DOWLE     SAVNRR
039200950131     C     NRR           CHAIN     LRA3DSF                            30
039300920907     C*
039400920907     C  N30VIDNFV        IFNE      *ZEROS
039500920907     C     VIDNFV        ANDNE     *BLANKS
039600920907     C*
039700920907     C                   MOVEA     *BLANKS       SKI
039800920907     C                   MOVEA     VIDNFV        SKI
039900920907     C*
040000920907     C* RICERCA IN TUTTI I FOGLI VIAGGIO
040100920907     C     '?'           LOOKUP    SKI                                    09
040200920907     C   09              DO
040300130506     C                   RESET                   fnlst3ds
040400950201     C* INTERROGO SOLO I FOGLI VIAGGIO IN DATA DI OGGI
040500951108     C                   Z-ADD     WDATE         PA5DIN
040600951108     C                   Z-ADD     WDATE         PA5DFI
040700020814     C                   z-add     SIMfel        PA5fgs
040800130506     C                   MOVEL     fnlst3ds      KPJBU
040900130506     C                   CALL      'FNLST3R'
041000920907     C                   PARM                    KPJBA
041100920907     C*
041200130506     C                   MOVEL     KPJBU         fnlst3ds
041300920907     C*
041400920907     C* SE SELEZIONATI FV LI SCRIVO
041500920907     C                   MOVEL     *BLANKS       VIDNFV
041600950131     C                   UPDATE    LRA3DSF
041700920907     C*
041800920907     C                   SUB       1             NRR
041900920907     C                   Z-ADD     1             I                 4 0
042000920907     C*
042100920907     C     FVA(I)        DOWNE     *ZEROS
042200920907     C                   ADD       1             NRR
042300950131     C     NRR           CHAIN     LRA3DSF                            30
042400920907     C*
042500920907     C* CREO NUOVA PAGINA DEL SUBFILE
042600920907     C   30              Z-ADD     NRR           NUM5              5 0
042700920915     C   30              SUB       1             NRR
042800920907     C   30              Z-ADD     1             B
042900920907     C   30              EXSR      PULSFL
043000920907     C   30              Z-ADD     NUM5          NRR
043100950131     C   30NRR           CHAIN     LRA3DSF                            30
043200920907     C*
043300920907     C* MEMORIZZO SOLO SE E' VUOTO ALTRIMENTI SU ALTRA RIGA
043400920907     C     VIDNFV        IFLT      *ZEROS
043500920907     C                   MOVEL     FVA(I)        VIDNFV
043600920907     C                   MOVE      FVA(I)        VIDLNA
043700920907     C     VIDLNA        CHAIN     AZORG                              31
043800920907     C  N31              MOVEL     ORGDES        DESLNA
043900920907     C   31              MOVEL     *BLANKS       DESLNA
044000920907     C*
044100950131     C                   UPDATE    LRA3DSF
044200920907     C                   ADD       1             I
044300920907     C                   END
044400920907     C                   END
044500920907     C* ESCO
044600920915     C     NRR           IFLT      1
044700920915     C                   Z-ADD     1             NRR
044800920915     C                   END
044900920907     C                   Z-ADD     NRR           REC
045000920907     C                   SETON                                        90
045100920907     C                   GOTO      ENDCT1
045200920907     C                   END
045300930609     C*
045400920907     C                   TESTN                   VIDNFV               31
045500920907     C  N31              SETON                                        4690
045600920907     C  N31              GOTO      ENDCT1
045700920907     C*
045800920907     C                   Z-ADD     0             CO2NFV            5 0
045900920907     C                   MOVEL     VIDNFV        CO2NFV
046000950131     C     KFGV          CHAIN     FNFGV000                           31
046100930609     C*
046200920907     C* ANNULLATO
046300920907     C  N31FGVATB        IFNE      *BLANKS
046400920907     C                   SETON                                        31
046500920907     C                   END
046600920907     C* ERRORE
046700920907     C   31              SETON                                        4690
046800920911     C   31              GOTO      ENDCT1
046900960201     C* NON POSSO UTILIZZARE FV CON TIPO TRAINO INOLTRO
047000020618     C**   FGVTTR        IFEQ      'I'
047100020618     C**                 SETON                                        5690
047200020618     C**                 GOTO      ENDCT1
047300020618     C**                 ENDIF
047400020618     c**
047500020618     c* Genericamente posso utilizzare soltanto i fgv DEFLUENZA
047600020618     C                   MOVEL     'TV'          COD
047700020618     C                   MOVEL     *BLANKS       KEY
047800020618     C                   MOVEL     fgvttr        KEY
047900020618     C     KTAB2         CHAIN     TABEL                              57
048000020618     C*
048100020618     C* ANNULLATA
048200020618IF  1C  N57TBLFLG        IFNE      ' '
048300020618     C                   SETON                                        57
048400020618X   1C                   ELSE
048500020618     c                   movel     tbluni        dstv
048600020618     C     §TVDEf        IFNE      'S'
048700020618     C                   SETON                                        5690
048800020618     C                   GOTO      ENDCT1
048900020618     C                   ENDIF
049000020618     C                   ENDIF
049100920907     C*
049200920907     C* DECODIFICHE: LINEA ARRIVO
049300920907     C                   MOVEL     FGVLNA        VIDLNA
049400920907     C     VIDLNA        CHAIN     AZORG                              31
049500920907     C   31              MOVEL     *BLANKS       DESLNA
049600920907     C  N31              MOVEL     ORGDES        DESLNA
049700070611     c
049800070611     c* Se si tratta di defluenza per l'estero, impedisco elaborazione
049900070611     c*  definitiva: necessario fare manifest
050000070611     c                   if        vidsn='S'
050100070611     c                   movel     orgde3        og143
050200070611     c                   if        §ogntw = 'EEX' or §ogntw = 'EUP' or
050300070611     c                             §ogntw = 'FED' or §ogntw = 'DPD'
050400070611     C                   SETON                                        5990
050500070611     C                   GOTO      ENDCT1
050600070611     c                   endif
050700070611     c                   endif
050800930609     C*
050900930609     C* DATA FOGLIO
051000950131     C                   Z-ADD     FGVDFV        G02INV
051100950131     C                   Z-ADD     *ZEROS        G02DAT
051200950131     C                   MOVEL     3             G02ERR
051300950131     C                   CALL      'XSRDA8'
051400950131     C                   PARM                    WLBDAT
051500950131     C                   Z-ADD     G02DAT        VIDDFV
051600920907     C*          TARGHE
051700920907     C                   MOVEL     FGVTRM        VIDTRM
051800920907     C                   MOVEL     FGVTRR        VIDTRR
051900920907     C* MEMORIZZO
052000920911     C                   MOVEL     VIDNFV        NFV(X)
052100920907     C                   ADD       1             X
052200920907     C* ERRORE
052300920907     C     ENDCT1        TAG
052400920907     C   90              Z-ADD     NRR           REC
052500950131     C  N09              UPDATE    LRA3DSF
052600920907     C   90              Z-ADD     SAVNRR        NRR
052700920907     C                   ELSE
052800920907     C                   MOVEL     *BLANKS       VIDNFV
052900920915     C                   MOVEL     *ZEROS        VIDDFV
053000920907     C                   MOVEL     *ZEROS        VIDLNA
053100920915     C                   MOVEL     *BLANKS       DESLNA
053200920907     C                   MOVEL     *BLANKS       VIDTRR
053300920907     C                   MOVEL     *BLANKS       VIDTRM
053400950131     C                   UPDATE    LRA3DSF
053500920907     C                   END
053600920915     C*
053700920915     C                   ADD       1             NRR
053800920907     C                   END
053900920907     C*
054000920915     C  N90              DO
054100930609     C*
054200920915     C* DEVE ESSERE IMMESSO ALMENO UN FOGLIO VIAGGIO PARTENZA
054300920915     C                   XFOOT     NFV           TOTNFV           13 0
054400920915     C     TOTNFV        IFEQ      0
054500920916     C                   SETON                                        47
054600920916     C                   Z-ADD     1             NRR
054700950131     C     NRR           CHAIN     LRA3DSF                            31
054800950131     C  N31              UPDATE    LRA3DSF
054900920915     C                   END
055000920915     C                   Z-ADD     1             REC
055100920915     C                   END
055200920915     C*
055300920911     C     ENDCT2        ENDSR
055400930609     C*---------------------------------------------------------------*
055500930609     C*                    CARL6                                      *
055600930609     C* ROUTINE DI CARICAMENTO TABELLA £6                             *
055700930609     C*---------------------------------------------------------------*
055800920907     C     CARL6         BEGSR
055900930609     C*
056000950131     C                   CLEAR                   DSUL06
056100950131     C                   MOVE      '£6'          D06COD
056200971009     C                   MOVEL     VIDFIL        D06KEY
056300971009     C                   MOVEL     'L'           D06TLA
056400950131     C                   MOVEL     DSUL06        KPJBU
056500950131     C*
056600950131     C                   CALL      'TRUL06R'
056700950131     C                   PARM                    KPJBA
056800950131     C                   MOVEL     KPJBU         DSUL06
056900950131     C                   MOVEA     LIN           L6
057000930609     C*
057100920907     C                   ENDSR
057200930609     C*---------------------------------------------------------------*
057300930609     C*                    DEFVAR                                     *
057400930609     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
057500930609     C*---------------------------------------------------------------*
057600930609     C     DEFVAR        BEGSR
057700930609     C*
057800930609     C     *ENTRY        PLIST
057900930609     C                   PARM                    KPJBA
058000930609     C*
058100101202     c                   exsr      sr_DatiJob
058200930609     C*
058300930609     C     KFVV          KLIST
058400950131     C                   KFLD                    COMNPG
058500950131     C                   KFLD                    COMNFV
058600950131     C                   KFLD                    VIDFIL
058700950131     C     KFGV          KLIST
058800950131     C                   KFLD                    CO2NFV
058900950131     C                   KFLD                    SIMFEL
059000930609     C*
059100930609     C     KTAB2         KLIST
059200930609     C                   KFLD                    CODUT
059300950131     C                   KFLD                    COD
059400950131     C                   KFLD                    KEY
059500950131     C*
059600950131     C     *LIKE         DEFINE    TBLCOD        COD
059700950131     C     *LIKE         DEFINE    TBLKEY        KEY
059800950131     C     *LIKE         DEFINE    FVVNPG        COMNPG
059900950131     C     *LIKE         DEFINE    FVVNFV        COMNFV
060000930609     C*
060100930609     C                   ENDSR
060200101202
060300101202      *---------------------------------------------------------------*
060400101202      *?Reperimento dati del job (Utente/Operativi)                  ?*
060500101202      *---------------------------------------------------------------*
060600101202     c     sr_DatiJob    BEGSR
060700101202      *
060800101202     c     *dtaara       define    §azute        AZUTEds
060900101202     c     *dtaara       define    §datiute      dDatiUte
061000101202      *
061100101202     c                   in(E)     *dtaara
061200101202     c                   IF        %Error or RSUT = *blanks
061300101202     c                   clear                   TIBS34ds
061400101202     c                   call      'TIBS34R'
061500101202     c                   parm                    TIBS34ds
061600101202     c                   in        *dtaara
061700101202     c                   ENDIF
061800101202      *
061900101202     c                   ENDSR
