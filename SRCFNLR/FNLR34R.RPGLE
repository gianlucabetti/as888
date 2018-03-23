000100921014     H DECEDIT('0,') DATEDIT(*DMY.)
000200050422      * FNLR34R *----------------------------------------------------*
000300020809      *         - FILTRO LISTA ANOMALIE
000400020809      *--------------------------------------------------------------*
000500941227     FFNLR34D   CF   E             WORKSTN
000600921014     FTABEL00F  IF   E           K DISK
000700960326     FAZORG01L  IF   E           K DISK
000800941227     FFNFGV01L  IF   E           K DISK
000900020918     fFNFGW01L  if   e           k disk
001000020916     FFNFVV01L  IF   E           K DISK
001100020809      *
001200960327     D TES             S             26    DIM(3) CTDATA PERRCD(1)              TESTATA
001300980403     D MSG             S             78    DIM(15) CTDATA PERRCD(1)             msg di errore
001400960926     D L6S             S              3  0 DIM(30)                              P.O.GEST.DA AL
001500960926     D L1              S              3  0 DIM(30)                              P.O.GESTITE
001600980402     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
001700941227     D*
001800960326     D* PARAMETRI RICEVUTI DALLA KPJBU
001900960326     D PARAM           DS
002000960326     D  R34PAR                 1      1
002100960326     D  R34GES                 2      2
002200941227     D***
002300941227     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002400941227     D***
002500941227     D WLBDAT          DS                  INZ
002600941227     D  G02DAT                 1      8  0
002700941227     D  G02INV                 9     16  0
002800941227     D  G02ERR                17     17
002900941227     D  G02TGI                18     22  0
003000941227     D***
003100941227     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
003200941227     D***
003300941227     D WGIDAT          DS                  INZ
003400941227     D  GIODAT                 1      8  0
003500941227     D  GIOINV                 9     16  0
003600941227     D  GIOTGI                17     21  0
003700921014     D                 DS
003800921014     D  VIDAN0                 1      3
003900921014     D  VIDAN1                 4      6
004000921014     D  VIDAN2                 7      9
004100921014     D  VIDAN3                10     12
004200921014     D  VIDAN4                13     15
004300921014     D  VIDAN5                16     18
004400921014     D  VIDAN6                19     21
004500921014     D  VIDAN7                22     24
004600921014     D  VIDAN8                25     27
004700921014     D  VIDAN9                28     30
004800921014     D  ANM                    1     30
004900921014     D                                     DIM(10)                              ANOMALIE VIDEO
005000130520     D* CHIAMATA AL PGM DI INTERROGAZ.FV PARTENZA             -FNLST3R-
005100130520     D fnlst3ds        DS
005200951109     D  PA5FLG                 1      1    INZ('R')
005300951109     D  PA5FL2                 2      2    INZ
005400951109     D  PA5NFV                 3      5P 0 INZ
005500951109     D  PA5DIN                 6     10P 0
005600951109     D  PA5DFI                11     15P 0
005700951109     D  PA5TFV                16     16    INZ
005800000614     D  PA5FGS               242    244  0
005900951109     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
006000951109     D*   SELEZIONATI ---> SE NON UTILIZZATO NON LO DEFINISCO A PGM
006100020809     D**FVA                   17    241P 0
006200020809      *
006300020809     D KPJBA         E DS
006400020809     D UT§DSE0F      E DS
006500020809     D CNCR80        E DS
006600951109     D*
006700921015     D* PASSAGGIO A INTERROGAZIONE FV ARRIVI
006800941227     D* DS STANDARD FOGLI VARI
006900941227     D DSLR01        E DS                  INZ
007000960326     D*
007100960326     D DS7A2         E DS
007200960326     D DS7D          E DS
007300960402     D DS7C          E DS
007400941227     D* DS PER TRUL06R - CARICAMENTO £X
007500020809     D TRUL06DS      E DS
007600941227     D  LIN                    1     90  0
007700960926     D                                     DIM(30)                              P.O. COMODO
007800050422
007900050422     d Trul09ds      e ds
008000050422     d  TFA                   19    108  0 Dim(30)
008100960326     D*
008200960326     D* DS PER PASSAGGIO DATI A STAMPA-FNLR35R O GESTIONE FNLR44R
008300020809     D FNLR34DS      E DS
008400020809     D FNLV55DS      E DS
008500020221     D OG143         E DS
008600020809      *
008700020809     D wOGntw          s                   like(§OGntw) inz
008800020809      *****************************************************************
008900020809      * RIEPILOGO INDICATORI
009000020809      *****************************************************************
009100020809      * 01    - SERVE PER IL CONTROLLO DELLE CAUSALI ANOMALIA
009200020809      * 02    - RICHIESTA LA STAMPA DELLE ANOMALIE
009300020809      * 04    - GESTIONE/INTERROGAZIONE
009400020809      * 05    - SPROTEGGO LA P.O. IN GESTIONE
009500020809      * 06    - SONO P.O. DI SECONDO LIVELLO: NON ABILITO F18
009600020809      * 10/12 - POSIZIONAMENTO CURSORE IN VIDEATA
009700020809      * 42/49 - ERRORE
009800020809      * 51/60 - ERRORE
009900020809      * 62/64 - ERRORE
010000020809      * 71/80 - POSIZIONAMENTO CURSORE SUI CAMPI DEI CODICI ANOMALIA
010100020809      * 81    - POSIZIONAMENTO CURSORE SU CAMPO PAR/ARR
010200020809      * 90    - ERRORE GENERICO
010300020809      *****************************************************************
010400911016     C*
010500900509     C     FOR01         TAG
010600941227     C                   EXFMT     LR34D01
010700960327     C                   SETOFF                                       289081
010800960327     C                   SETOFF                                       656640
010900910320     C** CMD3 - FINE LAVORO
011000900509     C   KC              GOTO      FINE
011100960326     C*
011200960926     C* F18 - CAMBIO P.O. IN GESTIONE
011300960326     C   KS              SETON                                        05
011400960326     C   KS              GOTO      FOR01
011500960326     C*
011600960926     C* 05 ON - CONTROLLO P.O. IN GESTIONE
011700960326     C   05              EXSR      CTRFGS
011800960326     C   05
011900960326     CAN 90              GOTO      FOR01
012000921014     C* CONTROLLO
012100921014     C                   EXSR      CONTR
012200960517     C* RITRONO A FOR01 PER ERRORE O SE SONO IN STAMPA SENZA F6
012300960517     C     *IN90         IFEQ      *ON
012400960517     C     *IN04         OREQ      *OFF
012500960517     C     *INKF         ANDEQ     *OFF
012600960517     C                   GOTO      FOR01
012700960517     C                   ENDIF
012800941227     C* CMD6 - ELABORAZIONE
012900020809     C                   CLEAR                   FNLR34DS
013000960326     C     VIDNFV        IFGT      *ZEROS
013100960326     C                   MOVEL     VIDNFV        D34NFV
013200960326     C                   MOVEL     WDFV          D34DFV
013300921015     C                   END
013400960327     C                   MOVEL     R34GES        D34GES
013500960402     C*
013600020809     C                   DO        10            XX                2 0
013700020809     C     ANM(XX)       IFLE      *ZEROS
013800020809     C                   CLEAR                   ANM(XX)
013900960402     C                   ENDIF
014000960402     C                   ENDDO
014100960327     C*
014200960327     C                   MOVEL     VIDDAD        D34DLG
014300960326     C                   MOVEL     COMDAD        D34DLA
014400960326     C                   MOVEL     VIDDAA        D34ALG
014500960326     C                   MOVEL     COMDAA        D34ALA
014600960326     C*
014700960327     C                   MOVEL     VIDDCD        D34DCG
014800960327     C                   MOVEL     COMDCD        D34DCA
014900960415     C                   MOVEL     VIDDCA        D34ACG
015000960415     C                   MOVEL     COMDCA        D34ACA
015100960327     C*
015200960326     C                   MOVEL     VIDLID        D34LID
015300960326     C                   MOVEL     VIDFGS        D34FGS
015400960326     C                   MOVEL     VIDPAR        D34PAR
015500960326     C                   MOVEL     VIDSTA        D34APR
015600960326     C                   MOVEL     VIDFAS        D34FAS
015700960326     C                   MOVEL     VIDINC        D34INC
015800960424     C                   MOVEL     VIDCCH        D34CCH
015900960326     C                   MOVEA     ANM           D34ANM
016000960506     C                   MOVEL     VIDIE         D34IE
016100980402     C                   MOVEL     VICLNA        D34LNA
016200030717     C                   MOVEL     VIDPOS        D34POS
016300030717     C                   MOVEL     VIDNRS        D34NRS
016400040610     C                   MOVEL     VIDbva        D34bva
016500051221     C                   MOVEL     VIDmed        D34med
016600020809     C                   MOVEL     FNLR34DS      KPJBU
016700921016     C*
016800960613    1C     R34GES        IFEQ      'S'
016900960926     C* SE P.O. IN GESTIONE = 0 LA LANCIO PER TUTTE
017000960613    2C     VIDFGS        IFEQ      0
017100020809     C                   Z-ADD     1             XX
017200960613     C*
017300050422     c                   if        d34par='P' or d34par=' '
017400020809    3C     L1(XX)        DOWGT     0
017500020809     C     L1(XX)        LOOKUP    L6S                                    30
017600960613    4C     *IN30         IFEQ      *OFF
017700020809     C                   MOVEL     L1(XX)        D34FGS
017800020809     C                   MOVEL     FNLR34DS      KPJBU
017900960326     C                   MOVEL     'LR35'        KCOAZ                          STAMPA
018000031008     C                   CALL      'BCH10'
018100031008 MOM C***                CALL      'FNLR35R'
018200900702     C                   PARM                    KPJBA
018300960613    4C                   ENDIF
018400960613     C*
018500020809     C                   ADD       1             XX
018600960613    3C                   ENDDO
018700050422     c
018800050422     c                   else
018900050422     c
019000050422    3C     tfa(xx)       DOWGT     0
019100050422     C     tfa(xx)       LOOKUP    L6S                                    30
019200050422    4C     *IN30         IFEQ      *OFF
019300050422     C                   MOVEL     tfa(xx)       D34FGS
019400050422     C                   MOVEL     FNLR34DS      KPJBU
019500050422     C                   MOVEL     'LR35'        KCOAZ                          STAMPA
019600050422     C                   CALL      'BCH10'
019700050422 MOM C***                CALL      'FNLR35R'
019800050422     C                   PARM                    KPJBA
019900050422    4C                   ENDIF
020000050422     C*
020100050422     C                   ADD       1             XX
020200050422    3C                   ENDDO
020300050422     c
020400050422     c                   endif
020500960613     C*
020600960613   X2C                   ELSE
020700020809     C                   MOVEL     FNLR34DS      KPJBU
020800960613     C                   MOVEL     'LR35'        KCOAZ                          STAMPA
020900031008     C                   CALL      'BCH10'
021000031008 MOM C***                CALL      'FNLR35R'
021100960613     C                   PARM                    KPJBA
021200960613    2C                   ENDIF
021300960613     C*
021400960613   X1C                   ELSE
021500960326     C*
021600960327     C                   MOVEL     'LR35'        KCOAZ                          GESTIONE
021700960327     C                   CALL      'FNLR35R'                                    INTERROGAZ
021800960326     C                   PARM                    KPJBA
021900020809     C                   MOVEL     KPJBU         FNLR34DS
022000960326     C*
022100960326     C                   SETOFF                                       05
022200960613    2C     D34F03        IFEQ      ' '
022300960326     C                   GOTO      FOR01
022400960613    2C                   ENDIF
022500960613    1C                   ENDIF
022600921015     C*
022700000000     C     FINE          TAG
022800000000     C                   SETON                                        LR
022900921014     C*
023000921014     C* CONTROLLI ----------------------------------------------------*
023100921014     C     CONTR         BEGSR
023200960326     C                   SETOFF                                       0110
023300921016     C                   SETOFF                                       117172
023400921016     C                   SETOFF                                       737475
023500921016     C                   SETOFF                                       767778
023600960326     C                   SETOFF                                       7980
023700980402     C                   SETOFF                                       13
023800960326     C                   CLEAR                   WDFV
023900950608     C                   CLEAR                   COMDAD
024000950608     C                   CLEAR                   COMDAA
024100960327     C                   CLEAR                   COMDCD
024200960327     C                   CLEAR                   COMDCA
024300960424     C                   CLEAR                   DESCCH
024400960424     C                   CLEAR                   DESFAS
024500960326     C*
024600960326     C* CONTROLLI INCROCIATI ANOMALIE PARTENZA/ARRIVO
024700960326     C* CON IL TIPO DI ANOMALIE DA STAMPARE
024800960327    1C     VIDPAR        IFEQ      ' '
024900960326     C     VIDIE         ANDNE     ' '
025000960326     C     VIDIE         ANDNE     'I'
025100960326     C                   SETON                                        819028
025200960326     C                   MOVEL     MSG(1)        VIDMSG
025300960326     C                   GOTO      ENDCTR
025400960327    1C                   ENDIF
025500960326     C**
025600960327     C* DATA APERTURA DAL
025700960326     C**
025800960326    1C     VIDDAD        IFNE      0
025900921014     C                   MOVE      VIDDAD        G02DAT
026000921014     C                   MOVEL     *BLANK        G02ERR
026100941227     C                   CALL      'XSRDA8'
026200921014     C                   PARM                    WLBDAT
026300960326    2C     G02ERR        IFEQ      '1'
026400921014     C                   SETON                                        42  90
026500921014     C                   GOTO      ENDCTR
026600960326    2C                   END
026700960326     C                   Z-ADD     G02INV        COMDAD            8 0
026800950608     C                   Z-ADD     G02DAT        VIDDAD
026900960326    1C                   END
027000921014     C*
027100921015     C* DATA APERTURA AL
027200960326    1C     VIDDAA        IFNE      0
027300921014     C                   MOVE      VIDDAA        G02DAT
027400921014     C                   MOVEL     *BLANK        G02ERR
027500941227     C                   CALL      'XSRDA8'
027600921014     C                   PARM                    WLBDAT
027700960326    2C     G02ERR        IFEQ      '1'
027800921014     C                   SETON                                        43  90
027900921014     C                   GOTO      ENDCTR
028000960326    2C                   END
028100960326     C                   Z-ADD     G02INV        COMDAA            8 0
028200950608     C                   Z-ADD     G02DAT        VIDDAA
028300921014     C* DATA DAL NO MAGGIORE DATA AL
028400960326    2C     COMDAD        IFGT      COMDAA
028500921014     C                   SETON                                        4490
028600921014     C                   GOTO      ENDCTR
028700960326    2C                   END
028800960327    1C                   END
028900960327     C**
029000960327     C* DATA CHIUSURA DAL
029100960327     C**
029200960327    1C     VIDDCD        IFNE      0
029300960327     C                   MOVE      VIDDCD        G02DAT
029400960327     C                   MOVEL     *BLANK        G02ERR
029500960327     C                   CALL      'XSRDA8'
029600960327     C                   PARM                    WLBDAT
029700960327    2C     G02ERR        IFEQ      '1'
029800960327     C                   MOVEL     MSG(6)        VIDMSG
029900960327     C                   SETON                                        652890
030000960327     C                   GOTO      ENDCTR
030100960327    2C                   ENDIF
030200960327     C                   Z-ADD     G02INV        COMDCD            8 0
030300960327     C                   Z-ADD     G02DAT        VIDDCD
030400960327    1C                   END
030500960327     C*
030600960327     C* DATA CHIUSURA AL
030700960327    1C     VIDDCA        IFNE      0
030800960327     C                   MOVE      VIDDCA        G02DAT
030900960327     C                   MOVEL     *BLANK        G02ERR
031000960327     C                   CALL      'XSRDA8'
031100960327     C                   PARM                    WLBDAT
031200960327    2C     G02ERR        IFEQ      '1'
031300960327     C                   MOVEL     MSG(6)        VIDMSG
031400960327     C                   SETON                                        662890
031500960327     C                   GOTO      ENDCTR
031600960327    2C                   END
031700960327     C                   Z-ADD     G02INV        COMDCA            8 0
031800960327     C                   Z-ADD     G02DAT        VIDDCA
031900960327     C* DATA DAL NO MAGGIORE DATA AL
032000960327    2C     COMDCD        IFGT      COMDCA
032100960327     C                   MOVEL     MSG(7)        VIDMSG
032200960327     C                   SETON                                        659028
032300960327     C                   GOTO      ENDCTR
032400960327    2C                   ENDIF
032500921014     C*
032600960327    1C                   ENDIF
032700960424     C*
032800960415     C* SE RICHIESTE SOLO LE ANOMALIE APERTE NON IMMETTERE LA DATA
032900960327     C*  DI CHIUSURA
033000960327    1C     VIDSTA        IFEQ      'S'
033100960327    2C     VIDDCD        IFGT      0
033200960327     C     VIDDCA        ORGT      0
033300960424     C     VIDCCH        ORNE      '  '
033400960327     C                   MOVEL     MSG(8)        VIDMSG
033500960327     C                   SETON                                        659028
033600960327     C                   GOTO      ENDCTR
033700960327    2C                   ENDIF
033800960327     C*
033900960327    1C                   ENDIF
034000921015     C*
034100921015     C* FASE  APERTURA ANOMALIA
034200960326    1C     VIDFAS        IFNE      ' '
034300960326    2C     VIDFAS        IFEQ      '?'
034400020809     C                   MOVEL     CODUT         §KUT
034500020809     C                   MOVEL     '7D'          §COD
034600921015     C                   MOVE      *BLANKS       VIDFAS
034700921015     C                   CALL      'X§TABER'
034800020809     C                   PARM                    §KUT              1 0
034900020809     C                   PARM                    §COD              2
035000921015     C                   PARM                    §KEY              8
035100921015     C                   PARM                    §DES             30
035200921015     C                   MOVEL     §KEY          VIDFAS
035300921015     C                   MOVEL     §DES          DESFAS
035400921016     C                   SETON                                        9010
035500921015     C                   GOTO      ENDCTR
035600921015     C*
035700960326   X2C                   ELSE
035800921014     C*
035900921015     C                   MOVEL     '7D'          COD
036000960326     C                   MOVEL(P)  VIDFAS        KEY
036100921015     C     KTAB          CHAIN     TABEL                              30
036200960326    3C  N30TBLFLG        IFNE      ' '
036300921015     C                   SETON                                        30
036400960326    3C                   END
036500921015     C*
036600921015     C   30              SETON                                        4690
036700921015     C   30              GOTO      ENDCTR
036800921015     C*
036900921015     C                   MOVEL     TBLUNI        DS7D
037000921015     C                   MOVEL     §7DDES        DESFAS
037100960326    2C                   END
037200960326    1C                   END
037300960424     C**
037400960424     C* CAUSALE CHIUSURA ANOMALIE
037500960424    1C     VIDCCH        IFNE      '  '
037600960424     C     '?'           SCAN      VIDCCH                                 90
037700960424    2C     *IN90         IFEQ      *ON
037800020809     C                   MOVEL     CODUT         §KUT
037900020809     C                   MOVEL     '7E'          §COD
038000960424     C                   MOVE      *BLANKS       VIDFAS
038100960424     C                   CALL      'X§TABER'
038200020809     C                   PARM                    §KUT              1 0
038300020809     C                   PARM                    §COD              2
038400960424     C                   PARM                    §KEY              8
038500960424     C                   PARM                    §DES             30
038600960424     C                   MOVEL     §KEY          VIDCCH
038700960424     C                   MOVEL     §DES          DESCCH
038800960424     C                   SETON                                        9012
038900960424     C                   GOTO      ENDCTR
039000960424     C*
039100960424   X2C                   ELSE
039200960424     C*
039300960424     C                   MOVEL     '7E'          COD
039400960424     C                   MOVEL(P)  VIDCCH        KEY
039500960424     C     KTAB          CHAIN     TABEL                              30
039600960424     C* ERRORE
039700960424    3C     *IN30         IFEQ      *ON
039800960424     C     TBLFLG        ORNE      ' '
039900960424     C                   SETON                                        129028
040000960426     C                   MOVEL     MSG(13)       VIDMSG
040100960424     C                   GOTO      ENDCTR
040200960424    3C                   ENDIF
040300960424     C*
040400960424     C                   MOVEL     TBLUNI        DESCCH
040500960424    2C                   ENDIF
040600960424    1C                   ENDIF
040700921015     C* NUMERO FOGLIO
040800960326    1C     VIDNFV        IFNE      *BLANKS
040900921015     C     VIDNFV        ANDNE     *ZEROS
041000921015     C*
041100960326     C* SE NUMERO FOGLIO OBBLIGATORIA LA FASE
041200960326    2C     VIDFAS        IFEQ      ' '
041300020919     C                   SETON                                        90  48
041400921015     C                   GOTO      ENDCTR
041500960326    2C                   END
041600921015     C*
041700921015     C* SE FASE 8 I.D.D. NON AMMESSO FOGLIO
041800960326    2C     §7DNPG        IFLT      1
041900921015     C                   SETON                                            49    90
042000921015     C                   GOTO      ENDCTR
042100960326    2C                   END
042200921015     C* RICERCA
042300941227     C     '?'           SCAN      VIDNFV                                 90
042400921015     C*
042500960326    2C     *IN90         IFEQ      '1'
042600921016     C                   SETON                                        11
042700941227     C* FOGLI VARI
042800960326    3C     §7DNPG        IFGT      1
042900960326     C                   MOVEL     VIDFGS        DLRFGS
043000941227     C                   Z-ADD     0             DLRNFV
043100941227     C                   MOVEL     §7DNPG        DLRNPG
043200941227     C                   MOVEL     'S'           DLRRIC
043300020919     C                   MOVEL     DSLR01        KPJBU
043400941227     C*
043500941227     C                   CALL      'FNLR02R'
043600921015     C                   PARM                    KPJBA
043700941227     C                   MOVEL     KPJBU         DSLR01
043800020917     C                   move      *zeros        VIDNFV
043900020917     C                   move      DLRNFV        VIDNFV
044000960326    3C                   END
044100921015     C* FOGLI PARTENZE
044200960326    3C     §7DNPG        IFEQ      1
044300130520     c                   reset                   fnlst3ds
044400941227     C                   CLEAR                   PA5NFV
044500020809     C                   z-add     SIMfel        PA5fgs
044600130520     C                   MOVEL     fnlst3ds      KPJBU
044700130520     C                   CALL      'FNLST3R'
044800921015     C                   PARM                    KPJBA
044900921015     C*
045000130520     C                   MOVEL     KPJBU         fnlst3ds
045100020917     C                   move      *zeros        VIDNFV
045200020917     C                   move      PA5NFV        VIDNFV
045300960326    3C                   END
045400020809     c                   exsr      CTRFGS
045500941227     C*
045600921015     C                   GOTO      ENDCTR
045700921015     C*
045800960326   X2C                   ELSE
045900941227     C* CONTROLLO
046000921015     C                   TESTN                   VIDNFV               31
046100020917     ***C  N31              SETON                                        6310
046200020917     C  N31              SETON                                        6390
046300921015     C  N31              GOTO      ENDCTR
046400921015     C*
046500960326    3C   31VIDNFV        IFLT      *ZEROS
046600960326     C                   SETON                                        6390
046700921015     C                   GOTO      ENDCTR
046800921015     C*
046900960326   X3C                   ELSE
047000921015     C*
047100990512     C                   MOVE      VIDNFV        NUM5              5 0
047200960326    4C     §7DNPG        IFEQ      1
047300020809     ***C     NUM5          CHAIN     FNFGV000                           32
047400020809     C     K02FGV01      chain     FNFGV000                           32
047500960326    5C  N32FGVATB        IFNE      ' '
047600921015     C                   SETON                                        32
047700960326    5C                   END
047800921015     C*
047900921015     C   32              SETON                                        6290
048000921015     C   32              GOTO      ENDCTR
048100020918     ***C*
048200020918     ***C* SE NON E' DEL P.O. IN GESTIONE ERRORE
048300020918     **5C     FGVLNP        IFNE      VIDFGS
048400020918     ***C                   SETON                                        4590
048500020918     ***C                   GOTO      ENDCTR
048600020918     **5C                   ENDIF
048700020918      *
048800020918      * Foglio di P.O. non gestibile
048900020918     c     K02FGV01      chain     FNFGW000
049000020918if  4c                   if           (wOGntw  =  'PPT'
049100020918     c                             and (not %found(FNFGW01L)
049200020918     c                             or   FGWatb <> *blanks
049300020918     c                             or   FGWfgs <> VIDfgs))
049400020918     c                             or (wOGntw  <> 'PPT'
049500020918     c                             and (%found(FNFGW01L)
049600020918     c                             and  FGWatb =  *blanks
049700020918     c                             and  FGWfgs <> *zeros
049800020918     c                             and  FGWfgs <> VIDfgs))
049900020918     c                   seton                                        4590
050000020918     c                   goto      ENDCTR
050100020918e   4c                   endif
050200921015     C*
050300960326     C                   MOVE      FGVDFV        WDFV              8 0
050400921015     C*
050500960326   X4C                   ELSE
050600921015     C*
050700921015     C                   MOVEL     §7DNPG        COMNPG
050800941227     C     KFVV          CHAIN     FNFVV000                           32
050900941227     C*
051000020916    5C**!!!*IN32         DOWEQ     *OFF
051100020916    5c                   If         *In32 or fvvatb <> *Blanks
051200960926     C* IL FOGLIO DEVE APPARTENERE AL P.O. IN GESTIONE
051300020916    6C**!!!FVVFGS        IFNE      VIDFGS
051400020916     C**!!!FVVATB        ORNE      ' '
051500941227     C                   SETOFF                                       31
051600020916    6C**!!!              ENDIF
051700941227     C*
051800020916     C*!N31KFVV          READE     FNFVV000                               32
051900020916     C*!!31              SETON                                        32
052000020916    5C                   ENDIF
052100921015     C*
052200941227     C  N31              SETON                                        6390
052300941227     C  N31              GOTO      ENDCTR
052400921015     C*
052500960326    5C     FVVSPG        IFNE      §7DSPG
052501180119    5c     FVVSPG        andne     §7DSPG2
052600921015     C                   SETON                                        6490
052700921015     C                   GOTO      ENDCTR
052800960326    5C                   END
052900921015     C*
053000960326     C                   MOVE      FVVDFV        WDFV
053100960326    4C                   END
053200921015     C*
053300960326    3C                   END
053400960326    2C                   END
053500960326    1C                   END
053600921015     C*
053700921014     C* CODICI ANOMALIA
053800921015     C                   MOVEL     '7C'          COD
053900020809     C                   Z-ADD     1             XB                3 0
054000921014     C*
054100020809    1C                   DO        10            XB
054200020809     C     '?'           SCAN      ANM(XB)                                31
054300960402     C* RICERCA
054400960402    2C     *IN31         IFEQ      *ON
054500020809     C                   MOVEL     CODUT         §KUT
054600020809     C                   MOVEL     '7C'          §COD
054700020809     C                   MOVE      *BLANKS       ANM(XB)
054800921014     C                   CALL      'X§TABER'
054900020809     C                   PARM                    §KUT              1 0
055000020809     C                   PARM                    §COD              2
055100921014     C                   PARM                    §KEY              8
055200921014     C                   PARM                    §DES             30
055300020809     C                   MOVEL     §KEY          ANM(XB)
055400921014     C                   SETON                                        90
055500020809     C     XB            ADD       70            IN                3 0
055600921016     C                   MOVEL     '1'           *IN(IN)
055700921014     C*
055800960402   X2C                   ELSE
055900921014     C*
056000960402    3C     *IN90         IFEQ      '0'
056100020809     C     ANM(XB)       ANDNE     *BLANKS
056200020809     C     ANM(XB)       ANDNE     *ZEROS
056300921014     C*
056400921014     C                   MOVEL     *BLANKS       KEY
056500020809     C                   MOVEL     ANM(XB)       KEY
056600921014     C     KTAB          CHAIN     TABEL                              30
056700960402    4C  N30TBLFLG        IFNE      ' '
056800921014     C                   SETON                                        30
056900960402    4C                   END
057000960402     C*
057100960402     C* ERRORE: CODICE ANNULLATO A INESISTENTE
057200960402    4C     *IN30         IFEQ      *ON
057300020809     C     XB            ADD       70            IN                3 0
057400960326     C                   MOVEL     '1'           *IN(IN)
057500960402     C                   SETON                                        9028
057600960402     C                   MOVEL     MSG(9)        VIDMSG
057700020809     C                   Z-ADD     10            XB
057800960402   X4C                   ELSE
057900960402     C* SE SELEZIONATA, CONTROLLO SE CONGRUENTE CON IL TIPO RICHIESTO
058000960402     C                   MOVEL     TBLUNI        DS7C
058100960402     C*
058200960506    5C     VIDIE         IFEQ      'I'
058300960402     C     §7CAIE        ANDEQ     'E'
058400960402     C*
058500960402     C     VIDIE         ORNE      'I'
058600960402     C     VIDIE         ANDNE     ' '
058700960402     C     §7CAIE        ANDEQ     'I'
058800960402     C                   SETON                                        2890
058900960402     C                   MOVEL     MSG(10)       VIDMSG
059000960506    5C                   ENDIF
059100960402     C*
059200960506    5C     VIDIE         IFEQ      'D'
059300960506    6C     §7CIDD        IFNE      'S'
059400960402     C                   SETON                                        2890
059500960402     C                   MOVEL     MSG(11)       VIDMSG
059600960506    6C                   ENDIF
059700960506    5C                   ENDIF
059800960402     C*
059900960506    5C     VIDIE         IFEQ      'N'
060000960506    6C     §7CIDD        IFEQ      'S'
060100960402     C                   SETON                                        2890
060200960402     C                   MOVEL     MSG(12)       VIDMSG
060300960506    6C                   ENDIF
060400960506    5C                   ENDIF
060500960506     C* VEDO SE CONGRUENTE TRA PARTENZA E ARRIVO
060600960506     C  N90VIDPAR        IFEQ      'P'
060700960506    5C     §7CPAR        ANDEQ     'A'
060800960506     C                   SETON                                        2890
060900960506     C                   MOVEL     MSG(2)        VIDMSG
061000960506    5C                   ENDIF
061100960506     c*
061200960506    5C  N90VIDPAR        IFEQ      'A'
061300960506     C     §7CPAR        ANDEQ     'P'
061400960506     C                   SETON                                        2890
061500960506     C                   MOVEL     MSG(3)        VIDMSG
061600960506    5C                   ENDIF
061700960402     C* ERRORE
061800960402     C     *IN28         IFEQ      *ON
061900020809     C     XB            ADD       70            IN                3 0
062000960402     C                   MOVEL     '1'           *IN(IN)
062100020809     C                   Z-ADD     10            XB
062200960402     C                   ENDIF
062300960402     C*
062400960326     C                   SETON                                        01
062500960402    4C                   ENDIF
062600960402    3C                   ENDIF
062700921014     C*
062800960402    2C                   END
062900960402    1C                   END
063000921014     C*
063100960326     C* SELEZIONARE ALMENO UNA ANOMALIA PER ESCLUSIONE
063200921014     C  N01
063300921014     CANN90VIDINC        IFEQ      'E'
063400921014     C                   SETON                                        4790
063500921014     C                   GOTO      ENDCTR
063600921014     C                   END
063700030717     C**
063800030717     C* P.O. SEGNACOLLO/NUMERO SERIE
063900030717     C                   IF        VIDNRS =  99 AND
064000030717     C                             VIDPOS <> *ZEROS
064100030717     C                   SETON                                        146790
064200030717     C                   GOTO      ENDCTR
064300030717     C                   ENDIF
064400030717     C                   IF        VIDNRS <> 99 AND
064500030717     C                             VIDPOS =  *ZEROS
064600030717     C                   SETON                                        146890
064700030717     C                   GOTO      ENDCTR
064800030717     C                   ENDIF
064900980402     C**
065000980402     C* LINEA DI ARRIVO
065100030717     C                   CLEAR                   VICLNA
065200030717     C                   CLEAR                   VIDDLA
065300980402    1C     VIDLNA        IFNE      '   '
065400980403     C     VIDLNA        ANDNE     '000'
065500980402     C*
065600980402     C     '?'           SCAN      VIDLNA                                 90
065700980402    2C     *IN90         IFEQ      *ON
065800980402     C                   MOVE      *BLANKS       §COD1
065900980402     C                   CALL      'TNSD19R'
066000980402     C                   PARM                    §COD1             3
066100980402     C                   PARM                    §TIP              1
066200980402     C                   PARM                    §DES1            25
066300980402     C                   MOVEL     §COD1         VIDLNA
066400980402     C                   MOVEL     §DES1         VIDDLA           25
066500980402     C                   SETON                                        9013
066600980402     C                   GOTO      ENDCTR
066700980402     C*
066800980402   2XC                   ELSE
066900980402     C*
067000980403     C                   TESTN                   VIDLNA               33
067100980403     C   33              MOVE      VIDLNA        UNO               1
067200980403     C   33UNO           COMP      *ZERO                                13
067300980403     C  N33              SETON                                        13
067400980403     C   13              MOVEL     MSG(14)       VIDMSG
067500980403     C   13              SETON                                        9028
067600980403     C   13              MOVEL     *BLANKS       VIDDLA
067700980403     C   13              GOTO      ENDCTR
067800980403     C                   MOVE      VIDLNA        VICLNA            3 0
067900980402     C     VICLNA        CHAIN     AZORG                              33
068000980402     C*
068100980402    3C  N33ORGFVA        IFNE      ' '
068200980402     C                   SETON                                        33
068300980402    3C                   ENDIF
068400980402     C*
068500980402    4C  N33ORGFAG        IFNE      'A'
068600980402     C     ORGFAG        ANDNE     'F'
068700980402     C                   SETON                                        33
068800980402    3C                   ENDIF
068900980402     C*
069000980402     C* 33 ON  - LINEA ARRIVO   INESISTENTE
069100980402    3C     *IN33         IFEQ      *ON
069200980402     C                   MOVEL     MSG(14)       VIDMSG
069300980402     C                   SETON                                        139028
069400980402     C                   MOVEL     *BLANKS       VIDDLA
069500980402     C                   GOTO      ENDCTR
069600980402   X3C                   ELSE
069700980403     C                   MOVEL     ORGDES        VIDDLA
069800980402    3C                   ENDIF
069900980402    2C                   ENDIF
070000980403     C*
070100980403    2C     VIDPAR        IFEQ      'A'
070200980403     C* IN CASO DI ANOMALIE IN ARRIVO CONTROLLO CHE LA LINEA DI ARRIVO SIA
070300980402     C* GESTITA DAL PUNTO OPERATIVO RICHIESTO
070400980402     C* CARICO LE LINEE DI ARRIVO DEL PUNTO OPERATIVO
070500980402     C                   EXSR      CARL6
070600980402     C* CONTROLLO LA LINEA DI ARRIVO SELEZIONATA CON LA L6
070700980402     C     VICLNA        LOOKUP    L6                                     34
070800980402     C  N34              SETON                                        139028
070900980402     C  N34              MOVEL     MSG(15)       VIDMSG
071000980403    2C                   ENDIF
071100980402     C*
071200980402    1C                   ENDIF
071300921014     C*
071400921014     C     ENDCTR        ENDSR
071500960326     C*
071600960926      * CONTROLLO IL P.O.IN GESTIONE -----------------------------*
071700960326     C     CTRFGS        BEGSR
071800960613     C                   CLEAR                   V1DFGS
071900020809     c                   clear                   wOGntw
072000960326     C*
072100960613     C* E' OBBLIGATORIA IN GESTIONE/INTERROGAZIONE
072200960613     C     R34GES        IFNE      'S'
072300960613     C     VIDFGS        ANDEQ     0
072400960613     C                   MOVEL     MSG(4)        VIDMSG
072500960613     C                   SETON                                        409028
072600960613     C                   GOTO      ENDCTF
072700960613     C                   ENDIF
072800960613     C**
072900960613     C     VIDFGS        IFGT      0
073000110111     c
073100960326     C     VIDFGS        LOOKUP    L1                                     30
073200110111     c                   if        not *in30 and vidpar='A'
073300110111     c
073400110111     c* Terminal di arrivo se anomalie arrivo            anche
073500050422     C     VIDFGS        LOOKUP    TFA                                    30
073600050422     c                   endif
073700110111     c*
073800960326     C     *IN30         IFEQ      *OFF
073900960326     C     VIDFGS        OREQ      0
074000960326     C                   MOVEL     MSG(4)        VIDMSG
074100960326     C                   SETON                                        409028
074200960326     C                   GOTO      ENDCTF
074300960326     C                   ENDIF
074400960326     C* DECODIFICA
074500960326     C     VIDFGS        CHAIN     AZORG                              31
074600960326     C  N31              MOVEL     ORGDES        V1DFGS
074700020809      * Controllo se P.O. Poste
074800020809     c                   if        not *in31
074900020809     c                             and ORGfva = *blanks
075000020809     c                   movel     ORGDE3        OG143
075100020809     c                   movel     §OGntw        wOGntw
075200020809     c                   endif
075300960326     C*
075400960926     C* GESTITA DA ALTRO P.O.
075500020919     C     VIDFGS        LOOKUP    L6S                                    30
075600020919     C     *IN30         IFEQ      *ON
075700020919     C                   SETON                                        402890
075800960326     C                   MOVEL     MSG(5)        VIDMSG
075900960326     C                   GOTO      ENDCTF
076000960326     C                   ENDIF
076100960613     C                   ENDIF
076200960326     C*
076300960326     C     ENDCTF        ENDSR
076400960326     C*---------------------------------------------------------------*
076500960326     C     *INZSR        BEGSR
076600960326     C*
076700960326     C     *ENTRY        PLIST
076800960326     C                   PARM                    KPJBA
076900960326     C                   MOVEL     KPJBU         PARAM
077000960326     C*
077100960326     C                   Z-ADD     1             CODUT
077200960326     C                   CALL      'X§PARUT'
077300020809     C                   PARM                    UT§DSE0F
077400960326     C                   MOVEL     RAGUT         RSUT             20
077500960326     C                   MOVEL     REC80         CNCR80
077600960326     C*---------------------------------------------------------------*
077700960326     C     KTAB          KLIST
077800960326     C                   KFLD                    CODUT
077900960326     C                   KFLD                    COD               2
078000960326     C                   KFLD                    KEY               8
078100960326     C     KFVV          KLIST
078200960326     C                   KFLD                    COMNPG            1 0
078300960326     C                   KFLD                    NUM5
078400020916     C                   KFLD                    Vidfgs
078500020809     C     K02FGV01      klist
078600020809     C                   kfld                    NUM5
078700020809     C                   kfld                    SIMfel
078800960326     C*---------------------------------------------------------------*
078900960326     C* VERIFICO SE PGM DI GESTIONE O STAMPA
079000960327     C                   MOVEL     'FNLR44R'     VIDPGM
079100030717     C                   SETON                                        69
079200960326     C     R34GES        IFEQ      'S'
079300960326     C                   MOVE      TES(1)        VIDTES
079400960326     C                   MOVEL     'FNLR34R'     VIDPGM
079500030717     C                   SETOFF                                       69
079600960327     C                   ENDIF
079700960327     C     R34GES        IFEQ      'G'
079800960326     C                   MOVE      TES(2)        VIDTES
079900960517     C                   SETON                                        04
080000960326     C                   ENDIF
080100960327     C     R34GES        IFEQ      'I'
080200960327     C                   MOVE      TES(3)        VIDTES
080300960517     C                   SETON                                        04
080400960327     C                   ENDIF
080500960326     C                   MOVEL     VIDPGM        DLRPGM
080600960326     C*---------------------------------------------------------------*
080700030717     C***
080800030717     C* INIZIALIZZO I VALORI DI DEFAULT A VIDEO
080900030717     C***
081000030717     C                   EVAL      VIDNRS = 99
081100960326     C***
081200960926     C* CARICO TABELLA P.O. GESTITI £1
081300960326     C***
081400170221     c                   if        simpou=simfel
081500020809     C                   CLEAR                   TRUL06DS
081600960326     C                   MOVE      '£1'          D06COD
081700960326     C                   MOVEL     SIMFEL        D06KEY
081800020809     C                   MOVEL     TRUL06DS      KPJBU
081900960326     C*
082000960326     C                   CALL      'TRUL06R'
082100960326     C                   PARM                    KPJBA
082200020809     C                   MOVEL     KPJBU         TRUL06DS
082300960326     C                   MOVEA     LIN           L1
082400170221     c                   else
082500170221     c                   clear                   l1
082600170221     c                   eval      l1(1)=simpou
082700170221     c                   endif
082800960326     C***
082900960926     C* CARICO TUTTI I P.O. GESTITI DA ALTRI (DA £6)
083000960326     C***
083100960326     C                   CLEAR                   L6S
083200020809     C                   CLEAR                   TRUL06DS
083300960326     C                   MOVE      '£6'          D06COD
083400020509     C**!!!              MOVEL     'S'           D06ESC
083500020509     c                   eval      d06esc = 'G'
083600020509     c                   movel     simfel        d06key
083700960326     C                   MOVEL     'L'           D06TLA
083800020809     C                   MOVEL     TRUL06DS      KPJBU
083900960326     C                   CALL      'TRUL06R'
084000960326     C                   PARM                    KPJBA
084100020809     C                   MOVEL     KPJBU         TRUL06DS
084200960326     C                   MOVEA     LIN           L6S
084300050422     C***
084400170221     C* CARICO tutti i p.o. 2 livello di simpou
084500050422     c
084600050422     c                   Clear                   Trul09ds
084700170221     c                   Eval      I09Mod = 'TFA'
084800170221     c                   Eval      I09Tfa = simpou
084900050422     c                   Eval      I09Tla = 'L'
085000050422     c                   Call      'TRUL09R'
085100050422     c                   Parm                    Trul09ds
085200050422     c
085300050422     c* se tfa=0 non è terminal di nessuno metto comunque lai in mec
085400170221     c                   if        tfa(1) = *zeros
085500170221     c                   if        simpou=simfel
085600170221     c                   movea     L1            tfa
085700170221     c                   else
085800170221     c                   movel     simpou        tfa(1)
085900170221     c                   endif
086000170221     c                   endif
086100050422     C***
086200020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
086300020424     C                   Z-ADD     SIMPOU        VIDFGS
086400020424     C     SIMTPP        IFEQ      '2'
086500020424     C     SIMTPP        OREQ      *BLANKS
086600020424     C                   SETON                                        06
086700020424     C                   ENDIF
086800170221     c* Se però sono terminal di arrivo anche se secondo in partenza
086900170221     c*  abilito F18
087000170221     c                   if        *in06 and tfa(2)>0
087100170221     c                   setoff                                       06
087200170221     c                   endif
087300960326     C*
087400960326     C     VIDFGS        CHAIN     AZORG01L                           31
087500960326     C  N31              MOVEL     ORGDES        V1DFGS
087600960326     C**
087700960326     C* CARICO LA DS7A2
087800960326     C**
087900960326     C                   MOVEL     '7A'          COD
088000960326     C                   MOVEL(P)  '2'           KEY
088100960326     C     KTAB          CHAIN     TABEL                              30
088200960326     C     *IN30         IFEQ      *OFF
088300960326     C     TBLFLG        ANDEQ     ' '
088400960326     C                   MOVEL     TBLUNI        DS7A2
088500960326     C                   ELSE
088600960326     C                   CLEAR                   DS7A2
088700960326     C                   ENDIF
088800960326     C*
088900960326     C*
089000960326     C* DATE PER INTERROGAZIONE
089100960326     C                   TIME                    W0140            14 0
089200960326     C                   MOVE      W0140         W0080             8 0
089300960326     C*
089400960326     C                   MOVE      W0080         G02DAT
089500960326     C                   MOVEL     *BLANK        G02ERR
089600960326     C                   CALL      'XSRDA8'
089700960326     C                   PARM                    WLBDAT
089800960326     C* DATA FINALE: UDATE
089900960326     C                   MOVEL     W0080         DLRGAL
090000960326     C                   Z-ADD     W0080         PA5DFI
090100960326     C                   Z-ADD     G02INV        DATEU             8 0
090200960326     C* DATA INIZIALE: UDATE -12
090300960326     C     G02TGI        SUB       12            GIOTGI
090400960326     C*
090500960326     C                   CALL      'XSRGI8'
090600960326     C                   PARM                    WGIDAT
090700960326     C                   MOVEL     GIODAT        DLRGDA
090800960326     C                   Z-ADD     GIODAT        PA5DIN
090900960326     C*
091000960326     C                   Z-ADD     0             VIDDAD
091100960326     C                   Z-ADD     0             VIDDAA
091200960326     C                   Z-ADD     0             COMDAD
091300960326     C                   Z-ADD     0             COMDAA
091400960326     C                   MOVEL     'I'           VIDINC
091500960326     C                   MOVEL     'S'           VIDSTA
091600960326     C                   MOVEL     ' '           VIDFAS
091700040609     C                   MOVEL     ' '           VIDbva
091800960326     C                   MOVEA     *BLANKS       ANM
091900960405     C                   CLEAR                   VIDLID
092000960326     C                   MOVEL     R34PAR        VIDPAR
092100960506     C*
092200960506     C                   MOVEL     'D'           VIDIE
092300051221     c                   movel     'N'           vidmed
092400960326     C*
092500960326     C                   ENDSR
092600980402     C*---------------------------------------------------------------*
092700980402     C*                    CARL6                                      *
092800980402     C* ROUTINE DI CARICAMENTO TABELLA £6                             *
092900980402     C*---------------------------------------------------------------*
093000980402     C     CARL6         BEGSR
093100980402     C*
093200020809     C                   CLEAR                   TRUL06DS
093300980402     C                   MOVE      '£6'          D06COD
093400980402     C                   MOVEL     VIDFGS        D06KEY
093500980402     C                   MOVEL     'L'           D06TLA
093600020809     C                   MOVEL     TRUL06DS      KPJBU
093700980402     C*
093800980402     C                   CALL      'TRUL06R'
093900980402     C                   PARM                    KPJBA
094000020809     C                   MOVEL     KPJBU         TRUL06DS
094100980402     C                   MOVEA     LIN           L6
094200980402     C*
094300980403     C* VEDO SE IL P.O. E' UN TERMINAL DI PARTENZA E SE HA DEI P.O.
094400980403     C* ESTERI --> LI CARICO
094500020809     C                   CLEAR                   FNLV55DS
094600980403     C                   MOVEL     'P'           D55TPT
094700980403     C                   MOVEL     VIDFGS        D55LIN
094800980403     C                   MOVEL     DATEU         D55DRF
094900980403     C                   CALL      'FNLV55R'
095000020809     C                   PARM                    FNLV55DS
095100980403     C     D55ERR        IFNE      ' '
095200980403     C                   MOVEL     D55LIN        D55TFP
095300980403     C                   ENDIF
095400980403     C                   MOVEL     D55TFP        FGSTFP            3 0
095500980403     C*§
095600980403    1C     VIDFGS        IFEQ      D55TFP
095700980403     C                   Z-ADD     1             L                 3 0
095800980403     C                   Z-ADD     *ZEROS        W0030             3 0
095900980403     C     W0030         LOOKUP    L6(L)                                  30
096000980403     C*
096100980403     C     *LOVAL        SETLL     AZORG01L
096200980403     C                   READ      AZORG01L                               30
096300980403     C*
096400980403    2C     *IN30         DOWEQ     *OFF
096500020221     C                   MOVEL     ORGDE3        OG143
096600980403     C* P.O. NON ANNULLATO
096700980403    3C     ORGFVA        IFEQ      ' '
096800020221     C     §OGNTW        ANDEQ     'EEX'                                        ESTERA
096900020221    3C     ORGFVA        OREQ      ' '
097000020221     C     §OGNTW        ANDEQ     'EUP'                                        ESTERA
097100020221    3C     ORGFVA        OREQ      ' '
097200020221     C     §OGNTW        ANDEQ     'FED'                                        ESTERA
097300020221    3C     ORGFVA        OREQ      ' '
097400020221     C     §OGNTW        ANDEQ     'DPD'                                        ESTERA
097500980403     C*
097600980403    4C     ORGFAG        IFEQ      'F'
097700980403     C     ORGFAG        OREQ      'A'
097800980403     C* VEDO SE HA COME TERMINAL DI PARTENZA LA FIL.GESTIONE
097900020809     C                   CLEAR                   FNLV55DS
098000980403     C                   MOVEL     'P'           D55TPT
098100980403     C                   MOVEL     ORGFIL        D55LIN
098200980403     C                   MOVEL     DATEU         D55DRF
098300980403     C                   CALL      'FNLV55R'
098400020809     C                   PARM                    FNLV55DS
098500980403    5C     D55ERR        IFNE      ' '
098600980403     C                   MOVEL     D55LIN        D55TFP
098700980403    5C                   ENDIF
098800980403    5C     D55TFP        IFEQ      VIDFGS
098900980403     C                   Z-ADD     ORGFIL        L6(L)
099000980403     C                   ADD       1             L
099100980403    5C                   ENDIF
099200980403    4C                   ENDIF
099300980403    3C                   ENDIF
099400980403     C*
099500980403     C                   READ      AZORG01L                               30
099600980403    2C                   ENDDO
099700980403     C*
099800980403    1C                   ENDIF
099900980403     C*
100000980402     C                   ENDSR
100100980402**
100200911104 ***  LISTA  ANOMALIE  ***
100300960326 **  GESTIONE ANOMALIE  **
100400960327 *INTERROGAZIONE ANOMALIE*
100500960326**
100600960326Per anomalie PARTENZA E ARRIVO richiedere solo anomalie interne o tutte
100700960506L'anomalia immessa non e' una anomalia PARTENZA                                2
100800960506L'anomalia immessa non e' una anomalia ARRIVO                                  3
100900960926Punto operativo in  gestione errato
101000960926Punto operativo errato: in gestione ad altro P.O.                              5
101100960327Data formalmente errata
101200960327Dal DAL non puo' essere SUPERIORE alla data AL                                 7
101300960424Se richieste solo le anomalie aperte, non immettere i dati di chiusura         8
101400960402Codice anomalia errato
101500960506L'Anomalia e' incongruente con il tipo richiesto: Interne/ IDD/ non IDD       10
101600960402L'anomalia immessa non e' una anomalia IDD!!!                                 11
101700960402L'anomalia immessa e' una anomalia IDD!!!                                     12
101800960424Causale chiusura errata                                                       13
101900980402Linea Arrivo errata                                                           14
102000980402Linea di arrivo non in gestione                                               15
