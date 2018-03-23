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
005000951109     D* PASSAGGIO DATI A INTERROGAZIONE FOGLI V. PARTENZE - FNLS53R -
005100941227     D PARAM5          DS
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
006400020809     D***                                     1   3 REM
006500020809     D***                                     4   6 REMFIL
006600020809     D UT§DSE0F      E DS
006700020809     D CNCR80        E DS
006800951109     D*
006900921015     D* PASSAGGIO A INTERROGAZIONE FV ARRIVI
007000941227     D* DS STANDARD FOGLI VARI
007100941227     D DSLR01        E DS                  INZ
007200960326     D*
007300960326     D DS7A2         E DS
007400960326     D DS7D          E DS
007500960402     D DS7C          E DS
007600941227     D* DS PER TRUL06R - CARICAMENTO £X
007700020809     D TRUL06DS      E DS
007800941227     D  LIN                    1     90  0
007900960926     D                                     DIM(30)                              P.O. COMODO
008000050422
008100050422     d Trul09ds      e ds
008200050422     d  TFA                   19    108  0 Dim(30)
008300960326     D*
008400960326     D* DS PER PASSAGGIO DATI A STAMPA-FNLR35R O GESTIONE FNLR44R
008500020809     D FNLR34DS      E DS
008600020809     D FNLV55DS      E DS
008700020221     D OG143         E DS
008800020809      *
008900020809     D wOGntw          s                   like(§OGntw) inz
009000020809      *****************************************************************
009100020809      * RIEPILOGO INDICATORI
009200020809      *****************************************************************
009300020809      * 01    - SERVE PER IL CONTROLLO DELLE CAUSALI ANOMALIA
009400020809      * 02    - RICHIESTA LA STAMPA DELLE ANOMALIE
009500020809      * 04    - GESTIONE/INTERROGAZIONE
009600020809      * 05    - SPROTEGGO LA P.O. IN GESTIONE
009700020809      * 06    - SONO P.O. DI SECONDO LIVELLO: NON ABILITO F18
009800020809      * 10/12 - POSIZIONAMENTO CURSORE IN VIDEATA
009900020809      * 42/49 - ERRORE
010000020809      * 51/60 - ERRORE
010100020809      * 62/64 - ERRORE
010200020809      * 71/80 - POSIZIONAMENTO CURSORE SUI CAMPI DEI CODICI ANOMALIA
010300020809      * 81    - POSIZIONAMENTO CURSORE SU CAMPO PAR/ARR
010400020809      * 90    - ERRORE GENERICO
010500020809      *****************************************************************
010600911016     C*
010700900509     C     FOR01         TAG
010800941227     C                   EXFMT     LR34D01
010900960327     C                   SETOFF                                       289081
011000960327     C                   SETOFF                                       656640
011100910320     C** CMD3 - FINE LAVORO
011200900509     C   KC              GOTO      FINE
011300960326     C*
011400960926     C* F18 - CAMBIO P.O. IN GESTIONE
011500960326     C   KS              SETON                                        05
011600960326     C   KS              GOTO      FOR01
011700960326     C*
011800960926     C* 05 ON - CONTROLLO P.O. IN GESTIONE
011900960326     C   05              EXSR      CTRFGS
012000960326     C   05
012100960326     CAN 90              GOTO      FOR01
012200921014     C* CONTROLLO
012300921014     C                   EXSR      CONTR
012400960517     C* RITRONO A FOR01 PER ERRORE O SE SONO IN STAMPA SENZA F6
012500960517     C     *IN90         IFEQ      *ON
012600960517     C     *IN04         OREQ      *OFF
012700960517     C     *INKF         ANDEQ     *OFF
012800960517     C                   GOTO      FOR01
012900960517     C                   ENDIF
013000941227     C* CMD6 - ELABORAZIONE
013100020809     C                   CLEAR                   FNLR34DS
013200960326     C     VIDNFV        IFGT      *ZEROS
013300960326     C                   MOVEL     VIDNFV        D34NFV
013400960326     C                   MOVEL     WDFV          D34DFV
013500921015     C                   END
013600960327     C                   MOVEL     R34GES        D34GES
013700960402     C*
013800020809     C                   DO        10            XX                2 0
013900020809     C     ANM(XX)       IFLE      *ZEROS
014000020809     C                   CLEAR                   ANM(XX)
014100960402     C                   ENDIF
014200960402     C                   ENDDO
014300960327     C*
014400960327     C                   MOVEL     VIDDAD        D34DLG
014500960326     C                   MOVEL     COMDAD        D34DLA
014600960326     C                   MOVEL     VIDDAA        D34ALG
014700960326     C                   MOVEL     COMDAA        D34ALA
014800960326     C*
014900960327     C                   MOVEL     VIDDCD        D34DCG
015000960327     C                   MOVEL     COMDCD        D34DCA
015100960415     C                   MOVEL     VIDDCA        D34ACG
015200960415     C                   MOVEL     COMDCA        D34ACA
015300960327     C*
015400960326     C                   MOVEL     VIDLID        D34LID
015500960326     C                   MOVEL     VIDFGS        D34FGS
015600960326     C                   MOVEL     VIDPAR        D34PAR
015700960326     C                   MOVEL     VIDSTA        D34APR
015800960326     C                   MOVEL     VIDFAS        D34FAS
015900960326     C                   MOVEL     VIDINC        D34INC
016000960424     C                   MOVEL     VIDCCH        D34CCH
016100960326     C                   MOVEA     ANM           D34ANM
016200960506     C                   MOVEL     VIDIE         D34IE
016300980402     C                   MOVEL     VICLNA        D34LNA
016400030717     C                   MOVEL     VIDPOS        D34POS
016500030717     C                   MOVEL     VIDNRS        D34NRS
016600040610     C                   MOVEL     VIDbva        D34bva
016700051221     C                   MOVEL     VIDmed        D34med
016800020809     C                   MOVEL     FNLR34DS      KPJBU
016900921016     C*
017000960613    1C     R34GES        IFEQ      'S'
017100960926     C* SE P.O. IN GESTIONE = 0 LA LANCIO PER TUTTE
017200960613    2C     VIDFGS        IFEQ      0
017300020809     C                   Z-ADD     1             XX
017400960613     C*
017500050422     c                   if        d34par='P' or d34par=' '
017600020809    3C     L1(XX)        DOWGT     0
017700020809     C     L1(XX)        LOOKUP    L6S                                    30
017800960613    4C     *IN30         IFEQ      *OFF
017900020809     C                   MOVEL     L1(XX)        D34FGS
018000020809     C                   MOVEL     FNLR34DS      KPJBU
018100960326     C                   MOVEL     'LR35'        KCOAZ                          STAMPA
018200031008     C                   CALL      'BCH10'
018300031008 MOM C***                CALL      'FNLR35R'
018400900702     C                   PARM                    KPJBA
018500960613    4C                   ENDIF
018600960613     C*
018700020809     C                   ADD       1             XX
018800960613    3C                   ENDDO
018900050422     c
019000050422     c                   else
019100050422     c
019200050422    3C     tfa(xx)       DOWGT     0
019300050422     C     tfa(xx)       LOOKUP    L6S                                    30
019400050422    4C     *IN30         IFEQ      *OFF
019500050422     C                   MOVEL     tfa(xx)       D34FGS
019600050422     C                   MOVEL     FNLR34DS      KPJBU
019700050422     C                   MOVEL     'LR35'        KCOAZ                          STAMPA
019800050422     C                   CALL      'BCH10'
019900050422 MOM C***                CALL      'FNLR35R'
020000050422     C                   PARM                    KPJBA
020100050422    4C                   ENDIF
020200050422     C*
020300050422     C                   ADD       1             XX
020400050422    3C                   ENDDO
020500050422     c
020600050422     c                   endif
020700960613     C*
020800960613   X2C                   ELSE
020900020809     C                   MOVEL     FNLR34DS      KPJBU
021000960613     C                   MOVEL     'LR35'        KCOAZ                          STAMPA
021100031008     C                   CALL      'BCH10'
021200031008 MOM C***                CALL      'FNLR35R'
021300960613     C                   PARM                    KPJBA
021400960613    2C                   ENDIF
021500960613     C*
021600960613   X1C                   ELSE
021700960326     C*
021800960327     C                   MOVEL     'LR35'        KCOAZ                          GESTIONE
021900960327     C                   CALL      'FNLR35R'                                    INTERROGAZ
022000960326     C                   PARM                    KPJBA
022100020809     C                   MOVEL     KPJBU         FNLR34DS
022200960326     C*
022300960326     C                   SETOFF                                       05
022400960613    2C     D34F03        IFEQ      ' '
022500960326     C                   GOTO      FOR01
022600960613    2C                   ENDIF
022700960613    1C                   ENDIF
022800921015     C*
022900000000     C     FINE          TAG
023000000000     C                   SETON                                        LR
023100921014     C*
023200921014     C* CONTROLLI ----------------------------------------------------*
023300921014     C     CONTR         BEGSR
023400960326     C                   SETOFF                                       0110
023500921016     C                   SETOFF                                       117172
023600921016     C                   SETOFF                                       737475
023700921016     C                   SETOFF                                       767778
023800960326     C                   SETOFF                                       7980
023900980402     C                   SETOFF                                       13
024000960326     C                   CLEAR                   WDFV
024100950608     C                   CLEAR                   COMDAD
024200950608     C                   CLEAR                   COMDAA
024300960327     C                   CLEAR                   COMDCD
024400960327     C                   CLEAR                   COMDCA
024500960424     C                   CLEAR                   DESCCH
024600960424     C                   CLEAR                   DESFAS
024700960326     C*
024800960326     C* CONTROLLI INCROCIATI ANOMALIE PARTENZA/ARRIVO
024900960326     C* CON IL TIPO DI ANOMALIE DA STAMPARE
025000960327    1C     VIDPAR        IFEQ      ' '
025100960326     C     VIDIE         ANDNE     ' '
025200960326     C     VIDIE         ANDNE     'I'
025300960326     C                   SETON                                        819028
025400960326     C                   MOVEL     MSG(1)        VIDMSG
025500960326     C                   GOTO      ENDCTR
025600960327    1C                   ENDIF
025700960326     C**
025800960327     C* DATA APERTURA DAL
025900960326     C**
026000960326    1C     VIDDAD        IFNE      0
026100921014     C                   MOVE      VIDDAD        G02DAT
026200921014     C                   MOVEL     *BLANK        G02ERR
026300941227     C                   CALL      'XSRDA8'
026400921014     C                   PARM                    WLBDAT
026500960326    2C     G02ERR        IFEQ      '1'
026600921014     C                   SETON                                        42  90
026700921014     C                   GOTO      ENDCTR
026800960326    2C                   END
026900960326     C                   Z-ADD     G02INV        COMDAD            8 0
027000950608     C                   Z-ADD     G02DAT        VIDDAD
027100960326    1C                   END
027200921014     C*
027300921015     C* DATA APERTURA AL
027400960326    1C     VIDDAA        IFNE      0
027500921014     C                   MOVE      VIDDAA        G02DAT
027600921014     C                   MOVEL     *BLANK        G02ERR
027700941227     C                   CALL      'XSRDA8'
027800921014     C                   PARM                    WLBDAT
027900960326    2C     G02ERR        IFEQ      '1'
028000921014     C                   SETON                                        43  90
028100921014     C                   GOTO      ENDCTR
028200960326    2C                   END
028300960326     C                   Z-ADD     G02INV        COMDAA            8 0
028400950608     C                   Z-ADD     G02DAT        VIDDAA
028500921014     C* DATA DAL NO MAGGIORE DATA AL
028600960326    2C     COMDAD        IFGT      COMDAA
028700921014     C                   SETON                                        4490
028800921014     C                   GOTO      ENDCTR
028900960326    2C                   END
029000960327    1C                   END
029100960327     C**
029200960327     C* DATA CHIUSURA DAL
029300960327     C**
029400960327    1C     VIDDCD        IFNE      0
029500960327     C                   MOVE      VIDDCD        G02DAT
029600960327     C                   MOVEL     *BLANK        G02ERR
029700960327     C                   CALL      'XSRDA8'
029800960327     C                   PARM                    WLBDAT
029900960327    2C     G02ERR        IFEQ      '1'
030000960327     C                   MOVEL     MSG(6)        VIDMSG
030100960327     C                   SETON                                        652890
030200960327     C                   GOTO      ENDCTR
030300960327    2C                   ENDIF
030400960327     C                   Z-ADD     G02INV        COMDCD            8 0
030500960327     C                   Z-ADD     G02DAT        VIDDCD
030600960327    1C                   END
030700960327     C*
030800960327     C* DATA CHIUSURA AL
030900960327    1C     VIDDCA        IFNE      0
031000960327     C                   MOVE      VIDDCA        G02DAT
031100960327     C                   MOVEL     *BLANK        G02ERR
031200960327     C                   CALL      'XSRDA8'
031300960327     C                   PARM                    WLBDAT
031400960327    2C     G02ERR        IFEQ      '1'
031500960327     C                   MOVEL     MSG(6)        VIDMSG
031600960327     C                   SETON                                        662890
031700960327     C                   GOTO      ENDCTR
031800960327    2C                   END
031900960327     C                   Z-ADD     G02INV        COMDCA            8 0
032000960327     C                   Z-ADD     G02DAT        VIDDCA
032100960327     C* DATA DAL NO MAGGIORE DATA AL
032200960327    2C     COMDCD        IFGT      COMDCA
032300960327     C                   MOVEL     MSG(7)        VIDMSG
032400960327     C                   SETON                                        659028
032500960327     C                   GOTO      ENDCTR
032600960327    2C                   ENDIF
032700921014     C*
032800960327    1C                   ENDIF
032900960424     C*
033000960415     C* SE RICHIESTE SOLO LE ANOMALIE APERTE NON IMMETTERE LA DATA
033100960327     C*  DI CHIUSURA
033200960327    1C     VIDSTA        IFEQ      'S'
033300960327    2C     VIDDCD        IFGT      0
033400960327     C     VIDDCA        ORGT      0
033500960424     C     VIDCCH        ORNE      '  '
033600960327     C                   MOVEL     MSG(8)        VIDMSG
033700960327     C                   SETON                                        659028
033800960327     C                   GOTO      ENDCTR
033900960327    2C                   ENDIF
034000960327     C*
034100960327    1C                   ENDIF
034200921015     C*
034300921015     C* FASE  APERTURA ANOMALIA
034400960326    1C     VIDFAS        IFNE      ' '
034500960326    2C     VIDFAS        IFEQ      '?'
034600020809     C                   MOVEL     CODUT         §KUT
034700020809     C                   MOVEL     '7D'          §COD
034800921015     C                   MOVE      *BLANKS       VIDFAS
034900921015     C                   CALL      'X§TABER'
035000020809     C                   PARM                    §KUT              1 0
035100020809     C                   PARM                    §COD              2
035200921015     C                   PARM                    §KEY              8
035300921015     C                   PARM                    §DES             30
035400921015     C                   MOVEL     §KEY          VIDFAS
035500921015     C                   MOVEL     §DES          DESFAS
035600921016     C                   SETON                                        9010
035700921015     C                   GOTO      ENDCTR
035800921015     C*
035900960326   X2C                   ELSE
036000921014     C*
036100921015     C                   MOVEL     '7D'          COD
036200960326     C                   MOVEL(P)  VIDFAS        KEY
036300921015     C     KTAB          CHAIN     TABEL                              30
036400960326    3C  N30TBLFLG        IFNE      ' '
036500921015     C                   SETON                                        30
036600960326    3C                   END
036700921015     C*
036800921015     C   30              SETON                                        4690
036900921015     C   30              GOTO      ENDCTR
037000921015     C*
037100921015     C                   MOVEL     TBLUNI        DS7D
037200921015     C                   MOVEL     §7DDES        DESFAS
037300960326    2C                   END
037400960326    1C                   END
037500960424     C**
037600960424     C* CAUSALE CHIUSURA ANOMALIE
037700960424    1C     VIDCCH        IFNE      '  '
037800960424     C     '?'           SCAN      VIDCCH                                 90
037900960424    2C     *IN90         IFEQ      *ON
038000020809     C                   MOVEL     CODUT         §KUT
038100020809     C                   MOVEL     '7E'          §COD
038200960424     C                   MOVE      *BLANKS       VIDFAS
038300960424     C                   CALL      'X§TABER'
038400020809     C                   PARM                    §KUT              1 0
038500020809     C                   PARM                    §COD              2
038600960424     C                   PARM                    §KEY              8
038700960424     C                   PARM                    §DES             30
038800960424     C                   MOVEL     §KEY          VIDCCH
038900960424     C                   MOVEL     §DES          DESCCH
039000960424     C                   SETON                                        9012
039100960424     C                   GOTO      ENDCTR
039200960424     C*
039300960424   X2C                   ELSE
039400960424     C*
039500960424     C                   MOVEL     '7E'          COD
039600960424     C                   MOVEL(P)  VIDCCH        KEY
039700960424     C     KTAB          CHAIN     TABEL                              30
039800960424     C* ERRORE
039900960424    3C     *IN30         IFEQ      *ON
040000960424     C     TBLFLG        ORNE      ' '
040100960424     C                   SETON                                        129028
040200960426     C                   MOVEL     MSG(13)       VIDMSG
040300960424     C                   GOTO      ENDCTR
040400960424    3C                   ENDIF
040500960424     C*
040600960424     C                   MOVEL     TBLUNI        DESCCH
040700960424    2C                   ENDIF
040800960424    1C                   ENDIF
040900921015     C* NUMERO FOGLIO
041000960326    1C     VIDNFV        IFNE      *BLANKS
041100921015     C     VIDNFV        ANDNE     *ZEROS
041200921015     C*
041300960326     C* SE NUMERO FOGLIO OBBLIGATORIA LA FASE
041400960326    2C     VIDFAS        IFEQ      ' '
041500020919     C                   SETON                                        90  48
041600921015     C                   GOTO      ENDCTR
041700960326    2C                   END
041800921015     C*
041900921015     C* SE FASE 8 I.D.D. NON AMMESSO FOGLIO
042000960326    2C     §7DNPG        IFLT      1
042100921015     C                   SETON                                            49    90
042200921015     C                   GOTO      ENDCTR
042300960326    2C                   END
042400921015     C* RICERCA
042500941227     C     '?'           SCAN      VIDNFV                                 90
042600921015     C*
042700960326    2C     *IN90         IFEQ      '1'
042800921016     C                   SETON                                        11
042900941227     C* FOGLI VARI
043000960326    3C     §7DNPG        IFGT      1
043100960326     C                   MOVEL     VIDFGS        DLRFGS
043200941227     C                   Z-ADD     0             DLRNFV
043300941227     C                   MOVEL     §7DNPG        DLRNPG
043400941227     C                   MOVEL     'S'           DLRRIC
043500020919     C                   MOVEL     DSLR01        KPJBU
043600941227     C*
043700941227     C                   CALL      'FNLR02R'
043800921015     C                   PARM                    KPJBA
043900941227     C                   MOVEL     KPJBU         DSLR01
044000020917     C***                   MOVE(P)   DLRNFV        VIDNFV
044100020917     C                   move      *zeros        VIDNFV
044200020917     C                   move      DLRNFV        VIDNFV
044300960326    3C                   END
044400921015     C* FOGLI PARTENZE
044500960326    3C     §7DNPG        IFEQ      1
044600941227     C                   CLEAR                   PA5NFV
044700020809     ***C                   Z-ADD     0             PA5FGS
044800020809     C                   if        wOGntw = 'PPT'
044900020809     C                   z-add     VIDfgs        PA5fgs
045000020809     C                   else
045100020809     C                   z-add     SIMfel        PA5fgs
045200020809     C                   endif
045300941227     C                   MOVEL     PARAM5        KPJBU
045400951109     C                   CALL      'FNLS53R'
045500921015     C                   PARM                    KPJBA
045600921015     C*
045700921015     C                   MOVEL     KPJBU         PARAM5
045800020917     C***                   MOVE(P)   PA5NFV        VIDNFV
045900020917     C                   move      *zeros        VIDNFV
046000020917     C                   move      PA5NFV        VIDNFV
046100960326    3C                   END
046200020809     c                   exsr      CTRFGS
046300941227     C*
046400921015     C                   GOTO      ENDCTR
046500921015     C*
046600960326   X2C                   ELSE
046700941227     C* CONTROLLO
046800921015     C                   TESTN                   VIDNFV               31
046900020917     ***C  N31              SETON                                        6310
047000020917     C  N31              SETON                                        6390
047100921015     C  N31              GOTO      ENDCTR
047200921015     C*
047300960326    3C   31VIDNFV        IFLT      *ZEROS
047400960326     C                   SETON                                        6390
047500921015     C                   GOTO      ENDCTR
047600921015     C*
047700960326   X3C                   ELSE
047800921015     C*
047900990512     C                   MOVE      VIDNFV        NUM5              5 0
048000960326    4C     §7DNPG        IFEQ      1
048100020809     ***C     NUM5          CHAIN     FNFGV000                           32
048200020809     C     K02FGV01      chain     FNFGV000                           32
048300960326    5C  N32FGVATB        IFNE      ' '
048400921015     C                   SETON                                        32
048500960326    5C                   END
048600921015     C*
048700921015     C   32              SETON                                        6290
048800921015     C   32              GOTO      ENDCTR
048900020918     ***C*
049000020918     ***C* SE NON E' DEL P.O. IN GESTIONE ERRORE
049100020918     **5C     FGVLNP        IFNE      VIDFGS
049200020918     ***C                   SETON                                        4590
049300020918     ***C                   GOTO      ENDCTR
049400020918     **5C                   ENDIF
049500020918      *
049600020918      * Foglio di P.O. non gestibile
049700020918     c     K02FGV01      chain     FNFGW000
049800020918if  4c                   if           (wOGntw  =  'PPT'
049900020918     c                             and (not %found(FNFGW01L)
050000020918     c                             or   FGWatb <> *blanks
050100020918     c                             or   FGWfgs <> VIDfgs))
050200020918     c                             or (wOGntw  <> 'PPT'
050300020918     c                             and (%found(FNFGW01L)
050400020918     c                             and  FGWatb =  *blanks
050500020918     c                             and  FGWfgs <> *zeros
050600020918     c                             and  FGWfgs <> VIDfgs))
050700020918     c                   seton                                        4590
050800020918     c                   goto      ENDCTR
050900020918e   4c                   endif
051000921015     C*
051100960326     C                   MOVE      FGVDFV        WDFV              8 0
051200921015     C*
051300960326   X4C                   ELSE
051400921015     C*
051500921015     C                   MOVEL     §7DNPG        COMNPG
051600941227     C     KFVV          CHAIN     FNFVV000                           32
051700941227     C*
051800020916    5C**!!!*IN32         DOWEQ     *OFF
051900020916    5c                   If         *In32 or fvvatb <> *Blanks
052000960926     C* IL FOGLIO DEVE APPARTENERE AL P.O. IN GESTIONE
052100020916    6C**!!!FVVFGS        IFNE      VIDFGS
052200020916     C**!!!FVVATB        ORNE      ' '
052300941227     C                   SETOFF                                       31
052400020916    6C**!!!              ENDIF
052500941227     C*
052600020916     C*!N31KFVV          READE     FNFVV000                               32
052700020916     C*!!31              SETON                                        32
052800020916    5C                   ENDIF
052900921015     C*
053000941227     C  N31              SETON                                        6390
053100941227     C  N31              GOTO      ENDCTR
053200921015     C*
053300960326    5C     FVVSPG        IFNE      §7DSPG
053400921015     C                   SETON                                        6490
053500921015     C                   GOTO      ENDCTR
053600960326    5C                   END
053700921015     C*
053800960326     C                   MOVE      FVVDFV        WDFV
053900960326    4C                   END
054000921015     C*
054100960326    3C                   END
054200960326    2C                   END
054300960326    1C                   END
054400921015     C*
054500921014     C* CODICI ANOMALIA
054600921015     C                   MOVEL     '7C'          COD
054700020809     C                   Z-ADD     1             XB                3 0
054800921014     C*
054900020809    1C                   DO        10            XB
055000020809     C     '?'           SCAN      ANM(XB)                                31
055100960402     C* RICERCA
055200960402    2C     *IN31         IFEQ      *ON
055300020809     C                   MOVEL     CODUT         §KUT
055400020809     C                   MOVEL     '7C'          §COD
055500020809     C                   MOVE      *BLANKS       ANM(XB)
055600921014     C                   CALL      'X§TABER'
055700020809     C                   PARM                    §KUT              1 0
055800020809     C                   PARM                    §COD              2
055900921014     C                   PARM                    §KEY              8
056000921014     C                   PARM                    §DES             30
056100020809     C                   MOVEL     §KEY          ANM(XB)
056200921014     C                   SETON                                        90
056300020809     C     XB            ADD       70            IN                3 0
056400921016     C                   MOVEL     '1'           *IN(IN)
056500921014     C*
056600960402   X2C                   ELSE
056700921014     C*
056800960402    3C     *IN90         IFEQ      '0'
056900020809     C     ANM(XB)       ANDNE     *BLANKS
057000020809     C     ANM(XB)       ANDNE     *ZEROS
057100921014     C*
057200921014     C                   MOVEL     *BLANKS       KEY
057300020809     C                   MOVEL     ANM(XB)       KEY
057400921014     C     KTAB          CHAIN     TABEL                              30
057500960402    4C  N30TBLFLG        IFNE      ' '
057600921014     C                   SETON                                        30
057700960402    4C                   END
057800960402     C*
057900960402     C* ERRORE: CODICE ANNULLATO A INESISTENTE
058000960402    4C     *IN30         IFEQ      *ON
058100020809     C     XB            ADD       70            IN                3 0
058200960326     C                   MOVEL     '1'           *IN(IN)
058300960402     C                   SETON                                        9028
058400960402     C                   MOVEL     MSG(9)        VIDMSG
058500020809     C                   Z-ADD     10            XB
058600960402   X4C                   ELSE
058700960402     C* SE SELEZIONATA, CONTROLLO SE CONGRUENTE CON IL TIPO RICHIESTO
058800960402     C                   MOVEL     TBLUNI        DS7C
058900960402     C*
059000960506    5C     VIDIE         IFEQ      'I'
059100960402     C     §7CAIE        ANDEQ     'E'
059200960402     C*
059300960402     C     VIDIE         ORNE      'I'
059400960402     C     VIDIE         ANDNE     ' '
059500960402     C     §7CAIE        ANDEQ     'I'
059600960402     C                   SETON                                        2890
059700960402     C                   MOVEL     MSG(10)       VIDMSG
059800960506    5C                   ENDIF
059900960402     C*
060000960506    5C     VIDIE         IFEQ      'D'
060100960506    6C     §7CIDD        IFNE      'S'
060200960402     C                   SETON                                        2890
060300960402     C                   MOVEL     MSG(11)       VIDMSG
060400960506    6C                   ENDIF
060500960506    5C                   ENDIF
060600960402     C*
060700960506    5C     VIDIE         IFEQ      'N'
060800960506    6C     §7CIDD        IFEQ      'S'
060900960402     C                   SETON                                        2890
061000960402     C                   MOVEL     MSG(12)       VIDMSG
061100960506    6C                   ENDIF
061200960506    5C                   ENDIF
061300960506     C* VEDO SE CONGRUENTE TRA PARTENZA E ARRIVO
061400960506     C  N90VIDPAR        IFEQ      'P'
061500960506    5C     §7CPAR        ANDEQ     'A'
061600960506     C                   SETON                                        2890
061700960506     C                   MOVEL     MSG(2)        VIDMSG
061800960506    5C                   ENDIF
061900960506     c*
062000960506    5C  N90VIDPAR        IFEQ      'A'
062100960506     C     §7CPAR        ANDEQ     'P'
062200960506     C                   SETON                                        2890
062300960506     C                   MOVEL     MSG(3)        VIDMSG
062400960506    5C                   ENDIF
062500960402     C* ERRORE
062600960402     C     *IN28         IFEQ      *ON
062700020809     C     XB            ADD       70            IN                3 0
062800960402     C                   MOVEL     '1'           *IN(IN)
062900020809     C                   Z-ADD     10            XB
063000960402     C                   ENDIF
063100960402     C*
063200960326     C                   SETON                                        01
063300960402    4C                   ENDIF
063400960402    3C                   ENDIF
063500921014     C*
063600960402    2C                   END
063700960402    1C                   END
063800921014     C*
063900960326     C* SELEZIONARE ALMENO UNA ANOMALIA PER ESCLUSIONE
064000921014     C  N01
064100921014     CANN90VIDINC        IFEQ      'E'
064200921014     C                   SETON                                        4790
064300921014     C                   GOTO      ENDCTR
064400921014     C                   END
064500030717     C**
064600030717     C* P.O. SEGNACOLLO/NUMERO SERIE
064700030717     C                   IF        VIDNRS =  99 AND
064800030717     C                             VIDPOS <> *ZEROS
064900030717     C                   SETON                                        146790
065000030717     C                   GOTO      ENDCTR
065100030717     C                   ENDIF
065200030717     C                   IF        VIDNRS <> 99 AND
065300030717     C                             VIDPOS =  *ZEROS
065400030717     C                   SETON                                        146890
065500030717     C                   GOTO      ENDCTR
065600030717     C                   ENDIF
065700980402     C**
065800980402     C* LINEA DI ARRIVO
065900030717     C                   CLEAR                   VICLNA
066000030717     C                   CLEAR                   VIDDLA
066100980402    1C     VIDLNA        IFNE      '   '
066200980403     C     VIDLNA        ANDNE     '000'
066300980402     C*
066400980402     C     '?'           SCAN      VIDLNA                                 90
066500980402    2C     *IN90         IFEQ      *ON
066600980402     C                   MOVE      *BLANKS       §COD1
066700980402     C                   CALL      'TNSD19R'
066800980402     C                   PARM                    §COD1             3
066900980402     C                   PARM                    §TIP              1
067000980402     C                   PARM                    §DES1            25
067100980402     C                   MOVEL     §COD1         VIDLNA
067200980402     C                   MOVEL     §DES1         VIDDLA           25
067300980402     C                   SETON                                        9013
067400980402     C                   GOTO      ENDCTR
067500980402     C*
067600980402   2XC                   ELSE
067700980402     C*
067800980403     C                   TESTN                   VIDLNA               33
067900980403     C   33              MOVE      VIDLNA        UNO               1
068000980403     C   33UNO           COMP      *ZERO                                13
068100980403     C  N33              SETON                                        13
068200980403     C   13              MOVEL     MSG(14)       VIDMSG
068300980403     C   13              SETON                                        9028
068400980403     C   13              MOVEL     *BLANKS       VIDDLA
068500980403     C   13              GOTO      ENDCTR
068600980403     C                   MOVE      VIDLNA        VICLNA            3 0
068700980402     C     VICLNA        CHAIN     AZORG                              33
068800980402     C*
068900980402    3C  N33ORGFVA        IFNE      ' '
069000980402     C                   SETON                                        33
069100980402    3C                   ENDIF
069200980402     C*
069300980402    4C  N33ORGFAG        IFNE      'A'
069400980402     C     ORGFAG        ANDNE     'F'
069500980402     C                   SETON                                        33
069600980402    3C                   ENDIF
069700980402     C*
069800980402     C* 33 ON  - LINEA ARRIVO   INESISTENTE
069900980402    3C     *IN33         IFEQ      *ON
070000980402     C                   MOVEL     MSG(14)       VIDMSG
070100980402     C                   SETON                                        139028
070200980402     C                   MOVEL     *BLANKS       VIDDLA
070300980402     C                   GOTO      ENDCTR
070400980402   X3C                   ELSE
070500980403     C                   MOVEL     ORGDES        VIDDLA
070600980402    3C                   ENDIF
070700980402    2C                   ENDIF
070800980403     C*
070900980403    2C     VIDPAR        IFEQ      'A'
071000980403     C* IN CASO DI ANOMALIE IN ARRIVO CONTROLLO CHE LA LINEA DI ARRIVO SIA
071100980402     C* GESTITA DAL PUNTO OPERATIVO RICHIESTO
071200980402     C* CARICO LE LINEE DI ARRIVO DEL PUNTO OPERATIVO
071300980402     C                   EXSR      CARL6
071400980402     C* CONTROLLO LA LINEA DI ARRIVO SELEZIONATA CON LA L6
071500980402     C     VICLNA        LOOKUP    L6                                     34
071600980402     C  N34              SETON                                        139028
071700980402     C  N34              MOVEL     MSG(15)       VIDMSG
071800980403    2C                   ENDIF
071900980402     C*
072000980402    1C                   ENDIF
072100921014     C*
072200921014     C     ENDCTR        ENDSR
072300960326     C*
072400960926      * CONTROLLO IL P.O.IN GESTIONE -----------------------------*
072500960326     C     CTRFGS        BEGSR
072600960613     C                   CLEAR                   V1DFGS
072700020809     c                   clear                   wOGntw
072800960326     C*
072900960613     C* E' OBBLIGATORIA IN GESTIONE/INTERROGAZIONE
073000960613     C     R34GES        IFNE      'S'
073100960613     C     VIDFGS        ANDEQ     0
073200960613     C                   MOVEL     MSG(4)        VIDMSG
073300960613     C                   SETON                                        409028
073400960613     C                   GOTO      ENDCTF
073500960613     C                   ENDIF
073600960613     C**
073700960613     C     VIDFGS        IFGT      0
073800110111     c
073900960326     C     VIDFGS        LOOKUP    L1                                     30
074000110111     c                   if        not *in30 and vidpar='A'
074100110111     c
074200110111     c* Terminal di arrivo se anomalie arrivo            anche
074300050422     C     VIDFGS        LOOKUP    TFA                                    30
074400050422     c                   endif
074500110111     c*
074600960326     C     *IN30         IFEQ      *OFF
074700960326     C     VIDFGS        OREQ      0
074800960326     C                   MOVEL     MSG(4)        VIDMSG
074900960326     C                   SETON                                        409028
075000960326     C                   GOTO      ENDCTF
075100960326     C                   ENDIF
075200960326     C* DECODIFICA
075300960326     C     VIDFGS        CHAIN     AZORG                              31
075400960326     C  N31              MOVEL     ORGDES        V1DFGS
075500020809      * Controllo se P.O. Poste
075600020809     c                   if        not *in31
075700020809     c                             and ORGfva = *blanks
075800020809     c                   movel     ORGDE3        OG143
075900020809     c                   movel     §OGntw        wOGntw
076000020809     c                   endif
076100960326     C*
076200960926     C* GESTITA DA ALTRO P.O.
076300020919     C     VIDFGS        LOOKUP    L6S                                    30
076400020919     C     *IN30         IFEQ      *ON
076500020919     C                   SETON                                        402890
076600960326     C                   MOVEL     MSG(5)        VIDMSG
076700960326     C                   GOTO      ENDCTF
076800960326     C                   ENDIF
076900960613     C                   ENDIF
077000960326     C*
077100960326     C     ENDCTF        ENDSR
077200960326     C*---------------------------------------------------------------*
077300960326     C     *INZSR        BEGSR
077400960326     C*
077500960326     C     *ENTRY        PLIST
077600960326     C                   PARM                    KPJBA
077700960326     C                   MOVEL     KPJBU         PARAM
077800960326     C*
077900960326     C                   Z-ADD     1             CODUT
078000960326     C                   CALL      'X§PARUT'
078100020809     C                   PARM                    UT§DSE0F
078200960326     C                   MOVEL     RAGUT         RSUT             20
078300960326     C                   MOVEL     REC80         CNCR80
078400960326     C*---------------------------------------------------------------*
078500960326     C     KTAB          KLIST
078600960326     C                   KFLD                    CODUT
078700960326     C                   KFLD                    COD               2
078800960326     C                   KFLD                    KEY               8
078900960326     C     KFVV          KLIST
079000960326     C                   KFLD                    COMNPG            1 0
079100960326     C                   KFLD                    NUM5
079200020916     C                   KFLD                    Vidfgs
079300020809     C     K02FGV01      klist
079400020809     C                   kfld                    NUM5
079500020809     C                   kfld                    SIMfel
079600960326     C*---------------------------------------------------------------*
079700960326     C* VERIFICO SE PGM DI GESTIONE O STAMPA
079800960327     C                   MOVEL     'FNLR44R'     VIDPGM
079900030717     C                   SETON                                        69
080000960326     C     R34GES        IFEQ      'S'
080100960326     C                   MOVE      TES(1)        VIDTES
080200960326     C                   MOVEL     'FNLR34R'     VIDPGM
080300030717     C                   SETOFF                                       69
080400960327     C                   ENDIF
080500960327     C     R34GES        IFEQ      'G'
080600960326     C                   MOVE      TES(2)        VIDTES
080700960517     C                   SETON                                        04
080800960326     C                   ENDIF
080900960327     C     R34GES        IFEQ      'I'
081000960327     C                   MOVE      TES(3)        VIDTES
081100960517     C                   SETON                                        04
081200960327     C                   ENDIF
081300960326     C                   MOVEL     VIDPGM        DLRPGM
081400960326     C*---------------------------------------------------------------*
081500030717     C***
081600030717     C* INIZIALIZZO I VALORI DI DEFAULT A VIDEO
081700030717     C***
081800030717     C                   EVAL      VIDNRS = 99
081900960326     C***
082000960926     C* CARICO TABELLA P.O. GESTITI £1
082100960326     C***
082200020809     C                   CLEAR                   TRUL06DS
082300960326     C                   MOVE      '£1'          D06COD
082400960326     C                   MOVEL     SIMFEL        D06KEY
082500020809     C                   MOVEL     TRUL06DS      KPJBU
082600960326     C*
082700960326     C                   CALL      'TRUL06R'
082800960326     C                   PARM                    KPJBA
082900020809     C                   MOVEL     KPJBU         TRUL06DS
083000960326     C                   MOVEA     LIN           L1
083100960326     C***
083200960926     C* CARICO TUTTI I P.O. GESTITI DA ALTRI (DA £6)
083300960326     C***
083400960326     C                   CLEAR                   L6S
083500020809     C                   CLEAR                   TRUL06DS
083600960326     C                   MOVE      '£6'          D06COD
083700020509     C**!!!              MOVEL     'S'           D06ESC
083800020509     c                   eval      d06esc = 'G'
083900020509     c                   movel     simfel        d06key
084000960326     C                   MOVEL     'L'           D06TLA
084100020809     C                   MOVEL     TRUL06DS      KPJBU
084200960326     C                   CALL      'TRUL06R'
084300960326     C                   PARM                    KPJBA
084400020809     C                   MOVEL     KPJBU         TRUL06DS
084500960326     C                   MOVEA     LIN           L6S
084600050422     C***
084700050422     C* CARICO tutti i p.o. 2 livello in arrivo di simfel
084800050422     c
084900050422     c                   Clear                   Trul09ds
085000050422     c                   Eval      I09Mod = 'TFU'
085100050422     c                   Eval      I09Tfa = simfel
085200050422     c                   Eval      I09Tla = 'L'
085300050422     c                   Call      'TRUL09R'
085400050422     c                   Parm                    Trul09ds
085500050422     c
085600050422     c* se tfa=0 non è terminal di nessuno metto comunque lai in mec
085700050422     c                   if        tfa(1) = *zeros
085800050422     c                   movea     L1            tfa
085900050422     c                   endif
086000050422     C***
086100020424     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
086200020424     C                   Z-ADD     SIMPOU        VIDFGS
086300020424     C     SIMTPP        IFEQ      '2'
086400020424     C     SIMTPP        OREQ      *BLANKS
086500020424     C                   SETON                                        06
086600020424     C                   ENDIF
086700960326     C*
086800960326     C     VIDFGS        CHAIN     AZORG01L                           31
086900960326     C  N31              MOVEL     ORGDES        V1DFGS
087000960326     C**
087100960326     C* CARICO LA DS7A2
087200960326     C**
087300960326     C                   MOVEL     '7A'          COD
087400960326     C                   MOVEL(P)  '2'           KEY
087500960326     C     KTAB          CHAIN     TABEL                              30
087600960326     C     *IN30         IFEQ      *OFF
087700960326     C     TBLFLG        ANDEQ     ' '
087800960326     C                   MOVEL     TBLUNI        DS7A2
087900960326     C                   ELSE
088000960326     C                   CLEAR                   DS7A2
088100960326     C                   ENDIF
088200960326     C*
088300960326     C*
088400960326     C* DATE PER INTERROGAZIONE
088500960326     C                   TIME                    W0140            14 0
088600960326     C                   MOVE      W0140         W0080             8 0
088700960326     C*
088800960326     C                   MOVE      W0080         G02DAT
088900960326     C                   MOVEL     *BLANK        G02ERR
089000960326     C                   CALL      'XSRDA8'
089100960326     C                   PARM                    WLBDAT
089200960326     C* DATA FINALE: UDATE
089300960326     C                   MOVEL     W0080         DLRGAL
089400960326     C                   Z-ADD     W0080         PA5DFI
089500960326     C                   Z-ADD     G02INV        DATEU             8 0
089600960326     C* DATA INIZIALE: UDATE -12
089700960326     C     G02TGI        SUB       12            GIOTGI
089800960326     C*
089900960326     C                   CALL      'XSRGI8'
090000960326     C                   PARM                    WGIDAT
090100960326     C                   MOVEL     GIODAT        DLRGDA
090200960326     C                   Z-ADD     GIODAT        PA5DIN
090300960326     C*
090400960326     C                   Z-ADD     0             VIDDAD
090500960326     C                   Z-ADD     0             VIDDAA
090600960326     C                   Z-ADD     0             COMDAD
090700960326     C                   Z-ADD     0             COMDAA
090800960326     C                   MOVEL     'I'           VIDINC
090900960326     C                   MOVEL     'S'           VIDSTA
091000960326     C                   MOVEL     ' '           VIDFAS
091100040609     C                   MOVEL     ' '           VIDbva
091200960326     C                   MOVEA     *BLANKS       ANM
091300960405     C                   CLEAR                   VIDLID
091400960326     C                   MOVEL     R34PAR        VIDPAR
091500960506     C*
091600960506     C                   MOVEL     'D'           VIDIE
091700051221     c                   movel     'N'           vidmed
091800960326     C*
091900960326     C                   ENDSR
092000980402     C*---------------------------------------------------------------*
092100980402     C*                    CARL6                                      *
092200980402     C* ROUTINE DI CARICAMENTO TABELLA £6                             *
092300980402     C*---------------------------------------------------------------*
092400980402     C     CARL6         BEGSR
092500980402     C*
092600020809     C                   CLEAR                   TRUL06DS
092700980402     C                   MOVE      '£6'          D06COD
092800980402     C                   MOVEL     VIDFGS        D06KEY
092900980402     C                   MOVEL     'L'           D06TLA
093000020809     C                   MOVEL     TRUL06DS      KPJBU
093100980402     C*
093200980402     C                   CALL      'TRUL06R'
093300980402     C                   PARM                    KPJBA
093400020809     C                   MOVEL     KPJBU         TRUL06DS
093500980402     C                   MOVEA     LIN           L6
093600980402     C*
093700980403     C* VEDO SE IL P.O. E' UN TERMINAL DI PARTENZA E SE HA DEI P.O.
093800980403     C* ESTERI --> LI CARICO
093900020809     C                   CLEAR                   FNLV55DS
094000980403     C                   MOVEL     'P'           D55TPT
094100980403     C                   MOVEL     VIDFGS        D55LIN
094200980403     C                   MOVEL     DATEU         D55DRF
094300980403     C                   CALL      'FNLV55R'
094400020809     C                   PARM                    FNLV55DS
094500980403     C     D55ERR        IFNE      ' '
094600980403     C                   MOVEL     D55LIN        D55TFP
094700980403     C                   ENDIF
094800980403     C                   MOVEL     D55TFP        FGSTFP            3 0
094900980403     C*§
095000980403    1C     VIDFGS        IFEQ      D55TFP
095100980403     C                   Z-ADD     1             L                 3 0
095200980403     C                   Z-ADD     *ZEROS        W0030             3 0
095300980403     C     W0030         LOOKUP    L6(L)                                  30
095400980403     C*
095500980403     C     *LOVAL        SETLL     AZORG01L
095600980403     C                   READ      AZORG01L                               30
095700980403     C*
095800980403    2C     *IN30         DOWEQ     *OFF
095900020221     C                   MOVEL     ORGDE3        OG143
096000980403     C* P.O. NON ANNULLATO
096100980403    3C     ORGFVA        IFEQ      ' '
096200020221     C     §OGNTW        ANDEQ     'EEX'                                        ESTERA
096300020221    3C     ORGFVA        OREQ      ' '
096400020221     C     §OGNTW        ANDEQ     'EUP'                                        ESTERA
096500020221    3C     ORGFVA        OREQ      ' '
096600020221     C     §OGNTW        ANDEQ     'FED'                                        ESTERA
096700020221    3C     ORGFVA        OREQ      ' '
096800020221     C     §OGNTW        ANDEQ     'DPD'                                        ESTERA
096900980403     C*
097000980403    4C     ORGFAG        IFEQ      'F'
097100980403     C     ORGFAG        OREQ      'A'
097200980403     C* VEDO SE HA COME TERMINAL DI PARTENZA LA FIL.GESTIONE
097300020809     C                   CLEAR                   FNLV55DS
097400980403     C                   MOVEL     'P'           D55TPT
097500980403     C                   MOVEL     ORGFIL        D55LIN
097600980403     C                   MOVEL     DATEU         D55DRF
097700980403     C                   CALL      'FNLV55R'
097800020809     C                   PARM                    FNLV55DS
097900980403    5C     D55ERR        IFNE      ' '
098000980403     C                   MOVEL     D55LIN        D55TFP
098100980403    5C                   ENDIF
098200980403    5C     D55TFP        IFEQ      VIDFGS
098300980403     C                   Z-ADD     ORGFIL        L6(L)
098400980403     C                   ADD       1             L
098500980403    5C                   ENDIF
098600980403    4C                   ENDIF
098700980403    3C                   ENDIF
098800980403     C*
098900980403     C                   READ      AZORG01L                               30
099000980403    2C                   ENDDO
099100980403     C*
099200980403    1C                   ENDIF
099300980403     C*
099400980402     C                   ENDSR
099500980402**
099600911104 ***  LISTA  ANOMALIE  ***
099700960326 **  GESTIONE ANOMALIE  **
099800960327 *INTERROGAZIONE ANOMALIE*
099900960326**
100000960326Per anomalie PARTENZA E ARRIVO richiedere solo anomalie interne o tutte
100100960506L'anomalia immessa non e' una anomalia PARTENZA                                2
100200960506L'anomalia immessa non e' una anomalia ARRIVO                                  3
100300960926Punto operativo in  gestione errato
100400960926Punto operativo errato: in gestione ad altro P.O.                              5
100500960327Data formalmente errata
100600960327Dal DAL non puo' essere SUPERIORE alla data AL                                 7
100700960424Se richieste solo le anomalie aperte, non immettere i dati di chiusura         8
100800960402Codice anomalia errato
100900960506L'Anomalia e' incongruente con il tipo richiesto: Interne/ IDD/ non IDD       10
101000960402L'anomalia immessa non e' una anomalia IDD!!!                                 11
101100960402L'anomalia immessa e' una anomalia IDD!!!                                     12
101200960424Causale chiusura errata                                                       13
101300980402Linea Arrivo errata                                                           14
101400980402Linea di arrivo non in gestione                                               15
