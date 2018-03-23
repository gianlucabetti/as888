000100000626     H DECEDIT('0,') DATEDIT(*DMY.)
000200951205     F* TRTR42R  *----------------------------------------------------*
000300960131     F*   RICEZIONE  FNFV200R  IN  SEDE
000400951205     F*---------------------------------------------------------------*
000500951115     FFNFV201R  UF   E           K DISK    USROPN
000600951117     F                                     RENAME(FNFV2000:FNFV2R)
000700951207     FFNFV201L  UF A E           K DISK    USROPN
000800960223     FFNFV203L  UF   E           K DISK    USROPN
000900960223     F                                     RENAME(FNFV2000:FNFV2003)
001000951207     FTNFVS01L  IF   E           K DISK    USROPN
001100051109     FFNFGV01L  iF   E           K DISK    USROPN
001200051109     F                                     INFDS(fgv1)
001300051109     FFNFGV00f  uF   E             DISK    USROPN
001400051109     F                                     RENAME(FNFgv000:FNFgvfis)
001500960131     FFNFV401L  IF   E           K DISK
001600960131     FTABEL00F  IF   E           K DISK
001700951206     FAZORG01L  IF   E           K DISK
001800000720     FAZCAE05L  IF   E           K DISK
001900951205     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002000951204     D*
002100951204     D* DEFINIZIONE SCHIERE
002200960131     D CMD7            S             55    DIM(1) CTDATA PERRCD(1)              ALCOBJ FNFV2
002300951117     D CMD8            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
002400960131     D TE              S             66    DIM(9) CTDATA PERRCD(1)              RIGHE STAMPA
002500000626     D LPT             S              3  0 DIM(30)                              FIL GESTITE POSTE
002600941122     D*
002700941122     D                 DS
002800941122     D  GG                     1      2  0
002900941122     D  MM                     3      4  0
003000941122     D  AA                     5      8  0
003100941122     D  DATA                   1      8  0
003200051109     D FGV1            DS
003300051109     D  FGVNRR               397    400B 0
003400941122     D*
003500951206     D* DS PER TRTR42R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
003600951206     D DSTR42        E DS                  EXTNAME(TRTR42DS)
003700960223     D*
003800960223     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL22R - CONTROLLO ABBINAB.
003900960223     D DSTL22        E DS                  EXTNAME(TNTL22DS)
004000960223     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL35R - CALCOLO DATA TEORICA
004100960223     D DSTL35        E DS                  EXTNAME(TNTL35DS)
004200951206     D*
004300951127     D DSTV          E DS
004400000626     D OG143         E DS
004500000626     D KPJBA         E DS
004600930716     D  LIBSYS                92    101
004700900517     D  LEGFIL               483    492
004800900514     D  MBRFIL               493    502
004900951116     D WLBDA8          DS
005000951116     D  G08DAT                 1      8  0
005100951116     D  G08INV                 9     16  0
005200951116     D  G08ERR                17     17
005300951116     D  G08TGI                18     22  0
005400951116     D WGIDAT          DS
005500951116     D  GI8DAT                 1      8  0
005600951116     D  GI8INV                 9     16  0
005700951116     D  GI8TGI                17     21  0
005800951205     D CNCR80        E DS
005900951205     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006000951205     C*****************************************************************
006100951120     C*  RIEPILOGO INDICATORI
006200951205     C*****************************************************************
006300951206     C* 05    - C'E' LA CAUSALE RITARDO
006400951207     C* 07    - PGM RICHIAMATO PER STAMPARE I DATI DELLA PARTENZA
006500951207     C* 08    - PGM RICHIAMATO PER STAMPARE I DATI DELL'ARRIVO
006600951205     C* 20    - DI COMODO
006700951206     C* 30/35 - DI COMODO
006800951205     C* 41/42 - DI COMODO
006900951205     C*****************************************************************
007000930120     C     *ENTRY        PLIST
007100930120     C                   PARM                    KPJBA
007200951206     C                   MOVEL     KPJBU         DSTR42
007300951205     C*
007400951205     C                   Z-ADD     1             CODUT
007500951205     C                   CALL      'X§PARUT'
007600951205     C                   PARM                    UT§DSE
007700951205     C                   MOVEL     RAGUT         RSUT             20
007800951205     C                   MOVEL     REC80         CNCR80
007900951205     C*
008000930120     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
008100930120     C* ASSUMO IL PRIMO MEMBRO DEL FILE
008200930120     C     MBRFIL        IFEQ      *BLANK
008300930120     C                   MOVEL     '*FIRST'      MBRFIL
008400930120     C                   END
008500930120     C*---------------------------------------------------------------*
008600960223     C* PULIZIA CAMPI CHE POTREBBERO RIMANERE IMPOSTATI MALE
008700960223     C                   CLEAR                   WTL22
008800960223     C                   CLEAR                   WTL35
008900960223     C*
009000951206     C* TIPO LANCIO: D42TLA = "C" -           CHIUDO CON LR
009100951206     C*              D42TLA = "L" - ELABORO E CHIUDO CON LR
009200951206     C*              D42TLA = " " - ELABORO E CHIUDO IN  RETRN
009300951206    1C     D42TLA        IFNE      'C'
009400951206     C*
009500951206     C*=========================================*
009600951206     C* IL PROGRAMMA     E' RICHIAMATO !!
009700951206     C*=========================================*
009800951206    2C     D42TUP        IFEQ      'R'
009900951207     C*
010000951206     C* MOUVO I CAMPI DELLA DS NEI CAMPI DI STAMPA
010100951207     C                   MOVEL     D42EPA        FV2EPA
010200951206     C                   MOVEL     D42LNP        FV2LNP
010300951206     C                   MOVEL     D42NFV        FV2NFV
010400951206     C                   MOVEL     D42LAI        FV2LAI
010500951206     C                   MOVEL     D42HPA        FV2HPA
010600951206     C                   MOVEL     D42RTC        FV2RTC
010700951206     C* GIRO DATA REALE DI ARRIVO
010800951206     C                   MOVE      D42DPA        G08INV
010900951206     C                   MOVEL     '3'           G08ERR
011000951206     C                   CALL      'XSRDA8'
011100951206     C                   PARM                    WLBDA8
011200951206     C                   Z-ADD     G08DAT        WDPA
011300951206     C*
011400951206     C                   EXSR      STAMPA
011500951206     C*
011600951206   X2C                   ELSE
011700951206     C*=========================================*
011800951206     C* IL PROGRAMMA NON E' RICHIAMATO !!
011900951206     C*=========================================*
012000951207     C                   OPEN      FNFV201L
012100960223     C                   OPEN      FNFV203L
012200960131     C     SIMFEL        IFEQ      0
012300960131     C                   OPEN      TNFVS01L
012400960131     C                   ELSE
012500960131     C                   OPEN      FNFGV01L
012600051109     C                   OPEN      FNFGV00F
012700960131     C                   ENDIF
012800951206     C*
012900930716     C* CONTATORI
013000951115     C                   Z-ADD     0             NUMFV2            7 0
013100930716     C*
013200930120     C* APERTURA MEMBRO DA TRASCODIFICARE
013300930120     C                   EXSR      CAEXA
013400930120     C*
013500930120     C* 20 ON - CHIUDO IL PROGRAMMA
013600930120     C   20              GOTO      FINE
013700951116     C***
013800960131     C***   F N F V 2
013900951116     C***
014000951204     C     'A'           SETLL     FNFV201R
014100951204     C     'A'           READE     FNFV201R                               30
014200951204     C*
014300951206    3C     *IN30         DOWEQ     *OFF
014400951204     C* CHAINO I FOGLI VIAGGIO PARTENZA IN SEDE PER DETERMINARE SE DEVO
014500951204     C*   FLAGGARE O SFLAGGARE I RECORDS DI FNFV2
014600000626     C                   SETOFF                                       75        ON=TROVATO PT
014700000626      *
014800000626      * .. PER SEDE CERCO FOGLIO VIAGGIO ANCHE CON LINEA POSTE DI LNP
014900960131     C     SIMFEL        IFEQ      0
015000951204     C     KFVS1         CHAIN     TNFVS01L                           31
015100000626     C     *IN31         IFEQ      *ON
015200000626     C                   EXSR      CARLPT
015300000626     C     1             DO        30            PT                3 0
015400000626     C     LPT(PT)       IFNE      0
015500000626     C                   Z-ADD     LPT(PT)       LPTLNP
015600000626     C     LPTLNP        CHAIN     AZORG01L                           74
015700000626     C     *IN74         IFEQ      *OFF
015800000626     C                   MOVEL     ORGDE3        OG143
015900000626     C     §OGPT         IFEQ      'S'
016000000626      * - - - - - - - - - - - - - - - - - - - - - - - - -
016100000626     C     KFVS2         CHAIN     TNFVS01L                           31
016200000626     C  N31              SETON                                        75        TROVATO
016300000626     C  N31              LEAVE
016400000626      * - - - - - - - - - - - - - - - - - - - - - - - - -
016500000626     C                   ENDIF
016600000626     C                   ENDIF
016700000626     C                   ENDIF
016800000626     C                   ENDDO
016900000626     C                   ENDIF
017000960131     C                   ELSE
017100000628      * .. PER FILIALE CERCO SOLO CON LNP
017200960131     C     KFGV          CHAIN     FNFGV01L                           31
017300960131     C                   ENDIF
017400951204     C*
017500951206    4C     *IN31         IFEQ      *OFF
017600951204     C*
017700000628     C                   Z-ADD     FV2LNP        COMOD3            3 0
017800000628     C*
017900000626     C     *IN75         IFEQ      *OFF
018000951205     C     KFV2L         CHAIN     FNFV201L                           33
018100000626     C                   ELSE
018200000626     C     KFV2L2        CHAIN     FNFV201L                           33
018300000626     C                   ENDIF
018400000626     C*
018500000628     C                   Z-ADD     COMOD3        FV2LNP            3 0
018600951205     C  N33KFV2R         CHAIN     FNFV201R                           30
018700951204     C*
018800960131     C* SE N.TRAINO DI SEDE = 9999999 O 9999998 VUOL DIRE CHE
018900960131     C*   NON E' DA CONTROLLARE PER CUI METTO ABN=S
019000960222   4AC     SIMFEL        IFEQ      0
019100960131    5C     FVSTRS        IFNE      9999999
019200960222     C     FVSTRS        ANDNE     9999998
019300951204     C                   CLEAR                   FV2ABN
019400951206   X5C                   ELSE
019500951204     C                   MOVEL     'S'           FV2ABN
019600951206    5C                   ENDIF
019700960222   4AC                   ENDIF
019800960222     C*
019900951204     C                   MOVEL     'R'           FV2FTR
020000951204     C                   MOVEL     NUMTRA        FV2DTR
020100951205     C*
020200000626     C   75              Z-ADD     LPTLNP        FV2LNP
020300951205     C   33              WRITE     FNFV2000
020400951205     C  N33              UPDATE    FNFV2000
020500960223     C*
020600960223     C* SE F.VIAGGIO ABBINATO CONTROLLO ABBINABILITA' AL TRAINO
020700960223   4AC     SIMFEL        IFGT      0
020800960223     C     FGVTRN        ANDGT     0
020900960223     C                   EXSR      CTRTRN
021000960223   4AC                   ENDIF
021100951204     C*
021200951204     C* DELETO RECORD DA FILE DI RICEZIONE
021300951116     C                   ADD       1             NUMFV2
021400951204     C                   DELETE    FNFV2R
021500951204     C*
021600951206   X4C                   ELSE
021700951204     C* SE NON TROVATO F.V. E SONO PASSATI PIU DI 3 GG DA DATA ARRIVO
021800951204     C*   STAMPO ERRORE
021900951116     C                   CLEAR                   WLBDA8
022000951116     C                   MOVE      FV2DPA        G08INV
022100951116     C                   MOVEL     '3'           G08ERR
022200951116     C                   CALL      'XSRDA8'
022300951116     C                   PARM                    WLBDA8
022400960213     C                   MOVEL     G08DAT        WDPA
022500960212     C*
022600951116     C                   CLEAR                   WGIDAT
022700951116     C     G08TGI        ADD       3             GI8TGI
022800951116     C                   CALL      'XSRGI8'
022900951116     C                   PARM                    WGIDAT
023000951206     C* DATA ARRIVO REALE
023100960201    5C     GI8INV        IFLT      NUMTRA
023200951205     C                   EXSR      STAMPA
023300951206    5C                   ENDIF
023400951204     C*
023500951206    4C                   ENDIF
023600951204     C*
023700951204     C     'A'           READE     FNFV201R                               30
023800951206    3C                   ENDDO
023900951122     C*
024000930714     C* CHIUSURA MEMBRI TRASCODIFICATI
024100951115     C                   CLOSE     FNFV201R
024200900514     C*
024300951117     C     NUMFV2        COMP      0                                  42
024400951206     C*
024500951213     C* SOTTOMETTO PGM DI AGGIORNAMENTO DATE/ORA ARRIVO REALI SU GRIGLIA
024600960223     C     SIMFEL        IFEQ      0
024700951213     C                   CLEAR                   KPJBU
024800951213     C                   MOVEL     'TL26'        KCOAZ
024900960301     C                   CALL      'TNTL26R'
025000951213     C                   PARM                    KPJBA
025100960223     C                   ENDIF
025200951213     C*
025300951206    2C                   ENDIF
025400951206    1C                   ENDIF
025500951127     C*
025600930120     C     FINE          TAG
025700960131     C* SE SONO IN FILIALE ED HO CREATO STAMPA STAMPO I PIEDI
025800960131     C   99SIMFEL        IFGT      0
025900960131     C                   EXCEPT    PIEDI
026000960131     C                   ENDIF
026100960223     C*
026200960223     C* CALL A VUOTO AL PGM TNTL22R PER CHIUDERE I FILES
026300960223     C     WTL22         IFEQ      '1'
026400960223     C                   MOVEL     'C'           D22TLA                         *TIPO LANCIO
026500960223     C                   MOVEL     DSTL22        KPJBU
026600960223     C                   CALL      'TNTL22R'
026700960223     C                   PARM                    KPJBA
026800960223     C                   ENDIF
026900960223     C* CALL A VUOTO AL PGM TNTL35R PER CHIUDERE I FILES
027000960223     C     WTL35         IFEQ      '1'
027100960223     C                   MOVEL     'C'           D35TLA                         *TIPO LANCIO
027200960223     C                   MOVEL     DSTL35        KPJBU
027300960223     C                   CALL      'TNTL35R'
027400960223     C                   PARM                    KPJBA
027500960223     C                   ENDIF
027600951206     C*
027700951206     C* CHIUSURA PGM
027800951206    1C     D42TLA        IFEQ      ' '
027900951206     C                   RETURN
028000951206   X1C                   ELSE
028100951206     C                   SETON                                        LR
028200951206    1C                   ENDIF
028300930714     C*
028400951205     C**-------------------------------------------------------------**
028500951205     C**  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                      **
028600951205     C**-------------------------------------------------------------**
028700951205     C     CAEXA         BEGSR
028800951205     C                   MOVE      ')'           VAR11            11
028900951205     C                   MOVEL     MBRFIL        VAR11
029000951205     C                   MOVE      '))'          VAR12            12
029100951205     C                   MOVEL     MBRFIL        VAR12
029200951205     C                   Z-ADD     55            LUNG             15 5
029300951205     C*
029400951205     C******
029500951205     C*** F N F V 2 0 1 R
029600951205     C******
029700951205     C** TENTO DI ALLOCARE MEMBRO
029800951205     C                   SETOFF                                       20
029900951205     C                   MOVE      VAR12         CMD7                           FNFV201R
030000951205     C                   MOVEL     *BLANKS       COMMAN
030100951205     C                   MOVEA     CMD7(1)       COMMAN           80
030200951205     C                   CALL      'QCMDEXC'                            20
030300951205     C                   PARM                    COMMAN
030400951205     C                   PARM                    LUNG
030500951205     C*
030600951205     C* SE ALLOCAZIONE NON RIUSCITA: CHKOBJ+ADDLFM
030700951205     C     *IN20         IFEQ      '1'
030800951205     C                   MOVEL     'FNFV200R'    FISICO
030900951205     C                   MOVEL     'FNFV201R'    LOGICO
031000951205     C*
031100951205     C                   CALL      'TRUL50C'
031200951205     C                   PARM                    FISICO           10
031300951205     C                   PARM                    MBRFIL
031400951205     C                   PARM                    LIBSYS
031500951205     C                   PARM                    LOGICO           10
031600951221     C                   PARM                    MBRFIL
031700951205     C                   PARM                    ULFLG             1
031800951205     C* RITENTO ALLOCAZIONE
031900951205     C                   CALL      'QCMDEXC'                            20
032000951205     C                   PARM                    COMMAN
032100951205     C                   PARM                    LUNG
032200951205     C                   ENDIF
032300951205     C*
032400951205     C   20              GOTO      ENDCAE
032500951205     C*
032600951205     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO               FNFV201R
032700951205     C                   MOVE      VAR11         CMD8
032800951205     C                   MOVEL     *BLANKS       COMMAN
032900951205     C                   MOVEA     CMD8(1)       COMMAN
033000951205     C                   CALL      'QCMDEXC'                            20
033100951205     C                   PARM                    COMMAN
033200951205     C                   PARM                    LUNG
033300951205     C*
033400951205     C   20              GOTO      ENDCAE
033500951205     C*
033600951205     C** APERTURA FILE
033700951205     C                   OPEN      FNFV201R
033800951205     C*
033900951205     C     ENDCAE        ENDSR
034000951205     C*
034100960222     C**-------------------------------------------------------------**
034200960222     C**  ROUTINE DI CONTROLLO ABBINABILITA' AL TRAINO               **
034300960222     C**-------------------------------------------------------------**
034400960222     C     CTRTRN        BEGSR
034500960223     C                   MOVEL     'T'           WTDH
034600960222     C*
034700960222     C* SE F.VIAGGIO ABBINATO CONTROLLO ABBINABILITA' AL TRAINO
034800960222     C                   CLEAR                   DSTL22
034900960222     C                   Z-ADD     FGVLNP        D22LNP
035000960222     C                   Z-ADD     FGVNFV        D22NFV
035100960222     C                   MOVEL     'F'           D22FFS
035200960222     C                   MOVEL     DSTL22        KPJBU
035300960222     C                   CALL      'TNTL22R'
035400960222     C                   PARM                    KPJBA
035500960222     C                   MOVEL     KPJBU         DSTL22
035600960222     C                   MOVEL     '1'           WTL22             1
035700960222     C*
035800960222     C* D22ERR <> " "  ---> TRAINO NON ABBINABILE: AZZERO I CAMPI DEL
035900960222     C*                     TRAINO SUL FILE FNFGV00F/FNFV200F
036000960222    1C     D22ERR        IFNE      ' '
036100051109     c* chain su fisico del foglio viaggio per eliminare il num traino
036200051109     c     fgvnrr        chain     fnfgv00f
036300960222     C*
036400960223     C* CLEARO CAMPI DI FNFGV00F
036500960223     C                   CLEAR                   FGVTRN
036600960223     C                   CLEAR                   FGVFRQ
036700051109     C                   UPDATE    FNFGVfis
036800960223     C*
036900960223     C* ANNULLO DATA/ORA TEORICA ARRIVO DAL FILE FNFV200F
037000960223     C                   MOVEL     'A'           WEPA
037100960223     C     KFV3          SETLL     FNFV203L
037200960223     C     KFV3          READE     FNFV203L                               32
037300960223    2C     *IN32         DOWEQ     *OFF
037400960223     C                   DELETE    FNFV2003
037500960223     C     KFV3          READE     FNFV203L                               32
037600960223    2C                   ENDDO
037700960223     C*
037800960223     C* ANNULLO DATA/ORA TEORICA PARTENZA DAL FILE FNFV200F
037900960223     C                   MOVEL     'P'           WEPA
038000960223     C                   CLEAR                   WLAI
038100960223     C     KFV2          CHAIN     FNFV201L                           32
038200960223     C  N32              DELETE    FNFV2000
038300960222     C*
038400960222   X1C                   ELSE
038500960223     C*
038600960223     C* CALCOLO LA DATA TEORICA DI ARRIVO DEL FOGLIO VIAGGIO E LA
038700960223     C*   SCRIVO SU FNFV200F
038800960223     C                   CLEAR                   DSTL35
038900960223     C                   MOVEL     'D'           D35TUP
039000960223     C                   MOVEL     'A'           D35EPA
039100960223     C                   MOVEL     FGVTRN        D35TRN
039200960223     C                   MOVEL     D22DDE        D35DDE
039300960223     C                   MOVEL     D22DTN        D35DTN
039400960223     C                   MOVEL     FV2LAI        D35FIL
039500960223     C                   MOVEL     FGVFRQ        D35TFM
039600960223     C                   MOVEL     DSTL35        KPJBU
039700960223     C                   CALL      'TNTL35R'
039800960223     C                   PARM                    KPJBA
039900960223     C                   MOVEL     KPJBU         DSTL35
040000960223     C                   MOVEL     '1'           WTL35             1
040100960223     C*
040200960223    2C     D35ERR        IFEQ      ' '
040300960223     C                   MOVEL     'A'           WEPA
040400960223     C                   MOVEL     FV2LAI        WLAI
040500960223     C     KFV2          CHAIN     FNFV201L                           32
040600960223     C*
040700960223     C                   CLEAR                   FV2ATB
040800960223     C                   CLEAR                   FV2RTC
040900960223     C                   CLEAR                   FV2ABN
041000960223     C                   MOVEL     'A'           FV2EPA
041100960223     C                   MOVEL     'T'           FV2TDH
041200960223     C                   MOVEL     D35DTE        FV2DPA
041300960223     C                   MOVEL     D35HTE        FV2HPA
041400960223     C                   MOVEL     'R'           FV2FTR
041500960223     C                   MOVEL     NUMTRA        FV2DTR
041600960223     C*
041700960223    3C     *IN32         IFEQ      *ON
041800960223     C                   WRITE     FNFV2000
041900960223   X3C                   ELSE
042000960223     C                   UPDATE    FNFV2000
042100960223    3C                   ENDIF
042200960223     C*
042300960223    2C                   ENDIF
042400960223    1C                   ENDIF
042500960222     C*
042600960222     C                   ENDSR
042700951205     C**-------------------------------------------------------------**
042800951205     C**  ROUTINE DI STAMPA                                          **
042900951205     C**-------------------------------------------------------------**
043000951205     C     STAMPA        BEGSR
043100951207     C                   SETOFF                                       05
043200960227     C                   CLEAR                   DESRTC
043300960227     C                   CLEAR                   WNOT
043400960131     C* EFFETTUATA STAMPA
043500960131     C                   SETON                                        99
043600951207     C*
043700951207     C* CONTROLLO SE PARTENZA O ARRIVO
043800951207    1C     *IN07         IFEQ      *OFF
043900951207     C     *IN08         ANDEQ     *OFF
044000951207    2C     FV2EPA        IFEQ      'P'
044100951207     C                   SETON                                        07
044200951207   X2C                   ELSE
044300951207     C                   SETON                                        08
044400951207    2C                   ENDIF
044500951207    1C                   ENDIF
044600951205     C*
044700951205    1C     *INOF         IFEQ      *ON
044800951205     C                   EXCEPT    TESTA
044900951205     C                   SETOFF                                       OF
045000951205    1C                   ENDIF
045100951205     C*
045200951206     C* DECODIFICA LINEA PARTENZA
045300000626     C  N75FV2LNP        CHAIN     AZORG01L                           35
045400000626     C   75LPTLNP        CHAIN     AZORG01L                           35
045500951206     C     *IN35         IFEQ      *OFF
045600951206     C                   MOVEL     ORGDES        DESLNP           15
045700951206     C                   ELSE
045800951206     C                   CLEAR                   DESLNP
045900951206     C                   ENDIF
046000951206     C* DECODIFICA LINEA INVIO
046100951206     C     FV2LAI        CHAIN     AZORG01L                           35
046200951206     C     *IN35         IFEQ      *OFF
046300951206     C                   MOVEL     ORGDES        DESLAI           15
046400951206     C                   ELSE
046500951206     C                   CLEAR                   DESLAI
046600951206     C                   ENDIF
046700951213     C*
046800951206     C* DECODIFICA CAUSALE RITARDO
046900951206    1C     FV2RTC        IFNE      ' '
047000951206     C                   SETON                                        05
047100951206     C                   MOVEL     'TC'          COD
047200951206     C                   MOVEL(P)  FV2RTC        KEY
047300951206     C     KTAB          CHAIN     TABEL                              35
047400951206    2C     *IN35         IFEQ      *OFF
047500951206     C     TBLFLG        ANDEQ     ' '
047600951206     C                   MOVEL     TBLUNI        DESRTC           20
047700951206   X2C                   ELSE
047800951206     C                   CLEAR                   DESRTC
047900951206    2C                   ENDIF
048000951213     C*
048100951213     C* ESTENSIONE DESCRIZIONE: STAMPO SOLO IL PRIMO RECORD CHE TROVO
048200951213     C                   MOVEL     'R'           WTRC
048300951213     C     KFV4L         CHAIN     FNFV401L                           35
048400951213    2C     *IN35         IFEQ      *OFF
048500951213     C                   MOVEL     FV4NOT        WNOT             17
048600951213   X2C                   ELSE
048700951213     C                   CLEAR                   WNOT
048800951213    2C                   ENDIF
048900951213    1C                   ENDIF
049000951205     C*
049100951205     C                   EXCEPT    DET
049200951205     C                   ENDSR
049300000626     C**-------------------------------------------------------------**
049400000626     C**  ROUTINE DI CARICO LPT PER LINEE POSTE                      **
049500000626     C**-------------------------------------------------------------**
049600000626     C     CARLPT        BEGSR
049700000720     C                   MOVEL     'P'           KEPA
049800000720     C                   MOVEL     FV2LNP        KTFE
049900000720     C                   CLEAR                   PT
050000000720     C*
050100000720     C     KCAE          SETLL     AZCAE05L
050200000720     C     KCAE          READE     AZCAE05L                               31
050300000720     C     *IN31         DOWEQ     *OFF
050400000720     C     CAEATB        IFEQ      ' '
050500000720     C     DATEU         ANDGE     CAEDDE
050600000720     C     DATEU         ANDLE     CAEDSC
050700000720     C                   ADD       1             PT
050800000720     C                   Z-ADD     CAETFP        LPT(PT)
050900000720     C                   ENDIF
051000000720     C     KCAE          READE     AZCAE05L                               31
051100000720     C                   ENDDO
051200000720     C*
051300000626     C                   ENDSR
051400951205     C*
051500951204     C*--- SR INIZIALE -----------------------------------------------*
051600951204     C     *INZSR        BEGSR
051700951204     C*
051800951204     C****  KLIST  ****
051900960131     C* ACCESSO FNFV401L
052000960131     C     KFV4L         KLIST
052100960131     C                   KFLD                    FV2LNP
052200960131     C                   KFLD                    FV2NFV
052300960131     C                   KFLD                    FV2EPA
052400960131     C                   KFLD                    FV2LAI
052500960131     C                   KFLD                    WTRC
052600951204     C* ACCESSO FNFV201L
052700951205     C     KFV2L         KLIST
052800951205     C                   KFLD                    FV2LNP
052900951205     C                   KFLD                    FV2NFV
053000951205     C                   KFLD                    FV2EPA
053100951205     C                   KFLD                    FV2LAI
053200960131     C                   KFLD                    FV2TDH
053300000626     C     KFV2L2        KLIST
053400000626     C                   KFLD                    LPTLNP
053500000626     C                   KFLD                    FV2NFV
053600000626     C                   KFLD                    FV2EPA
053700000626     C                   KFLD                    FV2LAI
053800000626     C                   KFLD                    FV2TDH
053900960223     C     KFV2          KLIST
054000960223     C                   KFLD                    FV2LNP
054100960223     C                   KFLD                    FV2NFV
054200960223     C                   KFLD                    WEPA
054300960223     C                   KFLD                    WLAI
054400960223     C                   KFLD                    WTDH
054500960223     C* ACCESSO FNFV203L
054600960223     C     KFV3          KLIST
054700960223     C                   KFLD                    FV2LNP
054800960223     C                   KFLD                    FV2NFV
054900960223     C                   KFLD                    WEPA
055000960223     C                   KFLD                    WTDH
055100951205     C* ACCESSO FNFV201R
055200951205     C     KFV2R         KLIST
055300951205     C                   KFLD                    FV2EPA
055400951205     C                   KFLD                    FV2LNP
055500951205     C                   KFLD                    FV2NFV
055600960131     C                   KFLD                    FV2LAI
055700960131     C                   KFLD                    FV2TDH
055800951204     C* ACCESSO TNFVS01L
055900951204     C     KFVS1         KLIST
056000951204     C                   KFLD                    FV2LNP
056100951204     C                   KFLD                    FV2NFV
056200000626     C     KFVS2         KLIST
056300000626     C                   KFLD                    LPTLNP            3 0
056400000626     C                   KFLD                    FV2NFV
056500960131     C     KFGV          KLIST
056600960131     C                   KFLD                    FV2NFV
056700960131     C                   KFLD                    FV2LNP
056800951204     C* ACCESSO TABEL00F
056900951204     C     KTAB          KLIST
057000951204     C                   KFLD                    CODUT
057100951204     C                   KFLD                    COD
057200951204     C                   KFLD                    KEY
057300000720     C     KCAE          KLIST
057400000720     C                   KFLD                    KEPA
057500000720     C                   KFLD                    KTFE
057600951205     C*
057700951205     C* ACCENDO INDICATORE DI OVERFLOW
057800951205     C                   SETON                                        OF
057900951207     C** NUMERO TRASMISSIONE = UDATE USA
058000951207     C** LA DATA DEL GIORNO LA PRENDO DA TIME
058100951207     C                   TIME                    W0140            14 0
058200951207     C                   MOVE      W0140         UDATE8            8 0
058300951207     C                   Z-ADD     UDATE8        DATA
058400951207     C                   Z-ADD     GG            MMGG              4 0
058500951207     C                   MOVEL     MM            MMGG
058600951207     C                   Z-ADD     MMGG          NUMTRA            8 0
058700951207     C                   MOVEL     AA            NUMTRA
058800951204     C*
058900951204     C****  DEFINIZIONE CAMPI  ****
059000951204     C     *LIKE         DEFINE    TBLCOD        COD
059100951204     C     *LIKE         DEFINE    TBLKEY        KEY
059200951204     C     *LIKE         DEFINE    FVSTTR        WTTR
059300951205     C     *LIKE         DEFINE    FV2DPA        WDPA
059400960131     C     *LIKE         DEFINE    FV4TRC        WTRC
059500960223     C     *LIKE         DEFINE    FV2TDH        WTDH
059600960223     C     *LIKE         DEFINE    FV2EPA        WEPA
059700960223     C     *LIKE         DEFINE    FV2LAI        WLAI
059800000720     C     *LIKE         DEFINE    CAEEPA        KEPA
059900000720     C     *LIKE         DEFINE    CAETFE        KTFE
060000000720     C*
060100000720     C                   TIME                    W0140            14 0
060200000720     C* UDATE IN GGMMAAAA
060300000720     C                   MOVE      W0140         WDTGIO            8 0
060400000720     C*
060500000720     C* UDATE IN AAAAMMGG
060600000720     C                   Z-ADD     WDTGIO        G08DAT
060700000720     C                   MOVEL     *BLANK        G08ERR
060800000720     C                   CALL      'XSRDA8'
060900000720     C                   PARM                    WLBDA8
061000000720     C                   MOVEL     G08INV        DATEU             8 0
061100951204     C                   ENDSR
061200951204     C*
061300951205     C*---------------------------------------------------------------*
061400951205     O*  T E S T A T A
061500951205     OQSYSPRT   E            TESTA            02
061600951205     O                       TE(1)               66
061700951205     O                       TE(2)              132
061800951205     O                       RSUT                20
061900951205     O                                           21 '/'
062000951205     O                       SIMFEL              24
062100951207     O                     07                    71 'PARTENZA'
062200951207     O                     08                    71 ' ARRIVO '
062300951205     O                       UDATE8             120 '  /  /    '
062400951205     O                       PAGE          Z    132
062500951205     O          E            TESTA            04
062600951205     O                       TE(3)               66
062700951205     O                       TE(6)              132
062800951207     O                     07                    74 'P A R T E N.'
062900951205     O          E            TESTA            05
063000951205     O                       TE(4)               66
063100951205     O                       TE(7)              132
063200951205     O*
063300951205     O*  D E T T A G L I O
063400951205     O          E            DET         1
063500000626     O              N75      FV2LNP               5
063600000626     O               75      LPTLNP               5
063700951206     O                                            7 '-'
063800951206     O                       DESLNP              23
063900951206     O                       FV2NFV        Z     32
064000951206     O                       FV2LAI              40
064100951206     O                                           42 '-'
064200951206     O                       DESLAI              58
064300951206     O                       WDPA                73 '  /  /    '
064400951213     O                       FV2HPA              83 '0 :  '
064500951206     O                       FV2RTC              90
064600951206     O                     05                    92 '-'
064700951206     O                       DESRTC             113
064800951213     O                       WNOT               132
064900960131     O          E            PIEDI       2
065000960131     O                                              ' '
065100960131     O          E            PIEDI       3
065200960131     O                       TE(9)              100
065300960131     O          E            PIEDI       0  0
065400960131     O                       TE(9)              100
065500951205     O*---------------------------------------------------------------*
065600960131**         CMD7
065700951115ALCOBJ OBJ((FNFV201R *FILE *EXCL           M234567890))
065800951115**         CMD8
065900951115OVRDBF FILE(FNFV201R)                   MBR(M234567890)
066000951205**         TE
066100960131XXXXXXXXXXXXXXXXXXXX               *** ELENCO DATE E ORA REALI XXX   1
066200960131XXXXX SENZA RELATIVO FV ***       TRTR42R   XX/XX/XXXX   PAG. XXXX   2
066300951206  LINEA PARTENZA           NUMERO    LINEA DI INVIO           A R    3
066400951206                           FOGLIO                               D    4
066500951206  XXX - XXXXXXXXXXXXXXX    XXXXX     XXX - XXXXXXXXXXXXXXX     XX/   5
066600951213R I V O   R E A L E    CAUSALE RITARDO           ESTENSIONE DESCR.   6
066700951206A T A       O R A                                                    7
066800951213XX/XXXX     XX:XX      X - XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXX   8
066900960131  **** MANDARE CON URGENZA IN SEDE ALL'UFFICIO CED !!!! ****         9
