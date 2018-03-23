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
002400060607     D CMD9            S             55    DIM(1) CTDATA PERRCD(1)              OVRDBF   "
002500960131     D TE              S             66    DIM(9) CTDATA PERRCD(1)              RIGHE STAMPA
002600000626     D LPT             S              3  0 DIM(30)                              FIL GESTITE POSTE
002700941122     D*
002800941122     D                 DS
002900941122     D  GG                     1      2  0
003000941122     D  MM                     3      4  0
003100941122     D  AA                     5      8  0
003200941122     D  DATA                   1      8  0
003300051109     D FGV1            DS
003400051109     D  FGVNRR               397    400B 0
003500941122     D*
003600951206     D* DS PER TRTR42R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
003700951206     D DSTR42        E DS                  EXTNAME(TRTR42DS)
003800960223     D*
003900960223     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL22R - CONTROLLO ABBINAB.
004000960223     D DSTL22        E DS                  EXTNAME(TNTL22DS)
004100960223     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL35R - CALCOLO DATA TEORICA
004200960223     D DSTL35        E DS                  EXTNAME(TNTL35DS)
004300951206     D*
004400951127     D DSTV          E DS
004500000626     D OG143         E DS
004600000626     D KPJBA         E DS
004700930716     D  LIBSYS                92    101
004800900517     D  LEGFIL               483    492
004900900514     D  MBRFIL               493    502
005000951116     D WLBDA8          DS
005100951116     D  G08DAT                 1      8  0
005200951116     D  G08INV                 9     16  0
005300951116     D  G08ERR                17     17
005400951116     D  G08TGI                18     22  0
005500951116     D WGIDAT          DS
005600951116     D  GI8DAT                 1      8  0
005700951116     D  GI8INV                 9     16  0
005800951116     D  GI8TGI                17     21  0
005900951205     D CNCR80        E DS
006000951205     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006100951205     C*****************************************************************
006200951120     C*  RIEPILOGO INDICATORI
006300951205     C*****************************************************************
006400951206     C* 05    - C'E' LA CAUSALE RITARDO
006500951207     C* 07    - PGM RICHIAMATO PER STAMPARE I DATI DELLA PARTENZA
006600951207     C* 08    - PGM RICHIAMATO PER STAMPARE I DATI DELL'ARRIVO
006700951205     C* 20    - DI COMODO
006800951206     C* 30/35 - DI COMODO
006900951205     C* 41/42 - DI COMODO
007000951205     C*****************************************************************
007100930120     C     *ENTRY        PLIST
007200930120     C                   PARM                    KPJBA
007300951206     C                   MOVEL     KPJBU         DSTR42
007400951205     C*
007500951205     C                   Z-ADD     1             CODUT
007600951205     C                   CALL      'X§PARUT'
007700951205     C                   PARM                    UT§DSE
007800951205     C                   MOVEL     RAGUT         RSUT             20
007900951205     C                   MOVEL     REC80         CNCR80
008000951205     C*
008100930120     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
008200930120     C* ASSUMO IL PRIMO MEMBRO DEL FILE
008300930120     C     MBRFIL        IFEQ      *BLANK
008400930120     C                   MOVEL     '*FIRST'      MBRFIL
008500930120     C                   END
008600930120     C*---------------------------------------------------------------*
008700960223     C* PULIZIA CAMPI CHE POTREBBERO RIMANERE IMPOSTATI MALE
008800960223     C                   CLEAR                   WTL22
008900960223     C                   CLEAR                   WTL35
009000960223     C*
009100951206     C* TIPO LANCIO: D42TLA = "C" -           CHIUDO CON LR
009200951206     C*              D42TLA = "L" - ELABORO E CHIUDO CON LR
009300951206     C*              D42TLA = " " - ELABORO E CHIUDO IN  RETRN
009400951206    1C     D42TLA        IFNE      'C'
009500951206     C*
009600951206     C*=========================================*
009700951206     C* IL PROGRAMMA     E' RICHIAMATO !!
009800951206     C*=========================================*
009900951206    2C     D42TUP        IFEQ      'R'
010000951207     C*
010100951206     C* MOUVO I CAMPI DELLA DS NEI CAMPI DI STAMPA
010200951207     C                   MOVEL     D42EPA        FV2EPA
010300951206     C                   MOVEL     D42LNP        FV2LNP
010400951206     C                   MOVEL     D42NFV        FV2NFV
010500951206     C                   MOVEL     D42LAI        FV2LAI
010600951206     C                   MOVEL     D42HPA        FV2HPA
010700951206     C                   MOVEL     D42RTC        FV2RTC
010800951206     C* GIRO DATA REALE DI ARRIVO
010900951206     C                   MOVE      D42DPA        G08INV
011000951206     C                   MOVEL     '3'           G08ERR
011100951206     C                   CALL      'XSRDA8'
011200951206     C                   PARM                    WLBDA8
011300951206     C                   Z-ADD     G08DAT        WDPA
011400951206     C*
011500951206     C                   EXSR      STAMPA
011600951206     C*
011700951206   X2C                   ELSE
011800951206     C*=========================================*
011900951206     C* IL PROGRAMMA NON E' RICHIAMATO !!
012000951206     C*=========================================*
012100951207     C                   OPEN      FNFV201L
012200960223     C                   OPEN      FNFV203L
012300960131     C     SIMFEL        IFEQ      0
012400960131     C                   OPEN      TNFVS01L
012500960131     C                   ELSE
012600960131     C                   OPEN      FNFGV01L
012700051109     C                   OPEN      FNFGV00F
012800960131     C                   ENDIF
012900951206     C*
013000930716     C* CONTATORI
013100951115     C                   Z-ADD     0             NUMFV2            7 0
013200930716     C*
013300930120     C* APERTURA MEMBRO DA TRASCODIFICARE
013400930120     C                   EXSR      CAEXA
013500930120     C*
013600930120     C* 20 ON - CHIUDO IL PROGRAMMA
013700930120     C   20              GOTO      FINE
013800951116     C***
013900960131     C***   F N F V 2
014000951116     C***
014100951204     C     'A'           SETLL     FNFV201R
014200951204     C     'A'           READE     FNFV201R                               30
014300951204     C*
014400951206    3C     *IN30         DOWEQ     *OFF
014500951204     C* CHAINO I FOGLI VIAGGIO PARTENZA IN SEDE PER DETERMINARE SE DEVO
014600951204     C*   FLAGGARE O SFLAGGARE I RECORDS DI FNFV2
014700000626     C                   SETOFF                                       75        ON=TROVATO PT
014800000626      *
014900000626      * .. PER SEDE CERCO FOGLIO VIAGGIO ANCHE CON LINEA POSTE DI LNP
015000960131     C     SIMFEL        IFEQ      0
015100951204     C     KFVS1         CHAIN     TNFVS01L                           31
015200000626     C     *IN31         IFEQ      *ON
015300000626     C                   EXSR      CARLPT
015400000626     C     1             DO        30            PT                3 0
015500000626     C     LPT(PT)       IFNE      0
015600000626     C                   Z-ADD     LPT(PT)       LPTLNP
015700000626     C     LPTLNP        CHAIN     AZORG01L                           74
015800000626     C     *IN74         IFEQ      *OFF
015900000626     C                   MOVEL     ORGDE3        OG143
016000000626     C     §OGPT         IFEQ      'S'
016100000626      * - - - - - - - - - - - - - - - - - - - - - - - - -
016200000626     C     KFVS2         CHAIN     TNFVS01L                           31
016300000626     C  N31              SETON                                        75        TROVATO
016400000626     C  N31              LEAVE
016500000626      * - - - - - - - - - - - - - - - - - - - - - - - - -
016600000626     C                   ENDIF
016700000626     C                   ENDIF
016800000626     C                   ENDIF
016900000626     C                   ENDDO
017000000626     C                   ENDIF
017100960131     C                   ELSE
017200000628      * .. PER FILIALE CERCO SOLO CON LNP
017300960131     C     KFGV          CHAIN     FNFGV01L                           31
017400960131     C                   ENDIF
017500951204     C*
017600951206    4C     *IN31         IFEQ      *OFF
017700951204     C*
017800000628     C                   Z-ADD     FV2LNP        COMOD3            3 0
017900000628     C*
018000000626     C     *IN75         IFEQ      *OFF
018100951205     C     KFV2L         CHAIN     FNFV201L                           33
018200000626     C                   ELSE
018300000626     C     KFV2L2        CHAIN     FNFV201L                           33
018400000626     C                   ENDIF
018500000626     C*
018600000628     C                   Z-ADD     COMOD3        FV2LNP            3 0
018700951205     C  N33KFV2R         CHAIN     FNFV201R                           30
018800951204     C*
018900960131     C* SE N.TRAINO DI SEDE = 9999999 O 9999998 VUOL DIRE CHE
019000960131     C*   NON E' DA CONTROLLARE PER CUI METTO ABN=S
019100960222   4AC     SIMFEL        IFEQ      0
019200960131    5C     FVSTRS        IFNE      9999999
019300960222     C     FVSTRS        ANDNE     9999998
019400951204     C                   CLEAR                   FV2ABN
019500951206   X5C                   ELSE
019600951204     C                   MOVEL     'S'           FV2ABN
019700951206    5C                   ENDIF
019800960222   4AC                   ENDIF
019900960222     C*
020000951204     C                   MOVEL     'R'           FV2FTR
020100951204     C                   MOVEL     NUMTRA        FV2DTR
020200951205     C*
020300000626     C   75              Z-ADD     LPTLNP        FV2LNP
020400951205     C   33              WRITE     FNFV2000
020500951205     C  N33              UPDATE    FNFV2000
020600960223     C*
020700960223     C* SE F.VIAGGIO ABBINATO CONTROLLO ABBINABILITA' AL TRAINO
020800960223   4AC     SIMFEL        IFGT      0
020900960223     C     FGVTRN        ANDGT     0
021000960223     C                   EXSR      CTRTRN
021100960223   4AC                   ENDIF
021200951204     C*
021300951204     C* DELETO RECORD DA FILE DI RICEZIONE
021400951116     C                   ADD       1             NUMFV2
021500951204     C                   DELETE    FNFV2R
021600951204     C*
021700951206   X4C                   ELSE
021800951204     C* SE NON TROVATO F.V. E SONO PASSATI PIU DI 3 GG DA DATA ARRIVO
021900951204     C*   STAMPO ERRORE
022000951116     C                   CLEAR                   WLBDA8
022100951116     C                   MOVE      FV2DPA        G08INV
022200951116     C                   MOVEL     '3'           G08ERR
022300951116     C                   CALL      'XSRDA8'
022400951116     C                   PARM                    WLBDA8
022500960213     C                   MOVEL     G08DAT        WDPA
022600960212     C*
022700951116     C                   CLEAR                   WGIDAT
022800951116     C     G08TGI        ADD       3             GI8TGI
022900951116     C                   CALL      'XSRGI8'
023000951116     C                   PARM                    WGIDAT
023100951206     C* DATA ARRIVO REALE
023200960201    5C     GI8INV        IFLT      NUMTRA
023300951205     C                   EXSR      STAMPA
023400951206    5C                   ENDIF
023500951204     C*
023600951206    4C                   ENDIF
023700951204     C*
023800951204     C     'A'           READE     FNFV201R                               30
023900951206    3C                   ENDDO
024000951122     C*
024100930714     C* CHIUSURA MEMBRI TRASCODIFICATI
024200951115     C                   CLOSE     FNFV201R
024300900514     C*
024400951117     C     NUMFV2        COMP      0                                  42
024500951206     C*
024600951213     C* SOTTOMETTO PGM DI AGGIORNAMENTO DATE/ORA ARRIVO REALI SU GRIGLIA
024700960223     C     SIMFEL        IFEQ      0
024800951213     C                   CLEAR                   KPJBU
024900951213     C                   MOVEL     'TL26'        KCOAZ
025000960301     C                   CALL      'TNTL26R'
025100951213     C                   PARM                    KPJBA
025200960223     C                   ENDIF
025300951213     C*
025400951206    2C                   ENDIF
025500951206    1C                   ENDIF
025600951127     C*
025700930120     C     FINE          TAG
025800060607     c
025900060607     c* Se non sono riuscita ad allocare i files, risottometto
026000060607     c                   if        *in20
026100060607     C                   CALL      'BCH10'
026200060607     C                   PARM                    KPJBA
026300060607     c                   else
026400060607     c
026500960131     C* SE SONO IN FILIALE ED HO CREATO STAMPA STAMPO I PIEDI
026600960131     C   99SIMFEL        IFGT      0
026700960131     C                   EXCEPT    PIEDI
026800960131     C                   ENDIF
026900960223     C*
027000960223     C* CALL A VUOTO AL PGM TNTL22R PER CHIUDERE I FILES
027100960223     C     WTL22         IFEQ      '1'
027200960223     C                   MOVEL     'C'           D22TLA                         *TIPO LANCIO
027300960223     C                   MOVEL     DSTL22        KPJBU
027400960223     C                   CALL      'TNTL22R'
027500960223     C                   PARM                    KPJBA
027600960223     C                   ENDIF
027700960223     C* CALL A VUOTO AL PGM TNTL35R PER CHIUDERE I FILES
027800960223     C     WTL35         IFEQ      '1'
027900960223     C                   MOVEL     'C'           D35TLA                         *TIPO LANCIO
028000960223     C                   MOVEL     DSTL35        KPJBU
028100960223     C                   CALL      'TNTL35R'
028200960223     C                   PARM                    KPJBA
028300960223     C                   ENDIF
028400060607     C                   ENDIF
028500951206     C*
028600951206     C* CHIUSURA PGM
028700951206    1C     D42TLA        IFEQ      ' '
028800951206     C                   RETURN
028900951206   X1C                   ELSE
029000951206     C                   SETON                                        LR
029100951206    1C                   ENDIF
029200930714     C*
029300951205     C**-------------------------------------------------------------**
029400951205     C**  ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                      **
029500951205     C**-------------------------------------------------------------**
029600951205     C     CAEXA         BEGSR
029700951205     C                   MOVE      ')'           VAR11            11
029800951205     C                   MOVEL     MBRFIL        VAR11
029900951205     C                   MOVE      '))'          VAR12            12
030000951205     C                   MOVEL     MBRFIL        VAR12
030100951205     C                   Z-ADD     55            LUNG             15 5
030200951205     C*
030300951205     C******
030400951205     C*** F N F V 2 0 1 R
030500951205     C******
030600060607     C** TENTO DI ALLOCARE MEMBRO  in filiale
030700951205     C                   SETOFF                                       20
030800060607     c                   if        simfel>0
030900951205     C                   MOVE      VAR12         CMD7                           FNFV201R
031000951205     C                   MOVEL     *BLANKS       COMMAN
031100951205     C                   MOVEA     CMD7(1)       COMMAN           80
031200951205     C                   CALL      'QCMDEXC'                            20
031300951205     C                   PARM                    COMMAN
031400951205     C                   PARM                    LUNG
031500060607     c                   else
031600060607     C** controllo solo esistena membro se in Sede
031700060607     C                   MOVE      VAR11         CMD9                           FNFV201R
031800060607     C                   MOVEL     *BLANKS       COMMAN
031900060607     C                   MOVEA     CMD9(1)       COMMAN           80
032000060607     C                   CALL      'QCMDEXC'                            20
032100060607     C                   PARM                    COMMAN
032200060607     C                   PARM                    LUNG
032300060607     c                   endif
032400951205     C*
032500951205     C* SE ALLOCAZIONE NON RIUSCITA: CHKOBJ+ADDLFM
032600951205     C     *IN20         IFEQ      '1'
032700951205     C                   MOVEL     'FNFV200R'    FISICO
032800951205     C                   MOVEL     'FNFV201R'    LOGICO
032900951205     C*
033000951205     C                   CALL      'TRUL50C'
033100951205     C                   PARM                    FISICO           10
033200951205     C                   PARM                    MBRFIL
033300951205     C                   PARM                    LIBSYS
033400951205     C                   PARM                    LOGICO           10
033500951221     C                   PARM                    MBRFIL
033600951205     C                   PARM                    ULFLG             1
033700951205     C* RITENTO ALLOCAZIONE
033800060607     c                   if        simfel>0
033900951205     C                   CALL      'QCMDEXC'                            20
034000951205     C                   PARM                    COMMAN
034100951205     C                   PARM                    LUNG
034200060607     c                   else
034300060607     c                   setoff                                       20
034400951205     C                   ENDIF
034500060607     C                   ENDIF
034600951205     C*
034700951205     C   20              GOTO      ENDCAE
034800951205     C*
034900951205     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO               FNFV201R
035000951205     C                   MOVE      VAR11         CMD8
035100951205     C                   MOVEL     *BLANKS       COMMAN
035200951205     C                   MOVEA     CMD8(1)       COMMAN
035300951205     C                   CALL      'QCMDEXC'                            20
035400951205     C                   PARM                    COMMAN
035500951205     C                   PARM                    LUNG
035600951205     C*
035700951205     C   20              GOTO      ENDCAE
035800951205     C*
035900951205     C** APERTURA FILE
036000951205     C                   OPEN      FNFV201R
036100951205     C*
036200951205     C     ENDCAE        ENDSR
036300951205     C*
036400960222     C**-------------------------------------------------------------**
036500960222     C**  ROUTINE DI CONTROLLO ABBINABILITA' AL TRAINO               **
036600960222     C**-------------------------------------------------------------**
036700960222     C     CTRTRN        BEGSR
036800960223     C                   MOVEL     'T'           WTDH
036900960222     C*
037000960222     C* SE F.VIAGGIO ABBINATO CONTROLLO ABBINABILITA' AL TRAINO
037100960222     C                   CLEAR                   DSTL22
037200960222     C                   Z-ADD     FGVLNP        D22LNP
037300960222     C                   Z-ADD     FGVNFV        D22NFV
037400960222     C                   MOVEL     'F'           D22FFS
037500960222     C                   MOVEL     DSTL22        KPJBU
037600960222     C                   CALL      'TNTL22R'
037700960222     C                   PARM                    KPJBA
037800960222     C                   MOVEL     KPJBU         DSTL22
037900960222     C                   MOVEL     '1'           WTL22             1
038000960222     C*
038100960222     C* D22ERR <> " "  ---> TRAINO NON ABBINABILE: AZZERO I CAMPI DEL
038200960222     C*                     TRAINO SUL FILE FNFGV00F/FNFV200F
038300960222    1C     D22ERR        IFNE      ' '
038400051109     c* chain su fisico del foglio viaggio per eliminare il num traino
038500051109     c     fgvnrr        chain     fnfgv00f
038600960222     C*
038700960223     C* CLEARO CAMPI DI FNFGV00F
038800960223     C                   CLEAR                   FGVTRN
038900960223     C                   CLEAR                   FGVFRQ
039000051109     C                   UPDATE    FNFGVfis
039100960223     C*
039200960223     C* ANNULLO DATA/ORA TEORICA ARRIVO DAL FILE FNFV200F
039300960223     C                   MOVEL     'A'           WEPA
039400960223     C     KFV3          SETLL     FNFV203L
039500960223     C     KFV3          READE     FNFV203L                               32
039600960223    2C     *IN32         DOWEQ     *OFF
039700960223     C                   DELETE    FNFV2003
039800960223     C     KFV3          READE     FNFV203L                               32
039900960223    2C                   ENDDO
040000960223     C*
040100960223     C* ANNULLO DATA/ORA TEORICA PARTENZA DAL FILE FNFV200F
040200960223     C                   MOVEL     'P'           WEPA
040300960223     C                   CLEAR                   WLAI
040400960223     C     KFV2          CHAIN     FNFV201L                           32
040500960223     C  N32              DELETE    FNFV2000
040600960222     C*
040700960222   X1C                   ELSE
040800960223     C*
040900960223     C* CALCOLO LA DATA TEORICA DI ARRIVO DEL FOGLIO VIAGGIO E LA
041000960223     C*   SCRIVO SU FNFV200F
041100960223     C                   CLEAR                   DSTL35
041200960223     C                   MOVEL     'D'           D35TUP
041300960223     C                   MOVEL     'A'           D35EPA
041400960223     C                   MOVEL     FGVTRN        D35TRN
041500960223     C                   MOVEL     D22DDE        D35DDE
041600960223     C                   MOVEL     D22DTN        D35DTN
041700960223     C                   MOVEL     FV2LAI        D35FIL
041800960223     C                   MOVEL     FGVFRQ        D35TFM
041900960223     C                   MOVEL     DSTL35        KPJBU
042000960223     C                   CALL      'TNTL35R'
042100960223     C                   PARM                    KPJBA
042200960223     C                   MOVEL     KPJBU         DSTL35
042300960223     C                   MOVEL     '1'           WTL35             1
042400960223     C*
042500960223    2C     D35ERR        IFEQ      ' '
042600960223     C                   MOVEL     'A'           WEPA
042700960223     C                   MOVEL     FV2LAI        WLAI
042800960223     C     KFV2          CHAIN     FNFV201L                           32
042900960223     C*
043000960223     C                   CLEAR                   FV2ATB
043100960223     C                   CLEAR                   FV2RTC
043200960223     C                   CLEAR                   FV2ABN
043300960223     C                   MOVEL     'A'           FV2EPA
043400960223     C                   MOVEL     'T'           FV2TDH
043500960223     C                   MOVEL     D35DTE        FV2DPA
043600960223     C                   MOVEL     D35HTE        FV2HPA
043700960223     C                   MOVEL     'R'           FV2FTR
043800960223     C                   MOVEL     NUMTRA        FV2DTR
043900960223     C*
044000960223    3C     *IN32         IFEQ      *ON
044100960223     C                   WRITE     FNFV2000
044200960223   X3C                   ELSE
044300960223     C                   UPDATE    FNFV2000
044400960223    3C                   ENDIF
044500960223     C*
044600960223    2C                   ENDIF
044700960223    1C                   ENDIF
044800960222     C*
044900960222     C                   ENDSR
045000951205     C**-------------------------------------------------------------**
045100951205     C**  ROUTINE DI STAMPA                                          **
045200951205     C**-------------------------------------------------------------**
045300951205     C     STAMPA        BEGSR
045400951207     C                   SETOFF                                       05
045500960227     C                   CLEAR                   DESRTC
045600960227     C                   CLEAR                   WNOT
045700960131     C* EFFETTUATA STAMPA
045800960131     C                   SETON                                        99
045900951207     C*
046000951207     C* CONTROLLO SE PARTENZA O ARRIVO
046100951207    1C     *IN07         IFEQ      *OFF
046200951207     C     *IN08         ANDEQ     *OFF
046300951207    2C     FV2EPA        IFEQ      'P'
046400951207     C                   SETON                                        07
046500951207   X2C                   ELSE
046600951207     C                   SETON                                        08
046700951207    2C                   ENDIF
046800951207    1C                   ENDIF
046900951205     C*
047000951205    1C     *INOF         IFEQ      *ON
047100951205     C                   EXCEPT    TESTA
047200951205     C                   SETOFF                                       OF
047300951205    1C                   ENDIF
047400951205     C*
047500951206     C* DECODIFICA LINEA PARTENZA
047600000626     C  N75FV2LNP        CHAIN     AZORG01L                           35
047700000626     C   75LPTLNP        CHAIN     AZORG01L                           35
047800951206     C     *IN35         IFEQ      *OFF
047900951206     C                   MOVEL     ORGDES        DESLNP           15
048000951206     C                   ELSE
048100951206     C                   CLEAR                   DESLNP
048200951206     C                   ENDIF
048300951206     C* DECODIFICA LINEA INVIO
048400951206     C     FV2LAI        CHAIN     AZORG01L                           35
048500951206     C     *IN35         IFEQ      *OFF
048600951206     C                   MOVEL     ORGDES        DESLAI           15
048700951206     C                   ELSE
048800951206     C                   CLEAR                   DESLAI
048900951206     C                   ENDIF
049000951213     C*
049100951206     C* DECODIFICA CAUSALE RITARDO
049200951206    1C     FV2RTC        IFNE      ' '
049300951206     C                   SETON                                        05
049400951206     C                   MOVEL     'TC'          COD
049500951206     C                   MOVEL(P)  FV2RTC        KEY
049600951206     C     KTAB          CHAIN     TABEL                              35
049700951206    2C     *IN35         IFEQ      *OFF
049800951206     C     TBLFLG        ANDEQ     ' '
049900951206     C                   MOVEL     TBLUNI        DESRTC           20
050000951206   X2C                   ELSE
050100951206     C                   CLEAR                   DESRTC
050200951206    2C                   ENDIF
050300951213     C*
050400951213     C* ESTENSIONE DESCRIZIONE: STAMPO SOLO IL PRIMO RECORD CHE TROVO
050500951213     C                   MOVEL     'R'           WTRC
050600951213     C     KFV4L         CHAIN     FNFV401L                           35
050700951213    2C     *IN35         IFEQ      *OFF
050800951213     C                   MOVEL     FV4NOT        WNOT             17
050900951213   X2C                   ELSE
051000951213     C                   CLEAR                   WNOT
051100951213    2C                   ENDIF
051200951213    1C                   ENDIF
051300951205     C*
051400951205     C                   EXCEPT    DET
051500951205     C                   ENDSR
051600000626     C**-------------------------------------------------------------**
051700000626     C**  ROUTINE DI CARICO LPT PER LINEE POSTE                      **
051800000626     C**-------------------------------------------------------------**
051900000626     C     CARLPT        BEGSR
052000000720     C                   MOVEL     'P'           KEPA
052100000720     C                   MOVEL     FV2LNP        KTFE
052200000720     C                   CLEAR                   PT
052300000720     C*
052400000720     C     KCAE          SETLL     AZCAE05L
052500000720     C     KCAE          READE     AZCAE05L                               31
052600000720     C     *IN31         DOWEQ     *OFF
052700000720     C     CAEATB        IFEQ      ' '
052800000720     C     DATEU         ANDGE     CAEDDE
052900000720     C     DATEU         ANDLE     CAEDSC
053000000720     C                   ADD       1             PT
053100000720     C                   Z-ADD     CAETFP        LPT(PT)
053200000720     C                   ENDIF
053300000720     C     KCAE          READE     AZCAE05L                               31
053400000720     C                   ENDDO
053500000720     C*
053600000626     C                   ENDSR
053700951205     C*
053800951204     C*--- SR INIZIALE -----------------------------------------------*
053900951204     C     *INZSR        BEGSR
054000951204     C*
054100951204     C****  KLIST  ****
054200960131     C* ACCESSO FNFV401L
054300960131     C     KFV4L         KLIST
054400960131     C                   KFLD                    FV2LNP
054500960131     C                   KFLD                    FV2NFV
054600960131     C                   KFLD                    FV2EPA
054700960131     C                   KFLD                    FV2LAI
054800960131     C                   KFLD                    WTRC
054900951204     C* ACCESSO FNFV201L
055000951205     C     KFV2L         KLIST
055100951205     C                   KFLD                    FV2LNP
055200951205     C                   KFLD                    FV2NFV
055300951205     C                   KFLD                    FV2EPA
055400951205     C                   KFLD                    FV2LAI
055500960131     C                   KFLD                    FV2TDH
055600000626     C     KFV2L2        KLIST
055700000626     C                   KFLD                    LPTLNP
055800000626     C                   KFLD                    FV2NFV
055900000626     C                   KFLD                    FV2EPA
056000000626     C                   KFLD                    FV2LAI
056100000626     C                   KFLD                    FV2TDH
056200960223     C     KFV2          KLIST
056300960223     C                   KFLD                    FV2LNP
056400960223     C                   KFLD                    FV2NFV
056500960223     C                   KFLD                    WEPA
056600960223     C                   KFLD                    WLAI
056700960223     C                   KFLD                    WTDH
056800960223     C* ACCESSO FNFV203L
056900960223     C     KFV3          KLIST
057000960223     C                   KFLD                    FV2LNP
057100960223     C                   KFLD                    FV2NFV
057200960223     C                   KFLD                    WEPA
057300960223     C                   KFLD                    WTDH
057400951205     C* ACCESSO FNFV201R
057500951205     C     KFV2R         KLIST
057600951205     C                   KFLD                    FV2EPA
057700951205     C                   KFLD                    FV2LNP
057800951205     C                   KFLD                    FV2NFV
057900960131     C                   KFLD                    FV2LAI
058000960131     C                   KFLD                    FV2TDH
058100951204     C* ACCESSO TNFVS01L
058200951204     C     KFVS1         KLIST
058300951204     C                   KFLD                    FV2LNP
058400951204     C                   KFLD                    FV2NFV
058500000626     C     KFVS2         KLIST
058600000626     C                   KFLD                    LPTLNP            3 0
058700000626     C                   KFLD                    FV2NFV
058800960131     C     KFGV          KLIST
058900960131     C                   KFLD                    FV2NFV
059000960131     C                   KFLD                    FV2LNP
059100951204     C* ACCESSO TABEL00F
059200951204     C     KTAB          KLIST
059300951204     C                   KFLD                    CODUT
059400951204     C                   KFLD                    COD
059500951204     C                   KFLD                    KEY
059600000720     C     KCAE          KLIST
059700000720     C                   KFLD                    KEPA
059800000720     C                   KFLD                    KTFE
059900951205     C*
060000951205     C* ACCENDO INDICATORE DI OVERFLOW
060100951205     C                   SETON                                        OF
060200951207     C** NUMERO TRASMISSIONE = UDATE USA
060300951207     C** LA DATA DEL GIORNO LA PRENDO DA TIME
060400951207     C                   TIME                    W0140            14 0
060500951207     C                   MOVE      W0140         UDATE8            8 0
060600951207     C                   Z-ADD     UDATE8        DATA
060700951207     C                   Z-ADD     GG            MMGG              4 0
060800951207     C                   MOVEL     MM            MMGG
060900951207     C                   Z-ADD     MMGG          NUMTRA            8 0
061000951207     C                   MOVEL     AA            NUMTRA
061100951204     C*
061200951204     C****  DEFINIZIONE CAMPI  ****
061300951204     C     *LIKE         DEFINE    TBLCOD        COD
061400951204     C     *LIKE         DEFINE    TBLKEY        KEY
061500951204     C     *LIKE         DEFINE    FVSTTR        WTTR
061600951205     C     *LIKE         DEFINE    FV2DPA        WDPA
061700960131     C     *LIKE         DEFINE    FV4TRC        WTRC
061800960223     C     *LIKE         DEFINE    FV2TDH        WTDH
061900960223     C     *LIKE         DEFINE    FV2EPA        WEPA
062000960223     C     *LIKE         DEFINE    FV2LAI        WLAI
062100000720     C     *LIKE         DEFINE    CAEEPA        KEPA
062200000720     C     *LIKE         DEFINE    CAETFE        KTFE
062300000720     C*
062400000720     C                   TIME                    W0140            14 0
062500000720     C* UDATE IN GGMMAAAA
062600000720     C                   MOVE      W0140         WDTGIO            8 0
062700000720     C*
062800000720     C* UDATE IN AAAAMMGG
062900000720     C                   Z-ADD     WDTGIO        G08DAT
063000000720     C                   MOVEL     *BLANK        G08ERR
063100000720     C                   CALL      'XSRDA8'
063200000720     C                   PARM                    WLBDA8
063300000720     C                   MOVEL     G08INV        DATEU             8 0
063400951204     C                   ENDSR
063500951204     C*
063600951205     C*---------------------------------------------------------------*
063700951205     O*  T E S T A T A
063800951205     OQSYSPRT   E            TESTA            02
063900951205     O                       TE(1)               66
064000951205     O                       TE(2)              132
064100951205     O                       RSUT                20
064200951205     O                                           21 '/'
064300951205     O                       SIMFEL              24
064400951207     O                     07                    71 'PARTENZA'
064500951207     O                     08                    71 ' ARRIVO '
064600951205     O                       UDATE8             120 '  /  /    '
064700951205     O                       PAGE          Z    132
064800951205     O          E            TESTA            04
064900951205     O                       TE(3)               66
065000951205     O                       TE(6)              132
065100951207     O                     07                    74 'P A R T E N.'
065200951205     O          E            TESTA            05
065300951205     O                       TE(4)               66
065400951205     O                       TE(7)              132
065500951205     O*
065600951205     O*  D E T T A G L I O
065700951205     O          E            DET         1
065800000626     O              N75      FV2LNP               5
065900000626     O               75      LPTLNP               5
066000951206     O                                            7 '-'
066100951206     O                       DESLNP              23
066200951206     O                       FV2NFV        Z     32
066300951206     O                       FV2LAI              40
066400951206     O                                           42 '-'
066500951206     O                       DESLAI              58
066600951206     O                       WDPA                73 '  /  /    '
066700951213     O                       FV2HPA              83 '0 :  '
066800951206     O                       FV2RTC              90
066900951206     O                     05                    92 '-'
067000951206     O                       DESRTC             113
067100951213     O                       WNOT               132
067200960131     O          E            PIEDI       2
067300960131     O                                              ' '
067400960131     O          E            PIEDI       3
067500960131     O                       TE(9)              100
067600960131     O          E            PIEDI       0  0
067700960131     O                       TE(9)              100
067800951205     O*---------------------------------------------------------------*
067900960131**         CMD7
068000951115ALCOBJ OBJ((FNFV201R *FILE *EXCL           M234567890))
068100951115**         CMD8
068200951115OVRDBF FILE(FNFV201R)                   MBR(M234567890)
068300060607**         CMD9
068400060607CHKOBJ OBJ(FNFV201R) OBJTYPE(*FILE)     MBR(M         )
068500951205**         TE
068600960131XXXXXXXXXXXXXXXXXXXX               *** ELENCO DATE E ORA REALI XXX   1
068700960131XXXXX SENZA RELATIVO FV ***       TRTR42R   XX/XX/XXXX   PAG. XXXX   2
068800951206  LINEA PARTENZA           NUMERO    LINEA DI INVIO           A R    3
068900951206                           FOGLIO                               D    4
069000951206  XXX - XXXXXXXXXXXXXXX    XXXXX     XXX - XXXXXXXXXXXXXXX     XX/   5
069100951213R I V O   R E A L E    CAUSALE RITARDO           ESTENSIONE DESCR.   6
069200951206A T A       O R A                                                    7
069300951213XX/XXXX     XX:XX      X - XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXX   8
069400960131  **** MANDARE CON URGENZA IN SEDE ALL'UFFICIO CED !!!! ****         9
