000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200951207     H* FNLSA4R *-----------------------------------------------------*
000300000427     H*   TRASMISSIONE A SEDE DI FNFGV01T - FGW00T- FNFVD00T - FNFV400T
000400951207     H*---------------------------------------------------------------*
000500960125     FFNFGV46L  UF   E           K DISK
000600960122     FFNFV201L  UF   E           K DISK
000700980218     FFNFVD02L  UF   E           K DISK
000800021106     FFNFV403L  UF   E           K DISK
000900960919     FFNFGV01L  IF   E           K DISK
001000960919     F                                     RENAME(FNFGV000:FNFGV001)
001100000427     FFNFGW01L  IF   E           K DISK
001200960919     FTABEL00F  IF   E           K DISK
001300960122     FFNFGV01T  O  A E             DISK    USROPN
001400951207     F                                     RENAME(FNFGV000:FNFGV0TT)
001500951207     FFNFVD00T  O  A E             DISK    USROPN
001600951207     F                                     RENAME(FNFVD000:FNFVD0TT)
001700951207     FFNFV400T  O  A E             DISK    USROPN
001800951207     F                                     RENAME(FNFV4000:FNFV40TT)
001900000427     FFNFGW00T  O  A E             DISK    USROPN
002000000427     F                                     RENAME(FNFGW000:FNFGW0TT)
002100951207     F*
002200951207     D* DEFINIZIONE SCHIERE
002300951207     D C17             S              1    DIM(45) CTDATA PERRCD(45)
002400021008     D C56             S              1    DIM(42) CTDATA PERRCD(42)
002500021008     D C57             S              1    DIM(42) CTDATA PERRCD(42)
002600960201     D MSG             S             70    DIM(2) CTDATA PERRCD(1)
002700951207     D*
002800951207     D* PARAMETRI PASSATI DAI PGM CHIAMANTI
002900951207     D PARAM           DS
003000951207     D  PARMBD                 1     10
003100971113     D  SIMFEL                11     13  0
003200960129     D* IL CAMPO PARALL VIENE RESTITUITO IN OUTPUT NEL CASO NON SIANO
003300960129     D*   RIUSCITE LE ALLOCAZIONI DI TUTTI I FILES
003400951207     D  PARALL                14     14
003500960129     D* TIPO LANCIO PROGRAMMA ("C" / "L" / " ")
003600951207     D  PARTLA                15     15
003700960129     D* PARFLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
003800960129     D  PARFLG                16     16
003900971204     D* IL CAMPO PARLNP DEVE ESSERE PASSATO SOLO SE IL PGM E' RICHIAMAT
004000971204     D*   DALLA GESTIONE F.VIAGGIO
004100960129     D  PARLNP                17     19  0
004200951207     D                 DS
004300951207     D  GG                     1      2  0
004400951207     D  MM                     3      4  0
004500951207     D  AA                     5      8  0
004600951207     D  WDATA                  1      8  0
004700960129     D                 DS
004800111103     D  MSGDET                 1   5000
004900971204     D  MSLNP                 50     52
005000951207     D*
005100960919     D DSTV          E DS
005200951207     D KPJBA         E DS
005300951207     D DSCMZ         E DS
005400021008     D TIBS55DS      E DS
005500951207     C*****************************************************************
005600951207     C* RIEPILOGO INDICATORI
005700951207     C*****************************************************************
005800960919     C* 12    - IL FV E' DA TRASMETTERE IN SEDE QUINDI ANCHE I RELATIVI
005900960919     C*         RECORD DI FNFV4 SONO DA TRASMETTERE
006000960919     C* 30/33 - DI COMODO
006100951207     C* 40    - NON SONO RIUSCITA AD ALLOCARE IL MEMBRO
006200951207     C* 55/56 - ERRORI IN ALLOCAZIONE
006300960122     C* 59    - ERRORI IN ALLOCAZIONE
006400951207     C*****************************************************************
006500951207     C     *ENTRY        PLIST
006600951207     C                   PARM                    KPJBA
006700951207     C                   MOVEL     KPJBU         PARAM
006800951207     C                   MOVEL     PARMBD        CMZMBD
006900021010     C                   MOVEL     PARMBD        CMZMBa
007000021008     C                   MOVEL     PARMBD        VAR               9
007100021008     C                   MOVE      '))'          VAR
007200951213     C*
007300951213     C*****************************************************************
007400951207     C                   SETOFF                                       405556
007500960122     C                   SETOFF                                       59
007600051121     c
007700051121     c                   if        parflg<>'M'
007800951207     C                   CLEAR                   PARALL
007900051121     c                   endif
008000951207     C*
008100951207     C* TIPO LANCIO: PARTLA = "C" -           CHIUDO CON LR
008200951207     C*              PARTLA = "L" - ELABORO E CHIUDO CON LR
008300951207     C*              PARTLA = " " - ELABORO E CHIUDO IN  RETRN
008400960122    1C     PARTLA        IFNE      'C'
008500951207     C*
008600960122     C** ALLOCAZIONE FNFGV01T
008700021008     C                   Z-ADD     42            LUNG             15 5
008800960122     C                   MOVEA     'FNFGV01T'    C56(13)
008900021008     C                   MOVEA     VAR           C56(34)
009000960122     C                   MOVEL     *BLANKS       COMMAN           80
009100960122     C                   MOVEA     C56           COMMAN
009200951207     C                   CALL      'QCMDEXC'                            55
009300951207     C                   PARM                    COMMAN
009400951207     C                   PARM                    LUNG
009500951207     C*
009600951207     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
009700951207     C   55              SETON                                          40
009800951207     C*
009900960122     C** ALLOCAZIONE FNFVD00T
010000951207     C                   MOVEA     'FNFVD00T'    C56(13)
010100021008     C                   MOVEA     VAR           C56(34)
010200951207     C                   MOVEL     *BLANKS       COMMAN           80
010300951207     C                   MOVEA     C56           COMMAN
010400951207     C                   CALL      'QCMDEXC'                            56
010500951207     C                   PARM                    COMMAN
010600951207     C                   PARM                    LUNG
010700951207     C* GIA' ALLOCATO RITENTO PIU' TARDI
010800951207     C   56              SETON                                        40
010900960122     C*
011000960122     C** ALLOCAZIONE FNFV400T
011100960122     C                   MOVEA     'FNFV400T'    C56(13)
011200021008     C                   MOVEA     VAR           C56(34)
011300960122     C                   MOVEL     *BLANKS       COMMAN           80
011400960122     C                   MOVEA     C56           COMMAN
011500960122     C                   CALL      'QCMDEXC'                            59
011600960122     C                   PARM                    COMMAN
011700960122     C                   PARM                    LUNG
011800960122     C* GIA' ALLOCATO RITENTO PIU' TARDI
011900960122     C   59              SETON                                        40
012000000427     C**
012100000427     C** ALLOCAZIONE FNFGW00T
012200021008     C                   Z-ADD     42            LUNG             15 5
012300000427     C                   MOVEA     'FNFGW00T'    C56(13)
012400021008     C                   MOVEA     VAR           C56(34)
012500000427     C                   MOVEL     *BLANKS       COMMAN           80
012600000427     C                   MOVEA     C56           COMMAN
012700000427     C                   CALL      'QCMDEXC'                            57
012800000427     C                   PARM                    COMMAN
012900000427     C                   PARM                    LUNG
013000000427     C*
013100000427     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
013200000427     C   57              SETON                                          40
013300951207     C*
013400980218     C* PROSEGUO SOLO SE E' RIUSCITA ALMENO UNA ALLOCAZIONE
013500960124    2C     *IN55         IFEQ      *OFF
013600000427     C     *IN57         ANDEQ     *OFF
013700980218     C     *IN56         OREQ      *OFF
013800960124     C     *IN59         OREQ      *OFF
013900951207     C*
014000960122     C* S C R I V O   M E M B R I   D I   S E D E
014100951207     C                   EXSR      WTRFV
014200951207     C*
014300960122     C* VEDO SE CONTIENE RECORD FNFGV01T
014400960122     C                   MOVEL     'FNFGV01T'    CMZFLD
014500951207     C                   EXSR      NUMREC
014600960122     C                   Z-ADD     NRR           RECFGV           10 0
014700000427     C* VEDO SE CONTIENE RECORD FNFGW00T
014800000427     C                   MOVEL     'FNFGW00T'    CMZFLD
014900000427     C                   EXSR      NUMREC
015000000427     C                   Z-ADD     NRR           RECFGW           10 0
015100951207     C* VEDO SE CONTIENE RECORD FNFVD00T
015200951207     C                   MOVEL     'FNFVD00T'    CMZFLD
015300951207     C                   EXSR      NUMREC
015400951207     C                   Z-ADD     NRR           RECFVD           10 0
015500951207     C* VEDO SE CONTIENE RECORD FNFV400T
015600951207     C                   MOVEL     'FNFV400T'    CMZFLD
015700951207     C                   EXSR      NUMREC
015800951207     C                   Z-ADD     NRR           RECFV4           10 0
015900951207     C*
016000951207     C* I N V I O
016100960122    3C     RECFGV        IFGT      0
016200980218     C     RECFGV        ANDNE     *HIVAL
016300980218     C     RECFVD        ORGT      0
016400980218     C     RECFGV        ANDNE     *HIVAL
016500951207     C     RECFV4        ORGT      0
016600980218     C     RECFGV        ANDNE     *HIVAL
016700951207     C                   MOVE      '046'         CMZDST
016800951207     C                   MOVE      'S'           CMZURG
016900951207     C                   MOVE      *BLANK        CMZPRR
017000951207     C                   MOVE      *BLANK        CMZAZN
017100951207     C*
017200951207     C                   EXSR      X25FV4
017300951207     C                   EXSR      X25FVD
017400000427     C                   EXSR      X25FGW
017500000427     C                   EXSR      X25FGV
017600960122    3C                   ENDIF
017700960125    2C                   ENDIF
017800960122     C*
017900960122     C** DISALLOCO FNFV400T
018000021008     C                   Z-ADD     42            LUNG
018100960125    2C     *IN59         IFEQ      *OFF
018200960122     C                   MOVEA     'FNFV400T'    C57(13)
018300021008     C                   MOVEA     VAR           C57(34)
018400960122     C                   MOVEL     *BLANKS       COMMAN           80
018500960122     C                   MOVEA     C57           COMMAN
018600980218     C                   CALL      'QCMDEXC'                            30
018700960122     C                   PARM                    COMMAN
018800960122     C                   PARM                    LUNG
018900960125    2C                   ENDIF
019000960125     C*
019100980218     C** DISALLOCO FNFVD00T
019200980218    2C     *IN56         IFEQ      *OFF
019300980218     C                   MOVEA     'FNFVD00T'    C57(13)
019400021008     C                   MOVEA     VAR           C57(34)
019500980218     C                   MOVEL     *BLANKS       COMMAN           80
019600980218     C                   MOVEA     C57           COMMAN
019700980218     C                   CALL      'QCMDEXC'                            30
019800980218     C                   PARM                    COMMAN
019900980218     C                   PARM                    LUNG
020000980218    2C                   ENDIF
020100021008     C** DISALLOCO FNFGW00T
020200021008    2C     *IN57         IFEQ      *OFF
020300021008     C                   MOVEA     'FNFGW00T'    C57(13)
020400021008     C                   MOVEA     VAR           C57(34)
020500021008     C                   MOVEL     *BLANKS       COMMAN           80
020600021008     C                   MOVEA     C57           COMMAN
020700021008     C                   CALL      'QCMDEXC'                            30
020800021008     C                   PARM                    COMMAN
020900021008     C                   PARM                    LUNG
021000021008    2C                   ENDIF
021100980218     C** DISALLOCO FNFGV01T
021200960125    2C     *IN55         IFEQ      *OFF
021300960122     C                   MOVEA     'FNFGV01T'    C57(13)
021400021008     C                   MOVEA     VAR           C57(34)
021500960122     C                   MOVEL     *BLANKS       COMMAN           80
021600960122     C                   MOVEA     C57           COMMAN
021700980218     C                   CALL      'QCMDEXC'                            30
021800951207     C                   PARM                    COMMAN
021900951207     C                   PARM                    LUNG
022000960125    2C                   ENDIF
022100960122     C*
022200960125     C* 40 ON  - RITENTARE PIU' TARDI PERCHE' NON E' RIUSCITA
022300960125     C*   L'ALLOCAZIONE DI TUTTI I MEMBRI
022400960125    2C     *IN40         IFEQ      *ON
022500960129     C*
022600960129     C* SE RICHIAMATO DA GESTIONE F.VIAGGIO E NON SONO AVVENUTE LE
022700960129     C*   ALLOCAZIONI DI TUTTI I FILES INVIO MESSAGGIO IN SEDE
022800051121    3c     PARFLG        IFEQ      'M'
022900051121     c* Tento 3 volte e poi do il messaggio
023000051121     C                   select
023100051121    4c                   when      parall=' '
023200051121     c                   eval      parall='1'
023300051121     C                   MOVEL     PARAM         KPJBU
023400051121     C                   MOVEL     'LSA4'        KCOAZ
023500051121     C                   CALL      'BCH10'
023600051121     C                   PARM                    KPJBA
023700051121     c
023800051121     c                   when      parall='1'
023900051121     c                   eval      parall='2'
024000051121     C                   MOVEL     PARAM         KPJBU
024100051121     C                   MOVEL     'LSA4'        KCOAZ
024200051121     C                   CALL      'BCH10'
024300051121     C                   PARM                    KPJBA
024400051121     c
024500051121     c                   when      parall='2'
024600051121     c                   eval      parall='3'
024700051121     C                   MOVEL     PARAM         KPJBU
024800051121     C                   MOVEL     'LSA4'        KCOAZ
024900051121     C                   CALL      'BCH10'
025000051121     C                   PARM                    KPJBA
025100051121     c
025200051121     c                   when      parall='3'
025300111103     C                   movel     *blanks       msgtes
025400111103     C                   movel     *blanks       msgdet
025500960129     C                   MOVEL     MSG(1)        MSGTES
025600960201     C                   MOVEL(P)  MSG(2)        MSGDET
025700960129     C                   MOVE      PARLNP        MSLNP
025800111103     C                   eval      wrkEml= 'ced@bartolini.it'
025900111103
026000111103     C                   call(e)   'TIS701C'
026100111103     C                   parm                    wrkEml          253
026200111103     C                   PARM                    MSGTES           44
026300111103     C                   PARM                    MSGDET
026400111103     c
026500051121     c                   other
026600051121     C                   MOVEL     '1'           PARALL
026700051121     C                   MOVEL     PARAM         KPJBU
026800051121    4C                   ENDsl
026900051121    3C                   ENDIF
027000960122    2C                   ENDIF
027100960122    1C                   ENDIF
027200951207     C*
027300951207     C* CHIUSURA PGM
027400960122    1C     PARTLA        IFEQ      ' '
027500951207     C                   RETURN
027600960122   X1C                   ELSE
027700951207     C                   SETON                                        LR
027800960122    1C                   ENDIF
027900951207     C*
028000951207     C**************************************************************************
028100951207     C*  SCRIVO MEMBRO FILE FOGLI VIAGGIO PER LA SEDE
028200951207     C**************************************************************************
028300951207     C     WTRFV         BEGSR
028400951207     C*
028500951207     C* APERTURA MEMBRI
028600951207     C                   EXSR      CAEXFV
028700951207     C*
028800960125    1C     *IN55         IFEQ      *OFF
028900000427    1C     *IN57         ANDEQ     *OFF
029000960125     C*
029100960122     C** SCRITTURA FNFGV01T
029200021009     C     SIMFEL        SETLL     FNFGV46L
029300021009     C     SIMFEL        READE     FNFGV46L                               31
029400000427     C*
029500960125    2C     *IN31         DOWEQ     *OFF
029600000427     C* ESTENSIONE FOGLI
029700021009     C     KFGW          CHAIN     FNFGW01L                           35
029800000427     C**
029900960125     C* SE PRESENTE REPERISCO LA CAUSALE RITARDO PARTENZA
030000960125     C                   CLEAR                   FGVRTP
030100960122     C     KFV2          CHAIN     FNFV201L                           30
030200960125    3C     *IN30         IFEQ      *OFF
030300960122     C                   MOVEL     FV2RTC        FGVRTP
030400960122     C                   MOVEL     'T'           FV2FTR
030500960122     C                   Z-ADD     NUMTRA        FV2DTR
030600960122     C                   UPDATE    FNFV2000
030700960125    3C                   ENDIF
030800960919     C*
030900951207     C                   WRITE     FNFGV0TT
031000000427     C  N35              WRITE     FNFGW0TT
031100000427     C*
031200951207     C                   DELETE    FNFGV000
031300951207     C*
031400021009     C     SIMFEL        READE     FNFGV46L                               31
031500960125    2C                   ENDDO
031600980218     C* CHIUSURA MEMBRO
031700960125     C                   CLOSE     FNFGV01T
031800000427     C                   CLOSE     FNFGW00T
031900960125    1C                   ENDIF
032000980218     C*
032100980218    1C     *IN56         IFEQ      *OFF
032200980218     C** SCRITTURA DI FNFVD00T
032300100316     c* Elaboro per terminal per evitare problemi in trasmissione
032400100316     C     kfvd          SETLL     FNFVD02L
032500100316     C     kfvd          READE     FNFVD02L                               32
032600980218    3C     *IN32         DOWEQ     *OFF
032700980218     C                   WRITE     FNFVD0TT
032800980218     C                   MOVEL     'T'           FVDFTR
032900980218     C                   Z-ADD     NUMTRA        FVDDTR
033000980218     C                   UPDATE    FNFVD000
033100100316     C     kfvd          READE     FNFVD02L                               32
033200980218    3C                   ENDDO
033300980218     C                   CLOSE     FNFVD00T
033400980218    1C                   ENDIF
033500960122     C*
033600960122     C** SCRITTURA FNFV400T
033700960125    1C     *IN59         IFEQ      *OFF
033800960125     C*
033900021106     C* Elaboro solo i record con "P-PARTENZA",
034000021106     C*                           Linea di Invio F.V. = 0
034100021106     C*                           Linea di Partenza = simfel
034200021106     C     K03FV403      SETLL     FNFV403L
034300021106     C     K03FV403      READE     FNFV403L                               31
034400960125    2C     *IN31         DOWEQ     *OFF
034500021106     C* Trasmetto solo i record con Flag Trasmissione a Sede vuoto
034600021106    3C     FV4FT2        IFEQ      ' '
034700960125     C*
034800960919     C* AGGANCIO IL FOGLIO VIAGGIO PER VEDERE SE I RECORD DI FNFV4
034900960919     C*   APPARTENGONO A UN FV DA TRASMETTERE IN SEDE
035000960919     C                   EXSR      CTRFGV
035100960919     C*
035200960919     C* 12 ON - IL FV E' DA TRASMETTERE IN SEDE QUINDI ANCHE I RELATIVI
035300960919     C*  RECORD DI FNFV4 SONO DA TRASMETTERE
035400960919    4C     *IN12         IFEQ      *ON
035500960125     C* IMPOSTO DATA E ORA VARIAZIONE CON UDATE E UTIME
035600960125     C                   Z-ADD     NUMTRA        FV4DTV
035700960125     C                   Z-ADD     UTIME         FV4ORV
035800960122     C                   WRITE     FNFV40TT
035900960122     C*
036000960125     C                   MOVEL     'T'           FV4FT2
036100960125     C                   Z-ADD     NUMTRA        FV4DT2
036200960122     C                   UPDATE    FNFV4000
036300960919    4C                   ENDIF
036400960125    3C                   ENDIF
036500960122     C*
036600021106     C     K03FV403      READE     FNFV403L                               31
036700960125    2C                   ENDDO
036800960125     C*
036900960125     C* CHIUSURA MEMBRI
037000960125     C                   CLOSE     FNFV400T
037100960125    1C                   ENDIF
037200951207     C*
037300951207     C                   ENDSR
037400951207     C**************************************************************************
037500951207     C*  ESEGUE CALL QCAEXEC PER OVRDBF SUI FILE
037600951207     C**************************************************************************
037700951207     C     CAEXFV        BEGSR
037800021008     C                   MOVEL     PARMBD        VAR8
037900021008     C                   MOVE      ')'           VAR8              8
038000951207     C                   Z-ADD     46            LUNG
038100951207     C*
038200980218     C** OVRDBF FNFGV01T
038300960125    1C     *IN55         IFEQ      *OFF
038400000427     C     *IN57         ANDEQ     *OFF
038500960122     C                   MOVEA     'FNFGV01T'    C17(13)
038600021008     C                   MOVEA     VAR8          C17(38)
038700951207     C                   MOVEL     *BLANKS       COMMAN
038800951207     C                   MOVEA     C17           COMMAN
038900951207     C                   CALL      'QCMDEXC'
039000951207     C                   PARM                    COMMAN
039100951207     C                   PARM                    LUNG
039200980218     C* APERTURA FILE
039300980218     C                   OPEN      FNFGV01T
039400000427     C** OVRDBF FNFGV01T
039500000427     C                   MOVEA     'FNFGW00T'    C17(13)
039600021008     C                   MOVEA     VAR8          C17(38)
039700000427     C                   MOVEL     *BLANKS       COMMAN
039800000427     C                   MOVEA     C17           COMMAN
039900000427     C                   CALL      'QCMDEXC'
040000000427     C                   PARM                    COMMAN
040100000427     C                   PARM                    LUNG
040200000427     C* APERTURA FILE
040300000427     C                   OPEN      FNFGW00T
040400980218    1C                   ENDIF
040500951207     C*
040600960122     C** OVRDBF FNFVD00T
040700980218     C     *IN56         IFEQ      *OFF
040800951207     C                   MOVEA     'FNFVD00T'    C17(13)
040900021008     C                   MOVEA     VAR8          C17(38)
041000951207     C                   MOVEL     *BLANKS       COMMAN
041100951207     C                   MOVEA     C17           COMMAN
041200951207     C                   CALL      'QCMDEXC'
041300951207     C                   PARM                    COMMAN
041400951207     C                   PARM                    LUNG
041500980218     C* APERTURA FILE
041600980218     C                   OPEN      FNFVD00T
041700980218    1C                   ENDIF
041800960125     C*
041900960125     C*
042000960122     C** OVRDBF FNFV400T
042100960125    1C     *IN59         IFEQ      *OFF
042200960122     C                   MOVEA     'FNFV400T'    C17(13)
042300021008     C                   MOVEA     VAR8          C17(38)
042400960122     C                   MOVEL     *BLANKS       COMMAN
042500960122     C                   MOVEA     C17           COMMAN
042600960122     C                   CALL      'QCMDEXC'
042700960122     C                   PARM                    COMMAN
042800960122     C                   PARM                    LUNG
042900951207     C*
043000951207     C* APERTURA FILE
043100951207     C                   OPEN      FNFV400T
043200960125    1C                   ENDIF
043300951207     C*
043400951207     C                   ENDSR
043500960919     C**************************************************************************
043600960919     C*  CONTROLLO SE FV DA TRASMETTERE IN SEDE
043700960919     C**************************************************************************
043800960919     C     CTRFGV        BEGSR
043900960919     C*
044000960919     C* AGGANCIO IL FOGLIO VIAGGIO SOLO QUANDO CAMBIA
044100960919    0C     WNFV          IFNE      FV4NFV
044200960919     C                   SETOFF                                       12
044300960919     C*
044400960919     C     KFGV          CHAIN     FNFGV01L                           32
044500960919    1C     *IN32         IFEQ      *OFF
044600960919     C*
044700960919     C* SOLO A CAMBIO TIPO TRAINO CONTROLLO SE IL FV E' DA TRASMETTERE
044800960919     C*   IN SEDE
044900960919    2C     WTTR          IFNE      FGVTTR
045000960919     C                   MOVEL(P)  FGVTTR        KEY
045100960919     C     KTAB          CHAIN     TABEL                              33
045200960919    3C     *IN33         IFEQ      *OFF
045300960919     C     TBLFLG        ANDEQ     ' '
045400960919     C                   MOVEL     TBLUNI        DSTV
045500960919   X3C                   ELSE
045600960919     C                   MOVEL     'S'           §TVUTC
045700960919    3C                   ENDIF
045800960919     C                   MOVEL     FGVTTR        WTTR
045900960919    2C                   ENDIF
046000960919     C*
046100960919     C* IL FOGLIO VIAGGIO E' DA INVIARE IN SEDE SE:
046200960919     C*   - IL FOGLIO NON E' ABBINATO
046300960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
046400960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
046500960919     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTC = "S")
046600960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
046700960919     C*     C'E' IL RECORD IN TABELLA
046800960919    2C     FGVTRN        IFEQ      0
046900960919     C     FGVTTR        OREQ      ' '
047000960919     C     §TVUTC        OREQ      'S'
047100960919     C                   SETON                                        12
047200960919    2C                   ENDIF
047300960919     C*
047400960919    1C                   ENDIF
047500960919     C                   MOVEL     FV4NFV        WNFV
047600960919    0C                   ENDIF
047700960919     C*
047800960919     C                   ENDSR
047900960122     C**************************************************************************
048000960122     C*  SOTTOMETTE TRASMISSIONE   F N F V 4 0 0 T
048100960122     C**************************************************************************
048200960122     C     X25FV4        BEGSR
048300960122     C*
048400960122     C     RECFV4        IFGT      0
048500980218     C     RECFV4        ANDNE     *HIVAL
048600960122     C                   MOVEL     'FNFV400T'    CMZFLD
048700960122     C                   MOVEL     'FNFV400R'    CMZFLA
048800960129     C                   MOVE      'TR41'        CMZAZN
048900960122     C                   MOVE      RECFV4        CMZNRR
049000960122     C*
049100960122     C                   EXSR      CALCMZ
049200960122     C                   ENDIF
049300960122     C*
049400960122     C                   ENDSR
049500951207     C**************************************************************************
049600951207     C*  SOTTOMETTE TRASMISSIONE   F N F V D 0 0 T
049700951207     C**************************************************************************
049800951207     C     X25FVD        BEGSR
049900951207     C*
050000980218     C     RECFVD        IFGT      0
050100980218     C     RECFVD        ANDNE     *HIVAL
050200980218     C     RECFGV        IFEQ      0
050300980218     C     RECFGV        OREQ      *HIVAL
050400980218     C                   MOVEL     'TR40'        CMZAZN
050500980218     C                   ELSE
050600960212     C                   MOVE      '    '        CMZAZN
050700980218     C                   END
050800951207     C                   MOVEL     'FNFVD00T'    CMZFLD
050900951207     C                   MOVEL     'FNFVD00R'    CMZFLA
051000951207     C                   MOVE      RECFVD        CMZNRR
051100951207     C*
051200951207     C                   EXSR      CALCMZ
051300951207     C                   ENDIF
051400951207     C*
051500951207     C                   ENDSR
051600951207     C**************************************************************************
051700960130     C*  SOTTOMETTE TRASMISSIONE   F N F G V 0 1 T
051800951207     C**************************************************************************
051900960122     C     X25FGV        BEGSR
052000951207     C*
052100960122     C     RECFGV        IFGT      0
052200980218     C     RECFGV        ANDNE     *HIVAL
052300960122     C                   MOVEL     'FNFGV01T'    CMZFLD
052400960122     C                   MOVEL     'FNFGV01R'    CMZFLA
052500960130     C                   MOVE      'TR40'        CMZAZN
052600960122     C                   MOVE      RECFGV        CMZNRR
052700951207     C*
052800951207     C                   EXSR      CALCMZ
052900951207     C                   ENDIF
053000951207     C*
053100951207     C                   ENDSR
053200000427     C**************************************************************************
053300000427     C*  SOTTOMETTE TRASMISSIONE   F N F G W 0 0 T
053400000427     C**************************************************************************
053500000427     C     X25FGW        BEGSR
053600000427     C*
053700000427     C     RECFGW        IFGT      0
053800000427     C     RECFGW        ANDNE     *HIVAL
053900000427     C                   MOVEL     'FNFGW00T'    CMZFLD
054000000427     C                   MOVEL     'FNFGW00R'    CMZFLA
054100000427     C                   MOVE      '    '        CMZAZN
054200000428     C                   MOVE      RECFGW        CMZNRR
054300000427     C*
054400000427     C                   EXSR      CALCMZ
054500000427     C                   ENDIF
054600000427     C*
054700000427     C                   ENDSR
054800951207     C**************************************************************************
054900951207     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
055000951207     C**************************************************************************
055100951207     C     NUMREC        BEGSR
055200951207     C*
055300951207     C                   Z-ADD     0             NRR
055400951207     C                   CALL      'TRUL51C'
055500951207     C                   PARM                    KNSIF
055600951207     C                   PARM                    CMZFLD           10
055700951207     C                   PARM                    PARMBD
055800951207     C                   PARM                    NRR              10 0
055900951207     C*
056000951207     C                   ENDSR
056100951207     C**************************************************************************
056200960122     C*  CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSIONE
056300951207     C**************************************************************************
056400951207     C     CALCMZ        BEGSR
056500951207     C*
056600000427     C                   CLEAR                   W236
056700021010     C****               CLEAR                   CMZLCL
056800021008     C                   CALL      'TIBS53C'
056900000427     C                   PARM                    CMZLCL            1
057000951207     C                   PARM                    CMZDST
057100951207     C                   PARM                    CMZFLD
057200951207     C                   PARM                    CMZMBD
057300951207     C                   PARM                    CMZLBD
057400951207     C                   PARM                    CMZFLA
057500951207     C                   PARM                    CMZMBA
057600951207     C                   PARM                    CMZLBA
057700951207     C                   PARM                    CMZORI
057800951207     C                   PARM                    CMZDTI
057900951207     C                   PARM                    CMZURG
058000951207     C                   PARM                    CMZADD
058100951207     C                   PARM                    CMZCLR
058200951207     C                   PARM                    CMZFTR
058300951207     C                   PARM                    CMZERR
058400951207     C                   PARM                    CMZAZN
058500951207     C                   PARM                    CMZSYF
058600951207     C                   PARM                    CMZNRR
058700951207     C                   PARM                    CMZPRR
058800000427     C                   PARM                    W236            236
058900951207     C                   ENDSR
059000951207     C**************************************************************************
059100951207     C*  SR INIZIALE
059200951207     C**************************************************************************
059300951207     C     *INZSR        BEGSR
059400951213     C*
059500971117     C                   MOVEL     KPJBU         PARAM
059600971117     C*
059700951213     C****  IMPOSTAZIONE CAMPI FISSI  ****
059800021010     C****               MOVEL     'M'           ALF4              4
059900021010     C****               MOVE      SIMFEL        ALF4
060000021010     C****               MOVEL     ALF4          CMZMBA
060100971113     C*  ESEGUO CALL PER REPERIRE LIBRERIA E S.I. DI TRAMSISSIONE
060200021008     C                   CLEAR                   TIBS55DS
060300971125     C                   MOVEL     'L'           I50TLA
060400021010     C                   MOVEL     simfel        I50Ppo
060500971113     C                   MOVEL     046           I50APO
060600021008     C                   CALL      'TIBS55R'
060700021008     C                   PARM                    TIBS55DS
060800971113     C**
060900021010     C                   MOVEL     O50lcl        CMZLcl
061000021010     C                   MOVEL     O50PLA        CMZLBD
061100971113     C                   MOVEL     O50ALA        CMZLBA
061200971113     C                   MOVEL     O50ASI        CMZSYF
061300951213     C                   MOVE      '0000'        CMZORI
061400951213     C                   MOVE      '0000000'     CMZDTI
061500951213     C                   MOVE      'A'           CMZADD
061600951213     C                   MOVE      'S'           CMZCLR
061700951213     C                   MOVE      'S'           CMZFTR
061800951213     C                   MOVE      *BLANK        CMZERR
061900960122     C* IMPOSTO CAMPI PER AGGANCIARE FNFV201L
062000960122     C                   MOVEL     'P'           WEPA
062100960122     C                   MOVEL     'R'           WTDH
062200021106     C                   clear                   Wzero
062300960919     C* IMPOSTO CAMPI PER AGGANCIARE TABEL00F
062400960919     C                   Z-ADD     1             CODUT
062500960919     C                   MOVEL     'TV'          COD
062600951213     C*
062700951213     C****  CONTROLLO ESISTENZA MEMBRI  ****
062800960122     C                   MOVEL     'FNFGV01T'    FILE
062900951213     C                   CALL      'TRUL50C'
063000951213     C                   PARM                    FILE
063100971114     C                   PARM                    PARMBD
063200951213     C                   PARM                    KNSIF
063300951213     C                   PARM                    LOGICO           10
063400951213     C                   PARM                    LOGMBR           10
063500951213     C                   PARM                    FLG               1
063600951213     C*
063700000428     C                   MOVEL     'FNFGW00T'    FILE
063800000428     C                   CALL      'TRUL50C'
063900000428     C                   PARM                    FILE
064000000428     C                   PARM                    PARMBD
064100000428     C                   PARM                    KNSIF
064200000428     C                   PARM                    LOGICO           10
064300000428     C                   PARM                    LOGMBR           10
064400000428     C                   PARM                    FLG               1
064500000428     C*
064600951213     C                   MOVEL     'FNFVD00T'    FILE
064700951213     C                   CALL      'TRUL50C'
064800951213     C                   PARM                    FILE             10
064900971114     C                   PARM                    PARMBD
065000951213     C                   PARM                    KNSIF
065100951213     C                   PARM                    LOGICO           10
065200951213     C                   PARM                    LOGMBR           10
065300951213     C                   PARM                    FLG               1
065400951213     C*
065500951213     C                   MOVEL     'FNFV400T'    FILE
065600951213     C                   CALL      'TRUL50C'
065700951213     C                   PARM                    FILE
065800971114     C                   PARM                    PARMBD
065900951213     C                   PARM                    KNSIF
066000951213     C                   PARM                    LOGICO           10
066100951213     C                   PARM                    LOGMBR           10
066200951213     C                   PARM                    FLG               1
066300951207     C*
066400951207     C* NUMERO TRASMISSIONE = DATA DEL GIORNO (AAAA/MM/GG)
066500951207     C                   TIME                    WTIME            14 0
066600960125     C                   MOVEL     WTIME         UTIME             6 0
066700951207     C                   MOVE      WTIME         UDATE8            8 0
066800951207     C                   Z-ADD     UDATE8        WDATA
066900951207     C                   Z-ADD     GG            MMGG              4 0
067000951207     C                   MOVEL     MM            MMGG
067100951207     C                   Z-ADD     MMGG          NUMTRA            8 0
067200951207     C                   MOVEL     AA            NUMTRA
067300960122     C*
067400960122     C****  KLIST  ****
067500960122     C* ACCESSO FNFV201L
067600960122     C     KFV2          KLIST
067700960122     C                   KFLD                    FGVLNP
067800960122     C                   KFLD                    FGVNFV
067900960122     C                   KFLD                    WEPA
068000960122     C                   KFLD                    WZERO             3 0
068100960122     C                   KFLD                    WTDH
068200960919     C* ACCESSO FNFGV01L
068300960919     C     KFGV          KLIST
068400960919     C                   KFLD                    FV4NFV
068500960919     C                   KFLD                    FV4LNP
068600021009     C     KFGW          KLIST
068700021009     C                   KFLD                    FGVNFV
068800021009     C                   KFLD                    FGVLNP
068900100316     C     KFVD          KLIST
069000100316     C                   KFLD                    kftr
069100100316     C                   KFLD                    simfel
069200100316     c                   clear                   kftr              1
069300021106     C* Accesso FNFV403L
069400021106     C     K03FV403      klist
069500021106     C                   kfld                    Wepa
069600021106     C                   kfld                    Wzero
069700021106     C                   kfld                    SIMfel
069800960919     C* ACCESSO TABEL00F
069900960919     C     KTAB          KLIST
070000960919     C                   KFLD                    CODUT
070100960919     C                   KFLD                    COD
070200960919     C                   KFLD                    KEY
070300960122     C*
070400960122     C****  DEFINIZIONE CAMPI  ****
070500960122     C     *LIKE         DEFINE    FV2EPA        WEPA
070600960122     C     *LIKE         DEFINE    FV2TDH        WTDH
070700960919     C     *LIKE         DEFINE    TBLKUT        CODUT
070800960919     C     *LIKE         DEFINE    TBLCOD        COD
070900960919     C     *LIKE         DEFINE    TBLKEY        KEY
071000960919     C     *LIKE         DEFINE    FGVTTR        WTTR
071100960919     C     *LIKE         DEFINE    FV4NFV        WNFV
071200951207     C                   ENDSR
071300951207     C**************************************************************************
071400971204**         C17
071500021008OVRDBF FILE(XXXXXXXX)            MBR(M000000)
071600951207**         C56
071700021008ALCOBJ OBJ((XXXXXXXX *FILE *EXCL MXXXyyy))
071800951207**         C57
071900021008DLCOBJ OBJ((XXXXXXXX *FILE *EXCL M000000))
072000960129** MSG
072100111103FNLSA4R:ERRORE TRASMISSIONE F.VIAGGIO PART
072200971204A SEDE: NON TRASMESSO FOGLIO VIAGGIO DALLA LINEA XXX
