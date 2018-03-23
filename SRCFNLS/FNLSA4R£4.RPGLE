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
001200121102     FFNFGs01L  IF   E           K DISK
001300960919     FTABEL00F  IF   E           K DISK
001400960122     FFNFGV01T  O  A E             DISK    USROPN
001500951207     F                                     RENAME(FNFGV000:FNFGV0TT)
001600951207     FFNFVD00T  O  A E             DISK    USROPN
001700951207     F                                     RENAME(FNFVD000:FNFVD0TT)
001800951207     FFNFV400T  O  A E             DISK    USROPN
001900951207     F                                     RENAME(FNFV4000:FNFV40TT)
002000000427     FFNFGW00T  O  A E             DISK    USROPN
002100000427     F                                     RENAME(FNFGW000:FNFGW0TT)
002200121102     FFNFGs00T  O  A E             DISK    USROPN
002300121102     F                                     RENAME(FNFGs000:FNFGs0TT)
002400951207     F*
002500951207     D* DEFINIZIONE SCHIERE
002600951207     D C17             S              1    DIM(45) CTDATA PERRCD(45)
002700021008     D C56             S              1    DIM(42) CTDATA PERRCD(42)
002800021008     D C57             S              1    DIM(42) CTDATA PERRCD(42)
002900960201     D MSG             S             70    DIM(2) CTDATA PERRCD(1)
003000951207     D*
003100951207     D* PARAMETRI PASSATI DAI PGM CHIAMANTI
003200951207     D PARAM           DS
003300951207     D  PARMBD                 1     10
003400971113     D  SIMFEL                11     13  0
003500960129     D* IL CAMPO PARALL VIENE RESTITUITO IN OUTPUT NEL CASO NON SIANO
003600960129     D*   RIUSCITE LE ALLOCAZIONI DI TUTTI I FILES
003700951207     D  PARALL                14     14
003800960129     D* TIPO LANCIO PROGRAMMA ("C" / "L" / " ")
003900951207     D  PARTLA                15     15
004000960129     D* PARFLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
004100960129     D  PARFLG                16     16
004200971204     D* IL CAMPO PARLNP DEVE ESSERE PASSATO SOLO SE IL PGM E' RICHIAMAT
004300971204     D*   DALLA GESTIONE F.VIAGGIO
004400960129     D  PARLNP                17     19  0
004500951207     D                 DS
004600951207     D  GG                     1      2  0
004700951207     D  MM                     3      4  0
004800951207     D  AA                     5      8  0
004900951207     D  WDATA                  1      8  0
005000960129     D                 DS
005100111103     D  MSGDET                 1   5000
005200971204     D  MSLNP                 50     52
005300951207     D*
005400960919     D DSTV          E DS
005500951207     D KPJBA         E DS
005600951207     D DSCMZ         E DS
005700021008     D TIBS55DS      E DS
005800951207     C*****************************************************************
005900951207     C* RIEPILOGO INDICATORI
006000951207     C*****************************************************************
006100960919     C* 12    - IL FV E' DA TRASMETTERE IN SEDE QUINDI ANCHE I RELATIVI
006200960919     C*         RECORD DI FNFV4 SONO DA TRASMETTERE
006300960919     C* 30/33 - DI COMODO
006400951207     C* 40    - NON SONO RIUSCITA AD ALLOCARE IL MEMBRO
006500951207     C* 55/56 - ERRORI IN ALLOCAZIONE
006600960122     C* 59    - ERRORI IN ALLOCAZIONE
006700951207     C*****************************************************************
006800951207     C     *ENTRY        PLIST
006900951207     C                   PARM                    KPJBA
007000951207     C                   MOVEL     KPJBU         PARAM
007100951207     C                   MOVEL     PARMBD        CMZMBD
007200021010     C                   MOVEL     PARMBD        CMZMBa
007300021008     C                   MOVEL     PARMBD        VAR               9
007400021008     C                   MOVE      '))'          VAR
007500951213     C*
007600951213     C*****************************************************************
007700951207     C                   SETOFF                                       405556
007800960122     C                   SETOFF                                       59
007900051121     c
008000051121     c                   if        parflg<>'M'
008100951207     C                   CLEAR                   PARALL
008200051121     c                   endif
008300951207     C*
008400951207     C* TIPO LANCIO: PARTLA = "C" -           CHIUDO CON LR
008500951207     C*              PARTLA = "L" - ELABORO E CHIUDO CON LR
008600951207     C*              PARTLA = " " - ELABORO E CHIUDO IN  RETRN
008700960122    1C     PARTLA        IFNE      'C'
008800951207     C*
008900960122     C** ALLOCAZIONE FNFGV01T
009000021008     C                   Z-ADD     42            LUNG             15 5
009100960122     C                   MOVEA     'FNFGV01T'    C56(13)
009200021008     C                   MOVEA     VAR           C56(34)
009300960122     C                   MOVEL     *BLANKS       COMMAN           80
009400960122     C                   MOVEA     C56           COMMAN
009500951207     C                   CALL      'QCMDEXC'                            55
009600951207     C                   PARM                    COMMAN
009700951207     C                   PARM                    LUNG
009800951207     C*
009900951207     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
010000951207     C   55              SETON                                          40
010100951207     C*
010200960122     C** ALLOCAZIONE FNFVD00T
010300951207     C                   MOVEA     'FNFVD00T'    C56(13)
010400021008     C                   MOVEA     VAR           C56(34)
010500951207     C                   MOVEL     *BLANKS       COMMAN           80
010600951207     C                   MOVEA     C56           COMMAN
010700951207     C                   CALL      'QCMDEXC'                            56
010800951207     C                   PARM                    COMMAN
010900951207     C                   PARM                    LUNG
011000951207     C* GIA' ALLOCATO RITENTO PIU' TARDI
011100951207     C   56              SETON                                        40
011200960122     C*
011300960122     C** ALLOCAZIONE FNFV400T
011400960122     C                   MOVEA     'FNFV400T'    C56(13)
011500021008     C                   MOVEA     VAR           C56(34)
011600960122     C                   MOVEL     *BLANKS       COMMAN           80
011700960122     C                   MOVEA     C56           COMMAN
011800960122     C                   CALL      'QCMDEXC'                            59
011900960122     C                   PARM                    COMMAN
012000960122     C                   PARM                    LUNG
012100960122     C* GIA' ALLOCATO RITENTO PIU' TARDI
012200960122     C   59              SETON                                        40
012300000427     C**
012400000427     C** ALLOCAZIONE FNFGW00T
012500021008     C                   Z-ADD     42            LUNG             15 5
012600000427     C                   MOVEA     'FNFGW00T'    C56(13)
012700021008     C                   MOVEA     VAR           C56(34)
012800000427     C                   MOVEL     *BLANKS       COMMAN           80
012900000427     C                   MOVEA     C56           COMMAN
013000000427     C                   CALL      'QCMDEXC'                            57
013100000427     C                   PARM                    COMMAN
013200000427     C                   PARM                    LUNG
013300000427     C*
013400000427     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
013500000427     C   57              SETON                                          40
013600121102
013700121102     C** ALLOCAZIONE FNFGS00T
013800121102     C                   Z-ADD     42            LUNG             15 5
013900121102     C                   MOVEA     'FNFGS00T'    C56(13)
014000121102     C                   MOVEA     VAR           C56(34)
014100121102     C                   MOVEL     *BLANKS       COMMAN           80
014200121102     C                   MOVEA     C56           COMMAN
014300121102     C                   CALL      'QCMDEXC'                            58
014400121102     C                   PARM                    COMMAN
014500121102     C                   PARM                    LUNG
014600121102     C*
014700121102     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
014800121102     C   58              SETON                                          40
014900951207     C*
015000980218     C* PROSEGUO SOLO SE E' RIUSCITA ALMENO UNA ALLOCAZIONE
015100960124    2C     *IN55         IFEQ      *OFF
015200000427     C     *IN57         ANDEQ     *OFF
015300121102     C     *IN58         ANDEQ     *OFF
015400980218     C     *IN56         OREQ      *OFF
015500960124     C     *IN59         OREQ      *OFF
015600951207     C*
015700960122     C* S C R I V O   M E M B R I   D I   S E D E
015800951207     C                   EXSR      WTRFV
015900951207     C*
016000960122     C* VEDO SE CONTIENE RECORD FNFGV01T
016100960122     C                   MOVEL     'FNFGV01T'    CMZFLD
016200951207     C                   EXSR      NUMREC
016300960122     C                   Z-ADD     NRR           RECFGV           10 0
016400000427     C* VEDO SE CONTIENE RECORD FNFGW00T
016500000427     C                   MOVEL     'FNFGW00T'    CMZFLD
016600000427     C                   EXSR      NUMREC
016700000427     C                   Z-ADD     NRR           RECFGW           10 0
016800951207     C* VEDO SE CONTIENE RECORD FNFVD00T
016900951207     C                   MOVEL     'FNFVD00T'    CMZFLD
017000951207     C                   EXSR      NUMREC
017100951207     C                   Z-ADD     NRR           RECFVD           10 0
017200951207     C* VEDO SE CONTIENE RECORD FNFV400T
017300951207     C                   MOVEL     'FNFV400T'    CMZFLD
017400951207     C                   EXSR      NUMREC
017500951207     C                   Z-ADD     NRR           RECFV4           10 0
017600121102     C* VEDO SE CONTIENE RECORD FNFGS00T
017700121102     C                   MOVEL     'FNFGS00T'    CMZFLD
017800121102     C                   EXSR      NUMREC
017900121102     C                   Z-ADD     NRR           RECFGS           10 0
018000951207     C*
018100951207     C* I N V I O
018200960122    3C     RECFGV        IFGT      0
018300980218     C     RECFGV        ANDNE     *HIVAL
018400980218     C     RECFVD        ORGT      0
018500980218     C     RECFGV        ANDNE     *HIVAL
018600951207     C     RECFV4        ORGT      0
018700980218     C     RECFGV        ANDNE     *HIVAL
018800951207     C                   MOVE      '046'         CMZDST
018900951207     C                   MOVE      'S'           CMZURG
019000951207     C                   MOVE      *BLANK        CMZPRR
019100951207     C                   MOVE      *BLANK        CMZAZN
019200951207     C*
019300951207     C                   EXSR      X25FV4
019400951207     C                   EXSR      X25FVD
019500000427     C                   EXSR      X25FGW
019600121102     C                   EXSR      X25FGs
019700000427     C                   EXSR      X25FGV
019800960122    3C                   ENDIF
019900960125    2C                   ENDIF
020000960122     C*
020100960122     C** DISALLOCO FNFV400T
020200021008     C                   Z-ADD     42            LUNG
020300960125    2C     *IN59         IFEQ      *OFF
020400960122     C                   MOVEA     'FNFV400T'    C57(13)
020500021008     C                   MOVEA     VAR           C57(34)
020600960122     C                   MOVEL     *BLANKS       COMMAN           80
020700960122     C                   MOVEA     C57           COMMAN
020800980218     C                   CALL      'QCMDEXC'                            30
020900960122     C                   PARM                    COMMAN
021000960122     C                   PARM                    LUNG
021100960125    2C                   ENDIF
021200960125     C*
021300980218     C** DISALLOCO FNFVD00T
021400980218    2C     *IN56         IFEQ      *OFF
021500980218     C                   MOVEA     'FNFVD00T'    C57(13)
021600021008     C                   MOVEA     VAR           C57(34)
021700980218     C                   MOVEL     *BLANKS       COMMAN           80
021800980218     C                   MOVEA     C57           COMMAN
021900980218     C                   CALL      'QCMDEXC'                            30
022000980218     C                   PARM                    COMMAN
022100980218     C                   PARM                    LUNG
022200980218    2C                   ENDIF
022300021008     C** DISALLOCO FNFGW00T
022400021008    2C     *IN57         IFEQ      *OFF
022500021008     C                   MOVEA     'FNFGW00T'    C57(13)
022600021008     C                   MOVEA     VAR           C57(34)
022700021008     C                   MOVEL     *BLANKS       COMMAN           80
022800021008     C                   MOVEA     C57           COMMAN
022900021008     C                   CALL      'QCMDEXC'                            30
023000021008     C                   PARM                    COMMAN
023100021008     C                   PARM                    LUNG
023200021008    2C                   ENDIF
023300121102     C** DISALLOCO FNFGs00T
023400121102    2C     *IN58         IFEQ      *OFF
023500121102     C                   MOVEA     'FNFGS00T'    C57(13)
023600121102     C                   MOVEA     VAR           C57(34)
023700121102     C                   MOVEL     *BLANKS       COMMAN           80
023800121102     C                   MOVEA     C57           COMMAN
023900121102     C                   CALL      'QCMDEXC'                            30
024000121102     C                   PARM                    COMMAN
024100121102     C                   PARM                    LUNG
024200121102    2C                   ENDIF
024300980218     C** DISALLOCO FNFGV01T
024400960125    2C     *IN55         IFEQ      *OFF
024500960122     C                   MOVEA     'FNFGV01T'    C57(13)
024600021008     C                   MOVEA     VAR           C57(34)
024700960122     C                   MOVEL     *BLANKS       COMMAN           80
024800960122     C                   MOVEA     C57           COMMAN
024900980218     C                   CALL      'QCMDEXC'                            30
025000951207     C                   PARM                    COMMAN
025100951207     C                   PARM                    LUNG
025200960125    2C                   ENDIF
025300960122     C*
025400960125     C* 40 ON  - RITENTARE PIU' TARDI PERCHE' NON E' RIUSCITA
025500960125     C*   L'ALLOCAZIONE DI TUTTI I MEMBRI
025600960125    2C     *IN40         IFEQ      *ON
025700960129     C*
025800960129     C* SE RICHIAMATO DA GESTIONE F.VIAGGIO E NON SONO AVVENUTE LE
025900960129     C*   ALLOCAZIONI DI TUTTI I FILES INVIO MESSAGGIO IN SEDE
026000051121    3c     PARFLG        IFEQ      'M'
026100051121     c* Tento 3 volte e poi do il messaggio
026200051121     C                   select
026300051121    4c                   when      parall=' '
026400051121     c                   eval      parall='1'
026500051121     C                   MOVEL     PARAM         KPJBU
026600051121     C                   MOVEL     'LSA4'        KCOAZ
026700051121     C                   CALL      'BCH10'
026800051121     C                   PARM                    KPJBA
026900051121     c
027000051121     c                   when      parall='1'
027100051121     c                   eval      parall='2'
027200051121     C                   MOVEL     PARAM         KPJBU
027300051121     C                   MOVEL     'LSA4'        KCOAZ
027400051121     C                   CALL      'BCH10'
027500051121     C                   PARM                    KPJBA
027600051121     c
027700051121     c                   when      parall='2'
027800051121     c                   eval      parall='3'
027900051121     C                   MOVEL     PARAM         KPJBU
028000051121     C                   MOVEL     'LSA4'        KCOAZ
028100051121     C                   CALL      'BCH10'
028200051121     C                   PARM                    KPJBA
028300051121     c
028400051121     c                   when      parall='3'
028500111103     C                   movel     *blanks       msgtes
028600111103     C                   movel     *blanks       msgdet
028700960129     C                   MOVEL     MSG(1)        MSGTES
028800960201     C                   MOVEL(P)  MSG(2)        MSGDET
028900960129     C                   MOVE      PARLNP        MSLNP
029000120302     C                   eval      wrkEml= 'ced@brt.it'
029100111103
029200111103     C                   call(e)   'TIS701C'
029300111103     C                   parm                    wrkEml          253
029400111103     C                   PARM                    MSGTES           44
029500111103     C                   PARM                    MSGDET
029600111103     c
029700051121     c                   other
029800051121     C                   MOVEL     '1'           PARALL
029900051121     C                   MOVEL     PARAM         KPJBU
030000051121    4C                   ENDsl
030100051121    3C                   ENDIF
030200960122    2C                   ENDIF
030300960122    1C                   ENDIF
030400951207     C*
030500951207     C* CHIUSURA PGM
030600960122    1C     PARTLA        IFEQ      ' '
030700951207     C                   RETURN
030800960122   X1C                   ELSE
030900951207     C                   SETON                                        LR
031000960122    1C                   ENDIF
031100951207     C*
031200951207     C**************************************************************************
031300951207     C*  SCRIVO MEMBRO FILE FOGLI VIAGGIO PER LA SEDE
031400951207     C**************************************************************************
031500951207     C     WTRFV         BEGSR
031600951207     C*
031700951207     C* APERTURA MEMBRI
031800951207     C                   EXSR      CAEXFV
031900951207     C*
032000960125    1C     *IN55         IFEQ      *OFF
032100000427    1C     *IN57         ANDEQ     *OFF
032200121105    1C     *IN58         ANDEQ     *OFF
032300960125     C*
032400960122     C** SCRITTURA FNFGV01T
032500021009     C     SIMFEL        SETLL     FNFGV46L
032600021009     C     SIMFEL        READE     FNFGV46L                               31
032700000427     C*
032800960125    2C     *IN31         DOWEQ     *OFF
032900000427     C* ESTENSIONE FOGLI
033000021009     C     KFGW          CHAIN     FNFGW01L                           35
033100121102     C     KFGW          CHAIN     FNFGs01L
033200000427     C**
033300960125     C* SE PRESENTE REPERISCO LA CAUSALE RITARDO PARTENZA
033400960125     C                   CLEAR                   FGVRTP
033500960122     C     KFV2          CHAIN     FNFV201L                           30
033600960125    3C     *IN30         IFEQ      *OFF
033700960122     C                   MOVEL     FV2RTC        FGVRTP
033800960122     C                   MOVEL     'T'           FV2FTR
033900960122     C                   Z-ADD     NUMTRA        FV2DTR
034000960122     C                   UPDATE    FNFV2000
034100960125    3C                   ENDIF
034200960919     C*
034300951207     C                   WRITE     FNFGV0TT
034400000427     C  N35              WRITE     FNFGW0TT
034500121102     c                   if        %found(fnfgs01l)
034600121102     C                   WRITE     FNFGs0TT
034700121102     c                   endif
034800000427     C*
034900951207     C                   DELETE    FNFGV000
035000951207     C*
035100021009     C     SIMFEL        READE     FNFGV46L                               31
035200960125    2C                   ENDDO
035300980218     C* CHIUSURA MEMBRO
035400960125     C                   CLOSE     FNFGV01T
035500000427     C                   CLOSE     FNFGW00T
035600121102     C                   CLOSE     FNFGs00T
035700960125    1C                   ENDIF
035800980218     C*
035900980218    1C     *IN56         IFEQ      *OFF
036000980218     C** SCRITTURA DI FNFVD00T
036100100316     c* Elaboro per terminal per evitare problemi in trasmissione
036200100316     C     kfvd          SETLL     FNFVD02L
036300100316     C     kfvd          READE     FNFVD02L                               32
036400980218    3C     *IN32         DOWEQ     *OFF
036500980218     C                   WRITE     FNFVD0TT
036600980218     C                   MOVEL     'T'           FVDFTR
036700980218     C                   Z-ADD     NUMTRA        FVDDTR
036800980218     C                   UPDATE    FNFVD000
036900100316     C     kfvd          READE     FNFVD02L                               32
037000980218    3C                   ENDDO
037100980218     C                   CLOSE     FNFVD00T
037200980218    1C                   ENDIF
037300960122     C*
037400960122     C** SCRITTURA FNFV400T
037500960125    1C     *IN59         IFEQ      *OFF
037600960125     C*
037700021106     C* Elaboro solo i record con "P-PARTENZA",
037800021106     C*                           Linea di Invio F.V. = 0
037900021106     C*                           Linea di Partenza = simfel
038000021106     C     K03FV403      SETLL     FNFV403L
038100021106     C     K03FV403      READE     FNFV403L                               31
038200960125    2C     *IN31         DOWEQ     *OFF
038300021106     C* Trasmetto solo i record con Flag Trasmissione a Sede vuoto
038400021106    3C     FV4FT2        IFEQ      ' '
038500960125     C*
038600960919     C* AGGANCIO IL FOGLIO VIAGGIO PER VEDERE SE I RECORD DI FNFV4
038700960919     C*   APPARTENGONO A UN FV DA TRASMETTERE IN SEDE
038800960919     C                   EXSR      CTRFGV
038900960919     C*
039000960919     C* 12 ON - IL FV E' DA TRASMETTERE IN SEDE QUINDI ANCHE I RELATIVI
039100960919     C*  RECORD DI FNFV4 SONO DA TRASMETTERE
039200960919    4C     *IN12         IFEQ      *ON
039300960125     C* IMPOSTO DATA E ORA VARIAZIONE CON UDATE E UTIME
039400960125     C                   Z-ADD     NUMTRA        FV4DTV
039500960125     C                   Z-ADD     UTIME         FV4ORV
039600960122     C                   WRITE     FNFV40TT
039700960122     C*
039800960125     C                   MOVEL     'T'           FV4FT2
039900130215   x4c                   else
040000130215     C                   MOVEL     'N'           FV4FT2
040100960919    4C                   ENDIF
040200130215    c
040300130215     C                   Z-ADD     NUMTRA        FV4DT2
040400130215     C                   UPDATE    FNFV4000
040500960125    3C                   ENDIF
040600960122     C*
040700021106     C     K03FV403      READE     FNFV403L                               31
040800960125    2C                   ENDDO
040900960125     C*
041000960125     C* CHIUSURA MEMBRI
041100960125     C                   CLOSE     FNFV400T
041200960125    1C                   ENDIF
041300951207     C*
041400951207     C                   ENDSR
041500951207     C**************************************************************************
041600951207     C*  ESEGUE CALL QCAEXEC PER OVRDBF SUI FILE
041700951207     C**************************************************************************
041800951207     C     CAEXFV        BEGSR
041900021008     C                   MOVEL     PARMBD        VAR8
042000021008     C                   MOVE      ')'           VAR8              8
042100951207     C                   Z-ADD     46            LUNG
042200951207     C*
042300980218     C** OVRDBF FNFGV01T
042400960125    1C     *IN55         IFEQ      *OFF
042500000427     C     *IN57         ANDEQ     *OFF
042600121102     C     *IN58         ANDEQ     *OFF
042700960122     C                   MOVEA     'FNFGV01T'    C17(13)
042800021008     C                   MOVEA     VAR8          C17(38)
042900951207     C                   MOVEL     *BLANKS       COMMAN
043000951207     C                   MOVEA     C17           COMMAN
043100951207     C                   CALL      'QCMDEXC'
043200951207     C                   PARM                    COMMAN
043300951207     C                   PARM                    LUNG
043400980218     C* APERTURA FILE
043500980218     C                   OPEN      FNFGV01T
043600121102     C** OVRDBF
043700000427     C                   MOVEA     'FNFGW00T'    C17(13)
043800021008     C                   MOVEA     VAR8          C17(38)
043900000427     C                   MOVEL     *BLANKS       COMMAN
044000000427     C                   MOVEA     C17           COMMAN
044100000427     C                   CALL      'QCMDEXC'
044200000427     C                   PARM                    COMMAN
044300000427     C                   PARM                    LUNG
044400000427     C* APERTURA FILE
044500000427     C                   OPEN      FNFGW00T
044600121102     C** OVRDBF
044700121102     C                   MOVEA     'FNFGS00T'    C17(13)
044800121102     C                   MOVEA     VAR8          C17(38)
044900121102     C                   MOVEL     *BLANKS       COMMAN
045000121102     C                   MOVEA     C17           COMMAN
045100121102     C                   CALL      'QCMDEXC'
045200121102     C                   PARM                    COMMAN
045300121102     C                   PARM                    LUNG
045400121102     C* APERTURA FILE
045500121102     C                   OPEN      FNFGS00T
045600980218    1C                   ENDIF
045700951207     C*
045800960122     C** OVRDBF FNFVD00T
045900980218     C     *IN56         IFEQ      *OFF
046000951207     C                   MOVEA     'FNFVD00T'    C17(13)
046100021008     C                   MOVEA     VAR8          C17(38)
046200951207     C                   MOVEL     *BLANKS       COMMAN
046300951207     C                   MOVEA     C17           COMMAN
046400951207     C                   CALL      'QCMDEXC'
046500951207     C                   PARM                    COMMAN
046600951207     C                   PARM                    LUNG
046700980218     C* APERTURA FILE
046800980218     C                   OPEN      FNFVD00T
046900980218    1C                   ENDIF
047000960125     C*
047100960125     C*
047200960122     C** OVRDBF FNFV400T
047300960125    1C     *IN59         IFEQ      *OFF
047400960122     C                   MOVEA     'FNFV400T'    C17(13)
047500021008     C                   MOVEA     VAR8          C17(38)
047600960122     C                   MOVEL     *BLANKS       COMMAN
047700960122     C                   MOVEA     C17           COMMAN
047800960122     C                   CALL      'QCMDEXC'
047900960122     C                   PARM                    COMMAN
048000960122     C                   PARM                    LUNG
048100951207     C*
048200951207     C* APERTURA FILE
048300951207     C                   OPEN      FNFV400T
048400960125    1C                   ENDIF
048500951207     C*
048600951207     C                   ENDSR
048700960919     C**************************************************************************
048800960919     C*  CONTROLLO SE FV DA TRASMETTERE IN SEDE
048900960919     C**************************************************************************
049000960919     C     CTRFGV        BEGSR
049100960919     C*
049200960919     C* AGGANCIO IL FOGLIO VIAGGIO SOLO QUANDO CAMBIA
049300960919    0C     WNFV          IFNE      FV4NFV
049400960919     C                   SETOFF                                       12
049500960919     C*
049600960919     C     KFGV          CHAIN     FNFGV01L                           32
049700960919    1C     *IN32         IFEQ      *OFF
049800960919     C*
049900960919     C* SOLO A CAMBIO TIPO TRAINO CONTROLLO SE IL FV E' DA TRASMETTERE
050000960919     C*   IN SEDE
050100960919    2C     WTTR          IFNE      FGVTTR
050200960919     C                   MOVEL(P)  FGVTTR        KEY
050300960919     C     KTAB          CHAIN     TABEL                              33
050400960919    3C     *IN33         IFEQ      *OFF
050500960919     C     TBLFLG        ANDEQ     ' '
050600960919     C                   MOVEL     TBLUNI        DSTV
050700960919   X3C                   ELSE
050800960919     C                   MOVEL     'S'           §TVUTC
050900960919    3C                   ENDIF
051000960919     C                   MOVEL     FGVTTR        WTTR
051100960919    2C                   ENDIF
051200960919     C*
051300960919     C* IL FOGLIO VIAGGIO E' DA INVIARE IN SEDE SE:
051400960919     C*   - IL FOGLIO NON E' ABBINATO
051500960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
051600960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
051700960919     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTC = "S")
051800960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
051900960919     C*     C'E' IL RECORD IN TABELLA
052000960919    2C     FGVTRN        IFEQ      0
052100960919     C     FGVTTR        OREQ      ' '
052200960919     C     §TVUTC        OREQ      'S'
052300960919     C                   SETON                                        12
052400960919    2C                   ENDIF
052500960919     C*
052600960919    1C                   ENDIF
052700960919     C                   MOVEL     FV4NFV        WNFV
052800960919    0C                   ENDIF
052900960919     C*
053000960919     C                   ENDSR
053100960122     C**************************************************************************
053200960122     C*  SOTTOMETTE TRASMISSIONE   F N F V 4 0 0 T
053300960122     C**************************************************************************
053400960122     C     X25FV4        BEGSR
053500960122     C*
053600960122     C     RECFV4        IFGT      0
053700980218     C     RECFV4        ANDNE     *HIVAL
053800960122     C                   MOVEL     'FNFV400T'    CMZFLD
053900960122     C                   MOVEL     'FNFV400R'    CMZFLA
054000960129     C                   MOVE      'TR41'        CMZAZN
054100960122     C                   MOVE      RECFV4        CMZNRR
054200960122     C*
054300960122     C                   EXSR      CALCMZ
054400960122     C                   ENDIF
054500960122     C*
054600960122     C                   ENDSR
054700951207     C**************************************************************************
054800951207     C*  SOTTOMETTE TRASMISSIONE   F N F V D 0 0 T
054900951207     C**************************************************************************
055000951207     C     X25FVD        BEGSR
055100951207     C*
055200980218     C     RECFVD        IFGT      0
055300980218     C     RECFVD        ANDNE     *HIVAL
055400980218     C     RECFGV        IFEQ      0
055500980218     C     RECFGV        OREQ      *HIVAL
055600980218     C                   MOVEL     'TR40'        CMZAZN
055700980218     C                   ELSE
055800960212     C                   MOVE      '    '        CMZAZN
055900980218     C                   END
056000951207     C                   MOVEL     'FNFVD00T'    CMZFLD
056100951207     C                   MOVEL     'FNFVD00R'    CMZFLA
056200951207     C                   MOVE      RECFVD        CMZNRR
056300951207     C*
056400951207     C                   EXSR      CALCMZ
056500951207     C                   ENDIF
056600951207     C*
056700951207     C                   ENDSR
056800951207     C**************************************************************************
056900960130     C*  SOTTOMETTE TRASMISSIONE   F N F G V 0 1 T
057000951207     C**************************************************************************
057100960122     C     X25FGV        BEGSR
057200951207     C*
057300960122     C     RECFGV        IFGT      0
057400980218     C     RECFGV        ANDNE     *HIVAL
057500960122     C                   MOVEL     'FNFGV01T'    CMZFLD
057600960122     C                   MOVEL     'FNFGV01R'    CMZFLA
057700960130     C                   MOVE      'TR40'        CMZAZN
057800960122     C                   MOVE      RECFGV        CMZNRR
057900951207     C*
058000951207     C                   EXSR      CALCMZ
058100951207     C                   ENDIF
058200951207     C*
058300951207     C                   ENDSR
058400000427     C**************************************************************************
058500000427     C*  SOTTOMETTE TRASMISSIONE   F N F G W 0 0 T
058600000427     C**************************************************************************
058700000427     C     X25FGW        BEGSR
058800000427     C*
058900000427     C     RECFGW        IFGT      0
059000000427     C     RECFGW        ANDNE     *HIVAL
059100000427     C                   MOVEL     'FNFGW00T'    CMZFLD
059200000427     C                   MOVEL     'FNFGW00R'    CMZFLA
059300000427     C                   MOVE      '    '        CMZAZN
059400000428     C                   MOVE      RECFGW        CMZNRR
059500000427     C*
059600000427     C                   EXSR      CALCMZ
059700000427     C                   ENDIF
059800000427     C*
059900000427     C                   ENDSR
060000121102     C**************************************************************************
060100121102     C*  SOTTOMETTE TRASMISSIONE   F N F G S 0 0 T
060200121102     C**************************************************************************
060300121102     C     X25FGS        BEGSR
060400121102     C*
060500121102     C     RECFGS        IFGT      0
060600121102     C     RECFGS        ANDNE     *HIVAL
060700121102     C                   MOVEL     'FNFGS00T'    CMZFLD
060800121102     C                   MOVEL     'FNFGS00R'    CMZFLA
060900121102     C                   MOVE      '    '        CMZAZN
061000121102     C                   MOVE      RECFGS        CMZNRR
061100121102     C*
061200121102     C                   EXSR      CALCMZ
061300121102     C                   ENDIF
061400121102     C*
061500121102     C                   ENDSR
061600951207     C**************************************************************************
061700951207     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
061800951207     C**************************************************************************
061900951207     C     NUMREC        BEGSR
062000951207     C*
062100951207     C                   Z-ADD     0             NRR
062200951207     C                   CALL      'TRUL51C'
062300951207     C                   PARM                    KNSIF
062400951207     C                   PARM                    CMZFLD           10
062500951207     C                   PARM                    PARMBD
062600951207     C                   PARM                    NRR              10 0
062700951207     C*
062800951207     C                   ENDSR
062900951207     C**************************************************************************
063000960122     C*  CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSIONE
063100951207     C**************************************************************************
063200951207     C     CALCMZ        BEGSR
063300951207     C*
063400000427     C                   CLEAR                   W236
063500021010     C****               CLEAR                   CMZLCL
063600021008     C                   CALL      'TIBS53C'
063700000427     C                   PARM                    CMZLCL            1
063800951207     C                   PARM                    CMZDST
063900951207     C                   PARM                    CMZFLD
064000951207     C                   PARM                    CMZMBD
064100951207     C                   PARM                    CMZLBD
064200951207     C                   PARM                    CMZFLA
064300951207     C                   PARM                    CMZMBA
064400951207     C                   PARM                    CMZLBA
064500951207     C                   PARM                    CMZORI
064600951207     C                   PARM                    CMZDTI
064700951207     C                   PARM                    CMZURG
064800951207     C                   PARM                    CMZADD
064900951207     C                   PARM                    CMZCLR
065000951207     C                   PARM                    CMZFTR
065100951207     C                   PARM                    CMZERR
065200951207     C                   PARM                    CMZAZN
065300951207     C                   PARM                    CMZSYF
065400951207     C                   PARM                    CMZNRR
065500951207     C                   PARM                    CMZPRR
065600000427     C                   PARM                    W236            236
065700951207     C                   ENDSR
065800951207     C**************************************************************************
065900951207     C*  SR INIZIALE
066000951207     C**************************************************************************
066100951207     C     *INZSR        BEGSR
066200951213     C*
066300971117     C                   MOVEL     KPJBU         PARAM
066400971117     C*
066500971113     C*  ESEGUO CALL PER REPERIRE LIBRERIA E S.I. DI TRAMSISSIONE
066600021008     C                   CLEAR                   TIBS55DS
066700971125     C                   MOVEL     'L'           I50TLA
066800021010     C                   MOVEL     simfel        I50Ppo
066900971113     C                   MOVEL     046           I50APO
067000021008     C                   CALL      'TIBS55R'
067100021008     C                   PARM                    TIBS55DS
067200971113     C**
067300021010     C                   MOVEL     O50lcl        CMZLcl
067400021010     C                   MOVEL     O50PLA        CMZLBD
067500971113     C                   MOVEL     O50ALA        CMZLBA
067600971113     C                   MOVEL     O50ASI        CMZSYF
067700951213     C                   MOVE      '0000'        CMZORI
067800951213     C                   MOVE      '0000000'     CMZDTI
067900951213     C                   MOVE      'A'           CMZADD
068000951213     C                   MOVE      'S'           CMZCLR
068100951213     C                   MOVE      'S'           CMZFTR
068200951213     C                   MOVE      *BLANK        CMZERR
068300960122     C* IMPOSTO CAMPI PER AGGANCIARE FNFV201L
068400960122     C                   MOVEL     'P'           WEPA
068500960122     C                   MOVEL     'R'           WTDH
068600021106     C                   clear                   Wzero
068700960919     C* IMPOSTO CAMPI PER AGGANCIARE TABEL00F
068800960919     C                   Z-ADD     1             CODUT
068900960919     C                   MOVEL     'TV'          COD
069000951213     C*
069100951213     C****  CONTROLLO ESISTENZA MEMBRI  ****
069200960122     C                   MOVEL     'FNFGV01T'    FILE
069300951213     C                   CALL      'TRUL50C'
069400951213     C                   PARM                    FILE
069500971114     C                   PARM                    PARMBD
069600951213     C                   PARM                    KNSIF
069700951213     C                   PARM                    LOGICO           10
069800951213     C                   PARM                    LOGMBR           10
069900951213     C                   PARM                    FLG               1
070000951213     C*
070100000428     C                   MOVEL     'FNFGW00T'    FILE
070200000428     C                   CALL      'TRUL50C'
070300000428     C                   PARM                    FILE
070400000428     C                   PARM                    PARMBD
070500000428     C                   PARM                    KNSIF
070600000428     C                   PARM                    LOGICO           10
070700000428     C                   PARM                    LOGMBR           10
070800000428     C                   PARM                    FLG               1
070900121102     C*
071000121102     C                   MOVEL     'FNFGS00T'    FILE
071100121102     C                   CALL      'TRUL50C'
071200121102     C                   PARM                    FILE
071300121102     C                   PARM                    PARMBD
071400121102     C                   PARM                    KNSIF
071500121102     C                   PARM                    LOGICO           10
071600121102     C                   PARM                    LOGMBR           10
071700121102     C                   PARM                    FLG               1
071800000428     C*
071900951213     C                   MOVEL     'FNFVD00T'    FILE
072000951213     C                   CALL      'TRUL50C'
072100951213     C                   PARM                    FILE             10
072200971114     C                   PARM                    PARMBD
072300951213     C                   PARM                    KNSIF
072400951213     C                   PARM                    LOGICO           10
072500951213     C                   PARM                    LOGMBR           10
072600951213     C                   PARM                    FLG               1
072700951213     C*
072800951213     C                   MOVEL     'FNFV400T'    FILE
072900951213     C                   CALL      'TRUL50C'
073000951213     C                   PARM                    FILE
073100971114     C                   PARM                    PARMBD
073200951213     C                   PARM                    KNSIF
073300951213     C                   PARM                    LOGICO           10
073400951213     C                   PARM                    LOGMBR           10
073500951213     C                   PARM                    FLG               1
073600951207     C*
073700951207     C* NUMERO TRASMISSIONE = DATA DEL GIORNO (AAAA/MM/GG)
073800951207     C                   TIME                    WTIME            14 0
073900960125     C                   MOVEL     WTIME         UTIME             6 0
074000951207     C                   MOVE      WTIME         UDATE8            8 0
074100951207     C                   Z-ADD     UDATE8        WDATA
074200951207     C                   Z-ADD     GG            MMGG              4 0
074300951207     C                   MOVEL     MM            MMGG
074400951207     C                   Z-ADD     MMGG          NUMTRA            8 0
074500951207     C                   MOVEL     AA            NUMTRA
074600960122     C*
074700960122     C****  KLIST  ****
074800960122     C* ACCESSO FNFV201L
074900960122     C     KFV2          KLIST
075000960122     C                   KFLD                    FGVLNP
075100960122     C                   KFLD                    FGVNFV
075200960122     C                   KFLD                    WEPA
075300960122     C                   KFLD                    WZERO             3 0
075400960122     C                   KFLD                    WTDH
075500960919     C* ACCESSO FNFGV01L
075600960919     C     KFGV          KLIST
075700960919     C                   KFLD                    FV4NFV
075800960919     C                   KFLD                    FV4LNP
075900021009     C     KFGW          KLIST
076000021009     C                   KFLD                    FGVNFV
076100021009     C                   KFLD                    FGVLNP
076200100316     C     KFVD          KLIST
076300100316     C                   KFLD                    kftr
076400100316     C                   KFLD                    simfel
076500100316     c                   clear                   kftr              1
076600021106     C* Accesso FNFV403L
076700021106     C     K03FV403      klist
076800021106     C                   kfld                    Wepa
076900021106     C                   kfld                    Wzero
077000021106     C                   kfld                    SIMfel
077100960919     C* ACCESSO TABEL00F
077200960919     C     KTAB          KLIST
077300960919     C                   KFLD                    CODUT
077400960919     C                   KFLD                    COD
077500960919     C                   KFLD                    KEY
077600960122     C*
077700960122     C****  DEFINIZIONE CAMPI  ****
077800960122     C     *LIKE         DEFINE    FV2EPA        WEPA
077900960122     C     *LIKE         DEFINE    FV2TDH        WTDH
078000960919     C     *LIKE         DEFINE    TBLKUT        CODUT
078100960919     C     *LIKE         DEFINE    TBLCOD        COD
078200960919     C     *LIKE         DEFINE    TBLKEY        KEY
078300960919     C     *LIKE         DEFINE    FGVTTR        WTTR
078400960919     C     *LIKE         DEFINE    FV4NFV        WNFV
078500951207     C                   ENDSR
078600951207     C**************************************************************************
078700971204**         C17
078800021008OVRDBF FILE(XXXXXXXX)            MBR(M000000)
078900951207**         C56
079000021008ALCOBJ OBJ((XXXXXXXX *FILE *EXCL MXXXyyy))
079100951207**         C57
079200021008DLCOBJ OBJ((XXXXXXXX *FILE *EXCL M000000))
079300960129** MSG
079400111103FNLSA4R:ERRORE TRASMISSIONE F.VIAGGIO PART
079500971204A SEDE: NON TRASMESSO FOGLIO VIAGGIO DALLA LINEA XXX
