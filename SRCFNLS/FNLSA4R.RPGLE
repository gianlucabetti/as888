000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200951207     H* FNLSA4R *-----------------------------------------------------*
000300000427     H*   TRASMISSIONE A SEDE DI FNFGV01T - FGW00T- FNFVD00T - FNFV400T
000400951207     H*---------------------------------------------------------------*
000500960125     FFNFGV46L  UF   E           K DISK
000600960122     FFNFV201L  UF   E           K DISK
000700980218     FFNFVD02L  UF   E           K DISK
000800021106     FFNFV403L  UF   E           K DISK
000900130515     FFNFV401L  IF   E           K DISK    RENAME(FNFv4000:FNFv4001)
001000960919     FFNFGV01L  IF   E           K DISK
001100960919     F                                     RENAME(FNFGV000:FNFGV001)
001200000427     FFNFGW01L  IF   E           K DISK
001300121102     FFNFGs01L  IF   E           K DISK
001400960919     FTABEL00F  IF   E           K DISK
001500130515     Faiata01l  UF   E           K DISK
001600960122     FFNFGV01T  O  A E             DISK    USROPN
001700951207     F                                     RENAME(FNFGV000:FNFGV0TT)
001800951207     FFNFVD00T  O  A E             DISK    USROPN
001900951207     F                                     RENAME(FNFVD000:FNFVD0TT)
002000951207     FFNFV400T  O  A E             DISK    USROPN
002100951207     F                                     RENAME(FNFV4000:FNFV40TT)
002200000427     FFNFGW00T  O  A E             DISK    USROPN
002300000427     F                                     RENAME(FNFGW000:FNFGW0TT)
002400121102     FFNFGs00T  O  A E             DISK    USROPN
002500121102     F                                     RENAME(FNFGs000:FNFGs0TT)
002600951207     F*
002700951207     D* DEFINIZIONE SCHIERE
002800951207     D C17             S              1    DIM(45) CTDATA PERRCD(45)
002900021008     D C56             S              1    DIM(42) CTDATA PERRCD(42)
003000021008     D C57             S              1    DIM(42) CTDATA PERRCD(42)
003100960201     D MSG             S             70    DIM(2) CTDATA PERRCD(1)
003200951207     D*
003300951207     D* PARAMETRI PASSATI DAI PGM CHIAMANTI
003400951207     D PARAM           DS
003500951207     D  PARMBD                 1     10
003600971113     D  SIMFEL                11     13  0
003700960129     D* IL CAMPO PARALL VIENE RESTITUITO IN OUTPUT NEL CASO NON SIANO
003800960129     D*   RIUSCITE LE ALLOCAZIONI DI TUTTI I FILES
003900951207     D  PARALL                14     14
004000960129     D* TIPO LANCIO PROGRAMMA ("C" / "L" / " ")
004100951207     D  PARTLA                15     15
004200960129     D* PARFLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
004300960129     D  PARFLG                16     16
004400971204     D* IL CAMPO PARLNP DEVE ESSERE PASSATO SOLO SE IL PGM E' RICHIAMAT
004500971204     D*   DALLA GESTIONE F.VIAGGIO
004600960129     D  PARLNP                17     19  0
004700951207     D                 DS
004800951207     D  GG                     1      2  0
004900951207     D  MM                     3      4  0
005000951207     D  AA                     5      8  0
005100951207     D  WDATA                  1      8  0
005200960129     D                 DS
005300111103     D  MSGDET                 1   5000
005400971204     D  MSLNP                 50     52
005500951207     D*
005600130515     D DS3I          E DS
005700130515     D DSTV          E DS
005800951207     D KPJBA         E DS
005900951207     D DSCMZ         E DS
006000021008     D TIBS55DS      E DS
006100130515     d
006200130515     d AlfaSOC         s              3
006300130515     d SocTraini       s              3  0
006400130515     D KPDR            S                   LIKE(ataksc)
006500130515     D Kdcn            S                   LIKE(ataaut)
006600130515     D KTRC            S                   LIKE(FV4TRC)
006700130515     D KEPA            S                   LIKE(FV4EPA)
006800130515     D KTFA            S                   LIKE(FGVLNA)
006900130515     D Kcnc            S                   like(atatel)
007000130515     C******************************* *********************************
007100951207     C* RIEPILOGO INDICATORI
007200951207     C*****************************************************************
007300960919     C* 12    - IL FV E' DA TRASMETTERE IN SEDE QUINDI ANCHE I RELATIVI
007400960919     C*         RECORD DI FNFV4 SONO DA TRASMETTERE
007500960919     C* 30/33 - DI COMODO
007600951207     C* 40    - NON SONO RIUSCITA AD ALLOCARE IL MEMBRO
007700951207     C* 55/56 - ERRORI IN ALLOCAZIONE
007800960122     C* 59    - ERRORI IN ALLOCAZIONE
007900951207     C*****************************************************************
008000951207     C     *ENTRY        PLIST
008100951207     C                   PARM                    KPJBA
008200951207     C                   MOVEL     KPJBU         PARAM
008300951207     C                   MOVEL     PARMBD        CMZMBD
008400021010     C                   MOVEL     PARMBD        CMZMBa
008500021008     C                   MOVEL     PARMBD        VAR               9
008600021008     C                   MOVE      '))'          VAR
008700951213     C*
008800951213     C*****************************************************************
008900951207     C                   SETOFF                                       405556
009000960122     C                   SETOFF                                       59
009100051121     c
009200051121     c                   if        parflg<>'M'
009300951207     C                   CLEAR                   PARALL
009400051121     c                   endif
009500951207     C*
009600951207     C* TIPO LANCIO: PARTLA = "C" -           CHIUDO CON LR
009700951207     C*              PARTLA = "L" - ELABORO E CHIUDO CON LR
009800951207     C*              PARTLA = " " - ELABORO E CHIUDO IN  RETRN
009900960122    1C     PARTLA        IFNE      'C'
010000951207     C*
010100960122     C** ALLOCAZIONE FNFGV01T
010200021008     C                   Z-ADD     42            LUNG             15 5
010300960122     C                   MOVEA     'FNFGV01T'    C56(13)
010400021008     C                   MOVEA     VAR           C56(34)
010500960122     C                   MOVEL     *BLANKS       COMMAN           80
010600960122     C                   MOVEA     C56           COMMAN
010700951207     C                   CALL      'QCMDEXC'                            55
010800951207     C                   PARM                    COMMAN
010900951207     C                   PARM                    LUNG
011000951207     C*
011100951207     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
011200951207     C   55              SETON                                          40
011300951207     C*
011400960122     C** ALLOCAZIONE FNFVD00T
011500951207     C                   MOVEA     'FNFVD00T'    C56(13)
011600021008     C                   MOVEA     VAR           C56(34)
011700951207     C                   MOVEL     *BLANKS       COMMAN           80
011800951207     C                   MOVEA     C56           COMMAN
011900951207     C                   CALL      'QCMDEXC'                            56
012000951207     C                   PARM                    COMMAN
012100951207     C                   PARM                    LUNG
012200951207     C* GIA' ALLOCATO RITENTO PIU' TARDI
012300951207     C   56              SETON                                        40
012400960122     C*
012500960122     C** ALLOCAZIONE FNFV400T
012600960122     C                   MOVEA     'FNFV400T'    C56(13)
012700021008     C                   MOVEA     VAR           C56(34)
012800960122     C                   MOVEL     *BLANKS       COMMAN           80
012900960122     C                   MOVEA     C56           COMMAN
013000960122     C                   CALL      'QCMDEXC'                            59
013100960122     C                   PARM                    COMMAN
013200960122     C                   PARM                    LUNG
013300960122     C* GIA' ALLOCATO RITENTO PIU' TARDI
013400960122     C   59              SETON                                        40
013500000427     C**
013600000427     C** ALLOCAZIONE FNFGW00T
013700021008     C                   Z-ADD     42            LUNG             15 5
013800000427     C                   MOVEA     'FNFGW00T'    C56(13)
013900021008     C                   MOVEA     VAR           C56(34)
014000000427     C                   MOVEL     *BLANKS       COMMAN           80
014100000427     C                   MOVEA     C56           COMMAN
014200000427     C                   CALL      'QCMDEXC'                            57
014300000427     C                   PARM                    COMMAN
014400000427     C                   PARM                    LUNG
014500000427     C*
014600000427     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
014700000427     C   57              SETON                                          40
014800121102
014900121102     C** ALLOCAZIONE FNFGS00T
015000121102     C                   Z-ADD     42            LUNG             15 5
015100121102     C                   MOVEA     'FNFGS00T'    C56(13)
015200121102     C                   MOVEA     VAR           C56(34)
015300121102     C                   MOVEL     *BLANKS       COMMAN           80
015400121102     C                   MOVEA     C56           COMMAN
015500121102     C                   CALL      'QCMDEXC'                            58
015600121102     C                   PARM                    COMMAN
015700121102     C                   PARM                    LUNG
015800121102     C*
015900121102     C* NON RIESCO AD ALLOCARE MEMBRO: SALVO E RIPROVO DOPO
016000121102     C   58              SETON                                          40
016100951207     C*
016200980218     C* PROSEGUO SOLO SE E' RIUSCITA ALMENO UNA ALLOCAZIONE
016300960124    2C     *IN55         IFEQ      *OFF
016400000427     C     *IN57         ANDEQ     *OFF
016500121102     C     *IN58         ANDEQ     *OFF
016600980218     C     *IN56         OREQ      *OFF
016700960124     C     *IN59         OREQ      *OFF
016800951207     C*
016900960122     C* S C R I V O   M E M B R I   D I   S E D E
017000951207     C                   EXSR      WTRFV
017100951207     C*
017200960122     C* VEDO SE CONTIENE RECORD FNFGV01T
017300960122     C                   MOVEL     'FNFGV01T'    CMZFLD
017400951207     C                   EXSR      NUMREC
017500960122     C                   Z-ADD     NRR           RECFGV           10 0
017600000427     C* VEDO SE CONTIENE RECORD FNFGW00T
017700000427     C                   MOVEL     'FNFGW00T'    CMZFLD
017800000427     C                   EXSR      NUMREC
017900000427     C                   Z-ADD     NRR           RECFGW           10 0
018000951207     C* VEDO SE CONTIENE RECORD FNFVD00T
018100951207     C                   MOVEL     'FNFVD00T'    CMZFLD
018200951207     C                   EXSR      NUMREC
018300951207     C                   Z-ADD     NRR           RECFVD           10 0
018400951207     C* VEDO SE CONTIENE RECORD FNFV400T
018500951207     C                   MOVEL     'FNFV400T'    CMZFLD
018600951207     C                   EXSR      NUMREC
018700951207     C                   Z-ADD     NRR           RECFV4           10 0
018800121102     C* VEDO SE CONTIENE RECORD FNFGS00T
018900121102     C                   MOVEL     'FNFGS00T'    CMZFLD
019000121102     C                   EXSR      NUMREC
019100121102     C                   Z-ADD     NRR           RECFGS           10 0
019200951207     C*
019300951207     C* I N V I O
019400960122    3C     RECFGV        IFGT      0
019500980218     C     RECFGV        ANDNE     *HIVAL
019600980218     C     RECFVD        ORGT      0
019700980218     C     RECFGV        ANDNE     *HIVAL
019800951207     C     RECFV4        ORGT      0
019900980218     C     RECFGV        ANDNE     *HIVAL
020000951207     C                   MOVE      '046'         CMZDST
020100951207     C                   MOVE      'S'           CMZURG
020200951207     C                   MOVE      *BLANK        CMZPRR
020300951207     C                   MOVE      *BLANK        CMZAZN
020400951207     C*
020500951207     C                   EXSR      X25FV4
020600951207     C                   EXSR      X25FVD
020700000427     C                   EXSR      X25FGW
020800121102     C                   EXSR      X25FGs
020900000427     C                   EXSR      X25FGV
021000960122    3C                   ENDIF
021100960125    2C                   ENDIF
021200960122     C*
021300960122     C** DISALLOCO FNFV400T
021400021008     C                   Z-ADD     42            LUNG
021500960125    2C     *IN59         IFEQ      *OFF
021600960122     C                   MOVEA     'FNFV400T'    C57(13)
021700021008     C                   MOVEA     VAR           C57(34)
021800960122     C                   MOVEL     *BLANKS       COMMAN           80
021900960122     C                   MOVEA     C57           COMMAN
022000980218     C                   CALL      'QCMDEXC'                            30
022100960122     C                   PARM                    COMMAN
022200960122     C                   PARM                    LUNG
022300960125    2C                   ENDIF
022400960125     C*
022500980218     C** DISALLOCO FNFVD00T
022600980218    2C     *IN56         IFEQ      *OFF
022700980218     C                   MOVEA     'FNFVD00T'    C57(13)
022800021008     C                   MOVEA     VAR           C57(34)
022900980218     C                   MOVEL     *BLANKS       COMMAN           80
023000980218     C                   MOVEA     C57           COMMAN
023100980218     C                   CALL      'QCMDEXC'                            30
023200980218     C                   PARM                    COMMAN
023300980218     C                   PARM                    LUNG
023400980218    2C                   ENDIF
023500021008     C** DISALLOCO FNFGW00T
023600021008    2C     *IN57         IFEQ      *OFF
023700021008     C                   MOVEA     'FNFGW00T'    C57(13)
023800021008     C                   MOVEA     VAR           C57(34)
023900021008     C                   MOVEL     *BLANKS       COMMAN           80
024000021008     C                   MOVEA     C57           COMMAN
024100021008     C                   CALL      'QCMDEXC'                            30
024200021008     C                   PARM                    COMMAN
024300021008     C                   PARM                    LUNG
024400021008    2C                   ENDIF
024500121102     C** DISALLOCO FNFGs00T
024600121102    2C     *IN58         IFEQ      *OFF
024700121102     C                   MOVEA     'FNFGS00T'    C57(13)
024800121102     C                   MOVEA     VAR           C57(34)
024900121102     C                   MOVEL     *BLANKS       COMMAN           80
025000121102     C                   MOVEA     C57           COMMAN
025100121102     C                   CALL      'QCMDEXC'                            30
025200121102     C                   PARM                    COMMAN
025300121102     C                   PARM                    LUNG
025400121102    2C                   ENDIF
025500980218     C** DISALLOCO FNFGV01T
025600960125    2C     *IN55         IFEQ      *OFF
025700960122     C                   MOVEA     'FNFGV01T'    C57(13)
025800021008     C                   MOVEA     VAR           C57(34)
025900960122     C                   MOVEL     *BLANKS       COMMAN           80
026000960122     C                   MOVEA     C57           COMMAN
026100980218     C                   CALL      'QCMDEXC'                            30
026200951207     C                   PARM                    COMMAN
026300951207     C                   PARM                    LUNG
026400960125    2C                   ENDIF
026500960122     C*
026600960125     C* 40 ON  - RITENTARE PIU' TARDI PERCHE' NON E' RIUSCITA
026700960125     C*   L'ALLOCAZIONE DI TUTTI I MEMBRI
026800960125    2C     *IN40         IFEQ      *ON
026900960129     C*
027000960129     C* SE RICHIAMATO DA GESTIONE F.VIAGGIO E NON SONO AVVENUTE LE
027100960129     C*   ALLOCAZIONI DI TUTTI I FILES INVIO MESSAGGIO IN SEDE
027200051121    3c     PARFLG        IFEQ      'M'
027300051121     c* Tento 3 volte e poi do il messaggio
027400051121     C                   select
027500051121    4c                   when      parall=' '
027600051121     c                   eval      parall='1'
027700051121     C                   MOVEL     PARAM         KPJBU
027800051121     C                   MOVEL     'LSA4'        KCOAZ
027900051121     C                   CALL      'BCH10'
028000051121     C                   PARM                    KPJBA
028100051121     c
028200051121     c                   when      parall='1'
028300051121     c                   eval      parall='2'
028400051121     C                   MOVEL     PARAM         KPJBU
028500051121     C                   MOVEL     'LSA4'        KCOAZ
028600051121     C                   CALL      'BCH10'
028700051121     C                   PARM                    KPJBA
028800051121     c
028900051121     c                   when      parall='2'
029000051121     c                   eval      parall='3'
029100051121     C                   MOVEL     PARAM         KPJBU
029200051121     C                   MOVEL     'LSA4'        KCOAZ
029300051121     C                   CALL      'BCH10'
029400051121     C                   PARM                    KPJBA
029500051121     c
029600051121     c                   when      parall='3'
029700111103     C                   movel     *blanks       msgtes
029800111103     C                   movel     *blanks       msgdet
029900960129     C                   MOVEL     MSG(1)        MSGTES
030000960201     C                   MOVEL(P)  MSG(2)        MSGDET
030100960129     C                   MOVE      PARLNP        MSLNP
030200120302     C                   eval      wrkEml= 'ced@brt.it'
030300111103
030400111103     C                   call(e)   'TIS701C'
030500111103     C                   parm                    wrkEml          253
030600111103     C                   PARM                    MSGTES           44
030700111103     C                   PARM                    MSGDET
030800111103     c
030900051121     c                   other
031000051121     C                   MOVEL     '1'           PARALL
031100051121     C                   MOVEL     PARAM         KPJBU
031200051121    4C                   ENDsl
031300051121    3C                   ENDIF
031400960122    2C                   ENDIF
031500960122    1C                   ENDIF
031600951207     C*
031700951207     C* CHIUSURA PGM
031800960122    1C     PARTLA        IFEQ      ' '
031900951207     C                   RETURN
032000960122   X1C                   ELSE
032100951207     C                   SETON                                        LR
032200960122    1C                   ENDIF
032300951207     C*
032400951207     C**************************************************************************
032500951207     C*  SCRIVO MEMBRO FILE FOGLI VIAGGIO PER LA SEDE
032600951207     C**************************************************************************
032700951207     C     WTRFV         BEGSR
032800951207     C*
032900951207     C* APERTURA MEMBRI
033000951207     C                   EXSR      CAEXFV
033100951207     C*
033200960125    1C     *IN55         IFEQ      *OFF
033300000427    1C     *IN57         ANDEQ     *OFF
033400121105    1C     *IN58         ANDEQ     *OFF
033500960125     C*
033600960122     C** SCRITTURA FNFGV01T
033700021009     C     SIMFEL        SETLL     FNFGV46L
033800021009     C     SIMFEL        READE     FNFGV46L                               31
033900000427     C*
034000960125    2C     *IN31         DOWEQ     *OFF
034100000427     C* ESTENSIONE FOGLI
034200021009     C     KFGW          CHAIN     FNFGW01L                           35
034300121102     C     KFGW          CHAIN     FNFGs01L
034400000427     C**
034500960125     C* SE PRESENTE REPERISCO LA CAUSALE RITARDO PARTENZA
034600960125     C                   CLEAR                   FGVRTP
034700960122     C     KFV2          CHAIN     FNFV201L                           30
034800960125    3C     *IN30         IFEQ      *OFF
034900960122     C                   MOVEL     FV2RTC        FGVRTP
035000960122     C                   MOVEL     'T'           FV2FTR
035100960122     C                   Z-ADD     NUMTRA        FV2DTR
035200960122     C                   UPDATE    FNFV2000
035300960125    3C                   ENDIF
035400960919     C*
035500951207     C                   WRITE     FNFGV0TT
035600000427     C  N35              WRITE     FNFGW0TT
035700121102     c                   if        %found(fnfgs01l)
035800121102     C                   WRITE     FNFGs0TT
035900121102     c                   endif
036000130515     c
036100130515     c* Aggiorno in cellulare del conducente sull'anagrafica degli acreditati
036200130515     c*  se la trovo
036300130515     C                   MOVEL     'P'           KEPA
036400130515     C                   MOVEL     'L'           KTRC
036500130515     C                   CLEAR                   KTFA
036600130515     C     KFV4          CHAIN     FNFV401L
036700130515     c                   if        %found(fnfv401l) and fv4not<>*blanks
036800130515     c                   movel     fv4not        kcnc
036900130515     c                   eval      kdcn=fgvpmb
037000130515     c                   eval      kpdr=fgvpdr
037100130515     c     kata          chain     aiata01l
037200130515     c                   if        %found(aiata01l) and atatel<>kcnc
037300130515     c                   eval      atatel=kcnc
037400130515     c                   update    aiata000
037500130515     c                   else
037600130515     c                   unlock    aiata01l
037700130515     c                   endif
037800130515     c                   endif
037900000427     C*
038000951207     C                   DELETE    FNFGV000
038100951207     C*
038200021009     C     SIMFEL        READE     FNFGV46L                               31
038300960125    2C                   ENDDO
038400980218     C* CHIUSURA MEMBRO
038500960125     C                   CLOSE     FNFGV01T
038600000427     C                   CLOSE     FNFGW00T
038700121102     C                   CLOSE     FNFGs00T
038800960125    1C                   ENDIF
038900980218     C*
039000980218    1C     *IN56         IFEQ      *OFF
039100980218     C** SCRITTURA DI FNFVD00T
039200100316     c* Elaboro per terminal per evitare problemi in trasmissione
039300100316     C     kfvd          SETLL     FNFVD02L
039400100316     C     kfvd          READE     FNFVD02L                               32
039500980218    3C     *IN32         DOWEQ     *OFF
039600980218     C                   WRITE     FNFVD0TT
039700980218     C                   MOVEL     'T'           FVDFTR
039800980218     C                   Z-ADD     NUMTRA        FVDDTR
039900980218     C                   UPDATE    FNFVD000
040000100316     C     kfvd          READE     FNFVD02L                               32
040100980218    3C                   ENDDO
040200980218     C                   CLOSE     FNFVD00T
040300980218    1C                   ENDIF
040400960122     C*
040500960122     C** SCRITTURA FNFV400T
040600960125    1C     *IN59         IFEQ      *OFF
040700960125     C*
040800021106     C* Elaboro solo i record con "P-PARTENZA",
040900021106     C*                           Linea di Invio F.V. = 0
041000021106     C*                           Linea di Partenza = simfel
041100021106     C     K03FV403      SETLL     FNFV403L
041200021106     C     K03FV403      READE     FNFV403L                               31
041300960125    2C     *IN31         DOWEQ     *OFF
041400021106     C* Trasmetto solo i record con Flag Trasmissione a Sede vuoto
041500021106    3C     FV4FT2        IFEQ      ' '
041600960125     C*
041700960919     C* AGGANCIO IL FOGLIO VIAGGIO PER VEDERE SE I RECORD DI FNFV4
041800960919     C*   APPARTENGONO A UN FV DA TRASMETTERE IN SEDE
041900960919     C                   EXSR      CTRFGV
042000960919     C*
042100960919     C* 12 ON - IL FV E' DA TRASMETTERE IN SEDE QUINDI ANCHE I RELATIVI
042200960919     C*  RECORD DI FNFV4 SONO DA TRASMETTERE
042300960919    4C     *IN12         IFEQ      *ON
042400960125     C* IMPOSTO DATA E ORA VARIAZIONE CON UDATE E UTIME
042500130503     c                   if        fgvfcf<>' '
042600960125     C                   Z-ADD     NUMTRA        FV4DTV
042700960125     C                   Z-ADD     UTIME         FV4ORV
042800960122     C                   WRITE     FNFV40TT
042900960122     C*
043000960125     C                   MOVEL     'T'           FV4FT2
043100130503     C                   Z-ADD     NUMTRA        FV4DT2
043200130503     c
043300130503     c                   endif
043400130215   x4c                   else
043500130215     C                   MOVEL     'N'           FV4FT2
043600130503     C                   Z-ADD     NUMTRA        FV4DT2
043700960919    4C                   ENDIF
043800130215    c
043900130215     C                   UPDATE    FNFV4000
044000960125    3C                   ENDIF
044100960122     C*
044200021106     C     K03FV403      READE     FNFV403L                               31
044300960125    2C                   ENDDO
044400960125     C*
044500960125     C* CHIUSURA MEMBRI
044600960125     C                   CLOSE     FNFV400T
044700960125    1C                   ENDIF
044800951207     C*
044900951207     C                   ENDSR
045000951207     C**************************************************************************
045100951207     C*  ESEGUE CALL QCAEXEC PER OVRDBF SUI FILE
045200951207     C**************************************************************************
045300951207     C     CAEXFV        BEGSR
045400021008     C                   MOVEL     PARMBD        VAR8
045500021008     C                   MOVE      ')'           VAR8              8
045600951207     C                   Z-ADD     46            LUNG
045700951207     C*
045800980218     C** OVRDBF FNFGV01T
045900960125    1C     *IN55         IFEQ      *OFF
046000000427     C     *IN57         ANDEQ     *OFF
046100121102     C     *IN58         ANDEQ     *OFF
046200960122     C                   MOVEA     'FNFGV01T'    C17(13)
046300021008     C                   MOVEA     VAR8          C17(38)
046400951207     C                   MOVEL     *BLANKS       COMMAN
046500951207     C                   MOVEA     C17           COMMAN
046600951207     C                   CALL      'QCMDEXC'
046700951207     C                   PARM                    COMMAN
046800951207     C                   PARM                    LUNG
046900980218     C* APERTURA FILE
047000980218     C                   OPEN      FNFGV01T
047100121102     C** OVRDBF
047200000427     C                   MOVEA     'FNFGW00T'    C17(13)
047300021008     C                   MOVEA     VAR8          C17(38)
047400000427     C                   MOVEL     *BLANKS       COMMAN
047500000427     C                   MOVEA     C17           COMMAN
047600000427     C                   CALL      'QCMDEXC'
047700000427     C                   PARM                    COMMAN
047800000427     C                   PARM                    LUNG
047900000427     C* APERTURA FILE
048000000427     C                   OPEN      FNFGW00T
048100121102     C** OVRDBF
048200121102     C                   MOVEA     'FNFGS00T'    C17(13)
048300121102     C                   MOVEA     VAR8          C17(38)
048400121102     C                   MOVEL     *BLANKS       COMMAN
048500121102     C                   MOVEA     C17           COMMAN
048600121102     C                   CALL      'QCMDEXC'
048700121102     C                   PARM                    COMMAN
048800121102     C                   PARM                    LUNG
048900121102     C* APERTURA FILE
049000121102     C                   OPEN      FNFGS00T
049100980218    1C                   ENDIF
049200951207     C*
049300960122     C** OVRDBF FNFVD00T
049400980218     C     *IN56         IFEQ      *OFF
049500951207     C                   MOVEA     'FNFVD00T'    C17(13)
049600021008     C                   MOVEA     VAR8          C17(38)
049700951207     C                   MOVEL     *BLANKS       COMMAN
049800951207     C                   MOVEA     C17           COMMAN
049900951207     C                   CALL      'QCMDEXC'
050000951207     C                   PARM                    COMMAN
050100951207     C                   PARM                    LUNG
050200980218     C* APERTURA FILE
050300980218     C                   OPEN      FNFVD00T
050400980218    1C                   ENDIF
050500960125     C*
050600960125     C*
050700960122     C** OVRDBF FNFV400T
050800960125    1C     *IN59         IFEQ      *OFF
050900960122     C                   MOVEA     'FNFV400T'    C17(13)
051000021008     C                   MOVEA     VAR8          C17(38)
051100960122     C                   MOVEL     *BLANKS       COMMAN
051200960122     C                   MOVEA     C17           COMMAN
051300960122     C                   CALL      'QCMDEXC'
051400960122     C                   PARM                    COMMAN
051500960122     C                   PARM                    LUNG
051600951207     C*
051700951207     C* APERTURA FILE
051800951207     C                   OPEN      FNFV400T
051900960125    1C                   ENDIF
052000951207     C*
052100951207     C                   ENDSR
052200960919     C**************************************************************************
052300960919     C*  CONTROLLO SE FV DA TRASMETTERE IN SEDE
052400960919     C**************************************************************************
052500960919     C     CTRFGV        BEGSR
052600960919     C*
052700960919     C* AGGANCIO IL FOGLIO VIAGGIO SOLO QUANDO CAMBIA
052800960919    0C     WNFV          IFNE      FV4NFV
052900960919     C                   SETOFF                                       12
053000960919     C*
053100960919     C     KFGV          CHAIN     FNFGV01L                           32
053200960919    1C     *IN32         IFEQ      *OFF
053300960919     C*
053400960919     C* SOLO A CAMBIO TIPO TRAINO CONTROLLO SE IL FV E' DA TRASMETTERE
053500960919     C*   IN SEDE
053600960919    2C     WTTR          IFNE      FGVTTR
053700130515     C                   MOVEL     'TV'          COD
053800960919     C                   MOVEL(P)  FGVTTR        KEY
053900960919     C     KTAB          CHAIN     TABEL                              33
054000960919    3C     *IN33         IFEQ      *OFF
054100960919     C     TBLFLG        ANDEQ     ' '
054200960919     C                   MOVEL     TBLUNI        DSTV
054300960919   X3C                   ELSE
054400960919     C                   MOVEL     'S'           §TVUTC
054500960919    3C                   ENDIF
054600960919     C                   MOVEL     FGVTTR        WTTR
054700960919    2C                   ENDIF
054800960919     C*
054900960919     C* IL FOGLIO VIAGGIO E' DA INVIARE IN SEDE SE:
055000960919     C*   - IL FOGLIO NON E' ABBINATO
055100960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' VUOTO
055200960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO ED E'
055300960919     C*     DA UTILIZZARE NEI CONTROLLI DI SEDE (§TVUTC = "S")
055400960919     C*   - IL FOGLIO     E' ABBINATO E IL TIPO TRAINO E' PIENO MA NON
055500960919     C*     C'E' IL RECORD IN TABELLA
055600960919    2C     FGVTRN        IFEQ      0
055700960919     C     FGVTTR        OREQ      ' '
055800960919     C     §TVUTC        OREQ      'S'
055900960919     C                   SETON                                        12
056000960919    2C                   ENDIF
056100960919     C*
056200960919    1C                   ENDIF
056300960919     C                   MOVEL     FV4NFV        WNFV
056400960919    0C                   ENDIF
056500960919     C*
056600960919     C                   ENDSR
056700960122     C**************************************************************************
056800960122     C*  SOTTOMETTE TRASMISSIONE   F N F V 4 0 0 T
056900960122     C**************************************************************************
057000960122     C     X25FV4        BEGSR
057100960122     C*
057200960122     C     RECFV4        IFGT      0
057300980218     C     RECFV4        ANDNE     *HIVAL
057400960122     C                   MOVEL     'FNFV400T'    CMZFLD
057500960122     C                   MOVEL     'FNFV400R'    CMZFLA
057600960129     C                   MOVE      'TR41'        CMZAZN
057700960122     C                   MOVE      RECFV4        CMZNRR
057800960122     C*
057900960122     C                   EXSR      CALCMZ
058000960122     C                   ENDIF
058100960122     C*
058200960122     C                   ENDSR
058300951207     C**************************************************************************
058400951207     C*  SOTTOMETTE TRASMISSIONE   F N F V D 0 0 T
058500951207     C**************************************************************************
058600951207     C     X25FVD        BEGSR
058700951207     C*
058800980218     C     RECFVD        IFGT      0
058900980218     C     RECFVD        ANDNE     *HIVAL
059000980218     C     RECFGV        IFEQ      0
059100980218     C     RECFGV        OREQ      *HIVAL
059200980218     C                   MOVEL     'TR40'        CMZAZN
059300980218     C                   ELSE
059400960212     C                   MOVE      '    '        CMZAZN
059500980218     C                   END
059600951207     C                   MOVEL     'FNFVD00T'    CMZFLD
059700951207     C                   MOVEL     'FNFVD00R'    CMZFLA
059800951207     C                   MOVE      RECFVD        CMZNRR
059900951207     C*
060000951207     C                   EXSR      CALCMZ
060100951207     C                   ENDIF
060200951207     C*
060300951207     C                   ENDSR
060400951207     C**************************************************************************
060500960130     C*  SOTTOMETTE TRASMISSIONE   F N F G V 0 1 T
060600951207     C**************************************************************************
060700960122     C     X25FGV        BEGSR
060800951207     C*
060900960122     C     RECFGV        IFGT      0
061000980218     C     RECFGV        ANDNE     *HIVAL
061100960122     C                   MOVEL     'FNFGV01T'    CMZFLD
061200960122     C                   MOVEL     'FNFGV01R'    CMZFLA
061300960130     C                   MOVE      'TR40'        CMZAZN
061400960122     C                   MOVE      RECFGV        CMZNRR
061500951207     C*
061600951207     C                   EXSR      CALCMZ
061700951207     C                   ENDIF
061800951207     C*
061900951207     C                   ENDSR
062000000427     C**************************************************************************
062100000427     C*  SOTTOMETTE TRASMISSIONE   F N F G W 0 0 T
062200000427     C**************************************************************************
062300000427     C     X25FGW        BEGSR
062400000427     C*
062500000427     C     RECFGW        IFGT      0
062600000427     C     RECFGW        ANDNE     *HIVAL
062700000427     C                   MOVEL     'FNFGW00T'    CMZFLD
062800000427     C                   MOVEL     'FNFGW00R'    CMZFLA
062900000427     C                   MOVE      '    '        CMZAZN
063000000428     C                   MOVE      RECFGW        CMZNRR
063100000427     C*
063200000427     C                   EXSR      CALCMZ
063300000427     C                   ENDIF
063400000427     C*
063500000427     C                   ENDSR
063600121102     C**************************************************************************
063700121102     C*  SOTTOMETTE TRASMISSIONE   F N F G S 0 0 T
063800121102     C**************************************************************************
063900121102     C     X25FGS        BEGSR
064000121102     C*
064100121102     C     RECFGS        IFGT      0
064200121102     C     RECFGS        ANDNE     *HIVAL
064300121102     C                   MOVEL     'FNFGS00T'    CMZFLD
064400121102     C                   MOVEL     'FNFGS00R'    CMZFLA
064500121102     C                   MOVE      '    '        CMZAZN
064600121102     C                   MOVE      RECFGS        CMZNRR
064700121102     C*
064800121102     C                   EXSR      CALCMZ
064900121102     C                   ENDIF
065000121102     C*
065100121102     C                   ENDSR
065200951207     C**************************************************************************
065300951207     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
065400951207     C**************************************************************************
065500951207     C     NUMREC        BEGSR
065600951207     C*
065700951207     C                   Z-ADD     0             NRR
065800951207     C                   CALL      'TRUL51C'
065900951207     C                   PARM                    KNSIF
066000951207     C                   PARM                    CMZFLD           10
066100951207     C                   PARM                    PARMBD
066200951207     C                   PARM                    NRR              10 0
066300951207     C*
066400951207     C                   ENDSR
066500951207     C**************************************************************************
066600960122     C*  CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSIONE
066700951207     C**************************************************************************
066800951207     C     CALCMZ        BEGSR
066900951207     C*
067000000427     C                   CLEAR                   W236
067100021010     C****               CLEAR                   CMZLCL
067200021008     C                   CALL      'TIBS53C'
067300000427     C                   PARM                    CMZLCL            1
067400951207     C                   PARM                    CMZDST
067500951207     C                   PARM                    CMZFLD
067600951207     C                   PARM                    CMZMBD
067700951207     C                   PARM                    CMZLBD
067800951207     C                   PARM                    CMZFLA
067900951207     C                   PARM                    CMZMBA
068000951207     C                   PARM                    CMZLBA
068100951207     C                   PARM                    CMZORI
068200951207     C                   PARM                    CMZDTI
068300951207     C                   PARM                    CMZURG
068400951207     C                   PARM                    CMZADD
068500951207     C                   PARM                    CMZCLR
068600951207     C                   PARM                    CMZFTR
068700951207     C                   PARM                    CMZERR
068800951207     C                   PARM                    CMZAZN
068900951207     C                   PARM                    CMZSYF
069000951207     C                   PARM                    CMZNRR
069100951207     C                   PARM                    CMZPRR
069200000427     C                   PARM                    W236            236
069300951207     C                   ENDSR
069400951207     C**************************************************************************
069500951207     C*  SR INIZIALE
069600951207     C**************************************************************************
069700951207     C     *INZSR        BEGSR
069800951213     C*
069900971117     C                   MOVEL     KPJBU         PARAM
070000971117     C*
070100971113     C*  ESEGUO CALL PER REPERIRE LIBRERIA E S.I. DI TRAMSISSIONE
070200021008     C                   CLEAR                   TIBS55DS
070300971125     C                   MOVEL     'L'           I50TLA
070400021010     C                   MOVEL     simfel        I50Ppo
070500971113     C                   MOVEL     046           I50APO
070600021008     C                   CALL      'TIBS55R'
070700021008     C                   PARM                    TIBS55DS
070800971113     C**
070900021010     C                   MOVEL     O50lcl        CMZLcl
071000021010     C                   MOVEL     O50PLA        CMZLBD
071100971113     C                   MOVEL     O50ALA        CMZLBA
071200971113     C                   MOVEL     O50ASI        CMZSYF
071300951213     C                   MOVE      '0000'        CMZORI
071400951213     C                   MOVE      '0000000'     CMZDTI
071500951213     C                   MOVE      'A'           CMZADD
071600951213     C                   MOVE      'S'           CMZCLR
071700951213     C                   MOVE      'S'           CMZFTR
071800951213     C                   MOVE      *BLANK        CMZERR
071900960122     C* IMPOSTO CAMPI PER AGGANCIARE FNFV201L
072000960122     C                   MOVEL     'P'           WEPA
072100960122     C                   MOVEL     'R'           WTDH
072200021106     C                   clear                   Wzero
072300951213     C*
072400951213     C****  CONTROLLO ESISTENZA MEMBRI  ****
072500960122     C                   MOVEL     'FNFGV01T'    FILE
072600951213     C                   CALL      'TRUL50C'
072700951213     C                   PARM                    FILE
072800971114     C                   PARM                    PARMBD
072900951213     C                   PARM                    KNSIF
073000951213     C                   PARM                    LOGICO           10
073100951213     C                   PARM                    LOGMBR           10
073200951213     C                   PARM                    FLG               1
073300951213     C*
073400000428     C                   MOVEL     'FNFGW00T'    FILE
073500000428     C                   CALL      'TRUL50C'
073600000428     C                   PARM                    FILE
073700000428     C                   PARM                    PARMBD
073800000428     C                   PARM                    KNSIF
073900000428     C                   PARM                    LOGICO           10
074000000428     C                   PARM                    LOGMBR           10
074100000428     C                   PARM                    FLG               1
074200121102     C*
074300121102     C                   MOVEL     'FNFGS00T'    FILE
074400121102     C                   CALL      'TRUL50C'
074500121102     C                   PARM                    FILE
074600121102     C                   PARM                    PARMBD
074700121102     C                   PARM                    KNSIF
074800121102     C                   PARM                    LOGICO           10
074900121102     C                   PARM                    LOGMBR           10
075000121102     C                   PARM                    FLG               1
075100000428     C*
075200951213     C                   MOVEL     'FNFVD00T'    FILE
075300951213     C                   CALL      'TRUL50C'
075400951213     C                   PARM                    FILE             10
075500971114     C                   PARM                    PARMBD
075600951213     C                   PARM                    KNSIF
075700951213     C                   PARM                    LOGICO           10
075800951213     C                   PARM                    LOGMBR           10
075900951213     C                   PARM                    FLG               1
076000951213     C*
076100951213     C                   MOVEL     'FNFV400T'    FILE
076200951213     C                   CALL      'TRUL50C'
076300951213     C                   PARM                    FILE
076400971114     C                   PARM                    PARMBD
076500951213     C                   PARM                    KNSIF
076600951213     C                   PARM                    LOGICO           10
076700951213     C                   PARM                    LOGMBR           10
076800951213     C                   PARM                    FLG               1
076900951207     C*
077000951207     C* NUMERO TRASMISSIONE = DATA DEL GIORNO (AAAA/MM/GG)
077100951207     C                   TIME                    WTIME            14 0
077200960125     C                   MOVEL     WTIME         UTIME             6 0
077300951207     C                   MOVE      WTIME         UDATE8            8 0
077400951207     C                   Z-ADD     UDATE8        WDATA
077500951207     C                   Z-ADD     GG            MMGG              4 0
077600951207     C                   MOVEL     MM            MMGG
077700951207     C                   Z-ADD     MMGG          NUMTRA            8 0
077800951207     C                   MOVEL     AA            NUMTRA
077900960122     C*
078000960122     C****  KLIST  ****
078100960122     C* ACCESSO FNFV201L
078200960122     C     KFV2          KLIST
078300960122     C                   KFLD                    FGVLNP
078400960122     C                   KFLD                    FGVNFV
078500960122     C                   KFLD                    WEPA
078600960122     C                   KFLD                    WZERO             3 0
078700960122     C                   KFLD                    WTDH
078800960919     C* ACCESSO FNFGV01L
078900960919     C     KFGV          KLIST
079000960919     C                   KFLD                    FV4NFV
079100960919     C                   KFLD                    FV4LNP
079200021009     C     KFGW          KLIST
079300021009     C                   KFLD                    FGVNFV
079400021009     C                   KFLD                    FGVLNP
079500100316     C     KFVD          KLIST
079600100316     C                   KFLD                    kftr
079700100316     C                   KFLD                    simfel
079800100316     c                   clear                   kftr              1
079900021106     C* Accesso FNFV403L
080000021106     C     K03FV403      klist
080100021106     C                   kfld                    Wepa
080200021106     C                   kfld                    Wzero
080300021106     C                   kfld                    SIMfel
080400130515     C     KFV4          KLIST
080500130515     C                   KFLD                    fgvlnp
080600130515     C                   KFLD                    fgvnfv
080700130515     C                   KFLD                    KEPA
080800130515     C                   KFLD                    KTFA
080900130515     C                   KFLD                    KTRC
081000130515     c     kata          KLIST
081100130515     c                   kfld                    soctraini
081200130515     c                   kfld                    kpdr
081300130515     c                   kfld                    kdcn
081400960919     C* ACCESSO TABEL00F
081500960919     C     KTAB          KLIST
081600960919     C                   KFLD                    CODUT
081700960919     C                   KFLD                    COD
081800960919     C                   KFLD                    KEY
081900960122     C*
082000960122     C****  DEFINIZIONE CAMPI  ****
082100960122     C     *LIKE         DEFINE    FV2EPA        WEPA
082200960122     C     *LIKE         DEFINE    FV2TDH        WTDH
082300960919     C     *LIKE         DEFINE    TBLKUT        CODUT
082400960919     C     *LIKE         DEFINE    TBLCOD        COD
082500960919     C     *LIKE         DEFINE    TBLKEY        KEY
082600960919     C     *LIKE         DEFINE    FGVTTR        WTTR
082700960919     C     *LIKE         DEFINE    FV4NFV        WNFV
082800130515     c
082900130515     C* IMPOSTO CAMPI PER AGGANCIARE TABEL00F
083000130515     c* Reperisco società dei trazionisti
083100130515     C                   Z-ADD     1             CODUT
083200130515     C                   MOVEL     '3I'          COD
083300130515     C                   MOVEL(P)  '1'           KEY
083400130515     C     KTAB          CHAIN     TABEL                              83
083500130515     C     *IN83         IFEQ      *OFF
083600130515     c                   movel     tbluni        ds3i
083700130515     c                   eval      alfasoc=%subst(§3ibst:7:3)
083800130515     c                   if        alfasoc>*zeros
083900130515     c                   EVAL      soctraini=%int(alfasoc)
084000130515     c                   endif
084100130515     c                   endif
084200130515     c*
084300130515     c*
084400951207     C                   ENDSR
084500951207     C**************************************************************************
084600971204**         C17
084700021008OVRDBF FILE(XXXXXXXX)            MBR(M000000)
084800951207**         C56
084900021008ALCOBJ OBJ((XXXXXXXX *FILE *EXCL MXXXyyy))
085000951207**         C57
085100021008DLCOBJ OBJ((XXXXXXXX *FILE *EXCL M000000))
085200960129** MSG
085300111103FNLSA4R:ERRORE TRASMISSIONE F.VIAGGIO PART
085400971204A SEDE: NON TRASMESSO FOGLIO VIAGGIO DALLA LINEA XXX
