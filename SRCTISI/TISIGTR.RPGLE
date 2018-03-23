000100000727     H DATEDIT(*YMD)
000200030128     F* tisigtR   *---------------------------------------------------*
000300931221     F*------------------*                                            *
000400030128     F*  TRASMISSIONE gestione bancali      DA FILIALE A SEDE.        *
000500931221     F*---------------------------------------------------------------*
000600030128     Fwfmgb01L  UF   E           K DISK
000700030128     Fwfmgb00T  O  A E             DISK    USROPN
000800030128     F                                     RENAME(wfmgb000:wfmgbT)
000900020805     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
001000020805     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)
001100020805     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)
001200000727     D DSCMZ         E DS
001300931221     D KPJBA         E DS
001400950427     D*
001500931215     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600000727     D CNCR80        E DS
001700020805     D DSBS50        E DS                  EXTNAME(TIBS55DS)
001800940121     C*****************************************************************
001900940121     C*    RIEPILOGO INDICATORI
002000940121     C*****************************************************************
002100940121     C* 20    - DI COMODO
002200940121     C* 30    - DI COMODO
002300940121     C*****************************************************************
002400931221     C     *ENTRY        PLIST
002500931221     C                   PARM                    KPJBA
002600000727     C*
002700931221     C                   Z-ADD     1             CODUT
002800931215     C                   CALL      'X§PARUT'
002900931215     C                   PARM                    UT§DSE
003000000727     C                   MOVEL     REC80         CNCR80
003100980327     C**
003200980327     C*  ESEGUO CALL PER REPERIRE LIBRERIA E S.I. DI TRAMSISSIONE
003300980327     C                   CLEAR                   DSBS50
003400000727     C                   MOVEL     KNSIF         I50PSI
003500020805     C                   Z-ADD     SIMFEL        I50PPO
003600020805     C                   Z-ADD     46            I50APO
003700000727     C                   MOVEL     'L'           I50TLA
003800020805     C                   CALL      'TIBS55R'
003900980327     C                   PARM                    DSBS50
004000020805     c                   move      simfel        com03             3
004100931221     C*---------------------------------------------------------------*
004200000727     C*
004300931215     C* APERTURA FILE DA TRASMETTERE
004400931221     C                   EXSR      CAEX
004500931221     C*
004600931215     C* AZZERO CONTATORI
004700030128     C                   Z-ADD     0             WFM               7 0
004800931221     C*
004900940121     C* E L A B O R O
005000931221     C                   EXSR      LEGGI
005100030128     C*
005200030128     C                   CLOSE     wfmgb00T
005300930118     C*
005400030128     C* TRASMETTO  wfmgb00T
005500030128     C                   EXSR      X25WFM
005600931215     C*
005700931215     C                   SETON                                        LR
005800000727     C*
005900930118     C**-------------------------------------------------------------**
006000000727     C**  ESEGUE QCMDEXC DI APERTURA SUI MEMBRI                      **
006100930118     C**-------------------------------------------------------------**
006200931221     C     CAEX          BEGSR
006300020805     C                   Z-ADD     48            LUNG             15 5
006400940120     C******
006500030128     C***  w f m g b 0 0 T
006600940120     C******
006700930118     C** CONTROLLO SE ESISTE IL MEMBRO
006800901002     C                   SETOFF                                       20
006900901002     C                   MOVEL     *BLANKS       COMMAN
007000901002     C                   MOVEA     CMD3(1)       COMMAN           80
007100020805     C                   eval      %subst(comman: 42: 3) = com03
007200901002     C                   CALL      'QCMDEXC'                            20
007300901002     C                   PARM                    COMMAN
007400901002     C                   PARM                    LUNG
007500901002     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
007600901002     C   20              DO
007700901002     C                   MOVEL     *BLANKS       COMMAN
007800901002     C                   MOVEA     CMD1(1)       COMMAN
007900020805     C                   eval      %subst(comman: 42: 3) = com03
008000901002     C                   CALL      'QCMDEXC'
008100901002     C                   PARM                    COMMAN
008200901002     C                   PARM                    LUNG
008300931215     C                   ENDDO
008400930118     C** ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
008500901002     C                   MOVEL     *BLANKS       COMMAN
008600901002     C                   MOVEA     CMD2(1)       COMMAN
008700020805     C                   eval      %subst(comman: 42: 3) = com03
008800901002     C                   CALL      'QCMDEXC'
008900901002     C                   PARM                    COMMAN
009000901002     C                   PARM                    LUNG
009100030128     C                   OPEN      wfmgb00T
009200940120     C*
009300931221     C                   ENDSR
009400000727     C*
009500931221     C**-------------------------------------------------------------**
009600940124     C**  ROUTINE DI LETTURA E SCRITTURA                             **
009700931221     C**-------------------------------------------------------------**
009800931221     C     LEGGI         BEGSR
009900940120     C******
010000030128     C***  w f m g b 0 0 F
010100940120     C******
010200030128     C     *LOVAL        SETLL     wfmgb000
010300030128     C                   READ      wfmgb000                               30
010400931221     C*
010500940120    1C     *IN30         DOWEQ     *OFF
010600000727     C*
010700000727     C* CONSIDERO SOLO QUELLI DA TRASMETTERE
010800000727     C*
010900030128     C                   MOVE      *DATE         wfmdtr
011000030128     C                   MOVE      'T'           wfmftr
011100030128     C                   WRITE     wfmgbT
011200000727      *                    -----------
011300030128     C                   ADD       1             wfm
011400030128     C                   delete    wfmgb000
011500931221     C*
011600030128     C  N30              READ      wfmgb000                               30
011700940120    1C                   ENDDO
011800931221     C                   ENDSR
011900000727     C*
012000930118     C**-------------------------------------------------------------**
012100030128     C**  SOTTOMETTE TRASMISSIONE wfmgb                              **
012200930118     C**-------------------------------------------------------------**
012300030128     C     X25WFM        BEGSR
012400000727     C                   MOVEL     O50LCL        CMZLCL            1
012500000727     C                   MOVE      *BLANKS       W236            236
012600000727     C                   MOVEL     '046'         CMZDST
012700030128     C                   MOVEL     'WFMGB00T'    CMZFLD
012800020805     C                   eval      cmZmbd = 'M' + com03 + '046'
012900000727     C                   MOVEL     O50PLA        CMZLBD
013000030128     C                   MOVEL     'WFMGB00R'    CMZFLA
013100020805     C                   eval      cmZmba = 'M' + com03 + '046'
013200000727     C                   MOVEL     O50ALG        CMZLBA
013300901002     C                   MOVE      '0000'        CMZORI
013400901002     C                   MOVE      '0000000'     CMZDTI
013500000727     C                   MOVE      'R'           CMZURG
013600901002     C                   MOVE      'A'           CMZADD
013700901003     C                   MOVE      'S'           CMZCLR
013800901002     C                   MOVE      'S'           CMZFTR
013900901002     C                   MOVE      *BLANK        CMZERR
014000030128     C                   MOVE      'SIG6'        CMZAZN
014100000727     C                   MOVEL     O50ASI        CMZSYF
014200030128     C                   MOVE      wfm           CMZNRR
014300950503     C                   MOVE      *BLANK        CMZPRR
014400020805     C                   CALL      'TIBS53C'
014500000727     C                   PARM                    CMZLCL            1
014600000727     C                   PARM                    CMZDST
014700940120     C                   PARM                    CMZFLD
014800940120     C                   PARM                    CMZMBD
014900940120     C                   PARM                    CMZLBD
015000940120     C                   PARM                    CMZFLA
015100940120     C                   PARM                    CMZMBA
015200940120     C                   PARM                    CMZLBA
015300940120     C                   PARM                    CMZORI
015400940120     C                   PARM                    CMZDTI
015500940120     C                   PARM                    CMZURG
015600940120     C                   PARM                    CMZADD
015700940120     C                   PARM                    CMZCLR
015800940120     C                   PARM                    CMZFTR
015900940120     C                   PARM                    CMZERR
016000940120     C                   PARM                    CMZAZN
016100940120     C                   PARM                    CMZSYF
016200940120     C                   PARM                    CMZNRR
016300940120     C                   PARM                    CMZPRR
016400000727     C                   PARM                    W236            236
016500940120     C                   ENDSR
016600020805**         CMD1
016700030128ADDPFM FILE(WFMGB00T)               MBR(M000046)
016800901002**         CMD2
016900030128OVRDBF FILE(WFMGB00T)               MBR(M000046)
017000901002**         CMD3
017100030128CHKOBJ OBJ(WFMGB00T) OBJTYPE(*FILE) MBR(M000046)
