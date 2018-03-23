000100981211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300981027     H*PARMS CVTOPT(*DATETIME)
000400981027     H DECEDIT('0,') DATEDIT(*DMY.)
000500920514     F*---------------------------------------------------------------*H
000600990429     F* NDCR83R   *      SCRITTURA ARCHIVIO IMPORTI CASELLE MOD. 770  *
000700990427     F*                  QUADRO ST                                    *
000800920514     F*---------------------------------------------------------------*
000900930909     F*   D A T A    B A S E                                          *
001000930909     F*---------------------------------------------------------------*
001100981027     FND77R01L  IF   E           K DISK
001200990430     FND77010F  O  A E             DISK
001300990421     D F0D             S            200    DIM(9)
001400990421     D STK             S             24    DIM(75)
001500990421     D CAM             S              1    DIM(16)
001600000413     D GGDS          E DS                  EXTNAME(ND77000EDS) INZ
001700920514     D KPJBA         E DS
001800981027     D ndcr80ds      E DS
001900981027     D*-------------
002000981027     D* Dati di ambiente ottenuti da XSOC
002100981027     D SOC001        E DS                  EXTNAME(XSOC001DS)
002200981027     D*-------------
002300981027     D* DS Interna per dati di output di XSOC
002400981027     D XSOCDS          DS          1000
002500981027     D*-------------
002600920514     D  PER1                   1      6  0
002700930909     D  ANNO                   1      4  0
002800920515     D  PER2                   7     12  0
002900981027     D*------------
003000990422     D AMG             DS
003100990422     D  AMGAnno                1      4
003200990422     D  T1                     5      5
003300990422     D  AMGMese                6      7
003400990422     D  T2                     8      8
003500990422     D  AMGGiorno              9     10
003600040622C1770D*-------------
003700040622  "  D* data di versamento
003800040622  "  D                 DS
003900040622  "  D  DVGG                   1      2  0
004000040622  "  D  DVMM                   3      4  0
004100040622  "  D  DVAAAA                 5      8  0
004200040622  "  D  DtVers                 1      8  0
004300040622  "  D*------------
004400040622C1770D DatMin          C                   CONST('0001-01-01')
004500990422     D*-------------
004600990421     D                 DS
004700990421     D  MM                     1      2  0
004800990421     D  SEC19                  3      4  0
004900990421     D  AA                     5      6  0
005000990421     D  DATA19                 1      6  0
005100990422     D*------------
005200990421     D                 DS
005300990421     D  GG1                    1      2  0
005400990421     D  MM1                    3      4  0
005500990422     D  AA1                    5      8  0
005600990421     D  AMG19                  1      8  0
005700990429     D*-------------
005800990421     D                 DS
005900990421     D  QUADRO                 1      2
006000990421     D  RIGO                   3      5  0
006100990421     D  COLO                   6      8  0
006200990421     D  CODICE                 1      8
006300990429     D*-------------
006400990429     D WrkData         S               D
006401160620R218 D $First          S              1
006403160620  "  D SavCdfs         S                   Like(R77CDFS)
006404160620R218 D SavEve          S                   Like(R77Eve)
006500130530R110 D*-------------
006600130530 "   D CampoWrk        DS
006700130530 "   D CampoInt                1     14
006800130530 "   D CampoDec               15     16
006900130530 "   D*
007000130530 "   D CampoIntero     DS
007100130530 "   D  Int                    1     13
007200130530 "   D  virgola               14     14
007300130530 "   D  Dec                   15     16
007400130530R110 D*-------------
007500920514     I*----------------------------------------------------*
007600930909     C*  C I C L O       P R I N C I P A L E               *
007700930909     C*----------------------------------------------------*
007800981027     C     KEY770        SETLL     ND77R01L                                     *
007900981027     C     KEY770        READE     ND77R01L                               80    *
007901160620R228 C                   If        not *in80
007902160620  "  C                   Eval      SavCdFs   =  R77Cdfs
007903160620  "  C                   Eval      SavEve    =  R77Eve
007904160620  "  C                   Eval      $First = *on
007906160620R218 C                   Endif
007907160620     C
008000980806     C     *IN80         DOWEQ     '0'
008100990421     C*
008200990421     C*              -------------------
008300990421     C*              SCRITTURA QUADRO ST
008400990421     C*              -------------------
008500990421     C*
008600990421     C                   MOVEL     'ST'          QUADRO
008601160620     C*
008602160620R218 C                   If        R77CdFs <> SavCdFs or
008603160620  "  C                             R77Eve  <> SavEve
008604160620  "  C                   Eval      $First = *on
008605160620     C                   EXSR      WRST
008606160620     C                   Z-ADD     1             STRIG
008607160620     C                   ADD       1             STMOD
008610160620C1667C                   MOVEL     *BLANKS       STK(1)
008613160620  "  C                   Endif
008614160620  "  C* Solo la prima volta oppure a rottura
008616160620  "  C                   If        $First = *on
008617160620  "  C* Se presente Codice fiscale sostituto di imposta
008618160620  "  C                   If        R77CDFS <> *blanks
008619160603  "  C                   Z-Add     1             STCOL
008620160620  "  C                   Movel(P)  R77CDFS       CAMPO
008621160603  "  C                   EXSR      WST
008622160603  "  C* Se presente anche eventi eccezionali
008623160620  "  C                   If        R77Eve <> *blanks
008624160603  "  C                   Z-Add     2             STCOL
008625160620  "  C                   Move(P)   R77Eve        Campo
008626160603  "  C                   EXSR      WST
008627160603  "  C                   Endif
008628160620  "  C                   Endif
008629160620  "  C                   Eval      SavCdfs = R77CdfS
008630160620  "  C                   Eval      SavEve  = R77Eve
008631160620  "  C                   Eval      $first  = *off
008633160620R218 C                   Endif
008700990421     C* PERIODO DI RIFERIMENTO
008800990421     C                   ADD       1             STRIG
008900990427     C                   Z-Add     1             STCOL
009000990421     C                   MOVE      R77MAC        AA
009100990421     C                   MOVEL     R77MAC        MM
009200990421     C                   MOVE      DATA19        CAMPO
009300990421     C                   EXSR      WST
009400990421     C* RITENUTE EFFETTUATE
009500020417     C                   Z-Add     2             STCOL
009600020423     C                   Z-add     R77RAC        IMPORT           13 2
009700020612D1143C     TroncR80      IfEq      *On
009800020612  "  C                   Z-Add     IMPORT        IMPORTZ          13 0
009900020607  "  C                   MOVE      IMPORTZ       CAMPO
010000020607D1143C                   Else
010100990805     C                   MOVE      IMPORT        CAMPO
010200130530R110 C                   EXSR      WSCINT
010300020607D1143C                   EndIf
010400990421     C                   EXSR      ABLANK
010500990421     C                   EXSR      WST
010600030911C1667C* Importo versato
010700090514D2500X***  ex C1667      Z-Add     5             STCOL
010800090406D2500C                   Z-Add     7             STCOL
010900090406C1667C                   Z-add     R77IMV        IMPORT           13 2
011000030911  "  C     TroncR80      IfEq      *On
011100030911  "  C                   Z-Add     IMPORT        IMPORTZ          13 0
011200030911  "  C                   MOVE      IMPORTZ       CAMPO
011300030911  "  C                   Else
011400130530C1667C                   MOVE      IMPORT        CAMPO
011500130530R110 C                   EXSR      WSCINT
011600130530C1667C                   EndIf
011700030911  "  C                   EXSR      ABLANK
011800030911C1667C                   EXSR      WST
011801150508R183 C* Interessi
011802150508  "  C                   Z-Add     8             STCOL
011803150514  "  C                   If        R77IRO        <> *zeros
011804150514  "  C                   Z-add     R77IRO        IMPORT           13 2
011805150508  "  C     TroncR80      IfEq      *On
011806150508  "  C                   Z-Add     IMPORT        IMPORTZ          13 0
011807150508  "  C                   MOVE      IMPORTZ       CAMPO
011809150508  "  C                   Else
011810150508  "  C                   MOVE      IMPORT        CAMPO
011811150508  "  C                   EXSR      WSCINT
011813150508  "  C                   EndIf
011814150508  "  C                   EXSR      ABLANK
011815150508  "  C                   EXSR      WST
011816150508R183 C                   EndIf
011900070620C2014C* Ravvedimento
012000090514D2500X*** ex C2014       Z-Add     7             STCOL
012100090406D2500C                   Z-Add     9             STCOL
012200090406C2014c                   If        R77ravvedi    <> *blanks
012201150521R183 C                   MOVE(P)   '1'           Campo
012300150508R183 X*** ex C2014       MOVEl     '1'           Campo
012400070918D2265X*ex C2014          else
012500070918  "  X*ex C2014          MOVEl     '0'           Campo
012600070918D2265X*ex C2014          endif
012700150508R183 X*** ex C2014       EXSR      ABLANK
012800070620C2014C                   EXSR      WST
012900070918D2265C                   endif
013000020417     C* CODICE TRIBUTO O CAPITOLO
013100030618C1667X****               Z-ADD     10            STCOL
013200090514D2500X** ex C1667        Z-ADD     9             STCOL
013300090406D2500C                   Z-ADD     11            STCOL
013400020417     C                   MOVEL     R77TRI        CAMPO
013500020417     C                   EXSR      WST
013600040622C1770C*
013700090406C1770C* Data di versamento
013800090514D2500X** ex C1770        Z-ADD     12            STCOL
013900090406D2500C                   Z-ADD     14            STCOL
014000090406C1770C                   if        R77DQV     <> DatMin
014100040622  "  C                   Move      R77DQV        AMG
014200040622  "  C                   Move      AMGAnno       DVAAAA
014300040622  "  C                   Move      AMGMese       DVMM
014400040622  "  C                   Move      AMGGiorno     DVGG
014500040622  "  C*
014600040622  "  C                   Move      DtVers        Campo
014700040622  "  C                   EXSR      WST
014800040622  "  C                   endif
014900040622C1770C*
015000930909     C* Nuova lettura                                      *
015100981027     C     KEY770        READE     ND77R01L                               80    *
015200930909     C                   END                                                    *
015300930909     C*                                                    *
015400030618C1667X**** ST            CASGT     1             WRST
015500030618C1667C     ST            CASGT     0             WRST
015600990421     C                   ENDCS
015700990421     C*
015800990421     C*
015900981027     C     FINE          TAG
016000930909     C                   SETON                                        LR        *
016100990421     C*----------------------------------------------------------------
016200990421     C*
016300990421     C* TOGLIE ZERI NON SIGNIFICATIVI DAL CAMPO
016400990421     C*
016500990421     C*----------------------------------------------------------------
016600990421     C     ABLANK        BEGSR
016700990421     C*
016800990421     C                   MOVEA     CAMPO         CAM
016900990421     C*
017000990421     C                   DO        16            X                 2 0
017100990421     C     CAM(X)        IFEQ      ' '
017200990421     C     CAM(X)        OREQ      '0'
017300990421     C                   MOVE      ' '           CAM(X)
017400990421     C                   ELSE
017500990421     C                   LEAVE
017600990421     C                   ENDIF
017700990421     C                   ENDDO
017800130530R110 C*
017900130530 "   C                   If        Cam(13)       = ' '  and
018000130530 "   C                             Cam(14)       = ','  and
018100130530 "   C                             Cam(15)       <>' '
018200130530 "   C                   Eval      Cam(13)       = '0'
018300130530R110 C                   EndIf
018400130530     C*
018500990421     C                   MOVEA     CAM           CAMPO            16
018600990421     C*
018700990421     C                   ENDSR
018800130530R110 C*----------------------------------------------------------------
018900130530 "   C* Tratta campo importo
019000130530 "   C*----------------------------------------------------------------
019100130530 "   C     WSCINT        BEGSR
019200130530 "   C*
019300130530 "   C                   If        Import        <> 0
019400130530 "   C                   Move      Campo         CampoWRk
019500130530 "   C                   Move      CampoInt      Int
019600130530 "   C                   Move      ','           Virgola
019700130530 "   C                   Move      CampoDec      Dec
019800130530 "   C                   Move      CampoIntero   Campo
019900130530 "   C                   EndIf
020000130530 "   C*
020100130530R110 C                   ENDSR
020200990421     C*----------------------------------------------------------------
020300990421     C*
020400990421     C* PREPARA SCHIERA ST
020500990421     C*
020600990421     C*----------------------------------------------------------------
020700990421     C     WST           BEGSR
020800990421     C*
020900990421     C                   Z-ADD     STCOL         COLO
021000990421     C                   Z-ADD     STRIG         RIGO
021100990421     C                   ADD       1             ST
021200990421     C                   MOVEL     CODICE        STK(ST)
021300990421     C                   MOVE      CAMPO         STK(ST)
021400990421     C                   MOVEL     *BLANK        CAMPO
021500990421     C*
021600990421     C* SE RIEMPIO LA SCHIERA CREO RECORD
021700990421     C*
021800990421     C     ST            CASEQ     75            WRST
021900990421     C                   ENDCS
022000990421     C*
022100990421     C* SE RAGGIUNGO IL NUMERO MAX DI RIGHE INCREMENTO MODULO
022101160620R218 C* OPPURE SE E' CAMBIATO IL CODICE FISCALE DEL SOSTITUTO
022200990421     C*
022300020417     C     STRIG         IFEQ      13
022400030630C1667X**** STCOL         ANDEQ     10
022500040624C1770X**** ex C1667 STCOL         ANDEQ     9
022600090623D2517X**** ex C1170 STCOL         ANDEQ     12
022700090623D2517C     STCOL         ANDEQ     14
022800990422     C                   EXSR      WRST
022900990421     C                   Z-ADD     1             STRIG
023000990421     C                   ADD       1             STMOD
023100030630C1667X****               ADD       1             ST
023200030618C1667X****               MOVEL     'ST001001'    STK(1)
023300030618C1667C                   MOVEL     *BLANKS       STK(1)
023400990421     C                   ENDIF
023500990421     C*
023600990421     C                   ENDSR
023700990421     C*----------------------------------------------------------------
023800990421     C*
023900990421     C* SCRIVE RECORD ST
024000990421     C*
024100990421     C*----------------------------------------------------------------
024200990421     C     WRST          BEGSR
024300990421     C*
024400160603R218 X***                MOVE      'E'           M710TR
024401160603R218 C                   MOVE      'D'           M710TR
024500990430     C                   MOVEL     XSCCDF        M710CF
024600990430     C                   Z-ADD     STMOD         M710PM
024700990421     C                   MOVEA     STK           F0D
024800990421     C                   MOVEA     F0D(1)        F01
024900990421     C                   MOVEA     F0D(2)        F02
025000990421     C                   MOVEA     F0D(3)        F03
025100990421     C                   MOVEA     F0D(4)        F04
025200990421     C                   MOVEA     F0D(5)        F05
025300990421     C                   MOVEA     F0D(6)        F06
025400990421     C                   MOVEA     F0D(7)        F07
025500990421     C                   MOVEA     F0D(8)        F08
025600990421     C                   MOVEA     F0D(9)        F09
025700990430     C                   MOVE      GGDS          M71REC
025800990430     C                   WRITE     M77010
025900990422     C                   Z-ADD     0             ST
026000990421     C                   CLEAR                   GGDS
026100990421     C                   CLEAR                   F0D
026200990430     C                   CLEAR                   M71REC
026300990421     C                   CLEAR                   STK
026400990421     C*
026500990421     C                   ENDSR
026600920514     C*----------------------------------------------------*
026700920514     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
026800920514     C*----------------------------------------------------*
026900930909     C     *INZSR        BEGSR                                                  *
027000930909     C* --                                                 *
027100920514     C     *ENTRY        PLIST                                                  *
027200920514     C                   PARM                    KPJBA                          *
027300981027     C                   MOVEL     KPJBU         ndcr80ds                       *
027400920514     C* --                                                 *
027500930909     C     KEY770        KLIST                                                  *
027600981027     C                   KFLD                    socR80                         *
027700930909     C                   KFLD                    R77ACF                         *
027800930909     C* --                                                 *
027900981028     C*  Posizionamento iniziale per anno di competenza e lettura
028000981028     C                   MOVEL     PER1R80       ANNO              4 0
028100981028     C                   Z-ADD     ANNO          R77ACF
028200010404C1478C****               Z-ADD     19            SEC19
028300010404C1478C                   Z-ADD     20            SEC19
028400990421     C*
028500990421     C* CAMPI QUADRO ST
028600990421     C*
028700990421     C                   Z-ADD     1             STRIG             3 0
028800990421     C                   Z-ADD     0             STCOL             3 0
028900030618C1667X****               Z-ADD     1             ST                3 0
029000030618C1667C                   Z-ADD     0             ST                3 0
029100990421     C                   Z-ADD     1             STMOD             8 0
029200030618C1667X****               MOVEL     'ST001001'    STK(1)
029300030618C1667C                   MOVEL     *BLANKS       STK(1)
029400981027     C* Chiamata a xsoc
029500981027     C                   move      socR80        SOCXSC
029600981027     C                   MOVEL     'SOC001'      TIPXSC
029700981027     C                   CALLb     'XSOC'
029800981027     C                   PARM                    TIPXSC            6
029900981027     C                   PARM                    SOCXSC            3
030000981027     C                   PARM                    CDSXSC            9 0
030100981027     C                   PARM                    MODXSC            3
030200981027     C                   PARM      *BLANKS       RTNXSC            1
030300981027     C                   PARM                    XSOCDS
030400981027     C                   PARM                    KPJBA
030500981027     C     RTNXSC        IFNE      '1'
030600981027     C                   MOVEL     XSOCDS        SOC001
030700981027     C                   ELSE
030800981027     C                   GOTO      FINE
030900981027     C                   ENDIF
031000981027     C*
031100130530R110 C                   Eval      TroncR80 = *Off                                                 C
031200130530R110 C*
031300980806     C                   ENDSR
031400920514     C*----------------------------------------------------*
