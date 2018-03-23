000100000131     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300960905     H*PARMS CVTOPT(*DATETIME)
000400960905     H DECEDIT('0,') DATEDIT(*DMY.)
000500960830     F*---------------------------------------------------------------*
000600960905     F* NDCR72R2  *        COMPATTA ND77000F X ANNO/QUADRO/COGNOME/   *
000700960905     F*                                        COD.FISCALE/RAGG.TRIB. *
000800960830     F*                                                               *
000801170125R250  *----------------------------------------------------*
000802170125R250  *  R250: Modificata anche dicitura del sorgente, togliendo
000803170125R250  *  il riferimento al 770.
000900000000     F*---------------------------------------------------------------*
001000960905     FND77004L  UF A E           K DISK
001100960905     D*-------------
001200000000     D KPJBA         E DS
001300960905     D*-------------
001400960905     D NDCR70DS      E DS
001500960905     D*------------
001600960905     D ND770           DS                  INZ
001700960905     D  SOC                    1      3
001800960905     D  ACF                    4      6P 0
001900960905     D  QUA                    7      8
002000960905     D  KCC                    9     14
002100960905     D  KSC                   15     22
002200960905     D  RGT                   23     26
002300960905     D  CDF                   27     46
002400960905     D  COG                   47     94
002500960905     D  NOM                   95    142
002600960905     D  LNC                  143    176
002700990504     D  DNC                  177    186D
002800960905     D  PNC                  187    188
002900960905     D  SES                  189    189
003000960905     D  CIT                  190    223
003100960905     D  VIA                  224    267
003200960905     D  NUM                  268    270
003300960905     D  PRV                  271    272
003400960905     D  C77                  273    292
003500960905     D  PRA                  333    335P 2
003600960905     D  PFG                  356    356
003700960905     D  SER                  357    391
003800960905     D  CSE                  392    400
003900960905     D  CDE                  401    425
004000960905     D  PRG                  426    427
004100130716R122 D  CODICE               428    428
004200960905     D*-------------
004300960905     D ND77RK          DS                  INZ
004400960905     D  RSOC                   1      3
004500960905     D  RACF                   4      6P 0
004600960905     D  RQUA                   7      8
004700960905     D  RKCC                   9     14
004800960905     D  RKSC                  15     22
004900960905     D  RRGT                  23     26
005000960905     D  RCDF                  27     46
005100960905     D  RCOG                  47     94
005200960905     D  RNOM                  95    142
005300960905     D  RLNC                 143    176
005400990504     D  RDNC                 177    186D
005500960905     D  RPNC                 187    188
005600960905     D  RSES                 189    189
005700960905     D  RCIT                 190    223
005800960905     D  RVIA                 224    267
005900960905     D  RNUM                 268    270
006000960905     D  RPRV                 271    272
006100960905     D  RC77                 273    292
006200960905     D  RITC                 293    302P 3
006300960905     D  RIMN                 303    312P 3
006400960905     D  RSNS                 313    322P 3
006500960905     D  RIMS                 323    332P 3
006600960905     D  RPRA                 333    335P 2
006700960905     D  RRAC                 336    345P 3
006800960905     D  RNET                 346    355P 3
006900960905     D  RPFG                 356    356
007000960905     D  RSER                 357    391
007100960905     D  RCSE                 392    400
007200960905     D  RCDE                 401    425
007300960905     D  RPRG                 426    427
007400130716R122 D  RCODICE              428    428
007500960905     D*-------------
007600960905     D N77X            DS
007700960905     D  N77SOCIETA             1      3
007800960905     D  N77ACF                 4      8  0
007900960905     D  N77QUA                 9     10
008000960905     D  N77COG                11     58
008100960905     D  N77CDF                59     78
008200960905     D  N77RGT                79     82
008300960905     D  N77PRG                83     84
008400150626R186 D  N77C77                85    104
008500960905     D*-------------
008600960905     D SAVN77          DS
008700960905     D  S77SOCIETA             1      3
008800960905     D  S77ACF                 4      8  0
008900960905     D  S77QUA                 9     10
009000960905     D  S77COG                11     58
009100960905     D  S77CDF                59     78
009200960905     D  S77RGT                79     82
009300960905     D  S77PRG                83     84
009400150626R186 D  S77C77                85    104
009500960905     D*-------------
009600960905     C*---------------------------------------------------------------*
009700960905     C*                     MAIN LINE PROGRAM                         *
009800960905     C*---------------------------------------------------------------*
009900960905     C*
010000000000     C                   EXSR      DEFCAM                                       *
010100960905     C*
010200960905     C                   Move      SocR70        N77Societa
010300960905     C                   Z-Add     AnnoR70       N77ACF
010400960905     C     KEY770        SETLL     ND77004L
010500960905     C*
010600000000     C                   DO        *HIVAL                                       *  -1-
010700960905     C     KEY770        READE     ND77004L                               90    *
010800960905     C*
010900960905     C     *IN90         IFNE      *ON
011000960905     C*
011100000131     C                   Seton                                        91
011200000131     C*
011300960830     C* 1° GIRO
011400960905     C     S77CDF        IFEQ      *BLANK
011500960830     C* SALVA LA CHIAVE
011600960905     C                   MOVE      N77X          SAVN77
011700960902     C* SALVA IL TRACCIATO
011800960902     C                   EXSR      SAVDB
011900960905     C                   ENDIF
012000960905     C*
012100960830     C* CHIAVE = SOMMO I DATI
012200960905     C     SAVN77        IFEQ      N77X
012300960830     C* SOMMA LA CHIAVE
012400960830     C                   EXSR      SOMMA
012500960830     C* CANCELLA RECORD
012600960830     C                   EXSR      CANC
012700960830     C                   ELSE
012800960830     C*
012900960830     C* CAMBIO CHIAVE SCARICA I TOTALI X CDF
013000960830     C*
013100960902     C* SALVA LA CHIAVE
013200960905     C                   MOVE      N77X          SAVN77
013300960905     C* SALVA IL TRACCIATO APPENA LETTO IN ND770RK
013400960903     C                   EXSR      SAVRK
013500960903     C* CANCELLA RECORD
013600960903     C                   EXSR      CANC
013700960903     C* SCRIVE LA CHIAVE PRECEDENTE TOTALIZZATA
013800960903     C                   EXSR      SCRIVI
013900960903     C* AZZERA I TOTALI PER LA CHIAVE PRECEDENTE
014000960903     C                   EXSR      AZZERA
014100960903     C* RESETTA IL RECORD LETTO
014200960903     C                   EXSR      RESRK
014300960903     C* SALVA IL TRACCIATO
014400960903     C                   EXSR      SAVDB
014500960830     C* SOMMA LA CHIAVE
014600960830     C                   EXSR      SOMMA
014700960905     C                   ENDIF
014800960830     C*
014900960905     C                   ENDIF
015000960905     C*
015100960905     C  N90              ENDDO
015200960905     C*
015300960903     C* SCRIVE LA CHIAVE PRECEDENTE TOTALIZZATA
015400000131     C   91              EXSR      SCRIVI
015500960905     C*
015600960830     C     FINE          TAG
015700960830     C                   SETON                                        LR
015800000000     C*----------------------------------------------------*
015900000000     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
016000000000     C*----------------------------------------------------*
016100000000     C     DEFCAM        BEGSR                                                  *
016200960905     C*
016300000000     C     *ENTRY        PLIST                                                  *
016400000000     C                   PARM                    KPJBA                          *
016500960905     C*
016600960905     C                   MOVEL     KPJBU         ndcr70ds                       *
016700960905     C*
016800960830     C     KEY770        KLIST                                                  *
016900960905     C                   KFLD                    N77SOCIETA                     *
017000960905     C                   KFLD                    N77ACF                         *
017100960905     C*
017200960905     C                   RESET                   ND770
017300000131     C                   Setoff                                       91
017400960905     C*
017500000000     C                   ENDSR                                                  *
017600960905     C*
017700960902     C*----------------------------------------------------*
017800960902     C*   SOMMA IMPORTI A PARITA' DI CHIAVE                *
017900960902     C*----------------------------------------------------*
018000960902     C     SOMMA         BEGSR                                                  *
018100960905     C*
018200960905     C     N77ITC        ADD       ITC           ITC              19 3          *
018300960905     C     N77IMN        ADD       IMN           IMN              19 3          *
018400960905     C     N77SNS        ADD       SNS           SNS              19 3          *
018500960905     C     N77IMS        ADD       IMS           IMS              19 3          *
018600960905     C     N77RAC        ADD       RAC           RAC              19 3          *
018700960905     C     N77NET        ADD       NET           NET              19 3          *
018800960905     C*
018900960902     C                   ENDSR                                                  *
019000960902     C*----------------------------------------------------*
019100960902     C*   AZZERA IMPORTI PER NUOVA CHIAVE                  *
019200960902     C*----------------------------------------------------*
019300960902     C     AZZERA        BEGSR                                                  *
019400960905     C*
019500960902     C                   Z-ADD     0             ITC                            *
019600960902     C                   Z-ADD     0             IMN                            *
019700960902     C                   Z-ADD     0             SNS                            *
019800960902     C                   Z-ADD     0             IMS                            *
019900960902     C                   Z-ADD     0             RAC                            *
020000960902     C                   Z-ADD     0             NET                            *
020100960905     C*
020200960902     C                   ENDSR                                                  *
020300960902     C*----------------------------------------------------*
020400960902     C*   CANCELLA RECORD TOTALIZZATO
020500960902     C*----------------------------------------------------*
020600960902     C     CANC          BEGSR                                                  *
020700960905     C*
020800960905     C                   DELETE    ND770000
020900960905     C*
021000960902     C                   ENDSR                                                  *
021100960902     C*----------------------------------------------------*
021200960902     C*   SALVA IL TRACCIATO
021300960902     C*----------------------------------------------------*
021400960902     C     SAVDB         BEGSR                                                  *
021500960905     C*
021600960905     C                   MOVE      N77SOCIETA    SOC                            *
021700960902     C                   MOVE      N77ACF        ACF
021800960902     C                   MOVE      N77QUA        QUA
021900960902     C                   MOVE      N77KCC        KCC
022000960902     C                   MOVE      N77KSC        KSC
022100960902     C                   MOVE      N77RGT        RGT
022200960902     C                   MOVE      N77CDF        CDF
022300960902     C                   MOVE      N77COG        COG
022400960902     C                   MOVE      N77NOM        NOM
022500960902     C                   MOVE      N77LNC        LNC
022600960902     C                   MOVE      N77DNC        DNC
022700960902     C                   MOVE      N77PNC        PNC
022800960902     C                   MOVE      N77SES        SES
022900960902     C                   MOVE      N77CIT        CIT
023000960902     C                   MOVE      N77VIA        VIA
023100960902     C                   MOVE      N77NUM        NUM
023200960902     C                   MOVE      N77PRV        PRV
023300960902     C                   MOVE      N77C77        C77
023400960902     C                   MOVE      N77PRA        PRA
023500960902     C                   MOVE      N77PFG        PFG
023600960902     C                   MOVE      N77SER        SER
023700960902     C                   MOVE      N77CSE        CSE
023800960902     C                   MOVE      N77CDE        CDE
023900960905     C                   MOVE      N77PRG        PRG
024000130716R122 C                   MOVE      N77Codice     Codice
024100960905     C*
024200960902     C                   ENDSR                                                  *
024300960903     C*----------------------------------------------------*
024400960903     C*   SALVA IL TRACCIATO APPENA LETTO
024500960903     C*----------------------------------------------------*
024600960903     C     SAVRK         BEGSR                                                  *
024700960905     C*
024800960905     C                   MOVE      N77SOCIETA    RSOC                            *
024900960903     C                   MOVE      N77ACF        RACF
025000960903     C                   MOVE      N77QUA        RQUA
025100960903     C                   MOVE      N77KCC        RKCC
025200960903     C                   MOVE      N77KSC        RKSC
025300960903     C                   MOVE      N77RGT        RRGT
025400960903     C                   MOVE      N77CDF        RCDF
025500960903     C                   MOVE      N77COG        RCOG
025600960903     C                   MOVE      N77NOM        RNOM
025700960903     C                   MOVE      N77LNC        RLNC
025800960903     C                   MOVE      N77DNC        RDNC
025900960903     C                   MOVE      N77PNC        RPNC
026000960903     C                   MOVE      N77SES        RSES
026100960903     C                   MOVE      N77CIT        RCIT
026200960903     C                   MOVE      N77VIA        RVIA
026300960903     C                   MOVE      N77NUM        RNUM
026400960903     C                   MOVE      N77PRV        RPRV
026500960903     C                   MOVE      N77C77        RC77
026600960903     C                   Z-ADD     N77ITC        RITC
026700960903     C                   Z-ADD     N77IMN        RIMN
026800960903     C                   Z-ADD     N77SNS        RSNS
026900960903     C                   Z-ADD     N77IMS        RIMS
027000960903     C                   Z-ADD     N77RAC        RRAC
027100960903     C                   Z-ADD     N77NET        RNET
027200960903     C                   MOVE      N77PRA        RPRA
027300960903     C                   MOVE      N77PFG        RPFG
027400960903     C                   MOVE      N77SER        RSER
027500960903     C                   MOVE      N77CSE        RCSE
027600960903     C                   MOVE      N77CDE        RCDE
027700960905     C                   MOVE      N77PRG        RPRG
027800130716R122 C                   MOVE      N77Codice     RCodice
027900960905     C*
028000960903     C                   ENDSR                                                  *
028100960903     C*----------------------------------------------------*
028200960903     C*   RESETTA IL TRACCIATO APPENA LETTO
028300960903     C*----------------------------------------------------*
028400960903     C     RESRK         BEGSR                                                  *
028500960905     C*
028600960905     C                   MOVE      RSOC          N77SOCIETA
028700960903     C                   MOVE      RACF          N77ACF
028800960903     C                   MOVE      RQUA          N77QUA
028900960903     C                   MOVE      RKCC          N77KCC
029000960903     C                   MOVE      RKSC          N77KSC
029100960903     C                   MOVE      RRGT          N77RGT
029200960903     C                   MOVE      RCDF          N77CDF
029300960903     C                   MOVE      RCOG          N77COG
029400960903     C                   MOVE      RNOM          N77NOM
029500960903     C                   MOVE      RLNC          N77LNC
029600960903     C                   MOVE      RDNC          N77DNC
029700960903     C                   MOVE      RPNC          N77PNC
029800960903     C                   MOVE      RSES          N77SES
029900960903     C                   MOVE      RCIT          N77CIT
030000960903     C                   MOVE      RVIA          N77VIA
030100960903     C                   MOVE      RNUM          N77NUM
030200960903     C                   MOVE      RPRV          N77PRV
030300960903     C                   MOVE      RC77          N77C77
030400960903     C                   Z-ADD     RITC          N77ITC
030500960903     C                   Z-ADD     RIMN          N77IMN
030600960903     C                   Z-ADD     RSNS          N77SNS
030700960903     C                   Z-ADD     RIMS          N77IMS
030800960903     C                   Z-ADD     RRAC          N77RAC
030900960903     C                   Z-ADD     RNET          N77NET
031000960903     C                   MOVE      RPRA          N77PRA
031100960903     C                   MOVE      RPFG          N77PFG
031200960903     C                   MOVE      RSER          N77SER
031300960903     C                   MOVE      RCSE          N77CSE
031400960903     C                   MOVE      RCDE          N77CDE
031500960905     C                   MOVE      RPRG          N77PRG
031600130716R122 C                   MOVE      RCODICE       N77CODICE
031700960905     C*
031800960903     C                   ENDSR                                                  *
031900000000     C*----------------------------------------------------*
032000000000     C* SUBROUTINE SCRITTURA FILE DA DS MULTIPLA           *
032100000000     C*----------------------------------------------------*
032200000000     C     SCRIVI        BEGSR                                                  *
032300960905     C*
032400960905     C                   MOVE      SOC           N77SOCIETA                     *
032500000000     C                   Z-ADD     ACF           N77ACF                         *
032600000000     C                   MOVE      QUA           N77QUA                         *
032700960905     C                   MOVE      KCC           N77KCC                         *
032800960905     C                   MOVE      KSC           N77KSC                         *
032900000000     C                   MOVE      RGT           N77RGT                         *
033000000000     C                   MOVE      CDF           N77CDF                         *
033100000000     C                   MOVE      COG           N77COG                         *
033200000000     C                   MOVE      NOM           N77NOM                         *
033300000000     C                   MOVE      LNC           N77LNC                         *
033400960905     C                   MOVE      DNC           N77DNC                         *
033500000000     C                   MOVE      PNC           N77PNC                         *
033600000000     C                   MOVE      SES           N77SES                         *
033700000000     C                   MOVE      CIT           N77CIT                         *
033800000000     C                   MOVE      VIA           N77VIA                         *
033900940316     C     CDE           IFNE      *BLANK
034000940316     C                   MOVE      '*'           NUM
034100940316     C                   MOVE      '  '          PRV
034200960905     C                   ENDIF
034300000000     C                   MOVE      NUM           N77NUM                         *
034400000000     C                   MOVE      PRV           N77PRV                         *
034500000000     C                   MOVEL     C77           N77C77                         *
034600000000     C                   Z-ADD     ITC           N77ITC                         *
034700000000     C                   Z-ADD     IMN           N77IMN                         *
034800000000     C                   Z-ADD     SNS           N77SNS                         *
034900000000     C                   Z-ADD     IMS           N77IMS                         *
035000000000     C                   Z-ADD     PRA           N77PRA                         *
035100000000     C                   Z-ADD     RAC           N77RAC                         *
035200000000     C                   Z-ADD     NET           N77NET                         *
035300000000     C                   MOVE      PFG           N77PFG                         *
035400940316     C                   MOVEL     SER           N77SER
035500960902     C                   MOVEL     CSE           N77CSE
035600940316     C     CSE           IFEQ      *ZEROS                                       ITALIA
035700940316     C                   MOVE      *BLANK        N77CSE
035800940316     C                   MOVE      *BLANK        N77SER
035900960905     C                   ENDIF
036000940316     C                   MOVEL     CDE           N77CDE
036100960905     C                   MOVEL     PRG           N77PRG
036200130716R122 C                   MOVEL     CODICE        N77CODICE
036300160215R215 C                   Eval      N77CitAP = N77Cit
036400160215  "  C                   Eval      N77PrvAP = N77Prv
036500160215R215 C                   Eval      N77Comap = N77Com
036600960905     C                   WRITE     ND770000                                     *
036700960905     C*
036800960902     C                   ENDSR                                                  *
