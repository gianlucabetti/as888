000100160616     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200160616     H*PARMS BNDDIR(PJXBND PJCBND)
000300160616     H*PARMS CVTOPT(*DATETIME)
000400160616     H DECEDIT('0,') DATEDIT(*DMY.)
000500160616     F*---------------------------------------------------------------*
000600160616     F* SDGSK05R2          COMPATTA SDGN7700F X ANNO/QUADRO/COGNOME/  *
000700160616     F*                                        ID. FISCALE/RAGG.TRIB. *
000800160616     F*                                                               *
000900160616     F*---------------------------------------------------------------*
001000160616     FSDGN7704L UF A E           K DISK
001100160616     D*-------------
001200160616     D KPJBA         E DS
001300160616     D*-------------
001400160617     D NDCR70DS      E DS
001500160616     D*------------
001600160616     D ND770           DS                  INZ
001700160616     D  SOC                    1      3
001800160616     D  ACF                    4      6P 0
001900160616     D  QUA                    7      8
002000160616     D  KCC                    9     14
002100160616     D  KSC                   15     22
002200160616     D  RGT                   23     26
002300160616     D  CDF                   27     46
002400160616     D  COG                   47     94
002500160616     D  NOM                   95    142
002600160616     D  LNC                  143    176
002700160616     D  DNC                  177    186D
002800160616     D  PNC                  187    188
002900160616     D  SES                  189    189
003000160616     D  CIT                  190    223
003100160616     D  VIA                  224    267
003200160616     D  NUM                  268    270
003300160616     D  PRV                  271    272
003400160616     D  C77                  273    292
003500160616     D  PRA                  333    335P 2
003600160616     D  PFG                  356    356
003700160616     D  SER                  357    391
003800160616     D  CSE                  392    400
003900160616     D  CDE                  401    425
004000160616     D  PRG                  426    427
004100160616     D  CODICE               428    428
004200160616     D*-------------
004300160616     D ND77RK          DS                  INZ
004400160616     D  RSOC                   1      3
004500160616     D  RACF                   4      6P 0
004600160616     D  RQUA                   7      8
004700160616     D  RKCC                   9     14
004800160616     D  RKSC                  15     22
004900160616     D  RRGT                  23     26
005000160616     D  RCDF                  27     46
005100160616     D  RCOG                  47     94
005200160616     D  RNOM                  95    142
005300160616     D  RLNC                 143    176
005400160616     D  RDNC                 177    186D
005500160616     D  RPNC                 187    188
005600160616     D  RSES                 189    189
005700160616     D  RCIT                 190    223
005800160616     D  RVIA                 224    267
005900160616     D  RNUM                 268    270
006000160616     D  RPRV                 271    272
006100160616     D  RC77                 273    292
006200160616     D  RITC                 293    302P 3
006300160616     D  RIMN                 303    312P 3
006400160616     D  RSNS                 313    322P 3
006500160616     D  RIMS                 323    332P 3
006600160616     D  RPRA                 333    335P 2
006700160616     D  RRAC                 336    345P 3
006800160616     D  RNET                 346    355P 3
006900160616     D  RPFG                 356    356
007000160616     D  RSER                 357    391
007100160616     D  RCSE                 392    400
007200160616     D  RCDE                 401    425
007300160616     D  RPRG                 426    427
007500160616     D*-------------
007600160616     D N77X            DS
007700160616     D  N77SOCIETA             1      3
007800160616     D  N77ACF                 4      8  0
007900160616     D  N77QUA                 9     10
008000160616     D  N77COG                11     58
008100160616     D  N77CDE                59     83
008200160616     D  N77RGT                84     87
008300160616     D  N77PRG                88     89
008400160616     D  N77C77                90    109
008500160616     D*-------------
008600160616     D SAVN77          DS
008700160616     D  S77SOCIETA             1      3
008800160616     D  S77ACF                 4      8  0
008900160616     D  S77QUA                 9     10
009000160616     D  S77COG                11     58
009100160616     D  S77CDE                59     83
009200160616     D  S77RGT                84     87
009300160616     D  S77PRG                88     89
009400160616     D  S77C77                90    109
009500160616     D*-------------
009600160616     C*---------------------------------------------------------------*
009700160616     C*                     MAIN LINE PROGRAM                         *
009800160616     C*---------------------------------------------------------------*
009900160616     C*
010000160616     C                   EXSR      DEFCAM                                       *
010100160616     C*
010200160617     C                   Move      SocR70        N77Societa
010300160617     C                   Z-Add     AnnoR70       N77ACF
010400160616     C     KEYN77        SETLL     SDGN7704L
010500160616     C*
010600160616     C                   DO        *HIVAL                                       *  -1-
010700160616     C     KEYN77        READE     SDGN7704L                              90    *
010800160616     C*
010900160616     C     *IN90         IFNE      *ON
011000160616     C*
011100160616     C                   Seton                                        91
011200160616     C*
011300160616     C* 1° GIRO
011400160616     C     S77CDE        IFEQ      *BLANK
011500160616     C* SALVA LA CHIAVE
011600160616     C                   MOVE      N77X          SAVN77
011700160616     C* SALVA IL TRACCIATO
011800160616     C                   EXSR      SAVDB
011900160616     C                   ENDIF
012000160616     C*
012100160616     C* CHIAVE = SOMMO I DATI
012200160616     C     SAVN77        IFEQ      N77X
012300160616     C* SOMMA LA CHIAVE
012400160616     C                   EXSR      SOMMA
012500160616     C* CANCELLA RECORD
012600160616     C                   EXSR      CANC
012700160616     C                   ELSE
012800160616     C*
012900160616     C* CAMBIO CHIAVE SCARICA I TOTALI X CDE
013000160616     C*
013100160616     C* SALVA LA CHIAVE
013200160616     C                   MOVE      N77X          SAVN77
013300160616     C* SALVA IL TRACCIATO APPENA LETTO IN ND770RK
013400160616     C                   EXSR      SAVRK
013500160616     C* CANCELLA RECORD
013600160616     C                   EXSR      CANC
013700160616     C* SCRIVE LA CHIAVE PRECEDENTE TOTALIZZATA
013800160616     C                   EXSR      SCRIVI
013900160616     C* AZZERA I TOTALI PER LA CHIAVE PRECEDENTE
014000160616     C                   EXSR      AZZERA
014100160616     C* RESETTA IL RECORD LETTO
014200160616     C                   EXSR      RESRK
014300160616     C* SALVA IL TRACCIATO
014400160616     C                   EXSR      SAVDB
014500160616     C* SOMMA LA CHIAVE
014600160616     C                   EXSR      SOMMA
014700160616     C                   ENDIF
014800160616     C*
014900160616     C                   ENDIF
015000160616     C*
015100160616     C  N90              ENDDO
015200160616     C*
015300160616     C* SCRIVE LA CHIAVE PRECEDENTE TOTALIZZATA
015400160616     C   91              EXSR      SCRIVI
015500160616     C*
015600160616     C     FINE          TAG
015700160616     C                   SETON                                        LR
015800160616     C*----------------------------------------------------*
015900160616     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
016000160616     C*----------------------------------------------------*
016100160616     C     DEFCAM        BEGSR                                                  *
016200160616     C*
016300160616     C     *ENTRY        PLIST                                                  *
016400160616     C                   PARM                    KPJBA                          *
016500160616     C*
016600160617     C                   MOVEL     KPJBU         NDCR70DS                       *
016700160616     C*
016800160616     C     KEYN77        KLIST                                                  *
016900160616     C                   KFLD                    N77SOCIETA                     *
017000160616     C                   KFLD                    N77ACF                         *
017100160616     C*
017200160616     C                   RESET                   ND770
017300160616     C                   Setoff                                       91
017400160616     C*
017500160616     C                   ENDSR                                                  *
017600160616     C*
017700160616     C*----------------------------------------------------*
017800160616     C*   SOMMA IMPORTI A PARITA' DI CHIAVE                *
017900160616     C*----------------------------------------------------*
018000160616     C     SOMMA         BEGSR                                                  *
018100160616     C*
018200160616     C     N77ITC        ADD       ITC           ITC              19 3          *
018300160616     C     N77IMN        ADD       IMN           IMN              19 3          *
018400160616     C     N77SNS        ADD       SNS           SNS              19 3          *
018500160616     C     N77IMS        ADD       IMS           IMS              19 3          *
018600160616     C     N77RAC        ADD       RAC           RAC              19 3          *
018700160616     C     N77NET        ADD       NET           NET              19 3          *
018800160616     C*
018900160616     C                   ENDSR                                                  *
019000160616     C*----------------------------------------------------*
019100160616     C*   AZZERA IMPORTI PER NUOVA CHIAVE                  *
019200160616     C*----------------------------------------------------*
019300160616     C     AZZERA        BEGSR                                                  *
019400160616     C*
019500160616     C                   Z-ADD     0             ITC                            *
019600160616     C                   Z-ADD     0             IMN                            *
019700160616     C                   Z-ADD     0             SNS                            *
019800160616     C                   Z-ADD     0             IMS                            *
019900160616     C                   Z-ADD     0             RAC                            *
020000160616     C                   Z-ADD     0             NET                            *
020100160616     C*
020200160616     C                   ENDSR                                                  *
020300160616     C*----------------------------------------------------*
020400160616     C*   CANCELLA RECORD TOTALIZZATO
020500160616     C*----------------------------------------------------*
020600160616     C     CANC          BEGSR                                                  *
020700160616     C*
020800160616     C                   DELETE    SDGN77000
020900160616     C*
021000160616     C                   ENDSR                                                  *
021100160616     C*----------------------------------------------------*
021200160616     C*   SALVA IL TRACCIATO
021300160616     C*----------------------------------------------------*
021400160616     C     SAVDB         BEGSR                                                  *
021500160616     C*
021600160616     C                   MOVE      N77SOCIETA    SOC                            *
021700160616     C                   MOVE      N77ACF        ACF
021800160616     C                   MOVE      N77QUA        QUA
021900160616     C                   MOVE      N77KCC        KCC
022000160616     C                   MOVE      N77KSC        KSC
022100160616     C                   MOVE      N77RGT        RGT
022200160616     C                   MOVE      N77CDF        CDF
022300160616     C                   MOVE      N77COG        COG
022400160616     C                   MOVE      N77NOM        NOM
022500160616     C                   CLear                   LNC
022600160616     C                   MOVE      N77DNC        DNC
022700160616     C                   Clear                   PNC
022800160616     C                   MOVE      N77SES        SES
022900160616     C                   MOVE      N77CIT        CIT
023000160616     C                   MOVE      N77VIA        VIA
023001160616     C                   Clear                   NUM
023200160616     C                   Clear                   PRV
023300160616     C                   MOVE      N77C77        C77
023400160616     C                   MOVE      N77PRA        PRA
023500160616     C                   MOVE      N77PFG        PFG
023600160616     C                   Clear                   SER
023700160616     C                   MOVE      N77CSE        CSE
023800160616     C                   MOVE      N77CDE        CDE
023900160616     C                   MOVE      N77PRG        PRG
024100160616     C*
024200160616     C                   ENDSR                                                  *
024300160616     C*----------------------------------------------------*
024400160616     C*   SALVA IL TRACCIATO APPENA LETTO
024500160616     C*----------------------------------------------------*
024600160616     C     SAVRK         BEGSR                                                  *
024700160616     C*
024800160616     C                   MOVE      N77SOCIETA    RSOC                            *
024900160616     C                   MOVE      N77ACF        RACF
025000160616     C                   MOVE      N77QUA        RQUA
025100160616     C                   MOVE      N77KCC        RKCC
025200160616     C                   MOVE      N77KSC        RKSC
025300160616     C                   MOVE      N77RGT        RRGT
025400160616     C                   MOVE      N77CDF        RCDF
025500160616     C                   MOVE      N77COG        RCOG
025600160616     C                   MOVE      N77NOM        RNOM
025700160616     C                   Clear                   RLNC
025800160616     C                   MOVE      N77DNC        RDNC
025900160616     C                   Clear                   RPNC
026000160616     C                   MOVE      N77SES        RSES
026100160616     C                   MOVE      N77CIT        RCIT
026200160616     C                   MOVE      N77VIA        RVIA
026300160616     C                   Clear                   RNUM
026400160616     C                   Clear                   RPRV
026500160616     C                   MOVE      N77C77        RC77
026600160616     C                   Z-ADD     N77ITC        RITC
026700160616     C                   Z-ADD     N77IMN        RIMN
026800160616     C                   Z-ADD     N77SNS        RSNS
026900160616     C                   Z-ADD     N77IMS        RIMS
027000160616     C                   Z-ADD     N77RAC        RRAC
027100160616     C                   Z-ADD     N77NET        RNET
027200160616     C                   MOVE      N77PRA        RPRA
027300160616     C                   MOVE      N77PFG        RPFG
027400160616     C                   Clear                   RSER
027500160616     C                   MOVE      N77CSE        RCSE
027600160616     C                   MOVE      N77CDE        RCDE
027700160616     C                   MOVE      N77PRG        RPRG
027900160616     C*
028000160616     C                   ENDSR                                                  *
028100160616     C*----------------------------------------------------*
028200160616     C*   RESETTA IL TRACCIATO APPENA LETTO
028300160616     C*----------------------------------------------------*
028400160616     C     RESRK         BEGSR                                                  *
028500160616     C*
028600160616     C                   MOVE      RSOC          N77SOCIETA
028700160616     C                   MOVE      RACF          N77ACF
028800160616     C                   MOVE      RQUA          N77QUA
028900160616     C                   MOVE      RKCC          N77KCC
029000160616     C                   MOVE      RKSC          N77KSC
029100160616     C                   MOVE      RRGT          N77RGT
029200160616     C                   MOVE      RCDF          N77CDF
029300160616     C                   MOVE      RCOG          N77COG
029400160616     C                   MOVE      RNOM          N77NOM
029600160616     C                   MOVE      RDNC          N77DNC
029800160616     C                   MOVE      RSES          N77SES
029900160616     C                   MOVE      RCIT          N77CIT
030000160616     C                   MOVE      RVIA          N77VIA
030300160616     C                   MOVE      RC77          N77C77
030400160616     C                   Z-ADD     RITC          N77ITC
030500160616     C                   Z-ADD     RIMN          N77IMN
030600160616     C                   Z-ADD     RSNS          N77SNS
030700160616     C                   Z-ADD     RIMS          N77IMS
030800160616     C                   Z-ADD     RRAC          N77RAC
030900160616     C                   Z-ADD     RNET          N77NET
031000160616     C                   MOVE      RPRA          N77PRA
031100160616     C                   MOVE      RPFG          N77PFG
031300160616     C                   MOVE      RCSE          N77CSE
031400160616     C                   MOVE      RCDE          N77CDE
031500160616     C                   MOVE      RPRG          N77PRG
031700160616     C*
031800160616     C                   ENDSR                                                  *
031900160616     C*----------------------------------------------------*
032000160616     C* SUBROUTINE SCRITTURA FILE DA DS MULTIPLA           *
032100160616     C*----------------------------------------------------*
032200160616     C     SCRIVI        BEGSR                                                  *
032300160616     C*
032400160616     C                   MOVE      SOC           N77SOCIETA                     *
032500160616     C                   Z-ADD     ACF           N77ACF                         *
032600160616     C                   MOVE      QUA           N77QUA                         *
032700160616     C                   MOVE      KCC           N77KCC                         *
032800160616     C                   MOVE      KSC           N77KSC                         *
032900160616     C                   MOVE      RGT           N77RGT                         *
033000160616     C                   MOVE      CDF           N77CDF                         *
033100160616     C                   MOVE      COG           N77COG                         *
033200160616     C                   MOVE      NOM           N77NOM                         *
033400160616     C                   MOVE      DNC           N77DNC                         *
033600160616     C                   MOVE      SES           N77SES                         *
033700160616     C                   MOVE      CIT           N77CIT                         *
033800160616     C                   MOVE      VIA           N77VIA                         *
034500160616     C                   MOVEL     C77           N77C77                         *
034600160616     C                   Z-ADD     ITC           N77ITC                         *
034700160616     C                   Z-ADD     IMN           N77IMN                         *
034800160616     C                   Z-ADD     SNS           N77SNS                         *
034900160616     C                   Z-ADD     IMS           N77IMS                         *
035000160616     C                   Z-ADD     PRA           N77PRA                         *
035100160616     C                   Z-ADD     RAC           N77RAC                         *
035200160616     C                   Z-ADD     NET           N77NET                         *
035300160616     C                   MOVE      PFG           N77PFG                         *
035500160616     C                   MOVEL     CSE           N77CSE
036000160616     C                   MOVEL     CDE           N77CDE
036100160616     C                   MOVEL     PRG           N77PRG
036600160616     C                   WRITE     SDGN77000                                    *
036700160616     C*
036800160616     C                   ENDSR                                                  *
