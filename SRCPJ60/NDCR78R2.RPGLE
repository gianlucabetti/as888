000100990507     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200990507     H*PARMS BNDDIR(PJXBND PJCBND)
000300990507     H DECEDIT('0,') DATEDIT(*DMY/)
000400990507     F*---------------------------------------------------------------*H
000500990507     F* NDCR78R2  *      STAMPA MOD.770 QUADRO  --- SE ---            *
000600990507     F*---------------------------------------------------------------*
000700990507     F*                                                               *
000800990507     F*   INDICATORI :                                                *
000900990507     F*                                                               *
001000990507     F*                                                               *
001100990507     F*---------------------------------------------------------------*
001200990507     FND77005L  IF   E           K DISK
001300990507     Fqsysprt   O    F   81        PRINTER OFLIND(*INOA) formlen(70) formofl(70)
001400990507     F*---------------------------------------------------------------*
001500990507     D CFD             S              1    DIM(16)
001600990507     D*------------
001700990507     D KPJBA         E DS
001800990507     D*------------
001900990507     D ndcr77ds      E DS
002000990507     D*------------
002100990507     D* Dati di ambiente ottenuti da XSOC
002200990507     D SOC001        E DS                  EXTNAME(XSOC001DS)
002300990507     D*-------------
002400990507     D* DS Interna per dati di output di XSOC
002500990507     D XSOCDS          DS          1000
002600990507     D*------------
002700990507     I*----------------------------------------------------*
002800990507      /EJECT
002900990507     C*
003000990507     C                   EXSR      INZVAR                                       *
003100990507     C*
003200990507     C*
003300990507     C                   MOVE      SocietaR77    N77Societa                     *
003400990507     C                   z-add     AnnoR77       N77ACF                         *
003500990507     C                   MOVE      QuaR77        N77qua                         *
003600990507     C     K03N7705      SETLL     nd77005L
003700990507     C     K03N7705      READE     nd77005L                               21
003800990507     C                   DOW       *in21   = *off
003900990507     C* --                                                 *
004000990507?    C* ----C I C L O    D I    S T A M P A----------------*
004100990507     C                   Z-ADD     0             CTR2              3 0          *
004200990507     C                   Z-ADD     1             FOGLIO            4 0          *
004300990507     C                   EXCEPT    RIPO1                                        *
004400990507     C                   Z-ADD     0             TR2IMT            9 0          *
004500990507     C                   Z-ADD     0             TR2IMN            9 0          *
004600990507     C                   Z-ADD     0             TR2SNS            9 0          *
004700990507     C                   Z-ADD     0             TR2IMS            9 0          *
004800990507     C                   Z-ADD     0             TR2RAC            9 0          *
004900990507     C                   Z-ADD     0             TR2NET            9 0          *
005000990507     C                   DOW       *in21   = *off
005100990507     C*
005200990507?    C* Dettaglio
005300990507     C                   ADD       1             CTR2                           *
00540099050777098C                   ADD       1             CTR               5 0          *
00550099050777098C                   MOVEL     N77SER        SER              33
00560099050777098C                   MOVEL     N77CSE        CSE               4
00570099050777098C                   MOVEL     N77CDE        CDE              20
00580099050777098C                   MOVEL     N77VIA        VIA              30
00590099050777098C                   MOVEL     N77CIT        CIT              29
006000990507     C     CTR2          COMP      1                                      20
006100990507?    C* ---------- Stampa Percipienti Pers.Fisiche---------*
006200990507     C     N77PFG        IFEQ      'F'
006300990507     C                   EXSR      RIGA2                                        *
006400990507     C                   EXCEPT    DET2                                         *
006500990507     C                   ENDIF
006600990507     C* --                                                 *
006700990507?    C* ---------- Stampa Percipienti Pers.Non Fisiche-----*
006800990507     C     N77PFG        IFEQ      'G'
006900990507     C                   EXSR      RIGA3                                        *
007000990507     C                   EXCEPT    DET3                                         *
007100990507     C                   ENDIF
007200990507     C* --                                                 *
007300990507?    C* ---------- FINE PRIMA PAGINA                  -----*
007400990507     C     CTR2          IFEQ      4                                            *
007500990507     C                   EXCEPT    BIANC1                                       *
007600990507     C                   ENDIF
007700990507     C* --
007800990507?    C* ---------- FINE SECONDA PAGINA                -----*
007900990507     C     CTR2          IFEQ      10                                           *
008000990507     C                   ADD       1             FOGLIO
008100990507     C                   Z-ADD     0             CTR2
008200990507     C                   EXCEPT    RIPO1
008300990507     C                   ENDIF
008400990507     C*
008500990507     c     K03N7705      READE     nd77005L                               21
008600990507     C                   EnddO                                                  *
008700990507     C                   EnddO                                                  *
008800990507     C*
008900990507     C* fine programma
009000990507     C                   EXSR      ENDPGM
009100990507     C*
009200990507     C*----------------------------------------------------*
009300990507     C* SUBROUTINE CALCOLI DETTAGLIO PERSONE FISICHE       *
009400990507     C*----------------------------------------------------*
009500990507     C     RIGA2         BEGSR                                                  *
009600990507     C* --                                                 *
009700990507     C* Totalizzazioni e troncamenti                       *
009800990507%%   C                   Z-ADD     0             IMT2              9 0          *
009900990507%%   C                   Z-ADD     0             IMN2              9 0          *
010000990507%%   C                   Z-ADD     0             SNS2              9 0          *
010100990507%%   C                   Z-ADD     0             IMS2              9 0          *
010200990507%%   C                   Z-ADD     0             RAC2              9 0          *
010300990507%%   C                   Z-ADD     0             NET2              9 0          *
010400990507     C* --                                                 *
010500990507     C                   ADD       N77ITC        TIMT2            15 0          *
010600990507     C                   ADD       N77IMN        TIMN2            15 0          *
010700990507     C                   ADD       N77SNS        TSNS2            15 0          *
010800990507     C                   ADD       N77IMS        TIMS2            15 0          *
010900990507     C                   ADD       N77RAC        TRAC2            15 0          *
011000990507     C                   ADD       N77NET        TNET2            15 0          *
011100990507     C* --                                                 *
011200990507     C     N77ITC        DIV       1000          IMT2                           *
011300990507     C     N77IMN        DIV       1000          IMN2                           *
011400990507     C     N77SNS        DIV       1000          SNS2                           *
011500990507     C     N77IMS        DIV       1000          IMS2                           *
011600990507     C     N77RAC        DIV       1000          RAC2                           *
011700990507     C     N77NET        DIV       1000          NET2                           *
011800990507     C* --                                                 *
011900990507     C                   MOVEL     N77COG        COG              46            *
012000990507     C                   MOVEL     N77NOM        NOM              17            *
012100990507     C* Inversione data di quietanza                    *
01220099050777098C                   EXTRCT    N77DNC:*Y     AN4DNC            4 0
01230099050777098C                   MOVE      AN4DNC        AN3DNC            3 0
01240099050777098C                   EXTRCT    N77DNC:*D     GIODNC            2 0
01250099050777098C                   EXTRCT    N77DNC:*M     MESDNC            2 0
012600990507     C*                                                    *
012700990507     C                   MOVEL     N77LNC        LNC              28            *
012800990507     C                   MOVEL     N77VIA        VIA              30            *
012900990507     C                   MOVEL     N77CIT        CIT              29            *
013000990507     C                   MOVEL     N77PRV        PRV               2            *
013100990507     C******               MOVELN77C77    C77    11        *
013200990507R32  C                   MOVEL     N77C77        C77               1            *
013300990507%%   C                   Z-ADD     0             PRA               4 2          *
013400990507%%   C                   Z-ADD     N77PRA        PRA                            *
013500990507     C                   ENDSR                                                  *
013600990507     C*----------------------------------------------------*
013700990507     C* SUBROUTINE CALCOLI DETTAGLIO PERSONE NON FISICHE   *
013800990507     C*----------------------------------------------------*
013900990507     C     RIGA3         BEGSR                                                  *
014000990507     C* --                                                 *
014100990507     C* Totalizzazioni e troncamenti                       *
014200990507     C                   Z-ADD     0             IMT2                           *
014300990507     C                   Z-ADD     0             IMN2                           *
014400990507     C                   Z-ADD     0             SNS2                           *
014500990507     C                   Z-ADD     0             IMS2                           *
014600990507     C                   Z-ADD     0             RAC2                           *
014700990507     C                   Z-ADD     0             NET2                           *
014800990507     C* --                                                 *
014900990507     C                   ADD       N77ITC        TIMT2                          *
015000990507     C                   ADD       N77IMN        TIMN2                          *
015100990507     C                   ADD       N77SNS        TSNS2                          *
015200990507     C                   ADD       N77IMS        TIMS2                          *
015300990507     C                   ADD       N77RAC        TRAC2                          *
015400990507     C                   ADD       N77NET        TNET2                          *
015500990507     C* --                                                 *
015600990507     C     N77ITC        DIV       1000          IMT2                           *
015700990507     C     N77IMN        DIV       1000          IMN2                           *
015800990507     C     N77SNS        DIV       1000          SNS2                           *
015900990507     C     N77IMS        DIV       1000          IMS2                           *
016000990507     C     N77RAC        DIV       1000          RAC2                           *
016100990507     C     N77NET        DIV       1000          NET2                           *
016200990507     C* --                                                 *
016300990507R4   C                   MOVEL     N77CDF        CDF              16            *
016400990507R4   C                   MOVEL     N77COG        RAG3             46            *
016500990507     C                   MOVEL     N77CIT        CIT3             29            *
016600990507     C                   MOVEL     N77PRV        PRV                            *
016700990507     C                   MOVEL     N77VIA        VIA3             30            *
016800990507     C                   MOVEL     N77C77        C77                            *
016900990507%%   C                   Z-ADD     0             PRA                            *
017000990507%%   C                   Z-ADD     N77PRA        PRA                            *
017100990507     C                   ENDSR                                                  *
017200990507     C*----------------------------------------------------*
017300990507      /EJECT
017400990507     C*----------------------------------------------------*
017500990507     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
017600990507     C*----------------------------------------------------*
017700990507     C     INZVAR        BEGSR                                                  *
017800990507     C*
017900990507     C     *ENTRY        PLIST                                                  *
018000990507     C                   PARM                    KPJBA                          *
018100990507     C                   MOVEL     KPJBU         NDCR77DS                       *
018200990507     C*
018300990507     C     K03N7705      KLIST
018400990507     C                   KFLD                    N77Societa                     *
018500990507     C                   KFLD                    N77ACF                         *
018600990507     C                   KFLD                    N77QUA            2            *
018700990507     C*
018800990507     C                   MOVE      'SE'          QUA               2            *
018900990507     C*
019000990507     C* Reperimento dati società
019100990507     C                   MOVEL     'SOC001'      TIPXSC
019200990507     C                   MOVEL     *BLANK        SOCXSC
019300990507     C                   EXSR      REPSOC
019400990507     C*
019500990507     C* Controlli dati società
019600990507     C     RTNXSC        IFNE      '1'
019700990507     C                   MOVEL     XSOCDS        SOC001
019800990507     C                   ELSE
019900990507     C                   EXSR      ENDPGM
020000990507     C                   ENDIF
020100990507     C*
020200990507     C* Vado al reperimento informazioni  sost.imposta
020300990507     C                   EXSR      DatiSost
020400990507      *
020500990507     C                   ENDSR                                                  *
020600990507      ************************************************************
020700990507      * Inizializzazione dati
020800990507      ************************************************************
020900990507     C     DatiSosT      BegSr
021000990507      *
021100990507     C                   If        XSCCDF        <> *blanks
021200990507     C                   MOVEL     XSCCDF        SOICDF           16            *COD.FISCALE
021300990507     C                   ElSe                                                   *
021400990507     C                   MOVEL     XSCIVA        SOICDF                         *COD.FISCALE
021500990507     C                   EndIf                                                  *
021600990507     C*
021700990507     C                   MOVEA     SOICDF        CFD
021800990507      *
021900990507     C     XDatiSost     EndSr
022000990507     C/EJECT
022100990507     C************************************************************
022200990507     C* REPERIMENTO DATI SOCIETA'
022300990507     C************************************************************
022400990507     C     REPSOC        BEGSR
022500990507     C*
022600990507     C                   CALLB     'XSOC'
022700990507     C                   PARM                    TIPXSC            6
022800990507     C                   PARM                    SOCXSC            3
022900990507     C                   PARM                    CDSXSC            9 0
023000990507     C                   PARM                    MODXSC            3
023100990507     C                   PARM      *BLANKS       RTNXSC            1
023200990507     C                   PARM                    XSOCDS
023300990507     C                   PARM                    KPJBA
023400990507     C*
023500990507     C                   ENDSR
023600990507     C/EJECT
023700990507     C************************************************************
023800990507     C* FINE PROGRAMMA
023900990507     C************************************************************
024000990507     C     ENDPGM        BEGSR
024100990507     C*
024200990507     C                   SETON                                            LR
024300990507     C                   RETURN
024400990507     C*
024500990507     C                   ENDSR
024600990507     C/EJECT
024700990507      ************************************************************
024800990507     Oqsysprt   E            RIPO1            03
024900990507     O                                            4 'X'
025000990507     O          E            RIPO1            06
025100990507     O                       CFD(1)              37
025200990507     O                       CFD(2)              39
025300990507     O                       CFD(3)              41
025400990507     O                       CFD(4)              43
025500990507     O                       CFD(5)              45
025600990507     O                       CFD(6)              47
025700990507     O                       CFD(7)              49
025800990507     O                       CFD(8)              51
025900990507     O                       CFD(9)              53
026000990507     O                       CFD(10)             55
026100990507     O                       CFD(11)             57
026200990507     O                       CFD(12)             59
026300990507     O                       CFD(13)             61
026400990507     O                       CFD(14)             63
026500990507     O                       CFD(15)             65
026600990507     O                       CFD(16)             67
026700990507     O          E            RIPO1            10 14
026800990507     O                       FOGLIO              67 '   & '
026900990507     C*------------ EXCPT DI POSIZIONAMENTO SU PERCIPIENTI *           --*
027000990507     O          E            BIANC1           04
027100990507     C*------------ DETTAGLIO PERC. PERSONE FISICHE -------*           --*
027200990507     O          E            DET2        2
027300990507     O                       N77CDF              35
027400990507     O                       COG                 79
027500990507     O          E    20      DET2        1
027600990507     O          E            DET2        2
027700990507     O                       NOM                 32
027800990507     O                       N77SES              34
027900990507     O                       GIODNC              39
028000990507     O                       MESDNC              42
028100990507     O                       AN3DNC              46
028200990507     O                       LNC                 75
028300990507     O                       N77PNC              79
028400990507     O          E    20      DET2        1
028500990507     O          E            DET2        2
028600990507     O                       CIT                 44
028700990507     O                       PRV                 48
028800990507     O                       VIA                 79
028900990507     O          E    20      DET2        1
029000990507     O          E            DET2        2
029100990507     O                       C77                 17
029200990507     O                       IMT2          Z     30
029300990507     O                       SNS2          Z     42
029400990507     O                       IMN2          Z     54
029500990507     O                       IMS2          Z     66
029600990507     O          E    20      DET2        1
029700990507     O          E            DET2        2
029800990507     O                       RAC2          Z     26
029900990507     O                       NET2          Z     38
030000990507     C*------------ DETTAGLIO PERC. PERSONE GIURIDICHE ----*           --*
030100990507     O          E            DET3        2
030200990507     O                       CDF                 35
030300990507     O                       RAG3                79
030400990507     O          E    20      DET3        1
030500990507     O          E            DET3        2
030600990507     O          E    20      DET3        1
030700990507     O          E            DET3        2
030800990507     O                       CIT3                44
030900990507     O                       N77PRV              48
031000990507     O                       VIA3                79
031100990507     O          E    20      DET3        1
031200990507     O          E            DET3        2
031300990507     O                       C77                 17
031400990507     O                       IMT2          Z     30
031500990507     O                       SNS2          Z     42
031600990507     O                       IMN2          Z     54
031700990507     O                       IMS2          Z     66
031800990507     O          E    20      DET3        1
031900990507     O          E            DET3        2
032000990507     O                       RAC2          Z     26
032100990507     O                       NET2          Z     38
