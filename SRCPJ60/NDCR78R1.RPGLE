000100990506     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200990506     H*PARMS BNDDIR(PJXBND PJCBND)
000300990506     H DECEDIT('0,') DATEDIT(*DMY/)
000400990506     F*---------------------------------------------------------------*H
000500990506     F* NDCR78r1  *      STAMPA MOD.770 QUADRO  --- SC   ---             *
000600990506     F*---------------------------------------------------------------*
000700990506     F*                                                               *
000800990506     F*                                                               *
000900990506     F*---------------------------------------------------------------*
001000990506     FND77005L  IF   E           K DISK
001100990507     Fqsysprt   O    F   81        PRINTER OFLIND(*INOA) formlen(70) formofl(70)
001200990506     F*----------------------------------------------------*     B
001300990506     D CFD             S              1    DIM(16)
001400990506     D*------------
001500990506     D KPJBA         E DS
001600990506     D*------------
001700990506     D ndcr77ds      E DS
001800990506     D*------------
001900990506     D* Dati di ambiente ottenuti da XSOC
002000990506     D SOC001        E DS                  EXTNAME(XSOC001DS)
002100990506     D*-------------
002200990506     D* DS Interna per dati di output di XSOC
002300990506     D XSOCDS          DS          1000
002400990506     D*------------
002500990506     C*----------------------------------------------------*
002600990506     C*
002700990506     C                   EXSR      INZVAR                                       *
002800990506     C*
002900990506     C*
003000990506     C*
003100990507     C                   MOVE      SocietaR77    N77Societa                     *
003200990507     C                   z-add     AnnoR77       N77ACF                         *
003300990507     C                   MOVE      QuaR77        N77qua                         *
003400990506     C     K03N7705      SETLL     nd77005L
003500990506     C     K03N7705      READE     nd77005L                               21
003600990506     C                   DOW       *in21   = *off
003700990506     C* --                                                 *
003800990506?    C* ----C I C L O    D I    S T A M P A----------------*
003900990506     C* --                                                 *
004000990506     C                   Z-ADD     0             CTR2              3 0          *
004100990506     C                   Z-ADD     1             FOGLIO            4 0          *
004200990506     C                   EXCEPT    RIPO1                                        *
004300990506     C                   Z-ADD     0             TR2IMT           11 0          *
004400990506     C                   Z-ADD     0             TR2NSO           12 0          *
004500990506     C                   Z-ADD     0             TR1NSO           12 0          *
004600990506     C                   Z-ADD     0             TR2IMS           10 0          *
004700990506     C                   Z-ADD     0             TR2RAC           11 0          *
004800990506     C                   Z-ADD     0             TR2NET           11 0          *
004900990506     C*
005000990506     C                   DOW       *in21   = *off
005100990506     C*
005200990506?    C* Dettaglio
005300990506     C                   ADD       1             CTR2                           *
00540099050677098C                   ADD       1             CTR               5 0          *
00550099050677098C                   MOVEL     N77SER        SER              33
00560099050677098C                   MOVEL     N77CSE        CSE               4
00570099050677098C                   MOVEL     N77CDE        CDE              20
00580099050677098C                   MOVEL     N77VIA        VIA              30
00590099050677098C                   MOVEL     N77CIT        CIT              29
00600099050677098C                   MOVEL     N77COG        COG              46            *
00610099050677098C                   MOVEL     N77NOM        NOM              17            *
006200990506     C     CTR2          COMP      1                                      20
006300990506     C* --                                                 *
006400990506     C     N77PFG        IFEQ      'F'
006500990506     C                   EXSR      RIGA2                                        *
006600990506     C                   EXCEPT    DET2
006700990506     C                   ENDIF
006800990506     C* --                                                 *
006900990506?    C* ---------- Stampa Percipienti Pers.Non Fisiche-----*
007000990506     C     N77PFG        IFEQ      'G'
007100990506     C                   EXSR      RIGA3                                        *
007200990506     C                   EXCEPT    DET3
007300990506     C                   ENDIF
007400990506     C* --                                                 *
007500990506?    C* ---------- FINE PRIMA PAGINA                  -----*
00760099050677098C     CTR2          IFEQ      4
007700990506     C                   EXCEPT    BIANC1
007800990506     C                   ENDIF
007900990506     C*
008000990506     c     K03N7705      READE     nd77005L                               21
008100990507     C* --                                                 *
008200990507?    C* ---------- FINE SECONDA PAGINA                -----*
00830099050777098C     CTR2          IFEQ      9
00840099050777098C     *in21         Andeq     '0'
008500990507     C                   ADD       1             FOGLIO
008600990507     C                   Z-ADD     0             CTR2
008700990507     C                   EXCEPT    RIPO1
008800990507     C                   ENDIF
008900990506     C                   EnddO                                                  *
009000990506     C                   EnddO                                                  *
009100990506     C*
009200990507     C* fine programma
009300990507     C                   EXSR      ENDPGM
009400990506      /EJECT
009500990506     C************************************************************
009600990506     C* SUBROUTINE CALCOLI DETTAGLIO PERSONE FISICHE       *
009700990506     C************************************************************
009800990506     C     RIGA2         BEGSR                                                  *
009900990506     C* --                                                 *
010000990506     C* Totalizzazioni e troncamenti                       *
010100990506     C                   Z-ADD     0             IMT2             11 0          *
010200990506     C                   Z-ADD     0             NSO2             12 0          *
010300990506     C                   Z-ADD     0             IMS2             10 0          *
010400990506     C                   Z-ADD     0             RAC2             11 0          *
010500990506     C                   Z-ADD     0             NET2             11 0          *
010600990506     C                   Z-ADD     0             NSO              15 0          *
010700990506     C* --                                                 *
010800020502     C**** N77ITC        DIV       1000          IMT2                           *
010900020502     C**** N77SNS        ADD       N77IMN        NSO                            *
011000020502     C**** NSO           DIV       1000          NSO2                           *
011100020502     C**** N77IMS        DIV       1000          IMS2                           *
011200020502     C**** N77RAC        DIV       1000          RAC2                           *
011300020502     C**** N77NET        DIV       1000          NET2                           *
011400020502     C* --                                                 *
011500020502     C                   Z-ADD     N77ITC        IMT2                           *
011600020502     C     N77SNS        ADD       N77IMN        NSO                            *
011700020502     C                   Z-ADD     NSO           NSO2                           *
011800020502     C                   Z-ADD     N77IMS        IMS2                           *
011900020502     C                   Z-ADD     N77RAC        RAC2                           *
012000020502     C                   Z-ADD     N77NET        NET2                           *
012100990506     C*
012200990506MP01 C                   SETOFF                                       101112
012300990506MP01 C     N77NUM        COMP      *BLANK                             1010
012400990506MP01 C   10N77RAC        COMP      0                                      11
012500990506MP01 C   10N77IMN        COMP      0                                  12
012600990506MP01 C   10N77SNS        COMP      0                                  15
012700990506     C* --
012800990506     C                   ADD       N77ITC        TIMT2            15 0
012900990506     C* ITALIA                                            CAS.17
013000990506     C  N10              ADD       NSO           TNSO1
013100990506     C* STRANIERO SENZA RITENUTE SENZA IMP. NON SOGGETTO  CAS.17
013200990506     C   10
013300990506     CAN 11
013400990506     CANN12              ADD       NSO           TNSO1            15 0
013500990506     C* STRANIERO SENZA RITENUTE E CON IMP. NON SOGGETTTO CAS.18
013600990506     C   10
013700990506     CAN 11
013800990506     CAN 12              ADD       NSO           TNSO2            15 0
013900990506     C* STRANIERO CON RITENUTE                            CAS.17
014000990506     C   10
014100990506     CANN11              ADD       NSO           TNSO1            15 0
014200990506     C* --
014300990506     C                   ADD       N77IMS        TIMS2            15 0
014400990506     C                   ADD       N77RAC        TRAC2            15 0
014500990506     C                   ADD       N77NET        TNET2            15 0
014600990506     C* --
014700990506     C* -- Inversione data di nascita                      *
01480099050677098C                   EXTRCT    N77DNC:*Y     AN4DNC            4 0
01490099050677098C                   MOVE      AN4DNC        AN3DNC            3 0
01500099050677098C                   EXTRCT    N77DNC:*D     GIODNC            2 0
01510099050677098C                   EXTRCT    N77DNC:*M     MESDNC            2 0
015200990506     C* --                                                 *
01530099050677098C                   MOVEL     N77LNC        LNC              28            *
015400990506     C* --                                                 *
015500990506     C* -- Percipiente Estero N77NUM = *                   *
015600990506      *
015700990506     C     N77NUM        IFEQ      *BLANK                                       *
015800990506     C                   MOVEL     N77PRV        PRV               2            *
015900990506     C                   MOVEL     *BLANK        NR                2            *
016000990506     C                   ELSE                                                   *
016100990506     C                   MOVEL     *BLANK        PRV                            *
016200990506     C                   MOVE      '/'           NR                             *
016300990506     C                   END                                                    *
016400990506     C                   MOVEL     N77C77        C77               1            *
016500990506     C                   Z-ADD     0             PRA               4 2          *
016600990506     C                   Z-ADD     N77PRA        PRA                            *
016700990506     C                   ENDSR                                                  *
016800990506     C************************************************************
016900990506     C* SUBROUTINE CALCOLI DETTAGLIO PERSONE NON FISICHE   *
017000990506     C************************************************************
017100990506     C     RIGA3         BEGSR                                                  *
017200990506     C* --                                                 *
017300990506     C* Totalizzazioni e troncamenti                       *
017400990506     C* --                                                 *
017500990506     C                   Z-ADD     0             IMT2             11 0          *
017600990506     C                   Z-ADD     0             NSO2             12 0          *
017700990506     C                   Z-ADD     0             IMS2             10 0          *
017800990506     C                   Z-ADD     0             RAC2             11 0          *
017900990506     C                   Z-ADD     0             NET2             11 0          *
018000990506     C                   Z-ADD     0             NSO              15 0          *
018100990506     C* --                                                 *
018200020502     C**** N77ITC        DIV       1000          IMT2                           *
018300020502     C**** N77SNS        ADD       N77IMN        NSO                            *
018400020502     C**** NSO           DIV       1000          NSO2                           *
018500020502     C**** N77IMS        DIV       1000          IMS2                           *
018600020502     C**** N77RAC        DIV       1000          RAC2                           *
018700020502     C**** N77NET        DIV       1000          NET2                           *
018800020502     C*                                                    *
018900020502     C                   Z-ADD     N77ITC        IMT2                           *
019000020502     C     N77SNS        ADD       N77IMN        NSO                            *
019100020502     C                   Z-ADD     NSO           NSO2                           *
019200020502     C                   Z-ADD     N77IMS        IMS2                           *
019300020502     C                   Z-ADD     N77RAC        RAC2                           *
019400020502     C                   Z-ADD     N77NET        NET2                           *
019500990506     C*
019600990506MP01 C                   SETOFF                                       101112
019700990506MP01 C     N77NUM        COMP      *BLANK                             1010
019800990506MP01 C   10N77RAC        COMP      0                                      11
019900990506MP01 C   10N77IMN        COMP      0                                  12
020000990506     C* --                                                 *
020100990506     C                   ADD       N77ITC        TIMT2            15 0          *
020200990506     C* ITALIA                                            CAS.17
020300990506     C  N10              ADD       NSO           TNSO1
020400990506     C* STRANIERO SENZA RITENUTE SENZA IMP. NON SOGGETTO  CAS.17
020500990506     C   10
020600990506     CAN 11
020700990506     CANN12              ADD       NSO           TNSO1            15 0
020800990506     C* STRANIERO SENZA RITENUTE E CON IMP. NON SOGGETTTO CAS.18
020900990506     C   10
021000990506     CAN 11
021100990506     CAN 12              ADD       NSO           TNSO2            15 0
021200990506     C* STRANIERO CON RITENUTE                            CAS.17
021300990506     C   10
021400990506     CANN11              ADD       NSO           TNSO1            15 0
021500990506     C* --                                                 *
021600990506     C                   ADD       N77IMS        TIMS2            15 0          *
021700990506     C                   ADD       N77RAC        TRAC2            15 0          *
021800990506     C                   ADD       N77NET        TNET2            15 0          *
021900990506     C* --                                                 *
022000990506     C                   MOVEL     N77CDF        CDF              17            *
022100990506     C                   MOVEL     N77COG        RAG3             46            *
022200990506     C* --                                                 *
022300990506     C     N77NUM        IFEQ      *BLANK                                       *
022400990506     C                   MOVEL     N77PRV        PRV                            *
022500990506     C                   MOVEL     *BLANK        NR                             *
022600990506     C                   ELSE                                                   *
022700990506     C                   MOVEL     *BLANK        PRV                            *
022800990506     C                   MOVE      '/'           NR                             *
022900990506     C                   END                                                    *
023000990506     C                   MOVEL     N77C77        C77                            *
023100990506     C                   Z-ADD     0             PRA               4 2          *
023200990506     C                   Z-ADD     N77PRA        PRA                            *
023300990506     C                   ENDSR                                                  *
023400990506     C*----------------------------------------------------*
023500990506      /EJECT
023600990506     C************************************************************
023700990506     C* SUBROUTINE INIZIALE DEFINIZIONE CAMPI E CHIAVI     *
023800990506     C************************************************************
023900990506     C     INZVAR        BEGSR                                                  *
024000990506     C*
024100990506     C     *ENTRY        PLIST                                                  *
024200990506     C                   PARM                    KPJBA                          *
024300990506     C                   MOVEL     KPJBU         NDCR77DS                       *
024400990506     C*
024500990506     C     K03N7705      KLIST
024600990506     C                   KFLD                    N77Societa                     *
024700990506     C                   KFLD                    N77ACF                         *
024800990506     C                   KFLD                    N77QUA            2            *
024900990506     C*
025000020415     C                   MOVE      'AU'          QUA               2            *
025100990506     C                   MOVE      ' '           SALPAG            1
025200990506     C*
025300990506     C* Reperimento dati società
025400990506     C                   MOVEL     'SOC001'      TIPXSC
025500990506     C                   MOVEL     *BLANK        SOCXSC
025600990506     C                   EXSR      REPSOC
025700990507     C*
025800990507     C* Controlli dati società
025900990507     C     RTNXSC        IFNE      '1'
026000990507     C                   MOVEL     XSOCDS        SOC001
026100990507     C                   ELSE
026200990507     C                   EXSR      ENDPGM
026300990507     C                   ENDIF
026400990506     C*
026500990506     C* Vado al reperimento informazioni  sost.imposta
026600990506     C                   EXSR      DatiSost
026700990506      *
026800990506     C                   ENDSR                                                  *
026900990506      ************************************************************
027000990506      * Inizializzazione dati
027100990506      ************************************************************
027200990506     C     DatiSosT      BegSr
027300990506      *
027400990506     C                   If        XSCCDF        <> *blanks
027500990506     C                   MOVEL     XSCCDF        SOICDF           16            *COD.FISCALE
027600990506     C                   ElSe                                                   *
027700990506     C                   MOVEL     XSCIVA        SOICDF                         *COD.FISCALE
027800990506     C                   EndIf                                                  *
027900990506     C*
028000990506     C                   MOVEA     SOICDF        CFD
028100990506      *
028200990506     C     XDatiSost     EndSr
028300990506     C/EJECT
028400990506     C************************************************************
028500990506     C* REPERIMENTO DATI SOCIETA'
028600990506     C************************************************************
028700990506     C     REPSOC        BEGSR
028800990506     C*
028900990506     C                   CALLB     'XSOC'
029000990506     C                   PARM                    TIPXSC            6
029100990506     C                   PARM                    SOCXSC            3
029200990506     C                   PARM                    CDSXSC            9 0
029300990506     C                   PARM                    MODXSC            3
029400990506     C                   PARM      *BLANKS       RTNXSC            1
029500990506     C                   PARM                    XSOCDS
029600990506     C                   PARM                    KPJBA
029700990506     C*
029800990506     C                   ENDSR
029900990506     C/EJECT
030000990507     C************************************************************
030100990507     C* FINE PROGRAMMA
030200990507     C************************************************************
030300990507     C     ENDPGM        BEGSR
030400990507     C*
030500990507     C                   SETON                                            LR
030600990507     C                   RETURN
030700990507     C*
030800990507     C                   ENDSR
030900990507     C/EJECT
031000990506      ************************************************************
031100990507     Oqsysprt   E            RIPO1            03
031200990506     O                                            4 'X'
031300990506     O          E            RIPO1            06
031400990506     O                       CFD(1)              37
031500990506     O                       CFD(2)              39
031600990506     O                       CFD(3)              41
031700990506     O                       CFD(4)              43
031800990506     O                       CFD(5)              45
031900990506     O                       CFD(6)              47
032000990506     O                       CFD(7)              49
032100990506     O                       CFD(8)              51
032200990506     O                       CFD(9)              53
032300990506     O                       CFD(10)             55
032400990506     O                       CFD(11)             57
032500990506     O                       CFD(12)             59
032600990506     O                       CFD(13)             61
032700990506     O                       CFD(14)             63
032800990506     O                       CFD(15)             65
032900990506     O                       CFD(16)             67
033000990506     O          E            RIPO1            10 14
033100990506     O                       FOGLIO              67 '   & '
033200990506     C*------------ EXCPT DI POSIZIONAMENTO SU PERCIPIENTI *           --*
033300990506     O          E            BIANC1           04
033400990506     C*------------ DETTAGLIO PERC. PERSONE FISICHE -------*           --*
033500990506     O          E            DET2        2
033600990507     O                       N77CDF              35
033700990506     O                       COG                 79
033800990506     O          E    20      DET2        1
033900990506     O          E            DET2        2
034000990506     O                       NOM                 32
034100990506     O                       N77SES              35
034200990506     O                       GIODNC              39
034300990506     O                       MESDNC              42
034400990506     O                       AN3DNC              46
034500990506     O                       LNC                 75
034600990506     O                       N77PNC              79
034700990506     O          E            DET2        2
034800990506     O                       CIT                 44
034900990506     O                       PRV                 48
035000990506     O                       VIA                 79
035100990506     O          E            DET2        2
035200990506     O                       NR                  19
035300990506     O                       SER                 53
035400990506     O                       CSE                 58
035500990506     O                       CDE                 79
035600990506     O          E    20      DET2        1
035700990506     O          E            DET2        2
035800990506     O                       C77                 17
035900990506     O                       IMT2          Z     30
036000990506     O                    N10NSO2          Z     42
036100990506     O               10 11N12NSO2          Z     42
036200990506     O               10N11   NSO2          Z     42
036300990506     O               10 11 12NSO2          Z     53
036400990506     O                       IMS2          Z     66
036500990506     O          E            DET2        2
036600990506     O                       RAC2          Z     26
036700990506     O                       NET2          Z     38
036800990506     C*------------ DETTAGLIO PERC. PERSONE GIURIDICHE-----*           --*
036900990506     O          E            DET3        2
037000990507     O                       N77CDF              35
037100990506     O                       COG                 79
037200990506     O          E            DET3        2
037300990506     O          E            DET3        2
037400990506     O                       CIT                 44
037500990506     O                       N77PRV              47
037600990506     O                       VIA                 79
037700990506     O          E    20      DET3        1
037800990506     O          E            DET3        2
037900990506     O                       NR                  19
038000990506     O                       SER                 53
038100990506     O                       CSE                 58
038200990506     O                       CDE                 79
038300990506     O          E    20      DET3        1
038400990506     O          E            DET3        2
038500990506     O                       C77                 17
038600990506     O                       IMT2          Z     30
038700990506     O                    N10NSO2          Z     42
038800990506     O               10 11N12NSO2          Z     42
038900990506     O                  10N11NSO2          Z     42
039000990506     O               10 11 12NSO2          Z     54
039100990506     O                       IMS2          Z     66
039200990506     O          E            DET3        2
039300990506     O                       RAC2          Z     26
039400990506     O                       NET2          Z     39
039500990506     C*
039600990506     C************************************************************
