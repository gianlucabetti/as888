000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*CALLER)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*----------------------------------------------------*
000400000000     H*
000500000000     H*         ------------------------------------------------
000600000000     H*                        Spesometro 2013
000700000000     H*                    STAMPA LISTA DI CONTROLLO
000800000000     H*         ------------------------------------------------
000900000000     F*----------------------------------------------------*
001000000000     F* File contenente i dati da trasmettere all'agenzia
001100000000     FSDGETE00F IF   E             DISK    UsrOpn
001200000000     F*----------------
001300000000     FSDGSG61P2 O    E             PRINTER OFLIND(*IN39)
001400000000     D*-
001500000000     D*----------------------------------------------------
001600000000     D*----------------
001700000000     D* Parametri ricevuti
001800000000     D SDGSG61DS     E DS
001900151203R195 X****-------------
002000151203 "   X**** Parametro modulo
002100151203R195 X**** SDGPA08DS     E DS
002200000000     D*-------------
002300000000     D* Dati di ambiente ottenuti da SDGSOCDR
002400000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
002500000000     D*-------------
002600000000     D* DS Interna per dati di output di SDGSOCDR
002700000000     D XSOCDS          S           1000
002800000000     D*----------------
002900000000     D* Reperimento nome PGM
003000000000     D STATUS         SDS           333
003100000000     D  DSPGM            *PROC
003200000000     D*------------
003300000000     D* Passaggio Parametri
003400000000     D KPJBA         E DS
003500000000     D*-------------
003600000000     D DATIREC         DS
003700000000     D  TREC                   1      1
003800000000     D  TQUA                  90     91
003900000000     D*-------------
004000000000     D DATICAS         DS
004100000000     D DATIETW1                1     89
004200000000R141 D DATIPMOD               18     25
004300000000     D DATIETW2               90   1890
004400000000     D*-------------
004500000000     D SKDATI          S             24    DIM(75)
004600000000     D SVContaRigo     S              3  0
004601150306R176 D SVTqua          S                   like(TQUA)
004700000000     D*-------------
004800000000     D CampoWrk        DS
004900000000     D  Quadro                 1      2
005000000000     D  ContaRigo              3      5  0
005100000000     D  ContaColo              6      8  0
005200000000     D  Campo_Valore           9     24
005300000000     D*-------------
005400000000     D DtGMA1          DS
005500000000     D  GMAGiorno1             1      2
005600000000     D  T1                     3      3
005700000000     D  GMAMese1               4      5
005800000000     D  T2                     6      6
005900000000     D  GMAAnno1               7     10
006000000000     D*-------------
006100000000     D DtGMA           DS
006200000000     D  GMAGiorno              1      2  0
006300000000     D  GMAMese                3      4  0
006400000000     D  GMAAnno                5      8  0
006500000000     D*-------------
006600000000     D SDGSG61DSA    E DS
006700000000     D*-
006800000000     D SDGSG61DSB    E DS
006900000000     D*-
007000000000     D SDGSG61DSE    E DS
007100000000     D*-
007200000000     D SDGSG61DSZ    E DS
007300000000     D*-
007301160204R195 D*-------------
007302160204 "   D* Sottotitolo di testata Black List
007303160204 "   D* "Spesometro"
007304160204 "   D SOTTITTE        C                   CONST('SDGMSG    *LIBL-
007305160204 "   D                                          SDC0087')
007306160204 "   D* Sottotitolo di testata Black List
007307160204 "   D* "Acquisti da san marino'
007308160204 "   D SOTTITTS        C                   CONST('SDGMSG    *LIBL-
007309160204 "   D                                          SDC0075')
007310160204 "   D* Sottotitolo di testata Black List
007311160204 "   D* "Black List"
007312160204 "   D SOTTITTB        C                   CONST('SDGMSG    *LIBL-
007313160204 "   D                                          SDC0076')
007314160204R195 D*-------------
007400000000     C*----------------------------------------------------*
007500000000     C*                MAIN LINE PROGRAM
007600000000     C*----------------------------------------------------*
007700000000     C*
007800000000     C* inizializzazione variabili
007900000000     C                   EXSR      INZVAR
008000000000     C*
008100000000     C                   OPEN      SDGETE00F
008200000000     C*
008300000000     C                   Seton                                        39
008400000000     C*
008500000000     C* -----------------------------------------------------
008600000000     C* Prima lettura
008700000000     C                   READ      SDGETE00F                              90
008800000000     C     *IN90         DOWEQ     *OFF
008900000000     C*
009000000000     C                   Z-Add     0             K
009100000000     C*
009200000000     C                   MoveL     ETEREC        DATIREC
009201150306R176 C* ---------------------------
009202150306 "   C* a cambio quadro reset intestazioni
009203150306 "   C                   IF        TQUA <> SVTQUA
009204150306 "   C                   Setoff                                       404142
009205150306 "   C                   Setoff                                       434445
009206150306 "   C                   Setoff                                       464748
009207150306 "   C                   Setoff                                       4950
009208150306 "   C                   Movel     TQUA          SVTQUA
009209150306R176 C                   Endif
009300000000     C*
009400000000     C                   SELECT
009500000000     C* ---------------------------
009600000000     C* Tipo Rec.  A
009700000000     C                   WHEN      TREC    = 'A'
009800000000     C                   Exsr      ElaRecA
009900000000     C* ---------------------------
010000000000     C* Tipo Rec.  B
010100000000     C                   WHEN      TREC    = 'B'
010200000000     C                   Exsr      ElaRecB
010300000000     C* ---------------------------
010400000000     C* QUADRO    FA
010500000000     C                   WHEN      TQUA    = 'FA'
010600000000     C  N40              SetOn                                        39
010700000000     C  N40              SetOn                                        40
010800000000     C                   Exsr      ElaRecFA
010900000000     C* ---------------------------
011000000000     C* QUADRO    SA
011100000000     C                   WHEN      TQUA    = 'SA'
011200000000     C  N41              SetOn                                        39
011300000000     C  N41              SetOn                                        41
011400000000     C                   Exsr      ElaRecSA
011500000000     C*
011600000000     C* ---------------------------
011700000000     C* QUADRO    BL
011800000000     C                   WHEN      TQUA    = 'BL'
011900000000     C  N42              WRITE     SG61T1
012000140207R132 C**N42              SetOn                                        39
012100000000     C  N42              SetOn                                        42
012200000000     C                   Exsr      ElaRecBL
012300000000     C*
012400000000     C* ---------------------------
012500000000     C* QUADRO    FE
012600000000     C                   WHEN      TQUA    = 'FE'
012700000000     C  N43              SetOn                                        39
012800000000     C  N43              SetOn                                        43
012900000000     C                   Exsr      ElaRecFE
013000000000     C*
013100000000     C* ---------------------------
013200000000     C* QUADRO    FR
013300000000     C                   WHEN      TQUA    = 'FR'
013400000000     C  N44              SetOn                                        39
013500000000     C  N44              SetOn                                        44
013600000000     C                   Exsr      ElaRecFR
013700000000     C*
013800000000     C* ---------------------------
013900000000     C* QUADRO    NE
014000000000     C                   WHEN      TQUA    = 'NE'
014100000000     C  N45              SetOn                                        39
014200000000     C  N45              SetOn                                        45
014300000000     C                   Exsr      ElaRecNE
014400000000     C*
014500000000     C* ---------------------------
014600000000     C* QUADRO    NR
014700000000     C                   WHEN      TQUA    = 'NR'
014800000000     C  N46              SetOn                                        39
014900000000     C  N46              SetOn                                        46
015000000000     C                   Exsr      ElaRecNR
015100000000     C*
015200000000     C* ---------------------------
015300000000     C* QUADRO    DF
015400000000     C                   WHEN      TQUA    = 'DF'
015500000000     C  N47              SetOn                                        39
015600000000     C  N47              SetOn                                        47
015700000000     C                   Exsr      ElaRecDF
015800000000     C*
015900000000     C* ---------------------------
016000000000     C* QUADRO    FN
016100000000     C                   WHEN      TQUA    = 'FN'
016200000000     C  N48              SetOn                                        39
016300000000     C  N48              SetOn                                        48
016400000000     C                   Exsr      ElaRecFN
016500000000     C*
016600000000     C* ---------------------------
016700000000     C* QUADRO    SE
016800000000     C                   WHEN      TQUA    = 'SE'
016900000000     C  N49              SetOn                                        39
017000000000     C  N49              SetOn                                        49
017100000000     C                   Exsr      ElaRecSE
017200000000     C*
017300000000R136 C* ---------------------------
017400000000 "   C* QUADRO    TU
017500000000 "   C                   WHEN      TQUA    = 'TU'
017600000000 "   C  N51              SetOn                                        39
017700000000 "   C  N51              SetOn                                        51
017800000000 "   C                   Exsr      ElaRecTU
017900000000R136 C*
018000000000     C* ---------------------------
018100000000     C* QUADRO    TA
018200000000     C                   WHEN      TQUA    = 'TA'
018300000000     C  N50              SetOn                                        39
018400000000     C  N50              SetOn                                        50
018500000000     C                   Exsr      ElaRecTA
018600000000     C*
018700000000     C* ---------------------------
018800000000     C* Tipo Rec.  B
018900000000     C                   WHEN      TREC    = 'Z'
019000000000     C  N51              SetOn                                        39
019100000000     C  N51              SetOn                                        51
019200000000     C                   Exsr      ElaRecZ
019300000000     C*
019400000000     C* ---------------------------
019500000000     C*
019600000000     C                   EndSl
019700000000     C*
019800000000     C                   READ      SDGETE00F                              90
019900000000     C                   EndDo
020000000000     C*
020100000000     C                   CLOSE     SDGETE00F
020200000000     C*
020300000000     C* fine programma
020400000000     C                   Exsr      EndPgm
020500000000     C*
020600000000     C************************************************************
020700000000     C* Tipo REC A
020800000000     C************************************************************
020900000000     C     ElaRecA       BEGSR
021000000000     C*
021100000000     C                   MoveL     ETEREC        SDGSG61DSA
021200000000     C*
021300000000     C                   Eval      P610FOR     = G61AFOR
021400000000     C                   Eval      P610CFI     = G61ACFI
021500000000     C                   Eval      P610PRGINV  = G61APRGINV
021600000000     C                   Eval      P610TOTINV  = G61ATOTINV
021700000000     C*
021800000000     C   39              WRITE     SG61T1
021900000000     C   39              Setoff                                       39
022000000000     C                   WRITE     SG61TRA
022100000000     C*
022200000000     C                   ENDSR
022300000000     C************************************************************
022400000000     C* Tipo REC B
022500000000     C************************************************************
022600000000     C     ElaRecB       BEGSR
022700000000     C*
022800000000     C                   MoveL     ETEREC        SDGSG61DSB
022900000000     C*
023000000000     C****               Eval      BLUONAS     = G61BLUONAS
023100000000     C                   CAT       G61BLUONAS  :0BLUONAS
023200000000     C                   Eval      BPRVNAS     = BPRVNAS
023300000000     C****               Eval      BDENOMI     = G61BDENOMI
023400000000     C                   CAT       G61BDENOMI  :0BDENOMI
023500000000     C****               Eval      BLUONAS1    = G61BLUONA1
023600000000     C                   CAT       G61BLUONA1  :0BLUONAS1
023700000000     C                   Eval      BPRVNAS1    = G61BPRVNA1
023800000000     C****               Eval      BDENOMI1    = G61BDENOM1
023900000000     C                   CAT       G61BDENOM1  :0BDENOMI1
024000140220     C*
024100000000     C                   If        G61BDTNASC    <> *blanks
024200000000     C                   Move      G61BDTNASC    DTGMA
024300000000     C                   Move      GMAAnno       GMAAnno1
024400000000     C                   Move      GMAMese       GMAMese1
024500000000     C                   Move      GMAGiorno     GMAGiorno1
024600000000     C                   Move      DTGMA1        BDTNAS
024700000000     C                   EndIf
024800000000     C*
024900000000     C                   If        G61BDTNAS1    <> *blanks
025000000000     C                   Move      G61BDTNAS1    DTGMA
025100000000     C                   Move      GMAAnno       GMAAnno1
025200000000     C                   Move      GMAMese       GMAMese1
025300000000     C                   Move      GMAGiorno     GMAGiorno1
025400000000     C                   Move      DTGMA1        BDTNAS1
025500000000     C                   EndIf
025600000000     C*
025700000000     C                   If        G61BDTIMPE    <> *blanks
025800000000     C                   Move      G61BDTIMPE    DTGMA
025900000000     C                   Move      GMAAnno       GMAAnno1
026000000000     C                   Move      GMAMese       GMAMese1
026100000000     C                   Move      GMAGiorno     GMAGiorno1
026200000000     C                   Move      DTGMA1        BDTIMPE
026300000000     C                   EndIf
026400140220R146 C*
026500140220 "   C                   If        G61BDTINP     <> *blanks
026600140220 "   C                   Move      G61BDTINP     DTGMA
026700140220 "   C                   Move      GMAAnno       GMAAnno1
026800140220 "   C                   Move      GMAMese       GMAMese1
026900140220 "   C                   Move      GMAGiorno     GMAGiorno1
027000140220 "   C                   Move      DTGMA1        BDTINP
027100140220 "   C                   EndIf
027200140220 "   C*
027300140220 "   C                   If        G61BDTFIP     <> *blanks
027400140220 "   C                   Move      G61BDTFIP     DTGMA
027500140220 "   C                   Move      GMAAnno       GMAAnno1
027600140220 "   C                   Move      GMAMese       GMAMese1
027700140220 "   C                   Move      GMAGiorno     GMAGiorno1
027800140220 "   C                   Move      DTGMA1        BDTFIP
027900140220R146 C                   EndIf
028000140220     C*
028100000000     C   39              WRITE     SG61T1
028200000000     C   39              Setoff                                       39
028300000000     C                   WRITE     SG61TRB
028400000000     C*
028500000000     C                   ENDSR
028600000000     C************************************************************
028700000000     C* Tipo QUA FA
028800000000     C************************************************************
028900000000     C     ElaRecFA      BEGSR
029000000000     C*
029100000000     c                   Clear                   SG61DRFA
029200000000     C                   Setoff                                       77
029300000000     C*
029400000000     C                   MoveL     ETEREC        DATICAS
029500000000     C                   MoveA     DATIETW2      SKDATI
029600000000     C*
029700000000     C     1             Do        75            K                 5 0
029800000000     C*-
029900000000     C                   MoveL     SKDATI(K)     CampoWrk
030000000000     C  N77              Move      ContaRigo     SVContarigo
030100000000     C  N77              Seton                                        77
030200000000     C*
030300000000     C                   If        Quadro        = *blanks      or
030400000000     C                             ContaRigo    <> SVContarigo
030500000000     C*
030600000000     C   39              WRITE     SG61T1
030700000000     C   39              WRITE     SG61TRFA
030800000000     C   39              Setoff                                       39
030900000000     C                   WRITE     SG61DRFA
031000000000     c                   Clear                   SG61DRFA
031100000000     C                   If        Quadro        <>*blanks
031200000000     C                   Move      ContaRigo     SVContarigo
031300000000     C                   ElSe
031400000000     C                   Leave
031500000000     C                   EndIf
031600000000     C                   EndIf
031700000000     C*-
031800000000     C                   Select
031900000000     C*-
032000000000     C                   When      ContaColo     = 1
032100000000     C                   Eval      P610PIVA    = Campo_Valore
032200000000     C                   When      ContaColo     = 2
032300000000     C                   Eval      P610CFI     = Campo_Valore
032400000000R135 C                   When      ContaColo     = 3
032500000000R135 C                   Move      Campo_Valore  P610DOCR
032600000000R135 C****               When      ContaColo     = 3
032700000000R135 C                   When      ContaColo     = 4
032800000000     C                   Move      Campo_Valore  P610NOpeA
032900000000R135 C****               When      ContaColo     = 4
033000000000R135 C                   When      ContaColo     = 5
033100000000     C                   Move      Campo_Valore  P610NOpeP
033200000000R135 C****               When      ContaColo     = 5
033300000000R135 C****               Move      Campo_Valore  P610Leas
033400000000R135 C                   When      ContaColo     = 6
033500000000R135 C                   MoveL     Campo_Valore  P610Leas
033600000000R135 C****               When      ContaColo     = 6
033700000000R135 C                   When      ContaColo     = 7
033800000000     C                   Move      Campo_Valore  P610Impon
033900000000R135 C****               When      ContaColo     = 7
034000000000R135 C                   When      ContaColo     = 8
034100000000     C                   Move      Campo_Valore  P610Impost
034200000000R135 C****               When      ContaColo     = 8
034300000000R135 C                   When      ContaColo     = 9
034400000000     C                   Move      Campo_Valore  P610TMargi
034500000000R135 C****               When      ContaColo     = 9
034600000000R135 C                   When      ContaColo     = 10
034700000000     C                   Move      Campo_Valore  P610IMPNDE
034800000000R135 C****               When      ContaColo     = 10
034900000000R135 C                   When      ContaColo     = 11
035000000000     C                   Move      Campo_Valore  P610IVANDE
035100000000R135 C****               When      ContaColo     = 11
035200000000R135 C                   When      ContaColo     = 12
035300000000     C                   Move      Campo_Valore  P610IMPPAS
035400000000R135 C****               When      ContaColo     = 12
035500000000R135 C                   When      ContaColo     = 13
035600000000     C                   Move      Campo_Valore  P610IVAPAS
035700000000R135 C****               When      ContaColo     = 13
035800000000R135 C                   When      ContaColo     = 14
035900000000     C                   Move      Campo_Valore  P610MARGIP
036000000000R135 C****               When      ContaColo     = 14
036100000000R135 C                   When      ContaColo     = 15
036200000000     C                   Move      Campo_Valore  P610IMPNCR
036300000000R135 C****               When      ContaColo     = 15
036400000000R135 C                   When      ContaColo     = 16
036500000000     C                   Move      Campo_Valore  P610IVANCR
036600000000     C*
036700000000     C                   EndSl
036800000000     C*
036900000000     C                   EndDo
037000000000     C*
037100000000     C                   EndSr
037200000000     C************************************************************
037300000000     C* Tipo QUA SA
037400000000     C************************************************************
037500000000     C     ElaRecSA      BEGSR
037600000000     C*
037700000000     C                   Clear                   SG61DRSA
037800000000     C                   Setoff                                       77
037900000000     C*
038000000000     C                   MoveL     ETEREC        DATICAS
038100000000     C                   MoveA     DATIETW2      SKDATI
038200000000     C*
038300000000     C     1             Do        75            K                 5 0
038400000000     C*
038500000000     C                   MoveL     SKDATI(K)     CampoWrk
038600000000     C  N77              Move      ContaRigo     SVContarigo
038700000000     C  N77              Seton                                        77
038800000000     C*
038900000000     C                   If        Quadro        = *blanks      or
039000000000     C                             ContaRigo    <> SVContarigo
039100000000     C   39              WRITE     SG61T1
039200000000     C   39              WRITE     SG61TRSA
039300000000     C   39              Setoff                                       39
039400000000     C                   WRITE     SG61DRSA
039500000000     C                   Clear                   SG61DRSA
039600000000     C                   If        Quadro        <>*blanks
039700000000     C                   Move      ContaRigo     SVContarigo
039800000000     C                   ElSe
039900000000     C                   Leave
040000000000     C                   EndIf
040100000000     C                   EndIf
040200000000     C*-
040300000000     C                   Select
040400000000     C                   When      ContaColo     = 1
040500000000     C                   Eval      P610CFI     = Campo_Valore
040600000000     C                   When      ContaColo     = 2
040700000000     C                   Move      Campo_Valore  P610NROPE
040800000000     C                   When      ContaColo     = 3
040900000000     C                   Move      Campo_Valore  P610IMPORT
041000000000     C                   When      ContaColo     = 4
041100000000R135 C****               Move      Campo_Valore  P610Leas
041200000000R135 C                   MoveL     Campo_Valore  P610Leas
041300000000     C*
041400000000     C                   EndSl
041500000000     C*
041600000000     C                   EndDo
041700000000     C*
041800000000     C                   EndSr
041900000000     C************************************************************
042000000000     C* Tipo QUA BL
042100000000     C************************************************************
042200000000     C     ElaRecBL      BEGSR
042300000000     C*
042400000000     C                   Clear                   SG61DRBL
042500000000     C*
042600000000     C                   MoveL     ETEREC        DATICAS
042700000000     C                   MoveA     DATIETW2      SKDATI
042800000000R141 C                   Move      DATIPMOD      P610PMOD
042900000000     C*
043000000000     C     1             Do        75            K                 5 0
043100000000     C*-
043200000000     C                   MoveL     SKDATI(K)     CampoWrk
043300000000     C*
043400000000     C                   If        Quadro        = *blanks
043500000000     C                   Leave
043600000000     C                   EndIf
043700000000     C*
043800000000     C                   Select
043900000000     C*-
044000000000     C                   When      ContaRigo     = 1    and
044100000000     C                             ContaColo     = 1
044200000000     C****               Eval      P610COGRAG  = Campo_Valore
044300000000     C                   CAT       Campo_Valore:0P610COGRAG
044400000000     C                   When      ContaRigo     = 1    and
044500000000     C                             ContaColo     = 2
044600000000     C****               Eval      P610NOMEG   = Campo_Valore
044700000000     C                   CAT       Campo_Valore:0P610NOMEG
044800000000     C                   When      ContaRigo     = 1    and
044900000000     C                             ContaColo     = 3
045000000000     C                   Move      Campo_Valore  DTGMA
045100000000     C                   Move      GMAAnno       GMAAnno1
045200000000     C                   Move      GMAMese       GMAMese1
045300000000     C                   Move      GMAGiorno     GMAGiorno1
045400000000     C                   Move      DTGMA1        P610DTNASC
045500000000     C                   When      ContaRigo     = 1    and
045600000000     C                             ContaColo     = 4
045700000000     C****               MoveL     Campo_Valore  P610LOCNAS
045800000000     C                   CAT       Campo_Valore:0P610LOCNAS
045900000000     C                   When      ContaRigo     = 1    and
046000000000     C                             ContaColo     = 5
046100000000     C                   MoveL     Campo_Valore  P610PRVNAS
046200000000     C                   When      ContaRigo     = 1    and
046300000000     C                             ContaColo     = 6
046400000000     C                   Move      Campo_Valore  P610UIC1
046500000000     C                   When      ContaRigo     = 1    and
046600000000     C                             ContaColo     = 7
046700000000     C****               MoveL     Campo_Valore  P610RAGSOC
046800000000     C                   CAT       Campo_Valore:0P610RAGSOC
046900000000     C                   When      ContaRigo     = 1    and
047000000000     C                             ContaColo     = 8
047100000000     C****               MoveL     Campo_Valore  P610CITTAE
047200000000     C                   CAT       Campo_Valore:0P610CITTAE
047300000000     C                   When      ContaRigo     = 1    and
047400000000     C                             ContaColo     = 9
047500000000     C                   Move      Campo_Valore  P610UIC2
047600000000     C                   When      ContaRigo     = 1    and
047700000000     C                             ContaColo     = 10
047800000000     C****               MoveL     Campo_Valore  P610INDIRE
047900000000     C                   CAT       Campo_Valore:0P610INDIRE
048000000000     C                   When      ContaRigo     = 2    and
048100000000     C                             ContaColo     = 1
048200000000     C                   MoveL     Campo_Valore  P610CIVA
048300000000     C                   When      ContaRigo     = 2    and
048400000000     C                             ContaColo     = 2
048500000000     C                   Move      Campo_Valore  P610OPEBL
048600000000     C                   When      ContaRigo     = 2    and
048700000000     C                             ContaColo     = 3
048800000000     C                   Move      Campo_Valore  P610OPENRE
048900000000     C                   When      ContaRigo     = 2    and
049000000000     C                             ContaColo     = 4
049100000000     C                   Move      Campo_Valore  P610ACQSRV
049200000000R132 C* BL003
049300000000     C                   When      ContaRigo     = 3    and
049400000000     C                             ContaColo     = 1
049500000000     C                   Move      Campo_Valore  P610IMPATT
049600000000     C                   When      ContaRigo     = 3    and
049700000000     C                             ContaColo     = 2
049800000000     C                   Move      Campo_Valore  P610IVAATT
049900000000R132 C* BL004
050000000000 "   C                   When      ContaRigo     = 4    and
050100000000 "   C                             ContaColo     = 1
050200000000 "   C                   Move      Campo_Valore  P610IMPANB
050300000000 "   C                   When      ContaRigo     = 4    and
050400000000 "   C                             ContaColo     = 2
050500000000R132 C                   Move      Campo_Valore  P610IMPANS
050600000000R132 C* BL005
050700000000 "   C                   When      ContaRigo     = 5    and
050800000000 "   C                             ContaColo     = 1
050900000000 "   C                   Move      Campo_Valore  P610IMPNAT
051000000000 "   C                   When      ContaRigo     = 5    and
051100000000 "   C                             ContaColo     = 2
051200000000R132 C                   Move      Campo_Valore  P610IVANAT
051300000000R132 C* BL006
051400000000     C                   When      ContaRigo     = 6    and
051500000000     C                             ContaColo     = 1
051600000000     C                   Move      Campo_Valore  P610IMPPAS
051700000000     C                   When      ContaRigo     = 6    and
051800000000     C                             ContaColo     = 2
051900000000     C                   Move      Campo_Valore  P610IVAPAS
052000000000R132 C* BL007
052100000000 "   C                   When      ContaRigo     = 7    and
052200000000 "   C                             ContaColo     = 1
052300000000R132 C                   Move      Campo_Valore  P610IMPPNB
052400000000R132 C* BL008
052500000000 "   C                   When      ContaRigo     = 8    and
052600000000 "   C                             ContaColo     = 1
052700000000 "   C                   Move      Campo_Valore  P610IMPNPA
052800000000 "   C                   When      ContaRigo     = 8    and
052900000000 "   C                             ContaColo     = 2
053000000000R132 C                   Move      Campo_Valore  P610IVANPA
053100000000     C*
053200000000     C                   EndSl
053300000000     C*
053400000000     C                   EndDo
053500000000     C*
053600000000     C   39              WRITE     SG61T1
053700000000R141 C***39              WRITE     SG61TRBL
053800000000R141 C                   WRITE     SG61TRBL
053900000000     C   39              Setoff                                       39
054000000000     C                   WRITE     SG61DRBL
054100000000     C*
054200000000     C                   Clear                   SG61DRBL
054300000000     C*
054400000000     C                   EndSr
054500000000     C************************************************************
054600000000     C* Tipo QUA DF
054700000000     C************************************************************
054800000000     C     ElaRecDF      BegSr
054900000000     C*
055000000000     C                   Clear                   SG61DRDF
055100000000     C                   Setoff                                       77
055200000000     C*
055300000000     C                   MoveL     ETEREC        DATICAS
055400000000     C                   MoveA     DATIETW2      SKDATI
055500000000     C*
055600000000     C     1             Do        75            K                 5 0
055700000000     C*
055800000000     C                   MoveL     SKDATI(K)     CampoWrk
055900000000     C  N77              Move      ContaRigo     SVContarigo
056000000000     C  N77              Seton                                        77
056100000000     C*
056200000000     C                   If        Quadro        = *blanks      or
056300000000     C                             ContaRigo    <> SVContarigo
056400000000     C   39              WRITE     SG61T1
056500000000     C   39              WRITE     SG61TRDF
056600000000     C   39              Setoff                                       39
056700000000     C                   WRITE     SG61DRDF
056800000000     C                   Clear                   SG61DRDF
056900000000     C                   If        Quadro        <>*blanks
057000000000     C                   Move      ContaRigo     SVContarigo
057100000000     C                   ElSe
057200000000     C                   Leave
057300000000     C                   EndIf
057400000000     C                   EndIf
057500000000     C*-
057600000000     C                   Select
057700000000     C                   When      ContaColo     = 1
057800000000     C                   Eval      P610CFI     = Campo_Valore
057900000000     C*-
058000000000     C                   When      ContaColo     = 2
058100000000     C                   Move      Campo_Valore  DTGMA
058200000000     C                   Move      GMAAnno       GMAAnno1
058300000000     C                   Move      GMAMese       GMAMese1
058400000000     C                   Move      GMAGiorno     GMAGiorno1
058500000000     C                   Move      DTGMA1        P610DTREG
058600000000     C*-
058700000000     C                   When      ContaColo     = 3
058800000000     C                   Move      Campo_Valore  P610IMPORT
058900000000     C                   When      ContaColo     = 4
059000000000R135 C****               Move      Campo_Valore  P610Leas
059100000000R135 C                   MoveL     Campo_Valore  P610Leas
059200000000     C                   When      ContaColo     = 5
059300000000     C                   Move      Campo_Valore  P610CONFIM
059400000000     C*
059500000000     C                   EndSl
059600000000     C*
059700000000     C                   EndDo
059800000000     C*
059900000000     C                   EndSr
060000000000     C************************************************************
060100000000     C* Tipo QUA FE
060200000000     C************************************************************
060300000000     C     ElaRecFE      BegSr
060400000000     C*
060500000000     C                   Clear                   SG61DRFE
060600000000     C                   Setoff                                       77
060700000000     C*
060800000000     C                   MoveL     ETEREC        DATICAS
060900000000     C                   MoveA     DATIETW2      SKDATI
061000000000     C*
061100000000     C     1             Do        75            K                 5 0
061200000000     C*
061300000000     C                   MoveL     SKDATI(K)     CampoWrk
061400000000     C  N77              Move      ContaRigo     SVContarigo
061500000000     C  N77              Seton                                        77
061600000000     C*
061700000000     C                   If        Quadro        = *blanks      or
061800000000     C                             ContaRigo    <> SVContarigo
061900000000     C   39              WRITE     SG61T1
062000000000     C   39              WRITE     SG61TRFE
062100000000     C   39              Setoff                                       39
062200000000     C                   WRITE     SG61DRFE
062300000000     C                   Clear                   SG61DRFE
062400000000     C                   If        Quadro        <>*blanks
062500000000     C                   Move      ContaRigo     SVContarigo
062600000000     C                   ElSe
062700000000     C                   Leave
062800000000     C                   EndIf
062900000000     C                   EndIf
063000000000     C*-
063100000000     C                   Select
063200000000     C                   When      ContaColo     = 1
063300000000     C                   Eval      P610PIVA    = Campo_Valore
063400000000     C                   When      ContaColo     = 2
063500000000     C                   Eval      P610CFI     = Campo_Valore
063600000000     C                   When      ContaColo     = 3
063700000000     C                   Move      Campo_Valore  P610DOCR
063800000000     C                   When      ContaColo     = 4
063900000000     C                   Move      Campo_Valore  P610MARGIN
064000000000     C                   When      ContaColo     = 5
064100000000     C                   Move      Campo_Valore  P610Leas
064200000000     C                   When      ContaColo     = 6
064300000000     C                   Move      Campo_Valore  P610AUTOFA
064400000000     C*
064500000000     C                   When      ContaColo     = 7
064600000000     C                   Move      Campo_Valore  DTGMA
064700000000     C                   Move      GMAAnno       GMAAnno1
064800000000     C                   Move      GMAMese       GMAMese1
064900000000     C                   Move      GMAGiorno     GMAGiorno1
065000000000     C                   Move      DTGMA1        P610DTDOC
065100000000     C*
065200000000     C                   When      ContaColo     = 8
065300000000     C                   Move      Campo_Valore  DTGMA
065400000000     C                   Move      GMAAnno       GMAAnno1
065500000000     C                   Move      GMAMese       GMAMese1
065600000000     C                   Move      GMAGiorno     GMAGiorno1
065700000000     C                   Move      DTGMA1        P610DTREG
065800000000     C*
065900000000     C                   When      ContaColo     = 9
066000000000     C                   MoveL     Campo_Valore  P610NRDOC
066100000000     C*
066200000000     C                   When      ContaColo     = 10
066300000000     C                   Move      Campo_Valore  P610IMPORT
066400000000     C                   When      ContaColo     = 11
066500000000     C                   Move      Campo_Valore  P610IVA
066600000000     C                   When      ContaColo     = 12
066700000000     C                   Move      Campo_Valore  P610CONFIM
066800000000     C*
066900000000     C                   EndSl
067000000000     C*
067100000000     C                   EndDo
067200000000     C*
067300000000     C                   EndSr
067400000000     C************************************************************
067500000000     C* Tipo QUA FN
067600000000     C************************************************************
067700000000     C     ElaRecFN      BegSr
067800000000     C*
067900000000     C                   Clear                   SG61DRFN
068000000000     C                   MoveL     ETEREC        DATICAS
068100000000     C                   MoveA     DATIETW2      SKDATI
068200000000     C*
068300000000     C     1             Do        75            K                 5 0
068400000000     C*-
068500000000     C                   MoveL     SKDATI(K)     CampoWrk
068600000000     C  N77              Move      ContaRigo     SVContarigo
068700000000     C  N77              Seton                                        77
068800000000     C*
068900000000     C                   If        Quadro        = *blanks      or
069000000000     C                             ContaRigo    <> SVContarigo
069100000000     C   39              WRITE     SG61T1
069200000000     C   39              WRITE     SG61TRFN
069300000000     C   39              Setoff                                       39
069400000000     C                   WRITE     SG61DRFN
069500000000     C                   Clear                   SG61DRFN
069600000000     C                   If        Quadro        <>*blanks
069700000000     C                   Move      ContaRigo     SVContarigo
069800000000     C                   ElSe
069900000000     C                   Leave
070000000000     C                   EndIf
070100000000     C                   EndIf
070200000000     C*
070300000000     C                   Select
070400000000     C*-
070500000000     C                   When      ContaColo     = 1
070600000000     C                   CAT       Campo_Valore:0P610COGRAG
070700000000     C                   When      ContaColo     = 2
070800000000     C                   CAT       Campo_Valore:0P610NOMEG
070900000000     C*
071000000000     C                   When      ContaColo     = 3
071100000000     C                   Move      Campo_Valore  DTGMA
071200000000     C                   Move      GMAAnno       GMAAnno1
071300000000     C                   Move      GMAMese       GMAMese1
071400000000     C                   Move      GMAGiorno     GMAGiorno1
071500000000     C                   Move      DTGMA1        P610DTNASC
071600000000     C*
071700000000     C                   When      ContaColo     = 4
071800000000     C                   CAT       Campo_Valore:0P610LOCNAS
071900000000     C                   When      ContaColo     = 5
072000000000     C                   MoveL     Campo_Valore  P610PRVNAS
072100000000     C                   When      ContaColo     = 6
072200000000     C                   Move      Campo_Valore  P610UIC1
072300000000     C                   When      ContaColo     = 7
072400000000     C                   CAT       Campo_Valore:0P610RAGSOC
072500000000     C***                Eval      P610RAGSOC  = P610RAGSOC +
072600000000     C***                          Campo_Valore
072700000000     C                   When      ContaColo     = 8
072800000000     C                   CAT       Campo_Valore:0P610CITTAE
072900000000     C                   When      ContaColo     = 9
073000000000     C                   Move      Campo_Valore  P610UIC2
073100000000     C                   When      ContaColo     = 10
073200000000     C                   CAT       Campo_Valore:0P610INDIRE
073300000000     C*
073400000000     C                   When      ContaColo     = 11
073500000000     C                   Move      Campo_Valore  DTGMA
073600000000     C                   Move      GMAAnno       GMAAnno1
073700000000     C                   Move      GMAMese       GMAMese1
073800000000     C                   Move      GMAGiorno     GMAGiorno1
073900000000     C                   Move      DTGMA1        P610DTDOC
074000000000     C*
074100000000     C                   When      ContaColo     = 12
074200000000     C                   Move      Campo_Valore  DTGMA
074300000000     C                   Move      GMAAnno       GMAAnno1
074400000000     C                   Move      GMAMese       GMAMese1
074500000000     C                   Move      GMAGiorno     GMAGiorno1
074600000000     C                   Move      DTGMA1        P610DTREG
074700000000     C*
074800000000     C                   When      ContaColo     = 13
074900000000     C                   MoveL     Campo_Valore  P610NRDOC
075000000000     C                   When      ContaColo     = 14
075100000000     C                   MoveL     Campo_Valore  P610LEASIN
075200000000     C                   When      ContaColo     = 15
075300000000     C                   Move      Campo_Valore  P610IMPATT
075400000000     C                   When      ContaColo     = 16
075500000000     C                   Move      Campo_Valore  P610IVAATT
075600000000     C                   When      ContaColo     = 17
075700000000     C                   Move      Campo_Valore  P610CONFIM
075800000000     C*
075900000000     C                   EndSl
076000000000     C*
076100000000     C                   EndDo
076200000000     C*
076300000000     C                   EndSr
076400000000     C************************************************************
076500000000     C* Tipo QUA FR
076600000000     C************************************************************
076700000000     C     ElaRecFR      BegSr
076800000000     C*
076900000000     C                   Clear                   SG61DRFR
077000000000     C                   Setoff                                       77
077100000000     C*
077200000000     C                   MoveL     ETEREC        DATICAS
077300000000     C                   MoveA     DATIETW2      SKDATI
077400000000     C*
077500000000     C     1             Do        75            K                 5 0
077600000000     C*
077700000000     C                   MoveL     SKDATI(K)     CampoWrk
077800000000     C  N77              Move      ContaRigo     SVContarigo
077900000000     C  N77              Seton                                        77
078000000000     C*
078100000000     C                   If        Quadro        = *blanks      or
078200000000     C                             ContaRigo    <> SVContarigo
078300000000     C   39              WRITE     SG61T1
078400000000     C   39              WRITE     SG61TRFR
078500000000     C   39              Setoff                                       39
078600000000     C                   WRITE     SG61DRFR
078700000000     C                   Clear                   SG61DRFR
078800000000     C                   If        Quadro        <>*blanks
078900000000     C                   Move      ContaRigo     SVContarigo
079000000000     C                   ElSe
079100000000     C                   Leave
079200000000     C                   EndIf
079300000000     C                   EndIf
079400000000     C*-
079500000000     C                   Select
079600000000     C*-
079700000000     C                   When      ContaColo     = 1
079800000000     C                   Eval      P610PIVA    = Campo_Valore
079900000000     C                   When      ContaColo     = 2
080000000000     C                   Move      Campo_Valore  P610DOCR
080100000000     C*
080200000000     C                   When      ContaColo     = 3
080300000000     C                   Move      Campo_Valore  DTGMA
080400000000     C                   Move      GMAAnno       GMAAnno1
080500000000     C                   Move      GMAMese       GMAMese1
080600000000     C                   Move      GMAGiorno     GMAGiorno1
080700000000     C                   Move      DTGMA1        P610DTDOC
080800000000     C*
080900000000     C                   When      ContaColo     = 4
081000000000     C                   Move      Campo_Valore  DTGMA
081100000000     C                   Move      GMAAnno       GMAAnno1
081200000000     C                   Move      GMAMese       GMAMese1
081300000000     C                   Move      GMAGiorno     GMAGiorno1
081400000000     C                   Move      DTGMA1        P610DTREG
081500000000     C*
081600000000     C                   When      ContaColo     = 5
081700000000     C                   Move      Campo_Valore  P610MARGIN
081800000000     C                   When      ContaColo     = 6
081900000000     C                   Move      Campo_Valore  P610REV
082000000000     C                   When      ContaColo     = 7
082100000000     C                   Move      Campo_Valore  P610AUTOFA
082200000000     C*
082300000000     C                   When      ContaColo     = 8
082400000000     C                   Move      Campo_Valore  P610IMPORT
082500000000     C                   When      ContaColo     = 9
082600000000     C                   Move      Campo_Valore  P610IVA
082700000000     C                   When      ContaColo     = 10
082800000000     C                   Move      Campo_Valore  P610CONFIM
082900000000     C*
083000000000     C                   EndSl
083100000000     C*
083200000000     C                   EndDo
083300000000     C*
083400000000     C                   EndSr
083500000000     C************************************************************
083600000000     C* Tipo QUA NE
083700000000     C************************************************************
083800000000     C     ElaRecNE      BegSr
083900000000     C*
084000000000     C                   Setoff                                       77
084100000000     C*
084200000000     C                   Clear                   SG61DRNE
084300000000     C                   MoveL     ETEREC        DATICAS
084400000000     C                   MoveA     DATIETW2      SKDATI
084500000000     C*
084600000000     C     1             Do        75            K                 5 0
084700000000     C*
084800000000     C                   MoveL     SKDATI(K)     CampoWrk
084900000000     C  N77              Move      ContaRigo     SVContarigo
085000000000     C  N77              Seton                                        77
085100000000     C*
085200000000     C                   If        Quadro        = *blanks      or
085300000000     C                             ContaRigo    <> SVContarigo
085400000000     C   39              WRITE     SG61T1
085500000000     C   39              WRITE     SG61TRNE
085600000000     C   39              Setoff                                       39
085700000000     C                   WRITE     SG61DRNE
085800000000     C                   Clear                   SG61DRNE
085900000000     C                   If        Quadro        <>*blanks
086000000000     C                   Move      ContaRigo     SVContarigo
086100000000     C                   ElSe
086200000000     C                   Leave
086300000000     C                   EndIf
086400000000     C                   EndIf
086500000000     C*-
086600000000     C                   Select
086700000000     C                   When      ContaColo     = 1
086800000000     C                   Eval      P610PIVA    = Campo_Valore
086900000000     C                   When      ContaColo     = 2
087000000000     C                   Eval      P610CFI     = Campo_Valore
087100000000     C*
087200000000     C                   When      ContaColo     = 3
087300000000     C                   Move      Campo_Valore  DTGMA
087400000000     C                   Move      GMAAnno       GMAAnno1
087500000000     C                   Move      GMAMese       GMAMese1
087600000000     C                   Move      GMAGiorno     GMAGiorno1
087700000000     C                   Move      DTGMA1        P610DTDOC
087800000000     C*
087900000000     C                   When      ContaColo     = 4
088000000000     C                   Move      Campo_Valore  DTGMA
088100000000     C                   Move      GMAAnno       GMAAnno1
088200000000     C                   Move      GMAMese       GMAMese1
088300000000     C                   Move      GMAGiorno     GMAGiorno1
088400000000     C                   Move      DTGMA1        P610DTREG
088500000000     C*
088600000000     C                   When      ContaColo     = 5
088700000000     C                   MoveL     Campo_Valore  P610NRDOC
088800000000     C*
088900000000     C                   When      ContaColo     = 6
089000000000     C                   Move      Campo_Valore  P610IMPORT
089100000000     C                   When      ContaColo     = 7
089200000000     C                   Move      Campo_Valore  P610IVA
089300000000     C                   When      ContaColo     = 8
089400000000     C                   Move      Campo_Valore  P610CONFIM
089500000000     C*
089600000000     C                   EndSl
089700000000     C*
089800000000     C                   EndDo
089900000000     C*
090000000000     C                   EndSr
090100000000     C************************************************************
090200000000     C* Tipo QUA NR
090300000000     C************************************************************
090400000000     C     ElaRecNR      BegSr
090500000000     C*
090600000000     C                   Clear                   SG61DRNR
090700000000     C                   Setoff                                       77
090800000000     C*
090900000000     C                   MoveL     ETEREC        DATICAS
091000000000     C                   MoveA     DATIETW2      SKDATI
091100000000     C*
091200000000     C     1             Do        75            K                 5 0
091300000000     C*
091400000000     C                   MoveL     SKDATI(K)     CampoWrk
091500000000     C  N77              Move      ContaRigo     SVContarigo
091600000000     C  N77              Seton                                        77
091700000000     C*
091800000000     C                   If        Quadro        = *blanks      or
091900000000     C                             ContaRigo    <> SVContarigo
092000000000     C   39              WRITE     SG61T1
092100000000     C   39              WRITE     SG61TRNR
092200000000     C   39              Setoff                                       39
092300000000     C                   WRITE     SG61DRNR
092400000000     C                   Clear                   SG61DRNR
092500000000     C                   If        Quadro        <>*blanks
092600000000     C                   Move      ContaRigo     SVContarigo
092700000000     C                   ElSe
092800000000     C                   Leave
092900000000     C                   EndIf
093000000000     C                   EndIf
093100000000     C*-
093200000000     C                   Select
093300000000     C*-
093400000000     C                   When      ContaColo     = 1
093500000000     C                   Eval      P610PIVA    = Campo_Valore
093600000000     C*
093700000000     C                   When      ContaColo     = 2
093800000000     C                   Move      Campo_Valore  DTGMA
093900000000     C                   Move      GMAAnno       GMAAnno1
094000000000     C                   Move      GMAMese       GMAMese1
094100000000     C                   Move      GMAGiorno     GMAGiorno1
094200000000     C                   Move      DTGMA1        P610DTDOC
094300000000     C*
094400000000     C                   When      ContaColo     = 3
094500000000     C                   Move      Campo_Valore  DTGMA
094600000000     C                   Move      GMAAnno       GMAAnno1
094700000000     C                   Move      GMAMese       GMAMese1
094800000000     C                   Move      GMAGiorno     GMAGiorno1
094900000000     C                   Move      DTGMA1        P610DTREG
095000000000     C*
095100000000     C                   When      ContaColo     = 4
095200000000     C                   Move      Campo_Valore  P610IMPORT
095300000000     C                   When      ContaColo     = 5
095400000000     C                   Move      Campo_Valore  P610IVA
095500000000     C                   When      ContaColo     = 6
095600000000     C                   Move      Campo_Valore  P610CONFIM
095700000000     C*
095800000000     C                   EndSl
095900000000     C*
096000000000     C                   EndDo
096100000000     C*
096200000000     C                   EndSr
096300000000     C************************************************************
096400000000     C* Tipo QUA SE
096500000000     C************************************************************
096600000000     C     ElaRecSE      BegSr
096700000000     C*
096800000000     C                   Clear                   SG61DRSE
096900000000     C*
097000000000     C                   Setoff                                       77
097100000000     C                   MoveL     ETEREC        DATICAS
097200000000     C                   MoveA     DATIETW2      SKDATI
097300000000     C*
097400000000     C     1             Do        75            K                 5 0
097500000000     C*-
097600000000     C                   MoveL     SKDATI(K)     CampoWrk
097700000000     C  N77              Move      ContaRigo     SVContarigo
097800000000     C  N77              Seton                                        77
097900000000     C*
098000000000     C                   If        Quadro        = *blanks      or
098100000000     C                             ContaRigo    <> SVContarigo
098200000000     C   39              WRITE     SG61T1
098300000000     C   39              WRITE     SG61TRSE
098400000000     C   39              Setoff                                       39
098500000000     C                   WRITE     SG61DRSE
098600000000     C                   Clear                   SG61DRSE
098700000000     C                   If        Quadro        <>*blanks
098800000000     C                   Move      ContaRigo     SVContarigo
098900000000     C                   ElSe
099000000000     C                   Leave
099100000000     C                   EndIf
099200000000     C                   EndIf
099300000000     C*
099400000000     C                   Select
099500000000     C*-
099600000000     C                   When      ContaColo     = 1
099700000000     C                   CAT       Campo_Valore:0P610COGRAG
099800000000     C                   When      ContaColo     = 2
099900000000     C                   CAT       Campo_Valore:0P610NOMEG
100000000000     C*
100100000000     C                   When      ContaColo     = 3
100200000000     C                   Move      Campo_Valore  DTGMA
100300000000     C                   Move      GMAAnno       GMAAnno1
100400000000     C                   Move      GMAMese       GMAMese1
100500000000     C                   Move      GMAGiorno     GMAGiorno1
100600000000     C                   Move      DTGMA1        P610DTNASC
100700000000     C*
100800000000     C                   When      ContaColo     = 4
100900000000     C                   CAT       Campo_Valore:0P610LOCNAS
101000000000     C                   When      ContaColo     = 5
101100000000     C                   MoveL     Campo_Valore  P610PRVNAS
101200000000     C                   When      ContaColo     = 6
101300000000     C                   Move      Campo_Valore  P610UIC1
101400000000     C                   When      ContaColo     = 7
101500000000     C                   CAT       Campo_Valore:0P610RAGSOC
101600000000     C                   When      ContaColo     = 8
101700000000     C                   CAT       Campo_Valore:0P610CITTAE
101800000000     C                   When      ContaColo     = 9
101900000000     C                   Move      Campo_Valore  P610UIC2
102000000000     C                   When      ContaColo     = 10
102100000000     C                   CAT       Campo_Valore:0P610INDIRE
102200000000     C                   When      ContaColo     = 11
102300000000     C                   Move      Campo_Valore  P610CIVA
102400000000     C*
102500000000     C                   When      ContaColo     = 12
102600000000     C                   Move      Campo_Valore  DTGMA
102700000000     C                   Move      GMAAnno       GMAAnno1
102800000000     C                   Move      GMAMese       GMAMese1
102900000000     C                   Move      GMAGiorno     GMAGiorno1
103000000000     C                   Move      DTGMA1        P610DTDOC
103100000000     C*
103200000000     C                   When      ContaColo     = 13
103300000000     C                   Move      Campo_Valore  DTGMA
103400000000     C                   Move      GMAAnno       GMAAnno1
103500000000     C                   Move      GMAMese       GMAMese1
103600000000     C                   Move      GMAGiorno     GMAGiorno1
103700000000     C                   Move      DTGMA1        P610DTREG
103800000000     C*
103900000000     C                   When      ContaColo     = 14
104000000000     C                   MoveL     Campo_Valore  P610NRDOC
104100000000     C                   When      ContaColo     = 15
104200000000     C                   Move      Campo_Valore  P610IMPATT
104300000000     C                   When      ContaColo     = 16
104400000000     C                   Move      Campo_Valore  P610IVAATT
104500000000     C                   When      ContaColo     = 17
104600000000     C                   Move      Campo_Valore  P610CONFIM
104700000000     C*
104800000000     C                   EndSl
104900000000     C*
105000000000     C                   EndDo
105100000000     C*
105200000000     C                   EndSr
105300000000R136 C************************************************************
105400000000     C* Tipo QUA TU
105500000000R136 C************************************************************
105600000000     C     ElaRecTU      BegSr
105700000000     C*
105800000000     C                   Clear                   SG61DRTU
105900000000     C*
106000000000     C                   Setoff                                       77
106100000000     C                   MoveL     ETEREC        DATICAS
106200000000     C                   MoveA     DATIETW2      SKDATI
106300000000     C*
106400000000     C     1             Do        75            K                 5 0
106500000000     C*-
106600000000     C                   MoveL     SKDATI(K)     CampoWrk
106700000000     C  N77              Move      ContaRigo     SVContarigo
106800000000     C  N77              Seton                                        77
106900000000     C*
107000000000     C                   If        Quadro        = *blanks      or
107100000000     C                             ContaRigo    <> SVContarigo
107200000000     C   39              WRITE     SG61T1
107300000000     C   39              WRITE     SG61TRTU
107400000000     C   39              Setoff                                       39
107500000000     C                   WRITE     SG61DRTU
107600000000     C                   Clear                   SG61DRTU
107700000000     C                   If        Quadro        <>*blanks
107800000000     C                   Move      ContaRigo     SVContarigo
107900000000     C                   ElSe
108000000000     C                   Leave
108100000000     C                   EndIf
108200000000     C                   EndIf
108300000000     C*
108400000000     C                   Select
108500000000     C*-
108600000000     C                   When      ContaColo     = 1
108700000000     C                   CAT       Campo_Valore:0P610COGRAG
108800000000     C                   When      ContaColo     = 2
108900000000     C                   CAT       Campo_Valore:0P610NOMEG
109000000000     C*
109100000000     C                   When      ContaColo     = 3
109200000000     C                   Move      Campo_Valore  DTGMA
109300000000     C                   Move      GMAAnno       GMAAnno1
109400000000     C                   Move      GMAMese       GMAMese1
109500000000     C                   Move      GMAGiorno     GMAGiorno1
109600000000     C                   Move      DTGMA1        P610DTNASC
109700000000     C*
109800000000     C                   When      ContaColo     = 4
109900000000     C                   CAT       Campo_Valore:0P610LOCNAS
110000000000     C                   When      ContaColo     = 5
110100000000     C                   MoveL     Campo_Valore  P610PRVNAS
110200000000     C                   When      ContaColo     = 6
110300000000     C                   CAT       Campo_Valore:0P610CITTAE
110400000000     C                   When      ContaColo     = 7
110500000000     C                   Move      Campo_Valore  P610UIC1
110600000000     C                   When      ContaColo     = 8
110700000000     C                   CAT       Campo_Valore:0P610INDIRE
110800000000     C*
110900000000     C                   When      ContaColo     = 9
111000000000     C                   Move      Campo_Valore  DTGMA
111100000000     C                   Move      GMAAnno       GMAAnno1
111200000000     C                   Move      GMAMese       GMAMese1
111300000000     C                   Move      GMAGiorno     GMAGiorno1
111400000000     C                   Move      DTGMA1        P610DTDOC
111500000000     C*
111600000000     C                   When      ContaColo     = 10
111700000000     C                   Move      Campo_Valore  DTGMA
111800000000     C                   Move      GMAAnno       GMAAnno1
111900000000     C                   Move      GMAMese       GMAMese1
112000000000     C                   Move      GMAGiorno     GMAGiorno1
112100000000     C                   Move      DTGMA1        P610DTREG
112200000000     C*
112300000000     C                   When      ContaColo     = 11
112400000000     C                   MoveL     Campo_Valore  P610NRDOC
112500000000     C                   When      ContaColo     = 12
112600000000     C                   Move      Campo_Valore  P610IMPATT
112700000000     C                   When      ContaColo     = 13
112800000000     C                   Move      Campo_Valore  P610IVAATT
112900000000     C                   When      ContaColo     = 14
113000000000     C                   Move      Campo_Valore  P610CONFIM
113100000000     C*
113200000000     C                   EndSl
113300000000     C*
113400000000     C                   EndDo
113500000000     C*
113600000000R136 C                   EndSr
113700000000     C************************************************************
113800000000     C* Tipo QUA TA
113900000000     C************************************************************
114000000000     C     ElaRecTA      BEGSR
114100000000     C*
114200000000     C                   Clear                   SG61TRTA
114300000000     C*
114400000000     C                   MoveL     ETEREC        DATICAS
114500000000     C                   MoveL     DATIETW1      SDGSG61DSE
114600000000     C                   Eval      P610CFI     = G61ECFI
114700000000     C                   Eval      P610IPS     = G61EIPS
114800000000     C                   MoveA     DATIETW2      SKDATI
114900000000     C*
115000000000     C     1             Do        75            K                 5 0
115100000000     C*-
115200000000     C                   MoveL     SKDATI(K)     CampoWrk
115300000000     C*
115400000000     C                   If        Quadro        = *blanks
115500000000     C                   Leave
115600000000     C                   EndIf
115700000000     C*
115800000000     C                   Select
115900000000     C*-
116000000000     C                   When      ContaRigo     = 1    and
116100000000     C                             ContaColo     = 1
116200000000     C                   Move      Campo_Valore  P610CPFA
116300000000     C                   When      ContaRigo     = 2    and
116400000000     C                             ContaColo     = 1
116500000000     C                   Move      Campo_Valore  P610CLSA
116600000000     C                   When      ContaRigo     = 3    and
116700000000     C                             ContaColo     = 1
116800000000     C                   Move      Campo_Valore  P610BLBL
116900000000     C                   When      ContaRigo     = 3    and
117000000000     C                             ContaColo     = 2
117100000000     C                   Move      Campo_Valore  P610NRBL
117200000000     C                   When      ContaRigo     = 3    and
117300000000     C                             ContaColo     = 3
117400000000     C                   Move      Campo_Valore  P610SRBL
117500000000     C                   When      ContaRigo     = 4    and
117600000000     C                             ContaColo     = 1
117700000000     C                   Move      Campo_Valore  P610FEFE
117800000000     C                   When      ContaRigo     = 4    and
117900000000     C                             ContaColo     = 2
118000000000     C                   Move      Campo_Valore  P610RIFE
118100000000     C                   When      ContaRigo     = 5    and
118200000000     C                             ContaColo     = 1
118300000000     C                   Move      Campo_Valore  P610FRFR
118400000000     C                   When      ContaRigo     = 5    and
118500000000     C                             ContaColo     = 2
118600000000     C                   Move      Campo_Valore  P610RIFR
118700000000     C                   When      ContaRigo     = 6    and
118800000000     C                             ContaColo     = 1
118900000000     C                   Move      Campo_Valore  P610NONE
119000000000     C                   When      ContaRigo     = 7    and
119100000000     C                             ContaColo     = 1
119200000000     C                   Move      Campo_Valore  P610NONR
119300000000     C                   When      ContaRigo     = 8    and
119400000000     C                             ContaColo     = 1
119500000000     C                   Move      Campo_Valore  P610CLDF
119600000000     C                   When      ContaRigo     = 9    and
119700000000     C                             ContaColo     = 1
119800000000     C                   Move      Campo_Valore  P610RIFN
119900000000     C                   When      ContaRigo     = 10   and
120000000000     C                             ContaColo     = 1
120100000000     C                   Move      Campo_Valore  P610RISE
120200000000     C                   When      ContaRigo     = 11   and
120300000000     C                             ContaColo     = 1
120400000000     C                   Move      Campo_Valore  P610CLTU
120500000000     C*
120600000000     C                   EndSl
120700000000     C*
120800000000     C                   EndDo
120900000000     C*
121000000000     C   39              WRITE     SG61T1
121100000000     C   39              WRITE     SG61TRTA
121200000000     C   39              Setoff                                       39
121300000000     C                   Clear                   SG61TRTA
121400000000     C*
121500000000     C                   EndSr
121600000000     C************************************************************
121700000000     C* Tipo REC Z
121800000000     C************************************************************
121900000000     C     ElaRecZ       BEGSR
122000000000     C*
122100000000     C                   MoveL     ETEREC        SDGSG61DSZ
122200000000     C*
122300000000     C   39              WRITE     SG61T1
122400000000     C   39              Setoff                                       39
122500000000     C                   WRITE     SG61TRZ
122600000000     C*
122700000000     C                   ENDSR
122800000000     C************************************************************
122900000000     C* Inizializzazioni variabili
123000000000     C************************************************************
123100000000     C     InzVar        BegSr
123200000000     C*
123300000000     C                   MoveL     KPJBU         SDGSG61DS
123400000000     C*
123500000000     C                   Eval      ERRG61      = *Off
123600000000     C*
123700000000     C                   MoveL     'SOC001'      TIPXSC
123800000000     C                   MoveL     SOCIETAG61    SOCXSC
123900000000     C*
124000000000     C                   Exsr      RepSoc
124100000000     C                   If        RTNXSC     <> '1'
124200000000     C                   MoveL     XSOCDS        SOC001
124300000000     C                   ElSe
124400000000     C                   Exsr      EndPgm
124500000000     C                   EndIf
124600000000     C*
124700000000     C                   CALL      'XNETA'                              21
124800000000     C                   PARM                    NOMSYS
124900000000     C*
125000000000     C                   MoveL     KNSIF         NOMSIF
125100000000     C                   MoveL     XSCDSI        NOMDIT
125200000000     C*
125300000000     C                   MoveL     DSPGM         NOMPGM
125400000000     C*
125401160208R195 C* Reperisco il sottotitolo di testata
125402160204  "  C                   Exsr      RepSottTitt
125403160204R195 C*
125500000000     C                   SetOff                                       414243
125600000000     C                   SetOff                                       4445
125601150306R176 C*
125602150306R176 C                   Clear                   SVTQUA
125700000000     C*
125800000000     C                   EndSr                                                  *
125801160204R195 C************************************************************
125802160204 "   C* Reperimento Sottotitolo di testata
125803160204 "   C************************************************************
125804160204R195 C     RepSottTitT   BEGSR
125805160208     C*
125806160204     C                   Select
125807160208     C* Spesometro
125808160208     C                   When      TpcomG61 = 'E'
125809160204     C                   Eval      IDMSG    = SOTTITTE
125810160208     C* Acquisti da San Marino
125811160208     C                   When      TpcomG61 = 'S'
125812160204     C                   Eval      IDMSG    = SOTTITTS
125813160208     C* Black List Mensile/Trimestrale
125814160208     C                   When      TpcomG61 = 'B'
125815160208     C                             or
125816160208     C* Black List Annuale
125817160208     C                             TpcomG61 = 'L'
125818160204     C                   Eval      IDMSG    = SOTTITTB
125819160204     C                   EndSl
125820160204     C*
125821160204     C                   CALLB     'XRTVM'
125822160204     C                   PARM                    IDMSG            27
125823160204     C                   PARM                    TXTMSG          100
125824160204     C                   PARM                    ERRMSG            1
125825160208     C                   PARM      *Off          CTRMSG            1            centratura
125826160204     C                   PARM      30            LENMSG            3 0          lun output.
125827160208     C*
125828160204     C     ERRMSG        IFNE      *ON
125829160204     C                   MOVEL     TXTMSG        SOTTITT
125830160204     C                   ELSE
125831160204     C                   MOVEL     *ALL'?'       SOTTITT
125832160204     C                   ENDIF
125833160208     C*
125834160204R195 C                   ENDSR
125835160204R195 C/EJECT
125900000000     C************************************************************
126000000000     C* OPERAZIONI INIZIALI
126100000000     C************************************************************
126200000000     C     *INZSR        BegSr
126300000000     C*
126400000000     C* Reperimento parametri
126500000000     C*
126600000000     C     *ENTRY        PLIST                                                  *
126700000000     C                   PARM                    KPJBA                          *
126800000000     C*
126900000000     C                   EndSr
127000000000     C************************************************************
127100000000     C* DEFINIZIONE KLIST
127200000000     C************************************************************
127300000000     C     DefKlist      BegSr
127400000000     C*
127500000000     C* KLIST
127600000000     C*
127700000000     C                   EndSr
127800151203R195 X************************************************************
127900151203 "   X* Reperimento parametri di setup per Black list
128000151203 "   X************************************************************
128100151203 "   X**** RepParMod     BEGSR
128200151203 "   X****
128300151203 "   X****               Clear                   SDGPA08DS
128400151203 "   X****
128500151203 "   X****               CALLB     'XPAR'
128600151203 "   X****               Parm                    XScSoc
128700151203 "   X****               Parm      'SDGSPECF'    XPaPar            8
128800151203 "   X****               Parm                    XPaOut         2000
128900151203 "   X****               Parm      *OFF          XPaErr            1
129000151203 "   X****               Parm      ' '           XPaRic            1
129100151203 "   X****
129200151203 "   X****               if        XPaErr     <> *On
129300151203 "   X****               MoveL     XPaOut        SDGPA08DS
129400151203 "   X****               endif
129500151203 "   X****
129600151203R195 X****               EndSr
129700000000     C************************************************************
129800000000     C* Reperimento dati società
129900000000     C************************************************************
130000000000     C     RepSoc        BegSr
130100000000     C*
130200000000     C                   CALL      'SDGSOCDR'
130300000000     C                   PARM                    TIPXSC            6
130400000000     C                   PARM                    SOCXSC            3
130500000000     C                   PARM                    CDSXSC            9 0
130600000000     C                   PARM                    MODXSC            3
130700000000     C                   PARM      *Blanks       RTNXSC            1
130800000000     C                   PARM                    XSOCDS
130900000000     C                   PARM                    KPJBA
131000000000     C*
131100000000     C                   EndSr
131200000000     C************************************************************
131300000000     C* FINE PROGRAMMA
131400000000     C************************************************************
131500000000     C     EndPgm        BegSr
131600000000     C*
131700000000     C                   MoveL     SDGSG61DS     KPJBU
131800000000     C                   SETON                                        LR
131900000000     C*
132000000000     C                   RETURN
132100000000     C*
132200000000     C                   EndSr
132300000000     C************************************************************
