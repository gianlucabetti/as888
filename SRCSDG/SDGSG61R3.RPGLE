000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*CALLER)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*----------------------------------------------------*
000400000000     H*
000500000000     H*         ------------------------------------------------
000600000000     H*              Black List - Comunicazione Polivalente
000700000000     H*                         STAMPA SU MODULO
000800000000     H*         ------------------------------------------------
000900000000     F*----------------------------------------------------*
001000000000     F* File contenente i dati da trasmettere all'agenzia
001100000000     FSDGETE00F IF   E             DISK    UsrOpn
001200000000     F*----------------
001300000000     FSDGSG61P3 O    E             PRINTER OFLIND(*IN39)
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
004200000000     D DATICFI                 2     17
004300000000     D DATIPRGMOD             18     25
004400000000     D DATIETW2               90   1890
004500000000     D*-------------
004600000000     D SKDATI          S             24    DIM(75)
004700000000     D SVContaRigo     S              3  0
004800000000     D*-------------
004900000000     D CampoWrk        DS
005000000000     D  Quadro                 1      2
005100000000     D  ContaRigo              3      5  0
005200000000     D  ContaColo              6      8  0
005300000000     D  Campo_Valore           9     24
005400000000     D*-------------
005500000000     D DtGMA1          DS
005600000000     D  GMAGiorno1             1      2
005700000000     D  T1                     3      3
005800000000     D  GMAMese1               4      5
005900000000     D  T2                     6      6
006000000000     D  GMAAnno1               7     10
006100000000     D*-------------
006200000000     D DtGMA           DS
006300000000     D  GMAGiorno              1      2  0
006400000000     D  GMAMese                3      4  0
006500000000     D  GMAAnno                5      8  0
006600000000     D*-------------
006700000000     D SDGSG61DSA    E DS
006800000000     D*-
006900000000     D SDGSG61DSB    E DS
007000000000     D*-
007100000000     D SDGSG61DSE    E DS
007200000000     D*-
007300000000     D SDGSG61DSZ    E DS
007400000000     D*-
007500000000     C*----------------------------------------------------*
007600000000     C*                MAIN LINE PROGRAM
007700000000     C*----------------------------------------------------*
007800000000     C*
007900000000     C* inizializzazione variabili
008000000000     C                   EXSR      INZVAR
008100000000     C*
008200000000     C                   OPEN      SDGETE00F
008300000000     C*
008400000000     C                   Seton                                        39
008500000000     C*
008600000000     C* -----------------------------------------------------
008700000000     C* Prima lettura
008800000000     C                   READ      SDGETE00F                              90
008900000000     C     *IN90         DOWEQ     *OFF
009000000000     C*
009100000000     C                   Z-Add     0             K
009200000000     C*
009300000000     C                   MoveL     ETEREC        DATIREC
009400000000     C*
009500000000     C                   SELECT
009600000000     C* ---------------------------
009700000000     C* Tipo Rec.  A
009800000000     C                   WHEN      TREC    = 'A'
009900000000     C                   Exsr      ElaRecA
010000000000     C* ---------------------------
010100000000     C* Tipo Rec.  B
010200000000     C                   WHEN      TREC    = 'B'
010300000000     C                   Exsr      ElaRecB
010400000000     C* ---------------------------
010500000000     C* QUADRO    BL
010600000000     C                   WHEN      TQUA    = 'BL'
010700000000     C  N42              SetOn                                        39
010800000000     C  N42              SetOn                                        42
010900000000     C                   Exsr      ElaRecBL
011000000000     C*
011100000000     C* ---------------------------
011200000000     C*
011300000000     C                   EndSl
011400000000     C*
011500000000     C                   READ      SDGETE00F                              90
011600000000     C                   EndDo
011700000000     C*
011800000000     C                   CLOSE     SDGETE00F
011900000000     C*
012000000000     C* fine programma
012100000000     C                   Exsr      EndPgm
012200000000     C*
012300000000     C************************************************************
012400000000     C* Tipo REC A
012500000000     C************************************************************
012600000000     C     ElaRecA       BEGSR
012700000000     C*
012800000000     C                   MoveL     ETEREC        SDGSG61DSA
012900000000     C*
013000000000     C*
013100000000     C                   ENDSR
013200000000     C************************************************************
013300000000     C* Tipo REC B
013400000000     C************************************************************
013500000000     C     ElaRecB       BEGSR
013600000000     C*
013700000000     C                   MoveL     ETEREC        SDGSG61DSB
013800000000     C*
013900000000     C                   Eval      G61BCFI1    = G61BCFI
014000000000     C                   CAT       G61BLUONAS  :0BLUONAS
014100000000     C                   Eval      BPRVNAS     = G61BPRVNAS
014200000000     C                   CAT       G61BDENOMI  :0BDENOMI
014300000000     C                   CAT       G61BLUONA1  :0BLUONAS1
014400000000     C                   Eval      BPRVNAS1    = G61BPRVNA1
014500000000     C                   CAT       G61BDENOM1  :0BDENOMI1
014600000000     C*
014700000000     C                   If        G61BDTNASC    <> *blanks
014800000000     C                   Move      G61BDTNASC    DTGMA
014900000000     C                   Move      GMAAnno       BDTNAA
015000000000     C                   Move      GMAMese       BDTNMM
015100000000     C                   Move      GMAGiorno     BDTNGG
015200000000     C***                Move      DTGMA1        BDTNAS
015300000000     C                   EndIf
015400000000     C*
015500000000     C                   If        G61BDTNAS1    <> *blanks
015600000000     C                   Move      G61BDTNAS1    DTGMA
015700000000     C                   Move      GMAAnno       BDTNAA1
015800000000     C                   Move      GMAMese       BDTNMM1
015900000000     C                   Move      GMAGiorno     BDTNGG1
016000000000     C***                Move      DTGMA1        BDTNAS1
016100000000     C                   EndIf
016200000000     C*
016300140225R146 C****               If        G61BDTIMPE    <> *blanks
016400140225R146 C****               Move      G61BDTIMPE    DTGMA
016500140225R146 C                   If        G61BDTINP     <> *blanks
016600140225R146 C                   Move      G61BDTINP     DTGMA
016700000000     C                   Move      GMAAnno       BDTINPAA
016800000000     C                   Move      GMAMese       BDTINPMM
016900000000     C                   Move      GMAGiorno     BDTINPGG
017000000000     C***                Move      DTGMA1        BDTIMPE
017100000000     C                   EndIf
017200140225R146 C                   If        G61BDTFIP     <> *blanks
017300140225 "   C                   Move      G61BDTFIP     DTGMA
017400140225 "   C                   Move      GMAAnno       BDTFIPAA
017500140225 "   C                   Move      GMAMese       BDTFIPMM
017600140225 "   C                   Move      GMAGiorno     BDTFIPGG
017700140225R146 C                   EndIf
017800140225R146 C                   If        G61BDTIMPE    <> *blanks
017900140225 "   C                   Move      G61BDTIMPE    DTGMA
018000140225 "   C                   Move      GMAAnno       BDTIMPAA
018100140225 "   C                   Move      GMAMese       BDTIMPMM
018200140225 "   C                   Move      GMAGiorno     BDTIMPGG
018300140225R146 C                   EndIf
018400000000     C*
018500140225R146 C                   If        G61BSESSO  = 'M'
018600140225 "   C                   Eval      PSessoM       = 'X'
018700140225 "   C                   EndIf
018800140225 "   C                   If        G61BSESSO  = 'F'
018900140225 "   C                   Eval      PSessoF       = 'X'
019000140225 "   C                   EndIf
019100140225 "   C                   If        G61BSESSO1 = 'M'
019200140225 "   C                   Eval      PSessoM1      = 'X'
019300140225 "   C                   EndIf
019400140225 "   C                   If        G61BSESSO1 = 'F'
019500140225 "   C                   Eval      PSessoF1      = 'X'
019600140225 "   C                   EndIf
019700140225R146 C*
019800140225     C                   WRITE     SG61T1
019900000000     C*
020000000000     C                   ENDSR
020100000000     C************************************************************
020200000000     C* Tipo QUA BL
020300000000     C************************************************************
020400000000     C     ElaRecBL      BEGSR
020500000000     C*
020600000000     C                   Clear                   SG61DRBL
020700000000     C*
020800000000     C                   MoveL     ETEREC        DATICAS
020900000000     C                   MoveA     DATIETW2      SKDATI
021000000000     C*
021100000000     C     1             Do        75            K                 5 0
021200000000     C*-
021300000000     C                   MoveL     SKDATI(K)     CampoWrk
021400000000     C*
021500000000     C                   If        Quadro        = *blanks
021600000000     C                   Leave
021700000000     C                   EndIf
021800000000     C*
021900000000     C                   Move      DATICFI       P610CFI
022000000000     C                   Move      DATIPRGMOD    P610NRMOD
022100000000     C*
022200000000     C                   Select
022300000000     C*-
022400000000     C                   When      ContaRigo     = 1    and
022500000000     C                             ContaColo     = 1
022600000000     C                   CAT       Campo_Valore:0P610COGRAG
022700000000     C                   When      ContaRigo     = 1    and
022800000000     C                             ContaColo     = 2
022900000000     C                   CAT       Campo_Valore:0P610NOMEG
023000000000     C                   When      ContaRigo     = 1    and
023100000000     C                             ContaColo     = 3
023200000000     C                   Move      Campo_Valore  DTGMA
023300000000     C                   Move      GMAAnno       P610DTNASA
023400000000     C                   Move      GMAMese       P610DTNASM
023500000000     C                   Move      GMAGiorno     P610DTNASG
023600000000     C****               Move      DTGMA1        P610DTNASC
023700000000     C                   When      ContaRigo     = 1    and
023800000000     C                             ContaColo     = 4
023900000000     C                   CAT       Campo_Valore:0P610LOCNAS
024000000000     C                   When      ContaRigo     = 1    and
024100000000     C                             ContaColo     = 5
024200000000     C                   MoveL     Campo_Valore  P610PRVNAS
024300000000     C                   When      ContaRigo     = 1    and
024400000000     C                             ContaColo     = 6
024500000000     C                   Move      Campo_Valore  P610UIC1
024600000000     C                   When      ContaRigo     = 1    and
024700000000     C                             ContaColo     = 7
024800000000     C                   CAT       Campo_Valore:0P610RAGSOC
024900000000     C                   When      ContaRigo     = 1    and
025000000000     C                             ContaColo     = 8
025100000000     C                   CAT       Campo_Valore:0P610CITTAE
025200000000     C                   When      ContaRigo     = 1    and
025300000000     C                             ContaColo     = 9
025400000000     C                   Move      Campo_Valore  P610UIC2
025500000000     C                   When      ContaRigo     = 1    and
025600000000     C                             ContaColo     = 10
025700000000     C                   CAT       Campo_Valore:0P610INDIRE
025800000000     C* Identificativo Iva
025900000000     C                   When      ContaRigo     = 2    and
026000000000     C                             ContaColo     = 1
026100000000     C                   MoveL     Campo_Valore  P610CIVA
026200000000     C* Operazioni con fiscalità privilegiata
026300000000     C                   When      ContaRigo     = 2    and
026400000000     C                             ContaColo     = 2
026500000000     C* Operazioni con soggetti non residenti
026600000000     C                   Move      Campo_Valore  P610OPEBL
026700000000     C                   When      ContaRigo     = 2    and
026800000000     C                             ContaColo     = 3
026900000000     C                   Move      Campo_Valore  P610OPENRE
027000000000     C* Acquisti di servizi da soggetti non residenti
027100000000     C                   When      ContaRigo     = 2    and
027200000000     C                             ContaColo     = 4
027300000000     C                   Move      Campo_Valore  P610ACQSRV
027400000000     C*
027500000000     C* Operazioni ATTIVE
027600000000     C* Operazioni imponibili, non imponibili ed esenti
027700000000     C                   When      ContaRigo     = 3    and
027800000000     C                             ContaColo     = 1
027900000000     C                   Move      Campo_Valore  P610IMPATT
028000000000     C                   When      ContaRigo     = 3    and
028100000000     C                             ContaColo     = 2
028200000000     C                   Move      Campo_Valore  P610IVAATT
028300000000     C* Operazioni non soggette a Iva
028400000000     C                   When      ContaRigo     = 4    and
028500000000     C                             ContaColo     = 1
028600000000     C                   Move      Campo_Valore  P610NOIVAB
028700000000     C                   When      ContaRigo     = 4    and
028800000000     C                             ContaColo     = 2
028900000000     C                   Move      Campo_Valore  P610NOIVAS
029000000000     C* Note di variazione
029100000000     C                   When      ContaRigo     = 5    and
029200000000     C                             ContaColo     = 1
029300000000     C                   Move      Campo_Valore  P610IMANOT
029400000000     C                   When      ContaRigo     = 5    and
029500000000     C                             ContaColo     = 2
029600000000     C                   Move      Campo_Valore  P610IVANOT
029700000000     C*
029800000000     C* Operazioni PASSIVE
029900000000     C* Operazioni imponibili, non imponibili ed esenti
030000000000     C                   When      ContaRigo     = 6    and
030100000000     C                             ContaColo     = 1
030200000000     C                   Move      Campo_Valore  P610IMPPAS
030300000000     C                   When      ContaRigo     = 6    and
030400000000     C                             ContaColo     = 2
030500000000     C                   Move      Campo_Valore  P610IVAPAS
030600000000     C* Operazioni non soggette a Iva
030700000000     C                   When      ContaRigo     = 7    and
030800000000     C                             ContaColo     = 1
030900000000     C                   Move      Campo_Valore  P610NOIMPO
031000000000     C* Note di variazione
031100000000     C                   When      ContaRigo     = 8    and
031200000000     C                             ContaColo     = 1
031300000000     C                   Move      Campo_Valore  P610IMPNOT
031400000000     C                   When      ContaRigo     = 8    and
031500000000     C                             ContaColo     = 2
031600000000     C                   Move      Campo_Valore  P610IVPNOT
031700000000     C*
031800000000     C                   EndSl
031900000000     C*
032000000000     C                   EndDo
032100000000     C*
032200000000     C                   WRITE     SG61DRBL
032300000000     C                   Clear                   SG61DRBL
032400000000     C*
032500000000     C                   EndSr
032600000000     C************************************************************
032700000000     C* Inizializzazioni variabili
032800000000     C************************************************************
032900000000     C     InzVar        BegSr
033000000000     C*
033100000000     C                   MoveL     KPJBU         SDGSG61DS
033200000000     C*
033300000000     C                   Eval      ERRG61      = *Off
033400000000     C*
033500000000     C                   MoveL     'SOC001'      TIPXSC
033600000000     C                   MoveL     SOCIETAG61    SOCXSC
033700000000     C*
033800000000     C                   Exsr      RepSoc
033900000000     C                   If        RTNXSC     <> '1'
034000000000     C                   MoveL     XSOCDS        SOC001
034100000000     C                   ElSe
034200000000     C                   Exsr      EndPgm
034300000000     C                   EndIf
034400000000     C*
034500000000     C                   EndSr                                                  *
034600000000     C************************************************************
034700000000     C* OPERAZIONI INIZIALI
034800000000     C************************************************************
034900000000     C     *INZSR        BegSr
035000000000     C*
035100000000     C* Reperimento parametri
035200000000     C*
035300000000     C     *ENTRY        PLIST                                                  *
035400000000     C                   PARM                    KPJBA                          *
035500000000     C*
035600000000     C                   EndSr
035700000000     C************************************************************
035800000000     C* DEFINIZIONE KLIST
035900000000     C************************************************************
036000000000     C     DefKlist      BegSr
036100000000     C*
036200000000     C* KLIST
036300000000     C*
036400000000     C                   EndSr
036500151203R195 X************************************************************
036600151203 "   X**** Reperimento parametri di setup per Black list
036700151203 "   X************************************************************
036800151203 "   X**** RepParMod     BEGSR
036900151203 "   X****
037000151203 "   X****               Clear                   SDGPA08DS
037100151203 "   X****
037200151203 "   X****               CALLB     'XPAR'
037300151203 "   X****               Parm                    XScSoc
037400151203 "   X****               Parm      'SDGSPECF'    XPaPar            8
037500151203 "   X****               Parm                    XPaOut         2000
037600151203 "   X****               Parm      *OFF          XPaErr            1
037700151203 "   X****               Parm      ' '           XPaRic            1
037800151203 "   X****
037900151203 "   X****               if        XPaErr     <> *On
038000151203 "   X****               MoveL     XPaOut        SDGPA08DS
038100151203 "   X****               endif
038200151203 "   X****
038300151203R195 X****               EndSr
038400000000     C************************************************************
038500000000     C* Reperimento dati società
038600000000     C************************************************************
038700000000     C     RepSoc        BegSr
038800000000     C*
038900000000     C                   CALL      'SDGSOCDR'
039000000000     C                   PARM                    TIPXSC            6
039100000000     C                   PARM                    SOCXSC            3
039200000000     C                   PARM                    CDSXSC            9 0
039300000000     C                   PARM                    MODXSC            3
039400000000     C                   PARM      *Blanks       RTNXSC            1
039500000000     C                   PARM                    XSOCDS
039600000000     C                   PARM                    KPJBA
039700000000     C*
039800000000     C                   EndSr
039900000000     C************************************************************
040000000000     C* FINE PROGRAMMA
040100000000     C************************************************************
040200000000     C     EndPgm        BegSr
040300000000     C*
040400000000     C                   MoveL     SDGSG61DS     KPJBU
040500000000     C                   SETON                                        LR
040600000000     C*
040700000000     C                   RETURN
040800000000     C*
040900000000     C                   EndSr
041000000000     C************************************************************
