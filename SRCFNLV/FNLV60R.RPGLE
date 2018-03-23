000100021009     H DECEDIT('0,') DATEDIT(*DMY.)
000200021009
000300021009     D CM2             S              1    DIM(48) CTDATA PERRCD(48)
000400021009     D CM3             S              1    DIM(48) CTDATA PERRCD(48)
000500021009     D*
000600960516     D DSLV60        E DS                  EXTNAME(FNLV60DS)
000700971119     D DSLV55        E DS                  EXTNAME(FNLV55DS)
000800960516     D KPJBA         E DS
000900021009     D UT§DSE0F      E DS
001000021009     D CNCR80        E DS
001100021009     D*
001200021009     D DSmembro        ds             7    inz
001300021009     D  MbrM                   1      1    inz('M')
001400021009     D  MbrPOP                 2      4  0 inz
001500021009     D  MbrPOA                 5      7  0 inz
001600021009     D*
001700971119     D WLBDAT          DS
001800971119     D  G02DAT                 1      8  0
001900971119     D  G02INV                 9     16  0
002000971119     D  G02ERR                17     17
002100971119     D  G02TGI                18     22  0
002200021009
002300960516     C     *ENTRY        PLIST
002400960516     C                   PARM                    KPJBA
002500960516     C*
002600960516     C                   MOVEL     KPJBU         DSLV60
002700021009     C*
002800021009    1C                   if        D60po0 <= *zeros
002900021009     C                   call      'X§PARUT'
003000021009     C                   parm                    UT§DSE0F
003100021009     C                   movel     Rec80         CNCR80
003200021009     C                   eval      D60po0 = SIMfel
003300021009    1C                   endif
003400960516     C*
003500960516    1C     D60TLA        IFNE      'C'
003600960516     C* PULISCO PARAMETRI DI OUTPUT
003700960516     C                   MOVE      'N'           D60ERR
003800960516     C                   CLEAR                   D60MB1
003900960516     C                   CLEAR                   D60MB2
004000960516     C* PULISCO CAMPI DI WORK
004100960516     C                   MOVE      *ZEROS        WFEL1A            3
004200960516     C                   MOVE      *ZEROS        WFEL2A            3
004300971119     C                   MOVE      *ZEROS        WDRF              8 0
004400971119     C* SE NON PASSATA DATA DI RIFERIM. LA IMPOSTO=ALLA DATA DEL GIORNO
004500971119     C     D60DRF        IFEQ      *ZEROS
004600971119     C                   TIME                    WTIME            14 0
004700971119     C                   MOVE      WTIME         WDATE             8 0
004800971119     C                   CLEAR                   WLBDAT
004900971119     C                   Z-ADD     WDATE         G02DAT
005000971119     C                   CALL      'XSRDA8'
005100971119     C                   PARM                    WLBDAT
005200971119     C                   Z-ADD     G02INV        WDRF
005300971119     C                   ELSE
005400971119     C                   Z-ADD     D60DRF        WDRF
005500971119     C                   END
005501060711     c
005502060711     c* Se passata azione = 'X' ma pieno secondo p.o. da allocare
005503060711     c*  trasformo il flag in "A"
005504060711     c                   if        d60azn='X' and d60fl2>0
005505060711     c                   eval      d60azn='A'
005506060711     c                   endif
005507060711     c
005600971119     C* IMPOSTO TERMINAL DI PARTENZA
005700960516    2C     D60FL1        IFGT      *ZEROS
005800971119     C                   CLEAR                   DSLV55
005900971119     C                   MOVEL     'P'           D55TPT
006000971119     C                   MOVEL     D60FL1        D55LIN
006100971119     C                   MOVEL     WDRF          D55DRF
006200971119     C                   CALL      'FNLV55R'
006300971119     C                   PARM                    DSLV55
006400971119     C     D55ERR        IFNE      *BLANKS
006500971119     C                   MOVE      D60FL1        D55TFP
006600971119     C                   END
006700971119     C                   MOVE      D55TFP        WFEL1A
006800960516    2C                   END
006900960516     C*
007000960516    2C     D60FL2        IFGT      *ZEROS
007100971119     C                   CLEAR                   DSLV55
007200971119     C                   MOVEL     'P'           D55TPT
007300971119     C                   MOVEL     D60FL2        D55LIN
007400971119     C                   MOVEL     WDRF          D55DRF
007500971119     C                   CALL      'FNLV55R'
007600971119     C                   PARM                    DSLV55
007700971119     C     D55ERR        IFNE      *BLANKS
007800971119     C                   MOVE      D60FL2        D55TFP
007900971119     C                   END
008000971119     C                   MOVE      D55TFP        WFEL2A
008100960516    2C                   END
008200960516     C*
008300971119     C* SE STESSO TERMINALE DI PARTENZA NE TENGO SOLO UNO
008400960516     C     WFEL1A        IFEQ      WFEL2A
008500960516     C                   MOVE      *ZEROS        WFEL2A
008600960516     C                   END
008700960516     C*
008800021009     c                   reset                   DSmembro
008900021009     c                   eval      MbrPOP = D60po0
009000021009     C***                MOVE      WFEL1A        D60MB1
009100021009     c                   move      wFEL1a        MbrPOA
009200021009     c                   movel(p)  DSmembro      D60mb1
009300021009     c                   if        wFEL2a <> *zeros
009400021009     C***                MOVE      WFEL2A        D60MB2
009500021009     c                   move      wFEL2a        MbrPOA
009600021009     c                   movel(p)  DSmembro      D60mb2
009700021009     c                   endif
009800960516     C*
009900960516    2C                   SELECT
010000960516     C* D60AZN='A' --> RICHIESTA  ALLOCAZIONE
010100960516     C     D60AZN        WHENEQ    'A'
010101060711     C     D60AZN        oreq      'X'
010200960516    3C     WFEL1A        IFGT      *ZEROS
010300021009     C                   MOVE      WFEL1A        MbrPOA
010301060711     c
010400060711     C* Chkobj e addpfm solo se lo devo anche creare
010401060711     C                   if        d60azn='A'
010500960516     C                   MOVEL     D60FIL        FISICO
010600021009     C                   MOVEL     D60mb1        FISMBR
010700960516     C                   CALL      'TRUL50C'
010800960516     C                   PARM                    FISICO           10
010900960516     C                   PARM                    FISMBR           10
011000960516     C                   PARM                    KNSIF
011100960516     C                   PARM                    LOGICO           10
011200960516     C                   PARM                    LOGMBR           10
011300960516     C                   PARM                    ULFLG             1
011301060711     c                   endif
011302060711     c
011400960516      * Tento l'allocazione del file di trasmissione
011500021009     C***                MOVEA     WFEL1A        CM2(41)
011600960516     C                   MOVEA     D60FIL        CM2(13)
011700971128     C                   MOVEA     D60VNC        CM2(30)
011800021009     C                   movea     DSmembro      CM2(40)
011900960516     C                   MOVEA(P)  CM2           COMMAN
012000960516     C                   CALL      'QCMDEXC'                            30
012100960516     C                   PARM                    COMMAN           80
012200960517     C                   PARM      80            LUNG             15 5
012300960516    4C     *IN30         IFEQ      *ON
012400960516     C                   MOVEL     'S'           D60ERR
012500960516     C                   GOTO      FINE
012600960516    4C                   END
012700960516    3C                   END
012800960516     C* Ripeto per la seconda filiale
012900960516    3C     WFEL2A        IFGT      *ZEROS
013000021009     C                   MOVE      WFEL2A        MbrPOA
013100960516     C* Chkobj e addpfm
013200960516     C                   MOVEL     D60FIL        FISICO
013300021009     C                   MOVEL(p)  D60mb2        FISMBR
013400960516     C                   CALL      'TRUL50C'
013500960516     C                   PARM                    FISICO           10
013600960516     C                   PARM                    FISMBR           10
013700960516     C                   PARM                    KNSIF
013800960516     C                   PARM                    LOGICO           10
013900960516     C                   PARM                    LOGMBR           10
014000960516     C                   PARM                    ULFLG             1
014100960516      * Tento l'allocazione del file di trasmissione
014200021009     C***                MOVEA     WFEL2A        CM2(41)
014300960516     C                   MOVEA     D60FIL        CM2(13)
014400971128     C                   MOVEA     D60VNC        CM2(30)
014500021009     C                   movea     DSmembro      CM2(40)
014600960516     C                   MOVEA(P)  CM2           COMMAN
014700960516     C                   CALL      'QCMDEXC'                            30
014800960516     C                   PARM                    COMMAN           80
014900960517     C                   PARM      80            LUNG             15 5
015000960516    4C     *IN30         IFEQ      *ON
015100961106     C* SE NON E' RIUSCITA L'ALLOCAZIONE DEL SECONDO MEMBRO, DISALLOCO
015200961106     C*   ANCHE IL PRIMO
015300961106     C                   EXSR      DLCFI1
015400960516     C                   MOVEL     'S'           D60ERR
015500960516     C                   GOTO      FINE
015600960516    4C                   END
015700960516    3C                   END
015800961106     C*
015900960516     C* D60AZN='D' --> RICHIESTA DISALLOCAZIONE
016000960516     C     D60AZN        WHENEQ    'D'
016100960516    3C     WFEL1A        IFGT      *ZEROS
016200961106     C                   EXSR      DLCFI1
016300960516    3C                   END
016400960516     C*
016500960516    3C     WFEL2A        IFGT      *ZEROS
016600021009     C                   move      wFEL2a        MbrPOA
016700021009     C***                MOVEA     WFEL2A        CM3(41)
016800960516     C                   MOVEA     D60FIL        CM3(13)
016900971128     C                   MOVEA     D60VNC        CM3(30)
017000021009     C                   movea     DSmembro      CM3(40)
017100960516     C                   MOVEA(P)  CM3           COMMAN
017200960516     C                   CALL      'QCMDEXC'                            30
017300960516     C                   PARM                    COMMAN           80
017400960517     C                   PARM      80            LUNG             15 5
017500960516    3C                   END
017600960516    2C                   ENDSL
017700960516    1C                   END
017800960516     C*
017900960516     C     FINE          TAG
018000960520     C                   MOVEL     DSLV60        KPJBU
018100960516     C*
018200960516    1C     D60TLA        IFEQ      ' '
018300960516     C                   RETURN
018400960516   X1C                   ELSE
018500971119     C                   MOVEL     'C'           D55TLA
018600971119     C                   CALL      'FNLV55R'
018700971119     C                   PARM                    DSLV55
018800960516     C                   SETON                                        LR
018900960516    1C                   END
019000961106     C*
019100961106     C*--- DISALLOCAZIONE PRIMO MEMBRO -------------------------------*
019200961106     C     DLCFI1        BEGSR
019300021009     C                   move      wFEL1a        MbrPOA
019400021009     C***                MOVEA     WFEL1A        CM3(41)
019500961106     C                   MOVEA     D60FIL        CM3(13)
019600971128     C                   MOVEA     D60VNC        CM3(30)
019700021009     C                   movea     DSmembro      CM3(40)
019800961106     C                   MOVEA(P)  CM3           COMMAN
019900961106     C                   CALL      'QCMDEXC'                            30
020000961106     C                   PARM                    COMMAN           80
020100961106     C                   PARM      80            LUNG             15 5
020200961106     C                   ENDSR
020300961106     C*---------------------------------------------------------------*
020400971128**         CM2
020500021009ALCOBJ OBJ((XXXXXXXXXX *FILE           M000000))
020600960516**         CM3
020700021009DLCOBJ OBJ((XXXXXXXXXX *FILE           M000000))
