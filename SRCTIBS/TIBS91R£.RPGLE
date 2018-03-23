000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200090116      * tibs91r *----------------------------------------------------*
000300090116      *    - Duplica dati di fisgn per etichettare in altro TERMINAL
000400940915      *--------------------------------------------------------------*
000500090116     FTIBS91D   CF   E             WORKSTN
000600951013     FAZORG01L  IF   E           K DISK
000700090116     Ffisgn07l  IF   E           K DISK
000800020805      *
000900020805      * DEFINIZIONE SCHIERE
001000960302     D*
001100050103     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001200040917     D KPJBA         E DS
001300040917     D CNCR80        E DS
001400040917     D UT§DSE0F      E DS
001500090116     D fnlv55ds      E DS
001600090116     D podatfp         S              3  0
001700090116     D poatfp          S              3  0
001800050103     C**********************************************************************
001900040917     C     *ENTRY        PLIST
002000040917     C                   PARM                    KPJBA
002100050103     c*
002200040917     C                   Z-ADD     1             CODUT
002300040917     C                   CALL      'X§PARUT'
002400040917     C                   PARM                    UT§DSE0F
002500040917     C                   MOVEL     REC80         CNCR80
002600040917     C                   MOVEL     RAGUT         VIDRSU
002700090116     C                   CLEAR                   V1CPOda
002800090116     C                   CLEAR                   V1DPOda
002900090116     C                   CLEAR                   V1Cpoa
003000090116     C                   CLEAR                   V1dpoa
003100040917     c
003200040917     C     FOR01         TAG
003300000828      *
003400090116     C                   EXFMT     BS91D01
003500970102     C                   SETOFF                                       289050
003600040917     C* F3 - FINE
003700040917     C   KC              GOTO      FINE
003800041220      *  CONTROLLI
003900040917     C                   EXSR      CTRD01
004000040917     c   90              goto      for01
004100000825      *
004200040917     c*
004300090116     c                   if        not *inkf
004400041220     c                   goto      for01
004500041220     c                   endif
004600040917     C*
004700040917      * F6  - SOTTOMETTO ELABORAZIONE
004800050412    1C     *INKF         IFEQ      *ON
004900050214     c*
005000090116     c                   call      'TIBS92R'
005100090116     c                   parm                    v1cpoda
005200090116     c                   parm                    v1cpoa
005300090119     c                   parm                    podatfp
005400090116    1C                   ENDIF
005500050412     c
005600900515      *
005700000000     C     FINE          TAG
005800090116     C*
005900090116     c                   clear                   fnlv55ds
006000090116     C                   MOVEL     'C'           D55TLA
006100090116     C                   CALL      'FNLV55R'
006200090116     C                   PARM                    FNLV55DS
006300000000     C                   SETON                                        LR
006400900515      *-----------------------------------------------------***********
006500090116      * CONTROLLI                                           *  CTRD01 *
006600900515      *-----------------------------------------------------***********
006700040917     C     CTRD01        BEGSR
006800050104     c                   setoff                                       010206
006900090116      * PUNTO OPERATIVO  DA
007000090116     C                   MOVE      V1CPOda       CODFIL            3 0
007100951025     C                   EXSR      CTRFIL
007200041220    1c                   if        not *in34
007300090116     c                   movel     orgdes        V1DPOda
007400040917     c
007500041220   x1C                   ELSE
007600040917     c                   seton                                        509028
007700041220     C                   MOVEL     MSG(1)        V1cMSG
007800041220     c                   goto      endct1
007900041220    1C                   ENDIF
008000090116     c* Verifico se presenti dei dati
008100090116     c     v1cpoda       setll     fisgn07l
008200090116     c                   if        not %equal(fisgn07l)
008300090116     c                   seton                                        509028
008400090116     C                   MOVEL     MSG(3)        V1cMSG
008500090116     c                   goto      endct1
008600090116     c                   endif
008700090116     c
008800090116     c* controllo il suo terminal partenza
008900090116     c                   clear                   fnlv55ds
009000090116     C                   MOVEL     'P'           D55TPT
009100090116     c                   eval      d55lin=v1cpoda
009200090116     c                   eval      d55drf=*date
009300090116     C                   CALL      'FNLV55R'
009400090116     C                   PARM                    FNLV55DS
009500090116     c                   eval      podatfp=d55tfp
009600090116     c*
009700090116      * PUNTO OPERATIVO  A  deve essere un terminal
009800090116     C                   MOVE      V1CPOa        CODFIL            3 0
009900090116     C                   EXSR      CTRFIL
010000090116    1c                   if        not *in34
010100090116     c                   movel     orgdes        V1DPOa
010200090116     c
010300090116   x1C                   ELSE
010400090116     c                   seton                                        519028
010500090116     C                   MOVEL     MSG(1)        V1cMSG
010600090116     c                   goto      endct1
010700090116    1C                   ENDIF
010800090116     c* controllo il suo terminal partenza
010900090116     c                   clear                   fnlv55ds
011000090116     C                   MOVEL     'P'           D55TPT
011100090116     c                   eval      d55lin=v1cpoa
011200090116     c                   eval      d55drf=*date
011300090116     C                   CALL      'FNLV55R'
011400090116     C                   PARM                    FNLV55DS
011500090116     c                   eval      poatfp=d55tfp
011600090116
011700090116     c                   if        poatfp<>v1cpoa
011800090116     c                   seton                                        519028
011900090116     C                   MOVEL     MSG(4)        V1cMSG
012000090116     c                   goto      endct1
012100090116    1C                   ENDIF
012200090116     c
012300090119     c* Devono essere diversi sia le filiali che i terminal
012400090116     c                   if        v1cpoda=v1cpoa  or  podatfp=poatfp
012500090116     c                   seton                                        519028
012600090116     C                   MOVEL     MSG(2)        V1cMSG
012700090116     c                   goto      endct1
012800090116    1C                   ENDIF
012900041220     c*
013000900515      *
013100040917     C     ENDCT1        ENDSR
013200951025     C**************************************************************************
013300951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
013400951025     C**************************************************************************
013500951025     C     CTRFIL        BEGSR
013600020213
013700951025     C     CODFIL        CHAIN     AZORG01L                           34
013800951025    1C  N34ORGFVA        IFNE      ' '
013900951025     C     ORGFAG        ORNE      'A'
014000951025     C     ORGFAG        ANDNE     'F'
014100951025     C                   SETON                                        34
014200951025    1C                   ENDIF
014300951025     C*
014400951025     C                   ENDSR
014500020805      **************************************************************************
014600960302** SCHIERA MSG - MESSAGGI DI ERRORE
014700090116Filiale  INESISTENTE                                                          01
014800090116Le 2 filiali  devono essere diverse e avere terminal di partenza diversi      01
014900090116Non è presente nessun record in FISGN00F per la filiale immessa
015000090116si può trasferire solo ad un terminal di partenza
