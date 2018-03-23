000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200991229     F* tibs06r * -----------------------------------------*
000300991230     F*          Stampa Tabelle
000400000000     F*----------------------------------------------------*
000500991229     Ftntbe02l  iF   E           K DISK
000600991230     Fprtf198   O    F  198        PRINTER oflind(*INOF)
000700991230     D TES             S             66    DIM(12) CTDATA PERRCD(1)             TESTATA STAMPA
000800991230     D PARUT         E DS                  EXTNAME(UT§DSE0F)
000900991230     D cncr80        E DS
001000991230     D KPJBA         E DS
001100991229     D TIBS06DS      E DS
001200991230     D wdata           S               d   datfmt(*eur)
001300971021     F*----------------------------------------------------*
001400971021      * INDICATORI
001500991230     c* 01   - stampo 2 righe di tbeuni
001600991230     c* 30   - comodo
001700000000     C*----------------------------------------------------*
001800000000      * CHIAVI
001900971016     C*----------------------------------------------------*
002000991230     C     KTbe          KLIST                                                  *
002100991230     C                   KFLD                    t06cod                         *
002200991230     C                   KFLD                    t06lin                         *
002300991230     C                   KFLD                    t06sif                         *
002400991230     C     KTbe2         KLIST                                                  *
002500991230     C                   KFLD                    kcod                           *
002600991230     C                   KFLD                    t06lin                         *
002700991230     C                   KFLD                    t06sif                         *
002800991230     C                   KFLD                    kke1                           *
002900971016     c*
003000971016     c     *like         define    tbecod        kcod
003100991230     c     *like         define    tbeke1        kke1
003200971016     C*----------------------------------------------------*
003300000000     C     *ENTRY        PLIST                                                  *
003400000000     C                   PARM                    KPJBA                          *
003500991230     C                   PARM                    tibs06ds                       *
003600991230     C
003700991230     c*
003800890429     C                   Z-ADD     1             CODUT
003900991230     C                   CALL      'X§PARUT'
004000000000     C                   PARM                    PARUT
004100000000     C                   MOVEL     RAGUT         RSUT             20
004200991230     c                   movel     rec80         cncr80
004300991230     c
004400991230     c                   time                    utime            14 0
004500991230     c                   move      utime         wdtgio            8 0
004600991230     c
004700991230     c* Decodifica codice tabella
004800991230     c                   clear                   kcod
004900991230     c                   movel     *all'0'       kke1
005000991230     c                   move      t06cod        kke1
005100991230     c     ktbe2         chain     tntbe02l                           31
005200991230     c**
005300991230     c                   movel     tbeuni        p1dcod           50
005400991230     c                   movel     tbeapl        p1capl            2
005500991230     c**
005600991230     c                   except    TESTA
005700991230     c**
005800991230     c     ktbe          setll     tntbe02l
005900991230     c     ktbe          reade     tntbe02l                               30
006000991230     c                   dow       not *in30
006100991230     c                   exsr      stampa
006200991230     c     ktbe          reade     tntbe02l                               30
006300991230     c                   enddo
006400991230     c                   except    PIEDI
006500971016     C*
006600000000     C                   SETON                                        LR        *
006700991229     c*--------------------------------------------------------------------
006800991230     c*  stampa tabella
006900991229     c*--------------------------------------------------------------------
007000991230     C     stampa        BEGSR                                                  *
007100991230     c                   if        *inof
007200991230     c                   except    TESTA
007300991230     c                   setoff                                       of
007400991230     c                   endif
007500991230     c
007600991230     c                   movel     tbeuni        p1ctb1          140
007700991230     c                   move      tbeuni        p1ctb2          116
007800991230     c                   setoff                                       01
007900991230     c
008000991230     c                   if        p1ctb2<>*blanks
008100991230     c                   seton                                        01
008200991230     c                   endif
008300991230     c
008400991230     c                   clear                   p1cftt
008500991230     c                   if        tbeftt='S'
008600991230     c                   movel     'SI'          p1cftt            2
008700991230     c                   endif
008800991230     c                   clear                   p1cdtr            8 0
008900991230     c                   if        tbedtr>0
009000991230     c     *iso          move      tbedtr        wdata
009100991230     c     *eur          move      wdata         p1cdtr
009200991230     c                   endif
009300991230     c
009400991230     c                   clear                   p1cann
009500991230     c                   if        tbeatb<>*blanks
009600991230     c                   movel     'ANN'         p1cann            3
009700991230     c                   endif
009800991230     c
009900991230     c                   except    dett
010000991229
010100991229     c                   ENDSR
010200991230     c
010300991230     OPRTF198   E            TESTA            02
010400991230     O                       tes(1)             132
010500991230     O                       RSUT                20
010600991230     O                                           21 '/'
010700991230     O                       SIMFEL        z     24
010800991230     O                       WDTGIO             150 '  /  /    '
010900991230     O                                          191 'PAG.'
011000991230     O                       PAGE          Z    198
011100991230     O          E            TESTA            04
011200991230     O                       tes(2)              66
011300991230     O                       tes(3)             132
011400991230     O                       tes(4)             198
011500991230     O                       t06cod              20
011600991230     O                       p1dcod              73
011700991230     O                       t06lin              97
011800991230     O                       t06sif             142
011900991230     O                       p1capl             170
012000991230     O          E            TESTA            06
012100991230     O                       tes(5)              66
012200991230     O                       tes(7)             198
012300991230     O          E            TESTA            07
012400991230     O                       tes(10)             66
012500991230     O                       tes(11)            132
012600991230     O                       tes(8)             198
012700991230     O          E            DETT        1
012800991230     O                       tbeke1              15
012900991230     O                       tbeke2           +   1
013000991230     O                       p1ctb1           +   1
013100991230     O                       p1cann           +   2
013200991230     O                       p1cftt           +   2
013300991230     O                       tbeflt        z  +   1
013400991230     O                       tbeftr           +   1
013500991230     O                       p1cdtr           +   1 '  /  /    '
013600991230     O          E    01      DETT        1
013700991230     O                       p1ctb2           +  32
013800991230     O          E            PIEDI            63
013900991230     O                       tes(12)            132
014000971016
014100971016**
014200991230             ***  S T A M P A       T A B E L L E  ***              1
014300991230Codice tabella : xxx - xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  2
014400991230xxxxxxx              Lingua : x              Sistema Informativo :  3
014500991230xxxxxxxxxx           Applicazione : xx                              4
014600991230----CHIAVE 1--- ----CHIAVE 2---                                     5
014700991230xxxxxxxxxxxxxxx xxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  6
014800991230                                                  TRASMISSIONE      7
014900991230....-....1....-....2....-....3....-....4       Si P.O     Data      8
015000991230                                          ANN  XX XXX X XX/XX/XXXX  9
015100991230                                ....-....1....-....2....-....3.... 10
015200991230-....4....-....5....-....6....-....7....-....8....-....9....-....0 11
015300991230                ***  F I N E   S T A M P A  ***                    12
