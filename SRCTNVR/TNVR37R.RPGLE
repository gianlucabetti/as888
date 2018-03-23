000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020326      * TNVR37R *----------------------------------------------------*
000300020326      * CONTROLLO VISITE CON PROGRESSIVO
000400020114      *--------------------------------------------------------------*
000500020115      *
000600020326     FTnVis05L  IF   E           K DISK
000700020118      *
000800020123     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900020115      *----------------------------------------------------------------
001000020125     D                 DS
001100020326     D  Vis                    1      7  0
001200020326     D  Cod                    1      6  0
001300020326     D  Prg                    7      7  0
001400020125      *
001500020326     D                 DS
001600020326     D  VisNrv                 1      7  0
001700020326     D  Prg6                   6      6  0
001800020326     D  Prg7                   7      7  0
001900921027     C*****************************************************************
002000020125      *
002100020326     C     *loval        setll     TnVis05l
002200020326     C                   read      TnVis05l                               30
002300020326      * 30 on - fine lettura
002400020326 1a  C     *in30         doweq     '0'
002500020326     C                   move      visnrv        SAVVIS            7 0
002600020326     C                   move      visdvi        SAVDVI            8
002700020326     C                   movel     SAVDVI        SAVAA             4
002800020326     C                   move      visnrv        Vis
002900020326      *
003000020326 2a  C     Prg           ifne      0
003100020326     C                   setoff                                       50
003200020326     C                   clear                   Pvisnrv
003300020326     C                   clear                   Pvistpv
003400020326     C                   clear                   Pvisksc
003500020326     C                   clear                   Pvisrag
003600020326     C                   clear                   Pviscmm
003700020326     C                   move      visnrv        Pvisnrv
003800020326     C                   move      visdvi        Pvisdvi
003900020326     C                   move      vistpv        Pvistpv
004000020326     C                   move      visksc        Pvisksc
004100020326     C                   move      visrag        Pvisrag
004200020326     C                   move      viscmm        Pviscmm
004300020326     C                   move      visfsv        Pvisfsv
004400020326      *
004500020326     C                   move      0             Prg7
004600020326     C     visnrv        chain     Tnvis05l                           31
004700020326  3a C     *in31         ifeq      *off
004800020326      *
004900020326      * controllo data visita
005000020326     C                   move      visdvi        $VisDvi           8
005100020326     C                   movel     $VisDvi       VisAA             4
005200020326  4a C     SavAA         ifge      VisAA
005300020326      *
005400020326  5a C     vistpv        ifne      Pvistpv
005500020326     C     visksc        orne      Pvisksc
005600020326     C     visrag        orne      Pvisrag
005700020326     C     viscmm        orne      Pviscmm
005800020326     C                   EXSR      Stampa
005900020326      * da togliere poi
006000020326  5e C                   else
006100020326     C***                EXSR      Stampa
006200020326     C                   seton                                        50
006300020326  5- C                   endif
006400020326  4- C                   endif
006500020326  3- C                   endif
006600020326      *
006700020326     C                   move      SAVVIS        Visnrv
006800020326     C                   move      0             Prg6
006900020326     C                   move      0             Prg7
007000020326     C     visnrv        chain     Tnvis05l                           32
007100020326  3a C     *in32         ifeq      *off
007200020326      *
007300020326      * controllo data visita
007400020326     C                   move      visdvi        $VisDvi           8
007500020326     C                   movel     $VisDvi       VisAA             4
007600020326  4a C     SavAA         ifge      VisAA
007700020326      *
007800020326  5a C     vistpv        ifeq      Pvistpv
007900020326     C     visksc        andeq     Pvisksc
008000020326     C     visrag        andeq     Pvisrag
008100020326     C     viscmm        andeq     Pviscmm
008200020326     C                   EXSR      Stampa
008300020326      * da togliere poi
008400020326  5e C                   else
008500020326   6 C     *in50         ifeq      *off
008600020326     C                   EXSR      Stampa
008700020326   6-C                   endif
008800020326  5- C                   endif
008900020326  4- C                   endif
009000020326  3- C                   endif
009100020326      *
009200020326      * mi riposiziono sulla visita letta inizio ciclo
009300020326     C     SAVVIS        setll     TnVis05l
009400020326     C                   read      TnVis05l
009500020326  2- C                   endif
009600020326      *
009700020326     C                   read      TnVis05l                               30
009800020326  1- C                   enddo
009900020326     C                   seton                                        LR
010000020326      *---------------------------------------------------------------*
010100020326     C     Stampa        Begsr
010200020326      * stampo
010300020326     C     *inOF         ifeq      *on
010400020326     C     *in77         oreq      *off
010500020326     C                   except    Testa
010600020326     C                   except    Testa1
010700020326     C                   seton                                        77
010800020326     C                   endif
010900020326     C                   except    DetPVis
011000020326     C                   except    DetVis
011100020326     C                   Endsr
011200020115      *---------------------------------------------------------------*
011300020114      *
011400020114      * DEFINIZIONE CAMPI
011500020115      *
011600020125      *---------------------------------------------------------------*
011700020326     C     *like         Define    vistpv        Pvistpv
011800020326     C     *like         Define    visksc        Pvisksc
011900020326     C     *like         Define    visrag        Pvisrag
012000020326     C     *like         Define    viscmm        Pviscmm
012100020326     C     *like         Define    visnrv        Pvisnrv
012200020326     C     *like         Define    visdvi        Pvisdvi
012300020326     C     *like         Define    visfsv        Pvisfsv
012400020111      *
012500020326     OQSYSPRT   E            TESTA          2 02
012600020326     O                                         + 35 'STAMPA CONTROLLO PROGRESS-
012700020326     O                                              IVI VISITA'
012800020326     O          E            TESTA1         1
012900020326     O                                         +  1 'N.Visita Dt.Visita  Cd.Sta-
013000020326     O                                              to S.Conto Descrizione'
013100020326     O                                          103 'Cd.Comm. Tp.Vis'
013200020326     O          E            DETPVis        2
013300020326     O                       PVisnrv           +  1 '      / '
013400020326     O                       PVisDvi           +  1 '    /  /  '
013500020326     O                       PVisfsv           +  5
013600020326     O                       PVisKsc           +  4
013700020326     O                       PVisrag           +  1
013800020326     O                       PViscmm           +  1
013900020326     O                       PVistpv           +  5
014000020326     O          E            DETVis         1
014100020326     O                       Visnrv            +  1 '      / '
014200020326     O                       VisDvi            +  1 '    /  /  '
014300020326     O                       Visfsv            +  5
014400020326     O                       VisKsc            +  4
014500020326     O                       Visrag            +  1
014600020326     O                       Viscmm            +  1
014700020326     O                       Vistpv            +  5
