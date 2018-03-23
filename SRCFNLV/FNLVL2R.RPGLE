000100980526     hdatfmt(*ymd) datedit(*ymd) decedit('0,')
000200980526
000300980603     ffnlvl02l  up   e             disk
000400980603     ffnlvld1l  if   e           k disk
000500980526     fprtf198   o    f  198        printer oflind(*inof)
000600980526
000700980603     dkpjba          e ds
000800980603
000900980603     ddatast           s              8  0
001000980527     d§dtada           s             10
001100980527     d§dtaa            s             10
001200980527     d§fmtda           s              7
001300980527     d§fmta            s              7
001400980527     d§sep             s              7
001500980527     d§err             s              1
001600980527
001700980526     dtesta01          c                   'Elenco resi Parfum et Beaute d-
001800980526     d                                     a ritirare'
001900980526
002000980526     ilnres
002100980526     i                                          lrprv         l1
002200980526     i                                          lrcli         l1
002300980603
002400980603     c     lrsoc         chain     lndiv                              01
002500980603
002600980603     c                   if        *in(01) = *off
002700980603
002800980526     c                   if        *inof = *on
002900980526     c                   except    testa
003000980526     c                   eval      *inof = *off
003100980526     c                   endif
003200980603
003300980526     c                   except    detta
003400980529
003500980603     c                   if        lrdts = 0
003600980603     c                   eval      lrdts = *date
003700980526     c                   except    aggres
003800980603     c                   endif
003900980529
004000980529     c                   endif
004100980529
004200980526     cl1                 except    vuota
004300980526
004400980526      *-------------
004500980526
004600980526     c     *inzsr        begsr
004700980527
004800980603     c     *entry        plist
004900980603     c                   parm                    kpjba
005000980603
005100980603     c                   movel(p)  *date         §dtada
005200980527     c                   eval      §dtaa = *blanks
005300980527     c                   eval      §fmtda = '*YYMD'
005400980527     c                   eval      §fmta = '*DMYY'
005500980527     c                   eval      §sep = '*NONE'
005600980527     c                   eval      §err = *blanks
005700980527     c                   call      'CVTDATA'
005800980527     c                   parm                    §dtada
005900980527     c                   parm                    §dtaa
006000980527     c                   parm                    §fmtda
006100980527     c                   parm                    §fmta
006200980527     c                   parm                    §sep
006300980527     c                   parm                    §err
006400980527
006500980527     c                   movel     §dtaa         datast
006600980527
006700980527     c                   eval      *inof = *on
006800980527
006900980526     c                   endsr
007000980526
007100980526      *-------------
007200980526
007300980526     olnres     e            aggres
007400980526     o                       lrdts
007500980526
007600980527     oprtf198   e            testa          2  3
007700980526     o                       testa01             99
007800980526     o                                          160 'Data'
007900980526     o                       datast             172 '  /  /    '
008000980526     o                                          190 'Pag.'
008100980526     o                       page          z    198
008200980526
008300980527     oprtf198   e            testa          2
008400980527     o                                           10 'Nominativo'
008500980527     o                                           46 'Indirizzo'
008600980527     o                                           77 'CAP'
008700980527     o                                           89 'Localita'
008800980527     o                                          110 'Pr'
008900980527     o                                          121 'Colli'
009000980527     o                                          130 'Peso'
009100980527     o                                          144 'N. reso'
009200980527     o                                          156 'Divisione'
009300980527
009400980527     oprtf198   e            detta          1
009500980526     o                       lrrgs
009600980526     o                       lrind            +   2
009700980526     o                       lrcap            +   2
009800980526     o                       lrloc            +   2
009900980526     o                       lrprv            +   2
010000980527     o                       lrcol         z  +   8
010100980526     o                       lrpkg         2  +   2
010200980527     o                       lrnrs         z  +   8
010300980527     o                       lddes            +   3
010400980526
010500980526     oprtf198   e            vuota
