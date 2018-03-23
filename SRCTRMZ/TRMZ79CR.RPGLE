000001111229     H DECEDIT('0,') DATEDIT(*DMY.)
000200050930
000300111229     Ftrmz79cd  CF   E             WORKSTN
000400050930     F                                     SFILE(S01:S01RRN)
000500111229     Faitrsa1l  IF   E           K DISK
001300050930
001301111229     d kpjba         e ds
001500111229     D parmcontr       s              7s 0
001501111229     d dataeur         s               d   datfmt(*eur)
001700050930
001800050930     D S01RRN          S              5I 0
001900050930     D S01RRNFirst     S              5I 0
002000050930     D S01RRNLast      S              5I 0
002100060302      *--------------------------------------------------------------------
002200060302     c                   exsr      elabora
002300060302     c                   seton                                        lr
002400050930
002500060302      *___________________________________________________                  ****
002600060302     C     elabora       BEGSR
002700060302      *___________________________________________________                  ****
002800111229      * reperisco appendici legate al contratto
002900111229     c     parmcontr     setll     aitrsa1l
003000060302     c                   do        *hival
003100111229     c     parmcontr     reade     aitrsa1l
003200111229     c                   if        %eof(aitrsa1l)
003300060302     c                   leave
003400060302     c                   endif
003800060302      * caricamento sfl
003900060302     c                   exsr      carica
004000060302     c                   enddo
004100060302      * emissione sfl
004101060302     c     s01rrn        comp      0                                  0101
004200060302     c                   if        s01rrn > 0
004300060302     c                   z-add     1             s01rrn
004400060302     c                   exsr      emissione
004500060302     c                   endif
004600060302     c                   endsr
004700060302      *___________________________________________________                  ****
004800060302     C     carica        BEGSR
004900060302      *___________________________________________________                  ****
004901111229     c                   move      trsacap       s01cap
004902111229     c                   clear                   s01dsc
004903111229     c                   clear                   s01dea
004904111229     c                   clear                   s01drc
004905111229     c                   clear                   s01unodc
004906111229     c                   if        trsadsc > 0
005000111229     c     *iso          move      trsadsc       dataeur
005100111229     c                   move      dataeur       s01dsc
005200111229     c                   endif
005201111229     c                   if        trsadea > 0
005202111229     c     *iso          move      trsadea       dataeur
005203111229     c                   move      dataeur       s01dea
005204111229     c                   endif
005205111229     c                   if        trsadrc > 0
005206111229     c     *iso          move      trsadrc       dataeur
005207111229     c                   move      dataeur       s01drc
005208111229     c                   endif
005209111229     c                   if        trsaunodc> 0
005210111229     c     *iso          move      trsaunodc     dataeur
005211111229     c                   move      dataeur       s01unodc
005212111229     c                   endif
005213120104     c                   if        s01unodc = s01dsc
005214120104     c                   clear                   s01dsc
005215120104     c                   endif
007616060323
007700060323     c                   if        s01rrn < 100
007800060302     c                   add       1             s01rrn
007900060302     c                   write     s01
008000060302     c                   endif
008100060302     c                   endsr
008200060302      *___________________________________________________                  ****
008300060302     C     emissione     BEGSR
008400060302      *___________________________________________________                  ****
008500051005
008501060323     c                   do        *hival
008701060302     C                   WRITE     f01
008702060302     C                   exfmt     c01
008900060302     c   kc              leave
009000060302     c                   enddo
009200050930
009201060323     C                   endsr
009300060302      *___________________________________________________                  ****
009400060302     C     *INZSR        BEGSR
009500060302      *___________________________________________________                  ****
009600060302     C     *ENTRY        PLIST
009700111229     C                   PARM                    kpjba
009701111229     c                   movel     kpjbu         parmcontr
010200111229     c                   movel     parmcontr     c01ctr
010700060302
011500060302      *inizializza sfl
011600060302     C                   SETOn                                        02
011700060302     C                   WRITE     C01
011800060302     C                   SETOff                                       02
011900060302     C                   CLEAR                   S01RRN
012000060302     C                   CLEAR                   S01RRNLast
012100060302
012200060302     C                   ENDSR
012300060302
