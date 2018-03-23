000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*-----------------*                                            *
000400050428     H*         - calcolo ricavi da c.e.
000401050428     H*-----------------*                                            *
001700050428     Ftnvrced2  CF   E             WORKSTN
001701050428     D ddatiute      e ds
001702050428     D azuteds       e ds                  extname(AZUTE00F)
001703050428     D tibs34ds      E DS                  inz
001704050428     D KPJBA         E DS
001705050428     d PARAM           DS
001706050428     D  PERIODOCE                     6s 0
001707050428     D  dettaglio                     1
002800050428     D data            DS
002801050428     d mm                             2  0
002802050428     d aaaa                           4  0
002803050428     D WLBDA8          DS
002900941230     D  G02DAT                 1      8  0
003000941230     D  G02INV                 9     16  0
003100941230     D  G02ERR                17     17
003200941230     D  G02TGI                18     22  0
003201050428     c*
004811050428     c                   do        *hival
006400050428     C                   EXFMT     CE02D01
006500050428     C** FINE LAVORO
006600050428     C   KC              leave
008400050428     C** CONTROLLO LA DATA IMMESSA
008401050428     c                   exsr      srctr
008402050428     c   90              iter
008403050428     c                   if        *inkf
008404050428     C                   exsr      srlancio
008405050428     c                   leave
008406050428     c                   end
008500911127     C*
008501050428     c                   enddo
008502050428     c                   seton                                        lr
008503050428     C*****************************************************************
008504050428     c     srctr         begsr
008505050428     C*****************************************************************
008506050428     C                   SETOFF                                       90
008507050428     c                   clear                   WLBDA8
008508050428     c                   if        viddtd = 0
008509050428     C                   SETON                                        1190
008510050428     c                   else
008511050428     c                   movel     viddtd        aaaa
008512050428     c                   move      viddtd        mm
008513050428     c                   move      data          g02dat
008514050428     c                   movel     01            g02dat
008700911127     C                   MOVEL     *BLANK        G02ERR
008800950102     C                   CALL      'XSRDA8'
008900950102     C                   PARM                    WLBDA8
009000911127     C     G02ERR        IFEQ      '1'
009100050428     C                   SETON                                        1190
009300911127     C                   END
009301050428     C                   END
021800920909     C*
021801050428     C                   ENDSR
021900050428     C*****************************************************************
022000050428     c     srlancio      begsr
022100050428     C*****************************************************************
022101050428     C*
022102050428     c                   eval      periodoce = viddtd
022103050428     c                   eval      dettaglio = viddet
022104050428     c                   eval      kpjbu = param
022105050428     C                   MOVEL     'VRC1'        KCOAZ
022110050428     C                   CALL      'BCH10'
022111050428     C                   PARM                    KPJBA
022113050428     C*
022114050428     C                   ENDSR
022115050428     C*****************************************************************
022116050428     c     *inzsr        begsr
022117050428     C*****************************************************************
022200050428     C     *ENTRY        PLIST
022300050428     C                   PARM                    KPJBA
022400050428     C*
022500050428     c     *dtaara       define    §azute        azuteds
022600050428     c     *dtaara       define    §datiute      ddatiute
022700050428     C                   in(E)     *dtaara
022800050428     C                   IF        %Error  or  RSUT = *blanks
022900050428     C                   call      'TIBS34R'
023000050428     C                   parm                    Tibs34Ds
023100050428     C                   in        *dtaara
023200050428     c                   ENDIF
023201050428     c                   eval      viddet = 'N'
023202050428     c                   eval      vidpgm = 'TNVRCER2'
023300050428     c                   endsr
