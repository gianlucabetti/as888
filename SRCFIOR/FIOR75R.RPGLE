000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300070906     Ffior75d   cf   e             workstn
000400070618     Fazorg01L  IF   E           K DISK
000500930707     F*
000600070213     D kpjba         E DS
000700070829     D OG147         E DS                  INZ
000800070618     D og150         E DS
000900950125     D*
001000070213     D wlbdat          DS
001100070213     D  g02dat                 1      8  0
001200070213     D  g02inv                 9     16  0
001300070213     D  g02err                17     17
001400070213     D  g02tgi                18     22  0
001500030723     D tibs36DS      E DS
001600070906     D fior76ds      E DS
001700070906     D fior62ds      e ds
001800070213     D trul06ds      E DS
001900070213     D  lin                    1     90  0 dim(30)
002000070213     D cncr80        E DS
002100070213     D ut§dse        E DS                  extname(UT§DSE0F)
002200061109     d dataiso         s               d   datfmt(*iso)
002300070510     d dataeur         s               d   datfmt(*eur)
002400070213     d  fil£1          s              3  0 dim(30)
002500070213     d  kpjbus         s                   like(kpjbu)
002600071010      *
002700070213      *___________________________________________________
002800070830      *
002900070213     C     *entry        plist
003000070213     C                   parm                    kpjba
003100070213     C                   z-add     1             codut
003200070213     C                   call      'X§PARUT'
003300070213     C                   parm                    ut§dse
003400070213     C                   movel     ragut         rsut             20
003500070213     C                   movel     rec80         cncr80
003600070213      *
003700070213     C                   CLEAR                   Tibs36Ds
003800070213     C                   EVAL      I36ute = knmus
003900070213     C                   CALL      'TIBS36R'
004000070213     C                   PARM                    tibs36ds
004100070213      * carico filiali £1
004200070213     c                   eval      kpjbus = kpjbu
004300070213     C                   clear                   trul06ds
004400070213     C                   movel     '£1'          d06cod
004500070411     C                   movel     o36pou        d06key
004600070213     C                   movel     trul06ds      kpjbu
004700070213     C                   CALL      'TRUL06R'
004800070213     C                   parm                    kpjba
004900070213     C                   movel     kpjbu         trul06ds
005000070213     C                   movea     lin           fil£1
005100070213     c                   eval      kpjbu = kpjbus
005200070510      *
005300070510     C                   time                    w0120            14 0
005400070510     C                   move      w0120         wdat              8 0
005500070510     C                   movel     w0120         wora              6 0
005600070510     c                   move      wdat          dataeur
005700070510     c                   move      dataeur       dataiso
005800070510     c                   move      dataiso       wdymd             8 0
005900070213
006000931116     C*---------------------------------------------------------------*
006100950411     C*
006200070213     C     inizio        tag
006300070213     C                   movel     o36pou        Vfil
006400070510     C                   movel     wdat          vdata
006500070213     C                   clear                   Vdist
006600070907     C                   move      'S'           VORMa
006700070329     C                   move      'S'           Vnoas
006800071012     c                   eval      Vdormn  = 'N'
006900071012     c                   eval      Vormall = ' '
007000070618     C                   clear                   Vtipo
007100070618     c                   exsr      srtpm
007200070829      *
007300070213     C     for01         tag
007400070906     C                   exfmt     or75D01
007500070213     c                   setoff                                       99
007600931116     C*
007700920323     C** CMD3 - FINE LAVORO
007800070213     C   kc              goto      fine
007900931015     C* CONTROLLI FORMATO1
008000070213     C                   exsr      contr
008100070213     C  nkf
008200070213     Cor 90
008300071012     Cor 94
008400070213     Cor 91              goto      for01
008500931116     C*
008600070906     c                   clear                   fior76ds
008700070906     c                   z-add     vfil          or76fil
008800070906     c                   z-add     vdatag        or76dat
008900070906     c                   z-add     vdist         or76dis
009000070907     C                   move      vorma         or76orma
009100070906     C                   move      vnoas         or76noas
009200070906     C                   move      vtipo         or76pame
009300071010     c                   eval      OR76detta  = Vdormn
009400071010     c                   eval      OR76allega = Vormall
009500070906     C                   movel     fior76ds      kpjbu
009600931202     C*
009700070906     C                   call      'FIOR76R'
009800011115     C                   parm                    Kpjba
009900070213     c                   seton                                        99
010000900611     C*
010100070213     C                   goto      inizio
010200070213     C     fine          tag
010300070213     C                   seton                                        LR
010400931111     C*
010500070213      *___________________________________________________
010600070213     C     contr         begsr
010700070213      *___________________________________________________
010800070830     C                   setoff                                       909193
010900071012     c                   setoff                                       94
011000070213      * profilo non edp verifico £1
011100070213     c                   if        %subst(knmus:1:3) <> 'EDP'
011200070213     c     vfil          lookup    fil£1                                  92
011300070213     c  n92              seton                                        90
011400070213     c                   endif
011500931126     C*
011600070829     C* D A T A  distinta
011700070213     C                   move      Vdata         G02DAT
011800070213     C                   movel     *blank        G02ERR
011900070213     C                   call      'XSRDA8'
012000070213     C                   parm                    WLBDAT
012100070213    1C                   if        g02err = '1'
012200070213     C                   seton                                        91
012300071012     c
012400070213    1C                   end
012500070213     C                   move      G02DAT        vdata
012600070829     C                   move      G02inv        vdatag            8 0
012700070829      *parcel/messaggerie/abbinamenti/ mat-pom
012800070829     c                   exsr      srtpm
012900071012      * se non sono stati richiesti i totali degli orm non assegnati posso richiedere
013000071012      * anche il dettaglio
013100071012     c                   If        vdormn = 'S' and Vnoas <> 'S'
013200071012     c                   seton                                        94
013300071012     c                   endif
013400070213      *
013500070213    1C                   endsr
013600070618     C*--------------------------------------------------------------*
013700070618     C*  parcel + messaggerie
013800070618     C*--------------------------------------------------------------*
013900070618     C     srtpm         BEGSR
014000070618     c* leggo organigramma x gestire ind 10 parcel+messaggerie
014100070829     c* leggo organigramma x gestire ind 09 mattina/pomeriggio
014200070618     c     vfil          chain     azorg01l
014300070618     c                   if        %found(azorg01l)
014400070618     c                   movel     orgdf0        og150
014500070906     c                   movel     orgde7        og147
014600070618     c     §OGPKM        comp      'S'                                    10
014700070926     c     §OGFRTUO      comp      'S'                                    09
014800070618     c                   end
014900070618     c* se p.o. non partito esco
015000070906     c                   if        §ogddao = ' '
015100070618     c                   seton                                        lr
015200070618     c                   return
015300070618     c                   end
015400070618     C                   ENDSR
