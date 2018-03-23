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
002600070213     I
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
006800070618     C                   clear                   Vtipo
006900070618     c                   exsr      srtpm
007000070829      *
007100070213     C     for01         tag
007200070906     C                   exfmt     or75D01
007300070213     c                   setoff                                       99
007400931116     C*
007500920323     C** CMD3 - FINE LAVORO
007600070213     C   kc              goto      fine
007700931015     C* CONTROLLI FORMATO1
007800070213     C                   exsr      contr
007900070213     C  nkf
008000070213     Cor 90
008100070213     Cor 91              goto      for01
008200931116     C*
008300070906     c                   clear                   fior76ds
008400070906     c                   z-add     vfil          or76fil
008500070906     c                   z-add     vdatag        or76dat
008600070906     c                   z-add     vdist         or76dis
008700070907     C                   move      vorma         or76orma
008800070906     C                   move      vnoas         or76noas
008900070906     C                   move      vtipo         or76pame
009000070906     C                   movel     fior76ds      kpjbu
009100931202     C*
009200070906     C                   call      'FIOR76R'
009300011115     C                   parm                    Kpjba
009400070213     c                   seton                                        99
009500900611     C*
009600070213     C                   goto      inizio
009700070213     C     fine          tag
009800070213     C                   seton                                        LR
009900931111     C*
010000070213      *___________________________________________________
010100070213     C     contr         begsr
010200070213      *___________________________________________________
010300070830     C                   setoff                                       909193
010400070213      * profilo non edp verifico £1
010500070213     c                   if        %subst(knmus:1:3) <> 'EDP'
010600070213     c     vfil          lookup    fil£1                                  92
010700070213     c  n92              seton                                        90
010800070213     c                   endif
010900931126     C*
011000070829     C* D A T A  distinta
011100070213     C                   move      Vdata         G02DAT
011200070213     C                   movel     *blank        G02ERR
011300070213     C                   call      'XSRDA8'
011400070213     C                   parm                    WLBDAT
011500070213    1C                   if        g02err = '1'
011600070213     C                   seton                                        91
011700070213    1C                   end
011800070213     C                   move      G02DAT        vdata
011900070829     C                   move      G02inv        vdatag            8 0
012000070829      *parcel/messaggerie/abbinamenti/ mat-pom
012100070829     c                   exsr      srtpm
012200070213      *
012300070213    1C                   endsr
012400070618     C*--------------------------------------------------------------*
012500070618     C*  parcel + messaggerie
012600070618     C*--------------------------------------------------------------*
012700070618     C     srtpm         BEGSR
012800070618     c* leggo organigramma x gestire ind 10 parcel+messaggerie
012900070829     c* leggo organigramma x gestire ind 09 mattina/pomeriggio
013000070618     c     vfil          chain     azorg01l
013100070618     c                   if        %found(azorg01l)
013200070618     c                   movel     orgdf0        og150
013300070906     c                   movel     orgde7        og147
013400070618     c     §OGPKM        comp      'S'                                    10
013401070926     c     §OGFRTUO      comp      'S'                                    09
013500070618     c                   end
013600070618     c* se p.o. non partito esco
013700070906     c                   if        §ogddao = ' '
013800070618     c                   seton                                        lr
013900070618     c                   return
014000070618     c                   end
014100070618     C                   ENDSR
