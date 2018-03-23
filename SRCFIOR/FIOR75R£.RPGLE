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
002700071010     d Win11           s              1    inz(*off)
002800071010     d Win12           s              1    inz(*off)
002900070213      *___________________________________________________
003000070830      *
003100070213     C     *entry        plist
003200070213     C                   parm                    kpjba
003300070213     C                   z-add     1             codut
003400070213     C                   call      'X§PARUT'
003500070213     C                   parm                    ut§dse
003600070213     C                   movel     ragut         rsut             20
003700070213     C                   movel     rec80         cncr80
003800070213      *
003900070213     C                   CLEAR                   Tibs36Ds
004000070213     C                   EVAL      I36ute = knmus
004100070213     C                   CALL      'TIBS36R'
004200070213     C                   PARM                    tibs36ds
004300070213      * carico filiali £1
004400070213     c                   eval      kpjbus = kpjbu
004500070213     C                   clear                   trul06ds
004600070213     C                   movel     '£1'          d06cod
004700070411     C                   movel     o36pou        d06key
004800070213     C                   movel     trul06ds      kpjbu
004900070213     C                   CALL      'TRUL06R'
005000070213     C                   parm                    kpjba
005100070213     C                   movel     kpjbu         trul06ds
005200070213     C                   movea     lin           fil£1
005300070213     c                   eval      kpjbu = kpjbus
005400070510      *
005500070510     C                   time                    w0120            14 0
005600070510     C                   move      w0120         wdat              8 0
005700070510     C                   movel     w0120         wora              6 0
005800070510     c                   move      wdat          dataeur
005900070510     c                   move      dataeur       dataiso
006000070510     c                   move      dataiso       wdymd             8 0
006100070213
006200931116     C*---------------------------------------------------------------*
006300950411     C*
006400070213     C     inizio        tag
006500070213     C                   movel     o36pou        Vfil
006600070510     C                   movel     wdat          vdata
006700070213     C                   clear                   Vdist
006800070907     C                   move      'S'           VORMa
006900070329     C                   move      'S'           Vnoas
007000071011     c                   eval      Vdormn  = *blanks
007100071011     c                   eval      Vormall = 'E'
007200070618     C                   clear                   Vtipo
007300070618     c                   exsr      srtpm
007400070829      *
007500070213     C     for01         tag
007600070906     C                   exfmt     or75D01
007700070213     c                   setoff                                       99
007800931116     C*
007900920323     C** CMD3 - FINE LAVORO
008000070213     C   kc              goto      fine
008100931015     C* CONTROLLI FORMATO1
008200070213     C                   exsr      contr
008300070213     C  nkf
008400070213     Cor 90
008500070213     Cor 91              goto      for01
008600931116     C*
008700070906     c                   clear                   fior76ds
008800070906     c                   z-add     vfil          or76fil
008900070906     c                   z-add     vdatag        or76dat
009000070906     c                   z-add     vdist         or76dis
009100070907     C                   move      vorma         or76orma
009200070906     C                   move      vnoas         or76noas
009300070906     C                   move      vtipo         or76pame
009400071010     c                   if        *in11
009500071010     c                   eval      OR76detta  = Vdormn
009600071010     c                   if        *in12
009700071010     c                   eval      OR76allega = Vormall
009800071010     c                   endif
009900071010     c                   endif
010000070906     C                   movel     fior76ds      kpjbu
010100931202     C*
010200070906     C                   call      'FIOR76R'
010300011115     C                   parm                    Kpjba
010400070213     c                   seton                                        99
010500900611     C*
010600070213     C                   goto      inizio
010700070213     C     fine          tag
010800070213     C                   seton                                        LR
010900931111     C*
011000070213      *___________________________________________________
011100070213     C     contr         begsr
011200070213      *___________________________________________________
011300070830     C                   setoff                                       909193
011400070213      * profilo non edp verifico £1
011500070213     c                   if        %subst(knmus:1:3) <> 'EDP'
011600070213     c     vfil          lookup    fil£1                                  92
011700070213     c  n92              seton                                        90
011800070213     c                   endif
011900931126     C*
012000070829     C* D A T A  distinta
012100070213     C                   move      Vdata         G02DAT
012200070213     C                   movel     *blank        G02ERR
012300070213     C                   call      'XSRDA8'
012400070213     C                   parm                    WLBDAT
012500070213    1C                   if        g02err = '1'
012600070213     C                   seton                                        91
012700070213    1C                   end
012800070213     C                   move      G02DAT        vdata
012900070829     C                   move      G02inv        vdatag            8 0
013000070829      *parcel/messaggerie/abbinamenti/ mat-pom
013100070829     c                   exsr      srtpm
013200071010      *
013300071010     c                   eval      Win11 = *in11
013400071010     c                   eval      Win12 = *in12
013500071010      * Abilitazione richiesta "Stampa dettaglio ORM da assegnare"
013600071010     c                   eval      *in11 = (VNOAS = 'S')
013700071010      * Abilitazione richiesta "ORM con allegati"
013800071010     c                   eval      *in12 = (*in11 and VDORMN = 'S')
013900071010      *
014000071010      * Se visualizzati/richiesti nuovi dati (in più o in meno)
014100071010      * ->  rivisualizzo la videata (anche se premuto F6)
014200071010     c                   if             *in11 <> Win11
014300071010     c                             or   *in12 <> Win12
014400071010     c                   if        *in12
014500071011     c                   eval      Vormall = 'E'
014600071010     c                   endif
014700071010     c                   eval      *inKF = *off
014800071010     c                   endif
014900070213      *
015000070213    1C                   endsr
015100070618     C*--------------------------------------------------------------*
015200070618     C*  parcel + messaggerie
015300070618     C*--------------------------------------------------------------*
015400070618     C     srtpm         BEGSR
015500070618     c* leggo organigramma x gestire ind 10 parcel+messaggerie
015600070829     c* leggo organigramma x gestire ind 09 mattina/pomeriggio
015700070618     c     vfil          chain     azorg01l
015800070618     c                   if        %found(azorg01l)
015900070618     c                   movel     orgdf0        og150
016000070906     c                   movel     orgde7        og147
016100070618     c     §OGPKM        comp      'S'                                    10
016200070926     c     §OGFRTUO      comp      'S'                                    09
016300070618     c                   end
016400070618     c* se p.o. non partito esco
016500070906     c                   if        §ogddao = ' '
016600070618     c                   seton                                        lr
016700070618     c                   return
016800070618     c                   end
016900070618     C                   ENDSR
