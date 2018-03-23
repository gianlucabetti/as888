000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000808     H*         - STAMPA RIEPILOGO CONTEGGI PADRONCINI
000300000000     H*--------------------------------------------------------------*
000400070213     Ffidg25d   cf   e             workstn
000500070618     Fazorg01L  IF   E           K DISK
000600070830     Ffiprm01l  IF   E           K DISK
000700930707     F*
000800070213     D kpjba         E DS
000900070618     D OG146         E DS                  INZ
001000070829     D OG147         E DS                  INZ
001100070618     D og150         E DS
001200950125     D*
001300070213     D wlbdat          DS
001400070213     D  g02dat                 1      8  0
001500070213     D  g02inv                 9     16  0
001600070213     D  g02err                17     17
001700070213     D  g02tgi                18     22  0
001800030723     D tibs36DS      E DS
001900070213     D fidg26ds      E DS
002000070830     D fidg17ds      e ds
002100070213     D trul06ds      E DS
002200070213     D  lin                    1     90  0 dim(30)
002300070213     D cncr80        E DS
002400070213     D ut§dse        E DS                  extname(UT§DSE0F)
002500061109     d dataiso         s               d   datfmt(*iso)
002600070510     d dataeur         s               d   datfmt(*eur)
002700070213     d  fil£1          s              3  0 dim(30)
002800070213     d  kpjbus         s                   like(kpjbu)
002900070213     I
003000070213      *___________________________________________________
003100070830     c     kprm          klist
003200070830     c                   kfld                    vfil
003300070830     c                   kfld                    pgm              10
003400070830     c                   movel     'FIDG17R'     pgm
003500070830      *
003600070213     C     *entry        plist
003700070213     C                   parm                    kpjba
003800070213     C                   z-add     1             codut
003900070213     C                   call      'X§PARUT'
004000070213     C                   parm                    ut§dse
004100070213     C                   movel     ragut         rsut             20
004200070213     C                   movel     rec80         cncr80
004300070213      *
004400070213     C                   CLEAR                   Tibs36Ds
004500070213     C                   EVAL      I36ute = knmus
004600070213     C                   CALL      'TIBS36R'
004700070213     C                   PARM                    tibs36ds
004800070213      * carico filiali £1
004900070213     c                   eval      kpjbus = kpjbu
005000070213     C                   clear                   trul06ds
005100070213     C                   movel     '£1'          d06cod
005200070411     C                   movel     o36pou        d06key
005300070213     C                   movel     trul06ds      kpjbu
005400070213     C                   CALL      'TRUL06R'
005500070213     C                   parm                    kpjba
005600070213     C                   movel     kpjbu         trul06ds
005700070213     C                   movea     lin           fil£1
005800070213     c                   eval      kpjbu = kpjbus
005900070510      *
006000070510     C                   time                    w0120            14 0
006100070510     C                   move      w0120         wdat              8 0
006200070510     C                   movel     w0120         wora              6 0
006300070510     c                   move      wdat          dataeur
006400070510     c                   move      dataeur       dataiso
006500070510     c                   move      dataiso       wdymd             8 0
006600070213
006700931116     C*---------------------------------------------------------------*
006800950411     C*
006900070213     C     inizio        tag
007000070213     C                   movel     o36pou        Vfil
007100070510     C                   movel     wdat          vdata
007200070213     C                   clear                   Vdist
007300070329     C                   move      'S'           Vspea
007400070329     C                   move      'S'           Vnoas
007500070618     C                   clear                   Vtipo
007600070618     c                   exsr      srtpm
007700070829      *
007800070213     C     for01         tag
007900070213     C                   exfmt     dg25D01
008000070213     c                   setoff                                       99
008100931116     C*
008200920323     C** CMD3 - FINE LAVORO
008300070213     C   kc              goto      fine
008400931015     C* CONTROLLI FORMATO1
008500070213     C                   exsr      contr
008600070213     C  nkf
008700070213     Cor 90
008800070213     Cor 91              goto      for01
008900931116     C*
009000070213     c                   clear                   fidg26ds
009100070213     c                   z-add     vfil          d26fil
009200070829     c                   z-add     vdatag        d26dat
009300070213     c                   z-add     vdist         d26dis
009400070329     C                   move      vspea         d26spea
009500070329     C                   move      vnoas         d26noas
009600070618     C                   move      vtipo         d26pame
009700070830     C                   move      vtipou        d26fppi
009800070830     c                   z-add     vdatalg       d26dtai
009900070830     c                   z-add     voral         d26orai
010000070213     C                   movel     fidg26ds      kpjbu
010100931202     C*
010200070213     C                   call      'FIDG26R'
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
013200070829     C* D A T A  abbinamenti
013300070829      * o8*on data e ora abbinamenti da gestire
013400070830     c                   if        *in08 and vdatal > 0
013500070829     C                   move      Vdatal        G02DAT
013600070829     C                   movel     *blank        G02ERR
013700070829     C                   call      'XSRDA8'
013800070829     C                   parm                    WLBDAT
013900070829    1C                   if        g02err = '1'
014000070830     C                   seton                                        93
014100070829    1C                   end
014200070829     C                   move      G02DAT        vdatal
014300070829     C                   move      G02inv        vdatalg           8 0
014400070829     c                   else
014500070829      * o8*off data e ora abbinamenti da pulire
014600070829     C                   clear                   vdatal
014700070829     C                   clear                   vdatalg
014800070829     C                   clear                   voral
014900070829     c                   endif
015000070830     c                   if        vdatalg > vdatag
015100070830     C                   seton                                        93
015200070830     c                   endif
015300070213      *
015400070213    1C                   endsr
015500070618     C*--------------------------------------------------------------*
015600070618     C*  parcel + messaggerie
015700070618     C*--------------------------------------------------------------*
015800070618     C     srtpm         BEGSR
015900070618     c* leggo organigramma x gestire ind 10 parcel+messaggerie
016000070829     c* leggo organigramma x gestire ind 09 mattina/pomeriggio
016100070829     c* leggo organigramma x gestire ind 08 data/ora limite abbinamento
016200070618     c     vfil          chain     azorg01l
016300070618     c                   if        %found(azorg01l)
016400070618     c                   movel     orgdf0        og150
016500070618     C                   MOVEL     ORGDE6        og146
016600070829     C                   MOVEL     ORGDE7        og147
016700070618     c     §OGPKM        comp      'S'                                    10
016800070829     c     §OGFPPDA      comp      'S'                                    09
016900070829     c     §OGDOADA      comp      'S'                                    08
017000070618     c                   end
017100070829      * reperisce parametri ultimo lancio assegnazione automatica
017200070829     c                   if        vfil <> exfil
017300070829     c                   exsr      srprm
017400070829     c                   z-add     vfil          exfil             3 0
017500070829     c                   endif
017600070618     c* se p.o. non partito esco
017700070618     c                   if        §ogdda = ' '
017800070618     c                   seton                                        lr
017900070618     c                   return
018000070618     c                   end
018100070618     C                   ENDSR
018200070829     C*--------------------------------------------------------------*
018300070829     C*  parametri ultimo lancio assegnazione automatica
018400070829     C*--------------------------------------------------------------*
018500070829     C     srprm         BEGSR
018600070829     c                   clear                   fidg17ds
018700070829     c     kprm          chain     fiprm01l
018800070829     c                   if        %found(fiprm01l)
018900070829     c                   movel     prmpar        fidg17ds
019000070830     c                   movel     prmdta        dataiso
019100070830     c                   movel     dataiso       dataeur
019200070830     c                   movel     dataeur       vdataul
019300070829     c                   movel     prmora        voraul
019400070829     c                   end
019500070830     c                   if        d17dtai > 0
019600070830     c                   movel     d17dtai       dataiso
019700070830     c                   movel     dataiso       dataeur
019800070830     c                   movel     dataeur       vdatal
019900070830     c                   endif
020000070829     c                   z-add     d17orai       voral
020100070829     c                   move      d17fppi       vtipou
020200070829     C                   ENDSR
