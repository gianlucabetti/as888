000200120320     H DEBUG DECEDIT('0,') DATEDIT(*YMD/) option(*nodebugio)
000201120314      *
000300120314     faitrs01l  uf a e           k disk
000700050118
000800050118      *ds comodo per tracciati da trascodificare
000900120314     d kpjba         e ds
000901120314     d trul33ds      e ds
000902120314     d dtrsflr       e ds
000903120314     d old_nrc         s              7s 0
000904120314     d new_nrc         s              7s 0
001200050114
001900000607      *--------------------------------------------------------------------
002000050114
002100120314      * reperimento dati file
002200050114      *
002300120314     c     *loval        setll     aitrs01l
002400000607     c                   do        *hival
002500120314     c                   read      aitrs01l
002600120314     c                   if        %eof(aitrs01l)
002700000607     c                   leave
002800000607     c                   end
002801120314     c                   movel     trsflr        dtrsflr
002802120314     c                   move      trsflr        comodo2           2
002803120314      * se impostate le date nel filler già modo nuovo quindi non si riemette
002804120314      * se già scaduto non si riemette
002805120314      * se mai stampato non si riemette
002806120314      * se annullato non si riemette
002807120314     c                   if        §trsunodc <> *blank or
002808120314     c                             trsdfc > 0          or
002809120314     c                             trsdsc = 0          or
002810120314     c                             trsann <> *blank
002811120314     c                   iter
002812120314     c                   end
002813120314      *aggiorna vecchio record  facendolo scadere
003000120315     c                   z-add     20120331      trsdfc
003001120320     c                   move      *date         trsdva
003002120320     c                   eval      trsute  = 'EDPCED'
003100120314     c                   update    aitrs000
003101120314      *prendo vecchio numero per disaccreditare autisti legati al contratto
003102120314     c                   z-add     trsnrc        old_nrc
003200050118
003201120314      * scrivo il rekord nuovo
003300120314     c                   clear                   trsdfc
003400120314     c                   clear                   trsdrc
003401120314     c                   clear                   trsdsc
003402120314     c                   clear                   trsflr
003403120320     c                   clear                   trsdva
003404120316     c                   z-add     20120401      trsdec
003405120314     c                   move      comodo2       trsflr
003406120320     c                   move      *date         trsdim
003407120320     c                   eval      trsute  = 'EDPCED'
003408120314      * reperimento nuovo numero contratto
003500120314     c                   exsr      repnum
003501120314     c                   z-add     new_nrc       trsnrc
003700120314     c                   write     aitrs000
003701120314      * richiamo pgm per disaccreditare autisti con vecchio contratto e
003702120314      * accreditamento con nuovo contratto
003703120314     c                   call      'TRULAUTR'
003704120314     c                   parm                    old_nrc
003705120314     c                   parm                    new_nrc
003800040507     c                   enddo
003900120314     c                   seton                                        lr
021701120314     c*-----------------------------------------------------
021702120314     c* reperimento numero contratto
021703120314     c*-----------------------------------------------------
021704120314     C     repnum        BEGSR
021705120314     c                   clear                   new_nrc
021706120314     c                   clear                   trul33ds
021707120314     c                   move      'L'           i33tla
021708120314     c                   z-add     32            i33cnu
021709120314     c                   z-add     1             i33num
021710120314     c                   movel     trul33ds      kpjbu
021711120314     c                   call      'TRUL33R'
021712120314     c                   PARM                    kpjba
021713120314     c                   movel     kpjbu         trul33ds
021714120314     c                   if        o33err <> *zeros
021715120314     c                   dump
021716120314     c                   else
021717120314     c                   z-add     o33nrf        new_NRC
021718120314     c                   endif
021719120314      *
021720120314     C                   endsr
021800050114      *-------------------------------------------------
021900050114     c     *inzsr        begsr
022000050114      *-------------------------------------------------
022100050114     c     *entry        plist
022200050114     c                   parm                    kpjba
022300050117
022900050114     c                   endsr
