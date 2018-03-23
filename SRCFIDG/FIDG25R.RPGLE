000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000808     H*         - STAMPA RIEPILOGO CONTEGGI PADRONCINI
000300000000     H*--------------------------------------------------------------*
000400070213     Ffidg25d   cf   e             workstn
000500070618     Fazorg01L  IF   E           K DISK
000600070830     Ffiprm01l  IF   E           K DISK
000601160428     Ffidst01l  IF   E           K DISK
000700930707     F*
000800070213     D kpjba         E DS
000900070618     D OG146         E DS                  INZ
001000070829     D OG147         E DS                  INZ
001100070618     D og150         E DS
001101160428     d ddstflr       e ds
001102100504     d tibs02ds      e ds
001103100504     d dvpodeco      e ds
001200950125     D*
001300070213     D wlbdat          DS
001400070213     D  g02dat                 1      8  0
001500070213     D  g02inv                 9     16  0
001600070213     D  g02err                17     17
001700070213     D  g02tgi                18     22  0
001800030723     D tibs36DS      E DS
001900070213     D fidg26ds      E DS
002000070830     D fidg17ds      e ds
002001150513     D ddatiute      e ds
002002150513     D azuteds       e ds                  extname(AZUTE00F)
002003150513     D tibs34ds      E DS                  inz
002004150513     d TRULVPODS     e ds
002005150513     d   sk_PDA               16    765    inz   dim(250)
002100070213     D trul06ds      E DS
002200070213     D  lin                    1     90  0 dim(30)
002500061109     d dataiso         s               d   datfmt(*iso)
002600070510     d dataeur         s               d   datfmt(*eur)
002700070213     d  fil£1          s              3  0 dim(30)
002800070213     d  kpjbus         s                   like(kpjbu)
002900070213     I
003000070213      *___________________________________________________
003100160428     c     kdst          klist
003200160428     c                   kfld                    DSTNPG
003300160428     c                   kfld                    DSTNfv
003301160428     c                   kfld                    vfil
003302160428     c                   movel     '4'           dstnpg
003303160428     c     kprm          klist
003304160428     c                   kfld                    vfil
003305160428     c                   kfld                    pgm              10
003400070830     c                   movel     'FIDG17R'     pgm
003500070830      *
003600070213     C     *entry        plist
003700070213     C                   parm                    kpjba
004201150513     c     *dtaara       define    §azute        azuteds
004202150513     c     *dtaara       define    §datiute      ddatiute
004203150513     C                   in(E)     *dtaara
004204150513     C                   IF        %Error  or  RSUT = *blanks
004205150513     C                   call      'TIBS34R'
004206150513     C                   parm                    Tibs34Ds
004207150513     C                   in        *dtaara
004208150513     c                   ENDIF
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
006501100504      * controllo se attivare o no particolarità
006502100504     c                   clear                   tibs02ds
006503100504     c                   clear                   dvpodeco
006504100504     c                   eval      t02mod = 'C'
006505100504     c                   eval      t02sif = knsif
006506100504     c                   eval      t02cod = 'VPO'
006507100504     c                   eval      t02ke1 = 'DECO'
006508100504     c                   call      'TIBS02R'
006509100504     c                   parm                    kpjba
006510100504     c                   parm                    tibs02ds
006511100504    2c                   if        t02err = *blanks
006512100504     c                   eval      dvpodeco  = t02uni
006513100504    2c                   endif
006514100505     c                   if        wdymd < §vpogma
006515100504     c                   seton                                        11
006516100504     c                   endif
006600070213
006700931116     C*---------------------------------------------------------------*
006800950411     C*
006900070213     C     inizio        tag
007000070213     C                   movel     o36pou        Vfil
007100070510     C                   movel     wdat          vdata
007101100504     C  n11              move      'N'           Vatte
007102100504     C   11              move      'S'           Vatte
007200070213     C                   clear                   Vdist
007300070329     C                   move      'S'           Vspea
007400070329     C                   move      'S'           Vnoas
007407160429     C                   eval      vturno = 'N'
007408150513     C                   clear                   Vp88
007409150513     c* ricerca P.O. abilitati da tabella VPO per step di test
007410150513     c                   clear                   trulvpods
007411150513     c                   eval      ivpoke1 = 'SQA'
007412150513     c                   call      'TRULVPOR'
007413150513     c                   parm                    trulvpods
007414150513     c                   move      '999'         fgsa              3
007415150513     c     fgsa          lookup    sk_pda                                 37
007416150513     c  n37              move      dutpou        fgsa
007417150513     c  n37fgsa          lookup    sk_pda                                 37
007418150513     c                   if        *in37
007419150513     c                   eval      vp88 = 'S'
007420150513     c                   else
007421150513     C                   CLEAR                   Vp88
007422150513     c                   end
007500070618     C                   clear                   Vtipo
007600070618     c                   exsr      srtpm
007601160506     C                   move      'U'           Vtur1
007602160506     C                   clear                   Vtur2
007603160506     c                   if        *in12
007604160506     C                   move      '1'           Vtur1
007605160506     C                   move      '2'           Vtur2
007606160506     c                   end
007700070829      *
007800070213     C     for01         tag
007900070213     C                   exfmt     dg25D01
008000070213     c                   setoff                                       99
008100931116     C*
008200920323     C** CMD3 - FINE LAVORO
008300070213     C   kc              goto      fine
008400931015     C* CONTROLLI FORMATO1
008500070213     C                   exsr      contr
008502160506     C  nkf
008800160506     Cor 28              goto      for01
008900931116     C*
009000070213     c                   clear                   fidg26ds
009100070213     c                   z-add     vfil          d26fil
009200070829     c                   z-add     vdatag        d26dat
009300070213     c                   z-add     vdist         d26dis
009400070329     C                   move      vspea         d26spea
009500070329     C                   move      vnoas         d26noas
009600070618     C                   move      vtipo         d26pame
009601160428     C                   move      vtur1         d26tur1
009602160428     C                   move      vtur2         d26tur2
009603160502     c                   movel     vturno        d26sptur
009700070830     C                   move      vtipou        d26fppi
009800070830     c                   z-add     vdatalg       d26dtai
009900070830     c                   z-add     voral         d26orai
009901150430     c                   movel     vp88          d26p88i
010000071210     c                   z-add     vdcrlg        d26dtdcr
010001100208     C                   move      vatte         d26atte
010100070213     C                   movel     fidg26ds      kpjbu
010200931202     C*
010300070213     C                   call      'FIDG26R'
010400011115     C                   parm                    Kpjba
010500070213     c                   seton                                        99
010600900611     C*
010700070213     C                   goto      inizio
010800070213     C     fine          tag
010900070213     C                   seton                                        LR
011000931111     C*
011100070213      *___________________________________________________
011200070213     C     contr         begsr
011300070213      *___________________________________________________
011400070830     C                   setoff                                       909193
011500160502     C                   setoff                                       950713
011501160506     C                   setoff                                       1428
011600070213      * profilo non edp verifico £1
011700070213     c                   if        %subst(knmus:1:3) <> 'EDP'
011800070213     c     vfil          lookup    fil£1                                  92
011900160506     c  n92              seton                                        9028
012000070213     c                   endif
012100931126     C*
012200070829     C* D A T A  distinta
012300070213     C                   move      Vdata         G02DAT
012400070213     C                   movel     *blank        G02ERR
012500070213     C                   call      'XSRDA8'
012600070213     C                   parm                    WLBDAT
012700070213    1C                   if        g02err = '1'
012800160506     C                   seton                                        9128
012900070213    1C                   end
013000070213     C                   move      G02DAT        vdata
013100070829     C                   move      G02inv        vdatag            8 0
013200070829      *parcel/messaggerie/abbinamenti/ mat-pom
013300070829     c                   exsr      srtpm
013301160502     c* per messaggerie non immettere il turno
013302160505     c                   if        *in12
013303160505     c                   if        vtipo = 'M'
013304160502     c                   if        (vtur1 = 'U' or vtur2 = 'U' )
013305160502     c                   else
013306160506     c                   seton                                        1328
013307160502     c                   end
013310160502     c                   end
013311160505     c                   if        vtur1 = ' ' and vtur2 = ' '
013312160505     c                             and (vtipo= 'P' or vtipo= ' ')
013313160506     c                   seton                                        1328
013314160505     c                   end
013315160505     c                   end
013400070829     C* D A T A  abbinamenti
013500070829      * o8*on data e ora abbinamenti da gestire
013600070830     c                   if        *in08 and vdatal > 0
013700070829     C                   move      Vdatal        G02DAT
013800070829     C                   movel     *blank        G02ERR
013900070829     C                   call      'XSRDA8'
014000070829     C                   parm                    WLBDAT
014100070829    1C                   if        g02err = '1'
014200160506     C                   seton                                        9328
014300070829    1C                   end
014400070829     C                   move      G02DAT        vdatal
014500070829     C                   move      G02inv        vdatalg           8 0
014600071210      * o8*on data e ora abbinamenti da pulire
014700070829     c                   else
014800070829     C                   clear                   vdatal
014900070829     C                   clear                   vdatalg
015000070829     C                   clear                   voral
015100070829     c                   endif
015200070830     c                   if        vdatalg > vdatag
015300160506     C                   seton                                        9328
015400070830     c                   endif
015500070213      *
015600071210      * data limite consegna richiesta prima del
015700071210     c                   if        vdcrl  > 0
015800071210     C                   move      Vdcrl         G02DAT
015900071210     C                   movel     *blank        G02ERR
016000071210     C                   call      'XSRDA8'
016100071210     C                   parm                    WLBDAT
016200071210    1C                   if        g02err = '1'
016300160506     C                   seton                                        9528
016400071210    1C                   end
016500071210     C                   move      G02DAT        vdcrl
016600071210     C                   move      G02inv        vdcrlg            8 0
016601100225     c                   if        vdcrlg <= vdatag
016602160506     C                   seton                                        9528
016603100225     c                   endif
016700071210     c                   else
016800071210      * data limite consegna richiesta prima del
016900071210     C                   clear                   vdcrlg
017000071210     c                   endif
017001160428     c*
017002160428     c                   clear                   ddstflr
017003160428     c                   if        vdist <> 0
017004160428     c                   move      vdist         dstnfv
017005160428     c     kdst          chain     fidst01l
017006160428     c                   if        %found(fidst01l)
017007160428     c                   movel     dstflr        ddstflr
017008160428     c                   else
017009160506     C                   seton                                        0728
017010160428     c                   end
017011160502     c* tipo incongruente
017012160502     c                   if        vtipo = 'P' and dsttpm <> ' ' or
017013160502     c                             vtipo = 'M' and dsttpm <> 'M'
017014160506     c                   seton                                        1428
017015160502     c                   end
017017160502     c* turno incongruente con la distinta
017028160502     c                   if        (vtur1 = ' ' or vtur2 = ' ')
017029160502     c                             and (vtur1 <>'U' and vtur2 <>'U')
017030160502     c                             and *in12
017031160502     c                   select
017032160502     c                   when      (vtur1 = '1' or
017033160502     c                              vtur2 = '1') and
017034160502     c                             §dstturno <> ' '
017035160506     C                   SETON                                        1328
017036160502     c                   when      (vtur1 = '2' or
017037160502     c                              vtur2 = '2') and
017038160502     c                             §dstturno <> '2'
017039160506     C                   SETON                                        1328
017040160502     c                   endsl
017047160502     c                   end
017048160502     c                   end
017400070213    1C                   endsr
017500070618     C*--------------------------------------------------------------*
017600070618     C*  parcel + messaggerie
017700070618     C*--------------------------------------------------------------*
017800070618     C     srtpm         BEGSR
017900070618     c* leggo organigramma x gestire ind 10 parcel+messaggerie
018000070829     c* leggo organigramma x gestire ind 09 mattina/pomeriggio
018100070829     c* leggo organigramma x gestire ind 08 data/ora limite abbinamento
018200070618     c     vfil          chain     azorg01l
018300070618     c                   if        %found(azorg01l)
018400070618     c                   movel     orgdf0        og150
018500070618     C                   MOVEL     ORGDE6        og146
018600070829     C                   MOVEL     ORGDE7        og147
018700070618     c     §OGPKM        comp      'S'                                    10
018800070829     c     §OGFPPDA      comp      'S'                                    09
018900070829     c     §OGDOADA      comp      'S'                                    08
018901160428     c     §OGturno      comp      'S'                                    12
019000070618     c                   end
019100070829      * reperisce parametri ultimo lancio assegnazione automatica
019200070829     c                   if        vfil <> exfil
019300070829     c                   exsr      srprm
019400070829     c                   z-add     vfil          exfil             3 0
019500070829     c                   endif
019600070618     c* se p.o. non partito esco
019700070618     c                   if        §ogdda = ' '
019800070618     c                   seton                                        lr
019900070618     c                   return
020000070618     c                   end
020100070618     C                   ENDSR
020200070829     C*--------------------------------------------------------------*
020300070829     C*  parametri ultimo lancio assegnazione automatica
020400070829     C*--------------------------------------------------------------*
020500070829     C     srprm         BEGSR
020600070829     c                   clear                   fidg17ds
020700070829     c     kprm          chain     fiprm01l
020800070829     c                   if        %found(fiprm01l)
020900070829     c                   movel     prmpar        fidg17ds
021000070830     c                   movel     prmdta        dataiso
021100070830     c                   movel     dataiso       dataeur
021200070830     c                   movel     dataeur       vdataul
021300070829     c                   movel     prmora        voraul
021400070829     c                   end
021500070830     c                   if        d17dtai > 0
021600070830     c                   movel     d17dtai       dataiso
021700070830     c                   movel     dataiso       dataeur
021800070830     c                   movel     dataeur       vdatal
021900070830     c                   endif
022000070919     c                   if        d17dfvi > 0
022100070919     c                   move      d17dfvi       dataiso
022200070919     c                   movel     dataiso       dataeur
022300070919     c                   movel     dataeur       vdata
022400070919     c                   endif
022500071210     c                   if        D17ldcri > 0
022600071210     c                   movel     d17ldcri      dataiso
022700071210     c                   movel     dataiso       dataeur
022800071210     c                   movel     dataeur       vdcrl
022900071210     c                   endif
023000070829     c                   z-add     d17orai       voral
023100070829     c                   move      d17fppi       vtipou
023102160428     c                   movel     d17tur1       vtur1
023103160428     c                   movel     d17tur2       vtur2
023104150513     c                   movel     d17p88i       vp88
023200070829     C                   ENDSR
