000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200001006
000300001006      *****************************************************************
000400080908      *       STATISTICA controllo assegnazione automatica ORM        *
000500001006      *****************************************************************
000600001006
000700001010      ****************************************************************
000800001010      *  RIEPILOGO INDICATORI
000900001010      ****************************************************************
001000010618      * 12 - Sono in sede
001100010705      * 13 - Utente EDP
001200001010      * 28 - ERRORE GENERICO DSPF
001300001010      * 30 - Comodo
001400010606      * 40 - ERRORE p.o. da elaborare
001500010618      * 41 - ERRORE data elaborazione da
001600010618      * 42 - ERRORE data elaborazione al
001700001010      ****************************************************************
001800001010
001900010605     fazorg01l  if   e           k disk
002000010618     ftabel00f  if   e           k disk
002100010618     fazcln01l  if   e           k disk
002200080908     ffior48d   cf   e             workstn
002300001010
002400001010      *------------------------------------------------------------------------*
002500010618
002600010618     d kcod            s                   like(tblcod)
002700010618     d kkey            s                   like(tblkey)
002800010618
002900010618     d ktfp            s                   like(clntfp)
003000010618     d ktfa            s                   like(clntfa)
003100010618     d kann            s                   like(clnann)
003200010618     d kmes            s                   like(clnmes)
003300030321
003400030321     d sav_g02tgi      s                   like(g02tgi)
003500010606
003600010618     d wcdtda          s              8  0
003700010618     d wcdtal          s              8  0
003800001009
003900080908     d wabi            s                   like(UteAut)
004000010625     d w0030           s              3  0
004100010625     d w0140           s             14  0
004200010605     d wdtgio          s              8  0
004300010618     d dataoggi        s              8  0
004400010622     d dtpuliaammgg    s              8  0
004500010622     d dtpuliggmmaa    s              8  0
004600010618     d wgiorno         s              2  0
004700010618     d wgiolav         s              5  0
004800010622     d datamsg         s             10
004900010705
005000010705     d wprofilo        s              3
005100080916     d wdao            s              8  0
005200010618
005300010618     d wdatada         s               d   datfmt(*iso)
005400010618     d wdataal         s               d   datfmt(*iso)
005500001006
005600001031
005700001031      *   S C H I E R E
005800080924     d msg             s             78    dim(13) ctdata perrcd(1)             msg video
005900080908     D $pog            s              3    dim(250)
006000080908     D $DIG            s              1    dim(20)
006100080908     D $ARG            s              3    dim(50)
006200001006
006300001010      *   D S   I N T E R N E / E S T E R N E
006400001009
006500010605     d wlbdat          ds                  inz
006600010605     d  g02dat                 1      8  0
006700010605     d  g02inv                 9     16  0
006800010605     d  g02err                17     17
006900010605     d  g02tgi                18     22  0
007000010618
007100010618     d wgidat          ds                  inz
007200010618     d  giodat                 1      8  0
007300010618     d  gioinv                 9     16  0
007400010618     d  giotgi                17     21  0
007500010618
007600010622     d datapul         ds                  inz
007700010622     d  giorno                 1      2
007800010622     d  mese                   3      4
007900010622     d  anno                   5      8
008000010622
008100010618     d clnmat          ds
008200010618     d  mat                    1     31    dim(31)
008300010618     d clnpom          ds
008400010618     d  pom                    1     31    dim(31)
008500001009
008600080908     d fior48ds        ds                  inz
008700080908     d  I48poel                1      3  0
008800080908     d  I48dtda                4     11  0
008900080908     d  I48dtal               12     19  0
009000080908     d  I48deta1              20     20
009100080908     d  I48detb1              21     21
009200080908     d  I48dtpo               22     29  0
009300080908     d  I48Sede               30     30
009400080923     d  I48Tip                31     31
009500080923     d  I48ela                32     32
009600080908
009700080908     D TIBS02DS      E DS
009800080908     D TRUL31DS      E DS
009900080908     D TRUL31DS2     E DS
010000080908     D TIBS34DS      E DS
010100080908     d Azuteds       e ds                  extname(Azute00f)
010200080908     d dDatiute      e ds
010300080908     d dLat          e ds
010400080908     d dute01        e ds
010500010618     d ds5as1        e ds
010600010618     d ds5a2         e ds
010700070418     d  §5astb2      e                     extfld(§5astb)
010800001010
010900080916     d og147         e ds
011000080916     d kpjba         e ds
011100001006      *------------------------------------------------------------------------*
011200010108
011300010618      * emissione Videata
011400010108
011500010618      * pulizia Videata
011600010606     c                   exsr      Sr_Pulvid
011700010606
011800080908     c                   if        duterr=' '
011900010606     c                   do        *hival
012000010108
012100080908     c                   exfmt     or48d01
012200010108
012300010108      * F3=Fine
012400010605     c   kc              leave
012500010605     c                   setoff                                       28
012600010109
012700010618      * controlli
012800010605     c                   exsr      Sr_Contr
012900010109
013000010109      * F6=Elabora
013100010618     c                   if        *inkf = *on and *in28 = *off
013200010618     c                   exsr      Sr_Contr
013300010618     c  n28              exsr      Sr_Elabora
013400010618     c  n28              leave
013500010618     c                   endif
013600010108
013700010605     c   28              iter
013800010108
013900010605     c                   enddo
014000080908     c                   endif
014100001010
014200010605     c                   eval      *inlr = *on
014300010108      *****************************************************************
014400010109      * PULIZIA VIDEATA
014500010108      *****************************************************************
014600010605     c     Sr_Pulvid     BEGSR
014700010108
014800010605     c                   clear                   v1cdtda
014900010605     c                   clear                   v1cdtal
015000080908     c                   eval      v1cdeta1= 'N'
015100080908     c                   eval      v1cdetb1= 'N'
015200080923     c                   eval      v1ctip  = 'S'
015300080923     c                   eval      v1cela  = 'S'
015400010108
015500010605     c                   endsr
015600010108      **********************************************************************
015700010618      * CONTROLLO I DATI DELLA VIDEATA
015800010108      **********************************************************************
015900010618     c     Sr_Contr      begsr
016000010618
016100010618      * reset indicatori
016200010618     c                   setoff                                       404142
016300080924     c                   setoff                                       434445
016400010618
016500010622      * p.o. da elaborare
016600010618     c                   exsr      Sr_Contrpo
016700010618     c                   if        *in28 = *on
016800010618     c                   goto      endsrcontr
016900010618     c                   endif
017000010618
017100010622      * date
017200010618     c                   exsr      Sr_Contrdate
017300010618     c                   if        *in28 = *on
017400010618     c                   goto      endsrcontr
017500010618     c                   endif
017600080923     c
017700080923     c* non posso richiedere lista di controllo se solo creazione file
017800080923     c                   if        v1cela='F' and v1cdeta1='S'
017900080923     c                   seton                                        43  28
018000080923     c                   movel     msg(12)       v1cmsg
018100080923     c                   goto      endsrcontr
018200080923     c                   endif
018300080923     c
018400080923     c                   if        v1cela='F' and v1cdetb1='S'
018500080923     c                   seton                                        44  28
018600080923     c                   movel     msg(12)       v1cmsg
018700080923     c                   goto      endsrcontr
018800080923     c                   endif
018900010622
019000010622      * richiesta lista di controllo
019100080908     c                   if        v1cpoel = 0   and
019200080908     c                             v1cdeta1= 'S'
019300010622     c                   seton                                        43  28
019400010622     c                   movel     msg(9)        v1cmsg
019500010622     c                   goto      endsrcontr
019600010622     c                   endif
019700080908
019800080908      * richiesta lista di controllo
019900080908     c                   if        v1cpoel = 0   and
020000080908     c                             v1cdetb1= 'S'
020100080908     c                   seton                                        44  28
020200080908     c                   movel     msg(9)        v1cmsg
020300080908     c                   goto      endsrcontr
020400080908     c                   endif
020500080924     c
020600080924     c* Se richeisto anche file, non posso chiedere lista sintetica
020700080924     c                   if        v1cela='E'  and v1ctip='S'
020800080924     c                   seton                                        45  28
020900080924     c                   movel     msg(13)       v1cmsg
021000080924     c                   goto      endsrcontr
021100080924     c                   endif
021200010618
021300010618     c     endsrcontr    endsr
021400010618      **********************************************************************
021500010618      * CONTROLLO IL P.O. DA ELABORARE
021600010618      **********************************************************************
021700010618     c     Sr_Contrpo    begsr
021800010108
021900010618     c                   setoff                                       40
022000010622     c                   clear                   v1dpoel
022100080916     c                   clear                   wdao
022200010108
022300080908      * l'utente vi deve essere abilitato
022400080908    1c                   if        v1cpoel>0
022500080908     c                   movel     v1cpoel       w003a             3
022600080908     c     w003a         lookup    $pog                                   30
022700080908    2c                   if        *in30 = *off
022800010618     c                   seton                                        40  28
022900010618     c                   movel     msg(1)        v1cmsg
023000080908   x2c                   else
023100010108
023200010618      * decodifica e controlla la validità
023300010618     c     v1cpoel       chain     azorg01L
023400080908    3c                   if        not%found(azorg01l) or
023500010618     c                             ORGfva <> *blanks
023600010618     c                   seton                                        40  28
023700010618     c                   movel     msg(1)        v1cmsg
023800080908   x3c                   else
023900080908    4c                   if        orgfag <> 'A' and orgfag <> 'F'
024000010618     c                   seton                                        40  28
024100010618     c                   movel     msg(1)        v1cmsg
024200080908    4c                   endif
024300010618     c                   movel     orgDES        v1dpoel
024400080908    3c                   endif
024500080916     c* Verifico se filiale partita con le distinte orm automatiche
024600080916     c                   movel     orgde7        og147
024700080916    3c                   if        §ogddao=*blanks or §ogddao<*zeros
024800080916     c                   seton                                        40  28
024900080916     c                   movel     msg(11)       v1cmsg
025000080916   x3c                   else
025100080916     c                   movel     §ogddao       wdao
025200080916    3c                   endif
025300080916    2c                   endif
025400080908    1c                   endif
025500010108
025600010622     c     endsrctpo     endsr
025700010618      **********************************************************************
025800010618      * CONTROLLO LE DATE
025900010618      **********************************************************************
026000010618     c     Sr_Contrdate  begsr
026100010618
026200010618     c                   setoff                                       4142
026300010618
026400010618      * data 'da' deve essere impostata
026500010618     c                   if        v1cdtda = *zeros
026600010618     c                   seton                                        41  28
026700010618     c                   movel     msg(2)        v1cmsg
026800010618     c                   goto      endsrctdate
026900010618     c                   endif
027000010618
027100010618      * controllo la data inserita 'da'
027200010618     c                   clear                   wcdtda
027300010618     c                   clear                   wlbdat
027400010618     c                   z-add     v1cdtda       G02dat
027500010618     c                   call      'XSRDA8'
027600010618     c                   parm                    wlbdat
027700010618     c                   if        G02err = '1'
027800010618     c                   eval      *in41 = *ON
027900010618     c                   eval      *in28 = *ON
028000010618     c                   movel     msg(3)        v1cmsg
028100010618     c                   goto      endsrctdate
028200010618     c                   else
028300010618     c                   z-add     G02dat        v1cdtda
028400010618     c                   z-add     G02inv        wcdtda
028500010618     c                   endif
028600010618
028700010618      * non può essere >= ad oggi
028800010618     c                   if        wcdtda >= dataoggi
028900010618     c                   seton                                        41  28
029000010618     c                   movel     msg(4)        v1cmsg
029100010618     c                   goto      endsrctdate
029200010618     c                   endif
029300010618
029400010618      * deve essere > della data di pulizia ORM
029500010622     c                   if        wcdtda <= dtpuliaammgg
029600010618     c                   seton                                        41  28
029700010618     c                   movel     msg(5)        v1cmsg
029800010622     c                   movel     dtpuliggmmaa  datapul
029900010622     c                   eval      datamsg = %trim (giorno + '/' +
030000010622     c                             mese + '/' + anno)
030100010622     c                   eval      %subst(v1cmsg: 62: 10) = datamsg
030200010618     c                   goto      endsrctdate
030300010618     c                   endif
030400010618
030500010618      * deve essere un giorno feriale
030600010618     c     *iso          movel     wcdtda        wdatada
030700010618     c                   extrct    wdatada:*y    kann
030800010618     c                   extrct    wdatada:*m    kmes
030900010618     c                   extrct    wdatada:*d    wgiorno
031000011217     c                   clear                   ktfa
031100080908     c                   if        v1cpoel = 0
031200011217     c                   movel     simfel        ktfp
031300010618     c                   else
031400011217     c                   eval      ktfp = v1cpoel
031500010618     c                   endif
031600010618     c     kazcln        chain     azcln01l
031700010618     c                   if        %found(azcln01l) and
031800010618     c                             (MAT(wgiorno) =  'F'
031900010618     c                                    or  POM(wgiorno) =  'F')
032000010618     c                   seton                                        41  28
032100010622     c                   movel     msg(6)        v1cmsg
032200010618     c                   goto      endsrctdate
032300010618     c                   endif
032400010621      * BLOCCO
032500010621      * non deve essere inferiore a giugno 2001
032600010621     c                   if        kann = 2001 and kmes < 06
032700010621     c                   seton                                        41  28
032800010622     c                   movel     msg(8)        v1cmsg
032900010621     c                   goto      endsrctdate
033000010621     c                   endif
033100080916     c
033200080916     c* Se immesso p.o. controllo se abilitato alle distinte alla
033300080916     c*  data richiesta
033400080916     c                   if        v1cpoel>0 and wcdtda<wdao
033500080916     c                   seton                                        40  28
033600080916     c                   movel     msg(11)       v1cmsg
033700080916     c                   goto      endsrctdate
033800080916     c                   endif
033900080916     c
034000010618
034100010618      * data 'al' se non impostata è = a data 'da'
034200010618     c                   if        v1cdtal = *zeros
034300010618     c                   eval      v1cdtal = v1cdtda
034400010618     c                   eval      wcdtal = wcdtda
034500010618     c                   goto      endsrctdate
034600010618
034700010618     c                   else
034800010621
034900010618      * controllo la data inserita 'al'
035000010618     c                   clear                   wcdtal
035100010618     c                   clear                   wlbdat
035200010618     c                   z-add     v1cdtal       G02dat
035300010618     c                   call      'XSRDA8'
035400010618     c                   parm                    wlbdat
035500010618     c                   if        G02err = '1'
035600010618     c                   eval      *in42 = *ON
035700010618     c                   eval      *in28 = *ON
035800010622     c                   movel     msg(3)        v1cmsg
035900010618     c                   goto      endsrctdate
036000010618     c                   else
036100010618     c                   z-add     G02dat        v1cdtal
036200010618     c                   z-add     G02inv        wcdtal
036300010618     c                   endif
036400010618
036500010618      * non può essere >= ad oggi
036600030314      * tranne per sede
036700010618     c                   if        wcdtal >= dataoggi
036800030314     c                             and *in12 = *off
036900010618     c                   seton                                        42  28
037000010622     c                   movel     msg(4)        v1cmsg
037100010618     c                   goto      endsrctdate
037200010618     c                   endif
037300010618
037400010618      * deve essere > della data di pulizia ORM
037500010622     c                   if        wcdtal <= dtpuliaammgg
037600010618     c                   seton                                        42  28
037700010622     c                   movel     msg(5)        v1cmsg
037800010622     c                   movel     dtpuliggmmaa  datapul
037900010622     c                   eval      datamsg = %trim (giorno + '/' +
038000010622     c                             mese + '/' + anno)
038100010622     c                   eval      %subst(v1cmsg: 62: 10) = datamsg
038200010618     c                   goto      endsrctdate
038300010618     c                   endif
038400010621
038500010618      * deve essere un giorno feriale
038600010618     c     *iso          movel     wcdtal        wdataal
038700010618     c                   extrct    wdataal:*y    kann
038800010618     c                   extrct    wdataal:*m    kmes
038900010618     c                   extrct    wdataal:*d    wgiorno
039000011217     c                   clear                   ktfa
039100080908     c                   if        v1cpoel = 0
039200011217     c                   movel     simfel        ktfp
039300010618     c                   else
039400011217     c                   eval      ktfp = v1cpoel
039500010618     c                   endif
039600010618     c     kazcln        chain     azcln01l
039700010618     c                   if        %found(azcln01l) and
039800010618     c                             (MAT(wgiorno) =  'F'
039900010618     c                                    or  POM(wgiorno) =  'F')
040000010618     c                   seton                                        42  28
040100010622     c                   movel     msg(6)        v1cmsg
040200010618     c                   goto      endsrctdate
040300010618     c                   endif
040400010621
040500010621      * BLOCCO
040600010621      * non deve essere inferiore a giugno 2001
040700010621     c                   if        kann = 2001 and kmes < 06
040800010621     c                   seton                                        42  28
040900010622     c                   movel     msg(8)        v1cmsg
041000010621     c                   goto      endsrctdate
041100010621     c                   endif
041200010618
041300010618      * range
041400010618     c                   if        wcdtda > wcdtal
041500010618     c                   seton                                        41  28
041600010622     c                   movel     msg(3)        v1cmsg
041700010618     c                   goto      endsrctdate
041800010618     c                   endif
041900010618
042000010618      * range massimo di 31 gg
042100010726      * dal 26/07/01 range massimo di 7 gg (Bocchi)
042200030307      * 07/03/03 - NICOLA per sede il lancio deve essere possibile per 31 gg
042300010618     c     wdataal       subdur    wdatada       wgiolav:*d
042400010703     c                   eval      wgiolav = wgiolav + 1
042500030307     c                   if        wgiolav > 7 and Not *In12
042600010622     c                   movel     msg(7)        v1cmsg
042700030307     c                   Eval      %Subst(V1cmsg:50:2) = ' 7'
042800010618     c                   seton                                        41  28
042900010618     c                   goto      endsrctdate
043000010618     c                   endif
043100030307     c                   if        wgiolav > 31 and *In12
043200030307     c                   movel     msg(7)        v1cmsg
043300030307     c                   Eval      %Subst(V1cmsg:50:2) = '31'
043400030307     c                   seton                                        41  28
043500030307     c                   goto      endsrctdate
043600030307     c                   endif
043700010618
043800010618     c                   endif
043900010618
044000010618     c     endsrctdate   endsr
044100010606      **********************************************************************
044200010606      * ELABORA
044300010606      **********************************************************************
044400010606     C     Sr_Elabora    begsr
044500010606
044600080908     c                   eval      i48poel = v1cpoel
044700010606
044800080908     c                   eval      i48dtda = wcdtda
044900080908     c                   eval      i48dtal = wcdtal
045000010606
045100080908     c                   eval      i48deta1= v1cdeta1
045200080908     c                   eval      i48detb1= v1cdetb1
045300080923     c                   eval      i48tip  = v1ctip
045400080923     c                   eval      i48ela  = v1cela
045500080924     c
045600080924     c* Per creazione solo file passo sempre flag analitico
045700080924     c                   if        i48ela='F'
045800080924     c                   eval      i48tip='A'
045900080924     c                   endif
046000010621
046100010621     c                   clear                   kpjbu
046200080908     c                   movel     fior48ds      kpjbu
046300010606
046400080908     c                   move      'OR49'        kcoaz
046500080908     c******             call      'FIOR49R'
046600030307     c                   If        *In12
046700030307     c                   call      'BCH09'
046800030307     c                   parm                    kpjba
046900030307     c                   EndIf
047000011217     c                   call      'BCH10'
047100030307     c                   parm                    kpjba
047200010606
047300010618     c                   endsr
047400001010      *****************************************************************
047500001010      * ROUTINE INIZIALE
047600001010      *****************************************************************
047700010618     c     *INZSR        begsr
047800001009
047900010618     c     *entry        plist
048000010618     c                   parm                    kpjba
048100080908      *
048200080908     c     *dtaara       define    §azute        azuteds
048300080908     c     *dtaara       define    §datiute      ddatiute
048400080908     c                   in(E)     *dtaara
048500080908    1c                   If        %error  or RSUT = *blanks
048600080908     c                   Clear                   Tibs34ds
048700080908     c                   Call      'TIBS34R'
048800080908     c                   Parm                    Tibs34ds
048900080908     c                   In        *dtaara
049000080908    1c                   EndIf
049100080908
049200080908     c                   Clear                   wabi
049300080908     c                   Clear                   v1cmsg
049400080908     c                   Clear                   dLat
049500080908
049600080908      * Verifica errori e autorità profilo
049700080908s   1c                   Select
049800080908      * se ho errori nei dati utente esco dal pgm
049900080908w   1c                   When      DutErr = 'E'
050000080908     c                   clear                   wabi
050100080908      * se non c'è l'abilitazione
050200080908      * --> se 1° livello, abilitazioni al terminal
050300080908      *     se 2° livello, abilitazioni al punto operativo
050400080908      *     se sede è impossibile ma se capita mando a fine il pgm
050500080908w   1c                   When      UteAut = *Blanks
050600080908if  2c                   If        DutLpo = '1'
050700080908     c                   Eval      wabi   = 'TP'
050800080908e   2c                   EndIf
050900080908if  2c                   If        DutLpo = '2'
051000080908     c                   Eval      wabi   = 'PO'
051100080908e   2c                   EndIf
051200080908if  2c                   If        DutLpo = 'S'
051300080908     c                   eval      duterr='E'
051400080908     c                   clear                   wabi
051500080908e   2c                   EndIf
051600080908      * carica le abilitazioni del profilo
051700080908x   1c                   Other
051800080908     c                   Eval      wabi = UteAut
051900080908e   1c                   EndSl
052000080908
052100080908      * controllo se ok l'abilitazione dell'utente
052200080908     c                   Clear                   Tibs02ds
052300080908     c                   Eval      T02Mod = 'C'
052400080908     c                   Eval      T02Sif = knsif
052500080908     c                   Eval      T02Cod = 'LAT'
052600080908     c                   Movel(p)  wabi          T02Ke1
052700080908     c                   Call      'TIBS02R'
052800080908     c                   Parm                    kpjba
052900080908     c                   Parm                    Tibs02ds
053000080908if  1c                   If        T02Err = *Blanks
053100080908     c                   Eval      dLat = T02Uni
053200080908e   1c                   EndIf
053300080908      * errore o non abilitato
053400080908if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
053500080908     c                   Seton                                        28
053600080908     C                   movel     msg(10)       v1cmsg
053700080908   x1c                   Else
053800080908      * Reperimento dei P.O. gestibili dall'utente
053900080908     c                   clear                   TRUL31DS
054000080908     c                   clear                   TRUL31DS2
054100080908     c                   eval      I31abi = wabi
054200080908     c                   eval      I31cdi = DUTdis
054300080908     c                   eval      I31car = DUTare
054400080908     c                   eval      I31cpo = DUTpou
054500080908     c                   call      'TRUL31R'
054600080908     c                   parm                    KPJBA
054700080908     c                   parm                    TRUL31DS
054800080908     c                   parm                    TRUL31DS2
054900080908if  2c                   if        O31pog > *zeros
055000080908     c                   movea     O31pog        $pog
055100080908     c                   movea     O31arg        $arg
055200080908     c                   movea     O31dig        $dig
055300080908x   2c                   else
055400080908     c                   Seton                                        28
055500080908     c                   movel     MSG(10)       v1cmsg
055600080908e   2c                   endif
055700080908    1c                   endif
055800010618
055900010618     c     ktab          klist
056000010618     c                   kfld                    codut
056100010618     c                   kfld                    kcod
056200010618     c                   kfld                    kkey
056300080908     c                   z-add     1             codut             1 0
056400010618
056500010618     c     kazcln        klist
056600010618     c                   kfld                    ktfp
056700010618     c                   kfld                    ktfa
056800010618     c                   kfld                    kann
056900010618     c                   kfld                    kmes
057000080908     c
057100080908     c                   clear                   fior48ds
057200010625     c                   clear                   v1cpoel
057300010625     c                   clear                   v1dpoel
057400010618
057500010618      * da sede non imposto il p.o. da elaborare
057600010625     c                   IF        simfel = *zeros
057700010618     c                   eval      *in12 = *on
057800080908     c                   eval      i48sede = 'S'
057900010625      *
058000010625     c                   ELSE
058100010625      *
058200010618      * da filiale imposto il p.o. da elaborare
058300080908     c                   eval      i48sede = 'F'
058400010108
058500080908     c                   if        dutlpo = '2' or dutlpo = *blanks
058600080908     c                   movel     dutpou        V1Cpoel
058700020808     c                   eval      *in06 = *On
058800010618     c                   else
058900010618     c                   movel     SIMFEL        V1Cpoel
059000010618     c                   endif
059100010625      *
059200010625     c     V1Cpoel       chain     azorg01L
059300010625     c                   IF        not%found(azorg01l) or
059400010625     c                             orgfva <> *blanks   or
059500010625     c                             (orgfag <> 'A' and orgfag <> 'F')
059600010625     c                   eval      *in28 = *on
059700010625     c                   eval      *in40 = *on
059800010625     c                   movel     msg(1)        v1cmsg
059900010625     c                   ELSE
060000010618      * decodifico p.o. da elaborare
060100010618     c                   movel     orgDES        v1dpoel
060200010625     c                   ENDIF
060300010618     c                   endif
060400010705
060500010705      * se profilo EDP richiedo la stampa di controllo
060600010705     c                   movel     knmus         wprofilo
060700010705     c                   eval      *in13 = (wprofilo = 'EDP')
060800001009
060900001023      * reperisce data e ora
061000010618     c                   TIME                    W0140
061100001010      * UDATE IN GGMMAAAA
061200010618     c                   MOVE      W0140         WDTGIO
061300001010      * UDATE IN AAAAMMGG
061400010618     c                   Z-ADD     WDTGIO        G02DAT
061500010618     c                   MOVEL     *BLANK        G02ERR
061600010618     c                   CALL      'XSRDA8'
061700010618     c                   PARM                    WLBDAT
061800010618     c                   Z-ADD     G02INV        dataoggi
061900030321     c                   z-add     g02tgi        sav_g02tgi
062000010618
062100010618      * cerco data limite lancio elaborazione
062200010618      * per sede
062300010625     c                   IF        *in12 = *on
062400010618     c                   clear                   ds5as1
062500010618     c                   movel     '5A'          kcod
062600010618     c                   movel     'SEDE1'       kkey
062700010618     c     ktab          chain     tabel00f
062800010618     c                   if        %found(tabel00f) and tblflg = *blanks
062900010618     c                   movel     tbluni        ds5as1
063000010625     c                   z-add     §5aors        W0030
063100010618     c                   endif
063200010625      *
063300010625     c                   ELSE
063400010625      * per filiale
063500010625     c                   clear                   ds5a2
063600010625     c                   movel     '5A'          kcod
063700010625     c                   movel     '2       '    kkey
063800010625     c     ktab          chain     tabel00f
063900010625     c                   if        %found(tabel00f) and tblflg = *blanks
064000010625     c                   movel     tbluni        ds5a2
064100010625     c                   z-add     §5aorf        W0030
064200010625     c                   endif
064300010625      *
064400010625     c                   ENDIF
064500010618
064600010618      * Calcolo data
064700030321     c     sav_g02tgi    sub       W0030         giotgi
064800010618     c                   call      'XSRGI8'
064900010618     c                   parm                    wgidat
065000010622     c                   z-add     giodat        dtpuliggmmaa
065100010618
065200010618      * Giro la data
065300010622     c                   if        dtpuliggmmaa > 0
065400010622     c                   move      dtpuliggmmaa  g02dat
065500010618     c                   movel     *blanks       g02err
065600010618     c                   call      'XSRDA8'
065700010618     c                   parm                    wlbdat
065800010618     c                   if        g02err = '1'
065900010625     c                   clear                   dtpuliggmmaa
066000010618     c                   else
066100010622     c                   z-add     g02inv        dtpuliaammgg
066200010618     c                   endif
066300010618     c                   endif
066400030321
066500030321      * se sono in sede
066600030321      * cerco anche quella dei p.o. per elaborare solo gli ORM presenti nei p.o.
066700030321     c                   IF        *in12 = *on
066800030321     c                   clear                   ds5a2
066900030321     c                   movel     '5A'          kcod
067000030321     c                   movel     '2       '    kkey
067100030321     c     ktab          chain     tabel00f
067200030321     c                   if        %found(tabel00f) and tblflg = *blanks
067300030321     c                   movel     tbluni        ds5a2
067400030321     c                   z-add     §5aorf        W0030
067500030321     c                   endif
067600030321      * Calcolo data
067700030321     c     sav_g02tgi    sub       W0030         giotgi
067800030321     c                   call      'XSRGI8'
067900030321     c                   parm                    wgidat
068000080908     c                   z-add     giodat        i48dtpo
068100030321
068200030321      * Giro la data
068300080908     c                   if        i48dtpo > 0
068400080908     c                   move      i48dtpo       g02dat
068500030321     c                   movel     *blanks       g02err
068600030321     c                   call      'XSRDA8'
068700030321     c                   parm                    wlbdat
068800030321     c                   if        g02err = '1'
068900080908     c                   clear                   i48dtpo
069000030321     c                   else
069100080908     c                   z-add     g02inv        i48dtpo
069200030321     c                   endif
069300030321     c                   endif
069400030321     c                   endif
069500010618
069600010618     c                   endsr
069700001010      *---------------------------------------------------------------------------------------------
069800001031** MSG  Lungh. 78                                                            *
069900080908Filiale errata o utente non abilitato a richiedere la filiale immessa               1
070000010622Inserire la data elaborazione                                                       2
070100010622Data elaborazione errata                                                            3
070200010622Data elaborazione deve essere inferiore alla data odierna                           4
070300010622Data elaborazione deve essere maggiore alla data pulizia ORM                        5
070400010622Data elaborazione deve essere un giorno feriale                                     6
070500030307Non è possibile elaborare un periodo superiore a xx giorni                          7
070600010622Data elaborazione deve essere maggiore del 31 Maggio 2001                           8
070700080908Dettaglio ORM richiedibile solo se si elabora una specifica filiale                 9
070800080908Utente non abilitato alla Stampa della statistica controllo ORM                    10
070900080916Filiale non partita con le distinte automatiche ORM                                11
071000080924Se elaborazione solo del file: impossibile richiedere dettaglio ORM                12
071100080924Per stampa e creazione file è necessario richiedere la stampa ANALITICA            13
