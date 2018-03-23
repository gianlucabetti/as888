000100020808      *PARMS CLOSQLCSR(*ENDMOD)
000200020808      *PARMS DYNUSRPRF(*OWNER)
000300010604     H DECEDIT('0,') DATEDIT(*DMY.)
000400010604      *----------------------------------------------------*
000500010605      *
000600010620      *
ATTENZIONE?: se variato il file WFROR bisogna
000700010620      *              variare la lunghezza della sk rfile
000800010620      *
000900010620      *----------------------------------------------------*
001000010705      *
            Statistiche ORM            ?
001100010605      *
001200010604      *----------------------------------------------------*
001300020808     ffnorm00f  if   e             disk
001400010606     ffnorf01l  if   e           k disk
001500040420     fFnorf03l  if   e           k disk    rename(Fnorf000:Fnorf03)
001600010614     ffnorv01l  if   e           k disk
001700010605     fazorg01l  if   e           k disk
001800010611     ftntbe01l  if   e           k disk
001900010606     fazcln01l  if   e           k disk
002000010620     ftabel00f  if   e           k disk
002100010621     fwfror01l  uf a e           k disk    usropn
002200010621     fwfror11l  uf a e           k disk    prefix(p_) usropn
002300010607     ffior41p   o    e             printer oflind(*in90)
002400010702     ffior41p1  o    e             printer oflind(*in92) usropn                 controllo
002500010621     ffior41p2  o    e             printer oflind(*in91) usropn                 comodo EDP
002600010614
002700010614      *----------------------------------------------------*
002800010614      *  RIEPILOGO INDICATORI
002900010614      *----------------------------------------------------*
003000010614
003100010620      * 10 - stampa lista di comodo EDP
003200010620      * 11 - stampa lista di controllo
003300010620      * 12 - sede
003400010621      * 13 - stampo per p.o. se stampa sintetica
003500030912      * 14 - ORM commissionati
003600030918      * 15 - Lettura rcd 'B' orm commissionati x stampa
003700020808      * 30 - Lookup
003800010614      * 90 - overflow file stampa statistica
003900010621      * 91 - overflow file stampa di comodo EDP
004000010620      * 92 - overflow file stampa di controllo
004100010605
004200010605      *----------------------------------------------------*
004300010605
004400010605     d kazorg          s                   like(orgfil)
004500010604
004600010605     d kormpoe         s                   like(ormpoe)
004700010605     d kormnsr         s                   like(ormnsr)
004800010605     d kormnor         s                   like(ormnor)
004900010605     d kormnrv         s                   like(ormnrv)
005000010605     d kormdfo         s                   like(ormdfo)
005100010606     d kormofo         s                   like(ormofo)
005200010606     d kormfao         s                   like(ormfao)
005300010618     d korvdtv         s                   like(orvdtv)
005400010606
005500030912     d krortrc         s                   like(rortrc)
005600010608     d krorpoe         s                   like(rorpoe)
005700010608     d krordta         s                   like(rordae)
005800010608
005900010606     d ktfp            s                   like(clntfp)
006000010606     d ktfa            s                   like(clntfa)
006100010606     d kann            s                   like(clnann)
006200010606     d kmes            s                   like(clnmes)
006300010604
006400010608     d wgiorno         s              2  0
006500010604     d w0140           s             14  0
006600010604     d wdtgio          s              8  0
006700010604     d wora            s              6  0
006800010604     d wdate           s              8  0
006900010606     d dataela         s              8  0
007000010621
007100010621     d lenght          s             15  5 inz(80)                              lunghezza cmd CLP
007200010621     d comman          s             80                                         valore cmd CLP
007300030912     d wlib            s             10
007400010620
007500010606     d woksk           s              1    inz('0')
007600010606     d wpoe            s              1    inz('0')
007700010606     d wpor            s              1    inz('0')
007800010620     d wokstp          s              1    inz('0')
007900010608     d wok600          s              1    inz('0')
008000010607     d wok900          s              1    inz('0')
008100010608     d wokrode         s              1    inz('0')
008200010613     d wokroes         s              1    inz('0')
008300010608     d wokrfile        s              1    inz('0')
008400010612     d wokfestae       s              1    inz('0')
008500010612     d wokfestar       s              1    inz('0')
008600010614     d wokstpcau       s              1    inz('0')
008700010605
008800010606     d wrorode         s              5  0 inz(*zeros)
008900010620     d wrordan         s              5  0 inz(*zeros)
009000010606     d wrordit         s              5  0 inz(*zeros)
009100010606     d wrordr1         s              5  0 inz(*zeros)
009200010606     d wrordr2         s              5  0 inz(*zeros)
009300010606     d wrordro         s              5  0 inz(*zeros)
009400010606     d wroroes         s              5  0 inz(*zeros)
009500010614     d wrorean         s              5  0 inz(*zeros)
009600010606     d wroreit         s              5  0 inz(*zeros)
009700010606     d wrorer1         s              5  0 inz(*zeros)
009800010606     d wrorer2         s              5  0 inz(*zeros)
009900010606     d wrorero         s              5  0 inz(*zeros)
010000010606     d wrorone         s              5  0 inz(*zeros)
010100010620     d wrornan         s              5  0 inz(*zeros)
010200010606     d wrornit         s              5  0 inz(*zeros)
010300010606     d wrornr1         s              5  0 inz(*zeros)
010400010606     d wrornr2         s              5  0 inz(*zeros)
010500010606     d wrornro         s              5  0 inz(*zeros)
010600010606     d wroroea         s              5  0 inz(*zeros)
010700010620     d wroraan         s              5  0 inz(*zeros)
010800010606     d wrorait         s              5  0 inz(*zeros)
010900010606     d wrorar1         s              5  0 inz(*zeros)
011000010606     d wrorar2         s              5  0 inz(*zeros)
011100010606     d wroraro         s              5  0 inz(*zeros)
011200010606     d wrorn01         s              5  0 inz(*zeros)
011300010606     d wrorn02         s              5  0 inz(*zeros)
011400010606     d wrorn03         s              5  0 inz(*zeros)
011500010606     d wrorn04         s              5  0 inz(*zeros)
011600010606     d wrorn05         s              5  0 inz(*zeros)
011700010606     d wrorn06         s              5  0 inz(*zeros)
011800010606     d wrorn07         s              5  0 inz(*zeros)
011900010606     d wrorn08         s              5  0 inz(*zeros)
012000010606     d wrorn09         s              5  0 inz(*zeros)
012100010606     d wrorn10         s              5  0 inz(*zeros)
012200010606     d wrora01         s              5  0 inz(*zeros)
012300010606     d wrora02         s              5  0 inz(*zeros)
012400010606     d wrora03         s              5  0 inz(*zeros)
012500010606     d wrora04         s              5  0 inz(*zeros)
012600010606     d wrora05         s              5  0 inz(*zeros)
012700010606     d wrora06         s              5  0 inz(*zeros)
012800010606     d wrora07         s              5  0 inz(*zeros)
012900010606     d wrora08         s              5  0 inz(*zeros)
013000010606     d wrora09         s              5  0 inz(*zeros)
013100010606     d wrora10         s              5  0 inz(*zeros)
013200010606     d wrora11         s              5  0 inz(*zeros)
013300010606     d wrora12         s              5  0 inz(*zeros)
013400010606     d wrora13         s              5  0 inz(*zeros)
013500010606     d wrora14         s              5  0 inz(*zeros)
013600010606     d wrora15         s              5  0 inz(*zeros)
013700010606     d wrornqd         s              5  0 inz(*zeros)
013800010606     d wrornbl         s              5  0 inz(*zeros)
013900010614     d wrornas         s              5  0 inz(*zeros)
014000010614     d wrormdr         s              5  0 inz(*zeros)
014100030912     d wroroned        s              5  0 inz(*zeros)
014200030912     d wrorn01d        s              5  0 inz(*zeros)
014300030912     d wrorn02d        s              5  0 inz(*zeros)
014400030912     d wrorn03d        s              5  0 inz(*zeros)
014500030912     d wrorn04d        s              5  0 inz(*zeros)
014600030912     d wrorn05d        s              5  0 inz(*zeros)
014700030912     d wrorn06d        s              5  0 inz(*zeros)
014800030912     d wrorn07d        s              5  0 inz(*zeros)
014900030912     d wrorn08d        s              5  0 inz(*zeros)
015000030912     d wrorn09d        s              5  0 inz(*zeros)
015100030912     d wrorn10d        s              5  0 inz(*zeros)
015200030917     d wrorifian       s              5  0 inz(*zeros)
015300030917     d wrorifine       s              5  0 inz(*zeros)
015400030917     d wrorifid        s              5  0 inz(*zeros)
015500030917     d wroricean       s              5  0 inz(*zeros)
015600030917     d wroricene       s              5  0 inz(*zeros)
015700030917     d wroriced        s              5  0 inz(*zeros)
015800030917     d wroriinan       s              5  0 inz(*zeros)
015900030917     d wroriinne       s              5  0 inz(*zeros)
016000030917     d wroriind        s              5  0 inz(*zeros)
016100030917     d wrorivaan       s              5  0 inz(*zeros)
016200030917     d wrorivane       s              5  0 inz(*zeros)
016300030917     d wrorivad        s              5  0 inz(*zeros)
016400030917     d wroriccan       s              5  0 inz(*zeros)
016500030917     d wroriccne       s              5  0 inz(*zeros)
016600030917     d wroriccd        s              5  0 inz(*zeros)
016700010605
016800010611     d dspoele         s                   like(f_rorpoe)
016900010611     d dspoelr         s                   like(f_rorpoe)
017000010611
017100010620     d wdatafes        s              8  0
017200010605     d wdtda01         s              8  0
017300010606     d wdtda02         s              8  0
017400130118     d wORMcor         s              7s 0 inz
017500010605
017600030918     d sav_trc         s                   like(f_rortrc)
017700010608     d sav_data        s                   like(f_rordae)
017800010621     d sav_dis         s                   like(f_rordis)
017900010621     d sav_are         s                   like(f_rorare)
018000010621     d sav_poel        s                   like(f_rorpoe)
018100010620     d sav_dsdatap     s                   like(f_rordae)
018200010620     d sav_dspoelp     s                   like(f_rorpoe)
018300010620     d sav_dsdatac     s                   like(f_rordae)
018400010620     d sav_dspoelc     s                   like(f_rorpoe)
018500010614     d sav_orvdar      s                   like(orvdar)
018600010607
018700010612     d xd              s              5i 0 inz(*zeros)                          indice sk date
018800010612     d xa              s              5i 0 inz(*zeros)                          indice sk po da ela.
018900010612     d xf              s              5i 0 inz(*zeros)                          indice sk file
019000020807     d Xb              s              5i 0 inz(*zeros)                          indice sk po TRUL26
019100030918     d sxf             s              5i 0 inz(*zeros)                          indice sk file save
019200130118     d xc              s              5i 0 inz(*zeros)
019300130118     d xx              s              5i 0 inz(*zeros)
019400020808
019500020808     d StringaSql      s            500    Varying
019600010605
019700010608     d wdtda           s               d   datfmt(*iso)
019800010608     d wdtal           s               d   datfmt(*iso)
019900031002
020000010605
020100010605      *   S C H I E R E
020200010605
020300010608     d skdate          s              8  0 dim(31)                              date da elab.
020400020807     d skpoel          s              3  0 dim(250)                             po da elab.
020500030912     d kfile           s             12    dim(18000)                           file (key)
020600010605     d                                     varying
020700010606     d                                     inz
020800030912     d kfile1          s             11    dim(18000)                           file (key) appoggio
020900030912     d                                     varying
021000030912     d                                     inz
021100030912     d rfile           s            456    dim(18000)                           file (record)
021200010605     d                                     varying
021300010606     d                                     inz
021400010621
021500010621     d cmd             s             80    dim(1) ctdata perrcd(1)              comando
021600031002
021700130118     d skcliosr        s              7s 0 dim(999)                             clienti tab OSR.
021800130118     d skstaosr        s              1    dim(999)                             OSR non da elab.
021900060926     d skfar           s              3  0 dim(999)                             fase da non elab.
022000010604
022100010604      *   D S   I N T E R N E / E S T E R N E
022200010604
022300010606     d dskfile         ds                  inz
022400030912     d  dstrc                              Like(f_rortrc)
022500010605     d  dsdata                             like(f_rordae)
022600010608     d  dspoel                             like(f_rorpoe)
022700030912
022800030912     d dskfile1        ds                  inz
022900030912     d  dsdata1                            like(f_rordae)
023000030912     d  dspoel1                            like(f_rorpoe)
023100010605
023200010604     d wlbdat          ds                  inz
023300010604     d  g02dat                 1      8  0
023400010604     d  g02inv                 9     16  0
023500010604     d  g02err                17     17
023600010604     d  g02tgi                18     22  0
023700031002
023800031002     d wosr            ds
023900031002     d  wOrmPoe                1      3
024000031002     d  wOrmNsr                4      5
024100010606
024200010606     d clnmat          ds
024300010606     d  mat                    1     31    dim(31)
024400010606     d clnpom          ds
024500010606     d  pom                    1     31    dim(31)
024600010604
024700010605     d dswfror       e ds                  extname(wfror00f)
024800010605     d                                     prefix(f_)
024900010606     d                                     inz
025000010608     d dswfror0      e ds                  extname(wfror00f)
025100010608     d                                     inz
025200010621     d dswfror1      e ds                  extname(wfror10f)
025300010621     d                                     prefix(p_)
025400010621     d                                     inz
025500010619     d dcmr          e ds
025600010604     d fior40ds      e ds
025700010604     d cncr80        e ds
025800010604     d ut§dse0f      e ds
025900030912     d dorm01        e ds
026000031002     d dosr          e ds
026100060926     d dfar          e ds
026200020807
026300020807     d FnormxxDs     e ds                  ExtName(Fnorm00f)
026400020807
026500020807     d Trul26Ds      e ds
026600020807     d  skpo                  11    760  0 dim(250)
026700020807     d  skpot                761   1510  0 dim(250)
026800010604
026900010604     d kpjba         e ds
027000010604     d  rem                    1      3
027100010604     d  remfil                 4      6
027200010604
027300010604      *--------------------------------------------------------------*
027400010604
027500010608      * stampo la videata delle selezioni
027600010612      *                            ========
027700010604     c                   exsr      PRESELEZ
027800010612      *                            ========
027900020808
028000020808      * Se sede e richiesto un solo p.o. o filiale devo elaborare con SQL
028100020808     c                   If        (I40sede <> 'F' and I40poel <> 999) or
028200020808     c                              I40sede = 'F'
028300020807      * Sql per selezionare gli ORM
028400020807      *                            ========
028500020808     c                   ExSr      SqlOrmPos
028600020807      *                            ========
028700020808     c                   EndIf
028800020807
028900010612     c                   eval      xd = 1
029000010608      * devo elaborare per data
029100010612     c                   do        31            xd
029200010612     c                   if        skdate(xd) = *zeros
029300010608     c                   leave
029400010608     c                   endif
029500010608
029600010612     c                   eval      dataela = skdate(xd)
029700020808
029800020808      * Se sede e richiesti tutti i p.o devo leggere tutto il file
029900020808     c                   If        I40sede <> 'F' and I40poel = 999
030000020808      *                            ===========
030100020808     c                   ExSr      Sr_LeggiOrm
030200020808      *                            ===========
030300020808     c                   Else
030400020808      * Se sede e richiesto un solo p.o. o filiale devo leggere con SQL
030500010614      *                            ========
030600010614     c                   exsr      SR_LEGGI
030700010614      *                            ========
030800020808     c                   EndIf
030900010608     c                   enddo
031000010608
031100010612      * creo il file e stampo la sk
031200010612     c                   eval      xf = 1
031300010612     c                   do        18000         xf
031400010612      * esco se non ho più niente caricato nella sk
031500030912     c                   if        kfile1(xf) = *zeros
031600010608     c                   leave
031700010608     c                   endif
031800010612      * carico le ds
031900010612     c                   eval      dskfile = kfile(xf)
032000030912     c                   eval      dskfile1 = kfile1(xf)
032100010612     c                   eval      dswfror = rfile(xf)
032200010621      * scrivo file appoggio
032300010621     c                   eval      dswfror1 = rfile(xf)
032400010621     c                   write     wfror100
032500010621     c                   clear                   dswfror1
032600010612      * scrivo il file
032700010608     c                   if        i40tpel = 'F'
032800030912     c                   eval      krortrc = dstrc
032900010608     c                   eval      krorpoe = dspoel
033000010608     c                   eval      krordta = dsdata
033100010608     c     kror          chain     wfror01l
033200010608     c                   if        not%found(wfror01l)
033300010612     c                   eval      dswfror0 = rfile(xf)
033400010612      *                            ========
033500010608     c                   write     wfror000
033600010612      *                            ========
033700010612     c                   clear                   dswfror0
033800010608     c                   endif
033900010608     c                   endif
034000010608
034100010608     c                   enddo
034200010621      * stampo
034300010621      *                            =========
034400010621     c                   exsr      SR_STAMPA
034500010621      *                            =========
034600010605
034700010608      * fine programma
034800010605     c                   eval      *inlr = *on
034900010605
035000010604      *--------------------------------------------------------------*
035100010604      * PREPARA LA STAMPA DELLA VIDEATA DELLE SELEZIONI
035200010604      *--------------------------------------------------------------*
035300010604     c     PRESELEZ      begsr
035400010608
035500010608      * se vengono creati i dati, li devo anche cancellare prima
035600010608     c                   if        i40tpel= 'F'
035700010608
035800010612     c                   eval      xa = 1
035900020807     c                   do        250           xa
036000010612     c                   if        skpoel(xa) = *zeros
036100010608     c                   leave
036200010608     c                   endif
036300010608
036400010611     c     *iso          movel     i40dtda       wdtda
036500010611     c     *iso          movel     i40dtal       wdtal
036600010620
036700010608     c                   dow       wdtda<=wdtal
036800010612     c                   eval      krorpoe = skpoel(xa)
036900010608     c                   move      wdtda         krordta
037000030919     c     kror1         setll     wfror01l
037100010608     c                   if        %equal(wfror01l)
037200030919     c     kror1         reade     wfror01l
037300010613     c                   dow       not%eof(wfror01l)
037400010608     c                   delete    wfror000
037500030919     c     kror1         reade     wfror01l
037600010608     c                   enddo
037700010608     c                   endif
037800010608     c
037900010608     c                   adddur    1:*d          wdtda
038000010608     c                   enddo
038100010608
038200010608     c                   enddo
038300010608     c                   endif
038400010618
038500010618      * stampo la prima pagina con le selezioni
038600010618     c                   eval      *in12 = (i40sede = 'S')
038700010618
038800010618     c                   if        i40poel = 999
038900070418     c                   eval      despo = 'Tutte le fil.dell''AS'
039000010618     c                   else
039100010618     c                   eval      kazorg = i40poel
039200010618     c     kazorg        chain     azorg01l
039300010618     c                   if        %found(azorg01l)
039400010618     c                   movel     orgdes        despo
039500010618     c                   endif
039600010618     c                   endif
039700010618
039800010618     c                   clear                   wlbdat
039900010618     c                   move      i40dtda       g02inv
040000010618     c                   movel     '3'           G02err
040100010618     c                   call      'XSRDA8'
040200010618     c                   parm                    wlbdat
040300010618     c                   movel     g02dat        datada
040400010618
040500010618     c                   clear                   wlbdat
040600010618     c                   move      i40dtal       g02inv
040700010618     c                   movel     '3'           G02err
040800010618     c                   call      'XSRDA8'
040900010618     c                   parm                    wlbdat
041000010618     c                   movel     g02dat        dataal
041100010618     c                   write     or41s1
041200010618
041300010621     c   11              write     or41s1c
041400010604     c                   endsr
041500010614
041600010614      *--------------------------------------------------------------*
041700010614      * STAMPA
041800010614      *--------------------------------------------------------------*
041900010614     c     SR_STAMPA     begsr
042000010621
042100010621      * leggo file di comodo per la stampa
042200010621     c     *loval        setll     wfror11l
042300010621 b1  c                   do        *hival
042400010621     c                   read      wfror11l
042500010621 b2  c                   if        %eof(wfror11l)
042600010621     c                   leave
042700010621 e2  c                   endif
042800030916
042900030918     c                   Eval      *In15 = (p_RorTrc = 'B')
043000010614
043100010705     ?* lancio fatto con un p.o. e più date devo stampare per p.o.
043200010621     ?* se richiesta la stampa sintetica
043300010621 b2  c                   if        *in13 = *on
043400010621      * rottura per p.o.
043500010621 b3  c                   if        sav_poel <> p_rorpoe
043600010621      * decodifico il p.o. che sto elaborando
043700010621     c                   eval      kazorg = p_rorpoe
043800010621     c     kazorg        chain     azorg01l
043900010621 b4  c                   if        %found(azorg01l)
044000010621     c                   movel     orgdes        despoel
044100010621 e4  c                   endif
044200010621      * decodifico il distretto
044300010621     c                   clear                   descrdis
044400010621     c                   movel     '17'          tblcod
044500010621     c                   movel(P)  p_rordis      tblkey
044600010621     c     ktab          chain     tabel00f
044700010621 b4  c                   if        %found(tabel00f)
044800010621     c                   movel     tbluni        descrdis
044900010621 e4  c                   endif
045000010621      * decodifico l'area
045100010621     c                   clear                   descrare
045200010621     c                   movel     '05'          tblcod
045300010621     c                   movel(P)  p_rorare      tblkey
045400010621     c     ktab          chain     tabel00f
045500010621 b4  c                   if        %found(tabel00f)
045600010621     c                   movel     tbluni        descrare
045700010621 e4  c                   endif
045800010621     c                   write     or41t1
045900010621     c                   write     or41t2
046000010621     c                   write     or41t3
046100010621     c                   write     or41t4
046200010621     c                   eval      sav_poel = p_rorpoe
046300010621 e3  c                   endif
046400010621      * giro la data di elaborazione
046500010621     c                   clear                   wlbdat
046600010621     c                   move      p_rordae      g02inv
046700010621     c                   movel     '3'           G02err
046800010621     c                   call      'XSRDA8'
046900010621     c                   parm                    wlbdat
047000010621     c                   movel     g02dat        dsdatap
047100010621      * salto pagina
047200010621 b3  c                   if        *in90 = *on
047300010621     c                   write     or41t1
047400010621     c                   write     or41t2
047500010621     c                   write     or41t3
047600010621     c                   write     or41t4
047700010621     c                   eval      *in90 = *off
047800010621 e3  c                   endif
047900010621      * calcolo la percentuale dei non eseguiti sui da eseguire
048000010705     c                   if        P_rorode > *zeros
048100010621     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
048200010705     c                   endif
048300030918     c  n15              write     or41d1
048400030918     c   15              write     or41d1b
048500010621     ?* stampo per data
048600010621 x2  c                   else
048700010621      * rottura per data
048800010621 b3  c                   if        sav_data <> p_rordae
048900010614     c                   clear                   wlbdat
049000010621     c                   move      p_rordae      g02inv
049100010614     c                   movel     '3'           G02err
049200010614     c                   call      'XSRDA8'
049300010614     c                   parm                    wlbdat
049400010614     c                   movel     g02dat        dsdatap
049500010614     c                   write     or41t1
049600010614     c                   write     or41t2
049700010621     c                   eval      sav_data = p_rordae
049800010621     c                   clear                   sav_dis
049900010621     c                   clear                   sav_are
050000010622     c                   clear                   sav_poel
050100030922     c                   clear                   sav_trc
050200010621 e3  c                   endif
050300010621      * rottura per distretto
050400010621 b3  c                   if        sav_dis <> p_rordis
050500010621      * distretto
050600010621     c                   clear                   descrdis
050700010621     c                   movel     '17'          tblcod
050800010621     c                   movel(P)  p_rordis      tblkey
050900010621     c     ktab          chain     tabel00f
051000010621 b4  c                   if        %found(tabel00f)
051100010621     c                   movel     tbluni        descrdis
051200010621 e4  c                   endif
051300010621      * area
051400010621     c                   clear                   descrare
051500010621     c                   movel     '05'          tblcod
051600010621     c                   movel(P)  p_rorare      tblkey
051700010621     c     ktab          chain     tabel00f
051800010621 b4  c                   if        %found(tabel00f)
051900010621     c                   movel     tbluni        descrare
052000010621 e4  c                   endif
052100010621     c                   if        sav_dis <> *blanks
052200010621     c                   write     or41t1
052300010621     c                   write     or41t2
052400010621     c                   endif
052500010621     c                   write     or41t3
052600010621     c                   write     or41t4
052700010621     c                   eval      sav_dis = p_rordis
052800010621     c                   eval      sav_are = p_rorare
052900010622     c                   clear                   sav_poel
053000031002     c                   clear                   sav_trc
053100010621 e3  c                   endif
053200010621      * rottura per area
053300010621 b3  c                   if        sav_are <> p_rorare
053400010621      * area
053500010621     c                   clear                   descrare
053600010621     c                   movel     '05'          tblcod
053700010621     c                   movel(P)  p_rorare      tblkey
053800010621     c     ktab          chain     tabel00f
053900010621 b4  c                   if        %found(tabel00f)
054000010621     c                   movel     tbluni        descrare
054100010621 e4  c                   endif
054200010621     c                   if        sav_are <> *zeros
054300010621     c                   write     or41t1
054400010621     c                   write     or41t2
054500010621     c                   endif
054600010621     c                   write     or41t3
054700010621     c                   write     or41t4
054800010621     c                   eval      sav_are = p_rorare
054900010622     c                   clear                   sav_poel
055000031002     c                   clear                   sav_trc
055100010621 e3  c                   endif
055200030922      * rottura per tipo record
055300030922      * se richiesta la stampa analitica
055400030922      * dopo la pagina dei totali x p.o. devo stampare i commissionati
055500030922      * se richiesta la stampa sintetica
055600030922      * dopo la riga dei totali x p.o. devo stampare i commissionati
055700030922 b3  c                   if        sav_trc <> p_rortrc
055800030922      * se non è il primo tipo record che leggo (quindi non è quello totale)
055900030922      * pulisco il sav_poel in modo da avere la rottura x p.o.
056000030922      * e stampare i commissionati in una pagina nuova
056100031002 b4  c                   If        sav_trc <> *Blanks
056200031002 b5  c                   If        i40tpst = 'A'
056300030922      * salto pagina
056400030922     c                   write     or41t1
056500030922     c                   write     or41t2
056600030922     c                   write     or41t3
056700030922     c                   write     or41t4
056800031002 e5  c                   EndIf
056900030922     c                   Clear                   sav_poel
057000031002 e4  c                   EndIf
057100030922     c                   Eval      sav_trc = p_rortrc
057200030922 e3  c                   EndIf
057300010621      * rottura per p.o.
057400010621 b3  c                   if        sav_poel <> p_rorpoe
057500010614      * decodifico il p.o. che sto elaborando
057600010621     c                   eval      kazorg = p_rorpoe
057700010614     c     kazorg        chain     azorg01l
057800010621 b4  c                   if        %found(azorg01l)
057900010614     c                   movel     orgdes        despoel
058000010621 e4  c                   endif
058100010614     ?* stampa sintetica
058200010621 b4  c                   if        i40tpst = 'S'
058300010614      * salto pagina
058400010621 b5  c                   if        *in90 = *on
058500010621     c                   write     or41t1
058600010621     c                   write     or41t2
058700010621     c                   write     or41t3
058800010621     c                   write     or41t4
058900010614     c                   eval      *in90 = *off
059000010621 e5  c                   endif
059100030922      * calcolo la percentuale dei non eseguiti sui da eseguire
059200030922     c                   clear                   perone
059300030922     c                   if        P_rorode > *zeros
059400030922     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
059500030922     c                   endif
059600030922      * stampo totali p.o.
059700030922     c  n15              write     or41d1
059800030922     c   15              write     or41d1b
059900010621 e4  c                   endif
060000010622     ?* stampa analitica
060100010622 b4  c                   if        i40tpst = 'A'
060200010622      * salto pagina
060300010622 b5  c                   if        sav_poel <> *zeros
060400010622     c                   write     or41t1
060500010622     c                   write     or41t2
060600010622     c                   write     or41t3
060700010622     c                   write     or41t4
060800010622 e5  c                   endif
060900010614      * calcolo la percentuale dei non eseguiti sui da eseguire
061000010625     c                   clear                   perone
061100010625     c                   if        P_rorode > *zeros
061200010621     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
061300010625     c                   endif
061400010622      * stampo totali p.o.
061500010614     c                   write     or41d1
061600010622      * stampo dettaglio
061700010614      *                            =======
061800010614     c                   exsr      SR_DETT
061900010614      *                            =======
062000010621 e4  c                   endif
062100010621     c                   eval      sav_poel = p_rorpoe
062200010621 e3  c                   endif
062300010621
062400010621 e2  c                   endif
062500010621
062600010621 e1  c                   enddo
062700010621
062800010621      * fine stampa
062900010621     c                   write     or41e1
063000010614
063100010614     c                   endsr
063200010614
063300010614      *--------------------------------------------------------------*
063400010614      * STAMPO IL DETTAGLIO
063500010614      *--------------------------------------------------------------*
063600010614     c     SR_DETT       begsr
063700010614
063800010614     c                   eval      wokstpcau = *off
063900010614
064000010614      * affidabilità
064100010614     c                   write     or41a1
064200010614
064300010621     c                   if        p_rordan > 0 or p_rorean > 0 or
064400010621     c                             p_roraan > 0 or p_rornan > 0
064500010614     c                   write     or41a11
064600010614     c                   endif
064700010621     c                   if        p_rordit > 0 or p_roreit > 0 or
064800010621     c                             p_rorait > 0 or p_rornit > 0
064900010614     c                   write     or41a12
065000010614     c                   endif
065100010621     c                   if        p_rordr1 > 0 or p_rorer1 > 0 or
065200010621     c                             p_rorar1 > 0 or p_rornr1 > 0
065300010614     c                   write     or41a13
065400010614     c                   endif
065500010621     c                   if        p_rordr2 > 0 or p_rorer2 > 0 or
065600010621     c                             p_rorar2 > 0 or p_rornr2 > 0
065700010614     c                   write     or41a14
065800010614     c                   endif
065900010621     c                   if        p_rordro > 0 or p_rorero > 0 or
066000010621     c                             p_roraro > 0 or p_rornro > 0
066100010614     c                   write     or41a15
066200010614     c                   endif
066300031105
066400031105      * O.R.M. non abbinati
066500031105     c                   If        p_rora09 > *Zeros
066600031105     c                   write     or41a29
066700031105     c                   endif
066800010614
066900010614      * causali es.anomali
067000030919     c                   if        p_rora01 > 0 or p_rora02 > 0 or
067100010621     c                             p_rora03 > 0 or p_rora04 > 0 or
067200010621     c                             p_rora05 > 0 or p_rora06 > 0 or
067300010621     c                             p_rora07 > 0 or p_rora08 > 0 or
067400010621     c                             p_rora09 > 0 or p_rora10 > 0 or
067500010621     c                             p_rora11 > 0 or p_rora12 > 0 or
067600010621     c                             p_rora13 > 0 or p_rora14 > 0 or
067700030919     c                             p_rora15 > 0
067800010614     c                   eval      wokstpcau = *on
067900010614     c                   write     or41a2
068000030919     c                   if        p_rora01 > 0
068100010614     c                   write     or41a21
068200010614     c                   endif
068300030919     c                   if        p_rora02 > 0
068400010614     c                   write     or41a22
068500010614     c                   endif
068600030919     c                   if        p_rora03 > 0
068700010614     c                   write     or41a23
068800010614     c                   endif
068900030919     c                   if        p_rora04 > 0
069000010614     c                   write     or41a24
069100010614     c                   endif
069200030919     c                   if        p_rora05 > 0
069300010614     c                   write     or41a25
069400010614     c                   endif
069500030919     c                   if        p_rora06 > 0
069600010614     c                   write     or41a26
069700010614     c                   endif
069800030919     c                   if        p_rora07 > 0
069900010614     c                   write     or41a27
070000010614     c                   endif
070100030919     c                   if        p_rora08 > 0
070200010614     c                   write     or41a28
070300010614     c                   endif
070400030919     c                   if        p_rora10 > 0
070500010614     c                   write     or41a210
070600010614     c                   endif
070700030919     c                   if        p_rora11 > 0
070800010614     c                   write     or41a211
070900010614     c                   endif
071000030919     c                   if        p_rora12 > 0
071100010614     c                   write     or41a212
071200010614     c                   endif
071300030919     c                   if        p_rora13 > 0
071400010614     c                   write     or41a213
071500010614     c                   endif
071600030919     c                   if        p_rora14 > 0
071700010614     c                   write     or41a214
071800010614     c                   endif
071900030919     c                   if        p_rora15 > 0
072000010614     c                   write     or41a215
072100010614     c                   endif
072200010614     c                   endif
072300010614
072400010614      * causali non eseguiti
072500010621     c                   if        p_rorn01 > 0 or p_rorn02 > 0 or
072600010621     c                             p_rorn03 > 0 or p_rorn04 > 0 or
072700010621     c                             p_rorn05 > 0 or p_rorn06 > 0 or
072800010621     c                             p_rorn07 > 0 or p_rorn08 > 0 or
072900030916     c                             p_rorn09 > 0 or p_rorn10 > 0 or
073000030916     c                             p_rorn01d > 0 or p_rorn02d > 0 or
073100030916     c                             p_rorn03d > 0 or p_rorn04d > 0 or
073200030916     c                             p_rorn05d > 0 or p_rorn06d > 0 or
073300030916     c                             p_rorn07d > 0 or p_rorn08d > 0 or
073400030916     c                             p_rorn09d > 0 or p_rorn10d > 0
073500010614     c                   if        wokstpcau = *on
073600010614     c                   write     or41a3
073700010614     c                   else
073800010614     c                   write     or41a2
073900010614     c                   endif
074000030916     c                   if        p_rorn01 > 0 or p_rorn01d > 0
074100010614     c                   write     or41a31
074200010614     c                   endif
074300030916     c                   if        p_rorn02 > 0 or p_rorn02d > 0
074400010614     c                   write     or41a32
074500010614     c                   endif
074600030916     c                   if        p_rorn03 > 0 or p_rorn03d > 0
074700010614     c                   write     or41a33
074800010614     c                   endif
074900030916     c                   if        p_rorn04 > 0 or p_rorn04d > 0
075000010614     c                   write     or41a34
075100010614     c                   endif
075200030916     c                   if        p_rorn05 > 0 or p_rorn05d > 0
075300010614     c                   write     or41a35
075400010614     c                   endif
075500030916     c                   if        p_rorn06 > 0 or p_rorn06d > 0
075600010614     c                   write     or41a36
075700010614     c                   endif
075800030916     c                   if        p_rorn07 > 0 or p_rorn07d > 0
075900010614     c                   write     or41a37
076000010614     c                   endif
076100030916     c                   if        p_rorn08 > 0 or p_rorn08d > 0
076200010614     c                   write     or41a38
076300010614     c                   endif
076400030916     c                   if        p_rorn09 > 0 or p_rorn09d > 0
076500010614     c                   write     or41a39
076600010614     c                   endif
076700030916     c                   if        p_rorn10 > 0 or p_rorn10d > 0
076800010614     c                   write     or41a310
076900010614     c                   endif
077000010614     c                   endif
077100010614
077200030919      * queste righe non le devo stampare x i soli ORM commissionati
077300030919     c                   If        not *In15
077400010614      * commissionati
077500010621     c                   if        p_roroco > 0
077600010614     c                   write     or41a4
077700010614     c                   endif
077800010614
077900010614      * tipo cliente
078000010614     c                   write     or41a5
078100010621     c                   if        p_rorico > 0
078200010614     c                   write     or41a51
078300010614     c                   endif
078400010621     c                   if        p_rorinc > 0
078500010614     c                   write     or41a52
078600010614     c                   endif
078700010614
078800010614      * tipo comunicazione
078900010614     c                   write     or41a6
079000030916     c                   if        p_rorifi > 0 or p_rorifian > 0 or
079100030916     c                             p_rorifid > 0 or p_rorifine  > 0
079200010614     c                   write     or41a61
079300010614     c                   endif
079400030916     c                   if        p_rorice > 0 or p_roricean > 0 or
079500030916     c                             p_roriced > 0 or p_roricene  > 0
079600010614     c                   write     or41a62
079700010614     c                   endif
079800030916     c                   if        p_roriin > 0 or p_roriinan > 0 or
079900030916     c                             p_roriind > 0 or p_roriinne  > 0
080000010614     c                   write     or41a63
080100010614     c                   endif
080200030916     c                   if        p_roriva > 0 or p_rorivaan > 0 or
080300030916     c                             p_rorivad > 0 or p_rorivane  > 0
080400010614     c                   write     or41a64
080500010614     c                   endif
080600030916     c                   if        p_roricc > 0 or p_roriccan > 0 or
080700030916     c                             p_roriccd > 0 or p_roriccne  > 0
080800010614     c                   write     or41a65
080900010614     c                   endif
081000030919
081100030919     c                   EndIF
081200010614
081300010614     c                   endsr
081400010614
081500020807      *--------------------------------------------------------------*
081600020807      * SQL  - Preparazione sql FNORM00F
081700020807      *--------------------------------------------------------------*
081800020807     c     SqlOrmPos     BegSr
081900020807
082000020808      * Preparo la stringa sql
082100020808     c                   Eval      StringaSql =
082200020808     c                             'select * '                                 +
082300020808     c                             'from fnorm00f where ormpoe in ('
082400020808      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
082500020808     c                   Do        250           Xa
082600020808     c                   If        Skpoel(Xa) = *Zeros
082700020808     c                   Leave
082800020808     c                   EndIf
082900020808     c                   Eval      StringaSql =
083000020808     c                             %trim(StringaSql) +  ' ' +
083100020808     c                             %editc(Skpoel(Xa):'1') + ','
083200020808     c                   EndDo
083300020808     c                   Eval      StringaSql =
083400020808     c                             %trim(StringaSql) + ') or ormpor in ('
083500020808      * ciclo sulla sk p.o. da elaborare per i p.o. ritiro
083600020808     c                   Do        250           Xa
083700020808     c                   If        Skpoel(Xa) = *Zeros
083800020808     c                   Leave
083900020808     c                   EndIf
084000020808     c                   Eval      StringaSql =
084100020808     c                             %trim(StringaSql) + ' ' +
084200020808     c                             %editc(Skpoel(Xa):'1') + ','
084300020808     c                   EndDo
084400020808     c                   Eval      StringaSql =
084500020808     c                             %trim(StringaSql) + ') for fetch only'
084600020807
084700020807     C/EXEC SQL
084800020808     C+ prepare S1 from :StringaSql
084900020807     C/END-EXEC
085000020807
085100020808     C/EXEC SQL
085200020808     C+ declare C1 cursor for S1
085300020808     C/END-EXEC
085400020807
085500020807     c                   EndSr
085600020807
085700010604      *--------------------------------------------------------------*
085800020808      * LEGGO IL FILE DEGLI ORM DA SQL
085900010604      *--------------------------------------------------------------*
086000010614     c     SR_LEGGI      begsr
086100010604
086200020808     C/EXEC SQL
086300020808     C+ Open C1
086400020808     C/END-EXEC
086500020808
086600020808     C/EXEC SQL
086700020808     C+ Fetch C1 into :fnormxxds
086800020808     C/END-EXEC
086900020808
087000020808 b1  c                   Dow       Sqlcod = 0                                   *record letto
087100010608      * no gli annullati
087200020808 b2  c                   If        Ormatb = *Blanks
087300010608
087400020808      * data immissione ORM non deve essere superiore alla data elaborazione
087500020808 b3  c                   If        Ormdao <= Dataela
087600030321
087700030321      * se sono in filiale elaboro tutto, se sono in sede devo controllare anche la
087800030321      * data pulizia ORM di filiale
087900030321     c                             and (I40Sede = 'F'  or
088000030321     c                                 (I40Sede <> 'F' and Ormdao >= I40dtpo))
088100031002
088200031002      * controllo se posso elaborare la serie
088300031002     c                   Move      OrmPoe        wOrmPoe
088400130118     c*******            Move      OrmNsr        wOrmNsr
088500130118     c                   eval      *in99 = *off
088600130118     c                   IF        ORMnsr > 0
088700130118     c                   eval      wORMcor =
088800130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
088900130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
089000130118     c                   IF        xx = 0
089100130118     c                   eval      *in99 = *on
089200130118     c                   ENDIF
089300130118     c                   IF        skstaosr(xx) <> *blanks
089400130118     c                   eval      *in99 = *on
089500130118     c                   ENDIF
089600130118     c                   ENDIF
089700130118     c*****wosr          Lookup    skosr                                  99
089800031002 b4  c                   If        Not *In99
089900020808
090000020808      *                            ==========
090100020808     c                   ExSr      Sr_Elabora
090200020808      *                            ==========
090300031002 b4  c                   EndIf
090400020808 b3  c                   EndIf
090500020808 b2  c                   EndIf
090600020808
090700020808     C/EXEC SQL
090800020808     C+ Fetch C1 into :fnormxxds
090900020808     C/END-EXEC
091000020808
091100020808 b1  c                   EndDo
091200020808
091300020808     C/EXEC SQL
091400020808     C+ close C1
091500020808     C/END-EXEC
091600020808
091700020808     c                   EndSr
091800010614
091900020808      *--------------------------------------------------------------*
092000020808      * LEGGO IL FILE DEGLI ORM
092100020808      *--------------------------------------------------------------*
092200020808     c     Sr_LeggiOrm   BegSr
092300020808
092400020808     c     *Start        Setll     Fnorm00f
092500020808     c                   Do        *Hival
092600020808     c                   Read      Fnorm00f
092700020808      * Fine file
092800020808 b1  c                   If        %Eof(Fnorm00f)
092900020808     c                   Leave
093000020808     c                   EndIf
093100020808      * no gli annullati
093200020808 b2  c                   If        Ormatb = *Blanks
093300020808
093400020808      * data immissione ORM non deve essere superiore alla data elaborazione
093500020808 b3  c                   If        Ormdao <= Dataela
093600030321
093700030321      * e controllo anche la data pulizia ORM di filiale
093800030321     c                             and Ormdao >= I40dtpo
093900031002
094000031002      * controllo se posso elaborare la serie
094100031002     c                   Move      OrmPoe        wOrmPoe
094200130118     c*******            Move      OrmNsr        wOrmNsr
094300130118     c                   eval      *in99 = *off
094400130118     c                   IF        ORMnsr > 0
094500130118     c                   eval      wORMcor =
094600130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
094700130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
094800130118     c                   IF        xx = 0
094900130118     c                   eval      *in99 = *on
095000130118     c                   ENDIF
095100130118     c                   IF        skstaosr(xx) <> *blanks
095200130118     c                   eval      *in99 = *on
095300130118     c                   ENDIF
095400130118     c                   ENDIF
095500130118     c*****wosr          Lookup    skosr                                  99
095600031002 b4  c                   If        Not *In99
095700020808
095800020808      *                            ==========
095900020808     c                   ExSr      Sr_Elabora
096000020808      *                            ==========
096100031002 b4  c                   EndIf
096200020808 b3  c                   EndIf
096300020808 b2  c                   EndIf
096400020808
096500020808 b1  c                   EndDo
096600020808
096700020808     c                   EndSr
096800020808
096900020808      *--------------------------------------------------------------*
097000020808      * ELABORO I DATI LETTI
097100020808      *--------------------------------------------------------------*
097200020808     c     Sr_Elabora    BegSr
097300020808
097400020808      * campi di appoggio
097500020808     c                   eval      xf = 1
097600020808     c                   eval      wpoe = *off
097700020808     c                   eval      wpor = *off
097800020808     c                   eval      wokstp = *off
097900020808     c                   eval      wok900 = *off
098000020808     c                   eval      wok600 = *off
098100020808     c                   eval      wokrode = *off
098200020808     c                   eval      wokroes = *off
098300020808     c                   eval      wokrfile = *off
098400020808     c                   eval      wokfestae = *off
098500020808     c                   eval      wokfestar = *off
098600020808
098700020808     c                   clear                   cauone
098800020808     c                   clear                   cauoea
098900030912
099000030912     c                   Eval      dorm01 = OrmFlo
099100030912      * ORM commissionato
099200030918     c                   Eval      *in14 = (§OrCom = 'S')
099300020808
099400020808      * controllo se il po emissione è nella sk dei p.o. da elaborare
099500020808     c                   eval      xa = 1
099600020808     c     Ormpoe        Lookup    Skpoel(Xa)                             30
099700020808     c                   If        *In30
099800020808     c                   eval      wpoe = *on
099900020808     c                   eval      dspoele = skpoel(xa)
100000020808     c                   endif
100100020808
100200020808      * controllo se il po ritiro è nella sk dei p.o. da elaborare
100300020808     c                   eval      xa = 1
100400020808     c     Ormpor        Lookup    Skpoel(Xa)                             30
100500020808     c                   If        *In30
100600020808     c                   eval      wpor = *on
100700020808     c                   eval      dspoelr = skpoel(xa)
100800020808     c                   endif
100900020808
101000020808      * se la data che sto elaborando è un festivo non devo calcolare
101100020808      *                            ========
101200020808     c                   exsr      SR_FESTA
101300020808      *                            ========
101400020808
101500020808     c                   if        wokfestae = *off and
101600020808     c                             wokfestar = *off
101700020808     c                   eval      dsdata = dataela
101800020808     c                   endif
101900020808
102000020808      * calcolo le date per affidabilità
102100020808      *                            =======
102200020808     c                   exsr      SR_DATA
102300020808      *                            =======
102400020808
102500020808      * pulisco i campi di comodo
102600020808      *                            ==========
102700020808     c                   exsr      SR_PULISCI
102800020808      *                            ==========
102900020808
103000020808      * faccio i controlli con il p.o. emissione
103100020808 b2  c                   if        wpoe = *on
103200020808      * se festa non faccio niente
103300020808 b3  c                   if        wokfestae = *off
103400020808      *                            ======
103500020808     c                   exsr      SR_POE
103600020808      *                            ======
103700020808 e3  c                   endif
103800020808 e2  c                   endif
103900020808
104000020808      * faccio i controlli con il p.o. ritiro
104100020808 b2  c                   if        wpor = *on
104200020808      * se festa non faccio niente
104300020808 b3  c                   if        wokfestae = *off
104400020808
104500020808      *                            ======
104600020808     c                   exsr      SR_POR
104700030916
104800030916 b4  c                   if        wokrfile = *on
104900020808      *                            ======
105000030918      * RECORD 'A'
105100020808      * controllo se esiste già un elemento di sk = alla mia key
105200020808      * e carico la sk relativa al file
105300020808     c                   eval      dspoel = dspoelr
105400030912     c                   eval      dspoel1 = dspoelr
105500030912     c                   eval      dsdata1 = dsdata
105600030918     c                   Eval      dstrc = 'A'
105700020808      *                            ==========
105800020808     c                   exsr      SR_CONTRSK
105900020808      *                            ==========
106000020808      * carico la sk del file
106100020808      *                            =========
106200020808     c                   exsr      SR_CARICA
106300020808      *                            =========
106400030918      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
106500030918     c                   Eval      sxf = xf
106600030918      *
106700030918      * RECORD 'B' ORM COMMISSIONATI
106800030912      * controllo se esiste già un elemento di sk = alla mia key
106900030912      * e carico la sk relativa al file
107000030916 b5  c                   If        *In14
107100030918     c                   Eval      dstrc = 'B'
107200030912      *                            ==========
107300030912     c                   exsr      SR_CONTRSK
107400030912      *                            ==========
107500030918      * carico la sk del file x record 'B' orm commissionato
107600030912      *                            =========
107700030912     c                   exsr      SR_CARICA
107800030912      *                            =========
107900030916 e5  c                   EndIf
108000020808 e4  c                   endif
108100020808 e3  c                   endif
108200020808 e2  c                   endif
108300030916      * ORM commissionato
108400030916     c                   If        *In14
108500030916     c                   Eval      commis = 'X'
108600030916     c                   Else
108700030916     c                   Clear                   commis
108800030916     c                   EndIf
108900020808
109000020808     ?* stampa di comodo EDP
109100020808 b2  c   10              if        wokstp = *on
109200030918     c                   eval      dswfror = rfile(sxf)
109300020808      * rottura di data
109400020808 b3  c                   if        dsdata <> sav_dsdatap
109500020808     c                   clear                   wlbdat
109600020808     c                   move      dsdata        g02inv
109700020808     c                   movel     '3'           G02err
109800020808     c                   call      'XSRDA8'
109900020808     c                   parm                    wlbdat
110000020808     c                   movel     g02dat        dsdatap
110100020808      * decodifico il p.o.
110200020808     c                   eval      kazorg = dspoel
110300020808     c     kazorg        chain     azorg01l
110400020808 b4  c                   if        %found(azorg01l)
110500020808     c                   movel     orgdes        despoel
110600020808 e4  c                   endif
110700020808     c                   write     or41t1p
110800020808     c                   eval      sav_dsdatap = dsdata
110900020808     c                   eval      sav_dspoelp = dspoel
111000020808 e3  c                   endif
111100020808      * rottura di p.o.
111200020808 b3  c                   if        dspoel <> sav_dspoelp
111300020808      * decodifico il p.o.
111400020808     c                   eval      kazorg = dspoel
111500020808     c     kazorg        chain     azorg01l
111600020808 b4  c                   if        %found(azorg01l)
111700020808     c                   movel     orgdes        despoel
111800020808 e4  c                   endif
111900020808     c                   write     or41t1p
112000020808     c                   eval      sav_dspoelp = dspoel
112100020808 e3  c                   endif
112200020808      * salto pagina
112300020808 b3  c                   if        *in91 = *on
112400020808     c                   write     or41t1p
112500020808     c                   eval      *in91 = *off
112600020808 e3  c                   endif
112700020808      * dettaglio
112800020808     c                   write     or41d1p
112900020808 e2  c                   endif
113000020808
113100020808     ?* stampa di controllo
113200020808 b2  c   11              if        wokstp = *on
113300030918     c                   eval      dswfror = rfile(sxf)
113400020808      * rottura di data
113500020808 b3  c                   if        dsdata <> sav_dsdatac
113600020808     c                   clear                   wlbdat
113700020808     c                   move      dsdata        g02inv
113800020808     c                   movel     '3'           G02err
113900020808     c                   call      'XSRDA8'
114000020808     c                   parm                    wlbdat
114100020808     c                   movel     g02dat        dsdatap
114200020808      * decodifico il p.o.
114300020808     c                   eval      kazorg = dspoel
114400020808     c     kazorg        chain     azorg01l
114500020808 b4  c                   if        %found(azorg01l)
114600020808     c                   movel     orgdes        despoel
114700020808 e4  c                   endif
114800020808     c                   write     or41t1c
114900020808     c                   eval      sav_dsdatac = dsdata
115000020808     c                   eval      sav_dspoelc = dspoel
115100020808 e3  c                   endif
115200020808      * rottura di p.o.
115300020808 b3  c                   if        dspoel <> sav_dspoelc
115400020808      * decodifico il p.o.
115500020808     c                   eval      kazorg = dspoel
115600020808     c     kazorg        chain     azorg01l
115700020808 b4  c                   if        %found(azorg01l)
115800020808     c                   movel     orgdes        despoel
115900020808 e4  c                   endif
116000020808     c                   write     or41t1c
116100020808     c                   eval      sav_dspoelc = dspoel
116200020808 e3  c                   endif
116300020808      * salto pagina
116400020808 b3  c                   if        *in92 = *on
116500020808     c                   write     or41t1c
116600020808     c                   eval      *in92 = *off
116700020808 e3  c                   endif
116800020808 b3  c                   if        w_rorone <> *blanks or
116900020808     c                             w_rornqd <> *blanks or
117000020808     c                             w_rornbl <> *blanks or
117100020808     c                             w_rormdr <> *blanks or
117200031105     c                             w_rornas <> *blanks
117300020808      * giro data immissione ORM
117400020808     c                   clear                   wlbdat
117500020808     c                   move      ormdao        g02inv
117600020808     c                   movel     '3'           G02err
117700020808     c                   call      'XSRDA8'
117800020808     c                   parm                    wlbdat
117900020808     c                   movel     g02dat        w_ormdao
118000020808      * dettaglio
118100020808     c                   write     or41d1c
118200020808 e3  c                   endif
118300020808 e2  c                   endif
118400020808
118500020808     c                   EndSr
118600020808
118700010612      *--------------------------------------------------------------*
118800010612      * CONTROLLO SE LA DATA CHE STO ELABORANDO E' FESTIVO
118900010612      *--------------------------------------------------------------*
119000010614     c     SR_FESTA      begsr
119100010612
119200010612     c                   eval      wokfestae = *off
119300010612     c                   eval      wokfestar = *off
119400010612
119500010612     c     *iso          movel     dataela       wdtda
119600010612     c                   extrct    wdtda:*y      kann
119700010612     c                   extrct    wdtda:*m      kmes
119800010612     c                   extrct    wdtda:*d      wgiorno
119900011217     c                   clear                   ktfa
120000010612      * cerco per il p.o. emissione
120100010612     c                   if        wpoe = *on
120200011217     c                   eval      ktfp = dspoele
120300010612     c     kazcln        chain     azcln01l
120400010612     c                   if        %found(azcln01l) and
120500010612     c                             (MAT(wgiorno) =  'F'
120600010612     c                                    or  POM(wgiorno) =  'F')
120700010612     c                   eval      wokfestae = *on
120800010612     c                   endif
120900010612     c                   endif
121000010612
121100010612      * cerco per il p.o. ritiro
121200010612     c                   if        wpor = *on
121300011217     c                   eval      ktfp = dspoelr
121400010612     c     kazcln        chain     azcln01l
121500010612     c                   if        %found(azcln01l) and
121600010612     c                             (MAT(wgiorno) =  'F'
121700010612     c                                    or  POM(wgiorno) =  'F')
121800010612     c                   eval      wokfestar = *on
121900010612     c                   endif
122000010612     c                   endif
122100010612
122200010612     c                   endsr
122300010614
122400010608      *--------------------------------------------------------------*
122500010608      * CALCOLO DATE X AFFIDABILITA'
122600010608      *--------------------------------------------------------------*
122700010614     c     SR_DATA       begsr
122800010608
122900010612      * calcolo la data da - 1
123000010611     c     *iso          movel     dataela       wdtda
123100010608     c                   do        *hival
123200010608     c                   subdur    1:*d          wdtda
123300010608     c                   extrct    wdtda:*y      kann
123400010608     c                   extrct    wdtda:*m      kmes
123500010608     c                   extrct    wdtda:*d      wgiorno
123600011217     c                   clear                   ktfa
123700020808     c                   If        Dspoelr <> *Zeros
123800020808     c                   eval      ktfp = Dspoelr
123900020808     c                   Else
124000020808     c                   Eval      Ktfp = Dspoele
124100020808     c                   EndIf
124200010608     c     kazcln        chain     azcln01l
124300010608     c                   if        not%found(azcln01l)
124400010608     c                   leave
124500010608     c                   endif
124600010608     c                   if        MAT(wgiorno) =  'F'
124700010608     c                             or  POM(wgiorno) =  'F'
124800010608     c                   iter
124900010608     c                   endif
125000010608     c                   leave
125100010608     c                   enddo
125200010611     c                   move      wdtda         wdtda01
125300010608
125400010608      * calcolo la data da - 2
125500010611     c     *iso          movel     wdtda01       wdtda
125600010608     c                   do        *hival
125700010611     c                   subdur    1:*d          wdtda
125800010608     c                   extrct    wdtda:*y      kann
125900010608     c                   extrct    wdtda:*m      kmes
126000010608     c                   extrct    wdtda:*d      wgiorno
126100011217     c                   clear                   ktfa
126200020808     c                   If        Dspoelr <> *Zeros
126300020808     c                   eval      ktfp = Dspoelr
126400020808     c                   Else
126500020808     c                   Eval      Ktfp = Dspoele
126600020808     c                   EndIf
126700010608     c     kazcln        chain     azcln01l
126800010608     c                   if        not%found(azcln01l)
126900010608     c                   leave
127000010608     c                   endif
127100010608     c                   if        MAT(wgiorno) =  'F'
127200010608     c                             or  POM(wgiorno) =  'F'
127300010608     c                   iter
127400010608     c                   endif
127500010608     c                   leave
127600010608     c                   enddo
127700010611     c                   move      wdtda         wdtda02
127800010608
127900010608     c                   endsr
128000010614
128100010605      *--------------------------------------------------------------*
128200010605      * CONTROLLO CON IL PO EMISSIONE
128300010605      *--------------------------------------------------------------*
128400010605     c     SR_POE        begsr
128500010620
128600010620      * se la data del p.o. emissione non è festa controllare se il giorno prima
128700010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
128800010620 b1  c                   if        wokfestae = *off
128900010620     c                   eval      dspoel = dspoele
129000010620      *                            ========
129100010620     c                   exsr      SR_DTFES
129200010620      *                            ========
129300010612
129400010620 b2  c                   if        ormdao = dataela or
129500010620     c                             (wdatafes > 0 and ormdao >= wdatafes and
129600010620     c                              ormdao < dataela)
129700010611     c                   eval      dspoel = dspoele
129800030912     c                   eval      dspoel1= dspoele
129900030912     c                   eval      dsdata1 = dsdata
130000030918     c                   Eval      dstrc = 'A'
130100010608      * controllo se esiste già un elemento di sk = alla mia key
130200010608      * e carico la sk relativa al file
130300010614      *                            ==========
130400010608     c                   exsr      SR_CONTRSK
130500010614      *                            ==========
130600010605
130700010605     ?* immessi ?
130800010605     c                   eval      f_roroim = f_roroim +1
130900010605     ?* commissionati ?
131000010620 b3  c                   if        ormpoe <> ormpor
131100010605     c                   eval      f_roroco = f_roroco +1
131200010620 e3  c                   endif
131300010605     ?* codificati ?
131400010620 b3  c                   if        ormcra <> *zeros
131500010605     c                             or ormcor <> *zeros
131600010605     c                   eval      f_rorico = f_rorico +1
131700010605     ?* non codificati ?
131800010620 x3  c                   else
131900010605     c                   eval      f_rorinc = f_rorinc +1
132000010620 e3  c                   endif
132100010605     ?* fissi ?
132200010620 b3  c                   if        ormtco = 'S'
132300010605     c                   eval      f_rorifi = f_rorifi +1
132400010620 e3  c                   endif
132500010605     ?* da centralino ?
132600010620 b3  c                   if        ormtco = 'M'  or
132700010606     c                             ormtco = 'P'
132800010605     c                   eval      f_rorice = f_rorice +1
132900010620 e3  c                   endif
133000010605     ?* da internet ?
133100010620 b3  c                   if        ormtco = 'I'
133200010605     c                   eval      f_roriin = f_roriin +1
133300010620 e3  c                   endif
133400010605     ?* da vas ?
133500010620 b3  c                   if        ormtco = 'F'
133600010605     c                   eval      f_roriva = f_roriva +1
133700010620 e3  c                   endif
133800010605     ?* da call center ?
133900010620 b3  c                   if        ormtco = 'C'
134000010605     c                   eval      f_roricc = f_roricc +1
134100010620 e3  c                   endif
134200010605
134300010612     c                   eval      rfile(xf) = dswfror
134400010607     c                   eval      wokstp = *on
134500031202      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
134600031202     c                   Eval      sxf = xf
134700010620 e2  c                   endif
134800010605
134900010620 e1  c                   endif
135000010620
135100010605     c                   endsr
135200010614
135300010620      *--------------------------------------------------------------*
135400010620      * CERCO IL PRIMO GIORNO FESTIVO PRECEDENTE
135500010620      *--------------------------------------------------------------*
135600010620     c     SR_DTFES      begsr
135700010620
135800010620      * calcolo il primo giorno festivo
135900010620     c     *iso          movel     dataela       wdtda
136000010620     c                   do        *hival
136100010620     c                   subdur    1:*d          wdtda
136200010620     c                   extrct    wdtda:*y      kann
136300010620     c                   extrct    wdtda:*m      kmes
136400010620     c                   extrct    wdtda:*d      wgiorno
136500011217     c                   clear                   ktfa
136600011217     c                   eval      ktfp = dspoel
136700010620     c     kazcln        chain     azcln01l
136800010620     c                   if        not%found(azcln01l)
136900010620     c                   leave
137000010620     c                   endif
137100010620     c                   if        MAT(wgiorno) =  'F'
137200010620     c                             or  POM(wgiorno) =  'F'
137300010620     c                   move      wdtda         wdatafes
137400010620     c                   iter
137500010620     c                   endif
137600010620     c                   leave
137700010620     c                   enddo
137800010620
137900010620     c                   endsr
138000010620
138100010608      *--------------------------------------------------------------*
138200010608      * PULISCO CAMPI DI COMODO
138300010608      *--------------------------------------------------------------*
138400010614     c     SR_PULISCI    begsr
138500010608
138600010608     c                   clear                   wrorode
138700010620     c                   clear                   wrordan
138800010608     c                   clear                   wrordit
138900010608     c                   clear                   wrordr1
139000010608     c                   clear                   wrordr2
139100010608     c                   clear                   wrordro
139200010608     c                   clear                   wroroes
139300010614     c                   clear                   wrorean
139400010608     c                   clear                   wroreit
139500010608     c                   clear                   wrorer1
139600010608     c                   clear                   wrorer2
139700010608     c                   clear                   wrorero
139800010608     c                   clear                   wrorone
139900010620     c                   clear                   wrornan
140000010608     c                   clear                   wrornit
140100010608     c                   clear                   wrornr1
140200010608     c                   clear                   wrornr2
140300010608     c                   clear                   wrornro
140400010608     c                   clear                   wrorn01
140500010608     c                   clear                   wrorn02
140600010608     c                   clear                   wrorn03
140700010608     c                   clear                   wrorn04
140800010608     c                   clear                   wrorn05
140900010608     c                   clear                   wrorn06
141000010608     c                   clear                   wrorn07
141100010608     c                   clear                   wrorn08
141200010608     c                   clear                   wrorn09
141300010608     c                   clear                   wrorn10
141400010608     c                   clear                   wroroea
141500010620     c                   clear                   wroraan
141600010608     c                   clear                   wrorait
141700010608     c                   clear                   wrorar1
141800010608     c                   clear                   wrorar2
141900010608     c                   clear                   wroraro
142000010608     c                   clear                   wrora01
142100010608     c                   clear                   wrora02
142200010608     c                   clear                   wrora03
142300010608     c                   clear                   wrora04
142400010608     c                   clear                   wrora05
142500010608     c                   clear                   wrora06
142600010608     c                   clear                   wrora07
142700010608     c                   clear                   wrora08
142800010608     c                   clear                   wrora09
142900010608     c                   clear                   wrora10
143000010608     c                   clear                   wrora11
143100010608     c                   clear                   wrora12
143200010608     c                   clear                   wrora13
143300010608     c                   clear                   wrora14
143400010608     c                   clear                   wrora15
143500010608     c                   clear                   wrornqd
143600010608     c                   clear                   wrornbl
143700010614     c                   clear                   wrormdr
143800010614     c                   clear                   wrornas
143900030912     c                   clear                   wroroned
144000030912     c                   clear                   wrorn01d
144100030912     c                   clear                   wrorn02d
144200030912     c                   clear                   wrorn03d
144300030912     c                   clear                   wrorn04d
144400030912     c                   clear                   wrorn05d
144500030912     c                   clear                   wrorn06d
144600030912     c                   clear                   wrorn07d
144700030912     c                   clear                   wrorn08d
144800030912     c                   clear                   wrorn09d
144900030912     c                   clear                   wrorn10d
145000030917     c                   clear                   wrorifian
145100030917     c                   clear                   wrorifine
145200030917     c                   clear                   wrorifid
145300030917     c                   clear                   wroricean
145400030917     c                   clear                   wroricene
145500030917     c                   clear                   wroriced
145600030917     c                   clear                   wroriinan
145700030917     c                   clear                   wroriinne
145800030917     c                   clear                   wroriind
145900030917     c                   clear                   wrorivaan
146000030917     c                   clear                   wrorivane
146100030917     c                   clear                   wrorivad
146200030917     c                   clear                   wroriccan
146300030917     c                   clear                   wroriccne
146400030917     c                   clear                   wroriccd
146500010620
146600010621     c                   clear                   w_rorone
146700010621     c                   clear                   w_rornqd
146800010621     c                   clear                   w_rornbl
146900010621     c                   clear                   w_rornas
147000010621     c                   clear                   w_rormdr
147100010608
147200010608     c                   endsr
147300010614
147400010605      *--------------------------------------------------------------*
147500010605      * CONTROLLO CON IL PO RITIRO
147600010605      *--------------------------------------------------------------*
147700010605     c     SR_POR        begsr
147800010620
147900010620     c                   clear                   wdatafes
148000010620
148100010620      * se la data del p.o. ritiro non è festa controllare se il giorno prima
148200010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
148300010620 b1  c                   if        wokfestar = *off
148400010620     c                   eval      dspoel = dspoelr
148500010620      *                            ========
148600010620     c                   exsr      SR_DTFES
148700010620      *                            ========
148800010620 e1  c                   endif
148900010608
149000010608     c                   eval      kormpoe = ormpoe
149100010608     c                   eval      kormnsr = ormnsr
149200010608     c                   eval      kormnor = ormnor
149300010608     c                   eval      kormnrv = ormnrv
149400010612
149500010614      * se data p.o. ritiro non è festa cerco gli ORM da eseguire
149600010619      * e le modifiche della data di ritiro
149700010613 b1  c                   if        wokfestar = *off
149800010606
149900010614     ?* ORM da eseguire oggi data ritiro = data elaborazione
150000010618 b2  c                   if        ormdar = dataela
150100010618     ?* data ultima fase < data elaborazione e già eseguito non devo elaborarlo
150200010618     ?*                                                     -------------------
150300010618     c                   if        ormdfo < dataela and ormfao >= 600
150400010618     c                   goto      endpor
150500010618     c                   endif
150600010618     c                   eval      wrorode = wrorode +1
150700010607     c                   eval      wrordit = wrordit +1
150800010607     c                   eval      wokstp = *on
150900010612     c                   eval      wokrfile = *on
151000010614 e2  c                   endif
151100010608
151200010614     ?* ORM da eseguire (non eseguiti entro oggi) data ritiro < data elaborazione
151300010613 b2  c                   if        ormdar < dataela
151400010608     c                   eval      wokrode = *off
151500010620
151600010620     c                   eval      kormdfo = ormdfo
151700010620     c                   eval      kormofo = ormofo
151800010620     c                   eval      kormfao = ormfao
151900010620     c     korf01        setll     fnorf01l
152000010620      * se la data ultima fase "testata" è >= della data elaborazione
152100010620      * o è <  della data elaborazione
152200010620      *   e >= alla data festiva (se sto elaborando un lunedì)
152300010620      * devo leggere la fase prima
152400010620     c                   select
152500010620 b3  c                   when      ormdfo >= dataela or
152600010620 b3  c                             (ormdfo < dataela  and
152700010620     c                             (wdatafes > 0 and ormdfo >= wdatafes))
152800010620     c     korf          readpe    fnorf01l
152900010620      * se la data ultima fase "testata" è <  della data elaborazione
153000010620      * e non sto elaborando un lunedì
153100010620      * devo leggere la fase stessa
153200010620     c                   other
153300010620     c     korf          reade     fnorf01l
153400010620 e3  c                   endsl
153500010620
153600030807      * senza fase 600-900-910-999 fino alla data elaborazione esclusa
153700010614 b3  c                   dow       not %eof(fnorf01l)
153800010620 b4  c                   if        orfdae < dataela
153900010620 b5  c                   if        orffar >=600
154000011019      * Attenzione!! E' possibile che sia stata fatta una modifica fase
154100011019      * se la fase della testata ORM è 500 o è 100 e la fase precedente
154200011019      * sul file delle fasi è una fase di fatto DEVO dare l'ORM come
154300011019      * da eseguire
154400011019     c                   if        ormfao <> 500 and ormfao <> 100
154500010608     c                   leave
154600011019     c                   endif
154700010614 e5  c                   endif
154800060926      * salto le fasi che non devono essere elaborate
154900060926     c     orffar        Lookup    skfar                                  30
155000060926     c**!!!              If        OrfFar <> 300
155100060926     c                   if        not *in30
155200010608     c                   eval      wokrode = *on
155300031105     c                   EndIf
155400010614 e4  c                   endif
155500010613     c     korf          readpe    fnorf01l
155600010614 e3  c                   enddo
155700010613     ?* da eseguire (in ritardo)
155800010613 b3  c                   if        wokrode = *on
155900010608     c                   eval      wrorode = wrorode +1
156000010608      * calcolo affidabilità. Uso >= per i da eseguire nei festivi
156100010614 b4  c                   select
156200010608     c                   when      ormdar >= wdtda01
156300010608     c                   eval      wrordr1 = wrordr1 +1
156400010608     c                   when      ormdar >= wdtda02
156500010608     c                   eval      wrordr2 = wrordr2 +1
156600010614 x4  c                   other
156700010608     c                   eval      wrordro = wrordro +1
156800010614 e4  c                   endsl
156900010608     c                   eval      wokstp = *on
157000010614     c                   eval      wokrfile = *on
157100010614     ?* è stato eseguito
157200010613 x3  c                   else
157300010614      * e la data ultima fase minore data elaborazione non devo elaborarlo
157400010614      *                                                -------------------
157500010620 b4  c                   if        ormdfo < dataela
157600010619     ?* ma prima controllo se deve andare a far parte dei non bollettati
157700010619     ?*                                                   --------------
157800010619 b5  c                   if        ormfao = 600 and
157900010619     c                             ormdfo >= wdtda01
158000010619     c                   eval      wrornbl = wrornbl +1
158100010621     c   11              eval      w_rornbl = 'X'
158200010619     c                   eval      wokstp = *on
158300010619     c                   eval      wokrfile = *on
158400010619 e5  c                   endif
158500010608     c                   goto      endpor
158600010614 e4  c                   endif
158700010613 e3  c                   endif
158800010613 e2  c                   endif
158900040420
159000040420     ?* ORM con data ritiro > data elaborazione
159100040420      * devo controllare che non sia stata fatta una fase di dirottamento dalla data elaborazione
159200040420      * in poi
159300040420if  2c                   If        OrmDar > Dataela
159400040420     c                   Eval      kormfao = 200
159500040420     c                   Eval      kormdfo = Dataela
159600040420     c     kOrf03        Setll     Fnorf03l
159700040420      * Se trovo già una fase di dirottamento in data elaborazione non devo elabore l'ORM
159800040420      *                                                            ----------------------
159900040420     c                   If        %Equal(Fnorf03l)
160000040420     c                   Goto      endpor
160100040420     c                   EndIf
160200040615      * Altrimenti leggo dalla data statistica in poi
160300040420do  3c                   Do        *Hival
160400040420     c     kOrf04        Reade     Fnorf03l
160500040420     c                   If        %Eof(Fnorf03l)
160600040420     c                   Leave
160700040420     c                   EndIf
160800040420      * Se trovo una fase di dirottamento non devo elabore l'ORM
160900040420      *                                   ----------------------
161000040420     c                   Goto      endpor
161100040420e   3c                   EndDo
161200040420e   2c                   EndIf
161300010618
161400010619      * cerco le modifiche alla data ritiro
161500010618      *                            =========
161600010618     c                   exsr      SR_MODRIT
161700010618      *                            =========
161800010619
161900010619 e1  c                   endif
162000010608
162100010614      * cerco sulle fasi sempre
162200010614      *                  ------
162300010614      *                            =======
162400010606     c                   exsr      SR_FASI
162500010614      *                            =======
162600010608
162700010608     c     endpor        endsr
162800010618
162900010618      *--------------------------------------------------------------*
163000010618      * CALCOLO MODIFICHE DATA RITIRO
163100010618      *--------------------------------------------------------------*
163200010618     c     SR_MODRIT     begsr
163300010618
163400010618     c                   eval      korvdtv = dataela
163500010620     c                   if        wdatafes > 0
163600010620     c                   eval      korvdtv = wdatafes
163700010620     c                   endif
163800010618     c                   clear                   sav_orvdar
163900010618
164000010620     c     korv          setll     fnorv01l
164100010618     c                   do        *hival
164200010620     c     korf          reade     fnorv01l
164300010618     c                   if        %eof(fnorv01l)
164400010618     c                   leave
164500010618     c                   endif
164600010620     c                   if        orvdtv > dataela
164700010620     c                   leave
164800010620     c                   endif
164900010620     c                   if        orvdar <> ormdar and
165000010620     c                             orvdar <> sav_orvdar
165100010618     c                   eval      wrormdr = wrormdr +1
165200010621     c   11              eval      w_rormdr = 'X'
165300010618     c                   eval      sav_orvdar = orvdar
165400010618     c                   eval      wokstp = *on
165500010618     c                   eval      wokrfile = *on
165600010618     c                   endif
165700010618     c                   enddo
165800010618
165900010618     c                   endsr
166000010618
166100010608      *--------------------------------------------------------------*
166200010608      * CONTROLLO LA SITUAZIONE DELL'ORM SULLE FASI
166300010608      *--------------------------------------------------------------*
166400010608     c     SR_FASI       begsr
166500010608
166600040421      * ATTENZIONE!!! la fase 200 di dirottato fa fermare la lettura delle fasi, appena
166700040421      *               la trovo esco dalla routine
166800040421
166900010608     c                   clear                   orfnor
167000010608     c                   clear                   orfpoe
167100010608     c                   clear                   orfpog
167200010608     c                   clear                   orfore
167300010608
167400010608     ?* se la data elaborazione è >= data ultima fase "testata"
167500010608     ?* mi posiziono sul file delle fasi con la data/ora/fase della
167600010608     ?* testata ORM
167700010614 b1  c                   if        dataela >= ormdfo
167800010608     c                   eval      kormdfo = ormdfo
167900010608     c                   eval      kormofo = ormofo
168000010608     c                   eval      kormfao = ormfao
168100010608     c     korf01        setll     fnorf01l
168200010608     c     korf01        reade     fnorf01l
168300010614 e1  c                   endif
168400010608     ?* se la data elaborazione è < data ultima fase  "testata"
168500010608     ?* mi posizione sul file delle fasi con la data elaborazione
168600030917     ?* fase = 0 e ora fase = a tutti '9' per leggere l'ultima fase fatta nel giorno
168700030917     ?* di elaborazione statistica
168800010614 b1  c                   if        dataela < ormdfo
168900010608     c                   eval      kormdfo = dataela
169000010608     c                   eval      kormofo = *all'9'
169100010608     c                   clear                   kormfao
169200010608     c     korf01        setgt     fnorf01l
169300010608     c     korf          readpe    fnorf01l
169400010705      * se la data di ritiro = alla data elaborazione e trovo subito il fine file
169500010705      * sulle fasi vuole dire che la prima fase è stata eseguita dopo la data elaborazione
169600010705      * quindi l'orm è non eseguito (es. orm copiati con data immissione e data ritiro = al
169700010705      * vecchio orm, ma la data della fase = alla data del giorno della copia, errore già
169800010705      * sistemato in gestione orm)
169900010705 b2  c                   if        %eof(fnorf01l)
170000010705     c                             and ormdar = dataela
170100010705     c                   eval      wrorone = wrorone +1
170200010717     c   11              eval      w_rorone = 'X'
170300010705     c                   eval      wrornit = wrornit +1
170400010705     c                   eval      wokstp = *on
170500010705     c                   eval      wokrfile = *on
170600010705 e2  c                   endif
170700010614 e1  c                   endif
170800030912      * _____________
170900010608      * leggo le fasi
171000030912      * -------------
171100010613 b1  c                   dow       not %eof(fnorf01l)
171200040421      * ____________________
171300040421      * Fase 200 devo uscire
171400040421      * --------------------
171500040618if  2c                   If        OrfFar = 200
171600040421     c                   Leave
171700040421e   2c                   EndIf
171800040421
171900010608     c                   eval      wok900 = *off
172000010615     c                   eval      wok600 = *off
172100030912      * _____________________________
172200010608      * data fase = data elaborazione
172300030912      * -----------------------------
172400010620 b2  c                   if        orfdae = dataela or
172500010620     c                             (wdatafes > 0 and orfdae >= wdatafes and
172600010620     c                              orfdae < dataela)
172700010614     ?* ESEGUITI
172800010614     ?* ORM eseguiti (fase 900) ?
172900010614 b3  c                   if        orffar = 900
173000010614      * devo controllare se c'è una fase 600 prima
173100030917      * il controllo è così:
173200030917      *     - se data fase < data elaborazione e fase >= 600 è stato fatto prima
173300030917      *       quindi lo devo saltare
173400030917      *     - se no vuol dire che è stato fatto in data elaborazione e lo do per fatto
173500040421      * qui fa solo UNA readpe quindi non faccio il controllo della fase 200
173600010614     c                   eval      wokroes = *off
173700010614     c     korf          readpe    fnorf01l
173800010615 b5  c                   if        orffar >= 600  and
173900010615     c                             orfdae < dataela
174000010615     c                   eval      wok600 = *on
174100030917 x5  c                   else
174200010614     c                   eval      wokroes = *on
174300030917 e5  c                   endif
174400010614
174500010614 b4  c                   if        wokroes = *on
174600010614     c                   eval      wok900 = *on
174700010608     c                   eval      wroroes = wroroes +1
174800010608      * calcolo affidabilità. Uso >= per i ritiri nei festivi
174900010614 b5  c                   select
175000010614     c                   when      ormdar > dataela
175100010614     c                   eval      wrorean = wrorean +1
175200010614     c                   when      ormdar = dataela
175300010608     c                   eval      wroreit = wroreit +1
175400010608     c                   when      ormdar >= wdtda01
175500010608     c                   eval      wrorer1 = wrorer1 +1
175600010608     c                   when      ormdar >= wdtda02
175700010608     c                   eval      wrorer2 = wrorer2 +1
175800010614 x5  c                   other
175900010608     c                   eval      wrorero = wrorero +1
176000010614 e5  c                   endsl
176100010608     c                   eval      wokstp = *on
176200010614     c                   eval      wokrfile = *on
176300010608     c                   leave
176400010614 x4  c                   else
176500010615 b5  c                   if        wok600 = *on
176600010615     c                   leave
176700010615 e5  c                   endif
176800010613 e4  c                   endif
176900010613 e3  c                   endif
177000010614
177100010614     ?* ORM eseguiti (fase 600) ?
177200010614 b3  c                   if        orffar = 600
177300010614     c                   eval      wroroes = wroroes +1
177400010614      * calcolo affidabilità. Uso >= per i ritiri nei festivi
177500010614 b4  c                   select
177600010614     c                   when      ormdar > dataela
177700010614     c                   eval      wrorean = wrorean +1
177800010614     c                   when      ormdar = dataela
177900010614     c                   eval      wroreit = wroreit +1
178000010614     c                   when      ormdar >= wdtda01
178100010614     c                   eval      wrorer1 = wrorer1 +1
178200010614     c                   when      ormdar >= wdtda02
178300010614     c                   eval      wrorer2 = wrorer2 +1
178400010614 x4  c                   other
178500010614     c                   eval      wrorero = wrorero +1
178600010614 e4  c                   endsl
178700010615     ?* ORM non bollettati
178800010615     c                   if        orfdae >= wdtda01 and
178900010618     c                             orfdae < dataela  and
179000010615     c                             ormdar < dataela
179100010615     c                   eval      wrornbl = wrornbl +1
179200010621     c   11              eval      w_rornbl = 'X'
179300010615     c                   endif
179400010614     c                   eval      wokstp = *on
179500010614     c                   eval      wokrfile = *on
179600010614     c                   leave
179700010614 e3  c                   endif
179800010612
179900010614      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
180000010613 b3  c                   if        wokfestar = *off
180100060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
180200010705      * in questo caso devo tornare a leggere la precedente
180300060926     c     orffar        Lookup    skfar                                  30
180400060926 b4  c**!!!              if        orffar = 300
180500060926     c                   if        *in30
180600010705     c                   goto      passa
180700010705 e4  c                   endif
180800040421     ?* ORM chiusi/eseguiti anomali (fase 910 - 999)
180900031105 b4  c                   if        orffar = 999 or OrfFar = 910
181000030917     c                   eval      wroroea = wroroea +1
181100030917      * calcolo affidabilità. Uso >= per gli anomali nei festivi
181200030917 b5  c                   select
181300030917     c                   when      ormdar > dataela
181400030917     c                   eval      wroraan = wroraan +1
181500030917     c                   when      ormdar = dataela
181600030917     c                   eval      wrorait = wrorait +1
181700030917     c                   when      ormdar >= wdtda01
181800030917     c                   eval      wrorar1 = wrorar1 +1
181900030917     c                   when      ormdar >= wdtda02
182000030917     c                   eval      wrorar2 = wrorar2 +1
182100030917 x5  c                   other
182200030917     c                   eval      wroraro = wroraro +1
182300030917 e5  c                   endsl
182400030917      * memorizzo il tipo comunicazione ORM
182500030917     ?* fissi ?
182600030917 b5  c                   if        ormtco = 'S'
182700030917     c                   eval      wrorifian = wrorifian +1
182800030917 e5  c                   endif
182900030917     ?* da centralino ?
183000030917 b5  c                   if        ormtco = 'M'  or
183100030917     c                             ormtco = 'P'
183200030917     c                   eval      wroricean = wroricean +1
183300030917 e5  c                   endif
183400030917     ?* da internet ?
183500030917 b5  c                   if        ormtco = 'I'
183600030917     c                   eval      wroriinan = wroriinan +1
183700030917 e5  c                   endif
183800030917     ?* da vas ?
183900030917 b5  c                   if        ormtco = 'F'
184000030917     c                   eval      wrorivaan = wrorivaan +1
184100030917 e5  c                   endif
184200030917     ?* da call center ?
184300030917 b5  c                   if        ormtco = 'C'
184400030917     c                   eval      wroriccan = wroriccan +1
184500030917 e5  c                   endif
184600030917     c                   eval      cauoea = orfcar
184700030917     c                   if        orfcar = '80'
184800030917     c                   eval      wrora01 = wrora01 +1
184900030917     c                   endif
185000030917     c                   if        orfcar = '81'
185100030917     c                   eval      wrora02 = wrora02 +1
185200030917     c                   endif
185300030917     c                   if        orfcar = '90'
185400030917     c                   eval      wrora03 = wrora03 +1
185500030917     c                   endif
185600030917     c                   if        orfcar = '91'
185700030917     c                   eval      wrora04 = wrora04 +1
185800030917     c                   endif
185900030917     c                   if        orfcar = '92'
186000030917     c                   eval      wrora05 = wrora05 +1
186100030917     c                   endif
186200030917     c                   if        orfcar = '93'
186300030917     c                   eval      wrora06 = wrora06 +1
186400030917     c                   endif
186500030917     c                   if        orfcar = '94'
186600030917     c                   eval      wrora07 = wrora07 +1
186700030917     c                   endif
186800030917     c                   if        orfcar = '95'
186900030917     c                   eval      wrora08 = wrora08 +1
187000030917     c                   endif
187100031105     c                   if        orfcar = '82'
187200031105     c                   eval      wrora09 = wrora09 +1
187300031105     c                   endif
187400040701     c                   if        orfcar = '96'
187500040701     c                   eval      wrora10 = wrora10 +1
187600040701     c                   endif
187700040701     c                   if        orfcar = '97'
187800040701     c                   eval      wrora11 = wrora11 +1
187900040701     c                   endif
188000110718     c**!!!              if        orfcar = '98'
188100110718     c**!!!              eval      wrora12 = wrora12 +1
188200110718     c**!!!              endif
188300030917     c                   eval      wokstp = *on
188400030917     c                   eval      wokrfile = *on
188500030917     c                   leave
188600030917 e4  c                   endif
188700010608     ?* ORM non eseguiti
188800030916 b4  c                   if        orffar < 600
188900030916     ?* con data ritiro <= data elaborazione
189000030916 b4a c                   If        ormdar <= dataela
189100010608     c                   eval      wrorone = wrorone +1
189200010621     c   11              eval      w_rorone = 'X'
189300030917     c                   eval      wokstp = *on
189400030917     c                   eval      wokrfile = *on
189500010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
189600010614 b5  c                   select
189700010620     c                   when      ormdar > dataela
189800010620     c                   eval      wrornan = wrornan +1
189900010608     c                   when      ormdar = dataela
190000010608     c                   eval      wrornit = wrornit +1
190100010608     c                   when      ormdar >= wdtda01
190200010608     c                   eval      wrornr1 = wrornr1 +1
190300010608     c                   when      ormdar >= wdtda02
190400010608     c                   eval      wrornr2 = wrornr2 +1
190500010614 x5  c                   other
190600010608     c                   eval      wrornro = wrornro +1
190700010614 e5  c                   endsl
190800030912      * memorizzo il tipo comunicazione ORM
190900030912     ?* fissi ?
191000030912 b5  c                   if        ormtco = 'S'
191100030917     c                   eval      wrorifine = wrorifine +1
191200030912 e5  c                   endif
191300030912     ?* da centralino ?
191400030912 b5  c                   if        ormtco = 'M'  or
191500030912     c                             ormtco = 'P'
191600030917     c                   eval      wroricene = wroricene +1
191700030912 e5  c                   endif
191800030912     ?* da internet ?
191900030912 b5  c                   if        ormtco = 'I'
192000030917     c                   eval      wroriinne = wroriinne +1
192100030912 e5  c                   endif
192200030912     ?* da vas ?
192300030912 b5  c                   if        ormtco = 'F'
192400030917     c                   eval      wrorivane = wrorivane +1
192500030912 e5  c                   endif
192600030912     ?* da call center ?
192700030912 b5  c                   if        ormtco = 'C'
192800030917     c                   eval      wroriccne = wroriccne +1
192900030912 e5  c                   endif
193000010608     ?* ORM non eseguiti con causale ?
193100010613 b5  c                   if        orffar = 500
193200010619     c                   eval      cauone = orfcar
193300010608     c                   if        orfcar = '01'
193400010608     c                   eval      wrorn01 = wrorn01 +1
193500010608     c                   endif
193600010608     c                   if        orfcar = '02'
193700010608     c                   eval      wrorn02 = wrorn02 +1
193800010608     c                   endif
193900010608     c                   if        orfcar = '03'
194000010608     c                   eval      wrorn03 = wrorn03 +1
194100010608     c                   endif
194200020527     c                   if        orfcar = '04'
194300020527     c                   eval      wrorn04 = wrorn04 +1
194400020527     c                   endif
194500030917     c                   leave
194600010613 e5  c                   endif
194700030917     ?* ORM non eseguiti no in fase 500 ?
194800030917     ?* ORM non assegnati
194900030917 b5  c                   if        orffar < 400
195000030917     c                   eval      wrornas = wrornas +1
195100030917     c   11              eval      w_rornas = 'X'
195200030917 e5  c                   endif
195300030912      * devo controllare se nel giorno è stata fatta una fase 500
195400030912     c     kOrf02        Readpe    Fnorf01l
195500030917 b5  c                   Dow       Not %Eof(Fnorf01l)
195600040421      * se trovo fase 200 esco
195700040618if  6c                   If        OrfFar = 200
195800040421     c                   Leave
195900040421e   6c                   EndIf
196000030917 b6  c                   If        OrfFar = 500
196100030912      * se trovo la fase 500 memorizzo la causale
196200030912     c                   Eval      %Subst(cauone:2:2) = OrfCar
196300030919     c                   Move      ')'           cauone
196400030919     c                   Movel     '('           cauone
196500030912     c                   If        OrfCar = '01'
196600030912     c                   Eval      wrorn01 = wrorn01 +1
196700030912     c                   Endif
196800030912     c                   If        OrfCar = '02'
196900030912     c                   Eval      wrorn02 = wrorn02 +1
197000030912     c                   Endif
197100030912     c                   If        OrfCar = '03'
197200030912     c                   Eval      wrorn03 = wrorn03 +1
197300030912     c                   Endif
197400030912     c                   If        OrfCar = '04'
197500030912     c                   Eval      wrorn04 = wrorn04 +1
197600030912     c                   Endif
197700030912     c                   Leave
197800030917 e6  c                   Endif
197900030912     c     kOrf02        Readpe    Fnorf01l
198000030917 e5  c                   EndDo
198100030917     c                   Leave
198200030916 e4a c                   endif
198300030916     ?* con data ritiro > data elaborazione
198400030919 b4a c                   If        ormdar > dataela
198500030929     ?* è un ORM non fatto se nel giorno c'è una fase 500 o 390
198600031105     ?* quindi lo metto nella colonna dei non eseguiti e anche in quella dei da eseguire
198700030929 b5  c                   If        OrfFar = 500 or OrfFar = 390
198800031105     c                   eval      wrorone = wrorone +1
198900031105     c                   eval      wroroned = wroroned +1
199000031105     c   11              eval      w_rorone = 'X'
199100030917     c                   eval      wokstp = *on
199200030917     c                   eval      wokrfile = *on
199300031105      * da eseguire
199400031105     c                   eval      wrorode = wrorode +1
199500031105      * sempre in anticipo xchè data ritiro > data elaborazione
199600031105     c                   eval      wrordan = wrordan +1
199700031106     c                   eval      wrornan = wrornan +1
199800030912      * memorizzo il tipo comunicazione ORM
199900030912     ?* fissi ?
200000030917 b6  c                   if        ormtco = 'S'
200100031105     c                   eval      wrorifine = wrorifine +1
200200031105     c                   eval      wrorifid = wrorifid +1
200300030917 e6  c                   endif
200400030912     ?* da centralino ?
200500030917 b6  c                   if        ormtco = 'M'  or
200600030912     c                             ormtco = 'P'
200700031105     c                   eval      wroricene = wroricene +1
200800031105     c                   eval      wroriced = wroriced +1
200900030917 e6  c                   endif
201000030912     ?* da internet ?
201100030917 b6  c                   if        ormtco = 'I'
201200031105     c                   eval      wroriinne = wroriinne +1
201300031105     c                   eval      wroriind = wroriind +1
201400030917 e6  c                   endif
201500030912     ?* da vas ?
201600030917 b6  c                   if        ormtco = 'F'
201700031105     c                   eval      wrorivane = wrorivane +1
201800031105     c                   eval      wrorivad = wrorivad +1
201900030917 e6  c                   endif
202000030912     ?* da call center ?
202100030917 b6  c                   if        ormtco = 'C'
202200031105     c                   eval      wroriccne = wroriccne +1
202300031105     c                   eval      wroriccd = wroriccd +1
202400030917 e6  c                   endif
202500030912     ?* ORM non eseguiti con causale ?
202600030929 b6  c                   If        OrfFar = 500
202700031105     c                   eval      cauone = orfcar
202800030912     c                   if        orfcar = '01'
202900031105     c                   eval      wrorn01 = wrorn01 +1
203000031105     c                   eval      wrorn01d = wrorn01d +1
203100030912     c                   endif
203200030912     c                   if        orfcar = '02'
203300031105     c                   eval      wrorn02 = wrorn02 +1
203400031105     c                   eval      wrorn02d = wrorn02d +1
203500030912     c                   endif
203600030912     c                   if        orfcar = '03'
203700031105     c                   eval      wrorn03 = wrorn03 +1
203800031105     c                   eval      wrorn03d = wrorn03d +1
203900030912     c                   endif
204000030912     c                   if        orfcar = '04'
204100031105     c                   eval      wrorn04 = wrorn04 +1
204200031105     c                   eval      wrorn04d = wrorn04d +1
204300030912     c                   endif
204400030929 e6  c                   endif
204500030929     c                   Leave
204600030917 e5  c                   endif
204700030929     ?* ORM no in fase 500 o 390 ?
204800030929      * devo controllare se nel giorno è stata fatta una fase 500 o 390
204900030912     c     kOrf02        Readpe    Fnorf01l
205000030917 b5  c                   Dow       Not %Eof(Fnorf01l)
205100040421      * se trovo fase 200 esco
205200040618if  6c                   If        OrfFar = 200
205300040421     c                   Leave
205400040421e   6c                   EndIf
205500030929 b6  c                   If        OrfFar = 500 or OrfFar = 390
205600031105     c                   eval      wrorone = wrorone +1
205700031105     c                   eval      wroroned = wroroned +1
205800031105     c   11              eval      w_rorone = 'X'
205900030929     c                   eval      wokstp = *on
206000030929     c                   eval      wokrfile = *on
206100031106      * da eseguire
206200031106     c                   eval      wrorode = wrorode +1
206300031106      * sempre in anticipo xchè data ritiro > data elaborazione
206400031106     c                   eval      wrordan = wrordan +1
206500031106     c                   eval      wrornan = wrornan +1
206600030929      * memorizzo il tipo comunicazione ORM
206700030929     ?* fissi ?
206800030929 b7  c                   if        ormtco = 'S'
206900031105     c                   eval      wrorifine = wrorifine +1
207000031105     c                   eval      wrorifid = wrorifid +1
207100030929 e7  c                   endif
207200030929     ?* da centralino ?
207300030929 b7  c                   if        ormtco = 'M'  or
207400030929     c                             ormtco = 'P'
207500031105     c                   eval      wroricene = wroricene +1
207600031105     c                   eval      wroriced = wroriced +1
207700030929 e7  c                   endif
207800030929     ?* da internet ?
207900030929 b7  c                   if        ormtco = 'I'
208000031105     c                   eval      wroriinne = wroriinne +1
208100031105     c                   eval      wroriind = wroriind +1
208200030929 e7  c                   endif
208300030929     ?* da vas ?
208400030929 b7  c                   if        ormtco = 'F'
208500031105     c                   eval      wrorivane = wrorivane +1
208600031105     c                   eval      wrorivad = wrorivad +1
208700030929 e7  c                   endif
208800030929     ?* da call center ?
208900030929 b7  c                   if        ormtco = 'C'
209000031105     c                   eval      wroriccne = wroriccne +1
209100031105     c                   eval      wroriccd = wroriccd +1
209200030929 e7  c                   endif
209300030912      * se trovo la fase 500 memorizzo la causale
209400030929 b7  c                   If        OrfFar = 500
209500031105     c                   Eval      %Subst(cauone:2:2) = OrfCar
209600031105     c                   Move      ')'           cauone
209700031105     c                   Movel     '('           cauone
209800030912     c                   If        OrfCar = '01'
209900031105     c                   Eval      wrorn01 = wrorn01 +1
210000031105     c                   Eval      wrorn01d = wrorn01d +1
210100030912     c                   Endif
210200030912     c                   If        OrfCar = '02'
210300031105     c                   Eval      wrorn02 = wrorn02 +1
210400031105     c                   Eval      wrorn02d = wrorn02d +1
210500030912     c                   Endif
210600030912     c                   If        OrfCar = '03'
210700031105     c                   Eval      wrorn03 = wrorn03 +1
210800031105     c                   Eval      wrorn03d = wrorn03d +1
210900030912     c                   Endif
211000030912     c                   If        OrfCar = '04'
211100031105     c                   Eval      wrorn04 = wrorn04 +1
211200031105     c                   Eval      wrorn04d = wrorn04d +1
211300030912     c                   Endif
211400030929 e7  c                   Endif
211500030912     c                   Leave
211600030917 e6  c                   Endif
211700030912     c     kOrf02        Readpe    Fnorf01l
211800030917 e5  c                   EndDo
211900030917     c                   Leave
212000030916 e4a c                   endif
212100010613 e4  c                   endif
212200010608
212300010613 e3  c                   endif
212400010612
212500010613 e2  c                   endif
212600010608
212700030912      * _____________________________
212800010608      * data fase < data elaborazione
212900030912      * -----------------------------
213000010620 b2  c                   if        orfdae < dataela
213100010705
213200010614      * se ORM eseguito (fase 900) esco dal giro
213300010614     c                   if        orffar >= 900
213400010614     c                   leave
213500010614     c                   endif
213600010612      * se data ritiro maggiore data elaborazione esco dal giro
213700010612     c                   if        ormdar > dataela
213800010612     c                   leave
213900010612     c                   endif
214000010619      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
214100010619      * e i non bollettati
214200010619 b3  c                   if        wokfestar = *off
214300010614     ?* ORM non bollettati
214400010615     c                   if        orffar = 600 and
214500010615     c                             orfdae >= wdtda01
214600010614     c                   eval      wrornbl = wrornbl +1
214700010621     c   11              eval      w_rornbl = 'X'
214800010614     c                   eval      wokstp = *on
214900010614     c                   eval      wokrfile = *on
215000010614     c                   leave
215100010614     c                   endif
215200010615     ?* ORM già eseguiti che non devo elaborare
215300010615     c                   if        orffar = 600
215400010615     c                   leave
215500010615     c                   endif
215600060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
215700010705      * in questo caso devo tornare a leggere la precedente
215800060926     c     orffar        Lookup    skfar                                  30
215900060926 b4  c**!!!              if        orffar = 300
216000060926     c                   if        *in30
216100010705     c                   goto      passa
216200010705 e4  c                   endif
216300010608     ?* ORM non eseguiti
216400010619 b4  c                   if        orffar < 600
216500010608     c                   eval      wrorone = wrorone +1
216600010621     c   11              eval      w_rorone = 'X'
216700030917     c                   eval      wokstp = *on
216800030917     c                   eval      wokrfile = *on
216900010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
217000010619 b5  c                   select
217100010608     c                   when      ormdar = dataela
217200010608     c                   eval      wrornit = wrornit +1
217300010608     c                   when      ormdar >= wdtda01
217400010608     c                   eval      wrornr1 = wrornr1 +1
217500010608     c                   when      ormdar >= wdtda02
217600010608     c                   eval      wrornr2 = wrornr2 +1
217700010619 x5  c                   other
217800010608     c                   eval      wrornro = wrornro +1
217900010619 e5  c                   endsl
218000030917      * memorizzo il tipo comunicazione ORM
218100030917     ?* fissi ?
218200030917 b5  c                   if        ormtco = 'S'
218300030917     c                   eval      wrorifine = wrorifine +1
218400030917 e5  c                   endif
218500030917     ?* da centralino ?
218600030917 b5  c                   if        ormtco = 'M'  or
218700030917     c                             ormtco = 'P'
218800030917     c                   eval      wroricene = wroricene +1
218900030917 e5  c                   endif
219000030917     ?* da internet ?
219100030917 b5  c                   if        ormtco = 'I'
219200030917     c                   eval      wroriinne = wroriinne +1
219300030917 e5  c                   endif
219400030917     ?* da vas ?
219500030917 b5  c                   if        ormtco = 'F'
219600030917     c                   eval      wrorivane = wrorivane +1
219700030917 e5  c                   endif
219800030917     ?* da call center ?
219900030917 b5  c                   if        ormtco = 'C'
220000030917     c                   eval      wroriccne = wroriccne +1
220100030917 e5  c                   endif
220200030917     ?* ORM non eseguiti con causale ?
220300030917 b5  c                   if        orffar = 500
220400030917     c                   eval      cauone = orfcar
220500030917     c                   if        orfcar = '01'
220600030917     c                   eval      wrorn01 = wrorn01 +1
220700030917     c                   endif
220800030917     c                   if        orfcar = '02'
220900030917     c                   eval      wrorn02 = wrorn02 +1
221000030917     c                   endif
221100030917     c                   if        orfcar = '03'
221200030917     c                   eval      wrorn03 = wrorn03 +1
221300030917     c                   endif
221400030917     c                   if        orfcar = '04'
221500030917     c                   eval      wrorn04 = wrorn04 +1
221600030917     c                   endif
221700030917     c                   leave
221800030917 e5  c                   endif
221900030917     ?* ORM non eseguiti no in fase 500 ?
222000010614     ?* ORM non assegnati
222100010619 b5  c                   if        orffar < 400
222200010614     c                   eval      wrornas = wrornas +1
222300010621     c   11              eval      w_rornas = 'X'
222400010619 e5  c                   endif
222500030917      * devo controllare se nel giorno è stata fatta una fase 500
222600030917     c     kOrf02        Readpe    Fnorf01l
222700030917 b5  c                   Dow       Not %Eof(Fnorf01l)
222800040421      * se trovo fase 200 esco
222900040618if  6c                   If        OrfFar = 200
223000040421     c                   Leave
223100040421e   6c                   EndIf
223200030917 b6  c                   If        OrfFar = 500
223300030917      * se trovo la fase 500 memorizzo la causale
223400030917     c                   Eval      %Subst(cauone:2:2) = OrfCar
223500031118     c                   Move      ')'           cauone
223600031118     c                   Movel     '('           cauone
223700030917     c                   If        OrfCar = '01'
223800030917     c                   Eval      wrorn01 = wrorn01 +1
223900030917     c                   Endif
224000030917     c                   If        OrfCar = '02'
224100030917     c                   Eval      wrorn02 = wrorn02 +1
224200030917     c                   Endif
224300030917     c                   If        OrfCar = '03'
224400030917     c                   Eval      wrorn03 = wrorn03 +1
224500030917     c                   Endif
224600030917     c                   If        OrfCar = '04'
224700030917     c                   Eval      wrorn04 = wrorn04 +1
224800030917     c                   Endif
224900030917     c                   Leave
225000030917 e6  c                   Endif
225100030917     c     kOrf02        Readpe    Fnorf01l
225200030917 e5  c                   EndDo
225300010608     c                   leave
225400010619 e4  c                   endif
225500010619
225600010619 e3  c                   endif
225700010608
225800010613 e2  c                   endif
225900010705
226000010705     c     passa         tag
226100010705
226200010608     c     korf          readpe    fnorf01l
226300010705
226400010613 e1  c                   enddo
226500010612
226600010612      * se ORM in fase 900 controllo se quadrato
226700010612     c                   if        wok900 = *on
226800010614      *                            =========
226900010614     c                   exsr      SR_QUADRA
227000010614      *                            =========
227100010612     c                   endif
227200010608
227300010608     c                   endsr
227400010614
227500010608      *--------------------------------------------------------------*
227600010608      * ORM NON QUADRATO
227700010608      *--------------------------------------------------------------*
227800010614     c     SR_QUADRA     begsr
227900010608
228000010608      * ORM non quadrati
228100010608      * mi devo posizionare sulle fasi con la data elaborazione
228200010608      * poi cercare se c'è una fase 900 controllare se esiste
228300010608      * una fase 600
228400010608
228500010608     c                   eval      wok600 = *off
228600010608
228700010608     c                   if        wok900 = *on
228800010608     c                   eval      kormdfo = dataela
228900010615     c                   clear                   kormofo
229000010615     c                   clear                   kormfao
229100010608     c     korf          setll     fnorf01l
229200010608     c                   do        *hival
229300010608     c     korf          reade     fnorf01l
229400010608     c                   if        %eof(fnorf01l)
229500010608     c                   leave
229600010608     c                   endif
229700010608     c                   if        orffar = 600
229800010608     c                   eval      wok600 = *on
229900010608     c                   leave
230000010608     c                   endif
230100010608     c                   enddo
230200010608     c                   if        wok600 = *off
230300010608     c                   eval      wrornqd = wrornqd +1
230400010621     c   11              eval      w_rornqd = 'X'
230500010608     c                   eval      wokstp = *on
230600010608     c                   endif
230700010608     c                   endif
230800010608
230900010608     c                   endsr
231000010614
231100010605      *--------------------------------------------------------------*
231200010605      * CONTROLLO E CARICAMENTO SK
231300010605      *--------------------------------------------------------------*
231400010605     c     SR_CONTRSK    begsr
231500010605
231600010606     c                   eval      woksk = *off
231700010606
231800010612     c                   do        18000         xf
231900030912     c                   if        kfile(xf) = dskfile
232000010605     c                   eval      woksk = *on
232100010605     c                   leave
232200010605     c                   endif
232300030912     c                   if        kfile1(xf) = *zeros
232400010605     c                   leave
232500010605     c                   endif
232600010605     c                   enddo
232700010605
232800010606     c                   if        woksk = *on
232900010612     c                   eval      dswfror = rfile(xf)
233000010606     c                   endif
233100010605
233200010605     c                   if        woksk = *off
233300010611     c                   clear                   dswfror
233400010612     c                   eval      kfile(xf) = dskfile
233500030916     c                   eval      kfile1(xf) = dskfile1
233600030916     c                   Eval      f_rortrc = dstrc
233700010605     c                   eval      f_rorpoe = dspoel
233800010605     c                   eval      f_rordae = dsdata
233900010605     c                   eval      kazorg = dspoel
234000010605     c     kazorg        chain     azorg01l
234100010605     c                   if        %found(azorg01l)
234200010605     c                   eval      f_rordis = orgfl3
234300010605     c                   eval      f_rorare = orgcar
234400010605     c                   endif
234500010605     c                   endif
234600010605
234700010605     c                   endsr
234800010614
234900010608      *--------------------------------------------------------------*
235000010608      * CARICO LA SK DEL FILE
235100010608      *--------------------------------------------------------------*
235200010614     c     SR_CARICA     begsr
235300010608
235400010608     c                   eval      f_rorode = (f_rorode + wrorode)
235500010608     c                   eval      f_roroes = (f_roroes + wroroes)
235600010608     c                   eval      f_rorone = (f_rorone + wrorone)
235700010608     c                   eval      f_rorn01 = (f_rorn01 + wrorn01)
235800010608     c                   eval      f_rorn02 = (f_rorn02 + wrorn02)
235900010608     c                   eval      f_rorn03 = (f_rorn03 + wrorn03)
236000010608     c                   eval      f_rorn04 = (f_rorn04 + wrorn04)
236100010608     c                   eval      f_rorn05 = (f_rorn05 + wrorn05)
236200010608     c                   eval      f_rorn06 = (f_rorn06 + wrorn06)
236300010608     c                   eval      f_rorn07 = (f_rorn07 + wrorn07)
236400010608     c                   eval      f_rorn08 = (f_rorn08 + wrorn08)
236500010608     c                   eval      f_rorn09 = (f_rorn09 + wrorn09)
236600010608     c                   eval      f_rorn10 = (f_rorn10 + wrorn10)
236700010608     c                   eval      f_roroea = (f_roroea + wroroea)
236800010608     c                   eval      f_rora01 = (f_rora01 + wrora01)
236900010608     c                   eval      f_rora02 = (f_rora02 + wrora02)
237000010608     c                   eval      f_rora03 = (f_rora03 + wrora03)
237100010608     c                   eval      f_rora04 = (f_rora04 + wrora04)
237200010608     c                   eval      f_rora05 = (f_rora05 + wrora05)
237300010608     c                   eval      f_rora06 = (f_rora06 + wrora06)
237400010608     c                   eval      f_rora07 = (f_rora07 + wrora07)
237500010608     c                   eval      f_rora08 = (f_rora08 + wrora08)
237600010608     c                   eval      f_rora09 = (f_rora09 + wrora09)
237700010608     c                   eval      f_rora10 = (f_rora10 + wrora10)
237800010608     c                   eval      f_rora11 = (f_rora11 + wrora11)
237900010608     c                   eval      f_rora12 = (f_rora12 + wrora12)
238000010608     c                   eval      f_rora13 = (f_rora13 + wrora13)
238100010608     c                   eval      f_rora14 = (f_rora14 + wrora14)
238200010608     c                   eval      f_rora15 = (f_rora15 + wrora15)
238300010620     c                   eval      f_rordan = (f_rordan + wrordan)
238400010608     c                   eval      f_rordit = (f_rordit + wrordit)
238500010608     c                   eval      f_rordr1 = (f_rordr1 + wrordr1)
238600010608     c                   eval      f_rordr2 = (f_rordr2 + wrordr2)
238700010608     c                   eval      f_rordro = (f_rordro + wrordro)
238800010614     c                   eval      f_rorean = (f_rorean + wrorean)
238900010608     c                   eval      f_roreit = (f_roreit + wroreit)
239000010608     c                   eval      f_rorer1 = (f_rorer1 + wrorer1)
239100010608     c                   eval      f_rorer2 = (f_rorer2 + wrorer2)
239200010608     c                   eval      f_rorero = (f_rorero + wrorero)
239300010620     c                   eval      f_rornan = (f_rornan + wrornan)
239400010608     c                   eval      f_rornit = (f_rornit + wrornit)
239500010608     c                   eval      f_rornr1 = (f_rornr1 + wrornr1)
239600010608     c                   eval      f_rornr2 = (f_rornr2 + wrornr2)
239700010608     c                   eval      f_rornro = (f_rornro + wrornro)
239800010620     c                   eval      f_roraan = (f_roraan + wroraan)
239900010608     c                   eval      f_rorait = (f_rorait + wrorait)
240000010608     c                   eval      f_rorar1 = (f_rorar1 + wrorar1)
240100010608     c                   eval      f_rorar2 = (f_rorar2 + wrorar2)
240200010608     c                   eval      f_roraro = (f_roraro + wroraro)
240300010608     c                   eval      f_rornqd = (f_rornqd + wrornqd)
240400010608     c                   eval      f_rornbl = (f_rornbl + wrornbl)
240500010614     c                   eval      f_rormdr = (f_rormdr + wrormdr)
240600010614     c                   eval      f_rornas = (f_rornas + wrornas)
240700030912     c                   eval      f_roroned = (f_roroned + wroroned)
240800030912     c                   eval      f_rorn01d = (f_rorn01d + wrorn01d)
240900030912     c                   eval      f_rorn02d = (f_rorn02d + wrorn02d)
241000030912     c                   eval      f_rorn03d = (f_rorn03d + wrorn03d)
241100030912     c                   eval      f_rorn04d = (f_rorn04d + wrorn04d)
241200030912     c                   eval      f_rorn05d = (f_rorn05d + wrorn05d)
241300030912     c                   eval      f_rorn06d = (f_rorn06d + wrorn06d)
241400030912     c                   eval      f_rorn07d = (f_rorn07d + wrorn07d)
241500030912     c                   eval      f_rorn08d = (f_rorn08d + wrorn08d)
241600030912     c                   eval      f_rorn09d = (f_rorn09d + wrorn09d)
241700030912     c                   eval      f_rorn10d = (f_rorn10d + wrorn10d)
241800030917     c                   eval      f_rorifian = (f_rorifian + wrorifian)
241900030917     c                   eval      f_rorifine = (f_rorifine + wrorifine)
242000030917     c                   eval      f_rorifid  = (f_rorifid  + wrorifid)
242100030917     c                   eval      f_roricean = (f_roricean + wroricean)
242200030917     c                   eval      f_roricene = (f_roricene + wroricene)
242300030917     c                   eval      f_roriced  = (f_roriced  + wroriced)
242400030917     c                   eval      f_roriinan = (f_roriinan + wroriinan)
242500030917     c                   eval      f_roriinne = (f_roriinne + wroriinne)
242600030917     c                   eval      f_roriind  = (f_roriind  + wroriind)
242700030917     c                   eval      f_rorivaan = (f_rorivaan + wrorivaan)
242800030917     c                   eval      f_rorivane = (f_rorivane + wrorivane)
242900030917     c                   eval      f_rorivad  = (f_rorivad  + wrorivad)
243000030917     c                   eval      f_roriccan = (f_roriccan + wroriccan)
243100030917     c                   eval      f_roriccne = (f_roriccne + wroriccne)
243200030917     c                   eval      f_roriccd  = (f_roriccd  + wroriccd)
243300010612     c                   eval      rfile(xf) = dswfror
243400010608
243500010608     c                   endsr
243600010614
243700010604      *--------------------------------------------------------------*
243800010604      * ROUTINE INIZIALE
243900010604      *--------------------------------------------------------------*
244000010604     c     *inzsr        begsr
244100010604      *
244200010604     c     *entry        plist
244300010604     c                   parm                    kpjba
244400010621
244500010621     c                   movel     kpjbu         fior40ds
244600010621
244700030912      * controllo il sistema informativo dove sono per reperire
244800030912      * la libreria
244900030912      *   ---> GAITRA201/GAITRAGRU
245000030912     c                   If        %subst(knsif:7:3) = '201'  and
245100030912     c                             %subst(knsif:1:1) = 'G'
245200030912     c                   Eval      wlib = 'GAITRAGRU '
245300030912     c                   EndIf
245400030916      *   ---> FILTRA201/FILTRAGRU
245500030912     c                   If        %subst(knsif:7:3) = '201'  and
245600030912     c                             %subst(knsif:1:1) = 'F'
245700030912     c                   Eval      wlib = 'FILTRAGRU '
245800030912     c                   EndIf
245900030916      *   ---> GAITRAPSM/GAITRAGRPS
246000030916     c                   If        %subst(knsif:7:3) = 'PSM'  and
246100030916     c                             %subst(knsif:1:1) = 'G'
246200030916     c                   Eval      wlib = 'GAITRAGRPS'
246300030916     c                   EndIf
246400030912      *   ---> GAITRAPSP/GAITRAGRPS
246500030912     c                   If        %subst(knsif:7:3) = 'PSP'  and
246600030912     c                             %subst(knsif:1:1) = 'G'
246700040421     c                   Eval      wlib = 'GAITRAGRPS'
246800040421     c**                 Eval      wlib = 'GAITRAFIL '
246900030912     c                   EndIf
247000030916      *   ---> GAITRAPFM/GAITRAGRPF
247100030916     c                   If        %subst(knsif:7:3) = 'PFM'  and
247200030916     c                             %subst(knsif:1:1) = 'G'
247300030916     c                   Eval      wlib = 'GAITRAGRPF'
247400030916     c                   EndIf
247500030912      *   ---> GAITRAPFP/GAITRAGRPF
247600030912     c                   If        %subst(knsif:7:3) = 'PFP'  and
247700030912     c                             %subst(knsif:1:1) = 'G'
247800040421     c                   Eval      wlib = 'GAITRAGRPF'
247900040421     c**                 Eval      wlib = 'GAITRAFIL '
248000030912     c                   EndIf
248100030912      *   ---> FILTRAPFP/FILTRAGRPF
248200030912     c                   If        %subst(knsif:7:3) = 'PFP'  and
248300030912     c                             %subst(knsif:1:1) = 'F'
248400040421     c                   Eval      wlib = 'FILTRAGRPF'
248500040421     c**                 Eval      wlib = 'GAITRAFIL '
248600030912     c                   EndIf
248700030912
248800010621      * duplico il file di comodo in qtemp
248900010621     c                   clear                   comman
249000010621     c                   movel(p)  cmd(1)        comman
249100030912     c                   Eval      %Subst(comman:32:10) = wlib
249200010621     c                   call      'QCMDEXC'
249300010621     c                   parm                    comman
249400010621     c                   parm                    lenght
249500010621      * apro il file di comodo per la stampa
249600010621     c                   open      wfror11l
249700010621
249800010604     c                   z-add     1             codut
249900010604     c                   call      'X§PARUT'
250000010604     c                   parm                    ut§dse0f
250100010614     c                   movel     ragut         rsut
250200010604     c                   movel     rec80         cncr80
250300010604
250400010604      * reperisco data e ora
250500010604     c                   time                    w0140
250600010604      * udate in GGMMAAAA
250700010604     c                   move      w0140         wdate
250800010604     c                   movel     w0140         wora
250900010608
251000010608      * pulisco le sk
251100010608     c                   clear                   kfile
251200030916     c                   clear                   kfile1
251300010608     c                   clear                   rfile
251400010608     c                   clear                   skdate
251500010608     c                   clear                   skpoel
251600130118     c******             Clear                   skosr
251700130118     c                   Clear                   skcliosr
251800130118     c                   Clear                   skstaosr
251900010621
252000010621      * controllo se devo stampare per data o per p.o.
252100010621     c                   if        i40poel <> 999 and
252200010621     c                             i40dtda <> i40dtal and
252300010621     c                             i40tpst = 'S'
252400010621     c                   eval      *in13 = *on
252500010621     c                   else
252600010621     c                   eval      *in13 = *off
252700010621     c                   endif
252800010608
252900010608      * carico la sk delle date da elaborare
253000010703     c                   eval      xd = 1
253100010703     c                   eval      skdate(xd) = i40dtda
253200010703     c                   if        i40dtda <> i40dtal
253300010703     c                   eval      wdtgio = i40dtda
253400010703     c     *iso          movel     i40dtda       wdtda
253500010703     c                   dow       wdtgio < i40dtal
253600010703     c                   eval      xd = xd +1
253700010703     c                   adddur    1:*d          wdtda
253800010703     c                   move      wdtda         wdtgio
253900010703     c                   eval      skdate(xd) = wdtgio
254000010703     c                   enddo
254100010703     c                   endif
254200020807
254300020808     c                   z-add     1             xa
254400010608
254500020807      * Elaboro tutti i p.o.
254600020807 b1  c                   if        i40poel = 999
254700020807      * Ricerco tutti i terminal di partenza
254800020807     c                   Clear                   Trul26Ds
254900020807     c                   Eval      I26tpt = 'P'
255000020807     c                   Eval      I26tla = 'L'
255100020807     c                   Call      'TRUL26R'
255200020807     c                   Parm                    Trul26Ds
255300020807      * carico i p.o. da elaborare dell'AS in un sk
255400020807 b2  c                   Do        250           Xb
255500020807     c                   If        Skpot(Xb) = *Zeros
255600020807     c                   Leave
255700020807     c                   EndIf
255800020807      * Per la filiale se lancio tutti i p.o. devo cercare quelli che
255900020807      * hanno il terminal di partenza = simfel
256000020807 b3  c                   If        i40sede = 'F'
256100020807     c                   If        Skpot(Xb) = Simfel
256200020807     c                   Eval      Skpoel(Xa) = Skpo(Xb)
256300020808     c                   Add       1             Xa
256400010608     c                   endif
256500020807      * Per la Sede se lancio tutti i p.o. carico tutti
256600020807 b3  c                   Else
256700020807     c                   Eval      Skpoel(Xb) = Skpo(Xb)
256800020807 e3  c                   EndIf
256900010608
257000020807 e2  c                   EndDo
257100020807 e1  c                   EndIf
257200010605      * lanciato un solo po carico questo nella sk
257300010605     c                   if        i40poel <> 999
257400010612     c                   eval      skpoel(xa) = i40poel
257500010605     c                   endif
257600031002
257700031002      * carico la sk delle serie da non elaborare in statistica
257800031002     c                   Eval      xc = 1
257900031002     c                   Eval      TbeCod = 'OSR'
258000031002     c     TbeCod        Setll     Tntbe01l
258100031002Do  1c                   Do        *Hival
258200031002     c     TbeCod        Reade     Tntbe01l
258300031002     c                   If        %Eof(Tntbe01l)
258400031002     c                   Leave
258500031002     c                   EndIf
258600130122     c                   IF        TBEatb <> *blanks
258700130122     c                   iter
258800130122     c                   ENDIF
258900031002     c                   Movel     TbeUni        dOsr
259000130118      /free
259100130118           IF d§osrcli <> 0;
259200130118             xx = %lookup(d§OSRcli:skCLIosr);
259300130118             IF  xx = 0;
259400130118               xx = %lookup(*zeros:skCLIosr);
259500130118               IF  xx > 0;
259600130118                 skCLIosr(xx) = d§OSRcli;
259700130118                 skstaosr(xx) = d§OSRsta;
259800130118               ENDIF;
259900130118             ENDIF;
260000130118           ENDIF;
260100130118
260200130118           IF d§osrcl2 <> 0;
260300130118             xx = %lookup(d§OSRcl2:skCLIosr);
260400130118             IF  xx = 0;
260500130118               xx = %lookup(*zeros:skCLIosr);
260600130118               IF  xx > 0;
260700130118                 skCLIosr(xx) = d§OSRcl2;
260800130118                 skstaosr(xx) = d§OSRsta;
260900130118               ENDIF;
261000130118             ENDIF;
261100130118           ENDIF;
261200130118
261300130118           IF d§osrcl3 <> 0;
261400130118             xx = %lookup(d§OSRcl3:skCLIosr);
261500130118             IF  xx = 0;
261600130118               xx = %lookup(*zeros:skCLIosr);
261700130118               IF  xx > 0;
261800130118                 skCLIosr(xx) = d§OSRcl3;
261900130118                 skstaosr(xx) = d§OSRsta;
262000130118               ENDIF;
262100130118             ENDIF;
262200130118           ENDIF;
262300130118
262400130118           IF d§osrcl4 <> 0;
262500130118             xx = %lookup(d§OSRcl4:skCLIosr);
262600130118             IF  xx = 0;
262700130118               xx = %lookup(*zeros:skCLIosr);
262800130118               IF  xx > 0;
262900130118                 skCLIosr(xx) = d§OSRcl4;
263000130118                 skstaosr(xx) = d§OSRsta;
263100130118               ENDIF;
263200130118             ENDIF;
263300130118           ENDIF;
263400130118
263500130118           IF d§osrcl5 <> 0;
263600130118             xx = %lookup(d§OSRcl5:skCLIosr);
263700130118             IF  xx = 0;
263800130118               xx = %lookup(*zeros:skCLIosr);
263900130118               IF  xx > 0;
264000130118                 skCLIosr(xx) = d§OSRcl5;
264100130118                 skstaosr(xx) = d§OSRsta;
264200130118               ENDIF;
264300130118             ENDIF;
264400130118           ENDIF;
264500130118
264600130118           IF d§osrcl6 <> 0;
264700130118             xx = %lookup(d§OSRcl6:skCLIosr);
264800130118             IF  xx = 0;
264900130118               xx = %lookup(*zeros:skCLIosr);
265000130118               IF  xx > 0;
265100130118                 skCLIosr(xx) = d§OSRcl6;
265200130118                 skstaosr(xx) = d§OSRsta;
265300130118               ENDIF;
265400130118             ENDIF;
265500130118           ENDIF;
265600130118
265700130118           IF d§osrcl7 <> 0;
265800130118             xx = %lookup(d§OSRcl7:skCLIosr);
265900130118             IF  xx = 0;
266000130118               xx = %lookup(*zeros:skCLIosr);
266100130118               IF  xx > 0;
266200130118                 skCLIosr(xx) = d§OSRcl7;
266300130118                 skstaosr(xx) = d§OSRsta;
266400130118               ENDIF;
266500130118             ENDIF;
266600130118           ENDIF;
266700130118
266800130118           IF d§osrcl8 <> 0;
266900130118             xx = %lookup(d§OSRcl8:skCLIosr);
267000130118             IF  xx = 0;
267100130118               xx = %lookup(*zeros:skCLIosr);
267200130118               IF  xx > 0;
267300130118                 skCLIosr(xx) = d§OSRcl8;
267400130118                 skstaosr(xx) = d§OSRsta;
267500130118               ENDIF;
267600130118             ENDIF;
267700130118           ENDIF;
267800130118
267900130118           IF d§osrcl9 <> 0;
268000130118             xx = %lookup(d§OSRcl9:skCLIosr);
268100130118             IF  xx = 0;
268200130118               xx = %lookup(*zeros:skCLIosr);
268300130118               IF  xx > 0;
268400130118                 skCLIosr(xx) = d§OSRcl9;
268500130118                 skstaosr(xx) = d§OSRsta;
268600130118               ENDIF;
268700130118             ENDIF;
268800130118           ENDIF;
268900130118
269000130118           IF d§osrcl10 <> 0;
269100130118             xx = %lookup(d§OSRcl10:skCLIosr);
269200130118             IF  xx = 0;
269300130118               xx = %lookup(*zeros:skCLIosr);
269400130118               IF  xx > 0;
269500130118                 skCLIosr(xx) = d§OSRcl10;
269600130118                 skstaosr(xx) = d§OSRsta;
269700130118               ENDIF;
269800130118             ENDIF;
269900130118           ENDIF;
270000130118
270100130118           IF d§osrcl11 <> 0;
270200130118             xx = %lookup(d§OSRcl11:skCLIosr);
270300130118             IF  xx = 0;
270400130118               xx = %lookup(*zeros:skCLIosr);
270500130118               IF  xx > 0;
270600130118                 skCLIosr(xx) = d§OSRcl11;
270700130118                 skstaosr(xx) = d§OSRsta;
270800130118               ENDIF;
270900130118             ENDIF;
271000130118           ENDIF;
271100130118
271200130118           IF d§osrcl12 <> 0;
271300130118             xx = %lookup(d§OSRcl12:skCLIosr);
271400130118             IF  xx = 0;
271500130118               xx = %lookup(*zeros:skCLIosr);
271600130118               IF  xx > 0;
271700130118                 skCLIosr(xx) = d§OSRcl12;
271800130118                 skstaosr(xx) = d§OSRsta;
271900130118               ENDIF;
272000130118             ENDIF;
272100130118           ENDIF;
272200130118
272300130118           IF d§osrcl13 <> 0;
272400130118             xx = %lookup(d§OSRcl13:skCLIosr);
272500130118             IF  xx = 0;
272600130118               xx = %lookup(*zeros:skCLIosr);
272700130118               IF  xx > 0;
272800130118                 skCLIosr(xx) = d§OSRcl13;
272900130118                 skstaosr(xx) = d§OSRsta;
273000130118               ENDIF;
273100130118             ENDIF;
273200130118           ENDIF;
273300130118
273400130118           IF d§osrcl14 <> 0;
273500130118             xx = %lookup(d§OSRcl14:skCLIosr);
273600130118             IF  xx = 0;
273700130118               xx = %lookup(*zeros:skCLIosr);
273800130118               IF  xx > 0;
273900130118                 skCLIosr(xx) = d§OSRcl14;
274000130118                 skstaosr(xx) = d§OSRsta;
274100130118               ENDIF;
274200130118             ENDIF;
274300130118           ENDIF;
274400130118
274500130118           IF d§osrcl15 <> 0;
274600130118             xx = %lookup(d§OSRcl15:skCLIosr);
274700130118             IF  xx = 0;
274800130118               xx = %lookup(*zeros:skCLIosr);
274900130118               IF  xx > 0;
275000130118                 skCLIosr(xx) = d§OSRcl15;
275100130118                 skstaosr(xx) = d§OSRsta;
275200130118               ENDIF;
275300130118             ENDIF;
275400130118           ENDIF;
275500130118
275600130118           IF d§osrcl16 <> 0;
275700130118             xx = %lookup(d§OSRcl16:skCLIosr);
275800130118             IF  xx = 0;
275900130118               xx = %lookup(*zeros:skCLIosr);
276000130118               IF  xx > 0;
276100130118                 skCLIosr(xx) = d§OSRcl16;
276200130118                 skstaosr(xx) = d§OSRsta;
276300130118               ENDIF;
276400130118             ENDIF;
276500130118           ENDIF;
276600130118      /end-free
276700130118
276800031002    1c                   EndDo
276900010611
277000010611      * carico nei campi di stampa le descrizioni delle causali
277100010611     c                   eval      tbecod = 'CMR'
277200010611      * causali di non fatto
277300010611     c                   eval      tbeke1 = '01'
277400010611     c     ktntbe        chain     tntbe01l
277500010611     c                   if        %found(tntbe01l)
277600010619     c                   clear                   dcmr
277700010619     c                   movel     tbeuni        dcmr
277800010619     c                   if        d§cmrdes1 <> *blanks
277900010619     c                   movel     d§cmrdes1     descau01n
278000010619     c                   else
278100010619     c                   movel     d§cmrdes      descau01n
278200010619     c                   endif
278300010611     c                   endif
278400010619
278500010611     c                   eval      tbeke1 = '02'
278600010611     c     ktntbe        chain     tntbe01l
278700010611     c                   if        %found(tntbe01l)
278800010619     c                   clear                   dcmr
278900010619     c                   movel     tbeuni        dcmr
279000010619     c                   if        d§cmrdes1 <> *blanks
279100010619     c                   movel     d§cmrdes1     descau02n
279200010619     c                   else
279300010619     c                   movel     d§cmrdes      descau02n
279400010619     c                   endif
279500010611     c                   endif
279600010619
279700010611     c                   eval      tbeke1 = '03'
279800010611     c     ktntbe        chain     tntbe01l
279900010611     c                   if        %found(tntbe01l)
280000010619     c                   clear                   dcmr
280100010619     c                   movel     tbeuni        dcmr
280200010619     c                   if        d§cmrdes1 <> *blanks
280300010619     c                   movel     d§cmrdes1     descau03n
280400010619     c                   else
280500010619     c                   movel     d§cmrdes      descau03n
280600010619     c                   endif
280700010611     c                   endif
280800020527
280900020527     c                   eval      tbeke1 = '04'
281000020527     c     ktntbe        chain     tntbe01l
281100020527     c                   if        %found(tntbe01l)
281200020527     c                   clear                   dcmr
281300020527     c                   movel     tbeuni        dcmr
281400020527     c                   if        d§cmrdes1 <> *blanks
281500020527     c                   movel     d§cmrdes1     descau04n
281600020527     c                   else
281700020527     c                   movel     d§cmrdes      descau04n
281800020527     c                   endif
281900020527     c                   endif
282000010619
282100010611      * causali di esito anomalo
282200010611     c                   eval      tbeke1 = '80'
282300010611     c     ktntbe        chain     tntbe01l
282400010611     c                   if        %found(tntbe01l)
282500010619     c                   clear                   dcmr
282600010619     c                   movel     tbeuni        dcmr
282700010619     c                   if        d§cmrdes1 <> *blanks
282800010619     c                   movel     d§cmrdes1     descau01a
282900010619     c                   else
283000010619     c                   movel     d§cmrdes      descau01a
283100010619     c                   endif
283200010611     c                   endif
283300010619
283400010611     c                   eval      tbeke1 = '81'
283500010611     c     ktntbe        chain     tntbe01l
283600010611     c                   if        %found(tntbe01l)
283700010619     c                   clear                   dcmr
283800010619     c                   movel     tbeuni        dcmr
283900010619     c                   if        d§cmrdes1 <> *blanks
284000010619     c                   movel     d§cmrdes1     descau02a
284100010619     c                   else
284200010619     c                   movel     d§cmrdes      descau02a
284300010619     c                   endif
284400010611     c                   endif
284500010619
284600010611     c                   eval      tbeke1 = '90'
284700010611     c     ktntbe        chain     tntbe01l
284800010611     c                   if        %found(tntbe01l)
284900010619     c                   clear                   dcmr
285000010619     c                   movel     tbeuni        dcmr
285100010619     c                   if        d§cmrdes1 <> *blanks
285200010619     c                   movel     d§cmrdes1     descau03a
285300010619     c                   else
285400010619     c                   movel     d§cmrdes      descau03a
285500010619     c                   endif
285600010611     c                   endif
285700010619
285800010611     c                   eval      tbeke1 = '91'
285900010611     c     ktntbe        chain     tntbe01l
286000010611     c                   if        %found(tntbe01l)
286100010619     c                   clear                   dcmr
286200010619     c                   movel     tbeuni        dcmr
286300010619     c                   if        d§cmrdes1 <> *blanks
286400010619     c                   movel     d§cmrdes1     descau04a
286500010619     c                   else
286600010619     c                   movel     d§cmrdes      descau04a
286700010619     c                   endif
286800010611     c                   endif
286900010619
287000010611     c                   eval      tbeke1 = '92'
287100010611     c     ktntbe        chain     tntbe01l
287200010611     c                   if        %found(tntbe01l)
287300010619     c                   clear                   dcmr
287400010619     c                   movel     tbeuni        dcmr
287500010619     c                   if        d§cmrdes1 <> *blanks
287600010619     c                   movel     d§cmrdes1     descau05a
287700010619     c                   else
287800010619     c                   movel     d§cmrdes      descau05a
287900010619     c                   endif
288000010611     c                   endif
288100010619
288200010611     c                   eval      tbeke1 = '93'
288300010611     c     ktntbe        chain     tntbe01l
288400010611     c                   if        %found(tntbe01l)
288500010619     c                   clear                   dcmr
288600010619     c                   movel     tbeuni        dcmr
288700010619     c                   if        d§cmrdes1 <> *blanks
288800010619     c                   movel     d§cmrdes1     descau06a
288900010619     c                   else
289000010619     c                   movel     d§cmrdes      descau06a
289100010619     c                   endif
289200010611     c                   endif
289300010619
289400010611     c                   eval      tbeke1 = '94'
289500010611     c     ktntbe        chain     tntbe01l
289600010611     c                   if        %found(tntbe01l)
289700010619     c                   clear                   dcmr
289800010619     c                   movel     tbeuni        dcmr
289900010619     c                   if        d§cmrdes1 <> *blanks
290000010619     c                   movel     d§cmrdes1     descau07a
290100010619     c                   else
290200010619     c                   movel     d§cmrdes      descau07a
290300010619     c                   endif
290400010611     c                   endif
290500020527
290600020527     c                   eval      tbeke1 = '95'
290700020527     c     ktntbe        chain     tntbe01l
290800020527     c                   if        %found(tntbe01l)
290900020527     c                   clear                   dcmr
291000020527     c                   movel     tbeuni        dcmr
291100020527     c                   if        d§cmrdes1 <> *blanks
291200020527     c                   movel     d§cmrdes1     descau08a
291300020527     c                   else
291400020527     c                   movel     d§cmrdes      descau08a
291500020527     c                   endif
291600020527     c                   endif
291700020625
291800020625     c                   eval      tbeke1 = '82'
291900020625     c     ktntbe        chain     tntbe01l
292000020625     c                   if        %found(tntbe01l)
292100020625     c                   clear                   dcmr
292200020625     c                   movel     tbeuni        dcmr
292300020625     c                   if        d§cmrdes1 <> *blanks
292400020625     c                   movel     d§cmrdes1     descau09a
292500020625     c                   else
292600020625     c                   movel     d§cmrdes      descau09a
292700020625     c                   endif
292800020625     c                   endif
292900040701
293000040701     c                   eval      tbeke1 = '96'
293100040701     c     ktntbe        chain     tntbe01l
293200040701     c                   if        %found(tntbe01l)
293300040701     c                   clear                   dcmr
293400040701     c                   movel     tbeuni        dcmr
293500040701     c                   if        d§cmrdes1 <> *blanks
293600040701     c                   movel     d§cmrdes1     descau10a
293700040701     c                   else
293800040701     c                   movel     d§cmrdes      descau10a
293900040701     c                   endif
294000040701     c                   endif
294100040701
294200040701     c                   eval      tbeke1 = '97'
294300040701     c     ktntbe        chain     tntbe01l
294400040701     c                   if        %found(tntbe01l)
294500040701     c                   clear                   dcmr
294600040701     c                   movel     tbeuni        dcmr
294700040701     c                   if        d§cmrdes1 <> *blanks
294800040701     c                   movel     d§cmrdes1     descau11a
294900040701     c                   else
295000040701     c                   movel     d§cmrdes      descau11a
295100040701     c                   endif
295200040701     c                   endif
295300100219
295400110718     c**!!!              eval      tbeke1 = '98'
295500110718     c**!!!ktntbe        chain     tntbe01l
295600110718     c**!!!              if        %found(tntbe01l)
295700110718     c**!!!              clear                   dcmr
295800110718     c**!!!              movel     tbeuni        dcmr
295900110718     c**!!!              if        d§cmrdes1 <> *blanks
296000110718     c**!!!              movel     d§cmrdes1     descau12a
296100110718     c**!!!              else
296200110718     c**!!!              movel     d§cmrdes      descau12a
296300110718     c**!!!              endif
296400110718     c**!!!              endif
296500060926
296600060926      * carico la sk delle fasi da non elaborare in statistica
296700060926     c                   eval      xc = 1
296800060926     c                   eval      tbecod = 'FAR'
296900060926     c     tbecod        setll     tntbe01l
297000060926Do  1c                   do        *hival
297100060926     c     tbecod        reade     tntbe01l
297200060926     c                   if        %eof(tntbe01l)
297300060926     c                   leave
297400060926     c                   endif
297500060926     c                   movel     tbeuni        dfar
297600060926     c                   if        d§farsta = *blanks
297700060926     c                   iter
297800060926     c                   endif
297900060926     c                   movel     tbeke1        skfar(xc)
298000060926     c                   add       1             xc
298100060926    1c                   enddo
298200010620
298300010620     c                   setoff                                       1011
298400010620
298500010705      * se richiesto apro il file della stampa di comodo EDP
298600010705     c                   if        i40edp = 'S'
298700010621     c                   open      fior41p2
298800010705     c                   eval      *in10 = *on
298900010620     c                   endif
299000010620
299100010705      * se richiesta la stampa di controllo apro il file di stampa
299200010620     c                   if        i40stpc = 'S'
299300010620     c                   open      fior41p1
299400010620     c                   eval      *in11 = *on
299500010620     c                   endif
299600010605
299700010621      * se richiesta la creazione del file lo apro
299800010621     c                   if        i40tpel = 'F'
299900010621     c                   open      wfror01l
300000010621     c                   endif
300100010621
300200010605      * chiavi
300300010605     c     korf          klist
300400010605     c                   kfld                    kormpoe
300500010605     c                   kfld                    kormnsr
300600010605     c                   kfld                    kormnor
300700010605     c                   kfld                    kormnrv
300800010604
300900010606     c     korf01        klist
301000010606     c                   kfld                    kormpoe
301100010606     c                   kfld                    kormnsr
301200010606     c                   kfld                    kormnor
301300010606     c                   kfld                    kormnrv
301400010606     c                   kfld                    kormdfo
301500010606     c                   kfld                    kormofo
301600010606     c                   kfld                    kormfao
301700010606
301800010606     c     korf02        klist
301900010606     c                   kfld                    kormpoe
302000010606     c                   kfld                    kormnsr
302100010606     c                   kfld                    kormnor
302200010606     c                   kfld                    kormnrv
302300010606     c                   kfld                    kormdfo
302400040420
302500040420     c     korf03        klist
302600040420     c                   kfld                    kormpoe
302700040420     c                   kfld                    kormnsr
302800040420     c                   kfld                    kormnor
302900040420     c                   kfld                    kormnrv
303000040420     c                   kfld                    kormfao
303100040420     c                   kfld                    kormdfo
303200040420
303300040420     c     korf04        klist
303400040420     c                   kfld                    kormpoe
303500040420     c                   kfld                    kormnsr
303600040420     c                   kfld                    kormnor
303700040420     c                   kfld                    kormnrv
303800040420     c                   kfld                    kormfao
303900010608
304000010618     c     korv          klist
304100010618     c                   kfld                    kormpoe
304200010618     c                   kfld                    kormnsr
304300010618     c                   kfld                    kormnor
304400010618     c                   kfld                    kormnrv
304500010618     c                   kfld                    korvdtv
304600010618
304700010608     c     kror          klist
304800010608     c                   kfld                    krorpoe
304900010608     c                   kfld                    krordta
305000030918     c                   kfld                    krortrc
305100030919
305200030919     c     kror1         klist
305300030919     c                   kfld                    krorpoe
305400030919     c                   kfld                    krordta
305500010606
305600010606     c     kazcln        klist
305700010606     c                   kfld                    ktfp
305800010606     c                   kfld                    ktfa
305900010606     c                   kfld                    kann
306000010606     c                   kfld                    kmes
306100010611
306200010620     c     ktab          klist
306300010620     c                   kfld                    codut
306400010620     c                   kfld                    tblcod
306500010620     c                   kfld                    tblkey
306600010620
306700010611     c     ktntbe        klist
306800010611     c                   kfld                    tbecod
306900010611     c                   kfld                    tbeke1
307000010606
307100010604     c                   endsr
307200010606      *--------------------------------------------------------------*
307300010621**   cmd - comando
307400030912CRTDUPOBJ OBJ(WFROR1*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
