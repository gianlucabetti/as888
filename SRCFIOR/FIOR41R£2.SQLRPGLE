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
089300131105     c                   IF        not *in99
089400131105     c                   IF        skstaosr(xx) <> *blanks
089500130118     c                   eval      *in99 = *on
089600130118     c                   ENDIF
089700131105     c                   ENDIF
089800130118     c                   ENDIF
089900130118     c*****wosr          Lookup    skosr                                  99
090000031002 b4  c                   If        Not *In99
090100020808
090200020808      *                            ==========
090300020808     c                   ExSr      Sr_Elabora
090400020808      *                            ==========
090500031002 b4  c                   EndIf
090600020808 b3  c                   EndIf
090700020808 b2  c                   EndIf
090800020808
090900020808     C/EXEC SQL
091000020808     C+ Fetch C1 into :fnormxxds
091100020808     C/END-EXEC
091200020808
091300020808 b1  c                   EndDo
091400020808
091500020808     C/EXEC SQL
091600020808     C+ close C1
091700020808     C/END-EXEC
091800020808
091900020808     c                   EndSr
092000010614
092100020808      *--------------------------------------------------------------*
092200020808      * LEGGO IL FILE DEGLI ORM
092300020808      *--------------------------------------------------------------*
092400020808     c     Sr_LeggiOrm   BegSr
092500020808
092600020808     c     *Start        Setll     Fnorm00f
092700020808     c                   Do        *Hival
092800020808     c                   Read      Fnorm00f
092900020808      * Fine file
093000020808 b1  c                   If        %Eof(Fnorm00f)
093100020808     c                   Leave
093200020808     c                   EndIf
093300020808      * no gli annullati
093400020808 b2  c                   If        Ormatb = *Blanks
093500020808
093600020808      * data immissione ORM non deve essere superiore alla data elaborazione
093700020808 b3  c                   If        Ormdao <= Dataela
093800030321
093900030321      * e controllo anche la data pulizia ORM di filiale
094000030321     c                             and Ormdao >= I40dtpo
094100031002
094200031002      * controllo se posso elaborare la serie
094300031002     c                   Move      OrmPoe        wOrmPoe
094400130118     c*******            Move      OrmNsr        wOrmNsr
094500130118     c                   eval      *in99 = *off
094600130118     c                   IF        ORMnsr > 0
094700130118     c                   eval      wORMcor =
094800130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
094900130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
095000130118     c                   IF        xx = 0
095100130118     c                   eval      *in99 = *on
095200130118     c                   ENDIF
095300130118     c                   IF        skstaosr(xx) <> *blanks
095400130118     c                   eval      *in99 = *on
095500130118     c                   ENDIF
095600130118     c                   ENDIF
095700130118     c*****wosr          Lookup    skosr                                  99
095800031002 b4  c                   If        Not *In99
095900020808
096000020808      *                            ==========
096100020808     c                   ExSr      Sr_Elabora
096200020808      *                            ==========
096300031002 b4  c                   EndIf
096400020808 b3  c                   EndIf
096500020808 b2  c                   EndIf
096600020808
096700020808 b1  c                   EndDo
096800020808
096900020808     c                   EndSr
097000020808
097100020808      *--------------------------------------------------------------*
097200020808      * ELABORO I DATI LETTI
097300020808      *--------------------------------------------------------------*
097400020808     c     Sr_Elabora    BegSr
097500020808
097600020808      * campi di appoggio
097700020808     c                   eval      xf = 1
097800020808     c                   eval      wpoe = *off
097900020808     c                   eval      wpor = *off
098000020808     c                   eval      wokstp = *off
098100020808     c                   eval      wok900 = *off
098200020808     c                   eval      wok600 = *off
098300020808     c                   eval      wokrode = *off
098400020808     c                   eval      wokroes = *off
098500020808     c                   eval      wokrfile = *off
098600020808     c                   eval      wokfestae = *off
098700020808     c                   eval      wokfestar = *off
098800020808
098900020808     c                   clear                   cauone
099000020808     c                   clear                   cauoea
099100030912
099200030912     c                   Eval      dorm01 = OrmFlo
099300030912      * ORM commissionato
099400030918     c                   Eval      *in14 = (§OrCom = 'S')
099500020808
099600020808      * controllo se il po emissione è nella sk dei p.o. da elaborare
099700020808     c                   eval      xa = 1
099800020808     c     Ormpoe        Lookup    Skpoel(Xa)                             30
099900020808     c                   If        *In30
100000020808     c                   eval      wpoe = *on
100100020808     c                   eval      dspoele = skpoel(xa)
100200020808     c                   endif
100300020808
100400020808      * controllo se il po ritiro è nella sk dei p.o. da elaborare
100500020808     c                   eval      xa = 1
100600020808     c     Ormpor        Lookup    Skpoel(Xa)                             30
100700020808     c                   If        *In30
100800020808     c                   eval      wpor = *on
100900020808     c                   eval      dspoelr = skpoel(xa)
101000020808     c                   endif
101100020808
101200020808      * se la data che sto elaborando è un festivo non devo calcolare
101300020808      *                            ========
101400020808     c                   exsr      SR_FESTA
101500020808      *                            ========
101600020808
101700020808     c                   if        wokfestae = *off and
101800020808     c                             wokfestar = *off
101900020808     c                   eval      dsdata = dataela
102000020808     c                   endif
102100020808
102200020808      * calcolo le date per affidabilità
102300020808      *                            =======
102400020808     c                   exsr      SR_DATA
102500020808      *                            =======
102600020808
102700020808      * pulisco i campi di comodo
102800020808      *                            ==========
102900020808     c                   exsr      SR_PULISCI
103000020808      *                            ==========
103100020808
103200020808      * faccio i controlli con il p.o. emissione
103300020808 b2  c                   if        wpoe = *on
103400020808      * se festa non faccio niente
103500020808 b3  c                   if        wokfestae = *off
103600020808      *                            ======
103700020808     c                   exsr      SR_POE
103800020808      *                            ======
103900020808 e3  c                   endif
104000020808 e2  c                   endif
104100020808
104200020808      * faccio i controlli con il p.o. ritiro
104300020808 b2  c                   if        wpor = *on
104400020808      * se festa non faccio niente
104500020808 b3  c                   if        wokfestae = *off
104600020808
104700020808      *                            ======
104800020808     c                   exsr      SR_POR
104900030916
105000030916 b4  c                   if        wokrfile = *on
105100020808      *                            ======
105200030918      * RECORD 'A'
105300020808      * controllo se esiste già un elemento di sk = alla mia key
105400020808      * e carico la sk relativa al file
105500020808     c                   eval      dspoel = dspoelr
105600030912     c                   eval      dspoel1 = dspoelr
105700030912     c                   eval      dsdata1 = dsdata
105800030918     c                   Eval      dstrc = 'A'
105900020808      *                            ==========
106000020808     c                   exsr      SR_CONTRSK
106100020808      *                            ==========
106200020808      * carico la sk del file
106300020808      *                            =========
106400020808     c                   exsr      SR_CARICA
106500020808      *                            =========
106600030918      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
106700030918     c                   Eval      sxf = xf
106800030918      *
106900030918      * RECORD 'B' ORM COMMISSIONATI
107000030912      * controllo se esiste già un elemento di sk = alla mia key
107100030912      * e carico la sk relativa al file
107200030916 b5  c                   If        *In14
107300030918     c                   Eval      dstrc = 'B'
107400030912      *                            ==========
107500030912     c                   exsr      SR_CONTRSK
107600030912      *                            ==========
107700030918      * carico la sk del file x record 'B' orm commissionato
107800030912      *                            =========
107900030912     c                   exsr      SR_CARICA
108000030912      *                            =========
108100030916 e5  c                   EndIf
108200020808 e4  c                   endif
108300020808 e3  c                   endif
108400020808 e2  c                   endif
108500030916      * ORM commissionato
108600030916     c                   If        *In14
108700030916     c                   Eval      commis = 'X'
108800030916     c                   Else
108900030916     c                   Clear                   commis
109000030916     c                   EndIf
109100020808
109200020808     ?* stampa di comodo EDP
109300020808 b2  c   10              if        wokstp = *on
109400030918     c                   eval      dswfror = rfile(sxf)
109500020808      * rottura di data
109600020808 b3  c                   if        dsdata <> sav_dsdatap
109700020808     c                   clear                   wlbdat
109800020808     c                   move      dsdata        g02inv
109900020808     c                   movel     '3'           G02err
110000020808     c                   call      'XSRDA8'
110100020808     c                   parm                    wlbdat
110200020808     c                   movel     g02dat        dsdatap
110300020808      * decodifico il p.o.
110400020808     c                   eval      kazorg = dspoel
110500020808     c     kazorg        chain     azorg01l
110600020808 b4  c                   if        %found(azorg01l)
110700020808     c                   movel     orgdes        despoel
110800020808 e4  c                   endif
110900020808     c                   write     or41t1p
111000020808     c                   eval      sav_dsdatap = dsdata
111100020808     c                   eval      sav_dspoelp = dspoel
111200020808 e3  c                   endif
111300020808      * rottura di p.o.
111400020808 b3  c                   if        dspoel <> sav_dspoelp
111500020808      * decodifico il p.o.
111600020808     c                   eval      kazorg = dspoel
111700020808     c     kazorg        chain     azorg01l
111800020808 b4  c                   if        %found(azorg01l)
111900020808     c                   movel     orgdes        despoel
112000020808 e4  c                   endif
112100020808     c                   write     or41t1p
112200020808     c                   eval      sav_dspoelp = dspoel
112300020808 e3  c                   endif
112400020808      * salto pagina
112500020808 b3  c                   if        *in91 = *on
112600020808     c                   write     or41t1p
112700020808     c                   eval      *in91 = *off
112800020808 e3  c                   endif
112900020808      * dettaglio
113000020808     c                   write     or41d1p
113100020808 e2  c                   endif
113200020808
113300020808     ?* stampa di controllo
113400020808 b2  c   11              if        wokstp = *on
113500030918     c                   eval      dswfror = rfile(sxf)
113600020808      * rottura di data
113700020808 b3  c                   if        dsdata <> sav_dsdatac
113800020808     c                   clear                   wlbdat
113900020808     c                   move      dsdata        g02inv
114000020808     c                   movel     '3'           G02err
114100020808     c                   call      'XSRDA8'
114200020808     c                   parm                    wlbdat
114300020808     c                   movel     g02dat        dsdatap
114400020808      * decodifico il p.o.
114500020808     c                   eval      kazorg = dspoel
114600020808     c     kazorg        chain     azorg01l
114700020808 b4  c                   if        %found(azorg01l)
114800020808     c                   movel     orgdes        despoel
114900020808 e4  c                   endif
115000020808     c                   write     or41t1c
115100020808     c                   eval      sav_dsdatac = dsdata
115200020808     c                   eval      sav_dspoelc = dspoel
115300020808 e3  c                   endif
115400020808      * rottura di p.o.
115500020808 b3  c                   if        dspoel <> sav_dspoelc
115600020808      * decodifico il p.o.
115700020808     c                   eval      kazorg = dspoel
115800020808     c     kazorg        chain     azorg01l
115900020808 b4  c                   if        %found(azorg01l)
116000020808     c                   movel     orgdes        despoel
116100020808 e4  c                   endif
116200020808     c                   write     or41t1c
116300020808     c                   eval      sav_dspoelc = dspoel
116400020808 e3  c                   endif
116500020808      * salto pagina
116600020808 b3  c                   if        *in92 = *on
116700020808     c                   write     or41t1c
116800020808     c                   eval      *in92 = *off
116900020808 e3  c                   endif
117000020808 b3  c                   if        w_rorone <> *blanks or
117100020808     c                             w_rornqd <> *blanks or
117200020808     c                             w_rornbl <> *blanks or
117300020808     c                             w_rormdr <> *blanks or
117400031105     c                             w_rornas <> *blanks
117500020808      * giro data immissione ORM
117600020808     c                   clear                   wlbdat
117700020808     c                   move      ormdao        g02inv
117800020808     c                   movel     '3'           G02err
117900020808     c                   call      'XSRDA8'
118000020808     c                   parm                    wlbdat
118100020808     c                   movel     g02dat        w_ormdao
118200020808      * dettaglio
118300020808     c                   write     or41d1c
118400020808 e3  c                   endif
118500020808 e2  c                   endif
118600020808
118700020808     c                   EndSr
118800020808
118900010612      *--------------------------------------------------------------*
119000010612      * CONTROLLO SE LA DATA CHE STO ELABORANDO E' FESTIVO
119100010612      *--------------------------------------------------------------*
119200010614     c     SR_FESTA      begsr
119300010612
119400010612     c                   eval      wokfestae = *off
119500010612     c                   eval      wokfestar = *off
119600010612
119700010612     c     *iso          movel     dataela       wdtda
119800010612     c                   extrct    wdtda:*y      kann
119900010612     c                   extrct    wdtda:*m      kmes
120000010612     c                   extrct    wdtda:*d      wgiorno
120100011217     c                   clear                   ktfa
120200010612      * cerco per il p.o. emissione
120300010612     c                   if        wpoe = *on
120400011217     c                   eval      ktfp = dspoele
120500010612     c     kazcln        chain     azcln01l
120600010612     c                   if        %found(azcln01l) and
120700010612     c                             (MAT(wgiorno) =  'F'
120800010612     c                                    or  POM(wgiorno) =  'F')
120900010612     c                   eval      wokfestae = *on
121000010612     c                   endif
121100010612     c                   endif
121200010612
121300010612      * cerco per il p.o. ritiro
121400010612     c                   if        wpor = *on
121500011217     c                   eval      ktfp = dspoelr
121600010612     c     kazcln        chain     azcln01l
121700010612     c                   if        %found(azcln01l) and
121800010612     c                             (MAT(wgiorno) =  'F'
121900010612     c                                    or  POM(wgiorno) =  'F')
122000010612     c                   eval      wokfestar = *on
122100010612     c                   endif
122200010612     c                   endif
122300010612
122400010612     c                   endsr
122500010614
122600010608      *--------------------------------------------------------------*
122700010608      * CALCOLO DATE X AFFIDABILITA'
122800010608      *--------------------------------------------------------------*
122900010614     c     SR_DATA       begsr
123000010608
123100010612      * calcolo la data da - 1
123200010611     c     *iso          movel     dataela       wdtda
123300010608     c                   do        *hival
123400010608     c                   subdur    1:*d          wdtda
123500010608     c                   extrct    wdtda:*y      kann
123600010608     c                   extrct    wdtda:*m      kmes
123700010608     c                   extrct    wdtda:*d      wgiorno
123800011217     c                   clear                   ktfa
123900020808     c                   If        Dspoelr <> *Zeros
124000020808     c                   eval      ktfp = Dspoelr
124100020808     c                   Else
124200020808     c                   Eval      Ktfp = Dspoele
124300020808     c                   EndIf
124400010608     c     kazcln        chain     azcln01l
124500010608     c                   if        not%found(azcln01l)
124600010608     c                   leave
124700010608     c                   endif
124800010608     c                   if        MAT(wgiorno) =  'F'
124900010608     c                             or  POM(wgiorno) =  'F'
125000010608     c                   iter
125100010608     c                   endif
125200010608     c                   leave
125300010608     c                   enddo
125400010611     c                   move      wdtda         wdtda01
125500010608
125600010608      * calcolo la data da - 2
125700010611     c     *iso          movel     wdtda01       wdtda
125800010608     c                   do        *hival
125900010611     c                   subdur    1:*d          wdtda
126000010608     c                   extrct    wdtda:*y      kann
126100010608     c                   extrct    wdtda:*m      kmes
126200010608     c                   extrct    wdtda:*d      wgiorno
126300011217     c                   clear                   ktfa
126400020808     c                   If        Dspoelr <> *Zeros
126500020808     c                   eval      ktfp = Dspoelr
126600020808     c                   Else
126700020808     c                   Eval      Ktfp = Dspoele
126800020808     c                   EndIf
126900010608     c     kazcln        chain     azcln01l
127000010608     c                   if        not%found(azcln01l)
127100010608     c                   leave
127200010608     c                   endif
127300010608     c                   if        MAT(wgiorno) =  'F'
127400010608     c                             or  POM(wgiorno) =  'F'
127500010608     c                   iter
127600010608     c                   endif
127700010608     c                   leave
127800010608     c                   enddo
127900010611     c                   move      wdtda         wdtda02
128000010608
128100010608     c                   endsr
128200010614
128300010605      *--------------------------------------------------------------*
128400010605      * CONTROLLO CON IL PO EMISSIONE
128500010605      *--------------------------------------------------------------*
128600010605     c     SR_POE        begsr
128700010620
128800010620      * se la data del p.o. emissione non è festa controllare se il giorno prima
128900010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
129000010620 b1  c                   if        wokfestae = *off
129100010620     c                   eval      dspoel = dspoele
129200010620      *                            ========
129300010620     c                   exsr      SR_DTFES
129400010620      *                            ========
129500010612
129600010620 b2  c                   if        ormdao = dataela or
129700010620     c                             (wdatafes > 0 and ormdao >= wdatafes and
129800010620     c                              ormdao < dataela)
129900010611     c                   eval      dspoel = dspoele
130000030912     c                   eval      dspoel1= dspoele
130100030912     c                   eval      dsdata1 = dsdata
130200030918     c                   Eval      dstrc = 'A'
130300010608      * controllo se esiste già un elemento di sk = alla mia key
130400010608      * e carico la sk relativa al file
130500010614      *                            ==========
130600010608     c                   exsr      SR_CONTRSK
130700010614      *                            ==========
130800010605
130900010605     ?* immessi ?
131000010605     c                   eval      f_roroim = f_roroim +1
131100010605     ?* commissionati ?
131200010620 b3  c                   if        ormpoe <> ormpor
131300010605     c                   eval      f_roroco = f_roroco +1
131400010620 e3  c                   endif
131500010605     ?* codificati ?
131600010620 b3  c                   if        ormcra <> *zeros
131700010605     c                             or ormcor <> *zeros
131800010605     c                   eval      f_rorico = f_rorico +1
131900010605     ?* non codificati ?
132000010620 x3  c                   else
132100010605     c                   eval      f_rorinc = f_rorinc +1
132200010620 e3  c                   endif
132300010605     ?* fissi ?
132400010620 b3  c                   if        ormtco = 'S'
132500010605     c                   eval      f_rorifi = f_rorifi +1
132600010620 e3  c                   endif
132700010605     ?* da centralino ?
132800010620 b3  c                   if        ormtco = 'M'  or
132900010606     c                             ormtco = 'P'
133000010605     c                   eval      f_rorice = f_rorice +1
133100010620 e3  c                   endif
133200010605     ?* da internet ?
133300010620 b3  c                   if        ormtco = 'I'
133400010605     c                   eval      f_roriin = f_roriin +1
133500010620 e3  c                   endif
133600010605     ?* da vas ?
133700010620 b3  c                   if        ormtco = 'F'
133800010605     c                   eval      f_roriva = f_roriva +1
133900010620 e3  c                   endif
134000010605     ?* da call center ?
134100010620 b3  c                   if        ormtco = 'C'
134200010605     c                   eval      f_roricc = f_roricc +1
134300010620 e3  c                   endif
134400010605
134500010612     c                   eval      rfile(xf) = dswfror
134600010607     c                   eval      wokstp = *on
134700031202      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
134800031202     c                   Eval      sxf = xf
134900010620 e2  c                   endif
135000010605
135100010620 e1  c                   endif
135200010620
135300010605     c                   endsr
135400010614
135500010620      *--------------------------------------------------------------*
135600010620      * CERCO IL PRIMO GIORNO FESTIVO PRECEDENTE
135700010620      *--------------------------------------------------------------*
135800010620     c     SR_DTFES      begsr
135900010620
136000010620      * calcolo il primo giorno festivo
136100010620     c     *iso          movel     dataela       wdtda
136200010620     c                   do        *hival
136300010620     c                   subdur    1:*d          wdtda
136400010620     c                   extrct    wdtda:*y      kann
136500010620     c                   extrct    wdtda:*m      kmes
136600010620     c                   extrct    wdtda:*d      wgiorno
136700011217     c                   clear                   ktfa
136800011217     c                   eval      ktfp = dspoel
136900010620     c     kazcln        chain     azcln01l
137000010620     c                   if        not%found(azcln01l)
137100010620     c                   leave
137200010620     c                   endif
137300010620     c                   if        MAT(wgiorno) =  'F'
137400010620     c                             or  POM(wgiorno) =  'F'
137500010620     c                   move      wdtda         wdatafes
137600010620     c                   iter
137700010620     c                   endif
137800010620     c                   leave
137900010620     c                   enddo
138000010620
138100010620     c                   endsr
138200010620
138300010608      *--------------------------------------------------------------*
138400010608      * PULISCO CAMPI DI COMODO
138500010608      *--------------------------------------------------------------*
138600010614     c     SR_PULISCI    begsr
138700010608
138800010608     c                   clear                   wrorode
138900010620     c                   clear                   wrordan
139000010608     c                   clear                   wrordit
139100010608     c                   clear                   wrordr1
139200010608     c                   clear                   wrordr2
139300010608     c                   clear                   wrordro
139400010608     c                   clear                   wroroes
139500010614     c                   clear                   wrorean
139600010608     c                   clear                   wroreit
139700010608     c                   clear                   wrorer1
139800010608     c                   clear                   wrorer2
139900010608     c                   clear                   wrorero
140000010608     c                   clear                   wrorone
140100010620     c                   clear                   wrornan
140200010608     c                   clear                   wrornit
140300010608     c                   clear                   wrornr1
140400010608     c                   clear                   wrornr2
140500010608     c                   clear                   wrornro
140600010608     c                   clear                   wrorn01
140700010608     c                   clear                   wrorn02
140800010608     c                   clear                   wrorn03
140900010608     c                   clear                   wrorn04
141000010608     c                   clear                   wrorn05
141100010608     c                   clear                   wrorn06
141200010608     c                   clear                   wrorn07
141300010608     c                   clear                   wrorn08
141400010608     c                   clear                   wrorn09
141500010608     c                   clear                   wrorn10
141600010608     c                   clear                   wroroea
141700010620     c                   clear                   wroraan
141800010608     c                   clear                   wrorait
141900010608     c                   clear                   wrorar1
142000010608     c                   clear                   wrorar2
142100010608     c                   clear                   wroraro
142200010608     c                   clear                   wrora01
142300010608     c                   clear                   wrora02
142400010608     c                   clear                   wrora03
142500010608     c                   clear                   wrora04
142600010608     c                   clear                   wrora05
142700010608     c                   clear                   wrora06
142800010608     c                   clear                   wrora07
142900010608     c                   clear                   wrora08
143000010608     c                   clear                   wrora09
143100010608     c                   clear                   wrora10
143200010608     c                   clear                   wrora11
143300010608     c                   clear                   wrora12
143400010608     c                   clear                   wrora13
143500010608     c                   clear                   wrora14
143600010608     c                   clear                   wrora15
143700010608     c                   clear                   wrornqd
143800010608     c                   clear                   wrornbl
143900010614     c                   clear                   wrormdr
144000010614     c                   clear                   wrornas
144100030912     c                   clear                   wroroned
144200030912     c                   clear                   wrorn01d
144300030912     c                   clear                   wrorn02d
144400030912     c                   clear                   wrorn03d
144500030912     c                   clear                   wrorn04d
144600030912     c                   clear                   wrorn05d
144700030912     c                   clear                   wrorn06d
144800030912     c                   clear                   wrorn07d
144900030912     c                   clear                   wrorn08d
145000030912     c                   clear                   wrorn09d
145100030912     c                   clear                   wrorn10d
145200030917     c                   clear                   wrorifian
145300030917     c                   clear                   wrorifine
145400030917     c                   clear                   wrorifid
145500030917     c                   clear                   wroricean
145600030917     c                   clear                   wroricene
145700030917     c                   clear                   wroriced
145800030917     c                   clear                   wroriinan
145900030917     c                   clear                   wroriinne
146000030917     c                   clear                   wroriind
146100030917     c                   clear                   wrorivaan
146200030917     c                   clear                   wrorivane
146300030917     c                   clear                   wrorivad
146400030917     c                   clear                   wroriccan
146500030917     c                   clear                   wroriccne
146600030917     c                   clear                   wroriccd
146700010620
146800010621     c                   clear                   w_rorone
146900010621     c                   clear                   w_rornqd
147000010621     c                   clear                   w_rornbl
147100010621     c                   clear                   w_rornas
147200010621     c                   clear                   w_rormdr
147300010608
147400010608     c                   endsr
147500010614
147600010605      *--------------------------------------------------------------*
147700010605      * CONTROLLO CON IL PO RITIRO
147800010605      *--------------------------------------------------------------*
147900010605     c     SR_POR        begsr
148000010620
148100010620     c                   clear                   wdatafes
148200010620
148300010620      * se la data del p.o. ritiro non è festa controllare se il giorno prima
148400010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
148500010620 b1  c                   if        wokfestar = *off
148600010620     c                   eval      dspoel = dspoelr
148700010620      *                            ========
148800010620     c                   exsr      SR_DTFES
148900010620      *                            ========
149000010620 e1  c                   endif
149100010608
149200010608     c                   eval      kormpoe = ormpoe
149300010608     c                   eval      kormnsr = ormnsr
149400010608     c                   eval      kormnor = ormnor
149500010608     c                   eval      kormnrv = ormnrv
149600010612
149700010614      * se data p.o. ritiro non è festa cerco gli ORM da eseguire
149800010619      * e le modifiche della data di ritiro
149900010613 b1  c                   if        wokfestar = *off
150000010606
150100010614     ?* ORM da eseguire oggi data ritiro = data elaborazione
150200010618 b2  c                   if        ormdar = dataela
150300010618     ?* data ultima fase < data elaborazione e già eseguito non devo elaborarlo
150400010618     ?*                                                     -------------------
150500010618     c                   if        ormdfo < dataela and ormfao >= 600
150600010618     c                   goto      endpor
150700010618     c                   endif
150800010618     c                   eval      wrorode = wrorode +1
150900010607     c                   eval      wrordit = wrordit +1
151000010607     c                   eval      wokstp = *on
151100010612     c                   eval      wokrfile = *on
151200010614 e2  c                   endif
151300010608
151400010614     ?* ORM da eseguire (non eseguiti entro oggi) data ritiro < data elaborazione
151500010613 b2  c                   if        ormdar < dataela
151600010608     c                   eval      wokrode = *off
151700010620
151800010620     c                   eval      kormdfo = ormdfo
151900010620     c                   eval      kormofo = ormofo
152000010620     c                   eval      kormfao = ormfao
152100010620     c     korf01        setll     fnorf01l
152200010620      * se la data ultima fase "testata" è >= della data elaborazione
152300010620      * o è <  della data elaborazione
152400010620      *   e >= alla data festiva (se sto elaborando un lunedì)
152500010620      * devo leggere la fase prima
152600010620     c                   select
152700010620 b3  c                   when      ormdfo >= dataela or
152800010620 b3  c                             (ormdfo < dataela  and
152900010620     c                             (wdatafes > 0 and ormdfo >= wdatafes))
153000010620     c     korf          readpe    fnorf01l
153100010620      * se la data ultima fase "testata" è <  della data elaborazione
153200010620      * e non sto elaborando un lunedì
153300010620      * devo leggere la fase stessa
153400010620     c                   other
153500010620     c     korf          reade     fnorf01l
153600010620 e3  c                   endsl
153700010620
153800030807      * senza fase 600-900-910-999 fino alla data elaborazione esclusa
153900010614 b3  c                   dow       not %eof(fnorf01l)
154000010620 b4  c                   if        orfdae < dataela
154100010620 b5  c                   if        orffar >=600
154200011019      * Attenzione!! E' possibile che sia stata fatta una modifica fase
154300011019      * se la fase della testata ORM è 500 o è 100 e la fase precedente
154400011019      * sul file delle fasi è una fase di fatto DEVO dare l'ORM come
154500011019      * da eseguire
154600011019     c                   if        ormfao <> 500 and ormfao <> 100
154700010608     c                   leave
154800011019     c                   endif
154900010614 e5  c                   endif
155000060926      * salto le fasi che non devono essere elaborate
155100060926     c     orffar        Lookup    skfar                                  30
155200060926     c**!!!              If        OrfFar <> 300
155300060926     c                   if        not *in30
155400010608     c                   eval      wokrode = *on
155500031105     c                   EndIf
155600010614 e4  c                   endif
155700010613     c     korf          readpe    fnorf01l
155800010614 e3  c                   enddo
155900010613     ?* da eseguire (in ritardo)
156000010613 b3  c                   if        wokrode = *on
156100010608     c                   eval      wrorode = wrorode +1
156200010608      * calcolo affidabilità. Uso >= per i da eseguire nei festivi
156300010614 b4  c                   select
156400010608     c                   when      ormdar >= wdtda01
156500010608     c                   eval      wrordr1 = wrordr1 +1
156600010608     c                   when      ormdar >= wdtda02
156700010608     c                   eval      wrordr2 = wrordr2 +1
156800010614 x4  c                   other
156900010608     c                   eval      wrordro = wrordro +1
157000010614 e4  c                   endsl
157100010608     c                   eval      wokstp = *on
157200010614     c                   eval      wokrfile = *on
157300010614     ?* è stato eseguito
157400010613 x3  c                   else
157500010614      * e la data ultima fase minore data elaborazione non devo elaborarlo
157600010614      *                                                -------------------
157700010620 b4  c                   if        ormdfo < dataela
157800010619     ?* ma prima controllo se deve andare a far parte dei non bollettati
157900010619     ?*                                                   --------------
158000010619 b5  c                   if        ormfao = 600 and
158100010619     c                             ormdfo >= wdtda01
158200010619     c                   eval      wrornbl = wrornbl +1
158300010621     c   11              eval      w_rornbl = 'X'
158400010619     c                   eval      wokstp = *on
158500010619     c                   eval      wokrfile = *on
158600010619 e5  c                   endif
158700010608     c                   goto      endpor
158800010614 e4  c                   endif
158900010613 e3  c                   endif
159000010613 e2  c                   endif
159100040420
159200040420     ?* ORM con data ritiro > data elaborazione
159300040420      * devo controllare che non sia stata fatta una fase di dirottamento dalla data elaborazione
159400040420      * in poi
159500040420if  2c                   If        OrmDar > Dataela
159600040420     c                   Eval      kormfao = 200
159700040420     c                   Eval      kormdfo = Dataela
159800040420     c     kOrf03        Setll     Fnorf03l
159900040420      * Se trovo già una fase di dirottamento in data elaborazione non devo elabore l'ORM
160000040420      *                                                            ----------------------
160100040420     c                   If        %Equal(Fnorf03l)
160200040420     c                   Goto      endpor
160300040420     c                   EndIf
160400040615      * Altrimenti leggo dalla data statistica in poi
160500040420do  3c                   Do        *Hival
160600040420     c     kOrf04        Reade     Fnorf03l
160700040420     c                   If        %Eof(Fnorf03l)
160800040420     c                   Leave
160900040420     c                   EndIf
161000040420      * Se trovo una fase di dirottamento non devo elabore l'ORM
161100040420      *                                   ----------------------
161200040420     c                   Goto      endpor
161300040420e   3c                   EndDo
161400040420e   2c                   EndIf
161500010618
161600010619      * cerco le modifiche alla data ritiro
161700010618      *                            =========
161800010618     c                   exsr      SR_MODRIT
161900010618      *                            =========
162000010619
162100010619 e1  c                   endif
162200010608
162300010614      * cerco sulle fasi sempre
162400010614      *                  ------
162500010614      *                            =======
162600010606     c                   exsr      SR_FASI
162700010614      *                            =======
162800010608
162900010608     c     endpor        endsr
163000010618
163100010618      *--------------------------------------------------------------*
163200010618      * CALCOLO MODIFICHE DATA RITIRO
163300010618      *--------------------------------------------------------------*
163400010618     c     SR_MODRIT     begsr
163500010618
163600010618     c                   eval      korvdtv = dataela
163700010620     c                   if        wdatafes > 0
163800010620     c                   eval      korvdtv = wdatafes
163900010620     c                   endif
164000010618     c                   clear                   sav_orvdar
164100010618
164200010620     c     korv          setll     fnorv01l
164300010618     c                   do        *hival
164400010620     c     korf          reade     fnorv01l
164500010618     c                   if        %eof(fnorv01l)
164600010618     c                   leave
164700010618     c                   endif
164800010620     c                   if        orvdtv > dataela
164900010620     c                   leave
165000010620     c                   endif
165100010620     c                   if        orvdar <> ormdar and
165200010620     c                             orvdar <> sav_orvdar
165300010618     c                   eval      wrormdr = wrormdr +1
165400010621     c   11              eval      w_rormdr = 'X'
165500010618     c                   eval      sav_orvdar = orvdar
165600010618     c                   eval      wokstp = *on
165700010618     c                   eval      wokrfile = *on
165800010618     c                   endif
165900010618     c                   enddo
166000010618
166100010618     c                   endsr
166200010618
166300010608      *--------------------------------------------------------------*
166400010608      * CONTROLLO LA SITUAZIONE DELL'ORM SULLE FASI
166500010608      *--------------------------------------------------------------*
166600010608     c     SR_FASI       begsr
166700010608
166800040421      * ATTENZIONE!!! la fase 200 di dirottato fa fermare la lettura delle fasi, appena
166900040421      *               la trovo esco dalla routine
167000040421
167100010608     c                   clear                   orfnor
167200010608     c                   clear                   orfpoe
167300010608     c                   clear                   orfpog
167400010608     c                   clear                   orfore
167500010608
167600010608     ?* se la data elaborazione è >= data ultima fase "testata"
167700010608     ?* mi posiziono sul file delle fasi con la data/ora/fase della
167800010608     ?* testata ORM
167900010614 b1  c                   if        dataela >= ormdfo
168000010608     c                   eval      kormdfo = ormdfo
168100010608     c                   eval      kormofo = ormofo
168200010608     c                   eval      kormfao = ormfao
168300010608     c     korf01        setll     fnorf01l
168400010608     c     korf01        reade     fnorf01l
168500010614 e1  c                   endif
168600010608     ?* se la data elaborazione è < data ultima fase  "testata"
168700010608     ?* mi posizione sul file delle fasi con la data elaborazione
168800030917     ?* fase = 0 e ora fase = a tutti '9' per leggere l'ultima fase fatta nel giorno
168900030917     ?* di elaborazione statistica
169000010614 b1  c                   if        dataela < ormdfo
169100010608     c                   eval      kormdfo = dataela
169200010608     c                   eval      kormofo = *all'9'
169300010608     c                   clear                   kormfao
169400010608     c     korf01        setgt     fnorf01l
169500010608     c     korf          readpe    fnorf01l
169600010705      * se la data di ritiro = alla data elaborazione e trovo subito il fine file
169700010705      * sulle fasi vuole dire che la prima fase è stata eseguita dopo la data elaborazione
169800010705      * quindi l'orm è non eseguito (es. orm copiati con data immissione e data ritiro = al
169900010705      * vecchio orm, ma la data della fase = alla data del giorno della copia, errore già
170000010705      * sistemato in gestione orm)
170100010705 b2  c                   if        %eof(fnorf01l)
170200010705     c                             and ormdar = dataela
170300010705     c                   eval      wrorone = wrorone +1
170400010717     c   11              eval      w_rorone = 'X'
170500010705     c                   eval      wrornit = wrornit +1
170600010705     c                   eval      wokstp = *on
170700010705     c                   eval      wokrfile = *on
170800010705 e2  c                   endif
170900010614 e1  c                   endif
171000030912      * _____________
171100010608      * leggo le fasi
171200030912      * -------------
171300010613 b1  c                   dow       not %eof(fnorf01l)
171400040421      * ____________________
171500040421      * Fase 200 devo uscire
171600040421      * --------------------
171700040618if  2c                   If        OrfFar = 200
171800040421     c                   Leave
171900040421e   2c                   EndIf
172000040421
172100010608     c                   eval      wok900 = *off
172200010615     c                   eval      wok600 = *off
172300030912      * _____________________________
172400010608      * data fase = data elaborazione
172500030912      * -----------------------------
172600010620 b2  c                   if        orfdae = dataela or
172700010620     c                             (wdatafes > 0 and orfdae >= wdatafes and
172800010620     c                              orfdae < dataela)
172900010614     ?* ESEGUITI
173000010614     ?* ORM eseguiti (fase 900) ?
173100010614 b3  c                   if        orffar = 900
173200010614      * devo controllare se c'è una fase 600 prima
173300030917      * il controllo è così:
173400030917      *     - se data fase < data elaborazione e fase >= 600 è stato fatto prima
173500030917      *       quindi lo devo saltare
173600030917      *     - se no vuol dire che è stato fatto in data elaborazione e lo do per fatto
173700040421      * qui fa solo UNA readpe quindi non faccio il controllo della fase 200
173800010614     c                   eval      wokroes = *off
173900010614     c     korf          readpe    fnorf01l
174000010615 b5  c                   if        orffar >= 600  and
174100010615     c                             orfdae < dataela
174200010615     c                   eval      wok600 = *on
174300030917 x5  c                   else
174400010614     c                   eval      wokroes = *on
174500030917 e5  c                   endif
174600010614
174700010614 b4  c                   if        wokroes = *on
174800010614     c                   eval      wok900 = *on
174900010608     c                   eval      wroroes = wroroes +1
175000010608      * calcolo affidabilità. Uso >= per i ritiri nei festivi
175100010614 b5  c                   select
175200010614     c                   when      ormdar > dataela
175300010614     c                   eval      wrorean = wrorean +1
175400010614     c                   when      ormdar = dataela
175500010608     c                   eval      wroreit = wroreit +1
175600010608     c                   when      ormdar >= wdtda01
175700010608     c                   eval      wrorer1 = wrorer1 +1
175800010608     c                   when      ormdar >= wdtda02
175900010608     c                   eval      wrorer2 = wrorer2 +1
176000010614 x5  c                   other
176100010608     c                   eval      wrorero = wrorero +1
176200010614 e5  c                   endsl
176300010608     c                   eval      wokstp = *on
176400010614     c                   eval      wokrfile = *on
176500010608     c                   leave
176600010614 x4  c                   else
176700010615 b5  c                   if        wok600 = *on
176800010615     c                   leave
176900010615 e5  c                   endif
177000010613 e4  c                   endif
177100010613 e3  c                   endif
177200010614
177300010614     ?* ORM eseguiti (fase 600) ?
177400010614 b3  c                   if        orffar = 600
177500010614     c                   eval      wroroes = wroroes +1
177600010614      * calcolo affidabilità. Uso >= per i ritiri nei festivi
177700010614 b4  c                   select
177800010614     c                   when      ormdar > dataela
177900010614     c                   eval      wrorean = wrorean +1
178000010614     c                   when      ormdar = dataela
178100010614     c                   eval      wroreit = wroreit +1
178200010614     c                   when      ormdar >= wdtda01
178300010614     c                   eval      wrorer1 = wrorer1 +1
178400010614     c                   when      ormdar >= wdtda02
178500010614     c                   eval      wrorer2 = wrorer2 +1
178600010614 x4  c                   other
178700010614     c                   eval      wrorero = wrorero +1
178800010614 e4  c                   endsl
178900010615     ?* ORM non bollettati
179000010615     c                   if        orfdae >= wdtda01 and
179100010618     c                             orfdae < dataela  and
179200010615     c                             ormdar < dataela
179300010615     c                   eval      wrornbl = wrornbl +1
179400010621     c   11              eval      w_rornbl = 'X'
179500010615     c                   endif
179600010614     c                   eval      wokstp = *on
179700010614     c                   eval      wokrfile = *on
179800010614     c                   leave
179900010614 e3  c                   endif
180000010612
180100010614      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
180200010613 b3  c                   if        wokfestar = *off
180300060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
180400010705      * in questo caso devo tornare a leggere la precedente
180500060926     c     orffar        Lookup    skfar                                  30
180600060926 b4  c**!!!              if        orffar = 300
180700060926     c                   if        *in30
180800010705     c                   goto      passa
180900010705 e4  c                   endif
181000040421     ?* ORM chiusi/eseguiti anomali (fase 910 - 999)
181100031105 b4  c                   if        orffar = 999 or OrfFar = 910
181200030917     c                   eval      wroroea = wroroea +1
181300030917      * calcolo affidabilità. Uso >= per gli anomali nei festivi
181400030917 b5  c                   select
181500030917     c                   when      ormdar > dataela
181600030917     c                   eval      wroraan = wroraan +1
181700030917     c                   when      ormdar = dataela
181800030917     c                   eval      wrorait = wrorait +1
181900030917     c                   when      ormdar >= wdtda01
182000030917     c                   eval      wrorar1 = wrorar1 +1
182100030917     c                   when      ormdar >= wdtda02
182200030917     c                   eval      wrorar2 = wrorar2 +1
182300030917 x5  c                   other
182400030917     c                   eval      wroraro = wroraro +1
182500030917 e5  c                   endsl
182600030917      * memorizzo il tipo comunicazione ORM
182700030917     ?* fissi ?
182800030917 b5  c                   if        ormtco = 'S'
182900030917     c                   eval      wrorifian = wrorifian +1
183000030917 e5  c                   endif
183100030917     ?* da centralino ?
183200030917 b5  c                   if        ormtco = 'M'  or
183300030917     c                             ormtco = 'P'
183400030917     c                   eval      wroricean = wroricean +1
183500030917 e5  c                   endif
183600030917     ?* da internet ?
183700030917 b5  c                   if        ormtco = 'I'
183800030917     c                   eval      wroriinan = wroriinan +1
183900030917 e5  c                   endif
184000030917     ?* da vas ?
184100030917 b5  c                   if        ormtco = 'F'
184200030917     c                   eval      wrorivaan = wrorivaan +1
184300030917 e5  c                   endif
184400030917     ?* da call center ?
184500030917 b5  c                   if        ormtco = 'C'
184600030917     c                   eval      wroriccan = wroriccan +1
184700030917 e5  c                   endif
184800030917     c                   eval      cauoea = orfcar
184900030917     c                   if        orfcar = '80'
185000030917     c                   eval      wrora01 = wrora01 +1
185100030917     c                   endif
185200030917     c                   if        orfcar = '81'
185300030917     c                   eval      wrora02 = wrora02 +1
185400030917     c                   endif
185500030917     c                   if        orfcar = '90'
185600030917     c                   eval      wrora03 = wrora03 +1
185700030917     c                   endif
185800030917     c                   if        orfcar = '91'
185900030917     c                   eval      wrora04 = wrora04 +1
186000030917     c                   endif
186100030917     c                   if        orfcar = '92'
186200030917     c                   eval      wrora05 = wrora05 +1
186300030917     c                   endif
186400030917     c                   if        orfcar = '93'
186500030917     c                   eval      wrora06 = wrora06 +1
186600030917     c                   endif
186700030917     c                   if        orfcar = '94'
186800030917     c                   eval      wrora07 = wrora07 +1
186900030917     c                   endif
187000030917     c                   if        orfcar = '95'
187100030917     c                   eval      wrora08 = wrora08 +1
187200030917     c                   endif
187300031105     c                   if        orfcar = '82'
187400031105     c                   eval      wrora09 = wrora09 +1
187500031105     c                   endif
187600040701     c                   if        orfcar = '96'
187700040701     c                   eval      wrora10 = wrora10 +1
187800040701     c                   endif
187900040701     c                   if        orfcar = '97'
188000040701     c                   eval      wrora11 = wrora11 +1
188100040701     c                   endif
188200110718     c**!!!              if        orfcar = '98'
188300110718     c**!!!              eval      wrora12 = wrora12 +1
188400110718     c**!!!              endif
188500030917     c                   eval      wokstp = *on
188600030917     c                   eval      wokrfile = *on
188700030917     c                   leave
188800030917 e4  c                   endif
188900010608     ?* ORM non eseguiti
189000030916 b4  c                   if        orffar < 600
189100030916     ?* con data ritiro <= data elaborazione
189200030916 b4a c                   If        ormdar <= dataela
189300010608     c                   eval      wrorone = wrorone +1
189400010621     c   11              eval      w_rorone = 'X'
189500030917     c                   eval      wokstp = *on
189600030917     c                   eval      wokrfile = *on
189700010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
189800010614 b5  c                   select
189900010620     c                   when      ormdar > dataela
190000010620     c                   eval      wrornan = wrornan +1
190100010608     c                   when      ormdar = dataela
190200010608     c                   eval      wrornit = wrornit +1
190300010608     c                   when      ormdar >= wdtda01
190400010608     c                   eval      wrornr1 = wrornr1 +1
190500010608     c                   when      ormdar >= wdtda02
190600010608     c                   eval      wrornr2 = wrornr2 +1
190700010614 x5  c                   other
190800010608     c                   eval      wrornro = wrornro +1
190900010614 e5  c                   endsl
191000030912      * memorizzo il tipo comunicazione ORM
191100030912     ?* fissi ?
191200030912 b5  c                   if        ormtco = 'S'
191300030917     c                   eval      wrorifine = wrorifine +1
191400030912 e5  c                   endif
191500030912     ?* da centralino ?
191600030912 b5  c                   if        ormtco = 'M'  or
191700030912     c                             ormtco = 'P'
191800030917     c                   eval      wroricene = wroricene +1
191900030912 e5  c                   endif
192000030912     ?* da internet ?
192100030912 b5  c                   if        ormtco = 'I'
192200030917     c                   eval      wroriinne = wroriinne +1
192300030912 e5  c                   endif
192400030912     ?* da vas ?
192500030912 b5  c                   if        ormtco = 'F'
192600030917     c                   eval      wrorivane = wrorivane +1
192700030912 e5  c                   endif
192800030912     ?* da call center ?
192900030912 b5  c                   if        ormtco = 'C'
193000030917     c                   eval      wroriccne = wroriccne +1
193100030912 e5  c                   endif
193200010608     ?* ORM non eseguiti con causale ?
193300010613 b5  c                   if        orffar = 500
193400010619     c                   eval      cauone = orfcar
193500010608     c                   if        orfcar = '01'
193600010608     c                   eval      wrorn01 = wrorn01 +1
193700010608     c                   endif
193800010608     c                   if        orfcar = '02'
193900010608     c                   eval      wrorn02 = wrorn02 +1
194000010608     c                   endif
194100010608     c                   if        orfcar = '03'
194200010608     c                   eval      wrorn03 = wrorn03 +1
194300010608     c                   endif
194400020527     c                   if        orfcar = '04'
194500020527     c                   eval      wrorn04 = wrorn04 +1
194600020527     c                   endif
194700030917     c                   leave
194800010613 e5  c                   endif
194900030917     ?* ORM non eseguiti no in fase 500 ?
195000030917     ?* ORM non assegnati
195100030917 b5  c                   if        orffar < 400
195200030917     c                   eval      wrornas = wrornas +1
195300030917     c   11              eval      w_rornas = 'X'
195400030917 e5  c                   endif
195500030912      * devo controllare se nel giorno è stata fatta una fase 500
195600030912     c     kOrf02        Readpe    Fnorf01l
195700030917 b5  c                   Dow       Not %Eof(Fnorf01l)
195800040421      * se trovo fase 200 esco
195900040618if  6c                   If        OrfFar = 200
196000040421     c                   Leave
196100040421e   6c                   EndIf
196200030917 b6  c                   If        OrfFar = 500
196300030912      * se trovo la fase 500 memorizzo la causale
196400030912     c                   Eval      %Subst(cauone:2:2) = OrfCar
196500030919     c                   Move      ')'           cauone
196600030919     c                   Movel     '('           cauone
196700030912     c                   If        OrfCar = '01'
196800030912     c                   Eval      wrorn01 = wrorn01 +1
196900030912     c                   Endif
197000030912     c                   If        OrfCar = '02'
197100030912     c                   Eval      wrorn02 = wrorn02 +1
197200030912     c                   Endif
197300030912     c                   If        OrfCar = '03'
197400030912     c                   Eval      wrorn03 = wrorn03 +1
197500030912     c                   Endif
197600030912     c                   If        OrfCar = '04'
197700030912     c                   Eval      wrorn04 = wrorn04 +1
197800030912     c                   Endif
197900030912     c                   Leave
198000030917 e6  c                   Endif
198100030912     c     kOrf02        Readpe    Fnorf01l
198200030917 e5  c                   EndDo
198300030917     c                   Leave
198400030916 e4a c                   endif
198500030916     ?* con data ritiro > data elaborazione
198600030919 b4a c                   If        ormdar > dataela
198700030929     ?* è un ORM non fatto se nel giorno c'è una fase 500 o 390
198800031105     ?* quindi lo metto nella colonna dei non eseguiti e anche in quella dei da eseguire
198900030929 b5  c                   If        OrfFar = 500 or OrfFar = 390
199000031105     c                   eval      wrorone = wrorone +1
199100031105     c                   eval      wroroned = wroroned +1
199200031105     c   11              eval      w_rorone = 'X'
199300030917     c                   eval      wokstp = *on
199400030917     c                   eval      wokrfile = *on
199500031105      * da eseguire
199600031105     c                   eval      wrorode = wrorode +1
199700031105      * sempre in anticipo xchè data ritiro > data elaborazione
199800031105     c                   eval      wrordan = wrordan +1
199900031106     c                   eval      wrornan = wrornan +1
200000030912      * memorizzo il tipo comunicazione ORM
200100030912     ?* fissi ?
200200030917 b6  c                   if        ormtco = 'S'
200300031105     c                   eval      wrorifine = wrorifine +1
200400031105     c                   eval      wrorifid = wrorifid +1
200500030917 e6  c                   endif
200600030912     ?* da centralino ?
200700030917 b6  c                   if        ormtco = 'M'  or
200800030912     c                             ormtco = 'P'
200900031105     c                   eval      wroricene = wroricene +1
201000031105     c                   eval      wroriced = wroriced +1
201100030917 e6  c                   endif
201200030912     ?* da internet ?
201300030917 b6  c                   if        ormtco = 'I'
201400031105     c                   eval      wroriinne = wroriinne +1
201500031105     c                   eval      wroriind = wroriind +1
201600030917 e6  c                   endif
201700030912     ?* da vas ?
201800030917 b6  c                   if        ormtco = 'F'
201900031105     c                   eval      wrorivane = wrorivane +1
202000031105     c                   eval      wrorivad = wrorivad +1
202100030917 e6  c                   endif
202200030912     ?* da call center ?
202300030917 b6  c                   if        ormtco = 'C'
202400031105     c                   eval      wroriccne = wroriccne +1
202500031105     c                   eval      wroriccd = wroriccd +1
202600030917 e6  c                   endif
202700030912     ?* ORM non eseguiti con causale ?
202800030929 b6  c                   If        OrfFar = 500
202900031105     c                   eval      cauone = orfcar
203000030912     c                   if        orfcar = '01'
203100031105     c                   eval      wrorn01 = wrorn01 +1
203200031105     c                   eval      wrorn01d = wrorn01d +1
203300030912     c                   endif
203400030912     c                   if        orfcar = '02'
203500031105     c                   eval      wrorn02 = wrorn02 +1
203600031105     c                   eval      wrorn02d = wrorn02d +1
203700030912     c                   endif
203800030912     c                   if        orfcar = '03'
203900031105     c                   eval      wrorn03 = wrorn03 +1
204000031105     c                   eval      wrorn03d = wrorn03d +1
204100030912     c                   endif
204200030912     c                   if        orfcar = '04'
204300031105     c                   eval      wrorn04 = wrorn04 +1
204400031105     c                   eval      wrorn04d = wrorn04d +1
204500030912     c                   endif
204600030929 e6  c                   endif
204700030929     c                   Leave
204800030917 e5  c                   endif
204900030929     ?* ORM no in fase 500 o 390 ?
205000030929      * devo controllare se nel giorno è stata fatta una fase 500 o 390
205100030912     c     kOrf02        Readpe    Fnorf01l
205200030917 b5  c                   Dow       Not %Eof(Fnorf01l)
205300040421      * se trovo fase 200 esco
205400040618if  6c                   If        OrfFar = 200
205500040421     c                   Leave
205600040421e   6c                   EndIf
205700030929 b6  c                   If        OrfFar = 500 or OrfFar = 390
205800031105     c                   eval      wrorone = wrorone +1
205900031105     c                   eval      wroroned = wroroned +1
206000031105     c   11              eval      w_rorone = 'X'
206100030929     c                   eval      wokstp = *on
206200030929     c                   eval      wokrfile = *on
206300031106      * da eseguire
206400031106     c                   eval      wrorode = wrorode +1
206500031106      * sempre in anticipo xchè data ritiro > data elaborazione
206600031106     c                   eval      wrordan = wrordan +1
206700031106     c                   eval      wrornan = wrornan +1
206800030929      * memorizzo il tipo comunicazione ORM
206900030929     ?* fissi ?
207000030929 b7  c                   if        ormtco = 'S'
207100031105     c                   eval      wrorifine = wrorifine +1
207200031105     c                   eval      wrorifid = wrorifid +1
207300030929 e7  c                   endif
207400030929     ?* da centralino ?
207500030929 b7  c                   if        ormtco = 'M'  or
207600030929     c                             ormtco = 'P'
207700031105     c                   eval      wroricene = wroricene +1
207800031105     c                   eval      wroriced = wroriced +1
207900030929 e7  c                   endif
208000030929     ?* da internet ?
208100030929 b7  c                   if        ormtco = 'I'
208200031105     c                   eval      wroriinne = wroriinne +1
208300031105     c                   eval      wroriind = wroriind +1
208400030929 e7  c                   endif
208500030929     ?* da vas ?
208600030929 b7  c                   if        ormtco = 'F'
208700031105     c                   eval      wrorivane = wrorivane +1
208800031105     c                   eval      wrorivad = wrorivad +1
208900030929 e7  c                   endif
209000030929     ?* da call center ?
209100030929 b7  c                   if        ormtco = 'C'
209200031105     c                   eval      wroriccne = wroriccne +1
209300031105     c                   eval      wroriccd = wroriccd +1
209400030929 e7  c                   endif
209500030912      * se trovo la fase 500 memorizzo la causale
209600030929 b7  c                   If        OrfFar = 500
209700031105     c                   Eval      %Subst(cauone:2:2) = OrfCar
209800031105     c                   Move      ')'           cauone
209900031105     c                   Movel     '('           cauone
210000030912     c                   If        OrfCar = '01'
210100031105     c                   Eval      wrorn01 = wrorn01 +1
210200031105     c                   Eval      wrorn01d = wrorn01d +1
210300030912     c                   Endif
210400030912     c                   If        OrfCar = '02'
210500031105     c                   Eval      wrorn02 = wrorn02 +1
210600031105     c                   Eval      wrorn02d = wrorn02d +1
210700030912     c                   Endif
210800030912     c                   If        OrfCar = '03'
210900031105     c                   Eval      wrorn03 = wrorn03 +1
211000031105     c                   Eval      wrorn03d = wrorn03d +1
211100030912     c                   Endif
211200030912     c                   If        OrfCar = '04'
211300031105     c                   Eval      wrorn04 = wrorn04 +1
211400031105     c                   Eval      wrorn04d = wrorn04d +1
211500030912     c                   Endif
211600030929 e7  c                   Endif
211700030912     c                   Leave
211800030917 e6  c                   Endif
211900030912     c     kOrf02        Readpe    Fnorf01l
212000030917 e5  c                   EndDo
212100030917     c                   Leave
212200030916 e4a c                   endif
212300010613 e4  c                   endif
212400010608
212500010613 e3  c                   endif
212600010612
212700010613 e2  c                   endif
212800010608
212900030912      * _____________________________
213000010608      * data fase < data elaborazione
213100030912      * -----------------------------
213200010620 b2  c                   if        orfdae < dataela
213300010705
213400010614      * se ORM eseguito (fase 900) esco dal giro
213500010614     c                   if        orffar >= 900
213600010614     c                   leave
213700010614     c                   endif
213800010612      * se data ritiro maggiore data elaborazione esco dal giro
213900010612     c                   if        ormdar > dataela
214000010612     c                   leave
214100010612     c                   endif
214200010619      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
214300010619      * e i non bollettati
214400010619 b3  c                   if        wokfestar = *off
214500010614     ?* ORM non bollettati
214600010615     c                   if        orffar = 600 and
214700010615     c                             orfdae >= wdtda01
214800010614     c                   eval      wrornbl = wrornbl +1
214900010621     c   11              eval      w_rornbl = 'X'
215000010614     c                   eval      wokstp = *on
215100010614     c                   eval      wokrfile = *on
215200010614     c                   leave
215300010614     c                   endif
215400010615     ?* ORM già eseguiti che non devo elaborare
215500010615     c                   if        orffar = 600
215600010615     c                   leave
215700010615     c                   endif
215800060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
215900010705      * in questo caso devo tornare a leggere la precedente
216000060926     c     orffar        Lookup    skfar                                  30
216100060926 b4  c**!!!              if        orffar = 300
216200060926     c                   if        *in30
216300010705     c                   goto      passa
216400010705 e4  c                   endif
216500010608     ?* ORM non eseguiti
216600010619 b4  c                   if        orffar < 600
216700010608     c                   eval      wrorone = wrorone +1
216800010621     c   11              eval      w_rorone = 'X'
216900030917     c                   eval      wokstp = *on
217000030917     c                   eval      wokrfile = *on
217100010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
217200010619 b5  c                   select
217300010608     c                   when      ormdar = dataela
217400010608     c                   eval      wrornit = wrornit +1
217500010608     c                   when      ormdar >= wdtda01
217600010608     c                   eval      wrornr1 = wrornr1 +1
217700010608     c                   when      ormdar >= wdtda02
217800010608     c                   eval      wrornr2 = wrornr2 +1
217900010619 x5  c                   other
218000010608     c                   eval      wrornro = wrornro +1
218100010619 e5  c                   endsl
218200030917      * memorizzo il tipo comunicazione ORM
218300030917     ?* fissi ?
218400030917 b5  c                   if        ormtco = 'S'
218500030917     c                   eval      wrorifine = wrorifine +1
218600030917 e5  c                   endif
218700030917     ?* da centralino ?
218800030917 b5  c                   if        ormtco = 'M'  or
218900030917     c                             ormtco = 'P'
219000030917     c                   eval      wroricene = wroricene +1
219100030917 e5  c                   endif
219200030917     ?* da internet ?
219300030917 b5  c                   if        ormtco = 'I'
219400030917     c                   eval      wroriinne = wroriinne +1
219500030917 e5  c                   endif
219600030917     ?* da vas ?
219700030917 b5  c                   if        ormtco = 'F'
219800030917     c                   eval      wrorivane = wrorivane +1
219900030917 e5  c                   endif
220000030917     ?* da call center ?
220100030917 b5  c                   if        ormtco = 'C'
220200030917     c                   eval      wroriccne = wroriccne +1
220300030917 e5  c                   endif
220400030917     ?* ORM non eseguiti con causale ?
220500030917 b5  c                   if        orffar = 500
220600030917     c                   eval      cauone = orfcar
220700030917     c                   if        orfcar = '01'
220800030917     c                   eval      wrorn01 = wrorn01 +1
220900030917     c                   endif
221000030917     c                   if        orfcar = '02'
221100030917     c                   eval      wrorn02 = wrorn02 +1
221200030917     c                   endif
221300030917     c                   if        orfcar = '03'
221400030917     c                   eval      wrorn03 = wrorn03 +1
221500030917     c                   endif
221600030917     c                   if        orfcar = '04'
221700030917     c                   eval      wrorn04 = wrorn04 +1
221800030917     c                   endif
221900030917     c                   leave
222000030917 e5  c                   endif
222100030917     ?* ORM non eseguiti no in fase 500 ?
222200010614     ?* ORM non assegnati
222300010619 b5  c                   if        orffar < 400
222400010614     c                   eval      wrornas = wrornas +1
222500010621     c   11              eval      w_rornas = 'X'
222600010619 e5  c                   endif
222700030917      * devo controllare se nel giorno è stata fatta una fase 500
222800030917     c     kOrf02        Readpe    Fnorf01l
222900030917 b5  c                   Dow       Not %Eof(Fnorf01l)
223000040421      * se trovo fase 200 esco
223100040618if  6c                   If        OrfFar = 200
223200040421     c                   Leave
223300040421e   6c                   EndIf
223400030917 b6  c                   If        OrfFar = 500
223500030917      * se trovo la fase 500 memorizzo la causale
223600030917     c                   Eval      %Subst(cauone:2:2) = OrfCar
223700031118     c                   Move      ')'           cauone
223800031118     c                   Movel     '('           cauone
223900030917     c                   If        OrfCar = '01'
224000030917     c                   Eval      wrorn01 = wrorn01 +1
224100030917     c                   Endif
224200030917     c                   If        OrfCar = '02'
224300030917     c                   Eval      wrorn02 = wrorn02 +1
224400030917     c                   Endif
224500030917     c                   If        OrfCar = '03'
224600030917     c                   Eval      wrorn03 = wrorn03 +1
224700030917     c                   Endif
224800030917     c                   If        OrfCar = '04'
224900030917     c                   Eval      wrorn04 = wrorn04 +1
225000030917     c                   Endif
225100030917     c                   Leave
225200030917 e6  c                   Endif
225300030917     c     kOrf02        Readpe    Fnorf01l
225400030917 e5  c                   EndDo
225500010608     c                   leave
225600010619 e4  c                   endif
225700010619
225800010619 e3  c                   endif
225900010608
226000010613 e2  c                   endif
226100010705
226200010705     c     passa         tag
226300010705
226400010608     c     korf          readpe    fnorf01l
226500010705
226600010613 e1  c                   enddo
226700010612
226800010612      * se ORM in fase 900 controllo se quadrato
226900010612     c                   if        wok900 = *on
227000010614      *                            =========
227100010614     c                   exsr      SR_QUADRA
227200010614      *                            =========
227300010612     c                   endif
227400010608
227500010608     c                   endsr
227600010614
227700010608      *--------------------------------------------------------------*
227800010608      * ORM NON QUADRATO
227900010608      *--------------------------------------------------------------*
228000010614     c     SR_QUADRA     begsr
228100010608
228200010608      * ORM non quadrati
228300010608      * mi devo posizionare sulle fasi con la data elaborazione
228400010608      * poi cercare se c'è una fase 900 controllare se esiste
228500010608      * una fase 600
228600010608
228700010608     c                   eval      wok600 = *off
228800010608
228900010608     c                   if        wok900 = *on
229000010608     c                   eval      kormdfo = dataela
229100010615     c                   clear                   kormofo
229200010615     c                   clear                   kormfao
229300010608     c     korf          setll     fnorf01l
229400010608     c                   do        *hival
229500010608     c     korf          reade     fnorf01l
229600010608     c                   if        %eof(fnorf01l)
229700010608     c                   leave
229800010608     c                   endif
229900010608     c                   if        orffar = 600
230000010608     c                   eval      wok600 = *on
230100010608     c                   leave
230200010608     c                   endif
230300010608     c                   enddo
230400010608     c                   if        wok600 = *off
230500010608     c                   eval      wrornqd = wrornqd +1
230600010621     c   11              eval      w_rornqd = 'X'
230700010608     c                   eval      wokstp = *on
230800010608     c                   endif
230900010608     c                   endif
231000010608
231100010608     c                   endsr
231200010614
231300010605      *--------------------------------------------------------------*
231400010605      * CONTROLLO E CARICAMENTO SK
231500010605      *--------------------------------------------------------------*
231600010605     c     SR_CONTRSK    begsr
231700010605
231800010606     c                   eval      woksk = *off
231900010606
232000010612     c                   do        18000         xf
232100030912     c                   if        kfile(xf) = dskfile
232200010605     c                   eval      woksk = *on
232300010605     c                   leave
232400010605     c                   endif
232500030912     c                   if        kfile1(xf) = *zeros
232600010605     c                   leave
232700010605     c                   endif
232800010605     c                   enddo
232900010605
233000010606     c                   if        woksk = *on
233100010612     c                   eval      dswfror = rfile(xf)
233200010606     c                   endif
233300010605
233400010605     c                   if        woksk = *off
233500010611     c                   clear                   dswfror
233600010612     c                   eval      kfile(xf) = dskfile
233700030916     c                   eval      kfile1(xf) = dskfile1
233800030916     c                   Eval      f_rortrc = dstrc
233900010605     c                   eval      f_rorpoe = dspoel
234000010605     c                   eval      f_rordae = dsdata
234100010605     c                   eval      kazorg = dspoel
234200010605     c     kazorg        chain     azorg01l
234300010605     c                   if        %found(azorg01l)
234400010605     c                   eval      f_rordis = orgfl3
234500010605     c                   eval      f_rorare = orgcar
234600010605     c                   endif
234700010605     c                   endif
234800010605
234900010605     c                   endsr
235000010614
235100010608      *--------------------------------------------------------------*
235200010608      * CARICO LA SK DEL FILE
235300010608      *--------------------------------------------------------------*
235400010614     c     SR_CARICA     begsr
235500010608
235600010608     c                   eval      f_rorode = (f_rorode + wrorode)
235700010608     c                   eval      f_roroes = (f_roroes + wroroes)
235800010608     c                   eval      f_rorone = (f_rorone + wrorone)
235900010608     c                   eval      f_rorn01 = (f_rorn01 + wrorn01)
236000010608     c                   eval      f_rorn02 = (f_rorn02 + wrorn02)
236100010608     c                   eval      f_rorn03 = (f_rorn03 + wrorn03)
236200010608     c                   eval      f_rorn04 = (f_rorn04 + wrorn04)
236300010608     c                   eval      f_rorn05 = (f_rorn05 + wrorn05)
236400010608     c                   eval      f_rorn06 = (f_rorn06 + wrorn06)
236500010608     c                   eval      f_rorn07 = (f_rorn07 + wrorn07)
236600010608     c                   eval      f_rorn08 = (f_rorn08 + wrorn08)
236700010608     c                   eval      f_rorn09 = (f_rorn09 + wrorn09)
236800010608     c                   eval      f_rorn10 = (f_rorn10 + wrorn10)
236900010608     c                   eval      f_roroea = (f_roroea + wroroea)
237000010608     c                   eval      f_rora01 = (f_rora01 + wrora01)
237100010608     c                   eval      f_rora02 = (f_rora02 + wrora02)
237200010608     c                   eval      f_rora03 = (f_rora03 + wrora03)
237300010608     c                   eval      f_rora04 = (f_rora04 + wrora04)
237400010608     c                   eval      f_rora05 = (f_rora05 + wrora05)
237500010608     c                   eval      f_rora06 = (f_rora06 + wrora06)
237600010608     c                   eval      f_rora07 = (f_rora07 + wrora07)
237700010608     c                   eval      f_rora08 = (f_rora08 + wrora08)
237800010608     c                   eval      f_rora09 = (f_rora09 + wrora09)
237900010608     c                   eval      f_rora10 = (f_rora10 + wrora10)
238000010608     c                   eval      f_rora11 = (f_rora11 + wrora11)
238100010608     c                   eval      f_rora12 = (f_rora12 + wrora12)
238200010608     c                   eval      f_rora13 = (f_rora13 + wrora13)
238300010608     c                   eval      f_rora14 = (f_rora14 + wrora14)
238400010608     c                   eval      f_rora15 = (f_rora15 + wrora15)
238500010620     c                   eval      f_rordan = (f_rordan + wrordan)
238600010608     c                   eval      f_rordit = (f_rordit + wrordit)
238700010608     c                   eval      f_rordr1 = (f_rordr1 + wrordr1)
238800010608     c                   eval      f_rordr2 = (f_rordr2 + wrordr2)
238900010608     c                   eval      f_rordro = (f_rordro + wrordro)
239000010614     c                   eval      f_rorean = (f_rorean + wrorean)
239100010608     c                   eval      f_roreit = (f_roreit + wroreit)
239200010608     c                   eval      f_rorer1 = (f_rorer1 + wrorer1)
239300010608     c                   eval      f_rorer2 = (f_rorer2 + wrorer2)
239400010608     c                   eval      f_rorero = (f_rorero + wrorero)
239500010620     c                   eval      f_rornan = (f_rornan + wrornan)
239600010608     c                   eval      f_rornit = (f_rornit + wrornit)
239700010608     c                   eval      f_rornr1 = (f_rornr1 + wrornr1)
239800010608     c                   eval      f_rornr2 = (f_rornr2 + wrornr2)
239900010608     c                   eval      f_rornro = (f_rornro + wrornro)
240000010620     c                   eval      f_roraan = (f_roraan + wroraan)
240100010608     c                   eval      f_rorait = (f_rorait + wrorait)
240200010608     c                   eval      f_rorar1 = (f_rorar1 + wrorar1)
240300010608     c                   eval      f_rorar2 = (f_rorar2 + wrorar2)
240400010608     c                   eval      f_roraro = (f_roraro + wroraro)
240500010608     c                   eval      f_rornqd = (f_rornqd + wrornqd)
240600010608     c                   eval      f_rornbl = (f_rornbl + wrornbl)
240700010614     c                   eval      f_rormdr = (f_rormdr + wrormdr)
240800010614     c                   eval      f_rornas = (f_rornas + wrornas)
240900030912     c                   eval      f_roroned = (f_roroned + wroroned)
241000030912     c                   eval      f_rorn01d = (f_rorn01d + wrorn01d)
241100030912     c                   eval      f_rorn02d = (f_rorn02d + wrorn02d)
241200030912     c                   eval      f_rorn03d = (f_rorn03d + wrorn03d)
241300030912     c                   eval      f_rorn04d = (f_rorn04d + wrorn04d)
241400030912     c                   eval      f_rorn05d = (f_rorn05d + wrorn05d)
241500030912     c                   eval      f_rorn06d = (f_rorn06d + wrorn06d)
241600030912     c                   eval      f_rorn07d = (f_rorn07d + wrorn07d)
241700030912     c                   eval      f_rorn08d = (f_rorn08d + wrorn08d)
241800030912     c                   eval      f_rorn09d = (f_rorn09d + wrorn09d)
241900030912     c                   eval      f_rorn10d = (f_rorn10d + wrorn10d)
242000030917     c                   eval      f_rorifian = (f_rorifian + wrorifian)
242100030917     c                   eval      f_rorifine = (f_rorifine + wrorifine)
242200030917     c                   eval      f_rorifid  = (f_rorifid  + wrorifid)
242300030917     c                   eval      f_roricean = (f_roricean + wroricean)
242400030917     c                   eval      f_roricene = (f_roricene + wroricene)
242500030917     c                   eval      f_roriced  = (f_roriced  + wroriced)
242600030917     c                   eval      f_roriinan = (f_roriinan + wroriinan)
242700030917     c                   eval      f_roriinne = (f_roriinne + wroriinne)
242800030917     c                   eval      f_roriind  = (f_roriind  + wroriind)
242900030917     c                   eval      f_rorivaan = (f_rorivaan + wrorivaan)
243000030917     c                   eval      f_rorivane = (f_rorivane + wrorivane)
243100030917     c                   eval      f_rorivad  = (f_rorivad  + wrorivad)
243200030917     c                   eval      f_roriccan = (f_roriccan + wroriccan)
243300030917     c                   eval      f_roriccne = (f_roriccne + wroriccne)
243400030917     c                   eval      f_roriccd  = (f_roriccd  + wroriccd)
243500010612     c                   eval      rfile(xf) = dswfror
243600010608
243700010608     c                   endsr
243800010614
243900010604      *--------------------------------------------------------------*
244000010604      * ROUTINE INIZIALE
244100010604      *--------------------------------------------------------------*
244200010604     c     *inzsr        begsr
244300010604      *
244400010604     c     *entry        plist
244500010604     c                   parm                    kpjba
244600010621
244700010621     c                   movel     kpjbu         fior40ds
244800010621
244900030912      * controllo il sistema informativo dove sono per reperire
245000030912      * la libreria
245100030912      *   ---> GAITRA201/GAITRAGRU
245200030912     c                   If        %subst(knsif:7:3) = '201'  and
245300030912     c                             %subst(knsif:1:1) = 'G'
245400030912     c                   Eval      wlib = 'GAITRAGRU '
245500030912     c                   EndIf
245600030916      *   ---> FILTRA201/FILTRAGRU
245700030912     c                   If        %subst(knsif:7:3) = '201'  and
245800030912     c                             %subst(knsif:1:1) = 'F'
245900030912     c                   Eval      wlib = 'FILTRAGRU '
246000030912     c                   EndIf
246100030916      *   ---> GAITRAPSM/GAITRAGRPS
246200030916     c                   If        %subst(knsif:7:3) = 'PSM'  and
246300030916     c                             %subst(knsif:1:1) = 'G'
246400030916     c                   Eval      wlib = 'GAITRAGRPS'
246500030916     c                   EndIf
246600030912      *   ---> GAITRAPSP/GAITRAGRPS
246700030912     c                   If        %subst(knsif:7:3) = 'PSP'  and
246800030912     c                             %subst(knsif:1:1) = 'G'
246900040421     c                   Eval      wlib = 'GAITRAGRPS'
247000040421     c**                 Eval      wlib = 'GAITRAFIL '
247100030912     c                   EndIf
247200030916      *   ---> GAITRAPFM/GAITRAGRPF
247300030916     c                   If        %subst(knsif:7:3) = 'PFM'  and
247400030916     c                             %subst(knsif:1:1) = 'G'
247500030916     c                   Eval      wlib = 'GAITRAGRPF'
247600030916     c                   EndIf
247700030912      *   ---> GAITRAPFP/GAITRAGRPF
247800030912     c                   If        %subst(knsif:7:3) = 'PFP'  and
247900030912     c                             %subst(knsif:1:1) = 'G'
248000040421     c                   Eval      wlib = 'GAITRAGRPF'
248100040421     c**                 Eval      wlib = 'GAITRAFIL '
248200030912     c                   EndIf
248300030912      *   ---> FILTRAPFP/FILTRAGRPF
248400030912     c                   If        %subst(knsif:7:3) = 'PFP'  and
248500030912     c                             %subst(knsif:1:1) = 'F'
248600040421     c                   Eval      wlib = 'FILTRAGRPF'
248700040421     c**                 Eval      wlib = 'GAITRAFIL '
248800030912     c                   EndIf
248900030912
249000010621      * duplico il file di comodo in qtemp
249100010621     c                   clear                   comman
249200010621     c                   movel(p)  cmd(1)        comman
249300030912     c                   Eval      %Subst(comman:32:10) = wlib
249400010621     c                   call      'QCMDEXC'
249500010621     c                   parm                    comman
249600010621     c                   parm                    lenght
249700010621      * apro il file di comodo per la stampa
249800010621     c                   open      wfror11l
249900010621
250000010604     c                   z-add     1             codut
250100010604     c                   call      'X§PARUT'
250200010604     c                   parm                    ut§dse0f
250300010614     c                   movel     ragut         rsut
250400010604     c                   movel     rec80         cncr80
250500010604
250600010604      * reperisco data e ora
250700010604     c                   time                    w0140
250800010604      * udate in GGMMAAAA
250900010604     c                   move      w0140         wdate
251000010604     c                   movel     w0140         wora
251100010608
251200010608      * pulisco le sk
251300010608     c                   clear                   kfile
251400030916     c                   clear                   kfile1
251500010608     c                   clear                   rfile
251600010608     c                   clear                   skdate
251700010608     c                   clear                   skpoel
251800130118     c******             Clear                   skosr
251900130118     c                   Clear                   skcliosr
252000130118     c                   Clear                   skstaosr
252100010621
252200010621      * controllo se devo stampare per data o per p.o.
252300010621     c                   if        i40poel <> 999 and
252400010621     c                             i40dtda <> i40dtal and
252500010621     c                             i40tpst = 'S'
252600010621     c                   eval      *in13 = *on
252700010621     c                   else
252800010621     c                   eval      *in13 = *off
252900010621     c                   endif
253000010608
253100010608      * carico la sk delle date da elaborare
253200010703     c                   eval      xd = 1
253300010703     c                   eval      skdate(xd) = i40dtda
253400010703     c                   if        i40dtda <> i40dtal
253500010703     c                   eval      wdtgio = i40dtda
253600010703     c     *iso          movel     i40dtda       wdtda
253700010703     c                   dow       wdtgio < i40dtal
253800010703     c                   eval      xd = xd +1
253900010703     c                   adddur    1:*d          wdtda
254000010703     c                   move      wdtda         wdtgio
254100010703     c                   eval      skdate(xd) = wdtgio
254200010703     c                   enddo
254300010703     c                   endif
254400020807
254500020808     c                   z-add     1             xa
254600010608
254700020807      * Elaboro tutti i p.o.
254800020807 b1  c                   if        i40poel = 999
254900020807      * Ricerco tutti i terminal di partenza
255000020807     c                   Clear                   Trul26Ds
255100020807     c                   Eval      I26tpt = 'P'
255200020807     c                   Eval      I26tla = 'L'
255300020807     c                   Call      'TRUL26R'
255400020807     c                   Parm                    Trul26Ds
255500020807      * carico i p.o. da elaborare dell'AS in un sk
255600020807 b2  c                   Do        250           Xb
255700020807     c                   If        Skpot(Xb) = *Zeros
255800020807     c                   Leave
255900020807     c                   EndIf
256000020807      * Per la filiale se lancio tutti i p.o. devo cercare quelli che
256100020807      * hanno il terminal di partenza = simfel
256200020807 b3  c                   If        i40sede = 'F'
256300020807     c                   If        Skpot(Xb) = Simfel
256400020807     c                   Eval      Skpoel(Xa) = Skpo(Xb)
256500020808     c                   Add       1             Xa
256600010608     c                   endif
256700020807      * Per la Sede se lancio tutti i p.o. carico tutti
256800020807 b3  c                   Else
256900020807     c                   Eval      Skpoel(Xb) = Skpo(Xb)
257000020807 e3  c                   EndIf
257100010608
257200020807 e2  c                   EndDo
257300020807 e1  c                   EndIf
257400010605      * lanciato un solo po carico questo nella sk
257500010605     c                   if        i40poel <> 999
257600010612     c                   eval      skpoel(xa) = i40poel
257700010605     c                   endif
257800031002
257900031002      * carico la sk delle serie da non elaborare in statistica
258000031002     c                   Eval      xc = 1
258100031002     c                   Eval      TbeCod = 'OSR'
258200031002     c     TbeCod        Setll     Tntbe01l
258300031002Do  1c                   Do        *Hival
258400031002     c     TbeCod        Reade     Tntbe01l
258500031002     c                   If        %Eof(Tntbe01l)
258600031002     c                   Leave
258700031002     c                   EndIf
258800130122     c                   IF        TBEatb <> *blanks
258900130122     c                   iter
259000130122     c                   ENDIF
259100031002     c                   Movel     TbeUni        dOsr
259200130118      /free
259300130118           IF d§osrcli <> 0;
259400130118             xx = %lookup(d§OSRcli:skCLIosr);
259500130118             IF  xx = 0;
259600130118               xx = %lookup(*zeros:skCLIosr);
259700130118               IF  xx > 0;
259800130118                 skCLIosr(xx) = d§OSRcli;
259900130118                 skstaosr(xx) = d§OSRsta;
260000130118               ENDIF;
260100130118             ENDIF;
260200130118           ENDIF;
260300130118
260400130118           IF d§osrcl2 <> 0;
260500130118             xx = %lookup(d§OSRcl2:skCLIosr);
260600130118             IF  xx = 0;
260700130118               xx = %lookup(*zeros:skCLIosr);
260800130118               IF  xx > 0;
260900130118                 skCLIosr(xx) = d§OSRcl2;
261000130118                 skstaosr(xx) = d§OSRsta;
261100130118               ENDIF;
261200130118             ENDIF;
261300130118           ENDIF;
261400130118
261500130118           IF d§osrcl3 <> 0;
261600130118             xx = %lookup(d§OSRcl3:skCLIosr);
261700130118             IF  xx = 0;
261800130118               xx = %lookup(*zeros:skCLIosr);
261900130118               IF  xx > 0;
262000130118                 skCLIosr(xx) = d§OSRcl3;
262100130118                 skstaosr(xx) = d§OSRsta;
262200130118               ENDIF;
262300130118             ENDIF;
262400130118           ENDIF;
262500130118
262600130118           IF d§osrcl4 <> 0;
262700130118             xx = %lookup(d§OSRcl4:skCLIosr);
262800130118             IF  xx = 0;
262900130118               xx = %lookup(*zeros:skCLIosr);
263000130118               IF  xx > 0;
263100130118                 skCLIosr(xx) = d§OSRcl4;
263200130118                 skstaosr(xx) = d§OSRsta;
263300130118               ENDIF;
263400130118             ENDIF;
263500130118           ENDIF;
263600130118
263700130118           IF d§osrcl5 <> 0;
263800130118             xx = %lookup(d§OSRcl5:skCLIosr);
263900130118             IF  xx = 0;
264000130118               xx = %lookup(*zeros:skCLIosr);
264100130118               IF  xx > 0;
264200130118                 skCLIosr(xx) = d§OSRcl5;
264300130118                 skstaosr(xx) = d§OSRsta;
264400130118               ENDIF;
264500130118             ENDIF;
264600130118           ENDIF;
264700130118
264800130118           IF d§osrcl6 <> 0;
264900130118             xx = %lookup(d§OSRcl6:skCLIosr);
265000130118             IF  xx = 0;
265100130118               xx = %lookup(*zeros:skCLIosr);
265200130118               IF  xx > 0;
265300130118                 skCLIosr(xx) = d§OSRcl6;
265400130118                 skstaosr(xx) = d§OSRsta;
265500130118               ENDIF;
265600130118             ENDIF;
265700130118           ENDIF;
265800130118
265900130118           IF d§osrcl7 <> 0;
266000130118             xx = %lookup(d§OSRcl7:skCLIosr);
266100130118             IF  xx = 0;
266200130118               xx = %lookup(*zeros:skCLIosr);
266300130118               IF  xx > 0;
266400130118                 skCLIosr(xx) = d§OSRcl7;
266500130118                 skstaosr(xx) = d§OSRsta;
266600130118               ENDIF;
266700130118             ENDIF;
266800130118           ENDIF;
266900130118
267000130118           IF d§osrcl8 <> 0;
267100130118             xx = %lookup(d§OSRcl8:skCLIosr);
267200130118             IF  xx = 0;
267300130118               xx = %lookup(*zeros:skCLIosr);
267400130118               IF  xx > 0;
267500130118                 skCLIosr(xx) = d§OSRcl8;
267600130118                 skstaosr(xx) = d§OSRsta;
267700130118               ENDIF;
267800130118             ENDIF;
267900130118           ENDIF;
268000130118
268100130118           IF d§osrcl9 <> 0;
268200130118             xx = %lookup(d§OSRcl9:skCLIosr);
268300130118             IF  xx = 0;
268400130118               xx = %lookup(*zeros:skCLIosr);
268500130118               IF  xx > 0;
268600130118                 skCLIosr(xx) = d§OSRcl9;
268700130118                 skstaosr(xx) = d§OSRsta;
268800130118               ENDIF;
268900130118             ENDIF;
269000130118           ENDIF;
269100130118
269200130118           IF d§osrcl10 <> 0;
269300130118             xx = %lookup(d§OSRcl10:skCLIosr);
269400130118             IF  xx = 0;
269500130118               xx = %lookup(*zeros:skCLIosr);
269600130118               IF  xx > 0;
269700130118                 skCLIosr(xx) = d§OSRcl10;
269800130118                 skstaosr(xx) = d§OSRsta;
269900130118               ENDIF;
270000130118             ENDIF;
270100130118           ENDIF;
270200130118
270300130118           IF d§osrcl11 <> 0;
270400130118             xx = %lookup(d§OSRcl11:skCLIosr);
270500130118             IF  xx = 0;
270600130118               xx = %lookup(*zeros:skCLIosr);
270700130118               IF  xx > 0;
270800130118                 skCLIosr(xx) = d§OSRcl11;
270900130118                 skstaosr(xx) = d§OSRsta;
271000130118               ENDIF;
271100130118             ENDIF;
271200130118           ENDIF;
271300130118
271400130118           IF d§osrcl12 <> 0;
271500130118             xx = %lookup(d§OSRcl12:skCLIosr);
271600130118             IF  xx = 0;
271700130118               xx = %lookup(*zeros:skCLIosr);
271800130118               IF  xx > 0;
271900130118                 skCLIosr(xx) = d§OSRcl12;
272000130118                 skstaosr(xx) = d§OSRsta;
272100130118               ENDIF;
272200130118             ENDIF;
272300130118           ENDIF;
272400130118
272500130118           IF d§osrcl13 <> 0;
272600130118             xx = %lookup(d§OSRcl13:skCLIosr);
272700130118             IF  xx = 0;
272800130118               xx = %lookup(*zeros:skCLIosr);
272900130118               IF  xx > 0;
273000130118                 skCLIosr(xx) = d§OSRcl13;
273100130118                 skstaosr(xx) = d§OSRsta;
273200130118               ENDIF;
273300130118             ENDIF;
273400130118           ENDIF;
273500130118
273600130118           IF d§osrcl14 <> 0;
273700130118             xx = %lookup(d§OSRcl14:skCLIosr);
273800130118             IF  xx = 0;
273900130118               xx = %lookup(*zeros:skCLIosr);
274000130118               IF  xx > 0;
274100130118                 skCLIosr(xx) = d§OSRcl14;
274200130118                 skstaosr(xx) = d§OSRsta;
274300130118               ENDIF;
274400130118             ENDIF;
274500130118           ENDIF;
274600130118
274700130118           IF d§osrcl15 <> 0;
274800130118             xx = %lookup(d§OSRcl15:skCLIosr);
274900130118             IF  xx = 0;
275000130118               xx = %lookup(*zeros:skCLIosr);
275100130118               IF  xx > 0;
275200130118                 skCLIosr(xx) = d§OSRcl15;
275300130118                 skstaosr(xx) = d§OSRsta;
275400130118               ENDIF;
275500130118             ENDIF;
275600130118           ENDIF;
275700130118
275800130118           IF d§osrcl16 <> 0;
275900130118             xx = %lookup(d§OSRcl16:skCLIosr);
276000130118             IF  xx = 0;
276100130118               xx = %lookup(*zeros:skCLIosr);
276200130118               IF  xx > 0;
276300130118                 skCLIosr(xx) = d§OSRcl16;
276400130118                 skstaosr(xx) = d§OSRsta;
276500130118               ENDIF;
276600130118             ENDIF;
276700130118           ENDIF;
276800130118      /end-free
276900130118
277000031002    1c                   EndDo
277100010611
277200010611      * carico nei campi di stampa le descrizioni delle causali
277300010611     c                   eval      tbecod = 'CMR'
277400010611      * causali di non fatto
277500010611     c                   eval      tbeke1 = '01'
277600010611     c     ktntbe        chain     tntbe01l
277700010611     c                   if        %found(tntbe01l)
277800010619     c                   clear                   dcmr
277900010619     c                   movel     tbeuni        dcmr
278000010619     c                   if        d§cmrdes1 <> *blanks
278100010619     c                   movel     d§cmrdes1     descau01n
278200010619     c                   else
278300010619     c                   movel     d§cmrdes      descau01n
278400010619     c                   endif
278500010611     c                   endif
278600010619
278700010611     c                   eval      tbeke1 = '02'
278800010611     c     ktntbe        chain     tntbe01l
278900010611     c                   if        %found(tntbe01l)
279000010619     c                   clear                   dcmr
279100010619     c                   movel     tbeuni        dcmr
279200010619     c                   if        d§cmrdes1 <> *blanks
279300010619     c                   movel     d§cmrdes1     descau02n
279400010619     c                   else
279500010619     c                   movel     d§cmrdes      descau02n
279600010619     c                   endif
279700010611     c                   endif
279800010619
279900010611     c                   eval      tbeke1 = '03'
280000010611     c     ktntbe        chain     tntbe01l
280100010611     c                   if        %found(tntbe01l)
280200010619     c                   clear                   dcmr
280300010619     c                   movel     tbeuni        dcmr
280400010619     c                   if        d§cmrdes1 <> *blanks
280500010619     c                   movel     d§cmrdes1     descau03n
280600010619     c                   else
280700010619     c                   movel     d§cmrdes      descau03n
280800010619     c                   endif
280900010611     c                   endif
281000020527
281100020527     c                   eval      tbeke1 = '04'
281200020527     c     ktntbe        chain     tntbe01l
281300020527     c                   if        %found(tntbe01l)
281400020527     c                   clear                   dcmr
281500020527     c                   movel     tbeuni        dcmr
281600020527     c                   if        d§cmrdes1 <> *blanks
281700020527     c                   movel     d§cmrdes1     descau04n
281800020527     c                   else
281900020527     c                   movel     d§cmrdes      descau04n
282000020527     c                   endif
282100020527     c                   endif
282200010619
282300010611      * causali di esito anomalo
282400010611     c                   eval      tbeke1 = '80'
282500010611     c     ktntbe        chain     tntbe01l
282600010611     c                   if        %found(tntbe01l)
282700010619     c                   clear                   dcmr
282800010619     c                   movel     tbeuni        dcmr
282900010619     c                   if        d§cmrdes1 <> *blanks
283000010619     c                   movel     d§cmrdes1     descau01a
283100010619     c                   else
283200010619     c                   movel     d§cmrdes      descau01a
283300010619     c                   endif
283400010611     c                   endif
283500010619
283600010611     c                   eval      tbeke1 = '81'
283700010611     c     ktntbe        chain     tntbe01l
283800010611     c                   if        %found(tntbe01l)
283900010619     c                   clear                   dcmr
284000010619     c                   movel     tbeuni        dcmr
284100010619     c                   if        d§cmrdes1 <> *blanks
284200010619     c                   movel     d§cmrdes1     descau02a
284300010619     c                   else
284400010619     c                   movel     d§cmrdes      descau02a
284500010619     c                   endif
284600010611     c                   endif
284700010619
284800010611     c                   eval      tbeke1 = '90'
284900010611     c     ktntbe        chain     tntbe01l
285000010611     c                   if        %found(tntbe01l)
285100010619     c                   clear                   dcmr
285200010619     c                   movel     tbeuni        dcmr
285300010619     c                   if        d§cmrdes1 <> *blanks
285400010619     c                   movel     d§cmrdes1     descau03a
285500010619     c                   else
285600010619     c                   movel     d§cmrdes      descau03a
285700010619     c                   endif
285800010611     c                   endif
285900010619
286000010611     c                   eval      tbeke1 = '91'
286100010611     c     ktntbe        chain     tntbe01l
286200010611     c                   if        %found(tntbe01l)
286300010619     c                   clear                   dcmr
286400010619     c                   movel     tbeuni        dcmr
286500010619     c                   if        d§cmrdes1 <> *blanks
286600010619     c                   movel     d§cmrdes1     descau04a
286700010619     c                   else
286800010619     c                   movel     d§cmrdes      descau04a
286900010619     c                   endif
287000010611     c                   endif
287100010619
287200010611     c                   eval      tbeke1 = '92'
287300010611     c     ktntbe        chain     tntbe01l
287400010611     c                   if        %found(tntbe01l)
287500010619     c                   clear                   dcmr
287600010619     c                   movel     tbeuni        dcmr
287700010619     c                   if        d§cmrdes1 <> *blanks
287800010619     c                   movel     d§cmrdes1     descau05a
287900010619     c                   else
288000010619     c                   movel     d§cmrdes      descau05a
288100010619     c                   endif
288200010611     c                   endif
288300010619
288400010611     c                   eval      tbeke1 = '93'
288500010611     c     ktntbe        chain     tntbe01l
288600010611     c                   if        %found(tntbe01l)
288700010619     c                   clear                   dcmr
288800010619     c                   movel     tbeuni        dcmr
288900010619     c                   if        d§cmrdes1 <> *blanks
289000010619     c                   movel     d§cmrdes1     descau06a
289100010619     c                   else
289200010619     c                   movel     d§cmrdes      descau06a
289300010619     c                   endif
289400010611     c                   endif
289500010619
289600010611     c                   eval      tbeke1 = '94'
289700010611     c     ktntbe        chain     tntbe01l
289800010611     c                   if        %found(tntbe01l)
289900010619     c                   clear                   dcmr
290000010619     c                   movel     tbeuni        dcmr
290100010619     c                   if        d§cmrdes1 <> *blanks
290200010619     c                   movel     d§cmrdes1     descau07a
290300010619     c                   else
290400010619     c                   movel     d§cmrdes      descau07a
290500010619     c                   endif
290600010611     c                   endif
290700020527
290800020527     c                   eval      tbeke1 = '95'
290900020527     c     ktntbe        chain     tntbe01l
291000020527     c                   if        %found(tntbe01l)
291100020527     c                   clear                   dcmr
291200020527     c                   movel     tbeuni        dcmr
291300020527     c                   if        d§cmrdes1 <> *blanks
291400020527     c                   movel     d§cmrdes1     descau08a
291500020527     c                   else
291600020527     c                   movel     d§cmrdes      descau08a
291700020527     c                   endif
291800020527     c                   endif
291900020625
292000020625     c                   eval      tbeke1 = '82'
292100020625     c     ktntbe        chain     tntbe01l
292200020625     c                   if        %found(tntbe01l)
292300020625     c                   clear                   dcmr
292400020625     c                   movel     tbeuni        dcmr
292500020625     c                   if        d§cmrdes1 <> *blanks
292600020625     c                   movel     d§cmrdes1     descau09a
292700020625     c                   else
292800020625     c                   movel     d§cmrdes      descau09a
292900020625     c                   endif
293000020625     c                   endif
293100040701
293200040701     c                   eval      tbeke1 = '96'
293300040701     c     ktntbe        chain     tntbe01l
293400040701     c                   if        %found(tntbe01l)
293500040701     c                   clear                   dcmr
293600040701     c                   movel     tbeuni        dcmr
293700040701     c                   if        d§cmrdes1 <> *blanks
293800040701     c                   movel     d§cmrdes1     descau10a
293900040701     c                   else
294000040701     c                   movel     d§cmrdes      descau10a
294100040701     c                   endif
294200040701     c                   endif
294300040701
294400040701     c                   eval      tbeke1 = '97'
294500040701     c     ktntbe        chain     tntbe01l
294600040701     c                   if        %found(tntbe01l)
294700040701     c                   clear                   dcmr
294800040701     c                   movel     tbeuni        dcmr
294900040701     c                   if        d§cmrdes1 <> *blanks
295000040701     c                   movel     d§cmrdes1     descau11a
295100040701     c                   else
295200040701     c                   movel     d§cmrdes      descau11a
295300040701     c                   endif
295400040701     c                   endif
295500100219
295600110718     c**!!!              eval      tbeke1 = '98'
295700110718     c**!!!ktntbe        chain     tntbe01l
295800110718     c**!!!              if        %found(tntbe01l)
295900110718     c**!!!              clear                   dcmr
296000110718     c**!!!              movel     tbeuni        dcmr
296100110718     c**!!!              if        d§cmrdes1 <> *blanks
296200110718     c**!!!              movel     d§cmrdes1     descau12a
296300110718     c**!!!              else
296400110718     c**!!!              movel     d§cmrdes      descau12a
296500110718     c**!!!              endif
296600110718     c**!!!              endif
296700060926
296800060926      * carico la sk delle fasi da non elaborare in statistica
296900060926     c                   eval      xc = 1
297000060926     c                   eval      tbecod = 'FAR'
297100060926     c     tbecod        setll     tntbe01l
297200060926Do  1c                   do        *hival
297300060926     c     tbecod        reade     tntbe01l
297400060926     c                   if        %eof(tntbe01l)
297500060926     c                   leave
297600060926     c                   endif
297700060926     c                   movel     tbeuni        dfar
297800060926     c                   if        d§farsta = *blanks
297900060926     c                   iter
298000060926     c                   endif
298100060926     c                   movel     tbeke1        skfar(xc)
298200060926     c                   add       1             xc
298300060926    1c                   enddo
298400010620
298500010620     c                   setoff                                       1011
298600010620
298700010705      * se richiesto apro il file della stampa di comodo EDP
298800010705     c                   if        i40edp = 'S'
298900010621     c                   open      fior41p2
299000010705     c                   eval      *in10 = *on
299100010620     c                   endif
299200010620
299300010705      * se richiesta la stampa di controllo apro il file di stampa
299400010620     c                   if        i40stpc = 'S'
299500010620     c                   open      fior41p1
299600010620     c                   eval      *in11 = *on
299700010620     c                   endif
299800010605
299900010621      * se richiesta la creazione del file lo apro
300000010621     c                   if        i40tpel = 'F'
300100010621     c                   open      wfror01l
300200010621     c                   endif
300300010621
300400010605      * chiavi
300500010605     c     korf          klist
300600010605     c                   kfld                    kormpoe
300700010605     c                   kfld                    kormnsr
300800010605     c                   kfld                    kormnor
300900010605     c                   kfld                    kormnrv
301000010604
301100010606     c     korf01        klist
301200010606     c                   kfld                    kormpoe
301300010606     c                   kfld                    kormnsr
301400010606     c                   kfld                    kormnor
301500010606     c                   kfld                    kormnrv
301600010606     c                   kfld                    kormdfo
301700010606     c                   kfld                    kormofo
301800010606     c                   kfld                    kormfao
301900010606
302000010606     c     korf02        klist
302100010606     c                   kfld                    kormpoe
302200010606     c                   kfld                    kormnsr
302300010606     c                   kfld                    kormnor
302400010606     c                   kfld                    kormnrv
302500010606     c                   kfld                    kormdfo
302600040420
302700040420     c     korf03        klist
302800040420     c                   kfld                    kormpoe
302900040420     c                   kfld                    kormnsr
303000040420     c                   kfld                    kormnor
303100040420     c                   kfld                    kormnrv
303200040420     c                   kfld                    kormfao
303300040420     c                   kfld                    kormdfo
303400040420
303500040420     c     korf04        klist
303600040420     c                   kfld                    kormpoe
303700040420     c                   kfld                    kormnsr
303800040420     c                   kfld                    kormnor
303900040420     c                   kfld                    kormnrv
304000040420     c                   kfld                    kormfao
304100010608
304200010618     c     korv          klist
304300010618     c                   kfld                    kormpoe
304400010618     c                   kfld                    kormnsr
304500010618     c                   kfld                    kormnor
304600010618     c                   kfld                    kormnrv
304700010618     c                   kfld                    korvdtv
304800010618
304900010608     c     kror          klist
305000010608     c                   kfld                    krorpoe
305100010608     c                   kfld                    krordta
305200030918     c                   kfld                    krortrc
305300030919
305400030919     c     kror1         klist
305500030919     c                   kfld                    krorpoe
305600030919     c                   kfld                    krordta
305700010606
305800010606     c     kazcln        klist
305900010606     c                   kfld                    ktfp
306000010606     c                   kfld                    ktfa
306100010606     c                   kfld                    kann
306200010606     c                   kfld                    kmes
306300010611
306400010620     c     ktab          klist
306500010620     c                   kfld                    codut
306600010620     c                   kfld                    tblcod
306700010620     c                   kfld                    tblkey
306800010620
306900010611     c     ktntbe        klist
307000010611     c                   kfld                    tbecod
307100010611     c                   kfld                    tbeke1
307200010606
307300010604     c                   endsr
307400010606      *--------------------------------------------------------------*
307500010621**   cmd - comando
307600030912CRTDUPOBJ OBJ(WFROR1*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
