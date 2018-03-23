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
003600140707      * 15 - Lettura rcd  'B' o 'C' ORM commissionati
003700140707      * 16 - ORM commissionati con Alert
003800140708      * 17 - Lettura rcd 'C' ORM commissionati con Alert
003900020808      * 30 - Lookup
004000010614      * 90 - overflow file stampa statistica
004100010621      * 91 - overflow file stampa di comodo EDP
004200010620      * 92 - overflow file stampa di controllo
004300010605
004400010605      *----------------------------------------------------*
004500010605
004600010605     d kazorg          s                   like(orgfil)
004700010604
004800010605     d kormpoe         s                   like(ormpoe)
004900010605     d kormnsr         s                   like(ormnsr)
005000010605     d kormnor         s                   like(ormnor)
005100010605     d kormnrv         s                   like(ormnrv)
005200010605     d kormdfo         s                   like(ormdfo)
005300010606     d kormofo         s                   like(ormofo)
005400010606     d kormfao         s                   like(ormfao)
005500010618     d korvdtv         s                   like(orvdtv)
005600010606
005700030912     d krortrc         s                   like(rortrc)
005800010608     d krorpoe         s                   like(rorpoe)
005900010608     d krordta         s                   like(rordae)
006000010608
006100010606     d ktfp            s                   like(clntfp)
006200010606     d ktfa            s                   like(clntfa)
006300010606     d kann            s                   like(clnann)
006400010606     d kmes            s                   like(clnmes)
006500010604
006600010608     d wgiorno         s              2  0
006700010604     d w0140           s             14  0
006800010604     d wdtgio          s              8  0
006900010604     d wora            s              6  0
007000010604     d wdate           s              8  0
007100010606     d dataela         s              8  0
007200010621
007300010621     d lenght          s             15  5 inz(80)                              lunghezza cmd CLP
007400010621     d comman          s             80                                         valore cmd CLP
007500030912     d wlib            s             10
007600010620
007700010606     d woksk           s              1    inz('0')
007800010606     d wpoe            s              1    inz('0')
007900010606     d wpor            s              1    inz('0')
008000010620     d wokstp          s              1    inz('0')
008100010608     d wok600          s              1    inz('0')
008200010607     d wok900          s              1    inz('0')
008300010608     d wokrode         s              1    inz('0')
008400010613     d wokroes         s              1    inz('0')
008500010608     d wokrfile        s              1    inz('0')
008600010612     d wokfestae       s              1    inz('0')
008700010612     d wokfestar       s              1    inz('0')
008800010614     d wokstpcau       s              1    inz('0')
008900010605
009000010606     d wrorode         s              5  0 inz(*zeros)
009100010620     d wrordan         s              5  0 inz(*zeros)
009200010606     d wrordit         s              5  0 inz(*zeros)
009300010606     d wrordr1         s              5  0 inz(*zeros)
009400010606     d wrordr2         s              5  0 inz(*zeros)
009500010606     d wrordro         s              5  0 inz(*zeros)
009600010606     d wroroes         s              5  0 inz(*zeros)
009700010614     d wrorean         s              5  0 inz(*zeros)
009800010606     d wroreit         s              5  0 inz(*zeros)
009900010606     d wrorer1         s              5  0 inz(*zeros)
010000010606     d wrorer2         s              5  0 inz(*zeros)
010100010606     d wrorero         s              5  0 inz(*zeros)
010200010606     d wrorone         s              5  0 inz(*zeros)
010300010620     d wrornan         s              5  0 inz(*zeros)
010400010606     d wrornit         s              5  0 inz(*zeros)
010500010606     d wrornr1         s              5  0 inz(*zeros)
010600010606     d wrornr2         s              5  0 inz(*zeros)
010700010606     d wrornro         s              5  0 inz(*zeros)
010800010606     d wroroea         s              5  0 inz(*zeros)
010900010620     d wroraan         s              5  0 inz(*zeros)
011000010606     d wrorait         s              5  0 inz(*zeros)
011100010606     d wrorar1         s              5  0 inz(*zeros)
011200010606     d wrorar2         s              5  0 inz(*zeros)
011300010606     d wroraro         s              5  0 inz(*zeros)
011400010606     d wrorn01         s              5  0 inz(*zeros)
011500010606     d wrorn02         s              5  0 inz(*zeros)
011600010606     d wrorn03         s              5  0 inz(*zeros)
011700010606     d wrorn04         s              5  0 inz(*zeros)
011800010606     d wrorn05         s              5  0 inz(*zeros)
011900010606     d wrorn06         s              5  0 inz(*zeros)
012000010606     d wrorn07         s              5  0 inz(*zeros)
012100010606     d wrorn08         s              5  0 inz(*zeros)
012200010606     d wrorn09         s              5  0 inz(*zeros)
012300010606     d wrorn10         s              5  0 inz(*zeros)
012400010606     d wrora01         s              5  0 inz(*zeros)
012500010606     d wrora02         s              5  0 inz(*zeros)
012600010606     d wrora03         s              5  0 inz(*zeros)
012700010606     d wrora04         s              5  0 inz(*zeros)
012800010606     d wrora05         s              5  0 inz(*zeros)
012900010606     d wrora06         s              5  0 inz(*zeros)
013000010606     d wrora07         s              5  0 inz(*zeros)
013100010606     d wrora08         s              5  0 inz(*zeros)
013200010606     d wrora09         s              5  0 inz(*zeros)
013300010606     d wrora10         s              5  0 inz(*zeros)
013400010606     d wrora11         s              5  0 inz(*zeros)
013500010606     d wrora12         s              5  0 inz(*zeros)
013600010606     d wrora13         s              5  0 inz(*zeros)
013700010606     d wrora14         s              5  0 inz(*zeros)
013800010606     d wrora15         s              5  0 inz(*zeros)
013900010606     d wrornqd         s              5  0 inz(*zeros)
014000010606     d wrornbl         s              5  0 inz(*zeros)
014100010614     d wrornas         s              5  0 inz(*zeros)
014200010614     d wrormdr         s              5  0 inz(*zeros)
014300030912     d wroroned        s              5  0 inz(*zeros)
014400030912     d wrorn01d        s              5  0 inz(*zeros)
014500030912     d wrorn02d        s              5  0 inz(*zeros)
014600030912     d wrorn03d        s              5  0 inz(*zeros)
014700030912     d wrorn04d        s              5  0 inz(*zeros)
014800030912     d wrorn05d        s              5  0 inz(*zeros)
014900030912     d wrorn06d        s              5  0 inz(*zeros)
015000030912     d wrorn07d        s              5  0 inz(*zeros)
015100030912     d wrorn08d        s              5  0 inz(*zeros)
015200030912     d wrorn09d        s              5  0 inz(*zeros)
015300030912     d wrorn10d        s              5  0 inz(*zeros)
015400030917     d wrorifian       s              5  0 inz(*zeros)
015500030917     d wrorifine       s              5  0 inz(*zeros)
015600030917     d wrorifid        s              5  0 inz(*zeros)
015700030917     d wroricean       s              5  0 inz(*zeros)
015800030917     d wroricene       s              5  0 inz(*zeros)
015900030917     d wroriced        s              5  0 inz(*zeros)
016000030917     d wroriinan       s              5  0 inz(*zeros)
016100030917     d wroriinne       s              5  0 inz(*zeros)
016200030917     d wroriind        s              5  0 inz(*zeros)
016300030917     d wrorivaan       s              5  0 inz(*zeros)
016400030917     d wrorivane       s              5  0 inz(*zeros)
016500030917     d wrorivad        s              5  0 inz(*zeros)
016600030917     d wroriccan       s              5  0 inz(*zeros)
016700030917     d wroriccne       s              5  0 inz(*zeros)
016800030917     d wroriccd        s              5  0 inz(*zeros)
016900010605
017000010611     d dspoele         s                   like(f_rorpoe)
017100010611     d dspoelr         s                   like(f_rorpoe)
017200010611
017300010620     d wdatafes        s              8  0
017400010605     d wdtda01         s              8  0
017500010606     d wdtda02         s              8  0
017600130118     d wORMcor         s              7s 0 inz
017700010605
017800030918     d sav_trc         s                   like(f_rortrc)
017900010608     d sav_data        s                   like(f_rordae)
018000010621     d sav_dis         s                   like(f_rordis)
018100010621     d sav_are         s                   like(f_rorare)
018200010621     d sav_poel        s                   like(f_rorpoe)
018300010620     d sav_dsdatap     s                   like(f_rordae)
018400010620     d sav_dspoelp     s                   like(f_rorpoe)
018500010620     d sav_dsdatac     s                   like(f_rordae)
018600010620     d sav_dspoelc     s                   like(f_rorpoe)
018700010614     d sav_orvdar      s                   like(orvdar)
018800010607
018900010612     d xd              s              5i 0 inz(*zeros)                          indice sk date
019000010612     d xa              s              5i 0 inz(*zeros)                          indice sk po da ela.
019100010612     d xf              s              5i 0 inz(*zeros)                          indice sk file
019200020807     d Xb              s              5i 0 inz(*zeros)                          indice sk po TRUL26
019300030918     d sxf             s              5i 0 inz(*zeros)                          indice sk file save
019400130118     d xc              s              5i 0 inz(*zeros)
019500130118     d xx              s              5i 0 inz(*zeros)
019600020808
019700020808     d StringaSql      s            500    Varying
019800010605
019900010608     d wdtda           s               d   datfmt(*iso)
020000010608     d wdtal           s               d   datfmt(*iso)
020100031002
020200010605
020300010605      *   S C H I E R E
020400010605
020500010608     d skdate          s              8  0 dim(31)                              date da elab.
020600020807     d skpoel          s              3  0 dim(250)                             po da elab.
020700030912     d kfile           s             12    dim(18000)                           file (key)
020800010605     d                                     varying
020900010606     d                                     inz
021000030912     d kfile1          s             11    dim(18000)                           file (key) appoggio
021100030912     d                                     varying
021200030912     d                                     inz
021300030912     d rfile           s            456    dim(18000)                           file (record)
021400010605     d                                     varying
021500010606     d                                     inz
021600010621
021700010621     d cmd             s             80    dim(1) ctdata perrcd(1)              comando
021800031002
021900130118     d skcliosr        s              7s 0 dim(999)                             clienti tab OSR.
022000130118     d skstaosr        s              1    dim(999)                             OSR non da elab.
022100060926     d skfar           s              3  0 dim(999)                             fase da non elab.
022200010604
022300010604      *   D S   I N T E R N E / E S T E R N E
022400010604
022500010606     d dskfile         ds                  inz
022600030912     d  dstrc                              Like(f_rortrc)
022700010605     d  dsdata                             like(f_rordae)
022800010608     d  dspoel                             like(f_rorpoe)
022900030912
023000030912     d dskfile1        ds                  inz
023100030912     d  dsdata1                            like(f_rordae)
023200030912     d  dspoel1                            like(f_rorpoe)
023300010605
023400010604     d wlbdat          ds                  inz
023500010604     d  g02dat                 1      8  0
023600010604     d  g02inv                 9     16  0
023700010604     d  g02err                17     17
023800010604     d  g02tgi                18     22  0
023900031002
024000031002     d wosr            ds
024100031002     d  wOrmPoe                1      3
024200031002     d  wOrmNsr                4      5
024300010606
024400010606     d clnmat          ds
024500010606     d  mat                    1     31    dim(31)
024600010606     d clnpom          ds
024700010606     d  pom                    1     31    dim(31)
024800010604
024900010605     d dswfror       e ds                  extname(wfror00f)
025000010605     d                                     prefix(f_)
025100010606     d                                     inz
025200010608     d dswfror0      e ds                  extname(wfror00f)
025300010608     d                                     inz
025400010621     d dswfror1      e ds                  extname(wfror10f)
025500010621     d                                     prefix(p_)
025600010621     d                                     inz
025700140707
025800140707     d wfrords       e ds                  extname(wfror10f) prefix(c_)
025900140707
026000010619     d dcmr          e ds
026100010604     d fior40ds      e ds
026200010604     d cncr80        e ds
026300010604     d ut§dse0f      e ds
026400030912     d dorm01        e ds
026500031002     d dosr          e ds
026600060926     d dfar          e ds
026700020807
026800020807     d FnormxxDs     e ds                  ExtName(Fnorm00f)
026900020807
027000020807     d Trul26Ds      e ds
027100020807     d  skpo                  11    760  0 dim(250)
027200020807     d  skpot                761   1510  0 dim(250)
027300010604
027400010604     d kpjba         e ds
027500010604     d  rem                    1      3
027600010604     d  remfil                 4      6
027700140707
027800140707      // - Driver per reperire mail/sms
027900140707     d FIOR900DS     e ds                  qualified inz
028000140707
028100140707      //---------------------------------------------------------------
028200140707      //?Definizione procedure esterne.
028300140707      //---------------------------------------------------------------
028400140707      // - Driver per reperire mail/sms
028500140707     d FIOR900R        pr                  extpgm('FIOR900R')
028600140707     d  fior900ds                          likeds(fior900ds)
028700010604
028800010604      *--------------------------------------------------------------*
028900010604
029000010608      * stampo la videata delle selezioni
029100010612      *                            ========
029200010604     c                   exsr      PRESELEZ
029300010612      *                            ========
029400020808
029500020808      * Se sede e richiesto un solo p.o. o filiale devo elaborare con SQL
029600020808     c                   If        (I40sede <> 'F' and I40poel <> 999) or
029700020808     c                              I40sede = 'F'
029800020807      * Sql per selezionare gli ORM
029900020807      *                            ========
030000020808     c                   ExSr      SqlOrmPos
030100020807      *                            ========
030200020808     c                   EndIf
030300020807
030400010612     c                   eval      xd = 1
030500010608      * devo elaborare per data
030600010612     c                   do        31            xd
030700010612     c                   if        skdate(xd) = *zeros
030800010608     c                   leave
030900010608     c                   endif
031000010608
031100010612     c                   eval      dataela = skdate(xd)
031200020808
031300020808      * Se sede e richiesti tutti i p.o devo leggere tutto il file
031400020808     c                   If        I40sede <> 'F' and I40poel = 999
031500020808      *                            ===========
031600020808     c                   ExSr      Sr_LeggiOrm
031700020808      *                            ===========
031800020808     c                   Else
031900020808      * Se sede e richiesto un solo p.o. o filiale devo leggere con SQL
032000010614      *                            ========
032100010614     c                   exsr      SR_LEGGI
032200010614      *                            ========
032300020808     c                   EndIf
032400010608     c                   enddo
032500010608
032600010612      * creo il file e stampo la sk
032700010612     c                   eval      xf = 1
032800010612     c                   do        18000         xf
032900010612      * esco se non ho più niente caricato nella sk
033000030912     c                   if        kfile1(xf) = *zeros
033100010608     c                   leave
033200010608     c                   endif
033300010612      * carico le ds
033400010612     c                   eval      dskfile = kfile(xf)
033500030912     c                   eval      dskfile1 = kfile1(xf)
033600010612     c                   eval      dswfror = rfile(xf)
033700010621      * scrivo file appoggio
033800010621     c                   eval      dswfror1 = rfile(xf)
033900010621     c                   write     wfror100
034000010621     c                   clear                   dswfror1
034100010612      * scrivo il file
034200010608     c                   if        i40tpel = 'F'
034300030912     c                   eval      krortrc = dstrc
034400010608     c                   eval      krorpoe = dspoel
034500010608     c                   eval      krordta = dsdata
034600010608     c     kror          chain     wfror01l
034700010608     c                   if        not%found(wfror01l)
034800010612     c                   eval      dswfror0 = rfile(xf)
034900010612      *                            ========
035000010608     c                   write     wfror000
035100010612      *                            ========
035200010612     c                   clear                   dswfror0
035300010608     c                   endif
035400010608     c                   endif
035500010608
035600010608     c                   enddo
035700010621      * stampo
035800010621      *                            =========
035900010621     c                   exsr      SR_STAMPA
036000010621      *                            =========
036100010605
036200010608      * fine programma
036300010605     c                   eval      *inlr = *on
036400010605
036500010604      *--------------------------------------------------------------*
036600010604      * PREPARA LA STAMPA DELLA VIDEATA DELLE SELEZIONI
036700010604      *--------------------------------------------------------------*
036800010604     c     PRESELEZ      begsr
036900010608
037000010608      * se vengono creati i dati, li devo anche cancellare prima
037100010608     c                   if        i40tpel= 'F'
037200010608
037300010612     c                   eval      xa = 1
037400020807     c                   do        250           xa
037500010612     c                   if        skpoel(xa) = *zeros
037600010608     c                   leave
037700010608     c                   endif
037800010608
037900010611     c     *iso          movel     i40dtda       wdtda
038000010611     c     *iso          movel     i40dtal       wdtal
038100010620
038200010608     c                   dow       wdtda<=wdtal
038300010612     c                   eval      krorpoe = skpoel(xa)
038400010608     c                   move      wdtda         krordta
038500030919     c     kror1         setll     wfror01l
038600010608     c                   if        %equal(wfror01l)
038700030919     c     kror1         reade     wfror01l
038800010613     c                   dow       not%eof(wfror01l)
038900010608     c                   delete    wfror000
039000030919     c     kror1         reade     wfror01l
039100010608     c                   enddo
039200010608     c                   endif
039300010608     c
039400010608     c                   adddur    1:*d          wdtda
039500010608     c                   enddo
039600010608
039700010608     c                   enddo
039800010608     c                   endif
039900010618
040000010618      * stampo la prima pagina con le selezioni
040100010618     c                   eval      *in12 = (i40sede = 'S')
040200010618
040300010618     c                   if        i40poel = 999
040400070418     c                   eval      despo = 'Tutte le fil.dell''AS'
040500010618     c                   else
040600010618     c                   eval      kazorg = i40poel
040700010618     c     kazorg        chain     azorg01l
040800010618     c                   if        %found(azorg01l)
040900010618     c                   movel     orgdes        despo
041000010618     c                   endif
041100010618     c                   endif
041200010618
041300010618     c                   clear                   wlbdat
041400010618     c                   move      i40dtda       g02inv
041500010618     c                   movel     '3'           G02err
041600010618     c                   call      'XSRDA8'
041700010618     c                   parm                    wlbdat
041800010618     c                   movel     g02dat        datada
041900010618
042000010618     c                   clear                   wlbdat
042100010618     c                   move      i40dtal       g02inv
042200010618     c                   movel     '3'           G02err
042300010618     c                   call      'XSRDA8'
042400010618     c                   parm                    wlbdat
042500010618     c                   movel     g02dat        dataal
042600010618     c                   write     or41s1
042700010618
042800010621     c   11              write     or41s1c
042900010604     c                   endsr
043000010614
043100010614      *--------------------------------------------------------------*
043200010614      * STAMPA
043300010614      *--------------------------------------------------------------*
043400010614     c     SR_STAMPA     begsr
043500010621
043600010621      * leggo file di comodo per la stampa
043700010621     c     *loval        setll     wfror11l
043800010621 b1  c                   do        *hival
043900010621     c                   read      wfror11l
044000010621 b2  c                   if        %eof(wfror11l)
044100010621     c                   leave
044200010621 e2  c                   endif
044300030916
044400140707      /free
044500140707        IF  p_RORtrc = 'B' or p_RORtrc = 'C';
044600140707          *in15 = *on;
044700140707        ELSE;
044800140707          *in15 = *off;
044900140707        ENDIF;
045000140708        IF  p_RORtrc = 'C';
045100140708          *in17 = *on;
045200140708        ELSE;
045300140708          *in17 = *off;
045400140708        ENDIF;
045500140707       //?Se sto leggendo il rcd 'B' a questo devo sottrarre i dati del rcd 'C'
045600140707        IF  p_RORtrc = 'B';
045700140707          exec sql
045800140707          select * into :wfrords from wfror10f
045900140707          where rordae = :p_rordae and
046000140707                rordis = :p_rordis and
046100140707                rorare = :p_rorare and
046200140707                rorpoe = :p_rorpoe and
046300140707                rortrc = 'C';
046400140707       //?trovo il rcd corrispondente
046500140707          IF  sqlcod = 0;
046600140707         //?sottraggo
046700140707            p_RORoim -= c_RORoim;
046800140707            p_RORode -= c_RORode;
046900140707            p_RORoes -= c_RORoes;
047000140707            p_RORoea -= c_RORoea;
047100140707            p_RORone -= c_RORone;
047200140707            p_RORnqd -= c_RORnqd;
047300140707            p_RORnbl -= c_RORnbl;
047400140707            p_RORmdr -= c_RORmdr;
047500140707            p_RORnas -= c_RORnas;
047600140707            p_RORdan -= c_RORdan;
047700140707            p_RORean -= c_RORean;
047800140707            p_RORaan -= c_RORaan;
047900140707            p_RORnan -= c_RORnan;
048000140708            p_RORdit -= c_RORdit;
048100140708            p_ROReit -= c_ROReit;
048200140708            p_RORait -= c_RORait;
048300140708            p_RORnit -= c_RORnit;
048400140708            p_RORdr1 -= c_RORdr1;
048500140708            p_RORer1 -= c_RORer1;
048600140708            p_RORar1 -= c_RORar1;
048700140708            p_RORnr1 -= c_RORnr1;
048800140708            p_RORdr2 -= c_RORdr2;
048900140708            p_RORer2 -= c_RORer2;
049000140708            p_RORar2 -= c_RORar2;
049100140708            p_RORnr2 -= c_RORnr2;
049200140708            p_RORdro -= c_RORdro;
049300140708            p_RORero -= c_RORero;
049400140708            p_RORaro -= c_RORaro;
049500140708            p_RORnro -= c_RORnro;
049600140708            p_RORa09 -= c_RORa09;
049700140708            p_RORa01 -= c_RORa01;
049800140708            p_RORa02 -= c_RORa02;
049900140708            p_RORa03 -= c_RORa03;
050000140708            p_RORa04 -= c_RORa04;
050100140708            p_RORa05 -= c_RORa05;
050200140708            p_RORa06 -= c_RORa06;
050300140708            p_RORa07 -= c_RORa07;
050400140708            p_RORa08 -= c_RORa08;
050500140708            p_RORa10 -= c_RORa10;
050600140708            p_RORa11 -= c_RORa11;
050700140708            p_RORa12 -= c_RORa12;
050800140708            p_RORa13 -= c_RORa13;
050900140708            p_RORa14 -= c_RORa14;
051000140708            p_RORa15 -= c_RORa15;
051100140708            p_RORn01 -= c_RORn01;
051200140708            p_RORn02 -= c_RORn02;
051300140708            p_RORn03 -= c_RORn03;
051400140708            p_RORn04 -= c_RORn04;
051500140708            p_RORn05 -= c_RORn05;
051600140708            p_RORn06 -= c_RORn06;
051700140708            p_RORn07 -= c_RORn07;
051800140708            p_RORn08 -= c_RORn08;
051900140708            p_RORn09 -= c_RORn09;
052000140708            p_RORn10 -= c_RORn10;
052100140708            p_RORoco -= c_RORoco;
052200140708            p_RORico -= c_RORico;
052300140708            p_RORinc -= c_RORinc;
052400140708            p_RORifi -= c_RORifi;
052500140708            p_RORifian -= c_RORifian;
052600140708            p_RORifine -= c_RORifine;
052700140708            p_RORice -= c_RORice;
052800140708            p_RORicean -= c_RORicean;
052900140708            p_RORicene -= c_RORicene;
053000140708            p_RORiin -= c_RORiin;
053100140708            p_RORiinan -= c_RORiinan;
053200140708            p_RORiinne -= c_RORiinne;
053300140708            p_RORiva -= c_RORiva;
053400140708            p_RORivaan -= c_RORivaan;
053500140708            p_RORivane -= c_RORivane;
053600140708            p_RORicc -= c_RORicc;
053700140708            p_RORiccan -= c_RORiccan;
053800140708            p_RORiccne -= c_RORiccne;
053900140707          ENDIF;
054000140707        ENDIF;
054100140707      /end-free
054200010614
054300010705     ?* lancio fatto con un p.o. e più date devo stampare per p.o.
054400010621     ?* se richiesta la stampa sintetica
054500010621 b2  c                   if        *in13 = *on
054600010621      * rottura per p.o.
054700010621 b3  c                   if        sav_poel <> p_rorpoe
054800010621      * decodifico il p.o. che sto elaborando
054900010621     c                   eval      kazorg = p_rorpoe
055000010621     c     kazorg        chain     azorg01l
055100010621 b4  c                   if        %found(azorg01l)
055200010621     c                   movel     orgdes        despoel
055300010621 e4  c                   endif
055400010621      * decodifico il distretto
055500010621     c                   clear                   descrdis
055600010621     c                   movel     '17'          tblcod
055700010621     c                   movel(P)  p_rordis      tblkey
055800010621     c     ktab          chain     tabel00f
055900010621 b4  c                   if        %found(tabel00f)
056000010621     c                   movel     tbluni        descrdis
056100010621 e4  c                   endif
056200010621      * decodifico l'area
056300010621     c                   clear                   descrare
056400010621     c                   movel     '05'          tblcod
056500010621     c                   movel(P)  p_rorare      tblkey
056600010621     c     ktab          chain     tabel00f
056700010621 b4  c                   if        %found(tabel00f)
056800010621     c                   movel     tbluni        descrare
056900010621 e4  c                   endif
057000010621     c                   write     or41t1
057100010621     c                   write     or41t2
057200010621     c                   write     or41t3
057300010621     c                   write     or41t4
057400010621     c                   eval      sav_poel = p_rorpoe
057500010621 e3  c                   endif
057600010621      * giro la data di elaborazione
057700010621     c                   clear                   wlbdat
057800010621     c                   move      p_rordae      g02inv
057900010621     c                   movel     '3'           G02err
058000010621     c                   call      'XSRDA8'
058100010621     c                   parm                    wlbdat
058200010621     c                   movel     g02dat        dsdatap
058300010621      * salto pagina
058400010621 b3  c                   if        *in90 = *on
058500010621     c                   write     or41t1
058600010621     c                   write     or41t2
058700010621     c                   write     or41t3
058800010621     c                   write     or41t4
058900010621     c                   eval      *in90 = *off
059000010621 e3  c                   endif
059100010621      * calcolo la percentuale dei non eseguiti sui da eseguire
059200010705     c                   if        P_rorode > *zeros
059300010621     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
059400010705     c                   endif
059500030918     c  n15              write     or41d1
059600140708     c                   IF        *in15 and not *in17
059700140708     c                   write     or41d1b
059800140708     c                   ENDIF
059900140708     c                   IF        *in17
060000140708     c                   write     or41d1a
060100140708     c                   ENDIF
060200010621     ?* stampo per data
060300010621 x2  c                   else
060400010621      * rottura per data
060500010621 b3  c                   if        sav_data <> p_rordae
060600010614     c                   clear                   wlbdat
060700010621     c                   move      p_rordae      g02inv
060800010614     c                   movel     '3'           G02err
060900010614     c                   call      'XSRDA8'
061000010614     c                   parm                    wlbdat
061100010614     c                   movel     g02dat        dsdatap
061200010614     c                   write     or41t1
061300010614     c                   write     or41t2
061400010621     c                   eval      sav_data = p_rordae
061500010621     c                   clear                   sav_dis
061600010621     c                   clear                   sav_are
061700010622     c                   clear                   sav_poel
061800030922     c                   clear                   sav_trc
061900010621 e3  c                   endif
062000010621      * rottura per distretto
062100010621 b3  c                   if        sav_dis <> p_rordis
062200010621      * distretto
062300010621     c                   clear                   descrdis
062400010621     c                   movel     '17'          tblcod
062500010621     c                   movel(P)  p_rordis      tblkey
062600010621     c     ktab          chain     tabel00f
062700010621 b4  c                   if        %found(tabel00f)
062800010621     c                   movel     tbluni        descrdis
062900010621 e4  c                   endif
063000010621      * area
063100010621     c                   clear                   descrare
063200010621     c                   movel     '05'          tblcod
063300010621     c                   movel(P)  p_rorare      tblkey
063400010621     c     ktab          chain     tabel00f
063500010621 b4  c                   if        %found(tabel00f)
063600010621     c                   movel     tbluni        descrare
063700010621 e4  c                   endif
063800010621     c                   if        sav_dis <> *blanks
063900010621     c                   write     or41t1
064000010621     c                   write     or41t2
064100010621     c                   endif
064200010621     c                   write     or41t3
064300010621     c                   write     or41t4
064400010621     c                   eval      sav_dis = p_rordis
064500010621     c                   eval      sav_are = p_rorare
064600010622     c                   clear                   sav_poel
064700031002     c                   clear                   sav_trc
064800010621 e3  c                   endif
064900010621      * rottura per area
065000010621 b3  c                   if        sav_are <> p_rorare
065100010621      * area
065200010621     c                   clear                   descrare
065300010621     c                   movel     '05'          tblcod
065400010621     c                   movel(P)  p_rorare      tblkey
065500010621     c     ktab          chain     tabel00f
065600010621 b4  c                   if        %found(tabel00f)
065700010621     c                   movel     tbluni        descrare
065800010621 e4  c                   endif
065900010621     c                   if        sav_are <> *zeros
066000010621     c                   write     or41t1
066100010621     c                   write     or41t2
066200010621     c                   endif
066300010621     c                   write     or41t3
066400010621     c                   write     or41t4
066500010621     c                   eval      sav_are = p_rorare
066600010622     c                   clear                   sav_poel
066700031002     c                   clear                   sav_trc
066800010621 e3  c                   endif
066900030922      * rottura per tipo record
067000030922      * se richiesta la stampa analitica
067100030922      * dopo la pagina dei totali x p.o. devo stampare i commissionati
067200030922      * se richiesta la stampa sintetica
067300030922      * dopo la riga dei totali x p.o. devo stampare i commissionati
067400030922 b3  c                   if        sav_trc <> p_rortrc
067500030922      * se non è il primo tipo record che leggo (quindi non è quello totale)
067600030922      * pulisco il sav_poel in modo da avere la rottura x p.o.
067700030922      * e stampare i commissionati in una pagina nuova
067800031002 b4  c                   If        sav_trc <> *Blanks
067900031002 b5  c                   If        i40tpst = 'A'
068000030922      * salto pagina
068100030922     c                   write     or41t1
068200030922     c                   write     or41t2
068300030922     c                   write     or41t3
068400030922     c                   write     or41t4
068500031002 e5  c                   EndIf
068600030922     c                   Clear                   sav_poel
068700031002 e4  c                   EndIf
068800030922     c                   Eval      sav_trc = p_rortrc
068900030922 e3  c                   EndIf
069000010621      * rottura per p.o.
069100010621 b3  c                   if        sav_poel <> p_rorpoe
069200010614      * decodifico il p.o. che sto elaborando
069300010621     c                   eval      kazorg = p_rorpoe
069400010614     c     kazorg        chain     azorg01l
069500010621 b4  c                   if        %found(azorg01l)
069600010614     c                   movel     orgdes        despoel
069700010621 e4  c                   endif
069800010614     ?* stampa sintetica
069900010621 b4  c                   if        i40tpst = 'S'
070000010614      * salto pagina
070100010621 b5  c                   if        *in90 = *on
070200010621     c                   write     or41t1
070300010621     c                   write     or41t2
070400010621     c                   write     or41t3
070500010621     c                   write     or41t4
070600010614     c                   eval      *in90 = *off
070700010621 e5  c                   endif
070800030922      * calcolo la percentuale dei non eseguiti sui da eseguire
070900030922     c                   clear                   perone
071000030922     c                   if        P_rorode > *zeros
071100030922     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
071200030922     c                   endif
071300030922      * stampo totali p.o.
071400030922     c  n15              write     or41d1
071500140708     c                   IF        *in15 and not *in17
071600140708     c                   write     or41d1b
071700140708     c                   ENDIF
071800140708     c                   IF        *in17
071900140708     c                   write     or41d1a
072000140708     c                   ENDIF
072100010621 e4  c                   endif
072200010622     ?* stampa analitica
072300010622 b4  c                   if        i40tpst = 'A'
072400010622      * salto pagina
072500010622 b5  c                   if        sav_poel <> *zeros
072600010622     c                   write     or41t1
072700010622     c                   write     or41t2
072800010622     c                   write     or41t3
072900010622     c                   write     or41t4
073000010622 e5  c                   endif
073100010614      * calcolo la percentuale dei non eseguiti sui da eseguire
073200010625     c                   clear                   perone
073300010625     c                   if        P_rorode > *zeros
073400010621     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
073500010625     c                   endif
073600010622      * stampo totali p.o.
073700010614     c                   write     or41d1
073800010622      * stampo dettaglio
073900010614      *                            =======
074000010614     c                   exsr      SR_DETT
074100010614      *                            =======
074200010621 e4  c                   endif
074300010621     c                   eval      sav_poel = p_rorpoe
074400010621 e3  c                   endif
074500010621
074600010621 e2  c                   endif
074700010621
074800010621 e1  c                   enddo
074900010621
075000010621      * fine stampa
075100010621     c                   write     or41e1
075200010614
075300010614     c                   endsr
075400010614
075500010614      *--------------------------------------------------------------*
075600010614      * STAMPO IL DETTAGLIO
075700010614      *--------------------------------------------------------------*
075800010614     c     SR_DETT       begsr
075900010614
076000010614     c                   eval      wokstpcau = *off
076100010614
076200010614      * affidabilità
076300010614     c                   write     or41a1
076400010614
076500010621     c                   if        p_rordan > 0 or p_rorean > 0 or
076600010621     c                             p_roraan > 0 or p_rornan > 0
076700010614     c                   write     or41a11
076800010614     c                   endif
076900010621     c                   if        p_rordit > 0 or p_roreit > 0 or
077000010621     c                             p_rorait > 0 or p_rornit > 0
077100010614     c                   write     or41a12
077200010614     c                   endif
077300010621     c                   if        p_rordr1 > 0 or p_rorer1 > 0 or
077400010621     c                             p_rorar1 > 0 or p_rornr1 > 0
077500010614     c                   write     or41a13
077600010614     c                   endif
077700010621     c                   if        p_rordr2 > 0 or p_rorer2 > 0 or
077800010621     c                             p_rorar2 > 0 or p_rornr2 > 0
077900010614     c                   write     or41a14
078000010614     c                   endif
078100010621     c                   if        p_rordro > 0 or p_rorero > 0 or
078200010621     c                             p_roraro > 0 or p_rornro > 0
078300010614     c                   write     or41a15
078400010614     c                   endif
078500031105
078600031105      * O.R.M. non abbinati
078700031105     c                   If        p_rora09 > *Zeros
078800031105     c                   write     or41a29
078900031105     c                   endif
079000010614
079100010614      * causali es.anomali
079200030919     c                   if        p_rora01 > 0 or p_rora02 > 0 or
079300010621     c                             p_rora03 > 0 or p_rora04 > 0 or
079400010621     c                             p_rora05 > 0 or p_rora06 > 0 or
079500010621     c                             p_rora07 > 0 or p_rora08 > 0 or
079600010621     c                             p_rora09 > 0 or p_rora10 > 0 or
079700010621     c                             p_rora11 > 0 or p_rora12 > 0 or
079800010621     c                             p_rora13 > 0 or p_rora14 > 0 or
079900030919     c                             p_rora15 > 0
080000010614     c                   eval      wokstpcau = *on
080100010614     c                   write     or41a2
080200030919     c                   if        p_rora01 > 0
080300010614     c                   write     or41a21
080400010614     c                   endif
080500030919     c                   if        p_rora02 > 0
080600010614     c                   write     or41a22
080700010614     c                   endif
080800030919     c                   if        p_rora03 > 0
080900010614     c                   write     or41a23
081000010614     c                   endif
081100030919     c                   if        p_rora04 > 0
081200010614     c                   write     or41a24
081300010614     c                   endif
081400030919     c                   if        p_rora05 > 0
081500010614     c                   write     or41a25
081600010614     c                   endif
081700030919     c                   if        p_rora06 > 0
081800010614     c                   write     or41a26
081900010614     c                   endif
082000030919     c                   if        p_rora07 > 0
082100010614     c                   write     or41a27
082200010614     c                   endif
082300030919     c                   if        p_rora08 > 0
082400010614     c                   write     or41a28
082500010614     c                   endif
082600030919     c                   if        p_rora10 > 0
082700010614     c                   write     or41a210
082800010614     c                   endif
082900030919     c                   if        p_rora11 > 0
083000010614     c                   write     or41a211
083100010614     c                   endif
083200030919     c                   if        p_rora12 > 0
083300010614     c                   write     or41a212
083400010614     c                   endif
083500030919     c                   if        p_rora13 > 0
083600010614     c                   write     or41a213
083700010614     c                   endif
083800030919     c                   if        p_rora14 > 0
083900010614     c                   write     or41a214
084000010614     c                   endif
084100030919     c                   if        p_rora15 > 0
084200010614     c                   write     or41a215
084300010614     c                   endif
084400010614     c                   endif
084500010614
084600010614      * causali non eseguiti
084700010621     c                   if        p_rorn01 > 0 or p_rorn02 > 0 or
084800010621     c                             p_rorn03 > 0 or p_rorn04 > 0 or
084900010621     c                             p_rorn05 > 0 or p_rorn06 > 0 or
085000010621     c                             p_rorn07 > 0 or p_rorn08 > 0 or
085100030916     c                             p_rorn09 > 0 or p_rorn10 > 0 or
085200030916     c                             p_rorn01d > 0 or p_rorn02d > 0 or
085300030916     c                             p_rorn03d > 0 or p_rorn04d > 0 or
085400030916     c                             p_rorn05d > 0 or p_rorn06d > 0 or
085500030916     c                             p_rorn07d > 0 or p_rorn08d > 0 or
085600030916     c                             p_rorn09d > 0 or p_rorn10d > 0
085700010614     c                   if        wokstpcau = *on
085800010614     c                   write     or41a3
085900010614     c                   else
086000010614     c                   write     or41a2
086100010614     c                   endif
086200030916     c                   if        p_rorn01 > 0 or p_rorn01d > 0
086300010614     c                   write     or41a31
086400010614     c                   endif
086500030916     c                   if        p_rorn02 > 0 or p_rorn02d > 0
086600010614     c                   write     or41a32
086700010614     c                   endif
086800030916     c                   if        p_rorn03 > 0 or p_rorn03d > 0
086900010614     c                   write     or41a33
087000010614     c                   endif
087100030916     c                   if        p_rorn04 > 0 or p_rorn04d > 0
087200010614     c                   write     or41a34
087300010614     c                   endif
087400030916     c                   if        p_rorn05 > 0 or p_rorn05d > 0
087500010614     c                   write     or41a35
087600010614     c                   endif
087700030916     c                   if        p_rorn06 > 0 or p_rorn06d > 0
087800010614     c                   write     or41a36
087900010614     c                   endif
088000030916     c                   if        p_rorn07 > 0 or p_rorn07d > 0
088100010614     c                   write     or41a37
088200010614     c                   endif
088300030916     c                   if        p_rorn08 > 0 or p_rorn08d > 0
088400010614     c                   write     or41a38
088500010614     c                   endif
088600030916     c                   if        p_rorn09 > 0 or p_rorn09d > 0
088700010614     c                   write     or41a39
088800010614     c                   endif
088900030916     c                   if        p_rorn10 > 0 or p_rorn10d > 0
089000010614     c                   write     or41a310
089100010614     c                   endif
089200010614     c                   endif
089300010614
089400030919      * queste righe non le devo stampare x i soli ORM commissionati
089500030919     c                   If        not *In15
089600010614      * commissionati
089700010621     c                   if        p_roroco > 0
089800010614     c                   write     or41a4
089900010614     c                   endif
090000010614
090100010614      * tipo cliente
090200010614     c                   write     or41a5
090300010621     c                   if        p_rorico > 0
090400010614     c                   write     or41a51
090500010614     c                   endif
090600010621     c                   if        p_rorinc > 0
090700010614     c                   write     or41a52
090800010614     c                   endif
090900010614
091000010614      * tipo comunicazione
091100010614     c                   write     or41a6
091200030916     c                   if        p_rorifi > 0 or p_rorifian > 0 or
091300030916     c                             p_rorifid > 0 or p_rorifine  > 0
091400010614     c                   write     or41a61
091500010614     c                   endif
091600030916     c                   if        p_rorice > 0 or p_roricean > 0 or
091700030916     c                             p_roriced > 0 or p_roricene  > 0
091800010614     c                   write     or41a62
091900010614     c                   endif
092000030916     c                   if        p_roriin > 0 or p_roriinan > 0 or
092100030916     c                             p_roriind > 0 or p_roriinne  > 0
092200010614     c                   write     or41a63
092300010614     c                   endif
092400030916     c                   if        p_roriva > 0 or p_rorivaan > 0 or
092500030916     c                             p_rorivad > 0 or p_rorivane  > 0
092600010614     c                   write     or41a64
092700010614     c                   endif
092800030916     c                   if        p_roricc > 0 or p_roriccan > 0 or
092900030916     c                             p_roriccd > 0 or p_roriccne  > 0
093000010614     c                   write     or41a65
093100010614     c                   endif
093200030919
093300030919     c                   EndIF
093400010614
093500010614     c                   endsr
093600010614
093700020807      *--------------------------------------------------------------*
093800020807      * SQL  - Preparazione sql FNORM00F
093900020807      *--------------------------------------------------------------*
094000020807     c     SqlOrmPos     BegSr
094100020807
094200020808      * Preparo la stringa sql
094300020808     c                   Eval      StringaSql =
094400020808     c                             'select * '                                 +
094500020808     c                             'from fnorm00f where ormpoe in ('
094600020808      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
094700020808     c                   Do        250           Xa
094800020808     c                   If        Skpoel(Xa) = *Zeros
094900020808     c                   Leave
095000020808     c                   EndIf
095100020808     c                   Eval      StringaSql =
095200020808     c                             %trim(StringaSql) +  ' ' +
095300020808     c                             %editc(Skpoel(Xa):'1') + ','
095400020808     c                   EndDo
095500020808     c                   Eval      StringaSql =
095600020808     c                             %trim(StringaSql) + ') or ormpor in ('
095700020808      * ciclo sulla sk p.o. da elaborare per i p.o. ritiro
095800020808     c                   Do        250           Xa
095900020808     c                   If        Skpoel(Xa) = *Zeros
096000020808     c                   Leave
096100020808     c                   EndIf
096200020808     c                   Eval      StringaSql =
096300020808     c                             %trim(StringaSql) + ' ' +
096400020808     c                             %editc(Skpoel(Xa):'1') + ','
096500020808     c                   EndDo
096600020808     c                   Eval      StringaSql =
096700020808     c                             %trim(StringaSql) + ') for fetch only'
096800020807
096900020807     C/EXEC SQL
097000020808     C+ prepare S1 from :StringaSql
097100020807     C/END-EXEC
097200020807
097300020808     C/EXEC SQL
097400020808     C+ declare C1 cursor for S1
097500020808     C/END-EXEC
097600020807
097700020807     c                   EndSr
097800020807
097900010604      *--------------------------------------------------------------*
098000020808      * LEGGO IL FILE DEGLI ORM DA SQL
098100010604      *--------------------------------------------------------------*
098200010614     c     SR_LEGGI      begsr
098300010604
098400020808     C/EXEC SQL
098500020808     C+ Open C1
098600020808     C/END-EXEC
098700020808
098800020808     C/EXEC SQL
098900020808     C+ Fetch C1 into :fnormxxds
099000020808     C/END-EXEC
099100020808
099200020808 b1  c                   Dow       Sqlcod = 0                                   *record letto
099300010608      * no gli annullati
099400020808 b2  c                   If        Ormatb = *Blanks
099500010608
099600020808      * data immissione ORM non deve essere superiore alla data elaborazione
099700020808 b3  c                   If        Ormdao <= Dataela
099800030321
099900030321      * se sono in filiale elaboro tutto, se sono in sede devo controllare anche la
100000030321      * data pulizia ORM di filiale
100100030321     c                             and (I40Sede = 'F'  or
100200030321     c                                 (I40Sede <> 'F' and Ormdao >= I40dtpo))
100300031002
100400031002      * controllo se posso elaborare la serie
100500031002     c                   Move      OrmPoe        wOrmPoe
100600130118     c*******            Move      OrmNsr        wOrmNsr
100700130118     c                   eval      *in99 = *off
100800130118     c                   IF        ORMnsr > 0
100900130118     c                   eval      wORMcor =
101000130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
101100130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
101200130118     c                   IF        xx = 0
101300130118     c                   eval      *in99 = *on
101400130118     c                   ENDIF
101500131105     c                   IF        not *in99
101600131105     c                   IF        skstaosr(xx) <> *blanks
101700130118     c                   eval      *in99 = *on
101800130118     c                   ENDIF
101900131105     c                   ENDIF
102000130118     c                   ENDIF
102100130118     c*****wosr          Lookup    skosr                                  99
102200031002 b4  c                   If        Not *In99
102300020808
102400020808      *                            ==========
102500020808     c                   ExSr      Sr_Elabora
102600020808      *                            ==========
102700031002 b4  c                   EndIf
102800020808 b3  c                   EndIf
102900020808 b2  c                   EndIf
103000020808
103100020808     C/EXEC SQL
103200020808     C+ Fetch C1 into :fnormxxds
103300020808     C/END-EXEC
103400020808
103500020808 b1  c                   EndDo
103600020808
103700020808     C/EXEC SQL
103800020808     C+ close C1
103900020808     C/END-EXEC
104000020808
104100020808     c                   EndSr
104200010614
104300020808      *--------------------------------------------------------------*
104400020808      * LEGGO IL FILE DEGLI ORM
104500020808      *--------------------------------------------------------------*
104600020808     c     Sr_LeggiOrm   BegSr
104700020808
104800020808     c     *Start        Setll     Fnorm00f
104900020808     c                   Do        *Hival
105000020808     c                   Read      Fnorm00f
105100020808      * Fine file
105200020808 b1  c                   If        %Eof(Fnorm00f)
105300020808     c                   Leave
105400020808     c                   EndIf
105500020808      * no gli annullati
105600020808 b2  c                   If        Ormatb = *Blanks
105700020808
105800020808      * data immissione ORM non deve essere superiore alla data elaborazione
105900020808 b3  c                   If        Ormdao <= Dataela
106000030321
106100030321      * e controllo anche la data pulizia ORM di filiale
106200030321     c                             and Ormdao >= I40dtpo
106300031002
106400031002      * controllo se posso elaborare la serie
106500031002     c                   Move      OrmPoe        wOrmPoe
106600130118     c*******            Move      OrmNsr        wOrmNsr
106700130118     c                   eval      *in99 = *off
106800130118     c                   IF        ORMnsr > 0
106900130118     c                   eval      wORMcor =
107000130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
107100130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
107200130118     c                   IF        xx = 0
107300130118     c                   eval      *in99 = *on
107400130118     c                   ENDIF
107500130118     c                   IF        skstaosr(xx) <> *blanks
107600130118     c                   eval      *in99 = *on
107700130118     c                   ENDIF
107800130118     c                   ENDIF
107900130118     c*****wosr          Lookup    skosr                                  99
108000031002 b4  c                   If        Not *In99
108100020808
108200020808      *                            ==========
108300020808     c                   ExSr      Sr_Elabora
108400020808      *                            ==========
108500031002 b4  c                   EndIf
108600020808 b3  c                   EndIf
108700020808 b2  c                   EndIf
108800020808
108900020808 b1  c                   EndDo
109000020808
109100020808     c                   EndSr
109200020808
109300020808      *--------------------------------------------------------------*
109400020808      * ELABORO I DATI LETTI
109500020808      *--------------------------------------------------------------*
109600020808     c     Sr_Elabora    BegSr
109700020808
109800020808      * campi di appoggio
109900020808     c                   eval      xf = 1
110000020808     c                   eval      wpoe = *off
110100020808     c                   eval      wpor = *off
110200020808     c                   eval      wokstp = *off
110300020808     c                   eval      wok900 = *off
110400020808     c                   eval      wok600 = *off
110500020808     c                   eval      wokrode = *off
110600020808     c                   eval      wokroes = *off
110700020808     c                   eval      wokrfile = *off
110800020808     c                   eval      wokfestae = *off
110900020808     c                   eval      wokfestar = *off
111000020808
111100020808     c                   clear                   cauone
111200020808     c                   clear                   cauoea
111300030912
111400030912     c                   Eval      dorm01 = OrmFlo
111500030912      * ORM commissionato
111600030918     c                   Eval      *in14 = (§OrCom = 'S')
111700140707      /free
111800140707         *in16 = *off;
111900140707       //?ORM commissionato con Alert
112000140707         fior900ds.IOR900trc = 'A';
112100140707         fior900ds.IOR900rcdm = 'MA';
112200140707         fior900ds.IOR900rcds = 'S ';
112300140707         fior900ds.IOR900poe = ORMpoe;
112400140707         fior900ds.IOR900nsr = ORMnsr;
112500140707         fior900ds.IOR900nor = ORMnor;
112600140707         fior900ds.IOR900nrv = ORMnrv;
112700140707         fior900r (fior900ds);
112800140707         IF  fior900ds.OOR900err <> 'N';
112900140707           *in16 = *on;
113000140707         ENDIF;
113100140707      /end-free
113200020808
113300020808      * controllo se il po emissione è nella sk dei p.o. da elaborare
113400020808     c                   eval      xa = 1
113500020808     c     Ormpoe        Lookup    Skpoel(Xa)                             30
113600020808     c                   If        *In30
113700020808     c                   eval      wpoe = *on
113800020808     c                   eval      dspoele = skpoel(xa)
113900020808     c                   endif
114000020808
114100020808      * controllo se il po ritiro è nella sk dei p.o. da elaborare
114200020808     c                   eval      xa = 1
114300020808     c     Ormpor        Lookup    Skpoel(Xa)                             30
114400020808     c                   If        *In30
114500020808     c                   eval      wpor = *on
114600020808     c                   eval      dspoelr = skpoel(xa)
114700020808     c                   endif
114800020808
114900020808      * se la data che sto elaborando è un festivo non devo calcolare
115000020808      *                            ========
115100020808     c                   exsr      SR_FESTA
115200020808      *                            ========
115300020808
115400020808     c                   if        wokfestae = *off and
115500020808     c                             wokfestar = *off
115600020808     c                   eval      dsdata = dataela
115700020808     c                   endif
115800020808
115900020808      * calcolo le date per affidabilità
116000020808      *                            =======
116100020808     c                   exsr      SR_DATA
116200020808      *                            =======
116300020808
116400020808      * pulisco i campi di comodo
116500020808      *                            ==========
116600020808     c                   exsr      SR_PULISCI
116700020808      *                            ==========
116800020808
116900020808      * faccio i controlli con il p.o. emissione
117000020808 b2  c                   if        wpoe = *on
117100020808      * se festa non faccio niente
117200020808 b3  c                   if        wokfestae = *off
117300020808      *                            ======
117400020808     c                   exsr      SR_POE
117500020808      *                            ======
117600020808 e3  c                   endif
117700020808 e2  c                   endif
117800020808
117900020808      * faccio i controlli con il p.o. ritiro
118000020808 b2  c                   if        wpor = *on
118100020808      * se festa non faccio niente
118200020808 b3  c                   if        wokfestae = *off
118300020808
118400020808      *                            ======
118500020808     c                   exsr      SR_POR
118600030916
118700030916 b4  c                   if        wokrfile = *on
118800020808      *                            ======
118900030918      * RECORD 'A'
119000020808      * controllo se esiste già un elemento di sk = alla mia key
119100020808      * e carico la sk relativa al file
119200020808     c                   eval      dspoel = dspoelr
119300030912     c                   eval      dspoel1 = dspoelr
119400030912     c                   eval      dsdata1 = dsdata
119500030918     c                   Eval      dstrc = 'A'
119600020808      *                            ==========
119700020808     c                   exsr      SR_CONTRSK
119800020808      *                            ==========
119900020808      * carico la sk del file
120000020808      *                            =========
120100020808     c                   exsr      SR_CARICA
120200020808      *                            =========
120300030918      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
120400030918     c                   Eval      sxf = xf
120500030918      *
120600140707      * RECORD 'B' ORM COMMISSIONATI  (ALERT + TELEFONO)
120700030912      * controllo se esiste già un elemento di sk = alla mia key
120800030912      * e carico la sk relativa al file
120900030916 b5  c                   If        *In14
121000030918     c                   Eval      dstrc = 'B'
121100030912      *                            ==========
121200030912     c                   exsr      SR_CONTRSK
121300030912      *                            ==========
121400030918      * carico la sk del file x record 'B' orm commissionato
121500140707      * Telefono + Alert
121600030912      *                            =========
121700030912     c                   exsr      SR_CARICA
121800030912      *                            =========
121900030916 e5  c                   EndIf
122000140707      *
122100140707      * RECORD 'C' ORM COMMISSIONATI  (SOLO ALERT)
122200140707      * controllo se esiste già un elemento di sk = alla mia key
122300140707      * e carico la sk relativa al file
122400140707 b5  c                   If        *In16
122500140707     c                   Eval      dstrc = 'C'
122600140707      *                            ==========
122700140707     c                   exsr      SR_CONTRSK
122800140707      *                            ==========
122900140707      * carico la sk del file x record 'C' orm commissionato
123000140707      * solo Alert
123100140707      *                            =========
123200140707     c                   exsr      SR_CARICA
123300140707      *                            =========
123400140707 e5  c                   EndIf
123500020808 e4  c                   endif
123600020808 e3  c                   endif
123700020808 e2  c                   endif
123800030916      * ORM commissionato
123900030916     c                   If        *In14
124000030916     c                   Eval      commis = 'X'
124100030916     c                   Else
124200030916     c                   Clear                   commis
124300030916     c                   EndIf
124400140708      /free
124500140708       //?ORM commissionato con Alert
124600140708         IF  *in16;
124700140708           comalert = 'X';
124800140708         ELSE;
124900140708           clear comalert;
125000140708         ENDIF;
125100140708      /end-free
125200020808
125300020808     ?* stampa di comodo EDP
125400020808 b2  c   10              if        wokstp = *on
125500030918     c                   eval      dswfror = rfile(sxf)
125600020808      * rottura di data
125700020808 b3  c                   if        dsdata <> sav_dsdatap
125800020808     c                   clear                   wlbdat
125900020808     c                   move      dsdata        g02inv
126000020808     c                   movel     '3'           G02err
126100020808     c                   call      'XSRDA8'
126200020808     c                   parm                    wlbdat
126300020808     c                   movel     g02dat        dsdatap
126400020808      * decodifico il p.o.
126500020808     c                   eval      kazorg = dspoel
126600020808     c     kazorg        chain     azorg01l
126700020808 b4  c                   if        %found(azorg01l)
126800020808     c                   movel     orgdes        despoel
126900020808 e4  c                   endif
127000020808     c                   write     or41t1p
127100020808     c                   eval      sav_dsdatap = dsdata
127200020808     c                   eval      sav_dspoelp = dspoel
127300020808 e3  c                   endif
127400020808      * rottura di p.o.
127500020808 b3  c                   if        dspoel <> sav_dspoelp
127600020808      * decodifico il p.o.
127700020808     c                   eval      kazorg = dspoel
127800020808     c     kazorg        chain     azorg01l
127900020808 b4  c                   if        %found(azorg01l)
128000020808     c                   movel     orgdes        despoel
128100020808 e4  c                   endif
128200020808     c                   write     or41t1p
128300020808     c                   eval      sav_dspoelp = dspoel
128400020808 e3  c                   endif
128500020808      * salto pagina
128600020808 b3  c                   if        *in91 = *on
128700020808     c                   write     or41t1p
128800020808     c                   eval      *in91 = *off
128900020808 e3  c                   endif
129000020808      * dettaglio
129100020808     c                   write     or41d1p
129200020808 e2  c                   endif
129300020808
129400020808     ?* stampa di controllo
129500020808 b2  c   11              if        wokstp = *on
129600030918     c                   eval      dswfror = rfile(sxf)
129700020808      * rottura di data
129800020808 b3  c                   if        dsdata <> sav_dsdatac
129900020808     c                   clear                   wlbdat
130000020808     c                   move      dsdata        g02inv
130100020808     c                   movel     '3'           G02err
130200020808     c                   call      'XSRDA8'
130300020808     c                   parm                    wlbdat
130400020808     c                   movel     g02dat        dsdatap
130500020808      * decodifico il p.o.
130600020808     c                   eval      kazorg = dspoel
130700020808     c     kazorg        chain     azorg01l
130800020808 b4  c                   if        %found(azorg01l)
130900020808     c                   movel     orgdes        despoel
131000020808 e4  c                   endif
131100020808     c                   write     or41t1c
131200020808     c                   eval      sav_dsdatac = dsdata
131300020808     c                   eval      sav_dspoelc = dspoel
131400020808 e3  c                   endif
131500020808      * rottura di p.o.
131600020808 b3  c                   if        dspoel <> sav_dspoelc
131700020808      * decodifico il p.o.
131800020808     c                   eval      kazorg = dspoel
131900020808     c     kazorg        chain     azorg01l
132000020808 b4  c                   if        %found(azorg01l)
132100020808     c                   movel     orgdes        despoel
132200020808 e4  c                   endif
132300020808     c                   write     or41t1c
132400020808     c                   eval      sav_dspoelc = dspoel
132500020808 e3  c                   endif
132600020808      * salto pagina
132700020808 b3  c                   if        *in92 = *on
132800020808     c                   write     or41t1c
132900020808     c                   eval      *in92 = *off
133000020808 e3  c                   endif
133100020808 b3  c                   if        w_rorone <> *blanks or
133200020808     c                             w_rornqd <> *blanks or
133300020808     c                             w_rornbl <> *blanks or
133400020808     c                             w_rormdr <> *blanks or
133500031105     c                             w_rornas <> *blanks
133600020808      * giro data immissione ORM
133700020808     c                   clear                   wlbdat
133800020808     c                   move      ormdao        g02inv
133900020808     c                   movel     '3'           G02err
134000020808     c                   call      'XSRDA8'
134100020808     c                   parm                    wlbdat
134200020808     c                   movel     g02dat        w_ormdao
134300020808      * dettaglio
134400020808     c                   write     or41d1c
134500020808 e3  c                   endif
134600020808 e2  c                   endif
134700020808
134800020808     c                   EndSr
134900020808
135000010612      *--------------------------------------------------------------*
135100010612      * CONTROLLO SE LA DATA CHE STO ELABORANDO E' FESTIVO
135200010612      *--------------------------------------------------------------*
135300010614     c     SR_FESTA      begsr
135400010612
135500010612     c                   eval      wokfestae = *off
135600010612     c                   eval      wokfestar = *off
135700010612
135800010612     c     *iso          movel     dataela       wdtda
135900010612     c                   extrct    wdtda:*y      kann
136000010612     c                   extrct    wdtda:*m      kmes
136100010612     c                   extrct    wdtda:*d      wgiorno
136200011217     c                   clear                   ktfa
136300010612      * cerco per il p.o. emissione
136400010612     c                   if        wpoe = *on
136500011217     c                   eval      ktfp = dspoele
136600010612     c     kazcln        chain     azcln01l
136700010612     c                   if        %found(azcln01l) and
136800010612     c                             (MAT(wgiorno) =  'F'
136900010612     c                                    or  POM(wgiorno) =  'F')
137000010612     c                   eval      wokfestae = *on
137100010612     c                   endif
137200010612     c                   endif
137300010612
137400010612      * cerco per il p.o. ritiro
137500010612     c                   if        wpor = *on
137600011217     c                   eval      ktfp = dspoelr
137700010612     c     kazcln        chain     azcln01l
137800010612     c                   if        %found(azcln01l) and
137900010612     c                             (MAT(wgiorno) =  'F'
138000010612     c                                    or  POM(wgiorno) =  'F')
138100010612     c                   eval      wokfestar = *on
138200010612     c                   endif
138300010612     c                   endif
138400010612
138500010612     c                   endsr
138600010614
138700010608      *--------------------------------------------------------------*
138800010608      * CALCOLO DATE X AFFIDABILITA'
138900010608      *--------------------------------------------------------------*
139000010614     c     SR_DATA       begsr
139100010608
139200010612      * calcolo la data da - 1
139300010611     c     *iso          movel     dataela       wdtda
139400010608     c                   do        *hival
139500010608     c                   subdur    1:*d          wdtda
139600010608     c                   extrct    wdtda:*y      kann
139700010608     c                   extrct    wdtda:*m      kmes
139800010608     c                   extrct    wdtda:*d      wgiorno
139900011217     c                   clear                   ktfa
140000020808     c                   If        Dspoelr <> *Zeros
140100020808     c                   eval      ktfp = Dspoelr
140200020808     c                   Else
140300020808     c                   Eval      Ktfp = Dspoele
140400020808     c                   EndIf
140500010608     c     kazcln        chain     azcln01l
140600010608     c                   if        not%found(azcln01l)
140700010608     c                   leave
140800010608     c                   endif
140900010608     c                   if        MAT(wgiorno) =  'F'
141000010608     c                             or  POM(wgiorno) =  'F'
141100010608     c                   iter
141200010608     c                   endif
141300010608     c                   leave
141400010608     c                   enddo
141500010611     c                   move      wdtda         wdtda01
141600010608
141700010608      * calcolo la data da - 2
141800010611     c     *iso          movel     wdtda01       wdtda
141900010608     c                   do        *hival
142000010611     c                   subdur    1:*d          wdtda
142100010608     c                   extrct    wdtda:*y      kann
142200010608     c                   extrct    wdtda:*m      kmes
142300010608     c                   extrct    wdtda:*d      wgiorno
142400011217     c                   clear                   ktfa
142500020808     c                   If        Dspoelr <> *Zeros
142600020808     c                   eval      ktfp = Dspoelr
142700020808     c                   Else
142800020808     c                   Eval      Ktfp = Dspoele
142900020808     c                   EndIf
143000010608     c     kazcln        chain     azcln01l
143100010608     c                   if        not%found(azcln01l)
143200010608     c                   leave
143300010608     c                   endif
143400010608     c                   if        MAT(wgiorno) =  'F'
143500010608     c                             or  POM(wgiorno) =  'F'
143600010608     c                   iter
143700010608     c                   endif
143800010608     c                   leave
143900010608     c                   enddo
144000010611     c                   move      wdtda         wdtda02
144100010608
144200010608     c                   endsr
144300010614
144400010605      *--------------------------------------------------------------*
144500010605      * CONTROLLO CON IL PO EMISSIONE
144600010605      *--------------------------------------------------------------*
144700010605     c     SR_POE        begsr
144800010620
144900010620      * se la data del p.o. emissione non è festa controllare se il giorno prima
145000010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
145100010620 b1  c                   if        wokfestae = *off
145200010620     c                   eval      dspoel = dspoele
145300010620      *                            ========
145400010620     c                   exsr      SR_DTFES
145500010620      *                            ========
145600010612
145700010620 b2  c                   if        ormdao = dataela or
145800010620     c                             (wdatafes > 0 and ormdao >= wdatafes and
145900010620     c                              ormdao < dataela)
146000010611     c                   eval      dspoel = dspoele
146100030912     c                   eval      dspoel1= dspoele
146200030912     c                   eval      dsdata1 = dsdata
146300030918     c                   Eval      dstrc = 'A'
146400010608      * controllo se esiste già un elemento di sk = alla mia key
146500010608      * e carico la sk relativa al file
146600010614      *                            ==========
146700010608     c                   exsr      SR_CONTRSK
146800010614      *                            ==========
146900010605
147000010605     ?* immessi ?
147100010605     c                   eval      f_roroim = f_roroim +1
147200010605     ?* commissionati ?
147300010620 b3  c                   if        ormpoe <> ormpor
147400010605     c                   eval      f_roroco = f_roroco +1
147500010620 e3  c                   endif
147600010605     ?* codificati ?
147700010620 b3  c                   if        ormcra <> *zeros
147800010605     c                             or ormcor <> *zeros
147900010605     c                   eval      f_rorico = f_rorico +1
148000010605     ?* non codificati ?
148100010620 x3  c                   else
148200010605     c                   eval      f_rorinc = f_rorinc +1
148300010620 e3  c                   endif
148400010605     ?* fissi ?
148500010620 b3  c                   if        ormtco = 'S'
148600010605     c                   eval      f_rorifi = f_rorifi +1
148700010620 e3  c                   endif
148800010605     ?* da centralino ?
148900010620 b3  c                   if        ormtco = 'M'  or
149000010606     c                             ormtco = 'P'
149100010605     c                   eval      f_rorice = f_rorice +1
149200010620 e3  c                   endif
149300010605     ?* da internet ?
149400010620 b3  c                   if        ormtco = 'I'
149500010605     c                   eval      f_roriin = f_roriin +1
149600010620 e3  c                   endif
149700010605     ?* da vas ?
149800010620 b3  c                   if        ormtco = 'F'
149900010605     c                   eval      f_roriva = f_roriva +1
150000010620 e3  c                   endif
150100010605     ?* da call center ?
150200010620 b3  c                   if        ormtco = 'C'
150300010605     c                   eval      f_roricc = f_roricc +1
150400010620 e3  c                   endif
150500010605
150600010612     c                   eval      rfile(xf) = dswfror
150700010607     c                   eval      wokstp = *on
150800031202      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
150900031202     c                   Eval      sxf = xf
151000010620 e2  c                   endif
151100010605
151200010620 e1  c                   endif
151300010620
151400010605     c                   endsr
151500010614
151600010620      *--------------------------------------------------------------*
151700010620      * CERCO IL PRIMO GIORNO FESTIVO PRECEDENTE
151800010620      *--------------------------------------------------------------*
151900010620     c     SR_DTFES      begsr
152000010620
152100010620      * calcolo il primo giorno festivo
152200010620     c     *iso          movel     dataela       wdtda
152300010620     c                   do        *hival
152400010620     c                   subdur    1:*d          wdtda
152500010620     c                   extrct    wdtda:*y      kann
152600010620     c                   extrct    wdtda:*m      kmes
152700010620     c                   extrct    wdtda:*d      wgiorno
152800011217     c                   clear                   ktfa
152900011217     c                   eval      ktfp = dspoel
153000010620     c     kazcln        chain     azcln01l
153100010620     c                   if        not%found(azcln01l)
153200010620     c                   leave
153300010620     c                   endif
153400010620     c                   if        MAT(wgiorno) =  'F'
153500010620     c                             or  POM(wgiorno) =  'F'
153600010620     c                   move      wdtda         wdatafes
153700010620     c                   iter
153800010620     c                   endif
153900010620     c                   leave
154000010620     c                   enddo
154100010620
154200010620     c                   endsr
154300010620
154400010608      *--------------------------------------------------------------*
154500010608      * PULISCO CAMPI DI COMODO
154600010608      *--------------------------------------------------------------*
154700010614     c     SR_PULISCI    begsr
154800010608
154900010608     c                   clear                   wrorode
155000010620     c                   clear                   wrordan
155100010608     c                   clear                   wrordit
155200010608     c                   clear                   wrordr1
155300010608     c                   clear                   wrordr2
155400010608     c                   clear                   wrordro
155500010608     c                   clear                   wroroes
155600010614     c                   clear                   wrorean
155700010608     c                   clear                   wroreit
155800010608     c                   clear                   wrorer1
155900010608     c                   clear                   wrorer2
156000010608     c                   clear                   wrorero
156100010608     c                   clear                   wrorone
156200010620     c                   clear                   wrornan
156300010608     c                   clear                   wrornit
156400010608     c                   clear                   wrornr1
156500010608     c                   clear                   wrornr2
156600010608     c                   clear                   wrornro
156700010608     c                   clear                   wrorn01
156800010608     c                   clear                   wrorn02
156900010608     c                   clear                   wrorn03
157000010608     c                   clear                   wrorn04
157100010608     c                   clear                   wrorn05
157200010608     c                   clear                   wrorn06
157300010608     c                   clear                   wrorn07
157400010608     c                   clear                   wrorn08
157500010608     c                   clear                   wrorn09
157600010608     c                   clear                   wrorn10
157700010608     c                   clear                   wroroea
157800010620     c                   clear                   wroraan
157900010608     c                   clear                   wrorait
158000010608     c                   clear                   wrorar1
158100010608     c                   clear                   wrorar2
158200010608     c                   clear                   wroraro
158300010608     c                   clear                   wrora01
158400010608     c                   clear                   wrora02
158500010608     c                   clear                   wrora03
158600010608     c                   clear                   wrora04
158700010608     c                   clear                   wrora05
158800010608     c                   clear                   wrora06
158900010608     c                   clear                   wrora07
159000010608     c                   clear                   wrora08
159100010608     c                   clear                   wrora09
159200010608     c                   clear                   wrora10
159300010608     c                   clear                   wrora11
159400010608     c                   clear                   wrora12
159500010608     c                   clear                   wrora13
159600010608     c                   clear                   wrora14
159700010608     c                   clear                   wrora15
159800010608     c                   clear                   wrornqd
159900010608     c                   clear                   wrornbl
160000010614     c                   clear                   wrormdr
160100010614     c                   clear                   wrornas
160200030912     c                   clear                   wroroned
160300030912     c                   clear                   wrorn01d
160400030912     c                   clear                   wrorn02d
160500030912     c                   clear                   wrorn03d
160600030912     c                   clear                   wrorn04d
160700030912     c                   clear                   wrorn05d
160800030912     c                   clear                   wrorn06d
160900030912     c                   clear                   wrorn07d
161000030912     c                   clear                   wrorn08d
161100030912     c                   clear                   wrorn09d
161200030912     c                   clear                   wrorn10d
161300030917     c                   clear                   wrorifian
161400030917     c                   clear                   wrorifine
161500030917     c                   clear                   wrorifid
161600030917     c                   clear                   wroricean
161700030917     c                   clear                   wroricene
161800030917     c                   clear                   wroriced
161900030917     c                   clear                   wroriinan
162000030917     c                   clear                   wroriinne
162100030917     c                   clear                   wroriind
162200030917     c                   clear                   wrorivaan
162300030917     c                   clear                   wrorivane
162400030917     c                   clear                   wrorivad
162500030917     c                   clear                   wroriccan
162600030917     c                   clear                   wroriccne
162700030917     c                   clear                   wroriccd
162800010620
162900010621     c                   clear                   w_rorone
163000010621     c                   clear                   w_rornqd
163100010621     c                   clear                   w_rornbl
163200010621     c                   clear                   w_rornas
163300010621     c                   clear                   w_rormdr
163400010608
163500010608     c                   endsr
163600010614
163700010605      *--------------------------------------------------------------*
163800010605      * CONTROLLO CON IL PO RITIRO
163900010605      *--------------------------------------------------------------*
164000010605     c     SR_POR        begsr
164100010620
164200010620     c                   clear                   wdatafes
164300010620
164400010620      * se la data del p.o. ritiro non è festa controllare se il giorno prima
164500010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
164600010620 b1  c                   if        wokfestar = *off
164700010620     c                   eval      dspoel = dspoelr
164800010620      *                            ========
164900010620     c                   exsr      SR_DTFES
165000010620      *                            ========
165100010620 e1  c                   endif
165200010608
165300010608     c                   eval      kormpoe = ormpoe
165400010608     c                   eval      kormnsr = ormnsr
165500010608     c                   eval      kormnor = ormnor
165600010608     c                   eval      kormnrv = ormnrv
165700010612
165800010614      * se data p.o. ritiro non è festa cerco gli ORM da eseguire
165900010619      * e le modifiche della data di ritiro
166000010613 b1  c                   if        wokfestar = *off
166100010606
166200010614     ?* ORM da eseguire oggi data ritiro = data elaborazione
166300010618 b2  c                   if        ormdar = dataela
166400010618     ?* data ultima fase < data elaborazione e già eseguito non devo elaborarlo
166500010618     ?*                                                     -------------------
166600010618     c                   if        ormdfo < dataela and ormfao >= 600
166700010618     c                   goto      endpor
166800010618     c                   endif
166900010618     c                   eval      wrorode = wrorode +1
167000010607     c                   eval      wrordit = wrordit +1
167100010607     c                   eval      wokstp = *on
167200010612     c                   eval      wokrfile = *on
167300010614 e2  c                   endif
167400010608
167500010614     ?* ORM da eseguire (non eseguiti entro oggi) data ritiro < data elaborazione
167600010613 b2  c                   if        ormdar < dataela
167700010608     c                   eval      wokrode = *off
167800010620
167900010620     c                   eval      kormdfo = ormdfo
168000010620     c                   eval      kormofo = ormofo
168100010620     c                   eval      kormfao = ormfao
168200010620     c     korf01        setll     fnorf01l
168300010620      * se la data ultima fase "testata" è >= della data elaborazione
168400010620      * o è <  della data elaborazione
168500010620      *   e >= alla data festiva (se sto elaborando un lunedì)
168600010620      * devo leggere la fase prima
168700010620     c                   select
168800010620 b3  c                   when      ormdfo >= dataela or
168900010620 b3  c                             (ormdfo < dataela  and
169000010620     c                             (wdatafes > 0 and ormdfo >= wdatafes))
169100010620     c     korf          readpe    fnorf01l
169200010620      * se la data ultima fase "testata" è <  della data elaborazione
169300010620      * e non sto elaborando un lunedì
169400010620      * devo leggere la fase stessa
169500010620     c                   other
169600010620     c     korf          reade     fnorf01l
169700010620 e3  c                   endsl
169800010620
169900030807      * senza fase 600-900-910-999 fino alla data elaborazione esclusa
170000010614 b3  c                   dow       not %eof(fnorf01l)
170100010620 b4  c                   if        orfdae < dataela
170200010620 b5  c                   if        orffar >=600
170300011019      * Attenzione!! E' possibile che sia stata fatta una modifica fase
170400011019      * se la fase della testata ORM è 500 o è 100 e la fase precedente
170500011019      * sul file delle fasi è una fase di fatto DEVO dare l'ORM come
170600011019      * da eseguire
170700011019     c                   if        ormfao <> 500 and ormfao <> 100
170800010608     c                   leave
170900011019     c                   endif
171000010614 e5  c                   endif
171100060926      * salto le fasi che non devono essere elaborate
171200060926     c     orffar        Lookup    skfar                                  30
171300060926     c                   if        not *in30
171400010608     c                   eval      wokrode = *on
171500031105     c                   EndIf
171600010614 e4  c                   endif
171700010613     c     korf          readpe    fnorf01l
171800010614 e3  c                   enddo
171900010613     ?* da eseguire (in ritardo)
172000010613 b3  c                   if        wokrode = *on
172100010608     c                   eval      wrorode = wrorode +1
172200010608      * calcolo affidabilità. Uso >= per i da eseguire nei festivi
172300010614 b4  c                   select
172400010608     c                   when      ormdar >= wdtda01
172500010608     c                   eval      wrordr1 = wrordr1 +1
172600010608     c                   when      ormdar >= wdtda02
172700010608     c                   eval      wrordr2 = wrordr2 +1
172800010614 x4  c                   other
172900010608     c                   eval      wrordro = wrordro +1
173000010614 e4  c                   endsl
173100010608     c                   eval      wokstp = *on
173200010614     c                   eval      wokrfile = *on
173300010614     ?* è stato eseguito
173400010613 x3  c                   else
173500010614      * e la data ultima fase minore data elaborazione non devo elaborarlo
173600010614      *                                                -------------------
173700010620 b4  c                   if        ormdfo < dataela
173800010619     ?* ma prima controllo se deve andare a far parte dei non bollettati
173900010619     ?*                                                   --------------
174000010619 b5  c                   if        ormfao = 600 and
174100010619     c                             ormdfo >= wdtda01
174200010619     c                   eval      wrornbl = wrornbl +1
174300010621     c   11              eval      w_rornbl = 'X'
174400010619     c                   eval      wokstp = *on
174500010619     c                   eval      wokrfile = *on
174600010619 e5  c                   endif
174700010608     c                   goto      endpor
174800010614 e4  c                   endif
174900010613 e3  c                   endif
175000010613 e2  c                   endif
175100040420
175200040420     ?* ORM con data ritiro > data elaborazione
175300040420      * devo controllare che non sia stata fatta una fase di dirottamento dalla data elaborazione
175400040420      * in poi
175500040420if  2c                   If        OrmDar > Dataela
175600040420     c                   Eval      kormfao = 200
175700040420     c                   Eval      kormdfo = Dataela
175800040420     c     kOrf03        Setll     Fnorf03l
175900040420      * Se trovo già una fase di dirottamento in data elaborazione non devo elabore l'ORM
176000040420      *                                                            ----------------------
176100040420     c                   If        %Equal(Fnorf03l)
176200040420     c                   Goto      endpor
176300040420     c                   EndIf
176400040615      * Altrimenti leggo dalla data statistica in poi
176500040420do  3c                   Do        *Hival
176600040420     c     kOrf04        Reade     Fnorf03l
176700040420     c                   If        %Eof(Fnorf03l)
176800040420     c                   Leave
176900040420     c                   EndIf
177000040420      * Se trovo una fase di dirottamento non devo elabore l'ORM
177100040420      *                                   ----------------------
177200040420     c                   Goto      endpor
177300040420e   3c                   EndDo
177400040420e   2c                   EndIf
177500010618
177600010619      * cerco le modifiche alla data ritiro
177700010618      *                            =========
177800010618     c                   exsr      SR_MODRIT
177900010618      *                            =========
178000010619
178100010619 e1  c                   endif
178200010608
178300010614      * cerco sulle fasi sempre
178400010614      *                  ------
178500010614      *                            =======
178600010606     c                   exsr      SR_FASI
178700010614      *                            =======
178800010608
178900010608     c     endpor        endsr
179000010618
179100010618      *--------------------------------------------------------------*
179200010618      * CALCOLO MODIFICHE DATA RITIRO
179300010618      *--------------------------------------------------------------*
179400010618     c     SR_MODRIT     begsr
179500010618
179600010618     c                   eval      korvdtv = dataela
179700010620     c                   if        wdatafes > 0
179800010620     c                   eval      korvdtv = wdatafes
179900010620     c                   endif
180000010618     c                   clear                   sav_orvdar
180100010618
180200010620     c     korv          setll     fnorv01l
180300010618     c                   do        *hival
180400010620     c     korf          reade     fnorv01l
180500010618     c                   if        %eof(fnorv01l)
180600010618     c                   leave
180700010618     c                   endif
180800010620     c                   if        orvdtv > dataela
180900010620     c                   leave
181000010620     c                   endif
181100010620     c                   if        orvdar <> ormdar and
181200010620     c                             orvdar <> sav_orvdar
181300010618     c                   eval      wrormdr = wrormdr +1
181400010621     c   11              eval      w_rormdr = 'X'
181500010618     c                   eval      sav_orvdar = orvdar
181600010618     c                   eval      wokstp = *on
181700010618     c                   eval      wokrfile = *on
181800010618     c                   endif
181900010618     c                   enddo
182000010618
182100010618     c                   endsr
182200010618
182300010608      *--------------------------------------------------------------*
182400010608      * CONTROLLO LA SITUAZIONE DELL'ORM SULLE FASI
182500010608      *--------------------------------------------------------------*
182600010608     c     SR_FASI       begsr
182700010608
182800040421      * ATTENZIONE!!! la fase 200 di dirottato fa fermare la lettura delle fasi, appena
182900040421      *               la trovo esco dalla routine
183000040421
183100010608     c                   clear                   orfnor
183200010608     c                   clear                   orfpoe
183300010608     c                   clear                   orfpog
183400010608     c                   clear                   orfore
183500010608
183600010608     ?* se la data elaborazione è >= data ultima fase "testata"
183700010608     ?* mi posiziono sul file delle fasi con la data/ora/fase della
183800010608     ?* testata ORM
183900010614 b1  c                   if        dataela >= ormdfo
184000010608     c                   eval      kormdfo = ormdfo
184100010608     c                   eval      kormofo = ormofo
184200010608     c                   eval      kormfao = ormfao
184300010608     c     korf01        setll     fnorf01l
184400010608     c     korf01        reade     fnorf01l
184500010614 e1  c                   endif
184600010608     ?* se la data elaborazione è < data ultima fase  "testata"
184700010608     ?* mi posizione sul file delle fasi con la data elaborazione
184800030917     ?* fase = 0 e ora fase = a tutti '9' per leggere l'ultima fase fatta nel giorno
184900030917     ?* di elaborazione statistica
185000010614 b1  c                   if        dataela < ormdfo
185100010608     c                   eval      kormdfo = dataela
185200010608     c                   eval      kormofo = *all'9'
185300010608     c                   clear                   kormfao
185400010608     c     korf01        setgt     fnorf01l
185500010608     c     korf          readpe    fnorf01l
185600010705      * se la data di ritiro = alla data elaborazione e trovo subito il fine file
185700010705      * sulle fasi vuole dire che la prima fase è stata eseguita dopo la data elaborazione
185800010705      * quindi l'orm è non eseguito (es. orm copiati con data immissione e data ritiro = al
185900010705      * vecchio orm, ma la data della fase = alla data del giorno della copia, errore già
186000010705      * sistemato in gestione orm)
186100010705 b2  c                   if        %eof(fnorf01l)
186200010705     c                             and ormdar = dataela
186300010705     c                   eval      wrorone = wrorone +1
186400010717     c   11              eval      w_rorone = 'X'
186500010705     c                   eval      wrornit = wrornit +1
186600010705     c                   eval      wokstp = *on
186700010705     c                   eval      wokrfile = *on
186800010705 e2  c                   endif
186900010614 e1  c                   endif
187000030912      * _____________
187100010608      * leggo le fasi
187200030912      * -------------
187300010613 b1  c                   dow       not %eof(fnorf01l)
187400040421      * ____________________
187500040421      * Fase 200 devo uscire
187600040421      * --------------------
187700040618if  2c                   If        OrfFar = 200
187800040421     c                   Leave
187900040421e   2c                   EndIf
188000040421
188100010608     c                   eval      wok900 = *off
188200010615     c                   eval      wok600 = *off
188300030912      * _____________________________
188400010608      * data fase = data elaborazione
188500030912      * -----------------------------
188600010620 b2  c                   if        orfdae = dataela or
188700010620     c                             (wdatafes > 0 and orfdae >= wdatafes and
188800010620     c                              orfdae < dataela)
188900010614     ?* ESEGUITI
189000010614     ?* ORM eseguiti (fase 900) ?
189100010614 b3  c                   if        orffar = 900
189200010614      * devo controllare se c'è una fase 600 prima
189300030917      * il controllo è così:
189400030917      *     - se data fase < data elaborazione e fase >= 600 è stato fatto prima
189500030917      *       quindi lo devo saltare
189600030917      *     - se no vuol dire che è stato fatto in data elaborazione e lo do per fatto
189700040421      * qui fa solo UNA readpe quindi non faccio il controllo della fase 200
189800010614     c                   eval      wokroes = *off
189900010614     c     korf          readpe    fnorf01l
190000010615 b5  c                   if        orffar >= 600  and
190100010615     c                             orfdae < dataela
190200010615     c                   eval      wok600 = *on
190300030917 x5  c                   else
190400010614     c                   eval      wokroes = *on
190500030917 e5  c                   endif
190600010614
190700010614 b4  c                   if        wokroes = *on
190800010614     c                   eval      wok900 = *on
190900010608     c                   eval      wroroes = wroroes +1
191000010608      * calcolo affidabilità. Uso >= per i ritiri nei festivi
191100010614 b5  c                   select
191200010614     c                   when      ormdar > dataela
191300010614     c                   eval      wrorean = wrorean +1
191400010614     c                   when      ormdar = dataela
191500010608     c                   eval      wroreit = wroreit +1
191600010608     c                   when      ormdar >= wdtda01
191700010608     c                   eval      wrorer1 = wrorer1 +1
191800010608     c                   when      ormdar >= wdtda02
191900010608     c                   eval      wrorer2 = wrorer2 +1
192000010614 x5  c                   other
192100010608     c                   eval      wrorero = wrorero +1
192200010614 e5  c                   endsl
192300010608     c                   eval      wokstp = *on
192400010614     c                   eval      wokrfile = *on
192500010608     c                   leave
192600010614 x4  c                   else
192700010615 b5  c                   if        wok600 = *on
192800010615     c                   leave
192900010615 e5  c                   endif
193000010613 e4  c                   endif
193100010613 e3  c                   endif
193200010614
193300010614     ?* ORM eseguiti (fase 600) ?
193400010614 b3  c                   if        orffar = 600
193500010614     c                   eval      wroroes = wroroes +1
193600010614      * calcolo affidabilità. Uso >= per i ritiri nei festivi
193700010614 b4  c                   select
193800010614     c                   when      ormdar > dataela
193900010614     c                   eval      wrorean = wrorean +1
194000010614     c                   when      ormdar = dataela
194100010614     c                   eval      wroreit = wroreit +1
194200010614     c                   when      ormdar >= wdtda01
194300010614     c                   eval      wrorer1 = wrorer1 +1
194400010614     c                   when      ormdar >= wdtda02
194500010614     c                   eval      wrorer2 = wrorer2 +1
194600010614 x4  c                   other
194700010614     c                   eval      wrorero = wrorero +1
194800010614 e4  c                   endsl
194900010615     ?* ORM non bollettati
195000010615     c                   if        orfdae >= wdtda01 and
195100010618     c                             orfdae < dataela  and
195200010615     c                             ormdar < dataela
195300010615     c                   eval      wrornbl = wrornbl +1
195400010621     c   11              eval      w_rornbl = 'X'
195500010615     c                   endif
195600010614     c                   eval      wokstp = *on
195700010614     c                   eval      wokrfile = *on
195800010614     c                   leave
195900010614 e3  c                   endif
196000010612
196100010614      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
196200010613 b3  c                   if        wokfestar = *off
196300060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
196400010705      * in questo caso devo tornare a leggere la precedente
196500060926     c     orffar        Lookup    skfar                                  30
196600060926     c                   if        *in30
196700010705     c                   goto      passa
196800010705 e4  c                   endif
196900040421     ?* ORM chiusi/eseguiti anomali (fase 910 - 999)
197000031105 b4  c                   if        orffar = 999 or OrfFar = 910
197100030917     c                   eval      wroroea = wroroea +1
197200030917      * calcolo affidabilità. Uso >= per gli anomali nei festivi
197300030917 b5  c                   select
197400030917     c                   when      ormdar > dataela
197500030917     c                   eval      wroraan = wroraan +1
197600030917     c                   when      ormdar = dataela
197700030917     c                   eval      wrorait = wrorait +1
197800030917     c                   when      ormdar >= wdtda01
197900030917     c                   eval      wrorar1 = wrorar1 +1
198000030917     c                   when      ormdar >= wdtda02
198100030917     c                   eval      wrorar2 = wrorar2 +1
198200030917 x5  c                   other
198300030917     c                   eval      wroraro = wroraro +1
198400030917 e5  c                   endsl
198500030917      * memorizzo il tipo comunicazione ORM
198600030917     ?* fissi ?
198700030917 b5  c                   if        ormtco = 'S'
198800030917     c                   eval      wrorifian = wrorifian +1
198900030917 e5  c                   endif
199000030917     ?* da centralino ?
199100030917 b5  c                   if        ormtco = 'M'  or
199200030917     c                             ormtco = 'P'
199300030917     c                   eval      wroricean = wroricean +1
199400030917 e5  c                   endif
199500030917     ?* da internet ?
199600030917 b5  c                   if        ormtco = 'I'
199700030917     c                   eval      wroriinan = wroriinan +1
199800030917 e5  c                   endif
199900030917     ?* da vas ?
200000030917 b5  c                   if        ormtco = 'F'
200100030917     c                   eval      wrorivaan = wrorivaan +1
200200030917 e5  c                   endif
200300030917     ?* da call center ?
200400030917 b5  c                   if        ormtco = 'C'
200500030917     c                   eval      wroriccan = wroriccan +1
200600030917 e5  c                   endif
200700030917     c                   eval      cauoea = orfcar
200800030917     c                   if        orfcar = '80'
200900030917     c                   eval      wrora01 = wrora01 +1
201000030917     c                   endif
201100030917     c                   if        orfcar = '81'
201200030917     c                   eval      wrora02 = wrora02 +1
201300030917     c                   endif
201400030917     c                   if        orfcar = '90'
201500030917     c                   eval      wrora03 = wrora03 +1
201600030917     c                   endif
201700030917     c                   if        orfcar = '91'
201800030917     c                   eval      wrora04 = wrora04 +1
201900030917     c                   endif
202000030917     c                   if        orfcar = '92'
202100030917     c                   eval      wrora05 = wrora05 +1
202200030917     c                   endif
202300030917     c                   if        orfcar = '93'
202400030917     c                   eval      wrora06 = wrora06 +1
202500030917     c                   endif
202600030917     c                   if        orfcar = '94'
202700030917     c                   eval      wrora07 = wrora07 +1
202800030917     c                   endif
202900030917     c                   if        orfcar = '95'
203000030917     c                   eval      wrora08 = wrora08 +1
203100030917     c                   endif
203200031105     c                   if        orfcar = '82'
203300031105     c                   eval      wrora09 = wrora09 +1
203400031105     c                   endif
203500040701     c                   if        orfcar = '96'
203600040701     c                   eval      wrora10 = wrora10 +1
203700040701     c                   endif
203800040701     c                   if        orfcar = '97'
203900040701     c                   eval      wrora11 = wrora11 +1
204000040701     c                   endif
204100030917     c                   eval      wokstp = *on
204200030917     c                   eval      wokrfile = *on
204300030917     c                   leave
204400030917 e4  c                   endif
204500010608     ?* ORM non eseguiti
204600030916 b4  c                   if        orffar < 600
204700030916     ?* con data ritiro <= data elaborazione
204800030916 b4a c                   If        ormdar <= dataela
204900010608     c                   eval      wrorone = wrorone +1
205000010621     c   11              eval      w_rorone = 'X'
205100030917     c                   eval      wokstp = *on
205200030917     c                   eval      wokrfile = *on
205300010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
205400010614 b5  c                   select
205500010620     c                   when      ormdar > dataela
205600010620     c                   eval      wrornan = wrornan +1
205700010608     c                   when      ormdar = dataela
205800010608     c                   eval      wrornit = wrornit +1
205900010608     c                   when      ormdar >= wdtda01
206000010608     c                   eval      wrornr1 = wrornr1 +1
206100010608     c                   when      ormdar >= wdtda02
206200010608     c                   eval      wrornr2 = wrornr2 +1
206300010614 x5  c                   other
206400010608     c                   eval      wrornro = wrornro +1
206500010614 e5  c                   endsl
206600030912      * memorizzo il tipo comunicazione ORM
206700030912     ?* fissi ?
206800030912 b5  c                   if        ormtco = 'S'
206900030917     c                   eval      wrorifine = wrorifine +1
207000030912 e5  c                   endif
207100030912     ?* da centralino ?
207200030912 b5  c                   if        ormtco = 'M'  or
207300030912     c                             ormtco = 'P'
207400030917     c                   eval      wroricene = wroricene +1
207500030912 e5  c                   endif
207600030912     ?* da internet ?
207700030912 b5  c                   if        ormtco = 'I'
207800030917     c                   eval      wroriinne = wroriinne +1
207900030912 e5  c                   endif
208000030912     ?* da vas ?
208100030912 b5  c                   if        ormtco = 'F'
208200030917     c                   eval      wrorivane = wrorivane +1
208300030912 e5  c                   endif
208400030912     ?* da call center ?
208500030912 b5  c                   if        ormtco = 'C'
208600030917     c                   eval      wroriccne = wroriccne +1
208700030912 e5  c                   endif
208800010608     ?* ORM non eseguiti con causale ?
208900010613 b5  c                   if        orffar = 500
209000010619     c                   eval      cauone = orfcar
209100010608     c                   if        orfcar = '01'
209200010608     c                   eval      wrorn01 = wrorn01 +1
209300010608     c                   endif
209400010608     c                   if        orfcar = '02'
209500010608     c                   eval      wrorn02 = wrorn02 +1
209600010608     c                   endif
209700010608     c                   if        orfcar = '03'
209800010608     c                   eval      wrorn03 = wrorn03 +1
209900010608     c                   endif
210000020527     c                   if        orfcar = '04'
210100020527     c                   eval      wrorn04 = wrorn04 +1
210200020527     c                   endif
210300030917     c                   leave
210400010613 e5  c                   endif
210500030917     ?* ORM non eseguiti no in fase 500 ?
210600030917     ?* ORM non assegnati
210700030917 b5  c                   if        orffar < 400
210800030917     c                   eval      wrornas = wrornas +1
210900030917     c   11              eval      w_rornas = 'X'
211000030917 e5  c                   endif
211100030912      * devo controllare se nel giorno è stata fatta una fase 500
211200030912     c     kOrf02        Readpe    Fnorf01l
211300030917 b5  c                   Dow       Not %Eof(Fnorf01l)
211400040421      * se trovo fase 200 esco
211500040618if  6c                   If        OrfFar = 200
211600040421     c                   Leave
211700040421e   6c                   EndIf
211800030917 b6  c                   If        OrfFar = 500
211900030912      * se trovo la fase 500 memorizzo la causale
212000030912     c                   Eval      %Subst(cauone:2:2) = OrfCar
212100030919     c                   Move      ')'           cauone
212200030919     c                   Movel     '('           cauone
212300030912     c                   If        OrfCar = '01'
212400030912     c                   Eval      wrorn01 = wrorn01 +1
212500030912     c                   Endif
212600030912     c                   If        OrfCar = '02'
212700030912     c                   Eval      wrorn02 = wrorn02 +1
212800030912     c                   Endif
212900030912     c                   If        OrfCar = '03'
213000030912     c                   Eval      wrorn03 = wrorn03 +1
213100030912     c                   Endif
213200030912     c                   If        OrfCar = '04'
213300030912     c                   Eval      wrorn04 = wrorn04 +1
213400030912     c                   Endif
213500030912     c                   Leave
213600030917 e6  c                   Endif
213700030912     c     kOrf02        Readpe    Fnorf01l
213800030917 e5  c                   EndDo
213900030917     c                   Leave
214000030916 e4a c                   endif
214100030916     ?* con data ritiro > data elaborazione
214200030919 b4a c                   If        ormdar > dataela
214300030929     ?* è un ORM non fatto se nel giorno c'è una fase 500 o 390
214400031105     ?* quindi lo metto nella colonna dei non eseguiti e anche in quella dei da eseguire
214500030929 b5  c                   If        OrfFar = 500 or OrfFar = 390
214600031105     c                   eval      wrorone = wrorone +1
214700031105     c                   eval      wroroned = wroroned +1
214800031105     c   11              eval      w_rorone = 'X'
214900030917     c                   eval      wokstp = *on
215000030917     c                   eval      wokrfile = *on
215100031105      * da eseguire
215200031105     c                   eval      wrorode = wrorode +1
215300031105      * sempre in anticipo xchè data ritiro > data elaborazione
215400031105     c                   eval      wrordan = wrordan +1
215500031106     c                   eval      wrornan = wrornan +1
215600030912      * memorizzo il tipo comunicazione ORM
215700030912     ?* fissi ?
215800030917 b6  c                   if        ormtco = 'S'
215900031105     c                   eval      wrorifine = wrorifine +1
216000031105     c                   eval      wrorifid = wrorifid +1
216100030917 e6  c                   endif
216200030912     ?* da centralino ?
216300030917 b6  c                   if        ormtco = 'M'  or
216400030912     c                             ormtco = 'P'
216500031105     c                   eval      wroricene = wroricene +1
216600031105     c                   eval      wroriced = wroriced +1
216700030917 e6  c                   endif
216800030912     ?* da internet ?
216900030917 b6  c                   if        ormtco = 'I'
217000031105     c                   eval      wroriinne = wroriinne +1
217100031105     c                   eval      wroriind = wroriind +1
217200030917 e6  c                   endif
217300030912     ?* da vas ?
217400030917 b6  c                   if        ormtco = 'F'
217500031105     c                   eval      wrorivane = wrorivane +1
217600031105     c                   eval      wrorivad = wrorivad +1
217700030917 e6  c                   endif
217800030912     ?* da call center ?
217900030917 b6  c                   if        ormtco = 'C'
218000031105     c                   eval      wroriccne = wroriccne +1
218100031105     c                   eval      wroriccd = wroriccd +1
218200030917 e6  c                   endif
218300030912     ?* ORM non eseguiti con causale ?
218400030929 b6  c                   If        OrfFar = 500
218500031105     c                   eval      cauone = orfcar
218600030912     c                   if        orfcar = '01'
218700031105     c                   eval      wrorn01 = wrorn01 +1
218800031105     c                   eval      wrorn01d = wrorn01d +1
218900030912     c                   endif
219000030912     c                   if        orfcar = '02'
219100031105     c                   eval      wrorn02 = wrorn02 +1
219200031105     c                   eval      wrorn02d = wrorn02d +1
219300030912     c                   endif
219400030912     c                   if        orfcar = '03'
219500031105     c                   eval      wrorn03 = wrorn03 +1
219600031105     c                   eval      wrorn03d = wrorn03d +1
219700030912     c                   endif
219800030912     c                   if        orfcar = '04'
219900031105     c                   eval      wrorn04 = wrorn04 +1
220000031105     c                   eval      wrorn04d = wrorn04d +1
220100030912     c                   endif
220200030929 e6  c                   endif
220300030929     c                   Leave
220400030917 e5  c                   endif
220500030929     ?* ORM no in fase 500 o 390 ?
220600030929      * devo controllare se nel giorno è stata fatta una fase 500 o 390
220700030912     c     kOrf02        Readpe    Fnorf01l
220800030917 b5  c                   Dow       Not %Eof(Fnorf01l)
220900040421      * se trovo fase 200 esco
221000040618if  6c                   If        OrfFar = 200
221100040421     c                   Leave
221200040421e   6c                   EndIf
221300030929 b6  c                   If        OrfFar = 500 or OrfFar = 390
221400031105     c                   eval      wrorone = wrorone +1
221500031105     c                   eval      wroroned = wroroned +1
221600031105     c   11              eval      w_rorone = 'X'
221700030929     c                   eval      wokstp = *on
221800030929     c                   eval      wokrfile = *on
221900031106      * da eseguire
222000031106     c                   eval      wrorode = wrorode +1
222100031106      * sempre in anticipo xchè data ritiro > data elaborazione
222200031106     c                   eval      wrordan = wrordan +1
222300031106     c                   eval      wrornan = wrornan +1
222400030929      * memorizzo il tipo comunicazione ORM
222500030929     ?* fissi ?
222600030929 b7  c                   if        ormtco = 'S'
222700031105     c                   eval      wrorifine = wrorifine +1
222800031105     c                   eval      wrorifid = wrorifid +1
222900030929 e7  c                   endif
223000030929     ?* da centralino ?
223100030929 b7  c                   if        ormtco = 'M'  or
223200030929     c                             ormtco = 'P'
223300031105     c                   eval      wroricene = wroricene +1
223400031105     c                   eval      wroriced = wroriced +1
223500030929 e7  c                   endif
223600030929     ?* da internet ?
223700030929 b7  c                   if        ormtco = 'I'
223800031105     c                   eval      wroriinne = wroriinne +1
223900031105     c                   eval      wroriind = wroriind +1
224000030929 e7  c                   endif
224100030929     ?* da vas ?
224200030929 b7  c                   if        ormtco = 'F'
224300031105     c                   eval      wrorivane = wrorivane +1
224400031105     c                   eval      wrorivad = wrorivad +1
224500030929 e7  c                   endif
224600030929     ?* da call center ?
224700030929 b7  c                   if        ormtco = 'C'
224800031105     c                   eval      wroriccne = wroriccne +1
224900031105     c                   eval      wroriccd = wroriccd +1
225000030929 e7  c                   endif
225100030912      * se trovo la fase 500 memorizzo la causale
225200030929 b7  c                   If        OrfFar = 500
225300031105     c                   Eval      %Subst(cauone:2:2) = OrfCar
225400031105     c                   Move      ')'           cauone
225500031105     c                   Movel     '('           cauone
225600030912     c                   If        OrfCar = '01'
225700031105     c                   Eval      wrorn01 = wrorn01 +1
225800031105     c                   Eval      wrorn01d = wrorn01d +1
225900030912     c                   Endif
226000030912     c                   If        OrfCar = '02'
226100031105     c                   Eval      wrorn02 = wrorn02 +1
226200031105     c                   Eval      wrorn02d = wrorn02d +1
226300030912     c                   Endif
226400030912     c                   If        OrfCar = '03'
226500031105     c                   Eval      wrorn03 = wrorn03 +1
226600031105     c                   Eval      wrorn03d = wrorn03d +1
226700030912     c                   Endif
226800030912     c                   If        OrfCar = '04'
226900031105     c                   Eval      wrorn04 = wrorn04 +1
227000031105     c                   Eval      wrorn04d = wrorn04d +1
227100030912     c                   Endif
227200030929 e7  c                   Endif
227300030912     c                   Leave
227400030917 e6  c                   Endif
227500030912     c     kOrf02        Readpe    Fnorf01l
227600030917 e5  c                   EndDo
227700030917     c                   Leave
227800030916 e4a c                   endif
227900010613 e4  c                   endif
228000010608
228100010613 e3  c                   endif
228200010612
228300010613 e2  c                   endif
228400010608
228500030912      * _____________________________
228600010608      * data fase < data elaborazione
228700030912      * -----------------------------
228800010620 b2  c                   if        orfdae < dataela
228900010705
229000010614      * se ORM eseguito (fase 900) esco dal giro
229100010614     c                   if        orffar >= 900
229200010614     c                   leave
229300010614     c                   endif
229400010612      * se data ritiro maggiore data elaborazione esco dal giro
229500010612     c                   if        ormdar > dataela
229600010612     c                   leave
229700010612     c                   endif
229800010619      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
229900010619      * e i non bollettati
230000010619 b3  c                   if        wokfestar = *off
230100010614     ?* ORM non bollettati
230200010615     c                   if        orffar = 600 and
230300010615     c                             orfdae >= wdtda01
230400010614     c                   eval      wrornbl = wrornbl +1
230500010621     c   11              eval      w_rornbl = 'X'
230600010614     c                   eval      wokstp = *on
230700010614     c                   eval      wokrfile = *on
230800010614     c                   leave
230900010614     c                   endif
231000010615     ?* ORM già eseguiti che non devo elaborare
231100010615     c                   if        orffar = 600
231200010615     c                   leave
231300010615     c                   endif
231400060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
231500010705      * in questo caso devo tornare a leggere la precedente
231600060926     c     orffar        Lookup    skfar                                  30
231700060926     c                   if        *in30
231800010705     c                   goto      passa
231900010705 e4  c                   endif
232000010608     ?* ORM non eseguiti
232100010619 b4  c                   if        orffar < 600
232200010608     c                   eval      wrorone = wrorone +1
232300010621     c   11              eval      w_rorone = 'X'
232400030917     c                   eval      wokstp = *on
232500030917     c                   eval      wokrfile = *on
232600010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
232700010619 b5  c                   select
232800010608     c                   when      ormdar = dataela
232900010608     c                   eval      wrornit = wrornit +1
233000010608     c                   when      ormdar >= wdtda01
233100010608     c                   eval      wrornr1 = wrornr1 +1
233200010608     c                   when      ormdar >= wdtda02
233300010608     c                   eval      wrornr2 = wrornr2 +1
233400010619 x5  c                   other
233500010608     c                   eval      wrornro = wrornro +1
233600010619 e5  c                   endsl
233700030917      * memorizzo il tipo comunicazione ORM
233800030917     ?* fissi ?
233900030917 b5  c                   if        ormtco = 'S'
234000030917     c                   eval      wrorifine = wrorifine +1
234100030917 e5  c                   endif
234200030917     ?* da centralino ?
234300030917 b5  c                   if        ormtco = 'M'  or
234400030917     c                             ormtco = 'P'
234500030917     c                   eval      wroricene = wroricene +1
234600030917 e5  c                   endif
234700030917     ?* da internet ?
234800030917 b5  c                   if        ormtco = 'I'
234900030917     c                   eval      wroriinne = wroriinne +1
235000030917 e5  c                   endif
235100030917     ?* da vas ?
235200030917 b5  c                   if        ormtco = 'F'
235300030917     c                   eval      wrorivane = wrorivane +1
235400030917 e5  c                   endif
235500030917     ?* da call center ?
235600030917 b5  c                   if        ormtco = 'C'
235700030917     c                   eval      wroriccne = wroriccne +1
235800030917 e5  c                   endif
235900030917     ?* ORM non eseguiti con causale ?
236000030917 b5  c                   if        orffar = 500
236100030917     c                   eval      cauone = orfcar
236200030917     c                   if        orfcar = '01'
236300030917     c                   eval      wrorn01 = wrorn01 +1
236400030917     c                   endif
236500030917     c                   if        orfcar = '02'
236600030917     c                   eval      wrorn02 = wrorn02 +1
236700030917     c                   endif
236800030917     c                   if        orfcar = '03'
236900030917     c                   eval      wrorn03 = wrorn03 +1
237000030917     c                   endif
237100030917     c                   if        orfcar = '04'
237200030917     c                   eval      wrorn04 = wrorn04 +1
237300030917     c                   endif
237400030917     c                   leave
237500030917 e5  c                   endif
237600030917     ?* ORM non eseguiti no in fase 500 ?
237700010614     ?* ORM non assegnati
237800010619 b5  c                   if        orffar < 400
237900010614     c                   eval      wrornas = wrornas +1
238000010621     c   11              eval      w_rornas = 'X'
238100010619 e5  c                   endif
238200030917      * devo controllare se nel giorno è stata fatta una fase 500
238300030917     c     kOrf02        Readpe    Fnorf01l
238400030917 b5  c                   Dow       Not %Eof(Fnorf01l)
238500040421      * se trovo fase 200 esco
238600040618if  6c                   If        OrfFar = 200
238700040421     c                   Leave
238800040421e   6c                   EndIf
238900030917 b6  c                   If        OrfFar = 500
239000030917      * se trovo la fase 500 memorizzo la causale
239100030917     c                   Eval      %Subst(cauone:2:2) = OrfCar
239200031118     c                   Move      ')'           cauone
239300031118     c                   Movel     '('           cauone
239400030917     c                   If        OrfCar = '01'
239500030917     c                   Eval      wrorn01 = wrorn01 +1
239600030917     c                   Endif
239700030917     c                   If        OrfCar = '02'
239800030917     c                   Eval      wrorn02 = wrorn02 +1
239900030917     c                   Endif
240000030917     c                   If        OrfCar = '03'
240100030917     c                   Eval      wrorn03 = wrorn03 +1
240200030917     c                   Endif
240300030917     c                   If        OrfCar = '04'
240400030917     c                   Eval      wrorn04 = wrorn04 +1
240500030917     c                   Endif
240600030917     c                   Leave
240700030917 e6  c                   Endif
240800030917     c     kOrf02        Readpe    Fnorf01l
240900030917 e5  c                   EndDo
241000010608     c                   leave
241100010619 e4  c                   endif
241200010619
241300010619 e3  c                   endif
241400010608
241500010613 e2  c                   endif
241600010705
241700010705     c     passa         tag
241800010705
241900010608     c     korf          readpe    fnorf01l
242000010705
242100010613 e1  c                   enddo
242200010612
242300010612      * se ORM in fase 900 controllo se quadrato
242400010612     c                   if        wok900 = *on
242500010614      *                            =========
242600010614     c                   exsr      SR_QUADRA
242700010614      *                            =========
242800010612     c                   endif
242900010608
243000010608     c                   endsr
243100010614
243200010608      *--------------------------------------------------------------*
243300010608      * ORM NON QUADRATO
243400010608      *--------------------------------------------------------------*
243500010614     c     SR_QUADRA     begsr
243600010608
243700010608      * ORM non quadrati
243800010608      * mi devo posizionare sulle fasi con la data elaborazione
243900010608      * poi cercare se c'è una fase 900 controllare se esiste
244000010608      * una fase 600
244100010608
244200010608     c                   eval      wok600 = *off
244300010608
244400010608     c                   if        wok900 = *on
244500010608     c                   eval      kormdfo = dataela
244600010615     c                   clear                   kormofo
244700010615     c                   clear                   kormfao
244800010608     c     korf          setll     fnorf01l
244900010608     c                   do        *hival
245000010608     c     korf          reade     fnorf01l
245100010608     c                   if        %eof(fnorf01l)
245200010608     c                   leave
245300010608     c                   endif
245400010608     c                   if        orffar = 600
245500010608     c                   eval      wok600 = *on
245600010608     c                   leave
245700010608     c                   endif
245800010608     c                   enddo
245900010608     c                   if        wok600 = *off
246000010608     c                   eval      wrornqd = wrornqd +1
246100010621     c   11              eval      w_rornqd = 'X'
246200010608     c                   eval      wokstp = *on
246300010608     c                   endif
246400010608     c                   endif
246500010608
246600010608     c                   endsr
246700010614
246800010605      *--------------------------------------------------------------*
246900010605      * CONTROLLO E CARICAMENTO SK
247000010605      *--------------------------------------------------------------*
247100010605     c     SR_CONTRSK    begsr
247200010605
247300010606     c                   eval      woksk = *off
247400010606
247500010612     c                   do        18000         xf
247600030912     c                   if        kfile(xf) = dskfile
247700010605     c                   eval      woksk = *on
247800010605     c                   leave
247900010605     c                   endif
248000030912     c                   if        kfile1(xf) = *zeros
248100010605     c                   leave
248200010605     c                   endif
248300010605     c                   enddo
248400010605
248500010606     c                   if        woksk = *on
248600010612     c                   eval      dswfror = rfile(xf)
248700010606     c                   endif
248800010605
248900010605     c                   if        woksk = *off
249000010611     c                   clear                   dswfror
249100010612     c                   eval      kfile(xf) = dskfile
249200030916     c                   eval      kfile1(xf) = dskfile1
249300030916     c                   Eval      f_rortrc = dstrc
249400010605     c                   eval      f_rorpoe = dspoel
249500010605     c                   eval      f_rordae = dsdata
249600010605     c                   eval      kazorg = dspoel
249700010605     c     kazorg        chain     azorg01l
249800010605     c                   if        %found(azorg01l)
249900010605     c                   eval      f_rordis = orgfl3
250000010605     c                   eval      f_rorare = orgcar
250100010605     c                   endif
250200010605     c                   endif
250300010605
250400010605     c                   endsr
250500010614
250600010608      *--------------------------------------------------------------*
250700010608      * CARICO LA SK DEL FILE
250800010608      *--------------------------------------------------------------*
250900010614     c     SR_CARICA     begsr
251000010608
251100010608     c                   eval      f_rorode = (f_rorode + wrorode)
251200010608     c                   eval      f_roroes = (f_roroes + wroroes)
251300010608     c                   eval      f_rorone = (f_rorone + wrorone)
251400010608     c                   eval      f_rorn01 = (f_rorn01 + wrorn01)
251500010608     c                   eval      f_rorn02 = (f_rorn02 + wrorn02)
251600010608     c                   eval      f_rorn03 = (f_rorn03 + wrorn03)
251700010608     c                   eval      f_rorn04 = (f_rorn04 + wrorn04)
251800010608     c                   eval      f_rorn05 = (f_rorn05 + wrorn05)
251900010608     c                   eval      f_rorn06 = (f_rorn06 + wrorn06)
252000010608     c                   eval      f_rorn07 = (f_rorn07 + wrorn07)
252100010608     c                   eval      f_rorn08 = (f_rorn08 + wrorn08)
252200010608     c                   eval      f_rorn09 = (f_rorn09 + wrorn09)
252300010608     c                   eval      f_rorn10 = (f_rorn10 + wrorn10)
252400010608     c                   eval      f_roroea = (f_roroea + wroroea)
252500010608     c                   eval      f_rora01 = (f_rora01 + wrora01)
252600010608     c                   eval      f_rora02 = (f_rora02 + wrora02)
252700010608     c                   eval      f_rora03 = (f_rora03 + wrora03)
252800010608     c                   eval      f_rora04 = (f_rora04 + wrora04)
252900010608     c                   eval      f_rora05 = (f_rora05 + wrora05)
253000010608     c                   eval      f_rora06 = (f_rora06 + wrora06)
253100010608     c                   eval      f_rora07 = (f_rora07 + wrora07)
253200010608     c                   eval      f_rora08 = (f_rora08 + wrora08)
253300010608     c                   eval      f_rora09 = (f_rora09 + wrora09)
253400010608     c                   eval      f_rora10 = (f_rora10 + wrora10)
253500010608     c                   eval      f_rora11 = (f_rora11 + wrora11)
253600010608     c                   eval      f_rora12 = (f_rora12 + wrora12)
253700010608     c                   eval      f_rora13 = (f_rora13 + wrora13)
253800010608     c                   eval      f_rora14 = (f_rora14 + wrora14)
253900010608     c                   eval      f_rora15 = (f_rora15 + wrora15)
254000010620     c                   eval      f_rordan = (f_rordan + wrordan)
254100010608     c                   eval      f_rordit = (f_rordit + wrordit)
254200010608     c                   eval      f_rordr1 = (f_rordr1 + wrordr1)
254300010608     c                   eval      f_rordr2 = (f_rordr2 + wrordr2)
254400010608     c                   eval      f_rordro = (f_rordro + wrordro)
254500010614     c                   eval      f_rorean = (f_rorean + wrorean)
254600010608     c                   eval      f_roreit = (f_roreit + wroreit)
254700010608     c                   eval      f_rorer1 = (f_rorer1 + wrorer1)
254800010608     c                   eval      f_rorer2 = (f_rorer2 + wrorer2)
254900010608     c                   eval      f_rorero = (f_rorero + wrorero)
255000010620     c                   eval      f_rornan = (f_rornan + wrornan)
255100010608     c                   eval      f_rornit = (f_rornit + wrornit)
255200010608     c                   eval      f_rornr1 = (f_rornr1 + wrornr1)
255300010608     c                   eval      f_rornr2 = (f_rornr2 + wrornr2)
255400010608     c                   eval      f_rornro = (f_rornro + wrornro)
255500010620     c                   eval      f_roraan = (f_roraan + wroraan)
255600010608     c                   eval      f_rorait = (f_rorait + wrorait)
255700010608     c                   eval      f_rorar1 = (f_rorar1 + wrorar1)
255800010608     c                   eval      f_rorar2 = (f_rorar2 + wrorar2)
255900010608     c                   eval      f_roraro = (f_roraro + wroraro)
256000010608     c                   eval      f_rornqd = (f_rornqd + wrornqd)
256100010608     c                   eval      f_rornbl = (f_rornbl + wrornbl)
256200010614     c                   eval      f_rormdr = (f_rormdr + wrormdr)
256300010614     c                   eval      f_rornas = (f_rornas + wrornas)
256400030912     c                   eval      f_roroned = (f_roroned + wroroned)
256500030912     c                   eval      f_rorn01d = (f_rorn01d + wrorn01d)
256600030912     c                   eval      f_rorn02d = (f_rorn02d + wrorn02d)
256700030912     c                   eval      f_rorn03d = (f_rorn03d + wrorn03d)
256800030912     c                   eval      f_rorn04d = (f_rorn04d + wrorn04d)
256900030912     c                   eval      f_rorn05d = (f_rorn05d + wrorn05d)
257000030912     c                   eval      f_rorn06d = (f_rorn06d + wrorn06d)
257100030912     c                   eval      f_rorn07d = (f_rorn07d + wrorn07d)
257200030912     c                   eval      f_rorn08d = (f_rorn08d + wrorn08d)
257300030912     c                   eval      f_rorn09d = (f_rorn09d + wrorn09d)
257400030912     c                   eval      f_rorn10d = (f_rorn10d + wrorn10d)
257500030917     c                   eval      f_rorifian = (f_rorifian + wrorifian)
257600030917     c                   eval      f_rorifine = (f_rorifine + wrorifine)
257700030917     c                   eval      f_rorifid  = (f_rorifid  + wrorifid)
257800030917     c                   eval      f_roricean = (f_roricean + wroricean)
257900030917     c                   eval      f_roricene = (f_roricene + wroricene)
258000030917     c                   eval      f_roriced  = (f_roriced  + wroriced)
258100030917     c                   eval      f_roriinan = (f_roriinan + wroriinan)
258200030917     c                   eval      f_roriinne = (f_roriinne + wroriinne)
258300030917     c                   eval      f_roriind  = (f_roriind  + wroriind)
258400030917     c                   eval      f_rorivaan = (f_rorivaan + wrorivaan)
258500030917     c                   eval      f_rorivane = (f_rorivane + wrorivane)
258600030917     c                   eval      f_rorivad  = (f_rorivad  + wrorivad)
258700030917     c                   eval      f_roriccan = (f_roriccan + wroriccan)
258800030917     c                   eval      f_roriccne = (f_roriccne + wroriccne)
258900030917     c                   eval      f_roriccd  = (f_roriccd  + wroriccd)
259000010612     c                   eval      rfile(xf) = dswfror
259100010608
259200010608     c                   endsr
259300010614
259400010604      *--------------------------------------------------------------*
259500010604      * ROUTINE INIZIALE
259600010604      *--------------------------------------------------------------*
259700010604     c     *inzsr        begsr
259800010604      *
259900010604     c     *entry        plist
260000010604     c                   parm                    kpjba
260100010621
260200010621     c                   movel     kpjbu         fior40ds
260300010621
260400030912      * controllo il sistema informativo dove sono per reperire
260500030912      * la libreria
260600030912      *   ---> GAITRA201/GAITRAGRU
260700030912     c                   If        %subst(knsif:7:3) = '201'  and
260800030912     c                             %subst(knsif:1:1) = 'G'
260900030912     c                   Eval      wlib = 'GAITRAGRU '
261000030912     c                   EndIf
261100030916      *   ---> FILTRA201/FILTRAGRU
261200030912     c                   If        %subst(knsif:7:3) = '201'  and
261300030912     c                             %subst(knsif:1:1) = 'F'
261400030912     c                   Eval      wlib = 'FILTRAGRU '
261500030912     c                   EndIf
261600030916      *   ---> GAITRAPSM/GAITRAGRPS
261700030916     c                   If        %subst(knsif:7:3) = 'PSM'  and
261800030916     c                             %subst(knsif:1:1) = 'G'
261900030916     c                   Eval      wlib = 'GAITRAGRPS'
262000030916     c                   EndIf
262100030912      *   ---> GAITRAPSP/GAITRAGRPS
262200030912     c                   If        %subst(knsif:7:3) = 'PSP'  and
262300030912     c                             %subst(knsif:1:1) = 'G'
262400040421     c                   Eval      wlib = 'GAITRAGRPS'
262500040421     c**                 Eval      wlib = 'GAITRAFIL '
262600030912     c                   EndIf
262700030916      *   ---> GAITRAPFM/GAITRAGRPF
262800030916     c                   If        %subst(knsif:7:3) = 'PFM'  and
262900030916     c                             %subst(knsif:1:1) = 'G'
263000030916     c                   Eval      wlib = 'GAITRAGRPF'
263100030916     c                   EndIf
263200030912      *   ---> GAITRAPFP/GAITRAGRPF
263300030912     c                   If        %subst(knsif:7:3) = 'PFP'  and
263400030912     c                             %subst(knsif:1:1) = 'G'
263500040421     c                   Eval      wlib = 'GAITRAGRPF'
263600030912     c                   EndIf
263700030912      *   ---> FILTRAPFP/FILTRAGRPF
263800030912     c                   If        %subst(knsif:7:3) = 'PFP'  and
263900030912     c                             %subst(knsif:1:1) = 'F'
264000040421     c                   Eval      wlib = 'FILTRAGRPF'
264100030912     c                   EndIf
264200030912
264300010621      * duplico il file di comodo in qtemp
264400010621     c                   clear                   comman
264500010621     c                   movel(p)  cmd(1)        comman
264600030912     c                   Eval      %Subst(comman:32:10) = wlib
264700010621     c                   call      'QCMDEXC'
264800010621     c                   parm                    comman
264900010621     c                   parm                    lenght
265000010621      * apro il file di comodo per la stampa
265100010621     c                   open      wfror11l
265200010621
265300010604     c                   z-add     1             codut
265400010604     c                   call      'X§PARUT'
265500010604     c                   parm                    ut§dse0f
265600010614     c                   movel     ragut         rsut
265700010604     c                   movel     rec80         cncr80
265800010604
265900010604      * reperisco data e ora
266000010604     c                   time                    w0140
266100010604      * udate in GGMMAAAA
266200010604     c                   move      w0140         wdate
266300010604     c                   movel     w0140         wora
266400010608
266500010608      * pulisco le sk
266600010608     c                   clear                   kfile
266700030916     c                   clear                   kfile1
266800010608     c                   clear                   rfile
266900010608     c                   clear                   skdate
267000010608     c                   clear                   skpoel
267100130118     c                   Clear                   skcliosr
267200130118     c                   Clear                   skstaosr
267300010621
267400010621      * controllo se devo stampare per data o per p.o.
267500010621     c                   if        i40poel <> 999 and
267600010621     c                             i40dtda <> i40dtal and
267700010621     c                             i40tpst = 'S'
267800010621     c                   eval      *in13 = *on
267900010621     c                   else
268000010621     c                   eval      *in13 = *off
268100010621     c                   endif
268200010608
268300010608      * carico la sk delle date da elaborare
268400010703     c                   eval      xd = 1
268500010703     c                   eval      skdate(xd) = i40dtda
268600010703     c                   if        i40dtda <> i40dtal
268700010703     c                   eval      wdtgio = i40dtda
268800010703     c     *iso          movel     i40dtda       wdtda
268900010703     c                   dow       wdtgio < i40dtal
269000010703     c                   eval      xd = xd +1
269100010703     c                   adddur    1:*d          wdtda
269200010703     c                   move      wdtda         wdtgio
269300010703     c                   eval      skdate(xd) = wdtgio
269400010703     c                   enddo
269500010703     c                   endif
269600020807
269700020808     c                   z-add     1             xa
269800010608
269900020807      * Elaboro tutti i p.o.
270000020807 b1  c                   if        i40poel = 999
270100020807      * Ricerco tutti i terminal di partenza
270200020807     c                   Clear                   Trul26Ds
270300020807     c                   Eval      I26tpt = 'P'
270400020807     c                   Eval      I26tla = 'L'
270500020807     c                   Call      'TRUL26R'
270600020807     c                   Parm                    Trul26Ds
270700020807      * carico i p.o. da elaborare dell'AS in un sk
270800020807 b2  c                   Do        250           Xb
270900020807     c                   If        Skpot(Xb) = *Zeros
271000020807     c                   Leave
271100020807     c                   EndIf
271200020807      * Per la filiale se lancio tutti i p.o. devo cercare quelli che
271300020807      * hanno il terminal di partenza = simfel
271400020807 b3  c                   If        i40sede = 'F'
271500020807     c                   If        Skpot(Xb) = Simfel
271600020807     c                   Eval      Skpoel(Xa) = Skpo(Xb)
271700020808     c                   Add       1             Xa
271800010608     c                   endif
271900020807      * Per la Sede se lancio tutti i p.o. carico tutti
272000020807 b3  c                   Else
272100020807     c                   Eval      Skpoel(Xb) = Skpo(Xb)
272200020807 e3  c                   EndIf
272300010608
272400020807 e2  c                   EndDo
272500020807 e1  c                   EndIf
272600010605      * lanciato un solo po carico questo nella sk
272700010605     c                   if        i40poel <> 999
272800010612     c                   eval      skpoel(xa) = i40poel
272900010605     c                   endif
273000031002
273100031002      * carico la sk delle serie da non elaborare in statistica
273200031002     c                   Eval      xc = 1
273300031002     c                   Eval      TbeCod = 'OSR'
273400031002     c     TbeCod        Setll     Tntbe01l
273500031002Do  1c                   Do        *Hival
273600031002     c     TbeCod        Reade     Tntbe01l
273700031002     c                   If        %Eof(Tntbe01l)
273800031002     c                   Leave
273900031002     c                   EndIf
274000130122     c                   IF        TBEatb <> *blanks
274100130122     c                   iter
274200130122     c                   ENDIF
274300031002     c                   Movel     TbeUni        dOsr
274400130118      /free
274500130118           IF d§osrcli <> 0;
274600130118             xx = %lookup(d§OSRcli:skCLIosr);
274700130118             IF  xx = 0;
274800130118               xx = %lookup(*zeros:skCLIosr);
274900130118               IF  xx > 0;
275000130118                 skCLIosr(xx) = d§OSRcli;
275100130118                 skstaosr(xx) = d§OSRsta;
275200130118               ENDIF;
275300130118             ENDIF;
275400130118           ENDIF;
275500130118
275600130118           IF d§osrcl2 <> 0;
275700130118             xx = %lookup(d§OSRcl2:skCLIosr);
275800130118             IF  xx = 0;
275900130118               xx = %lookup(*zeros:skCLIosr);
276000130118               IF  xx > 0;
276100130118                 skCLIosr(xx) = d§OSRcl2;
276200130118                 skstaosr(xx) = d§OSRsta;
276300130118               ENDIF;
276400130118             ENDIF;
276500130118           ENDIF;
276600130118
276700130118           IF d§osrcl3 <> 0;
276800130118             xx = %lookup(d§OSRcl3:skCLIosr);
276900130118             IF  xx = 0;
277000130118               xx = %lookup(*zeros:skCLIosr);
277100130118               IF  xx > 0;
277200130118                 skCLIosr(xx) = d§OSRcl3;
277300130118                 skstaosr(xx) = d§OSRsta;
277400130118               ENDIF;
277500130118             ENDIF;
277600130118           ENDIF;
277700130118
277800130118           IF d§osrcl4 <> 0;
277900130118             xx = %lookup(d§OSRcl4:skCLIosr);
278000130118             IF  xx = 0;
278100130118               xx = %lookup(*zeros:skCLIosr);
278200130118               IF  xx > 0;
278300130118                 skCLIosr(xx) = d§OSRcl4;
278400130118                 skstaosr(xx) = d§OSRsta;
278500130118               ENDIF;
278600130118             ENDIF;
278700130118           ENDIF;
278800130118
278900130118           IF d§osrcl5 <> 0;
279000130118             xx = %lookup(d§OSRcl5:skCLIosr);
279100130118             IF  xx = 0;
279200130118               xx = %lookup(*zeros:skCLIosr);
279300130118               IF  xx > 0;
279400130118                 skCLIosr(xx) = d§OSRcl5;
279500130118                 skstaosr(xx) = d§OSRsta;
279600130118               ENDIF;
279700130118             ENDIF;
279800130118           ENDIF;
279900130118
280000130118           IF d§osrcl6 <> 0;
280100130118             xx = %lookup(d§OSRcl6:skCLIosr);
280200130118             IF  xx = 0;
280300130118               xx = %lookup(*zeros:skCLIosr);
280400130118               IF  xx > 0;
280500130118                 skCLIosr(xx) = d§OSRcl6;
280600130118                 skstaosr(xx) = d§OSRsta;
280700130118               ENDIF;
280800130118             ENDIF;
280900130118           ENDIF;
281000130118
281100130118           IF d§osrcl7 <> 0;
281200130118             xx = %lookup(d§OSRcl7:skCLIosr);
281300130118             IF  xx = 0;
281400130118               xx = %lookup(*zeros:skCLIosr);
281500130118               IF  xx > 0;
281600130118                 skCLIosr(xx) = d§OSRcl7;
281700130118                 skstaosr(xx) = d§OSRsta;
281800130118               ENDIF;
281900130118             ENDIF;
282000130118           ENDIF;
282100130118
282200130118           IF d§osrcl8 <> 0;
282300130118             xx = %lookup(d§OSRcl8:skCLIosr);
282400130118             IF  xx = 0;
282500130118               xx = %lookup(*zeros:skCLIosr);
282600130118               IF  xx > 0;
282700130118                 skCLIosr(xx) = d§OSRcl8;
282800130118                 skstaosr(xx) = d§OSRsta;
282900130118               ENDIF;
283000130118             ENDIF;
283100130118           ENDIF;
283200130118
283300130118           IF d§osrcl9 <> 0;
283400130118             xx = %lookup(d§OSRcl9:skCLIosr);
283500130118             IF  xx = 0;
283600130118               xx = %lookup(*zeros:skCLIosr);
283700130118               IF  xx > 0;
283800130118                 skCLIosr(xx) = d§OSRcl9;
283900130118                 skstaosr(xx) = d§OSRsta;
284000130118               ENDIF;
284100130118             ENDIF;
284200130118           ENDIF;
284300130118
284400130118           IF d§osrcl10 <> 0;
284500130118             xx = %lookup(d§OSRcl10:skCLIosr);
284600130118             IF  xx = 0;
284700130118               xx = %lookup(*zeros:skCLIosr);
284800130118               IF  xx > 0;
284900130118                 skCLIosr(xx) = d§OSRcl10;
285000130118                 skstaosr(xx) = d§OSRsta;
285100130118               ENDIF;
285200130118             ENDIF;
285300130118           ENDIF;
285400130118
285500130118           IF d§osrcl11 <> 0;
285600130118             xx = %lookup(d§OSRcl11:skCLIosr);
285700130118             IF  xx = 0;
285800130118               xx = %lookup(*zeros:skCLIosr);
285900130118               IF  xx > 0;
286000130118                 skCLIosr(xx) = d§OSRcl11;
286100130118                 skstaosr(xx) = d§OSRsta;
286200130118               ENDIF;
286300130118             ENDIF;
286400130118           ENDIF;
286500130118
286600130118           IF d§osrcl12 <> 0;
286700130118             xx = %lookup(d§OSRcl12:skCLIosr);
286800130118             IF  xx = 0;
286900130118               xx = %lookup(*zeros:skCLIosr);
287000130118               IF  xx > 0;
287100130118                 skCLIosr(xx) = d§OSRcl12;
287200130118                 skstaosr(xx) = d§OSRsta;
287300130118               ENDIF;
287400130118             ENDIF;
287500130118           ENDIF;
287600130118
287700130118           IF d§osrcl13 <> 0;
287800130118             xx = %lookup(d§OSRcl13:skCLIosr);
287900130118             IF  xx = 0;
288000130118               xx = %lookup(*zeros:skCLIosr);
288100130118               IF  xx > 0;
288200130118                 skCLIosr(xx) = d§OSRcl13;
288300130118                 skstaosr(xx) = d§OSRsta;
288400130118               ENDIF;
288500130118             ENDIF;
288600130118           ENDIF;
288700130118
288800130118           IF d§osrcl14 <> 0;
288900130118             xx = %lookup(d§OSRcl14:skCLIosr);
289000130118             IF  xx = 0;
289100130118               xx = %lookup(*zeros:skCLIosr);
289200130118               IF  xx > 0;
289300130118                 skCLIosr(xx) = d§OSRcl14;
289400130118                 skstaosr(xx) = d§OSRsta;
289500130118               ENDIF;
289600130118             ENDIF;
289700130118           ENDIF;
289800130118
289900130118           IF d§osrcl15 <> 0;
290000130118             xx = %lookup(d§OSRcl15:skCLIosr);
290100130118             IF  xx = 0;
290200130118               xx = %lookup(*zeros:skCLIosr);
290300130118               IF  xx > 0;
290400130118                 skCLIosr(xx) = d§OSRcl15;
290500130118                 skstaosr(xx) = d§OSRsta;
290600130118               ENDIF;
290700130118             ENDIF;
290800130118           ENDIF;
290900130118
291000130118           IF d§osrcl16 <> 0;
291100130118             xx = %lookup(d§OSRcl16:skCLIosr);
291200130118             IF  xx = 0;
291300130118               xx = %lookup(*zeros:skCLIosr);
291400130118               IF  xx > 0;
291500130118                 skCLIosr(xx) = d§OSRcl16;
291600130118                 skstaosr(xx) = d§OSRsta;
291700130118               ENDIF;
291800130118             ENDIF;
291900130118           ENDIF;
292000130118      /end-free
292100130118
292200031002    1c                   EndDo
292300010611
292400010611      * carico nei campi di stampa le descrizioni delle causali
292500010611     c                   eval      tbecod = 'CMR'
292600010611      * causali di non fatto
292700010611     c                   eval      tbeke1 = '01'
292800010611     c     ktntbe        chain     tntbe01l
292900010611     c                   if        %found(tntbe01l)
293000010619     c                   clear                   dcmr
293100010619     c                   movel     tbeuni        dcmr
293200010619     c                   if        d§cmrdes1 <> *blanks
293300010619     c                   movel     d§cmrdes1     descau01n
293400010619     c                   else
293500010619     c                   movel     d§cmrdes      descau01n
293600010619     c                   endif
293700010611     c                   endif
293800010619
293900010611     c                   eval      tbeke1 = '02'
294000010611     c     ktntbe        chain     tntbe01l
294100010611     c                   if        %found(tntbe01l)
294200010619     c                   clear                   dcmr
294300010619     c                   movel     tbeuni        dcmr
294400010619     c                   if        d§cmrdes1 <> *blanks
294500010619     c                   movel     d§cmrdes1     descau02n
294600010619     c                   else
294700010619     c                   movel     d§cmrdes      descau02n
294800010619     c                   endif
294900010611     c                   endif
295000010619
295100010611     c                   eval      tbeke1 = '03'
295200010611     c     ktntbe        chain     tntbe01l
295300010611     c                   if        %found(tntbe01l)
295400010619     c                   clear                   dcmr
295500010619     c                   movel     tbeuni        dcmr
295600010619     c                   if        d§cmrdes1 <> *blanks
295700010619     c                   movel     d§cmrdes1     descau03n
295800010619     c                   else
295900010619     c                   movel     d§cmrdes      descau03n
296000010619     c                   endif
296100010611     c                   endif
296200020527
296300020527     c                   eval      tbeke1 = '04'
296400020527     c     ktntbe        chain     tntbe01l
296500020527     c                   if        %found(tntbe01l)
296600020527     c                   clear                   dcmr
296700020527     c                   movel     tbeuni        dcmr
296800020527     c                   if        d§cmrdes1 <> *blanks
296900020527     c                   movel     d§cmrdes1     descau04n
297000020527     c                   else
297100020527     c                   movel     d§cmrdes      descau04n
297200020527     c                   endif
297300020527     c                   endif
297400010619
297500010611      * causali di esito anomalo
297600010611     c                   eval      tbeke1 = '80'
297700010611     c     ktntbe        chain     tntbe01l
297800010611     c                   if        %found(tntbe01l)
297900010619     c                   clear                   dcmr
298000010619     c                   movel     tbeuni        dcmr
298100010619     c                   if        d§cmrdes1 <> *blanks
298200010619     c                   movel     d§cmrdes1     descau01a
298300010619     c                   else
298400010619     c                   movel     d§cmrdes      descau01a
298500010619     c                   endif
298600010611     c                   endif
298700010619
298800010611     c                   eval      tbeke1 = '81'
298900010611     c     ktntbe        chain     tntbe01l
299000010611     c                   if        %found(tntbe01l)
299100010619     c                   clear                   dcmr
299200010619     c                   movel     tbeuni        dcmr
299300010619     c                   if        d§cmrdes1 <> *blanks
299400010619     c                   movel     d§cmrdes1     descau02a
299500010619     c                   else
299600010619     c                   movel     d§cmrdes      descau02a
299700010619     c                   endif
299800010611     c                   endif
299900010619
300000010611     c                   eval      tbeke1 = '90'
300100010611     c     ktntbe        chain     tntbe01l
300200010611     c                   if        %found(tntbe01l)
300300010619     c                   clear                   dcmr
300400010619     c                   movel     tbeuni        dcmr
300500010619     c                   if        d§cmrdes1 <> *blanks
300600010619     c                   movel     d§cmrdes1     descau03a
300700010619     c                   else
300800010619     c                   movel     d§cmrdes      descau03a
300900010619     c                   endif
301000010611     c                   endif
301100010619
301200010611     c                   eval      tbeke1 = '91'
301300010611     c     ktntbe        chain     tntbe01l
301400010611     c                   if        %found(tntbe01l)
301500010619     c                   clear                   dcmr
301600010619     c                   movel     tbeuni        dcmr
301700010619     c                   if        d§cmrdes1 <> *blanks
301800010619     c                   movel     d§cmrdes1     descau04a
301900010619     c                   else
302000010619     c                   movel     d§cmrdes      descau04a
302100010619     c                   endif
302200010611     c                   endif
302300010619
302400010611     c                   eval      tbeke1 = '92'
302500010611     c     ktntbe        chain     tntbe01l
302600010611     c                   if        %found(tntbe01l)
302700010619     c                   clear                   dcmr
302800010619     c                   movel     tbeuni        dcmr
302900010619     c                   if        d§cmrdes1 <> *blanks
303000010619     c                   movel     d§cmrdes1     descau05a
303100010619     c                   else
303200010619     c                   movel     d§cmrdes      descau05a
303300010619     c                   endif
303400010611     c                   endif
303500010619
303600010611     c                   eval      tbeke1 = '93'
303700010611     c     ktntbe        chain     tntbe01l
303800010611     c                   if        %found(tntbe01l)
303900010619     c                   clear                   dcmr
304000010619     c                   movel     tbeuni        dcmr
304100010619     c                   if        d§cmrdes1 <> *blanks
304200010619     c                   movel     d§cmrdes1     descau06a
304300010619     c                   else
304400010619     c                   movel     d§cmrdes      descau06a
304500010619     c                   endif
304600010611     c                   endif
304700010619
304800010611     c                   eval      tbeke1 = '94'
304900010611     c     ktntbe        chain     tntbe01l
305000010611     c                   if        %found(tntbe01l)
305100010619     c                   clear                   dcmr
305200010619     c                   movel     tbeuni        dcmr
305300010619     c                   if        d§cmrdes1 <> *blanks
305400010619     c                   movel     d§cmrdes1     descau07a
305500010619     c                   else
305600010619     c                   movel     d§cmrdes      descau07a
305700010619     c                   endif
305800010611     c                   endif
305900020527
306000020527     c                   eval      tbeke1 = '95'
306100020527     c     ktntbe        chain     tntbe01l
306200020527     c                   if        %found(tntbe01l)
306300020527     c                   clear                   dcmr
306400020527     c                   movel     tbeuni        dcmr
306500020527     c                   if        d§cmrdes1 <> *blanks
306600020527     c                   movel     d§cmrdes1     descau08a
306700020527     c                   else
306800020527     c                   movel     d§cmrdes      descau08a
306900020527     c                   endif
307000020527     c                   endif
307100020625
307200020625     c                   eval      tbeke1 = '82'
307300020625     c     ktntbe        chain     tntbe01l
307400020625     c                   if        %found(tntbe01l)
307500020625     c                   clear                   dcmr
307600020625     c                   movel     tbeuni        dcmr
307700020625     c                   if        d§cmrdes1 <> *blanks
307800020625     c                   movel     d§cmrdes1     descau09a
307900020625     c                   else
308000020625     c                   movel     d§cmrdes      descau09a
308100020625     c                   endif
308200020625     c                   endif
308300040701
308400040701     c                   eval      tbeke1 = '96'
308500040701     c     ktntbe        chain     tntbe01l
308600040701     c                   if        %found(tntbe01l)
308700040701     c                   clear                   dcmr
308800040701     c                   movel     tbeuni        dcmr
308900040701     c                   if        d§cmrdes1 <> *blanks
309000040701     c                   movel     d§cmrdes1     descau10a
309100040701     c                   else
309200040701     c                   movel     d§cmrdes      descau10a
309300040701     c                   endif
309400040701     c                   endif
309500040701
309600040701     c                   eval      tbeke1 = '97'
309700040701     c     ktntbe        chain     tntbe01l
309800040701     c                   if        %found(tntbe01l)
309900040701     c                   clear                   dcmr
310000040701     c                   movel     tbeuni        dcmr
310100040701     c                   if        d§cmrdes1 <> *blanks
310200040701     c                   movel     d§cmrdes1     descau11a
310300040701     c                   else
310400040701     c                   movel     d§cmrdes      descau11a
310500040701     c                   endif
310600040701     c                   endif
310700060926
310800060926      * carico la sk delle fasi da non elaborare in statistica
310900060926     c                   eval      xc = 1
311000060926     c                   eval      tbecod = 'FAR'
311100060926     c     tbecod        setll     tntbe01l
311200060926Do  1c                   do        *hival
311300060926     c     tbecod        reade     tntbe01l
311400060926     c                   if        %eof(tntbe01l)
311500060926     c                   leave
311600060926     c                   endif
311700060926     c                   movel     tbeuni        dfar
311800060926     c                   if        d§farsta = *blanks
311900060926     c                   iter
312000060926     c                   endif
312100060926     c                   movel     tbeke1        skfar(xc)
312200060926     c                   add       1             xc
312300060926    1c                   enddo
312400010620
312500010620     c                   setoff                                       1011
312600010620
312700010705      * se richiesto apro il file della stampa di comodo EDP
312800010705     c                   if        i40edp = 'S'
312900010621     c                   open      fior41p2
313000010705     c                   eval      *in10 = *on
313100010620     c                   endif
313200010620
313300010705      * se richiesta la stampa di controllo apro il file di stampa
313400010620     c                   if        i40stpc = 'S'
313500010620     c                   open      fior41p1
313600010620     c                   eval      *in11 = *on
313700010620     c                   endif
313800010605
313900010621      * se richiesta la creazione del file lo apro
314000010621     c                   if        i40tpel = 'F'
314100010621     c                   open      wfror01l
314200010621     c                   endif
314300010621
314400010605      * chiavi
314500010605     c     korf          klist
314600010605     c                   kfld                    kormpoe
314700010605     c                   kfld                    kormnsr
314800010605     c                   kfld                    kormnor
314900010605     c                   kfld                    kormnrv
315000010604
315100010606     c     korf01        klist
315200010606     c                   kfld                    kormpoe
315300010606     c                   kfld                    kormnsr
315400010606     c                   kfld                    kormnor
315500010606     c                   kfld                    kormnrv
315600010606     c                   kfld                    kormdfo
315700010606     c                   kfld                    kormofo
315800010606     c                   kfld                    kormfao
315900010606
316000010606     c     korf02        klist
316100010606     c                   kfld                    kormpoe
316200010606     c                   kfld                    kormnsr
316300010606     c                   kfld                    kormnor
316400010606     c                   kfld                    kormnrv
316500010606     c                   kfld                    kormdfo
316600040420
316700040420     c     korf03        klist
316800040420     c                   kfld                    kormpoe
316900040420     c                   kfld                    kormnsr
317000040420     c                   kfld                    kormnor
317100040420     c                   kfld                    kormnrv
317200040420     c                   kfld                    kormfao
317300040420     c                   kfld                    kormdfo
317400040420
317500040420     c     korf04        klist
317600040420     c                   kfld                    kormpoe
317700040420     c                   kfld                    kormnsr
317800040420     c                   kfld                    kormnor
317900040420     c                   kfld                    kormnrv
318000040420     c                   kfld                    kormfao
318100010608
318200010618     c     korv          klist
318300010618     c                   kfld                    kormpoe
318400010618     c                   kfld                    kormnsr
318500010618     c                   kfld                    kormnor
318600010618     c                   kfld                    kormnrv
318700010618     c                   kfld                    korvdtv
318800010618
318900010608     c     kror          klist
319000010608     c                   kfld                    krorpoe
319100010608     c                   kfld                    krordta
319200030918     c                   kfld                    krortrc
319300030919
319400030919     c     kror1         klist
319500030919     c                   kfld                    krorpoe
319600030919     c                   kfld                    krordta
319700010606
319800010606     c     kazcln        klist
319900010606     c                   kfld                    ktfp
320000010606     c                   kfld                    ktfa
320100010606     c                   kfld                    kann
320200010606     c                   kfld                    kmes
320300010611
320400010620     c     ktab          klist
320500010620     c                   kfld                    codut
320600010620     c                   kfld                    tblcod
320700010620     c                   kfld                    tblkey
320800010620
320900010611     c     ktntbe        klist
321000010611     c                   kfld                    tbecod
321100010611     c                   kfld                    tbeke1
321200010606
321300010604     c                   endsr
321400010606      *--------------------------------------------------------------*
321500010621**   cmd - comando
321600030912CRTDUPOBJ OBJ(WFROR1*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
