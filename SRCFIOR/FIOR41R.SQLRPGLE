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
019200030918     d sxf             s              5i 0 inz(*zeros)                          indice sk file save
019300130118     d xc              s              5i 0 inz(*zeros)
019400130118     d xx              s              5i 0 inz(*zeros)
019500170322     d xb              s              5i 0 inz(*zeros)                          indice sk skpoel
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
026200030912     d dorm01        e ds
026300031002     d dosr          e ds
026400060926     d dfar          e ds
026500020807
026600020807     d FnormxxDs     e ds                  ExtName(Fnorm00f)
026700010604
026800010604     d kpjba         e ds
026900010604     d  rem                    1      3
027000010604     d  remfil                 4      6
027100140707
027200140707      // - Driver per reperire mail/sms
027300140707     d FIOR900DS     e ds                  qualified inz
027400170322
027500170322      // - Reperimento dati utente
027600170322     d TIBS34DS      e ds
027700170322
027800170322      // - Reperimento filiali gestite dall'utente
027900170322     d TRUL31DS      e ds
028000170322     d  pog                   10    759    dim(250)
028100170322
028200170322      //---------------------------------------------------------------
028300170322      //?Definizione aree dati.
028400170322      //---------------------------------------------------------------
028500170322      // - Dati utente
028600170322     d §AzUte        e ds                  extname(AZUTE00F)
028700170322     d                                     dtaara
028800170322     d §DatiUte      e ds                  extname(dDatiUte)
028900170322     d                                     dtaara
029000140707
029100140707      //---------------------------------------------------------------
029200140707      //?Definizione procedure esterne.
029300140707      //---------------------------------------------------------------
029400140707      // - Driver per reperire mail/sms
029500140707     d FIOR900R        pr                  extpgm('FIOR900R')
029600140707     d  fior900ds                          likeds(fior900ds)
029700170322
029800170322      /copy gaitrasrc/srcprotopr,TIBS34R
029900170322
030000170322      // - Caricamento Filiali in gestione
030100170322     d TRUL31R         pr                  extpgm('TRUL31R')
030200170322     d  kpjba                              likeds(kpjba)
030300170322     d  trul31ds                           likeds(trul31ds)
030400010604
030500010604      *--------------------------------------------------------------*
030600010604
030700010608      * stampo la videata delle selezioni
030800010612      *                            ========
030900010604     c                   exsr      PRESELEZ
031000010612      *                            ========
031100020808
031200020808      * Se sede e richiesto un solo p.o. o filiale devo elaborare con SQL
031300020808     c                   If        (I40sede <> 'F' and I40poel <> 999) or
031400020808     c                              I40sede = 'F'
031500020807      * Sql per selezionare gli ORM
031600020807      *                            ========
031700020808     c                   ExSr      SqlOrmPos
031800020807      *                            ========
031900020808     c                   EndIf
032000020807
032100010612     c                   eval      xd = 1
032200010608      * devo elaborare per data
032300010612     c                   do        31            xd
032400010612     c                   if        skdate(xd) = *zeros
032500010608     c                   leave
032600010608     c                   endif
032700010608
032800010612     c                   eval      dataela = skdate(xd)
032900020808
033000020808      * Se sede e richiesti tutti i p.o devo leggere tutto il file
033100020808     c                   If        I40sede <> 'F' and I40poel = 999
033200020808      *                            ===========
033300020808     c                   ExSr      Sr_LeggiOrm
033400020808      *                            ===========
033500020808     c                   Else
033600020808      * Se sede e richiesto un solo p.o. o filiale devo leggere con SQL
033700010614      *                            ========
033800010614     c                   exsr      SR_LEGGI
033900010614      *                            ========
034000020808     c                   EndIf
034100010608     c                   enddo
034200010608
034300010612      * creo il file e stampo la sk
034400010612     c                   eval      xf = 1
034500010612     c                   do        18000         xf
034600010612      * esco se non ho più niente caricato nella sk
034700030912     c                   if        kfile1(xf) = *zeros
034800010608     c                   leave
034900010608     c                   endif
035000010612      * carico le ds
035100010612     c                   eval      dskfile = kfile(xf)
035200030912     c                   eval      dskfile1 = kfile1(xf)
035300010612     c                   eval      dswfror = rfile(xf)
035400010621      * scrivo file appoggio
035500010621     c                   eval      dswfror1 = rfile(xf)
035600010621     c                   write     wfror100
035700010621     c                   clear                   dswfror1
035800010612      * scrivo il file
035900010608     c                   if        i40tpel = 'F'
036000030912     c                   eval      krortrc = dstrc
036100010608     c                   eval      krorpoe = dspoel
036200010608     c                   eval      krordta = dsdata
036300010608     c     kror          chain     wfror01l
036400010608     c                   if        not%found(wfror01l)
036500010612     c                   eval      dswfror0 = rfile(xf)
036600010612      *                            ========
036700010608     c                   write     wfror000
036800010612      *                            ========
036900010612     c                   clear                   dswfror0
037000010608     c                   endif
037100010608     c                   endif
037200010608
037300010608     c                   enddo
037400010621      * stampo
037500010621      *                            =========
037600010621     c                   exsr      SR_STAMPA
037700010621      *                            =========
037800010605
037900010608      * fine programma
038000010605     c                   eval      *inlr = *on
038100010605
038200010604      *--------------------------------------------------------------*
038300010604      * PREPARA LA STAMPA DELLA VIDEATA DELLE SELEZIONI
038400010604      *--------------------------------------------------------------*
038500010604     c     PRESELEZ      begsr
038600010608
038700010608      * se vengono creati i dati, li devo anche cancellare prima
038800010608     c                   if        i40tpel= 'F'
038900010608
039000010612     c                   eval      xa = 1
039100020807     c                   do        250           xa
039200010612     c                   if        skpoel(xa) = *zeros
039300010608     c                   leave
039400010608     c                   endif
039500010608
039600010611     c     *iso          movel     i40dtda       wdtda
039700010611     c     *iso          movel     i40dtal       wdtal
039800010620
039900010608     c                   dow       wdtda<=wdtal
040000010612     c                   eval      krorpoe = skpoel(xa)
040100010608     c                   move      wdtda         krordta
040200030919     c     kror1         setll     wfror01l
040300010608     c                   if        %equal(wfror01l)
040400030919     c     kror1         reade     wfror01l
040500010613     c                   dow       not%eof(wfror01l)
040600010608     c                   delete    wfror000
040700030919     c     kror1         reade     wfror01l
040800010608     c                   enddo
040900010608     c                   endif
041000010608     c
041100010608     c                   adddur    1:*d          wdtda
041200010608     c                   enddo
041300010608
041400010608     c                   enddo
041500010608     c                   endif
041600010618
041700010618      * stampo la prima pagina con le selezioni
041800010618     c                   eval      *in12 = (i40sede = 'S')
041900010618
042000010618     c                   if        i40poel = 999
042100070418     c                   eval      despo = 'Tutte le fil.dell''AS'
042200010618     c                   else
042300010618     c                   eval      kazorg = i40poel
042400010618     c     kazorg        chain     azorg01l
042500010618     c                   if        %found(azorg01l)
042600010618     c                   movel     orgdes        despo
042700010618     c                   endif
042800010618     c                   endif
042900010618
043000010618     c                   clear                   wlbdat
043100010618     c                   move      i40dtda       g02inv
043200010618     c                   movel     '3'           G02err
043300010618     c                   call      'XSRDA8'
043400010618     c                   parm                    wlbdat
043500010618     c                   movel     g02dat        datada
043600010618
043700010618     c                   clear                   wlbdat
043800010618     c                   move      i40dtal       g02inv
043900010618     c                   movel     '3'           G02err
044000010618     c                   call      'XSRDA8'
044100010618     c                   parm                    wlbdat
044200010618     c                   movel     g02dat        dataal
044300010618     c                   write     or41s1
044400010618
044500010621     c   11              write     or41s1c
044600010604     c                   endsr
044700010614
044800010614      *--------------------------------------------------------------*
044900010614      * STAMPA
045000010614      *--------------------------------------------------------------*
045100010614     c     SR_STAMPA     begsr
045200010621
045300010621      * leggo file di comodo per la stampa
045400010621     c     *loval        setll     wfror11l
045500010621 b1  c                   do        *hival
045600010621     c                   read      wfror11l
045700010621 b2  c                   if        %eof(wfror11l)
045800010621     c                   leave
045900010621 e2  c                   endif
046000030916
046100140707      /free
046200140707        IF  p_RORtrc = 'B' or p_RORtrc = 'C';
046300140707          *in15 = *on;
046400140707        ELSE;
046500140707          *in15 = *off;
046600140707        ENDIF;
046700140708        IF  p_RORtrc = 'C';
046800140708          *in17 = *on;
046900140708        ELSE;
047000140708          *in17 = *off;
047100140708        ENDIF;
047200140707       //?Se sto leggendo il rcd 'B' a questo devo sottrarre i dati del rcd 'C'
047300140707        IF  p_RORtrc = 'B';
047400140707          exec sql
047500140707          select * into :wfrords from wfror10f
047600140707          where rordae = :p_rordae and
047700140707                rordis = :p_rordis and
047800140707                rorare = :p_rorare and
047900140707                rorpoe = :p_rorpoe and
048000140707                rortrc = 'C';
048100140707       //?trovo il rcd corrispondente
048200140707          IF  sqlcod = 0;
048300140707         //?sottraggo
048400140707            p_RORoim -= c_RORoim;
048500140707            p_RORode -= c_RORode;
048600140707            p_RORoes -= c_RORoes;
048700140707            p_RORoea -= c_RORoea;
048800140707            p_RORone -= c_RORone;
048900140707            p_RORnqd -= c_RORnqd;
049000140707            p_RORnbl -= c_RORnbl;
049100140707            p_RORmdr -= c_RORmdr;
049200140707            p_RORnas -= c_RORnas;
049300140707            p_RORdan -= c_RORdan;
049400140707            p_RORean -= c_RORean;
049500140707            p_RORaan -= c_RORaan;
049600140707            p_RORnan -= c_RORnan;
049700140708            p_RORdit -= c_RORdit;
049800140708            p_ROReit -= c_ROReit;
049900140708            p_RORait -= c_RORait;
050000140708            p_RORnit -= c_RORnit;
050100140708            p_RORdr1 -= c_RORdr1;
050200140708            p_RORer1 -= c_RORer1;
050300140708            p_RORar1 -= c_RORar1;
050400140708            p_RORnr1 -= c_RORnr1;
050500140708            p_RORdr2 -= c_RORdr2;
050600140708            p_RORer2 -= c_RORer2;
050700140708            p_RORar2 -= c_RORar2;
050800140708            p_RORnr2 -= c_RORnr2;
050900140708            p_RORdro -= c_RORdro;
051000140708            p_RORero -= c_RORero;
051100140708            p_RORaro -= c_RORaro;
051200140708            p_RORnro -= c_RORnro;
051300140708            p_RORa09 -= c_RORa09;
051400140708            p_RORa01 -= c_RORa01;
051500140708            p_RORa02 -= c_RORa02;
051600140708            p_RORa03 -= c_RORa03;
051700140708            p_RORa04 -= c_RORa04;
051800140708            p_RORa05 -= c_RORa05;
051900140708            p_RORa06 -= c_RORa06;
052000140708            p_RORa07 -= c_RORa07;
052100140708            p_RORa08 -= c_RORa08;
052200140708            p_RORa10 -= c_RORa10;
052300140708            p_RORa11 -= c_RORa11;
052400140708            p_RORa12 -= c_RORa12;
052500140708            p_RORa13 -= c_RORa13;
052600140708            p_RORa14 -= c_RORa14;
052700140708            p_RORa15 -= c_RORa15;
052800140708            p_RORn01 -= c_RORn01;
052900140708            p_RORn02 -= c_RORn02;
053000140708            p_RORn03 -= c_RORn03;
053100140708            p_RORn04 -= c_RORn04;
053200140708            p_RORn05 -= c_RORn05;
053300140708            p_RORn06 -= c_RORn06;
053400140708            p_RORn07 -= c_RORn07;
053500140708            p_RORn08 -= c_RORn08;
053600140708            p_RORn09 -= c_RORn09;
053700140708            p_RORn10 -= c_RORn10;
053800140708            p_RORoco -= c_RORoco;
053900140708            p_RORico -= c_RORico;
054000140708            p_RORinc -= c_RORinc;
054100140708            p_RORifi -= c_RORifi;
054200140708            p_RORifian -= c_RORifian;
054300140708            p_RORifine -= c_RORifine;
054400140708            p_RORice -= c_RORice;
054500140708            p_RORicean -= c_RORicean;
054600140708            p_RORicene -= c_RORicene;
054700140708            p_RORiin -= c_RORiin;
054800140708            p_RORiinan -= c_RORiinan;
054900140708            p_RORiinne -= c_RORiinne;
055000140708            p_RORiva -= c_RORiva;
055100140708            p_RORivaan -= c_RORivaan;
055200140708            p_RORivane -= c_RORivane;
055300140708            p_RORicc -= c_RORicc;
055400140708            p_RORiccan -= c_RORiccan;
055500140708            p_RORiccne -= c_RORiccne;
055600140707          ENDIF;
055700140707        ENDIF;
055800140707      /end-free
055900010614
056000010705     ?* lancio fatto con un p.o. e più date devo stampare per p.o.
056100010621     ?* se richiesta la stampa sintetica
056200010621 b2  c                   if        *in13 = *on
056300010621      * rottura per p.o.
056400010621 b3  c                   if        sav_poel <> p_rorpoe
056500010621      * decodifico il p.o. che sto elaborando
056600010621     c                   eval      kazorg = p_rorpoe
056700010621     c     kazorg        chain     azorg01l
056800010621 b4  c                   if        %found(azorg01l)
056900010621     c                   movel     orgdes        despoel
057000010621 e4  c                   endif
057100010621      * decodifico il distretto
057200010621     c                   clear                   descrdis
057300010621     c                   movel     '17'          tblcod
057400010621     c                   movel(P)  p_rordis      tblkey
057500010621     c     ktab          chain     tabel00f
057600010621 b4  c                   if        %found(tabel00f)
057700010621     c                   movel     tbluni        descrdis
057800010621 e4  c                   endif
057900010621      * decodifico l'area
058000010621     c                   clear                   descrare
058100010621     c                   movel     '05'          tblcod
058200010621     c                   movel(P)  p_rorare      tblkey
058300010621     c     ktab          chain     tabel00f
058400010621 b4  c                   if        %found(tabel00f)
058500010621     c                   movel     tbluni        descrare
058600010621 e4  c                   endif
058700010621     c                   write     or41t1
058800010621     c                   write     or41t2
058900010621     c                   write     or41t3
059000010621     c                   write     or41t4
059100010621     c                   eval      sav_poel = p_rorpoe
059200010621 e3  c                   endif
059300010621      * giro la data di elaborazione
059400010621     c                   clear                   wlbdat
059500010621     c                   move      p_rordae      g02inv
059600010621     c                   movel     '3'           G02err
059700010621     c                   call      'XSRDA8'
059800010621     c                   parm                    wlbdat
059900010621     c                   movel     g02dat        dsdatap
060000010621      * salto pagina
060100010621 b3  c                   if        *in90 = *on
060200010621     c                   write     or41t1
060300010621     c                   write     or41t2
060400010621     c                   write     or41t3
060500010621     c                   write     or41t4
060600010621     c                   eval      *in90 = *off
060700010621 e3  c                   endif
060800010621      * calcolo la percentuale dei non eseguiti sui da eseguire
060900010705     c                   if        P_rorode > *zeros
061000010621     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
061100010705     c                   endif
061200030918     c  n15              write     or41d1
061300140708     c                   IF        *in15 and not *in17
061400140708     c                   write     or41d1b
061500140708     c                   ENDIF
061600140708     c                   IF        *in17
061700140708     c                   write     or41d1a
061800140708     c                   ENDIF
061900010621     ?* stampo per data
062000010621 x2  c                   else
062100010621      * rottura per data
062200010621 b3  c                   if        sav_data <> p_rordae
062300010614     c                   clear                   wlbdat
062400010621     c                   move      p_rordae      g02inv
062500010614     c                   movel     '3'           G02err
062600010614     c                   call      'XSRDA8'
062700010614     c                   parm                    wlbdat
062800010614     c                   movel     g02dat        dsdatap
062900010614     c                   write     or41t1
063000010614     c                   write     or41t2
063100010621     c                   eval      sav_data = p_rordae
063200010621     c                   clear                   sav_dis
063300010621     c                   clear                   sav_are
063400010622     c                   clear                   sav_poel
063500030922     c                   clear                   sav_trc
063600010621 e3  c                   endif
063700010621      * rottura per distretto
063800010621 b3  c                   if        sav_dis <> p_rordis
063900010621      * distretto
064000010621     c                   clear                   descrdis
064100010621     c                   movel     '17'          tblcod
064200010621     c                   movel(P)  p_rordis      tblkey
064300010621     c     ktab          chain     tabel00f
064400010621 b4  c                   if        %found(tabel00f)
064500010621     c                   movel     tbluni        descrdis
064600010621 e4  c                   endif
064700010621      * area
064800010621     c                   clear                   descrare
064900010621     c                   movel     '05'          tblcod
065000010621     c                   movel(P)  p_rorare      tblkey
065100010621     c     ktab          chain     tabel00f
065200010621 b4  c                   if        %found(tabel00f)
065300010621     c                   movel     tbluni        descrare
065400010621 e4  c                   endif
065500010621     c                   if        sav_dis <> *blanks
065600010621     c                   write     or41t1
065700010621     c                   write     or41t2
065800010621     c                   endif
065900010621     c                   write     or41t3
066000010621     c                   write     or41t4
066100010621     c                   eval      sav_dis = p_rordis
066200010621     c                   eval      sav_are = p_rorare
066300010622     c                   clear                   sav_poel
066400031002     c                   clear                   sav_trc
066500010621 e3  c                   endif
066600010621      * rottura per area
066700010621 b3  c                   if        sav_are <> p_rorare
066800010621      * area
066900010621     c                   clear                   descrare
067000010621     c                   movel     '05'          tblcod
067100010621     c                   movel(P)  p_rorare      tblkey
067200010621     c     ktab          chain     tabel00f
067300010621 b4  c                   if        %found(tabel00f)
067400010621     c                   movel     tbluni        descrare
067500010621 e4  c                   endif
067600010621     c                   if        sav_are <> *zeros
067700010621     c                   write     or41t1
067800010621     c                   write     or41t2
067900010621     c                   endif
068000010621     c                   write     or41t3
068100010621     c                   write     or41t4
068200010621     c                   eval      sav_are = p_rorare
068300010622     c                   clear                   sav_poel
068400031002     c                   clear                   sav_trc
068500010621 e3  c                   endif
068600030922      * rottura per tipo record
068700030922      * se richiesta la stampa analitica
068800030922      * dopo la pagina dei totali x p.o. devo stampare i commissionati
068900030922      * se richiesta la stampa sintetica
069000030922      * dopo la riga dei totali x p.o. devo stampare i commissionati
069100030922 b3  c                   if        sav_trc <> p_rortrc
069200030922      * se non è il primo tipo record che leggo (quindi non è quello totale)
069300030922      * pulisco il sav_poel in modo da avere la rottura x p.o.
069400030922      * e stampare i commissionati in una pagina nuova
069500031002 b4  c                   If        sav_trc <> *Blanks
069600031002 b5  c                   If        i40tpst = 'A'
069700030922      * salto pagina
069800030922     c                   write     or41t1
069900030922     c                   write     or41t2
070000030922     c                   write     or41t3
070100030922     c                   write     or41t4
070200031002 e5  c                   EndIf
070300030922     c                   Clear                   sav_poel
070400031002 e4  c                   EndIf
070500030922     c                   Eval      sav_trc = p_rortrc
070600030922 e3  c                   EndIf
070700010621      * rottura per p.o.
070800010621 b3  c                   if        sav_poel <> p_rorpoe
070900010614      * decodifico il p.o. che sto elaborando
071000010621     c                   eval      kazorg = p_rorpoe
071100010614     c     kazorg        chain     azorg01l
071200010621 b4  c                   if        %found(azorg01l)
071300010614     c                   movel     orgdes        despoel
071400010621 e4  c                   endif
071500010614     ?* stampa sintetica
071600010621 b4  c                   if        i40tpst = 'S'
071700010614      * salto pagina
071800010621 b5  c                   if        *in90 = *on
071900010621     c                   write     or41t1
072000010621     c                   write     or41t2
072100010621     c                   write     or41t3
072200010621     c                   write     or41t4
072300010614     c                   eval      *in90 = *off
072400010621 e5  c                   endif
072500030922      * calcolo la percentuale dei non eseguiti sui da eseguire
072600030922     c                   clear                   perone
072700030922     c                   if        P_rorode > *zeros
072800030922     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
072900030922     c                   endif
073000030922      * stampo totali p.o.
073100030922     c  n15              write     or41d1
073200140708     c                   IF        *in15 and not *in17
073300140708     c                   write     or41d1b
073400140708     c                   ENDIF
073500140708     c                   IF        *in17
073600140708     c                   write     or41d1a
073700140708     c                   ENDIF
073800010621 e4  c                   endif
073900010622     ?* stampa analitica
074000010622 b4  c                   if        i40tpst = 'A'
074100010622      * salto pagina
074200010622 b5  c                   if        sav_poel <> *zeros
074300010622     c                   write     or41t1
074400010622     c                   write     or41t2
074500010622     c                   write     or41t3
074600010622     c                   write     or41t4
074700010622 e5  c                   endif
074800010614      * calcolo la percentuale dei non eseguiti sui da eseguire
074900010625     c                   clear                   perone
075000010625     c                   if        P_rorode > *zeros
075100010621     c                   eval(h)   perone = (p_rorone * 100 / p_rorode)
075200010625     c                   endif
075300010622      * stampo totali p.o.
075400010614     c                   write     or41d1
075500010622      * stampo dettaglio
075600010614      *                            =======
075700010614     c                   exsr      SR_DETT
075800010614      *                            =======
075900010621 e4  c                   endif
076000010621     c                   eval      sav_poel = p_rorpoe
076100010621 e3  c                   endif
076200010621
076300010621 e2  c                   endif
076400010621
076500010621 e1  c                   enddo
076600010621
076700010621      * fine stampa
076800010621     c                   write     or41e1
076900010614
077000010614     c                   endsr
077100010614
077200010614      *--------------------------------------------------------------*
077300010614      * STAMPO IL DETTAGLIO
077400010614      *--------------------------------------------------------------*
077500010614     c     SR_DETT       begsr
077600010614
077700010614     c                   eval      wokstpcau = *off
077800010614
077900010614      * affidabilità
078000010614     c                   write     or41a1
078100010614
078200010621     c                   if        p_rordan > 0 or p_rorean > 0 or
078300010621     c                             p_roraan > 0 or p_rornan > 0
078400010614     c                   write     or41a11
078500010614     c                   endif
078600010621     c                   if        p_rordit > 0 or p_roreit > 0 or
078700010621     c                             p_rorait > 0 or p_rornit > 0
078800010614     c                   write     or41a12
078900010614     c                   endif
079000010621     c                   if        p_rordr1 > 0 or p_rorer1 > 0 or
079100010621     c                             p_rorar1 > 0 or p_rornr1 > 0
079200010614     c                   write     or41a13
079300010614     c                   endif
079400010621     c                   if        p_rordr2 > 0 or p_rorer2 > 0 or
079500010621     c                             p_rorar2 > 0 or p_rornr2 > 0
079600010614     c                   write     or41a14
079700010614     c                   endif
079800010621     c                   if        p_rordro > 0 or p_rorero > 0 or
079900010621     c                             p_roraro > 0 or p_rornro > 0
080000010614     c                   write     or41a15
080100010614     c                   endif
080200031105
080300031105      * O.R.M. non abbinati
080400031105     c                   If        p_rora09 > *Zeros
080500031105     c                   write     or41a29
080600031105     c                   endif
080700010614
080800010614      * causali es.anomali
080900030919     c                   if        p_rora01 > 0 or p_rora02 > 0 or
081000010621     c                             p_rora03 > 0 or p_rora04 > 0 or
081100010621     c                             p_rora05 > 0 or p_rora06 > 0 or
081200010621     c                             p_rora07 > 0 or p_rora08 > 0 or
081300010621     c                             p_rora09 > 0 or p_rora10 > 0 or
081400010621     c                             p_rora11 > 0 or p_rora12 > 0 or
081500010621     c                             p_rora13 > 0 or p_rora14 > 0 or
081600030919     c                             p_rora15 > 0
081700010614     c                   eval      wokstpcau = *on
081800010614     c                   write     or41a2
081900030919     c                   if        p_rora01 > 0
082000010614     c                   write     or41a21
082100010614     c                   endif
082200030919     c                   if        p_rora02 > 0
082300010614     c                   write     or41a22
082400010614     c                   endif
082500030919     c                   if        p_rora03 > 0
082600010614     c                   write     or41a23
082700010614     c                   endif
082800030919     c                   if        p_rora04 > 0
082900010614     c                   write     or41a24
083000010614     c                   endif
083100030919     c                   if        p_rora05 > 0
083200010614     c                   write     or41a25
083300010614     c                   endif
083400030919     c                   if        p_rora06 > 0
083500010614     c                   write     or41a26
083600010614     c                   endif
083700030919     c                   if        p_rora07 > 0
083800010614     c                   write     or41a27
083900010614     c                   endif
084000030919     c                   if        p_rora08 > 0
084100010614     c                   write     or41a28
084200010614     c                   endif
084300030919     c                   if        p_rora10 > 0
084400010614     c                   write     or41a210
084500010614     c                   endif
084600030919     c                   if        p_rora11 > 0
084700010614     c                   write     or41a211
084800010614     c                   endif
084900030919     c                   if        p_rora12 > 0
085000010614     c                   write     or41a212
085100010614     c                   endif
085200030919     c                   if        p_rora13 > 0
085300010614     c                   write     or41a213
085400010614     c                   endif
085500030919     c                   if        p_rora14 > 0
085600010614     c                   write     or41a214
085700010614     c                   endif
085800030919     c                   if        p_rora15 > 0
085900010614     c                   write     or41a215
086000010614     c                   endif
086100010614     c                   endif
086200010614
086300010614      * causali non eseguiti
086400010621     c                   if        p_rorn01 > 0 or p_rorn02 > 0 or
086500010621     c                             p_rorn03 > 0 or p_rorn04 > 0 or
086600010621     c                             p_rorn05 > 0 or p_rorn06 > 0 or
086700010621     c                             p_rorn07 > 0 or p_rorn08 > 0 or
086800030916     c                             p_rorn09 > 0 or p_rorn10 > 0 or
086900030916     c                             p_rorn01d > 0 or p_rorn02d > 0 or
087000030916     c                             p_rorn03d > 0 or p_rorn04d > 0 or
087100030916     c                             p_rorn05d > 0 or p_rorn06d > 0 or
087200030916     c                             p_rorn07d > 0 or p_rorn08d > 0 or
087300030916     c                             p_rorn09d > 0 or p_rorn10d > 0
087400010614     c                   if        wokstpcau = *on
087500010614     c                   write     or41a3
087600010614     c                   else
087700010614     c                   write     or41a2
087800010614     c                   endif
087900030916     c                   if        p_rorn01 > 0 or p_rorn01d > 0
088000010614     c                   write     or41a31
088100010614     c                   endif
088200030916     c                   if        p_rorn02 > 0 or p_rorn02d > 0
088300010614     c                   write     or41a32
088400010614     c                   endif
088500030916     c                   if        p_rorn03 > 0 or p_rorn03d > 0
088600010614     c                   write     or41a33
088700010614     c                   endif
088800030916     c                   if        p_rorn04 > 0 or p_rorn04d > 0
088900010614     c                   write     or41a34
089000010614     c                   endif
089100030916     c                   if        p_rorn05 > 0 or p_rorn05d > 0
089200010614     c                   write     or41a35
089300010614     c                   endif
089400030916     c                   if        p_rorn06 > 0 or p_rorn06d > 0
089500010614     c                   write     or41a36
089600010614     c                   endif
089700030916     c                   if        p_rorn07 > 0 or p_rorn07d > 0
089800010614     c                   write     or41a37
089900010614     c                   endif
090000030916     c                   if        p_rorn08 > 0 or p_rorn08d > 0
090100010614     c                   write     or41a38
090200010614     c                   endif
090300030916     c                   if        p_rorn09 > 0 or p_rorn09d > 0
090400010614     c                   write     or41a39
090500010614     c                   endif
090600030916     c                   if        p_rorn10 > 0 or p_rorn10d > 0
090700010614     c                   write     or41a310
090800010614     c                   endif
090900010614     c                   endif
091000010614
091100030919      * queste righe non le devo stampare x i soli ORM commissionati
091200030919     c                   If        not *In15
091300010614      * commissionati
091400010621     c                   if        p_roroco > 0
091500010614     c                   write     or41a4
091600010614     c                   endif
091700010614
091800010614      * tipo cliente
091900010614     c                   write     or41a5
092000010621     c                   if        p_rorico > 0
092100010614     c                   write     or41a51
092200010614     c                   endif
092300010621     c                   if        p_rorinc > 0
092400010614     c                   write     or41a52
092500010614     c                   endif
092600010614
092700010614      * tipo comunicazione
092800010614     c                   write     or41a6
092900030916     c                   if        p_rorifi > 0 or p_rorifian > 0 or
093000030916     c                             p_rorifid > 0 or p_rorifine  > 0
093100010614     c                   write     or41a61
093200010614     c                   endif
093300030916     c                   if        p_rorice > 0 or p_roricean > 0 or
093400030916     c                             p_roriced > 0 or p_roricene  > 0
093500010614     c                   write     or41a62
093600010614     c                   endif
093700030916     c                   if        p_roriin > 0 or p_roriinan > 0 or
093800030916     c                             p_roriind > 0 or p_roriinne  > 0
093900010614     c                   write     or41a63
094000010614     c                   endif
094100030916     c                   if        p_roriva > 0 or p_rorivaan > 0 or
094200030916     c                             p_rorivad > 0 or p_rorivane  > 0
094300010614     c                   write     or41a64
094400010614     c                   endif
094500030916     c                   if        p_roricc > 0 or p_roriccan > 0 or
094600030916     c                             p_roriccd > 0 or p_roriccne  > 0
094700010614     c                   write     or41a65
094800010614     c                   endif
094900030919
095000030919     c                   EndIF
095100010614
095200010614     c                   endsr
095300010614
095400020807      *--------------------------------------------------------------*
095500020807      * SQL  - Preparazione sql FNORM00F
095600020807      *--------------------------------------------------------------*
095700020807     c     SqlOrmPos     BegSr
095800020807
095900020808      * Preparo la stringa sql
096000020808     c                   Eval      StringaSql =
096100020808     c                             'select * '                                 +
096200020808     c                             'from fnorm00f where ormpoe in ('
096300020808      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
096400020808     c                   Do        250           Xa
096500020808     c                   If        Skpoel(Xa) = *Zeros
096600020808     c                   Leave
096700020808     c                   EndIf
096800020808     c                   Eval      StringaSql =
096900020808     c                             %trim(StringaSql) +  ' ' +
097000020808     c                             %editc(Skpoel(Xa):'1') + ','
097100020808     c                   EndDo
097200020808     c                   Eval      StringaSql =
097300020808     c                             %trim(StringaSql) + ') or ormpor in ('
097400020808      * ciclo sulla sk p.o. da elaborare per i p.o. ritiro
097500020808     c                   Do        250           Xa
097600020808     c                   If        Skpoel(Xa) = *Zeros
097700020808     c                   Leave
097800020808     c                   EndIf
097900020808     c                   Eval      StringaSql =
098000020808     c                             %trim(StringaSql) + ' ' +
098100020808     c                             %editc(Skpoel(Xa):'1') + ','
098200020808     c                   EndDo
098300020808     c                   Eval      StringaSql =
098400020808     c                             %trim(StringaSql) + ') for fetch only'
098500020807
098600020807     C/EXEC SQL
098700020808     C+ prepare S1 from :StringaSql
098800020807     C/END-EXEC
098900020807
099000020808     C/EXEC SQL
099100020808     C+ declare C1 cursor for S1
099200020808     C/END-EXEC
099300020807
099400020807     c                   EndSr
099500020807
099600010604      *--------------------------------------------------------------*
099700020808      * LEGGO IL FILE DEGLI ORM DA SQL
099800010604      *--------------------------------------------------------------*
099900010614     c     SR_LEGGI      begsr
100000010604
100100020808     C/EXEC SQL
100200020808     C+ Open C1
100300020808     C/END-EXEC
100400020808
100500020808     C/EXEC SQL
100600020808     C+ Fetch C1 into :fnormxxds
100700020808     C/END-EXEC
100800020808
100900020808 b1  c                   Dow       Sqlcod = 0                                   *record letto
101000010608      * no gli annullati
101100020808 b2  c                   If        Ormatb = *Blanks
101200010608
101300020808      * data immissione ORM non deve essere superiore alla data elaborazione
101400020808 b3  c                   If        Ormdao <= Dataela
101500030321
101600030321      * se sono in filiale elaboro tutto, se sono in sede devo controllare anche la
101700030321      * data pulizia ORM di filiale
101800030321     c                             and (I40Sede = 'F'  or
101900030321     c                                 (I40Sede <> 'F' and Ormdao >= I40dtpo))
102000031002
102100031002      * controllo se posso elaborare la serie
102200031002     c                   Move      OrmPoe        wOrmPoe
102300130118     c*******            Move      OrmNsr        wOrmNsr
102400130118     c                   eval      *in99 = *off
102500130118     c                   IF        ORMnsr > 0
102600130118     c                   eval      wORMcor =
102700130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
102800130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
102900130118     c                   IF        xx = 0
103000130118     c                   eval      *in99 = *on
103100130118     c                   ENDIF
103200131105     c                   IF        not *in99
103300131105     c                   IF        skstaosr(xx) <> *blanks
103400130118     c                   eval      *in99 = *on
103500130118     c                   ENDIF
103600131105     c                   ENDIF
103700130118     c                   ENDIF
103800130118     c*****wosr          Lookup    skosr                                  99
103900031002 b4  c                   If        Not *In99
104000020808
104100020808      *                            ==========
104200020808     c                   ExSr      Sr_Elabora
104300020808      *                            ==========
104400031002 b4  c                   EndIf
104500020808 b3  c                   EndIf
104600020808 b2  c                   EndIf
104700020808
104800020808     C/EXEC SQL
104900020808     C+ Fetch C1 into :fnormxxds
105000020808     C/END-EXEC
105100020808
105200020808 b1  c                   EndDo
105300020808
105400020808     C/EXEC SQL
105500020808     C+ close C1
105600020808     C/END-EXEC
105700020808
105800020808     c                   EndSr
105900010614
106000020808      *--------------------------------------------------------------*
106100020808      * LEGGO IL FILE DEGLI ORM
106200020808      *--------------------------------------------------------------*
106300020808     c     Sr_LeggiOrm   BegSr
106400020808
106500020808     c     *Start        Setll     Fnorm00f
106600020808     c                   Do        *Hival
106700020808     c                   Read      Fnorm00f
106800020808      * Fine file
106900020808 b1  c                   If        %Eof(Fnorm00f)
107000020808     c                   Leave
107100020808     c                   EndIf
107200020808      * no gli annullati
107300020808 b2  c                   If        Ormatb = *Blanks
107400020808
107500020808      * data immissione ORM non deve essere superiore alla data elaborazione
107600020808 b3  c                   If        Ormdao <= Dataela
107700030321
107800030321      * e controllo anche la data pulizia ORM di filiale
107900030321     c                             and Ormdao >= I40dtpo
108000031002
108100031002      * controllo se posso elaborare la serie
108200031002     c                   Move      OrmPoe        wOrmPoe
108300130118     c*******            Move      OrmNsr        wOrmNsr
108400130118     c                   eval      *in99 = *off
108500130118     c                   IF        ORMnsr > 0
108600130118     c                   eval      wORMcor =
108700130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
108800130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
108900130118     c                   IF        xx = 0
109000130118     c                   eval      *in99 = *on
109100130118     c                   ENDIF
109200130118     c                   IF        skstaosr(xx) <> *blanks
109300130118     c                   eval      *in99 = *on
109400130118     c                   ENDIF
109500130118     c                   ENDIF
109600130118     c*****wosr          Lookup    skosr                                  99
109700031002 b4  c                   If        Not *In99
109800020808
109900020808      *                            ==========
110000020808     c                   ExSr      Sr_Elabora
110100020808      *                            ==========
110200031002 b4  c                   EndIf
110300020808 b3  c                   EndIf
110400020808 b2  c                   EndIf
110500020808
110600020808 b1  c                   EndDo
110700020808
110800020808     c                   EndSr
110900020808
111000020808      *--------------------------------------------------------------*
111100020808      * ELABORO I DATI LETTI
111200020808      *--------------------------------------------------------------*
111300020808     c     Sr_Elabora    BegSr
111400020808
111500020808      * campi di appoggio
111600020808     c                   eval      xf = 1
111700020808     c                   eval      wpoe = *off
111800020808     c                   eval      wpor = *off
111900020808     c                   eval      wokstp = *off
112000020808     c                   eval      wok900 = *off
112100020808     c                   eval      wok600 = *off
112200020808     c                   eval      wokrode = *off
112300020808     c                   eval      wokroes = *off
112400020808     c                   eval      wokrfile = *off
112500020808     c                   eval      wokfestae = *off
112600020808     c                   eval      wokfestar = *off
112700020808
112800020808     c                   clear                   cauone
112900020808     c                   clear                   cauoea
113000030912
113100030912     c                   Eval      dorm01 = OrmFlo
113200030912      * ORM commissionato
113300030918     c                   Eval      *in14 = (§OrCom = 'S')
113400140707      /free
113500140707         *in16 = *off;
113600140707       //?ORM commissionato con Alert
113700140707         fior900ds.IOR900trc = 'A';
113800140707         fior900ds.IOR900rcdm = 'MA';
113900140707         fior900ds.IOR900rcds = 'S ';
114000140707         fior900ds.IOR900poe = ORMpoe;
114100140707         fior900ds.IOR900nsr = ORMnsr;
114200140707         fior900ds.IOR900nor = ORMnor;
114300140707         fior900ds.IOR900nrv = ORMnrv;
114400140707         fior900r (fior900ds);
114500140707         IF  fior900ds.OOR900err <> 'N';
114600140707           *in16 = *on;
114700140707         ENDIF;
114800140707      /end-free
114900020808
115000020808      * controllo se il po emissione è nella sk dei p.o. da elaborare
115100020808     c                   eval      xa = 1
115200020808     c     Ormpoe        Lookup    Skpoel(Xa)                             30
115300020808     c                   If        *In30
115400020808     c                   eval      wpoe = *on
115500020808     c                   eval      dspoele = skpoel(xa)
115600020808     c                   endif
115700020808
115800020808      * controllo se il po ritiro è nella sk dei p.o. da elaborare
115900020808     c                   eval      xa = 1
116000020808     c     Ormpor        Lookup    Skpoel(Xa)                             30
116100020808     c                   If        *In30
116200020808     c                   eval      wpor = *on
116300020808     c                   eval      dspoelr = skpoel(xa)
116400020808     c                   endif
116500020808
116600020808      * se la data che sto elaborando è un festivo non devo calcolare
116700020808      *                            ========
116800020808     c                   exsr      SR_FESTA
116900020808      *                            ========
117000020808
117100020808     c                   if        wokfestae = *off and
117200020808     c                             wokfestar = *off
117300020808     c                   eval      dsdata = dataela
117400020808     c                   endif
117500020808
117600020808      * calcolo le date per affidabilità
117700020808      *                            =======
117800020808     c                   exsr      SR_DATA
117900020808      *                            =======
118000020808
118100020808      * pulisco i campi di comodo
118200020808      *                            ==========
118300020808     c                   exsr      SR_PULISCI
118400020808      *                            ==========
118500020808
118600020808      * faccio i controlli con il p.o. emissione
118700020808 b2  c                   if        wpoe = *on
118800020808      * se festa non faccio niente
118900020808 b3  c                   if        wokfestae = *off
119000020808      *                            ======
119100020808     c                   exsr      SR_POE
119200020808      *                            ======
119300020808 e3  c                   endif
119400020808 e2  c                   endif
119500020808
119600020808      * faccio i controlli con il p.o. ritiro
119700020808 b2  c                   if        wpor = *on
119800020808      * se festa non faccio niente
119900020808 b3  c                   if        wokfestae = *off
120000020808
120100020808      *                            ======
120200020808     c                   exsr      SR_POR
120300030916
120400030916 b4  c                   if        wokrfile = *on
120500020808      *                            ======
120600030918      * RECORD 'A'
120700020808      * controllo se esiste già un elemento di sk = alla mia key
120800020808      * e carico la sk relativa al file
120900020808     c                   eval      dspoel = dspoelr
121000030912     c                   eval      dspoel1 = dspoelr
121100030912     c                   eval      dsdata1 = dsdata
121200030918     c                   Eval      dstrc = 'A'
121300020808      *                            ==========
121400020808     c                   exsr      SR_CONTRSK
121500020808      *                            ==========
121600020808      * carico la sk del file
121700020808      *                            =========
121800020808     c                   exsr      SR_CARICA
121900020808      *                            =========
122000030918      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
122100030918     c                   Eval      sxf = xf
122200030918      *
122300140707      * RECORD 'B' ORM COMMISSIONATI  (ALERT + TELEFONO)
122400030912      * controllo se esiste già un elemento di sk = alla mia key
122500030912      * e carico la sk relativa al file
122600030916 b5  c                   If        *In14
122700030918     c                   Eval      dstrc = 'B'
122800030912      *                            ==========
122900030912     c                   exsr      SR_CONTRSK
123000030912      *                            ==========
123100030918      * carico la sk del file x record 'B' orm commissionato
123200140707      * Telefono + Alert
123300030912      *                            =========
123400030912     c                   exsr      SR_CARICA
123500030912      *                            =========
123600030916 e5  c                   EndIf
123700140707      *
123800140707      * RECORD 'C' ORM COMMISSIONATI  (SOLO ALERT)
123900140707      * controllo se esiste già un elemento di sk = alla mia key
124000140707      * e carico la sk relativa al file
124100140707 b5  c                   If        *In16
124200140707     c                   Eval      dstrc = 'C'
124300140707      *                            ==========
124400140707     c                   exsr      SR_CONTRSK
124500140707      *                            ==========
124600140707      * carico la sk del file x record 'C' orm commissionato
124700140707      * solo Alert
124800140707      *                            =========
124900140707     c                   exsr      SR_CARICA
125000140707      *                            =========
125100140707 e5  c                   EndIf
125200020808 e4  c                   endif
125300020808 e3  c                   endif
125400020808 e2  c                   endif
125500030916      * ORM commissionato
125600030916     c                   If        *In14
125700030916     c                   Eval      commis = 'X'
125800030916     c                   Else
125900030916     c                   Clear                   commis
126000030916     c                   EndIf
126100140708      /free
126200140708       //?ORM commissionato con Alert
126300140708         IF  *in16;
126400140708           comalert = 'X';
126500140708         ELSE;
126600140708           clear comalert;
126700140708         ENDIF;
126800140708      /end-free
126900020808
127000020808     ?* stampa di comodo EDP
127100020808 b2  c   10              if        wokstp = *on
127200030918     c                   eval      dswfror = rfile(sxf)
127300020808      * rottura di data
127400020808 b3  c                   if        dsdata <> sav_dsdatap
127500020808     c                   clear                   wlbdat
127600020808     c                   move      dsdata        g02inv
127700020808     c                   movel     '3'           G02err
127800020808     c                   call      'XSRDA8'
127900020808     c                   parm                    wlbdat
128000020808     c                   movel     g02dat        dsdatap
128100020808      * decodifico il p.o.
128200020808     c                   eval      kazorg = dspoel
128300020808     c     kazorg        chain     azorg01l
128400020808 b4  c                   if        %found(azorg01l)
128500020808     c                   movel     orgdes        despoel
128600020808 e4  c                   endif
128700020808     c                   write     or41t1p
128800020808     c                   eval      sav_dsdatap = dsdata
128900020808     c                   eval      sav_dspoelp = dspoel
129000020808 e3  c                   endif
129100020808      * rottura di p.o.
129200020808 b3  c                   if        dspoel <> sav_dspoelp
129300020808      * decodifico il p.o.
129400020808     c                   eval      kazorg = dspoel
129500020808     c     kazorg        chain     azorg01l
129600020808 b4  c                   if        %found(azorg01l)
129700020808     c                   movel     orgdes        despoel
129800020808 e4  c                   endif
129900020808     c                   write     or41t1p
130000020808     c                   eval      sav_dspoelp = dspoel
130100020808 e3  c                   endif
130200020808      * salto pagina
130300020808 b3  c                   if        *in91 = *on
130400020808     c                   write     or41t1p
130500020808     c                   eval      *in91 = *off
130600020808 e3  c                   endif
130700020808      * dettaglio
130800020808     c                   write     or41d1p
130900020808 e2  c                   endif
131000020808
131100020808     ?* stampa di controllo
131200020808 b2  c   11              if        wokstp = *on
131300030918     c                   eval      dswfror = rfile(sxf)
131400020808      * rottura di data
131500020808 b3  c                   if        dsdata <> sav_dsdatac
131600020808     c                   clear                   wlbdat
131700020808     c                   move      dsdata        g02inv
131800020808     c                   movel     '3'           G02err
131900020808     c                   call      'XSRDA8'
132000020808     c                   parm                    wlbdat
132100020808     c                   movel     g02dat        dsdatap
132200020808      * decodifico il p.o.
132300020808     c                   eval      kazorg = dspoel
132400020808     c     kazorg        chain     azorg01l
132500020808 b4  c                   if        %found(azorg01l)
132600020808     c                   movel     orgdes        despoel
132700020808 e4  c                   endif
132800020808     c                   write     or41t1c
132900020808     c                   eval      sav_dsdatac = dsdata
133000020808     c                   eval      sav_dspoelc = dspoel
133100020808 e3  c                   endif
133200020808      * rottura di p.o.
133300020808 b3  c                   if        dspoel <> sav_dspoelc
133400020808      * decodifico il p.o.
133500020808     c                   eval      kazorg = dspoel
133600020808     c     kazorg        chain     azorg01l
133700020808 b4  c                   if        %found(azorg01l)
133800020808     c                   movel     orgdes        despoel
133900020808 e4  c                   endif
134000020808     c                   write     or41t1c
134100020808     c                   eval      sav_dspoelc = dspoel
134200020808 e3  c                   endif
134300020808      * salto pagina
134400020808 b3  c                   if        *in92 = *on
134500020808     c                   write     or41t1c
134600020808     c                   eval      *in92 = *off
134700020808 e3  c                   endif
134800020808 b3  c                   if        w_rorone <> *blanks or
134900020808     c                             w_rornqd <> *blanks or
135000020808     c                             w_rornbl <> *blanks or
135100020808     c                             w_rormdr <> *blanks or
135200031105     c                             w_rornas <> *blanks
135300020808      * giro data immissione ORM
135400020808     c                   clear                   wlbdat
135500020808     c                   move      ormdao        g02inv
135600020808     c                   movel     '3'           G02err
135700020808     c                   call      'XSRDA8'
135800020808     c                   parm                    wlbdat
135900020808     c                   movel     g02dat        w_ormdao
136000020808      * dettaglio
136100020808     c                   write     or41d1c
136200020808 e3  c                   endif
136300020808 e2  c                   endif
136400020808
136500020808     c                   EndSr
136600020808
136700010612      *--------------------------------------------------------------*
136800010612      * CONTROLLO SE LA DATA CHE STO ELABORANDO E' FESTIVO
136900010612      *--------------------------------------------------------------*
137000010614     c     SR_FESTA      begsr
137100010612
137200010612     c                   eval      wokfestae = *off
137300010612     c                   eval      wokfestar = *off
137400010612
137500010612     c     *iso          movel     dataela       wdtda
137600010612     c                   extrct    wdtda:*y      kann
137700010612     c                   extrct    wdtda:*m      kmes
137800010612     c                   extrct    wdtda:*d      wgiorno
137900011217     c                   clear                   ktfa
138000010612      * cerco per il p.o. emissione
138100010612     c                   if        wpoe = *on
138200011217     c                   eval      ktfp = dspoele
138300010612     c     kazcln        chain     azcln01l
138400010612     c                   if        %found(azcln01l) and
138500010612     c                             (MAT(wgiorno) =  'F'
138600010612     c                                    or  POM(wgiorno) =  'F')
138700010612     c                   eval      wokfestae = *on
138800010612     c                   endif
138900010612     c                   endif
139000010612
139100010612      * cerco per il p.o. ritiro
139200010612     c                   if        wpor = *on
139300011217     c                   eval      ktfp = dspoelr
139400010612     c     kazcln        chain     azcln01l
139500010612     c                   if        %found(azcln01l) and
139600010612     c                             (MAT(wgiorno) =  'F'
139700010612     c                                    or  POM(wgiorno) =  'F')
139800010612     c                   eval      wokfestar = *on
139900010612     c                   endif
140000010612     c                   endif
140100010612
140200010612     c                   endsr
140300010614
140400010608      *--------------------------------------------------------------*
140500010608      * CALCOLO DATE X AFFIDABILITA'
140600010608      *--------------------------------------------------------------*
140700010614     c     SR_DATA       begsr
140800010608
140900010612      * calcolo la data da - 1
141000010611     c     *iso          movel     dataela       wdtda
141100010608     c                   do        *hival
141200010608     c                   subdur    1:*d          wdtda
141300010608     c                   extrct    wdtda:*y      kann
141400010608     c                   extrct    wdtda:*m      kmes
141500010608     c                   extrct    wdtda:*d      wgiorno
141600011217     c                   clear                   ktfa
141700020808     c                   If        Dspoelr <> *Zeros
141800020808     c                   eval      ktfp = Dspoelr
141900020808     c                   Else
142000020808     c                   Eval      Ktfp = Dspoele
142100020808     c                   EndIf
142200010608     c     kazcln        chain     azcln01l
142300010608     c                   if        not%found(azcln01l)
142400010608     c                   leave
142500010608     c                   endif
142600010608     c                   if        MAT(wgiorno) =  'F'
142700010608     c                             or  POM(wgiorno) =  'F'
142800010608     c                   iter
142900010608     c                   endif
143000010608     c                   leave
143100010608     c                   enddo
143200010611     c                   move      wdtda         wdtda01
143300010608
143400010608      * calcolo la data da - 2
143500010611     c     *iso          movel     wdtda01       wdtda
143600010608     c                   do        *hival
143700010611     c                   subdur    1:*d          wdtda
143800010608     c                   extrct    wdtda:*y      kann
143900010608     c                   extrct    wdtda:*m      kmes
144000010608     c                   extrct    wdtda:*d      wgiorno
144100011217     c                   clear                   ktfa
144200020808     c                   If        Dspoelr <> *Zeros
144300020808     c                   eval      ktfp = Dspoelr
144400020808     c                   Else
144500020808     c                   Eval      Ktfp = Dspoele
144600020808     c                   EndIf
144700010608     c     kazcln        chain     azcln01l
144800010608     c                   if        not%found(azcln01l)
144900010608     c                   leave
145000010608     c                   endif
145100010608     c                   if        MAT(wgiorno) =  'F'
145200010608     c                             or  POM(wgiorno) =  'F'
145300010608     c                   iter
145400010608     c                   endif
145500010608     c                   leave
145600010608     c                   enddo
145700010611     c                   move      wdtda         wdtda02
145800010608
145900010608     c                   endsr
146000010614
146100010605      *--------------------------------------------------------------*
146200010605      * CONTROLLO CON IL PO EMISSIONE
146300010605      *--------------------------------------------------------------*
146400010605     c     SR_POE        begsr
146500010620
146600010620      * se la data del p.o. emissione non è festa controllare se il giorno prima
146700010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
146800010620 b1  c                   if        wokfestae = *off
146900010620     c                   eval      dspoel = dspoele
147000010620      *                            ========
147100010620     c                   exsr      SR_DTFES
147200010620      *                            ========
147300010612
147400010620 b2  c                   if        ormdao = dataela or
147500010620     c                             (wdatafes > 0 and ormdao >= wdatafes and
147600010620     c                              ormdao < dataela)
147700010611     c                   eval      dspoel = dspoele
147800030912     c                   eval      dspoel1= dspoele
147900030912     c                   eval      dsdata1 = dsdata
148000030918     c                   Eval      dstrc = 'A'
148100010608      * controllo se esiste già un elemento di sk = alla mia key
148200010608      * e carico la sk relativa al file
148300010614      *                            ==========
148400010608     c                   exsr      SR_CONTRSK
148500010614      *                            ==========
148600010605
148700010605     ?* immessi ?
148800010605     c                   eval      f_roroim = f_roroim +1
148900010605     ?* commissionati ?
149000010620 b3  c                   if        ormpoe <> ormpor
149100010605     c                   eval      f_roroco = f_roroco +1
149200010620 e3  c                   endif
149300010605     ?* codificati ?
149400010620 b3  c                   if        ormcra <> *zeros
149500010605     c                             or ormcor <> *zeros
149600010605     c                   eval      f_rorico = f_rorico +1
149700010605     ?* non codificati ?
149800010620 x3  c                   else
149900010605     c                   eval      f_rorinc = f_rorinc +1
150000010620 e3  c                   endif
150100010605     ?* fissi ?
150200010620 b3  c                   if        ormtco = 'S'
150300010605     c                   eval      f_rorifi = f_rorifi +1
150400010620 e3  c                   endif
150500010605     ?* da centralino ?
150600010620 b3  c                   if        ormtco = 'M'  or
150700010606     c                             ormtco = 'P'
150800010605     c                   eval      f_rorice = f_rorice +1
150900010620 e3  c                   endif
151000010605     ?* da internet ?
151100010620 b3  c                   if        ormtco = 'I'
151200010605     c                   eval      f_roriin = f_roriin +1
151300010620 e3  c                   endif
151400010605     ?* da vas ?
151500010620 b3  c                   if        ormtco = 'F'
151600010605     c                   eval      f_roriva = f_roriva +1
151700010620 e3  c                   endif
151800141212     ?* da mail ?
151900141212 b3  c                   if        ormtco = 'E'
152000010605     c                   eval      f_roricc = f_roricc +1
152100010620 e3  c                   endif
152200010605
152300010612     c                   eval      rfile(xf) = dswfror
152400010607     c                   eval      wokstp = *on
152500031202      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
152600031202     c                   Eval      sxf = xf
152700010620 e2  c                   endif
152800010605
152900010620 e1  c                   endif
153000010620
153100010605     c                   endsr
153200010614
153300010620      *--------------------------------------------------------------*
153400010620      * CERCO IL PRIMO GIORNO FESTIVO PRECEDENTE
153500010620      *--------------------------------------------------------------*
153600010620     c     SR_DTFES      begsr
153700010620
153800010620      * calcolo il primo giorno festivo
153900010620     c     *iso          movel     dataela       wdtda
154000010620     c                   do        *hival
154100010620     c                   subdur    1:*d          wdtda
154200010620     c                   extrct    wdtda:*y      kann
154300010620     c                   extrct    wdtda:*m      kmes
154400010620     c                   extrct    wdtda:*d      wgiorno
154500011217     c                   clear                   ktfa
154600011217     c                   eval      ktfp = dspoel
154700010620     c     kazcln        chain     azcln01l
154800010620     c                   if        not%found(azcln01l)
154900010620     c                   leave
155000010620     c                   endif
155100010620     c                   if        MAT(wgiorno) =  'F'
155200010620     c                             or  POM(wgiorno) =  'F'
155300010620     c                   move      wdtda         wdatafes
155400010620     c                   iter
155500010620     c                   endif
155600010620     c                   leave
155700010620     c                   enddo
155800010620
155900010620     c                   endsr
156000010620
156100010608      *--------------------------------------------------------------*
156200010608      * PULISCO CAMPI DI COMODO
156300010608      *--------------------------------------------------------------*
156400010614     c     SR_PULISCI    begsr
156500010608
156600010608     c                   clear                   wrorode
156700010620     c                   clear                   wrordan
156800010608     c                   clear                   wrordit
156900010608     c                   clear                   wrordr1
157000010608     c                   clear                   wrordr2
157100010608     c                   clear                   wrordro
157200010608     c                   clear                   wroroes
157300010614     c                   clear                   wrorean
157400010608     c                   clear                   wroreit
157500010608     c                   clear                   wrorer1
157600010608     c                   clear                   wrorer2
157700010608     c                   clear                   wrorero
157800010608     c                   clear                   wrorone
157900010620     c                   clear                   wrornan
158000010608     c                   clear                   wrornit
158100010608     c                   clear                   wrornr1
158200010608     c                   clear                   wrornr2
158300010608     c                   clear                   wrornro
158400010608     c                   clear                   wrorn01
158500010608     c                   clear                   wrorn02
158600010608     c                   clear                   wrorn03
158700010608     c                   clear                   wrorn04
158800010608     c                   clear                   wrorn05
158900010608     c                   clear                   wrorn06
159000010608     c                   clear                   wrorn07
159100010608     c                   clear                   wrorn08
159200010608     c                   clear                   wrorn09
159300010608     c                   clear                   wrorn10
159400010608     c                   clear                   wroroea
159500010620     c                   clear                   wroraan
159600010608     c                   clear                   wrorait
159700010608     c                   clear                   wrorar1
159800010608     c                   clear                   wrorar2
159900010608     c                   clear                   wroraro
160000010608     c                   clear                   wrora01
160100010608     c                   clear                   wrora02
160200010608     c                   clear                   wrora03
160300010608     c                   clear                   wrora04
160400010608     c                   clear                   wrora05
160500010608     c                   clear                   wrora06
160600010608     c                   clear                   wrora07
160700010608     c                   clear                   wrora08
160800010608     c                   clear                   wrora09
160900010608     c                   clear                   wrora10
161000010608     c                   clear                   wrora11
161100010608     c                   clear                   wrora12
161200010608     c                   clear                   wrora13
161300010608     c                   clear                   wrora14
161400010608     c                   clear                   wrora15
161500010608     c                   clear                   wrornqd
161600010608     c                   clear                   wrornbl
161700010614     c                   clear                   wrormdr
161800010614     c                   clear                   wrornas
161900030912     c                   clear                   wroroned
162000030912     c                   clear                   wrorn01d
162100030912     c                   clear                   wrorn02d
162200030912     c                   clear                   wrorn03d
162300030912     c                   clear                   wrorn04d
162400030912     c                   clear                   wrorn05d
162500030912     c                   clear                   wrorn06d
162600030912     c                   clear                   wrorn07d
162700030912     c                   clear                   wrorn08d
162800030912     c                   clear                   wrorn09d
162900030912     c                   clear                   wrorn10d
163000030917     c                   clear                   wrorifian
163100030917     c                   clear                   wrorifine
163200030917     c                   clear                   wrorifid
163300030917     c                   clear                   wroricean
163400030917     c                   clear                   wroricene
163500030917     c                   clear                   wroriced
163600030917     c                   clear                   wroriinan
163700030917     c                   clear                   wroriinne
163800030917     c                   clear                   wroriind
163900030917     c                   clear                   wrorivaan
164000030917     c                   clear                   wrorivane
164100030917     c                   clear                   wrorivad
164200030917     c                   clear                   wroriccan
164300030917     c                   clear                   wroriccne
164400030917     c                   clear                   wroriccd
164500010620
164600010621     c                   clear                   w_rorone
164700010621     c                   clear                   w_rornqd
164800010621     c                   clear                   w_rornbl
164900010621     c                   clear                   w_rornas
165000010621     c                   clear                   w_rormdr
165100010608
165200010608     c                   endsr
165300010614
165400010605      *--------------------------------------------------------------*
165500010605      * CONTROLLO CON IL PO RITIRO
165600010605      *--------------------------------------------------------------*
165700010605     c     SR_POR        begsr
165800010620
165900010620     c                   clear                   wdatafes
166000010620
166100010620      * se la data del p.o. ritiro non è festa controllare se il giorno prima
166200010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
166300010620 b1  c                   if        wokfestar = *off
166400010620     c                   eval      dspoel = dspoelr
166500010620      *                            ========
166600010620     c                   exsr      SR_DTFES
166700010620      *                            ========
166800010620 e1  c                   endif
166900010608
167000010608     c                   eval      kormpoe = ormpoe
167100010608     c                   eval      kormnsr = ormnsr
167200010608     c                   eval      kormnor = ormnor
167300010608     c                   eval      kormnrv = ormnrv
167400010612
167500010614      * se data p.o. ritiro non è festa cerco gli ORM da eseguire
167600010619      * e le modifiche della data di ritiro
167700010613 b1  c                   if        wokfestar = *off
167800010606
167900010614     ?* ORM da eseguire oggi data ritiro = data elaborazione
168000010618 b2  c                   if        ormdar = dataela
168100010618     ?* data ultima fase < data elaborazione e già eseguito non devo elaborarlo
168200010618     ?*                                                     -------------------
168300010618     c                   if        ormdfo < dataela and ormfao >= 600
168400010618     c                   goto      endpor
168500010618     c                   endif
168600010618     c                   eval      wrorode = wrorode +1
168700010607     c                   eval      wrordit = wrordit +1
168800010607     c                   eval      wokstp = *on
168900010612     c                   eval      wokrfile = *on
169000010614 e2  c                   endif
169100010608
169200010614     ?* ORM da eseguire (non eseguiti entro oggi) data ritiro < data elaborazione
169300010613 b2  c                   if        ormdar < dataela
169400010608     c                   eval      wokrode = *off
169500010620
169600010620     c                   eval      kormdfo = ormdfo
169700010620     c                   eval      kormofo = ormofo
169800010620     c                   eval      kormfao = ormfao
169900010620     c     korf01        setll     fnorf01l
170000010620      * se la data ultima fase "testata" è >= della data elaborazione
170100010620      * o è <  della data elaborazione
170200010620      *   e >= alla data festiva (se sto elaborando un lunedì)
170300010620      * devo leggere la fase prima
170400010620     c                   select
170500010620 b3  c                   when      ormdfo >= dataela or
170600010620 b3  c                             (ormdfo < dataela  and
170700010620     c                             (wdatafes > 0 and ormdfo >= wdatafes))
170800010620     c     korf          readpe    fnorf01l
170900010620      * se la data ultima fase "testata" è <  della data elaborazione
171000010620      * e non sto elaborando un lunedì
171100010620      * devo leggere la fase stessa
171200010620     c                   other
171300010620     c     korf          reade     fnorf01l
171400010620 e3  c                   endsl
171500010620
171600030807      * senza fase 600-900-910-999 fino alla data elaborazione esclusa
171700010614 b3  c                   dow       not %eof(fnorf01l)
171800010620 b4  c                   if        orfdae < dataela
171900010620 b5  c                   if        orffar >=600
172000011019      * Attenzione!! E' possibile che sia stata fatta una modifica fase
172100011019      * se la fase della testata ORM è 500 o è 100 e la fase precedente
172200011019      * sul file delle fasi è una fase di fatto DEVO dare l'ORM come
172300011019      * da eseguire
172400011019     c                   if        ormfao <> 500 and ormfao <> 100
172500010608     c                   leave
172600011019     c                   endif
172700010614 e5  c                   endif
172800060926      * salto le fasi che non devono essere elaborate
172900060926     c     orffar        Lookup    skfar                                  30
173000060926     c                   if        not *in30
173100010608     c                   eval      wokrode = *on
173200031105     c                   EndIf
173300010614 e4  c                   endif
173400010613     c     korf          readpe    fnorf01l
173500010614 e3  c                   enddo
173600010613     ?* da eseguire (in ritardo)
173700010613 b3  c                   if        wokrode = *on
173800010608     c                   eval      wrorode = wrorode +1
173900010608      * calcolo affidabilità. Uso >= per i da eseguire nei festivi
174000010614 b4  c                   select
174100010608     c                   when      ormdar >= wdtda01
174200010608     c                   eval      wrordr1 = wrordr1 +1
174300010608     c                   when      ormdar >= wdtda02
174400010608     c                   eval      wrordr2 = wrordr2 +1
174500010614 x4  c                   other
174600010608     c                   eval      wrordro = wrordro +1
174700010614 e4  c                   endsl
174800010608     c                   eval      wokstp = *on
174900010614     c                   eval      wokrfile = *on
175000010614     ?* è stato eseguito
175100010613 x3  c                   else
175200010614      * e la data ultima fase minore data elaborazione non devo elaborarlo
175300010614      *                                                -------------------
175400010620 b4  c                   if        ormdfo < dataela
175500010619     ?* ma prima controllo se deve andare a far parte dei non bollettati
175600010619     ?*                                                   --------------
175700010619 b5  c                   if        ormfao = 600 and
175800010619     c                             ormdfo >= wdtda01
175900010619     c                   eval      wrornbl = wrornbl +1
176000010621     c   11              eval      w_rornbl = 'X'
176100010619     c                   eval      wokstp = *on
176200010619     c                   eval      wokrfile = *on
176300010619 e5  c                   endif
176400010608     c                   goto      endpor
176500010614 e4  c                   endif
176600010613 e3  c                   endif
176700010613 e2  c                   endif
176800040420
176900040420     ?* ORM con data ritiro > data elaborazione
177000040420      * devo controllare che non sia stata fatta una fase di dirottamento dalla data elaborazione
177100040420      * in poi
177200040420if  2c                   If        OrmDar > Dataela
177300040420     c                   Eval      kormfao = 200
177400040420     c                   Eval      kormdfo = Dataela
177500040420     c     kOrf03        Setll     Fnorf03l
177600040420      * Se trovo già una fase di dirottamento in data elaborazione non devo elabore l'ORM
177700040420      *                                                            ----------------------
177800040420     c                   If        %Equal(Fnorf03l)
177900040420     c                   Goto      endpor
178000040420     c                   EndIf
178100040615      * Altrimenti leggo dalla data statistica in poi
178200040420do  3c                   Do        *Hival
178300040420     c     kOrf04        Reade     Fnorf03l
178400040420     c                   If        %Eof(Fnorf03l)
178500040420     c                   Leave
178600040420     c                   EndIf
178700040420      * Se trovo una fase di dirottamento non devo elabore l'ORM
178800040420      *                                   ----------------------
178900040420     c                   Goto      endpor
179000040420e   3c                   EndDo
179100040420e   2c                   EndIf
179200010618
179300010619      * cerco le modifiche alla data ritiro
179400010618      *                            =========
179500010618     c                   exsr      SR_MODRIT
179600010618      *                            =========
179700010619
179800010619 e1  c                   endif
179900010608
180000010614      * cerco sulle fasi sempre
180100010614      *                  ------
180200010614      *                            =======
180300010606     c                   exsr      SR_FASI
180400010614      *                            =======
180500010608
180600010608     c     endpor        endsr
180700010618
180800010618      *--------------------------------------------------------------*
180900010618      * CALCOLO MODIFICHE DATA RITIRO
181000010618      *--------------------------------------------------------------*
181100010618     c     SR_MODRIT     begsr
181200010618
181300010618     c                   eval      korvdtv = dataela
181400010620     c                   if        wdatafes > 0
181500010620     c                   eval      korvdtv = wdatafes
181600010620     c                   endif
181700010618     c                   clear                   sav_orvdar
181800010618
181900010620     c     korv          setll     fnorv01l
182000010618     c                   do        *hival
182100010620     c     korf          reade     fnorv01l
182200010618     c                   if        %eof(fnorv01l)
182300010618     c                   leave
182400010618     c                   endif
182500010620     c                   if        orvdtv > dataela
182600010620     c                   leave
182700010620     c                   endif
182800010620     c                   if        orvdar <> ormdar and
182900010620     c                             orvdar <> sav_orvdar
183000010618     c                   eval      wrormdr = wrormdr +1
183100010621     c   11              eval      w_rormdr = 'X'
183200010618     c                   eval      sav_orvdar = orvdar
183300010618     c                   eval      wokstp = *on
183400010618     c                   eval      wokrfile = *on
183500010618     c                   endif
183600010618     c                   enddo
183700010618
183800010618     c                   endsr
183900010618
184000010608      *--------------------------------------------------------------*
184100010608      * CONTROLLO LA SITUAZIONE DELL'ORM SULLE FASI
184200010608      *--------------------------------------------------------------*
184300010608     c     SR_FASI       begsr
184400010608
184500040421      * ATTENZIONE!!! la fase 200 di dirottato fa fermare la lettura delle fasi, appena
184600040421      *               la trovo esco dalla routine
184700040421
184800010608     c                   clear                   orfnor
184900010608     c                   clear                   orfpoe
185000010608     c                   clear                   orfpog
185100010608     c                   clear                   orfore
185200010608
185300010608     ?* se la data elaborazione è >= data ultima fase "testata"
185400010608     ?* mi posiziono sul file delle fasi con la data/ora/fase della
185500010608     ?* testata ORM
185600010614 b1  c                   if        dataela >= ormdfo
185700010608     c                   eval      kormdfo = ormdfo
185800010608     c                   eval      kormofo = ormofo
185900010608     c                   eval      kormfao = ormfao
186000010608     c     korf01        setll     fnorf01l
186100010608     c     korf01        reade     fnorf01l
186200010614 e1  c                   endif
186300010608     ?* se la data elaborazione è < data ultima fase  "testata"
186400010608     ?* mi posizione sul file delle fasi con la data elaborazione
186500030917     ?* fase = 0 e ora fase = a tutti '9' per leggere l'ultima fase fatta nel giorno
186600030917     ?* di elaborazione statistica
186700010614 b1  c                   if        dataela < ormdfo
186800010608     c                   eval      kormdfo = dataela
186900010608     c                   eval      kormofo = *all'9'
187000010608     c                   clear                   kormfao
187100010608     c     korf01        setgt     fnorf01l
187200010608     c     korf          readpe    fnorf01l
187300010705      * se la data di ritiro = alla data elaborazione e trovo subito il fine file
187400010705      * sulle fasi vuole dire che la prima fase è stata eseguita dopo la data elaborazione
187500010705      * quindi l'orm è non eseguito (es. orm copiati con data immissione e data ritiro = al
187600010705      * vecchio orm, ma la data della fase = alla data del giorno della copia, errore già
187700010705      * sistemato in gestione orm)
187800010705 b2  c                   if        %eof(fnorf01l)
187900010705     c                             and ormdar = dataela
188000010705     c                   eval      wrorone = wrorone +1
188100010717     c   11              eval      w_rorone = 'X'
188200010705     c                   eval      wrornit = wrornit +1
188300010705     c                   eval      wokstp = *on
188400010705     c                   eval      wokrfile = *on
188500010705 e2  c                   endif
188600010614 e1  c                   endif
188700030912      * _____________
188800010608      * leggo le fasi
188900030912      * -------------
189000010613 b1  c                   dow       not %eof(fnorf01l)
189100040421      * ____________________
189200040421      * Fase 200 devo uscire
189300040421      * --------------------
189400040618if  2c                   If        OrfFar = 200
189500040421     c                   Leave
189600040421e   2c                   EndIf
189700040421
189800010608     c                   eval      wok900 = *off
189900010615     c                   eval      wok600 = *off
190000030912      * _____________________________
190100010608      * data fase = data elaborazione
190200030912      * -----------------------------
190300010620 b2  c                   if        orfdae = dataela or
190400010620     c                             (wdatafes > 0 and orfdae >= wdatafes and
190500010620     c                              orfdae < dataela)
190600010614     ?* ESEGUITI
190700010614     ?* ORM eseguiti (fase 900) ?
190800010614 b3  c                   if        orffar = 900
190900010614      * devo controllare se c'è una fase 600 prima
191000030917      * il controllo è così:
191100030917      *     - se data fase < data elaborazione e fase >= 600 è stato fatto prima
191200030917      *       quindi lo devo saltare
191300030917      *     - se no vuol dire che è stato fatto in data elaborazione e lo do per fatto
191400040421      * qui fa solo UNA readpe quindi non faccio il controllo della fase 200
191500010614     c                   eval      wokroes = *off
191600010614     c     korf          readpe    fnorf01l
191700010615 b5  c                   if        orffar >= 600  and
191800010615     c                             orfdae < dataela
191900010615     c                   eval      wok600 = *on
192000030917 x5  c                   else
192100010614     c                   eval      wokroes = *on
192200030917 e5  c                   endif
192300010614
192400010614 b4  c                   if        wokroes = *on
192500010614     c                   eval      wok900 = *on
192600010608     c                   eval      wroroes = wroroes +1
192700010608      * calcolo affidabilità. Uso >= per i ritiri nei festivi
192800010614 b5  c                   select
192900010614     c                   when      ormdar > dataela
193000010614     c                   eval      wrorean = wrorean +1
193100010614     c                   when      ormdar = dataela
193200010608     c                   eval      wroreit = wroreit +1
193300010608     c                   when      ormdar >= wdtda01
193400010608     c                   eval      wrorer1 = wrorer1 +1
193500010608     c                   when      ormdar >= wdtda02
193600010608     c                   eval      wrorer2 = wrorer2 +1
193700010614 x5  c                   other
193800010608     c                   eval      wrorero = wrorero +1
193900010614 e5  c                   endsl
194000010608     c                   eval      wokstp = *on
194100010614     c                   eval      wokrfile = *on
194200010608     c                   leave
194300010614 x4  c                   else
194400010615 b5  c                   if        wok600 = *on
194500010615     c                   leave
194600010615 e5  c                   endif
194700010613 e4  c                   endif
194800010613 e3  c                   endif
194900010614
195000010614     ?* ORM eseguiti (fase 600) ?
195100010614 b3  c                   if        orffar = 600
195200010614     c                   eval      wroroes = wroroes +1
195300010614      * calcolo affidabilità. Uso >= per i ritiri nei festivi
195400010614 b4  c                   select
195500010614     c                   when      ormdar > dataela
195600010614     c                   eval      wrorean = wrorean +1
195700010614     c                   when      ormdar = dataela
195800010614     c                   eval      wroreit = wroreit +1
195900010614     c                   when      ormdar >= wdtda01
196000010614     c                   eval      wrorer1 = wrorer1 +1
196100010614     c                   when      ormdar >= wdtda02
196200010614     c                   eval      wrorer2 = wrorer2 +1
196300010614 x4  c                   other
196400010614     c                   eval      wrorero = wrorero +1
196500010614 e4  c                   endsl
196600010615     ?* ORM non bollettati
196700010615     c                   if        orfdae >= wdtda01 and
196800010618     c                             orfdae < dataela  and
196900010615     c                             ormdar < dataela
197000010615     c                   eval      wrornbl = wrornbl +1
197100010621     c   11              eval      w_rornbl = 'X'
197200010615     c                   endif
197300010614     c                   eval      wokstp = *on
197400010614     c                   eval      wokrfile = *on
197500010614     c                   leave
197600010614 e3  c                   endif
197700010612
197800010614      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
197900010613 b3  c                   if        wokfestar = *off
198000060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
198100010705      * in questo caso devo tornare a leggere la precedente
198200060926     c     orffar        Lookup    skfar                                  30
198300060926     c                   if        *in30
198400010705     c                   goto      passa
198500010705 e4  c                   endif
198600040421     ?* ORM chiusi/eseguiti anomali (fase 910 - 999)
198700031105 b4  c                   if        orffar = 999 or OrfFar = 910
198800030917     c                   eval      wroroea = wroroea +1
198900030917      * calcolo affidabilità. Uso >= per gli anomali nei festivi
199000030917 b5  c                   select
199100030917     c                   when      ormdar > dataela
199200030917     c                   eval      wroraan = wroraan +1
199300030917     c                   when      ormdar = dataela
199400030917     c                   eval      wrorait = wrorait +1
199500030917     c                   when      ormdar >= wdtda01
199600030917     c                   eval      wrorar1 = wrorar1 +1
199700030917     c                   when      ormdar >= wdtda02
199800030917     c                   eval      wrorar2 = wrorar2 +1
199900030917 x5  c                   other
200000030917     c                   eval      wroraro = wroraro +1
200100030917 e5  c                   endsl
200200030917      * memorizzo il tipo comunicazione ORM
200300030917     ?* fissi ?
200400030917 b5  c                   if        ormtco = 'S'
200500030917     c                   eval      wrorifian = wrorifian +1
200600030917 e5  c                   endif
200700030917     ?* da centralino ?
200800030917 b5  c                   if        ormtco = 'M'  or
200900030917     c                             ormtco = 'P'
201000030917     c                   eval      wroricean = wroricean +1
201100030917 e5  c                   endif
201200030917     ?* da internet ?
201300030917 b5  c                   if        ormtco = 'I'
201400030917     c                   eval      wroriinan = wroriinan +1
201500030917 e5  c                   endif
201600030917     ?* da vas ?
201700030917 b5  c                   if        ormtco = 'F'
201800030917     c                   eval      wrorivaan = wrorivaan +1
201900030917 e5  c                   endif
202000141212     ?* da mail ?
202100141212 b5  c                   if        ormtco = 'E'
202200030917     c                   eval      wroriccan = wroriccan +1
202300030917 e5  c                   endif
202400030917     c                   eval      cauoea = orfcar
202500030917     c                   if        orfcar = '80'
202600030917     c                   eval      wrora01 = wrora01 +1
202700030917     c                   endif
202800030917     c                   if        orfcar = '81'
202900030917     c                   eval      wrora02 = wrora02 +1
203000030917     c                   endif
203100030917     c                   if        orfcar = '90'
203200030917     c                   eval      wrora03 = wrora03 +1
203300030917     c                   endif
203400030917     c                   if        orfcar = '91'
203500030917     c                   eval      wrora04 = wrora04 +1
203600030917     c                   endif
203700030917     c                   if        orfcar = '92'
203800030917     c                   eval      wrora05 = wrora05 +1
203900030917     c                   endif
204000030917     c                   if        orfcar = '93'
204100030917     c                   eval      wrora06 = wrora06 +1
204200030917     c                   endif
204300030917     c                   if        orfcar = '94'
204400030917     c                   eval      wrora07 = wrora07 +1
204500030917     c                   endif
204600030917     c                   if        orfcar = '95'
204700030917     c                   eval      wrora08 = wrora08 +1
204800030917     c                   endif
204900031105     c                   if        orfcar = '82'
205000031105     c                   eval      wrora09 = wrora09 +1
205100031105     c                   endif
205200040701     c                   if        orfcar = '96'
205300040701     c                   eval      wrora10 = wrora10 +1
205400040701     c                   endif
205500040701     c                   if        orfcar = '97'
205600040701     c                   eval      wrora11 = wrora11 +1
205700040701     c                   endif
205800030917     c                   eval      wokstp = *on
205900030917     c                   eval      wokrfile = *on
206000030917     c                   leave
206100030917 e4  c                   endif
206200010608     ?* ORM non eseguiti
206300030916 b4  c                   if        orffar < 600
206400030916     ?* con data ritiro <= data elaborazione
206500030916 b4a c                   If        ormdar <= dataela
206600010608     c                   eval      wrorone = wrorone +1
206700010621     c   11              eval      w_rorone = 'X'
206800030917     c                   eval      wokstp = *on
206900030917     c                   eval      wokrfile = *on
207000010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
207100010614 b5  c                   select
207200010620     c                   when      ormdar > dataela
207300010620     c                   eval      wrornan = wrornan +1
207400010608     c                   when      ormdar = dataela
207500010608     c                   eval      wrornit = wrornit +1
207600010608     c                   when      ormdar >= wdtda01
207700010608     c                   eval      wrornr1 = wrornr1 +1
207800010608     c                   when      ormdar >= wdtda02
207900010608     c                   eval      wrornr2 = wrornr2 +1
208000010614 x5  c                   other
208100010608     c                   eval      wrornro = wrornro +1
208200010614 e5  c                   endsl
208300030912      * memorizzo il tipo comunicazione ORM
208400030912     ?* fissi ?
208500030912 b5  c                   if        ormtco = 'S'
208600030917     c                   eval      wrorifine = wrorifine +1
208700030912 e5  c                   endif
208800030912     ?* da centralino ?
208900030912 b5  c                   if        ormtco = 'M'  or
209000030912     c                             ormtco = 'P'
209100030917     c                   eval      wroricene = wroricene +1
209200030912 e5  c                   endif
209300030912     ?* da internet ?
209400030912 b5  c                   if        ormtco = 'I'
209500030917     c                   eval      wroriinne = wroriinne +1
209600030912 e5  c                   endif
209700030912     ?* da vas ?
209800030912 b5  c                   if        ormtco = 'F'
209900030917     c                   eval      wrorivane = wrorivane +1
210000030912 e5  c                   endif
210100141212     ?* da mail ?
210200141212 b5  c                   if        ormtco = 'E'
210300030917     c                   eval      wroriccne = wroriccne +1
210400030912 e5  c                   endif
210500010608     ?* ORM non eseguiti con causale ?
210600010613 b5  c                   if        orffar = 500
210700010619     c                   eval      cauone = orfcar
210800010608     c                   if        orfcar = '01'
210900010608     c                   eval      wrorn01 = wrorn01 +1
211000010608     c                   endif
211100010608     c                   if        orfcar = '02'
211200010608     c                   eval      wrorn02 = wrorn02 +1
211300010608     c                   endif
211400010608     c                   if        orfcar = '03'
211500010608     c                   eval      wrorn03 = wrorn03 +1
211600010608     c                   endif
211700020527     c                   if        orfcar = '04'
211800020527     c                   eval      wrorn04 = wrorn04 +1
211900020527     c                   endif
212000030917     c                   leave
212100010613 e5  c                   endif
212200030917     ?* ORM non eseguiti no in fase 500 ?
212300030917     ?* ORM non assegnati
212400030917 b5  c                   if        orffar < 400
212500030917     c                   eval      wrornas = wrornas +1
212600030917     c   11              eval      w_rornas = 'X'
212700030917 e5  c                   endif
212800030912      * devo controllare se nel giorno è stata fatta una fase 500
212900030912     c     kOrf02        Readpe    Fnorf01l
213000030917 b5  c                   Dow       Not %Eof(Fnorf01l)
213100040421      * se trovo fase 200 esco
213200040618if  6c                   If        OrfFar = 200
213300040421     c                   Leave
213400040421e   6c                   EndIf
213500030917 b6  c                   If        OrfFar = 500
213600030912      * se trovo la fase 500 memorizzo la causale
213700030912     c                   Eval      %Subst(cauone:2:2) = OrfCar
213800030919     c                   Move      ')'           cauone
213900030919     c                   Movel     '('           cauone
214000030912     c                   If        OrfCar = '01'
214100030912     c                   Eval      wrorn01 = wrorn01 +1
214200030912     c                   Endif
214300030912     c                   If        OrfCar = '02'
214400030912     c                   Eval      wrorn02 = wrorn02 +1
214500030912     c                   Endif
214600030912     c                   If        OrfCar = '03'
214700030912     c                   Eval      wrorn03 = wrorn03 +1
214800030912     c                   Endif
214900030912     c                   If        OrfCar = '04'
215000030912     c                   Eval      wrorn04 = wrorn04 +1
215100030912     c                   Endif
215200030912     c                   Leave
215300030917 e6  c                   Endif
215400030912     c     kOrf02        Readpe    Fnorf01l
215500030917 e5  c                   EndDo
215600030917     c                   Leave
215700030916 e4a c                   endif
215800030916     ?* con data ritiro > data elaborazione
215900030919 b4a c                   If        ormdar > dataela
216000030929     ?* è un ORM non fatto se nel giorno c'è una fase 500 o 390
216100031105     ?* quindi lo metto nella colonna dei non eseguiti e anche in quella dei da eseguire
216200030929 b5  c                   If        OrfFar = 500 or OrfFar = 390
216300031105     c                   eval      wrorone = wrorone +1
216400031105     c                   eval      wroroned = wroroned +1
216500031105     c   11              eval      w_rorone = 'X'
216600030917     c                   eval      wokstp = *on
216700030917     c                   eval      wokrfile = *on
216800031105      * da eseguire
216900031105     c                   eval      wrorode = wrorode +1
217000031105      * sempre in anticipo xchè data ritiro > data elaborazione
217100031105     c                   eval      wrordan = wrordan +1
217200031106     c                   eval      wrornan = wrornan +1
217300030912      * memorizzo il tipo comunicazione ORM
217400030912     ?* fissi ?
217500030917 b6  c                   if        ormtco = 'S'
217600031105     c                   eval      wrorifine = wrorifine +1
217700031105     c                   eval      wrorifid = wrorifid +1
217800030917 e6  c                   endif
217900030912     ?* da centralino ?
218000030917 b6  c                   if        ormtco = 'M'  or
218100030912     c                             ormtco = 'P'
218200031105     c                   eval      wroricene = wroricene +1
218300031105     c                   eval      wroriced = wroriced +1
218400030917 e6  c                   endif
218500030912     ?* da internet ?
218600030917 b6  c                   if        ormtco = 'I'
218700031105     c                   eval      wroriinne = wroriinne +1
218800031105     c                   eval      wroriind = wroriind +1
218900030917 e6  c                   endif
219000030912     ?* da vas ?
219100030917 b6  c                   if        ormtco = 'F'
219200031105     c                   eval      wrorivane = wrorivane +1
219300031105     c                   eval      wrorivad = wrorivad +1
219400030917 e6  c                   endif
219500141212     ?* da mail ?
219600141212 b6  c                   if        ormtco = 'E'
219700031105     c                   eval      wroriccne = wroriccne +1
219800031105     c                   eval      wroriccd = wroriccd +1
219900030917 e6  c                   endif
220000030912     ?* ORM non eseguiti con causale ?
220100030929 b6  c                   If        OrfFar = 500
220200031105     c                   eval      cauone = orfcar
220300030912     c                   if        orfcar = '01'
220400031105     c                   eval      wrorn01 = wrorn01 +1
220500031105     c                   eval      wrorn01d = wrorn01d +1
220600030912     c                   endif
220700030912     c                   if        orfcar = '02'
220800031105     c                   eval      wrorn02 = wrorn02 +1
220900031105     c                   eval      wrorn02d = wrorn02d +1
221000030912     c                   endif
221100030912     c                   if        orfcar = '03'
221200031105     c                   eval      wrorn03 = wrorn03 +1
221300031105     c                   eval      wrorn03d = wrorn03d +1
221400030912     c                   endif
221500030912     c                   if        orfcar = '04'
221600031105     c                   eval      wrorn04 = wrorn04 +1
221700031105     c                   eval      wrorn04d = wrorn04d +1
221800030912     c                   endif
221900030929 e6  c                   endif
222000030929     c                   Leave
222100030917 e5  c                   endif
222200030929     ?* ORM no in fase 500 o 390 ?
222300030929      * devo controllare se nel giorno è stata fatta una fase 500 o 390
222400030912     c     kOrf02        Readpe    Fnorf01l
222500030917 b5  c                   Dow       Not %Eof(Fnorf01l)
222600040421      * se trovo fase 200 esco
222700040618if  6c                   If        OrfFar = 200
222800040421     c                   Leave
222900040421e   6c                   EndIf
223000030929 b6  c                   If        OrfFar = 500 or OrfFar = 390
223100031105     c                   eval      wrorone = wrorone +1
223200031105     c                   eval      wroroned = wroroned +1
223300031105     c   11              eval      w_rorone = 'X'
223400030929     c                   eval      wokstp = *on
223500030929     c                   eval      wokrfile = *on
223600031106      * da eseguire
223700031106     c                   eval      wrorode = wrorode +1
223800031106      * sempre in anticipo xchè data ritiro > data elaborazione
223900031106     c                   eval      wrordan = wrordan +1
224000031106     c                   eval      wrornan = wrornan +1
224100030929      * memorizzo il tipo comunicazione ORM
224200030929     ?* fissi ?
224300030929 b7  c                   if        ormtco = 'S'
224400031105     c                   eval      wrorifine = wrorifine +1
224500031105     c                   eval      wrorifid = wrorifid +1
224600030929 e7  c                   endif
224700030929     ?* da centralino ?
224800030929 b7  c                   if        ormtco = 'M'  or
224900030929     c                             ormtco = 'P'
225000031105     c                   eval      wroricene = wroricene +1
225100031105     c                   eval      wroriced = wroriced +1
225200030929 e7  c                   endif
225300030929     ?* da internet ?
225400030929 b7  c                   if        ormtco = 'I'
225500031105     c                   eval      wroriinne = wroriinne +1
225600031105     c                   eval      wroriind = wroriind +1
225700030929 e7  c                   endif
225800030929     ?* da vas ?
225900030929 b7  c                   if        ormtco = 'F'
226000031105     c                   eval      wrorivane = wrorivane +1
226100031105     c                   eval      wrorivad = wrorivad +1
226200030929 e7  c                   endif
226300141212     ?* da mail ?
226400141212 b7  c                   if        ormtco = 'E'
226500031105     c                   eval      wroriccne = wroriccne +1
226600031105     c                   eval      wroriccd = wroriccd +1
226700030929 e7  c                   endif
226800030912      * se trovo la fase 500 memorizzo la causale
226900030929 b7  c                   If        OrfFar = 500
227000031105     c                   Eval      %Subst(cauone:2:2) = OrfCar
227100031105     c                   Move      ')'           cauone
227200031105     c                   Movel     '('           cauone
227300030912     c                   If        OrfCar = '01'
227400031105     c                   Eval      wrorn01 = wrorn01 +1
227500031105     c                   Eval      wrorn01d = wrorn01d +1
227600030912     c                   Endif
227700030912     c                   If        OrfCar = '02'
227800031105     c                   Eval      wrorn02 = wrorn02 +1
227900031105     c                   Eval      wrorn02d = wrorn02d +1
228000030912     c                   Endif
228100030912     c                   If        OrfCar = '03'
228200031105     c                   Eval      wrorn03 = wrorn03 +1
228300031105     c                   Eval      wrorn03d = wrorn03d +1
228400030912     c                   Endif
228500030912     c                   If        OrfCar = '04'
228600031105     c                   Eval      wrorn04 = wrorn04 +1
228700031105     c                   Eval      wrorn04d = wrorn04d +1
228800030912     c                   Endif
228900030929 e7  c                   Endif
229000030912     c                   Leave
229100030917 e6  c                   Endif
229200030912     c     kOrf02        Readpe    Fnorf01l
229300030917 e5  c                   EndDo
229400030917     c                   Leave
229500030916 e4a c                   endif
229600010613 e4  c                   endif
229700010608
229800010613 e3  c                   endif
229900010612
230000010613 e2  c                   endif
230100010608
230200030912      * _____________________________
230300010608      * data fase < data elaborazione
230400030912      * -----------------------------
230500010620 b2  c                   if        orfdae < dataela
230600010705
230700010614      * se ORM eseguito (fase 900) esco dal giro
230800010614     c                   if        orffar >= 900
230900010614     c                   leave
231000010614     c                   endif
231100010612      * se data ritiro maggiore data elaborazione esco dal giro
231200010612     c                   if        ormdar > dataela
231300010612     c                   leave
231400010612     c                   endif
231500010619      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
231600010619      * e i non bollettati
231700010619 b3  c                   if        wokfestar = *off
231800010614     ?* ORM non bollettati
231900010615     c                   if        orffar = 600 and
232000010615     c                             orfdae >= wdtda01
232100010614     c                   eval      wrornbl = wrornbl +1
232200010621     c   11              eval      w_rornbl = 'X'
232300010614     c                   eval      wokstp = *on
232400010614     c                   eval      wokrfile = *on
232500010614     c                   leave
232600010614     c                   endif
232700010615     ?* ORM già eseguiti che non devo elaborare
232800010615     c                   if        orffar = 600
232900010615     c                   leave
233000010615     c                   endif
233100060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
233200010705      * in questo caso devo tornare a leggere la precedente
233300060926     c     orffar        Lookup    skfar                                  30
233400060926     c                   if        *in30
233500010705     c                   goto      passa
233600010705 e4  c                   endif
233700010608     ?* ORM non eseguiti
233800010619 b4  c                   if        orffar < 600
233900010608     c                   eval      wrorone = wrorone +1
234000010621     c   11              eval      w_rorone = 'X'
234100030917     c                   eval      wokstp = *on
234200030917     c                   eval      wokrfile = *on
234300010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
234400010619 b5  c                   select
234500010608     c                   when      ormdar = dataela
234600010608     c                   eval      wrornit = wrornit +1
234700010608     c                   when      ormdar >= wdtda01
234800010608     c                   eval      wrornr1 = wrornr1 +1
234900010608     c                   when      ormdar >= wdtda02
235000010608     c                   eval      wrornr2 = wrornr2 +1
235100010619 x5  c                   other
235200010608     c                   eval      wrornro = wrornro +1
235300010619 e5  c                   endsl
235400030917      * memorizzo il tipo comunicazione ORM
235500030917     ?* fissi ?
235600030917 b5  c                   if        ormtco = 'S'
235700030917     c                   eval      wrorifine = wrorifine +1
235800030917 e5  c                   endif
235900030917     ?* da centralino ?
236000030917 b5  c                   if        ormtco = 'M'  or
236100030917     c                             ormtco = 'P'
236200030917     c                   eval      wroricene = wroricene +1
236300030917 e5  c                   endif
236400030917     ?* da internet ?
236500030917 b5  c                   if        ormtco = 'I'
236600030917     c                   eval      wroriinne = wroriinne +1
236700030917 e5  c                   endif
236800030917     ?* da vas ?
236900030917 b5  c                   if        ormtco = 'F'
237000030917     c                   eval      wrorivane = wrorivane +1
237100030917 e5  c                   endif
237200141212     ?* da mail ?
237300141212 b5  c                   if        ormtco = 'E'
237400030917     c                   eval      wroriccne = wroriccne +1
237500030917 e5  c                   endif
237600030917     ?* ORM non eseguiti con causale ?
237700030917 b5  c                   if        orffar = 500
237800030917     c                   eval      cauone = orfcar
237900030917     c                   if        orfcar = '01'
238000030917     c                   eval      wrorn01 = wrorn01 +1
238100030917     c                   endif
238200030917     c                   if        orfcar = '02'
238300030917     c                   eval      wrorn02 = wrorn02 +1
238400030917     c                   endif
238500030917     c                   if        orfcar = '03'
238600030917     c                   eval      wrorn03 = wrorn03 +1
238700030917     c                   endif
238800030917     c                   if        orfcar = '04'
238900030917     c                   eval      wrorn04 = wrorn04 +1
239000030917     c                   endif
239100030917     c                   leave
239200030917 e5  c                   endif
239300030917     ?* ORM non eseguiti no in fase 500 ?
239400010614     ?* ORM non assegnati
239500010619 b5  c                   if        orffar < 400
239600010614     c                   eval      wrornas = wrornas +1
239700010621     c   11              eval      w_rornas = 'X'
239800010619 e5  c                   endif
239900030917      * devo controllare se nel giorno è stata fatta una fase 500
240000030917     c     kOrf02        Readpe    Fnorf01l
240100030917 b5  c                   Dow       Not %Eof(Fnorf01l)
240200040421      * se trovo fase 200 esco
240300040618if  6c                   If        OrfFar = 200
240400040421     c                   Leave
240500040421e   6c                   EndIf
240600030917 b6  c                   If        OrfFar = 500
240700030917      * se trovo la fase 500 memorizzo la causale
240800030917     c                   Eval      %Subst(cauone:2:2) = OrfCar
240900031118     c                   Move      ')'           cauone
241000031118     c                   Movel     '('           cauone
241100030917     c                   If        OrfCar = '01'
241200030917     c                   Eval      wrorn01 = wrorn01 +1
241300030917     c                   Endif
241400030917     c                   If        OrfCar = '02'
241500030917     c                   Eval      wrorn02 = wrorn02 +1
241600030917     c                   Endif
241700030917     c                   If        OrfCar = '03'
241800030917     c                   Eval      wrorn03 = wrorn03 +1
241900030917     c                   Endif
242000030917     c                   If        OrfCar = '04'
242100030917     c                   Eval      wrorn04 = wrorn04 +1
242200030917     c                   Endif
242300030917     c                   Leave
242400030917 e6  c                   Endif
242500030917     c     kOrf02        Readpe    Fnorf01l
242600030917 e5  c                   EndDo
242700010608     c                   leave
242800010619 e4  c                   endif
242900010619
243000010619 e3  c                   endif
243100010608
243200010613 e2  c                   endif
243300010705
243400010705     c     passa         tag
243500010705
243600010608     c     korf          readpe    fnorf01l
243700010705
243800010613 e1  c                   enddo
243900010612
244000010612      * se ORM in fase 900 controllo se quadrato
244100010612     c                   if        wok900 = *on
244200010614      *                            =========
244300010614     c                   exsr      SR_QUADRA
244400010614      *                            =========
244500010612     c                   endif
244600010608
244700010608     c                   endsr
244800010614
244900010608      *--------------------------------------------------------------*
245000010608      * ORM NON QUADRATO
245100010608      *--------------------------------------------------------------*
245200010614     c     SR_QUADRA     begsr
245300010608
245400010608      * ORM non quadrati
245500010608      * mi devo posizionare sulle fasi con la data elaborazione
245600010608      * poi cercare se c'è una fase 900 controllare se esiste
245700010608      * una fase 600
245800010608
245900010608     c                   eval      wok600 = *off
246000010608
246100010608     c                   if        wok900 = *on
246200010608     c                   eval      kormdfo = dataela
246300010615     c                   clear                   kormofo
246400010615     c                   clear                   kormfao
246500010608     c     korf          setll     fnorf01l
246600010608     c                   do        *hival
246700010608     c     korf          reade     fnorf01l
246800010608     c                   if        %eof(fnorf01l)
246900010608     c                   leave
247000010608     c                   endif
247100010608     c                   if        orffar = 600
247200010608     c                   eval      wok600 = *on
247300010608     c                   leave
247400010608     c                   endif
247500010608     c                   enddo
247600010608     c                   if        wok600 = *off
247700010608     c                   eval      wrornqd = wrornqd +1
247800010621     c   11              eval      w_rornqd = 'X'
247900010608     c                   eval      wokstp = *on
248000010608     c                   endif
248100010608     c                   endif
248200010608
248300010608     c                   endsr
248400010614
248500010605      *--------------------------------------------------------------*
248600010605      * CONTROLLO E CARICAMENTO SK
248700010605      *--------------------------------------------------------------*
248800010605     c     SR_CONTRSK    begsr
248900010605
249000010606     c                   eval      woksk = *off
249100010606
249200010612     c                   do        18000         xf
249300030912     c                   if        kfile(xf) = dskfile
249400010605     c                   eval      woksk = *on
249500010605     c                   leave
249600010605     c                   endif
249700030912     c                   if        kfile1(xf) = *zeros
249800010605     c                   leave
249900010605     c                   endif
250000010605     c                   enddo
250100010605
250200010606     c                   if        woksk = *on
250300010612     c                   eval      dswfror = rfile(xf)
250400010606     c                   endif
250500010605
250600010605     c                   if        woksk = *off
250700010611     c                   clear                   dswfror
250800010612     c                   eval      kfile(xf) = dskfile
250900030916     c                   eval      kfile1(xf) = dskfile1
251000030916     c                   Eval      f_rortrc = dstrc
251100010605     c                   eval      f_rorpoe = dspoel
251200010605     c                   eval      f_rordae = dsdata
251300010605     c                   eval      kazorg = dspoel
251400010605     c     kazorg        chain     azorg01l
251500010605     c                   if        %found(azorg01l)
251600010605     c                   eval      f_rordis = orgfl3
251700010605     c                   eval      f_rorare = orgcar
251800010605     c                   endif
251900010605     c                   endif
252000010605
252100010605     c                   endsr
252200010614
252300010608      *--------------------------------------------------------------*
252400010608      * CARICO LA SK DEL FILE
252500010608      *--------------------------------------------------------------*
252600010614     c     SR_CARICA     begsr
252700010608
252800010608     c                   eval      f_rorode = (f_rorode + wrorode)
252900010608     c                   eval      f_roroes = (f_roroes + wroroes)
253000010608     c                   eval      f_rorone = (f_rorone + wrorone)
253100010608     c                   eval      f_rorn01 = (f_rorn01 + wrorn01)
253200010608     c                   eval      f_rorn02 = (f_rorn02 + wrorn02)
253300010608     c                   eval      f_rorn03 = (f_rorn03 + wrorn03)
253400010608     c                   eval      f_rorn04 = (f_rorn04 + wrorn04)
253500010608     c                   eval      f_rorn05 = (f_rorn05 + wrorn05)
253600010608     c                   eval      f_rorn06 = (f_rorn06 + wrorn06)
253700010608     c                   eval      f_rorn07 = (f_rorn07 + wrorn07)
253800010608     c                   eval      f_rorn08 = (f_rorn08 + wrorn08)
253900010608     c                   eval      f_rorn09 = (f_rorn09 + wrorn09)
254000010608     c                   eval      f_rorn10 = (f_rorn10 + wrorn10)
254100010608     c                   eval      f_roroea = (f_roroea + wroroea)
254200010608     c                   eval      f_rora01 = (f_rora01 + wrora01)
254300010608     c                   eval      f_rora02 = (f_rora02 + wrora02)
254400010608     c                   eval      f_rora03 = (f_rora03 + wrora03)
254500010608     c                   eval      f_rora04 = (f_rora04 + wrora04)
254600010608     c                   eval      f_rora05 = (f_rora05 + wrora05)
254700010608     c                   eval      f_rora06 = (f_rora06 + wrora06)
254800010608     c                   eval      f_rora07 = (f_rora07 + wrora07)
254900010608     c                   eval      f_rora08 = (f_rora08 + wrora08)
255000010608     c                   eval      f_rora09 = (f_rora09 + wrora09)
255100010608     c                   eval      f_rora10 = (f_rora10 + wrora10)
255200010608     c                   eval      f_rora11 = (f_rora11 + wrora11)
255300010608     c                   eval      f_rora12 = (f_rora12 + wrora12)
255400010608     c                   eval      f_rora13 = (f_rora13 + wrora13)
255500010608     c                   eval      f_rora14 = (f_rora14 + wrora14)
255600010608     c                   eval      f_rora15 = (f_rora15 + wrora15)
255700010620     c                   eval      f_rordan = (f_rordan + wrordan)
255800010608     c                   eval      f_rordit = (f_rordit + wrordit)
255900010608     c                   eval      f_rordr1 = (f_rordr1 + wrordr1)
256000010608     c                   eval      f_rordr2 = (f_rordr2 + wrordr2)
256100010608     c                   eval      f_rordro = (f_rordro + wrordro)
256200010614     c                   eval      f_rorean = (f_rorean + wrorean)
256300010608     c                   eval      f_roreit = (f_roreit + wroreit)
256400010608     c                   eval      f_rorer1 = (f_rorer1 + wrorer1)
256500010608     c                   eval      f_rorer2 = (f_rorer2 + wrorer2)
256600010608     c                   eval      f_rorero = (f_rorero + wrorero)
256700010620     c                   eval      f_rornan = (f_rornan + wrornan)
256800010608     c                   eval      f_rornit = (f_rornit + wrornit)
256900010608     c                   eval      f_rornr1 = (f_rornr1 + wrornr1)
257000010608     c                   eval      f_rornr2 = (f_rornr2 + wrornr2)
257100010608     c                   eval      f_rornro = (f_rornro + wrornro)
257200010620     c                   eval      f_roraan = (f_roraan + wroraan)
257300010608     c                   eval      f_rorait = (f_rorait + wrorait)
257400010608     c                   eval      f_rorar1 = (f_rorar1 + wrorar1)
257500010608     c                   eval      f_rorar2 = (f_rorar2 + wrorar2)
257600010608     c                   eval      f_roraro = (f_roraro + wroraro)
257700010608     c                   eval      f_rornqd = (f_rornqd + wrornqd)
257800010608     c                   eval      f_rornbl = (f_rornbl + wrornbl)
257900010614     c                   eval      f_rormdr = (f_rormdr + wrormdr)
258000010614     c                   eval      f_rornas = (f_rornas + wrornas)
258100030912     c                   eval      f_roroned = (f_roroned + wroroned)
258200030912     c                   eval      f_rorn01d = (f_rorn01d + wrorn01d)
258300030912     c                   eval      f_rorn02d = (f_rorn02d + wrorn02d)
258400030912     c                   eval      f_rorn03d = (f_rorn03d + wrorn03d)
258500030912     c                   eval      f_rorn04d = (f_rorn04d + wrorn04d)
258600030912     c                   eval      f_rorn05d = (f_rorn05d + wrorn05d)
258700030912     c                   eval      f_rorn06d = (f_rorn06d + wrorn06d)
258800030912     c                   eval      f_rorn07d = (f_rorn07d + wrorn07d)
258900030912     c                   eval      f_rorn08d = (f_rorn08d + wrorn08d)
259000030912     c                   eval      f_rorn09d = (f_rorn09d + wrorn09d)
259100030912     c                   eval      f_rorn10d = (f_rorn10d + wrorn10d)
259200030917     c                   eval      f_rorifian = (f_rorifian + wrorifian)
259300030917     c                   eval      f_rorifine = (f_rorifine + wrorifine)
259400030917     c                   eval      f_rorifid  = (f_rorifid  + wrorifid)
259500030917     c                   eval      f_roricean = (f_roricean + wroricean)
259600030917     c                   eval      f_roricene = (f_roricene + wroricene)
259700030917     c                   eval      f_roriced  = (f_roriced  + wroriced)
259800030917     c                   eval      f_roriinan = (f_roriinan + wroriinan)
259900030917     c                   eval      f_roriinne = (f_roriinne + wroriinne)
260000030917     c                   eval      f_roriind  = (f_roriind  + wroriind)
260100030917     c                   eval      f_rorivaan = (f_rorivaan + wrorivaan)
260200030917     c                   eval      f_rorivane = (f_rorivane + wrorivane)
260300030917     c                   eval      f_rorivad  = (f_rorivad  + wrorivad)
260400030917     c                   eval      f_roriccan = (f_roriccan + wroriccan)
260500030917     c                   eval      f_roriccne = (f_roriccne + wroriccne)
260600030917     c                   eval      f_roriccd  = (f_roriccd  + wroriccd)
260700010612     c                   eval      rfile(xf) = dswfror
260800010608
260900010608     c                   endsr
261000010614
261100010604      *--------------------------------------------------------------*
261200010604      * ROUTINE INIZIALE
261300010604      *--------------------------------------------------------------*
261400010604     c     *inzsr        begsr
261500010604      *
261600010604     c     *entry        plist
261700010604     c                   parm                    kpjba
261800010621
261900010621     c                   movel     kpjbu         fior40ds
262000010621
262100030912      * controllo il sistema informativo dove sono per reperire
262200030912      * la libreria
262300030912      *   ---> GAITRA201/GAITRAGRU
262400030912     c                   If        %subst(knsif:7:3) = '201'  and
262500030912     c                             %subst(knsif:1:1) = 'G'
262600030912     c                   Eval      wlib = 'GAITRAGRU '
262700030912     c                   EndIf
262800030916      *   ---> FILTRA201/FILTRAGRU
262900030912     c                   If        %subst(knsif:7:3) = '201'  and
263000030912     c                             %subst(knsif:1:1) = 'F'
263100030912     c                   Eval      wlib = 'FILTRAGRU '
263200030912     c                   EndIf
263300030916      *   ---> GAITRAPSM/GAITRAGRPS
263400030916     c                   If        %subst(knsif:7:3) = 'PSM'  and
263500030916     c                             %subst(knsif:1:1) = 'G'
263600030916     c                   Eval      wlib = 'GAITRAGRPS'
263700030916     c                   EndIf
263800030912      *   ---> GAITRAPSP/GAITRAGRPS
263900030912     c                   If        %subst(knsif:7:3) = 'PSP'  and
264000030912     c                             %subst(knsif:1:1) = 'G'
264100040421     c                   Eval      wlib = 'GAITRAGRPS'
264200040421     c**                 Eval      wlib = 'GAITRAFIL '
264300030912     c                   EndIf
264400030916      *   ---> GAITRAPFM/GAITRAGRPF
264500030916     c                   If        %subst(knsif:7:3) = 'PFM'  and
264600030916     c                             %subst(knsif:1:1) = 'G'
264700030916     c                   Eval      wlib = 'GAITRAGRPF'
264800030916     c                   EndIf
264900030912      *   ---> GAITRAPFP/GAITRAGRPF
265000030912     c                   If        %subst(knsif:7:3) = 'PFP'  and
265100030912     c                             %subst(knsif:1:1) = 'G'
265200040421     c                   Eval      wlib = 'GAITRAGRPF'
265300030912     c                   EndIf
265400030912      *   ---> FILTRAPFP/FILTRAGRPF
265500030912     c                   If        %subst(knsif:7:3) = 'PFP'  and
265600030912     c                             %subst(knsif:1:1) = 'F'
265700040421     c                   Eval      wlib = 'FILTRAGRPF'
265800030912     c                   EndIf
265900030912
266000010621      * duplico il file di comodo in qtemp
266100010621     c                   clear                   comman
266200010621     c                   movel(p)  cmd(1)        comman
266300030912     c                   Eval      %Subst(comman:32:10) = wlib
266400010621     c                   call      'QCMDEXC'
266500010621     c                   parm                    comman
266600010621     c                   parm                    lenght
266700010621      * apro il file di comodo per la stampa
266800010621     c                   open      wfror11l
266900010621
267000170322     c                   z-add     1             codut             1 0
267100010604
267200010604      * reperisco data e ora
267300010604     c                   time                    w0140
267400010604      * udate in GGMMAAAA
267500010604     c                   move      w0140         wdate
267600010604     c                   movel     w0140         wora
267700170322
267800170322      /free
267900170322       //?Reperimento dati job
268000170322         in(E) §AzUte;
268100170322         IF  NOT %error;
268200170322           in(E) §DatiUte;
268300170322         ENDIF;
268400170322         IF  %error or RSut = *blanks;
268500170322           clear TIBS34ds;
268600170322           tibs34r(tibs34ds);
268700170322           in §AzUte;
268800170322           in §DatiUte;
268900170322         ENDIF;
269000170322      /end-free
269100010608
269200010608      * pulisco le sk
269300010608     c                   clear                   kfile
269400030916     c                   clear                   kfile1
269500010608     c                   clear                   rfile
269600010608     c                   clear                   skdate
269700010608     c                   clear                   skpoel
269800130118     c                   Clear                   skcliosr
269900130118     c                   Clear                   skstaosr
270000010621
270100010621      * controllo se devo stampare per data o per p.o.
270200010621     c                   if        i40poel <> 999 and
270300010621     c                             i40dtda <> i40dtal and
270400010621     c                             i40tpst = 'S'
270500010621     c                   eval      *in13 = *on
270600010621     c                   else
270700010621     c                   eval      *in13 = *off
270800010621     c                   endif
270900010608
271000010608      * carico la sk delle date da elaborare
271100010703     c                   eval      xd = 1
271200010703     c                   eval      skdate(xd) = i40dtda
271300010703     c                   if        i40dtda <> i40dtal
271400010703     c                   eval      wdtgio = i40dtda
271500010703     c     *iso          movel     i40dtda       wdtda
271600010703     c                   dow       wdtgio < i40dtal
271700010703     c                   eval      xd = xd +1
271800010703     c                   adddur    1:*d          wdtda
271900010703     c                   move      wdtda         wdtgio
272000010703     c                   eval      skdate(xd) = wdtgio
272100010703     c                   enddo
272200010703     c                   endif
272300010608
272400020807      * Elaboro tutti i p.o.
272500020807 b1  c                   if        i40poel = 999
272600170322      /free
272700170322       //?Carico le filiali con abilitazione 'RC' Raggruppamento Centralino
272800170322       //?se utente di filiale
272900170328         IF  I40sede = 'F';
273000170322           clear TRUL31DS;
273100170322           I31abi = 'RC';
273200170322           I31cdi = DUTdis;
273300170322           I31car = DUTare;
273400170322           I31cpo = DUTpou;
273500170322           TRUL31R (kpjba:trul31ds);
273600170328         ENDIF;
273700170322       //?Carico le tutte le filiali
273800170322       //?se utente di sede
273900170328         IF  I40sede = 'S';
274000170322           clear TRUL31DS;
274100170322           I31abi = 'AZ';
274200170322           I31cdi = DUTdis;
274300170322           I31car = DUTare;
274400170322           I31cpo = DUTpou;
274500170322           TRUL31R (kpjba:trul31ds);
274600170322         ENDIF;
274700170322       //?Da O31pog alfa a SKpog numerica
274800170322         xb = 1;
274900170322         FOR xb by 1 to %elem(pog);
275000170322           IF  pog(xb) <> *blanks and pog(xb) <> *zeros;
275100170322             skpoel(xb) = %dec(pog(xb):3:0);
275200170322           ENDIF;
275300170322         ENDFOR;
275400170322      /end-free
275500020807 e1  c                   EndIf
275600010605      * lanciato un solo po carico questo nella sk
275700010605     c                   if        i40poel <> 999
275800170322     c                   eval      skpoel(01) = i40poel
275900010605     c                   endif
276000031002
276100031002      * carico la sk delle serie da non elaborare in statistica
276200031002     c                   Eval      xc = 1
276300031002     c                   Eval      TbeCod = 'OSR'
276400031002     c     TbeCod        Setll     Tntbe01l
276500031002Do  1c                   Do        *Hival
276600031002     c     TbeCod        Reade     Tntbe01l
276700031002     c                   If        %Eof(Tntbe01l)
276800031002     c                   Leave
276900031002     c                   EndIf
277000130122     c                   IF        TBEatb <> *blanks
277100130122     c                   iter
277200130122     c                   ENDIF
277300031002     c                   Movel     TbeUni        dOsr
277400130118      /free
277500130118           IF d§osrcli <> 0;
277600130118             xx = %lookup(d§OSRcli:skCLIosr);
277700130118             IF  xx = 0;
277800130118               xx = %lookup(*zeros:skCLIosr);
277900130118               IF  xx > 0;
278000130118                 skCLIosr(xx) = d§OSRcli;
278100130118                 skstaosr(xx) = d§OSRsta;
278200130118               ENDIF;
278300130118             ENDIF;
278400130118           ENDIF;
278500130118
278600130118           IF d§osrcl2 <> 0;
278700130118             xx = %lookup(d§OSRcl2:skCLIosr);
278800130118             IF  xx = 0;
278900130118               xx = %lookup(*zeros:skCLIosr);
279000130118               IF  xx > 0;
279100130118                 skCLIosr(xx) = d§OSRcl2;
279200130118                 skstaosr(xx) = d§OSRsta;
279300130118               ENDIF;
279400130118             ENDIF;
279500130118           ENDIF;
279600130118
279700130118           IF d§osrcl3 <> 0;
279800130118             xx = %lookup(d§OSRcl3:skCLIosr);
279900130118             IF  xx = 0;
280000130118               xx = %lookup(*zeros:skCLIosr);
280100130118               IF  xx > 0;
280200130118                 skCLIosr(xx) = d§OSRcl3;
280300130118                 skstaosr(xx) = d§OSRsta;
280400130118               ENDIF;
280500130118             ENDIF;
280600130118           ENDIF;
280700130118
280800130118           IF d§osrcl4 <> 0;
280900130118             xx = %lookup(d§OSRcl4:skCLIosr);
281000130118             IF  xx = 0;
281100130118               xx = %lookup(*zeros:skCLIosr);
281200130118               IF  xx > 0;
281300130118                 skCLIosr(xx) = d§OSRcl4;
281400130118                 skstaosr(xx) = d§OSRsta;
281500130118               ENDIF;
281600130118             ENDIF;
281700130118           ENDIF;
281800130118
281900130118           IF d§osrcl5 <> 0;
282000130118             xx = %lookup(d§OSRcl5:skCLIosr);
282100130118             IF  xx = 0;
282200130118               xx = %lookup(*zeros:skCLIosr);
282300130118               IF  xx > 0;
282400130118                 skCLIosr(xx) = d§OSRcl5;
282500130118                 skstaosr(xx) = d§OSRsta;
282600130118               ENDIF;
282700130118             ENDIF;
282800130118           ENDIF;
282900130118
283000130118           IF d§osrcl6 <> 0;
283100130118             xx = %lookup(d§OSRcl6:skCLIosr);
283200130118             IF  xx = 0;
283300130118               xx = %lookup(*zeros:skCLIosr);
283400130118               IF  xx > 0;
283500130118                 skCLIosr(xx) = d§OSRcl6;
283600130118                 skstaosr(xx) = d§OSRsta;
283700130118               ENDIF;
283800130118             ENDIF;
283900130118           ENDIF;
284000130118
284100130118           IF d§osrcl7 <> 0;
284200130118             xx = %lookup(d§OSRcl7:skCLIosr);
284300130118             IF  xx = 0;
284400130118               xx = %lookup(*zeros:skCLIosr);
284500130118               IF  xx > 0;
284600130118                 skCLIosr(xx) = d§OSRcl7;
284700130118                 skstaosr(xx) = d§OSRsta;
284800130118               ENDIF;
284900130118             ENDIF;
285000130118           ENDIF;
285100130118
285200130118           IF d§osrcl8 <> 0;
285300130118             xx = %lookup(d§OSRcl8:skCLIosr);
285400130118             IF  xx = 0;
285500130118               xx = %lookup(*zeros:skCLIosr);
285600130118               IF  xx > 0;
285700130118                 skCLIosr(xx) = d§OSRcl8;
285800130118                 skstaosr(xx) = d§OSRsta;
285900130118               ENDIF;
286000130118             ENDIF;
286100130118           ENDIF;
286200130118
286300130118           IF d§osrcl9 <> 0;
286400130118             xx = %lookup(d§OSRcl9:skCLIosr);
286500130118             IF  xx = 0;
286600130118               xx = %lookup(*zeros:skCLIosr);
286700130118               IF  xx > 0;
286800130118                 skCLIosr(xx) = d§OSRcl9;
286900130118                 skstaosr(xx) = d§OSRsta;
287000130118               ENDIF;
287100130118             ENDIF;
287200130118           ENDIF;
287300130118
287400130118           IF d§osrcl10 <> 0;
287500130118             xx = %lookup(d§OSRcl10:skCLIosr);
287600130118             IF  xx = 0;
287700130118               xx = %lookup(*zeros:skCLIosr);
287800130118               IF  xx > 0;
287900130118                 skCLIosr(xx) = d§OSRcl10;
288000130118                 skstaosr(xx) = d§OSRsta;
288100130118               ENDIF;
288200130118             ENDIF;
288300130118           ENDIF;
288400130118
288500130118           IF d§osrcl11 <> 0;
288600130118             xx = %lookup(d§OSRcl11:skCLIosr);
288700130118             IF  xx = 0;
288800130118               xx = %lookup(*zeros:skCLIosr);
288900130118               IF  xx > 0;
289000130118                 skCLIosr(xx) = d§OSRcl11;
289100130118                 skstaosr(xx) = d§OSRsta;
289200130118               ENDIF;
289300130118             ENDIF;
289400130118           ENDIF;
289500130118
289600130118           IF d§osrcl12 <> 0;
289700130118             xx = %lookup(d§OSRcl12:skCLIosr);
289800130118             IF  xx = 0;
289900130118               xx = %lookup(*zeros:skCLIosr);
290000130118               IF  xx > 0;
290100130118                 skCLIosr(xx) = d§OSRcl12;
290200130118                 skstaosr(xx) = d§OSRsta;
290300130118               ENDIF;
290400130118             ENDIF;
290500130118           ENDIF;
290600130118
290700130118           IF d§osrcl13 <> 0;
290800130118             xx = %lookup(d§OSRcl13:skCLIosr);
290900130118             IF  xx = 0;
291000130118               xx = %lookup(*zeros:skCLIosr);
291100130118               IF  xx > 0;
291200130118                 skCLIosr(xx) = d§OSRcl13;
291300130118                 skstaosr(xx) = d§OSRsta;
291400130118               ENDIF;
291500130118             ENDIF;
291600130118           ENDIF;
291700130118
291800130118           IF d§osrcl14 <> 0;
291900130118             xx = %lookup(d§OSRcl14:skCLIosr);
292000130118             IF  xx = 0;
292100130118               xx = %lookup(*zeros:skCLIosr);
292200130118               IF  xx > 0;
292300130118                 skCLIosr(xx) = d§OSRcl14;
292400130118                 skstaosr(xx) = d§OSRsta;
292500130118               ENDIF;
292600130118             ENDIF;
292700130118           ENDIF;
292800130118
292900130118           IF d§osrcl15 <> 0;
293000130118             xx = %lookup(d§OSRcl15:skCLIosr);
293100130118             IF  xx = 0;
293200130118               xx = %lookup(*zeros:skCLIosr);
293300130118               IF  xx > 0;
293400130118                 skCLIosr(xx) = d§OSRcl15;
293500130118                 skstaosr(xx) = d§OSRsta;
293600130118               ENDIF;
293700130118             ENDIF;
293800130118           ENDIF;
293900130118
294000130118           IF d§osrcl16 <> 0;
294100130118             xx = %lookup(d§OSRcl16:skCLIosr);
294200130118             IF  xx = 0;
294300130118               xx = %lookup(*zeros:skCLIosr);
294400130118               IF  xx > 0;
294500130118                 skCLIosr(xx) = d§OSRcl16;
294600130118                 skstaosr(xx) = d§OSRsta;
294700130118               ENDIF;
294800130118             ENDIF;
294900130118           ENDIF;
295000130118      /end-free
295100130118
295200031002    1c                   EndDo
295300010611
295400010611      * carico nei campi di stampa le descrizioni delle causali
295500010611     c                   eval      tbecod = 'CMR'
295600010611      * causali di non fatto
295700010611     c                   eval      tbeke1 = '01'
295800010611     c     ktntbe        chain     tntbe01l
295900010611     c                   if        %found(tntbe01l)
296000010619     c                   clear                   dcmr
296100010619     c                   movel     tbeuni        dcmr
296200010619     c                   if        d§cmrdes1 <> *blanks
296300010619     c                   movel     d§cmrdes1     descau01n
296400010619     c                   else
296500010619     c                   movel     d§cmrdes      descau01n
296600010619     c                   endif
296700010611     c                   endif
296800010619
296900010611     c                   eval      tbeke1 = '02'
297000010611     c     ktntbe        chain     tntbe01l
297100010611     c                   if        %found(tntbe01l)
297200010619     c                   clear                   dcmr
297300010619     c                   movel     tbeuni        dcmr
297400010619     c                   if        d§cmrdes1 <> *blanks
297500010619     c                   movel     d§cmrdes1     descau02n
297600010619     c                   else
297700010619     c                   movel     d§cmrdes      descau02n
297800010619     c                   endif
297900010611     c                   endif
298000010619
298100010611     c                   eval      tbeke1 = '03'
298200010611     c     ktntbe        chain     tntbe01l
298300010611     c                   if        %found(tntbe01l)
298400010619     c                   clear                   dcmr
298500010619     c                   movel     tbeuni        dcmr
298600010619     c                   if        d§cmrdes1 <> *blanks
298700010619     c                   movel     d§cmrdes1     descau03n
298800010619     c                   else
298900010619     c                   movel     d§cmrdes      descau03n
299000010619     c                   endif
299100010611     c                   endif
299200020527
299300020527     c                   eval      tbeke1 = '04'
299400020527     c     ktntbe        chain     tntbe01l
299500020527     c                   if        %found(tntbe01l)
299600020527     c                   clear                   dcmr
299700020527     c                   movel     tbeuni        dcmr
299800020527     c                   if        d§cmrdes1 <> *blanks
299900020527     c                   movel     d§cmrdes1     descau04n
300000020527     c                   else
300100020527     c                   movel     d§cmrdes      descau04n
300200020527     c                   endif
300300020527     c                   endif
300400010619
300500010611      * causali di esito anomalo
300600010611     c                   eval      tbeke1 = '80'
300700010611     c     ktntbe        chain     tntbe01l
300800010611     c                   if        %found(tntbe01l)
300900010619     c                   clear                   dcmr
301000010619     c                   movel     tbeuni        dcmr
301100010619     c                   if        d§cmrdes1 <> *blanks
301200010619     c                   movel     d§cmrdes1     descau01a
301300010619     c                   else
301400010619     c                   movel     d§cmrdes      descau01a
301500010619     c                   endif
301600010611     c                   endif
301700010619
301800010611     c                   eval      tbeke1 = '81'
301900010611     c     ktntbe        chain     tntbe01l
302000010611     c                   if        %found(tntbe01l)
302100010619     c                   clear                   dcmr
302200010619     c                   movel     tbeuni        dcmr
302300010619     c                   if        d§cmrdes1 <> *blanks
302400010619     c                   movel     d§cmrdes1     descau02a
302500010619     c                   else
302600010619     c                   movel     d§cmrdes      descau02a
302700010619     c                   endif
302800010611     c                   endif
302900010619
303000010611     c                   eval      tbeke1 = '90'
303100010611     c     ktntbe        chain     tntbe01l
303200010611     c                   if        %found(tntbe01l)
303300010619     c                   clear                   dcmr
303400010619     c                   movel     tbeuni        dcmr
303500010619     c                   if        d§cmrdes1 <> *blanks
303600010619     c                   movel     d§cmrdes1     descau03a
303700010619     c                   else
303800010619     c                   movel     d§cmrdes      descau03a
303900010619     c                   endif
304000010611     c                   endif
304100010619
304200010611     c                   eval      tbeke1 = '91'
304300010611     c     ktntbe        chain     tntbe01l
304400010611     c                   if        %found(tntbe01l)
304500010619     c                   clear                   dcmr
304600010619     c                   movel     tbeuni        dcmr
304700010619     c                   if        d§cmrdes1 <> *blanks
304800010619     c                   movel     d§cmrdes1     descau04a
304900010619     c                   else
305000010619     c                   movel     d§cmrdes      descau04a
305100010619     c                   endif
305200010611     c                   endif
305300010619
305400010611     c                   eval      tbeke1 = '92'
305500010611     c     ktntbe        chain     tntbe01l
305600010611     c                   if        %found(tntbe01l)
305700010619     c                   clear                   dcmr
305800010619     c                   movel     tbeuni        dcmr
305900010619     c                   if        d§cmrdes1 <> *blanks
306000010619     c                   movel     d§cmrdes1     descau05a
306100010619     c                   else
306200010619     c                   movel     d§cmrdes      descau05a
306300010619     c                   endif
306400010611     c                   endif
306500010619
306600010611     c                   eval      tbeke1 = '93'
306700010611     c     ktntbe        chain     tntbe01l
306800010611     c                   if        %found(tntbe01l)
306900010619     c                   clear                   dcmr
307000010619     c                   movel     tbeuni        dcmr
307100010619     c                   if        d§cmrdes1 <> *blanks
307200010619     c                   movel     d§cmrdes1     descau06a
307300010619     c                   else
307400010619     c                   movel     d§cmrdes      descau06a
307500010619     c                   endif
307600010611     c                   endif
307700010619
307800010611     c                   eval      tbeke1 = '94'
307900010611     c     ktntbe        chain     tntbe01l
308000010611     c                   if        %found(tntbe01l)
308100010619     c                   clear                   dcmr
308200010619     c                   movel     tbeuni        dcmr
308300010619     c                   if        d§cmrdes1 <> *blanks
308400010619     c                   movel     d§cmrdes1     descau07a
308500010619     c                   else
308600010619     c                   movel     d§cmrdes      descau07a
308700010619     c                   endif
308800010611     c                   endif
308900020527
309000020527     c                   eval      tbeke1 = '95'
309100020527     c     ktntbe        chain     tntbe01l
309200020527     c                   if        %found(tntbe01l)
309300020527     c                   clear                   dcmr
309400020527     c                   movel     tbeuni        dcmr
309500020527     c                   if        d§cmrdes1 <> *blanks
309600020527     c                   movel     d§cmrdes1     descau08a
309700020527     c                   else
309800020527     c                   movel     d§cmrdes      descau08a
309900020527     c                   endif
310000020527     c                   endif
310100020625
310200020625     c                   eval      tbeke1 = '82'
310300020625     c     ktntbe        chain     tntbe01l
310400020625     c                   if        %found(tntbe01l)
310500020625     c                   clear                   dcmr
310600020625     c                   movel     tbeuni        dcmr
310700020625     c                   if        d§cmrdes1 <> *blanks
310800020625     c                   movel     d§cmrdes1     descau09a
310900020625     c                   else
311000020625     c                   movel     d§cmrdes      descau09a
311100020625     c                   endif
311200020625     c                   endif
311300040701
311400040701     c                   eval      tbeke1 = '96'
311500040701     c     ktntbe        chain     tntbe01l
311600040701     c                   if        %found(tntbe01l)
311700040701     c                   clear                   dcmr
311800040701     c                   movel     tbeuni        dcmr
311900040701     c                   if        d§cmrdes1 <> *blanks
312000040701     c                   movel     d§cmrdes1     descau10a
312100040701     c                   else
312200040701     c                   movel     d§cmrdes      descau10a
312300040701     c                   endif
312400040701     c                   endif
312500040701
312600040701     c                   eval      tbeke1 = '97'
312700040701     c     ktntbe        chain     tntbe01l
312800040701     c                   if        %found(tntbe01l)
312900040701     c                   clear                   dcmr
313000040701     c                   movel     tbeuni        dcmr
313100040701     c                   if        d§cmrdes1 <> *blanks
313200040701     c                   movel     d§cmrdes1     descau11a
313300040701     c                   else
313400040701     c                   movel     d§cmrdes      descau11a
313500040701     c                   endif
313600040701     c                   endif
313700060926
313800060926      * carico la sk delle fasi da non elaborare in statistica
313900060926     c                   eval      xc = 1
314000060926     c                   eval      tbecod = 'FAR'
314100060926     c     tbecod        setll     tntbe01l
314200060926Do  1c                   do        *hival
314300060926     c     tbecod        reade     tntbe01l
314400060926     c                   if        %eof(tntbe01l)
314500060926     c                   leave
314600060926     c                   endif
314700060926     c                   movel     tbeuni        dfar
314800060926     c                   if        d§farsta = *blanks
314900060926     c                   iter
315000060926     c                   endif
315100060926     c                   movel     tbeke1        skfar(xc)
315200060926     c                   add       1             xc
315300060926    1c                   enddo
315400010620
315500010620     c                   setoff                                       1011
315600010620
315700010705      * se richiesto apro il file della stampa di comodo EDP
315800010705     c                   if        i40edp = 'S'
315900010621     c                   open      fior41p2
316000010705     c                   eval      *in10 = *on
316100010620     c                   endif
316200010620
316300010705      * se richiesta la stampa di controllo apro il file di stampa
316400010620     c                   if        i40stpc = 'S'
316500010620     c                   open      fior41p1
316600010620     c                   eval      *in11 = *on
316700010620     c                   endif
316800010605
316900010621      * se richiesta la creazione del file lo apro
317000010621     c                   if        i40tpel = 'F'
317100010621     c                   open      wfror01l
317200010621     c                   endif
317300010621
317400010605      * chiavi
317500010605     c     korf          klist
317600010605     c                   kfld                    kormpoe
317700010605     c                   kfld                    kormnsr
317800010605     c                   kfld                    kormnor
317900010605     c                   kfld                    kormnrv
318000010604
318100010606     c     korf01        klist
318200010606     c                   kfld                    kormpoe
318300010606     c                   kfld                    kormnsr
318400010606     c                   kfld                    kormnor
318500010606     c                   kfld                    kormnrv
318600010606     c                   kfld                    kormdfo
318700010606     c                   kfld                    kormofo
318800010606     c                   kfld                    kormfao
318900010606
319000010606     c     korf02        klist
319100010606     c                   kfld                    kormpoe
319200010606     c                   kfld                    kormnsr
319300010606     c                   kfld                    kormnor
319400010606     c                   kfld                    kormnrv
319500010606     c                   kfld                    kormdfo
319600040420
319700040420     c     korf03        klist
319800040420     c                   kfld                    kormpoe
319900040420     c                   kfld                    kormnsr
320000040420     c                   kfld                    kormnor
320100040420     c                   kfld                    kormnrv
320200040420     c                   kfld                    kormfao
320300040420     c                   kfld                    kormdfo
320400040420
320500040420     c     korf04        klist
320600040420     c                   kfld                    kormpoe
320700040420     c                   kfld                    kormnsr
320800040420     c                   kfld                    kormnor
320900040420     c                   kfld                    kormnrv
321000040420     c                   kfld                    kormfao
321100010608
321200010618     c     korv          klist
321300010618     c                   kfld                    kormpoe
321400010618     c                   kfld                    kormnsr
321500010618     c                   kfld                    kormnor
321600010618     c                   kfld                    kormnrv
321700010618     c                   kfld                    korvdtv
321800010618
321900010608     c     kror          klist
322000010608     c                   kfld                    krorpoe
322100010608     c                   kfld                    krordta
322200030918     c                   kfld                    krortrc
322300030919
322400030919     c     kror1         klist
322500030919     c                   kfld                    krorpoe
322600030919     c                   kfld                    krordta
322700010606
322800010606     c     kazcln        klist
322900010606     c                   kfld                    ktfp
323000010606     c                   kfld                    ktfa
323100010606     c                   kfld                    kann
323200010606     c                   kfld                    kmes
323300010611
323400010620     c     ktab          klist
323500010620     c                   kfld                    codut
323600010620     c                   kfld                    tblcod
323700010620     c                   kfld                    tblkey
323800010620
323900010611     c     ktntbe        klist
324000010611     c                   kfld                    tbecod
324100010611     c                   kfld                    tbeke1
324200010606
324300010604     c                   endsr
324400010606      *--------------------------------------------------------------*
324500010621**   cmd - comando
324600030912CRTDUPOBJ OBJ(WFROR1*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
