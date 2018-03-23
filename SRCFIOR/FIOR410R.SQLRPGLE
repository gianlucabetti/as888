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
002100151105     fwfror21l  uf a e           k disk
002200010614
002300010614      *----------------------------------------------------*
002400010614      *  RIEPILOGO INDICATORI
002500010614      *----------------------------------------------------*
002600010614
002700030912      * 14 - ORM commissionati
002800140707      * 15 - Lettura rcd  'B' o 'C' ORM commissionati
002900140707      * 16 - ORM commissionati con Alert
003000140708      * 17 - Lettura rcd 'C' ORM commissionati con Alert
003100020808      * 30 - Lookup
003200010614      * 90 - overflow file stampa statistica
003300010621      * 91 - overflow file stampa di comodo EDP
003400010620      * 92 - overflow file stampa di controllo
003500010605
003600010605      *----------------------------------------------------*
003700010605
003800010605     d kazorg          s                   like(orgfil)
003900010604
004000010605     d kormpoe         s                   like(ormpoe)
004100010605     d kormnsr         s                   like(ormnsr)
004200010605     d kormnor         s                   like(ormnor)
004300010605     d kormnrv         s                   like(ormnrv)
004400010605     d kormdfo         s                   like(ormdfo)
004500010606     d kormofo         s                   like(ormofo)
004600010606     d kormfao         s                   like(ormfao)
004700010618     d korvdtv         s                   like(orvdtv)
004800010606
004900030912     d krortrc         s                   like(rortrc)
005000010608     d krorpoe         s                   like(rorpoe)
005100010608     d krordta         s                   like(rordae)
005200010608
005300010606     d ktfp            s                   like(clntfp)
005400010606     d ktfa            s                   like(clntfa)
005500010606     d kann            s                   like(clnann)
005600010606     d kmes            s                   like(clnmes)
005700010604
005800010608     d wgiorno         s              2  0
005900010604     d w0140           s             14  0
006000010604     d wdtgio          s              8  0
006100010604     d wora            s              6  0
006200010604     d wdate           s              8  0
006300010606     d dataela         s              8  0
006400010620
006500010606     d woksk           s              1    inz('0')
006600010606     d wpoe            s              1    inz('0')
006700010606     d wpor            s              1    inz('0')
006800010608     d wok600          s              1    inz('0')
006900010607     d wok900          s              1    inz('0')
007000010608     d wokrode         s              1    inz('0')
007100010613     d wokroes         s              1    inz('0')
007200010608     d wokrfile        s              1    inz('0')
007300010612     d wokfestae       s              1    inz('0')
007400010612     d wokfestar       s              1    inz('0')
007500010605
007600010606     d wrorode         s              5  0 inz(*zeros)
007700010620     d wrordan         s              5  0 inz(*zeros)
007800010606     d wroroes         s              5  0 inz(*zeros)
007900010614     d wrorean         s              5  0 inz(*zeros)
008000010606     d wrorone         s              5  0 inz(*zeros)
008100010620     d wrornan         s              5  0 inz(*zeros)
008200010606     d wroroea         s              5  0 inz(*zeros)
008300010620     d wroraan         s              5  0 inz(*zeros)
008400010606     d wrorn01         s              5  0 inz(*zeros)
008500010606     d wrorn02         s              5  0 inz(*zeros)
008600010606     d wrorn03         s              5  0 inz(*zeros)
008700010606     d wrorn04         s              5  0 inz(*zeros)
008800010606     d wrorn05         s              5  0 inz(*zeros)
008900010606     d wrorn06         s              5  0 inz(*zeros)
009000010606     d wrorn07         s              5  0 inz(*zeros)
009100010606     d wrorn08         s              5  0 inz(*zeros)
009200010606     d wrorn09         s              5  0 inz(*zeros)
009300010606     d wrorn10         s              5  0 inz(*zeros)
009400010606     d wrora01         s              5  0 inz(*zeros)
009500010606     d wrora02         s              5  0 inz(*zeros)
009600010606     d wrora03         s              5  0 inz(*zeros)
009700010606     d wrora04         s              5  0 inz(*zeros)
009800010606     d wrora05         s              5  0 inz(*zeros)
009900010606     d wrora06         s              5  0 inz(*zeros)
010000010606     d wrora07         s              5  0 inz(*zeros)
010100010606     d wrora08         s              5  0 inz(*zeros)
010200010606     d wrora09         s              5  0 inz(*zeros)
010300010606     d wrora10         s              5  0 inz(*zeros)
010400010606     d wrora11         s              5  0 inz(*zeros)
010500010606     d wrora12         s              5  0 inz(*zeros)
010600010606     d wrora13         s              5  0 inz(*zeros)
010700010606     d wrora14         s              5  0 inz(*zeros)
010800010606     d wrora15         s              5  0 inz(*zeros)
010900151106     d wrornfi         s              5  0 inz(*zeros)
011000151106     d wrornf01        s              5  0 inz(*zeros)
011100151106     d wrornf02        s              5  0 inz(*zeros)
011200151106     d wrornf03        s              5  0 inz(*zeros)
011300151106     d wrornf04        s              5  0 inz(*zeros)
011400151106     d wrornf05        s              5  0 inz(*zeros)
011500151106     d wrornf06        s              5  0 inz(*zeros)
011600151106     d wrornf07        s              5  0 inz(*zeros)
011700151106     d wrornf08        s              5  0 inz(*zeros)
011800151106     d wrornf09        s              5  0 inz(*zeros)
011900151106     d wrornf10        s              5  0 inz(*zeros)
012000151106     d wrorafi         s              5  0 inz(*zeros)
012100151106     d wroraf01        s              5  0 inz(*zeros)
012200151106     d wroraf02        s              5  0 inz(*zeros)
012300151106     d wroraf03        s              5  0 inz(*zeros)
012400151106     d wroraf04        s              5  0 inz(*zeros)
012500151106     d wroraf05        s              5  0 inz(*zeros)
012600151106     d wroraf06        s              5  0 inz(*zeros)
012700151106     d wroraf07        s              5  0 inz(*zeros)
012800151106     d wroraf08        s              5  0 inz(*zeros)
012900151106     d wroraf09        s              5  0 inz(*zeros)
013000151106     d wroraf10        s              5  0 inz(*zeros)
013100151106     d wroraf11        s              5  0 inz(*zeros)
013200151106     d wroraf12        s              5  0 inz(*zeros)
013300151106     d wroraf13        s              5  0 inz(*zeros)
013400151106     d wroraf14        s              5  0 inz(*zeros)
013500151106     d wroraf15        s              5  0 inz(*zeros)
013600010605
013700010611     d dspoele         s                   like(f_rorpoe)
013800010611     d dspoelr         s                   like(f_rorpoe)
013900010611
014000010620     d wdatafes        s              8  0
014100151106     d wdtda01         s              8  0
014200130118     d wORMcor         s              7s 0 inz
014300010605
014400030918     d sav_trc         s                   like(f_rortrc)
014500010608     d sav_data        s                   like(f_rordae)
014600010621     d sav_dis         s                   like(f_rordis)
014700010621     d sav_are         s                   like(f_rorare)
014800010621     d sav_poel        s                   like(f_rorpoe)
014900010620     d sav_dsdatap     s                   like(f_rordae)
015000010620     d sav_dspoelp     s                   like(f_rorpoe)
015100010620     d sav_dsdatac     s                   like(f_rordae)
015200010620     d sav_dspoelc     s                   like(f_rorpoe)
015300010614     d sav_orvdar      s                   like(orvdar)
015400010607
015500010612     d xd              s              5i 0 inz(*zeros)                          indice sk date
015600010612     d xa              s              5i 0 inz(*zeros)                          indice sk po da ela.
015700010612     d xf              s              5i 0 inz(*zeros)                          indice sk file
015800020807     d Xb              s              5i 0 inz(*zeros)                          indice sk po TRUL26
015900030918     d sxf             s              5i 0 inz(*zeros)                          indice sk file save
016000130118     d xc              s              5i 0 inz(*zeros)
016100130118     d xx              s              5i 0 inz(*zeros)
016200020808
016300020808     d StringaSql      s            500    Varying
016400010605
016500010608     d wdtda           s               d   datfmt(*iso)
016600010608     d wdtal           s               d   datfmt(*iso)
016700031002
016800010605
016900010605      *   S C H I E R E
017000010605
017100010608     d skdate          s              8  0 dim(31)                              date da elab.
017200020807     d skpoel          s              3  0 dim(250)                             po da elab.
017300030912     d kfile           s             12    dim(18000)                           file (key)
017400010605     d                                     varying
017500010606     d                                     inz
017600030912     d kfile1          s             11    dim(18000)                           file (key) appoggio
017700030912     d                                     varying
017800030912     d                                     inz
017900151105     d rfile           s            456    dim(18000)                           file (record)
018000010605     d                                     varying
018100010606     d                                     inz
018200031002
018300130118     d skcliosr        s              7s 0 dim(999)                             clienti tab OSR.
018400130118     d skstaosr        s              1    dim(999)                             OSR non da elab.
018500060926     d skfar           s              3  0 dim(999)                             fase da non elab.
018600010604
018700010604      *   D S   I N T E R N E / E S T E R N E
018800010604
018900010606     d dskfile         ds                  inz
019000030912     d  dstrc                              Like(f_rortrc)
019100010605     d  dsdata                             like(f_rordae)
019200010608     d  dspoel                             like(f_rorpoe)
019300030912
019400030912     d dskfile1        ds                  inz
019500030912     d  dsdata1                            like(f_rordae)
019600030912     d  dspoel1                            like(f_rorpoe)
019700010605
019800010604     d wlbdat          ds                  inz
019900010604     d  g02dat                 1      8  0
020000010604     d  g02inv                 9     16  0
020100010604     d  g02err                17     17
020200010604     d  g02tgi                18     22  0
020300031002
020400031002     d wosr            ds
020500031002     d  wOrmPoe                1      3
020600031002     d  wOrmNsr                4      5
020700010606
020800010606     d clnmat          ds
020900010606     d  mat                    1     31    dim(31)
021000010606     d clnpom          ds
021100010606     d  pom                    1     31    dim(31)
021200010604
021300151105     d dswfror       e ds                  extname(wfror20f)
021400010605     d                                     prefix(f_)
021500010606     d                                     inz
021600151105     d dswfror0      e ds                  extname(wfror20f)
021700010608     d                                     inz
021800140707
021900010619     d dcmr          e ds
022000151105     d fior400ds     e ds
022100010604     d cncr80        e ds
022200010604     d ut§dse0f      e ds
022300030912     d dorm01        e ds
022400031002     d dosr          e ds
022500060926     d dfar          e ds
022600020807
022700020807     d FnormxxDs     e ds                  ExtName(Fnorm00f)
022800020807
022900020807     d Trul26Ds      e ds
023000020807     d  skpo                  11    760  0 dim(250)
023100020807     d  skpot                761   1510  0 dim(250)
023200010604
023300010604     d kpjba         e ds
023400010604     d  rem                    1      3
023500010604     d  remfil                 4      6
023600140707
023700140707      // - Driver per reperire mail/sms
023800140707     d FIOR900DS     e ds                  qualified inz
023900140707
024000140707      //---------------------------------------------------------------
024100140707      //?Definizione procedure esterne.
024200140707      //---------------------------------------------------------------
024300140707      // - Driver per reperire mail/sms
024400140707     d FIOR900R        pr                  extpgm('FIOR900R')
024500140707     d  fior900ds                          likeds(fior900ds)
024600010604
024700010604      *--------------------------------------------------------------*
024800020808
024900020808      * Se sede e richiesto un solo p.o. o filiale devo elaborare con SQL
025000151105     c                   If        I40poel <> 999
025100020807      * Sql per selezionare gli ORM
025200020807      *                            ========
025300020808     c                   ExSr      SqlOrmPos
025400020807      *                            ========
025500020808     c                   EndIf
025600020807
025700010612     c                   eval      xd = 1
025800010608      * devo elaborare per data
025900010612     c                   do        31            xd
026000010612     c                   if        skdate(xd) = *zeros
026100010608     c                   leave
026200010608     c                   endif
026300010608
026400010612     c                   eval      dataela = skdate(xd)
026500020808
026600020808      * Se sede e richiesti tutti i p.o devo leggere tutto il file
026700151105     c                   If        I40poel = 999
026800020808      *                            ===========
026900020808     c                   ExSr      Sr_LeggiOrm
027000020808      *                            ===========
027100020808     c                   EndIf
027200010608     c                   enddo
027300010608
027400010612      * creo il file e stampo la sk
027500010612     c                   eval      xf = 1
027600010612     c                   do        18000         xf
027700010612      * esco se non ho più niente caricato nella sk
027800030912     c                   if        kfile1(xf) = *zeros
027900010608     c                   leave
028000010608     c                   endif
028100010612      * carico le ds
028200010612     c                   eval      dskfile = kfile(xf)
028300030912     c                   eval      dskfile1 = kfile1(xf)
028400010612     c                   eval      dswfror = rfile(xf)
028500010612      * scrivo il file
028600030912     c                   eval      krortrc = dstrc
028700010608     c                   eval      krorpoe = dspoel
028800010608     c                   eval      krordta = dsdata
028900151105     c     kror          chain     wfror21l
029000151105     c                   if        not%found(wfror21l)
029100010612     c                   eval      dswfror0 = rfile(xf)
029200010612      *                            ========
029300151106     c                   write     wfror200
029400010612      *                            ========
029500010612     c                   clear                   dswfror0
029600010608     c                   endif
029700010608
029800010608     c                   enddo
029900010605
030000010608      * fine programma
030100010605     c                   eval      *inlr = *on
030200010614
030300020807      *--------------------------------------------------------------*
030400020807      * SQL  - Preparazione sql FNORM00F
030500020807      *--------------------------------------------------------------*
030600020807     c     SqlOrmPos     BegSr
030700020807
030800020808      * Preparo la stringa sql
030900020808     c                   Eval      StringaSql =
031000020808     c                             'select * '                                 +
031100020808     c                             'from fnorm00f where ormpoe in ('
031200020808      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
031300020808     c                   Do        250           Xa
031400020808     c                   If        Skpoel(Xa) = *Zeros
031500020808     c                   Leave
031600020808     c                   EndIf
031700020808     c                   Eval      StringaSql =
031800020808     c                             %trim(StringaSql) +  ' ' +
031900020808     c                             %editc(Skpoel(Xa):'1') + ','
032000020808     c                   EndDo
032100020808     c                   Eval      StringaSql =
032200020808     c                             %trim(StringaSql) + ') or ormpor in ('
032300020808      * ciclo sulla sk p.o. da elaborare per i p.o. ritiro
032400020808     c                   Do        250           Xa
032500020808     c                   If        Skpoel(Xa) = *Zeros
032600020808     c                   Leave
032700020808     c                   EndIf
032800020808     c                   Eval      StringaSql =
032900020808     c                             %trim(StringaSql) + ' ' +
033000020808     c                             %editc(Skpoel(Xa):'1') + ','
033100020808     c                   EndDo
033200020808     c                   Eval      StringaSql =
033300020808     c                             %trim(StringaSql) + ') for fetch only'
033400020807
033500020807     C/EXEC SQL
033600020808     C+ prepare S1 from :StringaSql
033700020807     C/END-EXEC
033800020807
033900020808     C/EXEC SQL
034000020808     C+ declare C1 cursor for S1
034100020808     C/END-EXEC
034200020807
034300020807     c                   EndSr
034400020807
034500010604      *--------------------------------------------------------------*
034600020808      * LEGGO IL FILE DEGLI ORM DA SQL
034700010604      *--------------------------------------------------------------*
034800010614     c     SR_LEGGI      begsr
034900010604
035000020808     C/EXEC SQL
035100020808     C+ Open C1
035200020808     C/END-EXEC
035300020808
035400020808     C/EXEC SQL
035500020808     C+ Fetch C1 into :fnormxxds
035600020808     C/END-EXEC
035700020808
035800020808 b1  c                   Dow       Sqlcod = 0                                   *record letto
035900010608      * no gli annullati
036000020808 b2  c                   If        Ormatb = *Blanks
036100010608
036200020808      * data immissione ORM non deve essere superiore alla data elaborazione
036300020808 b3  c                   If        Ormdao <= Dataela
036400030321
036500030321      * se sono in filiale elaboro tutto, se sono in sede devo controllare anche la
036600030321      * data pulizia ORM di filiale
036700151106     c                             and Ormdao >= I40dtpo
036800031002
036900031002      * controllo se posso elaborare la serie
037000031002     c                   Move      OrmPoe        wOrmPoe
037100130118     c                   eval      *in99 = *off
037200130118     c                   IF        ORMnsr > 0
037300130118     c                   eval      wORMcor =
037400130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
037500130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
037600130118     c                   IF        xx = 0
037700130118     c                   eval      *in99 = *on
037800130118     c                   ENDIF
037900131105     c                   IF        not *in99
038000131105     c                   IF        skstaosr(xx) <> *blanks
038100130118     c                   eval      *in99 = *on
038200130118     c                   ENDIF
038300131105     c                   ENDIF
038400130118     c                   ENDIF
038500031002 b4  c                   If        Not *In99
038600020808
038700020808      *                            ==========
038800020808     c                   ExSr      Sr_Elabora
038900020808      *                            ==========
039000031002 b4  c                   EndIf
039100020808 b3  c                   EndIf
039200020808 b2  c                   EndIf
039300020808
039400020808     C/EXEC SQL
039500020808     C+ Fetch C1 into :fnormxxds
039600020808     C/END-EXEC
039700020808
039800020808 b1  c                   EndDo
039900020808
040000020808     C/EXEC SQL
040100020808     C+ close C1
040200020808     C/END-EXEC
040300020808
040400020808     c                   EndSr
040500010614
040600020808      *--------------------------------------------------------------*
040700020808      * LEGGO IL FILE DEGLI ORM
040800020808      *--------------------------------------------------------------*
040900020808     c     Sr_LeggiOrm   BegSr
041000020808
041100020808     c     *Start        Setll     Fnorm00f
041200020808     c                   Do        *Hival
041300020808     c                   Read      Fnorm00f
041400020808      * Fine file
041500020808 b1  c                   If        %Eof(Fnorm00f)
041600020808     c                   Leave
041700020808     c                   EndIf
041800020808      * no gli annullati
041900020808 b2  c                   If        Ormatb = *Blanks
042000020808
042100020808      * data immissione ORM non deve essere superiore alla data elaborazione
042200020808 b3  c                   If        Ormdao <= Dataela
042300030321
042400030321      * e controllo anche la data pulizia ORM di filiale
042500030321     c                             and Ormdao >= I40dtpo
042600031002
042700031002      * controllo se posso elaborare la serie
042800031002     c                   Move      OrmPoe        wOrmPoe
042900130118     c                   eval      *in99 = *off
043000130118     c                   IF        ORMnsr > 0
043100130118     c                   eval      wORMcor =
043200130118     c                             %dec(%subst(%editc(ORMcor:'X'):1:7):7:0)
043300130118     c                   eval      xx = %lookup(wORMcor:skCLIosr)
043400130118     c                   IF        xx = 0
043500130118     c                   eval      *in99 = *on
043600130118     c                   ENDIF
043700130118     c                   IF        skstaosr(xx) <> *blanks
043800130118     c                   eval      *in99 = *on
043900130118     c                   ENDIF
044000130118     c                   ENDIF
044100031002 b4  c                   If        Not *In99
044200020808
044300020808      *                            ==========
044400020808     c                   ExSr      Sr_Elabora
044500020808      *                            ==========
044600031002 b4  c                   EndIf
044700020808 b3  c                   EndIf
044800020808 b2  c                   EndIf
044900020808
045000020808 b1  c                   EndDo
045100020808
045200020808     c                   EndSr
045300020808
045400020808      *--------------------------------------------------------------*
045500020808      * ELABORO I DATI LETTI
045600020808      *--------------------------------------------------------------*
045700020808     c     Sr_Elabora    BegSr
045800020808
045900020808      * campi di appoggio
046000020808     c                   eval      xf = 1
046100020808     c                   eval      wpoe = *off
046200020808     c                   eval      wpor = *off
046300020808     c                   eval      wok900 = *off
046400020808     c                   eval      wok600 = *off
046500020808     c                   eval      wokrode = *off
046600020808     c                   eval      wokroes = *off
046700020808     c                   eval      wokrfile = *off
046800020808     c                   eval      wokfestae = *off
046900020808     c                   eval      wokfestar = *off
047000030912
047100030912     c                   Eval      dorm01 = OrmFlo
047200030912      * ORM commissionato
047300030918     c                   Eval      *in14 = (§OrCom = 'S')
047400140707      /free
047500140707         *in16 = *off;
047600140707       //?ORM commissionato con Alert
047700140707         fior900ds.IOR900trc = 'A';
047800140707         fior900ds.IOR900rcdm = 'MA';
047900140707         fior900ds.IOR900rcds = 'S ';
048000140707         fior900ds.IOR900poe = ORMpoe;
048100140707         fior900ds.IOR900nsr = ORMnsr;
048200140707         fior900ds.IOR900nor = ORMnor;
048300140707         fior900ds.IOR900nrv = ORMnrv;
048400140707         fior900r (fior900ds);
048500140707         IF  fior900ds.OOR900err <> 'N';
048600140707           *in16 = *on;
048700140707         ENDIF;
048800140707      /end-free
048900020808
049000020808      * controllo se il po emissione è nella sk dei p.o. da elaborare
049100020808     c                   eval      xa = 1
049200020808     c     Ormpoe        Lookup    Skpoel(Xa)                             30
049300020808     c                   If        *In30
049400020808     c                   eval      wpoe = *on
049500020808     c                   eval      dspoele = skpoel(xa)
049600020808     c                   endif
049700020808
049800020808      * controllo se il po ritiro è nella sk dei p.o. da elaborare
049900020808     c                   eval      xa = 1
050000020808     c     Ormpor        Lookup    Skpoel(Xa)                             30
050100020808     c                   If        *In30
050200020808     c                   eval      wpor = *on
050300020808     c                   eval      dspoelr = skpoel(xa)
050400020808     c                   endif
050500020808
050600020808      * se la data che sto elaborando è un festivo non devo calcolare
050700020808      *                            ========
050800020808     c                   exsr      SR_FESTA
050900020808      *                            ========
051000020808
051100020808     c                   if        wokfestae = *off and
051200020808     c                             wokfestar = *off
051300020808     c                   eval      dsdata = dataela
051400020808     c                   endif
051500151106
051600151106      * calcolo la data da - 1
051700151106     c     *iso          movel     dataela       wdtda
051800151106     c                   do        *hival
051900151106     c                   subdur    1:*d          wdtda
052000151106     c                   extrct    wdtda:*y      kann
052100151106     c                   extrct    wdtda:*m      kmes
052200151106     c                   extrct    wdtda:*d      wgiorno
052300151106     c                   clear                   ktfa
052400151106     c                   If        Dspoelr <> *Zeros
052500151106     c                   eval      ktfp = Dspoelr
052600151106     c                   Else
052700151106     c                   Eval      Ktfp = Dspoele
052800151106     c                   EndIf
052900151106     c     kazcln        chain     azcln01l
053000151106     c                   if        not%found(azcln01l)
053100151106     c                   leave
053200151106     c                   endif
053300151106     c                   if        MAT(wgiorno) =  'F'
053400151106     c                             or  POM(wgiorno) =  'F'
053500151106     c                   iter
053600151106     c                   endif
053700151106     c                   leave
053800151106     c                   enddo
053900151106     c                   move      wdtda         wdtda01
054000020808
054100020808      * pulisco i campi di comodo
054200020808      *                            ==========
054300020808     c                   exsr      SR_PULISCI
054400020808      *                            ==========
054500020808
054600020808      * faccio i controlli con il p.o. emissione
054700020808 b2  c                   if        wpoe = *on
054800020808      * se festa non faccio niente
054900020808 b3  c                   if        wokfestae = *off
055000020808      *                            ======
055100020808     c                   exsr      SR_POE
055200020808      *                            ======
055300020808 e3  c                   endif
055400020808 e2  c                   endif
055500020808
055600020808      * faccio i controlli con il p.o. ritiro
055700020808 b2  c                   if        wpor = *on
055800020808      * se festa non faccio niente
055900020808 b3  c                   if        wokfestae = *off
056000020808
056100020808      *                            ======
056200020808     c                   exsr      SR_POR
056300030916
056400030916 b4  c                   if        wokrfile = *on
056500020808      *                            ======
056600030918      * RECORD 'A'
056700020808      * controllo se esiste già un elemento di sk = alla mia key
056800020808      * e carico la sk relativa al file
056900020808     c                   eval      dspoel = dspoelr
057000030912     c                   eval      dspoel1 = dspoelr
057100030912     c                   eval      dsdata1 = dsdata
057200030918     c                   Eval      dstrc = 'A'
057300020808      *                            ==========
057400020808     c                   exsr      SR_CONTRSK
057500020808      *                            ==========
057600020808      * carico la sk del file
057700020808      *                            =========
057800020808     c                   exsr      SR_CARICA
057900020808      *                            =========
058000030918      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
058100030918     c                   Eval      sxf = xf
058200030918      *
058300140707      * RECORD 'B' ORM COMMISSIONATI  (ALERT + TELEFONO)
058400030912      * controllo se esiste già un elemento di sk = alla mia key
058500030912      * e carico la sk relativa al file
058600030916 b5  c                   If        *In14
058700030918     c                   Eval      dstrc = 'B'
058800030912      *                            ==========
058900030912     c                   exsr      SR_CONTRSK
059000030912      *                            ==========
059100030918      * carico la sk del file x record 'B' orm commissionato
059200140707      * Telefono + Alert
059300030912      *                            =========
059400030912     c                   exsr      SR_CARICA
059500030912      *                            =========
059600030916 e5  c                   EndIf
059700140707      *
059800140707      * RECORD 'C' ORM COMMISSIONATI  (SOLO ALERT)
059900140707      * controllo se esiste già un elemento di sk = alla mia key
060000140707      * e carico la sk relativa al file
060100140707 b5  c                   If        *In16
060200140707     c                   Eval      dstrc = 'C'
060300140707      *                            ==========
060400140707     c                   exsr      SR_CONTRSK
060500140707      *                            ==========
060600140707      * carico la sk del file x record 'C' orm commissionato
060700140707      * solo Alert
060800140707      *                            =========
060900140707     c                   exsr      SR_CARICA
061000140707      *                            =========
061100140707 e5  c                   EndIf
061200020808 e4  c                   endif
061300020808 e3  c                   endif
061400020808 e2  c                   endif
061500020808
061600020808     c                   EndSr
061700020808
061800010612      *--------------------------------------------------------------*
061900010612      * CONTROLLO SE LA DATA CHE STO ELABORANDO E' FESTIVO
062000010612      *--------------------------------------------------------------*
062100010614     c     SR_FESTA      begsr
062200010612
062300010612     c                   eval      wokfestae = *off
062400010612     c                   eval      wokfestar = *off
062500010612
062600010612     c     *iso          movel     dataela       wdtda
062700010612     c                   extrct    wdtda:*y      kann
062800010612     c                   extrct    wdtda:*m      kmes
062900010612     c                   extrct    wdtda:*d      wgiorno
063000011217     c                   clear                   ktfa
063100010612      * cerco per il p.o. emissione
063200010612     c                   if        wpoe = *on
063300011217     c                   eval      ktfp = dspoele
063400010612     c     kazcln        chain     azcln01l
063500010612     c                   if        %found(azcln01l) and
063600010612     c                             (MAT(wgiorno) =  'F'
063700010612     c                                    or  POM(wgiorno) =  'F')
063800010612     c                   eval      wokfestae = *on
063900010612     c                   endif
064000010612     c                   endif
064100010612
064200010612      * cerco per il p.o. ritiro
064300010612     c                   if        wpor = *on
064400011217     c                   eval      ktfp = dspoelr
064500010612     c     kazcln        chain     azcln01l
064600010612     c                   if        %found(azcln01l) and
064700010612     c                             (MAT(wgiorno) =  'F'
064800010612     c                                    or  POM(wgiorno) =  'F')
064900010612     c                   eval      wokfestar = *on
065000010612     c                   endif
065100010612     c                   endif
065200010612
065300010612     c                   endsr
065400010614
065500010605      *--------------------------------------------------------------*
065600010605      * CONTROLLO CON IL PO EMISSIONE
065700010605      *--------------------------------------------------------------*
065800010605     c     SR_POE        begsr
065900010620
066000010620      * se la data del p.o. emissione non è festa controllare se il giorno prima
066100010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
066200010620 b1  c                   if        wokfestae = *off
066300010620     c                   eval      dspoel = dspoele
066400010620      *                            ========
066500010620     c                   exsr      SR_DTFES
066600010620      *                            ========
066700010612
066800010620 b2  c                   if        ormdao = dataela or
066900010620     c                             (wdatafes > 0 and ormdao >= wdatafes and
067000010620     c                              ormdao < dataela)
067100010611     c                   eval      dspoel = dspoele
067200030912     c                   eval      dspoel1= dspoele
067300030912     c                   eval      dsdata1 = dsdata
067400030918     c                   Eval      dstrc = 'A'
067500010608      * controllo se esiste già un elemento di sk = alla mia key
067600010608      * e carico la sk relativa al file
067700010614      *                            ==========
067800010608     c                   exsr      SR_CONTRSK
067900010614      *                            ==========
068000010605
068100010612     c                   eval      rfile(xf) = dswfror
068200031202      * salvo indice schiera x stampare il rcd giusto nella stp. di controllo EDP
068300031202     c                   Eval      sxf = xf
068400010620 e2  c                   endif
068500010605
068600010620 e1  c                   endif
068700010620
068800010605     c                   endsr
068900010614
069000010620      *--------------------------------------------------------------*
069100010620      * CERCO IL PRIMO GIORNO FESTIVO PRECEDENTE
069200010620      *--------------------------------------------------------------*
069300010620     c     SR_DTFES      begsr
069400010620
069500010620      * calcolo il primo giorno festivo
069600010620     c     *iso          movel     dataela       wdtda
069700010620     c                   do        *hival
069800010620     c                   subdur    1:*d          wdtda
069900010620     c                   extrct    wdtda:*y      kann
070000010620     c                   extrct    wdtda:*m      kmes
070100010620     c                   extrct    wdtda:*d      wgiorno
070200011217     c                   clear                   ktfa
070300011217     c                   eval      ktfp = dspoel
070400010620     c     kazcln        chain     azcln01l
070500010620     c                   if        not%found(azcln01l)
070600010620     c                   leave
070700010620     c                   endif
070800010620     c                   if        MAT(wgiorno) =  'F'
070900010620     c                             or  POM(wgiorno) =  'F'
071000010620     c                   move      wdtda         wdatafes
071100010620     c                   iter
071200010620     c                   endif
071300010620     c                   leave
071400010620     c                   enddo
071500010620
071600010620     c                   endsr
071700010620
071800010608      *--------------------------------------------------------------*
071900010608      * PULISCO CAMPI DI COMODO
072000010608      *--------------------------------------------------------------*
072100010614     c     SR_PULISCI    begsr
072200010608
072300010608     c                   clear                   wrorode
072400010620     c                   clear                   wrordan
072500010608     c                   clear                   wroroes
072600010614     c                   clear                   wrorean
072700010608     c                   clear                   wrorone
072800010620     c                   clear                   wrornan
072900010608     c                   clear                   wrorn01
073000010608     c                   clear                   wrorn02
073100010608     c                   clear                   wrorn03
073200010608     c                   clear                   wrorn04
073300010608     c                   clear                   wrorn05
073400010608     c                   clear                   wrorn06
073500010608     c                   clear                   wrorn07
073600010608     c                   clear                   wrorn08
073700010608     c                   clear                   wrorn09
073800010608     c                   clear                   wrorn10
073900010608     c                   clear                   wroroea
074000010620     c                   clear                   wroraan
074100010608     c                   clear                   wrora01
074200010608     c                   clear                   wrora02
074300010608     c                   clear                   wrora03
074400010608     c                   clear                   wrora04
074500010608     c                   clear                   wrora05
074600010608     c                   clear                   wrora06
074700010608     c                   clear                   wrora07
074800010608     c                   clear                   wrora08
074900010608     c                   clear                   wrora09
075000010608     c                   clear                   wrora10
075100010608     c                   clear                   wrora11
075200010608     c                   clear                   wrora12
075300010608     c                   clear                   wrora13
075400010608     c                   clear                   wrora14
075500010608     c                   clear                   wrora15
075600151106     c                   clear                   wrornfi
075700151106     c                   clear                   wrornf01
075800151106     c                   clear                   wrornf02
075900151106     c                   clear                   wrornf03
076000151106     c                   clear                   wrornf04
076100151106     c                   clear                   wrornf05
076200151106     c                   clear                   wrornf06
076300151106     c                   clear                   wrornf07
076400151106     c                   clear                   wrornf08
076500151106     c                   clear                   wrornf09
076600151106     c                   clear                   wrornf10
076700151106     c                   clear                   wrorafi
076800151106     c                   clear                   wroraf01
076900151106     c                   clear                   wroraf02
077000151106     c                   clear                   wroraf03
077100151106     c                   clear                   wroraf04
077200151106     c                   clear                   wroraf05
077300151106     c                   clear                   wroraf06
077400151106     c                   clear                   wroraf07
077500151106     c                   clear                   wroraf08
077600151106     c                   clear                   wroraf09
077700151106     c                   clear                   wroraf10
077800151106     c                   clear                   wroraf11
077900151106     c                   clear                   wroraf12
078000151106     c                   clear                   wroraf13
078100151106     c                   clear                   wroraf14
078200151106     c                   clear                   wroraf15
078300010608
078400010608     c                   endsr
078500010614
078600010605      *--------------------------------------------------------------*
078700010605      * CONTROLLO CON IL PO RITIRO
078800010605      *--------------------------------------------------------------*
078900010605     c     SR_POR        begsr
079000010620
079100010620     c                   clear                   wdatafes
079200010620
079300010620      * se la data del p.o. ritiro non è festa controllare se il giorno prima
079400010620      * è festa per poter elaborare anche gli ORM immessi nei giorni festivi
079500010620 b1  c                   if        wokfestar = *off
079600010620     c                   eval      dspoel = dspoelr
079700010620      *                            ========
079800010620     c                   exsr      SR_DTFES
079900010620      *                            ========
080000010620 e1  c                   endif
080100010608
080200010608     c                   eval      kormpoe = ormpoe
080300010608     c                   eval      kormnsr = ormnsr
080400010608     c                   eval      kormnor = ormnor
080500010608     c                   eval      kormnrv = ormnrv
080600010612
080700010614      * se data p.o. ritiro non è festa cerco gli ORM da eseguire
080800010619      * e le modifiche della data di ritiro
080900010613 b1  c                   if        wokfestar = *off
081000010606
081100010614     ?* ORM da eseguire oggi data ritiro = data elaborazione
081200010618 b2  c                   if        ormdar = dataela
081300010618     ?* data ultima fase < data elaborazione e già eseguito non devo elaborarlo
081400010618     ?*                                                     -------------------
081500010618     c                   if        ormdfo < dataela and ormfao >= 600
081600010618     c                   goto      endpor
081700010618     c                   endif
081800010618     c                   eval      wrorode = wrorode +1
081900010612     c                   eval      wokrfile = *on
082000010614 e2  c                   endif
082100010608
082200010614     ?* ORM da eseguire (non eseguiti entro oggi) data ritiro < data elaborazione
082300010613 b2  c                   if        ormdar < dataela
082400010608     c                   eval      wokrode = *off
082500010620
082600010620     c                   eval      kormdfo = ormdfo
082700010620     c                   eval      kormofo = ormofo
082800010620     c                   eval      kormfao = ormfao
082900010620     c     korf01        setll     fnorf01l
083000010620      * se la data ultima fase "testata" è >= della data elaborazione
083100010620      * o è <  della data elaborazione
083200010620      *   e >= alla data festiva (se sto elaborando un lunedì)
083300010620      * devo leggere la fase prima
083400010620     c                   select
083500010620 b3  c                   when      ormdfo >= dataela or
083600010620 b3  c                             (ormdfo < dataela  and
083700010620     c                             (wdatafes > 0 and ormdfo >= wdatafes))
083800010620     c     korf          readpe    fnorf01l
083900010620      * se la data ultima fase "testata" è <  della data elaborazione
084000010620      * e non sto elaborando un lunedì
084100010620      * devo leggere la fase stessa
084200010620     c                   other
084300010620     c     korf          reade     fnorf01l
084400010620 e3  c                   endsl
084500010620
084600030807      * senza fase 600-900-910-999 fino alla data elaborazione esclusa
084700010614 b3  c                   dow       not %eof(fnorf01l)
084800010620 b4  c                   if        orfdae < dataela
084900010620 b5  c                   if        orffar >=600
085000011019      * Attenzione!! E' possibile che sia stata fatta una modifica fase
085100011019      * se la fase della testata ORM è 500 o è 100 e la fase precedente
085200011019      * sul file delle fasi è una fase di fatto DEVO dare l'ORM come
085300011019      * da eseguire
085400011019     c                   if        ormfao <> 500 and ormfao <> 100
085500010608     c                   leave
085600011019     c                   endif
085700010614 e5  c                   endif
085800060926      * salto le fasi che non devono essere elaborate
085900060926     c     orffar        Lookup    skfar                                  30
086000060926     c                   if        not *in30
086100010608     c                   eval      wokrode = *on
086200031105     c                   EndIf
086300010614 e4  c                   endif
086400010613     c     korf          readpe    fnorf01l
086500010614 e3  c                   enddo
086600010613     ?* da eseguire (in ritardo)
086700010613 b3  c                   if        wokrode = *on
086800010608     c                   eval      wrorode = wrorode +1
086900010608      * calcolo affidabilità. Uso >= per i da eseguire nei festivi
087000010614     c                   eval      wokrfile = *on
087100010614     ?* è stato eseguito
087200010613 x3  c                   else
087300010614      * e la data ultima fase minore data elaborazione non devo elaborarlo
087400010614      *                                                -------------------
087500010620 b4  c                   if        ormdfo < dataela
087600010608     c                   goto      endpor
087700010614 e4  c                   endif
087800010613 e3  c                   endif
087900010613 e2  c                   endif
088000040420
088100040420     ?* ORM con data ritiro > data elaborazione
088200040420      * devo controllare che non sia stata fatta una fase di dirottamento dalla data elaborazione
088300040420      * in poi
088400040420if  2c                   If        OrmDar > Dataela
088500040420     c                   Eval      kormfao = 200
088600040420     c                   Eval      kormdfo = Dataela
088700040420     c     kOrf03        Setll     Fnorf03l
088800040420      * Se trovo già una fase di dirottamento in data elaborazione non devo elabore l'ORM
088900040420      *                                                            ----------------------
089000040420     c                   If        %Equal(Fnorf03l)
089100040420     c                   Goto      endpor
089200040420     c                   EndIf
089300040615      * Altrimenti leggo dalla data statistica in poi
089400040420do  3c                   Do        *Hival
089500040420     c     kOrf04        Reade     Fnorf03l
089600040420     c                   If        %Eof(Fnorf03l)
089700040420     c                   Leave
089800040420     c                   EndIf
089900040420      * Se trovo una fase di dirottamento non devo elabore l'ORM
090000040420      *                                   ----------------------
090100040420     c                   Goto      endpor
090200040420e   3c                   EndDo
090300040420e   2c                   EndIf
090400010619
090500010619 e1  c                   endif
090600010608
090700010614      * cerco sulle fasi sempre
090800010614      *                  ------
090900010614      *                            =======
091000010606     c                   exsr      SR_FASI
091100010614      *                            =======
091200010608
091300010608     c     endpor        endsr
091400010618
091500010608      *--------------------------------------------------------------*
091600010608      * CONTROLLO LA SITUAZIONE DELL'ORM SULLE FASI
091700010608      *--------------------------------------------------------------*
091800010608     c     SR_FASI       begsr
091900010608
092000040421      * ATTENZIONE!!! la fase 200 di dirottato fa fermare la lettura delle fasi, appena
092100040421      *               la trovo esco dalla routine
092200040421
092300010608     c                   clear                   orfnor
092400010608     c                   clear                   orfpoe
092500010608     c                   clear                   orfpog
092600010608     c                   clear                   orfore
092700010608
092800010608     ?* se la data elaborazione è >= data ultima fase "testata"
092900010608     ?* mi posiziono sul file delle fasi con la data/ora/fase della
093000010608     ?* testata ORM
093100010614 b1  c                   if        dataela >= ormdfo
093200010608     c                   eval      kormdfo = ormdfo
093300010608     c                   eval      kormofo = ormofo
093400010608     c                   eval      kormfao = ormfao
093500010608     c     korf01        setll     fnorf01l
093600010608     c     korf01        reade     fnorf01l
093700010614 e1  c                   endif
093800010608     ?* se la data elaborazione è < data ultima fase  "testata"
093900010608     ?* mi posizione sul file delle fasi con la data elaborazione
094000030917     ?* fase = 0 e ora fase = a tutti '9' per leggere l'ultima fase fatta nel giorno
094100030917     ?* di elaborazione statistica
094200010614 b1  c                   if        dataela < ormdfo
094300010608     c                   eval      kormdfo = dataela
094400010608     c                   eval      kormofo = *all'9'
094500010608     c                   clear                   kormfao
094600010608     c     korf01        setgt     fnorf01l
094700010608     c     korf          readpe    fnorf01l
094800010705      * se la data di ritiro = alla data elaborazione e trovo subito il fine file
094900010705      * sulle fasi vuole dire che la prima fase è stata eseguita dopo la data elaborazione
095000010705      * quindi l'orm è non eseguito (es. orm copiati con data immissione e data ritiro = al
095100010705      * vecchio orm, ma la data della fase = alla data del giorno della copia, errore già
095200010705      * sistemato in gestione orm)
095300010705 b2  c                   if        %eof(fnorf01l)
095400010705     c                             and ormdar = dataela
095500010705     c                   eval      wrorone = wrorone +1
095600010705     c                   eval      wokrfile = *on
095700010705 e2  c                   endif
095800010614 e1  c                   endif
095900030912      * _____________
096000010608      * leggo le fasi
096100030912      * -------------
096200010613 b1  c                   dow       not %eof(fnorf01l)
096300040421      * ____________________
096400040421      * Fase 200 devo uscire
096500040421      * --------------------
096600040618if  2c                   If        OrfFar = 200
096700040421     c                   Leave
096800040421e   2c                   EndIf
096900040421
097000010608     c                   eval      wok900 = *off
097100010615     c                   eval      wok600 = *off
097200030912      * _____________________________
097300010608      * data fase = data elaborazione
097400030912      * -----------------------------
097500010620 b2  c                   if        orfdae = dataela or
097600010620     c                             (wdatafes > 0 and orfdae >= wdatafes and
097700010620     c                              orfdae < dataela)
097800010614     ?* ESEGUITI
097900010614     ?* ORM eseguiti (fase 900) ?
098000010614 b3  c                   if        orffar = 900
098100010614      * devo controllare se c'è una fase 600 prima
098200030917      * il controllo è così:
098300030917      *     - se data fase < data elaborazione e fase >= 600 è stato fatto prima
098400030917      *       quindi lo devo saltare
098500030917      *     - se no vuol dire che è stato fatto in data elaborazione e lo do per fatto
098600040421      * qui fa solo UNA readpe quindi non faccio il controllo della fase 200
098700010614     c                   eval      wokroes = *off
098800010614     c     korf          readpe    fnorf01l
098900010615 b5  c                   if        orffar >= 600  and
099000010615     c                             orfdae < dataela
099100010615     c                   eval      wok600 = *on
099200030917 x5  c                   else
099300010614     c                   eval      wokroes = *on
099400030917 e5  c                   endif
099500010614
099600010614 b4  c                   if        wokroes = *on
099700010614     c                   eval      wok900 = *on
099800010608     c                   eval      wroroes = wroroes +1
099900010608      * calcolo affidabilità. Uso >= per i ritiri nei festivi
100000151106     c                   if        ormdar > dataela
100100010614     c                   eval      wrorean = wrorean +1
100200151106     c                   endif
100300010614     c                   eval      wokrfile = *on
100400010608     c                   leave
100500010614 x4  c                   else
100600010615 b5  c                   if        wok600 = *on
100700010615     c                   leave
100800010615 e5  c                   endif
100900010613 e4  c                   endif
101000010613 e3  c                   endif
101100010614
101200010614     ?* ORM eseguiti (fase 600) ?
101300010614 b3  c                   if        orffar = 600
101400010614     c                   eval      wroroes = wroroes +1
101500010614      * calcolo affidabilità. Uso >= per i ritiri nei festivi
101600151106     c                   if        ormdar > dataela
101700010614     c                   eval      wrorean = wrorean +1
101800151106     c                   endif
101900010614     c                   eval      wokrfile = *on
102000010614     c                   leave
102100010614 e3  c                   endif
102200010612
102300010614      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
102400010613 b3  c                   if        wokfestar = *off
102500060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
102600010705      * in questo caso devo tornare a leggere la precedente
102700060926     c     orffar        Lookup    skfar                                  30
102800060926     c                   if        *in30
102900010705     c                   goto      passa
103000151106     c                   endif
103100040421     ?* ORM chiusi/eseguiti anomali (fase 910 - 999)
103200031105 b4  c                   if        orffar = 999 or OrfFar = 910
103300030917     c                   eval      wroroea = wroroea +1
103400030917      * calcolo affidabilità. Uso >= per gli anomali nei festivi
103500151106     c                   if        ormdar > dataela
103600030917     c                   eval      wroraan = wroraan +1
103700151106     c                   endif
103800030917     c                   if        orfcar = '80'
103900030917     c                   eval      wrora01 = wrora01 +1
104000151106     c                   if        ormtco = 'S'
104100151106     c                   eval      wroraf01 = wroraf01 +1
104200151106     c                   endif
104300030917     c                   endif
104400030917     c                   if        orfcar = '81'
104500030917     c                   eval      wrora02 = wrora02 +1
104600151106     c                   if        ormtco = 'S'
104700151106     c                   eval      wroraf02 = wroraf02 +1
104800151106     c                   endif
104900030917     c                   endif
105000030917     c                   if        orfcar = '90'
105100030917     c                   eval      wrora03 = wrora03 +1
105200151106     c                   if        ormtco = 'S'
105300151106     c                   eval      wroraf03 = wroraf03 +1
105400151106     c                   endif
105500030917     c                   endif
105600030917     c                   if        orfcar = '91'
105700030917     c                   eval      wrora04 = wrora04 +1
105800151106     c                   if        ormtco = 'S'
105900151106     c                   eval      wroraf04 = wroraf04 +1
106000151106     c                   endif
106100030917     c                   endif
106200030917     c                   if        orfcar = '92'
106300030917     c                   eval      wrora05 = wrora05 +1
106400151106     c                   if        ormtco = 'S'
106500151106     c                   eval      wroraf05 = wroraf05 +1
106600151106     c                   endif
106700030917     c                   endif
106800030917     c                   if        orfcar = '93'
106900030917     c                   eval      wrora06 = wrora06 +1
107000151106     c                   if        ormtco = 'S'
107100151106     c                   eval      wroraf06 = wroraf06 +1
107200151106     c                   endif
107300030917     c                   endif
107400030917     c                   if        orfcar = '94'
107500030917     c                   eval      wrora07 = wrora07 +1
107600151106     c                   if        ormtco = 'S'
107700151106     c                   eval      wroraf07 = wroraf07 +1
107800151106     c                   endif
107900030917     c                   endif
108000030917     c                   if        orfcar = '95'
108100030917     c                   eval      wrora08 = wrora08 +1
108200151106     c                   if        ormtco = 'S'
108300151106     c                   eval      wroraf08 = wroraf08 +1
108400151106     c                   endif
108500030917     c                   endif
108600031105     c                   if        orfcar = '82'
108700031105     c                   eval      wrora09 = wrora09 +1
108800151106     c                   if        ormtco = 'S'
108900151106     c                   eval      wroraf09 = wroraf09 +1
109000151106     c                   endif
109100031105     c                   endif
109200040701     c                   if        orfcar = '96'
109300040701     c                   eval      wrora10 = wrora10 +1
109400151106     c                   if        ormtco = 'S'
109500151106     c                   eval      wroraf10 = wroraf10 +1
109600151106     c                   endif
109700040701     c                   endif
109800040701     c                   if        orfcar = '97'
109900040701     c                   eval      wrora11 = wrora11 +1
110000151106     c                   if        ormtco = 'S'
110100151106     c                   eval      wroraf11 = wroraf11 +1
110200151106     c                   endif
110300040701     c                   endif
110400030917     c                   eval      wokrfile = *on
110500030917     c                   leave
110600030917 e4  c                   endif
110700010608     ?* ORM non eseguiti
110800030916 b4  c                   if        orffar < 600
110900030916     ?* con data ritiro <= data elaborazione
111000030916 b4a c                   If        ormdar <= dataela
111100010608     c                   eval      wrorone = wrorone +1
111200030917     c                   eval      wokrfile = *on
111300010608      * calcolo affidabilità. Uso >= per i non eseguiti nei festivi
111400151106     c                   if        ormdar > dataela
111500010620     c                   eval      wrornan = wrornan +1
111600151106     c                   endif
111700010608     ?* ORM non eseguiti con causale ?
111800010613 b5  c                   if        orffar = 500
111900010608     c                   if        orfcar = '01'
112000010608     c                   eval      wrorn01 = wrorn01 +1
112100151106     c                   if        ormtco = 'S'
112200151106     c                   eval      wrornf01 = wrornf01 +1
112300151106     c                   endif
112400010608     c                   endif
112500010608     c                   if        orfcar = '02'
112600010608     c                   eval      wrorn02 = wrorn02 +1
112700151106     c                   if        ormtco = 'S'
112800151106     c                   eval      wrornf02 = wrornf02 +1
112900151106     c                   endif
113000010608     c                   endif
113100010608     c                   if        orfcar = '03'
113200010608     c                   eval      wrorn03 = wrorn03 +1
113300151106     c                   if        ormtco = 'S'
113400151106     c                   eval      wrornf03 = wrornf03 +1
113500151106     c                   endif
113600010608     c                   endif
113700020527     c                   if        orfcar = '04'
113800020527     c                   eval      wrorn04 = wrorn04 +1
113900151106     c                   if        ormtco = 'S'
114000151106     c                   eval      wrornf04 = wrornf04 +1
114100151106     c                   endif
114200020527     c                   endif
114300030917     c                   leave
114400010613 e5  c                   endif
114500030917     ?* ORM non eseguiti no in fase 500 ?
114600030912      * devo controllare se nel giorno è stata fatta una fase 500
114700030912     c     kOrf02        Readpe    Fnorf01l
114800030917 b5  c                   Dow       Not %Eof(Fnorf01l)
114900040421      * se trovo fase 200 esco
115000040618if  6c                   If        OrfFar = 200
115100040421     c                   Leave
115200040421e   6c                   EndIf
115300030917 b6  c                   If        OrfFar = 500
115400030912      * se trovo la fase 500 memorizzo la causale
115500030912     c                   If        OrfCar = '01'
115600030912     c                   Eval      wrorn01 = wrorn01 +1
115700151106     c                   if        ormtco = 'S'
115800151106     c                   eval      wrornf01 = wrornf01 +1
115900151106     c                   endif
116000030912     c                   Endif
116100030912     c                   If        OrfCar = '02'
116200030912     c                   Eval      wrorn02 = wrorn02 +1
116300151106     c                   if        ormtco = 'S'
116400151106     c                   eval      wrornf02 = wrornf02 +1
116500151106     c                   endif
116600030912     c                   Endif
116700030912     c                   If        OrfCar = '03'
116800030912     c                   Eval      wrorn03 = wrorn03 +1
116900151106     c                   if        ormtco = 'S'
117000151106     c                   eval      wrornf03 = wrornf03 +1
117100151106     c                   endif
117200030912     c                   Endif
117300030912     c                   If        OrfCar = '04'
117400030912     c                   Eval      wrorn04 = wrorn04 +1
117500151106     c                   if        ormtco = 'S'
117600151106     c                   eval      wrornf04 = wrornf04 +1
117700151106     c                   endif
117800030912     c                   Endif
117900030912     c                   Leave
118000030917 e6  c                   Endif
118100030912     c     kOrf02        Readpe    Fnorf01l
118200030917 e5  c                   EndDo
118300030917     c                   Leave
118400030916 e4a c                   endif
118500030916     ?* con data ritiro > data elaborazione
118600030919 b4a c                   If        ormdar > dataela
118700030929     ?* è un ORM non fatto se nel giorno c'è una fase 500 o 390
118800031105     ?* quindi lo metto nella colonna dei non eseguiti e anche in quella dei da eseguire
118900030929 b5  c                   If        OrfFar = 500 or OrfFar = 390
119000031105     c                   eval      wrorone = wrorone +1
119100030917     c                   eval      wokrfile = *on
119200031105      * da eseguire
119300031105     c                   eval      wrorode = wrorode +1
119400031105      * sempre in anticipo xchè data ritiro > data elaborazione
119500031105     c                   eval      wrordan = wrordan +1
119600031106     c                   eval      wrornan = wrornan +1
119700030912     ?* ORM non eseguiti con causale ?
119800030929 b6  c                   If        OrfFar = 500
119900030912     c                   if        orfcar = '01'
120000031105     c                   eval      wrorn01 = wrorn01 +1
120100151106     c                   if        ormtco = 'S'
120200151106     c                   eval      wrornf01 = wrornf01 +1
120300151106     c                   endif
120400030912     c                   endif
120500030912     c                   if        orfcar = '02'
120600031105     c                   eval      wrorn02 = wrorn02 +1
120700151106     c                   if        ormtco = 'S'
120800151106     c                   eval      wrornf02 = wrornf02 +1
120900151106     c                   endif
121000030912     c                   endif
121100030912     c                   if        orfcar = '03'
121200031105     c                   eval      wrorn03 = wrorn03 +1
121300151106     c                   if        ormtco = 'S'
121400151106     c                   eval      wrornf03 = wrornf03 +1
121500151106     c                   endif
121600030912     c                   endif
121700030912     c                   if        orfcar = '04'
121800031105     c                   eval      wrorn04 = wrorn04 +1
121900151106     c                   if        ormtco = 'S'
122000151106     c                   eval      wrornf04 = wrornf04 +1
122100151106     c                   endif
122200030912     c                   endif
122300151105 e6  c                   endif
122400030929     c                   Leave
122500030917 e5  c                   endif
122600030929     ?* ORM no in fase 500 o 390 ?
122700030929      * devo controllare se nel giorno è stata fatta una fase 500 o 390
122800030912     c     kOrf02        Readpe    Fnorf01l
122900030917 b5  c                   Dow       Not %Eof(Fnorf01l)
123000040421      * se trovo fase 200 esco
123100040618if  6c                   If        OrfFar = 200
123200040421     c                   Leave
123300040421e   6c                   EndIf
123400030929 b6  c                   If        OrfFar = 500 or OrfFar = 390
123500031105     c                   eval      wrorone = wrorone +1
123600030929     c                   eval      wokrfile = *on
123700031106      * da eseguire
123800031106     c                   eval      wrorode = wrorode +1
123900031106      * sempre in anticipo xchè data ritiro > data elaborazione
124000031106     c                   eval      wrordan = wrordan +1
124100031106     c                   eval      wrornan = wrornan +1
124200030912      * se trovo la fase 500 memorizzo la causale
124300030929 b7  c                   If        OrfFar = 500
124400030912     c                   If        OrfCar = '01'
124500031105     c                   Eval      wrorn01 = wrorn01 +1
124600151106     c                   if        ormtco = 'S'
124700151106     c                   eval      wrornf01 = wrornf01 +1
124800151106     c                   endif
124900030912     c                   Endif
125000030912     c                   If        OrfCar = '02'
125100031105     c                   Eval      wrorn02 = wrorn02 +1
125200151106     c                   if        ormtco = 'S'
125300151106     c                   eval      wrornf02 = wrornf02 +1
125400151106     c                   endif
125500030912     c                   Endif
125600030912     c                   If        OrfCar = '03'
125700031105     c                   Eval      wrorn03 = wrorn03 +1
125800151106     c                   if        ormtco = 'S'
125900151106     c                   eval      wrornf03 = wrornf03 +1
126000151106     c                   endif
126100030912     c                   Endif
126200030912     c                   If        OrfCar = '04'
126300031105     c                   Eval      wrorn04 = wrorn04 +1
126400151106     c                   if        ormtco = 'S'
126500151106     c                   eval      wrornf04 = wrornf04 +1
126600151106     c                   endif
126700030912     c                   Endif
126800151105 e7  c                   Endif
126900030912     c                   Leave
127000030917 e6  c                   Endif
127100030912     c     kOrf02        Readpe    Fnorf01l
127200030917 e5  c                   EndDo
127300030917     c                   Leave
127400030916 e4a c                   endif
127500010613 e4  c                   endif
127600010608
127700010613 e3  c                   endif
127800010612
127900010613 e2  c                   endif
128000010608
128100030912      * _____________________________
128200010608      * data fase < data elaborazione
128300030912      * -----------------------------
128400010620 b2  c                   if        orfdae < dataela
128500010705
128600010614      * se ORM eseguito (fase 900) esco dal giro
128700010614     c                   if        orffar >= 900
128800010614     c                   leave
128900010614     c                   endif
129000010612      * se data ritiro maggiore data elaborazione esco dal giro
129100010612     c                   if        ormdar > dataela
129200010612     c                   leave
129300010612     c                   endif
129400010619      * se data p.o. ritiro non è festa devo cercare gli ORM non eseguiti
129500010619      * e i non bollettati
129600010619 b3  c                   if        wokfestar = *off
129700010614     ?* ORM non bollettati
129800010615     c                   if        orffar = 600 and
129900010615     c                             orfdae >= wdtda01
130000010614     c                   eval      wokrfile = *on
130100010614     c                   leave
130200010614     c                   endif
130300010615     ?* ORM già eseguiti che non devo elaborare
130400010615     c                   if        orffar = 600
130500010615     c                   leave
130600010615     c                   endif
130700060926      * controllo prima se la fase che sto leggendo è una fase che non deve essere elaborata
130800010705      * in questo caso devo tornare a leggere la precedente
130900060926     c     orffar        Lookup    skfar                                  30
131000060926     c                   if        *in30
131100010705     c                   goto      passa
131200010705 e4  c                   endif
131300010608     ?* ORM non eseguiti
131400010619 b4  c                   if        orffar < 600
131500010608     c                   eval      wrorone = wrorone +1
131600030917     c                   eval      wokrfile = *on
131700030917     ?* ORM non eseguiti con causale ?
131800030917 b5  c                   if        orffar = 500
131900030917     c                   if        orfcar = '01'
132000030917     c                   eval      wrorn01 = wrorn01 +1
132100151106     c                   if        ormtco = 'S'
132200151106     c                   eval      wrornf01 = wrornf01 +1
132300151106     c                   endif
132400030917     c                   endif
132500030917     c                   if        orfcar = '02'
132600030917     c                   eval      wrorn02 = wrorn02 +1
132700151106     c                   if        ormtco = 'S'
132800151106     c                   eval      wrornf02 = wrornf02 +1
132900151106     c                   endif
133000030917     c                   endif
133100030917     c                   if        orfcar = '03'
133200030917     c                   eval      wrorn03 = wrorn03 +1
133300151106     c                   if        ormtco = 'S'
133400151106     c                   eval      wrornf03 = wrornf03 +1
133500151106     c                   endif
133600030917     c                   endif
133700030917     c                   if        orfcar = '04'
133800030917     c                   eval      wrorn04 = wrorn04 +1
133900151106     c                   if        ormtco = 'S'
134000151106     c                   eval      wrornf04 = wrornf04 +1
134100151106     c                   endif
134200030917     c                   endif
134300030917     c                   leave
134400030917 e5  c                   endif
134500030917     ?* ORM non eseguiti no in fase 500 ?
134600030917      * devo controllare se nel giorno è stata fatta una fase 500
134700030917     c     kOrf02        Readpe    Fnorf01l
134800030917 b5  c                   Dow       Not %Eof(Fnorf01l)
134900040421      * se trovo fase 200 esco
135000040618if  6c                   If        OrfFar = 200
135100040421     c                   Leave
135200040421e   6c                   EndIf
135300030917 b6  c                   If        OrfFar = 500
135400030917      * se trovo la fase 500 memorizzo la causale
135500030917     c                   If        OrfCar = '01'
135600030917     c                   Eval      wrorn01 = wrorn01 +1
135700151106     c                   if        ormtco = 'S'
135800151106     c                   eval      wrornf01 = wrornf01 +1
135900151106     c                   endif
136000030917     c                   Endif
136100030917     c                   If        OrfCar = '02'
136200030917     c                   Eval      wrorn02 = wrorn02 +1
136300151106     c                   if        ormtco = 'S'
136400151106     c                   eval      wrornf02 = wrornf02 +1
136500151106     c                   endif
136600030917     c                   Endif
136700030917     c                   If        OrfCar = '03'
136800030917     c                   Eval      wrorn03 = wrorn03 +1
136900151106     c                   if        ormtco = 'S'
137000151106     c                   eval      wrornf03 = wrornf03 +1
137100151106     c                   endif
137200030917     c                   Endif
137300030917     c                   If        OrfCar = '04'
137400030917     c                   Eval      wrorn04 = wrorn04 +1
137500151106     c                   if        ormtco = 'S'
137600151106     c                   eval      wrornf04 = wrornf04 +1
137700151106     c                   endif
137800030917     c                   Endif
137900030917     c                   Leave
138000030917 e6  c                   Endif
138100030917     c     kOrf02        Readpe    Fnorf01l
138200030917 e5  c                   EndDo
138300010608     c                   leave
138400010619 e4  c                   endif
138500010619
138600010619 e3  c                   endif
138700010608
138800010613 e2  c                   endif
138900010705
139000010705     c     passa         tag
139100010705
139200010608     c     korf          readpe    fnorf01l
139300010705
139400010613 e1  c                   enddo
139500010612
139600010612      * se ORM in fase 900 controllo se quadrato
139700010612     c                   if        wok900 = *on
139800010614      *                            =========
139900010614     c                   exsr      SR_QUADRA
140000010614      *                            =========
140100010612     c                   endif
140200010608
140300010608     c                   endsr
140400010614
140500010608      *--------------------------------------------------------------*
140600010608      * ORM NON QUADRATO
140700010608      *--------------------------------------------------------------*
140800010614     c     SR_QUADRA     begsr
140900010608
141000010608      * ORM non quadrati
141100010608      * mi devo posizionare sulle fasi con la data elaborazione
141200010608      * poi cercare se c'è una fase 900 controllare se esiste
141300010608      * una fase 600
141400010608
141500010608     c                   eval      wok600 = *off
141600010608
141700010608     c                   if        wok900 = *on
141800010608     c                   eval      kormdfo = dataela
141900010615     c                   clear                   kormofo
142000010615     c                   clear                   kormfao
142100010608     c     korf          setll     fnorf01l
142200010608     c                   do        *hival
142300010608     c     korf          reade     fnorf01l
142400010608     c                   if        %eof(fnorf01l)
142500010608     c                   leave
142600010608     c                   endif
142700010608     c                   if        orffar = 600
142800010608     c                   eval      wok600 = *on
142900010608     c                   leave
143000010608     c                   endif
143100010608     c                   enddo
143200010608     c                   endif
143300010608
143400010608     c                   endsr
143500010614
143600010605      *--------------------------------------------------------------*
143700010605      * CONTROLLO E CARICAMENTO SK
143800010605      *--------------------------------------------------------------*
143900010605     c     SR_CONTRSK    begsr
144000010605
144100010606     c                   eval      woksk = *off
144200010606
144300010612     c                   do        18000         xf
144400030912     c                   if        kfile(xf) = dskfile
144500010605     c                   eval      woksk = *on
144600010605     c                   leave
144700010605     c                   endif
144800030912     c                   if        kfile1(xf) = *zeros
144900010605     c                   leave
145000010605     c                   endif
145100010605     c                   enddo
145200010605
145300010606     c                   if        woksk = *on
145400010612     c                   eval      dswfror = rfile(xf)
145500010606     c                   endif
145600010605
145700010605     c                   if        woksk = *off
145800010611     c                   clear                   dswfror
145900010612     c                   eval      kfile(xf) = dskfile
146000030916     c                   eval      kfile1(xf) = dskfile1
146100030916     c                   Eval      f_rortrc = dstrc
146200010605     c                   eval      f_rorpoe = dspoel
146300010605     c                   eval      f_rordae = dsdata
146400010605     c                   eval      kazorg = dspoel
146500010605     c     kazorg        chain     azorg01l
146600010605     c                   if        %found(azorg01l)
146700010605     c                   eval      f_rordis = orgfl3
146800010605     c                   eval      f_rorare = orgcar
146900010605     c                   endif
147000010605     c                   endif
147100010605
147200010605     c                   endsr
147300010614
147400010608      *--------------------------------------------------------------*
147500010608      * CARICO LA SK DEL FILE
147600010608      *--------------------------------------------------------------*
147700010614     c     SR_CARICA     begsr
147800010608
147900010608     c                   eval      f_rorode = (f_rorode + wrorode)
148000010608     c                   eval      f_roroes = (f_roroes + wroroes)
148100010608     c                   eval      f_rorone = (f_rorone + wrorone)
148200010608     c                   eval      f_rorn01 = (f_rorn01 + wrorn01)
148300010608     c                   eval      f_rorn02 = (f_rorn02 + wrorn02)
148400010608     c                   eval      f_rorn03 = (f_rorn03 + wrorn03)
148500010608     c                   eval      f_rorn04 = (f_rorn04 + wrorn04)
148600010608     c                   eval      f_rorn05 = (f_rorn05 + wrorn05)
148700010608     c                   eval      f_rorn06 = (f_rorn06 + wrorn06)
148800010608     c                   eval      f_rorn07 = (f_rorn07 + wrorn07)
148900010608     c                   eval      f_rorn08 = (f_rorn08 + wrorn08)
149000010608     c                   eval      f_rorn09 = (f_rorn09 + wrorn09)
149100010608     c                   eval      f_rorn10 = (f_rorn10 + wrorn10)
149200010608     c                   eval      f_roroea = (f_roroea + wroroea)
149300010608     c                   eval      f_rora01 = (f_rora01 + wrora01)
149400010608     c                   eval      f_rora02 = (f_rora02 + wrora02)
149500010608     c                   eval      f_rora03 = (f_rora03 + wrora03)
149600010608     c                   eval      f_rora04 = (f_rora04 + wrora04)
149700010608     c                   eval      f_rora05 = (f_rora05 + wrora05)
149800010608     c                   eval      f_rora06 = (f_rora06 + wrora06)
149900010608     c                   eval      f_rora07 = (f_rora07 + wrora07)
150000010608     c                   eval      f_rora08 = (f_rora08 + wrora08)
150100010608     c                   eval      f_rora09 = (f_rora09 + wrora09)
150200010608     c                   eval      f_rora10 = (f_rora10 + wrora10)
150300010608     c                   eval      f_rora11 = (f_rora11 + wrora11)
150400010608     c                   eval      f_rora12 = (f_rora12 + wrora12)
150500010608     c                   eval      f_rora13 = (f_rora13 + wrora13)
150600010608     c                   eval      f_rora14 = (f_rora14 + wrora14)
150700010608     c                   eval      f_rora15 = (f_rora15 + wrora15)
150800010620     c                   eval      f_rordan = (f_rordan + wrordan)
150900010614     c                   eval      f_rorean = (f_rorean + wrorean)
151000010620     c                   eval      f_rornan = (f_rornan + wrornan)
151100010620     c                   eval      f_roraan = (f_roraan + wroraan)
151200151106     c                   eval      f_rornfi = (f_rornfi + wrornfi)
151300151106     c                   eval      f_rornf01 = (f_rornf01 + wrornf01)
151400151106     c                   eval      f_rornf02 = (f_rornf02 + wrornf02)
151500151106     c                   eval      f_rornf03 = (f_rornf03 + wrornf03)
151600151106     c                   eval      f_rornf04 = (f_rornf04 + wrornf04)
151700151106     c                   eval      f_rornf05 = (f_rornf05 + wrornf05)
151800151106     c                   eval      f_rornf06 = (f_rornf06 + wrornf06)
151900151106     c                   eval      f_rornf07 = (f_rornf07 + wrornf07)
152000151106     c                   eval      f_rornf08 = (f_rornf08 + wrornf08)
152100151106     c                   eval      f_rornf09 = (f_rornf09 + wrornf09)
152200151106     c                   eval      f_rornf10 = (f_rornf10 + wrornf10)
152300151106     c                   eval      f_rorafi = (f_rorafi + wrorafi)
152400151106     c                   eval      f_roraf01 = (f_roraf01 + wroraf01)
152500151106     c                   eval      f_roraf02 = (f_roraf02 + wroraf02)
152600151106     c                   eval      f_roraf03 = (f_roraf03 + wroraf03)
152700151106     c                   eval      f_roraf04 = (f_roraf04 + wroraf04)
152800151106     c                   eval      f_roraf05 = (f_roraf05 + wroraf05)
152900151106     c                   eval      f_roraf06 = (f_roraf06 + wroraf06)
153000151106     c                   eval      f_roraf07 = (f_roraf07 + wroraf07)
153100151106     c                   eval      f_roraf08 = (f_roraf08 + wroraf08)
153200151106     c                   eval      f_roraf09 = (f_roraf09 + wroraf09)
153300151106     c                   eval      f_roraf10 = (f_roraf10 + wroraf10)
153400151106     c                   eval      f_roraf11 = (f_roraf11 + wroraf11)
153500151106     c                   eval      f_roraf12 = (f_roraf12 + wroraf12)
153600151106     c                   eval      f_roraf13 = (f_roraf13 + wroraf13)
153700151106     c                   eval      f_roraf14 = (f_roraf14 + wroraf14)
153800151106     c                   eval      f_roraf15 = (f_roraf15 + wroraf15)
153900010612     c                   eval      rfile(xf) = dswfror
154000010608
154100010608     c                   endsr
154200010614
154300010604      *--------------------------------------------------------------*
154400010604      * ROUTINE INIZIALE
154500010604      *--------------------------------------------------------------*
154600010604     c     *inzsr        begsr
154700010604      *
154800010604     c     *entry        plist
154900010604     c                   parm                    kpjba
155000010621
155100151105     c                   movel     kpjbu         fior400ds
155200010621
155300010604     c                   z-add     1             codut
155400010604     c                   call      'X§PARUT'
155500010604     c                   parm                    ut§dse0f
155600010604     c                   movel     rec80         cncr80
155700010604
155800010604      * reperisco data e ora
155900010604     c                   time                    w0140
156000010604      * udate in GGMMAAAA
156100010604     c                   move      w0140         wdate
156200010604     c                   movel     w0140         wora
156300010608
156400010608      * pulisco le sk
156500010608     c                   clear                   kfile
156600030916     c                   clear                   kfile1
156700010608     c                   clear                   rfile
156800010608     c                   clear                   skdate
156900010608     c                   clear                   skpoel
157000130118     c                   Clear                   skcliosr
157100130118     c                   Clear                   skstaosr
157200010608
157300010608      * carico la sk delle date da elaborare
157400010703     c                   eval      xd = 1
157500010703     c                   eval      skdate(xd) = i40dtda
157600010703     c                   if        i40dtda <> i40dtal
157700010703     c                   eval      wdtgio = i40dtda
157800010703     c     *iso          movel     i40dtda       wdtda
157900010703     c                   dow       wdtgio < i40dtal
158000010703     c                   eval      xd = xd +1
158100010703     c                   adddur    1:*d          wdtda
158200010703     c                   move      wdtda         wdtgio
158300010703     c                   eval      skdate(xd) = wdtgio
158400010703     c                   enddo
158500010703     c                   endif
158600020807
158700020808     c                   z-add     1             xa
158800010608
158900020807      * Elaboro tutti i p.o.
159000020807 b1  c                   if        i40poel = 999
159100020807      * Ricerco tutti i terminal di partenza
159200020807     c                   Clear                   Trul26Ds
159300020807     c                   Eval      I26tpt = 'P'
159400020807     c                   Eval      I26tla = 'L'
159500020807     c                   Call      'TRUL26R'
159600020807     c                   Parm                    Trul26Ds
159700020807      * carico i p.o. da elaborare dell'AS in un sk
159800020807 b2  c                   Do        250           Xb
159900020807     c                   If        Skpot(Xb) = *Zeros
160000020807     c                   Leave
160100020807     c                   EndIf
160200020807      * Per la filiale se lancio tutti i p.o. devo cercare quelli che
160300020807      * hanno il terminal di partenza = simfel
160400151105 b3  c                   If        simfel > 0
160500020807     c                   If        Skpot(Xb) = Simfel
160600020807     c                   Eval      Skpoel(Xa) = Skpo(Xb)
160700020808     c                   Add       1             Xa
160800010608     c                   endif
160900020807      * Per la Sede se lancio tutti i p.o. carico tutti
161000020807 b3  c                   Else
161100020807     c                   Eval      Skpoel(Xb) = Skpo(Xb)
161200020807 e3  c                   EndIf
161300010608
161400020807 e2  c                   EndDo
161500020807 e1  c                   EndIf
161600010605      * lanciato un solo po carico questo nella sk
161700010605     c                   if        i40poel <> 999
161800010612     c                   eval      skpoel(xa) = i40poel
161900010605     c                   endif
162000031002
162100031002      * carico la sk delle serie da non elaborare in statistica
162200031002     c                   Eval      xc = 1
162300031002     c                   Eval      TbeCod = 'OSR'
162400031002     c     TbeCod        Setll     Tntbe01l
162500031002Do  1c                   Do        *Hival
162600031002     c     TbeCod        Reade     Tntbe01l
162700031002     c                   If        %Eof(Tntbe01l)
162800031002     c                   Leave
162900031002     c                   EndIf
163000130122     c                   IF        TBEatb <> *blanks
163100130122     c                   iter
163200130122     c                   ENDIF
163300031002     c                   Movel     TbeUni        dOsr
163400130118      /free
163500130118           IF d§osrcli <> 0;
163600130118             xx = %lookup(d§OSRcli:skCLIosr);
163700130118             IF  xx = 0;
163800130118               xx = %lookup(*zeros:skCLIosr);
163900130118               IF  xx > 0;
164000130118                 skCLIosr(xx) = d§OSRcli;
164100130118                 skstaosr(xx) = d§OSRsta;
164200130118               ENDIF;
164300130118             ENDIF;
164400130118           ENDIF;
164500130118
164600130118           IF d§osrcl2 <> 0;
164700130118             xx = %lookup(d§OSRcl2:skCLIosr);
164800130118             IF  xx = 0;
164900130118               xx = %lookup(*zeros:skCLIosr);
165000130118               IF  xx > 0;
165100130118                 skCLIosr(xx) = d§OSRcl2;
165200130118                 skstaosr(xx) = d§OSRsta;
165300130118               ENDIF;
165400130118             ENDIF;
165500130118           ENDIF;
165600130118
165700130118           IF d§osrcl3 <> 0;
165800130118             xx = %lookup(d§OSRcl3:skCLIosr);
165900130118             IF  xx = 0;
166000130118               xx = %lookup(*zeros:skCLIosr);
166100130118               IF  xx > 0;
166200130118                 skCLIosr(xx) = d§OSRcl3;
166300130118                 skstaosr(xx) = d§OSRsta;
166400130118               ENDIF;
166500130118             ENDIF;
166600130118           ENDIF;
166700130118
166800130118           IF d§osrcl4 <> 0;
166900130118             xx = %lookup(d§OSRcl4:skCLIosr);
167000130118             IF  xx = 0;
167100130118               xx = %lookup(*zeros:skCLIosr);
167200130118               IF  xx > 0;
167300130118                 skCLIosr(xx) = d§OSRcl4;
167400130118                 skstaosr(xx) = d§OSRsta;
167500130118               ENDIF;
167600130118             ENDIF;
167700130118           ENDIF;
167800130118
167900130118           IF d§osrcl5 <> 0;
168000130118             xx = %lookup(d§OSRcl5:skCLIosr);
168100130118             IF  xx = 0;
168200130118               xx = %lookup(*zeros:skCLIosr);
168300130118               IF  xx > 0;
168400130118                 skCLIosr(xx) = d§OSRcl5;
168500130118                 skstaosr(xx) = d§OSRsta;
168600130118               ENDIF;
168700130118             ENDIF;
168800130118           ENDIF;
168900130118
169000130118           IF d§osrcl6 <> 0;
169100130118             xx = %lookup(d§OSRcl6:skCLIosr);
169200130118             IF  xx = 0;
169300130118               xx = %lookup(*zeros:skCLIosr);
169400130118               IF  xx > 0;
169500130118                 skCLIosr(xx) = d§OSRcl6;
169600130118                 skstaosr(xx) = d§OSRsta;
169700130118               ENDIF;
169800130118             ENDIF;
169900130118           ENDIF;
170000130118
170100130118           IF d§osrcl7 <> 0;
170200130118             xx = %lookup(d§OSRcl7:skCLIosr);
170300130118             IF  xx = 0;
170400130118               xx = %lookup(*zeros:skCLIosr);
170500130118               IF  xx > 0;
170600130118                 skCLIosr(xx) = d§OSRcl7;
170700130118                 skstaosr(xx) = d§OSRsta;
170800130118               ENDIF;
170900130118             ENDIF;
171000130118           ENDIF;
171100130118
171200130118           IF d§osrcl8 <> 0;
171300130118             xx = %lookup(d§OSRcl8:skCLIosr);
171400130118             IF  xx = 0;
171500130118               xx = %lookup(*zeros:skCLIosr);
171600130118               IF  xx > 0;
171700130118                 skCLIosr(xx) = d§OSRcl8;
171800130118                 skstaosr(xx) = d§OSRsta;
171900130118               ENDIF;
172000130118             ENDIF;
172100130118           ENDIF;
172200130118
172300130118           IF d§osrcl9 <> 0;
172400130118             xx = %lookup(d§OSRcl9:skCLIosr);
172500130118             IF  xx = 0;
172600130118               xx = %lookup(*zeros:skCLIosr);
172700130118               IF  xx > 0;
172800130118                 skCLIosr(xx) = d§OSRcl9;
172900130118                 skstaosr(xx) = d§OSRsta;
173000130118               ENDIF;
173100130118             ENDIF;
173200130118           ENDIF;
173300130118
173400130118           IF d§osrcl10 <> 0;
173500130118             xx = %lookup(d§OSRcl10:skCLIosr);
173600130118             IF  xx = 0;
173700130118               xx = %lookup(*zeros:skCLIosr);
173800130118               IF  xx > 0;
173900130118                 skCLIosr(xx) = d§OSRcl10;
174000130118                 skstaosr(xx) = d§OSRsta;
174100130118               ENDIF;
174200130118             ENDIF;
174300130118           ENDIF;
174400130118
174500130118           IF d§osrcl11 <> 0;
174600130118             xx = %lookup(d§OSRcl11:skCLIosr);
174700130118             IF  xx = 0;
174800130118               xx = %lookup(*zeros:skCLIosr);
174900130118               IF  xx > 0;
175000130118                 skCLIosr(xx) = d§OSRcl11;
175100130118                 skstaosr(xx) = d§OSRsta;
175200130118               ENDIF;
175300130118             ENDIF;
175400130118           ENDIF;
175500130118
175600130118           IF d§osrcl12 <> 0;
175700130118             xx = %lookup(d§OSRcl12:skCLIosr);
175800130118             IF  xx = 0;
175900130118               xx = %lookup(*zeros:skCLIosr);
176000130118               IF  xx > 0;
176100130118                 skCLIosr(xx) = d§OSRcl12;
176200130118                 skstaosr(xx) = d§OSRsta;
176300130118               ENDIF;
176400130118             ENDIF;
176500130118           ENDIF;
176600130118
176700130118           IF d§osrcl13 <> 0;
176800130118             xx = %lookup(d§OSRcl13:skCLIosr);
176900130118             IF  xx = 0;
177000130118               xx = %lookup(*zeros:skCLIosr);
177100130118               IF  xx > 0;
177200130118                 skCLIosr(xx) = d§OSRcl13;
177300130118                 skstaosr(xx) = d§OSRsta;
177400130118               ENDIF;
177500130118             ENDIF;
177600130118           ENDIF;
177700130118
177800130118           IF d§osrcl14 <> 0;
177900130118             xx = %lookup(d§OSRcl14:skCLIosr);
178000130118             IF  xx = 0;
178100130118               xx = %lookup(*zeros:skCLIosr);
178200130118               IF  xx > 0;
178300130118                 skCLIosr(xx) = d§OSRcl14;
178400130118                 skstaosr(xx) = d§OSRsta;
178500130118               ENDIF;
178600130118             ENDIF;
178700130118           ENDIF;
178800130118
178900130118           IF d§osrcl15 <> 0;
179000130118             xx = %lookup(d§OSRcl15:skCLIosr);
179100130118             IF  xx = 0;
179200130118               xx = %lookup(*zeros:skCLIosr);
179300130118               IF  xx > 0;
179400130118                 skCLIosr(xx) = d§OSRcl15;
179500130118                 skstaosr(xx) = d§OSRsta;
179600130118               ENDIF;
179700130118             ENDIF;
179800130118           ENDIF;
179900130118
180000130118           IF d§osrcl16 <> 0;
180100130118             xx = %lookup(d§OSRcl16:skCLIosr);
180200130118             IF  xx = 0;
180300130118               xx = %lookup(*zeros:skCLIosr);
180400130118               IF  xx > 0;
180500130118                 skCLIosr(xx) = d§OSRcl16;
180600130118                 skstaosr(xx) = d§OSRsta;
180700130118               ENDIF;
180800130118             ENDIF;
180900130118           ENDIF;
181000130118      /end-free
181100130118
181200031002    1c                   EndDo
181300060926
181400060926      * carico la sk delle fasi da non elaborare in statistica
181500060926     c                   eval      xc = 1
181600060926     c                   eval      tbecod = 'FAR'
181700060926     c     tbecod        setll     tntbe01l
181800060926Do  1c                   do        *hival
181900060926     c     tbecod        reade     tntbe01l
182000060926     c                   if        %eof(tntbe01l)
182100060926     c                   leave
182200060926     c                   endif
182300060926     c                   movel     tbeuni        dfar
182400060926     c                   if        d§farsta = *blanks
182500060926     c                   iter
182600060926     c                   endif
182700060926     c                   movel     tbeke1        skfar(xc)
182800060926     c                   add       1             xc
182900060926    1c                   enddo
183000010620
183100010620     c                   setoff                                       1011
183200010621
183300010605      * chiavi
183400010605     c     korf          klist
183500010605     c                   kfld                    kormpoe
183600010605     c                   kfld                    kormnsr
183700010605     c                   kfld                    kormnor
183800010605     c                   kfld                    kormnrv
183900010604
184000010606     c     korf01        klist
184100010606     c                   kfld                    kormpoe
184200010606     c                   kfld                    kormnsr
184300010606     c                   kfld                    kormnor
184400010606     c                   kfld                    kormnrv
184500010606     c                   kfld                    kormdfo
184600010606     c                   kfld                    kormofo
184700010606     c                   kfld                    kormfao
184800010606
184900010606     c     korf02        klist
185000010606     c                   kfld                    kormpoe
185100010606     c                   kfld                    kormnsr
185200010606     c                   kfld                    kormnor
185300010606     c                   kfld                    kormnrv
185400010606     c                   kfld                    kormdfo
185500040420
185600040420     c     korf03        klist
185700040420     c                   kfld                    kormpoe
185800040420     c                   kfld                    kormnsr
185900040420     c                   kfld                    kormnor
186000040420     c                   kfld                    kormnrv
186100040420     c                   kfld                    kormfao
186200040420     c                   kfld                    kormdfo
186300040420
186400040420     c     korf04        klist
186500040420     c                   kfld                    kormpoe
186600040420     c                   kfld                    kormnsr
186700040420     c                   kfld                    kormnor
186800040420     c                   kfld                    kormnrv
186900040420     c                   kfld                    kormfao
187000010608
187100010618     c     korv          klist
187200010618     c                   kfld                    kormpoe
187300010618     c                   kfld                    kormnsr
187400010618     c                   kfld                    kormnor
187500010618     c                   kfld                    kormnrv
187600010618     c                   kfld                    korvdtv
187700010618
187800010608     c     kror          klist
187900010608     c                   kfld                    krorpoe
188000010608     c                   kfld                    krordta
188100030918     c                   kfld                    krortrc
188200030919
188300030919     c     kror1         klist
188400030919     c                   kfld                    krorpoe
188500030919     c                   kfld                    krordta
188600010606
188700010606     c     kazcln        klist
188800010606     c                   kfld                    ktfp
188900010606     c                   kfld                    ktfa
189000010606     c                   kfld                    kann
189100010606     c                   kfld                    kmes
189200010611
189300010620     c     ktab          klist
189400010620     c                   kfld                    codut
189500010620     c                   kfld                    tblcod
189600010620     c                   kfld                    tblkey
189700010620
189800010611     c     ktntbe        klist
189900010611     c                   kfld                    tbecod
190000010611     c                   kfld                    tbeke1
190100010606
190200010604     c                   endsr
