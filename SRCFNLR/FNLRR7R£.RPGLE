000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400041125     Ffiqdt05l  if   e           k disk
000500041115     Ffiqdc02l  if   e           k disk
000600041115     Ffiqdo02l  if   e           k disk
000700041027     Ffiqbt01l  if   e           k disk
000800021203     Ffiapd01l  if   e           k disk
000900030723     Fazorg01l  if   e           k disk
001000041118     Ffifss01l  if   e           k disk
001100041118     Ftntbe01l  if   e           k disk
001200041026     Ftabel00f  if   e           k disk
001300041118     Fprtf198   O    F  198        PRINTER prtctl(prtds)
001400041026
001500041026     d kpjba         e ds
001600110301     d dqdtflo       e ds
001700070828     d dapdflr       e ds
001800041026     d ds2a          e ds
001900041027     d dcmr          e ds
002000090609     d ds5e          e ds
002100030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200030505     D CNCR80        E DS
002300041026
002400030723     D PARAM           DS
002500030723     d  data1                         8  0
002600030723     d  data2                         8  0
002700030723     d  aut1                          7  0
002800030723     d  aut2                          7  0
002900041115     d  dett                          1
003000041124     d  valo                          1
003100070828     d  parmes                        1
003200071031     d  sede                          1
003300120406     D partar          DS
003400120406     D  parauti                       7s 0
003500120406     D  pardati                       8s 0
003600120406     D  partsri                       1
003700120406     d  partaro                       3
003800120406     d  esito                         1
003900120406     d  error                         1
004000020404     d prtds           ds
004100020404     d   spab                         3  0
004200020404     d   spaa                         3  0
004300020404     d   skab                         3  0
004400020404     d   skaa                         3  0
004500020404     d   line                         3  0
004600041026     d exqdtpdr        s                   like(qdtpdr)
004700120508     d autzero         s              7  0
004800030724     d primo           s              1
004900041028     d dataiso         s               d   datfmt(*iso)
005000041028     d dataeur         s               d   datfmt(*eur)
005100041028      *
005200071030     d xtbolaff        s              6  0
005300120406     d xtbolmig        s              6  0
005400071030     d xtotnof         s                   like(ttotnof)
005500071030     d xtotesp         s                   like(ttotesp)
005600071030     d xtotlav         s                   like(ttotlav)
005700071030     d xtcolnos        s                   like(ttcolnos)
005800071030     d xtincnof        s                   like(ttincnof)
005900071030     d xtormaf         s                   like(ttormaf)
006000071030     d xtormnf         s                   like(ttormnf)
006100071030     d xtormnfa        s                   like(ttormnfa)
006200071030     d xtcolrit        s              6  0
006300071030     d xtcobo          s              6  0
006400071030     d xtcoldiff       s              6  0
006500071030     d xtcospn         s              6  0
006600071030     d xtbanrie        s                   like(ttbanrie)
006700110304     d xtbandbar       s                   like(ttbandbar)
006800071030     d xtbandiff       s                   like(ttbandiff)
006900071030     d xtotcom         s                   like(ttotcom)
007000120508     d xtotggl         s                   like(ttotggl)
007100071030     d xtotcos         s                   like(ttotcos)
007200071030     d xtotdifv        s                   like(ttotdifv)
007300071030     d xtotdifp        s                   like(ttotdifp)
007400071030     d wxtotdifp       s                   like(wttotdifp)
007500120410      *skiere per totalizzazione report DLO per tipo mezzo
007600120508     d skaut           s              7  0 dim(1000)
007700120412     d sk1tpm          s             20    dim(3)
007800120531     d sk2naut         s              4  0 dim(3)
007900120531     d sk2nmz          s              4  0 dim(3)
008000120531     d sk2tur          s              4  0 dim(3)
008100120410     d sk3spt          s              5  0 dim(3)
008200120510     d sk3spu          s              5  0 dim(3)
008300120510     d sk3rit          s              5  0 dim(3)
008400120510     d sk3riu          s              5  0 dim(3)
008500120410     d sk4spm          s              5  0 dim(3)
008600120410     d sk5cos          s              9  2 dim(3)
008700120410     d sk6com          s              9  2 dim(3)
008800120411     d scos            s              9  2 dim(3)
008900120411     d scop            s              5  2 dim(3)
009000120411     d scosu           s              9  2 dim(3)
009100120411     d scomu           s              9  2 dim(3)
009200120411     d scou            s              9  2 dim(3)
009300120411     d scopu           s              5  2 dim(3)
009400120411     d auval           s              5  0 dim(3)
009500120412     D skd1            s              8  0 DIM(60)
009600120531     D sk1             s              4  0 DIM(60)
009700120412     D skd2            s              8  0 DIM(60)
009800120531     d sk2             s              4  0 DIM(60)
009900120412     D skd3            s              8  0 DIM(60)
010000120531     d sk3             s              4  0 DIM(60)
010100120531     d grup            s              4  0 DIM(3)
010200120531     d sk2nautt        s              4  0
010300120531     d sk2nmzt         s              4  0
010400120531     d sk2turt         s              4  0
010500120508     d sk3sptt         s              5  0
010600120510     d sk3sput         s              5  0
010700120510     d sk3ritt         s              5  0
010800120510     d sk3riut         s              5  0
010900120508     d sk4spmt         s              5  0
011000120508     d sk5cost         s              9  2
011100120508     d sk6comt         s              9  2
011200120508     d scost           s              9  2
011300120508     d scopt           s              5  2
011400120508     d scosut          s              9  2
011500120508     d scomut          s              9  2
011600120508     d scout           s              9  2
011700120508     d scoput          s              5  2
011800000000     C*---------------------------------------------------------------*
011900020304      *
012000041025     c                   exsr      fiqdt
012100050419      *
012200041123      *ultima rottura per fine file ripete sequenza di stampa
012300041123     c                   exsr      dettagli
012400050419      *
012500041123      * una riga in stampa per ogni data autista nel periodo richiesto
012600041123     c                   if        primo <> *blank
012700041123     c                   if        line > 60
012800041123     c                   except    testata
012900041123     c                   endif
013000050420      *
013100041124      *in caso di stampa anomalie valo=' ' verifico se i valori sensibili
013200041124      * sono diversi da zero se almeno uno lo è stampo la riga
013300041124     c                   if        valo = *blank and ttcoldiff <> 0 or
013400041124     c                             valo = *blank and ttbandiff <> 0 or
013500041124     c                             valo = *blank and ttcolnos  <> 0 or
013600041124     c                             valo = *blank and ttincnof  <> 0 or
013700050208     c                             valo <> *blank or anomalo <> *blank
013800050208     c                   if        anomalo <> *blank
013900050208     c                   except    tdettAN
014000050208     c                   else
014100050208     c                   except    tdetta
014200120412      * incrementa prospetto di riepilogo se auto non fittizio
014300120510     c                   if        idx > 0 and xapdpdd = *blank
014400120410     c                   add       ttbolaff      sk3spt(idx)
014500120510     c                   add       ttormaf       sk3rit(idx)
014600120410     c                   add       ttbolmig      sk4spm(idx)
014700120410     c                   add       ttotcos       sk5cos(idx)
014800120410     c                   add       ttotcom       sk6com(idx)
014900120410     c                   endif
015000071030     c                   exsr      sommadettagli
015100041124     c                   if        dett = 'S'
015200041124     c                   except    finka
015300041124     c                   endif
015400050208     c                   end
015500041124     c                   endif
015600041123     c                   endif
015700041123
015800071030      ** calcola percentuale totale competenze costi
015900071030      *
016000071030     c                   if        *in21 and xtotcom <> 0
016100071030     c                   eval      Wxtotdifp =((xtotcom -xtotcos) *100) /xtotcom
016200071030     c                   if        wxtotdifp > 999,99
016300071030     c                   eval      xtotdifp = 999,99
016400071030     c                   else
016500071030     c                   if        wxtotdifp < -999,99
016600071030     c                   eval      xtotdifp = -999,99
016700071030     c                   else
016800071030     c                   eval      xtotdifp = Wxtotdifp
016900071030     c                   end
017000071030     c                   end
017100071030     c                   else
017200071030     c                   eval      xtotdifp = 0
017300071030     c                   endif
017400071031      ** Stampa il totale generale solo se la richiesta proviene da profilo di sede
017500100118      ********** si vuole sempre la stampa dei totali.....quindi
017600100118     c**********         if        sede <> *blank
017700071030     c                   except    sommadet
017800100118     c**********         endif
017900050419      ** Stampa la legenda
018000050419     c                   except    legend
018100120412      * prepara campi per calcolo medie ponderate se autista non fittizio
018200120411     c                   exsr      prepararie
018300120411      * esegue calcoli per prospetto di riepilogo finale per tipo mezzo
018400120411     c                   exsr      calcolorie
018500120412     c                   except    legend2
018600120412
018700020308     c                   seton                                        lr
018800990113     c*-------------------------------------------------
018900041025     c     fiqdt         begsr
019000990113     c*-------------------------------------------------
019100990113     c                   setoff                                           99
019200041112     c     kqdt          setll     fiqdt05l
019300981029     c                   do        *hival
019400041112     c     filges        reade     fiqdt05l                               99
019500981029     c   99              leave
019600070828      * verifica se scelta distinzione autisti parcel messaggerie
019700070828     c     kapd          chain     fiapd01l                           97
019800070828      * recepisce dall'anagrafica la tipologia autista parcel messaggerie
019900070828     c   97              clear                   dapdflr
020000070828     c  n97              movel     apdflr        dapdflr
020100070828     c                   select
020200070828     c                   when      parmes = 'M' and §apdtpm <> 'M'
020300070828     c                   iter
020400070828     c                   when      parmes = 'P' and §apdtpm = 'M'
020500070828     c                   iter
020600070828     c                   endsl
020700050420      *
020800050207     c     kqbt          chain     fiqbt01l                           95
020900030724      * elimino i record che non rientrano nei parametri richiesti
021000041025     c                   if        qdtddc >= data1 and qdtddc <= data2 and
021100041025     c                             qdtpdr >= aut1  and qdtpdr <= aut2
021200041027
021300041027      *primo giro forzo codice padroncino per testare rottura calcoli totale
021400041027     c                   if        primo  = *blank
021500050624     c                   if        qdtdaeco = 0 or qdtdaeor = 0 or
021600050624     c                             not *in95 and  qbtdaebo = 0
021700050609     c                   move      'X'           anomalo
021800050609     c                   endif
021900041027     c                   z-add     qdtpdr        expdr             7 0
022000041027     c                   z-add     qdtddc        exddc             8 0
022100041027     c                   move      'X'           primo             1
022200120410      * dettagli autista
022300120410     c                   exsr      datiautista
022400120410     c                   endif
022500041027      * prepara dati per colli non bollettati e bancali a rottura autista
022600041112     c                   if        expdr <> qdtpdr
022700041123     c
022800041118     c                   exsr      dettagli
022900041116
023000041112     c                   if        line > 60
023100041112     c                   except    testata
023200041112     c                   endif
023300041112
023400041112      *totali dettaglio per periodo padroncino
023500041124      *in caso di stampa anomalie valo=' ' verifico se i valori sensibili
023600041124      * sono diversi da zero se almeno uno lo è stampo la riga
023700041124     c                   if        valo = *blank and ttcoldiff <> 0 or
023800041124     c                             valo = *blank and ttbandiff <> 0 or
023900041124     c                             valo = *blank and ttcolnos  <> 0 or
024000041124     c                             valo = *blank and ttincnof  <> 0 or
024100050208     c                             valo <> *blank or anomalo <> *blank
024200050208     c                   if        anomalo <> *blank
024300050208     c                   except    TdettAN
024400050208     c                   else
024500050208     c                   except    Tdetta
024600120510     c                   if        idx > 0 and xapdpdd = *blank
024700120410     c                   add       ttbolaff      sk3spt(idx)
024800120510     c                   add       ttormaf       sk3rit(idx)
024900120410     c                   add       ttbolmig      sk4spm(idx)
025000120410     c                   add       ttotcos       sk5cos(idx)
025100120410     c                   add       ttotcom       sk6com(idx)
025200120410     c                   endif
025300071030     c                   exsr      sommadettagli
025400041116     c                   if        dett = 'S'
025500041116     c                   except    finka
025600041116     c                   endif
025700050208     c                   end
025800041124     c                   endif
025900050208     c                   clear                   anomalo           1
026000050208      * se rekord non ancora trattato dall'elaborazione skedulata di
026100050208      * valorizzazione consegne orm o bollettazione segnalo l'anomalia
026200050624     c                   if        qdtdaeco = 0 or qdtdaeor = 0 or
026300050624     c                             not *in95 and  qbtdaebo = 0
026400050208     c                   move      'X'           anomalo
026500050208     c                   endif
026600050208
026700041027     c* azzero i dati per il totale  autista/data relativi ai BANCALI
026800041115     c                   clear                   ttbolaff          5 0
026900120406     c                   clear                   ttbolmig          5 0
027000041112     c                   clear                   ttotnof           5 0
027100041112     c                   clear                   ttotesp           5 0
027200041112     c                   clear                   ttotlav           5 0
027300041115     c                   clear                   ttcolnos          5 0
027400041115     c                   clear                   ttincnof          9 2
027500041115     c                   clear                   ttormaf           5 0
027600041112     c                   clear                   ttormnf           5 0
027700041112     c                   clear                   ttormnfa          5 0
027800050419     c                   clear                   ttbandiff         3 0
027900050419     c                   clear                   ttbanrie          3 0
028000050420     c                   clear                   FFtbanrie         1
028100110304     c                   clear                   ttbandbar         3 0
028200050420     c                   clear                   FFtbanaff         1
028300041115     c                   clear                   ttcoldiff         5 0
028400050420     c                   clear                   FFtcoldif         1
028500050419     c                   clear                   ttcobo            5 0
028600050420     c                   clear                   ttcospn           5 0
028700050420     c                   clear                   FFtcospn          1
028800050419     c                   clear                   ttcolrit          5 0
028900050420     c                   clear                   FFtcolrit         1
029000041027
029100041118     c                   clear                   ttotcos           9 2
029200041118     c                   clear                   ttotcom           9 2
029300120508     c                   clear                   ttotggl           3 0
029400120508     c                   clear                   ttotdifv          5 0
029500041123     c                   clear                   ttotdifp          5 2
029600050419     c                   clear                   wttotdifp         9 2
029700041027      * imposta dati nuovo autista o data dopo totali
029800041027     c                   z-add     qdtpdr        expdr
029900041027     c                   z-add     qdtddc        exddc
030000120410      * dettagli autista
030100120410     c                   exsr      datiautista
030200120411     c                   endif
030300041115
030400041115      * stampa  dettagli x data/autista se richiesto
030500041118      * a rottura di data o autista
030600041118     c*                  if        exddc <> qdtddc or expdr <> qdtpdr
030700041118     c                   if        exddc <> qdtddc
030800041115
030900050208      * se rekord non ancora trattato dall'elaborazione skedulata di
031000050208      * valorizzazione consegne orm o bollettazione segnalo l'anomalia
031100050624     c                   if        qdtdaeco = 0 or qdtdaeor = 0 or
031200050624     c                             not *in95 and  qbtdaebo = 0
031300050208     c                   move      'X'           anomalo
031400050208     c                   endif
031500050208
031600041118     c                   exsr      dettagli
031700041118      *
031800041116     c                   z-add     qdtpdr        expdr
031900041116     c                   z-add     qdtddc        exddc
032000041115
032100041115     c                   endif
032200041115      *  totalizzo se stesso autista e stessa data i dati testata
032300041115     c* se esistono più distinte esistono più rekord
032400050208     c                   if        anomalo = *blank
032500110301     c*                  add       qdtnbnaf      banaff            5 0
032600110310     c                   movel     qdtflo        dqdtflo
032700110304     c                   clear                   banbdar           5 0
032800110301     c                   if        §QDTNBDAR <> *blank
032900110304     c                   move      §qdtnbdar     banbdar           5 0
033000110301     c                   endif
033100050420     c                   if        Fbanaff = *blank or qdtFNBAF ='N'
033200050420     c                   move      qdtFNBAF      Fbanaff
033300050420     c                   end
033400041027     c                   add       qdtnbnri      banrie            5 0
033500050210     c                   add       qdtnbnbr      banrie
033600050420     c                   if        Fbanrie = *blank or qdtFNBRI ='N'
033700050420     c                   move      qdtFNBRI      Fbanrie
033800050420     c                   end
033900041027     c                   add       qdtnclrit     colrit            5 0
034000050420     c                   if        Fcolrit = *blank or qdtFNCRIT ='N'
034100050420     c                   move      qdtFNCRIT     Fcolrit
034200050420     c                   end
034300041115     c                   add       qdtnblaf      tbolaff
034400120406     c                   add       qdtnblmig     tbolmig
034500041115     c                   add       qdtnclns      tcolnos
034600041115     c                   add       qdtormaf      tormaf
034700041115     c                   add       qdtormnf      tormnf
034800041115      *incassi
034900041115     c                   add       qdtcasnf      tincnof
035000041115     c                   add       qdtiftnf      tincnof
035100050208     c                   endif
035200020404
035300041027      *data in stampa
035400041025     c                   move      qdtddc        dataiso
035500041025     c                   move      dataiso       dataeur
035600041025     c                   move      dataeur       qdtddcs           8 0
035700041025
035800041115      *data e autista all'interno del range richiesto a video
035900120410     c                   end
036000020404
036100020304     c                   enddo
036200041027      * ultimo totale x fine file
036300020304
036400020304     c                   endsr
036500981029      *
036600041026     c*-------------------------------------------------
036700041026     c     Non_cons      begsr
036800041026     c*-------------------------------------------------
036900041115     c     kdet          setll     fiqdc02l
037000041026     c                   do        *hival
037100041115     c     kdet          reade     fiqdc02l                               98
037200041026     c   98              leave
037300041026      * considero solo le testatine o meglio le righe che contengono
037400041026      * il codice evento riferito alla bolla gli altri rekord sono
037500041026      * il dettaglio dei colli non spuntati
037600041028      *
037700041026     c                   if        qdccev = *blank
037800041026     c                   iter
037900041026     c                   endif
038000041026      * verifico sulla tabella eventi la tipologia per totalizzare
038100041026      * le diverse motivazioni di non consegna
038200041026     c                   clear                   ds2a
038300041026     c                   move      '2A'          kcod
038400041026     c                   movel(p)  qdccev        kkey
038500041026     c     ktab          chain     tabel00f                           96
038600041026     c                   if        not *in96
038700041026     c                   movel     tbluni        ds2a
038800041026     c                   select
038900041026      * lasciati avvisi
039000041026     c                   when      §2aftc = 'A'
039100041112     c                   add       1             totlav            5 0
039200041026
039300041026      * Non fatte
039400041026     c                   when      §2aftc = 'N'
039500041112     c                   add       1             totnof            5 0
039600091008      * espressi o  h10:30
039700091008     c                   if        qdctsp = 'E' or qdctsp = 'H'
039800041112     c                   add       1             totesp            5 0
039900041112     c                   endif
040000041026
040100041026      * Giacenze
040200041026     c                   when      §2aftc = 'G'
040300041115     c                   add       1             totgia            5 0
040400041026
040500041026     c                   endsl
040600041026     c                   endif
040700041026     c                   enddo
040800041026     c                   endsr
040900041027     c*-------------------------------------------------
041000041027     c     ORM_noes      begsr
041100041027     c*-------------------------------------------------
041200041115     c     kdet          setll     fiqdo02l
041300041027     c                   do        *hival
041400041115     c     kdet          reade     fiqdo02l                               98
041500041027     c   98              leave
041600041027     c                   if        qdocar = *blank
041700041027     c                   iter
041800041027     c                   endif
041900041027      * reperisco dalla tabella la decodifica della causale x non eseguito
042000041027     c                   clear                   dcmr
042100041027     c                   move      'CMR'         tbecod
042200041027     c                   movel(p)  qdocar        tbeke1
042300041027     c     ktbe          chain     tntbe01l                           96
042400041027     c                   if        not *in96
042500041027     c                   movel     tbeuni        dcmr
042600041112      * non fatto con causale a responsabilità autista
042700041112     c                   if        d§cmrrea = 'S'
042800041112     c                   add       1             tormnfa           5 0
042900041112     c                   endif
043000041027     c                   endif
043100041027
043200041027     c                   enddo
043300041027     c                   endsr
043400041115     c*-------------------------------------------------
043500041115     c     Col_nobo      begsr
043600041115     c*-------------------------------------------------
043700041115      * totalizza i colli bollettati per spedizione da confrontare con
043800041115      * i colli ritirati dichiarati in distinta
043900041115      *
044000041115     c                   clear                   totcobo           5 0
044100050419     c                   clear                   tcobo             5 0
044200050420     c                   clear                   totcospn          5 0
044300050420     c                   clear                   fcospn            1
044400050420     c                   clear                   tcospn            5 0
044500050420     c                   clear                   ftcospn           1
044600041115     c     kdet          setll     fiqbt01l
044700050420      *
044800041115     c                   do        *hival
044900041115     c     kdet          reade     fiqbt01l                               98
045000041115     c   98              leave
045100041115     c                   add       qbtnclbo      totcobo
045200050420     c                   add       qbtnclsp      totcospn
045300050420      * se squadrati memorizza il flag
045400050420     c                   if        fcospn = *blank
045500050420     c                   move      qbtfquad      fcospn
045600050420     c                   end
045700041115     c                   enddo
045800050420
045900041115     c                   endsr
046000041118     c*-------------------------------------------------
046100041118     c     val_aut       begsr
046200041118     c*-------------------------------------------------
046300041118      * totalizza i colli bollettati per spedizione da confrontare con
046400041118      * i colli ritirati dichiarati in distinta
046500041118      *
046600041118     c     kdet          chain     fifss01l                           98
046700041118     c                   if        not *in98
046800041118     c                   add       fss001        totcos
046900041118     c                   add       fss005        totcos
047000041118     c                   add       fss016        totcos
047100041118     c                   add       fss019        totcos
047200041118     c                   add       fss034        totcos
047300041118     c                   add       fss035        totcos
047400041118     c                   add       fss997        totcos
047500041118     c                   add       fss998        totcos
047600041118     c                   add       fssxx1        totcos
047700041118     c                   add       fssxx2        totcos
047800041118     c
047900041118     c                   add       fss001b       totcom
048000041118     c                   add       fss005b       totcom
048100041118     c                   add       fss016b       totcom
048200041118     c                   add       fss019b       totcom
048300041118     c                   add       fss034b       totcom
048400041118     c                   add       fss035b       totcom
048500041118     c                   add       fss997b       totcom
048600041118     c                   add       fss998b       totcom
048700041118     c                   add       fssxx1b       totcom
048800041118     c                   add       fssxx2b       totcom
048900120508     c                   add       1             totggl
049000041118     c     totcom        sub       totcos        totdifv
049100050208     c                   if        totcom <> 0
049200050419     c                   eval      wtotdifp = ((totcom -totcos) * 100) /totcom
049300050419     c                   if        wtotdifp > 999,99
049400050419     c                   eval      totdifp = 999,99
049500050419     c                   else
049600050419     c                   if        wtotdifp < -999,99
049700050419     c                   eval      totdifp = -999,99
049800050419     c                   else
049900050419     c                   eval      totdifp = wtotdifp
050000050419     c                   end
050100050419     c                   end
050200050208     c                   else
050300050208     c                   eval      totdifp = 0
050400050208     c                   endif
050500120412      * conteggia numero valorizzazioni(autisti) per gruppo nel periodo
050600120412      * solo se autista non fittizio
050700120412     c                   if        APDPDD = *blank
050800120411     c                   add       1             auval(idx)
050900120411     c                   z-add     1             x                 2 0
051000120411     c                   clear                   vuoto8            8 0
051100120411     c                   select
051200120412      * gruppo 1 micro
051300120412      * memorizzo date relative al gruppo mezzo
051400120411     c                   when      idx = 1
051500120412     c     exddc         lookup    skd1(x)                                90
051600120412     c                   if        not *in90
051700120412     c     vuoto8        lookup    skd1(x)                                90
051800120412     c                   z-add     exddc         skd1(x)
051900120412     c                   endif
052000120412      *aggiungo l'autista
052100120411     c                   add       1             sk1(x)
052200120412      * gruppo 2
052300120411     c                   when      idx = 2
052400120412     c     exddc         lookup    skd2(x)                                90
052500120412     c                   if        not *in90
052600120412     c     vuoto8        lookup    skd2(x)                                90
052700120412     c                   z-add     exddc         skd2(x)
052800120412     c                   endif
052900120411     c                   add       1             sk2(x)
053000120412      * gruppo 2
053100120411     c                   when      idx = 3
053200120412     c     exddc         lookup    skd3(x)                                90
053300120412     c                   if        not *in90
053400120412     c     vuoto8        lookup    skd3(x)                                90
053500120412     c                   z-add     exddc         skd3(x)
053600120412     c                   endif
053700120411     c                   add       1             sk3(x)
053800120411     c                   endsl
053900120412     c                   endif
054000120412      *
054100041118     c                   endif
054200041118
054300041118     c                   endsr
054400041118     c*-------------------------------------------------
054500041118     c     dettagli      begsr
054600041118     c*-------------------------------------------------
054700041118      * calcola i valori per singola data nel periodo richiesto a video
054800041118
054900041118      *consegne
055000041118     c                   exsr      NoN_Cons
055100041118
055200041118      *orm
055300041118     c                   exsr      ORM_noes
055400041118
055500041118      *bollettato
055600041118     c                   exsr      col_nobo
055700041118
055800041118      *valorizzazioni
055900041124     c   21              exsr      val_aut
056000041118
056100050208     c                   if        anomalo = *blank
056200050419     c                   z-add     banrie        tbanrie
056300050420     c                   move      Fbanrie       Ftbanrie
056400110304     c                   z-add     banbdar       tbandbar
056500050420     c                   move      Fbanaff       Ftbanaff
056600110304     c     banrie        sub       banbdar       tbandiff
056700050419     c                   z-add     colrit        tcolrit
056800050420     c                   move      fcolrit       ftcolrit
056900050419     c                   z-add     totcobo       tcobo
057000050420     c                   z-add     totcospn      tcospn
057100050420     c                   move      fcospn        ftcospn
057200041118     c     colrit        sub       totcobo       tcoldiff
057300050420
057400050420      **  Segnala con (*) se a fronte di una differenza
057500050420      **    vi è una differenza anche fra gli spuntati e i bollettati
057600050420     c                   if        tcoldiff <> 0 and
057700050420     c                             tcospn   <> tcobo
057800050420     c                   move      '*'           ftcoldif
057900050420     c                   end
058000041118
058100041118      * sommo sui campi di totale per autista periodo
058200041118     c                   add       tbolaff       ttbolaff
058300120406     c                   add       tbolmig       ttbolmig
058400041118     c                   add       totnof        ttotnof
058500041118     c                   add       totesp        ttotesp
058600041118     c                   add       totlav        ttotlav
058700041118     c                   add       tincnof       ttincnof
058800041118     c                   add       tcolnos       ttcolnos
058900041118     c                   add       tormaf        ttormaf
059000041118     c                   add       tormnf        ttormnf
059100041118     c                   add       tormnfa       ttormnfa
059200041118     c                   add       totcos        ttotcos
059300041118     c                   add       totcom        ttotcom
059400120508     c                   add       totggl        ttotggl
059500041118
059600050419     c                   add       tbanrie       ttbanrie
059700110304     c                   add       tbandbar      ttbandbar
059800041118     c                   add       tbandiff      ttbandiff
059900050419     c                   add       tcolrit       ttcolrit
060000050419     c                   add       tcoldiff      ttcoldiff
060100050420     c                   move      Ftbanrie      FFtbanrie
060200050420     c                   move      Ftbanaff      FFtbanaff
060300050420     c                   move      Ftcolrit      FFtcolrit
060400050420     c                   move      Ftcoldif      FFtcoldif
060500050419     c                   add       tcobo         ttcobo
060600050420     c                   add       tcospn        ttcospn
060700050420     c                   move      Ftcospn       FFtcospn
060800041118     c                   add       totdifv       ttotdifv
060900050208     c                   if        *in21 and ttotcom <> 0
061000050419     c                   eval      Wttotdifp =((ttotcom -ttotcos) *100) /ttotcom
061100050419     c                   if        wttotdifp > 999,99
061200050419     c                   eval      ttotdifp = 999,99
061300050419     c                   else
061400050419     c                   if        wttotdifp < -999,99
061500050419     c                   eval      ttotdifp = -999,99
061600050419     c                   else
061700050419     c                   eval      ttotdifp = Wttotdifp
061800050419     c                   end
061900050419     c                   end
062000050208     c                   else
062100050208     c                   eval      ttotdifp = 0
062200050208     c                   endif
062300050208     c                   endif
062400041118      * una riga in stampa per ogni data autista nel periodo richiesto
062500041118     c                   if        dett = 'S'
062600041118     c                   if        line > 60
062700041118     c                   except    testata
062800041118     c                   endif
062900041118      *data in stampa x dettaglio
063000060118     c                   if        primo <> *blank
063100041118     c                   move      exddc         dataiso
063200041118     c                   move      dataiso       dataeur
063300041118     c                   move      dataeur       exddcs            8 0
063400060118     c                   endif
063500041124      *in caso di stampa anomalie valo=' ' verifico se i valori sensibili
063600041124      * sono diversi da zero se almeno uno lo è stampo la riga
063700041124     c                   if        valo = *blank and tcoldiff <> 0 or
063800041124     c                             valo = *blank and tbandiff <> 0 or
063900041124     c                             valo = *blank and tcolnos  <> 0 or
064000041124     c                             valo = *blank and tincnof  <> 0 or
064100050208     c                             valo <> *blank or anomalo <> *blank
064200050208     c                   if        anomalo <> *blank
064300050208     c                   except    dettaN
064400050208     c                   else
064500050208     c                   except    detta
064600050208     c                   end
064700041118     c                   endif
064800041124     c                   endif
064900041118     c* azzero campi dettaglio per data successiva
065000110304     c                   clear                   banbdar           5 0
065100050420     c                   clear                   Fbanaff           1
065200050208     c                   clear                   banrie            5 0
065300050420     c                   clear                   Fbanrie           1
065400050208     c                   clear                   colrit            5 0
065500050420     c                   clear                   Fcolrit           1
065600050419     c                   clear                   tcolrit           5 0
065700050420     c                   clear                   Ftcolrit          1
065800041118     c                   clear                   tbolaff           5 0
065900120406     c                   clear                   tbolmig           5 0
066000041118     c                   clear                   totnof            5 0
066100041118     c                   clear                   totesp            5 0
066200041118     c                   clear                   totlav            5 0
066300041118     c                   clear                   tormaf            5 0
066400041118     c                   clear                   tormnf            5 0
066500041118     c                   clear                   tormnfa           5 0
066600041118     c                   clear                   tincnof           9 2
066700041118     c                   clear                   tcolnos           5 0
066800050419     c                   clear                   tbandiff          3 0
066900050419     c                   clear                   tbanrie           3 0
067000050420     c                   clear                   Ftbanrie          1
067100110304     c                   clear                   tbandbar          3 0
067200050420     c                   clear                   Ftbanaff          1
067300041118     c                   clear                   tcoldiff          5 0
067400050420     c                   clear                   Ftcoldif          1
067500041118     c                   clear                   totcom            9 2
067600120508     c                   clear                   totggl            3 0
067700041118     c                   clear                   totcos            9 2
067800120508     c                   clear                   totdifv           5 0
067900041123     c                   clear                   totdifp           5 2
068000050419     c                   clear                   wtotdifp          9 2
068100041118
068200041118     c                   endsr
068300071030     c*-------------------------------------------------
068400071030     c     sommadettagli begsr
068500071030     c*-------------------------------------------------
068600071030      * somma valori di dettaglio per ottenere il totale dell'estrazione
068700071030     c                   add       ttbolaff      xtbolaff
068800120406     c                   add       ttbolmig      xtbolmig
068900071030     c                   add       ttotnof       xtotnof
069000071030     c                   add       ttotesp       xtotesp
069100071030     c                   add       ttotlav       xtotlav
069200071030     c                   add       ttcolnos      xtcolnos
069300071030     c                   add       ttincnof      xtincnof
069400071030     c                   add       ttormaf       xtormaf
069500071030     c                   add       ttormnf       xtormnf
069600071030     c                   add       ttormnfa      xtormnfa
069700071030     c                   add       ttcolrit      xtcolrit
069800071030     c                   add       ttcobo        xtcobo
069900071030     c                   add       ttcoldiff     xtcoldiff
070000071030     c                   add       ttcospn       xtcospn
070100071030     c                   add       ttbanrie      xtbanrie
070200110304     c                   add       ttbandbar     xtbandbar
070300071030     c                   add       ttbandiff     xtbandiff
070400071030     c                   add       ttotcom       xtotcom
070500120508     c                   add       ttotggl       xtotggl
070600071030     c                   add       ttotcos       xtotcos
070700071030     c                   add       ttotdifv      xtotdifv
070800071030     c                   endsr
070900120410     c*----------------------------------------------------
071000120410     c     datiautista   begsr
071100120410     c*----------------------------------------------------
071200120410      *decodifica autista
071300120410     c                   clear                   despdr           20
071400120412     c                   clear                   massa             5 1
071500120410     c                   clear                   kilom             3
071600120410     c                   z-add     1             idx
071700120410     c     kapdf         chain     fiapd01l
071800120410      *se l'anagrafica non corrisponde non stampo la decodifica
071900120410     c                   if        %found(fiapd01l)
072000120410     c                   movel     apdrsc        despdr
072100120510     c                   movel     apdpdd        xapdpdd           1
072200120410     c                   movel     apdflr        dapdflr
072300120410     c                   movel     §apdmcp       massa
072400120410     c                   movel     §apdkm        kilom
072500120410     c                   endif
072600120410      * reperimento indice totalizzazione skiere correlate per massa mezzo
072700120412
072800120412      * se autista fittizio cioè non fattura non rientra nei conteggi di riepilogo
072900120412     c                   if        APDPDD = *blank
073000120410     c                   move      'MAS'         tbecod
073100120411     c                   movel     §apdmcp       tbeke1
073200120410     c     ktbe          setgt     tntbe01l
073300120410     c                   readp     tntbe01l
073400120410     c                   if        not%eof(tntbe01l)
073500120410     c                   movel     tbeuni        vuoto2
073600120410     c     vuoto2        lookup    sk1tpm(idx)                            96
073700120411     c   96              add       1             sk2nmz(idx)
073800120508      *conteggio autisti impiegati per turnazione
073900120508      *memorizza codici
074000120508     c                   z-add     1             ida               3 0
074100120508     c     apdpdr        lookup    skaut(ida)                             96
074200120508     c                   if        not *in96
074300120508     c                   add       1             sk2naut(idx)
074400120508     c                   z-add     1             ida
074500120508     c     autzero       lookup    skaut(ida)                             96
074600120508     c   96              add       apdpdr        skaut(ida)
074700120508     c                   endif
074800120410     c                   endif
074900120410      * reperimento tariffa
075000120410     c                   clear                   tariffa           3
075100120412     c                   clear                   partar
075200120412     c                   move      expdr         parauti
075300120412     c                   move      exddc         pardati
075400120412     c                   move      'G'           partsri
075500120410     c                   call      'FNLRR7TR'
075600120412     c                   parm                    partar
075700120410     c                   if        error = *blank and esito ='1'
075800120410     c                   move      partaro       tariffa
075900120410     c                   endif
076000120412     c                   endif
076100120410     c                   endsr
076200120411     c*-------------------------------------------------
076300120411     c     prepararie    begsr
076400120411     c*-------------------------------------------------
076500120412      *gruppo 1 calcolo media autisti al lavoro per giornate di lavoro per gruppo mezzo
076600120411     c                   z-add     1             x                 2 0
076700120411     c                   clear                   vuoto8            8 0
076800120412     c     vuoto8        lookup    skd1(x)                                90
076900120411     c     x             sub       1             ngior             2 0
077000120411     c                   xfoot     sk1           grup(1)
077100120412     c                   if        grup(1) > 0 and ngior > 0
077200120412     c     grup(1)       div       ngior         sk2nmz(1)
077300120516     c                   if        sk2naut(1) <> 0
077400120508     c                   eval      sk2tur(1)= (sk2naut(1) - sk2nmz(1))*100
077500120510     c                             / sk2naut(1)
077600120516     c                   endif
077700120412     c                   endif
077800120412      *gruppo 2
077900120412     c                   z-add     1             x
078000120412     c     vuoto8        lookup    skd2(x)                                90
078100120412     c     x             sub       1             ngior
078200120411     c                   xfoot     sk2           grup(2)
078300120412     c                   if        grup(2) > 0 and ngior > 0
078400120412     c     grup(2)       div       ngior         sk2nmz(2)
078500120516     c                   if        sk2naut(2) <> 0
078600120508     c                   eval      sk2tur(2)= (sk2naut(2) - sk2nmz(2))*100
078700120510     c                             / sk2naut(2)
078800120516     c                   endif
078900120412     c                   endif
079000120412      *gruppo 3
079100120412     c                   z-add     1             x
079200120412     c     vuoto8        lookup    skd3(x)                                90
079300120412     c     x             sub       1             ngior
079400120411     c                   xfoot     sk3           grup(3)
079500120411     c                   if        grup(3) > 0 and ngior > 0
079600120411     c     grup(3)       div       ngior         sk2nmz(3)
079700120516     c                   if        sk2naut(3) <> 0
079800120508     c                   eval      sk2tur(3)= (sk2naut(3) - sk2nmz(3))*100
079900120510     c                             / sk2naut(3)
080000120516     c                   endif
080100120411     c                   endif
080200120411     c                   endsr
080300120411     c*-------------------------------------------------
080400120411     c     calcolorie    begsr
080500120411     c*-------------------------------------------------
080600120411      ** Stampa riepilogo per tipo mezzo
080700120411     c                   if        *in21
080800120411     c                   except    riepilogot
080900120411     c                   except    riepilogo
081000120411     c     1             do        3             i                 1 0
081100120411     c                   if        sk6com(i) <> 0
081200120411      * %scostamento competenze costi
081300120411     c                   eval      scop(i)=((Sk6com(i)-Sk5cos(i))*100)/Sk6com(i)
081400120411      * scostamento competenze costi
081500120411     c                   eval      scos(i) = Sk6com(i)-Sk5cos(i)
081600120411     c                   if        sk2nmz(i) > 0
081700120411      * competenze unitario
081800120516     c                   if        auval(i) <> 0
081900120411     c                   eval      scomu(i)= sk6com(i) / auval(i)
082000120411      * costo unitario
082100120411     c                   eval      scosu(i)= sk5cos(i) / auval(i)
082200120516     c                   endif
082300120411      * %scostamento unitario competenze costi
082400120516     c                   if        scomu(i) <> 0
082500120411     c                   eval      scopu(i)=((scomu(i)-scosu(i)) *100)/scomu(i)
082600120516     c                   endif
082700120411      * scostamento unitario competenze costi
082800120411     c                   eval      scou(i) = Scomu(i)- Scosu(i)
082900120510      * spedizioni unitarie
083000120516     c                   if        auval(i) <> 0
083100120510     c                   eval      sk3spu(i)= sk3spt(i) / auval(i)
083200120510     c                   eval      sk3riu(i)= sk3rit(i) / auval(i)
083300120516     c                   endif
083400120411     c                   endif
083500120411     c                   endif
083600120411     c                   except    riepilogo1
083700120411     c                   enddo
083800120508      * calcola totali secondo prospetto
083900120508     c                   xfoot     sk2naut       sk2nautt
084000120508     c                   xfoot     sk2nmz        sk2nmzt
084100120516     c                   if        sk2nautt <> 0
084200120510     c                   eval      sk2turt =((sk2nautt - sk2nmzt)*100)/sk2nautt
084300120516     c                   endif
084400120508     c                   xfoot     sk3spt        sk3sptt
084500120510     c                   xfoot     sk3spu        sk3sput
084600120510     c                   xfoot     sk3rit        sk3ritt
084700120510     c                   xfoot     sk3riu        sk3riut
084800120508     c                   xfoot     sk4spm        sk4spmt
084900120508     c                   xfoot     sk5cos        sk5cost
085000120508     c                   xfoot     sk6com        sk6comt
085100120508     c                   eval      scost = sk6comt - sk5cost
085200120516     c                   if        sk6comt <> 0
085300120508     c                   eval      scopt =((Sk6comt-Sk5cost)*100)/Sk6comt
085400120516     c                   endif
085500120508     c                   xfoot     scosu         scosut
085600120508     c                   xfoot     scomu         scomut
085700120508     c                   eval      scout = scomut - scosut
085800130918     c                   if         Scomut <> 0
085900120508     c                   eval      scoput =((Scomut-Scosut)*100)/Scomut
086000120516     c                   endif
086100120508     c                   except    totriep
086200120411     c                   endif
086300120411     c                   endsr
086400011026     c*-------------------------------------------------
086500011026     c     *inzsr        begsr
086600011026     c*-------------------------------------------------
086700020304     c     *entry        plist
086800020304     c                   parm                    kpjba
086900030723     c                   movel     kpjbu         param
087000031114
087100041124      * se richiesta stampa giorno 'C' comprende anche le valorizzazioni
087200041124      * il flag valo è <> ' ' stampa e calcola i valori dell'ultima
087300041124      * parte del prospetto
087400041124     c     valo          comp      *blank                             2121
087500041025     c                   movel     aut1          filges            3 0
087600030723     c                   move      data1         dataiso
087700030723     c                   move      dataiso       dataeur
087800030723     c                   move      dataeur       datadal           8 0
087900030723     c                   move      data2         dataiso
088000030723     c                   move      dataiso       dataeur
088100030723     c                   move      dataeur       dataal            8 0
088200041025     c     filges        chain     azorg01l
088300041025     c                   movel     orgdes        desfil           20
088400011026
088500020218     C                   time                    w0120            14 0
088600020218     C                   move      w0120         wdat              8 0
088700041103     C                   movel     w0120         wora              6 0
088800020404     c                   z-add     99            line
088900041026     C     *LIKE         DEFINE    TBLKUT        KKUT
089000041026     C     *LIKE         DEFINE    TBLCOD        KCOD
089100041026     C     *LIKE         DEFINE    TBLKEY        KKEY
089200041026     C*
089300041026     C     kqdt          klist
089400041026     C                   kfld                    filges
089500041115     c                   kfld                    aut1
089600041026     C                   kfld                    data1
089700041026     C*
089800041027     C     kdet          klist
089900041026     C                   kfld                    qdtfgs
090000041115     C                   kfld                    exddc
090100041115     C                   kfld                    expdr
090200050207     C*
090300050207     C     kqbt          klist
090400050207     C                   kfld                    qdtfgs
090500050207     C                   kfld                    qdtddc
090600050207     C                   kfld                    qdtpdr
090700041026     C*
090800041026     C     ktab          klist
090900041026     C                   kfld                    kkut
091000041026     C                   kfld                    kcod
091100041026     C                   kfld                    kkey
091200041026     C                   z-add     1             kkut
091300041027     C*
091400041027     C     ktbe          klist
091500041027     C                   kfld                    tbecod
091600041027     C                   kfld                    tbeke1
091700041027     C*
091800041026
091900021203     c     kapdf         klist
092000030723     c                   kfld                    tipo              1
092100041115     c                   kfld                    expdr
092200041025     c                   move      'A'           tipo
092300070828     c     kapd          klist
092400070828     c                   kfld                    tipo              1
092500070828     c                   kfld                    qdtpdr
092600070828     c                   move      'A'           tipo
092700030725
092800030505     C                   Z-ADD     1             CODUT
092900030505     C                   CALL      'X§PARUT'
093000030505     C                   PARM                    UT§DSE
093100030505     C                   MOVEL     RAGUT         RSUT             20
093200030505     C                   MOVEL     REC80         CNCR80
093300050419     c                   move      *all'_'       finkao          198
093400041124     c                   move      *all'_'       finkab          134
093500041116      *date in stampa parmetri
093600041116     c                   move      data1         dataiso
093700041116     c                   move      dataiso       dataeur
093800041116     c                   move      dataeur       data1s            8 0
093900041116     c                   move      data2         dataiso
094000041116     c                   move      dataiso       dataeur
094100041116     c                   move      dataeur       data2s            8 0
094200090609      * reperimento descrizione tipo servizio 'E' x stampa
094300090609     c                   clear                   ds5e
094400090609     c                   clear                   desserv           8
094500090609     c                   move      '5E'          kcod
094600090609     c                   movel(p)  'E'           kkey
094700090609     c     ktab          chain     tabel00f                           96
094800090609     c                   if        not *in96
094900090609     c                   movel     tbluni        ds5e
095000090609     c                   eval      desserv = §5ED08
095100090609     c                   endif
095200120410      * carico tabella dei tipi mezzo per indicizzare le totalizzazioni per tipo
095300120410      * successivamente nelle 6 skiere correlate
095400120412     c                   clear                   vuoto2           20
095500120410     c                   z-add     1             idx               1 0
095600120410     c                   move      'MAS'         tbecod
095700120410     c     tbecod        setll     tntbe01l
095800120410     c                   do        *hival
095900120410     c     tbecod        reade     tntbe01l
096000120410     c                   if        %eof(tntbe01l)
096100120410     c                   leave
096200120410     c                   endif
096300120410      * non fatto con causale a responsabilità autista
096400120410     c     vuoto2        lookup    sk1tpm(idx)                            96
096500120410     c   96              movel     tbeuni        sk1tpm(idx)
096600120410     c                   enddo
096700120411      * carico tabella dei tipi mezzo per indicizzare le totalizzazioni per tipo
096800120411      * successivamente nelle 6 skiere correlate
096900120411     c                   clear                   obbaz             3 0
097000120411     c                   move      'MIC'         tbecod
097100120411     c                   movel(p)  filges        tbeke1
097200120411     c     ktbe          chain     tntbe01l
097300120411     c                   if        %found(tntbe01l)
097400120411     c                   movel     tbeuni        obbaz
097500120411     c                   endif
097600011026     c                   endsr
097700020404     C*--------------------------------------------------------------------
097800041118     Oprtf198   E            testata          02
097900041103     o                       rsut                21
098000130321     o               21                         149 'R I E P I L O G O  -
098100130321     o                                                S E R V I Z I    -
098200130321     o                                                P R E S E   E   -
098300130321     o                                              C O N S E G N E '
098400130321     o              N21                         155 'R I E P I L O G O  -
098500130321     o                                                S E R V I Z I    -
098600130321     o                                                P R E S E   E   -
098700130321     o                                              C O N S E G N E   -
098800050204     o                                              C O N   -
098900050204     o                                              D I S C O R D A N Z E'
099000050419     o                                          186 'FNLRR7R'
099100050204     O                                          193 'Pag'
099200050204     o                       page          z    198
099300041103     O          e            testata     1
099400041103     o                       knsif               11
099500041103     o                       knmus               22
099600041118     o                                           80 'Periodo'
099700041118     o                       data1s              92 '  /  /    '
099800041118     o                                           95 'al'
099900041118     o                       data2s             106 '  /  /    '
100000041118     o                                          120 'Autisti'
100100041118     o                       aut1               128
100200041118     o                                          130 '-'
100300041118     o                       aut2               138
100400050419     o                       udate         y    187
100500050419     o                       wora               198 '  :  :  '
100600041026     O          e            testata     2
100700041115     o                                           18 'Autotrasportatore'
100800120406     o                                           80 'C   O   N   S   E   G   -
100900050419     o                                              N   E'
101000120405     o                                          120 'R  I  T  I  R  I'
101100120405     o                                          145 'C  O  L  L  I'
101200120405     o*                                         150 'B A N C A L I'
101300041115     O          e            testata     1
101400120412     o*                      desserv             72
101500120412     o                                           71 'PTY.h10'
101600120412     o                                           83 'Non'
101700120412     o                                           99 'Sped'
101800120405     o                                          101 '|'
101900120405     o                                          119 '( O    R    M )'
102000120405     o                                          124 '|'
102100120405     o                                          130 'Riti'
102200120405     o                                          139 'Bollet'
102300120405     o                                          146 'Diffe'
102400120405     o                                          153 'Spun'
102500120405     o*                                         134 '|'
102600120405     o*                                         139 'Rien'
102700120405     o*                                         146 ' Da '
102800120405     o*                                         153 'Diffe'
102900050419     o                                          155 '|'
103000050419     o               21                         190 'V A L O R I Z Z A Z I O N-
103100050419     o                                               I'
103200050419     O          e            testata     1
103300120406     o                                           35 'Massa'
103400120412     o                                           40 'Km.'
103500120412     o                                           46 'Tipo'
103600120508     o                                           47 '|'
103700120412     o                                           55 'Spediz.'
103800120412     o                                           62 'Non'
103900120412     o                                           68 'Non'
104000120410     o                                           76 'Lasc.'
104100120410     o                                           84 'Spunt.'
104200120410     o                                           92 'Non'
104300120406     o                                           99 'Area'
104400120405     o                                          101 '|'
104500120405     o                                          106 'Affi'
104600120405     o                                          113 'Non'
104700120412     o                                          122 'Non Fat'
104800120405     o                                          124 '|'
104900120405     o                                          130 'rati'
105000120405     o                                          138 'tati'
105100120405     o                                          146 'renza'
105200120405     o                                          153 'tati'
105300120405     o*                                         134 '|'
105400120405     o*                                         140 'trati'
105500120405     o*                                         146 'Rend.'
105600120405     o*                                         153 'renza'
105700050419     o                                          155 '|'
105800050419     o               21                         166 'Competenze'
105900050419     o               21                         175 'Costi'
106000120508     o               21                         186 'Differ.'
106100120508     o               21                         194 'Perc.%'
106200120508     o               21                         198 'Ggl'
106300041123     O          e            testata     1
106400120412     o                                           34 'Q.li'
106500120412     o                                           46 'Tar.'
106600120508     o                                           47 '|'
106700120412     o                                           55 'Affidate'
106800120412     o                                           62 'Fatte'
106900120412     o                                           69 'Fatti'
107000120410     o                                           76 'Avv.'
107100120410     o                                           84 'Rientro'
107200120410     o                                           94 'Incass.'
107300120410     o                                          100 'Micro'
107400120405     o                                          101 '|'
107500120405     o                                          106 'dati'
107600120405     o                                          114 'Fatti'
107700120405     o                                          123 'Resp.Aut'
107800120405     o*                                         134 '|'
107900050419     o                                          155 '|'
108000041123     O          e            testata     1
108100050419     o               21      finkao             198
108200041124     o              n21      finkab             134
108300050419      *----
108400041115     O          e            detta       1
108500041117     o                       exddcs              15 '  /  /    '
108600120508     o                                           47 '|'
108700120412     o                       tbolaff       z     55
108800120412     o                       totnof        z     61
108900120412     o                       totesp        z     70
109000120411     o                       totlav        z     76
109100120507     o                       tcolnos       z     83
109200120410     o                       tincnof       4     93
109300120410     o                       tbolmig       z     99
109400120405     o                                          101 '|'
109500120504     o                       tormaf        z    106
109600120405     o                       tormnf        z    114
109700120405     o                       tormnfa       z    122
109800120405     o                                          124 '|'
109900120405     o                       tcolrit       z    130
110000120405     o                       Ftcolrit           132
110100120405     o                       tcobo         z    138
110200120405     o                       tcoldiff      m    150
110300120405     o                       Ftcoldif           147
110400120405     o                       tcospn        z    153
110500120405     o                       Ftcospn            154
110600120405     o*                                         134 '|'
110700120405     o*                      tbanrie       z    138
110800120405     o*                      Ftbanrie           140
110900120405     o*                      tbandbar      z    145
111000110301     o*                      Ftbanaff           147
111100120405     o*                      tbandiff      m    153
111200050419     o                                          155 '|'
111300050419     o               21      totcom        4    166
111400050419     o               21      totcos        4    177
111500120508     o               21      totdifv       m    186
111600120508     o               21      totdifp       m    194
111700120508     o               21      totggl        m    198
111800050419      *----
111900041115     O          e            tdetta      1
112000041115     o                       expdr                8
112100041115     o                       despdr              30
112200120412     o                       massa               35 '    ,0'
112300120412     o                       kilom               40
112400120412     o                       tariffa             46
112500120508     o                                           47 '|'
112600120412     o                       ttbolaff      z     55
112700120412     o                       ttotnof       z     61
112800120412     o                       ttotesp       z     70
112900120411     o                       ttotlav       z     76
113000120507     o                       ttcolnos      z     83
113100120411     o                       ttincnof      4     93
113200120406     o                       ttbolmig      z     99
113300120405     o                                          101 '|'
113400120405     o                       ttormaf       z    106
113500120405     o                       ttormnf       z    114
113600120405     o                       ttormnfa      z    122
113700120405     o                                          124 '|'
113800120405     o                       ttcolrit      z    130
113900120405     o                       FFtcolrit          132
114000120405     o                       ttcobo        z    138
114100120405     o                       ttcoldiff     m    146
114200120405     o                       FFtcoldif          147
114300120405     o                       ttcospn       z    153
114400120405     o                       FFtcospn           154
114500120405     o*                                         134 '|'
114600120405     o*                      ttbanrie      z    138
114700120405     o*                      FFtbanrie          140
114800120405     o*                      ttbandbar     z    145
114900110301     o*                      FFtbanaff          147
115000120405     o*                      ttbandiff     m    153
115100050419     o                                          155 '|'
115200050419     o               21      ttotcom       4    166
115300050419     o               21      ttotcos       4    177
115400120508     o               21      ttotdifv      m    186
115500120508     o               21      ttotdifp      m    194
115600120508     o               21      ttotggl       m    198
115700050419      *---
115800041116     O          e            finka       0
115900041116     o                       expdr                8
116000041116     o                       despdr              30
116100120412     o                       massa               35 '    ,0'
116200120412     o                       kilom               40
116300120412     o                       tariffa             46
116400120508     o                                           47 '|'
116500120412     o                       ttbolaff      z     55
116600120412     o                       ttotnof       z     61
116700120412     o                       ttotesp       z     70
116800120411     o                       ttotlav       z     76
116900120507     o                       ttcolnos      z     83
117000120410     o                       ttincnof      4     93
117100120406     o                       ttbolmig      z     99
117200120405     o                       ttormaf       z    106
117300120405     o                       ttormnf       z    114
117400120405     o                       ttormnfa      z    122
117500120405     o                       ttcolrit      z    130
117600120405     o                       FFtcolrit          132
117700120405     o                       ttcobo        z    138
117800120405     o                       ttcoldiff     m    146
117900120405     o                       FFtcoldif          147
118000120405     o                       ttcospn       z    153
118100120405     o                       FFtcospn           154
118200120405     o*                      ttbanrie      z    138
118300120405     o*                      FFtbanrie          140
118400120405     o*                      ttbandbar     z    145
118500110301     o*                      FFtbanaff          147
118600120405     o*                      ttbandiff     m    153
118700050419     o               21      ttotcom       4    166
118800050419     o               21      ttotcos       4    177
118900120508     o               21      ttotdifv      m    186
119000120508     o               21      ttotdifp      m    194
119100120508     o               21      ttotggl       m    198
119200050419      *-----
119300041118     O          e            finka       0
119400041116     o                       expdr                8
119500041116     o                       despdr              30
119600120412     o                       massa               35 '    ,0'
119700120412     o                       kilom               40
119800120412     o                       tariffa             46
119900120508     o                                           47 '|'
120000120412     o                       ttbolaff      z     55
120100120412     o                       ttotnof       z     61
120200120412     o                       ttotesp       z     70
120300120411     o                       ttotlav       z     76
120400120507     o                       ttcolnos      z     83
120500120410     o                       ttincnof      4     93
120600120406     o                       ttbolmig      z     99
120700120405     o                       ttormaf       z    106
120800120405     o                       ttormnf       z    114
120900120405     o                       ttormnfa      z    122
121000120405     o                       ttcolrit      z    130
121100120405     o                       FFtcolrit          132
121200120405     o                       ttcobo        z    138
121300120405     o                       ttcoldiff     m    146
121400120405     o                       FFtcoldif          147
121500120405     o                       ttcospn       z    153
121600120405     o                       FFtcospn           154
121700120405     o*                      ttbanrie      z    138
121800120405     o*                      FFtbanrie          140
121900120405     o*                      ttbandbar     z    145
122000110301     o*                      FFtbanaff          147
122100120405     o*                      ttbandiff     m    153
122200050419     o               21      ttotcom       4    166
122300050419     o               21      ttotcos       4    177
122400120508     o               21      ttotdifv      m    186
122500120508     o               21      ttotdifp      m    194
122600120508     o               21      ttotggl       m    198
122700050419      *
122800041118     O          e            finka       1
122900120406     o*                                          79 '|'
123000120405     o                                          124 '|'
123100120405     o*                                         134 '|'
123200050419     o                                          155 '|'
123300050419      *
123400050208     O          e            tdettAN     1
123500050207     o                       expdr                8
123600050207     o                       despdr              30
123700050420     o                                           74 '  D A T I   N O N   D I S -
123800050419     O                                              P O N I B I L I'
123900120406     o*                                          79 '|'
124000120405     o                                          124 '|'
124100120405     o*                                         134 '|'
124200050419     o                                          155 '|'
124300050208     O          e            dettAN      1
124400050208     o                       exddcs              15 '  /  /    '
124500050419     o                                           74 '  D A T I   N O N   D I S -
124600050419     O                                              P O N I B I L I'
124700120406     o*                                          79 '|'
124800120405     o                                          124 '|'
124900120405     o*                                         134 '|'
125000050419     o                                          155 '|'
125100050419      *****
125200071030     O          e            sommadet    1
125300071030     o                                           13 'T O T A L E'
125400120508     o                                           47 '|'
125500120412     o                       xtbolaff      z     55
125600120412     o                       xtotnof       z     61
125700120412     o                       xtotesp       z     70
125800120411     o                       xtotlav       z     76
125900120507     o                       xtcolnos      z     83
126000120410     o                       xtincnof      4     93
126100120410     o                       xtbolmig      z     99
126200120405     o                                          101 '|'
126300120405     o                       xtormaf       z    106
126400120405     o                       xtormnf       z    114
126500120405     o                       xtormnfa      z    122
126600120405     o                                          124 '|'
126700120405     o                       xtcolrit      z    130
126800120405     o                       xtcobo        z    138
126900120405     o                       xtcoldiff     m    146
127000120405     o                       xtcospn       z    153
127100120405     o*                                         134 '|'
127200120405     o*                      xtbanrie      z    138
127300120405     o*                      xtbandbar     z    145
127400120405     o*                      xtbandiff     m    153
127500071030     o                                          155 '|'
127600071030     o               21      xtotcom       4    166
127700071030     o               21      xtotcos       4    177
127800120508     o               21      xtotdifv      m    186
127900120508     o               21      xtotdifp      m    194
128000120510     o*              21      xtotggl       m    198
128100071030      *---
128200050419     O          e            Legend      2
128300050419     o               21      finkao             198
128400050419     o              n21      finkab             134
128500050419     O          e            Legend      1
128600050419     o                                            1 ' '
128700050419     o                                          + 0 '* = La differenza fra coll'
128800050420     o                                          + 0 'i ritirati e bollettati è '
128900050420     o                                          + 0 'confermata anche dai colli'
129000050421     o                                          + 0 ' spuntati. '
129100050421     o                                          + 2 '§ = Il nr.dei colli spunta'
129200050419     o                                          + 0 'ti è inesatto per la prese'
129300050419     o                                          + 0 'nza di "Bancali oltre 20 c'
129400050420     o                                          + 0 'olli" spuntati '
129500050420     o                                          + 0 'parzialmente'
129600120104     O*         e            Legend      1
129700120104     o*                                           1 ' '
129800120104     o*                                         + 0 'Gli Importi sono '
129900120104     o*                                         + 0 'comprensivi del costo '
130000120104     o*                                         + 0 'stimato Adeguamento '
130100120104     o*                                         + 0 'Carburante'
130200120411     o*--------------------------------------------------------------------------
130300120412     O          e    21      riepilogot       02
130400120412     o                       rsut                21
130500130321     o                                          149 'R I E P I L O G O  -
130600130321     o                                                S E R V I Z I    -
130700130321     o                                                P R E S E   E   -
130800130321     o                                              C O N S E G N E '
130900120412     o                                          186 'FNLRR7R'
131000120412     O                                          193 'Pag'
131100120412     o                       page          z    198
131200120509     O          e    21      riepilogot  2
131300120412     o                       knsif               11
131400120412     o                       knmus               22
131500120412     o                                           80 'Periodo'
131600120412     o                       data1s              92 '  /  /    '
131700120412     o                                           95 'al'
131800120412     o                       data2s             106 '  /  /    '
131900120412     o                                          120 'Autisti'
132000120412     o                       aut1               128
132100120412     o                                          130 '-'
132200120412     o                       aut2               138
132300120412     o                       udate         y    187
132400120412     o                       wora               198 '  :  :  '
132500120509     O          e    21      riepilogot  2
132600120509     o                                           50 'AUTISTI   UTILIZZATI'
132700120510     o                                           78 'SPEDIZIONI'
132800120510     o                                          102 'R I T I R I'
132900120510     o                                          140 'T  O  T  A  L  E'
133000120510     o                                          190 'U  N  I  T  A  R  I  O'
133100120509     o                                           56 '|'
133200120510     o                                           88 '|'
133300120510     o                                          107 '|'
133400120510     o                                          158 '|'
133500120510     o                                          197 '|'
133600120508     O          e    21      riepilogo   1
133700120508     o                                           49 'Media          '
133800120509     o                                           53 '%'
133900120510     o                                          154 '%'
134000120510     o*mm                                       187 '%'
134100120509     o                                           56 '|'
134200120511     o                                           85 'Tot'
134300120510     o                                           88 '|'
134400120510     o                                          107 '|'
134500120510     o                                          158 '|'
134600120510     o                                          197 '|'
134700120412     O          e    21      riepilogo   1
134800120412     o                                           10 'Tipo Mezzo'
134900120509     o                                           32 'Totali'
135000120509     o                                           49 '  gg    /  Obv '
135100120508     o                                           55 'Turn.'
135200120509     o                                           63 'Totali'
135300120511     o                                           75 'Unitarie'
135400120510     o                                           86 'Micro'
135500120510     o                                           95 'Totali'
135600120510     o                                          106 'Unitari'
135700120510     o                                          120 'Costi'
135800120510     o                                          135 'Competenze'
135900120510     o                                          148 'Scostamento'
136000120510     o                                          156 'Scost.'
136100120510     o                                          170 'Costo'
136200120510     o                                          183 'Competenze'
136300120510     o                                          195 'Scostamento'
136400120510     o*mm                                       190 'Scost.'
136500120509     o                                           56 '|'
136600120510     o                                           88 '|'
136700120510     o                                          107 '|'
136800120510     o                                          158 '|'
136900120510     o                                          197 '|'
137000120412     O          e    21      riepilogo   1
137100120411     o                       finkao             198
137200120412     O          e    21      riepilogo1  1
137300120508     o                       sk1tpm(i)           20
137400120508     o                       sk2naut(i)    z     30
137500120508     o                       sk2nmz(i)     z     38
137600120508     o                       obbaz         zb    48
137700120509     o                       sk2tur(i)     z     54
137800120509     o                                           56 '|'
137900120508     o                       sk3spt(i)     z     62
138000120510     o                       sk3spu(i)     z     75
138100120510     o                       sk4spm(i)     z     86
138200120510     o                                           88 '|'
138300120510     o                       sk3rit(i)     z     95
138400120510     o                       sk3riu(i)     z    105
138500120510     o                                          107 '|'
138600120510     o                       sk5cos(i)     2    122
138700120510     o                       sk6com(i)     2    135
138800120510     o                       scos(i)       m    148
138900120510     o                       scop(i)       m    156
139000120510     o                                          158 '|'
139100120510     o                       scosu(i)      2    170
139200120510     o                       scomu(i)      2    183
139300120510     o                       scou(i)       m    195
139400120510     o*mm                    scopu(i)      m    190
139500120510     o                                          197 '|'
139600120412     O          e    21      riepilogo1  1
139700120509     o                                           56 '|'
139800120510     o                                           88 '|'
139900120510     o                                          107 '|'
140000120510     o                                          158 '|'
140100120510     o                                          197 '|'
140200120508     O          e    21      totriep     1
140300120508     o                       finkao             198
140400120508     O          e    21      totriep     1
140500120508     o                       sk2nautt      z     30
140600120508     o                       sk2nmzt       z     38
140700120509     o                       sk2turt       z     54
140800120508     o                       sk3sptt       z     62
140900120510     o*mm                    sk3sput       z     75
141000120510     o                       sk4spmt       z     86
141100120510     o                                           88 '|'
141200120510     o                       sk3ritt       z     95
141300120510     o*mm                    sk3riut       z    105
141400120510     o                                          107 '|'
141500120510     o                       sk5cost       2    122
141600120510     o                       sk6comt       2    135
141700120510     o                       scost         m    148
141800120510     o                       scopt         m    156
141900120510     o                                          158 '|'
142000120510     o*mm                    scosut        2    170
142100120510     o*mm                    scomut        2    183
142200120510     o*mm                    scout         m    195
142300120510     o*mm                    scoput        m    190
142400120510     o                                          197 '|'
142500120509     O          e    21      Legend2     4
142600120412     o                                            2 ' *'
142700120510     o                                          + 0 'Autisti Totali : Aut che '
142800120510     o                                          + 0 'hanno lavorato almeno un'
142900120510     o                                          + 0 ' giorno nel periodo '
143000120510     O          e    21      Legend2     2
143100120510     o                                            2 '  '
143200120510     o                                          + 0 'Autisti Media gg: Aut che'
143300120510     o                                          + 0 ' mediamente hanno lavorato'
143400120510     o                                          + 0 ' in 1 giorno '
143500120510     O          e    21      Legend2     2
143600120510     o                                            2 '  '
143700120510     o                                          + 0 'Autisti % Turn : incidenza'
143800120510     o                                          + 0 ' Autisti media gg su Autis'
143900120510     o                                          + 0 'ti totali '
144000120510     O          e    21      Legend2     3
144100120412     o                                            3 ' **'
144200120412     o                                          + 0 'I valori unitari sono calc'
144300120412     o                                          + 0 'olati sommando,per tipo me'
144400120412     o                                          + 0 'zzo, i valori di'
144500120412     o                                          + 0 ' tutti gli autisti'
144600120412     o                                          + 0 ' diviso il nume'
144700120412     o                                          + 0 'ro dei giorni valorizzati'
