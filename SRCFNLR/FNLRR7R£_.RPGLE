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
003201120406     D partar          DS
003202120406     D  parauti                       7s 0
003203120406     D  pardati                       8s 0
003204120406     D  partsri                       1
003205120406     d  partaro                       3
003206120406     d  esito                         1
003207120406     d  error                         1
003300020404     d prtds           ds
003400020404     d   spab                         3  0
003500020404     d   spaa                         3  0
003600020404     d   skab                         3  0
003700020404     d   skaa                         3  0
003800020404     d   line                         3  0
003900041026     d exqdtpdr        s                   like(qdtpdr)
003901120508     d autzero         s              7  0
004000030724     d primo           s              1
004100041028     d dataiso         s               d   datfmt(*iso)
004200041028     d dataeur         s               d   datfmt(*eur)
004300041028      *
004400071030     d xtbolaff        s              6  0
004401120406     d xtbolmig        s              6  0
004500071030     d xtotnof         s                   like(ttotnof)
004600071030     d xtotesp         s                   like(ttotesp)
004700071030     d xtotlav         s                   like(ttotlav)
004800071030     d xtcolnos        s                   like(ttcolnos)
004900071030     d xtincnof        s                   like(ttincnof)
005000071030     d xtormaf         s                   like(ttormaf)
005100071030     d xtormnf         s                   like(ttormnf)
005200071030     d xtormnfa        s                   like(ttormnfa)
005300071030     d xtcolrit        s              6  0
005400071030     d xtcobo          s              6  0
005500071030     d xtcoldiff       s              6  0
005600071030     d xtcospn         s              6  0
005700071030     d xtbanrie        s                   like(ttbanrie)
005800110304     d xtbandbar       s                   like(ttbandbar)
005900071030     d xtbandiff       s                   like(ttbandiff)
006000071030     d xtotcom         s                   like(ttotcom)
006001120508     d xtotggl         s                   like(ttotggl)
006100071030     d xtotcos         s                   like(ttotcos)
006200071030     d xtotdifv        s                   like(ttotdifv)
006300071030     d xtotdifp        s                   like(ttotdifp)
006400071030     d wxtotdifp       s                   like(wttotdifp)
006401120410      *skiere per totalizzazione report DLO per tipo mezzo
006402120508     d skaut           s              7  0 dim(1000)
006403120412     d sk1tpm          s             20    dim(3)
006404120531     d sk2naut         s              4  0 dim(3)
006405120531     d sk2nmz          s              4  0 dim(3)
006406120531     d sk2tur          s              4  0 dim(3)
006407120410     d sk3spt          s              5  0 dim(3)
006408120510     d sk3spu          s              5  0 dim(3)
006409120510     d sk3rit          s              5  0 dim(3)
006410120510     d sk3riu          s              5  0 dim(3)
006411120410     d sk4spm          s              5  0 dim(3)
006412120410     d sk5cos          s              9  2 dim(3)
006413120410     d sk6com          s              9  2 dim(3)
006414120411     d scos            s              9  2 dim(3)
006415120411     d scop            s              5  2 dim(3)
006416120411     d scosu           s              9  2 dim(3)
006417120411     d scomu           s              9  2 dim(3)
006418120411     d scou            s              9  2 dim(3)
006419120411     d scopu           s              5  2 dim(3)
006420120411     d auval           s              5  0 dim(3)
006421120412     D skd1            s              8  0 DIM(60)
006422120531     D sk1             s              4  0 DIM(60)
006423120412     D skd2            s              8  0 DIM(60)
006424120531     d sk2             s              4  0 DIM(60)
006425120412     D skd3            s              8  0 DIM(60)
006426120531     d sk3             s              4  0 DIM(60)
006427120531     d grup            s              4  0 DIM(3)
006428120531     d sk2nautt        s              4  0
006429120531     d sk2nmzt         s              4  0
006430120531     d sk2turt         s              4  0
006431120508     d sk3sptt         s              5  0
006432120510     d sk3sput         s              5  0
006433120510     d sk3ritt         s              5  0
006434120510     d sk3riut         s              5  0
006435120508     d sk4spmt         s              5  0
006436120508     d sk5cost         s              9  2
006437120508     d sk6comt         s              9  2
006438120508     d scost           s              9  2
006439120508     d scopt           s              5  2
006440120508     d scosut          s              9  2
006441120508     d scomut          s              9  2
006442120508     d scout           s              9  2
006443120508     d scoput          s              5  2
006500000000     C*---------------------------------------------------------------*
006600020304      *
006700041025     c                   exsr      fiqdt
006800050419      *
006900041123      *ultima rottura per fine file ripete sequenza di stampa
007000041123     c                   exsr      dettagli
007100050419      *
007200041123      * una riga in stampa per ogni data autista nel periodo richiesto
007300041123     c                   if        primo <> *blank
007400041123     c                   if        line > 60
007500041123     c                   except    testata
007600041123     c                   endif
007700050420      *
007800041124      *in caso di stampa anomalie valo=' ' verifico se i valori sensibili
007900041124      * sono diversi da zero se almeno uno lo è stampo la riga
008000041124     c                   if        valo = *blank and ttcoldiff <> 0 or
008100041124     c                             valo = *blank and ttbandiff <> 0 or
008200041124     c                             valo = *blank and ttcolnos  <> 0 or
008300041124     c                             valo = *blank and ttincnof  <> 0 or
008400050208     c                             valo <> *blank or anomalo <> *blank
008500050208     c                   if        anomalo <> *blank
008600050208     c                   except    tdettAN
008700050208     c                   else
008800050208     c                   except    tdetta
008801120412      * incrementa prospetto di riepilogo se auto non fittizio
008802120510     c                   if        idx > 0 and xapdpdd = *blank
008803120410     c                   add       ttbolaff      sk3spt(idx)
008804120510     c                   add       ttormaf       sk3rit(idx)
008805120410     c                   add       ttbolmig      sk4spm(idx)
008806120410     c                   add       ttotcos       sk5cos(idx)
008807120410     c                   add       ttotcom       sk6com(idx)
008808120410     c                   endif
008900071030     c                   exsr      sommadettagli
009000041124     c                   if        dett = 'S'
009100041124     c                   except    finka
009200041124     c                   endif
009300050208     c                   end
009400041124     c                   endif
009500041123     c                   endif
009600041123
009700071030      ** calcola percentuale totale competenze costi
009800071030      *
009900071030     c                   if        *in21 and xtotcom <> 0
010000071030     c                   eval      Wxtotdifp =((xtotcom -xtotcos) *100) /xtotcom
010100071030     c                   if        wxtotdifp > 999,99
010200071030     c                   eval      xtotdifp = 999,99
010300071030     c                   else
010400071030     c                   if        wxtotdifp < -999,99
010500071030     c                   eval      xtotdifp = -999,99
010600071030     c                   else
010700071030     c                   eval      xtotdifp = Wxtotdifp
010800071030     c                   end
010900071030     c                   end
011000071030     c                   else
011100071030     c                   eval      xtotdifp = 0
011200071030     c                   endif
011300071031      ** Stampa il totale generale solo se la richiesta proviene da profilo di sede
011400100118      ********** si vuole sempre la stampa dei totali.....quindi
011500100118     c**********         if        sede <> *blank
011600071030     c                   except    sommadet
011700100118     c**********         endif
011800050419      ** Stampa la legenda
011900050419     c                   except    legend
011901120412      * prepara campi per calcolo medie ponderate se autista non fittizio
011903120411     c                   exsr      prepararie
011904120411      * esegue calcoli per prospetto di riepilogo finale per tipo mezzo
011905120411     c                   exsr      calcolorie
011906120412     c                   except    legend2
011908120412
012000020308     c                   seton                                        lr
012100990113     c*-------------------------------------------------
012200041025     c     fiqdt         begsr
012300990113     c*-------------------------------------------------
012400990113     c                   setoff                                           99
012500041112     c     kqdt          setll     fiqdt05l
012600981029     c                   do        *hival
012700041112     c     filges        reade     fiqdt05l                               99
012800981029     c   99              leave
012900070828      * verifica se scelta distinzione autisti parcel messaggerie
013000070828     c     kapd          chain     fiapd01l                           97
013100070828      * recepisce dall'anagrafica la tipologia autista parcel messaggerie
013200070828     c   97              clear                   dapdflr
013300070828     c  n97              movel     apdflr        dapdflr
013400070828     c                   select
013500070828     c                   when      parmes = 'M' and §apdtpm <> 'M'
013600070828     c                   iter
013700070828     c                   when      parmes = 'P' and §apdtpm = 'M'
013800070828     c                   iter
013900070828     c                   endsl
014000050420      *
014100050207     c     kqbt          chain     fiqbt01l                           95
014200030724      * elimino i record che non rientrano nei parametri richiesti
014300041025     c                   if        qdtddc >= data1 and qdtddc <= data2 and
014400041025     c                             qdtpdr >= aut1  and qdtpdr <= aut2
014500041027
014600041027      *primo giro forzo codice padroncino per testare rottura calcoli totale
014700041027     c                   if        primo  = *blank
014800050624     c                   if        qdtdaeco = 0 or qdtdaeor = 0 or
014900050624     c                             not *in95 and  qbtdaebo = 0
015000050609     c                   move      'X'           anomalo
015100050609     c                   endif
015200041027     c                   z-add     qdtpdr        expdr             7 0
015300041027     c                   z-add     qdtddc        exddc             8 0
015400041027     c                   move      'X'           primo             1
015401120410      * dettagli autista
016100120410     c                   exsr      datiautista
016101120410     c                   endif
016200041027      * prepara dati per colli non bollettati e bancali a rottura autista
016300041112     c                   if        expdr <> qdtpdr
016400041123     c
016500041118     c                   exsr      dettagli
016600041116
016700041112     c                   if        line > 60
016800041112     c                   except    testata
016900041112     c                   endif
017000041112
017100041112      *totali dettaglio per periodo padroncino
017200041124      *in caso di stampa anomalie valo=' ' verifico se i valori sensibili
017300041124      * sono diversi da zero se almeno uno lo è stampo la riga
017400041124     c                   if        valo = *blank and ttcoldiff <> 0 or
017500041124     c                             valo = *blank and ttbandiff <> 0 or
017600041124     c                             valo = *blank and ttcolnos  <> 0 or
017700041124     c                             valo = *blank and ttincnof  <> 0 or
017800050208     c                             valo <> *blank or anomalo <> *blank
017900050208     c                   if        anomalo <> *blank
018000050208     c                   except    TdettAN
018100050208     c                   else
018200050208     c                   except    Tdetta
018201120510     c                   if        idx > 0 and xapdpdd = *blank
018202120410     c                   add       ttbolaff      sk3spt(idx)
018203120510     c                   add       ttormaf       sk3rit(idx)
018204120410     c                   add       ttbolmig      sk4spm(idx)
018205120410     c                   add       ttotcos       sk5cos(idx)
018206120410     c                   add       ttotcom       sk6com(idx)
018207120410     c                   endif
018300071030     c                   exsr      sommadettagli
018400041116     c                   if        dett = 'S'
018500041116     c                   except    finka
018600041116     c                   endif
018700050208     c                   end
018800041124     c                   endif
018900050208     c                   clear                   anomalo           1
019000050208      * se rekord non ancora trattato dall'elaborazione skedulata di
019100050208      * valorizzazione consegne orm o bollettazione segnalo l'anomalia
019200050624     c                   if        qdtdaeco = 0 or qdtdaeor = 0 or
019300050624     c                             not *in95 and  qbtdaebo = 0
019400050208     c                   move      'X'           anomalo
019500050208     c                   endif
019600050208
019700041027     c* azzero i dati per il totale  autista/data relativi ai BANCALI
019800041115     c                   clear                   ttbolaff          5 0
019801120406     c                   clear                   ttbolmig          5 0
019900041112     c                   clear                   ttotnof           5 0
020000041112     c                   clear                   ttotesp           5 0
020100041112     c                   clear                   ttotlav           5 0
020200041115     c                   clear                   ttcolnos          5 0
020300041115     c                   clear                   ttincnof          9 2
020400041115     c                   clear                   ttormaf           5 0
020500041112     c                   clear                   ttormnf           5 0
020600041112     c                   clear                   ttormnfa          5 0
020700050419     c                   clear                   ttbandiff         3 0
020800050419     c                   clear                   ttbanrie          3 0
020900050420     c                   clear                   FFtbanrie         1
021000110304     c                   clear                   ttbandbar         3 0
021100050420     c                   clear                   FFtbanaff         1
021200041115     c                   clear                   ttcoldiff         5 0
021300050420     c                   clear                   FFtcoldif         1
021400050419     c                   clear                   ttcobo            5 0
021500050420     c                   clear                   ttcospn           5 0
021600050420     c                   clear                   FFtcospn          1
021700050419     c                   clear                   ttcolrit          5 0
021800050420     c                   clear                   FFtcolrit         1
021900041027
022000041118     c                   clear                   ttotcos           9 2
022100041118     c                   clear                   ttotcom           9 2
022101120508     c                   clear                   ttotggl           3 0
022200120508     c                   clear                   ttotdifv          5 0
022300041123     c                   clear                   ttotdifp          5 2
022400050419     c                   clear                   wttotdifp         9 2
022500041027      * imposta dati nuovo autista o data dopo totali
022600041027     c                   z-add     qdtpdr        expdr
022700041027     c                   z-add     qdtddc        exddc
022701120410      * dettagli autista
022702120410     c                   exsr      datiautista
022703120411     c                   endif
023500041115
023600041115      * stampa  dettagli x data/autista se richiesto
023700041118      * a rottura di data o autista
023800041118     c*                  if        exddc <> qdtddc or expdr <> qdtpdr
023900041118     c                   if        exddc <> qdtddc
024000041115
024100050208      * se rekord non ancora trattato dall'elaborazione skedulata di
024200050208      * valorizzazione consegne orm o bollettazione segnalo l'anomalia
024300050624     c                   if        qdtdaeco = 0 or qdtdaeor = 0 or
024400050624     c                             not *in95 and  qbtdaebo = 0
024500050208     c                   move      'X'           anomalo
024600050208     c                   endif
024700050208
024800041118     c                   exsr      dettagli
024900041118      *
025000041116     c                   z-add     qdtpdr        expdr
025100041116     c                   z-add     qdtddc        exddc
025200041115
025300041115     c                   endif
025400041115      *  totalizzo se stesso autista e stessa data i dati testata
025500041115     c* se esistono più distinte esistono più rekord
025600050208     c                   if        anomalo = *blank
025700110301     c*                  add       qdtnbnaf      banaff            5 0
025800110310     c                   movel     qdtflo        dqdtflo
025900110304     c                   clear                   banbdar           5 0
026000110301     c                   if        §QDTNBDAR <> *blank
026100110304     c                   move      §qdtnbdar     banbdar           5 0
026200110301     c                   endif
026300050420     c                   if        Fbanaff = *blank or qdtFNBAF ='N'
026400050420     c                   move      qdtFNBAF      Fbanaff
026500050420     c                   end
026600041027     c                   add       qdtnbnri      banrie            5 0
026700050210     c                   add       qdtnbnbr      banrie
026800050420     c                   if        Fbanrie = *blank or qdtFNBRI ='N'
026900050420     c                   move      qdtFNBRI      Fbanrie
027000050420     c                   end
027100041027     c                   add       qdtnclrit     colrit            5 0
027200050420     c                   if        Fcolrit = *blank or qdtFNCRIT ='N'
027300050420     c                   move      qdtFNCRIT     Fcolrit
027400050420     c                   end
027500041115     c                   add       qdtnblaf      tbolaff
027501120406     c                   add       qdtnblmig     tbolmig
027600041115     c                   add       qdtnclns      tcolnos
027700041115     c                   add       qdtormaf      tormaf
027800041115     c                   add       qdtormnf      tormnf
027900041115      *incassi
028000041115     c                   add       qdtcasnf      tincnof
028100041115     c                   add       qdtiftnf      tincnof
028200050208     c                   endif
028300020404
028400041027      *data in stampa
028500041025     c                   move      qdtddc        dataiso
028600041025     c                   move      dataiso       dataeur
028700041025     c                   move      dataeur       qdtddcs           8 0
028800041025
028900041115      *data e autista all'interno del range richiesto a video
029000120410     c                   end
029100020404
029200020304     c                   enddo
029300041027      * ultimo totale x fine file
029400020304
029500020304     c                   endsr
029600981029      *
029700041026     c*-------------------------------------------------
029800041026     c     Non_cons      begsr
029900041026     c*-------------------------------------------------
030000041115     c     kdet          setll     fiqdc02l
030100041026     c                   do        *hival
030200041115     c     kdet          reade     fiqdc02l                               98
030300041026     c   98              leave
030400041026      * considero solo le testatine o meglio le righe che contengono
030500041026      * il codice evento riferito alla bolla gli altri rekord sono
030600041026      * il dettaglio dei colli non spuntati
030700041028      *
030800041026     c                   if        qdccev = *blank
030900041026     c                   iter
031000041026     c                   endif
031100041026      * verifico sulla tabella eventi la tipologia per totalizzare
031200041026      * le diverse motivazioni di non consegna
031300041026     c                   clear                   ds2a
031400041026     c                   move      '2A'          kcod
031500041026     c                   movel(p)  qdccev        kkey
031600041026     c     ktab          chain     tabel00f                           96
031700041026     c                   if        not *in96
031800041026     c                   movel     tbluni        ds2a
031900041026     c                   select
032000041026      * lasciati avvisi
032100041026     c                   when      §2aftc = 'A'
032200041112     c                   add       1             totlav            5 0
032300041026
032400041026      * Non fatte
032500041026     c                   when      §2aftc = 'N'
032600041112     c                   add       1             totnof            5 0
032700091008      * espressi o  h10:30
032800091008     c                   if        qdctsp = 'E' or qdctsp = 'H'
032900041112     c                   add       1             totesp            5 0
033000041112     c                   endif
033100041026
033200041026      * Giacenze
033300041026     c                   when      §2aftc = 'G'
033400041115     c                   add       1             totgia            5 0
033500041026
033600041026     c                   endsl
033700041026     c                   endif
033800041026     c                   enddo
033900041026     c                   endsr
034000041027     c*-------------------------------------------------
034100041027     c     ORM_noes      begsr
034200041027     c*-------------------------------------------------
034300041115     c     kdet          setll     fiqdo02l
034400041027     c                   do        *hival
034500041115     c     kdet          reade     fiqdo02l                               98
034600041027     c   98              leave
034700041027     c                   if        qdocar = *blank
034800041027     c                   iter
034900041027     c                   endif
035000041027      * reperisco dalla tabella la decodifica della causale x non eseguito
035100041027     c                   clear                   dcmr
035200041027     c                   move      'CMR'         tbecod
035300041027     c                   movel(p)  qdocar        tbeke1
035400041027     c     ktbe          chain     tntbe01l                           96
035500041027     c                   if        not *in96
035600041027     c                   movel     tbeuni        dcmr
035700041112      * non fatto con causale a responsabilità autista
035800041112     c                   if        d§cmrrea = 'S'
035900041112     c                   add       1             tormnfa           5 0
036000041112     c                   endif
036100041027     c                   endif
036200041027
036300041027     c                   enddo
036400041027     c                   endsr
036500041115     c*-------------------------------------------------
036600041115     c     Col_nobo      begsr
036700041115     c*-------------------------------------------------
036800041115      * totalizza i colli bollettati per spedizione da confrontare con
036900041115      * i colli ritirati dichiarati in distinta
037000041115      *
037100041115     c                   clear                   totcobo           5 0
037200050419     c                   clear                   tcobo             5 0
037300050420     c                   clear                   totcospn          5 0
037400050420     c                   clear                   fcospn            1
037500050420     c                   clear                   tcospn            5 0
037600050420     c                   clear                   ftcospn           1
037700041115     c     kdet          setll     fiqbt01l
037800050420      *
037900041115     c                   do        *hival
038000041115     c     kdet          reade     fiqbt01l                               98
038100041115     c   98              leave
038200041115     c                   add       qbtnclbo      totcobo
038300050420     c                   add       qbtnclsp      totcospn
038400050420      * se squadrati memorizza il flag
038500050420     c                   if        fcospn = *blank
038600050420     c                   move      qbtfquad      fcospn
038700050420     c                   end
038800041115     c                   enddo
038900050420
039000041115     c                   endsr
039100041118     c*-------------------------------------------------
039200041118     c     val_aut       begsr
039300041118     c*-------------------------------------------------
039400041118      * totalizza i colli bollettati per spedizione da confrontare con
039500041118      * i colli ritirati dichiarati in distinta
039600041118      *
039700041118     c     kdet          chain     fifss01l                           98
039800041118     c                   if        not *in98
039900041118     c                   add       fss001        totcos
040000041118     c                   add       fss005        totcos
040100041118     c                   add       fss016        totcos
040200041118     c                   add       fss019        totcos
040300041118     c                   add       fss034        totcos
040400041118     c                   add       fss035        totcos
040500041118     c                   add       fss997        totcos
040600041118     c                   add       fss998        totcos
040700041118     c                   add       fssxx1        totcos
040800041118     c                   add       fssxx2        totcos
040900041118     c
041000041118     c                   add       fss001b       totcom
041100041118     c                   add       fss005b       totcom
041200041118     c                   add       fss016b       totcom
041300041118     c                   add       fss019b       totcom
041400041118     c                   add       fss034b       totcom
041500041118     c                   add       fss035b       totcom
041600041118     c                   add       fss997b       totcom
041700041118     c                   add       fss998b       totcom
041800041118     c                   add       fssxx1b       totcom
041900041118     c                   add       fssxx2b       totcom
042000120508     c                   add       1             totggl
042100041118     c     totcom        sub       totcos        totdifv
042200050208     c                   if        totcom <> 0
042300050419     c                   eval      wtotdifp = ((totcom -totcos) * 100) /totcom
042400050419     c                   if        wtotdifp > 999,99
042500050419     c                   eval      totdifp = 999,99
042600050419     c                   else
042700050419     c                   if        wtotdifp < -999,99
042800050419     c                   eval      totdifp = -999,99
042900050419     c                   else
043000050419     c                   eval      totdifp = wtotdifp
043100050419     c                   end
043200050419     c                   end
043300050208     c                   else
043400050208     c                   eval      totdifp = 0
043500050208     c                   endif
043501120412      * conteggia numero valorizzazioni(autisti) per gruppo nel periodo
043502120412      * solo se autista non fittizio
043503120412     c                   if        APDPDD = *blank
043504120411     c                   add       1             auval(idx)
043505120411     c                   z-add     1             x                 2 0
043506120411     c                   clear                   vuoto8            8 0
043510120411     c                   select
043511120412      * gruppo 1 micro
043512120412      * memorizzo date relative al gruppo mezzo
043513120411     c                   when      idx = 1
043514120412     c     exddc         lookup    skd1(x)                                90
043515120412     c                   if        not *in90
043516120412     c     vuoto8        lookup    skd1(x)                                90
043517120412     c                   z-add     exddc         skd1(x)
043518120412     c                   endif
043519120412      *aggiungo l'autista
043520120411     c                   add       1             sk1(x)
043521120412      * gruppo 2
043522120411     c                   when      idx = 2
043523120412     c     exddc         lookup    skd2(x)                                90
043524120412     c                   if        not *in90
043525120412     c     vuoto8        lookup    skd2(x)                                90
043526120412     c                   z-add     exddc         skd2(x)
043527120412     c                   endif
043528120411     c                   add       1             sk2(x)
043529120412      * gruppo 2
043530120411     c                   when      idx = 3
043531120412     c     exddc         lookup    skd3(x)                                90
043532120412     c                   if        not *in90
043533120412     c     vuoto8        lookup    skd3(x)                                90
043534120412     c                   z-add     exddc         skd3(x)
043535120412     c                   endif
043536120411     c                   add       1             sk3(x)
043537120411     c                   endsl
043538120412     c                   endif
043539120412      *
043600041118     c                   endif
043700041118
043800041118     c                   endsr
043900041118     c*-------------------------------------------------
044000041118     c     dettagli      begsr
044100041118     c*-------------------------------------------------
044200041118      * calcola i valori per singola data nel periodo richiesto a video
044300041118
044400041118      *consegne
044500041118     c                   exsr      NoN_Cons
044600041118
044700041118      *orm
044800041118     c                   exsr      ORM_noes
044900041118
045000041118      *bollettato
045100041118     c                   exsr      col_nobo
045200041118
045300041118      *valorizzazioni
045400041124     c   21              exsr      val_aut
045500041118
045600050208     c                   if        anomalo = *blank
045700050419     c                   z-add     banrie        tbanrie
045800050420     c                   move      Fbanrie       Ftbanrie
045900110304     c                   z-add     banbdar       tbandbar
046000050420     c                   move      Fbanaff       Ftbanaff
046100110304     c     banrie        sub       banbdar       tbandiff
046200050419     c                   z-add     colrit        tcolrit
046300050420     c                   move      fcolrit       ftcolrit
046400050419     c                   z-add     totcobo       tcobo
046500050420     c                   z-add     totcospn      tcospn
046600050420     c                   move      fcospn        ftcospn
046700041118     c     colrit        sub       totcobo       tcoldiff
046800050420
046900050420      **  Segnala con (*) se a fronte di una differenza
047000050420      **    vi è una differenza anche fra gli spuntati e i bollettati
047100050420     c                   if        tcoldiff <> 0 and
047200050420     c                             tcospn   <> tcobo
047300050420     c                   move      '*'           ftcoldif
047400050420     c                   end
047500041118
047600041118      * sommo sui campi di totale per autista periodo
047700041118     c                   add       tbolaff       ttbolaff
047701120406     c                   add       tbolmig       ttbolmig
047800041118     c                   add       totnof        ttotnof
047900041118     c                   add       totesp        ttotesp
048000041118     c                   add       totlav        ttotlav
048100041118     c                   add       tincnof       ttincnof
048200041118     c                   add       tcolnos       ttcolnos
048300041118     c                   add       tormaf        ttormaf
048400041118     c                   add       tormnf        ttormnf
048500041118     c                   add       tormnfa       ttormnfa
048600041118     c                   add       totcos        ttotcos
048700041118     c                   add       totcom        ttotcom
048701120508     c                   add       totggl        ttotggl
048800041118
048900050419     c                   add       tbanrie       ttbanrie
049000110304     c                   add       tbandbar      ttbandbar
049100041118     c                   add       tbandiff      ttbandiff
049200050419     c                   add       tcolrit       ttcolrit
049300050419     c                   add       tcoldiff      ttcoldiff
049400050420     c                   move      Ftbanrie      FFtbanrie
049500050420     c                   move      Ftbanaff      FFtbanaff
049600050420     c                   move      Ftcolrit      FFtcolrit
049700050420     c                   move      Ftcoldif      FFtcoldif
049800050419     c                   add       tcobo         ttcobo
049900050420     c                   add       tcospn        ttcospn
050000050420     c                   move      Ftcospn       FFtcospn
050100041118     c                   add       totdifv       ttotdifv
050200050208     c                   if        *in21 and ttotcom <> 0
050300050419     c                   eval      Wttotdifp =((ttotcom -ttotcos) *100) /ttotcom
050400050419     c                   if        wttotdifp > 999,99
050500050419     c                   eval      ttotdifp = 999,99
050600050419     c                   else
050700050419     c                   if        wttotdifp < -999,99
050800050419     c                   eval      ttotdifp = -999,99
050900050419     c                   else
051000050419     c                   eval      ttotdifp = Wttotdifp
051100050419     c                   end
051200050419     c                   end
051300050208     c                   else
051400050208     c                   eval      ttotdifp = 0
051500050208     c                   endif
051600050208     c                   endif
051700041118      * una riga in stampa per ogni data autista nel periodo richiesto
051800041118     c                   if        dett = 'S'
051900041118     c                   if        line > 60
052000041118     c                   except    testata
052100041118     c                   endif
052200041118      *data in stampa x dettaglio
052300060118     c                   if        primo <> *blank
052400041118     c                   move      exddc         dataiso
052500041118     c                   move      dataiso       dataeur
052600041118     c                   move      dataeur       exddcs            8 0
052700060118     c                   endif
052800041124      *in caso di stampa anomalie valo=' ' verifico se i valori sensibili
052900041124      * sono diversi da zero se almeno uno lo è stampo la riga
053000041124     c                   if        valo = *blank and tcoldiff <> 0 or
053100041124     c                             valo = *blank and tbandiff <> 0 or
053200041124     c                             valo = *blank and tcolnos  <> 0 or
053300041124     c                             valo = *blank and tincnof  <> 0 or
053400050208     c                             valo <> *blank or anomalo <> *blank
053500050208     c                   if        anomalo <> *blank
053600050208     c                   except    dettaN
053700050208     c                   else
053800050208     c                   except    detta
053900050208     c                   end
054000041118     c                   endif
054100041124     c                   endif
054200041118     c* azzero campi dettaglio per data successiva
054300110304     c                   clear                   banbdar           5 0
054400050420     c                   clear                   Fbanaff           1
054500050208     c                   clear                   banrie            5 0
054600050420     c                   clear                   Fbanrie           1
054700050208     c                   clear                   colrit            5 0
054800050420     c                   clear                   Fcolrit           1
054900050419     c                   clear                   tcolrit           5 0
055000050420     c                   clear                   Ftcolrit          1
055100041118     c                   clear                   tbolaff           5 0
055101120406     c                   clear                   tbolmig           5 0
055200041118     c                   clear                   totnof            5 0
055300041118     c                   clear                   totesp            5 0
055400041118     c                   clear                   totlav            5 0
055500041118     c                   clear                   tormaf            5 0
055600041118     c                   clear                   tormnf            5 0
055700041118     c                   clear                   tormnfa           5 0
055800041118     c                   clear                   tincnof           9 2
055900041118     c                   clear                   tcolnos           5 0
056000050419     c                   clear                   tbandiff          3 0
056100050419     c                   clear                   tbanrie           3 0
056200050420     c                   clear                   Ftbanrie          1
056300110304     c                   clear                   tbandbar          3 0
056400050420     c                   clear                   Ftbanaff          1
056500041118     c                   clear                   tcoldiff          5 0
056600050420     c                   clear                   Ftcoldif          1
056700041118     c                   clear                   totcom            9 2
056701120508     c                   clear                   totggl            3 0
056800041118     c                   clear                   totcos            9 2
056900120508     c                   clear                   totdifv           5 0
057000041123     c                   clear                   totdifp           5 2
057100050419     c                   clear                   wtotdifp          9 2
057200041118
057300041118     c                   endsr
057400071030     c*-------------------------------------------------
057500071030     c     sommadettagli begsr
057600071030     c*-------------------------------------------------
057700071030      * somma valori di dettaglio per ottenere il totale dell'estrazione
057800071030     c                   add       ttbolaff      xtbolaff
057801120406     c                   add       ttbolmig      xtbolmig
057900071030     c                   add       ttotnof       xtotnof
058000071030     c                   add       ttotesp       xtotesp
058100071030     c                   add       ttotlav       xtotlav
058200071030     c                   add       ttcolnos      xtcolnos
058300071030     c                   add       ttincnof      xtincnof
058400071030     c                   add       ttormaf       xtormaf
058500071030     c                   add       ttormnf       xtormnf
058600071030     c                   add       ttormnfa      xtormnfa
058700071030     c                   add       ttcolrit      xtcolrit
058800071030     c                   add       ttcobo        xtcobo
058900071030     c                   add       ttcoldiff     xtcoldiff
059000071030     c                   add       ttcospn       xtcospn
059100071030     c                   add       ttbanrie      xtbanrie
059200110304     c                   add       ttbandbar     xtbandbar
059300071030     c                   add       ttbandiff     xtbandiff
059400071030     c                   add       ttotcom       xtotcom
059401120508     c                   add       ttotggl       xtotggl
059500071030     c                   add       ttotcos       xtotcos
059600071030     c                   add       ttotdifv      xtotdifv
059700071030     c                   endsr
059701120410     c*----------------------------------------------------
059702120410     c     datiautista   begsr
059703120410     c*----------------------------------------------------
059704120410      *decodifica autista
059705120410     c                   clear                   despdr           20
059706120412     c                   clear                   massa             5 1
059707120410     c                   clear                   kilom             3
059708120410     c                   z-add     1             idx
059709120410     c     kapdf         chain     fiapd01l
059710120410      *se l'anagrafica non corrisponde non stampo la decodifica
059711120410     c                   if        %found(fiapd01l)
059712120410     c                   movel     apdrsc        despdr
059713120510     c                   movel     apdpdd        xapdpdd           1
059714120410     c                   movel     apdflr        dapdflr
059715120410     c                   movel     §apdmcp       massa
059716120410     c                   movel     §apdkm        kilom
059717120410     c                   endif
059718120410      * reperimento indice totalizzazione skiere correlate per massa mezzo
059719120412
059720120412      * se autista fittizio cioè non fattura non rientra nei conteggi di riepilogo
059721120412     c                   if        APDPDD = *blank
059722120410     c                   move      'MAS'         tbecod
059723120411     c                   movel     §apdmcp       tbeke1
059724120410     c     ktbe          setgt     tntbe01l
059725120410     c                   readp     tntbe01l
059726120410     c                   if        not%eof(tntbe01l)
059727120410     c                   movel     tbeuni        vuoto2
059728120410     c     vuoto2        lookup    sk1tpm(idx)                            96
059729120411     c   96              add       1             sk2nmz(idx)
059730120508      *conteggio autisti impiegati per turnazione
059731120508      *memorizza codici
059732120508     c                   z-add     1             ida               3 0
059733120508     c     apdpdr        lookup    skaut(ida)                             96
059734120508     c                   if        not *in96
059735120508     c                   add       1             sk2naut(idx)
059736120508     c                   z-add     1             ida
059738120508     c     autzero       lookup    skaut(ida)                             96
059739120508     c   96              add       apdpdr        skaut(ida)
059740120508     c                   endif
059741120410     c                   endif
059742120410      * reperimento tariffa
059743120410     c                   clear                   tariffa           3
059744120412     c                   clear                   partar
059745120412     c                   move      expdr         parauti
059746120412     c                   move      exddc         pardati
059747120412     c                   move      'G'           partsri
059748120410     c                   call      'FNLRR7TR'
059749120412     c                   parm                    partar
059750120410     c                   if        error = *blank and esito ='1'
059751120410     c                   move      partaro       tariffa
059752120410     c                   endif
059753120412     c                   endif
059754120410     c                   endsr
059755120411     c*-------------------------------------------------
059756120411     c     prepararie    begsr
059757120411     c*-------------------------------------------------
059758120412      *gruppo 1 calcolo media autisti al lavoro per giornate di lavoro per gruppo mezzo
059759120411     c                   z-add     1             x                 2 0
059760120411     c                   clear                   vuoto8            8 0
059761120412     c     vuoto8        lookup    skd1(x)                                90
059762120411     c     x             sub       1             ngior             2 0
059763120411     c                   xfoot     sk1           grup(1)
059764120412     c                   if        grup(1) > 0 and ngior > 0
059765120412     c     grup(1)       div       ngior         sk2nmz(1)
059766120516     c                   if        sk2naut(1) <> 0
059767120508     c                   eval      sk2tur(1)= (sk2naut(1) - sk2nmz(1))*100
059768120510     c                             / sk2naut(1)
059769120516     c                   endif
059770120412     c                   endif
059771120412      *gruppo 2
059772120412     c                   z-add     1             x
059773120412     c     vuoto8        lookup    skd2(x)                                90
059774120412     c     x             sub       1             ngior
059775120411     c                   xfoot     sk2           grup(2)
059776120412     c                   if        grup(2) > 0 and ngior > 0
059777120412     c     grup(2)       div       ngior         sk2nmz(2)
059778120516     c                   if        sk2naut(2) <> 0
059779120508     c                   eval      sk2tur(2)= (sk2naut(2) - sk2nmz(2))*100
059780120510     c                             / sk2naut(2)
059781120516     c                   endif
059782120412     c                   endif
059783120412      *gruppo 3
059784120412     c                   z-add     1             x
059785120412     c     vuoto8        lookup    skd3(x)                                90
059786120412     c     x             sub       1             ngior
059787120411     c                   xfoot     sk3           grup(3)
059788120411     c                   if        grup(3) > 0 and ngior > 0
059789120411     c     grup(3)       div       ngior         sk2nmz(3)
059790120516     c                   if        sk2naut(3) <> 0
059791120508     c                   eval      sk2tur(3)= (sk2naut(3) - sk2nmz(3))*100
059792120510     c                             / sk2naut(3)
059793120516     c                   endif
059794120411     c                   endif
059795120411     c                   endsr
059796120411     c*-------------------------------------------------
059797120411     c     calcolorie    begsr
059798120411     c*-------------------------------------------------
059799120411      ** Stampa riepilogo per tipo mezzo
059800120411     c                   if        *in21
059801120411     c                   except    riepilogot
059802120411     c                   except    riepilogo
059803120411     c     1             do        3             i                 1 0
059804120411     c                   if        sk6com(i) <> 0
059805120411      * %scostamento competenze costi
059806120411     c                   eval      scop(i)=((Sk6com(i)-Sk5cos(i))*100)/Sk6com(i)
059807120411      * scostamento competenze costi
059808120411     c                   eval      scos(i) = Sk6com(i)-Sk5cos(i)
059809120411     c                   if        sk2nmz(i) > 0
059810120411      * competenze unitario
059811120516     c                   if        auval(i) <> 0
059812120411     c                   eval      scomu(i)= sk6com(i) / auval(i)
059813120411      * costo unitario
059814120411     c                   eval      scosu(i)= sk5cos(i) / auval(i)
059815120516     c                   endif
059816120411      * %scostamento unitario competenze costi
059817120516     c                   if        scomu(i) <> 0
059818120411     c                   eval      scopu(i)=((scomu(i)-scosu(i)) *100)/scomu(i)
059819120516     c                   endif
059820120411      * scostamento unitario competenze costi
059821120411     c                   eval      scou(i) = Scomu(i)- Scosu(i)
059822120510      * spedizioni unitarie
059823120516     c                   if        auval(i) <> 0
059824120510     c                   eval      sk3spu(i)= sk3spt(i) / auval(i)
059825120510     c                   eval      sk3riu(i)= sk3rit(i) / auval(i)
059826120516     c                   endif
059827120411     c                   endif
059828120411     c                   endif
059829120411     c                   except    riepilogo1
059830120411     c                   enddo
059831120508      * calcola totali secondo prospetto
059832120508     c                   xfoot     sk2naut       sk2nautt
059833120508     c                   xfoot     sk2nmz        sk2nmzt
059834120516     c                   if        sk2nautt <> 0
059835120510     c                   eval      sk2turt =((sk2nautt - sk2nmzt)*100)/sk2nautt
059836120516     c                   endif
059837120508     c                   xfoot     sk3spt        sk3sptt
059838120510     c                   xfoot     sk3spu        sk3sput
059839120510     c                   xfoot     sk3rit        sk3ritt
059840120510     c                   xfoot     sk3riu        sk3riut
059841120508     c                   xfoot     sk4spm        sk4spmt
059842120508     c                   xfoot     sk5cos        sk5cost
059843120508     c                   xfoot     sk6com        sk6comt
059844120508     c                   eval      scost = sk6comt - sk5cost
059845120516     c                   if        sk6comt <> 0
059846120508     c                   eval      scopt =((Sk6comt-Sk5cost)*100)/Sk6comt
059847120516     c                   endif
059848120508     c                   xfoot     scosu         scosut
059849120508     c                   xfoot     scomu         scomut
059850120508     c                   eval      scout = scomut - scosut
059851120516     c                   if        sk6comt <> 0
059852120508     c                   eval      scoput =((Scomut-Scosut)*100)/Scomut
059853120516     c                   endif
059854120508     c                   except    totriep
059855120411     c                   endif
059856120411     c                   endsr
059857011026     c*-------------------------------------------------
059900011026     c     *inzsr        begsr
060000011026     c*-------------------------------------------------
060100020304     c     *entry        plist
060200020304     c                   parm                    kpjba
060300030723     c                   movel     kpjbu         param
060400031114
060500041124      * se richiesta stampa giorno 'C' comprende anche le valorizzazioni
060600041124      * il flag valo è <> ' ' stampa e calcola i valori dell'ultima
060700041124      * parte del prospetto
060800041124     c     valo          comp      *blank                             2121
060900041025     c                   movel     aut1          filges            3 0
061000030723     c                   move      data1         dataiso
061100030723     c                   move      dataiso       dataeur
061200030723     c                   move      dataeur       datadal           8 0
061300030723     c                   move      data2         dataiso
061400030723     c                   move      dataiso       dataeur
061500030723     c                   move      dataeur       dataal            8 0
061600041025     c     filges        chain     azorg01l
061700041025     c                   movel     orgdes        desfil           20
061800011026
061900020218     C                   time                    w0120            14 0
062000020218     C                   move      w0120         wdat              8 0
062100041103     C                   movel     w0120         wora              6 0
062200020404     c                   z-add     99            line
062300041026     C     *LIKE         DEFINE    TBLKUT        KKUT
062400041026     C     *LIKE         DEFINE    TBLCOD        KCOD
062500041026     C     *LIKE         DEFINE    TBLKEY        KKEY
062600041026     C*
062700041026     C     kqdt          klist
062800041026     C                   kfld                    filges
062900041115     c                   kfld                    aut1
063000041026     C                   kfld                    data1
063100041026     C*
063200041027     C     kdet          klist
063300041026     C                   kfld                    qdtfgs
063400041115     C                   kfld                    exddc
063500041115     C                   kfld                    expdr
063600050207     C*
063700050207     C     kqbt          klist
063800050207     C                   kfld                    qdtfgs
063900050207     C                   kfld                    qdtddc
064000050207     C                   kfld                    qdtpdr
064100041026     C*
064200041026     C     ktab          klist
064300041026     C                   kfld                    kkut
064400041026     C                   kfld                    kcod
064500041026     C                   kfld                    kkey
064600041026     C                   z-add     1             kkut
064700041027     C*
064800041027     C     ktbe          klist
064900041027     C                   kfld                    tbecod
065000041027     C                   kfld                    tbeke1
065100041027     C*
065200041026
065300021203     c     kapdf         klist
065400030723     c                   kfld                    tipo              1
065500041115     c                   kfld                    expdr
065600041025     c                   move      'A'           tipo
065700070828     c     kapd          klist
065800070828     c                   kfld                    tipo              1
065900070828     c                   kfld                    qdtpdr
066000070828     c                   move      'A'           tipo
066100030725
066200030505     C                   Z-ADD     1             CODUT
066300030505     C                   CALL      'X§PARUT'
066400030505     C                   PARM                    UT§DSE
066500030505     C                   MOVEL     RAGUT         RSUT             20
066600030505     C                   MOVEL     REC80         CNCR80
066700050419     c                   move      *all'_'       finkao          198
066800041124     c                   move      *all'_'       finkab          134
066900041116      *date in stampa parmetri
067000041116     c                   move      data1         dataiso
067100041116     c                   move      dataiso       dataeur
067200041116     c                   move      dataeur       data1s            8 0
067300041116     c                   move      data2         dataiso
067400041116     c                   move      dataiso       dataeur
067500041116     c                   move      dataeur       data2s            8 0
067600090609      * reperimento descrizione tipo servizio 'E' x stampa
067700090609     c                   clear                   ds5e
067800090609     c                   clear                   desserv           8
067900090609     c                   move      '5E'          kcod
068000090609     c                   movel(p)  'E'           kkey
068100090609     c     ktab          chain     tabel00f                           96
068200090609     c                   if        not *in96
068300090609     c                   movel     tbluni        ds5e
068400090609     c                   eval      desserv = §5ED08
068500090609     c                   endif
068501120410      * carico tabella dei tipi mezzo per indicizzare le totalizzazioni per tipo
068502120410      * successivamente nelle 6 skiere correlate
068503120412     c                   clear                   vuoto2           20
068504120410     c                   z-add     1             idx               1 0
068505120410     c                   move      'MAS'         tbecod
068506120410     c     tbecod        setll     tntbe01l
068507120410     c                   do        *hival
068508120410     c     tbecod        reade     tntbe01l
068509120410     c                   if        %eof(tntbe01l)
068510120410     c                   leave
068511120410     c                   endif
068512120410      * non fatto con causale a responsabilità autista
068513120410     c     vuoto2        lookup    sk1tpm(idx)                            96
068514120410     c   96              movel     tbeuni        sk1tpm(idx)
068517120410     c                   enddo
068518120411      * carico tabella dei tipi mezzo per indicizzare le totalizzazioni per tipo
068519120411      * successivamente nelle 6 skiere correlate
068520120411     c                   clear                   obbaz             3 0
068522120411     c                   move      'MIC'         tbecod
068523120411     c                   movel(p)  filges        tbeke1
068524120411     c     ktbe          chain     tntbe01l
068527120411     c                   if        %found(tntbe01l)
068528120411     c                   movel     tbeuni        obbaz
068529120411     c                   endif
068600011026     c                   endsr
068700020404     C*--------------------------------------------------------------------
068800041118     Oprtf198   E            testata          02
068900041103     o                       rsut                21
069000130321     o               21                         149 'R I E P I L O G O  -
069100130321     o                                                S E R V I Z I    -
069200130321     o                                                P R E S E   E   -
069300130321     o                                              C O N S E G N E '
069400130321     o              N21                         155 'R I E P I L O G O  -
069500130321     o                                                S E R V I Z I    -
069600130321     o                                                P R E S E   E   -
069700130321     o                                              C O N S E G N E   -
069800050204     o                                              C O N   -
069900050204     o                                              D I S C O R D A N Z E'
070000050419     o                                          186 'FNLRR7R'
070100050204     O                                          193 'Pag'
070200050204     o                       page          z    198
070300041103     O          e            testata     1
070400041103     o                       knsif               11
070500041103     o                       knmus               22
070600041118     o                                           80 'Periodo'
070700041118     o                       data1s              92 '  /  /    '
070800041118     o                                           95 'al'
070900041118     o                       data2s             106 '  /  /    '
071000041118     o                                          120 'Autisti'
071100041118     o                       aut1               128
071200041118     o                                          130 '-'
071300041118     o                       aut2               138
071400050419     o                       udate         y    187
071500050419     o                       wora               198 '  :  :  '
071600041026     O          e            testata     2
071700041115     o                                           18 'Autotrasportatore'
071800120406     o                                           80 'C   O   N   S   E   G   -
071900050419     o                                              N   E'
072000120405     o                                          120 'R  I  T  I  R  I'
072100120405     o                                          145 'C  O  L  L  I'
072200120405     o*                                         150 'B A N C A L I'
072300041115     O          e            testata     1
072400120412     o*                      desserv             72
072401120412     o                                           71 'PTY.h10'
072500120412     o                                           83 'Non'
072501120412     o                                           99 'Sped'
072600120405     o                                          101 '|'
072700120405     o                                          119 '( O    R    M )'
072800120405     o                                          124 '|'
072900120405     o                                          130 'Riti'
073000120405     o                                          139 'Bollet'
073100120405     o                                          146 'Diffe'
073200120405     o                                          153 'Spun'
073300120405     o*                                         134 '|'
073400120405     o*                                         139 'Rien'
073500120405     o*                                         146 ' Da '
073600120405     o*                                         153 'Diffe'
073700050419     o                                          155 '|'
073800050419     o               21                         190 'V A L O R I Z Z A Z I O N-
073900050419     o                                               I'
074000050419     O          e            testata     1
074001120406     o                                           35 'Massa'
074002120412     o                                           40 'Km.'
074003120412     o                                           46 'Tipo'
074004120508     o                                           47 '|'
074100120412     o                                           55 'Spediz.'
074200120412     o                                           62 'Non'
074300120412     o                                           68 'Non'
074400120410     o                                           76 'Lasc.'
074500120410     o                                           84 'Spunt.'
074600120410     o                                           92 'Non'
074601120406     o                                           99 'Area'
074700120405     o                                          101 '|'
074800120405     o                                          106 'Affi'
074900120405     o                                          113 'Non'
075000120412     o                                          122 'Non Fat'
075100120405     o                                          124 '|'
075200120405     o                                          130 'rati'
075300120405     o                                          138 'tati'
075400120405     o                                          146 'renza'
075500120405     o                                          153 'tati'
075600120405     o*                                         134 '|'
075700120405     o*                                         140 'trati'
075800120405     o*                                         146 'Rend.'
075900120405     o*                                         153 'renza'
076000050419     o                                          155 '|'
076100050419     o               21                         166 'Competenze'
076200050419     o               21                         175 'Costi'
076300120508     o               21                         186 'Differ.'
076400120508     o               21                         194 'Perc.%'
076401120508     o               21                         198 'Ggl'
076500041123     O          e            testata     1
076501120412     o                                           34 'Q.li'
076502120412     o                                           46 'Tar.'
076503120508     o                                           47 '|'
076600120412     o                                           55 'Affidate'
076700120412     o                                           62 'Fatte'
076800120412     o                                           69 'Fatti'
076900120410     o                                           76 'Avv.'
077000120410     o                                           84 'Rientro'
077100120410     o                                           94 'Incass.'
077101120410     o                                          100 'Micro'
077200120405     o                                          101 '|'
077300120405     o                                          106 'dati'
077400120405     o                                          114 'Fatti'
077500120405     o                                          123 'Resp.Aut'
077700120405     o*                                         134 '|'
077800050419     o                                          155 '|'
077900041123     O          e            testata     1
078000050419     o               21      finkao             198
078100041124     o              n21      finkab             134
078200050419      *----
078300041115     O          e            detta       1
078400041117     o                       exddcs              15 '  /  /    '
078401120508     o                                           47 '|'
078500120412     o                       tbolaff       z     55
078600120412     o                       totnof        z     61
078700120412     o                       totesp        z     70
078800120411     o                       totlav        z     76
078900120507     o                       tcolnos       z     83
079000120410     o                       tincnof       4     93
079001120410     o                       tbolmig       z     99
079100120405     o                                          101 '|'
079200120504     o                       tormaf        z    106
079300120405     o                       tormnf        z    114
079400120405     o                       tormnfa       z    122
079500120405     o                                          124 '|'
079600120405     o                       tcolrit       z    130
079700120405     o                       Ftcolrit           132
079800120405     o                       tcobo         z    138
079900120405     o                       tcoldiff      m    150
080000120405     o                       Ftcoldif           147
080100120405     o                       tcospn        z    153
080200120405     o                       Ftcospn            154
080300120405     o*                                         134 '|'
080400120405     o*                      tbanrie       z    138
080500120405     o*                      Ftbanrie           140
080600120405     o*                      tbandbar      z    145
080700110301     o*                      Ftbanaff           147
080800120405     o*                      tbandiff      m    153
080900050419     o                                          155 '|'
081000050419     o               21      totcom        4    166
081100050419     o               21      totcos        4    177
081200120508     o               21      totdifv       m    186
081300120508     o               21      totdifp       m    194
081301120508     o               21      totggl        m    198
081400050419      *----
081500041115     O          e            tdetta      1
081600041115     o                       expdr                8
081700041115     o                       despdr              30
081701120412     o                       massa               35 '    ,0'
081702120412     o                       kilom               40
081703120412     o                       tariffa             46
081704120508     o                                           47 '|'
081800120412     o                       ttbolaff      z     55
081900120412     o                       ttotnof       z     61
082000120412     o                       ttotesp       z     70
082100120411     o                       ttotlav       z     76
082200120507     o                       ttcolnos      z     83
082300120411     o                       ttincnof      4     93
082301120406     o                       ttbolmig      z     99
082400120405     o                                          101 '|'
082500120405     o                       ttormaf       z    106
082600120405     o                       ttormnf       z    114
082700120405     o                       ttormnfa      z    122
082800120405     o                                          124 '|'
082900120405     o                       ttcolrit      z    130
083000120405     o                       FFtcolrit          132
083100120405     o                       ttcobo        z    138
083200120405     o                       ttcoldiff     m    146
083300120405     o                       FFtcoldif          147
083400120405     o                       ttcospn       z    153
083500120405     o                       FFtcospn           154
083600120405     o*                                         134 '|'
083700120405     o*                      ttbanrie      z    138
083800120405     o*                      FFtbanrie          140
083900120405     o*                      ttbandbar     z    145
084000110301     o*                      FFtbanaff          147
084100120405     o*                      ttbandiff     m    153
084200050419     o                                          155 '|'
084300050419     o               21      ttotcom       4    166
084400050419     o               21      ttotcos       4    177
084500120508     o               21      ttotdifv      m    186
084600120508     o               21      ttotdifp      m    194
084601120508     o               21      ttotggl       m    198
084700050419      *---
084800041116     O          e            finka       0
084900041116     o                       expdr                8
085000041116     o                       despdr              30
085001120412     o                       massa               35 '    ,0'
085002120412     o                       kilom               40
085003120412     o                       tariffa             46
085004120508     o                                           47 '|'
085100120412     o                       ttbolaff      z     55
085200120412     o                       ttotnof       z     61
085300120412     o                       ttotesp       z     70
085400120411     o                       ttotlav       z     76
085500120507     o                       ttcolnos      z     83
085600120410     o                       ttincnof      4     93
085601120406     o                       ttbolmig      z     99
085700120405     o                       ttormaf       z    106
085800120405     o                       ttormnf       z    114
085900120405     o                       ttormnfa      z    122
086000120405     o                       ttcolrit      z    130
086100120405     o                       FFtcolrit          132
086200120405     o                       ttcobo        z    138
086300120405     o                       ttcoldiff     m    146
086400120405     o                       FFtcoldif          147
086500120405     o                       ttcospn       z    153
086600120405     o                       FFtcospn           154
086700120405     o*                      ttbanrie      z    138
086800120405     o*                      FFtbanrie          140
086900120405     o*                      ttbandbar     z    145
087000110301     o*                      FFtbanaff          147
087100120405     o*                      ttbandiff     m    153
087200050419     o               21      ttotcom       4    166
087300050419     o               21      ttotcos       4    177
087400120508     o               21      ttotdifv      m    186
087500120508     o               21      ttotdifp      m    194
087501120508     o               21      ttotggl       m    198
087600050419      *-----
087700041118     O          e            finka       0
087800041116     o                       expdr                8
087900041116     o                       despdr              30
087901120412     o                       massa               35 '    ,0'
087902120412     o                       kilom               40
087903120412     o                       tariffa             46
087904120508     o                                           47 '|'
088000120412     o                       ttbolaff      z     55
088100120412     o                       ttotnof       z     61
088200120412     o                       ttotesp       z     70
088300120411     o                       ttotlav       z     76
088400120507     o                       ttcolnos      z     83
088500120410     o                       ttincnof      4     93
088501120406     o                       ttbolmig      z     99
088600120405     o                       ttormaf       z    106
088700120405     o                       ttormnf       z    114
088800120405     o                       ttormnfa      z    122
088900120405     o                       ttcolrit      z    130
089000120405     o                       FFtcolrit          132
089100120405     o                       ttcobo        z    138
089200120405     o                       ttcoldiff     m    146
089300120405     o                       FFtcoldif          147
089400120405     o                       ttcospn       z    153
089500120405     o                       FFtcospn           154
089600120405     o*                      ttbanrie      z    138
089700120405     o*                      FFtbanrie          140
089800120405     o*                      ttbandbar     z    145
089900110301     o*                      FFtbanaff          147
090000120405     o*                      ttbandiff     m    153
090100050419     o               21      ttotcom       4    166
090200050419     o               21      ttotcos       4    177
090300120508     o               21      ttotdifv      m    186
090400120508     o               21      ttotdifp      m    194
090401120508     o               21      ttotggl       m    198
090500050419      *
090600041118     O          e            finka       1
090700120406     o*                                          79 '|'
090800120405     o                                          124 '|'
090900120405     o*                                         134 '|'
091000050419     o                                          155 '|'
091100050419      *
091200050208     O          e            tdettAN     1
091300050207     o                       expdr                8
091400050207     o                       despdr              30
091500050420     o                                           74 '  D A T I   N O N   D I S -
091600050419     O                                              P O N I B I L I'
091700120406     o*                                          79 '|'
091800120405     o                                          124 '|'
091900120405     o*                                         134 '|'
092000050419     o                                          155 '|'
092100050208     O          e            dettAN      1
092200050208     o                       exddcs              15 '  /  /    '
092300050419     o                                           74 '  D A T I   N O N   D I S -
092400050419     O                                              P O N I B I L I'
092500120406     o*                                          79 '|'
092600120405     o                                          124 '|'
092700120405     o*                                         134 '|'
092800050419     o                                          155 '|'
092900050419      *****
093000071030     O          e            sommadet    1
093100071030     o                                           13 'T O T A L E'
093101120508     o                                           47 '|'
093200120412     o                       xtbolaff      z     55
093300120412     o                       xtotnof       z     61
093400120412     o                       xtotesp       z     70
093500120411     o                       xtotlav       z     76
093600120507     o                       xtcolnos      z     83
093700120410     o                       xtincnof      4     93
093701120410     o                       xtbolmig      z     99
093800120405     o                                          101 '|'
093900120405     o                       xtormaf       z    106
094000120405     o                       xtormnf       z    114
094100120405     o                       xtormnfa      z    122
094200120405     o                                          124 '|'
094300120405     o                       xtcolrit      z    130
094400120405     o                       xtcobo        z    138
094500120405     o                       xtcoldiff     m    146
094600120405     o                       xtcospn       z    153
094700120405     o*                                         134 '|'
094800120405     o*                      xtbanrie      z    138
094900120405     o*                      xtbandbar     z    145
095000120405     o*                      xtbandiff     m    153
095100071030     o                                          155 '|'
095200071030     o               21      xtotcom       4    166
095300071030     o               21      xtotcos       4    177
095400120508     o               21      xtotdifv      m    186
095500120508     o               21      xtotdifp      m    194
095501120510     o*              21      xtotggl       m    198
095600071030      *---
095700050419     O          e            Legend      2
095800050419     o               21      finkao             198
095900050419     o              n21      finkab             134
096000050419     O          e            Legend      1
096100050419     o                                            1 ' '
096200050419     o                                          + 0 '* = La differenza fra coll'
096300050420     o                                          + 0 'i ritirati e bollettati è '
096400050420     o                                          + 0 'confermata anche dai colli'
096500050421     o                                          + 0 ' spuntati. '
096600050421     o                                          + 2 '§ = Il nr.dei colli spunta'
096700050419     o                                          + 0 'ti è inesatto per la prese'
096800050419     o                                          + 0 'nza di "Bancali oltre 20 c'
096900050420     o                                          + 0 'olli" spuntati '
097000050420     o                                          + 0 'parzialmente'
097100120104     O*         e            Legend      1
097200120104     o*                                           1 ' '
097300120104     o*                                         + 0 'Gli Importi sono '
097400120104     o*                                         + 0 'comprensivi del costo '
097500120104     o*                                         + 0 'stimato Adeguamento '
097600120104     o*                                         + 0 'Carburante'
097601120411     o*--------------------------------------------------------------------------
097602120412     O          e    21      riepilogot       02
097603120412     o                       rsut                21
097604130321     o                                          149 'R I E P I L O G O  -
097605130321     o                                                S E R V I Z I    -
097606130321     o                                                P R E S E   E   -
097607130321     o                                              C O N S E G N E '
097614120412     o                                          186 'FNLRR7R'
097615120412     O                                          193 'Pag'
097616120412     o                       page          z    198
097617120509     O          e    21      riepilogot  2
097618120412     o                       knsif               11
097619120412     o                       knmus               22
097620120412     o                                           80 'Periodo'
097621120412     o                       data1s              92 '  /  /    '
097622120412     o                                           95 'al'
097623120412     o                       data2s             106 '  /  /    '
097624120412     o                                          120 'Autisti'
097625120412     o                       aut1               128
097626120412     o                                          130 '-'
097627120412     o                       aut2               138
097628120412     o                       udate         y    187
097629120412     o                       wora               198 '  :  :  '
097630120509     O          e    21      riepilogot  2
097631120509     o                                           50 'AUTISTI   UTILIZZATI'
097633120510     o                                           78 'SPEDIZIONI'
097634120510     o                                          102 'R I T I R I'
097635120510     o                                          140 'T  O  T  A  L  E'
097636120510     o                                          190 'U  N  I  T  A  R  I  O'
097637120509     o                                           56 '|'
097638120510     o                                           88 '|'
097639120510     o                                          107 '|'
097640120510     o                                          158 '|'
097641120510     o                                          197 '|'
097642120508     O          e    21      riepilogo   1
097643120508     o                                           49 'Media          '
097644120509     o                                           53 '%'
097645120510     o                                          154 '%'
097646120510     o*mm                                       187 '%'
097647120509     o                                           56 '|'
097648120511     o                                           85 'Tot'
097649120510     o                                           88 '|'
097650120510     o                                          107 '|'
097651120510     o                                          158 '|'
097652120510     o                                          197 '|'
097700120412     O          e    21      riepilogo   1
097701120412     o                                           10 'Tipo Mezzo'
097702120509     o                                           32 'Totali'
097704120509     o                                           49 '  gg    /  Obv '
097705120508     o                                           55 'Turn.'
097706120509     o                                           63 'Totali'
097707120511     o                                           75 'Unitarie'
097708120510     o                                           86 'Micro'
097709120510     o                                           95 'Totali'
097710120510     o                                          106 'Unitari'
097802120510     o                                          120 'Costi'
097803120510     o                                          135 'Competenze'
097804120510     o                                          148 'Scostamento'
097805120510     o                                          156 'Scost.'
097806120510     o                                          170 'Costo'
097807120510     o                                          183 'Competenze'
097808120510     o                                          195 'Scostamento'
097809120510     o*mm                                       190 'Scost.'
097810120509     o                                           56 '|'
097811120510     o                                           88 '|'
097812120510     o                                          107 '|'
097813120510     o                                          158 '|'
097814120510     o                                          197 '|'
097900120412     O          e    21      riepilogo   1
098000120411     o                       finkao             198
098100120412     O          e    21      riepilogo1  1
098200120508     o                       sk1tpm(i)           20
098201120508     o                       sk2naut(i)    z     30
098300120508     o                       sk2nmz(i)     z     38
098301120508     o                       obbaz         zb    48
098302120509     o                       sk2tur(i)     z     54
098303120509     o                                           56 '|'
098304120508     o                       sk3spt(i)     z     62
098305120510     o                       sk3spu(i)     z     75
098306120510     o                       sk4spm(i)     z     86
098307120510     o                                           88 '|'
098308120510     o                       sk3rit(i)     z     95
098309120510     o                       sk3riu(i)     z    105
098310120510     o                                          107 '|'
098400120510     o                       sk5cos(i)     2    122
098500120510     o                       sk6com(i)     2    135
098600120510     o                       scos(i)       m    148
098700120510     o                       scop(i)       m    156
098701120510     o                                          158 '|'
098800120510     o                       scosu(i)      2    170
098900120510     o                       scomu(i)      2    183
099000120510     o                       scou(i)       m    195
099100120510     o*mm                    scopu(i)      m    190
099200120510     o                                          197 '|'
099300120412     O          e    21      riepilogo1  1
099301120509     o                                           56 '|'
099302120510     o                                           88 '|'
099303120510     o                                          107 '|'
099304120510     o                                          158 '|'
099400120510     o                                          197 '|'
099401120508     O          e    21      totriep     1
099402120508     o                       finkao             198
099403120508     O          e    21      totriep     1
099404120508     o                       sk2nautt      z     30
099405120508     o                       sk2nmzt       z     38
099406120509     o                       sk2turt       z     54
099407120508     o                       sk3sptt       z     62
099408120510     o*mm                    sk3sput       z     75
099409120510     o                       sk4spmt       z     86
099410120510     o                                           88 '|'
099411120510     o                       sk3ritt       z     95
099412120510     o*mm                    sk3riut       z    105
099413120510     o                                          107 '|'
099414120510     o                       sk5cost       2    122
099415120510     o                       sk6comt       2    135
099416120510     o                       scost         m    148
099417120510     o                       scopt         m    156
099418120510     o                                          158 '|'
099419120510     o*mm                    scosut        2    170
099420120510     o*mm                    scomut        2    183
099421120510     o*mm                    scout         m    195
099422120510     o*mm                    scoput        m    190
099423120510     o                                          197 '|'
099500120509     O          e    21      Legend2     4
099600120412     o                                            2 ' *'
099700120510     o                                          + 0 'Autisti Totali : Aut che '
099800120510     o                                          + 0 'hanno lavorato almeno un'
099900120510     o                                          + 0 ' giorno nel periodo '
099901120510     O          e    21      Legend2     2
099902120510     o                                            2 '  '
099903120510     o                                          + 0 'Autisti Media gg: Aut che'
099904120510     o                                          + 0 ' mediamente hanno lavorato'
099905120510     o                                          + 0 ' in 1 giorno '
099906120510     O          e    21      Legend2     2
099907120510     o                                            2 '  '
099908120510     o                                          + 0 'Autisti % Turn : incidenza'
099909120510     o                                          + 0 ' Autisti media gg su Autis'
099910120510     o                                          + 0 'ti totali '
100001120510     O          e    21      Legend2     3
100002120412     o                                            3 ' **'
100100120412     o                                          + 0 'I valori unitari sono calc'
100200120412     o                                          + 0 'olati sommando,per tipo me'
100201120412     o                                          + 0 'zzo, i valori di'
100300120412     o                                          + 0 ' tutti gli autisti'
100301120412     o                                          + 0 ' diviso il nume'
100500120412     o                                          + 0 'ro dei giorni valorizzati'
