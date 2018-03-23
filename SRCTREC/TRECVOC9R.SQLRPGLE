000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000101160824     ftabel00f  if   e           k disk
000102160824     fwffss00f  o    e             disk    usropn
000103160824      //-----------------------------------------------------------------------
000104160824      //? VARIABILI
000105160824      //-----------------------------------------------------------------------
000106160824     d codut           s              1  0 inz(1)
000107160824     d lengh           s             15  5 inz(80)
000108160824     d kcod            s                   like(tblcod)
000109160824     d kkey            s                   like(tblkey)
000118160824      //-----------------------------------------------------------------------
000119160824      //? SCHIERE
000120160824      //-----------------------------------------------------------------------
000121160824     d cmd             s             80    dim(1) ctdata perrcd(1)
000122160823     d kpjba         e ds
000123160824     d ds$3          e ds
000500160824     d pdr             s                   like(ftdpdr)
000600160824     d ddc             s                   like(ftdddc)
000601160824     d pdrd            s                   like(ftdpdr)
000602160824     d ddcd            s                   like(ftdddc)
000603160824     d pdrf            s                   like(ftdpdr)
000604160824     d ddcf            s                   like(ftdddc)
000700160823     d b001            s                   like(fss001b)
000800160823     d b005            s                   like(fss005b)
000900160823     d b016            s                   like(fss016b)
001000160823     d b019            s                   like(fss016b)
001100160823     d b024            s                   like(fss016b)
001200160823     d b034            s                   like(fss016b)
001300160823     d b035            s                   like(fss016b)
001400160823     d b043            s                   like(fss016b)
001500160823     d b062            s                   like(fss016b)
001600160824     d fifssds       e ds                  extname(wffss00f)
001601160824     d fiftdds       e ds                  extname(fiftd00f)
001602160823      //-----------------------------------------------------------------------
001603160823      //? STRUTTURE DATI
001604160823      //-----------------------------------------------------------------------
001605160823     d param           ds
001606160823     d  waada                  1      4  0
001607160823     d  wmgda                  5      8  0
001608160823     d  wmmda                  5      6  0
001609160823     d  wggda                  7      8  0
001610160823     d  wdatada                1      8  0
001611160823     d  waaal                  9     12  0
001612160823     d  wmgal                 13     16  0
001613160823     d  wmmal                 13     14  0
001614160823     d  wggal                 15     16  0
001615160823     d  wdataal                9     16  0
001616160823     d  anno                  17     20  0
001618160823     c     *entry        plist
001619160823     c                   parm                    kpjba
001620160823     c                   eval      param = kpjbu
001621160824     c                   seton                                        lr
001622160824     c*
001623160824     c     ktabel        klist
001624160824     c                   kfld                    codut
001625160824     c                   kfld                    kcod
001626160824     c                   kfld                    kkey
001627160824     c* verifico se analizzare il TASP e il CEVP
001628160824     c                   eval      kcod = '$3'
001629160824     c                   movel     '1'           kkey
001630160824     c     ktabel        chain     tabel00f
001631160824     c                   if        %found(tabel00f)
001632160824     c                   eval      ds$3 = tbluni
001633160824     c                   end
001634160824      * clrpfm del file di work
001635160824     c                   call(e)   'QCMDEXC'
001636160824     c                   parm                    cmd(1)
001637160824     c                   parm                    lengh
001638160824      * se riuscito comando vado avanti con l'elaborazione
001639160824     c                   if        %error
001640160824     c                   return
001641160824     c                   end
001642160824     c                   open      wffss00f
001643160824     c* tasp o tas0
001644160824     c                   if        §$3D10>=wdataal
001645160824     c                   exsr      srtasp
001646160824     c                   else
001647160824     c                   exsr      srtas10
001648160824     c                   end
001649160824      //-----------------------------------------------------------------------
001650160824     c     srtas10       begsr
001651160824      //-----------------------------------------------------------------------
002000160823     C/EXEC SQL
002100160823     C+ DECLARE CONTANTE CURSOR FOR
002200160823     C+ with
002201160826     C+ eccet (pdr, ddc, b001, b005) AS (
002202160824     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
002203160824     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 and taspdc = ftdpdr
002204160824     C+ and tasddc = ftdddc then CETard else 0 end  as a,
002205160824     C+ case when ftdtsr = 'R' and taspdr = ftdpdr and tasdrt = ftdddc
002206160824     C+ then cetpar else 0 end as p FROM
002207160824     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
002208160824     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on :anno =
002209160824     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
002210160824     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:waada and tasmgs
002211160824     C+ between :wmgda and :wmgal ) as
002212160824     C+ tabella GROUP BY ftdpdr, ftdddc),
002213160824
002214160824     C+ ecced (pdrd, ddcd, b016, b019, b034, b035, b024, b043, b062) AS (
002215160824     C+ SELECT FTDPDR, FTDDDC, sum(x016) , sum(x019),
002216160824     C+ sum(x034), sum(x035),
002217160824     C+ sum(x024), sum(x043), sum(x062)
002218160824     C+ from ( SELECT FTDPDR, ftdddc,
002219160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 16
002220160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
002221160824     C+ then CEdimp else 0 end as x016,
002222160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 19
002223160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
002224160824     C+ then CEdimp else 0 end as x019,
002225160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 34
002226160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
002227160824     C+ then CEdimp else 0 end as x034,
002228160824     C+ case when ftdtsr = 'R' and cedcmp = 35
002229160824     C+ and taspdr = ftdpdr and tasdrt = ftdddc
002230160824     C+ then CEdimp else 0 end as x035,
002231160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 24
002232160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
002233160824     C+ then CEdimp else 0 end as x024,
002234160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 43
002235160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
002236160824     C+ then CEdimp else 0 end as x043,
002237160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 62
002238160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
002239160824     C+ then CEdimp else 0 end as x062
002240160824     C+ from
002241160824     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
002242160824     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join ecced00f on :anno =
002243160824     C+ cedaas and ftdlnp = cedlnp and ftdnrs = cednrs and ftdnsp = cednsp
002244160824     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:waada and tasmgs
002245160824     C+ between :wmgda and :wmgal) as
002246160824     C+ tabella GROUP BY ftdpdr, ftdddc),
006900160823
006901160823     C+ fifss (
006903160824     C+ pdrf,
006904160824     C+ ddcf,
006906160823     C+ FSS001,
006907160823     C+ FSS005,
006908160823     C+ FSS016,
006909160823     C+ FSS019,
006910160823     C+ FSS034,
006911160823     C+ FSS035,
006912160823     C+ FSS997,
006913160824     C+ FSS998
006928160823     C+ ) as ( select
006929160824     C+ fcePDR, fceDDC, sum(y001), sum(y005), sum(y016) , sum(y019),
006930160824     C+ sum(y034), sum(y035),
006931160824     C+ sum(y997), sum(y998)
006932160824     C+ from ( SELECT fcePDR, fceddc,
006933160824     C+ case when fcetsr = 'R' and fcecce ='001'
006934160824     C+ and taspdr = fcepdr and tasdrt = fceddc
006935160824     C+ then fceice else 0 end as y001,
006936160824     C+ case when fcetsr = 'C' and fcecce ='005'
006937160824     C+ and taspdc = fcepdr and tasddc = fceddc
006938160824     C+ then fceice else 0 end as y005,
006939160824     C+ case when fcetsr = 'C' and fcecce ='016'
006940160824     C+ and taspdc = fcepdr and tasddc = fceddc
006941160824     C+ then fceice else 0 end as y016,
006942160824     C+ case when fcetsr = 'C' and fcecce ='019'
006943160824     C+ and taspdc = fcepdr and tasddc = fceddc
006944160824     C+ then fceice else 0 end as y019,
006945160824     C+ case when fcetsr = 'C'  and fcecce ='034'
006946160824     C+ and taspdc = fcepdr and tasddc = fceddc
006947160824     C+ then fceice else 0 end as y034,
006948160824     C+ case when fcetsr = 'R' and fcecce ='035'
006949160824     C+ and taspdr = fcepdr and tasdrt = fceddc
006950160824     C+ then fceice else 0 end as y035,
006951160824     C+ case when fcetsr = 'C'  and fcecce ='997'
006952160824     C+ and taspdc = fcepdr and tasddc = fceddc
006953160824     C+ then fceice else 0 end as y997,
006954160824     C+ case when fcetsr = 'C'  and fcecce ='998'
006955160824     C+ and taspdc = fcepdr and tasddc = fceddc
006956160824     C+ then fceice else 0 end as y998
006957160824     C+ from
006958160824     C+ titas10f join fifce00f on tasaas = fceaas and taslnp = fcelnp and
006959160824     C+ tasnrs = fcenrs and tasnsp = fcensp
006961160824     C+ WHERE tasaas =:waada and tasmgs
006962160824     C+ between :wmgda and :wmgal) as
006963160824     C+ tabella GROUP BY fcepdr, fceddc)
006964160823
007001160824     C+ SELECT
007002160826     C+ pdr, ddc, b001, b005,
007003160824     C+ pdrf,
007004160824     C+ ddcf,
007005160824     C+ FSS001,
007006160824     C+ FSS005,
007007160824     C+ FSS016,
007008160824     C+ FSS019,
007009160824     C+ FSS034,
007010160824     C+ FSS035,
007011160824     C+ FSS997,
007012160824     C+ FSS998, ifnull(pdrd, 0),
007013160824     C+ ifnull(ddcd, 0),
007014160824     C+ ifnull(b016, 0),
007015160824     C+ ifnull(b019, 0),
007016160824     C+ ifnull(b034, 0),
007017160824     C+ ifnull(b035, 0),
007018160824     C+ ifnull(b024, 0),
007019160824     C+ ifnull(b043, 0),
007020160824     C+ ifnull(b062, 0)
007022160824     C+ from eccet join fifss on pdr=pdrf and ddc=ddcf left outer join
007023160824     C+ ecced on pdr = pdrd   and ddc = ddcd
007500160823     C/END-EXEC
007600160823     C/EXEC SQL
007700160823     C+ OPEN CONTANTE
007800160823     C/END-EXEC
007801160823     c                   do        *hival
007900160823     C/EXEC SQL
008000160826     C+ FETCH NEXT FROM CONTANTE INTO :pdr, :ddc, :b001, :b005,
008102160824     C+ :pdrf,
008103160824     C+ :ddcf,
008105160823     C+ :FSS001,
008106160823     C+ :FSS005,
008107160823     C+ :FSS016,
008108160823     C+ :FSS019,
008109160823     C+ :FSS034,
008110160823     C+ :FSS035,
008111160823     C+ :FSS997,
008112160824     C+ :FSS998,
008113160824     C+ :pdrd, :ddcd, :b016, :b019, :b034, :b035, :b024, :b043, :b062
008200160823     C/END-EXEC
008201160823     c                   if        sqlcod = 100 or sqlcod < 0
008202160823     c                   leave
008203160823     c                   end
008216160824     c                   exsr      srwrite
008217160823     c                   enddo
008300160823     C/EXEC SQL
008400160823     C+ CLOSE CONTANTE
008500160823     C/END-EXEC
008501160824     c                   endsr
008600160824      //-----------------------------------------------------------------------
008700160824     c     srtasp        begsr
008800160824      //-----------------------------------------------------------------------
008801160824     C/EXEC SQL
008802160824     C+ DECLARE CONTANT  CURSOR FOR
008803160824     C+ with
008804160826     C+ eccet (pdr, ddc, b001, b005) AS (
008805160824     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
008806160824     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 and taspdc = ftdpdr
008807160824     C+ and tasddc = ftdddc then CETard else 0 end  as a,
008808160824     C+ case when ftdtsr = 'R' and taspdr = ftdpdr and tasdrt = ftdddc
008809160824     C+ then cetpar else 0 end as p FROM
008810160824     C+ titasp0f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
008811160824     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccetp0f on :anno =
008812160824     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
008813160824     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:waada and tasmgs
008814160824     C+ between :wmgda and :wmgal ) as
008815160824     C+ tabella GROUP BY ftdpdr, ftdddc),
008816160824
008817160824     C+ ecced (pdrd, ddcd, b016, b019, b034, b035, b024, b043, b062) AS (
008818160824     C+ SELECT FTDPDR, FTDDDC, sum(x016) , sum(x019),
008819160824     C+ sum(x034), sum(x035),
008820160824     C+ sum(x024), sum(x043), sum(x062)
008821160824     C+ from ( SELECT FTDPDR, ftdddc,
008822160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 16
008823160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
008824160824     C+ then CEdimp else 0 end as x016,
008825160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 19
008826160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
008827160824     C+ then CEdimp else 0 end as x019,
008828160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 34
008829160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
008830160824     C+ then CEdimp else 0 end as x034,
008831160824     C+ case when ftdtsr = 'R' and cedcmp = 35
008832160824     C+ and taspdr = ftdpdr and tasdrt = ftdddc
008833160824     C+ then CEdimp else 0 end as x035,
008834160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 24
008835160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
008836160824     C+ then CEdimp else 0 end as x024,
008837160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 43
008838160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
008839160824     C+ then CEdimp else 0 end as x043,
008840160824     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 62
008841160824     C+ and taspdc = ftdpdr and tasddc = ftdddc
008842160824     C+ then CEdimp else 0 end as x062
008843160824     C+ from
008844160824     C+ titasp0f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
008845160824     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccedp0f on :anno =
008846160824     C+ cedaas and ftdlnp = cedlnp and ftdnrs = cednrs and ftdnsp = cednsp
008847160824     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:waada and tasmgs
008848160824     C+ between :wmgda and :wmgal) as
008849160824     C+ tabella GROUP BY ftdpdr, ftdddc),
008850160824
008851160824     C+ fifss (
008852160824     C+ pdrf,
008853160824     C+ ddcf,
008854160824     C+ FSS001,
008855160824     C+ FSS005,
008856160824     C+ FSS016,
008857160824     C+ FSS019,
008858160824     C+ FSS034,
008859160824     C+ FSS035,
008860160824     C+ FSS997,
008861160824     C+ FSS998
008862160824     C+ ) as ( select
008863160824     C+ fcePDR, fceDDC, sum(y001), sum(y005), sum(y016) , sum(y019),
008864160824     C+ sum(y034), sum(y035),
008865160824     C+ sum(y997), sum(y998)
008866160824     C+ from ( SELECT fcePDR, fceddc,
008867160824     C+ case when fcetsr = 'R' and fcecce ='001'
008868160824     C+ and taspdr = fcepdr and tasdrt = fceddc
008869160824     C+ then fceice else 0 end as y001,
008870160824     C+ case when fcetsr = 'C' and fcecce ='005'
008871160824     C+ and taspdc = fcepdr and tasddc = fceddc
008872160824     C+ then fceice else 0 end as y005,
008873160824     C+ case when fcetsr = 'C' and fcecce ='016'
008874160824     C+ and taspdc = fcepdr and tasddc = fceddc
008875160824     C+ then fceice else 0 end as y016,
008876160824     C+ case when fcetsr = 'C' and fcecce ='019'
008877160824     C+ and taspdc = fcepdr and tasddc = fceddc
008878160824     C+ then fceice else 0 end as y019,
008879160824     C+ case when fcetsr = 'C'  and fcecce ='034'
008880160824     C+ and taspdc = fcepdr and tasddc = fceddc
008881160824     C+ then fceice else 0 end as y034,
008882160824     C+ case when fcetsr = 'R' and fcecce ='035'
008883160824     C+ and taspdr = fcepdr and tasdrt = fceddc
008884160824     C+ then fceice else 0 end as y035,
008885160824     C+ case when fcetsr = 'C'  and fcecce ='997'
008886160824     C+ and taspdc = fcepdr and tasddc = fceddc
008887160824     C+ then fceice else 0 end as y997,
008888160824     C+ case when fcetsr = 'C'  and fcecce ='998'
008889160824     C+ and taspdc = fcepdr and tasddc = fceddc
008890160824     C+ then fceice else 0 end as y998
008891160824     C+ from
008892160824     C+ titasp0f join fifce00f on tasaas = fceaas and taslnp = fcelnp and
008893160824     C+ tasnrs = fcenrs and tasnsp = fcensp
008894160824     C+ WHERE tasaas =:waada and tasmgs
008895160824     C+ between :wmgda and :wmgal) as
008896160824     C+ tabella GROUP BY fcepdr, fceddc)
008897160824
008898160824     C+ SELECT
008899160826     C+ pdr, ddc, b001, b005,
008900160824     C+ pdrf,
008901160824     C+ ddcf,
008902160824     C+ FSS001,
008903160824     C+ FSS005,
008904160824     C+ FSS016,
008905160824     C+ FSS019,
008906160824     C+ FSS034,
008907160824     C+ FSS035,
008908160824     C+ FSS997,
008909160824     C+ FSS998, ifnull(pdrd, 0),
008910160824     C+ ifnull(ddcd, 0),
008911160824     C+ ifnull(b016, 0),
008912160824     C+ ifnull(b019, 0),
008913160824     C+ ifnull(b034, 0),
008914160824     C+ ifnull(b035, 0),
008915160824     C+ ifnull(b024, 0),
008916160824     C+ ifnull(b043, 0),
008917160824     C+ ifnull(b062, 0)
008918160824     C+ from eccet join fifss on pdr=pdrf and ddc=ddcf left outer join
008919160824     C+ ecced on pdr = pdrd   and ddc = ddcd
008920160824     C/END-EXEC
008921160824     C/EXEC SQL
008922160824     C+ OPEN CONTANT
008923160824     C/END-EXEC
008924160824     c                   do        *hival
008925160824     C/EXEC SQL
008926160826     C+ FETCH NEXT FROM CONTANT  INTO :pdr, :ddc, :b001, :b005,
008927160824     C+ :pdrf,
008928160824     C+ :ddcf,
008929160824     C+ :FSS001,
008930160824     C+ :FSS005,
008931160824     C+ :FSS016,
008932160824     C+ :FSS019,
008933160824     C+ :FSS034,
008934160824     C+ :FSS035,
008935160824     C+ :FSS997,
008936160824     C+ :FSS998,
008937160824     C+ :pdrd, :ddcd, :b016, :b019, :b034, :b035, :b024, :b043, :b062
008938160824     C/END-EXEC
008939160824     c                   if        sqlcod = 100 or sqlcod < 0
008940160824     c                   leave
008941160824     c                   end
008942160824     c                   exsr      srwrite
008943160824     c                   enddo
008944160824     C/EXEC SQL
008945160824     C+ CLOSE CONTANT
008946160824     C/END-EXEC
008947160824     c                   endsr
008948160824      //-----------------------------------------------------------------------
008949160824     c     srwrite       begsr
008950160824      //-----------------------------------------------------------------------
008951160824     c                   eval      fsspdr  = pdrf
008952160824     c                   eval      fssddc  = ddcf
008953160824     c                   movel     fsspdr        fssfgs
008954160824     c                   clear                   FSSxx1
008955160824     c                   clear                   FSSxx2
008956160824     c                   eval      fss001b = b001
008957160824     c                   eval      fss005b = b005
008958160824     c                   eval      fss016b = b016
008959160824     c                   eval      fss019b = b019
008960160824     c                   eval      fss034b = b034
008961160824     c                   eval      fss035b = b035
008962160824     c                   eval      fssxx1b = b024
008963160824     c                   eval      fssxx2b = b043 + b062
008964160824     c                   clear                   FSS998B
008965160824     c                   clear                   FSS997B
008966160824     c                   eval      fssdt1 = anno
008967160824     c                   write     fifss000
009000160824     c                   endsr
009100160824** cmd
009200160824CLRPFM FILE(WFFSS00F)
