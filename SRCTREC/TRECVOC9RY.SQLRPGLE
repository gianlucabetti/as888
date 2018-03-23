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
000500160823     d pdr             s                   like(fsspdr)
000600160823     d ddc             s                   like(fssddc)
000601160823     d pdrd            s                   like(fsspdr)
000602160823     d ddcd            s                   like(fssddc)
000700160823     d b001            s                   like(fss001b)
000800160823     d b005            s                   like(fss005b)
000900160823     d b016            s                   like(fss016b)
001000160823     d b019            s                   like(fss016b)
001100160823     d b024            s                   like(fss016b)
001200160823     d b034            s                   like(fss016b)
001300160823     d b035            s                   like(fss016b)
001400160823     d b043            s                   like(fss016b)
001500160823     d b062            s                   like(fss016b)
001600160823     d fifssds       e ds                  extname(fifss00f)
001601160823      //-----------------------------------------------------------------------
001602160823      //? STRUTTURE DATI
001603160823      //-----------------------------------------------------------------------
001604160823     d param           ds
001605160823     d  waada                  1      4  0
001606160823     d  wmgda                  5      8  0
001607160823     d  wmmda                  5      6  0
001608160823     d  wggda                  7      8  0
001609160823     d  wdatada                1      8  0
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
002300160823     C+ eccet (pdr, ddc, b001, b005) AS (
002400160823     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
002500160823     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 and taspdc = ftdpdr
002501160823     C+ and tasddc = ftdddc then CETPAR else 0 end  as a,
002601160823     C+ case when ftdtsr = 'R' and taspdr = ftdpdr and tasdrt = ftdddc
002602160823     C+ then cetard else 0 end as p FROM
002700160823     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
002800160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on :anno =
002900160823     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
003000160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:waada and tasmgs
003100160823     C+ between :wmgda and :wmgal ) as
003200160823     C+ tabella GROUP BY ftdpdr, ftdddc),
003300160823
003400160823     C+ ecced (pdrd, ddcd, b016, b019, b034, b035, b024, b043, b062) AS (
003500160823     C+ SELECT FTDPDR, FTDDDC, sum(x016) , sum(x019),
003600160823     C+ sum(x034), sum(x035),
003700160823     C+ sum(x024), sum(x043), sum(x062)
003800160823     C+ from ( SELECT FTDPDR, ftdddc,
003900160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 16
003901160823     C+ and taspdc = ftdpdr and tasddc = ftdddc
004000160823     C+ then CEdimp else 0 end as x016,
004100160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 19
004101160823     C+ and taspdc = ftdpdr and tasddc = ftdddc
004200160823     C+ then CEdimp else 0 end as x019,
004300160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 34
004301160823     C+ and taspdc = ftdpdr and tasddc = ftdddc
004400160823     C+ then CEdimp else 0 end as x034,
004500160823     C+ case when ftdtsr = 'R' and cedcmp = 35
004501160823     C+ and taspdr = ftdpdr and tasdrt = ftdddc
004600160823     C+ then CEdimp else 0 end as x035,
004700160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 24
004701160823     C+ and taspdc = ftdpdr and tasddc = ftdddc
004800160823     C+ then CEdimp else 0 end as x024,
004900160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 43
004901160823     C+ and taspdc = ftdpdr and tasddc = ftdddc
005000160823     C+ then CEdimp else 0 end as x043,
005100160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 62
005101160823     C+ and taspdc = ftdpdr and tasddc = ftdddc
005200160823     C+ then CEdimp else 0 end as x062
005300160823     C+ from
005400160823     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
005500160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join ecced00f on :anno =
005600160823     C+ cedaas and ftdlnp = cedlnp and ftdnrs = cednrs and ftdnsp = cednsp
005700160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:waada and tasmgs
005800160823     C+ between :wmgda and :wmgal) as
005900160823     C+ tabella GROUP BY ftdpdr, ftdddc),
006900160823
006901160823     C+ fifss (
006902160823     C+ FSSFGS,
006903160823     C+ FSSDDC,
006904160823     C+ FSSPDR,
006905160823     C+ FSSPDD,
006906160823     C+ FSS001,
006907160823     C+ FSS005,
006908160823     C+ FSS016,
006909160823     C+ FSS019,
006910160823     C+ FSS034,
006911160823     C+ FSS035,
006912160823     C+ FSS997,
006913160823     C+ FSS998,
006914160823     C+ FSSXX1,
006915160823     C+ FSSXX2
006928160823     C+ ) as ( select
006929160823     C+ FSSFGS,
006930160823     C+ FSSDDC,
006931160823     C+ FSSPDR,
006932160823     C+ FSSPDD,
006933160823     C+ FSS001,
006934160823     C+ FSS005,
006935160823     C+ FSS016,
006936160823     C+ FSS019,
006937160823     C+ FSS034,
006938160823     C+ FSS035,
006939160823     C+ FSS997,
006940160823     C+ FSS998,
006941160823     C+ FSSXX1,
006942160823     C+ FSSXX2
006943160823     C+ from ( SELECT ftdpdr, ftdddc FROM titas10f join
006944160823     C+ fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and tasnrs = ftdnrs
006945160823     C+ and tasnsp = ftdnsp
006946160823     C+ WHERE FTDCMC = ' '
006947160823     C+ and FTDSIC = ' ' and tasaas=:waada and tasmgs between :wmgda and
006948160823     C+ :wmgal
006949160823     C+ GROUP BY ftdpdr, ftdddc )
006950160823     C+ as tabella join fifss00f on fsspdr = ftdpdr and fssddc = ftdddc)
006951160823
007001160823     C+ SELECT * FROM eccet join ecced on pdr=pdrd and ddc=ddcd
007002160823     C+ join fifss on pdr = fsspdr and ddc = fssddc
007500160823     C/END-EXEC
007600160823     C/EXEC SQL
007700160823     C+ OPEN CONTANTE
007800160823     C/END-EXEC
007801160823     c                   do        *hival
007900160823     C/EXEC SQL
008000160823     C+ FETCH NEXT FROM CONTANTE INTO :pdr, :ddc, :b001, :b005, :pdrd,
008100160823     C+ :ddcd, :b016, :b019, :b034, :b035, :b024, :b043, :b062,
008101160823     C+ :FSSFGS,
008102160823     C+ :FSSDDC,
008103160823     C+ :FSSPDR,
008104160823     C+ :FSSPDD,
008105160823     C+ :FSS001,
008106160823     C+ :FSS005,
008107160823     C+ :FSS016,
008108160823     C+ :FSS019,
008109160823     C+ :FSS034,
008110160823     C+ :FSS035,
008111160823     C+ :FSS997,
008112160823     C+ :FSS998,
008113160823     C+ :FSSXX1,
008114160823     C+ :FSSXX2
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
008804160824     C+ eccet (pdr, ddc, b001, b005) AS (
008805160824     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
008806160824     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 and taspdc = ftdpdr
008807160824     C+ and tasddc = ftdddc then CETPAR else 0 end  as a,
008808160824     C+ case when ftdtsr = 'R' and taspdr = ftdpdr and tasdrt = ftdddc
008809160824     C+ then cetard else 0 end as p FROM
008810160824     C+ titasp0f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
008811160824     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on :anno =
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
008845160824     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join ecced00f on :anno =
008846160824     C+ cedaas and ftdlnp = cedlnp and ftdnrs = cednrs and ftdnsp = cednsp
008847160824     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:waada and tasmgs
008848160824     C+ between :wmgda and :wmgal) as
008849160824     C+ tabella GROUP BY ftdpdr, ftdddc),
008850160824
008851160824     C+ fifss (
008852160824     C+ FSSFGS,
008853160824     C+ FSSDDC,
008854160824     C+ FSSPDR,
008855160824     C+ FSSPDD,
008856160824     C+ FSS001,
008857160824     C+ FSS005,
008858160824     C+ FSS016,
008859160824     C+ FSS019,
008860160824     C+ FSS034,
008861160824     C+ FSS035,
008862160824     C+ FSS997,
008863160824     C+ FSS998,
008864160824     C+ FSSXX1,
008865160824     C+ FSSXX2
008866160824     C+ ) as ( select
008867160824     C+ FSSFGS,
008868160824     C+ FSSDDC,
008869160824     C+ FSSPDR,
008870160824     C+ FSSPDD,
008871160824     C+ FSS001,
008872160824     C+ FSS005,
008873160824     C+ FSS016,
008874160824     C+ FSS019,
008875160824     C+ FSS034,
008876160824     C+ FSS035,
008877160824     C+ FSS997,
008878160824     C+ FSS998,
008879160824     C+ FSSXX1,
008880160824     C+ FSSXX2
008881160824     C+ from ( SELECT ftdpdr, ftdddc FROM titasp0f join
008882160824     C+ fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and tasnrs = ftdnrs
008883160824     C+ and tasnsp = ftdnsp
008884160824     C+ WHERE FTDCMC = ' '
008885160824     C+ and FTDSIC = ' ' and tasaas=:waada and tasmgs between :wmgda and
008886160824     C+ :wmgal
008887160824     C+ GROUP BY ftdpdr, ftdddc )
008888160824     C+ as tabella join fifss00f on fsspdr = ftdpdr and fssddc = ftdddc)
008889160824
008890160824     C+ SELECT * FROM eccet join ecced on pdr=pdrd and ddc=ddcd
008891160824     C+ join fifss on pdr = fsspdr and ddc = fssddc
008892160824     C/END-EXEC
008893160824     C/EXEC SQL
008894160824     C+ OPEN CONTANT
008895160824     C/END-EXEC
008896160824     c                   do        *hival
008897160824     C/EXEC SQL
008898160824     C+ FETCH NEXT FROM CONTANT  INTO :pdr, :ddc, :b001, :b005, :pdrd,
008899160824     C+ :ddcd, :b016, :b019, :b034, :b035, :b024, :b043, :b062,
008900160824     C+ :FSSFGS,
008901160824     C+ :FSSDDC,
008902160824     C+ :FSSPDR,
008903160824     C+ :FSSPDD,
008904160824     C+ :FSS001,
008905160824     C+ :FSS005,
008906160824     C+ :FSS016,
008907160824     C+ :FSS019,
008908160824     C+ :FSS034,
008909160824     C+ :FSS035,
008910160824     C+ :FSS997,
008911160824     C+ :FSS998,
008912160824     C+ :FSSXX1,
008913160824     C+ :FSSXX2
008914160824     C/END-EXEC
008915160824     c                   if        sqlcod = 100 or sqlcod < 0
008916160824     c                   leave
008917160824     c                   end
008918160824     c                   exsr      srwrite
008930160824     c                   enddo
008931160824     C/EXEC SQL
008932160824     C+ CLOSE CONTANT
008933160824     C/END-EXEC
008934160824     c                   endsr
008935160824      //-----------------------------------------------------------------------
008936160824     c     srwrite       begsr
008937160824      //-----------------------------------------------------------------------
008938160824     c                   eval      fss001b = b001
008939160824     c                   eval      fss005b = b005
008940160824     c                   eval      fss016b = b016
008941160824     c                   eval      fss019b = b019
008942160824     c                   eval      fss034b = b034
008943160824     c                   eval      fss035b = b035
008944160824     c                   eval      fssxx1b = b024
008945160824     c                   eval      fssxx2b = b043 + b062
008946160824     c                   clear                   FSS998B
008947160824     c                   clear                   FSS997B
008948160824     c                   eval      fssdt1 = anno
008949160824     c                   write     fifss000
009000160824     c                   endsr
009100160824** cmd
009200160824CLRPFM FILE(WFFSS00F)
