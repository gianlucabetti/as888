000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200160823     d anno            s              4  0
000300160823     d da              s              4  0
000400160823     d a               s              4  0
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
001700160823     c                   eval      anno = 2016
001800160823     c                   eval      da   = 0201
001900160823     c                   eval      da   = 0229
002000160823     C/EXEC SQL
002100160823     C+ DECLARE CONTANTE CURSOR FOR
002200160823     C+ with
002300160823     C+ eccet (pdr, ddc, b001, b005) AS (
002400160823     C+ SELECT FTDPDR, FTDDDC, sum(p) , sum(a) from ( SELECT FTDPDR,
002500160823     C+ FTDDDC, case when ftdtsr = 'C' and FTDDCM<>0 then CETPAR else 0 end
002600160823     C+ as a , case when ftdtsr = 'R' then cetard else 0 end as p FROM
002700160823     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
002800160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join eccet00f on ftdaas =
002900160823     C+ cetaas and ftdlnp = cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp
003000160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:anno and tasmgs
003100160823     C+ between :da  and :a   and taspdc = ftdpdr and tasddc = ftdddc ) as
003200160823     C+ tabella GROUP BY ftdpdr, ftdddc),
003300160823
003400160823     C+ ecced (pdrd, ddcd, b016, b019, b034, b035, b024, b043, b062) AS (
003500160823     C+ SELECT FTDPDR, FTDDDC, sum(x016) , sum(x019),
003600160823     C+ sum(x034), sum(x035),
003700160823     C+ sum(x024), sum(x043), sum(x062)
003800160823     C+ from ( SELECT FTDPDR, ftdddc,
003900160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 16
004000160823     C+ then CEdimp else 0 end as x016,
004100160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 19
004200160823     C+ then CEdimp else 0 end as x019,
004300160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 34
004400160823     C+ then CEdimp else 0 end as x034,
004500160823     C+ case when ftdtsr = 'R' and cedcmp = 35
004600160823     C+ then CEdimp else 0 end as x035,
004700160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 24
004800160823     C+ then CEdimp else 0 end as x024,
004900160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 43
005000160823     C+ then CEdimp else 0 end as x043,
005100160823     C+ case when ftdtsr = 'C' and FTDDCM<>0 and cedcmp = 62
005200160823     C+ then CEdimp else 0 end as x062
005300160823     C+ from
005400160823     C+ titas10f join fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and
005500160823     C+ tasnrs = ftdnrs and tasnsp = ftdnsp join ecced00f on ftdaas =
005600160823     C+ cedaas and ftdlnp = cedlnp and ftdnrs = cednrs and ftdnsp = cednsp
005700160823     C+ WHERE FTDCMC = ' ' and FTDSIC = ' ' and tasaas =:anno and tasmgs
005800160823     C+ between :da  and :a   and taspdc = ftdpdr and tasddc = ftdddc ) as
005900160823     C+ tabella GROUP BY ftdpdr, ftdddc),
006000160823
006100160823     C+ fifss (fss001) as (
006200160823     C+ select fss001     from ( SELECT ftdpdr, ftdddc FROM titas10f join
006300160823     C+ fiftd00f on tasaas = ftdaas and taslnp = ftdlnp and tasnrs = ftdnrs
006400160823     C+ and tasnsp = ftdnsp join eccet00f on ftdaas = cetaas and ftdlnp =
006500160823     C+ cetlnp and ftdnrs = cetnrs and ftdnsp = cetnsp WHERE FTDCMC = ' '
006600160823     C+ and FTDSIC = ' ' and tasaas = 2016 and tasmgs between 0201 and 0229
006700160823     C+ and taspdc = ftdpdr and tasddc = ftdddc GROUP BY ftdpdr, ftdddc )
006800160823     C+ as tabella join fifss00f on fsspdr = ftdpdr and fssddc = ftdddc)
006900160823
007000160823     C+ SELECT * FROM eccet
007100160823     C+ UNION
007200160823     C+ SELECT * FROM ecced
007300160823     C+ UNION
007400160823     C+ SELECT * FROM fifss
007500160823     C/END-EXEC
007600160823     C/EXEC SQL
007700160823     C+ OPEN CONTANTE
007800160823     C/END-EXEC
007900160823     C/EXEC SQL
008000160823     C+ FETCH NEXT FROM CONTANTE INTO :pdr, :ddc, :b001, :b005, :pdrd,
008100160823     C+ :ddcd, :b016, :b019, :b034, :b035, :b024, :b043, :b062, :fss001
008200160823     C/END-EXEC
008300160823     C/EXEC SQL
008400160823     C+ CLOSE CONTANTE
008500160823     C/END-EXEC
008600160823     c                   seton                                        lr
