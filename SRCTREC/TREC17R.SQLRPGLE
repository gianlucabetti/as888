000100080731     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080731      // ----------------------------------------------------------------------
000300080725      //
000400110617      //         MONITOR PER attività PDA DETTAGLIO  ?
000500080725      //
000600080731      // ----------------------------------------------------------------------
000700080725      // ? DICHIARAZIONE DEI FILE ?
000800080731      // ----------------------------------------------------------------------
000900070209
001000170222     fecrcu01l  uf a e           k disk
001001170228     d £ecrcuds      e ds                  extname(ecrcu00f) prefix(£) inz
001003170228     d ecrcuds       e ds                  extname(ecrcu00f) inz
001004170228     d sisdcds       e ds                  extname(sisdc00f) inz
001005170224     d kpjba         e ds
001006170223     d trec17ds      e ds
001007170309     d i17dtaf                14     19
001008170223     d $finercu        s               n
001009170223     d aastas          s              4  0
001010170223     d mestas          s              2
001011170223     d anno            s              4  0
001012170223     d mese            s              2  0
001013170223     d lnp             s              3  0
001014170307     d lnpsdc          s              3
001015170223     d fpe             s              2
001016170223     d tpo             s              1
001017170223     d lnptas          s              3  0
001018170223     d fpetas          s              2
001019170223     d tpotas          s              1
001020170227     d impced          s             31  5
001021170223     d cmpced          s              3  0
001022170223     d mesrcu          s              2
001023170309     d data            s              8  0
001024170309     d dtaiso          s               d
018500080725     c     *entry        plist
018600080725     c                   parm                    kpjba
018700170222     c                   movel     kpjbu         trec17ds
018710170309     C                   movel     I17dtaf       data
018711170309     C                   move      01            data
018712170309     C                   movel     data          dtaiso
018713170309     c                   adddur    1:*m          dtaiso
018714170309     c                   subdur    1:*d          dtaiso
018715170309     C                   movel     dtaiso        data
018716170223     c     krcu          klist
018717170224     c                   kfld                    £rcuaas
018718170224     c                   kfld                    £rcumes
018719170224     c                   kfld                    £rcuksc
018720170308     c                   kfld                    £rcuuni
018721170224     c                   kfld                    £rculnp
018722170224     c                   kfld                    £rcufpe
018723170224     c                   kfld                    £rcutpo
018730170222     c* verifico se è un unificante oppure bollettazione
018731170223     c                   clear                   uni               5 0
018740170224     C/EXEC SQL
018741170223     C+ SELECT count(*) into :uni FROM tikun00f WHERE kuntle = 'ST' and
018742170223     C+   kunvat = ' '  and kuncop= :i17ksc
018743170223     C+   and :data >= kundde and :data <= kundsc
018744170223     C/END-EXEC
018745170223
018746170223     c* cancello eventuali record già scritti
018747170223     C/EXEC SQL
018748170308     C+ delete from ecrcu00f where rcuksc=:i17ksc and rcuuni=:i17uni and
018749170301     C+  (rcuaas>=:i17annoi and rcumes >= :i17mesei) and
018750170301     C+  (rcuaas<=:i17annof and rcumes <= :i17mesef)
018751170223     C/END-EXEC
019800071217
019900080725      /free
020000080725
020500170307          if uni > 0 and i17uni = 'S';
020700080805          exsr gess01;
020701170223          else;
020702170223          exsr gess02;
020703170223          endif;
021000080729
021001170228     c* cancello eventuali record già scritti
021002170228     C/EXEC SQL
021003170301     C+ DELETE FROM ECRCU00F where
021004170228     C+ RCUNSP = 0 and
021005170228     C+ RCUIRB = 0 and
021006170228     C+ RCUIRR = 0 and
021007170228     C+ RCUINC = 0 and
021008170228     C+ RCU001 = 0 and
021009170228     C+ RCU002 = 0 and
021010170228     C+ RCU003 = 0 and
021011170228     C+ RCU004 = 0 and
021012170228     C+ RCU005 = 0 and
021013170228     C+ RCU006 = 0 and
021014170228     C+ RCU007 = 0 and
021015170228     C+ RCU008 = 0 and
021016170228     C+ RCU009 = 0 and
021017170228     C+ RCU010 = 0 and
021018170228     C+ RCU011 = 0 and
021019170228     C+ RCU012 = 0 and
021020170228     C+ RCU013 = 0 and
021021170228     C+ RCU014 = 0 and
021022170228     C+ RCU015 = 0 and
021023170228     C+ RCU016 = 0 and
021024170228     C+ RCU017 = 0 and
021025170228     C+ RCU018 = 0 and
021026170228     C+ RCU019 = 0 and
021027170228     C+ RCU020 = 0 and
021028170228     C+ RCU021 = 0 and
021029170228     C+ RCU022 = 0 and
021030170228     C+ RCU023 = 0 and
021031170228     C+ RCU024 = 0 and
021032170228     C+ RCU025 = 0 and
021033170228     C+ RCU026 = 0 and
021034170228     C+ RCU027 = 0 and
021035170228     C+ RCU028 = 0 and
021036170228     C+ RCU029 = 0 and
021037170228     C+ RCU030 = 0 and
021038170228     C+ RCU031 = 0 and
021039170228     C+ RCU032 = 0 and
021040170228     C+ RCU033 = 0 and
021041170228     C+ RCU034 = 0 and
021042170228     C+ RCU035 = 0 and
021043170228     C+ RCU037 = 0 and
021044170228     C+ RCU039 = 0 and
021045170228     C+ RCU040 = 0 and
021046170228     C+ RCU041 = 0 and
021047170228     C+ RCU042 = 0 and
021048170228     C+ RCU043 = 0 and
021049170228     C+ RCU044 = 0 and
021050170228     C+ RCU045 = 0 and
021051170228     C+ RCU046 = 0 and
021052170228     C+ RCU047 = 0 and
021053170228     C+ RCU049 = 0 and
021054170228     C+ RCU050 = 0 and
021055170228     C+ RCU051 = 0 and
021056170228     C+ RCU052 = 0 and
021057170228     C+ RCU053 = 0 and
021058170228     C+ RCU054 = 0 and
021059170228     C+ RCU055 = 0 and
021060170228     C+ RCU056 = 0 and
021061170228     C+ RCU057 = 0 and
021062170228     C+ RCU058 = 0 and
021063170228     C+ RCU059 = 0 and
021064170228     C+ RCU063 = 0 and
021065170228     C+ RCU064 = 0 and
021066170228     C+ RCU065 = 0 and
021067170228     C+ RCU066 = 0 and
021068170228     C+ RCU067 = 0 and
021069170228     C+ RCU100 = 0 and
021070170228     C+ RCU101 = 0 and
021071170228     C+ RCU200 = 0 and
021072170228     C+ RCU201 = 0 and
021073170228     C+ RCU204 = 0 and
021074170228     C+ RCU205 = 0 and
021075170228     C+ RCU206 = 0 and
021076170228     C+ RCU207 = 0 and
021077170228     C+ RCU208 = 0 and
021078170228     C+ RCU300 = 0 and
021079170228     C+ RCU301 = 0 and
021080170228     C+ RCU302 = 0 and
021081170228     C+ RCU303 = 0 and
021082170228     C+ RCU304 = 0 and
021083170228     C+ RCU305 = 0 and
021084170228     C+ RCU306 = 0 and
021085170228     C+ RCU307 = 0 and
021086170228     C+ RCU308 = 0 and
021087170228     C+ RCU309 = 0
021111170228     C/END-EXEC
021112080806         clear kpjbu;
021200080729         *inlr = *on;
024400080725
024500080731       // ----------------------------------------------------------------------
024600170224       //?sql x cliente unificanti
024700080731       // ----------------------------------------------------------------------
024800080804       begsr gess01;
024900080731
046800080805         exec sql
046900170222          declare rcu cursor for
047000110629
047100170222         with
047101170227         SELUNI as (
047102170227         SELECT kuncof
047103170227         FROM tikun00f WHERE kuntle = 'ST' and kunvat = ' ' and kuncop=
047104170227         :i17ksc and :data >= kundde and :data <= kundsc GROUP BY
047105170227         kuncof),
047106170227
047107170227         SELTAS as (
047108170227         (SELECT tasaas, taslnp, tasnrs, tasnsp, tasmgs,
047109170227         rtvpes(tasncl,tasncp, taspkc, taspkf) taspkg,
047110170227         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) tasvol, tasncl,
047111170228         tasimv, tastbl,
047112170228         case when   cetpar is null then 0 else   cetpar end cetpar,
047113170228         case when   cetpam is null then 0 else   cetpam end cetpam,
047114170228         case when   cetpau is null then 0 else   cetpau end cetpau,
047115170228         case when   cetpav is null then 0 else   cetpav end cetpav,
047116170228         case when   cetard is null then 0 else   cetard end cetard,
047117170228         case when   cetarm is null then 0 else   cetarm end cetarm,
047118170228         case when   cetaru is null then 0 else   cetaru end cetaru,
047119170228         case when   cetarv is null then 0 else   cetarv end cetarv,
047120170228         case when   cettrl is null then 0 else   cettrl end cettrl,
047121170228         case when   cetpag is null then 0 else   cetpag end cetpag,
047122170228         case when   cetsea is null then 0 else   cetsea end cetsea,
047123170228         case when   cetsec is null then 0 else   cetsec end cetsec,
047124170228         case when   cetrcv is null then 0 else   cetrcv end cetrcv,
047125170228         case when   cetmde is null then 0 else   cetmde end cetmde,
047126170228         case when   cettde is null then 0 else   cettde end cettde,
047127170228         case when   cetmaf is null then 0 else   cetmaf end cetmaf,
047128170228         case when   cettaf is null then 0 else   cettaf end cettaf,
047129170228         case when   cetses is null then 0 else   cetses end cetses
047130170227         FROM titas10f
047131170228         LEFT OUTER JOIN
047132170228         eccet00f   on   cetaas=tasaas and   cetlnp=taslnp
047133170228         and   cetnrs=tasnrs and   cetnsp=tasnsp and
047134170228           cettbl=tastbl
047135170301         WHERE
047137170301         (tasaas>=:i17annoi and substr(digits(tasmgs), 1, 2)>=
047138170301         digits(:i17mesei)) and
047139170301         (tasaas<=:i17annof and substr(digits(tasmgs), 1, 2)<=
047140170301         digits(:i17mesef)) and
047142170228         (tasksc in ( SELECT kuncof FROM seluni) or tasksc = :i17ksc)
047143170227         )
047144170227         UNION
047145170227         (SELECT tasaas, taslnp, tasnrs, tasnsp, tasmgs,
047146170227         rtvpes(tasncl,tasncp, taspkc, taspkf) taspkg,
047147170227         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) tasvol, tasncl,
047148170228         tasimv, tastbl,
047149170228         case when   cetpar is null then 0 else   cetpar end cetpar,
047150170228         case when   cetpam is null then 0 else   cetpam end cetpam,
047151170228         case when   cetpau is null then 0 else   cetpau end cetpau,
047152170228         case when   cetpav is null then 0 else   cetpav end cetpav,
047153170228         case when   cetard is null then 0 else   cetard end cetard,
047154170228         case when   cetarm is null then 0 else   cetarm end cetarm,
047155170228         case when   cetaru is null then 0 else   cetaru end cetaru,
047156170228         case when   cetarv is null then 0 else   cetarv end cetarv,
047157170228         case when   cettrl is null then 0 else   cettrl end cettrl,
047158170228         case when   cetpag is null then 0 else   cetpag end cetpag,
047159170228         case when   cetsea is null then 0 else   cetsea end cetsea,
047160170228         case when   cetsec is null then 0 else   cetsec end cetsec,
047161170228         case when   cetrcv is null then 0 else   cetrcv end cetrcv,
047162170228         case when   cetmde is null then 0 else   cetmde end cetmde,
047163170228         case when   cettde is null then 0 else   cettde end cettde,
047164170228         case when   cetmaf is null then 0 else   cetmaf end cetmaf,
047165170228         case when   cettaf is null then 0 else   cettaf end cettaf,
047166170228         case when   cetses is null then 0 else   cetses end cetses
047167170227         FROM titasP0f
047168170228         LEFT OUTER JOIN
047169170228         eccetp0f   on   cetaas=tasaas and   cetlnp=taslnp
047170170228         and   cetnrs=tasnrs and   cetnsp=tasnsp and
047171170228           cettbl=tastbl
047172170301         WHERE
047173170301         (tasaas>=:i17annoi and substr(digits(tasmgs), 1, 2)>=
047174170301         digits(:i17mesei)) and
047175170301         (tasaas<=:i17annof and substr(digits(tasmgs), 1, 2)<=
047176170301         digits(:i17mesef)) and
047177170228         (tasksc in ( SELECT kuncof FROM seluni) or tasksc = :i17ksc)
047178170227         )),
047518170223
047519170228         SELCET as (
047520170228         select anno, mese, lnp, fpe, tpo ,
047521170228         sum(nsp) rcunsp,
047522170228         sum(pkg) rcupkg, sum(vol) rcuvol,
047523170228         sum(ncl) rcunco, sum(imv) rcuirb,
047524170301         sum(£cetpar) rcu001, sum(£cetpam) rcu002, sum(£cetpau) rcu003,
047525170301         sum(£cetpav) rcu004, sum(£cetard) rcu005, sum(£cetarm) rcu006,
047526170301         sum(£cetaru) rcu007, sum(£cetarv) rcu008,
047527170301         sum(£cettrl) rcu013, sum(£cetpag) rcu029, sum(£cetsea) rcu026,
047528170301         sum(£cetsec) rcu025, sum(£cetrcv) rcu027,
047529170301         sum(£cetmde) rcu010, sum(£cettde) rcu015, sum(£cetmaf) rcu009,
047530170301         sum(£cettaf) rcu014, sum(£cetses) rcu033
047531170228         FROM (SELECT seltas.tasaas anno,
047532170228         substr(digits(seltas.tasmgs), 1, 2) mese , seltas.taslnp lnp,
047533170228         substr(a.tblkey, 4, 2) fpe,
047534170228         substr(b.tbluni, 25, 1) tpo ,
047535170228         case when substr(c.tbluni, 1, 1) = 'S'
047536170228         then 1 else 0 end as nsp,
047537170228         case when substr(c.tbluni, 1, 1) = 'S'
047538170228         then seltas.taspkg else 0 end as pkg,
047539170228         case when substr(c.tbluni, 1, 1) = 'S'
047540170228         then seltas.tasvol else 0 end as vol,
047541170228         case when substr(c.tbluni, 1, 1) = 'S'
047542170228         then seltas.tasncl else 0 end as ncl,
047543170228         case when substr(c.tbluni, 2, 1) = 'S'
047544170228         then seltas.tasimv else 0 end as imv,
047545170301         case when seltas.cetpar is null then 0 else seltas.cetpar end
047546170301         £cetpar,
047547170301         case when seltas.cetpam is null then 0 else seltas.cetpam end
047548170301         £cetpam,
047549170301         case when seltas.cetpau is null then 0 else seltas.cetpau end
047550170301         £cetpau,
047551170301         case when seltas.cetpav is null then 0 else seltas.cetpav end
047552170301         £cetpav,
047553170301         case when seltas.cetard is null then 0 else seltas.cetard end
047554170301         £cetard,
047555170301         case when seltas.cetarm is null then 0 else seltas.cetarm end
047556170301         £cetarm,
047557170301         case when seltas.cetaru is null then 0 else seltas.cetaru end
047558170301         £cetaru,
047559170301         case when seltas.cetarv is null then 0 else seltas.cetarv end
047560170301         £cetarv,
047561170301         case when seltas.cettrl is null then 0 else seltas.cettrl end
047562170301         £cettrl,
047563170301         case when seltas.cetpag is null then 0 else seltas.cetpag end
047564170301         £cetpag,
047565170301         case when seltas.cetsea is null then 0 else seltas.cetsea end
047566170301         £cetsea,
047567170301         case when seltas.cetsec is null then 0 else seltas.cetsec end
047568170301         £cetsec,
047569170301         case when seltas.cetrcv is null then 0 else seltas.cetrcv end
047570170301         £cetrcv,
047571170301         case when seltas.cetmde is null then 0 else seltas.cetmde end
047572170301         £cetmde,
047573170301         case when seltas.cettde is null then 0 else seltas.cettde end
047574170301         £cettde,
047575170301         case when seltas.cetmaf is null then 0 else seltas.cetmaf end
047576170301         £cetmaf,
047577170301         case when seltas.cettaf is null then 0 else seltas.cettaf end
047578170301         £cettaf,
047579170301         case when seltas.cetses is null then 0 else seltas.cetses end
047580170301         £cetses
047588170228         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
047589170228         substr(a.tblkey, 1, 3) = 'S2C' and
047590170228         seltas.taspkg > substr(a.tbluni, 15, 6) and
047591170228         seltas.taspkg <= substr(a.tbluni, 21, 6)
047592170228         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
047593170228         JOIN tabel00f c ON c.tblcod='2W' and c.tblkey =
047594170228         substr(b.tbluni, 26, 1)
047595170228         ) as tabella GROUP BY anno, mese, lnp, fpe, tpo),
047596170228
047597170223         SELCED as (
047598170227         (select seltas.tasaas aastas,
047599170227         substr(digits(seltas.tasmgs), 1, 2) mgstas,
047600170227         seltas.taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
047601170227         substr(b.tbluni, 25, 1) tpotas, cedcmp,
047602170227         sum(cedimp) impced
047603170227         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
047604170227         substr(a.tblkey, 1, 3) = 'S2C' and
047605170227         seltas.taspkg > substr(a.tbluni, 15, 6) and
047606170227         seltas.taspkg <= substr(a.tbluni, 21, 6)
047607170227         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
047608170227         JOIN ecced00f on cedaas=seltas.tasaas and cedlnp=seltas.taslnp
047609170227         and cednrs=seltas.tasnrs and cednsp=seltas.tasnsp
047610170227         and cedtbl=seltas.tastbl
047611170227         group by
047612170227         seltas.tasaas,
047613170227         substr(digits(seltas.tasmgs), 1, 2),
047614170227         seltas.taslnp, substr(a.tblkey, 4, 2),
047615170227         substr(b.tbluni, 25, 1), cedcmp)
047616170228         UNION
047617170228         (select seltas.tasaas aastas,
047618170228         substr(digits(seltas.tasmgs), 1, 2) mgstas,
047619170228         seltas.taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
047620170228         substr(b.tbluni, 25, 1) tpotas, cedcmp,
047621170228         sum(cedimp) impced
047622170228         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
047623170228         substr(a.tblkey, 1, 3) = 'S2C' and
047624170228         seltas.taspkg > substr(a.tbluni, 15, 6) and
047625170228         seltas.taspkg <= substr(a.tbluni, 21, 6)
047626170228         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
047627170228         JOIN eccedp0f on cedaas=seltas.tasaas and cedlnp=seltas.taslnp
047628170228         and cednrs=seltas.tasnrs and cednsp=seltas.tasnsp
047629170228         and cedtbl=seltas.tastbl
047630170228         group by
047631170228         seltas.tasaas,
047632170228         substr(digits(seltas.tasmgs), 1, 2),
047633170228         seltas.taslnp, substr(a.tblkey, 4, 2),
047634170228         substr(b.tbluni, 25, 1), cedcmp)
047917170227         )
047918170223
048000170223          SELECT
048001170223          anno, mese, lnp, fpe, tpo,
048002170224          rcunsp, rcupkg, rcuvol, rcunco, rcuirb,
048003170223          rcu001, rcu002, rcu003,
048004170223          rcu004, rcu005, rcu006,
048005170223          rcu007, rcu008,
048006170223          rcu013, rcu029, rcu026,
048007170223          rcu025, rcu027,
048008170227          rcu010, rcu015, rcu009, rcu014, rcu033,
048009170227          CASE WHEN cedcmp is null THEN 0 ELSE cedcmp end,
048011170227          CASE WHEN impced is null THEN 0 ELSE impced end
048012170227          FROM  selcet LEFT OUTER JOIN selced on
048013170223          anno = aastas and mese=mgstas and lnp=lnptas
048014170223          and fpe=fpetas and tpo=tpotas;
051000110623
051100170223         exec sql open rcu;
051200110623
051300170223         dow not $finercu;
051400170223          exec sql fetch next from rcu into
051401170228          :£rcuaas, :mesrcu, :£rculnp, :£rcufpe, :£rcutpo,
051410170228          :£rcunsp, :£rcupkg, :£rcuvol, :£rcunco, :£rcuirb,
051411170228          :£rcu001, :£rcu002, :£rcu003,
051412170228          :£rcu004, :£rcu005, :£rcu006,
051413170228          :£rcu007, :£rcu008,
051414170228          :£rcu013, :£rcu029, :£rcu026,
051415170228          :£rcu025, :£rcu027,
051416170228          :£rcu010, :£rcu015, :£rcu009,
051417170228          :£rcu014, :£rcu033,
051418170228          :cmpced, :impced;
051800080805       //?fine file o errore sql esco
051900080805          if sqlcod = 100 or sqlcod < 0;
052000170223           $finercu = *on;
052100080806           leave;
052200080805          endif;
052201170223
052202170223          exsr scrivo;
058800080805
058900080805         enddo;
059100080806       //?chiudo il cursore
059200170223          exec sql close rcu;
059201170224
059202170224       //?leggo i saldi per reperire le note credito o debito e le
059203170224       //?metto nel record della lnp del cliente unificante
059204170224         exec sql
059205170224         DECLARE clienti NO SCROLL CURSOR FOR
059206170307         SELECT sdcann ,sdcmes, substr(digits(sdcksc), 1, 3), sdcfpe,
059207170307         sdctpo, sum(F) rcuirr, sum(G) rcuinc FROM
059208170308         (SELECT sdcann, sdcmes, sdcksc, sdcfpe, sdctpo,
059209170224         case when sdcrbl in
059210170224         ('N', 'R') then sdcirr else 0 end as F , case when sdcrbl in ('N',
059211170228         'R') then sdcinc else 0 end as G, sdcirp as H FROM sisdc00f WHERE
059212170301         (sdcann>=:i17annoi and sdcmes>=:i17mesei) and
059213170301         (sdcann<=:i17annof and sdcmes<=:i17mesef) and
059216170301         (sdcksc in ( SELECT kuncof FROM tikun00f WHERE
059217170228         kuntle = 'ST' and kunvat = ' ' and kuncop= :i17ksc and :data >=
059218170228         kundde and :data <= kundsc GROUP BY kuncof) or
059219170228         sdcksc = :i17ksc))
059220170307         as TABELLA GROUP BY sdcann, sdcmes,
059221170307         substr(digits(sdcksc), 1, 3), sdcfpe , sdctpo;
059222170224       EXEC SQL OPEN clienti;
059230170224
059231170224       DOU sqlCode <> 0;
059232170224
059233170224         EXEC SQL
059234170224           FETCH NEXT FROM clienti
059235170307             INTO :sdcann, :sdcmes, :lnpsdc, :sdcfpe, :sdctpo,
059236170228                  :£rcuirr, :£rcuinc;
059239170224
059240170224         SELECT;
059241170224           WHEN sqlCode = 100;
059242170224             LEAVE;
059243170224           WHEN sqlCode < 0;
059244170224             LEAVE;
059245170224         ENDSL;
059246170224
059247170224       exsr aggio;
059377170224
059378170224       ENDDO;
059379170224
059380170224       EXEC SQL CLOSE clienti;
059381080805
059400080805       endsr;
059401170223       // ----------------------------------------------------------------------
059402170224       //?sql x cliente bollettazione
059403170223       // ----------------------------------------------------------------------
059404170223       begsr gess02;
059405170223
059406170223         exec sql
059407170223          declare rcuk cursor for
059408170224
059409170224         with
059415170227
059416170227         SELTAS as (
059417170227         (SELECT tasaas, taslnp, tasnrs, tasnsp, tasmgs,
059418170227         rtvpes(tasncl,tasncp, taspkc, taspkf) taspkg,
059419170227         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) tasvol, tasncl,
059420170228         tasimv, tastbl,
059421170301         case when   cetpar is null then 0 else   cetpar end  cetpar,
059422170301         case when   cetpam is null then 0 else   cetpam end  cetpam,
059423170301         case when   cetpau is null then 0 else   cetpau end  cetpau,
059424170301         case when   cetpav is null then 0 else   cetpav end  cetpav,
059425170301         case when   cetard is null then 0 else   cetard end  cetard,
059426170301         case when   cetarm is null then 0 else   cetarm end  cetarm,
059427170301         case when   cetaru is null then 0 else   cetaru end  cetaru,
059428170301         case when   cetarv is null then 0 else   cetarv end  cetarv,
059429170301         case when   cettrl is null then 0 else   cettrl end  cettrl,
059430170301         case when   cetpag is null then 0 else   cetpag end  cetpag,
059431170301         case when   cetsea is null then 0 else   cetsea end  cetsea,
059432170301         case when   cetsec is null then 0 else   cetsec end  cetsec,
059433170301         case when   cetrcv is null then 0 else   cetrcv end  cetrcv,
059434170301         case when   cetmde is null then 0 else   cetmde end  cetmde,
059435170301         case when   cettde is null then 0 else   cettde end  cettde,
059436170301         case when   cetmaf is null then 0 else   cetmaf end  cetmaf,
059437170301         case when   cettaf is null then 0 else   cettaf end  cettaf,
059438170301         case when   cetses is null then 0 else   cetses end  cetses
059439170228         FROM titas10f
059440170228         LEFT OUTER JOIN
059441170228         eccet00f   on   cetaas=tasaas and   cetlnp=taslnp
059442170228         and   cetnrs=tasnrs and   cetnsp=tasnsp and
059443170228           cettbl=tastbl
059445170301         WHERE
059446170301         (tasaas>=:i17annoi and substr(digits(tasmgs), 1, 2)>=
059447170301         digits(:i17mesei)) and
059448170301         (tasaas<=:i17annof and substr(digits(tasmgs), 1, 2)<=
059449170301         digits(:i17mesef)) and
059453170301         tasksc = :i17ksc
059454170227         )
059455170227         UNION
059456170227         (SELECT tasaas, taslnp, tasnrs, tasnsp, tasmgs,
059457170227         rtvpes(tasncl,tasncp, taspkc, taspkf) taspkg,
059458170227         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) tasvol, tasncl,
059459170228         tasimv, tastbl,
059460170228         case when   cetpar is null then 0 else   cetpar end cetpar,
059461170228         case when   cetpam is null then 0 else   cetpam end cetpam,
059462170228         case when   cetpau is null then 0 else   cetpau end cetpau,
059463170228         case when   cetpav is null then 0 else   cetpav end cetpav,
059464170228         case when   cetard is null then 0 else   cetard end cetard,
059465170228         case when   cetarm is null then 0 else   cetarm end cetarm,
059466170228         case when   cetaru is null then 0 else   cetaru end cetaru,
059467170228         case when   cetarv is null then 0 else   cetarv end cetarv,
059468170228         case when   cettrl is null then 0 else   cettrl end cettrl,
059469170228         case when   cetpag is null then 0 else   cetpag end cetpag,
059470170228         case when   cetsea is null then 0 else   cetsea end cetsea,
059471170228         case when   cetsec is null then 0 else   cetsec end cetsec,
059472170228         case when   cetrcv is null then 0 else   cetrcv end cetrcv,
059473170228         case when   cetmde is null then 0 else   cetmde end cetmde,
059474170228         case when   cettde is null then 0 else   cettde end cettde,
059475170228         case when   cetmaf is null then 0 else   cetmaf end cetmaf,
059476170228         case when   cettaf is null then 0 else   cettaf end cettaf,
059477170228         case when   cetses is null then 0 else   cetses end cetses
059478170228         FROM titasP0f
059479170228         LEFT OUTER JOIN
059480170228         eccetp0f   on   cetaas=tasaas and   cetlnp=taslnp
059481170228         and   cetnrs=tasnrs and   cetnsp=tasnsp and
059482170228         cettbl=tastbl
059483170301         WHERE
059484170301         (tasaas>=:i17annoi and substr(digits(tasmgs), 1, 2)>=
059485170301         digits(:i17mesei)) and
059486170301         (tasaas<=:i17annof and substr(digits(tasmgs), 1, 2)<=
059487170301         digits(:i17mesef)) and
059491170301         tasksc = :i17ksc
059492170227         )),
059493170227
059494170227         SELCET as (
059495170228         select anno, mese, lnp, fpe, tpo ,
059496170228         sum(nsp) rcunsp,
059497170227         sum(pkg) rcupkg, sum(vol) rcuvol,
059498170227         sum(ncl) rcunco, sum(imv) rcuirb,
059499170301         sum(£cetpar) rcu001, sum(£cetpam) rcu002, sum(£cetpau) rcu003,
059500170301         sum(£cetpav) rcu004, sum(£cetard) rcu005, sum(£cetarm) rcu006,
059501170301         sum(£cetaru) rcu007, sum(£cetarv) rcu008,
059502170301         sum(£cettrl) rcu013, sum(£cetpag) rcu029, sum(£cetsea) rcu026,
059503170301         sum(£cetsec) rcu025, sum(£cetrcv) rcu027,
059504170301         sum(£cetmde) rcu010, sum(£cettde) rcu015, sum(£cetmaf) rcu009,
059505170301         sum(£cettaf) rcu014, sum(£cetses) rcu033
059506170227         FROM (SELECT seltas.tasaas anno,
059507170227         substr(digits(seltas.tasmgs), 1, 2) mese , seltas.taslnp lnp,
059508170227         substr(a.tblkey, 4, 2) fpe,
059509170227         substr(b.tbluni, 25, 1) tpo ,
059510170228         case when substr(c.tbluni, 1, 1) = 'S'
059511170228         then 1 else 0 end as nsp,
059512170228         case when substr(c.tbluni, 1, 1) = 'S'
059513170228         then seltas.taspkg else 0 end as pkg,
059514170228         case when substr(c.tbluni, 1, 1) = 'S'
059515170228         then seltas.tasvol else 0 end as vol,
059516170228         case when substr(c.tbluni, 1, 1) = 'S'
059517170228         then seltas.tasncl else 0 end as ncl,
059518170228         case when substr(c.tbluni, 2, 1) = 'S'
059519170228         then seltas.tasimv else 0 end as imv,
059520170301         case when seltas.cetpar is null then 0 else seltas.cetpar end
059521170301         £cetpar,
059522170301         case when seltas.cetpam is null then 0 else seltas.cetpam end
059523170301         £cetpam,
059524170301         case when seltas.cetpau is null then 0 else seltas.cetpau end
059525170301         £cetpau,
059526170301         case when seltas.cetpav is null then 0 else seltas.cetpav end
059527170301         £cetpav,
059528170301         case when seltas.cetard is null then 0 else seltas.cetard end
059529170301         £cetard,
059530170301         case when seltas.cetarm is null then 0 else seltas.cetarm end
059531170301         £cetarm,
059532170301         case when seltas.cetaru is null then 0 else seltas.cetaru end
059533170301         £cetaru,
059534170301         case when seltas.cetarv is null then 0 else seltas.cetarv end
059535170301         £cetarv,
059536170301         case when seltas.cettrl is null then 0 else seltas.cettrl end
059537170301         £cettrl,
059538170301         case when seltas.cetpag is null then 0 else seltas.cetpag end
059539170301         £cetpag,
059540170301         case when seltas.cetsea is null then 0 else seltas.cetsea end
059541170301         £cetsea,
059542170301         case when seltas.cetsec is null then 0 else seltas.cetsec end
059543170301         £cetsec,
059544170301         case when seltas.cetrcv is null then 0 else seltas.cetrcv end
059545170301         £cetrcv,
059546170301         case when seltas.cetmde is null then 0 else seltas.cetmde end
059547170301         £cetmde,
059548170301         case when seltas.cettde is null then 0 else seltas.cettde end
059549170301         £cettde,
059550170301         case when seltas.cetmaf is null then 0 else seltas.cetmaf end
059551170301         £cetmaf,
059552170301         case when seltas.cettaf is null then 0 else seltas.cettaf end
059553170301         £cettaf,
059554170301         case when seltas.cetses is null then 0 else seltas.cetses end
059555170301         £cetses
059565170227         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
059566170227         substr(a.tblkey, 1, 3) = 'S2C' and
059567170227         seltas.taspkg > substr(a.tbluni, 15, 6) and
059568170227         seltas.taspkg <= substr(a.tbluni, 21, 6)
059569170227         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
059570170228         JOIN tabel00f c ON c.tblcod='2W' and c.tblkey =
059571170228         substr(b.tbluni, 26, 1) LEFT OUTER
059572170227         JOIN eccet00f on cetaas=seltas.tasaas and cetlnp=seltas.taslnp
059573170227         and cetnrs=seltas.tasnrs and cetnsp=seltas.tasnsp and
059574170227         cettbl=seltas.tastbl
059575170227         ) as tabella GROUP BY anno, mese, lnp, fpe, tpo),
059576170227
059577170227         SELCED as (
059578170227         (select seltas.tasaas aastas,
059579170227         substr(digits(seltas.tasmgs), 1, 2) mgstas,
059580170227         seltas.taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
059581170227         substr(b.tbluni, 25, 1) tpotas, cedcmp,
059582170227         sum(cedimp) impced
059583170227         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
059584170227         substr(a.tblkey, 1, 3) = 'S2C' and
059585170227         seltas.taspkg > substr(a.tbluni, 15, 6) and
059586170227         seltas.taspkg <= substr(a.tbluni, 21, 6)
059587170227         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
059588170227         JOIN ecced00f on cedaas=seltas.tasaas and cedlnp=seltas.taslnp
059589170227         and cednrs=seltas.tasnrs and cednsp=seltas.tasnsp
059590170227         and cedtbl=seltas.tastbl
059591170227         group by
059592170227         seltas.tasaas,
059593170227         substr(digits(seltas.tasmgs), 1, 2),
059594170227         seltas.taslnp, substr(a.tblkey, 4, 2),
059595170227         substr(b.tbluni, 25, 1), cedcmp)
059596170227         )
059597170227
059598170227          SELECT
059599170227          anno, mese, lnp, fpe, tpo,
059600170227          rcunsp, rcupkg, rcuvol, rcunco, rcuirb,
059601170227          rcu001, rcu002, rcu003,
059602170227          rcu004, rcu005, rcu006,
059603170227          rcu007, rcu008,
059604170227          rcu013, rcu029, rcu026,
059605170227          rcu025, rcu027,
059606170227          rcu010, rcu015, rcu009,
059607170227          rcu014, rcu033,
059608170227          CASE WHEN cedcmp is null THEN 0 ELSE cedcmp end,
059609170227          CASE WHEN impced is null THEN 0 ELSE impced end
059610170227          FROM  selcet LEFT OUTER JOIN selced on
059611170227          anno = aastas and mese=mgstas and lnp=lnptas
059612170227          and fpe=fpetas and tpo=tpotas;
059613170227
059614170227         exec sql open rcuk;
059615170227
059616170227         dow not $finercu;
059617170227          exec sql fetch next from rcuk into
059618170228          :£rcuaas, :mesrcu, :£rculnp, :£rcufpe, :£rcutpo,
059619170228          :£rcunsp, :£rcupkg, :£rcuvol, :£rcunco, :£rcuirb,
059620170228          :£rcu001, :£rcu002, :£rcu003,
059621170228          :£rcu004, :£rcu005, :£rcu006,
059622170228          :£rcu007, :£rcu008,
059623170228          :£rcu013, :£rcu029, :£rcu026,
059624170228          :£rcu025, :£rcu027,
059625170228          :£rcu010, :£rcu015, :£rcu009,
059626170228          :£rcu014, :£rcu033,
059627170227          :cmpced, :impced;
059628170227       //?fine file o errore sql esco
059629170227          if sqlcod = 100 or sqlcod < 0;
059630170227           $finercu = *on;
059631170227           leave;
059632170227          endif;
059633170227
059634170227          exsr scrivo;
059635170227
059636170227         enddo;
059637170227       //?chiudo il cursore
059638170227          exec sql close rcuk;
059639170227
059640170227       //?leggo i saldi per reperire le note credito o debito e le
059641170227       //?metto nel record della lnp del cliente unificante
059642170227         exec sql
059643170227         DECLARE clientik NO SCROLL CURSOR FOR
059647170307         SELECT sdcann ,sdcmes, substr(digits(sdcksc), 1, 3), sdcfpe,
059648170307         sdctpo, sum(F) rcuirr, sum(G) rcuinc FROM
059649170308         (SELECT sdcann, sdcmes, sdcksc, sdcfpe, sdctpo,
059650170227         case when sdcrbl in
059651170227         ('N', 'R') then sdcirr else 0 end as F , case when sdcrbl in ('N',
059652170228         'R') then sdcinc else 0 end as G, sdcirp as H FROM sisdc00f WHERE
059653170301         sdcann >= :I17annoi and sdcmes >= :i17mesei and
059654170309         sdcann <= :I17annof and sdcmes <= :i17mesef and
059655170301         sdcksc = :i17ksc)
059656170307         as TABELLA GROUP BY sdcann, sdcmes,
059657170307         substr(digits(sdcksc), 1, 3), sdcfpe , sdctpo;
059658170227       EXEC SQL OPEN clientik;
059659170227
059660170227       DOU sqlCode <> 0;
059661170227
059662170227         EXEC SQL
059663170227           FETCH NEXT FROM clientik
059664170307             INTO :sdcann, :sdcmes, :lnpsdc, :sdcfpe, :sdctpo,
059665170228                  :£rcuirr, :£rcuinc;
059666170227
059667170227         SELECT;
059668170227           WHEN sqlCode = 100;
059669170227             LEAVE;
059670170227           WHEN sqlCode < 0;
059671170227             LEAVE;
059672170227         ENDSL;
059673170227
059674170227       exsr aggio;
059675170227
059676170227       ENDDO;
059677170227
059678170227       EXEC SQL CLOSE clientik;
059679170223
059680170223       endsr;
059681170223      /end-free
059682170223     c**********************************************************************
059683170223     c     scrivo        begsr
059684170223     c**********************************************************************
059685170224     c                   eval      £rcuksc = i17ksc
059686170308     c                   eval      £rcuuni = i17uni
059687170224     c                   move      mesrcu        £rcumes
059688170227     c     krcu          chain     ecrcu01l
059689170223     c                   if        %found(ecrcu01l)
059690170227     c                   exsr      srced
059691170223     c                   update    ecrcu000
059692170223     c                   else
059693170228     c                   clear                   ecrcuds
059694170227     c                   eval      ecrcuds = £ecrcuds
059695170227     c                   exsr      srced
059696170223     c                   write     ecrcu000
059697170223     c                   end
059698170223     c                   endsr
059699170223     c**********************************************************************
059700170223     c     srced         begsr
059701170223     c**********************************************************************
059702170223     c                   select
059703170223     c                   when      cmpced = 11
059704170223     c                   eval      rcu011=impced
059705170223     c                   when      cmpced = 12
059706170223     c                   eval      rcu012=impced
059707170223     c                   when      cmpced = 16
059708170223     c                   eval      rcu016=impced
059709170223     c                   when      cmpced = 17
059710170223     c                   eval      rcu017=impced
059711170223     c                   when      cmpced = 18
059712170223     c                   eval      rcu018=impced
059713170223     c                   when      cmpced = 19
059714170223     c                   eval      rcu019=impced
059715170223     c                   when      cmpced = 20
059716170223     c                   eval      rcu020=impced
059717170223     c                   when      cmpced = 21
059718170223     c                   eval      rcu021=impced
059719170223     c                   when      cmpced = 22
059720170223     c                   eval      rcu022=impced
059721170223     c                   when      cmpced = 23
059722170223     c                   eval      rcu023=impced
059723170223     c                   when      cmpced = 24
059724170223     c                   eval      rcu024=impced
059725170223     c                   when      cmpced = 28
059726170223     c                   eval      rcu028=impced
059727170223     c                   when      cmpced = 30
059728170223     c                   eval      rcu030=impced
059729170223     c                   when      cmpced = 31
059730170223     c                   eval      rcu030=impced
059731170223     c                   when      cmpced = 32
059732170223     c                   eval      rcu032=impced
059733170223     c                   when      cmpced = 34
059734170223     c                   eval      RCU034=impced
059735170223     c                   when      cmpced = 35
059736170223     c                   eval      RCU035=impced
059737170223     c                   when      cmpced = 37
059738170223     c                   eval      RCU037=impced
059739170223     c                   when      cmpced = 39
059740170223     c                   eval      RCU039=impced
059741170223     c                   when      cmpced = 40
059742170223     c                   eval      RCU040=impced
059743170223     c                   when      cmpced = 41
059744170223     c                   eval      RCU041=impced
059745170223     c                   when      cmpced = 42
059746170223     c                   eval      RCU042=impced
059747170223     c                   when      cmpced = 43
059748170223     c                   eval      RCU043=impced
059749170223     c                   when      cmpced = 44
059750170223     c                   eval      RCU044=impced
059751170223     c                   when      cmpced = 45
059752170223     c                   eval      RCU045=impced
059753170223     c                   when      cmpced = 46
059754170223     c                   eval      RCU046=impced
059755170223     c                   when      cmpced = 47
059756170223     c                   eval      RCU047=impced
059757170223     c                   when      cmpced = 49
059758170223     c                   eval      RCU049=impced
059759170223     c                   when      cmpced = 50
059760170223     c                   eval      RCU050=impced
059761170223     c                   when      cmpced = 51
059762170223     c                   eval      RCU051=impced
059763170223     c                   when      cmpced = 52
059764170223     c                   eval      RCU052=impced
059765170223     c                   when      cmpced = 53
059766170223     c                   eval      RCU053=impced
059767170223     c                   when      cmpced = 54
059768170223     c                   eval      RCU054=impced
059769170223     c                   when      cmpced = 55
059770170223     c                   eval      RCU055=impced
059771170223     c                   when      cmpced = 56
059772170223     c                   eval      RCU056=impced
059773170223     c                   when      cmpced = 57
059774170223     c                   eval      RCU057=impced
059775170223     c                   when      cmpced = 58
059776170223     c                   eval      RCU058=impced
059777170223     c                   when      cmpced = 59
059778170223     c                   eval      RCU059=impced
059779170223     c                   when      cmpced = 63
059780170223     c                   eval      RCU063=impced
059781170223     c                   when      cmpced = 64
059782170223     c                   eval      RCU064=impced
059783170223     c                   when      cmpced = 65
059784170223     c                   eval      RCU065=impced
059785170223     c                   when      cmpced = 66
059786170223     c                   eval      RCU066=impced
059787170223     c                   when      cmpced = 67
059788170223     c                   eval      RCU067=impced
059789170223     c                   when      cmpced = 100
059790170223     c                   eval      RCU100=impced
059791170223     c                   when      cmpced = 101
059792170223     c                   eval      RCU101=impced
059793170223     c                   when      cmpced = 200
059794170223     c                   eval      RCU200=impced
059795170223     c                   when      cmpced = 201
059796170223     c                   eval      RCU201=impced
059797170223     c                   when      cmpced = 204
059798170223     c                   eval      RCU204=impced
059799170223     c                   when      cmpced = 205
059800170223     c                   eval      RCU205=impced
059801170223     c                   when      cmpced = 206
059802170223     c                   eval      RCU206=impced
059803170223     c                   when      cmpced = 207
059804170223     c                   eval      RCU207=impced
059805170223     c                   when      cmpced = 208
059806170223     c                   eval      RCU208=impced
059807170223     c                   when      cmpced = 300
059808170223     c                   eval      RCU300=impced
059809170223     c                   when      cmpced = 301
059810170223     c                   eval      RCU301=impced
059811170223     c                   when      cmpced = 302
059812170223     c                   eval      RCU302=impced
059813170223     c                   when      cmpced = 303
059814170223     c                   eval      RCU303=impced
059815170223     c                   when      cmpced = 304
059816170223     c                   eval      RCU304=impced
059817170223     c                   when      cmpced = 305
059818170223     c                   eval      RCU305=impced
059819170223     c                   when      cmpced = 306
059820170223     c                   eval      RCU306=impced
059821170223     c                   when      cmpced = 307
059822170223     c                   eval      RCU307=impced
059823170223     c                   when      cmpced = 308
059824170223     c                   eval      RCU308=impced
059825170223     c                   when      cmpced = 309
059826170223     c                   eval      RCU309=impced
059827170223     c                   endsl
059828170223     c                   endsr
059829170224     c**********************************************************************
059900170224     c     aggio         begsr
060000170224     c**********************************************************************
060001170228     c                   if        £rcuirr <> 0 or £rcuinc <> 0
060100170224     c                   eval      £rcuksc = i17ksc
060200170224     c                   eval      £rcuaas = sdcann
060201170224     c                   eval      £rcumes = sdcmes
060202170224     c                   eval      £rcutpo = sdctpo
060203170224     c                   eval      £rcufpe = sdcfpe
060204170308     c                   eval      £rcuuni = i17uni
060205170307     c                   movel     lnpsdc        £rculnp
060206170224     c* aggiorno solo se trovo il record
060300170224     c     krcu          chain     ecrcu01l
060500170224     c                   if        %found(ecrcu01l)
060501170224     c                   eval      rcuirr = £rcuirr
060502170224     c                   eval      rcuinc = £rcuinc
060600170224     c                   update    ecrcu000
060601170228     c                   else
060602170228     c                   clear                   ecrcuds
060603170309     c                   eval      rcuuni = i17uni
060604170228     c                   eval      rcuksc = i17ksc
060605170228     c                   eval      rcuaas = sdcann
060606170228     c                   eval      rcumes = sdcmes
060607170228     c                   eval      rcutpo = sdctpo
060608170228     c                   eval      rcufpe = sdcfpe
060609170307     c                   movel     lnpsdc        rculnp
060610170228     c                   eval      rcuirr = £rcuirr
060611170228     c                   eval      rcuinc = £rcuinc
060612170228     c                   write     ecrcu000
060900170224     c                   end
060901170228     c                   end
061000170224     c                   endsr
