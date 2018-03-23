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
001002170228     d ecrcuds       e ds                  extname(ecrcu00f) inz
001003170228     d sisdcds       e ds                  extname(sisdc00f) inz
001004170224     d kpjba         e ds
001005170223     d trec17ds      e ds
001006170223     d $finercu        s               n
001008170223     d aastas          s              4  0
001009170223     d mestas          s              2
001010170223     d anno            s              4  0
001011170223     d mese            s              2  0
001012170223     d lnp             s              3  0
001013170223     d fpe             s              2
001014170223     d tpo             s              1
001015170223     d lnptas          s              3  0
001016170223     d fpetas          s              2
001017170223     d tpotas          s              1
001018170227     d impced          s             31  5
001019170223     d cmpced          s              3  0
001020170223     d mesrcu          s              2
001045170227     D*    CONTROLLA DATA (8)
001046170224     D WLBDA8          DS
001047170224     D  G08DAT                 1      8  0
001048170224     D  G08INV                 9     16  0
001049170224     D  G08ERR                17     17
001050170224     D  G08TGI                18     22  0
018500080725     c     *entry        plist
018600080725     c                   parm                    kpjba
018700170222     c                   movel     kpjbu         trec17ds
018702170223     C* REPERISCE LA DATA CORRENTE
018703170223     C                   TIME                    N14              14 0
018704170224     C                   MOVE      N14           n8                8 0
018705170224     C                   Z-ADD     n8            G08DAT
018706170224     C                   Z-ADD     *ZEROS        G08INV
018707170224     C                   MOVEL     '0'           G08ERR
018708170224     C                   CALL      'XSRDA8'
018709170224     C                   PARM                    WLBDA8
018710170224     C                   Z-ADD     G08INV        data              8 0
018711170223     c     krcu          klist
018712170224     c                   kfld                    £rcuaas
018713170224     c                   kfld                    £rcumes
018714170224     c                   kfld                    £rcuksc
018715170224     c                   kfld                    £rculnp
018716170224     c                   kfld                    £rcufpe
018717170224     c                   kfld                    £rcutpo
018719170228     c                   movel     i17dai        annoi             4 0
018720170228     c                   move      i17dai        mesex             2 0
018721170228     c                   movel     mesex         mesei             4 0
018722170228     c                   move      01            mesei
018723170228     c                   move      i17daf        mesex             2 0
018724170228     c                   movel     mesex         mesef             4 0
018725170228     c                   move      31            mesef
018726170228     c                   movel     i17dai        dai               8 0
018727170228     c                   move      00            dai
018728170228     c                   movel     i17daf        daf               8 0
018729170228     c                   move      99            daf
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
018748170223     C+ delete from ecrcu00f where rcuksc=:i17ksc and
018749170223     C+  rcuaas*100+rcumes >= :I17DAI and
018750170223     C+  rcuaas*100+rcumes <= :I17DAF
018751170223     C/END-EXEC
019800071217
019900080725      /free
020000080725
020500170223          if uni > 0;
020700080805          exsr gess01;
020701170223          else;
020702170223          exsr gess02;
020703170223          endif;
021000080729
021100080806         clear kpjbu;
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
047111170227         tasimv, tastbl
047112170227         FROM titas10f
047119170228         WHERE tasaas = :annoi and tasmgs between :mesei and :mesef and
047120170227       //tasaas*10000+tasmgs >= :DAI and
047121170227       //tasaas*10000+tasmgs <= :DAF and
047122170228         (tasksc in ( SELECT kuncof FROM seluni) or tasksc = :i17ksc)
047125170227         )
047126170227         UNION
047127170227         (SELECT tasaas, taslnp, tasnrs, tasnsp, tasmgs,
047128170227         rtvpes(tasncl,tasncp, taspkc, taspkf) taspkg,
047129170227         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) tasvol, tasncl,
047130170227         tasimv, tastbl
047131170227         FROM titasP0f
047132170228         WHERE tasaas = :annoi and tasmgs between :mesei and :mesef and
047133170227       //tasaas*10000+tasmgs >= :DAI and
047134170227       //tasaas*10000+tasmgs <= :DAF and
047136170228         (tasksc in ( SELECT kuncof FROM seluni) or tasksc = :i17ksc)
047137170227         )),
047152170227
047401170223         SELCET as (
047402170228         select anno, mese, lnp, fpe, tpo ,
047403170228         sum(nsp) rcunsp,
047404170228         sum(pkg) rcupkg, sum(vol) rcuvol,
047405170227         sum(ncl) rcunco, sum(imv) rcuirb,
047407170223         sum(cetpar) rcu001, sum(cetpam) rcu002, sum(cetpau) rcu003,
047408170223         sum(cetpav) rcu004, sum(cetard) rcu005, sum(cetarm) rcu006,
047409170223         sum(cetaru) rcu007, sum(cetarv) rcu008,
047410170223         sum(cettrl) rcu013, sum(cetpag) rcu029, sum(cetsea) rcu026,
047411170223         sum(cetsec) rcu025, sum(cetrcv) rcu027,
047412170223         sum(cetmde) rcu010, sum(cettde) rcu015, sum(cetmaf) rcu009,
047413170223         sum(cettaf) rcu014, sum(cetses) rcu033
047414170227         FROM (SELECT seltas.tasaas anno,
047415170227         substr(digits(seltas.tasmgs), 1, 2) mese , seltas.taslnp lnp,
047416170227         substr(a.tblkey, 4, 2) fpe,
047417170227         substr(b.tbluni, 25, 1) tpo ,
047418170228         case when substr(c.tbluni, 1, 1) = 'S'
047419170228         then 1 else 0 end as nsp,
047420170228         case when substr(c.tbluni, 1, 1) = 'S'
047421170228         then seltas.taspkg else 0 end as pkg,
047422170228         case when substr(c.tbluni, 1, 1) = 'S'
047423170228         then seltas.tasvol else 0 end as vol,
047424170228         case when substr(c.tbluni, 1, 1) = 'S'
047425170228         then seltas.tasncl else 0 end as ncl,
047426170228         case when substr(c.tbluni, 2, 1) = 'S'
047427170228         then seltas.tasimv else 0 end as imv,
047428170228         case when   cetpar is null then 0 else   cetpar end cetpar,
047430170228         case when   cetpam is null then 0 else   cetpam end cetpam,
047431170228         case when   cetpau is null then 0 else   cetpau end cetpau,
047432170228         case when   cetpav is null then 0 else   cetpav end cetpav,
047433170228         case when   cetard is null then 0 else   cetard end cetard,
047434170228         case when   cetarm is null then 0 else   cetarm end cetarm,
047435170228         case when   cetaru is null then 0 else   cetaru end cetaru,
047436170228         case when   cetarv is null then 0 else   cetarv end cetarv,
047437170228         case when   cettrl is null then 0 else   cettrl end cettrl,
047438170228         case when   cetpag is null then 0 else   cetpag end cetpag,
047439170228         case when   cetsea is null then 0 else   cetsea end cetsea,
047440170228         case when   cetsec is null then 0 else   cetsec end cetsec,
047441170228         case when   cetrcv is null then 0 else   cetrcv end cetrcv,
047442170228         case when   cetmde is null then 0 else   cetmde end cetmde,
047443170228         case when   cettde is null then 0 else   cettde end cettde,
047444170228         case when   cetmaf is null then 0 else   cetmaf end cetmaf,
047445170228         case when   cettaf is null then 0 else   cettaf end cettaf,
047446170228         case when   cetses is null then 0 else   cetses end cetses
047468170228         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
047469170227         substr(a.tblkey, 1, 3) = 'S2C' and
047470170227         seltas.taspkg > substr(a.tbluni, 15, 6) and
047471170227         seltas.taspkg <= substr(a.tbluni, 21, 6)
047472170227         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
047473170228         JOIN tabel00f c ON c.tblcod='2W' and c.tblkey =
047474170228         substr(b.tbluni, 26, 1)
047475170228         LEFT OUTER JOIN
047476170228         eccet00f   on   cetaas=seltas.tasaas and   cetlnp=seltas.taslnp
047477170228         and   cetnrs=seltas.tasnrs and   cetnsp=seltas.tasnsp and
047478170228           cettbl=seltas.tastbl
047483170227         ) as tabella GROUP BY anno, mese, lnp, fpe, tpo),
047518170223
047519170223         SELCED as (
047520170227         (select seltas.tasaas aastas,
047521170227         substr(digits(seltas.tasmgs), 1, 2) mgstas,
047522170227         seltas.taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
047523170227         substr(b.tbluni, 25, 1) tpotas, cedcmp,
047524170227         sum(cedimp) impced
047525170227         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
047526170227         substr(a.tblkey, 1, 3) = 'S2C' and
047527170227         seltas.taspkg > substr(a.tbluni, 15, 6) and
047528170227         seltas.taspkg <= substr(a.tbluni, 21, 6)
047529170227         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
047533170227         JOIN ecced00f on cedaas=seltas.tasaas and cedlnp=seltas.taslnp
047534170227         and cednrs=seltas.tasnrs and cednsp=seltas.tasnsp
047535170227         and cedtbl=seltas.tastbl
047536170227         group by
047537170227         seltas.tasaas,
047538170227         substr(digits(seltas.tasmgs), 1, 2),
047539170227         seltas.taslnp, substr(a.tblkey, 4, 2),
047540170227         substr(b.tbluni, 25, 1), cedcmp)
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
059206170224         SELECT sdcann ,sdcmes, sdcfpe, sdctpo,
059207170228         sum(F) rcuirr, sum(G) rcuinc FROM
059208170224         (SELECT sdcann, sdcmes, sdcfpe, sdctpo,
059209170224         case when sdcrbl in
059210170224         ('N', 'R') then sdcirr else 0 end as F , case when sdcrbl in ('N',
059211170228         'R') then sdcinc else 0 end as G, sdcirp as H FROM sisdc00f WHERE
059212170224         sdcann*100+sdcmes >= :I17DAI and
059213170224         sdcann*100+sdcmes <= :I17DAF
059214170228         and (sdcksc in ( SELECT kuncof FROM tikun00f WHERE
059215170228         kuntle = 'ST' and kunvat = ' ' and kuncop= :i17ksc and :data >=
059216170228         kundde and :data <= kundsc GROUP BY kuncof) or
059217170228         sdcksc = :i17ksc))
059218170224         as TABELLA GROUP BY sdcann, sdcmes, sdcfpe , sdctpo;
059219170224       EXEC SQL OPEN clienti;
059230170224
059231170224       DOU sqlCode <> 0;
059232170224
059233170224         EXEC SQL
059234170224           FETCH NEXT FROM clienti
059235170224             INTO :sdcann, :sdcmes, :sdcfpe, :sdctpo,
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
059420170227         tasimv, tastbl
059421170227         FROM titas10f
059423170228         WHERE tasaas = :annoi and tasmgs between :mesei and :mesef and
059424170227       //tasaas*10000+tasmgs >= :DAI and
059425170227       //tasaas*10000+tasmgs <= :DAF and
059426170227         tasksc = :i17ksc
059427170227         )
059428170227         UNION
059429170227         (SELECT tasaas, taslnp, tasnrs, tasnsp, tasmgs,
059430170227         rtvpes(tasncl,tasncp, taspkc, taspkf) taspkg,
059431170227         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) tasvol, tasncl,
059432170227         tasimv, tastbl
059433170227         FROM titasP0f
059434170228         WHERE tasaas = :annoi and tasmgs between :mesei and :mesef and
059435170227       //tasaas*10000+tasmgs >= :DAI and
059436170227       //tasaas*10000+tasmgs <= :DAF and
059437170227         tasksc = :i17ksc
059438170227         )),
059448170227
059449170227         SELCET as (
059450170228         select anno, mese, lnp, fpe, tpo ,
059451170228         sum(nsp) rcunsp,
059452170227         sum(pkg) rcupkg, sum(vol) rcuvol,
059453170227         sum(ncl) rcunco, sum(imv) rcuirb,
059454170227         sum(cetpar) rcu001, sum(cetpam) rcu002, sum(cetpau) rcu003,
059455170227         sum(cetpav) rcu004, sum(cetard) rcu005, sum(cetarm) rcu006,
059456170227         sum(cetaru) rcu007, sum(cetarv) rcu008,
059457170227         sum(cettrl) rcu013, sum(cetpag) rcu029, sum(cetsea) rcu026,
059458170227         sum(cetsec) rcu025, sum(cetrcv) rcu027,
059459170227         sum(cetmde) rcu010, sum(cettde) rcu015, sum(cetmaf) rcu009,
059460170227         sum(cettaf) rcu014, sum(cetses) rcu033
059461170227         FROM (SELECT seltas.tasaas anno,
059462170227         substr(digits(seltas.tasmgs), 1, 2) mese , seltas.taslnp lnp,
059463170227         substr(a.tblkey, 4, 2) fpe,
059464170227         substr(b.tbluni, 25, 1) tpo ,
059473170228         case when substr(c.tbluni, 1, 1) = 'S'
059474170228         then 1 else 0 end as nsp,
059475170228         case when substr(c.tbluni, 1, 1) = 'S'
059476170228         then seltas.taspkg else 0 end as pkg,
059477170228         case when substr(c.tbluni, 1, 1) = 'S'
059478170228         then seltas.tasvol else 0 end as vol,
059479170228         case when substr(c.tbluni, 1, 1) = 'S'
059480170228         then seltas.tasncl else 0 end as ncl,
059481170228         case when substr(c.tbluni, 2, 1) = 'S'
059482170228         then seltas.tasimv else 0 end as imv,
059483170228         case when cetpar is null then 0 else cetpar end cetpar,
059484170228         case when cetpam is null then 0 else cetpam end cetpam,
059485170228         case when cetpau is null then 0 else cetpau end cetpau,
059486170228         case when cetpav is null then 0 else cetpav end cetpav,
059487170228         case when cetard is null then 0 else cetard end cetard,
059488170228         case when cetarm is null then 0 else cetarm end cetarm,
059489170228         case when cetaru is null then 0 else cetaru end cetaru,
059490170228         case when cetarv is null then 0 else cetarv end cetarv,
059491170228         case when cettrl is null then 0 else cettrl end cettrl,
059492170228         case when cetpag is null then 0 else cetpag end cetpag,
059493170228         case when cetsea is null then 0 else cetsea end cetsea,
059494170228         case when cetsec is null then 0 else cetsec end cetsec,
059495170228         case when cetrcv is null then 0 else cetrcv end cetrcv,
059496170228         case when cetmde is null then 0 else cetmde end cetmde,
059497170228         case when cettde is null then 0 else cettde end cettde,
059498170228         case when cetmaf is null then 0 else cetmaf end cetmaf,
059499170228         case when cettaf is null then 0 else cettaf end cettaf,
059500170228         case when cetses is null then 0 else cetses end cetses
059504170227         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
059505170227         substr(a.tblkey, 1, 3) = 'S2C' and
059506170227         seltas.taspkg > substr(a.tbluni, 15, 6) and
059507170227         seltas.taspkg <= substr(a.tbluni, 21, 6)
059508170227         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
059509170228         JOIN tabel00f c ON c.tblcod='2W' and c.tblkey =
059510170228         substr(b.tbluni, 26, 1) LEFT OUTER
059511170227         JOIN eccet00f on cetaas=seltas.tasaas and cetlnp=seltas.taslnp
059512170227         and cetnrs=seltas.tasnrs and cetnsp=seltas.tasnsp and
059513170227         cettbl=seltas.tastbl
059514170227         ) as tabella GROUP BY anno, mese, lnp, fpe, tpo),
059515170227
059516170227         SELCED as (
059517170227         (select seltas.tasaas aastas,
059518170227         substr(digits(seltas.tasmgs), 1, 2) mgstas,
059519170227         seltas.taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
059520170227         substr(b.tbluni, 25, 1) tpotas, cedcmp,
059521170227         sum(cedimp) impced
059522170227         FROM seltas JOIN tabel00f a on a.tblcod = '2L' and
059523170227         substr(a.tblkey, 1, 3) = 'S2C' and
059524170227         seltas.taspkg > substr(a.tbluni, 15, 6) and
059525170227         seltas.taspkg <= substr(a.tbluni, 21, 6)
059526170227         JOIN tabel00f b ON b.tblcod='TB' and b.tblkey = seltas.tastbl
059527170227         JOIN ecced00f on cedaas=seltas.tasaas and cedlnp=seltas.taslnp
059528170227         and cednrs=seltas.tasnrs and cednsp=seltas.tasnsp
059529170227         and cedtbl=seltas.tastbl
059530170227         group by
059531170227         seltas.tasaas,
059532170227         substr(digits(seltas.tasmgs), 1, 2),
059533170227         seltas.taslnp, substr(a.tblkey, 4, 2),
059534170227         substr(b.tbluni, 25, 1), cedcmp)
059535170227         )
059536170227
059537170227          SELECT
059538170227          anno, mese, lnp, fpe, tpo,
059539170227          rcunsp, rcupkg, rcuvol, rcunco, rcuirb,
059540170227          rcu001, rcu002, rcu003,
059541170227          rcu004, rcu005, rcu006,
059542170227          rcu007, rcu008,
059543170227          rcu013, rcu029, rcu026,
059544170227          rcu025, rcu027,
059545170227          rcu010, rcu015, rcu009,
059546170227          rcu014, rcu033,
059547170227          CASE WHEN cedcmp is null THEN 0 ELSE cedcmp end,
059548170227          CASE WHEN impced is null THEN 0 ELSE impced end
059549170227          FROM  selcet LEFT OUTER JOIN selced on
059550170227          anno = aastas and mese=mgstas and lnp=lnptas
059551170227          and fpe=fpetas and tpo=tpotas;
059552170227
059553170227         exec sql open rcuk;
059554170227
059555170227         dow not $finercu;
059556170227          exec sql fetch next from rcuk into
059557170228          :£rcuaas, :mesrcu, :£rculnp, :£rcufpe, :£rcutpo,
059558170228          :£rcunsp, :£rcupkg, :£rcuvol, :£rcunco, :£rcuirb,
059559170228          :£rcu001, :£rcu002, :£rcu003,
059560170228          :£rcu004, :£rcu005, :£rcu006,
059561170228          :£rcu007, :£rcu008,
059562170228          :£rcu013, :£rcu029, :£rcu026,
059563170228          :£rcu025, :£rcu027,
059564170228          :£rcu010, :£rcu015, :£rcu009,
059565170228          :£rcu014, :£rcu033,
059566170227          :cmpced, :impced;
059567170227       //?fine file o errore sql esco
059568170227          if sqlcod = 100 or sqlcod < 0;
059569170227           $finercu = *on;
059570170227           leave;
059571170227          endif;
059572170227
059573170227          exsr scrivo;
059574170227
059575170227         enddo;
059576170227       //?chiudo il cursore
059577170227          exec sql close rcuk;
059578170227
059579170227       //?leggo i saldi per reperire le note credito o debito e le
059580170227       //?metto nel record della lnp del cliente unificante
059581170227         exec sql
059582170227         DECLARE clientik NO SCROLL CURSOR FOR
059583170227         SELECT sdcann ,sdcmes, sdcfpe, sdctpo,
059584170228         sum(F) rcuirr, sum(G) rcuinc FROM
059585170227         (SELECT sdcann, sdcmes, sdcfpe, sdctpo,
059586170227         case when sdcrbl in
059587170227         ('N', 'R') then sdcirr else 0 end as F , case when sdcrbl in ('N',
059588170228         'R') then sdcinc else 0 end as G, sdcirp as H FROM sisdc00f WHERE
059589170227         sdcann*100+sdcmes >= :I17DAI and
059590170227         sdcann*100+sdcmes <= :I17DAF
059591170227         and sdcksc = :i17ksc)
059592170227         as TABELLA GROUP BY sdcann, sdcmes, sdcfpe , sdctpo;
059593170227       EXEC SQL OPEN clientik;
059594170227
059595170227       DOU sqlCode <> 0;
059596170227
059597170227         EXEC SQL
059598170227           FETCH NEXT FROM clientik
059599170227             INTO :sdcann, :sdcmes, :sdcfpe, :sdctpo,
059600170228                  :£rcuirr, :£rcuinc;
059601170227
059602170227         SELECT;
059603170227           WHEN sqlCode = 100;
059604170227             LEAVE;
059605170227           WHEN sqlCode < 0;
059606170227             LEAVE;
059607170227         ENDSL;
059608170227
059609170227       exsr aggio;
059610170227
059611170227       ENDDO;
059612170227
059613170227       EXEC SQL CLOSE clientik;
059614170223
059615170223       endsr;
059616170223      /end-free
059617170223     c**********************************************************************
059618170223     c     scrivo        begsr
059619170223     c**********************************************************************
059620170224     c                   eval      £rcuksc = i17ksc
059621170224     c                   move      mesrcu        £rcumes
059622170227     c     krcu          chain     ecrcu01l
059623170223     c                   if        %found(ecrcu01l)
059624170227     c                   exsr      srced
059625170223     c                   update    ecrcu000
059626170223     c                   else
059627170228     c                   clear                   ecrcuds
059628170227     c                   eval      ecrcuds = £ecrcuds
059629170227     c                   exsr      srced
059630170223     c                   write     ecrcu000
059631170223     c                   end
059632170223     c                   endsr
059633170223     c**********************************************************************
059634170223     c     srced         begsr
059635170223     c**********************************************************************
059636170223     c                   select
059637170223     c                   when      cmpced = 11
059638170223     c                   eval      rcu011=impced
059639170223     c                   when      cmpced = 12
059640170223     c                   eval      rcu012=impced
059641170223     c                   when      cmpced = 16
059642170223     c                   eval      rcu016=impced
059643170223     c                   when      cmpced = 17
059644170223     c                   eval      rcu017=impced
059645170223     c                   when      cmpced = 18
059646170223     c                   eval      rcu018=impced
059647170223     c                   when      cmpced = 19
059648170223     c                   eval      rcu019=impced
059649170223     c                   when      cmpced = 20
059650170223     c                   eval      rcu020=impced
059651170223     c                   when      cmpced = 21
059652170223     c                   eval      rcu021=impced
059653170223     c                   when      cmpced = 22
059654170223     c                   eval      rcu022=impced
059655170223     c                   when      cmpced = 23
059656170223     c                   eval      rcu023=impced
059657170223     c                   when      cmpced = 24
059658170223     c                   eval      rcu024=impced
059659170223     c                   when      cmpced = 28
059660170223     c                   eval      rcu028=impced
059661170223     c                   when      cmpced = 30
059662170223     c                   eval      rcu030=impced
059663170223     c                   when      cmpced = 31
059664170223     c                   eval      rcu030=impced
059665170223     c                   when      cmpced = 32
059666170223     c                   eval      rcu032=impced
059667170223     c                   when      cmpced = 34
059668170223     c                   eval      RCU034=impced
059669170223     c                   when      cmpced = 35
059670170223     c                   eval      RCU035=impced
059671170223     c                   when      cmpced = 37
059672170223     c                   eval      RCU037=impced
059673170223     c                   when      cmpced = 39
059674170223     c                   eval      RCU039=impced
059675170223     c                   when      cmpced = 40
059676170223     c                   eval      RCU040=impced
059677170223     c                   when      cmpced = 41
059678170223     c                   eval      RCU041=impced
059679170223     c                   when      cmpced = 42
059680170223     c                   eval      RCU042=impced
059681170223     c                   when      cmpced = 43
059682170223     c                   eval      RCU043=impced
059683170223     c                   when      cmpced = 44
059684170223     c                   eval      RCU044=impced
059685170223     c                   when      cmpced = 45
059686170223     c                   eval      RCU045=impced
059687170223     c                   when      cmpced = 46
059688170223     c                   eval      RCU046=impced
059689170223     c                   when      cmpced = 47
059690170223     c                   eval      RCU047=impced
059691170223     c                   when      cmpced = 49
059692170223     c                   eval      RCU049=impced
059693170223     c                   when      cmpced = 50
059694170223     c                   eval      RCU050=impced
059695170223     c                   when      cmpced = 51
059696170223     c                   eval      RCU051=impced
059697170223     c                   when      cmpced = 52
059698170223     c                   eval      RCU052=impced
059699170223     c                   when      cmpced = 53
059700170223     c                   eval      RCU053=impced
059701170223     c                   when      cmpced = 54
059702170223     c                   eval      RCU054=impced
059703170223     c                   when      cmpced = 55
059704170223     c                   eval      RCU055=impced
059705170223     c                   when      cmpced = 56
059706170223     c                   eval      RCU056=impced
059707170223     c                   when      cmpced = 57
059708170223     c                   eval      RCU057=impced
059709170223     c                   when      cmpced = 58
059710170223     c                   eval      RCU058=impced
059711170223     c                   when      cmpced = 59
059712170223     c                   eval      RCU059=impced
059713170223     c                   when      cmpced = 63
059714170223     c                   eval      RCU063=impced
059715170223     c                   when      cmpced = 64
059716170223     c                   eval      RCU064=impced
059717170223     c                   when      cmpced = 65
059718170223     c                   eval      RCU065=impced
059719170223     c                   when      cmpced = 66
059720170223     c                   eval      RCU066=impced
059721170223     c                   when      cmpced = 67
059722170223     c                   eval      RCU067=impced
059723170223     c                   when      cmpced = 100
059724170223     c                   eval      RCU100=impced
059725170223     c                   when      cmpced = 101
059726170223     c                   eval      RCU101=impced
059727170223     c                   when      cmpced = 200
059728170223     c                   eval      RCU200=impced
059729170223     c                   when      cmpced = 201
059730170223     c                   eval      RCU201=impced
059731170223     c                   when      cmpced = 204
059732170223     c                   eval      RCU204=impced
059733170223     c                   when      cmpced = 205
059734170223     c                   eval      RCU205=impced
059735170223     c                   when      cmpced = 206
059736170223     c                   eval      RCU206=impced
059737170223     c                   when      cmpced = 207
059738170223     c                   eval      RCU207=impced
059739170223     c                   when      cmpced = 208
059740170223     c                   eval      RCU208=impced
059741170223     c                   when      cmpced = 300
059742170223     c                   eval      RCU300=impced
059743170223     c                   when      cmpced = 301
059744170223     c                   eval      RCU301=impced
059745170223     c                   when      cmpced = 302
059746170223     c                   eval      RCU302=impced
059747170223     c                   when      cmpced = 303
059748170223     c                   eval      RCU303=impced
059749170223     c                   when      cmpced = 304
059750170223     c                   eval      RCU304=impced
059751170223     c                   when      cmpced = 305
059752170223     c                   eval      RCU305=impced
059753170223     c                   when      cmpced = 306
059754170223     c                   eval      RCU306=impced
059755170223     c                   when      cmpced = 307
059756170223     c                   eval      RCU307=impced
059757170223     c                   when      cmpced = 308
059758170223     c                   eval      RCU308=impced
059759170223     c                   when      cmpced = 309
059760170223     c                   eval      RCU309=impced
059761170223     c                   endsl
059762170223     c                   endsr
059800170224     c**********************************************************************
059900170224     c     aggio         begsr
060000170224     c**********************************************************************
060001170228     c                   if        £rcuirr <> 0 or £rcuinc <> 0
060100170224     c                   eval      £rcuksc = i17ksc
060200170224     c                   eval      £rcuaas = sdcann
060201170224     c                   eval      £rcumes = sdcmes
060202170224     c                   eval      £rcutpo = sdctpo
060203170224     c                   eval      £rcufpe = sdcfpe
060204170224     c                   movel     i17ksc        £rculnp
060205170224     c* aggiorno solo se trovo il record
060300170224     c     krcu          chain     ecrcu01l
060500170224     c                   if        %found(ecrcu01l)
060501170224     c                   eval      rcuirr = £rcuirr
060502170224     c                   eval      rcuinc = £rcuinc
060600170224     c                   update    ecrcu000
060601170228     c                   else
060602170228     c                   clear                   ecrcuds
060603170228     c                   eval      rcuksc = i17ksc
060604170228     c                   eval      rcuaas = sdcann
060605170228     c                   eval      rcumes = sdcmes
060606170228     c                   eval      rcutpo = sdctpo
060607170228     c                   eval      rcufpe = sdcfpe
060608170228     c                   movel     i17ksc        rculnp
060609170228     c                   eval      rcuirr = £rcuirr
060610170228     c                   eval      rcuinc = £rcuinc
060611170228     c                   write     ecrcu000
060900170224     c                   end
060901170228     c                   end
061000170224     c                   endsr
