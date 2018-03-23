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
001001170224     d £ecrcuds      e ds                  extname(ecrcu00f) prefix(£)
001002170224     d ecrcuds       e ds                  extname(ecrcu00f)
001003170224     d sisdcds       e ds                  extname(sisdc00f)
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
001018170223     d impced          s             13  5
001019170223     d cmpced          s              3  0
001020170223     d mesrcu          s              2
001021170224     D* DS CONTROLLA DATA (8)
001022170224     D WLBDA8          DS
001023170224     D  G08DAT                 1      8  0
001024170224     D  G08INV                 9     16  0
001025170224     D  G08ERR                17     17
001026170224     D  G08TGI                18     22  0
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
018718170224     c                   movel     i17dai        dai               8 0
018719170224     c                   movel     i17daf        daf               8 0
018720170224     c                   move      99            daf
018721170222     c* verifico se è un unificante oppure bollettazione
018722170223     c                   clear                   uni               5 0
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
047118170227         WHERE
047119170227         tasaas*10000+tasmgs >= :DAI and
047120170227         tasaas*10000+tasmgs <= :DAF and
047121170227         tasksc in ( SELECT kuncof FROM seluni)
047125170227         )
047126170227         UNION
047127170227         (SELECT tasaas, taslnp, tasnrs, tasnsp, tasmgs,
047128170227         rtvpes(tasncl,tasncp, taspkc, taspkf) taspkg,
047129170227         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) tasvol, tasncl,
047130170227         tasimv, tastbl
047131170227         FROM titasP0f
047132170227         WHERE
047133170227         tasaas*10000+tasmgs >= :DAI and
047134170227         tasaas*10000+tasmgs <= :DAF and
047135170227         tasksc in ( SELECT kuncof FROM seluni)
047136170227         )),
047137170227
047138170227         selfpe as(
047141170227         SELECT tblkey, tbluni FROM seltas join
047142170227         tabel00f on tblcod= '2L' and substr (tblkey, 1, 3)='S2C' and
047143170227         taspkg > substr(tbluni, 15, 6) and taspkg <=
047145170227         substr(tbluni, 21, 6)),
047146170227
047147170227         seltpo as(
047148170227         SELECT tblkey, tbluni from seltas join
047149170227         tabel00f on tblcod= 'TB' and tblkey=tastbl),
047152170227
047401170223         SELCET as (
047402170224         select anno, mese, lnp, fpe, tpo , count(*) rcunsp,
047403170227         sum(pkg) rcupkg, sum(vol) rcuvol,
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
047416170227         substr(selfpe.tblkey, 4, 2) fpe,
047417170227         substr(seltpo.tbluni, 25, 1) tpo ,
047418170227         seltas.taspkg pkg, seltas.tasvol vol,
047419170227         seltas.tasncl ncl, seltas.tasimv imv,
047420170227         cetpar,
047421170223         cetpam, cetpau, cetpav, cetard, cetarm, cetaru, cetarv, cettrl,
047422170223         cetpag, cetsea, cetsec, cetrcv, cetmde, cettde, cetmaf, cettaf,
047423170223         cetses
047424170227         FROM seltas JOIN selfpe
047425170227         ON seltas.taspkg > substr(selfpe.tbluni, 15, 6) and
047428170227         seltas.taspkg <= substr(selfpe.tbluni, 21, 6)
047429170227         JOIN seltpo ON seltpo.tblkey = seltas.tastbl
047430170227         JOIN eccet00f on cetaas=seltas.tasaas and cetlnp=seltas.taslnp
047431170227         and cetnrs=seltas.tasnrs and cetnsp=seltas.tasnsp and
047432170227         cettbl=seltas.tastbl
047462170227         ) as tabella GROUP BY anno, mese, lnp, fpe, tpo),
047518170223
047519170223         SELCED as (
047520170227         (select seltas.tasaas aastas,
047521170227         substr(digits(seltas.tasmgs), 1, 2) mgstas,
047522170227         seltas.taslnp lnptas, substr(selfpe.tblkey, 4, 2) fpetas,
047523170227         substr(seltpo.tbluni, 25, 1) tpotas, cedcmp,
047524170227         sum(cedimp) impced FROM seltas JOIN selfpe ON
047525170227         seltas.taspkg > substr(selfpe.tbluni, 15, 6)
047526170227         and seltas.taspkg <= substr(selfpe.tbluni, 21, 6)
047527170227         JOIN seltpo ON seltpo.tblkey = seltas.tastbl
047528170227         JOIN ecced00f on cedaas=seltas.tasaas and cedlnp=seltas.taslnp
047529170227         and cednrs=seltas.tasnrs and cednsp=seltas.tasnsp
047530170227         and cedtbl=seltas.tastbl
047531170227         group by
047532170227         seltas.tasaas,
047533170227         substr(digits(seltas.tasmgs), 1, 2),
047534170227         seltas.taslnp, substr(selfpe.tblkey, 4, 2),
047535170227         substr(seltpo.tbluni, 25, 1), cedcmp)
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
048008170223          rcu010, rcu015, rcu009,
048009170223          rcu014, rcu033,
048011170223          cedcmp, impced
048012170227          FROM  selcet LEFT OUTER JOIN selced on
048013170223          anno = aastas and mese=mgstas and lnp=lnptas
048014170223          and fpe=fpetas and tpo=tpotas;
051000110623
051100170223         exec sql open rcu;
051200110623
051300170223         dow not $finercu;
051400170223          exec sql fetch next from rcu into
051401170224          :£rcuaas, :mesrcu, :£rcufpe, :£rcutpo,
051402170224          :£rcunsp, :£rcupkg, :£rcunco, :£rcuvol, :£rcuirb,
051403170224          :£rcu001, :£rcu002, :£rcu003,
051404170224          :£rcu004, :£rcu005, :£rcu006,
051405170224          :£rcu007, :£rcu008,
051406170224          :£rcu013, :£rcu029, :£rcu026,
051407170224          :£rcu025, :£rcu027,
051408170224          :£rcu010, :£rcu015, :£rcu009,
051409170224          :£rcu014, :£rcu033,
051410170223          :cmpced, :impced;
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
059207170224         sum(F) rcuirr, sum(G) rcuinc FROM
059208170224         (SELECT sdcann, sdcmes, sdcfpe, sdctpo,
059209170224         case when sdcrbl in
059210170224         ('N', 'R') then sdcirr else 0 end as F , case when sdcrbl in ('N',
059211170224         'R') then sdcinc else 0 end as G FROM sisdc00f WHERE
059212170224         sdcann*100+sdcmes >= :I17DAI and
059213170224         sdcann*100+sdcmes <= :I17DAF
059214170224         and sdcksc in ( SELECT kuncof FROM tikun00f WHERE
059215170224         kuntle = 'ST' and kunvat = ' ' and kuncop= :i17ksc and :data >=
059216170224         kundde and :data <= kundsc GROUP BY kuncof))
059217170224         as TABELLA GROUP BY sdcann, sdcmes, sdcfpe , sdctpo;
059218170224       EXEC SQL OPEN clienti;
059230170224
059231170224       DOU sqlCode <> 0;
059232170224
059233170224         EXEC SQL
059234170224           FETCH NEXT FROM clienti
059235170224             INTO :sdcann, :sdcmes, :sdcfpe, :sdctpo,
059236170224                  :£rcuirr, :£rcuinc;
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
059422170227         WHERE
059423170227         tasaas*10000+tasmgs >= :DAI and
059424170227         tasaas*10000+tasmgs <= :DAF and
059425170227         tasksc = :i17ksc
059426170227         )
059427170227         UNION
059428170227         (SELECT tasaas, taslnp, tasnrs, tasnsp, tasmgs,
059429170227         rtvpes(tasncl,tasncp, taspkc, taspkf) taspkg,
059430170227         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) tasvol, tasncl,
059431170227         tasimv, tastbl
059432170227         FROM titasP0f
059433170227         WHERE
059434170227         tasaas*10000+tasmgs >= :DAI and
059435170227         tasaas*10000+tasmgs <= :DAF and
059436170227         tasksc = :i17ksc
059437170227         )),
059438170227
059439170227         selfpe as(
059440170227         SELECT tblkey, tbluni FROM seltas join
059441170227         tabel00f on tblcod= '2L' and substr (tblkey, 1, 3)='S2C' and
059442170227         taspkg > substr(tbluni, 15, 6) and taspkg <=
059443170227         substr(tbluni, 21, 6)),
059444170227
059445170227         seltpo as(
059446170227         SELECT tblkey, tbluni from seltas join
059447170227         tabel00f on tblcod= 'TB' and tblkey=tastbl),
059448170227
059449170227         SELCET as (
059450170227         select anno, mese, lnp, fpe, tpo , count(*) rcunsp,
059451170227         sum(pkg) rcupkg, sum(vol) rcuvol,
059452170227         sum(ncl) rcunco, sum(imv) rcuirb,
059453170227         sum(cetpar) rcu001, sum(cetpam) rcu002, sum(cetpau) rcu003,
059454170227         sum(cetpav) rcu004, sum(cetard) rcu005, sum(cetarm) rcu006,
059455170227         sum(cetaru) rcu007, sum(cetarv) rcu008,
059456170227         sum(cettrl) rcu013, sum(cetpag) rcu029, sum(cetsea) rcu026,
059457170227         sum(cetsec) rcu025, sum(cetrcv) rcu027,
059458170227         sum(cetmde) rcu010, sum(cettde) rcu015, sum(cetmaf) rcu009,
059459170227         sum(cettaf) rcu014, sum(cetses) rcu033
059460170227         FROM (SELECT seltas.tasaas anno,
059461170227         substr(digits(seltas.tasmgs), 1, 2) mese , seltas.taslnp lnp,
059462170227         substr(selfpe.tblkey, 4, 2) fpe,
059463170227         substr(seltpo.tbluni, 25, 1) tpo ,
059464170227         seltas.taspkg pkg, seltas.tasvol vol,
059465170227         seltas.tasncl ncl, seltas.tasimv imv,
059466170227         cetpar,
059467170227         cetpam, cetpau, cetpav, cetard, cetarm, cetaru, cetarv, cettrl,
059468170227         cetpag, cetsea, cetsec, cetrcv, cetmde, cettde, cetmaf, cettaf,
059469170227         cetses
059470170227         FROM seltas JOIN selfpe
059471170227         ON seltas.taspkg > substr(selfpe.tbluni, 15, 6) and
059472170227         seltas.taspkg <= substr(selfpe.tbluni, 21, 6)
059473170227         JOIN seltpo ON seltpo.tblkey = seltas.tastbl
059474170227         JOIN eccet00f on cetaas=seltas.tasaas and cetlnp=seltas.taslnp
059475170227         and cetnrs=seltas.tasnrs and cetnsp=seltas.tasnsp and
059476170227         cettbl=seltas.tastbl
059477170227         ) as tabella GROUP BY anno, mese, lnp, fpe, tpo),
059478170227
059479170227         SELCED as (
059480170227         (select seltas.tasaas aastas,
059481170227         substr(digits(seltas.tasmgs), 1, 2) mgstas,
059482170227         seltas.taslnp lnptas, substr(selfpe.tblkey, 4, 2) fpetas,
059483170227         substr(seltpo.tbluni, 25, 1) tpotas, cedcmp,
059484170227         sum(cedimp) impced FROM seltas JOIN selfpe ON
059485170227         seltas.taspkg > substr(selfpe.tbluni, 15, 6)
059486170227         and seltas.taspkg <= substr(selfpe.tbluni, 21, 6)
059487170227         JOIN seltpo ON seltpo.tblkey = seltas.tastbl
059488170227         JOIN ecced00f on cedaas=seltas.tasaas and cedlnp=seltas.taslnp
059489170227         and cednrs=seltas.tasnrs and cednsp=seltas.tasnsp
059490170227         and cedtbl=seltas.tastbl
059491170227         group by
059492170227         seltas.tasaas,
059493170227         substr(digits(seltas.tasmgs), 1, 2),
059494170227         seltas.taslnp, substr(selfpe.tblkey, 4, 2),
059495170227         substr(seltpo.tbluni, 25, 1), cedcmp)
059496170227         )
059497170227
059498170227          SELECT
059499170227          anno, mese, lnp, fpe, tpo,
059500170227          rcunsp, rcupkg, rcuvol, rcunco, rcuirb,
059501170227          rcu001, rcu002, rcu003,
059502170227          rcu004, rcu005, rcu006,
059503170227          rcu007, rcu008,
059504170227          rcu013, rcu029, rcu026,
059505170227          rcu025, rcu027,
059506170227          rcu010, rcu015, rcu009,
059507170227          rcu014, rcu033,
059508170227          cedcmp, impced
059509170227          FROM  selcet LEFT OUTER JOIN selced on
059510170227          anno = aastas and mese=mgstas and lnp=lnptas
059511170227          and fpe=fpetas and tpo=tpotas;
059512170227
059513170227         exec sql open rcuk;
059514170227
059515170227         dow not $finercu;
059516170227          exec sql fetch next from rcuk into
059517170227          :£rcuaas, :mesrcu, :£rcufpe, :£rcutpo,
059518170227          :£rcunsp, :£rcupkg, :£rcunco, :£rcuvol, :£rcuirb,
059519170227          :£rcu001, :£rcu002, :£rcu003,
059520170227          :£rcu004, :£rcu005, :£rcu006,
059521170227          :£rcu007, :£rcu008,
059522170227          :£rcu013, :£rcu029, :£rcu026,
059523170227          :£rcu025, :£rcu027,
059524170227          :£rcu010, :£rcu015, :£rcu009,
059525170227          :£rcu014, :£rcu033,
059526170227          :cmpced, :impced;
059527170227       //?fine file o errore sql esco
059528170227          if sqlcod = 100 or sqlcod < 0;
059529170227           $finercu = *on;
059530170227           leave;
059531170227          endif;
059532170227
059533170227          exsr scrivo;
059534170227
059535170227         enddo;
059536170227       //?chiudo il cursore
059537170227          exec sql close rcuk;
059538170227
059539170227       //?leggo i saldi per reperire le note credito o debito e le
059540170227       //?metto nel record della lnp del cliente unificante
059541170227         exec sql
059542170227         DECLARE clientik NO SCROLL CURSOR FOR
059543170227         SELECT sdcann ,sdcmes, sdcfpe, sdctpo,
059544170227         sum(F) rcuirr, sum(G) rcuinc FROM
059545170227         (SELECT sdcann, sdcmes, sdcfpe, sdctpo,
059546170227         case when sdcrbl in
059547170227         ('N', 'R') then sdcirr else 0 end as F , case when sdcrbl in ('N',
059548170227         'R') then sdcinc else 0 end as G FROM sisdc00f WHERE
059549170227         sdcann*100+sdcmes >= :I17DAI and
059550170227         sdcann*100+sdcmes <= :I17DAF
059551170227         and sdcksc = :i17ksc)
059554170227         as TABELLA GROUP BY sdcann, sdcmes, sdcfpe , sdctpo;
059555170227       EXEC SQL OPEN clientik;
059556170227
059557170227       DOU sqlCode <> 0;
059558170227
059559170227         EXEC SQL
059560170227           FETCH NEXT FROM clientik
059561170227             INTO :sdcann, :sdcmes, :sdcfpe, :sdctpo,
059562170227                  :£rcuirr, :£rcuinc;
059563170227
059564170227         SELECT;
059565170227           WHEN sqlCode = 100;
059566170227             LEAVE;
059567170227           WHEN sqlCode < 0;
059568170227             LEAVE;
059569170227         ENDSL;
059570170227
059571170227       exsr aggio;
059572170227
059573170227       ENDDO;
059574170227
059575170227       EXEC SQL CLOSE clientik;
059585170223
059586170223       endsr;
059587170223      /end-free
059588170223     c**********************************************************************
059589170223     c     scrivo        begsr
059600170223     c**********************************************************************
059601170224     c                   eval      £rcuksc = i17ksc
059602170224     c                   move      mesrcu        £rcumes
059603170223     c     krcu          chain     ecrcu01l
059604170224     c                   eval      ecrcuds = £ecrcuds
059605170223     c                   exsr      srced
059606170223     c                   if        %found(ecrcu01l)
059607170223     c                   update    ecrcu000
059608170223     c                   else
059609170223     c                   write     ecrcu000
059610170223     c                   end
059611170223     c                   endsr
059624170223     c**********************************************************************
059625170223     c     srced         begsr
059626170223     c**********************************************************************
059627170223     c                   do        400           i                 3 0
059628170223     c                   select
059629170223     c                   when      cmpced = 11
059630170223     c                   eval      rcu011=impced
059631170223     c                   when      cmpced = 12
059632170223     c                   eval      rcu012=impced
059633170223     c                   when      cmpced = 16
059634170223     c                   eval      rcu016=impced
059635170223     c                   when      cmpced = 17
059636170223     c                   eval      rcu017=impced
059637170223     c                   when      cmpced = 18
059638170223     c                   eval      rcu018=impced
059639170223     c                   when      cmpced = 19
059640170223     c                   eval      rcu019=impced
059641170223     c                   when      cmpced = 20
059642170223     c                   eval      rcu020=impced
059643170223     c                   when      cmpced = 21
059644170223     c                   eval      rcu021=impced
059645170223     c                   when      cmpced = 22
059646170223     c                   eval      rcu022=impced
059647170223     c                   when      cmpced = 23
059648170223     c                   eval      rcu023=impced
059649170223     c                   when      cmpced = 24
059650170223     c                   eval      rcu024=impced
059651170223     c                   when      cmpced = 28
059652170223     c                   eval      rcu028=impced
059653170223     c                   when      cmpced = 30
059654170223     c                   eval      rcu030=impced
059655170223     c                   when      cmpced = 31
059656170223     c                   eval      rcu030=impced
059657170223     c                   when      cmpced = 32
059658170223     c                   eval      rcu032=impced
059659170223     c                   when      cmpced = 34
059660170223     c                   eval      RCU034=impced
059661170223     c                   when      cmpced = 35
059662170223     c                   eval      RCU035=impced
059663170223     c                   when      cmpced = 37
059664170223     c                   eval      RCU037=impced
059665170223     c                   when      cmpced = 39
059666170223     c                   eval      RCU039=impced
059667170223     c                   when      cmpced = 40
059668170223     c                   eval      RCU040=impced
059669170223     c                   when      cmpced = 41
059670170223     c                   eval      RCU041=impced
059671170223     c                   when      cmpced = 42
059672170223     c                   eval      RCU042=impced
059673170223     c                   when      cmpced = 43
059674170223     c                   eval      RCU043=impced
059675170223     c                   when      cmpced = 44
059676170223     c                   eval      RCU044=impced
059677170223     c                   when      cmpced = 45
059678170223     c                   eval      RCU045=impced
059679170223     c                   when      cmpced = 46
059680170223     c                   eval      RCU046=impced
059681170223     c                   when      cmpced = 47
059682170223     c                   eval      RCU047=impced
059683170223     c                   when      cmpced = 49
059684170223     c                   eval      RCU049=impced
059685170223     c                   when      cmpced = 50
059686170223     c                   eval      RCU050=impced
059687170223     c                   when      cmpced = 51
059688170223     c                   eval      RCU051=impced
059689170223     c                   when      cmpced = 52
059690170223     c                   eval      RCU052=impced
059691170223     c                   when      cmpced = 53
059692170223     c                   eval      RCU053=impced
059693170223     c                   when      cmpced = 54
059694170223     c                   eval      RCU054=impced
059695170223     c                   when      cmpced = 55
059696170223     c                   eval      RCU055=impced
059697170223     c                   when      cmpced = 56
059698170223     c                   eval      RCU056=impced
059699170223     c                   when      cmpced = 57
059700170223     c                   eval      RCU057=impced
059701170223     c                   when      cmpced = 58
059702170223     c                   eval      RCU058=impced
059703170223     c                   when      cmpced = 59
059704170223     c                   eval      RCU059=impced
059705170223     c                   when      cmpced = 63
059706170223     c                   eval      RCU063=impced
059707170223     c                   when      cmpced = 64
059708170223     c                   eval      RCU064=impced
059709170223     c                   when      cmpced = 65
059710170223     c                   eval      RCU065=impced
059711170223     c                   when      cmpced = 66
059712170223     c                   eval      RCU066=impced
059713170223     c                   when      cmpced = 67
059714170223     c                   eval      RCU067=impced
059715170223     c                   when      cmpced = 100
059716170223     c                   eval      RCU100=impced
059717170223     c                   when      cmpced = 101
059718170223     c                   eval      RCU101=impced
059719170223     c                   when      cmpced = 200
059720170223     c                   eval      RCU200=impced
059721170223     c                   when      cmpced = 201
059722170223     c                   eval      RCU201=impced
059723170223     c                   when      cmpced = 204
059724170223     c                   eval      RCU204=impced
059725170223     c                   when      cmpced = 205
059726170223     c                   eval      RCU205=impced
059727170223     c                   when      cmpced = 206
059728170223     c                   eval      RCU206=impced
059729170223     c                   when      cmpced = 207
059730170223     c                   eval      RCU207=impced
059731170223     c                   when      cmpced = 208
059732170223     c                   eval      RCU208=impced
059733170223     c                   when      cmpced = 300
059734170223     c                   eval      RCU300=impced
059735170223     c                   when      cmpced = 301
059736170223     c                   eval      RCU301=impced
059737170223     c                   when      cmpced = 302
059738170223     c                   eval      RCU302=impced
059739170223     c                   when      cmpced = 303
059740170223     c                   eval      RCU303=impced
059741170223     c                   when      cmpced = 304
059742170223     c                   eval      RCU304=impced
059743170223     c                   when      cmpced = 305
059744170223     c                   eval      RCU305=impced
059745170223     c                   when      cmpced = 306
059746170223     c                   eval      RCU306=impced
059747170223     c                   when      cmpced = 307
059748170223     c                   eval      RCU307=impced
059749170223     c                   when      cmpced = 308
059750170223     c                   eval      RCU308=impced
059751170223     c                   when      cmpced = 309
059752170223     c                   eval      RCU309=impced
059753170223     c                   when      cmpced = 900
059754170223     c                   eval      RCU900=impced
059755170223     c                   endsl
059756170223     c                   enddo
059757170223     c                   endsr
059800170224     c**********************************************************************
059900170224     c     aggio         begsr
060000170224     c**********************************************************************
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
060900170224     c                   end
061000170224     c                   endsr
