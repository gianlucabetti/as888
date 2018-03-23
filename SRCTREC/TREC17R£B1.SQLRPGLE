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
047401170223         SELCET as (
047402170224         select anno, mese, lnp, fpe, tpo , count(*) rcunsp,
047403170224         sum(pkg) rcupkg, sum(vol) rcuvol,
047405170224         sum(tasncl) rcunco, sum(tasimv) rcuirb,
047407170223         sum(cetpar) rcu001, sum(cetpam) rcu002, sum(cetpau) rcu003,
047408170223         sum(cetpav) rcu004, sum(cetard) rcu005, sum(cetarm) rcu006,
047409170223         sum(cetaru) rcu007, sum(cetarv) rcu008,
047410170223         sum(cettrl) rcu013, sum(cetpag) rcu029, sum(cetsea) rcu026,
047411170223         sum(cetsec) rcu025, sum(cetrcv) rcu027,
047412170223         sum(cetmde) rcu010, sum(cettde) rcu015, sum(cetmaf) rcu009,
047413170223         sum(cettaf) rcu014, sum(cetses) rcu033
047414170223         from ((SELECT tasaas anno,
047415170224         substr(digits( tasmgs), 1, 2) mese , taslnp lnp,
047416170223         substr(a.tblkey, 4, 2) fpe,
047417170223         substr(b.tbluni, 25, 1) tpo ,
047418170223         rtvpes(tasncl,tasncp, taspkc, taspkf) pkg,
047419170224         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) vol, tasncl,
047420170224         tasimv, cetpar,
047421170223         cetpam, cetpau, cetpav, cetard, cetarm, cetaru, cetarv, cettrl,
047422170223         cetpag, cetsea, cetsec, cetrcv, cetmde, cettde, cetmaf, cettaf,
047423170223         cetses
047424170223         FROM titas10f JOIN
047425170222         tabel00f a on a.tblcod= '2L' and substr (a.tblkey, 1, 3)='S2C' and
047426170222         digits( rtvpes(tasncl,tasncp, taspkc, taspkf)) > substr( a.tbluni,
047427170222         15, 6) and digits( rtvpes(tasncl,tasncp, taspkc, taspkf)) <=
047428170222         substr(a.tbluni, 21, 6) join tabel00f b on b.tblcod='TB' and
047429170222         b.tblkey = tastbl join eccet00f on cetaas=tasaas and cetlnp=taslnp
047430170223         and cetnrs=tasnrs and cetnsp=tasnsp and cettbl=tastbl
047431170224         WHERE
047432170224         tasaas*10000+tasmgs >= :DAI and
047433170224         tasaas*10000+tasmgs <= :DAF and
047434170224         tasksc in ( SELECT kuncof
047435170222         FROM tikun00f WHERE kuntle = 'ST' and kunvat = ' ' and kuncop=
047436170223         :i17ksc and :data >= kundde and :data <= kundsc GROUP BY
047437170223         kuncof))
047438170223         UNION
047439170224         (SELECT tasaas anno , substr(digits( tasmgs), 1, 2) mese,
047440170224         taslnp lnp,
047441170223         substr(a.tblkey, 4, 2) fpe , substr(b.tbluni, 25, 1) tpo ,
047442170224         rtvpes(tasncl,tasncp, taspkc, taspkf) pkg,
047443170224         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) vol, tasncl,
047444170224         tasimv,
047445170223         cetpar, cetpam, cetpau, cetpav, cetard, cetarm, cetaru, cetarv,
047446170223         cettrl, cetpag, cetsea, cetsec, cetrcv, cetmde, cettde, cetmaf,
047447170223         cettaf, cetses
047448170223         FROM titasp0f join tabel00f a on a.tblcod= '2L' and substr
047449170222         (a.tblkey, 1, 3)='S2C' and digits( rtvpes(tasncl,tasncp, taspkc,
047450170222         taspkf)) > substr( a.tbluni, 15, 6) and digits(
047451170222         rtvpes(tasncl,tasncp, taspkc, taspkf)) <= substr(a.tbluni, 21, 6)
047452170222         join tabel00f b on b.tblcod='TB' and b.tblkey = tastbl join
047453170222         eccet00f on cetaas=tasaas and cetlnp=taslnp and cetnrs=tasnrs and
047454170224         cetnsp=tasnsp and cettbl = tastbl
047455170224         WHERE
047456170224         tasaas*10000+tasmgs >= :DAI and
047457170224         tasaas*10000+tasmgs <= :DAF and
047458170224         tasksc in ( SELECT kuncof FROM tikun00f WHERE
047459170223         kuntle = 'ST' and kunvat = ' ' and kuncop= :i17ksc and :data >=
047460170223         kundde and :data <= kundsc GROUP BY kuncof))
047461170223         ) as tas group by anno , mese, lnp, fpe, tpo
047462170222         ),
047518170223
047519170223         SELCED as (
047520170223         (select tasaas aastas, substr(digits(tasmgs), 1, 2) mgstas,
047521170223         taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
047522170223         substr(b.tbluni, 25, 1) tpotas, cedcmp,
047523170223         sum(cedimp) impced FROM titas10f JOIN tabel00f
047524170223         a on a.tblcod= '2L' and substr (a.tblkey, 1, 3)='S2C' and digits(
047525170223         rtvpes(tasncl,tasncp, taspkc, taspkf)) > substr( a.tbluni, 15, 6)
047526170223         and digits( rtvpes(tasncl,tasncp, taspkc, taspkf)) <=
047527170223         substr(a.tbluni, 21, 6) JOIN tabel00f b on b.tblcod='TB' and
047528170223         b.tblkey = tastbl JOIN ecced00f on cedaas=tasaas and cedlnp=taslnp
047529170224         and cednrs=tasnrs and cednsp=tasnsp and cedtbl=tastbl where
047530170224         tasaas*10000+tasmgs >= :DAI and
047531170224         tasaas*10000+tasmgs <= :DAF and
047532170224         tasksc in ( SELECT kuncof
047533170223         FROM tikun00f WHERE kuntle = 'ST' and kunvat = ' ' and kuncop=
047534170223         :i17ksc and :data >= kundde and :data <= kundsc GROUP BY
047535170224         kuncof) group by tasaas, substr(digits(tasmgs), 1, 2), taslnp,
047536170223         substr(a.tblkey, 4, 2) , substr(b.tbluni, 25, 1), cedcmp)
047900170223         UNION
047901170223         (SELECT tasaas aastas, substr(digits(tasmgs), 1, 2) mgstas,
047902170223         taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
047903170223         substr(b.tbluni, 25, 1) tpotas, cedcmp,
047904170223         sum(cedimp) impced FROM titasp0f JOIN tabel00f
047905170223         a on a.tblcod= '2L' and substr (a.tblkey, 1, 3)='S2C' and digits(
047906170223         rtvpes(tasncl,tasncp, taspkc, taspkf)) > substr( a.tbluni, 15, 6)
047907170223         and digits( rtvpes(tasncl,tasncp, taspkc, taspkf)) <=
047908170223         substr(a.tbluni, 21, 6) JOIN tabel00f b on b.tblcod='TB' and
047909170223         b.tblkey = tastbl JOIN ecced00f on cedaas=tasaas and cedlnp=taslnp
047910170224         and cednrs=tasnrs and cednsp=tasnsp and cedtbl = tastbl where
047911170224         tasaas*10000+tasmgs >= :DAI and
047912170224         tasaas*10000+tasmgs <= :DAF and
047913170224         tasksc in ( SELECT kuncof
047914170223         FROM tikun00f WHERE kuntle = 'ST' and kunvat = ' ' and kuncop=
047915170223         :i17ksc and :data >= kundde and :data <= kundsc GROUP BY
047916170224         kuncof) group by tasaas, substr(digits(tasmgs), 1, 2), taslnp,
047917170223         substr(a.tblkey, 4, 2) , substr(b.tbluni, 25, 1), cedcmp))
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
048012170223          FROM  selcet JOIN selced on
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
059410170224         SELCET as (
059411170224         select anno, mese, lnp, fpe, tpo , count(*) rcunsp,
059412170224         sum(pkg) rcupkg, sum(vol) rcuvol,
059413170224         sum(tasncl) rcunco, sum(tasimv) rcuirb,
059414170224         sum(cetpar) rcu001, sum(cetpam) rcu002, sum(cetpau) rcu003,
059415170224         sum(cetpav) rcu004, sum(cetard) rcu005, sum(cetarm) rcu006,
059416170224         sum(cetaru) rcu007, sum(cetarv) rcu008,
059417170224         sum(cettrl) rcu013, sum(cetpag) rcu029, sum(cetsea) rcu026,
059418170224         sum(cetsec) rcu025, sum(cetrcv) rcu027,
059419170224         sum(cetmde) rcu010, sum(cettde) rcu015, sum(cetmaf) rcu009,
059420170224         sum(cettaf) rcu014, sum(cetses) rcu033
059421170224         from ((SELECT tasaas anno,
059422170224         substr(digits( tasmgs), 1, 2) mese , taslnp lnp,
059423170224         substr(a.tblkey, 4, 2) fpe,
059424170224         substr(b.tbluni, 25, 1) tpo ,
059425170224         rtvpes(tasncl,tasncp, taspkc, taspkf) pkg,
059426170224         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) vol, tasncl,
059427170224         tasimv, cetpar,
059428170224         cetpam, cetpau, cetpav, cetard, cetarm, cetaru, cetarv, cettrl,
059429170224         cetpag, cetsea, cetsec, cetrcv, cetmde, cettde, cetmaf, cettaf,
059430170224         cetses
059431170224         FROM titas10f JOIN
059432170224         tabel00f a on a.tblcod= '2L' and substr (a.tblkey, 1, 3)='S2C' and
059433170224         digits( rtvpes(tasncl,tasncp, taspkc, taspkf)) > substr( a.tbluni,
059434170224         15, 6) and digits( rtvpes(tasncl,tasncp, taspkc, taspkf)) <=
059435170224         substr(a.tbluni, 21, 6) join tabel00f b on b.tblcod='TB' and
059436170224         b.tblkey = tastbl join eccet00f on cetaas=tasaas and cetlnp=taslnp
059437170224         and cetnrs=tasnrs and cetnsp=tasnsp and cettbl=tastbl
059438170224         WHERE
059439170224         tasaas*10000+tasmgs >= :DAI and
059440170224         tasaas*10000+tasmgs <= :DAF and
059441170224         tasksc = :i17ksc)
059442170224         UNION
059443170224         (SELECT tasaas anno , substr(digits( tasmgs), 1, 2) mese,
059444170224         taslnp lnp,
059445170224         substr(a.tblkey, 4, 2) fpe , substr(b.tbluni, 25, 1) tpo ,
059446170224         rtvpes(tasncl,tasncp, taspkc, taspkf) pkg,
059447170224         rtvvol(tasncl, tasncr, tasfvf, tasvlc, tasvlf) vol, tasncl,
059448170224         tasimv,
059449170224         cetpar, cetpam, cetpau, cetpav, cetard, cetarm, cetaru, cetarv,
059450170224         cettrl, cetpag, cetsea, cetsec, cetrcv, cetmde, cettde, cetmaf,
059451170224         cettaf, cetses
059452170224         FROM titasp0f join tabel00f a on a.tblcod= '2L' and substr
059453170224         (a.tblkey, 1, 3)='S2C' and digits( rtvpes(tasncl,tasncp, taspkc,
059454170224         taspkf)) > substr( a.tbluni, 15, 6) and digits(
059455170224         rtvpes(tasncl,tasncp, taspkc, taspkf)) <= substr(a.tbluni, 21, 6)
059456170224         join tabel00f b on b.tblcod='TB' and b.tblkey = tastbl join
059457170224         eccet00f on cetaas=tasaas and cetlnp=taslnp and cetnrs=tasnrs and
059458170224         cetnsp=tasnsp and cettbl=tastbl
059459170224         WHERE
059460170224         tasaas*10000+tasmgs >= :DAI and
059461170224         tasaas*10000+tasmgs <= :DAF and
059462170224         tasksc = :i17ksc)
059463170224         ) as tas group by anno , mese, lnp, fpe, tpo
059464170224         ),
059465170224
059466170224         SELCED as (
059467170224         (select tasaas aastas, substr(digits(tasmgs), 1, 2) mgstas,
059468170224         taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
059469170224         substr(b.tbluni, 25, 1) tpotas, cedcmp,
059470170224         sum(cedimp) impced FROM titas10f JOIN tabel00f
059471170224         a on a.tblcod= '2L' and substr (a.tblkey, 1, 3)='S2C' and digits(
059472170224         rtvpes(tasncl,tasncp, taspkc, taspkf)) > substr( a.tbluni, 15, 6)
059473170224         and digits( rtvpes(tasncl,tasncp, taspkc, taspkf)) <=
059474170224         substr(a.tbluni, 21, 6) JOIN tabel00f b on b.tblcod='TB' and
059475170224         b.tblkey = tastbl JOIN ecced00f on cedaas=tasaas and cedlnp=taslnp
059476170224         and cednrs=tasnrs and cednsp=tasnsp and cedtbl=tastbl where
059477170224         tasaas*10000+tasmgs >= :DAI and
059478170224         tasaas*10000+tasmgs <= :DAF and
059479170224         tasksc = :i17ksc
059480170224         group by tasaas, substr(digits(tasmgs), 1, 2), lnptas,
059481170224         substr(a.tblkey, 4, 2) , substr(b.tbluni, 25, 1), cedcmp)
059482170224         UNION
059483170224         (SELECT tasaas aastas, substr(digits(tasmgs), 1, 2) mgstas,
059484170224         taslnp lnptas, substr(a.tblkey, 4, 2) fpetas,
059485170224         substr(b.tbluni, 25, 1) tpotas, cedcmp,
059486170224         sum(cedimp) impced FROM titasp0f JOIN tabel00f
059487170224         a on a.tblcod= '2L' and substr (a.tblkey, 1, 3)='S2C' and digits(
059488170224         rtvpes(tasncl,tasncp, taspkc, taspkf)) > substr( a.tbluni, 15, 6)
059489170224         and digits( rtvpes(tasncl,tasncp, taspkc, taspkf)) <=
059490170224         substr(a.tbluni, 21, 6) JOIN tabel00f b on b.tblcod='TB' and
059491170224         b.tblkey = tastbl JOIN ecced00f on cedaas=tasaas and cedlnp=taslnp
059492170224         and cednrs=tasnrs and cednsp=tasnsp and cedtbl = tastbl where
059493170224         tasaas*10000+tasmgs >= :DAI and
059494170224         tasaas*10000+tasmgs <= :DAF and
059495170224         tasksc = :i17ksc
059496170224         group by tasaas, substr(digits(tasmgs), 1, 2), lnptas,
059497170224         substr(a.tblkey, 4, 2) , substr(b.tbluni, 25, 1), cedcmp))
059498170224
059499170224          SELECT
059500170224          anno, mese, lnp, fpe, tpo,
059501170224          rcunsp, rcupkg, rcunco, rcuvol, rcuirb,
059502170224          rcu001, rcu002, rcu003,
059503170224          rcu004, rcu005, rcu006,
059504170224          rcu007, rcu008,
059505170224          rcu013, rcu029, rcu026,
059506170224          rcu025, rcu027,
059507170224          rcu010, rcu015, rcu009,
059508170224          rcu014, rcu033,
059509170224          cedcmp, impced
059510170224          FROM  selcet JOIN selced on
059511170224          anno = aastas and mese=mgstas and lnp=lnptas
059512170224          and fpe=fpetas and tpo=tpotas;
059513170224
059514170224         exec sql open rcuk;
059515170224
059516170224         dow not $finercu;
059517170224          exec sql fetch next from rcuk into
059518170224          :£rcuaas, :mesrcu, :£rcufpe, :£rcutpo,
059519170224          :£rcunsp, :£rcupkg, :£rcunco, :£rcuvol, :£rcuirb,
059520170224          :£rcu001, :£rcu002, :£rcu003,
059521170224          :£rcu004, :£rcu005, :£rcu006,
059522170224          :£rcu007, :£rcu008,
059523170224          :£rcu013, :£rcu029, :£rcu026,
059524170224          :£rcu025, :£rcu027,
059525170224          :£rcu010, :£rcu015, :£rcu009,
059526170224          :£rcu014, :£rcu033,
059527170224          :cmpced, :impced;
059528170224       //?fine file o errore sql esco
059529170224          if sqlcod = 100 or sqlcod < 0;
059530170224           $finercu = *on;
059531170224           leave;
059532170224          endif;
059533170224
059534170224          exsr scrivo;
059535170224
059536170224         enddo;
059544170223
059545170223       //?chiudo il cursore
059546170223          exec sql close rcuk;
059547170224
059548170224       //?leggo i saldi per reperire le note credito o debito e le
059549170224       //?metto nel record della lnp del cliente unificante
059550170224         exec sql
059551170224         DECLARE clientik NO SCROLL CURSOR FOR
059552170224         SELECT sdcann ,sdcmes, sdcfpe, sdctpo,
059553170224         sum(F) rcuirr, sum(G) rcuinc FROM
059554170224         (SELECT sdcann, sdcmes, sdcfpe, sdctpo,
059555170224         case when sdcrbl in
059556170224         ('N', 'R') then sdcirr else 0 end as F , case when sdcrbl in ('N',
059557170224         'R') then sdcinc else 0 end as G FROM sisdc00f WHERE
059558170224         sdcann*100+sdcmes >= :I17DAI and
059559170224         sdcann*100+sdcmes <= :I17DAF
059560170224         and sdcksc = :i17ksc)
059563170224         as TABELLA GROUP BY sdcann, sdcmes, sdcfpe , sdctpo;
059564170224       EXEC SQL OPEN clientik;
059565170224
059566170224       DOU sqlCode <> 0;
059567170224
059568170224         EXEC SQL
059569170224           FETCH NEXT FROM clientik
059570170224             INTO :sdcann, :sdcmes, :sdcfpe, :sdctpo,
059571170224                  :£rcuirr, :£rcuinc;
059572170224
059573170224         SELECT;
059574170224           WHEN sqlCode = 100;
059575170224             LEAVE;
059576170224           WHEN sqlCode < 0;
059577170224             LEAVE;
059578170224         ENDSL;
059579170224
059580170224       exsr aggio;
059581170224
059582170224       ENDDO;
059583170224
059584170224       EXEC SQL CLOSE clientik;
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
