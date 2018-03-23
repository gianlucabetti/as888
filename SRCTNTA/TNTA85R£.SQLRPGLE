000100070320     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200070320
000300070320      *------------------------------------------------------------------------*
000400070320      *
000500070320      *                      STAMPA LUOGHI             ?
000600070320      *
000700070320      *------------------------------------------------------------------------*
000800070320     fazorg01l  if   e           k disk
000900070326     ffnspe01l  if   e           k disk    prefix(w)
001000070326     ffnsp201l  if   e           k disk    prefix(w)
001100070320     ftabel00f  if   e           k disk
001200070330     ftnta85p   o    e             printer oflind(*in99) infds(prtline)
001300070320
001400070320      *------------------------------------------------------------------------*
001500070320      *  RIEPILOGO INDICATORI
001600070320      *------------------------------------------------------------------------*
001700070321      * 01 - richiesta stampa dei soli luoghi 300 con giri impostati
001800070326      * 02 - non stampa il tipo selezione
001900070330      * 03 - non stampa luogo
002000070321      * 98 - stampo la testata per la prima volta
002100070320      * 99 - salto pagina
002200070320      *------------------------------------------------------------------------*
002300070320
002400070320      *   V A R I A B I L I
002500070326     d savspecli       s                   like(specli)
002600070326     d savspecod       s                   like(specod)
002700070326     d xx              s              2  0
002800070326     d yy              s              2  0
002900070326     d zz              s              3  0
003000070326     d wfil            s              3
003100070326     d woklna          s              1    inz(*off)
003200070326     d wtime14         s             14  0
003300070320
003400070320      *   S C H I E R E
003500070326     d skg1            s             10    dim(6)
003600070326     d skg2            s             10    dim(6)
003700070326     d skg3            s             10    dim(6)
003800070326     d skg4            s             10    dim(6)
003900070326     d skpa            s              3    dim(20) inz(*zeros)
004000070326     d skpp            s              3    dim(20) inz(*zeros)
004100070326     d skpz            s              2    dim(20) inz(*zeros)
004200070320     d skpog           s              3    dim(250) inz(*zeros)
004300070320
004400070320      *   D S   I N T E R N E / E S T E R N E
004500070320     d param85         ds
004600070320     d  ta85fil                       3  0
004700070326     d  ta85luo                       3
004800070320     d  ta85ksc                       7  0
004900070320     d  ta85sel                       1
005000070320     d  ta85cgis                      1
005100070320     d  ta85abi                       2
005200070330
005300070330     d prtline         ds
005400070330     d  curline              367    368i 0
005500070321
005600070321     d                 ds
005700070321     d pivaeu                  1      2
005800070321     d pivait                  3     16
005900070321     d piva                    1     16
006000070320
006100070320     d azuteds       e ds                  extname(azute00f)
006200070320     d ddatiute      e ds
006300070320     d ds4l          e ds
006400070321     d ds_cnaco      e ds                  inz  extname(cnaco00f)
006500070321     d ds_cnind      e ds                  inz  extname(cnind00f)
006600070321     d ds_cnclp      e ds                  inz  extname(cnclp00f)
006700070321     d ds_fncls      e ds                  inz  extname(fncls00f)
006800070321     d fidg09ds      e ds
006900070320     d fnspeds       e ds                  extname(fnspe00f)
007000070320     d fnsp2ds       e ds                  extname(fnsp200f)
007100070320     d kpjba         e ds
007200070320     d tibs34ds      e ds
007300070321     d tibs69ds      e ds
007400070320     d trul31ds      e ds
007500070320
007600070320      *   C O S T A N T I
007700070320
007800070320      *------------------------------------------------------------------------*
007900070320
008000070320     c                   select
008100070320
008200070320      * --> richiesta per cliente
008300070320     c                   when      ta85ksc <> *zeros
008400070321     c                   exsr      sr_sqla1
008500070320
008600070321      * --> richiesta per una filiale e luogo ma non luogo 300
008700070326     c                   when      ta85fil <> 999 and ta85luo <> *blanks
008800070326     c                             and ta85luo <> '300'
008900070321     c                   exsr      sr_sqla2
009000070321
009100070321      * --> richiesta per tutte le filiali e luogo ma non luogo 300
009200070326     c                   when      ta85fil = 999 and ta85luo <> *blanks
009300070326     c                             and ta85luo <> '300'
009400070321     c                   exsr      sr_sqla3
009500070320
009600070321      * --> richiesta per tutti i luoghi 300 selezione per cliente
009700070321     c                   when      ta85cgis <> 'S' and ta85sel = '1'
009800070321     c                   exsr      sr_sqla4
009900070321
010000070321      * --> richiesta per tutti i luoghi 300 selezione per lna
010100070321     c                   when      ta85cgis <> 'S' and ta85sel = '2'
010200070321     c                   exsr      sr_sqla5
010300070326
010400070326      * --> richiesta luoghi 300 solo con giri impostati
010500070326     c                   other
010600070326     c                   exsr      sr_sqla6
010700070320
010800070320     c                   endsl
010900070320
011000070320     c                   eval      *inlr = *on
011100070320
011200070320      *------------------------------------------------------------------------*
011300070321      *  cursore a1 stampa x cliente
011400070320      *------------------------------------------------------------------------*
011500070321     c     sr_sqla1      begsr
011600070320
011700070320     c/exec sql
011800070320     c+ declare a1 cursor for select
011900070330     c+ fnspe00f.*, ifnull(sp2cli, specli),
012000070330     c+ ifnull(sp2cod, specod),
012100070330     c+ ifnull(sp2tpe, ' '),
012200070327     c+ ifnull(sp2est, ' ')
012300070327     c+ from fnspe00f left outer join fnsp200f
012400070327     c+ on specli = sp2cli and
012500070327     c+ specod=sp2cod
012600070320     c+ where spefls = 'L' and specli = :ta85ksc
012700070320     c+ order by
012800070320     c+ fnspe00f.specod
012900070320     c/end-exec
013000070320
013100070320     c/exec sql
013200070320     c+ open a1
013300070320     c/end-exec
013400070320
013500070320     c                   do        *hival
013600070320
013700070320     c/exec sql
013800070327     c+ fetch next from a1 into :fnspeds, :sp2cli,
013900070327     c+ :sp2cod, :sp2tpe, :sp2est
014000070320     c/end-exec
014100070320
014200070320     c                   select
014300070320      * fine file
014400070320     c                   when      sqlcod = 100
014500070320     c                   leave
014600070320      * errore
014700070320     c                   when      sqlcod < 0
014800070320     c                   leave
014900070320     c                   other
015000070320      * scarto i luoghi non richiesti
015100070326     c                   if        ta85luo <> *blanks
015200070320     c                             and specod <> ta85luo
015300070320     c                   iter
015400070320     c                   endif
015500070326
015600070326      * se luogo non richiesto imposto tutti i luoghi
015700070326     c                   if        ta85luo = *blanks
015800070402     c                   eval      desluo = 'TUTTI'
015900070330      * se luogo richiesto non devo stampare il luogo
016000070330     c                   else
016100070330     c                   eval      *in03 = *on
016200070330     c                   endif
016300070321
016400070330      * stampo la testata per la prima volta
016500070330     c                   if        not *in98
016600070330     c                   write     ta8501
016700070330     c                   eval      *in98 = *on
016800070330     c                   endif
016900070321
017000070321      * a cambio di luogo stampo la separazione
017100070321     c                   if        specod <> savspecod
017200070326      * prima a rottura di luogo stampo i dati
017300070326     c                   if        savspecod > *zeros
017400070326     c                   exsr      sr_stampa
017500070326     c                   endif
017600070330      * decodifico il luogo
017700070330     c                   if        not *in03
017800070321     c                   exsr      sr_decdati
017900070330     c                   add       2             curline
018000070330     c                   exsr      sr_saltopag
018100070330     c                   write     ta85021
018200070330     c                   endif
018300070321      * salvo il luogo
018400070321     c                   eval      savspecod = specod
018500070321     c                   endif
018600070320
018700070321      * imposto i campi per la stampa
018800070321     c                   exsr      sr_impdati
018900070321
019000070320     c                   endsl
019100070321
019200070321     c                   enddo
019300070320
019400070320     c/exec sql
019500070320     c+ close a1
019600070320     c/end-exec
019700070326
019800070326      * stampo dati ultimo luogo
019900070326     c                   if        savspecod > *zeros
020000070326     c                   exsr      sr_stampa
020100070326     c                   endif
020200070320
020300070320     c                   endsr
020400070321
020500070321      *------------------------------------------------------------------------*
020600070321      *  cursore a2 stampa x luogo una sola filiale no luogo 300
020700070321      *------------------------------------------------------------------------*
020800070321     c     sr_sqla2      begsr
020900070321
021000070321     c/exec sql
021100070321     c+ declare a2 cursor for select
021200070330     c+ fnspe00f.*, ifnull(sp2cli, specli),
021300070330     c+ ifnull(sp2cod, specod),
021400070330     c+ ifnull(sp2tpe, ' '),
021500070330     c+ ifnull(sp2est, ' ')
021600070330     c+ from fnspe00f left outer join fnsp200f
021700070330     c+ on specli = sp2cli and
021800070330     c+ specod=sp2cod
021900070321     c+ where spefls = 'L' and
022000070321     c+ substr(digits(specli), 1, 3) = :ta85fil
022100070321     c+ and specod = :ta85luo
022200070321     c+ order by
022300070321     c+ fnspe00f.specli
022400070321     c/end-exec
022500070321
022600070321     c/exec sql
022700070321     c+ open a2
022800070321     c/end-exec
022900070321
023000070321     c                   do        *hival
023100070321
023200070321     c/exec sql
023300070330     c+ fetch next from a2 into :fnspeds, :sp2cli,
023400070330     c+ :sp2cod, :sp2tpe, :sp2est
023500070321     c/end-exec
023600070321
023700070321     c                   select
023800070321      * fine file
023900070321     c                   when      sqlcod = 100
024000070321     c                   leave
024100070321      * errore
024200070321     c                   when      sqlcod < 0
024300070321     c                   leave
024400070321     c                   other
024500070330
024600070330      * stampo tutti i clienti
024700070402     c                   eval      desksc = 'TUTTI'
024800070321
024900070330      * stampo la testata per la prima volta
025000070330     c                   if        not *in98
025100070330     c                   write     ta8501
025200070330     c                   eval      *in98 = *on
025300070330     c                   endif
025400070321
025500070321      * a cambio di cliente stampo la separazione
025600070321     c                   if        specli <> savspecli
025700070326      * prima a rottura di cliente stampo i dati
025800070326     c                   if        savspecli > *zeros
025900070326     c                   exsr      sr_stampa
026000070326     c                   endif
026100070330      * decodifico il cliente
026200070321     c                   exsr      sr_decdati
026300070330     c                   add       2             curline
026400070330     c                   exsr      sr_saltopag
026500070330     c                   write     ta8502
026600070321      * salvo il cliente
026700070321     c                   eval      savspecli = specli
026800070321      * salvo il luogo (mi serve solo per la routine sr_stampa in questo tipo
026900070321      *                 di stampa ho solo un luogo richiesto)
027000070321     c                   eval      savspecod = specod
027100070321     c                   endif
027200070321
027300070321      * imposto i campi per la stampa
027400070321     c                   exsr      sr_impdati
027500070321
027600070321     c                   endsl
027700070321
027800070321     c                   enddo
027900070321
028000070321     c/exec sql
028100070321     c+ close a2
028200070321     c/end-exec
028300070326
028400070326      * stampo dati ultimo cliente
028500070326     c                   if        savspecli > *zeros
028600070326     c                   exsr      sr_stampa
028700070326     c                   endif
028800070321
028900070321     c                   endsr
029000070321
029100070321      *------------------------------------------------------------------------*
029200070321      *  cursore a3 stampa x luogo x tutte le filiali no luogo 300
029300070321      *------------------------------------------------------------------------*
029400070321     c     sr_sqla3      begsr
029500070321
029600070321      * devo loopare per tutte le filiali abilitate al profilo
029700070321     c                   eval      zz = 1
029800070321     c                   for       zz by 1 to 250
029900070321     c                   if        skpog(zz) <> *zeros
030000070330     c                   eval      *in98 = *off
030100070321     c                   move      skpog(zz)     wfil
030200070321
030300070321     c/exec sql
030400070321     c+ declare a3 cursor for select
030500070330     c+ fnspe00f.*, ifnull(sp2cli, specli),
030600070330     c+ ifnull(sp2cod, specod),
030700070330     c+ ifnull(sp2tpe, ' '),
030800070330     c+ ifnull(sp2est, ' ')
030900070330     c+ from fnspe00f left outer join fnsp200f
031000070330     c+ on specli = sp2cli and
031100070330     c+ specod=sp2cod
031200070321     c+ where spefls = 'L' and
031300070326     c+ substr(digits(specli), 1, 3) = :wfil
031400070321     c+ and specod = :ta85luo
031500070321     c+ order by
031600070321     c+ fnspe00f.specli
031700070321     c/end-exec
031800070321
031900070321     c/exec sql
032000070321     c+ open a3
032100070321     c/end-exec
032200070321
032300070321     c                   do        *hival
032400070321
032500070321     c/exec sql
032600070330     c+ fetch next from a3 into :fnspeds, :sp2cli,
032700070330     c+ :sp2cod, :sp2tpe, :sp2est
032800070321     c/end-exec
032900070321
033000070321     c                   select
033100070321      * fine file
033200070321     c                   when      sqlcod = 100
033300070321     c                   leave
033400070321      * errore
033500070321     c                   when      sqlcod < 0
033600070321     c                   leave
033700070321     c                   other
033800070330
033900070330      * stampo tutti i clienti
034000070402     c                   eval      desksc = 'TUTTI'
034100070321
034200070330      * stampo la testata per la prima volta
034300070330     c                   if        not *in98
034400070330     c                   write     ta8501
034500070330     c                   eval      *in98 = *on
034600070330     c                   endif
034700070321
034800070321      * a cambio di cliente stampo la separazione
034900070321     c                   if        specli <> savspecli
035000070326      * prima a rottura di cliente stampo i dati
035100070326     c                   if        savspecli > *zeros
035200070326     c                   exsr      sr_stampa
035300070326     c                   endif
035400070330      * decodifico il cliente
035500070321     c                   exsr      sr_decdati
035600070330     c                   add       2             curline
035700070330     c                   exsr      sr_saltopag
035800070330     c                   write     ta8502
035900070321      * salvo il cliente
036000070321     c                   eval      savspecli = specli
036100070321      * salvo il luogo (mi serve solo per la routine sr_stampa in questo tipo
036200070321      *                 di stampa ho solo un luogo richiesto)
036300070321     c                   eval      savspecod = specod
036400070321     c                   endif
036500070321
036600070321      * imposto i campi per la stampa
036700070321     c                   exsr      sr_impdati
036800070321
036900070321     c                   endsl
037000070321
037100070321     c                   enddo
037200070321
037300070321     c/exec sql
037400070321     c+ close a3
037500070321     c/end-exec
037600070326
037700070326      * stampo dati ultimo cliente
037800070326     c                   if        savspecli > *zeros
037900070326     c                   exsr      sr_stampa
038000070326     c                   endif
038100070321
038200070321     c                   endif
038300070321     c                   endfor
038400070321
038500070321     c                   endsr
038600070321
038700070321      *------------------------------------------------------------------------*
038800070321      *  cursore a4 stampa x tutti i luoghi 300 selezione per cliente
038900070321      *------------------------------------------------------------------------*
039000070321     c     sr_sqla4      begsr
039100070321
039200070321      * devo loopare per tutte le filiali abilitate al profilo
039300070321      * anche se richiesta solo una filiale
039400070321     c                   if        ta85fil <> 999
039500070330     c                   eval      skpog = *all'0'
039600070321     c                   move      ta85fil       skpog(1)
039700070321     c                   endif
039800070321
039900070321     c                   eval      zz = 1
040000070321     c                   for       zz by 1 to 250
040100070321     c                   if        skpog(zz) <> *zeros
040200070330     c                   eval      *in98 = *off
040300070321     c                   move      skpog(zz)     wfil
040400070321
040500070321     c/exec sql
040600070321     c+ declare a4 cursor for select
040700070330     c+ fnspe00f.*, ifnull(sp2cli, specli),
040800070330     c+ ifnull(sp2cod, specod),
040900070330     c+ ifnull(sp2tpe, ' '),
041000070330     c+ ifnull(sp2est, ' ')
041100070330     c+ from fnspe00f left outer join fnsp200f
041200070330     c+ on specli = sp2cli and
041300070330     c+ specod=sp2cod
041400070321     c+ where spefls = 'L' and
041500070321     c+ substr(digits(specli), 1, 3) = :wfil
041600070326     c+ and specod = '300'
041700070321     c+ order by
041800070321     c+ fnspe00f.specli
041900070321     c/end-exec
042000070321
042100070321     c/exec sql
042200070321     c+ open a4
042300070321     c/end-exec
042400070321
042500070321     c                   do        *hival
042600070321
042700070321     c/exec sql
042800070330     c+ fetch next from a4 into :fnspeds, :sp2cli,
042900070330     c+ :sp2cod, :sp2tpe, :sp2est
043000070321     c/end-exec
043100070321
043200070321     c                   select
043300070321      * fine file
043400070321     c                   when      sqlcod = 100
043500070321     c                   leave
043600070321      * errore
043700070321     c                   when      sqlcod < 0
043800070321     c                   leave
043900070321     c                   other
044000070330
044100070330      * stampa tutti i clienti
044200070402     c                   eval      desksc = 'TUTTI'
044300070321
044400070330      * stampo la testata per la prima volta
044500070330     c                   if        not *in98
044600070330     c                   write     ta8501
044700070330     c                   eval      *in98 = *on
044800070330     c                   endif
044900070321
045000070321      * a cambio di cliente stampo la separazione
045100070321     c                   if        specli <> savspecli
045200070326      * prima a rottura di cliente stampo i dati
045300070326     c                   if        savspecli > *zeros
045400070326     c                   exsr      sr_stampa
045500070326     c                   endif
045600070330      * decodifico il cliente
045700070321     c                   exsr      sr_decdati
045800070330     c                   add       2             curline
045900070330     c                   exsr      sr_saltopag
046000070330     c                   write     ta8502
046100070321      * salvo il cliente
046200070321     c                   eval      savspecli = specli
046300070321      * salvo il luogo (mi serve solo per la routine sr_stampa in questo tipo
046400070321      *                 di stampa ho solo un luogo richiesto)
046500070321     c                   eval      savspecod = specod
046600070321     c                   endif
046700070321
046800070321      * imposto i campi per la stampa
046900070321     c                   exsr      sr_impdati
047000070321
047100070321     c                   endsl
047200070321
047300070321     c                   enddo
047400070321
047500070321     c/exec sql
047600070321     c+ close a4
047700070321     c/end-exec
047800070326
047900070326      * stampo dati ultimo cliente
048000070326     c                   if        savspecli > *zeros
048100070326     c                   exsr      sr_stampa
048200070326     c                   endif
048300070321
048400070321     c                   endif
048500070321     c                   endfor
048600070321
048700070321     c                   endsr
048800070321
048900070321      *------------------------------------------------------------------------*
049000070321      *  cursore a5 stampa x tutti i luoghi 300 selezione per lna
049100070321      *------------------------------------------------------------------------*
049200070321     c     sr_sqla5      begsr
049300070321
049400070321      * devo loopare per tutte le filiali abilitate al profilo
049500070321      * anche se richiesta solo una filiale
049600070321     c                   if        ta85fil <> 999
049700070330     c                   eval      skpog = *all'0'
049800070321     c                   move      ta85fil       skpog(1)
049900070321     c                   endif
050000070321
050100070321     c                   eval      zz = 1
050200070321     c                   for       zz by 1 to 250
050300070321     c                   if        skpog(zz) <> *zeros
050400070330     c                   eval      *in98 = *off
050500070321     c                   move      skpog(zz)     wfil
050600070321
050700070326      * estraggo un file (solo sp2) con solo i codici clienti e luogo
050800070326      * clienti che hanno almeno una lna = alla lna che sto leggendo
050900070321     c/exec sql
051000070321     c+ declare a5 cursor for select
051100070326     c+ sp2cli, sp2cod from fnsp200f
051200070326     c+ where sp2tpe = 'PA' and
051300070326     c+ substr(sp2est, 1, 3) = :wfil or
051400070326     c+ sp2est = 'PA' and
051500070326     c+ substr(sp2est, 4, 3) = :wfil or
051600070326     c+ sp2est = 'PA' and
051700070326     c+ substr(sp2est, 7, 3) = :wfil or
051800070326     c+ sp2est = 'PA' and
051900070326     c+ substr(sp2est, 10, 3) = :wfil or
052000070326     c+ sp2est = 'PA' and
052100070326     c+ substr(sp2est, 13, 3) = :wfil or
052200070326     c+ sp2est = 'PA' and
052300070326     c+ substr(sp2est, 16, 3) = :wfil or
052400070326     c+ sp2est = 'PA' and
052500070326     c+ substr(sp2est, 19, 3) = :wfil or
052600070326     c+ sp2est = 'PA' and
052700070326     c+ substr(sp2est, 22, 3) = :wfil or
052800070326     c+ sp2est = 'PA' and
052900070326     c+ substr(sp2est, 25, 3) = :wfil or
053000070326     c+ sp2est = 'PA' and
053100070326     c+ substr(sp2est, 28, 3) = :wfil or
053200070326     c+ sp2est = 'PA' and
053300070326     c+ substr(sp2est, 31, 3) = :wfil or
053400070326     c+ sp2est = 'PA' and
053500070326     c+ substr(sp2est, 34, 3) = :wfil or
053600070326     c+ sp2est = 'PA' and
053700070326     c+ substr(sp2est, 37, 3) = :wfil or
053800070326     c+ sp2est = 'PA' and
053900070326     c+ substr(sp2est, 40, 3) = :wfil or
054000070326     c+ sp2est = 'PA' and
054100070326     c+ substr(sp2est, 43, 3) = :wfil or
054200070326     c+ sp2est = 'PA' and
054300070326     c+ substr(sp2est, 46, 3) = :wfil or
054400070326     c+ sp2est = 'PA' and
054500070326     c+ substr(sp2est, 49, 3) = :wfil or
054600070326     c+ sp2est = 'PA' and
054700070326     c+ substr(sp2est, 52, 3) = :wfil or
054800070326     c+ sp2est = 'PA' and
054900070326     c+ substr(sp2est, 55, 3) = :wfil or
055000070326     c+ sp2est = 'PA' and
055100070326     c+ substr(sp2est, 58, 3) = :wfil
055200070326     c+ order by sp2cli
055300070321     c/end-exec
055400070321
055500070321     c/exec sql
055600070326     c+ open a5
055700070321     c/end-exec
055800070321
055900070321     c                   do        *hival
056000070321
056100070321     c/exec sql
056200070326     c+ fetch next from a5 into :sp2cli, :sp2cod
056300070321     c/end-exec
056400070321
056500070321     c                   select
056600070321      * fine file
056700070321     c                   when      sqlcod = 100
056800070321     c                   leave
056900070321      * errore
057000070321     c                   when      sqlcod < 0
057100070321     c                   leave
057200070321     c                   other
057300070330
057400070330      * stampa tutti i clienti
057500070402     c                   eval      desksc = 'TUTTI'
057600070321
057700070330      * stampo la testata per la prima volta
057800070330     c                   if        not *in98
057900070321     c                   write     ta8501
058000070330     c                   eval      *in98 = *on
058100070321     c                   endif
058200070326
058300070326     c                   eval      specli = sp2cli
058400070326     c                   eval      specod = sp2cod
058500070321
058600070321      * a cambio di cliente stampo la separazione
058700070326     c                   if        specli <> savspecli
058800070326      * prima a rottura di cliente stampo i dati
058900070326     c                   if        savspecli > *zeros
059000070326     c                   exsr      sr_stampa
059100070326     c                   endif
059200070326      * decodifico il cliente e il luogo
059300070321     c                   exsr      sr_decdati
059400070330     c                   add       2             curline
059500070330     c                   exsr      sr_saltopag
059600070321     c                   write     ta8502
059700070321      * salvo il cliente
059800070326     c                   eval      savspecli = specli
059900070321      * salvo il luogo (mi serve solo per la routine sr_stampa in questo tipo
060000070321      *                 di stampa ho solo un luogo richiesto)
060100070326     c                   eval      savspecod = specod
060200070321     c                   endif
060300070321
060400070326      * cerco tutti i dati del cliente letto (luogo 300)
060500070326     c                   eval      spefls = 'L'
060600070326     c     kfnspe        chain     fnspe01l
060700070326     c                   if        %found(fnspe01l)
060800070326     c     kfnsp21       setll     fnsp201l
060900070326     c                   do        *hival
061000070326     c     kfnsp21       reade     fnsp201l
061100070326     c                   if        %eof(fnsp201l)
061200070326     c                   leave
061300070326     c                   endif
061400070326      * imposto i campi per la stampa
061500070326     c                   exsr      sr_impdatiw
061600070326     c                   enddo
061700070326     c                   endif
061800070321
061900070321     c                   endsl
062000070321
062100070321     c                   enddo
062200070321
062300070321     c/exec sql
062400070326     c+ close a5
062500070321     c/end-exec
062600070326
062700070326      * stampo dati ultimo cliente
062800070326     c                   if        savspecli > *zeros
062900070326     c                   exsr      sr_stampa
063000070326     c                   endif
063100070321
063200070321     c                   endif
063300070321     c                   endfor
063400070321
063500070321     c                   endsr
063600070326
063700070326      *------------------------------------------------------------------------*
063800070326      *  cursore a6 stampa x luoghi 300 con solo giri impostati
063900070326      *------------------------------------------------------------------------*
064000070326     c     sr_sqla6      begsr
064100070326
064200070326      * estraggo un file (solo sp2) con solo i codici clienti e luogo
064300070326      * tutti i clienti che hanno i giri impostati
064400070326     c/exec sql
064500070326     c+ declare a6 cursor for select
064600070326     c+ sp2cli, sp2cod from fnsp200f
064700070326     c+ where substr(sp2tpe, 1, 1) = 'G'
064800070326     c+ order by sp2cli
064900070326     c/end-exec
065000070326
065100070326     c/exec sql
065200070326     c+ open a6
065300070326     c/end-exec
065400070326
065500070326     c                   do        *hival
065600070326
065700070326     c/exec sql
065800070326     c+ fetch next from a6 into :sp2cli, :sp2cod
065900070326     c/end-exec
066000070326
066100070326     c                   select
066200070326      * fine file
066300070326     c                   when      sqlcod = 100
066400070326     c                   leave
066500070326      * errore
066600070326     c                   when      sqlcod < 0
066700070326     c                   leave
066800070326     c                   other
066900070330
067000070330      * stampa tutti i clienti
067100070402     c                   eval      desksc = 'TUTTI'
067200070326
067300070330      * stampo la testata per la prima volta
067400070330     c                   if        not *in98
067500070326     c                   write     ta8501
067600070330     c                   eval      *in98 = *on
067700070326     c                   endif
067800070326
067900070326      * devo loopare per tutte le filiali abilitate al profilo
068000070326      * anche se richiesta solo una filiale
068100070326     c                   if        ta85fil <> 999
068200070330     c                   eval      skpog = *all'0'
068300070326     c                   move      ta85fil       skpog(1)
068400070326     c                   endif
068500070326
068600070326      * se selezione per cliente controllo il cliente con le filiali autorizzate
068700070326     c                   if        ta85sel = '1'
068800070326     c                   movel     sp2cli        wfil
068900070326     c     wfil          lookup    skpog                                  30
069000070326     c  n30              iter
069100070326     c                   endif
069200070326
069300070326      * se selezione per lna controllo le lna con le filiali autorizzate
069400070326     c                   if        ta85sel = '2'
069500070326     c                   eval      woklna = *off
069600070326     c                   eval      sp2tpe = 'PA'
069700070326     c     kfnsp2        chain     fnsp201l
069800070326     c                   if        %found(fnsp201l)
069900070326     c                   movea     sp2est        skpa
070000070326     c                   eval      zz = 1
070100070326     c                   for       zz by 1 to 20
070200070326     c                   if        skpa(zz) <> *zeros
070300070326     c     skpa(zz)      lookup    skpog                                  30
070400070326     c  n30              iter
070500070326     c                   eval      woklna = *on
070600070326     c                   endif
070700070326     c                   endfor
070800070326     c                   if        woklna = *off
070900070326     c                   iter
071000070326     c                   endif
071100070326     c                   endif
071200070326     c                   endif
071300070326
071400070326     c                   eval      specli = sp2cli
071500070326     c                   eval      specod = sp2cod
071600070326
071700070326      * a cambio di cliente stampo la separazione
071800070326     c                   if        specli <> savspecli
071900070326      * prima a rottura di cliente stampo i dati
072000070326     c                   if        savspecli > *zeros
072100070326     c                   exsr      sr_stampa
072200070326     c                   endif
072300070326      * decodifico il cliente e il luogo
072400070326     c                   exsr      sr_decdati
072500070330     c                   add       2             curline
072600070330     c                   exsr      sr_saltopag
072700070326     c                   write     ta8502
072800070326      * salvo il cliente
072900070326     c                   eval      savspecli = specli
073000070326      * salvo il luogo (mi serve solo per la routine sr_stampa in questo tipo
073100070326      *                 di stampa ho solo un luogo richiesto)
073200070326     c                   eval      savspecod = specod
073300070326     c                   endif
073400070326
073500070326      * cerco tutti i dati del cliente letto (luogo 300)
073600070326     c                   eval      spefls = 'L'
073700070326     c     kfnspe        chain     fnspe01l
073800070326     c                   if        %found(fnspe01l)
073900070326     c     kfnsp21       setll     fnsp201l
074000070326     c                   do        *hival
074100070326     c     kfnsp21       reade     fnsp201l
074200070326     c                   if        %eof(fnsp201l)
074300070326     c                   leave
074400070326     c                   endif
074500070326      * imposto i campi per la stampa
074600070326     c                   exsr      sr_impdatiw
074700070326     c                   enddo
074800070326     c                   endif
074900070326
075000070326     c                   endsl
075100070326
075200070326     c                   enddo
075300070326
075400070326     c/exec sql
075500070326     c+ close a6
075600070326     c/end-exec
075700070326
075800070326      * stampo dati ultimo cliente
075900070326     c                   if        savspecli > *zeros
076000070326     c                   exsr      sr_stampa
076100070326     c                   endif
076200070326
076300070326     c                   endsr
076400070321
076500070321      *------------------------------------------------------------------------*
076600070321      *  decodifico cliente e luogo
076700070321      *------------------------------------------------------------------------*
076800070321     c     sr_decdati    begsr
076900070321
077000070321     c                   clear                   pdescli
077100070321     c                   clear                   pdescod
077200070321
077300070321      * decodifico cliente
077400070321     c                   clear                   tibs69ds
077500070321     c                   eval      i69kac = specli
077600070321     c                   call      'TIBS69R'
077700070321     c                   parm                    tibs69ds
077800070321     c                   parm                    ds_cnaco
077900070321     c                   parm                    ds_cnind
078000070321     c                   parm                    ds_cnclp
078100070321     c                   parm                    ds_fncls
078200070326     c                   if        o69err =  *blanks
078300070321     c                   eval      pdescli = acorag
078400070321     c                   endif
078500070321
078600070321      * decodifico luogo
078700070321     c                   clear                   ds4l
078800070326     c                   eval      tblkut = 1
078900070321     c                   eval      tblcod = '4L'
079000070321     c                   movel     specod        tblkey
079100070321     c     ktabel        chain     tabel00f
079200070326     c                   if        %found(tabel00f) and tblflg = *blanks
079300070326     c                   eval      ds4l = tbluni
079400070321     c                   endif
079500070326     c                   eval      pdescod = §4ldes
079600070321
079700070321     c                   endsr
079800070321
079900070321      *------------------------------------------------------------------------*
080000070321      *  imposto i dati da stampare
080100070321      *------------------------------------------------------------------------*
080200070321     c     sr_impdati    begsr
080300070321
080400070321      * dati anagrafici
080500070321     c                   eval      prag = sperag
080600070321     c                   eval      pind = speind
080700070321     c                   eval      pcap = specap
080800070321     c                   eval      ploc = speloc
080900070321     c                   eval      ppro = spepro
081000070321     c                   eval      pnaz = spenaz
081100070321      * e-mail
081200070321     c                   if        sp2tpe = 'EM'
081300070321     c                   eval      pmail = sp2est
081400070321     c                   endif
081500070321      * linee di partenza
081600070321     c                   if        sp2tpe = 'PP'
081700070321     c                   movea     sp2est        skpp
081800070321     c                   endif
081900070321      * linee di arrivo
082000070321     c                   if        sp2tpe = 'PA'
082100070321     c                   movea     sp2est        skpa
082200070321     c                   endif
082300070321      * zone
082400070321     c                   if        sp2tpe = 'PZ'
082500070321     c                   movea     sp2est        skpz
082600070321     c                   endif
082700070321      * giri
082800070321     c                   if        sp2tpe = 'G1'
082900070321     c                   movea     sp2est        skg1
083000070321     c                   endif
083100070321     c                   if        sp2tpe = 'G2'
083200070321     c                   movea     sp2est        skg2
083300070321     c                   endif
083400070321     c                   if        sp2tpe = 'G3'
083500070321     c                   movea     sp2est        skg3
083600070321     c                   endif
083700070321     c                   if        sp2tpe = 'G4'
083800070321     c                   movea     sp2est        skg4
083900070321     c                   endif
084000070321
084100070321     c                   endsr
084200070326
084300070326      *------------------------------------------------------------------------*
084400070326      *  imposto i dati da stampare (W)
084500070326      *------------------------------------------------------------------------*
084600070326     c     sr_impdatiw   begsr
084700070326
084800070326      * dati anagrafici
084900070326     c                   eval      prag = wsperag
085000070326     c                   eval      pind = wspeind
085100070326     c                   eval      pcap = wspecap
085200070326     c                   eval      ploc = wspeloc
085300070326     c                   eval      ppro = wspepro
085400070326     c                   eval      pnaz = wspenaz
085500070326      * e-mail
085600070326     c                   if        wsp2tpe = 'EM'
085700070326     c                   eval      pmail = wsp2est
085800070326     c                   endif
085900070326      * linee di partenza
086000070326     c                   if        wsp2tpe = 'PP'
086100070326     c                   movea     wsp2est       skpp
086200070326     c                   endif
086300070326      * linee di arrivo
086400070326     c                   if        wsp2tpe = 'PA'
086500070326     c                   movea     wsp2est       skpa
086600070326     c                   endif
086700070326      * zone
086800070326     c                   if        wsp2tpe = 'PZ'
086900070326     c                   movea     wsp2est       skpz
087000070326     c                   endif
087100070326      * giri
087200070326     c                   if        wsp2tpe = 'G1'
087300070326     c                   movea     wsp2est       skg1
087400070326     c                   endif
087500070326     c                   if        wsp2tpe = 'G2'
087600070326     c                   movea     wsp2est       skg2
087700070326     c                   endif
087800070326     c                   if        wsp2tpe = 'G3'
087900070326     c                   movea     wsp2est       skg3
088000070326     c                   endif
088100070326     c                   if        wsp2tpe = 'G4'
088200070326     c                   movea     wsp2est       skg4
088300070326     c                   endif
088400070326
088500070326     c                   endsr
088600070330
088700070330      *------------------------------------------------------------------------*
088800070330      *  controllo se salto pagina
088900070330      *------------------------------------------------------------------------*
089000070330     c     sr_saltopag   begsr
089100070330
089200070330     c                   if        *in99 or curline >= 65
089300070330     c                   write     ta8501
089400070330     c                   eval      *in99 = *off
089500070330     c                   endif
089600070330
089700070330     c                   endsr
089800070321
089900070321      *------------------------------------------------------------------------*
090000070330      *  stampa dettaglio
090100070321      *------------------------------------------------------------------------*
090200070321     c     sr_stampa     begsr
090300070321
090400070321      * stampo dati anagrafici
090500070330     c                   exsr      sr_saltopag
090600070321     c                   write     ta8503
090700070321      * stampo e-mail
090800070321     c                   if        pmail <> *blanks
090900070330     c                   exsr      sr_saltopag
091000070321     c                   write     ta8504
091100070321     c                   endif
091200070321      * stampo forzature luogo 300
091300070326     c                   if        savspecod = '300'
091400070321     c                   eval      xx = 1
091500070321     c                   for       xx by 1 to 20
091600070321      * se immessa la lnp
091700070321     c                   if        skpp(xx) <> *zeros
091800070321      * imposto la lnp
091900070321     c                   move      skpp(xx)      plnp
092000070321     c     plnp          chain     azorg01l
092100070321     c                   if        %found(azorg01l) and orgfva = *blanks and
092200070321     c                             (orgfag = 'F' or orgfag = 'A')
092300070321     c                   eval      pdeslnp = orgdes
092400070321     c                   endif
092500070321      * imposto la lna
092600070321     c                   move      skpa(xx)      plna
092700070321     c     plna          chain     azorg01l
092800070321     c                   if        %found(azorg01l) and orgfva = *blanks and
092900070321     c                             (orgfag = 'F' or orgfag = 'A')
093000070321     c                   eval      pdeslna = orgdes
093100070321     c                   endif
093200070321      * imposto la zona
093300070321     c                   move      skpz(xx)      pzona
093400070321      * imposto i giri
093500070321     c                   select
093600070321     c                   when      xx <= 6
093700070321     c                   move      skg1(xx)      pcgi
093800070321     c                   exsr      sr_decgiro
093900070321     c                   eval      pdescgi = d09odes
094000070321     c                   when      xx > 6 and xx <= 12
094100070321     c                   eval      yy = xx - 6
094200070321     c                   move      skg2(yy)      pcgi
094300070321     c                   exsr      sr_decgiro
094400070321     c                   eval      pdescgi = d09odes
094500070321     c                   when      xx > 12 and xx <= 18
094600070321     c                   eval      yy = xx - 12
094700070321     c                   move      skg3(yy)      pcgi
094800070321     c                   exsr      sr_decgiro
094900070321     c                   eval      pdescgi = d09odes
095000070321     c                   other
095100070321     c                   eval      xx = yy - 18
095200070321     c                   move      skg4(yy)      pcgi
095300070321     c                   exsr      sr_decgiro
095400070321     c                   eval      pdescgi = d09odes
095500070321     c                   endsl
095600070330     c                   exsr      sr_saltopag
095700070321     c                   write     ta8505
095800070321     c                   endif
095900070321     c                   endfor
096000070321     c                   endif
096100070321
096200070321      * pulisco i rcd di stampa
096300070321     c                   clear                   ta8503
096400070321     c                   clear                   ta8504
096500070321     c                   clear                   ta8505
096600070321
096700070321      * pulisco le schiere
096800070326     c                   clear                   skg1
096900070326     c                   clear                   skg2
097000070326     c                   clear                   skg3
097100070326     c                   clear                   skg4
097200070321     c                   reset                   skpp
097300070321     c                   reset                   skpa
097400070321     c                   reset                   skpz
097500070321
097600070321     c                   endsr
097700070321
097800070321      *------------------------------------------------------------------------*
097900070321      *  decodifico il codice giro
098000070321      *------------------------------------------------------------------------*
098100070321     c     sr_decgiro    begsr
098200070321
098300070321     c                   clear                   fidg09ds
098400070321     c                   eval      d09iop0 = '001'
098500070326     c                   eval      d09ifgs = plna
098600070321     c                   eval      d09idat = *date
098700070326     c                   eval      d09icgi = pcgi
098800070321     c                   eval      kpjbu = fidg09ds
098900070321     c                   call      'FIDG09R'
099000070321     c                   parm                    kpjba
099100070321     c                   eval      fidg09ds = kpjbu
099200070321
099300070321     c                   endsr
099400070320
099500070320      *------------------------------------------------------------------------*
099600070320      *  routine iniziale
099700070320      *------------------------------------------------------------------------*
099800070320     c     *inzsr        begsr
099900070320
100000070320     c     *entry        plist
100100070320     c                   parm                    kpjba
100200070320
100300070320     c                   eval      param85 = kpjbu
100400070320
100500070320     c     *dtaara       define    §azute        azuteds
100600070320     c     *dtaara       define    §datiute      ddatiute
100700070320     c                   in(e)     *dtaara
100800070320     c                   if        %error  or rsut = *blanks
100900070320     c                   clear                   tibs34ds
101000070320     c                   call      'TIBS34R'
101100070320     c                   parm                    tibs34ds
101200070320     c                   in        *dtaara
101300070320     c                   endif
101400070320
101500070320      * reperimento delle filiali abilitate per l'utente
101600070320     c                   clear                   trul31ds
101700070320     c                   eval      i31abi = ta85abi
101800070320     c                   eval      i31cdi = dutdis
101900070320     c                   eval      i31car = dutare
102000070320     c                   eval      i31cpo = dutpou
102100070320     c                   call      'TRUL31R'
102200070320     c                   parm                    kpjba
102300070320     c                   parm                    trul31ds
102400070320     c                   if        o31pog > *zeros
102500070320     c                   movea     o31pog        skpog
102600070320     c                   endif
102700070320
102800070320      * data e ora del lavoro
102900070320     c                   time                    wtime14
103000070320     c                   move      wtime14       wdate
103100070320     c                   movel     wtime14       wtime
103200070321
103300070321      * decodifico filiale richiesta
103400070321     c                   if        ta85fil <> 999
103500070321     c     ta85fil       chain     azorg01l
103600070321     c                   if        %found(azorg01l) and orgfva = *blanks and
103700070321     c                             (orgfag = 'F' or orgfag = 'A')
103800070321     c                   eval      desfil = orgdes
103900070321     c                   endif
104000070321     c                   else
104100070321     c                   eval      desfil = 'Filiali abilitate'
104200070321     c                   endif
104300070321      * decodifico luogo richiesto
104400070326     c                   if        ta85luo <> *blanks
104500070321     c                   clear                   ds4l
104600070326     c                   eval      tblkut = 1
104700070321     c                   eval      tblcod = '4L'
104800070326     c                   movel     ta85luo       tblkey
104900070321     c     ktabel        chain     tabel00f
105000070326     c                   if        %found(tabel00f) and tblflg = *blanks
105100070326     c                   eval      ds4l = tbluni
105200070321     c                   endif
105300070326     c                   eval      desluo = §4ldes
105400070321     c                   endif
105500070321      * decodifico cliente richiesto
105600070321     c                   if        ta85ksc <> *zeros
105700070321     c                   clear                   tibs69ds
105800070321     c                   eval      i69kac = ta85ksc
105900070321     c                   call      'TIBS69R'
106000070321     c                   parm                    tibs69ds
106100070321     c                   parm                    ds_cnaco
106200070321     c                   parm                    ds_cnind
106300070321     c                   parm                    ds_cnclp
106400070321     c                   parm                    ds_fncls
106500070326     c                   if        o69err =  *blanks
106600070321     c                   eval      desksc = acorag
106700070321     c                   endif
106800070321     c                   endif
106900070321      * selezione stampa luogo 300
107000070326     c                   eval      *in02 = *off
107100070321     c                   if        ta85sel = '1'
107200070321     c                   eval      dessel = 'Cliente'
107300070321     c                   endif
107400070321     c                   if        ta85sel = '2'
107500070321     c                   eval      dessel = 'LNA'
107600070321     c                   endif
107700070326     c                   if        ta85sel = *blanks
107800070326     c                   eval      *in02 = *on
107900070326     c                   endif
108000070321      * richiesta stampa giri
108100070321     c                   if        ta85cgis = 'S'
108200070321     c                   eval      *in01 = *on
108300070321     c                   else
108400070321     c                   eval      *in01 = *off
108500070321     c                   endif
108600070326
108700070326     c     ktabel        klist
108800070326     c                   kfld                    tblkut
108900070326     c                   kfld                    tblcod
109000070326     c                   kfld                    tblkey
109100070326
109200070326     c     kfnspe        klist
109300070326     c                   kfld                    spefls
109400070326     c                   kfld                    specli
109500070326     c                   kfld                    specod
109600070326
109700070326     c     kfnsp21       klist
109800070326     c                   kfld                    specli
109900070326     c                   kfld                    specod
110000070326
110100070326     c     kfnsp2        klist
110200070326     c                   kfld                    sp2cli
110300070326     c                   kfld                    sp2cod
110400070326     c                   kfld                    sp2tpe
110500070320
110600070320     c/exec sql
110700070320     c+ set option dynusrprf = *owner, closqlcsr = *endmod
110800070320     c/end-exec
110900070320
111000070320     c                   endsr
