000100031013      *PARMS CLOSQLCSR(*ENDMOD)
000200031013      *PARMS DYNUSRPRF(*OWNER)
000300010604     H DECEDIT('0,') DATEDIT(*DMY.)
000400010620      *----------------------------------------------------*
000500031010      *
000600031010      *
       Statistiche ORM x cliente        ?
000700010605      *
000800010604      *----------------------------------------------------*
000900031013     fFnorm00f  if   e             Disk
001000031013     fAzorg01l  if   e           k Disk
001100031013     fFnacr01l  if   e           k Disk
001200031013     fFnorf01l  if   e           k Disk
001300031013     fAzcln01l  if   e           k Disk
001400031013     fTabel00f  if   e           k disk
001500041115     fTntbe01l  if   e           k disk
001600031013     fWfroc00f  o    e             Disk    Usropn
001700031013     fWfroc10f  o    e             Disk    Prefix(p_) Usropn
001800031013     fWfroc11l  if   e           k Disk    Prefix(p_) Usropn
001900031013     f                                     Rename(wfroc100:wfroc11)
002000031013     fWfroc12l  if   e           k Disk    Prefix(p_) Usropn
002100031013     f                                     Rename(wfroc100:wfroc12)
002200031013     fFior43p   o    e             Printer OfLind(*In90)
002300010614
002400010614      *----------------------------------------------------*
002500010614      *  RIEPILOGO INDICATORI
002600010614      *----------------------------------------------------*
002700031013
002800031013      * 11 - Stampa in ordine di p.o. emissione
002900031013      * 12 - Sono in sede
003000031013      * 13 - Utente EDP
003100010614      * 90 - overflow file stampa statistica
003200010605
003300010605      *----------------------------------------------------*
003400010608
003500031013     d kAcr            s                   Like(AcrCro)
003600041115     d kCod            s                   Like(TbeCod)
003700031013     d kTfp            s                   like(ClnTfp)
003800031013     d kTfa            s                   like(ClnTfa)
003900031013     d kAnn            s                   like(ClnAnn)
004000031013     d kMes            s                   like(ClnMes)
004100031013     d ware            s                   Like(RocAre)
004200031013     d wcli            s                   Like(OrmCra)
004300031013     d wdis            s                   Like(RocDis)
004400031013     d wpoe            s                   Like(OrmPoe)
004500031014     d wpoed           s                   Like(OrmPoe)
004600031013     d wpor            s                   Like(OrmPor)
004700031013     d wtot            s                   Like(RocTot)
004800031015     d w050            s                   Like(Roc050)
004900031013     d w100            s                   Like(Roc100)
005000031015     d w300            s                   Like(Roc300)
005100031013     d w390            s                   Like(Roc390)
005200031013     d w400            s                   Like(Roc400)
005300031013     d w50001          s                   Like(Roc50001)
005400031013     d w50002          s                   Like(Roc50002)
005500031013     d w50004          s                   Like(Roc50004)
005600031013     d w600            s                   Like(Roc600)
005700031013     d w900            s                   Like(Roc900)
005800031014     d w900a           s                   Like(Roc900a)
005900031014     d w900o           s                   Like(Roc900o)
006000031014     d w900t           s                   Like(Roc900t)
006100031014     d w9001           s                   Like(Roc9001)
006200031014     d w9002           s                   Like(Roc9002)
006300031013     d w910            s                   Like(Roc910)
006400031014     d w910a           s                   Like(Roc910a)
006500031014     d w910o           s                   Like(Roc910o)
006600031014     d w910t           s                   Like(Roc910t)
006700031014     d w9101           s                   Like(Roc9101)
006800031014     d w9102           s                   Like(Roc9102)
006900031014     d w99980          s                   Like(Roc99980)
007000031013     d w99991          s                   Like(Roc99991)
007100031013     d w99992          s                   Like(Roc99992)
007200031013     d w99994          s                   Like(Roc99994)
007300031013     d w99995          s                   Like(Roc99995)
007400040701     d w99996          s                   Like(Roc99996)
007500040701     d w99997          s                   Like(Roc99997)
007600031013
007700031013     d codut           s              1  0 Inz(1)
007800031013     d comman          s             80                                         valore cmd CLP
007900031013     d lenght          s             15  5 inz(80)                              lunghezza cmd CLP
008000031013     d wcli70          s              7  0
008100040701     d wcli3           s              3
008200031013     d wdate           s              8  0
008300031013     d wdtda           s               d   datfmt(*iso)
008400031013     d wdtda01         s              8  0
008500031013     d wdtda02         s              8  0
008600031013     d wgiorno         s              2  0
008700031013     d wlib            s             10
008800031013     d wokcli          s              1
008900031014     d wokdes          s              1
009000031014     d wokmit          s              1
009100031014     d wokord          s              1
009200031013     d wora            s              6  0
009300031013     d w0140           s             14  0
009400010607
009500031013     d xa              s              5i 0 inz(*zeros)
009600031013     d xb              s              5i 0 inz(*zeros)
009700020808
009800041115     d StringaSql      s           1100    Varying
009900010605
010000010605      *   S C H I E R E
010100010605
010200031014     d All             s              1    dim(5)
010300031014     d Alls            s              2    dim(5)
010400031014     d Cli             s             10  0 dim(5)
010500031015     d Cli3            s              3    dim(5)
010600040701     d Cli7            s              7  0 dim(5)
010700031013     d cmd             s             80    dim(2) ctdata perrcd(1)              comando
010800031013     d Des             s              1    dim(5)
010900031013     d Mit             s              1    dim(5)
011000031013     d Ord             s              1    dim(5)
011100031013     d Rag             s             25    dim(5)
011200041115     d skfar           s              3    dim(99)
011300041115     d skdfar          s             35    dim(99)
011400041115     d skcmr           s              2    dim(99)
011500041115     d skdcmr          s             25    dim(99)
011600010604
011700010604      *   D S   I N T E R N E / E S T E R N E
011800031013
011900031013     d clnmat          ds
012000031013     d  mat                    1     31    dim(31)
012100031013     d clnpom          ds
012200031013     d  pom                    1     31    dim(31)
012300031013
012400031013     d ds_all          ds
012500031014     d  All1                          1
012600031014     d  All2                          1
012700031014     d  All3                          1
012800031014     d  All4                          1
012900031014     d  All5                          1
013000031013
013100031013     d ds_alls         ds
013200031014     d  All1s
013300031014     d  All2s
013400031014     d  All3s
013500031014     d  All4s
013600031014     d  All5s
013700031013
013800031013     d ds_cli1         ds
013900031014     d  Cli11
014000031014     d  Cli12
014100031014     d  Cli13
014200031013
014300031013     d ds_cli2         ds
014400031013     d  Cli21
014500031013     d  Cli22
014600031013     d  Cli23
014700031013
014800031013     d ds_cli3         ds
014900031013     d  Cli31
015000031013     d  Cli32
015100031013     d  Cli33
015200031013
015300031013     d ds_cli4         ds
015400031013     d  Cli41
015500031013     d  Cli42
015600031013     d  Cli43
015700031013
015800031013     d ds_cli5         ds
015900031013     d  Cli51
016000031013     d  Cli52
016100031013     d  Cli53
016200031013
016300031013     d ds_des          ds
016400031014     d  Des1
016500031014     d  Des2
016600031014     d  Des3
016700031014     d  Des4
016800031014     d  Des5
016900031013
017000031013     d ds_mit          ds
017100031014     d  Mit1
017200031014     d  Mit2
017300031014     d  Mit3
017400031014     d  Mit4
017500031014     d  Mit5
017600031013
017700031013     d ds_ord          ds
017800031014     d  Ord1
017900031014     d  Ord2
018000031014     d  Ord3
018100031014     d  Ord4
018200031014     d  Ord5
018300031013
018400031013     d ds_rag          ds
018500031013     d  Rag1
018600031013     d  Rag2
018700031013     d  Rag3
018800031013     d  Rag4
018900031013     d  Rag5
019000010605
019100010604     d wlbdat          ds                  inz
019200010604     d  g02dat                 1      8  0
019300010604     d  g02inv                 9     16  0
019400010604     d  g02err                17     17
019500010604     d  g02tgi                18     22  0
019600031013
019700031013     d dswfroc       e ds                  extname(wfroc00f)
019800031013     d                                     inz
019900031013     d dswfroc1      e ds                  extname(wfroc10f)
020000031013     d                                     prefix(p_)
020100031013     d                                     inz
020200031010     d Fior42ds      e ds
020300031013     d FnormxxDs     e ds                  ExtName(Fnorm00f)
020400031013     d Tibs02ds      e ds
020500041115     d dFar          e ds
020600041115     d dCmr          e ds
020700031013
020800031013     d Trul26Ds      e ds
020900031013     d  skpo                  11    760  0 dim(250)
021000031013     d  skpot                761   1510  0 dim(250)
021100031013
021200031010     d Tibs34ds      e ds                  Inz
021300031010     d dDatiute      e ds
021400031010     d Azuteds       e ds                  Extname(Azute00f)
021500031010     d Kpjba         e ds
021600010604
021700010604      *--------------------------------------------------------------*
021800010604
021900010608      * stampo la videata delle selezioni
022000031013      *                            =========
022100031013     c                   ExSr      Selezioni
022200031013      *                            =========
022300031013
022400031013      * sql per selezionare gli ORM
022500031013      *                            =========
022600031013     c                   ExSr      SqlOrmPos
022700031013      *                            =========
022800031013
022900031013      * leggo il file degli ORM
023000031013      *                            =====
023100031013     c                   ExSr      Leggi
023200031013      *                            =====
023300031013
023400010621      * stampo
023500031013      *                            ======
023600031013     c                   ExSr      Stampa
023700031013      *                            ======
023800010605
023900010608      * fine programma
024000031013     c                   Eval      *InLr = *On
024100010605
024200010604      *--------------------------------------------------------------*
024300031013      * STAMPA LE SELEZIONI
024400010604      *--------------------------------------------------------------*
024500031013     c     Selezioni     BegSr
024600010618
024700010618      * stampo la prima pagina con le selezioni
024800031013if  1c                   If        i42Poe = 999
024900070418     c                   Eval      di42poe = 'Tutte le fil.'
025000031013   x1c                   Else
025100031013     c     i42Poe        Chain     Azorg01l
025200031013if  2c                   If        %Found(Azorg01l)
025300031014     c                   Movel     OrgDes        di42poe
025400031013    2c                   EndIf
025500031013    1c                   EndIf
025600031013
025700031013if  1c                   If        i42Por = 999
025800070418     c                   Eval      di42por = 'Tutte le fil.'
025900031013   x1c                   Else
026000031013     c     i42Por        Chain     Azorg01l
026100031013if  2c                   If        %Found(Azorg01l)
026200031014     c                   Movel     OrgDes        di42por
026300031013    2c                   EndIf
026400031013    1c                   EndIf
026500010618
026600031013     c                   Clear                   wlbdat
026700031013     c                   Move      i42dtda       g02inv
026800031013     c                   Movel     '3'           G02err
026900031013     c                   Call      'XSRDA8'
027000031013     c                   Parm                    wlbdat
027100031015     c                   Movel     g02dat        datadal
027200010618
027300031013     c                   Clear                   wlbdat
027400031013     c                   Move      i42dtal       g02inv
027500031013     c                   Movel     '3'           G02err
027600031013     c                   Call      'XSRDA8'
027700031013     c                   Parm                    wlbdat
027800031013     c                   Movel     g02dat        dataal
027900031013
028000031013if  1c                   If        i42Tco <> *Blanks
028100031013     c                   Clear                   Tibs02ds
028200031013     c                   Eval      T02Mod = 'C'
028300031013     c                   Eval      T02Sif = Knsif
028400031013     c                   Eval      T02Cod = 'TCO'
028500031014     c                   Movel(p)  i42Tco        T02Ke1
028600031013     c                   Call      'TIBS02R'
028700031013     c                   Parm                    Kpjba
028800031013     c                   Parm                    Tibs02ds
028900031013     c                   Movel     T02Uni        destco
029000031013    1c                   EndIf
029100031013
029200031013     c                   If        i42File = 'S'
029300031013     c                   Eval      file = 'SI'
029400031013     c                   Else
029500031013     c                   Eval      file = 'NO'
029600031013     c                   EndIf
029700031013
029800031014     c                   Move      ds_cli1       cli(1)
029900031014     c                   Move      ds_cli2       cli(2)
030000031014     c                   Move      ds_cli3       cli(3)
030100031014     c                   Move      ds_cli4       cli(4)
030200031014     c                   Move      ds_cli5       cli(5)
030300040701     c                   Movel     ds_cli1       cli7(1)
030400040701     c                   Movel     ds_cli2       cli7(2)
030500040701     c                   Movel     ds_cli3       cli7(3)
030600040701     c                   Movel     ds_cli4       cli7(4)
030700040701     c                   Movel     ds_cli5       cli7(5)
030800031015     c                   Move      Cli13         cli3(1)
030900031015     c                   Move      Cli23         cli3(2)
031000031015     c                   Move      Cli33         cli3(3)
031100031015     c                   Move      Cli43         cli3(4)
031200031015     c                   Move      Cli53         cli3(5)
031300031013     c                   Movea     ds_all        all
031400031013     c                   Movea     ds_des        des
031500031013     c                   Movea     ds_mit        mit
031600031013     c                   Movea     ds_ord        ord
031700031013     c                   Clear                   Rag
031800031013     c                   Clear                   Alls
031900031013do  1c                   Do        5             xa
032000031013     c                   ExSr      Sr_DecoCli
032100031013    1c                   EndDo
032200031014
032300031014     c                   Movea     alls          ds_alls
032400031014     c                   Movea     rag           ds_rag
032500031015     c                   Move      cli3(1)       Cli13
032600031015     c                   Move      cli3(2)       Cli23
032700031015     c                   Move      cli3(3)       Cli33
032800031015     c                   Move      cli3(4)       Cli43
032900031015     c                   Move      cli3(5)       Cli53
033000041115
033100041115      * Stampo le fasi e le causali (solo 15 x ora bastano)
033200041115     c                   Eval      Far01 = skFar(1)
033300041115     c                   Eval      dFar01 = skdFar(1)
033400041115     c                   Eval      Far02 = skFar(2)
033500041115     c                   Eval      dFar02 = skdFar(2)
033600041115     c                   Eval      Far03 = skFar(3)
033700041115     c                   Eval      dFar03 = skdFar(3)
033800041115     c                   Eval      Far04 = skFar(4)
033900041115     c                   Eval      dFar04 = skdFar(4)
034000041115     c                   Eval      Far05 = skFar(5)
034100041115     c                   Eval      dFar05 = skdFar(5)
034200041115     c                   Eval      Far06 = skFar(6)
034300041115     c                   Eval      dFar06 = skdFar(6)
034400041115     c                   Eval      Far07 = skFar(7)
034500041115     c                   Eval      dFar07 = skdFar(7)
034600041115     c                   Eval      Far08 = skFar(8)
034700041115     c                   Eval      dFar08 = skdFar(8)
034800041115     c                   Eval      Far09 = skFar(9)
034900041115     c                   Eval      dFar09 = skdFar(9)
035000041115     c                   Eval      Far10 = skFar(10)
035100041115     c                   Eval      dFar10 = skdFar(10)
035200041115     c                   Eval      Far11 = skFar(11)
035300041115     c                   Eval      dFar11 = skdFar(11)
035400041115     c                   Eval      Far12 = skFar(12)
035500041115     c                   Eval      dFar12 = skdFar(12)
035600041115     c                   Eval      Far13 = skFar(13)
035700041115     c                   Eval      dFar13 = skdFar(13)
035800041115     c                   Eval      Far14 = skFar(14)
035900041115     c                   Eval      dFar14 = skdFar(14)
036000041115     c                   Eval      Far15 = skFar(15)
036100041115     c                   Eval      dFar15 = skdFar(15)
036200041115
036300041115     c                   Eval      Cmr01 = skCmr(1)
036400041115     c                   Eval      dCmr01 = skdCmr(1)
036500041115     c                   Eval      Cmr02 = skCmr(2)
036600041115     c                   Eval      dCmr02 = skdCmr(2)
036700041115     c                   Eval      Cmr03 = skCmr(3)
036800041115     c                   Eval      dCmr03 = skdCmr(3)
036900041115     c                   Eval      Cmr04 = skCmr(4)
037000041115     c                   Eval      dCmr04 = skdCmr(4)
037100041115     c                   Eval      Cmr05 = skCmr(5)
037200041115     c                   Eval      dCmr05 = skdCmr(5)
037300041115     c                   Eval      Cmr06 = skCmr(6)
037400041115     c                   Eval      dCmr06 = skdCmr(6)
037500041115     c                   Eval      Cmr07 = skCmr(7)
037600041115     c                   Eval      dCmr07 = skdCmr(7)
037700041115     c                   Eval      Cmr08 = skCmr(8)
037800041115     c                   Eval      dCmr08 = skdCmr(8)
037900041115     c                   Eval      Cmr09 = skCmr(9)
038000041115     c                   Eval      dCmr09 = skdCmr(9)
038100041115     c                   Eval      Cmr10 = skCmr(10)
038200041115     c                   Eval      dCmr10 = skdCmr(10)
038300041115     c                   Eval      Cmr11 = skCmr(11)
038400041115     c                   Eval      dCmr11 = skdCmr(11)
038500041115     c                   Eval      Cmr12 = skCmr(12)
038600041115     c                   Eval      dCmr12 = skdCmr(12)
038700041115     c                   Eval      Cmr13 = skCmr(13)
038800041115     c                   Eval      dCmr13 = skdCmr(13)
038900041115     c                   Eval      Cmr14 = skCmr(14)
039000041115     c                   Eval      dCmr14 = skdCmr(14)
039100041115     c                   Eval      Cmr15 = skCmr(15)
039200041115     c                   Eval      dCmr15 = skdCmr(15)
039300041115
039400031013     c                   Write     or43s1
039500010618
039600031013     c                   EndSr
039700031013
039800031013      *--------------------------------------------------------------*
039900031013      * SQL  - Preparazione sql FNORM00F
040000031013      *--------------------------------------------------------------*
040100031013     c     SqlOrmPos     BegSr
040200031013
040300031013      * Preparo la stringa sql
040400031013     c                   Eval      StringaSql =
040500031014     c                             'select * from fnorm00f '                   +
040600031014     c                             'where ormatb = '' '' and '                +
040700031014     c                             'ormpoe in ('
040800031013      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
040900031013     c                   Do        250           Xa
041000031013     c                   If        Skpo(Xa) = *Zeros
041100031013     c                   Leave
041200031013     c                   EndIf
041300031013     c                   Eval      StringaSql =
041400031014     c                             %trim(StringaSql) +  ' '                    +
041500031013     c                             %editc(Skpo(Xa):'1') + ','
041600031013     c                   EndDo
041700031013     c                   Eval      StringaSql =
041800031014     c                             %trim(StringaSql) + ')'
041900031014      * solo il range delle date immissione richiesto
042000031014     c                   Eval      StringaSql =
042100031014     c                             %trim(StringaSql) + ' '                     +
042200031014     c                             'and ormdao >= '
042300031014     c                   Eval      StringaSql =
042400031014     c                             %trim(StringaSql) + ' '                     +
042500031014     c                             %editc(i42Dtda:'4')
042600031014     c                   Eval      StringaSql =
042700031014     c                             %trim(StringaSql) + ' '                     +
042800031014     c                             'and ormdao <= '
042900031014     c                   Eval      StringaSql =
043000031014     c                             %trim(StringaSql) + ' '                     +
043100031014     c                             %editc(i42Dtal:'4')
043200031014      * ordinato per p.o. emissione e p.o. ritiro
043300031014     c                   Eval      StringaSql =
043400031014     c                             %trim(StringaSql) + ' '                     +
043500031014     c                             'order by ormpoe, ormpor for fetch only'
043600031013
043700031013     C/EXEC SQL
043800031013     C+ prepare S1 from :StringaSql
043900031013     C/END-EXEC
044000031013
044100031013     C/EXEC SQL
044200031013     C+ declare C1 cursor for S1
044300031013     C/END-EXEC
044400031013
044500031013     c                   EndSr
044600031013
044700031013      *--------------------------------------------------------------*
044800031013      * LEGGO IL FILE DEGLI ORM
044900031013      *--------------------------------------------------------------*
045000031013     c     Leggi         BegSr
045100031013
045200031013     C/EXEC SQL
045300031013     C+ Open C1
045400031013     C/END-EXEC
045500031013
045600031013     C/EXEC SQL
045700031013     C+ Fetch C1 into :fnormxxds
045800031013     C/END-EXEC
045900031013
046000031014do  1c                   Dow       Sqlcod = 0                                   *record letto
046100031013
046200031014      * p.o. ritiro
046300031013     c                   If        i42Por <> 999 and OrmPor <> i42Por
046400031014     c                   GoTo      leggircd
046500031013     c                   EndIf
046600031013      * x tipo comunicazione
046700031013     c                   If        i42Tco <> *Blanks and OrmTco <> i42Tco
046800031014     c                   GoTo      leggircd
046900031013     c                   EndIf
047000040701
047100040701      * Controllo se cliente valido
047200040701      * Mittente
047300040701if  2c                   If        OrmCra <> *Zeros
047400040701     c                   Eval      xb = 1
047500040701     c                   Movel     OrmCra        wcli70
047600040701     c                   Move      OrmCra        wcli3
047700040701      * tabella clienti selezionati
047800040701     c     wcli70        Lookup    cli7(xb)                               30
047900040701      * scelto flag di cliente mittente
048000040701if  3c   30              If        mit(xb) <> *Blanks
048100040701      * tutti i luoghi
048200040701if  4c                   If        all(xb) = 'S'
048300040701     c                   Goto      okcli
048400040701   x4c                   Else
048500040701      * solo luogo richiesto
048600040701     c                   If        wcli3 = cli3(xb)
048700040701     c                   Goto      okcli
048800040701    5c                   EndIf
048900040701    4c                   EndIf
049000040701    3c                   EndIf
049100040701e   2c                   EndIf
049200040701
049300040701      * Ordinante
049400040701if  2c                   If        OrmCor <> *Zeros
049500040701     c                   Eval      xb = 1
049600040701     c                   Movel     OrmCor        wcli70
049700040701     c                   Move      OrmCor        wcli3
049800040701      * tabella clienti selezionati
049900040701     c     wcli70        Lookup    cli7(xb)                               30
050000040701      * scelto flag di cliente ordinante
050100040701if  3c   30              If        ord(xb) <> *Blanks
050200040701      * tutti i luoghi
050300040701if  4c                   If        all(xb) = 'S'
050400040701     c                   Goto      okcli
050500040701   x4c                   Else
050600040701      * solo luogo richiesto
050700040701     c                   If        wcli3 = cli3(xb)
050800040701     c                   Goto      okcli
050900040701    5c                   EndIf
051000040701    4c                   EndIf
051100040701    3c                   EndIf
051200040701e   2c                   EndIf
051300040701
051400040701      * Destinatario
051500040701if  2c                   If        OrmCrc <> *Zeros
051600040701     c                   Eval      xb = 1
051700040701     c                   Movel     OrmCrc        wcli70
051800040701     c                   Move      OrmCrc        wcli3
051900040701      * tabella clienti selezionati
052000040701     c     wcli70        Lookup    cli7(xb)                               30
052100040701      * scelto flag di cliente destinatario
052200040701if  3c   30              If        des(xb) <> *Blanks
052300040701      * tutti i luoghi
052400040701if  4c                   If        all(xb) = 'S'
052500040701     c                   Goto      okcli
052600040701   x4c                   Else
052700040701      * solo luogo richiesto
052800040701     c                   If        wcli3 = cli3(xb)
052900040701     c                   Goto      okcli
053000040701    5c                   EndIf
053100040701    4c                   EndIf
053200040701    3c                   EndIf
053300040701e   2c                   EndIf
053400040701
053500040701      * se arrivo qua vuol dire che il cliente non è giusto
053600040701     c                   Goto      leggircd
053700031013
053800040701     c     okcli         Tag
053900031013     c                   Exsr      Sr_Elabora
054000031014
054100031014     c     leggircd      Tag
054200031013
054300031013     C/EXEC SQL
054400031013     C+ Fetch C1 into :fnormxxds
054500031013     C/END-EXEC
054600031013
054700031013    1c                   EndDo
054800031013
054900031013     C/EXEC SQL
055000031013     C+ close C1
055100031013     C/END-EXEC
055200031013
055300031013      * ultimo p.o. ritiro
055400031013      * Scrivo il file di appoggio
055500031013     c                   If        wpor > *Zeros
055600031013     c                   ExSr      Sr_Scrivi
055700031013     c                   EndIf
055800031013
055900031013     c                   EndSr
056000031013
056100031013      *--------------------------------------------------------------*
056200031013      * STAMPA
056300031013      *--------------------------------------------------------------*
056400031013     c     Stampa        BegSr
056500031013
056600031013      * leggo file di comodo per la stampa
056700031013     c  n11*loval        Setll     Wfroc11l
056800031013     c   11*loval        Setll     Wfroc12l
056900031013do  1c                   Do        *Hival
057000031013     c  n11              Read      Wfroc11l
057100031013     c   11              Read      Wfroc12l
057200031013     c                   If        %Eof
057300031013     c                   Leave
057400031013     c                   EndIf
057500031014      * scrivo il record nel file se richiesta la creazione
057600031014     c                   If        i42File = 'S'
057700031014     c                   Eval      dswfroc = dswfroc1
057800031014     c                   Write     Wfroc000
057900031014     c                   EndIf
058000031013
058100041216      * Rottura x --> P.O. EMISSIONE
058200031014if  2c                   If        p_RocPoe <> wpoe
058300041216      * --> Stampo i totali
058400041216if  3c                   If        wpoe <> *Zeros
058500041216      *     totale area
058600041216     c                   Write     or43ta
058700041216     c                   clear                   or43ta
058800041216      *     totale distretto
058900041216     c                   Write     or43td
059000041216     c                   clear                   or43td
059100041216e   3c                   EndIf
059200031014      * p.o. emissione
059300031013     c     p_RocPoe      Chain     Azorg01l
059400031013     c                   If        %Found(Azorg01l)
059500031014     c                   Movel     OrgDes        despoe
059600031013     c                   EndIf
059700031014      * distretto
059800031014     c                   Clear                   descrdis
059900031014     c                   Movel     '17'          TblCod
060000031014     c                   Movel(p)  p_RocDis      TblKey
060100031014     c     kTab          Chain     Tabel00f
060200031014     c                   If        %Found(Tabel00f)
060300031014     c                   Movel     TblUni        descrdis
060400031014     c                   EndIf
060500031014      * area
060600031014     c                   Clear                   descrare
060700031014     c                   Movel     '05'          TblCod
060800031014     c                   Movel(p)  p_RocAre      TblKey
060900031014     c     kTab          Chain     Tabel00f
061000031014     c                   If        %Found(Tabel00f)
061100031014     c                   Movel     TblUni        descrare
061200031014     c                   EndIf
061300041216      * --> Stampa testata
061400031013     c                   Write     or43t1
061500031015     c                   Write     or43t2
061600031014     c                   Write     or43t4
061700031014      * salvo i dati
061800031013     c                   Eval      wpoe = p_RocPoe
061900031014     c                   Eval      wdis = p_RocDis
062000031014     c                   Eval      ware = p_RocAre
062100031016      * spengo indicatore di salto pagina
062200031016     c                   Eval      *In90 = *Off
062300031014    2c                   EndIf
062400041216
062500041216      * Rottura per --> DISTRETTO
062600031014if  2c                   if        wdis <> p_RocDis
062700041216      * --> Stampo i totali
062800041216    3c                   If        wdis <> *Blanks
062900041216      *     totale area
063000041216     c                   Write     or43ta
063100041216     c                   clear                   or43ta
063200041216      *     totale distretto
063300041216     c                   Write     or43td
063400041216     c                   clear                   or43td
063500041216    3c                   EndIf
063600031013      * distretto
063700031014     c                   Clear                   descrdis
063800031014     c                   Movel     '17'          TblCod
063900031014     c                   Movel(p)  p_RocDis      TblKey
064000031014     c     kTab          Chain     Tabel00f
064100031014     c                   If        %Found(Tabel00f)
064200031014     c                   Movel     TblUni        descrdis
064300031014     c                   EndIf
064400031013      * area
064500031014     c                   Clear                   descrare
064600031014     c                   Movel     '05'          TblCod
064700031014     c                   Movel(p)  p_RocAre      TblKey
064800031014     c     kTab          Chain     Tabel00f
064900031014     c                   If        %Found(Tabel00f)
065000031014     c                   Movel     TblUni        descrare
065100031014     c                   EndIf
065200041216      * --> Stampa testata
065300031014     c                   Write     or43t1
065400031015     c                   Write     or43t2
065500031014     c                   write     or43t4
065600041216      * salvo i dati
065700031014     c                   Eval      wdis = p_RocDis
065800031014     c                   Eval      ware = p_RocAre
065900031016      * spengo indicatore di salto pagina
066000031016     c                   Eval      *In90 = *Off
066100031014    2c                   EndIf
066200041216
066300041216      * Rottura per --> AREA
066400031014if  2c                   If        ware <> p_RocAre
066500041216      * totale x area
066600041216if  3c                   If        ware <> *Zeros
066700041216     c                   Write     or43ta
066800041216     c                   clear                   or43ta
066900041216e   3c                   EndIf
067000041216      * salto pagina
067100041216if  2c                   If        *In90 = *On
067200041216     c                   Write     or43t1
067300041216     c                   Write     or43t2
067400041216     c                   Eval      *In90 = *Off
067500041216    2c                   EndIf
067600031013      * area
067700031014     c                   Clear                   descrare
067800031014     c                   Movel     '05'          TblCod
067900031014     c                   Movel(p)  p_RocAre      TblKey
068000031014     c     kTab          Chain     Tabel00f
068100031014     c                   If        %Found(Tabel00f)
068200031014     c                   Movel     TblUni        descrare
068300031014     c                   EndIf
068400031014     c                   Eval      ware = p_RocAre
068500031014    2c                   EndIf
068600031013
068700031013      * salto pagina
068800031014if  2c                   If        *In90 = *On
068900031013     c                   Write     or43t1
069000031015     c                   Write     or43t2
069100031013     c                   Write     or43t4
069200031013     c                   Eval      *In90 = *Off
069300031014    2c                   EndIf
069400031013
069500031014      * decodifico il p.o. ritiro
069600031014     c     p_RocPor      Chain     Azorg01l
069700031014     c                   If        %Found(Azorg01l)
069800031014     c                   Movel     OrgDes        despor
069900031014     c                   EndIf
070000041115
070100041115      * sommo i totali x il totale area
070200041115     c                   add       p_RocTot      taTot
070300041115     c                   add       p_Roc900      ta900
070400041115     c                   add       p_Roc910      ta910
070500041115     c                   add       p_Roc600      ta600
070600041115     c                   add       p_Roc99980    ta99980
070700041115     c                   add       p_Roc99991    ta99991
070800041115     c                   add       p_Roc99992    ta99992
070900041115     c                   add       p_Roc99994    ta99994
071000041115     c                   add       p_Roc99995    ta99995
071100041115     c                   add       p_Roc99996    ta99996
071200041115     c                   add       p_Roc99997    ta99997
071300041115     c                   add       p_Roc50001    ta50001
071400041115     c                   add       p_Roc50002    ta50002
071500041115     c                   add       p_Roc50004    ta50004
071600041115     c                   add       p_Roc400      ta400
071700041115     c                   add       p_Roc390      ta390
071800041115     c                   add       p_Roc300      ta300
071900041115     c                   add       p_Roc100      ta100
072000041115     c                   add       p_Roc050      ta050
072100041115      * sommo i totali x il totale distretto
072200041115     c                   add       p_RocTot      tdTot
072300041115     c                   add       p_Roc900      td900
072400041115     c                   add       p_Roc910      td910
072500041115     c                   add       p_Roc600      td600
072600041115     c                   add       p_Roc99980    td99980
072700041115     c                   add       p_Roc99991    td99991
072800041115     c                   add       p_Roc99992    td99992
072900041115     c                   add       p_Roc99994    td99994
073000041115     c                   add       p_Roc99995    td99995
073100041115     c                   add       p_Roc99996    td99996
073200041115     c                   add       p_Roc99997    td99997
073300041115     c                   add       p_Roc50001    td50001
073400041115     c                   add       p_Roc50002    td50002
073500041115     c                   add       p_Roc50004    td50004
073600041115     c                   add       p_Roc400      td400
073700041115     c                   add       p_Roc390      td390
073800041115     c                   add       p_Roc300      td300
073900041115     c                   add       p_Roc100      td100
074000041115     c                   add       p_Roc050      td050
074100041115      * sommo i totali x il totale generale
074200041115     c                   add       p_RocTot      tgTot
074300041115     c                   add       p_Roc900      tg900
074400041115     c                   add       p_Roc910      tg910
074500041115     c                   add       p_Roc600      tg600
074600041115     c                   add       p_Roc99980    tg99980
074700041115     c                   add       p_Roc99991    tg99991
074800041115     c                   add       p_Roc99992    tg99992
074900041115     c                   add       p_Roc99994    tg99994
075000041115     c                   add       p_Roc99995    tg99995
075100041115     c                   add       p_Roc99996    tg99996
075200041115     c                   add       p_Roc99997    tg99997
075300041115     c                   add       p_Roc50001    tg50001
075400041115     c                   add       p_Roc50002    tg50002
075500041115     c                   add       p_Roc50004    tg50004
075600041115     c                   add       p_Roc400      tg400
075700041115     c                   add       p_Roc390      tg390
075800041115     c                   add       p_Roc300      tg300
075900041115     c                   add       p_Roc100      tg100
076000041115     c                   add       p_Roc050      tg050
076100031014
076200031014      * stampo i totali
076300031014     c                   Write     or43d1
076400031014      * stampo affidabilità
076500031014if  2c                   If        p_Roc900a > *Zeros or p_Roc910a > *Zeros or
076600031014     c                             p_Roc900t > *Zeros or p_Roc910t > *Zeros or
076700031014     c                             p_Roc9001 > *Zeros or p_Roc9101 > *Zeros or
076800031014     c                             p_Roc9002 > *Zeros or p_Roc9102 > *Zeros or
076900031014     c                             p_Roc900o > *Zeros or p_Roc910o > *Zeros
077000031014     c                   Write     or43a1
077100031014
077200031014     c                   If        p_Roc900a > *Zeros or p_Roc910a > *Zeros
077300031014     c                   Write     or43a11
077400031014     c                   EndIf
077500031014     c                   If        p_Roc900t > *Zeros or p_Roc910t > *Zeros
077600031014     c                   Write     or43a12
077700031014     c                   EndIf
077800031014     c                   If        p_Roc9001 > *Zeros or p_Roc9101 > *Zeros
077900031014     c                   Write     or43a13
078000031014     c                   EndIf
078100031014     c                   If        p_Roc9002 > *Zeros or p_Roc9102 > *Zeros
078200031014     c                   Write     or43a14
078300031014     c                   EndIf
078400031014     c                   If        p_Roc900o > *Zeros or p_Roc910o > *Zeros
078500031014     c                   Write     or43a15
078600031014     c                   EndIf
078700031014    2c                   EndIf
078800031013
078900031014    1c                   EndDo
079000041115      * Totale area
079100041115     c                   Write     or43ta
079200041115      * Totale distretto
079300041115     c                   Write     or43td
079400041115      * Totale generale
079500041216      * a pagina nuova
079600041216     c                   Write     or43t1
079700041216     c                   Write     or43t4
079800041115     c                   Write     or43tg
079900031013
080000031013      * fine stampa
080100031014     c                   Write     or43e1
080200031013
080300031014     c                   EndSr
080400031013
080500031013      *---------------------------------------------------------------*
080600031013      * DECODIFICA CLIENTI
080700031013      *---------------------------------------------------------------*
080800031013     c     Sr_DecoCli    BegSr
080900031013
081000031015if  1c                   If        Cli(xa) <> *Zeros
081100031015      * se richiesto tutti i luoghi cerco se almeno 1 esiste
081200031015if  2c                   If        all(xa) = 'S'
081300031015     c                   Eval      alls(xa) = 'SI'
081400031015     c                   Clear                   cli3(xa)
081500031015     c                   Move      cli(xa)       kAcr
081600031013     c                   Move      000           kAcr
081700031013     c     kAcr          Setll     Fnacr01l
081800031015do  3c                   Do        *Hival
081900031013     c                   Read      Fnacr01l
082000031013     c                   If        %Eof(Fnacr01l)
082100031013     c                   Leave
082200031013     c                   EndIf
082300031015     c                   Movel     AcrRsr        rag(xa)
082400031013     c                   Leave
082500031015    3c                   EndDo
082600031013      * se no cerco il cliente specifico
082700031015   x2c                   Else
082800031015     c                   Eval      alls(xa) = 'NO'
082900031015     c                   Move      cli(xa)       kAcr
083000031013     c     kAcr          Chain     Fnacr01l
083100031013     c                   If        %Found(Fnacr01l)
083200031015     c                   Movel     AcrRsr        rag(xa)
083300031013     c                   EndIf
083400031015    2c                   EndIf
083500031015   x1c                   Else
083600031015     c                   Clear                   cli3(xa)
083700031015    1c                   EndIf
083800031013
083900031013     c                   EndSr
084000031013
084100031013      *--------------------------------------------------------------*
084200031013      * ELABORO I DATI LETTI
084300031013      *--------------------------------------------------------------*
084400031013     c     Sr_Elabora    BegSr
084500031013
084600031013      * Totali x  p.o. ritiro
084700031013if  1c                   If        OrmPor <> wpor
084800031013     c                             and wpor > *Zeros
084900031013      * Scrivo il file di appoggio
085000031013     c                   ExSr      Sr_Scrivi
085100031013     c                   Eval      wpor = OrmPor
085200031013    1c                   EndIf
085300031013
085400031013     c                   add       1             wtot
085500031013
085600031013      * calcolo le date x affidabilità in base alla data ritiro
085700031013     c                   ExSr      Sr_Date
085800031013
085900031013      * Fase 900
086000031014if  1c                   If        OrmFao = 900
086100031013     c                   add       1             w900
086200031013      * calcolo affidabilità fase 900
086300031013     c                   Select
086400031013     c                   When      OrmDfo < OrmDar
086500031013     c                   add       1             w900a
086600031014     c                   When      OrmDfo = OrmDar
086700031013     c                   add       1             w900t
086800031015     c                   When      OrmDfo > wdtda02
086900031015     c                   add       1             w900o
087000031015     c                   when      OrmDfo > wdtda01 and OrmDfo <= wdtda02
087100031015     c                   add       1             w9002
087200031015     c                   Other
087300031013     c                   add       1             w9001
087400031013     c                   endsl
087500031013    1c                   EndIf
087600031013
087700031013      * Fase 910
087800031014if  1c                   If        OrmFao = 910
087900031013     c                   add       1             w910
088000031013      * calcolo affidabilità fase 910
088100031013     c                   Select
088200031013     c                   When      OrmDfo < OrmDar
088300031014     c                   add       1             w910a
088400031014     c                   When      OrmDfo = OrmDar
088500031014     c                   add       1             w910t
088600031015     c                   When      OrmDfo > wdtda02
088700031015     c                   add       1             w910o
088800031015     c                   when      OrmDfo > wdtda01 and OrmDfo <= wdtda02
088900031015     c                   add       1             w9102
089000031015     c                   Other
089100031014     c                   add       1             w9101
089200031013     c                   endsl
089300031013    1c                   EndIf
089400031013
089500031013      * Fase 600
089600031014     c                   If        OrmFao = 600
089700031013     c                   add       1             w600
089800031013     c                   EndIf
089900031013
090000031013      * Fase 999 o 500
090100031014if  1c                   If        OrmFao = 999 or OrmFao = 500
090200031013     c     kOrf          Chain     Fnorf01l
090300031013if  2c                   If        %Found(Fnorf01l)
090400031014     c                   If        OrfCar = '80'
090500031014     c                   add       1             w99980
090600031014     c                   EndIf
090700031013     c                   If        OrfCar = '91'
090800031013     c                   add       1             w99991
090900031013     c                   EndIf
091000031013     c                   If        OrfCar = '92'
091100031013     c                   add       1             w99992
091200031013     c                   EndIf
091300031013     c                   If        OrfCar = '94'
091400031013     c                   add       1             w99994
091500031013     c                   EndIf
091600031013     c                   If        OrfCar = '95'
091700031013     c                   add       1             w99995
091800031013     c                   EndIf
091900040701     c                   If        OrfCar = '96'
092000040701     c                   add       1             w99996
092100040701     c                   EndIf
092200040701     c                   If        OrfCar = '97'
092300040701     c                   add       1             w99997
092400040701     c                   EndIf
092500031013     c                   If        OrfCar = '01'
092600031013     c                   add       1             w50001
092700031013     c                   EndIf
092800031013     c                   If        OrfCar = '02'
092900031013     c                   add       1             w50002
093000031013     c                   EndIf
093100031013     c                   If        OrfCar = '04'
093200031013     c                   add       1             w50004
093300031013     c                   EndIf
093400031013    2c                   EndIf
093500031013    1c                   EndIf
093600031013
093700031013      * Fase 400
093800031014     c                   If        OrmFao = 400
093900031013     c                   add       1             w400
094000031013     c                   EndIf
094100031013
094200031013      * Fase 390
094300031014     c                   If        OrmFao = 390
094400031013     c                   add       1             w390
094500031013     c                   EndIf
094600031015
094700031015      * Fase 300
094800031015     c                   If        OrmFao = 300
094900031015     c                   add       1             w300
095000031015     c                   EndIf
095100031013
095200031013      * Fase 100
095300031015     c                   If        OrmFao = 100
095400031013     c                   add       1             w100
095500031013     c                   EndIf
095600031015
095700031015      * Fase 050
095800031015     c                   If        OrmFao < 100
095900031015     c                   add       1             w050
096000031015     c                   EndIf
096100031013
096200031013      * p.o. ritiro
096300031013     c                   If        wpor = *Zeros
096400031013     c                   Eval      wpor = OrmPor
096500031013     c                   EndIf
096600031014      * p.o. emissione
096700031014     c                   Eval      wpoed = OrmPoe
096800031013
096900031013     c                   EndSr
097000031013
097100031013      *--------------------------------------------------------------*
097200031013      * CALCOLO DATE X AFFIDABILITA'
097300031013      *--------------------------------------------------------------*
097400031013     c     Sr_Date       BegSr
097500031013
097600031013     c                   Clear                   kTfa
097700031013     c                   Eval      kTfp = OrmPor
097800031013
097900031014      * calcolo la data da + 1
098000031013     c     *iso          Movel     OrmDar        wdtda
098100031013     c                   Do        *Hival
098200031014     c                   Adddur    1:*d          wdtda
098300031013     c                   Extrct    wdtda:*y      kAnn
098400031013     c                   Extrct    wdtda:*m      kMes
098500031013     c                   Extrct    wdtda:*d      wgiorno
098600031013     c     kAzcln        Chain     Azcln01l
098700031013     c                   If        Not %Found(Azcln01l)
098800031013     c                   Leave
098900031013     c                   EndIf
099000031013     c                   If        MAT(wgiorno) =  'F'
099100031013     c                             or  POM(wgiorno) =  'F'
099200031013     c                   Iter
099300031013     c                   EndIf
099400031013     c                   Leave
099500031013     c                   EndDo
099600031013     c                   move      wdtda         wdtda01
099700031013
099800031014      * calcolo la data da + 2
099900031013     c     *iso          Movel     wdtda01       wdtda
100000031013     c                   Do        *Hival
100100031014     c                   Adddur    1:*d          wdtda
100200031013     c                   Extrct    wdtda:*y      kAnn
100300031013     c                   Extrct    wdtda:*m      kMes
100400031013     c                   Extrct    wdtda:*d      wgiorno
100500031013     c     kAzcln        Chain     Azcln01l
100600031013     c                   If        Not %Found(Azcln01l)
100700031013     c                   Leave
100800031013     c                   EndIf
100900031013     c                   If        MAT(wgiorno) =  'F'
101000031013     c                             or  POM(wgiorno) =  'F'
101100031013     c                   Iter
101200031013     c                   EndIf
101300031013     c                   Leave
101400031013     c                   EndDo
101500031013     c                   move      wdtda         wdtda02
101600031013
101700031013     c                   EndSr
101800031013
101900031013      *--------------------------------------------------------------*
102000031013      * SCRIVO IL FILE DI APPOGGIO
102100031013      *--------------------------------------------------------------*
102200031013     c     Sr_Scrivi     BegSr
102300031013
102400031013     c                   Clear                   Wfroc100
102500031013
102600031014     c                   Eval      p_RocPoe = wpoed
102700031013     c     wpor          Chain     Azorg01l
102800031013     c                   If        %Found(Azorg01l)
102900031013     c                   Eval      p_RocDis = OrgFl3
103000031013     c                   Eval      p_RocAre = OrgCar
103100031013     c                   EndIf
103200031013     c                   Eval      p_RocPor = wpor
103300031014     c                   Eval      p_RocTot = wtot
103400031014     c                   Eval      p_Roc900 = w900
103500031014     c                   Eval      p_Roc900a = w900a
103600031014     c                   Eval      p_Roc900t = w900t
103700031014     c                   Eval      p_Roc9001 = w9001
103800031014     c                   Eval      p_Roc9002 = w9002
103900031014     c                   Eval      p_Roc900o = w900o
104000031014     c                   Eval      p_Roc910 = w910
104100031014     c                   Eval      p_Roc910a = w910a
104200031014     c                   Eval      p_Roc910t = w910t
104300031014     c                   Eval      p_Roc9101 = w9101
104400031014     c                   Eval      p_Roc9102 = w9102
104500031014     c                   Eval      p_Roc910o = w910o
104600031014     c                   Eval      p_Roc600 = w600
104700031014     c                   Eval      p_Roc99980 = w99980
104800031014     c                   Eval      p_Roc99991 = w99991
104900031014     c                   Eval      p_Roc99992 = w99992
105000031014     c                   Eval      p_Roc99994 = w99994
105100031014     c                   Eval      p_Roc99995 = w99995
105200040701     c                   Eval      p_Roc99996 = w99996
105300040701     c                   Eval      p_Roc99997 = w99997
105400031014     c                   Eval      p_Roc50001 = w50001
105500031014     c                   Eval      p_Roc50002 = w50002
105600031014     c                   Eval      p_Roc50004 = w50004
105700031014     c                   Eval      p_Roc400 = w400
105800031014     c                   Eval      p_Roc390 = w390
105900031015     c                   Eval      p_Roc300 = w300
106000031014     c                   Eval      p_Roc100 = w100
106100031015     c                   Eval      p_Roc050 = w050
106200031013
106300031013     c                   Write     Wfroc100
106400031013
106500031013     c                   ExSr      Sr_Pulisci
106600031013
106700031013     c                   EndSr
106800031013
106900031013      *--------------------------------------------------------------*
107000031013      * PULISCO CAMPI DI COMODO
107100031013      *--------------------------------------------------------------*
107200031013     c     Sr_Pulisci    BegSr
107300031013
107400031013     c                   Clear                   wtot
107500031013     c                   Clear                   wpor
107600031013     c                   Clear                   w900
107700031013     c                   Clear                   w900a
107800031013     c                   Clear                   w900t
107900031013     c                   Clear                   w9001
108000031013     c                   Clear                   w9002
108100031013     c                   Clear                   w900o
108200031013     c                   Clear                   w910
108300031013     c                   Clear                   w910a
108400031013     c                   Clear                   w910t
108500031013     c                   Clear                   w9101
108600031013     c                   Clear                   w9102
108700031013     c                   Clear                   w910o
108800031013     c                   Clear                   w600
108900031014     c                   Clear                   w99980
109000031013     c                   Clear                   w99991
109100031013     c                   Clear                   w99992
109200031013     c                   Clear                   w99994
109300031013     c                   Clear                   w99995
109400040701     c                   Clear                   w99996
109500040701     c                   Clear                   w99997
109600031013     c                   Clear                   w50001
109700031013     c                   Clear                   w50002
109800031013     c                   Clear                   w50004
109900031013     c                   Clear                   w400
110000031015     c                   Clear                   w390
110100031015     c                   Clear                   w300
110200031013     c                   Clear                   w100
110300031015     c                   Clear                   w050
110400031013
110500031013     c                   EndSr
110600010614
110700010604      *--------------------------------------------------------------*
110800010604      * ROUTINE INIZIALE
110900010604      *--------------------------------------------------------------*
111000031010     c     *Inzsr        BegSr
111100010604      *
111200031010     c     *Entry        Plist
111300031010     c                   Parm                    Kpjba
111400010621
111500031010     c                   Movel     Kpjbu         Fior42ds
111600010621
111700031010     c     *dtaara       define    §azute        azuteds
111800031010     c     *dtaara       define    §datiute      ddatiute
111900031010     c                   in(E)     *dtaara
112000031013     c                   If        %error  or RSUT = *blanks
112100031010     c                   CLEAR                   tibs34ds
112200031010     c                   CALL      'TIBS34R'
112300031010     c                   PARM                    tibs34ds
112400031010     c                   in        *dtaara
112500031013     c                   EndIf
112600031013
112700031013      * reperisco data e ora
112800031013     c                   Time                    w0140
112900031013      * udate in GGMMAAAA
113000031013     c                   Move      w0140         wdate
113100031013     c                   Movel     w0140         wora
113200031013
113300031013     c                   Clear                   Trul26Ds
113400031013      * Carico sk con i p.o. emissione da elaborare
113500031013if  1c                   If        i42Poe = 999
113600031013     c                   Eval      I26tpt = 'P'
113700031013     c                   Eval      I26tla = 'L'
113800031013     c                   Call      'TRUL26R'
113900031013     c                   Parm                    Trul26Ds
114000031013      * lanciato un solo p.o. carico questo nella sk
114100031013   x1c                   Else
114200031013     c                   Eval      Skpo(1) = i42Poe
114300031013    1c                   EndIf
114400031013
114500031013      * Controllo se lancio da sede
114600031013     c                   If        Simfel = *Zeros
114700031013     c                   Eval      *In12 = *On
114800031013     c                   EndIf
114900031013
115000031013      * Controllo se profilo EDP
115100031013     c                   If        %Subst(knmus:1:3) = 'EDP'
115200031013     c                   Eval      *In13 = *On
115300031013     c                   EndIf
115400031013
115500031014      * se richiesta la creazione del file
115600031013     c                   If        i42File = 'S' and
115700031013     c                             (*In12 or *In13)
115800031014      * prima devo pulirlo
115900031014     c                   Clear                   comman
116000031014     c                   Movel(p)  cmd(2)        comman
116100031014     c                   Call      'QCMDEXC'
116200031014     c                   Parm                    comman
116300031014     c                   Parm                    lenght
116400031014      * poi lo apro
116500031013     c                   Open      Wfroc00f
116600031013     c                   EndIf
116700031013
116800031013      * controllo il sistema informativo dove sono per reperire
116900031013      * la libreria
117000041112      *   ---> GAITRA201/FILTRA201 - GAITRAAZM
117100041112     c                   If        %subst(knsif:7:3) = '201'
117200041112     c                   Eval      wlib = 'GAITRAAZM '
117300031013     c                   EndIf
117400041112      *   ---> GAITRAPSP/FILTRAPFP - GAITRAAZP
117500041112     c                   If        %subst(knsif:7:3) = 'PSP' or
117600041112     c                             %subst(knsif:7:3) = 'PFP'
117700041112     c                   Eval      wlib = 'GAITRAAZP '
117800031013     c                   EndIf
117900031013
118000031013      * duplico il file di comodo in qtemp
118100031013     c                   Clear                   comman
118200031013     c                   Movel(p)  cmd(1)        comman
118300031013     c                   Eval      %Subst(comman:32:10) = wlib
118400031013     c                   Call      'QCMDEXC'
118500031013     c                   Parm                    comman
118600031013     c                   Parm                    lenght
118700031013      * apro il file di comodo per la stampa
118800031014     c                   Open      Wfroc10f
118900031013
119000031013      * se lancio fatto da sede e tutti i p.o. emissione apro l.f. in ordine di p.o. emissione
119100031013     c                   If        *In12 and i42Poe = 999
119200031013     c                   Open      Wfroc12l
119300031013     c                   Eval      *In11 = *On
119400031013      * altrimenti apro l.f. in ordine di distretto/area
119500031013     c                   Else
119600031013     c                   Open      Wfroc11l
119700031013     c                   Eval      *In11 = *Off
119800031013     c                   EndIf
119900031014
120000031014      * carico i dati nelle ds di appoggio
120100031014     c                   Move      i42Cli1       ds_cli1
120200031014     c                   Move      i42Cli2       ds_cli2
120300031014     c                   Move      i42Cli3       ds_cli3
120400031014     c                   Move      i42Cli4       ds_cli4
120500031014     c                   Move      i42Cli5       ds_cli5
120600031014     c                   Move      i42All1       all1
120700031014     c                   Move      i42All2       all2
120800031014     c                   Move      i42All3       all3
120900031014     c                   Move      i42All4       all4
121000031014     c                   Move      i42All5       all5
121100031014     c                   Move      i42Mit1       mit1
121200031014     c                   Move      i42Mit2       mit2
121300031014     c                   Move      i42Mit3       mit3
121400031014     c                   Move      i42Mit4       mit4
121500031014     c                   Move      i42Mit5       mit5
121600031014     c                   Move      i42Ord1       ord1
121700031014     c                   Move      i42Ord2       ord2
121800031014     c                   Move      i42Ord3       ord3
121900031014     c                   Move      i42Ord4       ord4
122000031014     c                   Move      i42Ord5       ord5
122100031014     c                   Move      i42Des1       des1
122200031014     c                   Move      i42Des2       des2
122300031014     c                   Move      i42Des3       des3
122400031014     c                   Move      i42Des4       des4
122500031014     c                   Move      i42Des5       des5
122600041115
122700041115      * carico la schiera delle fasi con le relative descrizioni
122800041115     c                   Clear                   xa
122900041115     c                   Eval      kCod = 'FAR'
123000041115     c     kCod          Setll     Tntbe01l
123100041115     c                   Do        *Hival
123200041115     c     kCod          Reade     Tntbe01l
123300041115     c                   If        %Eof(Tntbe01l)
123400041115     c                   Leave
123500041115     c                   EndIf
123600041115      * escludo --> le annullate
123700041115     c                   If        TbeAtb <> *Blanks
123800041115     c                   Iter
123900041115     c                   EndIf
124000041115      *         --> le non gestibili dai pgm ORM
124100041115     c                   Eval      dFar = TbeUni
124200041115     c                   If        d§FarNoo = 'S'
124300041115     c                   Iter
124400041115     c                   EndIf
124500041115      *         --> la 200 xchè mai nessun ORM sarà in fase 200
124600041115     c                   If        TbeKe1 = '200'
124700041115     c                   Iter
124800041115     c                   EndIf
124900041115     c                   add       1             xa
125000041115     c                   eval      skFar(xa) = TbeKe1
125100041115     c                   eval      skdFar(xa) = d§Fardes
125200041115     c                   EndDo
125300041115
125400041115      * carico la schiera delle causali con le relative descrizioni
125500041115     c                   Clear                   xa
125600041115     c                   Eval      kCod = 'CMR'
125700041115     c     kCod          Setll     Tntbe01l
125800041115     c                   Do        *Hival
125900041115     c     kCod          Reade     Tntbe01l
126000041115     c                   If        %Eof(Tntbe01l)
126100041115     c                   Leave
126200041115     c                   EndIf
126300041115      * escludo --> le annullate
126400041115     c                   If        TbeAtb <> *Blanks
126500041115     c                   Iter
126600041115     c                   EndIf
126700041115      *         --> le non gestibili dai pgm ORM
126800041115     c                   Eval      dCmr = TbeUni
126900041115     c                   If        d§CmrNoc = 'S' and d§CmrNoq = 'S' and
127000041115     c                             d§CmrNmf = 'S' and TbeKe1 <> '97'
127100041115     c                   Iter
127200041115     c                   EndIf
127300041115      *         --> la 82 xchè c'è già la fase 910
127400041115     c                   If        TbeKe1 = '82'
127500041115     c                   Iter
127600041115     c                   EndIf
127700041115     c                   add       1             xa
127800041115     c                   eval      skCmr(xa) = TbeKe1
127900041115     c                   eval      skdCmr(xa) = d§Cmrdes1
128000041115     c                   EndDo
128100031013
128200031013     c     kOrf          klist
128300031013     c                   kfld                    OrmPoe
128400031013     c                   kfld                    OrmNsr
128500031013     c                   kfld                    OrmNor
128600031013     c                   kfld                    OrmNrv
128700031013     c                   kfld                    OrmDfo
128800031013     c                   kfld                    OrmOfo
128900031014     c                   kfld                    OrmFao
129000031013
129100031013     c     kAzcln        klist
129200031013     c                   kfld                    ktfp
129300031013     c                   kfld                    ktfa
129400031013     c                   kfld                    kann
129500031013     c                   kfld                    kmes
129600031013
129700031013     c     kTab          klist
129800031013     c                   kfld                    codut
129900031013     c                   kfld                    TblCod
130000031013     c                   kfld                    TblKey
130100010606
130200031013     c                   EndSr
130300010606      *--------------------------------------------------------------*
130400031013**   cmd - comando
130500031013CRTDUPOBJ OBJ(WFROC1*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
130600031013CLRPFM FILE(WFROC00F)
