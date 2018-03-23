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
002300140609     f                                     infds(prtinfo)
002400010614
002500010614      *----------------------------------------------------*
002600010614      *  RIEPILOGO INDICATORI
002700010614      *----------------------------------------------------*
002800031013
002900031013      * 11 - Stampa in ordine di p.o. emissione
003000031013      * 12 - Sono in sede
003100031013      * 13 - Utente EDP
003200010614      * 90 - overflow file stampa statistica
003300010605
003400010605      *----------------------------------------------------*
003500010608
003600031013     d kAcr            s                   Like(AcrCro)
003700041115     d kCod            s                   Like(TbeCod)
003800031013     d kTfp            s                   like(ClnTfp)
003900031013     d kTfa            s                   like(ClnTfa)
004000031013     d kAnn            s                   like(ClnAnn)
004100031013     d kMes            s                   like(ClnMes)
004200031013     d ware            s                   Like(RocAre)
004300031013     d wcli            s                   Like(OrmCra)
004400031013     d wdis            s                   Like(RocDis)
004500031013     d wpoe            s                   Like(OrmPoe)
004600031014     d wpoed           s                   Like(OrmPoe)
004700031013     d wpor            s                   Like(OrmPor)
004800031013     d wtot            s                   Like(RocTot)
004900031015     d w050            s                   Like(Roc050)
005000031013     d w100            s                   Like(Roc100)
005100031015     d w300            s                   Like(Roc300)
005200031013     d w390            s                   Like(Roc390)
005300031013     d w400            s                   Like(Roc400)
005400031013     d w50001          s                   Like(Roc50001)
005500031013     d w50002          s                   Like(Roc50002)
005600031013     d w50004          s                   Like(Roc50004)
005700031013     d w600            s                   Like(Roc600)
005800031013     d w900            s                   Like(Roc900)
005900031014     d w900a           s                   Like(Roc900a)
006000031014     d w900o           s                   Like(Roc900o)
006100031014     d w900t           s                   Like(Roc900t)
006200031014     d w9001           s                   Like(Roc9001)
006300031014     d w9002           s                   Like(Roc9002)
006400031013     d w910            s                   Like(Roc910)
006500031014     d w910a           s                   Like(Roc910a)
006600031014     d w910o           s                   Like(Roc910o)
006700031014     d w910t           s                   Like(Roc910t)
006800031014     d w9101           s                   Like(Roc9101)
006900031014     d w9102           s                   Like(Roc9102)
007000031014     d w99980          s                   Like(Roc99980)
007100031013     d w99991          s                   Like(Roc99991)
007200031013     d w99992          s                   Like(Roc99992)
007300031013     d w99994          s                   Like(Roc99994)
007400031013     d w99995          s                   Like(Roc99995)
007500040701     d w99996          s                   Like(Roc99996)
007600040701     d w99997          s                   Like(Roc99997)
007700031013
007800031013     d codut           s              1  0 Inz(1)
007900031013     d comman          s             80                                         valore cmd CLP
008000031013     d lenght          s             15  5 inz(80)                              lunghezza cmd CLP
008100031013     d wcli70          s              7  0
008200040701     d wcli3           s              3
008300031013     d wdate           s              8  0
008400031013     d wdtda           s               d   datfmt(*iso)
008500031013     d wdtda01         s              8  0
008600031013     d wdtda02         s              8  0
008700031013     d wgiorno         s              2  0
008800031013     d wlib            s             10
008900140606     d wNoOkOrm        s               n   inz(*off)
009000031013     d wokcli          s              1
009100031014     d wokdes          s              1
009200031014     d wokmit          s              1
009300031014     d wokord          s              1
009400031013     d wora            s              6  0
009500031013     d w0140           s             14  0
009600010607
009700031013     d xa              s              5i 0 inz(*zeros)
009800031013     d xb              s              5i 0 inz(*zeros)
009900020808
010000140610     d StringaSql      s           4096    Varying
010100010605
010200010605      *   S C H I E R E
010300010605
010400031014     d All             s              1    dim(5)
010500031014     d Alls            s              2    dim(5)
010600031014     d Cli             s             10  0 dim(5)
010700031015     d Cli3            s              3    dim(5)
010800040701     d Cli7            s              7  0 dim(5)
010900031013     d cmd             s             80    dim(2) ctdata perrcd(1)              comando
011000031013     d Des             s              1    dim(5)
011100031013     d Mit             s              1    dim(5)
011200031013     d Ord             s              1    dim(5)
011300031013     d Rag             s             25    dim(5)
011400041115     d skfar           s              3    dim(99)
011500041115     d skdfar          s             35    dim(99)
011600041115     d skcmr           s              2    dim(99)
011700041115     d skdcmr          s             25    dim(99)
011800010604
011900010604      *   D S   I N T E R N E / E S T E R N E
012000031013
012100031013     d clnmat          ds
012200031013     d  mat                    1     31    dim(31)
012300031013     d clnpom          ds
012400031013     d  pom                    1     31    dim(31)
012500031013
012600031013     d ds_all          ds
012700031014     d  All1                          1
012800031014     d  All2                          1
012900031014     d  All3                          1
013000031014     d  All4                          1
013100031014     d  All5                          1
013200031013
013300031013     d ds_alls         ds
013400031014     d  All1s
013500031014     d  All2s
013600031014     d  All3s
013700031014     d  All4s
013800031014     d  All5s
013900031013
014000031013     d ds_cli1         ds
014100031014     d  Cli11
014200031014     d  Cli12
014300031014     d  Cli13
014400031013
014500031013     d ds_cli2         ds
014600031013     d  Cli21
014700031013     d  Cli22
014800031013     d  Cli23
014900031013
015000031013     d ds_cli3         ds
015100031013     d  Cli31
015200031013     d  Cli32
015300031013     d  Cli33
015400031013
015500031013     d ds_cli4         ds
015600031013     d  Cli41
015700031013     d  Cli42
015800031013     d  Cli43
015900031013
016000031013     d ds_cli5         ds
016100031013     d  Cli51
016200031013     d  Cli52
016300031013     d  Cli53
016400031013
016500031013     d ds_des          ds
016600031014     d  Des1
016700031014     d  Des2
016800031014     d  Des3
016900031014     d  Des4
017000031014     d  Des5
017100031013
017200031013     d ds_mit          ds
017300031014     d  Mit1
017400031014     d  Mit2
017500031014     d  Mit3
017600031014     d  Mit4
017700031014     d  Mit5
017800031013
017900031013     d ds_ord          ds
018000031014     d  Ord1
018100031014     d  Ord2
018200031014     d  Ord3
018300031014     d  Ord4
018400031014     d  Ord5
018500031013
018600031013     d ds_rag          ds
018700031013     d  Rag1
018800031013     d  Rag2
018900031013     d  Rag3
019000031013     d  Rag4
019100031013     d  Rag5
019200010605
019300010604     d wlbdat          ds                  inz
019400010604     d  g02dat                 1      8  0
019500010604     d  g02inv                 9     16  0
019600010604     d  g02err                17     17
019700010604     d  g02tgi                18     22  0
019800140609
019900140609     d PrtINFO         ds
020000140609     d  CurLine              367    368i 0
020100031013
020200031013     d dswfroc       e ds                  extname(wfroc00f)
020300031013     d                                     inz
020400031013     d dswfroc1      e ds                  extname(wfroc10f)
020500031013     d                                     prefix(p_)
020600031013     d                                     inz
020700031010     d Fior42ds      e ds
020800031013     d FnormxxDs     e ds                  ExtName(Fnorm00f)
020900031013     d Tibs02ds      e ds
021000041115     d dFar          e ds
021100041115     d dCmr          e ds
021200031013
021300031013     d Trul26Ds      e ds
021400031013     d  skpo                  11    760  0 dim(250)
021500031013     d  skpot                761   1510  0 dim(250)
021600031013
021700031010     d Tibs34ds      e ds                  Inz
021800031010     d dDatiute      e ds
021900031010     d Azuteds       e ds                  Extname(Azute00f)
022000031010     d Kpjba         e ds
022100140606
022200140606      // - Driver per reperire mail/sms
022300140606     d FIOR900DS     e ds                  qualified inz
022400140606
022500140606      // - Campo ORMFLO
022600140606     d dORM01        e ds                  qualified inz
022700140606
022800140606      //---------------------------------------------------------------
022900140606      //?Definizione procedure esterne.
023000140606      //---------------------------------------------------------------
023100140606      // - Driver per reperire mail/sms
023200140606     d FIOR900R        pr                  extpgm('FIOR900R')
023300140606     d  fior900ds                          likeds(fior900ds)
023400010604
023500010604      *--------------------------------------------------------------*
023600010604
023700010608      * stampo la videata delle selezioni
023800031013      *                            =========
023900031013     c                   ExSr      Selezioni
024000031013      *                            =========
024100031013
024200031013      * sql per selezionare gli ORM
024300031013      *                            =========
024400031013     c                   ExSr      SqlOrmPos
024500031013      *                            =========
024600031013
024700031013      * leggo il file degli ORM
024800031013      *                            =====
024900031013     c                   ExSr      Leggi
025000031013      *                            =====
025100031013
025200010621      * stampo
025300031013      *                            ======
025400031013     c                   ExSr      Stampa
025500031013      *                            ======
025600010605
025700010608      * fine programma
025800031013     c                   Eval      *InLr = *On
025900010605
026000010604      *--------------------------------------------------------------*
026100031013      * STAMPA LE SELEZIONI
026200010604      *--------------------------------------------------------------*
026300031013     c     Selezioni     BegSr
026400010618
026500010618      * stampo la prima pagina con le selezioni
026600031013if  1c                   If        i42Poe = 999
026700070418     c                   Eval      di42poe = 'Tutte le fil.'
026800031013   x1c                   Else
026900031013     c     i42Poe        Chain     Azorg01l
027000031013if  2c                   If        %Found(Azorg01l)
027100031014     c                   Movel     OrgDes        di42poe
027200031013    2c                   EndIf
027300031013    1c                   EndIf
027400031013
027500031013if  1c                   If        i42Por = 999
027600070418     c                   Eval      di42por = 'Tutte le fil.'
027700031013   x1c                   Else
027800031013     c     i42Por        Chain     Azorg01l
027900031013if  2c                   If        %Found(Azorg01l)
028000031014     c                   Movel     OrgDes        di42por
028100031013    2c                   EndIf
028200031013    1c                   EndIf
028300010618
028400031013     c                   Clear                   wlbdat
028500031013     c                   Move      i42dtda       g02inv
028600031013     c                   Movel     '3'           G02err
028700031013     c                   Call      'XSRDA8'
028800031013     c                   Parm                    wlbdat
028900031015     c                   Movel     g02dat        datadal
029000010618
029100031013     c                   Clear                   wlbdat
029200031013     c                   Move      i42dtal       g02inv
029300031013     c                   Movel     '3'           G02err
029400031013     c                   Call      'XSRDA8'
029500031013     c                   Parm                    wlbdat
029600031013     c                   Movel     g02dat        dataal
029700031013
029800031013if  1c                   If        i42Tco <> *Blanks
029900031013     c                   Clear                   Tibs02ds
030000031013     c                   Eval      T02Mod = 'C'
030100031013     c                   Eval      T02Sif = Knsif
030200031013     c                   Eval      T02Cod = 'TCO'
030300031014     c                   Movel(p)  i42Tco        T02Ke1
030400031013     c                   Call      'TIBS02R'
030500031013     c                   Parm                    Kpjba
030600031013     c                   Parm                    Tibs02ds
030700031013     c                   Movel     T02Uni        destco
030800031013    1c                   EndIf
030900140606
031000140606      /free
031100140610       //?Selezione Commissionati
031200140610        SELECT;
031300140610        WHEN  I42Comm = 'I';
031400140610          comm = 'Include SOLO ORM Commissionati';
031500140610        WHEN  I42Comm = 'E';
031600140610          comm = 'Esclude ORM Commissionati';
031700140610        WHEN  I42Comm = *blanks;
031800140610          comm = 'Tutti';
031900140610        OTHER;
032000140610          comm = 'Selezione errata';
032100140610        ENDSL;
032200140606       //?Selezione Alert
032300140606        SELECT;
032400140606        WHEN  I42Alert = 'I';
032500140606          alert = 'Include SOLO ORM con Alert';
032600140606        WHEN  I42Alert = 'E';
032700140606          alert = 'Esclude ORM con Alert';
032800140606        WHEN  I42Alert = *blanks;
032900140606          alert = 'Tutti';
033000140606        OTHER;
033100140606          alert = 'Selezione errata';
033200140606        ENDSL;
033300140606      /end-free
033400031013
033500031013     c                   If        i42File = 'S'
033600031013     c                   Eval      file = 'SI'
033700031013     c                   Else
033800031013     c                   Eval      file = 'NO'
033900031013     c                   EndIf
034000031013
034100031014     c                   Move      ds_cli1       cli(1)
034200031014     c                   Move      ds_cli2       cli(2)
034300031014     c                   Move      ds_cli3       cli(3)
034400031014     c                   Move      ds_cli4       cli(4)
034500031014     c                   Move      ds_cli5       cli(5)
034600040701     c                   Movel     ds_cli1       cli7(1)
034700040701     c                   Movel     ds_cli2       cli7(2)
034800040701     c                   Movel     ds_cli3       cli7(3)
034900040701     c                   Movel     ds_cli4       cli7(4)
035000040701     c                   Movel     ds_cli5       cli7(5)
035100031015     c                   Move      Cli13         cli3(1)
035200031015     c                   Move      Cli23         cli3(2)
035300031015     c                   Move      Cli33         cli3(3)
035400031015     c                   Move      Cli43         cli3(4)
035500031015     c                   Move      Cli53         cli3(5)
035600031013     c                   Movea     ds_all        all
035700031013     c                   Movea     ds_des        des
035800031013     c                   Movea     ds_mit        mit
035900031013     c                   Movea     ds_ord        ord
036000031013     c                   Clear                   Rag
036100031013     c                   Clear                   Alls
036200031013do  1c                   Do        5             xa
036300031013     c                   ExSr      Sr_DecoCli
036400031013    1c                   EndDo
036500031014
036600031014     c                   Movea     alls          ds_alls
036700031014     c                   Movea     rag           ds_rag
036800031015     c                   Move      cli3(1)       Cli13
036900031015     c                   Move      cli3(2)       Cli23
037000031015     c                   Move      cli3(3)       Cli33
037100031015     c                   Move      cli3(4)       Cli43
037200031015     c                   Move      cli3(5)       Cli53
037300041115
037400041115      * Stampo le fasi e le causali (solo 15 x ora bastano)
037500041115     c                   Eval      Far01 = skFar(1)
037600041115     c                   Eval      dFar01 = skdFar(1)
037700041115     c                   Eval      Far02 = skFar(2)
037800041115     c                   Eval      dFar02 = skdFar(2)
037900041115     c                   Eval      Far03 = skFar(3)
038000041115     c                   Eval      dFar03 = skdFar(3)
038100041115     c                   Eval      Far04 = skFar(4)
038200041115     c                   Eval      dFar04 = skdFar(4)
038300041115     c                   Eval      Far05 = skFar(5)
038400041115     c                   Eval      dFar05 = skdFar(5)
038500041115     c                   Eval      Far06 = skFar(6)
038600041115     c                   Eval      dFar06 = skdFar(6)
038700041115     c                   Eval      Far07 = skFar(7)
038800041115     c                   Eval      dFar07 = skdFar(7)
038900041115     c                   Eval      Far08 = skFar(8)
039000041115     c                   Eval      dFar08 = skdFar(8)
039100041115     c                   Eval      Far09 = skFar(9)
039200041115     c                   Eval      dFar09 = skdFar(9)
039300041115     c                   Eval      Far10 = skFar(10)
039400041115     c                   Eval      dFar10 = skdFar(10)
039500041115     c                   Eval      Far11 = skFar(11)
039600041115     c                   Eval      dFar11 = skdFar(11)
039700041115     c                   Eval      Far12 = skFar(12)
039800041115     c                   Eval      dFar12 = skdFar(12)
039900041115     c                   Eval      Far13 = skFar(13)
040000041115     c                   Eval      dFar13 = skdFar(13)
040100041115     c                   Eval      Far14 = skFar(14)
040200041115     c                   Eval      dFar14 = skdFar(14)
040300041115     c                   Eval      Far15 = skFar(15)
040400041115     c                   Eval      dFar15 = skdFar(15)
040500041115
040600041115     c                   Eval      Cmr01 = skCmr(1)
040700041115     c                   Eval      dCmr01 = skdCmr(1)
040800041115     c                   Eval      Cmr02 = skCmr(2)
040900041115     c                   Eval      dCmr02 = skdCmr(2)
041000041115     c                   Eval      Cmr03 = skCmr(3)
041100041115     c                   Eval      dCmr03 = skdCmr(3)
041200041115     c                   Eval      Cmr04 = skCmr(4)
041300041115     c                   Eval      dCmr04 = skdCmr(4)
041400041115     c                   Eval      Cmr05 = skCmr(5)
041500041115     c                   Eval      dCmr05 = skdCmr(5)
041600041115     c                   Eval      Cmr06 = skCmr(6)
041700041115     c                   Eval      dCmr06 = skdCmr(6)
041800041115     c                   Eval      Cmr07 = skCmr(7)
041900041115     c                   Eval      dCmr07 = skdCmr(7)
042000041115     c                   Eval      Cmr08 = skCmr(8)
042100041115     c                   Eval      dCmr08 = skdCmr(8)
042200041115     c                   Eval      Cmr09 = skCmr(9)
042300041115     c                   Eval      dCmr09 = skdCmr(9)
042400041115     c                   Eval      Cmr10 = skCmr(10)
042500041115     c                   Eval      dCmr10 = skdCmr(10)
042600041115     c                   Eval      Cmr11 = skCmr(11)
042700041115     c                   Eval      dCmr11 = skdCmr(11)
042800041115     c                   Eval      Cmr12 = skCmr(12)
042900041115     c                   Eval      dCmr12 = skdCmr(12)
043000041115     c                   Eval      Cmr13 = skCmr(13)
043100041115     c                   Eval      dCmr13 = skdCmr(13)
043200041115     c                   Eval      Cmr14 = skCmr(14)
043300041115     c                   Eval      dCmr14 = skdCmr(14)
043400041115     c                   Eval      Cmr15 = skCmr(15)
043500041115     c                   Eval      dCmr15 = skdCmr(15)
043600041115
043700031013     c                   Write     or43s1
043800010618
043900031013     c                   EndSr
044000031013
044100031013      *--------------------------------------------------------------*
044200031013      * SQL  - Preparazione sql FNORM00F
044300031013      *--------------------------------------------------------------*
044400031013     c     SqlOrmPos     BegSr
044500031013
044600031013      * Preparo la stringa sql
044700031013     c                   Eval      StringaSql =
044800031014     c                             'select * from fnorm00f '                   +
044900031014     c                             'where ormatb = '' '' and '                +
045000031014     c                             'ormpoe in ('
045100031013      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
045200031013     c                   Do        250           Xa
045300031013     c                   If        Skpo(Xa) = *Zeros
045400031013     c                   Leave
045500031013     c                   EndIf
045600031013     c                   Eval      StringaSql =
045700031014     c                             %trim(StringaSql) +  ' '                    +
045800031013     c                             %editc(Skpo(Xa):'1') + ','
045900031013     c                   EndDo
046000031013     c                   Eval      StringaSql =
046100031014     c                             %trim(StringaSql) + ')'
046200031014      * solo il range delle date immissione richiesto
046300031014     c                   Eval      StringaSql =
046400031014     c                             %trim(StringaSql) + ' '                     +
046500031014     c                             'and ormdao >= '
046600031014     c                   Eval      StringaSql =
046700031014     c                             %trim(StringaSql) + ' '                     +
046800031014     c                             %editc(i42Dtda:'4')
046900031014     c                   Eval      StringaSql =
047000031014     c                             %trim(StringaSql) + ' '                     +
047100031014     c                             'and ormdao <= '
047200031014     c                   Eval      StringaSql =
047300031014     c                             %trim(StringaSql) + ' '                     +
047400031014     c                             %editc(i42Dtal:'4')
047500031014      * ordinato per p.o. emissione e p.o. ritiro
047600031014     c                   Eval      StringaSql =
047700031014     c                             %trim(StringaSql) + ' '                     +
047800031014     c                             'order by ormpoe, ormpor for fetch only'
047900031013
048000031013     C/EXEC SQL
048100031013     C+ prepare S1 from :StringaSql
048200031013     C/END-EXEC
048300031013
048400031013     C/EXEC SQL
048500031013     C+ declare C1 cursor for S1
048600031013     C/END-EXEC
048700031013
048800031013     c                   EndSr
048900031013
049000031013      *--------------------------------------------------------------*
049100031013      * LEGGO IL FILE DEGLI ORM
049200031013      *--------------------------------------------------------------*
049300031013     c     Leggi         BegSr
049400031013
049500031013     C/EXEC SQL
049600031013     C+ Open C1
049700031013     C/END-EXEC
049800031013
049900031013     C/EXEC SQL
050000031013     C+ Fetch C1 into :fnormxxds
050100031013     C/END-EXEC
050200031013
050300031014do  1c                   Dow       Sqlcod = 0                                   *record letto
050400140606
050500140606      /free
050600140606        dORM01 = ORMflo;
050700140606      /end-free
050800031013
050900031014      * p.o. ritiro
051000031013     c                   If        i42Por <> 999 and OrmPor <> i42Por
051100031014     c                   GoTo      leggircd
051200031013     c                   EndIf
051300031013      * x tipo comunicazione
051400031013     c                   If        i42Tco <> *Blanks and OrmTco <> i42Tco
051500031014     c                   GoTo      leggircd
051600031013     c                   EndIf
051700040701
051800040701      * Controllo se cliente valido
051900040701      * Mittente
052000040701if  2c                   If        OrmCra <> *Zeros
052100040701     c                   Eval      xb = 1
052200040701     c                   Movel     OrmCra        wcli70
052300040701     c                   Move      OrmCra        wcli3
052400040701      * tabella clienti selezionati
052500040701     c     wcli70        Lookup    cli7(xb)                               30
052600040701      * scelto flag di cliente mittente
052700040701if  3c   30              If        mit(xb) <> *Blanks
052800040701      * tutti i luoghi
052900040701if  4c                   If        all(xb) = 'S'
053000040701     c                   Goto      okcli
053100040701   x4c                   Else
053200040701      * solo luogo richiesto
053300040701     c                   If        wcli3 = cli3(xb)
053400040701     c                   Goto      okcli
053500040701    5c                   EndIf
053600040701    4c                   EndIf
053700040701    3c                   EndIf
053800040701e   2c                   EndIf
053900040701
054000040701      * Ordinante
054100040701if  2c                   If        OrmCor <> *Zeros
054200040701     c                   Eval      xb = 1
054300040701     c                   Movel     OrmCor        wcli70
054400040701     c                   Move      OrmCor        wcli3
054500040701      * tabella clienti selezionati
054600040701     c     wcli70        Lookup    cli7(xb)                               30
054700040701      * scelto flag di cliente ordinante
054800040701if  3c   30              If        ord(xb) <> *Blanks
054900040701      * tutti i luoghi
055000040701if  4c                   If        all(xb) = 'S'
055100040701     c                   Goto      okcli
055200040701   x4c                   Else
055300040701      * solo luogo richiesto
055400040701     c                   If        wcli3 = cli3(xb)
055500040701     c                   Goto      okcli
055600040701    5c                   EndIf
055700040701    4c                   EndIf
055800040701    3c                   EndIf
055900040701e   2c                   EndIf
056000040701
056100040701      * Destinatario
056200040701if  2c                   If        OrmCrc <> *Zeros
056300040701     c                   Eval      xb = 1
056400040701     c                   Movel     OrmCrc        wcli70
056500040701     c                   Move      OrmCrc        wcli3
056600040701      * tabella clienti selezionati
056700040701     c     wcli70        Lookup    cli7(xb)                               30
056800040701      * scelto flag di cliente destinatario
056900040701if  3c   30              If        des(xb) <> *Blanks
057000040701      * tutti i luoghi
057100040701if  4c                   If        all(xb) = 'S'
057200040701     c                   Goto      okcli
057300040701   x4c                   Else
057400040701      * solo luogo richiesto
057500040701     c                   If        wcli3 = cli3(xb)
057600040701     c                   Goto      okcli
057700040701    5c                   EndIf
057800040701    4c                   EndIf
057900040701    3c                   EndIf
058000040701e   2c                   EndIf
058100040701
058200040701      * se arrivo qua vuol dire che il cliente non è giusto
058300040701     c                   Goto      leggircd
058400031013
058500040701     c     okcli         Tag
058600140606
058700140606      /free
058800140610       //?Varifico la selezione per commissionati
058900140610         wNoOkOrm = *off;
059000140610         SELECT;
059100140610       //?Tutti gli ORM
059200140610         WHEN  I42comm = *blanks;
059300140610       //?SOLO orm Commissionati
059400140610         WHEN  I42comm = 'I' and dorm01.§ORcom <> 'S';
059500140610           wNoOkOrm = *on;
059600140610       //?ESCLUDO ORM Commissionati
059700140610         WHEN  I42comm = 'E' and dorm01.§ORcom = 'S';
059800140610           wNoOkOrm = *on;
059900140610         ENDSL;
060000140610      /end-free
060100140610     c                   IF        wNoOkOrm
060200140610     c                   goto      LeggiRcd
060300140610     c                   ENDIF
060400140610      /free
060500140606       //?Cerco Alert
060600140606         fior900ds.IOR900trc = 'A';
060700140606         fior900ds.IOR900rcdm = 'MA';
060800140606         fior900ds.IOR900rcds = 'S ';
060900140606         fior900ds.IOR900poe = ORMpoe;
061000140606         fior900ds.IOR900nsr = ORMnsr;
061100140606         fior900ds.IOR900nor = ORMnor;
061200140606         fior900ds.IOR900nrv = ORMnrv;
061300140606         fior900r (fior900ds);
061400140606
061500140606       //?Verifico la selezione per alert
061600140606         wNoOkOrm = *off;
061700140606         SELECT;
061800140606       //?Tutti gli ORM
061900140606         WHEN  I42alert = *blanks;
062000140606       //?SOLO orm con alert
062100140606         WHEN  I42alert = 'I' and fior900ds.OOR900err = 'N';
062200140606           wNoOkOrm = *on;
062300140606       //?ESCLUDO ORM con Alert
062400140606         WHEN  I42alert = 'E' and fior900ds.OOR900err <> 'N';
062500140606           wNoOkOrm = *on;
062600140606         ENDSL;
062700140606      /end-free
062800140606     c                   IF        wNoOkOrm
062900140606     c                   goto      LeggiRcd
063000140606     c                   ENDIF
063100140606
063200031013     c                   Exsr      Sr_Elabora
063300031014
063400031014     c     leggircd      Tag
063500031013
063600031013     C/EXEC SQL
063700031013     C+ Fetch C1 into :fnormxxds
063800031013     C/END-EXEC
063900031013
064000031013    1c                   EndDo
064100031013
064200031013     C/EXEC SQL
064300031013     C+ close C1
064400031013     C/END-EXEC
064500031013
064600031013      * ultimo p.o. ritiro
064700031013      * Scrivo il file di appoggio
064800031013     c                   If        wpor > *Zeros
064900031013     c                   ExSr      Sr_Scrivi
065000031013     c                   EndIf
065100031013
065200031013     c                   EndSr
065300031013
065400031013      *--------------------------------------------------------------*
065500031013      * STAMPA
065600031013      *--------------------------------------------------------------*
065700031013     c     Stampa        BegSr
065800031013
065900031013      * leggo file di comodo per la stampa
066000031013     c  n11*loval        Setll     Wfroc11l
066100031013     c   11*loval        Setll     Wfroc12l
066200031013do  1c                   Do        *Hival
066300031013     c  n11              Read      Wfroc11l
066400031013     c   11              Read      Wfroc12l
066500031013     c                   If        %Eof
066600031013     c                   Leave
066700031013     c                   EndIf
066800031014      * scrivo il record nel file se richiesta la creazione
066900031014     c                   If        i42File = 'S'
067000031014     c                   Eval      dswfroc = dswfroc1
067100031014     c                   Write     Wfroc000
067200031014     c                   EndIf
067300031013
067400041216      * Rottura x --> P.O. EMISSIONE
067500031014if  2c                   If        p_RocPoe <> wpoe
067600041216      * --> Stampo i totali
067700041216if  3c                   If        wpoe <> *Zeros
067800041216      *     totale area
067900041216     c                   Write     or43ta
068000041216     c                   clear                   or43ta
068100041216      *     totale distretto
068200041216     c                   Write     or43td
068300041216     c                   clear                   or43td
068400041216e   3c                   EndIf
068500031014      * p.o. emissione
068600031013     c     p_RocPoe      Chain     Azorg01l
068700031013     c                   If        %Found(Azorg01l)
068800031014     c                   Movel     OrgDes        despoe
068900031013     c                   EndIf
069000031014      * distretto
069100031014     c                   Clear                   descrdis
069200031014     c                   Movel     '17'          TblCod
069300031014     c                   Movel(p)  p_RocDis      TblKey
069400031014     c     kTab          Chain     Tabel00f
069500031014     c                   If        %Found(Tabel00f)
069600031014     c                   Movel     TblUni        descrdis
069700031014     c                   EndIf
069800031014      * area
069900031014     c                   Clear                   descrare
070000031014     c                   Movel     '05'          TblCod
070100031014     c                   Movel(p)  p_RocAre      TblKey
070200031014     c     kTab          Chain     Tabel00f
070300031014     c                   If        %Found(Tabel00f)
070400031014     c                   Movel     TblUni        descrare
070500031014     c                   EndIf
070600041216      * --> Stampa testata
070700031013     c                   Write     or43t1
070800031015     c                   Write     or43t2
070900031014     c                   Write     or43t4
071000031014      * salvo i dati
071100031013     c                   Eval      wpoe = p_RocPoe
071200031014     c                   Eval      wdis = p_RocDis
071300031014     c                   Eval      ware = p_RocAre
071400031016      * spengo indicatore di salto pagina
071500031016     c                   Eval      *In90 = *Off
071600031014    2c                   EndIf
071700041216
071800041216      * Rottura per --> DISTRETTO
071900031014if  2c                   if        wdis <> p_RocDis
072000041216      * --> Stampo i totali
072100041216    3c                   If        wdis <> *Blanks
072200140609      /free
072300140609         //?Salto pagina
072400140609           IF  CurLine >= 62;
072500140609             write OR43T1;
072600140609             write OR43T2;
072700140609             write OR43T4;
072800140609             *in90 = *off;
072900140609           ENDIF;
073000140609      /end-free
073100041216      *     totale area
073200041216     c                   Write     or43ta
073300041216     c                   clear                   or43ta
073400140609      /free
073500140609         //?Salto pagina
073600140609           IF  CurLine >= 62;
073700140609             write OR43T1;
073800140609             write OR43T2;
073900140609             write OR43T4;
074000140609             *in90 = *off;
074100140609           ENDIF;
074200140609      /end-free
074300041216      *     totale distretto
074400041216     c                   Write     or43td
074500041216     c                   clear                   or43td
074600041216    3c                   EndIf
074700031013      * distretto
074800031014     c                   Clear                   descrdis
074900031014     c                   Movel     '17'          TblCod
075000031014     c                   Movel(p)  p_RocDis      TblKey
075100031014     c     kTab          Chain     Tabel00f
075200031014     c                   If        %Found(Tabel00f)
075300031014     c                   Movel     TblUni        descrdis
075400031014     c                   EndIf
075500031013      * area
075600031014     c                   Clear                   descrare
075700031014     c                   Movel     '05'          TblCod
075800031014     c                   Movel(p)  p_RocAre      TblKey
075900031014     c     kTab          Chain     Tabel00f
076000031014     c                   If        %Found(Tabel00f)
076100031014     c                   Movel     TblUni        descrare
076200031014     c                   EndIf
076300041216      * --> Stampa testata
076400031014     c                   Write     or43t1
076500031015     c                   Write     or43t2
076600031014     c                   write     or43t4
076700041216      * salvo i dati
076800031014     c                   Eval      wdis = p_RocDis
076900031014     c                   Eval      ware = p_RocAre
077000031016      * spengo indicatore di salto pagina
077100031016     c                   Eval      *In90 = *Off
077200031014    2c                   EndIf
077300041216
077400041216      * Rottura per --> AREA
077500031014if  2c                   If        ware <> p_RocAre
077600041216      * totale x area
077700041216if  3c                   If        ware <> *Zeros
077800140609      /free
077900140609         //?Salto pagina
078000140609           IF  CurLine >= 62;
078100140609             write OR43T1;
078200140609             write OR43T2;
078300140609             write OR43T4;
078400140609             *in90 = *off;
078500140609           ENDIF;
078600140609      /end-free
078700041216     c                   Write     or43ta
078800041216     c                   clear                   or43ta
078900041216e   3c                   EndIf
079000041216      * salto pagina
079100041216if  2c                   If        *In90 = *On
079200041216     c                   Write     or43t1
079300041216     c                   Write     or43t2
079400041216     c                   Eval      *In90 = *Off
079500041216    2c                   EndIf
079600031013      * area
079700031014     c                   Clear                   descrare
079800031014     c                   Movel     '05'          TblCod
079900031014     c                   Movel(p)  p_RocAre      TblKey
080000031014     c     kTab          Chain     Tabel00f
080100031014     c                   If        %Found(Tabel00f)
080200031014     c                   Movel     TblUni        descrare
080300031014     c                   EndIf
080400031014     c                   Eval      ware = p_RocAre
080500031014    2c                   EndIf
080600031013
080700031013      * salto pagina
080800031014if  2c                   If        *In90 = *On
080900031013     c                   Write     or43t1
081000031015     c                   Write     or43t2
081100031013     c                   Write     or43t4
081200031013     c                   Eval      *In90 = *Off
081300031014    2c                   EndIf
081400031013
081500031014      * decodifico il p.o. ritiro
081600031014     c     p_RocPor      Chain     Azorg01l
081700031014     c                   If        %Found(Azorg01l)
081800031014     c                   Movel     OrgDes        despor
081900031014     c                   EndIf
082000041115
082100041115      * sommo i totali x il totale area
082200041115     c                   add       p_RocTot      taTot
082300041115     c                   add       p_Roc900      ta900
082400041115     c                   add       p_Roc910      ta910
082500041115     c                   add       p_Roc600      ta600
082600041115     c                   add       p_Roc99980    ta99980
082700041115     c                   add       p_Roc99991    ta99991
082800041115     c                   add       p_Roc99992    ta99992
082900041115     c                   add       p_Roc99994    ta99994
083000041115     c                   add       p_Roc99995    ta99995
083100041115     c                   add       p_Roc99996    ta99996
083200041115     c                   add       p_Roc99997    ta99997
083300041115     c                   add       p_Roc50001    ta50001
083400041115     c                   add       p_Roc50002    ta50002
083500041115     c                   add       p_Roc50004    ta50004
083600041115     c                   add       p_Roc400      ta400
083700041115     c                   add       p_Roc390      ta390
083800041115     c                   add       p_Roc300      ta300
083900041115     c                   add       p_Roc100      ta100
084000041115     c                   add       p_Roc050      ta050
084100041115      * sommo i totali x il totale distretto
084200041115     c                   add       p_RocTot      tdTot
084300041115     c                   add       p_Roc900      td900
084400041115     c                   add       p_Roc910      td910
084500041115     c                   add       p_Roc600      td600
084600041115     c                   add       p_Roc99980    td99980
084700041115     c                   add       p_Roc99991    td99991
084800041115     c                   add       p_Roc99992    td99992
084900041115     c                   add       p_Roc99994    td99994
085000041115     c                   add       p_Roc99995    td99995
085100041115     c                   add       p_Roc99996    td99996
085200041115     c                   add       p_Roc99997    td99997
085300041115     c                   add       p_Roc50001    td50001
085400041115     c                   add       p_Roc50002    td50002
085500041115     c                   add       p_Roc50004    td50004
085600041115     c                   add       p_Roc400      td400
085700041115     c                   add       p_Roc390      td390
085800041115     c                   add       p_Roc300      td300
085900041115     c                   add       p_Roc100      td100
086000041115     c                   add       p_Roc050      td050
086100041115      * sommo i totali x il totale generale
086200041115     c                   add       p_RocTot      tgTot
086300041115     c                   add       p_Roc900      tg900
086400041115     c                   add       p_Roc910      tg910
086500041115     c                   add       p_Roc600      tg600
086600041115     c                   add       p_Roc99980    tg99980
086700041115     c                   add       p_Roc99991    tg99991
086800041115     c                   add       p_Roc99992    tg99992
086900041115     c                   add       p_Roc99994    tg99994
087000041115     c                   add       p_Roc99995    tg99995
087100041115     c                   add       p_Roc99996    tg99996
087200041115     c                   add       p_Roc99997    tg99997
087300041115     c                   add       p_Roc50001    tg50001
087400041115     c                   add       p_Roc50002    tg50002
087500041115     c                   add       p_Roc50004    tg50004
087600041115     c                   add       p_Roc400      tg400
087700041115     c                   add       p_Roc390      tg390
087800041115     c                   add       p_Roc300      tg300
087900041115     c                   add       p_Roc100      tg100
088000041115     c                   add       p_Roc050      tg050
088100140609      /free
088200140609         //?Salto pagina
088300140609           IF  CurLine >= 62;
088400140609             write OR43T1;
088500140609             write OR43T2;
088600140609             write OR43T4;
088700140609             *in90 = *off;
088800140609           ENDIF;
088900140609      /end-free
089000031014
089100031014      * stampo i totali
089200031014     c                   Write     or43d1
089300031014      * stampo affidabilità
089400031014if  2c                   If        p_Roc900a > *Zeros or p_Roc910a > *Zeros or
089500031014     c                             p_Roc900t > *Zeros or p_Roc910t > *Zeros or
089600031014     c                             p_Roc9001 > *Zeros or p_Roc9101 > *Zeros or
089700031014     c                             p_Roc9002 > *Zeros or p_Roc9102 > *Zeros or
089800031014     c                             p_Roc900o > *Zeros or p_Roc910o > *Zeros
089900031014     c                   Write     or43a1
090000031014
090100031014     c                   If        p_Roc900a > *Zeros or p_Roc910a > *Zeros
090200031014     c                   Write     or43a11
090300031014     c                   EndIf
090400031014     c                   If        p_Roc900t > *Zeros or p_Roc910t > *Zeros
090500031014     c                   Write     or43a12
090600031014     c                   EndIf
090700031014     c                   If        p_Roc9001 > *Zeros or p_Roc9101 > *Zeros
090800031014     c                   Write     or43a13
090900031014     c                   EndIf
091000031014     c                   If        p_Roc9002 > *Zeros or p_Roc9102 > *Zeros
091100031014     c                   Write     or43a14
091200031014     c                   EndIf
091300031014     c                   If        p_Roc900o > *Zeros or p_Roc910o > *Zeros
091400031014     c                   Write     or43a15
091500031014     c                   EndIf
091600031014    2c                   EndIf
091700031013
091800031014    1c                   EndDo
091900140609      /free
092000140609         //?Salto pagina
092100140609           IF  CurLine >= 62;
092200140609             write OR43T1;
092300140609             write OR43T2;
092400140609             write OR43T4;
092500140609             *in90 = *off;
092600140609           ENDIF;
092700140609      /end-free
092800041115      * Totale area
092900041115     c                   Write     or43ta
093000140609      /free
093100140609         //?Salto pagina
093200140609           IF  CurLine >= 62;
093300140609             write OR43T1;
093400140609             write OR43T2;
093500140609             write OR43T4;
093600140609             *in90 = *off;
093700140609           ENDIF;
093800140609      /end-free
093900041115      * Totale distretto
094000041115     c                   Write     or43td
094100041115      * Totale generale
094200041216      * a pagina nuova
094300041216     c                   Write     or43t1
094400041216     c                   Write     or43t4
094500041115     c                   Write     or43tg
094600031013
094700031013      * fine stampa
094800031014     c                   Write     or43e1
094900031013
095000031014     c                   EndSr
095100031013
095200031013      *---------------------------------------------------------------*
095300031013      * DECODIFICA CLIENTI
095400031013      *---------------------------------------------------------------*
095500031013     c     Sr_DecoCli    BegSr
095600031013
095700031015if  1c                   If        Cli(xa) <> *Zeros
095800031015      * se richiesto tutti i luoghi cerco se almeno 1 esiste
095900031015if  2c                   If        all(xa) = 'S'
096000031015     c                   Eval      alls(xa) = 'SI'
096100031015     c                   Clear                   cli3(xa)
096200031015     c                   Move      cli(xa)       kAcr
096300031013     c                   Move      000           kAcr
096400031013     c     kAcr          Setll     Fnacr01l
096500031015do  3c                   Do        *Hival
096600031013     c                   Read      Fnacr01l
096700031013     c                   If        %Eof(Fnacr01l)
096800031013     c                   Leave
096900031013     c                   EndIf
097000031015     c                   Movel     AcrRsr        rag(xa)
097100031013     c                   Leave
097200031015    3c                   EndDo
097300031013      * se no cerco il cliente specifico
097400031015   x2c                   Else
097500031015     c                   Eval      alls(xa) = 'NO'
097600031015     c                   Move      cli(xa)       kAcr
097700031013     c     kAcr          Chain     Fnacr01l
097800031013     c                   If        %Found(Fnacr01l)
097900031015     c                   Movel     AcrRsr        rag(xa)
098000031013     c                   EndIf
098100031015    2c                   EndIf
098200031015   x1c                   Else
098300031015     c                   Clear                   cli3(xa)
098400031015    1c                   EndIf
098500031013
098600031013     c                   EndSr
098700031013
098800031013      *--------------------------------------------------------------*
098900031013      * ELABORO I DATI LETTI
099000031013      *--------------------------------------------------------------*
099100031013     c     Sr_Elabora    BegSr
099200031013
099300031013      * Totali x  p.o. ritiro
099400031013if  1c                   If        OrmPor <> wpor
099500031013     c                             and wpor > *Zeros
099600031013      * Scrivo il file di appoggio
099700031013     c                   ExSr      Sr_Scrivi
099800031013     c                   Eval      wpor = OrmPor
099900031013    1c                   EndIf
100000031013
100100031013     c                   add       1             wtot
100200031013
100300031013      * calcolo le date x affidabilità in base alla data ritiro
100400031013     c                   ExSr      Sr_Date
100500031013
100600031013      * Fase 900
100700031014if  1c                   If        OrmFao = 900
100800031013     c                   add       1             w900
100900031013      * calcolo affidabilità fase 900
101000031013     c                   Select
101100031013     c                   When      OrmDfo < OrmDar
101200031013     c                   add       1             w900a
101300031014     c                   When      OrmDfo = OrmDar
101400031013     c                   add       1             w900t
101500031015     c                   When      OrmDfo > wdtda02
101600031015     c                   add       1             w900o
101700031015     c                   when      OrmDfo > wdtda01 and OrmDfo <= wdtda02
101800031015     c                   add       1             w9002
101900031015     c                   Other
102000031013     c                   add       1             w9001
102100031013     c                   endsl
102200031013    1c                   EndIf
102300031013
102400031013      * Fase 910
102500031014if  1c                   If        OrmFao = 910
102600031013     c                   add       1             w910
102700031013      * calcolo affidabilità fase 910
102800031013     c                   Select
102900031013     c                   When      OrmDfo < OrmDar
103000031014     c                   add       1             w910a
103100031014     c                   When      OrmDfo = OrmDar
103200031014     c                   add       1             w910t
103300031015     c                   When      OrmDfo > wdtda02
103400031015     c                   add       1             w910o
103500031015     c                   when      OrmDfo > wdtda01 and OrmDfo <= wdtda02
103600031015     c                   add       1             w9102
103700031015     c                   Other
103800031014     c                   add       1             w9101
103900031013     c                   endsl
104000031013    1c                   EndIf
104100031013
104200031013      * Fase 600
104300031014     c                   If        OrmFao = 600
104400031013     c                   add       1             w600
104500031013     c                   EndIf
104600031013
104700031013      * Fase 999 o 500
104800031014if  1c                   If        OrmFao = 999 or OrmFao = 500
104900031013     c     kOrf          Chain     Fnorf01l
105000031013if  2c                   If        %Found(Fnorf01l)
105100031014     c                   If        OrfCar = '80'
105200031014     c                   add       1             w99980
105300031014     c                   EndIf
105400031013     c                   If        OrfCar = '91'
105500031013     c                   add       1             w99991
105600031013     c                   EndIf
105700031013     c                   If        OrfCar = '92'
105800031013     c                   add       1             w99992
105900031013     c                   EndIf
106000031013     c                   If        OrfCar = '94'
106100031013     c                   add       1             w99994
106200031013     c                   EndIf
106300031013     c                   If        OrfCar = '95'
106400031013     c                   add       1             w99995
106500031013     c                   EndIf
106600040701     c                   If        OrfCar = '96'
106700040701     c                   add       1             w99996
106800040701     c                   EndIf
106900040701     c                   If        OrfCar = '97'
107000040701     c                   add       1             w99997
107100040701     c                   EndIf
107200031013     c                   If        OrfCar = '01'
107300031013     c                   add       1             w50001
107400031013     c                   EndIf
107500031013     c                   If        OrfCar = '02'
107600031013     c                   add       1             w50002
107700031013     c                   EndIf
107800031013     c                   If        OrfCar = '04'
107900031013     c                   add       1             w50004
108000031013     c                   EndIf
108100031013    2c                   EndIf
108200031013    1c                   EndIf
108300031013
108400140610      * Fase 400-410-420
108500140610     c                   If        OrmFao = 400 or ORMfao = 410
108600140610     c                             or ORMfao = 420
108700031013     c                   add       1             w400
108800031013     c                   EndIf
108900031013
109000031013      * Fase 390
109100031014     c                   If        OrmFao = 390
109200031013     c                   add       1             w390
109300031013     c                   EndIf
109400031015
109500031015      * Fase 300
109600031015     c                   If        OrmFao = 300
109700031015     c                   add       1             w300
109800031015     c                   EndIf
109900031013
110000031013      * Fase 100
110100031015     c                   If        OrmFao = 100
110200031013     c                   add       1             w100
110300031013     c                   EndIf
110400031015
110500031015      * Fase 050
110600031015     c                   If        OrmFao < 100
110700031015     c                   add       1             w050
110800031015     c                   EndIf
110900031013
111000031013      * p.o. ritiro
111100031013     c                   If        wpor = *Zeros
111200031013     c                   Eval      wpor = OrmPor
111300031013     c                   EndIf
111400031014      * p.o. emissione
111500031014     c                   Eval      wpoed = OrmPoe
111600031013
111700031013     c                   EndSr
111800031013
111900031013      *--------------------------------------------------------------*
112000031013      * CALCOLO DATE X AFFIDABILITA'
112100031013      *--------------------------------------------------------------*
112200031013     c     Sr_Date       BegSr
112300031013
112400031013     c                   Clear                   kTfa
112500031013     c                   Eval      kTfp = OrmPor
112600031013
112700031014      * calcolo la data da + 1
112800031013     c     *iso          Movel     OrmDar        wdtda
112900031013     c                   Do        *Hival
113000031014     c                   Adddur    1:*d          wdtda
113100031013     c                   Extrct    wdtda:*y      kAnn
113200031013     c                   Extrct    wdtda:*m      kMes
113300031013     c                   Extrct    wdtda:*d      wgiorno
113400031013     c     kAzcln        Chain     Azcln01l
113500031013     c                   If        Not %Found(Azcln01l)
113600031013     c                   Leave
113700031013     c                   EndIf
113800031013     c                   If        MAT(wgiorno) =  'F'
113900031013     c                             or  POM(wgiorno) =  'F'
114000031013     c                   Iter
114100031013     c                   EndIf
114200031013     c                   Leave
114300031013     c                   EndDo
114400031013     c                   move      wdtda         wdtda01
114500031013
114600031014      * calcolo la data da + 2
114700031013     c     *iso          Movel     wdtda01       wdtda
114800031013     c                   Do        *Hival
114900031014     c                   Adddur    1:*d          wdtda
115000031013     c                   Extrct    wdtda:*y      kAnn
115100031013     c                   Extrct    wdtda:*m      kMes
115200031013     c                   Extrct    wdtda:*d      wgiorno
115300031013     c     kAzcln        Chain     Azcln01l
115400031013     c                   If        Not %Found(Azcln01l)
115500031013     c                   Leave
115600031013     c                   EndIf
115700031013     c                   If        MAT(wgiorno) =  'F'
115800031013     c                             or  POM(wgiorno) =  'F'
115900031013     c                   Iter
116000031013     c                   EndIf
116100031013     c                   Leave
116200031013     c                   EndDo
116300031013     c                   move      wdtda         wdtda02
116400031013
116500031013     c                   EndSr
116600031013
116700031013      *--------------------------------------------------------------*
116800031013      * SCRIVO IL FILE DI APPOGGIO
116900031013      *--------------------------------------------------------------*
117000031013     c     Sr_Scrivi     BegSr
117100031013
117200031013     c                   Clear                   Wfroc100
117300031013
117400031014     c                   Eval      p_RocPoe = wpoed
117500031013     c     wpor          Chain     Azorg01l
117600031013     c                   If        %Found(Azorg01l)
117700031013     c                   Eval      p_RocDis = OrgFl3
117800031013     c                   Eval      p_RocAre = OrgCar
117900031013     c                   EndIf
118000031013     c                   Eval      p_RocPor = wpor
118100031014     c                   Eval      p_RocTot = wtot
118200031014     c                   Eval      p_Roc900 = w900
118300031014     c                   Eval      p_Roc900a = w900a
118400031014     c                   Eval      p_Roc900t = w900t
118500031014     c                   Eval      p_Roc9001 = w9001
118600031014     c                   Eval      p_Roc9002 = w9002
118700031014     c                   Eval      p_Roc900o = w900o
118800031014     c                   Eval      p_Roc910 = w910
118900031014     c                   Eval      p_Roc910a = w910a
119000031014     c                   Eval      p_Roc910t = w910t
119100031014     c                   Eval      p_Roc9101 = w9101
119200031014     c                   Eval      p_Roc9102 = w9102
119300031014     c                   Eval      p_Roc910o = w910o
119400031014     c                   Eval      p_Roc600 = w600
119500031014     c                   Eval      p_Roc99980 = w99980
119600031014     c                   Eval      p_Roc99991 = w99991
119700031014     c                   Eval      p_Roc99992 = w99992
119800031014     c                   Eval      p_Roc99994 = w99994
119900031014     c                   Eval      p_Roc99995 = w99995
120000040701     c                   Eval      p_Roc99996 = w99996
120100040701     c                   Eval      p_Roc99997 = w99997
120200031014     c                   Eval      p_Roc50001 = w50001
120300031014     c                   Eval      p_Roc50002 = w50002
120400031014     c                   Eval      p_Roc50004 = w50004
120500031014     c                   Eval      p_Roc400 = w400
120600031014     c                   Eval      p_Roc390 = w390
120700031015     c                   Eval      p_Roc300 = w300
120800031014     c                   Eval      p_Roc100 = w100
120900031015     c                   Eval      p_Roc050 = w050
121000031013
121100031013     c                   Write     Wfroc100
121200031013
121300031013     c                   ExSr      Sr_Pulisci
121400031013
121500031013     c                   EndSr
121600031013
121700031013      *--------------------------------------------------------------*
121800031013      * PULISCO CAMPI DI COMODO
121900031013      *--------------------------------------------------------------*
122000031013     c     Sr_Pulisci    BegSr
122100031013
122200031013     c                   Clear                   wtot
122300031013     c                   Clear                   wpor
122400031013     c                   Clear                   w900
122500031013     c                   Clear                   w900a
122600031013     c                   Clear                   w900t
122700031013     c                   Clear                   w9001
122800031013     c                   Clear                   w9002
122900031013     c                   Clear                   w900o
123000031013     c                   Clear                   w910
123100031013     c                   Clear                   w910a
123200031013     c                   Clear                   w910t
123300031013     c                   Clear                   w9101
123400031013     c                   Clear                   w9102
123500031013     c                   Clear                   w910o
123600031013     c                   Clear                   w600
123700031014     c                   Clear                   w99980
123800031013     c                   Clear                   w99991
123900031013     c                   Clear                   w99992
124000031013     c                   Clear                   w99994
124100031013     c                   Clear                   w99995
124200040701     c                   Clear                   w99996
124300040701     c                   Clear                   w99997
124400031013     c                   Clear                   w50001
124500031013     c                   Clear                   w50002
124600031013     c                   Clear                   w50004
124700031013     c                   Clear                   w400
124800031015     c                   Clear                   w390
124900031015     c                   Clear                   w300
125000031013     c                   Clear                   w100
125100031015     c                   Clear                   w050
125200031013
125300031013     c                   EndSr
125400010614
125500010604      *--------------------------------------------------------------*
125600010604      * ROUTINE INIZIALE
125700010604      *--------------------------------------------------------------*
125800031010     c     *Inzsr        BegSr
125900010604      *
126000031010     c     *Entry        Plist
126100031010     c                   Parm                    Kpjba
126200010621
126300031010     c                   Movel     Kpjbu         Fior42ds
126400010621
126500031010     c     *dtaara       define    §azute        azuteds
126600031010     c     *dtaara       define    §datiute      ddatiute
126700031010     c                   in(E)     *dtaara
126800031013     c                   If        %error  or RSUT = *blanks
126900031010     c                   CLEAR                   tibs34ds
127000031010     c                   CALL      'TIBS34R'
127100031010     c                   PARM                    tibs34ds
127200031010     c                   in        *dtaara
127300031013     c                   EndIf
127400031013
127500031013      * reperisco data e ora
127600031013     c                   Time                    w0140
127700031013      * udate in GGMMAAAA
127800031013     c                   Move      w0140         wdate
127900031013     c                   Movel     w0140         wora
128000031013
128100031013     c                   Clear                   Trul26Ds
128200031013      * Carico sk con i p.o. emissione da elaborare
128300031013if  1c                   If        i42Poe = 999
128400031013     c                   Eval      I26tpt = 'P'
128500031013     c                   Eval      I26tla = 'L'
128600031013     c                   Call      'TRUL26R'
128700031013     c                   Parm                    Trul26Ds
128800031013      * lanciato un solo p.o. carico questo nella sk
128900031013   x1c                   Else
129000031013     c                   Eval      Skpo(1) = i42Poe
129100031013    1c                   EndIf
129200031013
129300031013      * Controllo se lancio da sede
129400031013     c                   If        Simfel = *Zeros
129500031013     c                   Eval      *In12 = *On
129600031013     c                   EndIf
129700031013
129800031013      * Controllo se profilo EDP
129900031013     c                   If        %Subst(knmus:1:3) = 'EDP'
130000031013     c                   Eval      *In13 = *On
130100031013     c                   EndIf
130200031013
130300031014      * se richiesta la creazione del file
130400031013     c                   If        i42File = 'S' and
130500031013     c                             (*In12 or *In13)
130600031014      * prima devo pulirlo
130700031014     c                   Clear                   comman
130800031014     c                   Movel(p)  cmd(2)        comman
130900031014     c                   Call      'QCMDEXC'
131000031014     c                   Parm                    comman
131100031014     c                   Parm                    lenght
131200031014      * poi lo apro
131300031013     c                   Open      Wfroc00f
131400031013     c                   EndIf
131500031013
131600031013      * controllo il sistema informativo dove sono per reperire
131700031013      * la libreria
131800041112      *   ---> GAITRA201/FILTRA201 - GAITRAAZM
131900041112     c                   If        %subst(knsif:7:3) = '201'
132000041112     c                   Eval      wlib = 'GAITRAAZM '
132100031013     c                   EndIf
132200041112      *   ---> GAITRAPSP/FILTRAPFP - GAITRAAZP
132300041112     c                   If        %subst(knsif:7:3) = 'PSP' or
132400041112     c                             %subst(knsif:7:3) = 'PFP'
132500041112     c                   Eval      wlib = 'GAITRAAZP '
132600031013     c                   EndIf
132700031013
132800031013      * duplico il file di comodo in qtemp
132900031013     c                   Clear                   comman
133000031013     c                   Movel(p)  cmd(1)        comman
133100031013     c                   Eval      %Subst(comman:32:10) = wlib
133200031013     c                   Call      'QCMDEXC'
133300031013     c                   Parm                    comman
133400031013     c                   Parm                    lenght
133500031013      * apro il file di comodo per la stampa
133600031014     c                   Open      Wfroc10f
133700031013
133800031013      * se lancio fatto da sede e tutti i p.o. emissione apro l.f. in ordine di p.o. emissione
133900031013     c                   If        *In12 and i42Poe = 999
134000031013     c                   Open      Wfroc12l
134100031013     c                   Eval      *In11 = *On
134200031013      * altrimenti apro l.f. in ordine di distretto/area
134300031013     c                   Else
134400031013     c                   Open      Wfroc11l
134500031013     c                   Eval      *In11 = *Off
134600031013     c                   EndIf
134700031014
134800031014      * carico i dati nelle ds di appoggio
134900031014     c                   Move      i42Cli1       ds_cli1
135000031014     c                   Move      i42Cli2       ds_cli2
135100031014     c                   Move      i42Cli3       ds_cli3
135200031014     c                   Move      i42Cli4       ds_cli4
135300031014     c                   Move      i42Cli5       ds_cli5
135400031014     c                   Move      i42All1       all1
135500031014     c                   Move      i42All2       all2
135600031014     c                   Move      i42All3       all3
135700031014     c                   Move      i42All4       all4
135800031014     c                   Move      i42All5       all5
135900031014     c                   Move      i42Mit1       mit1
136000031014     c                   Move      i42Mit2       mit2
136100031014     c                   Move      i42Mit3       mit3
136200031014     c                   Move      i42Mit4       mit4
136300031014     c                   Move      i42Mit5       mit5
136400031014     c                   Move      i42Ord1       ord1
136500031014     c                   Move      i42Ord2       ord2
136600031014     c                   Move      i42Ord3       ord3
136700031014     c                   Move      i42Ord4       ord4
136800031014     c                   Move      i42Ord5       ord5
136900031014     c                   Move      i42Des1       des1
137000031014     c                   Move      i42Des2       des2
137100031014     c                   Move      i42Des3       des3
137200031014     c                   Move      i42Des4       des4
137300031014     c                   Move      i42Des5       des5
137400041115
137500041115      * carico la schiera delle fasi con le relative descrizioni
137600041115     c                   Clear                   xa
137700041115     c                   Eval      kCod = 'FAR'
137800041115     c     kCod          Setll     Tntbe01l
137900041115     c                   Do        *Hival
138000041115     c     kCod          Reade     Tntbe01l
138100041115     c                   If        %Eof(Tntbe01l)
138200041115     c                   Leave
138300041115     c                   EndIf
138400041115      * escludo --> le annullate
138500041115     c                   If        TbeAtb <> *Blanks
138600041115     c                   Iter
138700041115     c                   EndIf
138800041115      *         --> le non gestibili dai pgm ORM
138900041115     c                   Eval      dFar = TbeUni
139000041115     c                   If        d§FarNoo = 'S'
139100041115     c                   Iter
139200041115     c                   EndIf
139300041115      *         --> la 200 xchè mai nessun ORM sarà in fase 200
139400041115     c                   If        TbeKe1 = '200'
139500041115     c                   Iter
139600041115     c                   EndIf
139700041115     c                   add       1             xa
139800041115     c                   eval      skFar(xa) = TbeKe1
139900041115     c                   eval      skdFar(xa) = d§Fardes
140000041115     c                   EndDo
140100041115
140200041115      * carico la schiera delle causali con le relative descrizioni
140300041115     c                   Clear                   xa
140400041115     c                   Eval      kCod = 'CMR'
140500041115     c     kCod          Setll     Tntbe01l
140600041115     c                   Do        *Hival
140700041115     c     kCod          Reade     Tntbe01l
140800041115     c                   If        %Eof(Tntbe01l)
140900041115     c                   Leave
141000041115     c                   EndIf
141100041115      * escludo --> le annullate
141200041115     c                   If        TbeAtb <> *Blanks
141300041115     c                   Iter
141400041115     c                   EndIf
141500041115      *         --> le non gestibili dai pgm ORM
141600041115     c                   Eval      dCmr = TbeUni
141700041115     c                   If        d§CmrNoc = 'S' and d§CmrNoq = 'S' and
141800041115     c                             d§CmrNmf = 'S' and TbeKe1 <> '97'
141900041115     c                   Iter
142000041115     c                   EndIf
142100041115      *         --> la 82 xchè c'è già la fase 910
142200041115     c                   If        TbeKe1 = '82'
142300041115     c                   Iter
142400041115     c                   EndIf
142500041115     c                   add       1             xa
142600041115     c                   eval      skCmr(xa) = TbeKe1
142700041115     c                   eval      skdCmr(xa) = d§Cmrdes1
142800041115     c                   EndDo
142900031013
143000031013     c     kOrf          klist
143100031013     c                   kfld                    OrmPoe
143200031013     c                   kfld                    OrmNsr
143300031013     c                   kfld                    OrmNor
143400031013     c                   kfld                    OrmNrv
143500031013     c                   kfld                    OrmDfo
143600031013     c                   kfld                    OrmOfo
143700031014     c                   kfld                    OrmFao
143800031013
143900031013     c     kAzcln        klist
144000031013     c                   kfld                    ktfp
144100031013     c                   kfld                    ktfa
144200031013     c                   kfld                    kann
144300031013     c                   kfld                    kmes
144400031013
144500031013     c     kTab          klist
144600031013     c                   kfld                    codut
144700031013     c                   kfld                    TblCod
144800031013     c                   kfld                    TblKey
144900010606
145000031013     c                   EndSr
145100010606      *--------------------------------------------------------------*
145200031013**   cmd - comando
145300031013CRTDUPOBJ OBJ(WFROC1*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
145400031013CLRPFM FILE(WFROC00F)
