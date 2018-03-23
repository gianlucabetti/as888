000100040422      *PARMS CLOSQLCSR(*ENDMOD)
000200040422      *PARMS DYNUSRPRF(*OWNER)
000300040422     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000400040422      *------------------------------------------------------------------------*
000500040426      *
000600040428      *
       Statistica ORM manuali           ?
000700040426      *
000800040422      *------------------------------------------------------------------------*
000900040422
001000040426     fFnorm00f  if   e             Disk
001100040426     fAzorg01l  if   e           k Disk
001200130830     fAZCMM01L  if   e           k disk
001300040428     fFior46p   o    e             Printer OfLind(*In90)
001400040426
001500040426      *------------------------------------------------------------------------*
001600040426      *   S C H I E R E
001700040426      *------------------------------------------------------------------------*
001800040427     d skpoel          s              3  0 dim(250)                             po da elab.
001900040426
002000040426      *------------------------------------------------------------------------*
002100040426      *   V A R I A B I L I
002200040426      *------------------------------------------------------------------------*
002300130531     d nrmioorm        s                   like(nrorm)
002400130517     d savCMM          s              7s 0
002500040426     d savpoe          s                   like(ormpoe)
002600040426     d StringaSql      s           1000    Varying
002700040426     d xa              s              5i 0 inz(*zeros)
002800040427     d xb              s              5i 0 inz(*zeros)
002900040426     d wdate           s              8  0
003000040426     d wora            s              6  0
003100040426     d w0140           s             14  0
003200130517     d wEnd            s               n   inz(*off)
003300040426
003400040426      *------------------------------------------------------------------------*
003500040426      *   D S   I N T E R N E / E S T E R N E
003600040426      *------------------------------------------------------------------------*
003700040713     d ds_rigacli      ds
003800040713     d  scodcli                      10  0
003900040713     d  sragsoc                      35
004000040713     d  sindcli                      35
004100040713     d  scapcli                       9
004200040713     d  sloccli                      35
004300040713     d  sprvcli                       2
004400040713     d  snazcli                       3
004500040429
004600040713     d ds_savcli       ds
004700040429     d  codcli
004800040429     d  ragsoc
004900040429     d  indcli
005000040429     d  capcli
005100040429     d  loccli
005200040429     d  prvcli
005300040429     d  nazcli
005400040429
005500040426     d wlbdat          ds                  inz
005600040426     d  g02dat                 1      8  0
005700040426     d  g02inv                 9     16  0
005800040426     d  g02err                17     17
005900040426     d  g02tgi                18     22  0
006000040426
006100040426     d Azuteds       e ds                  Extname(Azute00f)
006200040426     d dDatiute      e ds
006300040426     d Fior44ds      e ds
006400040426     d FnormxxDs     e ds                  ExtName(Fnorm00f)
006500040426     d Kpjba         e ds
006600040426     d Tibs34ds      e ds                  Inz
006700040426     d Trul26Ds      e ds
006800040426     d  skpo                  11    760  0 dim(250)
006900040426     d  skpot                761   1510  0 dim(250)
007000130517     d WFROM10F      e ds                  extname(WFROM10F)
007100130517
007200130517      //---------------------------------------------------------------
007300130517      //?Definizione key-list.
007400130517      //---------------------------------------------------------------
007500130517
007600040426
007700040422      *------------------------------------------------------------------------*
007800040426      *  RIEPILOGO INDICATORI
007900040426      *------------------------------------------------------------------------*
008000040426      * 90 - overflow file stampa statistica
008100040426
008200040426      *------------------------------------------------------------------------*
008300130517
008400130830     c/EXEC SQL
008500130830     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008600130830     c/END-EXEC
008700130830
008800130517       // se ordinamento per commerciale leggo file di work
008900130523     c                   IF        I44ord = 'U'
009000130517     c                   exsr      LeggiWRK
009100130517     c                   goto      fine
009200130517     c                   ENDIF
009300040427
009400040426      * sql per selezionare gli ORM
009500040426      *                            =========
009600040426     c                   ExSr      SqlOrmPos
009700040426      *                            =========
009800040426
009900040426      * leggo il file degli ORM
010000040426      *                            =====
010100040426     c                   ExSr      Leggi
010200040426      *                            =====
010300040429
010400040429      * Stampo ultimo cliente
010500040429     c                   If        nrorm >= I44Orm
010600040429     c                   Write     or46d1
010700080721     c                   add       1             nrcli
010800130531     c                   add       nrorm         nrgorm
010900080721     c                   if        codcli > 0
011000080721     c                   add       1             nrclic
011100130531     c                   add       nrorm         nrgormc
011200080721     c                   else
011300080721     c                   add       1             nrclin
011400130531     c                   add       nrorm         nrgormn
011500080721     c                   endif
011600080909     C
011700130531    4c                   if        nrmioorm > 0
011800080909     c                   add       1             nrclimf
011900130531     c                   add       nrmioorm      nrgormf
012000080909    5c                   if        codcli > 0
012100080909     c                   add       1             nrclimcf
012200130531     c                   add       nrmioorm      nrgormcf
012300080909     c                   else
012400080909     c                   add       1             nrclimnf
012500130531     c                   add       nrmioorm      nrgormnf
012600080909    5c                   endif
012700080909    4c                   endif
012800141217      * di cui via mail
012900141217    4c                   if        nrormm > 0
013000141217     c                   add       1             nrclim
013100141217     c                   add       nrormm        nrgormm
013200141217    5c                   if        codcli > 0
013300141217     c                   add       1             nrclicm
013400141217     c                   add       nrormm        nrgormcm
013500141217     c                   else
013600141217     c                   add       1             nrclinm
013700141217     c                   add       nrormm        nrgormnm
013800141217    5c                   endif
013900141217    4c                   endif
014000080909    C
014100040429     c                   EndIf
014200130531
014300080909      * SALTO PAGINA
014400080909if  1c                   If        *In90 = *On
014500080909     c                   Write     or46t1
014600080909     c                   Write     or46t2
014700080909     c                   Eval      *In90 = *Off
014800080909e   1c                   EndIf
014900080909     c
015000080721     c                   Write     or46g1
015100040426      * fine stampa
015200040428     c                   Write     or46e1
015300040426      *                            ======
015400130517     c     fine          tag
015500040426
015600040426      * fine programma
015700040426     c                   Eval      *InLr = *On
015800040426
015900040426      *------------------------------------------------------------------------*
016000040426      * - SQL Preparazione sql FNORM00F
016100040426      *------------------------------------------------------------------------*
016200040426     c     SqlOrmPos     BegSr
016300040426
016400040426      * Preparo la stringa sql
016500040426     c                   Eval      StringaSql =
016600040426     c                             'select * from fnorm00f '                   +
016700040426     c                             'where ormatb = '' '' and '                 +
016800141215     c                             'ormtco in(''M'', ''E'') and '              +
016900040426     c                             'ormpoe in ('
017000040426      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
017100040426     c                   Do        250           Xa
017200040427     c                   If        Skpoel(Xa) = *Zeros
017300040426     c                   Leave
017400040426     c                   EndIf
017500040426     c                   Eval      StringaSql =
017600040426     c                             %trim(StringaSql) +  ' '                    +
017700040427     c                             %editc(Skpoel(Xa):'1') + ','
017800040426     c                   EndDo
017900040426     c                   Eval      StringaSql =
018000040426     c                             %trim(StringaSql) + ')'
018100040426      * solo il range delle date immissione richiesto
018200040426     c                   Eval      StringaSql =
018300040426     c                             %trim(StringaSql) + ' '                     +
018400040426     c                             'and ormdao >= '
018500040426     c                   Eval      StringaSql =
018600040426     c                             %trim(StringaSql) + ' '                     +
018700040426     c                             %editc(I44Dtda:'4')
018800040426     c                   Eval      StringaSql =
018900040426     c                             %trim(StringaSql) + ' '                     +
019000040426     c                             'and ormdao <= '
019100040426     c                   Eval      StringaSql =
019200040426     c                             %trim(StringaSql) + ' '                     +
019300040426     c                             %editc(I44Dtal:'4')
019400130523      * ordinato per orm filiale emissione
019500040426     c                   Eval      StringaSql =
019600040426     c                             %trim(StringaSql) + ' '                     +
019700040428     c                             'order by ormpoe,'
019800130523     c                   If        I44sel = 'M'
019900040428     c                   Eval      StringaSql =
020000040428     c                             %trim(StringaSql) + ' '                     +
020100040429     c                             'ormrsr, ormcra, orminr, ormcar, ormlor, '  +
020200040429     c                             'ormprr, ormnar'
020300040428     c                   EndIf
020400130523     c                   If        I44sel = 'O'
020500040428     c                   Eval      StringaSql =
020600040428     c                             %trim(StringaSql) + ' '                     +
020700040429     c                             'ormrso, ormcor, ormino, ormcao, ormloo, '  +
020800040429     c                             'ormpro, ormnao'
020900040428     c                   EndIf
021000040428     c                   Eval      StringaSql =
021100040428     c                             %trim(StringaSql) + ' '                     +
021200040428     c                             'for fetch only'
021300040428
021400040426     C/EXEC SQL
021500040426     C+ prepare S1 from :StringaSql
021600040426     C/END-EXEC
021700040426
021800040426     C/EXEC SQL
021900040426     C+ declare C1 cursor for S1
022000040426     C/END-EXEC
022100040426
022200040426     c                   EndSr
022300040426
022400040426      *------------------------------------------------------------------------*
022500040426      * - LEGGO IL FILE DEGLI ORM
022600040426      *------------------------------------------------------------------------*
022700040426     c     Leggi         BegSr
022800040426
022900040426     C/EXEC SQL
023000040426     C+ Open C1
023100040426     C/END-EXEC
023200040426
023300040426     C/EXEC SQL
023400040426     C+ Fetch C1 into :fnormxxds
023500040426     C/END-EXEC
023600040426
023700040426do  1c                   Dow       Sqlcod = 0                                   *record letto
023800040426
023900040429      * controllo se devo elaborare solo i codifiati o meno
024000040429     c                   Select
024100130523     c                   When      I44Cod = 'S' and I44sel = 'M' and
024200040429     c                             OrmCra = *Zeros
024300040429     c                   GoTo      leggircd
024400130523     c                   When      I44Cod = 'N' and I44sel = 'M' and
024500040429     c                             OrmCra > *Zeros
024600040429     c                   GoTo      leggircd
024700130523     c                   When      I44Cod = 'S' and I44sel = 'O' and
024800040429     c                             OrmCor = *Zeros
024900040429     c                   GoTo      leggircd
025000130523     c                   When      I44Cod = 'N' and I44sel = 'O' and
025100040429     c                             OrmCor > *Zeros
025200040429     c                   GoTo      leggircd
025300130523     c                   When      I44Cod = ' ' and I44sel = 'O' and
025400041005     c                             OrmCor = *Zeros and OrmRso = *Blanks
025500041005     c                   GoTo      leggircd
025600040429     c                   EndSl
025700040429
025800040426     c                   Exsr      Sr_Elabora
025900040429
026000040429     c     leggircd      Tag
026100040426
026200040426     C/EXEC SQL
026300040426     C+ Fetch C1 into :fnormxxds
026400040426     C/END-EXEC
026500040426
026600040426    1c                   EndDo
026700040426
026800040426     C/EXEC SQL
026900040426     C+ close C1
027000040426     C/END-EXEC
027100040426
027200040426     c                   EndSr
027300040426
027400040426      *------------------------------------------------------------------------*
027500040426      * - ELABORO I DATI LETTI
027600040426      *------------------------------------------------------------------------*
027700040426     c     Sr_Elabora    BegSr
027800040422
027900040427      * ROTTURA X P.O. EMISSIONE
028000040427if  1c                   If        OrmPoe <> savpoe
028100130523      * --> stampo ultimo cliente della filiale emissione
028200130523     c                   IF        savpoe > 0
028300130523     c                   IF        nrorm >= I44orm
028400130523     c                   write     or46d1
028500130523     c                   add       1             nrcli
028600130531     c                   add       nrorm         nrgorm
028700141217     c                   add       nrormm        nrgormm
028800130523     c                   if        codcli > 0
028900130523     c                   add       1             nrclic
029000130531     c                   add       nrorm         nrgormc
029100130523     c                   else
029200130523     c                   add       1             nrclin
029300130531     c                   add       nrorm         nrgormn
029400130523     c                   endif
029500130523      *
029600130531     c                   if        nrmioorm > 0
029700130523     c                   add       1             nrclimf
029800130531     c                   add       nrmioorm      nrgormf
029900130523     c                   if        codcli > 0
030000130523     c                   add       1             nrclimcf
030100130531     c                   add       nrmioorm      nrgormcf
030200130523     c                   else
030300130523     c                   add       1             nrclimnf
030400130531     c                   add       nrmioorm      nrgormnf
030500130523     c                   endif
030600130523     c                   endif
030700141217      * di cui via mail
030800141217    4c                   if        nrormm > 0
030900141217     c                   add       1             nrclim
031000141217     c                   add       nrormm        nrgormm
031100141217    5c                   if        codcli > 0
031200141217     c                   add       1             nrclicm
031300141217     c                   add       nrormm        nrgormcm
031400141217     c                   else
031500141217     c                   add       1             nrclinm
031600141217     c                   add       nrormm        nrgormnm
031700141217    5c                   endif
031800141217    4c                   endif
031900141217    C
032000130523     c                   ENDIF
032100130523     c                   clear                   ds_savcli
032200130531     c                   clear                   nrmioorm
032300130531     c                   clear                   nrorm
032400141217     c                   clear                   nrormm
032500130523     c                   ENDIF
032600040427      * --> salto Pagina a nuovo p.o. emissione
032700040428     c                   Write     or46t1
032800040426     c                   Eval      *In90 = *Off
032900040427      * --> decodifico il p.o. emissione
033000040426     c     OrmPoe        Chain     Azorg01l
033100040427if  2c                   If        %Found(Azorg01l)
033200040422     c                   Movel     OrgDes        despoe
033300040428     c                   Write     or46t2
033400040427e   2c                   EndIf
033500040428     c                   Write     or46t3
033600040427     c                   Eval      savpoe = OrmPoe
033700040427e   1c                   EndIf
033800040713
033900040713      * DETTAGLIO
034000040713s   1c                   Select
034100130523w   1c                   When      I44sel = 'M'
034200040713     c                   Eval      scodcli = OrmCra
034300040713     c                   Eval      sragsoc = OrmRsr
034400040713     c                   Eval      sindcli = OrmInr
034500040713     c                   Eval      scapcli = OrmCar
034600040713     c                   Eval      sloccli = OrmLor
034700040713     c                   Eval      sprvcli = OrmPrr
034800040713     c                   Eval      snazcli = OrmNar
034900130523w   1c                   When      I44sel = 'O'
035000040713     c                   Eval      scodcli = OrmCor
035100040713     c                   Eval      sragsoc = OrmRso
035200040713     c                   Eval      sindcli = OrmIno
035300040713     c                   Eval      scapcli = OrmCao
035400040713     c                   Eval      sloccli = OrmLoo
035500040713     c                   Eval      sprvcli = OrmPro
035600040713     c                   Eval      snazcli = OrmNao
035700040713e   1c                   EndSl
035800040713
035900040713      * ROTTURA X CLIENTE
036000080909    1c                   If        ds_rigacli <> ds_savcli
036100130531    2c                   If        ragsoc <> *Blanks
036200080909    3c                   If        nrorm >= I44Orm
036300040713     c                   Write     or46d1
036400080721      * sommo le righe stampate quindi i clienti a totale, codificati e non
036500130531      * + totali generali
036600080721     c                   add       1             nrcli
036700130531     c                   add       nrorm         nrgorm
036800080909    4c                   if        codcli > 0
036900080721     c                   add       1             nrclic
037000130531     c                   add       nrorm         nrgormc
037100080721     c                   else
037200080721     c                   add       1             nrclin
037300130531     c                   add       nrorm         nrgormn
037400080909    4c                   endif
037500080909     c
037600130531    4c                   if        nrmioorm > 0
037700080909     c                   add       1             nrclimf
037800130531     c                   add       nrmioorm      nrgormf
037900080909    5c                   if        codcli > 0
038000080909     c                   add       1             nrclimcf
038100130531     c                   add       nrmioorm      nrgormcf
038200080909     c                   else
038300080909     c                   add       1             nrclimnf
038400130531     c                   add       nrmioorm      nrgormnf
038500080909    5c                   endif
038600080909    4c                   endif
038700141217      * di cui via mail
038800141217    4c                   if        nrormm > 0
038900141217     c                   add       1             nrclim
039000141217     c                   add       nrormm        nrgormm
039100141217    5c                   if        codcli > 0
039200141217     c                   add       1             nrclicm
039300141217     c                   add       nrormm        nrgormcm
039400141217     c                   else
039500141217     c                   add       1             nrclinm
039600141217     c                   add       nrormm        nrgormnm
039700141217    5c                   endif
039800141217    4c                   endif
039900080909     c
040000080909    3c                   EndIf
040100080909     c
040200080909    2c                   EndIf
040300080909     c
040400130531     c                   Clear                   nrorm
040500141217     c                   Clear                   nrormm
040600040713     c                   Eval      ds_savcli = ds_rigacli
040700130531     c                   clear                   nrmioorm
040800080909    1c                   EndIf
040900040713
041000040713      * SALTO PAGINA
041100040713if  1c                   If        *In90 = *On
041200040713     c                   Write     or46t1
041300040713     c                   Write     or46t2
041400040713     c                   Write     or46t3
041500040713     c                   Eval      *In90 = *Off
041600040713e   1c                   EndIf
041700040713
041800130531      * Sommo i ritiri immessi
041900040713     c                   add       1             nrorm
042000080908     c
042100130531      * sommo gli ORM di mia competenza
042200080908     c                   if        ormpoe=ormpor
042300130531     c                   add       1             nrmioorm
042400080908     c                   endif
042500141217
042600141217      * Sommo i ritiri immessi via mail
042700141217     c                   IF        ORMtco = 'E'
042800141217     c                   add       1             nrormm
042900141217     c                   ENDIF
043000040426
043100040426     c                   EndSr
043200130517      /free
043300130517
043400130517       //--------------------------------------------------------------
043500130517       //?Leggo file di WORK
043600130517       //--------------------------------------------------------------
043700130517       BEGSR LeggiWRK;
043800130517
043900130517         wEnd = *off;
044000130524         *in40 = (I44poel = 999);
044100130517
044200130517         exec sql
044300130517         declare WFO cursor for
044400130517         select * from WFROM10F
044500130531         order by ROM1cmm, ROM1rag, ROM1cod, ROM1ind, ROM1cap,
044600130531                  ROM1loc, ROM1prv, ROM1naz;
044700130517
044800130517         exec sql
044900130517         open WFO;
045000130517         IF sqlcode < 0;
045100130517           wEnd = *on;
045200130517         ENDIF;
045300130517
045400130517         DOW not wEnd;
045500130517           exec sql
045600130517           fetch next from WFO into :WFROM10F;
045700130517           IF sqlcod = 100 or sqlcod < 0;
045800130517             wEnd = *on;
045900130517             leave;
046000130517           ENDIF;
046100130523
046200130523           IF  ROM1cmm = 9999999;
046300130905             IF savCMM = 0;
046400130905               savCMM = ROM1cmm;
046500130905             ENDIF;
046600130523             clear ROM1cmm;
046700130523           ENDIF;
046800130523
046900130517       // Rottura per Commerciale
047000130523           IF  ROM1cmm <> savCMM;
047100130523         // Stampo Totali per commerciale
047200130531             IF  savCMM > 0 and nrcorm > 0;
047300130523               write OR46C1;
047400130523           // sommo totali generali
047500130523               nrgorm   += nrcorm;
047600130523               nrgormc  += nrcormc;
047700130523               nrgormn  += nrcormn;
047800130523               nrgormf  += nrcormf;
047900130523               nrgormcf += nrcormcf;
048000130523               nrgormnf += nrcormnf;
048100141217               nrgormm  += nrcormm;
048200141217               nrgormcm += nrcormcm;
048300141217               nrgormnm += nrcormnm;
048400130523               nrcli    += nrccli;
048500130523               nrclic   += nrcclic;
048600130523               nrclin   += nrcclin;
048700130523               nrclimf  += nrcclimf;
048800130523               nrclimcf += nrcclimcf;
048900130523               nrclimnf += nrcclimnf;
049000141217               nrclim   += nrcclim;
049100141217               nrclicm  += nrcclicm;
049200141217               nrclinm  += nrcclinm;
049300130523               clear nrcorm;
049400130523               clear nrcormc;
049500130523               clear nrcormn;
049600130523               clear nrcormf;
049700130523               clear nrcormcf;
049800130523               clear nrcormnf;
049900141217               clear nrcormm;
050000141217               clear nrcormcm;
050100141217               clear nrcormnm;
050200130523               clear nrccli;
050300130523               clear nrcclic;
050400130523               clear nrcclin;
050500130523               clear nrcclimf;
050600130523               clear nrcclimcf;
050700130523               clear nrcclimnf;
050800141217               clear nrcclim;
050900141217               clear nrcclicm;
051000141217               clear nrcclinm;
051100130523             ENDIF;
051200130517             write OR46T1;
051300130520             *in90 = *off;
051400130517         // decodifico il commerciale
051500130523             IF  ROM1cmm > 0;
051600130830               chain  (ROM1cmm)  AZCMM000;
051700130830               IF  NOT %found( AZCMM01L );
051800130830                 clear  CMMdes;
051900130523               ENDIF;
052000130830               descmm = CMMdes;
052100130523             ELSE;
052200130523               descmm = 'NO COMMERCIALE';
052300130523             ENDIF;
052400130517             write OR46T2C;
052500130517             savCMM = ROM1Cmm;
052600130517         // decodifico la filiale emissione
052700130524             IF  not *in40;
052800130524               ORMpoe = I44poel;
052900130524               chain  ORMpoe AZORG01L;
053000130524               IF  %found(AZORG01L);
053100130524                 despoe = ORGdes;
053200130524               ENDIF;
053300130524             ELSE;
053400130524               clear ORMpoe;
053500130524               despoe = 'TERMINAL';
053600130524             ENDIF;
053700130524             write OR46T2;
053800130517             write OR46T3;
053900130517           ENDIF;
054000130517
054100130523           CODcli = ROM1cod;
054200130523           RAGsoc = ROM1rag;
054300130523           INDcli = ROM1ind;
054400130523           CAPcli = ROM1cap;
054500130523           LOCcli = ROM1loc;
054600130523           PRVcli = ROM1prv;
054700130523           NAZcli = ROM1naz;
054800130523
054900130523           IF  ROM1orm >= I44orm;
055000130523             nrorm  = ROM1orm;
055100141217             nrormm = ROM1ormm;
055200130517             write  OR46D1;
055300130523           // sommo totali per commerciale
055400130523             nrcorm   += ROM1orm;
055500130523             nrcormc  += ROM1ormc;
055600130523             nrcormn  += ROM1ormn;
055700130523             nrcormf  += ROM1corm;
055800130523             nrcormcf += ROM1cormc;
055900130523             nrcormnf += ROM1cormn;
056000141217             nrcormm  += ROM1ormm;
056100141217             nrcormcm += ROM1ormcm;
056200141217             nrcormnm += ROM1ormnm;
056300130523             nrccli   += 1;
056400130523             IF  codcli > 0;
056500130523               nrcclic  += 1;
056600130523             ELSE;
056700130523               nrcclin  += 1;
056800130523             ENDIF;
056900130523             IF  ROM1corm > 0;
057000130523               nrcclimf  += 1;
057100130523               IF  codcli > 0;
057200130523                 nrcclimcf += 1;
057300130523               ELSE;
057400130523                 nrcclimnf += 1;
057500130523               ENDIF;
057600130523             ENDIF;
057700141217             IF  ROM1ormm > 0;
057800141217               nrcclim  += 1;
057900141217               IF  codcli > 0;
058000141217                 nrcclicm += 1;
058100141217               ELSE;
058200141217                 nrcclinm += 1;
058300141217               ENDIF;
058400141217             ENDIF;
058500130523           ENDIF;
058600130517
058700130517       // Salto pagina;
058800130517           IF  *in90;
058900130517             write OR46T1;
059000130517             write OR46T2C;
059100130517             write OR46T2;
059200130517             write OR46T3;
059300130517             *in90 = *off;
059400130517           ENDIF;
059500130517
059600130517         ENDDO;
059700130517
059800130517         exec sql close WFO;
059900130523
060000130523       // Stampo totali commerciale
060100130523         write OR46C1;
060200130523         nrgorm   += nrcorm;
060300130523         nrgormc  += nrcormc;
060400130523         nrgormn  += nrcormn;
060500130523         nrgormf  += nrcormf;
060600130523         nrgormcf += nrcormcf;
060700130523         nrgormnf += nrcormnf;
060800141217         nrgormm  += nrcormm;
060900141217         nrgormcm += nrcormcm;
061000141217         nrgormnm += nrcormnm;
061100130523         nrcli    += nrccli;
061200130523         nrclic   += nrcclic;
061300130523         nrclin   += nrcclin;
061400130523         nrclimf  += nrcclimf;
061500130523         nrclimcf += nrcclimcf;
061600130523         nrclimnf += nrcclimnf;
061700141217         nrclim   += nrcclim;
061800141217         nrclicm  += nrcclicm;
061900141217         nrclinm  += nrcclinm;
062000130524
062100130517       // Stampo totali
062200130523         write OR46T1;
062300130524         write OR46T2;
062400130522         write OR46G1;
062500130524
062600130517       // Fine stampa
062700130517         write OR46E1;
062800130517
062900130517       ENDSR;
063000130517      /end-free
063100040427
063200040426      *------------------------------------------------------------------------*
063300040426      * - ROUTINE INIZIALE
063400040426      *------------------------------------------------------------------------*
063500040426     c     *Inzsr        BegSr
063600040422
063700040426     c     *Entry        Plist
063800040426     c                   Parm                    Kpjba
063900040426
064000040422     c                   Movel     Kpjbu         Fior44ds
064100040426
064200040426     c     *dtaara       define    §azute        azuteds
064300040426     c     *dtaara       define    §datiute      ddatiute
064400040426     c                   in(E)     *dtaara
064500040426     c                   If        %error  or RSUT = *blanks
064600040426     c                   CLEAR                   tibs34ds
064700040426     c                   CALL      'TIBS34R'
064800040426     c                   PARM                    tibs34ds
064900040426     c                   in        *dtaara
065000040426     c                   EndIf
065100040426
065200040426      * reperisco data e ora
065300040426     c                   Time                    w0140
065400040426      * udate in GGMMAAAA
065500040426     c                   Move      w0140         wdate
065600040426     c                   Movel     w0140         wora
065700040427
065800040427     c                   Clear                   stringasql
065900040427     c                   Clear                   skpoel
066000040427     c                   Eval      xa = 1
066100040426
066200040427      * Carico sk con i p.o. emissione da elaborare
066300040426if  1c                   If        I44Poel = 999
066400140625     c                   Clear                   ormpoe
066500140625     c                   Clear                   Trul26ds
066600040426     c                   Eval      I26tpt = 'P'
066700040426     c                   Eval      I26tla = 'L'
066800040426     c                   Call      'TRUL26R'
066900040427     c                   Parm                    Trul26ds
067000040427      * carico i p.o. da elaborare dell'AS in un sk
067100040427do  2c                   Do        250           xb
067200040427     c                   If        Skpot(xb) = *Zeros
067300040427     c                   Leave
067400040427     c                   EndIf
067500040427      * devo cercare quelli che hanno il terminal di partenza = simfel
067600040427     c                   If        Skpot(xb) = Simfel
067700040427     c                   Eval      Skpoel(xa) = Skpo(xb)
067800040427     c                   Add       1             xa
067900040427     c                   Endif
068000040427e   2c                   EndDo
068100040426      * lanciato un solo p.o. carico questo nella sk
068200040426   x1c                   Else
068300040427     c                   Eval      Skpoel(1) = I44Poel
068400140625     c                   eval      ormpoe=i44poel
068500140625    1c                   EndIf
068600040427
068700040427      * giro le date elaborazione
068800040427     c                   Clear                   wlbdat
068900040427     c                   Move      I44Dtda       g02inv
069000040427     c                   Movel     '3'           G02err
069100040427     c                   Call      'XSRDA8'
069200040427     c                   Parm                    wlbdat
069300040427     c                   Movel     g02dat        datadal
069400040427
069500040427     c                   Clear                   wlbdat
069600040427     c                   Move      I44Dtal       g02inv
069700040427     c                   Movel     '3'           G02err
069800040427     c                   Call      'XSRDA8'
069900040427     c                   Parm                    wlbdat
070000040427     c                   Movel     g02dat        dataal
070100040428
070200040428      * ordinamento
070300040428     c                   Select
070400130523     c                   When      I44sel = 'M'
070500040428     c                   Eval      desord = 'MITTENTE'
070600130523     c                   When      I44sel = 'O'
070700040428     c                   Eval      desord = 'ORDINANTE'
070800040428     c                   EndSl
070900040429
071000040429      * clienti codificati
071100040429     c                   Select
071200040429     c                   When      I44Cod = 'S'
071300040429     c                   Eval      descod = 'CODIFICATI'
071400040429     c                   When      I44Cod = 'N'
071500040429     c                   Eval      descod = 'NON CODIFICATI'
071600040429     c                   Other
071700040429     c                   Eval      descod = 'TUTTI'
071800040429     c                   EndSl
071900130524
072000130524     c                   eval      *in40 = *off
072100040426
072200040426     c                   EndSr
