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
001000130517     fFNACR01L  if   e           k disk
001100040426     fFnorm00f  if   e             Disk
001200040426     fAzorg01l  if   e           k Disk
001300130517     fTABEL00F  if   e           k disk
001400040428     fFior46p   o    e             Printer OfLind(*In90)
001500040426
001600040426      *------------------------------------------------------------------------*
001700040426      *   S C H I E R E
001800040426      *------------------------------------------------------------------------*
001900040427     d skpoel          s              3  0 dim(250)                             po da elab.
002000040426
002100040426      *------------------------------------------------------------------------*
002200040426      *   V A R I A B I L I
002300040426      *------------------------------------------------------------------------*
002400130531     d nrmioorm        s                   like(nrorm)
002500130517     d savCMM          s              7s 0
002600040426     d savpoe          s                   like(ormpoe)
002700040426     d StringaSql      s           1000    Varying
002800040426     d xa              s              5i 0 inz(*zeros)
002900040427     d xb              s              5i 0 inz(*zeros)
003000040426     d wdate           s              8  0
003100040426     d wora            s              6  0
003200040426     d w0140           s             14  0
003300130517     d wEnd            s               n   inz(*off)
003400040426
003500040426      *------------------------------------------------------------------------*
003600040426      *   D S   I N T E R N E / E S T E R N E
003700040426      *------------------------------------------------------------------------*
003800040713     d ds_rigacli      ds
003900040713     d  scodcli                      10  0
004000040713     d  sragsoc                      35
004100040713     d  sindcli                      35
004200040713     d  scapcli                       9
004300040713     d  sloccli                      35
004400040713     d  sprvcli                       2
004500040713     d  snazcli                       3
004600040429
004700040713     d ds_savcli       ds
004800040429     d  codcli
004900040429     d  ragsoc
005000040429     d  indcli
005100040429     d  capcli
005200040429     d  loccli
005300040429     d  prvcli
005400040429     d  nazcli
005500040429
005600040426     d wlbdat          ds                  inz
005700040426     d  g02dat                 1      8  0
005800040426     d  g02inv                 9     16  0
005900040426     d  g02err                17     17
006000040426     d  g02tgi                18     22  0
006100040426
006200040426     d Azuteds       e ds                  Extname(Azute00f)
006300040426     d dDatiute      e ds
006400040426     d Fior44ds      e ds
006500040426     d FnormxxDs     e ds                  ExtName(Fnorm00f)
006600040426     d Kpjba         e ds
006700040426     d Tibs34ds      e ds                  Inz
006800040426     d Trul26Ds      e ds
006900040426     d  skpo                  11    760  0 dim(250)
007000040426     d  skpot                761   1510  0 dim(250)
007100130517     d WFROM10F      e ds                  extname(WFROM10F)
007200130517      // - Tabella 01 = Codici Commerciali
007300130517     d ds01          e ds                  inz
007400130517
007500130517      //---------------------------------------------------------------
007600130517      //?Definizione key-list.
007700130517      //---------------------------------------------------------------
007800130517
007900130517       // - File TABEL00F
008000130517     d k03tabel      e ds                  extname(TABEL00F:*key)
008100130517     d                                     prefix(k_)
008200130517     d                                     inz
008300040426
008400040422      *------------------------------------------------------------------------*
008500040426      *  RIEPILOGO INDICATORI
008600040426      *------------------------------------------------------------------------*
008700040426      * 90 - overflow file stampa statistica
008800040426
008900040426      *------------------------------------------------------------------------*
009000130517
009100130517       // se ordinamento per commerciale leggo file di work
009200130523     c                   IF        I44ord = 'U'
009300130517     c                   exsr      LeggiWRK
009400130517     c                   goto      fine
009500130517     c                   ENDIF
009600040427
009700040426      * sql per selezionare gli ORM
009800040426      *                            =========
009900040426     c                   ExSr      SqlOrmPos
010000040426      *                            =========
010100040426
010200040426      * leggo il file degli ORM
010300040426      *                            =====
010400040426     c                   ExSr      Leggi
010500040426      *                            =====
010600040429
010700040429      * Stampo ultimo cliente
010800040429     c                   If        nrorm >= I44Orm
010900040429     c                   Write     or46d1
011000080721     c                   add       1             nrcli
011100130531     c                   add       nrorm         nrgorm
011200080721     c                   if        codcli > 0
011300080721     c                   add       1             nrclic
011400130531     c                   add       nrorm         nrgormc
011500080721     c                   else
011600080721     c                   add       1             nrclin
011700130531     c                   add       nrorm         nrgormn
011800080721     c                   endif
011900080909     C
012000130531    4c                   if        nrmioorm > 0
012100080909     c                   add       1             nrclimf
012200130531     c                   add       nrmioorm      nrgormf
012300080909    5c                   if        codcli > 0
012400080909     c                   add       1             nrclimcf
012500130531     c                   add       nrmioorm      nrgormcf
012600080909     c                   else
012700080909     c                   add       1             nrclimnf
012800130531     c                   add       nrmioorm      nrgormnf
012900080909    5c                   endif
013000080909    4c                   endif
013100080909    C
013200040429     c                   EndIf
013300130531
013400080909      * SALTO PAGINA
013500080909if  1c                   If        *In90 = *On
013600080909     c                   Write     or46t1
013700080909     c                   Write     or46t2
013800080909     c                   Eval      *In90 = *Off
013900080909e   1c                   EndIf
014000080909     c
014100080721     c                   Write     or46g1
014200040426      * fine stampa
014300040428     c                   Write     or46e1
014400040426      *                            ======
014500130517     c     fine          tag
014600040426
014700040426      * fine programma
014800040426     c                   Eval      *InLr = *On
014900040426
015000040426      *------------------------------------------------------------------------*
015100040426      * - SQL Preparazione sql FNORM00F
015200040426      *------------------------------------------------------------------------*
015300040426     c     SqlOrmPos     BegSr
015400040426
015500040426      * Preparo la stringa sql
015600040426     c                   Eval      StringaSql =
015700040426     c                             'select * from fnorm00f '                   +
015800040426     c                             'where ormatb = '' '' and '                 +
015900040428     c                             'ormtco = ''M'' and '                       +
016000040426     c                             'ormpoe in ('
016100040426      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
016200040426     c                   Do        250           Xa
016300040427     c                   If        Skpoel(Xa) = *Zeros
016400040426     c                   Leave
016500040426     c                   EndIf
016600040426     c                   Eval      StringaSql =
016700040426     c                             %trim(StringaSql) +  ' '                    +
016800040427     c                             %editc(Skpoel(Xa):'1') + ','
016900040426     c                   EndDo
017000040426     c                   Eval      StringaSql =
017100040426     c                             %trim(StringaSql) + ')'
017200040426      * solo il range delle date immissione richiesto
017300040426     c                   Eval      StringaSql =
017400040426     c                             %trim(StringaSql) + ' '                     +
017500040426     c                             'and ormdao >= '
017600040426     c                   Eval      StringaSql =
017700040426     c                             %trim(StringaSql) + ' '                     +
017800040426     c                             %editc(I44Dtda:'4')
017900040426     c                   Eval      StringaSql =
018000040426     c                             %trim(StringaSql) + ' '                     +
018100040426     c                             'and ormdao <= '
018200040426     c                   Eval      StringaSql =
018300040426     c                             %trim(StringaSql) + ' '                     +
018400040426     c                             %editc(I44Dtal:'4')
018500130523      * ordinato per orm filiale emissione
018600040426     c                   Eval      StringaSql =
018700040426     c                             %trim(StringaSql) + ' '                     +
018800040428     c                             'order by ormpoe,'
018900130523     c                   If        I44sel = 'M'
019000040428     c                   Eval      StringaSql =
019100040428     c                             %trim(StringaSql) + ' '                     +
019200040429     c                             'ormrsr, ormcra, orminr, ormcar, ormlor, '  +
019300040429     c                             'ormprr, ormnar'
019400040428     c                   EndIf
019500130523     c                   If        I44sel = 'O'
019600040428     c                   Eval      StringaSql =
019700040428     c                             %trim(StringaSql) + ' '                     +
019800040429     c                             'ormrso, ormcor, ormino, ormcao, ormloo, '  +
019900040429     c                             'ormpro, ormnao'
020000040428     c                   EndIf
020100040428     c                   Eval      StringaSql =
020200040428     c                             %trim(StringaSql) + ' '                     +
020300040428     c                             'for fetch only'
020400040428
020500040426     C/EXEC SQL
020600040426     C+ prepare S1 from :StringaSql
020700040426     C/END-EXEC
020800040426
020900040426     C/EXEC SQL
021000040426     C+ declare C1 cursor for S1
021100040426     C/END-EXEC
021200040426
021300040426     c                   EndSr
021400040426
021500040426      *------------------------------------------------------------------------*
021600040426      * - LEGGO IL FILE DEGLI ORM
021700040426      *------------------------------------------------------------------------*
021800040426     c     Leggi         BegSr
021900040426
022000040426     C/EXEC SQL
022100040426     C+ Open C1
022200040426     C/END-EXEC
022300040426
022400040426     C/EXEC SQL
022500040426     C+ Fetch C1 into :fnormxxds
022600040426     C/END-EXEC
022700040426
022800040426do  1c                   Dow       Sqlcod = 0                                   *record letto
022900040426
023000040429      * controllo se devo elaborare solo i codifiati o meno
023100040429     c                   Select
023200130523     c                   When      I44Cod = 'S' and I44sel = 'M' and
023300040429     c                             OrmCra = *Zeros
023400040429     c                   GoTo      leggircd
023500130523     c                   When      I44Cod = 'N' and I44sel = 'M' and
023600040429     c                             OrmCra > *Zeros
023700040429     c                   GoTo      leggircd
023800130523     c                   When      I44Cod = 'S' and I44sel = 'O' and
023900040429     c                             OrmCor = *Zeros
024000040429     c                   GoTo      leggircd
024100130523     c                   When      I44Cod = 'N' and I44sel = 'O' and
024200040429     c                             OrmCor > *Zeros
024300040429     c                   GoTo      leggircd
024400130523     c                   When      I44Cod = ' ' and I44sel = 'O' and
024500041005     c                             OrmCor = *Zeros and OrmRso = *Blanks
024600041005     c                   GoTo      leggircd
024700040429     c                   EndSl
024800040429
024900040426     c                   Exsr      Sr_Elabora
025000040429
025100040429     c     leggircd      Tag
025200040426
025300040426     C/EXEC SQL
025400040426     C+ Fetch C1 into :fnormxxds
025500040426     C/END-EXEC
025600040426
025700040426    1c                   EndDo
025800040426
025900040426     C/EXEC SQL
026000040426     C+ close C1
026100040426     C/END-EXEC
026200040426
026300040426     c                   EndSr
026400040426
026500040426      *------------------------------------------------------------------------*
026600040426      * - ELABORO I DATI LETTI
026700040426      *------------------------------------------------------------------------*
026800040426     c     Sr_Elabora    BegSr
026900040422
027000040427      * ROTTURA X P.O. EMISSIONE
027100040427if  1c                   If        OrmPoe <> savpoe
027200130523      * --> stampo ultimo cliente della filiale emissione
027300130523     c                   IF        savpoe > 0
027400130523     c                   IF        nrorm >= I44orm
027500130523     c                   write     or46d1
027600130523     c                   add       1             nrcli
027700130531     c                   add       nrorm         nrgorm
027800130523     c                   if        codcli > 0
027900130523     c                   add       1             nrclic
028000130531     c                   add       nrorm         nrgormc
028100130523     c                   else
028200130523     c                   add       1             nrclin
028300130531     c                   add       nrorm         nrgormn
028400130523     c                   endif
028500130523      *
028600130531     c                   if        nrmioorm > 0
028700130523     c                   add       1             nrclimf
028800130531     c                   add       nrmioorm      nrgormf
028900130523     c                   if        codcli > 0
029000130523     c                   add       1             nrclimcf
029100130531     c                   add       nrmioorm      nrgormcf
029200130523     c                   else
029300130523     c                   add       1             nrclimnf
029400130531     c                   add       nrmioorm      nrgormnf
029500130523     c                   endif
029600130523     c                   endif
029700130523     c                   ENDIF
029800130523     c                   clear                   ds_savcli
029900130531     c                   clear                   nrmioorm
030000130531     c                   clear                   nrorm
030100130523     c                   ENDIF
030200040427      * --> salto Pagina a nuovo p.o. emissione
030300040428     c                   Write     or46t1
030400040426     c                   Eval      *In90 = *Off
030500040427      * --> decodifico il p.o. emissione
030600040426     c     OrmPoe        Chain     Azorg01l
030700040427if  2c                   If        %Found(Azorg01l)
030800040422     c                   Movel     OrgDes        despoe
030900040428     c                   Write     or46t2
031000040427e   2c                   EndIf
031100040428     c                   Write     or46t3
031200040427     c                   Eval      savpoe = OrmPoe
031300040427e   1c                   EndIf
031400040713
031500040713      * DETTAGLIO
031600040713s   1c                   Select
031700130523w   1c                   When      I44sel = 'M'
031800040713     c                   Eval      scodcli = OrmCra
031900040713     c                   Eval      sragsoc = OrmRsr
032000040713     c                   Eval      sindcli = OrmInr
032100040713     c                   Eval      scapcli = OrmCar
032200040713     c                   Eval      sloccli = OrmLor
032300040713     c                   Eval      sprvcli = OrmPrr
032400040713     c                   Eval      snazcli = OrmNar
032500130523w   1c                   When      I44sel = 'O'
032600040713     c                   Eval      scodcli = OrmCor
032700040713     c                   Eval      sragsoc = OrmRso
032800040713     c                   Eval      sindcli = OrmIno
032900040713     c                   Eval      scapcli = OrmCao
033000040713     c                   Eval      sloccli = OrmLoo
033100040713     c                   Eval      sprvcli = OrmPro
033200040713     c                   Eval      snazcli = OrmNao
033300040713e   1c                   EndSl
033400040713
033500040713      * ROTTURA X CLIENTE
033600080909    1c                   If        ds_rigacli <> ds_savcli
033700130531    2c                   If        ragsoc <> *Blanks
033800080909    3c                   If        nrorm >= I44Orm
033900040713     c                   Write     or46d1
034000080721      * sommo le righe stampate quindi i clienti a totale, codificati e non
034100130531      * + totali generali
034200080721     c                   add       1             nrcli
034300130531     c                   add       nrorm         nrgorm
034400080909    4c                   if        codcli > 0
034500080721     c                   add       1             nrclic
034600130531     c                   add       nrorm         nrgormc
034700080721     c                   else
034800080721     c                   add       1             nrclin
034900130531     c                   add       nrorm         nrgormn
035000080909    4c                   endif
035100080909     c
035200130531    4c                   if        nrmioorm > 0
035300080909     c                   add       1             nrclimf
035400130531     c                   add       nrmioorm      nrgormf
035500080909    5c                   if        codcli > 0
035600080909     c                   add       1             nrclimcf
035700130531     c                   add       nrmioorm      nrgormcf
035800080909     c                   else
035900080909     c                   add       1             nrclimnf
036000130531     c                   add       nrmioorm      nrgormnf
036100080909    5c                   endif
036200080909    4c                   endif
036300080909     c
036400080909    3c                   EndIf
036500080909     c
036600080909    2c                   EndIf
036700080909     c
036800130531     c                   Clear                   nrorm
036900040713     c                   Eval      ds_savcli = ds_rigacli
037000130531     c                   clear                   nrmioorm
037100080909    1c                   EndIf
037200040713
037300040713      * SALTO PAGINA
037400040713if  1c                   If        *In90 = *On
037500040713     c                   Write     or46t1
037600040713     c                   Write     or46t2
037700040713     c                   Write     or46t3
037800040713     c                   Eval      *In90 = *Off
037900040713e   1c                   EndIf
038000040713
038100130531      * Sommo i ritiri immessi
038200040713     c                   add       1             nrorm
038300080908     c
038400130531      * sommo gli ORM di mia competenza
038500080908     c                   if        ormpoe=ormpor
038600130531     c                   add       1             nrmioorm
038700080908     c                   endif
038800040426
038900040426     c                   EndSr
039000130517      /free
039100130517
039200130517       //--------------------------------------------------------------
039300130517       //?Leggo file di WORK
039400130517       //--------------------------------------------------------------
039500130517       BEGSR LeggiWRK;
039600130517
039700130517         wEnd = *off;
039800130524         *in40 = (I44poel = 999);
039900130517
040000130517         exec sql
040100130517         declare WFO cursor for
040200130517         select * from WFROM10F
040300130531         order by ROM1cmm, ROM1rag, ROM1cod, ROM1ind, ROM1cap,
040400130531                  ROM1loc, ROM1prv, ROM1naz;
040500130517
040600130517         exec sql
040700130517         open WFO;
040800130517         IF sqlcode < 0;
040900130517           wEnd = *on;
041000130517         ENDIF;
041100130517
041200130517         DOW not wEnd;
041300130517           exec sql
041400130517           fetch next from WFO into :WFROM10F;
041500130517           IF sqlcod = 100 or sqlcod < 0;
041600130517             wEnd = *on;
041700130517             leave;
041800130517           ENDIF;
041900130523
042000130523           IF  ROM1cmm = 9999999;
042100130523             clear ROM1cmm;
042200130523           ENDIF;
042300130523
042400130517       // Rottura per Commerciale
042500130523           IF  ROM1cmm <> savCMM;
042600130523         // Stampo Totali per commerciale
042700130531             IF  savCMM > 0 and nrcorm > 0;
042800130523               write OR46C1;
042900130523           // sommo totali generali
043000130523               nrgorm   += nrcorm;
043100130523               nrgormc  += nrcormc;
043200130523               nrgormn  += nrcormn;
043300130523               nrgormf  += nrcormf;
043400130523               nrgormcf += nrcormcf;
043500130523               nrgormnf += nrcormnf;
043600130523               nrcli    += nrccli;
043700130523               nrclic   += nrcclic;
043800130523               nrclin   += nrcclin;
043900130523               nrclimf  += nrcclimf;
044000130523               nrclimcf += nrcclimcf;
044100130523               nrclimnf += nrcclimnf;
044200130523               clear nrcorm;
044300130523               clear nrcormc;
044400130523               clear nrcormn;
044500130523               clear nrcormf;
044600130523               clear nrcormcf;
044700130523               clear nrcormnf;
044800130523               clear nrccli;
044900130523               clear nrcclic;
045000130523               clear nrcclin;
045100130523               clear nrcclimf;
045200130523               clear nrcclimcf;
045300130523               clear nrcclimnf;
045400130523             ENDIF;
045500130517             write OR46T1;
045600130520             *in90 = *off;
045700130517         // decodifico il commerciale
045800130523             IF  ROM1cmm > 0;
045900130523               clear ds01;
046000130523               k_tblkut = 1;
046100130523               k_tblcod = '01';
046200130523               k_tblkey = %editc(ROM1cmm:'X');
046300130523               chain  %kds(K03tabel) TABEL00F;
046400130523               IF  %found( TABEL00F );
046500130523                 ds01 = TBLuni;
046600130523               ENDIF;
046700130523               descmm = §01age;
046800130523             ELSE;
046900130523               descmm = 'NO COMMERCIALE';
047000130523             ENDIF;
047100130517             write OR46T2C;
047200130517             savCMM = ROM1Cmm;
047300130517         // decodifico la filiale emissione
047400130524             IF  not *in40;
047500130524               ORMpoe = I44poel;
047600130524               chain  ORMpoe AZORG01L;
047700130524               IF  %found(AZORG01L);
047800130524                 despoe = ORGdes;
047900130524               ENDIF;
048000130524             ELSE;
048100130524               clear ORMpoe;
048200130524               despoe = 'TERMINAL';
048300130524             ENDIF;
048400130524             write OR46T2;
048500130517             write OR46T3;
048600130517           ENDIF;
048700130517
048800130523           CODcli = ROM1cod;
048900130523           RAGsoc = ROM1rag;
049000130523           INDcli = ROM1ind;
049100130523           CAPcli = ROM1cap;
049200130523           LOCcli = ROM1loc;
049300130523           PRVcli = ROM1prv;
049400130523           NAZcli = ROM1naz;
049500130523
049600130523           IF  ROM1orm >= I44orm;
049700130523             nrorm  = ROM1orm;
049800130517             write  OR46D1;
049900130523           // sommo totali per commerciale
050000130523             nrcorm   += ROM1orm;
050100130523             nrcormc  += ROM1ormc;
050200130523             nrcormn  += ROM1ormn;
050300130523             nrcormf  += ROM1corm;
050400130523             nrcormcf += ROM1cormc;
050500130523             nrcormnf += ROM1cormn;
050600130523             nrccli   += 1;
050700130523             IF  codcli > 0;
050800130523               nrcclic  += 1;
050900130523             ELSE;
051000130523               nrcclin  += 1;
051100130523             ENDIF;
051200130523             IF  ROM1corm > 0;
051300130523               nrcclimf  += 1;
051400130523               IF  codcli > 0;
051500130523                 nrcclimcf += 1;
051600130523               ELSE;
051700130523                 nrcclimnf += 1;
051800130523               ENDIF;
051900130523             ENDIF;
052000130523           ENDIF;
052100130517
052200130517       // Salto pagina;
052300130517           IF  *in90;
052400130517             write OR46T1;
052500130517             write OR46T2C;
052600130517             write OR46T2;
052700130517             write OR46T3;
052800130517             *in90 = *off;
052900130517           ENDIF;
053000130517
053100130517         ENDDO;
053200130517
053300130517         exec sql close WFO;
053400130523
053500130523       // Stampo totali commerciale
053600130523         write OR46C1;
053700130523         nrgorm   += nrcorm;
053800130523         nrgormc  += nrcormc;
053900130523         nrgormn  += nrcormn;
054000130523         nrgormf  += nrcormf;
054100130523         nrgormcf += nrcormcf;
054200130523         nrgormnf += nrcormnf;
054300130523         nrcli    += nrccli;
054400130523         nrclic   += nrcclic;
054500130523         nrclin   += nrcclin;
054600130523         nrclimf  += nrcclimf;
054700130523         nrclimcf += nrcclimcf;
054800130523         nrclimnf += nrcclimnf;
054900130524
055000130517       // Stampo totali
055100130523         write OR46T1;
055200130524         write OR46T2;
055300130522         write OR46G1;
055400130524
055500130517       // Fine stampa
055600130517         write OR46E1;
055700130517
055800130517       ENDSR;
055900130517      /end-free
056000040427
056100040426      *------------------------------------------------------------------------*
056200040426      * - ROUTINE INIZIALE
056300040426      *------------------------------------------------------------------------*
056400040426     c     *Inzsr        BegSr
056500040422
056600040426     c     *Entry        Plist
056700040426     c                   Parm                    Kpjba
056800040426
056900040422     c                   Movel     Kpjbu         Fior44ds
057000040426
057100040426     c     *dtaara       define    §azute        azuteds
057200040426     c     *dtaara       define    §datiute      ddatiute
057300040426     c                   in(E)     *dtaara
057400040426     c                   If        %error  or RSUT = *blanks
057500040426     c                   CLEAR                   tibs34ds
057600040426     c                   CALL      'TIBS34R'
057700040426     c                   PARM                    tibs34ds
057800040426     c                   in        *dtaara
057900040426     c                   EndIf
058000040426
058100040426      * reperisco data e ora
058200040426     c                   Time                    w0140
058300040426      * udate in GGMMAAAA
058400040426     c                   Move      w0140         wdate
058500040426     c                   Movel     w0140         wora
058600040427
058700040427     c                   Clear                   stringasql
058800040427     c                   Clear                   skpoel
058900040427     c                   Eval      xa = 1
059000040426
059100040427      * Carico sk con i p.o. emissione da elaborare
059200040426if  1c                   If        I44Poel = 999
059300040427     c                   Clear                   Trul26ds
059400040426     c                   Eval      I26tpt = 'P'
059500040426     c                   Eval      I26tla = 'L'
059600040426     c                   Call      'TRUL26R'
059700040427     c                   Parm                    Trul26ds
059800040427      * carico i p.o. da elaborare dell'AS in un sk
059900040427do  2c                   Do        250           xb
060000040427     c                   If        Skpot(xb) = *Zeros
060100040427     c                   Leave
060200040427     c                   EndIf
060300040427      * devo cercare quelli che hanno il terminal di partenza = simfel
060400040427     c                   If        Skpot(xb) = Simfel
060500040427     c                   Eval      Skpoel(xa) = Skpo(xb)
060600040427     c                   Add       1             xa
060700040427     c                   Endif
060800040427e   2c                   EndDo
060900040426      * lanciato un solo p.o. carico questo nella sk
061000040426   x1c                   Else
061100040427     c                   Eval      Skpoel(1) = I44Poel
061200040426    1c                   EndIf
061300040427
061400040427      * giro le date elaborazione
061500040427     c                   Clear                   wlbdat
061600040427     c                   Move      I44Dtda       g02inv
061700040427     c                   Movel     '3'           G02err
061800040427     c                   Call      'XSRDA8'
061900040427     c                   Parm                    wlbdat
062000040427     c                   Movel     g02dat        datadal
062100040427
062200040427     c                   Clear                   wlbdat
062300040427     c                   Move      I44Dtal       g02inv
062400040427     c                   Movel     '3'           G02err
062500040427     c                   Call      'XSRDA8'
062600040427     c                   Parm                    wlbdat
062700040427     c                   Movel     g02dat        dataal
062800040428
062900040428      * ordinamento
063000040428     c                   Select
063100130523     c                   When      I44sel = 'M'
063200040428     c                   Eval      desord = 'MITTENTE'
063300130523     c                   When      I44sel = 'O'
063400040428     c                   Eval      desord = 'ORDINANTE'
063500040428     c                   EndSl
063600040429
063700040429      * clienti codificati
063800040429     c                   Select
063900040429     c                   When      I44Cod = 'S'
064000040429     c                   Eval      descod = 'CODIFICATI'
064100040429     c                   When      I44Cod = 'N'
064200040429     c                   Eval      descod = 'NON CODIFICATI'
064300040429     c                   Other
064400040429     c                   Eval      descod = 'TUTTI'
064500040429     c                   EndSl
064600130524
064700130524     c                   eval      *in40 = *off
064800040426
064900040426     c                   EndSr
