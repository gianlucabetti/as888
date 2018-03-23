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
012800080909    C
012900040429     c                   EndIf
013000130531
013100080909      * SALTO PAGINA
013200080909if  1c                   If        *In90 = *On
013300080909     c                   Write     or46t1
013400080909     c                   Write     or46t2
013500080909     c                   Eval      *In90 = *Off
013600080909e   1c                   EndIf
013700080909     c
013800080721     c                   Write     or46g1
013900040426      * fine stampa
014000040428     c                   Write     or46e1
014100040426      *                            ======
014200130517     c     fine          tag
014300040426
014400040426      * fine programma
014500040426     c                   Eval      *InLr = *On
014600040426
014700040426      *------------------------------------------------------------------------*
014800040426      * - SQL Preparazione sql FNORM00F
014900040426      *------------------------------------------------------------------------*
015000040426     c     SqlOrmPos     BegSr
015100040426
015200040426      * Preparo la stringa sql
015300040426     c                   Eval      StringaSql =
015400040426     c                             'select * from fnorm00f '                   +
015500040426     c                             'where ormatb = '' '' and '                 +
015600040428     c                             'ormtco = ''M'' and '                       +
015700040426     c                             'ormpoe in ('
015800040426      * ciclo sulla sk p.o. da elaborare per i p.o. emissione
015900040426     c                   Do        250           Xa
016000040427     c                   If        Skpoel(Xa) = *Zeros
016100040426     c                   Leave
016200040426     c                   EndIf
016300040426     c                   Eval      StringaSql =
016400040426     c                             %trim(StringaSql) +  ' '                    +
016500040427     c                             %editc(Skpoel(Xa):'1') + ','
016600040426     c                   EndDo
016700040426     c                   Eval      StringaSql =
016800040426     c                             %trim(StringaSql) + ')'
016900040426      * solo il range delle date immissione richiesto
017000040426     c                   Eval      StringaSql =
017100040426     c                             %trim(StringaSql) + ' '                     +
017200040426     c                             'and ormdao >= '
017300040426     c                   Eval      StringaSql =
017400040426     c                             %trim(StringaSql) + ' '                     +
017500040426     c                             %editc(I44Dtda:'4')
017600040426     c                   Eval      StringaSql =
017700040426     c                             %trim(StringaSql) + ' '                     +
017800040426     c                             'and ormdao <= '
017900040426     c                   Eval      StringaSql =
018000040426     c                             %trim(StringaSql) + ' '                     +
018100040426     c                             %editc(I44Dtal:'4')
018200130523      * ordinato per orm filiale emissione
018300040426     c                   Eval      StringaSql =
018400040426     c                             %trim(StringaSql) + ' '                     +
018500040428     c                             'order by ormpoe,'
018600130523     c                   If        I44sel = 'M'
018700040428     c                   Eval      StringaSql =
018800040428     c                             %trim(StringaSql) + ' '                     +
018900040429     c                             'ormrsr, ormcra, orminr, ormcar, ormlor, '  +
019000040429     c                             'ormprr, ormnar'
019100040428     c                   EndIf
019200130523     c                   If        I44sel = 'O'
019300040428     c                   Eval      StringaSql =
019400040428     c                             %trim(StringaSql) + ' '                     +
019500040429     c                             'ormrso, ormcor, ormino, ormcao, ormloo, '  +
019600040429     c                             'ormpro, ormnao'
019700040428     c                   EndIf
019800040428     c                   Eval      StringaSql =
019900040428     c                             %trim(StringaSql) + ' '                     +
020000040428     c                             'for fetch only'
020100040428
020200040426     C/EXEC SQL
020300040426     C+ prepare S1 from :StringaSql
020400040426     C/END-EXEC
020500040426
020600040426     C/EXEC SQL
020700040426     C+ declare C1 cursor for S1
020800040426     C/END-EXEC
020900040426
021000040426     c                   EndSr
021100040426
021200040426      *------------------------------------------------------------------------*
021300040426      * - LEGGO IL FILE DEGLI ORM
021400040426      *------------------------------------------------------------------------*
021500040426     c     Leggi         BegSr
021600040426
021700040426     C/EXEC SQL
021800040426     C+ Open C1
021900040426     C/END-EXEC
022000040426
022100040426     C/EXEC SQL
022200040426     C+ Fetch C1 into :fnormxxds
022300040426     C/END-EXEC
022400040426
022500040426do  1c                   Dow       Sqlcod = 0                                   *record letto
022600040426
022700040429      * controllo se devo elaborare solo i codifiati o meno
022800040429     c                   Select
022900130523     c                   When      I44Cod = 'S' and I44sel = 'M' and
023000040429     c                             OrmCra = *Zeros
023100040429     c                   GoTo      leggircd
023200130523     c                   When      I44Cod = 'N' and I44sel = 'M' and
023300040429     c                             OrmCra > *Zeros
023400040429     c                   GoTo      leggircd
023500130523     c                   When      I44Cod = 'S' and I44sel = 'O' and
023600040429     c                             OrmCor = *Zeros
023700040429     c                   GoTo      leggircd
023800130523     c                   When      I44Cod = 'N' and I44sel = 'O' and
023900040429     c                             OrmCor > *Zeros
024000040429     c                   GoTo      leggircd
024100130523     c                   When      I44Cod = ' ' and I44sel = 'O' and
024200041005     c                             OrmCor = *Zeros and OrmRso = *Blanks
024300041005     c                   GoTo      leggircd
024400040429     c                   EndSl
024500040429
024600040426     c                   Exsr      Sr_Elabora
024700040429
024800040429     c     leggircd      Tag
024900040426
025000040426     C/EXEC SQL
025100040426     C+ Fetch C1 into :fnormxxds
025200040426     C/END-EXEC
025300040426
025400040426    1c                   EndDo
025500040426
025600040426     C/EXEC SQL
025700040426     C+ close C1
025800040426     C/END-EXEC
025900040426
026000040426     c                   EndSr
026100040426
026200040426      *------------------------------------------------------------------------*
026300040426      * - ELABORO I DATI LETTI
026400040426      *------------------------------------------------------------------------*
026500040426     c     Sr_Elabora    BegSr
026600040422
026700040427      * ROTTURA X P.O. EMISSIONE
026800040427if  1c                   If        OrmPoe <> savpoe
026900130523      * --> stampo ultimo cliente della filiale emissione
027000130523     c                   IF        savpoe > 0
027100130523     c                   IF        nrorm >= I44orm
027200130523     c                   write     or46d1
027300130523     c                   add       1             nrcli
027400130531     c                   add       nrorm         nrgorm
027500130523     c                   if        codcli > 0
027600130523     c                   add       1             nrclic
027700130531     c                   add       nrorm         nrgormc
027800130523     c                   else
027900130523     c                   add       1             nrclin
028000130531     c                   add       nrorm         nrgormn
028100130523     c                   endif
028200130523      *
028300130531     c                   if        nrmioorm > 0
028400130523     c                   add       1             nrclimf
028500130531     c                   add       nrmioorm      nrgormf
028600130523     c                   if        codcli > 0
028700130523     c                   add       1             nrclimcf
028800130531     c                   add       nrmioorm      nrgormcf
028900130523     c                   else
029000130523     c                   add       1             nrclimnf
029100130531     c                   add       nrmioorm      nrgormnf
029200130523     c                   endif
029300130523     c                   endif
029400130523     c                   ENDIF
029500130523     c                   clear                   ds_savcli
029600130531     c                   clear                   nrmioorm
029700130531     c                   clear                   nrorm
029800130523     c                   ENDIF
029900040427      * --> salto Pagina a nuovo p.o. emissione
030000040428     c                   Write     or46t1
030100040426     c                   Eval      *In90 = *Off
030200040427      * --> decodifico il p.o. emissione
030300040426     c     OrmPoe        Chain     Azorg01l
030400040427if  2c                   If        %Found(Azorg01l)
030500040422     c                   Movel     OrgDes        despoe
030600040428     c                   Write     or46t2
030700040427e   2c                   EndIf
030800040428     c                   Write     or46t3
030900040427     c                   Eval      savpoe = OrmPoe
031000040427e   1c                   EndIf
031100040713
031200040713      * DETTAGLIO
031300040713s   1c                   Select
031400130523w   1c                   When      I44sel = 'M'
031500040713     c                   Eval      scodcli = OrmCra
031600040713     c                   Eval      sragsoc = OrmRsr
031700040713     c                   Eval      sindcli = OrmInr
031800040713     c                   Eval      scapcli = OrmCar
031900040713     c                   Eval      sloccli = OrmLor
032000040713     c                   Eval      sprvcli = OrmPrr
032100040713     c                   Eval      snazcli = OrmNar
032200130523w   1c                   When      I44sel = 'O'
032300040713     c                   Eval      scodcli = OrmCor
032400040713     c                   Eval      sragsoc = OrmRso
032500040713     c                   Eval      sindcli = OrmIno
032600040713     c                   Eval      scapcli = OrmCao
032700040713     c                   Eval      sloccli = OrmLoo
032800040713     c                   Eval      sprvcli = OrmPro
032900040713     c                   Eval      snazcli = OrmNao
033000040713e   1c                   EndSl
033100040713
033200040713      * ROTTURA X CLIENTE
033300080909    1c                   If        ds_rigacli <> ds_savcli
033400130531    2c                   If        ragsoc <> *Blanks
033500080909    3c                   If        nrorm >= I44Orm
033600040713     c                   Write     or46d1
033700080721      * sommo le righe stampate quindi i clienti a totale, codificati e non
033800130531      * + totali generali
033900080721     c                   add       1             nrcli
034000130531     c                   add       nrorm         nrgorm
034100080909    4c                   if        codcli > 0
034200080721     c                   add       1             nrclic
034300130531     c                   add       nrorm         nrgormc
034400080721     c                   else
034500080721     c                   add       1             nrclin
034600130531     c                   add       nrorm         nrgormn
034700080909    4c                   endif
034800080909     c
034900130531    4c                   if        nrmioorm > 0
035000080909     c                   add       1             nrclimf
035100130531     c                   add       nrmioorm      nrgormf
035200080909    5c                   if        codcli > 0
035300080909     c                   add       1             nrclimcf
035400130531     c                   add       nrmioorm      nrgormcf
035500080909     c                   else
035600080909     c                   add       1             nrclimnf
035700130531     c                   add       nrmioorm      nrgormnf
035800080909    5c                   endif
035900080909    4c                   endif
036000080909     c
036100080909    3c                   EndIf
036200080909     c
036300080909    2c                   EndIf
036400080909     c
036500130531     c                   Clear                   nrorm
036600040713     c                   Eval      ds_savcli = ds_rigacli
036700130531     c                   clear                   nrmioorm
036800080909    1c                   EndIf
036900040713
037000040713      * SALTO PAGINA
037100040713if  1c                   If        *In90 = *On
037200040713     c                   Write     or46t1
037300040713     c                   Write     or46t2
037400040713     c                   Write     or46t3
037500040713     c                   Eval      *In90 = *Off
037600040713e   1c                   EndIf
037700040713
037800130531      * Sommo i ritiri immessi
037900040713     c                   add       1             nrorm
038000080908     c
038100130531      * sommo gli ORM di mia competenza
038200080908     c                   if        ormpoe=ormpor
038300130531     c                   add       1             nrmioorm
038400080908     c                   endif
038500040426
038600040426     c                   EndSr
038700130517      /free
038800130517
038900130517       //--------------------------------------------------------------
039000130517       //?Leggo file di WORK
039100130517       //--------------------------------------------------------------
039200130517       BEGSR LeggiWRK;
039300130517
039400130517         wEnd = *off;
039500130524         *in40 = (I44poel = 999);
039600130517
039700130517         exec sql
039800130517         declare WFO cursor for
039900130517         select * from WFROM10F
040000130531         order by ROM1cmm, ROM1rag, ROM1cod, ROM1ind, ROM1cap,
040100130531                  ROM1loc, ROM1prv, ROM1naz;
040200130517
040300130517         exec sql
040400130517         open WFO;
040500130517         IF sqlcode < 0;
040600130517           wEnd = *on;
040700130517         ENDIF;
040800130517
040900130517         DOW not wEnd;
041000130517           exec sql
041100130517           fetch next from WFO into :WFROM10F;
041200130517           IF sqlcod = 100 or sqlcod < 0;
041300130517             wEnd = *on;
041400130517             leave;
041500130517           ENDIF;
041600130523
041700130523           IF  ROM1cmm = 9999999;
041800130905             IF savCMM = 0;
041900130905               savCMM = ROM1cmm;
042000130905             ENDIF;
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
045900130830               chain  (ROM1cmm)  AZCMM000;
046000130830               IF  NOT %found( AZCMM01L );
046100130830                 clear  CMMdes;
046200130523               ENDIF;
046300130830               descmm = CMMdes;
046400130523             ELSE;
046500130523               descmm = 'NO COMMERCIALE';
046600130523             ENDIF;
046700130517             write OR46T2C;
046800130517             savCMM = ROM1Cmm;
046900130517         // decodifico la filiale emissione
047000130524             IF  not *in40;
047100130524               ORMpoe = I44poel;
047200130524               chain  ORMpoe AZORG01L;
047300130524               IF  %found(AZORG01L);
047400130524                 despoe = ORGdes;
047500130524               ENDIF;
047600130524             ELSE;
047700130524               clear ORMpoe;
047800130524               despoe = 'TERMINAL';
047900130524             ENDIF;
048000130524             write OR46T2;
048100130517             write OR46T3;
048200130517           ENDIF;
048300130517
048400130523           CODcli = ROM1cod;
048500130523           RAGsoc = ROM1rag;
048600130523           INDcli = ROM1ind;
048700130523           CAPcli = ROM1cap;
048800130523           LOCcli = ROM1loc;
048900130523           PRVcli = ROM1prv;
049000130523           NAZcli = ROM1naz;
049100130523
049200130523           IF  ROM1orm >= I44orm;
049300130523             nrorm  = ROM1orm;
049400130517             write  OR46D1;
049500130523           // sommo totali per commerciale
049600130523             nrcorm   += ROM1orm;
049700130523             nrcormc  += ROM1ormc;
049800130523             nrcormn  += ROM1ormn;
049900130523             nrcormf  += ROM1corm;
050000130523             nrcormcf += ROM1cormc;
050100130523             nrcormnf += ROM1cormn;
050200130523             nrccli   += 1;
050300130523             IF  codcli > 0;
050400130523               nrcclic  += 1;
050500130523             ELSE;
050600130523               nrcclin  += 1;
050700130523             ENDIF;
050800130523             IF  ROM1corm > 0;
050900130523               nrcclimf  += 1;
051000130523               IF  codcli > 0;
051100130523                 nrcclimcf += 1;
051200130523               ELSE;
051300130523                 nrcclimnf += 1;
051400130523               ENDIF;
051500130523             ENDIF;
051600130523           ENDIF;
051700130517
051800130517       // Salto pagina;
051900130517           IF  *in90;
052000130517             write OR46T1;
052100130517             write OR46T2C;
052200130517             write OR46T2;
052300130517             write OR46T3;
052400130517             *in90 = *off;
052500130517           ENDIF;
052600130517
052700130517         ENDDO;
052800130517
052900130517         exec sql close WFO;
053000130523
053100130523       // Stampo totali commerciale
053200130523         write OR46C1;
053300130523         nrgorm   += nrcorm;
053400130523         nrgormc  += nrcormc;
053500130523         nrgormn  += nrcormn;
053600130523         nrgormf  += nrcormf;
053700130523         nrgormcf += nrcormcf;
053800130523         nrgormnf += nrcormnf;
053900130523         nrcli    += nrccli;
054000130523         nrclic   += nrcclic;
054100130523         nrclin   += nrcclin;
054200130523         nrclimf  += nrcclimf;
054300130523         nrclimcf += nrcclimcf;
054400130523         nrclimnf += nrcclimnf;
054500130524
054600130517       // Stampo totali
054700130523         write OR46T1;
054800130524         write OR46T2;
054900130522         write OR46G1;
055000130524
055100130517       // Fine stampa
055200130517         write OR46E1;
055300130517
055400130517       ENDSR;
055500130517      /end-free
055600040427
055700040426      *------------------------------------------------------------------------*
055800040426      * - ROUTINE INIZIALE
055900040426      *------------------------------------------------------------------------*
056000040426     c     *Inzsr        BegSr
056100040422
056200040426     c     *Entry        Plist
056300040426     c                   Parm                    Kpjba
056400040426
056500040422     c                   Movel     Kpjbu         Fior44ds
056600040426
056700040426     c     *dtaara       define    §azute        azuteds
056800040426     c     *dtaara       define    §datiute      ddatiute
056900040426     c                   in(E)     *dtaara
057000040426     c                   If        %error  or RSUT = *blanks
057100040426     c                   CLEAR                   tibs34ds
057200040426     c                   CALL      'TIBS34R'
057300040426     c                   PARM                    tibs34ds
057400040426     c                   in        *dtaara
057500040426     c                   EndIf
057600040426
057700040426      * reperisco data e ora
057800040426     c                   Time                    w0140
057900040426      * udate in GGMMAAAA
058000040426     c                   Move      w0140         wdate
058100040426     c                   Movel     w0140         wora
058200040427
058300040427     c                   Clear                   stringasql
058400040427     c                   Clear                   skpoel
058500040427     c                   Eval      xa = 1
058600040426
058700040427      * Carico sk con i p.o. emissione da elaborare
058800040426if  1c                   If        I44Poel = 999
058900140625     c                   Clear                   ormpoe
059000140625     c                   Clear                   Trul26ds
059100040426     c                   Eval      I26tpt = 'P'
059200040426     c                   Eval      I26tla = 'L'
059300040426     c                   Call      'TRUL26R'
059400040427     c                   Parm                    Trul26ds
059500040427      * carico i p.o. da elaborare dell'AS in un sk
059600040427do  2c                   Do        250           xb
059700040427     c                   If        Skpot(xb) = *Zeros
059800040427     c                   Leave
059900040427     c                   EndIf
060000040427      * devo cercare quelli che hanno il terminal di partenza = simfel
060100040427     c                   If        Skpot(xb) = Simfel
060200040427     c                   Eval      Skpoel(xa) = Skpo(xb)
060300040427     c                   Add       1             xa
060400040427     c                   Endif
060500040427e   2c                   EndDo
060600040426      * lanciato un solo p.o. carico questo nella sk
060700040426   x1c                   Else
060800040427     c                   Eval      Skpoel(1) = I44Poel
060900140625     c                   eval      ormpoe=i44poel
061000140625    1c                   EndIf
061100040427
061200040427      * giro le date elaborazione
061300040427     c                   Clear                   wlbdat
061400040427     c                   Move      I44Dtda       g02inv
061500040427     c                   Movel     '3'           G02err
061600040427     c                   Call      'XSRDA8'
061700040427     c                   Parm                    wlbdat
061800040427     c                   Movel     g02dat        datadal
061900040427
062000040427     c                   Clear                   wlbdat
062100040427     c                   Move      I44Dtal       g02inv
062200040427     c                   Movel     '3'           G02err
062300040427     c                   Call      'XSRDA8'
062400040427     c                   Parm                    wlbdat
062500040427     c                   Movel     g02dat        dataal
062600040428
062700040428      * ordinamento
062800040428     c                   Select
062900130523     c                   When      I44sel = 'M'
063000040428     c                   Eval      desord = 'MITTENTE'
063100130523     c                   When      I44sel = 'O'
063200040428     c                   Eval      desord = 'ORDINANTE'
063300040428     c                   EndSl
063400040429
063500040429      * clienti codificati
063600040429     c                   Select
063700040429     c                   When      I44Cod = 'S'
063800040429     c                   Eval      descod = 'CODIFICATI'
063900040429     c                   When      I44Cod = 'N'
064000040429     c                   Eval      descod = 'NON CODIFICATI'
064100040429     c                   Other
064200040429     c                   Eval      descod = 'TUTTI'
064300040429     c                   EndSl
064400130524
064500130524     c                   eval      *in40 = *off
064600040426
064700040426     c                   EndSr
