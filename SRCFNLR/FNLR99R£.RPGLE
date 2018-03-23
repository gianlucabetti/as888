000100071001     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200071001     h dftactgrp(*no) actgrp(*caller)
000300030825      * FNLR99R *----------------------------------------------------*
000400930428      *---------*                                                    *
000500030825      * A seconda dei parametri ricevuti il pgm aggiorna ORM:        *
000600030825      * se riceve fase 400 mette  rif. distinta sull'ORM             *
000700030825      * se riceve fase 390 toglie rif. distinta dall'ORM             *
000800030825      * se riceve fase 999 e causale 91 reso spedizione              *
000900041014      * se riceve fase 999 e causale 90 dirottamento (fase 200)      *
001000030825      * Se non vengono passati riferimenti ORM il pgm li reperisce   *
001100030825      * dalla spedizione, quindi ci deve essere almeno un riferimento*
001200030825      * parametri obbligatori:
001300030825      * rif. spediz./ rif. ORM
001400030825      * fase
001500030825      * x 400 rif. distinta
001600030825      * x 390 nulla
001700030825      * x 999 causale
001800030825      * x 999 causale 90 LNA spediz. figlia
001900031003      *
002000031003      * flag di ritorno
002100031006      * ' ' scritta nuova situazione
002200031003      * '1' errore
002300031003      * '2' non fatto
002400960702     F*--------------------------------------------------------------*
002500031020     Ffnorm01L  uF   E           K DISK    commit(i99comit) usropn
002600031020     Ffnorf00f  if a E             DISK    commit(i99comit) usropn
002700070807     ffnorg01l  uf a e           k disk    commit(i99comit) usropn
002800030825     Ffiar501l  iF   E           K DISK
002900060214     FFiar404l  iF   E           K DISK
003000070307     fazorg01l  if   e           k disk
003100071001     ffidst01l  if   e           k disk
003200030730      *---------------------------------------------------------------
003300030730     d* ds esterne
003400030730      *---------------------------------------------------------------
003500030826     D kpjba         e ds
003600030826     D fnlr99ds      e ds
003700030826     D dar5rcc       e ds
003800030910     D TIBS02ds      E DS
003900030910     D DFAR          E DS
004000070904     d fior61ds      e ds
004100070307
004200071001     d og147         e ds
004300071206     d og148         e ds
004400070307     d fior56ds      e ds
004500070716     d dorf01        e ds
004600070924     d ddft          e ds
004700060515
004800070307     d*---------------------------------------------------------------*
004900070307     d* campi di comodo                                               *
005000070307     d*---------------------------------------------------------------*
005100071001     d savnpg          s                   like(i99npg)
005200071001     d savndc          s                   like(i99ndc)
005300071001     d savddc          s                   like(i99ddc)
005400071001     d savfgs          s                   like(i99fgs)
005500070807     d wpkg            s             10  1
005600070807     d wvlm            s             10  3
005700070807     d dataeur         s               d   datfmt(*eur)
005800070807     d dataiso         s               d   datfmt(*iso)
005900070807     d wdata           s              8  0
006000070807     d wora            s              6  0
006100070807     d w0140           s             14  0
006200071001     d data_org        s              8  0 inz
006300071001     d test_simula     s              1    inz
006400911016      *---------------------------------------------------------------*
006500911016      * FLUSSO PRINCIPALE                                             *
006600911016      *---------------------------------------------------------------*
006700030825     C     *ENTRY        PLIST
006800030826     C                   PARM                    kpjba
006900030826     C                   PARM                    fnlr99ds
007000070307
007100031020     c* disabilito comit su FNORM e FNORF a meno che non sia espressamente
007200031020     c* richiesto ('1')
007300031020     c                   if        i99comit <> '1'
007400031020     c                   movel     '0'           i99comit
007500071001     c                   movel     'N'           i99cmt
007600031020     c                   end
007700070807
007800031020     C                   OPEN      FNorm01L
007900031020     C                   OPEN      FNorf00f
008000070807     c                   open      fnorg01l
008100070807
008200030825     c                   clear                   o99err
008300030826     c                   clear                   fnorm000
008400030825     c* chiudo
008500030825     c                   if        i99tla = 'C'
008600031006     c                   exsr      srfine
008700030825     c                   end
008800070807
008900030825     c* controllo parametri ricevuti
009000030825     c                   exsr      srctr
009100070807
009200031006     c                   select
009300031003     c* errore
009400031006     c                   when      *in99
009500030825     c                   movel     '1'           o99err
009600031003     c* non fatto
009700031006     c                   when      *in98
009800031003     c                   movel     '2'           o99err
009900031006     c                   other
010000070807
010100031006     c* scrivo nuova situazione ORM
010200070807      * per filiale con procedura geo orm non attiva
010300071001     c                   if        test_simula = *blanks
010400030825     c                   exsr      sraggio
010500070807     c                   endif
010600070807
010700070807      * scrivo nuova situazione ORM
010800070807      * per filiale con procedura geo orm attiva o in simulazione
010900071001     c                   if        test_simula <> *blanks
011000070807     c                   exsr      sraggionew
011100070807     c                   endif
011200070807
011300031006     c                   endsl
011400040527
011500030825     c* chiudo
011600030825     c                   exsr      srfine
011700070807
011800030730     c**********************************************************************
011900030825     C     srfine        BEGSR
012000030730     c**********************************************************************
012100030825     c                   if        i99tla = ' '
012200030825     c                   seton                                        rt
012300030825     c                   else
012400030825     c                   seton                                        lr
012500030825     c                   end
012600030825     c                   return
012700030730     c                   endsr
012800070807
012900030730     c**********************************************************************
013000030730     C     srctr         BEGSR
013100071001
013200030825     C                   SETOFF                                       99
013300071001
013400071001      * per prima cosa verifico la congruenza dei flag relativi al commit
013500071001      * se apertura file con commit e non si specifica bene che cosa deve fare
013600071001      * il commit --> errore
013700071001     c                   if        i99comit = '1' and i99cmt <> 'N' and
013800071001     c                             i99cmt <> 'S'
013900071001     c                   eval      *in99 = *on
014000071001     c                   leavesr
014100071001     c                   endif
014200071001
014300071001     c                   exsr      sr_dataora
014400071001
014500030825     c* fase obbligatoria
014600030825     c                   if        i99fao = 0
014700030825     c                   seton                                        99
014800071001     c                   leavesr
014900030825     c                   end
015000030825     c* rif. spediz. o rif. ORM obbligatori
015100030825     c                   if        i99nsp = 0 and i99nor = 0
015200030825     c                   seton                                        99
015300071001     c                   leavesr
015400030825     c                   end
015500030826     c* rif. spedizione corretti
015600030826     c                   if        i99nsp <> 0 and i99nor = 0
015700030826     c     kar5          chain     fiar501l
015800030826     c                   If        not %Found(Fiar501l)
015900030826     c                   seton                                        99
016000071001     c                   leavesr
016100030826     c                   end
016200030826     c                   movel(p)  ar5uni        dar5rcc
016300030826     c                   eval      i99poe = §ar5rpoe
016400030826     c                   eval      i99nsr = §ar5rnsr
016500030826     c                   eval      i99nor = §ar5rnor
016600030826     c                   eval      i99nrv = §ar5rnrv
016700030826     c                   end
016800030826     c* rif. orm corretti
016900071001     c     korm          chain(n)  fnorm01l
017000030826     c                   if        not %found(fnorm01l)
017100030826     c                   seton                                        99
017200071001     c                   leavesr
017300080206     c                   end
017400071002
017500030910     c* controllo che l'orm NON sia chiuso
017600070716     c                   exsr      sr_fase
017700030910     C* SE ORM è IN FASE >= 900 errore
017800030910    4C     d§farafvv     ifeq      'S'
017900030910     C                   seton                                        99
018000071001     c                   leavesr
018100030910     c                   end
018200030910     c* l'orm non deve essere bollettato
018300030826     c                   if        ormfao >= 900
018400030826     c                   seton                                        99
018500071001     c                   leavesr
018600030826     c                   end
018700030826     c                   eval      §ar5rpoe = ormpoe
018800030826     c                   eval      §ar5rnsr = ormnsr
018900030826     c                   eval      §ar5rnor = ormnor
019000030826     c                   eval      §ar5rnrv = ormnrv
019100060214     c                   eval      ar4n14 = dar5rcc
019200060214      * faccio la chain solo su fiar4 xchè tanto se c'è la bolla in arrivo è sempre una bolla
019300060214      * l'orm non deve avere bolla partenza o arrivo che sia
019400060214     c     kar4          setll     fiar404l                               99
019500030826     c                   if        *in99
019600071001     c                   leavesr
019700030826     c                   end
019800071001
019900071001      * recupero se attiva o meno la procedura GEO ORM sulla filiale gestione della distinta
020000071206      * recupero anche il flag per abilitazione allo scarico PDA
020100071001     c                   clear                   test_simula
020200071001     c                   if        i99fgs <> *zeros
020300071001     c                   clear                   og147
020400071206     c                   clear                   og148
020500071001     c     i99fgs        chain     azorg01l
020600071001     c                   if        %found(azorg01l)
020700071001     c                   eval      og147 = orgde7
020800071206     c                   eval      og148 = orgde8
020900071001     c                   endif
021000071001     c                   if        §ogcgio = 'S' and
021100071001     c                             §ogddao > *zeros
021200071001     c                   move      §ogddao       data_org
021300071001      * controllo se siamo in simulazione
021400071001     c                   if        wdata < data_org
021500071001     c                   eval      test_simula = 'S'
021600071001     c                   else
021700071001     c                   eval      test_simula = 'N'
021800071001     c                   endif
021900071001     c                   endif
022000071206     c                   endif
022100071002
022200071002      * mi salvo i dati della distinta
022300071002     c                   eval      savfgs = i99fgs
022400071002     c                   if        i99npg > *zeros
022500071002     c                   eval      savnpg = i99npg
022600071002     c                   else
022700071002     c                   eval      savnpg = 4
022800071002     c                   endif
022900071002     c                   if        i99ndc > *zeros
023000071002     c                   eval      savndc = i99ndc
023100071002     c                   else
023200071002     c                   eval      savndc = ormndc
023300071002     c                   endif
023400071002     c                   if        i99ddc > *zeros
023500071002     c                   eval      savddc = i99ddc
023600071002     c                   else
023700071002     c                   eval      savddc = ormddc
023800071002     c                   endif
023900071001
024000030825     c* a seconda della fase controllo gli altri parametri
024100030826     c                   select
024200071001
024300030826     c* mette distinta nell'orm
024400030826     c                   when      i99fao = 400
024500030826     c                   if        i99npg = 0 or i99ndc = 0 or i99ddc = 0
024600071001     c                             or i99fgs = 0
024700030826     c                   seton                                        99
024800071001     c                   leavesr
024900030826     c                   end
025000071001      * faccio il controllo se stessa fase
025100030910     c                   if        i99ndc = ormndc and i99ddc = ormddc and
025200121126     c                             (i99fao = ormfao or ORMfao = 410 or
025300121126     c                              ORMfao = 420)
025400031003     c                   seton                                        98
025500071001     c                   leavesr
025600030910     c                   end
025700030826     c                   clear                   i99car
025800071001
025900030826     c* toglie distinta dall'orm
026000030826     c                   when      i99fao = 390
026100031006     c* se l'orm è già in fase 390 non fa nulla
026200031003     c                   if        i99fao = ormfao
026300031003     c                   seton                                        98
026400071001     c                   leavesr
026500031003     c                   end
026600031006     c* se l'orm non è in fase 400 non fa nulla
026700080313      * controllo anche 410 e 420 nuove fasi di ritorno dati da PDA
026800080313     c                   if        ormfao <> 400 and ormfao <> 410 and
026900080313     c                             ormfao <> 420
027000031006     c                   seton                                        99
027100071001     c                   leavesr
027200031006     c                   end
027300071001      * se filiale non attiva GEO ORM
027400071002      * pulisco dati distinta passati
027500071001     c                   if        test_simula = *blanks
027600030826     c                   clear                   i99npg
027700030826     c                   clear                   i99ndc
027800030826     c                   clear                   i99ddc
027900030826     c                   clear                   i99car
028000071001     c                   endif
028100071001
028200030826     c* per fase 999 (CHIUSURA) obbligatoria la causale
028300030826     c                   when      i99fao = 999
028400030826     c                   if        i99car = *blanks
028500030826     c                   seton                                        99
028600071001     c                   leavesr
028700070807     c                   endif
028800030826     c* per fase 999 (CHIUSURA) e causale 90 (ERRATO INSTRADAMENTO)
028900030826     c* dirottamento o cambio di porto obbligatorio il p.o. di arrivo della
029000030826     c* nuova bolla
029100030826     c                   if        i99car = '90' and i99lna = 0
029200030826     c                   seton                                        99
029300071001     c                   leavesr
029400030826     c                   end
029500071001
029600030826     c                   endsl
029700070807
029800911017     C                   ENDSR
029900070807
030000031006     C*-----------------------------------------------------*
030100031006     C* sraggio - aggiorno FNORM e FNORF                    *
030200031006     C*-----------------------------------------------------*
030300031006     c     sraggio       begsr
030400071001
030500071001     c     korm          chain     fnorm01l
030600070307
030700040527      * Scrivo fase 200
030800040527     c                   if        i99fao = 999 and i99car = '90'
030900040527     c                   Clear                   Fnorf000
031000040527     c                   Eval      OrfPoe = OrmPoe
031100040527     c                   Eval      OrfNsr = OrmNsr
031200040527     c                   Eval      OrfNor = OrmNor
031300040527     c                   Eval      OrfNrv = OrmNrv
031400040527     c                   Eval      OrfPog = OrmPor
031500071001     c                   exsr      sr_dataora
031600071001     c                   eval      orfdae = wdata
031700071001     c                   eval      orfore = wora
031800040527     c                   Eval      OrfFar = 200
031900040527     c                   Eval      OrfPue = knmus
032000040527     c                   Write     Fnorf000
032100070807      * devo aggiornare fnorg con la nuova filiale ritiro
032200070807     c     korm          chain     fnorg01l
032300070807     c                   if        not %found(fnorg01l)
032400070807     c                   clear                   fnorg000
032500070807      * calcolo il peso e il volume per fnorg
032600070807     c                   exsr      sr_pesvol
032700070807     c                   eval      orgpoe = ormpoe
032800070807     c                   eval      orgnsr = ormnsr
032900070807     c                   eval      orgnor = ormnor
033000070807     c                   eval      orgnrv = ormnrv
033100070808     c                   eval      orgpor = i99lna
033200070807     c                   eval      orgpkg = wpkg
033300070807     c                   eval      orgvlm = wvlm
033400070807     c                   write     fnorg000
033500070807     c                   endif
033600070807     c                   if        %found(fnorg01l)
033700070808     c                   eval      orgpor = i99lna
033800070807     c                   update    fnorg000
033900070807     c                   endif
034000040527     c                   endif
034100040527
034200031006     c* FNORM
034300030826     c                   select
034400040527      *
034500030826     c                   when      i99fao = 400 or i99fao = 390
034600030826     C                   Z-ADD     i99npg        ormnpg
034700030826     C                   Z-ADD     i99ndc        ormndc
034800030826     C                   Z-ADD     i99ddc        ormddc
034900040527     C                   Z-ADD     i99fao        ormfao
035000071001     c                   exsr      sr_dataora
035100071001     c                   eval      ormdfo = wdata
035200071001     C                   eval      ormofo = wora
035300040527
035400030826     c                   when      i99fao = 999 and i99car = '90'
035500030826     C                   Z-ADD     i99lna        ormpor
035600040527     c                   eval      ormfao = 100
035700040527     c                   Eval      OrmDfo = OrfDae
035800040527     c                   Eval      OrmOfo = OrfOre
035900040527     c                   add       1             OrmOfo
036000041013      * pulisco i dati della distinta
036100040527     c                   Clear                   OrmNpg
036200040527     c                   Clear                   OrmNdc
036300040527     c                   Clear                   OrmDdc
036400040527     c                   Clear                   OrmDst
036500040527
036600040527     c                   other
036700071001     c                   exsr      sr_dataora
036800071001     c                   eval      ormdfo = wdata
036900071001     C                   eval      ormofo = wora
037000040527     C                   Z-ADD     i99fao        ormfao
037100030826     c                   endsl
037200040527
037300070716     c                   exsr      sr_fase
037400070716     c                   eval      ormeti = d§farass
037500071001     c                   z-add     wdata         OrmDtt
037600030826     C                   UPDATE    fnorm000
037700030826     c* FNORF
037800030826     c                   clear                   fnorf000
037900030826     c                   eval      orfpoe = ormpoe
038000030826     c                   eval      orfnsr = ormnsr
038100030826     c                   eval      orfnor = ormnor
038200030826     c                   eval      orfnrv = ormnrv
038300040527
038400040527     c                   if        i99fao = 999 and i99car = '90'
038500040527     c                   Eval      OrfFar = OrmFao
038600040527     c                   else
038700040527     c                   eval      orffar = i99fao
038800040527     c                   eval      orfcar = i99car
038900040527     c                   endif
039000040527
039100041013     c                   Eval      OrfPog = OrmPor
039200030826     c                   eval      orfdae = ormdfo
039300030826     c                   eval      orfore = ormofo
039400030826     c                   eval      orfpue = knmus
039500040910     c                   If        OrmNdc > *Zeros
039600040910     c                   Eval      OrfNdc = OrmNdc
039700040910     c                   Eval      OrfDdc = OrmDdc
039800071001     c                   eval      orffgs = savfgs
039900041014     c                   Else
040000041014     c                   If        i99fao = 390
040100041014     c                   Eval      OrfNdc = savNdc
040200041014     c                   Eval      OrfDdc = savDdc
040300071001     c                   eval      orffgs = savfgs
040400041014     c                   EndIf
040500040910     c                   EndIf
040600070716      * se fase 400 imposto flag assegnazione
040700070716     c                   if        orffar = 400
040800070716     c                   eval      §orfass = 'M'
040900070716     c                   eval      orfflo = dorf01
041000070716     c                   endif
041100030826     c                   write     fnorf000
041200030730     c*
041300030730     c                   endsr
041400070807
041500070807      *--------------------------------------------------------------------*
041600070807      * Calcolo peso e volume
041700070807      *--------------------------------------------------------------------*
041800070807     c     sr_pesvol     begsr
041900070924
042000070924      * recupero rapporto peso volume
042100070924     c                   clear                   ddft
042200070924     c                   clear                   TIBS02DS
042300070924     c                   movel     'C'           T02mod
042400070924     c                   movel     knsif         t02sif
042500070924     c                   movel     'DFT'         t02cod
042600070924     c                   movel(p)  i99lna        T02ke1
042700070924     c                   call      'TIBS02R'
042800070924     c                   parm                    KPJBA
042900070924     c                   parm                    TIBS02DS
043000070924     c                   if        t02err =  *blanks
043100070924     c                   movel     t02uni        ddft
043200070924     c                   else
043300070924      * poi con 999 generico se non trovo con il mio p.o.
043400070924     C                   clear                   TIBS02DS
043500070924     C                   movel     'C'           T02mod
043600070924     C                   movel     knsif         t02sif
043700070924     C                   movel     'DFT'         t02cod
043800070924     C                   movel(p)  '999'         T02ke1
043900070924     C                   call      'TIBS02R'
044000070924     C                   parm                    KPJBA
044100070924     C                   parm                    TIBS02DS
044200070924     C                   if        t02err =  *blanks
044300070924     C                   movel     t02uni        ddft
044400070924     C                   endif
044500070924     C                   endif
044600070924      * se i dati relativi al peso e volume sono a zero li imposto a di dft io
044700070924      * caso che non dovrebbe capitare ma se capita nella routine 'pesvol' il
044800070924      * programma si spacca
044900070924     c                   if        d§dftpkg = *zeros
045000070924     c                   eval      d§dftpkg = 200
045100070924     c                   endif
045200070924     c                   if        d§dftbnc = *zeros
045300070924     c                   eval      d§dftbnc = 1
045400070924     c                   endif
045500070807
045600070807     c                   eval      wpkg = ormpkg
045700070807     c                   eval      wvlm = ormvlm
045800070807
045900070807      * se non inserito peso a video lo calcolo per memorizzarlo su fnorg00f
046000070807     c                   if        wpkg = *zeros
046100070807     c                   select
046200070924 b3  c                   when      ormbnc <> *zeros
046300070924     c                   eval      wpkg = ormbnc / d§dftbnc * d§dftpkg
046400070807 b2  c                   when      ormvlm <> *zeros
046500070924     c                   eval      wpkg = ormvlm * d§dftpkg
046600070807 e3  c                   endsl
046700070807      * se troppo alto il peso lo imposto al massimo
046800070807     c                   if        wpkg > 999999,9
046900070807     c                   eval      wpkg = 999999,9
047000070807     c                   endif
047100070807     c                   endif
047200070807
047300070807      * se non inserito volume a video lo calcolo per memorizzarlo su fnorg00f
047400070807     c                   if        wvlm = *zeros
047500070807     c                   select
047600070924 b3  c                   when      ormbnc <> *zeros
047700070924     c                   eval(h)   wvlm = ormbnc / d§dftbnc
047800070924 b2  c                   when      ormpkg <> *zeros
047900070924     c                   eval(h)   wvlm = ormpkg / d§dftpkg
048000070807 e3  c                   endsl
048100070807      * se troppo alto il volume lo imposto al massimo
048200070807     c                   if        wvlm > 99,999
048300070807     c                   eval      wvlm = 99,990
048400070807     c                   endif
048500070807     c                   endif
048600070807
048700070807     c                   endsr
048800070807
048900070807      *-----------------------------------------------------*
049000070807      * sraggionew - aggiorno i dati per GEO attivo su ORM
049100070807      *-----------------------------------------------------*
049200070807     c     sraggionew    begsr
049300070807
049400070807     c                   select
049500070807      * D I R O T T A
049600070807     c                   when      i99fao = 999 and i99car = '90'
049700071001     c     korm          chain     fnorm01l
049800071206     c                   eval      savndc = ormndc
049900071206     c                   eval      savddc = ormddc
050000070807      * scrivo fase 200 senza rif. distinta
050100070807     c                   clear                   fnorf000
050200070807     c                   eval      orfpoe = ormpoe
050300070807     c                   eval      orfnsr = ormnsr
050400070807     c                   eval      orfnor = ormnor
050500070807     c                   eval      orfnrv = ormnrv
050600070807     c                   eval      orfpog = ormpor
050700070807     c                   exsr      sr_dataora
050800070807     c                   eval      orfdae = wdata
050900070807     c                   eval      orfore = wora
051000070807     c                   eval      orffar = 200
051100070807     c                   eval      orfpue = knmus
051200070807     c                   write     fnorf000
051300070807      * scrive fase 100 senza rif. distinta ma con nuova filiale ritiro
051400070807     c                   clear                   fnorf000
051500070807     c                   eval      orfpoe = ormpoe
051600070807     c                   eval      orfnsr = ormnsr
051700070807     c                   eval      orfnor = ormnor
051800070807     c                   eval      orfnrv = ormnrv
051900070807     c                   eval      orfpog = i99lna
052000070807     c                   eval      orfdae = wdata
052100070807     c                   eval      orfore = wora
052200070807     c                   add       1             orfore
052300070807     c                   eval      orffar = 100
052400070807     c                   eval      orfpue = knmus
052500070807     c                   write     fnorf000
052600070807      * aggiorna filiale ritiro su FNORG e toglie rif. distinta e fase prec.
052700080313      * devo anche pulire il giro
052800070807      * non mi interessa se siamo in simulazione o no
052900070807     c     korm          chain     fnorg01l
053000070807     c                   if        %found(fnorg01l)
053100070807     c                   eval      orgpor = i99lna
053200080313     c                   clear                   orgtgi
053300080313     c                   clear                   orgpocgi
053400080313     c                   clear                   orgcgi
053500070807     c                   clear                   orgpdc
053600070807     c                   clear                   orgfgs
053700070807     c                   clear                   orgndc
053800070807     c                   clear                   orgddc
053900070807     c                   clear                   orgnftl
054000070904     c                   clear                   orgslo
054100071001     c                   clear                   orgdtvdis
054200071001     c                   clear                   orghvdis
054300080313     c                   eval      orgdtvtel = orfdae
054400080313     c                   eval      orghvtel = orfore
054500070807     c                   clear                   orgfao
054600070807     c                   clear                   orgdfo
054700070807     c                   clear                   orgofo
054800070807     c                   update    fnorg000
054900070807     c                   endif
055000070807      * aggiorna testata ORM senza rif. distinta e con nuova fil. ritiro
055100070807      * toglie flag di orm stampato
055200070807     c                   eval      ormpor = i99lna
055300070807     c                   eval      ormfao = orffar
055400070807     c                   eval      ormdfo = orfdae
055500070807     c                   eval      ormofo = orfore
055600070807     c                   clear                   ormnpg
055700070807     c                   clear                   ormndc
055800070807     c                   clear                   ormddc
055900070807     c                   clear                   ormdst
056000070807     c                   exsr      sr_fase
056100070807     c                   eval      ormeti = d§farass
056200070807     c                   eval      ormdtt = wdata
056300070807     c                   update    fnorm000
056400071001      * gestisco il commit
056500071001     c                   if        i99cmt = 'S'
056600071001     c                   commit
056700071001     c                   endif
056800071206      * se filiale abilitata allo scarico PDA richiamo aggiorno
056900071206      * e già partita con GEO ORM
057000080221     c                   if        §ogpdaorm <> *blanks
057100080221     c                             and test_simula = 'N'
057200071206     c                   exsr      sr_aggpdo
057300071206     c                   endif
057400070807
057500070807      * C H I U D E
057600070807     c                   when      i99fao = 999
057700071001     c     korm          chain     fnorm01l
057800070807      * scrivo fase 999 con rif. distinta
057900070807     c                   clear                   fnorf000
058000070807     c                   eval      orfpoe = ormpoe
058100070807     c                   eval      orfnsr = ormnsr
058200070807     c                   eval      orfnor = ormnor
058300070807     c                   eval      orfnrv = ormnrv
058400070807     c                   eval      orfpog = ormpor
058500070807     c                   exsr      sr_dataora
058600070807     c                   eval      orfdae = wdata
058700070807     c                   eval      orfore = wora
058800070904     c                   eval      orffar = i99fao
058900070807     c                   eval      orfcar = i99car
059000070807     c                   eval      orfpue = knmus
059100071001     c                   eval      orffgs = i99fgs
059200071002     c                   eval      orfndc = savndc
059300071002     c                   eval      orfddc = savddc
059400070807     c                   write     fnorf000
059500070807      * non faccio niente su FNORG
059600070807      * aggiorna testata ORM con rif. distinta
059700070807     c                   eval      ormfao = orffar
059800070807     c                   eval      ormdfo = orfdae
059900070807     c                   eval      ormofo = orfore
060000071002     c                   eval      ormnpg = savnpg
060100071002     c                   eval      ormndc = savndc
060200071002     c                   eval      ormddc = savddc
060300070807     c                   exsr      sr_fase
060400070807     c                   eval      ormeti = d§farass
060500070807     c                   eval      ormdtt = wdata
060600070807     c                   update    fnorm000
060700071001      * gestisco il commit
060800071001     c                   if        i99cmt = 'S'
060900071001     c                   commit
061000071001     c                   endif
061100071206      * se filiale abilitata allo scarico PDA richiamo aggiorno
061200080221      * e già partita con GEO ORM
061300080221     c                   if        §ogpdaorm <> *blanks
061400080221     c                             and test_simula = 'N'
061500071206     c                   exsr      sr_aggpdo
061600071206     c                   endif
061700070807
061800070807      * A S S E G N A
061900070807     c                   when      i99fao = 400
062000071001     c                   clear                   dstpdr
062100071001     c     kfidst        chain     fidst01l
062200070807      * aggiorna FNORG
062300070807     c     korm          chain     fnorg01l
062400070807     c                   if        %found(fnorg01l)
062500071001     c                   eval      orgfgs = i99fgs
062600070807     c                   eval      orgpdc = dstpdr
062700071002     c                   eval      orgndc = savndc
062800071002     c                   eval      orgddc = savddc
062900070807     c                   clear                   orgnftl
063000070807     c                   clear                   orgslo
063100070807     c                   exsr      sr_dataora
063200070807     c                   eval      orgdtvdis = wdata
063300070807     c                   eval      orghvdis = wora
063400070807     c                   update    fnorg000
063500070807     c                   endif
063600071023      * aggiorna ORM
063700080206      * se sono in simulazione subito
063800080206     c                   if        test_simula = 'S'
063900080206     c     korm          chain     fnorm01l
064000080206      * scrivo fase 400 con rif. distinta
064100080206     c                   clear                   fnorf000
064200080206     c                   eval      orfpoe = ormpoe
064300080206     c                   eval      orfnsr = ormnsr
064400080206     c                   eval      orfnor = ormnor
064500080206     c                   eval      orfnrv = ormnrv
064600080206     c                   eval      orfpog = ormpor
064700080206     c                   exsr      sr_dataora
064800080206     c                   eval      orfdae = wdata
064900080206     c                   eval      orfore = wora
065000080206     c                   eval      orffar = i99fao
065100080206     c                   eval      orfpue = knmus
065200080206     c                   eval      orffgs = i99fgs
065300080206     c                   eval      orfndc = savndc
065400080206     c                   eval      orfddc = savddc
065500080206     c                   write     fnorf000
065600080206      * aggiorna testata ORM con rif. distinta
065700080206     c                   eval      ormfao = orffar
065800080206     c                   eval      ormdfo = orfdae
065900080206     c                   eval      ormofo = orfore
066000080206     c                   eval      ormnpg = savnpg
066100080206     c                   eval      ormndc = savndc
066200080206     c                   eval      ormddc = savddc
066300080206     c                   exsr      sr_fase
066400080206     c                   eval      ormeti = d§farass
066500080206     c                   eval      ormdtt = wdata
066600080206     c                   update    fnorm000
066700080206      * gestisco il commit
066800080206     c                   if        i99cmt = 'S'
066900080206     c                   commit
067000080206     c                   endif
067100080206     c                   endif
067200080206      * se sono partita aggiorno dopo con fior61r
067300080206     c                   if        test_simula = 'N'
067400120123      /free
067500120123       //?Visto che può capitare di avere ORM RC ancora con n.distinta su FNORM
067600120123       //?prima di richiamare il pgm di assegnazione vado a svuotare il campo della
067700120123       //?distinta sulla testata dell'ORM
067800120123       //?prima di richiamare il FIOR61R
067900120123         chain (I99poe:I99nsr:I99nor:I99nrv) FNORM01L;
068000120123         IF  %found(FNORM01L);
068100120123           clear ORMndc;
068200120123           update fnorm000;
068300120123         ENDIF;
068400120123      /end-free
068500120123
068600080206     c                   clear                   fior61ds
068700071001     c                   eval      or61fgsi = i99fgs
068800071002     c                   eval      or61dfvi = savddc
068900070921     c                   eval      or61mtdi = 'M'
069000071002     c                   eval      or61nfvi = savndc
069100071001     c                   eval      or61rc = 'S'
069200071002     c                   eval      or61nftl = i99nftl
069300071002      * gestisco il commit
069400071002     c                   eval      or61cmti = 'N'
069500071002     c                   if        i99cmt = 'N' and i99comit = '0'
069600071002     c                   eval      or61cmti = 'S'
069700071002     c                   endif
069800071002      * gestisco l'apertura dei file sotto commit
069900071002     c                   if        i99comit = '0'
070000071002     c                   call      'FIOR61C'
070100071002     c                   parm                    kpjba
070200071002     c                   parm                    fior61ds
070300071002     c                   else
070400071002     c                   call      'FIOR61R'
070500071002     c                   parm                    kpjba
070600071002     c                   parm                    fior61ds
070700071002     c                   endif
070800071001     c                   if        i99cmt = 'S'
070900070807     c                   if        or61erro <> *blanks
071000070807      * problemi durante l'aggiornamento --> eseguire rollback  ?
071100070807     c                   rolbk
071200070807     c                   else
071300070807      * confermo le variazioni fatte
071400070807     c                   commit
071500070807     c                   endif
071600070807     c                   endif
071700080206     c                   endif
071800070807
071900070807      * D I S A S S E G N A
072000070807     c                   when      i99fao = 390
072100070807      * aggiorna FNORG
072200070807     c     korm          chain     fnorg01l
072300070807     c                   if        %found(fnorg01l)
072400070807     c                   clear                   orgfgs
072500070807     c                   clear                   orgpdc
072600070807     c                   clear                   orgndc
072700070807     c                   clear                   orgddc
072800070807     c                   clear                   orgnftl
072900070807     c                   clear                   orgslo
073000070807     c                   clear                   orgdtvdis
073100070807     c                   clear                   orghvdis
073200070807     c                   update    fnorg000
073300070807     c                   endif
073400080206      * aggiorna ORM
073500080206      * se sono in simulazione subito
073600080206     c                   if        test_simula = 'S'
073700080206     c     korm          chain     fnorm01l
073800080206      * scrivo fase 390 con rif. distinta
073900080206     c                   clear                   fnorf000
074000080206     c                   eval      orfpoe = ormpoe
074100080206     c                   eval      orfnsr = ormnsr
074200080206     c                   eval      orfnor = ormnor
074300080206     c                   eval      orfnrv = ormnrv
074400080206     c                   eval      orfpog = ormpor
074500080206     c                   exsr      sr_dataora
074600080206     c                   eval      orfdae = wdata
074700080206     c                   eval      orfore = wora
074800080206     c                   eval      orffar = i99fao
074900080206     c                   eval      orfcar = i99car
075000080206     c                   eval      orfpue = knmus
075100080206     c                   eval      orffgs = i99fgs
075200080206     c                   eval      orfndc = savndc
075300080206     c                   eval      orfddc = savddc
075400080206     c                   write     fnorf000
075500080206      * aggiorna testata ORM senza rif. distinta
075600080206     c                   eval      ormfao = orffar
075700080206     c                   eval      ormdfo = orfdae
075800080206     c                   eval      ormofo = orfore
075900080206     c                   clear                   ormnpg
076000080206     c                   clear                   ormndc
076100080206     c                   clear                   ormddc
076200080206     c                   exsr      sr_fase
076300080206     c                   eval      ormeti = d§farass
076400080206     c                   eval      ormdtt = wdata
076500080206     c                   update    fnorm000
076600080206      * gestisco il commit
076700080206     c                   if        i99cmt = 'S'
076800080206     c                   commit
076900080206     c                   endif
077000080206     c                   endif
077100080206      * se sono partita aggiorno dopo con fior61r
077200080206     c                   if        test_simula = 'N'
077300071001     c                   clear                   fior61ds
077400071023      * aggiorna ORM
077500071001     c                   eval      or61fgsi = i99fgs
077600071002     c                   eval      or61dfvi = savddc
077700070807     c                   eval      or61mtdi = 'T'
077800071002     c                   eval      or61nfvi = savndc
077900071001     c                   eval      or61rc = 'S'
078000071002     c                   eval      or61nftl = i99nftl
078100071002      * gestisco il commit
078200071002     c                   eval      or61cmti = 'N'
078300071002     c                   if        i99cmt = 'N' and i99comit = '0'
078400071002     c                   eval      or61cmti = 'S'
078500071002     c                   endif
078600071002      * gestisco l'apertura dei file sotto commit
078700071002     c                   if        i99comit = '0'
078800071002     c                   call      'FIOR61C'
078900071002     c                   parm                    kpjba
079000071002     c                   parm                    fior61ds
079100071002     c                   else
079200071002     c                   call      'FIOR61R'
079300071002     c                   parm                    kpjba
079400071002     c                   parm                    fior61ds
079500071002     c                   endif
079600071001     c                   if        i99cmt = 'S'
079700070807     c                   if        or61erro <> *blanks
079800070807      * problemi durante l'aggiornamento --> eseguire rollback  ?
079900070807     c                   rolbk
080000070807     c                   else
080100070807      * confermo le variazioni fatte
080200070807     c                   commit
080300070807     c                   endif
080400070807     c                   endif
080500080206     c                   endif
080600070807
080700070807     c                   endsl
080800070807
080900070807     c                   endsr
081000070807
081100070807      *------------------------------------------------------------------------*
081200070807      *   Imposto data e ora del momento
081300070807      *------------------------------------------------------------------------*
081400070807     c     sr_dataora    begsr
081500070807
081600070807     c                   time                    w0140
081700070807      * ora
081800070807     c                   movel     w0140         wora
081900070807      * data
082000070807     c                   move      w0140         wdata
082100070807     c     *eur          move      wdata         dataeur
082200070807     c                   move      dataeur       dataiso
082300070807     c                   move      dataiso       wdata
082400070807
082500070807     c                   endsr
082600070307
082700070307      *-----------------------------------------------------*
082800070307      * sr_aggpdo - aggiorno i dati su FIPDO se necessario  *
082900070307      *-----------------------------------------------------*
083000070307     c     sr_aggpdo     begsr
083100070307
083200070307     c                   clear                   fior56ds
083300080206      * se fase 999 richiamo con 'A'
083400071206      * il togli e metti dalla distinta richiama il FIOR61R e ci pensa lui a richiamare
083500071206      * il fior56r per il PDA
083600071206     c                   if        i99fao = 999
083700070307     c                   eval      or56tla = 'A'
083800140120     c                   eval      or56cmd = 'A'
083900071206     c                   endif
084000080313      * se richiamato con apertura dei file sotto comit e il comit non lo fa questo pgm
084100080313      * richiamo Fior56 con apertura fnorg con comit
084200080313     c                   if        i99comit = '1'
084300080313     c                   eval      or56comit = '1'
084400080313     c                   endif
084500071001     c                   eval      or56fgs = savfgs
084600071001     c                   eval      or56ddc = savddc
084700070307     c                   eval      or56poe = ormpoe
084800070307     c                   eval      or56nsr = ormnsr
084900070307     c                   eval      or56nor = ormnor
085000070307     c                   eval      or56nrv = ormnrv
085100071206     c                   eval      or56ndcd = savndc
085200071206     c                   eval      or56ndca = savndc
085300070307     c                   call      'FIOR56R'
085400070307     c                   parm                    kpjbu
085500070307     c                   parm                    fior56ds
085600070307      * il pgm torna degli errori ma non ci faccio niente
085700070307
085800070307     c                   endsr
085900070716
086000070716      *--------------------------------------------------------------------------------------------*
086100070716      *  Tabella Fase
086200070716      *--------------------------------------------------------------------------------------------*
086300070716     c     sr_fase       begsr
086400070716
086500070716     c                   clear                   tibs02ds
086600070716     c                   clear                   dfar
086700070716     c                   eval      t02mod = 'C'
086800070716     c                   eval      t02sif = knsif
086900070716     c                   eval      t02cod = 'FAR'
087000070716     c                   movel(p)  ormfao        t02ke1
087100070716     c                   call      'TIBS02R'
087200070716     c                   parm                    kpjba
087300070716     c                   parm                    tibs02ds
087400070716     c                   eval      dfar = t02uni
087500070716
087600070716     c                   endsr
087700060515
087800940322     C*-----------------------------------------------------*
087900941107     C*    Operazioni iniziali
088000941107     C*-----------------------------------------------------*
088100940322     C     *INZSR        BEGSR
088200070807
088300030826     C     Kar5          KLIST
088400030826     C                   KFLD                    i99aas
088500030826     C                   KFLD                    i99LNP
088600030826     C                   KFLD                    i99NRS
088700030826     C                   KFLD                    i99NSP
088800030826     C                   KFLD                    ar5trd
088900030826     c                   eval      ar5trd = 'RCC'
089000070807
089100030826     C     Korm          KLIST
089200030826     C                   KFLD                    i99poe
089300030826     C                   KFLD                    i99nsr
089400030826     C                   KFLD                    i99nor
089500030826     C                   KFLD                    i99nrv
089600070307
089700031006     C     Korf          KLIST
089800070307     C                   KFLD                    ormpoe
089900070307     C                   KFLD                    ormnsr
090000070307     C                   KFLD                    ormnor
090100070307     C                   KFLD                    ormnrv
090200031006     C                   KFLD                    ormdfo
090300031006     C                   KFLD                    ormofo
090400070307     C                   KFLD                    ormfao
090500070307
090600060214     C     Kar4          KLIST
090700060214     C                   KFLD                    ar4trc
090800060214     C                   KFLD                    ar4n14
090900060214     c                   eval      ar4trc = 'M'
091000071001
091100071001     c     kfidst        klist
091200071001     c                   kfld                    savnpg
091300071001     c                   kfld                    savndc
091400071001     c                   kfld                    savfgs
091500071001
091600940322     C                   ENDSR
