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
025200030910     c                             i99fao = ormfao
025300031003     c                   seton                                        98
025400071001     c                   leavesr
025500030910     c                   end
025600030826     c                   clear                   i99car
025700071001
025800030826     c* toglie distinta dall'orm
025900030826     c                   when      i99fao = 390
026000031006     c* se l'orm è già in fase 390 non fa nulla
026100031003     c                   if        i99fao = ormfao
026200031003     c                   seton                                        98
026300071001     c                   leavesr
026400031003     c                   end
026500031006     c* se l'orm non è in fase 400 non fa nulla
026600080313      * controllo anche 410 e 420 nuove fasi di ritorno dati da PDA
026700080313     c                   if        ormfao <> 400 and ormfao <> 410 and
026800080313     c                             ormfao <> 420
026900031006     c                   seton                                        99
027000071001     c                   leavesr
027100031006     c                   end
027200071001      * se filiale non attiva GEO ORM
027300071002      * pulisco dati distinta passati
027400071001     c                   if        test_simula = *blanks
027500030826     c                   clear                   i99npg
027600030826     c                   clear                   i99ndc
027700030826     c                   clear                   i99ddc
027800030826     c                   clear                   i99car
027900071001     c                   endif
028000071001
028100030826     c* per fase 999 (CHIUSURA) obbligatoria la causale
028200030826     c                   when      i99fao = 999
028300030826     c                   if        i99car = *blanks
028400030826     c                   seton                                        99
028500071001     c                   leavesr
028600070807     c                   endif
028700030826     c* per fase 999 (CHIUSURA) e causale 90 (ERRATO INSTRADAMENTO)
028800030826     c* dirottamento o cambio di porto obbligatorio il p.o. di arrivo della
028900030826     c* nuova bolla
029000030826     c                   if        i99car = '90' and i99lna = 0
029100030826     c                   seton                                        99
029200071001     c                   leavesr
029300030826     c                   end
029400071001
029500030826     c                   endsl
029600070807
029700911017     C                   ENDSR
029800070807
029900031006     C*-----------------------------------------------------*
030000031006     C* sraggio - aggiorno FNORM e FNORF                    *
030100031006     C*-----------------------------------------------------*
030200031006     c     sraggio       begsr
030300071001
030400071001     c     korm          chain     fnorm01l
030500070307
030600040527      * Scrivo fase 200
030700040527     c                   if        i99fao = 999 and i99car = '90'
030800040527     c                   Clear                   Fnorf000
030900040527     c                   Eval      OrfPoe = OrmPoe
031000040527     c                   Eval      OrfNsr = OrmNsr
031100040527     c                   Eval      OrfNor = OrmNor
031200040527     c                   Eval      OrfNrv = OrmNrv
031300040527     c                   Eval      OrfPog = OrmPor
031400071001     c                   exsr      sr_dataora
031500071001     c                   eval      orfdae = wdata
031600071001     c                   eval      orfore = wora
031700040527     c                   Eval      OrfFar = 200
031800040527     c                   Eval      OrfPue = knmus
031900040527     c                   Write     Fnorf000
032000070807      * devo aggiornare fnorg con la nuova filiale ritiro
032100070807     c     korm          chain     fnorg01l
032200070807     c                   if        not %found(fnorg01l)
032300070807     c                   clear                   fnorg000
032400070807      * calcolo il peso e il volume per fnorg
032500070807     c                   exsr      sr_pesvol
032600070807     c                   eval      orgpoe = ormpoe
032700070807     c                   eval      orgnsr = ormnsr
032800070807     c                   eval      orgnor = ormnor
032900070807     c                   eval      orgnrv = ormnrv
033000070808     c                   eval      orgpor = i99lna
033100070807     c                   eval      orgpkg = wpkg
033200070807     c                   eval      orgvlm = wvlm
033300070807     c                   write     fnorg000
033400070807     c                   endif
033500070807     c                   if        %found(fnorg01l)
033600070808     c                   eval      orgpor = i99lna
033700070807     c                   update    fnorg000
033800070807     c                   endif
033900040527     c                   endif
034000040527
034100031006     c* FNORM
034200030826     c                   select
034300040527      *
034400030826     c                   when      i99fao = 400 or i99fao = 390
034500030826     C                   Z-ADD     i99npg        ormnpg
034600030826     C                   Z-ADD     i99ndc        ormndc
034700030826     C                   Z-ADD     i99ddc        ormddc
034800040527     C                   Z-ADD     i99fao        ormfao
034900071001     c                   exsr      sr_dataora
035000071001     c                   eval      ormdfo = wdata
035100071001     C                   eval      ormofo = wora
035200040527
035300030826     c                   when      i99fao = 999 and i99car = '90'
035400030826     C                   Z-ADD     i99lna        ormpor
035500040527     c                   eval      ormfao = 100
035600040527     c                   Eval      OrmDfo = OrfDae
035700040527     c                   Eval      OrmOfo = OrfOre
035800040527     c                   add       1             OrmOfo
035900041013      * pulisco i dati della distinta
036000040527     c                   Clear                   OrmNpg
036100040527     c                   Clear                   OrmNdc
036200040527     c                   Clear                   OrmDdc
036300040527     c                   Clear                   OrmDst
036400040527
036500040527     c                   other
036600071001     c                   exsr      sr_dataora
036700071001     c                   eval      ormdfo = wdata
036800071001     C                   eval      ormofo = wora
036900040527     C                   Z-ADD     i99fao        ormfao
037000030826     c                   endsl
037100040527
037200070716     c                   exsr      sr_fase
037300070716     c                   eval      ormeti = d§farass
037400071001     c                   z-add     wdata         OrmDtt
037500030826     C                   UPDATE    fnorm000
037600030826     c* FNORF
037700030826     c                   clear                   fnorf000
037800030826     c                   eval      orfpoe = ormpoe
037900030826     c                   eval      orfnsr = ormnsr
038000030826     c                   eval      orfnor = ormnor
038100030826     c                   eval      orfnrv = ormnrv
038200040527
038300040527     c                   if        i99fao = 999 and i99car = '90'
038400040527     c                   Eval      OrfFar = OrmFao
038500040527     c                   else
038600040527     c                   eval      orffar = i99fao
038700040527     c                   eval      orfcar = i99car
038800040527     c                   endif
038900040527
039000041013     c                   Eval      OrfPog = OrmPor
039100030826     c                   eval      orfdae = ormdfo
039200030826     c                   eval      orfore = ormofo
039300030826     c                   eval      orfpue = knmus
039400040910     c                   If        OrmNdc > *Zeros
039500040910     c                   Eval      OrfNdc = OrmNdc
039600040910     c                   Eval      OrfDdc = OrmDdc
039700071001     c                   eval      orffgs = savfgs
039800041014     c                   Else
039900041014     c                   If        i99fao = 390
040000041014     c                   Eval      OrfNdc = savNdc
040100041014     c                   Eval      OrfDdc = savDdc
040200071001     c                   eval      orffgs = savfgs
040300041014     c                   EndIf
040400040910     c                   EndIf
040500070716      * se fase 400 imposto flag assegnazione
040600070716     c                   if        orffar = 400
040700070716     c                   eval      §orfass = 'M'
040800070716     c                   eval      orfflo = dorf01
040900070716     c                   endif
041000030826     c                   write     fnorf000
041100030730     c*
041200030730     c                   endsr
041300070807
041400070807      *--------------------------------------------------------------------*
041500070807      * Calcolo peso e volume
041600070807      *--------------------------------------------------------------------*
041700070807     c     sr_pesvol     begsr
041800070924
041900070924      * recupero rapporto peso volume
042000070924     c                   clear                   ddft
042100070924     c                   clear                   TIBS02DS
042200070924     c                   movel     'C'           T02mod
042300070924     c                   movel     knsif         t02sif
042400070924     c                   movel     'DFT'         t02cod
042500070924     c                   movel(p)  i99lna        T02ke1
042600070924     c                   call      'TIBS02R'
042700070924     c                   parm                    KPJBA
042800070924     c                   parm                    TIBS02DS
042900070924     c                   if        t02err =  *blanks
043000070924     c                   movel     t02uni        ddft
043100070924     c                   else
043200070924      * poi con 999 generico se non trovo con il mio p.o.
043300070924     C                   clear                   TIBS02DS
043400070924     C                   movel     'C'           T02mod
043500070924     C                   movel     knsif         t02sif
043600070924     C                   movel     'DFT'         t02cod
043700070924     C                   movel(p)  '999'         T02ke1
043800070924     C                   call      'TIBS02R'
043900070924     C                   parm                    KPJBA
044000070924     C                   parm                    TIBS02DS
044100070924     C                   if        t02err =  *blanks
044200070924     C                   movel     t02uni        ddft
044300070924     C                   endif
044400070924     C                   endif
044500070924      * se i dati relativi al peso e volume sono a zero li imposto a di dft io
044600070924      * caso che non dovrebbe capitare ma se capita nella routine 'pesvol' il
044700070924      * programma si spacca
044800070924     c                   if        d§dftpkg = *zeros
044900070924     c                   eval      d§dftpkg = 200
045000070924     c                   endif
045100070924     c                   if        d§dftbnc = *zeros
045200070924     c                   eval      d§dftbnc = 1
045300070924     c                   endif
045400070807
045500070807     c                   eval      wpkg = ormpkg
045600070807     c                   eval      wvlm = ormvlm
045700070807
045800070807      * se non inserito peso a video lo calcolo per memorizzarlo su fnorg00f
045900070807     c                   if        wpkg = *zeros
046000070807     c                   select
046100070924 b3  c                   when      ormbnc <> *zeros
046200070924     c                   eval      wpkg = ormbnc / d§dftbnc * d§dftpkg
046300070807 b2  c                   when      ormvlm <> *zeros
046400070924     c                   eval      wpkg = ormvlm * d§dftpkg
046500070807 e3  c                   endsl
046600070807      * se troppo alto il peso lo imposto al massimo
046700070807     c                   if        wpkg > 999999,9
046800070807     c                   eval      wpkg = 999999,9
046900070807     c                   endif
047000070807     c                   endif
047100070807
047200070807      * se non inserito volume a video lo calcolo per memorizzarlo su fnorg00f
047300070807     c                   if        wvlm = *zeros
047400070807     c                   select
047500070924 b3  c                   when      ormbnc <> *zeros
047600070924     c                   eval(h)   wvlm = ormbnc / d§dftbnc
047700070924 b2  c                   when      ormpkg <> *zeros
047800070924     c                   eval(h)   wvlm = ormpkg / d§dftpkg
047900070807 e3  c                   endsl
048000070807      * se troppo alto il volume lo imposto al massimo
048100070807     c                   if        wvlm > 99,999
048200070807     c                   eval      wvlm = 99,990
048300070807     c                   endif
048400070807     c                   endif
048500070807
048600070807     c                   endsr
048700070807
048800070807      *-----------------------------------------------------*
048900070807      * sraggionew - aggiorno i dati per GEO attivo su ORM
049000070807      *-----------------------------------------------------*
049100070807     c     sraggionew    begsr
049200070807
049300070807     c                   select
049400070807      * D I R O T T A
049500070807     c                   when      i99fao = 999 and i99car = '90'
049600071001     c     korm          chain     fnorm01l
049700071206     c                   eval      savndc = ormndc
049800071206     c                   eval      savddc = ormddc
049900070807      * scrivo fase 200 senza rif. distinta
050000070807     c                   clear                   fnorf000
050100070807     c                   eval      orfpoe = ormpoe
050200070807     c                   eval      orfnsr = ormnsr
050300070807     c                   eval      orfnor = ormnor
050400070807     c                   eval      orfnrv = ormnrv
050500070807     c                   eval      orfpog = ormpor
050600070807     c                   exsr      sr_dataora
050700070807     c                   eval      orfdae = wdata
050800070807     c                   eval      orfore = wora
050900070807     c                   eval      orffar = 200
051000070807     c                   eval      orfpue = knmus
051100070807     c                   write     fnorf000
051200070807      * scrive fase 100 senza rif. distinta ma con nuova filiale ritiro
051300070807     c                   clear                   fnorf000
051400070807     c                   eval      orfpoe = ormpoe
051500070807     c                   eval      orfnsr = ormnsr
051600070807     c                   eval      orfnor = ormnor
051700070807     c                   eval      orfnrv = ormnrv
051800070807     c                   eval      orfpog = i99lna
051900070807     c                   eval      orfdae = wdata
052000070807     c                   eval      orfore = wora
052100070807     c                   add       1             orfore
052200070807     c                   eval      orffar = 100
052300070807     c                   eval      orfpue = knmus
052400070807     c                   write     fnorf000
052500070807      * aggiorna filiale ritiro su FNORG e toglie rif. distinta e fase prec.
052600080313      * devo anche pulire il giro
052700070807      * non mi interessa se siamo in simulazione o no
052800070807     c     korm          chain     fnorg01l
052900070807     c                   if        %found(fnorg01l)
053000070807     c                   eval      orgpor = i99lna
053100080313     c                   clear                   orgtgi
053200080313     c                   clear                   orgpocgi
053300080313     c                   clear                   orgcgi
053400070807     c                   clear                   orgpdc
053500070807     c                   clear                   orgfgs
053600070807     c                   clear                   orgndc
053700070807     c                   clear                   orgddc
053800070807     c                   clear                   orgnftl
053900070904     c                   clear                   orgslo
054000071001     c                   clear                   orgdtvdis
054100071001     c                   clear                   orghvdis
054200080313     c                   eval      orgdtvtel = orfdae
054300080313     c                   eval      orghvtel = orfore
054400070807     c                   clear                   orgfao
054500070807     c                   clear                   orgdfo
054600070807     c                   clear                   orgofo
054700070807     c                   update    fnorg000
054800070807     c                   endif
054900070807      * aggiorna testata ORM senza rif. distinta e con nuova fil. ritiro
055000070807      * toglie flag di orm stampato
055100070807     c                   eval      ormpor = i99lna
055200070807     c                   eval      ormfao = orffar
055300070807     c                   eval      ormdfo = orfdae
055400070807     c                   eval      ormofo = orfore
055500070807     c                   clear                   ormnpg
055600070807     c                   clear                   ormndc
055700070807     c                   clear                   ormddc
055800070807     c                   clear                   ormdst
055900070807     c                   exsr      sr_fase
056000070807     c                   eval      ormeti = d§farass
056100070807     c                   eval      ormdtt = wdata
056200070807     c                   update    fnorm000
056300071001      * gestisco il commit
056400071001     c                   if        i99cmt = 'S'
056500071001     c                   commit
056600071001     c                   endif
056700071206      * se filiale abilitata allo scarico PDA richiamo aggiorno
056800071206      * e già partita con GEO ORM
056900080221     c                   if        §ogpdaorm <> *blanks
057000080221     c                             and test_simula = 'N'
057100071206     c                   exsr      sr_aggpdo
057200071206     c                   endif
057300070807
057400070807      * C H I U D E
057500070807     c                   when      i99fao = 999
057600071001     c     korm          chain     fnorm01l
057700070807      * scrivo fase 999 con rif. distinta
057800070807     c                   clear                   fnorf000
057900070807     c                   eval      orfpoe = ormpoe
058000070807     c                   eval      orfnsr = ormnsr
058100070807     c                   eval      orfnor = ormnor
058200070807     c                   eval      orfnrv = ormnrv
058300070807     c                   eval      orfpog = ormpor
058400070807     c                   exsr      sr_dataora
058500070807     c                   eval      orfdae = wdata
058600070807     c                   eval      orfore = wora
058700070904     c                   eval      orffar = i99fao
058800070807     c                   eval      orfcar = i99car
058900070807     c                   eval      orfpue = knmus
059000071001     c                   eval      orffgs = i99fgs
059100071002     c                   eval      orfndc = savndc
059200071002     c                   eval      orfddc = savddc
059300070807     c                   write     fnorf000
059400070807      * non faccio niente su FNORG
059500070807      * aggiorna testata ORM con rif. distinta
059600070807     c                   eval      ormfao = orffar
059700070807     c                   eval      ormdfo = orfdae
059800070807     c                   eval      ormofo = orfore
059900071002     c                   eval      ormnpg = savnpg
060000071002     c                   eval      ormndc = savndc
060100071002     c                   eval      ormddc = savddc
060200070807     c                   exsr      sr_fase
060300070807     c                   eval      ormeti = d§farass
060400070807     c                   eval      ormdtt = wdata
060500070807     c                   update    fnorm000
060600071001      * gestisco il commit
060700071001     c                   if        i99cmt = 'S'
060800071001     c                   commit
060900071001     c                   endif
061000071206      * se filiale abilitata allo scarico PDA richiamo aggiorno
061100080221      * e già partita con GEO ORM
061200080221     c                   if        §ogpdaorm <> *blanks
061300080221     c                             and test_simula = 'N'
061400071206     c                   exsr      sr_aggpdo
061500071206     c                   endif
061600070807
061700070807      * A S S E G N A
061800070807     c                   when      i99fao = 400
061900071001     c                   clear                   dstpdr
062000071001     c     kfidst        chain     fidst01l
062100070807      * aggiorna FNORG
062200070807     c     korm          chain     fnorg01l
062300070807     c                   if        %found(fnorg01l)
062400071001     c                   eval      orgfgs = i99fgs
062500070807     c                   eval      orgpdc = dstpdr
062600071002     c                   eval      orgndc = savndc
062700071002     c                   eval      orgddc = savddc
062800070807     c                   clear                   orgnftl
062900070807     c                   clear                   orgslo
063000070807     c                   exsr      sr_dataora
063100070807     c                   eval      orgdtvdis = wdata
063200070807     c                   eval      orghvdis = wora
063300070807     c                   update    fnorg000
063400070807     c                   endif
063500071023      * aggiorna ORM
063600080206      * se sono in simulazione subito
063700080206     c                   if        test_simula = 'S'
063800080206     c     korm          chain     fnorm01l
063900080206      * scrivo fase 400 con rif. distinta
064000080206     c                   clear                   fnorf000
064100080206     c                   eval      orfpoe = ormpoe
064200080206     c                   eval      orfnsr = ormnsr
064300080206     c                   eval      orfnor = ormnor
064400080206     c                   eval      orfnrv = ormnrv
064500080206     c                   eval      orfpog = ormpor
064600080206     c                   exsr      sr_dataora
064700080206     c                   eval      orfdae = wdata
064800080206     c                   eval      orfore = wora
064900080206     c                   eval      orffar = i99fao
065000080206     c                   eval      orfpue = knmus
065100080206     c                   eval      orffgs = i99fgs
065200080206     c                   eval      orfndc = savndc
065300080206     c                   eval      orfddc = savddc
065400080206     c                   write     fnorf000
065500080206      * aggiorna testata ORM con rif. distinta
065600080206     c                   eval      ormfao = orffar
065700080206     c                   eval      ormdfo = orfdae
065800080206     c                   eval      ormofo = orfore
065900080206     c                   eval      ormnpg = savnpg
066000080206     c                   eval      ormndc = savndc
066100080206     c                   eval      ormddc = savddc
066200080206     c                   exsr      sr_fase
066300080206     c                   eval      ormeti = d§farass
066400080206     c                   eval      ormdtt = wdata
066500080206     c                   update    fnorm000
066600080206      * gestisco il commit
066700080206     c                   if        i99cmt = 'S'
066800080206     c                   commit
066900080206     c                   endif
067000080206     c                   endif
067100080206      * se sono partita aggiorno dopo con fior61r
067200080206     c                   if        test_simula = 'N'
067300120123      /free
067400120123       //?Visto che può capitare di avere ORM RC ancora con n.distinta su FNORM
067500120123       //?prima di richiamare il pgm di assegnazione vado a svuotare il campo della
067600120123       //?distinta sulla testata dell'ORM
067700120123       //?prima di richiamare il FIOR61R
067800120123         chain (I99poe:I99nsr:I99nor:I99nrv) FNORM01L;
067900120123         IF  %found(FNORM01L);
068000120123           clear ORMndc;
068100120123           update fnorm000;
068200120123         ENDIF;
068300120123      /end-free
068400120123
068500080206     c                   clear                   fior61ds
068600071001     c                   eval      or61fgsi = i99fgs
068700071002     c                   eval      or61dfvi = savddc
068800070921     c                   eval      or61mtdi = 'M'
068900071002     c                   eval      or61nfvi = savndc
069000071001     c                   eval      or61rc = 'S'
069100071002     c                   eval      or61nftl = i99nftl
069200071002      * gestisco il commit
069300071002     c                   eval      or61cmti = 'N'
069400071002     c                   if        i99cmt = 'N' and i99comit = '0'
069500071002     c                   eval      or61cmti = 'S'
069600071002     c                   endif
069700071002      * gestisco l'apertura dei file sotto commit
069800071002     c                   if        i99comit = '0'
069900071002     c                   call      'FIOR61C'
070000071002     c                   parm                    kpjba
070100071002     c                   parm                    fior61ds
070200071002     c                   else
070300071002     c                   call      'FIOR61R'
070400071002     c                   parm                    kpjba
070500071002     c                   parm                    fior61ds
070600071002     c                   endif
070700071001     c                   if        i99cmt = 'S'
070800070807     c                   if        or61erro <> *blanks
070900070807      * problemi durante l'aggiornamento --> eseguire rollback  ?
071000070807     c                   rolbk
071100070807     c                   else
071200070807      * confermo le variazioni fatte
071300070807     c                   commit
071400070807     c                   endif
071500070807     c                   endif
071600080206     c                   endif
071700070807
071800070807      * D I S A S S E G N A
071900070807     c                   when      i99fao = 390
072000070807      * aggiorna FNORG
072100070807     c     korm          chain     fnorg01l
072200070807     c                   if        %found(fnorg01l)
072300070807     c                   clear                   orgfgs
072400070807     c                   clear                   orgpdc
072500070807     c                   clear                   orgndc
072600070807     c                   clear                   orgddc
072700070807     c                   clear                   orgnftl
072800070807     c                   clear                   orgslo
072900070807     c                   clear                   orgdtvdis
073000070807     c                   clear                   orghvdis
073100070807     c                   update    fnorg000
073200070807     c                   endif
073300080206      * aggiorna ORM
073400080206      * se sono in simulazione subito
073500080206     c                   if        test_simula = 'S'
073600080206     c     korm          chain     fnorm01l
073700080206      * scrivo fase 390 con rif. distinta
073800080206     c                   clear                   fnorf000
073900080206     c                   eval      orfpoe = ormpoe
074000080206     c                   eval      orfnsr = ormnsr
074100080206     c                   eval      orfnor = ormnor
074200080206     c                   eval      orfnrv = ormnrv
074300080206     c                   eval      orfpog = ormpor
074400080206     c                   exsr      sr_dataora
074500080206     c                   eval      orfdae = wdata
074600080206     c                   eval      orfore = wora
074700080206     c                   eval      orffar = i99fao
074800080206     c                   eval      orfcar = i99car
074900080206     c                   eval      orfpue = knmus
075000080206     c                   eval      orffgs = i99fgs
075100080206     c                   eval      orfndc = savndc
075200080206     c                   eval      orfddc = savddc
075300080206     c                   write     fnorf000
075400080206      * aggiorna testata ORM senza rif. distinta
075500080206     c                   eval      ormfao = orffar
075600080206     c                   eval      ormdfo = orfdae
075700080206     c                   eval      ormofo = orfore
075800080206     c                   clear                   ormnpg
075900080206     c                   clear                   ormndc
076000080206     c                   clear                   ormddc
076100080206     c                   exsr      sr_fase
076200080206     c                   eval      ormeti = d§farass
076300080206     c                   eval      ormdtt = wdata
076400080206     c                   update    fnorm000
076500080206      * gestisco il commit
076600080206     c                   if        i99cmt = 'S'
076700080206     c                   commit
076800080206     c                   endif
076900080206     c                   endif
077000080206      * se sono partita aggiorno dopo con fior61r
077100080206     c                   if        test_simula = 'N'
077200071001     c                   clear                   fior61ds
077300071023      * aggiorna ORM
077400071001     c                   eval      or61fgsi = i99fgs
077500071002     c                   eval      or61dfvi = savddc
077600070807     c                   eval      or61mtdi = 'T'
077700071002     c                   eval      or61nfvi = savndc
077800071001     c                   eval      or61rc = 'S'
077900071002     c                   eval      or61nftl = i99nftl
078000071002      * gestisco il commit
078100071002     c                   eval      or61cmti = 'N'
078200071002     c                   if        i99cmt = 'N' and i99comit = '0'
078300071002     c                   eval      or61cmti = 'S'
078400071002     c                   endif
078500071002      * gestisco l'apertura dei file sotto commit
078600071002     c                   if        i99comit = '0'
078700071002     c                   call      'FIOR61C'
078800071002     c                   parm                    kpjba
078900071002     c                   parm                    fior61ds
079000071002     c                   else
079100071002     c                   call      'FIOR61R'
079200071002     c                   parm                    kpjba
079300071002     c                   parm                    fior61ds
079400071002     c                   endif
079500071001     c                   if        i99cmt = 'S'
079600070807     c                   if        or61erro <> *blanks
079700070807      * problemi durante l'aggiornamento --> eseguire rollback  ?
079800070807     c                   rolbk
079900070807     c                   else
080000070807      * confermo le variazioni fatte
080100070807     c                   commit
080200070807     c                   endif
080300070807     c                   endif
080400080206     c                   endif
080500070807
080600070807     c                   endsl
080700070807
080800070807     c                   endsr
080900070807
081000070807      *------------------------------------------------------------------------*
081100070807      *   Imposto data e ora del momento
081200070807      *------------------------------------------------------------------------*
081300070807     c     sr_dataora    begsr
081400070807
081500070807     c                   time                    w0140
081600070807      * ora
081700070807     c                   movel     w0140         wora
081800070807      * data
081900070807     c                   move      w0140         wdata
082000070807     c     *eur          move      wdata         dataeur
082100070807     c                   move      dataeur       dataiso
082200070807     c                   move      dataiso       wdata
082300070807
082400070807     c                   endsr
082500070307
082600070307      *-----------------------------------------------------*
082700070307      * sr_aggpdo - aggiorno i dati su FIPDO se necessario  *
082800070307      *-----------------------------------------------------*
082900070307     c     sr_aggpdo     begsr
083000070307
083100070307     c                   clear                   fior56ds
083200080206      * se fase 999 richiamo con 'A'
083300071206      * il togli e metti dalla distinta richiama il FIOR61R e ci pensa lui a richiamare
083400071206      * il fior56r per il PDA
083500071206     c                   if        i99fao = 999
083600070307     c                   eval      or56tla = 'A'
083700071206     c                   endif
083800080313      * se richiamato con apertura dei file sotto comit e il comit non lo fa questo pgm
083900080313      * richiamo Fior56 con apertura fnorg con comit
084000080313     c                   if        i99comit = '1'
084100080313     c                   eval      or56comit = '1'
084200080313     c                   endif
084300071001     c                   eval      or56fgs = savfgs
084400071001     c                   eval      or56ddc = savddc
084500070307     c                   eval      or56poe = ormpoe
084600070307     c                   eval      or56nsr = ormnsr
084700070307     c                   eval      or56nor = ormnor
084800070307     c                   eval      or56nrv = ormnrv
084900071206     c                   eval      or56ndcd = savndc
085000071206     c                   eval      or56ndca = savndc
085100070307     c                   call      'FIOR56R'
085200070307     c                   parm                    kpjbu
085300070307     c                   parm                    fior56ds
085400070307      * il pgm torna degli errori ma non ci faccio niente
085500070307
085600070307     c                   endsr
085700070716
085800070716      *--------------------------------------------------------------------------------------------*
085900070716      *  Tabella Fase
086000070716      *--------------------------------------------------------------------------------------------*
086100070716     c     sr_fase       begsr
086200070716
086300070716     c                   clear                   tibs02ds
086400070716     c                   clear                   dfar
086500070716     c                   eval      t02mod = 'C'
086600070716     c                   eval      t02sif = knsif
086700070716     c                   eval      t02cod = 'FAR'
086800070716     c                   movel(p)  ormfao        t02ke1
086900070716     c                   call      'TIBS02R'
087000070716     c                   parm                    kpjba
087100070716     c                   parm                    tibs02ds
087200070716     c                   eval      dfar = t02uni
087300070716
087400070716     c                   endsr
087500060515
087600940322     C*-----------------------------------------------------*
087700941107     C*    Operazioni iniziali
087800941107     C*-----------------------------------------------------*
087900940322     C     *INZSR        BEGSR
088000070807
088100030826     C     Kar5          KLIST
088200030826     C                   KFLD                    i99aas
088300030826     C                   KFLD                    i99LNP
088400030826     C                   KFLD                    i99NRS
088500030826     C                   KFLD                    i99NSP
088600030826     C                   KFLD                    ar5trd
088700030826     c                   eval      ar5trd = 'RCC'
088800070807
088900030826     C     Korm          KLIST
089000030826     C                   KFLD                    i99poe
089100030826     C                   KFLD                    i99nsr
089200030826     C                   KFLD                    i99nor
089300030826     C                   KFLD                    i99nrv
089400070307
089500031006     C     Korf          KLIST
089600070307     C                   KFLD                    ormpoe
089700070307     C                   KFLD                    ormnsr
089800070307     C                   KFLD                    ormnor
089900070307     C                   KFLD                    ormnrv
090000031006     C                   KFLD                    ormdfo
090100031006     C                   KFLD                    ormofo
090200070307     C                   KFLD                    ormfao
090300070307
090400060214     C     Kar4          KLIST
090500060214     C                   KFLD                    ar4trc
090600060214     C                   KFLD                    ar4n14
090700060214     c                   eval      ar4trc = 'M'
090800071001
090900071001     c     kfidst        klist
091000071001     c                   kfld                    savnpg
091100071001     c                   kfld                    savndc
091200071001     c                   kfld                    savfgs
091300071001
091400940322     C                   ENDSR
