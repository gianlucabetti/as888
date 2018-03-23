000100041130     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200041130
000300041130     fFnorm99L  iF   e           K disk
000400041130     fTitas32c  if   e           k disk
000500041130     fFNBLP33L  if   e           k disk
000600041130     FFNACR01L  IF   E           K DISK
000700041201     fFnorf01l  if   e           k disk
000800041130     ffnblp00f  o    e             disk    RENAME(FNBLP000:FNBLP0)
000900041130     ffnbl400f  o    e             disk
001000041130     ffnarb00f  o    e             disk
001100041130     ffnar400f  o    e             disk
001200041130     ftitas00f  o    e             disk    rename(titas000:titas0)
001300041130     ftitaa00f  o    e             disk
001400041130     ftita400f  o    e             disk
001500041130     fQsysprt   o    f  132        printer oflind(*InOf)
001600041130
001700041130     d conta           s              7  0 inz
001800041130     d contasc         s              7  0 inz
001900041201     d conta999        s              7  0 inz
002000041130     d contaagg        s              7  0 inz
002100041201     d nobolla         s              1    inz(*Off)
002200041130     d wcreaft         s              1    inz(*Off)
002300041130
002400041130     d                 ds
002500041130     d  ormcor1                1      3  0
002600041130     d  ormcor2                4      7  0
002700041130     d  ormcor3                8     10  0
002800041130     d  OrmCor                 1     10  0
002900041130     d                 ds
003000041130     d  filler                 1      1  0
003100041130     d  OrmPoe                 2      4  0
003200041130     d  OrmNsr                 5      6  0
003300041130     d  OrmNor                 7     13  0
003400041130     d  OrmNrv                14     15  0
003500041130     d  KTASRMN                1     15  0
003600041130     d                 ds
003700041130     d  anno_oggi              1      4  0
003800041130     d  mgs_oggi               5      8  0
003900041201     d  data_oggi              1      8  0
004000041130
004100041130
004200041130     d dorm01        e ds
004300041130     d Kpjba         e ds
004400041130     d Trul34ds      e ds
004500041130     d Fnlv55ds      e ds
004600041130     d dsbl4a        e ds
004700041130
004800041130      * dalla data - alla data
004900041130     c     pdatada       setll     fnorm000
005000041130     c                   read      fnorm000
005100041130    1C                   DOW       NOT %EOF
005200041130if  2c                   If        Ormdao > pdataal
005300041130     c                   leave
005400041130    2c                   ENDIF
005500041130
005600041130     c                   Eval      dorm01 = OrmFlo
005700041130
005800041130      * Orm Commissionato
005900041130if  3c                   If        §OrCom = 'S' and OrmCor > *Zeros and
006000041130     c                             ormcor2 <> 8888 and ormcor2 <> 9999 and
006100041130     c                             OrmTor <> 'P'
006200041130     c                             and ormfao > 50  and   ormnsr = 00
006300041201
006400041201     c                   eval      nobolla = *Off
006500041201      * per prima cosa controllo se l'orm è stato chiuso prima di confermarlo
006600041201    4c                   If        ormfao = 999
006700041201      * cerco se la fase precedente è una 50
006800041201     c     korf          Setll     fnorf01l
006900041201     C     korf1         readpe    fnorf01l
007000041201    5C                   if        orffar = 50
007100041201     c                   add       1             conta999
007200041201     c                   Except    riga999
007300041201     c                   eval      nobolla = *On
007400041201    5c                   endif
007500041201    4c                   endif
007600041201
007700041201    4c                   if        nobolla = *Off
007800041130      * cerco se esiste già in TITAS
007900041130     c                   Eval      wcreaft = *Off
008000041130     c                   ExSr      Titas
008100041130      *       se NON ESISTE CREO BOLLE
008200041130     c                   If        wcreaft = *On
008300041130     c                   ExSr      Sr_BollaFt
008400041130     c                   Else
008500041130     c                   add       1             contasc
008600041130     c                   except    scarto
008700041130     c                   EndIf
008800041201    4c                   endif
008900041130      *
009000041130e   3c                   EndIf
009100041130      *
009200041130     c                   read      fnorm000
009300041130e   1c                   EndDo
009400041130
009500041130     c                   eval      *inlr = *on
009600041130     clr                 except    totale
009700041130      *-------------------------------------------------------------------------------*
009800041130      * Controllo se c'è già la bolla su TITAS/FNBLP
009900041130      *-------------------------------------------------------------------------------*
010000041130     c     Titas         BegSr
010100041130
010200041130     c     kTasRmn       Setll     Titas32c
010300041130     c                   If        Not %equal(Titas32c)
010400041130     c                   Eval      wcreaft = *On
010500041130     c                   GOTO      DOVEVOLETE
010600041130     c                   EndIf
010700041130      *
010800041130     c                   Eval      wcreaft = *On
010900041130do  1c                   Do        *Hival
011000041130     c     kTasRmn       Reade     Titas32c
011100041130     c                   If        %Eof(Titas32c)
011200041130     c                   Leave
011300041130     c                   EndIf
011400041130      * controllo che la bolla non sia del 2004 e che non sia una 'FT'
011500041130     c                   If        TasAas = 2004 and TasCbo = 'FT'
011600041130     c                   Eval      wcreaft = *Off
011700041130     c                   Leave
011800041130     c                   EndIf
011900041130e   1c                   EndDo
012000041130      *
012100041130     C     DOVEVOLETE    TAG
012200041130     c                   If        wcreaft = *ON
012300041130
012400041130     c     kTasRmn       Setll     FNBLP33L
012500041130     c                   If        Not %equal(FNBLP33L)
012600041130     c                   Eval      wcreaft = *On
012700041130     c                   Leavesr
012800041130     c                   EndIf
012900041130      *
013000041130     c                   Eval      wcreaft = *On
013100041130do  1c                   Do        *Hival
013200041130     c     kTasRmn       Reade     FNBLP33L
013300041130     c                   If        %Eof(FNBLP33L)
013400041130     c                   Leave
013500041130     c                   EndIf
013600041130      * controllo che la bolla non sia del 2004 e che non sia una 'FT'
013700041130     c                   If        BLPAas = 2004 and BLPCbo = 'FT'
013800041130     c                   Eval      wcreaft = *Off
013900041130     c                   Leave
014000041130     c                   EndIf
014100041130e   1c                   EndDo
014200041130     c                   EndIf
014300041130
014400041130     c                   EndSr
014500041130
014600041130      *-------------------------------------------------------------------------------*
014700041130      * Genero la bolla FT
014800041130      *-------------------------------------------------------------------------------*
014900041130     c     Sr_BollaFt    BegSr
015000041130
015100041130     c                   add       1             conta
015200041130     c                   except    riga
015300041130
015400041130if  1c                   If        pstampa <> 'S'
015500041130
015600041130      * Numero spedizione (da Flnuf cod. 3)
015700041130     c                   Clear                   Trul34ds
015800041130     c                   Eval      i34Aas = 2004
015900041130     c                   Eval      i34Lnp = ORMPOR
016000041130     c                   Call      'TRUL34R'
016100041130     c                   Parm                    kpjba
016200041130     c                   Parm                    Trul34ds
016300041130
016400041130     c                   If        o34err = *Zeros
016500041130      *
016600041130     c                   clear                   wccm
016700041130     c                   clear                   wlna
016800041130     c                   clear                   wncl
016900041130     c                   clear                   wpkb
017000041130     c                   clear                   wtfp
017100041130     c                   clear                   wtfa
017200041130      *
017300041130      * Cod.Cliente mittente
017400041130     C     ormcor        chain     fnacr01l
017500041130      * Ksc dell'ordinante da Fnacr
017600041130     c                   If        AcrKsc <> *Zeros  and
017700041130     c                             %FOUND(FNACR01L)
017800041130     c                   Eval      WCcm = Acrksc
017900041130     c                   Else
018000041130      * Primi 7 byte dell'ordinante
018100041130     c                   movel     ORMcor        wccm              7 0
018200041130     c                   EndIf
018300041130      *
018400041130     c                   movel     wccm          wlna              3 0
018500041130      *
018600041130     c                   IF        ORMncl > 0
018700041130     c                   z-add     ormncl        wncl              5 0
018800041130     c                   else
018900041130     c                   z-add     1             wncl
019000041130     c                   EndIf
019100041130      * peso = peso ORM - se a 0 ricavo dal volume o dai bancali dell'ORM
019200041130     c                   SELECT
019300041130     c                   WHEN      ORMpkg <> 0
019400041130     c                   eval      Wpkb = ORMpkg
019500041130     c                   WHEN      ORMvlm <> 0
019600041130     c     ORMvlm        mult      200           Wpkb              7 1
019700041130     c                   WHEN      ORMbnc <> 0
019800041130     c     ORMbnc        mult      200           wpkb
019900041130     c                   endsl
020000041130      * cerco il terminal di partenza
020100041130     C                   CLEAR                   fnlv55ds
020200041130     C                   MOVEL     'P'           D55TPT
020300041130     C                   MOVEL     ormpor        D55LIN
020400041130     C                   moveL     data_oggi     D55DRF
020500041130     C                   CALL      'FNLV55R'
020600041130     C                   PARM                    fnlv55ds
020700041130     C     D55ERR        IFEQ      ' '
020800041130     C                   MOVEL     D55TFP        wtfp              3 0
020900041130     C                   ENDIF
021000041130      * cerco il terminal di arrivo
021100041130     C                   CLEAR                   fnlv55ds
021200041130     C                   MOVEL     'A'           D55TPT
021300041130     C                   MOVEL     ormpor        D55LNP
021400041130     C                   MOVEL     wlna          D55LIN
021500041130     C                   moveL     data_oggi     D55DRF
021600041130     C                   CALL      'FNLV55R'
021700041130     C                   PARM                    fnlv55ds
021800041130     C     D55ERR        IFEQ      ' '
021900041130     C                   MOVEL     D55TFa        wtfa              3 0
022000041130     C                   ENDIF
022100041130      *
022200041130     c                   add       1             contaagg
022300041130     c                   ExSr      wblp
022400041130     c                   ExSr      warb
022500041130     c                   ExSr      wtas
022600041130      *
022700041130     c                   ELSE
022800041130     c                   except    rigaE
022900041130     c                   EndIf
023000041130
023100041130e   1c                   endif
023200041130
023300041130     c                   EndSr
023400041130
023500041130      *-------------------------------------------------------------------------------*
023600041130      * scrivo blp - bl4 - ar6
023700041130      *-------------------------------------------------------------------------------*
023800041130     c     wblp          BegSr
023900041130      *
024000041130     c                   clear                   FNBLP0
024100041130     C                   eval      blpflb = ormpor
024200041130     C                   eval      blpdim = data_oggi
024300041130     C                   eval      blpaas = anno_oggi
024400041130     C                   eval      blplnp = ormpor
024500041130     C                   eval      blpnsp = o34nsp
024600041130     C                   eval      blpmgs = mgs_oggi
024700041130     C                   eval      blpcbo = 'FT'
024800041130     C                   eval      blpsop = '+'
024900041130     C                   eval      blpfst = 'Y'
025000041130     C                   eval      blpfdn = 'S'
025100041130     C                   eval      blplna = wlna
025200041130     C                   eval      blpznc = *zeros
025300041130     C                   eval      blpksc = (wlna *10000) + 9999
025400041130     C                   eval      blprsm = ormrsr
025500041130     C                   eval      blpinm = orminr
025600041130     C                   eval      blpcam = ormcar
025700041130     C                   eval      blplom = ormlor
025800041130     C                   eval      blpprm = ormprr
025900041130     C                   eval      blpfap = 'C'
026000041130     C                   eval      blpnzm = ormnar
026100041130     C                   eval      blprsd = ormrso
026200041130     C                   eval      blpind = ormino
026300041130     C                   eval      blpcad = ormcao
026400041130     C                   eval      blplod = ormloo
026500041130     C                   eval      blpprd = ormpro
026600041130     C                   eval      blpfin = 'C'
026700041130     C                   eval      blpnzd = ormnao
026800041130     C                   eval      blpcts = '40'
026900041130     C                   eval      blptsp = 'C'
027000041130     C                   eval      blpnas = 'VARI.E'
027100041130     C                   eval      blpncl = Wncl
027200041130     C                   eval      blppkb = wpkb
027300041130     C                   eval      blppkf = Wpkb
027400041130     C                   eval      blpfvb = 'P'
027500041130     C                   eval      blpvlb = 0,001
027600041130     C                   eval      blpfvf = 'P'
027700041130     C                   eval      blpvlf = 0,001
027800041130     C                   eval      blprmn = Ktasrmn
027900041130     C                   eval      blprma = ORMrfa
028000041130     C                   eval      blpctm = '4F'
028100041130     C                   eval      blpdrt = data_oggi
028200041130     C                   eval      blpfpp = 'M'
028300041130     C                   eval      blppdr = (ORMpor *10000) + 999
028400041130     C                   eval      blpdbr = data_oggi
028500041130     C                   eval      blpnfv = 99998
028600041130     C                   eval      blpdce = data_oggi
028700041130     C                   eval      blpFLE = wtfp
028800041130     C                   eval      blptfp = wtfp
028900041130     C                   eval      blpfea = wtfa
029000041130     C                   eval      blptfa = wtfa
029100041130     C                   eval      blpFT1 = 'T'
029200041130     C                   eval      blpDT1 = DATA_OGGI
029300041130     C                   eval      blpFT2 = 'T'
029400041130     C                   eval      blpDT2 = DATA_OGGI
029500041130     C                   eval      blpFT3 = 'S'
029600041130     C                   eval      blpDT3 = DATA_OGGI
029700041130     c                   write     fnblp0
029800041130      *
029900041130     c                   clear                   FNBl4000
030000041130     c                   clear                   dsbl4a
030100041130     C                   eval      bl4aas = anno_oggi
030200041130     C                   eval      bl4lnp = ormpor
030300041130     C                   eval      bl4nsp = o34nsp
030400041130     C                   eval      BL4TRC = 'A'
030500041130     C                   eval      §B4AKS = wccm
030600041130     c                   eval      bl4not = dsbl4a
030700041130     C                   eval      BL4FTR = 'T'
030800041130     C                   eval      bl4DTR = DATA_OGGI
030900041130     c                   write     fnbl4000
031000041130      *
031100041130     c                   EndSr
031200041130
031300041130      *-------------------------------------------------------------------------------*
031400041130      * scrivo arb - ar4
031500041130      *-------------------------------------------------------------------------------*
031600041130     c     warb          BegSr
031700041130
031800041130     c                   clear                   fnarb000
031900041130     C                   eval      arbaas = anno_oggi
032000041130     C                   eval      arblnp = ormpor
032100041130     C                   eval      arbnsp = o34nsp
032200041130     C                   eval      arbmgs = mgs_oggi
032300041130     C                   eval      arbcbo = 'FT'
032400041130     C                   eval      arbfdn = 'S'
032500041130     C                   eval      arblna = wlna
032600041130     C                   eval      arbznc = *zeros
032700041130     C                   eval      arbksc = (wlna *10000) + 9999
032800041130     C                   eval      arbrsm = ormrsr
032900041130     C                   eval      arbinm = orminr
033000041130     C                   eval      arbcam = ormcar
033100041130     C                   eval      arblom = ormlor
033200041130     C                   eval      arbprm = ormprr
033300041130     C                   eval      arbfap = 'C'
033400041130     C                   eval      arbnzm = ormnar
033500041130     C                   eval      arbrsd = ormrso
033600041130     C                   eval      arbind = ormino
033700041130     C                   eval      arbcad = ormcao
033800041130     C                   eval      arblod = ormloo
033900041130     C                   eval      arbprd = ormpro
034000041130     C                   eval      arbfin = 'C'
034100041130     C                   eval      arbnzd = ormnao
034200041130     C                   eval      arbcts = '40'
034300041130     C                   eval      arbtsp = 'C'
034400041130     C                   eval      arbnas = 'VARI.E'
034500041130     C                   eval      arbncl = Wncl
034600041130     C                   eval      arbpkb = wpkb
034700041130     C                   eval      arbpkf = Wpkb
034800041130     C                   eval      arbfvb = 'P'
034900041130     C                   eval      arbvlb = 0,001
035000041130     C                   eval      arbfvf = 'P'
035100041130     C                   eval      arbvlf = 0,001
035200041130     C                   eval      arbrmn = Ktasrmn
035300041130     C                   eval      arbrma = ORMrfa
035400041130     C                   eval      arbdbr = data_oggi
035500041130     C                   eval      arbnfv = 99999
035600041130     C                   eval      arbdce = data_oggi
035700041130     C                   eval      arbAMA = 3
035800041130     C                   eval      arbACC = 'S'
035900041130     C                   eval      arbFLE = wtfp
036000041130     C                   eval      arbtfp = wtfp
036100041130     C                   eval      arbfea = wtfa
036200041130     C                   eval      arbtfa = wtfa
036300041130     c                   write     fnarb000
036400041130      *
036500041130     c                   clear                   fnar4000
036600041130     c                   clear                   dsbl4a
036700041130     C                   eval      ar4aas = anno_oggi
036800041130     C                   eval      ar4lnp = ormpor
036900041130     C                   eval      ar4nsp = o34nsp
037000041130     C                   eval      ar4TRC = 'A'
037100041130     C                   eval      §B4ABM = 'Y'
037200041130     C                   eval      §B4aso = '+'
037300041130     C                   eval      §B4AKS = wccm
037400041130     c                   eval      ar4not = dsbl4a
037500041130     C                   eval      ar4FTR = 'R'
037600041130     C                   eval      ar4DTR = DATA_OGGI
037700041130     c                   write     fnar4000
037800041130
037900041130     c                   EndSr
038000041130
038100041130      *-------------------------------------------------------------------------------*
038200041130      * scrivo tas - taa
038300041130      *-------------------------------------------------------------------------------*
038400041130     c     wtas          BegSr
038500041130
038600041130     c                   clear                   titas0
038700041130     C                   eval      TASaas = anno_oggi
038800041130     C                   eval      TASlnp = ormpor
038900041130     C                   eval      TASnsp = o34nsp
039000041130     C                   eval      TASmgs = mgs_oggi
039100041130     C                   eval      TASTBL = 'A3'
039200041130     C                   eval      TAScbo = 'FT'
039300041130     C                   eval      TASksc = (wlna *10000) + 9999
039400041130     C                   eval      TASlna = wlna
039500041130     C                   eval      TASncl = Wncl
039600041130     C                   eval      TASpkb = wpkb
039700041130     C                   eval      TASFPB = 'R'
039800041130     C                   eval      TASpkf = Wpkb
039900041130     C                   eval      TASFPF = 'R'
040000041130     C                   eval      TASfvb = 'P'
040100041130     C                   eval      TASvlb = 0,001
040200041130     C                   eval      TASfvf = 'P'
040300041130     C                   eval      TASvlf = 0,001
040400041130     C                   eval      TAStsp = 'C'
040500041130     C                   eval      TASrsd = ormrso
040600041130     C                   eval      TASind = ormino
040700041130     C                   eval      TAScad = ormcao
040800041130     C                   eval      TASlod = ormloo
040900041130     C                   eval      TASprd = ormpro
041000041130     C                   eval      TASnzd = ormnao
041100041130     C                   eval      TAScts = '40'
041200041130     C                   eval      TASfin = 'C'
041300041130     C                   eval      TASccm = (ORMPOR *10000) + 8888
041400041130     C                   eval      TASrmn = Ktasrmn
041500041130     C                   eval      TASmct = '40'
041600041130     C                   eval      TASfap = 'C'
041700041130     C                   eval      TASnas = 'VARI.E'
041800041130     C                   eval      TASflb = ORMPOR
041900041130     C                   eval      TAStfa = wtfa
042000041130     C                   eval      TAStfp = wtfp
042100041130     C                   eval      TASdim = data_oggi
042200041130     C                   eval      TASsop = '+'
042300041130     C                   eval      TASpdr = (ORMpor *10000) + 999
042400041130     C                   eval      TASdrt = data_oggi
042500041130     C                   eval      TASfpp = 'M'
042600041130     C                   eval      TASctm = '4F'
042700041130     C                   eval      TASdbr = data_oggi
042800041130     C                   eval      TASfdn = 'S'
042900041130     C                   eval      TASll1 = 'Y'
043000041130     C                   eval      TASflo = 'CC '
043100041130     C                   eval      TAScam = ormcar
043200041130     C                   eval      TASscl = 'B'
043300041130     c                   write     titas0
043400041130      *
043500041130     c                   clear                   titaa000
043600041130     C                   eval      TAAaas = anno_oggi
043700041130     C                   eval      TAAlnp = ormpor
043800041130     C                   eval      TAAnsp = o34nsp
043900041130     C                   eval      TAATRC = 'M'
044000041130     C                   eval      TAArsc = ormrsr
044100041130     C                   eval      TAAind = orminr
044200041130     C                   eval      TAAcap = ormcar
044300041130     C                   eval      TAAloc = ormlor
044400041130     C                   eval      TAAprv = ormprr
044500041130     C                   eval      TAAnaz = ormnar
044600041130     c                   write     titaa000
044700041130      *
044800041130     c                   if        ormrfa <> *Blanks
044900041130     c                   clear                   tita4000
045000041130     C                   eval      TA4aas = anno_oggi
045100041130     C                   eval      TA4lnp = ormpor
045200041130     C                   eval      TA4nsp = o34nsp
045300041130     C                   eval      TA4TRC = 'A'
045400041130     C                   eval      TA4not = ormrfa
045500041130     c                   write     tita4000
045600041130     c                   endif
045700041130
045800041130     c                   EndSr
045900041130
046000041130      *-------------------------------------------------------------------------------*
046100041130      * ROUTINE INIZIALE
046200041130      *-------------------------------------------------------------------------------*
046300041130     c     *Inzsr        BegSr
046400041130
046500041130     c     *Entry        Plist
046600041130     c                   Parm                    xdatada           8
046700041130     c                   Parm                    xdataal           8
046800041130     c                   Parm                    xstampa           1
046900041201     c                   Parm                    xdataft           8
047000041130     c                   Parm                    SI               10
047100041130
047200041130     c                   movel     xdatada       pdatada           8 0
047300041130     c                   movel     xdataal       pdataal           8 0
047400041130     c                   movel     xstampa       pstampa           1
047500041201     c                   movel     xdataft       data_oggi         8 0
047600041130
047700041201     c**********         eval      data_oggi = *date
047800041130     c                   eval      KNSIF = SI
047900041130
048000041130     c     kOrf          klist
048100041130     c                   kfld                    OrmPoe
048200041130     c                   kfld                    OrmNsr
048300041130     c                   kfld                    OrmNor
048400041130     c                   kfld                    OrmNrv
048500041201     c                   kfld                    OrmDfo
048600041201     C                   kfld                    OrmOfo
048700041201     C                   kfld                    OrmFao
048800041201
048900041201     c     kOrf1         klist
049000041201     c                   kfld                    OrmPoe
049100041201     c                   kfld                    OrmNsr
049200041201     c                   kfld                    OrmNor
049300041201     c                   kfld                    OrmNrv
049400041130
049500041130     c                   EndSr
049600041130
049700041130     oQsysprt   e            riga        1
049800041130     o                       Ormpoe               3
049900041130     o                       Ormnsr           +   1
050000041130     o                       Ormnor           +   1
050100041130     o                       Ormnrv           +   1
050200041130     o                       Ormpor           +   3
050300041130     o                       Ormcor           +   3
050400041130     o                       conta            +   1
050500041130     o          e            scartO      1
050600041130     O                                           10 'SCARTO'
050700041130     o                       Ormpoe           +   3
050800041130     o                       Ormnsr           +   1
050900041130     o                       Ormnor           +   1
051000041130     o                       Ormnrv           +   1
051100041130     o                       Ormpor           +   3
051200041130     o                       Ormcor           +   3
051300041130     o                       contaSC       Z  +   1
051400041201     o          e            riga999     1
051500041201     o                       Ormpoe               3
051600041201     o                       Ormnsr           +   1
051700041201     o                       Ormnor           +   1
051800041201     o                       Ormnrv           +   1
051900041201     o                       Ormpor           +   3
052000041201     o                       Ormcor           +   3
052100041201     o                       conta999      Z  +   1
052200041201     O                                        +   2 'ORM CHIUSO'
052300041130     o          e            rigaE       1
052400041130     o                       Ormpoe               3
052500041130     o                       Ormnsr           +   1
052600041130     o                       Ormnor           +   1
052700041130     o                       Ormnrv           +   1
052800041130     o                       Ormpor           +   3
052900041130     o                       Ormcor           +   3
053000041130     o                                        +   1 'ERRORE NUMERATORE BOLLE'
053100041130     o          e            totale           03
053200041130     o                                           20 'BOLLE CREATE: '
053300041130     o                       contaagg            +3
