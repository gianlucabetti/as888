000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500031105      *       AGGIORNA TARIFFE CON ADDIZIONALE                        *
000600031105      *                                                               *
000700031105      *****************************************************************
000800031105     FWfaut04l  up   E           K DISK
000900031105
001000031105     FTNTAM04L  UF   E           K DISK    rename(tntam000:tntam04)
001100031105     FTITAD04L  UF   E           K DISK    rename(titad000:titad04)
001200031105     FTITPT01L  UF   E           K DISK    rename(titpt000:titpt01)
001300031105     FTITPD01L  UF   E           K DISK    rename(titpd000:titpd01)
001400031105     FTITGC01L  UF   E           K DISK    rename(titgc000:titgc01)
001500031105
001600031105     FTNTAM01L  UF   E           K DISK    rename(tntam000:tntam01)
001700031105     F                                     prefix(old_)
001800031105
001900031105     FTNTAM00F  O    E             DISK    prefix(new_)
002000031105     FTITAD00F  O    E             DISK    prefix(new_)
002100031105     FTITPT00F  O    E             DISK    prefix(new_)
002200031105     FTITPD00F  O    E             DISK    prefix(new_)
002300031105     FTITGC00F  O    E             DISK    prefix(new_)
002400031105     FWADDT00F  O    E             DISK
002500031105
002600031105      *------------------------------------------------------------------------*
002700031105
002800031105     D Kprg            S                   LIKE(TAMprg)
002900031105     D §ksc            S                   LIKE(TAMksc)
003000031105     D §ctr            S                   LIKE(TAMctr)
003100031105     D §prg            S                   LIKE(TAMprg)
003200051024     D §ppa            S                   LIKE(TAMppa)
003300051104     D §pag            S                   LIKE(TAMpag)
003400031105     D §dst            S                   LIKE(TAMdst)
003500041126     D******savtampag       S                   LIKE(TAMpag)
003600031105     D W0140           S             14  0
003700031105     D Wdtgio          S              8  0
003800031105     D §oggi           S              8  0
003900051109     D caladd          s             30  9
004000031105     D §flagA          S              1
004100031105     D §flagB          S              1
004200031105     D §flagC          S              1
004300031105     D §flagD          S              1
004400031105
004500031105      *   D S   I N T E R N E / E S T E R N E
004600031105
004700031105     D WLBDAT          DS                  INZ
004800031105     D  G02DAT                 1      8  0
004900031105     D  G02INV                 9     16  0
005000031105     D  G02ERR                17     17
005100031105     D  G02TGI                18     22  0
005200031105
005300031105     D                 ds
005400031105     D tamksc                  1      7  0
005500031105     D  ksc4                   1      4  0
005600031105     D  ksc33                  1      3  0
005700031105     D  ksc3                   5      7  0
005800031105
005900061128     D Dsta01        e ds
006000031105     D kpjba         e ds
006100031105
006200031105      *---------------------------------------------------------------------------------------------
006300031105
006400031105      *
006500031105      * Aggancio Tntam04l
006600031105     c     Wfaclf        setll     tntam04l
006700031105     c                   do        *hival
006800031105     c     wfaclf        reade     tntam04l
006900031105      *
007000031105      * fine file
007100031105     c                   if        %eof(tntam04l)
007200031105     c                   leave
007300031105     c                   endif
007400031105
007500031105      * Se stessa tariffa e si è verificata una delle condizioni
007600031105      * non deve elaborare più niente
007700031105     C                   if        (TAMksc = §ksc and TAMctr = §ctr
007800031105     C                             and §flagA = *on) or
007900031105     C                             (TAMksc = §ksc and TAMctr = §ctr
008000031105     C                             and §flagB = *on) or
008100031105     C                             (TAMksc = §ksc and TAMctr = §ctr
008200031105     C                             and §flagC = *on)
008300031105     C                   iter
008400031105     C                   endif
008500031105
008600031105      * Cambia tariffa
008700031105     C                   if        TAMksc <> §ksc
008800031105     C                   eval      §ksc = TAMksc
008900031105     C                   eval      §flagA = *off
009000031105     C                   eval      §flagB = *off
009100031105     C                   eval      §flagC = *off
009200031105     C                   endif
009300031105     C                   if        TAMctr <> §ctr
009400031105     C                   eval      §ctr = TAMctr
009500031105     C                   eval      §flagA = *off
009600031105     C                   eval      §flagB = *off
009700031105     C                   eval      §flagC = *off
009800031105     C                   endif
009900031105
010000061128      * valorizzo i campi dei flag operativi
010100061128     c                   movel     tamflo        dsta01
010200061128
010300071030      * Decorrenza maggiore/uguale 01/01/2008
010400071030     C                   if        TAMddt >= 20080101
010500071030     C                   eval      §flagA = (TAMddt = 20080101)
010600031105     C                   eval      §flagD = *off
010700071030     C                   if        tamddt <> 20080101 and tamdst <> 20080101
010800031105     C                   exsr      Sr_CercaDopo
010900031105     C                   if        §flagD = *on
011000031105     C                   eval      kprg = TAMprg
011100031105     C                   exsr      Sr_Aggiorna
011200031105     C                   eval      TAMprg = (TAMprg + 1)
011300031105     C                   endif
011400031105     C                   endif
011500051103      * verifico se devo integrare e o se devo aggiornare altrimenti non faccio nulla
011600051021     c                   clear                   wadflg
011700051103      * mi salvo le percentuali precedenti l'inglobamento e/o l'aumento
011800051103     c                   z-add     tamppa        wadspa
011900051103     c                   z-add     tampag        wadsag
012000051021      * se esiste % anni corrente
012100051021     c                   If        tamppa > 0
012200051021      * se esiste % anni precedente sommiamo le 2 percentuali
012300051021     c                   If        tampag > 0
012400051109     c                   eval      caladd = tampag + tamppa +
012500051109     c                                      ((tampag *tamppa)/100)
012600051109     c                   z-add(h)  caladd        tampag
012700051021     c                   else
012800051021     c                   eval      tampag = tamppa
012900051021     c                   endif
013000051021     c                   eval      tamppa = 0
013100051021     c                   eval      wadflg = 'I'
013200051021     c                   endif
013300051021      * se devo aumentare
013400051021     c                   If        wfaftb = 'A'
013500051103     c                   eval      TAMppa = wfaper
013600051021     c                   eval      wadflg = 'A'
013700051021     c                   endif
013800061207      * se ho inglobato o aumentato scrivo su WADDT e  aggiorno TNTAM anche se cambiato progressivo
013900061207     c                   If        wadflg <> ' ' or  §flagD = *on
014000031105     C                   eval      TAMduv = §oggi
014100031105     C                   eval      TAMftr = *blanks
014200041116     C                   eval      TAMdtr = §oggi
014300031105     C                   update    tntam04
014400061207      * se ho inglobato o aumentato  aggiorno TNTAM e scrivo su WADDT
014500061207
014600061207     c                   If        wadflg <> ' '
014700031105     C                   eval      §prg = TAMprg
014800051024     C                   eval      §ppa = TAMppa
014900051104     C                   eval      §pag = TAMpag
015000031105     C                   exsr      Sr_ScriviWadd
015100061207     C                   endif
015200061207
015300031105     C                   endif
015400051021     c                   endif
015500031105
015600071030      * Scadenza maggiore/uguale 01/01/2008 e decorrenza minore 01/01/2008
015700071030     C                   if        TAMdst >= 20080101 and TAMddt < 20080101
015800031105     C                   eval      §flagB = *on
015900031105     C                   eval      §dst = TAMdst
016000031105     C                   eval      kprg = TAMprg
016100051021      * verifico se devo aggiornare la tariffa  o per integrazione o per aumento altrimenti
016200051021      * non faccio nulla
016300051021     c                   clear                   wadflg
016400051103      * mi salvo le percentuali precedenti l'inglobamento e/o l'aumento
016500051103     c                   z-add     tamppa        wadspa
016600051103     c                   z-add     tampag        wadsag
016700051103      *
016800051021     c                   If        tamppa > 0 or wfaftb = 'A'
016900031105     C                   exsr      Sr_Annulla
017000031105     C                   exsr      Sr_Scrivi
017100031105     C                   eval      §prg = (kprg +1)
017200051024     C                   eval      §ppa = New_TAMppa
017300051104     C                   eval      §pag = New_TAMpag
017400031105     C                   exsr      Sr_ScriviWadd
017500071030     C                   eval      TAMdst = 20071231
017600031105     C                   eval      TAMduv = §oggi
017700031105     C                   eval      TAMftr = *blanks
017800041116     C                   eval      TAMdtr = §oggi
017900031105     C                   update    tntam04
018000031105     C                   iter
018100031105     C                   endif
018200051021     C                   endif
018300031105
018400071030      * Scadenza minore/uguale 31/12/2007
018500071030     C                   if        TAMdst <= 20071231
018600031105     C                   eval      §flagC = *on
018700051021      * verifico se devo aggiornare la tariffa  o per integrazione o per aumento altrimenti
018800051021      * non faccio nulla
018900051021     c                   clear                   wadflg
019000051103      * mi salvo le percentuali precedenti l'inglobamento e/o l'aumento
019100051103     c                   z-add     tamppa        wadspa
019200051103     c                   z-add     tampag        wadsag
019300051103      *
019400051021     c                   If        tamppa > 0 or wfaftb = 'A'
019500071030     C                   eval      §dst = 20081231
019600031105     C                   eval      kprg = TAMprg
019700031105     C                   exsr      Sr_Annulla
019800031105     C                   exsr      Sr_Scrivi
019900031105     C                   eval      §prg = (kprg +1)
020000051024     C                   eval      §ppa = New_TAMppa
020100051104     C                   eval      §pag = New_TAMpag
020200031105     C                   exsr      Sr_ScriviWadd
020300031105     C                   endif
020400051021     C                   endif
020500051021
020600031105     C                   enddo
020700051021
020800051021      * aggiorno il flag di aelaborazione
020900051021     c                   eval      wfaela = 'S'
021000051021     c                   update    wfaut000
021100031105
021200031105      *****************************************************************
021300031105      * CERCA LA TARIFFA SUCESSIVA E CONTROLLA SE A CAVALLO D'ANNO
021400031105      *****************************************************************
021500031105     C     Sr_CercaDopo  BEGSR
021600031105
021700031105     C                   eval      kprg = (TAMprg - 1)
021800031105     C     ktam          chain(n)  tntam01l
021900031105     C                   if        %found(tntam01l)
022000071030     C                             and old_TAMdst >= 20080101
022100071030     C                             and old_TAMddt < 20080101
022200051021      * verifico se devo integrare le % di addizionale o se devo aumentare altrimenti lascio
022300051021      * tutto così com'è
022400051021     c                   If        old_tamppa > 0 or  wfaftb = 'A'
022500031105     C                   eval      §flagD = *on
022600051021     c                   endif
022700051021
022800031105     C                   endif
022900031105
023000031105     C                   endsr
023100031105      *****************************************************************
023200031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
023300031105      *****************************************************************
023400031105     C     Sr_Aggiorna   BEGSR
023500031105
023600031105     C     ktam          setll     titad04l
023700031105     C                   do        *hival
023800031105     C     ktam          reade     titad04l
023900031105     C                   if        %eof(titad04l)
024000031105     C                   leave
024100031105     C                   endif
024200031105     C                   if        TADatb <> *blanks
024300031105     C                   iter
024400031105     C                   endif
024500031105     C                   eval      TADprg = (TADprg + 1)
024600031105     C                   eval      TADftr = *blanks
024700041116     C                   eval      TADdtr = §oggi
024800031105     C                   update    titad04
024900031105     C                   enddo
025000031105
025100031105     C     ktam          setll     titpt01l
025200031105     C                   do        *hival
025300031105     C     ktam          reade     titpt01l
025400031105     C                   if        %eof(titpt01l)
025500031105     C                   leave
025600031105     C                   endif
025700031105     C                   if        TPTatb <> *blanks
025800031105     C                   iter
025900031105     C                   endif
026000031105     C                   eval      TPTprg = (TPTprg + 1)
026100031105     C                   eval      TPTduv = §oggi
026200031105     C                   eval      TPTftr = *blanks
026300061128     C                   eval      TPTdtr = §oggi
026400031105     C                   update    titpt01
026500031105     C                   enddo
026600031105
026700031105     C     ktam          setll     titpd01l
026800031105     C                   do        *hival
026900031105     C     ktam          reade     titpd01l
027000031105     C                   if        %eof(titpd01l)
027100031105     C                   leave
027200031105     C                   endif
027300031105     C                   if        TPDatb <> *blanks
027400031105     C                   iter
027500031105     C                   endif
027600031105     C                   eval      TPDprg = (TPDprg + 1)
027700031105     C                   eval      TPDftr = *blanks
027800041116     C                   eval      TPDdtr = §oggi
027900031105     C                   update    titpd01
028000031105     C                   enddo
028100031105
028200031105     C     ktam          setll     titgc01l
028300031105     C                   do        *hival
028400031105     C     ktam          reade     titgc01l
028500031105     C                   if        %eof(titgc01l)
028600031105     C                   leave
028700031105     C                   endif
028800031105     C                   if        TGCatb <> *blanks
028900031105     C                   iter
029000031105     C                   endif
029100031105     C                   eval      TGCprg = (TGCprg + 1)
029200031105     C                   eval      TGCduv = §oggi
029300031105     C                   eval      TGCftr = *blanks
029400041116     C                   eval      TGCdtr = §oggi
029500031105     C                   update    titgc01
029600031105     C                   enddo
029700031105
029800031105     C                   endsr
029900031105      *****************************************************************
030000031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
030100031105      *****************************************************************
030200031105     C     Sr_Annulla    BEGSR
030300031105
030400031105     C                   eval      kprg = (kprg + 1)
030500031105
030600031105     C     ktam          setll     titad04l
030700031105     C                   do        *hival
030800031105     C     ktam          reade     titad04l
030900031105     C                   if        %eof(titad04l)
031000031105     C                   leave
031100031105     C                   endif
031200031105     C                   if        TADatb = *blanks
031300031105     C                   iter
031400031105     C                   endif
031500031105     C                   delete    titad04
031600031105     C                   enddo
031700031105
031800031105     C     ktam          setll     titpt01l
031900031105     C                   do        *hival
032000031105     C     ktam          reade     titpt01l
032100031105     C                   if        %eof(titpt01l)
032200031105     C                   leave
032300031105     C                   endif
032400031105     C                   if        TPTatb = *blanks
032500031105     C                   iter
032600031105     C                   endif
032700031105     C                   delete    titpt01
032800031105     C                   enddo
032900031105
033000031105     C     ktam          setll     titpd01l
033100031105     C                   do        *hival
033200031105     C     ktam          reade     titpd01l
033300031105     C                   if        %eof(titpd01l)
033400031105     C                   leave
033500031105     C                   endif
033600031105     C                   if        TPDatb = *blanks
033700031105     C                   iter
033800031105     C                   endif
033900031105     C                   delete    titpd01
034000031105     C                   enddo
034100031105
034200031105     C     ktam          setll     titgc01l
034300031105     C                   do        *hival
034400031105     C     ktam          reade     titgc01l
034500031105     C                   if        %eof(titgc01l)
034600031105     C                   leave
034700031105     C                   endif
034800031105     C                   if        TGCatb = *blanks
034900031105     C                   iter
035000031105     C                   endif
035100031105     C                   delete    titgc01
035200031105     C                   enddo
035300031105
035400031105     C     ktam          chain     tntam01l
035500031105     C                   if        %found(tntam01l)
035600031105     C                             and old_TAMatb <> *blanks
035700031105     C                   delete    tntam01
035800031105     C                   endif
035900031105
036000031105     C                   eval      kprg = (kprg - 1)
036100031105
036200031105     C                   endsr
036300031105      *****************************************************************
036400031105      * SCRIVE LE NUOVE TARIFFE
036500031105      *****************************************************************
036600031105     C     Sr_Scrivi     BEGSR
036700031105
036800031105     C     ktam          setll     titad04l
036900031105     C                   do        *hival
037000031105     C     ktam          reade     titad04l
037100031105     C                   if        %eof(titad04l)
037200031105     C                   leave
037300031105     C                   endif
037400031105     C                   if        TADatb <> *blanks
037500031105     C                   iter
037600031105     C                   endif
037700031105     C                   eval      new_TADksc = TADksc
037800031105     C                   eval      new_TADctr = TADctr
037900031105     C                   eval      new_TADprg = (kprg + 1)
038000031105     C                   eval      new_TADlnp = TADlnp
038100031105     C                   eval      new_TADcts = TADcts
038200031105     C                   eval      new_TADnaz = TADnaz
038300031105     C                   eval      new_TADcap = TADcap
038400031105     C                   eval      new_TADsgl = TADsgl
038500031105     C                   eval      new_TADitr = TADitr
038600031105     C                   eval      new_TADino = TADino
038700031105     C                   eval      new_TADrpv = TADrpv
038800031105     C                   eval      new_TADorp = TADorp
038900031105     C                   eval      new_TADmin = TADmin
039000031105     C                   eval      new_TADmax = TADmax
039100031105     C                   eval      new_TADftr = *blanks
039200041116     C                   eval      new_TADdtr = §oggi
039300031105     C                   write     titad000
039400031105     C                   enddo
039500031105
039600031105     C     ktam          setll     titpt01l
039700031105     C                   do        *hival
039800031105     C     ktam          reade     titpt01l
039900031105     C                   if        %eof(titpt01l)
040000031105     C                   leave
040100031105     C                   endif
040200031105     C                   if        TPTatb <> *blanks
040300031105     C                   iter
040400031105     C                   endif
040500031105     C                   eval      new_TPTksc = TPTksc
040600031105     C                   eval      new_TPTctr = TPTctr
040700031105     C                   eval      new_TPTprg = (kprg + 1)
040800031105     C                   eval      new_TPTduv = §oggi
040900031105     C                   eval      new_TPTftc = TPTftc
041000031105     C                   eval      new_TPTtpg = TPTtpg
041100031105     C                   eval      new_TPTarl = TPTarl
041200031105     C                   eval      new_TPTarf = TPTarf
041300031105     C                   eval      new_TPTaro = TPTaro
041400031105     C                   eval      new_TPTrpv = TPTrpv
041500031105     C                   eval      new_TPTvlm = TPTvlm
041600031105     C                   eval      new_TPTvvm = TPTvvm
041700031105     C                   eval      new_TPTfvm = TPTfvm
041800031105     C                   eval      new_TPTtma = TPTtma
041900031105     C                   eval      new_TPTapl = TPTapl
042000031105     C                   eval      new_TPTftr = *blanks
042100041116     C                   eval      new_TPTdtr = §oggi
042200061128     C                   eval      new_TPTdpb = TPTdpb
042300061128     C                   eval      new_TPTdit = TPTdit
042400061128     C                   eval      new_TPTflo = TPTflo
042500031105     C                   write     titpt000
042600031105     C                   enddo
042700031105
042800031105     C     ktam          setll     titpd01l
042900031105     C                   do        *hival
043000031105     C     ktam          reade     titpd01l
043100031105     C                   if        %eof(titpd01l)
043200031105     C                   leave
043300031105     C                   endif
043400031105     C                   if        TPDatb <> *blanks
043500031105     C                   iter
043600031105     C                   endif
043700031105     C                   eval      new_TPDksc = TPDksc
043800031105     C                   eval      new_TPDctr = TPDctr
043900031105     C                   eval      new_TPDprg = (kprg + 1)
044000031105     C                   eval      new_TPDftc = TPDftc
044100031105     C                   eval      new_TPDcts = TPDcts
044200031105     C                   eval      new_TPDorp = TPDorp
044300031105     C                   eval      new_TPDsgl = TPDsgl
044400031105     C                   eval      new_TPDitr = TPDitr
044500031105     C                   eval      new_TPDmin = TPDmin
044600031105     C                   eval      new_TPDmax = TPDmax
044700031105     C                   eval      new_TPDain = TPDain
044800031105     C                   eval      new_TPDftr = *blanks
044900041116     C                   eval      new_TPDdtr = §oggi
045000031105     C                   write     titpd000
045100031105     C                   enddo
045200031105
045300031105     C     ktam          setll     titgc01l
045400031105     C                   do        *hival
045500031105     C     ktam          reade     titgc01l
045600031105     C                   if        %eof(titgc01l)
045700031105     C                   leave
045800031105     C                   endif
045900031105     C                   if        TGCatb <> *blanks
046000031105     C                   iter
046100031105     C                   endif
046200031105     C                   eval      new_TGCksc = TGCksc
046300031105     C                   eval      new_TGCctr = TGCctr
046400031105     C                   eval      new_TGCprg = (kprg + 1)
046500031105     C                   eval      new_TGCsgv = TGCsgv
046600031105     C                   eval      new_TGCsgr = TGCsgr
046700031105     C                   eval      new_TGCsgp = TGCsgp
046800031105     C                   eval      new_TGCsgd = TGCsgd
046900031105     C                   eval      new_TGCsg1 = TGCsg1
047000031105     C                   eval      new_TGCsg2 = TGCsg2
047100031105     C                   eval      new_TGCsg3 = TGCsg3
047200031105     C                   eval      new_TGCst1 = TGCst1
047300031105     C                   eval      new_TGCst2 = TGCst2
047400031105     C                   eval      new_TGCst3 = TGCst3
047500031105     C                   eval      new_TGCstm = TGCstm
047600031105     C                   eval      new_TGCrpv = TGCrpv
047700031105     C                   eval      new_TGCfaf = TGCfaf
047800031105     C                   eval      new_TGCsgf = TGCsgf
047900031105     C                   eval      new_TGCggr = TGCggr
048000031105     C                   eval      new_TGCtcm = TGCtcm
048100031105     C                   eval      new_TGCtfg = TGCtfg
048200031105     C                   eval      new_TGCduv = §oggi
048300031105     C                   eval      new_TGCftr = *blanks
048400041116     C                   eval      new_TGCdtr = §oggi
048500031105     C                   write     titgc000
048600031105     C                   enddo
048700031105
048800031105     C                   eval      new_TAMksc = TAMksc
048900031105     C                   eval      new_TAMctr = TAMctr
049000031105     C                   eval      new_TAMprg = (kprg + 1)
049100071030     C                   eval      new_TAMddt = 20080101
049200031105     C                   eval      new_TAMdst = §dst
049300031105     C                   eval      new_TAMduv = §oggi
049400031105     C                   eval      new_TAMdcv = TAMdcv
049500031105     C                   eval      new_TAMdif = TAMdif
049600031105     C                   eval      new_TAMfli = TAMfli
049700031105     C                   eval      new_TAMpri = TAMpri
049800031105     C                   eval      new_TAMfmp = TAMfmp
049900031105     C                   eval      new_TAMlrc = TAMlrc
050000031105     C                   eval      new_TAMlas = TAMlas
050100051021      * se % addizionale corrente maggiore di zero devo addizionarla a quella precedente
050200051021     c                   If        TAMppa > 0
050300051109     c                   eval      caladd = tampag + tamppa +
050400051109     c                                      ((tampag *tamppa)/100)
050500051109     c                   z-add(h)  caladd        new_TAMpag
050600051021     c                   eval      wadflg = 'I'
050700051021     c                   eval      new_TAMppa = *zeros
050800051104
050900051104     c                   else
051000051104     c                   eval      new_TAMpag = TAMpag
051100051021     c                   endif
051200051021      * se aumento
051300051021     c                   If        WFAftb = 'A'
051400051103     C                   eval      new_TAMppa = wfaper
051500051021     c                   eval      wadflg = 'A'
051600051021     c                   endif
051700051021
051800031105     C                   eval      new_TAMmpa = TAMmpa
051900031105     C                   eval      new_TAMpam = TAMpam
052000031105     C                   eval      new_TAMmpm = TAMmpm
052100031105     C                   eval      new_TAMtin = TAMtin
052200031105     C                   eval      new_TAMkpm = TAMkpm
052300031105     C                   eval      new_TAMarl = TAMarl
052400031105     C                   eval      new_TAMarf = TAMarf
052500031105     C                   eval      new_TAMaro = TAMaro
052600031105     C                   eval      new_TAMrat = TAMrat
052700031105     C                   eval      new_TAMrct = TAMrct
052800031105     C                   eval      new_TAMsc2 = TAMsc2
052900031105     C                   eval      new_TAMeds = TAMeds
053000031105     C                   eval      new_TAMars = TAMars
053100031105     C                   eval      new_TAMata = TAMata
053200031105     C                   eval      new_TAMftp = TAMftp
053300031105     C                   eval      new_TAMbap = TAMbap
053400031105     C                   eval      new_TAMtsp = TAMtsp
053500031105     C                   eval      new_TAMfie = TAMfie
053600031105     C                   eval      new_TAMfis = TAMfis
053700031105     C                   eval      new_TAMsis = TAMsis
053800031105     C                   eval      new_TAMfvd = TAMfvd
053900031105     C                   eval      new_TAMfts = TAMfts
054000031105     C                   eval      new_TAMnas = TAMnas
054100031105     C                   eval      new_TAMnot = TAMnot
054200031105     C                   eval      new_TAMftm = TAMftm
054300031105     C                   eval      new_TAMgca = TAMgca
054400031105     C                   eval      new_TAMgga = TAMgga
054500031105     C                   eval      new_TAMgma = TAMgma
054600031105     C                   eval      new_TAMgva = TAMgva
054700031105     C                   eval      new_TAMbam = TAMbam
054800031105     C                   eval      new_TAMtpr = TAMtpr
054900031105     C                   eval      new_TAMflo = TAMflo
055000031105     C                   eval      new_TAMdat = TAMdat
055100031105     C                   eval      new_TAMftr = *blanks
055200041116     C                   eval      new_TAMdtr = §oggi
055300031105     C                   write     tntam000
055400031105
055500031105     C                   endsr
055600031105      *****************************************************************
055700031105      * SCRIVE IL FILE WADDT00F
055800031105      *****************************************************************
055900031105     C     Sr_ScriviWadd BEGSR
056000031105
056100031105     C                   eval      WADksc = TAMksc
056200031105     C                   eval      WADctr = TAMctr
056300031105     C                   eval      WADprg = §prg
056400051104     C                   eval      WADpag = §pag
056500051024     C                   eval      WADppa = §ppa
056600031105     C                   write     waddt000
056700031105
056800031105     C                   endsr
056900031105      *****************************************************************
057000031105      * ROUTINE INIZIALE
057100031105      *****************************************************************
057200031105     C     *INZSR        BEGSR
057300031105
057400031105     C     *ENTRY        PLIST
057500031105     C                   PARM                    KPJBA
057600031105      *
057700031105     c
057800031105     C     Ktam          klist
057900031105     C                   kfld                    TAMksc
058000031105     C                   kfld                    TAMctr
058100031105     C                   kfld                    Kprg
058200031105
058300031105
058400031105      * reperisce data del giorno
058500031105     C                   TIME                    W0140
058600031105      * UDATE IN GGMMAAAA
058700031105     C                   MOVE      W0140         WDTGIO
058800031105      * UDATE IN AAAAMMGG
058900031105     C                   Z-ADD     WDTGIO        G02DAT
059000031105     C                   MOVEL     *BLANK        G02ERR
059100031105     C                   CALL      'XSRDA8'
059200031105     C                   PARM                    WLBDAT
059300031105     C                   Z-ADD     G02INV        §oggi
059400031105
059500031105     C                   endsr
