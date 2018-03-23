000100091203     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500091201      *       AGGIORNA TARIFFE CON ADDIZIONALE E ISTAT                *
000600031105      *                                                               *
000700031105      *****************************************************************
000800081128     FWfaut01l  uf   E           K DISK
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
002400031105
002500111213     fTitav01l  if a e           k disk
002600111213
002700081201     FTabel00f  if   e           k DISK
002800031105      *------------------------------------------------------------------------*
002900031105
003000081128     D W_wfaclf        s                   like(wfaclf)
003100081128     D W_wfatcl        s                   like(wfatcl)
003200081128     D W_wfafauc       s                   like(wfafauc)
003300081128
003400081128
003500031105     D Kprg            S                   LIKE(TAMprg)
003600031105     D §ksc            S                   LIKE(TAMksc)
003700031105     D §ctr            S                   LIKE(TAMctr)
003800031105     D §prg            S                   LIKE(TAMprg)
003900051024     D §ppa            S                   LIKE(TAMppa)
004000051104     D §pag            S                   LIKE(TAMpag)
004100031105     D §dst            S                   LIKE(TAMdst)
004200081201     d AU_Tipo         s                   like(WFAtcl) inz('C')
004300081201     d AU_Cliente      s                   like(WFAclf)
004400081201     d AU_Tariffa      s                   like(WFActrv)
004500081201     d AU_Progres      s                   like(WFAprgv)
004600081201     D Kcod            S                   like(TBLCOD)
004700081201     D Kkey            S                   like(TBLKEY)
004800081201     D Codut           s              1  0 inz(1)
004900081201
005000031105     D W0140           S             14  0
005100031105     D Wdtgio          S              8  0
005200031105     D §oggi           S              8  0
005300051109     D caladd          s             30  9
005400031105     D §flagA          S              1
005500031105     D §flagB          S              1
005600031105     D §flagC          S              1
005700031105     D §flagD          S              1
005800081201     D Up_tam          S              1
005900081201     D perce           s              5  2
006000121129     D ISTAT           s              3  0 inz(9)
006100031105
006200031105      *   D S   I N T E R N E / E S T E R N E
006300031105
006400031105     D WLBDAT          DS                  INZ
006500031105     D  G02DAT                 1      8  0
006600031105     D  G02INV                 9     16  0
006700031105     D  G02ERR                17     17
006800031105     D  G02TGI                18     22  0
006900031105
007000031105     D                 ds
007100031105     D tamksc                  1      7  0
007200031105     D  ksc4                   1      4  0
007300031105     D  ksc33                  1      3  0
007400031105     D  ksc3                   5      7  0
007500031105
007600061128     D Dsta01        e ds
007700031105     D kpjba         e ds
007800031105
007900031105      *---------------------------------------------------------------------------------------------
008000081128      *------------------------------------------------------------------------*
008100081128      * Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008200081128     c
008300081128     c/EXEC SQL
008400081128     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008500081128     c/END-EXEC
008600081128     c*
008700081128     C/EXEC SQL
008800081128     C+ declare C1 cursor for
008900081128     C+ SELECT wfatcl , wfaclf, wfafauc  FROM EDPAUMENTI/WFaut00F
009000081128     C+ WHERE WFATCL = 'C'
009100081201     C+ group BY wfatcl , wfaclf, wfafauc
009200081128     c+ for read only
009300081128     C/END-EXEC
009400081128
009500081128     C/EXEC SQL
009600081128     C+ open C1
009700081128     C/END-EXEC
009800081128     C
009900081128     C/EXEC SQL
010000081128     C+ Fetch C1 into :W_wfatcl , :W_wfaclf , :W_wfafauc
010100081128     C/END-EXEC
010200081128     C*
010300081128
010400081128     C*
010500081128     C                   dow       sqlcod = *zeros
010600031105
010700031105      *
010800031105      * Aggancio Tntam04l
010900081128     c     W_Wfaclf      setll     tntam04l
011000031105     c                   do        *hival
011100081128     c     W_wfaclf      reade     tntam04l
011200031105      *
011300031105      * fine file
011400031105     c                   if        %eof(tntam04l)
011500031105     c                   leave
011600031105     c                   endif
011700081201
011800081201     c                   eval      AU_cliente = tamksc
011900081201     c                   eval      AU_tariffa = tamctr
012000081201     c                   eval      AU_progres = tamprg
012100031105
012200031105      * Se stessa tariffa e si è verificata una delle condizioni
012300031105      * non deve elaborare più niente
012400031105     C                   if        (TAMksc = §ksc and TAMctr = §ctr
012500031105     C                             and §flagA = *on) or
012600031105     C                             (TAMksc = §ksc and TAMctr = §ctr
012700031105     C                             and §flagB = *on) or
012800031105     C                             (TAMksc = §ksc and TAMctr = §ctr
012900031105     C                             and §flagC = *on)
013000031105     C                   iter
013100031105     C                   endif
013200031105
013300031105      * Cambia tariffa
013400031105     C                   if        TAMksc <> §ksc
013500081201     C                   eval      §ksc = TAMksc
013600031105     C                   eval      §flagA = *off
013700031105     C                   eval      §flagB = *off
013800031105     C                   eval      §flagC = *off
013900031105     C                   endif
014000031105     C                   if        TAMctr <> §ctr
014100031105     C                   eval      §ctr = TAMctr
014200031105     C                   eval      §flagA = *off
014300031105     C                   eval      §flagB = *off
014400031105     C                   eval      §flagC = *off
014500031105     C                   endif
014600031105
014700061128      * valorizzo i campi dei flag operativi
014800061128     c                   movel     tamflo        dsta01
014900031105
015000141209      * Scadenza maggiore/uguale 01/01/2015 e decorrenza minore 01/01/2015
015100141209      * sono a cavallo d'anno
015200141209    1C                   if        TAMdst >= 20150101 and TAMddt < 20150101
015300031105     C                   eval      §flagB = *on
015400031105     C                   eval      §dst = TAMdst
015500031105     C                   eval      kprg = TAMprg
015600141209      * devo creare un nuovo progressivo
015700141209
015800141209
015900141209
016000141209
016100051021      * verifico se devo aggiornare la tariffa  o per integrazione o per aumento altrimenti
016200051021      * non faccio nulla
016300081201     c                   clear                   Up_tam
016400081201      * deleta i record annullati del progressivo successivo
016500031105     C                   exsr      Sr_Annulla
016600141209      * se esiste % anni corrente faccio l'inglobamento
016700141209    2c                   If        tamppa > 0
016800141209      * se esiste % anni precedente sommiamo le 2 percentuali
016900141209    3c                   If        tampag > 0
017000141209     c                   eval      caladd = tampag + tamppa +
017100141209     c                                      ((tampag *tamppa)/100)
017200141209     c                   z-add(h)  caladd        tampag
017300141209     c                   else
017400141209     c                   eval      tampag = tamppa
017500141209    3c                   endif
017600141209     c                   eval      tamppa = 0
017700141209     c                   eval      up_tam = *on
017800141209    2c                   endif
017900081201      * scrivo il nuovo progressivo del dettaglio a cavallo d'anno
018000031105     C                   exsr      Sr_Scrivi
018100031105     C                   eval      §prg = (kprg +1)
018200051024     C                   eval      §ppa = New_TAMppa
018300051104     C                   eval      §pag = New_TAMpag
018400141209     C                   eval      TAMdst = 20141231
018500031105     C                   eval      TAMduv = §oggi
018600031105     C                   eval      TAMftr = *blanks
018700041116     C                   eval      TAMdtr = §oggi
018800031105     C                   update    tntam04
018900111213      * --> SCRIVO TITAV
019000111213     c                   Clear                   Titav000
019100111213     c                   Eval      tavksc = tamksc
019200111213     c                   Eval      tavctr = tamctr
019300111213     c                   Eval      tavprg = tamprg
019400111213     c                   Eval      tavtrd = 'TES'
019500111213     c                   Eval      tavcta = 'D'
019600111213     c                   Eval      tavdav = *date
019700111213     c                   Time                    w0140
019800111213     c                   Movel     w0140         tavorv
019900111213     c                   Eval      tavnoj = 'AUMENTO '
020000111213     c                   Eval      tavpru = 'BATCH'
020100111213     c                   Eval      tavdtr = *date
020200111213     c                   Write     Titav000
020300031105     C                   iter
020400051021     C                   endif
020500031105
020600141209      * Scadenza minore/uguale 31/12/2014
020700141209     C                   if        TAMdst <= 20141231
020800031105     C                   eval      §flagC = *on
020900051103      *
021000141209     C                   eval      §dst = 20151231
021100031105     C                   eval      kprg = TAMprg
021200031105     C                   exsr      Sr_Annulla
021300031105     C                   exsr      Sr_Scrivi
021400031105     C                   endif
021500051021
021600031105     C                   enddo
021700081201     C
021800081201     C/EXEC SQL
021900081201     C+ Fetch C1 into :W_wfatcl , :W_wfaclf , :W_wfafauc
022000081201     C/END-EXEC
022100081201     C*
022200081201     C                   enddo
022300051021
022400081201     C*
022500081201     C/EXEC SQL
022600081201     C+ close C1
022700081201     C/END-EXEC
022800081201     C
022900081201
023000081201     c                   seton                                        lr
023100081201
023200031105      *****************************************************************
023300031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
023400031105      *****************************************************************
023500031105     C     Sr_Aggiorna   BEGSR
023600031105
023700031105     C     ktam          setll     titad04l
023800031105     C                   do        *hival
023900031105     C     ktam          reade     titad04l
024000031105     C                   if        %eof(titad04l)
024100031105     C                   leave
024200031105     C                   endif
024300031105     C                   if        TADatb <> *blanks
024400031105     C                   iter
024500031105     C                   endif
024600031105     C                   eval      TADprg = (TADprg + 1)
024700031105     C                   eval      TADftr = *blanks
024800041116     C                   eval      TADdtr = §oggi
024900031105     C                   update    titad04
025000031105     C                   enddo
025100031105
025200031105     C     ktam          setll     titpt01l
025300031105     C                   do        *hival
025400031105     C     ktam          reade     titpt01l
025500031105     C                   if        %eof(titpt01l)
025600031105     C                   leave
025700031105     C                   endif
025800031105     C                   if        TPTatb <> *blanks
025900031105     C                   iter
026000031105     C                   endif
026100031105     C                   eval      TPTprg = (TPTprg + 1)
026200031105     C                   eval      TPTduv = §oggi
026300031105     C                   eval      TPTftr = *blanks
026400061128     C                   eval      TPTdtr = §oggi
026500031105     C                   update    titpt01
026600031105     C                   enddo
026700031105
026800031105     C     ktam          setll     titpd01l
026900031105     C                   do        *hival
027000031105     C     ktam          reade     titpd01l
027100031105     C                   if        %eof(titpd01l)
027200031105     C                   leave
027300031105     C                   endif
027400031105     C                   if        TPDatb <> *blanks
027500031105     C                   iter
027600031105     C                   endif
027700031105     C                   eval      TPDprg = (TPDprg + 1)
027800031105     C                   eval      TPDftr = *blanks
027900041116     C                   eval      TPDdtr = §oggi
028000031105     C                   update    titpd01
028100031105     C                   enddo
028200031105
028300031105     C     ktam          setll     titgc01l
028400031105     C                   do        *hival
028500031105     C     ktam          reade     titgc01l
028600031105     C                   if        %eof(titgc01l)
028700031105     C                   leave
028800031105     C                   endif
028900031105     C                   if        TGCatb <> *blanks
029000031105     C                   iter
029100031105     C                   endif
029200031105     C                   eval      TGCprg = (TGCprg + 1)
029300031105     C                   eval      TGCduv = §oggi
029400031105     C                   eval      TGCftr = *blanks
029500041116     C                   eval      TGCdtr = §oggi
029600031105     C                   update    titgc01
029700031105     C                   enddo
029800031105
029900031105     C                   endsr
030000031105      *****************************************************************
030100031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
030200031105      *****************************************************************
030300031105     C     Sr_Annulla    BEGSR
030400031105
030500031105     C                   eval      kprg = (kprg + 1)
030600031105
030700031105     C     ktam          setll     titad04l
030800031105     C                   do        *hival
030900031105     C     ktam          reade     titad04l
031000031105     C                   if        %eof(titad04l)
031100031105     C                   leave
031200031105     C                   endif
031300031105     C                   if        TADatb = *blanks
031400031105     C                   iter
031500031105     C                   endif
031600031105     C                   delete    titad04
031700031105     C                   enddo
031800031105
031900031105     C     ktam          setll     titpt01l
032000031105     C                   do        *hival
032100031105     C     ktam          reade     titpt01l
032200031105     C                   if        %eof(titpt01l)
032300031105     C                   leave
032400031105     C                   endif
032500031105     C                   if        TPTatb = *blanks
032600031105     C                   iter
032700031105     C                   endif
032800031105     C                   delete    titpt01
032900031105     C                   enddo
033000031105
033100031105     C     ktam          setll     titpd01l
033200031105     C                   do        *hival
033300031105     C     ktam          reade     titpd01l
033400031105     C                   if        %eof(titpd01l)
033500031105     C                   leave
033600031105     C                   endif
033700031105     C                   if        TPDatb = *blanks
033800031105     C                   iter
033900031105     C                   endif
034000031105     C                   delete    titpd01
034100031105     C                   enddo
034200031105
034300031105     C     ktam          setll     titgc01l
034400031105     C                   do        *hival
034500031105     C     ktam          reade     titgc01l
034600031105     C                   if        %eof(titgc01l)
034700031105     C                   leave
034800031105     C                   endif
034900031105     C                   if        TGCatb = *blanks
035000031105     C                   iter
035100031105     C                   endif
035200031105     C                   delete    titgc01
035300031105     C                   enddo
035400031105
035500031105     C     ktam          chain     tntam01l
035600031105     C                   if        %found(tntam01l)
035700031105     C                             and old_TAMatb <> *blanks
035800031105     C                   delete    tntam01
035900031105     C                   endif
036000031105
036100031105     C                   eval      kprg = (kprg - 1)
036200031105
036300031105     C                   endsr
036400031105      *****************************************************************
036500031105      * SCRIVE LE NUOVE TARIFFE
036600031105      *****************************************************************
036700031105     C     Sr_Scrivi     BEGSR
036800031105
036900081202     c                   eval      up_tam = *on
037000081202
037100031105     C     ktam          setll     titad04l
037200031105     C                   do        *hival
037300031105     C     ktam          reade     titad04l
037400031105     C                   if        %eof(titad04l)
037500031105     C                   leave
037600031105     C                   endif
037700031105     C                   if        TADatb <> *blanks
037800031105     C                   iter
037900031105     C                   endif
038000031105     C                   eval      new_TADksc = TADksc
038100031105     C                   eval      new_TADctr = TADctr
038200031105     C                   eval      new_TADprg = (kprg + 1)
038300031105     C                   eval      new_TADlnp = TADlnp
038400031105     C                   eval      new_TADcts = TADcts
038500031105     C                   eval      new_TADnaz = TADnaz
038600031105     C                   eval      new_TADcap = TADcap
038700031105     C                   eval      new_TADsgl = TADsgl
038800031105     C                   eval      new_TADitr = TADitr
038900031105     C                   eval      new_TADino = TADino
039000031105     C                   eval      new_TADrpv = TADrpv
039100031105     C                   eval      new_TADorp = TADorp
039200031105     C                   eval      new_TADmin = TADmin
039300031105     C                   eval      new_TADmax = TADmax
039400031105     C                   eval      new_TADftr = *blanks
039500041116     C                   eval      new_TADdtr = §oggi
039600031105     C                   write     titad000
039700031105     C                   enddo
039800031105
039900031105     C     ktam          setll     titpt01l
040000031105     C                   do        *hival
040100031105     C     ktam          reade     titpt01l
040200031105     C                   if        %eof(titpt01l)
040300031105     C                   leave
040400031105     C                   endif
040500031105     C                   if        TPTatb <> *blanks
040600031105     C                   iter
040700031105     C                   endif
040800031105     C                   eval      new_TPTksc = TPTksc
040900031105     C                   eval      new_TPTctr = TPTctr
041000031105     C                   eval      new_TPTprg = (kprg + 1)
041100031105     C                   eval      new_TPTduv = §oggi
041200031105     C                   eval      new_TPTftc = TPTftc
041300031105     C                   eval      new_TPTtpg = TPTtpg
041400031105     C                   eval      new_TPTarl = TPTarl
041500031105     C                   eval      new_TPTarf = TPTarf
041600031105     C                   eval      new_TPTaro = TPTaro
041700031105     C                   eval      new_TPTrpv = TPTrpv
041800031105     C                   eval      new_TPTvlm = TPTvlm
041900031105     C                   eval      new_TPTvvm = TPTvvm
042000031105     C                   eval      new_TPTfvm = TPTfvm
042100031105     C                   eval      new_TPTtma = TPTtma
042200031105     C                   eval      new_TPTapl = TPTapl
042300031105     C                   eval      new_TPTftr = *blanks
042400041116     C                   eval      new_TPTdtr = §oggi
042500061128     C                   eval      new_TPTdpb = TPTdpb
042600061128     C                   eval      new_TPTdit = TPTdit
042700061128     C                   eval      new_TPTflo = TPTflo
042800031105     C                   write     titpt000
042900031105     C                   enddo
043000031105
043100031105     C     ktam          setll     titpd01l
043200031105     C                   do        *hival
043300031105     C     ktam          reade     titpd01l
043400031105     C                   if        %eof(titpd01l)
043500031105     C                   leave
043600031105     C                   endif
043700031105     C                   if        TPDatb <> *blanks
043800031105     C                   iter
043900031105     C                   endif
044000031105     C                   eval      new_TPDksc = TPDksc
044100031105     C                   eval      new_TPDctr = TPDctr
044200031105     C                   eval      new_TPDprg = (kprg + 1)
044300031105     C                   eval      new_TPDftc = TPDftc
044400031105     C                   eval      new_TPDcts = TPDcts
044500031105     C                   eval      new_TPDorp = TPDorp
044600031105     C                   eval      new_TPDsgl = TPDsgl
044700031105     C                   eval      new_TPDitr = TPDitr
044800031105     C                   eval      new_TPDmin = TPDmin
044900031105     C                   eval      new_TPDmax = TPDmax
045000031105     C                   eval      new_TPDain = TPDain
045100031105     C                   eval      new_TPDftr = *blanks
045200041116     C                   eval      new_TPDdtr = §oggi
045300031105     C                   write     titpd000
045400031105     C                   enddo
045500031105
045600031105     C     ktam          setll     titgc01l
045700031105     C                   do        *hival
045800031105     C     ktam          reade     titgc01l
045900031105     C                   if        %eof(titgc01l)
046000031105     C                   leave
046100031105     C                   endif
046200031105     C                   if        TGCatb <> *blanks
046300031105     C                   iter
046400031105     C                   endif
046500031105     C                   eval      new_TGCksc = TGCksc
046600031105     C                   eval      new_TGCctr = TGCctr
046700031105     C                   eval      new_TGCprg = (kprg + 1)
046800031105     C                   eval      new_TGCsgv = TGCsgv
046900031105     C                   eval      new_TGCsgr = TGCsgr
047000031105     C                   eval      new_TGCsgp = TGCsgp
047100031105     C                   eval      new_TGCsgd = TGCsgd
047200031105     C                   eval      new_TGCsg1 = TGCsg1
047300031105     C                   eval      new_TGCsg2 = TGCsg2
047400031105     C                   eval      new_TGCsg3 = TGCsg3
047500031105     C                   eval      new_TGCst1 = TGCst1
047600031105     C                   eval      new_TGCst2 = TGCst2
047700031105     C                   eval      new_TGCst3 = TGCst3
047800031105     C                   eval      new_TGCstm = TGCstm
047900031105     C                   eval      new_TGCrpv = TGCrpv
048000031105     C                   eval      new_TGCfaf = TGCfaf
048100031105     C                   eval      new_TGCsgf = TGCsgf
048200031105     C                   eval      new_TGCggr = TGCggr
048300031105     C                   eval      new_TGCtcm = TGCtcm
048400031105     C                   eval      new_TGCtfg = TGCtfg
048500031105     C                   eval      new_TGCduv = §oggi
048600031105     C                   eval      new_TGCftr = *blanks
048700041116     C                   eval      new_TGCdtr = §oggi
048800031105     C                   write     titgc000
048900031105     C                   enddo
049000031105
049100031105     C                   eval      new_TAMksc = TAMksc
049200031105     C                   eval      new_TAMctr = TAMctr
049300031105     C                   eval      new_TAMprg = (kprg + 1)
049400131209     C                   eval      new_TAMddt = 20140101
049500031105     C                   eval      new_TAMdst = §dst
049600031105     C                   eval      new_TAMduv = §oggi
049700031105     C                   eval      new_TAMdcv = TAMdcv
049800031105     C                   eval      new_TAMdif = TAMdif
049900031105     C                   eval      new_TAMfli = TAMfli
050000031105     C                   eval      new_TAMpri = TAMpri
050100031105     C                   eval      new_TAMfmp = TAMfmp
050200031105     C                   eval      new_TAMlrc = TAMlrc
050300031105     C                   eval      new_TAMlas = TAMlas
050400051021      * se % addizionale corrente maggiore di zero devo addizionarla a quella precedente
050500051021     c                   If        TAMppa > 0
050600051109     c                   eval      caladd = tampag + tamppa +
050700051109     c                                      ((tampag *tamppa)/100)
050800051109     c                   z-add(h)  caladd        new_TAMpag
050900051021     c                   eval      new_TAMppa = *zeros
051000051104
051100051104     c                   else
051200051104     c                   eval      new_TAMpag = TAMpag
051300081202     c                   eval      new_TAMppa = TAMppa
051400051021     c                   endif
051500051021      * se aumento
051600081202     c                   If        W_WFAfauc = 'A'
051700081201     C                   eval      new_TAMppa = perce
051800051021     c                   endif
051900051021
052000031105     C                   eval      new_TAMmpa = TAMmpa
052100031105     C                   eval      new_TAMpam = TAMpam
052200031105     C                   eval      new_TAMmpm = TAMmpm
052300031105     C                   eval      new_TAMtin = TAMtin
052400031105     C                   eval      new_TAMkpm = TAMkpm
052500031105     C                   eval      new_TAMarl = TAMarl
052600031105     C                   eval      new_TAMarf = TAMarf
052700031105     C                   eval      new_TAMaro = TAMaro
052800031105     C                   eval      new_TAMrat = TAMrat
052900031105     C                   eval      new_TAMrct = TAMrct
053000101129      * se devo aggiornare l'Istat
053100101129    2c                   If        W_wfafauc = 'I' and tamrct = 0
053200101129     c                   eval      new_TAMrct = ISTAT
053300101129     c                   eval      new_TAMpri = 100
053400101129    2c                   endif
053500101129
053600031105     C                   eval      new_TAMsc2 = TAMsc2
053700031105     C                   eval      new_TAMeds = TAMeds
053800031105     C                   eval      new_TAMars = TAMars
053900031105     C                   eval      new_TAMata = TAMata
054000031105     C                   eval      new_TAMftp = TAMftp
054100031105     C                   eval      new_TAMbap = TAMbap
054200031105     C                   eval      new_TAMtsp = TAMtsp
054300031105     C                   eval      new_TAMfie = TAMfie
054400031105     C                   eval      new_TAMfis = TAMfis
054500031105     C                   eval      new_TAMsis = TAMsis
054600031105     C                   eval      new_TAMfvd = TAMfvd
054700031105     C                   eval      new_TAMfts = TAMfts
054800031105     C                   eval      new_TAMnas = TAMnas
054900031105     C                   eval      new_TAMnot = TAMnot
055000031105     C                   eval      new_TAMftm = TAMftm
055100031105     C                   eval      new_TAMgca = TAMgca
055200031105     C                   eval      new_TAMgga = TAMgga
055300031105     C                   eval      new_TAMgma = TAMgma
055400031105     C                   eval      new_TAMgva = TAMgva
055500031105     C                   eval      new_TAMbam = TAMbam
055600031105     C                   eval      new_TAMtpr = TAMtpr
055700031105     C                   eval      new_TAMflo = TAMflo
055800031105     C                   eval      new_TAMdat = TAMdat
055900031105     C                   eval      new_TAMftr = *blanks
056000041116     C                   eval      new_TAMdtr = §oggi
056100031105     C                   write     tntam000
056200111213      * --> SCRIVO TITAV
056300111213     c                   Clear                   Titav000
056400111213     c                   Eval      tavksc = New_tamksc
056500111213     c                   Eval      tavctr = New_tamctr
056600111213     c                   Eval      tavprg = New_tamprg
056700111213     c                   Eval      tavtrd = 'TES'
056800111213     c                   Eval      tavcta = 'D'
056900111213     c                   Eval      tavdav = *date
057000111213     c                   Time                    w0140
057100111213     c                   Movel     w0140         tavorv
057200111213     c                   Eval      tavnoj = 'AUMENTO '
057300111213     c                   Eval      tavpru = 'BATCH'
057400111213     c                   Eval      tavdtr = *date
057500111213     c                   Write     Titav000
057600081201      * aggiorno il progressivo vecchio con quello nuovo
057700081201     c                   If        Up_tam = *on
057800081201     c     kaut          chain     wfaut01l
057900081201    3c                   if        %found(wfaut01l)
058000081201     c                   eval      wfaddtn = new_tamddt
058100081201     c                   eval      wfadstn = new_tamdst
058200081201     c                   eval      wfaprgn = new_tamprg
058300101129    4c                   if        new_tamrct <> 0
058400081209     c                   eval      wfaistn = 'S'
058500081201     c                   else
058600081209     c                   eval      wfaistn = 'N'
058700081201    4c                   endif
058800081201     c                   eval      wfapagn = new_tampag
058900081201     c                   eval      wfappan = new_tamppa
059000081202     c                   eval      wfaela = 'S'
059100081201     c                   update    wfaut000
059200081201    3C                   endif
059300081201    2C                   endif
059400031105
059500031105     C                   endsr
059600031105      *****************************************************************
059700031105      * ROUTINE INIZIALE
059800031105      *****************************************************************
059900031105     C     *INZSR        BEGSR
060000031105
060100031105     C     *ENTRY        PLIST
060200031105     C                   PARM                    KPJBA
060300031105      *
060400031105     c
060500031105     C     Ktam          klist
060600031105     C                   kfld                    TAMksc
060700031105     C                   kfld                    TAMctr
060800031105     C                   kfld                    Kprg
060900031105
061000081201      *
061100081201     c
061200081201     C     Kaut          klist
061300081201     C                   kfld                    AU_Tipo
061400081201     C                   kfld                    AU_Cliente
061500081201     C                   kfld                    AU_Tariffa
061600081201     C                   kfld                    AU_Progres
061700081201
061800081201      *
061900081201     c     keytab        klist
062000081201     c                   kfld                    codut
062100081201     c                   kfld                    kcod
062200081201     c                   kfld                    kkey
062300081201      *
062400031105      * reperisce data del giorno
062500031105     C                   TIME                    W0140
062600031105      * UDATE IN GGMMAAAA
062700031105     C                   MOVE      W0140         WDTGIO
062800031105      * UDATE IN AAAAMMGG
062900031105     C                   Z-ADD     WDTGIO        G02DAT
063000031105     C                   MOVEL     *BLANK        G02ERR
063100031105     C                   CALL      'XSRDA8'
063200031105     C                   PARM                    WLBDAT
063300031105     C                   Z-ADD     G02INV        §oggi
063400031105
063500081201      * cerco le percentuali di aumento
063600081201     c                   clear                   perce
063700141209     c                   move(p)   '2014'        kkey
063800081201     c                   movel(p)  '%A'          kcod
063900081201     c     Keytab        chain     tabel00f
064000081201     c                   If        %found(tabel00f)
064100081201     c                   movel     tbluni        perce
064200081201     c                   endif
064300031105     C                   endsr
