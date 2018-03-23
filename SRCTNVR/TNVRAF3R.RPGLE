000100080811     H DECEDIT('0,') DATEDIT(*YMD.)  option(*nodebugio)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500080806      *       AGGIORNA TARIFFE CON FUEL                               *
000600031105      *                                                               *
000700031105      *****************************************************************
000800031105
000900031105     FTNTAM04L  UF   E           K DISK    rename(tntam000:tntam04)
001000031105     FTITAD04L  UF   E           K DISK    rename(titad000:titad04)
001100031105     FTITPT01L  UF   E           K DISK    rename(titpt000:titpt01)
001200031105     FTITPD01L  UF   E           K DISK    rename(titpd000:titpd01)
001300031105     FTITGC01L  UF   E           K DISK    rename(titgc000:titgc01)
001400031105
001500031105     FTNTAM01L  UF   E           K DISK    rename(tntam000:tntam01)
001600031105     F                                     prefix(old_)
001700080811     FTITPD02L  IF   E           K DISK    rename(titpd000:titpd02)
001800080807     F                                     prefix(old_)
001900080814     FTabel00f  if   e           k DISK
002000031105
002100031105     FTNTAM00F  O    E             DISK    prefix(new_)
002200031105     FTITAD00F  O    E             DISK    prefix(new_)
002300031105     FTITPT00F  O    E             DISK    prefix(new_)
002400031105     FTITPD00F  O    E             DISK    prefix(new_)
002500031105     FTITGC00F  O    E             DISK    prefix(new_)
002600080808     FWFUEL00F  O    E             DISK
002700031105
002800031105      *------------------------------------------------------------------------*
002900031105
003000031105     D Kprg            S                   LIKE(TAMprg)
003100080807     D Kftc            S                   LIKE(TPTftc) inz('f ')
003200031105     D §ksc            S                   LIKE(TAMksc)
003300031105     D §ctr            S                   LIKE(TAMctr)
003400031105     D §prg            S                   LIKE(TAMprg)
003500031105     D §dst            S                   LIKE(TAMdst)
003600080808     D §sgl            S                   LIKE(TPDsgl)
003700080808     D §fdp            S                   LIKE(TPDitr)
003800080905     D §fdn            S                   LIKE(TPDitr)
003900080905     D §dpb            S                   LIKE(TPTdpb)
004000080814     D Kcod            S                   like(TBLCOD)
004100080814     D Kkey            S                   like(TBLKEY)
004200080814     D Codut           s              1  0 inz(1)
004300080814
004400031105     D W0140           S             14  0
004500031105     D Wdtgio          S              8  0
004600080814     D data_base       S              8  0
004700080904     D sgl_base        S             13  3
004800031105     D §oggi           S              8  0
004900031105     D §flagA          S              1
005000031105     D §flagB          S              1
005100031105     D §flagC          S              1
005200031105     D §flagD          S              1
005300080807     D §fuel_tpt       S              1
005400031105
005500031105      *   D S   I N T E R N E / E S T E R N E
005600031105
005700031105     D WLBDAT          DS                  INZ
005800031105     D  G02DAT                 1      8  0
005900031105     D  G02INV                 9     16  0
006000031105     D  G02ERR                17     17
006100031105     D  G02TGI                18     22  0
006200031105
006300031105     D                 ds
006400031105     D tamksc                  1      7  0
006500031105     D  ksc4                   1      4  0
006600031105     D  ksc33                  1      3  0
006700031105     D  ksc3                   5      7  0
006800031105
006900080806     D PARAM           DS
007000080806     D  Wfaclf                 1      7  0
007100080806     D  Wfactr                 8     10  0
007200080905     D  Wfatip                11     11
007300080806
007400061128     D Dsta01        e ds
007500031105     D kpjba         e ds
007600031105
007700031105      *---------------------------------------------------------------------------------------------
007800031105
007900031105      *
008000031105      * Aggancio Tntam04l
008100080806     c     ktamin        setll     tntam04l
008200031105     c                   do        *hival
008300080806     c     ktamin        reade     tntam04l
008400031105      *
008500031105      * fine file
008600031105     c                   if        %eof(tntam04l)
008700031105     c                   leave
008800031105     c                   endif
008900031105
009000031105      * Se stessa tariffa e si è verificata una delle condizioni
009100031105      * non deve elaborare più niente
009200031105     C                   if        (TAMksc = §ksc and TAMctr = §ctr
009300031105     C                             and §flagA = *on) or
009400031105     C                             (TAMksc = §ksc and TAMctr = §ctr
009500031105     C                             and §flagB = *on) or
009600031105     C                             (TAMksc = §ksc and TAMctr = §ctr
009700031105     C                             and §flagC = *on)
009800031105     C                   iter
009900031105     C                   endif
010000031105
010100031105      * Cambia tariffa
010200031105     C                   if        TAMksc <> §ksc
010300031105     C                   eval      §ksc = TAMksc
010400031105     C                   eval      §flagA = *off
010500031105     C                   eval      §flagB = *off
010600031105     C                   eval      §flagC = *off
010700080808     C                   eval      §flagD = *off
010800031105     C                   endif
010900031105     C                   if        TAMctr <> §ctr
011000031105     C                   eval      §ctr = TAMctr
011100031105     C                   eval      §flagA = *off
011200031105     C                   eval      §flagB = *off
011300031105     C                   eval      §flagC = *off
011400080808     C                   eval      §flagD = *off
011500031105     C                   endif
011600080811
011700031105
011800061128      * valorizzo i campi dei flag operativi
011900061128     c                   movel     tamflo        dsta01
012000061128
012100080806      * Decorrenza maggiore/uguale 01/10/2008
012200080806     C                   if        TAMddt >= 20081001
012300080811     C                   eval      §flagA = (TAMddt = 20081001)
012400031105     C                   eval      §flagD = *off
012500080806     C                   if        tamddt <> 20081001 and tamdst <> 20081001
012600031105     C                   exsr      Sr_CercaDopo
012700031105     C                   if        §flagD = *on
012800031105     C                   eval      kprg = TAMprg
012900080808     C                   eval      §prg = (TAMprg + 1)
013000080811     C                   exsr      Sr_Annulla
013100031105     C                   exsr      Sr_Aggiorna
013200031105     C                   eval      TAMprg = (TAMprg + 1)
013300031105     C                   endif
013400031105     C                   endif
013500080811      * se flagD *off significa che non ho progressivi tariffa precedenti oppure quelli
013600080811      * che ho non devono essere aggiornati
013700080811      * devo verificare se per la tariffa che sto leggendo
013800080811      * devo aggiornare la tariffa particolare 'f '
013900080811     c                   If        §flagA = *on or
014000080811     c                             §flagD = *off
014100080808     c                   exsr      SR_cercaFuel
014200080808     c                   endif
014300080808      * se ho incrementato/aumentato scrivo  aggiorno TNTAM anche se cambiato progressivo
014400080808     c                   If        §flagD = *on or §flagA = *on
014500031105     C                   eval      TAMduv = §oggi
014600031105     C                   eval      TAMftr = *blanks
014700041116     C                   eval      TAMdtr = §oggi
014800031105     C                   update    tntam04
014900031105     C                   endif
015000051021     c                   endif
015100031105
015200080808      * Scadenza maggiore/uguale 01/10/2008 e decorrenza minore 01/10/2008
015300080808     C                   if        TAMdst >= 20081001 and TAMddt < 20081001
015400080808      * verifico se devo aggiornare la tariffa  altrimenti
015500051021      * non faccio nulla
015600080808      * verifico se esiste la tariffa particolare fuel
015700080808     c     ktpd2         setll     titpd02l
015800080808     c                   if        not %equal(titpd02l)
015900080808     c                   eval      §flagB = *on
016000080808     c                   eval      §fdp = 0
016100080808     c                   endif
016200080808
016300080808     c                   dow       §flagB = *off
016400080808     c     ktpd2         reade     titpd02l
016500080808
016600080808     c                   If        %eof(titpd02l)
016700080808     c                   leave
016800080808     c                   endif
016900080808
017000080808     c                   if        (not %eof(titpd02l)  and
017100080808     c                             old_tpditr < 0,3)
017200080808     c                   eval      §flagB = *on
017300080808     c                   eval      §fdp = old_tpditr
017400080808     c                   endif
017500080808
017600080808     c                   enddo
017700051103      *
017800080808     c                   If        §flagb = *on
017900080808     C                   eval      §dst = TAMdst
018000080808     C                   eval      kprg = TAMprg
018100031105     C                   exsr      Sr_Annulla
018200031105     C                   exsr      Sr_Scrivi
018300080808     C                   eval      TAMdst = 20080930
018400031105     C                   eval      TAMduv = §oggi
018500031105     C                   eval      TAMftr = *blanks
018600041116     C                   eval      TAMdtr = §oggi
018700031105     C                   update    tntam04
018800031105     C                   iter
018900031105     C                   endif
019000051021     C                   endif
019100031105
019200080808      * Scadenza minore/uguale 30/09/2008
019300080808     C                   if        TAMdst <= 20080930
019400051021      * verifico se devo aggiornare la tariffa  o per integrazione o per aumento altrimenti
019500051021      * non faccio nulla
019600080808      * verifico se esiste la tariffa particolare fuel
019700080808     c     ktpd2         setll     titpd02l
019800080808     c                   if        not %equal(titpd02l)
019900080808     c                   eval      §flagC = *on
020000080808     c                   eval      §fdp = 0
020100080808     c                   endif
020200080808
020300080808     c                   dow       §flagC = *off
020400080808     c     ktpd2         reade     titpd02l
020500080808
020600080808     c                   If        %eof(titpd02l)
020700080808     c                   leave
020800080808     c                   endif
020900080808
021000080808     c                   if        (not %eof(titpd02l)  and
021100080808     c                             old_tpditr < 0,3)
021200080808     c                   eval      §flagC = *on
021300080808     c                   eval      §fdp = old_tpditr
021400080808     c                   endif
021500080808
021600080808     c                   enddo
021700051103      *
021800080808     c                   If        §flagC = *on
021900071030     C                   eval      §dst = 20081231
022000031105     C                   eval      kprg = TAMprg
022100031105     C                   exsr      Sr_Annulla
022200031105     C                   exsr      Sr_Scrivi
022300031105     C                   endif
022400051021     C                   endif
022500051021
022600031105     C                   enddo
022700051021
022800080808     C                   SETON                                        LR
022900031105
023000031105      *****************************************************************
023100080811      * CERCA LE TARIFFE SUCESSIVE E CONTROLLA SE SONO A CAVALLO
023200031105      *****************************************************************
023300031105     C     Sr_CercaDopo  BEGSR
023400031105
023500080811     C                   eval      kprg = (TAMprg - 1)
023600080811     c                   dow       §flagD = *off
023700031105     C     ktam          chain(n)  tntam01l
023800031105     C                   if        %found(tntam01l)
023900080807     C                             and old_TAMdst >= 20081001
024000080807     C                             and old_TAMddt < 20081001
024100080811      * verifico se devo aggiornare la tariffa  altrimenti
024200080811      * non faccio nulla
024300080811      * verifico se esiste la tariffa particolare fuel
024400080811     c     ktpd2old      setll     titpd02l
024500080811     c                   if        not %equal(titpd02l)
024600080811     c                   eval      §flagD = *on
024700080811     c                   endif
024800080811     c                   dow       §flagD = *off
024900080811     c     ktpd2old      reade     titpd02l
025000080811
025100080811     c                   If        %eof(titpd02l)
025200080811     c                   leave
025300080811     c                   endif
025400080811
025500080811     c                   if        (not %eof(titpd02l)  and
025600080811     c                             old_tpditr < 0,3)
025700080811     c                   eval      §flagD = *on
025800080811     c                   endif
025900080811
026000080811     c                   enddo
026100080808     c                   endif
026200080811     c                   if        not %found(tntam01l)
026300080811     c                   leave
026400080811     c                   endif
026500080811     C                   eval      kprg = (old_TAMprg - 1)
026600080811     c                   enddo
026700031105
026800031105     C                   endsr
026900031105      *****************************************************************
027000031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
027100031105      *****************************************************************
027200031105     C     Sr_Aggiorna   BEGSR
027300031105
027400080807
027500080807     c                   eval      §Fuel_tpt = *off
027600080807
027700031105     C     ktam          setll     titad04l
027800031105     C                   do        *hival
027900031105     C     ktam          reade     titad04l
028000031105     C                   if        %eof(titad04l)
028100031105     C                   leave
028200031105     C                   endif
028300031105     C                   if        TADatb <> *blanks
028400031105     C                   iter
028500031105     C                   endif
028600080811     C                   eval      TADprg = TADprg + 1
028700031105     C                   eval      TADftr = *blanks
028800041116     C                   eval      TADdtr = §oggi
028900031105     C                   update    titad04
029000031105     C                   enddo
029100031105
029200031105     C     ktam          setll     titpt01l
029300031105     C                   do        *hival
029400031105     C     ktam          reade     titpt01l
029500031105     C                   if        %eof(titpt01l)
029600031105     C                   leave
029700031105     C                   endif
029800031105     C                   if        TPTatb <> *blanks
029900031105     C                   iter
030000031105     C                   endif
030100080811     C                   eval      TPTprg = TPTprg + 1
030200031105     C                   eval      TPTduv = §oggi
030300031105     C                   eval      TPTftr = *blanks
030400061128     C                   eval      TPTdtr = §oggi
030500080807
030600080807     c                   If        TPTftc = 'f '
030700080807     c                   eval      §Fuel_tpt = *on
030800080807     c                   endif
030900080807
031000031105     C                   update    titpt01
031100031105     C                   enddo
031200031105
031300031105     C     ktam          setll     titpd01l
031400031105     C                   do        *hival
031500031105     C     ktam          reade     titpd01l
031600031105     C                   if        %eof(titpd01l)
031700031105     C                   leave
031800031105     C                   endif
031900031105     C                   if        TPDatb <> *blanks
032000031105     C                   iter
032100031105     C                   endif
032200080811     C                   eval      TPDprg = TPDprg + 1
032300031105     C                   eval      TPDftr = *blanks
032400041116     C                   eval      TPDdtr = §oggi
032500080904      * se trattasi di tariffa particolare fuel verifico :
032600080905      *  A) se importo F.D. uguale a zero oppure F.D. minore di 0,3 e tipo aumento = B (cliente C)
032700080904      *     porto importo F.D. = a 0,3 ed aggiorno la data del prezzo base per F.D. = a 0 con
032800080905      *     la data presa in tabella nel file titpt
032900080905      *  B) se importo F.D. > 0 e < 0,3 e tipo aumento = C (clienti ABDT) imposto lo scaglione =1,53
033000080904      *     con F.D. trovato e aggiungo lo scaglione 999999 con F.D. = 0,3
033100080808      * se devo aggiornare il fuel
033200080905     c                   if        TPDftc = 'f '  and (TPDitr = 0  or
033300080905     c                             (wfatip = 'B' and tpditr < 0,3))
033400080808
033500080808      * aggiorno wfuel
033600080808     C                   eval      §prg = TPDprg
033700080808     C                   eval      §sgl = TPDsgl
033800080808     C                   eval      §fdp = TPDitr
033900080905     C                   eval      §fdn = 0,3
034000080905     C                   if        TPDitr = 0
034100080905     C                   eval      WFUflg = 'A'
034200080905      * aggiorno titpt con data prezzo base preso da tabella
034300080905     C     ktpt          chain     titpt01l
034400080905     C                   if        %found(titpt01l)
034500080905     C                   eval      WFUpdb = tptdpb
034600080905     C                   eval      tptdpb = data_base
034700080905     C                   eval      WFUndb = tptdpb
034800080905     c                   update    titpt01
034900080905     c                   endif
035000080905
035100080905     C                   else
035200080905     C                   eval      WFUflg = 'I'
035300080905     C                   clear                   WFUpdb
035400080905     C                   clear                   WFUndb
035500080905     C                   endif
035600080808     C                   exsr      Sr_ScriviWfue
035700080808
035800080808     c                   eval      TPDitr = 0,3
035900080808
036000080808     c                   endif
036100080905      * tipo aumento "C" aumento scaglionato
036200080905     c                   if        TPDftc = 'f '  and TPDitr > 0  and
036300080905     c                             tpditr < 0,3   and Wfatip = 'C'
036400080905      * scrivo un nuovo scaglione 1,53 (come scritto in tabella) con FD attuale
036500080905     C                   eval      new_TPDksc = TPDksc
036600080905     C                   eval      new_TPDctr = TPDctr
036700080905     C                   eval      new_TPDprg = TPDprg
036800080905     C                   eval      new_TPDftc = TPDftc
036900080905     C                   eval      new_TPDcts = TPDcts
037000080905     C                   eval      new_TPDorp = TPDorp
037100080905     C                   eval      new_TPDsgl = sgl_base
037200080905     C                   eval      new_TPDitr = TPDitr
037300080905     C                   eval      new_TPDmin = TPDmin
037400080905     C                   eval      new_TPDmax = TPDmax
037500080905     C                   eval      new_TPDain = TPDain
037600080905     C                   eval      new_TPDftr = *blanks
037700080905     C                   eval      new_TPDdtr = §oggi
037800080905     C                   write     titpd000
037900080905
038000080905      * aggiorno wfuel
038100080905     C                   eval      §prg = TPDprg
038200080919     C                   eval      §sgl = NEW_TPDSGL
038300080905     C                   eval      §fdp = 0
038400080905     C                   eval      §fdn = TPDitr
038500080905     C                   eval      WFUflg = 'A'
038600080905     C                   clear                   WFUpdb
038700080905     C                   clear                   WFUndb
038800080905     C                   exsr      Sr_ScriviWfue
038900080905      * aggiorno lo scaglione attuale con FD = 0,3
039000080905
039100080905      *
039200080905     C                   eval      §prg = TPDprg
039300080905     C                   eval      §sgl = TPDsgl
039400080905     C                   eval      §fdp = TPDitr
039500080905     C                   eval      §fdn = 0,3
039600080905     C                   eval      WFUflg = 'I'
039700080905     C                   clear                   WFUpdb
039800080905     C                   clear                   WFUndb
039900080905     C                   exsr      Sr_ScriviWfue
040000080905
040100080905     c                   eval      TPDitr = 0,3
040200080905     c                   endif
040300080905
040400080905     C                   update    titpd01
040500031105     C                   enddo
040600031105
040700031105     C     ktam          setll     titgc01l
040800031105     C                   do        *hival
040900031105     C     ktam          reade     titgc01l
041000031105     C                   if        %eof(titgc01l)
041100031105     C                   leave
041200031105     C                   endif
041300031105     C                   if        TGCatb <> *blanks
041400031105     C                   iter
041500031105     C                   endif
041600080811     C                   eval      TGCprg = TGCprg + 1
041700031105     C                   eval      TGCduv = §oggi
041800031105     C                   eval      TGCftr = *blanks
041900041116     C                   eval      TGCdtr = §oggi
042000031105     C                   update    titgc01
042100031105     C                   enddo
042200080808
042300080808      * se devo scrivere il record del fuel
042400080808     c                   If        §Fuel_tpt = *off
042500080811     c                   eval      §prg = (Kprg + 1)
042600080808     c                   exsr      sr_fuel
042700080808     c                   endif
042800031105
042900031105     C                   endsr
043000080808      *****************************************************************
043100080808      * CERCA LA TARIFFA PARTICOLARE FUEL
043200080808      *****************************************************************
043300080808     C     Sr_CercaFUEL  BEGSR
043400080808
043500080808     c                   eval      §flagA = *off
043600080808      * verifico se esiste la tariffa particolare fuel
043700080808     c     ktpd2         setll     titpd02l
043800080808      * se non trovato scrivo altrimenti verifico
043900080808     c                   If         not %equal(titpd02l)
044000080808     c                   eval      §flagA = *on
044100080808     c                   eval      §prg = tamprg
044200080808     c                   exsr      sr_Fuel
044300080808     c                   endif
044400080808
044500080808     c                   dow       §flagA = *off
044600080808     c     ktpd2         reade     titpd02l
044700080808
044800080808     c                   If        %eof(titpd02l)
044900080808     c                   leave
045000080808     c                   endif
045100080808
045200080808     c                   if        (not %eof(titpd02l)  and
045300080808     c                             old_tpditr < 0,3)
045400080808     c                   eval      §flagA = *on
045500080808     c                   exsr      sr_aggfuel
045600080808     c                   endif
045700080905
045800080808
045900080808     c                   enddo
046000080808     C                   ENDSR
046100080808      *****************************************************************
046200080808      * AGGIORNA LA TARIFFA PARTICOLARE FUEL
046300080808      *****************************************************************
046400080808     C     Sr_AggFuel    BEGSR
046500080808
046600080808     C     ktpd2         chain     titpt01l
046700080808     C                   if        TPTatb = *blanks
046800080808     C                   eval      TPTduv = §oggi
046900080808     C                   eval      TPTftr = *blanks
047000080808     C                   eval      TPTdtr = §oggi
047100080905      * se il F.D. è uguale a zero recupero la data prezzo base
047200080905     c                   if        old_tpditr = 0
047300080905     C                   eval      WFUpdb = tptdpb
047400080905     C                   eval      tptdpb = data_base
047500080905     C                   eval      WFUndb = tptdpb
047600080905     C                   eval      §prg = TPDprg
047700080905     C                   eval      §sgl = TPDsgl
047800080905     C                   eval      §fdp = TPDitr
047900080905     C                   eval      §fdn = 0,3
048000080905     C                   eval      WFUflg = 'A'
048100080905     c                   endif
048200080808     C                   update    titpt01
048300080808     C                   endif
048400080808
048500080808     C     ktpd2         setll     titpd01l
048600080808     C                   do        *hival
048700080808     C     ktpd2         reade     titpd01l
048800080808     C                   if        %eof(titpd01l)
048900080808     C                   leave
049000080808     C                   endif
049100080808     C                   if        TPDatb <> *blanks
049200080808     C                   iter
049300080808     C                   endif
049400080808     C                   eval      TPDftr = *blanks
049500080808     C                   eval      TPDdtr = §oggi
049600080808      * se devo aggiornare il fuel
049700080905     c                   if        (TPDitr < 0,3  and TPDitr > 0
049800080905      * e tipo aumento uguale a B imposto tutto a 0,3
049900080905     c                             and wfatip = 'B' ) or
050000080905      * F.D. = a zero
050100080905     c                              TPDitr = 0
050200080808
050300080808      * aggiorno wfuel
050400080808
050500080808     C                   eval      §prg = TPDprg
050600080808     C                   eval      §sgl = TPDsgl
050700080808     C                   eval      §fdp = TPDitr
050800080909     C                   eval      §fdn = 0,3
050900080905     c                   if        TPDitr > 0
051000080808     C                   eval      WFUflg = 'I'
051100080905     c                   endif
051200080808     C                   exsr      Sr_ScriviWfue
051300080808
051400080808     c                   eval      TPDitr = 0,3
051500080905     c                   endif
051600080905
051700080905      * se tipo aumento uguale a C imposto un nuovo scaglione
051800080905
051900080905     c                   if        TPDitr < 0,3  and TPDitr > 0
052000080905     c                             and wfatip = 'C'
052100080905      * scrivo un nuovo scaglione 1,53 (come scritto in tabella) con FD attuale
052200080905     c                   clear                   titpd000
052300080905     C                   eval      new_TPDksc = TPDksc
052400080905     C                   eval      new_TPDctr = TPDctr
052500080905     C                   eval      new_TPDprg = TPDprg
052600080905     C                   eval      new_TPDftc = TPDftc
052700080905     C                   eval      new_TPDcts = TPDcts
052800080905     C                   eval      new_TPDorp = TPDorp
052900080905     C                   eval      new_TPDsgl = sgl_base
053000080905     C                   eval      new_TPDitr = TPDitr
053100080905     C                   eval      new_TPDmin = TPDmin
053200080905     C                   eval      new_TPDmax = TPDmax
053300080905     C                   eval      new_TPDain = TPDain
053400080905     C                   eval      new_TPDftr = *blanks
053500080905     C                   eval      new_TPDdtr = §oggi
053600080905     C                   write     titpd000
053700080905
053800080905      * aggiorno wfuel
053900080905     C                   eval      §prg = TPDprg
054000080919     C                   eval      §sgl = NEW_TPDSGL
054100080905     C                   eval      §fdp = 0
054200080905     C                   eval      §fdn = TPDitr
054300080905     C                   eval      WFUflg = 'A'
054400080905     C                   clear                   WFUpdb
054500080905     C                   clear                   WFUndb
054600080905     C                   exsr      Sr_ScriviWfue
054700080905      * aggiorno lo scaglione attuale con FD = 0,3
054800080905
054900080905      *
055000080905     C                   eval      §prg = TPDprg
055100080905     C                   eval      §sgl = TPDsgl
055200080905     C                   eval      §fdp = TPDitr
055300080905     C                   eval      §fdn = 0,3
055400080905     C                   eval      WFUflg = 'I'
055500080905     C                   clear                   WFUpdb
055600080905     C                   clear                   WFUndb
055700080905     C                   exsr      Sr_ScriviWfue
055800080905
055900080905     c                   eval      TPDitr = 0,3
056000080905     c                   endif
056100080905
056200080808
056300080808     c                   update    titpd01
056400080808
056500080808     C                   enddo
056600080808
056700080808
056800080808     C                   ENDSR
056900031105      *****************************************************************
057000031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
057100031105      *****************************************************************
057200031105     C     Sr_Annulla    BEGSR
057300031105
057400031105     C                   eval      kprg = (kprg + 1)
057500031105
057600031105     C     ktam          setll     titad04l
057700031105     C                   do        *hival
057800031105     C     ktam          reade     titad04l
057900031105     C                   if        %eof(titad04l)
058000031105     C                   leave
058100031105     C                   endif
058200031105     C                   if        TADatb = *blanks
058300031105     C                   iter
058400031105     C                   endif
058500031105     C                   delete    titad04
058600031105     C                   enddo
058700031105
058800031105     C     ktam          setll     titpt01l
058900031105     C                   do        *hival
059000031105     C     ktam          reade     titpt01l
059100031105     C                   if        %eof(titpt01l)
059200031105     C                   leave
059300031105     C                   endif
059400031105     C                   if        TPTatb = *blanks
059500031105     C                   iter
059600031105     C                   endif
059700031105     C                   delete    titpt01
059800031105     C                   enddo
059900031105
060000031105     C     ktam          setll     titpd01l
060100031105     C                   do        *hival
060200031105     C     ktam          reade     titpd01l
060300031105     C                   if        %eof(titpd01l)
060400031105     C                   leave
060500031105     C                   endif
060600031105     C                   if        TPDatb = *blanks
060700031105     C                   iter
060800031105     C                   endif
060900031105     C                   delete    titpd01
061000031105     C                   enddo
061100031105
061200031105     C     ktam          setll     titgc01l
061300031105     C                   do        *hival
061400031105     C     ktam          reade     titgc01l
061500031105     C                   if        %eof(titgc01l)
061600031105     C                   leave
061700031105     C                   endif
061800031105     C                   if        TGCatb = *blanks
061900031105     C                   iter
062000031105     C                   endif
062100031105     C                   delete    titgc01
062200031105     C                   enddo
062300031105
062400031105     C     ktam          chain     tntam01l
062500031105     C                   if        %found(tntam01l)
062600031105     C                             and old_TAMatb <> *blanks
062700031105     C                   delete    tntam01
062800031105     C                   endif
062900031105
063000031105     C                   eval      kprg = (kprg - 1)
063100031105
063200031105     C                   endsr
063300031105      *****************************************************************
063400031105      * SCRIVE LE NUOVE TARIFFE
063500031105      *****************************************************************
063600031105     C     Sr_Scrivi     BEGSR
063700031105
063800080808
063900080808     c                   eval      §Fuel_tpt = *off
064000080808
064100031105     C     ktam          setll     titad04l
064200031105     C                   do        *hival
064300031105     C     ktam          reade     titad04l
064400031105     C                   if        %eof(titad04l)
064500031105     C                   leave
064600031105     C                   endif
064700031105     C                   if        TADatb <> *blanks
064800031105     C                   iter
064900031105     C                   endif
065000031105     C                   eval      new_TADksc = TADksc
065100031105     C                   eval      new_TADctr = TADctr
065200031105     C                   eval      new_TADprg = (kprg + 1)
065300031105     C                   eval      new_TADlnp = TADlnp
065400031105     C                   eval      new_TADcts = TADcts
065500031105     C                   eval      new_TADnaz = TADnaz
065600031105     C                   eval      new_TADcap = TADcap
065700031105     C                   eval      new_TADsgl = TADsgl
065800031105     C                   eval      new_TADitr = TADitr
065900031105     C                   eval      new_TADino = TADino
066000031105     C                   eval      new_TADrpv = TADrpv
066100031105     C                   eval      new_TADorp = TADorp
066200031105     C                   eval      new_TADmin = TADmin
066300031105     C                   eval      new_TADmax = TADmax
066400031105     C                   eval      new_TADftr = *blanks
066500041116     C                   eval      new_TADdtr = §oggi
066600031105     C                   write     titad000
066700031105     C                   enddo
066800031105
066900031105     C     ktam          setll     titpt01l
067000031105     C                   do        *hival
067100031105     C     ktam          reade     titpt01l
067200031105     C                   if        %eof(titpt01l)
067300031105     C                   leave
067400031105     C                   endif
067500031105     C                   if        TPTatb <> *blanks
067600031105     C                   iter
067700031105     C                   endif
067800031105     C                   eval      new_TPTksc = TPTksc
067900031105     C                   eval      new_TPTctr = TPTctr
068000031105     C                   eval      new_TPTprg = (kprg + 1)
068100031105     C                   eval      new_TPTduv = §oggi
068200031105     C                   eval      new_TPTftc = TPTftc
068300031105     C                   eval      new_TPTtpg = TPTtpg
068400031105     C                   eval      new_TPTarl = TPTarl
068500031105     C                   eval      new_TPTarf = TPTarf
068600031105     C                   eval      new_TPTaro = TPTaro
068700031105     C                   eval      new_TPTrpv = TPTrpv
068800031105     C                   eval      new_TPTvlm = TPTvlm
068900031105     C                   eval      new_TPTvvm = TPTvvm
069000031105     C                   eval      new_TPTfvm = TPTfvm
069100031105     C                   eval      new_TPTtma = TPTtma
069200031105     C                   eval      new_TPTapl = TPTapl
069300031105     C                   eval      new_TPTftr = *blanks
069400041116     C                   eval      new_TPTdtr = §oggi
069500061128     C                   eval      new_TPTdpb = TPTdpb
069600061128     C                   eval      new_TPTdit = TPTdit
069700061128     C                   eval      new_TPTflo = TPTflo
069800080808
069900080808     c                   If        TPTftc = 'f '
070000080808     c                   eval      §Fuel_tpt = *on
070100080808     c                   endif
070200080808
070300031105     C                   write     titpt000
070400031105     C                   enddo
070500031105
070600031105     C     ktam          setll     titpd01l
070700031105     C                   do        *hival
070800031105     C     ktam          reade     titpd01l
070900031105     C                   if        %eof(titpd01l)
071000031105     C                   leave
071100031105     C                   endif
071200031105     C                   if        TPDatb <> *blanks
071300031105     C                   iter
071400031105     C                   endif
071500031105     C                   eval      new_TPDksc = TPDksc
071600031105     C                   eval      new_TPDctr = TPDctr
071700031105     C                   eval      new_TPDprg = (kprg + 1)
071800031105     C                   eval      new_TPDftc = TPDftc
071900031105     C                   eval      new_TPDcts = TPDcts
072000031105     C                   eval      new_TPDorp = TPDorp
072100031105     C                   eval      new_TPDsgl = TPDsgl
072200031105     C                   eval      new_TPDitr = TPDitr
072300031105     C                   eval      new_TPDmin = TPDmin
072400031105     C                   eval      new_TPDmax = TPDmax
072500031105     C                   eval      new_TPDain = TPDain
072600031105     C                   eval      new_TPDftr = *blanks
072700041116     C                   eval      new_TPDdtr = §oggi
072800080905      * se trattasi di tariffa particolare fuel verifico :
072900080905      *  A) se importo F.D. uguale a zero oppure F.D. minore di 0,3 e tipo aumento = B (cliente C)
073000080905      *     porto importo F.D. = a 0,3 ed aggiorno la data del prezzo base per F.D. = a 0 con
073100080905      *     la data presa in tabella nel file titpt
073200080905      *  B) se importo F.D. > 0 e < 0,3 e tipo aumento = C (clienti ABDT) imposto lo scaglione =1,53
073300080905      *     con F.D. trovato e aggiungo lo scaglione 999999 con F.D. = 0,3
073400080905      * se devo aggiornare il fuel
073500080905     c                   if        TPDftc = 'f '  and (TPDitr = 0  or
073600080918     c                             (wfatip = 'B' and tpditr < 0,3) or
073700080918     c                             (wfatip = 'A' and tpditr < 0,3) )
073800080905
073900080905      * aggiorno wfuel
074000080918     C                   eval      §prg = New_TPDprg
074100080905     C                   eval      §sgl = TPDsgl
074200080905     C                   eval      §fdp = TPDitr
074300080905     C                   eval      §fdn = 0,3
074400080905     C                   if        TPDitr = 0
074500080905     C                   eval      WFUflg = 'A'
074600080905      * aggiorno titpt con data prezzo base preso da tabella
074700080905     C     ktptnew       chain     titpt01l
074800080905     C                   if        %found(titpt01l)
074900080905     C                   eval      WFUpdb = tptdpb
075000080905     C                   eval      tptdpb = data_base
075100080905     C                   eval      WFUndb = tptdpb
075200080905     c                   update    titpt01
075300080905     c                   endif
075400080905
075500080905     C                   else
075600080905     C                   eval      WFUflg = 'I'
075700080905     C                   clear                   WFUpdb
075800080905     C                   clear                   WFUndb
075900080905     C                   endif
076000080905     C                   exsr      Sr_ScriviWfue
076100080905
076200080905     c                   eval      new_TPDitr = 0,3
076300080905
076400080905     c                   endif
076500080905      * tipo aumento "C" aumento scaglionato
076600080905     c                   if        TPDftc = 'f '  and TPDitr > 0  and
076700080905     c                             tpditr < 0,3   and Wfatip = 'C'
076800080905      * scrivo un nuovo scaglione 1,53 (come scritto in tabella) con FD attuale
076900080905     C                   eval      new_TPDsgl = sgl_base
077000080905     C                   write     titpd000
077100080905
077200080905      * aggiorno wfuel
077300080918     C                   eval      §prg = New_TPDprg
077400080908     C                   eval      §sgl = sgl_base
077500080905     C                   eval      §fdp = 0
077600080905     C                   eval      §fdn = TPDitr
077700080905     C                   eval      WFUflg = 'A'
077800080905     C                   clear                   WFUpdb
077900080905     C                   clear                   WFUndb
078000080905     C                   exsr      Sr_ScriviWfue
078100080905      * aggiorno lo scaglione attuale con FD = 0,3
078200080905
078300080905      *
078400080918     C                   eval      §prg = New_TPDprg
078500080905     C                   eval      §sgl = TPDsgl
078600080905     C                   eval      §fdp = TPDitr
078700080905     C                   eval      §fdn = 0,3
078800080905     C                   eval      WFUflg = 'I'
078900080905     C                   clear                   WFUpdb
079000080905     C                   clear                   WFUndb
079100080905     C                   exsr      Sr_ScriviWfue
079200080905
079300080905     c                   eval      new_TPDsgl = tpdsgl
079400080905     c                   eval      new_TPDitr = 0,3
079500080905     c                   endif
079600080905
079700031105     C                   write     titpd000
079800031105     C                   enddo
079900031105
080000031105     C     ktam          setll     titgc01l
080100031105     C                   do        *hival
080200031105     C     ktam          reade     titgc01l
080300031105     C                   if        %eof(titgc01l)
080400031105     C                   leave
080500031105     C                   endif
080600031105     C                   if        TGCatb <> *blanks
080700031105     C                   iter
080800031105     C                   endif
080900031105     C                   eval      new_TGCksc = TGCksc
081000031105     C                   eval      new_TGCctr = TGCctr
081100031105     C                   eval      new_TGCprg = (kprg + 1)
081200031105     C                   eval      new_TGCsgv = TGCsgv
081300031105     C                   eval      new_TGCsgr = TGCsgr
081400031105     C                   eval      new_TGCsgp = TGCsgp
081500031105     C                   eval      new_TGCsgd = TGCsgd
081600031105     C                   eval      new_TGCsg1 = TGCsg1
081700031105     C                   eval      new_TGCsg2 = TGCsg2
081800031105     C                   eval      new_TGCsg3 = TGCsg3
081900031105     C                   eval      new_TGCst1 = TGCst1
082000031105     C                   eval      new_TGCst2 = TGCst2
082100031105     C                   eval      new_TGCst3 = TGCst3
082200031105     C                   eval      new_TGCstm = TGCstm
082300031105     C                   eval      new_TGCrpv = TGCrpv
082400031105     C                   eval      new_TGCfaf = TGCfaf
082500031105     C                   eval      new_TGCsgf = TGCsgf
082600031105     C                   eval      new_TGCggr = TGCggr
082700031105     C                   eval      new_TGCtcm = TGCtcm
082800031105     C                   eval      new_TGCtfg = TGCtfg
082900031105     C                   eval      new_TGCduv = §oggi
083000031105     C                   eval      new_TGCftr = *blanks
083100041116     C                   eval      new_TGCdtr = §oggi
083200031105     C                   write     titgc000
083300031105     C                   enddo
083400031105
083500031105     C                   eval      new_TAMksc = TAMksc
083600031105     C                   eval      new_TAMctr = TAMctr
083700031105     C                   eval      new_TAMprg = (kprg + 1)
083800080808     C                   eval      new_TAMddt = 20081001
083900031105     C                   eval      new_TAMdst = §dst
084000031105     C                   eval      new_TAMduv = §oggi
084100031105     C                   eval      new_TAMdcv = TAMdcv
084200031105     C                   eval      new_TAMdif = TAMdif
084300031105     C                   eval      new_TAMfli = TAMfli
084400031105     C                   eval      new_TAMpri = TAMpri
084500031105     C                   eval      new_TAMfmp = TAMfmp
084600031105     C                   eval      new_TAMlrc = TAMlrc
084700031105     C                   eval      new_TAMlas = TAMlas
084800080808     c                   eval      new_TAMppa = TAMppa
084900051104     c                   eval      new_TAMpag = TAMpag
085000031105     C                   eval      new_TAMmpa = TAMmpa
085100031105     C                   eval      new_TAMpam = TAMpam
085200031105     C                   eval      new_TAMmpm = TAMmpm
085300031105     C                   eval      new_TAMtin = TAMtin
085400031105     C                   eval      new_TAMkpm = TAMkpm
085500031105     C                   eval      new_TAMarl = TAMarl
085600031105     C                   eval      new_TAMarf = TAMarf
085700031105     C                   eval      new_TAMaro = TAMaro
085800031105     C                   eval      new_TAMrat = TAMrat
085900031105     C                   eval      new_TAMrct = TAMrct
086000031105     C                   eval      new_TAMsc2 = TAMsc2
086100031105     C                   eval      new_TAMeds = TAMeds
086200031105     C                   eval      new_TAMars = TAMars
086300031105     C                   eval      new_TAMata = TAMata
086400031105     C                   eval      new_TAMftp = TAMftp
086500031105     C                   eval      new_TAMbap = TAMbap
086600031105     C                   eval      new_TAMtsp = TAMtsp
086700031105     C                   eval      new_TAMfie = TAMfie
086800031105     C                   eval      new_TAMfis = TAMfis
086900031105     C                   eval      new_TAMsis = TAMsis
087000031105     C                   eval      new_TAMfvd = TAMfvd
087100031105     C                   eval      new_TAMfts = TAMfts
087200031105     C                   eval      new_TAMnas = TAMnas
087300031105     C                   eval      new_TAMnot = TAMnot
087400031105     C                   eval      new_TAMftm = TAMftm
087500031105     C                   eval      new_TAMgca = TAMgca
087600031105     C                   eval      new_TAMgga = TAMgga
087700031105     C                   eval      new_TAMgma = TAMgma
087800031105     C                   eval      new_TAMgva = TAMgva
087900031105     C                   eval      new_TAMbam = TAMbam
088000031105     C                   eval      new_TAMtpr = TAMtpr
088100031105     C                   eval      new_TAMflo = TAMflo
088200031105     C                   eval      new_TAMdat = TAMdat
088300031105     C                   eval      new_TAMftr = *blanks
088400041116     C                   eval      new_TAMdtr = §oggi
088500031105     C                   write     tntam000
088600031105
088700080808      * se devo scrivere il record del fuel
088800080808     c                   If        §Fuel_tpt = *off
088900080811     c                   eval      §prg = (kprg + 1)
089000080808     c                   exsr      sr_fuel
089100080808     c                   endif
089200080808
089300031105     C                   endsr
089400080807      *****************************************************************
089500080807      * SCRIVE TARIFFA PARTICOLARE FUEL
089600080807      *****************************************************************
089700080807     C     Sr_Fuel       BEGSR
089800080807
089900080807     c
090000080807     C                   clear                   titpt000
090100080807     C                   eval      new_TPTksc = tamksc
090200080807     C                   eval      new_TPTctr = tamctr
090300080807     C                   eval      new_TPTprg = §prg
090400080807     C                   eval      new_TPTduv = §oggi
090500080807     C                   eval      new_TPTftc = 'f '
090600080807     C                   eval      new_TPTtpg = 'V'
090700080807     C                   eval      new_TPTftr = *blanks
090800080807     C                   eval      new_TPTdtr = §oggi
090900080814     C                   eval      new_TPTdpb = data_base
091000080807     C                   eval      new_TPTdit = §oggi
091100080807     C                   write     titpt000
091200080807     C                   clear                   titpd000
091300080807     C                   eval      new_TPDksc = tamksc
091400080807     C                   eval      new_TPDctr = tamctr
091500080807     C                   eval      new_TPDprg = §prg
091600080807     C                   eval      new_TPDftc = 'f '
091700080807     c                   if        tamfie = 'I'
091800080807     C                   eval      new_TPDcts = 'IT'
091900080807     C                   eval      new_TPDorp = '  A 9'
092000080807     c                   endif
092100080807     c                   if        tamfie = 'E'
092200080807     C                   eval      new_TPDcts = 'EE'
092300080807     C                   eval      new_TPDorp = '  999'
092400080807     c                   endif
092500080807     C                   eval      new_TPDsgl = 9999999999,000
092600080807     C                   eval      new_TPDitr = 0,3
092700080807     C                   eval      new_TPDftr = *blanks
092800080807     C                   eval      new_TPDdtr = §oggi
092900080807     C                   write     titpd000
093000080808
093100080811     C                   eval      §prg = new_TPDprg
093200080811     C                   eval      §sgl = new_TPDsgl
093300080908     C                   eval      §fdn = new_Tpditr
093400080808     C                   eval      §fdp = 0
093500080808     C                   eval      WFUflg = 'A'
093600080808     C                   exsr      Sr_ScriviWfue
093700080807
093800080807     c                   endsr
093900031105      *****************************************************************
094000080808      * SCRIVE IL FILE WFUEL00F
094100031105      *****************************************************************
094200080808     C     Sr_ScriviWfue BEGSR
094300031105
094400080808     C                   eval      WFUksc = TAMksc
094500080808     C                   eval      WFUctr = TAMctr
094600080808     C                   eval      WFUprg = §prg
094700080811     C                   eval      WFUsgl = §sgl
094800080808     C                   eval      WFUpfd = §fdp
094900080905     C                   eval      WFUnfd = §fdn
095000080808     C                   write     wfuel000
095100031105
095200031105     C                   endsr
095300031105      *****************************************************************
095400031105      * ROUTINE INIZIALE
095500031105      *****************************************************************
095600031105     C     *INZSR        BEGSR
095700031105
095800031105     C     *ENTRY        PLIST
095900031105     C                   PARM                    KPJBA
096000080806     c                   movel     kpjbu         param
096100031105      *
096200031105     c
096300031105     C     Ktam          klist
096400031105     C                   kfld                    TAMksc
096500031105     C                   kfld                    TAMctr
096600031105     C                   kfld                    Kprg
096700080806
096800080806     C     Ktamin        klist
096900080806     C                   kfld                    wfaclf
097000080806     C                   kfld                    wfactr
097100080807
097200080807     C     Ktpd          klist
097300080807     C                   kfld                    old_tamksc
097400080807     C                   kfld                    old_tamctr
097500080807     C                   kfld                    old_tamprg
097600080807     C                   kfld                    Kftc
097700031105
097800080808     C     Ktpd2         klist
097900080808     C                   kfld                    tamksc
098000080808     C                   kfld                    tamctr
098100080808     C                   kfld                    tamprg
098200080808     C                   kfld                    Kftc
098300080811
098400080811     C     Ktpd2old      klist
098500080811     C                   kfld                    old_tamksc
098600080811     C                   kfld                    old_tamctr
098700080811     C                   kfld                    old_tamprg
098800080811     C                   kfld                    Kftc
098900080905
099000080905     C     Ktpt          klist
099100080905     C                   kfld                    tpdksc
099200080905     C                   kfld                    tpdctr
099300080905     C                   kfld                    tpdprg
099400080905     C                   kfld                    tpdftc
099500080905
099600080905     C     Ktptnew       klist
099700080905     C                   kfld                    new_tpdksc
099800080905     C                   kfld                    new_tpdctr
099900080905     C                   kfld                    new_tpdprg
100000080905     C                   kfld                    new_tpdftc
100100080814
100200080814     c     keytab        klist
100300080814     c                   kfld                    codut
100400080814     c                   kfld                    kcod
100500080814     c                   kfld                    kkey
100600031105
100700031105      * reperisce data del giorno
100800031105     C                   TIME                    W0140
100900031105      * UDATE IN GGMMAAAA
101000031105     C                   MOVE      W0140         WDTGIO
101100031105      * UDATE IN AAAAMMGG
101200031105     C                   Z-ADD     WDTGIO        G02DAT
101300031105     C                   MOVEL     *BLANK        G02ERR
101400031105     C                   CALL      'XSRDA8'
101500031105     C                   PARM                    WLBDAT
101600031105     C                   Z-ADD     G02INV        §oggi
101700080814      * cerco la data prezzo base
101800080814     c                   move(p)   'FUEL'        kkey
101900080814     c                   movel(p)  '%A'          kcod
102000080814     c     Keytab        chain     tabel00f
102100080814     c                   If        %found(tabel00f)
102200080814     c                   movel     tbluni        data_base
102300080814     c                   endif
102400080904      * cerco lo scaglione precedente  il 999999
102500080904     c                   move(p)   'FUELSGL'     kkey
102600080904     c                   movel(p)  '%A'          kcod
102700080904     c     Keytab        chain     tabel00f
102800080904     c                   If        %found(tabel00f)
102900080904     c                   movel     tbluni        sgl_base
103000080904     c                   endif
103100031105
103200031105     C                   endsr
