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
003800080814     D Kcod            S                   like(TBLCOD)
003900080814     D Kkey            S                   like(TBLKEY)
004000080814     D Codut           s              1  0 inz(1)
004100080814
004200031105     D W0140           S             14  0
004300031105     D Wdtgio          S              8  0
004400080814     D data_base       S              8  0
004500031105     D §oggi           S              8  0
004600031105     D §flagA          S              1
004700031105     D §flagB          S              1
004800031105     D §flagC          S              1
004900031105     D §flagD          S              1
005000080807     D §fuel_tpt       S              1
005100031105
005200031105      *   D S   I N T E R N E / E S T E R N E
005300031105
005400031105     D WLBDAT          DS                  INZ
005500031105     D  G02DAT                 1      8  0
005600031105     D  G02INV                 9     16  0
005700031105     D  G02ERR                17     17
005800031105     D  G02TGI                18     22  0
005900031105
006000031105     D                 ds
006100031105     D tamksc                  1      7  0
006200031105     D  ksc4                   1      4  0
006300031105     D  ksc33                  1      3  0
006400031105     D  ksc3                   5      7  0
006500031105
006600080806     D PARAM           DS
006700080806     D  Wfaclf                 1      7  0
006800080806     D  Wfactr                 8     10  0
006900080806
007000061128     D Dsta01        e ds
007100031105     D kpjba         e ds
007200031105
007300031105      *---------------------------------------------------------------------------------------------
007400031105
007500031105      *
007600031105      * Aggancio Tntam04l
007700080806     c     ktamin        setll     tntam04l
007800031105     c                   do        *hival
007900080806     c     ktamin        reade     tntam04l
008000031105      *
008100031105      * fine file
008200031105     c                   if        %eof(tntam04l)
008300031105     c                   leave
008400031105     c                   endif
008500031105
008600031105      * Se stessa tariffa e si è verificata una delle condizioni
008700031105      * non deve elaborare più niente
008800031105     C                   if        (TAMksc = §ksc and TAMctr = §ctr
008900031105     C                             and §flagA = *on) or
009000031105     C                             (TAMksc = §ksc and TAMctr = §ctr
009100031105     C                             and §flagB = *on) or
009200031105     C                             (TAMksc = §ksc and TAMctr = §ctr
009300031105     C                             and §flagC = *on)
009400031105     C                   iter
009500031105     C                   endif
009600031105
009700031105      * Cambia tariffa
009800031105     C                   if        TAMksc <> §ksc
009900031105     C                   eval      §ksc = TAMksc
010000031105     C                   eval      §flagA = *off
010100031105     C                   eval      §flagB = *off
010200031105     C                   eval      §flagC = *off
010300080808     C                   eval      §flagD = *off
010400031105     C                   endif
010500031105     C                   if        TAMctr <> §ctr
010600031105     C                   eval      §ctr = TAMctr
010700031105     C                   eval      §flagA = *off
010800031105     C                   eval      §flagB = *off
010900031105     C                   eval      §flagC = *off
011000080808     C                   eval      §flagD = *off
011100031105     C                   endif
011200080811
011300031105
011400061128      * valorizzo i campi dei flag operativi
011500061128     c                   movel     tamflo        dsta01
011600061128
011700080806      * Decorrenza maggiore/uguale 01/10/2008
011800080806     C                   if        TAMddt >= 20081001
011900080811     C                   eval      §flagA = (TAMddt = 20081001)
012000031105     C                   eval      §flagD = *off
012100080806     C                   if        tamddt <> 20081001 and tamdst <> 20081001
012200031105     C                   exsr      Sr_CercaDopo
012300031105     C                   if        §flagD = *on
012400031105     C                   eval      kprg = TAMprg
012500080808     C                   eval      §prg = (TAMprg + 1)
012600080811     C                   exsr      Sr_Annulla
012700031105     C                   exsr      Sr_Aggiorna
012800031105     C                   eval      TAMprg = (TAMprg + 1)
012900031105     C                   endif
013000031105     C                   endif
013100080811      * se flagD *off significa che non ho progressivi tariffa precedenti oppure quelli
013200080811      * che ho non devono essere aggiornati
013300080811      * devo verificare se per la tariffa che sto leggendo
013400080811      * devo aggiornare la tariffa particolare 'f '
013500080811     c                   If        §flagA = *on or
013600080811     c                             §flagD = *off
013700080808     c                   exsr      SR_cercaFuel
013800080808     c                   endif
013900080808      * se ho incrementato/aumentato scrivo  aggiorno TNTAM anche se cambiato progressivo
014000080808     c                   If        §flagD = *on or §flagA = *on
014100031105     C                   eval      TAMduv = §oggi
014200031105     C                   eval      TAMftr = *blanks
014300041116     C                   eval      TAMdtr = §oggi
014400031105     C                   update    tntam04
014500031105     C                   endif
014600051021     c                   endif
014700031105
014800080808      * Scadenza maggiore/uguale 01/10/2008 e decorrenza minore 01/10/2008
014900080808     C                   if        TAMdst >= 20081001 and TAMddt < 20081001
015000080808      * verifico se devo aggiornare la tariffa  altrimenti
015100051021      * non faccio nulla
015200080808      * verifico se esiste la tariffa particolare fuel
015300080808     c     ktpd2         setll     titpd02l
015400080808     c                   if        not %equal(titpd02l)
015500080808     c                   eval      §flagB = *on
015600080808     c                   eval      §fdp = 0
015700080808     c                   endif
015800080808
015900080808     c                   dow       §flagB = *off
016000080808     c     ktpd2         reade     titpd02l
016100080808
016200080808     c                   If        %eof(titpd02l)
016300080808     c                   leave
016400080808     c                   endif
016500080808
016600080808     c                   if        (not %eof(titpd02l)  and
016700080808     c                             old_tpditr < 0,3)
016800080808     c                   eval      §flagB = *on
016900080808     c                   eval      §fdp = old_tpditr
017000080808     c                   endif
017100080808
017200080808     c                   enddo
017300051103      *
017400080808     c                   If        §flagb = *on
017500080808     C                   eval      §dst = TAMdst
017600080808     C                   eval      kprg = TAMprg
017700031105     C                   exsr      Sr_Annulla
017800031105     C                   exsr      Sr_Scrivi
017900080808     C                   eval      TAMdst = 20080930
018000031105     C                   eval      TAMduv = §oggi
018100031105     C                   eval      TAMftr = *blanks
018200041116     C                   eval      TAMdtr = §oggi
018300031105     C                   update    tntam04
018400031105     C                   iter
018500031105     C                   endif
018600051021     C                   endif
018700031105
018800080808      * Scadenza minore/uguale 30/09/2008
018900080808     C                   if        TAMdst <= 20080930
019000051021      * verifico se devo aggiornare la tariffa  o per integrazione o per aumento altrimenti
019100051021      * non faccio nulla
019200080808      * verifico se esiste la tariffa particolare fuel
019300080808     c     ktpd2         setll     titpd02l
019400080808     c                   if        not %equal(titpd02l)
019500080808     c                   eval      §flagC = *on
019600080808     c                   eval      §fdp = 0
019700080808     c                   endif
019800080808
019900080808     c                   dow       §flagC = *off
020000080808     c     ktpd2         reade     titpd02l
020100080808
020200080808     c                   If        %eof(titpd02l)
020300080808     c                   leave
020400080808     c                   endif
020500080808
020600080808     c                   if        (not %eof(titpd02l)  and
020700080808     c                             old_tpditr < 0,3)
020800080808     c                   eval      §flagC = *on
020900080808     c                   eval      §fdp = old_tpditr
021000080808     c                   endif
021100080808
021200080808     c                   enddo
021300051103      *
021400080808     c                   If        §flagC = *on
021500071030     C                   eval      §dst = 20081231
021600031105     C                   eval      kprg = TAMprg
021700031105     C                   exsr      Sr_Annulla
021800031105     C                   exsr      Sr_Scrivi
021900031105     C                   endif
022000051021     C                   endif
022100051021
022200031105     C                   enddo
022300051021
022400080808     C                   SETON                                        LR
022500031105
022600031105      *****************************************************************
022700080811      * CERCA LE TARIFFE SUCESSIVE E CONTROLLA SE SONO A CAVALLO
022800031105      *****************************************************************
022900031105     C     Sr_CercaDopo  BEGSR
023000031105
023100080811     C                   eval      kprg = (TAMprg - 1)
023200080811     c                   dow       §flagD = *off
023300031105     C     ktam          chain(n)  tntam01l
023400031105     C                   if        %found(tntam01l)
023500080807     C                             and old_TAMdst >= 20081001
023600080807     C                             and old_TAMddt < 20081001
023700080811      * verifico se devo aggiornare la tariffa  altrimenti
023800080811      * non faccio nulla
023900080811      * verifico se esiste la tariffa particolare fuel
024000080811     c     ktpd2old      setll     titpd02l
024100080811     c                   if        not %equal(titpd02l)
024200080811     c                   eval      §flagD = *on
024300080811     c                   endif
024400080811     c                   dow       §flagD = *off
024500080811     c     ktpd2old      reade     titpd02l
024600080811
024700080811     c                   If        %eof(titpd02l)
024800080811     c                   leave
024900080811     c                   endif
025000080811
025100080811     c                   if        (not %eof(titpd02l)  and
025200080811     c                             old_tpditr < 0,3)
025300080811     c                   eval      §flagD = *on
025400080811     c                   endif
025500080811
025600080811     c                   enddo
025700080808     c                   endif
025800080811     c                   if        not %found(tntam01l)
025900080811     c                   leave
026000080811     c                   endif
026100080811     C                   eval      kprg = (old_TAMprg - 1)
026200080811     c                   enddo
026300031105
026400031105     C                   endsr
026500031105      *****************************************************************
026600031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
026700031105      *****************************************************************
026800031105     C     Sr_Aggiorna   BEGSR
026900031105
027000080807
027100080807     c                   eval      §Fuel_tpt = *off
027200080807
027300031105     C     ktam          setll     titad04l
027400031105     C                   do        *hival
027500031105     C     ktam          reade     titad04l
027600031105     C                   if        %eof(titad04l)
027700031105     C                   leave
027800031105     C                   endif
027900031105     C                   if        TADatb <> *blanks
028000031105     C                   iter
028100031105     C                   endif
028200080811     C                   eval      TADprg = TADprg + 1
028300031105     C                   eval      TADftr = *blanks
028400041116     C                   eval      TADdtr = §oggi
028500031105     C                   update    titad04
028600031105     C                   enddo
028700031105
028800031105     C     ktam          setll     titpt01l
028900031105     C                   do        *hival
029000031105     C     ktam          reade     titpt01l
029100031105     C                   if        %eof(titpt01l)
029200031105     C                   leave
029300031105     C                   endif
029400031105     C                   if        TPTatb <> *blanks
029500031105     C                   iter
029600031105     C                   endif
029700080811     C                   eval      TPTprg = TPTprg + 1
029800031105     C                   eval      TPTduv = §oggi
029900031105     C                   eval      TPTftr = *blanks
030000061128     C                   eval      TPTdtr = §oggi
030100080807
030200080807     c                   If        TPTftc = 'f '
030300080807     c                   eval      §Fuel_tpt = *on
030400080807     c                   endif
030500080807
030600031105     C                   update    titpt01
030700031105     C                   enddo
030800031105
030900031105     C     ktam          setll     titpd01l
031000031105     C                   do        *hival
031100031105     C     ktam          reade     titpd01l
031200031105     C                   if        %eof(titpd01l)
031300031105     C                   leave
031400031105     C                   endif
031500031105     C                   if        TPDatb <> *blanks
031600031105     C                   iter
031700031105     C                   endif
031800080811     C                   eval      TPDprg = TPDprg + 1
031900031105     C                   eval      TPDftr = *blanks
032000041116     C                   eval      TPDdtr = §oggi
032100080808      * se devo aggiornare il fuel
032200080808     c                   if        TPDftc = 'f '  and TPDitr < 0,3
032300080808
032400080808      * aggiorno wfuel
032500080808     C                   eval      §prg = TPDprg
032600080808     C                   eval      §sgl = TPDsgl
032700080808     C                   eval      §fdp = TPDitr
032800080808     C                   eval      WFUflg = 'I'
032900080808     C                   exsr      Sr_ScriviWfue
033000080808
033100080808     c                   eval      TPDitr = 0,3
033200080808
033300080808     c                   endif
033400031105     C                   update    titpd01
033500031105     C                   enddo
033600031105
033700031105     C     ktam          setll     titgc01l
033800031105     C                   do        *hival
033900031105     C     ktam          reade     titgc01l
034000031105     C                   if        %eof(titgc01l)
034100031105     C                   leave
034200031105     C                   endif
034300031105     C                   if        TGCatb <> *blanks
034400031105     C                   iter
034500031105     C                   endif
034600080811     C                   eval      TGCprg = TGCprg + 1
034700031105     C                   eval      TGCduv = §oggi
034800031105     C                   eval      TGCftr = *blanks
034900041116     C                   eval      TGCdtr = §oggi
035000031105     C                   update    titgc01
035100031105     C                   enddo
035200080808
035300080808      * se devo scrivere il record del fuel
035400080808     c                   If        §Fuel_tpt = *off
035500080811     c                   eval      §prg = (Kprg + 1)
035600080808     c                   exsr      sr_fuel
035700080808     c                   endif
035800031105
035900031105     C                   endsr
036000080808      *****************************************************************
036100080808      * CERCA LA TARIFFA PARTICOLARE FUEL
036200080808      *****************************************************************
036300080808     C     Sr_CercaFUEL  BEGSR
036400080808
036500080808     c                   eval      §flagA = *off
036600080808      * verifico se esiste la tariffa particolare fuel
036700080808     c     ktpd2         setll     titpd02l
036800080808      * se non trovato scrivo altrimenti verifico
036900080808     c                   If         not %equal(titpd02l)
037000080808     c                   eval      §flagA = *on
037100080808     c                   eval      §prg = tamprg
037200080808     c                   exsr      sr_Fuel
037300080808     c                   endif
037400080808
037500080808     c                   dow       §flagA = *off
037600080808     c     ktpd2         reade     titpd02l
037700080808
037800080808     c                   If        %eof(titpd02l)
037900080808     c                   leave
038000080808     c                   endif
038100080808
038200080808     c                   if        (not %eof(titpd02l)  and
038300080808     c                             old_tpditr < 0,3)
038400080808     c                   eval      §flagA = *on
038500080808     c                   exsr      sr_aggfuel
038600080808     c                   endif
038700080808
038800080808     c                   enddo
038900080808     C                   ENDSR
039000080808      *****************************************************************
039100080808      * AGGIORNA LA TARIFFA PARTICOLARE FUEL
039200080808      *****************************************************************
039300080808     C     Sr_AggFuel    BEGSR
039400080808
039500080808     C     ktpd2         chain     titpt01l
039600080808     C                   if        TPTatb = *blanks
039700080808     C                   eval      TPTduv = §oggi
039800080808     C                   eval      TPTftr = *blanks
039900080808     C                   eval      TPTdtr = §oggi
040000080808     C                   update    titpt01
040100080808     C                   endif
040200080808
040300080808     C     ktpd2         setll     titpd01l
040400080808     C                   do        *hival
040500080808     C     ktpd2         reade     titpd01l
040600080808     C                   if        %eof(titpd01l)
040700080808     C                   leave
040800080808     C                   endif
040900080808     C                   if        TPDatb <> *blanks
041000080808     C                   iter
041100080808     C                   endif
041200080808     C                   eval      TPDftr = *blanks
041300080808     C                   eval      TPDdtr = §oggi
041400080808      * se devo aggiornare il fuel
041500080808     c                   if        TPDitr < 0,3
041600080808
041700080808      * aggiorno wfuel
041800080808
041900080808     C                   eval      §prg = TPDprg
042000080808     C                   eval      §sgl = TPDsgl
042100080808     C                   eval      §fdp = TPDitr
042200080808     C                   eval      WFUflg = 'I'
042300080808     C                   exsr      Sr_ScriviWfue
042400080808
042500080808     c                   eval      TPDitr = 0,3
042600080808
042700080808     c                   update    titpd01
042800080808     c                   endif
042900080808
043000080808     C                   enddo
043100080808
043200080808
043300080808     C                   ENDSR
043400031105      *****************************************************************
043500031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
043600031105      *****************************************************************
043700031105     C     Sr_Annulla    BEGSR
043800031105
043900031105     C                   eval      kprg = (kprg + 1)
044000031105
044100031105     C     ktam          setll     titad04l
044200031105     C                   do        *hival
044300031105     C     ktam          reade     titad04l
044400031105     C                   if        %eof(titad04l)
044500031105     C                   leave
044600031105     C                   endif
044700031105     C                   if        TADatb = *blanks
044800031105     C                   iter
044900031105     C                   endif
045000031105     C                   delete    titad04
045100031105     C                   enddo
045200031105
045300031105     C     ktam          setll     titpt01l
045400031105     C                   do        *hival
045500031105     C     ktam          reade     titpt01l
045600031105     C                   if        %eof(titpt01l)
045700031105     C                   leave
045800031105     C                   endif
045900031105     C                   if        TPTatb = *blanks
046000031105     C                   iter
046100031105     C                   endif
046200031105     C                   delete    titpt01
046300031105     C                   enddo
046400031105
046500031105     C     ktam          setll     titpd01l
046600031105     C                   do        *hival
046700031105     C     ktam          reade     titpd01l
046800031105     C                   if        %eof(titpd01l)
046900031105     C                   leave
047000031105     C                   endif
047100031105     C                   if        TPDatb = *blanks
047200031105     C                   iter
047300031105     C                   endif
047400031105     C                   delete    titpd01
047500031105     C                   enddo
047600031105
047700031105     C     ktam          setll     titgc01l
047800031105     C                   do        *hival
047900031105     C     ktam          reade     titgc01l
048000031105     C                   if        %eof(titgc01l)
048100031105     C                   leave
048200031105     C                   endif
048300031105     C                   if        TGCatb = *blanks
048400031105     C                   iter
048500031105     C                   endif
048600031105     C                   delete    titgc01
048700031105     C                   enddo
048800031105
048900031105     C     ktam          chain     tntam01l
049000031105     C                   if        %found(tntam01l)
049100031105     C                             and old_TAMatb <> *blanks
049200031105     C                   delete    tntam01
049300031105     C                   endif
049400031105
049500031105     C                   eval      kprg = (kprg - 1)
049600031105
049700031105     C                   endsr
049800031105      *****************************************************************
049900031105      * SCRIVE LE NUOVE TARIFFE
050000031105      *****************************************************************
050100031105     C     Sr_Scrivi     BEGSR
050200031105
050300080808
050400080808     c                   eval      §Fuel_tpt = *off
050500080808
050600031105     C     ktam          setll     titad04l
050700031105     C                   do        *hival
050800031105     C     ktam          reade     titad04l
050900031105     C                   if        %eof(titad04l)
051000031105     C                   leave
051100031105     C                   endif
051200031105     C                   if        TADatb <> *blanks
051300031105     C                   iter
051400031105     C                   endif
051500031105     C                   eval      new_TADksc = TADksc
051600031105     C                   eval      new_TADctr = TADctr
051700031105     C                   eval      new_TADprg = (kprg + 1)
051800031105     C                   eval      new_TADlnp = TADlnp
051900031105     C                   eval      new_TADcts = TADcts
052000031105     C                   eval      new_TADnaz = TADnaz
052100031105     C                   eval      new_TADcap = TADcap
052200031105     C                   eval      new_TADsgl = TADsgl
052300031105     C                   eval      new_TADitr = TADitr
052400031105     C                   eval      new_TADino = TADino
052500031105     C                   eval      new_TADrpv = TADrpv
052600031105     C                   eval      new_TADorp = TADorp
052700031105     C                   eval      new_TADmin = TADmin
052800031105     C                   eval      new_TADmax = TADmax
052900031105     C                   eval      new_TADftr = *blanks
053000041116     C                   eval      new_TADdtr = §oggi
053100031105     C                   write     titad000
053200031105     C                   enddo
053300031105
053400031105     C     ktam          setll     titpt01l
053500031105     C                   do        *hival
053600031105     C     ktam          reade     titpt01l
053700031105     C                   if        %eof(titpt01l)
053800031105     C                   leave
053900031105     C                   endif
054000031105     C                   if        TPTatb <> *blanks
054100031105     C                   iter
054200031105     C                   endif
054300031105     C                   eval      new_TPTksc = TPTksc
054400031105     C                   eval      new_TPTctr = TPTctr
054500031105     C                   eval      new_TPTprg = (kprg + 1)
054600031105     C                   eval      new_TPTduv = §oggi
054700031105     C                   eval      new_TPTftc = TPTftc
054800031105     C                   eval      new_TPTtpg = TPTtpg
054900031105     C                   eval      new_TPTarl = TPTarl
055000031105     C                   eval      new_TPTarf = TPTarf
055100031105     C                   eval      new_TPTaro = TPTaro
055200031105     C                   eval      new_TPTrpv = TPTrpv
055300031105     C                   eval      new_TPTvlm = TPTvlm
055400031105     C                   eval      new_TPTvvm = TPTvvm
055500031105     C                   eval      new_TPTfvm = TPTfvm
055600031105     C                   eval      new_TPTtma = TPTtma
055700031105     C                   eval      new_TPTapl = TPTapl
055800031105     C                   eval      new_TPTftr = *blanks
055900041116     C                   eval      new_TPTdtr = §oggi
056000061128     C                   eval      new_TPTdpb = TPTdpb
056100061128     C                   eval      new_TPTdit = TPTdit
056200061128     C                   eval      new_TPTflo = TPTflo
056300080808
056400080808     c                   If        TPTftc = 'f '
056500080808     c                   eval      §Fuel_tpt = *on
056600080808     c                   endif
056700080808
056800031105     C                   write     titpt000
056900031105     C                   enddo
057000031105
057100031105     C     ktam          setll     titpd01l
057200031105     C                   do        *hival
057300031105     C     ktam          reade     titpd01l
057400031105     C                   if        %eof(titpd01l)
057500031105     C                   leave
057600031105     C                   endif
057700031105     C                   if        TPDatb <> *blanks
057800031105     C                   iter
057900031105     C                   endif
058000031105     C                   eval      new_TPDksc = TPDksc
058100031105     C                   eval      new_TPDctr = TPDctr
058200031105     C                   eval      new_TPDprg = (kprg + 1)
058300031105     C                   eval      new_TPDftc = TPDftc
058400031105     C                   eval      new_TPDcts = TPDcts
058500031105     C                   eval      new_TPDorp = TPDorp
058600031105     C                   eval      new_TPDsgl = TPDsgl
058700031105     C                   eval      new_TPDitr = TPDitr
058800031105     C                   eval      new_TPDmin = TPDmin
058900031105     C                   eval      new_TPDmax = TPDmax
059000031105     C                   eval      new_TPDain = TPDain
059100031105     C                   eval      new_TPDftr = *blanks
059200041116     C                   eval      new_TPDdtr = §oggi
059300080808      * se devo aggiornare il fuel
059400080808     c                   if        TPDftc = 'f '  and TPDitr < 0,3
059500080808
059600080808      * aggiorno wfuel
059700080808     C                   eval      §prg = new_TPDprg
059800080808     C                   eval      §sgl = new_TPDsgl
059900080808     C                   eval      §fdp = TPDitr
060000080808     C                   eval      WFUflg = 'I'
060100080808     C                   exsr      Sr_ScriviWfue
060200080808
060300080811     C                   eval      new_TPDitr = 0,3
060400080808
060500080808     c                   endif
060600031105     C                   write     titpd000
060700031105     C                   enddo
060800031105
060900031105     C     ktam          setll     titgc01l
061000031105     C                   do        *hival
061100031105     C     ktam          reade     titgc01l
061200031105     C                   if        %eof(titgc01l)
061300031105     C                   leave
061400031105     C                   endif
061500031105     C                   if        TGCatb <> *blanks
061600031105     C                   iter
061700031105     C                   endif
061800031105     C                   eval      new_TGCksc = TGCksc
061900031105     C                   eval      new_TGCctr = TGCctr
062000031105     C                   eval      new_TGCprg = (kprg + 1)
062100031105     C                   eval      new_TGCsgv = TGCsgv
062200031105     C                   eval      new_TGCsgr = TGCsgr
062300031105     C                   eval      new_TGCsgp = TGCsgp
062400031105     C                   eval      new_TGCsgd = TGCsgd
062500031105     C                   eval      new_TGCsg1 = TGCsg1
062600031105     C                   eval      new_TGCsg2 = TGCsg2
062700031105     C                   eval      new_TGCsg3 = TGCsg3
062800031105     C                   eval      new_TGCst1 = TGCst1
062900031105     C                   eval      new_TGCst2 = TGCst2
063000031105     C                   eval      new_TGCst3 = TGCst3
063100031105     C                   eval      new_TGCstm = TGCstm
063200031105     C                   eval      new_TGCrpv = TGCrpv
063300031105     C                   eval      new_TGCfaf = TGCfaf
063400031105     C                   eval      new_TGCsgf = TGCsgf
063500031105     C                   eval      new_TGCggr = TGCggr
063600031105     C                   eval      new_TGCtcm = TGCtcm
063700031105     C                   eval      new_TGCtfg = TGCtfg
063800031105     C                   eval      new_TGCduv = §oggi
063900031105     C                   eval      new_TGCftr = *blanks
064000041116     C                   eval      new_TGCdtr = §oggi
064100031105     C                   write     titgc000
064200031105     C                   enddo
064300031105
064400031105     C                   eval      new_TAMksc = TAMksc
064500031105     C                   eval      new_TAMctr = TAMctr
064600031105     C                   eval      new_TAMprg = (kprg + 1)
064700080808     C                   eval      new_TAMddt = 20081001
064800031105     C                   eval      new_TAMdst = §dst
064900031105     C                   eval      new_TAMduv = §oggi
065000031105     C                   eval      new_TAMdcv = TAMdcv
065100031105     C                   eval      new_TAMdif = TAMdif
065200031105     C                   eval      new_TAMfli = TAMfli
065300031105     C                   eval      new_TAMpri = TAMpri
065400031105     C                   eval      new_TAMfmp = TAMfmp
065500031105     C                   eval      new_TAMlrc = TAMlrc
065600031105     C                   eval      new_TAMlas = TAMlas
065700080808     c                   eval      new_TAMppa = TAMppa
065800051104     c                   eval      new_TAMpag = TAMpag
065900031105     C                   eval      new_TAMmpa = TAMmpa
066000031105     C                   eval      new_TAMpam = TAMpam
066100031105     C                   eval      new_TAMmpm = TAMmpm
066200031105     C                   eval      new_TAMtin = TAMtin
066300031105     C                   eval      new_TAMkpm = TAMkpm
066400031105     C                   eval      new_TAMarl = TAMarl
066500031105     C                   eval      new_TAMarf = TAMarf
066600031105     C                   eval      new_TAMaro = TAMaro
066700031105     C                   eval      new_TAMrat = TAMrat
066800031105     C                   eval      new_TAMrct = TAMrct
066900031105     C                   eval      new_TAMsc2 = TAMsc2
067000031105     C                   eval      new_TAMeds = TAMeds
067100031105     C                   eval      new_TAMars = TAMars
067200031105     C                   eval      new_TAMata = TAMata
067300031105     C                   eval      new_TAMftp = TAMftp
067400031105     C                   eval      new_TAMbap = TAMbap
067500031105     C                   eval      new_TAMtsp = TAMtsp
067600031105     C                   eval      new_TAMfie = TAMfie
067700031105     C                   eval      new_TAMfis = TAMfis
067800031105     C                   eval      new_TAMsis = TAMsis
067900031105     C                   eval      new_TAMfvd = TAMfvd
068000031105     C                   eval      new_TAMfts = TAMfts
068100031105     C                   eval      new_TAMnas = TAMnas
068200031105     C                   eval      new_TAMnot = TAMnot
068300031105     C                   eval      new_TAMftm = TAMftm
068400031105     C                   eval      new_TAMgca = TAMgca
068500031105     C                   eval      new_TAMgga = TAMgga
068600031105     C                   eval      new_TAMgma = TAMgma
068700031105     C                   eval      new_TAMgva = TAMgva
068800031105     C                   eval      new_TAMbam = TAMbam
068900031105     C                   eval      new_TAMtpr = TAMtpr
069000031105     C                   eval      new_TAMflo = TAMflo
069100031105     C                   eval      new_TAMdat = TAMdat
069200031105     C                   eval      new_TAMftr = *blanks
069300041116     C                   eval      new_TAMdtr = §oggi
069400031105     C                   write     tntam000
069500031105
069600080808      * se devo scrivere il record del fuel
069700080808     c                   If        §Fuel_tpt = *off
069800080811     c                   eval      §prg = (kprg + 1)
069900080808     c                   exsr      sr_fuel
070000080808     c                   endif
070100080808
070200031105     C                   endsr
070300080807      *****************************************************************
070400080807      * SCRIVE TARIFFA PARTICOLARE FUEL
070500080807      *****************************************************************
070600080807     C     Sr_Fuel       BEGSR
070700080807
070800080807     c
070900080807     C                   clear                   titpt000
071000080807     C                   eval      new_TPTksc = tamksc
071100080807     C                   eval      new_TPTctr = tamctr
071200080807     C                   eval      new_TPTprg = §prg
071300080807     C                   eval      new_TPTduv = §oggi
071400080807     C                   eval      new_TPTftc = 'f '
071500080807     C                   eval      new_TPTtpg = 'V'
071600080807     C                   eval      new_TPTftr = *blanks
071700080807     C                   eval      new_TPTdtr = §oggi
071800080814     C                   eval      new_TPTdpb = data_base
071900080807     C                   eval      new_TPTdit = §oggi
072000080807     C                   write     titpt000
072100080807     C                   clear                   titpd000
072200080807     C                   eval      new_TPDksc = tamksc
072300080807     C                   eval      new_TPDctr = tamctr
072400080807     C                   eval      new_TPDprg = §prg
072500080807     C                   eval      new_TPDftc = 'f '
072600080807     c                   if        tamfie = 'I'
072700080807     C                   eval      new_TPDcts = 'IT'
072800080807     C                   eval      new_TPDorp = '  A 9'
072900080807     c                   endif
073000080807     c                   if        tamfie = 'E'
073100080807     C                   eval      new_TPDcts = 'EE'
073200080807     C                   eval      new_TPDorp = '  999'
073300080807     c                   endif
073400080807     C                   eval      new_TPDsgl = 9999999999,000
073500080807     C                   eval      new_TPDitr = 0,3
073600080807     C                   eval      new_TPDftr = *blanks
073700080807     C                   eval      new_TPDdtr = §oggi
073800080807     C                   write     titpd000
073900080808
074000080811     C                   eval      §prg = new_TPDprg
074100080811     C                   eval      §sgl = new_TPDsgl
074200080808     C                   eval      §fdp = 0
074300080808     C                   eval      WFUflg = 'A'
074400080808     C                   exsr      Sr_ScriviWfue
074500080807
074600080807     c                   endsr
074700031105      *****************************************************************
074800080808      * SCRIVE IL FILE WFUEL00F
074900031105      *****************************************************************
075000080808     C     Sr_ScriviWfue BEGSR
075100031105
075200080808     C                   eval      WFUksc = TAMksc
075300080808     C                   eval      WFUctr = TAMctr
075400080808     C                   eval      WFUprg = §prg
075500080811     C                   eval      WFUsgl = §sgl
075600080808     C                   eval      WFUpfd = §fdp
075700080811     C                   eval      WFUnfd = 0,3
075800080808     C                   write     wfuel000
075900031105
076000031105     C                   endsr
076100031105      *****************************************************************
076200031105      * ROUTINE INIZIALE
076300031105      *****************************************************************
076400031105     C     *INZSR        BEGSR
076500031105
076600031105     C     *ENTRY        PLIST
076700031105     C                   PARM                    KPJBA
076800080806     c                   movel     kpjbu         param
076900031105      *
077000031105     c
077100031105     C     Ktam          klist
077200031105     C                   kfld                    TAMksc
077300031105     C                   kfld                    TAMctr
077400031105     C                   kfld                    Kprg
077500080806
077600080806     C     Ktamin        klist
077700080806     C                   kfld                    wfaclf
077800080806     C                   kfld                    wfactr
077900080807
078000080807     C     Ktpd          klist
078100080807     C                   kfld                    old_tamksc
078200080807     C                   kfld                    old_tamctr
078300080807     C                   kfld                    old_tamprg
078400080807     C                   kfld                    Kftc
078500031105
078600080808     C     Ktpd2         klist
078700080808     C                   kfld                    tamksc
078800080808     C                   kfld                    tamctr
078900080808     C                   kfld                    tamprg
079000080808     C                   kfld                    Kftc
079100080811
079200080811     C     Ktpd2old      klist
079300080811     C                   kfld                    old_tamksc
079400080811     C                   kfld                    old_tamctr
079500080811     C                   kfld                    old_tamprg
079600080811     C                   kfld                    Kftc
079700080814
079800080814     c     keytab        klist
079900080814     c                   kfld                    codut
080000080814     c                   kfld                    kcod
080100080814     c                   kfld                    kkey
080200031105
080300031105      * reperisce data del giorno
080400031105     C                   TIME                    W0140
080500031105      * UDATE IN GGMMAAAA
080600031105     C                   MOVE      W0140         WDTGIO
080700031105      * UDATE IN AAAAMMGG
080800031105     C                   Z-ADD     WDTGIO        G02DAT
080900031105     C                   MOVEL     *BLANK        G02ERR
081000031105     C                   CALL      'XSRDA8'
081100031105     C                   PARM                    WLBDAT
081200031105     C                   Z-ADD     G02INV        §oggi
081300080814      * cerco la data prezzo base
081400080814     c                   move(p)   'FUEL'        kkey
081500080814     c                   movel(p)  '%A'          kcod
081600080814     c     Keytab        chain     tabel00f
081700080814     c                   If        %found(tabel00f)
081800080814     c                   movel     tbluni        data_base
081900080814     c                   endif
082000031105
082100031105     C                   endsr
