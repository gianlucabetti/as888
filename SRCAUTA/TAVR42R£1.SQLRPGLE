000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300031105      *****************************************************************
000400031105      *                                                               *
000500031105      *       AGGIORNA TARIFFE CON ADDIZIONALE                        *
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
002500081201     FTabel00f  if   e           k DISK
002600031105      *------------------------------------------------------------------------*
002700031105
002800081128     D W_wfaclf        s                   like(wfaclf)
002900081128     D W_wfatcl        s                   like(wfatcl)
003000081128     D W_wfafauc       s                   like(wfafauc)
003100081128
003200081128
003300031105     D Kprg            S                   LIKE(TAMprg)
003400031105     D §ksc            S                   LIKE(TAMksc)
003500031105     D §ctr            S                   LIKE(TAMctr)
003600031105     D §prg            S                   LIKE(TAMprg)
003700051024     D §ppa            S                   LIKE(TAMppa)
003800051104     D §pag            S                   LIKE(TAMpag)
003900031105     D §dst            S                   LIKE(TAMdst)
004000041126     D******savtampag       S                   LIKE(TAMpag)
004100081201     d AU_Tipo         s                   like(WFAtcl) inz('C')
004200081201     d AU_Cliente      s                   like(WFAclf)
004300081201     d AU_Tariffa      s                   like(WFActrv)
004400081201     d AU_Progres      s                   like(WFAprgv)
004500081201     D Kcod            S                   like(TBLCOD)
004600081201     D Kkey            S                   like(TBLKEY)
004700081201     D Codut           s              1  0 inz(1)
004800081201
004900031105     D W0140           S             14  0
005000031105     D Wdtgio          S              8  0
005100031105     D §oggi           S              8  0
005200051109     D caladd          s             30  9
005300031105     D §flagA          S              1
005400031105     D §flagB          S              1
005500031105     D §flagC          S              1
005600031105     D §flagD          S              1
005700081201     D Up_tam          S              1
005800081201     D perce           s              5  2
005900031105
006000031105      *   D S   I N T E R N E / E S T E R N E
006100031105
006200031105     D WLBDAT          DS                  INZ
006300031105     D  G02DAT                 1      8  0
006400031105     D  G02INV                 9     16  0
006500031105     D  G02ERR                17     17
006600031105     D  G02TGI                18     22  0
006700031105
006800031105     D                 ds
006900031105     D tamksc                  1      7  0
007000031105     D  ksc4                   1      4  0
007100031105     D  ksc33                  1      3  0
007200031105     D  ksc3                   5      7  0
007300031105
007400061128     D Dsta01        e ds
007500031105     D kpjba         e ds
007600031105
007700031105      *---------------------------------------------------------------------------------------------
007800081128      *------------------------------------------------------------------------*
007900081128      * Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008000081128     c
008100081128     c/EXEC SQL
008200081128     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008300081128     c/END-EXEC
008400081128     c*
008500081128     C/EXEC SQL
008600081128     C+ declare C1 cursor for
008700081128     C+ SELECT wfatcl , wfaclf, wfafauc  FROM EDPAUMENTI/WFaut00F
008800081128     C+ WHERE WFATCL = 'C'
008900081201     C+ group BY wfatcl , wfaclf, wfafauc
009000081128     c+ for read only
009100081128     C/END-EXEC
009200081128
009300081128     C/EXEC SQL
009400081128     C+ open C1
009500081128     C/END-EXEC
009600081128     C
009700081128     C/EXEC SQL
009800081128     C+ Fetch C1 into :W_wfatcl , :W_wfaclf , :W_wfafauc
009900081128     C/END-EXEC
010000081128     C*
010100081128
010200081128     C*
010300081128     C                   dow       sqlcod = *zeros
010400031105
010500031105      *
010600031105      * Aggancio Tntam04l
010700081128     c     W_Wfaclf      setll     tntam04l
010800031105     c                   do        *hival
010900081128     c     W_wfaclf      reade     tntam04l
011000031105      *
011100031105      * fine file
011200031105     c                   if        %eof(tntam04l)
011300031105     c                   leave
011400031105     c                   endif
011500081201
011600081201     c                   eval      AU_cliente = tamksc
011700081201     c                   eval      AU_tariffa = tamctr
011800081201     c                   eval      AU_progres = tamprg
011900031105
012000031105      * Se stessa tariffa e si è verificata una delle condizioni
012100031105      * non deve elaborare più niente
012200031105     C                   if        (TAMksc = §ksc and TAMctr = §ctr
012300031105     C                             and §flagA = *on) or
012400031105     C                             (TAMksc = §ksc and TAMctr = §ctr
012500031105     C                             and §flagB = *on) or
012600031105     C                             (TAMksc = §ksc and TAMctr = §ctr
012700031105     C                             and §flagC = *on)
012800031105     C                   iter
012900031105     C                   endif
013000031105
013100031105      * Cambia tariffa
013200031105     C                   if        TAMksc <> §ksc
013300081201     C                   eval      §ksc = TAMksc
013400031105     C                   eval      §flagA = *off
013500031105     C                   eval      §flagB = *off
013600031105     C                   eval      §flagC = *off
013700031105     C                   endif
013800031105     C                   if        TAMctr <> §ctr
013900031105     C                   eval      §ctr = TAMctr
014000031105     C                   eval      §flagA = *off
014100031105     C                   eval      §flagB = *off
014200031105     C                   eval      §flagC = *off
014300031105     C                   endif
014400031105
014500061128      * valorizzo i campi dei flag operativi
014600061128     c                   movel     tamflo        dsta01
014700061128
014800081128      * Decorrenza maggiore/uguale 01/01/2009
014900081201    1C                   if        TAMddt >= 20090101
015000081201     c                   clear                   up_tam
015100081128     C                   eval      §flagA = (TAMddt = 20090101)
015200031105     C                   eval      §flagD = *off
015300081201    2C                   if        tamddt <> 20090101 and tamdst <> 20090101
015400031105     C                   exsr      Sr_CercaDopo
015500081201    3C                   if        §flagD = *on
015600081201     c                   eval      up_tam = *on
015700031105     C                   eval      kprg = TAMprg
015800031105     C                   exsr      Sr_Aggiorna
015900031105     C                   eval      TAMprg = (TAMprg + 1)
016000081201    3C                   endif
016100081201    2C                   endif
016200081201      * se esiste % anni corrente faccio l'inglobamento
016300081201    2c                   If        tamppa > 0
016400051021      * se esiste % anni precedente sommiamo le 2 percentuali
016500081201    3c                   If        tampag > 0
016600051109     c                   eval      caladd = tampag + tamppa +
016700051109     c                                      ((tampag *tamppa)/100)
016800051109     c                   z-add(h)  caladd        tampag
016900051021     c                   else
017000051021     c                   eval      tampag = tamppa
017100081201    3c                   endif
017200051021     c                   eval      tamppa = 0
017300081201     c                   eval      up_tam = *on
017400081201    2c                   endif
017500051021      * se devo aumentare
017600081202    2c                   If        W_wfafauc = 'A'
017700081201     c                   eval      TAMppa = perce
017800081201     c                   eval      up_tam = *on
017900081201    2c                   endif
018000081201      * se devo aggiornare l'Istat
018100081202    2c                   If        W_wfafauc = 'I' and tamfli = 0
018200081201     c                   eval      TAMfli = 5
018300081201     c                   eval      TAMpri = 100
018400081201     c                   eval      up_tam = *on
018500081201    2c                   endif
018600081201      * aggiorno TNTAM anche se cambiato progressivo
018700081201    2c                   If        up_tam = *on
018800031105     C                   eval      TAMduv = §oggi
018900031105     C                   eval      TAMftr = *blanks
019000041116     C                   eval      TAMdtr = §oggi
019100031105     C                   update    tntam04
019200081201      * aggiorno il progressivo vecchio con quello nuovo
019300081201     c     kaut          chain     wfaut01l
019400081201    3c                   if        %found(wfaut01l)
019500081201     c                   eval      wfaprgn = tamprg
019600081201     c                   eval      wfaddtn = tamddt
019700081201     c                   eval      wfadstn = tamdst
019800081201    4c                   if        tamfli <> 0
019900081201     c                   eval      wfaistn = '1'
020000081201     c                   else
020100081201     c                   eval      wfaistn = '0'
020200081201    4c                   endif
020300081201     c                   eval      wfapagn = tampag
020400081201     c                   eval      wfappan = tamppa
020500081202     c                   eval      wfaela = 'S'
020600081201     c                   update    wfaut000
020700081201    3C                   endif
020800081201    2C                   endif
020900061207
021000081201    1c                   endif
021100031105
021200081128      * Scadenza maggiore/uguale 01/01/2009 e decorrenza minore 01/01/2009
021300081201    1C                   if        TAMdst >= 20090101 and TAMddt < 20090101
021400031105     C                   eval      §flagB = *on
021500031105     C                   eval      §dst = TAMdst
021600031105     C                   eval      kprg = TAMprg
021700051021      * verifico se devo aggiornare la tariffa  o per integrazione o per aumento altrimenti
021800051021      * non faccio nulla
021900081201     c                   clear                   Up_tam
022000081201      * deleta i record annullati del progressivo successivo
022100031105     C                   exsr      Sr_Annulla
022200081201      * scrivo il nuovo progressivo del dettaglio a cavallo d'anno
022300031105     C                   exsr      Sr_Scrivi
022400031105     C                   eval      §prg = (kprg +1)
022500051024     C                   eval      §ppa = New_TAMppa
022600051104     C                   eval      §pag = New_TAMpag
022700081201     C                   eval      TAMdst = 20081231
022800031105     C                   eval      TAMduv = §oggi
022900031105     C                   eval      TAMftr = *blanks
023000041116     C                   eval      TAMdtr = §oggi
023100031105     C                   update    tntam04
023200031105     C                   iter
023300051021     C                   endif
023400031105
023500081201      * Scadenza minore/uguale 31/12/2008
023600081128     C                   if        TAMdst <= 20081231
023700031105     C                   eval      §flagC = *on
023800051103      *
023900081128     C                   eval      §dst = 20091231
024000031105     C                   eval      kprg = TAMprg
024100031105     C                   exsr      Sr_Annulla
024200031105     C                   exsr      Sr_Scrivi
024300031105     C                   endif
024400051021
024500031105     C                   enddo
024600081201     C
024700081201     C/EXEC SQL
024800081201     C+ Fetch C1 into :W_wfatcl , :W_wfaclf , :W_wfafauc
024900081201     C/END-EXEC
025000081201     C*
025100081201     C                   enddo
025200051021
025300081201     C*
025400081201     C/EXEC SQL
025500081201     C+ close C1
025600081201     C/END-EXEC
025700081201     C
025800081201
025900081201     c                   seton                                        lr
026000081201
026100031105      *****************************************************************
026200031105      * CERCA LA TARIFFA SUCESSIVA E CONTROLLA SE A CAVALLO D'ANNO
026300031105      *****************************************************************
026400031105     C     Sr_CercaDopo  BEGSR
026500031105
026600031105     C                   eval      kprg = (TAMprg - 1)
026700031105     C     ktam          chain(n)  tntam01l
026800031105     C                   if        %found(tntam01l)
026900081128     C                             and old_TAMdst >= 20090101
027000081128     C                             and old_TAMddt < 20090101
027100081201      * valorizzo il falg D perchè comunque sia devo sempre scrivere un nuovo progressivo
027200031105     C                   eval      §flagD = *on
027300051021
027400031105     C                   endif
027500031105
027600031105     C                   endsr
027700031105      *****************************************************************
027800031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
027900031105      *****************************************************************
028000031105     C     Sr_Aggiorna   BEGSR
028100031105
028200031105     C     ktam          setll     titad04l
028300031105     C                   do        *hival
028400031105     C     ktam          reade     titad04l
028500031105     C                   if        %eof(titad04l)
028600031105     C                   leave
028700031105     C                   endif
028800031105     C                   if        TADatb <> *blanks
028900031105     C                   iter
029000031105     C                   endif
029100031105     C                   eval      TADprg = (TADprg + 1)
029200031105     C                   eval      TADftr = *blanks
029300041116     C                   eval      TADdtr = §oggi
029400031105     C                   update    titad04
029500031105     C                   enddo
029600031105
029700031105     C     ktam          setll     titpt01l
029800031105     C                   do        *hival
029900031105     C     ktam          reade     titpt01l
030000031105     C                   if        %eof(titpt01l)
030100031105     C                   leave
030200031105     C                   endif
030300031105     C                   if        TPTatb <> *blanks
030400031105     C                   iter
030500031105     C                   endif
030600031105     C                   eval      TPTprg = (TPTprg + 1)
030700031105     C                   eval      TPTduv = §oggi
030800031105     C                   eval      TPTftr = *blanks
030900061128     C                   eval      TPTdtr = §oggi
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
032200031105     C                   eval      TPDprg = (TPDprg + 1)
032300031105     C                   eval      TPDftr = *blanks
032400041116     C                   eval      TPDdtr = §oggi
032500031105     C                   update    titpd01
032600031105     C                   enddo
032700031105
032800031105     C     ktam          setll     titgc01l
032900031105     C                   do        *hival
033000031105     C     ktam          reade     titgc01l
033100031105     C                   if        %eof(titgc01l)
033200031105     C                   leave
033300031105     C                   endif
033400031105     C                   if        TGCatb <> *blanks
033500031105     C                   iter
033600031105     C                   endif
033700031105     C                   eval      TGCprg = (TGCprg + 1)
033800031105     C                   eval      TGCduv = §oggi
033900031105     C                   eval      TGCftr = *blanks
034000041116     C                   eval      TGCdtr = §oggi
034100031105     C                   update    titgc01
034200031105     C                   enddo
034300031105
034400031105     C                   endsr
034500031105      *****************************************************************
034600031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
034700031105      *****************************************************************
034800031105     C     Sr_Annulla    BEGSR
034900031105
035000031105     C                   eval      kprg = (kprg + 1)
035100031105
035200031105     C     ktam          setll     titad04l
035300031105     C                   do        *hival
035400031105     C     ktam          reade     titad04l
035500031105     C                   if        %eof(titad04l)
035600031105     C                   leave
035700031105     C                   endif
035800031105     C                   if        TADatb = *blanks
035900031105     C                   iter
036000031105     C                   endif
036100031105     C                   delete    titad04
036200031105     C                   enddo
036300031105
036400031105     C     ktam          setll     titpt01l
036500031105     C                   do        *hival
036600031105     C     ktam          reade     titpt01l
036700031105     C                   if        %eof(titpt01l)
036800031105     C                   leave
036900031105     C                   endif
037000031105     C                   if        TPTatb = *blanks
037100031105     C                   iter
037200031105     C                   endif
037300031105     C                   delete    titpt01
037400031105     C                   enddo
037500031105
037600031105     C     ktam          setll     titpd01l
037700031105     C                   do        *hival
037800031105     C     ktam          reade     titpd01l
037900031105     C                   if        %eof(titpd01l)
038000031105     C                   leave
038100031105     C                   endif
038200031105     C                   if        TPDatb = *blanks
038300031105     C                   iter
038400031105     C                   endif
038500031105     C                   delete    titpd01
038600031105     C                   enddo
038700031105
038800031105     C     ktam          setll     titgc01l
038900031105     C                   do        *hival
039000031105     C     ktam          reade     titgc01l
039100031105     C                   if        %eof(titgc01l)
039200031105     C                   leave
039300031105     C                   endif
039400031105     C                   if        TGCatb = *blanks
039500031105     C                   iter
039600031105     C                   endif
039700031105     C                   delete    titgc01
039800031105     C                   enddo
039900031105
040000031105     C     ktam          chain     tntam01l
040100031105     C                   if        %found(tntam01l)
040200031105     C                             and old_TAMatb <> *blanks
040300031105     C                   delete    tntam01
040400031105     C                   endif
040500031105
040600031105     C                   eval      kprg = (kprg - 1)
040700031105
040800031105     C                   endsr
040900031105      *****************************************************************
041000031105      * SCRIVE LE NUOVE TARIFFE
041100031105      *****************************************************************
041200031105     C     Sr_Scrivi     BEGSR
041300031105
041400081202     c                   eval      up_tam = *on
041500081202
041600031105     C     ktam          setll     titad04l
041700031105     C                   do        *hival
041800031105     C     ktam          reade     titad04l
041900031105     C                   if        %eof(titad04l)
042000031105     C                   leave
042100031105     C                   endif
042200031105     C                   if        TADatb <> *blanks
042300031105     C                   iter
042400031105     C                   endif
042500031105     C                   eval      new_TADksc = TADksc
042600031105     C                   eval      new_TADctr = TADctr
042700031105     C                   eval      new_TADprg = (kprg + 1)
042800031105     C                   eval      new_TADlnp = TADlnp
042900031105     C                   eval      new_TADcts = TADcts
043000031105     C                   eval      new_TADnaz = TADnaz
043100031105     C                   eval      new_TADcap = TADcap
043200031105     C                   eval      new_TADsgl = TADsgl
043300031105     C                   eval      new_TADitr = TADitr
043400031105     C                   eval      new_TADino = TADino
043500031105     C                   eval      new_TADrpv = TADrpv
043600031105     C                   eval      new_TADorp = TADorp
043700031105     C                   eval      new_TADmin = TADmin
043800031105     C                   eval      new_TADmax = TADmax
043900031105     C                   eval      new_TADftr = *blanks
044000041116     C                   eval      new_TADdtr = §oggi
044100031105     C                   write     titad000
044200031105     C                   enddo
044300031105
044400031105     C     ktam          setll     titpt01l
044500031105     C                   do        *hival
044600031105     C     ktam          reade     titpt01l
044700031105     C                   if        %eof(titpt01l)
044800031105     C                   leave
044900031105     C                   endif
045000031105     C                   if        TPTatb <> *blanks
045100031105     C                   iter
045200031105     C                   endif
045300031105     C                   eval      new_TPTksc = TPTksc
045400031105     C                   eval      new_TPTctr = TPTctr
045500031105     C                   eval      new_TPTprg = (kprg + 1)
045600031105     C                   eval      new_TPTduv = §oggi
045700031105     C                   eval      new_TPTftc = TPTftc
045800031105     C                   eval      new_TPTtpg = TPTtpg
045900031105     C                   eval      new_TPTarl = TPTarl
046000031105     C                   eval      new_TPTarf = TPTarf
046100031105     C                   eval      new_TPTaro = TPTaro
046200031105     C                   eval      new_TPTrpv = TPTrpv
046300031105     C                   eval      new_TPTvlm = TPTvlm
046400031105     C                   eval      new_TPTvvm = TPTvvm
046500031105     C                   eval      new_TPTfvm = TPTfvm
046600031105     C                   eval      new_TPTtma = TPTtma
046700031105     C                   eval      new_TPTapl = TPTapl
046800031105     C                   eval      new_TPTftr = *blanks
046900041116     C                   eval      new_TPTdtr = §oggi
047000061128     C                   eval      new_TPTdpb = TPTdpb
047100061128     C                   eval      new_TPTdit = TPTdit
047200061128     C                   eval      new_TPTflo = TPTflo
047300031105     C                   write     titpt000
047400031105     C                   enddo
047500031105
047600031105     C     ktam          setll     titpd01l
047700031105     C                   do        *hival
047800031105     C     ktam          reade     titpd01l
047900031105     C                   if        %eof(titpd01l)
048000031105     C                   leave
048100031105     C                   endif
048200031105     C                   if        TPDatb <> *blanks
048300031105     C                   iter
048400031105     C                   endif
048500031105     C                   eval      new_TPDksc = TPDksc
048600031105     C                   eval      new_TPDctr = TPDctr
048700031105     C                   eval      new_TPDprg = (kprg + 1)
048800031105     C                   eval      new_TPDftc = TPDftc
048900031105     C                   eval      new_TPDcts = TPDcts
049000031105     C                   eval      new_TPDorp = TPDorp
049100031105     C                   eval      new_TPDsgl = TPDsgl
049200031105     C                   eval      new_TPDitr = TPDitr
049300031105     C                   eval      new_TPDmin = TPDmin
049400031105     C                   eval      new_TPDmax = TPDmax
049500031105     C                   eval      new_TPDain = TPDain
049600031105     C                   eval      new_TPDftr = *blanks
049700041116     C                   eval      new_TPDdtr = §oggi
049800031105     C                   write     titpd000
049900031105     C                   enddo
050000031105
050100031105     C     ktam          setll     titgc01l
050200031105     C                   do        *hival
050300031105     C     ktam          reade     titgc01l
050400031105     C                   if        %eof(titgc01l)
050500031105     C                   leave
050600031105     C                   endif
050700031105     C                   if        TGCatb <> *blanks
050800031105     C                   iter
050900031105     C                   endif
051000031105     C                   eval      new_TGCksc = TGCksc
051100031105     C                   eval      new_TGCctr = TGCctr
051200031105     C                   eval      new_TGCprg = (kprg + 1)
051300031105     C                   eval      new_TGCsgv = TGCsgv
051400031105     C                   eval      new_TGCsgr = TGCsgr
051500031105     C                   eval      new_TGCsgp = TGCsgp
051600031105     C                   eval      new_TGCsgd = TGCsgd
051700031105     C                   eval      new_TGCsg1 = TGCsg1
051800031105     C                   eval      new_TGCsg2 = TGCsg2
051900031105     C                   eval      new_TGCsg3 = TGCsg3
052000031105     C                   eval      new_TGCst1 = TGCst1
052100031105     C                   eval      new_TGCst2 = TGCst2
052200031105     C                   eval      new_TGCst3 = TGCst3
052300031105     C                   eval      new_TGCstm = TGCstm
052400031105     C                   eval      new_TGCrpv = TGCrpv
052500031105     C                   eval      new_TGCfaf = TGCfaf
052600031105     C                   eval      new_TGCsgf = TGCsgf
052700031105     C                   eval      new_TGCggr = TGCggr
052800031105     C                   eval      new_TGCtcm = TGCtcm
052900031105     C                   eval      new_TGCtfg = TGCtfg
053000031105     C                   eval      new_TGCduv = §oggi
053100031105     C                   eval      new_TGCftr = *blanks
053200041116     C                   eval      new_TGCdtr = §oggi
053300031105     C                   write     titgc000
053400031105     C                   enddo
053500031105
053600031105     C                   eval      new_TAMksc = TAMksc
053700031105     C                   eval      new_TAMctr = TAMctr
053800031105     C                   eval      new_TAMprg = (kprg + 1)
053900081201     C                   eval      new_TAMddt = 20090101
054000031105     C                   eval      new_TAMdst = §dst
054100031105     C                   eval      new_TAMduv = §oggi
054200031105     C                   eval      new_TAMdcv = TAMdcv
054300031105     C                   eval      new_TAMdif = TAMdif
054400031105     C                   eval      new_TAMfli = TAMfli
054500031105     C                   eval      new_TAMpri = TAMpri
054600031105     C                   eval      new_TAMfmp = TAMfmp
054700031105     C                   eval      new_TAMlrc = TAMlrc
054800031105     C                   eval      new_TAMlas = TAMlas
054900051021      * se % addizionale corrente maggiore di zero devo addizionarla a quella precedente
055000051021     c                   If        TAMppa > 0
055100051109     c                   eval      caladd = tampag + tamppa +
055200051109     c                                      ((tampag *tamppa)/100)
055300051109     c                   z-add(h)  caladd        new_TAMpag
055400051021     c                   eval      new_TAMppa = *zeros
055500051104
055600051104     c                   else
055700051104     c                   eval      new_TAMpag = TAMpag
055800081202     c                   eval      new_TAMppa = TAMppa
055900051021     c                   endif
056000051021      * se aumento
056100081202     c                   If        W_WFAfauc = 'A'
056200081201     C                   eval      new_TAMppa = perce
056300051021     c                   endif
056400051021
056500081201      * se devo aggiornare l'Istat
056600081202    2c                   If        W_wfafauc = 'I' and tamfli = 0
056700081201     c                   eval      new_TAMfli = 5
056800081201     c                   eval      new_TAMpri = 100
056900081201    2c                   endif
057000081201
057100031105     C                   eval      new_TAMmpa = TAMmpa
057200031105     C                   eval      new_TAMpam = TAMpam
057300031105     C                   eval      new_TAMmpm = TAMmpm
057400031105     C                   eval      new_TAMtin = TAMtin
057500031105     C                   eval      new_TAMkpm = TAMkpm
057600031105     C                   eval      new_TAMarl = TAMarl
057700031105     C                   eval      new_TAMarf = TAMarf
057800031105     C                   eval      new_TAMaro = TAMaro
057900031105     C                   eval      new_TAMrat = TAMrat
058000031105     C                   eval      new_TAMrct = TAMrct
058100031105     C                   eval      new_TAMsc2 = TAMsc2
058200031105     C                   eval      new_TAMeds = TAMeds
058300031105     C                   eval      new_TAMars = TAMars
058400031105     C                   eval      new_TAMata = TAMata
058500031105     C                   eval      new_TAMftp = TAMftp
058600031105     C                   eval      new_TAMbap = TAMbap
058700031105     C                   eval      new_TAMtsp = TAMtsp
058800031105     C                   eval      new_TAMfie = TAMfie
058900031105     C                   eval      new_TAMfis = TAMfis
059000031105     C                   eval      new_TAMsis = TAMsis
059100031105     C                   eval      new_TAMfvd = TAMfvd
059200031105     C                   eval      new_TAMfts = TAMfts
059300031105     C                   eval      new_TAMnas = TAMnas
059400031105     C                   eval      new_TAMnot = TAMnot
059500031105     C                   eval      new_TAMftm = TAMftm
059600031105     C                   eval      new_TAMgca = TAMgca
059700031105     C                   eval      new_TAMgga = TAMgga
059800031105     C                   eval      new_TAMgma = TAMgma
059900031105     C                   eval      new_TAMgva = TAMgva
060000031105     C                   eval      new_TAMbam = TAMbam
060100031105     C                   eval      new_TAMtpr = TAMtpr
060200031105     C                   eval      new_TAMflo = TAMflo
060300031105     C                   eval      new_TAMdat = TAMdat
060400031105     C                   eval      new_TAMftr = *blanks
060500041116     C                   eval      new_TAMdtr = §oggi
060600031105     C                   write     tntam000
060700081201      * aggiorno il progressivo vecchio con quello nuovo
060800081201     c                   If        Up_tam = *on
060900081201     c     kaut          chain     wfaut01l
061000081201    3c                   if        %found(wfaut01l)
061100081201     c                   eval      wfaddtn = new_tamddt
061200081201     c                   eval      wfadstn = new_tamdst
061300081201     c                   eval      wfaprgn = new_tamprg
061400081201    4c                   if        new_tamfli <> 0
061500081201     c                   eval      wfaistn = '1'
061600081201     c                   else
061700081201     c                   eval      wfaistn = '0'
061800081201    4c                   endif
061900081201     c                   eval      wfapagn = new_tampag
062000081201     c                   eval      wfappan = new_tamppa
062100081202     c                   eval      wfaela = 'S'
062200081201     c                   update    wfaut000
062300081201    3C                   endif
062400081201    2C                   endif
062500031105
062600031105     C                   endsr
062700031105      *****************************************************************
062800031105      * ROUTINE INIZIALE
062900031105      *****************************************************************
063000031105     C     *INZSR        BEGSR
063100031105
063200031105     C     *ENTRY        PLIST
063300031105     C                   PARM                    KPJBA
063400031105      *
063500031105     c
063600031105     C     Ktam          klist
063700031105     C                   kfld                    TAMksc
063800031105     C                   kfld                    TAMctr
063900031105     C                   kfld                    Kprg
064000031105
064100081201      *
064200081201     c
064300081201     C     Kaut          klist
064400081201     C                   kfld                    AU_Tipo
064500081201     C                   kfld                    AU_Cliente
064600081201     C                   kfld                    AU_Tariffa
064700081201     C                   kfld                    AU_Progres
064800081201
064900081201      *
065000081201     c     keytab        klist
065100081201     c                   kfld                    codut
065200081201     c                   kfld                    kcod
065300081201     c                   kfld                    kkey
065400081201      *
065500031105      * reperisce data del giorno
065600031105     C                   TIME                    W0140
065700031105      * UDATE IN GGMMAAAA
065800031105     C                   MOVE      W0140         WDTGIO
065900031105      * UDATE IN AAAAMMGG
066000031105     C                   Z-ADD     WDTGIO        G02DAT
066100031105     C                   MOVEL     *BLANK        G02ERR
066200031105     C                   CALL      'XSRDA8'
066300031105     C                   PARM                    WLBDAT
066400031105     C                   Z-ADD     G02INV        §oggi
066500031105
066600081201      * cerco le percentuali di aumento
066700081201     c                   clear                   perce
066800081201     c                   move(p)   '2008'        kkey
066900081201     c                   movel(p)  '%A'          kcod
067000081201     c     Keytab        chain     tabel00f
067100081201     c                   If        %found(tabel00f)
067200081201     c                   movel     tbluni        perce
067300081201     c                   endif
067400031105     C                   endsr
