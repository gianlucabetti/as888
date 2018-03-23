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
004200041126     D******savtampag       S                   LIKE(TAMpag)
004300081201     d AU_Tipo         s                   like(WFAtcl) inz('C')
004400081201     d AU_Cliente      s                   like(WFAclf)
004500081201     d AU_Tariffa      s                   like(WFActrv)
004600081201     d AU_Progres      s                   like(WFAprgv)
004700081201     D Kcod            S                   like(TBLCOD)
004800081201     D Kkey            S                   like(TBLKEY)
004900081201     D Codut           s              1  0 inz(1)
005000081201
005100031105     D W0140           S             14  0
005200031105     D Wdtgio          S              8  0
005300031105     D §oggi           S              8  0
005400051109     D caladd          s             30  9
005500031105     D §flagA          S              1
005600031105     D §flagB          S              1
005700031105     D §flagC          S              1
005800031105     D §flagD          S              1
005900081201     D Up_tam          S              1
006000081201     D perce           s              5  2
006100111209     D ISTAT           s              3  0 inz(8)
006200031105
006300031105      *   D S   I N T E R N E / E S T E R N E
006400031105
006500031105     D WLBDAT          DS                  INZ
006600031105     D  G02DAT                 1      8  0
006700031105     D  G02INV                 9     16  0
006800031105     D  G02ERR                17     17
006900031105     D  G02TGI                18     22  0
007000031105
007100031105     D                 ds
007200031105     D tamksc                  1      7  0
007300031105     D  ksc4                   1      4  0
007400031105     D  ksc33                  1      3  0
007500031105     D  ksc3                   5      7  0
007600031105
007700061128     D Dsta01        e ds
007800031105     D kpjba         e ds
007900031105
008000031105      *---------------------------------------------------------------------------------------------
008100081128      *------------------------------------------------------------------------*
008200081128      * Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008300081128     c
008400081128     c/EXEC SQL
008500081128     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008600081128     c/END-EXEC
008700081128     c*
008800081128     C/EXEC SQL
008900081128     C+ declare C1 cursor for
009000081128     C+ SELECT wfatcl , wfaclf, wfafauc  FROM EDPAUMENTI/WFaut00F
009100081128     C+ WHERE WFATCL = 'C'
009200081201     C+ group BY wfatcl , wfaclf, wfafauc
009300081128     c+ for read only
009400081128     C/END-EXEC
009500081128
009600081128     C/EXEC SQL
009700081128     C+ open C1
009800081128     C/END-EXEC
009900081128     C
010000081128     C/EXEC SQL
010100081128     C+ Fetch C1 into :W_wfatcl , :W_wfaclf , :W_wfafauc
010200081128     C/END-EXEC
010300081128     C*
010400081128
010500081128     C*
010600081128     C                   dow       sqlcod = *zeros
010700031105
010800031105      *
010900031105      * Aggancio Tntam04l
011000081128     c     W_Wfaclf      setll     tntam04l
011100031105     c                   do        *hival
011200081128     c     W_wfaclf      reade     tntam04l
011300031105      *
011400031105      * fine file
011500031105     c                   if        %eof(tntam04l)
011600031105     c                   leave
011700031105     c                   endif
011800081201
011900081201     c                   eval      AU_cliente = tamksc
012000081201     c                   eval      AU_tariffa = tamctr
012100081201     c                   eval      AU_progres = tamprg
012200031105
012300031105      * Se stessa tariffa e si è verificata una delle condizioni
012400031105      * non deve elaborare più niente
012500031105     C                   if        (TAMksc = §ksc and TAMctr = §ctr
012600031105     C                             and §flagA = *on) or
012700031105     C                             (TAMksc = §ksc and TAMctr = §ctr
012800031105     C                             and §flagB = *on) or
012900031105     C                             (TAMksc = §ksc and TAMctr = §ctr
013000031105     C                             and §flagC = *on)
013100031105     C                   iter
013200031105     C                   endif
013300031105
013400031105      * Cambia tariffa
013500031105     C                   if        TAMksc <> §ksc
013600081201     C                   eval      §ksc = TAMksc
013700031105     C                   eval      §flagA = *off
013800031105     C                   eval      §flagB = *off
013900031105     C                   eval      §flagC = *off
014000031105     C                   endif
014100031105     C                   if        TAMctr <> §ctr
014200031105     C                   eval      §ctr = TAMctr
014300031105     C                   eval      §flagA = *off
014400031105     C                   eval      §flagB = *off
014500031105     C                   eval      §flagC = *off
014600031105     C                   endif
014700031105
014800061128      * valorizzo i campi dei flag operativi
014900061128     c                   movel     tamflo        dsta01
015000061128
015100111209      * Decorrenza maggiore/uguale 01/01/2012
015200111209    1C                   if        TAMddt >= 20120101
015300081201     c                   clear                   up_tam
015400111209     C                   eval      §flagA = (TAMddt = 20120101)
015500031105     C                   eval      §flagD = *off
015600111209    2C                   if        tamddt <> 20120101 and tamdst <> 20120101
015700111215      * solo se non è il progressivo  0
015800111215     c                   If        tamprg > 0
015900031105     C                   exsr      Sr_CercaDopo
016000111215     c                   endif
016100081201    3C                   if        §flagD = *on
016200081201     c                   eval      up_tam = *on
016300031105     C                   eval      kprg = TAMprg
016400031105     C                   exsr      Sr_Aggiorna
016500031105     C                   eval      TAMprg = (TAMprg + 1)
016600081201    3C                   endif
016700081201    2C                   endif
016800081201      * se esiste % anni corrente faccio l'inglobamento
016900081201    2c                   If        tamppa > 0
017000051021      * se esiste % anni precedente sommiamo le 2 percentuali
017100081201    3c                   If        tampag > 0
017200051109     c                   eval      caladd = tampag + tamppa +
017300051109     c                                      ((tampag *tamppa)/100)
017400051109     c                   z-add(h)  caladd        tampag
017500051021     c                   else
017600051021     c                   eval      tampag = tamppa
017700081201    3c                   endif
017800051021     c                   eval      tamppa = 0
017900081201     c                   eval      up_tam = *on
018000081201    2c                   endif
018100051021      * se devo aumentare
018200081202    2c                   If        W_wfafauc = 'A'
018300081201     c                   eval      TAMppa = perce
018400081201     c                   eval      up_tam = *on
018500081201    2c                   endif
018600081201      * se devo aggiornare l'Istat
018700101129    2c                   If        W_wfafauc = 'I' and tamrct = 0
018800101129     c                   eval      TAMrct = ISTAT
018900081201     c                   eval      TAMpri = 100
019000081201     c                   eval      up_tam = *on
019100081201    2c                   endif
019200081201      * aggiorno TNTAM anche se cambiato progressivo
019300081201    2c                   If        up_tam = *on
019400031105     C                   eval      TAMduv = §oggi
019500031105     C                   eval      TAMftr = *blanks
019600041116     C                   eval      TAMdtr = §oggi
019700031105     C                   update    tntam04
019800081201      * aggiorno il progressivo vecchio con quello nuovo
019900081201     c     kaut          chain     wfaut01l
020000081201    3c                   if        %found(wfaut01l)
020100081201     c                   eval      wfaprgn = tamprg
020200081201     c                   eval      wfaddtn = tamddt
020300081201     c                   eval      wfadstn = tamdst
020400101129    4c                   if        tamrct <> 0
020500081209     c                   eval      wfaistn = 'S'
020600081201     c                   else
020700081209     c                   eval      wfaistn = 'N'
020800081201    4c                   endif
020900081201     c                   eval      wfapagn = tampag
021000081201     c                   eval      wfappan = tamppa
021100081202     c                   eval      wfaela = 'S'
021200081201     c                   update    wfaut000
021300081201    3C                   endif
021400081201    2C                   endif
021500061207
021600081201    1c                   endif
021700031105
021800111209      * Scadenza maggiore/uguale 01/01/2012 e decorrenza minore 01/01/2012
021900111209    1C                   if        TAMdst >= 20120101 and TAMddt < 20120101
022000031105     C                   eval      §flagB = *on
022100031105     C                   eval      §dst = TAMdst
022200031105     C                   eval      kprg = TAMprg
022300051021      * verifico se devo aggiornare la tariffa  o per integrazione o per aumento altrimenti
022400051021      * non faccio nulla
022500081201     c                   clear                   Up_tam
022600081201      * deleta i record annullati del progressivo successivo
022700031105     C                   exsr      Sr_Annulla
022800081201      * scrivo il nuovo progressivo del dettaglio a cavallo d'anno
022900031105     C                   exsr      Sr_Scrivi
023000031105     C                   eval      §prg = (kprg +1)
023100051024     C                   eval      §ppa = New_TAMppa
023200051104     C                   eval      §pag = New_TAMpag
023300111209     C                   eval      TAMdst = 20111231
023400031105     C                   eval      TAMduv = §oggi
023500031105     C                   eval      TAMftr = *blanks
023600041116     C                   eval      TAMdtr = §oggi
023700031105     C                   update    tntam04
023800111213      * --> SCRIVO TITAV
023900111213     c                   Clear                   Titav000
024000111213     c                   Eval      tavksc = tamksc
024100111213     c                   Eval      tavctr = tamctr
024200111213     c                   Eval      tavprg = tamprg
024300111213     c                   Eval      tavtrd = 'TES'
024400111213     c                   Eval      tavcta = 'D'
024500111213     c                   Eval      tavdav = *date
024600111213     c                   Time                    w0140
024700111213     c                   Movel     w0140         tavorv
024800111213     c                   Eval      tavnoj = 'AUMENTO '
024900111213     c                   Eval      tavpru = 'BATCH'
025000111213     c                   Eval      tavdtr = *date
025100111213     c                   Write     Titav000
025200031105     C                   iter
025300051021     C                   endif
025400031105
025500111209      * Scadenza minore/uguale 31/12/2011
025600111209     C                   if        TAMdst <= 20111231
025700031105     C                   eval      §flagC = *on
025800051103      *
025900111209     C                   eval      §dst = 20121231
026000031105     C                   eval      kprg = TAMprg
026100031105     C                   exsr      Sr_Annulla
026200031105     C                   exsr      Sr_Scrivi
026300031105     C                   endif
026400051021
026500031105     C                   enddo
026600081201     C
026700081201     C/EXEC SQL
026800081201     C+ Fetch C1 into :W_wfatcl , :W_wfaclf , :W_wfafauc
026900081201     C/END-EXEC
027000081201     C*
027100081201     C                   enddo
027200051021
027300081201     C*
027400081201     C/EXEC SQL
027500081201     C+ close C1
027600081201     C/END-EXEC
027700081201     C
027800081201
027900081201     c                   seton                                        lr
028000081201
028100031105      *****************************************************************
028200031105      * CERCA LA TARIFFA SUCESSIVA E CONTROLLA SE A CAVALLO D'ANNO
028300031105      *****************************************************************
028400031105     C     Sr_CercaDopo  BEGSR
028500031105
028600031105     C                   eval      kprg = (TAMprg - 1)
028700111209     c                   do        *hival
028800031105     C     ktam          chain(n)  tntam01l
028900031105     C                   if        %found(tntam01l)
029000111209     C                             and old_TAMdst >= 20120101
029100111209     C                             and old_TAMddt < 20120101
029200081201      * valorizzo il falg D perchè comunque sia devo sempre scrivere un nuovo progressivo
029300031105     C                   eval      §flagD = *on
029400111209     c                   leave
029500111209
029600111209     C                   endif
029700111209     c                   if        kprg =  0
029800111209     c                   leave
029900111209     c                   endif
030000111209     c                   eval      kprg = (kprg - 1)
030100111209     c                   enddo
030200111209
030300031105
030400031105     C                   endsr
030500031105      *****************************************************************
030600031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
030700031105      *****************************************************************
030800031105     C     Sr_Aggiorna   BEGSR
030900031105
031000031105     C     ktam          setll     titad04l
031100031105     C                   do        *hival
031200031105     C     ktam          reade     titad04l
031300031105     C                   if        %eof(titad04l)
031400031105     C                   leave
031500031105     C                   endif
031600031105     C                   if        TADatb <> *blanks
031700031105     C                   iter
031800031105     C                   endif
031900031105     C                   eval      TADprg = (TADprg + 1)
032000031105     C                   eval      TADftr = *blanks
032100041116     C                   eval      TADdtr = §oggi
032200031105     C                   update    titad04
032300031105     C                   enddo
032400031105
032500031105     C     ktam          setll     titpt01l
032600031105     C                   do        *hival
032700031105     C     ktam          reade     titpt01l
032800031105     C                   if        %eof(titpt01l)
032900031105     C                   leave
033000031105     C                   endif
033100031105     C                   if        TPTatb <> *blanks
033200031105     C                   iter
033300031105     C                   endif
033400031105     C                   eval      TPTprg = (TPTprg + 1)
033500031105     C                   eval      TPTduv = §oggi
033600031105     C                   eval      TPTftr = *blanks
033700061128     C                   eval      TPTdtr = §oggi
033800031105     C                   update    titpt01
033900031105     C                   enddo
034000031105
034100031105     C     ktam          setll     titpd01l
034200031105     C                   do        *hival
034300031105     C     ktam          reade     titpd01l
034400031105     C                   if        %eof(titpd01l)
034500031105     C                   leave
034600031105     C                   endif
034700031105     C                   if        TPDatb <> *blanks
034800031105     C                   iter
034900031105     C                   endif
035000031105     C                   eval      TPDprg = (TPDprg + 1)
035100031105     C                   eval      TPDftr = *blanks
035200041116     C                   eval      TPDdtr = §oggi
035300031105     C                   update    titpd01
035400031105     C                   enddo
035500031105
035600031105     C     ktam          setll     titgc01l
035700031105     C                   do        *hival
035800031105     C     ktam          reade     titgc01l
035900031105     C                   if        %eof(titgc01l)
036000031105     C                   leave
036100031105     C                   endif
036200031105     C                   if        TGCatb <> *blanks
036300031105     C                   iter
036400031105     C                   endif
036500031105     C                   eval      TGCprg = (TGCprg + 1)
036600031105     C                   eval      TGCduv = §oggi
036700031105     C                   eval      TGCftr = *blanks
036800041116     C                   eval      TGCdtr = §oggi
036900031105     C                   update    titgc01
037000031105     C                   enddo
037100031105
037200031105     C                   endsr
037300031105      *****************************************************************
037400031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
037500031105      *****************************************************************
037600031105     C     Sr_Annulla    BEGSR
037700031105
037800031105     C                   eval      kprg = (kprg + 1)
037900031105
038000031105     C     ktam          setll     titad04l
038100031105     C                   do        *hival
038200031105     C     ktam          reade     titad04l
038300031105     C                   if        %eof(titad04l)
038400031105     C                   leave
038500031105     C                   endif
038600031105     C                   if        TADatb = *blanks
038700031105     C                   iter
038800031105     C                   endif
038900031105     C                   delete    titad04
039000031105     C                   enddo
039100031105
039200031105     C     ktam          setll     titpt01l
039300031105     C                   do        *hival
039400031105     C     ktam          reade     titpt01l
039500031105     C                   if        %eof(titpt01l)
039600031105     C                   leave
039700031105     C                   endif
039800031105     C                   if        TPTatb = *blanks
039900031105     C                   iter
040000031105     C                   endif
040100031105     C                   delete    titpt01
040200031105     C                   enddo
040300031105
040400031105     C     ktam          setll     titpd01l
040500031105     C                   do        *hival
040600031105     C     ktam          reade     titpd01l
040700031105     C                   if        %eof(titpd01l)
040800031105     C                   leave
040900031105     C                   endif
041000031105     C                   if        TPDatb = *blanks
041100031105     C                   iter
041200031105     C                   endif
041300031105     C                   delete    titpd01
041400031105     C                   enddo
041500031105
041600031105     C     ktam          setll     titgc01l
041700031105     C                   do        *hival
041800031105     C     ktam          reade     titgc01l
041900031105     C                   if        %eof(titgc01l)
042000031105     C                   leave
042100031105     C                   endif
042200031105     C                   if        TGCatb = *blanks
042300031105     C                   iter
042400031105     C                   endif
042500031105     C                   delete    titgc01
042600031105     C                   enddo
042700031105
042800031105     C     ktam          chain     tntam01l
042900031105     C                   if        %found(tntam01l)
043000031105     C                             and old_TAMatb <> *blanks
043100031105     C                   delete    tntam01
043200031105     C                   endif
043300031105
043400031105     C                   eval      kprg = (kprg - 1)
043500031105
043600031105     C                   endsr
043700031105      *****************************************************************
043800031105      * SCRIVE LE NUOVE TARIFFE
043900031105      *****************************************************************
044000031105     C     Sr_Scrivi     BEGSR
044100031105
044200081202     c                   eval      up_tam = *on
044300081202
044400031105     C     ktam          setll     titad04l
044500031105     C                   do        *hival
044600031105     C     ktam          reade     titad04l
044700031105     C                   if        %eof(titad04l)
044800031105     C                   leave
044900031105     C                   endif
045000031105     C                   if        TADatb <> *blanks
045100031105     C                   iter
045200031105     C                   endif
045300031105     C                   eval      new_TADksc = TADksc
045400031105     C                   eval      new_TADctr = TADctr
045500031105     C                   eval      new_TADprg = (kprg + 1)
045600031105     C                   eval      new_TADlnp = TADlnp
045700031105     C                   eval      new_TADcts = TADcts
045800031105     C                   eval      new_TADnaz = TADnaz
045900031105     C                   eval      new_TADcap = TADcap
046000031105     C                   eval      new_TADsgl = TADsgl
046100031105     C                   eval      new_TADitr = TADitr
046200031105     C                   eval      new_TADino = TADino
046300031105     C                   eval      new_TADrpv = TADrpv
046400031105     C                   eval      new_TADorp = TADorp
046500031105     C                   eval      new_TADmin = TADmin
046600031105     C                   eval      new_TADmax = TADmax
046700031105     C                   eval      new_TADftr = *blanks
046800041116     C                   eval      new_TADdtr = §oggi
046900031105     C                   write     titad000
047000031105     C                   enddo
047100031105
047200031105     C     ktam          setll     titpt01l
047300031105     C                   do        *hival
047400031105     C     ktam          reade     titpt01l
047500031105     C                   if        %eof(titpt01l)
047600031105     C                   leave
047700031105     C                   endif
047800031105     C                   if        TPTatb <> *blanks
047900031105     C                   iter
048000031105     C                   endif
048100031105     C                   eval      new_TPTksc = TPTksc
048200031105     C                   eval      new_TPTctr = TPTctr
048300031105     C                   eval      new_TPTprg = (kprg + 1)
048400031105     C                   eval      new_TPTduv = §oggi
048500031105     C                   eval      new_TPTftc = TPTftc
048600031105     C                   eval      new_TPTtpg = TPTtpg
048700031105     C                   eval      new_TPTarl = TPTarl
048800031105     C                   eval      new_TPTarf = TPTarf
048900031105     C                   eval      new_TPTaro = TPTaro
049000031105     C                   eval      new_TPTrpv = TPTrpv
049100031105     C                   eval      new_TPTvlm = TPTvlm
049200031105     C                   eval      new_TPTvvm = TPTvvm
049300031105     C                   eval      new_TPTfvm = TPTfvm
049400031105     C                   eval      new_TPTtma = TPTtma
049500031105     C                   eval      new_TPTapl = TPTapl
049600031105     C                   eval      new_TPTftr = *blanks
049700041116     C                   eval      new_TPTdtr = §oggi
049800061128     C                   eval      new_TPTdpb = TPTdpb
049900061128     C                   eval      new_TPTdit = TPTdit
050000061128     C                   eval      new_TPTflo = TPTflo
050100031105     C                   write     titpt000
050200031105     C                   enddo
050300031105
050400031105     C     ktam          setll     titpd01l
050500031105     C                   do        *hival
050600031105     C     ktam          reade     titpd01l
050700031105     C                   if        %eof(titpd01l)
050800031105     C                   leave
050900031105     C                   endif
051000031105     C                   if        TPDatb <> *blanks
051100031105     C                   iter
051200031105     C                   endif
051300031105     C                   eval      new_TPDksc = TPDksc
051400031105     C                   eval      new_TPDctr = TPDctr
051500031105     C                   eval      new_TPDprg = (kprg + 1)
051600031105     C                   eval      new_TPDftc = TPDftc
051700031105     C                   eval      new_TPDcts = TPDcts
051800031105     C                   eval      new_TPDorp = TPDorp
051900031105     C                   eval      new_TPDsgl = TPDsgl
052000031105     C                   eval      new_TPDitr = TPDitr
052100031105     C                   eval      new_TPDmin = TPDmin
052200031105     C                   eval      new_TPDmax = TPDmax
052300031105     C                   eval      new_TPDain = TPDain
052400031105     C                   eval      new_TPDftr = *blanks
052500041116     C                   eval      new_TPDdtr = §oggi
052600031105     C                   write     titpd000
052700031105     C                   enddo
052800031105
052900031105     C     ktam          setll     titgc01l
053000031105     C                   do        *hival
053100031105     C     ktam          reade     titgc01l
053200031105     C                   if        %eof(titgc01l)
053300031105     C                   leave
053400031105     C                   endif
053500031105     C                   if        TGCatb <> *blanks
053600031105     C                   iter
053700031105     C                   endif
053800031105     C                   eval      new_TGCksc = TGCksc
053900031105     C                   eval      new_TGCctr = TGCctr
054000031105     C                   eval      new_TGCprg = (kprg + 1)
054100031105     C                   eval      new_TGCsgv = TGCsgv
054200031105     C                   eval      new_TGCsgr = TGCsgr
054300031105     C                   eval      new_TGCsgp = TGCsgp
054400031105     C                   eval      new_TGCsgd = TGCsgd
054500031105     C                   eval      new_TGCsg1 = TGCsg1
054600031105     C                   eval      new_TGCsg2 = TGCsg2
054700031105     C                   eval      new_TGCsg3 = TGCsg3
054800031105     C                   eval      new_TGCst1 = TGCst1
054900031105     C                   eval      new_TGCst2 = TGCst2
055000031105     C                   eval      new_TGCst3 = TGCst3
055100031105     C                   eval      new_TGCstm = TGCstm
055200031105     C                   eval      new_TGCrpv = TGCrpv
055300031105     C                   eval      new_TGCfaf = TGCfaf
055400031105     C                   eval      new_TGCsgf = TGCsgf
055500031105     C                   eval      new_TGCggr = TGCggr
055600031105     C                   eval      new_TGCtcm = TGCtcm
055700031105     C                   eval      new_TGCtfg = TGCtfg
055800031105     C                   eval      new_TGCduv = §oggi
055900031105     C                   eval      new_TGCftr = *blanks
056000041116     C                   eval      new_TGCdtr = §oggi
056100031105     C                   write     titgc000
056200031105     C                   enddo
056300031105
056400031105     C                   eval      new_TAMksc = TAMksc
056500031105     C                   eval      new_TAMctr = TAMctr
056600031105     C                   eval      new_TAMprg = (kprg + 1)
056700111209     C                   eval      new_TAMddt = 20120101
056800031105     C                   eval      new_TAMdst = §dst
056900031105     C                   eval      new_TAMduv = §oggi
057000031105     C                   eval      new_TAMdcv = TAMdcv
057100031105     C                   eval      new_TAMdif = TAMdif
057200031105     C                   eval      new_TAMfli = TAMfli
057300031105     C                   eval      new_TAMpri = TAMpri
057400031105     C                   eval      new_TAMfmp = TAMfmp
057500031105     C                   eval      new_TAMlrc = TAMlrc
057600031105     C                   eval      new_TAMlas = TAMlas
057700051021      * se % addizionale corrente maggiore di zero devo addizionarla a quella precedente
057800051021     c                   If        TAMppa > 0
057900051109     c                   eval      caladd = tampag + tamppa +
058000051109     c                                      ((tampag *tamppa)/100)
058100051109     c                   z-add(h)  caladd        new_TAMpag
058200051021     c                   eval      new_TAMppa = *zeros
058300051104
058400051104     c                   else
058500051104     c                   eval      new_TAMpag = TAMpag
058600081202     c                   eval      new_TAMppa = TAMppa
058700051021     c                   endif
058800051021      * se aumento
058900081202     c                   If        W_WFAfauc = 'A'
059000081201     C                   eval      new_TAMppa = perce
059100051021     c                   endif
059200051021
059300031105     C                   eval      new_TAMmpa = TAMmpa
059400031105     C                   eval      new_TAMpam = TAMpam
059500031105     C                   eval      new_TAMmpm = TAMmpm
059600031105     C                   eval      new_TAMtin = TAMtin
059700031105     C                   eval      new_TAMkpm = TAMkpm
059800031105     C                   eval      new_TAMarl = TAMarl
059900031105     C                   eval      new_TAMarf = TAMarf
060000031105     C                   eval      new_TAMaro = TAMaro
060100031105     C                   eval      new_TAMrat = TAMrat
060200031105     C                   eval      new_TAMrct = TAMrct
060300101129      * se devo aggiornare l'Istat
060400101129    2c                   If        W_wfafauc = 'I' and tamrct = 0
060500101129     c                   eval      new_TAMrct = ISTAT
060600101129     c                   eval      new_TAMpri = 100
060700101129    2c                   endif
060800101129
060900031105     C                   eval      new_TAMsc2 = TAMsc2
061000031105     C                   eval      new_TAMeds = TAMeds
061100031105     C                   eval      new_TAMars = TAMars
061200031105     C                   eval      new_TAMata = TAMata
061300031105     C                   eval      new_TAMftp = TAMftp
061400031105     C                   eval      new_TAMbap = TAMbap
061500031105     C                   eval      new_TAMtsp = TAMtsp
061600031105     C                   eval      new_TAMfie = TAMfie
061700031105     C                   eval      new_TAMfis = TAMfis
061800031105     C                   eval      new_TAMsis = TAMsis
061900031105     C                   eval      new_TAMfvd = TAMfvd
062000031105     C                   eval      new_TAMfts = TAMfts
062100031105     C                   eval      new_TAMnas = TAMnas
062200031105     C                   eval      new_TAMnot = TAMnot
062300031105     C                   eval      new_TAMftm = TAMftm
062400031105     C                   eval      new_TAMgca = TAMgca
062500031105     C                   eval      new_TAMgga = TAMgga
062600031105     C                   eval      new_TAMgma = TAMgma
062700031105     C                   eval      new_TAMgva = TAMgva
062800031105     C                   eval      new_TAMbam = TAMbam
062900031105     C                   eval      new_TAMtpr = TAMtpr
063000031105     C                   eval      new_TAMflo = TAMflo
063100031105     C                   eval      new_TAMdat = TAMdat
063200031105     C                   eval      new_TAMftr = *blanks
063300041116     C                   eval      new_TAMdtr = §oggi
063400031105     C                   write     tntam000
063500111213      * --> SCRIVO TITAV
063600111213     c                   Clear                   Titav000
063700111213     c                   Eval      tavksc = New_tamksc
063800111213     c                   Eval      tavctr = New_tamctr
063900111213     c                   Eval      tavprg = New_tamprg
064000111213     c                   Eval      tavtrd = 'TES'
064100111213     c                   Eval      tavcta = 'D'
064200111213     c                   Eval      tavdav = *date
064300111213     c                   Time                    w0140
064400111213     c                   Movel     w0140         tavorv
064500111213     c                   Eval      tavnoj = 'AUMENTO '
064600111213     c                   Eval      tavpru = 'BATCH'
064700111213     c                   Eval      tavdtr = *date
064800111213     c                   Write     Titav000
064900081201      * aggiorno il progressivo vecchio con quello nuovo
065000081201     c                   If        Up_tam = *on
065100081201     c     kaut          chain     wfaut01l
065200081201    3c                   if        %found(wfaut01l)
065300081201     c                   eval      wfaddtn = new_tamddt
065400081201     c                   eval      wfadstn = new_tamdst
065500081201     c                   eval      wfaprgn = new_tamprg
065600101129    4c                   if        new_tamrct <> 0
065700081209     c                   eval      wfaistn = 'S'
065800081201     c                   else
065900081209     c                   eval      wfaistn = 'N'
066000081201    4c                   endif
066100081201     c                   eval      wfapagn = new_tampag
066200081201     c                   eval      wfappan = new_tamppa
066300081202     c                   eval      wfaela = 'S'
066400081201     c                   update    wfaut000
066500081201    3C                   endif
066600081201    2C                   endif
066700031105
066800031105     C                   endsr
066900031105      *****************************************************************
067000031105      * ROUTINE INIZIALE
067100031105      *****************************************************************
067200031105     C     *INZSR        BEGSR
067300031105
067400031105     C     *ENTRY        PLIST
067500031105     C                   PARM                    KPJBA
067600031105      *
067700031105     c
067800031105     C     Ktam          klist
067900031105     C                   kfld                    TAMksc
068000031105     C                   kfld                    TAMctr
068100031105     C                   kfld                    Kprg
068200031105
068300081201      *
068400081201     c
068500081201     C     Kaut          klist
068600081201     C                   kfld                    AU_Tipo
068700081201     C                   kfld                    AU_Cliente
068800081201     C                   kfld                    AU_Tariffa
068900081201     C                   kfld                    AU_Progres
069000081201
069100081201      *
069200081201     c     keytab        klist
069300081201     c                   kfld                    codut
069400081201     c                   kfld                    kcod
069500081201     c                   kfld                    kkey
069600081201      *
069700031105      * reperisce data del giorno
069800031105     C                   TIME                    W0140
069900031105      * UDATE IN GGMMAAAA
070000031105     C                   MOVE      W0140         WDTGIO
070100031105      * UDATE IN AAAAMMGG
070200031105     C                   Z-ADD     WDTGIO        G02DAT
070300031105     C                   MOVEL     *BLANK        G02ERR
070400031105     C                   CALL      'XSRDA8'
070500031105     C                   PARM                    WLBDAT
070600031105     C                   Z-ADD     G02INV        §oggi
070700031105
070800081201      * cerco le percentuali di aumento
070900081201     c                   clear                   perce
071000111209     c                   move(p)   '2011'        kkey
071100081201     c                   movel(p)  '%A'          kcod
071200081201     c     Keytab        chain     tabel00f
071300081201     c                   If        %found(tabel00f)
071400081201     c                   movel     tbluni        perce
071500081201     c                   endif
071600031105     C                   endsr
