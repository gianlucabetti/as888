000100091203     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200031105
000300031105      *****************************************************************
000400110224      *       AGGIunta tariffa FUEL
000500031105      *****************************************************************
000600110224
000700110225     FWFfua00F  iF   E             DISK    rename(wffua00f:wffua000)
000800110224     FTNTAM04L  UF   E           K DISK    rename(tntam000:tntam04)
000900031105     FTITAD04L  UF   E           K DISK    rename(titad000:titad04)
001000031105     FTITPT01L  UF   E           K DISK    rename(titpt000:titpt01)
001100031105     FTITPD01L  UF   E           K DISK    rename(titpd000:titpd01)
001200031105     FTITGC01L  UF   E           K DISK    rename(titgc000:titgc01)
001300031105
001400031105     FTNTAM01L  UF   E           K DISK    rename(tntam000:tntam01)
001500031105     F                                     prefix(old_)
001600031105
001700031105     FTNTAM00F  O    E             DISK    prefix(new_)
001800031105     FTITAD00F  O    E             DISK    prefix(new_)
001900031105     FTITPT00F  O    E             DISK    prefix(new_)
002000031105     FTITPD00F  O    E             DISK    prefix(new_)
002100031105     FTITGC00F  O    E             DISK    prefix(new_)
002200031105
002300081201     FTabel00f  if   e           k DISK
002400110224     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002500031105      *------------------------------------------------------------------------*
002600081128
002700031105     D Kprg            S                   LIKE(TAMprg)
002800031105     D §ksc            S                   LIKE(TAMksc)
002900031105     D §ctr            S                   LIKE(TAMctr)
003000031105     D §prg            S                   LIKE(TAMprg)
003100051024     D §ppa            S                   LIKE(TAMppa)
003200051104     D §pag            S                   LIKE(TAMpag)
003300031105     D §dst            S                   LIKE(TAMdst)
003400081201     D Kcod            S                   like(TBLCOD)
003500081201     D Kkey            S                   like(TBLKEY)
003600110224     D Kftc            S                   like(tptftc)
003700081201     D Codut           s              1  0 inz(1)
003800081201
003900031105     D W0140           S             14  0
004000031105     D Wdtgio          S              8  0
004100031105     D §oggi           S              8  0
004200051109     D caladd          s             30  9
004300031105     D §flagA          S              1
004400031105     D §flagB          S              1
004500031105     D §flagC          S              1
004600031105     D §flagD          S              1
004700081201     D Up_tam          S              1
004800031105
004900031105      *   D S   I N T E R N E / E S T E R N E
005000031105
005100031105     D WLBDAT          DS                  INZ
005200031105     D  G02DAT                 1      8  0
005300031105     D  G02INV                 9     16  0
005400031105     D  G02ERR                17     17
005500031105     D  G02TGI                18     22  0
005600031105
005700061128     D Dsta01        e ds
005800031105     D kpjba         e ds
005900031105
006000081128      *------------------------------------------------------------------------*
006100110225     c                   except    testa
006200110225     c
006300110225     c                   read      wffua000
006400081128     C*
006500110225     C                   dow       not %eof(wffua00f)
006600031105      *
006700110225     c                   clear                   wprima            1 0
006800031105      * Aggancio Tntam04l
006900110225     c     ktam4         setll     tntam04l
007000110225    1c                   do        *hival
007100110225     c     ktam4         reade     tntam04l
007200031105      *
007300031105      * fine file
007400110225    2c                   if        %eof(tntam04l)
007500110225     c*  Se prima lettura stampo errore per non trovata tarifffa
007600110225     c                   if        Wprima=0
007700110228     c                   except    nontrov
007800110225     c                   endif
007900110225     c
008000031105     c                   leave
008100110225    2c                   endif
008200110225     c
008300110225     c* Se prima c'e' stato errore per tariffa ancora da decorrere e
008400110225     c*  c'e' un altro progressivo in decorrenza uguale per ksc e ctr
008500110225     c* do di nuovo errore
008600110225    2c                   if        §ksc=tamksc and §ctr=tamctr and
008700110225     c                             err='1'
008800110225     c                   if        tamdst<20110301
008900110225     c                   leave
009000110225     c                   else
009100110225     c***                except    errdec
009200110225     c                   iter
009300110225    2c                   endif
009400110225    2c                   endif
009500110225     c
009600110225     C                   eval      §ksc = TAMksc
009700110225     C                   eval      §ctr = TAMctr
009800110225     c                   clear                   err               1
009900110224     c
010000110224     c* se ancora da decorrere, --> stampo e non aggiorno
010100110225    2c                   if        tamddt>=20110301
010200110225     c****               except    errdec
010300110225     c                   eval      err='1'
010400110225    2c                   endif
010500081201
010600110224     c* Se già presente tariffa FUEL --> non aggiorno
010700110225    2c                   if        err=' '
010800110224
010900110224     c                   eval      kprg=tamprg
011000110224     c                   eval      kftc='f'
011100110224     c     ktpt          chain     titpt01l
011200110225     c
011300110225    3c                   if        %found(titpt01l) and tptatb= ' '
011400110225     c     ktpt          setll     titpd01l
011500110225     c     ktpt          reade     titpd01l
011600110225     c
011700110225    4c                   dow       not %eof(titpd01l) and err=' '
011800110225     c* se trovato uno scagione pieno --> stampo errore
011900110225    5c                   if        tpdatb=' ' and tpditr>0
012000110225     c                   eval      err='2'
012100110225     c                   else
012200110225     c     ktpt          reade     titpd01l
012300110225    5c                   endif
012400110225    4c                   enddo
012500110225     c
012600110225    4c                   if        err='2'
012700110224     c                   except    errpres
012800110225     c                   leave
012900110225    4c                   endif
013000110225     c
013100110225    3c                   endif
013200110225    2c                   endif
013300110224     c
013400110225     c                   eval      dsta01=tamflo
013500110224
013600110225     c* Se tariffe FEDEX  fuel non previsto
013700110225    2c                   if        err=' '
013800110225    3c                   if        §tafed='S'
013900110224     c                   except    errfed
014000110224     c                   eval      err='1'
014100110225     c                   leave
014200110225    3c                   endif
014300110225    2c                   endif
014400110224     c
014500110225    2c                   if        err=' '
014600110225     c***                except    tariffa
014700110224     c
014800031105      * Cambia tariffa
014900110225    3C                   if        TAMksc <> §ksc
015000081201     C                   eval      §ksc = TAMksc
015100031105     C                   eval      §flagA = *off
015200031105     C                   eval      §flagB = *off
015300031105     C                   eval      §flagC = *off
015400110225    3C                   endif
015500110225    3C                   if        TAMctr <> §ctr
015600031105     C                   eval      §ctr = TAMctr
015700031105     C                   eval      §flagA = *off
015800031105     C                   eval      §flagB = *off
015900031105     C                   eval      §flagC = *off
016000110225    3C                   endif
016100061128
016200110224      * Decorrenza maggiore/uguale 01/03/2011   --> per ora non gestito
016300110225    3C                   if        TAMddt >= 20110301
016400081201     c                   clear                   up_tam
016500101129     C                   eval      §flagA = (TAMddt = 20110101)
016600031105     C                   eval      §flagD = *off
016700101129    2C                   if        tamddt <> 20110101 and tamdst <> 20110101
016800110224     C******             exsr      Sr_CercaDopo
016900081201    3C                   if        §flagD = *on
017000081201     c                   eval      up_tam = *on
017100031105     C                   eval      kprg = TAMprg
017200110224     C******             exsr      Sr_Aggiorna
017300031105     C                   eval      TAMprg = (TAMprg + 1)
017400081201    3C                   endif
017500081201    2C                   endif
017600081201      * aggiorno TNTAM anche se cambiato progressivo
017700081201    2c                   If        up_tam = *on
017800031105     C                   eval      TAMduv = §oggi
017900031105     C                   eval      TAMftr = *blanks
018000041116     C                   eval      TAMdtr = §oggi
018100110224     C*******            update    tntam04
018200081201      * aggiorno il progressivo vecchio con quello nuovo
018300110225     c                   endif
018400110225    3c                   endif
018500031105
018600110224      * Scadenza maggiore/uguale 01/03/2011 e decorrenza minore 01/03/2011
018700110224     c                   select
018800110225    3C                   when      TAMdst >= 20110301 and TAMddt < 20110301
018900031105     C                   eval      §flagB = *on
019000031105     C                   eval      §dst = TAMdst
019100031105     C                   eval      kprg = TAMprg
019200051021      * verifico se devo aggiornare la tariffa  o per integrazione o per aumento altrimenti
019300051021      * non faccio nulla
019400081201     c                   clear                   Up_tam
019500081201      * deleta i record annullati del progressivo successivo
019600031105     C                   exsr      Sr_Annulla
019700031105     C                   exsr      Sr_Scrivi
019800110224     c
019900110224     C                   eval      TAMdst = 20110228
020000031105     C                   eval      TAMduv = §oggi
020100031105     C                   eval      TAMftr = *blanks
020200041116     C                   eval      TAMdtr = §oggi
020300110225     c                   if        Waggio=' '
020400031105     C                   update    tntam04
020500110225     c                   endif
020600110225     c****               except    stampaupd
020700031105
020800110225      * Scadenza minore/uguale 28/02/2011
020900110225    3C                   when      TAMdst <= 20110228
021000031105     C                   eval      §flagC = *on
021100051103      *
021200110224     C                   eval      §dst = 20110331
021300031105     C                   eval      kprg = TAMprg
021400031105     C                   exsr      Sr_Annulla
021500031105     C                   exsr      Sr_Scrivi
021600110224     c
021700110224     C                   eval      TAMdst = 20110228
021800110224     C                   eval      TAMduv = §oggi
021900110224     C                   eval      TAMftr = *blanks
022000110224     C                   eval      TAMdtr = §oggi
022100110225     c                   if        Waggio=' '
022200110224     C                   update    tntam04
022300110225     c                   endif
022400110225     c****               except    stampaupd
022500110225    3C                   endsl
022600110224     c
022700110224     c                   exsr      AggFuel
022800110225     c                   leave
022900110225     c
023000110225    2c                   endif
023100110225    1C                   enddo
023200110225     c
023300110225     c                   read      wffua000
023400110225    1C                   enddo
023500081201     C
023600081201
023700081201     c                   seton                                        lr
023800081201
023900031105      *****************************************************************
024000031105      * CERCA LA TARIFFA SUCESSIVA E CONTROLLA SE A CAVALLO D'ANNO
024100031105      *****************************************************************
024200031105     C     Sr_CercaDopo  BEGSR
024300031105
024400031105     C                   eval      kprg = (TAMprg - 1)
024500031105     C     ktam          chain(n)  tntam01l
024600031105     C                   if        %found(tntam01l)
024700101129     C                             and old_TAMdst >= 20110101
024800101129     C                             and old_TAMddt < 20110101
024900081201      * valorizzo il falg D perchè comunque sia devo sempre scrivere un nuovo progressivo
025000031105     C                   eval      §flagD = *on
025100051021
025200031105     C                   endif
025300031105
025400031105     C                   endsr
025500031105      *****************************************************************
025600031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
025700031105      *****************************************************************
025800031105     C     Sr_Aggiorna   BEGSR
025900031105
026000031105     C     ktam          setll     titad04l
026100031105     C                   do        *hival
026200031105     C     ktam          reade     titad04l
026300031105     C                   if        %eof(titad04l)
026400031105     C                   leave
026500031105     C                   endif
026600031105     C                   if        TADatb <> *blanks
026700031105     C                   iter
026800031105     C                   endif
026900031105     C                   eval      TADprg = (TADprg + 1)
027000031105     C                   eval      TADftr = *blanks
027100041116     C                   eval      TADdtr = §oggi
027200110225     c                   if        Waggio=' '
027300031105     C                   update    titad04
027400110225     c                   endif
027500031105     C                   enddo
027600031105
027700031105     C     ktam          setll     titpt01l
027800031105     C                   do        *hival
027900031105     C     ktam          reade     titpt01l
028000031105     C                   if        %eof(titpt01l)
028100031105     C                   leave
028200031105     C                   endif
028300031105     C                   if        TPTatb <> *blanks
028400031105     C                   iter
028500031105     C                   endif
028600031105     C                   eval      TPTprg = (TPTprg + 1)
028700031105     C                   eval      TPTduv = §oggi
028800031105     C                   eval      TPTftr = *blanks
028900061128     C                   eval      TPTdtr = §oggi
029000110225     c                   if        Waggio=' '
029100031105     C                   update    titpt01
029200110225     c                   endif
029300031105     C                   enddo
029400031105
029500031105     C     ktam          setll     titpd01l
029600031105     C                   do        *hival
029700031105     C     ktam          reade     titpd01l
029800031105     C                   if        %eof(titpd01l)
029900031105     C                   leave
030000031105     C                   endif
030100031105     C                   if        TPDatb <> *blanks
030200031105     C                   iter
030300031105     C                   endif
030400031105     C                   eval      TPDprg = (TPDprg + 1)
030500031105     C                   eval      TPDftr = *blanks
030600041116     C                   eval      TPDdtr = §oggi
030700110225     c                   if        Waggio=' '
030800031105     C                   update    titpd01
030900110225     c                   endif
031000031105     C                   enddo
031100031105
031200031105     C     ktam          setll     titgc01l
031300031105     C                   do        *hival
031400031105     C     ktam          reade     titgc01l
031500031105     C                   if        %eof(titgc01l)
031600031105     C                   leave
031700031105     C                   endif
031800031105     C                   if        TGCatb <> *blanks
031900031105     C                   iter
032000031105     C                   endif
032100031105     C                   eval      TGCprg = (TGCprg + 1)
032200031105     C                   eval      TGCduv = §oggi
032300031105     C                   eval      TGCftr = *blanks
032400041116     C                   eval      TGCdtr = §oggi
032500110225     c                   if        Waggio=' '
032600031105     C                   update    titgc01
032700110225     c                   endif
032800031105     C                   enddo
032900031105
033000031105     C                   endsr
033100031105      *****************************************************************
033200031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
033300031105      *****************************************************************
033400031105     C     Sr_Annulla    BEGSR
033500031105
033600031105     C                   eval      kprg = (kprg + 1)
033700031105
033800031105     C     ktam          setll     titad04l
033900031105     C                   do        *hival
034000031105     C     ktam          reade     titad04l
034100031105     C                   if        %eof(titad04l)
034200031105     C                   leave
034300031105     C                   endif
034400031105     C                   if        TADatb = *blanks
034500031105     C                   iter
034600031105     C                   endif
034700110225     c                   if        Waggio=' '
034800031105     C                   delete    titad04
034900110225     C                   endif
035000031105     C                   enddo
035100031105
035200031105     C     ktam          setll     titpt01l
035300031105     C                   do        *hival
035400031105     C     ktam          reade     titpt01l
035500031105     C                   if        %eof(titpt01l)
035600031105     C                   leave
035700031105     C                   endif
035800031105     C                   if        TPTatb = *blanks
035900031105     C                   iter
036000031105     C                   endif
036100110225     c                   if        Waggio=' '
036200031105     C                   delete    titpt01
036300110225     c                   endif
036400031105     C                   enddo
036500031105
036600031105     C     ktam          setll     titpd01l
036700031105     C                   do        *hival
036800031105     C     ktam          reade     titpd01l
036900031105     C                   if        %eof(titpd01l)
037000031105     C                   leave
037100031105     C                   endif
037200031105     C                   if        TPDatb = *blanks
037300031105     C                   iter
037400031105     C                   endif
037500110225     c                   if        Waggio=' '
037600031105     C                   delete    titpd01
037700110225     c                   endif
037800031105     C                   enddo
037900031105
038000031105     C     ktam          setll     titgc01l
038100031105     C                   do        *hival
038200031105     C     ktam          reade     titgc01l
038300031105     C                   if        %eof(titgc01l)
038400031105     C                   leave
038500031105     C                   endif
038600031105     C                   if        TGCatb = *blanks
038700031105     C                   iter
038800031105     C                   endif
038900110225     c                   if        Waggio=' '
039000031105     C                   delete    titgc01
039100110225     c                   endif
039200031105     C                   enddo
039300031105
039400031105     C     ktam          chain     tntam01l
039500031105     C                   if        %found(tntam01l)
039600031105     C                             and old_TAMatb <> *blanks
039700110225     c                   if        Waggio=' '
039800031105     C                   delete    tntam01
039900110225     c                   endif
040000031105     C                   endif
040100031105
040200031105     C                   eval      kprg = (kprg - 1)
040300031105
040400031105     C                   endsr
040500031105      *****************************************************************
040600031105      * SCRIVE LE NUOVE TARIFFE
040700031105      *****************************************************************
040800031105     C     Sr_Scrivi     BEGSR
040900031105
041000081202     c                   eval      up_tam = *on
041100081202
041200031105     C     ktam          setll     titad04l
041300031105     C                   do        *hival
041400031105     C     ktam          reade     titad04l
041500031105     C                   if        %eof(titad04l)
041600031105     C                   leave
041700031105     C                   endif
041800031105     C                   if        TADatb <> *blanks
041900031105     C                   iter
042000031105     C                   endif
042100031105     C                   eval      new_TADksc = TADksc
042200031105     C                   eval      new_TADctr = TADctr
042300031105     C                   eval      new_TADprg = (kprg + 1)
042400031105     C                   eval      new_TADlnp = TADlnp
042500031105     C                   eval      new_TADcts = TADcts
042600031105     C                   eval      new_TADnaz = TADnaz
042700031105     C                   eval      new_TADcap = TADcap
042800031105     C                   eval      new_TADsgl = TADsgl
042900031105     C                   eval      new_TADitr = TADitr
043000031105     C                   eval      new_TADino = TADino
043100031105     C                   eval      new_TADrpv = TADrpv
043200031105     C                   eval      new_TADorp = TADorp
043300031105     C                   eval      new_TADmin = TADmin
043400031105     C                   eval      new_TADmax = TADmax
043500031105     C                   eval      new_TADftr = *blanks
043600041116     C                   eval      new_TADdtr = §oggi
043700110225
043800110225     c                   if        Waggio=' '
043900110225     C                   write     titad000
044000110225     c                   endif
044100110225
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
045300110225     c* non scrivo nemmeno tariffa FUEL
045400110225     c                   if        tptftc='f'
045500110225     C                   iter
045600110225     C                   endif
045700031105     C                   eval      new_TPTksc = TPTksc
045800031105     C                   eval      new_TPTctr = TPTctr
045900031105     C                   eval      new_TPTprg = (kprg + 1)
046000031105     C                   eval      new_TPTduv = §oggi
046100031105     C                   eval      new_TPTftc = TPTftc
046200031105     C                   eval      new_TPTtpg = TPTtpg
046300031105     C                   eval      new_TPTarl = TPTarl
046400031105     C                   eval      new_TPTarf = TPTarf
046500031105     C                   eval      new_TPTaro = TPTaro
046600031105     C                   eval      new_TPTrpv = TPTrpv
046700031105     C                   eval      new_TPTvlm = TPTvlm
046800031105     C                   eval      new_TPTvvm = TPTvvm
046900031105     C                   eval      new_TPTfvm = TPTfvm
047000031105     C                   eval      new_TPTtma = TPTtma
047100031105     C                   eval      new_TPTapl = TPTapl
047200031105     C                   eval      new_TPTftr = *blanks
047300041116     C                   eval      new_TPTdtr = §oggi
047400061128     C                   eval      new_TPTdpb = TPTdpb
047500061128     C                   eval      new_TPTdit = TPTdit
047600061128     C                   eval      new_TPTflo = TPTflo
047700110225     c                   if        Waggio=' '
047800031105     C                   write     titpt000
047900110225     c                   endif
048000031105     C                   enddo
048100031105
048200031105     C     ktam          setll     titpd01l
048300031105     C                   do        *hival
048400031105     C     ktam          reade     titpd01l
048500031105     C                   if        %eof(titpd01l)
048600031105     C                   leave
048700031105     C                   endif
048800031105     C                   if        TPDatb <> *blanks
048900031105     C                   iter
049000031105     C                   endif
049100110225     c* non scrivo nemmeno tariffa FUEL
049200110225     c                   if        tpdftc='f'
049300110225     C                   iter
049400110225     C                   endif
049500031105     C                   eval      new_TPDksc = TPDksc
049600031105     C                   eval      new_TPDctr = TPDctr
049700031105     C                   eval      new_TPDprg = (kprg + 1)
049800031105     C                   eval      new_TPDftc = TPDftc
049900031105     C                   eval      new_TPDcts = TPDcts
050000031105     C                   eval      new_TPDorp = TPDorp
050100031105     C                   eval      new_TPDsgl = TPDsgl
050200031105     C                   eval      new_TPDitr = TPDitr
050300031105     C                   eval      new_TPDmin = TPDmin
050400031105     C                   eval      new_TPDmax = TPDmax
050500031105     C                   eval      new_TPDain = TPDain
050600031105     C                   eval      new_TPDftr = *blanks
050700041116     C                   eval      new_TPDdtr = §oggi
050800110225     c                   if        Waggio=' '
050900031105     C                   write     titpd000
051000110225     c                   endif
051100031105     C                   enddo
051200031105
051300031105     C     ktam          setll     titgc01l
051400031105     C                   do        *hival
051500031105     C     ktam          reade     titgc01l
051600031105     C                   if        %eof(titgc01l)
051700031105     C                   leave
051800031105     C                   endif
051900031105     C                   if        TGCatb <> *blanks
052000031105     C                   iter
052100031105     C                   endif
052200031105     C                   eval      new_TGCksc = TGCksc
052300031105     C                   eval      new_TGCctr = TGCctr
052400031105     C                   eval      new_TGCprg = (kprg + 1)
052500031105     C                   eval      new_TGCsgv = TGCsgv
052600031105     C                   eval      new_TGCsgr = TGCsgr
052700031105     C                   eval      new_TGCsgp = TGCsgp
052800031105     C                   eval      new_TGCsgd = TGCsgd
052900031105     C                   eval      new_TGCsg1 = TGCsg1
053000031105     C                   eval      new_TGCsg2 = TGCsg2
053100031105     C                   eval      new_TGCsg3 = TGCsg3
053200031105     C                   eval      new_TGCst1 = TGCst1
053300031105     C                   eval      new_TGCst2 = TGCst2
053400031105     C                   eval      new_TGCst3 = TGCst3
053500031105     C                   eval      new_TGCstm = TGCstm
053600031105     C                   eval      new_TGCrpv = TGCrpv
053700031105     C                   eval      new_TGCfaf = TGCfaf
053800031105     C                   eval      new_TGCsgf = TGCsgf
053900031105     C                   eval      new_TGCggr = TGCggr
054000031105     C                   eval      new_TGCtcm = TGCtcm
054100031105     C                   eval      new_TGCtfg = TGCtfg
054200031105     C                   eval      new_TGCduv = §oggi
054300031105     C                   eval      new_TGCftr = *blanks
054400041116     C                   eval      new_TGCdtr = §oggi
054500110225     c                   if        Waggio=' '
054600031105     C                   write     titgc000
054700110225     c                   endif
054800031105     C                   enddo
054900031105
055000031105     C                   eval      new_TAMksc = TAMksc
055100031105     C                   eval      new_TAMctr = TAMctr
055200031105     C                   eval      new_TAMprg = (kprg + 1)
055300110224     C                   eval      new_TAMddt = 20110301
055400031105     C                   eval      new_TAMdst = §dst
055500031105     C                   eval      new_TAMduv = §oggi
055600031105     C                   eval      new_TAMdcv = TAMdcv
055700031105     C                   eval      new_TAMdif = TAMdif
055800031105     C                   eval      new_TAMfli = TAMfli
055900031105     C                   eval      new_TAMpri = TAMpri
056000031105     C                   eval      new_TAMfmp = TAMfmp
056100031105     C                   eval      new_TAMlrc = TAMlrc
056200031105     C                   eval      new_TAMlas = TAMlas
056300051104     c                   eval      new_TAMpag = TAMpag
056400081202     c                   eval      new_TAMppa = TAMppa
056500051021
056600031105     C                   eval      new_TAMmpa = TAMmpa
056700031105     C                   eval      new_TAMpam = TAMpam
056800031105     C                   eval      new_TAMmpm = TAMmpm
056900031105     C                   eval      new_TAMtin = TAMtin
057000031105     C                   eval      new_TAMkpm = TAMkpm
057100031105     C                   eval      new_TAMarl = TAMarl
057200031105     C                   eval      new_TAMarf = TAMarf
057300031105     C                   eval      new_TAMaro = TAMaro
057400031105     C                   eval      new_TAMrat = TAMrat
057500031105     C                   eval      new_TAMrct = TAMrct
057600101129
057700031105     C                   eval      new_TAMsc2 = TAMsc2
057800031105     C                   eval      new_TAMeds = TAMeds
057900031105     C                   eval      new_TAMars = TAMars
058000031105     C                   eval      new_TAMata = TAMata
058100031105     C                   eval      new_TAMftp = TAMftp
058200031105     C                   eval      new_TAMbap = TAMbap
058300031105     C                   eval      new_TAMtsp = TAMtsp
058400031105     C                   eval      new_TAMfie = TAMfie
058500031105     C                   eval      new_TAMfis = TAMfis
058600031105     C                   eval      new_TAMsis = TAMsis
058700031105     C                   eval      new_TAMfvd = TAMfvd
058800031105     C                   eval      new_TAMfts = TAMfts
058900031105     C                   eval      new_TAMnas = TAMnas
059000031105     C                   eval      new_TAMnot = TAMnot
059100031105     C                   eval      new_TAMftm = TAMftm
059200031105     C                   eval      new_TAMgca = TAMgca
059300031105     C                   eval      new_TAMgga = TAMgga
059400031105     C                   eval      new_TAMgma = TAMgma
059500031105     C                   eval      new_TAMgva = TAMgva
059600031105     C                   eval      new_TAMbam = TAMbam
059700031105     C                   eval      new_TAMtpr = TAMtpr
059800031105     C                   eval      new_TAMflo = TAMflo
059900031105     C                   eval      new_TAMdat = TAMdat
060000031105     C                   eval      new_TAMftr = *blanks
060100041116     C                   eval      new_TAMdtr = §oggi
060200110225     c                   if        Waggio=' '
060300031105     C                   write     tntam000
060400110225     c                   endif
060500110225     c
060600110225     c****               except    stampanew
060700031105
060800031105     C                   endsr
060900110224      *****************************************************************
061000110224      * Aggiunta FUEL
061100110224      *****************************************************************
061200110224     C     AGGFUEL       BEGSR
061300110224     c                   clear                   titpt000
061400110224     C                   eval      new_TPTksc = tamksc
061500110224     C                   eval      new_TPTctr = tamctr
061600110224     C                   eval      new_TPTprg = (kprg + 1)
061700110224     C                   eval      new_TPTduv = §oggi
061800110224     C                   eval      new_TPTftc = 'f'
061900110224     C                   eval      new_TPTtpg = 'V'
062000110224     C                   eval      new_TPTdtr = §oggi
062100110224     C                   eval      new_TPTdpb = 20110301
062200110225     C                   eval      new_TPTdit = 20110301
062300110225     c                   if        Waggio=' '
062400110224     C                   write     titpt000
062500110225     c                   endif
062600110224     c
062700110224     C                   eval      new_TPDksc = tamksc
062800110224     C                   eval      new_TPDctr = tamctr
062900110224     C                   eval      new_TPDprg = (kprg + 1)
063000110224     C                   eval      new_TPDftc = 'f'
063100110225     c                   if        tamfie='I'
063200110224     C                   eval      new_TPDcts = 'IT'
063300110224     C                   eval      new_TPDorp = 'A 9'
063400110224     c                   else
063500110224     C                   eval      new_TPDcts = 'EE'
063600110224     C                   eval      new_TPDorp = '999'
063700110224     c                   endif
063800110224     C                   eval      new_TPDsgl = 9999999999
063900110224     C                   eval      new_TPDitr = 0,3
064000110224     C                   eval      new_TPDftr = *blanks
064100110224     C                   eval      new_TPDdtr = §oggi
064200110225     c                   if        Waggio=' '
064300110224     C                   write     titpd000
064400110225     c                   endif
064500110225     c*****              except    newfuel
064600110224     c
064700110224     C                   endsr
064800031105      *****************************************************************
064900031105      * ROUTINE INIZIALE
065000031105      *****************************************************************
065100031105     C     *INZSR        BEGSR
065200031105
065300031105     C     *ENTRY        PLIST
065400031105     C                   PARM                    KPJBA
065500110225     c                   movel     kpjbu         WAGGIO            1
065600031105      *
065700031105     c
065800110225     C     Ktam          klist
065900110225     C                   kfld                    TAMksc
066000110225     C                   kfld                    TAMctr
066100110225     C                   kfld                    Kprg
066200110225
066300110224     C     Ktpt          klist
066400031105     C                   kfld                    TAMksc
066500031105     C                   kfld                    TAMctr
066600031105     C                   kfld                    Kprg
066700110224     C                   kfld                    Kftc
066800110225     c     ktam4         klist
066900110224     c                   kfld                    ccm
067000110224     c                   kfld                    ctr
067100081201      *
067200081201     c
067300081201      *
067400081201     c     keytab        klist
067500081201     c                   kfld                    codut
067600081201     c                   kfld                    kcod
067700081201     c                   kfld                    kkey
067800081201      *
067900031105      * reperisce data del giorno
068000031105     C                   TIME                    W0140
068100031105      * UDATE IN GGMMAAAA
068200031105     C                   MOVE      W0140         WDTGIO
068300031105      * UDATE IN AAAAMMGG
068400031105     C                   Z-ADD     WDTGIO        G02DAT
068500031105     C                   MOVEL     *BLANK        G02ERR
068600031105     C                   CALL      'XSRDA8'
068700031105     C                   PARM                    WLBDAT
068800031105     C                   Z-ADD     G02INV        §oggi
068900031105
069000031105     C                   endsr
069100110224     OPRTF198   E            TESTA            02
069200110224     O                                            8 'TAVRF2R '
069300110224     O                                           82 'Scrittura Tariffa FUEL'
069400110224     OPRTF198   E            ERRDEC      1
069500110224     o                       tamksc
069600110224     o                       tamctr           +   1
069700110224     o                       tamprg        z  +   1
069800110224     o                       tamfie           +   1
069900110224     o                       tamddt           +   1 '    /  /  '
070000110224     o                       tamdst           +   1 '    /  /  '
070100110225     o                                           49 ' '
070200110225     O                                        +   1 'ERR:Tariffa ancora da '
070300110224     O                                        +   0 'decorrere:sistemare a '
070400110224     O                                        +   0 'mano'
070500110224     OPRTF198   E            ERRpres     1
070600110224     o                       tamksc
070700110224     o                       tamctr           +   1
070800110224     o                       tamprg        z  +   1
070900110224     o                       tamfie           +   1
071000110224     o                       tamddt           +   1 '    /  /  '
071100110224     o                       tamdst           +   1 '    /  /  '
071200110225     o                                           49 ' '
071300110225     O                                        +   1 'ERR:Tariffa FUEL presente'
071400110225     OPRTF198   E            nontrov     1
071500110225     o                       ccm
071600110225     o                       ctr              +   1
071700110225     o                                           49 ' '
071800110225     O                                        +   1 'ERR:non trovata Tariffa'
071900110224     OPRTF198   E            ERRfed      1
072000110224     o                       tamksc
072100110224     o                       tamctr           +   1
072200110224     o                       tamprg        z  +   1
072300110224     o                       tamfie           +   1
072400110224     o                       tamddt           +   1 '    /  /  '
072500110224     o                       tamdst           +   1 '    /  /  '
072600110225     o                                           49 ' '
072700110225     O                                        +   1 'ERR:Tariffa FEDEX'
072800110225     OPRTF198   E            tariffa     1
072900110225     o                       tamksc
073000110225     o                       tamctr           +   1
073100110225     o                       tamprg        z  +   1
073200110225     o                       tamfie           +   1
073300110225     o                       tamddt           +   1 '    /  /  '
073400110225     o                       tamdst           +   1 '    /  /  '
073500110225     o                                           49 ' '
073600110225     O                                        +   1 'Tariffa da aggiornare '
073700110225     OPRTF198   E            stampanew   1
073800110225     O                                        +   0 '     '
073900110225     o                       new_tamksc       +   1
074000110225     o                       new_tamctr       +   1
074100110225     o                       new_tamprg    z  +   1
074200110225     o                       new_tamfie       +   1
074300110225     o                       new_tamddt       +   1 '    /  /  '
074400110225     o                       new_tamdst       +   1 '    /  /  '
074500110225     o                                           49 ' '
074600110225     O                                        +   1 'Nuovo progressivo creato'
074700110225     OPRTF198   E            stampaupd   1
074800110225     O                                        +   0 '     '
074900110225     o                       tamksc           +   1
075000110225     o                       tamctr           +   1
075100110225     o                       tamprg        z  +   1
075200110225     o                       tamfie           +   1
075300110225     o                       tamddt           +   1 '    /  /  '
075400110225     o                       tamdst           +   1 '    /  /  '
075500110225     o                                           49 ' '
075600110225     O                                        +   1 'Progressivo aggiornato'
075700110225     OPRTF198   E            newfuel     1
075800110225     O                                        +   0 '     '
075900110225     o                       new_tptksc       +   1
076000110225     o                       new_tptctr       +   1
076100110225     o                       new_tptprg    z  +   1
076200110225     o                       new_tptftc       +   1
076300110225     o                       new_tptdpb       +   1 '    /  /  '
076400110225     o                                           49 ' '
076500110225     O                                        +   1 'Fuel inserito'
