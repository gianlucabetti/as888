000100091203     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200031105
000300031105      *****************************************************************
000400110224      *       AGGIunta tariffa FUEL
000500031105      *****************************************************************
000600110224
000700110228     FWFfua00F  uF   E             DISK    rename(wffua00f:wffua000)
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
004800110228     D Err             S              1
004900110228     D Savaggio        S              1
005000110228     D general         S              9  0
005100031105
005200031105      *   D S   I N T E R N E / E S T E R N E
005300031105
005400031105     D WLBDAT          DS                  INZ
005500031105     D  G02DAT                 1      8  0
005600031105     D  G02INV                 9     16  0
005700031105     D  G02ERR                17     17
005800031105     D  G02TGI                18     22  0
005900031105
006000110228     D Dsta01        e ds
006100031105     D kpjba         e ds
006200031105
006300081128      *------------------------------------------------------------------------*
006400110225     c                   except    testa
006500110228     C/EXEC SQL
006600110228     C+ UPDATE  WFFUA00F   set FLG=' '
006700110228     C/END-EXEC
006800110228     c
006900110228     c                   eval      savaggio=Waggio
007000110228     c
007100110228     c* Primo giro--> cerco con CTR e PRG indicato: se trovo già il fuel flaggo
007200110228     c                   read      wffua00f
007300110228     c
007400110228    1c                   dow       not %eof(wffua00f)
007500110228     c     kfua          chain     tntam04l
007600110228    2c                   if        not %found(tntam04l)
007700110228     c                   except    nontrov2
007800110228     c                   add       1             totnot            5 0
007900110228   x2c                   else
008000110228     c* Verifico fuel
008100110228     c                   eval      waggio='N'
008200110228     c                   exsr      cercafuel
008300110228    3c                   if        wfusi='S'
008400110228     c                   add       1             totesf            5 0
008500110301     c* solo se decorrente non aggiorno, altrimenti se scaduta o da decorrere faccio lo
008600110301     c*  stesso il controllo
008700110301    4c                   if        tamddt<=20110301 and tamdst>=20110301
008800110228     c                   eval      flg='S'
008900110228     c                   update    wffua000
009000110301    4c                   endif
009100110301    3c                   endif
009200110228     c
009300110228     c                   eval      dsta01=tamflo
009400110228
009500110228     c* Se tariffe FEDEX  fuel non previsto
009600110228    3c                   if        §tafed='S'
009700110228     c                   except    errfed
009800110228     c                   add       1             totfed            5 0
009900110228     c                   eval      flg='F'
010000110228     c                   update    wffua000
010100110228    3c                   endif
010200110228     c
010300110228    2c                   endif
010400110228     c
010500110228     c                   read      wffua00f
010600110228    1c                   enddo
010700110228     c
010800110228     c                   eval      Waggio=savaggio
010900110302     C
011000110302     C                   IF        WAGGIO=' '
011100110302     C                   SETON                                        10
011200110302     C                   ENDIF
011300110228     c*
011400110228     C/EXEC SQL
011500110228     C+ declare C1 cursor for
011600110228     C+ SELECT CCM, CTR  FROM WFFUA00F  where FLG=' '
011700110228     C+ group BY CCM, CTR order by CCM, CTR
011800110228     c+ for read only
011900110228     C/END-EXEC
012000110228
012100110228     C/EXEC SQL
012200110228     C+ open C1
012300110228     C/END-EXEC
012400110228     C
012500110228     C/EXEC SQL
012600110228     C+ Fetch C1 into :ccm, :ctr
012700110228     C/END-EXEC
012800110228     C*
012900110228     C*
013000110228     C                   dow       sqlcod = *zeros
013100031105      *
013200110225     c                   clear                   wprima            1 0
013300110228     c                   clear                   err
013400031105      * Aggancio Tntam04l
013500110225     c     ktam4         setll     tntam04l
013600110225    1c                   do        *hival
013700110225     c     ktam4         reade     tntam04l
013800031105      *
013900031105      * fine file
014000110225    2c                   if        %eof(tntam04l)
014100110228     c*  Se prima lettura stampo errore per non trovata tariffa
014200110228     c                   except    nontrov
014300110301     c                   add       1             totnotar          5 0
014400110301     c/exec sql
014500110301     c+ update wffua00f set flg='O'
014600110301     c+ where   ccm=: ccm and ctr=:ctr
014700110301     c/end-exec sql
014800031105     c                   leave
014900110225    2c                   endif
015000110224     c
015100110224     c
015200110225    2c                   if        err=' '
015300110228     c                   eval      wprima=wprima+1
015400110228
015500110228     c                   except    tariffa
015600110224     c
015700110228      * Decorrenza maggiore/uguale 01/03/2011   --> updato prg+1
015800110228     c                   select
015900110228    3C                   when      TAMddt >= 20110301
016000081201     c                   clear                   up_tam
016100110228     C                   eval      §flagA = (TAMddt = 20110301)
016200031105     C                   eval      §flagD = *off
016300110228
016400110228    4C                   if        tamddt <> 20110301 and tamdst <> 20110301
016500081201     c                   eval      up_tam = *on
016600031105     C                   eval      kprg = TAMprg
016700110228     C                   exsr      Sr_Aggiorna
016800031105     C                   eval      TAMprg = (TAMprg + 1)
016900110228
017000110228      * aggiorno TNTAM anche se cambiato progressivo
017100031105     C                   eval      TAMduv = §oggi
017200031105     C                   eval      TAMftr = *blanks
017300041116     C                   eval      TAMdtr = §oggi
017400110228    5C                   IF        WAGGIO=' '
017500110228     C                   update    tntam04
017600110228    5c                   endif
017700110228     c                   add       1             totdec            5 0
017800110228     c
017900110228     c                   except    stampaupd
018000110228     c                   else
018100110228     c                   add       1             tot0103           5 0
018200110228    4c                   endif
018300110228     c*
018400110228     c                   exsr      CercaFuel
018500110228     c
018600110228     c                   if        wfusi=' '
018700110228     c                   eval      kprg=tamprg
018800110228     c                   exsr      Aggfuel
018900110228     c                   endif
019000110301     c
019100110301     c* Se la tariffa decorre dal 1/3 --> non devo aggiornare il progressivo precedente
019200110301     c                   if        tamddt=20110301
019300110301     c/exec sql
019400110301     c+ update wffua00f set flg='K'
019500110301     c+ where   ccm=: ccm and ctr=:ctr
019600110301     c/end-exec sql
019700110301     c                   leave
019800110301     c                   endif
019900110228     c
020000110224      * Scadenza maggiore/uguale 01/03/2011 e decorrenza minore 01/03/2011
020100110225    3C                   when      TAMdst >= 20110301 and TAMddt < 20110301
020200110228     c* Se FUEL c'e' già non faccio nulla
020300110228     c                   exsr      CercaFuel
020400110228     c
020500110228    4c                   if        wfusi=' '
020600110228     c
020700031105     C                   eval      §flagB = *on
020800031105     C                   eval      §dst = TAMdst
020900031105     C                   eval      kprg = TAMprg
021000081201      * deleta i record annullati del progressivo successivo
021100031105     C                   exsr      Sr_Annulla
021200031105     C                   exsr      Sr_Scrivi
021300110224     c
021400110224     C                   eval      TAMdst = 20110228
021500031105     C                   eval      TAMduv = §oggi
021600031105     C                   eval      TAMftr = *blanks
021700041116     C                   eval      TAMdtr = §oggi
021800110228    5c                   if        Waggio=' '
021900031105     C                   update    tntam04
022000110228    5c                   endif
022100110228     c
022200110228     c                   except    stampaupd
022300110228     c
022400110228     c                   eval      kprg=kprg + 1
022500110228     c                   exsr      Aggfuel
022600110301     c                   add       1             totcav            5 0
022700110301     c                   else
022800110301     c                   add       1             totesf            5 0
022900110228    4c                   endif
023000110301
023100110301     c/exec sql
023200110301     c+ update wffua00f set flg='K'
023300110301     c+ where   ccm=: ccm and ctr=:ctr
023400110301     c/end-exec sql
023500110228     c                   leave
023600031105
023700110225      * Scadenza minore/uguale 28/02/2011
023800110225    3C                   when      TAMdst <= 20110228
023900110228     c
024000110228     c* Se FUEL c'e' già non faccio nulla
024100110228     c                   exsr      CercaFuel
024200110228     c
024300110228    4c                   if        wfusi=' '
024400031105     C                   eval      §flagC = *on
024500051103      *
024600110301     C                   eval      §dst = 20110301
024700031105     C                   eval      kprg = TAMprg
024800031105     C                   exsr      Sr_Annulla
024900031105     C                   exsr      Sr_Scrivi
025000110224     c
025100110224     C                   eval      TAMdst = 20110228
025200110224     C                   eval      TAMduv = §oggi
025300110224     C                   eval      TAMftr = *blanks
025400110224     C                   eval      TAMdtr = §oggi
025500110225     c                   if        Waggio=' '
025600110224     C                   update    tntam04
025700110225     c                   endif
025800110228     c                   except    stampaupd
025900110224     c
026000110228     c                   eval      kprg=kprg + 1
026100110224     c                   exsr      AggFuel
026200110301     c                   add       1             totsca            5 0
026300110301     c                   else
026400110301     c                   add       1             totesf            5 0
026500110228    4c                   endif
026600110228     c
026700110301     c/exec sql
026800110301     c+ update wffua00f set flg='K'
026900110301     c+ where   ccm=: ccm and ctr=:ctr
027000110301     c/end-exec sql
027100110228     c                   leave
027200110228    3C                   endsl
027300110228
027400110225     c
027500110225    2c                   endif
027600110225    1C                   enddo
027700110225     c
027800110228     C/EXEC SQL
027900110228     C+ Fetch C1 into :ccm, :ctr
028000110228     C/END-EXEC
028100110225    1C                   enddo
028200081201     C
028300110228     C/EXEC SQL
028400110228     C+ open C1
028500110228     C/END-EXEC
028600081201
028700110301     c                   eval      general=totnotar+totesf+totfed+totdec
028800110228     c                                    +tot0103+totsca+totcav
028900110228     c                   except    totali
029000081201     c                   seton                                        lr
029100031105      *****************************************************************
029200031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
029300031105      *****************************************************************
029400031105     C     Sr_Aggiorna   BEGSR
029500031105
029600031105     C     ktam          setll     titad04l
029700031105     C                   do        *hival
029800031105     C     ktam          reade     titad04l
029900031105     C                   if        %eof(titad04l)
030000031105     C                   leave
030100031105     C                   endif
030200031105     C                   if        TADatb <> *blanks
030300031105     C                   iter
030400031105     C                   endif
030500031105     C                   eval      TADprg = (TADprg + 1)
030600031105     C                   eval      TADftr = *blanks
030700041116     C                   eval      TADdtr = §oggi
030800110225     c                   if        Waggio=' '
030900031105     C                   update    titad04
031000110225     c                   endif
031100031105     C                   enddo
031200031105
031300031105     C     ktam          setll     titpt01l
031400031105     C                   do        *hival
031500031105     C     ktam          reade     titpt01l
031600031105     C                   if        %eof(titpt01l)
031700031105     C                   leave
031800031105     C                   endif
031900031105     C                   if        TPTatb <> *blanks
032000031105     C                   iter
032100031105     C                   endif
032200031105     C                   eval      TPTprg = (TPTprg + 1)
032300031105     C                   eval      TPTduv = §oggi
032400031105     C                   eval      TPTftr = *blanks
032500061128     C                   eval      TPTdtr = §oggi
032600110225     c                   if        Waggio=' '
032700031105     C                   update    titpt01
032800110225     c                   endif
032900031105     C                   enddo
033000031105
033100031105     C     ktam          setll     titpd01l
033200031105     C                   do        *hival
033300031105     C     ktam          reade     titpd01l
033400031105     C                   if        %eof(titpd01l)
033500031105     C                   leave
033600031105     C                   endif
033700031105     C                   if        TPDatb <> *blanks
033800031105     C                   iter
033900031105     C                   endif
034000031105     C                   eval      TPDprg = (TPDprg + 1)
034100031105     C                   eval      TPDftr = *blanks
034200041116     C                   eval      TPDdtr = §oggi
034300110225     c                   if        Waggio=' '
034400031105     C                   update    titpd01
034500110225     c                   endif
034600031105     C                   enddo
034700031105
034800031105     C     ktam          setll     titgc01l
034900031105     C                   do        *hival
035000031105     C     ktam          reade     titgc01l
035100031105     C                   if        %eof(titgc01l)
035200031105     C                   leave
035300031105     C                   endif
035400031105     C                   if        TGCatb <> *blanks
035500031105     C                   iter
035600031105     C                   endif
035700031105     C                   eval      TGCprg = (TGCprg + 1)
035800031105     C                   eval      TGCduv = §oggi
035900031105     C                   eval      TGCftr = *blanks
036000041116     C                   eval      TGCdtr = §oggi
036100110225     c                   if        Waggio=' '
036200031105     C                   update    titgc01
036300110225     c                   endif
036400031105     C                   enddo
036500031105
036600031105     C                   endsr
036700031105      *****************************************************************
036800031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
036900031105      *****************************************************************
037000031105     C     Sr_Annulla    BEGSR
037100031105
037200031105     C                   eval      kprg = (kprg + 1)
037300031105
037400031105     C     ktam          setll     titad04l
037500031105     C                   do        *hival
037600031105     C     ktam          reade     titad04l
037700031105     C                   if        %eof(titad04l)
037800031105     C                   leave
037900031105     C                   endif
038000031105     C                   if        TADatb = *blanks
038100031105     C                   iter
038200031105     C                   endif
038300110225     c                   if        Waggio=' '
038400031105     C                   delete    titad04
038500110225     C                   endif
038600031105     C                   enddo
038700031105
038800031105     C     ktam          setll     titpt01l
038900031105     C                   do        *hival
039000031105     C     ktam          reade     titpt01l
039100031105     C                   if        %eof(titpt01l)
039200031105     C                   leave
039300031105     C                   endif
039400031105     C                   if        TPTatb = *blanks
039500031105     C                   iter
039600031105     C                   endif
039700110225     c                   if        Waggio=' '
039800031105     C                   delete    titpt01
039900110225     c                   endif
040000031105     C                   enddo
040100031105
040200031105     C     ktam          setll     titpd01l
040300031105     C                   do        *hival
040400031105     C     ktam          reade     titpd01l
040500031105     C                   if        %eof(titpd01l)
040600031105     C                   leave
040700031105     C                   endif
040800031105     C                   if        TPDatb = *blanks
040900031105     C                   iter
041000031105     C                   endif
041100110225     c                   if        Waggio=' '
041200031105     C                   delete    titpd01
041300110225     c                   endif
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
042500110225     c                   if        Waggio=' '
042600031105     C                   delete    titgc01
042700110225     c                   endif
042800031105     C                   enddo
042900031105
043000031105     C     ktam          chain     tntam01l
043100031105     C                   if        %found(tntam01l)
043200031105     C                             and old_TAMatb <> *blanks
043300110225     c                   if        Waggio=' '
043400031105     C                   delete    tntam01
043500110225     c                   endif
043600031105     C                   endif
043700031105
043800031105     C                   eval      kprg = (kprg - 1)
043900031105
044000031105     C                   endsr
044100031105      *****************************************************************
044200031105      * SCRIVE LE NUOVE TARIFFE
044300031105      *****************************************************************
044400031105     C     Sr_Scrivi     BEGSR
044500081202
044600031105     C     ktam          setll     titad04l
044700031105     C                   do        *hival
044800031105     C     ktam          reade     titad04l
044900031105     C                   if        %eof(titad04l)
045000031105     C                   leave
045100031105     C                   endif
045200031105     C                   if        TADatb <> *blanks
045300031105     C                   iter
045400031105     C                   endif
045500031105     C                   eval      new_TADksc = TADksc
045600031105     C                   eval      new_TADctr = TADctr
045700031105     C                   eval      new_TADprg = (kprg + 1)
045800031105     C                   eval      new_TADlnp = TADlnp
045900031105     C                   eval      new_TADcts = TADcts
046000031105     C                   eval      new_TADnaz = TADnaz
046100031105     C                   eval      new_TADcap = TADcap
046200031105     C                   eval      new_TADsgl = TADsgl
046300031105     C                   eval      new_TADitr = TADitr
046400031105     C                   eval      new_TADino = TADino
046500031105     C                   eval      new_TADrpv = TADrpv
046600031105     C                   eval      new_TADorp = TADorp
046700031105     C                   eval      new_TADmin = TADmin
046800031105     C                   eval      new_TADmax = TADmax
046900031105     C                   eval      new_TADftr = *blanks
047000041116     C                   eval      new_TADdtr = §oggi
047100110225
047200110225     c                   if        Waggio=' '
047300110225     C                   write     titad000
047400110225     c                   endif
047500110225
047600031105     C                   enddo
047700031105
047800031105     C     ktam          setll     titpt01l
047900031105     C                   do        *hival
048000031105     C     ktam          reade     titpt01l
048100031105     C                   if        %eof(titpt01l)
048200031105     C                   leave
048300031105     C                   endif
048400031105     C                   if        TPTatb <> *blanks
048500031105     C                   iter
048600031105     C                   endif
048700110225     c* non scrivo nemmeno tariffa FUEL
048800110225     c                   if        tptftc='f'
048900110225     C                   iter
049000110225     C                   endif
049100031105     C                   eval      new_TPTksc = TPTksc
049200031105     C                   eval      new_TPTctr = TPTctr
049300031105     C                   eval      new_TPTprg = (kprg + 1)
049400031105     C                   eval      new_TPTduv = §oggi
049500031105     C                   eval      new_TPTftc = TPTftc
049600031105     C                   eval      new_TPTtpg = TPTtpg
049700031105     C                   eval      new_TPTarl = TPTarl
049800031105     C                   eval      new_TPTarf = TPTarf
049900031105     C                   eval      new_TPTaro = TPTaro
050000031105     C                   eval      new_TPTrpv = TPTrpv
050100031105     C                   eval      new_TPTvlm = TPTvlm
050200031105     C                   eval      new_TPTvvm = TPTvvm
050300031105     C                   eval      new_TPTfvm = TPTfvm
050400031105     C                   eval      new_TPTtma = TPTtma
050500031105     C                   eval      new_TPTapl = TPTapl
050600031105     C                   eval      new_TPTftr = *blanks
050700041116     C                   eval      new_TPTdtr = §oggi
050800061128     C                   eval      new_TPTdpb = TPTdpb
050900061128     C                   eval      new_TPTdit = TPTdit
051000061128     C                   eval      new_TPTflo = TPTflo
051100110225     c                   if        Waggio=' '
051200031105     C                   write     titpt000
051300110225     c                   endif
051400031105     C                   enddo
051500031105
051600031105     C     ktam          setll     titpd01l
051700031105     C                   do        *hival
051800031105     C     ktam          reade     titpd01l
051900031105     C                   if        %eof(titpd01l)
052000031105     C                   leave
052100031105     C                   endif
052200031105     C                   if        TPDatb <> *blanks
052300031105     C                   iter
052400031105     C                   endif
052500110225     c* non scrivo nemmeno tariffa FUEL
052600110225     c                   if        tpdftc='f'
052700110225     C                   iter
052800110225     C                   endif
052900031105     C                   eval      new_TPDksc = TPDksc
053000031105     C                   eval      new_TPDctr = TPDctr
053100031105     C                   eval      new_TPDprg = (kprg + 1)
053200031105     C                   eval      new_TPDftc = TPDftc
053300031105     C                   eval      new_TPDcts = TPDcts
053400031105     C                   eval      new_TPDorp = TPDorp
053500031105     C                   eval      new_TPDsgl = TPDsgl
053600031105     C                   eval      new_TPDitr = TPDitr
053700031105     C                   eval      new_TPDmin = TPDmin
053800031105     C                   eval      new_TPDmax = TPDmax
053900031105     C                   eval      new_TPDain = TPDain
054000031105     C                   eval      new_TPDftr = *blanks
054100041116     C                   eval      new_TPDdtr = §oggi
054200110225     c                   if        Waggio=' '
054300031105     C                   write     titpd000
054400110225     c                   endif
054500031105     C                   enddo
054600031105
054700031105     C     ktam          setll     titgc01l
054800031105     C                   do        *hival
054900031105     C     ktam          reade     titgc01l
055000031105     C                   if        %eof(titgc01l)
055100031105     C                   leave
055200031105     C                   endif
055300031105     C                   if        TGCatb <> *blanks
055400031105     C                   iter
055500031105     C                   endif
055600031105     C                   eval      new_TGCksc = TGCksc
055700031105     C                   eval      new_TGCctr = TGCctr
055800031105     C                   eval      new_TGCprg = (kprg + 1)
055900031105     C                   eval      new_TGCsgv = TGCsgv
056000031105     C                   eval      new_TGCsgr = TGCsgr
056100031105     C                   eval      new_TGCsgp = TGCsgp
056200031105     C                   eval      new_TGCsgd = TGCsgd
056300031105     C                   eval      new_TGCsg1 = TGCsg1
056400031105     C                   eval      new_TGCsg2 = TGCsg2
056500031105     C                   eval      new_TGCsg3 = TGCsg3
056600031105     C                   eval      new_TGCst1 = TGCst1
056700031105     C                   eval      new_TGCst2 = TGCst2
056800031105     C                   eval      new_TGCst3 = TGCst3
056900031105     C                   eval      new_TGCstm = TGCstm
057000031105     C                   eval      new_TGCrpv = TGCrpv
057100031105     C                   eval      new_TGCfaf = TGCfaf
057200031105     C                   eval      new_TGCsgf = TGCsgf
057300031105     C                   eval      new_TGCggr = TGCggr
057400031105     C                   eval      new_TGCtcm = TGCtcm
057500031105     C                   eval      new_TGCtfg = TGCtfg
057600031105     C                   eval      new_TGCduv = §oggi
057700031105     C                   eval      new_TGCftr = *blanks
057800041116     C                   eval      new_TGCdtr = §oggi
057900110225     c                   if        Waggio=' '
058000031105     C                   write     titgc000
058100110225     c                   endif
058200031105     C                   enddo
058300031105
058400031105     C                   eval      new_TAMksc = TAMksc
058500031105     C                   eval      new_TAMctr = TAMctr
058600031105     C                   eval      new_TAMprg = (kprg + 1)
058700110224     C                   eval      new_TAMddt = 20110301
058800031105     C                   eval      new_TAMdst = §dst
058900031105     C                   eval      new_TAMduv = §oggi
059000031105     C                   eval      new_TAMdcv = TAMdcv
059100031105     C                   eval      new_TAMdif = TAMdif
059200031105     C                   eval      new_TAMfli = TAMfli
059300031105     C                   eval      new_TAMpri = TAMpri
059400031105     C                   eval      new_TAMfmp = TAMfmp
059500031105     C                   eval      new_TAMlrc = TAMlrc
059600031105     C                   eval      new_TAMlas = TAMlas
059700051104     c                   eval      new_TAMpag = TAMpag
059800081202     c                   eval      new_TAMppa = TAMppa
059900051021
060000031105     C                   eval      new_TAMmpa = TAMmpa
060100031105     C                   eval      new_TAMpam = TAMpam
060200031105     C                   eval      new_TAMmpm = TAMmpm
060300031105     C                   eval      new_TAMtin = TAMtin
060400031105     C                   eval      new_TAMkpm = TAMkpm
060500031105     C                   eval      new_TAMarl = TAMarl
060600031105     C                   eval      new_TAMarf = TAMarf
060700031105     C                   eval      new_TAMaro = TAMaro
060800031105     C                   eval      new_TAMrat = TAMrat
060900031105     C                   eval      new_TAMrct = TAMrct
061000101129
061100031105     C                   eval      new_TAMsc2 = TAMsc2
061200031105     C                   eval      new_TAMeds = TAMeds
061300031105     C                   eval      new_TAMars = TAMars
061400031105     C                   eval      new_TAMata = TAMata
061500031105     C                   eval      new_TAMftp = TAMftp
061600031105     C                   eval      new_TAMbap = TAMbap
061700031105     C                   eval      new_TAMtsp = TAMtsp
061800031105     C                   eval      new_TAMfie = TAMfie
061900031105     C                   eval      new_TAMfis = TAMfis
062000031105     C                   eval      new_TAMsis = TAMsis
062100031105     C                   eval      new_TAMfvd = TAMfvd
062200031105     C                   eval      new_TAMfts = TAMfts
062300031105     C                   eval      new_TAMnas = TAMnas
062400031105     C                   eval      new_TAMnot = TAMnot
062500031105     C                   eval      new_TAMftm = TAMftm
062600031105     C                   eval      new_TAMgca = TAMgca
062700031105     C                   eval      new_TAMgga = TAMgga
062800031105     C                   eval      new_TAMgma = TAMgma
062900031105     C                   eval      new_TAMgva = TAMgva
063000031105     C                   eval      new_TAMbam = TAMbam
063100031105     C                   eval      new_TAMtpr = TAMtpr
063200031105     C                   eval      new_TAMflo = TAMflo
063300031105     C                   eval      new_TAMdat = TAMdat
063400031105     C                   eval      new_TAMftr = *blanks
063500041116     C                   eval      new_TAMdtr = §oggi
063600110225     c                   if        Waggio=' '
063700031105     C                   write     tntam000
063800110225     c                   endif
063900110225     c
064000110228     c                   except    stampanew
064100031105
064200031105     C                   endsr
064300110224      *****************************************************************
064400110224      * Aggiunta FUEL
064500110224      *****************************************************************
064600110224     C     AGGFUEL       BEGSR
064700110224     c                   clear                   titpt000
064800110224     C                   eval      new_TPTksc = tamksc
064900110224     C                   eval      new_TPTctr = tamctr
065000110228     C                   eval      new_TPTprg =  kprg
065100110224     C                   eval      new_TPTduv = §oggi
065200110224     C                   eval      new_TPTftc = 'f'
065300110224     C                   eval      new_TPTtpg = 'V'
065400110224     C                   eval      new_TPTdtr = §oggi
065500110224     C                   eval      new_TPTdpb = 20110301
065600110225     C                   eval      new_TPTdit = 20110301
065700110225     c                   if        Waggio=' '
065800110224     C                   write     titpt000
065900110225     c                   endif
066000110224     c
066100110224     C                   eval      new_TPDksc = tamksc
066200110224     C                   eval      new_TPDctr = tamctr
066300110228     C                   eval      new_TPDprg =  kprg
066400110224     C                   eval      new_TPDftc = 'f'
066500110225     c                   if        tamfie='I'
066600110224     C                   eval      new_TPDcts = 'IT'
066700110301     C                   eval      new_TPDorp = '  A 9'
066800110224     c                   else
066900110224     C                   eval      new_TPDcts = 'EE'
067000110301     C                   eval      new_TPDorp = '  999'
067100110224     c                   endif
067200110224     C                   eval      new_TPDsgl = 9999999999
067300110224     C                   eval      new_TPDitr = 0,3
067400110224     C                   eval      new_TPDftr = *blanks
067500110224     C                   eval      new_TPDdtr = §oggi
067600110225     c                   if        Waggio=' '
067700110224     C                   write     titpd000
067800110225     c                   endif
067900110228     c                   except    newfuel
068000110224     c
068100110224     C                   endsr
068200031105      *****************************************************************
068300031105      * ROUTINE INIZIALE
068400031105      *****************************************************************
068500031105     C     *INZSR        BEGSR
068600031105
068700031105     C     *ENTRY        PLIST
068800031105     C                   PARM                    KPJBA
068900110225     c                   movel     kpjbu         WAGGIO            1
069000031105      *
069100031105     c
069200110225     C     Ktam          klist
069300110225     C                   kfld                    TAMksc
069400110225     C                   kfld                    TAMctr
069500110225     C                   kfld                    Kprg
069600110225
069700110224     C     Ktpt          klist
069800031105     C                   kfld                    TAMksc
069900031105     C                   kfld                    TAMctr
070000031105     C                   kfld                    Kprg
070100110224     C                   kfld                    Kftc
070200110225     c     ktam4         klist
070300110224     c                   kfld                    ccm
070400110224     c                   kfld                    ctr
070500110228     c     kfua          klist
070600110228     c                   kfld                    ccm
070700110228     c                   kfld                    ctr
070800110228     c                   kfld                    prg
070900081201      *
071000081201     c
071100081201     c     keytab        klist
071200081201     c                   kfld                    codut
071300081201     c                   kfld                    kcod
071400081201     c                   kfld                    kkey
071500081201      *
071600031105      * reperisce data del giorno
071700031105     C                   TIME                    W0140
071800031105      * UDATE IN GGMMAAAA
071900031105     C                   MOVE      W0140         WDTGIO
072000031105      * UDATE IN AAAAMMGG
072100031105     C                   Z-ADD     WDTGIO        G02DAT
072200031105     C                   MOVEL     *BLANK        G02ERR
072300031105     C                   CALL      'XSRDA8'
072400031105     C                   PARM                    WLBDAT
072500031105     C                   Z-ADD     G02INV        §oggi
072600031105
072700031105     C                   endsr
072800110228     c*-----------------------------------------------------------------------------------------
072900110228     c     Cercafuel     BEGSR
073000110228
073100110228     c                   clear                   wfusi             1
073200110228     c                   eval      kprg=tamprg
073300110228     c                   eval      kftc='f'
073400110228     c     ktpt          chain     titpt01l
073500110228     c
073600110228    3c                   if        %found(titpt01l) and tptatb= ' '
073700110228     c     ktpt          setll     titpd01l
073800110228     c     ktpt          reade     titpd01l
073900110228     c
074000110228    4c                   dow       not %eof(titpd01l) and WFUSI=' '
074100110228     c* se trovato uno scaglione pieno --> stampo errore
074200110228    5c                   if        tpdatb=' ' and tpditr>0
074300110228     c                   eval      Wfusi='S'
074400110228     c                   else
074500110228     c     ktpt          reade     titpd01l
074600110228    5c                   endif
074700110228    4c                   enddo
074800110228     c
074900110228    4c                   if        WFUsi='S'
075000110228     c                   except    errpres
075100110228    4c                   endif
075200110228     c
075300110228    3c                   endif
075400110228     c
075500110228     c* cancello se presete una tariffa a zero a annullata
075600110228     c                   if        wfusi<>'S'  and waggio=' '
075700110228     c     ktpt          delete    titpt01l
075800110228     c
075900110228     c     ktpt          setll     titpd01l
076000110228     c     ktpt          reade     titpd01l
076100110228     c
076200110228    4c                   dow       not %eof(titpd01l)
076300110228     c                   delete    titpd01
076400110228     c
076500110228     c     ktpt          reade     titpd01l
076600110228     c                   enddo
076700110228     c
076800110228     c                   endif
076900110228     c
077000110228     c                   ENDSR
077100110224     OPRTF198   E            TESTA            02
077200110224     O                                            8 'TAVRF2R '
077300110302     O                                           62 '* Scrittura Tariffa FUEL *'
077400110302     o               10                         100 '   AGGIORNA ARCHIVI'
077500110302     o              N10                         100 '   SOLO CONTROLLO  '
077600110224     OPRTF198   E            ERRDEC      1
077700110224     o                       tamksc
077800110224     o                       tamctr           +   1
077900110224     o                       tamprg        z  +   1
078000110224     o                       tamfie           +   1
078100110224     o                       tamddt           +   1 '    /  /  '
078200110224     o                       tamdst           +   1 '    /  /  '
078300110225     o                                           49 ' '
078400110225     O                                        +   1 'ERR:Tariffa ancora da '
078500110224     O                                        +   0 'decorrere:sistemare a '
078600110224     O                                        +   0 'mano'
078700110224     OPRTF198   E            ERRpres     1
078800110224     o                       tamksc
078900110224     o                       tamctr           +   1
079000110224     o                       tamprg        z  +   1
079100110224     o                       tamfie           +   1
079200110224     o                       tamddt           +   1 '    /  /  '
079300110224     o                       tamdst           +   1 '    /  /  '
079400110225     o                                           49 ' '
079500110225     O                                        +   1 'ERR:Tariffa FUEL presente'
079600110225     OPRTF198   E            nontrov     1
079700110225     o                       ccm
079800110225     o                       ctr              +   1
079900110225     o                                           49 ' '
080000110225     O                                        +   1 'ERR:non trovata Tariffa'
080100110228     OPRTF198   E            nontrov2    1
080200110228     o                       ccm
080300110228     o                       ctr              +   1
080400110228     o                       prg           z  +   1
080500110228     o                                           49 ' '
080600110301     O                                        +   1 'ERR:non trovato progressiv'
080700110301     o                                        +   0 'o tariffa'
080800110224     OPRTF198   E            ERRfed      1
080900110224     o                       tamksc
081000110224     o                       tamctr           +   1
081100110224     o                       tamprg        z  +   1
081200110224     o                       tamfie           +   1
081300110224     o                       tamddt           +   1 '    /  /  '
081400110224     o                       tamdst           +   1 '    /  /  '
081500110225     o                                           49 ' '
081600110225     O                                        +   1 'ERR:Tariffa FEDEX'
081700110225     OPRTF198   E            tariffa     1
081800110225     o                       tamksc
081900110225     o                       tamctr           +   1
082000110225     o                       tamprg        z  +   1
082100110225     o                       tamfie           +   1
082200110225     o                       tamddt           +   1 '    /  /  '
082300110225     o                       tamdst           +   1 '    /  /  '
082400110225     o                                           49 ' '
082500110225     O                                        +   1 'Tariffa da aggiornare '
082600110225     OPRTF198   E            stampanew   1
082700110225     O                                        +   0 '     '
082800110225     o                       new_tamksc       +   1
082900110225     o                       new_tamctr       +   1
083000110225     o                       new_tamprg    z  +   1
083100110225     o                       new_tamfie       +   1
083200110225     o                       new_tamddt       +   1 '    /  /  '
083300110225     o                       new_tamdst       +   1 '    /  /  '
083400110225     o                                           49 ' '
083500110225     O                                        +   1 'Nuovo progressivo creato'
083600110225     OPRTF198   E            stampaupd   1
083700110225     O                                        +   0 '     '
083800110225     o                       tamksc           +   1
083900110225     o                       tamctr           +   1
084000110225     o                       tamprg        z  +   1
084100110225     o                       tamfie           +   1
084200110225     o                       tamddt           +   1 '    /  /  '
084300110225     o                       tamdst           +   1 '    /  /  '
084400110225     o                                           49 ' '
084500110225     O                                        +   1 'Progressivo aggiornato'
084600110225     OPRTF198   E            newfuel     1
084700110225     O                                        +   0 '     '
084800110225     o                       new_tptksc       +   1
084900110225     o                       new_tptctr       +   1
085000110225     o                       new_tptprg    z  +   1
085100110225     o                       new_tptftc       +   1
085200110225     o                       new_tptdpb       +   1 '    /  /  '
085300110225     o                                           49 ' '
085400110225     O                                        +   1 'Fuel inserito'
085500110228     OPRTF198   E            totali      2
085600110228     O                                        +   0 'TARIFFE AGGIORANTE:'
085700110228     O                                        +   2 'da decorrere'
085800110228     o                       totdec        z  +   1
085900110228     O                                        +   2 'decorrenza 01/03'
086000110228     o                       tot0103       z  +   1
086100110228     O                                        +   2 'a cavallo'
086200110228     o                       totcav        z  +   1
086300110228     O                                        +   2 'scadute'
086400110228     o                       totsca        z  +   1
086500110228     o
086600110228     OPRTF198   E            totali      2
086700110228     O                                        +   0 'ERRORI:'
086800110301     O                                        +   2 'non trovata tariffa'
086900110301     o                       totnotar      z  +   1
087000110228     O                                        +   2 'FUEL presente'
087100110228     o                       totesf        z  +   1
087200110228     O                                        +   2 'tar. FEDEX'
087300110228     o                       totfed        z  +   1
087400110301     O                                        +   4 'TOT. GENERALE :'
087500110228     o                       GENERal       2  +   1
087600110301     OPRTF198   E            totali      2
087700110301     O                                        +   2 'di cui'
087800110301     O                                        +   2 'non trovato progr.'
087900110301     o                       totnot        z  +   1
