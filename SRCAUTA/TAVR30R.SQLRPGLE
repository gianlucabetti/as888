000100091203     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200031105
000300031105      *****************************************************************
000400111109      *       Aumento demoltiplicatore FUEL a 0,5
000500031105      *****************************************************************
000600110224
000700111110     FWFfue01L  uF   E           K DISK
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
002300111114     fTitav01l  if a e           k disk
002400081201     FTabel00f  if   e           k DISK
002500110224     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002600031105      *------------------------------------------------------------------------*
002700081128
002800031105     D Kprg            S                   LIKE(TAMprg)
002900031105     D §ksc            S                   LIKE(TAMksc)
003000031105     D §ctr            S                   LIKE(TAMctr)
003100031105     D §prg            S                   LIKE(TAMprg)
003200051024     D §ppa            S                   LIKE(TAMppa)
003300051104     D §pag            S                   LIKE(TAMpag)
003400031105     D §dst            S                   LIKE(TAMdst)
003500081201     D Kcod            S                   like(TBLCOD)
003600081201     D Kkey            S                   like(TBLKEY)
003700110224     D Kftc            S                   like(tptftc)
003800111111     d AU_Cliente      s                   like(WFFksc)
003900111111     d AU_Tariffa      s                   like(WFFctrv)
004000111111     d AU_Progres      s                   like(WFFprgv)
004100081201     D Codut           s              1  0 inz(1)
004200081201
004300031105     D W0140           S             14  0
004400031105     D Wdtgio          S              8  0
004500031105     D §oggi           S              8  0
004600051109     D caladd          s             30  9
004700031105     D §flagA          S              1
004800031105     D §flagB          S              1
004900031105     D §flagC          S              1
005000031105     D §flagD          S              1
005100111114     D §Agg_tpt        S              1
005200081201     D Up_tam          S              1
005300110228     D Err             S              1
005400110228     D Savaggio        S              1
005500110228     D general         S              9  0
005600111114     d contatore       s              5  0
005700031105
005800031105      *   D S   I N T E R N E / E S T E R N E
005900031105
006000031105     D WLBDAT          DS                  INZ
006100031105     D  G02DAT                 1      8  0
006200031105     D  G02INV                 9     16  0
006300031105     D  G02ERR                17     17
006400031105     D  G02TGI                18     22  0
006500031105
006600110228     D Dsta01        e ds
006700031105     D kpjba         e ds
006800031105
006900081128      *------------------------------------------------------------------------*
007000110225     c                   except    testa
007100110228     c
007200111111     c
007300111111     c/EXEC SQL
007400111111     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007500111111     c/END-EXEC
007600111111     c*
007700110228     c*
007800110228     C/EXEC SQL
007900110228     C+ declare C1 cursor for
008000111109     C+ SELECT WFFKSC, WFFCTRV  FROM WFFUE00F  where WFFAUM = 'S'
008100111109     C+ and WFFELA = ' '
008200111110     C+ group BY WFFKSC, WFFCTRV order by WFFKSC, WFFCTRV
008300110228     c+ for read only
008400110228     C/END-EXEC
008500110228
008600110228     C/EXEC SQL
008700110228     C+ open C1
008800110228     C/END-EXEC
008900110228     C
009000110228     C/EXEC SQL
009100111110     C+ Fetch C1 into :WFFKSC, :WFFCTRV
009200110228     C/END-EXEC
009300110228     C*
009400111110     C
009500111110     C                   IF        WAGGIO=' '
009600111110     C                   SETON                                        10
009700111110     C                   ENDIF
009800110228     C*
009900111110  do1C                   dow       sqlcod = *zeros
010000031105      *
010100110225     c                   clear                   wprima            1 0
010200110228     c                   clear                   err
010300031105      * Aggancio Tntam04l
010400110225     c     ktam4         setll     tntam04l
010500111110  do2c                   do        *hival
010600110225     c     ktam4         reade     tntam04l
010700031105      *
010800031105      * fine file
010900111110    1c                   if        %eof(tntam04l)
011000110228     c*  Se prima lettura stampo errore per non trovata tariffa
011100111110     C                   If        Wprima = 0
011200110228     c                   except    nontrov
011300110301     c                   add       1             totnotar          5 0
011400110301     c/exec sql
011500111109     c+ update wffue00f set wffela='O'
011600111110     c+ where   wffksc=:wffksc and wffctrv=:wffctrv
011700110301     c/end-exec sql
011800111110     C                   Endif
011900031105     c                   leave
012000111110    1c                   endif
012100110224     c
012200111111     c                   eval      AU_cliente = tamksc
012300111111     c                   eval      AU_tariffa = tamctr
012400111111     c                   eval      AU_progres = tamprg
012500111111
012600111111      * Se stessa tariffa e si è verificata una delle condizioni
012700111111      * non deve elaborare più niente
012800111111     C                   if        (TAMksc = §ksc and TAMctr = §ctr
012900111111     C                             and §flagA = *on) or
013000111111     C                             (TAMksc = §ksc and TAMctr = §ctr
013100111111     C                             and §flagB = *on) or
013200111111     C                             (TAMksc = §ksc and TAMctr = §ctr
013300111111     C                             and §flagC = *on)
013400111114     C                   leave
013500111111     C                   endif
013600111111
013700111111      * Cambia tariffa
013800111111     C                   if        TAMksc <> §ksc
013900111111     C                   eval      §ksc = TAMksc
014000111111     C                   eval      §flagA = *off
014100111111     C                   eval      §flagB = *off
014200111111     C                   eval      §flagC = *off
014300111111     C                   endif
014400111111     C                   if        TAMctr <> §ctr
014500111111     C                   eval      §ctr = TAMctr
014600111111     C                   eval      §flagA = *off
014700111111     C                   eval      §flagB = *off
014800111111     C                   eval      §flagC = *off
014900111111     C                   endif
015000110224     c
015100111110    1c                   if        err=' '
015200110228     c                   eval      wprima=wprima+1
015300110228
015400110228     c                   except    tariffa
015500110224     c
015600111109      * Decorrenza maggiore/uguale 01/12/2011   --> updato prg+1
015700111110    2c                   select
015800111110     C                   when      TAMddt >= 20111201
015900081201     c                   clear                   up_tam
016000111109     C                   eval      §flagA = (TAMddt = 20111201)
016100031105     C                   eval      §flagD = *off
016200110228
016300111110    3C                   if        tamddt <> 20111201 and tamdst <> 20111201
016400111116      * solo se non è il progressivo  0
016500111116     c                   If        tamprg > 0
016600111111     C                   exsr      Sr_CercaDopo
016700111116     c                   endif
016800111111    3C                   if        §flagD = *on
016900081201     c                   eval      up_tam = *on
017000031105     C                   eval      kprg = TAMprg
017100110228     C                   exsr      Sr_Aggiorna
017200031105     C                   eval      TAMprg = (TAMprg + 1)
017300110228
017400110228      * aggiorno TNTAM anche se cambiato progressivo
017500031105     C                   eval      TAMduv = §oggi
017600031105     C                   eval      TAMftr = *blanks
017700041116     C                   eval      TAMdtr = §oggi
017800111110    4C                   IF        WAGGIO=' '
017900110228     C                   update    tntam04
018000111116      * --> SCRIVO TITAV
018100111116     c                   Clear                   Titav000
018200111116     c                   Eval      tavksc = tamksc
018300111116     c                   Eval      tavctr = tamctr
018400111116     c                   Eval      tavprg = tamprg
018500111116     c                   Eval      tavtrd = 'TES'
018600111116     c                   Eval      tavcta = 'D'
018700111116     c                   Eval      tavdav = *date
018800111116     c                   Time                    w0140
018900111116     c                   Movel     w0140         tavorv
019000111116     c                   Eval      tavnoj = 'AUM_FUEL'
019100111116     c                   Eval      tavpru = 'BATCH'
019200111116     c                   Eval      tavdtr = *date
019300111116     c                   Write     Titav000
019400111116    4c                   endif
019500111116
019600111110     c     Ktam          chain     wffue01l
019700111110    4c                   If        %found(wffue01l)
019800111110     c                   eval      wffprgn = tamprg
019900111110     c                   eval      wffddtn = tamddt
020000111110     c                   eval      wffdstn = tamdst
020100111110     c                   eval      wffela = 'S'
020200111110     c                   update    wffue000
020300111110    4c                   Endif
020400111110
020500110228     c                   add       1             totdec            5 0
020600110228     c
020700110228     c                   except    stampaupd
020800111111     c                   endif
020900111110   x3c                   else
021000111110     c                   add       1             tot0112           5 0
021100111110    3c                   endif
021200111109     c
021300111111     c                   If        §flagD = *off
021400110228     c                   eval      kprg=tamprg
021500110228     c                   exsr      Aggfuel
021600111111      * aggiorno TNTAM  xchè aggiornato fuel
021700111116     c                   If        §agg_tpt = *on
021800111111     C                   eval      TAMduv = §oggi
021900111111     C                   eval      TAMftr = *blanks
022000111111     C                   eval      TAMdtr = §oggi
022100111111    4C                   IF        WAGGIO=' '
022200111111     C                   update    tntam04
022300111111    4c                   endif
022400111111     c     Ktam          chain     wffue01l
022500111111    4c                   If        %found(wffue01l)
022600111111     c                   eval      wffprgn = tamprg
022700111111     c                   eval      wffddtn = tamddt
022800111111     c                   eval      wffdstn = tamdst
022900111111     c                   eval      wffela = 'S'
023000111111     c                   update    wffue000
023100111111    4c                   Endif
023200111111    4c                   Endif
023300111116    4c                   Endif
023400111109     c
023500110301     c
023600111109     c* Se la tariffa decorre dal 1/12--> non devo aggiornare il progressivo precedente
023700111111    3c***                if        tamddt=20111201
023800111111     c***                leave
023900111111    3c***                endif
024000110228     c
024100111109      * Scadenza maggiore/uguale 01/12/2011 e decorrenza minore 01/12/2011
024200111110     C                   when      TAMdst >= 20111201 and TAMddt < 20111201
024300110228     c
024400031105     C                   eval      §flagB = *on
024500031105     C                   eval      §dst = TAMdst
024600031105     C                   eval      kprg = TAMprg
024700081201      * deleta i record annullati del progressivo successivo
024800031105     C                   exsr      Sr_Annulla
024900031105     C                   exsr      Sr_Scrivi
025000111110     c     Ktam          chain     wffue01l
025100111110    3c                   If        %found(wffue01l)
025200111110     c                   eval      wffprgn = New_tamprg
025300111110     c                   eval      wffddtn = New_tamddt
025400111110     c                   eval      wffdstn = New_tamdst
025500111110     c                   eval      wffela = 'S'
025600111110     c                   update    wffue000
025700111110    3c                   Endif
025800110224     c
025900111109     C                   eval      TAMdst = 20111130
026000031105     C                   eval      TAMduv = §oggi
026100031105     C                   eval      TAMftr = *blanks
026200041116     C                   eval      TAMdtr = §oggi
026300111110    3c                   if        Waggio=' '
026400031105     C                   update    tntam04
026500111110    3c                   endif
026600110228     c
026700110228     c                   except    stampaupd
026800110228     c
026900110301     c                   add       1             totcav            5 0
027000110301
027100111111     c****               leave
027200031105
027300111109      * Scadenza minore/uguale 30/11/2011
027400111109    3C                   when      TAMdst <= 20111130
027500110228     c
027600031105     C                   eval      §flagC = *on
027700111109      * (20110301... per me un pò strana)
027800111110     C                   eval      §dst = 20111231
027900031105     C                   eval      kprg = TAMprg
028000031105     C                   exsr      Sr_Annulla
028100031105     C                   exsr      Sr_Scrivi
028200111110     c     Ktam          chain     wffue01l
028300111110    3c                   If        %found(wffue01l)
028400111110     c                   eval      wffprgn = New_tamprg
028500111110     c                   eval      wffddtn = New_tamddt
028600111110     c                   eval      wffdstn = New_tamdst
028700111110     c                   eval      wffela = 'S'
028800111110     c                   update    wffue000
028900111110    3c                   Endif
029000110224     c
029100111111     C***                eval      TAMdst = 20111130
029200111111     C***                eval      TAMduv = §oggi
029300111111     C***                eval      TAMftr = *blanks
029400111111     C***                eval      TAMdtr = §oggi
029500111111    3c***                if        Waggio=' '
029600111111     C***                update    tntam04
029700111111    3c***                endif
029800111111     c***                except    stampaupd
029900110224     c
030000110301     c                   add       1             totsca            5 0
030100110228     c
030200110228     c                   leave
030300111110    2C                   endsl
030400110228
030500110225     c
030600111110    1c                   endif
030700111110  do2C                   enddo
030800110225     c
030900110228     C/EXEC SQL
031000111110     C+ Fetch C1 into :WFFKSC, :WFFCTRV
031100110228     C/END-EXEC
031200111110  do1C                   enddo
031300081201     C
031400110228     C/EXEC SQL
031500110228     C+ open C1
031600110228     C/END-EXEC
031700081201
031800111110     c                   eval      general=totnotar+totdec
031900111110     c                                    +tot0112+totsca+totcav
032000110228     c                   except    totali
032100081201     c                   seton                                        lr
032200031105      *****************************************************************
032300031105      * AGGIORNA I FILE LEGATI CON IL NUOVO PROGRESSIVO
032400031105      *****************************************************************
032500031105     C     Sr_Aggiorna   BEGSR
032600031105
032700031105     C     ktam          setll     titad04l
032800031105     C                   do        *hival
032900031105     C     ktam          reade     titad04l
033000031105     C                   if        %eof(titad04l)
033100031105     C                   leave
033200031105     C                   endif
033300031105     C                   if        TADatb <> *blanks
033400031105     C                   iter
033500031105     C                   endif
033600031105     C                   eval      TADprg = (TADprg + 1)
033700031105     C                   eval      TADftr = *blanks
033800041116     C                   eval      TADdtr = §oggi
033900110225     c                   if        Waggio=' '
034000031105     C                   update    titad04
034100110225     c                   endif
034200031105     C                   enddo
034300031105
034400031105     C     ktam          setll     titpt01l
034500031105     C                   do        *hival
034600031105     C     ktam          reade     titpt01l
034700031105     C                   if        %eof(titpt01l)
034800031105     C                   leave
034900031105     C                   endif
035000031105     C                   if        TPTatb <> *blanks
035100031105     C                   iter
035200031105     C                   endif
035300031105     C                   eval      TPTprg = (TPTprg + 1)
035400031105     C                   eval      TPTduv = §oggi
035500031105     C                   eval      TPTftr = *blanks
035600061128     C                   eval      TPTdtr = §oggi
035700110225     c                   if        Waggio=' '
035800031105     C                   update    titpt01
035900110225     c                   endif
036000031105     C                   enddo
036100031105
036200031105     C     ktam          setll     titpd01l
036300031105     C                   do        *hival
036400031105     C     ktam          reade     titpd01l
036500031105     C                   if        %eof(titpd01l)
036600031105     C                   leave
036700031105     C                   endif
036800031105     C                   if        TPDatb <> *blanks
036900031105     C                   iter
037000031105     C                   endif
037100111110     c* scrivo la tariffa FUEL con aumenti
037200111110     c                   if        tpdftc='f ' and
037300111110     c                             TPDitr > 0 and TPDitr < 0,5
037400111110     c                   eval      TPDitr = 0,5
037500111110     C                   endif
037600031105     C                   eval      TPDprg = (TPDprg + 1)
037700031105     C                   eval      TPDftr = *blanks
037800041116     C                   eval      TPDdtr = §oggi
037900110225     c                   if        Waggio=' '
038000031105     C                   update    titpd01
038100110225     c                   endif
038200031105     C                   enddo
038300031105
038400031105     C     ktam          setll     titgc01l
038500031105     C                   do        *hival
038600031105     C     ktam          reade     titgc01l
038700031105     C                   if        %eof(titgc01l)
038800031105     C                   leave
038900031105     C                   endif
039000031105     C                   if        TGCatb <> *blanks
039100031105     C                   iter
039200031105     C                   endif
039300031105     C                   eval      TGCprg = (TGCprg + 1)
039400031105     C                   eval      TGCduv = §oggi
039500031105     C                   eval      TGCftr = *blanks
039600041116     C                   eval      TGCdtr = §oggi
039700110225     c                   if        Waggio=' '
039800031105     C                   update    titgc01
039900110225     c                   endif
040000031105     C                   enddo
040100031105
040200031105     C                   endsr
040300031105      *****************************************************************
040400031105      * ANNULLA SE IL PROGRESSIVO ESISTE GIA' ED E' ANNULLATO
040500031105      *****************************************************************
040600031105     C     Sr_Annulla    BEGSR
040700031105
040800031105     C                   eval      kprg = (kprg + 1)
040900031105
041000031105     C     ktam          setll     titad04l
041100031105     C                   do        *hival
041200031105     C     ktam          reade     titad04l
041300031105     C                   if        %eof(titad04l)
041400031105     C                   leave
041500031105     C                   endif
041600031105     C                   if        TADatb = *blanks
041700031105     C                   iter
041800031105     C                   endif
041900110225     c                   if        Waggio=' '
042000031105     C                   delete    titad04
042100110225     C                   endif
042200031105     C                   enddo
042300031105
042400031105     C     ktam          setll     titpt01l
042500031105     C                   do        *hival
042600031105     C     ktam          reade     titpt01l
042700031105     C                   if        %eof(titpt01l)
042800031105     C                   leave
042900031105     C                   endif
043000031105     C                   if        TPTatb = *blanks
043100031105     C                   iter
043200031105     C                   endif
043300110225     c                   if        Waggio=' '
043400031105     C                   delete    titpt01
043500110225     c                   endif
043600031105     C                   enddo
043700031105
043800031105     C     ktam          setll     titpd01l
043900031105     C                   do        *hival
044000031105     C     ktam          reade     titpd01l
044100031105     C                   if        %eof(titpd01l)
044200031105     C                   leave
044300031105     C                   endif
044400031105     C                   if        TPDatb = *blanks
044500031105     C                   iter
044600031105     C                   endif
044700110225     c                   if        Waggio=' '
044800031105     C                   delete    titpd01
044900110225     c                   endif
045000031105     C                   enddo
045100031105
045200031105     C     ktam          setll     titgc01l
045300031105     C                   do        *hival
045400031105     C     ktam          reade     titgc01l
045500031105     C                   if        %eof(titgc01l)
045600031105     C                   leave
045700031105     C                   endif
045800031105     C                   if        TGCatb = *blanks
045900031105     C                   iter
046000031105     C                   endif
046100110225     c                   if        Waggio=' '
046200031105     C                   delete    titgc01
046300110225     c                   endif
046400031105     C                   enddo
046500031105
046600031105     C     ktam          chain     tntam01l
046700031105     C                   if        %found(tntam01l)
046800031105     C                             and old_TAMatb <> *blanks
046900110225     c                   if        Waggio=' '
047000031105     C                   delete    tntam01
047100110225     c                   endif
047200031105     C                   endif
047300031105
047400031105     C                   eval      kprg = (kprg - 1)
047500031105
047600031105     C                   endsr
047700031105      *****************************************************************
047800031105      * SCRIVE LE NUOVE TARIFFE
047900031105      *****************************************************************
048000031105     C     Sr_Scrivi     BEGSR
048100081202
048200031105     C     ktam          setll     titad04l
048300031105     C                   do        *hival
048400031105     C     ktam          reade     titad04l
048500031105     C                   if        %eof(titad04l)
048600031105     C                   leave
048700031105     C                   endif
048800031105     C                   if        TADatb <> *blanks
048900031105     C                   iter
049000031105     C                   endif
049100031105     C                   eval      new_TADksc = TADksc
049200031105     C                   eval      new_TADctr = TADctr
049300031105     C                   eval      new_TADprg = (kprg + 1)
049400031105     C                   eval      new_TADlnp = TADlnp
049500031105     C                   eval      new_TADcts = TADcts
049600031105     C                   eval      new_TADnaz = TADnaz
049700031105     C                   eval      new_TADcap = TADcap
049800031105     C                   eval      new_TADsgl = TADsgl
049900031105     C                   eval      new_TADitr = TADitr
050000031105     C                   eval      new_TADino = TADino
050100031105     C                   eval      new_TADrpv = TADrpv
050200031105     C                   eval      new_TADorp = TADorp
050300031105     C                   eval      new_TADmin = TADmin
050400031105     C                   eval      new_TADmax = TADmax
050500031105     C                   eval      new_TADftr = *blanks
050600041116     C                   eval      new_TADdtr = §oggi
050700110225
050800110225     c                   if        Waggio=' '
050900110225     C                   write     titad000
051000110225     c                   endif
051100110225
051200031105     C                   enddo
051300031105
051400031105     C     ktam          setll     titpt01l
051500031105     C                   do        *hival
051600031105     C     ktam          reade     titpt01l
051700031105     C                   if        %eof(titpt01l)
051800031105     C                   leave
051900031105     C                   endif
052000031105     C                   if        TPTatb <> *blanks
052100031105     C                   iter
052200031105     C                   endif
052300031105     C                   eval      new_TPTksc = TPTksc
052400031105     C                   eval      new_TPTctr = TPTctr
052500031105     C                   eval      new_TPTprg = (kprg + 1)
052600031105     C                   eval      new_TPTduv = §oggi
052700031105     C                   eval      new_TPTftc = TPTftc
052800031105     C                   eval      new_TPTtpg = TPTtpg
052900031105     C                   eval      new_TPTarl = TPTarl
053000031105     C                   eval      new_TPTarf = TPTarf
053100031105     C                   eval      new_TPTaro = TPTaro
053200031105     C                   eval      new_TPTrpv = TPTrpv
053300031105     C                   eval      new_TPTvlm = TPTvlm
053400031105     C                   eval      new_TPTvvm = TPTvvm
053500031105     C                   eval      new_TPTfvm = TPTfvm
053600031105     C                   eval      new_TPTtma = TPTtma
053700031105     C                   eval      new_TPTapl = TPTapl
053800031105     C                   eval      new_TPTftr = *blanks
053900041116     C                   eval      new_TPTdtr = §oggi
054000061128     C                   eval      new_TPTdpb = TPTdpb
054100061128     C                   eval      new_TPTdit = TPTdit
054200061128     C                   eval      new_TPTflo = TPTflo
054300110225     c                   if        Waggio=' '
054400031105     C                   write     titpt000
054500110225     c                   endif
054600031105     C                   enddo
054700031105
054800111114     c                   eval      contatore = 0
054900031105     C     ktam          setll     titpd01l
055000031105     C                   do        *hival
055100031105     C     ktam          reade     titpd01l
055200031105     C                   if        %eof(titpd01l)
055300031105     C                   leave
055400031105     C                   endif
055500031105     C                   if        TPDatb <> *blanks
055600031105     C                   iter
055700031105     C                   endif
055800111114      * scaglioni
055900111114     c                   add       1             contatore
056000031105     C                   eval      new_TPDksc = TPDksc
056100031105     C                   eval      new_TPDctr = TPDctr
056200031105     C                   eval      new_TPDprg = (kprg + 1)
056300031105     C                   eval      new_TPDftc = TPDftc
056400031105     C                   eval      new_TPDcts = TPDcts
056500031105     C                   eval      new_TPDorp = TPDorp
056600031105     C                   eval      new_TPDsgl = TPDsgl
056700111110     c* scrivo la tariffa FUEL con aumenti
056800111110     c                   if        tpdftc='f ' and
056900111114     c                             ((TPDitr > 0 and TPDitr < 0,5) or
057000111114     c                             (tpditr = 0 and contatore > 1))
057100111110     c                   eval      new_TPDitr = 0,5
057200111110     C                   else
057300111110     C                   eval      new_TPDitr = TPDitr
057400111110     C                   endif
057500031105     C                   eval      new_TPDmin = TPDmin
057600031105     C                   eval      new_TPDmax = TPDmax
057700031105     C                   eval      new_TPDain = TPDain
057800031105     C                   eval      new_TPDftr = *blanks
057900041116     C                   eval      new_TPDdtr = §oggi
058000110225     c                   if        Waggio=' '
058100031105     C                   write     titpd000
058200110225     c                   endif
058300031105     C                   enddo
058400031105
058500031105     C     ktam          setll     titgc01l
058600031105     C                   do        *hival
058700031105     C     ktam          reade     titgc01l
058800031105     C                   if        %eof(titgc01l)
058900031105     C                   leave
059000031105     C                   endif
059100031105     C                   if        TGCatb <> *blanks
059200031105     C                   iter
059300031105     C                   endif
059400031105     C                   eval      new_TGCksc = TGCksc
059500031105     C                   eval      new_TGCctr = TGCctr
059600031105     C                   eval      new_TGCprg = (kprg + 1)
059700031105     C                   eval      new_TGCsgv = TGCsgv
059800031105     C                   eval      new_TGCsgr = TGCsgr
059900031105     C                   eval      new_TGCsgp = TGCsgp
060000031105     C                   eval      new_TGCsgd = TGCsgd
060100031105     C                   eval      new_TGCsg1 = TGCsg1
060200031105     C                   eval      new_TGCsg2 = TGCsg2
060300031105     C                   eval      new_TGCsg3 = TGCsg3
060400031105     C                   eval      new_TGCst1 = TGCst1
060500031105     C                   eval      new_TGCst2 = TGCst2
060600031105     C                   eval      new_TGCst3 = TGCst3
060700031105     C                   eval      new_TGCstm = TGCstm
060800031105     C                   eval      new_TGCrpv = TGCrpv
060900031105     C                   eval      new_TGCfaf = TGCfaf
061000031105     C                   eval      new_TGCsgf = TGCsgf
061100031105     C                   eval      new_TGCggr = TGCggr
061200031105     C                   eval      new_TGCtcm = TGCtcm
061300031105     C                   eval      new_TGCtfg = TGCtfg
061400031105     C                   eval      new_TGCduv = §oggi
061500031105     C                   eval      new_TGCftr = *blanks
061600041116     C                   eval      new_TGCdtr = §oggi
061700110225     c                   if        Waggio=' '
061800031105     C                   write     titgc000
061900110225     c                   endif
062000031105     C                   enddo
062100031105
062200031105     C                   eval      new_TAMksc = TAMksc
062300031105     C                   eval      new_TAMctr = TAMctr
062400031105     C                   eval      new_TAMprg = (kprg + 1)
062500111110     C                   eval      new_TAMddt = 20111201
062600031105     C                   eval      new_TAMdst = §dst
062700031105     C                   eval      new_TAMduv = §oggi
062800031105     C                   eval      new_TAMdcv = TAMdcv
062900031105     C                   eval      new_TAMdif = TAMdif
063000031105     C                   eval      new_TAMfli = TAMfli
063100031105     C                   eval      new_TAMpri = TAMpri
063200031105     C                   eval      new_TAMfmp = TAMfmp
063300031105     C                   eval      new_TAMlrc = TAMlrc
063400031105     C                   eval      new_TAMlas = TAMlas
063500051104     c                   eval      new_TAMpag = TAMpag
063600081202     c                   eval      new_TAMppa = TAMppa
063700051021
063800031105     C                   eval      new_TAMmpa = TAMmpa
063900031105     C                   eval      new_TAMpam = TAMpam
064000031105     C                   eval      new_TAMmpm = TAMmpm
064100031105     C                   eval      new_TAMtin = TAMtin
064200031105     C                   eval      new_TAMkpm = TAMkpm
064300031105     C                   eval      new_TAMarl = TAMarl
064400031105     C                   eval      new_TAMarf = TAMarf
064500031105     C                   eval      new_TAMaro = TAMaro
064600031105     C                   eval      new_TAMrat = TAMrat
064700031105     C                   eval      new_TAMrct = TAMrct
064800101129
064900031105     C                   eval      new_TAMsc2 = TAMsc2
065000031105     C                   eval      new_TAMeds = TAMeds
065100031105     C                   eval      new_TAMars = TAMars
065200031105     C                   eval      new_TAMata = TAMata
065300031105     C                   eval      new_TAMftp = TAMftp
065400031105     C                   eval      new_TAMbap = TAMbap
065500031105     C                   eval      new_TAMtsp = TAMtsp
065600031105     C                   eval      new_TAMfie = TAMfie
065700031105     C                   eval      new_TAMfis = TAMfis
065800031105     C                   eval      new_TAMsis = TAMsis
065900031105     C                   eval      new_TAMfvd = TAMfvd
066000031105     C                   eval      new_TAMfts = TAMfts
066100031105     C                   eval      new_TAMnas = TAMnas
066200031105     C                   eval      new_TAMnot = TAMnot
066300031105     C                   eval      new_TAMftm = TAMftm
066400031105     C                   eval      new_TAMgca = TAMgca
066500031105     C                   eval      new_TAMgga = TAMgga
066600031105     C                   eval      new_TAMgma = TAMgma
066700031105     C                   eval      new_TAMgva = TAMgva
066800031105     C                   eval      new_TAMbam = TAMbam
066900031105     C                   eval      new_TAMtpr = TAMtpr
067000031105     C                   eval      new_TAMflo = TAMflo
067100031105     C                   eval      new_TAMdat = TAMdat
067200031105     C                   eval      new_TAMftr = *blanks
067300041116     C                   eval      new_TAMdtr = §oggi
067400110225     c                   if        Waggio=' '
067500031105     C                   write     tntam000
067600110225     c                   endif
067700110225     c
067800110228     c                   except    stampanew
067900111114      * --> SCRIVO TITAV
068000111114if  1c                   If        Waggio = ' '
068100111114     c                   Clear                   Titav000
068200111114     c                   Eval      tavksc = New_tamksc
068300111114     c                   Eval      tavctr = New_tamctr
068400111114     c                   Eval      tavprg = New_tamprg
068500111114     c                   Eval      tavtrd = 'TES'
068600111114     c                   Eval      tavcta = 'D'
068700111114     c                   Eval      tavdav = *date
068800111114     c                   Time                    w0140
068900111114     c                   Movel     w0140         tavorv
069000111114     c                   Eval      tavnoj = 'AUM_FUEL'
069100111114     c                   Eval      tavpru = 'BATCH'
069200111114     c                   Eval      tavdtr = *date
069300111114     c                   Write     Titav000
069400111114    1c                   EndIf
069500111114
069600031105
069700031105     C                   endsr
069800110224      *****************************************************************
069900110224      * Aggiunta FUEL
070000110224      *****************************************************************
070100110224     C     AGGFUEL       BEGSR
070200110224     c
070300111109      * controllo se Fuel presente
070400111109     c                   eval      kftc = 'f '
070500111114     c                   eval      contatore = 0
070600111114     c                   eval      §agg_tpt = *on
070700111109      * cerco importo percentuale
070800111109     c     ktpt          setll     titpd01l
070900111109    3c                   do        *hival
071000111109     c     ktpt          reade     titpd01l
071100111109      * fine file
071200111109     c                   if        %eof(titpd01l)
071300111109     c                   leave
071400111109     c                   endif
071500111114      * non annullate
071600111114     c                   if        tpdatb <> *blanks
071700111114     c                   iter
071800111114     c                   endif
071900111114      * scaglioni
072000111114     c                   add       1             contatore
072100111114
072200111114      * valorizzo importo tariffa a 0,5 quando è maggiore di zero e minore di 0,5 oppure
072300111114      * e a zero ma è il secondo scaglione
072400111114     c                   If        (TPDitr > 0 and TPDitr < 0,5) or
072500111114     c                             (tpditr = 0 and contatore > 1)
072600111109     c                   eval      TPDitr = 0,5
072700111110     c                   endif
072800111109     C                   eval      TPDftr = *blanks
072900111109     C                   eval      TPDdtr = §oggi
073000111109     c                   if        Waggio=' '
073100111109     C                   update    titpd01
073200111114     c                   eval      §agg_tpt = *on
073300111114     c                   endif
073400111109
073500111109     c                   enddo
073600111114
073700111114     c                   If        §agg_tpt = *on
073800111114      * devo aggiornare anche la data ultima variazione nella testata della tariffa particolare
073900111114     c     ktpt          chain     titpt01l
074000111114     c                   if        %found(titpt01l)
074100111114     c                   eval      TPTduv = §oggi
074200111114     C                   eval      TPTftr = *blanks
074300111114     C                   eval      TPTdtr = §oggi
074400111114     c                   update    titpt01
074500111114      * devo scrivere la variazione
074600111114     c                   Clear                   Titav000
074700111114     c                   Eval      tavksc = tpdksc
074800111114     c                   Eval      tavctr = tpdctr
074900111114     c                   Eval      tavprg = tpdprg
075000111114     c                   Eval      tavtrd = 'PAR'
075100111114     c                   Eval      tavcta = 'M'
075200111114     c                   Eval      tavftc = tpdftc
075300111114     c                   Eval      tavdav = *date
075400111114     c                   Time                    w0140
075500111114     c                   Movel     w0140         tavorv
075600111114     c                   Eval      tavnoj = 'AUM_FUEL'
075700111114     c                   Eval      tavpru = 'BATCH'
075800111114     c                   Eval      tavdtr = *date
075900111114     c                   Write     Titav000
076000111114     c                   Endif
076100111109
076200111114     c                   Endif
076300111114
076400110224     c
076500110224     C                   endsr
076600111111      *****************************************************************
076700111111      * CERCA LA TARIFFA SUCESSIVA E CONTROLLA SE A CAVALLO D'ANNO
076800111111      *****************************************************************
076900111111     C     Sr_CercaDopo  BEGSR
077000111111
077100111111     C                   eval      kprg = (TAMprg - 1)
077200111115     c                   do        *hival
077300111111     C     ktam          chain(n)  tntam01l
077400111111     C                   if        %found(tntam01l)
077500111111     C                             and old_TAMdst >= 20111201
077600111115     C                             and old_TAMddt < 20111201
077700111111      * valorizzo il falg D perchè comunque sia devo sempre scrivere un nuovo progressivo
077800111111     C                   eval      §flagD = *on
077900111115     c                   leave
078000111111
078100111111     C                   endif
078200111115     c                   if        kprg =  0
078300111115     c                   leave
078400111115     c                   endif
078500111115     c                   eval      kprg = (kprg - 1)
078600111115     c                   enddo
078700111111
078800111111     C                   endsr
078900031105      *****************************************************************
079000031105      * ROUTINE INIZIALE
079100031105      *****************************************************************
079200031105     C     *INZSR        BEGSR
079300031105
079400031105     C     *ENTRY        PLIST
079500031105     C                   PARM                    KPJBA
079600110225     c                   movel     kpjbu         WAGGIO            1
079700031105      *
079800031105     c
079900110225     C     Ktam          klist
080000110225     C                   kfld                    TAMksc
080100110225     C                   kfld                    TAMctr
080200110225     C                   kfld                    Kprg
080300110225
080400110224     C     Ktpt          klist
080500031105     C                   kfld                    TAMksc
080600031105     C                   kfld                    TAMctr
080700031105     C                   kfld                    Kprg
080800110224     C                   kfld                    Kftc
080900110225     c     ktam4         klist
081000111109     c                   kfld                    wffksc
081100111110     c                   kfld                    wffctrv
081200111110     c     kfue          klist
081300111111     C                   kfld                    AU_Cliente
081400111111     C                   kfld                    AU_Tariffa
081500111111     C                   kfld                    AU_Progres
081600081201      *
081700081201      *
081800031105      * reperisce data del giorno
081900031105     C                   TIME                    W0140
082000031105      * UDATE IN GGMMAAAA
082100031105     C                   MOVE      W0140         WDTGIO
082200031105      * UDATE IN AAAAMMGG
082300031105     C                   Z-ADD     WDTGIO        G02DAT
082400031105     C                   MOVEL     *BLANK        G02ERR
082500031105     C                   CALL      'XSRDA8'
082600031105     C                   PARM                    WLBDAT
082700031105     C                   Z-ADD     G02INV        §oggi
082800031105
082900031105     C                   endsr
083000110224     OPRTF198   E            TESTA            02
083100111109     O                                            8 'TAVR30R '
083200110302     O                                           62 '* Scrittura Tariffa FUEL *'
083300110302     o               10                         100 '   AGGIORNA ARCHIVI'
083400110302     o              N10                         100 '   SOLO CONTROLLO  '
083500110224     OPRTF198   E            ERRDEC      1
083600110224     o                       tamksc
083700110224     o                       tamctr           +   1
083800110224     o                       tamprg        z  +   1
083900110224     o                       tamfie           +   1
084000110224     o                       tamddt           +   1 '    /  /  '
084100110224     o                       tamdst           +   1 '    /  /  '
084200110225     o                                           49 ' '
084300110225     O                                        +   1 'ERR:Tariffa ancora da '
084400110224     O                                        +   0 'decorrere:sistemare a '
084500110224     O                                        +   0 'mano'
084600110224     OPRTF198   E            ERRpres     1
084700110224     o                       tamksc
084800110224     o                       tamctr           +   1
084900110224     o                       tamprg        z  +   1
085000110224     o                       tamfie           +   1
085100110224     o                       tamddt           +   1 '    /  /  '
085200110224     o                       tamdst           +   1 '    /  /  '
085300110225     o                                           49 ' '
085400110225     O                                        +   1 'ERR:Tariffa FUEL presente'
085500110225     OPRTF198   E            nontrov     1
085600111110     o                       WFFksc
085700111110     o                       WFFctrv          +   1
085800110225     o                                           49 ' '
085900110225     O                                        +   1 'ERR:non trovata Tariffa'
086000110228     OPRTF198   E            nontrov2    1
086100111110     o                       WFFksc
086200111110     o                       WFFctrv          +   1
086300111110     o                       WFFprgv       z  +   1
086400110228     o                                           49 ' '
086500110301     O                                        +   1 'ERR:non trovato progressiv'
086600110301     o                                        +   0 'o tariffa'
086700110225     OPRTF198   E            tariffa     1
086800110225     o                       tamksc
086900110225     o                       tamctr           +   1
087000110225     o                       tamprg        z  +   1
087100110225     o                       tamfie           +   1
087200110225     o                       tamddt           +   1 '    /  /  '
087300110225     o                       tamdst           +   1 '    /  /  '
087400110225     o                                           49 ' '
087500110225     O                                        +   1 'Tariffa da aggiornare '
087600110225     OPRTF198   E            stampanew   1
087700110225     O                                        +   0 '     '
087800110225     o                       new_tamksc       +   1
087900110225     o                       new_tamctr       +   1
088000110225     o                       new_tamprg    z  +   1
088100110225     o                       new_tamfie       +   1
088200110225     o                       new_tamddt       +   1 '    /  /  '
088300110225     o                       new_tamdst       +   1 '    /  /  '
088400110225     o                                           49 ' '
088500110225     O                                        +   1 'Nuovo progressivo creato'
088600110225     OPRTF198   E            stampaupd   1
088700110225     O                                        +   0 '     '
088800110225     o                       tamksc           +   1
088900110225     o                       tamctr           +   1
089000110225     o                       tamprg        z  +   1
089100110225     o                       tamfie           +   1
089200110225     o                       tamddt           +   1 '    /  /  '
089300110225     o                       tamdst           +   1 '    /  /  '
089400110225     o                                           49 ' '
089500110225     O                                        +   1 'Progressivo aggiornato'
089600110225     OPRTF198   E            newfuel     1
089700110225     O                                        +   0 '     '
089800110225     o                       new_tptksc       +   1
089900110225     o                       new_tptctr       +   1
090000110225     o                       new_tptprg    z  +   1
090100110225     o                       new_tptftc       +   1
090200110225     o                       new_tptdpb       +   1 '    /  /  '
090300110225     o                                           49 ' '
090400110225     O                                        +   1 'Fuel inserito'
090500110228     OPRTF198   E            totali      2
090600110228     O                                        +   0 'TARIFFE AGGIORANTE:'
090700110228     O                                        +   2 'da decorrere'
090800110228     o                       totdec        z  +   1
090900111110     O                                        +   2 'decorrenza 01/12'
091000111110     o                       tot0112       z  +   1
091100110228     O                                        +   2 'a cavallo'
091200110228     o                       totcav        z  +   1
091300110228     O                                        +   2 'scadute'
091400110228     o                       totsca        z  +   1
091500110228     o
091600110228     OPRTF198   E            totali      2
091700110228     O                                        +   0 'ERRORI:'
091800110301     O                                        +   2 'non trovata tariffa'
091900110301     o                       totnotar      z  +   1
092000110301     O                                        +   4 'TOT. GENERALE :'
092100110228     o                       GENERal       2  +   1
