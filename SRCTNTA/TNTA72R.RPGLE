000100041123     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200041124
000300041116     H* TNTA72R *-----------------------------------------------------*
000400041116     H*         - LISTA TIPI INVIO ANAGRAFICHE CLIENTI - BATCH        *
000500970225     H*---------------------------------------------------------------*
000600041124
000700050804     FAZORG02L  IF   E           K DISK
000800970225     FTABEL00F  IF   E           K DISK
000900050406     FCNACO06l  IF   E           K DISK    usropn
001000050406     FCNACO04l  IF   E           K DISK    rename(cnaco000:cnaco004)
001100050406     F                                     usropn
001200041118     FCNIND00F  IF   E           K DISK
001300041118     FCNCLP00F  IF   E           K DISK
001400041118     Ffncls01l  IF   E           K DISK
001500041118     Ffnspe01l  IF   E           K DISK
001600041118     Ffnsp201l  IF   E           K DISK
001700041118     FTFNTC01L  IF   E           K DISK
001800041118     FTitgc01l  IF   E           K DISK
001900041123     FTNTA72P   O    E             PRINTER oflind(*In99)
002000041124
002100041116     D L6              S              3  0 DIM(30)
002200041118     D L1              S              3  0 DIM(30)
002300041118     D LEL             S              3  0 DIM(250)
002400041118     D S2F             S              1    DIM(20)
002500041118     D S2Ft            S              1    DIM(20)
002600041124     D S2FD            S             20    DIM(20)
002700041118     D S2h             S              1    DIM(20)
002800041118     D S2ht            S              1    DIM(20)
002900041124     D S2hd            S             20    DIM(20)
003000041118     D S4u             S              1    DIM(20)
003100041119     D S4l             S              3    DIM(30)
003200041125     D S4ld            S             25    DIM(30)
003300041123     D S1t             S              2    DIM(30)
003400041122     D S1td            S             20    DIM(30)
003500041125     D tgi             S              3    DIM(20)
003600041125     D pgi             S              3    DIM(20)
003700041118     D igi             S              1    DIM(20)
003800041125     D tgf             S              3    DIM(20)
003900041125     D pgf             S              3    DIM(20)
004000041118     D igf             S              1    DIM(20)
004100041123
004200041125     d aa              s              3  0
004300041125     d bb              s              3  0
004400041124     d ii              s              3  0
004500041124     d savcli          s                   like(acoksc)
004600050405     d kftt            s                   like(acoftt) inz('3')
004700041124     d xx              s              3  0
004800041124     d yy              s              3  0
004900041123     d wclsag          s              1
005000041123     d wclsfg          s              1
005100041124     d wluogo          s              1    inz('0')
005200041124     d wmail           s              1    inz('0')
005300041124     d wnota           s              1    inz('0')
005400041124     d wriga           s              3  0 inz
005500041124     d wstpgia         s              1    inz('0')
005600041125     d wstpgiaf        s              1    inz('0')
005700041124     d wstpbon         s              1    inz('0')
005800041124     d wstpdan         s              1    inz('0')
005900041125     d zz              s              3  0
006000050627     d wnog1a          s              1    inz(' ')
006100050627     d wnog1e          s              1    inz(' ')
006200050627     d wnog2a          s              1    inz(' ')
006300050627     d wnog2e          s              1    inz(' ')
006400050804     D WARE            s                   like(ORGCAR)
006500050804     d werro           s              1    inz('0')
006600041116
006700041118     D trul06ds      E DS
006800041125     D  LIN                    1     90  0 dim(30)
006900041118     D trul43ds      E DS
007000970224     D KPJBA         E DS
007100041116     D TNTA71DS      E DS
007200041118     d ds2f          e ds
007300041118     d ds2h          e ds
007400041118     d ds4u          e ds
007500041119     d ds4l          e ds
007600041119     d ds1t          e ds
007700041116     d Azuteds       e ds                  Extname(Azute00f)
007800041116     d dDatiute      e ds
007900041116     d Tibs34ds      e ds                  Inz
008000050804     d dute01        e ds
008100050804     d trul31ds      e ds
008200050804     D  $pog                  10    759  0 dim(250)
008300050804     D TRUL31DS2     E DS
008400041124
008500050804     d wabi            s                   like(UteAut)
008600041118     D codut           S                   LIKE(TBLkut) inz(1)
008700041118     D kcod            S                   LIKE(TBLcod)
008800050804     D kkey            S                   LIKE(TBLkey)
008900041118     D kspecod         S                   LIKE(specod)
009000041118     D ksp2tpe         S                   LIKE(sp2tpe) inz('EM')
009100041118     D kspefls         S                   LIKE(spefls) inz('L')
009200041124
009300041119     D CARG1           C                   CONST('APERTURA GIACENZA')
009400041125     D CARG2           C                   CONST('CESSATA GIACENZA ')
009500041125     D CARG3           C                   CONST('AVVISO BONIFICO  ')
009600041125     D CARG4           C                   CONST('PRE/AVVISO DANNO ')
009700041125     D CARG5           C                   CONST('TESTI LETTERA    ')
009800041126     D CARG6           C                   CONST('                  ')
009900041124
010000970228      *---------------------------------------------------------------*
010100050804     c                   if        werro = '1'
010200050804     c                   goto      fine
010300050804     c                   endif
010400050406     c* Se ho un solo p.o. da elaborare utilizzo v.l cnaco06l (p.o.+rag
010500050406     c* altrimenti uso cnaco04l (per rag. sociale)
010600050406     c                   if        lel(2) = *zeros
010700050406     c                   eval      *in11 = *on
010800050406     c                   open      cnaco06l
010900050406     c                   else
011000050406     c                   eval      *in11 = *off
011100050406     c                   open      cnaco04l
011200050406     c                   endif
011300050406     c* Lettura anagrafica clienti
011400050406     c   11kaco1         setll     cnaco06l
011500050406     c  n11kaco2         setll     cnaco04l
011600041118     c                   do        *hival
011700050406     c   11kaco1         reade     cnaco06l
011800050406     c  n11kaco2         reade     cnaco04l
011900050406     c                   if        %eof
012000041118     c                   leave
012100041118     c                   endif
012200130315     c                   if        acoabl <> *blanks
012300041209     c                             and d71abl = 'E'
012400041209     c                   iter
012500041209     c                   endif
012600130315     c                   if        acoabl = *blanks
012700041209     c                             and d71abl = 'S'
012800041209     c                   iter
012900041209     c                   endif
013000050406     c* se sto leggendo tutto cnaco scarto se il p.o. del cliente letto non
013100050406     c* è presente nella schiera dei p.o. da elaborare
013200050406     c                   if        *in11 = *off
013300050406     c                   if        acoflt = *zeros
013400050406     c                   iter
013500050406     c                   else
013600050406     c     acoflt        lookup    lel                                    30
013700050406     c  n30              iter
013800050406     c                   endif
013900050406     c                   endif
014000041118     c* elaboro il record letto
014100041118     c                   exsr      elarcd
014200041118     c                   enddo
014300041118     c*
014400050804     c     fine          tag
014500041118     c                   seton                                        lr
014600041124
014700041124      *---------------------------------------------------------------*
014800041118     C* Elaborazione record di cnaco
014900041124      *---------------------------------------------------------------*
015000041118     C     ELARCD        BEGSR
015100041124
015200041124     c                   Clear                   wriga
015300041124     c                   eval      *In01 = *Off
015400041124
015500041118     c     kindclp       chain     cnind00f
015600041119     c* imposto campi chiave "fissi" per chain su file note
015700041119     c                   movel(P)  dutkci        ntcnk1
015800041119     c                   move      acoksc        ntcnk1
015900041119     c                   clear                   ntcnk2
016000041124
016100041119     c* COMUNICAZ. DI GIACENZA
016200041118     c     d71sl0        ifeq      'X'
016300041118     C     d71sl1        oreq      'X'
016400041118     c                   exsr      elagia
016500041118     c                   endif
016600041124
016700041119     C* AVVISO DI BONIFICO
016800041118     c     d71sl2        ifeq      'X'
016900041124     c                   exsr      avvbon
017000041118     c                   endif
017100041124
017200041119     c* PREAVVISO/AVVISO DI DANNO
017300041119     c     d71sl3        ifeq      'X'
017400041119     c                   exsr      avvdan
017500041119     c                   endif
017600041124
017700041124     c* TESTI LETTERA
017800041119     c     d71sl4        ifeq      'X'
017900041119     c                   exsr      txtlet
018000041119     c                   endif
018100041124
018200041124      * stampa
018300041124if  1c                   If        wriga > 0
018400041124if  2c                   if        wriga = 1
018500041124     c                   clear                   rcd004
018600041126     c****               Eval      p72cli = indcit
018700130315     c                   if        acoabl <> *blanks
018800041207     c                   eval      p72cli = '    (*) ' + indcit
018900041207     c                   else
019000041126     c                   eval      p72cli = '        ' + indcit
019100041207     c                   endif
019200041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
019300041126     c
019400041124if  3c                   if        *in99
019500041124     c                   write     rcd001
019600041124     c                   write     rcd003
019700041124     c                   Eval      *In99 = *Off
019800041124e   3c                   endif
019900041124     c                   write     rcd004
020000041124e   2c                   endif
020100041124
020200041124     c                   clear                   rcd004
020300041124     c                   eval      *In01 = *On
020400041124     c                   write     rcd004
020500041124e   1c                   endif
020600041124
020700041118     c                   endsr
020800041124      *---------------------------------------------------------------*
020900041118     C* OPERAZIONI per comunicazione di giacenza
021000041124      *---------------------------------------------------------------*
021100041118     C     elagia        BEGSR
021200041124
021300041124     c                   reset                   wnota
021400041124     c                   reset                   wstpgia
021500041125     c                   reset                   wstpgiaf
021600050627     c                   reset                   wnog1a
021700050627     c                   reset                   wnog1e
021800050627     c                   reset                   wnog2a
021900050627     c                   reset                   wnog2e
022000041124
022100041124     c     acoksc        chain     fncls01l
022200041124     c                   If        Not %Found(Fncls01l)
022300041124     c                   Clear                   clsflo
022400041124     c                   endif
022500041124
022600041123     c                   eval      wclsag = %subst(clsflo:10:1)
022700041123     c                   eval      wclsfg = %subst(clsflo:1:1)
022800041124
022900041124      * ricerca luogo comunicazione giacenza
023000041124     c                   clear                   trul43ds
023100041124     c                   z-add     acoksc        d43ksc
023200041124     c                   call      'TRUL43R'
023300041124     c                   parm                    trul43ds
023400041124
023500041124      * ricerco decodifica luogo
023600041124     c                   z-add     1             xx
023700041124     c     '005'         lookup    s4l(xx)                                30
023800041124     c  n30              leavesr
023900041124
024000041124      * ricerca nota '88'
024100041124     c                   clear                   ntcrnt
024200041124     c                   movel     '88'          ntctnt
024300041124     c     kntc          chain     tfntc01l
024400041124     c                   if        %found(tfntc01l)
024500041124     c                   eval      wnota = *On
024600041124     c                   endif
024700041124
024800041124      * decodifica nota
024900041124     c                   z-add     1             yy
025000041124     c     '88'          lookup    s1t(yy)                                30
025100041124     c  n30              leavesr
025200041124
025300041124      * ricerca tarifffe con tipo comunicazione diverso
025400041124     c                   exsr      srtitgc
025500041124
025600041124      * ------------ APERTURA GIACENZA ------------------
025700041124if  1c                   if        d71sl0 = 'X'
025800041124
025900041124      * ricerco tipo invio
026000041124     c                   z-add     1             ii
026100041124     c     wclsag        lookup    s2f(ii)                                30
026200041124     c  n30              goto      nogia
026300041124
026400041124      * controllo tipo invio
026500041124if  2c                   If        d71tag <> 'T'
026600041118     c                   select
026700041124     c                   when      d71tag = 'E' and s2ft(ii) <> 'M'
026800041124     c                   goto      nogia
026900041124     c                   when      d71tag = '§' and s2ft(ii) = 'M'
027000041124     c                   goto      nogia
027100041124     c                   when      d71tag = 'F' and s2ft(ii) <> 'F'
027200041124     c                   goto      nogia
027300041124     c                   when      d71tag = 'L' and
027400041124     c                             (s2ft(ii) = 'M' or s2ft(ii) = 'F')
027500041124     c                   goto      nogia
027600041118     c                   endsl
027700041124e   2c                   endif
027800041124
027900041124      * ANOMALIE --> verifico se presenti anomalie
028000041124if  2c                   if        d71an1 = 'S'
028100050404s   3c                   Select
028200050629      * . . . non invia via e-mail ma c'è indirizzo e-mail --> SI ANOMALIA
028300050629     c                   when      s2ft(ii) <> 'M' and
028400050629     c                             ((d43trv='S' and d43trvm='S') or
028500050629     c                             (d43trv='N' and wnota=*on))
028600050629      * . . . non invia via fax o via mail   --> NO ANOMALIA
028700050404w   3c                   When      s2ft(ii) <> 'F' and s2ft(ii) <> 'M'
028800050627     c                   eval      wnog1a = 'N'
028900050629      * . . . invio via fax e c'è numero fax --> NO ANOMALIA
029000041124w   3c                   When      s2ft(ii) = 'F'
029100041124     c                   if        d43trv = 'S' and d43fax <> *Blanks
029200050627     c                   eval      wnog1a = 'N'
029300041124     c                   endif
029400041124     c                   If        d43trv = 'N' and indtlf <> *Blanks
029500050627     c                   eval      wnog1a = 'N'
029600041124     c                   endif
029700050629      * . . . invio via e-mail e c'è ind.Email-> NO ANOMALIA
029800041124w   3c                   when      s2ft(ii) = 'M'
029900041124     c                   if        d43trv = 'S' and d43trvm = 'S'
030000050627     c                   eval      wnog1a = 'N'
030100041124     c                   EndIf
030200041125     c                   If        d43trv = 'N' and wnota = *On
030300050627     c                   eval      wnog1a = 'N'
030400041124     c                   endif
030500041124e   3c                   endsl
030600041124e   2c                   endif
030700041124
030800041124      * ECCEZIONI --> verifico se presenti eccezioni
030900041125if  2c                   if        d71ec0 = 'S' and
031000041125     c                             d43trv = 'N' and tgi(1) = *Blanks
031100050627     c                   eval      wnog1e = 'N'
031200041124e   2c                   endif
031300041124
031400041124      * STAMPO
031500050627   1bc                   if        (d71an1='S' and wnog1a=' ') or
031600050627     c                             (d71ec0='S' and wnog1e=' ') or
031700050628     c                             d71an1='N' and d71ec0='N'
031800050627     c
031900041124     c                   eval      wstpgia = *On
032000041124      * --> cliente
032100041124     c                   If        savcli <> acoksc
032200041124     c                   Movel(p)  AcoKsc        p72cli
032300041124     c                   eval      p72cli = %trim(p72cli) + ' ' + acorag
032400041124     c                   eval      savcli = acoksc
032500041124     c                   endif
032600041124      * --> argomento
032700041125     c                   eval      p72arg = carg1 + ' ' + wclsag + '-'
032800041125     c                             + s2fd(ii)
032900041124      * --> ubicazione
033000041124      * luogo se c'è
033100041124if  2c                   if        d43trv = 'S'
033200041125     c                   Eval      p72ubi = 'LUOGO  005-' + s4ld(xx)
033300041124     c                   eval      p72fax = d43fax
033400041124     c                   clear                   p72ema
033500041124     c                   if        d43trvm = 'S'
033600041124     c                   eval      p72ema = d43mail
033700041124     c                   endif
033800041124      * riga di stampa
033900041124if  3c                   if        *in99
034000041124     c                   write     rcd001
034100041124     c                   write     rcd003
034200041124     c                   Eval      *In99 = *Off
034300041124e   3c                   endif
034400041124     c                   write     rcd004
034500041124     c                   eval      wriga = wriga + 1
034600041124e   2c                   endif
034700041124      * se già stampata una riga
034800041124     c                   If        wriga = 1
034900130315     c                   if        acoabl <> *blanks
035000041207     c                   eval      p72cli = '    (*) ' + indcit
035100041207     c                   else
035200041126     c                   eval      p72cli = '        ' + indcit
035300041207     c                   endif
035400041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
035500041124     c                   clear                   p72arg
035600041124     c                   EndIf
035700041124      * anagrafica/nota
035800041125     c                   eval      p72ubi = 'ANAGRAFICA/NOTA 88-' + s1td(yy)
035900041124     c                   eval      p72fax = indtlf
036000041124     c                   eval      p72ema = ntcrnt
036100041124      * riga di stampa
036200041124if  2c                   if        *in99
036300041124     c                   write     rcd001
036400041124     c                   write     rcd003
036500041124     c                   Eval      *In99 = *Off
036600041124e   2c                   endif
036700041124     c                   write     rcd004
036800041124     c                   eval      wriga = wriga + 1
036900041125      * --> Tariffe
037000041125do  2c                   do        20            aa
037100041125     c                   if        tgi(aa) = *blanks
037200041125     c                   Leave
037300041125     c                   EndIf
037400041125     c                   z-add     1             bb
037500041125     c     igi(aa)       lookup    s2f(bb)                                30
037600041126     c                   eval      p72arg = carg6 + igi(aa) +  '-'
037700041125     c                             + s2fd(bb)
037800041126     c                   eval      p72ubi = 'TARIFFA/Prg.  - '
037900041125     c                             + tgi(aa) + '/' + pgi(aa)
038000041125      * se già stampata una riga
038100041125     c                   If        wriga = 1
038200130315     c                   if        acoabl <> *blanks
038300041207     c                   eval      p72cli = '    (*) ' + indcit
038400041207     c                   else
038500041126     c                   eval      p72cli = '        ' + indcit
038600041207     c                   endif
038700041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
038800041125     c                   EndIf
038900041125     c                   If        wriga = 2
039000041125     c                   Clear                   p72cli
039100041125     c                   EndIf
039200041125     c                   clear                   p72fax
039300041125     c                   clear                   p72ema
039400041125      * riga di stampa
039500041125if  3c                   if        *in99
039600041125     c                   write     rcd001
039700041125     c                   write     rcd003
039800041125     c                   Eval      *In99 = *Off
039900041125e   3c                   endif
040000041125     c                   write     rcd004
040100041125     c                   eval      wriga = wriga + 1
040200041125e   2c                   enddo
040300041124
040400050627e  1bc                   endif
040500041124e   1c                   endif
040600041124
040700041124     c     nogia         tag
040800041124
040900041124
041000041124      * ------------ FINE GIACENZA -------------
041100041124if  1c                   if        d71sl1 = 'X'
041200041124
041300041124      * ricerco tipo invio
041400041124     c                   z-add     1             ii
041500041124     c     wclsfg        lookup    s2h(ii)                                30
041600041124     c  n30              leavesr
041700041124
041800041124      * controllo tipo invio
041900041124if  2c                   If        d71tcg <> 'T'
042000041124     c                   select
042100041124     c                   when      d71tcg = 'E' and s2ht(ii) <> 'M'
042200041124     c                   leavesr
042300041124     c                   when      d71tcg = '§' and s2ht(ii) = 'M'
042400041124     c                   leavesr
042500041124     c                   when      d71tcg = 'F' and s2ht(ii) <> 'F'
042600041124     c                   leavesr
042700041124     c                   when      d71tcg = 'L' and
042800041124     c                             (s2ht(ii) = 'M' or s2ht(ii) = 'F')
042900041124     c                   leavesr
043000041124     c                   endsl
043100041124e   2c                   endif
043200041124
043300041124      * ANOMALIE --> verifico se presenti anomalie
043400041124if  2c                   if        d71an2 = 'S'
043500041124s   3c                   select
043600050629      * . . . non invia via e-mail ma c'è indirizzo e-mail --> SI ANOMALIA
043700050629     c                   when      s2ht(ii) <> 'M' and
043800050629     c                             ((d43trv='S' and d43trvm='S') or
043900050629     c                             (d43trv='N' and wnota=*on))
044000050629      * . . . non invia via fax o via mail --> NO ANOMALIA
044100050404w   3c                   When      s2ht(ii) <> 'F' and s2ht(ii) <> 'M'
044200050627     c                   movel     'N'           wnog2a
044300050629      * . . . invio via fax e cè numero fax--> NO ANOMALIA
044400041124w   3c                   when      s2ht(ii) = 'F'
044500041124     c                   if        d43trv = 'S' and d43fax <> *Blanks
044600050627     c                   movel     'N'           wnog2a
044700041124     c                   endif
044800050404     c                   if        d43trv = 'N' and indtlf <> *Blanks
044900050627     c                   movel     'N'           wnog2a
045000050404     c                   endif
045100050629      * . . . invio via e-mail e c'è ind.Email-> NO ANOMALIA
045200041124w   3c                   when      s2ht(ii) = 'M'
045300041124     c                   if        d43trv = 'S' and d43trvm = 'S'
045400050627     c                   movel     'N'           wnog2a
045500041124     c                   EndIf
045600041125     c                   If        d43trv = 'N' and wnota = *On
045700050627     c                   movel     'N'           wnog2a
045800041124     c                   endif
045900041124e   3c                   endsl
046000041124e   2c                   endif
046100041124
046200041124      * ECCEZIONI --> verifico se presenti eccezioni
046300041125if  2c                   if        d71ec1 = 'S' and
046400041125     c                             d43trv = 'N' and tgf(1) = *Blanks
046500050627     c                   movel     'N'           wnog2e
046600041124e   2c                   endif
046700041124
046800041124      * STAMPO
046900050627   1bc                   if        (d71an2='S' and wnog2a=' ') or
047000050627     c                             (d71ec1='S' and wnog2e=' ') or
047100050628     c                             d71an2='N' and d71ec1='N'
047200041124      * --> cliente
047300041124     c                   If        savcli <> acoksc
047400041124     c                   Movel(p)  AcoKsc        p72cli
047500041124     c                   eval      p72cli = %trim(p72cli) + ' ' + acorag
047600041124     c                   eval      savcli = acoksc
047700041124     c                   endif
047800041124      * --> argomento
047900041125     c                   eval      p72arg = carg2 + ' ' + wclsfg + '-'
048000041125     c                             + s2hd(ii)
048100041124      * --> ubicazione
048200041124      * luogo se c'è
048300041124if  2c                   if        d43trv = 'S'
048400041125     c                   eval      wstpgiaf = *On
048500041125     c                   eval      p72ubi = 'LUOGO 005-' + s4ld(xx)
048600041124     c                   eval      p72fax = d43fax
048700041124     c                   clear                   p72ema
048800041124     c                   if        d43trvm = 'S'
048900041124     c                   eval      p72ema = d43mail
049000041124     c                   endif
049100041124      * se già stampata una riga
049200041124     c                   If        wriga = 2
049300041124     c                   Clear                   p72cli
049400041124     c                   EndIf
049500041124      * riga di stampa
049600041124if  3c                   if        *in99
049700041124     c                   write     rcd001
049800041124     c                   write     rcd003
049900041124     c                   Eval      *In99 = *Off
050000041124e   3c                   endif
050100041124     c                   write     rcd004
050200041124     c                   eval      wriga = wriga + 1
050300041124e   2c                   endif
050400041124      * se già stampata una riga
050500041124     c                   If        wriga = 1
050600130315     c                   if        acoabl <> *blanks
050700041207     c                   eval      p72cli = '    (*) ' + indcit
050800041207     c                   else
050900041126     c                   eval      p72cli = '        ' + indcit
051000041207     c                   endif
051100041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
051200041125     c                   if        wstpgiaf = *On
051300041124     c                   clear                   p72arg
051400041124     c                   endif
051500041124     c                   EndIf
051600041125     c                   If        wriga = 2
051700041125     c                   Clear                   p72cli
051800041125     c                   EndIf
051900041124     c                   If        wriga = 3
052000041124     c                   Clear                   p72cli
052100041125     c                   if        wstpgiaf = *On
052200041124     c                   clear                   p72arg
052300041125     c                   endif
052400041124     c                   EndIf
052500041124      * anagrafica/nota
052600041125     c                   eval      p72ubi = 'ANAGRAFICA/NOTA 88-' + s1td(yy)
052700041124     c                   eval      p72fax = indtlf
052800041124     c                   eval      p72ema = ntcrnt
052900041124      * riga di stampa
053000041124if  2c                   if        *in99
053100041124     c                   write     rcd001
053200041124     c                   write     rcd003
053300041124     c                   Eval      *In99 = *Off
053400041124e   2c                   endif
053500041124     c                   write     rcd004
053600041124     c                   eval      wriga = wriga + 1
053700041125      * --> Tariffe
053800041125do  2c                   do        20            aa
053900041125     c                   if        tgf(aa) = *blanks
054000041125     c                   Leave
054100041125     c                   EndIf
054200041125     c                   z-add     1             bb
054300041125     c     igf(aa)       lookup    s2h(bb)                                30
054400041126     c                   eval      p72arg = carg6 + igf(aa) +  '-'
054500041125     c                             + s2hd(bb)
054600041126     c                   eval      p72ubi = 'TARIFFA/Prg.  - '
054700041125     c                             + tgf(aa) + '/' + pgf(aa)
054800041125      * se già stampata una riga
054900041125     c                   If        wriga = 1
055000130315     c                   if        acoabl <> *blanks
055100041207     c                   eval      p72cli = '    (*) ' + indcit
055200041207     c                   else
055300041126     c                   eval      p72cli = '        ' + indcit
055400041207     c                   endif
055500041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
055600041125     c                   EndIf
055700041125     c                   If        wriga >= 2
055800041125     c                   Clear                   p72cli
055900041125     c                   EndIf
056000041125     c                   clear                   p72fax
056100041125     c                   clear                   p72ema
056200041125      * riga di stampa
056300041125if  3c                   if        *in99
056400041125     c                   write     rcd001
056500041125     c                   write     rcd003
056600041125     c                   Eval      *In99 = *Off
056700041125e   3c                   endif
056800041125     c                   write     rcd004
056900041125     c                   eval      wriga = wriga + 1
057000041125e   2c                   enddo
057100041124
057200050627e  1bc                   endif
057300041124e   1c                   endif
057400041124
057500041118     c                   endsr
057600041124
057700041124      *---------------------------------------------------------------*
057800041118     C* Ricerca tariffe di giacenza con tipo invio comunicaz. giac <>
057900041124      *---------------------------------------------------------------*
058000041118     C     srtitgc       BEGSR
058100041124
058200041118     c                   clear                   tgi
058300041118     c                   clear                   pgi
058400041118     c                   clear                   igi
058500041118     c                   clear                   tgf
058600041118     c                   clear                   pgf
058700041118     c                   clear                   igf
058800041125     c                   clear                   aa
058900041125     c                   clear                   bb
059000041124
059100041118     c     acoksc        setll     titgc01l
059200041123do  1c                   do        *hival
059300041118     c     acoksc        reade     titgc01l
059400041118     c                   if        %eof(titgc01l)
059500041118     c                   leave
059600041118     c                   endif
059700041125      * apertura giacenza
059800041125if  2c                   If        d71sl0 = 'X' and tgctcm <> *Blanks and
059900041124     c                             tgctcm <> wclsag
060000041125     c                   z-add     1             ii
060100041125     c     wclsag        lookup    s2f(ii)                                30
060200041125     c                   z-add     1             bb
060300041125     c     tgctcm        lookup    s2f(bb)                                30
060400041125if  3c                   if        s2ft(ii) <> s2ft(bb)
060500041118     c* carico in schiera cod. tariffa, progessivo e tipo invio
060600041124     c                   add       1             aa
060700041125     c                   move      tgcctr        tgi(aa)
060800041125     c                   move      tgcprg        pgi(aa)
060900041124     c                   movel     tgctcm        igi(aa)
061000041125e   3c                   endif
061100041123e   2c                   endif
061200041125      * cessata giacenza
061300041124     c                   If        d71sl1 = 'X' and tgctfg <> *Blanks and
061400041124     c                             tgctfg <> wclsfg
061500041125     c                   z-add     1             ii
061600041125     c     wclsfg        lookup    s2h(ii)                                30
061700041125     c                   z-add     1             bb
061800041125     c     tgctcm        lookup    s2h(bb)                                30
061900041125if  3c                   if        s2ht(ii) <> s2ht(bb)
062000041118     c* carico in schiera cod. tariffa, progessivo e tipo invio
062100041124     c                   add       1             bb
062200041125     c                   move      tgcctr        tgf(bb)
062300041125     c                   move      tgcprg        pgf(bb)
062400041125     c                   movel     tgctfg        igf(bb)
062500041125e   3c                   endif
062600041125e   2c                   endif
062700041123e   1c                   enddo
062800041124
062900041118     c                   endsr
063000041124
063100041124      *---------------------------------------------------------------*
063200041118     C* OPERAZIONI per avviso di bonifico
063300041124      *---------------------------------------------------------------*
063400041118     C     avvbon        BEGSR
063500041124
063600041124     c                   reset                   wnota
063700041124     c                   reset                   wmail
063800041124     c                   reset                   wluogo
063900041124     c                   reset                   wstpbon
064000041124
064100041124     c     kindclp       chain     cnclp00f
064200041125     c                   If        clpfpc = *Blanks
064300041125     c                   Leavesr
064400041125     c                   EndIf
064500041124     c     clpfpc        lookup    s4u                                    30
064600041124     c  n30              Leavesr
064700041124
064800041124      * Cerco luogo 555
064900041124     c                   clear                   spefax
065000041119     c                   eval      kspecod = '555'
065100041119     c     kspe          chain     fnspe01l
065200041124      * se non trovato luogo e richiesta stampa solo con eccez. scarto
065300041124     c                   if        Not %found(fnspe01l) and d71ec2 = 'S'
065400041124     c                   Leavesr
065500041124     c                   Endif
065600041124     c                   If        %Found(Fnspe01l)
065700041124     c                   eval      wluogo = *On
065800041124     c                   endif
065900041124
066000041124      * decodifica luogo
066100041124     c                   z-add     1             xx
066200041124     c     '555'         lookup    s4l(xx)                                30
066300041124     c  n30              Leavesr
066400041124
066500041124      * cerco email
066600041124     c                   clear                   sp2est
066700041119     c     ksp2          chain     fnsp201l
066800041124     c                   if        %found(Fnsp201l) and sp2est <> *Blanks
066900041124     c                   eval      wmail = *On
067000041124     c                   endif
067100041124
067200041124      * cerco la nota '85'
067300041124     c                   clear                   ntcrnt
067400041119     c                   movel     '85'          ntctnt
067500041119     c     kntc          chain     tfntc01l
067600041119     c                   if        %found(tfntc01l)
067700041124     c                   eval      wnota = *On
067800041119     c                   endif
067900041124
068000041124      * decodifica nota
068100041124     c                   z-add     1             yy
068200041124     c     '85'          lookup    s1t(yy)                                30
068300041124     c  n30              leavesr
068400041119
068500041125      * Controllo il tipo invio
068600041119     c* tipo invio = EMail e non ho ind. EMail cliente da scartare
068700041119     c                   select
068800041125     c                   when      d71tca = 'E' and wmail = *Off and
068900041124     c                             wnota = *Off
069000041124     c                   leavesr
069100041125     c* tipo invio = Lettera e ho ind Email cliente da scartare
069200041124     c                   when      (d71tca = 'L' or d71tca = '§') and
069300041124     c                             (wmail = *On or wnota = *On)
069400041124     c                   leavesr
069500041119     c                   endsl
069600041125
069700041125      * ECCEZIONI --> verifico se presenti eccezioni
069800041125if  1c                   if        d71ec2 = 'S' and wluogo = *Off
069900041125     c                   leavesr
070000041125e   1c                   EndIf
070100041124
070200041124      * STAMPO
070300041124      * --> cliente
070400041124     c                   If        savcli <> acoksc
070500041124     c                   Movel(p)  AcoKsc        p72cli
070600041124     c                   eval      p72cli = %trim(p72cli) + ' ' + acorag
070700041124     c                   eval      savcli = acoksc
070800041124     c                   endif
070900041124      * --> argomento
071000041125     c                   eval      p72arg = carg3 + '  -'
071100041125     c                   if        wmail = *On or wnota = *On
071200041125     c                   eval      p72arg = %trim(p72arg) + 'E-mail'
071300041125     c                   else
071400041125     c                   eval      p72arg = %trim(p72arg) + 'Lettera'
071500041124     c                   endif
071600041124      * --> ubicazione
071700041124      * luogo se c'è
071800041124if  1c                   if        wluogo = *On
071900041125     c                   eval      p72ubi = 'LUOGO 555-' + s4ld(xx)
072000041125     c                   clear                   p72fax
072100041124     c                   clear                   p72ema
072200041125     c                   if        wmail = *On
072300041124     c                   eval      p72ema = sp2est
072400041124     c                   endif
072500041124      * se già stampata una riga
072600041124     c                   If        wriga = 1
072700130315     c                   if        acoabl <> *blanks
072800041207     c                   eval      p72cli = '    (*) ' + indcit
072900041207     c                   else
073000041126     c                   eval      p72cli = '        ' + indcit
073100041207     c                   endif
073200041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
073300041124     c                   EndIf
073400041124     c                   If        wriga >= 2
073500041124     c                   Clear                   p72cli
073600041124     c                   EndIf
073700041124      * riga di stampa
073800041124if  2c                   if        *in99
073900041124     c                   write     rcd001
074000041124     c                   write     rcd003
074100041124     c                   Eval      *In99 = *Off
074200041124e   2c                   endif
074300041124     c                   write     rcd004
074400041124     c                   eval      wriga = wriga + 1
074500041124     c                   eval      wstpbon = *On
074600041124e   1c                   endif
074700041124      * se già stampata una riga
074800041124     c                   If        wriga = 1
074900130315     c                   if        acoabl <> *blanks
075000041207     c                   eval      p72cli = '    (*) ' + indcit
075100041207     c                   else
075200041126     c                   eval      p72cli = '        ' + indcit
075300041207     c                   endif
075400041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
075500041124     c                   If        wstpbon = *On
075600041124     c                   Clear                   p72arg
075700041124     c                   endif
075800041124     c                   EndIf
075900041124     c                   If        wriga >= 2
076000041124     c                   Clear                   p72cli
076100041124     c                   If        wstpbon = *On
076200041124     c                   Clear                   p72arg
076300041124     c                   endif
076400041124     c                   EndIf
076500041124      * anagrafica/nota
076600041125     c                   eval      p72ubi = 'ANAGRAFICA/NOTA 85-' + s1td(yy)
076700041125     c                   clear                   p72fax
076800041124     c                   eval      p72ema = ntcrnt
076900041124      * riga di stampa
077000041124if  2c                   if        *in99
077100041124     c                   write     rcd001
077200041124     c                   write     rcd003
077300041124     c                   Eval      *In99 = *Off
077400041124e   2c                   endif
077500041124     c                   write     rcd004
077600041124     c                   eval      wriga = wriga + 1
077700041124
077800041118     c                   endsr
077900041124
078000041124      *---------------------------------------------------------------*
078100041119     C* OPERAZIONI per Pre/avviso di danno
078200041124      *---------------------------------------------------------------*
078300041119     C     avvdan        BEGSR
078400041124
078500041124     c                   reset                   wnota
078600041124     c                   reset                   wmail
078700041124     c                   reset                   wluogo
078800041124     c                   reset                   wstpdan
078900041124
079000041124      * Cerco luogo 006
079100041124     c                   clear                   spefax
079200041119     c                   eval      kspecod = '006'
079300041119     c     kspe          chain     fnspe01l
079400041124     c                   if        Not %found(fnspe01l) and d71ec3 = 'S'
079500041124     c                   Leavesr
079600041124     c                   Endif
079700041124     c                   If        %Found(Fnspe01l)
079800041124     c                   eval      wluogo = *On
079900041124     c                   endif
080000041124
080100041124      * decodifica luogo
080200041124     c                   z-add     1             xx
080300041124     c     '006'         lookup    s4l(xx)                                30
080400041124     c  n30              Leavesr
080500041124
080600041124      * cerco email
080700041124     c                   clear                   sp2est
080800041119     c     ksp2          chain     fnsp201l
080900041124     c                   if        %found(Fnsp201l) and sp2est <> *Blanks
081000041124     c                   eval      wmail = *On
081100041124     c                   endif
081200041124
081300041124      * cerco la nota '87'
081400041124     c                   clear                   ntcrnt
081500041124     c                   movel     '87'          ntctnt
081600041124     c     kntc          chain     tfntc01l
081700041124     c                   if        %found(tfntc01l)
081800041124     c                   eval      wnota = *On
081900041124     c                   endif
082000041124
082100041124      * decodifica nota
082200041124     c                   z-add     1             yy
082300041124     c     '87'          lookup    s1t(yy)                                30
082400041124     c  n30              leavesr
082500041124
082600041125      * Controllo il tipo invio
082700041119     c* tipo invio = EMail e non ho ind. EMail cliente da scartare
082800041119     c                   select
082900041124     c                   when      d71tpd = 'E' and wmail = *Off and
083000041124     c                             wnota = *Off
083100041124     c                   leavesr
083200041119     c* tipo invio = no email e ho ind. email --> cliente da scartare
083300041124     c                   when      d71tpd = '§' and
083400041124     c                             (wmail = *On or wnota = *On)
083500041124     c                   leavesr
083600041119     c* tipo invio = fax e ho ind email o non ho il fax scarto
083700041124     c                   when      d71tpd = 'F' and
083800041125     c                             (spefax = *Blanks or indtlf = *Blanks)
083900041124     c                   leavesr
084000041119     c* tipo invio = lettera e ho email o il fax scarto
084100041124     c                   when      d71tpd = 'L' and
084200041124     c                             (wmail = *On or wnota = *On or
084300041124     c                              spefax <> *Blanks or indtlf <> *Blanks)
084400041124     c                   leavesr
084500041119     c                   endsl
084600041125
084700041125      * ECCEZIONI --> verifico se presenti eccezioni
084800041125if  1c                   if        d71ec3 = 'S' and wluogo = *Off
084900041125     c                   leavesr
085000041125e   1c                   EndIf
085100041124
085200041124      * STAMPO
085300041124      * --> cliente
085400041124     c                   If        savcli <> acoksc
085500041124     c                   Movel(p)  AcoKsc        p72cli
085600041124     c                   eval      p72cli = %trim(p72cli) + ' ' + acorag
085700041124     c                   eval      savcli = acoksc
085800041124     c                   endif
085900041124      * --> argomento
086000041125     c                   eval      p72arg = carg4 + '  -'
086100041124     c                   select
086200041125     c                   when      wmail = *On or wnota = *On
086300041125     c                   eval      p72arg = %trim(p72arg) + 'E-mail'
086400041125     c                   when      spefax <> *Blanks or indtlf <> *Blanks
086500041125     c                   eval      p72arg = %trim(p72arg) + 'Fax'
086600041125     c                   other
086700041125     c                   eval      p72arg = %trim(p72arg) + 'Lettera'
086800041124     c                   endsl
086900041124      * --> ubicazione
087000041124      * luogo se c'è
087100041124if  1c                   if        wluogo = *On
087200041125     c                   eval      p72ubi = 'LUOGO 006-' + s4ld(xx)
087300041124     c                   eval      p72fax = spefax
087400041124     c                   clear                   p72ema
087500041125     c                   if        wmail = *On
087600041124     c                   eval      p72ema = sp2est
087700041124     c                   endif
087800041124      * se già stampata una riga
087900041124     c                   If        wriga = 1
088000130315     c                   if        acoabl <> *blanks
088100041207     c                   eval      p72cli = '    (*) ' + indcit
088200041207     c                   else
088300041126     c                   eval      p72cli = '        ' + indcit
088400041207     c                   endif
088500041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
088600041124     c                   EndIf
088700041124     c                   If        wriga >= 2
088800041124     c                   Clear                   p72cli
088900041124     c                   EndIf
089000041124      * riga di stampa
089100041124if  2c                   if        *in99
089200041124     c                   write     rcd001
089300041124     c                   write     rcd003
089400041124     c                   Eval      *In99 = *Off
089500041124e   2c                   endif
089600041124     c                   write     rcd004
089700041124     c                   eval      wriga = wriga + 1
089800041124     c                   eval      wstpdan = *On
089900041124e   1c                   endif
090000041124      * se già stampata una riga
090100041124     c                   If        wriga = 1
090200130315     c                   if        acoabl <> *blanks
090300041207     c                   eval      p72cli = '    (*) ' + indcit
090400041207     c                   else
090500041126     c                   eval      p72cli = '        ' + indcit
090600041207     c                   endif
090700041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
090800041124     c                   If        wstpdan = *On
090900041124     c                   Clear                   p72arg
091000041124     c                   endif
091100041124     c                   EndIf
091200041124     c                   If        wriga >= 2
091300041124     c                   Clear                   p72cli
091400041124     c                   If        wstpdan = *On
091500041124     c                   Clear                   p72arg
091600041124     c                   endif
091700041124     c                   endif
091800041124      * anagrafica/nota
091900041125     c                   eval      p72ubi = 'ANAGRAFICA/NOTA 87-' + s1td(yy)
092000041124     c                   eval      p72fax = indtlf
092100041124     c                   eval      p72ema = ntcrnt
092200041124      * riga di stampa
092300041124if  2c                   if        *in99
092400041124     c                   write     rcd001
092500041124     c                   write     rcd003
092600041124     c                   Eval      *In99 = *Off
092700041124e   2c                   endif
092800041124     c                   write     rcd004
092900041124     c                   eval      wriga = wriga + 1
093000041124
093100041119     c                   endsr
093200041124
093300041124      *---------------------------------------------------------------*
093400041119     C* OPERAZIONI per testi lettera
093500041124      *---------------------------------------------------------------*
093600041119     C     txtlet        BEGSR
093700041124
093800041124     c                   reset                   wnota
093900041124
094000041119     c* cerco la nota '06'
094100041124     c                   clear                   ntcrnt
094200041119     c                   movel     '06'          ntctnt
094300041119     c     kntc          chain     tfntc01l
094400041119     c                   if        %found(tfntc01l)
094500041124     c                   eval      wnota = *On
094600041119     c                   endif
094700041124
094800041124      * decodifica nota
094900041124     c                   z-add     1             yy
095000041124     c     '06'          lookup    s1t(yy)                                30
095100041124     c  n30              leavesr
095200041124
095300041125      * Controllo il tipo invio
095400041119     c* tipo invio = EMail e non ho ind. EMail
095500041119     c                   select
095600041124     c                   when      d71ttx = 'E' and wnota = *Off
095700041124     c                   Leavesr
095800041125     c* tipo invio = No Email e ho l'ind. email
095900041125     c                   when      d71ttx = '§' and wnota = *On
096000041125     c                   leavesr
096100041125     c* tipo invio = Fax e non ho numero fax
096200041125     c                   when      d71ttx = 'F' and indtlf = *Blanks
096300041124     c                   leavesr
096400041119     c* tipo invio = Lettera e ho numero fax o l'in. email
096500041124     c                   when      d71ttx = 'L' and
096600041124     c                             (indtlf <> *Blanks or wnota = *On)
096700041124     c                   leavesr
096800041119     c                   endsl
096900041124
097000041124      * STAMPO
097100041124      * --> cliente
097200041124     c                   If        savcli <> acoksc
097300041124     c                   Movel(p)  AcoKsc        p72cli
097400041124     c                   eval      p72cli = %trim(p72cli) + ' ' + acorag
097500041124     c                   eval      savcli = acoksc
097600041124     c                   endif
097700041124      * --> argomento
097800041125     c                   eval      p72arg = carg5 + '  -'
097900041124     c                   select
098000041124     c                   when      wnota = *On
098100041125     c                   eval      p72arg = %trim(p72arg) + 'E-mail'
098200041124     c                   when      indtlf <> *Blanks
098300041125     c                   eval      p72arg = %trim(p72arg) + 'Fax'
098400041124     c                   other
098500041125     c                   eval      p72arg = %trim(p72arg) + 'Lettera'
098600041124     c                   endsl
098700041124      * --> ubicazione
098800041124      * se già stampata una riga
098900041124     c                   If        wriga = 1
099000130315     c                   if        acoabl <> *blanks
099100041207     c                   eval      p72cli = '    (*) ' + indcit
099200041207     c                   else
099300041126     c                   eval      p72cli = '        ' + indcit
099400041207     c                   endif
099500041126     c                   Eval      p72cli = %trimr(p72cli) + ' ' + indprv
099600041124     c                   EndIf
099700041124     c                   If        wriga >= 2
099800041124     c                   Clear                   p72cli
099900041124     c                   EndIf
100000041124      * anagrafica/nota
100100041125     c                   eval      p72ubi = 'ANAGRAFICA/NOTA 06-' + s1td(yy)
100200041124     c                   eval      p72fax = indtlf
100300041124     c                   eval      p72ema = ntcrnt
100400041124      * riga di stampa
100500041124if  1c                   if        *in99
100600041124     c                   write     rcd001
100700041124     c                   write     rcd003
100800041124     c                   Eval      *In99 = *Off
100900041124e   1c                   endif
101000041124     c                   write     rcd004
101100041124     c                   eval      wriga = wriga + 1
101200041124
101300041119     c                   endsr
101400041124
101500041124      *---------------------------------------------------------------*
101600970224     C* OPERAZIONI INIZIALI
101700041124      *---------------------------------------------------------------*
101800970224     C     *INZSR        BEGSR
101900970224     C*
102000970224     C     *ENTRY        PLIST
102100970224     C                   PARM                    KPJBA
102200041116     C                   MOVEL     KPJBU         tnta71ds
102300970224     C*
102400041116     c     *dtaara       define    §azute        azuteds
102500041116     c     *dtaara       define    §datiute      ddatiute
102600041116     c                   in(E)     *dtaara
102700041116     c                   If        %error  or RSUT = *blanks
102800041116     c                   CLEAR                   tibs34ds
102900041116     c                   CALL      'TIBS34R'
103000041116     c                   PARM                    tibs34ds
103100041116     c                   in        *dtaara
103200041116     c                   EndIf
103300041118     c*
103400970224     C                   TIME                    WTIME            14 0
103500970224     C                   MOVE      WTIME         WDATE             8 0
103600970311     C                   MOVEL     WTIME         UTIME             6 0
103700041118     C* CARICO SCHIERA DEI P.O DA ELABORARE
103800041118     c                   clear                   lel
103900050804    1c     d71pop        ifgt      *zeros
104000041118     C* Richiesto un p.o.:carico schiera dei po gestiti dal p.o. passato
104100041118     C                   CLEAR                   trul06ds
104200000404     C                   MOVE      '£6'          D06COD
104300041123     c                   Movel     d71pop        d06key
104400000404     C                   MOVEL     'C'           D06ERR
104500041123     C                   MOVEL     Trul06ds      KPJBU
104600000404     C                   CALL      'TRUL06R'
104700000404     C                   PARM                    KPJBA
104800041118     C                   MOVEL     KPJBU         trul06ds
104900041118     C                   MOVEL     LIN           lel
105000050804   x1c                   else
105100050804     c* se non richiesto un distretto o un area specifiche i p.o. da
105200050804     c* elaborare sono tutti i p.o. gestibili da profilo utente --> trul31r
105300050804    2c                   if        d71cdi = *blanks and (d71car = *blanks or
105400050804     c                             d71car = *zeros)
105500050804     c* richiamo trul31r per ottenere la schiera dei p.o. da elaborare in ba
105600050804     c* alle abilitazioni del profilo utente
105700050804     c                   Clear                   wabi
105800050804      * Verifica errori e autorità profilo
105900050804s   3c                   Select
106000050804      * se ho errori nei dati utente esco dal pgm
106100050804w   3c                   When      DutErr = 'E'
106200050804     c                   eval      werro = '1'
106300050804     c                   GoTo      Finsr
106400050804      * se non c'è l'abilitazione
106500050804      * --> se 1° livello, abilitazioni al terminal
106600050804      *     se 2° livello, abilitazioni al punto operativo
106700050804w   3c                   When      UteAut = *Blanks
106800050804if  4c                   If        DutLpo = '1'
106900050804     c                   Eval      wabi   = 'TP'
107000050804e   4c                   EndIf
107100050804if  4c                   If        DutLpo = '2'
107200050804     c                   Eval      wabi   = 'PO'
107300050804e   4c                   EndIf
107400050804if  4c                   If        DutLpo = 'S'
107500050804     c                   Eval      wabi   = 'AZ'
107600050804e   4c                   EndIf
107700050804      * carica le abilitazioni del profilo
107800050804x   3c                   Other
107900050804     c                   Movel     UteFaf        Dute01
108000050804    4c                   If        §UteCli <> *Blanks
108100050804     c                   Eval      wabi = §UteCli
108200050804   x4c                   Else
108300050804     c                   Eval      wabi = UteAut
108400050804    4c                   EndIf
108500050804e   3c                   EndSl
108600050804
108700050804      * Reperimento dei P.O. gestibili dall'utente
108800050804     c                   clear                   TRUL31DS
108900050804     c                   clear                   TRUL31DS2
109000050804     c                   eval      I31abi = wabi
109100050804     c                   eval      I31cdi = DUTdis
109200050804     c                   eval      I31car = DUTare
109300050804     c                   eval      I31cpo = DUTpou
109400050804     c                   call      'TRUL31R'
109500050804     c                   parm                    KPJBA
109600050804     c                   parm                    TRUL31DS
109700050804     c                   parm                    TRUL31DS2
109800050804if  3c                   if        o31pog > *zeros
109900050804     c                   movea     $pog          lel
110000050804x   3c                   else
110100050804     c                   eval      werro = '1'
110200050804     c                   goto      finsr
110300050804e   3c                   endif
110400050804   x2c                   else
110500050804     c* se richiesto distretto o distretto+area carico tutti i p.o. leggendo
110600050804     c* azorg
110700050804    3c                   if        d71car > *zeros
110800050804     c                   move      d71car        ware
110900050804     c     korg          setll     azorg02l
111000050804   x3c                   else
111100050804     c     d71cdi        setll     azorg02l
111200050804    3c                   endif
111300050804     C                   Z-ADD     1             C                 4 0
111400050804    3c                   do        *hival
111500050804    4c                   if        d71car > *zeros
111600050804     c     korg          reade     azorg02l
111700050804   x4c                   else
111800050804     c     d71cdi        reade     azorg02l
111900050804    4c                   endif
112000050804    4c                   if        %eof(azorg02l)
112100050804     c                   leave
112200050804    4c                   endif
112300050804    4c                   if        orgfag='F' or orgfag='A'
112400050804    5C     ORGFVA        IFEQ      ' '
112500050804     c                   z-add     orgfil        lel(c)
112600050804     C                   ADD       1             C
112700050804    5c                   endif
112800050804    4c                   endif
112900050804    3c                   enddo
113000050804    2c                   endif
113100050804    1c                   endif
113200041118     c* carico schiera 2F
113300041118     c     d71sl0        ifeq      'X'
113400041118     c                   movel(p)  '2F'          kcod
113500041124     c                   z-add     0             ii
113600041118     c                   clear                   s2f
113700041118     c                   clear                   s2ft
113800041119     c                   clear                   s2fd
113900041118     c     ktab          setll     tabel00f
114000041118     c                   do        *hival
114100041118     c     ktab          reade     tabel00f
114200041118     c                   if        %eof(tabel00f)
114300041118     c                   leave
114400041118     c                   endif
114500041118     c                   add       1             ii
114600041118     c                   movel     tblkey        s2f(ii)
114700041118     c                   movel     tbluni        ds2f
114800041118     c                   movel     §2fftp        s2ft(ii)
114900041119     c                   movel     §2fdes        s2fd(ii)
115000041118     c                   enddo
115100041118     c                   endif
115200041118     c
115300041118     c* carico schiera 2h
115400041118     c     d71sl1        ifeq      'X'
115500041118     c                   movel(p)  '2H'          kcod
115600041124     c                   z-add     0             ii
115700041118     c                   clear                   s2h
115800041118     c                   clear                   s2ht
115900041119     c                   clear                   s2hd
116000041118     c     ktab          setll     tabel00f
116100041118     c                   do        *hival
116200041118     c     ktab          reade     tabel00f
116300041118     c                   if        %eof(tabel00f)
116400041118     c                   leave
116500041118     c                   endif
116600041118     c                   add       1             ii
116700041118     c                   movel     tblkey        s2h(ii)
116800041118     c                   movel     tbluni        ds2h
116900041118     c                   movel     §2hfax        s2ht(ii)
117000041119     c                   movel     §2hdes        s2hd(ii)
117100041118     c                   enddo
117200041118     c                   endif
117300041118     c* carico schiera "4U" tutti i tipi pagamenti a mezzo bonifico
117400041118     c     d71sl2        ifeq      'X'
117500041118     c                   movel(p)  '4U'          kcod
117600041124     c                   z-add     0             ii
117700041118     c                   clear                   s4u
117800041118     c     ktab          setll     tabel00f
117900041118     c                   do        *hival
118000041118     c     ktab          reade     tabel00f
118100041118     c                   if        %eof(tabel00f)
118200041118     c                   leave
118300041118     c                   endif
118400041118     c                   movel     tbluni        ds4u
118500041118     c     §4ucod        ifeq      'B'
118600041118     c                   add       1             ii
118700041118     c                   movel     tblkey        s4u(ii)
118800041118     c                   endif
118900041118     c                   enddo
119000041118     c                   endif
119100041119     c* carico tabella "4L" dei codici luogo
119200041119     c                   movel(P)  '4L'          kcod
119300041124     c                   z-add     0             ii
119400041119     c                   clear                   s4l
119500041119     c                   clear                   s4ld
119600041119     c     ktab          setll     tabel00f
119700041119     c                   do        *hival
119800041119     c     ktab          reade     tabel00f
119900041119     c                   if        %eof(tabel00f)
120000041119     c                   leave
120100041119     c                   endif
120200041119     c                   movel     tbluni        ds4l
120300041119     c                   add       1             ii
120400041119     c                   movel     tblkey        s4l(ii)
120500041119     c                   movel     §4ldes        s4ld(ii)
120600041119     c                   enddo
120700041122     c* carico tabella "1T" dei tipi nota
120800041122     c                   movel(P)  '1T'          kcod
120900041124     c                   z-add     0             ii
121000041122     c                   clear                   s1t
121100041122     c                   clear                   s1td
121200041122     c     ktab          setll     tabel00f
121300041122     c                   do        *hival
121400041122     c     ktab          reade     tabel00f
121500041122     c                   if        %eof(tabel00f)
121600041122     c                   leave
121700041122     c                   endif
121800041122     c                   movel     tbluni        ds1t
121900041122     c                   add       1             ii
122000041122     c                   movel     tblkey        s1t(ii)
122100041122     c                   movel     §1tdes        s1td(ii)
122200041122     c                   enddo
122300050804     c* decodifica distretto se richiesto
122400050804     c                   clear                   p71ddst
122500050804     c                   if        d71cdi <> *blanks
122600050804     c                   movel(p)  '17'          Kcod
122700050804     c                   movel(p)  d71cdi        kkey
122800050804     c     ktab1         chain     tabel                              30
122900050804     c  N30              movel     tbluni        p71ddst
123000050804     c                   endif
123100050804     c* decodifica area se richiesta
123200050804     c                   clear                   p71car
123300050804     c                   clear                   p71dcar
123400050804     c                   if        d71car > *zeros
123500050804     c                   move      d71car        p71car
123600050804     c                   movel(P)  '05'          kcod
123700050804     c                   movel(p)  d71car        kkey
123800050804     c     ktab1         chain     tabel                              30
123900050804     c  n30              movel     tbluni        p71dcar
124000050804     c                   endif
124100041123
124200041209     c* decodifica clienti bloccati
124300041209     c                   select
124400041209     c                   when      d71abl = 'I'
124500041209     c                   eval      p71dbl = 'Inclusi'
124600041209     c                   when      d71abl = 'E'
124700041209     c                   eval      p71dbl = 'Esclusi'
124800041209     c                   when      d71abl = 'S'
124900041209     c                   eval      p71dbl = 'Solo bloccati'
125000041209     c                   endsl
125100041123     c                   write     rcd001
125200041123     c                   write     par001
125300041123      * tipo comunicazione
125400041123      * apertura giacenza
125500041123     c                   If        d71sl0 = 'X'
125600041124     c                   eval      *In02 = *On
125700041124     c                   eval      *In03 = *On
125800041123     c                   Eval      p72des = carg1
125900050804     c***                Eval      p72tpi = d71tag
126000041123     c                   select
126100041123     c                   when      d71tag = 'E'
126200041123     c                   Eval      p72dtp = 'E-Mail'
126300041123     c                   when      d71tag = '§'
126400041123     c                   Eval      p72dtp = 'No E-Mail'
126500041123     c                   when      d71tag = 'F'
126600041123     c                   Eval      p72dtp = 'Fax'
126700041123     c                   when      d71tag = 'L'
126800041123     c                   Eval      p72dtp = 'Lettera'
126900041123     c                   Other
127000041123     c                   Eval      p72dtp = 'Tutti'
127100041123     c                   EndSl
127200041123     c                   eval      p72ecc = d71ec0
127300041123     c                   eval      p72anm = d71an1
127400041123     c                   write     rcd002
127500041123     c                   endif
127600041123      * fine giacenza
127700041123     c                   If        d71sl1 = 'X'
127800041124     c                   eval      *In02 = *On
127900041124     c                   eval      *In03 = *On
128000041123     c                   Eval      p72des = carg2
128100050804     c***                Eval      p72tpi = d71tcg
128200041123     c                   select
128300041123     c                   when      d71tcg = 'E'
128400041123     c                   Eval      p72dtp = 'E-Mail'
128500041123     c                   when      d71tcg = '§'
128600041123     c                   Eval      p72dtp = 'No E-Mail'
128700041123     c                   when      d71tcg = 'F'
128800041123     c                   Eval      p72dtp = 'Fax'
128900041123     c                   when      d71tcg = 'L'
129000041123     c                   Eval      p72dtp = 'Lettera'
129100041123     c                   Other
129200041123     c                   Eval      p72dtp = 'Tutti'
129300041123     c                   EndSl
129400041123     c                   eval      p72ecc = d71ec1
129500041123     c                   eval      p72anm = d71an2
129600041123     c                   write     rcd002
129700041123     c                   endif
129800041123      * bonifico
129900041123     c                   If        d71sl2 = 'X'
130000041124     c                   eval      *In02 = *Off
130100041124     c                   eval      *In03 = *On
130200041123     c                   Eval      p72des = carg3
130300050804     c***                Eval      p72tpi = d71tca
130400041123     c                   select
130500041123     c                   when      d71tca = 'E'
130600041123     c                   Eval      p72dtp = 'E-Mail'
130700041123     c                   when      d71tca = '§'
130800041123     c                   Eval      p72dtp = 'No E-Mail'
130900041123     c                   when      d71tca = 'F'
131000041123     c                   Eval      p72dtp = 'Fax'
131100041123     c                   when      d71tca = 'L'
131200041123     c                   Eval      p72dtp = 'Lettera'
131300041123     c                   Other
131400041123     c                   Eval      p72dtp = 'Tutti'
131500041123     c                   EndSl
131600041123     c                   eval      p72ecc = d71ec2
131700041123     c                   clear                   p72anm
131800041123     c                   write     rcd002
131900041123     c                   endif
132000041123      * avviso danno
132100041123     c                   If        d71sl3 = 'X'
132200041124     c                   eval      *In02 = *Off
132300041124     c                   eval      *In03 = *On
132400041123     c                   Eval      p72des = carg4
132500050804     c***                Eval      p72tpi = d71tpd
132600041123     c                   select
132700041123     c                   when      d71tpd = 'E'
132800041123     c                   Eval      p72dtp = 'E-Mail'
132900041123     c                   when      d71tpd = '§'
133000041123     c                   Eval      p72dtp = 'No E-Mail'
133100041123     c                   when      d71tpd = 'F'
133200041123     c                   Eval      p72dtp = 'Fax'
133300041123     c                   when      d71tpd = 'L'
133400041123     c                   Eval      p72dtp = 'Lettera'
133500041123     c                   Other
133600041123     c                   Eval      p72dtp = 'Tutti'
133700041123     c                   EndSl
133800041123     c                   eval      p72ecc = d71ec3
133900041123     c                   clear                   p72anm
134000041123     c                   write     rcd002
134100041123     c                   endif
134200041123      * lettera
134300041123     c                   If        d71sl4 = 'X'
134400041124     c                   eval      *In02 = *Off
134500041124     c                   eval      *In03 = *Off
134600041123     c                   Eval      p72des = carg5
134700050804     c***                Eval      p72tpi = d71ttx
134800041123     c                   select
134900041123     c                   when      d71ttx = 'E'
135000041123     c                   Eval      p72dtp = 'E-Mail'
135100041123     c                   when      d71ttx = '§'
135200041123     c                   Eval      p72dtp = 'No E-Mail'
135300041123     c                   when      d71ttx = 'F'
135400041123     c                   Eval      p72dtp = 'Fax'
135500041123     c                   when      d71ttx = 'L'
135600041123     c                   Eval      p72dtp = 'Lettera'
135700041123     c                   Other
135800041123     c                   Eval      p72dtp = 'Tutti'
135900041123     c                   EndSl
136000041123     c                   clear                   p72ecc
136100041123     c                   clear                   p72anm
136200041123     c                   write     rcd002
136300041123     c                   endif
136400041123
136500041123     c                   write     rcd001
136600041123     c                   write     rcd003
136700041118     c
136800041118     C* key accesso cnaco
136900041118     c     kaco1         klist
137000050405     c                   kfld                    codut
137100050405     c                   kfld                    kftt
137200050406     c                   kfld                    lel(1)
137300041118     c                   kfld                    dutkci
137400050406     c     kaco2         klist
137500050406     c                   kfld                    codut
137600050406     c                   kfld                    dutkci
137700041118     C* key accesso cnind e cnclp
137800041118     c     kindclp       klist
137900041118     c                   kfld                    acokut
138000041118     c                   kfld                    acokcc
138100041118     c                   kfld                    acoksc
138200041118     C* key accesso fnsp201l
138300041118     c     ksp2          klist
138400041118     c                   kfld                    acoksc
138500041118     c                   kfld                    kspecod
138600041118     c                   kfld                    ksp2tpe
138700041118     C* key accesso fnspe01l
138800041118     c     kspe          klist
138900041118     c                   kfld                    kspefls
139000041118     c                   kfld                    acoksc
139100041118     c                   kfld                    kspecod
139200041118     C     Kntc          KLIST
139300041118     C                   KFLD                    ntcapl
139400041118     C                   KFLD                    ntcnk1
139500041118     C                   KFLD                    ntcnk2
139600041118     C                   KFLD                    ntctnt
139700041118     c                   eval      ntcapl='C'
139800970224     C*
139900970224     C     KTAB          KLIST
140000970224     C                   KFLD                    CODUT
140100970224     C                   KFLD                    KCOD
140200050804     c*
140300050804     C     KTAB1         KLIST
140400050804     C                   KFLD                    CODUT
140500050804     C                   KFLD                    KCOD
140600050804     C                   KFLD                    Kkey
140700050804     c*
140800050804     c     kORG          klist
140900050804     c                   kfld                    d71cdi
141000050804     c                   kfld                    ware
141100970226     C*
141200050804     C     finsr         ENDSR
