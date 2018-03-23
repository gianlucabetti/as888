000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200070206     h dftactgrp(*no) actgrp(*caller)
000300070322     H* FIDG22R *----------------------------------------------------*
000400070322     H*         Assegnaz.distinta di consegna auto.
000500070322     H*         Aggiorna sequenza lettura bolla e fermata x giro
000600070322     H*--------------------------------------------------------------*
000700070222     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000800070205     Ffiarg00f  uf   E             DISK    commit
000900070206     FAZORG01L  IF   E           K DISK
001000070206     Ftntbe01l  if   E           k DISK
001100100203     Ftabel00f  if   E           k DISK
001101180307     Fcnclp00f  if   E           k DISK
001200070206      *--------------------------------------------------------------*
001300070206      *   DS
001400070206      *--------------------------------------------------------------*
001500070206     D fnarbds       e ds                  extname(fnarb00f)
001600070206     D fiargds       e ds                  extname(fiarg00f)
001701150430     D ddatiute      e ds
001800030910     D azuteds       e ds                  extname(AZUTE00F)
001900100202     D dargflo       E DS                  inz
002000100202     D tibs34ds      E DS                  inz
002100070206     D dslb          E DS                  inz
002200100203     D ds7r          E DS                  inz
002201160421     D ds03          E DS                  inz
002300950524     D KPJBA         E DS
002400070206     D Fidg22ds      E DS
002500070206     D Fidg16ds      E DS
002600070523     d trul82ds      e ds
002700130423     d TNSD99DS      e ds
002800070206     D OG146         E DS                  INZ
002900071107     D OG147         E DS                  INZ
003000071107     D OG150         E DS                  INZ
003001180313     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003002180313     D  TCU                  398    697
003003180313     D                                     DIM(50)                              TAB FLAG C
003004180313     D  KCU                  698    847P 0
003005180313     D                                     DIM(50)                              TAB CAPO C
003006180313     D                                     PACKEVEN
003007180313      *
003008180313     D TCUDS           DS
003009180313     D  F1                     1      1
003010180313     D  F3                     3      3
003011180313     D  F2                     2      2
003012180313     D  F4                     4      4
003013180313     D  F56                    5      6
003200180313     d DCR             s              4  0 dim(4)
003300070827     drrnarg           s              9  0
003400070206     drrnarb           s              9  0
003500070906     dcomdam           s               d
003600070827     dcomdfv           s               d
003700070827     ddtaiso           s               d
003800070206     ddtaeur           s               d   datfmt(*eur)
003900070206     dcomnftl          s                   like(argnftl)
004000070206     dcomslb           s                   like(argslb)
004100070222     dsavnftl          s                   like(argnftl)
004200070222     dsavslb           s                   like(argslb)
004300070206     ddsrsdind         s             70
004400070206     dsavrsdind        s                   like(dsrsdind)
004500100203     dslbattesa        s                   like(argslb)
004600100203     dmaxslb           s                   like(argslb)
004700070424     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
004800070424     D WLBDAT          DS                  INZ
004900070424     D  G02DAT                 1      8  0
005000070424     D  G02INV                 9     16  0
005100070424     D  G02ERR                17     17
005200070424     D  G02TGI                18     22  0
005300070523     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
005400950524     C*--------------------------------------------------------------*
005500950524     C*  Ciclo principale
005600950524     C*--------------------------------------------------------------*
005700070209     c* prima attibuisco la sequenza di lettura bolla e poi il numero della
005800070209     c* fermata
005900070209     c*
006000070205     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
006100070410     c* se il p.o. è partito testo la distinta su ARB
006200070209     c*
006300070209     c* per il momento aggiorno solo ARG perchè ARB lo aggiornerò alla fine
006400070205     c* quando calcolo anche gli stop
006500070424     c                   if        comdda= 20391231 or comdda > dateu8
006600061003     C/EXEC SQL
006700070206     C+ DECLARE A1 CURSOR FOR SELECT rrn(fiarg01l), fiarg01l.*,
006800070206     C+ fnarb01l.* FROM fiarg01l join fnarb01l on arbaas= argaas and
006900070206     C+ arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp WHERE argndc
007000070412     C+ = 0 and argcgi=:d22cgii and argpoc = :d22fgsi and argslb = 0
007100061003     C/END-EXEC
007200061003
007300061003     C/EXEC SQL
007400070205     C+ OPEN a1
007500061003     C/END-EXEC
007600070205     c                   else
007700070206     C/EXEC SQL
007800070206     C+ DECLARE B1 CURSOR FOR SELECT rrn(fiarg01l), fiarg01l.*,
007900070206     C+ fnarb01l.* FROM fiarg01l join fnarb01l on arbaas= argaas and
008000070206     C+ arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp WHERE arbndc
008100070412     C+ = 0 and argcgi=:d22cgii and argndc = 0 and argslb = 0
008200070214     C+ and argpoc = :d22fgsi
008300070205     C/END-EXEC
008400070205
008500070205     C/EXEC SQL
008600070205     C+ OPEN b1
008700070205     C/END-EXEC
008800070205     c                   end
008900061003
009000061003     C                   do        *hival
009100061003
009200070424     c                   if        comdda= 20391231 or comdda > dateu8
009300061003     C/EXEC SQL
009400070209     C+ FETCH NEXT FROM a1 INTO :rrnarg, :fiargds, :fnarbds
009500061003     C/END-EXEC
009600070205     c                   else
009700070205     C/EXEC SQL
009800070209     C+ FETCH NEXT FROM b1 INTO :rrnarg, :fiargds, :fnarbds
009900070205     C/END-EXEC
010000070205     c                   end
010100061003
010200061003     C                   select
010300061003     C                   when      sqlcod = 100
010400061003     c                   leave
010500070205     C                   WHEN      SqlCod < 0
010600070205     c                   if        sqlcod <> -913
010700070205     C                   seton                                        H1
010800070209     c                   end
010900070222     c                   seton                                        83
011000070222     c                   exsr      srstampa
011100070206     c                   exsr      srfine
011200070206     C                   other
011300070206     C                   exsr      gessped
011400061003     C                   endsl
011500061003     C*
011600061003     C                   enddo
011700061003
011800070424     c                   if        comdda= 20391231 or comdda > dateu8
011900061003     C/EXEC SQL
012000070205     C+ CLOSE a1
012100061003     C/END-EXEC
012200070205     c                   else
012300070205     C/EXEC SQL
012400070205     C+ CLOSE b1
012500070205     C/END-EXEC
012600070205     c                   end
012700070222     c*
012800070222     c* ora che ho la sequenza della bolla calcolo il numero della fermata
012900070222     c*
013000070209     c                   eval      comnftl = 1
013100070424     c                   if        comdda= 20391231 or comdda > dateu8
013200070209     C/EXEC SQL
013300070209     C+ DECLARE C1 CURSOR FOR SELECT rrn(fiarg01l), fiarg01l.*,
013400070209     C+ fnarb01l.* FROM fiarg01l join fnarb01l on arbaas= argaas and
013500070209     C+ arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp WHERE argndc
013600070214     C+ = 0 and argcgi=:d22cgii and argslb <> 0 and argpoc = :d22fgsi
013700070214     C+ ORDER BY arbrsd, arbind
013800070214     C* ARGXLO, ARGYLA
013900070209     C/END-EXEC
014000070209
014100070209     C/EXEC SQL
014200070209     C+ OPEN c1
014300070209     C/END-EXEC
014400070209     c                   else
014500070209     C/EXEC SQL
014600070209     C+ DECLARE D1 CURSOR FOR SELECT rrn(fiarg01l), fiarg01l.*,
014700070209     C+ fnarb01l.* FROM fiarg01l join fnarb01l on arbaas= argaas and
014800070209     C+ arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp WHERE arbndc
014900070412     C+ = 0 and argcgi=:d22cgii and argslb <>0 and argndc = 0
015000070214     C+ and argpoc = :d22fgsi
015100070214     C* ORDER BY arbrsd, arbind, argxlo, argyla
015200070214     C+ ORDER BY arbrsd, arbind
015300070209     C/END-EXEC
015400070209
015500070209     C/EXEC SQL
015600070209     C+ OPEN d1
015700070209     C/END-EXEC
015800070209     c                   end
015900070209
016000070209     C                   do        *hival
016100070209
016200070424     c                   if        comdda= 20391231 or comdda > dateu8
016300070209     C/EXEC SQL
016400070209     C+ FETCH NEXT FROM c1 INTO :rrnarg, :fiargds, :fnarbds
016500070209     C/END-EXEC
016600070209     c                   else
016700070209     C/EXEC SQL
016800070209     C+ FETCH NEXT FROM d1 INTO :rrnarg, :fiargds, :fnarbds
016900070209     C/END-EXEC
017000070209     c                   end
017100070209
017200070209     C                   select
017300070209     C                   when      sqlcod = 100
017400070209     c                   leave
017500070209     C                   WHEN      SqlCod < 0
017600070209     c                   if        sqlcod <> -913
017700070209     C                   seton                                        H1
017800070209     c                   end
017900070222     c                   seton                                        83
018000070222     c                   exsr      srstampa
018100070209     c                   exsr      srfine
018200070209     C                   other
018300070209     C                   exsr      gessped1
018400070209     C                   endsl
018500070209     C*
018600070209     C                   enddo
018700070209
018800070424     c                   if        comdda= 20391231 or comdda > dateu8
018900070209     C/EXEC SQL
019000070209     C+ CLOSE c1
019100070209     C/END-EXEC
019200070209     c                   else
019300070209     C/EXEC SQL
019400070209     C+ CLOSE d1
019500070209     C/END-EXEC
019600070209     c                   end
019700070222     c*
019800070222     c* ora che ho la sequenza della bolla e il numero della fermata
019900070222     c* attribuisco la stessa sequenza bolla a parità di fermata
020000070222     c*
020100070222     c                   clear                   savnftl
020200070222     c                   clear                   savslb
020300070424     c                   if        comdda= 20391231 or comdda > dateu8
020400070222     C/EXEC SQL
020500100202     C+ DECLARE E1 CURSOR FOR SELECT rrn(fiarg01l), fiarg01l.* FROM
020600100202     C+ fiarg01l join fnarb01l on arbaas= argaas and arblnp=arglnp and
020700100202     C+ arbnrs= argnrs and arbnsp= argnsp WHERE argndc = 0 and
020800100202     C+ argcgi=:d22cgii and argslb <> 0 and argpoc = :d22fgsi and argnftl
020900100203     C+ <> 0 ORDER BY argnftl, argslb desc, substr(argflo, 1, 1)
021000070222     C/END-EXEC
021100070222
021200070222     C/EXEC SQL
021300070222     C+ OPEN e1
021400070222     C/END-EXEC
021500070222     c                   else
021600070222     C/EXEC SQL
021700100203     C+ DECLARE F1 CURSOR FOR SELECT rrn(fiarg01l), fiarg01l.* FROM
021800100203     C+ fiarg01l join fnarb01l on arbaas= argaas and arblnp=arglnp and
021900100203     C+ arbnrs= argnrs and arbnsp= argnsp WHERE arbndc = 0 and
022000100203     C+ argcgi=:d22cgii and argslb <>0 and argndc = 0 and argpoc =
022100100203     C+ :d22fgsi and argnftl <> 0 ORDER BY argnftl, argslb desc,
022200100203     C+ substr(argflo, 1, 1)
022300070222     C/END-EXEC
022400070222
022500070222     C/EXEC SQL
022600070222     C+ OPEN f1
022700070222     C/END-EXEC
022800070222     c                   end
022900070222
023000070222     C                   do        *hival
023100070222
023200070424     c                   if        comdda= 20391231 or comdda > dateu8
023300070222     C/EXEC SQL
023400070222     C+ FETCH NEXT FROM e1 INTO :rrnarg, :fiargds
023500070222     C/END-EXEC
023600070222     c                   else
023700070222     C/EXEC SQL
023800070222     C+ FETCH NEXT FROM f1 INTO :rrnarg, :fiargds
023900070222     C/END-EXEC
024000070222     c                   end
024100070222
024200070222     C                   select
024300070222     C                   when      sqlcod = 100
024400070222     c                   leave
024500070222     C                   WHEN      SqlCod < 0
024600070222     c                   if        sqlcod <> -913
024700070222     C                   seton                                        H1
024800070222     c                   end
024900070222     c                   seton                                        83
025000070222     c                   exsr      srstampa
025100070222     c                   exsr      srfine
025200070222     C                   other
025300100203     c* imposto la sequenza di lettura maggiore a tutte le spedizioni
025400100203     c* della fermata
025500070222     c                   if        savnftl <> argnftl
025600070222     c                   eval      savslb = argslb
025700070222     c                   eval      savnftl = argnftl
025800100203     c* se la sequenza maggiore della fermata è senza attesa (1° argflo =S)
025900100203     c* vado in consegna, altrimenti devo vedere se ho raggiunto la seq.
026000100203     c* per la consegna oggi, in caso contrario devo controllare se nella
026100100203     c* fermata ci sono spedizioni con sequenza minore senza attesa
026200100202     c                   if        %subst(argflo: 1: 1) ='S'
026300100202     c                             and argslb < slbattesa
026400100203     c* verifico se cio sono sped. senza attesa
026500100203     c                   exsr      gessped3
026600100203     c                   eval      savslb = maxslb
026700100203     c                   end
026800100202     c                   end
026900100203     c                   if        savslb <> argslb
027000070222     C                   exsr      gessped2
027100100203     c                   end
027200070222     C                   endsl
027300070222     C*
027400070222     C                   enddo
027500070222
027600070424     c                   if        comdda= 20391231 or comdda > dateu8
027700070222     C/EXEC SQL
027800070222     C+ CLOSE e1
027900070222     C/END-EXEC
028000070222     c                   else
028100070222     C/EXEC SQL
028200070222     C+ CLOSE f1
028300070222     C/END-EXEC
028400070222     c                   end
028500950525     C*
028600070206     c                   if        d22cmti= 'S'
028700070206     c                   commit
028800070206     c                   end
028900070323     c                   eval      d22pag = pages
029000070323     c                   eval      d22riga= riga
029100070206     c*
029200070206     c                   eval      kpjbu = fidg22ds
029300950524     C                   SETON                                        LR
029400070222     c*--------------------------------------------------------------*
029500070205     C*  Gestione singola spedizione
029600030708     C*--------------------------------------------------------------*
029700070205     C     GESsped       BEGSR
029800070205     c* chiamo pgm per vedere se spedizione da mettere in distinta
029900070205     c                   exsr      srspedcon
030000070206     c* stampo la spedizione esclusa dalla consegna
030100070206     c                   if        d16conso = 'N'
030200070329     c                   if        d16msgo <> ' '
030300070315     c                   seton                                        85
030400070704     c                   eval      prtaas = D16AASI
030500070704     c                   eval      prtlnp = D16LNPI
030600070704     c                   eval      prtnrs = D16NRSI
030700070704     c                   eval      prtnsp = D16NSPI
030800070704     c                   eval      prtmsg = D16MSGO
030900070315     c                   exsr      srstampa
031000070329     c                   end
031001150717     c                   leavesr
031100150430     c                   end
031101160421     c* nel turno 1 devo escludere le zone del turno 2
031102160426     c                   if        (d22tur1 = '1' or d22tur2 = '1')
031103160506     c                             and d22turno = '1'
031104160421     c                   clear                   ds03
031107160421     C                   eval      tblcod = '03'
031108160421     C                   movel     d22fgsi       com05             5
031109160421     C                   move      arbznc        com05             5
031110160421     C                   movel(p)  com05         tblkey
031111160421     C     ktab          chain     Tabel00f
031112160421     c                   if        %found(tabel00f)
031113160421     C                   MOVEL     TBLUNI        DS03
031114160421     c                   end
031115160421     c                   if        §03turno = '2'
031116160421     c                   seton                                        85
031117160421     c                   eval      prtaas = argaas
031118160421     c                   eval      prtlnp = arglnp
031119160421     c                   eval      prtnrs = argnrs
031120160421     c                   eval      prtnsp = argnsp
031121160421     C                   eval      prtmsg = 'La spedizione è di una zona del se-
031122160421     C                             condo turno'
031123160421     c                   exsr      srstampa
031124160421     c                   leavesr
031125160421     c                   end
031126160421     c                   end
031200070209     c* se spedizione da mettere in distinta
031300070412     c* aggancio ARG
031400070412     c     RRNARG        chain(e)  fiarg00f
031500070412     c                   if        %error
031600070523     c                   exsr      geslck
031700070523     c                   if        ul82§sts = 'A'
031800070704     c                   seton                                        85
031900070704     c                   eval      prtaas = argaas
032000070704     c                   eval      prtlnp = arglnp
032100070704     c                   eval      prtnrs = argnrs
032200070704     c                   eval      prtnsp = argnsp
032300070704     C                   eval      prtmsg = 'La spedizione non è stata aggiorna-
032400070704     C                             ta correttamente.VERIFICARLA'
032500070412     c                   exsr      srstampa
032600070412     c                   leavesr
032700070528     c                   else
032800070528     c     RRNARG        chain     fiarg00f
032900070412     c                   end
033000070523     c                   end
033100070412     c                   if        not %found(fiarg00f)
033200070412     c                   seton                                        84
033300070412     c                   exsr      srstampa
033400070412     c                   leavesr
033500070412     c                   else
033600070209     c*
033700070209     c* calcolo la sequenza di lettura bolla
033800070206     c                   exsr      srslb
033900070206     c                   eval      argslb = comslb
034000070410     c                   eval      argndc = 0
034100070410     c                   eval      argpdc = 0
034200070410     c                   eval      argfgs = 0
034300070410     c                   eval      argddc = 0
034400070410     c                   eval      argstp = 0
034500070410     c                   eval      argnftl= 0
034600070410     c                   eval      argdtvdis= 0
034700070410     c                   eval      arghvdis= 0
034800100202     c* aggiorno il flag di spedizione con attesa se particolarità lo
034900100202     c* richiede
035000100202     c                   clear                   ds7r
035100100202     c                   if        arbgma <> ' '
035200100202     C                   eval      tblcod = '7R'
035300100202     C                   eval      tblkey = arbgma
035400100202     C     ktab          chain     Tabel00f
035500100202     c                   if        %found(tabel00f)
035600100202     C                   MOVEL     TBLUNI        DS7R
035700100202     c                   end
035800100202     c                   end
035900100202     c                   eval      dargflo = argflo
036000100202     c                   eval      §argattesa = §7ratt
036100100202     c                   eval      argflo = dargflo
036200070206     c                   update    fiarg000
036300070205     c                   end
036500950524     C*
036600070222     C                   ENDSR
036700070209     C*--------------------------------------------------------------*
036800070209     C*  Gestione singola spedizione
036900070209     C*--------------------------------------------------------------*
037000070209     C     GESsped1      BEGSR
037100070209     c* calcolo la fermata
037200070209     c                   movel     arbrsd        dsrsdind
037300070209     c                   move      arbind        dsrsdind
037400070214     c*                  movel     argxlo        dsxy
037500070214     c*                  move      argyla        dsxy
037600070209     c                   if        dsrsdind<>savrsdind and savrsdind<>' '
037700070214     c*                             and dsxy <>savxy and savxy<>' '
037800070209     c                   add       1             comnftl
037900070209     c                   end
038000070209     c                   eval      savrsdind = dsrsdind
038100070214     c*                  eval      savxy = dsxy
038200070209     c* aggiorno ARG
038300070209     c     RRNARG        chain(e)  fiarg00f
038400070209     c                   if        %error
038500070523     c                   exsr      geslck
038600070523     c                   if        ul82§sts = 'A'
038700070704     c                   seton                                        85
038800070704     c                   eval      prtaas = argaas
038900070704     c                   eval      prtlnp = arglnp
039000070704     c                   eval      prtnrs = argnrs
039100070704     c                   eval      prtnsp = argnsp
039200070704     C                   eval      prtmsg = 'La spedizione non è stata aggiorna-
039300070704     C                             ta correttamente.VERIFICARLA'
039400070222     c                   exsr      srstampa
039500070209     c                   leavesr
039600070528     c                   else
039700070528     c     RRNARG        chain     fiarg00f
039800070209     c                   end
039900070523     c                   end
040000070209     c*
040100070209     c                   if        %found(fiarg00f)
040200070209     c                   eval      argnftl= comnftl
040300070209     c                   update    fiarg000
040400070209     c                   else
040500070222     c                   seton                                        84
040600070209     c                   exsr      srstampa
040700070209     c                   leavesr
040800070209     c                   end
040900070209     C*
041000070209     C                   ENDSR
041100070222     c*--------------------------------------------------------------*
041200070222     C*  Gestione singola spedizione
041300070222     C*--------------------------------------------------------------*
041400070222     C     GESsped2      BEGSR
041500070222     c* aggiorno ARG
041600070222     c     RRNARG        chain(e)  fiarg00f
041700070222     c                   if        %error
041800070523     c                   exsr      geslck
041900070523     c                   if        ul82§sts = 'A'
042000070704     c                   seton                                        85
042100070704     c                   eval      prtaas = argaas
042200070704     c                   eval      prtlnp = arglnp
042300070704     c                   eval      prtnrs = argnrs
042400070704     c                   eval      prtnsp = argnsp
042500070704     C                   eval      prtmsg = 'La spedizione non è stata aggiorna-
042600070704     C                             ta correttamente.VERIFICARLA'
042700070222     c                   exsr      srstampa
042800070222     c                   leavesr
042900070528     c                   else
043000070528     c     RRNARG        chain     fiarg00f
043100070222     c                   end
043200070523     c                   end
043300070222     c*
043400070222     c                   if        %found(fiarg00f)
043500070222     c                   eval      argslb = savslb
043600070222     c                   update    fiarg000
043700070222     c                   else
043800070222     c                   seton                                        84
043900070222     c                   exsr      srstampa
044000070222     c                   leavesr
044100070222     c                   end
044200070222     C*
044300070222     C                   ENDSR
044400100203      *-----------------------------------------------
044500100203     C     gessped3      begsr
044600100203      *-----------------------------------------------
044700100203     c                   clear                   maxslb
044800100203     c                   if        comdda= 20391231 or comdda > dateu8
044900100203     C/EXEC SQL
045000100203     C+ SELECT max(argslb) INTO :maxslb FROM fiarg01l join fnarb01l on
045100100203     C+ arbaas= argaas and arblnp=arglnp and arbnrs= argnrs and arbnsp=
045200100203     C+ argnsp WHERE argndc = 0 and argcgi=:d22cgii and argslb <> 0 and
045300100203     C+ argpoc = :d22fgsi and argnftl = :savnftl and substr(argflo, 1, 1)
045400100203     C+ = ' '
045500100203     C/END-EXEC
045600100203     c                   else
045700100203     C/EXEC SQL
045800100203     C+ SELECT max(argslb) INTO :maxslb FROM fiarg01l join fnarb01l on
045900100203     C+ arbaas= argaas and arblnp=arglnp and arbnrs= argnrs and arbnsp=
046000100203     C+ argnsp WHERE arbndc = 0 and argcgi=:d22cgii and argslb <>0 and
046100100203     C+ argndc = 0 and argpoc = :d22fgsi and argnftl =:savnftl and
046200100203     C+ substr(argflo, 1, 1) = ' '
046300100203     C/END-EXEC
046400100203     c                   end
046500100203     C                   ENDSR
046600061003      *-----------------------------------------------
046700070205     C     srspedcon     begsr
046800061003      *-----------------------------------------------
046900070205     c                   clear                   fidg16ds
047000141223     c                   eval      D16dtaI = D22dtaI
047100141223     c                   eval      D16oraI = D22oraI
047101150430     c                   eval      D16oraI = D22oraI
047102150430     c                   eval      D16p88I = D22p88I
047200141223     c                   eval      d16aasi = arbaas
047300141223     c                   eval      d16lnpi = arblnp
047400141223     c                   eval      d16nrsi = arbnrs
047500141223     c                   eval      d16nspi = arbnsp
047600141223     c                   eval      d16dfvi = d22dfvi
047700141223     c                   if        dateu8 >= comdda
047800141223     c                   eval      d16ndci = 'E'
047900141223     c                   else
048000141223     c                   eval      d16ndci = 'G'
048100141223     c                   end
048200141223     c                   eval      D16ldcri= D22ldcri
048300141223      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
048400141223      ***  quindi si deve sostituire il test per identificare il Perfect Order
048500141223     c*******            if        arbxco <> ' '
048600141223     c                   if        arbxco > '0'
048700141223     c                   eval      d16prfoI= 'S'
048800141223     c                   end
048900141223     c* se sto assegnando una spedizione presente nel giro DIR
049000141223     c* inibisco alcuni controlli di
049100141223     c* assegnabilità in quanto inutili visto che sto dirottando
049200141223     c* es. la sospensione alla consegna, la data consegna richiesta ecc.)
049300141223     c* questo lo faccio abilitando i singoli flag e non
049400141223     c* quello totale
049500141223     c                   if        argcgi = 'DIR'
049600141223     c                   eval      d16dcmi = 'S'
049700141223     c                   eval      d16fbci = 'S'
049800141223     c                   eval      d16cmci = 'S'
049900141223     c                   eval      d16lblmi= 'S'
050000141223     c                   eval      d16colci= 'S'
050100141223     c                   eval      d16parasgi='S'
050200141223     c                   else
050300141223     c                   eval      d16consi= 'S'
050400141223     c                   end
050500070205     c                   eval      kpjbu = fidg16ds
050600070205     c                   call      'FIDG16R'
050700070205     c                   parm                    kpjba
050800070205     c                   parm                    fnarbds
050900070205     c                   parm                    fiargds
051000070205     c                   eval      fidg16ds = kpjbu
051100070205     C                   ENDSR
051200070827      *-----------------------------------------------
051300070827     C     srslb         begsr
051400070827      *-----------------------------------------------
051500070827     c* imposto sequenza 1 se non trovo record sulla tabella
051600070827     c                   eval      comslb = 1
051700070827     c* aggancio la tabella SLB per reperire la squenza lettura bolla
051800070827     c                   eval      tbecod = 'SLB'
051900110727     c* ARBTSP (tipo servizio espresso H 10,30)
052000070827     c                   if        arbtsp <> ' '
052100070827     c                   eval      tbeke1 = 'TS'
052200070827     c                   eval      tbeke2 = arbtsp
052300070827     c                   exsr      srtbe
052400070827     c                   end
052500110727     c* ARBXCO (perfect order)
052600130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
052700130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
052800130221     c***********        if        arbxco <> ' '
052900130221     c                   if        arbxco > '0'
053000110727     c                   eval      tbeke1 = 'XCO'
053100110727     c                   clear                   tbeke2
053200110727     c                   exsr      srtbe
053300110727     c                   end
053400070827     c* data consegna richiesta e data arrivo trazione
053500070827     c                   exsr      srslb1
053501180307     c* classe cliente
053502180307     c                   if        %subst(%editc(arbksc:'X'):4:4)<>'9999' and
053503180307     c                             %subst(%editc(arbksc:'X'):4:4)<>'8888'
053504180307     c                   exsr      srclv
053505180307     c                   end
053600070827     C*
053700070827     C                   ENDSR
053800070827      *-----------------------------------------------
053900070827     C     srtbe         begsr
054000070827      *-----------------------------------------------
054100070827     C*
054200070827     c     ktbe          chain     tntbe01l
054300070827     c                   if        %found(tntbe01l) and tbeatb = ' '
054400070827     c                   eval      dslb = tbeuni
054500070905     c                   if        D§SLBPRG > comslb
054600070827     c                   eval      comslb = D§SLBPRG
054700070827     c                   end
054800070827     c                   end
054900070827     C*
055000070827     C                   ENDSR
055100070827      *-----------------------------------------------
055200070827     C     srslb1        begsr
055300070827      *-----------------------------------------------
055400070827     c* data consegna richiesta
055500070827     c* Se DCR=0   -> priorità "normale"
055501180302     c* Se DCR=0 cliente vario  -> priorità "normale" - 10
055600070827     c* Se DCR=DDC -> P diventa IL, D scartato da FIDG16R, IL resta IL
055700070827     c* Se DCR>DDC -> P resta P, D o IL scartato da FIFG16R
055800070827     c* Se DCR<DDC -> P o IL spedizioni con DCR=0, D resta D
055900070827     c                   eval      x = 1
056000070827     c                   if        arbdcr = 0 or
056100070827     c                             (arbdcr <> 0 and arbdcr < d22dfvi and
056200070827     c                             (arbtcr = ' ' or arbtcr = 'P'))
056300070827     c                   else
056400070827     c                   select
056500070827     c                   when      arbdcr = d22dfvi
056600070827     c* IL
056700070827     c                   eval      x = 4
056800070827     c                   when      arbdcr > d22dfvi
056900070827     c* può essere solo P perchè gli altri sono scartati ds FIDG16R
057000070827     c                   eval      x = 2
057100100223     c* se Prima del e data dcr domani scatta IL
057200130429     c                   clear                   tnsd99ds
057300130423     C                   Z-ADD     d22dfvi       D98DFC
057400130423     C                   Z-ADD     2359          D98OFC
057500130423     C                   MOVEL     ' '           D98TLA
057600130423     C                   MOVEL     'A'           D98TBO
057700130423     C                   Z-ADD     ARBAAS        D98AAS
057800130423     C                   Z-ADD     ARBLNP        D98LNP
057900130423     C                   Z-ADD     ARBNRS        D98NRS
058000130423     C                   Z-ADD     ARBNSP        D98NSP
058100130423     C                   CALL      'TNSD99R'
058200130423     C                   PARM                    TNSD99DS
058300130423     c                   if        d98nci > -24
058400100223     c                   eval      x = 4
058500100223     c                   end
058600070827     c                   when      arbdcr < d22dfvi and arbtcr ='D'
058700070827     c                   eval      x = 3
058800070827     c                   endsl
058900070827     c                   end
059000070827     c* elemento 1 = nessuna data
059100070827     c* elemento 2 = prima del (TCR='P')
059200070827     c* elemento 3 = dopo il (TCR='D')
059300070827     c* elemento 4 = IL (TCR=' ')
059400070827     c*
059500070827     c* al massimo 9999
059600070827     c                   if        comslb + dcr(x) <= 9999
059700070827     c                   add       dcr(x)        comslb
059800070827     c                   else
059900070827     c                   z-add     9999          comslb
060000070827     c                   end
060100070827     c* data arrivo trazione
060200070906     c* se tra dam e ddc ci sono + di 5 gg imposto 5
060401150417     c                   if        arbdam > 0 and arbdam <=d22dfvi
060500070904     c                   z-add     1             y                 4 0
060600070827     c                   move      d22dfvi       comdfv
060700070906     c                   move      arbdam        comdam
060800070906     c     comdfv        subdur    comdam        gg:*D             4 0
060801150417     c                   move      gg            com1              1
060900150417     c*
061000150417     c                   if        gg > 5
061001150417     c                   movel(p)  '5'           tbeke2
061200070827     c                   else
061201150417     c                   movel(p)  com1          tbeke2
061400070827     c                   end
061401150417     c                   eval      tbecod = 'SLB'
061402150417     c                   eval      tbeke1 = 'DAM'
061404150417     c     ktbe          chain     tntbe01l
061405150417     c                   if        %found(tntbe01l) and tbeatb = ' '
061406150417     c                   movel     tbeuni        dslb
061500070827     c* al massimo 9999
061600150417     c                   if        comslb + D§SLBPRG <= 9999
061700150417     c                   add       D§SLBPRG      comslb
061800070827     c                   else
061900070827     c                   z-add     9999          comslb
062000070827     c                   end
062100070904     c                   end
062101150417     c                   end
062200070827     C*
062300070827     C                   ENDSR
062301180307     C*--------------------------------------------------------------*
062302180307     C*  aggiungo priorità per categoria cliente
062303180307     C*--------------------------------------------------------------*
062304180307     C     srclv         BEGSR
062307180307     c     kclp          chain     cnclp00f
062308180307     c                   if        %found(cnclp00f)
062319180307     c                   eval      tbecod = 'SLB'
062320180307     c                   eval      tbeke1 = 'CLV'
062321180307     c                   eval      tbeke2 = clpclv
062322180307     c     ktbe          chain     tntbe01l
062323180307     c                   if        %found(tntbe01l) and tbeatb = ' '
062324180307     c                   movel     tbeuni        dslb
062325180307     c* al massimo 9999
062326180307     c                   if        comslb + D§SLBPRG <= 9999
062327180307     c                   add       D§SLBPRG      comslb
062328180307     c                   else
062329180307     c                   z-add     9999          comslb
062330180307     c                   end
062331180307     c                   end
062332180307     c                   end
062333180307     C*
062334180307     C                   ENDSR
062400070206     C*--------------------------------------------------------------*
062500070206     C*  Operazioni finali in caso di errore
062600070206     C*--------------------------------------------------------------*
062700070206     C     srfine        BEGSR
062800070206     c                   if        d22cmti= 'S'
062900070206     c                   rolbk
063000070206     c                   end
063100070206     c                   eval      d22erro= '1'
063200070206     c                   eval      kpjbu = fidg22ds
063300070206     c                   seton                                        lr
063400070206     c                   return
063500070206     c                   endsr
063600070222     C*------------------------------------------------------------------------*
063700070222     C* stampa
063800070222     C*------------------------------------------------------------------------*
063900070222     C     srstampa      BEGSR
064000071107     c* stampo solo per assegnaz. automatica
064100071107     c                   if        kcdaz= 'DG17' or kcdaz='DG18'
064200070222     c                   move      d22dfvi       dtaiso
064300070222     c                   move      dtaiso        dtaeur
064400070222     c                   move      dtaeur        prtdfv
064500071107     c* decodifico parametri di lancio
064600071210     c                   if        d22ldcri <> 0
064700071219     c                   move      d22LDCRI      dtaiso
064800071107     c                   move      dtaiso        dtaeur
064900071210     c                   move      dtaeur        decldcr
065000071210     c                   else
065100071210     c                   clear                   decldcr
065200071210     c                   end
065300071210     c                   if        *in71
065400071210     c                   move      d22dtai       dtaiso
065500071210     c                   move      dtaiso        dtaeur
065600071210     c                   move      dtaeur        decdta
065700071107     c                   eval      decora = d22orai
065800071107     c                   end
065900071107     c                   if        d22cleari = 'S'
066000071107     c                   eval      decclear='SI'
066100071107     c                   else
066200071107     c                   eval      decclear='NO'
066300071107     c                   end
066301150430     c                   if        d22p88i = 'N'
066306150515     c                   eval      decp88r='Spediz. pistola 88 NO'
066307150515     c                   else
066308150515     c                   clear                   decp88r
066309150430     c                   end
066400071107     c                   if        *in70
066500071107     c                   select
066600071107     c                   when      d22tpmi ='M'
066700071107     c                   eval      dectpm ='MESSAGGERIE'
066800071107     c                   when      d22tpmi ='P'
066900071107     c                   eval      dectpm ='PARCEL'
067000071107     c                   when      d22tpmi =' '
067100071107     c                   eval      dectpm ='TUTTE'
067200071107     c                   endsl
067300071107     c                   end
067301160420     c                   if        *in72
067302160420     c                   eval      dectur1 = d22tur1
067303160420     c                   eval      dectur2 = d22tur2
067304160420     c                   end
067400071107     c                   select
067500071107     c                   when      d22fppi ='M'
067600071107     c                   eval      decfpp ='MAT'
067700071107     c                   when      d22fppi ='P'
067800071107     c                   eval      decfpp ='POM'
067900071107     c                   when      d22fppi =' '
068000071107     c                   eval      decfpp ='TUTTE'
068100071107     c                   endsl
068200070323     C* PAGINA IN STAMPA
068300070323     C                   IF        *IN80 OR RIGA >=60
068400070323     C                   SETON                                        80
068500070323     C                   ADD       1             PAGES
068600071107     C                   Z-ADD     5             RIGA              3 0
068700070323     C                   END
068800070222     c*
068900070222     c   80              write     testa
069000071107     c   80              write     testap
069100070222     c                   setoff                                       80
069200070222     c   83              write     riga3
069300070323     c   83              add       1             riga
069400070222     c   84              write     riga4
069500070323     c   84              add       1             riga
069600070305     c   85              write     riga5
069700070323     c   85              add       1             riga
069800070305     c                   setoff                                       838485
069900071107     c                   end
070000070222     c*
070100070222     c                   endsr
070200950525     C*--------------------------------------------------------------*
070300950525     C*  Operazioni iniziali
070400950525     C*--------------------------------------------------------------*
070500950525     C     *INZSR        BEGSR
070600950525     C*
070700950525     C     *ENTRY        PLIST
070800950525     C                   PARM                    KPJBA
070900070206     C                   MOVEL(P)  KPJBU         Fidg22ds
071000070323     c                   eval      pages = d22pag
071100070323     c                   eval      riga  = d22riga
071200070206     c                   clear                   d22erro
071201180314     c*
071202180314     C                   CALL      'X§PARUT'
071203180314     C                   PARM                    UT§DSE
071300030718     c*
071400030718     c     *dtaara       define    §azute        azuteds
071500030718     c     *dtaara       define    §datiute      ddatiute
071600030718     C                   in(E)     *dtaara
071700030718     C                   IF        %Error  or  RSUT = *blanks
071800030718     C                   call      'TIBS34R'
071900030718     C                   parm                    Tibs34Ds
072000030718     C                   in        *dtaara
072100030718     c                   ENDIF
072200070205     c*
072300070205     C                   TIME                    WHHUDT           14 0
072400070205     C                   MOVEL     WHHUDT        ORA               6 0
072500070510     C                   MOVE      WHHUDT        UDATE8            8 0
072600070510     c                   move      udate8        dtaeur
072700070510     c                   move      dtaeur        dtaiso
072800070510      *
072900070510     c*********          move      *date         dtaiso
073000070206     c                   move      dtaiso        dateu8            8 0
073100070206     C     d22FGSi       CHAIN     AZORG01l
073200070205    1C                   IF        %found(azorg01l)
073300070205     C                   MOVEL     ORGde6        og146
073400071107     c                   movel     orgdf0        og150
073500071107     C                   MOVEL     ORGDE7        og147
073600071107     c     §OGPKM        comp      'S'                                    70
073700071107     c     §OGdoada      comp      'S'                                    71
073800070205     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
073900070205     c* se data distinte automatiche < oggi p.o. partito
074000070205     c                   if        §ogdda = ' '
074100070205     c                   exsr      srfine
074200070205     c                   else
074300070205     c                   move      §ogdda        comdda            8 0
074400070205     c                   end
074500070205     c                   end
074600070206     C*  tntbe01l
074700100203     C     Ktab          KLIST
074800100203     C                   KFLD                    TBLKUT
074900100203     C                   KFLD                    TBLCOD
075000100203     C                   KFLD                    TBLKEY
075100100203     c                   eval      tblkut = 1
075101180313     C*  cnclp01l categoria cliente
075102180313     C     Kclp          KLIST
075103180313     C                   KFLD                    tblKUT
075104180314     C                   KFLD                    kci
075105180313     C                   KFLD                    arbksc
075200100203     C     Ktbe1         KLIST
075300100203     C                   KFLD                    tbecod
075400100203     C                   KFLD                    tbeke1
075500070827     C     Ktbe          KLIST
075600070827     C                   KFLD                    tbecod
075700070827     C                   KFLD                    tbeke1
075800070206     C                   KFLD                    tbeke2
075900070223     C                   TIME                    TIMES             6 0
076000070223     C                   TIME                    WHDAT            12 0
076100070223     C                   MOVE      WHDAT         WOGGI             6 0
076200070424     C                   TIME                    W0140            14 0
076300070424     C                   MOVE      W0140         UDATE8            8 0
076301180313      *  RICERCA CAPOCONTI
076302180314     C                   DO        50            X
076303180313     C                   MOVE      TCU(X)        TCUDS
076304180313     C     F56           CABNE     'CG'          END1
076305180313     C     F4            COMP      '1'                                    21
076306180313     C     F3            COMP      '0'                                242425
076307180313      ** 1 CLIENTI   21
076308180313      ** 0 ITALIA   25
076309180313      ** 1 ESTERO   24
076310180313     C   21
076311180313     CAN 25              Z-ADD     KCU(X)        KCI               4 0
076312180313     C     END1          TAG
076313180313     C                   END
076400070424     C*
076500070424     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
076600070424     C                   MOVEL     *BLANK        G02ERR
076700070424     C                   CALL      'XSRDA8'
076800070424     C                   PARM                    WLBDAT
076900070424     C                   Z-ADD     G02INV        DATEU8                         *AAAA/MM/GG
077000070906     c* carico schiera DCR e DAM
077100070827     c                   exsr      srsch
077200100203     c* cerco sequenza per consegna oggi
077300100203     c                   clear                   slbattesa
077400100203     c     'SLB'         setll     tntbe01l
077500100203     c                   do        *hival
077600100203     c     'SLB'         reade     tntbe01l
077700100203     c                   if        %eof(tntbe01l)
077800100203     c                   leave
077900100203     c                   end
078000100203     c                   movel     tbeuni        dslb
078100100203     c                   if        d§slbatt = 'S'
078200100203     c                   eval      slbattesa = D§SLBPRG
078300100203     c                   leave
078400100203     c                   end
078500100203     c                   enddo
078600950525     C                   ENDSR
078700070523     C*----------------------------------------------------*
078800070827     C     srsch         begsr
078900070523     C*----------------------------------------------------*
079000070827     c* DCR
079100070827     c                   eval      tbecod = 'SLB'
079200070827     c                   eval      tbeke1 = 'DCR'
079300070827     c     ktbe1         setll     tntbe01l
079400070827     c                   do        *hival
079500070827     c     ktbe1         reade     tntbe01l
079600070827     c                   if        %eof(tntbe01l)
079700070827     c                   leave
079800070827     c                   end
079900070827     c                   if        tbeatb <> ' '
080000070827     c                   iter
080100070827     c                   end
080200070827     c                   eval      dslb = tbeuni
080300070827     c                   clear                   x                 4 0
080400070827     c                   select
080500070827     c                   when      tbeke2 = ' '
080600070827     c                   eval      x = 1
080700070827     c                   when      tbeke2 = 'P'
080800070827     c                   eval      x = 2
080900070827     c                   when      tbeke2 = 'D'
081000070827     c                   eval      x = 3
081100070827     c                   when      tbeke2 = 'IL'
081200070827     c                   eval      x = 4
081300070827     c                   endsl
081400070827     c                   if        x <> 0
081500070827     c                   eval      dcr(x) = D§SLBPRG
081600070827     c                   end
081700070827     c                   enddo
083500070827     C                   ENDSR
083600070827     C*----------------------------------------------------*
083700070827     C     geslck        begsr
083800070827     C*----------------------------------------------------*
083900070523     c                   clear                   trul82ds
084000070523     c                   Eval      ul82§rrn = rrnarg
084100070523     c                   Eval      UL82§FIL = 'FIARG00F'
084200070523     c                   Eval      UL82§WIN = 'S'
084300070523     c                   Eval      UL82§num = 2
084400070523     c                   Eval      UL82§att = 2
084500070523     c                   eval      ul82§mss = ErrMsg(1)
084600070523     c                   Eval      UL82§msw = 'La Spedizione '
084700070523     c                             + %editc(arglnp:'Z') +  ' '
084800070523     c                             + %editc(argnrs:'Z') +  ' '
084900070523     c                             + %editc(argnsp:'Z') +  ' è già utilizza ta -
085000070523     c                             da un altro lavoro e non è manutenzionabile'
085100070523      * Effettuo la chiamata al *PGM d utilità
085200070523     c                   Call(e)   'TRUL82R'
085300070523     c                   Parm                    Trul82ds
085400070523
085500070523     C                   ENDSR
085600070523** ErrMsg
085700070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
