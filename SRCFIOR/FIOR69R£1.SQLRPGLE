000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200070205     h dftactgrp(*no) actgrp(*caller)
000300070322     H* FIDG21R *----------------------------------------------------*
000400070322     H*         Assegnaz.distinta di consegna auto. x giro e sequenza bolla
000500070322     H*--------------------------------------------------------------*
000600070222     FFidg18p   O    E             PRINTER OFLIND(*IN80)
000700070319     Ftabel00f  IF   E           K DISK
000800070206     FFIAR601L  IF   E           K DISK
000900070206     FAZORG01L  IF   E           K DISK
001000070205     Ffiarg00f  uf   E             DISK    commit
001100070205      *--------------------------------------------------------------*
001200070205      *   DS
001300070205      *--------------------------------------------------------------*
001400070205     D fiargds       e ds                  extname(fiarg00f)
001500070205     D fnarbds       e ds                  extname(fnarb00f)
001600070207     D fidstds       e ds                  extname(fidst00f)
001700070302     D fidstds£      e ds                  extname(fidst00f) prefix(£)
001701070523     d trul82ds      e ds
001800050520     D ddatiute      e ds
001900030910     D azuteds       e ds                  extname(AZUTE00F)
002000030910     D tibs34ds      E DS                  inz
002100070319     D DS3A          E DS
002200070319     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
002300950524     D KPJBA         E DS
002400070205     D Fidg21ds      E DS
002500070205     D Fidg22ds      E DS
002600070302     D Fidg27ds      E DS
002700070205     D OG146         E DS                  INZ
002800070205     drrnarg           s              9  0
002900070205     drrnarb           s              9  0
003000070205     ddtaiso           s               d
003100070205     ddtaeur           s               d   datfmt(*eur)
003200070302     D arbpeso         S                   INZ like(arbpkb)
003300070302     D arbvolume       S                   INZ like(arbvlb)
003400070214     D peso            S                   INZ like(arbpkb)
003500070214     D volume          S                   INZ like(arbvlb)
003600070214     D savnftl         S                   INZ like(argnftl)
003700070207     D compkl          S                   INZ like(dstpkl)
003800070207     D comvlu          S                   INZ like(dstvlu)
003900070207     D comnftl         S                   INZ like(dstnftl)
004000070215     D conta           S                   INZ like(dstnftl)
004100070214     D nftl            S                   INZ like(dstnftl)
004200070206     D prima           S              1
004300070330     D tolle           S              1
004400070206     D oktoll          S              1
004500070206     c*
004600070206     D Ptc1            S               *
004700070207     D                                     INZ(%ADDR(dsttc1))
004800070207     D tc1             S                   LIKE(dsttc1)
004900070206     D                                     DIM(5)
005000070206     D                                     BASED(Ptc1)
005100070206     D Pts1            S               *
005200070207     D                                     INZ(%ADDR(dstts1))
005300070207     D ts1             S                   LIKE(dstts1)
005400070206     D                                     DIM(2)
005500070206     D                                     BASED(Pts1)
005600070424     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005700070424     D WLBDAT          DS                  INZ
005800070424     D  G02DAT                 1      8  0
005900070424     D  G02INV                 9     16  0
006000070424     D  G02ERR                17     17
006100070424     D  G02TGI                18     22  0
006101070523     D Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
006200950524     C*--------------------------------------------------------------*
006300950524     C*  Ciclo principale
006400950524     C*--------------------------------------------------------------*
006500070215     c                   clear                   savnftl
006600070222     c                   clear                   conta
006700070206     c                   clear                   prima
006800070330     c                   clear                   tolle
006900070206     c* per prima cosa imposto la seq. di lettura delle bolle e la fermata
007000070206     c                   clear                   fidg22ds
007100070209     c                   eval      D22FGSI = D21FGSI
007200070206     c                   eval      d22NPGI = d21NPGI
007300070206     c                   eval      d22DFVI = d21DFVI
007400070206     c                   eval      d22NFVI = d21NFVI
007500070206     c                   eval      D22PDRI = D21PDRI
007600070206     c                   eval      D22FGSGI= D21FGSGI
007700070206     c                   eval      D22CGII = D21CGII
007800070206     c                   eval      D22PKLI = D21PKLI
007900070206     c                   eval      D22VLUI = D21VLUI
008000070206     c                   eval      D22NFTLI= D21NFTLI
008100070323     c                   eval      d22pag = pages
008200070323     c                   eval      d22riga= riga
008300070206     C                   eval      kpjbu = FiDG22DS
008400070205     c                   call      'FIDG22R'
008500070205     c                   parm                    kpjba
008600070209     c                   eval      fidg22ds = kpjbu
008700070326     c                   eval      pages = d22pag
008800070326     c                   eval      riga  = d22riga
008900070206     c* se ritorno con errore non proseguo
009000070206     c                   if        d22erro <> ' '
009100070206     c                   exsr      srfine
009200070206     c                   end
009300070206     c*
009400070205     c* se il p.o. è in parallelo testo la distinta su ARG altrimenti
009500070410     c* se il p.o. è partito testo la distinta su ARB
009600070209     c*
009700070209     c* per il momento aggiorno solo ARG perchè ARB lo aggiornerò alla fine
009800070205     c* quando calcolo anche gli stop
009900070209     c*
010000070209     c* se la sequenza bolla é = 0 la spedizione non è da elaborare
010100070206     c*
010200070424     c                   if        comdda= 20391231 or comdda > dateu8
010300061003     C/EXEC SQL
010400070205     C+ DECLARE A1 CURSOR FOR SELECT rrn(fiarg01l), rrn(fnarb01l),
010500070302     C+ fiarg01l.*, fnarb01l.*, argnftl
010600070214     C+ FROM fiarg01l join fnarb01l on arbaas=
010700070205     C+ argaas and arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp
010800070214     C+ WHERE argndc = 0 and argcgi=:d21cgii and argslb <> 0 and
010900070214     C+ argpoc = :d21fgsi  ORDER BY
011000070222     C+ FIARG01L.ARGslb, FIARG01L.ARGnftl
011100061003     C/END-EXEC
011200061003
011300061003     C/EXEC SQL
011400070205     C+ OPEN a1
011500061003     C/END-EXEC
011600070205     c                   else
011700070412     c* oltre a ARBNDC = 0 testo anche ARGNDC=0 per evitare di rielaborare
011800070412     c* le spedizione già inserite nelle precedenti distinte
011900070205     C/EXEC SQL
012000070205     C+ DECLARE B1 CURSOR FOR SELECT rrn(fiarg01l), rrn(fnarb01l),
012100070302     C+ fiarg01l.*, fnarb01l.*, argnftl
012200070214     C+ FROM fiarg01l join fnarb01l on arbaas=
012300070205     C+ argaas and arblnp=arglnp and arbnrs= argnrs and arbnsp= argnsp
012400070410     C+ WHERE arbndc = 0 and argcgi=:d21cgii and argslb <>
012500070412     C+ 0 and argpoc = :d21fgsi and argndc = 0
012600070222     C+  ORDER BY FIARG01L.ARGslb, FIARG01L.ARGnftl
012700070205     C/END-EXEC
012800070205
012900070205     C/EXEC SQL
013000070205     C+ OPEN b1
013100070205     C/END-EXEC
013200070205     c                   end
013300061003
013400061003     C                   do        *hival
013500061003
013600070424     c                   if        comdda= 20391231 or comdda > dateu8
013700061003     C/EXEC SQL
013800070302     C+ FETCH NEXT FROM a1 INTO :rrnarg, :rrnarb, :fiargds, :fnarbds, :nftl
013900061003     C/END-EXEC
014000070205     c                   else
014100070205     C/EXEC SQL
014200070302     C+ FETCH NEXT FROM b1 INTO :rrnarg, :rrnarb, :fiargds, :fnarbds, :nftl
014300070205     C/END-EXEC
014400070205     c                   end
014500061003
014600061003     C                   select
014700061003     C                   when      sqlcod = 100
014800061003     c                   leave
014900070205     C                   WHEN      SqlCod < 0
015000070205     c                   if        sqlcod <> -913
015100070205     C                   seton                                        H1
015200070209     c                   end
015300070222     c                   seton                                        83
015400070222     c                   exsr      srstampa
015500070206     c                   exsr      srfine
015600070206     C                   other
015700070206     C                   exsr      gessped
015800070206     c* se sono a capienza esco
015900070206     c                   if        prima = '1'
016000070206     c                   leave
016100070206     c                   end
016200061003     C                   endsl
016300061003     C*
016400061003     C                   enddo
016500061003
016600070424     c                   if        comdda= 20391231 or comdda > dateu8
016700061003     C/EXEC SQL
016800070205     C+ CLOSE a1
016900061003     C/END-EXEC
017000070205     c                   else
017100070205     C/EXEC SQL
017200070205     C+ CLOSE b1
017300070205     C/END-EXEC
017400070205     c                   end
017500070206     C*
017600070206     c                   if        d21cmti= 'S'
017700070206     c                   commit
017800070206     c                   end
017900070323     c                   eval      d21pag = pages
018000070323     c                   eval      d21riga= riga
018100070206     c*
018200070206     c                   eval      kpjbu = fidg21ds
018300950525     C*
018400950524     C                   SETON                                        LR
018500030708     C*--------------------------------------------------------------*
018600070205     C*  Gestione singola spedizione
018700030708     C*--------------------------------------------------------------*
018800070205     C     GESsped       BEGSR
018900070302     c* calcolo pesi e volume
019000070302     c                   exsr      srpesvol
019100070302     c                   eval      arbpeso = d27pkldco
019200070302     c                   eval      arbvolume = d27vludco
019300070214     c* controllo che la spedizione non raggiunga i limiti di capienza
019400070214     c* e il tipo consegne
019500070214     c                   exsr      srctrsped
019600070214     c                   if        not *in28
019700070227     c* aggiorno sempre ARG
019800070528     c     RRNARG        chain(e)  fiarg00f
019900070206     c                   if        %error
019901070523     c                   exsr      geslck
019902070523     c                   if        ul82§sts = 'A'
020000070704     c                   seton                                        85
020001070704     C                   eval      prtmsg = 'La spedizione non è stata aggiorna-
020002070704     C                             ta correttamente.VERIFICARLA'
020100070222     c                   exsr      srstampa
020200070206     c                   leavesr
020201070528     c                   else
020202070528     c     RRNARG        chain     fiarg00f
020300070206     c                   end
020301070523     c                   end
020400070206     c*
020500070206     c                   if        not %found(fiarg00f)
020600070222     c                   seton                                        84
020700070222     c                   exsr      srstampa
020800070206     c                   else
020900070206     c* se non ci sono errori aggiorno ARG
021000070205     c                   eval      argpdc = d21pdri
021100070205     c                   eval      argFGS = d21fgsi
021200070205     c                   eval      argNDC = d21nfvi
021300070205     c                   eval      argDDC = d21dfvi
021400070205     c                   eval      argDTVDIS = dateu8
021500070205     c                   eval      argHVDIS= ora
021600070206     c                   update    fiarg000
021700070302     c                   eval      D21PKLo = d21pklo + arbpeso
021800070302     c                   eval      D21VLUo = d21vluo + arbvolume
021900070216     c                   eval      D21nftlo= conta
022000070222     c                   eval      D21nblo = d21nblo + 1
022100070222     c                   end
022200070227     c                   end
022300950524     C*
022400070205     C                   ENDSR
022500070205      *-----------------------------------------------
022600070302     C     srpesvol      begsr
022700070205      *-----------------------------------------------
022800070302     c                   clear                   fidg27ds
022900070302     c                   eval      D27TELAI = 'R'
023000070322     c                   eval      D27dfvi = d21dfvi
023100070322     c                   eval      D27AASI = arbaas
023200070302     c                   eval      D27LNPI = arblnp
023300070302     c                   eval      D27NRSI = arbnrs
023400070302     c                   eval      D27NSPI = arbnsp
023500070302     c                   eval      kpjbu = fidg27ds
023600070302     c                   call      'FIDG27R'
023700070302     c                   parm                    kpjba
023800070302     c                   parm                    fnarbds
023900070302     c                   eval      fidg27ds = kpjbu
024000070302     C                   ENDSR
024100070302      *-----------------------------------------------
024200070302     C     srctrsped     begsr
024300070302      *-----------------------------------------------
024400070202     c                   setoff                                       28
024500070206     c* controllo consegna particolare
024600070206     c                   movea     tc1           com5              5
024700070313     c                   if        com5 <> ' ' and (arbtc1 <> ' ' or
024800070313     c                             arbtc2 <> ' ' )
024900070313     c                   if        arbtc1 <> ' '
025000070206     c     arbtc1        lookup    tc1                                    88
025100070313     c                   if        not *in88  and dstftc = 'I'
025200070328     c                   seton                                        2885
025300070328     c                   eval      prtmsg = 'Consegna particolare esclusa in di-
025400070328     c                             stinta'
025500070328     c                   exsr      srstampa
025600070313     c                   leavesr
025700070313     c                   end
025800070313     c                   if        *in88  and dstftc = 'E'
025900070328     c                   seton                                        2885
026000070328     c                   eval      prtmsg = 'Consegna particolare esclusa in di-
026100070328     c                             stinta'
026200070328     c                   exsr      srstampa
026300070313     c                   leavesr
026400070313     c                   end
026500070313     c                   end
026600070313     c                   if        arbtc2 <> ' '
026700070206     c     arbtc2        lookup    tc1                                    88
026800070207     c                   if        not *in88  and dstftc = 'I'
026900070328     c                   seton                                        2885
027000070328     c                   eval      prtmsg = 'Consegna particolare esclusa in di-
027100070328     c                             stinta'
027200070328     c                   exsr      srstampa
027300070206     c                   leavesr
027400070206     c                   end
027500070207     c                   if        *in88  and dstftc = 'E'
027600070328     c                   seton                                        2885
027700070328     c                   eval      prtmsg = 'Consegna particolare esclusa in di-
027800070328     c                             stinta'
027900070328     c                   exsr      srstampa
028000070206     c                   leavesr
028100070206     c                   end
028200070313     c                   end
028300070206     c                   end
028400070206     c* controllo tipo servizio
028500070206     c                   movea     ts1           com2              2
028600070313     c                   if        com2 <> ' ' and arbtsp <> ' '
028700070206     c     arbtsp        lookup    ts1                                    88
028800070207     c                   if        not *in88  and dstftc = 'I'
028900070328     c                   seton                                        2885
029000070328     c                   eval      prtmsg = 'Tipo servizio esclusa in di-
029100070328     c                             stinta'
029200070328     c                   exsr      srstampa
029300070206     c                   leavesr
029400070206     c                   end
029500070207     c                   if        *in88  and dstftc = 'E'
029600070328     c                   seton                                        2885
029700070328     c                   eval      prtmsg = 'Tipo servizio esclusa in di-
029800070328     c                             stinta'
029900070328     c                   exsr      srstampa
030000070206     c                   leavesr
030100070206     c                   end
030200070206     c                   end
030300070206     c* controllo incassi
030400070207     c                   if        dstinc ='N'
030500070319     c* controllo il tipo bolla se prevede il c/assegno
030600070319     c* ????
030700070319     C     ARBCBO        LOOKUP    C3A(X)                                 28
030800070319     C     *IN28         IFEQ      '1'
030900070328     c                   seton                                        2885
031000070328     c                   eval      prtmsg = 'Spedizione con incasso ma in disti-
031100070328     c                             nta no'
031200070328     c                   exsr      srstampa
031300070206     c                   leavesr
031400070206     c                   end
031500070326     c*
031600070326     c     kspe          setll     fiar601l
031700070326     c                   do        *hival
031800070326     c     kspe          reade     fiar601l
031900070326     c                   if        %eof(fiar601l)
032000070326     c                   leave
032100070326     c                   end
032200070326     c                   if        ar6nft <> 0
032300070328     c                   seton                                        2885
032400070328     c                   eval      prtmsg = 'Spedizione con incasso ma in disti-
032500070328     c                             nta no'
032600070328     c                   exsr      srstampa
032700070206     c                   leavesr
032800070206     c                   end
032900070326     c                   end
033000070206     c                   end
033100070330     c* ROTTURA di fermata
033200070214     c                   if        argnftl <> savnftl
033300070410     C* se ho già dato una fermata utilizzando la tolleranza esco
033400070330     c                   if        tolle = '1'
033500070507     C                   seton                                        2885
033600070330     c                   seton                                        90
033700070330     c                   eval      prtmsg = 'Prima sped-fermata che eccede per -
033800070330     c                             peso o volume'
033900070330     c                   exsr      srstampa
034000070330     c                   eval      prima = '1'
034100070330     c                   leavesr
034200070330     c                   end
034300070330     c* controllo la capienza A ROTTURA di fermata
034400070214     c                   exsr      srecce
034500070214     c                   end
034600070222     c* controllo che le bolle con stessa fermata non mi facciano
034700070227     c* eccedere la capienza solo se data consegna = 0 altrimenti sono dei
034800070227     c* ritorni all'incasso quindi non conteggio il peso ma solo la fermata
034900070227     c* (per le parziali ????? )
035000070227     c                   if        arbdcm = 0
035100070214     c                   if        (peso > (d21pkli - d21pklo) or
035200070222     c                             volume > (d21vlui - d21vluo))
035300070214     c* provo con la tolleranza
035400070302     c                   if        dsttol <> 0
035500070504     c* testo il peso e il volume solo se non ho già attribuito una o +
035600070504     c* spedizioni tocciando dalla tolleranza. in questo caso finisco
035700070507     c* di attribuire le spediz. con stessa fermata
035800070504     c                   if        tolle = ' '
035900070329     c                   if        peso > (compkl + (d21pkli - d21pklo)) or
036000070329     c                             volume > (comvlu + (d21vlui - d21vluo))
036100070507     C                   seton                                        2885
036200070329     c                   seton                                        90
036300070329     c                   eval      prtmsg = 'Prima sped-fermata che eccede per -
036400070329     c                             peso o volume'
036500070328     c                   exsr      srstampa
036600070627     c*                  eval      prima = '1'
036700070330     c                   leavesr
036800070330     c                   else
036900070330     c                   eval      TOLLE = '1'
037000070330     c                   end
037100070504     c                   end
037200070214     c                   else
037300070507     c                   seton                                        2885
037400070329     c                   seton                                        90
037500070329     c                   eval      prtmsg = 'Prima sped-fermata che eccede per -
037600070329     c                             peso o volume'
037700070328     c                   exsr      srstampa
037800070222     c                   leavesr
037900070214     c                   end
038000070214     c                   end
038100070227     c                   end
038200070222     c* controllo le fermate
038300070222     c                   if        argnftl <> savnftl
038400070222     c                   add       1             conta
038500070222     c                   end
038600070222     c                   if        conta> d21nftli
038700070222     c* provo con la tolleranza
038800070330     c*                  if        dsttol <> 0
038900070330     c*                  if        conta> comnftl + d21nftli
039000070330     c*                  seton                                        2885
039100070330     c*                  seton                                        90
039200070330     c*                  eval      prtmsg = 'La spedizione eccede per numero fe-
039300070330     c*                            rmate'
039400070330     c*                  exsr      srstampa
039500070330     c*                  eval      prima = '1'
039600070330     c*                  leavesr
039700070330     c*                  end
039800070330     c*                  else
039900070329     c                   seton                                        2885
040000070329     c                   seton                                        90
040100070328     c                   eval      prtmsg = 'La spedizione eccede per numero fe-
040200070328     c                             rmate'
040300070328     c                   exsr      srstampa
040400070222     c                   eval      prima = '1'
040500070222     c                   leavesr
040600070330     c*                  end
040700070222     c                   end
040800070222     c                   eval      savnftl = argnftl
040900070202     C*
041000070202     C                   ENDSR
041100070206     C*--------------------------------------------------------------*
041200070214     C* verifico peso volume x fermata
041300070206     C*--------------------------------------------------------------*
041400070214     C     srecce        BEGSR
041500070302     c                   clear                   peso
041600070302     c                   clear                   volume
041700070424     c                   if        comdda= 20391231 or comdda > dateu8
041800070302     C/EXEC SQL
041900070302     C+ DECLARE C1 CURSOR FOR SELECT fnarb01l.* FROM fiarg01l join
042000070302     C+ fnarb01l on arbaas= argaas and arblnp=arglnp and arbnrs= argnrs
042100070302     C+ and arbnsp= argnsp WHERE argndc = 0 and argcgi=:d21cgii and
042200070302     C+ argslb <> 0 and argpoc = :d21fgsi and argnftl = :nftl
042300070302     C/END-EXEC
042400070302
042500070302     C/EXEC SQL
042600070302     C+ OPEN c1
042700070302     C/END-EXEC
042800070302     c                   else
042900070302     C/EXEC SQL
043000070302     C+ DECLARE D1 CURSOR FOR SELECT fnarb01l.* FROM fiarg01l join
043100070302     C+ fnarb01l on arbaas= argaas and arblnp=arglnp and arbnrs= argnrs
043200070410     C+ and arbnsp= argnsp WHERE arbndc = 0 and argcgi=:d21cgii
043300070410     C+ and argslb <> 0 and argpoc = :d21fgsi and argnftl =
043400070412     C+ :nftl and argndc = 0 ORDER BY FIARG01L.ARGslb, FIARG01L.ARGnftl
043500070302     C/END-EXEC
043600070302
043700070302     C/EXEC SQL
043800070302     C+ OPEN d1
043900070302     C/END-EXEC
044000070302     c                   end
044100070302
044200070302     C                   do        *hival
044300070302
044400070424     c                   if        comdda= 20391231 or comdda > dateu8
044500070302     C/EXEC SQL
044600070302     C+ FETCH NEXT FROM c1 INTO :fnarbds
044700070302     C/END-EXEC
044800070302     c                   else
044900070302     C/EXEC SQL
045000070302     C+ FETCH NEXT FROM d1 INTO :fnarbds
045100070302     C/END-EXEC
045200070302     c                   end
045300070302
045400070302     C                   select
045500070302     C                   when      sqlcod = 100
045600070302     c                   leave
045700070302     C                   WHEN      SqlCod < 0
045800070302     c                   if        sqlcod <> -913
045900070302     C                   seton                                        H1
046000070302     c                   end
046100070302     c                   seton                                        83
046200070302     c                   exsr      srstampa
046300070302     c                   exsr      srfine
046400070302     C                   other
046500070302     C                   exsr      srpesvol
046600070302     c                   eval      peso = peso + d27pkldco
046700070302     c                   eval      volume = volume + d27vludco
046800070302     C                   endsl
046900070302     C*
047000070302     C                   enddo
047100070302
047200070424     c                   if        comdda= 20391231 or comdda > dateu8
047300070302     C/EXEC SQL
047400070302     C+ CLOSE c1
047500070302     C/END-EXEC
047600070302     c                   else
047700070302     C/EXEC SQL
047800070302     C+ CLOSE d1
047900070302     C/END-EXEC
048000070302     c                   end
048100070214     C                   ENDSR
048200070214     C*--------------------------------------------------------------*
048300070214     C* calcola la capienza
048400070214     C*--------------------------------------------------------------*
048500070214     C     srtol         BEGSR
048600070209     c*
048700070214     c                   clear                   compkl
048800070214     c                   clear                   comvlu
048900070214     c                   clear                   comnftl
049000070214     c*
049100070302     c                   if        dsttol <> 0
049200070329     c                   eval      compkl = ((dstpkl * dsttol)/ 100)
049300070329     c                   eval      comvlu = ((dstvlu * dsttol)/ 100)
049400070329     c                   eval      comnftl= ((dstnftl * dsttol)/ 100)
049500070214     c                   end
049600070206     C                   ENDSR
049700070222     C*------------------------------------------------------------------------*
049800070222     C* stampa
049900070222     C*------------------------------------------------------------------------*
050000070222     C     srstampa      BEGSR
050100070222     c                   move      d21dfvi       dtaiso
050200070222     c                   move      dtaiso        dtaeur
050300070222     c                   move      dtaeur        prtdfv
050400070323     C* PAGINA IN STAMPA
050500070323     C                   IF        *IN80 OR RIGA >=60
050600070323     C                   SETON                                        80
050700070323     C                   ADD       1             PAGES
050800070323     C                   Z-ADD     3             RIGA              3 0
050900070323     C                   END
051000070328     c*
051100070328     c                   if        *in85
051200070328     c                   eval      prtaas = arbAAS
051300070328     c                   eval      prtlnp = arbLNP
051400070328     c                   eval      prtnrs = arbNRS
051500070328     c                   eval      prtnsp = arbNSP
051600070329     c                   eval      prtnfv = d21nfvi
051700070329     c                   eval      prtcgi = d21cgii
051800070328     c                   end
051900070328     c                   if        *in86
052000070328     c                   eval      prtpesb= peso
052100070328     c                   eval      prtvolb= volume
052200070329     c                   if        dsttol <> 0
052300070507     c                   eval      prtpesd = (compkl + (d21pkli - d21pklo))
052400070507     c                   eval      prtvold = (comvlu + (d21vlui - d21vluo))
052500070329     c                   else
052600070329     c                   eval      prtpesd = (d21pkli - d21pklo)
052700070329     c                   eval      prtvold = (d21vlui - d21vluo)
052800070328     c                   end
052900070329     c                   end
053000070222     c*
053100070222     c   80              write     testa
053200070222     c                   setoff                                       80
053300070222     c   83              write     riga3
053400070323     C   83              ADD       1             RIGA
053500070222     c   84              write     riga4
053600070323     C   84              ADD       1             RIGA
053700070328     c   85              write     riga5
053800070328     C   85              ADD       1             RIGA
053900070328     c   86              write     riga6
054000070328     C   86              ADD       1             RIGA
054100070328     c                   setoff                                       838485
054200070328     c                   setoff                                       86
054300070222     c*
054400070222     c                   endsr
054500070206     C*--------------------------------------------------------------*
054600070206     C*  Operazioni iniziali
054700070206     C*--------------------------------------------------------------*
054800070206     C     *INZSR        BEGSR
054900950525     C*
055000950525     C     *ENTRY        PLIST
055100950525     C                   PARM                    KPJBA
055200070302     C                   PARM                    fidstds£
055300070206     C                   MOVEL(P)  KPJBU         Fidg21ds
055400070323     c                   eval      pages = d21pag
055500070323     c                   eval      riga = d21riga
055600070302     c                   eval      fidstds = fidstds£
055700070206     c                   clear                   d21erro
055800030718     c*
055900030718     c     *dtaara       define    §azute        azuteds
056000030718     c     *dtaara       define    §datiute      ddatiute
056100030718     C                   in(E)     *dtaara
056200030718     C                   IF        %Error  or  RSUT = *blanks
056300030718     C                   call      'TIBS34R'
056400030718     C                   parm                    Tibs34Ds
056500030718     C                   in        *dtaara
056600030718     c                   ENDIF
056700070205     c*
056800070205     C                   TIME                    WHHUDT           14 0
056900070205     C                   MOVEL     WHHUDT        ORA               6 0
057000070510     C                   MOVE      WHHUDT        UDATE8            8 0
057100070510     c                   move      udate8        dtaeur
057200070510     c                   move      dtaeur        dtaiso
057300070510      *
057400070510     c*********          move      *date         dtaiso
057500070206     c                   move      dtaiso        dateu8            8 0
057600070205     C     d21FGSi       CHAIN     AZORG01l
057700070205    1C                   IF        %found(azorg01l)
057800070205     C                   MOVEL     ORGde6        og146
057900070205     c* se data distinte automatiche = 31/12/2039 p.o. in fase di test
058000070205     c* se data distinte automatiche < oggi p.o. partito
058100070205     c                   if        §ogdda = ' '
058200070205     c                   exsr      srfine
058300070205     c                   else
058400070205     c                   move      §ogdda        comdda            8 0
058500070205     c                   end
058600070205     c                   end
058700950525     C*  Definisco chiavi di accesso
058800070206     c     Kspe          Klist
058900070206     c                   Kfld                    arbaas
059000070206     c                   Kfld                    arblnp
059100070206     c                   Kfld                    arbnrs
059200070206     c                   Kfld                    arbnsp
059300070319     C     KTAB1         KLIST
059400070319     C                   KFLD                    tblkut
059500070319     C                   KFLD                    TBLCOD
059600070319     c                   z-add     1             tblkut
059700070206     c*
059800070209     c                   exsr      srtol
059900070223     C                   TIME                    TIMES             6 0
060000070223     C                   TIME                    WHDAT            12 0
060100070424     C                   TIME                    W0140            14 0
060200070424     C                   MOVE      W0140         UDATE8            8 0
060300070223     C                   MOVE      WHDAT         WOGGI             6 0
060400070424     C*
060500070424     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
060600070424     C                   MOVEL     *BLANK        G02ERR
060700070424     C                   CALL      'XSRDA8'
060800070424     C                   PARM                    WLBDAT
060900070424     C                   Z-ADD     G02INV        DATEU8                         *AAAA/MM/GG
061000070319     C*  CARICO codici della tabella 3a per i quali è previsto c/ass.
061100070319     C                   MOVEL     '3A'          TBLCOD
061200070319     C                   Z-ADD     0             X                 4 0
061300070319     C     KTAB1         setll     TABEL00F
061400070319     C                   DO        *hival
061500070319     C     KTAB1         READE     TABEL00F
061600070319     c                   if        %eof(tabel00f)
061700070319     c                   leave
061800070319     c                   end
061900070319     C                   if        x = 100
062000070319     c                   leave
062100070319     c                   end
062200070319     C                   MOVEL     TBLUNI        ds3a
062300070319     c                   if        §3afca <> 0
062400070319     C                   ADD       1             X
062500070319     C                   MOVEL     TBLKEY        C3A(X)
062600070319     C                   end
062700070319     C                   ENDdo
062800950525     C                   ENDSR
062900070205     C*--------------------------------------------------------------*
063000070205     C*  Operazioni finali in caso di errore
063100070205     C*--------------------------------------------------------------*
063200070205     C     srfine        BEGSR
063300070206     c                   if        d21cmti= 'S'
063400070205     c                   rolbk
063500070205     c                   end
063600070206     c                   eval      d21erro= '1'
063700070205     c                   eval      kpjbu = fidg21ds
063800070205     c                   seton                                        lr
063900070205     c                   return
064000070205     c                   endsr
064100070523     C*----------------------------------------------------*
064200070523     C     geslck        begsr
064300070523     C*----------------------------------------------------*
064400070523     c                   clear                   trul82ds
064500070523     c                   Eval      ul82§rrn = rrnarg
064600070523     c                   Eval      UL82§FIL = 'FIARG00F'
064700070523     c                   Eval      UL82§WIN = 'S'
064900070523     c                   Eval      UL82§num = 2
065000070523     c                   Eval      UL82§att = 2
065100070523     c                   eval      ul82§mss = ErrMsg(1)
065200070523     c                   Eval      UL82§msw = 'La Spedizione '
065300070523     c                             + %editc(arglnp:'Z') +  ' '
065301070523     c                             + %editc(argnrs:'Z') +  ' '
065302070523     c                             + %editc(argnsp:'Z') +  ' è già utilizza ta -
065400070523     c                             da un altro lavoro e non è manutenzionabile'
065500070523      * Effettuo la chiamata al *PGM d utilità
065600070523     c                   Call(e)   'TRUL82R'
065700070523     c                   Parm                    Trul82ds
065800070523
065900070523     C                   ENDSR
066000070523** ErrMsg
066100070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
