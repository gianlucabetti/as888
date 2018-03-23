000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300040916      *clienti a cui inviare da bollettazione
000400060214     ffnblp65j  if   e           k disk
000500040916      *clienti a cui inviare da orm commissionato
000600060214     ffnblp66j  if   e           k disk
000700010927     f                                     rename(fnblp000:fnblp)
000800000529     ffnvab00t  uf a e             disk    usropn
000900000518     ffnvab0wt  uf a e             disk    rename(fnvab000:fnvabw)
001000051114     ffIAR901l  if   e           k disk
001100060214     ffiar401l  uf   e           k disk    commit
001200021223     fFiar501l  if   e           k Disk
001300011005     FFIARBF1C  IF   E           K DISK
001400011005     F                                     ignore(FNARBD00:FIARBT00:FNARBG00)
001500011005     F                                     ignore(fnarbp00)
001600011005     fazorg01l  if   e           k disk
001700000517     ftabel00f  if   e           k disk
001800040825     ffnarb01l  if   e           k disk
001900000517      *--------------------------------------------------------------------
002000000522     D kpjba         e ds
002100000517     D ds3q          e ds
002200000518     D dsbl4w        e ds
002300000605     D dsbl4j        e ds
002400010927     D dsbl4z        e ds
002500011005     D og143         e ds
002600021223     d dar5ban       e ds
002700000529     d  membro         s              8    inz('M       ')
002800000518     d  comman         s             80
002900000518     d  lungh          s             15  5
003000000518     d  x              s              3  0
003100011005     d  y              s              3  0
003200000517     d  codut          s              1  0 inz(1)
003300000517     d  cod            s                   like(tblcod)
003400000517     d  key            s                   like(tblkey)
003500000517     d  ksc            s                   like(blpksc)
003600010927     d  kscj           s              7
003700000517     d  ksc99          s                   like(blpksc)
003800000517     d  ccm            s                   like(blpccm)
003900060214     d  ktrc           s                   like(ar4trc)
004000000605     d  aggaut         s                   like(§b4bau)
004100021223     d  KAr5Trd        s                   Like(Ar5Trd) Inz('BAN')
004200000518     d  woggi          s              8  0
004300000518     d  woggig         s              8  0
004400000518     d  dataora        s             14  0
004500000518     d  dataeur        s               d   datfmt(*eur)
004600000518     d  dataiso        s               d   datfmt(*iso)
004700000519     d  como7          s              7
004800021223
004900021223     d                 ds
005000021223     d VabTba                  1      2  0
005100021223     d VabNba                  3      4  0
005200021223     d VabTb2                  5      6  0
005300021223     d VabNb2                  7      8  0
005400021223     d VabAnt                  1      9  0
005500021223
005600000519     d kscalfa         ds
005700000519     d  kscal                         7
005800000519     d  progr                         1
005900000529     d  mbr            s              7    dim(10)
006000011005     d  fip            s              3  0 dim(50)
006100000517     d
006200000529     d  cm1            s              1    dim(47) ctdata perrcd(47)
006300000529     d  cm2            s              1    dim(48) ctdata perrcd(48)
006400000529     d  cm3            s              1    dim(48) ctdata perrcd(48)
006500000529     d  cm4            s              1    dim(51) ctdata perrcd(51)
006600000529     d  cm5            s              1    dim(37) ctdata perrcd(37)
006700000529     d  cm6            s              1    dim(27) ctdata perrcd(27)
006800000517      *--------------------------------------------------------------------
006900000517     c
007000000519     c     ktab1         setll     tabel                                  95
007100000517     c                   do        *hival
007200000519     c   95              read      tabel                                  99
007300000517     C   99              leave
007400000529     c                   if        tblcod <> '3Q'
007500000529     c                   leave
007600000529     c                   end
007700000522     c                   if        tblflg <> *blank
007800000522     c                   iter
007900000522     c                   end
008000000519     c                   setoff                                       95
008100000517     c                   movel     tbluni        ds3q
008200000517     c                   movel     tblkey        ksc
008300000519     c                   movel     tblkey        kscalfa
008400000517     c                   movel     tblkey        ksc99
008500000517     c                   move      9999          ksc99
008600000915     c                   move      ksc           ccm
008700010927     c                   move      ksc           kscj
008800000517     c
008900000914     c
009000060214     c     ksc           chain     fnblp65j                           98
009100060214     c   98kblp          chain     fnblp65j                           98
009200060214     c   98kscj          chain     fnblp66j                           98
009300000914     c
009400060214     c                   if        %found(fnblp65j) or %found(fnblp66j)
009500000518     c                   exsr      carica
009600000517     c                   exsr      alloca
009700000518     c                   exsr      estrai
009800000518     c                   exsr      aggmbr
009900000518     c                   exsr      pulisci
010000000518     c                   exsr      libera
010100000529     c                   else
010200000529      *se non trovo nulla per il cliente leggo la tabella fino al prossimo codic
010300000529     c                   do        *hival
010400000529     c                   read      tabel                                  99
010500000529     c   99              leave
010600000529     c                   if        tblflg <> *blank
010700000529     c                   iter
010800000529     c                   end
010900000529     c                   movel     tblkey        como7
011000000529      *confronto se stesso cliente
011100000529     c                   if        kscal = como7
011200000529     c                   iter
011300000529     c                   else
011400000529     c                   movel     como7         key
011500000529     c                   move      ' '           key
011600000529     c     ktab          setll     tabel                                  95
011700000529     c                   leave
011800000529     c                   endif
011900000529     c                   enddo
012000000518     c                   endif
012100000517     c                   enddo
012200990504     c                   seton                                        lr
012300000518?    C*-------------------------------------------------------------
012400000518?    C* estrai - estrae x file in qtemp x successiva scrittura membri
012500000518?    C*-------------------------------------------------------------
012600000518     c     estrai        begsr
012700010927      *loop di lettura per codice cliente mittente in KSC
012800000518      *
012900000518     c                   setoff                                       98
013000060214     c     ksc           setll     fnblp65j
013100000518     c                   do        *hival
013200060214     c     ksc           reade     fnblp65j                               98
013300000518     c   98              leave
013400000621     c                   clear                   fnvabw
013500040916     c                   z-add     blpdse        blpdsew           8 0
013600040920      * prima di verificare il record da elaborare verifico che non sia
013700040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
013800040920      * se è così viene flaggato il BL4 per non trattare il record
013900040920      *
014000040920     c                   setoff                                       56
014100040920     c                   if        blpdbr > 0
014200040920     c                   exsr      aggbl4gg15
014300040920      * *in56 = on bypass il resto del loop
014400040920     c   56              iter
014500040920     c                   endif
014600000522      *aggiorno e spedisco solo le righe con data minore uguale a oggi
014700000623     c                   if        blpdse > woggig or blpft1 = *blank
014800000705     c                             or blpdse = 0
014900040825     c*
015000040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
015100040825     c*partenza a *zeros
015200040825     c                   exsr      chkdse
015300040825     c*
015400040825     c  N55              iter
015500000522     c                   end
015600000519     c                   exsr      datiagg
015700000518     c                   exsr      scrivi
015800000519     c                   exsr      aggbl4
015900000518     c                   enddo
016000010927      *loop di lettura con codice cliente in CCM e ksc 9999
016100000518     c                   setoff                                       98
016200060214     c     kblp          setll     fnblp65j
016300000518     c                   do        *hival
016400060214     c     kblp          reade     fnblp65j                               98
016500000518     c   98              leave
016600000518     c                   clear                   fnvabw
016700040916     c                   z-add     blpdse        blpdsew
016800040920      * prima di verificare il record da elaborare verifico che non sia
016900040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
017000040920      * se è così viene flaggato il BL4 per non trattare il record
017100040920      *
017200040920     c                   setoff                                       56
017300040920     c                   if        blpdbr > 0
017400040920     c                   exsr      aggbl4gg15
017500040920      * *in56 = on bypass il resto del loop
017600040920     c   56              iter
017700040920     c                   endif
017800000623     c                   if        blpdse > woggig or blpft1 = *blank
017900000705     c                             or blpdse = 0
018000040825     c*
018100040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
018200040825     c*partenza a *zeros
018300040825     c                   exsr      chkdse
018400040825     c*
018500040825     c  N55              iter
018600000522     c                   end
018700000518     c                   exsr      datiagg
018800000518     c                   exsr      scrivi
018900000518     c                   exsr      aggbl4
019000000518     c                   enddo
019100060214      *loop di lettura con codice cliente in AR4NOT (altra V.L.)
019200000518     c
019300010927     c                   setoff                                       98
019400060214     c     kscj          setll     fnblp66j
019500011002     c                   seton                                        66
019600010927     c                   do        *hival
019700060214     c     kscj          reade     fnblp66j                               98
019800010927     c   98              leave
019900010927     c                   clear                   fnvabw
020000040916     c                   z-add     blpdse        blpdsew
020100040920      * prima di verificare il record da elaborare verifico che non sia
020200040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
020300040920      * se è così viene flaggato il BL4 per non trattare il record
020400040920      *
020500040920     c                   setoff                                       56
020600040920     c                   if        blpdbr > 0
020700040920     c                   exsr      aggbl4zgg15
020800040920      * *in56 = on bypass il resto del loop
020900040920     c   56              iter
021000040920     c                   endif
021100010927     c                   if        blpdse > woggig or blpft1 = *blank
021200010927     c                             or blpdse = 0
021300040825     c*
021400040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
021500040825     c*partenza a *zeros
021600040825     c                   exsr      chkdse
021700040825     c*
021800040825     c  N55              iter
021900010927     c                   end
022000010927     c                   exsr      datiagg
022100010927     c                   exsr      scrivi
022200010927     c                   exsr      aggbl4Z
022300010927     c                   enddo
022400011002     c                   setoff                                       66
022500010927
022600000518     c                   endsr
022700040825?    C*-------------------------------------------------------------
022800040825?    C* chkdse - verifiche aggiuntive in caso d data spunta entrata in part. = 0
022900040825?    C*-------------------------------------------------------------
023000040825     c     chkdse        begsr
023100040825     c*
023200040825     c*se in partenza data spunta entrata è *zeros chaino le bolle in arrivo
023300040825     c*x verificare se collo è stato poi cmq "spuntato" in arrivo
023400040825     c                   setoff                                       55
023500040825     c                   if        blpdse = 0
023600040825     c     karb1_c       chain     fnarb01l
023700040825     c                   if        %found(fnarb01l)
023800040916     c                   if        arbdam > 0 AND  arbcca <> '5' and
023900040825     c*paletto la condizione a partire dal 20040823 xchè è il giorno in cui
024000040825     c*mi sono accorto del problema (l'ha detto Luciano d fare così!)
024100040916     c                             blpdim > 20040823   OR
024200040916     c                             arbdam > 0 AND  arbcca = '5' and
024300040916     c                             arbdam <> arbdcm and blpdim > 20040823
024400040825     c                   seton                                        55
024500040916     c                   z-add     blpdrt        blpdsew
024600040825     c                   endif
024700040825     c                   endif
024800040825     c                   endif
024900040825     c*
025000040825     c                   endsr
025100000518?    C*-------------------------------------------------------------
025200060214?    C* datiagg- dati aggiuntivi da ar4 e AR9
025300000518?    C*-------------------------------------------------------------
025400000518     c     datiagg       begsr
025500000518      *
025600000518      *seconda ragione sociale cliente
025700000518     c                   move      'D'           ktrc
025800060519     c     kar4          chain(N)  fiar401l                           97
025900060214     c  n97              movel     ar4not        vabrd2
026000000621      *memorizzo se autogenerata aggiornata
026100000605     c                   move      'J'           ktrc
026200060519     c     kar4          chain(N)  fiar401l                           97
026300060214     c  n97              movel     ar4not        dsbl4j
026400000605     c  n97              movel     §b4bau        aggaut
026500000605     c   97              movel     *blank        aggaut
026600000621      *memorizzo importo assicuraz. ordinaria
026700090216     c* n97              movel     §b4xco        vabxco
026800090216     c* n97              z-add     §b4ias        vabias
026900090216     c* n97              movel     §b4vas        vabvas
027000000605      *campo note bolla
027100000605     c                   move      '8'           ktrc
027200060519     c     kar4          chain(N)  fiar401l                           97
027300060214     c  n97              movel     ar4not        vabnot
027400000518      *campo note bolla 2
027500000518     c                   move      '9'           ktrc
027600060519     c     kar4          chain(N)  fiar401l                           97
027700060214     c  n97              movel     ar4not        vabnt2
027800000518      *dati relativi al contrassegno
027900051114     c     kAR9          chain     fIAR901l                           97
028000011005      * non trovato C/A nell'estensione se c'è anomalia ed è una bolla poste
028100011005      * controllo nelle variazioni se esisteva il contrassegno
028200011005     c                   if        *in97
028300011005     c                   if        blpcca <> *blank
028400011005     c     blplnp        lookup    fip                                    22
028500011005     c                   if        *in22
028600051114     c     kAR9          chain     fnarbk00                           97
028700011005     c  n97              z-add     arbcas        vabcas
028800011005     c  n97              movel     arbtic        vabtic
028900011005     c  n97              movel     arbgca        vabgca
029000011005     c  n97              movel     arbvca        vabvca
029100011005     c                   end
029200011005     c                   end
029300011005     c                   else
029400051114     c                   z-add     AR9cas        vabcas
029500051114     c                   movel     AR9tic        vabtic
029600051114     c                   movel     AR9gca        vabgca
029700051114     c                   movel     AR9vca        vabvca
029800011005     c                   endif
029900021223      * Dati bancali
030000021223     c                   Clear                   VabAnt
030100030130     c                   If        %Subst(BlpGva:1:1) = 'B'
030200021223     c     KFiar5        Chain     Fiar501l
030300021223     c                   If        %Found(Fiar501l)
030400021223     c                   Movel     Ar5Uni        Dar5Ban
030500021223     c                   Movel     §Ar5Tba       VabTba
030600021223     c                   Movel     §Ar5Nba       VabNba
030700021223     c                   Movel     §Ar5Tb2       VabTb2
030800021223     c                   Movel     §Ar5Nb2       VabNb2
030900021223     c                   EndIf
031000021223     c                   EndIf
031100021223
031200000518     c                   endsr
031300000518?    C*-------------------------------------------------
031400000518?    C* scrivi - scrive   file work qtemp
031500000518?    C*-------------------------------------------------
031600000518     c     scrivi        begsr
031700000605     c                   move      *blank        vabscl
031800000605     c                   if        blpsop = '&'and
031900000605     c                             aggaut = *blank
032000000605     c                   movel     'A'           vabscl
032100000605     c                   endif
032200070222     c                   clear                   vabatb
032300000518     c                   movel     blpcbo        vabcbo
032400000518     c                   movel     blprsd        vabrsd
032500000518     c                   movel     blpind        vabind
032600000518     c                   movel     blpcad        vabcad
032700010927     c                   movel     blplod        vablod
032800000518     c                   movel     blpprd        vabprd
032900000518     c                   movel     blpnzd        vabnzd
033000000518     c                   movel     blpgc1        vabgc1
033100000518     c                   movel     blpgc2        vabgc2
033200000518     c                   movel     blptsp        vabtsp
033300000518     c                   movel     blpnas        vabnas
033400000518     c                   movel     blprma        vabrma
033500000518     c                   movel     blpctm        vabctm
033600000518     c                   movel     blpffd        vabffd
033700000518     c                   movel     blptcr        vabtcr
033800000518     c                   movel     blpcts        vabcts
033900000518     c                   movel     blpftm        vabftm
034000000518     c                   movel     blpvad        vabvad
034100000518     c                   movel     blpgma        vabgma
034200000518     c                   movel     blpgga        vabgga
034300000518     c                   movel     blpgva        vabgva
034400010111     c                   move      ' '           vabgva
034500000518     c                   movel     blptc1        vabtc1
034600000518     c                   movel     blptc2        vabtc2
034700000518     c                   movel     *blank        vabcmo
034800000518     c                   movel     *blank        vabnmo
034900000703     c                   move      '.'           vabnmo
035000060214      * se codice cliente contenuto nel campo note di FIAR4
035100011002     c                   if        *in66
035200060214     c                   move      ar4ksc        vabccm
035300011002     c                   else
035400011002      * se codice cliente in FNBLP nel campo blpccm
035500000628     c                   if        blpccm > 0
035600000628     c                   z-add     blpccm        vabccm
035700000628     c                   else
035800011002      * se codice cliente in FNBLP nel campo blpksc
035900000628     c                   z-add     blpksc        vabccm
036000000628     c                   end
036100011002     c                   end
036200000518     c                   z-add     blplnp        vablnp
036300000518     c                   z-add     blpaas        vabaas
036400000518     c                   z-add     blpmgs        vabmgs
036500000518     c                   z-add     blpnrs        vabnrs
036600000518     c                   z-add     blpnsp        vabnsp
036700000518     c                   z-add     blplna        vablna
036800000518     c                   z-add     blpctr        vabctr
036900000518     c                   z-add     blpncl        vabncl
037000000703     c                   z-add     blppkf        vabpkb
037100000703     c                   z-add     blpvlf        vabvlb
037200000518     c                   z-add     blpqft        vabqft
037300000518     c                   z-add     blprmn        vabrmn
037400000518     c                   z-add     blpncd        vabncd
037500000518     c                   z-add     blpnca        vabnca
037600000518     c                   z-add     blpznc        vabznc
037700040916     c                   z-add     blpdsew       vabdcr
037800000621     c                   z-add     0             vabhcr
037900000518     c                   z-add     blpvmd        vabvmd
038000021223     c**!!!              z-add     0             vabant
038100001106      *
038200001106     c                   movel     blprmo        vabrmo
038300001106     c                   IF        VABRMO <> *blanks
038400001106     c                   movel     blpcam        vabcmo
038500001106     c                   movel     blpnzm        vabnmo
038600001106     c                   ELSE
038700001106     c                   clear                   vabcmo
038800001106     c                   clear                   vabnmo
038900001106     c                   ENDIF
039000001106     c                   move      '.'           vabnmo
039001090216     c                   z-add     blpias        vabias
039002090216     c                   movel     blpvas        vabvas
039003090220     C                   movel     blpXCO        vabXCO
039100001106      *
039200000518     c                   write     fnvabw
039300001106      *
039400000518     c                   endsr
039500000518?    C*-------------------------------------------------------------
039600060214?    C* aggbl4 - aggiornamento tipo record W ar4 data presa in carico
039700000518?    C*-------------------------------------------------------------
039800000518     c     aggbl4        begsr
039900000518      *
040000000519      *aggiorna record con data presa in carico spedizione
040100000518     c                   move      'W'           ktrc
040200060214     c     kar4          chain     fiar401l                           97
040300000518     c                   if        not *in97
040400000518     c                   movel     woggig        §b4dat
040500060214     c                   movel     dsbl4w        ar4not
040600060214     c                   z-add     woggig        ar4duv
040700060214     c                   update    fiar4000
040800000518     c                   endif
040900000518     c                   endsr
041000010927?    C*-------------------------------------------------------------
041100060214?    C* aggbl4Z- aggiornamento tipo record Z ar4 cod.cli/datainvio
041200010927?    C*-------------------------------------------------------------
041300010927     c     aggbl4Z       begsr
041400010927      *
041500010927      *aggiorna record con data elaborazione
041600010927     c                   move      'Z'           ktrc
041700060214     c     kar4          chain     fiar401l                           97
041800010927     c                   if        not *in97
041900010927     c                   movel     woggig        §b4dtr
042000011010     c                   movel     kscj          §b4okc
042100060214     c                   movel     dsbl4z        ar4not
042200060214     c                   z-add     woggig        ar4duv
042300060214     c                   update    fiar4000
042400010927     c                   endif
042500010927     c                   endsr
042600040916?    C*-------------------------------------------------------------
042700060214?    C* aggbl4gg15 - aggiornamento tipo record W ar4 data ritiro + 15 giorni
042800040916?    C*-------------------------------------------------------------
042900040916     c     aggbl4gg15    begsr
043000040916      *
043100040920     c                   if        blpdbr < woggig15
043200040916      *aggiorna record con data presa in carico spedizione
043300040916     c                   move      'W'           ktrc
043400060214     c     kar4          chain     fiar401l                           97
043500040916     c                   if        not *in97
043600040916     c                   movel     20010101      §b4dat
043700060214     c                   movel     dsbl4w        ar4not
043800060214     c                   z-add     woggig        ar4duv
043900060214     c                   update    fiar4000
044000040920     c                   seton                                        56
044100040916     c                   endif
044200040916     c                   endif
044300040916     c                   endsr
044400040916?    C*-------------------------------------------------------------
044500040916?    C* aggbl4Zgg15- aggiornamento tipo record Z bl4 cod.cli/datainvio
044600040916?    C*-------------------------------------------------------------
044700040916     c     aggbl4Zgg15   begsr
044800040916      *
044900040920     c                   if        blpdbr < woggig15
045000040916      *aggiorna record con data elaborazione
045100040916     c                   move      'Z'           ktrc
045200060214     c     kar4          chain     fiar401l                           97
045300040916     c                   if        not *in97
045400040916     c                   movel     20010101      §b4dtr
045500040916     c                   movel     kscj          §b4okc
045600060214     c                   movel     dsbl4z        ar4not
045700060214     c                   z-add     woggig        ar4duv
045800040920     c                   seton                                        56
045900060214     c                   update    fiar4000
046000040916     c                   endif
046100040916     c                   endif
046200040916     c                   endsr
046300000517?    C*-------------------------------------------------
046400000517?    C* carica - prepara skiera
046500000517?    C*-------------------------------------------------
046600000517     c     carica        begsr
046700000517     c                   clear                   mbr
046800000517     c                   z-add     1             x
046900000529     c                   movel     §3qcks        mbr(x)
047000000517     c                   do        *hival
047100000519     c                   read      tabel                                  99
047200000522     c   99              leave
047300000522     c                   if        tblflg <> *blank
047400000522     c                   iter
047500000522     c                   end
047600000519     c                   movel     tblkey        como7
047700000522     c                   movel     tbluni        ds3q
047800000522      *confronto se stesso cliente
047900000519     c                   if        kscal = como7
048000000517     c                   add       1             x
048100000529     c                   movel     §3qcks        mbr(x)
048200000519     c                   else
048300000522     c                   movel     como7         key
048400000522     c                   move      ' '           key
048500000522     c     ktab          setll     tabel                                  95
048600000519     c                   leave
048700000519     c                   endif
048800000517     c                   enddo
048900000517     c                   endsr
049000000517?    C*-------------------------------------------------
049100000517?    C* alloca - allocazione membri da tabella cliente
049200000517?    C*-------------------------------------------------
049300000517     c     alloca        begsr
049400000518     c                   do        10            x
049500000518     c                   if        mbr(x) <> *blank
049600000529     c                   move      mbr(x)        membro
049700000522     c                   exsr      check
049800000518     c                   movel     *blanks       comman
049900000518     c                   movea     membro        cm2(37)
050000000529     c                   movea     cm2           comman
050100000518     c                   z-add     48            lungh
050200000518     c                   do        *hival
050300000518     c                   exsr      esegue
050400000518     c  n60              leave
050500000518     c                   enddo
050600000529     c                   else
050700000529     c                   leave
050800000518     c                   endif
050900000518     c                   enddo
051000000522     c                   endsr
051100000518?    C*-------------------------------------------------
051200000522?    C* check  - testa esistenza membri
051300000518?    C*-------------------------------------------------
051400000522     c     check         begsr
051500000518     c                   movel     *blanks       comman
051600000518     c                   movea     membro        cm4(41)
051700000529     c                   movea     cm4           comman
051800000518     c                   z-add     51            lungh
051900000518     c                   exsr      esegue
052000000518     c   60              exsr      crea
052100000518     c                   endsr
052200000518?    C*-------------------------------------------------
052300000518?    C* crea   - crea  membri
052400000518?    C*-------------------------------------------------
052500000518     c     crea          begsr
052600000518     c                   movel     *blanks       comman
052700000518     c                   movea     membro        cm5(27)
052800000529     c                   movea     cm5           comman
052900000518     c                   z-add     37            lungh
053000000518     c                   exsr      esegue
053100000518     c                   endsr
053200000518?    C*--------------------------------------------------
053300000518?    C* aggmbr - ovrdbf x  membri cliente
053400000518?    C*--------------------------------------------------
053500000518     c     aggmbr        begsr
053600000518     c                   do        10            x
053700000518     c                   if        mbr(x) <> *blank
053800000529     c                   move      mbr(x)        membro
053900000518     c                   movel     *blanks       comman
054000000518     c                   movea     membro        cm1(37)
054100000529     c                   movea     cm1           comman
054200000518     c                   z-add     47            lungh
054300000518     c                   exsr      esegue
054400000518     c                   open      fnvab00t
054500000518     c                   exsr      copia
054600000518     c                   close     fnvab00t
054700000529     c                   else
054800000529     c                   leave
054900000518     c                   endif
055000000518     c                   enddo
055100000518
055200000518?    C                   commit
055300000518
055400000518     c                   endsr
055500000518?    C*-------------------------------------------------
055600000518?    C* copia  - copia record nei membri cliente
055700000518?    C*-------------------------------------------------
055800000518     c     copia         begsr
055900000518     c     *start        setll     fnvab0wt
056000000518     c                   do        *hival
056100000518     c                   read      fnvab0wt                               96
056200000518     c   96              leave
056300000518     c                   write     fnvab000
056400000518     c                   enddo
056500000518     c                   endsr
056600000518?    C*-------------------------------------------------
056700000518?    C* esegue - crea  membri
056800000518?    C*-------------------------------------------------
056900000518     c     esegue        begsr
057000000518     c                   call      'QCMDEXC'                            60
057100000518     c                   parm                    comman
057200000518     c                   parm                    lungh
057300000518     c                   endsr
057400000518?    C*--------------------------------------------------
057500000518?    C* pulisci - pulisce file di qtemp per nuovo cliente
057600000518?    C*--------------------------------------------------
057700000518     c     pulisci       begsr
057800000529     c                   close     fnvab0wt
057900000518     c                   movel     *blanks       comman
058000000529     c                   movea     cm6           comman
058100000518     c                   z-add     27            lungh
058200000518     c                   exsr      esegue
058300000529     c                   open      fnvab0wt
058400000518     c                   endsr
058500000518?    C*-------------------------------------------------
058600000518?    C* libera - disallocazione membri da tabella cliente
058700000518?    C*-------------------------------------------------
058800000518     c     libera        begsr
058900000518     c                   do        10            x
059000000518     c                   if        mbr(x) <> *blank
059100000529     c                   move      mbr(x)        membro
059200000518     c                   movel     *blanks       comman
059300000518     c                   movea     membro        cm3(37)
059400000529     c                   movea     cm3           comman
059500000518     c                   z-add     48            lungh
059600000518     c                   do        *hival
059700000518     c                   exsr      esegue
059800000518     c  n60              leave
059900000518     c                   enddo
060000000529     c                   else
060100000529     c                   leave
060200000518     c                   endif
060300000518     c                   enddo
060400000518     c                   endsr
060500000517?    C*-------------------------------------------------
060600000517?    C* *INZSR - Settaggio iniziale programma STANDARD
060700000517?    C*-------------------------------------------------
060800000517     C     *inzsr        begsr
060900000517?    C*
061000000517     C     *entry        plist
061100000517     C                   parm                    kpjba
061200000517     c     ktab          klist
061300000517     c                   kfld                    codut
061400000517     c                   kfld                    cod
061500000517     c                   kfld                    key
061600000517      *
061700000517     c     ktab1         klist
061800000517     c                   kfld                    codut
061900000517     c                   kfld                    cod
062000000517     c                   move      '3Q'          cod
062100000517     c     kblp          klist
062200000517     c                   kfld                    ksc99
062300000517     c                   kfld                    ccm
062400051114     c     kAR9          klist
062500000518     c                   kfld                    blpaas
062600000518     c                   kfld                    blplnp
062700000518     c                   kfld                    blpnrs
062800000518     c                   kfld                    blpnsp
062900060214     c     kar4          klist
063000000518     c                   kfld                    blpaas
063100000518     c                   kfld                    blplnp
063200000518     c                   kfld                    blpnrs
063300000518     c                   kfld                    blpnsp
063400000518     c                   kfld                    ktrc
063500021223     c     KFiar5        Klist
063600021223     c                   Kfld                    BlpAas
063700021223     c                   Kfld                    BlpLnp
063800021223     c                   Kfld                    BlpNrs
063900021223     c                   Kfld                    BlpNsp
064000021223     c                   Kfld                    KAr5Trd
064100040825     c     Karb1_c       Klist
064200040825     c                   Kfld                    BlpAas
064300040825     c                   Kfld                    BlpLnp
064400040825     c                   Kfld                    BlpNrs
064500040825     c                   Kfld                    BlpNsp
064600000518      *
064700000518     c                   time                    dataora
064800000518     c                   move      dataora       woggi
064900000518     c                   move      woggi         dataeur
065000000518     c                   move      dataeur       dataiso
065100000518     c                   move      dataiso       woggig
065200040916     c     dataiso       subdur    15:*d         dataiso
065300040916     c                   move      dataiso       woggig15          8 0
065400011005     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
065500011005     C                   CLEAR                   FIP
065600011005     C     *LOVAL        SETLL     AZORG01L
065700011005     C                   DO        *HIVAL
065800011005     C                   READ      AZORG01L                               94
065900011005     C   94              LEAVE
066000011005     C                   MOVEL     oRGDE3        OG143
066100020717     C*** sostituito con il network
066200020717     C***  §OGPT         IFEQ      'S'
066300020717     C     §OgNTW        IFEQ      'PPT'
066400011005     C                   Z-ADD     1             Y
066500011005     C     *ZEROS        LOokuP    FIP(Y)                                 23
066600011005     C   23              Z-ADD     ORGFIL        FIP(Y)
066700011005     C                   END
066800011005     C                   END
066900011005     C*---------------------------------------------------------------*
067000000517     c                   endsr
067100000518**
067200000529OVRDBF FILE(FNVAB00T)           MBR(MXX       )
067300000518**
067400000529ALCOBJ OBJ((FNVAB00T *FILE *EXCL    MXX       ))
067500000518**
067600000529DLCOBJ OBJ((FNVAB00T *FILE *EXCL    MXX       ))
067700000518**
067800000529CHKOBJ OBJ(FNVAB00T) OBJTYPE(*FILE) MBR(MXX       )
067900000518**
068000000529ADDPFM FILE(FNVAB00T) MBR(MXX       )
068100000518**
068200000518CLRPFM FILE(QTEMP/FNVAB0WT)
