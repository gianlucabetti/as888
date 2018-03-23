000100040830     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300030422     fsicop01l  if   e           k disk
000400030108     ffictt01l  uf a e           k disk
000500030108     ffictd01l  uf a e           k disk
000600030108     fazorg01l  if   e           k disk
000700030109     ffifpt01l  if   e           k disk
000800030120     ffifre01l  if   e           k disk
000900030123     ftntbe01l  if   e           k disk
001000030123     ftabel00f  if   e           k disk
001100030207     ffiapd01l  if   e           k disk
001200030305     fazcae02l  if   e           k disk    usropn
001300030305     f*zcae03l  if   e           k disk    usropn
001400020517     fqsysprt   o    f  132        printer infds(stamp)
001500020626     d kpjba         e ds
001600030110
001700030109      *ingresso
001800030108     D param           DS
001900030108     D  datadal                       8s 0
002000030108     D  dataal                        8s 0
002100030108     D  parpo                         3s 0
002200030415      *chiamata rettifiche costi figurativi
002300030415     D parrett         DS
002400030415     D  data1                         8s 0
002500030415     D  poret                         3s 0
002600030109      *ds reperimento tariffa
002700030109     D partar          DS
002800030109     D  data                          8s 0
002900030109     D  poges                         3s 0
003000030109     D  tipopre                       1
003100030109     d  dsfgtout                           like(fgtds)
003200030109     d  error                         1
003300061009     D pomail          s              3s 0
003400061009     D pomaila         s              3
003500030822     d dctt01        e ds
003600030109     d fgtds         e ds                  extname(fifgt00f)
003700030123
003800030414     d og150         e ds
003900030123     d dcre          e ds
004000030123     d ds8d          e ds
004100030204     d dccg          e ds
004200030519     d ds8a          e ds
004300030109
004400020517     d stamp           ds
004500020517     d  lin                  367    368b 0
004600030108     d dataiso         s               d   datfmt(*iso)
004700030108     d dataeur         s               d   datfmt(*eur)
004800030109     d import          s             13  3
004900030109     d tcopnsp         s                   like(import)
005000030109     d tcopncl         s                   like(import)
005100030109     d tcoppkf         s                   like(import)
005200030109     d tcopvlf         s                   like(import)
005300030109     d tcopban         s                   like(import)
005400030109     d tcopimv         s                   like(import)
005500030109     d timport         s                   like(import)
005600030123     d tfretimp        s                   like(import)
005700030123     d tfretima        s                   like(import)
005800030305     d kcopfil         s                   like(copfil)
005900030109     d excopdat        s                   like(copdat)
006000030212     d excopvoc        s                   like(copvoc)
006100030109     d copvoca         s              3
006200030109     d keypo           s              3  0
006300030519     d x               s              3  0 inz
006400090903     d xo              s              3  0 inz
006500030519     d com03           s              3    inz
006600030523     c* voci delle competenze da considerare
006700030519     d voc             s              3    inz dim(500)
006800090903     d voco            s              3    inz dim(100)
006900090903     d vocor           s              3    inz dim(100)
007000030110
007100030110     D WLBDAT          DS
007200030110     D  G02DAT                 1      8  0
007300030110     D  G02INV                 9     16  0
007400030110     D  G02ERR                17     17
007500030110     D  G02TGI                18     22  0
007600030109     d
007700000607      *--------------------------------------------------------------------
007800000607     c
007900030108      * se non impostata la parzializzazione per il PO li elaboro tutti
008000030108     c                   if        parpo = 0
008100040831      * richiamo pgm azzeramento e pulizia dettaglio prima di elaborare
008200040831     c                   call      'FICN36R6'
008300040831     c                   parm                    parpo
008400040831     c                   parm                    datadal
008500040831     c                   parm                    dataal
008600040831
008700030305     c                   open      azcae02l
008800030108      *loop sui record di azorg per reperire tutti i po
008900030422     c     *loval        setll     azorg01l
009000030422     c                   do        *hival
009100030422     c                   read      azorg01l                               99
009200030422     c   99              leave
009300030422     c                   z-add     orgfil        keypo
009400030422      * verifica se gestito macchinone controllo rettifiche per affitto vdl
009500041202     c*****              movel     orgdf0        og150
009600041202     c*****              if        §ogvol <> *BLANK  and §ogvol <> *zeros
009700041202      * variata la chiamata per affitti figurativi ora tenta per tutte le
009800060901      * filiali e verifica se esistono affitti/accrediti da aggiungere in c.e.g.
009900041202     c                   if        orgfag = 'F'
010000030422     c                   move      dataal        data1
010100030422     c                   move      01            data1
010200030422     c                   move      orgfil        poret
010300060901      * rettifica FAF affitti figurativi vdl e impianti
010400030422     c                   call      'FICN36R4'
010500030422     c                   parm                    parrett
010600060901      * rettifica FAC accrediti facchinaggio da altri P.O.
010700060901     c                   call      'FICN36R7'
010800060901     c                   parm                    parrett
010900060901      * rettifica RAC accrediti ritiri da altri P.O.
011000060901     c                   call      'FICN36R8'
011100060901     c                   parm                    parrett
011200060901      * rettifica CAC accrediti consegne da altri P.O.
011300060901     c                   call      'FICN36R9'
011400060901     c                   parm                    parrett
011500030422     c                   endif
011600030422
011700030422     c                   exsr      elabora
011800030422     c                   enddo
011900030422     c                   else
012000040831      * richiamo pgm azzeramento e pulizia dettaglio prima di elaborare
012100040831     c                   z-add     parpo         keypo
012200040831
012300040831     c                   call      'FICN36R6'
012400040831     c                   parm                    parpo
012500040831     c                   parm                    datadal
012600040831     c                   parm                    dataal
012700040831
012800030422     c                   exsr      elabora
012900030422     c                   endif
013000020517
013100020521     c                   seton                                        lr
013200020521      *------------------------------------------------------
013300020521     c     elabora       begsr
013400020521      *------------------------------------------------------
013500030109     c                   clear                   excopdat
013600030305      *
013700030109     c     ksic          setll     sicop01l
013800030109     c                   do        *hival
013900030113     c     keypo         reade     sicop01l                               98
014000030109     c   98              leave
014100030109      * se data maggiore al limite massimo richiesto esco dal loop
014200030110     c                   if        copdat > dataal
014300030109     c                   leave
014400030109     c                   endif
014500030523     c* se la voce non è stata caricata nella schiere VOC vuol dire che è da
014600030519     c* escludere
014700030519     c                   z-add     1             x
014800030519     c                   move      copvoc        com03
014900030519     c     com03         lookup    voc(x)                                 91
015000030523     c                   if        not *in91
015100030519     c                   iter
015200030519     c                   end
015300030918      * se la filiale è estera e la voce è una voce di giacenza la salto
015400030918     c                   if        orgfl1 = 'E'
015500030918     c                   if        copvoc = 262 or copvoc = 265 or copvoc = 267
015600030918     c                   iter
015700030918     c                   endif
015800030918     c                   endif
015900030109      * a rottura di data reperisco la tariffa valida per quel giorno e i
016000030109      * dati relativi alla coop
016100030109     c                   if        copdat <> excopdat
016200030109      * controllo cosa fare per la testata scrittura o aggiornamento
016300030109     c                   if        excopdat > *zeros
016400030109     c                   exsr      trattatesta
016500030109     c                   endif
016600030109      *pulisco campi di comodo per totali testata
016700030109     c                   clear                   tcopnsp
016800030109     c                   clear                   tcopncl
016900030109     c                   clear                   tcoppkf
017000030109     c                   clear                   tcopvlf
017100030109     c                   clear                   tcopban
017200030109     c                   clear                   tcopimv
017300030109     c                   clear                   timport
017400030905     c                   clear                   tfretimp
017500030905     c                   clear                   tfretima
017600040830      * verifico se il po in esame è da accumulare su un PO gestione diverso
017700040830      * prima imposto la chiave per le testate e i dettagli se trovo da
017800040830      *sostituire il POgestione in azcae lo forzo in seguito
017900030305     c                   z-add     copfil        kcopfil
018000030305     c                   if        parpo = 0
018100030305     c                   exsr      cercapadre
018200030305     c                   end
018300030305      *controllo la tariffa se valida altrimenti la reperisco
018400030305     c                   exsr      reptar
018500030526     c*                  move      copdat        excopdat
018600030109      * a rottura di data leggo anche la testata della valorizzazione
018700030707     c                   seton                                        97
018800040901     c                   clear                   fictt000
018900030707     c     kctt          setll     fictt01l
019000030707     c                   do        *hival
019100030707     c     kctt          reade     fictt01l
019200030707     c                   if        %eof(fictt01l)
019300030707     c                   leave
019400030109     c                   endif
019500030707     c                   if        ctttsr = 'X'
019600030707     c                   setoff                                       97
019700030707     c                   leave
019800040902     c                   else
019900040902     c                   clear                   fictt000
020000030707     c                   end
020100030707     c                   enddo
020200030708     c                   endif
020300030822     c                   move      cttflr        dctt01
020400030109
020500040830      *ricerca il record per verificare se devo cumulare o rigenerare
020600040830      * il record di dettaglio
020700030109     c     kctd          chain     fictd01l                           96
020800030124     c                   move      copvoc        copvoca
020900030109     c     kfpt          chain     fifpt01l                           95
021000090903     c                   if        *in95
021100090903     c                   z-add     1             xo
021200090908     c     copvoca       lookup    voco(xo)                               89
021300090908     c   89              move      vocor(xo)     copvoca
021400090903     c     kfpt          chain     fifpt01l                           95
021500090903     c                   endif
021600090903      *
021700040830      *record dettaglio in scrittura se non esiste
021800040830     c                   if        *in96
021900030109     c                   clear                   fictd000
022000030109     c                   z-add     fgtpdr        ctdpdr
022100030109     c                   move      fgttsr        ctdtsr
022200030305     c                   z-add     kcopfil       ctdfgs
022300030526     c*                  z-add     excopdat      ctdddc
022400030526     c                   z-add     copdat        ctdddc
022500030109     c                   z-add     copvoc        ctdvoc
022600030109     c                   z-add     copnsp        ctdnus
022700030109     c                   z-add     copncl        ctdncl
022800030109     c                   z-add     coppkf        ctdpkf
022900030109     c                   z-add     copvlf        ctdvlf
023000030109     c                   z-add     copban        ctdban
023100030109     c                   move      fpttpg        ctdtpg
023200030109     c                   z-add     copimv        ctdied
023300040830      * se la testata è già confermata azzero il costo per non cumularlo
023400030708     C                   if        cttfvl <> 'C'
023500030109     c                   exsr      calcimp
023600030109     c                   z-add     import        ctditc
023700030708     c                   else
023800030708     c                   z-add     0             ctditc
023900030708     c                   end
024000030109     c                   write     fictd000
024100030109
024200040831      *record trovato vado in add
024300030109     c                   else
024400040830
024500040817     c                   add       copimv        ctdied
024600030109      * in aggiornamento ricalcolo il valore solo se non consolidato
024700030109     c                   if        cttfvl <> 'C'
024800030109     c                   exsr      calcimp
024900030127     c                   move      fpttpg        ctdtpg
025000030127     c                   z-add     fgtpdr        ctdpdr
025100030127     c                   move      fgttsr        ctdtsr
025200030821     c                   add       import        ctditc
025300030821     c                   add       copnsp        ctdnus
025400030821     c                   add       copncl        ctdncl
025500030821     c                   add       coppkf        ctdpkf
025600030821     c                   add       copvlf        ctdvlf
025700030821     c                   add       copban        ctdban
025800030109     c                   end
025900030214     c                   update    fictd000
026000030109     c                   endif
026100030109      *totalizzo valori di testata
026200030109     c                   add       copnsp        tcopnsp
026300030109     c                   add       copncl        tcopncl
026400030109     c                   add       coppkf        tcoppkf
026500030109     c                   add       copvlf        tcopvlf
026600030109     c                   add       copban        tcopban
026700030109     c                   add       copimv        tcopimv
026800030109     c                   add       import        timport
026900030212     c                   z-add     copvoc        excopvoc
027000030526     c                   move      copdat        excopdat
027100030109
027200030109     c                   enddo
027300030109      * rottura di filiale tratto la testata per non perdere l'ultima
027400030129     c                   if        excopdat > *zeros
027500030129     c                   exsr      trattatesta
027600030129     c                   endif
027700020521     c                   endsr
027800030109      *-------------------------------------------------
027900030109     c     calcimp       begsr
028000030109      *-------------------------------------------------
028100030109     c                   clear                   import
028200030109     c                   select
028300030109      *non trovato dettaglio tariffario per la voce
028400030307     c                   when      *in95 or fptatb = 'A'
028500030109     c                   z-add     0             import
028600030109
028700030109      *peso quintali
028800030109     c                   when      fpttpg = '0'
028900100108     c                   eval(h)   import = (coppkf * fptata) /100
029000030124     c*    coppkf        mult      fptata        import
029100030109
029200030109      *colli
029300030109     c                   when      fpttpg = '1'
029400100108     c     copncl        mult(h)   fptata        import
029500030109
029600030109      *spedizioni
029700030109     c                   when      fpttpg = '2'
029800100108     c     copnsp        mult(h)   fptata        import
029900030109
030000030109      *peso kg.
030100030109     c                   when      fpttpg = '3'
030200100108     c     coppkf        mult(h)   fptata        import
030300030109
030400030109      *importo fisso
030500030109     c                   when      fpttpg = 'F'
030600100108     c                   z-add(h)  fptata        import
030700030109
030800030729      * a bancali
030900030729     c                   when      fpttpg = 'H'
031000100108     c     copban        mult(h)   fptata        import
031100030729
031200030109     c                   endsl
031300030109     c                   endsr
031400030305      *------------------------------------------------------
031500030305     c     cercapadre    begsr
031600030305      *------------------------------------------------------
031700040830      * ricerca su azcae i rekord eccezione
031800040830      * la presenza di questi rekord fa si che un P.O venga gestito da un
031900040830      * altro durante il periodo di validità del rekord
032000030305     c                   setoff                                       92
032100030305     c     kcae          setgt     azcae02l
032200030305     c     kcaep         readpe    azcae02l                               92
032300030305     c                   if        not *in92
032400030305     c                   if        copdat >= caedde and copdat <= caedsc
032500030305     c                   z-add     caetfe        kcopfil
032600030305     c                   endif
032700030305     c                   endif
032800030305     c
032900030305     c                   endsr
033000030109      *-------------------------------------------------
033100030109     c     trattatesta   begsr
033200030109      *-------------------------------------------------
033300030120      *prima di trattare la testata rilevo se esistono rettifiche da portare
033400030120      * nei valori di testata
033500030905      *cerco le rettifiche solo se non sono una filiale in gestione ad altri
033600030905     c                   if        copfil = kcopfil
033700030120     c                   exsr      cercafre
033800030905     c                   endif
033900030109     c                   if        *in97
034000030109     c                   clear                   fictt000
034100030206     c                   if        fgtpdr = 0 or fgtdcn = 0
034200030205     c                   move      'S'           cttfla
034300030205     c                   endif
034400030120     c                   z-add     fgtpdr        cttpdr
034500030120     c                   move      fgttsr        ctttsr
034600030109     c                   move      fgtdiv        cttdiv
034700120827     c*                  move      *all'0'       cttcdf
034800120827     c*                  move      apdksc        cttcdf
034900120827     c*                  move      apdcsf        cttsoc
034901120920     c                   clear                   cttsoc
034902120920     c                   clear                   cttcdf
035000030305     c                   z-add     kcopfil       cttfgs
035100030109     c                   z-add     excopdat      cttddc
035200030109     c                   z-add     tcopnsp       cttnus
035300030109     c                   z-add     tcopncl       cttncl
035400030109     c                   z-add     tcoppkf       cttpkf
035500030109     c                   z-add     tcopvlf       cttvlf
035600030109     c                   z-add     tcopban       cttban
035700030120     c                   z-add     tcopimv       cttied
035800030120     c                   z-add     timport       cttitc
035900030123     c                   z-add     tfretimp      cttitmp
036000030123     c                   z-add     tfretima      cttitma
036100030822     c                   movel     wfiller       §ctt01
036200030822     c                   move      dctt01        cttflr
036300030109     c                   write     fictt000
036400030109     c                   else
036500030109      *aggiorno solo se non consolidato
036600030109     c                   if        cttfvl <> 'C'
036700030206      *controllo se segnalare il manca tariffa se non esiste oppure se
036800030206      *non è ancora stata convalidata
036900030206     c                   if        fgtpdr = 0 or fgtdcn = 0
037000030205     c                   move      'S'           cttfla
037100030205     c                   else
037200030205     c                   move      *blank        cttfla
037300030205     c                   endif
037400030821
037500030127     c                   z-add     fgtpdr        cttpdr
037600030127     c                   move      fgttsr        ctttsr
037700030311     c                   move      fgtdiv        cttdiv
037800120827     c*                  move      *all'0'       cttcdf
037900120827     c*                  move      apdksc        cttcdf
038000120827     c*                  move      apdcsf        cttsoc
038001120920     c                   clear                   cttsoc
038002120920     c                   clear                   cttcdf
038100030821      * verifico se sono nell'ambito della stessa elaborazione e sto
038200030821      * forzando la scrittura di una filiale gestita da altri
038300030822     c                   if        §ctt01  <> wfiller
038400030109     c                   z-add     tcopnsp       cttnus
038500030109     c                   z-add     tcopncl       cttncl
038600030109     c                   z-add     tcoppkf       cttpkf
038700030109     c                   z-add     tcopvlf       cttvlf
038800030109     c                   z-add     tcopban       cttban
038900030120     c                   z-add     timport       cttitc
039000030123     c                   z-add     tfretimp      cttitmp
039100030123     c                   z-add     tfretima      cttitma
039200030109     c                   else
039300030821     c                   add       tcopnsp       cttnus
039400030821     c                   add       tcopncl       cttncl
039500030821     c                   add       tcoppkf       cttpkf
039600030821     c                   add       tcopvlf       cttvlf
039700030821     c                   add       tcopban       cttban
039800030821     c                   add       timport       cttitc
039900030821     c                   add       tfretimp      cttitmp
040000030821     c                   add       tfretima      cttitma
040100030821     c                   endif
040200030821
040300030821      *rek già consolidato vario solo il c.e. dinamico
040400030821     c                   else
040500030821      * verifico se sono nell'ambito della stessa elaborazione e sto
040600030821      * forzando la scrittura di una filiale gestita da altri
040700030822     c                   if        §ctt01  <> wfiller
040800030821     c                   z-add     tcopimv       cttied
040900030821     c                   else
041000030821     c                   add       tcopimv       cttied
041100030821     c                   endif
041200030821
041300030109     c                   endif
041400030822     c                   movel     wfiller       §ctt01
041500030822     c                   move      dctt01        cttflr
041600030124     c                   update    fictt000
041700030110     c                   endif
041800030109     c                   endsr
041900030120      *-------------------------------------------------
042000030120     c     cercafre      begsr
042100030120      *-------------------------------------------------
042200030120     c     kfre          setll     fifre01l
042300030120     c                   do        *hival
042400030120     c     kfre          reade     fifre01l                               94
042500030120     c   94              leave
042600030120      *verifico se il tipo servizio della rettifica corrisponde al tipo
042700030120      * servizio che sto trattando
042800030123     c                   if        fretsr <> fgttsr
042900030123     c                   iter
043000030123     c                   endif
043100030123     c                   movel(p)  frecre        tbeke1
043200030204     c                   movel     'CRE'         tbecod
043300030123     c     ktbe          chain     tntbe01l                           90
043400030123     c                   if        not *in90
043500030123     c                   movel     tbeuni        dcre
043600030123     c                   movel(p)  §crvoc        tblkey
043700030123     c     ktab          chain     tabel00f                           90
043800030123     c                   if        not *in90
043900030123     c                   movel     tbluni        ds8d
044000030123     c                   endif
044100030123     c                   endif
044200030123     c                   if        §8dpa = 'P'
044300030123     c                   add       fretim        tfretimp
044400030123     c                   else
044500030123     c                   add       fretim        tfretima
044600030123     c                   endif
044700030120     c                   enddo
044800030120     c                   endsr
044900030109      *-------------------------------------------------
045000030109     c     reptar        begsr
045100030109      *-------------------------------------------------
045200030109     c                   z-add     copdat        data
045300030305     c                   z-add     kcopfil       poges
045400030109     c                   move      'X'           tipopre
045500030109     c                   clear                   dsfgtout
045600030109     c                   clear                   error
045700040830      * reperisce la tariffa valida nella data che sto trattando per il P.O.
045800030109     c
045900030109     c                   call      'FICN36R1'
046000030109     c                   parm                    data
046100030109     c                   parm                    poges
046200030109     c                   parm                    tipopre
046300030109     c                   parm                    dsfgtout
046400030109     c                   parm                    error
046500030109     c                   if        error = '1'
046600030109     c                   clear                   fgtds
046700030901     c                   clear                   apdksc
046800030901     c                   clear                   apdcsf
046900030213     c                   if        lin > 60
047000030213     c                   except    testa
047100030213     c                   endif
047200030109     c                   except    notariffa
047300030109     c                   else
047400030109     c                   movel     dsfgtout      fgtds
047500030207      *reperisce codice fornitore e codice società fornitore
047600030207     c     kapd          chain     fiapd01l                           93
047700080206     c                   if        *in93 or apdksc = 0
047800030207     c                   clear                   apdksc
047900030207     c                   clear                   apdcsf
048000061006      * invia mail segnalazione assenza anagrafica coop
048100061006     c                   exsr      inviamail
048200030207     c                   endif
048300030207     c
048400030109     c                   end
048500030109     c                   endsr
048600061006      *-------------------------------------------------
048700061006     c     inviamail     begsr
048800061006      *-------------------------------------------------
048900061006     C* Inizializzo variabili
049000061006     c                   if        poges <> pomail
049100061006     c                   move      poges         pomail
049200061006     c                   move      poges         pomaila
049300061006     C                   movel     *blanks       wrkEml          253
049400061006     C                   movel     *blanks       wrkMsg         5000
049500061006     C                   movel     *blanks       wrkOgg           44
049600061006     C* Valorizzo i campi della e-m@ail
049700061006     C                   eval      wrkEml= 'Michele.Malipiero@bartolini.it'
049800061006
049900061006     C                   eval      wrkOgg='Valorizzazione COOP ' + pomaila
050000061006     C                   eval      wrkMsg='Attenzione Valorizzazione COOP  ' +
050100061006     C                             'Manca L''anagrafica cooperativa del ' +
050200061006     C                             'P.O. in oggetto.'+':/N'+
050300061006     C                             'Questo può generare dati falsati nel ' +
050400061006     C                             'C.E. GIORNALIERO '+':/N'+
050500061006     C                             'CONTATTARE AL PIU PRESTO IL P.O PER ' +
050600061006     C                             'L''INSERIMENTO.'
050700061006     C                   call(e)   'TIS701C'
050800061006     C                   parm                    wrkEml
050900061006     C                   parm                    wrkOgg
051000061006     C                   parm                    wrkMsg
051100061006     c                   endif
051200061006     c                   endsr
051300020521      *-------------------------------------------------
051400020521     c     *inzsr        begsr
051500020521      *-------------------------------------------------
051600020626     c     *entry        plist
051700020626     c                   parm                    kpjba
051800030108     c                   if        kpjbu = *blank
051900030113     c                   clear                   param
052000030113     c                   movel(p)  param         kpjbu
052100030108     c                   end
052200030108     c                   movel     kpjbu         param
052300020626
052400030108     C***
052500030108     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
052600030108     C***
052700030108     C                   TIME                    W0120            14 0
052800030108     C                   MOVE      W0120         WDAT              8 0
052900030821     C                   MOVE      W0120         Wfiller          14
053000030108     C*
053100030108     C                   Z-ADD     WDAT          G02DAT
053200030108     C                   MOVEL     *BLANK        G02ERR
053300030108     C                   CALL      'XSRDA8'
053400030108     C                   PARM                    WLBDAT
053500030108     C* UDATE A 8 IN AAAA/MM/GG
053600030108     C                   move      G02INV        dataiso
053700030108      *se non sono stati ricevuti dei parametri significa che il lancio è
053800030108      * stato schedulato per cui imposto i parametri di limite elaborazione
053900030110     c                   if        datadal = 0
054000030204      *reperisco limiti da tabella
054100030204     c                   movel(p)  '1'           tbeke1
054200030204     c                   movel     'CCG'         tbecod
054300030204     c     ktbe          chain     tntbe01l                           90
054400030204     c                   if        not *in90
054500030204     c                   movel     tbeuni        dccg
054600030204      *tolgo giorni di tabella da udate
054700030204     c     dataiso       subdur    §cgguv:*d     dataiso
054800030204     c                   move      dataiso       dataal
054900030204     c                   move      dataal        dataiso
055000030204      *tolgo giorni di tabella da data calcolata per primo limite
055100030204     c     dataiso       subdur    §cgglv:*d     dataiso
055200030110     c                   move      dataiso       datadal
055300030204     c                   endif
055400030108     c                   endif
055500030108     c
055600030213     c                   z-add     99            lin
055700030108     c     ksic          klist
055800030109     c                   kfld                    keypo
055900030108     c                   kfld                    datadal
056000020521
056100030109     c     kctt          klist
056200030305     c                   kfld                    kcopfil
056300030305     c                   kfld                    copdat
056400030109
056500030109     c     kctd          klist
056600030305     c                   kfld                    kcopfil
056700030305     c                   kfld                    copdat
056800030109     c                   kfld                    copvoc
056900030109
057000030109     c     kfpt          klist
057100030109     c                   kfld                    fgtpdr
057200030109     c                   kfld                    fgtsml
057300030109     c                   kfld                    fgttsr
057400030109     c                   kfld                    fgtctr
057500030109     c                   kfld                    fgtprg
057600030109     c                   kfld                    copvoca
057700030109
057800030120     c     kfre          klist
057900030120     c                   kfld                    fgtpdr
058000030120     c                   kfld                    excopdat
058100030120
058200030123     c     ktbe          klist
058300030123     c                   kfld                    tbecod
058400030123     c                   kfld                    tbeke1
058500030123
058600030123     c     ktab          klist
058700030123     c                   kfld                    tblkut
058800030123     c                   kfld                    tblcod
058900030123     c                   kfld                    tblkey
059000030123     c                   z-add     1             tblkut
059100030519     c     ktabc         klist
059200030519     c                   kfld                    tblkut
059300030519     c                   kfld                    tblcod
059400030123
059500030207     c     kapd          klist
059600030207     c                   kfld                    tipo
059700030207     c                   kfld                    fgtpdr
059800030207     c                   move      'C'           tipo              1
059900030305
060000030305     c     kcae          klist
060100030305     c                   kfld                    tipo
060200030305     c                   kfld                    copfil
060300030305     c                   kfld                    kcaetfp
060400030305     c                   kfld                    copdat
060500030305     c                   z-add     0             kcaetfp           3 0
060600030207
060700030305     c     kcaep         klist
060800030305     c                   kfld                    tipo
060900030305     c                   kfld                    copfil
061000030305     c                   kfld                    kcaetfp
061100030523     c* carico le voci delle competenze da considerare nella lettura del sicop
061200030519     c                   move      '8A'          tblcod
061300030519     c     ktabc         setll     tabel00f
061400030519     c                   do        *hival
061500030519     c     ktabc         reade     tabel00f
061600030519     c                   if        %eof(tabel00f)
061700030519     c                   leave
061800030519     c                   end
061900030519     c                   movel     tbluni        ds8a
062000030523     c                   if        §8ausc = 'X' or §8ausc = ' '
062100030519     c                   add       1             x
062200030519     c                   movel     tblkey        voc(x)
062300030519     c                   end
062400090903     c                   if        §8aobb = 'S' and §8afac = 'C'
062500090903     c                   add       1             xo
062600090903     c                   movel     tblkey        voco(xo)
062700090903     c                   movel     §8avocr       vocor(xo)
062800090903     c                   end
062900030519     c                   enddo
063000030519     c                   move      '8D'          tblcod
063100020521     c                   endsr
063200020517     O*------------------------------------------------------*
063300020517     OQSYSPRT   e            TESTA             2
063400020517     O                                           70 'Anomalie riscontrate fluss-
063500030109     O                                              o valorizzazione Coop'
063600030109     O          e            notariffa   1
063700030109     o                                              'Non è stata trovata la -
063800030109     o                                               tariffa valida in data'
063900030109     o                       copdat             + 2 '    /  /  '
064000030109     o                                          + 2 'Filiale'
064100030305     o                       kcopfil            + 2
