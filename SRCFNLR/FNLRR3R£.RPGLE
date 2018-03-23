000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400041025     Ffiqdt04l  if   e           k disk
000500041025     Ffiqdc01l  if   e           k disk
000600041027     Ffiqdo01l  if   e           k disk
000700041027     Ffiqbt01l  if   e           k disk
000800041027     Ffiqbd01l  if   e           k disk
000900021203     Ffiapd01l  if   e           k disk
001000030723     Fazorg01l  if   e           k disk
001100041027     Ftntbe01l  if   e           k disk
001200041026     Ftabel00f  if   e           k disk
001300041025     Fqsysprt   O    F  132        PRINTER prtctl(prtds)
001400041026
001500041026     d kpjba         e ds
001501110301     d dqdtflo       e ds
001600041026     d ds2a          e ds
001700041027     d dcmr          e ds
001800090609     d ds5e          e ds
001900030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002000030505     D CNCR80        E DS
002100041026
002200030723     D PARAM           DS
002300030723     d  data1                         8  0
002400030723     d  data2                         8  0
002500030723     d  aut1                          7  0
002600030723     d  aut2                          7  0
002700030723     d  prosp                         1
002800020404     d prtds           ds
002900020404     d   spab                         3  0
003000020404     d   spaa                         3  0
003100020404     d   skab                         3  0
003200020404     d   skaa                         3  0
003300020404     d   line                         3  0
003400050119
003500050119     d wfnbaf          s                   like(qdtfnbaf) inz('S')
003600050119     d wfnbri          s                   like(qdtfnbri) inz('S')
003700050119     d wfncrit         s                   like(qdtfncrit) inz('S')
003800050119
003900041026     d exqdtpdr        s                   like(qdtpdr)
004000030724     d primo           s              1
004100041028     d dataiso         s               d   datfmt(*iso)
004200041028     d dataeur         s               d   datfmt(*eur)
004300041028      *
004400041028      * colli non spuntati al rientro da consegna
004500050520     d i               s              4  0
004600041028     d h               s              1  0
004700050520     d sksp8           s                   like(nosp8) dim(9999)
004800041028     d skst8           s                   like(stsp8) dim(5)
004900041028     d nosp8w          s                   like(nosp8)
005000041028     d skst8w          s                   like(stsp8)
005100041028     d nosp8           ds
005200041028     d  aasds                         4s 0
005300041028     d  lnpds                         3s 0
005400041028     d  nspds                         7s 0
005500041028     d  flsds                         3s 0
005600041103     d  nrsds                         2s 0
005700041028     d  nscds                         7s 0
005800041028     d  lnads                         3s 0
005900041028     d  stsp8                        15s 0 overlay(nosp8:15)
006000041028     d
006100000000     C*---------------------------------------------------------------*
006200020304      *
006300041025     c                   exsr      fiqdt
006400050420
006500050420      ** Stampa la legenda
006600050420     c                   except    legend
006700020308     c                   seton                                        lr
006800990113     c*-------------------------------------------------
006900041025     c     fiqdt         begsr
007000990113     c*-------------------------------------------------
007100990113     c                   setoff                                           99
007200041025     c     kqdt          setll     fiqdt04l
007300981029     c                   do        *hival
007400041026     c     filges        reade     fiqdt04l                               99
007500981029     c   99              leave
007600030724      * elimino i record che non rientrano nei parametri richiesti
007700041025     c                   if        qdtddc >= data1 and qdtddc <= data2 and
007800041025     c                             qdtpdr >= aut1  and qdtpdr <= aut2
007900041027
008000041027      *primo giro forzo codice padroncino per testare rottura calcoli totale
008100041027     c                   if        primo  = *blank
008200041027     c                   z-add     qdtpdr        expdr             7 0
008300041027     c                   z-add     qdtddc        exddc             8 0
008400041027     c                   move      'X'           primo             1
008500041027     c                   endif
008600041027
008700041027      * prepara dati per colli non bollettati e bancali a rottura autista
008800041027      * o data prestazione
008900041027
009000041027     c                   if        expdr <> qdtpdr or qdtddc <> exddc
009100050420      **
009200041027     c                   exsr      Col_noboT
009300050420      **
009400050119      * testo se nella totalizzazione ho trovato delle distinte con
009500050119      * bancali affidati o rientrati non dichiarati nel caso stampo i
009600050119      * flag e non stampo i totali
009700110301     c*                  if        wfnbaf <> 'S' or wfnbri <> 'S'
009701110301     c                   if        wfnbri <> 'S'
009800050119     c                   clear                   banrie
010000050119     c                   else
010100110304     c     banrie        sub       banbdar       bandiff
010200050421     c****               clear                   wfnbaf
010300050421     c****               clear                   wfnbri
010400050119     c                   endif
010500050119      * stessa cosa di cui sopra per i colli ritirati
010600050119     c                   if        wfncrit <> 'S'
010700050119     c                   clear                   colrit
010800050119     c                   clear                   totcobo
010900050119     c                   else
011000041027     c     colrit        sub       totcobo       coldiff
011100050420
011200050420      **  Segnala con (*) se a fronte di una differenza
011300050420      **    vi è una differenza anche fra gli spuntati e i bollettati
011400050420     c                   if        coldiff <> 0 and
011500050420     c                             totcospn   <> totcobo
011600050420     c                   move      '*'           fcoldif
011700050420     c                   end
011800050420
011900050421     c****               clear                   wfncrit
012000050119     c                   endif
012100050420      **
012200041027     c                   except    Tcoban
012300050420      **
012400041027     c* azzero i dati per il totale  autista/data relativi ai BANCALI
012500110304     c                   clear                   banbdar
012600041027     c                   clear                   banrie
012700041027     c                   clear                   bandiff           5 0
012800041027     c                   clear                   colrit
012900041027     c                   clear                   coldiff           5 0
013000050420     c                   clear                   Fcoldif           1
013100050119     c                   move      'S'           wfnbaf
013200050119     c                   move      'S'           wfnbri
013300050119     c                   move      'S'           wfncrit
013400041027
013500041027      * imposta dati nuovo autista o data dopo totali
013600041027     c                   z-add     qdtpdr        expdr
013700041027     c                   z-add     qdtddc        exddc
013800041027
013900041027     c                   endif
014000041027      * forzo rottura per ogni distinta per stampare una distinta a pagina
014100041027     c                   z-add     99            line
014200030724     c
014300021203     c     kapdf         chain     fiapd01l                           97
014400030724      *se l'anagrafica non corrisponde non stampo la decodifica
014500030724     c*  97              iter
014600030723     c   97              clear                   despdr           25
014700030723     c  n97              movel     apdrsc        despdr
014800041027     c* totalizzo i dati per distinta per il totale finale autista/data
014901110301     c                   move      qdtflo        dqdtflo
014902110304     c                   clear                   banbdar           5 0
014903110301     c                   if        §QDTNBDAR <> *blank
014904110304     c                   move      §qdtnbdar     banbdar
014905110301     c                   endif
015000041027     c                   add       qdtnbnri      banrie            5 0
015100050210     c                   add       qdtnbnbr      banrie
015200041027     c                   add       qdtnclrit     colrit            5 0
015300050119      *testo i flag di iserimento dei dati riepilogativi x bancali
015400050119      * e colli affidati e rientrati se qualcuno non è dichiarato
015500050119      * in fase di stampa azzererò i numeri e stamperò i flag
015600050119     c                   if        qdtfnbaf <> 'S'
015700050119     c                   move      qdtfnbaf      wfnbaf
015800050119     c                   endif
015900050119     c                   if        qdtfnbri <> 'S'
016000050119     c                   move      qdtfnbri      wfnbri
016100050119     c                   endif
016200050119     c                   if        qdtfncrit <> 'S'
016300050119     c                   move      qdtfncrit     wfncrit
016400050119     c                   endif
016500020404
016600041027      *data in stampa
016700041025     c                   move      qdtddc        dataiso
016800041025     c                   move      dataiso       dataeur
016900041025     c                   move      dataeur       qdtddcs           8 0
017000041025
017100020404     c                   if        line > 60
017200020404     c                   except    testata
017300020404     c                   endif
017400041026      * calcola dettaglio spedizioni non consegnate raggruppate per
017500041026      * tipo mancata consegna
017600041026
017700041026     c                   clear                   totlav            5 0
017800041026     c                   clear                   totnof            5 0
017900041026     c                   clear                   totgia            5 0
018000041124     c                   clear                   totesp            5 0
018100041026     c                   exsr      NoN_Cons
018200041025     c                   except    detta
018300041103      * se richiesto prospetto con dettaglio colli non consegnati e non
018400041103      * spuntati al rientro
018500041103     c                   if        prosp = 'S'
018600041103      * verifico se esiste un dettaglio
018700041103     c                   if        sksp8(1) <> nosp8w
018800041103     c                   move      sksp8(1)      nosp8
018900041103     c                   exsr      spunte8
019000041103     c                   endif
019100041103     c                   endif
019200041103      * consegnati e non incassati
019300041103     c                   except    dettni
019400041027
019500041103      * Testata orm affidati no ritirati
019600041103     c                   except    Torm
019700041103      * prepara dati per ORM non eseguiti
019800041103     c                   except    Tormne
019900041027     c                   exsr      ORM_noes
020000041027
020100041027      * prepara dati per ORM eseguiti non bollettati
020200041029     c                   except    Tormnb
020300041027     c                   exsr      ORM_nobo
020400041027
020500020404
020600020114     c                   end
020700020404
020800020304     c                   enddo
020900041027      * ultimo totale x fine file
021000041027     c                   exsr      Col_noboT
021100050119      * testo se nella totalizzazione ho trovato delle distinte con
021200050119      * bancali affidati o rientrati non dichiarati nel caso stampo i
021300050119      * flag e non stampo i totali
021400110301     c*                  if        wfnbaf <> 'S' or wfnbri <> 'S'
021401110301     c                   if        wfnbri <> 'S'
021500050119     c                   clear                   banrie
021600110304     c                   clear                   banbdar
021700050119     c                   else
021800110304     c     banrie        sub       banbdar       bandiff
021900050421     c*****              clear                   wfnbaf
022000050421     c*****              clear                   wfnbri
022100050119     c                   endif
022200050119      * stessa cosa di cui sopra per i colli ritirati
022300050119     c                   if        wfncrit <> 'S'
022400050119     c                   clear                   colrit
022500050119     c                   clear                   totcobo
022600050119     c                   else
022700041027     c     colrit        sub       totcobo       coldiff
022800050420
022900050420      **  Segnala con (*) se a fronte di una differenza
023000050420      **    vi è una differenza anche fra gli spuntati e i bollettati
023100050420     c                   if        coldiff <> 0 and
023200050420     c                             totcospn   <> totcobo
023300050420     c                   move      '*'           fcoldif
023400050420     c                   end
023500050420
023600050421     c*****              clear                   wfncrit
023700050119     c                   endif
023800050119
023900041027     c                   except    Tcoban
024000020304
024100020304     c                   endsr
024200981029      *
024300041026     c*-------------------------------------------------
024400041026     c     Non_cons      begsr
024500041026     c*-------------------------------------------------
024600041028     c                   clear                   i
024700041028     c                   clear                   sksp8
024800041027     c     kdet          setll     fiqdc01l
024900041026     c                   do        *hival
025000041027     c     kdet          reade     fiqdc01l                               98
025100041026     c   98              leave
025200041026      * considero solo le testatine o meglio le righe che contengono
025300041026      * il codice evento riferito alla bolla gli altri rekord sono
025400041026      * il dettaglio dei colli non spuntati
025500041028      * questi ultimi devono essere skierati in modo da essere stampati
025600041028      * se viene chiesto il dettaglio dei colli non consegnati senza
025700041028      * spunta di rientro
025800041028      *
025900041026     c                   if        qdccev = *blank
026000041028     c                   z-add     QDCAAS        aasds
026100041028     c                   z-add     QDCLNP        lnpds
026200041028     c                   z-add     QDCNRS        nrsds
026300041028     c                   z-add     QDCNSP        nspds
026400041028     c                   z-add     QDCFLS        flsds
026500041028     c                   z-add     QDCNSC        nscds
026600041028     c                   z-add     QDCLNA        lnads
026700041028     c                   add       1             i
026800041028     c                   move      nosp8         sksp8(i)
026900041028
027000041026     c                   iter
027100041026     c                   endif
027200041026      * verifico sulla tabella eventi la tipologia per totalizzare
027300041026      * le diverse motivazioni di non consegna
027400041026     c                   clear                   ds2a
027500041026     c                   move      '2A'          kcod
027600041026     c                   movel(p)  qdccev        kkey
027700041026     c     ktab          chain     tabel00f                           96
027800041026     c                   if        not *in96
027900041026     c                   movel     tbluni        ds2a
028000041026     c                   select
028100041026      * lasciati avvisi
028200041026     c                   when      §2aftc = 'A'
028300041026     c                   add       1             totlav
028400041026
028500041026      * Non fatte
028600041026     c                   when      §2aftc = 'N'
028700041026     c                   add       1             totnof
028800091008      * espressi o  h10:30
028900091008     c                   if        qdctsp = 'E' or qdctsp = 'H'
029000041124     c                   add       1             totesp            5 0
029100041124     c                   endif
029200041026
029300041026      * Giacenze
029400041026     c                   when      §2aftc = 'G'
029500041026     c                   add       1             totgia
029600041026
029700041026     c                   endsl
029800041026     c                   endif
029900041026     c                   enddo
030000041026     c                   endsr
030100041027     c*-------------------------------------------------
030200041027     c     ORM_noes      begsr
030300041027     c*-------------------------------------------------
030400041027     c     kdet          setll     fiqdo01l
030500041027     c                   do        *hival
030600041027     c     kdet          reade     fiqdo01l                               98
030700041027     c   98              leave
030800041027      * considero solo le testatine o meglio le righe che contengono
030900041027      * il codice evento riferito alla bolla gli altri rekord sono
031000041027      * il dettaglio dei colli non spuntati
031100041027     c                   if        qdocar = *blank
031200041027     c                   iter
031300041027     c                   endif
031400041027      * reperisco dalla tabella la decodifica della causale x non eseguito
031500041027     c                   clear                   dcmr
031600041027     c                   move      'CMR'         tbecod
031700041027     c                   movel(p)  qdocar        tbeke1
031800041027     c     ktbe          chain     tntbe01l                           96
031900041027     c                   if        not *in96
032000041027     c                   movel     tbeuni        dcmr
032100041027     c                   endif
032200041027      * stampa riga di dettaglio
032300041027     c                   except    Dormne
032400041027
032500041027     c                   enddo
032600041027     c                   endsr
032700041027     c*-------------------------------------------------
032800041027     c     ORM_nobo      begsr
032900041027     c*-------------------------------------------------
033000041027     c     kdet          setll     fiqdo01l
033100041027     c                   do        *hival
033200041027     c     kdet          reade     fiqdo01l                               98
033300041027     c   98              leave
033400041027      * considero solo le testatine o meglio le righe che contengono
033500041027      * il codice evento riferito alla bolla gli altri rekord sono
033600041027      * il dettaglio dei colli non spuntati
033700041027     c                   if        qdocar <> *blank
033800041027     c                   iter
033900041027     c                   endif
034000041027      * stampa riga di dettaglio
034100041027     c                   except    Dormnb
034200041027
034300041027     c                   enddo
034400041027     c                   endsr
034500041027     c*-------------------------------------------------
034600041027     c     Col_noboT     begsr
034700041027     c*-------------------------------------------------
034800041027     c                   clear                   totcobo           5 0
034900050420     c                   clear                   totcospn          5 0
035000050420     c                   clear                   fcospn            1
035100041027     c     kqb           setll     fiqbT01l
035200041027     c                   do        *hival
035300041027     c     kqb           reade     fiqbT01l                               98
035400041027     c   98              leave
035500041027     c                   if        qbtatb = ' '
035600041027     c                   add       qbtnclbo      totcobo
035700050420     c                   add       qbtnclsp      totcospn
035800050420      * se squadrati memorizza il flag
035900050420     c                   if        fcospn = *blank
036000050420     c                   move      qbtfquad      fcospn
036100050420     c                   end
036200041027     c                   endif
036300041027
036400041027     c                   enddo
036500041027      * dettaglio non spuntati
036600041027     c                   exsr      Col_nobo
036700041027
036800041027     c                   endsr
036900041027     c*-------------------------------------------------
037000041027     c     Col_nobo      begsr
037100041027     c*-------------------------------------------------
037200041027     c                   clear                   totnosp           5 0
037300041027     c     kqb           setll     fiqbd01l
037400041027     c                   do        *hival
037500041027     c     kqb           reade     fiqbd01l                               98
037600041027     c   98              leave
037700041027     c                   add       1             totnosp
037800041027
037900041027     c                   enddo
038000041027     c                   endsr
038100041028     c*-------------------------------------------------
038200041028     c     spunte8       begsr
038300041028     c*-------------------------------------------------
038400041028      * preparo skiera da mettere in stampa per segnacolli non spuntati
038500041028      * massimo 5 segnacolli per spedizione
038600041028     c                   z-add     1             h
038700050520     c                   do        9999          i
038800041028      * verifico rottura di spedizione con la precedente
038900041028     c                   if        %subst(sksp8(i):1:16) <> %subst(nosp8:1:16)
039000041028      * stampo il dettaglio a rottura di spedizione
039100041028     c                   except    detta1
039200041028     c                   z-add     1             h
039300041028     c                   clear                   skst8
039400041028      * se l'elemento non è valorizzato esco dal loop
039500041028     c                   if        sksp8(i) = nosp8w
039600041028     c                   leave
039700041028     c                   else
039800041028      * altrimenti imposto valori per primo elemento nuova spedizione
039900041028     c                   move      sksp8(i)      nosp8
040000041028     c     skst8w        lookup    skst8(h)                               80
040100041028     c   80              move      stsp8         skst8(h)
040200041028     c                   endif
040300041028     c                   else
040400041028      * incasello gli eventuali 5 segnacolli
040500041028     c                   if        h < 5
040600041028     c                   move      sksp8(i)      nosp8
040700041028     c     skst8w        lookup    skst8(h)                               80
040800041028     c   80              move      stsp8         skst8(h)
040900041028     c                   endif
041000041028     c                   endif
041100041028     c                   enddo
041200041028     c                   endsr
041300011026     c*-------------------------------------------------
041400011026     c     *inzsr        begsr
041500011026     c*-------------------------------------------------
041600020304     c     *entry        plist
041700020304     c                   parm                    kpjba
041800030723     c                   movel     kpjbu         param
041900031114
042000041025     c                   movel     aut1          filges            3 0
042100030723     c                   move      data1         dataiso
042200030723     c                   move      dataiso       dataeur
042300030723     c                   move      dataeur       datadal           8 0
042400030723     c                   move      data2         dataiso
042500030723     c                   move      dataiso       dataeur
042600030723     c                   move      dataeur       dataal            8 0
042700041025     c     filges        chain     azorg01l
042800041025     c                   movel     orgdes        desfil           20
042900011026
043000020218     C                   time                    w0120            14 0
043100020218     C                   move      w0120         wdat              8 0
043200041103     C                   movel     w0120         wora              6 0
043300020404     c                   z-add     99            line
043400041026     C     *LIKE         DEFINE    TBLKUT        KKUT
043500041026     C     *LIKE         DEFINE    TBLCOD        KCOD
043600041026     C     *LIKE         DEFINE    TBLKEY        KKEY
043700041026     C*
043800041026     C     kqdt          klist
043900041026     C                   kfld                    filges
044000041026     C                   kfld                    data1
044100041026     C*
044200041027     C     kdet          klist
044300041026     C                   kfld                    qdtfgs
044400041026     C                   kfld                    qdtnfv
044500041026     C                   kfld                    qdtddc
044600041026     C                   kfld                    qdtpdr
044700041026     C*
044800041026     C     ktab          klist
044900041026     C                   kfld                    kkut
045000041026     C                   kfld                    kcod
045100041026     C                   kfld                    kkey
045200041026     C                   z-add     1             kkut
045300041027     C*
045400041027     C     ktbe          klist
045500041027     C                   kfld                    tbecod
045600041027     C                   kfld                    tbeke1
045700041027     C*
045800041027     C     kqb           klist
045900041027     C                   kfld                    qdtfgs
046000041027     C                   kfld                    exddc
046100041027     C                   kfld                    expdr
046200041026
046300021203     c     kapdf         klist
046400030723     c                   kfld                    tipo              1
046500041026     c                   kfld                    qdtpdr
046600041025     c                   move      'A'           tipo
046700030725
046800030505     C                   Z-ADD     1             CODUT
046900030505     C                   CALL      'X§PARUT'
047000030505     C                   PARM                    UT§DSE
047100030505     C                   MOVEL     RAGUT         RSUT             20
047200030505     C                   MOVEL     REC80         CNCR80
047300041103     c                   move      *all'_'       finkao          100
047400041103     c                   move      *all'_'       finkbo           80
047500090609      * reperimento descrizione tipo servizio 'E' x stampa
047600090609     c                   clear                   ds5e
047700090609     c                   clear                   desserv          15
047800090609     c                   move      '5E'          kcod
047900090609     c                   movel(p)  'E'           kkey
048000090609     c     ktab          chain     tabel00f                           96
048100090609     c                   if        not *in96
048200090609     c                   movel     tbluni        ds5e
048300090609     c                   eval      desserv = §5ED05 + ' non fatti'
048400090609     c                   endif
048500011026     c                   endsr
048600020404     C*--------------------------------------------------------------------
048700041026     Oqsysprt   E            testata          02
048800041103     o                       rsut                21
048900050204     o                                           72 'D E T T A G L I O    -
049000050204     o                                              C O N T R O L L O'
049100050204     O                                           89 'Del'
049200050204     o                       qdtddcs            100 '  /  /    '
049300050420     o                                          120 'FNLRR3R'
049400050119     o                                          126 'Pag.'
049500050119     o                       page               132
049600041103     O          e            testata     1
049700041103     o                       knsif               11
049800041103     o                       knmus               22
049900050204     o                                           70 'A U T O -
050000050204     o                                              T R A S P O R T A T O R I'
050100050420     o                       udate         y    120
050200050420     o                       wora               132 '  :  :  '
050300041026     O          e            testata     2
050400041026     o                                            6 'P.O.'
050500041026     o                       qdtfgs              10
050600041026     o                       desfil              32
050700041103     o                                           50 'Autotrasportatore'
050800041103     o                       qdtpdr              59
050900041103     o                       despdr              85
051000041103     O          e            testata     0
051100041103     o                       despdr              85
051200041103     O          e            testata     0
051300041103     o                       despdr              85
051400041026     O          e            testata     2
051500041026     o                                           60 'CONSEGNE'
051600041026     O          e            testata     0
051700041026     o                                           60 'CONSEGNE'
051800041026     O          e            testata     0
051900041026     o                                           60 'CONSEGNE'
052000041103     O          e            testata     1
052100041103     o                       finkao             101
052200041026     O          e            testata     2
052300041026     o                                           10 'Distinta  '
052400041026     o                                           35 'Bolle     '
052500041026     o                                           45 'Colli     '
052600041026     o                                           55 'Peso      '
052700041026     o                                           65 'Volume    '
052800041026     o                                           80 'C/Assegni '
052900041026     o                                           95 'Assegnati '
053000041026     O          e            testata     2
053100041026     o                       qdtnfv        z      7
053200041026     o                                           18 'Affidati  '
053300041026     o                       qdtnblaf      z     30
053400041026     o                       qdtnclaf      z     40
053500041026     o                       qdtpkfaf      2     52
053600041026     o                       qdtvlfaf      2     61
053700041026     o                       qdtcasaf      2     79
053800041026     o                       qdtiftaf      2     96
053900041026     O          e            testata     1
054000041026     o                                           22 'Non consegnate'
054100041026     o                       qdtnblnc      z     30
054200041026     o                       qdtnclnc      z     40
054300041026     o                       qdtpkfnc      2     52
054400041026     o                       qdtvlfnc      2     61
054500041026     o                       qdtcasnc      2     79
054600041026     o                       qdtiftnc      2     96
054700041103     O          e            testata     1
054800041103     o                       finkao             101
054900041103     O          e            detta       3
055000041103     o                                           22 'Consegne non eseguite'
055100041103     o                                         +  2 'Non fatte'
055200041103     o                       totnof        z   +  2
055300041103     o                                         +  2 'Lasciato avviso'
055400041103     o                       totlav        z   +  2
055500041103     o                                         +  2 'Giacenze'
055600041103     o                       totgia        z   +  2
055700090609     o*                                        +  2 'Espr.non fatti'
055800090609     o                       desserv           +  2
055900041124     o                       totesp        z   +  2
056000041103     O          e            detta       0
056100041103     o                                           22 'Consegne non eseguite'
056200041103     O          e            detta       0
056300041103     o                                           22 'Consegne non eseguite'
056400041103     O          e            detta       2
056500041103     o                                           47 'Colli non consegnati e -
056600041103     o                                              non spuntati al rientro'
056700041103     o                       qdtnclns      z     55
056800041103     O          e            detta       0
056900041103     o                                           47 'Colli non consegnati e -
057000041103     o                                              non spuntati al rientro'
057100041103     O          e            detta       0
057200041103     o                                           47 'Colli non consegnati e -
057300041103     o                                              non spuntati al rientro'
057400041103      * Elenco spedizioni con colli senza spunta di rientro
057500041103     O          e            detta1      1
057600041103     o                       aasds                5
057700041103     o                       lnpds                9
057800041103     o                       nrsds               12
057900041103     o                       nspds               21
058000041103     o                       skst8(1)          +  2 '   -  -       -   '
058100041103     o                       skst8(2)          +  2 '   -  -       -   '
058200041103     o                       skst8(3)          +  2 '   -  -       -   '
058300041103     o                       skst8(4)          +  2 '   -  -       -   '
058400041103     o                       skst8(5)          +  2 '   -  -       -   '
058500041103     O          e            dettni      2
058600041026     o                                           27 'Consegnati e non incassati'
058700041026     o                       qdtcasnf      2     79
058800041026     o                       qdtiftnf      2     96
058900041103     O          e            dettni      0
059000041026     o                                           27 'Consegnati e non incassati'
059100041103     O          e            dettni      0
059200041026     o                                           27 'Consegnati e non incassati'
059300041027      * ORM non eseguiti elenco
059400041103     o          e            Torm        3
059500041103     o                       finkao             101
059600041103     o          e            Torm        1
059700041103     o                                            4 'ORM'
059800041104     o                                           22 'Numero'
059900041103     o          e            Torm        0
060000041103     o                                            4 'ORM'
060100041104     o                                           22 'Numero'
060200041103     o          e            Torm        0
060300041103     o                                            4 'ORM'
060400041104     o                                           22 'Numero'
060500041103     o          e            Torm        1
060600041103     o                                           13 'Affidati    '
060700041104     o                       qdtormaf      z    + 4
060800041103     o          e            Torm        1
060900041103     o                                           13 'Non Ritirati'
061000041104     o                       qdtormnf      z    + 4
061100041103     o          e            Torm        1
061200041103     o                       finkao             101
061300041103      *---------------------------------------------------
061400041027     O          e            Tormne      3
061500041027     o                                           17 'ORM non eseguiti'
061600041027     O          e            Tormne      0
061700041027     o                                           17 'ORM non eseguiti'
061800041027     O          e            Tormne      0
061900041027     o                                           17 'ORM non eseguiti'
062000041027     O          e            Tormne      1
062100041027     o                                            8 'NR. ORM'
062200041027     o                                           40 'Causale mancato ritiro'
062300041027     O          e            Dormne      1
062400041027     o                       qdopoe               4
062500041027     o                       qdonsr               7
062600041027     o                       qdonor              15
062700041027     o                       qdonrv              18
062800041027     o                       qdocar              22
062900041027     o                       d§cmrdes          +  1
063000041027
063100041027      * ORM  eseguiti non bollettati
063200041104     O          e            Tormnb      3
063300041027     o                                           28 'ORM eseguiti non -
063400041027     o                                              bollettati'
063500041027     O          e            Tormnb      0
063600041027     o                                           28 'ORM eseguiti non -
063700041027     o                                              bollettati'
063800041027     O          e            Tormnb      0
063900041027     o                                           28 'ORM eseguiti non -
064000041027     o                                              bollettati'
064100041027     O          e            Dormnb      1
064200041027     o                       qdopoe               4
064300041027     o                       qdonsr               7
064400041027     o                       qdonor              15
064500041027     o                       qdonrv              18
064600041027      * Colli e bancali
064700041104     O          e            Tcoban      2
064800041104     o                       finkao             101
064900041103     O          e            Tcoban      1
065000041027     o                                           22 'Colli'
065100050420     o                                           80 'Bancali'
065200041027     O          e            Tcoban      0
065300041027     o                                           22 'Colli'
065400050420     o                                           80 'Bancali'
065500041027     O          e            Tcoban      0
065600041027     o                                           22 'Colli'
065700050420     o                                           80 'Bancali'
065800041027     O          e            Tcoban      1
065900041027     o                                           10 'Rit.dich.'
066000041027     o                                           22 'Bollettati'
066100041027     o                                           33 'Differenza'
066200050420     o                                           44 'Spuntati'
066300050420      *
066400050420     o                                           70 'Rientrati'
066500110311     o                                           81 'Da Rendere'
066600050420     o                                           93 'Differenza'
066700041027     O          e            Tcoban      1
066800041027     o                       colrit        z      8
066900050421     o                       wfncrit             10
067000041027     o                       totcobo       z     18
067100041104     o                       coldiff       k     30
067200050420     o                       Fcoldif             32
067300050420     o                       totcospn      z     42
067400050420     o                       fcospn              44
067500050421     o                       banrie        z     67
067600050421     o                       wfnbri              69
067700110304     o                       banbdar       z     78
067800110301     o*                      wfnbaf              80
067900050420     o                       bandiff       k     90
068000041103     O          e            Tcoban      1
068100041104     o                       finkao             101
068200041103
068300050420      *****
068400050420     O          e            Legend      2
068500050420     o                       finkao             101
068600050420     O          e            Legend      1
068700050420     o                                            1 ' '
068800050420     o                                          + 0 '* = La differenza fra coll'
068900050420     o                                          + 0 'i ritirati e bollettati è '
069000050420     o                                          + 0 'confermata anche dai colli'
069100050421     o                                          + 0 ' spuntati. '
069200050420     O          e            Legend      1
069300050420     o                                            1 ' '
069400050420     o                                          + 0 '§ = Il nr.dei colli spunta'
069500050420     o                                          + 0 'ti è inesatto per la prese'
069600050421     o                                          + 0 'nza di "Bancali > 20 c'
069700050420     o                                          + 0 'olli" spuntati '
069800050420     o                                          + 0 'parzialmente'
