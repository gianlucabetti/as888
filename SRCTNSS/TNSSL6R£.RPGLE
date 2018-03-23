000100980521      ****************************************************************
000200980521      *  RIEPILOGO INDICATORI
000300981008      ****************************************************************
000400990602      * 01 - ON=pgm lanciato da filiale; OFF=lanciato da sede
000500990603      * 10 - condizionamento in stampa
000600990603      * 20 - condizionamento in stampa
000700990317      ****************************************************************
000800980521     H DECEDIT('0,') DATEDIT(*DMY.)
000900980623
001000990602      *------------------------------------------------------------------------*
001200120912     f***FNblp43L  IF   E           K DISK    usropn
001300120912     f***tigcp01l  IF   E           K DISK    usropn
001400990819     ffnevb01l  IF   E           K DISK
001500990819     ftitas38c  IF   E           K DISK    usropn
001501120912     ftitas39c  IF   E           K DISK    extfile(wlibsede) usropn
001502120912     f                                     rename(titas000:titas390)
001503120912     f                                     rename(titas010:titas391)
001504120912     f                                     rename(titasp00:titas39p)
001600990602     ftabel00f  IF   E           K DISK
001700990602     ftnssl6p   o    E             printer
001800980521      *------------------------------------------------------------------------*
001900980702
002000990602      * Definizione campi di work ------------------------------------------
002100990603     D wtibs69r        s              1
002200990601     d wdtspe          s              8  0
002300990601     d kcod            s                   like(tblcod)
002400990601     d kkey            s                   like(tblkey)
002500990602     d regsav          s                   like(reg)
002600990602     d wctcor          s                   like(§ctcor)
002700020913     d depkpjbu        s                   like(kpjbu)
002800990616     d $righi          s              2  0 inz(00)
002801120912     D Wlibsede        S             21
002802120912     D WlibsedeP       S             21    inz('GAITRAGRPS/TITAS39C')
002803120912     D WlibsedeB       S             21    INZ('GAITRAGRU /TITAS39C')
002900990602      * Schiere ------------------------------------------------------------
003000990609     d snrc            s              3  0 dim(6) ctdata perrcd(6)
003100080903     d c2a             s              3    dim(200)
003200990608     d cbo             s              2    dim(50)
003300990831     d tbl             s              2    dim(50)
003400990608     d cre             s              2    dim(100)
003500990603     d dre             s             20    dim(100)
003600020213     d rbok            s              7  0 dim(8)
003700990603     d rsok            s              7  0 dim(5)
003800020213     d rbnk            s              7  0 dim(8)
003900990609     d rsnk            s              7  0 dim(5)
004000020213     d gbok            s              7  0 dim(8)
004100990603     d gsok            s              7  0 dim(5)
004200020213     d gbnk            s              7  0 dim(8)
004300990609     d gsnk            s              7  0 dim(5)
004400990602      * rpord contiene i codici di ordinamento regione/provincia
004500990604     d rpord           s              3    dim(200) inz('999')
004600990602      * rpords contiene i codici di ordinam.regione/provincia ordinati
004700990603     d rpords          s              3    dim(200)
004800990601
004900990602      * DS esterne ---------------------------------------------------------
005000990603     d TIBS69DS      E DS
005100990603     d DS_cnaco      E DS                  extname(CNACO00F)
005200990603     d DS_cnind      E DS                  extname(CNIND00F)
005300990603     d DS_cnclp      E DS                  extname(CNCLP00F)
005400990603     d DS_fncls      E DS                  extname(FNCLS00F)
005500990602     D ds14          E DS
005600990602     D dsct          E DS
005700990608     D ds2a          E DS
005800990608     D ds3a          E DS
005900990831     D dstb          E DS
006000990602
006100990602     D tnssl5ds      E DS
006200990603     d ksc                            7  0 dim(10) overlay(tnssl5ds:22)
006300990602
006400990603     D UT§DSE        E DS                  extname(ut§dse0f)
006500990602     D KPJBA         E DS
006600990602      * DS interne ---------------------------------------------------------
006700990602
006800990601     D DStot           ds                  occurs(200)
006900990602     d  reg                           2
007000990602     d  prv                           2
007100990602     d  dprv                         20
007200990601     d  sok                           5  0 dim(5)
007300990601     d  snk                           5  0 dim(5)
007400020213     d  bok                           5  0 dim(8)
007500020213     d  bnk                           5  0 dim(8)
007600980525      *
007700990601     d wdat8           ds
007800990601     d  datada                        8  0
007900990601     d  dataa                         8  0
008000990601     d  ggl                           5  0
008100990601
008200980521     D WLBDAT          DS                  INZ
008300980521     D  G02DAT                 1      8  0
008400980521     D  G02INV                 9     16  0
008500980521     D  G02ERR                17     17
008600980521     D  G02TGI                18     22  0
008700980525      *
008800990601
008900990603     d  dtini          ds             8
009000990601     d  aasini                        4  0 overlay(dtini:1)
009100990603     d  mgsini                        4  0 overlay(dtini:5)
009200990602
009300990602     d                 ds
009400990602     d  prt1gg                        5  0
009500990602     d  prt2gg                        5  0
009600990602     d  prt3gg                        5  0
009700990602     d  prt4gg                        5  0
009800990602     d  prtol4                        5  0
009900990602     d  prtgg                  1     25  0 dim(5)
010000990602     d                 ds
010100990602     d  prtcon                        5  0
010200990609     d  prtr12                        5  0
010300990609     d  prtr24                        5  0
010400990609     d  prtr36                        5  0
010500990609     d  prtr48                        5  0
010600990609     d  prtr60                        5  0
010700990609     d  prto60                        5  0
010800020213     d  prtasr                        5  0
010900020213     d  prtore                 1     40  0 dim(8)
011000990601
011100990603     d                 ds
011200990603     d  prr1gg                        7  0
011300990603     d  prr2gg                        7  0
011400990603     d  prr3gg                        7  0
011500990603     d  prr4gg                        7  0
011600990603     d  prrol4                        7  0
011700990603     d  prrgg                  1     35  0 dim(5)
011800990603     d                 ds
011900990603     d  prrcon                        7  0
012000990609     d  prrr12                        7  0
012100990609     d  prrr24                        7  0
012200990609     d  prrr36                        7  0
012300990609     d  prrr48                        7  0
012400990609     d  prrr60                        7  0
012500990609     d  prro60                        7  0
012600020213     d  prrasr                        7  0
012700020213     d  prrore                 1     56  0 dim(8)
012800990603
012900990604     d                 ds
013000990603     d  prdks0                       20
013100990603     d  prdks1                       20
013200990603     d  prdks2                       20
013300990603     d  prdks3                       20
013400990603     d  prdks4                       20
013500990603     d  prdks5                       20
013600990603     d  prdks6                       20
013700990603     d  prdks7                       20
013800990603     d  prdks8                       20
013900990603     d  prdks9                       20
014000990603     d  rag                    1    200    dim(10)
014100990604     d                 ds
014200990604     d  prtks0                        7  0
014300990604     d  prtks1                        7  0
014400990604     d  prtks2                        7  0
014500990604     d  prtks3                        7  0
014600990604     d  prtks4                        7  0
014700990604     d  prtks5                        7  0
014800990604     d  prtks6                        7  0
014900990604     d  prtks7                        7  0
015000990604     d  prtks8                        7  0
015100990604     d  prtks9                        7  0
015200990604     d  pksc                   1     70  0 dim(10)
015300020911     D*-------------
015400020911     D* DS DI WRK
015500020911     D*-------------
015600020911     D TIBS34DS      E DS                                                       *Profili utente
015700020911     D DDATIUTE      E DS                                                       *Dati utente
015800020911     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
015900020913     D TRUL06DS      E DS                                                       *caricamento £x
016000020913     D  LIA                    1     90  0 DIM(30)                               -filiali
016100020911     D TRUL26DS      E DS                                                       *caricamento P.O.
016200020911     D  SKPO                  11    760  0 dim(250)
016300020911     D  SKPOT                761   1510  0 dim(250)
016400020911     D*-------------
016500020911     D* FILIALI IN GESTIONE
016600020911     D*-------------
016700020911     D L1              S              3  0 DIM(250)                             *FILIALI IN GESTIONE
016800990603
016900120912     I***FNBLP000
017000120912     I*             BLPAas                      TASAas
017100120912     I*             BLPlnp                      TASlnp
017200120912     I*             BLPnrs                      TASnrs
017300120912     I*             BLPnsp                      TASnsp
017400120912     I*             BLPmgs                      TASMGS
017500120912     I*             BLPksc                      tasksc
017600120912     I*             BLPcts                      tascts
017700120912     I*             BLPdcm                      tasdcm
017800120912     I*             BLPcca                      tascca
017900120912     I*             BLPnrc                      tasnrc
018000120912     I*             BLPcbo                      tascbo
018100120912     I*             BLPtc1                      tasftc
018200120912     I*             BLPtc2                      tastc2
018300120912     I*             BLPdcr                      tasdcr
018400980521      *
018500990603     c* STAMPO INTESTAZIONE
018600990603     c* decodifico codici clienti
018700990603     c                   z-add     1             ii
018800990604     c                   z-add     1             yy
018900990604     c                   clear                   pksc
019000990604     c                   clear                   rag
019100990603     c     ii            dowle     10
019200990603     c                   if        ksc(ii) > 0
019300990603     C                   clear                   TIBS69DS
019400990603     c                   z-add     ksc(ii)       I69kac
019500990603      *
019600990603     C                   CALL      'TIBS69R'
019700990603     C                   PARM                    tibs69DS
019800990603     C                   PARM                    DS_cnaco
019900990603     C                   PARM                    DS_cnind
020000990603     C                   PARM                    DS_cnclp
020100990603     C                   PARM                    DS_fncls
020200990604     c                   if        o69err = ' '
020300990604     c                   movel     acorag        rag(yy)
020400990604     c                   move      ksc(ii)       pksc(yy)
020500990604     c                   add       1             yy
020600990603     c                   endif
020700990603     c                   endif
020800990603     c                   add       1             ii
020900990603     c                   enddo
021000990603
021100990603     c                   move      '1'           wprima            1
021200990608     c                   exsr      prtint
021300990603     c                   move      ' '           wprima            1
021400980623
021500020911     c*
021600020911     c* Ciclo x tutte le filiale in gestione
021700020911     c     1             DO        250           III               3 0
021800020911     c                   Z-ADD     L1(III)       taslnp
021900021009     C*
022000021009     C* elabora solo se la linea è piena (SOLO se filiale)
022100021009     c                   if        *in01 = '1' AND                              *FNBLP
022200021009     c                             L1(III) = *zeros
022300021009     c                   leave
022400021009     c                   endif
022500020911     c*
022600990608     c* CICLO ELABORAZIONE DATI
022700990819     c  n01kbol          setll     titas38c
022800120912     c   01kbol1         setll     titas39c
022801120912     c** 01kbol1         setll     fnblp000
022900990603
023000990819     c  n01              read      titas38c                               31
023100120912     c   01              read      titas39c                               31
023101120912     c***01              read      fnblp000                               31
023200990603
023300990601     c     *in31         doweq     *off
023400990601     c                   movel     tasaas        wdtspe
023500990601     c                   move      tasmgs        wdtspe
023600120912     c* se sto leggendo perfiliale effettuo posizionamenti se non sono nel dal al
023700990603     c* richiesto e poi torno a leggere
023800120912     c                   if        *in01 = '1'
023900021009     C*
024000021009     C* a rottura di linea esce dal ciclo di lettura (SOLO se filiale) e legge la prossima l1
024100021009     c                   if        taslnp <> l1(iii)
024200021009     c                   leave
024300021009     c                   end
024400021009     c*
024500990603     c     wdtspe        ifgt      dl5dtf
024600120912     c***  taslnp        setgt     fnblp000
024601120912     c     taslnp        setgt     titas39c
024700990603     c                   goto      leggi
024800990603     c                   else
024900990603     c     wdtspe        iflt      dl5dti
025000120912     c***  kbol1         setll     fnblp000
025001120912     c     kbol1         setll     titas39c
025100990603     c                   goto      leggi
025200990603     c                   end
025300990603     c                   end
025400020213     c                   else                                                   *TITAS
025500990819     c* se sto leggendo titas e ho superato la data al esco dal ciclo
025600990603     c* di lettura
025700990603     c     wdtspe        ifgt      dl5dtf
025800990601     c                   seton                                        31
025900990601     c                   end
026000990603     c                   end
026100990603     c*
026200990601     c     *in31         ifeq      *off
026300990608     c* escludo recuperi
026400990831     c* Uso campo tascbo anche per bolle di sede solo se diverso da blanks
026500990831     C* (prima della trascodifica da tntas a titas infatti tale campo non
026600990831     c*  esisteva e quindi nelle bolle vecchie è ora = *blanks)
026700120912     c**   *in01         ifeq      *on
026800120912     c**   tascbo        orne      '  '
026900990819     c     tascbo        lookup    cbo                                    98
027000120912     c***                else
027100120912     c**   tastbl        lookup    tbl                                    98
027200120912     c**                 end
027300990819     c     *in98         ifeq      *off
027400990608     c     tasdcm        andgt     *zeros
027500990601     c     tasksc        lookup    ksc                                    32
027600990601     c     *in32         ifeq      *on
027700990602     c                   movel     'CT'          kcod
027800990602     c                   movel(P)  tascts        kkey
027900990602     c                   clear                   dsct
028000990602     c     ktab          chain     tabel00f                           32
028100990602     c  n32              movel     tbluni        dsct
028200990601
028300990602     c                   movel     §ctcor        wctcor
028400990601     c     iter          tag
028500990602     c                   z-add     1             id                3 0
028600990602     c     wctcor        lookup    rpord(ID)                              32
028700990604     c  n32              move      *all'9'       wctcor
028800990602     c  n32              goto      iter
028900990601
029000990602     c                   movel     §ctcor        rpord(id)
029100990601     c     id            occur     dstot
029200990602     c                   movel     §ctrap        reg
029300990602     c                   movel     §ctprv        prv
029400990602     c                   movel     §ctdes        dprv
029500990601     c* calcolo n.giorni impiegati per la consegna
029600990601     c                   clear                   wdat8
029700990601     c                   movel     tasaas        datada
029800990601     c                   move      tasmgs        datada
029900990601     c                   z-add     tasdcm        dataa
030000990601     c                   call      'XSRLAV8'
030100990601     c                   parm                    wdat8
030200990604     c                   sub       1             ggl
030300990601
030400990608     c                   select
030500990608     c     ggl           whenle    0
030600990608     c                   z-add     1             ggl
030700990608     c     ggl           whengt    4
030800990601     c                   z-add     5             ggl
030900990608     c                   endsl
031000990608     c*
031100990608     c* Verifico se spedizione con consegna anomala o giacenza o lasciato
031200990819     c* avviso
031300990608     c                   exsr      ctrspe
031400990608     c*
031500990608     c                   if        wnorm = *blanks
031600020213     c                   add       1             sok(ggl)
031700990601     c                   else
031800990601     c                   add       1             snk(ggl)
031900990601     c                   endif
032000990602      *
032100990602     c                   select
032200990609     c     tasnrc        whengt    60
032300990601     c                   z-add     7             ii
032400990616     c* le spedizioni in anticipo vengono cumulate fra le conformi
032500990602     c     tasnrc        whenlt    0
032600990616     c                   z-add     1             ii
032700990602     c                   other
032800990601     c                   z-add     1             ii
032900990603     c     tasnrc        lookup    snrc(ii)                               32
033000990602     c                   endsl
033100990608     c                   if        wnorm = *blanks
033200020213     c                   add       1             bok(ii)
033300020213     c* conteggio delle consegne su appuntamento e delle consegne ai supermercati
033400020213     c                   if        tasftc = 'A' or
033500020213     c                             tasftc = 'S' or
033600020213     c                             tastc2 = 'A' or
033700020213     c                             tastc2 = 'S' or
033800020213     c                             tasdcr > *zeros
033900020213     c                   add       1             bok(8)
034000020213     c                   endif
034100020213     c*
034200990601     c                   else
034300990601     c                   add       1             bnk(ii)
034400990601     c                   endif
034500990601     c
034600990601     c                   end
034700990603     c                   end
034800990603     c     leggi         tag
034900990819     c  n01              read      titas38c                               31
035000120912     c   01              read      titas39c                               31
035001120912     c***01              read      fnblp000                               31
035100990601     c                   end
035200990601     c                   enddo
035300020911     c*
035400120912     c* Se sto leggendo per la sede esco dal ciclo delle filiali in gestione
035500020911     c                   if        *in01 = *off
035600020911     c                   leave
035700020911     c                   endif
035800020911     c*
035900020911     c                   ENDDO
036000990602
036100990602     c*  CICLO DI STAMPA
036200990602     c                   movea     rpord         rpords
036300990602     c                   sorta     rpords
036400990601     c                   z-add     1             ii
036500990601     c     ii            dowle     200
036600990604     c     rpords(ii)    andne     '999'
036700990602     c                   z-add     1             yy                3 0
036800990602     c     rpords(ii)    lookup    rpord(yy)                              31
036900990602     c     yy            occur     dstot
037000990603     c* OPERAZIONI A ROTTURA DI REGIONE
037100990602     c     reg           ifne      regsav
037200990602     c* stampo totali regione precedente
037300990602     c* sposto schiera calcolo in schiera di stampa
037400990603     c* (salto queste operazioni la prima volta)
037500990603     c                   if        ii > 1
037600990603     c                   movea     rsok          prrgg
037700990603     c                   movea     rbok          prrore
037800990608     c                   xfoot     rsok          prrtot
037900990608     c                   if        $righi  > 60
038000990616     c                   exsr      prtint
038100990603     c                   endif
038200990608     c                   add       3             $righi
038300990616     c                   write     ssl6t3
038400990616     c                   write     ssl6tr
038500990608     c*
038600990608     c                   xfoot     rsnk          prrtot
038700990608     c* la riga spedizioni non ok la stampo solo su stampa ad uso interno
038800990616     c                   if        prrtot > 0
038900990602     c                   seton                                        10
039000990603     c                   movea     rsnk          prrgg
039100990603     c                   movea     rbnk          prrore
039200990608     c                   add       1             $righi
039300990616     c                   write     ssl6tr
039400990616     c                   setoff                                       10
039500990602     c                   endif
039600990608     c*
039700990616     c                   write     ssl6s1
039800990603     c* sommo totali regione in schiere totali generali
039900990603     c                   add       rsok          gsok
040000990603     c                   add       rsnk          gsnk
040100990603     c                   add       rbok          gbok
040200990603     c                   add       rbnk          gbnk
040300990603     c* pulisco schiere totali regione
040400990603     c                   clear                   rsok
040500990603     c                   clear                   rsnk
040600990603     c                   clear                   rbok
040700990603     c                   clear                   rbnk
040800990603     c                   endif
040900990602     c* decodifico e stampo la nuova regione
041000990602     c                   z-add     1             id
041100990602     c     reg           lookup    cre(id)                                31
041200990602     c   31              movel     dre(id)       prdreg
041300990602     c  n31              move      *blanks       prdreg
041400990608     c                   if        $righi > 59
041500990616     c                   exsr      prtint
041600990603     c                   endif
041700990608     c                   add       3             $righi
041800990616     c                   write     ssl6t4
041900990602
042000990602     c                   move      reg           regsav
042100990601     c                   end
042200990603     c* STAMPO RIGA DI DETTAGLIO
042300990602     c* sposto schiera calcolo in schiera di stampa
042400990602     c                   movea     sok           prtgg
042500990602     c                   movea     bok           prtore
042600990608     c                   xfoot     sok           prttot
042700990604     c                   movel     dprv          prdpro
042800990608     c                   if        $righi > 62
042900990616     c                   exsr      prtint
043000990608     c                   add       3             $righi
043100990616     c                   write     ssl6t4
043200990603     c                   endif
043300990608     C                   add       1             $RIGHi
043400990616     c                   write     ssl6d1
043500990608     c                   xfoot     snk           prttot
043600990602      * stampo riga spedizioni non ok (l.avviso,giacenti,chiusure anomale)
043700990616     c                   if        prttot > 0
043800990602     c                   seton                                        10
043900990602     c                   movea     snk           prtgg
044000990602     c                   movea     bnk           prtore
044100990608     c                   add       1             $righi
044200990616     c                   write     ssl6d1
044300990616     c                   setoff                                       10
044400990602     c                   endif
044500990602     c* sommo totali in schiera totali per regione
044600990602     c                   add       sok           rsok
044700990604     c                   add       snk           rsnk
044800990604     c                   add       bok           rbok
044900990604     c                   add       bnk           rbnk
045000990602
045100990601     c                   add       1             ii
045200990601     c                   enddo
045300990603
045400990603     c* OPERAZIONI A FINE CICLO:
045500990603     c* stampo totali regione
045600990603     c                   movea     rsok          prrgg
045700990608     c                   xfoot     rsok          prrtot
045800990603     c                   movea     rbok          prrore
045900990608     c                   if        $righi  > 57
046000990616     c                   exsr      prtint
046100990603     c                   endif
046200990608     C                   add       3             $righi
046300990616     c                   write     ssl6t3
046400990616     c                   write     ssl6tr
046500990608     c                   xfoot     rsnk          prrtot
046600990603      * stampo riga spedizioni non ok (l.avviso,giacenti,chiusure anomale)
046700990616     c                   if        prrtot> 0
046800990603     c                   seton                                        10
046900990603     c                   movea     rsnk          prrgg
047000990603     c                   movea     rbnk          prrore
047100990608     C                   add       1             $righi
047200990616     c                   write     ssl6tr
047300990616     c                   setoff                                       10
047400990603     c                   endif
047500990616     c                   write     ssl6s1
047600990603     c* sommo totali regione in schiere totali generali
047700990603     c                   add       rsok          gsok
047800990603     c                   add       rsnk          gsnk
047900990603     c                   add       rbok          gbok
048000990603     c                   add       rbnk          gbnk
048100990603     c* stampo totale generale
048200990603     c                   movea     gsok          prrgg
048300990608     c                   xfoot     gsok          prrtot
048400990603     c                   movea     gbok          prrore
048500990603     c                   seton                                        20
048600990608     C                   add       1             $righi
048700990616     c                   write     ssl6tr
048800990608     c                   xfoot     gsnk          prrtot
048900990603      * stampo riga spedizioni non ok (l.avviso,giacenti,chiusure anomale)
049000990616     c                   if        prrtot> 0
049100990603     c                   seton                                        10
049200990603     c                   movea     gsnk          prrgg
049300990603     c                   movea     gbnk          prrore
049400990608     c                   add       1             $righi
049500990616     c                   write     ssl6tr
049600990603     c                   endif
049700990616     c                   write     ssl6s1
049800990603     c* stampo "fine stampa"
049900990603     c                   write     ssl6e2
050000990616     c                   write     ssl6n1i
050100990603
050200990603     C                   IF        Wtibs69r  <>  *BLANKS
050300990603     C                   EVAL      I69TLA  = 'C'
050400990603     C                   CALL      'TIBS69R'
050500990603     C                   PARM                    TIBS69DS
050600990603     C                   ENDIF
050700990603     c*
050800990603     c                   seton                                        lr
050900990608     c**********************************************************************
051000990608     c     ctrspe        begsr
051100990608     c*
051200990608     c                   clear                   wnorm             1
051300990608
051400990608     c                   select
051500990608     c* consegna anomala
051600990608     c     tascca        whenne    *blanks
051700990608     c                   move      '1'           wnorm
051800990819     c* 01 off --> sono su titas (controllo se c'è stata giacenza)
051900120912     c***  *in01         wheneq    *off
052000120912     c***  tasfgc        andeq     'S'
052001120912     c     tasfgc        wheneq    'S'
052100990608     c                   move      '1'           wnorm
052200990608     c* 01 on  --> sono su blp
052300120912     c**   *in01         wheneq    *on
052400990608     c* controllo se c'è stata giacenza
052500120912     c**   kspe          chain     tigcp01l                           32
052600120912     c**                 if        *in32 = '0' and gcpatb = ' '
052700120912     c**                 move      '1'           wnorm
052800120912     c**                 endif
052900990819     c                   endsl
053000990819
053100990819     c* Se non c'è stata nè consegna anomala nè giacenza controllo se c'è
053200990819     c* stato lasciato avviso)
053300990819     c                   if        wnorm = ' '
053400990608     c* controllo se c'è stato lasciato avviso
053500990819     c     kspe          setll     fnevb01l
053600990819     c     kspe          reade     fnevb01l                               32
053700990608     c     *in32         doweq     *off
053800990608     c     wnorm         andeq     *blanks
053900990608     c     evbcev        lookup    c2a                                    32
054000990608     c   32              move      '1'           wnorm
054100990819     c     kspe          reade     fnevb01l                               32
054200990608     c                   enddo
054300990608     c                   endif
054400990608     c                   endsr
054500990603     c**********************************************************************
054600990616     c     prtint        begsr
054700990603     c*
054800990603     c*il piede lo stampo solo se non è la prima volta
054900990603     c                   if        wprima = ' '
055000990616     c                   write     ssl6n1i
055100990603     c                   endif
055200990603
055300990616     c                   write     ssl6t1
055400990603
055500990603     c                   if        ksc(6) > 0
055600990616     C                   write     ssl6t1b
055700990603     c                   else
055800990616     c                   write     ssl6tv
055900990603     c                   endif
056000990603     c
056100990616     c                   write     ssl6t2
056200990616     c                   write     ssl6t3
056300990608     c                   z-add     9             $righi
056400990603     c                   endsr
056500020911     C*--------------------------------------------------------------------------------------------*
056600020911     C* REPERISCE I DATI UTENTE
056700020911     C*--------------------------------------------------------------------------------------------*
056800020911     C     REPDATIUTE    BEGSR
056900020911     C*
057000020911     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
057100020911     C                   CLEAR                   L1
057200020913     C                   CLEAR                   TRUL06DS
057300020913     C                   CLEAR                   LIA
057400020911     C                   CLEAR                   TRUL26DS
057500020911     C                   CLEAR                   SKPO
057600020911     C                   CLEAR                   SKPOT
057700020911     C                   CLEAR                   TIBS34DS
057800020911     C                   CLEAR                   AZUTEDS
057900020911     C                   CLEAR                   DDATIUTE
058000020911     C*
058100020911     C     *DTAARA       DEFINE    §azute        azuteds
058200020911     C     *DTAARA       DEFINE    §datiute      ddatiute
058300020911     C                   IN(E)     *DTAARA
058400020911if  1C                   IF        %Error
058500020911     c                   EVAL      I34Tla = 'L'
058600020911     C                   CALL      'TIBS34R'
058700020911     C                   PARM                    Tibs34Ds
058800020911     C                   IN        *DTAARA
058900020911e   1C                   ENDIF
059000020911     C*
059100020911     C* SE SONO IN FILIALE DI 1° LIVELLO
059200020911     C                   IF        DUTLPO = '1'
059300020913     C* CARICA LE FILIALI IN GESTIONE
059400020913     C                   EVAL      DEPKPJBU = KPJBU
059500020913     C                   MOVEL     'L'           D06TLA
059600020913     C                   MOVEL     '£1'          D06COD
059700021007     C                   MOVEL     duttfp        D06KEY
059800020913     C                   MOVEL     TRUL06DS      KPJBU
059900020913     C                   CALL      'TRUL06R'
060000020913     C                   PARM                    KPJBA
060100020913     C                   MOVEL     KPJBU         TRUL06DS
060200020913     C                   MOVEA     LIA           L1                              -P.O. terminal
060300020913     C                   EVAL      KPJBU = DEPKPJBU
060400020911     C                   ENDIF
060500020911     C*
060600020911     C* SE SONO IN FILIALE DI 2° LIVELLO
060700020913     C                   IF        DUTLPO = '2'
060800020911     C                   EVAL      L1(1) = DUTPOU
060900020911     C                   ENDIF
061000020911     C*
061100020911     C                   ENDSR
061200990608     c**********************************************************************
061300990608     c     *inzsr        begsr
061400990608
061500990608     C     *entry        plist
061600990608     C                   parm                    kpjba
061700990608     C                   movel     kpjbu         tnssl5ds
061800021009     C*
061900021009     C* REPERISCE I DATI UTENTE
062000021009     C                   exsr      repdatiute
062100990608
062200990608     c     kbol          klist
062300990608     c                   kfld                    aasini
062400990608     c                   kfld                    mgsini
062500990608     c     kbol1         klist
062600990819     c                   kfld                    taslnp
062700990608     c                   kfld                    aasini
062800990608     c                   kfld                    mgsini
062900990608     c     kspe          klist
063000990608     c                   kfld                    tasaas
063100990819     c                   kfld                    taslnp
063200990819     c                   kfld                    tasnrs
063300990819     c                   kfld                    tasnsp
063400990608     c     ktab          klist
063500990608     c                   kfld                    codut
063600990608     c                   kfld                    kcod
063700990608     c                   kfld                    kkey
063800990608     c     ktab2         klist
063900990608     c                   kfld                    codut
064000990608     c                   kfld                    kcod
064100021009     c                   Z-ADD     1             codut
064200990608     c*
064300990608     c                   move      dl5dti        dtini
064400990608     c
064500021007     c                   if        simfel = *zeros
064600990819     c                   open      titas38c
064700990608     c                   else
064800120912     c***                open      fnblp43L
065000120912     c***                open      tigcp01l
065001120912     c                   if        %subst(knsif:7:1)='P'
065002120912     c                   eval      wlibsede=wlibsedep
065003120912     c                   else
065004120912     c                   eval      wlibsede=wlibsedeb
065005120912     c                   endif
065006120912     c                   open      titas39c
065100990608     c                   seton                                        01
065200990608     c                   endif
065300990608     c*
065400990608     c* pulizia schiere e ds multipla
065500990608     c                   clear                   rpords
065600990608     c                   z-add     1             ii                3 0
065700990608     c                   do        200           ii
065800990608     c     ii            occur     dstot
065900990608     c                   clear                   reg
066000990608     c                   clear                   prv
066100990608     c                   clear                   sok
066200990608     c                   clear                   snk
066300990608     c                   clear                   bok
066400990608     c                   clear                   bnk
066500990608     c                   enddo
066600990608     c* caricamento schiera regioni
066700990608     c                   z-add     0             ii
066800990608     c                   movel     'CT'          kcod
066900990608     c     ktab2         setll     tabel00f
067000990608     c     ktab2         reade     tabel00f                               31
067100990608     c     *in31         doweq     *off
067200990608     c     tblflg        ifeq      ' '
067300990608     c                   movel     tbluni        dsct
067400990608     c                   move      §ctcor        ds6pr2            1
067500990608     c     ds6pr2        ifeq      '9'
067600990608     c                   add       1             ii
067700990608     c                   movel     tblkey        cre(ii)
067800990608     c                   move      §ctdes        dre(ii)
067900990608     c                   end
068000990608     c                   endif
068100990608     c     ktab2         reade     tabel00f                               31
068200990608     c                   enddo
068300990608     c*
068400990608     c* caricamento tabella 2a (solo lasciato avviso)
068500990608     c                   z-add     0             ii
068600990608     c                   movel     '2A'          kcod
068700990608     c     ktab2         setll     tabel00f
068800990608     c     ktab2         reade     tabel00f                               31
068900990608     c     *in31         doweq     *off
069000990608     c     tblflg        ifeq      ' '
069100990608     c                   movel     tbluni        ds2a
069200990608     c     §2aftc        ifeq      'A'
069300990608     c                   add       1             ii
069400990608     c                   movel     tblkey        c2a(ii)
069500990608     c                   end
069600990608     c                   endif
069700990608     c     ktab2         reade     tabel00f                               31
069800990608     c                   enddo
069900990608     c* caricamento tabella 3a (solo recuperi)
070000990608     c                   z-add     0             ii
070100990608     c                   movel     '3A'          kcod
070200990608     c     ktab2         setll     tabel00f
070300990608     c     ktab2         reade     tabel00f                               31
070400990608     c     *in31         doweq     *off
070500990608     c     tblflg        ifeq      ' '
070600990608     c                   movel     tbluni        ds3a
070700990608     c     §3arbl        ifeq      'R'
070800990608     c                   add       1             ii
070900990608     c                   movel     tblkey        cbo(ii)
071000990608     c                   end
071100990608     c                   endif
071200990608     c     ktab2         reade     tabel00f                               31
071300990608     c                   enddo
071400990831     c* caricamento tabella tb (solo recuperi)
071500120912     c**                 z-add     0             ii
071600120912     c**                 movel     'TB'          kcod
071700120912     c**   ktab2         setll     tabel00f
071800120912     c**   ktab2         reade     tabel00f                               31
071900120912     c**   *in31         doweq     *off
072000120912     c**   tblflg        ifeq      ' '
072100120912     c**                 movel     tbluni        dstb
072200120912     c**   §tbrbl        ifeq      'R'
072300120912     c**                 add       1             ii
072400120912     c**                 movel     tblkey        tbl(ii)
072500120912     c**                 end
072600120912     c**                 endif
072700120912     c**   ktab2         reade     tabel00f                               31
072800120912     c**                 enddo
072900990831     c
073000990608     c                   clear                   prtgg
073100990608     c                   clear                   prtore
073200990608     c                   clear                   prrgg
073300990608     c                   clear                   prrore
073400990608
073500990608     c                   endsr
073600990601**
073700990609000012024036048060
