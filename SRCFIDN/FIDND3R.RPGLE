000100140130     /*END
000200041025     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000300041020      *----------------------------------------------------*
000400041230      *
  Statistiche RAC per Cliente       ?
000500041020      *
000600041020      *----------------------------------------------------*
000700140205     fazorg01l  if   e           k disk
000800140205     fTNTBE01L  if   e           k disk
000900041221     ffitgd10l  if   e           k disk
001000050113     ffitgd03l  if   e           k disk    rename(fitgd000:fitgd03)
001100041020     ffitgr01l  if   e           k disk
001200041026     ffitgn01l  if   e           k disk
001300140205     ftitas30c  if   e           k disk    usropn
001400140205     ffnorm01l  if   e           k disk
001500041228     fwfrcc00f  uf a e           k disk    usropn
001600041221     ffidnd3p   o    e             printer oflind(*in90)
001700041020      *----------------------------------------------------*
001800041020      *  RIEPILOGO INDICATORI
001900041020      *----------------------------------------------------*
002000041026      *  10/11- emisisone oppure no dei campi
002100041221      *  20/26- emissione oppure no dei campi
002200041229      *  15   - emissione oppure no dei campi
002300050111      *  16   - emissione oppure no p.o. responsabile
002400050117      *  17   - stampa dettaglio figli per codice unificante
002500041026      *  30   - comodo
002600041026      *----------------------------------------------------*
002700041020      *
002800041020
002900041020      *   S C H I E R E
003000041020
003100041227     d skcliel         s              7  0 dim(500)                             clienti da elaborare
003200041026     d skmad           s              3    dim(30)                              mot.apert.
003300150113     d sk_MAR          s                   like(TGDmad)   dim(90)               Motivo Apertura Recl
003400150113     d sk_MARd         s                   like(§MARdesc) dim(90)               Desc.Motivo Apertura
003500150113     d sk_MARs         s                   like(§MAReSt)  dim(90)               Mot.Ape. Esclus.Stat
003600041228     d cmd             s             80    dim(2) ctdata perrcd(1)              comando
003700041020
003800041022
003900041022      *   V A R I A B I L I
004000041022
004100041022     d lenght          s             15  5 inz(80)                              lunghezza cmd CLP
004200041022     d comman          s             80                                         valore cmd CLP
004300041022     d wlib            s             10
004400041229     d wlib1           s             10
004500041022
004600041025     d W_lib           s             30
004700041228     d wtibs69r        s              1
004800041022     d xx              s              3  0
004900041202     d x1              s              3  0
005000041026     d mm              s              3  0
005100041026     d xm              s              3  0
005200140205     d yy              s              3  0
005300041022     d Wrecok          s              1
005400041229     d W_clielwf       s              1
005500041229     d W_trc           s              1    inz('1')
005600041025     d ST_testa        s              1
005700041025     d dateu           s              8  0
005800041228     d Wrk_dta         s              8  0
005900041025     d w0140           s             14  0
006000041026     d w_tutte         s              7  0
006100041026
006200041026     d W_pttota        s             15  0
006300041026     d W_pctotch       s             15  0
006400041026     d W_pttotf        s             15  0
006500041026     d W_pttotn        s             15  0
006600041026     d W_pttoti        s             15  0
006700041130     d W_pttomc        s             15  0
006800041026     d W_pttotd        s             15  0
006900041026     d W_pttotc        s             15  0
007000041026     d W_pttotx        s             15  0
007100041020
007200041023     d K_tgrtri        s                   like(TGRtri) inz('N')
007300041020
007400041020      *   D S   I N T E R N E / E S T E R N E
007500041020
007600041020
007700041020     d wlbdat          ds                  inz
007800041020     d  g02dat                 1      8  0
007900041020     d  g02inv                 9     16  0
008000041020     d  g02err                17     17
008100041020     d  g02tgi                18     22  0
008200041022
008300041023     D                 DS
008400041023     D  WRKlnp                        3
008500041023     D  WRKnrs                        2
008600041023     D  WRKnsp                        7
008700041023     D  WRKann                        4
008800041023     D  WRKSPE                 1     16
008900041023
009000041023     D                 DS
009100041023     D  WRKpoe                        3
009200041023     D  WRKnso                        2
009300041023     D  WRKnor                        7
009400041023     D  WRKnrv                        2
009500041023     D  WRKORM                 1     14
009600041025
009700041228      * - DS di comodo    ORM
009800041228     D                 DS
009900041228     D  W_poe                         3  0
010000041228     D  W_nsr                         2  0
010100041228     D  W_nor                         7  0
010200041228     D  W_nrv                         2  0
010300041228     D  Wormkey                1     14  0 inz
010400041228
010500041228      * - DS di comodo    spedizione
010600041228     D                 DS
010700041228     D  W_lnp                         3  0
010800041228     D  W_nrs                         2  0
010900041228     D  W_nsp                         7  0
011000041228     D  W_aas                         4  0
011100041228     D  Wspekey                1     16  0 inz
011200041022
011300041022     d Kpjba         e ds
011400041022
011500041227
011600041227      * Parametri per pgm. TIBS10R (reperimento codice padre cliente)
011700041227     D TIBS10ds      e ds                  inz
011800041227     D   skc11                21   5520  0 DIM(500)
011900041228      * - Ds per Anagrafiche Clienti
012000041228     d TIBS69DS      E DS
012100041228     d DS_cnaco      E DS                  extname(CNACO00F)
012200041228     d DS_cnind      E DS                  extname(CNIND00F)
012300041228     d DS_cnclp      E DS                  extname(CNCLP00F)
012400041228     d DS_fncls      E DS                  extname(FNCLS00F)
012500041228      *
012600041227
012700041022      * - Parametri x Controllo profilo utenti
012800041022     d TIBS34DS      e ds                  inz
012900041022      * - Ds di riferimento al file esterno AZUTE00F
013000041022     d AZUTEDS       e ds                  extname(AZUTE00F)
013100041022      * - Ds per dati organigramma
013200041022     d DDatiUte      e ds
013300041022      * - Parametri per gestione reclami già esistenti
013400041221     d FIDND2DS      e ds
013500140925     d  TCO                  116    119
013600140925     d                                     DIM(4)
013700140925     d  MAR                  120    143
013800041022     d                                     DIM(8)
013900140925     d  CCR                  144    152
014000041022     d                                     DIM(3)
014100041026      * - Tabelle
014200041026     d TIBS02ds      e ds                  inz
014300041026
014400041103     d dchr          e ds
014500041103     d dmar          e ds
014600140205
014700140205       //--------------------------------------------------------------
014800140205       //?Definizione key-list.                                        ?
014900140205       //--------------------------------------------------------------
015000140205
015100140205       // -?File TNTBE01L?
015200140205     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
015300140205     d                                     prefix(k_)   inz
015400041228
015500041202      *
015600041022      *---------------------------------------------------------------*
015700041022
015800041022
015900041022      *---------------------------------------------------------------*
016000041022      *            Ciclo principale Statistica                        *
016100041022      *---------------------------------------------------------------*
016200041022      *
016300041022      * stampo le richieste
016400041022     c                   exsr      SR_TESTA
016500041022      * Verifico le richieste
016600041227      * se scelta 2 (un cliente unificante) recupero i clienti a lui legato
016700041022
016800041227     c                   If        id2sce = '2'
016900041227     c                   exsr      SR_reccli
017000041227      *
017100041227     c                   else
017200041227     c                   eval      skcliel(1) = id2ksc
017300041227     c                   endif
017400041227
017500050113      * Se viene richiesto il dettagli oper ogni figlio oppure scelta unico cliente
017600050113     c                   If        id2sce='1' or (id2sce='2' and id2det='S')
017700041227     c                   exsr      SR_eladet
017800050113      *
017900050113     c                   else
018000041227      *
018100050113     c                   exsr      SR_elauni
018200050113     c                   endif
018300041022      *
018400041228
018500041228     C                   IF        Wtibs69r  <>  *BLANKS
018600041228     C                   eval      I69TLA  = 'C'
018700041228     C                   call      'TIBS69R'
018800041228     C                   parm                    TIBS69DS
018900041228     C                   ENDIF
019000041228      *
019100041022     c                   seton                                        lr
019200041022      *
019300041022      *---------------------------------------------------------------*
019400041227      * Recupero i clienti da elaborare con codice padre richiesto    *
019500041022      *---------------------------------------------------------------*
019600041227     c     Sr_RECcli     begsr
019700041022
019800041227      *
019900041227      * cerco i clienti figli del cliente unificante attraverso il tibs10
020000041227      *
020100041227     c                   clear                   tibs10ds
020200041227     c                   eval      d10tle = 'ST'
020300041227     c                   eval      d10paf = 'F'
020400041227     c                   z-add     Id2ksc        d10cod
020500041227     c                   z-add     dateu         d10drf
020600041227     c                   CALL      'TIBS10R'
020700041227     c                   parm                    tibs10ds
020800041227     c                   parm      ' '           partpexe          1
020900041227      * se non ci sono errori carico la schiera dei figli che contiene il codice del cliente padre
021000041227     c                   if        d10err = ' '
021100041227      * muovo in una schiera di 7
021200041227     c                   z-add     skc11         skcliel
021300041227      *
021400041227     c                   endif
021500041022      *
021600041022     c                   endsr
021700041022      *---------------------------------------------------------------*
021800041228      * Elaborazione Reclami per CODICE CLIENTE                       *
021900041022      *---------------------------------------------------------------*
022000041022     c     SR_Eladet     BegSr
022100041022      *
022200041022     c                   clear                   xx
022300041022      *
022400041022     c                   Do        *hival
022500041022      *
022600041022     c                   add       1             xx
022700041022
022800041022      * se uguale a zero oppure xx = 501 vado a fine
022900041022
023000041228     c                   if        Skcliel(xx) = 0 or xx = 501
023100041022     c                   leave
023200041022     c                   endif
023300041022
023400041228      * eseguo pulizie  ed imposto i dati per la testa
023500041228     c                   eval      codcli = Skcliel(xx)
023600041228     c                   exsr      SR_newcli
023700041023
023800041022      * leggo il file
023900041022
024000041228     c     Skcliel(xx)   setll     fitgd10l
024100041022      *
024200041025     c                   do        *hival
024300041025      *
024400041228     c     Skcliel(xx)   reade     fitgd10l
024500041022
024600041228     c                   if        %eof(fitgd10l)
024700041022     c                   exsr      SR_totale
024800041022     c                   leave
024900041022     c                   endif
025000041022      * controllo il record con le selezioni fatte
025100041022     c                   exsr      SR_ctrrec
025200041022      * se record non ok leggo il successivo
025300041022     c                   if        Wrecok = ' '
025400041022     c                   iter
025500041022     c                   endif
025600041228      * preparo i campi a li stampo
025700041022     c                   exsr      SR_prepa
025800041022
025900041022     c                   enddo
026000041025
026100041025     c                   enddo
026200041022
026300041022      *
026400041022     c                   endsr
026500050113      *---------------------------------------------------------------*
026600050113      * Elaborazione Reclami per CODICE CLIENTE UNIFICANTE            *
026700050113      *---------------------------------------------------------------*
026800050113     c     SR_Elauni     BegSr
026900050113
027000050113      *  imposto i dati per la testa
027100050113     c                   eval      codcli = id2ksc
027200050113     c                   exsr      SR_newcli
027300050113      *
027400050113      * lettura file per data richiesta
027500050113     c     id2amgdcd     setll     fitgd03l
027600050113      *
027700050113     c                   Do        *hival
027800050113      *
027900050113      *
028000050113     c                   read      fitgd03l
028100050113
028200050113     c                   if        %eof(fitgd03l) or (tgddar > id2amgdca and
028300050113     c                             id2amgdca > 0)
028400050113     c                   exsr      SR_totale
028500050113     c                   leave
028600050113     c                   endif
028700050113
028800050113      * verifico se cliente rientra nei figli legati all'unificante
028900050113
029000050113      * se uguale a zero leggo il successivo
029100050113     c                   if        tgdksc = 0
029200050113     c                   iter
029300050113     c                   endif
029400050113      * controllo se codice presente nella schiera dei figli legati all'unificante
029500050113     c     tgdksc        lookup    skcliel(1)                             31
029600050113     c                   If        not %found
029700050113     c                   iter
029800050113     c                   endif
029900050113      * controllo il record con le selezioni fatte
030000050113     c                   exsr      SR_ctrrec
030100050113      * se record non ok leggo il successivo
030200050113     c                   if        Wrecok = ' '
030300050113     c                   iter
030400050113     c                   endif
030500050113      * preparo i campi a li stampo
030600050113     c                   exsr      SR_prepa
030700050113
030800050113     c                   enddo
030900050113
031000050113
031100050113      *
031200050113     c                   endsr
031300041023      *---------------------------------------------------------------*
031400041228      * Preparazione campi per il nuovo codice cliente                *
031500041023      *---------------------------------------------------------------*
031600041228     c     SR_Newcli     BegSr
031700041228      *
031800041228     c                   clear                   skmad
031900041228     c                   clear                   mm
032000041228
032100041228      *
032200041228      * decodifica cliente
032300041228      *
032400041228
032500041228     C                   clear                   ragcli
032600041228     C                   clear                   TIBS69DS
032700041228     c                   z-add     codcli        I69kac
032800041228     C                   call      'TIBS69R'
032900041228     C                   parm                    tibs69DS
033000041228     C                   parm                    DS_cnaco
033100041228     C                   parm                    DS_cnind
033200041228     C                   parm                    DS_cnclp
033300041228     C                   parm                    DS_fncls
033400041228     C                   eval      wtibs69r = '1'
033500041228     c                   If         O69ERR =  ' '
033600041228     c                   movel     ACOrag        ragcli
033700041228     c                   Endif
033800041228
033900041230     c                   Endsr
034000041228      *
034100041228      *---------------------------------------------------------------*
034200041228      * Controllo i dati con le richieste del video                   *
034300041228      *---------------------------------------------------------------*
034400041228     c     SR_ctrrec     BegSr
034500041228
034600041228     c                   clear                   Wrecok
034700041228      *
034800041228     c                   do        *hival
034900041228      *
035000041228      *- P.O. responsabile
035100041228      * diverso da quello a video
035200041230     c                   if        ID2prs > *zeros and tgdpor <> id2prs
035300041228     c                   leave
035400041228     c                   endif
035500041228      *
035600041228      *- P.O. apertura
035700041228      * diverso da quello a video
035800041230     c                   if        id2fil > *zeros and tgdpot <> id2fil
035900041228     c                   leave
036000041228     c                   endif
036100041228      *
036200041228      *- Date chiamata
036300041228      * data minore di quella a video
036400041230     c                   if        id2AMGdcd > 0  and tgddar < id2AMGdcd
036500041228     c                   leave
036600041228     c                   endif
036700041228      * data maggiore di  quella a video
036800041230     c                   if        id2AMGdca > 0  and tgddar > id2AMGdca
036900041228     c                   leave
037000041228     c                   endif
037100041228      *
037200041228      *- Date chiusura
037300041228      * data minore di quella a video
037400041230     c                   if        id2AMGdhd > 0  and tgddcd < id2AMGdhd
037500041228     c                   leave
037600041228     c                   endif
037700041228      * data maggiore di  quella a video
037800041230     c                   if        id2AMGdha > 0  and tgddcd > id2AMGdha
037900041228     c                   leave
038000041228     c                   endif
038100041228      *- Tipo comunicazione
038200041228      *  diverso da quelli del video
038300140424     c                   clear                   w_lib
038400041228     c                   movea     tco           w_lib
038500041228     c                   If        w_lib <> *blank
038600041228     c     tgdtco        lookup    tco                                    30
038700041228     c                   if        not %found
038800041228     c                   leave
038900041228     c                   endif
039000041228     c                   endif
039100041228      *- Motivo apertura
039200041228      *  diverso da quelli del video
039300140424     c                   clear                   w_lib
039400041228     c                   movea     mar           w_lib
039500041228     c                   If        w_lib <> *blank
039600041228     c     tgdmad        lookup    mar                                    30
039700041228     c                   if        not %found
039800041228     c                   leave
039900041228     c                   endif
040000041228     c                   endif
040100140210      * ?solo motivi NON esclusi dalle statistiche?
040200140205     c                   eval      yy = %lookup( TGDmad : sk_MAR )
040300140210     c                   if        yy > *zero  and  sk_MARs(yy) = 'S'
040400140205     c                   leave
040500140205     c                   endif
040600041228      *- Codici chiusura
040700041228      *  diverso da quelli del video
040800140424     c                   clear                   w_lib
040900041228     c                   movea     ccr           w_lib
041000041228     c                   If        w_lib <> *blank
041100041228     c     tgdcdt        lookup    ccr                                    30
041200041228     c                   if        not %found
041300041228     c                   leave
041400041228     c                   endif
041500041228     c                   endif
041600041228      *- Solo le aperte
041700041228      *  e codice chiusura valorizzato
041800041230     c                   If        id2sra = 'S' and tgdcdt <> *blank
041900041228     c                   leave
042000041228     c                   endif
042100041228      *
042200041228      * se tutto OK record OK
042300041228     c                   eval      Wrecok = 'Y'
042400041228     c                   leave
042500041228      *
042600041228     c                   enddo
042700041228
042800041228     c                   endsr
042900041023      *---------------------------------------------------------------*
043000041023      * Fine lettura di un P.O. TOTALI                                *
043100041023      *---------------------------------------------------------------*
043200041023     c     SR_Totale     BegSr
043300041023      *
043400041229      * se ho caricato almeno un elemento della schiera motivo reclamo stampo
043500041229     c                   if        mm > 0
043600041229     c                   write     wfrcc000
043700041229      * pulisco prima volta che imposto i dati per ogni CLIENTE
043800041229     c                   eval      W_clielwf = ' '
043900041026      *
044000041026      * stampa anche dei totali
044100041026      *
044200041026      * testate
044300041229     c                   write     DND3t1
044400041229     c                   write     DND3tS
044500041229     c                   write     DND3t3
044600041229     c                   write     DND3t5
044700041229     c                   write     DND3t3
044800041026
044900041229     c                   write     DND3tt
045000041229     c                   write     DND3t3B
045100041026      *
045200041026     c                   exsr      SR_statot
045300041025      * pulisco il record
045400041229     c                   clear                   wfrcc000
045500041023     c                   endif
045600041023     c                   endsr
045700041026      *---------------------------------------------------------------*
045800041026      * Elaboro e stampo i totali                                     *
045900041026      *---------------------------------------------------------------*
046000041026     c     SR_Statot     BegSr
046100041026      *
046200041026      * pulisco i campi di comodo
046300041026      *
046400041026     c                   clear                   W_pttota
046500041026     c                   clear                   W_pctotch
046600041026     c                   clear                   W_pttotf
046700041026     c                   clear                   W_pttotn
046800041026     c                   clear                   W_pttoti
046900041130     c                   clear                   W_pttomc
047000041026     c                   clear                   W_pttotd
047100041026     c                   clear                   W_pttotc
047200041026     c                   clear                   W_pttotx
047300041026      * riga dei totali
047400041026      *
047500041026     c                   movel(p)  'T O T A L E' PTCdes
047600041026      * totali aperti
047700041230     c                   eval      W_pttota= rcca01 + rcca02 + rcca03 + rcca05 +
047800041230     c                             rcca06 + rcca07 + rcca10 + rcca11 + rcca12 +
047900041230     c                             rcca13 + rcca20 + rcca21 + rcca22 + rcca30 +
048000120914     c                             rcca99 + rccax1 + rccax2 + rcca14 + rcca15 +
048100151202     c                             rcca16 + rcca50 + rcca17 + rcca18
048200041026     c                   z-add     W_pttota      pttota
048300041026      * totali chiusi fondati
048400041230     c                   eval      W_pttotf= rccf01 + rccf02 + rccf03 + rccf05 +
048500041230     c                             rccf06 + rccf07 + rccf10 + rccf11 + rccf12 +
048600041230     c                             rccf13 + rccf20 + rccf21 + rccf22 + rccf30 +
048700120914     c                             rccf99 + rccfx1 + rccfx2 + rccf14 + rccf15 +
048800151202     c                             rccf16 + rccf50 + rccf17 + rccf18
048900041026     c                   z-add     W_pttotf      pttotf
049000041026      * totali chiusi infondati
049100041230     c                   eval      W_pttotn= rccn01 + rccn02 + rccn03 + rccn05 +
049200041230     c                             rccn06 + rccn07 + rccn10 + rccn11 + rccn12 +
049300041230     c                             rccn13 + rccn20 + rccn21 + rccn22 + rccn30 +
049400120914     c                             rccn99 + rccnx1 + rccnx2 + rccn14 + rccn15 +
049500151202     c                             rccn16 + rccn50 + rccn17 + rccn18
049600041026     c                   z-add     W_pttotn      pttotn
049700041026      * totali chiusi informativi
049800041230     c                   eval      W_pttoti= rcci01 + rcci02 + rcci03 + rcci05 +
049900041230     c                             rcci06 + rcci07 + rcci10 + rcci11 + rcci12 +
050000041230     c                             rcci13 + rcci20 + rcci21 + rcci22 + rcci30 +
050100120914     c                             rcci99 + rccix1 + rccix2 + rcci14 + rcci15 +
050200151202     c                             rcci16 + rcci50 + rcci17 + rcci18
050300041026     c                   z-add     W_pttoti      pttoti
050400041026      * totali chiusi
050500041026     c                   eval      W_pctotch =W_pttotf + W_pttotn + W_pttoti
050600041026     c                   z-add     W_pctotch     pctotch
050700041026      * totali aperte + chiuse
050800041026     c                   eval      W_tutte = W_pttota + W_pctotch
050900041026      * totali per chi chiama  Mittente
051000041230     c                   eval      W_pttomc= rccm01 + rccm02 + rccm03 + rccm05 +
051100041230     c                             rccm06 + rccm07 + rccm10 + rccm11 + rccm12 +
051200041230     c                             rccm13 + rccm20 + rccm21 + rccm22 + rccm30 +
051300120914     c                             rccm99 + rccmx1 + rccmx2 + rccm14 + rccm15 +
051400151202     c                             rccm16 + rccm50 + rccm17 + rccm18
051500041130     c                   z-add     W_pttomc      pttomc
051600041026      * totali per chi chiama  Destinatario
051700041230     c                   eval      W_pttotd= rccd01 + rccd02 + rccd03 + rccd05 +
051800041230     c                             rccd06 + rccd07 + rccd10 + rccd11 + rccd12 +
051900041230     c                             rccd13 + rccd20 + rccd21 + rccd22 + rccd30 +
052000120914     c                             rccd99 + rccdx1 + rccdx2 + rccd14 + rccd15 +
052100151202     c                             rccd16 + rccd50 + rccd17 + rccd18
052200041026     c                   z-add     W_pttotd      pttotd
052300041026      * totali per chi chiama  Cliente
052400041230     c                   eval      W_pttotc= rccc01 + rccc02 + rccc03 + rccc05 +
052500041230     c                             rccc06 + rccc07 + rccc10 + rccc11 + rccc12 +
052600041230     c                             rccc13 + rccc20 + rccc21 + rccc22 + rccc30 +
052700120914     c                             rccc99 + rcccx1 + rcccx2 + rccc14 + rccc15 +
052800151202     c                             rccc16 + rccc50 + rccc17 + rccc18
052900041130     c                   add       W_pttotc      pttomc
053000041026      * totali per chi chiama  Altro
053100041230     c                   eval      W_pttotx= rccx01 + rccx02 + rccx03 + rccx05 +
053200041230     c                             rccx06 + rccx07 + rccx10 + rccx11 + rccx12 +
053300041230     c                             rccx13 + rccx20 + rccx21 + rccx22 + rccx30 +
053400120914     c                             rccx99 + rccxx1 + rccxx2 + rccx14 + rccx15 +
053500151202     c                             rccx16 + rccx50 + rccx17 + rccx18
053600041026     c                   z-add     W_pttotx      pttotx
053700041026
053800041026      * gestisco indicatore solo per la  riga dei totali
053900041026
054000041026     c                   seton                                        10
054100041026     c                   setoff                                       11
054200041026
054300041229     c                   write     DND3dt2
054400041026     c                   seton                                        11
054500041026     c                   setoff                                       10
054600041026
054700041026      * riga delle percentuali
054800041026
054900041026      * Chiuse
055000041026     c                   if        pttotf > 0
055100041104     c                   eval(h)   tcfper = (pttotf * 100) / pctotch
055200041026     c                   seton                                        20
055300041026     c                   else
055400041026     c                   clear                   tcfper
055500041026     c                   setoff                                       20
055600041026     c                   endif
055700041026
055800041026     c                   if        pttotn > 0
055900041104     c                   eval(h)   tifper = (pttotn * 100) / pctotch
056000041026     c                   seton                                        21
056100041026     c                   else
056200041026     c                   clear                   tifper
056300041026     c                   setoff                                       21
056400041026     c                   endif
056500041026
056600041026     c                   if        pttoti > 0
056700041104     c                   eval(h)   tinper = (pttoti * 100) / pctotch
056800041026     c                   seton                                        22
056900041026     c                   else
057000041026     c                   clear                   tinper
057100041026     c                   setoff                                       22
057200041026     c                   endif
057300041026      * Chi Chiama
057400041130     c                   if        pttomc > 0
057500041130     c                   eval(h)   mclper = (pttomc * 100) / W_tutte
057600041026     c                   seton                                        23
057700041026     c                   else
057800041130     c                   clear                   mclper
057900041026     c                   setoff                                       23
058000041026     c                   endif
058100041026
058200041026     c                   if        pttotd > 0
058300041104     c                   eval(h)   desper = (pttotd * 100) / W_tutte
058400041026     c                   seton                                        24
058500041026     c                   else
058600041026     c                   setoff                                       24
058700041026     c                   clear                   desper
058800041026     c                   endif
058900041026
059000041026     c                   if        pttotx > 0
059100041104     c                   eval(h)   altper = (pttotx * 100) / W_tutte
059200041026     c                   seton                                        26
059300041026     c                   else
059400041026     c                   setoff                                       26
059500041026     c                   clear                   altper
059600041026     c                   endif
059700041026
059800041026      * stampo riga percentuali
059900041229     c                   write     DND3dt4
060000041026      * stampo riga titolo motivo d'apertura
060100041229     c                   write     DND3dt3
060200041026      * elaboro per ogni motivo
060300041026      *
060400041026     c                   z-add     0             xm
060500041026      *
060600041026     c                   do        mm            xm
060700041026     c                   if        skmad(xm) <> *blanks
060800041026      * decodifico
060900140205     c                   eval      yy = %lookup( skMAD(xm) : sk_MAR )
061000140205     c                   if        yy > *zero
061100140205     c                   eval      PTCdes = sk_MARd(yy)
061200140205     c                   else
061300140205     c                   eval      PTCdes = *all'? '
061400140205     c                   endif
061500041026      *
061600041026      * valorizzo i campi di stampa
061700041026     c                   IF        skmad(xm) = '  1'
061800041229     c                   z-add     rcca01        pttota
061900041229     c                   z-add     rccf01        pctotch
062000041229     c                   add       rccn01        pctotch
062100041229     c                   add       rcci01        pctotch
062200041229     c                   z-add     rccf01        pttotf
062300041229     c                   z-add     rccn01        pttotn
062400041229     c                   z-add     rcci01        pttoti
062500041229     c     rccc01        add       rccm01        pttomc
062600041229     c                   z-add     rccd01        pttotd
062700041229     c                   z-add     rccx01        pttotx
062800041026     c                   endif
062900041026
063000041026     c                   IF        skmad(xm) = '  2'
063100041229     c                   z-add     rcca02        pttota
063200041229     c                   z-add     rccf02        pctotch
063300041229     c                   add       rccn02        pctotch
063400041229     c                   add       rcci02        pctotch
063500041229     c                   z-add     rccf02        pttotf
063600041229     c                   z-add     rccn02        pttotn
063700041229     c                   z-add     rcci02        pttoti
063800041229     c     rccc02        add       rccm02        pttomc
063900041229     c                   z-add     rccd02        pttotd
064000041229     c                   z-add     rccx02        pttotx
064100041026     c                   endif
064200041026
064300041026     c                   IF        skmad(xm) = '  3'
064400041229     c                   z-add     rcca03        pttota
064500041229     c                   z-add     rccf03        pctotch
064600041229     c                   add       rccn03        pctotch
064700041229     c                   add       rcci03        pctotch
064800041229     c                   z-add     rccf03        pttotf
064900041229     c                   z-add     rccn03        pttotn
065000041229     c                   z-add     rcci03        pttoti
065100041229     c     rccc03        add       rccm03        pttomc
065200041229     c                   z-add     rccd03        pttotd
065300041229     c                   z-add     rccx03        pttotx
065400041026     c                   endif
065500041026
065600041026     c                   IF        skmad(xm) = '  5'
065700041229     c                   z-add     rcca05        pttota
065800041229     c                   z-add     rccf05        pctotch
065900041229     c                   add       rccn05        pctotch
066000041229     c                   add       rcci05        pctotch
066100041229     c                   z-add     rccf05        pttotf
066200041229     c                   z-add     rccn05        pttotn
066300041229     c                   z-add     rcci05        pttoti
066400041229     c     rccc05        add       rccm05        pttomc
066500041229     c                   z-add     rccd05        pttotd
066600041229     c                   z-add     rccx05        pttotx
066700041026     c                   endif
066800041026
066900041026     c                   IF        skmad(xm) = '  6'
067000041229     c                   z-add     rcca06        pttota
067100041229     c                   z-add     rccf06        pctotch
067200041229     c                   add       rccn06        pctotch
067300041229     c                   add       rcci06        pctotch
067400041229     c                   z-add     rccf06        pttotf
067500041229     c                   z-add     rccn06        pttotn
067600041229     c                   z-add     rcci06        pttoti
067700041229     c     rccc06        add       rccm06        pttomc
067800041229     c                   z-add     rccd06        pttotd
067900041229     c                   z-add     rccx06        pttotx
068000041026     c                   endif
068100041026
068200041026     c                   IF        skmad(xm) = '  7'
068300041229     c                   z-add     rcca07        pttota
068400041229     c                   z-add     rccf07        pctotch
068500041229     c                   add       rccn07        pctotch
068600041229     c                   add       rcci07        pctotch
068700041229     c                   z-add     rccf07        pttotf
068800041229     c                   z-add     rccn07        pttotn
068900041229     c                   z-add     rcci07        pttoti
069000041229     c     rccc07        add       rccm07        pttomc
069100041229     c                   z-add     rccd07        pttotd
069200041229     c                   z-add     rccx07        pttotx
069300041026     c                   endif
069400041026
069500041026     c                   IF        skmad(xm) = ' 10'
069600041229     c                   z-add     rcca10        pttota
069700041229     c                   z-add     rccf10        pctotch
069800041229     c                   add       rccn10        pctotch
069900041229     c                   add       rcci10        pctotch
070000041229     c                   z-add     rccf10        pttotf
070100041229     c                   z-add     rccn10        pttotn
070200041229     c                   z-add     rcci10        pttoti
070300041229     c     rccc10        add       rccm10        pttomc
070400041229     c                   z-add     rccd10        pttotd
070500041229     c                   z-add     rccx10        pttotx
070600041026     c                   endif
070700041026
070800041026     c                   IF        skmad(xm) = ' 11'
070900041229     c                   z-add     rcca11        pttota
071000041229     c                   z-add     rccf11        pctotch
071100041229     c                   add       rccn11        pctotch
071200041229     c                   add       rcci11        pctotch
071300041229     c                   z-add     rccf11        pttotf
071400041229     c                   z-add     rccn11        pttotn
071500041229     c                   z-add     rcci11        pttoti
071600041229     c     rccc11        add       rccm11        pttomc
071700041229     c                   z-add     rccd11        pttotd
071800041229     c                   z-add     rccx11        pttotx
071900041026     c                   endif
072000041026
072100041026     c                   IF        skmad(xm) = ' 12'
072200041229     c                   z-add     rcca12        pttota
072300041229     c                   z-add     rccf12        pctotch
072400041229     c                   add       rccn12        pctotch
072500041229     c                   add       rcci12        pctotch
072600041229     c                   z-add     rccf12        pttotf
072700041229     c                   z-add     rccn12        pttotn
072800041229     c                   z-add     rcci12        pttoti
072900041229     c     rccc12        add       rccm12        pttomc
073000041229     c                   z-add     rccd12        pttotd
073100041229     c                   z-add     rccx12        pttotx
073200041026     c                   endif
073300041026
073400041026     c                   IF        skmad(xm) = ' 13'
073500041229     c                   z-add     rcca13        pttota
073600041229     c                   z-add     rccf13        pctotch
073700041229     c                   add       rccn13        pctotch
073800041229     c                   add       rcci13        pctotch
073900041229     c                   z-add     rccf13        pttotf
074000041229     c                   z-add     rccn13        pttotn
074100041229     c                   z-add     rcci13        pttoti
074200041229     c     rccc13        add       rccm13        pttomc
074300041229     c                   z-add     rccd13        pttotd
074400041229     c                   z-add     rccx13        pttotx
074500041026     c                   endif
074600120914
074700120914     c                   IF        skmad(xm) = ' 14'
074800120914     c                   z-add     rcca14        pttota
074900120914     c                   z-add     rccf14        pctotch
075000120914     c                   add       rccn14        pctotch
075100120914     c                   add       rcci14        pctotch
075200120914     c                   z-add     rccf14        pttotf
075300120914     c                   z-add     rccn14        pttotn
075400120914     c                   z-add     rcci14        pttoti
075500120914     c     rccc14        add       rccm14        pttomc
075600120914     c                   z-add     rccd14        pttotd
075700120914     c                   z-add     rccx14        pttotx
075800120914     c                   endif
075900120914
076000120914     c                   IF        skmad(xm) = ' 15'
076100120914     c                   z-add     rcca15        pttota
076200120914     c                   z-add     rccf15        pctotch
076300120914     c                   add       rccn15        pctotch
076400120914     c                   add       rcci15        pctotch
076500120914     c                   z-add     rccf15        pttotf
076600120914     c                   z-add     rccn15        pttotn
076700120914     c                   z-add     rcci15        pttoti
076800120914     c     rccc15        add       rccm15        pttomc
076900120914     c                   z-add     rccd15        pttotd
077000120914     c                   z-add     rccx15        pttotx
077100120914     c                   endif
077200120914
077300120914     c                   IF        skmad(xm) = ' 16'
077400120914     c                   z-add     rcca16        pttota
077500120914     c                   z-add     rccf16        pctotch
077600120914     c                   add       rccn16        pctotch
077700120914     c                   add       rcci16        pctotch
077800120914     c                   z-add     rccf16        pttotf
077900120914     c                   z-add     rccn16        pttotn
078000120914     c                   z-add     rcci16        pttoti
078100120914     c     rccc16        add       rccm16        pttomc
078200120914     c                   z-add     rccd16        pttotd
078300120914     c                   z-add     rccx16        pttotx
078400120914     c                   endif
078500041026
078600041026     c                   IF        skmad(xm) = ' 20'
078700041229     c                   z-add     rcca20        pttota
078800041229     c                   z-add     rccf20        pctotch
078900041229     c                   add       rccn20        pctotch
079000041229     c                   add       rcci20        pctotch
079100041229     c                   z-add     rccf20        pttotf
079200041229     c                   z-add     rccn20        pttotn
079300041229     c                   z-add     rcci20        pttoti
079400041229     c     rccc20        add       rccm20        pttomc
079500041229     c                   z-add     rccd20        pttotd
079600041229     c                   z-add     rccx20        pttotx
079700041026     c                   endif
079800041026
079900041026     c                   IF        skmad(xm) = ' 21'
080000041229     c                   z-add     rcca21        pttota
080100041229     c                   z-add     rccf21        pctotch
080200041229     c                   add       rccn21        pctotch
080300041229     c                   add       rcci21        pctotch
080400041229     c                   z-add     rccf21        pttotf
080500041229     c                   z-add     rccn21        pttotn
080600041229     c                   z-add     rcci21        pttoti
080700041229     c     rccc21        add       rccm21        pttomc
080800041229     c                   z-add     rccd21        pttotd
080900041229     c                   z-add     rccx21        pttotx
081000041026     c                   endif
081100041026
081200041026     c                   IF        skmad(xm) = ' 22'
081300041229     c                   z-add     rcca22        pttota
081400041229     c                   z-add     rccf22        pctotch
081500041229     c                   add       rccn22        pctotch
081600041229     c                   add       rcci22        pctotch
081700041229     c                   z-add     rccf22        pttotf
081800041229     c                   z-add     rccn22        pttotn
081900041229     c                   z-add     rcci22        pttoti
082000041229     c     rccc22        add       rccm22        pttomc
082100041229     c                   z-add     rccd22        pttotd
082200041229     c                   z-add     rccx22        pttotx
082300041026     c                   endif
082400041026
082500041026     c                   IF        skmad(xm) = ' 30'
082600041229     c                   z-add     rcca30        pttota
082700041229     c                   z-add     rccf30        pctotch
082800041229     c                   add       rccn30        pctotch
082900041229     c                   add       rcci30        pctotch
083000041229     c                   z-add     rccf30        pttotf
083100041229     c                   z-add     rccn30        pttotn
083200041229     c                   z-add     rcci30        pttoti
083300041229     c     rccc30        add       rccm30        pttomc
083400041229     c                   z-add     rccd30        pttotd
083500041229     c                   z-add     rccx30        pttotx
083600041026     c                   endif
083700120914
083800120914     c                   IF        skmad(xm) = ' 50'
083900120914     c                   z-add     rcca50        pttota
084000120914     c                   z-add     rccf50        pctotch
084100120914     c                   add       rccn50        pctotch
084200120914     c                   add       rcci50        pctotch
084300120914     c                   z-add     rccf50        pttotf
084400120914     c                   z-add     rccn50        pttotn
084500120914     c                   z-add     rcci50        pttoti
084600120914     c     rccc50        add       rccm50        pttomc
084700120914     c                   z-add     rccd50        pttotd
084800120914     c                   z-add     rccx50        pttotx
084900120914     c                   endif
085000041026
085100041026     c                   IF        skmad(xm) = ' 99'
085200041229     c                   z-add     rcca99        pttota
085300041229     c                   z-add     rccf99        pctotch
085400041229     c                   add       rccn99        pctotch
085500041229     c                   add       rcci99        pctotch
085600041229     c                   z-add     rccf99        pttotf
085700041229     c                   z-add     rccn99        pttotn
085800041229     c                   z-add     rcci99        pttoti
085900041229     c     rccc99        add       rccm99        pttomc
086000041229     c                   z-add     rccd99        pttotd
086100041229     c                   z-add     rccx99        pttotx
086200041026     c                   endif
086300130515
086400130515     c                   IF        skmad(xm) = '  8'
086500130515     c                   z-add     rccaX1        pttota
086600130515     c                   z-add     rccfX1        pctotch
086700130515     c                   add       rccnX1        pctotch
086800130515     c                   add       rcciX1        pctotch
086900130515     c                   z-add     rccfX1        pttotf
087000130515     c                   z-add     rccnX1        pttotn
087100130515     c                   z-add     rcciX1        pttoti
087200130515     c     rcccX1        add       rccmX1        pttomc
087300130515     c                   z-add     rccdX1        pttotd
087400130515     c                   z-add     rccxX1        pttotx
087500130515     c                   endif
087600140409
087700140409     c                   IF        skmad(xm) = ' 88'
087800140409     c                   z-add     rccaX2        pttota
087900140409     c                   z-add     rccfX2        pctotch
088000140409     c                   add       rccnX2        pctotch
088100140409     c                   add       rcciX2        pctotch
088200140409     c                   z-add     rccfX2        pttotf
088300140409     c                   z-add     rccnX2        pttotn
088400140409     c                   z-add     rcciX2        pttoti
088500140409     c     rcccX2        add       rccmX2        pttomc
088600140409     c                   z-add     rccdX2        pttotd
088700140409     c                   z-add     rccxX2        pttotx
088800140409     c                   endif
088900151202
089000151202     c                   IF        skmad(xm) = ' 17'
089100151202     c                   z-add     rcca17        pttota
089200151202     c                   z-add     rccf17        pctotch
089300151202     c                   add       rccn17        pctotch
089400151202     c                   add       rcci17        pctotch
089500151202     c                   z-add     rccf17        pttotf
089600151202     c                   z-add     rccn17        pttotn
089700151202     c                   z-add     rcci17        pttoti
089800151202     c     rccc17        add       rccm17        pttomc
089900151202     c                   z-add     rccd17        pttotd
090000151202     c                   z-add     rccx17        pttotx
090100151202     c                   endif
090200151202
090300151202     c                   IF        skmad(xm) = ' 18'
090400151202     c                   z-add     rcca18        pttota
090500151202     c                   z-add     rccf18        pctotch
090600151202     c                   add       rccn18        pctotch
090700151202     c                   add       rcci18        pctotch
090800151202     c                   z-add     rccf18        pttotf
090900151202     c                   z-add     rccn18        pttotn
091000151202     c                   z-add     rcci18        pttoti
091100151202     c     rccc18        add       rccm18        pttomc
091200151202     c                   z-add     rccd18        pttotd
091300151202     c                   z-add     rccx18        pttotx
091400151202     c                   endif
091500041026
091600041026      * percentuale
091700041026
091800041026     c                   If        pctotch > 0
091900041104     c                   eval(h)   pcperm = (pctotch * 100) / W_pctotch
092000041026     c                   seton                                        11
092100041026     c                   else
092200041026     c                   clear                   pcperm
092300041026     c                   setoff                                       11
092400041026     c                   endif
092500041026      *
092600041229     c                   write     DND3dt2
092700041026      *
092800041026     c                   endif
092900041026      *
093000041026     c                   enddo
093100041104      * stampo la leggenda
093200041229     c                   write     DND3le3
093300041026
093400041026     c                   endsr
093500041026
093600041022      *---------------------------------------------------------------*
093700041022      * Preparazione campi e stampa del dettaglio                     *
093800041022      *---------------------------------------------------------------*
093900041022     c     SR_Prepa      BegSr
094000041025      *
094100041228     c                   clear                   WRK_dta
094200041229     c                   clear                   dtaogg
094300041228     c                   clear                   RIFERI
094400041228     c                   clear                   destin
094500041228     c                   clear                   locdes
094600041228     c                   clear                   prodes
094700041228     c                   eval      *in15 = '0'
094800041023      *
094900041023      * data richiesta
095000041023      *
095100041023     c                   clear                   WLBdat
095200041025     c                   eval      G02inv = tgddar
095300041023     c                   eval      G02err = '3'
095400041023     c                   call      'XSRDA8'
095500041023     c                   parm                    WLBdat
095600041023     c                   z-add     G02dat        p3cdac                         data ricez.comunicaz
095700041023      *
095800041023      * Oggetto
095900041023      *
096000041023     c                   clear                   p3cogg
096100041023      * ---------- SPEDIZIONE -----------
096200041023     c                   if        tgdtor = 'S'
096300041023      *
096400041228     c                   movel     tgdogg        Wspekey
096500041228     c                   movel     tgdogg        WRKspe
096600041023     c                   eval      p3cogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
096700041023     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
096800041023     c                             ' ' + WRKann
096900041023      *
097000041023     c                   endif
097100041023      * ----------    ORM     -----------
097200041023     c                   if        tgdtor = 'O'
097300041023      *
097400041228     c                   movel     tgdogg        Wormkey
097500041228     c                   movel     tgdogg        WRKorm
097600050111     c                   eval      p3cogg = 'ORM   ' + %trim(WRKpoe) +  '/' +
097700041023     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
097800041023     c                             '/' + %trim(WRKnrv)
097900041023      *
098000041023     c                   endif
098100041023      * ----------  GENERICO  -----------
098200041023     c                   if        tgdtor = 'G'
098300041023     c                   EVAL      p3cogg = TGDogg
098400041023     c                   endif
098500041023      *
098600041228      * Recupero dati relativi l'oggetto se non è un oggetto generico
098700041228
098800041228      *  spedizione
098900041228     c                   if        tgdtor = 'S'
099000041228     c     Kspedw        chain     titas30c
099100041228     c                   if        %found(titas30c)
099200041228      * Data oggetto
099300041228     c                   movel     tasaas        WRK_dta
099400041228     c                   move      tasmgs        WRK_dta
099500041228      * Destinatario
099600041228     c                   eval      destin = tasrsd
099700041228     c                   eval      locdes = taslod
099800041228     c                   eval      prodes = tasprd
099900041228     c                   eval      *in15 = '1'
100000050112      * riferimento
100100050112     c                   evalr     riferi = %editc(tasrmn:'Z')
100200041228     c                   endif
100300041228
100400041228     c                   endif
100500041228
100600041228      *  ORM
100700041228     c                   if        tgdtor = 'O'
100800041228     c     Kormw         chain     Fnorm01l
100900041228     c                   if        %found(Fnorm01l)
101000041228      * Data ritiro
101100041228     c                   move      ormdar        WRK_dta
101200041228      * Ritiro
101300050111     c                   eval      destin = ormrsr
101400041228     c                   eval      locdes = ormlor
101500041228     c                   eval      prodes = ormprr
101600041228     c                   eval      *in15 = '1'
101700041228      * riferimento
101800041228     c                   movel     ormrfa        RIFERI
101900041228     c                   endif
102000041228
102100041228     c                   endif
102200041229      * rovescio la data
102300041229     c                   If        WRK_dta > 0
102400041229     c                   clear                   WLBdat
102500041229     c                   eval      G02inv = wrk_dta
102600041229     c                   eval      G02err = '3'
102700041229     c                   call      'XSRDA8'
102800041229     c                   parm                    WLBdat
102900041229     c                   z-add     G02dat        dtaogg                         data oggetto
103000041229     c                   endif
103100041229
103200041229      *
103300041023      * note
103400041023      *
103500041023     c                   clear                   p3cno1
103600041023     c                   clear                   p3cno2
103700050111      * recupero le note se richiesto
103800041023      *
103900050111     c                   If        id2not = 'S'
104000050111
104100041026     c     ktgn          setll     Fitgn01l
104200041026     c*
104300041026     c     ktgn          reade     Fitgn01l
104400050111     c                   if        not %eof(Fitgn01l) and tgnsts <> 'N'
104500041026     c                   movel     tgnnot        p3cno1
104600041026     c                   endif
104700041026     c     ktgn          reade     Fitgn01l
104800050111     c                   if        not %eof(Fitgn01l) and tgnsts <> 'N'
104900041026     c                   movel     tgnnot        p3cno2
105000041026     c                   endif
105100050111
105200050111     c                   endif
105300050111
105400041023      * aggancio record dei totali
105500041023      *
105600041229     c                   If        W_clielwf = ' '
105700041229     C     Krcc          chain     Wfrcc00f
105800041229     c                   if        not %found(wfrcc00f)
105900041229     c                   clear                   wfrcc000
106000041023      * valorizzo i primi  campi del file
106100041229     c                   eval      Rcctpe = '1'
106200041229     c                   eval      Rccksc = codcli
106300041229     c                   eval      Rccdte = dateu
106400041230     c                   eval      Rccddr = id2AMGDCD
106500041230     c                   eval      Rccadr = id2AMGDCA
106600041230     c                   eval      Rccddc = id2AMGDHD
106700041230     c                   eval      Rccadc = id2AMGDHA
106800041229      * eval prima volta che imposto i dati per ogni Cliente
106900041229     c                   eval      W_clielwf = 'Y'
107000041229      * eval prima volta che stampo testats a cambio Cliente
107100041025     c                   eval      ST_testa = 'Y'
107200041023     c                   endif
107300041025     c                   endif
107400041229      * stampo testate se richiesta stampa del dettaglio se prima volta del Cliente o se OF
107500050110     c                   If        id2tot = ' '
107600041025     c                   If        ST_testa = 'Y' or *in90
107700041023      * testa
107800041229     c                   write     DND3t1
107900041229     c                   write     DND3tS
108000041229     c                   write     DND3t3
108100041229     c                   write     DND3t5
108200041229     c                   write     DND3t3
108300041229     c                   write     DND3t2
108400041229     c                   write     DND3t3
108500041023      *
108600041025     c   90              setoff                                       90
108700041025      *
108800041025     c                   clear                   ST_testa
108900041025      *
109000041023     c                   endif
109100041103      *
109200041103      * decodifico
109300140130      *?(già fatto in fase di selezione)?
109400140205     c                   if        yy = *zero  or  TGDmad <> sk_MAR(yy)
109500140205     c                   eval      yy = %lookup( TGDmad : sk_MAR )
109600140205     c                   endif
109700140205     c                   if        yy > *zero
109800140205     c                   eval      DecMAD = sk_MARd(yy)
109900140205     c                   else
110000140205     c                   eval      DecMAD = *all'? '
110100140205     c                   endif
110200140130      *
110300041103      * chi chiama  solo mittente destinatario cliente
110400041103
110500041103     c                   select
110600041103
110700041103     c                   when      tgdtch ='M'
110800041201     c                   eval      dectch = 'MIT/CLI'
110900041103
111000041103     c                   when      tgdtch ='D'
111100041103     c                   eval      dectch = 'DESTIN.'
111200041103
111300041103     c                   when      tgdtch ='C'
111400041201     c                   eval      dectch = 'MIT/CLI'
111500041103
111600041103     c                   other
111700041103     c                   eval      dectch = 'ALTRO  '
111800041103
111900041103     c                   endsl
112000140130      *
112100041103
112200041103     c                   if        tgdcdt <> *blanks
112300041103     c                   clear                   tibs02ds
112400041103     C                   MOVEL     'C'           t02mod
112500041103     C                   MOVEL     knsif         t02sif
112600041103     C                   MOVEL     'CHR'         t02cod
112700041103     C                   MOVEL     tgdcdt        t02ke1
112800041103      *
112900041103     C                   CALL      'TIBS02R'
113000041103     C                   PARM                    KPJBA
113100041103     C                   PARM                    TIBS02DS
113200041103      *
113300041103     C                   movel     T02uni        DCHR
113400041103     C                   movel(p)  §CHRdesc      deccdt
113500140130      *
113600041103     c                   else
113700041103     c                   clear                   deccdt
113800041103     c                   endif
113900041023      *
114000041023      * stampo la riga di dettaglio 1 + note se ci sono
114100041023      *
114200050111      *
114300041229     c                   write     DND3d03
114400041023      *
114500041229     c                   if        P3cno2 <> *blanks or *in15
114600041229     c                   write     DND3d04
114700041229     c                   endif
114800041229      * se la seconda nota è valorizzata cerco e stampo le note successive
114900050111     c                   If        id2not = 'S'
115000050111
115100041229     c                   dow       p3cno2 <> *blanks
115200041229     c                   clear                   p3cno2
115300041229     c     ktgn          reade     Fitgn01l
115400050111     c                   if        not %eof(Fitgn01l) and tgnsts <> 'N'
115500041229     c                   movel     tgnnot        p3cno2
115600041229     c                   setoff                                       15
115700041229      * se è OF
115800041229     c                   If        *in90
115900041229      * testa
116000041229     c                   write     DND3t1
116100041229     c                   write     DND3tS
116200041229     c                   write     DND3t3
116300041229     c                   write     DND3t5
116400041229     c                   write     DND3t3
116500041229     c                   write     DND3t2
116600041229     c                   write     DND3t3
116700041229     c                   setoff                                       90
116800041229     c                   endif
116900041229      *
117000041229     c                   write     DND3d04
117100041230     c                   endif
117200041229     c                   enddo
117300050111     c                   endif
117400041230     c                   endif
117500041229
117600041023      *
117700041023      * C A L C O L I
117800041023      *
117900041026
118000041026      * mi salvo la schiera dei motivi d'apertura per p.o.
118100041026
118200140130      *
118300041026     c     tgdmad        lookup    skmad                                  30
118400041026      *
118500041026     c                   if        not %found
118600041026     c                   add       1             mm
118700041026     c                   eval      skmad(mm) = tgdmad
118800041026     c                   endif
118900041023
119000041023      * Aperte suddivise per motivo
119100041023
119200041023     c                   If        tgdcdt = *blanks
119300041023
119400140409     c                   Select
119500140409     c                   when      TGDmad = '  1'
119600041229     c                   add       1             rcca01
119700041023
119800140409     c                   when      TGDmad = '  2'
119900041229     c                   add       1             rcca02
120000041023
120100140409     c                   when      TGDmad = '  3'
120200041229     c                   add       1             rcca03
120300041023
120400140409     c                   when      TGDmad = '  5'
120500041229     c                   add       1             rcca05
120600041023
120700140409     c                   when      TGDmad = '  6'
120800041229     c                   add       1             rcca06
120900041023
121000140409     c                   when      TGDmad = '  7'
121100041229     c                   add       1             rcca07
121200041023
121300140409     c                   when      TGDmad = ' 10'
121400041229     c                   add       1             rcca10
121500041023
121600140409     c                   when      TGDmad = ' 11'
121700041229     c                   add       1             rcca11
121800041023
121900140409     c                   when      TGDmad = ' 12'
122000041229     c                   add       1             rcca12
122100041023
122200140409     c                   when      TGDmad = ' 13'
122300041229     c                   add       1             rcca13
122400120914
122500140409     c                   when      TGDmad = ' 14'
122600120914     c                   add       1             rcca14
122700120914
122800140409     c                   when      TGDmad = ' 15'
122900120914     c                   add       1             rcca15
123000120914
123100140409     c                   when      TGDmad = ' 16'
123200120914     c                   add       1             rcca16
123300041023
123400140409     c                   when      TGDmad = ' 20'
123500041229     c                   add       1             rcca20
123600041023
123700140409     c                   when      TGDmad = ' 21'
123800041229     c                   add       1             rcca21
123900041023
124000140409     c                   when      TGDmad = ' 22'
124100041229     c                   add       1             rcca22
124200041023
124300140409     c                   when      TGDmad = ' 30'
124400041229     c                   add       1             rcca30
124500120914
124600140409     c                   when      TGDmad = ' 50'
124700120914     c                   add       1             rcca50
124800041023
124900140409     c                   when      TGDmad = ' 99'
125000041229     c                   add       1             rcca99
125100130515
125200140409     c                   when      TGDmad = '  8'
125300130515     c                   add       1             rccaX1
125400140409
125500140409     c                   when      TGDmad = ' 88'
125600140409     c                   add       1             rccaX2
125700151202
125800151202     c                   when      TGDmad = ' 17'
125900151202     c                   add       1             rcca17
126000151202
126100151202     c                   when      TGDmad = ' 18'
126200151202     c                   add       1             rcca18
126300140409     c                   EndSl
126400041023
126500041023     c                   endif
126600041023
126700041023      * CHIUSI Fondati     suddivisi per motivo
126800041023
126900041023     c                   If        tgdcdt = '  F'
127000041023      *
127100140409     c                   Select
127200140409     c                   when      TGDmad = '  1'
127300041229     c                   add       1             rccf01
127400041023
127500140409     c                   when      TGDmad = '  2'
127600041229     c                   add       1             rccf02
127700041023
127800140409     c                   when      TGDmad = '  3'
127900041229     c                   add       1             rccf03
128000041023
128100140409     c                   when      TGDmad = '  5'
128200041229     c                   add       1             rccf05
128300041023
128400140409     c                   when      TGDmad = '  6'
128500041229     c                   add       1             rccf06
128600041023
128700140409     c                   when      TGDmad = '  7'
128800041229     c                   add       1             rccf07
128900041023
129000140409     c                   when      TGDmad = ' 10'
129100041229     c                   add       1             rccf10
129200041023
129300140409     c                   when      TGDmad = ' 11'
129400041229     c                   add       1             rccf11
129500041023
129600140409     c                   when      TGDmad = ' 12'
129700041229     c                   add       1             rccf12
129800041023
129900140409     c                   when      TGDmad = ' 13'
130000041229     c                   add       1             rccf13
130100120914
130200140409     c                   when      TGDmad = ' 14'
130300120914     c                   add       1             rccf14
130400120914
130500140409     c                   when      TGDmad = ' 15'
130600120914     c                   add       1             rccf15
130700120914
130800140409     c                   when      TGDmad = ' 16'
130900120914     c                   add       1             rccf16
131000041023
131100140409     c                   when      TGDmad = ' 20'
131200041229     c                   add       1             rccf20
131300041023
131400140409     c                   when      TGDmad = ' 21'
131500041229     c                   add       1             rccf21
131600041023
131700140409     c                   when      TGDmad = ' 22'
131800041229     c                   add       1             rccf22
131900041023
132000140409     c                   when      TGDmad = ' 30'
132100041229     c                   add       1             rccf30
132200120914
132300140409     c                   when      TGDmad = ' 50'
132400120914     c                   add       1             rccf50
132500041023
132600140409     c                   when      TGDmad = ' 99'
132700041229     c                   add       1             rccf99
132800130515
132900140409     c                   when      TGDmad = '  8'
133000130515     c                   add       1             rccfX1
133100140409
133200140409     c                   when      TGDmad = ' 88'
133300140409     c                   add       1             rccfX2
133400151202
133500151202     c                   when      TGDmad = ' 17'
133600151202     c                   add       1             rccf17
133700151202
133800151202     c                   when      TGDmad = ' 18'
133900151202     c                   add       1             rccf18
134000140409     c                   EndSl
134100041023      *
134200041023     c                   endif
134300041023
134400041023      * CHIUSI InFondati     suddivisi per motivo
134500041023
134600041023     c                   If        tgdcdt = '  N'
134700041023      *
134800140409     c                   Select
134900140409     c                   when      TGDmad = '  1'
135000041229     c                   add       1             rccn01
135100041023
135200140409     c                   when      TGDmad = '  2'
135300041229     c                   add       1             rccn02
135400041023
135500140409     c                   when      TGDmad = '  3'
135600041229     c                   add       1             rccn03
135700041023
135800140409     c                   when      TGDmad = '  5'
135900041229     c                   add       1             rccn05
136000041023
136100140409     c                   when      TGDmad = '  6'
136200041229     c                   add       1             rccn06
136300041023
136400140409     c                   when      TGDmad = '  7'
136500041229     c                   add       1             rccn07
136600041023
136700140409     c                   when      TGDmad = ' 10'
136800041229     c                   add       1             rccn10
136900041023
137000140409     c                   when      TGDmad = ' 11'
137100041229     c                   add       1             rccn11
137200041023
137300140409     c                   when      TGDmad = ' 12'
137400041229     c                   add       1             rccn12
137500041023
137600140409     c                   when      TGDmad = ' 13'
137700041229     c                   add       1             rccn13
137800120914
137900140409     c                   when      TGDmad = ' 14'
138000120914     c                   add       1             rccn14
138100120914
138200140409     c                   when      TGDmad = ' 15'
138300120914     c                   add       1             rccn15
138400120914
138500140409     c                   when      TGDmad = ' 16'
138600120914     c                   add       1             rccn16
138700041023
138800140409     c                   when      TGDmad = ' 20'
138900041229     c                   add       1             rccn20
139000041023
139100140409     c                   when      TGDmad = ' 21'
139200041229     c                   add       1             rccn21
139300041023
139400140409     c                   when      TGDmad = ' 22'
139500041229     c                   add       1             rccn22
139600041023
139700140409     c                   when      TGDmad = ' 30'
139800041229     c                   add       1             rccn30
139900120914
140000140409     c                   when      TGDmad = ' 50'
140100120914     c                   add       1             rccn50
140200041023
140300140409     c                   when      TGDmad = ' 99'
140400041229     c                   add       1             rccn99
140500130515
140600140409     c                   when      TGDmad = '  8'
140700130515     c                   add       1             rccnX1
140800140409
140900140409     c                   when      TGDmad = ' 88'
141000140409     c                   add       1             rccnX2
141100151202
141200151202     c                   when      TGDmad = ' 17'
141300151202     c                   add       1             rccn17
141400151202
141500151202     c                   when      TGDmad = ' 18'
141600151202     c                   add       1             rccn18
141700140409     c                   EndSl
141800041023      *
141900041023     c                   endif
142000041023
142100041023      * CHIUSI InFormativi   suddivisi per motivo
142200041023
142300041023     c                   If        tgdcdt = '  I'
142400041023      *
142500140409     c                   Select
142600140409     c                   when      TGDmad = '  1'
142700041229     c                   add       1             rcci01
142800041023
142900140409     c                   when      TGDmad = '  2'
143000041229     c                   add       1             rcci02
143100041023
143200140409     c                   when      TGDmad = '  3'
143300041229     c                   add       1             rcci03
143400041023
143500140409     c                   when      TGDmad = '  5'
143600041229     c                   add       1             rcci05
143700041023
143800140409     c                   when      TGDmad = '  6'
143900041229     c                   add       1             rcci06
144000041023
144100140409     c                   when      TGDmad = '  7'
144200041229     c                   add       1             rcci07
144300041023
144400140409     c                   when      TGDmad = ' 10'
144500041229     c                   add       1             rcci10
144600041023
144700140409     c                   when      TGDmad = ' 11'
144800041229     c                   add       1             rcci11
144900041023
145000140409     c                   when      TGDmad = ' 12'
145100041229     c                   add       1             rcci12
145200041023
145300140409     c                   when      TGDmad = ' 13'
145400041229     c                   add       1             rcci13
145500120914
145600140409     c                   when      TGDmad = ' 14'
145700120914     c                   add       1             rcci14
145800120914
145900140409     c                   when      TGDmad = ' 15'
146000120914     c                   add       1             rcci15
146100120914
146200140409     c                   when      TGDmad = ' 16'
146300120914     c                   add       1             rcci16
146400041023
146500140409     c                   when      TGDmad = ' 20'
146600041229     c                   add       1             rcci20
146700041023
146800140409     c                   when      TGDmad = ' 21'
146900041229     c                   add       1             rcci21
147000041023
147100140409     c                   when      TGDmad = ' 22'
147200041229     c                   add       1             rcci22
147300041023
147400140409     c                   when      TGDmad = ' 30'
147500041229     c                   add       1             rcci30
147600120914
147700140409     c                   when      TGDmad = ' 50'
147800120914     c                   add       1             rcci50
147900041023
148000140409     c                   when      TGDmad = ' 99'
148100041229     c                   add       1             rcci99
148200130515
148300140409     c                   when      TGDmad = '  8'
148400130515     c                   add       1             rcciX1
148500140409
148600140409     c                   when      TGDmad = ' 88'
148700140409     c                   add       1             rcciX2
148800151202
148900151202     c                   when      TGDmad = ' 17'
149000151202     c                   add       1             rcci17
149100151202
149200151202     c                   when      TGDmad = ' 18'
149300151202     c                   add       1             rcci18
149400140409     c                   EndSl
149500041023      *
149600041023     c                   endif
149700041023
149800041023      * Chiamante Destinatario suddivisi per motivo
149900041023
150000041023     c                   If        tgdtch = 'D'
150100041023      *
150200140409     c                   Select
150300140409     c                   when      TGDmad = '  1'
150400041229     c                   add       1             rccd01
150500041023
150600140409     c                   when      TGDmad = '  2'
150700041229     c                   add       1             rccd02
150800041023
150900140409     c                   when      TGDmad = '  3'
151000041229     c                   add       1             rccd03
151100041023
151200140409     c                   when      TGDmad = '  5'
151300041229     c                   add       1             rccd05
151400041023
151500140409     c                   when      TGDmad = '  6'
151600041229     c                   add       1             rccd06
151700041023
151800140409     c                   when      TGDmad = '  7'
151900041229     c                   add       1             rccd07
152000041023
152100140409     c                   when      TGDmad = ' 10'
152200041229     c                   add       1             rccd10
152300041023
152400140409     c                   when      TGDmad = ' 11'
152500041229     c                   add       1             rccd11
152600041023
152700140409     c                   when      TGDmad = ' 12'
152800041229     c                   add       1             rccd12
152900041023
153000140409     c                   when      TGDmad = ' 13'
153100041229     c                   add       1             rccd13
153200120914
153300140409     c                   when      TGDmad = ' 14'
153400120914     c                   add       1             rccd14
153500120914
153600140409     c                   when      TGDmad = ' 15'
153700120914     c                   add       1             rccd15
153800120914
153900140409     c                   when      TGDmad = ' 16'
154000120914     c                   add       1             rccd16
154100041023
154200140409     c                   when      TGDmad = ' 20'
154300041229     c                   add       1             rccd20
154400041023
154500140409     c                   when      TGDmad = ' 21'
154600041229     c                   add       1             rccd21
154700041023
154800140409     c                   when      TGDmad = ' 22'
154900041229     c                   add       1             rccd22
155000041023
155100140409     c                   when      TGDmad = ' 30'
155200041229     c                   add       1             rccd30
155300120914
155400140409     c                   when      TGDmad = ' 50'
155500120914     c                   add       1             rccd50
155600041023
155700140409     c                   when      TGDmad = ' 99'
155800041229     c                   add       1             rccd99
155900130515
156000140409     c                   when      TGDmad = '  8'
156100130515     c                   add       1             rccdX1
156200140409
156300140409     c                   when      TGDmad = ' 88'
156400140409     c                   add       1             rccdX2
156500151202
156600151202     c                   when      TGDmad = ' 17'
156700151202     c                   add       1             rccd17
156800151202
156900151202     c                   when      TGDmad = ' 18'
157000151202     c                   add       1             rccd18
157100140409     c                   EndSl
157200041023      *
157300041023     c                   endif
157400041023
157500041023      * Chiamante Mittente     suddivisi per motivo
157600041023
157700041023     c                   If        tgdtch = 'M'
157800041023      *
157900140409     c                   Select
158000140409     c                   when      TGDmad = '  1'
158100041229     c                   add       1             rccm01
158200041023
158300140409     c                   when      TGDmad = '  2'
158400041229     c                   add       1             rccm02
158500041023
158600140409     c                   when      TGDmad = '  3'
158700041229     c                   add       1             rccm03
158800041023
158900140409     c                   when      TGDmad = '  5'
159000041229     c                   add       1             rccm05
159100041023
159200140409     c                   when      TGDmad = '  6'
159300041229     c                   add       1             rccm06
159400041023
159500140409     c                   when      TGDmad = '  7'
159600041229     c                   add       1             rccm07
159700041023
159800140409     c                   when      TGDmad = ' 10'
159900041229     c                   add       1             rccm10
160000041023
160100140409     c                   when      TGDmad = ' 11'
160200041229     c                   add       1             rccm11
160300041023
160400140409     c                   when      TGDmad = ' 12'
160500041229     c                   add       1             rccm12
160600041023
160700140409     c                   when      TGDmad = ' 13'
160800041229     c                   add       1             rccm13
160900120914
161000140409     c                   when      TGDmad = ' 14'
161100120914     c                   add       1             rccm14
161200120914
161300140409     c                   when      TGDmad = ' 15'
161400120914     c                   add       1             rccm15
161500120914
161600140409     c                   when      TGDmad = ' 16'
161700120914     c                   add       1             rccm16
161800041023
161900140409     c                   when      TGDmad = ' 20'
162000041229     c                   add       1             rccm20
162100041023
162200140409     c                   when      TGDmad = ' 21'
162300041229     c                   add       1             rccm21
162400041023
162500140409     c                   when      TGDmad = ' 22'
162600041229     c                   add       1             rccm22
162700041023
162800140409     c                   when      TGDmad = ' 30'
162900041229     c                   add       1             rccm30
163000120914
163100140409     c                   when      TGDmad = ' 50'
163200120914     c                   add       1             rccm50
163300041023
163400140409     c                   when      TGDmad = ' 99'
163500041229     c                   add       1             rccm99
163600130515
163700140409     c                   when      TGDmad = '  8'
163800130515     c                   add       1             rccmX1
163900140409
164000140409     c                   when      TGDmad = ' 88'
164100140409     c                   add       1             rccmX2
164200151202
164300151202     c                   when      TGDmad = ' 17'
164400151202     c                   add       1             rccm17
164500151202
164600151202     c                   when      TGDmad = ' 18'
164700151202     c                   add       1             rccm18
164800140409     c                   EndSl
164900041023      *
165000041023     c                   endif
165100041023
165200041023      * Chiamante Cliente      suddivisi per motivo
165300041023
165400041023     c                   If        tgdtch = 'C'
165500041023      *
165600140409     c                   Select
165700140409     c                   when      TGDmad = '  1'
165800041229     c                   add       1             rccc01
165900041023
166000140409     c                   when      TGDmad = '  2'
166100041229     c                   add       1             rccc02
166200041023
166300140409     c                   when      TGDmad = '  3'
166400041229     c                   add       1             rccc03
166500041023
166600140409     c                   when      TGDmad = '  5'
166700041229     c                   add       1             rccc05
166800041023
166900140409     c                   when      TGDmad = '  6'
167000041229     c                   add       1             rccc06
167100041023
167200140409     c                   when      TGDmad = '  7'
167300041229     c                   add       1             rccc07
167400041023
167500140409     c                   when      TGDmad = ' 10'
167600041229     c                   add       1             rccc10
167700041023
167800140409     c                   when      TGDmad = ' 11'
167900041229     c                   add       1             rccc11
168000041023
168100140409     c                   when      TGDmad = ' 12'
168200041229     c                   add       1             rccc12
168300041023
168400140409     c                   when      TGDmad = ' 13'
168500041229     c                   add       1             rccc13
168600120914
168700140409     c                   when      TGDmad = ' 14'
168800120914     c                   add       1             rccc14
168900120914
169000140409     c                   when      TGDmad = ' 15'
169100120914     c                   add       1             rccc15
169200120914
169300140409     c                   when      TGDmad = ' 16'
169400120914     c                   add       1             rccc16
169500041023
169600140409     c                   when      TGDmad = ' 20'
169700041229     c                   add       1             rccc20
169800041023
169900140409     c                   when      TGDmad = ' 21'
170000041229     c                   add       1             rccc21
170100041023
170200140409     c                   when      TGDmad = ' 22'
170300041229     c                   add       1             rccc22
170400041023
170500140409     c                   when      TGDmad = ' 30'
170600041229     c                   add       1             rccc30
170700120914
170800140409     c                   when      TGDmad = ' 50'
170900120914     c                   add       1             rccc50
171000041023
171100140409     c                   when      TGDmad = ' 99'
171200041229     c                   add       1             rccc99
171300130515
171400140409     c                   when      TGDmad = '  8'
171500130515     c                   add       1             rcccX1
171600140409
171700140409     c                   when      TGDmad = ' 88'
171800140409     c                   add       1             rcccX2
171900151202
172000151202     c                   when      TGDmad = ' 17'
172100151202     c                   add       1             rccc17
172200151202
172300151202     c                   when      TGDmad = ' 18'
172400151202     c                   add       1             rccc18
172500140409     c                   EndSl
172600041023      *
172700041023     c                   endif
172800041023
172900041023      * Chiamante Altro        suddivisi per motivo
173000041023
173100041023     c                   If        tgdtch<>'D' and tgdtch<>'M' and tgdtch<>'C'
173200041023      *
173300140409     c                   Select
173400140409     c                   when      TGDmad = '  1'
173500041229     c                   add       1             rccx01
173600041023
173700140409     c                   when      TGDmad = '  2'
173800041229     c                   add       1             rccx02
173900041023
174000140409     c                   when      TGDmad = '  3'
174100041229     c                   add       1             rccx03
174200041023
174300140409     c                   when      TGDmad = '  5'
174400041229     c                   add       1             rccx05
174500041023
174600140409     c                   when      TGDmad = '  6'
174700041229     c                   add       1             rccx06
174800041023
174900140409     c                   when      TGDmad = '  7'
175000041229     c                   add       1             rccx07
175100041023
175200140409     c                   when      TGDmad = ' 10'
175300041229     c                   add       1             rccx10
175400041023
175500140409     c                   when      TGDmad = ' 11'
175600041229     c                   add       1             rccx11
175700041023
175800140409     c                   when      TGDmad = ' 12'
175900041229     c                   add       1             rccx12
176000041023
176100140409     c                   when      TGDmad = ' 13'
176200041229     c                   add       1             rccx13
176300120914
176400140409     c                   when      TGDmad = ' 14'
176500120914     c                   add       1             rccx14
176600120914
176700140409     c                   when      TGDmad = ' 15'
176800120914     c                   add       1             rccx15
176900120914
177000140409     c                   when      TGDmad = ' 16'
177100120914     c                   add       1             rccx16
177200041023
177300140409     c                   when      TGDmad = ' 20'
177400041229     c                   add       1             rccx20
177500041023
177600140409     c                   when      TGDmad = ' 21'
177700041229     c                   add       1             rccx21
177800041023
177900140409     c                   when      TGDmad = ' 22'
178000041229     c                   add       1             rccx22
178100041023
178200140409     c                   when      TGDmad = ' 30'
178300041229     c                   add       1             rccx30
178400120914
178500140409     c                   when      TGDmad = ' 50'
178600120914     c                   add       1             rccx50
178700041023
178800140409     c                   when      TGDmad = ' 99'
178900041229     c                   add       1             rccx99
179000130515
179100140409     c                   when      TGDmad = '  8'
179200130515     c                   add       1             rccxX1
179300140409
179400140409     c                   when      TGDmad = ' 88'
179500140409     c                   add       1             rccxX2
179600151202
179700151202     c                   when      TGDmad = ' 17'
179800151202     c                   add       1             rccx17
179900151202
180000151202     c                   when      TGDmad = ' 18'
180100151202     c                   add       1             rccx18
180200140409     c                   EndSl
180300041023      *
180400041023     c                   endif
180500041023      *
180600041023     c                   endsr
180700041022      *
180800041023      *---------------------------------------------------------------*
180900041023      * Stampa della testata                                          *
181000041023      *---------------------------------------------------------------*
181100041023     c     SR_Testa      BegSr
181200041026      *
181300041227     c* Scelta
181400041227
181500041228     c                   if        id2sce='1'
181600041227     c                   eval      dessce='Un solo cliente'
181700041023     c                   endif
181800041228     c                   if        id2sce='2'
181900041227     c                   eval      dessce='Un cliente unificante'
182000041023     c                   endif
182100041023      * p.o. responsabile
182200041227     c                   If        id2prs > 0
182300041227     c     id2prs        chain     azorg01l
182400041230     c                   movel     orgdes        desprs
182500041023     c                   endif
182600041023      *
182700041023      * p.o. apertura
182800041227     c                   If        id2fil > 0
182900041227     c     id2fil        chain     azorg01l
183000041230     c                   movel     orgdes        desfil
183100041023     c                   endif
183200041023      *
183300041229     c                   write     DND3t1
183400041229     c                   write     DND3ts
183500041229     c                   write     DND302
183600041023
183700041023     c                   endsr
183800041023      *
183900041022      *---------------------------------------------------------------*
184000041022      * Reperimento Dati del job (Utente/Operativi)                   *
184100041022      *---------------------------------------------------------------*
184200041022     c     DatiJob       BegSr
184300041022      *
184400041022     c     *dtaara       define    §azute        azuteds
184500041022     c     *dtaara       define    §datiute      ddatiute
184600041022      *
184700041022     c                   in(E)     *dtaara
184800041022     c                   IF        %ERROR or RSUT = *blanks
184900041022     c                   clear                   Tibs34Ds
185000041022     c                   call      'TIBS34R'
185100041022     c                   parm                    Tibs34Ds
185200041022     c                   in        *dtaara
185300041022     c                   ENDIF
185400041023      * - file FITGR01L
185500041023     c     ktgr          klist
185600041023     c                   kfld                    tgdant
185700041023     c                   kfld                    tgdnut
185800041023     c                   kfld                    tgdpdt
185900041023     c                   kfld                    K_tgrtri
186000041026      * - file FITGN01L
186100041026     c     ktgN          klist
186200041026     c                   kfld                    tgdant
186300041026     c                   kfld                    tgdnut
186400041026     c                   kfld                    tgdpdt
186500041228      * - file TITAS30C
186600041228     c     kspedw        klist
186700041228     c                   kfld                    W_aas
186800041228     c                   kfld                    W_lnp
186900041228     c                   kfld                    W_nrs
187000041228     c                   kfld                    W_nsp
187100041228      * - file FNORM01L
187200041228     c     kormw         klist
187300041228     c                   kfld                    W_poe
187400041228     c                   kfld                    W_nsr
187500041228     c                   kfld                    W_nor
187600041228     c                   kfld                    W_nrv
187700041229      * - file WFRCC00F
187800041229     c     krcc          klist
187900041229     c                   kfld                    W_trc
188000041229     c                   kfld                    codcli
188100041022      *
188200041022     c                   EndSr
188300041020
188400041022      *---------------------------------------------------------------*
188500041022      * Routine Iniziale                                              *
188600041022      *---------------------------------------------------------------*
188700041022     c     *InzSR        BegSR
188800041022      *
188900041022     c     *Entry        plist
189000041022     c                   parm                    KPJBA
189100041230     c                   movel     kpjbu         fidnD2ds
189200041022      * Reperisco data corrente
189300041022     c                   time                    w0140
189400041023     c                   move      w0140         wDate
189500041022     c                   clear                   WLBdat
189600041023     c                   eval      G02dat = wDate
189700041022     c                   call      'XSRDA8'
189800041022     c                   parm                    WLBdat
189900041022     c                   z-add     G02inv        DateU
190000041022      *
190100041022      * Reperisco dati job
190200041022     c                   exsr      DatiJob
190300041022
190400041022
190500041022      * controllo il sistema informativo dove sono per reperire
190600041022      * la libreria
190700041231      *
190800041026     c                   If        %subst(knsif:7:1) <> 'P'
190900041231     c                   Eval      wlib = 'GAITRAAZM '
191000041231     c                   Eval      wlib1 = 'GAITRAGRU '
191100041022     c                   EndIf
191200041231      *
191300041022     c                   If        %subst(knsif:7:1) = 'P'
191400120914     c                   Eval      wlib = 'GAITRAAZP'
191500041231     c                   Eval      wlib1 = 'GAITRAGRPS'
191600041022     c                   EndIf
191700041231
191800041231      * duplico il file di comodo in qtemp
191900041231     c                   clear                   comman
192000041231     c                   movel(p)  cmd(1)        comman
192100041231     c                   Eval      %Subst(comman:33:10) = wlib
192200041231     c                   call      'QCMDEXC'
192300041231     c                   parm                    comman
192400041231     c                   parm                    lenght
192500041231      * apro il file di comodo per la stampa
192600041231     c                   open      wfrcc00f
192700041231
192800041228
192900041228      * apro il file titas
193000041228     c                   Clear                   comman
193100041231     c                   Movel(p)  cmd(2)        comman
193200041228     c                   Eval      %Subst(comman:13:8) = 'TITAS30C'
193300041231     c                   Eval      %Subst(comman:30:10) = wlib1
193400041228     c                   Eval      comman =
193500041228     c                             %trim(comman) + '/TITAS30C)'
193600041228     c                   Call      'QCMDEXC'                            99
193700041228     c                   Parm                    comman
193800041228     c                   Parm                    lenght
193900041231      *
194000041228     c                   open      titas30c
194100140205
194200140205      /free
194300140205
194400140205         // -?Caricamento dati tab. "MAR" = Motivi Apertura Reclami?
194500140205         clear  yy;
194600140205         clear  sk_MAR;
194700140205         clear  sk_MARd;
194800140210         clear  sk_MARs;
194900140205         clear  KeyTNTBE01;
195000140205         k_TBEcod = 'MAR';
195100140205         setll  %kds( keyTNTBE01 )  TNTBE000;
195200140205         reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
195300140205         DoW  Not  %eof(TNTBE01L);
195400140205           if  TBEatb = *blank;
195500140205             dMAR = TBEuni;
195600140205             yy += 1;
195700140205             sk_MAR(yy)  = TBEke1;
195800140205             sk_MARd(yy) = §MARdesc;
195900140210             sk_MARs(yy) = §MAReSt;
196000140205           endif;
196100140205           reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
196200140205         EndDo;
196300140205
196400140205      /end-free
196500140205      *
196600050111      * se devo stampare il P.O. responsabile lascio spento il 16
196700050111     c                   If        Id2por <> 'S'
196800050111     c                   seton                                        16
196900050111     c                   endif
197000050117      * se devo stampare le note lascio spento il 18
197100050117     c                   If        Id2not <> 'S'
197200050117     c                   seton                                        18
197300050117     c                   endif
197400050113      * se devo stampare il dettaglio per figlio
197500050113     c                   If        Id2det =  'S'
197600050113     c                   seton                                        17
197700050113     c                   endif
197800041022
197900041022     C                   ENDSR
198000041231      *
198100041231
198200041022**   cmd - comando
198300041231CRTDUPOBJ OBJ(WFRCC00F) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
198400041228OVRDBF FILE(        ) TOFILE(
