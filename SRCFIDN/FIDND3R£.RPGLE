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
048100120914     c                             rcca16 + rcca50
048200041026     c                   z-add     W_pttota      pttota
048300041026      * totali chiusi fondati
048400041230     c                   eval      W_pttotf= rccf01 + rccf02 + rccf03 + rccf05 +
048500041230     c                             rccf06 + rccf07 + rccf10 + rccf11 + rccf12 +
048600041230     c                             rccf13 + rccf20 + rccf21 + rccf22 + rccf30 +
048700120914     c                             rccf99 + rccfx1 + rccfx2 + rccf14 + rccf15 +
048800120914     c                             rccf16 + rccf50
048900041026     c                   z-add     W_pttotf      pttotf
049000041026      * totali chiusi infondati
049100041230     c                   eval      W_pttotn= rccn01 + rccn02 + rccn03 + rccn05 +
049200041230     c                             rccn06 + rccn07 + rccn10 + rccn11 + rccn12 +
049300041230     c                             rccn13 + rccn20 + rccn21 + rccn22 + rccn30 +
049400120914     c                             rccn99 + rccnx1 + rccnx2 + rccn14 + rccn15 +
049500120914     c                             rccn16 + rccn50
049600041026     c                   z-add     W_pttotn      pttotn
049700041026      * totali chiusi informativi
049800041230     c                   eval      W_pttoti= rcci01 + rcci02 + rcci03 + rcci05 +
049900041230     c                             rcci06 + rcci07 + rcci10 + rcci11 + rcci12 +
050000041230     c                             rcci13 + rcci20 + rcci21 + rcci22 + rcci30 +
050100120914     c                             rcci99 + rccix1 + rccix2 + rcci14 + rcci15 +
050200120914     c                             rcci16 + rcci50
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
051400120914     c                             rccm16 + rccm50
051500041130     c                   z-add     W_pttomc      pttomc
051600041026      * totali per chi chiama  Destinatario
051700041230     c                   eval      W_pttotd= rccd01 + rccd02 + rccd03 + rccd05 +
051800041230     c                             rccd06 + rccd07 + rccd10 + rccd11 + rccd12 +
051900041230     c                             rccd13 + rccd20 + rccd21 + rccd22 + rccd30 +
052000120914     c                             rccd99 + rccdx1 + rccdx2 + rccd14 + rccd15 +
052100120914     c                             rccd16 + rccd50
052200041026     c                   z-add     W_pttotd      pttotd
052300041026      * totali per chi chiama  Cliente
052400041230     c                   eval      W_pttotc= rccc01 + rccc02 + rccc03 + rccc05 +
052500041230     c                             rccc06 + rccc07 + rccc10 + rccc11 + rccc12 +
052600041230     c                             rccc13 + rccc20 + rccc21 + rccc22 + rccc30 +
052700120914     c                             rccc99 + rcccx1 + rcccx2 + rccc14 + rccc15 +
052800120914     c                             rccc16 + rccc50
052900041130     c                   add       W_pttotc      pttomc
053000041026      * totali per chi chiama  Altro
053100041230     c                   eval      W_pttotx= rccx01 + rccx02 + rccx03 + rccx05 +
053200041230     c                             rccx06 + rccx07 + rccx10 + rccx11 + rccx12 +
053300041230     c                             rccx13 + rccx20 + rccx21 + rccx22 + rccx30 +
053400120914     c                             rccx99 + rccxx1 + rccxx2 + rccx14 + rccx15 +
053500120914     c                             rccx16 + rccx50
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
088900041026
089000041026      * percentuale
089100041026
089200041026     c                   If        pctotch > 0
089300041104     c                   eval(h)   pcperm = (pctotch * 100) / W_pctotch
089400041026     c                   seton                                        11
089500041026     c                   else
089600041026     c                   clear                   pcperm
089700041026     c                   setoff                                       11
089800041026     c                   endif
089900041026      *
090000041229     c                   write     DND3dt2
090100041026      *
090200041026     c                   endif
090300041026      *
090400041026     c                   enddo
090500041104      * stampo la leggenda
090600041229     c                   write     DND3le3
090700041026
090800041026     c                   endsr
090900041026
091000041022      *---------------------------------------------------------------*
091100041022      * Preparazione campi e stampa del dettaglio                     *
091200041022      *---------------------------------------------------------------*
091300041022     c     SR_Prepa      BegSr
091400041025      *
091500041228     c                   clear                   WRK_dta
091600041229     c                   clear                   dtaogg
091700041228     c                   clear                   RIFERI
091800041228     c                   clear                   destin
091900041228     c                   clear                   locdes
092000041228     c                   clear                   prodes
092100041228     c                   eval      *in15 = '0'
092200041023      *
092300041023      * data richiesta
092400041023      *
092500041023     c                   clear                   WLBdat
092600041025     c                   eval      G02inv = tgddar
092700041023     c                   eval      G02err = '3'
092800041023     c                   call      'XSRDA8'
092900041023     c                   parm                    WLBdat
093000041023     c                   z-add     G02dat        p3cdac                         data ricez.comunicaz
093100041023      *
093200041023      * Oggetto
093300041023      *
093400041023     c                   clear                   p3cogg
093500041023      * ---------- SPEDIZIONE -----------
093600041023     c                   if        tgdtor = 'S'
093700041023      *
093800041228     c                   movel     tgdogg        Wspekey
093900041228     c                   movel     tgdogg        WRKspe
094000041023     c                   eval      p3cogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
094100041023     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
094200041023     c                             ' ' + WRKann
094300041023      *
094400041023     c                   endif
094500041023      * ----------    ORM     -----------
094600041023     c                   if        tgdtor = 'O'
094700041023      *
094800041228     c                   movel     tgdogg        Wormkey
094900041228     c                   movel     tgdogg        WRKorm
095000050111     c                   eval      p3cogg = 'ORM   ' + %trim(WRKpoe) +  '/' +
095100041023     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
095200041023     c                             '/' + %trim(WRKnrv)
095300041023      *
095400041023     c                   endif
095500041023      * ----------  GENERICO  -----------
095600041023     c                   if        tgdtor = 'G'
095700041023     c                   EVAL      p3cogg = TGDogg
095800041023     c                   endif
095900041023      *
096000041228      * Recupero dati relativi l'oggetto se non è un oggetto generico
096100041228
096200041228      *  spedizione
096300041228     c                   if        tgdtor = 'S'
096400041228     c     Kspedw        chain     titas30c
096500041228     c                   if        %found(titas30c)
096600041228      * Data oggetto
096700041228     c                   movel     tasaas        WRK_dta
096800041228     c                   move      tasmgs        WRK_dta
096900041228      * Destinatario
097000041228     c                   eval      destin = tasrsd
097100041228     c                   eval      locdes = taslod
097200041228     c                   eval      prodes = tasprd
097300041228     c                   eval      *in15 = '1'
097400050112      * riferimento
097500050112     c                   evalr     riferi = %editc(tasrmn:'Z')
097600041228     c                   endif
097700041228
097800041228     c                   endif
097900041228
098000041228      *  ORM
098100041228     c                   if        tgdtor = 'O'
098200041228     c     Kormw         chain     Fnorm01l
098300041228     c                   if        %found(Fnorm01l)
098400041228      * Data ritiro
098500041228     c                   move      ormdar        WRK_dta
098600041228      * Ritiro
098700050111     c                   eval      destin = ormrsr
098800041228     c                   eval      locdes = ormlor
098900041228     c                   eval      prodes = ormprr
099000041228     c                   eval      *in15 = '1'
099100041228      * riferimento
099200041228     c                   movel     ormrfa        RIFERI
099300041228     c                   endif
099400041228
099500041228     c                   endif
099600041229      * rovescio la data
099700041229     c                   If        WRK_dta > 0
099800041229     c                   clear                   WLBdat
099900041229     c                   eval      G02inv = wrk_dta
100000041229     c                   eval      G02err = '3'
100100041229     c                   call      'XSRDA8'
100200041229     c                   parm                    WLBdat
100300041229     c                   z-add     G02dat        dtaogg                         data oggetto
100400041229     c                   endif
100500041229
100600041229      *
100700041023      * note
100800041023      *
100900041023     c                   clear                   p3cno1
101000041023     c                   clear                   p3cno2
101100050111      * recupero le note se richiesto
101200041023      *
101300050111     c                   If        id2not = 'S'
101400050111
101500041026     c     ktgn          setll     Fitgn01l
101600041026     c*
101700041026     c     ktgn          reade     Fitgn01l
101800050111     c                   if        not %eof(Fitgn01l) and tgnsts <> 'N'
101900041026     c                   movel     tgnnot        p3cno1
102000041026     c                   endif
102100041026     c     ktgn          reade     Fitgn01l
102200050111     c                   if        not %eof(Fitgn01l) and tgnsts <> 'N'
102300041026     c                   movel     tgnnot        p3cno2
102400041026     c                   endif
102500050111
102600050111     c                   endif
102700050111
102800041023      * aggancio record dei totali
102900041023      *
103000041229     c                   If        W_clielwf = ' '
103100041229     C     Krcc          chain     Wfrcc00f
103200041229     c                   if        not %found(wfrcc00f)
103300041229     c                   clear                   wfrcc000
103400041023      * valorizzo i primi  campi del file
103500041229     c                   eval      Rcctpe = '1'
103600041229     c                   eval      Rccksc = codcli
103700041229     c                   eval      Rccdte = dateu
103800041230     c                   eval      Rccddr = id2AMGDCD
103900041230     c                   eval      Rccadr = id2AMGDCA
104000041230     c                   eval      Rccddc = id2AMGDHD
104100041230     c                   eval      Rccadc = id2AMGDHA
104200041229      * eval prima volta che imposto i dati per ogni Cliente
104300041229     c                   eval      W_clielwf = 'Y'
104400041229      * eval prima volta che stampo testats a cambio Cliente
104500041025     c                   eval      ST_testa = 'Y'
104600041023     c                   endif
104700041025     c                   endif
104800041229      * stampo testate se richiesta stampa del dettaglio se prima volta del Cliente o se OF
104900050110     c                   If        id2tot = ' '
105000041025     c                   If        ST_testa = 'Y' or *in90
105100041023      * testa
105200041229     c                   write     DND3t1
105300041229     c                   write     DND3tS
105400041229     c                   write     DND3t3
105500041229     c                   write     DND3t5
105600041229     c                   write     DND3t3
105700041229     c                   write     DND3t2
105800041229     c                   write     DND3t3
105900041023      *
106000041025     c   90              setoff                                       90
106100041025      *
106200041025     c                   clear                   ST_testa
106300041025      *
106400041023     c                   endif
106500041103      *
106600041103      * decodifico
106700140130      *?(già fatto in fase di selezione)?
106800140205     c                   if        yy = *zero  or  TGDmad <> sk_MAR(yy)
106900140205     c                   eval      yy = %lookup( TGDmad : sk_MAR )
107000140205     c                   endif
107100140205     c                   if        yy > *zero
107200140205     c                   eval      DecMAD = sk_MARd(yy)
107300140205     c                   else
107400140205     c                   eval      DecMAD = *all'? '
107500140205     c                   endif
107600140130      *
107700041103      * chi chiama  solo mittente destinatario cliente
107800041103
107900041103     c                   select
108000041103
108100041103     c                   when      tgdtch ='M'
108200041201     c                   eval      dectch = 'MIT/CLI'
108300041103
108400041103     c                   when      tgdtch ='D'
108500041103     c                   eval      dectch = 'DESTIN.'
108600041103
108700041103     c                   when      tgdtch ='C'
108800041201     c                   eval      dectch = 'MIT/CLI'
108900041103
109000041103     c                   other
109100041103     c                   eval      dectch = 'ALTRO  '
109200041103
109300041103     c                   endsl
109400140130      *
109500041103
109600041103     c                   if        tgdcdt <> *blanks
109700041103     c                   clear                   tibs02ds
109800041103     C                   MOVEL     'C'           t02mod
109900041103     C                   MOVEL     knsif         t02sif
110000041103     C                   MOVEL     'CHR'         t02cod
110100041103     C                   MOVEL     tgdcdt        t02ke1
110200041103      *
110300041103     C                   CALL      'TIBS02R'
110400041103     C                   PARM                    KPJBA
110500041103     C                   PARM                    TIBS02DS
110600041103      *
110700041103     C                   movel     T02uni        DCHR
110800041103     C                   movel(p)  §CHRdesc      deccdt
110900140130      *
111000041103     c                   else
111100041103     c                   clear                   deccdt
111200041103     c                   endif
111300041023      *
111400041023      * stampo la riga di dettaglio 1 + note se ci sono
111500041023      *
111600050111      *
111700041229     c                   write     DND3d03
111800041023      *
111900041229     c                   if        P3cno2 <> *blanks or *in15
112000041229     c                   write     DND3d04
112100041229     c                   endif
112200041229      * se la seconda nota è valorizzata cerco e stampo le note successive
112300050111     c                   If        id2not = 'S'
112400050111
112500041229     c                   dow       p3cno2 <> *blanks
112600041229     c                   clear                   p3cno2
112700041229     c     ktgn          reade     Fitgn01l
112800050111     c                   if        not %eof(Fitgn01l) and tgnsts <> 'N'
112900041229     c                   movel     tgnnot        p3cno2
113000041229     c                   setoff                                       15
113100041229      * se è OF
113200041229     c                   If        *in90
113300041229      * testa
113400041229     c                   write     DND3t1
113500041229     c                   write     DND3tS
113600041229     c                   write     DND3t3
113700041229     c                   write     DND3t5
113800041229     c                   write     DND3t3
113900041229     c                   write     DND3t2
114000041229     c                   write     DND3t3
114100041229     c                   setoff                                       90
114200041229     c                   endif
114300041229      *
114400041229     c                   write     DND3d04
114500041230     c                   endif
114600041229     c                   enddo
114700050111     c                   endif
114800041230     c                   endif
114900041229
115000041023      *
115100041023      * C A L C O L I
115200041023      *
115300041026
115400041026      * mi salvo la schiera dei motivi d'apertura per p.o.
115500041026
115600140130      *
115700041026     c     tgdmad        lookup    skmad                                  30
115800041026      *
115900041026     c                   if        not %found
116000041026     c                   add       1             mm
116100041026     c                   eval      skmad(mm) = tgdmad
116200041026     c                   endif
116300041023
116400041023      * Aperte suddivise per motivo
116500041023
116600041023     c                   If        tgdcdt = *blanks
116700041023
116800140409     c                   Select
116900140409     c                   when      TGDmad = '  1'
117000041229     c                   add       1             rcca01
117100041023
117200140409     c                   when      TGDmad = '  2'
117300041229     c                   add       1             rcca02
117400041023
117500140409     c                   when      TGDmad = '  3'
117600041229     c                   add       1             rcca03
117700041023
117800140409     c                   when      TGDmad = '  5'
117900041229     c                   add       1             rcca05
118000041023
118100140409     c                   when      TGDmad = '  6'
118200041229     c                   add       1             rcca06
118300041023
118400140409     c                   when      TGDmad = '  7'
118500041229     c                   add       1             rcca07
118600041023
118700140409     c                   when      TGDmad = ' 10'
118800041229     c                   add       1             rcca10
118900041023
119000140409     c                   when      TGDmad = ' 11'
119100041229     c                   add       1             rcca11
119200041023
119300140409     c                   when      TGDmad = ' 12'
119400041229     c                   add       1             rcca12
119500041023
119600140409     c                   when      TGDmad = ' 13'
119700041229     c                   add       1             rcca13
119800120914
119900140409     c                   when      TGDmad = ' 14'
120000120914     c                   add       1             rcca14
120100120914
120200140409     c                   when      TGDmad = ' 15'
120300120914     c                   add       1             rcca15
120400120914
120500140409     c                   when      TGDmad = ' 16'
120600120914     c                   add       1             rcca16
120700041023
120800140409     c                   when      TGDmad = ' 20'
120900041229     c                   add       1             rcca20
121000041023
121100140409     c                   when      TGDmad = ' 21'
121200041229     c                   add       1             rcca21
121300041023
121400140409     c                   when      TGDmad = ' 22'
121500041229     c                   add       1             rcca22
121600041023
121700140409     c                   when      TGDmad = ' 30'
121800041229     c                   add       1             rcca30
121900120914
122000140409     c                   when      TGDmad = ' 50'
122100120914     c                   add       1             rcca50
122200041023
122300140409     c                   when      TGDmad = ' 99'
122400041229     c                   add       1             rcca99
122500130515
122600140409     c                   when      TGDmad = '  8'
122700130515     c                   add       1             rccaX1
122800140409
122900140409     c                   when      TGDmad = ' 88'
123000140409     c                   add       1             rccaX2
123100140409     c                   EndSl
123200041023
123300041023     c                   endif
123400041023
123500041023      * CHIUSI Fondati     suddivisi per motivo
123600041023
123700041023     c                   If        tgdcdt = '  F'
123800041023      *
123900140409     c                   Select
124000140409     c                   when      TGDmad = '  1'
124100041229     c                   add       1             rccf01
124200041023
124300140409     c                   when      TGDmad = '  2'
124400041229     c                   add       1             rccf02
124500041023
124600140409     c                   when      TGDmad = '  3'
124700041229     c                   add       1             rccf03
124800041023
124900140409     c                   when      TGDmad = '  5'
125000041229     c                   add       1             rccf05
125100041023
125200140409     c                   when      TGDmad = '  6'
125300041229     c                   add       1             rccf06
125400041023
125500140409     c                   when      TGDmad = '  7'
125600041229     c                   add       1             rccf07
125700041023
125800140409     c                   when      TGDmad = ' 10'
125900041229     c                   add       1             rccf10
126000041023
126100140409     c                   when      TGDmad = ' 11'
126200041229     c                   add       1             rccf11
126300041023
126400140409     c                   when      TGDmad = ' 12'
126500041229     c                   add       1             rccf12
126600041023
126700140409     c                   when      TGDmad = ' 13'
126800041229     c                   add       1             rccf13
126900120914
127000140409     c                   when      TGDmad = ' 14'
127100120914     c                   add       1             rccf14
127200120914
127300140409     c                   when      TGDmad = ' 15'
127400120914     c                   add       1             rccf15
127500120914
127600140409     c                   when      TGDmad = ' 16'
127700120914     c                   add       1             rccf16
127800041023
127900140409     c                   when      TGDmad = ' 20'
128000041229     c                   add       1             rccf20
128100041023
128200140409     c                   when      TGDmad = ' 21'
128300041229     c                   add       1             rccf21
128400041023
128500140409     c                   when      TGDmad = ' 22'
128600041229     c                   add       1             rccf22
128700041023
128800140409     c                   when      TGDmad = ' 30'
128900041229     c                   add       1             rccf30
129000120914
129100140409     c                   when      TGDmad = ' 50'
129200120914     c                   add       1             rccf50
129300041023
129400140409     c                   when      TGDmad = ' 99'
129500041229     c                   add       1             rccf99
129600130515
129700140409     c                   when      TGDmad = '  8'
129800130515     c                   add       1             rccfX1
129900140409
130000140409     c                   when      TGDmad = ' 88'
130100140409     c                   add       1             rccfX2
130200140409     c                   EndSl
130300041023      *
130400041023     c                   endif
130500041023
130600041023      * CHIUSI InFondati     suddivisi per motivo
130700041023
130800041023     c                   If        tgdcdt = '  N'
130900041023      *
131000140409     c                   Select
131100140409     c                   when      TGDmad = '  1'
131200041229     c                   add       1             rccn01
131300041023
131400140409     c                   when      TGDmad = '  2'
131500041229     c                   add       1             rccn02
131600041023
131700140409     c                   when      TGDmad = '  3'
131800041229     c                   add       1             rccn03
131900041023
132000140409     c                   when      TGDmad = '  5'
132100041229     c                   add       1             rccn05
132200041023
132300140409     c                   when      TGDmad = '  6'
132400041229     c                   add       1             rccn06
132500041023
132600140409     c                   when      TGDmad = '  7'
132700041229     c                   add       1             rccn07
132800041023
132900140409     c                   when      TGDmad = ' 10'
133000041229     c                   add       1             rccn10
133100041023
133200140409     c                   when      TGDmad = ' 11'
133300041229     c                   add       1             rccn11
133400041023
133500140409     c                   when      TGDmad = ' 12'
133600041229     c                   add       1             rccn12
133700041023
133800140409     c                   when      TGDmad = ' 13'
133900041229     c                   add       1             rccn13
134000120914
134100140409     c                   when      TGDmad = ' 14'
134200120914     c                   add       1             rccn14
134300120914
134400140409     c                   when      TGDmad = ' 15'
134500120914     c                   add       1             rccn15
134600120914
134700140409     c                   when      TGDmad = ' 16'
134800120914     c                   add       1             rccn16
134900041023
135000140409     c                   when      TGDmad = ' 20'
135100041229     c                   add       1             rccn20
135200041023
135300140409     c                   when      TGDmad = ' 21'
135400041229     c                   add       1             rccn21
135500041023
135600140409     c                   when      TGDmad = ' 22'
135700041229     c                   add       1             rccn22
135800041023
135900140409     c                   when      TGDmad = ' 30'
136000041229     c                   add       1             rccn30
136100120914
136200140409     c                   when      TGDmad = ' 50'
136300120914     c                   add       1             rccn50
136400041023
136500140409     c                   when      TGDmad = ' 99'
136600041229     c                   add       1             rccn99
136700130515
136800140409     c                   when      TGDmad = '  8'
136900130515     c                   add       1             rccnX1
137000140409
137100140409     c                   when      TGDmad = ' 88'
137200140409     c                   add       1             rccnX2
137300140409     c                   EndSl
137400041023      *
137500041023     c                   endif
137600041023
137700041023      * CHIUSI InFormativi   suddivisi per motivo
137800041023
137900041023     c                   If        tgdcdt = '  I'
138000041023      *
138100140409     c                   Select
138200140409     c                   when      TGDmad = '  1'
138300041229     c                   add       1             rcci01
138400041023
138500140409     c                   when      TGDmad = '  2'
138600041229     c                   add       1             rcci02
138700041023
138800140409     c                   when      TGDmad = '  3'
138900041229     c                   add       1             rcci03
139000041023
139100140409     c                   when      TGDmad = '  5'
139200041229     c                   add       1             rcci05
139300041023
139400140409     c                   when      TGDmad = '  6'
139500041229     c                   add       1             rcci06
139600041023
139700140409     c                   when      TGDmad = '  7'
139800041229     c                   add       1             rcci07
139900041023
140000140409     c                   when      TGDmad = ' 10'
140100041229     c                   add       1             rcci10
140200041023
140300140409     c                   when      TGDmad = ' 11'
140400041229     c                   add       1             rcci11
140500041023
140600140409     c                   when      TGDmad = ' 12'
140700041229     c                   add       1             rcci12
140800041023
140900140409     c                   when      TGDmad = ' 13'
141000041229     c                   add       1             rcci13
141100120914
141200140409     c                   when      TGDmad = ' 14'
141300120914     c                   add       1             rcci14
141400120914
141500140409     c                   when      TGDmad = ' 15'
141600120914     c                   add       1             rcci15
141700120914
141800140409     c                   when      TGDmad = ' 16'
141900120914     c                   add       1             rcci16
142000041023
142100140409     c                   when      TGDmad = ' 20'
142200041229     c                   add       1             rcci20
142300041023
142400140409     c                   when      TGDmad = ' 21'
142500041229     c                   add       1             rcci21
142600041023
142700140409     c                   when      TGDmad = ' 22'
142800041229     c                   add       1             rcci22
142900041023
143000140409     c                   when      TGDmad = ' 30'
143100041229     c                   add       1             rcci30
143200120914
143300140409     c                   when      TGDmad = ' 50'
143400120914     c                   add       1             rcci50
143500041023
143600140409     c                   when      TGDmad = ' 99'
143700041229     c                   add       1             rcci99
143800130515
143900140409     c                   when      TGDmad = '  8'
144000130515     c                   add       1             rcciX1
144100140409
144200140409     c                   when      TGDmad = ' 88'
144300140409     c                   add       1             rcciX2
144400140409     c                   EndSl
144500041023      *
144600041023     c                   endif
144700041023
144800041023      * Chiamante Destinatario suddivisi per motivo
144900041023
145000041023     c                   If        tgdtch = 'D'
145100041023      *
145200140409     c                   Select
145300140409     c                   when      TGDmad = '  1'
145400041229     c                   add       1             rccd01
145500041023
145600140409     c                   when      TGDmad = '  2'
145700041229     c                   add       1             rccd02
145800041023
145900140409     c                   when      TGDmad = '  3'
146000041229     c                   add       1             rccd03
146100041023
146200140409     c                   when      TGDmad = '  5'
146300041229     c                   add       1             rccd05
146400041023
146500140409     c                   when      TGDmad = '  6'
146600041229     c                   add       1             rccd06
146700041023
146800140409     c                   when      TGDmad = '  7'
146900041229     c                   add       1             rccd07
147000041023
147100140409     c                   when      TGDmad = ' 10'
147200041229     c                   add       1             rccd10
147300041023
147400140409     c                   when      TGDmad = ' 11'
147500041229     c                   add       1             rccd11
147600041023
147700140409     c                   when      TGDmad = ' 12'
147800041229     c                   add       1             rccd12
147900041023
148000140409     c                   when      TGDmad = ' 13'
148100041229     c                   add       1             rccd13
148200120914
148300140409     c                   when      TGDmad = ' 14'
148400120914     c                   add       1             rccd14
148500120914
148600140409     c                   when      TGDmad = ' 15'
148700120914     c                   add       1             rccd15
148800120914
148900140409     c                   when      TGDmad = ' 16'
149000120914     c                   add       1             rccd16
149100041023
149200140409     c                   when      TGDmad = ' 20'
149300041229     c                   add       1             rccd20
149400041023
149500140409     c                   when      TGDmad = ' 21'
149600041229     c                   add       1             rccd21
149700041023
149800140409     c                   when      TGDmad = ' 22'
149900041229     c                   add       1             rccd22
150000041023
150100140409     c                   when      TGDmad = ' 30'
150200041229     c                   add       1             rccd30
150300120914
150400140409     c                   when      TGDmad = ' 50'
150500120914     c                   add       1             rccd50
150600041023
150700140409     c                   when      TGDmad = ' 99'
150800041229     c                   add       1             rccd99
150900130515
151000140409     c                   when      TGDmad = '  8'
151100130515     c                   add       1             rccdX1
151200140409
151300140409     c                   when      TGDmad = ' 88'
151400140409     c                   add       1             rccdX2
151500140409     c                   EndSl
151600041023      *
151700041023     c                   endif
151800041023
151900041023      * Chiamante Mittente     suddivisi per motivo
152000041023
152100041023     c                   If        tgdtch = 'M'
152200041023      *
152300140409     c                   Select
152400140409     c                   when      TGDmad = '  1'
152500041229     c                   add       1             rccm01
152600041023
152700140409     c                   when      TGDmad = '  2'
152800041229     c                   add       1             rccm02
152900041023
153000140409     c                   when      TGDmad = '  3'
153100041229     c                   add       1             rccm03
153200041023
153300140409     c                   when      TGDmad = '  5'
153400041229     c                   add       1             rccm05
153500041023
153600140409     c                   when      TGDmad = '  6'
153700041229     c                   add       1             rccm06
153800041023
153900140409     c                   when      TGDmad = '  7'
154000041229     c                   add       1             rccm07
154100041023
154200140409     c                   when      TGDmad = ' 10'
154300041229     c                   add       1             rccm10
154400041023
154500140409     c                   when      TGDmad = ' 11'
154600041229     c                   add       1             rccm11
154700041023
154800140409     c                   when      TGDmad = ' 12'
154900041229     c                   add       1             rccm12
155000041023
155100140409     c                   when      TGDmad = ' 13'
155200041229     c                   add       1             rccm13
155300120914
155400140409     c                   when      TGDmad = ' 14'
155500120914     c                   add       1             rccm14
155600120914
155700140409     c                   when      TGDmad = ' 15'
155800120914     c                   add       1             rccm15
155900120914
156000140409     c                   when      TGDmad = ' 16'
156100120914     c                   add       1             rccm16
156200041023
156300140409     c                   when      TGDmad = ' 20'
156400041229     c                   add       1             rccm20
156500041023
156600140409     c                   when      TGDmad = ' 21'
156700041229     c                   add       1             rccm21
156800041023
156900140409     c                   when      TGDmad = ' 22'
157000041229     c                   add       1             rccm22
157100041023
157200140409     c                   when      TGDmad = ' 30'
157300041229     c                   add       1             rccm30
157400120914
157500140409     c                   when      TGDmad = ' 50'
157600120914     c                   add       1             rccm50
157700041023
157800140409     c                   when      TGDmad = ' 99'
157900041229     c                   add       1             rccm99
158000130515
158100140409     c                   when      TGDmad = '  8'
158200130515     c                   add       1             rccmX1
158300140409
158400140409     c                   when      TGDmad = ' 88'
158500140409     c                   add       1             rccmX2
158600140409     c                   EndSl
158700041023      *
158800041023     c                   endif
158900041023
159000041023      * Chiamante Cliente      suddivisi per motivo
159100041023
159200041023     c                   If        tgdtch = 'C'
159300041023      *
159400140409     c                   Select
159500140409     c                   when      TGDmad = '  1'
159600041229     c                   add       1             rccc01
159700041023
159800140409     c                   when      TGDmad = '  2'
159900041229     c                   add       1             rccc02
160000041023
160100140409     c                   when      TGDmad = '  3'
160200041229     c                   add       1             rccc03
160300041023
160400140409     c                   when      TGDmad = '  5'
160500041229     c                   add       1             rccc05
160600041023
160700140409     c                   when      TGDmad = '  6'
160800041229     c                   add       1             rccc06
160900041023
161000140409     c                   when      TGDmad = '  7'
161100041229     c                   add       1             rccc07
161200041023
161300140409     c                   when      TGDmad = ' 10'
161400041229     c                   add       1             rccc10
161500041023
161600140409     c                   when      TGDmad = ' 11'
161700041229     c                   add       1             rccc11
161800041023
161900140409     c                   when      TGDmad = ' 12'
162000041229     c                   add       1             rccc12
162100041023
162200140409     c                   when      TGDmad = ' 13'
162300041229     c                   add       1             rccc13
162400120914
162500140409     c                   when      TGDmad = ' 14'
162600120914     c                   add       1             rccc14
162700120914
162800140409     c                   when      TGDmad = ' 15'
162900120914     c                   add       1             rccc15
163000120914
163100140409     c                   when      TGDmad = ' 16'
163200120914     c                   add       1             rccc16
163300041023
163400140409     c                   when      TGDmad = ' 20'
163500041229     c                   add       1             rccc20
163600041023
163700140409     c                   when      TGDmad = ' 21'
163800041229     c                   add       1             rccc21
163900041023
164000140409     c                   when      TGDmad = ' 22'
164100041229     c                   add       1             rccc22
164200041023
164300140409     c                   when      TGDmad = ' 30'
164400041229     c                   add       1             rccc30
164500120914
164600140409     c                   when      TGDmad = ' 50'
164700120914     c                   add       1             rccc50
164800041023
164900140409     c                   when      TGDmad = ' 99'
165000041229     c                   add       1             rccc99
165100130515
165200140409     c                   when      TGDmad = '  8'
165300130515     c                   add       1             rcccX1
165400140409
165500140409     c                   when      TGDmad = ' 88'
165600140409     c                   add       1             rcccX2
165700140409     c                   EndSl
165800041023      *
165900041023     c                   endif
166000041023
166100041023      * Chiamante Altro        suddivisi per motivo
166200041023
166300041023     c                   If        tgdtch<>'D' and tgdtch<>'M' and tgdtch<>'C'
166400041023      *
166500140409     c                   Select
166600140409     c                   when      TGDmad = '  1'
166700041229     c                   add       1             rccx01
166800041023
166900140409     c                   when      TGDmad = '  2'
167000041229     c                   add       1             rccx02
167100041023
167200140409     c                   when      TGDmad = '  3'
167300041229     c                   add       1             rccx03
167400041023
167500140409     c                   when      TGDmad = '  5'
167600041229     c                   add       1             rccx05
167700041023
167800140409     c                   when      TGDmad = '  6'
167900041229     c                   add       1             rccx06
168000041023
168100140409     c                   when      TGDmad = '  7'
168200041229     c                   add       1             rccx07
168300041023
168400140409     c                   when      TGDmad = ' 10'
168500041229     c                   add       1             rccx10
168600041023
168700140409     c                   when      TGDmad = ' 11'
168800041229     c                   add       1             rccx11
168900041023
169000140409     c                   when      TGDmad = ' 12'
169100041229     c                   add       1             rccx12
169200041023
169300140409     c                   when      TGDmad = ' 13'
169400041229     c                   add       1             rccx13
169500120914
169600140409     c                   when      TGDmad = ' 14'
169700120914     c                   add       1             rccx14
169800120914
169900140409     c                   when      TGDmad = ' 15'
170000120914     c                   add       1             rccx15
170100120914
170200140409     c                   when      TGDmad = ' 16'
170300120914     c                   add       1             rccx16
170400041023
170500140409     c                   when      TGDmad = ' 20'
170600041229     c                   add       1             rccx20
170700041023
170800140409     c                   when      TGDmad = ' 21'
170900041229     c                   add       1             rccx21
171000041023
171100140409     c                   when      TGDmad = ' 22'
171200041229     c                   add       1             rccx22
171300041023
171400140409     c                   when      TGDmad = ' 30'
171500041229     c                   add       1             rccx30
171600120914
171700140409     c                   when      TGDmad = ' 50'
171800120914     c                   add       1             rccx50
171900041023
172000140409     c                   when      TGDmad = ' 99'
172100041229     c                   add       1             rccx99
172200130515
172300140409     c                   when      TGDmad = '  8'
172400130515     c                   add       1             rccxX1
172500140409
172600140409     c                   when      TGDmad = ' 88'
172700140409     c                   add       1             rccxX2
172800140409     c                   EndSl
172900041023      *
173000041023     c                   endif
173100041023      *
173200041023     c                   endsr
173300041022      *
173400041023      *---------------------------------------------------------------*
173500041023      * Stampa della testata                                          *
173600041023      *---------------------------------------------------------------*
173700041023     c     SR_Testa      BegSr
173800041026      *
173900041227     c* Scelta
174000041227
174100041228     c                   if        id2sce='1'
174200041227     c                   eval      dessce='Un solo cliente'
174300041023     c                   endif
174400041228     c                   if        id2sce='2'
174500041227     c                   eval      dessce='Un cliente unificante'
174600041023     c                   endif
174700041023      * p.o. responsabile
174800041227     c                   If        id2prs > 0
174900041227     c     id2prs        chain     azorg01l
175000041230     c                   movel     orgdes        desprs
175100041023     c                   endif
175200041023      *
175300041023      * p.o. apertura
175400041227     c                   If        id2fil > 0
175500041227     c     id2fil        chain     azorg01l
175600041230     c                   movel     orgdes        desfil
175700041023     c                   endif
175800041023      *
175900041229     c                   write     DND3t1
176000041229     c                   write     DND3ts
176100041229     c                   write     DND302
176200041023
176300041023     c                   endsr
176400041023      *
176500041022      *---------------------------------------------------------------*
176600041022      * Reperimento Dati del job (Utente/Operativi)                   *
176700041022      *---------------------------------------------------------------*
176800041022     c     DatiJob       BegSr
176900041022      *
177000041022     c     *dtaara       define    §azute        azuteds
177100041022     c     *dtaara       define    §datiute      ddatiute
177200041022      *
177300041022     c                   in(E)     *dtaara
177400041022     c                   IF        %ERROR or RSUT = *blanks
177500041022     c                   clear                   Tibs34Ds
177600041022     c                   call      'TIBS34R'
177700041022     c                   parm                    Tibs34Ds
177800041022     c                   in        *dtaara
177900041022     c                   ENDIF
178000041023      * - file FITGR01L
178100041023     c     ktgr          klist
178200041023     c                   kfld                    tgdant
178300041023     c                   kfld                    tgdnut
178400041023     c                   kfld                    tgdpdt
178500041023     c                   kfld                    K_tgrtri
178600041026      * - file FITGN01L
178700041026     c     ktgN          klist
178800041026     c                   kfld                    tgdant
178900041026     c                   kfld                    tgdnut
179000041026     c                   kfld                    tgdpdt
179100041228      * - file TITAS30C
179200041228     c     kspedw        klist
179300041228     c                   kfld                    W_aas
179400041228     c                   kfld                    W_lnp
179500041228     c                   kfld                    W_nrs
179600041228     c                   kfld                    W_nsp
179700041228      * - file FNORM01L
179800041228     c     kormw         klist
179900041228     c                   kfld                    W_poe
180000041228     c                   kfld                    W_nsr
180100041228     c                   kfld                    W_nor
180200041228     c                   kfld                    W_nrv
180300041229      * - file WFRCC00F
180400041229     c     krcc          klist
180500041229     c                   kfld                    W_trc
180600041229     c                   kfld                    codcli
180700041022      *
180800041022     c                   EndSr
180900041020
181000041022      *---------------------------------------------------------------*
181100041022      * Routine Iniziale                                              *
181200041022      *---------------------------------------------------------------*
181300041022     c     *InzSR        BegSR
181400041022      *
181500041022     c     *Entry        plist
181600041022     c                   parm                    KPJBA
181700041230     c                   movel     kpjbu         fidnD2ds
181800041022      * Reperisco data corrente
181900041022     c                   time                    w0140
182000041023     c                   move      w0140         wDate
182100041022     c                   clear                   WLBdat
182200041023     c                   eval      G02dat = wDate
182300041022     c                   call      'XSRDA8'
182400041022     c                   parm                    WLBdat
182500041022     c                   z-add     G02inv        DateU
182600041022      *
182700041022      * Reperisco dati job
182800041022     c                   exsr      DatiJob
182900041022
183000041022
183100041022      * controllo il sistema informativo dove sono per reperire
183200041022      * la libreria
183300041231      *
183400041026     c                   If        %subst(knsif:7:1) <> 'P'
183500041231     c                   Eval      wlib = 'GAITRAAZM '
183600041231     c                   Eval      wlib1 = 'GAITRAGRU '
183700041022     c                   EndIf
183800041231      *
183900041022     c                   If        %subst(knsif:7:1) = 'P'
184000120914     c                   Eval      wlib = 'GAITRAAZP'
184100041231     c                   Eval      wlib1 = 'GAITRAGRPS'
184200041022     c                   EndIf
184300041231
184400041231      * duplico il file di comodo in qtemp
184500041231     c                   clear                   comman
184600041231     c                   movel(p)  cmd(1)        comman
184700041231     c                   Eval      %Subst(comman:33:10) = wlib
184800041231     c                   call      'QCMDEXC'
184900041231     c                   parm                    comman
185000041231     c                   parm                    lenght
185100041231      * apro il file di comodo per la stampa
185200041231     c                   open      wfrcc00f
185300041231
185400041228
185500041228      * apro il file titas
185600041228     c                   Clear                   comman
185700041231     c                   Movel(p)  cmd(2)        comman
185800041228     c                   Eval      %Subst(comman:13:8) = 'TITAS30C'
185900041231     c                   Eval      %Subst(comman:30:10) = wlib1
186000041228     c                   Eval      comman =
186100041228     c                             %trim(comman) + '/TITAS30C)'
186200041228     c                   Call      'QCMDEXC'                            99
186300041228     c                   Parm                    comman
186400041228     c                   Parm                    lenght
186500041231      *
186600041228     c                   open      titas30c
186700140205
186800140205      /free
186900140205
187000140205         // -?Caricamento dati tab. "MAR" = Motivi Apertura Reclami?
187100140205         clear  yy;
187200140205         clear  sk_MAR;
187300140205         clear  sk_MARd;
187400140210         clear  sk_MARs;
187500140205         clear  KeyTNTBE01;
187600140205         k_TBEcod = 'MAR';
187700140205         setll  %kds( keyTNTBE01 )  TNTBE000;
187800140205         reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
187900140205         DoW  Not  %eof(TNTBE01L);
188000140205           if  TBEatb = *blank;
188100140205             dMAR = TBEuni;
188200140205             yy += 1;
188300140205             sk_MAR(yy)  = TBEke1;
188400140205             sk_MARd(yy) = §MARdesc;
188500140210             sk_MARs(yy) = §MAReSt;
188600140205           endif;
188700140205           reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
188800140205         EndDo;
188900140205
189000140205      /end-free
189100140205      *
189200050111      * se devo stampare il P.O. responsabile lascio spento il 16
189300050111     c                   If        Id2por <> 'S'
189400050111     c                   seton                                        16
189500050111     c                   endif
189600050117      * se devo stampare le note lascio spento il 18
189700050117     c                   If        Id2not <> 'S'
189800050117     c                   seton                                        18
189900050117     c                   endif
190000050113      * se devo stampare il dettaglio per figlio
190100050113     c                   If        Id2det =  'S'
190200050113     c                   seton                                        17
190300050113     c                   endif
190400041022
190500041022     C                   ENDSR
190600041231      *
190700041231
190800041022**   cmd - comando
190900041231CRTDUPOBJ OBJ(WFRCC00F) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
191000041228OVRDBF FILE(        ) TOFILE(
