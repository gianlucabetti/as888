000100070705     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300070316
000400070619      *------------------------------------------------------------------------*
000500070619      *                   ASSEGNAZIONE MANUALE ORM     ?
000600070619      *------------------------------------------------------------------------*
000700070619
000800070619     fazorg01l  if   e           k disk
000900070719     ffiapd01l  if   e           k disk
001000070720     ffidst00f  uf   e             disk    commit
001100070619     ffidst01l  if   e           k disk    rename(fidst000:fidst01)
001200070720     f                                     infds(dstds)
001300070620     ffidst08l  if   e           k disk    rename(fidst000:fidst08)
001400070802     ffnorm01l  if   e           k disk
001500070720     ffnorg01l  uf   e           k disk    commit infds(orgds)
001600071109     ffnorp01l  if   e           k disk
001700110523     ffipdo02t  if   e           k disk
001800140211     ftntbe01l  if   e           k disk
001900070621     ffior60d   cf   e             workstn sfile(or60s01:s1nrr)
002000070622     f                                     sfile(or60s02:s2nrr)
002100070720     f                                     sfile(or60s03:s3nrr)
002200070619     f                                     infds(dsfmt)
002300070619
002400070619      *------------------------------------------------------------------------*
002500070619      *  RIEPILOGO INDICATORI
002600070619      *------------------------------------------------------------------------*
002700070621      *  05 - sproteggo filiale in gestione
002800070620      *  06 - non abilito f18
002900071022      *  07 - HI campo giro
003000080314      *  08 - ORM comunicato o scaricato a PDA
003100071022      *  09 - Protezione campo scelta subfile 2
003200070719      *  10 - filiale abilitata al picking messaggerie
003300071108      *  15 - proposte da analizzare
003400080610      *  16 - pgm richiamato da Monitor PDA
003500080610      *  17 - PDA ORM attiva o in test
003600071022      *  20 - gestione subfile
003700071022      *  21 - gestione subfile
003800070802      *  22 - sflnxtchg
003900071022      *  23 - gestione subfile
004000080514      *  25 - RI scelta subfile 2 riepilogo ORM in disinta - esitato
004100080514      *  26 - RI scelta subfile 2 riepilogo ORM in disinta - comunicato
004200080611      *  27 - scelta non valida
004300070620      *  28 - errore generico
004400070621      *  30 - generico operazioni i/o
004500070620      *  40 - distinta immessa nella scelta 1
004600070720      *  41 - errore stessa distinta per trasferisce
004700140530      *  42 - Attiva F8 nella Window finale di conferma
004800070720      *  50 - emissione della window totale/parziale
004900070720      *  51 - 57 attributi visualizzazione campo
005000070619
005100070619      *------------------------------------------------------------------------*
005200070619      *   V A R I A B I L I
005300070619      *------------------------------------------------------------------------*
005400070619     d aggior_dist     s              1    inz('N')
005500070622     d cerca_stringa   s              1
005600070619     d con_giro        s              1    inz
005700070621     d conta_giri      s              3  0 inz
005800071011     d data_oggi       s              8  0 inz
005900070718     d data_org        s              8  0 inz
006000070619     d error_dist      s              1    inz('N')
006100070720     d exec_rb         s              1    inz
006200070619     d found_dist      s              1    inz('N')
006300070621     d giri_scelti     s              3  0 inz
006400080610     d kpjbusav        s                   like(kpjbu)
006500071011     d mese            s              2  0 inz
006600071011     d mmgg            s              4  0 inz
006700070620     d msgz            s             80    inz
006800071011     d nrgg            s              3  0 inz
006900070622     d nrr_s1          s                   like(s1nrr)
007000070622     d nrr_s2          s                   like(s2nrr)
007100071011     d oggi            s              8  0 inz
007200070620     d pagine          s              4  0 inz
007300080610     d pdaorm          s              1    inz(' ')
007400080610     d pgm_chiamante   s             10
007500070621     d posizionamento  s              1    inz
007600070621     d posizionato     s              1    inz
007700070622     d primogiro_blk   s              1    inz
007800070720     d problemi        s              1    inz
007900070619     d rec_allocato    s              1    inz('N')
008000070620     d resto           s              3  0 inz
008100070621     d rigpag_sfl1     s              3  0 inz(18)
008200131127     d rigpag_sfl2     s              3  0 inz(7)
008300070622     d rigpag_sfl3     s              3  0 inz(8)
008400070619     d salta_video1    s              1    inz('N')
008500070621     d salva_v1sce     s                   like(v1sce)
008600070620     d savcgi          s                   like(orgcgi)
008700070620     d savorgcgi       s                   like(orgcgi)
008800071030     d savposnf        s                   like(fdgnfpos)
008900070619     d sav$gest        s              2    inz
009000070622     d sav_pos1        s                   like(vc2pos1)
009100070622     d sav_pos2        s                   like(vc2pos2)
009200070622     d sav_pos3        s                   like(vc2pos3)
009300070622     d sav_pos4        s                   like(vc2pos4)
009400070622     d sav_pos5        s                   like(vc2pos5)
009500070622     d sav_pos6        s                   like(vc2pos6)
009600070621     d sav_sxcgi       s                   like(v1ccgi)
009700070704     d sav_s2cgi       s                   like(vs2cgi)
009800070619     d sav_v1cddc      s                   like(v1cddc) inz
009900070619     d sav_v1cfgs      s                   like(v1cfgs) inz
010000070620     d sav_wmax        s                   like(wmax)
010100070620     d sav_wpag        s                   like(wpag)
010200070620     d sav_wsfl        s                   like(wsfl)
010300070621     d sav_s1nrr       s                   like(vc1rcd)
010400070718     d stringasql      s           1100    varying
010500070622     d stringa_20a     s             20    inz
010600070621     d s1nrr           s                   like(vc1rcd)
010700070621     d s1pag           s                   like(vc1rcd)
010800070622     d s2nrr           s                   like(vc2rcd)
010900070705     d s2nrr_xx        s              5  0
011000070704     d s2pag           s                   like(vc2rcd)
011100070720     d s3nrr           s                   like(vc3rcd)
011200070720     d s3pag           s                   like(vc3rcd)
011300070718     d test_simula     s              1    inz
011400070620     d totnor_cgi      s                   like(vs1tnor)
011500070910     d totpkg_cgi      s             10  1
011600070910     d totvlm_cgi      s             10  3
011700070621     d totnor_s1       s                   like(vs1tnor)
011800070621     d totpkg_s1       s                   like(vs1tpkg)
011900070621     d totvlm_s1       s                   like(vs1tvlm)
012000070913     d totnor_s2       s                   like(vs1tnor)
012100070913     d totpkg_s2       s                   like(vs1tpkg)
012200070913     d totvlm_s2       s                   like(vs1tvlm)
012300070619     d tutte_chiuse    s              1    inz
012400070621     d vc2rcd_pos      s                   like(vc2rcd)
012500070731     d vedi_video      s              1
012600070619     d wcgi            s                   like(v1ccgi)
012700071011     d wdata           s              8  0 inz
012800070619     d wddc            s                   like(dstdfv)
012900070619     d wfgs            s                   like(dstfgs)
013000070919     d wforzaspi       s              1    inz(*off)
013100070622     d wlen            s              3i 0
013200070620     d wmax            s                   like(vc1rcd)
013300070619     d wndc            s                   like(dstnfv)
013400070619     d wnfv            s                   like(dstnfv)
013500070619     d wnpg            s                   like(dstnpg) inz(4)
013600080611     d wokinfo         s              1n
013700131119     d wopz            s             20    inz('157')
013800140211     d wopz1           s             20    inz('15879')
013900131126     d wopza           s             20    inz('15')
014000140211     d wopzb           s             20    inz('1589')
014100070704     d wpag            s                   like(vc1rcd) inz(1)
014200070704     d wpagine         s                   like(vc1rcd)
014300070622     d wpos            s              3i 0
014400131127     d wrigsfl2        s              3  0 inz(14)
014500070620     d write_riga      s              1    inz
014600070620     d wsfl            s                   like(vc1nrr)
014700071017     d wvlm            s                   like(w1davlm1) inz
014800071011     d w0140           s             14  0
014900070621     d w1sce           s                   like(v1sce)
015000141222     d xx              s              4  0 inz
015100141222     d yy              s              4  0 inz
015200070620     d $efile          s              1    inz(*off)
015300070704     d $esci           s              1    inz(*off)
015400070621     d $fine           s              1    inz(*off)
015500070619     d $fv6            s              6p 0 dim(600)
015600070619     d $gest           s              2    inz
015700070619     d $inzd1          s              1    inz(*off)
015800070620     d $inzs1          s              1    inz(*off)
015900070621     d $inzs2          s              1    inz(*off)
016000070705     d $inzs3          s              1    inz(*off)
016100070704     d $inzw1          s              1    inz(*off)
016200070622     d $ndcalloc       s              1    inz(*off)
016300070622     d $ndcchiu        s              1    inz(*off)
016400070622     d $ndcerr         s              1    inz(*off)
016500070921     d $ormcomun       s              1    inz(*off)
016600080314     d $ormesitono     s              1    inz(*off)
016700080314     d $ormesitook     s              1    inz(*off)
016800080314     d $ormpda         s              1    inz(*off)
016900070801     d $ormsecco       s              1    inz(*off)
017000070919     d $ormspi         s              1    inz(*off)
017100070622     d $piugiri        s              1    inz(*off)
017200070620     d $rcdok          s              1    inz(*off)
017300070621     d $viss2          s              1    inz(*off)
017400131118     d wora            s                   like(vs1ora)
017500131118     d wordina         s               n   inz
017600131118     d waggiorna       s               n   inz
017700131119     d sav_wmax2       s                   like(wmax)
017800131127     d wnowin          s               n   inz
017900140211     d dataiso         s               d   datfmt(*iso)
018000140211     d dataeur         s               d   datfmt(*eur)
018100140211     d WDcodInfo       s             20a
018200140529     d InviaDatiPDA    s               n   inz
018300070619
018400070619      *------------------------------------------------------------------------*
018500070619      *   S C H I E R E
018600070619      *------------------------------------------------------------------------*
018700070921     d msg             s             78    dim(40) ctdata perrcd(1)
018800080612     d opz             s              1    dim(20)
018900070619
019000070619      *------------------------------------------------------------------------*
019100070619      *   D S   I N T E R N E / E S T E R N E
019200070619      *------------------------------------------------------------------------*
019300070720     d dstds           ds
019400070619     d  dst_nrr              397    400b 0
019500070720     d orgds           ds
019600070720     d  org_nrr              397    400b 0
019700070720     d ormds           ds
019800070720     d  orm_nrr              397    400b 0
019900080611     d prods           ds
020000080611     d  pro_nrr              397    400b 0
020100070619
020200070619     d dsfmt           ds           512
020300070619     d  $tasto               369    369
020400070619     d  sflnrr               378    379B 0
020500070705
020600070803     d dsfior07        ds
020700070803     d  dsopoe                        3  0
020800070803     d  dsonor                        7  0
020900070803     d  dsonsr                        2  0
021000070803     d  dsonrv                        2  0
021100070803     d  dsoopz                        1
021200070803     d  dsofgs                        3  0
021300070719
021400070719     d                 ds
021500070719     d  v1cnor                 1      9  0
021600070719     d  v1nsr                  1      2  0
021700070719     d  v1nor                  3      9  0
021800070619
021900070619     d wlbdat          ds                  inz
022000070719     d  g08dat                 1      8  0
022100070719     d  g08ggmm                1      4  0
022200070719     d  g08inv                 9     16  0
022300070719     d  g08err                17     17
022400070719     d  g08tgi                18     22  0
022500071011
022600071011     d wdata8          ds
022700071011     d  data1                  1      8  0
022800071011     d  data2                  9     16  0
022900071011     d  giolav                17     21  0
023000070619
023100070620     d azuteds       e ds                  extname(azute00f)
023200070620     d ddatiute      e ds
023300071011     d ddgd          e ds
023400070619     d dgrgflo       e ds
023500080314     d dorg01        e ds
023600070719     d dorm01        e ds
023700070619     d fidg09ds      e ds
023800070704     d fidg31ds      e ds
023900150625     d fior010ds     e ds
024000071119     d fior20ds      e ds
024100080714     d fior57ds      e ds
024200080610     d fior60ds      e ds
024300070731     d fior61ds      e ds
024400070718     d fior64ds      e ds
024500070731     d fior67ds      e ds
024600140211     d fior82ds      e ds
024700080327     d fior90ds      e ds
024800070619     d fnlv50ds      e ds
024900070622     d fnorgds       e ds                  extname(fnorg00f)
025000070622     d fnormds       e ds                  extname(fnorm00f)
025100071108     d fnorpds       e ds                  extname(fnorp00f)
025200070619     d kpjba         e ds
025300070718     d og147         e ds
025400080610     d og148         e ds
025500070719     d og150         e ds
025600071011     d tibs02ds      e ds
025700070620     d tibs34ds      e ds
025800070717     d trul06ds      e ds
025900070717     d  lin                    1     90  0 dim(30)
026000070620     d trul82ds      e ds
026100140319     d TRULORSDS     e ds
026200140319     d TRULOR2DS     e ds
026300131119     d FIOR73DS      e ds
026400140530
026500140530      // - DS richiamo pgm Preparazione dati PDA
026600140530     d FIOR56DS      e ds                  qualified inz
026700161108
026800161108      // - Tabella DFT - default ORM
026900161108     d dDFT          e ds
027000070619
027100070619     d                sds
027200070619     d  vidpgm                 1     10
027300070619
027400070619      *------------------------------------------------------------------------*
027500070619      *   C O S T A N T I
027600070619      *------------------------------------------------------------------------*
027700070621     d digits          c                   '0123456789'
027800131126     d dopz            c                   const('Opz. 1=sel 5=vis.ORM')
027900080612     d dopz1           c                   const('Opz. 1=sel 5=vis.ORM 8=chiudi-
028000080612     d                                      INFO')
028100140211     d dopz2           c                   const('     7=Attribuzione GIRO     -
028200140211     d                                           ')
028300140211     d dopz3           c                   const('     9=Int.Info              -
028400140211     d                                           ')
028500140211     d dopz4           c                   const('     7=Attribuzione GIRO 9=In-
028600140211     d                                     t.Info')
028700070621     d enter           c                   const(X'F1')
028800070619     d f01             c                   const(X'31')
028900070619     d f02             c                   const(X'32')
029000070619     d f03             c                   const(X'33')
029100070619     d f04             c                   const(X'34')
029200070619     d f05             c                   const(X'35')
029300070619     d f06             c                   const(X'36')
029400070619     d f07             c                   const(X'37')
029500070619     d f08             c                   const(X'38')
029600070619     d f09             c                   const(X'39')
029700070619     d f10             c                   const(X'3A')
029800070619     d f11             c                   const(X'3B')
029900070619     d f12             c                   const(X'3C')
030000070619     d f13             c                   const(X'B1')
030100070619     d f14             c                   const(X'B2')
030200070619     d f18             c                   const(X'B6')
030300070620     d fun_1           c                   'F1=Sel.tutto'
030400070621     d fun_6           c                   'F6=Conferma Giro NO DETT. ORM'
030500070621     d fun_enter       c                   'Enter=Con Dettaglio ORM'
030600070622
030700070622      **********************************************************************
030800070622      * Constants
030900070802      * MaxKey - è il max. num. di campi chiave permesso in questo pgm
031000070622      **********************************************************************
031100070622     d ord_giromit     c                   1
031200070622     d ord_mittente    c                   2
031300080128     d ord_ora         c                   3
031400070622     d ord_localita    c                   4
031500070622     d ord_volume      c                   5
031600070622     d ord_peso        c                   6
031700070802      * Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
031800070802     d MaxKey          c                   9
031900070802     d Ascendente      c                   1
032000070802     d Discendente     c                   2
032100070802     d Carattere       c                   6
032200070802     d Put             c                   1
032300070802     d EndPut          c                   2
032400070802     d Get             c                   3
032500070802     d Numerico        c                   8
032600070622      **********************************************************************
032700070622      * Campi utili:
032800070622      *     RRN1       - Numero relativo di record del Subfile
032900070622      *     SizeList   - Dimensione della lista
033000070622      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
033100070622      **********************************************************************
033200070802     d Rrn1            s              5I 0
033300070802     d NotUsed         s             16A
033400070802     d ReturnSize      s              9B 0
033500070802     d SizeList        s              9B 0
033600070802     d RrnLast         s              5I 0
033700070802     d WrkSort         s              1  0 inz(0)
033800070802     d WindSort        s              3I 0 inz(0)
033900070622      **********************************************************************
034000070622      * Data Structures
034100070622      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
034200070622      *     QLGSCB     - The sort request block for the QLGSORT API
034300070622      *     QLGSCB00   - The sort request block for the QLGSRTIO API
034400070622      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
034500070622      *     QUSEC      - Error structure for the QLGSORT API
034600070622      **********************************************************************
034700070622     d sflrcd          ds
034800070622     d  vs2cgi
034900070704     d  vs2rsr
035000080128     d  vs2inr
035100070622     d  vs2lor
035200070622     d  vs2pkg
035300070622     d  vs2vlm
035400131119     d  vs2ora
035500080612     d  vs2info
035600070622     d  h2subst
035700131119     d  vs2orr
035800071008     d  vs2all
035900070719     d  vs2sto
036000070622     d  vs2poe
036100070622     d  vs2nsr
036200070622     d  vs2nor
036300070622     d  vs2nrv
036400070704     d  vs2sce
036500070622     d  vs2dar
036600070719     d  vs2com
036700070719     d  vs2spi
036800070719     d  vs2aut
036900070719     d  vs2ncl
037000070704     d  vs2bnc
037100070920     d  vs2comun
037200071022     d  vs2prot
037300071109     d  vs2prop
037400080314     d  vs2pda
037500080314     d  vs2far
037600080514     d  vs2forza
037700080611     d  vs2nrrpro
037800140211     d  vs2dao
037900140211     d  vs2rag
038000140211     d  vh2notinfo
038100070622     d  selected                      1A
038200131118
038300131118     d sflrcd1         ds
038400131118     d  vs1cgi
038500131118     d  vs1des
038600131118     d  vs1tnor
038700131118     d  vs1tpkg
038800131118     d  vs1tvlm
038900131118     d  vs1ora
039000131118     d  selected1                     1A
039100131118     d  h1indis
039200131118     d  vs1sce
039300070622
039400070622      /COPY QSYSINC/QRPGLESRC,QLGSORT
039500070802     d QLGKL                         16    DIM(MaxKey)
039600070802     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
039700070802     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
039800070802     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
039900070802     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
040000070622
040100070622      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
040200070622      /COPY QSYSINC/QRPGLESRC,QUSEC
040300140211      /copy gaitrasrc/srcprotopr,fior82r
040400131118      //---------------------------------------------------------------
040500131118      //?Definizione procedure utilizzate.
040600131118      //---------------------------------------------------------------
040700131118
040800131118      // - Ricerca orari di servizio
040900140319     d trulorsr        pr                  extpgm('TRULORSR')
041000131118     d  kpjba                              likeds(KPJBA)
041100140319     d  trulorsds                          likeds(TRULORSDS)
041200140319     d  trulor2ds                          likeds(TRULOR2DS)
041300140319     d                                     options (*nopass)
041400140530
041500140530      // - Preparazione dati a PDA
041600140530     d fior56r         pr                  extpgm('FIOR56R')
041700140530     d  kpjba                              likeds(KPJBA)
041800140530     d  fior56ds                           likeds(FIOR56DS)
041900161108
042000161108      // - Ricerca tabelle
042100161108      /copy gaitrasrc/srcprotopr,TIBS02R
042200070619
042300070619      *------------------------------------------------------------------------*
042400070619
042500070619     c                   dow        $fine = *off
042600070619
042700070619     c                   select
042800070621
042900070619      * 1° video --> parametri
043000070619     c                   when      $gest = 'D1'
043100070704     c                   exsr      gesd1
043200070621
043300070620      * lista giri se scelta assegnazione e non impostato il giro fra i parametri d'ingresso
043400070620     c                   when      $gest = 'S1'
043500070704     c                   exsr      gess1
043600070621
043700070621      * lista di dettaglio degli ORM x selezione e guida aggiornamento data base
043800070621     c                   when      $gest = 'S2'
043900070704     c                   exsr      gess2
044000070705
044100070720      * finestra dei totali riepilogativa prima dell'aggiornamento dei dati
044200070705     c                   when      $gest = 'W1'
044300070705     c                   exsr      gesw1
044400070619
044500070720      * durante l'aggiornamento dei dati scrivo eventuali errori ORM da visualizzare
044600070720      * su questo ultimo SFL
044700070705     c                   when      $gest = 'S3'
044800070705     c                   exsr      gess3
044900070705
045000070619     c                   endsl
045100070619
045200070619     c                   enddo
045300080610
045400080610     c                   eval      kpjbu = kpjbusav
045500070619
045600070619     c                   eval      *inlr = *on
045700070619
045800070619      *------------------------------------------------------------------------*
045900070619      *   Gestione primo video --> parametri             ?
046000070619      *------------------------------------------------------------------------*
046100070704     c     gesd1         begsr
046200070621
046300070619      * inizializzo il video
046400070619     c                   if        $inzd1 = *on
046500070704     c                   exsr      inzd1
046600070619     c                   eval      $inzd1 = *off
046700070619     c                   endif
046800070619
046900070621     c     video_1       tag
047000080715      * controllo se per la filiale gestione ci sono delle proposte variazioni
047100080715      * ancora da analizzare
047200080715     c                   exsr      sr_controrp
047300070621
047400070620      * emissione video
047500070621     c                   if        salta_video1 = 'N'
047600070619      *                  __________________
047700070619     c                   exfmt     or60d01
047800070619      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
047900071030     c                   clear                   savposnf
048000070621     c                   else
048100070621     c                   eval      salta_video1 = 'N'
048200070621     c                   end
048300070619
048400070619     c                   eval      *in28 = *off
048500070621     c                   clear                   vidmsg
048600070619     c                   eval      sav$gest = 'D1'
048700070621
048800070619      * selezioni
048900070620     c                   select
049000070619      * f3=fine
049100070621     c                   when      $tasto = f03
049200070704     c                   exsr      f03_esci
049300070720     c                   rolbk
049400070719      * f2=interrogazione ORM
049500070719     c                   when      $tasto = f02
049600070719     c                   exsr      f02_intorm
049700070719     c                   goto      video_1
049800071119      * f7=gestione proposte ORM
049900071119     c                   when      $tasto = f07
050000071119     c                   exsr      f07_proposte
050100071119     c                   goto      video_1
050200070619      * f18=cambio filiale
050300070621     c                   when      $tasto = f18
050400070619     c                   eval      *in05 = *on
050500070621     c                   goto      video_1
050600070619
050700070620     c                   other
050800070619      * controllo dati immessi
050900070704     c                   exsr      ctrd1
051000080912      * per pgm richiamto da monitor
051100080912      * la routine ctrd1 la richiamo solo perchè così mi richiama l'interrogazione distinte
051200080912      * però al ritorno non devo interessarmi dei vari errori....tanto i controlli sulla distinta
051300080912      * li ha già fatti il chiamate quindi se richiamato da monitor non devo tornare all'inizio
051400080912      * ma andare avanti
051500080912     c                   if        pgm_chiamante <> 'MONITOR'
051600070621     c   28              goto      video_1
051700080912     c                   endif
051800070621
051900070620     c                   endsl
052000080610
052100080610      * Se richiamato da MONITOR con PDA
052200080610      * e se la ricerca distinta ha chiuso con F3 o F12
052300080610     c                   if        pgm_chiamante = 'MONITOR' and
052400080610     c                             (fdgtfu ='03' or fdgtfu ='12')
052500080612     c                   exsr      f03_esci
052600080610     c                   leavesr
052700080610     c                   endif
052800070620
052900070620      * se non ci sono errori passa alle operazioni da esguire
053000070620     c                   select
053100070620
053200070620     c                   when      v1sce = '1'
053300070801      * emissione sfl con elenco dei giri da assegnare
053400070801      * se non è stato richiesto un ORM
053500070801     c                   if        v1cnor = *zeros
053600070801     c                   eval      $gest ='S1'
053700070801     c                   eval      $inzs1 = *on
053800070801     c                   endif
053900070801      * se richiesto un ORM devo emettere subito il subfile degli ORM
054000070801     c                   if        v1cnor <> *zeros
054100070801     c                   eval      $gest ='S2'
054200070801     c                   eval      $inzs2 = *on
054300070801     c                   eval      $inzw1 = *on
054400070801     c                   eval      $inzs3 = *on
054500070801     c                   eval      $viss2 = *on
054600070801     c                   endif
054700070620
054800070621     c                   when      v1sce = '2' and h1ndc_meno > *zeros
054900070801      * emissione sfl con elenco degli ORM da togliere alla distinta
055000070801     c                   eval      $gest ='S2'
055100070801     c                   eval      $inzs2 = *on
055200070801     c                   eval      $inzw1 = *on
055300070801     c                   eval      $inzs3 = *on
055400070620
055500070621     c                   when      v1sce = '3' and h1ndc_meno > *zeros
055600070801      * emissione sfl con elenco degli ORM da trasferire ad altra distinta
055700070801     c                   eval      $gest ='S2'
055800070801     c                   eval      $inzs2 = *on
055900070801     c                   eval      $inzw1 = *on
056000070801     c                   eval      $inzs3 = *on
056100070719
056200070620     c                   endsl
056300070620
056400070620     c                   endsr
056500070619
056600070719      *------------------------------------------------------------------------*
056700070705      *   Inizializzo primo video --> parametri                  ?
056800070619      *------------------------------------------------------------------------*
056900070704     c     inzd1         begsr
057000070619
057100070619     c                   clear                   h1riga
057200070619     c                   clear                   h1colo
057300070718     c                   clear                   h1cddc
057400070621     c                   clear                   h1ndc_meno
057500080730     c                   clear                   h1pdr_meno
057600070731     c                   clear                   h1fpp_meno
057700070620     c                   clear                   h1ndc_piu
057800070720     c                   clear                   h1pdr_piu
057900070720     c                   clear                   h1fpp_piu
058000070919     c                   clear                   h1spi_piu
058100070719
058200070719     c                   clear                   wcgi
058300070705
058400070705     c                   clear                   v1sce
058500070705     c                   clear                   v1ccgi
058600070705     c                   clear                   v1ndc1
058700070719     c                   clear                   v1cpoe
058800070719     c                   clear                   v1cnor
058900070719     c                   clear                   v1cnrv
059000070705     c                   clear                   v1ndc2
059100070705     c                   clear                   v1ndc3
059200070705     c                   clear                   v1ndc4
059300070801
059400070801     c                   eval      $ormsecco = *off
059500070619
059600070720      * filiale gestione da filiale utente
059700070619     c                   eval      v1cfgs = dutpou
059800070619      * abilito f18
059900070619     c                   eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
060000070720      * data distinta = data del giorno
060100071011     c                   move      oggi          v1cddc
060200080610
060300080610      * se richiamato dal MONITOR
060400080610     c                   if        pgm_chiamante = 'MONITOR'
060500080610      * imposta l'FGS se passata
060600080610     c                   if        o60fgs > *zeros
060700080610     c                   eval      v1cfgs = o60fgs
060800080610     c                   endif
060900080610      * imposta la DATA se passata
061000080610     c                   if        o60dfv > *zeros
061100080610     c                   eval      g08inv = o60dfv
061200080610     c                   eval      g08err = '3'
061300080610     c                   call      'XSRDA8'
061400080610     c                   parm                    wlbdat
061500080610     c                   eval      v1cddc = g08dat
061600080610     c                   endif
061700080610     c                   endif
061800080610      * aggancio l'organigramma
061900080610     c                   clear                   og148
062000070719     c                   clear                   og150
062100070719     c     v1cfgs        chain     azorg01l
062200070719     c                   if        %found(azorg01l)
062300080610     c                   eval      og148 = orgde8
062400070719     c                   eval      og150 = orgdf0
062500070719     c                   endif
062600080610
062700080610      * imposto se PDA ORM attiva/test
062800080610     c                   eval      *in17 = (§ogpdaorm <> *blanks)
062900080610     c                   eval      pdaorm = §ogpdaorm
063000080610
063100080610      * visualizzo picking messaggerie se filiale abilitata
063200070719     c                   eval      *in10 = (§ogpkm ='S')
063300080610
063400080610      * Se richiamato dal MONITOR
063500080610      * salta tutte le altre impostazioni della precedente chiamata
063600080610     c                   if        pgm_chiamante = 'MONITOR'
063700080610     c                   eval      v1sce = '2'
063800080610     c                   leavesr
063900080610     c                   endif
064000070619
064100070720      * filiale gestione se precedentemente impostata deve rimanere quella
064200070619     c                   if        sav_v1cfgs > 0
064300070619     c                   eval      v1cfgs = sav_v1cfgs
064400070619     c                   endif
064500070619
064600070720      * data distinta se precedentemente impostata deve rimanere quella
064700070619     c                   if        sav_v1cddc > 0
064800070619     c                   eval      v1cddc = sav_v1cddc
064900070619     c                   endif
065000161108
065100161108      /free
065200161108        wfgs = V1Cfgs;
065300161108        exsr CarTabDFT;
065400161117        V1pesod = %editc(d§DFTvidpk:'4');
065500161108      /end-free
065600070619
065700070619     c                   endsr
065800070619
065900070619      *------------------------------------------------------------------------*
066000070705      *   Controlli primo video --> parametri
066100070619      *------------------------------------------------------------------------*
066200070704     c     ctrd1         begsr
066300070619
066400070619     c     ini_ctrd1     tag
066500070619
066600070619     c                   clear                   h1riga
066700070619     c                   clear                   h1colo
066800070621     c                   clear                   h1ndc_meno
066900080730     c                   clear                   h1pdr_meno
067000070731     c                   clear                   h1fpp_meno
067100070620     c                   clear                   h1ndc_piu
067200070720     c                   clear                   h1pdr_piu
067300070720     c                   clear                   h1fpp_piu
067400070919     c                   clear                   h1spi_piu
067500070718     c                   clear                   con_giro
067600070718     c                   clear                   totnor_cgi
067700070718     c                   clear                   totpkg_cgi
067800070718     c                   clear                   totvlm_cgi
067900070619     c                   eval      *in28 = *off
068000070801     c                   eval      $ormsecco = *off
068100070619
068200070718     ?* filiale gestione
068300070718     c                   clear                   og147
068400080610     c                   clear                   og148
068500070719     c                   clear                   og150
068600070619     c     v1cfgs        chain     azorg01l
068700070619     c                   if        not %found(azorg01l)
068800070619     c                   eval      vidmsg = msg(01)
068900070619     c                   eval      *in28 = *on
069000070719     c                   eval      h1riga = 04
069100070619     c                   eval      h1colo = 27
069200070619     c                   leavesr
069300070619     c                   endif
069400070718     c                   eval      og147 = orgde7
069500080610     c                   eval      og148 = orgde8
069600070719     c                   eval      og150 = orgdf0
069700070719     c                   if        §ogddao = *blank or §ogddao = *all'0'
069800070719     c                   eval      $fine = *on
069900070719     c                   leavesr
070000070719     c                   endif
070100080610      * imposto se PDA ORM attiva/test
070200080610     c                   eval      *in17 = (§ogpdaorm <> *blanks)
070300080610     c                   eval      pdaorm = §ogpdaorm
070400070719      * picking messaggerie
070500070719     c                   eval      *in10 = (§ogpkm ='S')
070600070718      * data partenza procedura in organigramma
070700070718     c                   move      §ogddao       data_org
070800070718      * controllo se siamo in simulazione
070900071011     c                   if        data_oggi < data_org
071000070718     c                   eval      test_simula = 'S'
071100070718     c                   else
071200070718     c                   clear                   test_simula
071300070718     c                   endif
071400070619
071500070619     c                   clear                   fnlv50ds
071600070619     c                   eval      d50pru = knmus
071700070619     c                   eval      d50fgs = v1cfgs
071800070619     c                   call      'FNLV50R'
071900070619     c                   parm                    fnlv50ds
072000070619     c                   if        d50err <> *blanks
072100070619     c                   eval      vidmsg = msg(02)
072200070619     c                   eval      *in28 = *on
072300070719     c                   eval      h1riga = 04
072400070619     c                   eval      h1colo = 27
072500070619     c                   leavesr
072600070619     c                   endif
072700070717
072800070717      * carico la £6 della filiale in gestione
072900070717     c                   clear                   trul06ds
073000070717     c                   eval      d06cod = '£6'
073100070717     c                   movel     v1cfgs        d06key
073200070717     c                   eval      kpjbu = trul06ds
073300070717     c                   call      'TRUL06R'
073400070717     c                   parm                    kpjba
073500070717     c                   eval      trul06ds = kpjbu
073600161108      /free
073700161108      //?Se Filiale gestione variata ricarico la tabella DFT
073800161108        IF  V1Cfgs <> sav_V1Cfgs;
073900161108          wfgs = V1Cfgs;
074000161108          exsr CarTabDFT;
074100161117          V1pesod = %editc(d§DFTvidpk:'4');
074200161108        ENDIF;
074300161108      /end-free
074400070619
074500070619      * salva la filiale
074600070619     c                   eval      wfgs = v1cfgs
074700070619     c                   eval      sav_v1cfgs = v1cfgs
074800070621     c                   eval      *in05 = *off
074900070619
075000070619     ?* data distinta
075100070619     c                   clear                   wddc
075200070619     c                   clear                   wlbdat
075300070719     c                   z-add     v1cddc        g08dat
075400070619     c                   call      'XSRDA8'
075500070619     c                   parm                    wlbdat
075600070719     c                   if        g08err = '1'
075700070621     c                   eval      vidmsg = msg(03)
075800070619     c                   eval      *in28 = *on
075900070719     c                   eval      h1riga = 04
076000070619     c                   eval      h1colo = 27
076100070619     c                   leavesr
076200070619     c                   endif
076300070619
076400070719     c                   eval      v1cddc = g08dat
076500070719     c                   eval      wddc = g08inv
076600070619     c                   eval      sav_v1cddc = v1cddc
076700070718     c                   eval      h1cddc = wddc
076800070619
076900070619      * controlla se nella data scelta ci sono distinte aperte
077000070619     c                   eval      tutte_chiuse = 'S'
077100070619     c     kdst8         setll     fidst08l
077200070619     c                   do        *hival
077300070619     c     kdst8         reade     fidst08l
077400070619     c                   if        %eof(fidst08l)
077500070619     c                   leave
077600070619     c                   endif
077700070619     c                   if        dstfcf <> 'S' and dstatb = ' '
077800070619     c                   clear                   tutte_chiuse
077900070619     c                   leave
078000070619     c                   endif
078100070619     c                   enddo
078200070720      * non ci sono distinte aperte
078300070619     c                   if        tutte_chiuse = 'S'
078400070621     c                   eval      vidmsg = msg(04)
078500070619     c                   eval      *in28 = *on
078600070719     c                   eval      h1riga = 06
078700070619     c                   eval      h1colo = 27
078800070619     c                   leavesr
078900070619     c                   endif
079000071011
079100071011      * utente NO EDP controllo data distinta
079200071011     c                   if        %subst(knmus:1:3) <> 'EDP'
079300071011      * non posso andare indietro rispetto a oggi
079400071011     c                   if        g08inv < data_oggi
079500071011     c                   eval      vidmsg = msg(24)
079600071011     c                   eval      *in28 = *on
079700071011     c                   eval      h1riga = 06
079800071011     c                   eval      h1colo = 27
079900071011     c                   leavesr
080000071011     c                   endif
080100071011      * controllo range fra le date di oggi e quella immessa
080200071011      * non può essere superiore di 3 giorni lavorativi
080300071011     c                   clear                   wdata8
080400071011     c                   eval      data1 = data_oggi
080500071011     c                   eval      data2 = g08inv
080600071011     c                   call      'XSRLAV8'
080700071011     c                   parm                    wdata8
080800071011     c                   if        giolav > §dgddc or
080900071011     c                             giolav > 3
081000071011     c                   eval      vidmsg = msg(25)
081100071011     c                   eval      *in28 = *on
081200071011     c                   eval      h1riga = 06
081300071011     c                   eval      h1colo = 27
081400071011     c                   leavesr
081500071011     c                   endif
081600071011      * controllo che la data della distinta non sia antecedente o
081700071011      * seguente a oggi di oltre 15 giorni
081800071011     c                   move      data_oggi     mmgg
081900071011     c                   movel     mmgg          mese
082000071011      * giorni standard
082100071011     c                   eval      nrgg = §dgdgg
082200071011      * periodo estivo
082300071011     c                   if        mmgg >= §dgdei and
082400071011     c                             mmgg <= §dgdef
082500071011     c                   eval      nrgg = §dgdge
082600071011     c                   endif
082700071011      * periodo invernale
082800071011     c                   if        data_oggi >= §dgdii and
082900071011     c                             data_oggi <= §dgdif
083000071011     c                   eval      nrgg = §dgdgi
083100071011     c                   endif
083200071011      * calcolo la data indietro
083300071011     c                   clear                   wdata8
083400071011     c                   eval      giolav = g08tgi - nrgg
083500071011     c                   call      'XSRGI8'
083600071011     c                   parm                    wdata8
083700071011     c                   eval      wdata = data2
083800071011      * controllo
083900071011     c                   if        wdata > data_oggi
084000071011     c                   eval      vidmsg = msg(26)
084100071011     c                   eval      *in28 = *on
084200071011     c                   eval      h1riga = 06
084300071011     c                   eval      h1colo = 27
084400071011     c                   leavesr
084500071011     c                   endif
084600071011      * calcolo la data in avanti
084700071011     c                   clear                   wdata8
084800071011     c                   eval      giolav = g08tgi + nrgg
084900071011     c                   call      'XSRGI8'
085000071011     c                   parm                    wdata8
085100071011     c                   eval      wdata = data2
085200071011      * controllo
085300071011     c                   if        wdata < data_oggi
085400071011     c                   eval      vidmsg = msg(26)
085500071011     c                   eval      *in28 = *on
085600071011     c                   eval      h1riga = 06
085700071011     c                   eval      h1colo = 27
085800071011     c                   leavesr
085900071011     c                   endif
086000071011     c                   endif
086100070619
086200070619     ?* scelta
086300070619     c                   if        v1sce = ' '
086400070621     c                   eval      vidmsg = msg(05)
086500070619     c                   eval      *in28 = *on
086600071008     c                   eval      h1riga = 13
086700070619     c                   eval      h1colo = 27
086800070619     c                   leavesr
086900070619     c                   endif
087000070619
087100070619      * pulisce i campi che non servono
087200070619     c                   if        v1sce = '1'
087300070619     c                   clear                   v1ndc2
087400070619     c                   clear                   v1ndc3
087500070619     c                   clear                   v1ndc4
087600070619     c                   endif
087700070621     c                   if        v1sce = '2'
087800070619     c                   clear                   v1ccgi
087900070619     c                   clear                   v1ndc1
088000070619     c                   clear                   v1ndc3
088100070619     c                   clear                   v1ndc4
088200070619     c                   endif
088300070621     c                   if        v1sce = '3'
088400070619     c                   clear                   v1ccgi
088500070619     c                   clear                   v1ndc1
088600070619     c                   clear                   v1ndc2
088700070619     c                   endif
088800070619
088900070619     ?* scelta (1) assegna giro alla distinta
089000070619     c                   if        v1sce = '1'
089100070719      * --> non si può richiedere giro e ORM assieme
089200070719     c                   if        v1ccgi <> *blanks and v1cnor <> *zeros
089300070719     c                   eval      *in28 = *on
089400070719     c                   eval      vidmsg = msg(22)
089500071008     c                   eval      h1riga = 15
089600070719     c                   eval      h1colo = 38
089700070719     c                   leavesr
089800070719     c                   endif
089900070619      * --> ricerca giro
090000070619     c                   if        %scan('?':v1ccgi) > *zeros
090100070619     c                   clear                   fidg09ds
090200070619     c                   eval      d09iop0 = 'P01'
090300070619     c                   eval      d09ifgs = v1cfgs
090400071011     c                   eval      d09idat = data_oggi
090500070704     c                   eval      d09itug = 'R'
090600070619     c                   eval      kpjbu = fidg09ds
090700070619     c                   call      'FIDG09R'
090800070619     c                   parm                    kpjba
090900070619     c                   eval      fidg09ds = kpjbu
091000070619     c                   eval      dgrgflo = d09oflo
091100070619      *     imposto il giro scelto
091200070619     c                   eval      v1ccgi = d09ocgi
091300070619     c                   endif
091400070619      *     controllo
091500070619     c                   if        v1ccgi <> *blank
091600070620     c                   eval      savcgi = v1ccgi
091700070704     c                   exsr      ctrgiro
091800070619      *     giro errato o non si può mettere in distinta
091900070619     c                   if        d09oerr = '1' or §grgdst = 'N'
092000070619     c                   eval      *in28 = *on
092100070621     c                   eval      vidmsg = msg(06)
092200071008     c                   eval      h1riga = 15
092300070619     c                   eval      h1colo = 38
092400070619     c                   leavesr
092500070619     c                   endif
092600070619      *     imposto la descrizione del giro
092700070619     c                   if        d09ocgi <> ' '
092800070619      *     salvo il giro
092900070619     c                   eval      wcgi = v1ccgi
093000070619     c                   eval      con_giro = 'S'
093100070619     c                   endif
093200070619     c                   endif
093300070719
093400070719      * --> ORM
093500070719     c                   if        v1cpoe <> *zeros or v1cnor <> *zeros or
093600070719     c                             v1cnrv <> *zeros
093700070719     c     korm          chain     fnorm01l
093800070720      *     deve essere un ORM valido su fnorm
093900070719     c                   if        not %found(fnorm01l)
094000070719     c                   eval      *in28 = *on
094100070719     c                   eval      vidmsg = msg(21)
094200071008     c                   eval      h1riga = 15
094300070801     c                   eval      h1colo = 58
094400070719     c                   leavesr
094500070719     c                   endif
094600070720      *     deve essere un ORM valido su fnorg
094700070802     c     korm          chain(n)  fnorg01l
094800070720     c                   if        not %found(fnorg01l)
094900070720     c                   eval      *in28 = *on
095000070720     c                   eval      vidmsg = msg(21)
095100071008     c                   eval      h1riga = 15
095200070801     c                   eval      h1colo = 58
095300070720     c                   leavesr
095400070720     c                   endif
095500070720      *     deve essere un ORM in gestione alla filiale
095600070719     c     ormpor        lookup    lin                                    30
095700070719     c                   if        not *in30
095800070719     c                   eval      *in28 = *on
095900070719     c                   eval      vidmsg = msg(21)
096000071008     c                   eval      h1riga = 15
096100070801     c                   eval      h1colo = 58
096200070719     c                   leavesr
096300070719     c                   endif
096400070720      *     se simulazione deve avere orgndc = 0
096500070719     c                   if        test_simula = 'S' and orgndc <> *zeros
096600070719     c                   eval      *in28 = *on
096700070719     c                   eval      vidmsg = msg(23)
096800071008     c                   eval      h1riga = 15
096900070801     c                   eval      h1colo = 58
097000070719     c                   leavesr
097100070719     c                   endif
097200070720      *     se filiale già attiva deve avere ormndc = 0
097300070719     c                   if        test_simula <> 'S' and ormndc <> *zeros
097400070719     c                   eval      *in28 = *on
097500070719     c                   eval      vidmsg = msg(23)
097600071008     c                   eval      h1riga = 15
097700070801     c                   eval      h1colo = 58
097800070719     c                   leavesr
097900070719     c                   endif
098000071108     c                   eval      $ormsecco = *on
098100070801      *     controllo se l'ORM è assegnabile
098200070801     c                   exsr      check_orm
098300070801     c                   if        or64asso = 'N'
098400070801     c                   eval      *in28 = *on
098500070801     c                   eval      vidmsg = or64msgo
098600071008     c                   eval      h1riga = 15
098700070801     c                   eval      h1colo = 58
098800070801     c                   leavesr
098900070801     c                   endif
099000070719     c                   endif
099100070619
099200070619      * --> numero distinta
099300070621     c                   if        v1ndc1 <> *blanks
099400070621
099500070619     c                   if        v1ndc1 = *zeros
099600070619     c                   clear                   v1ndc1
099700070619     c                   endif
099800070621      *     ricerca
099900070619     c                   if        %scan('?':v1ndc1) > *zeros
100000070619     c                   clear                   wndc
100100070621     c                   eval      w1sce = v1sce
100200070704     c                   exsr      ric_dist
100300070619     c                   eval      v1ndc1 = *all'0'
100400070704     c                   move      fdgnfv        v1ndc1
100500070619     c                   endif
100600070619     c                   if        v1ndc1 = *zeros
100700070619     c                   clear                   v1ndc1
100800070619     c                   endif
100900070621      *     deve essere numerica
101000070621     c                   if        %check(digits:v1ndc1) > 0
101100070704     c                   eval      vidmsg = msg(09)
101200070621     c                   eval      *in28 = *on
101300080327     c                   eval      h1riga = 16
101400080327     c                   eval      h1colo = 42
101500070621     c                   leavesr
101600070621     c                   endif
101700070621      *     controlla
101800070619     c                   move      v1ndc1        wnfv
101900070619     c                   eval      aggior_dist = 'N'
102000070704     c                   exsr      chain_dist
102100070620     c                   if        error_dist = 'S'
102200070620     c                   if        rec_allocato = 'S'
102300070621     c                   eval      vidmsg = msg(08)
102400070620     c                   endif
102500070620     c                   eval      *in28 = *on
102600080327     c                   eval      h1riga = 16
102700080327     c                   eval      h1colo = 42
102800070620     c                   leavesr
102900070620     c                   endif
103000070621      *     distinta non trovata
103100070620     c                   if        found_dist = 'N'
103200070620     c                   eval      *in28 = *on
103300070621     c                   eval      vidmsg = msg(09)
103400080327     c                   eval      h1riga = 16
103500080327     c                   eval      h1colo = 42
103600070620     c                   leavesr
103700070620     c                   endif
103800070621      *     distinta non nella data
103900070620     c                   if        dstdfv <> wddc
104000070620     c                   eval      *in28 = *on
104100070621     c                   eval      vidmsg = msg(10)
104200080327     c                   eval      h1riga = 16
104300080327     c                   eval      h1colo = 42
104400070620     c                   leavesr
104500070620     c                   endif
104600070621      *     distinta chiusa o annullata
104700070620     c                   if        dstfcf = 'S' or dstatb <> *blanks
104800070620     c                   eval      *in28 = *on
104900070621     c                   eval      vidmsg = msg(11)
105000080327     c                   eval      h1riga = 16
105100080327     c                   eval      h1colo = 42
105200070620     c                   leavesr
105300070620     c                   endif
105400080415      *     controllo esistenza fase 'RCH' o 'RQE'
105500080327     c                   exsr      sr_fase
105600080415      * se trova la fase (una delle due)
105700080415      * non si può più assegnare ORM a questa distinta
105800080415     c                   if        or90ndcok <> *blanks
105900080327     c                   eval      *in28 = *on
106000080327     c                   eval      vidmsg = msg(38)
106100080327     c                   eval      h1riga = 16
106200080327     c                   eval      h1colo = 42
106300080327     c                   leavesr
106400080327     c                   endif
106500070621      *     se tutto ok
106600070620     c                   move      v1ndc1        h1ndc_piu
106700070720     c                   eval      h1pdr_piu = dstpdr
106800070720     c                   eval      h1fpp_piu = dstfpp
106900070919     c                   eval      h1spi_piu = dstrspi
107000070620
107100070620     c                   endif
107200070621     c                   endif
107300070621
107400070621     ?* scelta (2) toglie dalla distinta
107500070621     c                   if        v1sce = '2'
107600070621      * --> numero distinta
107700070621     c                   if        v1ndc2 = *zeros
107800070621     c                   clear                   v1ndc2
107900070621     c                   endif
108000070621      *     ricerca
108100070621     c                   if        %scan('?':v1ndc2) > *zeros
108200070621     c                             or v1ndc2 = *blanks or v1ndc2 = *zeros
108300070621     c                   clear                   wndc
108400070621     c                   eval      w1sce = v1sce
108500070704     c                   exsr      ric_dist
108600070621
108700070621      * se tornando dalla ricerca distinta è stato scelto di fare un trasferimento
108800070621      *  invece di togliere e basta
108900070704     c                   if        fdgopz = '3'
109000070621     c                   eval      v1sce = '3'
109100070621     c                   eval      v1ndc3 = *zeros
109200070704     c                   move      fdgnfv        v1ndc3
109300070621     c                   goto      ini_ctrd1
109400070621     c                   endif
109500070621
109600070621     c                   eval      v1ndc2 = *zeros
109700070704     c                   move      fdgnfv        v1ndc2
109800070621     c                   endif
109900070621      *     deve essere numerica
110000070621     c                   if        %check(digits:v1ndc2) > 0
110100070704     c                   eval      vidmsg = msg(09)
110200070621     c                   eval      *in28 = *on
110300071008     c                   eval      h1riga = 18
110400070621     c                   eval      h1colo = 42
110500070621     c                   leavesr
110600070621     c                   endif
110700070621      *     controlla
110800070621     c                   move      v1ndc2        wnfv
110900070621     c                   eval      aggior_dist = 'N'
111000070704     c                   exsr      chain_dist
111100070621     c                   if        error_dist = 'S'
111200070621     c                   if        rec_allocato = 'S'
111300070621     c                   eval      vidmsg = msg(08)
111400070621     c                   endif
111500070621     c                   eval      *in28 = *on
111600071008     c                   eval      h1riga = 18
111700070621     c                   eval      h1colo = 42
111800070621     c                   leavesr
111900070621     c                   endif
112000070621      *     distinta non trovata
112100070621     c                   if        found_dist = 'N'
112200080912     c                   eval      *in28 = *on
112300070621     c                   eval      vidmsg = msg(09)
112400071008     c                   eval      h1riga = 18
112500070621     c                   eval      h1colo = 42
112600070621     c                   leavesr
112700070621     c                   endif
112800070621      *     distinta non nella data
112900070621     c                   if        dstdfv <> wddc
113000070621     c                   eval      *in28 = *on
113100070621     c                   eval      vidmsg = msg(10)
113200071008     c                   eval      h1riga = 18
113300070621     c                   eval      h1colo = 42
113400070621     c                   leavesr
113500070621     c                   endif
113600070621      *     distinta chiusa o annullata
113700070621     c                   if        dstfcf = 'S' or dstatb <> *blanks
113800070621     c                   eval      *in28 = *on
113900070621     c                   eval      vidmsg = msg(11)
114000071008     c                   eval      h1riga = 18
114100070621     c                   eval      h1colo = 42
114200070621     c                   leavesr
114300070621     c                   endif
114400080415      *     controllo esistenza fase 'RCH' o 'RQE'
114500080327     c                   exsr      sr_fase
114600080415      * se trova la fase (una delle due)
114700080415      * non si può più assegnare ORM a questa distinta
114800080415     c                   if        or90ndcok <> *blanks
114900080327     c                   eval      *in28 = *on
115000080327     c                   eval      vidmsg = msg(38)
115100080327     c                   eval      h1riga = 18
115200080327     c                   eval      h1colo = 42
115300080327     c                   leavesr
115400080327     c                   endif
115500070621      *     se tutto ok
115600070621     c                   move      v1ndc2        h1ndc_meno
115700080730     c                   eval      h1pdr_meno = dstpdr
115800070731     c                   move      dstfpp        h1fpp_meno
115900070622     c                   endif
116000070621
116100070621     ?* scelta (3) trasferisce dalla distinta alla distinta
116200070621     c                   if        v1sce = '3'
116300070621      * --> numero distinta
116400070621     c                   if        v1ndc3 = *zeros
116500070621     c                   clear                   v1ndc3
116600070621     c                   endif
116700070621      *     ricerca
116800070621     c                   if        %scan('?':v1ndc3) > *zeros
116900070621     c                             or v1ndc3 = *blanks or v1ndc3 = *zeros
117000070621     c                   clear                   wndc
117100070621     c                   eval      w1sce = v1sce
117200070704     c                   exsr      ric_dist
117300070621
117400070720      *     se tornando dalla ricerca distinta è stato scelto di fare un togli
117500070720      *     invece di trasferire e basta
117600070704     c                   if        fdgopz = '2'
117700070621     c                   eval      v1sce = '2'
117800070621     c                   eval      v1ndc2 = *zeros
117900070704     c                   move      fdgnfv        v1ndc2
118000070621     c                   goto      ini_ctrd1
118100070621     c                   endif
118200070621
118300070621     c                   eval      v1ndc3 = *zeros
118400070704     c                   move      fdgnfv        v1ndc3
118500070621     c                   endif
118600070621      *     deve essere numerica
118700070621     c                   if        %check(digits:v1ndc3) > 0
118800070704     c                   eval      vidmsg = msg(09)
118900070621     c                   eval      *in28 = *on
119000071008     c                   eval      h1riga = 20
119100070621     c                   eval      h1colo = 42
119200070621     c                   leavesr
119300070621     c                   endif
119400070621      *     controlla
119500070621     c                   move      v1ndc3        wnfv
119600070621     c                   eval      aggior_dist = 'N'
119700070704     c                   exsr      chain_dist
119800070621     c                   if        error_dist = 'S'
119900070621     c                   if        rec_allocato = 'S'
120000070621     c                   eval      vidmsg = msg(08)
120100070621     c                   endif
120200070621     c                   eval      *in28 = *on
120300071008     c                   eval      h1riga = 20
120400070621     c                   eval      h1colo = 42
120500070621     c                   leavesr
120600070621     c                   endif
120700070621      *     distinta non trovata
120800070621     c                   if        found_dist = 'N'
120900070621     c                   eval      *in28 = *on
121000070621     c                   eval      vidmsg = msg(09)
121100071008     c                   eval      h1riga = 20
121200070621     c                   eval      h1colo = 42
121300070621     c                   leavesr
121400070621     c                   endif
121500070621      *     distinta non nella data
121600070621     c                   if        dstdfv <> wddc
121700070621     c                   eval      *in28 = *on
121800070621     c                   eval      vidmsg = msg(10)
121900071008     c                   eval      h1riga = 20
122000070621     c                   eval      h1colo = 42
122100070621     c                   leavesr
122200070621     c                   endif
122300070621      *     distinta chiusa o annullata
122400070621     c                   if        dstfcf = 'S' or dstatb <> *blanks
122500070621     c                   eval      *in28 = *on
122600070621     c                   eval      vidmsg = msg(11)
122700071008     c                   eval      h1riga = 20
122800070621     c                   eval      h1colo = 42
122900070621     c                   leavesr
123000070621     c                   endif
123100080415      *     controllo esistenza fase 'RCH' o 'RQE'
123200080327     c                   exsr      sr_fase
123300080415      * se trova la fase (una delle due)
123400080415      * non si può più assegnare ORM a questa distinta
123500080415     c                   if        or90ndcok <> *blanks
123600080327     c                   eval      *in28 = *on
123700080327     c                   eval      vidmsg = msg(38)
123800080327     c                   eval      h1riga = 20
123900080327     c                   eval      h1colo = 42
124000080327     c                   leavesr
124100080327     c                   endif
124200070621      *     se tutto ok
124300070621     c                   move      v1ndc3        h1ndc_meno
124400080730     c                   eval      h1pdr_meno = dstpdr
124500070731     c                   move      dstfpp        h1fpp_meno
124600070621
124700070621      * --> numero distinta
124800070621     c                   if        v1ndc4 <> *blanks
124900070621
125000070621     c                   if        v1ndc4 = *zeros
125100070621     c                   clear                   v1ndc4
125200070621     c                   endif
125300070621      *     ricerca
125400070621     c                   if        %scan('?':v1ndc4) > *zeros
125500070621     c                   move      v1ndc3        wndc
125600070621     c                   eval      w1sce = '1'
125700070704     c                   exsr      ric_dist
125800070621     c                   clear                   wndc
125900070621     c                   eval      v1ndc4 = *zeros
126000070704     c                   move      fdgnfv        v1ndc4
126100070621     c                   endif
126200070621      *     deve essere numerica
126300070621     c                   if        %check(digits:v1ndc4) > 0
126400070704     c                   eval      vidmsg = msg(09)
126500070621     c                   eval      *in28 = *on
126600071008     c                   eval      h1riga = 20
126700070621     c                   eval      h1colo = 67
126800070621     c                   leavesr
126900070621     c                   endif
127000070621      *     controlla
127100070621     c                   move      v1ndc4        wnfv
127200070621     c                   eval      aggior_dist = 'N'
127300070704     c                   exsr      chain_dist
127400070621     c                   if        error_dist = 'S'
127500070621     c                   if        rec_allocato = 'S'
127600070621     c                   eval      vidmsg = msg(08)
127700070621     c                   endif
127800070621     c                   eval      *in28 = *on
127900071008     c                   eval      h1riga = 20
128000070621     c                   eval      h1colo = 67
128100070621     c                   leavesr
128200070621     c                   endif
128300070621      *     distinta non trovata
128400070621     c                   if        found_dist = 'N'
128500070621     c                   eval      *in28 = *on
128600070621     c                   eval      vidmsg = msg(09)
128700071008     c                   eval      h1riga = 20
128800070621     c                   eval      h1colo = 67
128900070621     c                   leavesr
129000070621     c                   endif
129100070621      *     distinta non nella data
129200070621     c                   if        dstdfv <> wddc
129300070621     c                   eval      *in28 = *on
129400070621     c                   eval      vidmsg = msg(10)
129500071008     c                   eval      h1riga = 20
129600070621     c                   eval      h1colo = 67
129700070621     c                   leavesr
129800070621     c                   endif
129900070621      *     distinta chiusa o annullata
130000070621     c                   if        dstfcf = 'S' or dstatb <> *blanks
130100070621     c                   eval      *in28 = *on
130200070621     c                   eval      vidmsg = msg(11)
130300071008     c                   eval      h1riga = 20
130400070621     c                   eval      h1colo = 67
130500070621     c                   leavesr
130600070621     c                   endif
130700070621      *     no uguale alla distinta alla distinta 'da'
130800070621     c                   if        v1ndc3 = v1ndc4
130900070621     c                   eval      *in28 = *on
131000070621     c                   eval      vidmsg = msg(09)
131100071008     c                   eval      h1riga = 20
131200070621     c                   eval      h1colo = 67
131300070621     c                   leavesr
131400070621     c                   endif
131500080415      *     controllo esistenza fase 'RCH' o 'RQE'
131600080327     c                   exsr      sr_fase
131700080415      * se trova la fase (una delle due)
131800080415      * non si può più assegnare ORM a questa distinta
131900080415     c                   if        or90ndcok <> *blanks
132000080327     c                   eval      *in28 = *on
132100080327     c                   eval      vidmsg = msg(38)
132200080327     c                   eval      h1riga = 20
132300080327     c                   eval      h1colo = 67
132400080327     c                   leavesr
132500080327     c                   endif
132600070621      *     se tutto ok
132700070621     c                   move      v1ndc4        h1ndc_piu
132800070720     c                   eval      h1pdr_piu = dstpdr
132900070720     c                   eval      h1fpp_piu = dstfpp
133000070919     c                   eval      h1spi_piu = dstrspi
133100070621     c                   endif
133200070622     c                   endif
133300070620
133400070620     c                   endsr
133500071108
133600071108      *------------------------------------------------------------------------*
133700071108      *   Controllo se ci sono proposte da analizzare              ?
133800071108      *------------------------------------------------------------------------*
133900071108     c     sr_controrp   begsr
134000071108
134100071108     c                   eval      *in15 = *off
134200071108
134300071108     C/EXEC SQL
134400071108     C+ DECLARE c1 CURSOR FOR SELECT fnorp00f.*
134500071108     C+ FROM fnorp00f
134600071108     C+ WHERE orppor=:v1cfgs and orpfev = ' '
134700071108     C/END-EXEC
134800071108
134900071108     C/EXEC SQL
135000071108     C+ OPEN C1
135100071108     C/END-EXEC
135200071108
135300071108     c                   do        *hival
135400071108
135500071108     C/EXEC SQL
135600071108     C+ FETCH NEXT FROM C1 INTO :fnorpds
135700071108     C/END-EXEC
135800071108
135900071108     c                   if        sqlcod = 100 or sqlcod < 0
136000071108     c                   leave
136100071108     c                   endif
136200071108
136300071108     c                   eval      *in15 = *on
136400071108     c                   leave
136500071108     c                   enddo
136600071108
136700071108     C/EXEC SQL
136800071108     C+ CLOSE C1
136900071108     C/END-EXEC
137000071108
137100071108     c                   endsr
137200070620
137300070620      *------------------------------------------------------------------------*
137400070705      *   Gestione sfl giri  da selezionare                        ?
137500070620      *------------------------------------------------------------------------*
137600070704     c     gess1         begsr
137700070620
137800070620      * inizializzazione videata
137900070620     c                   if        $inzs1 = *on
138000070704     c                   exsr      inzs1
138100070620     c                   eval      $inzs1 = *off
138200070620     c                   endif
138300070620
138400070620      * inizializza e imposta piede videata
138500070704     c                   exsr      inzz1
138600070620
138700070620      * emissione piede videata
138800070620      *                  __________________
138900070704     c                   write     or60z01
139000070620      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
139100070620
139200070620     c                   if        wmax = 0
139300070620      * non ci sono record
139400070620      *                  __________________
139500070620     c                   write     or60v01
139600070620      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
139700070620     c                   eval      *in20 = *off
139800070620     c                   else
139900131118      /free
140000131118       //?Ordino il subfile per ORA cut off ma solo quando emetto
140100131118       //?il subfile dopo la prima videata no da f12 dal secondo subfile
140200131118         IF not wordina;
140300131118           exsr ordina_sfl;
140400131118           wordina = *on;
140500131118         ENDIF;
140600131118      /end-free
140700131118
140800070718     c                   eval      *in20 = *on
140900070718     c                   eval      sav_wsfl =  wsfl
141000070718     c                   eval      sav_wpag =  wpag
141100070620
141200070620     c                   if        wsfl > *zeros
141300070620     c                   eval      vc1rcd = wsfl
141400070620     c                   else
141500070620     c                   if        wpag > *zeros
141600070620     c                   eval      vc1rcd = wpag
141700070620     c                   endif
141800070620     c                   endif
141900070620     c                   endif
142000070620
142100070620      * emissione sfl dei giri
142200070620      *                  __________________
142300070620     c                   exfmt     or60c01
142400070620      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
142500070620     c                   eval      *in28 = *off
142600070620     c                   eval      sav$gest = 'S1'
142700070720
142800070620     c                   if        vc1nrr <> *zeros
142900070620     c                   eval      wsfl = vc1nrr
143000070620     c                   endif
143100070620     c                   eval      vc1rcd = sflnrr
143200070620
143300070620      * selezioni
143400070620     c                   select
143500070620
143600070620      * f3=fine
143700070704     c                   when      $tasto = f03
143800070704     c                   exsr      f03_esci
143900070720     c                   rolbk
144000070620      * f5=int.tutte le distinte
144100070621     c                   when      $tasto = f05
144200070704     c                   exsr      f05_int
144300070620      * f12=ritorno
144400070621     c                   when      $tasto = f12
144500070620     c                   eval      $gest = 'D1'
144600070620     c                   eval      $inzd1 = *on
144700070720     c                   rolbk
144800070620      * f1=seleziona tutti
144900070621     c                   when      $tasto = f01
145000070704     c                   exsr      f01_1_sel
145100070620     c                   other
145200070620      * controllo subfile
145300070704     c                   exsr      ctrs1
145400070621     c   28              leavesr
145500070620      * f4=int.distinte stesso giro
145600070720      * ma deve esserne selezionata solo una altrimenti segnala l'errore
145700070621     c                   if        $tasto = f04
145800070620     c                   if        conta_giri = 1
145900070704     c                   exsr      f04_int
146000070720      * se non ha scelto la distinta deve eliminare la selezione sul sfl dei giri
146100070704     c                   if        fdgnfv  = *zeros
146200070621     c     sav_s1nrr     chain     or60s01                            30
146300070621     c                   if        not *in30
146400070620     c                   clear                   vs1sce
146500070620     c                   update    or60s01
146600070620     c                   endif
146700070620     c                   endif
146800070620
146900070620     c                   else
147000070620      * errore
147100070620     c                   eval      *in28 = *on
147200070704     c                   eval      $piugiri = *on
147300070621     c                   leavesr
147400070620     c                   endif
147500070620     c                   endif
147600070621
147700070621      * emette la finestra dei totali Distinta se non ci sono stati errori precedentemente
147800070622      * se non premuto enter ma F6 e immessa una distinta, e se è stato ripulito il messaggio
147900070621      * degli errori precedenti se il sfl è carico.
148000070704     c                   if        $tasto = f06 and $tasto <> enter and
148100070621     c                             msgz = *blanks and wmax > *zeros
148200070621     c                   eval      $inzs2 = *on
148300070621     c                   eval      $gest = 'S2'
148400070621     c                   eval      $viss2 = *off
148500070621     c                   leavesr
148600070621     c                   endif
148700070620
148800070620      * se premuto "enter" è stato richiesto il dettaglio di tutti gli ORM
148900070621      * relativi ai giri selezionati per selezione ORM.
149000070704     c                   if        $tasto = enter
149100070620     c                   eval      $inzs2 = *on
149200070620     c                   eval      $gest = 'S2'
149300070620     c                   eval      $viss2 = *on
149400070621     c                   endif
149500070620
149600070620     c                   endsl
149700070620
149800070620     c                   endsr
149900070620
150000070620      *------------------------------------------------------------------------*
150100070705      *   Inizializzazione e imposta il piede del sfl                 ?
150200070620      *------------------------------------------------------------------------*
150300070704     c     inzz1         begsr
150400070620
150500070620      * messaggio o tasti funzionali
150600070620     c                   clear                   z1dmsg
150700070620     c                   clear                   msgz
150800070620
150900070620      * errore di selezione di + giri x richiesta interr.stesso giro
151000070622     c                   if        $piugiri = *on
151100070620     c                   eval      msgz = msg(12)
151200070622     c                   eval      $piugiri = *off
151300070620     c                   endif
151400070622      * errore di distinta non trovata
151500070622     c                   if        $ndcerr = *on
151600070621     c                   eval      msgz = msg(17)
151700070622     c                   eval      $ndcerr = *off
151800070621     c                   endif
151900070622      * errore di distinta allocata
152000070622     c                   if        $ndcalloc = *on
152100070621     c                   eval      msgz = msg(18)
152200070622     c                   eval      $ndcalloc = *off
152300070621     c                   endif
152400070622      * errore di distinta chiusa
152500070622     c                   if        $ndcchiu = *on
152600070622     c                   eval      msgz = msg(19)
152700070622     c                   eval      $ndcchiu = *off
152800070622     c                   endif
152900070620
153000070620      * imposta eventuali errori
153100070620     c                   eval      z1dmsg = msgz
153200070620      * tasti funzionali
153300070620     c                   if        msgz = *blank
153400070620     c                   eval      *in28 = *off
153500070620     c                   eval      z1dmsg = fun_1
153600070621     c                   eval      %subst(z1dmsg:20:29) = fun_6
153700070621     c                   eval      %subst(z1dmsg:54:23) = fun_enter
153800070620     c                   endif
153900070620
154000070620     c                   endsr
154100070620
154200070620      *------------------------------------------------------------------------*
154300070705      *   Inizializzazione lista dei giri da selezionare              ?
154400070620      *------------------------------------------------------------------------*
154500070704     c     inzs1         begsr
154600070622
154700070622     c                   eval      $ndcalloc = *off
154800070622     c                   eval      $ndcchiu = *off
154900070622     c                   eval      $ndcerr = *off
155000070622     c                   eval      $piugiri = *off
155100070719     c                   eval      $viss2 = *off
155200131126     c                   eval      wordina = *on
155300070620      * pulizia sfl
155400070620     c                   eval      *in20 = *off
155500070620     c                   eval      *in21 = *off
155600070620      *                  __________________
155700070620     c                   write     or60c01
155800070620      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
155900070620     c                   eval      *in21 = *on
156000070620
156100070620      * righe x pagina del 1° sfl
156200070620     c                   eval      s1pag = rigpag_sfl1
156300070620
156400070620      * caricamento sfl totale
156500070620     c                   clear                   s1nrr
156600070620     c                   eval      vc1rcd = 1
156700070620     c                   clear                   wmax
156800070620     c                   clear                   wsfl
156900070620
157000070620     c                   clear                   vc1ndc1
157100070620
157200070620      * se immessa la distinta  disabilita tasti funzionali di ricerca
157300070620     c                   eval      *in40 = *off
157400070620     c                   if        h1ndc_piu > *zeros
157500070620     c                   eval      vc1ndc1 = 'alla_dist.:' +
157600080912     c                             %editc(h1ndc_piu:'Z')
157700070620     c                   endif
157800070620
157900070718      * preparo la stringa sql per estrarre i dati che servono
158000070718     c                   exsr      sr_sqlorg
158100070718
158200070718     C/EXEC SQL
158300070718     C+ PREPARE S1 FROM :stringasql
158400070718     C/END-EXEC
158500070718
158600070718     C/EXEC SQL
158700070718     C+ DECLARE A1 CURSOR FOR S1
158800070718     C/END-EXEC
158900070718
159000070718     C/EXEC SQL
159100070718     C+ OPEN A1
159200070718     C/END-EXEC
159300070720
159400070704     c                   exsr      rols1
159500070718
159600070718     C/EXEC SQL
159700070718     C+ CLOSE A1
159800070718     C/END-EXEC
159900070620
160000070620     c                   eval      wpag = 1
160100070620     c                   eval      sav_wmax = wmax
160200070620
160300070620     c                   endsr
160400070620
160500070620      *------------------------------------------------------------------------*
160600070705      *   Caricamento pagina lista                                    ?
160700070620      *------------------------------------------------------------------------*
160800070704     c     rols1         begsr
160900070620
161000070620     c                   eval      *in22 = *off
161100070620     c                   clear                   yy
161200070620     c                   eval      s1nrr = wmax
161300070620
161400070620      * caricamento del sfl x giro
161500070704     c                   exsr      write_s1
161600070620
161700070620     c                   eval      wmax = s1nrr
161800070620
161900070620      * posizionamento al primo rcd della pagina
162000070620     c                   eval      pagine = %div(s1nrr:s1pag)
162100070620     c                   eval      resto = %rem(s1nrr:s1pag)
162200070620     c                   eval      vc1rcd = pagine * s1pag
162300070620     c                   if        resto > *zeros
162400070620     c                   eval      vc1rcd = vc1rcd + 1
162500070620     c                   else
162600070620     c                   eval      vc1rcd = vc1rcd - s1pag
162700070620     c                   eval      vc1rcd = vc1rcd + 1
162800070620     c                   endif
162900070620
163000070620     c                   endsr
163100070620
163200070620      *------------------------------------------------------------------------*
163300070705      *   Scrive x ogni rottura di giro                              ?
163400070620      *------------------------------------------------------------------------*
163500070704     c     write_s1      begsr
163600131118
163700131118     c                   eval      wora = 2359
163800070620
163900070620      * esegue una lettura di posizionamento giro fuori per poi poter totalizzare
164000070718      * x giro
164100070704     c                   exsr      legge_rec
164200070620
164300070620     c                   dow       $efile = *off
164400070620
164500070620      * pulizia totali x giro
164600070620     c                   eval      savorgcgi = orgcgi
164700070620
164800070620      * finchè il giro è lo stesso
164900070620     c                   dow       $efile = *off and orgcgi = savorgcgi
165000070620
165100070620      * somma ai totali del giro
165200070620     c                   add       1             totnor_cgi
165300070620      * pesi volumi colli da ritirare
165400070718     c                   add       orgpkg        totpkg_cgi
165500070718     c                   add       orgvlm        totvlm_cgi
165600131119
165700131115      /free
165800131115       //?Cerco l'ora di CUT OFF più basse per il giro che sto
165900131115       //?caricando
166000131127         //exsr cercaCUTOFF;
166100140319         //IF  OOR2lrnc < wora and OOR2lrnc > 0;
166200140319         //  wora = OOR2lrnc;
166300131127         //ENDIF;
166400131115      /end-free
166500070620
166600070620      * esce da qui con un record di totale x giro
166700070704     c                   exsr      legge_rec
166800070620     c                   enddo
166900070620
167000070620      * scrive la riga di sfl a rottura di giro
167100070620     c                   clear                   or60s01
167200070622      * codice giro
167300070620     c                   eval      vs1cgi = savorgcgi
167400070620     c                   eval      write_riga = 'S'
167500070620      * descrizione giro
167600070620     c                   eval      savcgi = vs1cgi
167700070704     c                   exsr      ctrgiro
167800070620     c                   eval      vs1des = d09odes
167900070620      * se non deve andare in distinta
168000070620     c                   if        §grgdst = 'N'
168100070620     c                   eval      h1indis =  §grgdst
168200070620     c                   endif
168300070620      * totali orm
168400070913      * n.ORM
168500070620     c                   eval      vs1tnor = totnor_cgi
168600070913      * peso
168700070910     c                   if        totpkg_cgi > 999999,9
168800070910     c                   eval      totpkg_cgi = 999999,9
168900070910     c                   endif
169000070620     c                   eval      vs1tpkg = totpkg_cgi
169100070913      * volume
169200070910     c                   if        totvlm_cgi > 99,999
169300070910     c                   eval      totvlm_cgi = 99,990
169400070910     c                   endif
169500070620     c                   eval      vs1tvlm = totvlm_cgi
169600131118
169700131118     c                   eval      vs1ora = wora
169800070620
169900070620      * se selezionato un solo giro viene impostato automaticamente la selezione della
170000070622      * riga con il sflnxtchg acceso
170100070620     c                   if        v1ccgi <> *blank
170200070620     c                   eval      vs1sce = '1'
170300070620     c                   eval      *in22 = *on
170400070620     c                   endif
170500070620
170600070620     c                   if        write_riga = 'S'
170700070620     c                   eval      s1nrr = s1nrr + 1
170800070620     c                   eval      yy = yy +1
170900070704      *                  __________________
171000070620     c                   write     or60s01
171100070704      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
171200070620     c                   endif
171300070718
171400070718     c                   clear                   totnor_cgi
171500070718     c                   clear                   totpkg_cgi
171600070718     c                   clear                   totvlm_cgi
171700131118     c                   eval      wora = 2359
171800070620
171900070620     c                   enddo
172000070620
172100070620     c                   endsr
172200070620
172300070620      *------------------------------------------------------------------------*
172400070705      *   Lettura rcd archivio pilota                                ?
172500070620      *------------------------------------------------------------------------*
172600070704     c     legge_rec     begsr
172700070620
172800070620      * esce per fine file
172900070620     c                   eval      $efile = *off
173000070620      * per rcd ok deve uscire riportando i dati
173100070620     c                   eval      $rcdok = *off
173200070620
173300070620     c                   dou       $efile = *on or $rcdok = *on
173400070718
173500070718     C/EXEC SQL
173600071025     C+ FETCH NEXT FROM A1 INTO :fnorgds, :fnormds
173700070718     C/END-EXEC
173800070718
173900070718      * errore nel sql esco dal pgm
174000070718     c                   if        sqlcod < 0
174100070718     c                   eval      *inlr = *on
174200070718     c                   return
174300070718     c                   endif
174400070622      * fine file
174500070718     c                   if        sqlcod = 100
174600070620     c                   eval      $efile = *on
174700070620     c                   eval      *in23 = $efile
174800070718
174900070620     c                   else
175000070718
175100071022      *  per Assegnare gli ORM deve controllare se l'ORM può essere assegnato
175200071022     c                   if        v1sce = '1'
175300070718     c                   exsr      check_orm
175400070718     c                   if        or64asso <> 'N'
175500070718     c                   eval      $rcdok = *on
175600070718     c                   endif
175700071022     c                   endif
175800071022
175900071022      * Per Togliere o Trasfererire
176000071022     c                   if        v1sce <> '1'
176100071022     c                   eval      $rcdok = *on
176200071022     c                   endif
176300070718
176400070718     c                   endif
176500070718
176600070620     c                   enddo
176700070620
176800070620     c                   endsr
176900131115      /free
177000131115       //--------------------------------------------------------------
177100131115       //?Cerco l'orari di CUT OFF
177200131115       //--------------------------------------------------------------
177300131115       BEGSR cercaCUTOFF;
177400140319
177500140319         clear trulorsds;
177600140319         clear trulor2ds;
177700140319         IOREfil = ORMpor;
177800140319         IOREcap = ORMcar;
177900140319         IOREloc = ORMlor;
178000140319         IOREnar = ORMnar;
178100140319         IOREdta = ORMdar;
178200140319         IOREtser = 'R';
178300140319         trulorsr (kpjba:trulorsds:trulor2ds);
178400140319         IF  OOREerr <> *blanks or OOR2err <> *blanks;
178500140319           leavesr;
178600140319         ENDIF;
178700131115
178800131115       ENDSR;
178900131118
179000131118       //--------------------------------------------------------------
179100131118       //?Ordino il subfile per Ora Cut Off
179200131118       //--------------------------------------------------------------
179300131118       BEGSR Ordina_Sfl;
179400131118
179500131118       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
179600131118       // presente nel subfile -- "Selected1"?-- questo è aggiunto al record.
179700131118       // il campo è usato per selezionare i records dando un ordine a quelli
179800131118       // selezionati davanti ai non selezionati.
179900131118         clear QLGSCB;
180000131118         clear QLGSCB00;
180100131118         rrnlast = S1nrr;
180200131118
180300131118       // 1 campi chiave x Ora
180400131118         QLGNBRK = 1;
180500131118
180600131118       // imposto la posizione della Mittente sul subfile e la lunghezza
180700131118       // l'ordinamento è su un campo alfanumerico e deve essere ascending
180800131118         QLGSP = 1 + %size(VS1cgi) + %size(VS1des) +
180900131118                     %size(VS1tnor) + %size(VS1tpkg) +
181000131118                     %size(VS1tvlm);
181100131118         QLGSS = %size(VS1ora);
181200131118         QLGDT = Numerico;
181300131118         QLGSO = Ascendente;
181400131118         QLGKL(1) = QLGSKL;
181500131118
181600131118       // Load other sort parameters.
181700131118         QLGLB = 80 + 16 * MaxKey;
181800131118         QLGRL = %size(sflrcd1) - 1;
181900131118         QLGRT = 8;
182000131118         QLGOKL = 80;
182100131118         QLGLKE = 16;
182200131118         QLGLSS = 290;
182300131118
182400131118       // Initialize Sort I/O API fields.
182500131118         QLGRL00 = QLGRL;
182600131118         QLGRC00 = 1;
182700131118         clear QUSEI;
182800131118         QUSBPRV = %size(QUSEC);
182900131118
183000131118      /end-free
183100131118      * First step - Initialize the sort routine.
183200131118     c                   call      'QLGSORT'
183300131118     c                   parm                    QLGSCB
183400131118     c                   parm                    NotUsed
183500131118     c                   parm                    NotUsed
183600131118     c                   parm                    SizeList
183700131118     c                   parm                    ReturnSize
183800131118     c                   parm                    QUSEC
183900131118      /free
184000131118
184100131118       // Next step - Write records to I/O routine.
184200131118         QLGRT00 = Put;
184300131118         for xx = 1 to rrnlast;
184400131118          chain xx OR60S01;
184500131118
184600131118       // solo le righe con Selected = 'Y' sono riordinate,
184700131118       // quindi per fare un ordinamento di tutte le righe
184800131118       // metto 'Y' sempre.
184900131118          selected1 = 'Y';
185000131118          clear QUSEI;
185100131118          QUSBPRV = %SIZE(QUSEC);
185200131118
185300131118      /end-free
185400131118     c                   call      'QLGSRTIO'
185500131118     c                   parm                    QLGSCB00
185600131118     c                   parm                    SflRcd1
185700131118     c                   parm                    NotUsed
185800131118     c                   parm                    SizeList
185900131118     c                   parm                    NotUsed
186000131118     c                   parm                    QUSEC
186100131118      /free
186200131118
186300131118         endfor;
186400131118
186500131118       // Next step - Signal end of input, clear subfile for reload.
186600131118         QLGRT00 = EndPut;
186700131118         clear QUSEI;
186800131118         QUSBPRV = %SIZE(QUSEC);
186900131118
187000131118      /end-free
187100131118     c                   call      'QLGSRTIO'
187200131118     c                   parm                    QLGSCB00
187300131118     c                   parm                    SflRcd1
187400131118     c                   parm                    NotUsed
187500131118     c                   parm                    SizeList
187600131118     c                   parm                    NotUsed
187700131118     c                   parm                    QUSEC
187800131118      /free
187900131118
188000131118       // pulizia SFL
188100131118         *in20 = *off;
188200131118         *in21 = *off;
188300131118         write OR60C01;
188400131118         *in21 = *on;
188500131118
188600131118       // Final step - Write the records back to the subfile.
188700131118         QLGRT00 = Get;
188800131118         for xx = 1 to rrnlast;
188900131118          clear QUSEI;
189000131118          QUSBPRV = %size(QUSEC);
189100131118
189200131118      /end-free
189300131118     c                   call      'QLGSRTIO'
189400131118     c                   parm                    QLGSCB00
189500131118     c                   parm                    NotUsed
189600131118     c                   parm                    SflRcd1
189700131118     c                   parm                    QLGRL00
189800131118     c                   parm                    NotUsed
189900131118     c                   parm                    QUSEC
190000131118      /free
190100131118
190200131118          s1nrr = xx;
190300131118          write OR60S01;
190400131118         endfor;
190500131118
190600131118       ENDSR;
190700131118
190800131115      /end-free
190900070621
191000070621      *------------------------------------------------------------------------*
191100070705      *   Controllo opzioni lista                                   ?
191200070621      *------------------------------------------------------------------------*
191300070704     c     ctrs1         begsr
191400070621
191500070621      * pulizia campi totali
191600070621     c                   clear                   totnor_cgi
191700070621     c                   clear                   totpkg_cgi
191800070621     c                   clear                   totvlm_cgi
191900070621
192000070621      * pulizia campi totali
192100070621     c                   clear                   totnor_s1
192200070621     c                   clear                   totpkg_s1
192300070621     c                   clear                   totvlm_s1
192400070621
192500070621     c                   clear                   conta_giri
192600070621     c                   eval      $esci = *off
192700070621     c                   eval      *in28 = *off
192800070621
192900070621      * leggo il sfl solo se ci sono rcd
193000070621     c                   if        wmax > *zeros
193100070621     c                   readc     or60s01                                30
193200070621
193300070621      * esce se fine sfl o errore che richiede l'uscita
193400070621     c                   dow       not *in30 and $esci = *off
193500070621     c                   eval      vc1rcd = s1nrr
193600070621      * ctrl su riga
193700070704     c                   exsr      recs1
193800070621
193900070621      * se rilevato errore o richiesta uscita, attivo sflnxtchg
194000070621     c                   if        *in28 or $esci = *on
194100070720      * disabilito l'eventuale richiesta di reinizializzazione sfl
194200070622      * la ripristinerò a conclusione del ciclo di readc
194300070621     c                   eval      $inzs1 = *off
194400070621     c                   endif
194500070621
194600070622      * attivo sempre il sflnxtchg e aggiorno sfl
194700070621     c                   eval      *in22 = *on
194800070621     c                   update    or60s01
194900070621
195000070621      * leggo prossimo rcd dal sfl se no richiesta uscita ciclo
195100070621     c                   if        $esci = *off
195200070621     c                   readc     or60s01                                30
195300070621
195400070621      * a fine ciclo ripristino il flag richiesta iniz.sfl
195500070621     c                   if        *in30
195600070621      * calcolo pagina a cui deve posizionarsi
195700070704     c                   eval      wpagine = s1pag
195800070704     c                   exsr      clcpag
195900070621     c                   endif
196000070621     c                   endif
196100070621
196200070621     c                   enddo
196300070621
196400070621     c                   endif
196500070621
196600070622      * imposta i totali come se avesse eseguito il 2°sfl
196700070621     c                   eval      totnor_s2 = totnor_s1
196800070621     c                   eval      totpkg_s2 = totpkg_s1
196900070621     c                   eval      totvlm_s2 = totvlm_s1
197000070621
197100070621      * controlli generali sulle scelte fatte
197200070621     c                   if        msgz = *blank
197300070621
197400070621      * se non è stata scelto nemmeno un giro... errore
197500070621     c                   clear                   err001
197600070621     c                   clear                   err002
197700070621     c                   clear                   err003
197800070621
197900070621     c                   if        conta_giri = 0
198000070621     c                   eval      err001 = msg(13)
198100070621     c                   eval      err002 = msg(14)
198200070621     c                   clear                   err003
198300070621     c                   else
198400070720      * se non è stata scelta nemmeno una distinta... errore
198500070621     c                   if        h1ndc_piu = 0
198600070621     c                   if        $tasto <> f04 and $tasto <> f05 and
198700070622     c                             $tasto <> enter
198800070621      * emette messaggio di avviso x scegliere una distinta se non c'è
198900070621     c                   eval      err001 = msg(15)
199000070621     c                   clear                   err002
199100070621     c                   eval      err003 = msg(16)
199200070621     c                   endif
199300070621     c                   endif
199400070621     c                   endif
199500070621
199600070621      * messaggio di avviso
199700070621     c                   if        err001 <> *blank or
199800070621     c                             err002 <> *blank or
199900070621     c                             err003 <> *blank
200000070621     c                   eval      *in28 = *on
200100070621      *                  __________________
200200070621     c                   exfmt     or60wer
200300070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
200400070621     c                   endif
200500070621
200600070621     c                   endif
200700070621
200800070621     c                   endsr
200900070621
201000070621      *------------------------------------------------------------------------*
201100070705      *   Controllo campi i/o riga lista                            ?
201200070621      *------------------------------------------------------------------------*
201300070704     c     recs1         begsr
201400070621
201500070621      * se è stato scelto ed è un giro che non può essere messo
201600070720      * in distinta errore
201700070621     c                   if        vs1sce = '1' and h1indis = 'N'
201800070622     c                   eval      *in28 = *on
201900070621     c                   leavesr
202000070621     c                   endif
202100070621
202200070621      * gestione opzioni
202300070621     c                   if        vs1sce <> *blank
202400070704     c                   exsr      sce_s1
202500070621     c                   endif
202600070621
202700070621     c                   endsr
202800070621
202900070621      *------------------------------------------------------------------------*
203000070705      *   Gestione opzioni lsita                                    ?
203100070621      *------------------------------------------------------------------------*
203200070704     c     sce_s1        begsr
203300070621
203400070720      * totalizza gli ORM/pesi/volumi/colli
203500070621     c                   if        vs1sce = '1'
203600070621     c                   eval      conta_giri = conta_giri + 1
203700070621     c                   eval      sav_sxcgi = vs1cgi
203800070621     c                   eval      sav_s1nrr = s1nrr
203900070621     c                   add       vs1tnor       totnor_s1
204000070621     c                   add       vs1tpkg       totpkg_s1
204100070621     c                   add       vs1tvlm       totvlm_s1
204200070621     c                   endif
204300070621
204400070621     c                   endsr
204500070622
204600070621      *------------------------------------------------------------------------*
204700070705      *   Gestione sfl ORM da selezionare                        ?
204800070621      *------------------------------------------------------------------------*
204900070704     c     gess2         begsr
205000070621
205100070621      * inizializzazione videata
205200070621     c                   if        $inzs2 = *on
205300070704     c                   exsr      inzs2
205400070621     c                   eval      $inzs2 = *off
205500070621     c                   endif
205600070621
205700070621      * se non si sta eseguendo l'assegnazione oppure se siamo in assegnazione
205800070621      * ed è stato richiesto di selezionare gli ORM dei giri selezionati
205900070621     c                   if        $viss2 = *on or v1sce <> '1'
206000070621
206100070621      * inizializza e imposta piede videata
206200070704     c                   exsr      inzz2
206300070621
206400070621      * emissione piede videata
206500070621      *                  __________________
206600070621     c                   write     or60z02
206700070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
206800070621     c                   if        wmax = *zeros
206900070621      * non ci sono records
207000070621      *                  __________________
207100070621     c                   write     or60v01
207200070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
207300070621     c                   eval      *in20 = *off
207400070621     c                   else
207500070621     c                   eval      *in20 = *on
207600070621
207700070621     c                   if        posizionamento <> 'S'
207800080612     c                   if        posizionato =  'S'
207900070621     c                   eval      vc2rcd = vc2rcd_pos
208000070621     c                   else
208100070621     c                   if        wsfl > *zeros
208200070622      * essendo il posizionamento *top non è + corretto impostare direttamente
208300070621      * il record ma occorre ricavare il primo record della pagina.
208400070621     c                   clear                   vc2rcd
208500080303
208600070621     c                   if        wrigsfl2 > *zeros
208700070621     c                   eval      vc2rcd = %div(wsfl:wrigsfl2)
208800080303     c                   eval      resto = %rem(wsfl:wrigsfl2)
208900080303     c                   if        vc2rcd > *zeros
209000070704     c                   eval      vc2rcd = vc2rcd * wrigsfl2
209100080303     c                   if        resto > *zeros
209200080303     c                   eval      vc2rcd = vc2rcd + 1
209300080303     c                   endif
209400080303     c                   else
209500080303     c                   eval      vc2rcd = 1
209600080303     c                   endif
209700080303     c                   endif
209800080303      * attenzione all'incremento di 1 può far schiantare il sfl
209900080303      * poichè può superare il nr.max delle righe presenti.
210000080303      * esempio se wsfl = 16 o un suo multiplo essendo wrigsfl2 = 16
210100080303      * c2rcd alla fine equivale a wsfl ossia 16 ....e se in seguito
210200080303      * si aggiunge 1 al sflrcdnbr andiamo fuori limite max record
210300080303      * caricati e si schianta in emissione del sfl.
210400080303     c                   if        resto = *zeros
210500080303     c                   eval      vc2rcd = vc2rcd - (wrigsfl2 - 1)
210600080303     c                   else
210700080303     c                   if        vc2rcd = *zeros
210800080303     c                   add       1             vc2rcd
210900080303     c                   endif
211000080303     c                   endif
211100070621     c                   else
211200070621     c                   if        wpag > *zeros
211300070704     c                   eval      vc2rcd = wpag
211400070621     c                   endif
211500070621     c                   endif
211600070621     c                   endif
211700070621     c                   endif
211800070621     c                   endif
211900070621
212000070622      *  emissione sfl degli ORM
212100070621      *                  __________________
212200070621     c                   exfmt     or60c02
212300070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
212400070621     c                   eval      *in28 = *off
212500070621     c                   eval      sav$gest = 'S2'
212600070621
212700070621     c                   if        vc2nrr <> *zeros
212800070621     c                   eval      wsfl = vc2nrr
212900070621     c                   endif
213000070621     c                   eval      vc2rcd = sflnrr
213100070621     c                   eval      vc2rcd_pos = sflnrr
213200070621
213300070621     c                   endif
213400070621      * selezioni
213500070621     c                   select
213600070621      * f3=fine
213700070621     c                   when      $tasto = f03
213800070704     c                   exsr      f03_esci
213900070720     c                   rolbk
214000070621
214100070621      * f1=seleziona tutti
214200070704     c                   when      $tasto = f01
214300070704     c                   exsr      f01_2_sel
214400070621
214500070621      * f5=int.tutte le distinte
214600070621     c                   when      $tasto = f05
214700070704     c                   exsr      f05_int
214800070621
214900070621      * f07=ordinamento x peso
215000070621     c                   when      $tasto = f07
215100070621     c                   eval      wrksort = 6
215200070704     c                   exsr      clear_ordina
215300070704     c                   exsr      ordina_sfl2
215400070621
215500070621      * f08=ordinamento x volume
215600070704     c                   when      $tasto = f08
215700070621     c                   eval      wrksort = 5
215800070704     c                   exsr      clear_ordina
215900070704     c                   exsr      ordina_sfl2
216000070621
216100070621      * f10=ordinamento x località
216200070704     c                   when      $tasto = f10
216300070621     c                   eval      wrksort = 4
216400070704     c                   exsr      clear_ordina
216500070704     c                   exsr      ordina_sfl2
216600070621
216700080128      * f11=ordinamento x ora
216800070704     c                   when      $tasto = f11
216900070621     c                   eval      wrksort = 3
217000070704     c                   exsr      clear_ordina
217100070704     c                   exsr      ordina_sfl2
217200070621
217300070621      * f13=ordinamento x mittente
217400070704     c                   when      $tasto = f13
217500070621     c                   eval      wrksort = 2
217600070704     c                   exsr      clear_ordina
217700070704     c                   exsr      ordina_sfl2
217800070621
217900070621      * f14=ordinamento x giro/mittente
218000070704     c                   when      $tasto = f14
218100070621     c                   eval      wrksort = 1
218200070704     c                   exsr      clear_ordina
218300070704     c                   exsr      ordina_sfl2
218400070621
218500070621      * f12=ritorno
218600070704     c                   when      $tasto = f12
218700070719     c                   if        v1sce = '1'
218800070719     c                   if        v1cnor = *zeros
218900070621     c                   eval      $gest = 'S1'
219000070621     c                   eval      wsfl = vc1rcd
219100131118     c                   IF        waggiorna
219200131118     c                   eval      $inzs1 = *on
219300131118     c                   eval      waggiorna = *off
219400131118     c                   clear                   con_giro
219500131118     c                   ENDIF
219600070719     c                   endif
219700070719     c                   if        v1cnor <> *zeros
219800070719     c                   eval      $gest = 'D1'
219900070719     c                   eval      $inzd1 = *on
220000070720     c                   rolbk
220100070719     c                   endif
220200070621     c                   else
220300070720      * se toglie o trasferisce
220400070720      * vado direttamente sull'interrogazione distinte
220500080611      * ma se richiamato dal Monitor e si deve tornare alla scelta
220600080611      * sulla distinta.
220700080611     c                   if        pgm_chiamante = 'MONITOR'
220800080611     c                             and v1sce <> '2'
220900080611     c                   eval      v1sce = '2'
221000080611     c                   end
221100080611      *
221200070621     c                   eval      salva_v1sce = v1sce
221300070621     c                   eval      salta_video1 = 'S'
221400070621     c                   eval      $gest = 'D1'
221500070621     c                   eval      $inzd1 = *off
221600070704     c                   exsr      inzd1
221700070621     c                   eval      v1sce = salva_v1sce
221800070720     c                   rolbk
221900070621     c                   endif
222000070621
222100070621     c                   other
222200070622      * controllo testata
222300070704     c                   exsr      ctrc2
222400070621     c   28              leavesr
222500070622      * controllo subfile
222600070918     c                   exsr      ctrs2
222700070621     c   28              leavesr
222800070621
222900070621      * f4=int.distinte stesso giro
223000070621     c                   if        $tasto = f04
223100070621     c                   if        giri_scelti = 1
223200070704     c                   exsr      f04_int
223300070621     c                   else
223400070621      * errore x + giri con F4 (x un solo giro)
223500070621     c                   eval      *in28 = *on
223600080612     c                   eval      $piugiri = *on
223700070621     c                   leavesr
223800070621     c                   endif
223900070621     c                   endif
224000070621
224100070720      * se NON premuto F9 e immessa una distinta, emette la finestra dei totali Distinta
224200070720      * se non ci sono stati errori precdentemente e se è stato ripulito il messaggio
224300070720      * degli errori precedenti se il sfl è carico.
224400080611      * e non è stata fatta l'opzione di chiudi INFO
224500070621     c                   if        msgz = *blanks and wmax > *zeros
224600131127     c                             and vs2sce <> '5' and vs2sce <> '8'
224700140212     c                             and vs2sce <> '9'
224800070621
224900070704     c                   if        (h1ndc_piu > *zeros and v1sce = '1' and
225000071008     c                              totnor_s2 > *zeros) or
225100070621     c                             (v1sce = '2' and totnor_s2 > *zeros) or
225200070621     c                             (v1sce = '3' and totnor_s2 > *zeros and
225300070621     c                              h1ndc_piu > *zeros)
225400070621     c                   eval      $inzw1 = *on
225500070621     c                   eval      $gest = 'W1'
225600070621     c                   else
225700131127      * emetto il messaggio solo la selezione fatta prevede la richiesta della distinta
225800131127     c                   IF        not wnowin
225900070621      * emette messaggio di Avviso x scegliere una Distinta se non c'è
226000070621      * se si era scelta l'opzione di assegnazione e non si era immessa
226100070621      * una distinta
226200070621     c                   if        totnor_s2 = *zeros
226300070621     c                   eval      err001 = msg(13)
226400070621     c                   eval      err002 = msg(14)
226500070621     c                   clear                   err003
226600071015     c                   eval      $ormcomun = *off
226700080314     c                   eval      $ormesitono = *off
226800080314     c                   eval      $ormesitook = *off
226900080314     c                   eval      $ormpda = *off
227000071012     c                   eval      wforzaspi = *off
227100070621     c                   else
227200070621     c                   eval      err001 = msg(15)
227300070621     c                   clear                   err002
227400070621     c                   eval      err003 = msg(16)
227500070621     c                   endif
227600070621      *                  __________________
227700070621     c                   exfmt     or60wer
227800070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
227900071012     c                   leavesr
228000070621     c                   endif
228100070621     c                   endif
228200131127     c                   ENDIF
228300070621
228400070621     c                   endsl
228500070925
228600070925      * se ho scelto una distinta controllo la sponda idraulica
228700070925     c                   if         h1ndc_piu > *zeros and
228800070925     c                             (v1sce = '1' or v1sce = '3') and
228900070925     c                              $ormspi = *on and wforzaspi = *off
229000070925      * se ORM con sponda e distinta senza emetto window con errore
229100070925     c                   eval      err001 = msg(31)
229200070925     c                   eval      err002 = msg(32)
229300070925     c                   clear                   err003
229400070925      *                  __________________
229500070925     c                   exfmt     or60wer
229600070925      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
229700070925     c                   eval       wforzaspi = *on
229800070925     c                   leavesr
229900070925     c                   endif
230000070621
230100070621     c                   endsr
230200070621
230300070621      *------------------------------------------------------------------------*
230400070705      *   Inizializzazione e imposta il piede del sfl               ?
230500070621      *------------------------------------------------------------------------*
230600070704     c     inzz2         begsr
230700070621
230800070720      * pulisco messaggio o tasti funzionali
230900070621     c                   clear                   z2dmsg
231000070621     c                   clear                   msgz
231100070621
231200070622      * errore di selezione di + giri x richiesta interr.stesso giro
231300070622     c                   if        $piugiri = *on
231400070622     c                   eval      msgz = msg(12)
231500070622     c                   eval      $piugiri = *off
231600070622     c                   endif
231700070622      * errore di distinta non trovata
231800070622     c                   if        $ndcerr = *on
231900070622     c                   eval      msgz = msg(17)
232000070622     c                   eval      $ndcerr = *off
232100070622     c                   endif
232200070622      * errore di distinta allocata
232300070622     c                   if        $ndcalloc = *on
232400070622     c                   eval      msgz = msg(18)
232500070622     c                   eval      $ndcalloc = *off
232600070622     c                   endif
232700070622      * errore di distinta chiusa
232800070622     c                   if        $ndcchiu = *on
232900070622     c                   eval      msgz = msg(19)
233000070622     c                   eval      $ndcchiu = *off
233100070622     c                   endif
233200070622      * stessa distinta mette/toglie
233300070720     c                   if        *in41
233400070622     c                   eval      msgz = msg(20)
233500070720     c                   eval      *in41 = *off
233600070622     c                   clear                   h1ndc_piu
233700070720     c                   clear                   h1pdr_piu
233800070720     c                   clear                   h1fpp_piu
233900070919     c                   clear                   h1spi_piu
234000070622     c                   endif
234100080612
234200080612      * se ho il 27 acceso scelta errata
234300080612     c                   if        *in27
234400080612     c                   eval      msgz = msg(37)
234500080612     c                   eval      *in27 = *off
234600080612     c                   endif
234700080612
234800080612      * se ho il 25 acceso (errore sul subfile per ORM già esitato)
234900080612      * imposto il messaggio a video
235000080612     c                   if        *in25
235100080612     c                   select
235200080612      * ORM con esito OK da PDA avviso
235300080612     c                   when      $ormesitook = *on
235400080612     c                   eval      msgz = msg(35)
235500080612     c                   eval      $ormesitook = *off
235600080612      * ORM con esito NON OK da PDA avviso
235700080612     c                   when      $ormesitono = *on
235800080612     c                   eval      msgz = msg(36)
235900080612     c                   eval      $ormesitono = *off
236000080612     c                   endsl
236100080612     c                   eval      *in25 = *off
236200080612     c                   endif
236300080612      * se ho il 26 acceso (errore sul subfile per ORM già comunicato)
236400080612      * imposto il messaggio a video
236500080612     c                   if        *in26
236600080612     c                   select
236700080612      * ORM già comunicato a PDA
236800080612     c                   when      $ormpda = *on
236900080612     c                   eval      msgz = msg(34)
237000080612     c                   eval      $ormpda = *off
237100080612      * ORM già comunicato ad AUT
237200080612     c                   when      $ormcomun = *on
237300080612     c                   eval      msgz = msg(33)
237400080612     c                   eval      $ormcomun = *off
237500080612     c                   endsl
237600080612     c                   eval      *in26 = *off
237700080612     c                   endif
237800070622
237900070621      * imposta eventuali errori
238000070622     c                   eval      z2dmsg = msgz
238100070622
238200070621      * tasti funzionali se non è stato emesso alcun messaggio
238300070919     c                   if        msgz = *blank
238400070622     c                   eval      *in28 = *off
238500070622     c                   endif
238600070622
238700070621      * se è stato scelto l'assegnazione (1) e non è stato immessa la distinta
238800070622     c                   if        v1sce = '1' or v1sce = '3'
238900070622     c                   eval      *in40 = *off
239000070621     c                   else
239100070622     c                   eval      *in40 = *on
239200070720     c                   endif
239300070622
239400070622     c                   endsr
239500070622
239600070622      *------------------------------------------------------------------------*
239700070705      *   Inizializzazione lista ORM da giri o da distinta
239800070622      *------------------------------------------------------------------------*
239900070704     c     inzs2         begsr
240000070622
240100070720     c                   eval      *in41 = *off
240200070622     c                   eval      $ndcalloc = *off
240300070622     c                   eval      $ndcchiu = *off
240400070622     c                   eval      $ndcerr = *off
240500070622     c                   eval      $piugiri = *off
240600070919     c                   eval      $ormspi = *off
240700070919     c                   eval      wforzaspi = *off
240800070921     c                   eval      $ormcomun = *off
240900080314     c                   eval      $ormesitono = *off
241000080314     c                   eval      $ormesitook = *off
241100080314     c                   eval      $ormpda = *off
241200131119     c                   clear                   rrnlast
241300080613
241400080613      * imposto le opzioni del subfile
241500080613     c                   clear                   opz
241600131126     c                   select
241700131126     c                   when      v1sce = '1'
241800080718     c                   if        not *in17
241900080718     c                   eval      v2copz = dopz
242000131126     c                   eval      v2copz2 = dopz2
242100080718     c                   movea     wopz          opz
242200080613     c                   else
242300080718     c                   eval      v2copz = dopz1
242400140211     c                   eval      v2copz2 = dopz4
242500080718     c                   movea     wopz1         opz
242600080613     c                   endif
242700131126     c                   other
242800131126     c                   clear                   v2copz2
242900131126     c                   if        not *in17
243000131126     c                   eval      v2copz = dopz
243100131126     c                   movea     wopza         opz
243200131126     c                   else
243300131126     c                   eval      v2copz = dopz1
243400140211     c                   eval      v2copz2 = dopz3
243500131126     c                   movea     wopzb         opz
243600131126     c                   endif
243700131126     c                   endsl
243800070622
243900070622      * pulizia sfl
244000070622     c                   eval      *in20 = *off
244100070622     c                   eval      *in21 = *off
244200070622      *                  __________________
244300070622     c                   write     or60c02
244400070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
244500070622     c                   eval      *in21 = *on
244600070622
244700070622      * righe x pagina del 2° sfl
244800070622     c                   eval      s2pag = rigpag_sfl2
244900070622
245000070622     c                   eval      wrksort  = ord_giromit
245100070622     c                   eval      windsort = ord_giromit
245200070622
245300070622      * caricamento sfl totale
245400070622     c                   clear                   s2nrr
245500070622     c                   eval      vc2rcd = 1
245600070622     c                   clear                   wmax
245700070622     c                   clear                   wsfl
245800070720
245900070622     c                   clear                   vc2pos1
246000070622     c                   clear                   vc2pos2
246100070622     c                   clear                   vc2pos3
246200070622     c                   clear                   vc2pos4
246300070622     c                   clear                   vc2pos5
246400070622     c                   clear                   vc2pos6
246500070622     c                   clear                   sav_pos1
246600070622     c                   clear                   sav_pos2
246700070622     c                   clear                   sav_pos3
246800070622     c                   clear                   sav_pos4
246900070622     c                   clear                   sav_pos5
247000070622     c                   clear                   sav_pos6
247100131119     c                   clear                   sav_wmax2
247200070720
247300070622      * tipo funzione
247400070622     c                   select
247500070622
247600070622     c                   when      v1sce = '1'
247700070622     c                   eval      vc2dsce = 'ASSEGNA'
247800070622     c                   eval      vc2dess = 'alla Distinta nr.' +
247900070621     c                             %trim(%editc(h1ndc_piu:'Z'))
248000070622
248100070705     c                   when      v1sce = '2'
248200070622     c                   eval      vc2dsce = 'TOGLIE'
248300070622     c                   eval      vc2dess = 'dalla Distinta nr.' +
248400070621     c                             %trim(%editc(h1ndc_meno:'Z'))
248500070622
248600070622     c                   when      v1sce = '3'
248700070622     c                   eval      vc2dsce = 'TRASFERISCE'
248800070622     c                   eval      vc2dess = 'dalla Distinta nr.' +
248900070621     c                             %trim(%editc(h1ndc_meno:'Z'))
249000070622
249100070622     c                   endsl
249200070622
249300070622      * carica il sfl
249400070704     c                   exsr      rols2
249500070622
249600070622     c                   eval      wpag = 1
249700070622
249800070622     c                   endsr
249900070622
250000070622      *------------------------------------------------------------------------*
250100070705      *   Caricamento pagina da sfl dei giri                         ?
250200070622      *------------------------------------------------------------------------*
250300070704     c     rols2         begsr
250400070622
250500070622     c                   eval      *in22 = *off
250600070622     c                   clear                   yy
250700070622     c                   eval      s2nrr = wmax
250800070622
250900070720      * se scelto caricamento x assegnazione
251000070622     c                   if        v1sce = '1'
251100070720      * carica da sfl1 se non richiesto un ORM singolo
251200070719     c                   if        v1cnor = *zeros
251300070704     c                   exsr      car_da_sfl1
251400070719     c                   endif
251500070720      * carica da ORM se richiesto un ORM singolo
251600070719     c                   if        v1cnor <> *zeros
251700070719     c                   exsr      car_da_orm
251800070719     c                   endif
251900070621     c                   else
252000070720      * carica da numero distinta
252100070704     c                   exsr      car_da_ndc
252200070622     c                   endif
252300070622      * fine caricamento
252400070622     c                   eval      wmax = s2nrr
252500070622
252600070720      * esegue l'ordinamento x giro/ORM
252700070704     c                   exsr      clear_ordina
252800070704     c                   exsr      ordina_sfl2
252900070622
253000070622      * posizionamento al 1° rcd della pagina
253100070622     c                   eval      pagine = %div(s2nrr:s2pag)
253200070622     c                   eval      resto = %rem(s2nrr:s2pag)
253300070622     c                   eval      vc2rcd = pagine * s2pag
253400070622     c                   if        resto > *zeros
253500070622     c                   eval      vc2rcd = vc2rcd + 1
253600070622     c                   else
253700070622     c                   eval      vc2rcd = vc2rcd - s2pag
253800070622     c                   eval      vc2rcd = vc2rcd + 1
253900070622     c                   endif
254000131119     c                   eval      sav_wmax2 = wmax
254100070622
254200070622     c                   endsr
254300070622
254400070622      *------------------------------------------------------------------------*
254500070705      *   Carica dal sfl dei giri                                  ?
254600070622      *------------------------------------------------------------------------*
254700070704     c     car_da_sfl1   begsr
254800070622
254900070622      * legge il sfl dei giri dove selezionato con (1) le righe
255000070720      * dei giri
255100070622     c                   readc     or60S01                                30
255200070622     c                   dow       not *in30
255300070622
255400070622      * solo dai giri selezionati
255500070622     c                   if        vs1sce = '1'
255600070718     c                   if        vs1cgi <> *blanks
255700070718     c                   eval      con_giro = 'S'
255800070718     c                   eval      wcgi = vs1cgi
255900070718     c                   else
256000070718     c                   eval      con_giro = 'N'
256100070808     c                   clear                   wcgi
256200070718     c                   endif
256300070718      * preparo la stringa sql per estrarre i dati che servono
256400070718     c                   exsr      sr_sqlorg
256500070718      * apro il cursore comune per lettura da sfl e da distinta
256600070718     c                   exsr      sr_sqlopen
256700070718
256800070720      * leggendo gli ORM
256900070704     c                   exsr      legge_orm
257000070622
257100070622      * leggo dal file per caricare la lista
257200070622     c                   dow       $efile = *off
257300070622
257400070720      * imposto i campi del subfile
257500070720     c                   clear                   or60s02
257600070704     c                   exsr      riga_sfl2
257700070622
257800070622      * se si è scelto di non visualizzare gli ORM comunque li carico
257900070720      * tutti selezionati con sflnxtchg x la routine di aggiornamento
258000070720      * finale
258100070622     c                   if        $viss2 = *off
258200070921     c                             or vs2cgi = fdgcgi and
258300070704     c                             (fdgopz='3' or fdgopz='2')
258400070622     c                   eval      vs2sce = '1'
258500070622     c                   eval      *in22 = *on
258600070622     c                   endif
258700080723      * imposto se filiale ha PDA
258800080723     c                   eval      *in17 = (pdaorm <> *blanks)
258900070921
259000070622     c                   eval      s2nrr = s2nrr + 1
259100070622     c                   eval      rrnlast = s2nrr
259200070622     c                   eval      yy = yy + 1
259300070622      *                  __________________
259400070622     c                   write     or60s02
259500070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
259600070622      * lettura di un record
259700070704     c                   exsr      legge_orm
259800070622     c                   enddo
259900070622
260000070622      * attivo sempre il sflnxtchg e aggiorno sfl dei giri
260100070622     c                   eval      *in22 = *on
260200070622      *                  __________________
260300070622     c                   update    or60s01
260400070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
260500070622     c                   endif
260600080709      * chiudo il cursore
260700080709     c                   exsr      sr_sqlclose
260800070622
260900070622     c                   readc     or60s01                                30
261000070622     c                   enddo
261100070622
261200070622     c                   endsr
261300070719
261400070719      *------------------------------------------------------------------------*
261500070719      *   Carica da numero ORM                                     ?
261600070719      *------------------------------------------------------------------------*
261700070719     c     car_da_orm    begsr
261800070719
261900070720      * preparo la stringa sql per estrarre i dati che servono
262000070719     c                   exsr      sr_sqlorg
262100070719      * apro il cursore comune per lettura da sfl e da distinta
262200070719     c                   exsr      sr_sqlopen
262300070719
262400070720      * leggo gli ORM
262500070719     c                   exsr      legge_orm
262600070719
262700070719      * leggo dal file per caricare la lista
262800070719     c                   dow       $efile = *off
262900070719
263000070720      * imposto i campi del subfile
263100070720     c                   clear                   or60s02
263200070719     c                   exsr      riga_sfl2
263300070719
263400070720      * carico tutti gli ORM selezionati con sflnxtchg
263500070720      * x la routine di aggiornamento finale
263600070921     c                   if        vs2cgi = fdgcgi and
263700070719     c                             (fdgopz='3' or fdgopz='2')
263800070719     c                   eval      vs2sce = '1'
263900070719     c                   eval      *in22 = *on
264000070719     c                   endif
264100080723      * imposto se filiale ha PDA
264200080723     c                   eval      *in17 = (pdaorm <> *blanks)
264300070719
264400070719     c                   eval      s2nrr = s2nrr + 1
264500070719     c                   eval      rrnlast = s2nrr
264600070719     c                   eval      yy = yy +1
264700070719      *                  __________________
264800070719     c                   write     or60s02
264900070719      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
265000070719      * lettura di un record
265100070719     c                   exsr      legge_orm
265200070719     c                   enddo
265300070719
265400070720      * chiudo il cursore
265500070719     c                   exsr      sr_sqlclose
265600070719
265700070719     c                   endsr
265800070622
265900070622      *------------------------------------------------------------------------*
266000070705      *   Carica da numero distinta                                ?
266100070622      *------------------------------------------------------------------------*
266200070704     c     car_da_ndc    begsr
266300070622
266400070622      * se toglie o trasferisce:  caricamento da distinta
266500070622     c                   if        v1sce = '2' or v1sce = '3'
266600070718      * preparo la stringa sql per estrarre i dati che servono
266700070718     c                   exsr      sr_sqlorg
266800070718      * apro il cursore comune per lettura da sfl e da distinta
266900070718     c                   exsr      sr_sqlopen
267000070718     c                   endif
267100070718
267200070720      * leggo gli ORM
267300070704     c                   exsr      legge_orm
267400070622
267500070622      * leggo dal file per caricare la lista
267600070622     c                   dow       $efile = *off
267700070622
267800070720      * imposto i campi del subfile
267900070622     c                   clear                   or60s02
268000070704     c                   exsr      riga_sfl2
268100070622
268200080723      * imposto se filiale ha PDA
268300080723     c                   eval      *in17 = (pdaorm <> *blanks)
268400070622
268500070622     c                   eval      s2nrr = s2nrr + 1
268600070622     c                   eval      rrnlast = s2nrr
268700070622     c                   eval      yy = yy +1
268800070622      *                  __________________
268900070622     c                   write     or60s02
269000070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
269100070622      * lettura di un record
269200070704     c                   exsr      legge_orm
269300070622     c                   enddo
269400070718
269500070720      * chiudo il cursore
269600070718     c                   exsr      sr_sqlclose
269700070622
269800070622     c                   endsr
269900070622
270000070622      *------------------------------------------------------------------------*
270100070705      *   Lettura rcd archivio pilota                                ?
270200070622      *------------------------------------------------------------------------*
270300070704     c     legge_orm     begsr
270400070622
270500070622      * esce per fine file
270600070622     c                   eval      $efile = *off
270700070622      * per rcd ok deve uscire riportando i dati
270800070622     c                   eval      $rcdok = *off
270900070622
271000070622     c                   dou       $efile = *on or $rcdok = *on
271100070622
271200070718     C/EXEC SQL
271300071025     C+ FETCH NEXT FROM A2 INTO  :fnorgds, :fnormds
271400070718     C/END-EXEC
271500070622
271600070718     c                   select
271700070622      * fine file
271800070718     c                   when      sqlcod = 100
271900070718     c                   eval      $efile = *on
272000070718     c                   eval      *in23 = $efile
272100070718      * errori
272200070718     c                   when      sqlcod < 0
272300070718     c                   if        sqlcod <> -913
272400070718     c                   eval      *inlr = *on
272500070718     c                   return
272600070718     c                   endif
272700070718
272800070718     c                   other
272900070720      *  per Assegnare gli ORM deve controllare se l'ORM può essere assegnato
273000070718     c                   if        v1sce = '1'
273100070718     c                   exsr      check_orm
273200070718     c                   if        or64asso <> 'N'
273300070718     c                   eval      $rcdok = *on
273400070718     c                   endif
273500070718     c                   endif
273600070622
273700070621      * Per Togliere o Trasfererire
273800070622     c                   if        v1sce <> '1'
273900070622     c                   eval      $rcdok = *on
274000070622     c                   endif
274100070622
274200070622     c                   endsl
274300070622
274400070622     c                   enddo
274500070622
274600070622     c                   endsr
274700070622
274800070622      *------------------------------------------------------------------------*
274900070705      *   Imposta campi del 2°sfl                                    ?
275000070622      *------------------------------------------------------------------------*
275100070704     c     riga_sfl2     begsr
275200070720
275300070720      * codice giro
275400070720     c                   eval      vs2cgi = orgcgi
275500070720      * chiave ORM
275600070622     c                   eval      vs2poe = ormpoe
275700070622     c                   eval      vs2nsr = ormnsr
275800070622     c                   eval      vs2nor = ormnor
275900070622     c                   eval      vs2nrv = ormnrv
276000070719      * mittente
276100140211     c                   eval      vs2rag = ormrsr
276200070704     c                   eval      vs2rsr = ormrsr
276300070622     c                   eval      vs2inr = orminr
276400070622     c                   eval      vs2lor = ormlor
276500140211      *
276600140211     c                   eval      vs2dao = ormdao
276700070719      * peso e volume da org
276800071109      * devo togliere i decimali dal peso
276900071109     c                   if        orgpkg > *zeros
277000080215     c                   if        orgpkg > 99999
277100080215     c                   eval      vs2pkg = 99999
277200080215     c                   else
277300071109     c                   eval      vs2pkg = %dech(orgpkg:5:0)
277400080215     c                   endif
277500071109     c                   if        vs2pkg = *zeros
277600071109     c                   eval      vs2pkg = 1
277700071109     c                   endif
277800071109     c                   else
277900071109     c                   clear                   vs2pkg
278000071109     c                   endif
278100070718     c                   eval      vs2vlm = orgvlm
278200070719      * priorità
278300070719     c                   z-add     ormsto        vs2sto
278400070719      * colli/bancali
278500070622     c                   eval      vs2ncl = ormncl
278600070704     c                   eval      vs2bnc = ormbnc
278700070719      * data
278800070622     c                   clear                   wlbdat
278900070719     c                   z-add     ormdar        g08inv
279000070719     c                   eval      g08err = '3'
279100070622     c                   call      'XSRDA8'
279200070622     c                   parm                    wlbdat
279300070719     c                   eval      vs2dar = g08ggmm
279400131119
279500131119      /free
279600131119       //?Cerco l'ora di CUT OFF più basse per il giro che sto
279700131119       //?caricando
279800131126         //exsr cercaCUTOFF;
279900140319         //IF  OOR2lrnc < wora and OOR2lrnc > 0;
280000140319         //  vs2ora = OOR2lrnc;
280100131126         //ENDIF;
280200131119      /end-free
280300131119
280400070719      * ora
280500070719     c                   eval      vs2orr = ormorr
280600131126     c                   eval      vs2ora = ormorr
280700070719
280800070719     c                   eval      dorm01 = ormflo
280900070719      * commissionato
281000070921     c                   if        §orcom = 'S'
281100070719     c                   eval      vs2com = §orcom
281200070921     c                   else
281300070921     c                   clear                   vs2com
281400070921     c                   endif
281500070719      * sponda idraulica
281600070921     c                   if        ormspi = 'S'
281700070921     c                   eval      vs2spi = 'Sp'
281800070921     c                   else
281900070921     c                   clear                   vs2spi
282000070921     c                   endif
282100070719      * automezzo
282200070719     c                   select
282300070719     c                   when      ormblc <> *zeros
282400070719     c                   eval      vs2aut = 'BIL'
282500070719     c                   when      ormatt <> *zeros
282600070719     c                   eval      vs2aut = 'AUT'
282700070719     c                   when      ormmtc <> *zeros
282800070719     c                   eval      vs2aut = 'MOT'
282900070719     c                   other
283000070719     c                   clear                   vs2aut
283100070719     c                   endsl
283200071008
283300071008     c                   clear                   vs2all
283400070719      * prepagato
283500070719     c                   if        ormtor = 'P'
283600071008     c                   eval      vs2all = 'Pp'
283700070719     c                   endif
283800071022      * ricevuta di ritiro
283900071008     c                   if        §orsrm = 'S'
284000071009     c                   eval      vs2all = 'RiO'
284100071008     c                   endif
284200070920
284300070920      * controllo se ORM Comunicato
284400070920     c                   if        ormdst <> *zeros
284500070920     c                   eval      vs2comun = *on
284600070920     c                   else
284700070920     c                   eval      vs2comun = *off
284800070920     c                   endif
284900080314
285000080314      * controllo se ORM inviato a PDA
285100080314     c                   eval      dorg01 = orgflo
285200080314     c                   if        §orgpda = 'S'
285300080314     c                   eval      vs2pda = *on
285400080314     c                   else
285500080314     c                   eval      vs2pda = *off
285600080314     c                   endif
285700110523      /free
285800110523       //?Se ORM RC il controllo lo faccio andando su FIPDO
285900110523         IF  ORMnsr > 0;
286000110523           chain (vs2poe:vs2nsr:vs2nor:vs2nrv) FIPDO02T;
286100110523           IF  %found(FIPDO02T);
286200110523             VS2pda = *on;
286300110523           ENDIF;
286400110523         ENDIF;
286500110523      /end-free
286600080314
286700080314      * mi memorizzo la fase dell'ORM
286800080314     c                   eval      vs2far = ormfao
286900080512      * pulisco il campo di forzatura messagggio
287000080514     c                   eval      vs2forza = *off
287100071022
287200071022      * protezione scelta del subfile
287300071022     c                   eval      vs2prot = *off
287400071022      * proteggo sempre se ORM RC
287500071022      * e imposto il campo degli allegati
287600071022     c                   if        ormnsr > *zeros
287700071022     c                   eval      vs2all = 'RC'
287800071022     c                   eval      vs2prot = *on
287900071022     c                   leavesr
288000071022     c                   endif
288100071022      * in base a quanto richiesto in prima videata per gli allegati
288200071022      * se è Toglie o Trasferisce
288300071022     c                   if        v1sce = '2' or v1sce = '3'
288400071022     c                   if        v1all = 'I' and ormtor <> 'P' and
288500071022     c                             §orsrm <> 'S'
288600071022     c                   eval      vs2prot = *on
288700071022     c                   endif
288800071022     c                   if        v1all = 'E' and (ormtor = 'P' or
288900071022     c                             §orsrm = 'S')
289000071022     c                   eval      vs2prot = *on
289100071022     c                   endif
289200071022     c                   endif
289300071109
289400071109      * cerco le proposte da analizzare e quelle eseguite
289500071109     c                   clear                   vs2prop
289600071109     c     kaggiorna     setgt     fnorp01l
289700071112     c                   do        *hival
289800071109     c     kaggiorna     readpe    fnorp01l
289900071109     c                   select
290000071109     c                   when      %eof(fnorp01l)
290100071112     c                   leave
290200071109     c                   when      orpfev = *blanks
290300071109     c                   eval      vs2prop = 'P'
290400071112     c                   leave
290500071112     c                   when      orpfev = 'R'
290600071112     c                   iter
290700071109     c                   when      orpfev = 'E'
290800071109     c                   if        ormndc > *zeros and ormndc = orpndc
290900071109     c                   eval      vs2prop = 'V'
291000071109     c                   endif
291100071112     c                   leave
291200071109     c                   endsl
291300071112     c                   enddo
291400080611
291500080611      * se attivo/test il PDA sulla filiale cerco le INFO da PDA
291600080611     c                   clear                   vs2nrrpro
291700080611     c                   if        pdaorm <> *blanks
291800080611     c                   exsr      sr_infopda
291900080611     c                   endif
292000070622
292100070622     c                   endsr
292200080611
292300080611      *------------------------------------------------------------------------*
292400080611      *   Controlla se presente delle INFO x PDA da visualizzare   ?
292500080611      *------------------------------------------------------------------------*
292600080611     c     sr_infopda    begsr
292700080611
292800140211      /FREE
292900140211           clear fior82ds;
293000140211           clear  vs2info ;
293100140211           clear  vh2notinfo ;
293200140211           c82fgs = wfgs;
293300140211           c82ndc = h1ndc_meno ;
293400140211           c82codaut = h1pdr_meno;
293500140211           c82POE = vs2poe;
293600140211           c82NSR = vs2nsr;
293700140211           c82NOR = vs2nor;
293800140211           c82NRV = vs2nrv;
293900140211           kpjbu = fior82ds;
294000140211           fior82r(kpjba);
294100140211           fior82ds = kpjbu;
294200140211           if c82davisio <> '';
294300140211             vs2info = C82DAVISIO;
294400140211             vh2notinfo = C82NOTASS;
294500140211           endif;
294600140211      /END-FREE
294700080611
294800080611     c                   endsr
294900070622
295000070622      *------------------------------------------------------------------------*
295100070720      *   Controlla testata lista ORM                                ?
295200070622      *------------------------------------------------------------------------*
295300070704     c     ctrc2         begsr
295400070622
295500070622     c                   eval      *in28 = *off
295600070622
295700070622      * se si tratta di trasferimento
295800070720      * non deve essere la stessa distinta da cui trasferisce
295900070622     c                   if        v1sce = '3' and
296000070622     c                             h1ndc_piu = h1ndc_meno
296100070622     c                   eval      *in28 = *on
296200070720     c                   eval      *in41 = *on
296300070622     c                   endif
296400070622
296500070720      * controllo campi di posizionamento
296600070720      * o di ricerca stringa (?) in primo Byte
296700070621     c                   eval      posizionamento = 'N'
296800070621     c                   eval      posizionato    = 'N'
296900070621     c                   eval      cerca_stringa  = 'N'
297000070622     c                   clear                   stringa_20a
297100070622
297200070621     c                   select
297300070621      * giro
297400070622     c                   when      *in01 and sav_pos1 <> vc2pos1
297500070622     c                   eval      sav_pos1 = vc2pos1
297600070621     c                   eval      posizionamento = 'S'
297700070622      * mittente
297800070622     c                   when      *in02 and sav_pos2 <> vc2pos2
297900070622     c                   eval      sav_pos2 = vc2pos2
298000070720      * è una ricerca x stringa
298100070621     c                   eval      cerca_stringa  = 'S'
298200070622     c                   eval      stringa_20a = %subst(vc2pos2:1)
298300080128      * ora
298400070622     c                   when      *in03 and sav_pos3 <> vc2pos3
298500070622     c                   eval      sav_pos3 = vc2pos3
298600080128     c                   eval      posizionamento = 'S'
298700070622      * località
298800070622     c                   when      *in04 and sav_pos4 <> vc2pos4
298900070622     c                   eval      sav_pos4 = vc2pos4
299000070720      * è una ricerca x stringa
299100070621     c                   eval      cerca_stringa  = 'S'
299200070622     c                   eval      stringa_20a = %subst(vc2pos4:1)
299300070622      * volume
299400070622     c                   when      *in05 and sav_pos5 <> vc2pos5
299500070622     c                   eval      sav_pos5 = vc2pos5
299600070621     c                   eval      posizionamento = 'S'
299700070622      * peso
299800070622     c                   when      *in06 and sav_pos6 <> vc2pos6
299900070622     c                   eval      sav_pos6 = vc2pos6
300000070621     c                   eval      posizionamento = 'S'
300100070622
300200070622      * se precedentemente si era posizionato
300300070622     c                   when      vc2pos1 <> *blank  or
300400070622     c                             vc2pos2 <> *blank  or
300500080128     c                             vc2pos3 <> *zeros  or
300600070622     c                             vc2pos4 <> *blank  or
300700070622     c                             vc2pos5 <> *zero   or
300800070622     c                             vc2pos6 <> *zero
300900070621     c                   eval      posizionato = 'S'
301000070622
301100070622     c                   endsl
301200070622
301300070622      * se si deve posizionare sul sfl on *top
301400070621     c                   if        posizionamento = 'S'
301500070704     c                   exsr      da_posiz
301600070622     c                   endif
301700070622
301800070622      * se si deve cercare una stringa con (*)
301900070621     c                   if        cerca_stringa  = 'S'
302000070704     c                   exsr      cerca_strin
302100070622     c                   endif
302200070622
302300070622     c                   endsr
302400070622
302500070622      *------------------------------------------------------------------------*
302600070705      *   Imposta il posizionamento al record top del sfl2           ?
302700070622      *------------------------------------------------------------------------*
302800070704     c     da_posiz      begsr
302900070622
303000070622      * se si deve posizionare sul sfl on *top
303100070622     c                   eval      *in28 = *on
303200070622     c                   eval      vc2rcd =  1
303300070622
303400070622      * primo ciclo per togliere le priorità a tutti records
303500070622      * del sfl
303600070622     c                   do        rrnlast       s2nrr
303700070622     c     s2nrr         chain     or60s02
303800080611     c                   if        vs2sce = '1' or vs2sce = '5' or
303900140211     c                             vs2sce = '8' or vs2sce = '7' or
304000140211     c                             vs2sce = '9'
304100070622     c                   eval      *in22 = *on
304200070621     c                   else
304300070622     c                   eval      *in22 = *off
304400070622     c                   endif
304500070622     c                   clear                   h2subst
304600080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
304700080314     c                   if        vs2comun = *on or vs2pda = *on
304800080314     c                   eval      *in08 = *on
304900080314     c                   else
305000080314     c                   eval      *in08 = *off
305100080314     c                   endif
305200071022      * protezione o meno campo scelta
305300071022     c                   eval      *in09 = vs2prot
305400080723      * imposto se filiale ha PDA
305500080723     c                   eval      *in17 = (pdaorm <> *blanks)
305600070622      *                  __________________
305700070622     c                   update    or60s02
305800070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
305900070622     c                   enddo
306000070622
306100070720      * ordina il sfl
306200070704     c                   exsr      ordina_sfl2
306300070622
306400070622      * secondo ciclo x trovare il posizionamento dal record....
306500070622     c                   do        rrnlast       s2nrr
306600070704     c     s2nrr         chain     or60s02
306700070921     c                   if        (*in01 and vc2pos1 > vs2cgi)    or
306800070704     c                             (*in02 and vc2pos2 > vs2rsr)    or
306900131119     c                             (*in03 and vc2pos3 < vs2ora)    or
307000070622     c                             (*in04 and vc2pos4 > vs2lor)    or
307100070622     c                             (*in05 and vc2pos5 < vs2vlm)    or
307200070622     c                             (*in06 and vc2pos6 < vs2pkg)
307300070622
307400070622     c                   eval      vc2rcd = s2nrr + 1
307500070622     c                   if        vc2rcd > rrnlast
307600070622     c                   eval      vc2rcd = rrnlast
307700070622     c                   endif
307800070622
307900070622      * si tiene in memoria la riga toccata con sflnxtchg
308000080611     c                   if        vs2sce = '1' or vs2sce = '5' or
308100140211     c                             vs2sce = '8' or vs2sce = '7' or
308200140211     c                             vs2sce = '9'
308300070622     c                   eval      *in22 = *on
308400070621     c                   else
308500070622     c                   eval      *in22 = *off
308600070622     c                   endif
308700080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
308800080314     c                   if        vs2comun = *on or vs2pda = *on
308900080314     c                   eval      *in08 = *on
309000080314     c                   else
309100080314     c                   eval      *in08 = *off
309200080314     c                   endif
309300071022      * protezione o meno campo scelta
309400071022     c                   eval      *in09 = vs2prot
309500080723      * imposto se filiale ha PDA
309600080723     c                   eval      *in17 = (pdaorm <> *blanks)
309700070622      *                  __________________
309800070622     c                   update    or60s02
309900070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
310000070621     c                   else
310100070621     c                   iter
310200070622     c                   endif
310300070622     c                   enddo
310400070622
310500070622     c                   endsr
310600070622
310700070622      *------------------------------------------------------------------------*
310800070705      *   Se richiesta una ricerca di stringa fra i record del SFL   ?
310900070622      *------------------------------------------------------------------------*
311000070704     c     cerca_strin   begsr
311100070622
311200070622      * deve cercare la stringa e visualizzare prioritariamente i records
311300070622      * contenenti la stringa
311400070622     c                   eval      *in28 = *on
311500070622     c                   eval      vc2rcd =  1
311600070622     c                   eval      wlen = %len(%trim(stringa_20a))
311700070622
311800070622     c                   do        rrnlast       s2nrr
311900070704     c     s2nrr         chain     or60s02
312000070622     c                   clear                   wpos
312100070622     c                   clear                   h2subst
312200070622
312300070705     c   02              eval      wpos = %scan(%subst(stringa_20a:1:wLen) :
312400070704     c                              vs2rsr)
312500070622
312600070705     c   04              eval      wpos = %scan(%subst(stringa_20a:1:wLen) :
312700070622     c                              vs2lor)
312800070621
312900070622      * se ha trovato la stringa cercata
313000070622     c                   if        wpos > 0
313100080612     c                   eval      h2subst = 1
313200070622     c                   endif
313300070622
313400070622      * si tiene in memoria la riga toccata con sflnxtchg
313500070622     c                   if        vs2sce = '1' or vs2sce = '5'
313600131119     c                             or vs2sce = '7'
313700070622     c                   eval      *in22 = *on
313800070621     c                   else
313900070622     c                   eval      *in22 = *off
314000070622     c                   endif
314100080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
314200080314     c                   if        vs2comun = *on or vs2pda = *on
314300080314     c                   eval      *in08 = *on
314400080314     c                   else
314500080314     c                   eval      *in08 = *off
314600080314     c                   endif
314700071022      * protezione o meno campo scelta
314800071022     c                   eval      *in09 = vs2prot
314900080723      * imposto se filiale ha PDA
315000080723     c                   eval      *in17 = (pdaorm <> *blanks)
315100070622      *                  __________________
315200070622     c                   update    or60s02
315300070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
315400070622     c                   enddo
315500070622
315600070622      * quindi emette il sfl riordinato
315700070704     c                   exsr      ordina_sfl2
315800070622
315900070622     c                   endsr
316000070622
316100070622      *------------------------------------------------------------------------*
316200070705      *   Controllo opzioni lista                                    ?
316300070622      *------------------------------------------------------------------------*
316400070704     c     ctrs2         begsr
316500070622
316600070720      * pulizia dei campi di totale + campi di comodo e indicatori
316700070622     c                   clear                   totnor_s2
316800070622     c                   clear                   totpkg_s2
316900070622     c                   clear                   totvlm_s2
317000071017     c                   clear                   wvlm
317100070622     c                   clear                   giri_scelti
317200070704     c                   clear                   sav_s2cgi
317300070622     c                   clear                   primogiro_blk
317400080611     c                   eval      wokinfo = *off
317500070622     c                   eval      $esci = *off
317600070622     c                   eval      *in28 = *off
317700080612
317800080612     c                   eval      *in25 = *off
317900080612     c                   eval      *in26 = *off
318000080612     c                   eval      *in27 = *off
318100070622
318200070622      * leggo il sfl solo se ci sono rcd
318300070622     c                   if        wmax > *zeros
318400131127     c                   readc     or60s02                                30
318500070622
318600070622      * esce se fine sfl o errore che richiede l'uscita
318700131127     c                   dow       not *in30 and $esci = *off
318800070622     c                   eval      vc2rcd = s2nrr
318900070919     c                   eval      *in39 = *off
319000070622      * ctrl su riga
319100070704     c                   exsr      recs2
319200070621      *
319300070622      * se rilevato errore o richiesta uscita, attivo sflnxtchg
319400070622     c                   if        *in28 or $esci = *on
319500070720      * disabilito l'eventuale richiesta di reinizializzazione sfl
319600070622      * la ripristinerò a conclusione del ciclo di readc
319700131119     c                   IF        $esci = *off
319800070622     c                   eval      $inzs2 = *off
319900131119     c                   ELSE
320000131119     c                   eval      $inzs2 = *on
320100131119     c                   ENDIF
320200070622     c                   endif
320300070622
320400070622      * attivo sempre il sflnxtchg e aggiorno sfl
320500080613     c                   if        vs2sce <> *blanks
320600070622     c                   eval      *in22 = *on
320700070621     c                   else
320800070622     c                   eval      *in22 = *off
320900070622     c                   endif
321000080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
321100080314     c                   if        vs2comun = *on or vs2pda = *on
321200080314     c                   eval      *in08 = *on
321300080314     c                   else
321400080314     c                   eval      *in08 = *off
321500080314     c                   endif
321600071022      * protezione o meno campo scelta
321700071022     c                   eval      *in09 = vs2prot
321800080723      * imposto se filiale ha PDA
321900080723     c                   eval      *in17 = (pdaorm <> *blanks)
322000070622      *                  __________________
322100070622     c                   update    or60s02
322200070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
322300070622      * leggo prossimo rcd dal sfl se no richiesta uscita ciclo
322400070622     c                   if        $esci = *off
322500070622     c                   readc     or60s02                                30
322600070622
322700070720      * a fine ciclo ripristino il flag richiesta inizializzazione sfl
322800070622     c                   if        *in30
322900070622      * calcolo pagina a cui deve posizionarsi
323000070719     c                   eval      wpagine = s2pag
323100070704     c                   exsr      clcpag
323200070622     c                   endif
323300070622     c                   endif
323400070622
323500070622     c                   enddo
323600070622
323700070622     c                   endif
323800070622
323900070622     c                   endsr
324000070622
324100070622      *------------------------------------------------------------------------*
324200070705      *   Controllo campi i/o riga lista                             ?
324300070622      *------------------------------------------------------------------------*
324400070704     c     recs2         begsr
324500070622
324600070622      * gestione opzioni
324700070622     c                   if        vs2sce <> *blanks and not *in28
324800080612     c                   if        %lookup(vs2sce :opz) = *zeros
324900080612     c                   eval      *in27 = *on
325000080612     c                   eval      *in28 = *on
325100131127     c                   leavesr
325200080612     c                   endif
325300070720
325400070720      * interrogazione orm
325500070720     c                   if        vs2sce = '5'
325600070803     c                   clear                   dsfior07
325700070720     c                   eval      dsopoe = vs2poe
325800070720     c                   eval      dsonor = vs2nor
325900070720     c                   eval      dsonsr = vs2nsr
326000070720     c                   eval      dsonrv = vs2nrv
326100070720     c                   eval      dsoopz = vs2sce
326200070720     c                   eval      dsofgs = v1cfgs
326300070803     c                   eval      kpjbu = dsfior07
326400070720     c                   call      'FIOR07R'
326500070720     c                   parm                    kpjba
326600071012     c                   clear                   vs2sce
326700131127     c                   eval      wnowin = *on
326800070720     c                   endif
326900131119
327000131119      * Assegna giro
327100131119     c                   if        vs2sce = '7'
327200131119     c                   clear                   fior73ds
327300131119     c                   eval      IOR73poe = vs2poe
327400131119     c                   eval      IOR73nor = vs2nor
327500131119     c                   eval      IOR73nsr = vs2nsr
327600131119     c                   eval      IOR73nrv = vs2nrv
327700131119     c                   clear                   kpjbu
327800131119     c                   call      'FIOR73R'
327900131119     c                   parm                    kpjba
328000131119     c                   parm                    fior73ds
328100131119     c                   clear                   vs2sce
328200131127     c                   eval      wnowin = *on
328300131119     c                   IF        OOR73agg = *on
328400131127     c                   eval      $inzs2 = *on
328500131119     c                   eval      waggiorna = *on
328600131119     c                   ENDIF
328700131119     c                   endif
328800070919
328900070919      * se Assegna o Trasferisce
329000070919      * imposto se anche solo 1 orm selezionato ha la sponda idraulica
329100070919     c                   if        (v1sce = '1' or v1sce = '3')
329200070925     c                              and vs2sce = '1' and vs2spi = 'Sp'
329300070919     c                   eval      $ormspi = *on
329400131127     c                   eval      wnowin = *off
329500070919     c                   endif
329600070921
329700070921      * se Toglie o Trasferisce
329800080314     c                   if        (v1sce = '2' or v1sce = '3')
329900080526     c                             and vs2sce = '1'
330000080526     c                   eval      $ormesitono = *off
330100080526     c                   eval      $ormesitook = *off
330200080526     c                   eval      $ormpda = *off
330300080526     c                   eval      $ormcomun = *off
330400131127     c                   eval      wnowin = *off
330500080317     c                   select
330600080526     c                   when      vs2forza = *on
330700080317      * imposto se ORM in fase 410 o 420
330800080526     c                   when      vs2far = 410
330900080314     c                   eval      $ormesitono = *on
331000080526     c                   eval      *in25 = *on
331100080526     c                   eval      *in28 = *on
331200080526     c                   eval      vs2forza = *on
331300080317     c                   when      vs2far = 420
331400080314     c                   eval      $ormesitook = *on
331500080526     c                   eval      *in25 = *on
331600080526     c                   eval      *in28 = *on
331700080526     c                   eval      vs2forza = *on
331800080317      * Stessa cosa se ORM scaricato a PDA
331900080317     c                   when      vs2pda = *on
332000080317     c                   eval      $ormpda = *on
332100080526     c                   eval      *in26 = *on
332200080526     c                   eval      *in28 = *on
332300080526     c                   eval      vs2forza = *on
332400080317      * imposto se anche solo 1 orm selezionato è comunicato
332500080317     c                   when      vs2comun = *on
332600080317     c                   eval      $ormcomun = *on
332700080526     c                   eval      *in26 = *on
332800080526     c                   eval      *in28 = *on
332900080526     c                   eval      vs2forza = *on
333000080317     c                   endsl
333100080508
333200080314     c                   endif
333300070720
333400070720      * totalizza gli ORM/pesi/volumi/colli
333500070720     c                   if        vs2sce = '1'
333600070720     c                   eval      totnor_s2 = totnor_s2 + 1
333700070720     c                   eval      totpkg_s2 = totpkg_s2 + vs2pkg
333800071017     c                   if        wvlm < 99,990
333900071017     c                   eval      wvlm = wvlm + vs2vlm
334000071017     c                   endif
334100071017     c                   if        wvlm >= 99,990
334200071017     c                   eval      totvlm_s2 = 99,999
334300071017     c                   else
334400071017     c                   eval      totvlm_s2 = wvlm
334500071017     c                   endif
334600070720
334700131127     c                   eval      wnowin = *off
334800070720     c                   eval      sav_sxcgi = vs2cgi
334900070720
335000070720      * conta quanti giri sono stati scelti nel sfl
335100070720      * per permettere di interrogare con l'F4 con stesso giro
335200070720     c                   if        vs2cgi <> sav_s2cgi  or
335300070720     c                             primogiro_blk = *blank and vs2cgi = *blank
335400070720
335500070720      * e se il giro è blank non deve ripetere un eventuale conto di altri con giro blank
335600070720     c                   eval      primogiro_blk = 'S'
335700070720
335800070720     c                   eval      giri_scelti = giri_scelti + 1
335900070720     c                   eval      sav_s2cgi = vs2cgi
336000070720     c                   eval      vc2giro = vs2cgi
336100070720     c                   endif
336200070720     c                   endif
336300080611
336400080611      * chiude INFO se presente una INFO
336500080612     c                   if        vs2sce = '8' and
336600080714      * se procedura PDA ORM attiva o in test
336700080714     c                             pdaorm <> *blanks
336800131127     c                   eval      wnowin = *on
336900080613      * se non c'è la info a video errore
337000080613     c                   if        vs2info = *blanks
337100080613     c                   eval      *in27 = *on
337200080613     c                   eval      *in28 = *on
337300080613     c                   leavesr
337400080613     c                   endif
337500080611      * aggiorna il PRO togliendo la INFO
337600080611     c                   exsr      sr_chiuinfo
337700080613     c                   clear                   vs2sce
337800080611     c                   clear                   vs2info
337900080611     c                   eval      wokinfo = *on
338000080611     c                   endif
338100070720
338200070622     c                   endif
338300070622
338400140211      * chiude INFO se presente una INFO
338500140211     c                   if        vs2sce = '9' and
338600140211      * se procedura PDA ORM attiva o in test
338700140211     c                             pdaorm <> *blanks
338800140211      * se non c'è la info a video errore
338900140211     c                   if        vs2info = *blanks
339000140211     c                   eval      *in27 = *on
339100140211     c                   eval      *in28 = *on
339200140211     c                   leavesr
339300140211     c                   endif
339400140211      * aggiorna il PRO togliendo la INFO
339500140211     c                   exsr      sr_visinfo
339600140211     c                   clear                   vs2sce
339700140212     c                   eval      wnowin = *on
339800140211     c                   endif
339900140211
340000070622     c                   endsr
340100070718
340200070718      *------------------------------------------------------------------------*
340300070719      *   Gestione ultima finestra di riepilogo                  ?
340400070718      *------------------------------------------------------------------------*
340500070718     c     gesw1         begsr
340600070719
340700070719      * inizializzazione videata
340800070719     c                   if        $inzw1 = *on
340900070719     c                   exsr      inzw1
341000070719     c                   eval      $inzw1 = *off
341100070719     c                   endif
341200070719
341300070719     c     window_1      tag
341400070719
341500070719      * emissione video
341600070719      *                  __________________
341700070719     c                   exfmt     or60w01
341800070719      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
341900070719     c                   eval      *in28 = *off
342000070719
342100070719      * selezioni
342200070720     c                   select
342300070719      * f3=fine
342400070719     c                   when      $tasto = f03
342500070719     c                   exsr      f03_esci
342600070720     c                   rolbk
342700070719      * f12=ritorno
342800070719     c                   when      $tasto = f12
342900070720     c                   eval      $gest = 'S2'
343000070720      * scelta = 1 se veniva da SFL 1
343100070720     c                   if        v1sce = '1' and sav$gest ='S1'
343200070720     c                   eval      $gest = 'S1'
343300070719     c                   endif
343400070719
343500070720     c                   other
343600070719      * controllo
343700070719     c                   exsr      ctrw1
343800070720     c                   endsl
343900070719
344000070719      * per errori
344100070719     c                   if        *in28
344200070719     c                   goto      window_1
344300070719     c                   endif
344400070719
344500070719      * f6=aggiorna
344600070719     c                   if        $tasto = f06
344700070719     c                   exsr      f06_aggiorna
344800070719     c                   endif
344900140529
345000140530      /free
345100140530       //?F8=Aggiorna + Preparazione dati
345200140530         IF  $tasto = F08;
345300140530           exsr F06_aggiorna;
345400140530           InviaDatiPDA = *on;
345500140530         ENDIF;
345600140530      /end-free
345700070719
345800070718     c                   endsr
345900070719
346000070719      *------------------------------------------------------------------------*
346100070719      *   Inizializzo window finale prima dell'aggiornamento     ?
346200070719      *------------------------------------------------------------------------*
346300070719     c     inzw1         begsr
346400070719
346500070719     c                   eval      *in28 = *off
346600070719     c                   eval      *in50 = *off
346700070719     c                   setoff                                       515253
346800070719     c                   setoff                                       545556
346900140529     c                   eval      InviaDatiPda = *off
347000140530     c                   eval      *in42 = *off
347100070719
347200070719      * pulizia formato record
347300070719     c                   clear                   or60w01
347400070719
347500070719      * imposta i campi a video
347600070719     c                   eval      w1snor1 = totnor_s2
347700070719     c                   eval      w1spkg1 = totpkg_s2
347800070719     c                   eval      w1svlm1 = totvlm_s2
347900070719
348000070719     c                   eval      w1snor2 = totnor_s2
348100070719     c                   eval      w1spkg2 = totpkg_s2
348200070719     c                   eval      w1svlm2 = totvlm_s2
348300070719
348400070719     c                   select
348500070719      *  Assegnare
348600070719     c                   when      v1sce = '1'
348700070719     c                   eval      w1ndc1 = h1ndc_piu
348800070719     c                   eval      w1ada = 'alla'
348900070720     c                   eval      %subst(w1dsce:6:11) = 'ASSEGNA'
349000140530     c                   eval      *in42 = *on
349100070719
349200070719      *  Togliere
349300070719     c                   when      v1sce = '2'
349400070719     c                   eval      w1ndc1 = h1ndc_meno
349500070719     c                   eval      w1ada = 'dalla'
349600070719     c                   eval      %subst(w1dsce:7:11) = 'TOGLIE'
349700070719
349800070719      *  Trasferire
349900070719     c                   when      v1sce = '3'
350000070719      * se sta eseguendo il trasferimento occorre aprire la finestra
350100070719     c                   eval      *in50 = *on
350200070719     c                   eval      w1ndc1 = h1ndc_meno
350300070719     c                   eval      w1ndc2 = h1ndc_piu
350400070719     c                   eval      w1ada = 'dalla'
350500070719     c                   eval      %subst(w1dsce:6:11) = 'TRASFERISCE'
350600140530     c                   eval      *in42 = *on
350700070719
350800070719     c                   endsl
350900070719
351000070719      * legge la distinta
351100070719     c                   move      w1ndc1        wnfv
351200070719     c                   eval      aggior_dist = 'N'
351300070719     c                   exsr      chain_dist
351400070720      * distinta non trovata
351500070719     c                   if        found_dist = 'N'
351600070719     c                   eval      w1pdr1 = *all'?'
351700070719     c                   else
351800070719     c     kapd          chain     fiapd01l
351900070719     c                   if        %found(fiapd01l)
352000070719     c                   eval      w1pdr1 = apdrsc
352100070719     c                   endif
352200070719
352300070731     c                   eval      w1dmpkg1 = dstrpkl
352400070731     c                   eval      w1dmvlm1 = dstrvlu
352500070731     c                   eval      w1dmnor1 = dstrnbl
352600070903
352700070903     c                   eval      w1daspk1 = dstpkla
352800070903     c                   eval      w1dasvl1 = dstvlua
352900070903     c                   eval      w1dansp1 = dstnbla
353000070719
353100070731     c                   eval      w1dapkg1 = dstrpkla
353200070731     c                   eval      w1davlm1 = dstrvlua
353300070731     c                   eval      w1danor1 = dstrnbla
353400070719     c                   endif
353500070719
353600070719      * legge la distinta
353700070719     c                   move      w1ndc2        wnfv
353800070719     c                   eval      aggior_dist = 'N'
353900070719     c                   exsr      chain_dist
354000070720      * distinta non trovata
354100070719     c                   if        found_dist = 'N'
354200070719     c                   eval      w1pdr2 = *all'?'
354300070719     c                   else
354400070719     c     kapd          chain     fiapd01l
354500070719     c                   if        %found(fiapd01l)
354600070719     c                   eval      w1pdr2 = apdrsc
354700070719     c                   endif
354800070719
354900070731     c                   eval      w1dmpkg2 = dstrpkl
355000070731     c                   eval      w1dmvlm2 = dstrvlu
355100070731     c                   eval      w1dmnor2 = dstrnbl
355200070903
355300070903     c                   eval      w1daspk2 = dstpkla
355400070903     c                   eval      w1dasvl2 = dstvlua
355500070903     c                   eval      w1dansp2 = dstnbla
355600070719
355700070731     c                   eval      w1dapkg2 = dstrpkla
355800070731     c                   eval      w1davlm2 = dstrvlua
355900070731     c                   eval      w1danor2 = dstrnbla
356000070719     c                   endif
356100070719
356200070719     c                   select
356300070719      * Assegnare
356400070719     c                   when      v1sce = '1'
356500070719     c     w1danor1      add       totnor_s2     w1tnor1
356600070719     c     w1dapkg1      add       totpkg_s2     w1tpkg1
356700070719     c     w1davlm1      add       totvlm_s2     w1tvlm1
356800070719     c     w1tnor1       comp      w1dmnor1                           51
356900070719     c     w1tpkg1       comp      w1dmpkg1                           52
357000070719     c     w1tvlm1       comp      w1dmvlm1                           53
357100070719
357200070719      * Togliere
357300070719     c                   when      v1sce = '2'
357400070719     c     w1danor1      sub       totnor_s2     w1tnor1
357500070719     c     w1dapkg1      sub       totpkg_s2     w1tpkg1
357600070719     c     w1davlm1      sub       totvlm_s2     w1tvlm1
357700070719     c     w1tnor1       comp      w1dmnor1                           51
357800070719     c     w1tpkg1       comp      w1dmpkg1                           52
357900070719     c     w1tvlm1       comp      w1dmvlm1                           53
358000070719
358100070719      * Trasferire
358200070719     c                   when      v1sce = '3'
358300070719     c     w1danor1      sub       totnor_s2     w1tnor1
358400070719     c     w1dapkg1      sub       totpkg_s2     w1tpkg1
358500070719     c     w1davlm1      sub       totvlm_s2     w1tvlm1
358600070719     c     w1tnor1       comp      w1dmnor1                           51
358700070719     c     w1tpkg1       comp      w1dmpkg1                           52
358800070719     c     w1tvlm1       comp      w1dmvlm1                           53
358900070719
359000070719     c     w1danor2      add       totnor_s2     w1tnor2
359100070719     c     w1dapkg2      add       totpkg_s2     w1tpkg2
359200070719     c     w1davlm2      add       totvlm_s2     w1tvlm2
359300070719     c     w1tnor2       comp      w1dmnor2                           54
359400070719     c     w1tpkg2       comp      w1dmpkg2                           55
359500070719     c     w1tvlm2       comp      w1dmvlm2                           56
359600070719
359700070719     c                   endsl
359800070719
359900070719     c                   endsr
360000070719
360100070720      *------------------------------------------------------------------------*
360200070720      *  Controlli della finestra di conferma aggiornamento         ?
360300070720      *------------------------------------------------------------------------*
360400070720     c     ctrw1         begsr
360500070720
360600070720     c                   eval      *in28 = *off
360700070720      * se superano i MAX previsti in distinta
360800070720     c                   if        v1sce = '3'
360900070720     c     w1tnor2       comp      w1dmnor2                           54
361000070720     c     w1tpkg2       comp      w1dmpkg2                           55
361100070720     c     w1tvlm2       comp      w1dmvlm2                           56
361200070719     c                   else
361300070720     c     w1tnor1       comp      w1dmnor1                           51
361400070720     c     w1tpkg1       comp      w1dmpkg1                           52
361500070720     c     w1tvlm1       comp      w1dmvlm1                           53
361600070720     c                   endif
361700070720
361800070720     c                   endsr
361900070719
362000070719      *------------------------------------------------------------------------*
362100070720      *  Gestione sfl ORM da selezionare                        ?
362200070719      *------------------------------------------------------------------------*
362300070719     c     gess3         begsr
362400070720
362500070720      * inizializzazione videata
362600070720     c                   if        $inzs3 = *on
362700070720     c                   exsr      inzs3
362800070720     c                   eval      $inzs3 = *off
362900070720     c                   endif
363000070720
363100070720      * fase di commit o rollback    ?
363200070720     c                   if        exec_rb = 'S'
363300070720      * esegue il rollback x un precedente problema su uno degli aggiornamenti
363400070720     c                   rolbk
363500070720     c                   else
363600070720      * esegue il commit quanto tutto è ok?
363700070720     c                   commit
363800080714      * se è togli o trasferisci devo quadrare i dati di PDO con quelli di ORM
363900140121      * e i dati delle telefonate AUT
364000080714     c                   if        v1sce = '2' or v1sce = '3'
364100080714     c                   exsr      sr_quadrapdo
364200080714     c                   endif
364300140530      /free
364400140530       //?Se richiesto 'Metti' o 'Trasferisci' e F08 e NON ci sono stati errori
364500140530       //?devo richiamare la preparazione dati a PDA
364600140530         IF  (V1sce = '1' or V1sce = '3') and InviaDatiPDA and exec_rb <> 'S';
364700140530           clear FIOR56DS;
364800140530           fior56ds.OR56tla = 'T';
364900140530           fior56ds.OR56fgs = wfgs;
365000140530           fior56ds.OR56ddc = wddc;
365100140530           fior56ds.OR56ndcd = h1ndc_piu;
365200140530           fior56ds.OR56ndca = h1ndc_piu;
365300140530           fior56ds.OR56cmd = 'I';
365400140530           kpjbu = FIOR56DS;
365500140530           fior56r (kpjba:FIOR56DS);
365600140530         ENDIF;
365700140530      /end-free
365800070720     c                   endif
365900070720
366000070720      * inizializza e imposta piede videata
366100070720     c                   exsr      inzz3
366200070720
366300070720      * emissione piede videata
366400070720      *                  __________________
366500070720     c                   write     or60z03
366600070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
366700071002      * qualcosa è andato storto
366800080714     c                   if        wmax > *zeros
366900070720     c                   eval      vedi_video = 'S'
367000070720     c                   eval      *in20 = *on
367100070720     c                   if        wsfl > *zeros
367200070720     c                   eval      vc3rcd = wsfl
367300070720     c                   else
367400070720     c                   if        wpag > *zeros
367500070720     c                   eval      vc3rcd = wpag
367600070720     c                   endif
367700070720     c                   endif
367800070720     c                   endif
367900080714     c                   if        exec_rb = 'S'
368000080714     c                   eval      vedi_video = 'S'
368100080717     c                   else
368200080717     c                   clear                   vedi_video
368300080714     c                   endif
368400070720
368500070720      * emissione SFL dopo aggiornamento dati
368600070720      * con errori quindi tutta la transazione deve essere rifatta
368700070720     c                   if        vedi_video = 'S'
368800070720      *                  __________________
368900070720     c                   exfmt     or60c03
369000070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
369100070720     c                   endif
369200070720     c                   eval      *in28 = *off
369300070720      *
369400070720     c                   if        vc3nrr <> *zeros
369500070720     c                   eval      wsfl = vc3nrr
369600070720     c                   endif
369700070720     c                   eval      vc3rcd = sflnrr
369800070720      * selezioni
369900070720     c                   select
370000070720      * f3=fine
370100070720     c                   when      $tasto = f03
370200070720     c                   exsr      f03_esci
370300070720
370400070720     c                   other
370500070720     c                   eval      *in28 = *off
370600070731     c                   endsl
370700070720
370800070720      * riemette il primo video
370900070720     c                   eval      $gest = 'D1'
371000070720      * non reinizializzato se ci sono stati problemi
371100070720     c                   if        wmax > *zeros
371200070720     c                   eval      $inzd1 = *off
371300070720     c                   rolbk
371400070720     c                   else
371500070720      * reinizializzato se non ci sono stati problemi
371600070720     c                   eval      $inzd1 = *on
371700070720     c                   endif
371800070720      * se si era scelto di Assegnare si deve ritornare sul SFL dei GIRI
371900070720      * dove era impostata la scelta e pulire la distinta x riselezionarla.
372000070720     c                   if        v1sce = '1'
372100131118     c                   eval      waggiorna = *on
372200131114     c                   eval      $gest = 'S2'
372300131114     c                   eval      $inzs2 = *on
372400070720     c                   clear                   h1ndc_piu
372500070720     c                   clear                   h1pdr_piu
372600070720     c                   clear                   h1fpp_piu
372700070919     c                   clear                   h1spi_piu
372800070809     c                   clear                   con_giro
372900070809     c                   clear                   wcgi
373000070720     c                   eval      wsfl = sav_wsfl
373100131119     c                   eval      wmax = sav_wmax2
373200070720     c                   eval      wpag = sav_wpag
373300070720      * avendo assegnato l'unico record non posso riemettere il SFL vuoto
373400070720      * quindi torno al video introduttivo.
373500070720     c                   if        wmax  = 1
373600131114     c                   eval      $gest = 'S1'
373700131114     c                   eval      $inzs1 = *on
373800070720     c                   endif
373900070720
374000070720     c                   else
374100070720      * se togliere o trasferire
374200070720      * vado direttamente sull'interrogazione deistinte
374300070720     c                   eval      salva_v1sce = v1sce
374400070720     c                   eval      salta_video1 = 'S'
374500070720     c                   eval      $gest = 'D1'
374600070720     c                   eval      $inzd1 = *off
374700070720     c                   exsr      inzd1
374800070720     c                   eval      v1sce = salva_v1sce
374900070720     c                   endif
375000070720
375100070720     c                   endsr
375200070720
375300070720      *------------------------------------------------------------------------*
375400070720      *  Inizializzazione e imposta il piede del sfl               ?
375500070720      *------------------------------------------------------------------------*
375600070720     c     inzz3         begsr
375700070720
375800070720      * messaggio o tasti funzionali
375900070720     c                   clear                   z3dmsg
376000070720     c                   clear                   z3der1
376100070720     c                   clear                   z3der2
376200070720
376300070720      * se ci sono dei records allora ci sono stati dei problemi
376400070720      * durante l'aggiornamento orm x orm
376500070720     c                   if        wmax > *zeros
376600070720     c                   eval      *in57 = *on
376700070720     c                   eval      z3der1 = msg(27)
376800070720     c                   eval      z3der2 = msg(28)
376900070720
377000070720     c                   else
377100070720      * se ci sono stati dei problemi durante gli aggiornamenti
377200070720      * o su ORM o su DST tanto da eseguire comunque il rollback
377300070720      *  imposta un altro tipo di messaggio.
377400070720     c                   if        exec_RB = 'S'
377500070720     c                   eval      *in57 = *on
377600070720     c                   eval      z3der1 = msg(29)
377700070720     c                   eval      z3der2 = msg(30)
377800070720     c                   endif
377900070720     c                   endif
378000070720
378100070720      * tasti funzionali se non è stato emesso alcun messaggio
378200070720     c                   if        z3dmsg = *blank
378300070720     c                   eval      *in28 = *off
378400070720     c                   eval      z3dmsg = 'F3=Fine'
378500071002     c                   eval      %subst(z3dmsg:35:13) = 'F9=Altri Dati'
378600071002     c                   eval      %subst(z3dmsg:68:11) = 'F12=Ritorno'
378700070720     c                   endif
378800070720
378900070720     c                   endsr
379000070720
379100070720      *------------------------------------------------------------------------*
379200070720      *  Inizializzazione lista ORM da giri o da distinta
379300070720      *------------------------------------------------------------------------*
379400070720     c     inzs3         begsr
379500070720
379600070720      * pulizia sfl
379700070720     c                   eval      *in20 = *off
379800070720     c                   eval      *in21 = *off
379900070720      *                  __________________
380000070720     c                   write     or60c03
380100070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
380200070720     c                   eval      *in21 = *on
380300070720
380400070720      * righe x pagina del 3° sfl
380500070720     c                   eval      s3pag = rigpag_sfl3
380600070720
380700070720      * caricamento sfl totale
380800070720     c                   clear                   s3nrr
380900070720     c                   eval      vc3rcd = 1
381000070720     c                   clear                   wmax
381100070720     c                   clear                   wsfl
381200070720      * tipo funzione
381300070720     c                   select
381400070720     c                   when      v1sce = '1'
381500070720     c                   eval      vc3dsce = 'ASSEGNA'
381600070720     c                   eval      vc3dess = 'alla Distinta nr.' +
381700070720     c                             %trim(%editc(h1ndc_piu:'Z'))
381800070720
381900070720     c                   when      v1sce = '2'
382000070720     c                   eval      vc3dsce = 'TOGLIE'
382100070720     c                   eval      vc3dess = 'dalla Distinta nr.' +
382200070720     c                             %trim(%editc(h1ndc_meno:'Z'))
382300070720
382400070720     c                   when      v1sce = '3'
382500070720     c                   eval      vc3dsce = 'TRASFERISCE'
382600070720     c                   eval      vc3dess = 'dalla Distinta nr.' +
382700070720     c                             %trim(%editc(h1ndc_meno:'Z'))
382800070720
382900070720     c                   endsl
383000070720
383100070720      * carica il sfl
383200070720     c                   exsr      rols3
383300070720
383400070720     c                   eval      wpag = 1
383500070720
383600070720     c                   endsr
383700070720
383800070720      *------------------------------------------------------------------------*
383900070720      *  Esegue aggiornamento dei dati caricando il SFL3 x problemi ?
384000070720      *------------------------------------------------------------------------*
384100070720     c     rols3         begsr
384200070720
384300070720     c                   eval      *in22 = *off
384400070720     c                   clear                   yy
384500070720     c                   eval      s3nrr = wmax
384600070720
384700070720      *  L'aggiornamento è pilotato dalle scelte effettuate sul SFL2 ?
384800070720      *  degli ORM e il caricamento del SFL3 serve per elencare    ?
384900070720      *  gli ORM che non sono stati aggiornati x qualsiasi motivo   ?
385000070720      *  classico motivo: allocazione record
385100070720     c                   exsr      agg_da_sfl2
385200070720      *  Dopo aver aggiornato i files x singolo ORM, aggiorna ?
385300070720      *  a livello di distinta a meno che non ci sia già da      ?
385400070720      *  eseguire il rollback x precedenti problemi           ?
385500070720     c                   if        exec_rb <> 'S'
385600070720     c                   exsr      agg_x_dist
385700070720     c                   endif
385800070720
385900070720      * fine caricamento
386000070720     c                   eval      wmax = s3nrr
386100070720
386200070720      * posizionamento al 1° rcd della pagina
386300070720     c                   eval      pagine = %div(s3nrr:s3pag)
386400070731     c                   eval      resto = %rem(s3nrr:s3pag)
386500070720     c                   eval      vc3rcd = pagine * s3pag
386600070720     c                   if        resto > *zeros
386700070720     c                   eval      vc3rcd = vc3rcd + 1
386800070720     c                   else
386900070720     c                   eval      vc3rcd = vc3rcd - s3pag
387000070720     c                   eval      vc3rcd = vc3rcd + 1
387100070720     c                   endif
387200070720
387300070720     c                   endsr
387400070720
387500070720      *------------------------------------------------------------------------*
387600070720      *  Carica il SFL degli ORM non aggiornati se ci sono stati problemi?
387700070720      *------------------------------------------------------------------------*
387800070720     c     agg_da_sfl2   begsr
387900070720
388000070720      * si ricorda di eseguire il rollback se si sono verificati dei
388100070720      * problemi in qualsiasi parte dell'aggiornamento dei dati
388200070720     c                   eval      exec_rb = 'N'
388300070720
388400080708      * legge il SFL degli ORM dove selezionato con (1) le righe
388500080708      * da aggiornare
388600070720     c                   readc     or60S02                                30
388700070720
388800070720     c                   dow       not *in30 and vs2sce = '1'
388900070720
389000070720      * carico il SFL solo se ci sono dei problemi in aggiornamento degli ORM
389100070720      * per allocazione dei records o altro
389200070720     c                   clear                   problemi
389300070720      *                  _______________________
389400070720     c                   exsr      aggiorna_org
389500070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
389600070720      * x problemi sui records in aggiornamento
389700070720     c                   if        problemi = 'S'
389800070720     c                   eval      exec_rb = 'S'
389900070720
390000070720      * imposto i campi del subfile
390100070720     c                   clear                   or60s03
390200070720     c                   exsr      riga_sfl3
390300070720
390400070720      * attivo sempre il sflnxtchg e aggiorno sfl
390500070720     c                   eval      *in22 = *on
390600070720     c                   eval      s3nrr = s3nrr + 1
390700070720     c                   eval      yy = yy + 1
390800070720      *                  __________________
390900070720     c                   write     or60s03
391000070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
391100070720     c                   endif
391200070720
391300070720     c                   readc     or60S02                                30
391400070720     c                   enddo
391500070720
391600070720     c                   endsr
391700070720      *------------------------------------------------------------------------*
391800070720      *  Imposta campi del 3°SFL                                    ?
391900070720      *------------------------------------------------------------------------*
392000070720     c     riga_sfl3     begsr
392100070720
392200070720      * deve semplicemente impostare dal 2° al 3° SFL i campi
392300070927     c                   eval      vs3cgi = vs2cgi
392400070720     c                   eval      vs3poe = vs2poe
392500070720     c                   eval      vs3nsr = vs2nsr
392600070720     c                   eval      vs3nor = vs2nor
392700070720     c                   eval      vs3nrv = vs2nrv
392800070720     c                   eval      vs3rsr = vs2rsr
392900070720     c                   eval      vs3inr = vs2inr
393000070720     c                   eval      vs3lor = vs2lor
393100070720     c                   eval      vs3pkg = vs2pkg
393200070720     c                   eval      vs3vlm = vs2vlm
393300070720     c                   eval      vs3sto = vs2sto
393400070720     c                   eval      vs3ncl = vs2ncl
393500070720     c                   eval      vs3bnc = vs2bnc
393600070720     c                   eval      vs3dar = vs2dar
393700070720     c                   eval      vs3orr = vs2orr
393800131126     c                   eval      vs3orr = vs2ora
393900070720     c                   eval      vs3com = vs2com
394000070720     c                   eval      vs3spi = vs2spi
394100070720     c                   eval      vs3aut = vs2aut
394200071009     c                   eval      vs3all = vs2all
394300070720
394400070720     c                   endsr
394500070720
394600070720      *------------------------------------------------------------------------*
394700070720      *  Aggiornamento dei files a livello di ORM     (COMMIT)    ?
394800070720      *------------------------------------------------------------------------*
394900070720     c     aggiorna_org  begsr
395000070720
395100070720      * aggiorna org x ORM
395200070720     c     kaggiorna     chain(e)  fnorg01l
395300070720      * se ci sono problemi di allocazione record    ?
395400070720     c                   if        %error
395500070720      * problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
395600070720     c                   eval      problemi = 'S'
395700070720     c                   clear                   trul82ds
395800070720     c                   eval      ul82§rrn = org_nrr
395900070720     c                   eval      ul82§fil = 'FNORG01L'
396000070720     c                   eval      ul82§win = 'S'
396100070720     c                   eval      ul82§f7  = 'S'
396200070720     c                   eval      ul82§num = 2
396300070720     c                   eval      ul82§att = 2
396400070720     c                   eval      ul82§mss = msg(07)
396500070720     c                   eval      UL82§msw = 'La Distinta '
396600070720     c                             + %editc(w1ndc1:'Z') +  ' '
396700070720     c                             + ' non è manutenzionabile.'
396800070720      * chiamo il pgm che manda il messaggio info all'utente
396900070720     c                   call(e)   'TRUL82R'
397000070720     c                   parm                    trul82ds
397100070720      * ancora allocata
397200070720     c                   if        ul82§sts = 'A'
397300070720      * problemi durante l'aggiornamento --> eseguire rollback  ?
397400070720     c                   eval      problemi = 'S'
397500070720     c                   endif
397600070720
397700070720     c                   else
397800070720
397900070731     c                   if        %found(fnorg01l)
398000070720      * imposta i campi da aggiornare ?
398100070720     c                   select
398200070720      * Assegna o Trasferisce
398300070720     c                   when      v1sce = '1' or v1sce = '3'
398400080717      * se sto assegnando l'orm non deve avere la distinta su fnorg
398500080717     c                   if        v1sce = '1' and orgndc > 0
398600080717     c                   eval      problemi = 'S'
398700080717     c                   unlock    fnorg01l
398800080717     c                   leavesr
398900080717     c                   endif
399000090303      * se sto assegnando l'orm la filiale ritiro deve essere la stessa della distinta
399100090303     c                   if        v1sce = '1' and orgpor <> wfgs
399200090303     c                   eval      problemi = 'S'
399300090303     c                   unlock    fnorg01l
399400090303     c                   leavesr
399500090303     c                   endif
399600100624      * se sto assegnando l'orm questo deve essere ancora in fase assegnabile
399700100624      * se sto togliendo l'orm questo deve essere ancora in fase assegnabile
399800090403     c     kaggiorna     chain     fnorm01l
399900100409     c                   if        not%found(fnorm01l)
400000100409     c                   eval      problemi = 'S'
400100100409     c                   unlock    fnorg01l
400200100409     c                   leavesr
400300100409     c                   endif
400400100409     c                   if        v1sce = '1' and ormeti = ' '
400500090403     c                   eval      problemi = 'S'
400600090403     c                   unlock    fnorg01l
400700090403     c                   leavesr
400800090403     c                   endif
400900100624     c                   if        v1sce = '3' and ormeti = ' '
401000100409     c                   eval      problemi = 'S'
401100100409     c                   unlock    fnorg01l
401200100409     c                   leavesr
401300090403     c                   endif
401400080717      * se sto trasferendo la distinta su ORG deve essere quella che sto togliendo
401500080717     c                   if        v1sce = '3' and orgndc <> h1ndc_meno
401600080717     c                   eval      problemi = 'S'
401700080717     c                   unlock    fnorg01l
401800080717     c                   leavesr
401900080717     c                   endif
402000070720     c                   eval      orgfgs = wfgs
402100070731     c                   eval      orgpdc = h1pdr_piu
402200070720     c                   eval      orgndc = h1ndc_piu
402300070720     c                   eval      orgddc = h1cddc
402400070720     c                   eval      orgnftl = *all'9'
402500070720     c                   eval      orgslo = *zeros
402600071011     c                   time                    w0140
402700071011     c                   movel     w0140         orghvdis
402800071011     c                   clear                   wlbdat
402900071011     c                   move      w0140         g08dat
403000071011     c                   call      'XSRDA8'
403100071011     c                   parm                    wlbdat
403200071011     c                   z-add     g08inv        orgdtvdis
403300070720      * Toglie
403400070720     c                   when      v1sce = '2'
403500100624      * se sto togliendo l'orm questo deve essere ancora in fase assegnabile
403600100409     c     kaggiorna     chain     fnorm01l
403700100624     c                   if        not%found(fnorm01l) or ormeti = ' '
403800100409     c                   eval      problemi = 'S'
403900100409     c                   unlock    fnorg01l
404000100409     c                   leavesr
404100100409     c                   endif
404200070720     c                   clear                   orgpdc
404300070720     c                   clear                   orgfgs
404400070720     c                   clear                   orgndc
404500070720     c                   clear                   orgddc
404600070921     c                   clear                   orgnftl
404700070720     c                   clear                   orgslo
404800070720     c                   clear                   orgdtvdis
404900070720     c                   clear                   orghvdis
405000070720
405100070720     c                   endsl
405200070720
405300070720     c                   update    fnorg000
405400070720     c                   endif
405500070720
405600070720     c                   endif
405700070720
405800070720     c                   endsr
405900070720
406000070720      *------------------------------------------------------------------------*
406100070720      *  Aggiornamento dei files a livello distinta   (COMMIT)    ?
406200070731      *------------------------------------------------------------------------*
406300070720     c     agg_x_dist    begsr
406400070720
406500070720      * Deve aggiornare anche ORM solo se siamo in produzione  ?
406600070720      * mediante il FIOR61R ma a livello di distinta ?
406700070720     c                   if        test_simula <> 'S' and exec_rb <> 'S'
406800070720
406900070720     c                   clear                   fior61ds
407000070720     c                   eval      or61cmti = 'N'
407100070720     c                   eval      or61fgsi = wfgs
407200070720     c                   eval      or61dfvi = h1cddc
407300070720      * Se deve Assegnare: ?
407400070720     c                   if        v1sce = '1'
407500070720     c                   eval      or61mtdi = 'M'
407600070720     c                   eval      or61nfvi = h1ndc_piu
407700070921     c                   eval      or61nftl = 99999
407800070720     c                   endif
407900070905      * Se deve Togliere: ?
408000070905     c                   if        v1sce = '2'
408100070720     c                   eval      or61mtdi = 'T'
408200070720     c                   eval      or61nfvi = h1ndc_meno
408300071106     c                   eval      or61nftl = 99998
408400070720     c                   endif
408500070905      * Se deve Trasferire:
408600070905     c                   if        v1sce = '3'
408700070905     c                   eval      or61mtdi = 'X'
408800070905     c                   eval      or61nfvi = h1ndc_meno
408900070905     c                   eval      or61nfvin = h1ndc_piu
409000071106     c                   eval      or61nftl = 99998
409100070905     c                   endif
409200070904     c                   call      'FIOR61R'
409300070720     c                   parm                    kpjba
409400070720     c                   parm                    fior61ds
409500070720     c                   if        or61erro <> *blanks
409600070720      * problemi durante l'aggiornamento --> eseguire rollback  ?
409700070720     c                   eval      exec_rb  = 'S'
409800070720     c                   endif
409900070731     c                   endif
410000070720
410100070720      * deve aggiornare la o le distinte   ?
410200070720     c                   if        exec_rb  <> 'S'
410300070720      * richiamo in aggiornamento dati
410400070720     c                   move      w1ndc1        wnfv
410500070720      *                       ______________________
410600070720     c                   eval      aggior_dist = 'S'
410700070720      *                       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
410800070720     c                   exsr      chain_dist
410900070720
411000070720     c                   if        error_dist   = 'S'  or
411100070720     c                             rec_allocato = 'S'  or
411200070720     c                             found_dist   = 'N'
411300070720      * problemi durante l'aggiornamento --> eseguire rollback  ?
411400070720     c                   eval      exec_rb  = 'S'
411500070720     c                   endif
411600070720     c                   endif
411700070720
411800070720      * se si tratta di un trasferimento c'è il 2°aggancio alla distinta
411900070720     c                   if        w1ndc2  > 0 and exec_rb  <> 'S'
412000070720      * richiamo in aggiornamento dati
412100070720     c                   move      w1ndc2        wnfv
412200070720      *                       ______________________
412300070720     c                   eval      aggior_dist = 'S'
412400070720      *                       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
412500070720     c                   exsr      chain_dist
412600070720
412700070720     c                   if        error_dist   = 'S'  or
412800070720     c                             rec_allocato = 'S'  or
412900070720     c                             found_dist   = 'N'
413000070720      * problemi durante l'aggiornamento --> eseguire rollback  ?
413100070720     c                   eval      exec_rb  = 'S'
413200070720     c                   endif
413300070720     c                   endif
413400070720
413500070719     c                   endsr
413600070718
413700070718      *------------------------------------------------------------------------*
413800070718      *   Aggancia la distinta x allocarla ed in seguito x aggiornarla ?
413900070718      *------------------------------------------------------------------------*
414000070718     c     ric_dist      begsr
414100070718
414200070718     c                   clear                   fidg31ds
414300071030      * posizionamento cursore nel subfile per interrogazione distinte
414400071030     c                   move      savposnf      fdgnfpos
414500070718     c                   eval      fdgfgs = wfgs
414600070718
414700070718     c                   if        $tasto <> f05
414800070718     c                   eval      fdgcgi = wcgi
414900070718     c                   endif
415000070718      *
415100070718     c                   eval      fdggda = wddc
415200070718     c                   eval      fdggal = wddc
415300070718     c                   eval      fdgnpg = wnpg
415400070718     c                   eval      fdgric = 'S'
415500070718     c                   eval      fdgtfv = '2'
415600070718     c                   eval      fdgtpv = 'P'
415700070718     c                   eval      fdgopz = w1sce
415800070718     c                   eval      fdgnf1 = wndc
415900070719     c                   eval      fdgtdis = v1tdis
416000070718     c                   eval      fdgtusc = v1tusc
416100080708      * esclude le distinte con fase RCH e RQE
416200080708     c                   eval      fdgiefas = 'E'
416300080708     c                   eval      fdgfas01 = 'RCH'
416400080708     c                   eval      fdgfas02 = 'RQE'
416500070718     c                   clear                   $fv6
416600070718      * impostato a tutti 99999
416700070718     c                   do        600           xx
416800070718     c                   z-add     999999        $fv6(xx)
416900070718     c                   enddo
417000080715     c                   eval      fdgpgm = 'ASSMANUALE'
417100080611      * Attenzione:
417200080611      * Se richiamato da MONITOR con PDA, occorre modificare
417300080611      * alcuni parametri:
417400080611     c                   if        pgm_chiamante = 'MONITOR'
417500080611     c                             and v1sce = '2'
417600080611     c                   eval      fdgPGM = 'MONITOR'
417700080611     c                   eval      fdgSTP = ' '
417800080611     c                   eval      fdgnfv = o60nfv
417900080708      * se richiamato dal monitor non devo escludere nessuna fase
418000080708     c                   eval      fdgiefas = 'I'
418100080708     c                   clear                   fdgfas01
418200080708     c                   clear                   fdgfas02
418300080611     c                   endif
418400080611
418500070904     c                   eval      fdgtpop = 'R'
418600070718     c                   eval      kpjbu = fidg31ds
418700070718     c                   call      'FIDG31R'
418800070718     c                   parm                    kpjba
418900070718     c                   parm                    $fv6
419000070718     c                   eval      fidg31ds = kpjbu
419100071030      * posizionamento cursore nel subfile per interrogazione distinte
419200071030     c                   if        fdgnf1 > *zeros
419300071030     c                   move      fdgnf1        fdgnfpos
419400071030     c                   endif
419500071030     c                   move      fdgnfpos      savposnf
419600070718     c                   clear                   fdgnf1
419700070718     c                   clear                   wndc
419800070718
419900070718     c                   endsr
420000070718
420100070718      *------------------------------------------------------------------------*
420200070718      *   Aggancia la distinta x allocarla ed in seguito x aggiornarla ?
420300070718      *------------------------------------------------------------------------*
420400070718     c     chain_dist    begsr
420500070718
420600070718      * inizializza il trovato/non trovato
420700070718     c                   eval      found_dist = 'N'
420800070718     c                   eval      error_dist = 'N'
420900070718     c                   eval      rec_allocato = 'N'
421000070718
421100070720      * dalla vista logica prende il nrr
421200070718     c     kdst          chain     fidst01l
421300071029
421400071029      *?Se non trovava il record allocava l'ultima distinta che aveva
421500071029      *?trovato poichè il nrelrec. non si resettava ma era impostato
421600071029      *?da prima.
421700071029     c                   if        not %Found(fidst01l)
421800071029     c                   eval      dst_nrr = *hival
421900071029     c                   end
422000071029
422100070720      * x agganciare il fisico solo x allocare il record  ?
422200070718     c     dst_nrr       chain(e)  fidst00f
422300070720      * è stata usata questa tecnica perchè l'aggiornamento del FIDST
422400070731      * è fatto mediante il FIOR67R e,sotto COMIT,xchè tutto funzioni
422500070720      * correttamente,il file deve essere condiviso in SHARE altrimenti
422600070720      * non funziona. (Si è scelto di utilizzare x tutti il fisico)
422700070718     c                   if        %error
422800070718     c                   eval      error_dist = 'S'
422900070718      * se il record è allocato mando messaggio info all'utente che alloca il rcd
423000070718     c                   clear                   trul82ds
423100070718     c                   eval      ul82§rrn = dst_nrr
423200070718     c                   eval      ul82§fil = 'FIDST00F'
423300070718     c                   eval      ul82§win = 'S'
423400070718     c                   eval      ul82§f7  = 'S'
423500070718     c                   eval      ul82§num = 2
423600070718     c                   eval      ul82§att = 2
423700070718     c                   eval      ul82§mss = msg(07)
423800070720     c                   eval      ul82§msw = 'La Distinta '
423900070718     c                             + %editc(wnfv:'Z') +  ' è già utilizzata da -
424000070718     c                              un altro lavoro e non è manutenzionabile'
424100070718      * chiamo il pgm che manda il messaggio info all'utente
424200070718     c                   call(e)   'TRUL82R'
424300070718     c                   parm                    trul82ds
424400070720      * ancora allocato
424500070718     c                   if        ul82§sts = 'A'
424600070718     c                   eval      rec_allocato = 'S'
424700070718     c                   endif
424800070718     c                   endif
424900070718      * trovato record
425000070718     c                   if        %found(fidst00f)
425100160219      * record annullato
425200160219     c                   IF        DSTatb <> *blanks and
425300160219     c                             aggior_dist = 'S'
425400160219     c                   unlock    fidst00f
425500160219     c                   leavesr
425600160219     c                   ENDIF
425700160219      * record ok
425800070718     c                   eval      found_dist = 'S'
425900070720      * se richiamato x aggiornamento finale dopo ORG e ORM  ?
426000070720      * quindi non solo per semplice allocazione del record. ?
426100070720     c                   if        aggior_dist = 'S'
426200070720     c                   exsr      campi_dist
426300070720     c                   else
426400070720      * qui l'update serve esclusivamente per tenere allocato  ?
426500070802      * il record in quanto nel CL FIOR60C x il COMMIT è    ?
426600070720      * stato attivato lo STRCMTCTL con *CHG.    ?
426700070720     c                   update    fidst000
426800070720     c                   endif
426900070720
427000070718     c                   endif
427100070718
427200070718     c                   endsr
427300080327
427400080327      *------------------------------------------------------------------------*
427500080327      *   Controlla esistenza fase 'RCH' sulla distinta
427600080327      *------------------------------------------------------------------------*
427700080327     c     sr_fase       begsr
427800080327
427900080415     c                   clear                   fior90ds
428000080327     c                   eval      or90tla = 'C'
428100080415     c                   eval      or90fas = 'RCH'
428200080327     c                   eval      or90fgs = wfgs
428300080327     c                   eval      or90ndc = wnfv
428400080327     c                   call      'FIOR90R'
428500080327     c                   parm                    kpjba
428600080327     c                   parm                    fior90ds
428700080415      * se non trovo la fase 'RCH' provo con la fase 'RQE'
428800080415     c                   if        or90ndcok = *blanks
428900080415     c                   clear                   fior90ds
429000080415     c                   eval      or90tla = 'C'
429100080415     c                   eval      or90fas = 'RQE'
429200080415     c                   eval      or90fgs = wfgs
429300080415     c                   eval      or90ndc = wnfv
429400080415     c                   call      'FIOR90R'
429500080415     c                   parm                    kpjba
429600080415     c                   parm                    fior90ds
429700080415     c                   endif
429800080327
429900080327     c                   endsr
430000080714
430100080714      *------------------------------------------------------------------------*
430200080714      *   Quadro i dati di PDO con ORM
430300080714      *------------------------------------------------------------------------*
430400080714     c     sr_quadrapdo  begsr
430500080714
430600080714     c                   clear                   fior57ds
430700080714     c                   eval      or57fgs = wfgs
430800080714     c                   eval      or57ndc = h1ndc_meno
430900080714     c                   call      'FIOR57R'
431000080714     c                   parm                    kpjba
431100080714     c                   parm                    fior57ds
431200080714
431300080714     c                   endsr
431400070720
431500070720      *------------------------------------------------------------------------*
431600070720      *  Imposta i campi della distinta da aggiornare             ?
431700070720      *------------------------------------------------------------------------*
431800070720     c     campi_dist    begsr
431900070720
432000070731     c                   clear                   fior67ds
432100070731     c                   eval      or67fgsi = wfgs
432200070731     c                   eval      or67dfvi = h1cddc
432300070731     c                   eval      or67nfvi = wnfv
432400070731      * non deve eseguire il Commit perchè lo faccio già in questo pgm
432500070731     c                   eval      or67cmti = 'N'
432600070731     c                   eval      kpjbu  = fior67ds
432700070731     c                   call      'FIOR67R'
432800070720     c                   parm                    kpjba
432900080611     c                   eval      fior67ds = kpjbu
433000070731     c                   if        or67erro = *blanks
433100070731     c                   eval      dstrpkla = or67rpklao
433200070731     c                   eval      dstrvlua = or67rvluao
433300070731     c                   eval      dstrstpa = or67rstpao
433400070731     c                   eval      dstrnbla = or67rnblao
433500070720     c                   else
433600070731      * problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
433700070731     c                   eval      error_dist  = 'S'
433800070731     c                   endif
433900070720
434000070720     c                   endsr
434100070718
434200070718      *------------------------------------------------------------------------*
434300070718      *  Controlla validità dell'ORM
434400070718      *------------------------------------------------------------------------*
434500070718     c     check_orm     begsr
434600070718
434700070718      * per controllare se ORM assegnabile richiamo pgm esterno
434800070718     c                   clear                   fior64ds
434900070718     c                   eval      or64poei = ormpoe
435000070718     c                   eval      or64nsri = ormnsr
435100070718     c                   eval      or64nori = ormnor
435200070718     c                   eval      or64nrvi = ormnrv
435300070801      * imposto la data
435400070718     c                   eval      or64dfvi = h1cddc
435500070801      * se sono in simulazione controllo la distinta su ORG
435600070801      * e imposto il flag di controllo ORMETI in simulazione
435700070718     c                   if        test_simula = 'S'
435800070801     c                   eval      or64ndci = 'G'
435900070801     c                   eval      or64etii = 'S'
436000070801     c                   else
436100070801      * se già attivo controllo la distinta su entrambi
436200070801      * e imposto il flag di controllo ORMETI solo per filiale attiva
436300070801     c                   eval      or64ndci = 'E'
436400070801     c                   clear                   or64etii
436500070801     c                   endif
436600070801      * controlla la data di ritiro
436700070801     c                   eval      or64dari = 'S'
436800070801      * se ORM secco (da prima videata) non devo controllare la data ritiro
436900070801     c                   if        $ormsecco = *on
437000070801     c                   clear                   or64dari
437100070801     c                   endif
437200070718     c                   call      'FIOR64R'
437300070718     c                   parm                    kpjba
437400070718     c                   parm                    fior64ds
437500070718
437600070718     c                   endsr
437700070718
437800070718      *------------------------------------------------------------------------*
437900070718      *  Preparazione stringa sql
438000070718      *------------------------------------------------------------------------*
438100070718     c     sr_sqlorg     begsr
438200070718
438300070718     c                   clear                   stringasql
438400070718
438500070718      * preparo la stringa sql
438600070718     c                   eval      stringasql =
438700071025     c                             'select fnorg52j.* from '                  +
438800071025     c                             'fnorg52j '                                +
438900071025     c                             'where orgpor = '                          +
439000071025     c                              %editc(wfgs:'X') + ' '
439100070718      * se presente il giro seleziono solo quel giro
439200070718     c                   if        con_giro = 'S'
439300070718     c                   eval      stringasql =
439400070718     c                             %trim(stringasql) + ' '                     +
439500070718     c                             'and orgpocgi = ' + %editc(wfgs:'X') + ' '  +
439600070719     c                             'and orgcgi = ' + '''' + wcgi + ''''
439700070718     c                   endif
439800070718      * solo gli orm senza giro
439900070718     c                   if        con_giro = 'N'
440000070718     c                   eval      stringasql =
440100070718     c                             %trim(stringasql) + ' '                     +
440200070718     c                             'and orgpocgi = 0 '                         +
440300070719     c                             'and orgcgi = ' + '''' + wcgi + ''''
440400070718     c                   endif
440500070719      * se presente l'ORM seleziono solo quell'ORM
440600070719     c                   if        v1cnor <> *zeros
440700070719     c                   eval      stringasql =
440800070719     c                             %trim(stringasql) + ' '                     +
440900070719     c                             'and ormpoe = ' +  %editc(v1cpoe:'X') + ' ' +
441000070719     c                             'and ormnsr = ' +  %editc(v1nsr:'X') + ' '  +
441100070719     c                             'and ormnor = ' +  %editc(v1nor:'X') + ' '  +
441200070719     c                             'and ormnrv = ' +  %editc(v1cnrv:'X') + ' '
441300070719     c                   endif
441400070718      * se assegna
441500070718     c                   if        v1sce = '1'
441600071022      * solo gli ORM senza Serie
441700071022     c                   eval      stringasql =
441800071022     c                             %trim(stringasql) + ' '                     +
441900071022     c                             'and ormnsr = 0 '
442000071022      * se non è stato richiesto un ORM specifico
442100071022     c                   if        $ormsecco = *off
442200071022      * eclude ORM con allegati
442300071022     c                   if        v1all = 'E'
442400071022     c                   eval      stringasql =
442500071022     c                             %trim(stringasql) + ' '                     +
442600071022     c                             'and ormtor <> ''P'' '                      +
442700071022     c                             'and substr(ormflo, 9, 1) <> ''S'''
442800071022     c                   endif
442900071022      * include ORM con allegati
443000071022     c                   if        v1all = 'I'
443100071022     c                   eval      stringasql =
443200071022     c                             %trim(stringasql) + ' '                     +
443300071022     c                             'and (ormtor = ''P'' '                      +
443400071022     c                             'or substr(ormflo, 9, 1) = ''S'')'
443500071022     c                   endif
443600161108      /free
443700161108       //?Esclude ORM con peso inferiore a 5 Kg.
443800161108         IF  V1peso = 'E';
443900161108           stringasql = %trim(stringasql) + ' and ORGpkg >= ' +
444000161108                        %editc(d§DFTpkgdt:'4');
444100161108         ENDIF;
444200161108       //?Include ORM con peso inferiore a 5 Kg.
444300161108         IF  V1peso = 'I';
444400161108           stringasql = %trim(stringasql) + ' and ORGpkg < ' +
444500161108                        %editc(d§DFTpkgdt:'4');
444600161108         ENDIF;
444700161108      /end-free
444800161108
444900071022     c                   endif
445000070718      * se simulazione solo orgndc = 0
445100070718     c                   if        test_simula = 'S'
445200070718     c                   eval      stringasql =
445300070718     c                             %trim(stringasql) + ' '                     +
445400070718     c                             'and orgndc = 0'
445500070718     c                   endif
445600070718      * se filiale già attiva solo ormndc = 0
445700070718     c                   if        test_simula <> 'S'
445800070718     c                   eval      stringasql =
445900070718     c                             %trim(stringasql) + ' '                     +
446000070718     c                             'and ormndc = 0'
446100070718     c                   endif
446200070718      * ordinato per p.o. giro e giro
446300070718     c                   eval      stringasql =
446400070718     c                             %trim(stringasql) + ' '                     +
446500071025     c                             'order by orgpor, orgcgi'
446600070718     c                   endif
446700070718      * se toglie o trasferisce
446800070718     c                   if        v1sce = '2' or v1sce = '3'
446900070718      * se simulazione solo orgndc = 0
447000070718     c                   if        test_simula = 'S'
447100070718     c                   eval      stringasql =
447200070718     c                             %trim(stringasql) + ' '                     +
447300070718     c                             'and orgndc = ' + %editc(h1ndc_meno:'X')
447400070718     c                   endif
447500070718      * se filiale già attiva solo ormndc = 0
447600070718     c                   if        test_simula <> 'S'
447700070718     c                   eval      stringasql =
447800070718     c                             %trim(stringasql) + ' '                     +
447900070718     c                             'and ormndc = ' + %editc(h1ndc_meno:'X')
448000070718     c                   endif
448100070718     c                   endif
448200070718
448300070718     c                   endsr
448400070718
448500070718      *------------------------------------------------------------------------*
448600070718      *   Apro il cursore per ORM+ORG
448700070718      *------------------------------------------------------------------------*
448800070718     c     sr_sqlopen    begsr
448900070718
449000070718     C/EXEC SQL
449100070718     C+ PREPARE S2 FROM :stringasql
449200070718     C/END-EXEC
449300070718
449400070718     C/EXEC SQL
449500070718     C+ DECLARE A2 CURSOR FOR S2
449600070718     C/END-EXEC
449700070718
449800070718     C/EXEC SQL
449900070718     C+ OPEN A2
450000070718     C/END-EXEC
450100070718
450200070718     c                   endsr
450300070718
450400070718      *------------------------------------------------------------------------*
450500070718      *   Chiudo il cursore per ORM+ORG
450600070718      *------------------------------------------------------------------------*
450700070718     c     sr_sqlclose   begsr
450800070718
450900070718     C/EXEC SQL
451000070718     C+ CLOSE A2
451100070718     C/END-EXEC
451200070718
451300070718     c                   endsr
451400070621
451500070621      *------------------------------------------------------------------------*
451600070705      *   Seleziona tutti i record del sfl                         ?
451700070621      *------------------------------------------------------------------------*
451800070704     c     f01_1_sel     begsr
451900070621
452000070622     c                   if        wmax > *zeros
452100070622      * seleziona tutte le righe del sfl accendendo il sflnxtchg
452200070622     c                   eval      nrr_s1 = 1
452300070622     c     nrr_s1        chain     or60s01                            30
452400070622     c                   dow       not *in30
452500070622     c                   eval      vs1sce = '1'
452600070622
452700070622      * attivo sempre il sflnxtchg e aggiorno sfl
452800070622     c                   eval      *in22 = *on
452900070622     c                   update    or60s01
453000070622
453100070622     c                   eval      nrr_s1 = nrr_s1 + 1
453200070622     c     nrr_s1        chain     or60s01                            30
453300070622     c                   enddo
453400070622     c                   endif
453500070621
453600070621     c                   endsr
453700070622
453800070622      *------------------------------------------------------------------------*
453900070705      *   Seleziona tutti i record del sfl                         ?
454000070622      *------------------------------------------------------------------------*
454100070704     c     f01_2_sel     begsr
454200070622
454300070622     c                   if        wmax > *zeros
454400070622      * seleziona tutte le righe del sfl accendendo il sflnxtchg
454500070622     c                   eval      nrr_s2 = 1
454600070622     c     nrr_s2        chain     or60s02                            30
454700070622     c                   dow       not *in30
454800071022      * solo se non protetto il campo della scelta
454900071022     c                   if        vs2prot = *off
455000071022     c                   eval      vs2sce = '1'
455100071022     c                   endif
455200070921
455300080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
455400080314     c                   if        vs2comun = *on or vs2pda = *on
455500080314     c                   eval      *in08 = *on
455600080314     c                   else
455700080314     c                   eval      *in08 = *off
455800080314     c                   endif
455900071022      * protezione o meno campo scelta
456000071022     c                   eval      *in09 = vs2prot
456100080723      * imposto se filiale ha PDA
456200080723     c                   eval      *in17 = (pdaorm <> *blanks)
456300070622      * attivo sempre il sflnxtchg e aggiorno sfl
456400070622     c                   eval      *in22 = *on
456500071022      *                  __________________
456600070622     c                   update    or60s02
456700071022      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
456800070622     c                   eval      nrr_s2 = nrr_s2 + 1
456900070622     c     nrr_s2        chain     or60s02                            30
457000070622     c                   enddo
457100070622     c                   endif
457200070622
457300070622     c                   endsr
457400070719
457500070719      *------------------------------------------------------------------------*
457600070719      *   Gestione F02
457700070719      *------------------------------------------------------------------------*
457800070719     c     f02_intorm    begsr
457900070719
458000150625     c                   clear                   fior010ds
458100150625     c                   eval      IOR010ric = '1'
458200150625     c                   eval      IOR010por = v1cfgs
458300150625     c                   call      'FIOR010R'
458400070719     c                   parm                    kpjba
458500150625     c                   parm                    fior010ds
458600150625     c                   if        OOR010F03 = *blanks
458700150625     c                   eval      v1cpoe = OOR010poe
458800150625     c                   eval      v1nsr  = OOR010nsr
458900150625     c                   eval      v1nor  = OOR010nor
459000150625     c                   eval      v1cnrv = OOR010nrv
459100070719     c                   endif
459200070719
459300070719     c                   endsr
459400070718
459500070621      *------------------------------------------------------------------------*
459600070705      *   Gestione F03
459700070621      *------------------------------------------------------------------------*
459800070704     c     f03_esci      begsr
459900070621
460000070621     c                   eval      $fine = *on
460100070621
460200070621     c                   endsr
460300070621
460400070621      *------------------------------------------------------------------------*
460500070705      *   Gestione F04                                                ?
460600070621      *------------------------------------------------------------------------*
460700070704     c     f04_int       begsr
460800070621
460900070622     c                   eval      *in28 = *on
461000070919     c                   eval      wforzaspi = *off
461100070919
461200070622     c                   eval      wcgi = sav_sxcgi
461300070622     c                   clear                   wndc
461400070622     c                   eval      w1sce = '1'
461500070622     c                   if        v1sce = '3'
461600070622     c                   move      v1ndc3        wndc
461700070622     c                   endif
461800070704     c                   exsr      ric_dist
461900070622
462000070622      * controlla la distinta selezionata
462100070704     c                   z-add     fdgnfv        wnfv
462200070622     c                   eval      aggior_dist = 'N'
462300070704     c                   exsr      chain_dist
462400070622     c                   if        error_dist = 'S'
462500070622     c                   if        rec_allocato = 'S'
462600070622     c                   eval      $ndcalloc = *on
462700070622     c                   endif
462800070622     c                   eval      *in28 = *on
462900070622     c                   leavesr
463000070622     c                   endif
463100070622      * distinta non trovata
463200070622     c                   if        found_dist = 'N'
463300070622     c                   eval      $ndcerr = *on
463400070622     c                   eval      *in28 = *on
463500070622     c                   leavesr
463600070622     c                   endif
463700070622      * distinta chiusa o annullata
463800070622     c                   if        dstfcf = 'S' or dstatb <> *blanks
463900070622     c                   eval      $ndcchiu = *on
464000070622     c                   eval      *in28 = *on
464100070622     c                   leavesr
464200070622     c                   endif
464300100409      *     controllo esistenza fase 'RCH' o 'RQE'
464400100409     c                   exsr      sr_fase
464500100409      * se trova la fase (una delle due)
464600100409      * non si può più assegnare ORM a questa distinta
464700100409     c                   if        or90ndcok <> *blanks
464800100409     c                   eval      $ndcchiu = *on
464900100409     c                   eval      *in28 = *on
465000100409     c                   leavesr
465100100409     c                   endif
465200070622      * se tutto OK
465300070622     c                   eval      v1ndc1 = *zeros
465400070704     c                   move      fdgnfv        v1ndc1
465500070704     c                   eval      h1ndc_piu = fdgnfv
465600070720     c                   eval      h1pdr_piu = dstpdr
465700070720     c                   eval      h1fpp_piu = dstfpp
465800070919     c                   eval      h1spi_piu = dstrspi
465900070622
466000070622      * intestazione sf1
466100070622     c                   if        v1sce = '1' and h1ndc_piu > *zeros
466200070622     c                   eval      vc1ndc1 = 'alla_dist.:' +
466300080912     c                             %editc(h1ndc_piu:'Z')
466400070622     c                   eval      vc2dsce = 'ASSEGNA'
466500070622     c                   eval      vc2dess = 'alla Distinta nr.' +
466600070622     c                             %trim(%editc(h1ndc_piu:'Z'))
466700070622     c                   endif
466800070622
466900070622     c                   endsr
467000070621
467100070621      *------------------------------------------------------------------------*
467200070705      *   Gestione F05                                               ?
467300070621      *------------------------------------------------------------------------*
467400070704     c     f05_int       begsr
467500070621
467600070622     c                   eval      *in28 = *on
467700070919     c                   eval      wforzaspi = *off
467800070919
467900070622     c                   clear                   wndc
468000070622     c                   eval      w1sce = '1'
468100070622     c                   if        v1sce = '3'
468200070622     c                   move      v1ndc3        wndc
468300070622     c                   endif
468400070704     c                   exsr      ric_dist
468500070622
468600070622      * controlla la distinta selezionata
468700070704     c                   z-add     fdgnfv        wnfv
468800070622     c                   eval      aggior_dist = 'N'
468900070704     c                   exsr      chain_dist
469000070622     c                   if        error_dist = 'S'
469100070622     c                   if        rec_allocato = 'S'
469200070622     c                   eval      $ndcalloc = *on
469300070622     c                   endif
469400070622     c                   eval      *in28 = *on
469500070622     c                   leavesr
469600070622     c                   endif
469700070622      * distinta non trovata
469800070622     c                   if        found_dist = 'N'
469900070622     c                   eval      $ndcerr = *on
470000070622     c                   eval      *in28 = *on
470100070622     c                   leavesr
470200070622     c                   endif
470300070622      * distinta chiusa o annullata
470400070622     c                   if        dstfcf = 'S' or dstatb <> *blanks
470500070622     c                   eval      $ndcchiu = *on
470600070622     c                   eval      *in28 = *on
470700070622     c                   leavesr
470800070622     c                   endif
470900070622      * se tutto OK
471000070622     c                   eval      v1ndc1 = *zeros
471100070704     c                   move      fdgnfv        v1ndc1
471200070704     c                   eval      h1ndc_piu = fdgnfv
471300070720     c                   eval      h1pdr_piu = dstpdr
471400070720     c                   eval      h1fpp_piu = dstfpp
471500070919     c                   eval      h1spi_piu = dstrspi
471600070622
471700070622      * intestazione sf1
471800070622     c                   if        v1sce = '1' and h1ndc_piu > *zeros
471900070622     c                   eval      vc1ndc1 = 'alla_dist.:' +
472000080912     c                             %editc(h1ndc_piu:'Z')
472100070622     c                   eval      vc2dsce = 'ASSEGNA'
472200070622     c                   eval      vc2dess = 'alla Distinta nr.' +
472300070622     c                             %trim(%editc(h1ndc_piu:'Z'))
472400070622     c                   endif
472500070622
472600070621     c                   endsr
472700070719
472800070719      *------------------------------------------------------------------------*
472900070719      *   Gestione F06                                                ?
473000070719      *------------------------------------------------------------------------*
473100070719     c     f06_aggiorna  begsr
473200070731      *
473300070731      * Leggendo sempre gli orm selezionati dal 2°SFL
473400070731      * esegue aggiornamento degli archivi e se ci sono problemi
473500070731      * durante l'aggiornamento dei singoli orm emette 3°SFL con la
473600070731      * lista degli orm non aggiornati avvertendo del mancato aggiornamento.
473700070731
473800070731      * L'utente deve quindi rieseguire l'operazione poichè viene annullata tutta
473900070731      * la transazione con il Rolback.
474000070731
474100070731      * Esegue l'aggiornamento ed il controllo sul 3° SFL
474200070731     c                   eval      $gest = 'S3'
474300070731     c                   eval      $inzs3 = *on
474400070731
474500070719     c                   endsr
474600071119
474700071119      *------------------------------------------------------------------------*
474800071119      *   Gestione F07
474900071119      *------------------------------------------------------------------------*
475000071119     c     f07_proposte  begsr
475100071119
475200071119     c                   clear                   fior20ds
475300071119     c                   eval      i20fgs = v1cfgs
475400071119     c                   eval      i20data = v1cddc
475500071119     c                   call      'FIOR20R'
475600071119     c                   parm                    kpjba
475700071119     c                   parm                    fior20ds
475800071119
475900071119     c                   endsr
476000070621
476100070621      *------------------------------------------------------------------------*
476200070705      *   Calcolo pagina fino a cui deve essere ricaricato il sfl   ?
476300070621      *------------------------------------------------------------------------*
476400070704     c     clcpag        begsr
476500070621
476600070621      * input :
476700070621      * - wsfl = numero dell'ultimo rcd su cui era posizionato il
476800070621      *          cursore
476900070621      * - wpagine= numero rcd per pagina sfl
477000070621      * output :
477100070621      * - wpag = pagina fino a cui deve essere ricaricato il sfl
477200070621
477300070621     c                   eval      pagine = %div(wsfl:wpagine)
477400070621     c                   eval      resto = %rem(wsfl:wpagine)
477500070621     c                   if        resto > *zeros
477600070621     c                   eval      pagine = pagine + 1
477700070621     c                   endif
477800070621     c                   eval      wpag = pagine * wpagine
477900070621
478000070621     c                   endsr
478100070622
478200070622      *------------------------------------------------------------------------*
478300070705      *   Riordina comunque il sfl                                     ?
478400070622      *------------------------------------------------------------------------*
478500070704     c     clear_ordina  begsr
478600070620
478700070622     c                   eval      posizionamento = 'N'
478800070622     c                   eval      posizionato    = 'N'
478900070622     c                   clear                   vc2pos1
479000070622     c                   clear                   vc2pos2
479100070622     c                   clear                   vc2pos3
479200070622     c                   clear                   vc2pos4
479300070622     c                   clear                   vc2pos5
479400070622     c                   clear                   vc2pos6
479500070622     c                   clear                   sav_pos1
479600070622     c                   clear                   sav_pos2
479700070622     c                   clear                   sav_pos3
479800070622     c                   clear                   sav_pos4
479900070622     c                   clear                   sav_pos5
480000070622     c                   clear                   sav_pos6
480100070622
480200070622     c                   endsr
480300070622
480400070622      *------------------------------------------------------------------------*
480500070705      *   Riordina comunque il sfl                                     ?
480600080611      *   se richiamato dal MONITOR deve visionare prima gli ORM con INFO
480700070622      *------------------------------------------------------------------------*
480800070704     c     ordina_sfl2   begsr
480900070705
481000070705      * totale records caricati nel 2°SFL
481100070705     c                   eval      wsfl = 1
481200070705
481300070719      * se richiesto ordinamento sul SFL da operatore
481400070705     c                   setoff                                       010203
481500070705     c                   setoff                                       040506
481600070920     c                   setoff                                       0708
481700070705
481800070705      * inizializza i campi chiave x l'ordinamento. C'è un campo in più non
481900070802      * presente nel subfile -- "Selected"?-- questo è aggiunto al record.
482000070705      * il campo è usato per selezionare i records dando un ordine a quelli
482100070705      * selezionati davanti ai non selezionati.
482200070802     c                   clear                   QLGSCB
482300070802     c                   clear                   QLGSCB00
482400070705
482500070705     c                   select
482600070705
482700080612      *?  Ordinamento per GIRO/Mittente  ?
482800070705     c                   when      wrksort = ord_giromit
482900070705     c                   move      ord_giromit   windsort
483000070705
483100070705      * x giro (illumina il campo) con lo 07 = *on
483200070705     c                   eval      *in07 = *on
483300080611
483400080612      *? se richiamato dal MONITOR ordino per INFO
483500080611     c                   if        pgm_chiamante = 'MONITOR'
483600080612     c                   eval      QLGNBRK    = 3
483700080611
483800080612      * il flag INFO è in posizione (xx) xx Bytes char descending
483900080611     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
484000080611     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
484100131119     c                             %size(vs2vlm)+ %size(vs2ora)
484200080611     c                   eval      QLGSS    = %SIZE(vs2info)
484300080611     c                   eval      QLGDT    = Carattere
484400080611     c                   eval      QLGSO    = Discendente
484500080612     c                   eval      QLGKL(1) = QLGSKL
484600080611
484700080612      * il GIRO è in posizione (1) xx Bytes char ascending
484800080611     c                   eval      QLGSP      = 1
484900080611     c                   eval      QLGSS      = %SIZE(vs2cgi)
485000080611     c                   eval      QLGDT      = Carattere
485100080611     c                   eval      QLGSO      = Ascendente
485200080612     c                   eval      QLGKL(2)   = QLGSKL
485300080611
485400080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
485500080611     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
485600080611     c                   eval      QLGSS    = %SIZE(vs2rsr)
485700080611     c                   eval      QLGDT    = Carattere
485800080611     c                   eval      QLGSO    = Ascendente
485900080612     c                   eval      QLGKL(3) = QLGSKL
486000080611
486100080612      *? non richiamato dal MONITOR
486200080611     c                   else
486300070705
486400070705      * 2 campi chiave.   x GIRO e MITTENTE
486500080612     c                   eval      QLGNBRK    = 2
486600080612
486700080612      * il GIRO è in posizione (1) xx Bytes char ascending
486800070802     c                   eval      QLGSP      = 1
486900070802     c                   eval      QLGSS      = %SIZE(vs2cgi)
487000070802     c                   eval      QLGDT      = Carattere
487100070802     c                   eval      QLGSO      = Ascendente
487200080612     c                   eval      QLGKL(1)   = QLGSKL
487300070705
487400080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
487500070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
487600070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
487700070802     c                   eval      QLGDT    = Carattere
487800070802     c                   eval      QLGSO    = Ascendente
487900080612     c                   eval      QLGKL(2) = QLGSKL
488000080611
488100080611     c                   endif
488200070705
488300080612      *?  Ordinamento per Mittente  ?
488400070705     c                   when      wrksort = ord_mittente
488500070705     c                   move      ord_mittente  windsort
488600080612
488700080612      *? se richiamato dal MONITOR ordino per INFO
488800080612     c                   if        pgm_chiamante = 'MONITOR'
488900080612     c                   eval      QLGNBRK    = 3
489000080612
489100080612      * La Priorità se richiesta ricerca di stringa
489200080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) + %SIZE(vs2rsr) +
489300080612     c                             %SIZE(vs2inr)+ %SIZE(vs2lor)+ %SIZE(vs2pkg) +
489400131119     c                             %SIZE(vs2vlm)+ %size(vs2ora)+ %size(vs2info)
489500080612     c                   eval      QLGSS    = %SIZE(h2subst)
489600080612     c                   eval      QLGDT    = Numerico
489700080612     c                   eval      QLGSO    = Discendente
489800080612     c                   eval      QLGKL(1) = QLGSKL
489900080612
490000080612      * il flag info è in posizione (xx) xx Bytes char descending
490100080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
490200080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
490300131119     c                             %size(vs2vlm)+ %size(vs2ora)
490400080612     c                   eval      QLGSS    = %SIZE(vs2info)
490500080612     c                   eval      QLGDT    = Carattere
490600080612     c                   eval      QLGSO    = Discendente
490700080612     c                   eval      QLGKL(2) = QLGSKL
490800080612
490900080612      * il mittente è in posizione (xx) xx Bytes char ascending
491000080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
491100080612     c                   eval      QLGSS    = %SIZE(vs2rsr)
491200080612     c                   eval      QLGDT    = Carattere
491300080612     c                   eval      QLGSO    = Ascendente
491400080612     c                   eval      QLGKL(3) = QLGSKL
491500080612
491600080612      *? non richiamato dal MONITOR
491700080612     c                   else
491800070705
491900070705      * 2 campi chiave.   x substring e mittente
492000070802     c                   eval      QLGNBRK    = 2
492100070705
492200080612      * La Priorità se richiesta ricerca di stringa
492300070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) + %SIZE(vs2rsr) +
492400080128     c                             %SIZE(vs2inr)+ %SIZE(vs2lor)+ %SIZE(vs2pkg) +
492500131119     c                             %SIZE(vs2vlm)+ %size(vs2ora)+ %size(vs2info)
492600070802     c                   eval      QLGSS    = %SIZE(h2subst)
492700070802     c                   eval      QLGDT    = Numerico
492800070802     c                   eval      QLGSO    = Discendente
492900070802     c                   eval      QLGKL(1) = QLGSKL
493000070705
493100080612      * il mittente è in posizione (xx) xx Bytes char ascending
493200070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
493300070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
493400070802     c                   eval      QLGDT    = Carattere
493500070802     c                   eval      QLGSO    = Ascendente
493600070802     c                   eval      QLGKL(2) = QLGSKL
493700080612
493800080612     c                   endif
493900070705
494000080612      *?  Ordinamento per Località/Mittente  ?
494100070705     c                   when      wrksort = ord_localita
494200070705     c                   move      ord_localita  windsort
494300080612
494400080612      *? se richiamato dal MONITOR ordino per INFO
494500080612     c                   if        pgm_chiamante = 'MONITOR'
494600080612     c                   eval      QLGNBRK    = 4
494700080612
494800080612      * La Priorità se richiesta ricerca di stringa
494900080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) + %SIZE(vs2rsr) +
495000080612     c                             %SIZE(vs2inr)+ %SIZE(vs2lor)+ %SIZE(vs2pkg) +
495100131119     c                             %SIZE(vs2vlm)+ %size(vs2ora)+ %size(vs2info)
495200080612     c                   eval      QLGSS    = %SIZE(h2subst)
495300080612     c                   eval      QLGDT    = Numerico
495400080612     c                   eval      QLGSO    = Discendente
495500080612     c                   eval      QLGKL(1) = QLGSKL
495600080612
495700080612      * il flag info è in posizione (xx) xx Bytes char descending
495800080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
495900080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
496000131119     c                             %size(vs2vlm)+ %size(vs2ora)
496100080612     c                   eval      QLGSS    = %SIZE(vs2info)
496200080612     c                   eval      QLGDT    = Carattere
496300080612     c                   eval      QLGSO    = Discendente
496400080612     c                   eval      QLGKL(2) = QLGSKL
496500080612
496600080612      * La LOCALITA' è in posizione (xx) xx Bytes char ascending
496700080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
496800080612     c                                           %SIZE(vs2rsr) +
496900080612     c                                           %SIZE(vs2inr)
497000080612     c                   eval      QLGSS    = %SIZE(vs2lor)
497100080612     c                   eval      QLGDT    = Carattere
497200080612     c                   eval      QLGSO    = Ascendente
497300080612     c                   eval      QLGKL(3) = QLGSKL
497400080612
497500080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
497600080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
497700080612     c                   eval      QLGSS    = %SIZE(vs2rsr)
497800080612     c                   eval      QLGDT    = Carattere
497900080612     c                   eval      QLGSO    = Ascendente
498000080612     c                   eval      QLGKL(4) = QLGSKL
498100080612
498200080612      *? non richiamato dal MONITOR
498300080612     c                   else
498400070705
498500070705      * 3 campi chiave.   x LOCALITA'/MITTENTE
498600070802     c                   eval      QLGNBRK    = 3
498700070705
498800080612      * La Priorità se richiesta ricerca di stringa
498900070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) + %SIZE(vs2rsr) +
499000080128     c                             %SIZE(vs2inr)+ %SIZE(vs2lor)+ %SIZE(vs2pkg) +
499100131119     c                             %SIZE(vs2vlm)+ %size(vs2ora)+ %size(vs2info)
499200070802     c                   eval      QLGSS    = %SIZE(h2subst)
499300070802     c                   eval      QLGDT    = Numerico
499400070802     c                   eval      QLGSO    = Discendente
499500070802     c                   eval      QLGKL(1) = QLGSKL
499600070705
499700080612      * La LOCALITA' è in posizione (xx) xx Bytes char ascending
499800070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
499900070705     c                                           %SIZE(vs2rsr) +
500000080128     c                                           %SIZE(vs2inr)
500100070802     c                   eval      QLGSS    = %SIZE(vs2lor)
500200070802     c                   eval      QLGDT    = Carattere
500300070802     c                   eval      QLGSO    = Ascendente
500400070802     c                   eval      QLGKL(2) = QLGSKL
500500070705
500600080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
500700070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
500800070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
500900070802     c                   eval      QLGDT    = Carattere
501000070802     c                   eval      QLGSO    = Ascendente
501100070802     c                   eval      QLGKL(3) = QLGSKL
501200080612
501300080612     c                   endif
501400080612
501500080612      *?  Ordinamento per Peso/Mittente  ?
501600070705     c                   when      wrksort = ord_peso
501700070705     c                   move      ord_peso      windsort
501800080612
501900080612      *? se richiamato dal MONITOR ordino per INFO
502000080612     c                   if        pgm_chiamante = 'MONITOR'
502100080612     c                   eval      QLGNBRK    = 3
502200080612
502300080612      * il flag info è in posizione (xx) xx Bytes char descending
502400080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
502500080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
502600131119     c                             %size(vs2vlm)+ %size(vs2ora)
502700080612     c                   eval      QLGSS    = %SIZE(vs2info)
502800080612     c                   eval      QLGDT    = Carattere
502900080612     c                   eval      QLGSO    = Discendente
503000080612     c                   eval      QLGKL(1) = QLGSKL
503100080612
503200080612      * il PESO è in posizione (xx) xx Digits Num descending
503300080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
503400080612     c                                           %SIZE(vs2rsr) +
503500080612     c                                           %SIZE(vs2inr) +
503600080612     c                                           %SIZE(vs2lor)
503700080612     c                   eval      QLGSS    = %SIZE(vs2pkg)
503800080612     c                   eval      QLGDT    = Numerico
503900080612     c                   eval      QLGSO    = Discendente
504000080612     c                   eval      QLGKL(2) = QLGSKL
504100080612
504200080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
504300080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
504400080612     c                   eval      QLGSS    = %SIZE(vs2rsr)
504500080612     c                   eval      QLGDT    = Carattere
504600080612     c                   eval      QLGSO    = Ascendente
504700080612     c                   eval      QLGKL(3) = QLGSKL
504800080612
504900080612      *? non richiamato dal MONITOR
505000080612     c                   else
505100070705
505200070705      * 2 campi chiave.   x PESO/MITTENTE
505300080612     c                   eval      QLGNBRK    = 2
505400070705
505500080612      * il PESO è in posizione (xx) xx Digits Num descending
505600070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
505700070705     c                                           %SIZE(vs2rsr) +
505800080128     c                                           %SIZE(vs2inr) +
505900070705     c                                           %SIZE(vs2lor)
506000070802     c                   eval      QLGSS    = %SIZE(vs2pkg)
506100070802     c                   eval      QLGDT    = Numerico
506200070802     c                   eval      QLGSO    = Discendente
506300080612     c                   eval      QLGKL(1) = QLGSKL
506400070705
506500080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
506600070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
506700070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
506800070802     c                   eval      QLGDT    = Carattere
506900070802     c                   eval      QLGSO    = Ascendente
507000080612     c                   eval      QLGKL(2) = QLGSKL
507100080612
507200080612     c                   endif
507300070705
507400080612      *?  Ordinamento per Volume/Mittente  ?
507500070705     c                   when      wrksort = ord_volume
507600070705     c                   move      ord_volume    windsort
507700080612
507800080612      *? se richiamato dal MONITOR ordino per INFO
507900080612     c                   if        pgm_chiamante = 'MONITOR'
508000080612     c                   eval      QLGNBRK    = 3
508100080612
508200080612      * il flag info è in posizione (xx) xx Bytes char descending
508300080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
508400080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
508500131119     c                             %size(vs2vlm)+ %size(vs2ora)
508600080612     c                   eval      QLGSS    = %SIZE(vs2info)
508700080612     c                   eval      QLGDT    = Carattere
508800080612     c                   eval      QLGSO    = Discendente
508900080612     c                   eval      QLGKL(1) = QLGSKL
509000080612
509100080612      * il VOLUME è in posizione (xx) xx Digits Num descending
509200080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
509300080612     c                                           %SIZE(vs2rsr) +
509400080612     c                                           %SIZE(vs2inr) +
509500080612     c                                           %SIZE(vs2lor) +
509600080612     c                                           %SIZE(vs2pkg)
509700080612     c                   eval      QLGSS    = %SIZE(vs2vlm)
509800080612     c                   eval      QLGDT    = Numerico
509900080612     c                   eval      QLGSO    = Discendente
510000080612     c                   eval      QLGKL(2) = QLGSKL
510100080612
510200080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
510300080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
510400080612     c                   eval      QLGSS    = %SIZE(vs2rsr)
510500080612     c                   eval      QLGDT    = Carattere
510600080612     c                   eval      QLGSO    = Ascendente
510700080612     c                   eval      QLGKL(3) = QLGSKL
510800080612
510900080612      *? non richiamato dal MONITOR
511000080612     c                   else
511100070705
511200070705      * 2 campi chiave.   x VOLUME/MITTENTE
511300080612     c                   eval      QLGNBRK    = 2
511400070705
511500080612      * il VOLUME è in posizione (xx) xx Digits Num descending
511600070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
511700070705     c                                           %SIZE(vs2rsr) +
511800080128     c                                           %SIZE(vs2inr) +
511900070705     c                                           %SIZE(vs2lor) +
512000070705     c                                           %SIZE(vs2pkg)
512100070802     c                   eval      QLGSS    = %SIZE(vs2vlm)
512200070802     c                   eval      QLGDT    = Numerico
512300070802     c                   eval      QLGSO    = Discendente
512400080612     c                   eval      QLGKL(1) = QLGSKL
512500070705
512600080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
512700070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
512800070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
512900070802     c                   eval      QLGDT    = Carattere
513000070802     c                   eval      QLGSO    = Ascendente
513100080612     c                   eval      QLGKL(2) = QLGSKL
513200080612
513300080612     c                   endif
513400080128
513500080612      *?  Ordinamento per Ora/Giro  ?
513600080128     c                   when      wrksort = ord_ora
513700080128     c                   move      ord_ora       windsort
513800080612
513900080612      *? se richiamato dal MONITOR ordino per INFO
514000080612     c                   if        pgm_chiamante = 'MONITOR'
514100080612     c                   eval      QLGNBRK    = 3
514200080612
514300080612      * il flag info è in posizione (xx) xx Bytes char descending
514400080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
514500080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
514600131119     c                             %size(vs2vlm)+ %size(vs2ora)
514700080612     c                   eval      QLGSS    = %SIZE(vs2info)
514800080612     c                   eval      QLGDT    = Carattere
514900080612     c                   eval      QLGSO    = Discendente
515000080612     c                   eval      QLGKL(1) = QLGSKL
515100080612
515200080612      * l' ORA è in posizione (xx) xx digits Num descending
515300080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)  +
515400080612     c                                           %SIZE(vs2rsr) +
515500080612     c                                           %SIZE(vs2inr) +
515600080612     c                                           %SIZE(vs2lor) +
515700080612     c                                           %SIZE(vs2pkg) +
515800080612     c                                           %SIZE(vs2vlm)
515900131119     c                   eval      QLGSS    = %SIZE(vs2ora)
516000080612     c                   eval      QLGDT    = Numerico
516100131119     c                   eval      QLGSO    = Ascendente
516200080612     c                   eval      QLGKL(2) = QLGSKL
516300080612
516400080612      * il GIRO è in posizione (1) xx Bytes char ascending
516500080612     c                   eval      QLGSP   = 1
516600080612     c                   eval      QLGSS    = %SIZE(vs2cgi)
516700080612     c                   eval      QLGDT    = Carattere
516800080612     c                   eval      QLGSO    = Ascendente
516900080612     c                   eval      QLGKL(3) = QLGSKL
517000080612
517100080612      *? non richiamato dal MONITOR
517200080612     c                   else
517300080128
517400080128      * 2 campi chiave.   x ORA/GIRO
517500080612     c                   eval      QLGNBRK    = 2
517600080128
517700080612      * l' ORA è in posizione (xx) xx digits Num descending
517800080128     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)  +
517900080128     c                                           %SIZE(vs2rsr) +
518000080128     c                                           %SIZE(vs2inr) +
518100080128     c                                           %SIZE(vs2lor) +
518200080128     c                                           %SIZE(vs2pkg) +
518300080128     c                                           %SIZE(vs2vlm)
518400131119     c                   eval      QLGSS    = %SIZE(vs2ora)
518500080128     c                   eval      QLGDT    = Numerico
518600131119     c                   eval      QLGSO    = Ascendente
518700080612     c                   eval      QLGKL(1) = QLGSKL
518800080128
518900080612      * il GIRO è in posizione (1) xx Bytes char ascending
519000080128     c                   eval      QLGSP   = 1
519100080128     c                   eval      QLGSS    = %SIZE(vs2cgi)
519200080128     c                   eval      QLGDT    = Carattere
519300080128     c                   eval      QLGSO    = Ascendente
519400080612     c                   eval      QLGKL(2) = QLGSKL
519500080612
519600080612     c                   endif
519700070705
519800070705     c                   endsl
519900070705
520000070705      * Load other sort parameters.
520100070802     c                   eval      QLGLB     = 80 + 16 * MaxKey
520200070802     c                   eval      QLGRL     = %SIZE(sflrcd) - 1
520300070802     c                   eval      QLGRT     = 8
520400070802     c                   eval      QLGOKL    = 80
520500070802     c                   eval      QLGLKE    = 16
520600070802     c                   eval      QLGLSS    = 290
520700070705
520800070705      * Initialize Sort I/O API fields.
520900070802     c                   eval      QLGRL00  = QLGRL
521000070802     c                   eval      QLGRC00  = 1
521100070802     c                   clear                   QUSEI
521200070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
521300070705
521400070705      * First step - Initialize the sort routine.
521500070802     c                   call      'QLGSORT'
521600070802     c                   parm                    QLGSCB
521700070802     c                   parm                    NotUsed
521800070802     c                   parm                    NotUsed
521900070802     c                   parm                    SizeList
522000070802     c                   parm                    ReturnSize
522100070802     c                   parm                    QUSEC
522200070705
522300070705      * Next step - Write records to I/O routine.
522400070802     c                   eval      QLGRT00 = Put
522500070705
522600070705     c                   do        rrnlast       s2nrr_xx
522700070705     c     s2nrr_xx      chain     or60s02
522800070705
522900070705      * solo le righe con Selected = 'Y' sono riordinate,
523000070705      * quindi per fare un ordinamento di tutte le righe
523100070705      * metto 'Y' sempre.
523200070705     c                   eval      selected  = 'Y'
523300070705
523400070802     c                   clear                   QUSEI
523500070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
523600070705
523700070802     c                   call      'QLGSRTIO'
523800070802     c                   parm                    QLGSCB00
523900070802     c                   parm                    SflRcd
524000070802     c                   parm                    NotUsed
524100070802     c                   parm                    SizeList
524200070802     c                   parm                    NotUsed
524300070802     c                   parm                    QUSEC
524400070705
524500070705     c                   enddo
524600070705
524700070705      * Next step - Signal end of input, clear subfile for reload.
524800070802     c                   eval      QLGRT00 = EndPut
524900070802     c                   clear                   QUSEI
525000070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
525100070705
525200070802     c                   call      'QLGSRTIO'
525300070802     c                   parm                    QLGSCB00
525400070802     c                   parm                    SflRcd
525500070802     c                   parm                    NotUsed
525600070802     c                   parm                    SizeList
525700070802     c                   parm                    NotUsed
525800070802     c                   parm                    QUSEC
525900070802      * pulizia SFL 2  ?
526000070705     c                   eval      *in20 = *off
526100070705     c                   eval      *in21 = *off
526200070705      *                  __________________
526300070705     c                   write     or60c02
526400070705      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
526500070705     c                   eval      *in21 = *on
526600070705
526700070802      * Campo di Ordinamento in REVERSE ?
526800070705     c                   if        windsort  > 0
526900070705     c                   move      *on           *in(windsort)
527000070705     c                   endif
527100070705
527200070802      * Campo di Posizionamento  ?
527300070705     c   01              eval      vc2desp = '     Posiz. x GIRO :'
527400070705     c   02              eval      vc2desp = 'Scans. x MITTENTE  :'
527500080128     c   03              eval      vc2desp = '      Posiz. x ORA :'
527600070705     c   04              eval      vc2desp = 'Scans. x  LOCALITA :'
527700070705     c   05              eval      vc2desp = '   Posiz. x VOLUME :'
527800070705     c   06              eval      vc2desp = '     Posiz. x PESO :'
527900070705
528000070705      * Final step - Write the records back to the subfile.
528100070802     c                   eval      QLGRT00 = Get
528200070705
528300070705     c                   do        rrnlast       s2nrr_xx
528400070705     c                   eval      s2nrr = s2nrr_xx
528500070802     c                   clear                   QUSEI
528600070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
528700070802     c                   call      'QLGSRTIO'
528800070802     c                   parm                    QLGSCB00
528900070802     c                   parm                    NotUsed
529000070802     c                   parm                    SflRcd
529100070802     c                   parm                    QLGRL00
529200070802     c                   parm                    NotUsed
529300070802     c                   parm                    QUSEC
529400070705      * SFLnxtCHG
529500070705     c                   if        vs2sce <> *blank
529600070705     c                   eval      *in22 = *on
529700070705     c                   else
529800070705     c                   eval      *in22 = *off
529900070705     c                   endif
530000080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
530100080314     c                   if        vs2comun = *on or vs2pda = *on
530200080314     c                   eval      *in08 = *on
530300080314     c                   else
530400080314     c                   eval      *in08 = *off
530500080314     c                   endif
530600071022      * protezione o meno campo scelta
530700071022     c                   eval      *in09 = vs2prot
530800080723      * imposto se filiale ha PDA
530900080723     c                   eval      *in17 = (pdaorm <> *blanks)
531000070705      *                  __________________
531100070705     c                   write     or60s02
531200070705      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
531300070705     c                   enddo
531400070705
531500070705      *  All'uscita di queste chiamate a routine di sistema il SFL record
531600070705      *   si incrementa stranamente quindi lo reimposto correttamente
531700070705      *    prima di lasciare la routine.
531800070705     c                   eval      s2nrr = rrnlast
531900070705
532000070705     c                   endsr
532100080612
532200080612      *------------------------------------------------------------------------*
532300080612      *   Aggiorno il flag INFO su Fipro
532400080612      *------------------------------------------------------------------------*
532500140211      /free
532600140211       begsr sr_chiuinfo;
532700140211
532800140211       //Solo se doveva essere Visionato:
532900140211       // significa che comunque c'era il flag INFO
533000140211
533100140211
533200140211        if vs2info <> ' ' ;
533300140211         clear fior82ds;
533400140211         c82fgs = wfgs;
533500140211         c82ndc = h1ndc_meno ;
533600140211         c82codaut = h1pdr_meno;
533700140211         c82POE = vs2poe;
533800140211         c82NSR = vs2nsr;
533900140211         c82NOR = vs2nor;
534000140211         c82NRV = vs2nrv;
534100140211         c82aggvis = 'S';
534200140211         kpjbu = fior82ds;
534300140211         fior82r(kpjba);
534400140211         fior82ds = kpjbu;
534500140211        endif;
534600080612
534700140211       endsr;
534800070622
534900140211       // ----------------------------------------------------------------------
535000140211       //?Visualizzo le spunte
535100140211       // ----------------------------------------------------------------------
535200140211       begsr sr_visinfo ;
535300140211
535400140211         clear NOTINFOr;
535500140211           WSPED =  %editc(vs2poe:'X') + '/' + %editc(vs2nsr:'X') + '/' +
535600140211                    %editc(vs2Nor:'X') + '/' + %editc(vs2Nrv:'X');
535700140211
535800140211          dataiso = %date(vs2dao:*iso);
535900140211          dataeur = dataiso;
536000140211          WDEL   = %dec(dataeur);
536100140211
536200140211           WDEST = Vs2RAG;
536300140211            if   vs2INFo <> 'S ';
536400140211                 tbeCOD = 'RAS';
536500140211                 tbeKE1 = vs2INFo;
536600140211                 chain (tbecod:tbeke1) tntbe01l;
536700140211                 clear WDcodInfo;
536800140211                   if  %found(tntbe01l);
536900140211                      WDcodInfo = tbeUNI;
537000140211                   endif;
537100140211                 WCODINFO= %trim(vs2INFo)  + ': ' + WdcodInfo;
537200140211                 WNOTASS = vh2notinfo;
537300140211             endif;
537400140211           exfmt NOTINFOr;
537500140211
537600140211       endsr;
537700140211
537800140211      /end-free
537900070621      *------------------------------------------------------------------------*
538000070705      *   Controllo e decodifica giro
538100070621      *------------------------------------------------------------------------*
538200070704     c     ctrgiro       begsr
538300070620
538400070620     c                   clear                   fidg09ds
538500070620     c                   eval      d09iop0 = '001'
538600070620     c                   eval      d09ifgs = wfgs
538700071011     c                   eval      d09idat = data_oggi
538800070620     c                   eval      d09icgi = savcgi
538900070704     c                   eval      d09itug = 'R'
539000070620     c                   eval      kpjbu = fidg09ds
539100070802     c                   call      'FIDG09R'
539200070620     c                   parm                    kpjba
539300070620     c                   eval      fidg09ds = kpjbu
539400070620     c                   eval      dgrgflo = d09oflo
539500070620
539600070620     c                   endsr
539700070619
539800070620      *------------------------------------------------------------------------*
539900070705      *   Operazioni iniziali      ?
540000070620      *------------------------------------------------------------------------*
540100070619     c     *inzsr        begsr
540200070619
540300070619     c     *entry        plist
540400070619     c                   parm                    kpjba
540500080610
540600080610      * controllo se il programma è richiamato dal MONITOR x il PDA
540700080610     c                   eval      *in16 = *off
540800080610     c                   clear                   pgm_chiamante
540900080610     c                   clear                   fior60ds
541000080610     c                   if        %subst(kpjbu:1:10) <> *blanks
541100080612     c                   eval      fior60ds = kpjbu
541200080610      * se si deve cambiare nome al MONITOR basta modificarlo solo qui
541300080610     c                   if        o60pgm = 'MONITOR'
541400080610     c                   eval      pgm_chiamante = 'MONITOR'
541500080610     c                   eval      salta_video1 = 'S'
541600080610     c                   eval      *in16 = *on
541700080610     c                   else
541800080610     c                   eval      pgm_chiamante = o60pgm
541900080610     c                   endif
542000080610     c                   endif
542100070619
542200070619     c     *dtaara       define    §azute        azuteds
542300070619     c     *dtaara       define    §datiute      ddatiute
542400070619     c                   in(e)     *dtaara
542500070704     c                   if        %error or rsut = *blanks
542600070619     c                   clear                   tibs34ds
542700070619     c                   call      'TIBS34R'
542800070619     c                   parm                    tibs34ds
542900070619     c                   in        *dtaara
543000070619     c                   endif
543100070619
543200070619      * imposto la data del giorno
543300071011     c                   time                    w0140
543400071011     c                   move      w0140         oggi
543500071011     c                   clear                   wlbdat
543600071011     c                   move      w0140         g08dat
543700071011     c                   call      'XSRDA8'
543800071011     c                   parm                    wlbdat
543900071011     c                   z-add     g08inv        data_oggi
544000071011
544100071011      * recupero da tabella DGD i limiti per controllo data distinta
544200071011     c                   clear                   ddgd
544300071011     c                   clear                   tibs02ds
544400071011     c                   eval      t02mod = 'C'
544500071011     c                   eval      t02sif = knsif
544600071011     c                   eval      t02cod = 'DGD'
544700071011     c                   eval      t02ke1 = '1'
544800071011     c                   call      'TIBS02R'
544900071011     c                   parm                    kpjba
545000071011     c                   parm                    tibs02ds
545100071011     c                   eval      ddgd = t02uni
545200070704
545300070704      * gestione videate
545400070704     c                   eval      $gest = 'D1'
545500070704     c                   eval      $inzd1 = *on
545600070619
545700070619     c     kdst          klist
545800070619     c                   kfld                    wnpg
545900070619     c                   kfld                    wnfv
546000070619     c                   kfld                    wfgs
546100070619
546200070619     c     kdst8         klist
546300070619     c                   kfld                    wfgs
546400070619     c                   kfld                    wnpg
546500070619     c                   kfld                    wddc
546600070719
546700070719     c     korm          klist
546800070719     c                   kfld                    v1cpoe
546900070719     c                   kfld                    v1nsr
547000070719     c                   kfld                    v1nor
547100070719     c                   kfld                    v1cnrv
547200070719
547300070719     c     kapd          klist
547400070719     c                   kfld                    apdtip
547500070719     c                   kfld                    dstpdr
547600070720
547700070720     c     kaggiorna     klist
547800070720     c                   kfld                    vs2poe
547900070720     c                   kfld                    vs2nsr
548000070720     c                   kfld                    vs2nor
548100070720     c                   kfld                    vs2nrv
548200080611
548300070719     c                   eval      apdtip = 'A'
548400080610
548500080610     c                   eval      kpjbusav = kpjbu
548600070620
548700070619     c                   endsr
548800161108      /free
548900161108       //--------------------------------------------------------------
549000161108       //?Carico Tabella DFT
549100161108       //--------------------------------------------------------------
549200161108       BEGSR CarTabDFT;
549300161108
549400161108       //?Carico i dati di default
549500161108       //?prima con la filiale gestione
549600161108         clear TIBS02DS;
549700161108         clear dDFT;
549800161108         T02mod = 'C';
549900161108         T02sif = knsif;
550000161108         T02cod = 'DFT';
550100161108         T02ke1 = %editc(wfgs:'X');
550200161108         TNTBE_RicercaControllo (kpjba:tibs02ds);
550300161108         IF  T02err = *blanks;
550400161108           dDFT = T02uni;
550500161108           leavesr;
550600161108         ENDIF;
550700161108       //?se non trovo con la filiale gestione provo con 999 generico
550800161108         clear TIBS02DS;
550900161108         clear dDFT;
551000161108         T02mod = 'C';
551100161108         T02sif = knsif;
551200161108         T02cod = 'DFT';
551300161108         T02ke1 = '999';
551400161108         TNTBE_RicercaControllo (kpjba:tibs02ds);
551500161108         IF  T02err = *blanks;
551600161108           dDFT = T02uni;
551700161108         ENDIF;
551800161108
551900161108       ENDSR;
552000161108
552100161108      /end-free
552200070619
552300070620      *------------------------------------------------------------------------*
552400070619** msg
552500070619Filiale errata                                                                 01
552600070619La Filiale non è in gestione all'utente                                        02
552700070619Data errata                                                                    03
552800070619Non esistono distinte aperte nella data scelta                                 04
552900070619Scelta obbligatoria                                                            05
553000070619Codice Giro non corretto o non assegnabile ad una distinta                     06
553100070620Si sta bloccando l'assegnazione ORM: SI PREGA DI USCIRE dal lavoro!            07
553200070620Distinta ancora in uso da un altro lavoro.                                     08
553300070620Numero Distinta non valido o inesistente                                       09
553400070620Distinta non appartenente alla Data selezionata                                10
553500070620Distinta chiusa o annullata                                                    11
553600070620Scegliere 1 solo giro x effettuare l'interrogazione                            12
553700070621 Occorre selezionare almeno una riga per poter proseguire                      13
553800070621  l'elaborazione.                                                              14
553900070621 Non è stata scelta la DISTINTA per assegnare gli O.R.M.                       15
554000070621   Effettuare una scelta mediante le funzioni di ricerca.                      16
554100070621La Distinta è inesistente o non è stata selezionata                            17
554200070621La Distinta è ancora allocata. Non è quindi possibile selezionarla.            18
554300070622La Distinta è chiusa o annullata                                               19
554400070622Non si può trasferire sulla stessa Distinta                                    20
554500070719ORM errato o non in gestione alla filiale                                      21
554600070719Immettere il giro o l'ORM. Entrambi non è possibile                            22
554700070719Impossibile assegnare ORM già assegnato                                        23
554800071011Data antecedente alla data odierna                                             24
554900071011Data distinta superiore alla data odierna dei gg.ammessi                       25
555000071011Data distinta anteriore o superiore alla data odierna dei gg.ammessi           26
555100080717 Gli ORM sopra elencati NON sono stati aggiornati perchè IN USO da altro       27
555200080717 Lavoro.  CONTROLLARE l'operazione                                             28
555300070720 Durante l'aggiornamento archivi ci sono stati dei problemi. Non eseguito il   29
555400080717 Lavoro.  CONTROLLARE l'operazione                                             30
555500070926         Selezionato un ORM con Sponda Idraulica                               31
555600070926           non prevista in distinta, verificare.                               32
555700070925   Selezionato un ORM già comunicato all'AUT, verificare.                      33
555800080612   Selezionato un ORM già scaricato a PDA, verificare.                         34
555900080612Attenzione: il ritiro ha già esito "RITIRATO" dal PDA, verificare!             35
556000080612Attenzione: il ritiro ha già esito "NON RITIRATO" dal PDA, verificare!         36
556100080612Scelta non valida                                                              37
556200080327Non si può più utilizzare una distinta già quadrata!                           38
556300080611Il valore immesso per il campo non è valido                                    39
