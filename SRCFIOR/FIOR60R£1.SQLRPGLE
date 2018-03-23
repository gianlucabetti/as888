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
013000080610     d**!!!wforzacomun     s              1    inz(*off)
013100070919     d wforzaspi       s              1    inz(*off)
013200070622     d wlen            s              3i 0
013300070620     d wmax            s                   like(vc1rcd)
013400070619     d wndc            s                   like(dstnfv)
013500070619     d wnfv            s                   like(dstnfv)
013600070619     d wnpg            s                   like(dstnpg) inz(4)
013700080611     d wokinfo         s              1n
013800131119     d wopz            s             20    inz('157')
013900140211     d wopz1           s             20    inz('15879')
014000131126     d wopza           s             20    inz('15')
014100140211     d wopzb           s             20    inz('1589')
014200070704     d wpag            s                   like(vc1rcd) inz(1)
014300070704     d wpagine         s                   like(vc1rcd)
014400070622     d wpos            s              3i 0
014500131127     d wrigsfl2        s              3  0 inz(14)
014600070620     d write_riga      s              1    inz
014700070620     d wsfl            s                   like(vc1nrr)
014800071017     d wvlm            s                   like(w1davlm1) inz
014900071011     d w0140           s             14  0
015000070621     d w1sce           s                   like(v1sce)
015100141222     d xx              s              4  0 inz
015200141222     d yy              s              4  0 inz
015300070620     d $efile          s              1    inz(*off)
015400070704     d $esci           s              1    inz(*off)
015500070621     d $fine           s              1    inz(*off)
015600070619     d $fv6            s              6p 0 dim(600)
015700070619     d $gest           s              2    inz
015800070619     d $inzd1          s              1    inz(*off)
015900070620     d $inzs1          s              1    inz(*off)
016000070621     d $inzs2          s              1    inz(*off)
016100070705     d $inzs3          s              1    inz(*off)
016200070704     d $inzw1          s              1    inz(*off)
016300070622     d $ndcalloc       s              1    inz(*off)
016400070622     d $ndcchiu        s              1    inz(*off)
016500070622     d $ndcerr         s              1    inz(*off)
016600070921     d $ormcomun       s              1    inz(*off)
016700080314     d $ormesitono     s              1    inz(*off)
016800080314     d $ormesitook     s              1    inz(*off)
016900080314     d $ormpda         s              1    inz(*off)
017000070801     d $ormsecco       s              1    inz(*off)
017100070919     d $ormspi         s              1    inz(*off)
017200070622     d $piugiri        s              1    inz(*off)
017300070620     d $rcdok          s              1    inz(*off)
017400070621     d $viss2          s              1    inz(*off)
017500131118     d wora            s                   like(vs1ora)
017600131118     d wordina         s               n   inz
017700131118     d waggiorna       s               n   inz
017800131119     d sav_wmax2       s                   like(wmax)
017900131127     d wnowin          s               n   inz
018000140211     d dataiso         s               d   datfmt(*iso)
018100140211     d dataeur         s               d   datfmt(*eur)
018200140211     d WDcodInfo       s             20a
018300140529     d InviaDatiPDA    s               n   inz
018400070619
018500070619      *------------------------------------------------------------------------*
018600070619      *   S C H I E R E
018700070619      *------------------------------------------------------------------------*
018800070921     d msg             s             78    dim(40) ctdata perrcd(1)
018900080612     d opz             s              1    dim(20)
019000070619
019100070619      *------------------------------------------------------------------------*
019200070619      *   D S   I N T E R N E / E S T E R N E
019300070619      *------------------------------------------------------------------------*
019400070720     d dstds           ds
019500070619     d  dst_nrr              397    400b 0
019600070720     d orgds           ds
019700070720     d  org_nrr              397    400b 0
019800070720     d ormds           ds
019900070720     d  orm_nrr              397    400b 0
020000080611     d prods           ds
020100080611     d  pro_nrr              397    400b 0
020200070619
020300070619     d dsfmt           ds           512
020400070619     d  $tasto               369    369
020500070619     d  sflnrr               378    379B 0
020600070705
020700070803     d dsfior07        ds
020800070803     d  dsopoe                        3  0
020900070803     d  dsonor                        7  0
021000070803     d  dsonsr                        2  0
021100070803     d  dsonrv                        2  0
021200070803     d  dsoopz                        1
021300070803     d  dsofgs                        3  0
021400070719
021500070719     d                 ds
021600070719     d  v1cnor                 1      9  0
021700070719     d  v1nsr                  1      2  0
021800070719     d  v1nor                  3      9  0
021900070619
022000070619     d wlbdat          ds                  inz
022100070719     d  g08dat                 1      8  0
022200070719     d  g08ggmm                1      4  0
022300070719     d  g08inv                 9     16  0
022400070719     d  g08err                17     17
022500070719     d  g08tgi                18     22  0
022600071011
022700071011     d wdata8          ds
022800071011     d  data1                  1      8  0
022900071011     d  data2                  9     16  0
023000071011     d  giolav                17     21  0
023100070619
023200070620     d azuteds       e ds                  extname(azute00f)
023300070620     d ddatiute      e ds
023400071011     d ddgd          e ds
023500070619     d dgrgflo       e ds
023600080314     d dorg01        e ds
023700070719     d dorm01        e ds
023800070619     d fidg09ds      e ds
023900070704     d fidg31ds      e ds
024000150625     d fior010ds     e ds
024100071119     d fior20ds      e ds
024200080714     d fior57ds      e ds
024300080610     d fior60ds      e ds
024400070731     d fior61ds      e ds
024500070718     d fior64ds      e ds
024600070731     d fior67ds      e ds
024700140211     d fior82ds      e ds
024800080327     d fior90ds      e ds
024900070619     d fnlv50ds      e ds
025000070622     d fnorgds       e ds                  extname(fnorg00f)
025100070622     d fnormds       e ds                  extname(fnorm00f)
025200071108     d fnorpds       e ds                  extname(fnorp00f)
025300070619     d kpjba         e ds
025400070718     d og147         e ds
025500080610     d og148         e ds
025600070719     d og150         e ds
025700071011     d tibs02ds      e ds
025800070620     d tibs34ds      e ds
025900070717     d trul06ds      e ds
026000070717     d  lin                    1     90  0 dim(30)
026100070620     d trul82ds      e ds
026200140319     d TRULORSDS     e ds
026300140319     d TRULOR2DS     e ds
026400131119     d FIOR73DS      e ds
026500140530
026600140530      // - DS richiamo pgm Preparazione dati PDA
026700140530     d FIOR56DS      e ds                  qualified inz
026800070619
026900070619     d                sds
027000070619     d  vidpgm                 1     10
027100070619
027200070619      *------------------------------------------------------------------------*
027300070619      *   C O S T A N T I
027400070619      *------------------------------------------------------------------------*
027500070621     d digits          c                   '0123456789'
027600131126     d dopz            c                   const('Opz. 1=sel 5=vis.ORM')
027700080612     d dopz1           c                   const('Opz. 1=sel 5=vis.ORM 8=chiudi-
027800080612     d                                      INFO')
027900140211     d dopz2           c                   const('     7=Attribuzione GIRO     -
028000140211     d                                           ')
028100140211     d dopz3           c                   const('     9=Int.Info              -
028200140211     d                                           ')
028300140211     d dopz4           c                   const('     7=Attribuzione GIRO 9=In-
028400140211     d                                     t.Info')
028500070621     d enter           c                   const(X'F1')
028600070619     d f01             c                   const(X'31')
028700070619     d f02             c                   const(X'32')
028800070619     d f03             c                   const(X'33')
028900070619     d f04             c                   const(X'34')
029000070619     d f05             c                   const(X'35')
029100070619     d f06             c                   const(X'36')
029200070619     d f07             c                   const(X'37')
029300070619     d f08             c                   const(X'38')
029400070619     d f09             c                   const(X'39')
029500070619     d f10             c                   const(X'3A')
029600070619     d f11             c                   const(X'3B')
029700070619     d f12             c                   const(X'3C')
029800070619     d f13             c                   const(X'B1')
029900070619     d f14             c                   const(X'B2')
030000070619     d f18             c                   const(X'B6')
030100070620     d fun_1           c                   'F1=Sel.tutto'
030200070621     d fun_6           c                   'F6=Conferma Giro NO DETT. ORM'
030300070621     d fun_enter       c                   'Enter=Con Dettaglio ORM'
030400070622
030500070622      **********************************************************************
030600070622      * Constants
030700070802      * MaxKey - è il max. num. di campi chiave permesso in questo pgm
030800070622      **********************************************************************
030900070622     d ord_giromit     c                   1
031000070622     d ord_mittente    c                   2
031100080128     d ord_ora         c                   3
031200070622     d ord_localita    c                   4
031300070622     d ord_volume      c                   5
031400070622     d ord_peso        c                   6
031500070802      * Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
031600070802     d MaxKey          c                   9
031700070802     d Ascendente      c                   1
031800070802     d Discendente     c                   2
031900070802     d Carattere       c                   6
032000070802     d Put             c                   1
032100070802     d EndPut          c                   2
032200070802     d Get             c                   3
032300070802     d Numerico        c                   8
032400070622      **********************************************************************
032500070622      * Campi utili:
032600070622      *     RRN1       - Numero relativo di record del Subfile
032700070622      *     SizeList   - Dimensione della lista
032800070622      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
032900070622      **********************************************************************
033000070802     d Rrn1            s              5I 0
033100070802     d NotUsed         s             16A
033200070802     d ReturnSize      s              9B 0
033300070802     d SizeList        s              9B 0
033400070802     d RrnLast         s              5I 0
033500070802     d WrkSort         s              1  0 inz(0)
033600070802     d WindSort        s              3I 0 inz(0)
033700070622      **********************************************************************
033800070622      * Data Structures
033900070622      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
034000070622      *     QLGSCB     - The sort request block for the QLGSORT API
034100070622      *     QLGSCB00   - The sort request block for the QLGSRTIO API
034200070622      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
034300070622      *     QUSEC      - Error structure for the QLGSORT API
034400070622      **********************************************************************
034500070622     d sflrcd          ds
034600070622     d  vs2cgi
034700070704     d  vs2rsr
034800080128     d  vs2inr
034900070622     d  vs2lor
035000070622     d  vs2pkg
035100070622     d  vs2vlm
035200131119     d  vs2ora
035300080612     d  vs2info
035400070622     d  h2subst
035500131119     d  vs2orr
035600071008     d  vs2all
035700070719     d  vs2sto
035800070622     d  vs2poe
035900070622     d  vs2nsr
036000070622     d  vs2nor
036100070622     d  vs2nrv
036200070704     d  vs2sce
036300070622     d  vs2dar
036400070719     d  vs2com
036500070719     d  vs2spi
036600070719     d  vs2aut
036700070719     d  vs2ncl
036800070704     d  vs2bnc
036900070920     d  vs2comun
037000071022     d  vs2prot
037100071109     d  vs2prop
037200080314     d  vs2pda
037300080314     d  vs2far
037400080514     d  vs2forza
037500080611     d  vs2nrrpro
037600140211     d  vs2dao
037700140211     d  vs2rag
037800140211     d  vh2notinfo
037900070622     d  selected                      1A
038000131118
038100131118     d sflrcd1         ds
038200131118     d  vs1cgi
038300131118     d  vs1des
038400131118     d  vs1tnor
038500131118     d  vs1tpkg
038600131118     d  vs1tvlm
038700131118     d  vs1ora
038800131118     d  selected1                     1A
038900131118     d  h1indis
039000131118     d  vs1sce
039100070622
039200070622      /COPY QSYSINC/QRPGLESRC,QLGSORT
039300070802     d QLGKL                         16    DIM(MaxKey)
039400070802     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
039500070802     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
039600070802     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
039700070802     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
039800070622
039900070622      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
040000070622      /COPY QSYSINC/QRPGLESRC,QUSEC
040100140211      /copy gaitrasrc/srcprotopr,fior82r
040200131118      //---------------------------------------------------------------
040300131118      //?Definizione procedure utilizzate.
040400131118      //---------------------------------------------------------------
040500131118
040600131118      // - Ricerca orari di servizio
040700140319     d trulorsr        pr                  extpgm('TRULORSR')
040800131118     d  kpjba                              likeds(KPJBA)
040900140319     d  trulorsds                          likeds(TRULORSDS)
041000140319     d  trulor2ds                          likeds(TRULOR2DS)
041100140319     d                                     options (*nopass)
041200140530
041300140530      // - Preparazione dati a PDA
041400140530     d fior56r         pr                  extpgm('FIOR56R')
041500140530     d  kpjba                              likeds(KPJBA)
041600140530     d  fior56ds                           likeds(FIOR56DS)
041700070619
041800070619      *------------------------------------------------------------------------*
041900070619
042000070619     c                   dow        $fine = *off
042100070619
042200070619     c                   select
042300070621
042400070619      * 1° video --> parametri
042500070619     c                   when      $gest = 'D1'
042600070704     c                   exsr      gesd1
042700070621
042800070620      * lista giri se scelta assegnazione e non impostato il giro fra i parametri d'ingresso
042900070620     c                   when      $gest = 'S1'
043000070704     c                   exsr      gess1
043100070621
043200070621      * lista di dettaglio degli ORM x selezione e guida aggiornamento data base
043300070621     c                   when      $gest = 'S2'
043400070704     c                   exsr      gess2
043500070705
043600070720      * finestra dei totali riepilogativa prima dell'aggiornamento dei dati
043700070705     c                   when      $gest = 'W1'
043800070705     c                   exsr      gesw1
043900070619
044000070720      * durante l'aggiornamento dei dati scrivo eventuali errori ORM da visualizzare
044100070720      * su questo ultimo SFL
044200070705     c                   when      $gest = 'S3'
044300070705     c                   exsr      gess3
044400070705
044500070619     c                   endsl
044600070619
044700070619     c                   enddo
044800080610
044900080610     c                   eval      kpjbu = kpjbusav
045000070619
045100070619     c                   eval      *inlr = *on
045200070619
045300070619      *------------------------------------------------------------------------*
045400070619      *   Gestione primo video --> parametri             ?
045500070619      *------------------------------------------------------------------------*
045600070704     c     gesd1         begsr
045700070621
045800070619      * inizializzo il video
045900070619     c                   if        $inzd1 = *on
046000070704     c                   exsr      inzd1
046100070619     c                   eval      $inzd1 = *off
046200070619     c                   endif
046300070619
046400070621     c     video_1       tag
046500080715      * controllo se per la filiale gestione ci sono delle proposte variazioni
046600080715      * ancora da analizzare
046700080715     c                   exsr      sr_controrp
046800070621
046900070620      * emissione video
047000070621     c                   if        salta_video1 = 'N'
047100070619      *                  __________________
047200070619     c                   exfmt     or60d01
047300070619      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
047400071030     c                   clear                   savposnf
047500070621     c                   else
047600070621     c                   eval      salta_video1 = 'N'
047700070621     c                   end
047800070619
047900070619     c                   eval      *in28 = *off
048000070621     c                   clear                   vidmsg
048100070619     c                   eval      sav$gest = 'D1'
048200070621
048300070619      * selezioni
048400070620     c                   select
048500070619      * f3=fine
048600070621     c                   when      $tasto = f03
048700070704     c                   exsr      f03_esci
048800070720     c                   rolbk
048900070719      * f2=interrogazione ORM
049000070719     c                   when      $tasto = f02
049100070719     c                   exsr      f02_intorm
049200070719     c                   goto      video_1
049300071119      * f7=gestione proposte ORM
049400071119     c                   when      $tasto = f07
049500071119     c                   exsr      f07_proposte
049600071119     c                   goto      video_1
049700070619      * f18=cambio filiale
049800070621     c                   when      $tasto = f18
049900070619     c                   eval      *in05 = *on
050000070621     c                   goto      video_1
050100070619
050200070620     c                   other
050300070619      * controllo dati immessi
050400070704     c                   exsr      ctrd1
050500080912      * per pgm richiamto da monitor
050600080912      * la routine ctrd1 la richiamo solo perchè così mi richiama l'interrogazione distinte
050700080912      * però al ritorno non devo interessarmi dei vari errori....tanto i controlli sulla distinta
050800080912      * li ha già fatti il chiamate quindi se richiamato da monitor non devo tornare all'inizio
050900080912      * ma andare avanti
051000080912     c                   if        pgm_chiamante <> 'MONITOR'
051100070621     c   28              goto      video_1
051200080912     c                   endif
051300070621
051400070620     c                   endsl
051500080610
051600080610      * Se richiamato da MONITOR con PDA
051700080610      * e se la ricerca distinta ha chiuso con F3 o F12
051800080610     c                   if        pgm_chiamante = 'MONITOR' and
051900080610     c                             (fdgtfu ='03' or fdgtfu ='12')
052000080612     c                   exsr      f03_esci
052100080610     c                   leavesr
052200080610     c                   endif
052300070620
052400070620      * se non ci sono errori passa alle operazioni da esguire
052500070620     c                   select
052600070620
052700070620     c                   when      v1sce = '1'
052800070801      * emissione sfl con elenco dei giri da assegnare
052900070801      * se non è stato richiesto un ORM
053000070801     c                   if        v1cnor = *zeros
053100070801     c                   eval      $gest ='S1'
053200070801     c                   eval      $inzs1 = *on
053300070801     c                   endif
053400070801      * se richiesto un ORM devo emettere subito il subfile degli ORM
053500070801     c                   if        v1cnor <> *zeros
053600070801     c                   eval      $gest ='S2'
053700070801     c                   eval      $inzs2 = *on
053800070801     c                   eval      $inzw1 = *on
053900070801     c                   eval      $inzs3 = *on
054000070801     c                   eval      $viss2 = *on
054100070801     c                   endif
054200070620
054300070621     c                   when      v1sce = '2' and h1ndc_meno > *zeros
054400070801      * emissione sfl con elenco degli ORM da togliere alla distinta
054500070801     c                   eval      $gest ='S2'
054600070801     c                   eval      $inzs2 = *on
054700070801     c                   eval      $inzw1 = *on
054800070801     c                   eval      $inzs3 = *on
054900070620
055000070621     c                   when      v1sce = '3' and h1ndc_meno > *zeros
055100070801      * emissione sfl con elenco degli ORM da trasferire ad altra distinta
055200070801     c                   eval      $gest ='S2'
055300070801     c                   eval      $inzs2 = *on
055400070801     c                   eval      $inzw1 = *on
055500070801     c                   eval      $inzs3 = *on
055600070719
055700070620     c                   endsl
055800070620
055900070620     c                   endsr
056000070619
056100070719      *------------------------------------------------------------------------*
056200070705      *   Inizializzo primo video --> parametri                  ?
056300070619      *------------------------------------------------------------------------*
056400070704     c     inzd1         begsr
056500070619
056600070619     c                   clear                   h1riga
056700070619     c                   clear                   h1colo
056800070718     c                   clear                   h1cddc
056900070621     c                   clear                   h1ndc_meno
057000080730     c                   clear                   h1pdr_meno
057100070731     c                   clear                   h1fpp_meno
057200070620     c                   clear                   h1ndc_piu
057300070720     c                   clear                   h1pdr_piu
057400070720     c                   clear                   h1fpp_piu
057500070919     c                   clear                   h1spi_piu
057600070719
057700070719     c                   clear                   wcgi
057800070705
057900070705     c                   clear                   v1sce
058000070705     c                   clear                   v1ccgi
058100070705     c                   clear                   v1ndc1
058200070719     c                   clear                   v1cpoe
058300070719     c                   clear                   v1cnor
058400070719     c                   clear                   v1cnrv
058500070705     c                   clear                   v1ndc2
058600070705     c                   clear                   v1ndc3
058700070705     c                   clear                   v1ndc4
058800070801
058900070801     c                   eval      $ormsecco = *off
059000070619
059100070720      * filiale gestione da filiale utente
059200070619     c                   eval      v1cfgs = dutpou
059300070619      * abilito f18
059400070619     c                   eval      *in06 = (dutlpo = '2' or dutlpo = *blanks)
059500070720      * data distinta = data del giorno
059600071011     c                   move      oggi          v1cddc
059700080610
059800080610      * se richiamato dal MONITOR
059900080610     c                   if        pgm_chiamante = 'MONITOR'
060000080610      * imposta l'FGS se passata
060100080610     c                   if        o60fgs > *zeros
060200080610     c                   eval      v1cfgs = o60fgs
060300080610     c                   endif
060400080610      * imposta la DATA se passata
060500080610     c                   if        o60dfv > *zeros
060600080610     c                   eval      g08inv = o60dfv
060700080610     c                   eval      g08err = '3'
060800080610     c                   call      'XSRDA8'
060900080610     c                   parm                    wlbdat
061000080610     c                   eval      v1cddc = g08dat
061100080610     c                   endif
061200080610     c                   endif
061300080610      * aggancio l'organigramma
061400080610     c                   clear                   og148
061500070719     c                   clear                   og150
061600070719     c     v1cfgs        chain     azorg01l
061700070719     c                   if        %found(azorg01l)
061800080610     c                   eval      og148 = orgde8
061900070719     c                   eval      og150 = orgdf0
062000070719     c                   endif
062100080610
062200080610      * imposto se PDA ORM attiva/test
062300080610     c                   eval      *in17 = (§ogpdaorm <> *blanks)
062400080610     c                   eval      pdaorm = §ogpdaorm
062500080610
062600080610      * visualizzo picking messaggerie se filiale abilitata
062700070719     c                   eval      *in10 = (§ogpkm ='S')
062800080610
062900080610      * Se richiamato dal MONITOR
063000080610      * salta tutte le altre impostazioni della precedente chiamata
063100080610     c                   if        pgm_chiamante = 'MONITOR'
063200080610     c                   eval      v1sce = '2'
063300080610     c                   leavesr
063400080610     c                   endif
063500070619
063600070720      * filiale gestione se precedentemente impostata deve rimanere quella
063700070619     c                   if        sav_v1cfgs > 0
063800070619     c                   eval      v1cfgs = sav_v1cfgs
063900070619     c                   endif
064000070619
064100070720      * data distinta se precedentemente impostata deve rimanere quella
064200070619     c                   if        sav_v1cddc > 0
064300070619     c                   eval      v1cddc = sav_v1cddc
064400070619     c                   endif
064500070619
064600070619     c                   endsr
064700070619
064800070619      *------------------------------------------------------------------------*
064900070705      *   Controlli primo video --> parametri
065000070619      *------------------------------------------------------------------------*
065100070704     c     ctrd1         begsr
065200070619
065300070619     c     ini_ctrd1     tag
065400070619
065500070619     c                   clear                   h1riga
065600070619     c                   clear                   h1colo
065700070621     c                   clear                   h1ndc_meno
065800080730     c                   clear                   h1pdr_meno
065900070731     c                   clear                   h1fpp_meno
066000070620     c                   clear                   h1ndc_piu
066100070720     c                   clear                   h1pdr_piu
066200070720     c                   clear                   h1fpp_piu
066300070919     c                   clear                   h1spi_piu
066400070718     c                   clear                   con_giro
066500070718     c                   clear                   totnor_cgi
066600070718     c                   clear                   totpkg_cgi
066700070718     c                   clear                   totvlm_cgi
066800070619     c                   eval      *in28 = *off
066900070801     c                   eval      $ormsecco = *off
067000070619
067100070718     ?* filiale gestione
067200070718     c                   clear                   og147
067300080610     c                   clear                   og148
067400070719     c                   clear                   og150
067500070619     c     v1cfgs        chain     azorg01l
067600070619     c                   if        not %found(azorg01l)
067700070619     c                   eval      vidmsg = msg(01)
067800070619     c                   eval      *in28 = *on
067900070719     c                   eval      h1riga = 04
068000070619     c                   eval      h1colo = 27
068100070619     c                   leavesr
068200070619     c                   endif
068300070718     c                   eval      og147 = orgde7
068400080610     c                   eval      og148 = orgde8
068500070719     c                   eval      og150 = orgdf0
068600070719     c                   if        §ogddao = *blank or §ogddao = *all'0'
068700070719     c                   eval      $fine = *on
068800070719     c                   leavesr
068900070719     c                   endif
069000080610      * imposto se PDA ORM attiva/test
069100080610     c                   eval      *in17 = (§ogpdaorm <> *blanks)
069200080610     c                   eval      pdaorm = §ogpdaorm
069300070719      * picking messaggerie
069400070719     c                   eval      *in10 = (§ogpkm ='S')
069500070718      * data partenza procedura in organigramma
069600070718     c                   move      §ogddao       data_org
069700070718      * controllo se siamo in simulazione
069800071011     c                   if        data_oggi < data_org
069900070718     c                   eval      test_simula = 'S'
070000070718     c                   else
070100070718     c                   clear                   test_simula
070200070718     c                   endif
070300070619
070400070619     c                   clear                   fnlv50ds
070500070619     c                   eval      d50pru = knmus
070600070619     c                   eval      d50fgs = v1cfgs
070700070619     c                   call      'FNLV50R'
070800070619     c                   parm                    fnlv50ds
070900070619     c                   if        d50err <> *blanks
071000070619     c                   eval      vidmsg = msg(02)
071100070619     c                   eval      *in28 = *on
071200070719     c                   eval      h1riga = 04
071300070619     c                   eval      h1colo = 27
071400070619     c                   leavesr
071500070619     c                   endif
071600070717
071700070717      * carico la £6 della filiale in gestione
071800070717     c                   clear                   trul06ds
071900070717     c                   eval      d06cod = '£6'
072000070717     c                   movel     v1cfgs        d06key
072100070717     c                   eval      kpjbu = trul06ds
072200070717     c                   call      'TRUL06R'
072300070717     c                   parm                    kpjba
072400070717     c                   eval      trul06ds = kpjbu
072500070619
072600070619      * salva la filiale
072700070619     c                   eval      wfgs = v1cfgs
072800070619     c                   eval      sav_v1cfgs = v1cfgs
072900070621     c                   eval      *in05 = *off
073000070619
073100070619     ?* data distinta
073200070619     c                   clear                   wddc
073300070619     c                   clear                   wlbdat
073400070719     c                   z-add     v1cddc        g08dat
073500070619     c                   call      'XSRDA8'
073600070619     c                   parm                    wlbdat
073700070719     c                   if        g08err = '1'
073800070621     c                   eval      vidmsg = msg(03)
073900070619     c                   eval      *in28 = *on
074000070719     c                   eval      h1riga = 04
074100070619     c                   eval      h1colo = 27
074200070619     c                   leavesr
074300070619     c                   endif
074400070619
074500070719     c                   eval      v1cddc = g08dat
074600070719     c                   eval      wddc = g08inv
074700070619     c                   eval      sav_v1cddc = v1cddc
074800070718     c                   eval      h1cddc = wddc
074900070619
075000070619      * controlla se nella data scelta ci sono distinte aperte
075100070619     c                   eval      tutte_chiuse = 'S'
075200070619     c     kdst8         setll     fidst08l
075300070619     c                   do        *hival
075400070619     c     kdst8         reade     fidst08l
075500070619     c                   if        %eof(fidst08l)
075600070619     c                   leave
075700070619     c                   endif
075800070619     c                   if        dstfcf <> 'S' and dstatb = ' '
075900070619     c                   clear                   tutte_chiuse
076000070619     c                   leave
076100070619     c                   endif
076200070619     c                   enddo
076300070720      * non ci sono distinte aperte
076400070619     c                   if        tutte_chiuse = 'S'
076500070621     c                   eval      vidmsg = msg(04)
076600070619     c                   eval      *in28 = *on
076700070719     c                   eval      h1riga = 06
076800070619     c                   eval      h1colo = 27
076900070619     c                   leavesr
077000070619     c                   endif
077100071011
077200071011      * utente NO EDP controllo data distinta
077300071011     c                   if        %subst(knmus:1:3) <> 'EDP'
077400071011      * non posso andare indietro rispetto a oggi
077500071011     c                   if        g08inv < data_oggi
077600071011     c                   eval      vidmsg = msg(24)
077700071011     c                   eval      *in28 = *on
077800071011     c                   eval      h1riga = 06
077900071011     c                   eval      h1colo = 27
078000071011     c                   leavesr
078100071011     c                   endif
078200071011      * controllo range fra le date di oggi e quella immessa
078300071011      * non può essere superiore di 3 giorni lavorativi
078400071011     c                   clear                   wdata8
078500071011     c                   eval      data1 = data_oggi
078600071011     c                   eval      data2 = g08inv
078700071011     c                   call      'XSRLAV8'
078800071011     c                   parm                    wdata8
078900071011     c                   if        giolav > §dgddc or
079000071011     c                             giolav > 3
079100071011     c                   eval      vidmsg = msg(25)
079200071011     c                   eval      *in28 = *on
079300071011     c                   eval      h1riga = 06
079400071011     c                   eval      h1colo = 27
079500071011     c                   leavesr
079600071011     c                   endif
079700071011      * controllo che la data della distinta non sia antecedente o
079800071011      * seguente a oggi di oltre 15 giorni
079900071011     c                   move      data_oggi     mmgg
080000071011     c                   movel     mmgg          mese
080100071011      * giorni standard
080200071011     c                   eval      nrgg = §dgdgg
080300071011      * periodo estivo
080400071011     c                   if        mmgg >= §dgdei and
080500071011     c                             mmgg <= §dgdef
080600071011     c                   eval      nrgg = §dgdge
080700071011     c                   endif
080800071011      * periodo invernale
080900071011     c                   if        data_oggi >= §dgdii and
081000071011     c                             data_oggi <= §dgdif
081100071011     c                   eval      nrgg = §dgdgi
081200071011     c                   endif
081300071011      * calcolo la data indietro
081400071011     c                   clear                   wdata8
081500071011     c                   eval      giolav = g08tgi - nrgg
081600071011     c                   call      'XSRGI8'
081700071011     c                   parm                    wdata8
081800071011     c                   eval      wdata = data2
081900071011      * controllo
082000071011     c                   if        wdata > data_oggi
082100071011     c                   eval      vidmsg = msg(26)
082200071011     c                   eval      *in28 = *on
082300071011     c                   eval      h1riga = 06
082400071011     c                   eval      h1colo = 27
082500071011     c                   leavesr
082600071011     c                   endif
082700071011      * calcolo la data in avanti
082800071011     c                   clear                   wdata8
082900071011     c                   eval      giolav = g08tgi + nrgg
083000071011     c                   call      'XSRGI8'
083100071011     c                   parm                    wdata8
083200071011     c                   eval      wdata = data2
083300071011      * controllo
083400071011     c                   if        wdata < data_oggi
083500071011     c                   eval      vidmsg = msg(26)
083600071011     c                   eval      *in28 = *on
083700071011     c                   eval      h1riga = 06
083800071011     c                   eval      h1colo = 27
083900071011     c                   leavesr
084000071011     c                   endif
084100071011     c                   endif
084200070619
084300070619     ?* scelta
084400070619     c                   if        v1sce = ' '
084500070621     c                   eval      vidmsg = msg(05)
084600070619     c                   eval      *in28 = *on
084700071008     c                   eval      h1riga = 13
084800070619     c                   eval      h1colo = 27
084900070619     c                   leavesr
085000070619     c                   endif
085100070619
085200070619      * pulisce i campi che non servono
085300070619     c                   if        v1sce = '1'
085400070619     c                   clear                   v1ndc2
085500070619     c                   clear                   v1ndc3
085600070619     c                   clear                   v1ndc4
085700070619     c                   endif
085800070621     c                   if        v1sce = '2'
085900070619     c                   clear                   v1ccgi
086000070619     c                   clear                   v1ndc1
086100070619     c                   clear                   v1ndc3
086200070619     c                   clear                   v1ndc4
086300070619     c                   endif
086400070621     c                   if        v1sce = '3'
086500070619     c                   clear                   v1ccgi
086600070619     c                   clear                   v1ndc1
086700070619     c                   clear                   v1ndc2
086800070619     c                   endif
086900070619
087000070619     ?* scelta (1) assegna giro alla distinta
087100070619     c                   if        v1sce = '1'
087200070719      * --> non si può richiedere giro e ORM assieme
087300070719     c                   if        v1ccgi <> *blanks and v1cnor <> *zeros
087400070719     c                   eval      *in28 = *on
087500070719     c                   eval      vidmsg = msg(22)
087600071008     c                   eval      h1riga = 15
087700070719     c                   eval      h1colo = 38
087800070719     c                   leavesr
087900070719     c                   endif
088000070619      * --> ricerca giro
088100070619     c                   if        %scan('?':v1ccgi) > *zeros
088200070619     c                   clear                   fidg09ds
088300070619     c                   eval      d09iop0 = 'P01'
088400070619     c                   eval      d09ifgs = v1cfgs
088500071011     c                   eval      d09idat = data_oggi
088600070704     c                   eval      d09itug = 'R'
088700070619     c                   eval      kpjbu = fidg09ds
088800070619     c                   call      'FIDG09R'
088900070619     c                   parm                    kpjba
089000070619     c                   eval      fidg09ds = kpjbu
089100070619     c                   eval      dgrgflo = d09oflo
089200070619      *     imposto il giro scelto
089300070619     c                   eval      v1ccgi = d09ocgi
089400070619     c                   endif
089500070619      *     controllo
089600070619     c                   if        v1ccgi <> *blank
089700070620     c                   eval      savcgi = v1ccgi
089800070704     c                   exsr      ctrgiro
089900070619      *     giro errato o non si può mettere in distinta
090000070619     c                   if        d09oerr = '1' or §grgdst = 'N'
090100070619     c                   eval      *in28 = *on
090200070621     c                   eval      vidmsg = msg(06)
090300071008     c                   eval      h1riga = 15
090400070619     c                   eval      h1colo = 38
090500070619     c                   leavesr
090600070619     c                   endif
090700070619      *     imposto la descrizione del giro
090800070619     c                   if        d09ocgi <> ' '
090900070619      *     salvo il giro
091000070619     c                   eval      wcgi = v1ccgi
091100070619     c                   eval      con_giro = 'S'
091200070619     c                   endif
091300070619     c                   endif
091400070719
091500070719      * --> ORM
091600070719     c                   if        v1cpoe <> *zeros or v1cnor <> *zeros or
091700070719     c                             v1cnrv <> *zeros
091800070719     c     korm          chain     fnorm01l
091900070720      *     deve essere un ORM valido su fnorm
092000070719     c                   if        not %found(fnorm01l)
092100070719     c                   eval      *in28 = *on
092200070719     c                   eval      vidmsg = msg(21)
092300071008     c                   eval      h1riga = 15
092400070801     c                   eval      h1colo = 58
092500070719     c                   leavesr
092600070719     c                   endif
092700070720      *     deve essere un ORM valido su fnorg
092800070802     c     korm          chain(n)  fnorg01l
092900070720     c                   if        not %found(fnorg01l)
093000070720     c                   eval      *in28 = *on
093100070720     c                   eval      vidmsg = msg(21)
093200071008     c                   eval      h1riga = 15
093300070801     c                   eval      h1colo = 58
093400070720     c                   leavesr
093500070720     c                   endif
093600070720      *     deve essere un ORM in gestione alla filiale
093700070719     c     ormpor        lookup    lin                                    30
093800070719     c                   if        not *in30
093900070719     c                   eval      *in28 = *on
094000070719     c                   eval      vidmsg = msg(21)
094100071008     c                   eval      h1riga = 15
094200070801     c                   eval      h1colo = 58
094300070719     c                   leavesr
094400070719     c                   endif
094500070720      *     se simulazione deve avere orgndc = 0
094600070719     c                   if        test_simula = 'S' and orgndc <> *zeros
094700070719     c                   eval      *in28 = *on
094800070719     c                   eval      vidmsg = msg(23)
094900071008     c                   eval      h1riga = 15
095000070801     c                   eval      h1colo = 58
095100070719     c                   leavesr
095200070719     c                   endif
095300070720      *     se filiale già attiva deve avere ormndc = 0
095400070719     c                   if        test_simula <> 'S' and ormndc <> *zeros
095500070719     c                   eval      *in28 = *on
095600070719     c                   eval      vidmsg = msg(23)
095700071008     c                   eval      h1riga = 15
095800070801     c                   eval      h1colo = 58
095900070719     c                   leavesr
096000070719     c                   endif
096100071108     c                   eval      $ormsecco = *on
096200070801      *     controllo se l'ORM è assegnabile
096300070801     c                   exsr      check_orm
096400070801     c                   if        or64asso = 'N'
096500070801     c                   eval      *in28 = *on
096600070801     c                   eval      vidmsg = or64msgo
096700071008     c                   eval      h1riga = 15
096800070801     c                   eval      h1colo = 58
096900070801     c                   leavesr
097000070801     c                   endif
097100070719     c                   endif
097200070619
097300070619      * --> numero distinta
097400070621     c                   if        v1ndc1 <> *blanks
097500070621
097600070619     c                   if        v1ndc1 = *zeros
097700070619     c                   clear                   v1ndc1
097800070619     c                   endif
097900070621      *     ricerca
098000070619     c                   if        %scan('?':v1ndc1) > *zeros
098100070619     c                   clear                   wndc
098200070621     c                   eval      w1sce = v1sce
098300070704     c                   exsr      ric_dist
098400070619     c                   eval      v1ndc1 = *all'0'
098500070704     c                   move      fdgnfv        v1ndc1
098600070619     c                   endif
098700070619     c                   if        v1ndc1 = *zeros
098800070619     c                   clear                   v1ndc1
098900070619     c                   endif
099000070621      *     deve essere numerica
099100070621     c                   if        %check(digits:v1ndc1) > 0
099200070704     c                   eval      vidmsg = msg(09)
099300070621     c                   eval      *in28 = *on
099400080327     c                   eval      h1riga = 16
099500080327     c                   eval      h1colo = 42
099600070621     c                   leavesr
099700070621     c                   endif
099800070621      *     controlla
099900070619     c                   move      v1ndc1        wnfv
100000070619     c                   eval      aggior_dist = 'N'
100100070704     c                   exsr      chain_dist
100200070620     c                   if        error_dist = 'S'
100300070620     c                   if        rec_allocato = 'S'
100400070621     c                   eval      vidmsg = msg(08)
100500070620     c                   endif
100600070620     c                   eval      *in28 = *on
100700080327     c                   eval      h1riga = 16
100800080327     c                   eval      h1colo = 42
100900070620     c                   leavesr
101000070620     c                   endif
101100070621      *     distinta non trovata
101200070620     c                   if        found_dist = 'N'
101300070620     c                   eval      *in28 = *on
101400070621     c                   eval      vidmsg = msg(09)
101500080327     c                   eval      h1riga = 16
101600080327     c                   eval      h1colo = 42
101700070620     c                   leavesr
101800070620     c                   endif
101900070621      *     distinta non nella data
102000070620     c                   if        dstdfv <> wddc
102100070620     c                   eval      *in28 = *on
102200070621     c                   eval      vidmsg = msg(10)
102300080327     c                   eval      h1riga = 16
102400080327     c                   eval      h1colo = 42
102500070620     c                   leavesr
102600070620     c                   endif
102700070621      *     distinta chiusa o annullata
102800070620     c                   if        dstfcf = 'S' or dstatb <> *blanks
102900070620     c                   eval      *in28 = *on
103000070621     c                   eval      vidmsg = msg(11)
103100080327     c                   eval      h1riga = 16
103200080327     c                   eval      h1colo = 42
103300070620     c                   leavesr
103400070620     c                   endif
103500080415      *     controllo esistenza fase 'RCH' o 'RQE'
103600080327     c                   exsr      sr_fase
103700080415      * se trova la fase (una delle due)
103800080415      * non si può più assegnare ORM a questa distinta
103900080415     c                   if        or90ndcok <> *blanks
104000080327     c                   eval      *in28 = *on
104100080327     c                   eval      vidmsg = msg(38)
104200080327     c                   eval      h1riga = 16
104300080327     c                   eval      h1colo = 42
104400080327     c                   leavesr
104500080327     c                   endif
104600070621      *     se tutto ok
104700070620     c                   move      v1ndc1        h1ndc_piu
104800070720     c                   eval      h1pdr_piu = dstpdr
104900070720     c                   eval      h1fpp_piu = dstfpp
105000070919     c                   eval      h1spi_piu = dstrspi
105100070620
105200070620     c                   endif
105300070621     c                   endif
105400070621
105500070621     ?* scelta (2) toglie dalla distinta
105600070621     c                   if        v1sce = '2'
105700070621      * --> numero distinta
105800070621     c                   if        v1ndc2 = *zeros
105900070621     c                   clear                   v1ndc2
106000070621     c                   endif
106100070621      *     ricerca
106200070621     c                   if        %scan('?':v1ndc2) > *zeros
106300070621     c                             or v1ndc2 = *blanks or v1ndc2 = *zeros
106400070621     c                   clear                   wndc
106500070621     c                   eval      w1sce = v1sce
106600070704     c                   exsr      ric_dist
106700070621
106800070621      * se tornando dalla ricerca distinta è stato scelto di fare un trasferimento
106900070621      *  invece di togliere e basta
107000070704     c                   if        fdgopz = '3'
107100070621     c                   eval      v1sce = '3'
107200070621     c                   eval      v1ndc3 = *zeros
107300070704     c                   move      fdgnfv        v1ndc3
107400070621     c                   goto      ini_ctrd1
107500070621     c                   endif
107600070621
107700070621     c                   eval      v1ndc2 = *zeros
107800070704     c                   move      fdgnfv        v1ndc2
107900070621     c                   endif
108000070621      *     deve essere numerica
108100070621     c                   if        %check(digits:v1ndc2) > 0
108200070704     c                   eval      vidmsg = msg(09)
108300070621     c                   eval      *in28 = *on
108400071008     c                   eval      h1riga = 18
108500070621     c                   eval      h1colo = 42
108600070621     c                   leavesr
108700070621     c                   endif
108800070621      *     controlla
108900070621     c                   move      v1ndc2        wnfv
109000070621     c                   eval      aggior_dist = 'N'
109100070704     c                   exsr      chain_dist
109200070621     c                   if        error_dist = 'S'
109300070621     c                   if        rec_allocato = 'S'
109400070621     c                   eval      vidmsg = msg(08)
109500070621     c                   endif
109600070621     c                   eval      *in28 = *on
109700071008     c                   eval      h1riga = 18
109800070621     c                   eval      h1colo = 42
109900070621     c                   leavesr
110000070621     c                   endif
110100070621      *     distinta non trovata
110200070621     c                   if        found_dist = 'N'
110300080912     c                   eval      *in28 = *on
110400070621     c                   eval      vidmsg = msg(09)
110500071008     c                   eval      h1riga = 18
110600070621     c                   eval      h1colo = 42
110700070621     c                   leavesr
110800070621     c                   endif
110900070621      *     distinta non nella data
111000070621     c                   if        dstdfv <> wddc
111100070621     c                   eval      *in28 = *on
111200070621     c                   eval      vidmsg = msg(10)
111300071008     c                   eval      h1riga = 18
111400070621     c                   eval      h1colo = 42
111500070621     c                   leavesr
111600070621     c                   endif
111700070621      *     distinta chiusa o annullata
111800070621     c                   if        dstfcf = 'S' or dstatb <> *blanks
111900070621     c                   eval      *in28 = *on
112000070621     c                   eval      vidmsg = msg(11)
112100071008     c                   eval      h1riga = 18
112200070621     c                   eval      h1colo = 42
112300070621     c                   leavesr
112400070621     c                   endif
112500080415      *     controllo esistenza fase 'RCH' o 'RQE'
112600080327     c                   exsr      sr_fase
112700080415      * se trova la fase (una delle due)
112800080415      * non si può più assegnare ORM a questa distinta
112900080415     c                   if        or90ndcok <> *blanks
113000080327     c                   eval      *in28 = *on
113100080327     c                   eval      vidmsg = msg(38)
113200080327     c                   eval      h1riga = 18
113300080327     c                   eval      h1colo = 42
113400080327     c                   leavesr
113500080327     c                   endif
113600070621      *     se tutto ok
113700070621     c                   move      v1ndc2        h1ndc_meno
113800080730     c                   eval      h1pdr_meno = dstpdr
113900070731     c                   move      dstfpp        h1fpp_meno
114000070622     c                   endif
114100070621
114200070621     ?* scelta (3) trasferisce dalla distinta alla distinta
114300070621     c                   if        v1sce = '3'
114400070621      * --> numero distinta
114500070621     c                   if        v1ndc3 = *zeros
114600070621     c                   clear                   v1ndc3
114700070621     c                   endif
114800070621      *     ricerca
114900070621     c                   if        %scan('?':v1ndc3) > *zeros
115000070621     c                             or v1ndc3 = *blanks or v1ndc3 = *zeros
115100070621     c                   clear                   wndc
115200070621     c                   eval      w1sce = v1sce
115300070704     c                   exsr      ric_dist
115400070621
115500070720      *     se tornando dalla ricerca distinta è stato scelto di fare un togli
115600070720      *     invece di trasferire e basta
115700070704     c                   if        fdgopz = '2'
115800070621     c                   eval      v1sce = '2'
115900070621     c                   eval      v1ndc2 = *zeros
116000070704     c                   move      fdgnfv        v1ndc2
116100070621     c                   goto      ini_ctrd1
116200070621     c                   endif
116300070621
116400070621     c                   eval      v1ndc3 = *zeros
116500070704     c                   move      fdgnfv        v1ndc3
116600070621     c                   endif
116700070621      *     deve essere numerica
116800070621     c                   if        %check(digits:v1ndc3) > 0
116900070704     c                   eval      vidmsg = msg(09)
117000070621     c                   eval      *in28 = *on
117100071008     c                   eval      h1riga = 20
117200070621     c                   eval      h1colo = 42
117300070621     c                   leavesr
117400070621     c                   endif
117500070621      *     controlla
117600070621     c                   move      v1ndc3        wnfv
117700070621     c                   eval      aggior_dist = 'N'
117800070704     c                   exsr      chain_dist
117900070621     c                   if        error_dist = 'S'
118000070621     c                   if        rec_allocato = 'S'
118100070621     c                   eval      vidmsg = msg(08)
118200070621     c                   endif
118300070621     c                   eval      *in28 = *on
118400071008     c                   eval      h1riga = 20
118500070621     c                   eval      h1colo = 42
118600070621     c                   leavesr
118700070621     c                   endif
118800070621      *     distinta non trovata
118900070621     c                   if        found_dist = 'N'
119000070621     c                   eval      *in28 = *on
119100070621     c                   eval      vidmsg = msg(09)
119200071008     c                   eval      h1riga = 20
119300070621     c                   eval      h1colo = 42
119400070621     c                   leavesr
119500070621     c                   endif
119600070621      *     distinta non nella data
119700070621     c                   if        dstdfv <> wddc
119800070621     c                   eval      *in28 = *on
119900070621     c                   eval      vidmsg = msg(10)
120000071008     c                   eval      h1riga = 20
120100070621     c                   eval      h1colo = 42
120200070621     c                   leavesr
120300070621     c                   endif
120400070621      *     distinta chiusa o annullata
120500070621     c                   if        dstfcf = 'S' or dstatb <> *blanks
120600070621     c                   eval      *in28 = *on
120700070621     c                   eval      vidmsg = msg(11)
120800071008     c                   eval      h1riga = 20
120900070621     c                   eval      h1colo = 42
121000070621     c                   leavesr
121100070621     c                   endif
121200080415      *     controllo esistenza fase 'RCH' o 'RQE'
121300080327     c                   exsr      sr_fase
121400080415      * se trova la fase (una delle due)
121500080415      * non si può più assegnare ORM a questa distinta
121600080415     c                   if        or90ndcok <> *blanks
121700080327     c                   eval      *in28 = *on
121800080327     c                   eval      vidmsg = msg(38)
121900080327     c                   eval      h1riga = 20
122000080327     c                   eval      h1colo = 42
122100080327     c                   leavesr
122200080327     c                   endif
122300070621      *     se tutto ok
122400070621     c                   move      v1ndc3        h1ndc_meno
122500080730     c                   eval      h1pdr_meno = dstpdr
122600070731     c                   move      dstfpp        h1fpp_meno
122700070621
122800070621      * --> numero distinta
122900070621     c                   if        v1ndc4 <> *blanks
123000070621
123100070621     c                   if        v1ndc4 = *zeros
123200070621     c                   clear                   v1ndc4
123300070621     c                   endif
123400070621      *     ricerca
123500070621     c                   if        %scan('?':v1ndc4) > *zeros
123600070621     c                   move      v1ndc3        wndc
123700070621     c                   eval      w1sce = '1'
123800070704     c                   exsr      ric_dist
123900070621     c                   clear                   wndc
124000070621     c                   eval      v1ndc4 = *zeros
124100070704     c                   move      fdgnfv        v1ndc4
124200070621     c                   endif
124300070621      *     deve essere numerica
124400070621     c                   if        %check(digits:v1ndc4) > 0
124500070704     c                   eval      vidmsg = msg(09)
124600070621     c                   eval      *in28 = *on
124700071008     c                   eval      h1riga = 20
124800070621     c                   eval      h1colo = 67
124900070621     c                   leavesr
125000070621     c                   endif
125100070621      *     controlla
125200070621     c                   move      v1ndc4        wnfv
125300070621     c                   eval      aggior_dist = 'N'
125400070704     c                   exsr      chain_dist
125500070621     c                   if        error_dist = 'S'
125600070621     c                   if        rec_allocato = 'S'
125700070621     c                   eval      vidmsg = msg(08)
125800070621     c                   endif
125900070621     c                   eval      *in28 = *on
126000071008     c                   eval      h1riga = 20
126100070621     c                   eval      h1colo = 67
126200070621     c                   leavesr
126300070621     c                   endif
126400070621      *     distinta non trovata
126500070621     c                   if        found_dist = 'N'
126600070621     c                   eval      *in28 = *on
126700070621     c                   eval      vidmsg = msg(09)
126800071008     c                   eval      h1riga = 20
126900070621     c                   eval      h1colo = 67
127000070621     c                   leavesr
127100070621     c                   endif
127200070621      *     distinta non nella data
127300070621     c                   if        dstdfv <> wddc
127400070621     c                   eval      *in28 = *on
127500070621     c                   eval      vidmsg = msg(10)
127600071008     c                   eval      h1riga = 20
127700070621     c                   eval      h1colo = 67
127800070621     c                   leavesr
127900070621     c                   endif
128000070621      *     distinta chiusa o annullata
128100070621     c                   if        dstfcf = 'S' or dstatb <> *blanks
128200070621     c                   eval      *in28 = *on
128300070621     c                   eval      vidmsg = msg(11)
128400071008     c                   eval      h1riga = 20
128500070621     c                   eval      h1colo = 67
128600070621     c                   leavesr
128700070621     c                   endif
128800070621      *     no uguale alla distinta alla distinta 'da'
128900070621     c                   if        v1ndc3 = v1ndc4
129000070621     c                   eval      *in28 = *on
129100070621     c                   eval      vidmsg = msg(09)
129200071008     c                   eval      h1riga = 20
129300070621     c                   eval      h1colo = 67
129400070621     c                   leavesr
129500070621     c                   endif
129600080415      *     controllo esistenza fase 'RCH' o 'RQE'
129700080327     c                   exsr      sr_fase
129800080415      * se trova la fase (una delle due)
129900080415      * non si può più assegnare ORM a questa distinta
130000080415     c                   if        or90ndcok <> *blanks
130100080327     c                   eval      *in28 = *on
130200080327     c                   eval      vidmsg = msg(38)
130300080327     c                   eval      h1riga = 20
130400080327     c                   eval      h1colo = 67
130500080327     c                   leavesr
130600080327     c                   endif
130700070621      *     se tutto ok
130800070621     c                   move      v1ndc4        h1ndc_piu
130900070720     c                   eval      h1pdr_piu = dstpdr
131000070720     c                   eval      h1fpp_piu = dstfpp
131100070919     c                   eval      h1spi_piu = dstrspi
131200070621     c                   endif
131300070622     c                   endif
131400070620
131500070620     c                   endsr
131600071108
131700071108      *------------------------------------------------------------------------*
131800071108      *   Controllo se ci sono proposte da analizzare              ?
131900071108      *------------------------------------------------------------------------*
132000071108     c     sr_controrp   begsr
132100071108
132200071108     c                   eval      *in15 = *off
132300071108
132400071108     C/EXEC SQL
132500071108     C+ DECLARE c1 CURSOR FOR SELECT fnorp00f.*
132600071108     C+ FROM fnorp00f
132700071108     C+ WHERE orppor=:v1cfgs and orpfev = ' '
132800071108     C/END-EXEC
132900071108
133000071108     C/EXEC SQL
133100071108     C+ OPEN C1
133200071108     C/END-EXEC
133300071108
133400071108     c                   do        *hival
133500071108
133600071108     C/EXEC SQL
133700071108     C+ FETCH NEXT FROM C1 INTO :fnorpds
133800071108     C/END-EXEC
133900071108
134000071108     c                   if        sqlcod = 100 or sqlcod < 0
134100071108     c                   leave
134200071108     c                   endif
134300071108
134400071108     c                   eval      *in15 = *on
134500071108     c                   leave
134600071108     c                   enddo
134700071108
134800071108     C/EXEC SQL
134900071108     C+ CLOSE C1
135000071108     C/END-EXEC
135100071108
135200071108     c                   endsr
135300070620
135400070620      *------------------------------------------------------------------------*
135500070705      *   Gestione sfl giri  da selezionare                        ?
135600070620      *------------------------------------------------------------------------*
135700070704     c     gess1         begsr
135800070620
135900070620      * inizializzazione videata
136000070620     c                   if        $inzs1 = *on
136100070704     c                   exsr      inzs1
136200070620     c                   eval      $inzs1 = *off
136300070620     c                   endif
136400070620
136500070620      * inizializza e imposta piede videata
136600070704     c                   exsr      inzz1
136700070620
136800070620      * emissione piede videata
136900070620      *                  __________________
137000070704     c                   write     or60z01
137100070620      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
137200070620
137300070620     c                   if        wmax = 0
137400070620      * non ci sono record
137500070620      *                  __________________
137600070620     c                   write     or60v01
137700070620      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
137800070620     c                   eval      *in20 = *off
137900070620     c                   else
138000131118      /free
138100131118       //?Ordino il subfile per ORA cut off ma solo quando emetto
138200131118       //?il subfile dopo la prima videata no da f12 dal secondo subfile
138300131118         IF not wordina;
138400131118           exsr ordina_sfl;
138500131118           wordina = *on;
138600131118         ENDIF;
138700131118      /end-free
138800131118
138900070718     c                   eval      *in20 = *on
139000070718     c                   eval      sav_wsfl =  wsfl
139100070718     c                   eval      sav_wpag =  wpag
139200070620
139300070620     c                   if        wsfl > *zeros
139400070620     c                   eval      vc1rcd = wsfl
139500070620     c                   else
139600070620     c                   if        wpag > *zeros
139700070620     c                   eval      vc1rcd = wpag
139800070620     c                   endif
139900070620     c                   endif
140000070620     c                   endif
140100070620
140200070620      * emissione sfl dei giri
140300070620      *                  __________________
140400070620     c                   exfmt     or60c01
140500070620      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
140600070620     c                   eval      *in28 = *off
140700070620     c                   eval      sav$gest = 'S1'
140800070720
140900070620     c                   if        vc1nrr <> *zeros
141000070620     c                   eval      wsfl = vc1nrr
141100070620     c                   endif
141200070620     c                   eval      vc1rcd = sflnrr
141300070620
141400070620      * selezioni
141500070620     c                   select
141600070620
141700070620      * f3=fine
141800070704     c                   when      $tasto = f03
141900070704     c                   exsr      f03_esci
142000070720     c                   rolbk
142100070620      * f5=int.tutte le distinte
142200070621     c                   when      $tasto = f05
142300070704     c                   exsr      f05_int
142400070620      * f12=ritorno
142500070621     c                   when      $tasto = f12
142600070620     c                   eval      $gest = 'D1'
142700070620     c                   eval      $inzd1 = *on
142800070720     c                   rolbk
142900070620      * f1=seleziona tutti
143000070621     c                   when      $tasto = f01
143100070704     c                   exsr      f01_1_sel
143200070620     c                   other
143300070620      * controllo subfile
143400070704     c                   exsr      ctrs1
143500070621     c   28              leavesr
143600070620      * f4=int.distinte stesso giro
143700070720      * ma deve esserne selezionata solo una altrimenti segnala l'errore
143800070621     c                   if        $tasto = f04
143900070620     c                   if        conta_giri = 1
144000070704     c                   exsr      f04_int
144100070720      * se non ha scelto la distinta deve eliminare la selezione sul sfl dei giri
144200070704     c                   if        fdgnfv  = *zeros
144300070621     c     sav_s1nrr     chain     or60s01                            30
144400070621     c                   if        not *in30
144500070620     c                   clear                   vs1sce
144600070620     c                   update    or60s01
144700070620     c                   endif
144800070620     c                   endif
144900070620
145000070620     c                   else
145100070620      * errore
145200070620     c                   eval      *in28 = *on
145300070704     c                   eval      $piugiri = *on
145400070621     c                   leavesr
145500070620     c                   endif
145600070620     c                   endif
145700070621
145800070621      * emette la finestra dei totali Distinta se non ci sono stati errori precedentemente
145900070622      * se non premuto enter ma F6 e immessa una distinta, e se è stato ripulito il messaggio
146000070621      * degli errori precedenti se il sfl è carico.
146100070704     c                   if        $tasto = f06 and $tasto <> enter and
146200070621     c                             msgz = *blanks and wmax > *zeros
146300070621     c                   eval      $inzs2 = *on
146400070621     c                   eval      $gest = 'S2'
146500070621     c                   eval      $viss2 = *off
146600070621     c                   leavesr
146700070621     c                   endif
146800070620
146900070620      * se premuto "enter" è stato richiesto il dettaglio di tutti gli ORM
147000070621      * relativi ai giri selezionati per selezione ORM.
147100070704     c                   if        $tasto = enter
147200070620     c                   eval      $inzs2 = *on
147300070620     c                   eval      $gest = 'S2'
147400070620     c                   eval      $viss2 = *on
147500070621     c                   endif
147600070620
147700070620     c                   endsl
147800070620
147900070620     c                   endsr
148000070620
148100070620      *------------------------------------------------------------------------*
148200070705      *   Inizializzazione e imposta il piede del sfl                 ?
148300070620      *------------------------------------------------------------------------*
148400070704     c     inzz1         begsr
148500070620
148600070620      * messaggio o tasti funzionali
148700070620     c                   clear                   z1dmsg
148800070620     c                   clear                   msgz
148900070620
149000070620      * errore di selezione di + giri x richiesta interr.stesso giro
149100070622     c                   if        $piugiri = *on
149200070620     c                   eval      msgz = msg(12)
149300070622     c                   eval      $piugiri = *off
149400070620     c                   endif
149500070622      * errore di distinta non trovata
149600070622     c                   if        $ndcerr = *on
149700070621     c                   eval      msgz = msg(17)
149800070622     c                   eval      $ndcerr = *off
149900070621     c                   endif
150000070622      * errore di distinta allocata
150100070622     c                   if        $ndcalloc = *on
150200070621     c                   eval      msgz = msg(18)
150300070622     c                   eval      $ndcalloc = *off
150400070621     c                   endif
150500070622      * errore di distinta chiusa
150600070622     c                   if        $ndcchiu = *on
150700070622     c                   eval      msgz = msg(19)
150800070622     c                   eval      $ndcchiu = *off
150900070622     c                   endif
151000070620
151100070620      * imposta eventuali errori
151200070620     c                   eval      z1dmsg = msgz
151300070620      * tasti funzionali
151400070620     c                   if        msgz = *blank
151500070620     c                   eval      *in28 = *off
151600070620     c                   eval      z1dmsg = fun_1
151700070621     c                   eval      %subst(z1dmsg:20:29) = fun_6
151800070621     c                   eval      %subst(z1dmsg:54:23) = fun_enter
151900070620     c                   endif
152000070620
152100070620     c                   endsr
152200070620
152300070620      *------------------------------------------------------------------------*
152400070705      *   Inizializzazione lista dei giri da selezionare              ?
152500070620      *------------------------------------------------------------------------*
152600070704     c     inzs1         begsr
152700070622
152800070622     c                   eval      $ndcalloc = *off
152900070622     c                   eval      $ndcchiu = *off
153000070622     c                   eval      $ndcerr = *off
153100070622     c                   eval      $piugiri = *off
153200070719     c                   eval      $viss2 = *off
153300131126     c                   eval      wordina = *on
153400070620      * pulizia sfl
153500070620     c                   eval      *in20 = *off
153600070620     c                   eval      *in21 = *off
153700070620      *                  __________________
153800070620     c                   write     or60c01
153900070620      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
154000070620     c                   eval      *in21 = *on
154100070620
154200070620      * righe x pagina del 1° sfl
154300070620     c                   eval      s1pag = rigpag_sfl1
154400070620
154500070620      * caricamento sfl totale
154600070620     c                   clear                   s1nrr
154700070620     c                   eval      vc1rcd = 1
154800070620     c                   clear                   wmax
154900070620     c                   clear                   wsfl
155000070620
155100070620     c                   clear                   vc1ndc1
155200070620
155300070620      * se immessa la distinta  disabilita tasti funzionali di ricerca
155400070620     c                   eval      *in40 = *off
155500070620     c                   if        h1ndc_piu > *zeros
155600070620     c                   eval      vc1ndc1 = 'alla_dist.:' +
155700080912     c                             %editc(h1ndc_piu:'Z')
155800070620     c                   endif
155900070620
156000070718      * preparo la stringa sql per estrarre i dati che servono
156100070718     c                   exsr      sr_sqlorg
156200070718
156300070718     C/EXEC SQL
156400070718     C+ PREPARE S1 FROM :stringasql
156500070718     C/END-EXEC
156600070718
156700070718     C/EXEC SQL
156800070718     C+ DECLARE A1 CURSOR FOR S1
156900070718     C/END-EXEC
157000070718
157100070718     C/EXEC SQL
157200070718     C+ OPEN A1
157300070718     C/END-EXEC
157400070720
157500070704     c                   exsr      rols1
157600070718
157700070718     C/EXEC SQL
157800070718     C+ CLOSE A1
157900070718     C/END-EXEC
158000070620
158100070620     c                   eval      wpag = 1
158200070620     c                   eval      sav_wmax = wmax
158300070620
158400070620     c                   endsr
158500070620
158600070620      *------------------------------------------------------------------------*
158700070705      *   Caricamento pagina lista                                    ?
158800070620      *------------------------------------------------------------------------*
158900070704     c     rols1         begsr
159000070620
159100070620     c                   eval      *in22 = *off
159200070620     c                   clear                   yy
159300070620     c                   eval      s1nrr = wmax
159400070620
159500070620      * caricamento del sfl x giro
159600070704     c                   exsr      write_s1
159700070620
159800070620     c                   eval      wmax = s1nrr
159900070620
160000070620      * posizionamento al primo rcd della pagina
160100070620     c                   eval      pagine = %div(s1nrr:s1pag)
160200070620     c                   eval      resto = %rem(s1nrr:s1pag)
160300070620     c                   eval      vc1rcd = pagine * s1pag
160400070620     c                   if        resto > *zeros
160500070620     c                   eval      vc1rcd = vc1rcd + 1
160600070620     c                   else
160700070620     c                   eval      vc1rcd = vc1rcd - s1pag
160800070620     c                   eval      vc1rcd = vc1rcd + 1
160900070620     c                   endif
161000070620
161100070620     c                   endsr
161200070620
161300070620      *------------------------------------------------------------------------*
161400070705      *   Scrive x ogni rottura di giro                              ?
161500070620      *------------------------------------------------------------------------*
161600070704     c     write_s1      begsr
161700131118
161800131118     c                   eval      wora = 2359
161900070620
162000070620      * esegue una lettura di posizionamento giro fuori per poi poter totalizzare
162100070718      * x giro
162200070704     c                   exsr      legge_rec
162300070620
162400070620     c                   dow       $efile = *off
162500070620
162600070620      * pulizia totali x giro
162700070620     c                   eval      savorgcgi = orgcgi
162800070620
162900070620      * finchè il giro è lo stesso
163000070620     c                   dow       $efile = *off and orgcgi = savorgcgi
163100070620
163200070620      * somma ai totali del giro
163300070620     c                   add       1             totnor_cgi
163400070620      * pesi volumi colli da ritirare
163500070718     c                   add       orgpkg        totpkg_cgi
163600070718     c                   add       orgvlm        totvlm_cgi
163700131119
163800131115      /free
163900131115       //?Cerco l'ora di CUT OFF più basse per il giro che sto
164000131115       //?caricando
164100131127         //exsr cercaCUTOFF;
164200140319         //IF  OOR2lrnc < wora and OOR2lrnc > 0;
164300140319         //  wora = OOR2lrnc;
164400131127         //ENDIF;
164500131115      /end-free
164600070620
164700070620      * esce da qui con un record di totale x giro
164800070704     c                   exsr      legge_rec
164900070620     c                   enddo
165000070620
165100070620      * scrive la riga di sfl a rottura di giro
165200070620     c                   clear                   or60s01
165300070622      * codice giro
165400070620     c                   eval      vs1cgi = savorgcgi
165500070620     c                   eval      write_riga = 'S'
165600070620      * descrizione giro
165700070620     c                   eval      savcgi = vs1cgi
165800070704     c                   exsr      ctrgiro
165900070620     c                   eval      vs1des = d09odes
166000070620      * se non deve andare in distinta
166100070620     c                   if        §grgdst = 'N'
166200070620     c                   eval      h1indis =  §grgdst
166300070620     c                   endif
166400070620      * totali orm
166500070913      * n.ORM
166600070620     c                   eval      vs1tnor = totnor_cgi
166700070913      * peso
166800070910     c                   if        totpkg_cgi > 999999,9
166900070910     c                   eval      totpkg_cgi = 999999,9
167000070910     c                   endif
167100070620     c                   eval      vs1tpkg = totpkg_cgi
167200070913      * volume
167300070910     c                   if        totvlm_cgi > 99,999
167400070910     c                   eval      totvlm_cgi = 99,990
167500070910     c                   endif
167600070620     c                   eval      vs1tvlm = totvlm_cgi
167700131118
167800131118     c                   eval      vs1ora = wora
167900070620
168000070620      * se selezionato un solo giro viene impostato automaticamente la selezione della
168100070622      * riga con il sflnxtchg acceso
168200070620     c                   if        v1ccgi <> *blank
168300070620     c                   eval      vs1sce = '1'
168400070620     c                   eval      *in22 = *on
168500070620     c                   endif
168600070620
168700070620     c                   if        write_riga = 'S'
168800070620     c                   eval      s1nrr = s1nrr + 1
168900070620     c                   eval      yy = yy +1
169000070704      *                  __________________
169100070620     c                   write     or60s01
169200070704      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
169300070620     c                   endif
169400070718
169500070718     c                   clear                   totnor_cgi
169600070718     c                   clear                   totpkg_cgi
169700070718     c                   clear                   totvlm_cgi
169800131118     c                   eval      wora = 2359
169900070620
170000070620     c                   enddo
170100070620
170200070620     c                   endsr
170300070620
170400070620      *------------------------------------------------------------------------*
170500070705      *   Lettura rcd archivio pilota                                ?
170600070620      *------------------------------------------------------------------------*
170700070704     c     legge_rec     begsr
170800070620
170900070620      * esce per fine file
171000070620     c                   eval      $efile = *off
171100070620      * per rcd ok deve uscire riportando i dati
171200070620     c                   eval      $rcdok = *off
171300070620
171400070620     c                   dou       $efile = *on or $rcdok = *on
171500070718
171600070718     C/EXEC SQL
171700071025     C+ FETCH NEXT FROM A1 INTO :fnorgds, :fnormds
171800070718     C/END-EXEC
171900070718
172000070718      * errore nel sql esco dal pgm
172100070718     c                   if        sqlcod < 0
172200070718     c                   eval      *inlr = *on
172300070718     c                   return
172400070718     c                   endif
172500070622      * fine file
172600070718     c                   if        sqlcod = 100
172700070620     c                   eval      $efile = *on
172800070620     c                   eval      *in23 = $efile
172900070718
173000070620     c                   else
173100070718
173200071022      *  per Assegnare gli ORM deve controllare se l'ORM può essere assegnato
173300071022     c                   if        v1sce = '1'
173400070718     c                   exsr      check_orm
173500070718     c                   if        or64asso <> 'N'
173600070718     c                   eval      $rcdok = *on
173700070718     c                   endif
173800071022     c                   endif
173900071022
174000071022      * Per Togliere o Trasfererire
174100071022     c                   if        v1sce <> '1'
174200071022     c                   eval      $rcdok = *on
174300071022     c                   endif
174400070718
174500070718     c                   endif
174600070718
174700070620     c                   enddo
174800070620
174900070620     c                   endsr
175000131115      /free
175100131115       //--------------------------------------------------------------
175200131115       //?Cerco l'orari di CUT OFF
175300131115       //--------------------------------------------------------------
175400131115       BEGSR cercaCUTOFF;
175500140319
175600140319         clear trulorsds;
175700140319         clear trulor2ds;
175800140319         IOREfil = ORMpor;
175900140319         IOREcap = ORMcar;
176000140319         IOREloc = ORMlor;
176100140319         IOREnar = ORMnar;
176200140319         IOREdta = ORMdar;
176300140319         IOREtser = 'R';
176400140319         trulorsr (kpjba:trulorsds:trulor2ds);
176500140319         IF  OOREerr <> *blanks or OOR2err <> *blanks;
176600140319           leavesr;
176700140319         ENDIF;
176800131115
176900131115       ENDSR;
177000131118
177100131118       //--------------------------------------------------------------
177200131118       //?Ordino il subfile per Ora Cut Off
177300131118       //--------------------------------------------------------------
177400131118       BEGSR Ordina_Sfl;
177500131118
177600131118       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
177700131118       // presente nel subfile -- "Selected1"?-- questo è aggiunto al record.
177800131118       // il campo è usato per selezionare i records dando un ordine a quelli
177900131118       // selezionati davanti ai non selezionati.
178000131118         clear QLGSCB;
178100131118         clear QLGSCB00;
178200131118         rrnlast = S1nrr;
178300131118
178400131118       // 1 campi chiave x Ora
178500131118         QLGNBRK = 1;
178600131118
178700131118       // imposto la posizione della Mittente sul subfile e la lunghezza
178800131118       // l'ordinamento è su un campo alfanumerico e deve essere ascending
178900131118         QLGSP = 1 + %size(VS1cgi) + %size(VS1des) +
179000131118                     %size(VS1tnor) + %size(VS1tpkg) +
179100131118                     %size(VS1tvlm);
179200131118         QLGSS = %size(VS1ora);
179300131118         QLGDT = Numerico;
179400131118         QLGSO = Ascendente;
179500131118         QLGKL(1) = QLGSKL;
179600131118
179700131118       // Load other sort parameters.
179800131118         QLGLB = 80 + 16 * MaxKey;
179900131118         QLGRL = %size(sflrcd1) - 1;
180000131118         QLGRT = 8;
180100131118         QLGOKL = 80;
180200131118         QLGLKE = 16;
180300131118         QLGLSS = 290;
180400131118
180500131118       // Initialize Sort I/O API fields.
180600131118         QLGRL00 = QLGRL;
180700131118         QLGRC00 = 1;
180800131118         clear QUSEI;
180900131118         QUSBPRV = %size(QUSEC);
181000131118
181100131118      /end-free
181200131118      * First step - Initialize the sort routine.
181300131118     c                   call      'QLGSORT'
181400131118     c                   parm                    QLGSCB
181500131118     c                   parm                    NotUsed
181600131118     c                   parm                    NotUsed
181700131118     c                   parm                    SizeList
181800131118     c                   parm                    ReturnSize
181900131118     c                   parm                    QUSEC
182000131118      /free
182100131118
182200131118       // Next step - Write records to I/O routine.
182300131118         QLGRT00 = Put;
182400131118         for xx = 1 to rrnlast;
182500131118          chain xx OR60S01;
182600131118
182700131118       // solo le righe con Selected = 'Y' sono riordinate,
182800131118       // quindi per fare un ordinamento di tutte le righe
182900131118       // metto 'Y' sempre.
183000131118          selected1 = 'Y';
183100131118          clear QUSEI;
183200131118          QUSBPRV = %SIZE(QUSEC);
183300131118
183400131118      /end-free
183500131118     c                   call      'QLGSRTIO'
183600131118     c                   parm                    QLGSCB00
183700131118     c                   parm                    SflRcd1
183800131118     c                   parm                    NotUsed
183900131118     c                   parm                    SizeList
184000131118     c                   parm                    NotUsed
184100131118     c                   parm                    QUSEC
184200131118      /free
184300131118
184400131118         endfor;
184500131118
184600131118       // Next step - Signal end of input, clear subfile for reload.
184700131118         QLGRT00 = EndPut;
184800131118         clear QUSEI;
184900131118         QUSBPRV = %SIZE(QUSEC);
185000131118
185100131118      /end-free
185200131118     c                   call      'QLGSRTIO'
185300131118     c                   parm                    QLGSCB00
185400131118     c                   parm                    SflRcd1
185500131118     c                   parm                    NotUsed
185600131118     c                   parm                    SizeList
185700131118     c                   parm                    NotUsed
185800131118     c                   parm                    QUSEC
185900131118      /free
186000131118
186100131118       // pulizia SFL
186200131118         *in20 = *off;
186300131118         *in21 = *off;
186400131118         write OR60C01;
186500131118         *in21 = *on;
186600131118
186700131118       // Final step - Write the records back to the subfile.
186800131118         QLGRT00 = Get;
186900131118         for xx = 1 to rrnlast;
187000131118          clear QUSEI;
187100131118          QUSBPRV = %size(QUSEC);
187200131118
187300131118      /end-free
187400131118     c                   call      'QLGSRTIO'
187500131118     c                   parm                    QLGSCB00
187600131118     c                   parm                    NotUsed
187700131118     c                   parm                    SflRcd1
187800131118     c                   parm                    QLGRL00
187900131118     c                   parm                    NotUsed
188000131118     c                   parm                    QUSEC
188100131118      /free
188200131118
188300131118          s1nrr = xx;
188400131118          write OR60S01;
188500131118         endfor;
188600131118
188700131118       ENDSR;
188800131118
188900131115      /end-free
189000070621
189100070621      *------------------------------------------------------------------------*
189200070705      *   Controllo opzioni lista                                   ?
189300070621      *------------------------------------------------------------------------*
189400070704     c     ctrs1         begsr
189500070621
189600070621      * pulizia campi totali
189700070621     c                   clear                   totnor_cgi
189800070621     c                   clear                   totpkg_cgi
189900070621     c                   clear                   totvlm_cgi
190000070621
190100070621      * pulizia campi totali
190200070621     c                   clear                   totnor_s1
190300070621     c                   clear                   totpkg_s1
190400070621     c                   clear                   totvlm_s1
190500070621
190600070621     c                   clear                   conta_giri
190700070621     c                   eval      $esci = *off
190800070621     c                   eval      *in28 = *off
190900070621
191000070621      * leggo il sfl solo se ci sono rcd
191100070621     c                   if        wmax > *zeros
191200070621     c                   readc     or60s01                                30
191300070621
191400070621      * esce se fine sfl o errore che richiede l'uscita
191500070621     c                   dow       not *in30 and $esci = *off
191600070621     c                   eval      vc1rcd = s1nrr
191700070621      * ctrl su riga
191800070704     c                   exsr      recs1
191900070621
192000070621      * se rilevato errore o richiesta uscita, attivo sflnxtchg
192100070621     c                   if        *in28 or $esci = *on
192200070720      * disabilito l'eventuale richiesta di reinizializzazione sfl
192300070622      * la ripristinerò a conclusione del ciclo di readc
192400070621     c                   eval      $inzs1 = *off
192500070621     c                   endif
192600070621
192700070622      * attivo sempre il sflnxtchg e aggiorno sfl
192800070621     c                   eval      *in22 = *on
192900070621     c                   update    or60s01
193000070621
193100070621      * leggo prossimo rcd dal sfl se no richiesta uscita ciclo
193200070621     c                   if        $esci = *off
193300070621     c                   readc     or60s01                                30
193400070621
193500070621      * a fine ciclo ripristino il flag richiesta iniz.sfl
193600070621     c                   if        *in30
193700070621      * calcolo pagina a cui deve posizionarsi
193800070704     c                   eval      wpagine = s1pag
193900070704     c                   exsr      clcpag
194000070621     c                   endif
194100070621     c                   endif
194200070621
194300070621     c                   enddo
194400070621
194500070621     c                   endif
194600070621
194700070622      * imposta i totali come se avesse eseguito il 2°sfl
194800070621     c                   eval      totnor_s2 = totnor_s1
194900070621     c                   eval      totpkg_s2 = totpkg_s1
195000070621     c                   eval      totvlm_s2 = totvlm_s1
195100070621
195200070621      * controlli generali sulle scelte fatte
195300070621     c                   if        msgz = *blank
195400070621
195500070621      * se non è stata scelto nemmeno un giro... errore
195600070621     c                   clear                   err001
195700070621     c                   clear                   err002
195800070621     c                   clear                   err003
195900070621
196000070621     c                   if        conta_giri = 0
196100070621     c                   eval      err001 = msg(13)
196200070621     c                   eval      err002 = msg(14)
196300070621     c                   clear                   err003
196400070621     c                   else
196500070720      * se non è stata scelta nemmeno una distinta... errore
196600070621     c                   if        h1ndc_piu = 0
196700070621     c                   if        $tasto <> f04 and $tasto <> f05 and
196800070622     c                             $tasto <> enter
196900070621      * emette messaggio di avviso x scegliere una distinta se non c'è
197000070621     c                   eval      err001 = msg(15)
197100070621     c                   clear                   err002
197200070621     c                   eval      err003 = msg(16)
197300070621     c                   endif
197400070621     c                   endif
197500070621     c                   endif
197600070621
197700070621      * messaggio di avviso
197800070621     c                   if        err001 <> *blank or
197900070621     c                             err002 <> *blank or
198000070621     c                             err003 <> *blank
198100070621     c                   eval      *in28 = *on
198200070621      *                  __________________
198300070621     c                   exfmt     or60wer
198400070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
198500070621     c                   endif
198600070621
198700070621     c                   endif
198800070621
198900070621     c                   endsr
199000070621
199100070621      *------------------------------------------------------------------------*
199200070705      *   Controllo campi i/o riga lista                            ?
199300070621      *------------------------------------------------------------------------*
199400070704     c     recs1         begsr
199500070621
199600070621      * se è stato scelto ed è un giro che non può essere messo
199700070720      * in distinta errore
199800070621     c                   if        vs1sce = '1' and h1indis = 'N'
199900070622     c                   eval      *in28 = *on
200000070621     c                   leavesr
200100070621     c                   endif
200200070621
200300070621      * gestione opzioni
200400070621     c                   if        vs1sce <> *blank
200500070704     c                   exsr      sce_s1
200600070621     c                   endif
200700070621
200800070621     c                   endsr
200900070621
201000070621      *------------------------------------------------------------------------*
201100070705      *   Gestione opzioni lsita                                    ?
201200070621      *------------------------------------------------------------------------*
201300070704     c     sce_s1        begsr
201400070621
201500070720      * totalizza gli ORM/pesi/volumi/colli
201600070621     c                   if        vs1sce = '1'
201700070621     c                   eval      conta_giri = conta_giri + 1
201800070621     c                   eval      sav_sxcgi = vs1cgi
201900070621     c                   eval      sav_s1nrr = s1nrr
202000070621     c                   add       vs1tnor       totnor_s1
202100070621     c                   add       vs1tpkg       totpkg_s1
202200070621     c                   add       vs1tvlm       totvlm_s1
202300070621     c                   endif
202400070621
202500070621     c                   endsr
202600070622
202700070621      *------------------------------------------------------------------------*
202800070705      *   Gestione sfl ORM da selezionare                        ?
202900070621      *------------------------------------------------------------------------*
203000070704     c     gess2         begsr
203100070621
203200070621      * inizializzazione videata
203300070621     c                   if        $inzs2 = *on
203400070704     c                   exsr      inzs2
203500070621     c                   eval      $inzs2 = *off
203600070621     c                   endif
203700070621
203800070621      * se non si sta eseguendo l'assegnazione oppure se siamo in assegnazione
203900070621      * ed è stato richiesto di selezionare gli ORM dei giri selezionati
204000070621     c                   if        $viss2 = *on or v1sce <> '1'
204100070621
204200070621      * inizializza e imposta piede videata
204300070704     c                   exsr      inzz2
204400070621
204500070621      * emissione piede videata
204600070621      *                  __________________
204700070621     c                   write     or60z02
204800070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
204900070621     c                   if        wmax = *zeros
205000070621      * non ci sono records
205100070621      *                  __________________
205200070621     c                   write     or60v01
205300070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
205400070621     c                   eval      *in20 = *off
205500070621     c                   else
205600070621     c                   eval      *in20 = *on
205700070621
205800070621     c                   if        posizionamento <> 'S'
205900080612     c                   if        posizionato =  'S'
206000070621     c                   eval      vc2rcd = vc2rcd_pos
206100070621     c                   else
206200070621     c                   if        wsfl > *zeros
206300070622      * essendo il posizionamento *top non è + corretto impostare direttamente
206400070621      * il record ma occorre ricavare il primo record della pagina.
206500070621     c                   clear                   vc2rcd
206600080303
206700070621     c                   if        wrigsfl2 > *zeros
206800070621     c                   eval      vc2rcd = %div(wsfl:wrigsfl2)
206900080303     c                   eval      resto = %rem(wsfl:wrigsfl2)
207000080303     c                   if        vc2rcd > *zeros
207100070704     c                   eval      vc2rcd = vc2rcd * wrigsfl2
207200080303     c                   if        resto > *zeros
207300080303     c                   eval      vc2rcd = vc2rcd + 1
207400080303     c                   endif
207500080303     c                   else
207600080303     c                   eval      vc2rcd = 1
207700080303     c                   endif
207800080303     c                   endif
207900080303      * attenzione all'incremento di 1 può far schiantare il sfl
208000080303      * poichè può superare il nr.max delle righe presenti.
208100080303      * esempio se wsfl = 16 o un suo multiplo essendo wrigsfl2 = 16
208200080303      * c2rcd alla fine equivale a wsfl ossia 16 ....e se in seguito
208300080303      * si aggiunge 1 al sflrcdnbr andiamo fuori limite max record
208400080303      * caricati e si schianta in emissione del sfl.
208500080303     c                   if        resto = *zeros
208600080303     c                   eval      vc2rcd = vc2rcd - (wrigsfl2 - 1)
208700080303     c                   else
208800080303     c                   if        vc2rcd = *zeros
208900080303     c                   add       1             vc2rcd
209000080303     c                   endif
209100080303     c                   endif
209200070621     c                   else
209300070621     c                   if        wpag > *zeros
209400070704     c                   eval      vc2rcd = wpag
209500070621     c                   endif
209600070621     c                   endif
209700070621     c                   endif
209800070621     c                   endif
209900070621     c                   endif
210000070621
210100070622      *  emissione sfl degli ORM
210200070621      *                  __________________
210300070621     c                   exfmt     or60c02
210400070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
210500070621     c                   eval      *in28 = *off
210600070621     c                   eval      sav$gest = 'S2'
210700070621
210800070621     c                   if        vc2nrr <> *zeros
210900070621     c                   eval      wsfl = vc2nrr
211000070621     c                   endif
211100070621     c                   eval      vc2rcd = sflnrr
211200070621     c                   eval      vc2rcd_pos = sflnrr
211300070621
211400070621     c                   endif
211500070621      * selezioni
211600070621     c                   select
211700070621      * f3=fine
211800070621     c                   when      $tasto = f03
211900070704     c                   exsr      f03_esci
212000070720     c                   rolbk
212100070621
212200070621      * f1=seleziona tutti
212300070704     c                   when      $tasto = f01
212400070704     c                   exsr      f01_2_sel
212500070621
212600070621      * f5=int.tutte le distinte
212700070621     c                   when      $tasto = f05
212800070704     c                   exsr      f05_int
212900070621
213000070621      * f07=ordinamento x peso
213100070621     c                   when      $tasto = f07
213200070621     c                   eval      wrksort = 6
213300070704     c                   exsr      clear_ordina
213400070704     c                   exsr      ordina_sfl2
213500070621
213600070621      * f08=ordinamento x volume
213700070704     c                   when      $tasto = f08
213800070621     c                   eval      wrksort = 5
213900070704     c                   exsr      clear_ordina
214000070704     c                   exsr      ordina_sfl2
214100070621
214200070621      * f10=ordinamento x località
214300070704     c                   when      $tasto = f10
214400070621     c                   eval      wrksort = 4
214500070704     c                   exsr      clear_ordina
214600070704     c                   exsr      ordina_sfl2
214700070621
214800080128      * f11=ordinamento x ora
214900070704     c                   when      $tasto = f11
215000070621     c                   eval      wrksort = 3
215100070704     c                   exsr      clear_ordina
215200070704     c                   exsr      ordina_sfl2
215300070621
215400070621      * f13=ordinamento x mittente
215500070704     c                   when      $tasto = f13
215600070621     c                   eval      wrksort = 2
215700070704     c                   exsr      clear_ordina
215800070704     c                   exsr      ordina_sfl2
215900070621
216000070621      * f14=ordinamento x giro/mittente
216100070704     c                   when      $tasto = f14
216200070621     c                   eval      wrksort = 1
216300070704     c                   exsr      clear_ordina
216400070704     c                   exsr      ordina_sfl2
216500070621
216600070621      * f12=ritorno
216700070704     c                   when      $tasto = f12
216800070719     c                   if        v1sce = '1'
216900070719     c                   if        v1cnor = *zeros
217000070621     c                   eval      $gest = 'S1'
217100070621     c                   eval      wsfl = vc1rcd
217200131118     c                   IF        waggiorna
217300131118     c                   eval      $inzs1 = *on
217400131118     c                   eval      waggiorna = *off
217500131118     c                   clear                   con_giro
217600131118     c                   ENDIF
217700070719     c                   endif
217800070719     c                   if        v1cnor <> *zeros
217900070719     c                   eval      $gest = 'D1'
218000070719     c                   eval      $inzd1 = *on
218100070720     c                   rolbk
218200070719     c                   endif
218300070621     c                   else
218400070720      * se toglie o trasferisce
218500070720      * vado direttamente sull'interrogazione distinte
218600080611      * ma se richiamato dal Monitor e si deve tornare alla scelta
218700080611      * sulla distinta.
218800080611     c                   if        pgm_chiamante = 'MONITOR'
218900080611     c                             and v1sce <> '2'
219000080611     c                   eval      v1sce = '2'
219100080611     c                   end
219200080611      *
219300070621     c                   eval      salva_v1sce = v1sce
219400070621     c                   eval      salta_video1 = 'S'
219500070621     c                   eval      $gest = 'D1'
219600070621     c                   eval      $inzd1 = *off
219700070704     c                   exsr      inzd1
219800070621     c                   eval      v1sce = salva_v1sce
219900070720     c                   rolbk
220000070621     c                   endif
220100070621
220200070621     c                   other
220300070622      * controllo testata
220400070704     c                   exsr      ctrc2
220500070621     c   28              leavesr
220600070622      * controllo subfile
220700070918     c                   exsr      ctrs2
220800070621     c   28              leavesr
220900070621
221000070621      * f4=int.distinte stesso giro
221100070621     c                   if        $tasto = f04
221200070621     c                   if        giri_scelti = 1
221300070704     c                   exsr      f04_int
221400070621     c                   else
221500070621      * errore x + giri con F4 (x un solo giro)
221600070621     c                   eval      *in28 = *on
221700080612     c                   eval      $piugiri = *on
221800070621     c                   leavesr
221900070621     c                   endif
222000070621     c                   endif
222100070621
222200070720      * se NON premuto F9 e immessa una distinta, emette la finestra dei totali Distinta
222300070720      * se non ci sono stati errori precdentemente e se è stato ripulito il messaggio
222400070720      * degli errori precedenti se il sfl è carico.
222500080611      * e non è stata fatta l'opzione di chiudi INFO
222600070621     c                   if        msgz = *blanks and wmax > *zeros
222700131127     c                             and vs2sce <> '5' and vs2sce <> '8'
222800140212     c                             and vs2sce <> '9'
222900070621
223000070704     c                   if        (h1ndc_piu > *zeros and v1sce = '1' and
223100071008     c                              totnor_s2 > *zeros) or
223200070621     c                             (v1sce = '2' and totnor_s2 > *zeros) or
223300070621     c                             (v1sce = '3' and totnor_s2 > *zeros and
223400070621     c                              h1ndc_piu > *zeros)
223500070621     c                   eval      $inzw1 = *on
223600070621     c                   eval      $gest = 'W1'
223700070621     c                   else
223800131127      * emetto il messaggio solo la selezione fatta prevede la richiesta della distinta
223900131127     c                   IF        not wnowin
224000070621      * emette messaggio di Avviso x scegliere una Distinta se non c'è
224100070621      * se si era scelta l'opzione di assegnazione e non si era immessa
224200070621      * una distinta
224300070621     c                   if        totnor_s2 = *zeros
224400070621     c                   eval      err001 = msg(13)
224500070621     c                   eval      err002 = msg(14)
224600070621     c                   clear                   err003
224700071015     c                   eval      $ormcomun = *off
224800080314     c                   eval      $ormesitono = *off
224900080314     c                   eval      $ormesitook = *off
225000080314     c                   eval      $ormpda = *off
225100071012     c                   eval      wforzaspi = *off
225200070621     c                   else
225300070621     c                   eval      err001 = msg(15)
225400070621     c                   clear                   err002
225500070621     c                   eval      err003 = msg(16)
225600070621     c                   endif
225700070621      *                  __________________
225800070621     c                   exfmt     or60wer
225900070621      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
226000071012     c                   leavesr
226100070621     c                   endif
226200070621     c                   endif
226300131127     c                   ENDIF
226400070621
226500070621     c                   endsl
226600070925
226700070925      * se ho scelto una distinta controllo la sponda idraulica
226800070925     c                   if         h1ndc_piu > *zeros and
226900070925     c                             (v1sce = '1' or v1sce = '3') and
227000070925     c                              $ormspi = *on and wforzaspi = *off
227100070925      * se ORM con sponda e distinta senza emetto window con errore
227200070925     c                   eval      err001 = msg(31)
227300070925     c                   eval      err002 = msg(32)
227400070925     c                   clear                   err003
227500070925      *                  __________________
227600070925     c                   exfmt     or60wer
227700070925      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
227800070925     c                   eval       wforzaspi = *on
227900070925     c                   leavesr
228000070925     c                   endif
228100070621
228200070621     c                   endsr
228300070621
228400070621      *------------------------------------------------------------------------*
228500070705      *   Inizializzazione e imposta il piede del sfl               ?
228600070621      *------------------------------------------------------------------------*
228700070704     c     inzz2         begsr
228800070621
228900070720      * pulisco messaggio o tasti funzionali
229000070621     c                   clear                   z2dmsg
229100070621     c                   clear                   msgz
229200070621
229300070622      * errore di selezione di + giri x richiesta interr.stesso giro
229400070622     c                   if        $piugiri = *on
229500070622     c                   eval      msgz = msg(12)
229600070622     c                   eval      $piugiri = *off
229700070622     c                   endif
229800070622      * errore di distinta non trovata
229900070622     c                   if        $ndcerr = *on
230000070622     c                   eval      msgz = msg(17)
230100070622     c                   eval      $ndcerr = *off
230200070622     c                   endif
230300070622      * errore di distinta allocata
230400070622     c                   if        $ndcalloc = *on
230500070622     c                   eval      msgz = msg(18)
230600070622     c                   eval      $ndcalloc = *off
230700070622     c                   endif
230800070622      * errore di distinta chiusa
230900070622     c                   if        $ndcchiu = *on
231000070622     c                   eval      msgz = msg(19)
231100070622     c                   eval      $ndcchiu = *off
231200070622     c                   endif
231300070622      * stessa distinta mette/toglie
231400070720     c                   if        *in41
231500070622     c                   eval      msgz = msg(20)
231600070720     c                   eval      *in41 = *off
231700070622     c                   clear                   h1ndc_piu
231800070720     c                   clear                   h1pdr_piu
231900070720     c                   clear                   h1fpp_piu
232000070919     c                   clear                   h1spi_piu
232100070622     c                   endif
232200080612
232300080612      * se ho il 27 acceso scelta errata
232400080612     c                   if        *in27
232500080612     c                   eval      msgz = msg(37)
232600080612     c                   eval      *in27 = *off
232700080612     c                   endif
232800080612
232900080612      * se ho il 25 acceso (errore sul subfile per ORM già esitato)
233000080612      * imposto il messaggio a video
233100080612     c                   if        *in25
233200080612     c                   select
233300080612      * ORM con esito OK da PDA avviso
233400080612     c                   when      $ormesitook = *on
233500080612     c                   eval      msgz = msg(35)
233600080612     c                   eval      $ormesitook = *off
233700080612      * ORM con esito NON OK da PDA avviso
233800080612     c                   when      $ormesitono = *on
233900080612     c                   eval      msgz = msg(36)
234000080612     c                   eval      $ormesitono = *off
234100080612     c                   endsl
234200080612     c                   eval      *in25 = *off
234300080612     c                   endif
234400080612      * se ho il 26 acceso (errore sul subfile per ORM già comunicato)
234500080612      * imposto il messaggio a video
234600080612     c                   if        *in26
234700080612     c                   select
234800080612      * ORM già comunicato a PDA
234900080612     c                   when      $ormpda = *on
235000080612     c                   eval      msgz = msg(34)
235100080612     c                   eval      $ormpda = *off
235200080612      * ORM già comunicato ad AUT
235300080612     c                   when      $ormcomun = *on
235400080612     c                   eval      msgz = msg(33)
235500080612     c                   eval      $ormcomun = *off
235600080612     c                   endsl
235700080612     c                   eval      *in26 = *off
235800080612     c                   endif
235900070622
236000070621      * imposta eventuali errori
236100070622     c                   eval      z2dmsg = msgz
236200070622
236300070621      * tasti funzionali se non è stato emesso alcun messaggio
236400070919     c                   if        msgz = *blank
236500070622     c                   eval      *in28 = *off
236600070622     c                   endif
236700070622
236800070621      * se è stato scelto l'assegnazione (1) e non è stato immessa la distinta
236900070622     c                   if        v1sce = '1' or v1sce = '3'
237000070622     c                   eval      *in40 = *off
237100070621     c                   else
237200070622     c                   eval      *in40 = *on
237300070720     c                   endif
237400070622
237500070622     c                   endsr
237600070622
237700070622      *------------------------------------------------------------------------*
237800070705      *   Inizializzazione lista ORM da giri o da distinta
237900070622      *------------------------------------------------------------------------*
238000070704     c     inzs2         begsr
238100070622
238200070720     c                   eval      *in41 = *off
238300070622     c                   eval      $ndcalloc = *off
238400070622     c                   eval      $ndcchiu = *off
238500070622     c                   eval      $ndcerr = *off
238600070622     c                   eval      $piugiri = *off
238700070919     c                   eval      $ormspi = *off
238800070919     c                   eval      wforzaspi = *off
238900070921     c                   eval      $ormcomun = *off
239000080314     c                   eval      $ormesitono = *off
239100080314     c                   eval      $ormesitook = *off
239200080314     c                   eval      $ormpda = *off
239300131119     c                   clear                   rrnlast
239400080613
239500080613      * imposto le opzioni del subfile
239600080613     c                   clear                   opz
239700131126     c                   select
239800131126     c                   when      v1sce = '1'
239900080718     c                   if        not *in17
240000080718     c                   eval      v2copz = dopz
240100131126     c                   eval      v2copz2 = dopz2
240200080718     c                   movea     wopz          opz
240300080613     c                   else
240400080718     c                   eval      v2copz = dopz1
240500140211     c                   eval      v2copz2 = dopz4
240600080718     c                   movea     wopz1         opz
240700080613     c                   endif
240800131126     c                   other
240900131126     c                   clear                   v2copz2
241000131126     c                   if        not *in17
241100131126     c                   eval      v2copz = dopz
241200131126     c                   movea     wopza         opz
241300131126     c                   else
241400131126     c                   eval      v2copz = dopz1
241500140211     c                   eval      v2copz2 = dopz3
241600131126     c                   movea     wopzb         opz
241700131126     c                   endif
241800131126     c                   endsl
241900070622
242000070622      * pulizia sfl
242100070622     c                   eval      *in20 = *off
242200070622     c                   eval      *in21 = *off
242300070622      *                  __________________
242400070622     c                   write     or60c02
242500070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
242600070622     c                   eval      *in21 = *on
242700070622
242800070622      * righe x pagina del 2° sfl
242900070622     c                   eval      s2pag = rigpag_sfl2
243000070622
243100070622     c                   eval      wrksort  = ord_giromit
243200070622     c                   eval      windsort = ord_giromit
243300070622
243400070622      * caricamento sfl totale
243500070622     c                   clear                   s2nrr
243600070622     c                   eval      vc2rcd = 1
243700070622     c                   clear                   wmax
243800070622     c                   clear                   wsfl
243900070720
244000070622     c                   clear                   vc2pos1
244100070622     c                   clear                   vc2pos2
244200070622     c                   clear                   vc2pos3
244300070622     c                   clear                   vc2pos4
244400070622     c                   clear                   vc2pos5
244500070622     c                   clear                   vc2pos6
244600070622     c                   clear                   sav_pos1
244700070622     c                   clear                   sav_pos2
244800070622     c                   clear                   sav_pos3
244900070622     c                   clear                   sav_pos4
245000070622     c                   clear                   sav_pos5
245100070622     c                   clear                   sav_pos6
245200131119     c                   clear                   sav_wmax2
245300070720
245400070622      * tipo funzione
245500070622     c                   select
245600070622
245700070622     c                   when      v1sce = '1'
245800070622     c                   eval      vc2dsce = 'ASSEGNA'
245900070622     c                   eval      vc2dess = 'alla Distinta nr.' +
246000070621     c                             %trim(%editc(h1ndc_piu:'Z'))
246100070622
246200070705     c                   when      v1sce = '2'
246300070622     c                   eval      vc2dsce = 'TOGLIE'
246400070622     c                   eval      vc2dess = 'dalla Distinta nr.' +
246500070621     c                             %trim(%editc(h1ndc_meno:'Z'))
246600070622
246700070622     c                   when      v1sce = '3'
246800070622     c                   eval      vc2dsce = 'TRASFERISCE'
246900070622     c                   eval      vc2dess = 'dalla Distinta nr.' +
247000070621     c                             %trim(%editc(h1ndc_meno:'Z'))
247100070622
247200070622     c                   endsl
247300070622
247400070622      * carica il sfl
247500070704     c                   exsr      rols2
247600070622
247700070622     c                   eval      wpag = 1
247800070622
247900070622     c                   endsr
248000070622
248100070622      *------------------------------------------------------------------------*
248200070705      *   Caricamento pagina da sfl dei giri                         ?
248300070622      *------------------------------------------------------------------------*
248400070704     c     rols2         begsr
248500070622
248600070622     c                   eval      *in22 = *off
248700070622     c                   clear                   yy
248800070622     c                   eval      s2nrr = wmax
248900070622
249000070720      * se scelto caricamento x assegnazione
249100070622     c                   if        v1sce = '1'
249200070720      * carica da sfl1 se non richiesto un ORM singolo
249300070719     c                   if        v1cnor = *zeros
249400070704     c                   exsr      car_da_sfl1
249500070719     c                   endif
249600070720      * carica da ORM se richiesto un ORM singolo
249700070719     c                   if        v1cnor <> *zeros
249800070719     c                   exsr      car_da_orm
249900070719     c                   endif
250000070621     c                   else
250100070720      * carica da numero distinta
250200070704     c                   exsr      car_da_ndc
250300070622     c                   endif
250400070622      * fine caricamento
250500070622     c                   eval      wmax = s2nrr
250600070622
250700070720      * esegue l'ordinamento x giro/ORM
250800070704     c                   exsr      clear_ordina
250900070704     c                   exsr      ordina_sfl2
251000070622
251100070622      * posizionamento al 1° rcd della pagina
251200070622     c                   eval      pagine = %div(s2nrr:s2pag)
251300070622     c                   eval      resto = %rem(s2nrr:s2pag)
251400070622     c                   eval      vc2rcd = pagine * s2pag
251500070622     c                   if        resto > *zeros
251600070622     c                   eval      vc2rcd = vc2rcd + 1
251700070622     c                   else
251800070622     c                   eval      vc2rcd = vc2rcd - s2pag
251900070622     c                   eval      vc2rcd = vc2rcd + 1
252000070622     c                   endif
252100131119     c                   eval      sav_wmax2 = wmax
252200070622
252300070622     c                   endsr
252400070622
252500070622      *------------------------------------------------------------------------*
252600070705      *   Carica dal sfl dei giri                                  ?
252700070622      *------------------------------------------------------------------------*
252800070704     c     car_da_sfl1   begsr
252900070622
253000070622      * legge il sfl dei giri dove selezionato con (1) le righe
253100070720      * dei giri
253200070622     c                   readc     or60S01                                30
253300070622     c                   dow       not *in30
253400070622
253500070622      * solo dai giri selezionati
253600070622     c                   if        vs1sce = '1'
253700070718     c                   if        vs1cgi <> *blanks
253800070718     c                   eval      con_giro = 'S'
253900070718     c                   eval      wcgi = vs1cgi
254000070718     c                   else
254100070718     c                   eval      con_giro = 'N'
254200070808     c                   clear                   wcgi
254300070718     c                   endif
254400070718      * preparo la stringa sql per estrarre i dati che servono
254500070718     c                   exsr      sr_sqlorg
254600070718      * apro il cursore comune per lettura da sfl e da distinta
254700070718     c                   exsr      sr_sqlopen
254800070718
254900070720      * leggendo gli ORM
255000070704     c                   exsr      legge_orm
255100070622
255200070622      * leggo dal file per caricare la lista
255300070622     c                   dow       $efile = *off
255400070622
255500070720      * imposto i campi del subfile
255600070720     c                   clear                   or60s02
255700070704     c                   exsr      riga_sfl2
255800070622
255900070622      * se si è scelto di non visualizzare gli ORM comunque li carico
256000070720      * tutti selezionati con sflnxtchg x la routine di aggiornamento
256100070720      * finale
256200070622     c                   if        $viss2 = *off
256300070921     c                             or vs2cgi = fdgcgi and
256400070704     c                             (fdgopz='3' or fdgopz='2')
256500070622     c                   eval      vs2sce = '1'
256600070622     c                   eval      *in22 = *on
256700070622     c                   endif
256800080723      * imposto se filiale ha PDA
256900080723     c                   eval      *in17 = (pdaorm <> *blanks)
257000070921
257100070622     c                   eval      s2nrr = s2nrr + 1
257200070622     c                   eval      rrnlast = s2nrr
257300070622     c                   eval      yy = yy + 1
257400070622      *                  __________________
257500070622     c                   write     or60s02
257600070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
257700070622      * lettura di un record
257800070704     c                   exsr      legge_orm
257900070622     c                   enddo
258000070622
258100070622      * attivo sempre il sflnxtchg e aggiorno sfl dei giri
258200070622     c                   eval      *in22 = *on
258300070622      *                  __________________
258400070622     c                   update    or60s01
258500070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
258600070622     c                   endif
258700080709      * chiudo il cursore
258800080709     c                   exsr      sr_sqlclose
258900070622
259000070622     c                   readc     or60s01                                30
259100070622     c                   enddo
259200070622
259300070622     c                   endsr
259400070719
259500070719      *------------------------------------------------------------------------*
259600070719      *   Carica da numero ORM                                     ?
259700070719      *------------------------------------------------------------------------*
259800070719     c     car_da_orm    begsr
259900070719
260000070720      * preparo la stringa sql per estrarre i dati che servono
260100070719     c                   exsr      sr_sqlorg
260200070719      * apro il cursore comune per lettura da sfl e da distinta
260300070719     c                   exsr      sr_sqlopen
260400070719
260500070720      * leggo gli ORM
260600070719     c                   exsr      legge_orm
260700070719
260800070719      * leggo dal file per caricare la lista
260900070719     c                   dow       $efile = *off
261000070719
261100070720      * imposto i campi del subfile
261200070720     c                   clear                   or60s02
261300070719     c                   exsr      riga_sfl2
261400070719
261500070720      * carico tutti gli ORM selezionati con sflnxtchg
261600070720      * x la routine di aggiornamento finale
261700070921     c                   if        vs2cgi = fdgcgi and
261800070719     c                             (fdgopz='3' or fdgopz='2')
261900070719     c                   eval      vs2sce = '1'
262000070719     c                   eval      *in22 = *on
262100070719     c                   endif
262200080723      * imposto se filiale ha PDA
262300080723     c                   eval      *in17 = (pdaorm <> *blanks)
262400070719
262500070719     c                   eval      s2nrr = s2nrr + 1
262600070719     c                   eval      rrnlast = s2nrr
262700070719     c                   eval      yy = yy +1
262800070719      *                  __________________
262900070719     c                   write     or60s02
263000070719      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
263100070719      * lettura di un record
263200070719     c                   exsr      legge_orm
263300070719     c                   enddo
263400070719
263500070720      * chiudo il cursore
263600070719     c                   exsr      sr_sqlclose
263700070719
263800070719     c                   endsr
263900070622
264000070622      *------------------------------------------------------------------------*
264100070705      *   Carica da numero distinta                                ?
264200070622      *------------------------------------------------------------------------*
264300070704     c     car_da_ndc    begsr
264400070622
264500070622      * se toglie o trasferisce:  caricamento da distinta
264600070622     c                   if        v1sce = '2' or v1sce = '3'
264700070718      * preparo la stringa sql per estrarre i dati che servono
264800070718     c                   exsr      sr_sqlorg
264900070718      * apro il cursore comune per lettura da sfl e da distinta
265000070718     c                   exsr      sr_sqlopen
265100070718     c                   endif
265200070718
265300070720      * leggo gli ORM
265400070704     c                   exsr      legge_orm
265500070622
265600070622      * leggo dal file per caricare la lista
265700070622     c                   dow       $efile = *off
265800070622
265900070720      * imposto i campi del subfile
266000070622     c                   clear                   or60s02
266100070704     c                   exsr      riga_sfl2
266200070622
266300080723      * imposto se filiale ha PDA
266400080723     c                   eval      *in17 = (pdaorm <> *blanks)
266500070622
266600070622     c                   eval      s2nrr = s2nrr + 1
266700070622     c                   eval      rrnlast = s2nrr
266800070622     c                   eval      yy = yy +1
266900070622      *                  __________________
267000070622     c                   write     or60s02
267100070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
267200070622      * lettura di un record
267300070704     c                   exsr      legge_orm
267400070622     c                   enddo
267500070718
267600070720      * chiudo il cursore
267700070718     c                   exsr      sr_sqlclose
267800070622
267900070622     c                   endsr
268000070622
268100070622      *------------------------------------------------------------------------*
268200070705      *   Lettura rcd archivio pilota                                ?
268300070622      *------------------------------------------------------------------------*
268400070704     c     legge_orm     begsr
268500070622
268600070622      * esce per fine file
268700070622     c                   eval      $efile = *off
268800070622      * per rcd ok deve uscire riportando i dati
268900070622     c                   eval      $rcdok = *off
269000070622
269100070622     c                   dou       $efile = *on or $rcdok = *on
269200070622
269300070718     C/EXEC SQL
269400071025     C+ FETCH NEXT FROM A2 INTO  :fnorgds, :fnormds
269500070718     C/END-EXEC
269600070622
269700070718     c                   select
269800070622      * fine file
269900070718     c                   when      sqlcod = 100
270000070718     c                   eval      $efile = *on
270100070718     c                   eval      *in23 = $efile
270200070718      * errori
270300070718     c                   when      sqlcod < 0
270400070718     c                   if        sqlcod <> -913
270500070718     c                   eval      *inlr = *on
270600070718     c                   return
270700070718     c                   endif
270800070718
270900070718     c                   other
271000070720      *  per Assegnare gli ORM deve controllare se l'ORM può essere assegnato
271100070718     c                   if        v1sce = '1'
271200070718     c                   exsr      check_orm
271300070718     c                   if        or64asso <> 'N'
271400070718     c                   eval      $rcdok = *on
271500070718     c                   endif
271600070718     c                   endif
271700070622
271800070621      * Per Togliere o Trasfererire
271900070622     c                   if        v1sce <> '1'
272000070622     c                   eval      $rcdok = *on
272100070622     c                   endif
272200070622
272300070622     c                   endsl
272400070622
272500070622     c                   enddo
272600070622
272700070622     c                   endsr
272800070622
272900070622      *------------------------------------------------------------------------*
273000070705      *   Imposta campi del 2°sfl                                    ?
273100070622      *------------------------------------------------------------------------*
273200070704     c     riga_sfl2     begsr
273300070720
273400070720      * codice giro
273500070720     c                   eval      vs2cgi = orgcgi
273600070720      * chiave ORM
273700070622     c                   eval      vs2poe = ormpoe
273800070622     c                   eval      vs2nsr = ormnsr
273900070622     c                   eval      vs2nor = ormnor
274000070622     c                   eval      vs2nrv = ormnrv
274100070719      * mittente
274200140211     c                   eval      vs2rag = ormrsr
274300070704     c                   eval      vs2rsr = ormrsr
274400070622     c                   eval      vs2inr = orminr
274500070622     c                   eval      vs2lor = ormlor
274600140211      *
274700140211     c                   eval      vs2dao = ormdao
274800070719      * peso e volume da org
274900071109      * devo togliere i decimali dal peso
275000071109     c                   if        orgpkg > *zeros
275100080215     c                   if        orgpkg > 99999
275200080215     c                   eval      vs2pkg = 99999
275300080215     c                   else
275400071109     c                   eval      vs2pkg = %dech(orgpkg:5:0)
275500080215     c                   endif
275600071109     c                   if        vs2pkg = *zeros
275700071109     c                   eval      vs2pkg = 1
275800071109     c                   endif
275900071109     c                   else
276000071109     c                   clear                   vs2pkg
276100071109     c                   endif
276200070718     c                   eval      vs2vlm = orgvlm
276300070719      * priorità
276400070719     c                   z-add     ormsto        vs2sto
276500070719      * colli/bancali
276600070622     c                   eval      vs2ncl = ormncl
276700070704     c                   eval      vs2bnc = ormbnc
276800070719      * data
276900070622     c                   clear                   wlbdat
277000070719     c                   z-add     ormdar        g08inv
277100070719     c                   eval      g08err = '3'
277200070622     c                   call      'XSRDA8'
277300070622     c                   parm                    wlbdat
277400070719     c                   eval      vs2dar = g08ggmm
277500131119
277600131119      /free
277700131119       //?Cerco l'ora di CUT OFF più basse per il giro che sto
277800131119       //?caricando
277900131126         //exsr cercaCUTOFF;
278000140319         //IF  OOR2lrnc < wora and OOR2lrnc > 0;
278100140319         //  vs2ora = OOR2lrnc;
278200131126         //ENDIF;
278300131119      /end-free
278400131119
278500070719      * ora
278600070719     c                   eval      vs2orr = ormorr
278700131126     c                   eval      vs2ora = ormorr
278800070719
278900070719     c                   eval      dorm01 = ormflo
279000070719      * commissionato
279100070921     c                   if        §orcom = 'S'
279200070719     c                   eval      vs2com = §orcom
279300070921     c                   else
279400070921     c                   clear                   vs2com
279500070921     c                   endif
279600070719      * sponda idraulica
279700070921     c                   if        ormspi = 'S'
279800070921     c                   eval      vs2spi = 'Sp'
279900070921     c                   else
280000070921     c                   clear                   vs2spi
280100070921     c                   endif
280200070719      * automezzo
280300070719     c                   select
280400070719     c                   when      ormblc <> *zeros
280500070719     c                   eval      vs2aut = 'BIL'
280600070719     c                   when      ormatt <> *zeros
280700070719     c                   eval      vs2aut = 'AUT'
280800070719     c                   when      ormmtc <> *zeros
280900070719     c                   eval      vs2aut = 'MOT'
281000070719     c                   other
281100070719     c                   clear                   vs2aut
281200070719     c                   endsl
281300071008
281400071008     c                   clear                   vs2all
281500070719      * prepagato
281600070719     c                   if        ormtor = 'P'
281700071008     c                   eval      vs2all = 'Pp'
281800070719     c                   endif
281900071022      * ricevuta di ritiro
282000071008     c                   if        §orsrm = 'S'
282100071009     c                   eval      vs2all = 'RiO'
282200071008     c                   endif
282300070920
282400070920      * controllo se ORM Comunicato
282500070920     c                   if        ormdst <> *zeros
282600070920     c                   eval      vs2comun = *on
282700070920     c                   else
282800070920     c                   eval      vs2comun = *off
282900070920     c                   endif
283000080314
283100080314      * controllo se ORM inviato a PDA
283200080314     c                   eval      dorg01 = orgflo
283300080314     c                   if        §orgpda = 'S'
283400080314     c                   eval      vs2pda = *on
283500080314     c                   else
283600080314     c                   eval      vs2pda = *off
283700080314     c                   endif
283800110523      /free
283900110523       //?Se ORM RC il controllo lo faccio andando su FIPDO
284000110523         IF  ORMnsr > 0;
284100110523           chain (vs2poe:vs2nsr:vs2nor:vs2nrv) FIPDO02T;
284200110523           IF  %found(FIPDO02T);
284300110523             VS2pda = *on;
284400110523           ENDIF;
284500110523         ENDIF;
284600110523      /end-free
284700080314
284800080314      * mi memorizzo la fase dell'ORM
284900080314     c                   eval      vs2far = ormfao
285000080512      * pulisco il campo di forzatura messagggio
285100080514     c                   eval      vs2forza = *off
285200071022
285300071022      * protezione scelta del subfile
285400071022     c                   eval      vs2prot = *off
285500071022      * proteggo sempre se ORM RC
285600071022      * e imposto il campo degli allegati
285700071022     c                   if        ormnsr > *zeros
285800071022     c                   eval      vs2all = 'RC'
285900071022     c                   eval      vs2prot = *on
286000071022     c                   leavesr
286100071022     c                   endif
286200071022      * in base a quanto richiesto in prima videata per gli allegati
286300071022      * se è Toglie o Trasferisce
286400071022     c                   if        v1sce = '2' or v1sce = '3'
286500071022     c                   if        v1all = 'I' and ormtor <> 'P' and
286600071022     c                             §orsrm <> 'S'
286700071022     c                   eval      vs2prot = *on
286800071022     c                   endif
286900071022     c                   if        v1all = 'E' and (ormtor = 'P' or
287000071022     c                             §orsrm = 'S')
287100071022     c                   eval      vs2prot = *on
287200071022     c                   endif
287300071022     c                   endif
287400071109
287500071109      * cerco le proposte da analizzare e quelle eseguite
287600071109     c                   clear                   vs2prop
287700071109     c     kaggiorna     setgt     fnorp01l
287800071112     c                   do        *hival
287900071109     c     kaggiorna     readpe    fnorp01l
288000071109     c                   select
288100071109     c                   when      %eof(fnorp01l)
288200071112     c                   leave
288300071109     c                   when      orpfev = *blanks
288400071109     c                   eval      vs2prop = 'P'
288500071112     c                   leave
288600071112     c                   when      orpfev = 'R'
288700071112     c                   iter
288800071109     c                   when      orpfev = 'E'
288900071109     c                   if        ormndc > *zeros and ormndc = orpndc
289000071109     c                   eval      vs2prop = 'V'
289100071109     c                   endif
289200071112     c                   leave
289300071109     c                   endsl
289400071112     c                   enddo
289500080611
289600080611      * se attivo/test il PDA sulla filiale cerco le INFO da PDA
289700080611     c                   clear                   vs2nrrpro
289800080611     c                   if        pdaorm <> *blanks
289900080611     c                   exsr      sr_infopda
290000080611     c                   endif
290100070622
290200070622     c                   endsr
290300080611
290400080611      *------------------------------------------------------------------------*
290500080611      *   Controlla se presente delle INFO x PDA da visualizzare   ?
290600080611      *------------------------------------------------------------------------*
290700080611     c     sr_infopda    begsr
290800080611
290900140211      /FREE
291000140211           clear fior82ds;
291100140211           clear  vs2info ;
291200140211           clear  vh2notinfo ;
291300140211           c82fgs = wfgs;
291400140211           c82ndc = h1ndc_meno ;
291500140211           c82codaut = h1pdr_meno;
291600140211           c82POE = vs2poe;
291700140211           c82NSR = vs2nsr;
291800140211           c82NOR = vs2nor;
291900140211           c82NRV = vs2nrv;
292000140211           kpjbu = fior82ds;
292100140211           fior82r(kpjba);
292200140211           fior82ds = kpjbu;
292300140211           if c82davisio <> '';
292400140211             vs2info = C82DAVISIO;
292500140211             vh2notinfo = C82NOTASS;
292600140211           endif;
292700140211      /END-FREE
292800080611
292900080611     c                   endsr
293000070622
293100070622      *------------------------------------------------------------------------*
293200070720      *   Controlla testata lista ORM                                ?
293300070622      *------------------------------------------------------------------------*
293400070704     c     ctrc2         begsr
293500070622
293600070622     c                   eval      *in28 = *off
293700070622
293800070622      * se si tratta di trasferimento
293900070720      * non deve essere la stessa distinta da cui trasferisce
294000070622     c                   if        v1sce = '3' and
294100070622     c                             h1ndc_piu = h1ndc_meno
294200070622     c                   eval      *in28 = *on
294300070720     c                   eval      *in41 = *on
294400070622     c                   endif
294500070622
294600070720      * controllo campi di posizionamento
294700070720      * o di ricerca stringa (?) in primo Byte
294800070621     c                   eval      posizionamento = 'N'
294900070621     c                   eval      posizionato    = 'N'
295000070621     c                   eval      cerca_stringa  = 'N'
295100070622     c                   clear                   stringa_20a
295200070622
295300070621     c                   select
295400070621      * giro
295500070622     c                   when      *in01 and sav_pos1 <> vc2pos1
295600070622     c                   eval      sav_pos1 = vc2pos1
295700070621     c                   eval      posizionamento = 'S'
295800070622      * mittente
295900070622     c                   when      *in02 and sav_pos2 <> vc2pos2
296000070622     c                   eval      sav_pos2 = vc2pos2
296100070720      * è una ricerca x stringa
296200070621     c                   eval      cerca_stringa  = 'S'
296300070622     c                   eval      stringa_20a = %subst(vc2pos2:1)
296400080128      * ora
296500070622     c                   when      *in03 and sav_pos3 <> vc2pos3
296600070622     c                   eval      sav_pos3 = vc2pos3
296700080128     c                   eval      posizionamento = 'S'
296800070622      * località
296900070622     c                   when      *in04 and sav_pos4 <> vc2pos4
297000070622     c                   eval      sav_pos4 = vc2pos4
297100070720      * è una ricerca x stringa
297200070621     c                   eval      cerca_stringa  = 'S'
297300070622     c                   eval      stringa_20a = %subst(vc2pos4:1)
297400070622      * volume
297500070622     c                   when      *in05 and sav_pos5 <> vc2pos5
297600070622     c                   eval      sav_pos5 = vc2pos5
297700070621     c                   eval      posizionamento = 'S'
297800070622      * peso
297900070622     c                   when      *in06 and sav_pos6 <> vc2pos6
298000070622     c                   eval      sav_pos6 = vc2pos6
298100070621     c                   eval      posizionamento = 'S'
298200070622
298300070622      * se precedentemente si era posizionato
298400070622     c                   when      vc2pos1 <> *blank  or
298500070622     c                             vc2pos2 <> *blank  or
298600080128     c                             vc2pos3 <> *zeros  or
298700070622     c                             vc2pos4 <> *blank  or
298800070622     c                             vc2pos5 <> *zero   or
298900070622     c                             vc2pos6 <> *zero
299000070621     c                   eval      posizionato = 'S'
299100070622
299200070622     c                   endsl
299300070622
299400070622      * se si deve posizionare sul sfl on *top
299500070621     c                   if        posizionamento = 'S'
299600070704     c                   exsr      da_posiz
299700070622     c                   endif
299800070622
299900070622      * se si deve cercare una stringa con (*)
300000070621     c                   if        cerca_stringa  = 'S'
300100070704     c                   exsr      cerca_strin
300200070622     c                   endif
300300070622
300400070622     c                   endsr
300500070622
300600070622      *------------------------------------------------------------------------*
300700070705      *   Imposta il posizionamento al record top del sfl2           ?
300800070622      *------------------------------------------------------------------------*
300900070704     c     da_posiz      begsr
301000070622
301100070622      * se si deve posizionare sul sfl on *top
301200070622     c                   eval      *in28 = *on
301300070622     c                   eval      vc2rcd =  1
301400070622
301500070622      * primo ciclo per togliere le priorità a tutti records
301600070622      * del sfl
301700070622     c                   do        rrnlast       s2nrr
301800070622     c     s2nrr         chain     or60s02
301900080611     c                   if        vs2sce = '1' or vs2sce = '5' or
302000140211     c                             vs2sce = '8' or vs2sce = '7' or
302100140211     c                             vs2sce = '9'
302200070622     c                   eval      *in22 = *on
302300070621     c                   else
302400070622     c                   eval      *in22 = *off
302500070622     c                   endif
302600070622     c                   clear                   h2subst
302700080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
302800080314     c                   if        vs2comun = *on or vs2pda = *on
302900080314     c                   eval      *in08 = *on
303000080314     c                   else
303100080314     c                   eval      *in08 = *off
303200080314     c                   endif
303300071022      * protezione o meno campo scelta
303400071022     c                   eval      *in09 = vs2prot
303500080723      * imposto se filiale ha PDA
303600080723     c                   eval      *in17 = (pdaorm <> *blanks)
303700070622      *                  __________________
303800070622     c                   update    or60s02
303900070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
304000070622     c                   enddo
304100070622
304200070720      * ordina il sfl
304300070704     c                   exsr      ordina_sfl2
304400070622
304500070622      * secondo ciclo x trovare il posizionamento dal record....
304600070622     c                   do        rrnlast       s2nrr
304700070704     c     s2nrr         chain     or60s02
304800070921     c                   if        (*in01 and vc2pos1 > vs2cgi)    or
304900070704     c                             (*in02 and vc2pos2 > vs2rsr)    or
305000131119     c                             (*in03 and vc2pos3 < vs2ora)    or
305100070622     c                             (*in04 and vc2pos4 > vs2lor)    or
305200070622     c                             (*in05 and vc2pos5 < vs2vlm)    or
305300070622     c                             (*in06 and vc2pos6 < vs2pkg)
305400070622
305500070622     c                   eval      vc2rcd = s2nrr + 1
305600070622     c                   if        vc2rcd > rrnlast
305700070622     c                   eval      vc2rcd = rrnlast
305800070622     c                   endif
305900070622
306000070622      * si tiene in memoria la riga toccata con sflnxtchg
306100080611     c                   if        vs2sce = '1' or vs2sce = '5' or
306200140211     c                             vs2sce = '8' or vs2sce = '7' or
306300140211     c                             vs2sce = '9'
306400070622     c                   eval      *in22 = *on
306500070621     c                   else
306600070622     c                   eval      *in22 = *off
306700070622     c                   endif
306800080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
306900080314     c                   if        vs2comun = *on or vs2pda = *on
307000080314     c                   eval      *in08 = *on
307100080314     c                   else
307200080314     c                   eval      *in08 = *off
307300080314     c                   endif
307400071022      * protezione o meno campo scelta
307500071022     c                   eval      *in09 = vs2prot
307600080723      * imposto se filiale ha PDA
307700080723     c                   eval      *in17 = (pdaorm <> *blanks)
307800070622      *                  __________________
307900070622     c                   update    or60s02
308000070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
308100070621     c                   else
308200070621     c                   iter
308300070622     c                   endif
308400070622     c                   enddo
308500070622
308600070622     c                   endsr
308700070622
308800070622      *------------------------------------------------------------------------*
308900070705      *   Se richiesta una ricerca di stringa fra i record del SFL   ?
309000070622      *------------------------------------------------------------------------*
309100070704     c     cerca_strin   begsr
309200070622
309300070622      * deve cercare la stringa e visualizzare prioritariamente i records
309400070622      * contenenti la stringa
309500070622     c                   eval      *in28 = *on
309600070622     c                   eval      vc2rcd =  1
309700070622     c                   eval      wlen = %len(%trim(stringa_20a))
309800070622
309900070622     c                   do        rrnlast       s2nrr
310000070704     c     s2nrr         chain     or60s02
310100070622     c                   clear                   wpos
310200070622     c                   clear                   h2subst
310300070622
310400070705     c   02              eval      wpos = %scan(%subst(stringa_20a:1:wLen) :
310500070704     c                              vs2rsr)
310600070622
310700070705     c   04              eval      wpos = %scan(%subst(stringa_20a:1:wLen) :
310800070622     c                              vs2lor)
310900070621
311000070622      * se ha trovato la stringa cercata
311100070622     c                   if        wpos > 0
311200080612     c                   eval      h2subst = 1
311300070622     c                   endif
311400070622
311500070622      * si tiene in memoria la riga toccata con sflnxtchg
311600070622     c                   if        vs2sce = '1' or vs2sce = '5'
311700131119     c                             or vs2sce = '7'
311800070622     c                   eval      *in22 = *on
311900070621     c                   else
312000070622     c                   eval      *in22 = *off
312100070622     c                   endif
312200080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
312300080314     c                   if        vs2comun = *on or vs2pda = *on
312400080314     c                   eval      *in08 = *on
312500080314     c                   else
312600080314     c                   eval      *in08 = *off
312700080314     c                   endif
312800071022      * protezione o meno campo scelta
312900071022     c                   eval      *in09 = vs2prot
313000080723      * imposto se filiale ha PDA
313100080723     c                   eval      *in17 = (pdaorm <> *blanks)
313200070622      *                  __________________
313300070622     c                   update    or60s02
313400070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
313500070622     c                   enddo
313600070622
313700070622      * quindi emette il sfl riordinato
313800070704     c                   exsr      ordina_sfl2
313900070622
314000070622     c                   endsr
314100070622
314200070622      *------------------------------------------------------------------------*
314300070705      *   Controllo opzioni lista                                    ?
314400070622      *------------------------------------------------------------------------*
314500070704     c     ctrs2         begsr
314600070622
314700070720      * pulizia dei campi di totale + campi di comodo e indicatori
314800070622     c                   clear                   totnor_s2
314900070622     c                   clear                   totpkg_s2
315000070622     c                   clear                   totvlm_s2
315100071017     c                   clear                   wvlm
315200070622     c                   clear                   giri_scelti
315300070704     c                   clear                   sav_s2cgi
315400070622     c                   clear                   primogiro_blk
315500080611     c                   eval      wokinfo = *off
315600070622     c                   eval      $esci = *off
315700070622     c                   eval      *in28 = *off
315800080612
315900080612     c                   eval      *in25 = *off
316000080612     c                   eval      *in26 = *off
316100080612     c                   eval      *in27 = *off
316200070622
316300070622      * leggo il sfl solo se ci sono rcd
316400070622     c                   if        wmax > *zeros
316500131127     c                   readc     or60s02                                30
316600070622
316700070622      * esce se fine sfl o errore che richiede l'uscita
316800131127     c                   dow       not *in30 and $esci = *off
316900070622     c                   eval      vc2rcd = s2nrr
317000070919     c                   eval      *in39 = *off
317100070622      * ctrl su riga
317200070704     c                   exsr      recs2
317300070621      *
317400070622      * se rilevato errore o richiesta uscita, attivo sflnxtchg
317500070622     c                   if        *in28 or $esci = *on
317600070720      * disabilito l'eventuale richiesta di reinizializzazione sfl
317700070622      * la ripristinerò a conclusione del ciclo di readc
317800131119     c                   IF        $esci = *off
317900070622     c                   eval      $inzs2 = *off
318000131119     c                   ELSE
318100131119     c                   eval      $inzs2 = *on
318200131119     c                   ENDIF
318300070622     c                   endif
318400070622
318500070622      * attivo sempre il sflnxtchg e aggiorno sfl
318600080613     c                   if        vs2sce <> *blanks
318700070622     c                   eval      *in22 = *on
318800070621     c                   else
318900070622     c                   eval      *in22 = *off
319000070622     c                   endif
319100080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
319200080314     c                   if        vs2comun = *on or vs2pda = *on
319300080314     c                   eval      *in08 = *on
319400080314     c                   else
319500080314     c                   eval      *in08 = *off
319600080314     c                   endif
319700071022      * protezione o meno campo scelta
319800071022     c                   eval      *in09 = vs2prot
319900080723      * imposto se filiale ha PDA
320000080723     c                   eval      *in17 = (pdaorm <> *blanks)
320100070622      *                  __________________
320200070622     c                   update    or60s02
320300070622      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
320400070622      * leggo prossimo rcd dal sfl se no richiesta uscita ciclo
320500070622     c                   if        $esci = *off
320600070622     c                   readc     or60s02                                30
320700070622
320800070720      * a fine ciclo ripristino il flag richiesta inizializzazione sfl
320900070622     c                   if        *in30
321000070622      * calcolo pagina a cui deve posizionarsi
321100070719     c                   eval      wpagine = s2pag
321200070704     c                   exsr      clcpag
321300070622     c                   endif
321400070622     c                   endif
321500070622
321600070622     c                   enddo
321700070622
321800070622     c                   endif
321900070622
322000070622     c                   endsr
322100070622
322200070622      *------------------------------------------------------------------------*
322300070705      *   Controllo campi i/o riga lista                             ?
322400070622      *------------------------------------------------------------------------*
322500070704     c     recs2         begsr
322600070622
322700070622      * gestione opzioni
322800070622     c                   if        vs2sce <> *blanks and not *in28
322900080612     c                   if        %lookup(vs2sce :opz) = *zeros
323000080612     c                   eval      *in27 = *on
323100080612     c                   eval      *in28 = *on
323200131127     c                   leavesr
323300080612     c                   endif
323400070720
323500070720      * interrogazione orm
323600070720     c                   if        vs2sce = '5'
323700070803     c                   clear                   dsfior07
323800070720     c                   eval      dsopoe = vs2poe
323900070720     c                   eval      dsonor = vs2nor
324000070720     c                   eval      dsonsr = vs2nsr
324100070720     c                   eval      dsonrv = vs2nrv
324200070720     c                   eval      dsoopz = vs2sce
324300070720     c                   eval      dsofgs = v1cfgs
324400070803     c                   eval      kpjbu = dsfior07
324500070720     c                   call      'FIOR07R'
324600070720     c                   parm                    kpjba
324700071012     c                   clear                   vs2sce
324800131127     c                   eval      wnowin = *on
324900070720     c                   endif
325000131119
325100131119      * Assegna giro
325200131119     c                   if        vs2sce = '7'
325300131119     c                   clear                   fior73ds
325400131119     c                   eval      IOR73poe = vs2poe
325500131119     c                   eval      IOR73nor = vs2nor
325600131119     c                   eval      IOR73nsr = vs2nsr
325700131119     c                   eval      IOR73nrv = vs2nrv
325800131119     c                   clear                   kpjbu
325900131119     c                   call      'FIOR73R'
326000131119     c                   parm                    kpjba
326100131119     c                   parm                    fior73ds
326200131119     c                   clear                   vs2sce
326300131127     c                   eval      wnowin = *on
326400131119     c                   IF        OOR73agg = *on
326500131127     c                   eval      $inzs2 = *on
326600131119     c                   eval      waggiorna = *on
326700131119     c                   ENDIF
326800131119     c                   endif
326900070919
327000070919      * se Assegna o Trasferisce
327100070919      * imposto se anche solo 1 orm selezionato ha la sponda idraulica
327200070919     c                   if        (v1sce = '1' or v1sce = '3')
327300070925     c                              and vs2sce = '1' and vs2spi = 'Sp'
327400070919     c                   eval      $ormspi = *on
327500131127     c                   eval      wnowin = *off
327600070919     c                   endif
327700070921
327800070921      * se Toglie o Trasferisce
327900080314     c                   if        (v1sce = '2' or v1sce = '3')
328000080526     c                             and vs2sce = '1'
328100080526     c                   eval      $ormesitono = *off
328200080526     c                   eval      $ormesitook = *off
328300080526     c                   eval      $ormpda = *off
328400080526     c                   eval      $ormcomun = *off
328500131127     c                   eval      wnowin = *off
328600080317     c                   select
328700080526     c                   when      vs2forza = *on
328800080317      * imposto se ORM in fase 410 o 420
328900080526     c                   when      vs2far = 410
329000080314     c                   eval      $ormesitono = *on
329100080526     c                   eval      *in25 = *on
329200080526     c                   eval      *in28 = *on
329300080526     c                   eval      vs2forza = *on
329400080317     c                   when      vs2far = 420
329500080314     c                   eval      $ormesitook = *on
329600080526     c                   eval      *in25 = *on
329700080526     c                   eval      *in28 = *on
329800080526     c                   eval      vs2forza = *on
329900080317      * Stessa cosa se ORM scaricato a PDA
330000080317     c                   when      vs2pda = *on
330100080317     c                   eval      $ormpda = *on
330200080526     c                   eval      *in26 = *on
330300080526     c                   eval      *in28 = *on
330400080526     c                   eval      vs2forza = *on
330500080317      * imposto se anche solo 1 orm selezionato è comunicato
330600080317     c                   when      vs2comun = *on
330700080317     c                   eval      $ormcomun = *on
330800080526     c                   eval      *in26 = *on
330900080526     c                   eval      *in28 = *on
331000080526     c                   eval      vs2forza = *on
331100080317     c                   endsl
331200080508
331300080314     c                   endif
331400070720
331500070720      * totalizza gli ORM/pesi/volumi/colli
331600070720     c                   if        vs2sce = '1'
331700070720     c                   eval      totnor_s2 = totnor_s2 + 1
331800070720     c                   eval      totpkg_s2 = totpkg_s2 + vs2pkg
331900071017     c                   if        wvlm < 99,990
332000071017     c                   eval      wvlm = wvlm + vs2vlm
332100071017     c                   endif
332200071017     c                   if        wvlm >= 99,990
332300071017     c                   eval      totvlm_s2 = 99,999
332400071017     c                   else
332500071017     c                   eval      totvlm_s2 = wvlm
332600071017     c                   endif
332700070720
332800131127     c                   eval      wnowin = *off
332900070720     c                   eval      sav_sxcgi = vs2cgi
333000070720
333100070720      * conta quanti giri sono stati scelti nel sfl
333200070720      * per permettere di interrogare con l'F4 con stesso giro
333300070720     c                   if        vs2cgi <> sav_s2cgi  or
333400070720     c                             primogiro_blk = *blank and vs2cgi = *blank
333500070720
333600070720      * e se il giro è blank non deve ripetere un eventuale conto di altri con giro blank
333700070720     c                   eval      primogiro_blk = 'S'
333800070720
333900070720     c                   eval      giri_scelti = giri_scelti + 1
334000070720     c                   eval      sav_s2cgi = vs2cgi
334100070720     c                   eval      vc2giro = vs2cgi
334200070720     c                   endif
334300070720     c                   endif
334400080611
334500080611      * chiude INFO se presente una INFO
334600080612     c                   if        vs2sce = '8' and
334700080714      * se procedura PDA ORM attiva o in test
334800080714     c                             pdaorm <> *blanks
334900131127     c                   eval      wnowin = *on
335000080613      * se non c'è la info a video errore
335100080613     c                   if        vs2info = *blanks
335200080613     c                   eval      *in27 = *on
335300080613     c                   eval      *in28 = *on
335400080613     c                   leavesr
335500080613     c                   endif
335600080611      * aggiorna il PRO togliendo la INFO
335700080611     c                   exsr      sr_chiuinfo
335800080613     c                   clear                   vs2sce
335900080611     c                   clear                   vs2info
336000080611     c                   eval      wokinfo = *on
336100080611     c                   endif
336200070720
336300070622     c                   endif
336400070622
336500140211      * chiude INFO se presente una INFO
336600140211     c                   if        vs2sce = '9' and
336700140211      * se procedura PDA ORM attiva o in test
336800140211     c                             pdaorm <> *blanks
336900140211      * se non c'è la info a video errore
337000140211     c                   if        vs2info = *blanks
337100140211     c                   eval      *in27 = *on
337200140211     c                   eval      *in28 = *on
337300140211     c                   leavesr
337400140211     c                   endif
337500140211      * aggiorna il PRO togliendo la INFO
337600140211     c                   exsr      sr_visinfo
337700140211     c                   clear                   vs2sce
337800140212     c                   eval      wnowin = *on
337900140211     c                   endif
338000140211
338100070622     c                   endsr
338200070718
338300070718      *------------------------------------------------------------------------*
338400070719      *   Gestione ultima finestra di riepilogo                  ?
338500070718      *------------------------------------------------------------------------*
338600070718     c     gesw1         begsr
338700070719
338800070719      * inizializzazione videata
338900070719     c                   if        $inzw1 = *on
339000070719     c                   exsr      inzw1
339100070719     c                   eval      $inzw1 = *off
339200070719     c                   endif
339300070719
339400070719     c     window_1      tag
339500070719
339600070719      * emissione video
339700070719      *                  __________________
339800070719     c                   exfmt     or60w01
339900070719      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
340000070719     c                   eval      *in28 = *off
340100070719
340200070719      * selezioni
340300070720     c                   select
340400070719      * f3=fine
340500070719     c                   when      $tasto = f03
340600070719     c                   exsr      f03_esci
340700070720     c                   rolbk
340800070719      * f12=ritorno
340900070719     c                   when      $tasto = f12
341000070720     c                   eval      $gest = 'S2'
341100070720      * scelta = 1 se veniva da SFL 1
341200070720     c                   if        v1sce = '1' and sav$gest ='S1'
341300070720     c                   eval      $gest = 'S1'
341400070719     c                   endif
341500070719
341600070720     c                   other
341700070719      * controllo
341800070719     c                   exsr      ctrw1
341900070720     c                   endsl
342000070719
342100070719      * per errori
342200070719     c                   if        *in28
342300070719     c                   goto      window_1
342400070719     c                   endif
342500070719
342600070719      * f6=aggiorna
342700070719     c                   if        $tasto = f06
342800070719     c                   exsr      f06_aggiorna
342900070719     c                   endif
343000140529
343100140530      /free
343200140530       //?F8=Aggiorna + Preparazione dati
343300140530         IF  $tasto = F08;
343400140530           exsr F06_aggiorna;
343500140530           InviaDatiPDA = *on;
343600140530         ENDIF;
343700140530      /end-free
343800070719
343900070718     c                   endsr
344000070719
344100070719      *------------------------------------------------------------------------*
344200070719      *   Inizializzo window finale prima dell'aggiornamento     ?
344300070719      *------------------------------------------------------------------------*
344400070719     c     inzw1         begsr
344500070719
344600070719     c                   eval      *in28 = *off
344700070719     c                   eval      *in50 = *off
344800070719     c                   setoff                                       515253
344900070719     c                   setoff                                       545556
345000140529     c                   eval      InviaDatiPda = *off
345100140530     c                   eval      *in42 = *off
345200070719
345300070719      * pulizia formato record
345400070719     c                   clear                   or60w01
345500070719
345600070719      * imposta i campi a video
345700070719     c                   eval      w1snor1 = totnor_s2
345800070719     c                   eval      w1spkg1 = totpkg_s2
345900070719     c                   eval      w1svlm1 = totvlm_s2
346000070719
346100070719     c                   eval      w1snor2 = totnor_s2
346200070719     c                   eval      w1spkg2 = totpkg_s2
346300070719     c                   eval      w1svlm2 = totvlm_s2
346400070719
346500070719     c                   select
346600070719      *  Assegnare
346700070719     c                   when      v1sce = '1'
346800070719     c                   eval      w1ndc1 = h1ndc_piu
346900070719     c                   eval      w1ada = 'alla'
347000070720     c                   eval      %subst(w1dsce:6:11) = 'ASSEGNA'
347100140530     c                   eval      *in42 = *on
347200070719
347300070719      *  Togliere
347400070719     c                   when      v1sce = '2'
347500070719     c                   eval      w1ndc1 = h1ndc_meno
347600070719     c                   eval      w1ada = 'dalla'
347700070719     c                   eval      %subst(w1dsce:7:11) = 'TOGLIE'
347800070719
347900070719      *  Trasferire
348000070719     c                   when      v1sce = '3'
348100070719      * se sta eseguendo il trasferimento occorre aprire la finestra
348200070719     c                   eval      *in50 = *on
348300070719     c                   eval      w1ndc1 = h1ndc_meno
348400070719     c                   eval      w1ndc2 = h1ndc_piu
348500070719     c                   eval      w1ada = 'dalla'
348600070719     c                   eval      %subst(w1dsce:6:11) = 'TRASFERISCE'
348700140530     c                   eval      *in42 = *on
348800070719
348900070719     c                   endsl
349000070719
349100070719      * legge la distinta
349200070719     c                   move      w1ndc1        wnfv
349300070719     c                   eval      aggior_dist = 'N'
349400070719     c                   exsr      chain_dist
349500070720      * distinta non trovata
349600070719     c                   if        found_dist = 'N'
349700070719     c                   eval      w1pdr1 = *all'?'
349800070719     c                   else
349900070719     c     kapd          chain     fiapd01l
350000070719     c                   if        %found(fiapd01l)
350100070719     c                   eval      w1pdr1 = apdrsc
350200070719     c                   endif
350300070719
350400070731     c                   eval      w1dmpkg1 = dstrpkl
350500070731     c                   eval      w1dmvlm1 = dstrvlu
350600070731     c                   eval      w1dmnor1 = dstrnbl
350700070903
350800070903     c                   eval      w1daspk1 = dstpkla
350900070903     c                   eval      w1dasvl1 = dstvlua
351000070903     c                   eval      w1dansp1 = dstnbla
351100070719
351200070731     c                   eval      w1dapkg1 = dstrpkla
351300070731     c                   eval      w1davlm1 = dstrvlua
351400070731     c                   eval      w1danor1 = dstrnbla
351500070719     c                   endif
351600070719
351700070719      * legge la distinta
351800070719     c                   move      w1ndc2        wnfv
351900070719     c                   eval      aggior_dist = 'N'
352000070719     c                   exsr      chain_dist
352100070720      * distinta non trovata
352200070719     c                   if        found_dist = 'N'
352300070719     c                   eval      w1pdr2 = *all'?'
352400070719     c                   else
352500070719     c     kapd          chain     fiapd01l
352600070719     c                   if        %found(fiapd01l)
352700070719     c                   eval      w1pdr2 = apdrsc
352800070719     c                   endif
352900070719
353000070731     c                   eval      w1dmpkg2 = dstrpkl
353100070731     c                   eval      w1dmvlm2 = dstrvlu
353200070731     c                   eval      w1dmnor2 = dstrnbl
353300070903
353400070903     c                   eval      w1daspk2 = dstpkla
353500070903     c                   eval      w1dasvl2 = dstvlua
353600070903     c                   eval      w1dansp2 = dstnbla
353700070719
353800070731     c                   eval      w1dapkg2 = dstrpkla
353900070731     c                   eval      w1davlm2 = dstrvlua
354000070731     c                   eval      w1danor2 = dstrnbla
354100070719     c                   endif
354200070719
354300070719     c                   select
354400070719      * Assegnare
354500070719     c                   when      v1sce = '1'
354600070719     c     w1danor1      add       totnor_s2     w1tnor1
354700070719     c     w1dapkg1      add       totpkg_s2     w1tpkg1
354800070719     c     w1davlm1      add       totvlm_s2     w1tvlm1
354900070719     c     w1tnor1       comp      w1dmnor1                           51
355000070719     c     w1tpkg1       comp      w1dmpkg1                           52
355100070719     c     w1tvlm1       comp      w1dmvlm1                           53
355200070719
355300070719      * Togliere
355400070719     c                   when      v1sce = '2'
355500070719     c     w1danor1      sub       totnor_s2     w1tnor1
355600070719     c     w1dapkg1      sub       totpkg_s2     w1tpkg1
355700070719     c     w1davlm1      sub       totvlm_s2     w1tvlm1
355800070719     c     w1tnor1       comp      w1dmnor1                           51
355900070719     c     w1tpkg1       comp      w1dmpkg1                           52
356000070719     c     w1tvlm1       comp      w1dmvlm1                           53
356100070719
356200070719      * Trasferire
356300070719     c                   when      v1sce = '3'
356400070719     c     w1danor1      sub       totnor_s2     w1tnor1
356500070719     c     w1dapkg1      sub       totpkg_s2     w1tpkg1
356600070719     c     w1davlm1      sub       totvlm_s2     w1tvlm1
356700070719     c     w1tnor1       comp      w1dmnor1                           51
356800070719     c     w1tpkg1       comp      w1dmpkg1                           52
356900070719     c     w1tvlm1       comp      w1dmvlm1                           53
357000070719
357100070719     c     w1danor2      add       totnor_s2     w1tnor2
357200070719     c     w1dapkg2      add       totpkg_s2     w1tpkg2
357300070719     c     w1davlm2      add       totvlm_s2     w1tvlm2
357400070719     c     w1tnor2       comp      w1dmnor2                           54
357500070719     c     w1tpkg2       comp      w1dmpkg2                           55
357600070719     c     w1tvlm2       comp      w1dmvlm2                           56
357700070719
357800070719     c                   endsl
357900070719
358000070719     c                   endsr
358100070719
358200070720      *------------------------------------------------------------------------*
358300070720      *  Controlli della finestra di conferma aggiornamento         ?
358400070720      *------------------------------------------------------------------------*
358500070720     c     ctrw1         begsr
358600070720
358700070720     c                   eval      *in28 = *off
358800070720      * se superano i MAX previsti in distinta
358900070720     c                   if        v1sce = '3'
359000070720     c     w1tnor2       comp      w1dmnor2                           54
359100070720     c     w1tpkg2       comp      w1dmpkg2                           55
359200070720     c     w1tvlm2       comp      w1dmvlm2                           56
359300070719     c                   else
359400070720     c     w1tnor1       comp      w1dmnor1                           51
359500070720     c     w1tpkg1       comp      w1dmpkg1                           52
359600070720     c     w1tvlm1       comp      w1dmvlm1                           53
359700070720     c                   endif
359800070720
359900070720     c                   endsr
360000070719
360100070719      *------------------------------------------------------------------------*
360200070720      *  Gestione sfl ORM da selezionare                        ?
360300070719      *------------------------------------------------------------------------*
360400070719     c     gess3         begsr
360500070720
360600070720      * inizializzazione videata
360700070720     c                   if        $inzs3 = *on
360800070720     c                   exsr      inzs3
360900070720     c                   eval      $inzs3 = *off
361000070720     c                   endif
361100070720
361200070720      * fase di commit o rollback    ?
361300070720     c                   if        exec_rb = 'S'
361400070720      * esegue il rollback x un precedente problema su uno degli aggiornamenti
361500070720     c                   rolbk
361600070720     c                   else
361700070720      * esegue il commit quanto tutto è ok?
361800070720     c                   commit
361900080714      * se è togli o trasferisci devo quadrare i dati di PDO con quelli di ORM
362000140121      * e i dati delle telefonate AUT
362100080714     c                   if        v1sce = '2' or v1sce = '3'
362200080714     c                   exsr      sr_quadrapdo
362300080714     c                   endif
362400140530      /free
362500140530       //?Se richiesto 'Metti' o 'Trasferisci' e F08 e NON ci sono stati errori
362600140530       //?devo richiamare la preparazione dati a PDA
362700140530         IF  (V1sce = '1' or V1sce = '3') and InviaDatiPDA and exec_rb <> 'S';
362800140530           clear FIOR56DS;
362900140530           fior56ds.OR56tla = 'T';
363000140530           fior56ds.OR56fgs = wfgs;
363100140530           fior56ds.OR56ddc = wddc;
363200140530           fior56ds.OR56ndcd = h1ndc_piu;
363300140530           fior56ds.OR56ndca = h1ndc_piu;
363400140530           fior56ds.OR56cmd = 'I';
363500140530           kpjbu = FIOR56DS;
363600140530           fior56r (kpjba:FIOR56DS);
363700140530         ENDIF;
363800140530      /end-free
363900070720     c                   endif
364000070720
364100070720      * inizializza e imposta piede videata
364200070720     c                   exsr      inzz3
364300070720
364400070720      * emissione piede videata
364500070720      *                  __________________
364600070720     c                   write     or60z03
364700070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
364800071002      * qualcosa è andato storto
364900080714     c                   if        wmax > *zeros
365000070720     c                   eval      vedi_video = 'S'
365100070720     c                   eval      *in20 = *on
365200070720     c                   if        wsfl > *zeros
365300070720     c                   eval      vc3rcd = wsfl
365400070720     c                   else
365500070720     c                   if        wpag > *zeros
365600070720     c                   eval      vc3rcd = wpag
365700070720     c                   endif
365800070720     c                   endif
365900070720     c                   endif
366000080714     c                   if        exec_rb = 'S'
366100080714     c                   eval      vedi_video = 'S'
366200080717     c                   else
366300080717     c                   clear                   vedi_video
366400080714     c                   endif
366500070720
366600070720      * emissione SFL dopo aggiornamento dati
366700070720      * con errori quindi tutta la transazione deve essere rifatta
366800070720     c                   if        vedi_video = 'S'
366900070720      *                  __________________
367000070720     c                   exfmt     or60c03
367100070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
367200070720     c                   endif
367300070720     c                   eval      *in28 = *off
367400070720      *
367500070720     c                   if        vc3nrr <> *zeros
367600070720     c                   eval      wsfl = vc3nrr
367700070720     c                   endif
367800070720     c                   eval      vc3rcd = sflnrr
367900070720      * selezioni
368000070720     c                   select
368100070720      * f3=fine
368200070720     c                   when      $tasto = f03
368300070720     c                   exsr      f03_esci
368400070720
368500070720     c                   other
368600070720     c                   eval      *in28 = *off
368700070731     c                   endsl
368800070720
368900070720      * riemette il primo video
369000070720     c                   eval      $gest = 'D1'
369100070720      * non reinizializzato se ci sono stati problemi
369200070720     c                   if        wmax > *zeros
369300070720     c                   eval      $inzd1 = *off
369400070720     c                   rolbk
369500070720     c                   else
369600070720      * reinizializzato se non ci sono stati problemi
369700070720     c                   eval      $inzd1 = *on
369800070720     c                   endif
369900070720      * se si era scelto di Assegnare si deve ritornare sul SFL dei GIRI
370000070720      * dove era impostata la scelta e pulire la distinta x riselezionarla.
370100070720     c                   if        v1sce = '1'
370200131118     c                   eval      waggiorna = *on
370300131114     c                   eval      $gest = 'S2'
370400131114     c                   eval      $inzs2 = *on
370500070720     c                   clear                   h1ndc_piu
370600070720     c                   clear                   h1pdr_piu
370700070720     c                   clear                   h1fpp_piu
370800070919     c                   clear                   h1spi_piu
370900070809     c                   clear                   con_giro
371000070809     c                   clear                   wcgi
371100070720     c                   eval      wsfl = sav_wsfl
371200131119     c                   eval      wmax = sav_wmax2
371300070720     c                   eval      wpag = sav_wpag
371400070720      * avendo assegnato l'unico record non posso riemettere il SFL vuoto
371500070720      * quindi torno al video introduttivo.
371600070720     c                   if        wmax  = 1
371700131114     c                   eval      $gest = 'S1'
371800131114     c                   eval      $inzs1 = *on
371900070720     c                   endif
372000070720
372100070720     c                   else
372200070720      * se togliere o trasferire
372300070720      * vado direttamente sull'interrogazione deistinte
372400070720     c                   eval      salva_v1sce = v1sce
372500070720     c                   eval      salta_video1 = 'S'
372600070720     c                   eval      $gest = 'D1'
372700070720     c                   eval      $inzd1 = *off
372800070720     c                   exsr      inzd1
372900070720     c                   eval      v1sce = salva_v1sce
373000070720     c                   endif
373100070720
373200070720     c                   endsr
373300070720
373400070720      *------------------------------------------------------------------------*
373500070720      *  Inizializzazione e imposta il piede del sfl               ?
373600070720      *------------------------------------------------------------------------*
373700070720     c     inzz3         begsr
373800070720
373900070720      * messaggio o tasti funzionali
374000070720     c                   clear                   z3dmsg
374100070720     c                   clear                   z3der1
374200070720     c                   clear                   z3der2
374300070720
374400070720      * se ci sono dei records allora ci sono stati dei problemi
374500070720      * durante l'aggiornamento orm x orm
374600070720     c                   if        wmax > *zeros
374700070720     c                   eval      *in57 = *on
374800070720     c                   eval      z3der1 = msg(27)
374900070720     c                   eval      z3der2 = msg(28)
375000070720
375100070720     c                   else
375200070720      * se ci sono stati dei problemi durante gli aggiornamenti
375300070720      * o su ORM o su DST tanto da eseguire comunque il rollback
375400070720      *  imposta un altro tipo di messaggio.
375500070720     c                   if        exec_RB = 'S'
375600070720     c                   eval      *in57 = *on
375700070720     c                   eval      z3der1 = msg(29)
375800070720     c                   eval      z3der2 = msg(30)
375900070720     c                   endif
376000070720     c                   endif
376100070720
376200070720      * tasti funzionali se non è stato emesso alcun messaggio
376300070720     c                   if        z3dmsg = *blank
376400070720     c                   eval      *in28 = *off
376500070720     c                   eval      z3dmsg = 'F3=Fine'
376600071002     c                   eval      %subst(z3dmsg:35:13) = 'F9=Altri Dati'
376700071002     c                   eval      %subst(z3dmsg:68:11) = 'F12=Ritorno'
376800070720     c                   endif
376900070720
377000070720     c                   endsr
377100070720
377200070720      *------------------------------------------------------------------------*
377300070720      *  Inizializzazione lista ORM da giri o da distinta
377400070720      *------------------------------------------------------------------------*
377500070720     c     inzs3         begsr
377600070720
377700070720      * pulizia sfl
377800070720     c                   eval      *in20 = *off
377900070720     c                   eval      *in21 = *off
378000070720      *                  __________________
378100070720     c                   write     or60c03
378200070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
378300070720     c                   eval      *in21 = *on
378400070720
378500070720      * righe x pagina del 3° sfl
378600070720     c                   eval      s3pag = rigpag_sfl3
378700070720
378800070720      * caricamento sfl totale
378900070720     c                   clear                   s3nrr
379000070720     c                   eval      vc3rcd = 1
379100070720     c                   clear                   wmax
379200070720     c                   clear                   wsfl
379300070720      * tipo funzione
379400070720     c                   select
379500070720     c                   when      v1sce = '1'
379600070720     c                   eval      vc3dsce = 'ASSEGNA'
379700070720     c                   eval      vc3dess = 'alla Distinta nr.' +
379800070720     c                             %trim(%editc(h1ndc_piu:'Z'))
379900070720
380000070720     c                   when      v1sce = '2'
380100070720     c                   eval      vc3dsce = 'TOGLIE'
380200070720     c                   eval      vc3dess = 'dalla Distinta nr.' +
380300070720     c                             %trim(%editc(h1ndc_meno:'Z'))
380400070720
380500070720     c                   when      v1sce = '3'
380600070720     c                   eval      vc3dsce = 'TRASFERISCE'
380700070720     c                   eval      vc3dess = 'dalla Distinta nr.' +
380800070720     c                             %trim(%editc(h1ndc_meno:'Z'))
380900070720
381000070720     c                   endsl
381100070720
381200070720      * carica il sfl
381300070720     c                   exsr      rols3
381400070720
381500070720     c                   eval      wpag = 1
381600070720
381700070720     c                   endsr
381800070720
381900070720      *------------------------------------------------------------------------*
382000070720      *  Esegue aggiornamento dei dati caricando il SFL3 x problemi ?
382100070720      *------------------------------------------------------------------------*
382200070720     c     rols3         begsr
382300070720
382400070720     c                   eval      *in22 = *off
382500070720     c                   clear                   yy
382600070720     c                   eval      s3nrr = wmax
382700070720
382800070720      *  L'aggiornamento è pilotato dalle scelte effettuate sul SFL2 ?
382900070720      *  degli ORM e il caricamento del SFL3 serve per elencare    ?
383000070720      *  gli ORM che non sono stati aggiornati x qualsiasi motivo   ?
383100070720      *  classico motivo: allocazione record
383200070720     c                   exsr      agg_da_sfl2
383300070720      *  Dopo aver aggiornato i files x singolo ORM, aggiorna ?
383400070720      *  a livello di distinta a meno che non ci sia già da      ?
383500070720      *  eseguire il rollback x precedenti problemi           ?
383600070720     c                   if        exec_rb <> 'S'
383700070720     c                   exsr      agg_x_dist
383800070720     c                   endif
383900070720
384000070720      * fine caricamento
384100070720     c                   eval      wmax = s3nrr
384200070720
384300070720      * posizionamento al 1° rcd della pagina
384400070720     c                   eval      pagine = %div(s3nrr:s3pag)
384500070731     c                   eval      resto = %rem(s3nrr:s3pag)
384600070720     c                   eval      vc3rcd = pagine * s3pag
384700070720     c                   if        resto > *zeros
384800070720     c                   eval      vc3rcd = vc3rcd + 1
384900070720     c                   else
385000070720     c                   eval      vc3rcd = vc3rcd - s3pag
385100070720     c                   eval      vc3rcd = vc3rcd + 1
385200070720     c                   endif
385300070720
385400070720     c                   endsr
385500070720
385600070720      *------------------------------------------------------------------------*
385700070720      *  Carica il SFL degli ORM non aggiornati se ci sono stati problemi?
385800070720      *------------------------------------------------------------------------*
385900070720     c     agg_da_sfl2   begsr
386000070720
386100070720      * si ricorda di eseguire il rollback se si sono verificati dei
386200070720      * problemi in qualsiasi parte dell'aggiornamento dei dati
386300070720     c                   eval      exec_rb = 'N'
386400070720
386500080708      * legge il SFL degli ORM dove selezionato con (1) le righe
386600080708      * da aggiornare
386700070720     c                   readc     or60S02                                30
386800070720
386900070720     c                   dow       not *in30 and vs2sce = '1'
387000070720
387100070720      * carico il SFL solo se ci sono dei problemi in aggiornamento degli ORM
387200070720      * per allocazione dei records o altro
387300070720     c                   clear                   problemi
387400070720      *                  _______________________
387500070720     c                   exsr      aggiorna_org
387600070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
387700070720      * x problemi sui records in aggiornamento
387800070720     c                   if        problemi = 'S'
387900070720     c                   eval      exec_rb = 'S'
388000070720
388100070720      * imposto i campi del subfile
388200070720     c                   clear                   or60s03
388300070720     c                   exsr      riga_sfl3
388400070720
388500070720      * attivo sempre il sflnxtchg e aggiorno sfl
388600070720     c                   eval      *in22 = *on
388700070720     c                   eval      s3nrr = s3nrr + 1
388800070720     c                   eval      yy = yy + 1
388900070720      *                  __________________
389000070720     c                   write     or60s03
389100070720      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
389200070720     c                   endif
389300070720
389400070720     c                   readc     or60S02                                30
389500070720     c                   enddo
389600070720
389700070720     c                   endsr
389800070720      *------------------------------------------------------------------------*
389900070720      *  Imposta campi del 3°SFL                                    ?
390000070720      *------------------------------------------------------------------------*
390100070720     c     riga_sfl3     begsr
390200070720
390300070720      * deve semplicemente impostare dal 2° al 3° SFL i campi
390400070927     c                   eval      vs3cgi = vs2cgi
390500070720     c                   eval      vs3poe = vs2poe
390600070720     c                   eval      vs3nsr = vs2nsr
390700070720     c                   eval      vs3nor = vs2nor
390800070720     c                   eval      vs3nrv = vs2nrv
390900070720     c                   eval      vs3rsr = vs2rsr
391000070720     c                   eval      vs3inr = vs2inr
391100070720     c                   eval      vs3lor = vs2lor
391200070720     c                   eval      vs3pkg = vs2pkg
391300070720     c                   eval      vs3vlm = vs2vlm
391400070720     c                   eval      vs3sto = vs2sto
391500070720     c                   eval      vs3ncl = vs2ncl
391600070720     c                   eval      vs3bnc = vs2bnc
391700070720     c                   eval      vs3dar = vs2dar
391800070720     c                   eval      vs3orr = vs2orr
391900131126     c                   eval      vs3orr = vs2ora
392000070720     c                   eval      vs3com = vs2com
392100070720     c                   eval      vs3spi = vs2spi
392200070720     c                   eval      vs3aut = vs2aut
392300071009     c                   eval      vs3all = vs2all
392400070720
392500070720     c                   endsr
392600070720
392700070720      *------------------------------------------------------------------------*
392800070720      *  Aggiornamento dei files a livello di ORM     (COMMIT)    ?
392900070720      *------------------------------------------------------------------------*
393000070720     c     aggiorna_org  begsr
393100070720
393200070720      * aggiorna org x ORM
393300070720     c     kaggiorna     chain(e)  fnorg01l
393400070720      * se ci sono problemi di allocazione record    ?
393500070720     c                   if        %error
393600070720      * problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
393700070720     c                   eval      problemi = 'S'
393800070720     c                   clear                   trul82ds
393900070720     c                   eval      ul82§rrn = org_nrr
394000070720     c                   eval      ul82§fil = 'FNORG01L'
394100070720     c                   eval      ul82§win = 'S'
394200070720     c                   eval      ul82§f7  = 'S'
394300070720     c                   eval      ul82§num = 2
394400070720     c                   eval      ul82§att = 2
394500070720     c                   eval      ul82§mss = msg(07)
394600070720     c                   eval      UL82§msw = 'La Distinta '
394700070720     c                             + %editc(w1ndc1:'Z') +  ' '
394800070720     c                             + ' non è manutenzionabile.'
394900070720      * chiamo il pgm che manda il messaggio info all'utente
395000070720     c                   call(e)   'TRUL82R'
395100070720     c                   parm                    trul82ds
395200070720      * ancora allocata
395300070720     c                   if        ul82§sts = 'A'
395400070720      * problemi durante l'aggiornamento --> eseguire rollback  ?
395500070720     c                   eval      problemi = 'S'
395600070720     c                   endif
395700070720
395800070720     c                   else
395900070720
396000070731     c                   if        %found(fnorg01l)
396100070720      * imposta i campi da aggiornare ?
396200070720     c                   select
396300070720      * Assegna o Trasferisce
396400070720     c                   when      v1sce = '1' or v1sce = '3'
396500080717      * se sto assegnando l'orm non deve avere la distinta su fnorg
396600080717     c                   if        v1sce = '1' and orgndc > 0
396700080717     c                   eval      problemi = 'S'
396800080717     c                   unlock    fnorg01l
396900080717     c                   leavesr
397000080717     c                   endif
397100090303      * se sto assegnando l'orm la filiale ritiro deve essere la stessa della distinta
397200090303     c                   if        v1sce = '1' and orgpor <> wfgs
397300090303     c                   eval      problemi = 'S'
397400090303     c                   unlock    fnorg01l
397500090303     c                   leavesr
397600090303     c                   endif
397700100624      * se sto assegnando l'orm questo deve essere ancora in fase assegnabile
397800100624      * se sto togliendo l'orm questo deve essere ancora in fase assegnabile
397900090403     c     kaggiorna     chain     fnorm01l
398000100409     c                   if        not%found(fnorm01l)
398100100409     c                   eval      problemi = 'S'
398200100409     c                   unlock    fnorg01l
398300100409     c                   leavesr
398400100409     c                   endif
398500100409     c                   if        v1sce = '1' and ormeti = ' '
398600090403     c                   eval      problemi = 'S'
398700090403     c                   unlock    fnorg01l
398800090403     c                   leavesr
398900090403     c                   endif
399000100624     c                   if        v1sce = '3' and ormeti = ' '
399100100409     c                   eval      problemi = 'S'
399200100409     c                   unlock    fnorg01l
399300100409     c                   leavesr
399400090403     c                   endif
399500080717      * se sto trasferendo la distinta su ORG deve essere quella che sto togliendo
399600080717     c                   if        v1sce = '3' and orgndc <> h1ndc_meno
399700080717     c                   eval      problemi = 'S'
399800080717     c                   unlock    fnorg01l
399900080717     c                   leavesr
400000080717     c                   endif
400100070720     c                   eval      orgfgs = wfgs
400200070731     c                   eval      orgpdc = h1pdr_piu
400300070720     c                   eval      orgndc = h1ndc_piu
400400070720     c                   eval      orgddc = h1cddc
400500070720     c                   eval      orgnftl = *all'9'
400600070720     c                   eval      orgslo = *zeros
400700071011     c                   time                    w0140
400800071011     c                   movel     w0140         orghvdis
400900071011     c                   clear                   wlbdat
401000071011     c                   move      w0140         g08dat
401100071011     c                   call      'XSRDA8'
401200071011     c                   parm                    wlbdat
401300071011     c                   z-add     g08inv        orgdtvdis
401400070720      * Toglie
401500070720     c                   when      v1sce = '2'
401600100624      * se sto togliendo l'orm questo deve essere ancora in fase assegnabile
401700100409     c     kaggiorna     chain     fnorm01l
401800100624     c                   if        not%found(fnorm01l) or ormeti = ' '
401900100409     c                   eval      problemi = 'S'
402000100409     c                   unlock    fnorg01l
402100100409     c                   leavesr
402200100409     c                   endif
402300070720     c                   clear                   orgpdc
402400070720     c                   clear                   orgfgs
402500070720     c                   clear                   orgndc
402600070720     c                   clear                   orgddc
402700070921     c                   clear                   orgnftl
402800070720     c                   clear                   orgslo
402900070720     c                   clear                   orgdtvdis
403000070720     c                   clear                   orghvdis
403100070720
403200070720     c                   endsl
403300070720
403400070720     c                   update    fnorg000
403500070720     c                   endif
403600070720
403700070720     c                   endif
403800070720
403900070720     c                   endsr
404000070720
404100070720      *------------------------------------------------------------------------*
404200070720      *  Aggiornamento dei files a livello distinta   (COMMIT)    ?
404300070731      *------------------------------------------------------------------------*
404400070720     c     agg_x_dist    begsr
404500070720
404600070720      * Deve aggiornare anche ORM solo se siamo in produzione  ?
404700070720      * mediante il FIOR61R ma a livello di distinta ?
404800070720     c                   if        test_simula <> 'S' and exec_rb <> 'S'
404900070720
405000070720     c                   clear                   fior61ds
405100070720     c                   eval      or61cmti = 'N'
405200070720     c                   eval      or61fgsi = wfgs
405300070720     c                   eval      or61dfvi = h1cddc
405400070720      * Se deve Assegnare: ?
405500070720     c                   if        v1sce = '1'
405600070720     c                   eval      or61mtdi = 'M'
405700070720     c                   eval      or61nfvi = h1ndc_piu
405800070921     c                   eval      or61nftl = 99999
405900070720     c                   endif
406000070905      * Se deve Togliere: ?
406100070905     c                   if        v1sce = '2'
406200070720     c                   eval      or61mtdi = 'T'
406300070720     c                   eval      or61nfvi = h1ndc_meno
406400071106     c                   eval      or61nftl = 99998
406500070720     c                   endif
406600070905      * Se deve Trasferire:
406700070905     c                   if        v1sce = '3'
406800070905     c                   eval      or61mtdi = 'X'
406900070905     c                   eval      or61nfvi = h1ndc_meno
407000070905     c                   eval      or61nfvin = h1ndc_piu
407100071106     c                   eval      or61nftl = 99998
407200070905     c                   endif
407300070904     c                   call      'FIOR61R'
407400070720     c                   parm                    kpjba
407500070720     c                   parm                    fior61ds
407600070720     c                   if        or61erro <> *blanks
407700070720      * problemi durante l'aggiornamento --> eseguire rollback  ?
407800070720     c                   eval      exec_rb  = 'S'
407900070720     c                   endif
408000070731     c                   endif
408100070720
408200070720      * deve aggiornare la o le distinte   ?
408300070720     c                   if        exec_rb  <> 'S'
408400070720      * richiamo in aggiornamento dati
408500070720     c                   move      w1ndc1        wnfv
408600070720      *                       ______________________
408700070720     c                   eval      aggior_dist = 'S'
408800070720      *                       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
408900070720     c                   exsr      chain_dist
409000070720
409100070720     c                   if        error_dist   = 'S'  or
409200070720     c                             rec_allocato = 'S'  or
409300070720     c                             found_dist   = 'N'
409400070720      * problemi durante l'aggiornamento --> eseguire rollback  ?
409500070720     c                   eval      exec_rb  = 'S'
409600070720     c                   endif
409700070720     c                   endif
409800070720
409900070720      * se si tratta di un trasferimento c'è il 2°aggancio alla distinta
410000070720     c                   if        w1ndc2  > 0 and exec_rb  <> 'S'
410100070720      * richiamo in aggiornamento dati
410200070720     c                   move      w1ndc2        wnfv
410300070720      *                       ______________________
410400070720     c                   eval      aggior_dist = 'S'
410500070720      *                       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
410600070720     c                   exsr      chain_dist
410700070720
410800070720     c                   if        error_dist   = 'S'  or
410900070720     c                             rec_allocato = 'S'  or
411000070720     c                             found_dist   = 'N'
411100070720      * problemi durante l'aggiornamento --> eseguire rollback  ?
411200070720     c                   eval      exec_rb  = 'S'
411300070720     c                   endif
411400070720     c                   endif
411500070720
411600070719     c                   endsr
411700070718
411800070718      *------------------------------------------------------------------------*
411900070718      *   Aggancia la distinta x allocarla ed in seguito x aggiornarla ?
412000070718      *------------------------------------------------------------------------*
412100070718     c     ric_dist      begsr
412200070718
412300070718     c                   clear                   fidg31ds
412400071030      * posizionamento cursore nel subfile per interrogazione distinte
412500071030     c                   move      savposnf      fdgnfpos
412600070718     c                   eval      fdgfgs = wfgs
412700070718
412800070718     c                   if        $tasto <> f05
412900070718     c                   eval      fdgcgi = wcgi
413000070718     c                   endif
413100070718      *
413200070718     c                   eval      fdggda = wddc
413300070718     c                   eval      fdggal = wddc
413400070718     c                   eval      fdgnpg = wnpg
413500070718     c                   eval      fdgric = 'S'
413600070718     c                   eval      fdgtfv = '2'
413700070718     c                   eval      fdgtpv = 'P'
413800070718     c                   eval      fdgopz = w1sce
413900070718     c                   eval      fdgnf1 = wndc
414000070719     c                   eval      fdgtdis = v1tdis
414100070718     c                   eval      fdgtusc = v1tusc
414200080708      * esclude le distinte con fase RCH e RQE
414300080708     c                   eval      fdgiefas = 'E'
414400080708     c                   eval      fdgfas01 = 'RCH'
414500080708     c                   eval      fdgfas02 = 'RQE'
414600070718     c                   clear                   $fv6
414700070718      * impostato a tutti 99999
414800070718     c                   do        600           xx
414900070718     c                   z-add     999999        $fv6(xx)
415000070718     c                   enddo
415100080715     c                   eval      fdgpgm = 'ASSMANUALE'
415200080611      * Attenzione:
415300080611      * Se richiamato da MONITOR con PDA, occorre modificare
415400080611      * alcuni parametri:
415500080611     c                   if        pgm_chiamante = 'MONITOR'
415600080611     c                             and v1sce = '2'
415700080611     c                   eval      fdgPGM = 'MONITOR'
415800080611     c                   eval      fdgSTP = ' '
415900080611     c                   eval      fdgnfv = o60nfv
416000080708      * se richiamato dal monitor non devo escludere nessuna fase
416100080708     c                   eval      fdgiefas = 'I'
416200080708     c                   clear                   fdgfas01
416300080708     c                   clear                   fdgfas02
416400080611     c                   endif
416500080611
416600070904     c                   eval      fdgtpop = 'R'
416700070718     c                   eval      kpjbu = fidg31ds
416800070718     c                   call      'FIDG31R'
416900070718     c                   parm                    kpjba
417000070718     c                   parm                    $fv6
417100070718     c                   eval      fidg31ds = kpjbu
417200071030      * posizionamento cursore nel subfile per interrogazione distinte
417300071030     c                   if        fdgnf1 > *zeros
417400071030     c                   move      fdgnf1        fdgnfpos
417500071030     c                   endif
417600071030     c                   move      fdgnfpos      savposnf
417700070718     c                   clear                   fdgnf1
417800070718     c                   clear                   wndc
417900070718
418000070718     c                   endsr
418100070718
418200070718      *------------------------------------------------------------------------*
418300070718      *   Aggancia la distinta x allocarla ed in seguito x aggiornarla ?
418400070718      *------------------------------------------------------------------------*
418500070718     c     chain_dist    begsr
418600070718
418700070718      * inizializza il trovato/non trovato
418800070718     c                   eval      found_dist = 'N'
418900070718     c                   eval      error_dist = 'N'
419000070718     c                   eval      rec_allocato = 'N'
419100070718
419200070720      * dalla vista logica prende il nrr
419300070718     c     kdst          chain     fidst01l
419400071029
419500071029      *?Se non trovava il record allocava l'ultima distinta che aveva
419600071029      *?trovato poichè il nrelrec. non si resettava ma era impostato
419700071029      *?da prima.
419800071029     c                   if        not %Found(fidst01l)
419900071029     c                   eval      dst_nrr = *hival
420000071029     c                   end
420100071029
420200070720      * x agganciare il fisico solo x allocare il record  ?
420300070718     c     dst_nrr       chain(e)  fidst00f
420400070720      * è stata usata questa tecnica perchè l'aggiornamento del FIDST
420500070731      * è fatto mediante il FIOR67R e,sotto COMIT,xchè tutto funzioni
420600070720      * correttamente,il file deve essere condiviso in SHARE altrimenti
420700070720      * non funziona. (Si è scelto di utilizzare x tutti il fisico)
420800070718     c                   if        %error
420900070718     c                   eval      error_dist = 'S'
421000070718      * se il record è allocato mando messaggio info all'utente che alloca il rcd
421100070718     c                   clear                   trul82ds
421200070718     c                   eval      ul82§rrn = dst_nrr
421300070718     c                   eval      ul82§fil = 'FIDST00F'
421400070718     c                   eval      ul82§win = 'S'
421500070718     c                   eval      ul82§f7  = 'S'
421600070718     c                   eval      ul82§num = 2
421700070718     c                   eval      ul82§att = 2
421800070718     c                   eval      ul82§mss = msg(07)
421900070720     c                   eval      ul82§msw = 'La Distinta '
422000070718     c                             + %editc(wnfv:'Z') +  ' è già utilizzata da -
422100070718     c                              un altro lavoro e non è manutenzionabile'
422200070718      * chiamo il pgm che manda il messaggio info all'utente
422300070718     c                   call(e)   'TRUL82R'
422400070718     c                   parm                    trul82ds
422500070720      * ancora allocato
422600070718     c                   if        ul82§sts = 'A'
422700070718     c                   eval      rec_allocato = 'S'
422800070718     c                   endif
422900070718     c                   endif
423000070718      * trovato record
423100070718     c                   if        %found(fidst00f)
423200160219      * record annullato
423300160219     c                   IF        DSTatb <> *blanks and
423400160219     c                             aggior_dist = 'S'
423500160219     c                   unlock    fidst00f
423600160219     c                   leavesr
423700160219     c                   ENDIF
423800160219      * record ok
423900070718     c                   eval      found_dist = 'S'
424000070720      * se richiamato x aggiornamento finale dopo ORG e ORM  ?
424100070720      * quindi non solo per semplice allocazione del record. ?
424200070720     c                   if        aggior_dist = 'S'
424300070720     c                   exsr      campi_dist
424400070720     c                   else
424500070720      * qui l'update serve esclusivamente per tenere allocato  ?
424600070802      * il record in quanto nel CL FIOR60C x il COMMIT è    ?
424700070720      * stato attivato lo STRCMTCTL con *CHG.    ?
424800070720     c                   update    fidst000
424900070720     c                   endif
425000070720
425100070718     c                   endif
425200070718
425300070718     c                   endsr
425400080327
425500080327      *------------------------------------------------------------------------*
425600080327      *   Controlla esistenza fase 'RCH' sulla distinta
425700080327      *------------------------------------------------------------------------*
425800080327     c     sr_fase       begsr
425900080327
426000080415     c                   clear                   fior90ds
426100080327     c                   eval      or90tla = 'C'
426200080415     c                   eval      or90fas = 'RCH'
426300080327     c                   eval      or90fgs = wfgs
426400080327     c                   eval      or90ndc = wnfv
426500080327     c                   call      'FIOR90R'
426600080327     c                   parm                    kpjba
426700080327     c                   parm                    fior90ds
426800080415      * se non trovo la fase 'RCH' provo con la fase 'RQE'
426900080415     c                   if        or90ndcok = *blanks
427000080415     c                   clear                   fior90ds
427100080415     c                   eval      or90tla = 'C'
427200080415     c                   eval      or90fas = 'RQE'
427300080415     c                   eval      or90fgs = wfgs
427400080415     c                   eval      or90ndc = wnfv
427500080415     c                   call      'FIOR90R'
427600080415     c                   parm                    kpjba
427700080415     c                   parm                    fior90ds
427800080415     c                   endif
427900080327
428000080327     c                   endsr
428100080714
428200080714      *------------------------------------------------------------------------*
428300080714      *   Quadro i dati di PDO con ORM
428400080714      *------------------------------------------------------------------------*
428500080714     c     sr_quadrapdo  begsr
428600080714
428700080714     c                   clear                   fior57ds
428800080714     c                   eval      or57fgs = wfgs
428900080714     c                   eval      or57ndc = h1ndc_meno
429000080714     c                   call      'FIOR57R'
429100080714     c                   parm                    kpjba
429200080714     c                   parm                    fior57ds
429300080714
429400080714     c                   endsr
429500070720
429600070720      *------------------------------------------------------------------------*
429700070720      *  Imposta i campi della distinta da aggiornare             ?
429800070720      *------------------------------------------------------------------------*
429900070720     c     campi_dist    begsr
430000070720
430100070731     c                   clear                   fior67ds
430200070731     c                   eval      or67fgsi = wfgs
430300070731     c                   eval      or67dfvi = h1cddc
430400070731     c                   eval      or67nfvi = wnfv
430500070731      * non deve eseguire il Commit perchè lo faccio già in questo pgm
430600070731     c                   eval      or67cmti = 'N'
430700070731     c                   eval      kpjbu  = fior67ds
430800070731     c                   call      'FIOR67R'
430900070720     c                   parm                    kpjba
431000080611     c                   eval      fior67ds = kpjbu
431100070731     c                   if        or67erro = *blanks
431200070731     c                   eval      dstrpkla = or67rpklao
431300070731     c                   eval      dstrvlua = or67rvluao
431400070731     c                   eval      dstrstpa = or67rstpao
431500070731     c                   eval      dstrnbla = or67rnblao
431600070720     c                   else
431700070731      * problemi durante l'aggiornamento --> eseguire ROLLBACK  ?
431800070731     c                   eval      error_dist  = 'S'
431900070731     c                   endif
432000070720
432100070720     c                   endsr
432200070718
432300070718      *------------------------------------------------------------------------*
432400070718      *  Controlla validità dell'ORM
432500070718      *------------------------------------------------------------------------*
432600070718     c     check_orm     begsr
432700070718
432800070718      * per controllare se ORM assegnabile richiamo pgm esterno
432900070718     c                   clear                   fior64ds
433000070718     c                   eval      or64poei = ormpoe
433100070718     c                   eval      or64nsri = ormnsr
433200070718     c                   eval      or64nori = ormnor
433300070718     c                   eval      or64nrvi = ormnrv
433400070801      * imposto la data
433500070718     c                   eval      or64dfvi = h1cddc
433600070801      * se sono in simulazione controllo la distinta su ORG
433700070801      * e imposto il flag di controllo ORMETI in simulazione
433800070718     c                   if        test_simula = 'S'
433900070801     c                   eval      or64ndci = 'G'
434000070801     c                   eval      or64etii = 'S'
434100070801     c                   else
434200070801      * se già attivo controllo la distinta su entrambi
434300070801      * e imposto il flag di controllo ORMETI solo per filiale attiva
434400070801     c                   eval      or64ndci = 'E'
434500070801     c                   clear                   or64etii
434600070801     c                   endif
434700070801      * controlla la data di ritiro
434800070801     c                   eval      or64dari = 'S'
434900070801      * se ORM secco (da prima videata) non devo controllare la data ritiro
435000070801     c                   if        $ormsecco = *on
435100070801     c                   clear                   or64dari
435200070801     c                   endif
435300070718     c                   call      'FIOR64R'
435400070718     c                   parm                    kpjba
435500070718     c                   parm                    fior64ds
435600070718
435700070718     c                   endsr
435800070718
435900070718      *------------------------------------------------------------------------*
436000070718      *  Preparazione stringa sql
436100070718      *------------------------------------------------------------------------*
436200070718     c     sr_sqlorg     begsr
436300070718
436400070718     c                   clear                   stringasql
436500070718
436600070718      * preparo la stringa sql
436700070718     c                   eval      stringasql =
436800071025     c                             'select fnorg52j.* from '                  +
436900071025     c                             'fnorg52j '                                +
437000071025     c                             'where orgpor = '                          +
437100071025     c                              %editc(wfgs:'X') + ' '
437200070718      * se presente il giro seleziono solo quel giro
437300070718     c                   if        con_giro = 'S'
437400070718     c                   eval      stringasql =
437500070718     c                             %trim(stringasql) + ' '                     +
437600070718     c                             'and orgpocgi = ' + %editc(wfgs:'X') + ' '  +
437700070719     c                             'and orgcgi = ' + '''' + wcgi + ''''
437800070718     c                   endif
437900070718      * solo gli orm senza giro
438000070718     c                   if        con_giro = 'N'
438100070718     c                   eval      stringasql =
438200070718     c                             %trim(stringasql) + ' '                     +
438300070718     c                             'and orgpocgi = 0 '                         +
438400070719     c                             'and orgcgi = ' + '''' + wcgi + ''''
438500070718     c                   endif
438600070719      * se presente l'ORM seleziono solo quell'ORM
438700070719     c                   if        v1cnor <> *zeros
438800070719     c                   eval      stringasql =
438900070719     c                             %trim(stringasql) + ' '                     +
439000070719     c                             'and ormpoe = ' +  %editc(v1cpoe:'X') + ' ' +
439100070719     c                             'and ormnsr = ' +  %editc(v1nsr:'X') + ' '  +
439200070719     c                             'and ormnor = ' +  %editc(v1nor:'X') + ' '  +
439300070719     c                             'and ormnrv = ' +  %editc(v1cnrv:'X') + ' '
439400070719     c                   endif
439500070718      * se assegna
439600070718     c                   if        v1sce = '1'
439700071022      * solo gli ORM senza Serie
439800071022     c                   eval      stringasql =
439900071022     c                             %trim(stringasql) + ' '                     +
440000071022     c                             'and ormnsr = 0 '
440100071022      * se non è stato richiesto un ORM specifico
440200071022     c                   if        $ormsecco = *off
440300071022      * eclude ORM con allegati
440400071022     c                   if        v1all = 'E'
440500071022     c                   eval      stringasql =
440600071022     c                             %trim(stringasql) + ' '                     +
440700071022     c                             'and ormtor <> ''P'' '                      +
440800071022     c                             'and substr(ormflo, 9, 1) <> ''S'''
440900071022     c                   endif
441000071022      * include ORM con allegati
441100071022     c                   if        v1all = 'I'
441200071022     c                   eval      stringasql =
441300071022     c                             %trim(stringasql) + ' '                     +
441400071022     c                             'and (ormtor = ''P'' '                      +
441500071022     c                             'or substr(ormflo, 9, 1) = ''S'')'
441600071022     c                   endif
441700071022     c                   endif
441800070718      * se simulazione solo orgndc = 0
441900070718     c                   if        test_simula = 'S'
442000070718     c                   eval      stringasql =
442100070718     c                             %trim(stringasql) + ' '                     +
442200070718     c                             'and orgndc = 0'
442300070718     c                   endif
442400070718      * se filiale già attiva solo ormndc = 0
442500070718     c                   if        test_simula <> 'S'
442600070718     c                   eval      stringasql =
442700070718     c                             %trim(stringasql) + ' '                     +
442800070718     c                             'and ormndc = 0'
442900070718     c                   endif
443000070718      * ordinato per p.o. giro e giro
443100070718     c                   eval      stringasql =
443200070718     c                             %trim(stringasql) + ' '                     +
443300071025     c                             'order by orgpor, orgcgi'
443400070718     c                   endif
443500070718      * se toglie o trasferisce
443600070718     c                   if        v1sce = '2' or v1sce = '3'
443700070718      * se simulazione solo orgndc = 0
443800070718     c                   if        test_simula = 'S'
443900070718     c                   eval      stringasql =
444000070718     c                             %trim(stringasql) + ' '                     +
444100070718     c                             'and orgndc = ' + %editc(h1ndc_meno:'X')
444200070718     c                   endif
444300070718      * se filiale già attiva solo ormndc = 0
444400070718     c                   if        test_simula <> 'S'
444500070718     c                   eval      stringasql =
444600070718     c                             %trim(stringasql) + ' '                     +
444700070718     c                             'and ormndc = ' + %editc(h1ndc_meno:'X')
444800070718     c                   endif
444900070718     c                   endif
445000070718
445100070718     c                   endsr
445200070718
445300070718      *------------------------------------------------------------------------*
445400070718      *   Apro il cursore per ORM+ORG
445500070718      *------------------------------------------------------------------------*
445600070718     c     sr_sqlopen    begsr
445700070718
445800070718     C/EXEC SQL
445900070718     C+ PREPARE S2 FROM :stringasql
446000070718     C/END-EXEC
446100070718
446200070718     C/EXEC SQL
446300070718     C+ DECLARE A2 CURSOR FOR S2
446400070718     C/END-EXEC
446500070718
446600070718     C/EXEC SQL
446700070718     C+ OPEN A2
446800070718     C/END-EXEC
446900070718
447000070718     c                   endsr
447100070718
447200070718      *------------------------------------------------------------------------*
447300070718      *   Chiudo il cursore per ORM+ORG
447400070718      *------------------------------------------------------------------------*
447500070718     c     sr_sqlclose   begsr
447600070718
447700070718     C/EXEC SQL
447800070718     C+ CLOSE A2
447900070718     C/END-EXEC
448000070718
448100070718     c                   endsr
448200070621
448300070621      *------------------------------------------------------------------------*
448400070705      *   Seleziona tutti i record del sfl                         ?
448500070621      *------------------------------------------------------------------------*
448600070704     c     f01_1_sel     begsr
448700070621
448800070622     c                   if        wmax > *zeros
448900070622      * seleziona tutte le righe del sfl accendendo il sflnxtchg
449000070622     c                   eval      nrr_s1 = 1
449100070622     c     nrr_s1        chain     or60s01                            30
449200070622     c                   dow       not *in30
449300070622     c                   eval      vs1sce = '1'
449400070622
449500070622      * attivo sempre il sflnxtchg e aggiorno sfl
449600070622     c                   eval      *in22 = *on
449700070622     c                   update    or60s01
449800070622
449900070622     c                   eval      nrr_s1 = nrr_s1 + 1
450000070622     c     nrr_s1        chain     or60s01                            30
450100070622     c                   enddo
450200070622     c                   endif
450300070621
450400070621     c                   endsr
450500070622
450600070622      *------------------------------------------------------------------------*
450700070705      *   Seleziona tutti i record del sfl                         ?
450800070622      *------------------------------------------------------------------------*
450900070704     c     f01_2_sel     begsr
451000070622
451100070622     c                   if        wmax > *zeros
451200070622      * seleziona tutte le righe del sfl accendendo il sflnxtchg
451300070622     c                   eval      nrr_s2 = 1
451400070622     c     nrr_s2        chain     or60s02                            30
451500070622     c                   dow       not *in30
451600071022      * solo se non protetto il campo della scelta
451700071022     c                   if        vs2prot = *off
451800071022     c                   eval      vs2sce = '1'
451900071022     c                   endif
452000070921
452100080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
452200080314     c                   if        vs2comun = *on or vs2pda = *on
452300080314     c                   eval      *in08 = *on
452400080314     c                   else
452500080314     c                   eval      *in08 = *off
452600080314     c                   endif
452700071022      * protezione o meno campo scelta
452800071022     c                   eval      *in09 = vs2prot
452900080723      * imposto se filiale ha PDA
453000080723     c                   eval      *in17 = (pdaorm <> *blanks)
453100070622      * attivo sempre il sflnxtchg e aggiorno sfl
453200070622     c                   eval      *in22 = *on
453300071022      *                  __________________
453400070622     c                   update    or60s02
453500071022      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
453600070622     c                   eval      nrr_s2 = nrr_s2 + 1
453700070622     c     nrr_s2        chain     or60s02                            30
453800070622     c                   enddo
453900070622     c                   endif
454000070622
454100070622     c                   endsr
454200070719
454300070719      *------------------------------------------------------------------------*
454400070719      *   Gestione F02
454500070719      *------------------------------------------------------------------------*
454600070719     c     f02_intorm    begsr
454700070719
454800150625     c                   clear                   fior010ds
454900150625     c                   eval      IOR010ric = '1'
455000150625     c                   eval      IOR010por = v1cfgs
455100150625     c                   call      'FIOR010R'
455200070719     c                   parm                    kpjba
455300150625     c                   parm                    fior010ds
455400150625     c                   if        OOR010F03 = *blanks
455500150625     c                   eval      v1cpoe = OOR010poe
455600150625     c                   eval      v1nsr  = OOR010nsr
455700150625     c                   eval      v1nor  = OOR010nor
455800150625     c                   eval      v1cnrv = OOR010nrv
455900070719     c                   endif
456000070719
456100070719     c                   endsr
456200070718
456300070621      *------------------------------------------------------------------------*
456400070705      *   Gestione F03
456500070621      *------------------------------------------------------------------------*
456600070704     c     f03_esci      begsr
456700070621
456800070621     c                   eval      $fine = *on
456900070621
457000070621     c                   endsr
457100070621
457200070621      *------------------------------------------------------------------------*
457300070705      *   Gestione F04                                                ?
457400070621      *------------------------------------------------------------------------*
457500070704     c     f04_int       begsr
457600070621
457700070622     c                   eval      *in28 = *on
457800070919     c                   eval      wforzaspi = *off
457900070919
458000070622     c                   eval      wcgi = sav_sxcgi
458100070622     c                   clear                   wndc
458200070622     c                   eval      w1sce = '1'
458300070622     c                   if        v1sce = '3'
458400070622     c                   move      v1ndc3        wndc
458500070622     c                   endif
458600070704     c                   exsr      ric_dist
458700070622
458800070622      * controlla la distinta selezionata
458900070704     c                   z-add     fdgnfv        wnfv
459000070622     c                   eval      aggior_dist = 'N'
459100070704     c                   exsr      chain_dist
459200070622     c                   if        error_dist = 'S'
459300070622     c                   if        rec_allocato = 'S'
459400070622     c                   eval      $ndcalloc = *on
459500070622     c                   endif
459600070622     c                   eval      *in28 = *on
459700070622     c                   leavesr
459800070622     c                   endif
459900070622      * distinta non trovata
460000070622     c                   if        found_dist = 'N'
460100070622     c                   eval      $ndcerr = *on
460200070622     c                   eval      *in28 = *on
460300070622     c                   leavesr
460400070622     c                   endif
460500070622      * distinta chiusa o annullata
460600070622     c                   if        dstfcf = 'S' or dstatb <> *blanks
460700070622     c                   eval      $ndcchiu = *on
460800070622     c                   eval      *in28 = *on
460900070622     c                   leavesr
461000070622     c                   endif
461100100409      *     controllo esistenza fase 'RCH' o 'RQE'
461200100409     c                   exsr      sr_fase
461300100409      * se trova la fase (una delle due)
461400100409      * non si può più assegnare ORM a questa distinta
461500100409     c                   if        or90ndcok <> *blanks
461600100409     c                   eval      $ndcchiu = *on
461700100409     c                   eval      *in28 = *on
461800100409     c                   leavesr
461900100409     c                   endif
462000070622      * se tutto OK
462100070622     c                   eval      v1ndc1 = *zeros
462200070704     c                   move      fdgnfv        v1ndc1
462300070704     c                   eval      h1ndc_piu = fdgnfv
462400070720     c                   eval      h1pdr_piu = dstpdr
462500070720     c                   eval      h1fpp_piu = dstfpp
462600070919     c                   eval      h1spi_piu = dstrspi
462700070622
462800070622      * intestazione sf1
462900070622     c                   if        v1sce = '1' and h1ndc_piu > *zeros
463000070622     c                   eval      vc1ndc1 = 'alla_dist.:' +
463100080912     c                             %editc(h1ndc_piu:'Z')
463200070622     c                   eval      vc2dsce = 'ASSEGNA'
463300070622     c                   eval      vc2dess = 'alla Distinta nr.' +
463400070622     c                             %trim(%editc(h1ndc_piu:'Z'))
463500070622     c                   endif
463600070622
463700070622     c                   endsr
463800070621
463900070621      *------------------------------------------------------------------------*
464000070705      *   Gestione F05                                               ?
464100070621      *------------------------------------------------------------------------*
464200070704     c     f05_int       begsr
464300070621
464400070622     c                   eval      *in28 = *on
464500070919     c                   eval      wforzaspi = *off
464600070919
464700070622     c                   clear                   wndc
464800070622     c                   eval      w1sce = '1'
464900070622     c                   if        v1sce = '3'
465000070622     c                   move      v1ndc3        wndc
465100070622     c                   endif
465200070704     c                   exsr      ric_dist
465300070622
465400070622      * controlla la distinta selezionata
465500070704     c                   z-add     fdgnfv        wnfv
465600070622     c                   eval      aggior_dist = 'N'
465700070704     c                   exsr      chain_dist
465800070622     c                   if        error_dist = 'S'
465900070622     c                   if        rec_allocato = 'S'
466000070622     c                   eval      $ndcalloc = *on
466100070622     c                   endif
466200070622     c                   eval      *in28 = *on
466300070622     c                   leavesr
466400070622     c                   endif
466500070622      * distinta non trovata
466600070622     c                   if        found_dist = 'N'
466700070622     c                   eval      $ndcerr = *on
466800070622     c                   eval      *in28 = *on
466900070622     c                   leavesr
467000070622     c                   endif
467100070622      * distinta chiusa o annullata
467200070622     c                   if        dstfcf = 'S' or dstatb <> *blanks
467300070622     c                   eval      $ndcchiu = *on
467400070622     c                   eval      *in28 = *on
467500070622     c                   leavesr
467600070622     c                   endif
467700070622      * se tutto OK
467800070622     c                   eval      v1ndc1 = *zeros
467900070704     c                   move      fdgnfv        v1ndc1
468000070704     c                   eval      h1ndc_piu = fdgnfv
468100070720     c                   eval      h1pdr_piu = dstpdr
468200070720     c                   eval      h1fpp_piu = dstfpp
468300070919     c                   eval      h1spi_piu = dstrspi
468400070622
468500070622      * intestazione sf1
468600070622     c                   if        v1sce = '1' and h1ndc_piu > *zeros
468700070622     c                   eval      vc1ndc1 = 'alla_dist.:' +
468800080912     c                             %editc(h1ndc_piu:'Z')
468900070622     c                   eval      vc2dsce = 'ASSEGNA'
469000070622     c                   eval      vc2dess = 'alla Distinta nr.' +
469100070622     c                             %trim(%editc(h1ndc_piu:'Z'))
469200070622     c                   endif
469300070622
469400070621     c                   endsr
469500070719
469600070719      *------------------------------------------------------------------------*
469700070719      *   Gestione F06                                                ?
469800070719      *------------------------------------------------------------------------*
469900070719     c     f06_aggiorna  begsr
470000070731      *
470100070731      * Leggendo sempre gli orm selezionati dal 2°SFL
470200070731      * esegue aggiornamento degli archivi e se ci sono problemi
470300070731      * durante l'aggiornamento dei singoli orm emette 3°SFL con la
470400070731      * lista degli orm non aggiornati avvertendo del mancato aggiornamento.
470500070731
470600070731      * L'utente deve quindi rieseguire l'operazione poichè viene annullata tutta
470700070731      * la transazione con il Rolback.
470800070731
470900070731      * Esegue l'aggiornamento ed il controllo sul 3° SFL
471000070731     c                   eval      $gest = 'S3'
471100070731     c                   eval      $inzs3 = *on
471200070731
471300070719     c                   endsr
471400071119
471500071119      *------------------------------------------------------------------------*
471600071119      *   Gestione F07
471700071119      *------------------------------------------------------------------------*
471800071119     c     f07_proposte  begsr
471900071119
472000071119     c                   clear                   fior20ds
472100071119     c                   eval      i20fgs = v1cfgs
472200071119     c                   eval      i20data = v1cddc
472300071119     c                   call      'FIOR20R'
472400071119     c                   parm                    kpjba
472500071119     c                   parm                    fior20ds
472600071119
472700071119     c                   endsr
472800070621
472900070621      *------------------------------------------------------------------------*
473000070705      *   Calcolo pagina fino a cui deve essere ricaricato il sfl   ?
473100070621      *------------------------------------------------------------------------*
473200070704     c     clcpag        begsr
473300070621
473400070621      * input :
473500070621      * - wsfl = numero dell'ultimo rcd su cui era posizionato il
473600070621      *          cursore
473700070621      * - wpagine= numero rcd per pagina sfl
473800070621      * output :
473900070621      * - wpag = pagina fino a cui deve essere ricaricato il sfl
474000070621
474100070621     c                   eval      pagine = %div(wsfl:wpagine)
474200070621     c                   eval      resto = %rem(wsfl:wpagine)
474300070621     c                   if        resto > *zeros
474400070621     c                   eval      pagine = pagine + 1
474500070621     c                   endif
474600070621     c                   eval      wpag = pagine * wpagine
474700070621
474800070621     c                   endsr
474900070622
475000070622      *------------------------------------------------------------------------*
475100070705      *   Riordina comunque il sfl                                     ?
475200070622      *------------------------------------------------------------------------*
475300070704     c     clear_ordina  begsr
475400070620
475500070622     c                   eval      posizionamento = 'N'
475600070622     c                   eval      posizionato    = 'N'
475700070622     c                   clear                   vc2pos1
475800070622     c                   clear                   vc2pos2
475900070622     c                   clear                   vc2pos3
476000070622     c                   clear                   vc2pos4
476100070622     c                   clear                   vc2pos5
476200070622     c                   clear                   vc2pos6
476300070622     c                   clear                   sav_pos1
476400070622     c                   clear                   sav_pos2
476500070622     c                   clear                   sav_pos3
476600070622     c                   clear                   sav_pos4
476700070622     c                   clear                   sav_pos5
476800070622     c                   clear                   sav_pos6
476900070622
477000070622     c                   endsr
477100070622
477200070622      *------------------------------------------------------------------------*
477300070705      *   Riordina comunque il sfl                                     ?
477400080611      *   se richiamato dal MONITOR deve visionare prima gli ORM con INFO
477500070622      *------------------------------------------------------------------------*
477600070704     c     ordina_sfl2   begsr
477700070705
477800070705      * totale records caricati nel 2°SFL
477900070705     c                   eval      wsfl = 1
478000070705
478100070719      * se richiesto ordinamento sul SFL da operatore
478200070705     c                   setoff                                       010203
478300070705     c                   setoff                                       040506
478400070920     c                   setoff                                       0708
478500070705
478600070705      * inizializza i campi chiave x l'ordinamento. C'è un campo in più non
478700070802      * presente nel subfile -- "Selected"?-- questo è aggiunto al record.
478800070705      * il campo è usato per selezionare i records dando un ordine a quelli
478900070705      * selezionati davanti ai non selezionati.
479000070802     c                   clear                   QLGSCB
479100070802     c                   clear                   QLGSCB00
479200070705
479300070705     c                   select
479400070705
479500080612      *?  Ordinamento per GIRO/Mittente  ?
479600070705     c                   when      wrksort = ord_giromit
479700070705     c                   move      ord_giromit   windsort
479800070705
479900070705      * x giro (illumina il campo) con lo 07 = *on
480000070705     c                   eval      *in07 = *on
480100080611
480200080612      *? se richiamato dal MONITOR ordino per INFO
480300080611     c                   if        pgm_chiamante = 'MONITOR'
480400080612     c                   eval      QLGNBRK    = 3
480500080611
480600080612      * il flag INFO è in posizione (xx) xx Bytes char descending
480700080611     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
480800080611     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
480900131119     c                             %size(vs2vlm)+ %size(vs2ora)
481000080611     c                   eval      QLGSS    = %SIZE(vs2info)
481100080611     c                   eval      QLGDT    = Carattere
481200080611     c                   eval      QLGSO    = Discendente
481300080612     c                   eval      QLGKL(1) = QLGSKL
481400080611
481500080612      * il GIRO è in posizione (1) xx Bytes char ascending
481600080611     c                   eval      QLGSP      = 1
481700080611     c                   eval      QLGSS      = %SIZE(vs2cgi)
481800080611     c                   eval      QLGDT      = Carattere
481900080611     c                   eval      QLGSO      = Ascendente
482000080612     c                   eval      QLGKL(2)   = QLGSKL
482100080611
482200080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
482300080611     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
482400080611     c                   eval      QLGSS    = %SIZE(vs2rsr)
482500080611     c                   eval      QLGDT    = Carattere
482600080611     c                   eval      QLGSO    = Ascendente
482700080612     c                   eval      QLGKL(3) = QLGSKL
482800080611
482900080612      *? non richiamato dal MONITOR
483000080611     c                   else
483100070705
483200070705      * 2 campi chiave.   x GIRO e MITTENTE
483300080612     c                   eval      QLGNBRK    = 2
483400080612
483500080612      * il GIRO è in posizione (1) xx Bytes char ascending
483600070802     c                   eval      QLGSP      = 1
483700070802     c                   eval      QLGSS      = %SIZE(vs2cgi)
483800070802     c                   eval      QLGDT      = Carattere
483900070802     c                   eval      QLGSO      = Ascendente
484000080612     c                   eval      QLGKL(1)   = QLGSKL
484100070705
484200080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
484300070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
484400070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
484500070802     c                   eval      QLGDT    = Carattere
484600070802     c                   eval      QLGSO    = Ascendente
484700080612     c                   eval      QLGKL(2) = QLGSKL
484800080611
484900080611     c                   endif
485000070705
485100080612      *?  Ordinamento per Mittente  ?
485200070705     c                   when      wrksort = ord_mittente
485300070705     c                   move      ord_mittente  windsort
485400080612
485500080612      *? se richiamato dal MONITOR ordino per INFO
485600080612     c                   if        pgm_chiamante = 'MONITOR'
485700080612     c                   eval      QLGNBRK    = 3
485800080612
485900080612      * La Priorità se richiesta ricerca di stringa
486000080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) + %SIZE(vs2rsr) +
486100080612     c                             %SIZE(vs2inr)+ %SIZE(vs2lor)+ %SIZE(vs2pkg) +
486200131119     c                             %SIZE(vs2vlm)+ %size(vs2ora)+ %size(vs2info)
486300080612     c                   eval      QLGSS    = %SIZE(h2subst)
486400080612     c                   eval      QLGDT    = Numerico
486500080612     c                   eval      QLGSO    = Discendente
486600080612     c                   eval      QLGKL(1) = QLGSKL
486700080612
486800080612      * il flag info è in posizione (xx) xx Bytes char descending
486900080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
487000080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
487100131119     c                             %size(vs2vlm)+ %size(vs2ora)
487200080612     c                   eval      QLGSS    = %SIZE(vs2info)
487300080612     c                   eval      QLGDT    = Carattere
487400080612     c                   eval      QLGSO    = Discendente
487500080612     c                   eval      QLGKL(2) = QLGSKL
487600080612
487700080612      * il mittente è in posizione (xx) xx Bytes char ascending
487800080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
487900080612     c                   eval      QLGSS    = %SIZE(vs2rsr)
488000080612     c                   eval      QLGDT    = Carattere
488100080612     c                   eval      QLGSO    = Ascendente
488200080612     c                   eval      QLGKL(3) = QLGSKL
488300080612
488400080612      *? non richiamato dal MONITOR
488500080612     c                   else
488600070705
488700070705      * 2 campi chiave.   x substring e mittente
488800070802     c                   eval      QLGNBRK    = 2
488900070705
489000080612      * La Priorità se richiesta ricerca di stringa
489100070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) + %SIZE(vs2rsr) +
489200080128     c                             %SIZE(vs2inr)+ %SIZE(vs2lor)+ %SIZE(vs2pkg) +
489300131119     c                             %SIZE(vs2vlm)+ %size(vs2ora)+ %size(vs2info)
489400070802     c                   eval      QLGSS    = %SIZE(h2subst)
489500070802     c                   eval      QLGDT    = Numerico
489600070802     c                   eval      QLGSO    = Discendente
489700070802     c                   eval      QLGKL(1) = QLGSKL
489800070705
489900080612      * il mittente è in posizione (xx) xx Bytes char ascending
490000070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
490100070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
490200070802     c                   eval      QLGDT    = Carattere
490300070802     c                   eval      QLGSO    = Ascendente
490400070802     c                   eval      QLGKL(2) = QLGSKL
490500080612
490600080612     c                   endif
490700070705
490800080612      *?  Ordinamento per Località/Mittente  ?
490900070705     c                   when      wrksort = ord_localita
491000070705     c                   move      ord_localita  windsort
491100080612
491200080612      *? se richiamato dal MONITOR ordino per INFO
491300080612     c                   if        pgm_chiamante = 'MONITOR'
491400080612     c                   eval      QLGNBRK    = 4
491500080612
491600080612      * La Priorità se richiesta ricerca di stringa
491700080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) + %SIZE(vs2rsr) +
491800080612     c                             %SIZE(vs2inr)+ %SIZE(vs2lor)+ %SIZE(vs2pkg) +
491900131119     c                             %SIZE(vs2vlm)+ %size(vs2ora)+ %size(vs2info)
492000080612     c                   eval      QLGSS    = %SIZE(h2subst)
492100080612     c                   eval      QLGDT    = Numerico
492200080612     c                   eval      QLGSO    = Discendente
492300080612     c                   eval      QLGKL(1) = QLGSKL
492400080612
492500080612      * il flag info è in posizione (xx) xx Bytes char descending
492600080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
492700080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
492800131119     c                             %size(vs2vlm)+ %size(vs2ora)
492900080612     c                   eval      QLGSS    = %SIZE(vs2info)
493000080612     c                   eval      QLGDT    = Carattere
493100080612     c                   eval      QLGSO    = Discendente
493200080612     c                   eval      QLGKL(2) = QLGSKL
493300080612
493400080612      * La LOCALITA' è in posizione (xx) xx Bytes char ascending
493500080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
493600080612     c                                           %SIZE(vs2rsr) +
493700080612     c                                           %SIZE(vs2inr)
493800080612     c                   eval      QLGSS    = %SIZE(vs2lor)
493900080612     c                   eval      QLGDT    = Carattere
494000080612     c                   eval      QLGSO    = Ascendente
494100080612     c                   eval      QLGKL(3) = QLGSKL
494200080612
494300080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
494400080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
494500080612     c                   eval      QLGSS    = %SIZE(vs2rsr)
494600080612     c                   eval      QLGDT    = Carattere
494700080612     c                   eval      QLGSO    = Ascendente
494800080612     c                   eval      QLGKL(4) = QLGSKL
494900080612
495000080612      *? non richiamato dal MONITOR
495100080612     c                   else
495200070705
495300070705      * 3 campi chiave.   x LOCALITA'/MITTENTE
495400070802     c                   eval      QLGNBRK    = 3
495500070705
495600080612      * La Priorità se richiesta ricerca di stringa
495700070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) + %SIZE(vs2rsr) +
495800080128     c                             %SIZE(vs2inr)+ %SIZE(vs2lor)+ %SIZE(vs2pkg) +
495900131119     c                             %SIZE(vs2vlm)+ %size(vs2ora)+ %size(vs2info)
496000070802     c                   eval      QLGSS    = %SIZE(h2subst)
496100070802     c                   eval      QLGDT    = Numerico
496200070802     c                   eval      QLGSO    = Discendente
496300070802     c                   eval      QLGKL(1) = QLGSKL
496400070705
496500080612      * La LOCALITA' è in posizione (xx) xx Bytes char ascending
496600070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
496700070705     c                                           %SIZE(vs2rsr) +
496800080128     c                                           %SIZE(vs2inr)
496900070802     c                   eval      QLGSS    = %SIZE(vs2lor)
497000070802     c                   eval      QLGDT    = Carattere
497100070802     c                   eval      QLGSO    = Ascendente
497200070802     c                   eval      QLGKL(2) = QLGSKL
497300070705
497400080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
497500070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
497600070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
497700070802     c                   eval      QLGDT    = Carattere
497800070802     c                   eval      QLGSO    = Ascendente
497900070802     c                   eval      QLGKL(3) = QLGSKL
498000080612
498100080612     c                   endif
498200080612
498300080612      *?  Ordinamento per Peso/Mittente  ?
498400070705     c                   when      wrksort = ord_peso
498500070705     c                   move      ord_peso      windsort
498600080612
498700080612      *? se richiamato dal MONITOR ordino per INFO
498800080612     c                   if        pgm_chiamante = 'MONITOR'
498900080612     c                   eval      QLGNBRK    = 3
499000080612
499100080612      * il flag info è in posizione (xx) xx Bytes char descending
499200080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
499300080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
499400131119     c                             %size(vs2vlm)+ %size(vs2ora)
499500080612     c                   eval      QLGSS    = %SIZE(vs2info)
499600080612     c                   eval      QLGDT    = Carattere
499700080612     c                   eval      QLGSO    = Discendente
499800080612     c                   eval      QLGKL(1) = QLGSKL
499900080612
500000080612      * il PESO è in posizione (xx) xx Digits Num descending
500100080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
500200080612     c                                           %SIZE(vs2rsr) +
500300080612     c                                           %SIZE(vs2inr) +
500400080612     c                                           %SIZE(vs2lor)
500500080612     c                   eval      QLGSS    = %SIZE(vs2pkg)
500600080612     c                   eval      QLGDT    = Numerico
500700080612     c                   eval      QLGSO    = Discendente
500800080612     c                   eval      QLGKL(2) = QLGSKL
500900080612
501000080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
501100080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
501200080612     c                   eval      QLGSS    = %SIZE(vs2rsr)
501300080612     c                   eval      QLGDT    = Carattere
501400080612     c                   eval      QLGSO    = Ascendente
501500080612     c                   eval      QLGKL(3) = QLGSKL
501600080612
501700080612      *? non richiamato dal MONITOR
501800080612     c                   else
501900070705
502000070705      * 2 campi chiave.   x PESO/MITTENTE
502100080612     c                   eval      QLGNBRK    = 2
502200070705
502300080612      * il PESO è in posizione (xx) xx Digits Num descending
502400070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
502500070705     c                                           %SIZE(vs2rsr) +
502600080128     c                                           %SIZE(vs2inr) +
502700070705     c                                           %SIZE(vs2lor)
502800070802     c                   eval      QLGSS    = %SIZE(vs2pkg)
502900070802     c                   eval      QLGDT    = Numerico
503000070802     c                   eval      QLGSO    = Discendente
503100080612     c                   eval      QLGKL(1) = QLGSKL
503200070705
503300080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
503400070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
503500070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
503600070802     c                   eval      QLGDT    = Carattere
503700070802     c                   eval      QLGSO    = Ascendente
503800080612     c                   eval      QLGKL(2) = QLGSKL
503900080612
504000080612     c                   endif
504100070705
504200080612      *?  Ordinamento per Volume/Mittente  ?
504300070705     c                   when      wrksort = ord_volume
504400070705     c                   move      ord_volume    windsort
504500080612
504600080612      *? se richiamato dal MONITOR ordino per INFO
504700080612     c                   if        pgm_chiamante = 'MONITOR'
504800080612     c                   eval      QLGNBRK    = 3
504900080612
505000080612      * il flag info è in posizione (xx) xx Bytes char descending
505100080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
505200080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
505300131119     c                             %size(vs2vlm)+ %size(vs2ora)
505400080612     c                   eval      QLGSS    = %SIZE(vs2info)
505500080612     c                   eval      QLGDT    = Carattere
505600080612     c                   eval      QLGSO    = Discendente
505700080612     c                   eval      QLGKL(1) = QLGSKL
505800080612
505900080612      * il VOLUME è in posizione (xx) xx Digits Num descending
506000080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
506100080612     c                                           %SIZE(vs2rsr) +
506200080612     c                                           %SIZE(vs2inr) +
506300080612     c                                           %SIZE(vs2lor) +
506400080612     c                                           %SIZE(vs2pkg)
506500080612     c                   eval      QLGSS    = %SIZE(vs2vlm)
506600080612     c                   eval      QLGDT    = Numerico
506700080612     c                   eval      QLGSO    = Discendente
506800080612     c                   eval      QLGKL(2) = QLGSKL
506900080612
507000080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
507100080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
507200080612     c                   eval      QLGSS    = %SIZE(vs2rsr)
507300080612     c                   eval      QLGDT    = Carattere
507400080612     c                   eval      QLGSO    = Ascendente
507500080612     c                   eval      QLGKL(3) = QLGSKL
507600080612
507700080612      *? non richiamato dal MONITOR
507800080612     c                   else
507900070705
508000070705      * 2 campi chiave.   x VOLUME/MITTENTE
508100080612     c                   eval      QLGNBRK    = 2
508200070705
508300080612      * il VOLUME è in posizione (xx) xx Digits Num descending
508400070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi) +
508500070705     c                                           %SIZE(vs2rsr) +
508600080128     c                                           %SIZE(vs2inr) +
508700070705     c                                           %SIZE(vs2lor) +
508800070705     c                                           %SIZE(vs2pkg)
508900070802     c                   eval      QLGSS    = %SIZE(vs2vlm)
509000070802     c                   eval      QLGDT    = Numerico
509100070802     c                   eval      QLGSO    = Discendente
509200080612     c                   eval      QLGKL(1) = QLGSKL
509300070705
509400080612      * il MITTENTE è in posizione (xx) xx Bytes char ascending
509500070802     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)
509600070802     c                   eval      QLGSS    = %SIZE(vs2rsr)
509700070802     c                   eval      QLGDT    = Carattere
509800070802     c                   eval      QLGSO    = Ascendente
509900080612     c                   eval      QLGKL(2) = QLGSKL
510000080612
510100080612     c                   endif
510200080128
510300080612      *?  Ordinamento per Ora/Giro  ?
510400080128     c                   when      wrksort = ord_ora
510500080128     c                   move      ord_ora       windsort
510600080612
510700080612      *? se richiamato dal MONITOR ordino per INFO
510800080612     c                   if        pgm_chiamante = 'MONITOR'
510900080612     c                   eval      QLGNBRK    = 3
511000080612
511100080612      * il flag info è in posizione (xx) xx Bytes char descending
511200080612     c                   eval      QLGSP   = 1 + %size(vs2cgi) + %size(vs2rsr) +
511300080612     c                             %size(vs2inr)+ %size(vs2lor)+ %size(vs2pkg) +
511400131119     c                             %size(vs2vlm)+ %size(vs2ora)
511500080612     c                   eval      QLGSS    = %SIZE(vs2info)
511600080612     c                   eval      QLGDT    = Carattere
511700080612     c                   eval      QLGSO    = Discendente
511800080612     c                   eval      QLGKL(1) = QLGSKL
511900080612
512000080612      * l' ORA è in posizione (xx) xx digits Num descending
512100080612     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)  +
512200080612     c                                           %SIZE(vs2rsr) +
512300080612     c                                           %SIZE(vs2inr) +
512400080612     c                                           %SIZE(vs2lor) +
512500080612     c                                           %SIZE(vs2pkg) +
512600080612     c                                           %SIZE(vs2vlm)
512700131119     c                   eval      QLGSS    = %SIZE(vs2ora)
512800080612     c                   eval      QLGDT    = Numerico
512900131119     c                   eval      QLGSO    = Ascendente
513000080612     c                   eval      QLGKL(2) = QLGSKL
513100080612
513200080612      * il GIRO è in posizione (1) xx Bytes char ascending
513300080612     c                   eval      QLGSP   = 1
513400080612     c                   eval      QLGSS    = %SIZE(vs2cgi)
513500080612     c                   eval      QLGDT    = Carattere
513600080612     c                   eval      QLGSO    = Ascendente
513700080612     c                   eval      QLGKL(3) = QLGSKL
513800080612
513900080612      *? non richiamato dal MONITOR
514000080612     c                   else
514100080128
514200080128      * 2 campi chiave.   x ORA/GIRO
514300080612     c                   eval      QLGNBRK    = 2
514400080128
514500080612      * l' ORA è in posizione (xx) xx digits Num descending
514600080128     c                   eval      QLGSP   = 1 + %SIZE(vs2cgi)  +
514700080128     c                                           %SIZE(vs2rsr) +
514800080128     c                                           %SIZE(vs2inr) +
514900080128     c                                           %SIZE(vs2lor) +
515000080128     c                                           %SIZE(vs2pkg) +
515100080128     c                                           %SIZE(vs2vlm)
515200131119     c                   eval      QLGSS    = %SIZE(vs2ora)
515300080128     c                   eval      QLGDT    = Numerico
515400131119     c                   eval      QLGSO    = Ascendente
515500080612     c                   eval      QLGKL(1) = QLGSKL
515600080128
515700080612      * il GIRO è in posizione (1) xx Bytes char ascending
515800080128     c                   eval      QLGSP   = 1
515900080128     c                   eval      QLGSS    = %SIZE(vs2cgi)
516000080128     c                   eval      QLGDT    = Carattere
516100080128     c                   eval      QLGSO    = Ascendente
516200080612     c                   eval      QLGKL(2) = QLGSKL
516300080612
516400080612     c                   endif
516500070705
516600070705     c                   endsl
516700070705
516800070705      * Load other sort parameters.
516900070802     c                   eval      QLGLB     = 80 + 16 * MaxKey
517000070802     c                   eval      QLGRL     = %SIZE(sflrcd) - 1
517100070802     c                   eval      QLGRT     = 8
517200070802     c                   eval      QLGOKL    = 80
517300070802     c                   eval      QLGLKE    = 16
517400070802     c                   eval      QLGLSS    = 290
517500070705
517600070705      * Initialize Sort I/O API fields.
517700070802     c                   eval      QLGRL00  = QLGRL
517800070802     c                   eval      QLGRC00  = 1
517900070802     c                   clear                   QUSEI
518000070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
518100070705
518200070705      * First step - Initialize the sort routine.
518300070802     c                   call      'QLGSORT'
518400070802     c                   parm                    QLGSCB
518500070802     c                   parm                    NotUsed
518600070802     c                   parm                    NotUsed
518700070802     c                   parm                    SizeList
518800070802     c                   parm                    ReturnSize
518900070802     c                   parm                    QUSEC
519000070705
519100070705      * Next step - Write records to I/O routine.
519200070802     c                   eval      QLGRT00 = Put
519300070705
519400070705     c                   do        rrnlast       s2nrr_xx
519500070705     c     s2nrr_xx      chain     or60s02
519600070705
519700070705      * solo le righe con Selected = 'Y' sono riordinate,
519800070705      * quindi per fare un ordinamento di tutte le righe
519900070705      * metto 'Y' sempre.
520000070705     c                   eval      selected  = 'Y'
520100070705
520200070802     c                   clear                   QUSEI
520300070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
520400070705
520500070802     c                   call      'QLGSRTIO'
520600070802     c                   parm                    QLGSCB00
520700070802     c                   parm                    SflRcd
520800070802     c                   parm                    NotUsed
520900070802     c                   parm                    SizeList
521000070802     c                   parm                    NotUsed
521100070802     c                   parm                    QUSEC
521200070705
521300070705     c                   enddo
521400070705
521500070705      * Next step - Signal end of input, clear subfile for reload.
521600070802     c                   eval      QLGRT00 = EndPut
521700070802     c                   clear                   QUSEI
521800070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
521900070705
522000070802     c                   call      'QLGSRTIO'
522100070802     c                   parm                    QLGSCB00
522200070802     c                   parm                    SflRcd
522300070802     c                   parm                    NotUsed
522400070802     c                   parm                    SizeList
522500070802     c                   parm                    NotUsed
522600070802     c                   parm                    QUSEC
522700070802      * pulizia SFL 2  ?
522800070705     c                   eval      *in20 = *off
522900070705     c                   eval      *in21 = *off
523000070705      *                  __________________
523100070705     c                   write     or60c02
523200070705      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
523300070705     c                   eval      *in21 = *on
523400070705
523500070802      * Campo di Ordinamento in REVERSE ?
523600070705     c                   if        windsort  > 0
523700070705     c                   move      *on           *in(windsort)
523800070705     c                   endif
523900070705
524000070802      * Campo di Posizionamento  ?
524100070705     c   01              eval      vc2desp = '     Posiz. x GIRO :'
524200070705     c   02              eval      vc2desp = 'Scans. x MITTENTE  :'
524300080128     c   03              eval      vc2desp = '      Posiz. x ORA :'
524400070705     c   04              eval      vc2desp = 'Scans. x  LOCALITA :'
524500070705     c   05              eval      vc2desp = '   Posiz. x VOLUME :'
524600070705     c   06              eval      vc2desp = '     Posiz. x PESO :'
524700070705
524800070705      * Final step - Write the records back to the subfile.
524900070802     c                   eval      QLGRT00 = Get
525000070705
525100070705     c                   do        rrnlast       s2nrr_xx
525200070705     c                   eval      s2nrr = s2nrr_xx
525300070802     c                   clear                   QUSEI
525400070802     c                   eval      QUSBPRV = %SIZE(QUSEC)
525500070802     c                   call      'QLGSRTIO'
525600070802     c                   parm                    QLGSCB00
525700070802     c                   parm                    NotUsed
525800070802     c                   parm                    SflRcd
525900070802     c                   parm                    QLGRL00
526000070802     c                   parm                    NotUsed
526100070802     c                   parm                    QUSEC
526200070705      * SFLnxtCHG
526300070705     c                   if        vs2sce <> *blank
526400070705     c                   eval      *in22 = *on
526500070705     c                   else
526600070705     c                   eval      *in22 = *off
526700070705     c                   endif
526800080314      * se ORM comunicato o se ORM scaricato a PDA sottolineo il mittente
526900080314     c                   if        vs2comun = *on or vs2pda = *on
527000080314     c                   eval      *in08 = *on
527100080314     c                   else
527200080314     c                   eval      *in08 = *off
527300080314     c                   endif
527400071022      * protezione o meno campo scelta
527500071022     c                   eval      *in09 = vs2prot
527600080723      * imposto se filiale ha PDA
527700080723     c                   eval      *in17 = (pdaorm <> *blanks)
527800070705      *                  __________________
527900070705     c                   write     or60s02
528000070705      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
528100070705     c                   enddo
528200070705
528300070705      *  All'uscita di queste chiamate a routine di sistema il SFL record
528400070705      *   si incrementa stranamente quindi lo reimposto correttamente
528500070705      *    prima di lasciare la routine.
528600070705     c                   eval      s2nrr = rrnlast
528700070705
528800070705     c                   endsr
528900080612
529000080612      *------------------------------------------------------------------------*
529100080612      *   Aggiorno il flag INFO su Fipro
529200080612      *------------------------------------------------------------------------*
529300140211      /free
529400140211       begsr sr_chiuinfo;
529500140211
529600140211       //Solo se doveva essere Visionato:
529700140211       // significa che comunque c'era il flag INFO
529800140211
529900140211
530000140211        if vs2info <> ' ' ;
530100140211         clear fior82ds;
530200140211         c82fgs = wfgs;
530300140211         c82ndc = h1ndc_meno ;
530400140211         c82codaut = h1pdr_meno;
530500140211         c82POE = vs2poe;
530600140211         c82NSR = vs2nsr;
530700140211         c82NOR = vs2nor;
530800140211         c82NRV = vs2nrv;
530900140211         c82aggvis = 'S';
531000140211         kpjbu = fior82ds;
531100140211         fior82r(kpjba);
531200140211         fior82ds = kpjbu;
531300140211        endif;
531400080612
531500140211       endsr;
531600070622
531700140211       // ----------------------------------------------------------------------
531800140211       //?Visualizzo le spunte
531900140211       // ----------------------------------------------------------------------
532000140211       begsr sr_visinfo ;
532100140211
532200140211         clear NOTINFOr;
532300140211           WSPED =  %editc(vs2poe:'X') + '/' + %editc(vs2nsr:'X') + '/' +
532400140211                    %editc(vs2Nor:'X') + '/' + %editc(vs2Nrv:'X');
532500140211
532600140211          dataiso = %date(vs2dao:*iso);
532700140211          dataeur = dataiso;
532800140211          WDEL   = %dec(dataeur);
532900140211
533000140211           WDEST = Vs2RAG;
533100140211            if   vs2INFo <> 'S ';
533200140211                 tbeCOD = 'RAS';
533300140211                 tbeKE1 = vs2INFo;
533400140211                 chain (tbecod:tbeke1) tntbe01l;
533500140211                 clear WDcodInfo;
533600140211                   if  %found(tntbe01l);
533700140211                      WDcodInfo = tbeUNI;
533800140211                   endif;
533900140211                 WCODINFO= %trim(vs2INFo)  + ': ' + WdcodInfo;
534000140211                 WNOTASS = vh2notinfo;
534100140211             endif;
534200140211           exfmt NOTINFOr;
534300140211
534400140211       endsr;
534500140211
534600140211      /end-free
534700070621      *------------------------------------------------------------------------*
534800070705      *   Controllo e decodifica giro
534900070621      *------------------------------------------------------------------------*
535000070704     c     ctrgiro       begsr
535100070620
535200070620     c                   clear                   fidg09ds
535300070620     c                   eval      d09iop0 = '001'
535400070620     c                   eval      d09ifgs = wfgs
535500071011     c                   eval      d09idat = data_oggi
535600070620     c                   eval      d09icgi = savcgi
535700070704     c                   eval      d09itug = 'R'
535800070620     c                   eval      kpjbu = fidg09ds
535900070802     c                   call      'FIDG09R'
536000070620     c                   parm                    kpjba
536100070620     c                   eval      fidg09ds = kpjbu
536200070620     c                   eval      dgrgflo = d09oflo
536300070620
536400070620     c                   endsr
536500070619
536600070620      *------------------------------------------------------------------------*
536700070705      *   Operazioni iniziali      ?
536800070620      *------------------------------------------------------------------------*
536900070619     c     *inzsr        begsr
537000070619
537100070619     c     *entry        plist
537200070619     c                   parm                    kpjba
537300080610
537400080610      * controllo se il programma è richiamato dal MONITOR x il PDA
537500080610     c                   eval      *in16 = *off
537600080610     c                   clear                   pgm_chiamante
537700080610     c                   clear                   fior60ds
537800080610     c                   if        %subst(kpjbu:1:10) <> *blanks
537900080612     c                   eval      fior60ds = kpjbu
538000080610      * se si deve cambiare nome al MONITOR basta modificarlo solo qui
538100080610     c                   if        o60pgm = 'MONITOR'
538200080610     c                   eval      pgm_chiamante = 'MONITOR'
538300080610     c                   eval      salta_video1 = 'S'
538400080610     c                   eval      *in16 = *on
538500080610     c                   else
538600080610     c                   eval      pgm_chiamante = o60pgm
538700080610     c                   endif
538800080610     c                   endif
538900070619
539000070619     c     *dtaara       define    §azute        azuteds
539100070619     c     *dtaara       define    §datiute      ddatiute
539200070619     c                   in(e)     *dtaara
539300070704     c                   if        %error or rsut = *blanks
539400070619     c                   clear                   tibs34ds
539500070619     c                   call      'TIBS34R'
539600070619     c                   parm                    tibs34ds
539700070619     c                   in        *dtaara
539800070619     c                   endif
539900070619
540000070619      * imposto la data del giorno
540100071011     c                   time                    w0140
540200071011     c                   move      w0140         oggi
540300071011     c                   clear                   wlbdat
540400071011     c                   move      w0140         g08dat
540500071011     c                   call      'XSRDA8'
540600071011     c                   parm                    wlbdat
540700071011     c                   z-add     g08inv        data_oggi
540800071011
540900071011      * recupero da tabella DGD i limiti per controllo data distinta
541000071011     c                   clear                   ddgd
541100071011     c                   clear                   tibs02ds
541200071011     c                   eval      t02mod = 'C'
541300071011     c                   eval      t02sif = knsif
541400071011     c                   eval      t02cod = 'DGD'
541500071011     c                   eval      t02ke1 = '1'
541600071011     c                   call      'TIBS02R'
541700071011     c                   parm                    kpjba
541800071011     c                   parm                    tibs02ds
541900071011     c                   eval      ddgd = t02uni
542000070704
542100070704      * gestione videate
542200070704     c                   eval      $gest = 'D1'
542300070704     c                   eval      $inzd1 = *on
542400070619
542500070619     c     kdst          klist
542600070619     c                   kfld                    wnpg
542700070619     c                   kfld                    wnfv
542800070619     c                   kfld                    wfgs
542900070619
543000070619     c     kdst8         klist
543100070619     c                   kfld                    wfgs
543200070619     c                   kfld                    wnpg
543300070619     c                   kfld                    wddc
543400070719
543500070719     c     korm          klist
543600070719     c                   kfld                    v1cpoe
543700070719     c                   kfld                    v1nsr
543800070719     c                   kfld                    v1nor
543900070719     c                   kfld                    v1cnrv
544000070719
544100070719     c     kapd          klist
544200070719     c                   kfld                    apdtip
544300070719     c                   kfld                    dstpdr
544400070720
544500070720     c     kaggiorna     klist
544600070720     c                   kfld                    vs2poe
544700070720     c                   kfld                    vs2nsr
544800070720     c                   kfld                    vs2nor
544900070720     c                   kfld                    vs2nrv
545000080611
545100070719     c                   eval      apdtip = 'A'
545200080610
545300080610     c                   eval      kpjbusav = kpjbu
545400070620
545500070619     c                   endsr
545600070619
545700070620      *------------------------------------------------------------------------*
545800070619** msg
545900070619Filiale errata                                                                 01
546000070619La Filiale non è in gestione all'utente                                        02
546100070619Data errata                                                                    03
546200070619Non esistono distinte aperte nella data scelta                                 04
546300070619Scelta obbligatoria                                                            05
546400070619Codice Giro non corretto o non assegnabile ad una distinta                     06
546500070620Si sta bloccando l'assegnazione ORM: SI PREGA DI USCIRE dal lavoro!            07
546600070620Distinta ancora in uso da un altro lavoro.                                     08
546700070620Numero Distinta non valido o inesistente                                       09
546800070620Distinta non appartenente alla Data selezionata                                10
546900070620Distinta chiusa o annullata                                                    11
547000070620Scegliere 1 solo giro x effettuare l'interrogazione                            12
547100070621 Occorre selezionare almeno una riga per poter proseguire                      13
547200070621  l'elaborazione.                                                              14
547300070621 Non è stata scelta la DISTINTA per assegnare gli O.R.M.                       15
547400070621   Effettuare una scelta mediante le funzioni di ricerca.                      16
547500070621La Distinta è inesistente o non è stata selezionata                            17
547600070621La Distinta è ancora allocata. Non è quindi possibile selezionarla.            18
547700070622La Distinta è chiusa o annullata                                               19
547800070622Non si può trasferire sulla stessa Distinta                                    20
547900070719ORM errato o non in gestione alla filiale                                      21
548000070719Immettere il giro o l'ORM. Entrambi non è possibile                            22
548100070719Impossibile assegnare ORM già assegnato                                        23
548200071011Data antecedente alla data odierna                                             24
548300071011Data distinta superiore alla data odierna dei gg.ammessi                       25
548400071011Data distinta anteriore o superiore alla data odierna dei gg.ammessi           26
548500080717 Gli ORM sopra elencati NON sono stati aggiornati perchè IN USO da altro       27
548600080717 Lavoro.  CONTROLLARE l'operazione                                             28
548700070720 Durante l'aggiornamento archivi ci sono stati dei problemi. Non eseguito il   29
548800080717 Lavoro.  CONTROLLARE l'operazione                                             30
548900070926         Selezionato un ORM con Sponda Idraulica                               31
549000070926           non prevista in distinta, verificare.                               32
549100070925   Selezionato un ORM già comunicato all'AUT, verificare.                      33
549200080612   Selezionato un ORM già scaricato a PDA, verificare.                         34
549300080612Attenzione: il ritiro ha già esito "RITIRATO" dal PDA, verificare!             35
549400080612Attenzione: il ritiro ha già esito "NON RITIRATO" dal PDA, verificare!         36
549500080612Scelta non valida                                                              37
549600080327Non si può più utilizzare una distinta già quadrata!                           38
549700080611Il valore immesso per il campo non è valido                                    39
