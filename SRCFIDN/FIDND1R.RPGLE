000100041025     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200121116     h dftactgrp (*no) actgrp(*new)
000300041020      *----------------------------------------------------*
000400041020      *
            Statistiche RAC            ?
000500041020      *
000600041020      *----------------------------------------------------*
000700140205     ftabel00f  if   e           k disk
000800140205     fTNTBE01L  if   e           k disk
000900140205     fazcln01l  if   e           k disk
001000041022     ffitgd04l  if   e           k disk
001100041022     ffitgd09l  if   e           k disk    rename(fitgd000:fitgd09)
001200041026     ffitgn01l  if   e           k disk
001300041022     fazorg01l  if   e           k disk    rename(Azorg:azorg01)
001400041022     fazorg02l  if   e           k disk
001500041202     fsisdr01l  if   e           k disk
001600041020     fwfrac01l  uf a e           k disk     usropn
001700041023     fwfrac11l  uf a e           k disk     usropn
001800121116     fWFRAD01L  uf a e           k disk     usropn
001900041023     fwfrut00f  uf a e           k disk     usropn
002000041020     ffidnd1p   o    e             printer oflind(*in90)
002100041020      *----------------------------------------------------*
002200041020      *  RIEPILOGO INDICATORI
002300041020      *----------------------------------------------------*
002400041026      *  01   - on - sono in sede    off - sono in filiale
002500041026      *  10/11- emisisone oppure no dei campi
002600041026      *  12   - internazionale
002700041104      *  20/31- emisisone oppure no dei campi
002800041026      *  30   - comodo
002900041026      *----------------------------------------------------*
003000041020      *
003100041020
003200041020      *   S C H I E R E
003300041020
003400041022     d skpoel          s              3  0 dim(500)                             po da elab.
003500060511     d skpoel_i        s              3  0 dim(500)                             po da elab. internaz
003600041026     d skmad           s              3    dim(30)                              mot.apert.
003700150109     d sk_MAR          s                   like(TGDmad)   dim(90)               Motivo Apertura Recl
003800150109     d sk_MARd         s                   like(§MARdesc) dim(90)               Desc.Motivo Apertura
003900150109     d sk_MARs         s                   like(§MAReSt)  dim(90)               Mot.Ape. Esclus.Stat
004000060511     d sksped          s             13  0 dim(500)                             sped.part/arr x p.o
004100060511     d sksped_i        s             13  0 dim(500)                             sped.part/arr x inte
004200041202     d skpoin          s              3  0 dim(30)                              po da £1
004300121116     d cmd             s             80    dim(5) ctdata perrcd(1)              comando
004400041020
004500041022
004600041022      *   V A R I A B I L I
004700041022
004800041022     d lenght          s             15  5 inz(80)                              lunghezza cmd CLP
004900041022     d comman          s             80                                         valore cmd CLP
005000121116     d wcmd            s            512a   varying
005100041022     d wlib            s             10
005200141205     d wselugr         s               n   inz(*off)
005300041022
005400041025     d W_lib           s             30
005500041022     d xx              s              3  0
005600041202     d x1              s              3  0
005700041026     d mm              s              3  0
005800041026     d xm              s              3  0
005900140205     d yy              s              3  0
006000141205     d z1              s              3  0
006100041022     d Wrecok          s              1
006200041025     d W_poelwf        s              1
006300041025     d cisono_int      s              1
006400041025     d ST_testa        s              1
006500041023     d codut           s              1  0 inz(1)
006600041023     d giorni          s              3  0
006700041025     d wgiorno         s              2  0
006800041025     d dateu           s              8  0
006900041025     d w0140           s             14  0
007000041026     d w_tutte         s              7  0
007100041027     d rot_ute         s              3
007200041027
007300041027     d T_rutape        s             15  0
007400041027     d T_rutdxg        s             15  0
007500041027     d T_rutfon        s             15  0
007600041027     d T_rutcfa        s             15  0
007700041027     d T_rutifo        s             15  0
007800041027     d T_rutinf        s             15  0
007900041027     d T_ruttcf        s             15  0
008000041027     d T_ruttcm        s             15  0
008100041027     d T_ruttcl        s             15  0
008200041027     d T_ruttct        s             15  0
008300141203     d T_rutccd        s             15  0
008400141203     d T_rutccm        s             15  0
008500141203     d T_rutccc        s             15  0
008600141203     d T_rutcca        s             15  0
008700041026
008800041026     d W_pttota        s             15  0
008900041026     d W_pttotg        s             15  0
009000041026     d W_pctotch       s             15  0
009100041026     d W_pttotf        s             15  0
009200041026     d W_pttoth        s             15  0
009300041026     d W_pttotn        s             15  0
009400041026     d W_pttoti        s             15  0
009500041130     d W_pttomc        s             15  0
009600041026     d W_pttotd        s             15  0
009700041026     d W_pttotc        s             15  0
009800041026     d W_pttotx        s             15  0
009900041020
010000041023     d ktfp            s                   like(clntfp)
010100041023     d ktfa            s                   like(clntfa)
010200041023     d kann            s                   like(clnann)
010300041023     d kmes            s                   like(clnmes)
010400041023     d w_tpo           s                   like(ractpo)
010500041022     d wfil            s                   like(orgfil)
010600041022     d wrk_ugr         s                   like(tgdrpg)
010700041023     d Cod             s                   like(TBLcod)
010800041023     d Key             s                   like(TBLkey)
010900041023
011000041025     D WdataEUR        S               D   DATFMT(*eur)
011100041025     D WdataOGGI       S              8  0
011200041020
011300041020      *   D S   I N T E R N E / E S T E R N E
011400041020
011500041020
011600041020     d wlbdat          ds                  inz
011700041020     d  g02dat                 1      8  0
011800041020     d  g02inv                 9     16  0
011900041020     d  g02err                17     17
012000041020     d  g02tgi                18     22  0
012100041022
012200041023     D                 DS
012300041023     D  WRKlnp                        3
012400041023     D  WRKnrs                        2
012500041023     D  WRKnsp                        7
012600041023     D  WRKann                        4
012700041023     D  WRKSPE                 1     16
012800041023
012900041023     D                 DS
013000041023     D  WRKpoe                        3
013100041023     D  WRKnso                        2
013200041023     D  WRKnor                        7
013300041023     D  WRKnrv                        2
013400041023     D  WRKORM                 1     14
013500041025
013600041025     d clnmat          ds
013700041025     d  mat                    1     31    dim(31)
013800041025     d clnpom          ds
013900041025     d  pom                    1     31    dim(31)
014000041025
014100041202     D                 DS
014200041202     D  Wsdrann                       4  0
014300041202     D  Wsdrmes                       2  0
014400041202     D  Wsdrper                1      6  0
014500041201     D                 DS
014600041201     D  annoda                        4  0
014700041201     D  meseda                        2  0
014800041202     D  periododa              1      6  0
014900041201     D                 DS
015000041201     D  annoa                         4  0
015100041201     D  mesea                         2  0
015200041202     D  periodoa               1      6  0
015300041022
015400041022     d Kpjba         e ds
015500041022
015600041022      * - Parametri x Controllo profilo utenti
015700041022     d TIBS34DS      e ds                  inz
015800041022      * - Ds di riferimento al file esterno AZUTE00F
015900041022     d AZUTEDS       e ds                  extname(AZUTE00F)
016000041022      * - Ds per dati organigramma
016100041022     d DDatiUte      e ds
016200041022      * DS per TRUL06R - Caricamento £X
016300041022     d TRUL06DS      E DS
016400041022     d  LIN                    1     90  0
016500041022     d                                     DIM(30)
016600041022      * - Parametri per gestione reclami già esistenti
016700041022     d FIDND0DS      e ds
016800140925     d  TCO                   81     84
016900140925     d                                     DIM(4)
017000140925     d  MAR                   85    108
017100041022     d                                     DIM(8)
017200140925     d  CCR                  109    117
017300041022     d                                     DIM(3)
017400141204     d  UGRmin               122    171    dim(5)
017500141204     d  UGRmax               172    221    dim(5)
017600141204
017700041026      * - Tabelle
017800041026     d TIBS02ds      e ds                  inz
017900041026
018000041103     d dccr          e ds
018100041103     d dchr          e ds
018200041103     d dcor          e ds
018300041103     d dmar          e ds
018400041202
018500041202     d og143         e ds
018600050404     d og148         e ds                  inz
018700041202      *
018800041022      *---------------------------------------------------------------*
018900121116
019000121116       //--------------------------------------------------------------
019100121116       //?Definizione prototipi.
019200121116       //--------------------------------------------------------------
019300121116      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
019400140205
019500140205       //--------------------------------------------------------------
019600140205       //?Definizione key-list.                                        ?
019700140205       //--------------------------------------------------------------
019800140205
019900140205       // -?File TNTBE01L?
020000140205     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
020100140205     d                                     prefix(k_)   inz
020200041022
020300121116       //--------------------------------------------------------------
020400041022
020500041022      *---------------------------------------------------------------*
020600041022      *            Ciclo principale Statistica                        *
020700041022      *---------------------------------------------------------------*
020800041022      *
020900041022      * stampo le richieste
021000041022     c                   exsr      SR_TESTA
021100041022      * Verifico le richieste
021200041022
021300041022     c                   select
021400041022
021500041022      * se trovo la filiale valorizzata leggo solo quella
021600041022
021700041022      * per responsabilità
021800041022     c                   when      id0ord = '2' and id0prs > 0
021900041022     c                   eval      Wfil = id0prs
022000041022     c                   Exsr      SR_CARL6
022100041022     c                   Exsr      SR_eladet
022200041022      * per P.O. apertura
022300041022     c                   when      id0ord = '1' and id0fil > 0
022400041022     c                   eval      Wfil = id0fil
022500041022     c                   Exsr      SR_CARL6
022600041022     c                   Exsr      SR_eladet
022700041022
022800041022      * altrimemti elaboro leggendo i PO di APPARTENENZA
022900041022
023000041022     c                   OTHER
023100041022     c                   exsr      SR_CARPO
023200041022     c                   Exsr      SR_eladet
023300041022
023400041022     c                   endsl
023500041022
023600041022      * sono in sede devo elaborare i dati report ?
023700041022     c                   If        DUTlpo = 'S' and  id0crp = 'S'
023800060511      * carico i p.o. internazionali
023900060511     c                   Exsr      SR_CARpoint
024000060511
024100041022     c                   exsr      SR_report
024200121116      /free
024300121116       //?Se richiesto scrivo il WFRAD nella buona dalla QTEMP
024400121116         IF  ID0crptch = 'S';
024500121116           feod WFRAD01L;
024600121116           wcmd = cmd(5);
024700121116           wcmd = %trim(wcmd) + %trim(wlib) + %trim('/WFRAD00F)');
024800121116           wcmd = %trim(wcmd) + ' MBROPT(*REPLACE)';
024900121116           ExecuteCommand (wcmd);
025000121116         ENDIF;
025100121116      /end-free
025200121116
025300041022     c                   endif
025400041022      *
025500041022     c                   seton                                        lr
025600041022      *
025700041022      *---------------------------------------------------------------*
025800041022      * Recupero i P.O. da elaborare  della L6 del P.O.               *
025900041022      *---------------------------------------------------------------*
026000041022     c     Sr_CARL6      begsr
026100041022
026200041022      * Recupero i PO della £6
026300041022      *
026400041022     c                   clear                   trul06ds
026500041022     c                   move      '£6'          d06cod
026600041022     c                   movel     Wfil          d06key
026700041022     c                   movel(p)  trul06ds      KPJBU
026800041022
026900041022     c                   call      'TRUL06R'
027000041022     c                   parm                    kpjba
027100041022     c                   movel     kpjbu         trul06ds
027200041022     c                   movea     lin           Skpoel
027300041022      *
027400041022     c                   endsr
027500041022      *---------------------------------------------------------------*
027600041022      * Recupero i P.O. da elaborare  del Distetto Area selezionati   *
027700041022      *---------------------------------------------------------------*
027800041022     c     Sr_CARPO      begsr
027900041022
028000041022      * Recupero i PO dal AZORG
028100041022
028200041022      * distretto
028300050404     c     id0dst        setll     azorg
028400041022     c                   do        *hival
028500041022      * distretto valorizzato e non
028600041025     c                   if        id0dst <> *blanks
028700050404     c     id0dst        reade     azorg
028800041022     c                   else
028900050404     c                   read      azorg
029000041022     c                   endif
029100041022
029200041022     c                   if        %eof(azorg02l)
029300041022     c                   leave
029400041022     c                   endif
029500041022      *
029600041022     C                   if             ORGfva = *blanks
029700041022     C                             and (ORGfag = 'A'
029800041022     C                              or  ORGfag = 'F') and
029900041022      * area se inserita
030000041025     c                             ((ORGcar = id0are and id0are > 0) or
030100041026     c                             id0are = 0) OR
030200041026     C                             (ID0DST = ' ' AND ID0ARE = 0 AND
030300041026     C                              ORGFIL = 046)
030400041022     C                   add       1             XX
030500041022     C                   move      ORGfil        Skpoel(xx)
030600041022     C                   endif
030700041022      *
030800041022     c                   enddo
030900041022      *
031000041022     c                   endsr
031100060511      *---------------------------------------------------------------*
031200060511      * Recupero i P.O. Internazionali                                *
031300060511      *---------------------------------------------------------------*
031400060511     c     Sr_CARPOint   begsr
031500060511
031600060512     c                   clear                   xx
031700060511      * Recupero i PO dal AZORG
031800060511
031900060511     c     *loval        setll     azorg01
032000060511     c                   do        *hival
032100060511      *
032200060511     c                   read      azorg01
032300060511
032400060511     c                   if        %eof(azorg01l)
032500060511     c                   leave
032600060511     c                   endif
032700060511      *
032800060511     c                   if             ORGfva = *blanks
032900060511     c                             and (ORGfag = 'A'
033000060511     c                              or  ORGfag = 'F')
033100060511      * controllo il network
033200060511     c                   movel     orgde3        og143
033300060511     c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
033400060511     c                                            or §ogntw = 'FED'
033500060511     c                   add       1             XX
033600060511     c                   move      ORGfil        Skpoel_i(xx)
033700060511     c                   endif
033800060511      *
033900060511     c                   endif
034000060511      *
034100060511     c                   enddo
034200060511      *
034300060511     c                   endsr
034400041022      *---------------------------------------------------------------*
034500041022      * Elaborazione Reclami per P.O.                                 *
034600041022      *---------------------------------------------------------------*
034700041022     c     SR_Eladet     BegSr
034800041022      *
034900041022     c                   clear                   xx
035000041022      *
035100041022     c                   Do        *hival
035200041022      *
035300041022     c                   add       1             xx
035400041022
035500050404     c                   if        Skpoel(xx) = 0
035600050404     c                             or     xx  > %elem(SKpoel)
035700041022     c                   leave
035800041022     c                   endif
035900041022
036000041023      * eseguo delle pulizie di file di lavoro ed imposto i dati per la testa
036100041023     c                   exsr      SR_newpo
036200041023
036300041022      * leggo il file
036400041022
036500041022     c                   if        id0ord = '2'
036600050404     c     Skpoel(xx)    setll     fitgd09                                      responsabilità
036700041022     c                   else
036800050404     c     Skpoel(xx)    setll     fitgd000                                     apertura
036900041022     c                   endif
037000041022      *
037100041025     c                   do        *hival
037200041025      *
037300041022     c                   if        id0ord = '2'
037400050404     c     Skpoel(xx)    reade     fitgd09                                      responsabilità
037500041022     c                   else
037600050404     c     Skpoel(xx)    reade     fitgd000                                     apertura
037700041022     c                   endif
037800041022
037900140130      *
038000041022     c                   if        id0ord = '2' and
038100041022     c                             %eof(fitgd09l)                               responsabilità
038200041022     c                   exsr      SR_totale
038300041022     c                   leave
038400041022     c                   endif
038500041022     c                   if        id0ord = '1' and
038600041022     c                             %eof(fitgd04l)                               apertura
038700041022     c                   exsr      SR_totale
038800041025      * verifico se ci sono degli utenti internazionali rielaboro il P.O.
038900041025      * per i soli internazionali
039000041025     c                   if        cisono_int = 'Y'
039100041025     c                   exsr      SR_newpo
039200041025     c                   eval      W_tpo = 'I'
039300041026     c                   seton                                        12
039400050404     c     Skpoel(xx)    setll     fitgd000                                     apertura
039500041025     c                   iter
039600041025     c                   else
039700041025     c                   leave
039800041025     c                   endif
039900041025      *
040000041022     c                   endif
040100041022      * controllo il record con le selezioni fatte
040200041022     c                   exsr      SR_ctrrec
040300041022      * se record non ok leggo il successivo
040400041022     c                   if        Wrecok = ' '
040500041022     c                   iter
040600041022     c                   endif
040700041022      * preparo i campi a li stampa
040800041022     c                   exsr      SR_prepa
040900121116
041000121116      /free
041100121116       //?Memorizzo i dati per chiamante
041200121126         IF  ID0crptch = 'S';
041300121126           exsr MemoTCH;
041400121126         ENDIF;
041500121116      /end-free
041600041022
041700041022     c                   enddo
041800041025
041900041025     c                   enddo
042000041022
042100041022      *
042200041022     c                   endsr
042300041023      *---------------------------------------------------------------*
042400041023      * Preparazione campi e file per il nuovo P.O.                   *
042500041023      *---------------------------------------------------------------*
042600041023     c     SR_Newpo      BegSr
042700041023      *
042800041023      * se ho richiesto l'ordinamento 1 (per P.O. apertura) e la stampa anche del dettaglio
042900041023      * pulisco il file degli utenti
043000041023      *
043100041027     c                   if        id0ord = '1' and not *in01
043200121116      /free
043300121116        wcmd = cmd(4);
043400121116        ExecuteCommand (wcmd);
043500121116      /end-free
043600041023     c                   endif
043700041023      *
043800041023      * decodifica del P.O. e se sono in sede del distretto e dell'area
043900041023      *
044000050404     c     skpoel(xx)    chain     azorg01                            30
044100041023     c                   movel(p)  orgfl3        p3cdst
044200041023     c                   movel(p)  orgcar        p3care
044300041023     c                   movel(p)  skpoel(xx)    p3cfil
044400041023     c                   movel(p)  orgdes        p3dfil
044500041023      *
044600041023      * decodifico area e distretto
044700041023      *
044800041023     c                   movel     '17'          cod
044900041023     c                   movel(p)  p3cdst        key
045000050404     c     ktab          chain     tabel
045100041023      *
045200041023     c                   movel(p)  tbluni        p3ddst
045300041023      *
045400041023     c                   movel     '05'          cod
045500041023     c                   movel(p)  p3care        key
045600050404     c     ktab          chain     tabel
045700041023      *
045800041023     c                   movel(p)  tbluni        p3dare
045900041023      *
046000041023      * Calcolo da di confronto per il conteggio delle aperte da xx giorni
046100041023      *
046200041023     c                   If        id0xgg > 0
046300041025     c     *iso          movel     dateu         Wdataeur
046400041023     c                   z-add     0             giorni
046500041023      *
046600041023     c                   dow       giorni < id0xgg
046700041025     c                   subdur    1:*d          Wdataeur
046800041025     c                   extrct    wdataeur:*y   kann
046900041025     c                   extrct    wdataeur:*m   kmes
047000041025     c                   extrct    wdataeur:*d   wgiorno
047100041023     c                   clear                   ktfa
047200041023     c                   eval      ktfp = p3cfil
047300050404     c     kazcln        chain     azcln000
047400041023     c                   if        not%found(azcln01l)
047500041023     c                   leave
047600041023     c                   endif
047700041023     c                   if        MAT(wgiorno) =  'F'
047800041023     c                             or  POM(wgiorno) =  'F'
047900041023     c                   iter
048000041023     c                   else
048100041023     c                   add       1             giorni
048200041023     c                   endif
048300041023     c                   enddo
048400041023     c                   endif
048500041025     c                   move      wdataeur      wdataoggi
048600041109      *
048700041109      * giro la data
048800041109      *
048900041109     c                   clear                   WLBdat
049000041109     c                   eval      G02dat = wDataoggi
049100041109     c                   call      'XSRDA8'
049200041109     c                   parm                    WLBdat
049300041109     c                   z-add     G02inv        wdataoggi
049400041025      * Pulisco flag di segnalazine di reclami aperti da INT
049500041025      * se richiesto tipo ordinamento 1
049600041025     c                   if        id0ord = '1'
049700041025     c                   clear                   cisono_int
049800041025     c                   clear                   W_tpo
049900041026     c                   setoff                                       12
050000041025     c                   endif
050100041026      * se richiesto solo estero
050200041026     c                   if        id0sre = 'S'
050300041026     c                   eval      W_tpo = 'I'
050400041026     c                   seton                                        12
050500041026     c                   endif
050600041026      * Pulisco la schiera dei motivi di apertura
050700041026     c                   clear                   skmad
050800041026     c                   z-add     0             mm
050900041025      *
051000041023     c                   endsr
051100041023      *
051200041023      *---------------------------------------------------------------*
051300041023      * Fine lettura di un P.O. TOTALI                                *
051400041023      *---------------------------------------------------------------*
051500041023     c     SR_Totale     BegSr
051600041023      *
051700041023     c                   if        ractcf > 0 or ractcm > 0 or ractcl > 0 or
051800041023     c                             ractct > 0
051900041023     c                   write     wfrac100
052000041025      * pulisco prima volta che imposto i dati per ogni P.O.
052100041025     c                   eval      W_poelwf = ' '
052200041026      *
052300041026      * stampa anche dei totali
052400041026      *
052500041026      * testate
052600041026     c                   write     Dnd1t1
052700041026     c                   write     Dnd1tS
052800041026     c                   write     Dnd1t3
052900041026     c   01              write     Dnd1t4
053000041026     c                   write     Dnd1t5
053100041026     c                   write     Dnd1t3
053200041026
053300041026     c                   write     Dnd1tt
053400041104     c                   write     Dnd1t3B
053500041026      *
053600041026     c                   exsr      SR_statot
053700041025      * pulisco il record
053800041025     c                   clear                   wfrac100
053900041023     c                   endif
054000041023     c                   endsr
054100041026      *---------------------------------------------------------------*
054200041026      * Elaboro e stampo i totali                                     *
054300041026      *---------------------------------------------------------------*
054400041026     c     SR_Statot     BegSr
054500041026      *
054600041026      * pulisco i campi di comodo
054700041026      *
054800041026     c                   clear                   W_pttota
054900041026     c                   clear                   W_pttotg
055000041026     c                   clear                   W_pctotch
055100041026     c                   clear                   W_pttotf
055200041026     c                   clear                   W_pttoth
055300041026     c                   clear                   W_pttotn
055400041026     c                   clear                   W_pttoti
055500041130     c                   clear                   W_pttomc
055600041026     c                   clear                   W_pttotd
055700041026     c                   clear                   W_pttotc
055800041026     c                   clear                   W_pttotx
055900041026      * riga dei totali
056000041026      *
056100041026     c                   movel(p)  'T O T A L E' PTCdes
056200041026      * totali aperti
056300041026     c                   eval      W_pttota= raca01 + raca02 + raca03 + raca05 +
056400041026     c                             raca06 + raca07 + raca10 + raca11 + raca12 +
056500041026     c                             raca13 + raca20 + raca21 + raca22 + raca30 +
056600070710     c                             raca99 + racax1 + racax2 + raca14 + raca15 +
056700151202     c                             raca16 + raca50 + raca17 + raca18
056800041026     c                   z-add     W_pttota      pttota
056900041026      * totali aperti da oltre xx giorni
057000041026     c                   eval      W_pttotg= racg01 + racg02 + racg03 + racg05 +
057100041026     c                             racg06 + racg07 + racg10 + racg11 + racg12 +
057200041026     c                             racg13 + racg20 + racg21 + racg22 + racg30 +
057300070710     c                             racg99 + racgx1 + racgx2 + racg14 + racg15 +
057400151202     c                             racg16 + racg50 + racg17 + racg18
057500041026     c                   z-add     W_pttotg      pttotg
057600041026      * totali chiusi fondati
057700041026     c                   eval      W_pttotf= racf01 + racf02 + racf03 + racf05 +
057800041026     c                             racf06 + racf07 + racf10 + racf11 + racf12 +
057900041026     c                             racf13 + racf20 + racf21 + racf22 + racf30 +
058000070710     c                             racf99 + racfx1 + racfx2 + racf14 + racf15 +
058100151202     c                             racf16 + racf50 + racf17 + racf18
058200041026     c                   z-add     W_pttotf      pttotf
058300041026      * totali chiusi fondati da altri
058400041026     c                   eval      W_pttoth= rach01 + rach02 + rach03 + rach05 +
058500041026     c                             rach06 + rach07 + rach10 + rach11 + rach12 +
058600041026     c                             rach13 + rach20 + rach21 + rach22 + rach30 +
058700070710     c                             rach99 + rachx1 + rachx2 + rach14 + rach15 +
058800151202     c                             rach16 + rach50 + rach17 + rach18
058900041026     c                   z-add     W_pttoth      pttoth
059000041026      * totali chiusi infondati
059100041026     c                   eval      W_pttotn= racn01 + racn02 + racn03 + racn05 +
059200041026     c                             racn06 + racn07 + racn10 + racn11 + racn12 +
059300041026     c                             racn13 + racn20 + racn21 + racn22 + racn30 +
059400070710     c                             racn99 + racnx1 + racnx2 + racn14 + racn15 +
059500151202     c                             racn16 + racn50 + racn17 + racn18
059600041026     c                   z-add     W_pttotn      pttotn
059700041026      * totali chiusi informativi
059800041026     c                   eval      W_pttoti= raci01 + raci02 + raci03 + raci05 +
059900041026     c                             raci06 + raci07 + raci10 + raci11 + raci12 +
060000041026     c                             raci13 + raci20 + raci21 + raci22 + raci30 +
060100070710     c                             raci99 + racix1 + racix2 + raci14 + raci15 +
060200151202     c                             raci16 + raci50 + raci17 + raci18
060300041026     c                   z-add     W_pttoti      pttoti
060400041026      * totali chiusi
060500041026     c                   eval      W_pctotch =W_pttotf + W_pttotn + W_pttoti
060600041026     c                   z-add     W_pctotch     pctotch
060700041026      * totali aperte + chiuse
060800041026     c                   eval      W_tutte = W_pttota + W_pctotch
060900041026      * totali per chi chiama  Mittente
061000041130     c                   eval      W_pttomc= racm01 + racm02 + racm03 + racm05 +
061100041026     c                             racm06 + racm07 + racm10 + racm11 + racm12 +
061200041026     c                             racm13 + racm20 + racm21 + racm22 + racm30 +
061300070710     c                             racm99 + racmx1 + racmx2 + racm14 + racm15 +
061400151202     c                             racm16 + racm50 + racm17 + racm18
061500041130     c                   z-add     W_pttomc      pttomc
061600041026      * totali per chi chiama  Destinatario
061700041026     c                   eval      W_pttotd= racd01 + racd02 + racd03 + racd05 +
061800041026     c                             racd06 + racd07 + racd10 + racd11 + racd12 +
061900041026     c                             racd13 + racd20 + racd21 + racd22 + racd30 +
062000070710     c                             racd99 + racdx1 + racdx2 + racd14 + racd15 +
062100151202     c                             racd16 + racd50 + racd17 + racd18
062200041026     c                   z-add     W_pttotd      pttotd
062300041026      * totali per chi chiama  Cliente
062400041026     c                   eval      W_pttotc= racc01 + racc02 + racc03 + racc05 +
062500041026     c                             racc06 + racc07 + racc10 + racc11 + racc12 +
062600041026     c                             racc13 + racc20 + racc21 + racc22 + racc30 +
062700070710     c                             racc99 + raccx1 + raccx2 + racc14 + racc15 +
062800151202     c                             racc16 + racc50 + racc17 + racc18
062900041130     c                   add       W_pttotc      pttomc
063000041026      * totali per chi chiama  Altro
063100041026     c                   eval      W_pttotx= racx01 + racx02 + racx03 + racx05 +
063200041026     c                             racx06 + racx07 + racx10 + racx11 + racx12 +
063300041026     c                             racx13 + racx20 + racx21 + racx22 + racx30 +
063400070710     c                             racx99 + racxx1 + racxx2 + racx14 + racx15 +
063500151202     c                             racx16 + racx50 + racx17 + racx18
063600041026     c                   z-add     W_pttotx      pttotx
063700041026      * percentuali sul tipo comunicazioni sul numero totale dei reclami del periodo
063800041026     c                   if        ractcf > 0
063900041104     c                   eval(h)   pertcf = (ractcf * 100) / W_tutte
064000041104     c                   seton                                        27
064100041026     c                   else
064200041026     c                   clear                   pertcf
064300041104     c                   setoff                                       27
064400041026     c                   endif
064500041026
064600041026     c                   if        ractcm > 0
064700041104     c                   eval(h)   pertcm = (ractcm * 100) / W_tutte
064800041104     c                   seton                                        28
064900041026     c                   else
065000041026     c                   clear                   pertcm
065100041104     c                   setoff                                       28
065200041026     c                   endif
065300041026
065400041026     c                   if        ractcl > 0
065500041104     c                   eval(h)   pertcl = (ractcl * 100) / W_tutte
065600041104     c                   seton                                        29
065700041026     c                   else
065800041026     c                   clear                   pertcl
065900041104     c                   setoff                                       29
066000041026     c                   endif
066100041026
066200041026     c                   if        ractct > 0
066300041104     c                   eval(h)   pertct = (ractct * 100) / W_tutte
066400041104     c                   seton                                        31
066500041026     c                   else
066600041026     c                   clear                   pertct
066700041104     c                   setoff                                       31
066800041026     c                   endif
066900041026
067000041026      * gestisco indicatore solo per la  riga dei totali
067100041026
067200041026     c                   seton                                        10
067300041026     c                   setoff                                       11
067400041026
067500041026     c                   write     dnd1dt2
067600041026     c                   seton                                        11
067700041026     c                   setoff                                       10
067800041026
067900041026      * riga delle percentuali
068000041026
068100041026      * Chiuse
068200041026     c                   if        pttotf > 0
068300041104     c                   eval(h)   tcfper = (pttotf * 100) / pctotch
068400041026     c                   seton                                        20
068500041026     c                   else
068600041026     c                   clear                   tcfper
068700041026     c                   setoff                                       20
068800041026     c                   endif
068900041026
069000041026     c                   if        pttotn > 0
069100041104     c                   eval(h)   tifper = (pttotn * 100) / pctotch
069200041026     c                   seton                                        21
069300041026     c                   else
069400041026     c                   clear                   tifper
069500041026     c                   setoff                                       21
069600041026     c                   endif
069700041026
069800041026     c                   if        pttoti > 0
069900041104     c                   eval(h)   tinper = (pttoti * 100) / pctotch
070000041026     c                   seton                                        22
070100041026     c                   else
070200041026     c                   clear                   tinper
070300041026     c                   setoff                                       22
070400041026     c                   endif
070500041026      * Chi Chiama
070600041130     c                   if        pttomc > 0
070700041130     c                   eval(h)   mclper = (pttomc * 100) / W_tutte
070800041026     c                   seton                                        23
070900041026     c                   else
071000041130     c                   clear                   mclper
071100041026     c                   setoff                                       23
071200041026     c                   endif
071300041026
071400041026     c                   if        pttotd > 0
071500041104     c                   eval(h)   desper = (pttotd * 100) / W_tutte
071600041026     c                   seton                                        24
071700041026     c                   else
071800041026     c                   setoff                                       24
071900041026     c                   clear                   desper
072000041026     c                   endif
072100041026
072200041026     c                   if        pttotx > 0
072300041104     c                   eval(h)   altper = (pttotx * 100) / W_tutte
072400041026     c                   seton                                        26
072500041026     c                   else
072600041026     c                   setoff                                       26
072700041026     c                   clear                   altper
072800041026     c                   endif
072900041026
073000041026      * stampo riga percentuali
073100041026     c                   write     dnd1dt4
073200041026      * stampo riga titolo motivo d'apertura
073300041026     c                   write     dnd1dt3
073400041026      * elaboro per ogni motivo
073500041026      *
073600041026     c                   z-add     0             xm
073700041026      *
073800041026     c                   do        mm            xm
073900041026     c                   if        skmad(xm) <> *blanks
074000041026      * decodifico
074100140205     c                   eval      yy = %lookup( skMAD(xm) : sk_MAR )
074200140205     c                   if        yy > *zero
074300140205     c                   eval      PTCdes = sk_MARd(yy)
074400140205     c                   else
074500140205     c                   eval      PTCdes = *all'? '
074600140205     c                   endif
074700041026      *
074800041026      * valorizzo i campi di stampa
074900041026     c                   IF        skmad(xm) = '  1'
075000041026     c                   z-add     raca01        pttota
075100041026     c                   z-add     racg01        pttotg
075200041026     c                   z-add     racf01        pctotch
075300041026     c                   add       racn01        pctotch
075400041026     c                   add       raci01        pctotch
075500041026     c                   z-add     racf01        pttotf
075600041026     c                   z-add     rach01        pttoth
075700041026     c                   z-add     racn01        pttotn
075800041026     c                   z-add     raci01        pttoti
075900041130     c     racc01        add       racm01        pttomc
076000041026     c                   z-add     racd01        pttotd
076100041026     c                   z-add     racx01        pttotx
076200041026     c                   endif
076300041026
076400041026     c                   IF        skmad(xm) = '  2'
076500041026     c                   z-add     raca02        pttota
076600041026     c                   z-add     racg02        pttotg
076700041026     c                   z-add     racf02        pctotch
076800041026     c                   add       racn02        pctotch
076900041026     c                   add       raci02        pctotch
077000041026     c                   z-add     racf02        pttotf
077100041026     c                   z-add     rach02        pttoth
077200041026     c                   z-add     racn02        pttotn
077300041026     c                   z-add     raci02        pttoti
077400041130     c     racc02        add       racm02        pttomc
077500041026     c                   z-add     racd02        pttotd
077600041026     c                   z-add     racx02        pttotx
077700041026     c                   endif
077800041026
077900041026     c                   IF        skmad(xm) = '  3'
078000041026     c                   z-add     raca03        pttota
078100041026     c                   z-add     racg03        pttotg
078200041026     c                   z-add     racf03        pctotch
078300041026     c                   add       racn03        pctotch
078400041026     c                   add       raci03        pctotch
078500041026     c                   z-add     racf03        pttotf
078600041026     c                   z-add     rach03        pttoth
078700041026     c                   z-add     racn03        pttotn
078800041026     c                   z-add     raci03        pttoti
078900041130     c     racc03        add       racm03        pttomc
079000041026     c                   z-add     racd03        pttotd
079100041026     c                   z-add     racx03        pttotx
079200041026     c                   endif
079300041026
079400041026     c                   IF        skmad(xm) = '  5'
079500041026     c                   z-add     raca05        pttota
079600041026     c                   z-add     racg05        pttotg
079700041026     c                   z-add     racf05        pctotch
079800041026     c                   add       racn05        pctotch
079900041026     c                   add       raci05        pctotch
080000041026     c                   z-add     racf05        pttotf
080100041026     c                   z-add     rach05        pttoth
080200041026     c                   z-add     racn05        pttotn
080300041026     c                   z-add     raci05        pttoti
080400041130     c     racc05        add       racm05        pttomc
080500041026     c                   z-add     racd05        pttotd
080600041026     c                   z-add     racx05        pttotx
080700041026     c                   endif
080800041026
080900041026     c                   IF        skmad(xm) = '  6'
081000041026     c                   z-add     raca06        pttota
081100041026     c                   z-add     racg06        pttotg
081200041026     c                   z-add     racf06        pctotch
081300041026     c                   add       racn06        pctotch
081400041026     c                   add       raci06        pctotch
081500041026     c                   z-add     racf06        pttotf
081600041026     c                   z-add     rach06        pttoth
081700041026     c                   z-add     racn06        pttotn
081800041026     c                   z-add     raci06        pttoti
081900041130     c     racc06        add       racm06        pttomc
082000041026     c                   z-add     racd06        pttotd
082100041026     c                   z-add     racx06        pttotx
082200041026     c                   endif
082300041026
082400041026     c                   IF        skmad(xm) = '  7'
082500041026     c                   z-add     raca07        pttota
082600041026     c                   z-add     racg07        pttotg
082700041026     c                   z-add     racf07        pctotch
082800041026     c                   add       racn07        pctotch
082900041026     c                   add       raci07        pctotch
083000041026     c                   z-add     racf07        pttotf
083100041026     c                   z-add     rach07        pttoth
083200041026     c                   z-add     racn07        pttotn
083300041026     c                   z-add     raci07        pttoti
083400041130     c     racc07        add       racm07        pttomc
083500041026     c                   z-add     racd07        pttotd
083600041026     c                   z-add     racx07        pttotx
083700041026     c                   endif
083800041026
083900041026     c                   IF        skmad(xm) = ' 10'
084000041026     c                   z-add     raca10        pttota
084100041026     c                   z-add     racg10        pttotg
084200041026     c                   z-add     racf10        pctotch
084300041026     c                   add       racn10        pctotch
084400041026     c                   add       raci10        pctotch
084500041026     c                   z-add     racf10        pttotf
084600041026     c                   z-add     rach10        pttoth
084700041026     c                   z-add     racn10        pttotn
084800041026     c                   z-add     raci10        pttoti
084900041130     c     racc10        add       racm10        pttomc
085000041130     c                   z-add     racd10        pttotd
085100041026     c                   z-add     racx10        pttotx
085200041026     c                   endif
085300041026
085400041026     c                   IF        skmad(xm) = ' 11'
085500041026     c                   z-add     raca11        pttota
085600041026     c                   z-add     racg11        pttotg
085700041026     c                   z-add     racf11        pctotch
085800041026     c                   add       racn11        pctotch
085900041026     c                   add       raci11        pctotch
086000041026     c                   z-add     racf11        pttotf
086100041026     c                   z-add     rach11        pttoth
086200041026     c                   z-add     racn11        pttotn
086300041026     c                   z-add     raci11        pttoti
086400041130     c     racc11        add       racm11        pttomc
086500041026     c                   z-add     racd11        pttotd
086600041026     c                   z-add     racx11        pttotx
086700041026     c                   endif
086800041026
086900041026     c                   IF        skmad(xm) = ' 12'
087000041026     c                   z-add     raca12        pttota
087100041026     c                   z-add     racg12        pttotg
087200041026     c                   z-add     racf12        pctotch
087300041026     c                   add       racn12        pctotch
087400041026     c                   add       raci12        pctotch
087500041026     c                   z-add     racf12        pttotf
087600041026     c                   z-add     rach12        pttoth
087700041026     c                   z-add     racn12        pttotn
087800041026     c                   z-add     raci12        pttoti
087900041130     c     racc12        add       racm12        pttomc
088000041026     c                   z-add     racd12        pttotd
088100041026     c                   z-add     racx12        pttotx
088200041026     c                   endif
088300041026
088400041026     c                   IF        skmad(xm) = ' 13'
088500041026     c                   z-add     raca13        pttota
088600041026     c                   z-add     racg13        pttotg
088700041026     c                   z-add     racf13        pctotch
088800041026     c                   add       racn13        pctotch
088900041026     c                   add       raci13        pctotch
089000041026     c                   z-add     racf13        pttotf
089100041026     c                   z-add     rach13        pttoth
089200041026     c                   z-add     racn13        pttotn
089300041026     c                   z-add     raci13        pttoti
089400041130     c     racc13        add       racm13        pttomc
089500041026     c                   z-add     racd13        pttotd
089600041026     c                   z-add     racx13        pttotx
089700041026     c                   endif
089800070710
089900070710     c                   IF        skmad(xm) = ' 14'
090000070710     c                   z-add     raca14        pttota
090100070710     c                   z-add     racg14        pttotg
090200070710     c                   z-add     racf14        pctotch
090300070710     c                   add       racn14        pctotch
090400070710     c                   add       raci14        pctotch
090500070710     c                   z-add     racf14        pttotf
090600070710     c                   z-add     rach14        pttoth
090700070710     c                   z-add     racn14        pttotn
090800070710     c                   z-add     raci14        pttoti
090900070710     c     racc14        add       racm14        pttomc
091000070710     c                   z-add     racd14        pttotd
091100070710     c                   z-add     racx14        pttotx
091200070710     c                   endif
091300070710
091400070710     c                   IF        skmad(xm) = ' 15'
091500070710     c                   z-add     raca15        pttota
091600070710     c                   z-add     racg15        pttotg
091700070710     c                   z-add     racf15        pctotch
091800070710     c                   add       racn15        pctotch
091900070710     c                   add       raci15        pctotch
092000070710     c                   z-add     racf15        pttotf
092100070710     c                   z-add     rach15        pttoth
092200070710     c                   z-add     racn15        pttotn
092300070710     c                   z-add     raci15        pttoti
092400070710     c     racc15        add       racm15        pttomc
092500070710     c                   z-add     racd15        pttotd
092600070710     c                   z-add     racx15        pttotx
092700070710     c                   endif
092800070710
092900070710     c                   IF        skmad(xm) = ' 16'
093000070710     c                   z-add     raca16        pttota
093100070710     c                   z-add     racg16        pttotg
093200070710     c                   z-add     racf16        pctotch
093300070710     c                   add       racn16        pctotch
093400070710     c                   add       raci16        pctotch
093500070710     c                   z-add     racf16        pttotf
093600070710     c                   z-add     rach16        pttoth
093700070710     c                   z-add     racn16        pttotn
093800070710     c                   z-add     raci16        pttoti
093900070710     c     racc16        add       racm16        pttomc
094000070710     c                   z-add     racd16        pttotd
094100070710     c                   z-add     racx16        pttotx
094200070710     c                   endif
094300070710
094400041026     c                   IF        skmad(xm) = ' 20'
094500041026     c                   z-add     raca20        pttota
094600041026     c                   z-add     racg20        pttotg
094700041026     c                   z-add     racf20        pctotch
094800041026     c                   add       racn20        pctotch
094900041026     c                   add       raci20        pctotch
095000041026     c                   z-add     racf20        pttotf
095100041026     c                   z-add     rach20        pttoth
095200041026     c                   z-add     racn20        pttotn
095300041026     c                   z-add     raci20        pttoti
095400041130     c     racc20        add       racm20        pttomc
095500041026     c                   z-add     racd20        pttotd
095600041026     c                   z-add     racx20        pttotx
095700041026     c                   endif
095800041026
095900041026     c                   IF        skmad(xm) = ' 21'
096000041026     c                   z-add     raca21        pttota
096100041026     c                   z-add     racg21        pttotg
096200041026     c                   z-add     racf21        pctotch
096300041026     c                   add       racn21        pctotch
096400041026     c                   add       raci21        pctotch
096500041026     c                   z-add     racf21        pttotf
096600041026     c                   z-add     rach21        pttoth
096700041026     c                   z-add     racn21        pttotn
096800041026     c                   z-add     raci21        pttoti
096900041130     c     racc21        add       racm21        pttomc
097000041026     c                   z-add     racd21        pttotd
097100041026     c                   z-add     racx21        pttotx
097200041026     c                   endif
097300041026
097400041026     c                   IF        skmad(xm) = ' 22'
097500041026     c                   z-add     raca22        pttota
097600041026     c                   z-add     racg22        pttotg
097700041026     c                   z-add     racf22        pctotch
097800041026     c                   add       racn22        pctotch
097900041026     c                   add       raci22        pctotch
098000041026     c                   z-add     racf22        pttotf
098100041026     c                   z-add     rach22        pttoth
098200041026     c                   z-add     racn22        pttotn
098300041026     c                   z-add     raci22        pttoti
098400041130     c     racc22        add       racm22        pttomc
098500041026     c                   z-add     racd22        pttotd
098600041026     c                   z-add     racx22        pttotx
098700041026     c                   endif
098800041026
098900041026     c                   IF        skmad(xm) = ' 30'
099000041026     c                   z-add     raca30        pttota
099100041026     c                   z-add     racg30        pttotg
099200041026     c                   z-add     racf30        pctotch
099300041026     c                   add       racn30        pctotch
099400041026     c                   add       raci30        pctotch
099500041026     c                   z-add     racf30        pttotf
099600041026     c                   z-add     rach30        pttoth
099700041026     c                   z-add     racn30        pttotn
099800041026     c                   z-add     raci30        pttoti
099900041130     c     racc30        add       racm30        pttomc
100000041026     c                   z-add     racd30        pttotd
100100041026     c                   z-add     racx30        pttotx
100200041026     c                   endif
100300120914
100400120914     c                   IF        skmad(xm) = ' 50'
100500120914     c                   z-add     raca50        pttota
100600120914     c                   z-add     racg50        pttotg
100700120914     c                   z-add     racf50        pctotch
100800120914     c                   add       racn50        pctotch
100900120914     c                   add       raci50        pctotch
101000120914     c                   z-add     racf50        pttotf
101100120914     c                   z-add     rach50        pttoth
101200120914     c                   z-add     racn50        pttotn
101300120914     c                   z-add     raci50        pttoti
101400120914     c     racc50        add       racm50        pttomc
101500120914     c                   z-add     racd50        pttotd
101600120914     c                   z-add     racx50        pttotx
101700120914     c                   endif
101800041026
101900041026     c                   IF        skmad(xm) = ' 99'
102000041026     c                   z-add     raca99        pttota
102100041026     c                   z-add     racg99        pttotg
102200041026     c                   z-add     racf99        pctotch
102300041026     c                   add       racn99        pctotch
102400041026     c                   add       raci99        pctotch
102500041026     c                   z-add     racf99        pttotf
102600041026     c                   z-add     rach99        pttoth
102700041026     c                   z-add     racn99        pttotn
102800041026     c                   z-add     raci99        pttoti
102900041130     c     racc99        add       racm99        pttomc
103000041026     c                   z-add     racd99        pttotd
103100041026     c                   z-add     racx99        pttotx
103200041026     c                   endif
103300130515
103400130515     c                   IF        skmad(xm) = '  8'
103500130515     c                   z-add     racaX1        pttota
103600130515     c                   z-add     racgX1        pttotg
103700130515     c                   z-add     racfX1        pctotch
103800130515     c                   add       racnX1        pctotch
103900130515     c                   add       raciX1        pctotch
104000130515     c                   z-add     racfX1        pttotf
104100130515     c                   z-add     rachX1        pttoth
104200130515     c                   z-add     racnX1        pttotn
104300130515     c                   z-add     raciX1        pttoti
104400130515     c     raccX1        add       racmX1        pttomc
104500130515     c                   z-add     racdX1        pttotd
104600130515     c                   z-add     racxX1        pttotx
104700130515     c                   endif
104800140409
104900140409     c                   IF        skmad(xm) = ' 88'
105000140409     c                   z-add     racaX2        pttota
105100140409     c                   z-add     racgX2        pttotg
105200140409     c                   z-add     racfX2        pctotch
105300140409     c                   add       racnX2        pctotch
105400140409     c                   add       raciX2        pctotch
105500140409     c                   z-add     racfX2        pttotf
105600140409     c                   z-add     rachX2        pttoth
105700140409     c                   z-add     racnX2        pttotn
105800140409     c                   z-add     raciX2        pttoti
105900140409     c     raccX2        add       racmX2        pttomc
106000140409     c                   z-add     racdX2        pttotd
106100140409     c                   z-add     racxX2        pttotx
106200140409     c                   endif
106300151202
106400151202     c                   IF        skmad(xm) = ' 17'
106500151202     c                   z-add     raca17        pttota
106600151202     c                   z-add     racg17        pttotg
106700151202     c                   z-add     racf17        pctotch
106800151202     c                   add       racn17        pctotch
106900151202     c                   add       raci17        pctotch
107000151202     c                   z-add     racf17        pttotf
107100151202     c                   z-add     rach17        pttoth
107200151202     c                   z-add     racn17        pttotn
107300151202     c                   z-add     raci17        pttoti
107400151202     c     racc17        add       racm17        pttomc
107500151202     c                   z-add     racd17        pttotd
107600151202     c                   z-add     racx17        pttotx
107700151202     c                   endif
107800151202
107900151202     c                   IF        skmad(xm) = ' 18'
108000151202     c                   z-add     raca18        pttota
108100151202     c                   z-add     racg18        pttotg
108200151202     c                   z-add     racf18        pctotch
108300151202     c                   add       racn18        pctotch
108400151202     c                   add       raci18        pctotch
108500151202     c                   z-add     racf18        pttotf
108600151202     c                   z-add     rach18        pttoth
108700151202     c                   z-add     racn18        pttotn
108800151202     c                   z-add     raci18        pttoti
108900151202     c     racc18        add       racm18        pttomc
109000151202     c                   z-add     racd18        pttotd
109100151202     c                   z-add     racx18        pttotx
109200151202     c                   endif
109300041026
109400041026      * percentuale
109500041026
109600041026     c                   If        pctotch > 0
109700041104     c                   eval(h)   pcperm = (pctotch * 100) / W_pctotch
109800041026     c                   seton                                        11
109900041026     c                   else
110000041026     c                   clear                   pcperm
110100041026     c                   setoff                                       11
110200041026     c                   endif
110300041026      *
110400041026     c                   write     dnd1dt2
110500041026      *
110600041026     c                   endif
110700041026      *
110800041026     c                   enddo
110900041104      * stampo la leggenda
111000041130     c                   write     dnd1le3
111100041130
111200041104     c                   If        Id0ord = '1'
111300041104     c                   write     Dnd1le1
111400041104     c                   endif
111500041104     c                   If        Id0ord = '2'
111600041104     c                   write     Dnd1le2
111700041104     c                   endif
111800041026
111900041026      * se tipo ordinamento 1 e sono in filiale sxtampo anche il dettaglio utente
112000041026     c                   IF        id0ord = '1' and not *in01
112100041026
112200041104     c                   setoff                                       90
112300041026      * testate
112400041103     c                   write     Dnd1t1
112500041103     c                   write     Dnd1tS
112600041103     c                   write     Dnd1t3
112700041103     c   01              write     Dnd1t4
112800041103     c                   write     Dnd1t5
112900041103     c                   write     Dnd1t3
113000041103
113100041026     c                   write     Dnd1ut
113200041026
113300041026     c                   feod      wfrut00f
113400041027     c                   clear                   rot_ute
113500041026
113600041026     c     *loval        setll     wfrut00f
113700041026
113800041026     c                   do        *hival
113900041026     c                   read      wfrut00f
114000041026     c                   if        %eof(wfrut00f)
114100041027     c                   exsr      SR_totute
114200041026     c                   leave
114300041026     c                   endif
114400041027      * mi salvo il prefisso utente  se uguale a *blank
114500041027     c                   if        rot_ute = *blanks
114600041027     c                   eval      rot_ute = %subst(rutute:1:3)
114700041027     c                   endif
114800041027      *
114900041027     c                   If        rot_ute <> %subst(rutute:1:3)
115000041027     c                   exsr      SR_totute
115100041027     c                   endif
115200041027      *
115300041026     c                   movel(p)  rutute        pudute
115400041026      * calcolo le totale chiuse
115500041026     c                   z-add     rutfon        pudtch
115600041026     c                   add       rutifo        pudtch
115700041026     c                   add       rutinf        pudtch
115800141204      /free
115900141204       //?Calcolo il totale del chi chiama Mittente/Cliente
116000141204         PUTcmc = RUTccm + RUTccc;
116100141204      /end-free
116200141204     c
116300041027      * calcolo i totali per ufficio
116400041027     c                   add       rutape        T_rutape
116500041027     c                   add       rutdxg        T_rutdxg
116600041027     c                   add       rutfon        T_rutfon
116700041027     c                   add       rutcfa        T_rutcfa
116800041027     c                   add       rutifo        T_rutifo
116900041027     c                   add       rutinf        T_rutinf
117000041027     c                   add       ruttcf        T_ruttcf
117100041027     c                   add       ruttcm        T_ruttcm
117200041027     c                   add       ruttcl        T_ruttcl
117300041027     c                   add       ruttct        T_ruttct
117400141203     c                   add       rutccd        T_rutccd
117500141203     c                   add       rutccm        T_rutccm
117600141203     c                   add       rutccc        T_rutccc
117700141203     c                   add       rutcca        T_rutcca
117800041027      *
117900041027      *
118000041026     c                   If        *in90
118100041026     c                   setoff                                       90
118200041026      * testate
118300041026     c                   write     Dnd1t1
118400041026     c                   write     Dnd1tS
118500041026     c                   write     Dnd1t3
118600041026     c   01              write     Dnd1t4
118700041026     c                   write     Dnd1t5
118800041026     c                   write     Dnd1t3
118900041026
119000041026     c                   write     Dnd1ut
119100041026
119200041026     c                   endif
119300041026
119400041026     c                   write     Dnd1du1
119500041027      *
119600041027      * se ho stampato i totali rileggo il record
119700041026
119800041026     c                   enddo
119900041104
120000041104      * stampo la leggenda
120100041104     c                   If        Id0ord = '1'
120200041104     c                   write     Dnd1le1
120300041104     c                   endif
120400041104     c                   If        Id0ord = '2'
120500041104     c                   write     Dnd1le2
120600041104     c                   endif
120700041104
120800041026     c                   endif
120900041026
121000041026     c                   endsr
121100041026
121200041026      *
121300041027      *---------------------------------------------------------------*
121400041027      * Totali per ufficio                                            *
121500041027      *---------------------------------------------------------------*
121600041027     c     SR_totute     BegSr
121700041027
121800041027
121900041027     c                   if        rot_ute <> *blanks
122000041027
122100041027     c                   clear                   pudute
122200041027     c                   eval      pudute = 'T O T A L E  ' +
122300041027     c                             rot_ute
122400041027      * calcolo le totale chiuse
122500041027     c                   z-add     T_rutfon      pudtch
122600041027     c                   add       T_rutifo      pudtch
122700041027     c                   add       T_rutinf      pudtch
122800141204      /free
122900141204       //?Calcolo il totale del chi chiama Mittente/Cliente
123000141204         PUTcmc = t_RUTccm + t_RUTccc;
123100141204      /end-free
123200041027      * calcolo i totali per ufficio
123300041027     c                   z-add     T_rutape      rutape
123400041027     c                   z-add     T_rutdxg      rutdxg
123500041027     c                   z-add     T_rutfon      rutfon
123600041027     c                   z-add     T_rutcfa      rutcfa
123700041027     c                   z-add     T_rutifo      rutifo
123800041027     c                   z-add     T_rutinf      rutinf
123900041027     c                   z-add     T_ruttcf      ruttcf
124000041027     c                   z-add     T_ruttcm      ruttcm
124100041027     c                   z-add     T_ruttcl      ruttcl
124200041027     c                   z-add     T_ruttct      ruttct
124300141203     c                   z-add     T_rutccd      rutccd
124400141203     c                   z-add     T_rutccm      rutccm
124500141203     c                   z-add     T_rutccc      rutccc
124600141203     c                   z-add     T_rutcca      rutcca
124700041027
124800041027     c                   If        *in90
124900041027     c                   setoff                                       90
125000041027      * testate
125100041027     c                   write     Dnd1t1
125200041027     c                   write     Dnd1tS
125300041027     c                   write     Dnd1t3
125400041027     c   01              write     Dnd1t4
125500041027     c                   write     Dnd1t5
125600041027     c                   write     Dnd1t3
125700041027
125800041027     c                   write     Dnd1ut
125900041027
126000041027     c                   endif
126100041027
126200041027     c                   write     Dnd1du1
126300041027      *
126400041027      * riaggancio il record se non è EOF
126500041027      *
126600041027     c                   if        not %eof
126700041027     c     rutute        chain     wfrut00f
126800041027     c                   eval      rot_ute = %subst(rutute:1:3)
126900041027     c                   endif
127000041027
127100041027     c                   clear                   T_rutape
127200041027     c                   clear                   T_rutdxg
127300041027     c                   clear                   T_rutfon
127400041027     c                   clear                   T_rutcfa
127500041027     c                   clear                   T_rutifo
127600041027     c                   clear                   T_rutinf
127700041027     c                   clear                   T_ruttcf
127800041027     c                   clear                   T_ruttcm
127900041027     c                   clear                   T_ruttcl
128000041027     c                   clear                   T_ruttct
128100141203     c                   clear                   T_rutccd
128200141203     c                   clear                   T_rutccm
128300141203     c                   clear                   T_rutccc
128400141203     c                   clear                   T_rutcca
128500041027      *
128600041027     c                   endif
128700041027      *
128800041027     c                   endsr
128900041027      *
129000041022      *---------------------------------------------------------------*
129100041022      * Controllo i dati con le richieste del video                   *
129200041022      *---------------------------------------------------------------*
129300041022     c     SR_ctrrec     BegSr
129400041022
129500041022     c                   clear                   Wrecok
129600041022      *
129700041022     c                   do        *hival
129800041026      * se tipo tipo elaborazione uguale a ' ' e ordinamento 1 verifico se ci sono
129900041026      * reclami aprti da INT
130000060510      * se tipo tipo elaborazione uguale a ' ' e ordinamento 1 verifico se ci sono
130100060510      * reclami aperti da utente RAI*
130200041026      *
130300041026     c                   If        W_tpo = ' '  and id0ord = '1'
130400060510     c                             and (%subst(tgdpru:1:3) = 'INT' or
130500060510     c                                  %subst(tgdpru:1:3) = 'RAI')
130600041026     c                             and cisono_int = ' '
130700041026     c                   eval      cisono_int = 'Y'
130800041026     c                   endif
130900041025      * se tipo elaborazione internazionale verifico l'utente  e viceversa
131000041025      *
131100050421
131200050421      * se richiesto tipo ordinamneto "1" oppure rischiesto solo estero
131300050421
131400050421     c                   If        id0ord = '1'  or Id0sre ='S'
131500050421
131600060510     c                   If        W_tpo = 'I' and (%subst(tgdpru:1:3) <> 'INT'
131700060511     c                                         and  %subst(tgdpru:1:3) <> 'RAI')
131800041025     c                   leave
131900041025     c                   endif
132000060510
132100060510     c                   If        W_tpo = ' ' and (%subst(tgdpru:1:3) = 'INT'
132200060510     c                                         or   %subst(tgdpru:1:3) = 'RAI')
132300041025     c                   leave
132400041025     c                   endif
132500050421
132600050421     c                   endif
132700041022      *
132800041022      *- P.O. responsabile
132900041022      * diverso da quello a video
133000041022     c                   if        ID0prs > *zeros and tgdpor <> ID0prs
133100041022     c                   leave
133200041022     c                   endif
133300041022      *
133400041022      *- P.O. apertura
133500041022      * diverso da quello a video
133600041022     c                   if        ID0fil > *zeros and tgdpot <> ID0fil
133700041022     c                   leave
133800041022     c                   endif
133900041022      *
134000041022      * Solo le aperte da altro P.O. responsabile
134100041022      *
134200041022     c                   If        TGDpor>0 and TGDpor=TGDpot and ID0AAP='S'
134300041022     c                   leave
134400041022     c                   endif
134500041022      *
134600041022      * Solo le aperte da utente Internazionale
134700041022      *
134800060510     c                   If        ID0sre = 'S' and (%subst(tgdpru:1:3)<>'INT'
134900060515     c                                          and  %subst(tgdpru:1:3)<>'RAI')
135000041022     c                   leave
135100041022     c                   endif
135200041022      *
135300041022      *- Date chiamata
135400041022      * data minore di quella a video
135500041025     c                   if        ID0AMGdcd > 0  and tgddar < ID0AMGdcd
135600041022     c                   leave
135700041022     c                   endif
135800041022      * data maggiore di  quella a video
135900041025     c                   if        ID0AMGdca > 0  and tgddar > ID0AMGdca
136000041022     c                   leave
136100041022     c                   endif
136200041022      *
136300041022      *- Date chiusura
136400041022      * data minore di quella a video
136500041023     c                   if        ID0AMGdhd > 0  and tgddcd < ID0AMGdhd
136600041022     c                   leave
136700041022     c                   endif
136800041022      * data maggiore di  quella a video
136900041023     c                   if        ID0AMGdha > 0  and tgddcd > ID0AMGdha
137000041022     c                   leave
137100041022     c                   endif
137200041022      *- Tipo comunicazione
137300041022      *  diverso da quelli del video
137400140423     c                   clear                   w_lib
137500041025     c                   movea     tco           w_lib
137600041025     c                   If        w_lib <> *blank
137700041025     c     tgdtco        lookup    tco                                    30
137800041022     c                   if        not %found
137900041022     c                   leave
138000041022     c                   endif
138100041022     c                   endif
138200041022      *- Motivo apertura
138300041022      *  diverso da quelli del video
138400140423     c                   clear                   w_lib
138500041025     c                   movea     mar           w_lib
138600041025     c                   If        w_lib <> *blank
138700041025     c     tgdmad        lookup    mar                                    30
138800041022     c                   if        not %found
138900041022     c                   leave
139000041022     c                   endif
139100041022     c                   endif
139200140210      * ?solo motivi NON esclusi dalle statistiche?
139300140205     c                   eval      yy = %lookup( TGDmad : sk_MAR )
139400140210     c                   if        yy > *zero  and  sk_MARs(yy) = 'S'
139500140205     c                   leave
139600140205     c                   endif
139700041022      *- Codici chiusura
139800041022      *  diverso da quelli del video
139900140423     c                   clear                   w_lib
140000041025     c                   movea     ccr           w_lib
140100041025     c                   If        w_lib <> *blank
140200041025     c     tgdcdt        lookup    ccr                                    30
140300041022     c                   if        not %found
140400041022     c                   leave
140500041022     c                   endif
140600041022     c                   endif
140700041022      *- Solo le aperte
140800041022      *  e codice chiusura valorizzato
140900041022     c                   If        ID0sra = 'S' and tgdcdt <> *blank
141000041022     c                   leave
141100041022     c                   endif
141200041022      *
141300041022      * ricerca per utente
141400041022      *
141500041022     c                   If        tgdcdt <> *blanks
141600041022     c                   eval      WRK_ugr = tgdutc
141700041022     c                   else
141800041022     c                   eval      WRK_ugr = tgdrpg
141900041022     c                   endif
142000141204
142100141204      /free
142200141204       //?Selezione per utente
142300141205         wselugr = *off;
142400141205         IF  ID0UGRmin1 <> *blanks or ID0UGRmin2 <> *blanks or
142500141205             ID0UGRmin3 <> *blanks or ID0UGRmin4 <> *blanks or
142600141205             ID0UGRmin5 <> *blanks;
142700141205           z1 = 1;
142800141205           FOR  z1 by 1 to %elem(UGRmin);
142900141205             IF  UGRmin(z1) <> *blanks;
143000141205           //?verifico se i primi 3 byte dell'utente NON coincidono con i primi 3 del MIN
143100141205           //?provo con un altro
143200141205               IF  %subst(wrk_ugr:1:3) <> %subst(UGRmin(z1):1:3);
143300141205                 iter;
143400141205               ENDIF;
143500141205           //?richiesta selezione utente generico e non coincidono provo con un altro
143600141205               IF  UGRmax(z1) <> *blanks and
143700141205                   (wrk_ugr < UGRmin(z1) or wrk_ugr > UGRmax(z1));
143800141205                 iter;
143900141205               ENDIF;
144000141205           //?richiesta selezione utente NON generico e non coincidono provo con un altro
144100141205               IF  UGRmax(z1) = *blanks and wrk_ugr <> UGRmin(z1);
144200141205                 iter;
144300141205               ENDIF;
144400141205           //?se arrivo qua vuol dire che l'utente letto è tra quelli selezionati
144500141205           //?quindi esco dalla lettura delle selezioni
144600141205               wselugr = *on;
144700141205               leave;
144800141205             ENDIF;
144900141205           ENDFOR;
145000141205         //?se arrivo qua con il flag spento vuol dire che l'utente della RA
145100141205         //?non soddisfa nessuna selezione quindi NON devo elaborare il rcd letto
145200141205           IF  not wselugr;
145300141205             leave;
145400141205           ENDIF;
145500141205         ENDIF;
145600141204      /end-free
145700041022      *
145800041022      * se tutto OK record OK
145900041022     c                   eval      Wrecok = 'Y'
146000041022     c                   leave
146100041022      *
146200041022     c                   enddo
146300041022
146400041022     c                   endsr
146500041022      *---------------------------------------------------------------*
146600041022      * Preparazione campi e stampa del dettaglio                     *
146700041022      *---------------------------------------------------------------*
146800041022     c     SR_Prepa      BegSr
146900041025      *
147000041023      *
147100041023      * data richiesta
147200041023      *
147300041023     c                   clear                   WLBdat
147400041025     c                   eval      G02inv = tgddar
147500041023     c                   eval      G02err = '3'
147600041023     c                   call      'XSRDA8'
147700041023     c                   parm                    WLBdat
147800041023     c                   z-add     G02dat        p3cdac                         data ricez.comunicaz
147900041023      *
148000041023      * Oggetto
148100041023      *
148200041023     c                   clear                   p3cogg
148300050331      *
148400050331     c                   select
148500041023      * ---------- SPEDIZIONE -----------
148600050331     c                   when      tgdtor = 'S'
148700041023     c                   movel     tgdogg        WRKspe
148800041023     c                   eval      p3cogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
148900041023     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
149000041023     c                             ' ' + WRKann
149100041023      *
149200041023      * ----------    ORM     -----------
149300050331     c                   when      tgdtor = 'O'
149400041023     c                   movel     tgdogg        WRKorm
149500041023     c                   eval      p3cogg = 'ORM ' + %trim(WRKpoe) +  '/' +
149600041023     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
149700041023     c                             '/' + %trim(WRKnrv)
149800041023      *
149900041023      * ----------  GENERICO  -----------
150000050331     c                   when      tgdtor = 'G'
150100050331     c                   eval      p3cogg = TGDogg
150200050331      *
150300050331     c                   endsl
150400041023      *
150500041023      * note
150600041023      *
150700041023     c                   clear                   p3cno1
150800041023     c                   clear                   p3cno2
150900041023      * recupero le note
151000041023      *
151100050404     c     ktgn          setll     Fitgn000
151200041026     c*
151300050404     c     ktgn          reade     Fitgn000
151400041026     c                   if        not %eof(Fitgn01l)
151500041026     c                   movel     tgnnot        p3cno1
151600041026     c                   endif
151700050404     c     ktgn          reade     Fitgn000
151800041026     c                   if        not %eof(Fitgn01l)
151900041026     c                   movel     tgnnot        p3cno2
152000041026     c                   endif
152100041023      * aggancio record dei totali
152200041023      *
152300041025     c                   If        W_poelwf = ' '
152400041023     C     Krac          chain     Wfrac11l
152500041023     c                   if        not %found(wfrac11l)
152600041023     c                   clear                   wfrac100
152700041023      * valorizzo i primi  campi del file
152800041023     c                   eval      Ractpe = ID0ord
152900041025     c                   eval      Racdte = dateu
153000041023     c                   eval      Racddr = ID0AMGDCD
153100041023     c                   eval      Racadr = ID0AMGDCA
153200041023     c                   eval      Racddc = ID0AMGDHD
153300041023     c                   eval      Racadc = ID0AMGDHA
153400041023     c                   eval      Racpoe = p3cfil
153500041023     c                   eval      Ractpo = W_tpo
153600041023     c                   eval      Racdis = P3cdst
153700041023     c                   eval      Racare = P3care
153800041025      * eval prima volta che imposto i dati per ogni P.O.
153900041025     c                   eval      W_poelwf = 'Y'
154000041025      * eval prima volta che stampo testats a cambio P.O.
154100041025     c                   eval      ST_testa = 'Y'
154200041023     c                   endif
154300041025     c                   endif
154400041023      * stampo testate se richiesta stampa del dettaglio se prima volta del P:O: o se OF
154500041026     c                   If        id0toi = 'N'
154600041025     c                   If        ST_testa = 'Y' or *in90
154700041023      * testa
154800041023     c                   write     Dnd1t1
154900041023     c                   write     Dnd1tS
155000041025     c                   write     Dnd1t3
155100041025     c   01              write     Dnd1t4
155200041025     c                   write     Dnd1t5
155300041025     c                   write     Dnd1t3
155400041025     c                   write     Dnd1t2
155500041025     c                   write     Dnd1t3
155600041023      *
155700041025     c   90              setoff                                       90
155800041025      *
155900041025     c                   clear                   ST_testa
156000041025      *
156100041023     c                   endif
156200041103      *
156300041103      * La priorità la stampo solo se diversa da  9
156400041103     c                   clear                   decprt
156500041103     c                   if        tgdprt <> '9'
156600041103     c                   eval      decprt = tgdprt
156700041103     c                   endif
156800041103      *
156900041103      * decodifico
157000140130      *?(già fatto in fase di selezione)?
157100140205     c                   if        yy = *zero  or  TGDmad <> sk_MAR(yy)
157200140205     c                   eval      yy = %lookup( TGDmad : sk_MAR )
157300140205     c                   endif
157400140205     c                   if        yy > *zero
157500140205     c                   eval      DecMAD = sk_MARd(yy)
157600140205     c                   else
157700140205     c                   eval      DecMAD = *all'? '
157800140205     c                   endif
157900140130      *
158000041103      * chi chiama  solo mittente destinatario cliente
158100041103
158200041103     c                   select
158300041103
158400041103     c                   when      tgdtch ='M'
158500041201     c                   eval      dectch = 'MIT/CLI'
158600041103
158700041103     c                   when      tgdtch ='D'
158800041103     c                   eval      dectch = 'DESTIN.'
158900041103
159000041103     c                   when      tgdtch ='C'
159100041201     c                   eval      dectch = 'MIT/CLI'
159200041103
159300041103     c                   other
159400041103     c                   eval      dectch = 'ALTRO  '
159500041103
159600041103     c                   endsl
159700041103
159800140130      *
159900041103     c                   clear                   tibs02ds
160000041103     C                   MOVEL     'C'           t02mod
160100041103     C                   MOVEL     knsif         t02sif
160200041103     C                   MOVEL     'COR'         t02cod
160300041103     C                   MOVEL     tgdtco        t02ke1
160400041103      *
160500041103     C                   CALL      'TIBS02R'
160600041103     C                   PARM                    KPJBA
160700041103     C                   PARM                    TIBS02DS
160800041103      *
160900041103     C                   movel     T02uni        DCOR
161000041103     C                   movel(p)  §CORdesc      dectco
161100140130      *
161200041103
161300041103     c                   if        tgdcdt <> *blanks
161400041103     c                   clear                   tibs02ds
161500041103     C                   MOVEL     'C'           t02mod
161600041103     C                   MOVEL     knsif         t02sif
161700041103     C                   MOVEL     'CHR'         t02cod
161800041103     C                   MOVEL     tgdcdt        t02ke1
161900041103      *
162000041103     C                   CALL      'TIBS02R'
162100041103     C                   PARM                    KPJBA
162200041103     C                   PARM                    TIBS02DS
162300041103      *
162400041103     C                   movel     T02uni        DCHR
162500041103     C                   movel(p)  §CHRdesc      deccdt
162600140130      *
162700041103     c                   else
162800041103     c                   clear                   deccdt
162900041103     c                   endif
163000041023      *
163100041023      * stampo la riga di dettaglio 1 + note se ci sono
163200041023      *
163300041025     c                   write     dnd1d03
163400041023      *
163500041025     c                   write     dnd1d04
163600041026
163700041026     c                   endif
163800041023      *
163900041023      * C A L C O L I
164000041023      *
164100041026
164200041026      * mi salvo la schiera dei motivi d'apertura per p.o.
164300041026
164400140130      *
164500041026     c     tgdmad        lookup    skmad                                  30
164600041026      *
164700041026     c                   if        not %found
164800041026     c                   add       1             mm
164900041026     c                   eval      skmad(mm) = tgdmad
165000041026     c                   endif
165100041023
165200050331     c                   SELECT
165300050331
165400041023      * Aperte suddivise per motivo
165500041023
165600050331     c                   WHEN      tgdcdt = *blanks
165700041023
165800050331     c                   select
165900050331     c                   when      TGDmad = '  1'
166000041023     c                   add       1             raca01
166100041023
166200050331     c                   when      TGDmad = '  2'
166300041023     c                   add       1             raca02
166400041023
166500050331     c                   when      TGDmad = '  3'
166600041023     c                   add       1             raca03
166700041023
166800050331     c                   when      TGDmad = '  5'
166900041023     c                   add       1             raca05
167000041023
167100050331     c                   when      TGDmad = '  6'
167200041023     c                   add       1             raca06
167300041023
167400050331     c                   when      TGDmad = '  7'
167500041023     c                   add       1             raca07
167600041023
167700050331     c                   when      TGDmad = ' 10'
167800041023     c                   add       1             raca10
167900041023
168000050331     c                   when      TGDmad = ' 11'
168100041023     c                   add       1             raca11
168200041023
168300050331     c                   when      TGDmad = ' 12'
168400041023     c                   add       1             raca12
168500041023
168600050331     c                   when      TGDmad = ' 13'
168700041023     c                   add       1             raca13
168800070710
168900070710     c                   when      TGDmad = ' 14'
169000070710     c                   add       1             raca14
169100070710
169200070710     c                   when      TGDmad = ' 15'
169300070710     c                   add       1             raca15
169400070710
169500070710     c                   when      TGDmad = ' 16'
169600070710     c                   add       1             raca16
169700041023
169800050331     c                   when      TGDmad = ' 20'
169900041023     c                   add       1             raca20
170000041023
170100050331     c                   when      TGDmad = ' 21'
170200041023     c                   add       1             raca21
170300041023
170400050331     c                   when      TGDmad = ' 22'
170500041023     c                   add       1             raca22
170600041023
170700050331     c                   when      TGDmad = ' 30'
170800041023     c                   add       1             raca30
170900120914
171000120914     c                   when      TGDmad = ' 50'
171100120914     c                   add       1             raca50
171200041023
171300050331     c                   when      TGDmad = ' 99'
171400041023     c                   add       1             raca99
171500130515
171600130515     c                   when      TGDmad = '  8'
171700130515     c                   add       1             racaX1
171800140409
171900140409     c                   when      TGDmad = ' 88'
172000140409     c                   add       1             racaX2
172100151202
172200151202     c                   when      TGDmad = ' 17'
172300151202     c                   add       1             raca17
172400151202
172500151202     c                   when      TGDmad = ' 18'
172600151202     c                   add       1             raca18
172700050331     c                   endsl
172800041023
172900041023      * aperte da x giorni suddivisi per motivo
173000041023
173100041023     c                   If        Id0xgg > 0 and id0ord = '1'  and
173200041023      *
173300041023      * controllo la data apertura con la data di oggi - 3 giorni lavorativi se minore
173400041023      * lo conteggio negli aperti da xx giorni suddivisi per motivo
173500041023      *
173600041025     c                             tgddar <= wdataoggi
173700041023      *
173800050331     c                   select
173900050331     c                   when      TGDmad = '  1'
174000041023     c                   add       1             racg01
174100041023
174200050331     c                   when      TGDmad = '  2'
174300041023     c                   add       1             racg02
174400041023
174500050331     c                   when      TGDmad = '  3'
174600041023     c                   add       1             racg03
174700041023
174800050331     c                   when      TGDmad = '  5'
174900041023     c                   add       1             racg05
175000041023
175100050331     c                   when      TGDmad = '  6'
175200041023     c                   add       1             racg06
175300041023
175400050331     c                   when      TGDmad = '  7'
175500041023     c                   add       1             racg07
175600041023
175700050331     c                   when      TGDmad = ' 10'
175800041023     c                   add       1             racg10
175900041023
176000050331     c                   when      TGDmad = ' 11'
176100041023     c                   add       1             racg11
176200041023
176300050331     c                   when      TGDmad = ' 12'
176400041023     c                   add       1             racg12
176500041023
176600050331     c                   when      TGDmad = ' 13'
176700041023     c                   add       1             racg13
176800041023
176900070710     c                   when      TGDmad = ' 14'
177000070710     c                   add       1             racg14
177100070710
177200070710     c                   when      TGDmad = ' 15'
177300070710     c                   add       1             racg15
177400070710
177500070710     c                   when      TGDmad = ' 16'
177600070710     c                   add       1             racg16
177700070710
177800050331     c                   when      TGDmad = ' 20'
177900041023     c                   add       1             racg20
178000041023
178100050331     c                   when      TGDmad = ' 21'
178200041023     c                   add       1             racg21
178300041023
178400050331     c                   when      TGDmad = ' 22'
178500041023     c                   add       1             racg22
178600041023
178700050331     c                   when      TGDmad = ' 30'
178800041023     c                   add       1             racg30
178900120914
179000120914     c                   when      TGDmad = ' 50'
179100120914     c                   add       1             racg50
179200041023
179300050331     c                   when      TGDmad = ' 99'
179400041023     c                   add       1             racg99
179500130515
179600130515     c                   when      TGDmad = '  8'
179700130515     c                   add       1             racgX1
179800140409
179900140409     c                   when      TGDmad = ' 88'
180000140409     c                   add       1             racgX2
180100151202
180200151202     c                   when      TGDmad = ' 17'
180300151202     c                   add       1             racg17
180400151202
180500151202     c                   when      TGDmad = ' 18'
180600151202     c                   add       1             racg18
180700050331     c                   endsl
180800050331
180900041023     c                   endif
181000041023
181100041023      * CHIUSI Fondati     suddivisi per motivo
181200041023
181300050331     c                   WHEN      tgdcdt = '  F'
181400041023      *
181500050331     c                   select
181600050331     c                   when      TGDmad = '  1'
181700041023     c                   add       1             racf01
181800041023
181900050331     c                   when      TGDmad = '  2'
182000041023     c                   add       1             racf02
182100041023
182200050331     c                   when      TGDmad = '  3'
182300041023     c                   add       1             racf03
182400041023
182500050331     c                   when      TGDmad = '  5'
182600041023     c                   add       1             racf05
182700041023
182800050331     c                   when      TGDmad = '  6'
182900041023     c                   add       1             racf06
183000041023
183100050331     c                   when      TGDmad = '  7'
183200041023     c                   add       1             racf07
183300041023
183400050331     c                   when      TGDmad = ' 10'
183500041023     c                   add       1             racf10
183600041023
183700050331     c                   when      TGDmad = ' 11'
183800041023     c                   add       1             racf11
183900041023
184000050331     c                   when      TGDmad = ' 12'
184100041023     c                   add       1             racf12
184200041023
184300050331     c                   when      TGDmad = ' 13'
184400041023     c                   add       1             racf13
184500070710
184600070710     c                   when      TGDmad = ' 14'
184700070710     c                   add       1             racf14
184800070710
184900070710     c                   when      TGDmad = ' 15'
185000070710     c                   add       1             racf15
185100070710
185200070710     c                   when      TGDmad = ' 16'
185300070710     c                   add       1             racf16
185400041023
185500050331     c                   when      TGDmad = ' 20'
185600041023     c                   add       1             racf20
185700041023
185800050331     c                   when      TGDmad = ' 21'
185900041023     c                   add       1             racf21
186000041023
186100050331     c                   when      TGDmad = ' 22'
186200041023     c                   add       1             racf22
186300041023
186400050331     c                   when      TGDmad = ' 30'
186500041023     c                   add       1             racf30
186600120914
186700120914     c                   when      TGDmad = ' 50'
186800120914     c                   add       1             racf50
186900041023
187000050331     c                   when      TGDmad = ' 99'
187100041023     c                   add       1             racf99
187200130515
187300130515     c                   when      TGDmad = '  8'
187400130515     c                   add       1             racfX1
187500140409
187600140409     c                   when      TGDmad = ' 88'
187700140409     c                   add       1             racfX2
187800151202
187900151202     c                   when      TGDmad = ' 17'
188000151202     c                   add       1             racf17
188100151202
188200151202     c                   when      TGDmad = ' 18'
188300151202     c                   add       1             racf18
188400050331     c                   endsl
188500041023
188600041023      * CHIUSI Fondati     suddivisi per motivo    P.O. apertura diverso da P.O responsabile
188700041023
188800041023     c                   If        tgdcdt = '  F'  and tgdpor <> tgdpot
188900041023      *
189000050331     c                   select
189100050331     c                   when      TGDmad = '  1'
189200041023     c                   add       1             rach01
189300041023
189400050331     c                   when      TGDmad = '  2'
189500041023     c                   add       1             rach02
189600041023
189700050331     c                   when      TGDmad = '  3'
189800041023     c                   add       1             rach03
189900041023
190000050331     c                   when      TGDmad = '  5'
190100041023     c                   add       1             rach05
190200041023
190300050331     c                   when      TGDmad = '  6'
190400041023     c                   add       1             rach06
190500041023
190600050331     c                   when      TGDmad = '  7'
190700041023     c                   add       1             rach07
190800041023
190900050331     c                   when      TGDmad = ' 10'
191000041023     c                   add       1             rach10
191100041023
191200050331     c                   when      TGDmad = ' 11'
191300041023     c                   add       1             rach11
191400041023
191500050331     c                   when      TGDmad = ' 12'
191600041023     c                   add       1             rach12
191700041023
191800050331     c                   when      TGDmad = ' 13'
191900041023     c                   add       1             rach13
192000041023
192100070710     c                   when      TGDmad = ' 14'
192200070710     c                   add       1             rach14
192300070710
192400070710     c                   when      TGDmad = ' 15'
192500070710     c                   add       1             rach15
192600070710
192700070710     c                   when      TGDmad = ' 16'
192800070710     c                   add       1             rach16
192900070710
193000050331     c                   when      TGDmad = ' 20'
193100041023     c                   add       1             rach20
193200041023
193300050331     c                   when      TGDmad = ' 21'
193400041023     c                   add       1             rach21
193500041023
193600050331     c                   when      TGDmad = ' 22'
193700041023     c                   add       1             rach22
193800041023
193900050331     c                   when      TGDmad = ' 30'
194000041023     c                   add       1             rach30
194100120914
194200120914     c                   when      TGDmad = ' 50'
194300120914     c                   add       1             rach50
194400041023
194500050331     c                   when      TGDmad = ' 99'
194600041023     c                   add       1             rach99
194700130515
194800130515     c                   when      TGDmad = '  8'
194900130515     c                   add       1             rachX1
195000140409
195100140409     c                   when      TGDmad = ' 88'
195200140409     c                   add       1             rachX2
195300151202
195400151202     c                   when      TGDmad = ' 17'
195500151202     c                   add       1             rach17
195600151202
195700151202     c                   when      TGDmad = ' 18'
195800151202     c                   add       1             rach18
195900050331     c                   endsl
196000041023      *
196100041023     c                   endif
196200041023
196300041023      * CHIUSI InFondati     suddivisi per motivo
196400041023
196500050331     c                   WHEN      tgdcdt = '  N'
196600041023      *
196700050331     c                   select
196800050331     c                   when      TGDmad = '  1'
196900041023     c                   add       1             racn01
197000041023
197100050331     c                   when      TGDmad = '  2'
197200041023     c                   add       1             racn02
197300041023
197400050331     c                   when      TGDmad = '  3'
197500041023     c                   add       1             racn03
197600041023
197700050331     c                   when      TGDmad = '  5'
197800041023     c                   add       1             racn05
197900041023
198000050331     c                   when      TGDmad = '  6'
198100041023     c                   add       1             racn06
198200041023
198300050331     c                   when      TGDmad = '  7'
198400041023     c                   add       1             racn07
198500041023
198600050331     c                   when      TGDmad = ' 10'
198700041023     c                   add       1             racn10
198800041023
198900050331     c                   when      TGDmad = ' 11'
199000041023     c                   add       1             racn11
199100041023
199200050331     c                   when      TGDmad = ' 12'
199300041023     c                   add       1             racn12
199400041023
199500050331     c                   when      TGDmad = ' 13'
199600041023     c                   add       1             racn13
199700041023
199800070710     c                   when      TGDmad = ' 14'
199900070710     c                   add       1             racn14
200000070710
200100070710     c                   when      TGDmad = ' 15'
200200070710     c                   add       1             racn15
200300070710
200400070710     c                   when      TGDmad = ' 16'
200500070710     c                   add       1             racn16
200600070710
200700050331     c                   when      TGDmad = ' 20'
200800041023     c                   add       1             racn20
200900041023
201000050331     c                   when      TGDmad = ' 21'
201100041023     c                   add       1             racn21
201200041023
201300050331     c                   when      TGDmad = ' 22'
201400041023     c                   add       1             racn22
201500041023
201600050331     c                   when      TGDmad = ' 30'
201700041023     c                   add       1             racn30
201800120914
201900120914     c                   when      TGDmad = ' 50'
202000120914     c                   add       1             racn50
202100041023
202200050331     c                   when      TGDmad = ' 99'
202300041023     c                   add       1             racn99
202400130515
202500130515     c                   when      TGDmad = '  8'
202600130515     c                   add       1             racnX1
202700140409
202800140409     c                   when      TGDmad = ' 88'
202900140409     c                   add       1             racnX2
203000151202
203100151202     c                   when      TGDmad = ' 17'
203200151202     c                   add       1             racn17
203300151202
203400151202     c                   when      TGDmad = ' 18'
203500151202     c                   add       1             racn18
203600050331     c                   endsl
203700041023
203800041023      * CHIUSI InFormativi   suddivisi per motivo
203900041023
204000050331     c                   WHEN      tgdcdt = '  I'
204100041023      *
204200050331     c                   select
204300050331     c                   when      TGDmad = '  1'
204400041023     c                   add       1             raci01
204500041023
204600050331     c                   when      TGDmad = '  2'
204700041023     c                   add       1             raci02
204800041023
204900050331     c                   when      TGDmad = '  3'
205000041023     c                   add       1             raci03
205100041023
205200050331     c                   when      TGDmad = '  5'
205300041023     c                   add       1             raci05
205400041023
205500050331     c                   when      TGDmad = '  6'
205600041023     c                   add       1             raci06
205700041023
205800050331     c                   when      TGDmad = '  7'
205900041023     c                   add       1             raci07
206000041023
206100050331     c                   when      TGDmad = ' 10'
206200041023     c                   add       1             raci10
206300041023
206400050331     c                   when      TGDmad = ' 11'
206500041023     c                   add       1             raci11
206600041023
206700050331     c                   when      TGDmad = ' 12'
206800041023     c                   add       1             raci12
206900041023
207000050331     c                   when      TGDmad = ' 13'
207100041023     c                   add       1             raci13
207200041023
207300070710     c                   when      TGDmad = ' 14'
207400070710     c                   add       1             raci14
207500070710
207600070710     c                   when      TGDmad = ' 15'
207700070710     c                   add       1             raci15
207800070710
207900070710     c                   when      TGDmad = ' 16'
208000070710     c                   add       1             raci16
208100070710
208200050331     c                   when      TGDmad = ' 20'
208300041023     c                   add       1             raci20
208400041023
208500050331     c                   when      TGDmad = ' 21'
208600041023     c                   add       1             raci21
208700041023
208800050331     c                   when      TGDmad = ' 22'
208900041023     c                   add       1             raci22
209000041023
209100050331     c                   when      TGDmad = ' 30'
209200041023     c                   add       1             raci30
209300120914
209400120914     c                   when      TGDmad = ' 50'
209500120914     c                   add       1             raci50
209600041023
209700050331     c                   when      TGDmad = ' 99'
209800041023     c                   add       1             raci99
209900130515
210000130515     c                   when      TGDmad = '  8'
210100130515     c                   add       1             raciX1
210200140409
210300140409     c                   when      TGDmad = ' 88'
210400140409     c                   add       1             raciX2
210500151202
210600151202     c                   when      TGDmad = ' 17'
210700151202     c                   add       1             raci17
210800151202
210900151202     c                   when      TGDmad = ' 18'
211000151202     c                   add       1             raci18
211100050331     c                   endsl
211200041023      *
211300050331     c                   ENDSL
211400050331
211500050331     c                   SELECT
211600041023
211700041023      * Chiamante Destinatario suddivisi per motivo
211800041023
211900050331     c                   WHEN      tgdtch = 'D'
212000041023      *
212100050331     c                   select
212200050331     c                   when      TGDmad = '  1'
212300041023     c                   add       1             racd01
212400041023
212500050331     c                   when      TGDmad = '  2'
212600041023     c                   add       1             racd02
212700041023
212800050331     c                   when      TGDmad = '  3'
212900041023     c                   add       1             racd03
213000041023
213100050331     c                   when      TGDmad = '  5'
213200041023     c                   add       1             racd05
213300041023
213400050331     c                   when      TGDmad = '  6'
213500041023     c                   add       1             racd06
213600041023
213700050331     c                   when      TGDmad = '  7'
213800041023     c                   add       1             racd07
213900041023
214000050331     c                   when      TGDmad = ' 10'
214100041023     c                   add       1             racd10
214200041023
214300050331     c                   when      TGDmad = ' 11'
214400041023     c                   add       1             racd11
214500041023
214600050331     c                   when      TGDmad = ' 12'
214700041023     c                   add       1             racd12
214800041023
214900050331     c                   when      TGDmad = ' 13'
215000041023     c                   add       1             racd13
215100041023
215200070710     c                   when      TGDmad = ' 14'
215300070710     c                   add       1             racd14
215400070710
215500070710     c                   when      TGDmad = ' 15'
215600070710     c                   add       1             racd15
215700070710
215800070710     c                   when      TGDmad = ' 16'
215900070710     c                   add       1             racd16
216000070710
216100050331     c                   when      TGDmad = ' 20'
216200041023     c                   add       1             racd20
216300041023
216400050331     c                   when      TGDmad = ' 21'
216500041023     c                   add       1             racd21
216600041023
216700050331     c                   when      TGDmad = ' 22'
216800041023     c                   add       1             racd22
216900041023
217000050331     c                   when      TGDmad = ' 30'
217100041023     c                   add       1             racd30
217200120914
217300120914     c                   when      TGDmad = ' 50'
217400120914     c                   add       1             racd50
217500041023
217600050331     c                   when      TGDmad = ' 99'
217700041023     c                   add       1             racd99
217800130515
217900130515     c                   when      TGDmad = '  8'
218000130515     c                   add       1             racdX1
218100140409
218200140409     c                   when      TGDmad = ' 88'
218300140409     c                   add       1             racdX2
218400151202
218500151202     c                   when      TGDmad = ' 17'
218600151202     c                   add       1             racd17
218700151202
218800151202     c                   when      TGDmad = ' 18'
218900151202     c                   add       1             racd18
219000050331     c                   endsl
219100041023
219200041023      * Chiamante Mittente     suddivisi per motivo
219300041023
219400050331     c                   WHEN      tgdtch = 'M'
219500041023      *
219600050331     c                   select
219700050331     c                   when      TGDmad = '  1'
219800041023     c                   add       1             racm01
219900041023
220000050331     c                   when      TGDmad = '  2'
220100041023     c                   add       1             racm02
220200041023
220300050331     c                   when      TGDmad = '  3'
220400041023     c                   add       1             racm03
220500041023
220600050331     c                   when      TGDmad = '  5'
220700041023     c                   add       1             racm05
220800041023
220900050331     c                   when      TGDmad = '  6'
221000041023     c                   add       1             racm06
221100041023
221200050331     c                   when      TGDmad = '  7'
221300041023     c                   add       1             racm07
221400041023
221500050331     c                   when      TGDmad = ' 10'
221600041023     c                   add       1             racm10
221700041023
221800050331     c                   when      TGDmad = ' 11'
221900041023     c                   add       1             racm11
222000041023
222100050331     c                   when      TGDmad = ' 12'
222200041023     c                   add       1             racm12
222300041023
222400050331     c                   when      TGDmad = ' 13'
222500041023     c                   add       1             racm13
222600041023
222700070710     c                   when      TGDmad = ' 14'
222800070710     c                   add       1             racm14
222900070710
223000070710     c                   when      TGDmad = ' 15'
223100070710     c                   add       1             racm15
223200070710
223300070710     c                   when      TGDmad = ' 16'
223400070710     c                   add       1             racm16
223500070710
223600050331     c                   when      TGDmad = ' 20'
223700041023     c                   add       1             racm20
223800041023
223900050331     c                   when      TGDmad = ' 21'
224000041023     c                   add       1             racm21
224100041023
224200050331     c                   when      TGDmad = ' 22'
224300041023     c                   add       1             racm22
224400041023
224500050331     c                   when      TGDmad = ' 30'
224600041023     c                   add       1             racm30
224700120914
224800120914     c                   when      TGDmad = ' 50'
224900120914     c                   add       1             racm50
225000041023
225100050331     c                   when      TGDmad = ' 99'
225200041023     c                   add       1             racm99
225300130515
225400130515     c                   when      TGDmad = '  8'
225500130515     c                   add       1             racmX1
225600140409
225700140409     c                   when      TGDmad = ' 88'
225800140409     c                   add       1             racmX2
225900151202
226000151202     c                   when      TGDmad = ' 17'
226100151202     c                   add       1             racm17
226200151202
226300151202     c                   when      TGDmad = ' 18'
226400151202     c                   add       1             racm18
226500050331     c                   endsl
226600041023
226700041023      * Chiamante Cliente      suddivisi per motivo
226800041023
226900050331     c                   WHEN      tgdtch = 'C'
227000041023      *
227100050331     c                   select
227200050331     c                   when      TGDmad = '  1'
227300041023     c                   add       1             racc01
227400041023
227500050331     c                   when      TGDmad = '  2'
227600041023     c                   add       1             racc02
227700041023
227800050331     c                   when      TGDmad = '  3'
227900041023     c                   add       1             racc03
228000041023
228100050331     c                   when      TGDmad = '  5'
228200041023     c                   add       1             racc05
228300041023
228400050331     c                   when      TGDmad = '  6'
228500041023     c                   add       1             racc06
228600041023
228700050331     c                   when      TGDmad = '  7'
228800041023     c                   add       1             racc07
228900041023
229000050331     c                   when      TGDmad = ' 10'
229100041023     c                   add       1             racc10
229200041023
229300050331     c                   when      TGDmad = ' 11'
229400041023     c                   add       1             racc11
229500041023
229600050331     c                   when      TGDmad = ' 12'
229700041023     c                   add       1             racc12
229800041023
229900050331     c                   when      TGDmad = ' 13'
230000041023     c                   add       1             racc13
230100041023
230200070710     c                   when      TGDmad = ' 14'
230300070710     c                   add       1             racc14
230400070710
230500070710     c                   when      TGDmad = ' 15'
230600070710     c                   add       1             racc15
230700070710
230800070710     c                   when      TGDmad = ' 16'
230900070710     c                   add       1             racc16
231000070710
231100050331     c                   when      TGDmad = ' 20'
231200041023     c                   add       1             racc20
231300041023
231400050331     c                   when      TGDmad = ' 21'
231500041023     c                   add       1             racc21
231600041023
231700050331     c                   when      TGDmad = ' 22'
231800041023     c                   add       1             racc22
231900041023
232000050331     c                   when      TGDmad = ' 30'
232100041023     c                   add       1             racc30
232200120914
232300120914     c                   when      TGDmad = ' 50'
232400120914     c                   add       1             racc50
232500041023
232600050331     c                   when      TGDmad = ' 99'
232700041023     c                   add       1             racc99
232800130515
232900130515     c                   when      TGDmad = '  8'
233000130515     c                   add       1             raccX1
233100140409
233200140409     c                   when      TGDmad = ' 88'
233300140409     c                   add       1             raccX2
233400151202
233500151202     c                   when      TGDmad = ' 17'
233600151202     c                   add       1             racc17
233700151202
233800151202     c                   when      TGDmad = ' 18'
233900151202     c                   add       1             racc18
234000050331     c                   endsl
234100041023
234200041023      * Chiamante Altro        suddivisi per motivo
234300041023
234400050331     c                   OTHER
234500041023      *
234600050331     c                   select
234700050331     c                   when      TGDmad = '  1'
234800041023     c                   add       1             racx01
234900041023
235000050331     c                   when      TGDmad = '  2'
235100041023     c                   add       1             racx02
235200041023
235300050331     c                   when      TGDmad = '  3'
235400041023     c                   add       1             racx03
235500041023
235600050331     c                   when      TGDmad = '  5'
235700041023     c                   add       1             racx05
235800041023
235900050331     c                   when      TGDmad = '  6'
236000041023     c                   add       1             racx06
236100041023
236200050331     c                   when      TGDmad = '  7'
236300041023     c                   add       1             racx07
236400041023
236500050331     c                   when      TGDmad = ' 10'
236600041023     c                   add       1             racx10
236700041023
236800050331     c                   when      TGDmad = ' 11'
236900041023     c                   add       1             racx11
237000041023
237100050331     c                   when      TGDmad = ' 12'
237200041023     c                   add       1             racx12
237300041023
237400050331     c                   when      TGDmad = ' 13'
237500041023     c                   add       1             racx13
237600041023
237700070710     c                   when      TGDmad = ' 14'
237800070710     c                   add       1             racx14
237900070710
238000070710     c                   when      TGDmad = ' 15'
238100070710     c                   add       1             racx15
238200070710
238300070710     c                   when      TGDmad = ' 16'
238400070710     c                   add       1             racx16
238500070710
238600050331     c                   when      TGDmad = ' 20'
238700041023     c                   add       1             racx20
238800041023
238900050331     c                   when      TGDmad = ' 21'
239000041023     c                   add       1             racx21
239100041023
239200050331     c                   when      TGDmad = ' 22'
239300041023     c                   add       1             racx22
239400041023
239500050331     c                   when      TGDmad = ' 30'
239600041023     c                   add       1             racx30
239700120914
239800120914     c                   when      TGDmad = ' 50'
239900120914     c                   add       1             racx50
240000041023
240100050331     c                   when      TGDmad = ' 99'
240200041023     c                   add       1             racx99
240300130515
240400130515     c                   when      TGDmad = '  8'
240500130515     c                   add       1             racxX1
240600140409
240700140409     c                   when      TGDmad = ' 88'
240800140409     c                   add       1             racxX2
240900151202
241000151202     c                   when      TGDmad = ' 17'
241100151202     c                   add       1             racx17
241200151202
241300151202     c                   when      TGDmad = ' 18'
241400151202     c                   add       1             racx18
241500050331     c                   endsl
241600041023      *
241700050331     c                   ENDSL
241800041023      *
241900041023
242000041023      * tipo comunicazione
242100050331     c                   select
242200041023      * fax
242300050331     c                   when      tgdtco = 'F'
242400041023     c                   add       1             ractcf
242500041023      * mail
242600050331     c                   when      tgdtco = 'M'
242700041023     c                   add       1             ractcm
242800041023      * lettera
242900050331     c                   when      tgdtco = 'L'
243000041023     c                   add       1             ractcl
243100041023      * telefono
243200050331     c                   when      tgdtco = 'T'
243300041023     c                   add       1             ractct
243400050331     c                   endsl
243500041023      *
243600041023      * aggancio record dell'utente
243700041023      *
243800041027     c                   if        Id0ord = '1' and not *in01
243900041023     c                   if        tgdcdt <> *blanks
244000041023     c     tgdutc        chain     Wfrut00f
244100041023     c                   else
244200041023     C     tgdrpg        chain     Wfrut00f
244300041023     c                   endif
244400041023     c                   if        not %found(wfrut00f)
244500041023     c                   clear                   wfrut000
244600041023     c                   if        tgdcdt <> *blanks
244700041023     c                   eval      rutute = tgdutc
244800041023     c                   else
244900041023     c                   eval      rutute = tgdrpg
245000041023     c                   endif
245100041023     c                   endif
245200041023
245300041023     c                   if        tgdcdt = *blanks
245400041023      * le aperte
245500041023     c                   add       1             rutape
245600041023      * le aperte  da xx giorni
245700041023     c                   If        Id0xgg > 0 and id0ord = '1'  and
245800041025     c                             tgddar <= wdataoggi
245900041023     c                   add       1             rutdxg
246000041023     c                   endif
246100041023      *
246200041023      * chiusi
246300041023      *
246400041023     c                   else
246500050331     c                   select
246600041023      * fondati
246700050331     c                   when      tgdcdt = '  F'
246800041023     c                   add       1             rutfon
246900041023      * chiusi da altri
247000041023     c                   if        tgdpor <> tgdpot
247100041023     c                   add       1             rutcfa
247200041023     c                   endif
247300041023      * infondati
247400050331     c                   when      tgdcdt = '  N'
247500041023     c                   add       1             rutifo
247600041023      * informativo
247700050331     c                   when      tgdcdt = '  I'
247800041023     c                   add       1             rutinf
247900050331     c                   endsl
248000041023
248100041023     c                   endif
248200041023
248300041023      * tipo comunicazione
248400041023
248500050331     c                   select
248600041023      * fax
248700050331     c                   when      tgdtco = 'F'
248800041023     c                   add       1             ruttcf
248900041023      * mail
249000050331     c                   when      tgdtco = 'M'
249100041023     c                   add       1             ruttcm
249200041023      * lettera
249300050331     c                   when      tgdtco = 'L'
249400041023     c                   add       1             ruttcl
249500041023      * telefono
249600050331     c                   when      tgdtco = 'T'
249700041023     c                   add       1             ruttct
249800050331     c                   endsl
249900141203
250000141203      /free
250100141203       //?Chi chiama
250200141203         SELECT;
250300141203         WHEN  TGDtch = 'D';
250400141203           RUTccd += 1;
250500141203         WHEN  TGDtch = 'M';
250600141203           RUTccm += 1;
250700141203         WHEN  TGDtch = 'C';
250800141203           RUTccc += 1;
250900141203         OTHER;
251000141203           RUTcca += 1;
251100141203         ENDSL;
251200141203      /end-free
251300141203
251400041022      *
251500041023     c                   if        not %found(wfrut00f)
251600041025     c                   write     wfrut000
251700041023     c                   else
251800041025     c                   update    wfrut000
251900041023     c                   endif
252000041025     c                   endif
252100041023      *
252200041023     c                   endsr
252300121116
252400121116      /free
252500121116       //--------------------------------------------------------------
252600121116       //?Memorizzo sul file di work per dettaglio chiamante.
252700121116       //--------------------------------------------------------------
252800121116       BEGSR MemoTCH;
252900121116
253000121116       //?Aggancio il file di work x tipo chiamante
253100121116         chain (RACdis:RACare:RACpoe:RACtpo:TGDtch) WFRAD01L;
253200121116         IF  not %found(WFRAD01L);
253300121116           clear WFRAD000;
253400121116           RADtpe = RACtpe;
253500121116           RADdte = RACdte;
253600121116           RADddr = RACddr;
253700121116           RADadr = RACadr;
253800121116           RADddc = RACddc;
253900121116           RADadc = RACadc;
254000121116           RADpoe = RACpoe;
254100121116           RADtpo = RACtpo;
254200121116           RADdis = RACdis;
254300121116           RADare = RACare;
254400121116           RADtch = TGDtch;
254500121116           exsr prepRAD;
254600121116           WRITE  WFRAD000;
254700121116         ELSE;
254800121116           exsr prepRAD;
254900121116           UPDATE WFRAD000;
255000121116         ENDIF;
255100121116
255200121116       ENDSR;
255300121116       //--------------------------------------------------------------
255400121116       //?Preparo i campi per il file WFRAD00F.
255500121116       //--------------------------------------------------------------
255600121116       BEGSR prepRAD;
255700121119
255800121119         SELECT;
255900121119       //?Aperti
256000121119           WHEN  TGDcdt = *blanks;
256100121119         //?Suddivisi per motivo
256200121119             SELECT;
256300121119               WHEN  TGDmad = '  1';
256400121119                 RADa01 += 1;
256500121119               WHEN  TGDmad = '  2';
256600121119                 RADa02 += 1;
256700121119               WHEN  TGDmad = '  3';
256800121119                 RADa03 += 1;
256900121119               WHEN  TGDmad = '  5';
257000121119                 RADa05 += 1;
257100121119               WHEN  TGDmad = '  6';
257200121119                 RADa06 += 1;
257300121119               WHEN  TGDmad = '  7';
257400121119                 RADa07 += 1;
257500121119               WHEN  TGDmad = ' 10';
257600121119                 RADa10 += 1;
257700121119               WHEN  TGDmad = ' 11';
257800121119                 RADa11 += 1;
257900121119               WHEN  TGDmad = ' 12';
258000121119                 RADa12 += 1;
258100121119               WHEN  TGDmad = ' 13';
258200121119                 RADa13 += 1;
258300121119               WHEN  TGDmad = ' 14';
258400121119                 RADa14 += 1;
258500121119               WHEN  TGDmad = ' 15';
258600121119                 RADa15 += 1;
258700121119               WHEN  TGDmad = ' 16';
258800121119                 RADa16 += 1;
258900121119               WHEN  TGDmad = ' 20';
259000121119                 RADa20 += 1;
259100121119               WHEN  TGDmad = ' 21';
259200121119                 RADa21 += 1;
259300121119               WHEN  TGDmad = ' 22';
259400121119                 RADa22 += 1;
259500121119               WHEN  TGDmad = ' 30';
259600121119                 RADa30 += 1;
259700121119               WHEN  TGDmad = ' 50';
259800121119                 RADa50 += 1;
259900121119               WHEN  TGDmad = ' 99';
260000121119                 RADa99 += 1;
260100130515               WHEN  TGDmad = '  8';
260200130515                 RADaX1 += 1;
260300140409               WHEN  TGDmad = ' 88';
260400140409                 RADaX2 += 1;
260500151202               WHEN  TGDmad = ' 17';
260600151202                 RADa17 += 1;
260700151202               WHEN  TGDmad = ' 18';
260800151202                 RADa18 += 1;
260900121119             ENDSL;
261000121119
261100121119         //?Aperti da x gg. suddivisi per motivo
261200121119             IF  ID0xgg > 0 and ID0ord = '1' and TGDdar <= wdataoggi;
261300121119           //?Suddivisi per motivo
261400121119               SELECT;
261500121119                 WHEN  TGDmad = '  1';
261600121119                   RADg01 += 1;
261700121119                 WHEN  TGDmad = '  2';
261800121119                   RADg02 += 1;
261900121119                 WHEN  TGDmad = '  3';
262000121119                   RADg03 += 1;
262100121119                 WHEN  TGDmad = '  5';
262200121119                   RADg05 += 1;
262300121119                 WHEN  TGDmad = '  6';
262400121119                   RADg06 += 1;
262500121119                 WHEN  TGDmad = '  7';
262600121119                   RADg07 += 1;
262700121119                 WHEN  TGDmad = ' 10';
262800121119                   RADg10 += 1;
262900121119                 WHEN  TGDmad = ' 11';
263000121119                   RADg11 += 1;
263100121119                 WHEN  TGDmad = ' 12';
263200121119                   RADg12 += 1;
263300121119                 WHEN  TGDmad = ' 13';
263400121119                   RADg13 += 1;
263500121119                 WHEN  TGDmad = ' 14';
263600121119                   RADg14 += 1;
263700121119                 WHEN  TGDmad = ' 15';
263800121119                   RADg15 += 1;
263900121119                 WHEN  TGDmad = ' 16';
264000121119                   RADg16 += 1;
264100121119                 WHEN  TGDmad = ' 20';
264200121119                   RADg20 += 1;
264300121119                 WHEN  TGDmad = ' 21';
264400121119                   RADg21 += 1;
264500121119                 WHEN  TGDmad = ' 22';
264600121119                   RADg22 += 1;
264700121119                 WHEN  TGDmad = ' 30';
264800121119                   RADg30 += 1;
264900121119                 WHEN  TGDmad = ' 50';
265000121119                   RADg50 += 1;
265100121119                 WHEN  TGDmad = ' 99';
265200121119                   RADg99 += 1;
265300130515                 WHEN  TGDmad = '  8';
265400130515                   RADgX1 += 1;
265500140409                 WHEN  TGDmad = ' 88';
265600140409                   RADgX2 += 1;
265700151202                 WHEN  TGDmad = ' 17';
265800151202                   RADg17 += 1;
265900151202                 WHEN  TGDmad = ' 18';
266000151202                   RADg18 += 1;
266100121119               ENDSL;
266200121119             ENDIF;
266300121119
266400121119       //?Chiusi Fondati
266500121119           WHEN  TGDcdt = '  F';
266600121119         //?Suddivisi per motivo
266700121119             SELECT;
266800121119               WHEN  TGDmad = '  1';
266900121119                 RADf01 += 1;
267000121119               WHEN  TGDmad = '  2';
267100121119                 RADf02 += 1;
267200121119               WHEN  TGDmad = '  3';
267300121119                 RADf03 += 1;
267400121119               WHEN  TGDmad = '  5';
267500121119                 RADf05 += 1;
267600121119               WHEN  TGDmad = '  6';
267700121119                 RADf06 += 1;
267800121119               WHEN  TGDmad = '  7';
267900121119                 RADf07 += 1;
268000121119               WHEN  TGDmad = ' 10';
268100121119                 RADf10 += 1;
268200121119               WHEN  TGDmad = ' 11';
268300121119                 RADf11 += 1;
268400121119               WHEN  TGDmad = ' 12';
268500121119                 RADf12 += 1;
268600121119               WHEN  TGDmad = ' 13';
268700121119                 RADf13 += 1;
268800121119               WHEN  TGDmad = ' 14';
268900121119                 RADf14 += 1;
269000121119               WHEN  TGDmad = ' 15';
269100121119                 RADf15 += 1;
269200121119               WHEN  TGDmad = ' 16';
269300121119                 RADf16 += 1;
269400121119               WHEN  TGDmad = ' 20';
269500121119                 RADf20 += 1;
269600121119               WHEN  TGDmad = ' 21';
269700121119                 RADf21 += 1;
269800121119               WHEN  TGDmad = ' 22';
269900121119                 RADf22 += 1;
270000121119               WHEN  TGDmad = ' 30';
270100121119                 RADf30 += 1;
270200121119               WHEN  TGDmad = ' 50';
270300121119                 RADf50 += 1;
270400121119               WHEN  TGDmad = ' 99';
270500121119                 RADf99 += 1;
270600130515               WHEN  TGDmad = '  8';
270700130515                 RADfX1 += 1;
270800140409               WHEN  TGDmad = ' 88';
270900140409                 RADfX2 += 1;
271000151202               WHEN  TGDmad = ' 17';
271100151202                 RADf17 += 1;
271200151202               WHEN  TGDmad = ' 18';
271300151202                 RADf18 += 1;
271400121119             ENDSL;
271500121119
271600121119       //?Chiusi Fondati da altri --> P.O. apertura diverso da P.O. responsabile
271700121119             IF  TGDcdt = '  F'  and TGDpor <> TGDpot;
271800121119         //?Suddivisi per motivo
271900121119               SELECT;
272000121119                 WHEN  TGDmad = '  1';
272100121119                   RADh01 += 1;
272200121119                 WHEN  TGDmad = '  2';
272300121119                   RADh02 += 1;
272400121119                 WHEN  TGDmad = '  3';
272500121119                   RADh03 += 1;
272600121119                 WHEN  TGDmad = '  5';
272700121119                   RADh05 += 1;
272800121119                 WHEN  TGDmad = '  6';
272900121119                   RADh06 += 1;
273000121119                 WHEN  TGDmad = '  7';
273100121119                   RADh07 += 1;
273200121119                 WHEN  TGDmad = ' 10';
273300121119                   RADh10 += 1;
273400121119                 WHEN  TGDmad = ' 11';
273500121119                   RADh11 += 1;
273600121119                 WHEN  TGDmad = ' 12';
273700121119                   RADh12 += 1;
273800121119                 WHEN  TGDmad = ' 13';
273900121119                   RADh13 += 1;
274000121119                 WHEN  TGDmad = ' 14';
274100121119                   RADh14 += 1;
274200121119                 WHEN  TGDmad = ' 15';
274300121119                   RADh15 += 1;
274400121119                 WHEN  TGDmad = ' 16';
274500121119                   RADh16 += 1;
274600121119                 WHEN  TGDmad = ' 20';
274700121119                   RADh20 += 1;
274800121119                 WHEN  TGDmad = ' 21';
274900121119                   RADh21 += 1;
275000121119                 WHEN  TGDmad = ' 22';
275100121119                   RADh22 += 1;
275200121119                 WHEN  TGDmad = ' 30';
275300121119                   RADh30 += 1;
275400121119                 WHEN  TGDmad = ' 50';
275500121119                   RADh50 += 1;
275600121119                 WHEN  TGDmad = ' 99';
275700121119                   RADh99 += 1;
275800130515                 WHEN  TGDmad = '  8';
275900130515                   RADhX1 += 1;
276000140409                 WHEN  TGDmad = ' 88';
276100140409                   RADhX2 += 1;
276200151202                 WHEN  TGDmad = ' 17';
276300151202                   RADh17 += 1;
276400151202                 WHEN  TGDmad = ' 18';
276500151202                   RADh18 += 1;
276600121119               ENDSL;
276700121119             ENDIF;
276800121119
276900121119       //?Chiusi Infondati
277000121119           WHEN  TGDcdt = '  N';
277100121119         //?Suddivisi per motivo
277200121119             SELECT;
277300121119               WHEN  TGDmad = '  1';
277400121119                 RADn01 += 1;
277500121119               WHEN  TGDmad = '  2';
277600121119                 RADn02 += 1;
277700121119               WHEN  TGDmad = '  3';
277800121119                 RADn03 += 1;
277900121119               WHEN  TGDmad = '  5';
278000121119                 RADn05 += 1;
278100121119               WHEN  TGDmad = '  6';
278200121119                 RADn06 += 1;
278300121119               WHEN  TGDmad = '  7';
278400121119                 RADn07 += 1;
278500121119               WHEN  TGDmad = ' 10';
278600121119                 RADn10 += 1;
278700121119               WHEN  TGDmad = ' 11';
278800121119                 RADn11 += 1;
278900121119               WHEN  TGDmad = ' 12';
279000121119                 RADn12 += 1;
279100121119               WHEN  TGDmad = ' 13';
279200121119                 RADn13 += 1;
279300121119               WHEN  TGDmad = ' 14';
279400121119                 RADn14 += 1;
279500121119               WHEN  TGDmad = ' 15';
279600121119                 RADn15 += 1;
279700121119               WHEN  TGDmad = ' 16';
279800121119                 RADn16 += 1;
279900121119               WHEN  TGDmad = ' 20';
280000121119                 RADn20 += 1;
280100121119               WHEN  TGDmad = ' 21';
280200121119                 RADn21 += 1;
280300121119               WHEN  TGDmad = ' 22';
280400121119                 RADn22 += 1;
280500121119               WHEN  TGDmad = ' 30';
280600121119                 RADn30 += 1;
280700121119               WHEN  TGDmad = ' 50';
280800121119                 RADn50 += 1;
280900121119               WHEN  TGDmad = ' 99';
281000121119                 RADn99 += 1;
281100130515               WHEN  TGDmad = '  8';
281200130515                 RADnX1 += 1;
281300140409               WHEN  TGDmad = ' 88';
281400140409                 RADnX2 += 1;
281500151202               WHEN  TGDmad = ' 17';
281600151202                 RADn17 += 1;
281700151202               WHEN  TGDmad = ' 18';
281800151202                 RADn18 += 1;
281900121119             ENDSL;
282000121119
282100121119       //?Chiusi Informativi
282200121119           WHEN  TGDcdt = '  I' ;
282300121119         //?Suddivisi per motivo
282400121119             SELECT;
282500121119               WHEN  TGDmad = '  1';
282600121119                 RADi01 += 1;
282700121119               WHEN  TGDmad = '  2';
282800121119                 RADi02 += 1;
282900121119               WHEN  TGDmad = '  3';
283000121119                 RADi03 += 1;
283100121119               WHEN  TGDmad = '  5';
283200121119                 RADi05 += 1;
283300121119               WHEN  TGDmad = '  6';
283400121119                 RADi06 += 1;
283500121119               WHEN  TGDmad = '  7';
283600121119                 RADi07 += 1;
283700121119               WHEN  TGDmad = ' 10';
283800121119                 RADi10 += 1;
283900121119               WHEN  TGDmad = ' 11';
284000121119                 RADi11 += 1;
284100121119               WHEN  TGDmad = ' 12';
284200121119                 RADi12 += 1;
284300121119               WHEN  TGDmad = ' 13';
284400121119                 RADi13 += 1;
284500121119               WHEN  TGDmad = ' 14';
284600121119                 RADi14 += 1;
284700121119               WHEN  TGDmad = ' 15';
284800121119                 RADi15 += 1;
284900121119               WHEN  TGDmad = ' 16';
285000121119                 RADi16 += 1;
285100121119               WHEN  TGDmad = ' 20';
285200121119                 RADi20 += 1;
285300121119               WHEN  TGDmad = ' 21';
285400121119                 RADi21 += 1;
285500121119               WHEN  TGDmad = ' 22';
285600121119                 RADi22 += 1;
285700121119               WHEN  TGDmad = ' 30';
285800121119                 RADi30 += 1;
285900121119               WHEN  TGDmad = ' 50';
286000121119                 RADi50 += 1;
286100121119               WHEN  TGDmad = ' 99';
286200121119                 RADi99 += 1;
286300130515               WHEN  TGDmad = '  8';
286400130515                 RADiX1 += 1;
286500140409               WHEN  TGDmad = ' 88';
286600140409                 RADiX2 += 1;
286700151202               WHEN  TGDmad = ' 17';
286800151202                 RADi17 += 1;
286900151202               WHEN  TGDmad = ' 18';
287000151202                 RADi18 += 1;
287100121119             ENDSL;
287200121119
287300121119         ENDSL;
287400121119
287500121119       //?Tipo comunicazione
287600121119         SELECT;
287700121119         //?Fax
287800121119           WHEN  TGDtco = 'F';
287900121119             RADtcf += 1;
288000121119         //?Mail
288100121119           WHEN  TGDtco = 'M';
288200121119             RADtcm += 1;
288300121119         //?Lettera
288400121119           WHEN  TGDtco = 'L';
288500121119             RADtcl += 1;
288600121119         //?Telefono
288700121119           WHEN  TGDtco = 'T';
288800121119             RADtct += 1;
288900121119         ENDSL;
289000121116
289100121116       ENDSR;
289200121116      /end-free
289300041022      *
289400041023      *---------------------------------------------------------------*
289500041023      * Scrittura del file di reportistica                            *
289600041023      *---------------------------------------------------------------*
289700041023     c     SR_Report     BegSr
289800041023      *
289900041201      * Calcolo totale spedizioni del periodo
290000041201      *
290100041201
290200041201      * imposto il periodo di ricerca numero spedizioni
290300041201      *
290400050405      * se immesso  un  solo  range di date: "vince" quello
290500050405      * se immessi entrambi i range di date: "vince" quello x data richiesta
290600050405     c                   if        id0amgdcd > *zeros
290700041202     c                   MOVEL     id0amgdcd     periododa
290800041202     c                   MOVEL     id0amgdca     periodoa
290900041201     c                   else
291000041202     c                   MOVEL     id0amgdhd     periododa
291100041202     c                   MOVEL     id0amgdha     periodoa
291200041201     c                   endif
291300060515      * se tipo ordinamento 1 e selezionato un solo PO  metto la  L1 nei po da elaborare
291400060515     c                   if        id0ord = '1' and id0fil > 0
291500041202     c                   clear                   trul06ds
291600041202     c                   move      '£1'          d06cod
291700041202     c                   movel     id0fil        d06key
291800041202     c                   movel(p)  trul06ds      KPJBU
291900041202
292000041202     c                   call      'TRUL06R'
292100041202     c                   parm                    kpjba
292200041202     c                   movel     kpjbu         trul06ds
292300041202    3c                   do        30            x1
292400041202    4c                   if        Lin(x1) = 0
292500041202     c                   leave
292600041202    4c                   endif
292700041203     c     lin(x1)       lookup    skpoel(1)                              30
292800041202      *
292900041202    6c                   if        not %found
293000041202     c                   z-add     1             xx
293100041202     c     000           lookup    SKpoel(xx)                             30
293200041202     c                   movea     lin(x1)       skpoel(xx)
293300041202    6c                   endif
293400041202      *
293500041202     c                   enddo
293600041202      *
293700041202     c                   endif
293800041202      *
293900041201      *
294000041202      * lettura del file sisdr01l
294100041201      *
294200050404     c     annoda        setll     sisdr000
294300041201     c                   do        *hival
294400041202
294500050404     c                   read      sisdr000
294600041201      *
294700041202     c                   if        %eof(sisdr01l)
294800041201     c                   leave
294900041201     c                   endif
295000041201      *
295100041201      * verifico il periodo
295200041202
295300041202     c                   z-add     sdrann        wsdrann
295400041202     c                   z-add     sdrmes        wsdrmes
295500041202
295600041201     c                   if        wsdrper > periodoa
295700041202     c                   iter
295800041201     c                   endif
295900041201      *
296000041201     c                   if        wsdrper < periododa
296100041201     c                   iter
296200041201     c                   endif
296300041201      * verifico se spedizioni normali
296400041201     c                   if        sdrrbl <> 'N'
296500041201     c                   iter
296600041201     c                   endif
296700041201      *
296800041201      * eseguo caricamento schiera e calcoli
296900041201
297000041201      * verifico punto operativo
297100041201      *
297200041201     c                   z-add     1             xx
297300041202     c     sdrflp        lookup    skpoel(XX)                             30
297400041203     c                   If        %found
297500041201      * totalizzo le spedizioni
297600041201     c                   add       sdrnsp        sksped(xx)
297700041203     c                   endif
297800060511      * verifico se punto operativo  internazionale
297900060511      *
298000060511     c                   z-add     1             xx
298100060511     c     sdrflp        lookup    skpoel_i(XX)                           30
298200060511     c                   If        %found
298300060511      * totalizzo le spedizioni internazionali
298400060511     c                   add       sdrnsp        sksped_i(xx)
298500060511     c                   endif
298600060511
298700041202     c                   z-add     1             xx
298800041202     c     sdrfla        lookup    skpoel(XX)                             30
298900041203     c                   If        %found
299000041202      * totalizzo le spedizioni
299100041202     c                   add       sdrnsp        sksped(xx)
299200041203     c                   endif
299300060511      * verifico se punto operativo  internazionale
299400060511      *
299500060511     c                   z-add     1             xx
299600060511     c     sdrfla        lookup    skpoel_i(XX)                           30
299700060511     c                   If        %found
299800060511      * totalizzo le spedizioni internazionali
299900060511     c                   add       sdrnsp        sksped_i(xx)
300000060511     c                   endif
300100060511
300200041201      *
300300041201     c                   enddo
300400041202      *
300500041202
300600140130      *
300700121116      /free
300800121116        wcmd = 'CLRPFM '+ %trim(wlib) + '/WFRAC00F';
300900121116        ExecuteCommand (wcmd);
301000121116      /end-free
301100041201
301200041023     c                   open      wfrac01l
301300121116
301400041023      *
301500041023     c                   feod      wfrac11l
301600041023     c     *loval        setll     wfrac11l
301700041023      *
301800041202    1c                   do        *hival
301900041023     c                   read      wfrac11l
302000041202    2c                   if        %eof(wfrac11l)
302100041023     c                   leave
302200041202    2c                   endif
302300041202      * se non è internazionale
302400041202    2c                   if        ractpo = ' '
302500041202     c                   z-add     1             xx
302600041202     c     racpoe        lookup    skpoel(xx)                             30
302700041202      *
302800041202    3c                   if        %found
302900041202     c                   z-add     SKsped(xx)    racspe
303000050401     c                   z-sub     SKpoel(xx)    SKpoel(xx)
303100121116
303200121116      /free
303300121116       //?Imposto il totale anche su tutti i rcd della stessa filiale su WFRAD
303400121116         IF  ID0crptch = 'S';
303500121116           exsr aggRAD;
303600121116         ENDIF;
303700121116      /end-free
303800121116
303900041202    3c                   endif
304000041202      *
304100041202   x2c                   else
304200041202      * internazionale
304300060511      *                cerco i po internazionali  se non sono la sede
304400041202      *
304500060511   2ac                   If        racpoe <> 046
304600041202     c                   clear                   trul06ds
304700041202     c                   move      '£1'          d06cod
304800041202     c                   movel     racpoe        d06key
304900041202     c                   movel(p)  trul06ds      KPJBU
305000041202
305100041202     c                   call      'TRUL06R'
305200041202     c                   parm                    kpjba
305300041202     c                   movel     kpjbu         trul06ds
305400041202     c                   movea     lin           Skpoin
305500041202     c                   z-add     0             x1
305600041202    3c                   do        30            x1
305700041202    4c                   if        skpoin(x1) = 0
305800041202     c                   leave
305900041202    4c                   endif
306000050404     c     skpoin(x1)    chain     azorg01
306100041202    4c                   if        %found(azorg01l)
306200041202      * controllo il network
306300041202     c                   movel     orgde3        og143
306400041202    5c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
306500050411     c                                            or §ogntw = 'FED'
306600041202     c                   z-add     1             xx
306700041202     c     orgfil        lookup    skpoel(xx)                             30
306800041202      *
306900041202    6c                   if        %found
307000041202     c                   add       SKsped(xx)    racspe
307100050401     c                   z-sub     SKpoel(xx)    SKpoel(xx)
307200041202    6c                   endif
307300121116
307400121116      /free
307500121116       //?Imposto il totale anche su tutti i rcd della stessa filiale su WFRAD
307600121116         IF  ID0crptch = 'S';
307700121116           exsr aggRAD;
307800121116         ENDIF;
307900121116      /end-free
308000121116
308100041202      *
308200041202    5c                   endif
308300041202      *
308400041202    4c                   endif
308500041202      *
308600041202    3c                   enddo
308700060511      * se sono in sede interanzionale
308800060511   2ac                   else
308900060511     c                   xfoot     sksped_i      racspe
309000121116
309100121116      /free
309200121116       //?Imposto il totale anche su tutti i rcd della stessa filiale su WFRAD
309300121116       //?internazionale
309400121116         IF  ID0crptch = 'S';
309500121116           exsr aggRAD;
309600121116         ENDIF;
309700121116      /end-free
309800121116
309900060511   2ac                   endif
310000041202      *
310100041202    2c                   endif
310200041202      *
310300041025     c                   write     wfrac000
310400041202    1c                   enddo
310500050404      *
310600050404      *
310700050404      * Proseguo SOLO SE se richiesto l'ordinamento '2'
310800050404      *   (per p.o. responsabile)
310900050404if  1c                   if        ID0ord = '1'
311000050404     c                   leavesr
311100050404e   1c                   endif
311200050404      *
311300050401      *
311400050401      * Verifico se ho registrato tutti i P.O
311500050401      *   ovvero se ce ne sono senza reclami...
311600050401     c                   clear                   WFRAC000
311700050401     c                   eval      RACtpe = ID0ord
311800050401     c                   eval      RACdte = DateU
311900050401     c                   eval      RACddr = ID0AMGDCD
312000050401     c                   eval      RACadr = ID0AMGDCA
312100050401     c                   eval      RACddc = ID0AMGDHD
312200050401     c                   eval      RACadc = ID0AMGDHA
312300050401      *
312400050401     c                   eval      xx =  1
312500050401do  1c                   dow           xx         <= %elem(SKpoel)
312600050401     c                             and SKpoel(xx) <> *zeros
312700050401      *
312800050401      * seleziono i p.o. di cui NON ho ancora registrato record
312900050404      *   (anche se con totale spedizioni = *zeros)
313000050401if  2c                   if            SKpoel(xx) >  *zeros
313100050401      *
313200050401     c     SKpoel(xx)    chain     AZORG01
313300050401if  3c                   if        %found(AZORG01L)
313400050404     c                             and   ORGfva = *blanks
313500050404     c                   movel     ORGde3        Og143
313600050404     c                   movel     ORGde8        Og148
313700050404if  4c                   if            (§OGlpo = '1'
313800050404     c                              or  §OGlpo = '2')
313900050404     c                             and (§OGntw = 'COR'
314000050404     c                              or  §OGntw = 'MES')
314100050404     c                              or  §OGntw = 'EEX'
314200050404     c                              or  §OGntw = 'DPD'
314300050404     c                              or  §OGntw = 'FED'
314400050404     c                              or  §OGntw = 'PPT'
314500050401     c                   eval      RACdis = ORGfl3
314600050401     c                   eval      RACare = ORGcar
314700050401     c                   eval      RACpoe = SKpoel(xx)
314800050401     c                   eval      RACspe = SKsped(xx)
314900050401      *
315000050401     c                   WRITE     WFRAC000
315100121116
315200121116      /free
315300121116       //?Se richiesto scrivo anche il WFRAD
315400121116         IF  ID0crptch = 'S';
315500121116           exsr wrtRAD;
315600121116         ENDIF;
315700121116      /end-free
315800121116
315900050404e   4c                   endif
316000050404e   3c                   endif
316100050401      *
316200050401e   2c                   endif
316300050401     c                   eval      xx =  xx + 1
316400050401      *
316500050401e   1c                   enddo
316600041023      *
316700041023     c                   endsr
316800121116
316900121116      /free
317000121116       //--------------------------------------------------------------
317100121116       //?Aggiorno i dati su WFRAD.
317200121116       //--------------------------------------------------------------
317300121116       BEGSR aggRAD;
317400121116
317500121116       //?Aggiorno il totale spedizioni per tutti i rcd con la stessa filiale
317600121116         setll (RACdis:RACare:RACpoe:RACtpo) WFRAD01L;
317700121116         reade (RACdis:RACare:RACpoe:RACtpo) WFRAD01L;
317800121116         DOW not %eof(WFRAD01L);
317900121116           RADspe = RACspe;
318000121116           UPDATE WFRAD000;
318100121116           reade (RACdis:RACare:RACpoe:RACtpo) WFRAD01L;
318200121116         ENDDO;
318300121116
318400121116       ENDSR;
318500121116       //--------------------------------------------------------------
318600121116       //?Sscrivo i dati su WFRAD.
318700121116       //--------------------------------------------------------------
318800121116       BEGSR wrtRAD;
318900121116
319000121116       //?Scrivo gli stessi rcd aggiunti su WFRAC anche sul WFRAD
319100121116         clear WFRAD000;
319200121116         RADtpe = RACtpe;
319300121116         RADdte = RACdte;
319400121116         RADddr = RACddr;
319500121116         RADadr = RACadr;
319600121116         RADddc = RACddc;
319700121116         RADadc = RACadc;
319800121116         RADpoe = RACpoe;
319900121116         RADtpo = RACtpo;
320000121116         RADdis = RACdis;
320100121116         RADare = RACare;
320200121116         RADspe = RACspe;
320300121116         WRITE  WFRAD000;
320400121116
320500121116       ENDSR;
320600121116      /end-free
320700121116
320800041023      *---------------------------------------------------------------*
320900041023      * Stampa della testata                                          *
321000041023      *---------------------------------------------------------------*
321100041023     c     SR_Testa      BegSr
321200041026      *
321300041026     c                   seton                                        15
321400041023      * se sono in sede e ci sono distretto valorizzato lo devo decodificare
321500041023     c                   if        *in01   and id0dst <> *blanks
321600041023     c                   eval      p3cdst = id0dst
321700041023      *
321800041023     c                   movel     '17'          cod
321900041023     c                   movel(p)  p3cdst        key
322000050404     c     ktab          chain     tabel
322100041023      *
322200041023     c                   movel(p)  tbluni        p3dds2
322300041023     c                   endif
322400041023      *
322500041025     c                   if        *in01   and id0are > 0
322600041023     c                   eval      p3care = id0are
322700041023      *
322800041023     c                   movel     '05'          cod
322900041023     c                   movel(p)  p3care        key
323000050404     c     ktab          chain     tabel
323100041023      *
323200041023     c                   movel(p)  tbluni        p3dar2
323300041023     c                   endif
323400041023     c* Ordinamento
323500041023     c                   if        id0ord='1'
323600041023     c                   eval      p1dord='Per Numero Richiesta '
323700041023     c                   endif
323800041023     c                   if        id0ord='2'
323900041023     c                   eval      p1dord='Per P.O. RESPONSABILE'
324000041023     c                   endif
324100041023      * p.o. responsabile
324200041023     c                   If        id0prs > 0
324300050404     c     id0prs        chain     azorg01
324400041230     c                   movel     orgdes        p1dprs
324500041023     c                   endif
324600041023      *
324700041023      * p.o. apertura
324800041023     c                   If        id0fil > 0
324900050404     c     id0fil        chain     azorg01
325000041230     c                   movel     orgdes        p1dfil
325100041023     c                   endif
325200041023      * se sono in sede e P.O. non valorizzati e distretto e area non valorizzato metto
325300041023      * tutti
325400041025     c                   if        *in01 and id0dst = ' ' and id0are = 0 and
325500041023     c                             id0prs = 0 and id0fil = 0
325600041023     c                   eval      p3dds2 = 'TUTTI'
325700041023     c                   eval      p3dar2 = 'TUTTE'
325800041023     c                   endif
325900041023      *
326000041025     c                   if        *in01 and id0dst<>' ' and id0are=0 and
326100041023     c                             id0prs = 0 and id0fil = 0
326200041023     c                   eval      p3dar2 = 'TUTTE'
326300041023     c                   endif
326400041023      *
326500041023     c                   write     dnd1t1
326600041023     c                   write     dnd1ts
326700041023     c                   write     dnd102
326800041026
326900041026     c                   setoff                                       15
327000041023
327100041023     c                   endsr
327200041023      *
327300041022      *---------------------------------------------------------------*
327400041022      * Reperimento Dati del job (Utente/Operativi)                   *
327500041022      *---------------------------------------------------------------*
327600041022     c     DatiJob       BegSr
327700041022      *
327800041022     c     *dtaara       define    §azute        azuteds
327900041022     c     *dtaara       define    §datiute      ddatiute
328000041022      *
328100041022     c                   in(E)     *dtaara
328200041022     c                   IF        %ERROR or RSUT = *blanks
328300041022     c                   clear                   Tibs34Ds
328400041022     c                   call      'TIBS34R'
328500041022     c                   parm                    Tibs34Ds
328600041022     c                   in        *dtaara
328700041022     c                   ENDIF
328800041023      * - file TABEL00F
328900041022     c     ktab          klist
329000041022     c                   kfld                    codut
329100041022     c                   kfld                    cod
329200041022     c                   kfld                    key
329300041026      * - file FITGN01L
329400041026     c     ktgN          klist
329500041026     c                   kfld                    tgdant
329600041026     c                   kfld                    tgdnut
329700041026     c                   kfld                    tgdpdt
329800041023      * - file WFRAC11L
329900041023     c     Krac          klist
330000041023     c                   kfld                    p3cdst
330100041023     c                   kfld                    p3care
330200041023     c                   kfld                    p3cfil
330300041023     c                   kfld                    W_tpo
330400041023      * - file AZCLN01L
330500041023     c     kazcln        klist
330600041023     c                   kfld                    ktfp
330700041023     c                   kfld                    ktfa
330800041023     c                   kfld                    kann
330900041023     c                   kfld                    kmes
331000041022      *
331100041022     c                   EndSr
331200041020
331300041022      *---------------------------------------------------------------*
331400041022      * Routine Iniziale                                              *
331500041022      *---------------------------------------------------------------*
331600041022     c     *InzSR        BegSR
331700041022      *
331800041022     c     *Entry        plist
331900041022     c                   parm                    KPJBA
332000041022     c                   movel     kpjbu         fidnD0ds
332100041022      * Reperisco data corrente
332200041022     c                   time                    w0140
332300041023     c                   move      w0140         wDate
332400041022     c                   clear                   WLBdat
332500041023     c                   eval      G02dat = wDate
332600041022     c                   call      'XSRDA8'
332700041022     c                   parm                    WLBdat
332800041022     c                   z-add     G02inv        DateU
332900041022      *
333000041022      * Reperisco dati job
333100041022     c                   exsr      DatiJob
333200041022
333300041022      * Se sono in sede accendo 01
333400041022     c                   eval      *in01=(dutlpo ='S')
333500041022
333600041022      * controllo il sistema informativo dove sono per reperire
333700041022      * la libreria
333800041027      *   ---> GAITRA201/GAITRAAZM
333900041026     c                   If        %subst(knsif:7:1) <> 'P'
334000041027     c                   Eval      wlib = 'GAITRAAZM '
334100041022     c                   EndIf
334200041022      *   ---> GAITRAP/FILTRAP/UNITRAGRU
334300041022     c                   If        %subst(knsif:7:1) = 'P'
334400120914     c                   Eval      wlib = 'GAITRAAZP'
334500041022     c                   EndIf
334600041022
334700041022      * duplico il file di comodo in qtemp
334800041022     c                   clear                   comman
334900041022     c                   movel(p)  cmd(1)        comman
335000041022     c                   Eval      %Subst(comman:32:10) = wlib
335100041022     c                   call      'QCMDEXC'
335200041022     c                   parm                    comman
335300041022     c                   parm                    lenght
335400041022      * apro il file di comodo per la stampa
335500041022     c                   open      wfrac11l
335600041023
335700041023      * duplico il file di comodo in qtemp se ordinamento 1 e richiesta stampa dettaglio
335800041027     c                   If        id0ord = '1' and not *in01
335900041023     c                   clear                   comman
336000041023     c                   movel(p)  cmd(2)        comman
336100041023     c                   Eval      %Subst(comman:32:10) = wlib
336200041023     c                   call      'QCMDEXC'
336300041023     c                   parm                    comman
336400041023     c                   parm                    lenght
336500041023      * apro il file di comodo per la stampa
336600041023     c                   open      wfrut00f
336700041023     c                   endif
336800121116
336900121116      /free
337000121116       //?Se richiesto il file x chiamante
337100121116         IF  ID0crptch = 'S';
337200121116         //?Duplico in Qtemp il file WFRAD
337300121116           wcmd = cmd(3);
337400121116           %subst(wcmd:32:10) = wlib;
337500121116           ExecuteCommand (wcmd);
337600121116         //?Apro il file
337700121116           open  WFRAD01L;
337800121116         ENDIF;
337900140205
338000140205         // -?Caricamento dati tab. "MAR" = Motivi Apertura Reclami?
338100140206         clear  yy;
338200140205         clear  sk_MAR;
338300140205         clear  sk_MARd;
338400140210         clear  sk_MARs;
338500140205         clear  KeyTNTBE01;
338600140205         k_TBEcod = 'MAR';
338700140205         setll  %kds( keyTNTBE01 )  TNTBE000;
338800140205         reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
338900140205         DoW  Not  %eof(TNTBE01L);
339000140205           if  TBEatb = *blank;
339100140205             dMAR = TBEuni;
339200140205             yy += 1;
339300140205             sk_MAR(yy)  = TBEke1;
339400140205             sk_MARd(yy) = §MARdesc;
339500140210             sk_MARs(yy) = §MAReSt;
339600140205           endif;
339700140205           reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
339800140205         EndDo;
339900140205
340000121116      /end-free
340100041022
340200041022     C                   ENDSR
340300041022**   cmd - comando
340400041022CRTDUPOBJ OBJ(WFRAC1*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
340500041023CRTDUPOBJ OBJ(WFRUT0*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
340600121116CRTDUPOBJ OBJ(WFRAD0*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
340700121116CLRPFM QTEMP/WFRUT00F
340800121116CPYF FROMFILE(QTEMP/WFRAD00F) TOFILE(
