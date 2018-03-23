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
056700120914     c                             raca16 + raca50
056800041026     c                   z-add     W_pttota      pttota
056900041026      * totali aperti da oltre xx giorni
057000041026     c                   eval      W_pttotg= racg01 + racg02 + racg03 + racg05 +
057100041026     c                             racg06 + racg07 + racg10 + racg11 + racg12 +
057200041026     c                             racg13 + racg20 + racg21 + racg22 + racg30 +
057300070710     c                             racg99 + racgx1 + racgx2 + racg14 + racg15 +
057400120914     c                             racg16 + racg50
057500041026     c                   z-add     W_pttotg      pttotg
057600041026      * totali chiusi fondati
057700041026     c                   eval      W_pttotf= racf01 + racf02 + racf03 + racf05 +
057800041026     c                             racf06 + racf07 + racf10 + racf11 + racf12 +
057900041026     c                             racf13 + racf20 + racf21 + racf22 + racf30 +
058000070710     c                             racf99 + racfx1 + racfx2 + racf14 + racf15 +
058100120914     c                             racf16 + racf50
058200041026     c                   z-add     W_pttotf      pttotf
058300041026      * totali chiusi fondati da altri
058400041026     c                   eval      W_pttoth= rach01 + rach02 + rach03 + rach05 +
058500041026     c                             rach06 + rach07 + rach10 + rach11 + rach12 +
058600041026     c                             rach13 + rach20 + rach21 + rach22 + rach30 +
058700070710     c                             rach99 + rachx1 + rachx2 + rach14 + rach15 +
058800120914     c                             rach16 + rach50
058900041026     c                   z-add     W_pttoth      pttoth
059000041026      * totali chiusi infondati
059100041026     c                   eval      W_pttotn= racn01 + racn02 + racn03 + racn05 +
059200041026     c                             racn06 + racn07 + racn10 + racn11 + racn12 +
059300041026     c                             racn13 + racn20 + racn21 + racn22 + racn30 +
059400070710     c                             racn99 + racnx1 + racnx2 + racn14 + racn15 +
059500120914     c                             racn16 + racn50
059600041026     c                   z-add     W_pttotn      pttotn
059700041026      * totali chiusi informativi
059800041026     c                   eval      W_pttoti= raci01 + raci02 + raci03 + raci05 +
059900041026     c                             raci06 + raci07 + raci10 + raci11 + raci12 +
060000041026     c                             raci13 + raci20 + raci21 + raci22 + raci30 +
060100070710     c                             raci99 + racix1 + racix2 + raci14 + raci15 +
060200120914     c                             raci16 + raci50
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
061400120914     c                             racm16 + racm50
061500041130     c                   z-add     W_pttomc      pttomc
061600041026      * totali per chi chiama  Destinatario
061700041026     c                   eval      W_pttotd= racd01 + racd02 + racd03 + racd05 +
061800041026     c                             racd06 + racd07 + racd10 + racd11 + racd12 +
061900041026     c                             racd13 + racd20 + racd21 + racd22 + racd30 +
062000070710     c                             racd99 + racdx1 + racdx2 + racd14 + racd15 +
062100120914     c                             racd16 + racd50
062200041026     c                   z-add     W_pttotd      pttotd
062300041026      * totali per chi chiama  Cliente
062400041026     c                   eval      W_pttotc= racc01 + racc02 + racc03 + racc05 +
062500041026     c                             racc06 + racc07 + racc10 + racc11 + racc12 +
062600041026     c                             racc13 + racc20 + racc21 + racc22 + racc30 +
062700070710     c                             racc99 + raccx1 + raccx2 + racc14 + racc15 +
062800120914     c                             racc16 + racc50
062900041130     c                   add       W_pttotc      pttomc
063000041026      * totali per chi chiama  Altro
063100041026     c                   eval      W_pttotx= racx01 + racx02 + racx03 + racx05 +
063200041026     c                             racx06 + racx07 + racx10 + racx11 + racx12 +
063300041026     c                             racx13 + racx20 + racx21 + racx22 + racx30 +
063400070710     c                             racx99 + racxx1 + racxx2 + racx14 + racx15 +
063500120914     c                             racx16 + racx50
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
106300041026
106400041026      * percentuale
106500041026
106600041026     c                   If        pctotch > 0
106700041104     c                   eval(h)   pcperm = (pctotch * 100) / W_pctotch
106800041026     c                   seton                                        11
106900041026     c                   else
107000041026     c                   clear                   pcperm
107100041026     c                   setoff                                       11
107200041026     c                   endif
107300041026      *
107400041026     c                   write     dnd1dt2
107500041026      *
107600041026     c                   endif
107700041026      *
107800041026     c                   enddo
107900041104      * stampo la leggenda
108000041130     c                   write     dnd1le3
108100041130
108200041104     c                   If        Id0ord = '1'
108300041104     c                   write     Dnd1le1
108400041104     c                   endif
108500041104     c                   If        Id0ord = '2'
108600041104     c                   write     Dnd1le2
108700041104     c                   endif
108800041026
108900041026      * se tipo ordinamento 1 e sono in filiale sxtampo anche il dettaglio utente
109000041026     c                   IF        id0ord = '1' and not *in01
109100041026
109200041104     c                   setoff                                       90
109300041026      * testate
109400041103     c                   write     Dnd1t1
109500041103     c                   write     Dnd1tS
109600041103     c                   write     Dnd1t3
109700041103     c   01              write     Dnd1t4
109800041103     c                   write     Dnd1t5
109900041103     c                   write     Dnd1t3
110000041103
110100041026     c                   write     Dnd1ut
110200041026
110300041026     c                   feod      wfrut00f
110400041027     c                   clear                   rot_ute
110500041026
110600041026     c     *loval        setll     wfrut00f
110700041026
110800041026     c                   do        *hival
110900041026     c                   read      wfrut00f
111000041026     c                   if        %eof(wfrut00f)
111100041027     c                   exsr      SR_totute
111200041026     c                   leave
111300041026     c                   endif
111400041027      * mi salvo il prefisso utente  se uguale a *blank
111500041027     c                   if        rot_ute = *blanks
111600041027     c                   eval      rot_ute = %subst(rutute:1:3)
111700041027     c                   endif
111800041027      *
111900041027     c                   If        rot_ute <> %subst(rutute:1:3)
112000041027     c                   exsr      SR_totute
112100041027     c                   endif
112200041027      *
112300041026     c                   movel(p)  rutute        pudute
112400041026      * calcolo le totale chiuse
112500041026     c                   z-add     rutfon        pudtch
112600041026     c                   add       rutifo        pudtch
112700041026     c                   add       rutinf        pudtch
112800141204      /free
112900141204       //?Calcolo il totale del chi chiama Mittente/Cliente
113000141204         PUTcmc = RUTccm + RUTccc;
113100141204      /end-free
113200141204     c
113300041027      * calcolo i totali per ufficio
113400041027     c                   add       rutape        T_rutape
113500041027     c                   add       rutdxg        T_rutdxg
113600041027     c                   add       rutfon        T_rutfon
113700041027     c                   add       rutcfa        T_rutcfa
113800041027     c                   add       rutifo        T_rutifo
113900041027     c                   add       rutinf        T_rutinf
114000041027     c                   add       ruttcf        T_ruttcf
114100041027     c                   add       ruttcm        T_ruttcm
114200041027     c                   add       ruttcl        T_ruttcl
114300041027     c                   add       ruttct        T_ruttct
114400141203     c                   add       rutccd        T_rutccd
114500141203     c                   add       rutccm        T_rutccm
114600141203     c                   add       rutccc        T_rutccc
114700141203     c                   add       rutcca        T_rutcca
114800041027      *
114900041027      *
115000041026     c                   If        *in90
115100041026     c                   setoff                                       90
115200041026      * testate
115300041026     c                   write     Dnd1t1
115400041026     c                   write     Dnd1tS
115500041026     c                   write     Dnd1t3
115600041026     c   01              write     Dnd1t4
115700041026     c                   write     Dnd1t5
115800041026     c                   write     Dnd1t3
115900041026
116000041026     c                   write     Dnd1ut
116100041026
116200041026     c                   endif
116300041026
116400041026     c                   write     Dnd1du1
116500041027      *
116600041027      * se ho stampato i totali rileggo il record
116700041026
116800041026     c                   enddo
116900041104
117000041104      * stampo la leggenda
117100041104     c                   If        Id0ord = '1'
117200041104     c                   write     Dnd1le1
117300041104     c                   endif
117400041104     c                   If        Id0ord = '2'
117500041104     c                   write     Dnd1le2
117600041104     c                   endif
117700041104
117800041026     c                   endif
117900041026
118000041026     c                   endsr
118100041026
118200041026      *
118300041027      *---------------------------------------------------------------*
118400041027      * Totali per ufficio                                            *
118500041027      *---------------------------------------------------------------*
118600041027     c     SR_totute     BegSr
118700041027
118800041027
118900041027     c                   if        rot_ute <> *blanks
119000041027
119100041027     c                   clear                   pudute
119200041027     c                   eval      pudute = 'T O T A L E  ' +
119300041027     c                             rot_ute
119400041027      * calcolo le totale chiuse
119500041027     c                   z-add     T_rutfon      pudtch
119600041027     c                   add       T_rutifo      pudtch
119700041027     c                   add       T_rutinf      pudtch
119800141204      /free
119900141204       //?Calcolo il totale del chi chiama Mittente/Cliente
120000141204         PUTcmc = t_RUTccm + t_RUTccc;
120100141204      /end-free
120200041027      * calcolo i totali per ufficio
120300041027     c                   z-add     T_rutape      rutape
120400041027     c                   z-add     T_rutdxg      rutdxg
120500041027     c                   z-add     T_rutfon      rutfon
120600041027     c                   z-add     T_rutcfa      rutcfa
120700041027     c                   z-add     T_rutifo      rutifo
120800041027     c                   z-add     T_rutinf      rutinf
120900041027     c                   z-add     T_ruttcf      ruttcf
121000041027     c                   z-add     T_ruttcm      ruttcm
121100041027     c                   z-add     T_ruttcl      ruttcl
121200041027     c                   z-add     T_ruttct      ruttct
121300141203     c                   z-add     T_rutccd      rutccd
121400141203     c                   z-add     T_rutccm      rutccm
121500141203     c                   z-add     T_rutccc      rutccc
121600141203     c                   z-add     T_rutcca      rutcca
121700041027
121800041027     c                   If        *in90
121900041027     c                   setoff                                       90
122000041027      * testate
122100041027     c                   write     Dnd1t1
122200041027     c                   write     Dnd1tS
122300041027     c                   write     Dnd1t3
122400041027     c   01              write     Dnd1t4
122500041027     c                   write     Dnd1t5
122600041027     c                   write     Dnd1t3
122700041027
122800041027     c                   write     Dnd1ut
122900041027
123000041027     c                   endif
123100041027
123200041027     c                   write     Dnd1du1
123300041027      *
123400041027      * riaggancio il record se non è EOF
123500041027      *
123600041027     c                   if        not %eof
123700041027     c     rutute        chain     wfrut00f
123800041027     c                   eval      rot_ute = %subst(rutute:1:3)
123900041027     c                   endif
124000041027
124100041027     c                   clear                   T_rutape
124200041027     c                   clear                   T_rutdxg
124300041027     c                   clear                   T_rutfon
124400041027     c                   clear                   T_rutcfa
124500041027     c                   clear                   T_rutifo
124600041027     c                   clear                   T_rutinf
124700041027     c                   clear                   T_ruttcf
124800041027     c                   clear                   T_ruttcm
124900041027     c                   clear                   T_ruttcl
125000041027     c                   clear                   T_ruttct
125100141203     c                   clear                   T_rutccd
125200141203     c                   clear                   T_rutccm
125300141203     c                   clear                   T_rutccc
125400141203     c                   clear                   T_rutcca
125500041027      *
125600041027     c                   endif
125700041027      *
125800041027     c                   endsr
125900041027      *
126000041022      *---------------------------------------------------------------*
126100041022      * Controllo i dati con le richieste del video                   *
126200041022      *---------------------------------------------------------------*
126300041022     c     SR_ctrrec     BegSr
126400041022
126500041022     c                   clear                   Wrecok
126600041022      *
126700041022     c                   do        *hival
126800041026      * se tipo tipo elaborazione uguale a ' ' e ordinamento 1 verifico se ci sono
126900041026      * reclami aprti da INT
127000060510      * se tipo tipo elaborazione uguale a ' ' e ordinamento 1 verifico se ci sono
127100060510      * reclami aperti da utente RAI*
127200041026      *
127300041026     c                   If        W_tpo = ' '  and id0ord = '1'
127400060510     c                             and (%subst(tgdpru:1:3) = 'INT' or
127500060510     c                                  %subst(tgdpru:1:3) = 'RAI')
127600041026     c                             and cisono_int = ' '
127700041026     c                   eval      cisono_int = 'Y'
127800041026     c                   endif
127900041025      * se tipo elaborazione internazionale verifico l'utente  e viceversa
128000041025      *
128100050421
128200050421      * se richiesto tipo ordinamneto "1" oppure rischiesto solo estero
128300050421
128400050421     c                   If        id0ord = '1'  or Id0sre ='S'
128500050421
128600060510     c                   If        W_tpo = 'I' and (%subst(tgdpru:1:3) <> 'INT'
128700060511     c                                         and  %subst(tgdpru:1:3) <> 'RAI')
128800041025     c                   leave
128900041025     c                   endif
129000060510
129100060510     c                   If        W_tpo = ' ' and (%subst(tgdpru:1:3) = 'INT'
129200060510     c                                         or   %subst(tgdpru:1:3) = 'RAI')
129300041025     c                   leave
129400041025     c                   endif
129500050421
129600050421     c                   endif
129700041022      *
129800041022      *- P.O. responsabile
129900041022      * diverso da quello a video
130000041022     c                   if        ID0prs > *zeros and tgdpor <> ID0prs
130100041022     c                   leave
130200041022     c                   endif
130300041022      *
130400041022      *- P.O. apertura
130500041022      * diverso da quello a video
130600041022     c                   if        ID0fil > *zeros and tgdpot <> ID0fil
130700041022     c                   leave
130800041022     c                   endif
130900041022      *
131000041022      * Solo le aperte da altro P.O. responsabile
131100041022      *
131200041022     c                   If        TGDpor>0 and TGDpor=TGDpot and ID0AAP='S'
131300041022     c                   leave
131400041022     c                   endif
131500041022      *
131600041022      * Solo le aperte da utente Internazionale
131700041022      *
131800060510     c                   If        ID0sre = 'S' and (%subst(tgdpru:1:3)<>'INT'
131900060515     c                                          and  %subst(tgdpru:1:3)<>'RAI')
132000041022     c                   leave
132100041022     c                   endif
132200041022      *
132300041022      *- Date chiamata
132400041022      * data minore di quella a video
132500041025     c                   if        ID0AMGdcd > 0  and tgddar < ID0AMGdcd
132600041022     c                   leave
132700041022     c                   endif
132800041022      * data maggiore di  quella a video
132900041025     c                   if        ID0AMGdca > 0  and tgddar > ID0AMGdca
133000041022     c                   leave
133100041022     c                   endif
133200041022      *
133300041022      *- Date chiusura
133400041022      * data minore di quella a video
133500041023     c                   if        ID0AMGdhd > 0  and tgddcd < ID0AMGdhd
133600041022     c                   leave
133700041022     c                   endif
133800041022      * data maggiore di  quella a video
133900041023     c                   if        ID0AMGdha > 0  and tgddcd > ID0AMGdha
134000041022     c                   leave
134100041022     c                   endif
134200041022      *- Tipo comunicazione
134300041022      *  diverso da quelli del video
134400140423     c                   clear                   w_lib
134500041025     c                   movea     tco           w_lib
134600041025     c                   If        w_lib <> *blank
134700041025     c     tgdtco        lookup    tco                                    30
134800041022     c                   if        not %found
134900041022     c                   leave
135000041022     c                   endif
135100041022     c                   endif
135200041022      *- Motivo apertura
135300041022      *  diverso da quelli del video
135400140423     c                   clear                   w_lib
135500041025     c                   movea     mar           w_lib
135600041025     c                   If        w_lib <> *blank
135700041025     c     tgdmad        lookup    mar                                    30
135800041022     c                   if        not %found
135900041022     c                   leave
136000041022     c                   endif
136100041022     c                   endif
136200140210      * ?solo motivi NON esclusi dalle statistiche?
136300140205     c                   eval      yy = %lookup( TGDmad : sk_MAR )
136400140210     c                   if        yy > *zero  and  sk_MARs(yy) = 'S'
136500140205     c                   leave
136600140205     c                   endif
136700041022      *- Codici chiusura
136800041022      *  diverso da quelli del video
136900140423     c                   clear                   w_lib
137000041025     c                   movea     ccr           w_lib
137100041025     c                   If        w_lib <> *blank
137200041025     c     tgdcdt        lookup    ccr                                    30
137300041022     c                   if        not %found
137400041022     c                   leave
137500041022     c                   endif
137600041022     c                   endif
137700041022      *- Solo le aperte
137800041022      *  e codice chiusura valorizzato
137900041022     c                   If        ID0sra = 'S' and tgdcdt <> *blank
138000041022     c                   leave
138100041022     c                   endif
138200041022      *
138300041022      * ricerca per utente
138400041022      *
138500041022     c                   If        tgdcdt <> *blanks
138600041022     c                   eval      WRK_ugr = tgdutc
138700041022     c                   else
138800041022     c                   eval      WRK_ugr = tgdrpg
138900041022     c                   endif
139000141204
139100141204      /free
139200141204       //?Selezione per utente
139300141205         wselugr = *off;
139400141205         IF  ID0UGRmin1 <> *blanks or ID0UGRmin2 <> *blanks or
139500141205             ID0UGRmin3 <> *blanks or ID0UGRmin4 <> *blanks or
139600141205             ID0UGRmin5 <> *blanks;
139700141205           z1 = 1;
139800141205           FOR  z1 by 1 to %elem(UGRmin);
139900141205             IF  UGRmin(z1) <> *blanks;
140000141205           //?verifico se i primi 3 byte dell'utente NON coincidono con i primi 3 del MIN
140100141205           //?provo con un altro
140200141205               IF  %subst(wrk_ugr:1:3) <> %subst(UGRmin(z1):1:3);
140300141205                 iter;
140400141205               ENDIF;
140500141205           //?richiesta selezione utente generico e non coincidono provo con un altro
140600141205               IF  UGRmax(z1) <> *blanks and
140700141205                   (wrk_ugr < UGRmin(z1) or wrk_ugr > UGRmax(z1));
140800141205                 iter;
140900141205               ENDIF;
141000141205           //?richiesta selezione utente NON generico e non coincidono provo con un altro
141100141205               IF  UGRmax(z1) = *blanks and wrk_ugr <> UGRmin(z1);
141200141205                 iter;
141300141205               ENDIF;
141400141205           //?se arrivo qua vuol dire che l'utente letto è tra quelli selezionati
141500141205           //?quindi esco dalla lettura delle selezioni
141600141205               wselugr = *on;
141700141205               leave;
141800141205             ENDIF;
141900141205           ENDFOR;
142000141205         //?se arrivo qua con il flag spento vuol dire che l'utente della RA
142100141205         //?non soddisfa nessuna selezione quindi NON devo elaborare il rcd letto
142200141205           IF  not wselugr;
142300141205             leave;
142400141205           ENDIF;
142500141205         ENDIF;
142600141204      /end-free
142700041022      *
142800041022      * se tutto OK record OK
142900041022     c                   eval      Wrecok = 'Y'
143000041022     c                   leave
143100041022      *
143200041022     c                   enddo
143300041022
143400041022     c                   endsr
143500041022      *---------------------------------------------------------------*
143600041022      * Preparazione campi e stampa del dettaglio                     *
143700041022      *---------------------------------------------------------------*
143800041022     c     SR_Prepa      BegSr
143900041025      *
144000041023      *
144100041023      * data richiesta
144200041023      *
144300041023     c                   clear                   WLBdat
144400041025     c                   eval      G02inv = tgddar
144500041023     c                   eval      G02err = '3'
144600041023     c                   call      'XSRDA8'
144700041023     c                   parm                    WLBdat
144800041023     c                   z-add     G02dat        p3cdac                         data ricez.comunicaz
144900041023      *
145000041023      * Oggetto
145100041023      *
145200041023     c                   clear                   p3cogg
145300050331      *
145400050331     c                   select
145500041023      * ---------- SPEDIZIONE -----------
145600050331     c                   when      tgdtor = 'S'
145700041023     c                   movel     tgdogg        WRKspe
145800041023     c                   eval      p3cogg = 'Sped. ' + %trim(WRKlnp) +  '/' +
145900041023     c                             %trim(WRKnrs) + '/' + %trim(WRKnsp) +
146000041023     c                             ' ' + WRKann
146100041023      *
146200041023      * ----------    ORM     -----------
146300050331     c                   when      tgdtor = 'O'
146400041023     c                   movel     tgdogg        WRKorm
146500041023     c                   eval      p3cogg = 'ORM ' + %trim(WRKpoe) +  '/' +
146600041023     c                             %trim(WRKnso) + '/' + %trim(WRKnor) +
146700041023     c                             '/' + %trim(WRKnrv)
146800041023      *
146900041023      * ----------  GENERICO  -----------
147000050331     c                   when      tgdtor = 'G'
147100050331     c                   eval      p3cogg = TGDogg
147200050331      *
147300050331     c                   endsl
147400041023      *
147500041023      * note
147600041023      *
147700041023     c                   clear                   p3cno1
147800041023     c                   clear                   p3cno2
147900041023      * recupero le note
148000041023      *
148100050404     c     ktgn          setll     Fitgn000
148200041026     c*
148300050404     c     ktgn          reade     Fitgn000
148400041026     c                   if        not %eof(Fitgn01l)
148500041026     c                   movel     tgnnot        p3cno1
148600041026     c                   endif
148700050404     c     ktgn          reade     Fitgn000
148800041026     c                   if        not %eof(Fitgn01l)
148900041026     c                   movel     tgnnot        p3cno2
149000041026     c                   endif
149100041023      * aggancio record dei totali
149200041023      *
149300041025     c                   If        W_poelwf = ' '
149400041023     C     Krac          chain     Wfrac11l
149500041023     c                   if        not %found(wfrac11l)
149600041023     c                   clear                   wfrac100
149700041023      * valorizzo i primi  campi del file
149800041023     c                   eval      Ractpe = ID0ord
149900041025     c                   eval      Racdte = dateu
150000041023     c                   eval      Racddr = ID0AMGDCD
150100041023     c                   eval      Racadr = ID0AMGDCA
150200041023     c                   eval      Racddc = ID0AMGDHD
150300041023     c                   eval      Racadc = ID0AMGDHA
150400041023     c                   eval      Racpoe = p3cfil
150500041023     c                   eval      Ractpo = W_tpo
150600041023     c                   eval      Racdis = P3cdst
150700041023     c                   eval      Racare = P3care
150800041025      * eval prima volta che imposto i dati per ogni P.O.
150900041025     c                   eval      W_poelwf = 'Y'
151000041025      * eval prima volta che stampo testats a cambio P.O.
151100041025     c                   eval      ST_testa = 'Y'
151200041023     c                   endif
151300041025     c                   endif
151400041023      * stampo testate se richiesta stampa del dettaglio se prima volta del P:O: o se OF
151500041026     c                   If        id0toi = 'N'
151600041025     c                   If        ST_testa = 'Y' or *in90
151700041023      * testa
151800041023     c                   write     Dnd1t1
151900041023     c                   write     Dnd1tS
152000041025     c                   write     Dnd1t3
152100041025     c   01              write     Dnd1t4
152200041025     c                   write     Dnd1t5
152300041025     c                   write     Dnd1t3
152400041025     c                   write     Dnd1t2
152500041025     c                   write     Dnd1t3
152600041023      *
152700041025     c   90              setoff                                       90
152800041025      *
152900041025     c                   clear                   ST_testa
153000041025      *
153100041023     c                   endif
153200041103      *
153300041103      * La priorità la stampo solo se diversa da  9
153400041103     c                   clear                   decprt
153500041103     c                   if        tgdprt <> '9'
153600041103     c                   eval      decprt = tgdprt
153700041103     c                   endif
153800041103      *
153900041103      * decodifico
154000140130      *?(già fatto in fase di selezione)?
154100140205     c                   if        yy = *zero  or  TGDmad <> sk_MAR(yy)
154200140205     c                   eval      yy = %lookup( TGDmad : sk_MAR )
154300140205     c                   endif
154400140205     c                   if        yy > *zero
154500140205     c                   eval      DecMAD = sk_MARd(yy)
154600140205     c                   else
154700140205     c                   eval      DecMAD = *all'? '
154800140205     c                   endif
154900140130      *
155000041103      * chi chiama  solo mittente destinatario cliente
155100041103
155200041103     c                   select
155300041103
155400041103     c                   when      tgdtch ='M'
155500041201     c                   eval      dectch = 'MIT/CLI'
155600041103
155700041103     c                   when      tgdtch ='D'
155800041103     c                   eval      dectch = 'DESTIN.'
155900041103
156000041103     c                   when      tgdtch ='C'
156100041201     c                   eval      dectch = 'MIT/CLI'
156200041103
156300041103     c                   other
156400041103     c                   eval      dectch = 'ALTRO  '
156500041103
156600041103     c                   endsl
156700041103
156800140130      *
156900041103     c                   clear                   tibs02ds
157000041103     C                   MOVEL     'C'           t02mod
157100041103     C                   MOVEL     knsif         t02sif
157200041103     C                   MOVEL     'COR'         t02cod
157300041103     C                   MOVEL     tgdtco        t02ke1
157400041103      *
157500041103     C                   CALL      'TIBS02R'
157600041103     C                   PARM                    KPJBA
157700041103     C                   PARM                    TIBS02DS
157800041103      *
157900041103     C                   movel     T02uni        DCOR
158000041103     C                   movel(p)  §CORdesc      dectco
158100140130      *
158200041103
158300041103     c                   if        tgdcdt <> *blanks
158400041103     c                   clear                   tibs02ds
158500041103     C                   MOVEL     'C'           t02mod
158600041103     C                   MOVEL     knsif         t02sif
158700041103     C                   MOVEL     'CHR'         t02cod
158800041103     C                   MOVEL     tgdcdt        t02ke1
158900041103      *
159000041103     C                   CALL      'TIBS02R'
159100041103     C                   PARM                    KPJBA
159200041103     C                   PARM                    TIBS02DS
159300041103      *
159400041103     C                   movel     T02uni        DCHR
159500041103     C                   movel(p)  §CHRdesc      deccdt
159600140130      *
159700041103     c                   else
159800041103     c                   clear                   deccdt
159900041103     c                   endif
160000041023      *
160100041023      * stampo la riga di dettaglio 1 + note se ci sono
160200041023      *
160300041025     c                   write     dnd1d03
160400041023      *
160500041025     c                   write     dnd1d04
160600041026
160700041026     c                   endif
160800041023      *
160900041023      * C A L C O L I
161000041023      *
161100041026
161200041026      * mi salvo la schiera dei motivi d'apertura per p.o.
161300041026
161400140130      *
161500041026     c     tgdmad        lookup    skmad                                  30
161600041026      *
161700041026     c                   if        not %found
161800041026     c                   add       1             mm
161900041026     c                   eval      skmad(mm) = tgdmad
162000041026     c                   endif
162100041023
162200050331     c                   SELECT
162300050331
162400041023      * Aperte suddivise per motivo
162500041023
162600050331     c                   WHEN      tgdcdt = *blanks
162700041023
162800050331     c                   select
162900050331     c                   when      TGDmad = '  1'
163000041023     c                   add       1             raca01
163100041023
163200050331     c                   when      TGDmad = '  2'
163300041023     c                   add       1             raca02
163400041023
163500050331     c                   when      TGDmad = '  3'
163600041023     c                   add       1             raca03
163700041023
163800050331     c                   when      TGDmad = '  5'
163900041023     c                   add       1             raca05
164000041023
164100050331     c                   when      TGDmad = '  6'
164200041023     c                   add       1             raca06
164300041023
164400050331     c                   when      TGDmad = '  7'
164500041023     c                   add       1             raca07
164600041023
164700050331     c                   when      TGDmad = ' 10'
164800041023     c                   add       1             raca10
164900041023
165000050331     c                   when      TGDmad = ' 11'
165100041023     c                   add       1             raca11
165200041023
165300050331     c                   when      TGDmad = ' 12'
165400041023     c                   add       1             raca12
165500041023
165600050331     c                   when      TGDmad = ' 13'
165700041023     c                   add       1             raca13
165800070710
165900070710     c                   when      TGDmad = ' 14'
166000070710     c                   add       1             raca14
166100070710
166200070710     c                   when      TGDmad = ' 15'
166300070710     c                   add       1             raca15
166400070710
166500070710     c                   when      TGDmad = ' 16'
166600070710     c                   add       1             raca16
166700041023
166800050331     c                   when      TGDmad = ' 20'
166900041023     c                   add       1             raca20
167000041023
167100050331     c                   when      TGDmad = ' 21'
167200041023     c                   add       1             raca21
167300041023
167400050331     c                   when      TGDmad = ' 22'
167500041023     c                   add       1             raca22
167600041023
167700050331     c                   when      TGDmad = ' 30'
167800041023     c                   add       1             raca30
167900120914
168000120914     c                   when      TGDmad = ' 50'
168100120914     c                   add       1             raca50
168200041023
168300050331     c                   when      TGDmad = ' 99'
168400041023     c                   add       1             raca99
168500130515
168600130515     c                   when      TGDmad = '  8'
168700130515     c                   add       1             racaX1
168800140409
168900140409     c                   when      TGDmad = ' 88'
169000140409     c                   add       1             racaX2
169100050331     c                   endsl
169200041023
169300041023      * aperte da x giorni suddivisi per motivo
169400041023
169500041023     c                   If        Id0xgg > 0 and id0ord = '1'  and
169600041023      *
169700041023      * controllo la data apertura con la data di oggi - 3 giorni lavorativi se minore
169800041023      * lo conteggio negli aperti da xx giorni suddivisi per motivo
169900041023      *
170000041025     c                             tgddar <= wdataoggi
170100041023      *
170200050331     c                   select
170300050331     c                   when      TGDmad = '  1'
170400041023     c                   add       1             racg01
170500041023
170600050331     c                   when      TGDmad = '  2'
170700041023     c                   add       1             racg02
170800041023
170900050331     c                   when      TGDmad = '  3'
171000041023     c                   add       1             racg03
171100041023
171200050331     c                   when      TGDmad = '  5'
171300041023     c                   add       1             racg05
171400041023
171500050331     c                   when      TGDmad = '  6'
171600041023     c                   add       1             racg06
171700041023
171800050331     c                   when      TGDmad = '  7'
171900041023     c                   add       1             racg07
172000041023
172100050331     c                   when      TGDmad = ' 10'
172200041023     c                   add       1             racg10
172300041023
172400050331     c                   when      TGDmad = ' 11'
172500041023     c                   add       1             racg11
172600041023
172700050331     c                   when      TGDmad = ' 12'
172800041023     c                   add       1             racg12
172900041023
173000050331     c                   when      TGDmad = ' 13'
173100041023     c                   add       1             racg13
173200041023
173300070710     c                   when      TGDmad = ' 14'
173400070710     c                   add       1             racg14
173500070710
173600070710     c                   when      TGDmad = ' 15'
173700070710     c                   add       1             racg15
173800070710
173900070710     c                   when      TGDmad = ' 16'
174000070710     c                   add       1             racg16
174100070710
174200050331     c                   when      TGDmad = ' 20'
174300041023     c                   add       1             racg20
174400041023
174500050331     c                   when      TGDmad = ' 21'
174600041023     c                   add       1             racg21
174700041023
174800050331     c                   when      TGDmad = ' 22'
174900041023     c                   add       1             racg22
175000041023
175100050331     c                   when      TGDmad = ' 30'
175200041023     c                   add       1             racg30
175300120914
175400120914     c                   when      TGDmad = ' 50'
175500120914     c                   add       1             racg50
175600041023
175700050331     c                   when      TGDmad = ' 99'
175800041023     c                   add       1             racg99
175900130515
176000130515     c                   when      TGDmad = '  8'
176100130515     c                   add       1             racgX1
176200140409
176300140409     c                   when      TGDmad = ' 88'
176400140409     c                   add       1             racgX2
176500050331     c                   endsl
176600050331
176700041023     c                   endif
176800041023
176900041023      * CHIUSI Fondati     suddivisi per motivo
177000041023
177100050331     c                   WHEN      tgdcdt = '  F'
177200041023      *
177300050331     c                   select
177400050331     c                   when      TGDmad = '  1'
177500041023     c                   add       1             racf01
177600041023
177700050331     c                   when      TGDmad = '  2'
177800041023     c                   add       1             racf02
177900041023
178000050331     c                   when      TGDmad = '  3'
178100041023     c                   add       1             racf03
178200041023
178300050331     c                   when      TGDmad = '  5'
178400041023     c                   add       1             racf05
178500041023
178600050331     c                   when      TGDmad = '  6'
178700041023     c                   add       1             racf06
178800041023
178900050331     c                   when      TGDmad = '  7'
179000041023     c                   add       1             racf07
179100041023
179200050331     c                   when      TGDmad = ' 10'
179300041023     c                   add       1             racf10
179400041023
179500050331     c                   when      TGDmad = ' 11'
179600041023     c                   add       1             racf11
179700041023
179800050331     c                   when      TGDmad = ' 12'
179900041023     c                   add       1             racf12
180000041023
180100050331     c                   when      TGDmad = ' 13'
180200041023     c                   add       1             racf13
180300070710
180400070710     c                   when      TGDmad = ' 14'
180500070710     c                   add       1             racf14
180600070710
180700070710     c                   when      TGDmad = ' 15'
180800070710     c                   add       1             racf15
180900070710
181000070710     c                   when      TGDmad = ' 16'
181100070710     c                   add       1             racf16
181200041023
181300050331     c                   when      TGDmad = ' 20'
181400041023     c                   add       1             racf20
181500041023
181600050331     c                   when      TGDmad = ' 21'
181700041023     c                   add       1             racf21
181800041023
181900050331     c                   when      TGDmad = ' 22'
182000041023     c                   add       1             racf22
182100041023
182200050331     c                   when      TGDmad = ' 30'
182300041023     c                   add       1             racf30
182400120914
182500120914     c                   when      TGDmad = ' 50'
182600120914     c                   add       1             racf50
182700041023
182800050331     c                   when      TGDmad = ' 99'
182900041023     c                   add       1             racf99
183000130515
183100130515     c                   when      TGDmad = '  8'
183200130515     c                   add       1             racfX1
183300140409
183400140409     c                   when      TGDmad = ' 88'
183500140409     c                   add       1             racfX2
183600050331     c                   endsl
183700041023
183800041023      * CHIUSI Fondati     suddivisi per motivo    P.O. apertura diverso da P.O responsabile
183900041023
184000041023     c                   If        tgdcdt = '  F'  and tgdpor <> tgdpot
184100041023      *
184200050331     c                   select
184300050331     c                   when      TGDmad = '  1'
184400041023     c                   add       1             rach01
184500041023
184600050331     c                   when      TGDmad = '  2'
184700041023     c                   add       1             rach02
184800041023
184900050331     c                   when      TGDmad = '  3'
185000041023     c                   add       1             rach03
185100041023
185200050331     c                   when      TGDmad = '  5'
185300041023     c                   add       1             rach05
185400041023
185500050331     c                   when      TGDmad = '  6'
185600041023     c                   add       1             rach06
185700041023
185800050331     c                   when      TGDmad = '  7'
185900041023     c                   add       1             rach07
186000041023
186100050331     c                   when      TGDmad = ' 10'
186200041023     c                   add       1             rach10
186300041023
186400050331     c                   when      TGDmad = ' 11'
186500041023     c                   add       1             rach11
186600041023
186700050331     c                   when      TGDmad = ' 12'
186800041023     c                   add       1             rach12
186900041023
187000050331     c                   when      TGDmad = ' 13'
187100041023     c                   add       1             rach13
187200041023
187300070710     c                   when      TGDmad = ' 14'
187400070710     c                   add       1             rach14
187500070710
187600070710     c                   when      TGDmad = ' 15'
187700070710     c                   add       1             rach15
187800070710
187900070710     c                   when      TGDmad = ' 16'
188000070710     c                   add       1             rach16
188100070710
188200050331     c                   when      TGDmad = ' 20'
188300041023     c                   add       1             rach20
188400041023
188500050331     c                   when      TGDmad = ' 21'
188600041023     c                   add       1             rach21
188700041023
188800050331     c                   when      TGDmad = ' 22'
188900041023     c                   add       1             rach22
189000041023
189100050331     c                   when      TGDmad = ' 30'
189200041023     c                   add       1             rach30
189300120914
189400120914     c                   when      TGDmad = ' 50'
189500120914     c                   add       1             rach50
189600041023
189700050331     c                   when      TGDmad = ' 99'
189800041023     c                   add       1             rach99
189900130515
190000130515     c                   when      TGDmad = '  8'
190100130515     c                   add       1             rachX1
190200140409
190300140409     c                   when      TGDmad = ' 88'
190400140409     c                   add       1             rachX2
190500050331     c                   endsl
190600041023      *
190700041023     c                   endif
190800041023
190900041023      * CHIUSI InFondati     suddivisi per motivo
191000041023
191100050331     c                   WHEN      tgdcdt = '  N'
191200041023      *
191300050331     c                   select
191400050331     c                   when      TGDmad = '  1'
191500041023     c                   add       1             racn01
191600041023
191700050331     c                   when      TGDmad = '  2'
191800041023     c                   add       1             racn02
191900041023
192000050331     c                   when      TGDmad = '  3'
192100041023     c                   add       1             racn03
192200041023
192300050331     c                   when      TGDmad = '  5'
192400041023     c                   add       1             racn05
192500041023
192600050331     c                   when      TGDmad = '  6'
192700041023     c                   add       1             racn06
192800041023
192900050331     c                   when      TGDmad = '  7'
193000041023     c                   add       1             racn07
193100041023
193200050331     c                   when      TGDmad = ' 10'
193300041023     c                   add       1             racn10
193400041023
193500050331     c                   when      TGDmad = ' 11'
193600041023     c                   add       1             racn11
193700041023
193800050331     c                   when      TGDmad = ' 12'
193900041023     c                   add       1             racn12
194000041023
194100050331     c                   when      TGDmad = ' 13'
194200041023     c                   add       1             racn13
194300041023
194400070710     c                   when      TGDmad = ' 14'
194500070710     c                   add       1             racn14
194600070710
194700070710     c                   when      TGDmad = ' 15'
194800070710     c                   add       1             racn15
194900070710
195000070710     c                   when      TGDmad = ' 16'
195100070710     c                   add       1             racn16
195200070710
195300050331     c                   when      TGDmad = ' 20'
195400041023     c                   add       1             racn20
195500041023
195600050331     c                   when      TGDmad = ' 21'
195700041023     c                   add       1             racn21
195800041023
195900050331     c                   when      TGDmad = ' 22'
196000041023     c                   add       1             racn22
196100041023
196200050331     c                   when      TGDmad = ' 30'
196300041023     c                   add       1             racn30
196400120914
196500120914     c                   when      TGDmad = ' 50'
196600120914     c                   add       1             racn50
196700041023
196800050331     c                   when      TGDmad = ' 99'
196900041023     c                   add       1             racn99
197000130515
197100130515     c                   when      TGDmad = '  8'
197200130515     c                   add       1             racnX1
197300140409
197400140409     c                   when      TGDmad = ' 88'
197500140409     c                   add       1             racnX2
197600050331     c                   endsl
197700041023
197800041023      * CHIUSI InFormativi   suddivisi per motivo
197900041023
198000050331     c                   WHEN      tgdcdt = '  I'
198100041023      *
198200050331     c                   select
198300050331     c                   when      TGDmad = '  1'
198400041023     c                   add       1             raci01
198500041023
198600050331     c                   when      TGDmad = '  2'
198700041023     c                   add       1             raci02
198800041023
198900050331     c                   when      TGDmad = '  3'
199000041023     c                   add       1             raci03
199100041023
199200050331     c                   when      TGDmad = '  5'
199300041023     c                   add       1             raci05
199400041023
199500050331     c                   when      TGDmad = '  6'
199600041023     c                   add       1             raci06
199700041023
199800050331     c                   when      TGDmad = '  7'
199900041023     c                   add       1             raci07
200000041023
200100050331     c                   when      TGDmad = ' 10'
200200041023     c                   add       1             raci10
200300041023
200400050331     c                   when      TGDmad = ' 11'
200500041023     c                   add       1             raci11
200600041023
200700050331     c                   when      TGDmad = ' 12'
200800041023     c                   add       1             raci12
200900041023
201000050331     c                   when      TGDmad = ' 13'
201100041023     c                   add       1             raci13
201200041023
201300070710     c                   when      TGDmad = ' 14'
201400070710     c                   add       1             raci14
201500070710
201600070710     c                   when      TGDmad = ' 15'
201700070710     c                   add       1             raci15
201800070710
201900070710     c                   when      TGDmad = ' 16'
202000070710     c                   add       1             raci16
202100070710
202200050331     c                   when      TGDmad = ' 20'
202300041023     c                   add       1             raci20
202400041023
202500050331     c                   when      TGDmad = ' 21'
202600041023     c                   add       1             raci21
202700041023
202800050331     c                   when      TGDmad = ' 22'
202900041023     c                   add       1             raci22
203000041023
203100050331     c                   when      TGDmad = ' 30'
203200041023     c                   add       1             raci30
203300120914
203400120914     c                   when      TGDmad = ' 50'
203500120914     c                   add       1             raci50
203600041023
203700050331     c                   when      TGDmad = ' 99'
203800041023     c                   add       1             raci99
203900130515
204000130515     c                   when      TGDmad = '  8'
204100130515     c                   add       1             raciX1
204200140409
204300140409     c                   when      TGDmad = ' 88'
204400140409     c                   add       1             raciX2
204500050331     c                   endsl
204600041023      *
204700050331     c                   ENDSL
204800050331
204900050331     c                   SELECT
205000041023
205100041023      * Chiamante Destinatario suddivisi per motivo
205200041023
205300050331     c                   WHEN      tgdtch = 'D'
205400041023      *
205500050331     c                   select
205600050331     c                   when      TGDmad = '  1'
205700041023     c                   add       1             racd01
205800041023
205900050331     c                   when      TGDmad = '  2'
206000041023     c                   add       1             racd02
206100041023
206200050331     c                   when      TGDmad = '  3'
206300041023     c                   add       1             racd03
206400041023
206500050331     c                   when      TGDmad = '  5'
206600041023     c                   add       1             racd05
206700041023
206800050331     c                   when      TGDmad = '  6'
206900041023     c                   add       1             racd06
207000041023
207100050331     c                   when      TGDmad = '  7'
207200041023     c                   add       1             racd07
207300041023
207400050331     c                   when      TGDmad = ' 10'
207500041023     c                   add       1             racd10
207600041023
207700050331     c                   when      TGDmad = ' 11'
207800041023     c                   add       1             racd11
207900041023
208000050331     c                   when      TGDmad = ' 12'
208100041023     c                   add       1             racd12
208200041023
208300050331     c                   when      TGDmad = ' 13'
208400041023     c                   add       1             racd13
208500041023
208600070710     c                   when      TGDmad = ' 14'
208700070710     c                   add       1             racd14
208800070710
208900070710     c                   when      TGDmad = ' 15'
209000070710     c                   add       1             racd15
209100070710
209200070710     c                   when      TGDmad = ' 16'
209300070710     c                   add       1             racd16
209400070710
209500050331     c                   when      TGDmad = ' 20'
209600041023     c                   add       1             racd20
209700041023
209800050331     c                   when      TGDmad = ' 21'
209900041023     c                   add       1             racd21
210000041023
210100050331     c                   when      TGDmad = ' 22'
210200041023     c                   add       1             racd22
210300041023
210400050331     c                   when      TGDmad = ' 30'
210500041023     c                   add       1             racd30
210600120914
210700120914     c                   when      TGDmad = ' 50'
210800120914     c                   add       1             racd50
210900041023
211000050331     c                   when      TGDmad = ' 99'
211100041023     c                   add       1             racd99
211200130515
211300130515     c                   when      TGDmad = '  8'
211400130515     c                   add       1             racdX1
211500140409
211600140409     c                   when      TGDmad = ' 88'
211700140409     c                   add       1             racdX2
211800050331     c                   endsl
211900041023
212000041023      * Chiamante Mittente     suddivisi per motivo
212100041023
212200050331     c                   WHEN      tgdtch = 'M'
212300041023      *
212400050331     c                   select
212500050331     c                   when      TGDmad = '  1'
212600041023     c                   add       1             racm01
212700041023
212800050331     c                   when      TGDmad = '  2'
212900041023     c                   add       1             racm02
213000041023
213100050331     c                   when      TGDmad = '  3'
213200041023     c                   add       1             racm03
213300041023
213400050331     c                   when      TGDmad = '  5'
213500041023     c                   add       1             racm05
213600041023
213700050331     c                   when      TGDmad = '  6'
213800041023     c                   add       1             racm06
213900041023
214000050331     c                   when      TGDmad = '  7'
214100041023     c                   add       1             racm07
214200041023
214300050331     c                   when      TGDmad = ' 10'
214400041023     c                   add       1             racm10
214500041023
214600050331     c                   when      TGDmad = ' 11'
214700041023     c                   add       1             racm11
214800041023
214900050331     c                   when      TGDmad = ' 12'
215000041023     c                   add       1             racm12
215100041023
215200050331     c                   when      TGDmad = ' 13'
215300041023     c                   add       1             racm13
215400041023
215500070710     c                   when      TGDmad = ' 14'
215600070710     c                   add       1             racm14
215700070710
215800070710     c                   when      TGDmad = ' 15'
215900070710     c                   add       1             racm15
216000070710
216100070710     c                   when      TGDmad = ' 16'
216200070710     c                   add       1             racm16
216300070710
216400050331     c                   when      TGDmad = ' 20'
216500041023     c                   add       1             racm20
216600041023
216700050331     c                   when      TGDmad = ' 21'
216800041023     c                   add       1             racm21
216900041023
217000050331     c                   when      TGDmad = ' 22'
217100041023     c                   add       1             racm22
217200041023
217300050331     c                   when      TGDmad = ' 30'
217400041023     c                   add       1             racm30
217500120914
217600120914     c                   when      TGDmad = ' 50'
217700120914     c                   add       1             racm50
217800041023
217900050331     c                   when      TGDmad = ' 99'
218000041023     c                   add       1             racm99
218100130515
218200130515     c                   when      TGDmad = '  8'
218300130515     c                   add       1             racmX1
218400140409
218500140409     c                   when      TGDmad = ' 88'
218600140409     c                   add       1             racmX2
218700050331     c                   endsl
218800041023
218900041023      * Chiamante Cliente      suddivisi per motivo
219000041023
219100050331     c                   WHEN      tgdtch = 'C'
219200041023      *
219300050331     c                   select
219400050331     c                   when      TGDmad = '  1'
219500041023     c                   add       1             racc01
219600041023
219700050331     c                   when      TGDmad = '  2'
219800041023     c                   add       1             racc02
219900041023
220000050331     c                   when      TGDmad = '  3'
220100041023     c                   add       1             racc03
220200041023
220300050331     c                   when      TGDmad = '  5'
220400041023     c                   add       1             racc05
220500041023
220600050331     c                   when      TGDmad = '  6'
220700041023     c                   add       1             racc06
220800041023
220900050331     c                   when      TGDmad = '  7'
221000041023     c                   add       1             racc07
221100041023
221200050331     c                   when      TGDmad = ' 10'
221300041023     c                   add       1             racc10
221400041023
221500050331     c                   when      TGDmad = ' 11'
221600041023     c                   add       1             racc11
221700041023
221800050331     c                   when      TGDmad = ' 12'
221900041023     c                   add       1             racc12
222000041023
222100050331     c                   when      TGDmad = ' 13'
222200041023     c                   add       1             racc13
222300041023
222400070710     c                   when      TGDmad = ' 14'
222500070710     c                   add       1             racc14
222600070710
222700070710     c                   when      TGDmad = ' 15'
222800070710     c                   add       1             racc15
222900070710
223000070710     c                   when      TGDmad = ' 16'
223100070710     c                   add       1             racc16
223200070710
223300050331     c                   when      TGDmad = ' 20'
223400041023     c                   add       1             racc20
223500041023
223600050331     c                   when      TGDmad = ' 21'
223700041023     c                   add       1             racc21
223800041023
223900050331     c                   when      TGDmad = ' 22'
224000041023     c                   add       1             racc22
224100041023
224200050331     c                   when      TGDmad = ' 30'
224300041023     c                   add       1             racc30
224400120914
224500120914     c                   when      TGDmad = ' 50'
224600120914     c                   add       1             racc50
224700041023
224800050331     c                   when      TGDmad = ' 99'
224900041023     c                   add       1             racc99
225000130515
225100130515     c                   when      TGDmad = '  8'
225200130515     c                   add       1             raccX1
225300140409
225400140409     c                   when      TGDmad = ' 88'
225500140409     c                   add       1             raccX2
225600050331     c                   endsl
225700041023
225800041023      * Chiamante Altro        suddivisi per motivo
225900041023
226000050331     c                   OTHER
226100041023      *
226200050331     c                   select
226300050331     c                   when      TGDmad = '  1'
226400041023     c                   add       1             racx01
226500041023
226600050331     c                   when      TGDmad = '  2'
226700041023     c                   add       1             racx02
226800041023
226900050331     c                   when      TGDmad = '  3'
227000041023     c                   add       1             racx03
227100041023
227200050331     c                   when      TGDmad = '  5'
227300041023     c                   add       1             racx05
227400041023
227500050331     c                   when      TGDmad = '  6'
227600041023     c                   add       1             racx06
227700041023
227800050331     c                   when      TGDmad = '  7'
227900041023     c                   add       1             racx07
228000041023
228100050331     c                   when      TGDmad = ' 10'
228200041023     c                   add       1             racx10
228300041023
228400050331     c                   when      TGDmad = ' 11'
228500041023     c                   add       1             racx11
228600041023
228700050331     c                   when      TGDmad = ' 12'
228800041023     c                   add       1             racx12
228900041023
229000050331     c                   when      TGDmad = ' 13'
229100041023     c                   add       1             racx13
229200041023
229300070710     c                   when      TGDmad = ' 14'
229400070710     c                   add       1             racx14
229500070710
229600070710     c                   when      TGDmad = ' 15'
229700070710     c                   add       1             racx15
229800070710
229900070710     c                   when      TGDmad = ' 16'
230000070710     c                   add       1             racx16
230100070710
230200050331     c                   when      TGDmad = ' 20'
230300041023     c                   add       1             racx20
230400041023
230500050331     c                   when      TGDmad = ' 21'
230600041023     c                   add       1             racx21
230700041023
230800050331     c                   when      TGDmad = ' 22'
230900041023     c                   add       1             racx22
231000041023
231100050331     c                   when      TGDmad = ' 30'
231200041023     c                   add       1             racx30
231300120914
231400120914     c                   when      TGDmad = ' 50'
231500120914     c                   add       1             racx50
231600041023
231700050331     c                   when      TGDmad = ' 99'
231800041023     c                   add       1             racx99
231900130515
232000130515     c                   when      TGDmad = '  8'
232100130515     c                   add       1             racxX1
232200140409
232300140409     c                   when      TGDmad = ' 88'
232400140409     c                   add       1             racxX2
232500050331     c                   endsl
232600041023      *
232700050331     c                   ENDSL
232800041023      *
232900041023
233000041023      * tipo comunicazione
233100050331     c                   select
233200041023      * fax
233300050331     c                   when      tgdtco = 'F'
233400041023     c                   add       1             ractcf
233500041023      * mail
233600050331     c                   when      tgdtco = 'M'
233700041023     c                   add       1             ractcm
233800041023      * lettera
233900050331     c                   when      tgdtco = 'L'
234000041023     c                   add       1             ractcl
234100041023      * telefono
234200050331     c                   when      tgdtco = 'T'
234300041023     c                   add       1             ractct
234400050331     c                   endsl
234500041023      *
234600041023      * aggancio record dell'utente
234700041023      *
234800041027     c                   if        Id0ord = '1' and not *in01
234900041023     c                   if        tgdcdt <> *blanks
235000041023     c     tgdutc        chain     Wfrut00f
235100041023     c                   else
235200041023     C     tgdrpg        chain     Wfrut00f
235300041023     c                   endif
235400041023     c                   if        not %found(wfrut00f)
235500041023     c                   clear                   wfrut000
235600041023     c                   if        tgdcdt <> *blanks
235700041023     c                   eval      rutute = tgdutc
235800041023     c                   else
235900041023     c                   eval      rutute = tgdrpg
236000041023     c                   endif
236100041023     c                   endif
236200041023
236300041023     c                   if        tgdcdt = *blanks
236400041023      * le aperte
236500041023     c                   add       1             rutape
236600041023      * le aperte  da xx giorni
236700041023     c                   If        Id0xgg > 0 and id0ord = '1'  and
236800041025     c                             tgddar <= wdataoggi
236900041023     c                   add       1             rutdxg
237000041023     c                   endif
237100041023      *
237200041023      * chiusi
237300041023      *
237400041023     c                   else
237500050331     c                   select
237600041023      * fondati
237700050331     c                   when      tgdcdt = '  F'
237800041023     c                   add       1             rutfon
237900041023      * chiusi da altri
238000041023     c                   if        tgdpor <> tgdpot
238100041023     c                   add       1             rutcfa
238200041023     c                   endif
238300041023      * infondati
238400050331     c                   when      tgdcdt = '  N'
238500041023     c                   add       1             rutifo
238600041023      * informativo
238700050331     c                   when      tgdcdt = '  I'
238800041023     c                   add       1             rutinf
238900050331     c                   endsl
239000041023
239100041023     c                   endif
239200041023
239300041023      * tipo comunicazione
239400041023
239500050331     c                   select
239600041023      * fax
239700050331     c                   when      tgdtco = 'F'
239800041023     c                   add       1             ruttcf
239900041023      * mail
240000050331     c                   when      tgdtco = 'M'
240100041023     c                   add       1             ruttcm
240200041023      * lettera
240300050331     c                   when      tgdtco = 'L'
240400041023     c                   add       1             ruttcl
240500041023      * telefono
240600050331     c                   when      tgdtco = 'T'
240700041023     c                   add       1             ruttct
240800050331     c                   endsl
240900141203
241000141203      /free
241100141203       //?Chi chiama
241200141203         SELECT;
241300141203         WHEN  TGDtch = 'D';
241400141203           RUTccd += 1;
241500141203         WHEN  TGDtch = 'M';
241600141203           RUTccm += 1;
241700141203         WHEN  TGDtch = 'C';
241800141203           RUTccc += 1;
241900141203         OTHER;
242000141203           RUTcca += 1;
242100141203         ENDSL;
242200141203      /end-free
242300141203
242400041022      *
242500041023     c                   if        not %found(wfrut00f)
242600041025     c                   write     wfrut000
242700041023     c                   else
242800041025     c                   update    wfrut000
242900041023     c                   endif
243000041025     c                   endif
243100041023      *
243200041023     c                   endsr
243300121116
243400121116      /free
243500121116       //--------------------------------------------------------------
243600121116       //?Memorizzo sul file di work per dettaglio chiamante.
243700121116       //--------------------------------------------------------------
243800121116       BEGSR MemoTCH;
243900121116
244000121116       //?Aggancio il file di work x tipo chiamante
244100121116         chain (RACdis:RACare:RACpoe:RACtpo:TGDtch) WFRAD01L;
244200121116         IF  not %found(WFRAD01L);
244300121116           clear WFRAD000;
244400121116           RADtpe = RACtpe;
244500121116           RADdte = RACdte;
244600121116           RADddr = RACddr;
244700121116           RADadr = RACadr;
244800121116           RADddc = RACddc;
244900121116           RADadc = RACadc;
245000121116           RADpoe = RACpoe;
245100121116           RADtpo = RACtpo;
245200121116           RADdis = RACdis;
245300121116           RADare = RACare;
245400121116           RADtch = TGDtch;
245500121116           exsr prepRAD;
245600121116           WRITE  WFRAD000;
245700121116         ELSE;
245800121116           exsr prepRAD;
245900121116           UPDATE WFRAD000;
246000121116         ENDIF;
246100121116
246200121116       ENDSR;
246300121116       //--------------------------------------------------------------
246400121116       //?Preparo i campi per il file WFRAD00F.
246500121116       //--------------------------------------------------------------
246600121116       BEGSR prepRAD;
246700121119
246800121119         SELECT;
246900121119       //?Aperti
247000121119           WHEN  TGDcdt = *blanks;
247100121119         //?Suddivisi per motivo
247200121119             SELECT;
247300121119               WHEN  TGDmad = '  1';
247400121119                 RADa01 += 1;
247500121119               WHEN  TGDmad = '  2';
247600121119                 RADa02 += 1;
247700121119               WHEN  TGDmad = '  3';
247800121119                 RADa03 += 1;
247900121119               WHEN  TGDmad = '  5';
248000121119                 RADa05 += 1;
248100121119               WHEN  TGDmad = '  6';
248200121119                 RADa06 += 1;
248300121119               WHEN  TGDmad = '  7';
248400121119                 RADa07 += 1;
248500121119               WHEN  TGDmad = ' 10';
248600121119                 RADa10 += 1;
248700121119               WHEN  TGDmad = ' 11';
248800121119                 RADa11 += 1;
248900121119               WHEN  TGDmad = ' 12';
249000121119                 RADa12 += 1;
249100121119               WHEN  TGDmad = ' 13';
249200121119                 RADa13 += 1;
249300121119               WHEN  TGDmad = ' 14';
249400121119                 RADa14 += 1;
249500121119               WHEN  TGDmad = ' 15';
249600121119                 RADa15 += 1;
249700121119               WHEN  TGDmad = ' 16';
249800121119                 RADa16 += 1;
249900121119               WHEN  TGDmad = ' 20';
250000121119                 RADa20 += 1;
250100121119               WHEN  TGDmad = ' 21';
250200121119                 RADa21 += 1;
250300121119               WHEN  TGDmad = ' 22';
250400121119                 RADa22 += 1;
250500121119               WHEN  TGDmad = ' 30';
250600121119                 RADa30 += 1;
250700121119               WHEN  TGDmad = ' 50';
250800121119                 RADa50 += 1;
250900121119               WHEN  TGDmad = ' 99';
251000121119                 RADa99 += 1;
251100130515               WHEN  TGDmad = '  8';
251200130515                 RADaX1 += 1;
251300140409               WHEN  TGDmad = ' 88';
251400140409                 RADaX2 += 1;
251500121119             ENDSL;
251600121119
251700121119         //?Aperti da x gg. suddivisi per motivo
251800121119             IF  ID0xgg > 0 and ID0ord = '1' and TGDdar <= wdataoggi;
251900121119           //?Suddivisi per motivo
252000121119               SELECT;
252100121119                 WHEN  TGDmad = '  1';
252200121119                   RADg01 += 1;
252300121119                 WHEN  TGDmad = '  2';
252400121119                   RADg02 += 1;
252500121119                 WHEN  TGDmad = '  3';
252600121119                   RADg03 += 1;
252700121119                 WHEN  TGDmad = '  5';
252800121119                   RADg05 += 1;
252900121119                 WHEN  TGDmad = '  6';
253000121119                   RADg06 += 1;
253100121119                 WHEN  TGDmad = '  7';
253200121119                   RADg07 += 1;
253300121119                 WHEN  TGDmad = ' 10';
253400121119                   RADg10 += 1;
253500121119                 WHEN  TGDmad = ' 11';
253600121119                   RADg11 += 1;
253700121119                 WHEN  TGDmad = ' 12';
253800121119                   RADg12 += 1;
253900121119                 WHEN  TGDmad = ' 13';
254000121119                   RADg13 += 1;
254100121119                 WHEN  TGDmad = ' 14';
254200121119                   RADg14 += 1;
254300121119                 WHEN  TGDmad = ' 15';
254400121119                   RADg15 += 1;
254500121119                 WHEN  TGDmad = ' 16';
254600121119                   RADg16 += 1;
254700121119                 WHEN  TGDmad = ' 20';
254800121119                   RADg20 += 1;
254900121119                 WHEN  TGDmad = ' 21';
255000121119                   RADg21 += 1;
255100121119                 WHEN  TGDmad = ' 22';
255200121119                   RADg22 += 1;
255300121119                 WHEN  TGDmad = ' 30';
255400121119                   RADg30 += 1;
255500121119                 WHEN  TGDmad = ' 50';
255600121119                   RADg50 += 1;
255700121119                 WHEN  TGDmad = ' 99';
255800121119                   RADg99 += 1;
255900130515                 WHEN  TGDmad = '  8';
256000130515                   RADgX1 += 1;
256100140409                 WHEN  TGDmad = ' 88';
256200140409                   RADgX2 += 1;
256300121119               ENDSL;
256400121119             ENDIF;
256500121119
256600121119       //?Chiusi Fondati
256700121119           WHEN  TGDcdt = '  F';
256800121119         //?Suddivisi per motivo
256900121119             SELECT;
257000121119               WHEN  TGDmad = '  1';
257100121119                 RADf01 += 1;
257200121119               WHEN  TGDmad = '  2';
257300121119                 RADf02 += 1;
257400121119               WHEN  TGDmad = '  3';
257500121119                 RADf03 += 1;
257600121119               WHEN  TGDmad = '  5';
257700121119                 RADf05 += 1;
257800121119               WHEN  TGDmad = '  6';
257900121119                 RADf06 += 1;
258000121119               WHEN  TGDmad = '  7';
258100121119                 RADf07 += 1;
258200121119               WHEN  TGDmad = ' 10';
258300121119                 RADf10 += 1;
258400121119               WHEN  TGDmad = ' 11';
258500121119                 RADf11 += 1;
258600121119               WHEN  TGDmad = ' 12';
258700121119                 RADf12 += 1;
258800121119               WHEN  TGDmad = ' 13';
258900121119                 RADf13 += 1;
259000121119               WHEN  TGDmad = ' 14';
259100121119                 RADf14 += 1;
259200121119               WHEN  TGDmad = ' 15';
259300121119                 RADf15 += 1;
259400121119               WHEN  TGDmad = ' 16';
259500121119                 RADf16 += 1;
259600121119               WHEN  TGDmad = ' 20';
259700121119                 RADf20 += 1;
259800121119               WHEN  TGDmad = ' 21';
259900121119                 RADf21 += 1;
260000121119               WHEN  TGDmad = ' 22';
260100121119                 RADf22 += 1;
260200121119               WHEN  TGDmad = ' 30';
260300121119                 RADf30 += 1;
260400121119               WHEN  TGDmad = ' 50';
260500121119                 RADf50 += 1;
260600121119               WHEN  TGDmad = ' 99';
260700121119                 RADf99 += 1;
260800130515               WHEN  TGDmad = '  8';
260900130515                 RADfX1 += 1;
261000140409               WHEN  TGDmad = ' 88';
261100140409                 RADfX2 += 1;
261200121119             ENDSL;
261300121119
261400121119       //?Chiusi Fondati da altri --> P.O. apertura diverso da P.O. responsabile
261500121119             IF  TGDcdt = '  F'  and TGDpor <> TGDpot;
261600121119         //?Suddivisi per motivo
261700121119               SELECT;
261800121119                 WHEN  TGDmad = '  1';
261900121119                   RADh01 += 1;
262000121119                 WHEN  TGDmad = '  2';
262100121119                   RADh02 += 1;
262200121119                 WHEN  TGDmad = '  3';
262300121119                   RADh03 += 1;
262400121119                 WHEN  TGDmad = '  5';
262500121119                   RADh05 += 1;
262600121119                 WHEN  TGDmad = '  6';
262700121119                   RADh06 += 1;
262800121119                 WHEN  TGDmad = '  7';
262900121119                   RADh07 += 1;
263000121119                 WHEN  TGDmad = ' 10';
263100121119                   RADh10 += 1;
263200121119                 WHEN  TGDmad = ' 11';
263300121119                   RADh11 += 1;
263400121119                 WHEN  TGDmad = ' 12';
263500121119                   RADh12 += 1;
263600121119                 WHEN  TGDmad = ' 13';
263700121119                   RADh13 += 1;
263800121119                 WHEN  TGDmad = ' 14';
263900121119                   RADh14 += 1;
264000121119                 WHEN  TGDmad = ' 15';
264100121119                   RADh15 += 1;
264200121119                 WHEN  TGDmad = ' 16';
264300121119                   RADh16 += 1;
264400121119                 WHEN  TGDmad = ' 20';
264500121119                   RADh20 += 1;
264600121119                 WHEN  TGDmad = ' 21';
264700121119                   RADh21 += 1;
264800121119                 WHEN  TGDmad = ' 22';
264900121119                   RADh22 += 1;
265000121119                 WHEN  TGDmad = ' 30';
265100121119                   RADh30 += 1;
265200121119                 WHEN  TGDmad = ' 50';
265300121119                   RADh50 += 1;
265400121119                 WHEN  TGDmad = ' 99';
265500121119                   RADh99 += 1;
265600130515                 WHEN  TGDmad = '  8';
265700130515                   RADhX1 += 1;
265800140409                 WHEN  TGDmad = ' 88';
265900140409                   RADhX2 += 1;
266000121119               ENDSL;
266100121119             ENDIF;
266200121119
266300121119       //?Chiusi Infondati
266400121119           WHEN  TGDcdt = '  N';
266500121119         //?Suddivisi per motivo
266600121119             SELECT;
266700121119               WHEN  TGDmad = '  1';
266800121119                 RADn01 += 1;
266900121119               WHEN  TGDmad = '  2';
267000121119                 RADn02 += 1;
267100121119               WHEN  TGDmad = '  3';
267200121119                 RADn03 += 1;
267300121119               WHEN  TGDmad = '  5';
267400121119                 RADn05 += 1;
267500121119               WHEN  TGDmad = '  6';
267600121119                 RADn06 += 1;
267700121119               WHEN  TGDmad = '  7';
267800121119                 RADn07 += 1;
267900121119               WHEN  TGDmad = ' 10';
268000121119                 RADn10 += 1;
268100121119               WHEN  TGDmad = ' 11';
268200121119                 RADn11 += 1;
268300121119               WHEN  TGDmad = ' 12';
268400121119                 RADn12 += 1;
268500121119               WHEN  TGDmad = ' 13';
268600121119                 RADn13 += 1;
268700121119               WHEN  TGDmad = ' 14';
268800121119                 RADn14 += 1;
268900121119               WHEN  TGDmad = ' 15';
269000121119                 RADn15 += 1;
269100121119               WHEN  TGDmad = ' 16';
269200121119                 RADn16 += 1;
269300121119               WHEN  TGDmad = ' 20';
269400121119                 RADn20 += 1;
269500121119               WHEN  TGDmad = ' 21';
269600121119                 RADn21 += 1;
269700121119               WHEN  TGDmad = ' 22';
269800121119                 RADn22 += 1;
269900121119               WHEN  TGDmad = ' 30';
270000121119                 RADn30 += 1;
270100121119               WHEN  TGDmad = ' 50';
270200121119                 RADn50 += 1;
270300121119               WHEN  TGDmad = ' 99';
270400121119                 RADn99 += 1;
270500130515               WHEN  TGDmad = '  8';
270600130515                 RADnX1 += 1;
270700140409               WHEN  TGDmad = ' 88';
270800140409                 RADnX2 += 1;
270900121119             ENDSL;
271000121119
271100121119       //?Chiusi Informativi
271200121119           WHEN  TGDcdt = '  I' ;
271300121119         //?Suddivisi per motivo
271400121119             SELECT;
271500121119               WHEN  TGDmad = '  1';
271600121119                 RADi01 += 1;
271700121119               WHEN  TGDmad = '  2';
271800121119                 RADi02 += 1;
271900121119               WHEN  TGDmad = '  3';
272000121119                 RADi03 += 1;
272100121119               WHEN  TGDmad = '  5';
272200121119                 RADi05 += 1;
272300121119               WHEN  TGDmad = '  6';
272400121119                 RADi06 += 1;
272500121119               WHEN  TGDmad = '  7';
272600121119                 RADi07 += 1;
272700121119               WHEN  TGDmad = ' 10';
272800121119                 RADi10 += 1;
272900121119               WHEN  TGDmad = ' 11';
273000121119                 RADi11 += 1;
273100121119               WHEN  TGDmad = ' 12';
273200121119                 RADi12 += 1;
273300121119               WHEN  TGDmad = ' 13';
273400121119                 RADi13 += 1;
273500121119               WHEN  TGDmad = ' 14';
273600121119                 RADi14 += 1;
273700121119               WHEN  TGDmad = ' 15';
273800121119                 RADi15 += 1;
273900121119               WHEN  TGDmad = ' 16';
274000121119                 RADi16 += 1;
274100121119               WHEN  TGDmad = ' 20';
274200121119                 RADi20 += 1;
274300121119               WHEN  TGDmad = ' 21';
274400121119                 RADi21 += 1;
274500121119               WHEN  TGDmad = ' 22';
274600121119                 RADi22 += 1;
274700121119               WHEN  TGDmad = ' 30';
274800121119                 RADi30 += 1;
274900121119               WHEN  TGDmad = ' 50';
275000121119                 RADi50 += 1;
275100121119               WHEN  TGDmad = ' 99';
275200121119                 RADi99 += 1;
275300130515               WHEN  TGDmad = '  8';
275400130515                 RADiX1 += 1;
275500140409               WHEN  TGDmad = ' 88';
275600140409                 RADiX2 += 1;
275700121119             ENDSL;
275800121119
275900121119         ENDSL;
276000121119
276100121119       //?Tipo comunicazione
276200121119         SELECT;
276300121119         //?Fax
276400121119           WHEN  TGDtco = 'F';
276500121119             RADtcf += 1;
276600121119         //?Mail
276700121119           WHEN  TGDtco = 'M';
276800121119             RADtcm += 1;
276900121119         //?Lettera
277000121119           WHEN  TGDtco = 'L';
277100121119             RADtcl += 1;
277200121119         //?Telefono
277300121119           WHEN  TGDtco = 'T';
277400121119             RADtct += 1;
277500121119         ENDSL;
277600121116
277700121116       ENDSR;
277800121116      /end-free
277900041022      *
278000041023      *---------------------------------------------------------------*
278100041023      * Scrittura del file di reportistica                            *
278200041023      *---------------------------------------------------------------*
278300041023     c     SR_Report     BegSr
278400041023      *
278500041201      * Calcolo totale spedizioni del periodo
278600041201      *
278700041201
278800041201      * imposto il periodo di ricerca numero spedizioni
278900041201      *
279000050405      * se immesso  un  solo  range di date: "vince" quello
279100050405      * se immessi entrambi i range di date: "vince" quello x data richiesta
279200050405     c                   if        id0amgdcd > *zeros
279300041202     c                   MOVEL     id0amgdcd     periododa
279400041202     c                   MOVEL     id0amgdca     periodoa
279500041201     c                   else
279600041202     c                   MOVEL     id0amgdhd     periododa
279700041202     c                   MOVEL     id0amgdha     periodoa
279800041201     c                   endif
279900060515      * se tipo ordinamento 1 e selezionato un solo PO  metto la  L1 nei po da elaborare
280000060515     c                   if        id0ord = '1' and id0fil > 0
280100041202     c                   clear                   trul06ds
280200041202     c                   move      '£1'          d06cod
280300041202     c                   movel     id0fil        d06key
280400041202     c                   movel(p)  trul06ds      KPJBU
280500041202
280600041202     c                   call      'TRUL06R'
280700041202     c                   parm                    kpjba
280800041202     c                   movel     kpjbu         trul06ds
280900041202    3c                   do        30            x1
281000041202    4c                   if        Lin(x1) = 0
281100041202     c                   leave
281200041202    4c                   endif
281300041203     c     lin(x1)       lookup    skpoel(1)                              30
281400041202      *
281500041202    6c                   if        not %found
281600041202     c                   z-add     1             xx
281700041202     c     000           lookup    SKpoel(xx)                             30
281800041202     c                   movea     lin(x1)       skpoel(xx)
281900041202    6c                   endif
282000041202      *
282100041202     c                   enddo
282200041202      *
282300041202     c                   endif
282400041202      *
282500041201      *
282600041202      * lettura del file sisdr01l
282700041201      *
282800050404     c     annoda        setll     sisdr000
282900041201     c                   do        *hival
283000041202
283100050404     c                   read      sisdr000
283200041201      *
283300041202     c                   if        %eof(sisdr01l)
283400041201     c                   leave
283500041201     c                   endif
283600041201      *
283700041201      * verifico il periodo
283800041202
283900041202     c                   z-add     sdrann        wsdrann
284000041202     c                   z-add     sdrmes        wsdrmes
284100041202
284200041201     c                   if        wsdrper > periodoa
284300041202     c                   iter
284400041201     c                   endif
284500041201      *
284600041201     c                   if        wsdrper < periododa
284700041201     c                   iter
284800041201     c                   endif
284900041201      * verifico se spedizioni normali
285000041201     c                   if        sdrrbl <> 'N'
285100041201     c                   iter
285200041201     c                   endif
285300041201      *
285400041201      * eseguo caricamento schiera e calcoli
285500041201
285600041201      * verifico punto operativo
285700041201      *
285800041201     c                   z-add     1             xx
285900041202     c     sdrflp        lookup    skpoel(XX)                             30
286000041203     c                   If        %found
286100041201      * totalizzo le spedizioni
286200041201     c                   add       sdrnsp        sksped(xx)
286300041203     c                   endif
286400060511      * verifico se punto operativo  internazionale
286500060511      *
286600060511     c                   z-add     1             xx
286700060511     c     sdrflp        lookup    skpoel_i(XX)                           30
286800060511     c                   If        %found
286900060511      * totalizzo le spedizioni internazionali
287000060511     c                   add       sdrnsp        sksped_i(xx)
287100060511     c                   endif
287200060511
287300041202     c                   z-add     1             xx
287400041202     c     sdrfla        lookup    skpoel(XX)                             30
287500041203     c                   If        %found
287600041202      * totalizzo le spedizioni
287700041202     c                   add       sdrnsp        sksped(xx)
287800041203     c                   endif
287900060511      * verifico se punto operativo  internazionale
288000060511      *
288100060511     c                   z-add     1             xx
288200060511     c     sdrfla        lookup    skpoel_i(XX)                           30
288300060511     c                   If        %found
288400060511      * totalizzo le spedizioni internazionali
288500060511     c                   add       sdrnsp        sksped_i(xx)
288600060511     c                   endif
288700060511
288800041201      *
288900041201     c                   enddo
289000041202      *
289100041202
289200140130      *
289300121116      /free
289400121116        wcmd = 'CLRPFM '+ %trim(wlib) + '/WFRAC00F';
289500121116        ExecuteCommand (wcmd);
289600121116      /end-free
289700041201
289800041023     c                   open      wfrac01l
289900121116
290000041023      *
290100041023     c                   feod      wfrac11l
290200041023     c     *loval        setll     wfrac11l
290300041023      *
290400041202    1c                   do        *hival
290500041023     c                   read      wfrac11l
290600041202    2c                   if        %eof(wfrac11l)
290700041023     c                   leave
290800041202    2c                   endif
290900041202      * se non è internazionale
291000041202    2c                   if        ractpo = ' '
291100041202     c                   z-add     1             xx
291200041202     c     racpoe        lookup    skpoel(xx)                             30
291300041202      *
291400041202    3c                   if        %found
291500041202     c                   z-add     SKsped(xx)    racspe
291600050401     c                   z-sub     SKpoel(xx)    SKpoel(xx)
291700121116
291800121116      /free
291900121116       //?Imposto il totale anche su tutti i rcd della stessa filiale su WFRAD
292000121116         IF  ID0crptch = 'S';
292100121116           exsr aggRAD;
292200121116         ENDIF;
292300121116      /end-free
292400121116
292500041202    3c                   endif
292600041202      *
292700041202   x2c                   else
292800041202      * internazionale
292900060511      *                cerco i po internazionali  se non sono la sede
293000041202      *
293100060511   2ac                   If        racpoe <> 046
293200041202     c                   clear                   trul06ds
293300041202     c                   move      '£1'          d06cod
293400041202     c                   movel     racpoe        d06key
293500041202     c                   movel(p)  trul06ds      KPJBU
293600041202
293700041202     c                   call      'TRUL06R'
293800041202     c                   parm                    kpjba
293900041202     c                   movel     kpjbu         trul06ds
294000041202     c                   movea     lin           Skpoin
294100041202     c                   z-add     0             x1
294200041202    3c                   do        30            x1
294300041202    4c                   if        skpoin(x1) = 0
294400041202     c                   leave
294500041202    4c                   endif
294600050404     c     skpoin(x1)    chain     azorg01
294700041202    4c                   if        %found(azorg01l)
294800041202      * controllo il network
294900041202     c                   movel     orgde3        og143
295000041202    5c                   if        §ogntw = 'DPD' or §ogntw = 'EEX'
295100050411     c                                            or §ogntw = 'FED'
295200041202     c                   z-add     1             xx
295300041202     c     orgfil        lookup    skpoel(xx)                             30
295400041202      *
295500041202    6c                   if        %found
295600041202     c                   add       SKsped(xx)    racspe
295700050401     c                   z-sub     SKpoel(xx)    SKpoel(xx)
295800041202    6c                   endif
295900121116
296000121116      /free
296100121116       //?Imposto il totale anche su tutti i rcd della stessa filiale su WFRAD
296200121116         IF  ID0crptch = 'S';
296300121116           exsr aggRAD;
296400121116         ENDIF;
296500121116      /end-free
296600121116
296700041202      *
296800041202    5c                   endif
296900041202      *
297000041202    4c                   endif
297100041202      *
297200041202    3c                   enddo
297300060511      * se sono in sede interanzionale
297400060511   2ac                   else
297500060511     c                   xfoot     sksped_i      racspe
297600121116
297700121116      /free
297800121116       //?Imposto il totale anche su tutti i rcd della stessa filiale su WFRAD
297900121116       //?internazionale
298000121116         IF  ID0crptch = 'S';
298100121116           exsr aggRAD;
298200121116         ENDIF;
298300121116      /end-free
298400121116
298500060511   2ac                   endif
298600041202      *
298700041202    2c                   endif
298800041202      *
298900041025     c                   write     wfrac000
299000041202    1c                   enddo
299100050404      *
299200050404      *
299300050404      * Proseguo SOLO SE se richiesto l'ordinamento '2'
299400050404      *   (per p.o. responsabile)
299500050404if  1c                   if        ID0ord = '1'
299600050404     c                   leavesr
299700050404e   1c                   endif
299800050404      *
299900050401      *
300000050401      * Verifico se ho registrato tutti i P.O
300100050401      *   ovvero se ce ne sono senza reclami...
300200050401     c                   clear                   WFRAC000
300300050401     c                   eval      RACtpe = ID0ord
300400050401     c                   eval      RACdte = DateU
300500050401     c                   eval      RACddr = ID0AMGDCD
300600050401     c                   eval      RACadr = ID0AMGDCA
300700050401     c                   eval      RACddc = ID0AMGDHD
300800050401     c                   eval      RACadc = ID0AMGDHA
300900050401      *
301000050401     c                   eval      xx =  1
301100050401do  1c                   dow           xx         <= %elem(SKpoel)
301200050401     c                             and SKpoel(xx) <> *zeros
301300050401      *
301400050401      * seleziono i p.o. di cui NON ho ancora registrato record
301500050404      *   (anche se con totale spedizioni = *zeros)
301600050401if  2c                   if            SKpoel(xx) >  *zeros
301700050401      *
301800050401     c     SKpoel(xx)    chain     AZORG01
301900050401if  3c                   if        %found(AZORG01L)
302000050404     c                             and   ORGfva = *blanks
302100050404     c                   movel     ORGde3        Og143
302200050404     c                   movel     ORGde8        Og148
302300050404if  4c                   if            (§OGlpo = '1'
302400050404     c                              or  §OGlpo = '2')
302500050404     c                             and (§OGntw = 'COR'
302600050404     c                              or  §OGntw = 'MES')
302700050404     c                              or  §OGntw = 'EEX'
302800050404     c                              or  §OGntw = 'DPD'
302900050404     c                              or  §OGntw = 'FED'
303000050404     c                              or  §OGntw = 'PPT'
303100050401     c                   eval      RACdis = ORGfl3
303200050401     c                   eval      RACare = ORGcar
303300050401     c                   eval      RACpoe = SKpoel(xx)
303400050401     c                   eval      RACspe = SKsped(xx)
303500050401      *
303600050401     c                   WRITE     WFRAC000
303700121116
303800121116      /free
303900121116       //?Se richiesto scrivo anche il WFRAD
304000121116         IF  ID0crptch = 'S';
304100121116           exsr wrtRAD;
304200121116         ENDIF;
304300121116      /end-free
304400121116
304500050404e   4c                   endif
304600050404e   3c                   endif
304700050401      *
304800050401e   2c                   endif
304900050401     c                   eval      xx =  xx + 1
305000050401      *
305100050401e   1c                   enddo
305200041023      *
305300041023     c                   endsr
305400121116
305500121116      /free
305600121116       //--------------------------------------------------------------
305700121116       //?Aggiorno i dati su WFRAD.
305800121116       //--------------------------------------------------------------
305900121116       BEGSR aggRAD;
306000121116
306100121116       //?Aggiorno il totale spedizioni per tutti i rcd con la stessa filiale
306200121116         setll (RACdis:RACare:RACpoe:RACtpo) WFRAD01L;
306300121116         reade (RACdis:RACare:RACpoe:RACtpo) WFRAD01L;
306400121116         DOW not %eof(WFRAD01L);
306500121116           RADspe = RACspe;
306600121116           UPDATE WFRAD000;
306700121116           reade (RACdis:RACare:RACpoe:RACtpo) WFRAD01L;
306800121116         ENDDO;
306900121116
307000121116       ENDSR;
307100121116       //--------------------------------------------------------------
307200121116       //?Sscrivo i dati su WFRAD.
307300121116       //--------------------------------------------------------------
307400121116       BEGSR wrtRAD;
307500121116
307600121116       //?Scrivo gli stessi rcd aggiunti su WFRAC anche sul WFRAD
307700121116         clear WFRAD000;
307800121116         RADtpe = RACtpe;
307900121116         RADdte = RACdte;
308000121116         RADddr = RACddr;
308100121116         RADadr = RACadr;
308200121116         RADddc = RACddc;
308300121116         RADadc = RACadc;
308400121116         RADpoe = RACpoe;
308500121116         RADtpo = RACtpo;
308600121116         RADdis = RACdis;
308700121116         RADare = RACare;
308800121116         RADspe = RACspe;
308900121116         WRITE  WFRAD000;
309000121116
309100121116       ENDSR;
309200121116      /end-free
309300121116
309400041023      *---------------------------------------------------------------*
309500041023      * Stampa della testata                                          *
309600041023      *---------------------------------------------------------------*
309700041023     c     SR_Testa      BegSr
309800041026      *
309900041026     c                   seton                                        15
310000041023      * se sono in sede e ci sono distretto valorizzato lo devo decodificare
310100041023     c                   if        *in01   and id0dst <> *blanks
310200041023     c                   eval      p3cdst = id0dst
310300041023      *
310400041023     c                   movel     '17'          cod
310500041023     c                   movel(p)  p3cdst        key
310600050404     c     ktab          chain     tabel
310700041023      *
310800041023     c                   movel(p)  tbluni        p3dds2
310900041023     c                   endif
311000041023      *
311100041025     c                   if        *in01   and id0are > 0
311200041023     c                   eval      p3care = id0are
311300041023      *
311400041023     c                   movel     '05'          cod
311500041023     c                   movel(p)  p3care        key
311600050404     c     ktab          chain     tabel
311700041023      *
311800041023     c                   movel(p)  tbluni        p3dar2
311900041023     c                   endif
312000041023     c* Ordinamento
312100041023     c                   if        id0ord='1'
312200041023     c                   eval      p1dord='Per Numero Richiesta '
312300041023     c                   endif
312400041023     c                   if        id0ord='2'
312500041023     c                   eval      p1dord='Per P.O. RESPONSABILE'
312600041023     c                   endif
312700041023      * p.o. responsabile
312800041023     c                   If        id0prs > 0
312900050404     c     id0prs        chain     azorg01
313000041230     c                   movel     orgdes        p1dprs
313100041023     c                   endif
313200041023      *
313300041023      * p.o. apertura
313400041023     c                   If        id0fil > 0
313500050404     c     id0fil        chain     azorg01
313600041230     c                   movel     orgdes        p1dfil
313700041023     c                   endif
313800041023      * se sono in sede e P.O. non valorizzati e distretto e area non valorizzato metto
313900041023      * tutti
314000041025     c                   if        *in01 and id0dst = ' ' and id0are = 0 and
314100041023     c                             id0prs = 0 and id0fil = 0
314200041023     c                   eval      p3dds2 = 'TUTTI'
314300041023     c                   eval      p3dar2 = 'TUTTE'
314400041023     c                   endif
314500041023      *
314600041025     c                   if        *in01 and id0dst<>' ' and id0are=0 and
314700041023     c                             id0prs = 0 and id0fil = 0
314800041023     c                   eval      p3dar2 = 'TUTTE'
314900041023     c                   endif
315000041023      *
315100041023     c                   write     dnd1t1
315200041023     c                   write     dnd1ts
315300041023     c                   write     dnd102
315400041026
315500041026     c                   setoff                                       15
315600041023
315700041023     c                   endsr
315800041023      *
315900041022      *---------------------------------------------------------------*
316000041022      * Reperimento Dati del job (Utente/Operativi)                   *
316100041022      *---------------------------------------------------------------*
316200041022     c     DatiJob       BegSr
316300041022      *
316400041022     c     *dtaara       define    §azute        azuteds
316500041022     c     *dtaara       define    §datiute      ddatiute
316600041022      *
316700041022     c                   in(E)     *dtaara
316800041022     c                   IF        %ERROR or RSUT = *blanks
316900041022     c                   clear                   Tibs34Ds
317000041022     c                   call      'TIBS34R'
317100041022     c                   parm                    Tibs34Ds
317200041022     c                   in        *dtaara
317300041022     c                   ENDIF
317400041023      * - file TABEL00F
317500041022     c     ktab          klist
317600041022     c                   kfld                    codut
317700041022     c                   kfld                    cod
317800041022     c                   kfld                    key
317900041026      * - file FITGN01L
318000041026     c     ktgN          klist
318100041026     c                   kfld                    tgdant
318200041026     c                   kfld                    tgdnut
318300041026     c                   kfld                    tgdpdt
318400041023      * - file WFRAC11L
318500041023     c     Krac          klist
318600041023     c                   kfld                    p3cdst
318700041023     c                   kfld                    p3care
318800041023     c                   kfld                    p3cfil
318900041023     c                   kfld                    W_tpo
319000041023      * - file AZCLN01L
319100041023     c     kazcln        klist
319200041023     c                   kfld                    ktfp
319300041023     c                   kfld                    ktfa
319400041023     c                   kfld                    kann
319500041023     c                   kfld                    kmes
319600041022      *
319700041022     c                   EndSr
319800041020
319900041022      *---------------------------------------------------------------*
320000041022      * Routine Iniziale                                              *
320100041022      *---------------------------------------------------------------*
320200041022     c     *InzSR        BegSR
320300041022      *
320400041022     c     *Entry        plist
320500041022     c                   parm                    KPJBA
320600041022     c                   movel     kpjbu         fidnD0ds
320700041022      * Reperisco data corrente
320800041022     c                   time                    w0140
320900041023     c                   move      w0140         wDate
321000041022     c                   clear                   WLBdat
321100041023     c                   eval      G02dat = wDate
321200041022     c                   call      'XSRDA8'
321300041022     c                   parm                    WLBdat
321400041022     c                   z-add     G02inv        DateU
321500041022      *
321600041022      * Reperisco dati job
321700041022     c                   exsr      DatiJob
321800041022
321900041022      * Se sono in sede accendo 01
322000041022     c                   eval      *in01=(dutlpo ='S')
322100041022
322200041022      * controllo il sistema informativo dove sono per reperire
322300041022      * la libreria
322400041027      *   ---> GAITRA201/GAITRAAZM
322500041026     c                   If        %subst(knsif:7:1) <> 'P'
322600041027     c                   Eval      wlib = 'GAITRAAZM '
322700041022     c                   EndIf
322800041022      *   ---> GAITRAP/FILTRAP/UNITRAGRU
322900041022     c                   If        %subst(knsif:7:1) = 'P'
323000120914     c                   Eval      wlib = 'GAITRAAZP'
323100041022     c                   EndIf
323200041022
323300041022      * duplico il file di comodo in qtemp
323400041022     c                   clear                   comman
323500041022     c                   movel(p)  cmd(1)        comman
323600041022     c                   Eval      %Subst(comman:32:10) = wlib
323700041022     c                   call      'QCMDEXC'
323800041022     c                   parm                    comman
323900041022     c                   parm                    lenght
324000041022      * apro il file di comodo per la stampa
324100041022     c                   open      wfrac11l
324200041023
324300041023      * duplico il file di comodo in qtemp se ordinamento 1 e richiesta stampa dettaglio
324400041027     c                   If        id0ord = '1' and not *in01
324500041023     c                   clear                   comman
324600041023     c                   movel(p)  cmd(2)        comman
324700041023     c                   Eval      %Subst(comman:32:10) = wlib
324800041023     c                   call      'QCMDEXC'
324900041023     c                   parm                    comman
325000041023     c                   parm                    lenght
325100041023      * apro il file di comodo per la stampa
325200041023     c                   open      wfrut00f
325300041023     c                   endif
325400121116
325500121116      /free
325600121116       //?Se richiesto il file x chiamante
325700121116         IF  ID0crptch = 'S';
325800121116         //?Duplico in Qtemp il file WFRAD
325900121116           wcmd = cmd(3);
326000121116           %subst(wcmd:32:10) = wlib;
326100121116           ExecuteCommand (wcmd);
326200121116         //?Apro il file
326300121116           open  WFRAD01L;
326400121116         ENDIF;
326500140205
326600140205         // -?Caricamento dati tab. "MAR" = Motivi Apertura Reclami?
326700140206         clear  yy;
326800140205         clear  sk_MAR;
326900140205         clear  sk_MARd;
327000140210         clear  sk_MARs;
327100140205         clear  KeyTNTBE01;
327200140205         k_TBEcod = 'MAR';
327300140205         setll  %kds( keyTNTBE01 )  TNTBE000;
327400140205         reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
327500140205         DoW  Not  %eof(TNTBE01L);
327600140205           if  TBEatb = *blank;
327700140205             dMAR = TBEuni;
327800140205             yy += 1;
327900140205             sk_MAR(yy)  = TBEke1;
328000140205             sk_MARd(yy) = §MARdesc;
328100140210             sk_MARs(yy) = §MAReSt;
328200140205           endif;
328300140205           reade  %kds( keyTNTBE01 : 1 )  TNTBE000;
328400140205         EndDo;
328500140205
328600121116      /end-free
328700041022
328800041022     C                   ENDSR
328900041022**   cmd - comando
329000041022CRTDUPOBJ OBJ(WFRAC1*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
329100041023CRTDUPOBJ OBJ(WFRUT0*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
329200121116CRTDUPOBJ OBJ(WFRAD0*) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
329300121116CLRPFM QTEMP/WFRUT00F
329400121116CPYF FROMFILE(QTEMP/WFRAD00F) TOFILE(
