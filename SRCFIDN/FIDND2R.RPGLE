000100131018     /*END
000200040930     H DECEDIT('0,') DATEDIT(*DMY.)
000300040930      *----------------------------------------------------*
000400040930      *  Statistica Richiesta Assistenza Clienti           *
000500040930      *----------------------------------------------------*
000600041220     fFidnd2d   cf   e             workstn
000700041018     fAzorg01l  if   e           k disk
000800050523     fTabel00f  if   e           k disk
000900040930
001000041014      *---------------------------------------------------------------*
001100040930
001200041014      *
001300041014      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
001400041014      *
001500041014
001600050523     d msg             s             78    dim(12) ctdata perrcd(1)             msg video
001700041020     d l6              S              3  0 dim(30)
001800041020     d poa             S              3  0 dim(250)
001900041220      * Capoconto / Sottoconto di ritorno "XCLIR"
002000041220     d Ksc             S              7  0 dim(30)
002100041220     d Ksa             S              4    dim(30)
002200040930
002300041014      *
002400041014      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
002500041014      *
002600040930
002700040930     d Kpjba         e ds
002800050523     d ds5a2         e ds
002900140130     d dMAR          e ds                  inz
003000040930
003100041014      *
003200041221      * - Parametri x recupero codici clienti ed unificanti "XCLIR"
003300041221     D XCLIRDS       E DS
003400041221      * - Parametri x recupero codici clienti ed unificanti "TIBS10R"
003500041221     D TIBS10DS      E DS
003600041014      * - Parametri x Controllo profilo utenti
003700041014     d TIBS34DS      e ds                  inz
003800041014      * - Ds di riferimento al file esterno AZUTE00F
003900041014     d AZUTEDS       e ds                  extname(AZUTE00F)
004000041014      * - Ds per dati organigramma
004100041014     d DDatiUte      e ds
004200041018      * - Tabelle
004300041018     d TIBS02ds      e ds                  inz
004400131018     d tntbMARds1    e ds                  inz
004500041221      * - Parametri
004600041221     d FIDND2DS      e ds
004700041019      * DS per TRUL06R - Caricamento £X
004800041019     d TRUL06DS      E DS
004900041019     d  LIN                    1     90  0
005000041019     d                                     DIM(30)
005100041020      * DS per TRUL31R - Caricamento P.O. abilitati
005200041020     d TRUL31DS      E DS
005300041020     d  OPG                   10    759  0
005400041020     d                                     DIM(250)
005500041220      * - Ds per Anagrafiche Clienti
005600041220     d TIBS69DS      E DS
005700041220     d DS_cnaco      E DS                  extname(CNACO00F)
005800041220     d DS_cnind      E DS                  extname(CNIND00F)
005900041220     d DS_cnclp      E DS                  extname(CNCLP00F)
006000041220     d DS_fncls      E DS                  extname(FNCLS00F)
006100041220      *
006200140925      // - Tabella COR = Tipo comunicazione
006300140925     d dCOR          e ds                  inz
006400041014      *
006500041014     d WLBdat          ds                  inz
006600041014     d  G02dat                 1      8  0 inz
006700041014     d  G02inv                 9     16  0 inz
006800041014     d  G02err                17     17    inz('3')
006900041014     d  G02tgi                18     22  0 inz
007000050523     D WGIDAT          DS
007100050523     D  GIODAT                 1      8  0
007200050523     D  GIOINV                 9     16  0
007300050523     D  GIOTGI                17     21  0
007400041221      *
007500041221      * DS per passaggio campi Packed
007600041221      *
007700041221     D                 DS
007800041221     D  Dsksn                  1      4P 0
007900041221     D  Dsksa                  1      4
008000041014      *
008100041014      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
008200041014      *
008300041014
008400041014      * - Campi di comodo
008500041014     d w0140           s             14  0
008600041014     d wData           s              8  0
008700041014     d DateU           s              8  0
008800041221     d wfil            s                   like(orgfil)
008900041018     d wamgdcd         s                   like(V1cdcd)
009000041018     d wamgdca         s                   like(V1cdcd)
009100041018     d wamgdhd         s                   like(V1cdcd)
009200041018     d wamgdha         s                   like(V1cdcd)
009300041018     d wrk_cor         s                   like(v1ctc1)
009400041018     d wrk_mar         s                   like(v1cma1)
009500041018     d wrk_chr         s                   like(v1ccc1)
009600041220     d wtibs69r        s              1
009700050523     d sav_tgi         s                   like(g02tgi)
009800050523     d codut           s              1  0 inz(1)
009900050523     d Cod             s                   like(TBLcod)
010000050523     d Key             s                   like(TBLkey)
010100041014      *------------------------------------------------------------------------*
010200041014      * INDICATORI
010300041014      *------------------------------------------------------------------------*
010400041014      *  06   - 2 livello
010500041220      *  10/12- errori prima parte del video
010600041014      *  28   - emetto campo messaggio
010700041014      *  30/31- comodo
010800041221      *  40/63- errori posizionamento cursore sui campi
010900041014      *  90   - riemissione videata per errore o ricerca
011000041014      *---------------------------------------------------------------*
011100041014
011200041014      * imposto i campi iniziali
011300041014
011400041220     c                   exsr      sr_inzvideo
011500041014      *
011600041220     c                   do        *hival
011700041014      *
011800041220     c                   exfmt     dnd2d01
011900041019     c                   setoff                                       2890
012000041014      *
012100041014      * f3 fine lavoro
012200041014     c                   if        *inkc
012300041014     c                   leave
012400041014     c                   endif
012500041014      *
012600041014      * controllo dati del video
012700041014      *
012800041220     c                   exsr      sr_ctrd01
012900041014
013000041014     c                   if        *in90
013100041014     c                   iter
013200041014     c                   endif
013300041220      *
013400041220      * F6 di conferma lancio la statistica
013500041220      *
013600041220     c                   If        *inKF
013700041220     c                   exsr      SR_conferma
013800041220     c                   leave
013900041220     c                   endif
014000041014      *
014100041014     c                   enddo
014200041014      *
014300041220
014400041220     C                   IF        Wtibs69r  <>  *BLANKS
014500041220     C                   eval      I69TLA  = 'C'
014600041220     C                   call      'TIBS69R'
014700041220     C                   parm                    TIBS69DS
014800041220     C                   ENDIF
014900041220      *
015000041014      *
015100041014     c                   seton                                        lr
015200041014      *
015300041014      *---------------------------------------------------------------*
015400041220      * Inizializzo i campi della videata                             *
015500041014      *---------------------------------------------------------------*
015600041220     c     SR_inzvideo   BegSr
015700041014      *
015800041220      * imposto i campi del video
015900041014      *
016000041015      *
016100041014     c                   clear                   v1cprs
016200041014     c                   clear                   v1dprs
016300041014     c                   clear                   v1cfil
016400041014     c                   clear                   v1dfil
016500041014     c                   clear                   v1cdcd
016600041014     c                   clear                   v1cdca
016700041014     c                   clear                   v1cdhd
016800041014     c                   clear                   v1cdha
016900041014     c                   clear                   v1ctc1
017000041014     c                   clear                   v1ctc2
017100041014     c                   clear                   v1ctc3
017200041014     c                   clear                   v1ctc4
017300041014     c                   clear                   v1cma1
017400041014     c                   clear                   v1cma2
017500041014     c                   clear                   v1cma3
017600041014     c                   clear                   v1cma4
017700041014     c                   clear                   v1cma5
017800041014     c                   clear                   v1cma6
017900041014     c                   clear                   v1cma7
018000041014     c                   clear                   v1cma8
018100041014     c                   clear                   v1ccc1
018200041014     c                   clear                   v1ccc2
018300041014     c                   clear                   v1ccc3
018400041014     c                   eval      v1csra = ' '
018500041220     c                   eval      v1cnot = ' '
018600041220     c                   eval      v1ctot = ' '
018700041220     c                   eval      v1cpor = ' '
018800041014      *
018900041014     c                   endsr
019000041018      *
019100041015      *---------------------------------------------------------------*
019200041015      * Controllo i campi della seconda videata                       *
019300041015      *---------------------------------------------------------------*
019400041015     c     SR_ctrd01     BegSr
019500041015      *
019600041015     c                   do
019700041019
019800041220     c                   setoff                                       101112
019900041019     c                   setoff                                       404142
020000041019     c                   setoff                                       434445
020100041019     c                   setoff                                       464748
020200041019     c                   setoff                                       495051
020300041019     c                   setoff                                       525354
020400041019     c                   setoff                                       555657
020500041019     c                   setoff                                       585960
020600041019     c                   setoff                                       616263
020700041018
020800041220      * verifico il tipo di scelta
020900041220
021000041221     c                   If        v1csce = ' '
021100041220     c                   seton                                        109028
021200041220     c                   movel     MSG(1)        v1cmsg
021300041220     c                   leave
021400041220     c                   endif
021500041220      * controllo il cliente
021600041220      * se viene dato F11 richiamo il programma di interrogazione
021700041220     c                   If        *inkk
021800041220      * richiamo il programma XCLIR interrogazione clienti unificanti e non
021900041220     c                   clear                   xclirds
022000041220     c                   z-add     dutkci        dxccap
022100041220     c                   movel     'AZ'          dxcaut
022200041220
022300041220     c                   call      'XCLIR'
022400041220     c                   parm                    xclirds
022500041220
022600041220     c                   movea     dxcksc        ksa
022700041220     c                   move      ksa(1)        dsksa
022800041220     c                   z-add     dsksn         ksc(1)
022900041221     c                   z-add     ksc(1)        v1cksc
023000041220
023100041220     c                   endif
023200041220
023300041221     c                   if        v1cksc > 0
023400041220      *
023500041220      * se scelta è uguale a 2 codice unificante controllo  e reperisce il padre
023600041221     c                   If        v1csce = '2'
023700041220     c                   clear                   Tibs10ds
023800041220     c                   z-add     dateu         d10drf                         *data riferimento
023900041220     c                   movel     'ST'          d10tle                         *tipo legame
024000041220     c                   movel     'P'           d10paf                         *cerca il padre
024100041221     c                   move      v1cksc        d10cod
024200041220     c                   call      'TIBS10R'
024300041220     c                   parm                    Tibs10ds
024400041220
024500041220     c                   If        d10err = ' ' and d10cod <> d10cop
024600041221     c                   eval      v1dcli = v1cksc
024700041221     c                   eval      v1cksc = d10cop
024800041220     c                   seton                                        12
024900041220     c                   endif
025000041220
025100041220     c                   endif
025200041220
025300041220      * aggancio anagrafico
025400041220
025500041220     C                   clear                   TIBS69DS
025600041221     c                   z-add     v1cksc        I69kac
025700041220     C                   call      'TIBS69R'
025800041220     C                   parm                    tibs69DS
025900041220     C                   parm                    DS_cnaco
026000041220     C                   parm                    DS_cnind
026100041220     C                   parm                    DS_cnclp
026200041220     C                   parm                    DS_fncls
026300041220     C                   eval      wtibs69r = '1'
026400041220     c                   If         O69ERR =  '1'
026500041220     C                   seton                                        112890
026600041220     c                   movel     msg(2)        V1Cmsg
026700041221     c                   clear                   v1dksc
026800041220     C                   leave
026900050111     c                   else
027000041220      *
027100041221     c                   movel     ACOrag        v1dksc
027200041220     c                   Endif
027300041220
027400050111     c                   Else
027500050111     C                   seton                                        112890
027600050111     c                   movel     msg(11)       V1Cmsg
027700050111     C                   leave
027800041220     c                   Endif
027900041220
028000041018      * P.O. Responsabile
028100041015     c                   clear                   v1dprs
028200041022     c                   If        v1cprs > 0
028300041022
028400041015     c                   eval      wfil=v1cprs
028500041015     c                   exsr      CTRPO
028600041018
028700041015     c   90              seton                                        40
028800041018     c   90              leave
028900041018
029000041015     c                   eval      v1dprs=orgdes
029100041018
029200041015     c                   endif
029300041015     c
029400041015
029500041018      * P.O. Apertura Reclamo
029600041015     c                   clear                   v1dfil
029700041015     c                   if        v1cfil>0
029800041022
029900041015     c                   eval      wfil=v1cfil
030000041015     c                   exsr      CTRPO
030100041015     c   90              seton                                        41
030200041018     c   90              leave
030300041018
030400041015     c                   eval      v1dfil=orgdes
030500041018
030600041015     c                   endif
030700041130
030800041018      * Data Richiesta Assistenza
030900041018
031000041018     c                   if        v1cdcd = 0 and v1cdca > 0
031100041018     c                   seton                                        422890
031200041221     c                   movel     MSG(4)        v1cmsg
031300041018     c                   leave
031400041018     c                   endif
031500041018
031600041018     c                   clear                   wamgdcd
031700041018     c                   clear                   wamgdca
031800041018      * Da data
031900041018     c                   if        v1cdcd > 0
032000041018     c                   move      V1cdcd        g02dat
032100041018     c                   eval      g02err = *blanks
032200041018     c                   call      'XSRDA8'
032300041018     c                   parm                    wlbdat
032400041018      *
032500041018     c                   if        g02err = '1'
032600041018     c                   seton                                        284290
032700041221     c                   movel     msg(4)        v1cmsg
032800041018     c                   leave
032900041018     c                   else
033000041018     c                   eval      wamgdcd=g02inv
033100041018     c                   eval      v1cdcd =g02dat
033200041018     c                   endif
033300041018
033400041018     c                   endif
033500041018      * A data
033600041018     c                   if        v1cdca > 0
033700041018     c                   move      V1cdca        g02dat
033800041018     c                   eval      g02err = *blanks
033900041018     c                   call      'XSRDA8'
034000041018     c                   parm                    wlbdat
034100041018      *  Errore
034200041018     c                   if        g02err = '1'
034300041018     c                   seton                                        284390
034400041221     c                   movel     msg(4)        v1cmsg
034500041018     c                   leave
034600041018
034700041018     c                   else
034800041018
034900041018     c                   eval      wamgdca=g02inv
035000041018     c                   eval      v1cdca =g02dat
035100041018     c
035200041018     c                   if        wamgdca < wamgdcd
035300041018     c                   seton                                        284390
035400041221     c                   movel     msg(5)        v1cmsg
035500041018     c                   leave
035600041018     c                   endif
035700041018
035800041018     c                   endif
035900041018
036000041018     c                   endif
036100041018
036200041018      * Data chiusura Richesta Assistenza
036300041018
036400041018     c                   if        v1cdhd = 0 and v1cdha > 0
036500041018     c                   seton                                        442890
036600041221     c                   movel     msg(4)        v1cmsg
036700041018     c                   leave
036800041018     c                   endif
036900041018
037000041018     c                   clear                   wamgdhd
037100041018     c                   clear                   wamgdha
037200041018      * Da data
037300041018     c                   if        v1cdhd > 0
037400041018     c                   move      V1cdhd        g02dat
037500041018     c                   eval      g02err = *blanks
037600041018     c                   call      'XSRDA8'
037700041018     c                   parm                    wlbdat
037800041018      *
037900041018     c                   if        g02err = '1'
038000041018     c                   seton                                        284490
038100041221     C                   movel     msg(4)        v1cmsg
038200041018     c                   leave
038300041018
038400041018     c                   else
038500041018
038600041018     c                   eval      wamgdhd=g02inv
038700041018     c                   eval      v1cdhd =g02dat
038800050523     c                   if        wamgdhd < wdapul and v1csra = ' '
038900050523     c                   seton                                        284490
039000050523     C                   movel     msg(12)       v1cmsg
039100050523     c                   leave
039200050523     c                   endif
039300041018     c                   endif
039400041018
039500041018     c                   endif
039600041018      * a data
039700041018     c                   if        v1cdha > 0
039800041018     c                   move      V1cdha        g02dat
039900041018     c                   eval      g02err = *blanks
040000041018     c                   call      'XSRDA8'
040100041018     c                   parm                    wlbdat
040200041018      *  Errore
040300041018     c                   if        g02err = '1'
040400041018     c                   seton                                        284590
040500041221     c                   movel     msg(4)        v1cmsg
040600041018     c                   leave
040700041018
040800041018     c                   else
040900041018
041000041018     c                   eval      wamgdha=g02inv
041100041018     c                   eval      v1cdha =g02dat
041200041018     c
041300041018     c                   if        wamgdha < wamgdhd
041400041018     c                   seton                                        284590
041500041221     c                   movel     msg(5)        v1cmsg
041600041018     c                   leave
041700041018     c                   endif
041800041018
041900041018     c                   endif
042000041018
042100041018     c                   endif
042200041020      *
042300041020      * almeno un'intervallo di data deve essere impostato
042400041020      *
042500041020     c                   if        v1cdcd = 0 and v1cdca = 0 and
042600041020     c                             v1cdhd = 0 and v1cdha = 0
042700041020     c                   seton                                        289042
042800041221     c                   movel     msg(10)       v1cmsg
042900041020     c                   leave
043000041020     c                   endif
043100041018
043200041018      *
043300041018      * Tipo Comunicazione
043400041018      *          (1)
043500041018
043600041018     c                   If        v1ctc1 <> ' '
043700041018
043800041018     c                   eval      wrk_cor = v1ctc1
043900041018     c                   exsr      ctr_cor
044000041018     c                   eval      v1ctc1  = wrk_cor
044100041018
044200041018     c                   if        *in90
044300041018     c                   eval      *in46 = *on
044400041018     c                   leave
044500041018     c                   endif
044600041018
044700041018     c                   endif
044800041018      *
044900041018      * Tipo Comunicazione
045000041018      *          (2)
045100041018
045200041018     c                   If        v1ctc2 <> ' '
045300041018
045400041018     c                   eval      wrk_cor = v1ctc2
045500041018     c                   exsr      ctr_cor
045600041018     c                   eval      v1ctc2  = wrk_cor
045700041018
045800041018     c                   if        *in90
045900041018     c                   eval      *in47 = *on
046000041018     c                   leave
046100041018     c                   endif
046200041018
046300041018     c                   endif
046400041018      *
046500041018      * Tipo Comunicazione
046600041018      *          (3)
046700041018
046800041018     c                   If        v1ctc3 <> ' '
046900041018
047000041018     c                   eval      wrk_cor = v1ctc3
047100041018     c                   exsr      ctr_cor
047200041018     c                   eval      v1ctc3  = wrk_cor
047300041018
047400041018     c                   if        *in90
047500041018     c                   eval      *in48 = *on
047600041018     c                   leave
047700041018     c                   endif
047800041018
047900041018     c                   endif
048000041018      *
048100041018      * Tipo Comunicazione
048200041018      *          (4)
048300041018
048400041018     c                   If        v1ctc4 <> ' '
048500041018
048600041018     c                   eval      wrk_cor = v1ctc4
048700041018     c                   exsr      ctr_cor
048800041018     c                   eval      v1ctc4  = wrk_cor
048900041018
049000041018     c                   if        *in90
049100041018     c                   eval      *in49 = *on
049200041018     c                   leave
049300041018     c                   endif
049400041018
049500041018     c                   endif
049600041018
049700041018      *
049800041018      * Motivo Apertura
049900041018      *          (1)
050000041018
050100041018     c                   If        v1cma1 <> '  '
050200041018
050300041018     c                   eval      wrk_mar = v1cma1
050400041018     c                   exsr      ctr_mar
050500041019     c                   eval      v1cma1  = wrk_mar
050600041018
050700041018     c                   if        *in90
050800041018     c                   eval      *in50 = *on
050900041018     c                   leave
051000041018     c                   endif
051100041018
051200041018     c                   endif
051300041018      *
051400041018      * Motivo Apertura
051500041018      *          (2)
051600041018
051700041018     c                   If        v1cma2 <> '  '
051800041018
051900041018     c                   eval      wrk_mar = v1cma2
052000041018     c                   exsr      ctr_mar
052100041019     c                   eval      v1cma2  = wrk_mar
052200041018
052300041018     c                   if        *in90
052400041018     c                   eval      *in51 = *on
052500041018     c                   leave
052600041018     c                   endif
052700041018
052800041018     c                   endif
052900041018      *
053000041018      * Motivo Apertura
053100041018      *          (3)
053200041018
053300041018     c                   If        v1cma3 <> '  '
053400041018
053500041018     c                   eval      wrk_mar = v1cma3
053600041018     c                   exsr      ctr_mar
053700041019     c                   eval      v1cma3  = wrk_mar
053800041018
053900041018     c                   if        *in90
054000041018     c                   eval      *in52 = *on
054100041018     c                   leave
054200041018     c                   endif
054300041018
054400041018     c                   endif
054500041018      *
054600041018      * Motivo Apertura
054700041018      *          (4)
054800041018
054900041018     c                   If        v1cma4 <> '  '
055000041018
055100041018     c                   eval      wrk_mar = v1cma4
055200041018     c                   exsr      ctr_mar
055300041019     c                   eval      v1cma4  = wrk_mar
055400041018
055500041018     c                   if        *in90
055600041018     c                   eval      *in53 = *on
055700041018     c                   leave
055800041018     c                   endif
055900041018
056000041018     c                   endif
056100041018      *
056200041018      * Motivo Apertura
056300041018      *          (5)
056400041018
056500041018     c                   If        v1cma5 <> '  '
056600041018
056700041018     c                   eval      wrk_mar = v1cma5
056800041018     c                   exsr      ctr_mar
056900041019     c                   eval      v1cma5  = wrk_mar
057000041018
057100041018     c                   if        *in90
057200041018     c                   eval      *in54 = *on
057300041018     c                   leave
057400041018     c                   endif
057500041018
057600041018     c                   endif
057700041018      *
057800041018      * Motivo Apertura
057900041018      *          (6)
058000041018
058100041018     c                   If        v1cma6 <> '  '
058200041018
058300041018     c                   eval      wrk_mar = v1cma6
058400041018     c                   exsr      ctr_mar
058500041019     c                   eval      v1cma6  = wrk_mar
058600041018
058700041018     c                   if        *in90
058800041018     c                   eval      *in55 = *on
058900041018     c                   leave
059000041018     c                   endif
059100041018
059200041018     c                   endif
059300041018      *
059400041018      * Motivo Apertura
059500041018      *          (7)
059600041018
059700041018     c                   If        v1cma7 <> '  '
059800041018
059900041018     c                   eval      wrk_mar = v1cma7
060000041018     c                   exsr      ctr_mar
060100041019     c                   eval      v1cma7  = wrk_mar
060200041018
060300041018     c                   if        *in90
060400041018     c                   eval      *in56 = *on
060500041018     c                   leave
060600041018     c                   endif
060700041018
060800041018     c                   endif
060900041018      *
061000041018      * Motivo Apertura
061100041018      *          (8)
061200041018
061300041018     c                   If        v1cma8 <> '  '
061400041018
061500041018     c                   eval      wrk_mar = v1cma8
061600041018     c                   exsr      ctr_mar
061700041019     c                   eval      v1cma8  = wrk_mar
061800041018
061900041018     c                   if        *in90
062000041018     c                   eval      *in57 = *on
062100041018     c                   leave
062200041018     c                   endif
062300041018
062400041018     c                   endif
062500041018      *
062600041018      * Codice Chiusura
062700041018      *          (1)
062800041018
062900041018     c                   If        v1ccc1 <> '   '
063000041018
063100041018     c                   eval      wrk_chr = v1ccc1
063200041018     c                   exsr      ctr_chr
063300041018     c                   eval      v1ccc1  = wrk_chr
063400041018
063500041018     c                   if        *in90
063600041018     c                   eval      *in58 = *on
063700041018     c                   leave
063800041018     c                   endif
063900041018
064000041018     c                   endif
064100041018      *
064200041018      * Codice Chiusura
064300041018      *          (2)
064400041018
064500041018     c                   If        v1ccc2 <> '   '
064600041018
064700041018     c                   eval      wrk_chr = v1ccc2
064800041018     c                   exsr      ctr_chr
064900041018     c                   eval      v1ccc2  = wrk_chr
065000041018
065100041018     c                   if        *in90
065200041018     c                   eval      *in59 = *on
065300041018     c                   leave
065400041018     c                   endif
065500041018
065600041018     c                   endif
065700041018      *
065800041018      * Codice Chiusura
065900041018      *          (3)
066000041018
066100041018     c                   If        v1ccc3 <> '   '
066200041018
066300041018     c                   eval      wrk_chr = v1ccc3
066400041018     c                   exsr      ctr_chr
066500041018     c                   eval      v1ccc3  = wrk_chr
066600041018
066700041018     c                   if        *in90
066800041018     c                   eval      *in60 = *on
066900041018     c                   leave
067000041018     c                   endif
067100041018
067200041018     c                   endif
067300041018      *
067400041018      * Se richieste solo le aperte non posso chiedere causale chiusura
067500041018      *  o date chiusura
067600041018     c                   if        v1csra='S'
067700041018     c                   if        v1cdhd>0 or v1ccc1<>'   ' or v1ccc2<>'   '
067800041018     c                             or v1ccc3<>'   '
067900041018     c                   seton                                        286190
068000041221     c                   movel     msg(9)        v1cmsg
068100041018     c                   leave
068200041018     c                   endif
068300041018     c                   endif
068400041018      *
068500041018     c                   leave
068600041018      *
068700041018     c                   enddo
068800041018
068900041018     c                   endsr
069000041018      *---------------------------------------------------------------*
069100041018      * CONTROLLO P.O.                                                *
069200041018      *---------------------------------------------------------------*
069300041018     C     CTRPO         BEGSR
069400041018      *
069500041018     c     wfil          chain     azorg01l
069600041018
069700041019     c                   if        not %found(azorg01l) or (orgfva <> ' ')
069800041026     c                             or (orgfag <> 'A' and orgfag <> 'F' and
069900041026     c                              orgfil <> 046)
070000041221     c                   eval      v1cmsg=msg(3)
070100041018     c                   seton                                        2890
070200041018     c                   endif
070300041018      *
070400041018     c                   ENDSR
070500041018      *---------------------------------------------------------------*
070600041018      * CONTROLLO tipo comunicazione                                  *
070700041018      *---------------------------------------------------------------*
070800041018     C     CTR_COR       BEGSR
070900041018
071000041018      * ricerca
071100041018
071200041018     c     '?'           scan      wrk_cor                                90
071300041018     c                   clear                   tibs02ds
071400041018
071500041018     c                   if        *in90
071600041018     c                   movel     'R'           t02mod
071700041018     c                   movel     knsif         t02sif
071800041018     c                   movel     'COR'         t02cod
071900041018      *
072000041018     c                   call      'TIBS02R'
072100041018     c                   parm                    kpjba
072200041018     c                   parm                    tibs02ds
072300041018      *
072400041018     c                   if        t02err = ' '
072500041018     c                   movel     t02ke1        wrk_cor
072600041018     c                   endif
072700041018      *
072800041018     c                   else
072900041018      *  controllo
073000041018     c                   clear                   tibs02ds
073100041018     c                   movel     'C'           t02mod
073200041018     c                   movel     knsif         t02sif
073300041018     c                   movel     'COR'         t02cod
073400041018     c                   movel     wrk_cor       t02ke1
073500041018      *
073600041018     c                   call      'TIBS02R'
073700041018     c                   parm                    kpjba
073800041018     c                   parm                    tibs02ds
073900041018      *
074000041018     c                   if        t02err <> ' '
074100041018     c                   seton                                        2890
074200041221     c                   movel     MSG(6)        v1cmsg
074300140925     c                   leavesr
074400041018     c                   endif
074500140925
074600140925      /free
074700140925       //?Verifico se il tipo comunicazione è utilizzabile dall'utente
074800140925         dCOR = T02uni;
074900140925         IF  §CORtcut  = 'N';
075000140925           *in28 = *on;
075100140925           *in90 = *on;
075200141006           V1Cmsg = msg(6);
075300140925           V1Cmsg = %trim(V1Cmsg) + '. Non utilizzabile dall''utente';
075400140925         ENDIF;
075500140925      /end-free
075600041018      *
075700041018     c                   endif
075800041018
075900041018     c                   endsr
076000041018      *---------------------------------------------------------------*
076100041018      * CONTROLLO motivo apertura                                     *
076200041018      *---------------------------------------------------------------*
076300041018     C     CTR_MAR       BEGSR
076400041018
076500041018      * ricerca
076600041018
076700041018     c     '?'           scan      wrk_mar                                90
076800131018      *
076900041018     c                   if        *in90
077000131018      *
077100131018     c                   clear                   tntbMARds1
077200140210     c*//                eval      iMARutut = 'I'
077300140210     c                   eval      iMARest  = 'I'
077400131018     c                   movel(p)  tntbMARds1    KPJBU
077500131018      *
077600131018     c                   call      'TNTBMARR1'
077700131018     c                   parm                    KPJBA
077800131018      *
077900131018     c                   movel     KPJBU         tntbMARds1
078000131018     c                   if        oMARfxx = *blank  and  oMARerr = *off
078100131018     c                   movel     oMARcod       wrk_mar
078200041018     c                   endif
078300041018      *
078400041018     c                   else
078500041018      *  controllo
078600041018     c                   clear                   tibs02ds
078700041018     c                   movel     'C'           t02mod
078800041018     c                   movel     knsif         t02sif
078900041018     c                   movel     'MAR'         t02cod
079000041018     c                   movel     wrk_mar       t02ke1
079100041018      *
079200041018     c                   call      'TIBS02R'
079300041018     c                   parm                    kpjba
079400041018     c                   parm                    tibs02ds
079500041018      *
079600140130     c                   clear                   dMAR
079700140130     c                   if        T02err = *blanks
079800140130     c                   eval      dMAR = T02uni
079900140130     c                   endif
080000140130      *
080100140210     c*//                if        t02err <> ' '  or  §MARutUt = 'N'
080200140210     c                   if        t02err <> ' '  or  §MARest  = 'S'
080300041018     c                   seton                                        2890
080400041221     c                   movel     MSG(7)        v1cmsg
080500041018     c                   endif
080600041018      *
080700041018     c                   endif
080800041018
080900041018     c                   endsr
081000041018      *---------------------------------------------------------------*
081100041018      * CONTROLLO codice chiusura                                     *
081200041018      *---------------------------------------------------------------*
081300041018     C     CTR_CHR       BEGSR
081400041018
081500041018      * ricerca
081600041018
081700041018     c     '?'           scan      wrk_chr                                90
081800041018     c                   clear                   tibs02ds
081900041018
082000041018     c                   if        *in90
082100041018     c                   movel     'R'           t02mod
082200041018     c                   movel     knsif         t02sif
082300041018     c                   movel     'CHR'         t02cod
082400041018      *
082500041018     c                   call      'TIBS02R'
082600041018     c                   parm                    kpjba
082700041018     c                   parm                    tibs02ds
082800041018      *
082900041018     c                   if        t02err = ' '
083000041018     c                   movel     t02ke1        wrk_chr
083100041018     c                   endif
083200041018      *
083300041018     c                   else
083400041018      *  controllo
083500041018     c                   clear                   tibs02ds
083600041018     c                   movel     'C'           t02mod
083700041018     c                   movel     knsif         t02sif
083800041018     c                   movel     'CHR'         t02cod
083900041018     c                   movel     wrk_chr       t02ke1
084000041018      *
084100041018     c                   call      'TIBS02R'
084200041018     c                   parm                    kpjba
084300041018     c                   parm                    tibs02ds
084400041018      *
084500041018     c                   if        t02err <> ' '
084600041018     c                   seton                                        2890
084700041221     c                   movel     MSG(8)        v1cmsg
084800041018     c                   endif
084900041018      *
085000041018     c                   endif
085100041018
085200041018     c                   endsr
085300041018      *---------------------------------------------------------------*
085400041018      * CONFERMA ELABORAZIONE E LANCIO                                *
085500041018      *---------------------------------------------------------------*
085600041018     C     SR_conferma   BEGSR
085700041018
085800041018      * imposto la DS per il lancio
085900041221     c                   clear                   fidnd2ds
086000041019
086100041221     c                   eval      id2sce = v1csce
086200041221     c                   eval      id2ksc = v1cksc
086300041223     c                   eval      id2rag = v1dksc
086400041019
086500041221     c                   eval      id2prs = v1cprs
086600041221     c                   eval      id2fil = v1cfil
086700041019
086800041221     c                   eval      id2amgdcd = wamgdcd
086900041221     c                   eval      id2amgdca = wamgdca
087000041221     c                   eval      id2amgdhd = wamgdhd
087100041221     c                   eval      id2amgdha = wamgdha
087200041221     c                   eval      id2dcd = v1cdcd
087300041221     c                   eval      id2dca = v1cdca
087400041221     c                   eval      id2dhd = v1cdhd
087500041221     c                   eval      id2dha = v1cdha
087600041019
087700041221     c                   eval      id2tc1 = v1ctc1
087800041221     c                   eval      id2tc2 = v1ctc2
087900041221     c                   eval      id2tc3 = v1ctc3
088000140925     c                   eval      id2tc4 = v1ctc4
088100041019
088200041221     c                   eval      id2ma1 = v1cma1
088300041221     c                   eval      id2ma2 = v1cma2
088400041221     c                   eval      id2ma3 = v1cma3
088500041221     c                   eval      id2ma4 = v1cma4
088600041221     c                   eval      id2ma5 = v1cma5
088700041221     c                   eval      id2ma6 = v1cma6
088800041221     c                   eval      id2ma7 = v1cma7
088900041221     c                   eval      id2ma8 = v1cma8
089000041019
089100041221     c                   eval      id2cc1 = v1ccc1
089200041221     c                   eval      id2cc2 = v1ccc2
089300041221     c                   eval      id2cc3 = v1ccc3
089400041019
089500041221     c                   eval      id2sra = v1csra
089600041221     c                   eval      id2not = v1cnot
089700041221     c                   eval      id2por = v1cpor
089800041221     c                   eval      id2tot = v1ctot
089900050113     c                   eval      id2det = v1cdet
090000041019
090100041019     c
090200041019      *
090300041019     c                   clear                   Wpgm             10
090400041019      *
090500041221     c                   movel     fidnd2ds      kpjbu
090600041019     c
090700050113     c                   eval      kcoaz='DND3'
090800050113     c******             eval      Wpgm = 'FIDND3R   '
090900050113     c                   eval      Wpgm = 'BCH10     '
091000041203     c                   call      Wpgm
091100041019     c                   parm                    kpjba
091200041019     c                   endsr
091300041018     c
091400041018      *---------------------------------------------------------------*
091500041014      * Reperimento Dati del job (Utente/Operativi)                   *
091600041014      *---------------------------------------------------------------*
091700041014     c     DatiJob       BegSr
091800041014      *
091900041014     c     *dtaara       define    §azute        azuteds
092000041014     c     *dtaara       define    §datiute      ddatiute
092100041014      *
092200041014     c                   in(E)     *dtaara
092300041014     c                   IF        %ERROR or RSUT = *blanks
092400041014     c                   clear                   Tibs34Ds
092500041014     c                   call      'TIBS34R'
092600041014     c                   parm                    Tibs34Ds
092700041014     c                   in        *dtaara
092800041014     c                   ENDIF
092900050523     c*
093000050523     c     ktab          klist
093100050523     c                   kfld                    codut
093200050523     c                   kfld                    cod
093300050523     c                   kfld                    key
093400041015      *
093500041014     c                   EndSr
093600041014      *------------------------------------------------------------------------*
093700041014      * Routine Iniziale                                              *
093800041014      *---------------------------------------------------------------*
093900041014     c     *InzSR        BegSR
094000041014      *
094100041014     c     *Entry        plist
094200041014     c                   parm                    KPJBA
094300041014      * Reperisco data corrente
094400041014     c                   time                    w0140
094500041014     c                   move      w0140         wData
094600041014     c                   clear                   WLBdat
094700041014     c                   eval      G02dat = wData
094800041014     c                   call      'XSRDA8'
094900041014     c                   parm                    WLBdat
095000041014     c                   z-add     G02inv        DateU
095100050523     c                   z-add     g02tgi        sav_tgi
095200041014      *
095300041014      * Reperisco dati job
095400041014     c                   exsr      DatiJob
095500041014
095600041014      * Se sono in 2°livell accendo 06
095700041014     c                   eval      *in06=(dutlpo ='2')
095800041020      * CARICO p.o. per eventuali controlli
095900041020     c                   if        dutlpo <>'S'
096000041020      * cerco i PO autorizzati
096100041020     c                   clear                   trul31ds
096200041021     c                   eval      I31cdi = dutdis
096300041021     c                   eval      I31car = dutare
096400041020     c                   eval      i31cpo = dutpou
096500041020     c                   eval      i31abi = 'RA'
096600041020
096700041020     c                   call      'TRUL31R'
096800041020     c                   parm                    kpjba
096900041021     c                   parm                    trul31ds
097000041020     c                   movea     opg           poa
097100041020     c                   Endif
097200041020      * se secondo livello guardo anche la L6
097300041020     c                   If        Dutlpo = '2'
097400041019     c                   clear                   trul06ds
097500041020     c                   move      '£6'          d06cod
097600041019     c                   movel     dutpou        d06key
097700041021     c                   movel(p)  trul06ds      KPJBU
097800041019
097900041019     c                   call      'TRUL06R'
098000041019     c                   parm                    kpjba
098100041019     c                   movel     kpjbu         trul06ds
098200041020     c                   movea     lin           l6
098300041019     c                   Endif
098400050523     c*
098500050523      * RECUPERO DA TABELLA 5A I PARAMETRI DI PULIZIA
098600050523      *
098700050523     C                   MOVEL     '5A'          COD
098800050523     C                   MOVEL(p)  '2'           KEY
098900050523     C     KTAB          CHAIN     TABEL00F                           30
099000050523     C  N30              MOVEL     TBLUNI        DS5A2
099100050523     C   30              CLEAR                   DS5A2
099200050523     c* determino data pulizia file reclami
099300050523     C     sav_tgi       SUB       §5Atgd        GIOTGI
099400050523     C                   CALL      'XSRGI8'
099500050523     C                   PARM                    WGIDAT
099600050523     C                   Z-ADD     GIOinv        Wdapul            8 0          GG/MM/AAAA
099700041014
099800041014     c                   ENDSR
099900041015** MSG  Lungh. 78                                                            *
100000041220Fare una scelta                                                                1
100100041220Codice  cliente errato                                                         2
100200041221Punto operativo inesistente                                                    3
100300041221Data errata                                                                    4
100400041221Data AL   MINORE  data DAL                                                     5
100500041221Tipo Comunicazione errata                                                      6
100600041221Motivo Apertura errato                                                         7
100700041221Codice Chiusura errato                                                         8
100800041221Se selezionate le "Solo aperte" è inutile selezionare data e codici Chiusura   9
100900041221Immettere almeno un'intervallo di date                                        10
101000050111Immettere codice cliente                                                      11
101100050523Data non valida per trascorsi termini di pulizia                              12
